			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
#ifdef MSDOS
#include <string.h>
#endif

#define EXT extern
#include "wisp.h"
#include "directiv.h"
#include "wispfile.h"
#include "token.h"
#include "node.h"
#include "wmalloc.h"

extern int 	symbzero;
extern char	decimal_is;

char	*figcon1_ring = 0;
struct	figcon1_struct
{
	int	value;
	char	name[40];
} figcon1_item;
int figcon1_compare();

NODE get_statement();

NODE identification_division();
NODE environment_division();
NODE configuration_section();

/*
**	Routine:	identification_division()
**
**	Function:	To process the IDENTIFICATION DIVISION.
**
**	Description:	Get and put statements until out of this division then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not IDENTIFICATION DIVISION then an error will be generated.
**
**	Arguments:
**	the_statement	The pre-loaded first statement.
**
**	Globals:	None
**
**	Return:		The next statement that is not part of this division.
**
**	Warnings:	None
**
**	History:	
**	06/01/93	Written by GSL
**
*/
NODE identification_division(the_statement)
NODE	the_statement;
{
	NODE	next_statement;
	NODE	curr_node, temp_node;
	int	fix_idcomments;

	curr_node = the_statement->next;

	if (eq_token(curr_node->token,KEYWORD,"IDENTIFICATION"))
	{
		write_log("WISP",'I',"IDENTDIV","Processing IDENTIFICATION DIVISION.");
		tput_statement(12,the_statement);
		free_statement(the_statement);
	}
	else
	{
		write_log("WISP",'F',"IDENTDIV","IDENTIFICATION DIVISION not found, possibly not a COBOL program.");
		exit_with_err();
	}

	fix_idcomments = 0;
	next_statement = NULL;

	for(;;)
	{
		if (next_statement)
		{
			the_statement = next_statement;
			next_statement = NULL;
		}
		else
		{
			the_statement = get_statement();
		}

		curr_node = the_statement->next;

		if (eq_token(curr_node->token,KEYWORD,"PROGRAM-ID"))
		{
			tput_statement(12,the_statement);			/* Write the PROGRAM-ID. paragraph header.	*/
			free_statement(the_statement);

			the_statement = get_statement();			/* Get the program name.			*/
			curr_node = the_statement->next;
			strcpy(prog_id,token_data(curr_node->token));
			write_log("WISP",'I',"PROGRAMID", "Found PROGRAM-ID %s.",prog_id);
			if (init_data)
			{
				tie_down(curr_node,make_clause(12, "IS INITIAL PROGRAM", NULL));
			}
			if (!curr_node->next->token || PERIOD != curr_node->next->token->type)
			{
				write_log("WISP",'W',"MISSPERIOD","Missing period following \"PROGRAM-ID. %s\"", 
					token_data(curr_node->token));

				temp_node = curr_node->next;
				curr_node->next = maketoknode(make_token(PERIOD,"."));

				while( temp_node && temp_node->token && 
					temp_node->token->line == curr_node->token->line )
				{
					/*
					**	Step thru trailing tokens on this line following the program name.
					**	(These nodes are being left floating in limbo.)
					*/
					temp_node = temp_node->next;
				}
				if (temp_node)
				{
					if (temp_node->token)
					{
						next_statement = makenode(NODE_START,NULL,NULL,NULL);
						next_statement->next = temp_node;
					}
				}
			}
			else
			{
				fix_idcomments = 1;
			}
		}
		else if (fix_idcomments && IDCOMMENT==curr_node->token->type)
		{
			char	buff[128];

			/*
			**	The Wang was looser regarding comment entries in ID Div.
			**	It allowed them following the program name, these we have to change
			**	into '*' type comments.
			*/

			memset(buff,' ',curr_node->token->column);
			strcpy(&buff[curr_node->token->column-1],curr_node->token->data);
			buff[6] = '*';
			edit_token(curr_node->token,buff);
			curr_node->token->column = 1;
		}
		else if (eq_token(curr_node->token,KEYWORD,"AUTHOR") 		||
			 eq_token(curr_node->token,KEYWORD,"INSTALLATION") 	||
			 eq_token(curr_node->token,KEYWORD,"DATE-WRITTEN") 	||
			 eq_token(curr_node->token,KEYWORD,"DATE-COMPILED") 	||
			 eq_token(curr_node->token,KEYWORD,"SECURITY")		  )
		{
			fix_idcomments = 0;
		}
		else if (eq_token(curr_node->token,KEYWORD,"ENVIRONMENT") 	||
			 eq_token(curr_node->token,KEYWORD,"DATA") 		||
			 eq_token(curr_node->token,KEYWORD,"PROCEDURE")		  )
		{
			return(the_statement);
		}
		else if (curr_node->token->column < 12)
		{
			write_tlog(curr_node->token, 
				"WISP",'F',"PARSEID","Expecting next DIVISION found %s.",
				token_data(curr_node->token));
			exit_with_err();
		}

		tput_statement(12,the_statement);
		free_statement(the_statement);
	}
}

/*
**	Routine:	environment_division()
**
**	Function:	To process the ENVIRONMENT DIVISION.
**
**	Description:	Get and put statements until out of this division then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not ENVIRONMENT DIVISION then one will be generated.
**
**	Arguments:
**	the_statement	The pre-loaded first statement.
**
**	Globals:	None
**
**	Return:		The next statement that is not part of this division.
**
**	Warnings:	None
**
**	History:	
**	06/01/93	Written by GSL
**
*/
NODE environment_division(the_statement)
NODE the_statement;
{
	if (eq_token(the_statement->next->token,KEYWORD,"ENVIRONMENT"))
	{
		write_log("WISP",'I',"ENVIRONDIV","Processing ENVIRONMENT DIVISION.");
		division = ENVIRONMENT_DIVISION;

		if (!comments) tput_blank();
		tput_statement(12,the_statement);
		free_statement(the_statement);

		the_statement = get_statement();

	}
	else if (eq_token(the_statement->next->token,KEYWORD,"DATA") 	 ||
		 eq_token(the_statement->next->token,KEYWORD,"PROCEDURE")  )
	{
		write_log("WISP",'I',"ENVIRONDIV","ENVIRONMENT DIVISION not found.");

		/*
		**	The ENVIRONMENT DIVISION was not found so one will be generated,
		**	but first write out any fluff that came out of the IDENTIFICATION
		**	DIVISION.
		*/
		tput_statement(12,the_statement->down);
		free_statement(the_statement->down);
		the_statement->down = NULL;

		tput_line_at(8, "ENVIRONMENT DIVISION.");
	}
	else
	{
		write_tlog(the_statement->next->token, 
				"WISP",'F',"PARSEENV","Expecting ENVIRONMENT, DATA, or PROCEDURE DIVISION found %s.",
				token_data(the_statement->next->token));
		exit_with_err();
	}

	/*
	**	CONFIGURATION SECTION
	*/
	the_statement = configuration_section(the_statement);

	/*
	**	INPUT-OUTPUT SECTION
	*/
	the_statement = (NODE)input_output_section(the_statement);

	return(the_statement);
}

/*
**	Routine:	configuration_section()
**
**	Function:	To process the CONFIGURATION SECTION.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not CONFIGURATION SECTION then one will be generated.
**
**	Arguments:
**	the_statement	The pre-loaded first statement.
**
**	Globals:	None
**
**	Return:		The next statement that is not part of this section.
**
**	Warnings:	None
**
**	History:	
**	06/01/93	Written by GSL
**
*/
NODE configuration_section(the_statement)
NODE the_statement;
{
	NODE	curr_node, temp_node;
	NODE	next_statement;
	NODE	special_statement, figcon_statement;
	NODE	hold_special;
	int	rc;

	if (eq_token(the_statement->next->token,KEYWORD,"CONFIGURATION"))
	{
		write_log("WISP",'I',"CONFIGSECT","Processing CONFIGURATION SECTION.");

		tput_statement(12,the_statement);
		free_statement(the_statement);
		next_statement = NULL;
	}
	else
	{
		write_log("WISP",'I',"CONFIGSECT","CONFIGURATION SECTION not found.");

		tput_line_at(8, "CONFIGURATION SECTION.");

		next_statement = the_statement;
	}

	special_statement = NULL;
	figcon_statement = NULL;
	hold_special = NULL;

	for(;;)
	{
		if (next_statement)
		{
			the_statement = next_statement;
			next_statement = NULL;
		}
		else
		{
			the_statement = get_statement();
		}

		if (eq_token(the_statement->next->token,KEYWORD,"SOURCE-COMPUTER") ||
		    eq_token(the_statement->next->token,KEYWORD,"OBJECT-COMPUTER")    )
		{
			tput_statement(12,the_statement);
			free_statement(the_statement);

			the_statement = get_statement();
			curr_node = the_statement->next;
			if (curr_node->token->column < 12)
			{
				next_statement = the_statement;
				continue;
			}

			if (vax_cobol)
				edit_token(curr_node->token,"VAX");
			else if (dos_cobol)
				edit_token(curr_node->token,"MSDOS");
			else
				edit_token(curr_node->token,"UNIX");

			curr_node = curr_node->next;
			if (IDENTIFIER == curr_node->token->type)
			{
				/*
				**	This is to handle 2 part names like "WANG VS-1000".
				*/
				cleartoknode(curr_node);
			}

			tput_statement(12,the_statement);
			free_statement(the_statement);
		}
		else if (eq_token(the_statement->next->token,KEYWORD,"SPECIAL-NAMES"))
		{
			NODE	symbolic_node, class_node, currency_node, dp_node, period_node;

			tput_statement(12,the_statement);
			free_statement(the_statement);
			wrote_special_names = 1;

			the_statement = get_statement();
			curr_node = the_statement->next;
			if (curr_node->token->column < 12)
			{
				next_statement = the_statement;
				continue;
			}

			special_statement = the_statement;
			symbolic_node = class_node = currency_node = dp_node = period_node = NULL;

			while( NODE_END != curr_node->type )
			{
				if (eq_token(curr_node->token,KEYWORD,"SYMBOLIC"))
				{
					symbolic_node = curr_node;
				}
				else if (eq_token(curr_node->token,KEYWORD,"CLASS"))
				{
					class_node = curr_node;
				}
				else if (eq_token(curr_node->token,KEYWORD,"CURRENCY"))
				{
					currency_node = curr_node;
				}
				else if (eq_token(curr_node->token,KEYWORD,"DECIMAL-POINT"))
				{
					write_log("WISP",'I',"DPCOMMA","Found DECIMAL-POINT IS COMMA clause.");
					dp_node = curr_node;
					decimal_is = ',';
					set_dpcomma();
				}
				else if (PERIOD == curr_node->token->type)
				{
					period_node = curr_node;
				}
				curr_node = curr_node->next;
			}

			/*
			**	Find the node where we will insert SYMBOLICS before
			*/
			hold_special = NULL;
			if 	(class_node) 	hold_special = class_node;
			else if (currency_node) hold_special = currency_node;
			else if (dp_node) 	hold_special = dp_node;
			else if (period_node) 	hold_special = period_node;

			if (hold_special)
			{
				/*
				**	Write out the first part of the statement up to the hold_node.
				*/

				curr_node = the_statement;
				while( curr_node != hold_special )
				{
					tput_token(12,curr_node->token);
					tput_statement(12,curr_node->down);
					curr_node = curr_node->next;
				}
			}
			else
			{
				free_statement(12,the_statement);
				special_statement = NULL;
				write_log("WISP",'E',"SPECIAL","Error parsing SPECIAL-NAMES para, period not found.");
			}
		}
		else if (eq_token(the_statement->next->token,KEYWORD,"FIGURATIVE-CONSTANTS"))
		{
			write_log("WISP",'I',"BEGINFIGCON","Begin FIGURATIVE-CONSTANTS Analysis.");
			tput_fluff(the_statement);
			free_statement(the_statement);

			init_figcons();

			the_statement = get_statement();
			curr_node = the_statement->next;
			if (curr_node->token->column < 12)
			{
				next_statement = the_statement;
				continue;
			}

			while (PERIOD != curr_node->token->type)
			{
				int	figvalue;

				if (IDENTIFIER != curr_node->token->type)
				{
					if (KEYWORD == curr_node->token->type)
					{
						write_tlog(curr_node->token,"WISP",'W',"FIGCONS",
							"Reserved word [%s] found parsing FIGURATIVE CONSTANTS",
							token_data(curr_node->token));
					}
					else
					{
						write_tlog(curr_node->token,"WISP",'E',"FIGCONS",
							"Error parsing FIGURATIVE CONSTANTS invalid token [token=%s]",
							token_data(curr_node->token));
						break;
					}
				}
				temp_node = curr_node;

				tput_fluff(curr_node->down);
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,KEYWORD,"IS"))
				{
					tput_fluff(curr_node->down);
					curr_node = curr_node->next;
				}

				if (LITERAL != curr_node->token->type)
				{
					write_tlog(curr_node->token,"WISP",'E',"FIGCONS",
							"Error parsing FIGURATIVE CONSTANTS [token=%s]",
							token_data(curr_node->token));
					break;
				}

				sscanf(&curr_node->token->data[1],"%x",&figvalue);	/* Scan for the hex value		*/

				if (figvalue > 255)
				{
					char	figbuff[16];

					/*
					**	Handle 2 byte figcons
					*/
					strcpy(fig_cons[fig_count],token_data(temp_node->token));

					figbuff[0] = curr_node->token->data[1];
					figbuff[1] = curr_node->token->data[2];
					figbuff[2] = (char)0;
					sscanf(figbuff,"%x",&fig_val[fig_count][0]);

					figbuff[0] = curr_node->token->data[3];
					figbuff[1] = curr_node->token->data[4];
					figbuff[2] = (char)0;
					sscanf(figbuff,"%x",&fig_val[fig_count][1]);

					write_log("WISP",'I',"2BYTEFIGCON","2 byte Figurative constant, values are %d, %d.",
										fig_val[fig_count][0],fig_val[fig_count][1]);
					fig_count++;
				}
				else
				{
					if (!figcon1_ring)
					{
						if( rc = ring_open(&figcon1_ring,sizeof(struct figcon1_struct),25,25,
									figcon1_compare,1) )
						{
							write_log("WISP",'F',"RINGOPEN",
								"Unable to open ring [figcon1_ring] rc=%d [%s]",rc,ring_error(rc));
							exit_with_err();
						}
					}
					strcpy(figcon1_item.name,token_data(temp_node->token));
					figcon1_item.value = figvalue;
					if (rc = ring_add(figcon1_ring,0,&figcon1_item)) /* Store the figcon into the ring	*/
					{
						write_log("WISP",'F',"RINGADD",
							"Unable to add to ring [figcon1_ring] rc=%d [%s]",rc,ring_error(rc));
						exit_with_err();
					}
				
					if (!symbzero) figvalue++;		/* Add 1 to correct collating sequence	*/
					tput_line_at(12, "SYMBOLIC %s IS %d\n",token_data(temp_node->token),figvalue);
				}
				tput_fluff(curr_node->down);
				curr_node = curr_node->next;
			}

			free_statement(the_statement);
		}
		else if (eq_token(the_statement->next->token,KEYWORD,"INPUT-OUTPUT") 	||
			 eq_token(the_statement->next->token,KEYWORD,"DATA") 		||
			 eq_token(the_statement->next->token,KEYWORD,"PROCEDURE")	  )
		{
			/*
			**	Found end of CONFIGURATION SECTION.
			*/

			init_figcons();						/* Write canned figcons, it not already done	*/
			finish_figcons();

			if (hold_special)					/* Write trailing SPECIAL-NAMES statement	*/
			{
				if (PERIOD != hold_special->token->type)
				{
					tput_flush();
				}
				tput_statement(12,hold_special);
			}
			else
			{
				tput_clause(12, ".");
			}

			if (special_statement)
			{
				free_statement(special_statement);
			}

			return(the_statement);
		}
		else
		{
			write_tlog(the_statement->next->token,
				"WISP",'F',"PARSEENV","Error parsing the ENVIRONMENT DIVISION, unrecognized token [%s]",
				token_data(the_statement->next->token));
			exit_with_err();
		}
	}
		
}

static int flag_is_dpcomma = 0;
set_dpcomma()
{
	flag_is_dpcomma = 1;
}
int is_dpcomma()
{
	return(flag_is_dpcomma);
}


int figcon1_compare(p1,p2)
struct figcon1_struct *p1, *p2;
{
	int	cmp;

	cmp = strcmp(p1->name, p2->name);
	return(cmp);
}

int isafigcon1(item)
char	*item;
{
	int	rc;
	if (!figcon1_ring) return(0);
	if (strlen(item) > 39) return(0);					/* Item is too big to be a figcon		*/

	strcpy(figcon1_item.name,item);
	rc = ring_find(figcon1_ring,&figcon1_item,0,0);
	return(!rc);
}

init_figcons()
{
	static int first = 1;

	int i,j,l,k,offset;
	int	use_copy, output_copy;
	char	COPY_SYMBOLICS[40];
	cob_file	*cob_file_ptr;

	if (!first) return(0);
	first = 0;

	use_copy = 0;									/* Use a "COPY" statement		*/
	output_copy = 0;								/* Write the copy code.			*/

	if (symbzero) 	strcpy(COPY_SYMBOLICS,"wc001x04.cpy");
	else    	strcpy(COPY_SYMBOLICS,"wc001004.cpy");

	write_log("WISP",'I',"INITFIGCON","Creating default FIGURATIVE-CONSTANTS.");

	if (!wrote_special_names)
	{
		tput_line_at(8, "SPECIAL-NAMES.");					/* Output special names section		*/
	}

	if ( writing_cob_main() || !copylib )						/* If not writing to a copybook file	*/
	{
		tput_line_at(12,"COPY \"%s\".",COPY_SYMBOLICS);				/* Write the copy statement		*/
		use_copy = 1;
		if ( access(COPY_SYMBOLICS,0) != 0 )					/* If copybook doesn't exist ...	*/
		{
			output_copy = 1;
			cob_file_ptr = open_cob_file(COPY_SYMBOLICS,FOR_OUTPUT,1);	/* open it and start writing.		*/
			override_output_stream(cob_file_ptr);
		}
	}
	else
	{
		output_copy = 1;							/* Just output the code to the file	*/
	}
	
	if (output_copy)
	{
		tput_blank();
		tput_scomment("***********************************************");
		tput_scomment("*******          WISP SYMBOLICS         *******");
		tput_scomment("***********************************************");

		if (symbzero)	offset=0;
		else		offset=1;

		tput_line_at(12, "SYMBOLIC WISP-SCWCC   IS %d",(160+offset));
		tput_line_at(12, "SYMBOLIC WFAC-MODIFY  IS %d",(129+offset));
		tput_line_at(12, "SYMBOLIC WFAC-PROTECT IS %d",(140+offset));
		tput_line_at(12, "SYMBOLIC");

		for (i=0; i<256; i += 2)
		{
			tput_line_at(12, "DEC-BYTE-%d IS %d", i,   i+offset   );
			tput_line_at(40, "DEC-BYTE-%d IS %d", i+1, i+offset+1 );
		}
	}

	if (use_copy && output_copy)
	{
		release_output_stream();
		close_cob_file(cob_file_ptr);
	}

}

finish_figcons()
{
	static	int	first = 1;
	int	offset;

	if (!first) return(0);
	first = 0;

	if (symbzero)	offset=0;
	else		offset=1;

	tput_line(	 "           SYMBOLIC EFFAC IS %d",(140+offset));
	write_log("WISP",'I',"FINFIGCON","Finish FIGURATIVE-CONSTANTS Analysis.");
}

