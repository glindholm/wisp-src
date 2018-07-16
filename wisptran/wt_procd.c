static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		wt_procd.c
**
**	Project:	WISP/TRAN
**
**	RCS:		$Source:$
**
**	Purpose:	Main PROCEDURE DIVISION routines
**
**	Routines:	
*/

/*
**	Includes
*/

#include <string.h>
#include <assert.h>

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "cobfiles.h"
#include "wispfile.h"
#include "statment.h"
#include "wt_disp.h"
#include "wt_procd.h"
#include "wt_locks.h"
#include "reduce.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

int	mwconv_flag=0;


/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
static void end_of_procedure_division(void);
static NODE parse_declaratives(NODE the_sentence);
static void parse_sentence(NODE the_sentence);
static NODE add_wisp_main_section(NODE the_sentence);
static NODE parse_one_verb(NODE the_statement, NODE the_sentence);
static NODE parse_move(NODE the_statement, NODE the_sentence);
static NODE parse_perform(NODE the_statement, NODE the_sentence);
static NODE parse_stop(NODE the_statement);
static NODE parse_exit(NODE the_statement);
static NODE parse_set(NODE the_statement);
static NODE parse_else(NODE the_statement);
static NODE parse_go(NODE the_statement);
static void exit_copy_mode(NODE the_sentence);
static NODE parse_evaluate(NODE the_statement, NODE the_sentence);


/*
**	Routine:	procedure_division()
**
**	Function:	To process the PROCEDURE DIVISION.
**
**	Description:	Get and put statements until end of program.
**
**			If the first statement is not PROCEDURE DIVISION then an error will be generated.
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
**	06/02/93	Written by GSL
**
*/
void procedure_division(NODE the_statement)
{
	NODE the_sentence;
	
	if (eq_token(the_statement->next->token,KEYWORD,"PROCEDURE"))
	{
		write_log("WISP",'I',"PROCDIV","Processing PROCEDURE DIVISION.");
		division = PROCEDURE_DIVISION;

		if (!comments) tput_blank();
		tput_statement(12,the_statement);
		free_statement(the_statement);
	}
	else
	{
		write_log("WISP",'F',"PROCDIV","PROCEDURE DIVISION not found, possible earlier syntax error.");
		exit_with_err();
	}

	the_sentence = get_sentence_tree();
	the_sentence = parse_declaratives(the_sentence);

	the_sentence = add_wisp_main_section(the_sentence);
	
	while(the_sentence)
	{
		parse_sentence(the_sentence);
		the_sentence = get_sentence_tree();
	}

	end_of_procedure_division();
}

static void end_of_procedure_division(void)
{
	exit_copy_mode(NULL);

	if (decl_performs_cnt)
	{
		write_log("WISP",'I',"PARSREF",
			  "Paragraphs referenced in the PROCEDURE DIVISION by DECLARATIVES.\n");
		new_para();						/* New paragraphs were discovered.	*/
	}

	/*
	**	If writing to a copybook then we need to close the copybook
	**	and switch the context back to the main file before
	**	ending stuff is generated.
	**	Create a comment token with a main context and tput it will
	**	cause this context switch to occur.
	*/
	if (copylib)
	{
		NODE the_statement;

		the_statement = make_statement("      *", main_cob_context);
		tput_statement(8,the_statement);
		the_statement = free_statement(the_statement);
		tput_flush();
	}
}


NODE is_section(NODE the_sentence)
{
	NODE first_node;

	first_node = first_non_fluff_node(the_sentence);
	if (first_node && first_node->next && eq_token(first_node->next->token, KEYWORD, "SECTION"))
	{
		return first_node;
	}
	else
	{
		return NULL;
	}
}

NODE is_paragraph(NODE the_sentence)
{
	NODE first_node;

	first_node = first_non_fluff_node(the_sentence);
	if (first_node && first_node->token && IDENTIFIER == first_node->token->type)
	{
		return first_node;
	}
	else
	{
		return NULL;
	}
}

static void parse_sentence(NODE the_sentence)
{
	NODE	the_statement;
	NODE	first_node;
	
	if (!the_sentence)
	{
		return;
	}
	
	if (is_paragraph(the_sentence))
	{
		exit_copy_mode(the_sentence);
		the_sentence = chk_dpar(the_sentence);

		if (the_sentence)
		{
			tput_statement(8, the_sentence);
			the_sentence = free_statement(the_sentence);
		}

		return;
	}

	the_statement = NULL;
	
	for(;;)
	{
		if (the_statement)
		{
			/* Already have the next statement */
		}
		else
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}

		if (!the_statement)
		{
			the_sentence = free_statement(the_sentence);
			return;
		}

		if (is_verb_statement(the_statement))
		{
			the_statement = parse_one_verb(the_statement, the_sentence);
		}
		else if (the_statement->next && the_statement->next->token && PERIOD == the_statement->next->token->type)
		{
			decontext_statement(the_statement);
			tput_statement(12, the_statement);
			the_statement = free_statement(the_statement);
		}
		else
		{
			/*
			**	Not a verb or a period!
			*/
			first_node = first_non_fluff_node(the_statement);

			if (first_node)
			{
				write_tlog(first_node->token,"WISP",'E',"PARSE","Expected VERB found [%s].", 
					   token_data(first_node->token));
			}

			tput_statement(12, the_statement);
			the_statement = free_statement(the_statement);
		}
	}
}

static NODE parse_declaratives(NODE the_sentence)
{
	NODE first_node;
	int	i,j;
	
	/*
	**	[DECLARITIVES.
	**		section-name SECTION.
	**		USE statement.
	**	END DECLARITIVES.]
	*/

	first_node = first_non_fluff_node(the_sentence);
	if (first_node && eq_token(first_node->token, KEYWORD, "DECLARATIVES"))
	{
		in_decl = 1;

		tput_statement(8, the_sentence);
		the_sentence = free_statement(the_sentence);
		the_sentence = get_sentence_tree();
	}
	else
	{
		/* No DECLARATIVES so generate our own */
		in_decl = 0;

		if (prog_cnt - prog_sort)					/* and there are valid files.		*/
		{								/* so put ours in place			*/
			write_log("WISP",'I',"INSERTDECL","Inserting default DECLARATIVES.");
			tput_line_at(8, "DECLARATIVES.");
			gen_defdecl();						/* And generate the default declaratives.*/
			tput_line_at(8, "END DECLARATIVES.");
			tput_blank();
		}

		for (i=0; i<proc_performs_cnt; i++)				/* Delete any procedure division copys	*/
		{								/* (from .OPT file)			*/
			proc_performs[i][0] = '\0';
		}

		return the_sentence;
	}

	/*
	**	Loop thru the decaratives processing sentences and looking for END DECLARATIVES
	*/
	for(;;)
	{
		if (del_use)
		{
			if (is_section(the_sentence))
			{
				del_use = 0;
			}
			else
			{
				the_sentence = free_statement(the_sentence);
			}
		}

		parse_sentence(the_sentence);

		the_sentence = get_sentence_tree();
		if (!the_sentence)
		{
			return the_sentence;
		}

		first_node = first_non_fluff_node(the_sentence);
		if (first_node && eq_token(first_node->token, KEYWORD, "END") &&
		    first_node->next && eq_token(first_node->next->token, KEYWORD, "DECLARATIVES"))
		{
			/* We found the END DECLARATIVES, break out and finish processing */
			del_use = 0;
			exit_copy_mode(the_sentence);
			break;
		}
		
	}

	/*
	**	Do END DECLARATIVES processing
	*/

	gen_dexit();							/* First terminate any unfinished one.	*/

	tput_line_at(8,  "WISP-DECLARATIVES-DISPLAY SECTION.");
	tput_line_at(12, "USE AFTER STANDARD ERROR PROCEDURE ON WISP-DECLARATIVES-FILE.");

	if (proc_paras_cnt)						/* Were there any paragraphs to copy?	*/
	{								/* If there were, include a COPY state-	*/
		tput_line_at(12, "COPY \"%s\".",dcl_fname);		/* ment for the wisp-generated LIB file.*/
		decl_stop_exit = 1;					/* Just in case copybook has stop/exit	*/
	}

	gen_screen_paras();						/* Now generate the screens used in DECL*/
	tput_line_at(8, "END-WISP-DECLARATIVES.");
	if (decl_stop_exit) d_wisp_exit_para();

	/* Now see if the paragraphs to be	*/
	for (i=0; i<proc_performs_cnt; i++)				/* copied to the PROCEDURE DIVISION	*/
	{								/* really exist (from .OPT file)	*/
		for (j=0; j<decl_paras_cnt; j++)			/* These are paragraphs which exist in	*/
		{							/* DECL, but are performed by proc div.	*/
			if (!strcmp(proc_performs[i],decl_paras[j]))	/* See if they match. if they do, we	*/
			{						/* can skip the loop.		 	*/
				break;					/* and stop looking for this one.	*/
			}
		}
		if (j == decl_paras_cnt) proc_performs[i][0] = '\0';	/* Wasn't in the list, delete it.	*/
	}

	if (decl_performs_cnt) check_decl();				/* If there are any unaccounted for	*/
										/* performs, examine the declaratives	*/
										/* table.				*/

	if (prog_cnt - prog_sort)					/* If there were valid files...		*/
	{
		gen_defdecl();						/* Generate declaratives for files with	*/
	}								/* no declaratives.			*/

	tput_statement(first_node->token->column, the_sentence);
	free_statement(the_sentence);
	in_decl = 0;

	return get_sentence_tree();
}

static NODE add_wisp_main_section(NODE the_sentence)
{
	if (is_section(the_sentence))
	{
		tput_statement(8, the_sentence);
		free_statement(the_sentence);
		the_sentence = get_sentence_tree();
	}
	else
	{
		tput_line_at(8, "WISP-MAIN SECTION.");
	}
	
	tput_line_at(8, "WISP-START-PROGRAM.");

	tput_line_at(12, "CALL \"initwisp2\" USING WISP-TRAN-VERSION,");
	tput_line_at(12, "                       WISP-LIB-VERSION,");
	tput_line_at(12, "                       WISP-COBOL-TYPE,");
	tput_line_at(12, "                       WISP-APPLICATION-NAME,");
	tput_line_at(12, "                       WISPRUNNAME,");
	tput_line_at(12, "                       WISP-SWAP-ON,");
	tput_line_at(12, "                       WISP-ERR-LOGGING.");

	return the_sentence;
}


int sect_num = 0;

/*
**	Exit paragraph relocation copying.
**
**	At the end of the input file this will be called with the_sentence=NULL.
*/
static void exit_copy_mode(NODE the_sentence)
{
	extern int sect_num;								/* Current SECTION number.		*/

	tput_flush();

	if (copy_sect)									/* Have we been copying a SECTION?	*/
	{
		if (!the_sentence || is_section(the_sentence))				/* Is this a section?			*/
		{
			TOKEN	*tok;
			char	buff[80];

			copy_sect = 0;							/* Only stop on a new section.		*/
			copy_to_dcl_file = 0;

			sprintf(buff,"       WISP-SECTION-%d-END.",sect_num++);		/* Put in the ending paragraph.		*/
			tok = make_token(NOPROCESS,buff);
			tok->column = 1;
			tok->column_fixed = 1;
			split_token_to_dcl_file(1, tok);
			free_token(tok);
		}
	}
	else if (copy_to_dcl_file)							/* Have we been copying paragraphs?	*/
	{
		copy_to_dcl_file = 0;							/* reset the mode.			*/
	}
	else if (copy_to_dtp_file)							/* Have we been copying paragraphs?	*/
	{
		copy_to_dtp_file = 0;							/* reset the mode.			*/
	}
}



/*
**	ROUTINE:	parse_error_clause_verb()
**
**	FUNCTION:	Parse a generic verb statement with a possible error clause.
**
**	DESCRIPTION:	This parses verb statements with the following syntax.
**
**				verb [tokens] ...
**				    [ error_clause imperative-statement ]
**				    [ NOT error_clause imperative-statement ]
**				    [ END-verb ]
**
**				error_clause:  INVALID [KEY]
**					       [AT] END
**					       [ON] OVERFLOW
**					       [ON] EXCEPTION
**					       [ON] [SIZE] ERROR
**
**	ARGUMENTS:	
**	the_statement	The first statement fragment
**	the_sentence	The current sentence or NULL
**
**	GLOBALS:	None
**
**	RETURN:		The next statement fragment that is not part of this verb statement or NULL.
**
**	WARNINGS:	None
**
*/
NODE parse_error_clause_verb(NODE the_statement, NODE the_sentence)
{
	NODE verb_node, curr_node;
	char	end_verb[40];

	verb_node =  is_verb_statement(the_statement);
	
	if (!verb_node)
	{
		write_log("WISP",'E',"PARSE","Expecting verb found [%s].",token_data(the_statement->next->token));
		return the_statement;
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s statement.", token_data(verb_node->token));

	sprintf(end_verb, "END-%s",token_data(verb_node->token));

	tput_statement(verb_node->token->column, the_statement);
	
	the_statement = free_statement(the_statement);
	verb_node = NULL;
				
	/*
	**	[NOT] INVALID [KEY]
	**	[NOT] [AT] END
	**	[NOT] [ON] OVERFLOW
	**	[NOT] [ON] EXCEPTION
	**	[NOT] [ON] [SIZE] ERROR
	*/

	the_statement = get_statement_from_sentence(the_sentence);
	if (!the_statement)
	{
		return NULL;
	}

	if (is_verb_statement(the_statement))
	{
		return the_statement;
	}

	curr_node = the_statement->next;
	if (!curr_node)
	{
		tput_statement(12, the_statement);
	
		return the_statement = free_statement(the_statement);
	}
	
	if (eq_token(curr_node->token, KEYWORD, "INVALID") ||
	    eq_token(curr_node->token, KEYWORD, "AT") ||
	    eq_token(curr_node->token, KEYWORD, "ON") ||
	    eq_token(curr_node->token, KEYWORD, "END") ||
	    eq_token(curr_node->token, KEYWORD, "OVERFLOW") ||
	    eq_token(curr_node->token, KEYWORD, "EXCEPTION") ||
	    eq_token(curr_node->token, KEYWORD, "SIZE") ||
	    eq_token(curr_node->token, KEYWORD, "ERROR") )
	{
		curr_node->token->column_fixed = 1;
		tput_statement(curr_node->token->column, the_statement);
		the_statement = free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement) 
		{
			return NULL;
		}

		curr_node = the_statement->next;
		if (!curr_node)
		{
			tput_statement(12, the_statement);
	
			return the_statement = free_statement(the_statement);
		}
	}

	if (eq_token(curr_node->token, KEYWORD, "NOT"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(curr_node->token->column, the_statement);
		the_statement = free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement) 
		{
			return NULL;
		}

		curr_node = the_statement->next;
		if (!curr_node)
		{
			tput_statement(12, the_statement);
	
			return the_statement = free_statement(the_statement);
		}
	}

	if (eq_token(curr_node->token, KEYWORD, end_verb))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(curr_node->token->column, the_statement);
		return free_statement(the_statement);
	}

	return the_statement;
}

/*
**	ROUTINE:	parse_simple_verb()
**
**	FUNCTION:	Handle a simple verb statement.
**
**	DESCRIPTION:	This handles a simple verb statement with no clauses
**			that would come in as separate fragments.
**
**	ARGUMENTS:	
**	the_statement	The first statement fragment
**	the_sentence	The current sentence or NULL
**
**	GLOBALS:	none
**
**	RETURN:		The next statement fragment that is not part of this verb statement or NULL.
**
**	WARNINGS:	none
**
*/
NODE parse_simple_verb(NODE the_statement, NODE the_sentence)
{
	NODE verb_node;

	verb_node =  is_verb_statement(the_statement);
	
	if (!verb_node)
	{
		write_log("WISP",'E',"PARSE","Expecting verb found [%s].",token_data(the_statement->next->token));
		return the_statement;
	}
	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s statement.", token_data(verb_node->token));

	tput_statement(verb_node->token->column, the_statement);
	the_statement = free_statement(the_statement);

	return the_statement;
}

static NODE parse_one_verb(NODE the_statement, NODE the_sentence)
{
	NODE verb_node;
	NODE orig_statement;

	orig_statement = the_statement;

	verb_node =  is_verb_statement(the_statement);
	
	if (!verb_node)
	{
		write_log("WISP",'E',"PARSE","Expecting verb found [%s].",token_data(the_statement->next->token));
		return the_statement;
	}

	if (verb_node->token->column >= 12 ||
	    verb_node->token->column <= 36)
	{
		verb_node->token->column_fixed = 1;
	}

	/*
	**	Parse the verb
	*/
	if (eq_token(verb_node->token,VERB,"ACCEPT"))
	{
		tput_leading_fluff(the_statement);
		the_statement = parse_accept(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"ADD"))
	{
	        the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"ALTER"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"BEGIN"))
	{
		tput_scomment("****** WISP: Verb [%s] is not supported. ******",token_data(verb_node->token));
		write_tlog(verb_node->token,"WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",token_data(verb_node->token));
	        the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"CALL"))
	{
		tput_leading_fluff(the_statement);
		the_statement = parse_call(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"CANCEL"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"CHAIN"))
	{
	        the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"CLOSE"))
	{
		the_statement = parse_close(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"COMMIT"))
	{
		the_statement = parse_commit(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"COMPUTE"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"CONTINUE"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"COPY"))
	{
		write_tlog(verb_node->token,"WISP",'E',"PARSE","Error parsing verb [%s]", token_data(verb_node->token));
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"DELETE"))
	{
		the_statement = parse_delete(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"DESTROY"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"DISPLAY"))
	{
		the_statement = parse_display(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"DIVIDE"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"ENTER"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"EVALUATE"))
	{
		the_statement = parse_evaluate(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"EXIT"))
	{
		the_statement = parse_exit(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"FREE"))
	{
		the_statement = parse_free(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"GO"))
	{
		the_statement = parse_go(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"GOBACK"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"HOLD"))
	{
	        the_statement = parse_hold(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"IF"))
	{
		the_statement = parse_if(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"INITIALIZE"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"INQUIRE"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"INSPECT"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"MERGE"))
	{
		the_statement = parse_sort(the_statement, the_sentence);  /* MERGE & SORT use same logic */
	}
	else if (eq_token(verb_node->token,VERB,"MOVE"))
	{
		the_statement = parse_move(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"MULTIPLY"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"OPEN"))
	{
		the_statement = parse_open(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"PERFORM"))
	{
		the_statement = parse_perform(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"READ"))
	{
		the_statement = parse_read(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"RELEASE"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"RETURN"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"REWRITE"))
	{
		the_statement = parse_rewrite(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"ROLLBACK"))
	{
		tput_scomment("****** WISP: Verb [%s] is not supported. ******",token_data(verb_node->token));
		write_tlog(verb_node->token,"WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",token_data(verb_node->token));
	        the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"SEARCH"))
	{
		the_statement = parse_search(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"SET"))
	{
		the_statement = parse_set(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"SORT"))
	{
		the_statement = parse_sort(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"START"))
	{
		the_statement = parse_start(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"STOP"))
	{
		the_statement = parse_stop(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"STRING"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"SUBTRACT"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"UNLOCK"))
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"UNSTRING"))
	{
		the_statement = parse_error_clause_verb(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"WRITE"))
	{
		the_statement = parse_write(the_statement, the_sentence);
	}
	else
	{
		write_tlog(verb_node->token,"WISP",'W',"VERB","Unrecognized verb [%s]",token_data(verb_node->token));
	        the_statement = parse_simple_verb(the_statement, the_sentence);
	}

	if (orig_statement == the_statement)
	{
		/*
		**	Statement came thru unhandled.
		**	Write it out to prevent a loop.
		*/
		write_tlog(verb_node->token,"WISP",'E',"PARSE","Error parsing verb [%s]",token_data(verb_node->token));

		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);
	}

	return the_statement;
}

/*
**	ROUTINE:	parse_imperative_statements()
**
**	FUNCTION:	Handle an imperative statement clause.
**
**	DESCRIPTION:	Parse the next imperative statements in this sentence.
**			An imperative statement can be multiple consecutive imperative
**			statements.
**
**
**	ARGUMENTS:	
**	the_statement	The next statement if already read or NULL if not read.
**	the_sentence	The sentence to get the statement fragments from or NULL
**			if to use get_statement_fragment().
**
**	GLOBALS:	None
**
**	RETURN:		The next statement fragment which is not part of the imperative
**			statement.
**
**	WARNINGS:	None that will help.
**
*/
NODE parse_imperative_statements(NODE the_statement, NODE the_sentence)
{
	NODE	period_node = NULL;
	int	verbs_processed = 0;
	
	for(;;)
	{
		if (the_statement)
		{
			/* Already have the next statement */
		}
		else
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}

		if (!the_statement)
		{
			break;
		}

		if (!is_verb_statement(the_statement))
		{
			/*
			**	Not a verb so it must be part of a higher level statement
			*/
			break;
		}
		

		the_statement = parse_one_verb(the_statement, the_sentence);

		verbs_processed++;
	}

	if (0==verbs_processed)
	{
		tput_line_at(24, "CONTINUE");
	}
	
	return the_statement;
}


/*
**	ROUTINE:	is_verb_statement()
**
**	FUNCTION:	Check if the first significant token is a verb
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	the_statement	Statement to test
**
**	GLOBALS:	None
**
**	RETURN:		The verb node or NULL
**
**	WARNINGS:	None
**
*/
NODE is_verb_statement(NODE the_statement)
{
	if (the_statement &&
	    the_statement->next && 
	    the_statement->next->token && 
	    the_statement->next->token->type == VERB)
	{
		return the_statement->next;
	}
	else
	{
		return NULL;
	}
}


void add_perf(char *the_name)								/* add a paragraph name to the perform	*/
{
	if (!paracmp(the_name,decl_performs,decl_performs_cnt))				/* If not in table add it.		*/
	{
		strcpy(decl_performs[decl_performs_cnt++],the_name);	
	}
}

static NODE parse_stop(NODE the_statement)
{
	NODE	curr_node, verb_node;
	int	col;
	NODE	trailing_fluff_node = NULL;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"STOP"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected STOP found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	curr_node = verb_node->next;

	col = verb_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;


	if (eq_token(curr_node->token,KEYWORD,"RUN"))
	{
		/*
		**	STOP RUN	->	PERFORM WISP-STOP-RUN
		**			->	PERFORM D-WISP-STOP-RUN
		*/

		edit_token(verb_node->token,"PERFORM");

		if (in_decl)
		{
			edit_token(curr_node->token,"D-WISP-STOP-RUN");
			decl_stop_exit = 1;
		}
		else
		{
			edit_token(curr_node->token,"WISP-STOP-RUN");
		}
		curr_node->token->type = IDENTIFIER;

		tput_statement(col,the_statement);
		return(free_statement(the_statement));
	}

	/*
	**	STOP literal
	*/
	write_log("WISP",'I',"STOPLITRL","Processing STOP literal statement.");

	if (!proc_display)
	{
		write_log("WISP",'I',"SKIPSTOP","Skipped processing of STOP statement.");
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}

	tput_leading_fluff(the_statement);
	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	tput_line_at(col,"MOVE SPACES TO WISP-CRT-SCREEN-AREA");
	edit_token(verb_node->token,"STRING");
	verb_node->token->column = col;
	verb_node->token->column_fixed = 1;

	if (curr_node->token->type != LITERAL)
	{
		char	buff[80];
		sprintf(buff,"\"%s\"",token_data(curr_node->token));
		edit_token(curr_node->token,buff);
		curr_node->token->type = LITERAL;
	}

	delint_statement(the_statement);
	tput_statement(col,the_statement);

	tput_line_at(col+4, "DELIMITED BY SIZE INTO");
	tput_clause (col+8, "WISP-CRT-SCREEN-AREA");
	tput_line_at(col,   "CALL \"WSTOP\" USING WISP-CRT-SCREEN-AREA");
	tput_clause (col,   "END-CALL");

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);

	return(free_statement(the_statement));
}

static NODE parse_exit(NODE the_statement)
{
	NODE	curr_node, verb_node;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"EXIT"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected EXIT found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	curr_node = verb_node->next;

	if (eq_token(curr_node->token,KEYWORD,"PROGRAM"))
	{
		/*
		**	EXIT PROGRAM	->	PERFORM WISP-EXIT-PROGRAM
		**			->	PERFORM D-WISP-EXIT-PROGRAM
		*/

		tput_leading_fluff(the_statement);

		edit_token(verb_node->token,"PERFORM");

		if (in_decl)
		{
			edit_token(curr_node->token,"D-WISP-EXIT-PROGRAM");
			decl_stop_exit = 1;
		}
		else
		{
			edit_token(curr_node->token,"WISP-EXIT-PROGRAM");
		}
		curr_node->token->type = IDENTIFIER;

		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}

	tput_statement(12,the_statement);
	return(free_statement(the_statement));
}

static NODE parse_move(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node, ident_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"MOVE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected MOVE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	col = verb_node->token->column;
	if (col > 24) col = 24;
	if (col < 12) col = 12;

	curr_node = verb_node->next;

	if (eq_token(curr_node->token,KEYWORD,"WITH"))
	{
		NODE	trailing_fluff_node = NULL;

		/*
		**	MOVE WITH CONVERSION identifier-1 TO identifier-2
		**		[ON ERROR 
		**			imperative-statement]		| this portion is not part of the_statement
		**		[END-MOVE]				|
		**
		**	MOVE identifier-1 TO WISP-ALPHA-CONVERSION-FIELD,
		**	PERFORM WISP-MOVE-WITH-CONVERSION,
		**	IF WISP-NO-NUMERIC-CONV-ERROR
		**		COMPUTE identifier-2 = 0 + WISP-CONVERTED-FRACTION-FIELD
		**		IF WISP-CONVERTED-FRACTION-FIELD NOT = identifier-2 THEN
		**			MOVE "Y" TO WISP-NUMERIC-CONVERSION-FLAG
		**		END-IF
		**		ADD WISP-CONVERTED-INTEGER-FIELD TO identifier-2
		**			ON SIZE ERROR 
		**			MOVE "Y" TO WISP-NUMERIC-CONVERSION-FLAG
		**		END-ADD
		**	END-IF
		**	[IF WISP-A-NUMERIC-CONV-ERROR
		**		MOVE ZERO TO identifier-2
		**		[imperative-statement]
		**	 END-IF]
		*/

		write_tlog(verb_node->token,"WISP",'I',"VERB","Processing MOVE WITH CONVERSION Statement.");

		tput_leading_fluff(the_statement);
		trailing_fluff_node = unhook_trailing_fluff(the_statement);

		mwconv_flag = 1;						/* we need to generate mwconv code		*/

		free_token_from_node(curr_node);				/* Delete the WITH token			*/
		curr_node = curr_node->next;
		free_token_from_node(curr_node);				/* Delete the CONVERSION token			*/
		curr_node = curr_node->next;					/* Point to identifier-1			*/
		if (!reduce_data_item(curr_node))				/* Reduce identifier-1				*/
		{
			write_log("WISP",'W',"MOVEWC","Error parsing MOVE WITH CONVERSION, unrecognized data item.[%s]",
							token_data(curr_node->token));
			reduce_one(curr_node);
		}

		if (!eq_token(curr_node->next->token,KEYWORD,"TO"))
		{
			write_log("WISP",'E',"MOVEWC","Error parsing MOVE WITH CONVERSION, expecting keyword \"TO\" found [%s]",
							token_data(curr_node->next->token));
			tput_statement(12,the_statement);
			return(free_statement(the_statement));
		}

		tput_token(col,verb_node->token);				/* Write MOVE					*/
		decontext_statement(curr_node->down);
		tput_statement(col,curr_node->down);				/* Write identifier-1				*/

		curr_node = curr_node->next;					/* Point at "TO" node				*/
		curr_node = curr_node->next;					/* Point at "identifier-2"			*/
		if (!reduce_data_item(curr_node))				/* Reduce identifier-2				*/
		{
			write_log("WISP",'W',"MOVEWC","Error parsing MOVE WITH CONVERSION, unrecognized data item.[%s]",
							token_data(curr_node->token));
			reduce_one(curr_node);
		}

		/*
		**	Un-hook the ident_node
		*/
		ident_node = curr_node->down;
		curr_node->down = NULL;
		decontext_statement(ident_node);
		delint_statement(ident_node);

		tput_clause (col+4, "TO WISP-ALPHA-CONVERSION-FIELD,");
		tput_line_at(col,   "PERFORM WISP-MOVE-WITH-CONVERSION,");

		tput_line_at(col,   "IF WISP-NO-NUMERIC-CONV-ERROR THEN");
		tput_line_at(col,   "    COMPUTE");
		tput_statement(col+8,ident_node);				/* Write identifier-2				*/
		tput_clause (col+8, "= 0 + WISP-CONVERTED-FRACTION-FIELD");
		tput_line_at(col,   "    IF WISP-CONVERTED-FRACTION-FIELD NOT =");
		tput_statement(col+8,ident_node);				/* Write identifier-2				*/
		tput_line_at(col,   "        MOVE \"Y\" TO WISP-NUMERIC-CONVERSION-FLAG");
		tput_line_at(col,   "    END-IF");
		tput_line_at(col,   "    ADD WISP-CONVERTED-INTEGER-FIELD TO");
		tput_statement(col+8,ident_node);				/* Write identifier-2				*/
		tput_line_at(col,   "        ON SIZE ERROR");
		tput_line_at(col,   "        MOVE \"Y\" TO WISP-NUMERIC-CONVERSION-FLAG");
		tput_line_at(col,   "    END-ADD");
		tput_line_at(col,   "END-IF");
		tput_flush();

		curr_node = curr_node->next;

		if (NODE_END != curr_node->type)
		{
			write_tlog(curr_node->token, "WISP",'E',"PARSE","Error parsing MOVE WITH CONVERSION, found [%s]",
				   token_data(curr_node->token));
		}
		
		the_statement = free_statement(the_statement);
		the_statement = get_statement_from_sentence(the_sentence);
		curr_node = the_statement->next;
		
		if (eq_token(curr_node->token,KEYWORD,"ON"))
		{
			curr_node = curr_node->next;
		}
		if (eq_token(curr_node->token,KEYWORD,"ERROR"))
		{
			tput_statement(col, trailing_fluff_node);
			trailing_fluff_node = free_statement(trailing_fluff_node);

			tput_line_at(col,   "IF WISP-A-NUMERIC-CONV-ERROR THEN");
			tput_line_at(col,   "    MOVE ZERO TO");
			tput_statement(col+4,ident_node);			/* Write identifier-2				*/
			
			the_statement = free_statement(the_statement);

			the_statement = parse_imperative_statements(the_statement, the_sentence);

			if (!the_statement)
			{
				the_statement = get_statement_from_sentence(the_sentence);
			}
			curr_node = the_statement->next;

			tput_line_at(col,   "END-IF");
		}

		ident_node = free_statement(ident_node);

		if (eq_token(curr_node->token, KEYWORD, "END-MOVE"))
		{
			the_statement =  free_statement(the_statement);
		}

		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

	}
	else if (init_move && 
		 (eq_token(curr_node->token,KEYWORD,"SPACES") || eq_token(curr_node->token,KEYWORD,"SPACE")) )
	{
		/*
		**	MOVE SPACES TO identifiers ...
		**
		**	INITIALIZE     identifiers ...
		*/

		write_tlog(verb_node->token,"WISP",'I',"MOVESPACES","Changing MOVE SPACE(S) to INITIALIZE.");

		edit_token(verb_node->token,"INITIALIZE");			/* Change the "MOVE" into "INITIALIZE"		*/
		free_token_from_node(curr_node);				/* Delete "SPACES" token			*/

		curr_node = curr_node->next;					/* Point to the "TO" node			*/
		if (eq_token(curr_node->token,KEYWORD,"TO"))
		{
			free_token_from_node(curr_node);			/* Delete "TO" token				*/
		}
		else
		{
			write_log("WISP",'E',"MOVESPACES","Error parsing MOVE SPACES TO, expecting keyword \"TO\" found [%s]",
							token_data(curr_node->token));
		}

		tput_statement(col, the_statement);
		the_statement = free_statement(the_statement);
	}
	else
	{
		the_statement = parse_simple_verb(the_statement, the_sentence);
	}

	return the_statement;
}

static NODE parse_set(NODE the_statement)
{
	NODE	curr_node, verb_node, target_node, figcon_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"SET"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected SET found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;

	figcon_node = verb_node->next;
	curr_node = figcon_node->next;

	if (isafigcon1(token_data(figcon_node->token)) &&
		( eq_token(curr_node->token,KEYWORD,"IN") || 
		  eq_token(curr_node->token,KEYWORD,"OF")    ) )
	{
		char	*bit_xxx;
		NODE	trailing_fluff_node = NULL;

		/*
		**	[SET figcon IN FAC OF target {ON|OFF}]
		**	SET figcon IN      F-target {ON|OFF}
		**	SET figcon OF        target {ON|OFF}
		**
		**	MOVE figcon TO WISP-SET-BYTE
		**	CALL "bit_on" USING WISP-SET-BYTE, target
		**
		** (ACU)
		**	MOVE figcon TO WISP-SET-BYTE
		**	MOVE target TO WISP-TEST-BYTE
		**	CALL "bit_on" USING WISP-SET-BYTE, WISP-TEST-BYTE
		**	MOVE WISP-TEST-BYTE TO target
		*/

		tput_leading_fluff(the_statement);
		trailing_fluff_node = unhook_trailing_fluff(the_statement);

		target_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"OF"))
		{
			/*
			**	May need to make target into a FAC (f-target)
			*/
			if (find_item(token_data(target_node->token)))
			{
				/*
				**	It is a screen item do make into a FAC
				*/
				char	fac[50];

				make_fac(fac,token_data(target_node->token));
				edit_token(target_node->token,fac);
			}
		}

		reduce_data_item(target_node);

		curr_node = target_node->next;
		if (eq_token(curr_node->token,KEYWORD,"ON"))
		{
			bit_xxx = "bit_on";
		}
		else if (eq_token(curr_node->token,KEYWORD,"OFF"))
		{
			bit_xxx = "bit_off";
		}
		else
		{
			write_log("WISP",'E',"SET","Error in SET stmt, expecting ON or OFF found %s.",
				token_data(curr_node->token));
			tput_statement(col,the_statement);
			return( free_statement(the_statement) );
		}

		curr_node = curr_node->next;
		if (NODE_END != curr_node->type)
		{
			write_tlog(curr_node->token, "WISP",'E',"PARSE","Error parsing SET, found [%s]",
				   token_data(curr_node->token));
		}

		decontext_statement(the_statement);

		tput_line_at(col,"MOVE %s TO WISP-SET-BYTE",token_data(figcon_node->token));

		tput_line_at  (col,   "MOVE");
		tput_statement(col+4,      target_node->down);
		tput_clause   (col+4,     "TO WISP-TEST-BYTE");
		tput_line_at  (col,   "CALL \"%s\" USING WISP-SET-BYTE, WISP-TEST-BYTE",bit_xxx);
		tput_line_at  (col,   "MOVE WISP-TEST-BYTE TO");
		tput_statement(col+4,      target_node->down);

		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

		the_statement = free_statement(the_statement);
	}
	else
	{
		tput_statement(col,the_statement);
		the_statement = free_statement(the_statement);
	}

	return the_statement;
}

NODE first_token_node(NODE the_statement)
{
	NODE	verb_node, curr_node;

	curr_node = the_statement;
	if (NODE_TOKEN != curr_node->type) 
	{
		curr_node = curr_node->next;
	}

	verb_node = curr_node;

	/*
	**	We should probably do some checking here to ensure we really have the verb.
	*/

	return verb_node;
}

/*
**	ROUTINE:	parse_go()
**
**	FUNCTION:	Handle the GO statement
**
**	DESCRIPTION:	We don't change the GO statement just check it for
**			paragraph relocation.
**
**	ARGUMENTS:	
**	the_statement	The full statement
**
**	GLOBALS:	None
**
**	RETURN:		NULL
**
**	WARNINGS:	None
**
*/
static NODE parse_go(NODE the_statement)
{
	NODE	verb_node, curr_node;

	verb_node = first_token_node(the_statement);
	if (!eq_token(verb_node->token,VERB,"GO"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected GO found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	curr_node = verb_node->next;

	if (eq_token(curr_node->token,KEYWORD,"TO"))
	{
		curr_node = curr_node->next;
	}

	while(curr_node && curr_node->type != NODE_END)
	{
		if (eq_token(curr_node->token,KEYWORD,"DEPENDING")) break;

		if (curr_node->token && IDENTIFIER == curr_node->token->type)
		{
			char	procedure_name[80], buff[80];

			strcpy(procedure_name,token_data(curr_node->token));

			if (in_decl)								/* is it in the declaratives?	*/
			{
				if (paracmp(procedure_name,proc_paras,proc_paras_cnt))		/* Found it			*/
				{								/* now process it.		*/
					make_fld(buff,procedure_name,"D-");
					edit_token(curr_node->token,buff);			/* replace it in the line.	*/
				}
				else								/* Otherwise save the name for	*/
				{								/* later analysis.		*/
					add_perf(procedure_name);
				}

			}
			else if (copy_to_dcl_file)						/* Are we copying paragraphs?	*/
			{
				if (!paracmp(procedure_name,proc_paras,proc_paras_cnt))		/* If not found			*/
				{
					add_perf(procedure_name);				/* Add to list			*/
				}
			}
			else if (proc_performs_cnt)						/* Check to see if it is a ref	*/
			{									/* to a paragraph in the DECLAR	*/

				if (paracmp(procedure_name,proc_performs,proc_performs_cnt))
				{								/* now process it.		*/
					make_fld(buff,procedure_name,"D-");
					edit_token(curr_node->token,buff);			/* replace it in the line.	*/
				}
			}

		}		
		curr_node = curr_node->next;
	}

	tput_statement(12,the_statement);
	return(free_statement(the_statement));
}

/*
**	PERFORM [procedure-name-1 [{THROUGH|THRU} procedure-name-2]]
**		[condition/times/test/varying]
**		[imperative-statement-1 END-PERFORM]
**
**	Note: Statement contains either procedure-name-1 or imperative-statement-1 but not both.
**
**	We don't change the PERFORM statement just check it for paragraph relocation.
**	We do need to process the whole thing.
**
**	NOTE:	Currently paragraph relocating doesn't deal with PERFORM THRU logic.
*/
static NODE parse_perform(NODE the_statement, NODE the_sentence)
{
	NODE	verb_node, curr_node;
	int	inline_perform;
	int	col;

	verb_node = first_token_node(the_statement);
	if (!eq_token(verb_node->token,VERB,"PERFORM"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected PERFORM found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;

	curr_node = verb_node->next;

	/*
	**	PERFORM [procedure-name-1 ...]
	**		[identifier TIMES ...]
	*/
	if (curr_node && curr_node->token && IDENTIFIER == curr_node->token->type)
	{
		if (curr_node->next && eq_token(curr_node->next->token,KEYWORD,"TIMES"))
		{
			inline_perform = 1;
		}
		else
		{
			inline_perform = 0;
		}
	}
	else
	{
		inline_perform = 1;
	}
	
	if (inline_perform)
	{
		/*
		**	Write the first fragment which will include
		**
		**	Frag-1:		PERFORM [TIMES/TEST/VARYING conditions]
		**	Frag-2:		imperative-statement-1
		**	Frag-3:		END-PERFORM
		*/
		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);

		/*
		**	Next fragment should be the imperative-statement-1
		*/
		the_statement = get_statement_from_sentence(the_sentence);
		if (is_verb_statement(the_statement))
		{
			the_statement = parse_imperative_statements(the_statement, the_sentence);
		}
		else
		{
			curr_node = first_non_fluff_node(the_statement);
			write_tlog(curr_node->token, "WISP",'E',"PARSE",
				   "Error parsing PERFORM (inline), missing imperative statement. Found [%s]",
				   token_data(curr_node->token));
		}
		
		if (!the_statement) 
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}

		curr_node = first_non_fluff_node(the_statement);
		if (curr_node && eq_token(curr_node->token,KEYWORD,"END-PERFORM"))
		{
			curr_node->token->column_fixed = 1;
			tput_statement(col, the_statement);
			the_statement = free_statement(the_statement);
		}
		else
		{
			write_tlog(curr_node->token, "WISP",'E',"PARSE",
				   "Error parsing PERFORM (inline), missing END-PERFORM. Found [%s]",
				   token_data(curr_node->token));
		}

		return the_statement;
	}
	else	/* out of line perform */
	{
		char	procedure_name[80], buff[80];

		/*
		**	Out-Of-Line PERFORM
		**
		**	Handle the paragraph relocating stuff.
		**
		**	Frag-1:		PERFORM procedure-name-1 [THRU procedure-name-2] [TIMES/TEST/VARYING conditions]
		**	Frag-2:
		**
		*/
		strcpy(procedure_name,token_data(curr_node->token));

		if (in_decl)								/* is it in the declaratives?	*/
		{
			if (paracmp(procedure_name,proc_paras,proc_paras_cnt))		/* Found it			*/
			{								/* now process it.		*/
				make_fld(buff,procedure_name,"D-");
				edit_token(curr_node->token,buff);			/* replace it in the line.	*/
			}
			else								/* Otherwise save the name for	*/
			{								/* later analysis.		*/
				add_perf(procedure_name);
			}
			
		}
		else if (copy_to_dcl_file)						/* Are we copying paragraphs?	*/
		{
			if (!paracmp(procedure_name,proc_paras,proc_paras_cnt))		/* If not found			*/
			{
				add_perf(procedure_name);				/* Add to list			*/
			}
		}
		else if (proc_performs_cnt)						/* Check to see if it is a ref	*/
		{									/* to a paragraph in the DECLAR	*/

			if (paracmp(procedure_name,proc_performs,proc_performs_cnt))
			{								/* now process it.		*/
				make_fld(buff,procedure_name,"D-");
				edit_token(curr_node->token,buff);			/* replace it in the line.	*/
			}
		}

		tput_statement(col,the_statement);
		the_statement = free_statement(the_statement);
		return the_statement;
	}
}

/*
**	Frags:	EVALUATE ...
**		WHEN ... 
**			 {imperative-statement-2} ...
**		[END-EVALUATE]
**
*/
static NODE parse_evaluate(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"EVALUATE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected EVALUATE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_statement(col,the_statement);
	the_statement = free_statement(the_statement);

	the_statement = get_statement_from_sentence(the_sentence);
	curr_node = the_statement->next;

	/*
	**	Handle multiple WHEN clauses
	*/
	while (eq_token(curr_node->token, KEYWORD, "WHEN"))
	{
		curr_node->token->column_fixed = 1;
		
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);
			
		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}
	
	if (eq_token(curr_node->token, KEYWORD, "END-EVALUATE"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);
	}
	
	return the_statement;
}

/*
**	Frags:	SEARCH ...
**		[AT END
**			imperative-statement-1]
**		WHEN ... {NEXT STATEMENT        }
**			 {imperative-statement-2} ...
**		[END-SEARCH]
**
*/
NODE parse_search(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"SEARCH"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected SEARCH found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_statement(col,the_statement);
	the_statement = free_statement(the_statement);

	the_statement = get_statement_from_sentence(the_sentence);
	curr_node = the_statement->next;
	
	/*
	**	Handle optional AT END clause
	*/
	if (eq_token(curr_node->token, KEYWORD, "AT") ||
	    eq_token(curr_node->token, KEYWORD, "END"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	/*
	**	Handle multiple WHEN clauses
	*/
	while (eq_token(curr_node->token, KEYWORD, "WHEN"))
	{
		int	next_sentence = 0;

		curr_node->token->column_fixed = 1;
		
		/* check if NEXT SENTENCE is present */
		for(curr_node=curr_node->next; NODE_END != curr_node->type; curr_node=curr_node->next)
		{
			if (eq_token(curr_node->token,KEYWORD,"SENTENCE"))
			{
				next_sentence = 1;
				break;
			}
		}

		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);

		if (!next_sentence)
		{
			the_statement = parse_imperative_statements(the_statement, the_sentence);
		}
			
		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}
	
	if (eq_token(curr_node->token, KEYWORD, "END-SEARCH"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);
	}
	
	return the_statement;
}

/*
**	frag-1:		HOLD ...
**	frag-2:		[TIMEOUT ...]
**	frag-3:		[imperative-statement]
**	frag-4:		[END-HOLD]
*/
NODE parse_hold(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"HOLD"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected HOLD found [%s].", token_data(verb_node->token));
		return(the_statement);
	}
	tput_leading_fluff(the_statement);

	tput_scomment("****** WISP: Verb [%s] is not supported. ******",token_data(verb_node->token));
	write_tlog(verb_node->token,"WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_statement(col, the_statement);
	the_statement = free_statement(the_statement);
	the_statement = get_statement_from_sentence(the_sentence);
	curr_node = first_token_node(the_statement);

	if (curr_node && eq_token(curr_node->token,KEYWORD,"TIMEOUT"))
	{
		int	next_sentence = 0;
		
		/* check if NEXT SENTENCE is present */
		while(curr_node && NODE_END != curr_node->type)
		{
			if (eq_token(curr_node->token,KEYWORD,"NEXT"))
			{
				next_sentence = 1;
				break;
			}
			curr_node = curr_node->next;
		}

		tput_statement(col, the_statement);
		the_statement = free_statement(the_statement);
		the_statement = get_statement_from_sentence(the_sentence);
		curr_node = first_token_node(the_statement);

		/*
		**	If no NEXT SENTENCE then parse the imperative.
		*/
		if (!next_sentence)
		{
			the_statement = parse_imperative_statements(the_statement, the_sentence);

			if (!the_statement) 
			{
				the_statement = get_statement_from_sentence(the_sentence);
			}

			curr_node = first_token_node(the_statement);
		}
	}
	
	if (curr_node && eq_token(curr_node->token,KEYWORD,"END-HOLD"))
	{
		tput_statement(col, the_statement);
		the_statement = free_statement(the_statement);
	}

	return the_statement;
}

/*
**	History:
**	$Log: wt_procd.c,v $
**	Revision 1.22  1998/06/09 17:11:23  gsl
**	Move the parse_rewrite() routine to wt_delet.c
**	
**	Revision 1.21  1998-03-26 15:38:17-05  gsl
**	If last statement in a program is a COPY statement then the
**	end stuff was being generated into the copybook file.
**
**	Revision 1.20  1998-03-26 12:09:21-05  gsl
**	Change MOVE WITH CONVERSION para name
**	,
**
**	Revision 1.19  1998-03-20 17:41:47-05  gsl
**	Fix calls to write_tlog()
**
**	Revision 1.18  1998-03-17 17:06:13-05  gsl
**	Moved OLD to old.c
**
**	Revision 1.17  1998-03-04 14:58:04-05  gsl
**	Add parse_evaluate() and MERGE logic
**
**	Revision 1.16  1998-03-04 13:46:18-05  gsl
**	fix end_of_procedure_division() processing
**	Fix OVERFLOW and EXCEPTION clause processing
**
**	Revision 1.15  1998-03-03 16:20:32-05  gsl
**	Massive COBOL-85 changes.
**
**	Revision 1.14  1997-09-24 15:29:37-04  gsl
**	Remove STOP verb native screen warning
**
**	Revision 1.13  1997-09-18 13:43:32-04  gsl
**	Added STOP verb native screens warning
**
**	Revision 1.12  1997-09-17 17:10:47-04  gsl
**	Change CALL verb to parse_call()
**
**	Revision 1.11  1997-05-08 15:39:11-04  gsl
**	Changed STOP verb to call WSTOP instead of WDISPLAY
**
**	Revision 1.10  1996-06-24 14:18:59-04  gsl
**	fix return codes and unused data items
**
**	Revision 1.9  1995-05-09 04:32:04-07  gsl
**	Fixed SET verb to test for isafigcon1().
**	This corrects the SET condition TO TRUE. bug.
**
**
**
*/
