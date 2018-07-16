			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* Routines to process the START statements.										*/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "token.h"
#include "node.h"
#include "reduce.h"


/*
**	Routine:	parse_start()
**
**	Function:	To process the START statement.
**
**	Description:	Change the Wang START syntax to the target COBOL syntax.
**			The main difference is on Wang the data-name-1 before the relational operator specifies
**			the key to use and if not specified the primary key is used.  Data-name-2 is usually
**			the same as data-name-1 but can be any data item that starts at the same byte location
**			as the active key (data-name-1 or primary).  This data-name-2 is not allowed in ANSI COBOL.
**			To translate the KEY clause we assume that data-name-1 == data-name-2 and delete data-name-1
**			if it was found.  On VAX we add the "REGARDLESS OF LOCK" clause.  Also add "CONTINUE" to 
**			the INVALID KEY clause if followed by a period.
**
**			WCB:	START file-name-1 [KEY [data-name-1] {IS = ...} data-name-2] 
**
**			COB:	MOVE "ST" TO WISP-DECLARATIVES-STATUS
**				START file-name-1 [KEY {IS = ...} data-name-1]
**
**			VAX:	START file-name-1 [KEY {IS = ...} data-name-1] REGARDLESS OF LOCK
**
**	Arguments:	
**	the_statement	The START statement
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	09/20/93	Written by GSL
**
*/
parse_start(the_statement)
NODE the_statement;
{
	NODE	curr_node, start_node, file_node, data_name_1_node, invalid_node;
	int	col,fnum;

	write_log("WISP",'I',"PROCSTART","Processing START Statement.");

	data_name_1_node = NULL;
	invalid_node = NULL;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"START"))
	{
		write_log("WISP",'W',"NOTSTART","NOT A START Statement. STATEMENT NOT PROCESSED");
		tput_statement(12,the_statement);
		return(0);
	}

	start_node = curr_node;
	col = start_node->token->column;

	curr_node = curr_node->next;
	file_node = curr_node;

	if (crt_index(token_data(file_node->token)) != -1)
	{
		/*
		**	Replace a START crt-file with a CONTINUE.
		*/

		write_log("WISP",'I',"STARTCRT","START crt-file replaced with CONTINUE.");
		edit_token(start_node->token,"CONTINUE");
		edit_token(file_node->token,"");
		tput_statement(12,the_statement);
		return(0);
	}

	fnum = file_index(token_data(file_node->token));

	if (fnum == -1)								/* no file matched, error		*/
	{
		write_log("WISP",'F',"STARTFNF",
			"Error -- File %s, referenced by START statement but not Declared.",token_data(file_node->token));
		exit_wisp(EXIT_WITH_ERR);
	}

	write_log("WISP",'I',"HANDLESTART","Handling START of %s.",prog_files[fnum]);

	if (trap_start)
	{
		tput_line_at(col, "MOVE \"%s\" TO",token_data(file_node->token));
		tput_clause (col+4, "WISP-CURRENT-FILE-ID");
	}

	if (copy_to_dcl_file || in_decl)
	{
		write_log("WISP",'W',"STARTINDECL","START statement in DECLARATIVES, record locking not applied.");
	}

	tput_line_at(col,"MOVE \"ST\" TO WISP-DECLARATIVES-STATUS");
	tput_flush();

	if (eq_token(curr_node->next->token,KEYWORD,"KEY"))
	{
		/*
		**	START file-name KEY data-name-1 IS = data-name-2 
		**	START file-name KEY IS = data-name-2
		*/
		curr_node = curr_node->next;
		if (curr_node->next->token && IDENTIFIER == curr_node->next->token->type)
		{
			curr_node = curr_node->next;
			data_name_1_node = curr_node;
			reduce_data_item(data_name_1_node);
		}

		if (data_name_1_node)
		{
			write_log("WISP",'I',"STARTFLDDEL","Field name %s removed from START.",
				token_data(data_name_1_node->down->token));
			free_statement(data_name_1_node->down);
			curr_node->down = NULL;
		}
	}

	/*
	**	Now look for the place to add the "REGARDLESS OF LOCK" clause, it goes after the KEY clause
	**	and before the INVALID clause.  Also find INVALID clause.
	**	
	**	START file-name [KEY clause] [INVALID ...] [END-START]
	**	START file-name [KEY clause] [NOT INVALID ...] [END-START]
	*/
	while( curr_node->next && NODE_END != curr_node->next->type )
	{
		if (IDENTIFIER == curr_node->next->token->type)
		{
			char	buff[80];

			strcpy(buff,token_data(curr_node->next->token));
			key_name(buff,0);					/* Do key name translation		*/
			if (0 != strcmp(buff,token_data(curr_node->next->token)))
			{
				edit_token(curr_node->next->token,buff);
			}
		}

		if (PERIOD == curr_node->next->token->type)
		{
			break;
		}

		if ( eq_token(curr_node->next->token,KEYWORD,"END-START") )
		{
			break;
		}

		if ( eq_token(curr_node->next->token,KEYWORD,"INVALID") )
		{
			invalid_node = curr_node->next;
			break;
		}

		if ( eq_token(curr_node->next->token,KEYWORD,"NOT") &&
		     eq_token(curr_node->next->next->token,KEYWORD,"INVALID") )
		{
			invalid_node = curr_node->next->next;
			break;
		}

		curr_node = curr_node->next;
	}

	if (vax_cobol)
	{
		/*
		**	add REGARDLESS OF LOCK clause (tie down in reverse order)
		*/
		tie_down(curr_node,maketoknode(make_token(KEYWORD,"LOCK")));
		tie_down(curr_node,maketoknode(make_token(KEYWORD,"OF")));
		tie_down(curr_node,maketoknode(make_token(KEYWORD,"REGARDLESS")));
	}

	if (invalid_node)
	{
		/*
		**	... INVALID.
		**	... INVALID KEY.
		*/
		if ( eq_token(invalid_node->next->token,KEYWORD,"KEY") )
		{
			invalid_node = invalid_node->next;
		}

		if (invalid_node->next && invalid_node->next->token && 
		    PERIOD == invalid_node->next->token->type)
		{
			write_log("WISP",'I',"BADINVKEY","Bad START syntax, INVALID KEY followed by a period.");
			tie_next(invalid_node,maketoknode(make_token(VERB,"CONTINUE")));
		}
	}

	tput_statement(col+4,the_statement);

	write_log("WISP",'I',"STARTDONE","Completed START statement.");

	return(0);
}

#ifdef OLD

p_start()										/* Process START statements.		*/
{
	int i,j,hold,fnum,inv_key,n_alt,is_crt;
	char tstr[132];
	char t_mktemp[40];
	short	keeplooping;
	int	lock_clause;

	write_log("WISP",'I',"PROCSTART","Processing START Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be START)	*/
	j = peek_param(o_parms[1]);							/* peek at the file name		*/

											/* is it a crt record?			*/
	is_crt = (crt_index(o_parms[1]) == -1) ? 0 : 1;

	if (is_crt)
	{
		stredt(inline," START "," ");						/* Remove start.			*/
		stredt(inline,o_parms[1],"CONTINUE");					/* Replace crt file with n-s.		*/
		tput_line("%s", inline);						/* Output the line.			*/
		return(0);
	}
	else
	{
		lock_clause = 0;
		i = 0;
		o_parms[5][0] = '\0';							/* no status field yet			*/

		fnum = file_index(o_parms[1]);

		if (fnum == -1)								/* no file matched, error		*/
		{
			write_log("WISP",'F',"STARTFNF",
			"Error -- File %s, referenced by START statement but not Declared.",o_parms[1]);
			exit_wisp(EXIT_WITH_ERR);
		}
		else
		{
			write_log("WISP",'I',"HANDLESTART","Handling START of %s.",prog_files[fnum]);
			strcpy(o_parms[5],prog_fstats[fnum]);				/* copy the status field		*/
		}

		if (trap_start)
		{
			tput_line_at(12, "MOVE \"%s\" TO",o_parms[1]);
			tput_clause (16, "WISP-CURRENT-FILE-ID");
		}

		if (copy_to_dcl_file || in_decl)
			write_log("WISP",'W',"STARTINDECL","START statement in DECLARATIVES, record locking not applied.");


		tput_line("           MOVE \"ST\" TO WISP-DECLARATIVES-STATUS\n");
		tput_line("           START");						/* start a new line			*/


		hold = 0;								/* no hold yet...			*/
		inv_key = 0;								/* no INVALID KEY yet...		*/

		keeplooping = 1;
		do
		{
			if (o_parms[0][0]) stredt(inline,o_parms[0],"");		/* Remove it from the input line	*/
			ptype = get_param(o_parms[0]);					/* get a new parm....			*/
			if (ptype == -1) keeplooping = 0;

			if (ptype == 1)							/* first parm on a new line		*/
			{
				tput_flush();
			}

			if (!strcmp(o_parms[0],"KEY"))					/* handle KEY.				*/
			{

				tput_clause(16, "KEY");					/* output parm				*/

				i = strpos(inline," KEY");				/* Remove the KEY word.			*/
				stredt(inline," KEY","");
				ptype = get_param(o_parms[0]);				/* get the key name.			*/
				if (ptype == -1) keeplooping = 0;

				if (strpos(" IS EQUAL = GREATER > NOT LESS < <= >= ",o_parms[0]) == -1) /* Must be a variable	*/
				{
					strcpy(templine," ");
					strcat(templine,o_parms[0]);

					if (ptype == 1) 				/* First on line?			*/
					{
						i = 0;
						tput_flush();
					}

					stredt(&inline[i],templine,"");			/* remove the field name		*/
					write_log("WISP",'I',"STARTFLDDEL","Field name %s removed from START.",o_parms[0]);
					o_parms[0][0] = '\0';				/* Clear it out.			*/
				}
				else
				{
					write_log("WISP",'I',"NOMODS","No mods made to START.");

					if (ptype == 1)					/* first parm on a new line		*/
					{
						tput_flush();
					}

				}
			}
			else if (!strcmp(o_parms[0],"INVALID"))				/* INVALID KEY phrase?			*/
			{
				int	add_cont;
				add_cont = 0;
				stredt(inline," INVALID"," ");				/* remove INVALID			*/
				if (ptype != -1)
				{
					peek_param(o_parms[7]);				/* get "KEY"				*/
					if (!strcmp(o_parms[7],"KEY"))			/* Was it KEY?				*/
					{
						ptype = get_param(o_parms[0]);		/* Get it, and remove it.		*/
						if (ptype == -1) keeplooping = 0;
						stredt(inline," KEY"," ");		/* remove KEY				*/
					}
				}

				if (ptype == -1)					/* Premature period!			*/
				{
					keeplooping = 0;
					write_log("WISP",'I',"BADINVKEY",
						"Bad START syntax, INVALID KEY followed by a period.");
					o_parms[0][0] = '\0';
					add_cont = 1;
				}
				else
				{
					ptype = get_param(o_parms[0]);			/* what to do?				*/
					if (ptype == -1) keeplooping = 0;
				}

				if ( vax_cobol )
				{
					tput_line("               REGARDLESS OF LOCK");
					lock_clause = 1;
				}
				tput_line        ("               INVALID KEY"); 	/* write it out				*/
				if (add_cont) 
					tput_line("               CONTINUE");		/* Invalid key followed by period	*/ 

				inv_key = 1;						/* flag it				*/
			}

			if (proc_keyword(o_parms[0]))					/* If we hit a keyword	terminate loop	*/
			{
				keeplooping = 0;
			}
			else
			{
				/*
					Output the param
				*/

				key_name(o_parms[0],0);					/* Do key name translation		*/

				tput_clause(16, "%s",o_parms[0]);			/* output parm				*/

			}

		} while (keeplooping);		/* do till we hit a LAST parm or keyword*/


		if ( vax_cobol && !lock_clause )
		{
			tput_line("               REGARDLESS OF LOCK");
			lock_clause = 1;
		}

		if (ptype == -1) tput_clause(16, "."); 				/* a LAST parm				*/

		if (ptype != -1) hold_line();

		write_log("WISP",'I',"STARTDONE","Completed START analysys");
	}

}
#endif /* OLD */

