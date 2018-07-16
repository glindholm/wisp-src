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

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "cobfiles.h"
#include "wispfile.h"
#include "statment.h"
#include "wt_disp.h"
#include "wt_procd.h"
#include "reduce.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

extern int unterminated_if;
int	mwconv_flag=0;

NODE find_verb_node(NODE the_statement);
NODE parse_else(NODE the_statement);
NODE parse_goto(NODE the_statement);
NODE parse_perform(NODE the_statement);
NODE parse_call(NODE the_statement);

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

static void exit_copy_mode();
static int p_proc();
static void stmt_unsupported();
static void p_rewrite();

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
void procedure_division(the_statement)
NODE	the_statement;
{
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

	/*
	**	The following "logic" handles the PROCEDURE DIVISION including the DECLARATIVES.
	**	It has been relocated from wisp.c and wt_divs.c but otherwise remains unchanged.
	**	It has not yet been moderized to use "statement" logic.
	*/

	get_line();								/* get a new line.			*/

	if (strpos(linein,"DECLARATIVES") == -1)				/* no declaratives			*/
	{
		int i;

		if (prog_cnt - prog_sort)					/* and there are valid files.		*/
		{								/* so put ours in place			*/
			write_log("WISP",'I',"INSERTDECL","Inserting default DECLARATIVES.");
			tput_line_at(8, "DECLARATIVES.");
			gen_defdecl();						/* And generate the default declaratives.*/
			tput_line_at(8, "END DECLARATIVES.");
			tput_blank();
		}								/* continue processing the line 	*/
		check_section();						/* See if there is a section name	*/

		for (i=0; i<proc_performs_cnt; i++)				/* Delete any procedure division copys	*/
		{								/* (from .OPT file)			*/
			proc_performs[i][0] = '\0';
		}
	}
	else									/* we do have declaratives, flag it	*/
	{
		in_decl = 1;
	}

	hold_line();

	for(;;)
	{
		/*
		**	This is the main loop that was in wisp.c, the check_div() call has been replaced
		**	by check_proc_div() which is a subset of it that applies to the procedure division.
		*/
		get_line();

		if (strcmp(area_a,"    "))					/* If area_a is not empty then there	*/
		{
			tput_flush();

			exit_copy_mode();

			if (in_decl)
			{
				/*
				**	If in DECLARATIVES then check for and process END DECLARATIVES.
				*/
				if (check_proc_div())
				{
					hold_line();
					continue;
				}
			}
										/* If in the procedure division, when	*/
										/* something is in area a, it has to	*/
			if (chk_dpar())						/* be a paragraph, or declarative sect.	*/
			{
				hold_line();
				continue;
			}

		}

		p_proc();
	}
}

int sect_num = 0;

static void exit_copy_mode()
{
	extern int sect_num;								/* Current SECTION number.		*/

	if (copy_sect)									/* Have we been copying a SECTION?	*/
	{
		if (!strncmp(parms[1],"SECTION",7))					/* Is this a section?			*/
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

static int p_proc()
{
	int i;
	NODE	the_statement;
	char	test_keyword[80];

	sprintf(test_keyword,"%s ",parms[0]);


#define PROC_MOVE	0
#define PROC_OPEN 	5
#define PROC_CLOSE	10
#define PROC_WRITE	16
#define PROC_REWRITE	22
#define PROC_READ	30
#define PROC_SET	35
#define PROC_START	39
#define PROC_CALL	45
#define PROC_EXIT	50
#define PROC_PERFORM	55
#define PROC_STOP	63
#define PROC_IF		68
#define PROC_GOTO	71
#define PROC_SORT	74
#define PROC_ACCEPT	79
#define PROC_DISPLAY	86
#define PROC_DELETE	94
#define PROC_ELSE	101

/*       0         1         2         3         4         5         6         7         8         9         0  	*/
/*       01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456	*/
	if ((i = strpos(
        "MOVE OPEN CLOSE WRITE REWRITE READ SET START CALL EXIT PERFORM STOP IF GO SORT ACCEPT DISPLAY DELETE ELSE ",
			test_keyword)) != -1)
	{
		switch (i)
		{
			case PROC_CALL:
			case PROC_DISPLAY:
			case PROC_ELSE:
			case PROC_EXIT:
			case PROC_GOTO:
			case PROC_IF:
			case PROC_MOVE:
			case PROC_PERFORM:
			case PROC_SET:
			case PROC_START:
			case PROC_STOP:
			{
				hold_line();
				get_line();

				the_statement = get_verb_statement();
				parse_verb_statement(the_statement);

				hold_token_cache();
				break;
			}

			case PROC_OPEN:							/* do OPEN statements			*/
			{
				p_open();						/* call a proc to do it			*/
				break;
			}

			case PROC_CLOSE:						/* do CLOSE statements			*/
			{
				p_close();						/* call the proc to do it		*/
				break;
			}

			case PROC_REWRITE:						/* a REWRITE statement			*/
			{
				ptype = get_param(o_parms[0]);				/* absorb REWRITE			*/
				peek_param(o_parms[1]);					/* what are we supposed to rewrite?	*/
				write_log("WISP",'I',"EXREWRITE","Examine REWRITE of %s.",o_parms[1]);

				i = 0;

				while (i<crt_record_count && strcmp(crt_record[i],o_parms[1]))	/* is it a crt record?		*/
				{
					i++;						/* no, keep looking			*/
				}

				if (i == crt_record_count)				/* didn't find it, copy the line 	*/
				{
					p_rewrite();					/* Go process it for INVALID KEY.	*/
				}
				else							/* it was a CRT record, look for parms	*/
				{
					stredt(linein,"REWRITE","");			/* Remove REWRITE statement.		*/
					ptype = get_param(o_parms[1]);			/* Actually get the name.		*/
					stredt(linein,o_parms[1],"");			/* Remove file name.			*/
					crt_rewrite(i);					/* call a proc to do it			*/
				}
				break;
			}

			case PROC_READ:							/* a READ statement			*/
			{
				p_read();						/* Use a proc.				*/
				break;
			}

			case PROC_WRITE:						/* WRITE statement.			*/
			{
				p_write();
				break;
			}

			case PROC_SORT:								/* Process SORT statements.	*/
			{
				write_log("WISP",'I',"SORTFOUND","SORT statement being processed.");
				p_sort();
				break;
			}

			case PROC_ACCEPT:						/* Process ACCEPT statements		*/
			{
				p_accept();
				break;
			}

			case PROC_DELETE:
			{
				p_delete();						/* Examine DELETE statements.		*/
				break;
			}

			default:
			{
				tput_line("%s", linein);
				break;
			}
		}									/* end of switch			*/
	}
#define PROC_FREE	1
#define PROC_COMMIT	6
#define PROC_HOLD	13
#define PROC_ROLLBACK	18
#define PROC_BEGIN	27

			/*    0         1         2         3         4         5	  6	    7	      8			*/
			/*    012345678901234567890123456789012345678901234567890123456789012345678901234567890			*/
	else if ((i = strpos(" FREE COMMIT HOLD ROLLBACK BEGIN ", test_keyword)) != -1)
	{
		switch (i)
		{
		case PROC_FREE:
			if (do_locking) 
			{
				p_free();
			}
			else if (acu_cobol) 
			{
				write_log("WISP",'W',"FREEALL","FREE ALL changed to UNLOCK ALL.");
				tput_scomment("*** FREE ALL changed to UNLOCK ALL. ***");
				stredt(linein,"FREE","UNLOCK");
				tput_line("%s", linein);
			}
			else 
			{
				write_log("WISP",'W',"FREEALL","FREE ALL changed to UNLOCKs.");
				stredt(linein,"FREE","PERFORM");
				stredt(linein,"ALL","WISP-FREE-ALL");
				tput_line("%s", linein);
			}
			break;
		case PROC_COMMIT:
			if (do_locking) 
			{
				p_free();
			}
			else if (acu_cobol) 
			{
				write_log("WISP",'W',"COMMIT","COMMIT changed to UNLOCK ALL.");
				tput_scomment("*** COMMIT changed to UNLOCK ALL. ***");
				stredt(linein,"COMMIT","UNLOCK ALL");
				tput_line("%s", linein);
			}
			else
			{
				stmt_unsupported("COMMIT");
			}
			break;
		case PROC_HOLD:
			stmt_unsupported("HOLD");
			break;
		case PROC_ROLLBACK:
			stmt_unsupported("ROLLBACK");
			break;
		case PROC_BEGIN:
			stmt_unsupported("BEGIN");
			break;
		default:
			tput_line("%s", linein);
			break;
		}
	}
	else
	{
#ifdef OLD
		tput_line("%s",linein);							/* For now, just copy it.		*/
#endif
		hold_line();
		get_line();

		the_statement = get_verb_statement();
		parse_verb_statement(the_statement);

		hold_token_cache();
	}
	return 0;
}

/*
**	Routine:	parse_verb_statement()
**
**	Function:	Handle one verb statement.
**
**	Description:	
**
**	Arguments:
**	the_statement	A verb statement.
**
**	Globals:	None
**
**	Return:		NULL
**
**	Warnings:	None
**
**	History:	
**	07/22/94	Written by gsl
**
*/
NODE parse_verb_statement(NODE the_statement)
{
	NODE	verb_node;

	if (!the_statement) return(the_statement);

	verb_node = find_verb_node(the_statement);

	if(!verb_node)
	{
		tput_statement(12,the_statement);
		return( free_statement(the_statement) );
	}

	if (eq_token(verb_node->token,VERB,"MOVE"))
	{
		the_statement = parse_move(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"START"))
	{
		parse_start(the_statement);
		the_statement = free_statement(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"CALL"))
	{
		the_statement = parse_call(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"EXIT"))
	{
		the_statement = parse_exit(the_statement);
	}
	else if (eq_token(verb_node->token,KEYWORD,"ELSE"))	/* NOTE: ELSE is a KEYWORD not a VERB */
	{
		the_statement = parse_else(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"GO"))
	{
		the_statement = parse_goto(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"PERFORM"))
	{
		the_statement = parse_perform(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"STOP"))
	{
		the_statement = parse_stop(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"IF"))
	{
		unterminated_if = 0;							/* Clear flag, next ELSE belongs to	*/
											/* this IF stmt.			*/
		parse_if(the_statement);
		the_statement = free_statement(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"DISPLAY"))
	{
		the_statement = parse_display(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"SET"))
	{
		the_statement = parse_set(the_statement);
	}
	else
	{
		/*
		**	Unrecognized VERB, just print it out.
		*/
		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);
	}

	if (the_statement)
	{
		/*
		**	If there is a statement then just print it.
		*/
		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);
	}

	return the_statement;
}

static void stmt_unsupported(verb)
char	*verb;
{
	tput_scomment("****** WISP: Verb [%s] is not supported. ******",verb);
	write_log("WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",verb);
	tput_line("%s",linein);
}

static void p_rewrite()
{

	int i,fnum,inv_key;
	char	recname[40];
	int	lock_clause;
	int	fd;
	char	fdname[40];

	i = 0;
	o_parms[5][0] = '\0';								/* no status field yet			*/

	peek_param(recname);
	if (strchr(recname,'.')) *((char *)strchr(recname,'.')) = 0;

	fd = fd_record( recname );
	if (fd == -1)
	{
		write_log("WISP",'E',"REWRITE","Unknown Record Name [%s]",recname);
		strcpy(fdname,"Unknown-Record-Name");
	}
	else
	{
		strcpy(fdname, prog_files[fd]);
	}

	fnum = -1;									/* set fnum to error status		*/

	prerewrite_locking();								/* Add locking logic			*/

	tput_line		("           MOVE \"RW\" TO WISP-DECLARATIVES-STATUS");
	if (vax_cobol) tput_line("           MOVE \"N\" TO WISP-TEST-BYTE");
	tput_flush();

	inv_key = 0;									/* no INVALID KEY yet...		*/
	lock_clause = 0;

	do
	{
		tput_clause(12, "%s",o_parms[0]);					/* output parm				*/

		if (o_parms[0][0]) stredt(linein,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			tput_flush();
		}

		if (!strcmp(o_parms[0],"INVALID"))					/* INVALID KEY phrase?			*/
		{
			stredt(linein," INVALID"," ");					/* remove INVALID			*/
			if (ptype != -1)
			{
				peek_param(o_parms[7]);					/* get "KEY"				*/
				if (!strcmp(o_parms[7],"KEY"))				/* Was it KEY?				*/
				{
					ptype = get_param(o_parms[0]);			/* Get it, and remove it.		*/
					stredt(linein," KEY"," ");			/* remove KEY				*/
				}
			}

			if (vax_cobol && !lock_clause && !(prog_ftypes[fd] & AUTOLOCK))
			{
				tput_line("               ALLOWING NO OTHERS");
				lock_clause = 1;
			}

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'I',"BADINVKEY",
				"Bad REWRITE syntax, INVALID KEY followed by a period.");
				o_parms[0][0] = 0;
			}
			else
			{
				ptype = get_param(o_parms[0]);				/* what to do?				*/
			}

			tput_line        ("               INVALID KEY ");		/* write it out				*/
			if (vax_cobol)
			{
				tput_line("               MOVE \"Y\" TO WISP-TEST-BYTE");
				tput_line("           END-REWRITE");
			}
			inv_key = 1;							/* flag it				*/
		}
	}	while ((ptype != -1) && !proc_keyword(o_parms[0]));			/* do till we hit a LAST parm or keyword*/

	if (ptype == -1)								/* a LAST parm				*/
	{
		tput_line("                   %s\n",o_parms[0]); 
	}

	if (vax_cobol && !lock_clause && !(prog_ftypes[fd] & AUTOLOCK))
	{
		tput_line("               ALLOWING NO OTHERS\n");
		lock_clause = 1;
	}


	if ( vax_cobol )
	{
		if (inv_key)
		{
			tput_line("           IF WISP-TEST-BYTE = \"N\" THEN");
			tput_line("               MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fd]);
			tput_line("               UNLOCK %s", fdname );
			tput_line("               MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fd]);
			tput_line("           ELSE");
		}
		else
		{
			tput_line("           MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fd]);
			tput_line("           UNLOCK %s", fdname );
			tput_line("           MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fd]);
		}
	}

	if (ptype == -1)
	{
		tput_line  ("           CONTINUE.");
	}


	if (ptype != -1) hold_line();

	write_log("WISP",'I',"REWRITEDONE","Completed REWRITE analysys");

}

void add_perf(the_name)									/* add a paragraph name to the perform	*/
char *the_name;
{
	if (!paracmp(the_name,decl_performs,decl_performs_cnt))				/* If not in table add it.		*/
	{
		strcpy(decl_performs[decl_performs_cnt++],the_name);	
	}
}

NODE parse_stop(NODE the_statement)
{
	NODE	curr_node, stop_node, period_node;
	int	col;

	curr_node = find_verb_node(the_statement);

	if (!eq_token(curr_node->token,VERB,"STOP"))
	{
		return(the_statement);
	}

	stop_node = curr_node;
	curr_node = curr_node->next;

	if (eq_token(curr_node->token,KEYWORD,"RUN"))
	{
		/*
		**	STOP RUN	->	PERFORM WISP-STOP-RUN
		**			->	PERFORM D-WISP-STOP-RUN
		*/

		edit_token(stop_node->token,"PERFORM");

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

		tput_statement(12,the_statement);
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

	col = stop_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	tput_line_at(col,"MOVE SPACES TO WISP-CRT-SCREEN-AREA");
	tput_flush();
	edit_token(stop_node->token,"STRING");
	stop_node->token->column = col;

	if (curr_node->token->type != LITERAL)
	{
		char	buff[80];
		sprintf(buff,"\"%s\"",token_data(curr_node->token));
		edit_token(curr_node->token,buff);
		curr_node->token->type = LITERAL;
	}

	for(period_node=NULL; NODE_END != curr_node->type; curr_node=curr_node->next)
	{
		if (PERIOD==curr_node->token->type)
		{
			period_node = curr_node;
			free_token_from_node(period_node);
			break;
		}
	}

	tput_statement(12,the_statement);

	tput_line_at(col+4, "DELIMITED BY SIZE INTO");
	tput_clause (col+8, "WISP-CRT-SCREEN-AREA");
	tput_line_at(col,   "CALL \"WSTOP\" USING WISP-CRT-SCREEN-AREA");

	if (period_node)
	{
		tput_clause(col,".");
	}
	tput_flush();

	return(free_statement(the_statement));
}

NODE parse_exit(the_statement)
NODE the_statement;
{
	NODE	curr_node, exit_node;

	curr_node = find_verb_node(the_statement);

	if (!eq_token(curr_node->token,VERB,"EXIT"))
	{
		return(the_statement);
	}

	exit_node = curr_node;
	curr_node = curr_node->next;

	if (eq_token(curr_node->token,KEYWORD,"PROGRAM"))
	{
		/*
		**	EXIT PROGRAM	->	PERFORM WISP-EXIT-PROGRAM
		**			->	PERFORM D-WISP-EXIT-PROGRAM
		*/

		edit_token(exit_node->token,"PERFORM");

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

NODE parse_move(the_statement)
NODE the_statement;
{
	NODE	curr_node, move_node, ident_node;
	int	col;

	curr_node = find_verb_node(the_statement);

	if (!eq_token(curr_node->token,VERB,"MOVE"))
	{
		return(the_statement);
	}

	move_node = curr_node;
	col = move_node->token->column;

	curr_node = curr_node->next;

	if (eq_token(curr_node->token,KEYWORD,"WITH"))
	{
		/*
		**	MOVE WITH CONVERSION identifier-1 TO identifier-2
		**		[ON ERROR 
		**			imperative-statement]		| this portion is not part of the_statement
		**		[END-MOVE]				|
		**
		**	MOVE identifier-1 TO WISP-ALPHA-CONVERSION-FIELD,
		**	PERFORM CONVERT-ALPHA-VALUE-TO-NUMERIC,
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
		**		MOVE ZERO TO identifier-2]
		**		[imperative-statement]
		*/

		write_log("WISP",'I',"MOVEWC","Processing MOVE WITH CONVERSION.");
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

		if (col < 12) col = 12;
		else if (col > 24) col = 24;

		tput_token(col,move_node->token);				/* Write MOVE					*/
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
		ident_node = curr_node->down;
		decontext_statement(ident_node);

		tput_clause (col+4, "TO WISP-ALPHA-CONVERSION-FIELD,");
		tput_line_at(col,   "PERFORM CONVERT-ALPHA-VALUE-TO-NUMERIC,");

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
		if (eq_token(curr_node->token,KEYWORD,"ON"))
		{
			curr_node = curr_node->next;
		}
		if (eq_token(curr_node->token,KEYWORD,"ERROR"))
		{
			tput_line_at(col,   "IF WISP-A-NUMERIC-CONV-ERROR THEN");
			tput_line_at(col,   "    MOVE ZERO TO");
			tput_statement(col+4,ident_node);			/* Write identifier-2				*/
			curr_node = curr_node->next;
			unterminated_if = 1;					/* Cause an END-IF to be added if needed.	*/
		}
		tput_statement(col,curr_node);					/* Write trailing nodes (I.e. PERIOD)		*/
	}
	else if (init_move && 
		 (eq_token(curr_node->token,KEYWORD,"SPACES") || eq_token(curr_node->token,KEYWORD,"SPACE")) )
	{
		/*
		**	MOVE SPACES TO identifiers ...
		**
		**	INITIALIZE     identifiers ...
		*/

		write_log("WISP",'I',"MOVESPACES","Changing MOVE SPACE(S) to INITIALIZE.");

		edit_token(move_node->token,"INITIALIZE");			/* Change the "MOVE" into "INITIALIZE"		*/
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

		tput_statement(12, the_statement);
	}
	else
	{
		tput_statement(12, the_statement);
	}

	return(free_statement(the_statement));
}

NODE parse_set(the_statement)
NODE the_statement;
{
	NODE	curr_node, verb_node, target_node, figcon_node;
	int	col;

	verb_node = find_verb_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"SET"))
	{
		return(the_statement);
	}

	write_log("WISP",'I',"SET","SET statement being processed.");

	col = verb_node->token->column;

	figcon_node = verb_node->next;
	curr_node = figcon_node->next;

	if (isafigcon1(token_data(figcon_node->token)) &&
		( eq_token(curr_node->token,KEYWORD,"IN") || 
		  eq_token(curr_node->token,KEYWORD,"OF")    ) )
	{
		char	*bit_xxx;

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

		decontext_statement(the_statement);

		tput_line_at(col,"MOVE %s TO WISP-SET-BYTE",token_data(figcon_node->token));

		if (acu_cobol)
		{
			tput_line_at  (col,   "MOVE");
			tput_statement(col+4,      target_node->down);
			tput_clause   (col+4,     "TO WISP-TEST-BYTE");
			tput_line_at  (col,   "CALL \"%s\" USING WISP-SET-BYTE, WISP-TEST-BYTE",bit_xxx);
			tput_line_at  (col,   "MOVE WISP-TEST-BYTE TO");
			tput_statement(col+4,      target_node->down);
		}
		else
		{
			tput_line_at(col, "CALL \"%s\" USING WISP-SET-BYTE,",bit_xxx);
			tput_statement(col+4,target_node->down);
		}

		tput_statement(col,curr_node);
		the_statement = free_statement(the_statement);
	}
	else
	{
		tput_statement(col,the_statement);
		the_statement = free_statement(the_statement);
	}

	return the_statement;
}

NODE parse_else(NODE the_statement)
{
	if (unterminated_if)					/* If wisp generated an unterminated	*/
	{							/* "IF" stmt then need to terminate it	*/
								/* before we output this "ELSE".	*/
		unterminated_if = 0;
		tput_line("           END-IF");			/* Close the IF				*/
		tput_flush();
	}

	tput_statement(12,the_statement);
	return(free_statement(the_statement));
}

NODE find_verb_node(NODE the_statement)
{
	NODE	verb_node, curr_node;

	curr_node = the_statement;
	if (NODE_START == curr_node->type) 
	{
		curr_node = curr_node->next;
	}

	verb_node = curr_node;

	/*
	**	We should probably do some checking here to ensure we really have the verb.
	*/

	return verb_node;
}

NODE parse_goto(NODE the_statement)
{
	NODE	verb_node, curr_node;

	verb_node = find_verb_node(the_statement);
	if (!eq_token(verb_node->token,VERB,"GO"))
	{
		return(the_statement);
	}
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

NODE parse_perform(NODE the_statement)
{
	NODE	verb_node, curr_node;

	verb_node = find_verb_node(the_statement);
	if (!eq_token(verb_node->token,VERB,"PERFORM"))
	{
		return(the_statement);
	}
	curr_node = verb_node->next;

	/*
	**	Handle the paragraph relocating stuff.
	**
	**	NOTE:	Currently paragraph relocating doesn't deal with PERFORM THRU logic.
	*/
	if (curr_node && curr_node->token && IDENTIFIER == curr_node->token->type)
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

	tput_statement(12,the_statement);
	return(free_statement(the_statement));
}

/*
**	History:
**	$Log: wt_procd.c,v $
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
