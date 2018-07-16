			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
#ifdef MSDOS
#include <string.h>
#endif

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "cobfiles.h"
#include "wispfile.h"

NODE get_statement();
NODE get_verb_statement();
static int exit_copy_mode();

extern int unterminated_if;
int	mwconv_flag=0;

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
procedure_division(the_statement)
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

	if (strpos(inline,"DECLARATIVES") == -1)				/* no declaratives			*/
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
			check_proc_div();
										/* If in the procedure division, when	*/
										/* something is in area a, it has to	*/
			chk_dpar();						/* be a paragraph, or declarative sect.	*/
	
		}

		p_proc();
	}
}

int sect_num = 0;

static exit_copy_mode()
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

p_proc()
{
	int i,j,hold,fnum,inv_key,n_alt,ml,mwc_len;
	char tstr[132],pstr[256],outline[132];
	int	startpos;
	NODE	the_statement;

	if ( !strcmp(parms[0],"DISPLAY") && !strcmp(parms[1],"AND"))
	{
		/*
			DISPLAY AND READ [ALTERED] record-name ON crt-file
				[ [ONLY] {PFKEY|PFKEYS} {ident} [ident ...]                  ]
				[ [ON    {PFKEY|PFKEYS} {ident} [ident ...] imperative-stmt] ]
				[ [NO-MOD imperative-stmt]                                   ]
		*/

		skip_param(3);								/* skip forward 3 parameters		*/
		get_param(templine);							/* get next parameter			*/
		if (!strcmp(templine,"ALTERED"))					/* was an ALTERED parm, skip it		*/
		{
			get_param(templine);						/* Get the screen definition name	*/
		}
		write_log("WISP",'I',"DISPANDREAD","DISPLAY AND READ statement of screen record %s.",templine);

		tput_scomment("**** DISPLAY AND READ %s.",templine); 

		scount = 0;
											/* Try to find a match in the known	*/
											/* screen list				*/
		while ( strcmp(templine,scrn_name[scount]) && (scount < num_screens)) scount++;
		if (scount == num_screens)						/* Didn't find one.... error		*/
		{
			write_log("WISP",'F',"DISPANDRDERR","Error in DISPLAY AND READ Statement, Input line is;");
			write_log(" ",' '," ","%s",inline);
			write_log("WISP",'F',"CANTFINDSCRN","Could not find screen named %s",templine);
			exit_with_err();
		}

		if (in_decl)								/* if we are currently in declaratives	*/
		{
			scrn_flags[scount] |= SCRN_IN_DECLARATIVES;			/* flag it as such			*/
		}
		if (!in_decl || copy_to_dtp_file)					/* If not, or copying.			*/
		{
			scrn_flags[scount] |= SCRN_IN_PROCDIV;				/* Otherwise flag procedure division	*/
		}

		if (pmode == DISPLAY)							/* Nested DISPLAY AND READ		*/
		{
			write_log("WISP",'F',"ERRNESTDISP","Error -- NESTED DISPLAY AND READ Statement, Input line is;");
			write_log(" ",'M'," ","%s",inline);
			exit_with_err();
		}
					
		this_item = screen_item[scount];					/* point to first item in the list for	*/
											/* this screen				*/
		last_item = this_item;

		ptype = get_param(templine);						/* skip over the ON keyword		*/

		ptype = get_param(templine);						/* skip over the CRT keyword		*/
											/* Find out which crt it is.		*/
		cur_crt = crt_index(templine);
		if (-1 == cur_crt)
		{
			write_log("WISP",'E',"NOTWS","DISPLAY AND READ on unknown Workstation file [%s]",templine);
			cur_crt = 0;
		}
		scrn_crt[scount] = cur_crt;						/* Remember which one it was.		*/

		d_period = 0;								/* no period found			*/
		pfkeys = 0;								/* PFKEYS not found yet			*/
		on_pfkeys = 0;								/* haven't found ON PFKEYS		*/
		no_mod = 0;								/* nor have we seen NO-MOD		*/

											/* Found a period			*/
		if (ptype == -1) 							/* No other parms			*/
		{									/* finish up DISPLAY AND READ		*/
			n_pfkeys(1);							/* write an empty pfkey phrase w/period	*/
		}
		else
		{
			pmode = DISPLAY;						/* set the mode and continue		*/
			pf_str[0] = 0;							/* PFKEYS phrase string = 0		*/
			nm_str[0] = 0;							/* NO-MOD phrase is null		*/
			wd_scan();							/* scan for the parts			*/
		}
	}
	else if (pmode == DISPLAY)							/* a DISPLAY parse is in progress	*/
	{
		wd_scan();								/* just scan for the parts		*/
	}										/* end of DISPLAY AND READ PROCESING	*/

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
	else if ((i = strpos(
        "MOVE OPEN CLOSE WRITE REWRITE READ SET START CALL EXIT PERFORM STOP IF GO SORT ACCEPT DISPLAY DELETE ELSE ",
			parms[0])) != -1)
	{
		switch (i)
		{
			case PROC_MOVE:
			{
				hold_line();
				get_line();
				the_statement = get_verb_statement();
				
				parse_move(the_statement);

				free_statement(the_statement);
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
					stredt(inline,"REWRITE","");			/* Remove REWRITE statement.		*/
					ptype = get_param(o_parms[1]);			/* Actually get the name.		*/
					stredt(inline,o_parms[1],"");			/* Remove file name.			*/
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

			case PROC_SET: 								/* A SET statement		*/
			{

				ptype = get_param(o_parms[0]);					/* Get the SET keyword.		*/
				strcpy(outline,inline);						/* Save the line.		*/
				ml = 0;								/* Not multi line yet.		*/

				ptype = get_param(o_parms[0]);					/* Get the source keyword.	*/
				if (ptype == 1) ml = 1;						/* Check if multi line.		*/

				ptype = get_param(o_parms[2]);					/* Get the IN or OF keyword.	*/
				if (ptype == 1) ml = 1;						/* Check if multi line.		*/

				if (!strcmp(o_parms[2],"IN") || !strcmp(o_parms[2],"OF"))	/* It is a set statement	*/
				{
					get_param(o_parms[1]);					/* get name of variable to set	*/

					if (!strcmp(o_parms[2],"OF"))				/* If OF phrase, may need FAC.	*/
					{
						if (find_item(o_parms[1]))			/* see if this is a screen item.*/
						{
							strcpy(o_parms[9],o_parms[1]);		/* It is, make a fac now.	*/
							make_fac(o_parms[1],o_parms[9]);
						}
					}

					ptype = get_param(o_parms[2]);				/* will be ON or OFF		*/


					for (i=0;i<10;i++)					/* Try 10 words.		*/
					{							/* if not ON or OFF		*/
						if (strcmp(o_parms[2],"ON") && strcmp(o_parms[2],"OFF")) /* must be a subscript	*/
						{
							strcat(o_parms[1]," ");			/* add a space			*/
							strcat(o_parms[1],o_parms[2]);		/* then the item		*/
							ptype = get_param(o_parms[2]);		/* try again			*/
						}
						else
						{						/* Change to lower case.	*/
							if (!strcmp(o_parms[2],"ON"))	strcpy(o_parms[2],"on");
							else				strcpy(o_parms[2],"off");

							break;
						}
					}

					write_log("WISP",'I',"PROCSETBIT","Processing SET BITS %s.",o_parms[2]);

					tput_line_at(16, "MOVE %s TO WISP-SET-BYTE",o_parms[0]);

											/* Can't call with indexed vars		*/
											/* When using acu cobol.		*/
					if (acu_cobol)
					{
						tput_line_at(16, "MOVE %s",o_parms[1]);
						tput_clause (20, "TO WISP-TEST-BYTE");
					}

					tput_line_at(16, "CALL \"bit_%s\" USING WISP-SET-BYTE,",o_parms[2]);

					if (acu_cobol)
						tput_clause(20, "WISP-TEST-BYTE");
					else
						tput_clause(20, "%s",o_parms[1]);

					if (acu_cobol)
					{
						tput_line_at(16, "MOVE WISP-TEST-BYTE TO");
						tput_clause (20, "%s",o_parms[1]);
					}

					if (ptype == -1)					/* end of statement		*/
					{
						tput_clause(16, ".");				/* write a period		*/
					}

					write_log("WISP",'I',"SETBITDONE","Done processing SET BITS command");
				}
				else
				{	if (ml)
					{							/* A multi line parse.		*/

						tput_block(outline);				/* Ouput the starting line.	*/
						hold_line();					/* hold the second line.	*/
					}
					else
					{
						tput_line("%s", inline);			/* Singular line.		*/
					}
				}
				break;
			}


			case PROC_SORT:								/* Process SORT statements.	*/
			{
				write_log("WISP",'I',"SORTFOUND","SORT statement being processed.");
				p_sort();
				break;
			}

			case PROC_START:							/* START statements		*/
			{
#ifdef OLD
				write_log("WISP",'I',"FOUNDSTART","START statement found.");
				p_start();
				break;
#endif

				hold_line();
				get_line();
				the_statement = get_verb_statement();
				
				parse_start(the_statement);

				free_statement(the_statement);
				hold_token_cache();

				break;
			}

			case PROC_CALL:							/* CALL statements			*/
			{
				p_call();						/* Go process it.			*/
				break;
			}

			case PROC_PERFORM:							/* PERFORM statement		*/
			{
				if (in_decl)							/* is it in the declaratives?	*/
				{
					strcpy(pstr,parms[1]);					/* get the name.		*/
					stredt(pstr,".","");

					if (paracmp(pstr,proc_paras,proc_paras_cnt))		/* Found it.			*/
					{							/* now process it.		*/
						make_fld(tstr,pstr,"D-");
						stredt(inline,pstr,tstr);			/* replace it in the line.	*/
					}
					else							/* Otherwise save the name for	*/
					{							/* later analysis.		*/
						add_perf(pstr);
					}
				}
				else if (copy_to_dcl_file)					/* Are we copying paragraphs?	*/
				{
					strcpy(pstr,parms[1]);					/* get the name.		*/
					stredt(pstr,".","");

					if (!paracmp(pstr,proc_paras,proc_paras_cnt))	
					{
						add_perf(pstr);					/* Not found, add to list	*/
					}
				}
				else if (proc_performs_cnt)					/* Check to see if it is a ref	*/
				{								/* to a paragraph in the DECLAR	*/
					strcpy(pstr,parms[1]);					/* get the name.		*/
					stredt(pstr,".","");

					if (paracmp(pstr,proc_performs,proc_performs_cnt))
					{
						make_fld(tstr,pstr,"D-");			/* If found, make the new name.	*/
						stredt(inline,pstr,tstr);			/* Then change the statement.	*/
					}
				}
				tput_line("%s", inline);
				break;
			}

			case PROC_GOTO:								/* GO TO statement		*/
			{
				if (in_decl)							/* is it in the declaratives?	*/
				{
					strcpy(pstr,parms[2]);					/* get the name.		*/
					stredt(pstr,".","");

					if (paracmp(pstr,proc_paras,proc_paras_cnt))		/* Found it			*/
					{							/* now process it.		*/
						make_fld(tstr,pstr,"D-");
						stredt(inline,pstr,tstr);			/* replace it in the line.	*/
					}
					else							/* Otherwise save the name for	*/
					{							/* later analysis.		*/
						add_perf(pstr);
					}

				}
				else if (copy_to_dcl_file)					/* Are we copying paragraphs?	*/
				{
					strcpy(pstr,parms[2]);					/* get the name.		*/
					stredt(pstr,".","");

					if (!paracmp(pstr,proc_paras,proc_paras_cnt))		/* If not found			*/
					{
						add_perf(pstr);					/* Add to list			*/
					}
				}
				else if (proc_performs_cnt)					/* Check to see if it is a ref	*/
				{								/* to a paragraph in the DECLAR	*/
					strcpy(pstr,parms[2]);					/* get the name.		*/
					stredt(pstr,".","");

					if (paracmp(pstr,proc_performs,proc_performs_cnt))
					{
						make_fld(tstr,pstr,"D-");			/* If found, make the new name.	*/
						stredt(inline,pstr,tstr);			/* Then change the statement.	*/
					}
				}
				tput_line("%s", inline);
				break;
			}

			case PROC_EXIT:							/* EXIT statement		*/
			{
				hold_line();
				get_line();
				the_statement = get_verb_statement();
				
				parse_exit(the_statement);

				free_statement(the_statement);
				hold_token_cache();

				break;
			}

			case PROC_STOP:							/* process STOP statements		*/
			{
				hold_line();
				get_line();
				the_statement = get_verb_statement();
				
				parse_stop(the_statement);

				free_statement(the_statement);
				hold_token_cache();

				break;
			}

			case PROC_IF:
			{								/* Process IF's for ALTERED and BIT IN.	*/
				unterminated_if = 0;					/* Clear flag, next ELSE belongs to	*/
											/* this IF stmt.			*/
				hold_line();
				get_line();
				the_statement = get_verb_statement();
				
				parse_if(the_statement);

				free_statement(the_statement);
				hold_token_cache();

				break;
			}

			case PROC_ELSE:
			{
				if (unterminated_if)					/* If wisp generated an unterminated	*/
				{							/* "IF" stmt then need to terminate it	*/
											/* before we output this "ELSE".	*/
					unterminated_if = 0;
					tput_line("           END-IF");			/* Close the IF				*/
				}

				tput_line("%s", inline);
				break;
			}

			case PROC_ACCEPT:						/* Process ACCEPT statements		*/
			{
				p_accept();
				break;
			}

			case PROC_DISPLAY:
			{
				hold_line();
				get_line();
				the_statement = get_verb_statement();
				
				parse_display(the_statement);

				free_statement(the_statement);
				hold_token_cache();

				break;
			}

			case PROC_DELETE:
			{
				p_delete();						/* Examine DELETE statements.		*/
				break;
			}

			default:
			{
				tput_line("%s", inline);
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
	else if ((i = strpos(" FREE COMMIT HOLD ROLLBACK BEGIN ", parms[0])) != -1)
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
				stredt(inline,"FREE","UNLOCK");
				tput_line("%s", inline);
			}
			else 
			{
				stmt_unsupported("FREE");
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
				stredt(inline,"COMMIT","UNLOCK ALL");
				tput_line("%s", inline);
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
			tput_line("%s", inline);
			break;
		}
	}
	else
	{
		tput_line("%s",inline);							/* For now, just copy it.		*/
	}

}

stmt_unsupported(verb)
char	*verb;
{
	tput_scomment("****** WISP: Verb [%s] is not supported. ******",verb);
	write_log("WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",verb);
	tput_line("%s",inline);
}

p_rewrite()
{

	int i,j,hold,fnum,inv_key,n_alt;
	char tstr[132];
	char	recname[40];
	int	lock_clause;

	i = 0;
	o_parms[5][0] = '\0';								/* no status field yet			*/

	peek_param(recname);
	if (strchr(recname,'.')) *((char *)strchr(recname,'.')) = 0;

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

		if (o_parms[0][0]) stredt(inline,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			tput_flush();
		}

		if (!strcmp(o_parms[0],"INVALID"))					/* INVALID KEY phrase?			*/
		{
			stredt(inline," INVALID"," ");					/* remove INVALID			*/
			if (ptype != -1)
			{
				peek_param(o_parms[7]);					/* get "KEY"				*/
				if (!strcmp(o_parms[7],"KEY"))				/* Was it KEY?				*/
				{
					ptype = get_param(o_parms[0]);			/* Get it, and remove it.		*/
					stredt(inline," KEY"," ");			/* remove KEY				*/
				}
			}

			if (vax_cobol && !lock_clause)
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

	if ( vax_cobol && !lock_clause )
	{
		tput_line("               ALLOWING NO OTHERS\n");
		lock_clause = 1;
	}


	if ( vax_cobol )
	{
		int	fd;
		char	fdname[40];

		fd = fd_record( recname );
		if (fd == -1)
		{
			write_log("WISP",'E',"REWRITE-UNLOCK","Unknown Record Name");
			strcpy(fdname,"????");
		}
		else
		{
			strcpy(fdname, prog_files[fd]);
		}

		if (inv_key)
		{
			tput_line("           IF WISP-TEST-BYTE = \"N\" THEN");
			tput_line("               MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fd]);
			tput_line("               UNLOCK %s ALL", fdname );
			tput_line("               MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fd]);
			tput_line("           ELSE");
		}
		else
		{
			tput_line("           MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fd]);
			tput_line("           UNLOCK %s ALL", fdname );
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

wd_scan()										/* scan for the parts of D & R		*/
{
	ptype = get_param(templine);							/* get the next parm			*/
	if (!pfkeys && (pmode == DISPLAY)) t_pfkeys(templine);				/* test for PFKEYS			*/
	if (!on_pfkeys && (pmode == DISPLAY)) t_onpfkeys(templine);			/* test for ON PFKEYS			*/
	if (!no_mod && (pmode == DISPLAY)) t_nomod(templine);				/* test for NO-MOD			*/
	t_d_exit(templine);								/* test for exit of display procs	*/
}


add_perf(the_name)									/* add a paragraph name to the perform	*/
char *the_name;
{
	if (!paracmp(the_name,decl_performs,decl_performs_cnt))				/* If not in table add it.		*/
	{
		strcpy(decl_performs[decl_performs_cnt++],the_name);	
	}
	return(0);
}

parse_display(the_statement)
NODE the_statement;
{
	NODE	curr_node, display_node, period_node;
	int	col;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"DISPLAY"))
	{
		tput_statement(12,the_statement);
		return(0);
	}

	write_log("WISP",'I',"PROCDISP","Processing DISPLAY statement.");

	display_node = curr_node;

	if (!proc_display)
	{
		write_log("WISP",'I',"SKIPDISP","Skipped processing of DISPLAY statement.");
		tput_statement(12,the_statement);
		return(0);
	}

	col = display_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	tput_line_at(col,"MOVE SPACES TO WISP-CRT-SCREEN-AREA");
	edit_token(display_node->token,"STRING");
	display_node->token->column = col;

	curr_node = curr_node->next;
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
	tput_line_at(col,   "MOVE \"Press RETURN to proceed.\" TO");
	tput_clause (col+4, "WISP-SCREEN-LINE(24)");
	tput_line_at(col,   "MOVE WISP-FULL-SCREEN-ORDER-AREA TO");
	tput_clause (col+4, "WISP-CRT-ORDER-AREA");
	tput_line_at(col,   "CALL \"WDISPLAY\" USING WISP-CRT-RECORD");

	if (period_node)
	{
		tput_clause(col,".");
	}
	tput_flush();

	return(0);
}

parse_stop(the_statement)
NODE the_statement;
{
	NODE	curr_node, stop_node, period_node;
	int	col;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"STOP"))
	{
		tput_statement(12,the_statement);
		return(0);
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
		return(0);
	}

	/*
	**	STOP literal
	*/
	write_log("WISP",'I',"STOPLITRL","Processing STOP literal statement.");

	if (!proc_display)
	{
		write_log("WISP",'I',"SKIPSTOP","Skipped processing of STOP statement.");
		tput_statement(12,the_statement);
		return(0);
	}

	col = stop_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	tput_line_at(col,"MOVE SPACES TO WISP-CRT-SCREEN-AREA");
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
	tput_line_at(col,   "MOVE \"Press RETURN to proceed.\" TO");
	tput_clause (col+4, "WISP-SCREEN-LINE(24)");
	tput_line_at(col,   "MOVE WISP-FULL-SCREEN-ORDER-AREA TO");
	tput_clause (col+4, "WISP-CRT-ORDER-AREA");
	tput_line_at(col,   "CALL \"WDISPLAY\" USING WISP-CRT-RECORD");

	if (period_node)
	{
		tput_clause(col,".");
	}
	tput_flush();

	return(0);
}

parse_exit(the_statement)
NODE the_statement;
{
	NODE	curr_node, exit_node;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"EXIT"))
	{
		tput_statement(12,the_statement);
		return(0);
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
		return(0);
	}

	tput_statement(12,the_statement);
	return(0);
}

parse_move(the_statement)
NODE the_statement;
{
	NODE	curr_node, move_node, temp_node, ident_node;
	int	col;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"MOVE"))
	{
		write_log("WISP",'W',"NOTMOVE","NOT A MOVE Statement. STATEMENT NOT PROCESSED");
		tput_statement(12,the_statement);
		return(0);
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
			return(0);
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

	return(0);
}
