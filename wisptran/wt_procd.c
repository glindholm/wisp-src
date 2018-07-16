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

extern item_record *find_item();
extern int unterminated_if;
int	mwconv_flag=0;

p_proc()
{
	int i,j,hold,fnum,inv_key,n_alt,ml,first,mwc_len;
	char tstr[132],pstr[256],outline[132];

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

		write_line("\n      **** DISPLAY AND READ %s.\n",templine); 

		scount = 0;
											/* Try to find a match in the known	*/
											/* screen list				*/
		while ( strcmp(templine,scrn_name[scount]) && (scount < num_screens)) scount++;
		if (scount == num_screens)						/* Didn't find one.... error		*/
		{
			write_log("WISP",'F',"DISPANDRDERR","Error in DISPLAY AND READ Statement, Input line is;");
			write_log(" ",' '," ","%s",inline);
			write_log("WISP",'F',"CANTFINDSCRN","Could not find screen named %s",templine);
			exit_wisp(-1);
		}

		if (in_decl)								/* if we are currently in declaratives	*/
		{
			scrn_flags[scount] |= SCRN_IN_DECLARATIVES;			/* flag it as such			*/
		}
		if (!in_decl || copy_decl)						/* If not, or copying.			*/
		{
			scrn_flags[scount] |= SCRN_IN_PROCDIV;				/* Otherwise flag procedure division	*/
		}

		if (pmode == DISPLAY)							/* Nested DISPLAY AND READ		*/
		{
			write_log("WISP",'F',"ERRNESTDISP","Error -- NESTED DISPLAY AND READ Statement, Input line is;");
			write_log(" ",' '," ","%s",inline);
			exit_wisp(-1);
		}
					
		this_item = screen_item[scount];					/* point to first item in the list for	*/
											/* this screen				*/
		last_item = this_item;

		ptype = get_param(templine);						/* skip over the ON keyword		*/

		ptype = get_param(templine);						/* skip over the CRT keyword		*/
											/* Find out which crt it is.		*/
		for (i=0; i<crt_fcount; i++) if (!strcmp(templine,crt_file[i])) cur_crt = i;
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

/*       0         1         2         3         4         5	  6	    7	      8					*/
/*       01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456	*/
	else if ((i = strpos(
        "MOVE OPEN CLOSE WRITE REWRITE READ SET START CALL EXIT PERFORM STOP IF GO SORT ACCEPT DISPLAY DELETE ELSE ",
			parms[0])) != -1)
	{
		switch (i)
		{
			case PROC_MOVE:							/* look for MOVE WITH CONVERSION	*/
			{								/* or MOVE SPACES TO			*/
				if (!strcmp(parms[1],"WITH"))				/* found WITH, this is it.		*/
				{
					mwconv_flag = 1;				/* we need to generate mwconv code	*/
					skip_param(3);					/* skip the phrase			*/
					ptype = get_param(o_parms[0]);			/* get the source field			*/
					ptype = get_param(o_parms[1]);			/* get the destination field		*/

					write_log("WISP",'I',"MOVEWCONV","MOVE WITH CONVERSION, field is %s.",o_parms[0]);
											/* if open paren and no close paren or	*/
					if (((strpos(o_parms[0],"(") != -1) && (strpos(o_parms[0],")") == -1)) ||
						(o_parms[1][0] == '('))			/* next parm has open,it is subscripted	*/
					{						/* it has a subscript			*/
						strcat(o_parms[0]," ");			/* add a space				*/
						strcat(o_parms[0],o_parms[1]);		/* add it on to the end of the name	*/
						while (strpos(o_parms[1],")") == -1)	/* no closing parentheses		*/
						{
							ptype = get_param(o_parms[1]);	/* get the next part of the sub script	*/
							if (o_parms[1][0] != ')')	/* a field is next, add a space	*/
							{
								strcat(o_parms[0]," ");
							}
							strcat(o_parms[0],o_parms[1]);	/* add it on to the end of the name	*/
						}
						write_log("WISP",'I',"SUBSCRMOVE",
							"Handle subscripted source field %s in MOVE WITH CONVERSION.",o_parms[0]);
						ptype = get_param(o_parms[1]);		/* get the destination			*/
					}
					if (!strcmp(o_parms[1],"TO"))			/* skip over "TO"			*/
					{
						ptype = get_param(o_parms[1]);		/* get the destination field		*/
					}
											/* Put into WISP's field, do the conv.	*/
					write_line("             MOVE %s TO\n",o_parms[0]);
					put_line  ("                  WISP-ALPHA-CONVERSION-FIELD,\n");
					put_line  ("             PERFORM CONVERT-ALPHA-VALUE-TO-NUMERIC,\n");

					strcpy(o_parms[0],o_parms[1]);

					peek_param(o_parms[1]);

					if (((strpos(o_parms[0],"(") != -1) && (strpos(o_parms[0],")") == -1)) ||
						(o_parms[1][0] == '('))			/* next parm has open,it is subscripted	*/
					{						/* it has a subscript			*/
						ptype = get_param(o_parms[1]);
						strcat(o_parms[0]," ");			/* add a space				*/
						strcat(o_parms[0],o_parms[1]);		/* add it on to the end of the name	*/
						while (strpos(o_parms[1],")") == -1)	/* no closing parentheses		*/
						{
							ptype = get_param(o_parms[1]);	/* get the next part of the sub script	*/
							if (o_parms[1][0] != ')')	/* a field is next, add a space	*/
							{
								strcat(o_parms[0]," ");
							}
							strcat(o_parms[0],o_parms[1]);	/* add it on to the end of the name	*/
						}
						write_log("WISP",'I',"SUBSCRMOVE",
							"Handle subscripted destination field %s in MOVE WITH CONVERSION.",
														o_parms[0]);
					}

					mwc_len = strlen(o_parms[0]);
											/* First zero the target field.		*/
											/* Now check it for errors.		*/
					put_line  ("             IF WISP-NO-NUMERIC-CONV-ERROR\n");	/* None by MWCONV.	*/
					if (mwc_len > 44)				/* Check for too long			*/
					{						/* Put on 2 lines			*/
						write_line("             MOVE ZERO TO\n             %s\n",o_parms[0]);
					}
					else
					{
						write_line("             MOVE ZERO TO %s\n",o_parms[0]);
					}
					put_line  ("                  ADD WISP-CONVERTED-INTEGER-FIELD TO\n");

					if (mwc_len > 48)				/* Too big of a name, subscript combo.	*/
					{
						i = strpos(o_parms[0],"(");		/* Find the open paren.			*/
						o_parms[0][i] = '\0';			/* Replace with null.			*/
						write_line("                     %s\n",o_parms[0]);	/* Write main name.	*/
						o_parms[0][i] = '(';			/* Put open paren back.			*/
						write_line("                     %s\n",&o_parms[0][i]);		/* Write subscr.*/
					}
					else
					{
						write_line("                     %s\n",o_parms[0]);
					}
					put_line  ("                     ON SIZE ERROR\n");
					put_line  ("                        MOVE \"Y\" TO WISP-NUMERIC-CONVERSION-FLAG\n");
					put_line  ("                     END-ADD\n");
					put_line  ("                  ADD WISP-CONVERTED-FRACTION-FIELD TO\n");

					if (mwc_len > 48)				/* Too big of a name, subscript combo.	*/
					{
						i = strpos(o_parms[0],"(");		/* Find the open paren.			*/
						o_parms[0][i] = '\0';			/* Replace with null.			*/
						write_line("                     %s\n",o_parms[0]);	/* Write main name.	*/
						o_parms[0][i] = '(';			/* Put open paren back.			*/
						write_line("                     %s\n",&o_parms[0][i]);		/* Write subscr.*/
					}
					else
					{
						write_line("                     %s\n",o_parms[0]);
					}

					put_line  ("                     ON SIZE ERROR\n");
					put_line  ("                        MOVE \"Y\" TO WISP-NUMERIC-CONVERSION-FLAG\n");
					put_line  ("                     END-ADD\n");
					put_line  ("             END-IF\n");		/* End of these checks.			*/

					put_line  ("             IF WISP-NO-NUMERIC-CONV-ERROR\n");	/* None by ADD.		*/
					put_line  ("                CONTINUE");		/* Do this to be sure ELSE's are paired	*/

					strcpy(o_parms[1],o_parms[0]);			/* move it back				*/
					if (ptype != -1)				/* no period, look for ON ERROR		*/
					{
						put_line(",\n");			/* finish previous line			*/
						ptype  = get_param(o_parms[0]);		/* get next word			*/
						if (!strcmp(o_parms[0],"ON"))		/* it's ON, must be error phrase	*/
						{
							i = get_ppos();			/* Get parameter position of ON.	*/
							memset(inline,' ',i);		/* Fill with spaces.			*/
							get_param(o_parms[0]);		/* skip ERROR keyword			*/
							stredt(inline," ON ","  ");	/* remove ON				*/
							stredt(inline," ERROR"," ");	/* remove ERROR 			*/
							put_line  ("             ELSE\n");
							if (strlen(o_parms[1]) < 40)
							{
								write_line("                  MOVE ZERO TO %s,\n",o_parms[1]);
							}
							else
							{
								put_line("                  MOVE ZERO TO\n");

								if (mwc_len > 48)	/* Too big of a name, subscript combo.	*/
								{
									i = strpos(o_parms[1],"(");
									o_parms[1][i] = '\0';
									write_line("                  %s\n",o_parms[1]);
									o_parms[1][i] = '(';
									write_line("                  %s\n",&o_parms[1][i]);
								}
								else
								{
									write_line("                  %s\n",o_parms[1]);
								}
							}
							hold_line();			/* Hold the ERROR line			*/
						}
						else if (!strcmp(o_parms[0],"ERROR"))	/* it's ERROR alone, error phrase	*/
						{
							stredt(inline," ERROR"," ");	/* remove ERROR 			*/
							put_line  ("             ELSE\n");
							if (mwc_len > 40)
							{
								put_line  ("                  MOVE ZERO TO\n");
								if (mwc_len > 48)	/* Too big of a name, subscript combo.	*/
								{
									i = strpos(o_parms[1],"(");
									o_parms[1][i] = '\0';
									write_line("                  %s\n",o_parms[1]);
									o_parms[1][i] = '(';
									write_line("                  %s\n",&o_parms[1][i]);
								}
								else
								{
									write_line("                  %s\n",o_parms[1]);
								}

							}
							else
							{
								write_line("                  MOVE ZERO TO %s,\n",o_parms[1]);
							}
							hold_line();			/* Save the ERROR line			*/
						}
						else					/* has no ON ERROR PHRASE		*/
						{
							put_line  ("             END-IF\n");	/* end it			*/
							if (ptype == 1) hold_line();   /* new line? save it			*/
						}
					}
					else						/* ended with a period			*/
					{
						put_line(".\n");			/* terminate last line			*/
					}
				}
				else if (!strcmp(parms[1],"SPACES") && init_move)	/* found SPACES, do INITIALIZE if desired*/
				{

					ptype = get_param(o_parms[0]);			/* get MOVE keyword			*/
					stredt(inline," MOVE"," ");			/* remove it from the input line	*/
					ptype = get_param(o_parms[0]);			/* get SPACES keyword			*/
					stredt(inline," SPACES","INITIALIZE");		/* change it to INITIALIZE		*/
					if (peek_param(o_parms[0]) == 1)		/* last one on line, write it		*/
					{
						put_line(inline);
					}
					ptype = get_param(o_parms[0]);			/* should be TO				*/

					if (!strcmp(o_parms[0],"TO"))			/* skip over "TO"			*/
					{
						stredt(inline," TO"," ");		/* remove it				*/
					}

					write_log("WISP",'I',"MOVESPACES","MOVE SPACES changed to INITIALIZE.");
					put_line(inline);

				}
				else if (!strcmp(parms[1],"SPACE") && init_move)	/* found SPACE, do INITIALIZE if desired */
				{

					ptype = get_param(o_parms[0]);			/* get MOVE keyword			*/
					stredt(inline," MOVE"," ");			/* remove it from the input line	*/
					ptype = get_param(o_parms[0]);			/* get SPACE keyword			*/
					stredt(inline," SPACE","INITIALIZE");		/* change it to INITIALIZE		*/
					if (peek_param(o_parms[0]) == 1)		/* last one on line, write it		*/
					{
						put_line(inline);
					}
					ptype = get_param(o_parms[0]);			/* should be TO				*/

					if (!strcmp(o_parms[0],"TO"))			/* skip over "TO"			*/
					{
						stredt(inline," TO"," ");		/* remove it				*/
					}

					write_log("WISP",'I',"MOVESPACES","MOVE SPACE changed to INITIALIZE.");
					put_line(inline);

				}
				else							/* not a conversion, just write		*/
				{
					put_line(inline);
				}
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

				while (i<crt_count && strcmp(crt_record[i],o_parms[1]))	/* is it a crt record?			*/
				{
					i++;						/* no, keep looking			*/
				}

				if (i == crt_count)					/* didn't find it, copy the line 	*/
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

					write_line("               MOVE %s TO WISP-SET-BYTE",o_parms[0]);
					put_char('\n');						/* end the line			*/

											/* Can't call with indexed vars		*/
											/* When using acu cobol.		*/
					if (acu_cobol)
					{
						write_line("               MOVE %s\n",o_parms[1]);
						put_line(  "                 TO WISP-TEST-BYTE\n");
					}

					write_line("               CALL \042bit_%s\042 USING\n",o_parms[2]);
					put_line  ("                    WISP-SET-BYTE,\n");

					if (acu_cobol)
						put_line("                    WISP-TEST-BYTE");
					else
						write_line("                    %s",o_parms[1]);

					if (acu_cobol)
					{
						put_line("\n               MOVE WISP-TEST-BYTE\n");
						write_line("                 TO %s",o_parms[1]);
					}

					if (ptype == -1)					/* end of statement		*/
					{
						put_char('.');					/* write a period		*/
					}
					put_char('\n');						/* end the line			*/
					write_log("WISP",'I',"SETBITDONE","Done processing SET BITS command");
				}
				else
				{	if (ml)
					{							/* A multi line parse.		*/

						put_line(outline);				/* Ouput the starting line.	*/
						hold_line();					/* hold the second line.	*/
					}
					else
					{
						put_line(inline);				/* Singular line.		*/
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
				write_log("WISP",'I',"FOUNDSTART","START statement found.");
				p_start();
				break;
			}

			case PROC_CALL:							/* CALL statements			*/
			{
				p_call();						/* Go process it.			*/
				break;
			}

			case PROC_EXIT:							/* EXIT statement		*/
			{
				if (!strcmp(parms[1],"PROGRAM."))
				{							/* an EXIT-PROGRAM 		*/
					ptype = get_param(o_parms[9]);			/* Get the EXIT.			*/
					ptype = get_param(o_parms[9]);			/* Get the PROGRAM.			*/

					if (in_decl)
					{
						put_line("           PERFORM D-WISP-EXIT-PROGRAM");
						decl_stop_exit = 1;
					}
					else
						put_line("           PERFORM WISP-EXIT-PROGRAM");
					if (ptype == -1)
						put_line(".\n");		/* Put in a period.			*/
					else
						put_line("\n");
				}
				else
				{
					put_line(inline);
				}
				break;
			}

			case PROC_PERFORM:							/* PERFORM statement		*/
			{
				if (in_decl)							/* is it in the declaratives?	*/
				{
					strcpy(pstr,parms[1]);					/* get the name.		*/
					stredt(pstr,".","");
					if (par_count)						/* Is there a paragraph		*/
					{							/* substitute list?		*/
						for (i=0; i<par_count; i++)			/* See if it is a paragraph fix.*/
						{
							if (!strcmp(pstr,par_name[i])) break;	/* found it.			*/
						}

						if (i < par_count)
						{						/* now process it.		*/
							make_fld(tstr,pstr,"D-");
							stredt(inline,pstr,tstr);		/* replace it in the line.	*/
						}
						else						/* Otherwise save the name for	*/
						{						/* later analysis.		*/
							add_perf(pstr);
						}
					}
					else							/* Otherwise save the name for	*/
					{							/* later analysis.		*/
						add_perf(pstr);
					}
				}
				else if (copy_para)						/* Are we copying paragraphs?	*/
				{
					strcpy(pstr,parms[1]);					/* get the name.		*/
					stredt(pstr,".","");
					if (par_count)						/* Is there a paragraph		*/
					{							/* substitute list?		*/
						for (i=0; i<par_count; i++)			/* See if it is a paragraph fix.*/
						{
							if (!strcmp(pstr,par_name[i])) break;	/* found it.			*/
						}

						if (i == par_count)
						{						/* not found, process it.	*/
							add_perf(pstr);
						}
					}
					else							/* Otherwise save the name for	*/
					{							/* later analysis.		*/
						add_perf(pstr);
					}
				}
				else if (ppdiv_count)						/* Check to see if it is a ref	*/
				{								/* to a paragraph in the DECLAR	*/
					strcpy(pstr,parms[1]);					/* get the name.		*/
					stredt(pstr,".","");
					for (i=0; i<ppdiv_count; i++)				/* Search the list.		*/
					{
						if (!strcmp(pstr,perf_pdiv[i])) break;		/* found it.			*/
					}
					if (i != ppdiv_count)
					{
						make_fld(tstr,pstr,"D-");			/* If found, make the new name.	*/
						stredt(inline,pstr,tstr);			/* Then change the statement.	*/
					}
				}
				put_line(inline);
				break;
			}

			case PROC_GOTO:								/* GO TO statement		*/
			{
				if (in_decl)							/* is it in the declaratives?	*/
				{
					strcpy(pstr,parms[2]);					/* get the name.		*/
					stredt(pstr,".","");
					if (par_count)						/* Is there a paragraph		*/
					{							/* substitute list?		*/
						for (i=0; i<par_count; i++)			/* See if it is a paragraph fix.*/
						{
							if (!strcmp(pstr,par_name[i])) break;	/* found it.			*/
						}

						if (i < par_count)
						{						/* now process it.		*/
							make_fld(tstr,pstr,"D-");
							stredt(inline,pstr,tstr);		/* replace it in the line.	*/
						}
						else						/* Otherwise save the name for	*/
						{						/* later analysis.		*/
							add_perf(pstr);
						}
					}
					else							/* Otherwise save the name for	*/
					{							/* later analysis.		*/
						add_perf(pstr);
					}
				}
				else if (copy_para)						/* Are we copying paragraphs?	*/
				{
					strcpy(pstr,parms[2]);					/* get the name.		*/
					stredt(pstr,".","");
					if (par_count)						/* Is there a paragraph		*/
					{							/* substitute list?		*/
						for (i=0; i<par_count; i++)			/* See if it is a paragraph fix.*/
						{
							if (!strcmp(pstr,par_name[i])) break;	/* found it.			*/
						}

						if (i == par_count)
						{						/* not found, process it.	*/
							add_perf(pstr);
						}
					}
					else							/* Otherwise save the name for	*/
					{							/* later analysis.		*/
						add_perf(pstr);
					}
				}
				else if (ppdiv_count)						/* Check to see if it is a ref	*/
				{								/* to a paragraph in the DECLAR	*/
					strcpy(pstr,parms[2]);					/* get the name.		*/
					stredt(pstr,".","");
					for (i=0; i<ppdiv_count; i++)				/* Search the list.		*/
					{
						if (!strcmp(pstr,perf_pdiv[i])) break;		/* found it.			*/
					}
					if (i != ppdiv_count)
					{
						make_fld(tstr,pstr,"D-");			/* If found, make the new name.	*/
						stredt(inline,pstr,tstr);			/* Then change the statement.	*/
					}
				}
				put_line(inline);
				break;
			}

			case PROC_STOP:							/* process STOP RUN's			*/
			{
				ptype = get_param(o_parms[0]);				/* Get STOP				*/
				ptype = get_param(o_parms[0]);				/* Get RUN or literal to display.	*/
				if (!strcmp(o_parms[0],"RUN"))
				{
					if (in_decl)
					{
						put_line("           PERFORM D-WISP-STOP-RUN");
						decl_stop_exit = 1;
					}
					else
						put_line("           PERFORM WISP-STOP-RUN");

					if (ptype == -1)
						put_line(".\n");			/* Put in a period.			*/
					else
						put_line("\n");
				}
				else if (o_parms[0][0] == '\"')				/* Its a literal.			*/
				{
					put_line("           DISPLAY\n");		/* Write DISPLAY.			*/
					stredt(inline," STOP ","      ");		/* Remove STOP.				*/
					hold_line();					/* Re parm it.				*/
				}
				else
				{
					put_line(inline);
				}

				break;
			}

			case PROC_IF:
			{								/* Process IF's for ALTERED and BIT IN.	*/
				unterminated_if = 0;					/* Clear flag, next ELSE belongs to	*/
											/* this IF stmt.			*/
				p_if();
				break;
			}

			case PROC_ELSE:
			{
				if (unterminated_if)					/* If wisp generated an unterminated	*/
				{							/* "IF" stmt then need to terminate it	*/
											/* before we output this "ELSE".	*/
					unterminated_if = 0;
					put_line("           END-IF\n");		/* Close the IF				*/
				}

				put_line(inline);
				break;
			}

			case PROC_ACCEPT:						/* Process ACCEPT statements		*/
			{
				p_accept();
				break;
			}

			case PROC_DISPLAY:
			{
				int	startpos;

				write_log("WISP",'I',"PROCDISP","Processing DISPLAY statement.");

				if (!proc_display)					/* We aren't supposed to do this.	*/
				{
					write_log("WISP",'I',"SKIPDISP","Skipped processing of DISPLAY statement.");
					put_line(inline);				/* So just write it out.		*/
					break;						/* And exit.				*/
				}

				ptype = get_param(o_parms[0]);				/* skip DISPLAY statement.		*/

				startpos = get_ppos();
				if (startpos > 30 || startpos < 12) startpos = 15;
				memset(tstr,' ',startpos);
				tstr[startpos] = '\0';					/* Initialize screen area.		*/
				strcat(tstr,"MOVE SPACES TO WISP-CRT-SCREEN-AREA\n");
				put_line(tstr);

				tstr[startpos] = '\0';
				strcat(tstr,"STRING\n");				/* Start with the string statement.	*/
				put_line(tstr);
				tstr[startpos] = '\0';					/* Now clear tstr.			*/

				first = 1;

				do
				{
					ptype = get_param(o_parms[0]);			/* get parm				*/

					i = get_ppos();					/* Get the character position now.	*/

					if (keyword(o_parms[0],proc_keywords))		/* Is it a keyword?			*/
					{						/* it is keyword, quit parsing		*/
						ptype = 9;				/* signal keyword found			*/
					}
					else
					{
						/*
						**  Remember that "(" ")" parens can occur in literals
						*/

						if (o_parms[0][0] == '\"') goto skip_parens;

											/* Is there open paren w/no close?	*/
						if ((strpos(o_parms[0],"(") != -1) && (strpos(o_parms[0],")") == -1))
						{
							ptype = get_param(o_parms[1]);	/* Get the close paren.			*/
							strcat(o_parms[0],o_parms[1]);
						}
						else
						{
							peek_param(o_parms[1]);
							/*
							** If NOT a literal and open paren found then scan through looking
							** for the closing paren.
							*/
							if (o_parms[1][0] != '\"' && strpos(o_parms[1],"(") != -1)
							{
								ptype = get_param(o_parms[1]);	/* really get it.		*/
								strcat(o_parms[0],o_parms[1]);

								/*
								** Loop while a closing paren is not found (only looking at
								** non-literals).
								*/
								while (o_parms[0][0] != '\"' && strpos(o_parms[0],")") == -1)
								{
									ptype = get_param(o_parms[1]);
									strcat(o_parms[0],o_parms[1]);
								}
							}
						}

skip_parens:
						memset(tstr,' ',i);			/* Set up a new position string.	*/
						tstr[i] = '\0';
						if (first)
						{
							first = 0;
							if (strlen(tstr)+strlen(o_parms[0]) > 72)
								write_line("%s\n           %s",tstr,o_parms[0]);
							else
								write_line("%s%s",tstr,o_parms[0]);
						}
						else
						{
							if (has_cont) tstr[6] = '-';	/* Put continuation in if needed.	*/
							has_cont = 0;
							write_line("%s%s",tstr,o_parms[0]);
						}


						if (ptype == -1)			/* write a period?			*/
						{
							tstr[startpos] = '\0';
							tstr[6] = ' ';
							finish_disp(ptype);
						}
						else if (ptype == 9)
						{
							tstr[startpos] = '\0';
							tstr[6] = ' ';
							finish_disp(ptype);
						}
						else
						{
							put_line("\n");			/* no period				*/
						}
					}
				} while ((ptype != -1) && (ptype != 9));		/* quit if period (or keyword)		*/
				if (ptype == 9)
				{
					finish_disp(ptype);				/* All done.				*/
					i = get_ppos();					/* Find the position of the keyword.	*/
					memset(inline,' ',i);				/* Wipe out any leftover stuff.		*/
					hold_line();					/* Now save the line.			*/
				}
				break;
			}

			case PROC_DELETE:
			{
				p_delete();						/* Examine DELETE statements.		*/
				break;
			}

			default:
			{
				put_line(inline);
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
				put_line("      * FREE ALL changed to UNLOCK ALL.\n");
				stredt(inline,"FREE","UNLOCK");
				put_line(inline);
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
				put_line("      * COMMIT changed to UNLOCK ALL.\n");
				stredt(inline,"COMMIT","UNLOCK ALL");
				put_line(inline);
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
			put_line(inline);
			break;
		}
	}
	else
	{
		put_line(inline);							/* For now, just copy it.		*/
	}

}

stmt_unsupported(verb)
char	*verb;
{
	write_line("******* WISP: Verb [%s] is not supported.\n",verb);
	write_log("WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",verb);
	put_line(inline);
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

	put_line		("           MOVE \"RW\" TO WISP-DECLARATIVES-STATUS\n");
	if (vax_cobol) put_line	("           MOVE \"N\" TO WISP-TEST-BYTE\n");
	put_line("          ");								/* start a new line			*/

	inv_key = 0;									/* no INVALID KEY yet...		*/
	lock_clause = 0;

	do
	{
		put_line(" ");
		write_line("%s",o_parms[0]);						/* output parm				*/

		if (o_parms[0][0]) stredt(inline,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			put_line("\n");							/* end the previous line		*/
			put_line("          ");  					/* start a new line 			*/
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
				put_line("\n               ALLOWING NO OTHERS");
				lock_clause = 1;
			}

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'W',"BADINVKEY",
				"Bad REWRITE syntax, INVALID KEY followed by a period.");
				o_parms[0][0] = 0;
			}
			else
			{
				ptype = get_param(o_parms[0]);				/* what to do?				*/
			}

			put_line        ("\n               INVALID KEY ");		/* write it out				*/
			if (vax_cobol)
			{
				put_line("\n               MOVE \"Y\" TO WISP-TEST-BYTE");
				put_line("\n           END-REWRITE");
			}
			inv_key = 1;							/* flag it				*/
		}
	}	while ((ptype != -1) && !keyword(o_parms[0],proc_keywords));		/* do till we hit a LAST parm or keyword*/


	put_line("\n");								/* finish this line			*/

	if (ptype == -1)								/* a LAST parm				*/
	{
		write_line("                   %s\n",o_parms[0]); 
	}

	if ( vax_cobol && !lock_clause )
	{
		put_line("               ALLOWING NO OTHERS\n");
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
			put_line  ("           IF WISP-TEST-BYTE = \"N\" THEN\n");
			write_line("               MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fd]);
			write_line("               UNLOCK %s ALL\n", fdname );
			write_line("               MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fd]);
			put_line  ("           ELSE\n");
		}
		else
		{
			write_line("           MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fd]);
			write_line("           UNLOCK %s ALL\n", fdname );
			write_line("           MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fd]);
		}
	}

	if (ptype == -1)
	{
		put_line  ("           CONTINUE.\n");
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
{											/* list after checking for uniqueness.	*/
	int i;

	for (i=0; i<pr_count; i++)
	{
		if (!strcmp(perf_decl[i],the_name)) return(0);				/* if we already have it, skip it.	*/
	}

	strcpy(perf_decl[pr_count++],the_name);						/* save it.				*/
}

static finish_disp(type)
int type;
{
	put_line("\n");
	put_line("               DELIMITED BY SIZE INTO WISP-CRT-SCREEN-AREA\n");
	put_line("               MOVE \"Press RETURN to proceed.\" TO WISP-SCREEN-LINE(24)\n");
	put_line("               MOVE WISP-FULL-SCREEN-ORDER-AREA TO WISP-CRT-ORDER-AREA\n");
	put_line("               CALL \"WDISPLAY\" USING WISP-CRT-RECORD");		/* Call WDISPLAY.			*/

	if (type == -1) put_line(".\n");						/* Terminat with correct item.		*/
	else		put_line("\n");
}
