static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	These are OLD wisp routines removed from other source files.
*/
#ifdef OLD

/*
**	wt_io.c
*/
static int skip_kw = 0;									/* Skip keyword processing once.	*/
static int skip_fac = 0;								/* Flag to indicate FAC processing skip.*/
static int else_flag = 0;								/* Check for ELSE ELSE.			*/

static int copy_code = 0;								/* not currently copying commented src	*/

static int	period_found= 0;							/* Has period been found.		*/

static int 	parm_num;								/* The current parm being scanned	*/

static int twiddle_count = 0;								/* Count how many times line is re-used.*/

static int oa_flag  = 0;								/* pending ORDER-AREA flag		*/
static int fac_flag = 0;								/* pending FAC flag			*/

/*
	hold_line:	Hold the current line for reuse by next get_line().
*/
void hold_line()									/* flag the last input line as held	*/
{
	hold_this_line(linein);

	invalidate_parms();							/* Reset the get param buffer			*/
	invalidate_token_cache();						/* Flush the token cache			*/

	if (twiddle_count > 1000)							/* Have we re-used this line too much?!	*/
	{
		write_log("WISP",'F',"INTERR","Internal error processing line\n<%s>\nProbably not a COBOL program.\n",linein);
		exit_wisp(EXIT_WITH_ERR);						/* Error exit.				*/
	}
	twiddle_count++;
}

/*
**	get_line:	Get the next input line. Do all the COPY stmt processing plus handle $xxx_CODE etc.
*/
int get_line()										/* Read a line and extract the parms	*/
{
	static int 	lstat = NORMAL_LINE;
	static int 	copy_in_copy_code = 0;					/* COPY stmt inside $xxx_CODE			*/
	static char 	brkline[256] = "";					/* The line to hold rest of linein after break.	*/

	int i,j,k,facpos,oapos,flen,fplen,pflag;
	char *scn_ptr,*prm_ptr;
	char tstr[132],outline[132],comstr[132];
	char	*split_line;								/* Where to split the line.		*/
	int	virgin_line;								/* Is this a new line or re-used.	*/
	int	scan_count;								/* Number of times line has been scanned*/

read_input:

	if (end_of_input())
	{
		/* 
		**	There is no more input and this is the second call to get_line() so just exit.
		*/
		exit_wisp(EXIT_OK);							/* All done, exit wisp.			*/
	}

	/*
	**	Get the next line to process using the following priority:
	**
	**		1) Held line	- this line has already been delivered up to be processed, has been
	**					partial processed/modified, and is re-inserted into the
	**					input stream to finish its processing.
	**
	**		2) brkline	- the last line get_line processed was split into two part, the first
	**					half has been delivered and processed, next we deal with the
	**					second half.
	**
	**		3) new line	- get a new line from the input steam.
	*/

	virgin_line = 0;								/* Assume not-virgin : re-used		*/
	if (held_line())								/* recycle the last line		*/
	{
		lstat = get_cobol_inline();						/* This will get held line.		*/
		skip_fac = 0;
		skip_kw = 0;
		else_flag = 0;								/* clear the else flag.			*/

		/*
		**	A side effect of the interchange between get_line() and get_statement() is that
		**	isalit can be left set by a get_line() before a get_statement() and a following
		**	get_line() will complain.  This fix should work in most cases.
		*/
		isalit = 0;
	}
	else
	{
		twiddle_count = 0;							/* No more twiddleing.			*/

		lstat = get_cobol_inline();						/* Get the next valid cleaned line	*/

		if (EOF == lstat) 							/* Read failed.				*/
		{
			tput_flush();
											/* There are no more files or data.	*/
			copy_to_dcl_file = 0;						/* Alway reset the copy flags.		*/
			copy_to_dtp_file = 0;
			copy_sect = 0;
			if (decl_performs_cnt)
			{
				write_log("WISP",'I',"PARSREF",
							"Paragraphs referenced in the PROCEDURE DIVISION by DECLARATIVES.\n");
				new_para();						/* New paragraphs were discovered.	*/
			}

			/*
			**	END OF INPUT.
			**	
			**	There are no more input lines.  
			**	Set the end of input flag then reset the parms info then return with
			**	the end of input code.  This gives the caller a chance to cleanup and finish
			**	processing whatever is being worked on.  The true exit will happen on the
			**	next call to get_line().
			*/
			parms[0][0] = (char)0;						/* There are no parms			*/
			parm_num = 0;							/* Reset parm number			*/
			linein[0] = (char)0;						/* clear the linein			*/

			return(0);							/* Return with EOI flag			*/
		}

		skip_fac = 0;								/* make sure it's fac's are processed.	*/
		skip_kw = 0;								/* Make sure it's keywords are too.	*/
		virgin_line = 1;							/* This is a new line.			*/
	}

	/*
	**	Set up the tput token context.  This is only needed when using the get_line() input method.
	*/
	tput_set_context(get_curr_cob_context());

	copy_code = (lstat == PROCESS_LINE);

	/* 
	**	Handle COMMENTS
	*/

	if (SPECIAL_COMMENT == lstat)						/* Always write SPECIAL COMMENTS		*/
	{
		tput_token_cache();
		goto read_input;
	}

	if ( iscomment(linein) )							/* Is it a comment?			*/
	{
		if (copy_only)								/* just copy it.			*/
		{
			tput_token_cache();						/* Comments are just copies		*/
		}
		else
		{
			if (comments)							/* not copying, are comments ok?	*/
			{
				tput_line("%s", linein);				/* Comments are just copies		*/
			}
		}
		goto read_input;
	}										/* end of comments processing		*/

	/*
	**	Handle NOPROCESS lines from conditional code.
	*/

	if (NOPROCESS_LINE == lstat)
	{
		/*
		**	Copy to output with NO processing.
		*/

		tput_token_cache();

		goto read_input;
	}

	if (!linein[0] || '\n' == linein[0])
	{
		/*
		**	If a blank line
		*/

		if (comments && virgin_line)
		{
			tput_blank();
		}

		goto read_input;
	}

/*	At this point, we scan the input line and extract all the parms from it, and also extract the area A data.		*/

	memcpy(area_a,&linein[7],4);							/* Get the chars in area a (col 8-11)	*/
	area_a[4] = 0;
	area_a_num = 0;
	sscanf(area_a,"%d",&area_a_num);						/* And see if they are a number		*/

	i = strlen(linein);
	if (linein[i-1] == '\n')
	{
		linein[i-1] = 0;
		linein[72] = 0;								/* Discard the notes collumn		*/
		if (i > 72) trim(linein);						/* and trim the notes, if any		*/
		strcat(linein,"\n");							/* then put the newline back		*/
	}
	else
	{
		linein[72] = 0;
		if (i > 72) trim(linein);
	}
	i = strlen(linein);


	/*
	**	SCAN THE LINE
	*/

	scan_count = 0;
scan_line:
	scan_count++;


	if (isalit && virgin_line)							/* Was last line an open literal?	*/
	{
		if (linein[6] != '-')
		{
			write_log("WISP",'W',"NOCONT","No continuation for open literal in column 7, added by WISP.");
			linein[6] = '-';
		}
	}

	isalit = 0;									/* Clear has an open literal flag	*/
	has_lit = 0;									/* Clear has a lit flag			*/
	has_cont = 0;									/* Clear has a continuation flag	*/

	period_found = 0;								/* A period has not yet been found	*/
	split_line = NULL;								/* Clear pointer where to split line	*/

	for (i=0; i<24; i++) parms[i][0] = 0;						/* Set parms to null string first	*/

	if (strlen(linein) > 7)								/* anything to scan?			*/
	{
		i = 0;
		j = 7;

		if (linein[6] == '-') has_cont = 1;					/* Does it have a continuation character*/

		scn_ptr = &linein[7];							/* Point to first char in margin A.	*/
		if (*scn_ptr) do							/* scan the line and extract the parms	*/
		{									/* This part of the 'if' is just used to*/
											/* skip over spaces, commas, newlines	*/
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == ';') ||
			    (*scn_ptr == '\n') || (*scn_ptr == '%'))
			{
				if (*scn_ptr == '%')					/* The '%' means comments.		*/
				{
					break;						/* Skip percent signs.			*/
				}							/* Done scanning for parms.		*/
				scn_ptr++;
				j++;
			}
			else	/* NOT whitespace */					/* This part of the 'if' copies the parm*/
			{
				if (*scn_ptr == '\"')					/* The parm is a literal.		*/
				{
					isalit = 1;					/* Flag the literal is 'opened'.	*/
					has_lit = 1;					/* Flag that there is a literal here.	*/
				}

				p_parms[i] = j;						/* remember where we started		*/
				prm_ptr = parms[i++];					/* copy the first character.		*/

				if (isalit)						/* If it is a literal...		*/
				{
					int embeddedquote;
					embeddedquote =0;

					do
					{						/* copy literals till null, ", or \n.	*/
						*prm_ptr++ = *scn_ptr++;		/* Copy the char.			*/
						j++;

						if (embeddedquote)
						{
							embeddedquote = 0;
						}
						else if (scn_ptr[0] == '\"') 		/* Ended on quote?			*/
						{
							if (scn_ptr[1] == '\"')		/* Embedded quote.			*/
								embeddedquote = 1;
							else
								isalit = 0;		/* closed the literal.			*/
						}
					} while ( isalit && (*scn_ptr) && (*scn_ptr != '\n') );

					while (!isalit &&				/* Literal closed, get periods, etc...	*/
 					       ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != ',') && (*scn_ptr != ';') &&
					        (*scn_ptr != '\n')&& (*scn_ptr != '\t')))
					{						/* copy till next whitespace		*/
						*prm_ptr++ = *scn_ptr++;
						j++;
					}

					if (isalit)					/* Left open, pad with spaces!!		*/
					{
						*prm_ptr = '\0';			/* Need a null to get length.		*/
						k = strlen(parms[i-1]) + p_parms[i-1];	/* First get length.			*/
						while (k < 72)
						{
							*prm_ptr++ = ' ';		/* Pad the parm with extra spaces.	*/
							*scn_ptr++ = ' ';		/* Pad linein, so stredt's will work too*/
							k++;
						}
						*scn_ptr++ = '\n';			/* Add the newline.			*/
						*scn_ptr = '\0';			/* And the null.			*/
					}
				} /* End-IF isalit */
				else							/* It's not a literal, copy the parm.	*/
				{

					if (period_found && !split_line) 		/* We found another token after the	*/
					{						/* period was found. So we need to	*/
						split_line = scn_ptr;			/* split the line at this point.	*/
						break;
					}

					do
					{						/* copy till next whitespace		*/

						*scn_ptr = toupper(*scn_ptr);		/* Shift all non-literals to uppercase	*/

						if ( period_found )    period_found = 0;
						if ( *scn_ptr == '.' ) period_found = 1;
						*prm_ptr++ = *scn_ptr++;
						j++;
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != ',') && (*scn_ptr != ';') &&
						 (*scn_ptr != '\n'));
				}

				if (*scn_ptr == ',')
				{
					scn_ptr++;					/* skip over commas			*/
					j++;
				}
				*prm_ptr = 0;						/* end with a null			*/
			} /* End-IF NOT whitespace */
		} while (*scn_ptr);							/* till a null is found			*/

		if (*scn_ptr == '%')							/* Remove imbedded comments.		*/
		{
			strcpy(comstr,"      *  ");					/* Start a comment line.		*/
			strcat(comstr,scn_ptr);						/* Add the %comment.			*/
			tput_line("%s", comstr);					/* Now output it.			*/

			*scn_ptr++ = '\n';						/* Remove it from LINEIN		*/
			*scn_ptr = '\0';
		}
	} /* End-IF something on line to scan */

	parm_num = 0;									/* Current parm for parsing		*/

	if (parms[0][0] == 0)								/* Must be a blank line			*/
	{										/* If blanks are kept, then just	*/
		if (blanklines) 							/* write a blank line			*/
		{
			i = strlen(linein);
			if (linein[i-1] == '\n')
			{
				linein[i-1] = 0;					/* remove the ending newline (if any)	*/
			}
			tput_line("%s", linein);					/* Blanks are treated as comments	*/
		}
		goto read_input;							/* Get another line			*/
	}

/*			 from here on we will process the parms and try to decipher what the program is doing.			*/
/* 			This section represents things to do irregardless of what division we are currently in, like		*/
/* 			comment and copy command processing is always done.							*/


	if (del_use)									/* We are deleting a USE procedure.	*/
	{										/* If it's the end of DECLARATIVES...	*/
											/* or a new SECTION...			*/
		if ((!strcmp(parms[0],"END") && !strcmp(parms[1],"DECLARATIVES.")) || !strncmp(parms[1],"SECTION.",7))
		{
			del_use = 0;							/* Stop deleting.			*/
		}
		else
		{
			goto read_input;						/* Otherwise delete it (skip it).	*/
		}
	}

	if ( split_line )								/* Split the line.			*/
	{
		strcpy(brkline,"           ");						/* Copy 2nd half to brkline.		*/
		strcat(brkline,split_line);
		split_line[0] = '\n';							/* Terminate 1st half of line.		*/
		split_line[1] = '\0';
		goto scan_line;								/* Rescan.				*/
	}

	if (	0==strcmp(parms[0],"MOVE") ||
		0==strcmp(parms[0],"SET")  ||
		0==strcmp(parms[0],"IF")     )
	{
		/*
		**	MOVE is going to use the get_verb_statement() logic which will
		**	handle the FAC OF and ORDER-AREA OF stuff
		*/
		skip_fac = 1;
		fac_flag = 0;
		oa_flag = 0;
	}


	if (!copy_only && (division == PROCEDURE_DIVISION) && !skip_fac)
	{
											/* Handle FAC OF problems		*/
		if (1 == scan_count && 1 == fac_flag)					/* finish a pending FAC			*/
		{
			fac_flag = 0;
			write_log("WISP",'I',"FINISHFAC","Finished PENDING FAC.");
			strcpy(templine,parms[0]);					/* get variable name			*/
			if (!strcmp(templine,"OF"))					/* it was really OF			*/
			{
				parms[0][0] = 0;
				strcpy(templine,parms[1]);				/* get actual variable name		*/
				stredt(linein," OF","");				/* remove the word			*/
			}
			stredt(templine,".","");					/* Remove period, if any from var name	*/

			make_fac(outline,templine);					/* make a FAC variable			*/

			stredt(linein,templine,outline);				/* and replace old var with new		*/
			wsqueeze(linein,72);						/* keep the line less than 73 chars	*/
			goto scan_line;							/* go and re-parm the line		*/
		}
		else if (fac_flag == 2) fac_flag--;

		if (1 == scan_count && 1 == oa_flag)					/* finish a pending ORDER-AREA		*/
		{
			oa_flag = 0;
			write_log("WISP",'I',"FINISHOA","Finished PENDING ORDER-AREA.");
			strcpy(templine,parms[0]);					/* get variable name			*/
			if (!strcmp(templine,"OF"))					/* it was really OF			*/
			{
				parms[0][0] = 0;
				strcpy(templine,parms[1]);				/* get actual variable name		*/
				stredt(linein," OF","");				/* remove the word			*/
			}
			stredt(templine,".","");					/* Remove period, if any from var name	*/

			make_oa(outline,templine);					/* make an ORDER-AREA variable		*/
			stredt(linein,templine,outline);				/* and replace old var with new		*/
			wsqueeze(linein,72);						/* keep the line less than 73 chars	*/
			goto scan_line;							/* go and re-parm the line		*/
		}
		else if (oa_flag == 2) oa_flag--;

		i = 0;
		j = 0;
		pflag = 0;
		do
		{									/* FAC found				*/
			if (!strcmp(parms[i],"FAC") || !strcmp(parms[i],"(FAC"))
			{
				write_log("WISP",'I',"FACOFFIX","FAC OF statement corrected.");
				facpos = p_parms[i] - 1;				/* get position of parm in line		*/

				if (parms[i][0] == '(')
				{
					pflag = 1;					/* Flag it.				*/
					stredt(&linein[facpos],"(FAC","(");		/* Edit (FAC into spaces		*/
					flen = 2;
				}
				else
				{
					stredt(&linein[facpos]," FAC","    ");		/* Edit FAC into spaces			*/
					flen = 4;
					facpos += 3;					/* advance the pointer.			*/
				}

				p_left(i,flen);						/* Shift parms left one position.	*/

				j++;

				if (!strcmp(parms[i],"OF"))				/* is it followed by OF verb		*/
				{
					flen = -1;					/* Remove 4 chars during shift.		*/
					if (stredt(&linein[facpos]," OF ","") == -1)	/* Edit OF into null, leaving room for	*/
					{						/* F-O-	to be appended			*/
						stredt(&linein[facpos]," OF","");	/* it is at end of line			*/
						flen = 0;
					}
					p_left(i,flen);					/* Shift parms left one position.	*/
					j++;
				}


				if (parms[i][0] == 0)					/* the variable must be on next line	*/
				{
					if (j < 3) fac_flag = 1;			/* The line only has FAC OF, 1 scan	*/
					else fac_flag = 2;				/* else takes 2 scans			*/
					write_log("WISP",'I',"STARTPFAC","Start PENDING FAC.");
				}
				else
				{
					strcpy(templine,parms[i]);			/* get variable name			*/
					stredt(templine,".","");			/* Remove period, if any from var name	*/
					fplen = strlen(templine);
	
					make_fac(outline,templine);			/* make a FAC variable			*/

					flen = 2;

					stredt(&linein[facpos],templine,outline);	/* And replace old var with new		*/

				}
				goto scan_line;						/* go and re-parm the line		*/
			}
											/* ORDER-AREA OF found			*/
			else if (!strcmp(parms[i],"ORDER-AREA") || !strcmp(parms[i],"(ORDER-AREA"))
			{
				write_log("WISP",'I',"FIXOA","ORDER-AREA OF statement corrected.");
				oapos = p_parms[i] - 1;					/* get position of parm in line		*/

				if (parms[i][0] == '(')
				{
					stredt(&linein[oapos],"(ORDER-AREA","(");	/* Edit (ORDER-AREA into spaces		*/
					flen = 2;
				}
				else
				{
					stredt(&linein[oapos]," ORDER-AREA","           ");	/* Edit ORDER-AREA into spaces	*/
					flen = 11;
					oapos +=10;
				}

				p_left(i,flen);						/* Shift parms left one position.	*/

				j++;

				if (!strcmp(parms[i],"OF"))
				{
					flen = -1;
					if (stredt(&linein[oapos]," OF ","") == -1)	/* Edit OF into null, leaving room for	*/
					{						/* O-A-	to be appended			*/
						stredt(&linein[oapos]," OF","");	/* it is at end of line			*/
						flen = 0;
					}
					p_left(i,flen);					/* Shift parms left one position.	*/
					j++;
				}
	
				if (parms[i][0] == 0)					/* the variable must be on next line	*/
				{
					if (j < 3) oa_flag = 1;
					else oa_flag = 2;				/* takes 2 scans			*/
					write_log("WISP",'I',"STRTOA","Start PENDING ORDER-AREA.");
				}
				else
				{
					strcpy(templine,parms[i]);			/* get variable name			*/
					stredt(templine,".","");			/* Remove period, if any from var name	*/

					make_oa(outline,templine);			/* make an ORDER-AREA variable		*/

					stredt(&linein[oapos],templine,outline);	/* and replace old var with new		*/
				}
				goto scan_line;						/* go and re-parm the line		*/
			}
			i++;
		}
		while ( parms[i][0] );							/* Till no more parms on this line	*/
	}
	else
	{
		skip_fac = 0;								/* Be sure to allow FAC processing.	*/
	}

	if (!copy_only && parms[0][0] != '\"')						/* need to examine for keywords		*/
	{
		i = 0;
		j = 0;									/* used to remember edit offset		*/

		if (else_flag)								/* Was there an ELSE by itself before?	*/
		{
			else_flag = 0;							/* Always clear the ELSE flag.		*/
			if (!strcmp(parms[0],"ELSE"))					/* Handle possible ELSE ELSE.		*/
			{
				strcpy(brkline,linein);					/* save the current line.		*/
				strcpy(linein,"             CONTINUE\n");		/* Put a CONTINUE between the lines.	*/
				goto scan_line;						/* Parm the CONTINUE line.		*/
			}
		}

		do
		{
			p_parms[i] += j;						/* add any offsets...			*/
			strcpy(tstr,parms[i]);						/* Now take the name.			*/
			stredt(tstr,"(","");						/* Remove any open paren.		*/
			stredt(tstr,")","");						/* Remove any close paren.		*/
			stredt(tstr,".","");						/* Remove any periods.			*/
	
			if (!skip_kw && kwproc && reserved_keyword(tstr))		/* Have we found one.			*/
			{
				write_log("WISP",'I',"REPLRESERVE","Reserved word %s renamed to W-%s.\n",tstr,tstr);
				strcpy(templine,"W-");
				strcat(templine,tstr);					/* build the new name for it.		*/
				stredt(&linein[p_parms[i]],tstr,templine);		/* replace it in the input line		*/

				if (wsqueeze(linein,72)) goto scan_line;		/* If it needed a squeeze, reparm it.	*/

				stredt(parms[i],tstr,templine);				/* and fix up the parm list too		*/
				j += 2;							/* new offset ptr			*/
			}
			else if (!strcmp(tstr,"ELSE"))
			{
				if (!parms[i+1][0])					/* If ELSE is followed by blanks,	*/
				{
					else_flag = 1;					/* Set the else flag.			*/
				}
				else							/* Otherwise, set up for fixes.		*/
				{
					write_log("WISP",'I',"ELSEBROK","Else statement broken apart. Old line is:\n%s\n",linein);
					memset(brkline,' ',p_parms[i]+4);		/* Fill with spaces first.		*/
					brkline[p_parms[i]+4] = '\0';			/* Null terminate.			*/
					strcat(brkline,&linein[p_parms[i] + 4]);	/* save the current line.		*/
					linein[p_parms[i] + 4] = '\n';			/* Terminate the old line		*/
					linein[p_parms[i] + 5] = '\0';			/* Terminate the old line		*/
					parms[i+1][0] = '\0';				/* No more parms.			*/
				}
			}								/* After skipping first word,		*/
			else if ( i && brk_keyword(tstr))				/* Is it a second keyword?		*/
			{
				memset(brkline,' ',p_parms[i]);				/* Fill with spaces first.		*/
				brkline[p_parms[i]] = '\0';				/* Null terminate.			*/
				strcat(brkline,&linein[p_parms[i]]);			/* save the current line.		*/
				linein[p_parms[i]] = '\n';				/* Terminate the old line		*/
				linein[p_parms[i] + 1] = '\0';				/* Terminate the old line		*/
				parms[i][0] = '\0';					/* No more parms.			*/
				write_log("WISP",'I',"SECKEWD","Second keyword on line.\nOld: %sNew: %s",linein,brkline);
				break;
			}
			i++;
		} while ( parms[i][0] );						/* Till all parms have been checked	*/
	}

	if (brkline[0])
	{
		hold_this_line(brkline);
		brkline[0] = (char)0;
	}
	return( 1 );									/* Got a line				*/
}


p_left(pnum,p_off)									/* Shift all parms left in the list.	*/
int pnum,p_off;
{
	while ((pnum < 23) && parms[pnum][0])						/* While there are parms to move...	*/
	{
		strcpy(parms[pnum],parms[pnum+1]);
		p_parms[pnum] += p_off;							/* Update their positions.		*/
		pnum++;
	}
	return 0;
}

p_left_1(pnum,p_off)									/* Shift all parms left in the list.	*/
int pnum,p_off;
{

	while ((pnum < 23) && parms[pnum][0])						/* While there are parms to move...	*/
	{
		strcpy(parms[pnum],parms[pnum+1]);
		p_parms[pnum] = p_parms[pnum+1] + p_off;				/* Shift their positions.		*/
		pnum++;
	}
	return 0;
}

/*
	extract_param	This routine will get the next parm and remove it from linein.
*/
int extract_param(the_parm)
char 	the_parm[];
{
	int	ptype;

	ptype = get_param(the_parm);
	stredt(&linein[7],the_parm,"");
	return(ptype);
}

int invalidate_parms()
{
	parm_num = 0;
	parms[parm_num][0] = 0;
	return 0;
}

int get_param(the_parm)									/* Scan input parms			*/
char the_parm[];									/* returns a 0 if a "normal" parm	*/
{											/* returns a 1 if first parm in a line	*/
	int retval,i;									/* returns -1 if last parm (has period)	*/
	retval = 0;									/* assume a normal parm			*/
	g_newline = 0;									/* Global newline flag.			*/
	if (parms[parm_num][0] == 0)
	{										/* No parms left, read another line	*/
		get_line();
		retval = 1;								/* First parm in a line			*/
	}
	else if (!parm_num)								/* Or someone just read it.		*/
	{
		retval = 1;
	}

	g_newline = retval;								/* Do we have a newline?		*/
											/* We set this to get around the problem*/
											/* of a new line that contains one arg	*/
											/* with a period returning -1 not 1.	*/

	strcpy(the_parm,parms[parm_num++]);						/* Copy the parm into the target string	*/

	i = strlen(the_parm) - 1;

	if (the_parm[0] == '\"' && the_parm[i-1] != '\"' && the_parm[i] != '\"')	/* If its an open literal...		*/
	{
											/* Just return.				*/
	}
	else if (the_parm[i] == '.')
	{
		the_parm[i] = 0;							/* remove the period			*/
		retval = -1;								/* it's the last parm			*/
	}
	else if ((parms[parm_num][0] == '.') && (!parms[parm_num][1]))			/* Is it a period alone?		*/
	{
		parms[parm_num][0] = '\0';						/* a period alone...			*/
		retval = -1;								/* it's the last parm			*/
	}
	return(retval);
}


int get_ppos()										/* Return the location of the last parm	*/
{											/* must be called AFTER get_param	*/
	int retval;

	if (parm_num == 0)
	{
		retval = 0;								/* some problem, called before parm was	*/
	}										/* fetched				*/
	else
	{
		retval = p_parms[parm_num-1];						/* return the actual location		*/
	}

	return(retval);									/* easy to do				*/
}

/*
	next_param	This routine returns the next param without incrementing the pointer.
			It will call get_line() if at end of current line.
*/
int next_param(the_parm)
char the_parm[];
{
	if (parms[parm_num][0] == 0)
	{										/* No parms left, read another line	*/
		get_line();
	}
	return( peek_param(the_parm) );
}


/*
	peek_param	The routine will return the next param on the current line.
*/
											/* same as get_param but no advance	*/
int peek_param(the_parm)								/* Scan input parms			*/
char the_parm[];									/* returns a 0 if a "normal" parm	*/
{											/* returns a 1 if first parm in a line	*/
	int retval,i;									/* returns -1 if last parm (has period)	*/
	retval = 0;									/* assume a normal parm			*/

	strcpy(the_parm,parms[parm_num]);						/* Copy the parm into the target string	*/

	i = strlen(the_parm);

	if (i)
	{
		i--;									/* point to end				*/
	}
	else
	{										/* zero length				*/
		retval = 1;								/* first parm on a new line		*/
	}

	if (the_parm[i] == '.')
	{
		the_parm[i] = 0;							/* remove the period			*/
		retval = -1;								/* it's the last parm			*/
	}
	return(retval);
}

skip_param(num_parms)									/* Skip input parms			*/
int num_parms;
{
	do
	{
		parm_num++;
		if (parms[parm_num][0] == 0)						/* No parms left, read another line	*/
		{
			get_line();
		}
	} while (--num_parms);
	return(0);
}
/*
**	END wt_io.c
*/

/*
**	keywords.c
*/
int keyword (char *the_word, char *the_list[])				/* search a list of keywords to see if the_word is in	*/
{
	int 	i;
	char	**list_ptr;

	for(i=0, list_ptr = the_list; **list_ptr; i++, list_ptr++)	/* this routine will return keyword numbers starting	*/
	{								/* with 1, not 0.					*/
		if (*the_word == **list_ptr)				/* Is first char the same ?				*/
		{
			if (0==strcmp(the_word,*list_ptr)) 
				return(i+1);				/* found a match					*/
		}		
	}

	return(0);							/* no match						*/
}

/* 					This is a list of keywords which begin something					*/
static char *proc_keywords[] = 	
{
	"ACCEPT", "ADD","ALTER",
	"BEGIN",
	"CALL","CANCEL","CLOSE","COMMIT","COMPUTE","CONTINUE",
	"DELETE","DISABLE","DISPLAY","DIVIDE",
	"ELSE","ENABLE","ENTER","EXIT",
	"FREE",
	"GENERATE","GO",
	"HOLD",
	"IF","INITIALIZE","INITIATE","INSPECT",
	"MERGE","MOVE","MULTIPLY",
	"NEXT",
	"OPEN",
	"PERFORM",
	"READ","READY","RECEIVE","RELEASE","RESET","RETURN","REWRITE","ROLLBACK",
	"SEARCH","SEND","SET","SORT","START","STOP","STRING","SUBTRACT","SUPRESS",
	"TERMINATE",
	"UNSTRING",
	"WHEN","WRITE",
	""
};

int proc_keyword(char *the_word)
{
	static int count = 0;

	if (0==count)
	{
		for(;proc_keywords[count][0];count++);
	}

	return(binkeyword(the_word,proc_keywords,count));
}

static char *brk_keywords[] = 	
{
	"ACCEPT",
	"CALL","CLOSE","COMMIT",
	"DELETE","DISPLAY",
	"EXIT",
	"FREE",
	"GO",
	"IF",
	"MOVE",
	"OPEN",
	"PERFORM",
	"READ","REWRITE","ROLLBACK",
	"SEARCH","SORT","START","STOP",
	"WHEN","WRITE",
	""
};

int brk_keyword(char *the_word)
{
	static int count = 0;

	if (0==count)
	{
		for(;brk_keywords[count][0];count++);
	}

	return(binkeyword(the_word,brk_keywords,count));
}

/*
**	wt_procd.c
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

			exit_copy_mode(the_sentence);

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

static int p_proc(void)
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
			case PROC_ACCEPT:
			case PROC_CALL:
			case PROC_CLOSE:
			case PROC_DELETE:
			case PROC_DISPLAY:
			case PROC_ELSE:
			case PROC_EXIT:
			case PROC_GOTO:
			case PROC_IF:
			case PROC_MOVE:
			case PROC_OPEN:
			case PROC_PERFORM:
			case PROC_READ:
			case PROC_SET:
			case PROC_START:
			case PROC_STOP:
			{
				hold_line();
				get_line();

				the_statement = get_statement_fragment();
				parse_verb_statement(the_statement, NULL);

				hold_token_cache();
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

			case PROC_WRITE:						/* WRITE statement.			*/
			{
				p_write();
				break;
			}

			case PROC_SORT:							/* Process SORT statements.		*/
			{
				write_log("WISP",'I',"SORTFOUND","SORT statement being processed.");
				p_sort();
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
			{
				hold_line();
				get_line();

				the_statement = get_statement_fragment();
				parse_verb_statement(the_statement, NULL);

				hold_token_cache();
				break;
			}
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
		hold_line();
		get_line();

		the_statement = get_statement_fragment();
		parse_verb_statement(the_statement, NULL);

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
NODE parse_verb_statement(NODE the_statement, NODE the_sentence)
{
	NODE	verb_node, original_statement;

	if (!the_statement) return(the_statement);

	original_statement = the_statement;
	
	verb_node = first_token_node(the_statement);

	if(!verb_node)
	{
		tput_statement(12,the_statement);
		return( free_statement(the_statement) );
	}

	if (eq_token(verb_node->token,VERB,"MOVE"))
	{
		the_statement = parse_move(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"START"))
	{
		the_statement = parse_start(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"ACCEPT"))
	{
		the_statement = parse_accept(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"CALL"))
	{
		the_statement = parse_call(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"CLOSE"))
	{
		the_statement = parse_close(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"COMMIT"))
	{
		the_statement = parse_commit(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"DELETE"))
	{
		the_statement = parse_delete(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"EXIT"))
	{
		the_statement = parse_exit(the_statement);
	}
	else if (eq_token(verb_node->token,KEYWORD,"ELSE"))	/* NOTE: ELSE is a KEYWORD not a VERB */
	{
		the_statement = parse_else(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"FREE"))
	{
		the_statement = parse_free(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"GO"))
	{
		the_statement = parse_go(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"OPEN"))
	{
		the_statement = parse_open(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"PERFORM"))
	{
		the_statement = parse_perform(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"STOP"))
	{
		the_statement = parse_stop(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"IF"))
	{
		the_statement = parse_if(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"DISPLAY"))
	{
		the_statement = parse_display(the_statement, the_sentence);
	}
	else if (eq_token(verb_node->token,VERB,"SET"))
	{
		the_statement = parse_set(the_statement);
	}
	else if (eq_token(verb_node->token,VERB,"READ"))
	{
		the_statement = parse_read(the_statement, the_sentence);
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
		if (the_statement != original_statement)
		{
			/*
			**	There is something remaining but it's different then 
			**	the original so re-process it.
			*/
			the_statement = parse_verb_statement(the_statement, the_sentence);
		}
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

static void stmt_unsupported(char* verb)
{
	tput_scomment("****** WISP: Verb [%s] is not supported. ******",verb);
	write_log("WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",verb);
	tput_line("%s",linein);
}

static NODE parse_else(NODE the_statement)
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

static void p_rewrite(void)
{

	int i,fnum,inv_key;
	char	recname[40];
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

	prerewrite_locking(16);								/* Add locking logic			*/

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

/*
**	wt_divs.c
*/
check_section()										/* Examine the current line, which should*/
{											/* Be the first line in the PROCEDURE	*/
	return 0;
}

nopunct(dst,src)
char	*dst, *src;
{
	char	i,j;
	for(i=0,j=0;src[i];i++)
	{
		if (src[i] != '.' && src[i] != ',' && src[i] != ';') dst[j++] = src[i];
	}
	dst[j] = '\0';
	return 0;
}

int check_proc_div()
{
	/*
	**	This was extracted out of check_div() for use in PROCEDURE DIVISION only.
	*/

	int i,j;
	char	token[80];

	if (!in_decl) return(0);						/* no divisions after proc. & declare.	*/

										/* Must be in DECLARATIVES.		*/
	nopunct(token,parms[1]);
	if (!strcmp(parms[0],"END") && !strcmp(token,"DECLARATIVES"))		/* look for end of declaratives		*/
	{									/* Found it, add our entries.		*/
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

		if (prog_cnt -prog_sort)					/* If there were valid files...		*/
		{
			gen_defdecl();						/* Generate declaratives for files with	*/
		}								/* no declaratives.			*/
		tput_line("%s", linein);	       				/* Put the END DECLARATIVES out.	*/
		get_line();							/* Get a new line.			*/
		check_section();						/* See if it's in a section		*/
		in_decl = 0;
		return(1); /* input stream has changed */
	}
	else
	{
		return(0);
	}
}

/*
** 	wt_opcls.c
*/

int p_open(void)
{
	char o_fnames[MAX_FILES][40];							/* a place to save the file names	*/
	char o_fmode[MAX_FILES][40];							/* And the open mode for the file.	*/
	char o_allow[MAX_FILES][40];							/* Also if ALLOWING needed.		*/
	int  o_fref[MAX_FILES];								/* ref nums of the files		*/
	int  o_omode[MAX_FILES];
	int  o_count;									/* a count of the file names saved	*/
	int done,is_crt;
	int  fnum;
	int open_mode,syntax_ok;

	o_count = 0;									/* no files yet				*/
	done = 0;									/* not done yet				*/
	open_mode = 0;
	syntax_ok = 0;									/* Not sure if the syntax is ok yet.	*/

	ptype = get_param(o_parms[0]);							/* skip the OPEN verb			*/
	stredt(linein,o_parms[0],"");							/* Remove the word.			*/

	o_parms[5][0] = '\0';								/* Clear the OPEN MODE string		*/
	o_parms[6][0] = '\0';								/* And the trailing MODE string		*/

	do
	{
		ptype = get_param(o_parms[0]);						/* get a word				*/

		is_crt = 0;
		if ((cur_crt = crt_index(o_parms[0])) != -1) 
		{
			is_crt = 1;
			syntax_ok = 1;
		}

		fnum = 0;								/* It's not a file name yet		*/
		if (!strcmp(o_parms[0],"SHARED"))					/* handle SHARED phrase	    		*/
		{
			open_mode = WFOPEN_SHARED;
		}									/* Handle other modes			*/
		else if (!strcmp(o_parms[0],"SPECIAL-INPUT"))				/* handle SPECIAL-INPUT phrase		*/
		{
			open_mode = WFOPEN_SPECIAL_INPUT;
		}									/* Handle other modes			*/
		else if (!strcmp(o_parms[0],"I-O"))					/* handle I-O phrase			*/
		{
			open_mode = WFOPEN_I_O;
		}									/* Handle other modes			*/
		else if (!strcmp(o_parms[0],"INPUT"))
		{
			open_mode = WFOPEN_INPUT;
		}
		else if (!strcmp(o_parms[0],"OUTPUT"))
		{
			open_mode = WFOPEN_OUTPUT;
		}
		else if (!strcmp(o_parms[0],"EXTEND"))
		{
			open_mode = WFOPEN_EXTEND;
		}
		else if (is_crt)							/* delete CRT file opens		*/
		{
			write_log("WISP",'I',"DELCRTOPEN","Removed OPEN of crt file, %s.",crt_file[cur_crt]);
		}
		else									/* see if it is a file name		*/
		{
			fnum = file_index(o_parms[0]);

			if (fnum != -1)
			{
				syntax_ok = 1;						/* Syntax is ok, there was a file.	*/
				if (o_count > MAX_FILES)				/* Exceeded file buffer.		*/
				{
					write_log("WISP",'F',"EXCFILECNT","Exceeded FILE count in OPEN statement.");
					exit_wisp(EXIT_WITH_ERR);
				}

				o_omode[o_count] = open_mode;
				o_fref[o_count] = fnum;					/* remember which one it is		*/
				prog_ref[fnum]++;					/* Say it was used.			*/

				if (open_mode == WFOPEN_SHARED)				/* handle SHARED phrase	    		*/
				{							/* If Sequential, change to EXTEND	*/
					if (prog_ftypes[fnum] & (SEQ_DYN + SEQ_FILE))
					{
						strcpy(o_fmode[o_count],"EXTEND");	/* Make it EXTEND.			*/
					}
					else
					{
						strcpy(o_fmode[o_count],"I-O");		/* Change to I-O			*/
					}

					if (lpi_cobol||mf_aix)
						o_allow[o_count][0] = '\0';		/* LPI don't work like this.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING ALL");	/* And the trailing mode	*/

					if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
					{
						write_log("WISP",'W',"AUTOLOCK","File %s OPEN SHARED using AUTOLOCK.",o_parms[0]);
					}

				}							/* Handle other modes			*/
				else if (open_mode == WFOPEN_SPECIAL_INPUT)		/* handle SPECIAL-INPUT phrase		*/
				{
					strcpy(o_fmode[o_count],"INPUT");		/* CHANGE IT				*/
					if (lpi_cobol||mf_aix)
						o_allow[o_count][0] = '\0';		/* LPI don't work like this.		*/
					else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING ALL");	/* And the trailing mode	*/
				}							/* Handle other modes			*/
				else if (open_mode == WFOPEN_I_O)				/* handle I-O phrase			*/
				{
					strcpy(o_fmode[o_count],"I-O");			/* Set to I-O				*/

					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if ((vax_cobol || acu_cobol) && (prog_ftypes[fnum] & OPENIOX_FILE))
						strcpy(o_allow[o_count]," ALLOWING NO OTHERS");	/* OPEN I-O exclusive		*/
					else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING READERS");	/* Manual record locking	*/
											/* Vax needs readers - READFDR		*/
				}							/* Handle other modes			*/
				else if (open_mode == WFOPEN_INPUT)
				{
					strcpy(o_fmode[o_count],"INPUT");		/* save the mode			*/
					o_allow[o_count][0] = 0;			/* Wang/VAX/LPI allow other readers	*/
					if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else if (vax_cobol || acu_cobol)		/* Manual record locking		*/
						strcpy(o_allow[o_count], " ALLOWING READERS");
				}
				else if (open_mode == WFOPEN_OUTPUT)
				{
					strcpy(o_fmode[o_count],"OUTPUT");		/* save the mode			*/
					strcpy(o_allow[o_count]," ");			/* Automatic record locking.		*/
					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING NO OTHERS");	/* Manual record locking.	*/
#ifdef OLD
					/*
					**	The "ALLOWING READERS" clause on the VAX was causing
					**	it to be very slow.  On the Wang an OPEN OUTPUT is exclusive,
					**	so the only reason for this must have been the "READFDR" problem.
					*/
					else
						strcpy(o_allow[o_count]," ALLOWING READERS");	/* Manual record locking.	*/
#endif
				}
				else if (open_mode == WFOPEN_EXTEND)
				{
					strcpy(o_fmode[o_count],"EXTEND");		/* save the mode			*/
					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING READERS");	/* Manual record locking.	*/
				}

				strcpy(o_fnames[o_count],o_parms[0]);			/* save it				*/
				o_count++;
			}
		}									/* not found, i = prog_cnt		*/
		if ((ptype == -1) || (fnum == -1)) done = 1;				/* period or no file name means done.	*/
		if (!done) stredt(linein,o_parms[0],"");				/* Remove the last word.		*/
	} while (!done);								/* continue till done			*/
											/* If we ended on a keyword, then we set*/
	if (fnum == -1) ptype = 1;							/* up as if we ended on a new line.	*/

	if (o_count)									/* output the open statements		*/
	{
		int i;
		
		for (i=0; i<o_count; i++)
		{
			int	gen_bitset;

			if (isaproc)
			{
				gen_bitset = 1;
			}
			else
			{
				gen_bitset = 0;
			}

			if (gen_bitset)
			{
				make_fld(o_parms[9],o_fnames[i],"S-");			/* create the status byte		*/
				tput_line          ("           MOVE ZERO TO WISP-BIT-CLEAR, WISP-BIT-SET\n");

				if (isaproc)						/* Was the procedure flag set?		*/
				{
					tput_line  ("           ADD WISP-PROC    TO WISP-BIT-SET,\n");
				}
				tput_line  	   ("           CALL \"wsetstat\" USING WISP-BIT-SET, WISP-BIT-CLEAR,\n");
				tput_line          ("                                 %s\n",o_parms[9]);
			}

			if (prog_ftypes[o_fref[i]] & PRINTER_FILE)
			{
				if (!prog_fnames[o_fref[i]][0])			/* If no FD FILENAME given then...		*/
				{
					make_fld(o_parms[9],o_fnames[i],"F-");
					tput_line("           MOVE SPACES TO %s,\n",o_parms[9]);
				}
				else if (prog_fnames[o_fref[i]][0] == '\"')	/* If FD FILENAME is literal then...		*/
				{
					make_fld(o_parms[9],o_fnames[i],"F-");
					tput_line("           MOVE %s TO %s,\n",prog_fnames[o_fref[i]],o_parms[9]);
				}
			}

			if (!(prog_ftypes[o_fref[i]] & NORESPECIFY))				/* Was the NORESPECIFY flag set?*/
			{									/* If RESPECIFY, then PERFORM it.*/
				tput_line("           PERFORM WITH TEST AFTER UNTIL\n");
				tput_line("               %s < \"10\"\n",prog_fstats[o_fref[i]]);
			}

			make_fld(o_parms[9],o_fnames[i],"S-");				/* create the status byte		*/
			tput_line_at(16, "MOVE \"OP\" TO WISP-DECLARATIVES-STATUS\n");	/* Signal an open.		*/

			if (x4dbfile && (prog_ftypes[o_fref[i]] & INDEXED_FILE))
			{
				/*
				**	If an INDEXED file and the x4dbfile flag set then
				**	add runtime logic to set the IS_DBFILE flag.
				*/
				tput_line_at(16, "CALL \"x4dbfile\" USING \"%s\",",o_fnames[i]);
				tput_clause (20, "%s", o_parms[9]);
			}

			tput_line_at(16, "CALL \"wfopen3\" USING %s,\n",o_parms[9]);	/* start with the status byte	*/

									/* literal or null volume, pass the created field */
			if ((prog_vnames[o_fref[i]][0] == '\"') || (!prog_vnames[o_fref[i]][0]))
			{
				make_fld(o_parms[9],o_fnames[i],"V-");
				tput_clause(20, "%s,",o_parms[9]);
			}
			else
			{								/* use the programs own field		*/
				tput_clause(20, "%s,",prog_vnames[o_fref[i]]);
			}

										/* literal or null lib, pass the created field */
			if ((prog_lnames[o_fref[i]][0] == '\"') || (!prog_lnames[o_fref[i]][0]))
			{
				make_fld(o_parms[9],o_fnames[i],"L-");
				tput_clause(20, "%s,",o_parms[9]);
			}
			else
			{								/* use the programs own field		*/
				tput_clause(20, "%s,",prog_lnames[o_fref[i]]);
			}

										/* literal or null name, pass the created field */
			if ((prog_fnames[o_fref[i]][0] == '\"') || (!prog_fnames[o_fref[i]][0]))
			{
				make_fld(o_parms[9],o_fnames[i],"F-");
				tput_clause(20, "%s,",o_parms[9]);
			}
			else
			{								/* use the programs own field		*/
				tput_clause(20, "%s,",prog_fnames[o_fref[i]]);
			}

											/* always pass the created name		*/
			make_fld(o_parms[9],o_fnames[i],"N-");
			tput_clause(20, "%s,",o_parms[9]);
			tput_clause(20, "WISP-APPLICATION-NAME,");
			make_fld(o_parms[9],o_fnames[i],"PR-");
			tput_clause(20, "%s,",o_parms[9]);

			tput_clause(20, "%s,",open_mode_name[o_omode[i]]);

			tput_line_at(16, "OPEN %s %s",o_fmode[i],o_fnames[i]);
			tput_clause (20, "%s",o_allow[i]);
			if (vax_cobol)
			{
				make_fld(o_parms[9],o_fnames[i],"S-");			/* create the status byte		*/
				tput_line("               CALL \"wdellock\" USING %s,\n",o_parms[9]);
				make_fld(o_parms[9],o_fnames[i],"N-");
	                        tput_line("                                     %s\n",o_parms[9]);
			}
			tput_line  ("               MOVE SPACES TO WISP-DECLARATIVES-STATUS");	/* Signal an open.		*/

			if (!(prog_ftypes[o_fref[i]] & NORESPECIFY))			/* Was the NORESPECIFY flag set?	*/
			{
				tput_line("           END-PERFORM");			/* No, End of the perform.		*/
			}

			if ((i == o_count-1) && (ptype == -1)) tput_clause(12, ".");	/* Last file name needs a period?	*/
			else				       tput_clause(12, ",");


		}
		isaproc = 0;								/* Clear the proc flag if needed.	*/
	}
	else										/* There were no files, must have been	*/
	{										/* a CRT file only. write a place holder*/
		if (ptype == -1)
			tput_line("           CONTINUE.\n");
		else
			tput_line("           CONTINUE,\n");
	}
	if (ptype == 1) hold_line();							/* ended on a new line, hold it		*/

	if (!syntax_ok)
	{
		write_log("WISP",'F',"OPNSYN","Syntax Error in OPEN statement, No file name.\n%s",linein);
		exit_wisp(EXIT_WITH_ERR);
	}
	return 0;
}

			/* Process CLOSE statements for possible multiple file names amd CRT file closes	*/

p_close()
{
	char c_fnames[MAX_FILES][40];							/* a place to save the file names	*/
	char c_flags[MAX_FILES];							/* Remember their types too.		*/
	int  c_count;									/* a count of the file names saved	*/
	int i,done,is_crt;

	c_count = 0;									/* no files yet				*/
	done = 0;									/* not done yet				*/

	ptype = get_param(o_parms[0]);							/* skip the CLOSE verb		*/
	stredt(&linein[7],o_parms[0],"");

	do
	{
		ptype = get_param(o_parms[0]);						/* get a word				*/

		is_crt = 0;
		if ((cur_crt = crt_index(o_parms[0])) != -1) 
		{
			is_crt = 1; 
		}

		if (is_crt)								/* delete CRT file closes		*/
		{
			write_log("WISP",'I',"DELCRTCLOSE","Fixed CLOSE of crt file, %s.",crt_file[cur_crt]);
			if (acn_cobol)
			{
				write_log("WISP",'W',"NATIVE","Workstation CLOSE %s removed for Native Screens",
					  crt_file[cur_crt]);
				tput_scomment("*>>> Workstation CLOSE removed for Native Screens.");
				tput_scomment("*    CLOSE %s\n",crt_file[cur_crt]);
			}
			else
			{
				tput_line_at(16,"CALL \"vwang\" USING VWANG-CLOSE-WS,\n");
			}
		}
		else									/* see if it is a file name		*/
		{
			i = file_index(o_parms[0]);

			if (i != -1)							/* Found the file.			*/
			{
				if (c_count > MAX_FILES)				/* Exceeded file buffer.		*/
				{
					write_log("WISP",'F',"EXCFILECNT","Exceeded FILE count in CLOSE statement.");
					exit_wisp(EXIT_WITH_ERR);
				}
				c_flags[c_count] = prog_ftypes[i];			/* Remember the file type.		*/
				strcpy(c_fnames[c_count++],o_parms[0]);			/* save it				*/

				preclose_locking(i,12);					/* Add DMS locking logic		*/
			}
			else
			{
				done = 1;						/* Not a file name -- next statement	*/
			}
		}

		if (!done) 
		{
			stredt(&linein[7],o_parms[0],"");
		}

		if (ptype == -1) 
		{
			stredt(&linein[7],".","");
			done = 1;
		}

	} while (!done);								/* continue till done			*/

	if (c_count)									/* output the open statements		*/
	{
		tput_line("           MOVE \"CL\" TO WISP-DECLARATIVES-STATUS\n");

		for (i=0; i<c_count; i++)
		{
			tput_line("           CLOSE %s",c_fnames[i]);
			if ((i == c_count-1) && (ptype == -1))				/* handle last file, with a period.	*/
			{
				if (c_flags[i] & PRINTER_FILE)				/* If it is a printer file, call wfclose*/
				{
					tput_line("           CALL \"wfclose\" USING N-%s.\n",c_fnames[i]);
				}
				else
				{
					tput_clause(12, ".");
				}
			}
			else
			{
				if (c_flags[i] & PRINTER_FILE)				/* If it is a printer file, call wfclose*/
				{
					tput_line("           CALL \"wfclose\" USING N-%s\n",c_fnames[i]);
				}
				else
				{
					tput_clause(12, ",");
				}
			}
		}
	}
	else										/* There were no files, must have been	*/
	{										/* a CRT file only. write a place holder*/
		if (ptype == -1)
			tput_line("           CONTINUE.\n");
		else
			tput_line("           CONTINUE,\n");
	}

	hold_line();
	return 0;
}

/*
**	wt_sort.c
*/
p_sort()
{
	char sortbuf[2048];
	char file1[40], file2[40];
	int fnum;

	strcpy(sortbuf,linein);							/* Put the line into the sort buffer.		*/

	ptype = get_param(o_parms[0]);						/* get the SORT verb.				*/
	ptype = get_param(file1);						/* Now the first files name.			*/

	if (ptype == 1) strcat(sortbuf,linein);					/* if a new line, add it on.			*/

	fnum = file_index(file1);

	if (fnum == -1) 
	{
		write_log("WISP",'F',"SORTNOTFND","SORT file %s not found.",file1);
		fnum = prog_cnt;
	}

	s_wfopen(fnum,1);							/* Generate a WFOPEN call.	(output)	*/

	do
	{
		ptype = get_param(o_parms[0]);					/* Next word to parse.				*/
		if (g_newline)
		{
			if (proc_keyword(o_parms[0]))				/* If new line and a keyword.			*/
			{
				hold_line();					/* Hold for the keyword.			*/
				break;						/* Keyword means break.				*/
			}
			else
			{
				strcat(sortbuf,linein);				/* Add to the buffer.				*/
			}
		}

		if (!strcmp(o_parms[0],"USING"))				/* Is it a USING word?				*/
		{
			ptype = get_param(file1);				/* Get the file name.				*/
			if (ptype == 1) strcat(sortbuf,linein);
			fnum = file_index(file1);
			if (fnum == -1) 
			{
				write_log("WISP",'F',"SORTNOTFND","SORT file %s not found.",file1);
				fnum = prog_cnt;
			}
			s_wfopen(fnum,0);					/* Generate a WFOPEN call.	(input)		*/			
		}
		else if (!strcmp(o_parms[0],"GIVING"))				/* Is it a GIVING word?				*/
		{
			ptype = get_param(file2);				/* Get the file name.				*/
			if (ptype == 1) strcat(sortbuf,linein);
			for (fnum=0; fnum<prog_cnt; fnum++)			/* look for the file spec			*/
			{
				if (!strcmp(prog_files[fnum],file2)) break;	/* found it					*/
			}
			if (fnum == prog_cnt) write_log("WISP",'F',"SORTNOTFND","SORT file %s not found.",file2);
			if ( 0 != strcmp(file1,file2) )
			{
				s_wfopen(fnum,1);				/* Generate a WFOPEN call.	(output)	*/
			}
		}
										/* do till we hit a LAST parm or keyword	*/
	} while (ptype != -1);							/* (Keyword test is earlier)			*/


	tput_line("           MOVE \"SO\" TO WISP-DECLARATIVES-STATUS");
	tput_block(sortbuf);
	return 0;
}

int s_wfopen(int fnum, int fmode)
{
	make_fld(o_parms[9],prog_files[fnum],"S-");				/* create the status byte			*/

	tput_line_at(12, "CALL \"wfopen3\" USING");
	tput_clause (16, "%s,",o_parms[9]);					/* start with the status byte			*/

										/* literal or null vol, pass the created field */
	if ((prog_vnames[fnum][0] == '\"') || (!prog_vnames[fnum][0]))
	{
		make_fld(o_parms[9],prog_files[fnum],"V-");
		tput_clause(16, "%s,",o_parms[9]);
	}
	else
	{									/* use the programs own field			*/
		tput_clause(16, "%s,",prog_vnames[fnum]);
	}

										/* literal or null lib, pass the created field 	*/
	if ((prog_lnames[fnum][0] == '\"') || (!prog_lnames[fnum][0]))
	{
		make_fld(o_parms[9],prog_files[fnum],"L-");
		tput_clause(16, "%s,",o_parms[9]);
	}
	else
	{									/* use the programs own field			*/
		tput_clause(16, "%s,",prog_lnames[fnum]);
	}

										/* literal or null name, pass the created field */
	if ((prog_fnames[fnum][0] == '\"') || (!prog_fnames[fnum][0]))
	{
		make_fld(o_parms[9],prog_files[fnum],"F-");
		tput_clause(16, "%s,",o_parms[9]);
	}
	else
	{									/* use the programs own field			*/
		tput_clause(16, "%s,",prog_fnames[fnum]);
	}

										/* always pass the created name			*/
	make_fld(o_parms[9],prog_files[fnum],"N-");
	tput_clause(16, "%s,",o_parms[9]);

	tput_clause(16, "WISP-APPLICATION-NAME,");

	make_fld(o_parms[9],prog_files[fnum],"PR-");
	tput_clause(16, "%s,",o_parms[9]);		/* And the prname.				*/
	tput_clause(16, "%s", (fmode==1) ? "WFOPEN-SORT" : "WFOPEN-INPUT");
	return 0;
}

/*
**	wt_read.c
*/

#define READ_FILE "$WRXXXXXX"

int	unterminated_if = 0;

static char rd_str[STRING_BUFFER_SIZE];							/* Buffer for READ statements.		*/
static char tm_str[STRING_BUFFER_SIZE];							/* Buffer for TIMEOUT statements.	*/

p_read()
{
	int i,j,hold,fnum,invkey_clause,atend_clause,t_out,t_flag,is_crt,did_lockclause,indexed_file;
	char  	*crt_type;
	char  	tstr[132],into_rec[40];
	int	col, savecol, maincol, addnl, savenl;
	static  int readnum = 0;							/* current paragraph number for READ's	*/

	char hstr[40];									/* Holder-ID string.			*/
	char timstr[40];								/* TIMEOUT value string.		*/

	static FILE	*read_temp_file;

	write_log("WISP",'I',"PROCREAD","Processing Read Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be READ)	*/
	col   = 1 + get_ppos();								/* Find out its column location.	*/
	maincol = col;
	j = peek_param(o_parms[1]);							/* peek at the file name		*/

	cur_crt = crt_index(o_parms[1]);
	is_crt = (-1 != cur_crt);

	if (is_crt)									/* is it a crt record?			*/
	{
		if (acn_cobol)
		{
			write_log("WISP",'W',"NATIVE","Workstation READ %s uses WISP Screens",crt_file[cur_crt]);
		}
		
		write_log("WISP",'I',"FIXREAD","READ of %s repaired.",crt_file[cur_crt]); /* yes, fix it			*/
		ptype = get_param(o_parms[1]);						/* Really get it now.			*/

		crt_type = "VWANG-READ-ALL";						/* Set type of read to READ-ALL.	*/
		into_rec[0] = 0;

		stredt(linein,"READ","");
		stredt(linein,o_parms[1],"");

		j = ptype;

		if (ptype != -1)							/* No period means could have INTO.	*/
		{
			ptype = get_param(o_parms[0]);					/* check for INTO.			*/

			if (!strcmp(o_parms[0],"RECORD"))				/* Found a RECORD keyword, remove it.	*/
			{
				j = ptype;						/* Remember punctuation.		*/
				ptype = get_param(o_parms[0]);
			}

			if (!strcmp(o_parms[0],"MODIFIABLE"))				/* Found a MODIFIABLE keyword, remove it.*/
			{
				crt_type = "VWANG-READ-MODIFIABLE";			/* Set type string.			*/
				j = ptype;						/* Remember punctuation.		*/
				ptype = get_param(o_parms[0]);
			}

			if (!strcmp(o_parms[0],"ALTERED"))				/* Found a ALTERED keyword, remove it.	*/
			{
				crt_type = "VWANG-READ-ALTERED";
				j = ptype;						/* Remember punctuation.		*/
				ptype = get_param(o_parms[0]);
			}

			if (!strcmp(o_parms[0],"INTO"))					/* Is it?				*/
			{
				ptype = get_param(into_rec);				/* Get the field name.			*/
				j = ptype;						/* Remember the item type.		*/
			}
			else hold_line();						/* Otherwise hold on to the line.	*/
		}

					/*123456789012345678901234567890123456789012345678901234567890123456789012*/
		tput_blank();

		if (crt_relative[cur_crt][0])						/* Load relative key into order-area	*/
		{
			tput_line_at	(12, "CALL \"xx2byte\" USING %s,",crt_relative[cur_crt]);
			tput_clause  	(16, "%s",crt_record[crt_prime_rec[cur_crt]]);
		}
											/* Copy the Order-Area for the read	*/
		tput_line_at		(12, "MOVE %s TO WISP-CRT-ORDER-AREA\n",crt_record[crt_prime_rec[cur_crt]]);
											/* Load number of lines			*/
		tput_line_at		(12, "MOVE WISP-SYMB-%d TO VWANG-LINES,\n",
									(crt_record_size[crt_prime_rec[cur_crt]]-4)/80);
		tput_line_at		(12, "MOVE SPACES TO %s,\n",crt_status[cur_crt]);/* Must clear status field	*/
		tput_line_at  		(12, "MOVE \"A\" TO WISP-ALLOWABLE-PF-KEYS\n");
		tput_line_at		(12, "CALL \"vwang\" USING %s,",crt_type);
		tput_clause		(16, "WISP-CRT-RECORD,");
		tput_clause		(16, "VWANG-LINES,");
		tput_clause		(16, "WISP-ALLOWABLE-PF-KEYS,");
		tput_clause		(16, "%s,",crt_pfkey[cur_crt]);
		tput_clause		(16, "%s",crt_status[cur_crt]);
		tput_line_at		(12, "MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[cur_crt]]);

		if (into_rec[0])							/* Had an INTO phrase.			*/
		{
			tput_line_at	(12, "MOVE %s TO",crt_record[crt_prime_rec[cur_crt]]);
			tput_clause	(16, "%s",into_rec);
		}

		if (crt_cursor[cur_crt][0])						/* if there is a cursor			*/
		{
			tput_line_at  	(12, "CALL \"w2rowcol\" USING WISP-CRT-ORDER-AREA-3,");
			tput_clause	(16, "%s",crt_cursor[cur_crt]);
		}

		if (j == -1)								/* has a period in it...		*/
		{
			tput_clause(12,".");
		}

		tput_flush();
		return(0);								/* exit the FOR loop			*/
	}
	else /* Not a CRT read */
	{
		hstr[0] = '\0';
		strcpy(timstr,"9999");							/* Default timeout.			*/

		i = 0;

		fnum = file_index(o_parms[1]);

		if (fnum == -1)								/* no file matched, error		*/
		{
			write_log("WISP",'F',"READFNF",
			"Error -- File %s, referenced by READ statement but not Declared.",o_parms[1]);
			exit_wisp(EXIT_WITH_ERR);
		}

		write_log("WISP",'I',"HANDLEREAD","Handling READ of %s.",prog_files[fnum]);

		strcpy(tm_str,"");							/* Need to init timeout string.		*/

		strcpy(rd_str,"");							/* start a new line			*/

		if (!read_file_ptr)							/* if there has not yet been a file made*/
		{
			read_file_ptr = open_cob_file(read_fname,FOR_OUTPUT,1);		/* open for write			*/
			if (!read_file_ptr) exit_with_err();
			read_temp_file = read_file_ptr->a_file->file;
			fprintf(read_temp_file,"***********  READ STATEMENTS\n");
			read_file_ptr->line_count++;
											/* put a line in it			*/
		}

		hold = 0;								/* no hold yet...			*/
		invkey_clause = 0;							/* no INVALID KEY yet...		*/
		atend_clause = 0;							/* no AT END yet...			*/
		t_out = 0;								/* No TIMEOUT phrase...			*/
		t_flag = 0;								/* Global flag for timeout phrase.	*/
		did_lockclause = 0;							/* Wrote the locking clause		*/
		addnl = 0;


		if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))			/* If using automatic record locking	*/
		{									/* then don't supply an "ALLOWING" 	*/
			did_lockclause = 1;						/* clause. (Trick it into not writing	*/
		}									/* the ALLOWING clause.)		*/

		indexed_file = (prog_ftypes[fnum] & INDEXED_FILE);

		do
		{
			if (t_out)							/* If timeout found.			*/
			{
				add2buff(tm_str,o_parms[0],col,addnl);
			}
			else 								/* Put it in temp string		*/
			{
				add2buff(rd_str,o_parms[0],col+4,addnl);
			}

			if (o_parms[0][0]) stredt(linein,o_parms[0],"");		/* Remove it from the input line	*/
			ptype = get_param(o_parms[0]);					/* get a new parm....			*/
			adjcol(ptype,&col,&addnl);

			if (ptype == 1)							/* first parm on a new line		*/
			{
				if (has_cont)						/* Force in the continuation		*/
				{
					strcpy(tstr,"\n      -");

					if (t_out)
					{
						strcat(tm_str,tstr);			/* start a new line			*/
					}
					else
					{
						strcat(rd_str,tstr);			/* start a new line			*/
					}
					addnl = 0;
				}
				else
				{
					addnl = 1;
				}
			}

			savenl = addnl;
			savecol = col;

			if (!strcmp(o_parms[0],"WITH") || !strcmp(o_parms[0],"HOLD"))
			{
				stredt(linein,o_parms[0],"");
				o_parms[0][0] = 0;					/* clear it				*/
				write_log("WISP",'I',"FIXWITHHOLD","Corrected WITH HOLD in READ.");
				hold = 1;						/* flag it				*/
			}
			else if (t_out && !strcmp(o_parms[0],"NEXT"))			/* Handle NEXT SENTENCE in timeout	*/
			{
				if (o_parms[0][0]) stredt(linein,o_parms[0],"");	/* Remove it from the input line	*/
				ptype = get_param(o_parms[0]);				/* get a new parm....			*/
				adjcol(ptype,&col,&addnl);
				if ( !strcmp(o_parms[0],"SENTENCE") )
				{
					o_parms[0][0] = '\0';
					add2buff(tm_str,"CONTINUE",col,addnl);
				}
				else
				{
					add2buff(tm_str,"NEXT",col,addnl);
					add2buff(tm_str,o_parms[0],col,0);
					o_parms[0][0] = '\0';
					write_log("WISP",'E',"READ","Unable to parse keyword NEXT in READ.");
				}
				
			}
			else if (!strcmp(o_parms[0],"NEXT"))				/* handle NEXT special because		*/
			{								/* it is in the procedure division	*/
											/* keyword list and will exit the while.*/
				stredt(o_parms[0],"N"," N");				/* Just put a space in, it won't match	*/
											/* the keyword list any more!		*/
			}
			else if (!strcmp(o_parms[0],"INTO"))				/* handle INTO special because		*/
			{								/* it signals a NO LOCK for LPI		*/
				if (lpi_cobol || acu_cobol)
				{							/* NO LOCK must precede INTO.		*/
					addlockclause(&did_lockclause,indexed_file,hold,rd_str,col+8);
				}
			}
			else if (!strcmp(o_parms[0],"INVALID"))				/* INVALID KEY phrase?			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,savecol+8);

				stredt(linein," INVALID"," ");				/* remove INVALID			*/
				if (ptype != -1)
				{
					peek_param(o_parms[7]);				/* get "KEY"				*/
					if (!strcmp(o_parms[7],"KEY"))			/* Was it KEY?				*/
					{
						ptype = get_param(o_parms[0]);		/* Get it, and remove it.		*/
						adjcol(ptype,&col,&addnl);
						if (addnl) { savenl = 1; savecol = col; }
						stredt(linein," KEY"," ");		/* remove KEY				*/
					}
				}

				if (ptype == -1)					/* Premature period!			*/
				{
					write_log("WISP",'I',"BADINVKEY",
					"Bad READ syntax, INVALID KEY followed by a period.");
					o_parms[0][0] = '\0';
				}
				else
				{
					ptype = get_param(o_parms[0]);			/* what to do?				*/
					adjcol(ptype,&col,&addnl);
				}

				add2buff(rd_str,"INVALID KEY",savecol+4,savenl);	/* write it out				*/

				if (hold) add2buff(rd_str,"MOVE \"Y\" TO WISP-TEST-BYTE.",maincol+8,1);

				t_out = 0;						/* Clear any TIMEOUT flag.		*/
				invkey_clause = 1;					/* flag it				*/
			}
			else if (!strcmp(o_parms[0],"AT") || !strcmp(o_parms[0],"END")) /* AT END phrase?			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,savecol+8);

				if (o_parms[0][0] == 'A')
				{
					stredt(linein," AT "," ");			/* remove AT				*/
					ptype = get_param(o_parms[0]);			/* get "END"				*/
					adjcol(ptype,&col,&addnl);
					if (addnl) { savenl = 1; savecol = col; }
				}
				stredt(linein," END"," ");				/* remove END				*/
				if (ptype == -1)					/* Prematur period.			*/
				{
					write_log("WISP",'I',"BADATEND",
					"Bad READ syntax, AT END followed by a period.");
					o_parms[0][0] = '\0';
				}
				else
				{
					ptype = get_param(o_parms[0]);			/* what to do?				*/
					adjcol(ptype,&col,&addnl);
				}

				add2buff(rd_str,"AT END",savecol+4,savenl);		/* write it out				*/

				if (hold) add2buff(rd_str,"MOVE \"Y\" TO WISP-TEST-BYTE.",maincol+8,1);

				t_out = 0;						/* Clear any TIMEOUT flag.		*/
				atend_clause = 1;					/* flag it				*/
			}
			else if (!strcmp(o_parms[0],"TIMEOUT"))				/* TIMEOUT phrase?			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,savecol+8);

				write_log("WISP",'I',"READWTIME","READ with TIMEOUT modified.");
				stredt(linein," TIMEOUT"," ");				/* remove TIMEOUT			*/
				ptype = get_param(timstr);				/* Get the timeout value.		*/
				adjcol(ptype,&col,&addnl);
				if (!strcmp(timstr,"OF"))
				{							/* Skip over OF keyword.		*/
					stredt(linein,"OF","");
					ptype = get_param(timstr);
					adjcol(ptype,&col,&addnl);
				}

				t_out = 1;						/* Flag it.				*/
				t_flag = 1;

				o_parms[0][0] = '\0';

			}
			else if (!strcmp(o_parms[0],"HOLDER-ID"))			/* HOLDER-ID phrase?			*/
			{
				stredt(linein," HOLDER-ID"," ");			/* remove HOLDER-ID			*/
				ptype = get_param(hstr);				/* Get the holder field.		*/
				if (!addnl) adjcol(ptype,&col,&addnl);
				if (!strcmp(hstr,"IN"))
				{							/* Skip over IN keyword.		*/
					stredt(linein,"IN","");
					ptype = get_param(hstr);
					if (!addnl) adjcol(ptype,&col,&addnl);
				}
				o_parms[0][0] = '\0';
			}
			else if (!strcmp(o_parms[0],"SECOND") || !strcmp(o_parms[0],"SECONDS"))
			{
				o_parms[0][0] = '\0';					/* Just remove seconds.			*/
			}
			else if (!strcmp(o_parms[0],"KEY"))				/* Handle KEY phrase.			*/
			{
				addlockclause(&did_lockclause,indexed_file,hold,rd_str,col+8);

				stredt(linein,o_parms[0],"");				/* Remove "KEY" from line		*/
				ptype = get_param(o_parms[0]);
				adjcol(ptype,&col,&addnl);
				if (!strcmp(o_parms[0],"IS"))
				{
					if (addnl) { savenl = 1; savecol = col; }
					stredt(linein,o_parms[0],"");			/* Remove "IS" from line		*/
					ptype = get_param(o_parms[0]);
					adjcol(ptype,&col,&addnl);
				}

				add2buff(rd_str,"KEY IS",savecol+4,savenl);

				key_name(o_parms[0],0);					/* Key name conversion			*/

			}
		} while ((ptype != -1) && (!proc_keyword(o_parms[0]) || t_out));
											/* do till we hit a LAST parm or keyword*/

		if (ptype == -1)							/* a LAST parm				*/
		{
			strcpy(tstr,o_parms[0]);
		}
		else
		{
			*tstr = 0;							/* Fake the last parameter		*/
		}

		if (t_out)								/* If there is a timeout, (also a hold)	*/
		{
			add2buff(tm_str,tstr,col,addnl);
		}
		else
		{	
			add2buff(rd_str,tstr,col+4,addnl);				/* Put out the last parameter		*/

			addlockclause(&did_lockclause,indexed_file,hold,rd_str,col+8);

		}

		if (hold)								/* If going to hold the record,		*/
		{									/* Wait till no soft/hard lock		*/
			sprintf(tstr, "CALL \"wfwait\" USING %s",prog_fstats[fnum]);
			add2buff(rd_str,tstr,maincol+4,1);
			add2buff(rd_str,"WISP-FILE-TIMEOUT",maincol+8,0);
		}

		strcat(rd_str,"\n");							/* finish this line			*/
		strcat(tm_str,"\n");							/* finish this line			*/

										 	/* Now write everything out to the file.*/

		tput_line          ("           MOVE \"RD\" TO WISP-DECLARATIVES-STATUS\n");

		if ( hold && (invkey_clause || atend_clause) )
		{
			tput_line  ("           MOVE \"N\" TO WISP-TEST-BYTE\n"); 	/* Set up the INVALID KEY flag.		*/
		}

		if (t_flag)
		{
			/* Compute the TIMEOUT value.		*/
			tput_line_at(12,	"COMPUTE WISP-FILE-TIMEOUT = 100 * ");
			tput_clause (16, 	    "%s", timstr);
		}
		else if (hold)
		{
			tput_line("           MOVE 0 TO WISP-FILE-TIMEOUT\n"); 	/* Store the TIMEOUT value.		*/
		}




		if (hold)			/* Insert all the special logic to handle WITH HOLD			*/
						/*    - if INVALID KEY or AT END then put READ into separate para	*/
						/*    - keep trying to read until you get the record			*/
						/*    - handle TIMEOUT logic						*/
						/*    - add UNLOCK logic						*/

		{								/* Gonna do a hold, free last file.	*/
			set_lock(fnum,12);					/* Set new lock.			*/

			if (invkey_clause || atend_clause)			/* If HOLD and INVALID KEY or AT END use*/
			{							/* Paragraph performs.			*/
				if (copylib && writing_copybook())		/* Are we in a copy lib?		*/
				{
					tput_line("           PERFORM WISP-READ-%s-%d",cpy_file,cpy_seq);
					if (!re_copy)				/* Is this a second time?		*/
					{
						fprintf(read_temp_file,"\n       WISP-READ-%s-%d.\n",cpy_file,cpy_seq);
						read_file_ptr->line_count += 2;
						fprintf(read_temp_file,"%s",rd_str); /* Now write out the whole thing.	*/
						read_file_ptr->line_count += strchrcnt(rd_str,'\n');
						fprintf(read_temp_file,"           CONTINUE.\n");
						read_file_ptr->line_count++;
					}
					cpy_seq++;
				}
				else
				{
					tput_line("           PERFORM WISP-READ-NUMBER-%d",readnum);
										/* write the paragraph name to the file	*/
					fprintf(read_temp_file,"\n       WISP-READ-NUMBER-%d.\n",readnum);
					read_file_ptr->line_count += 2;
					fprintf(read_temp_file,"%s",rd_str);	/* Now write out the whole thing.	*/
					read_file_ptr->line_count += strchrcnt(rd_str,'\n');
					fprintf(read_temp_file,"           CONTINUE.\n");
					read_file_ptr->line_count++;
					readnum++;				/* increment the counter		*/
				}
			}
			else							/* No INVALID KEY, use linein PERFORM.	*/
			{
				tput_line  	("           PERFORM WITH");
			}

			tput_clause		(16, "TEST AFTER");			/* Wait for hard locks.			*/

			tput_line		("                   UNTIL (%s IS NOT = \"%s\")\n",prog_fstats[fnum],hard_lock);

			if (t_flag) tput_line  	("                   OR (WISP-FILE-TIMEOUT IS < 1)\n");

			if (!(invkey_clause || atend_clause))
			{
				tput_block(rd_str);					/* Now write out the whole thing.	*/
				tput_line	("           END-PERFORM\n");
			}
		}
		else /* no WITH HOLD */
		{
			tput_block(rd_str);						/* Now write out the whole thing.	*/
		}

		if (t_flag)
		{
			tput_line		("           IF ( %s = \"%s\" AND\n",prog_fstats[fnum],hard_lock);
			tput_clause		(16,"WISP-FILE-TIMEOUT < 1)");
			clear_locking();						/* Add locking logic			*/

			tput_block(tm_str);
			tput_flush();
		}

		if (ptype == -1)
		{
			if (t_flag)
			{
				tput_line  	("           END-IF.\n");
			}
			else
				tput_line	("           CONTINUE.\n");
		}
		else
		{                                                                            
			if (t_flag)
			{
				tput_line  	("           ELSE\n");
			}

			if ( hold && (invkey_clause || atend_clause) ) 
			{
				tput_line  	("           IF WISP-TEST-BYTE = \"Y\" THEN\n");
				clear_locking();					/* Add locking logic			*/
				unterminated_if = 1;					/* Weve added an IF statement		*/
			}
		}


		if (ptype != -1) hold_line();

		if (!vax_cobol && hold && did_lockclause)
		{
			write_log("WISP",'F',"READERR","NO LOCK inserted when HOLD was specified, Keywords out of order.");
		}
		write_log("WISP",'I',"READDONE","Completed READ analysys");
	}

}
static adjcol(int ptype, int* col, int* addnl)
{
	if (ptype == 1)
	{
		*col = 1 + get_ppos();							/* Find out its column location.	*/
		*addnl = 1;
	}
	else
	{
		*addnl = 0;
	}
	return 0;
}

static void addlockclause(int* did_lockclause, int indexed_file, int hold, char* buff, int col)
{
	if ( *did_lockclause )
	{
		return;
	}

	if ( vax_cobol )
	{
		if ( hold ) add2buff(buff,"ALLOWING NO OTHERS",col,0);
		else	    add2buff(buff,"REGARDLESS OF LOCK",col,0);
		*did_lockclause = 1;
	}
	else
	{
		if (!hold)
		{
			if (mf_cobol && indexed_file)
			{
				add2buff(buff,"IGNORE LOCK",col,0);
				*did_lockclause = 1;
			}
			else
			{
				add2buff(buff,"NO LOCK",col,0);
				*did_lockclause = 1;
			}
		}
	}
}

/*
**	tokenize.c
*/
int hold_token_cache(void)
{
	int	i;
	int	count;
	TOKEN	tmptok;
	char	the_line[128];

	count = token_cache_count();
	if (count)
	{
		memset(the_line,' ',80);
		for(i=0; i<count; i++)
		{
			token_cache_unque(&tmptok);
			if (tmptok.indata)
				memcpy(&the_line[tmptok.column-1],tmptok.indata,strlen(tmptok.indata));
			else
				memcpy(&the_line[tmptok.column-1],tmptok.data,strlen(tmptok.data));

			if (i == count-1)
			{
				the_line[tmptok.column+strlen(tmptok.data)-1] = (char)0;
				the_line[80] = (char)0;
				strcat(the_line,"\n");
			}
			clean_token(&tmptok);
		}
	}
	else
	{
		the_line[0] = (char)0;
	}
	
	hold_this_line(the_line);
/*	invalidate_parms(); */
	return 0;
}

/*
**	wt_utils.c
*/

#define HT '\t'										/* Define horizontal tab.		*/
#define SP ' '										/* Define space character.		*/
#ifdef NULL
#undef NULL
#endif
#define NULL '\0'

int trim(string) char string[];								/* Trim a string of trailing blanks.	*/

{
    register int i;									/* Working register storage.		*/

    for (i = strlen(string)-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == NULL)); i--) string[i] = NULL;

    return(i+1);									/* Return the string length.		*/
}
 
int strlast(char *string, char srch)							/* determine if the srch char is the	*/
											/* last non-whitespace			*/
{
	register int i;									/* Working register storage.		*/
	for 	(i = strlen(string)-1;
		(i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == NULL) || (string[i] == '\n'));
		(i--));
	if (string[i] == srch)	return(1);						/* it is the last char			*/
	else			return(0);						/* not found				*/
}

/*				Squeeze a line until it is equal to the_length							*/
int wsqueeze(the_line,length)
char *the_line;
int  length;
{
	int i,j;

	i = strlen(the_line);							/* First examine the line.			*/

	if (the_line[i-1] == '\n') i--;						/* Ignore the newline.				*/

	if (i <= length) return(0);						/* no need to squeeze the line			*/

	do
	{
		j = stredt(&the_line[12],"  "," ");				/* change 2 spaces to 1 space after col 12	*/
	} while ((j != -1) && (--i > length));					/* till no more or length is ok			*/

	if (i > length) return(-1);						/* Couldn't do it.				*/
	else return(1);								/* say we did it				*/
}

/*
**	wt_crtrw.c
*/

/*						Process REWRITE CRT-FILE statements						*/
#define PROC_ALARM	0
#define	PROC_SETTING	6
#define	PROC_CURSOR	14
#define	PROC_COLUMN	21
#define	PROC_ROW	28
#define	PROC_LINE	32
#define PROC_ROLL	37
#define	PROC_ERASE	42
#define PROC_FROM	48
#define PROC_AFTER	53

crt_rewrite(crt_num)
int crt_num;										/* the crt-file number being rewritten	*/
{
	int kword,j;
	int wcc_byte;
	char from_item[40],row_field[40],col_field[40];

	row_field[0] = 0;
	col_field[0] = 0;
	kword = 0;
	wcc_byte = 0;
	from_item[0] = '\0';

	write_log("WISP",'I',"REWRITECRT","Rewrite of a crt file record.");
	if (ptype != -1)								/* could have parameters		*/
	{
		do									/* scan for them...			*/
		{
			if (ptype != -1)						/* not the end yet			*/
			{
				ptype = get_param(templine);				/* get a new parm			*/

	 	                        /*  0123456789012345678901234567890123456789012345678901234567890			*/
				kword = strpos("ALARM SETTING CURSOR COLUMN ROW LINE ROLL ERASE FROM AFTER",templine);

				if (kword != -1) stredt(linein,templine,"");		/* Remove it from the input.		*/

				switch (kword)						/* do the appropriate thing		*/
				{
					case PROC_ALARM:				/* process ALARM			*/
					{
						wcc_byte |= 0x0C0;			/* Set ALARM bit to WCC.		*/
						write_log("WISP",'I',"ALARM","ALARM found.");
						break;
					}

					case PROC_AFTER:
					{
						break;
					}

					case PROC_SETTING:
					{
						break;
					}

					case PROC_CURSOR:
					{
						wcc_byte |= 0x0A0;			/* Set CURSOR-POS bit.			*/
						break;
					}

					case PROC_COLUMN:				/* Process "COLUMN"			*/
					{
						ptype = get_param(col_field);		/* Actually read in the COLUMN value	*/
						stredt(linein,col_field,"");		/* Remove it.				*/
						stredt(col_field,".","");
						write_log("WISP",'I',"COLUMN","COLUMN found.");
						break;
					}

					case PROC_FROM:					/* Process "FROM"			*/
					{
						ptype = get_param(from_item);		/* Actually read in the FROM item	*/
						stredt(linein,templine,"");		/* Remove it.				*/

						write_log("WISP",'I',"FROMITM","FROM item found, %s.",from_item);
						break;
					}

					case PROC_ROW:					/* Process "ROW"			*/
					case PROC_LINE:					/* or "LINE"				*/
					{
						ptype = get_param(row_field);		/* Actually read in a ROW value		*/
						stredt(linein,row_field,"");		/* Remove it.				*/
						stredt(row_field,".","");		/* Fixup possible period.		*/
						write_log("WISP",'I',"ROW","ROW found.");
						break;
					}

					case PROC_ROLL:					/* Process "ROLL"			*/
					{
						ptype = get_param(templine);		/* Actually read in the UP/DOWN value	*/
						stredt(linein,templine,"");		/* Remove it.				*/
						write_log("WISP",'I',"ROLL","ROLL found.");
						break;
					}

					case PROC_ERASE:				/* Process "ERASE"			*/
					{
						ptype = get_param(templine);		/* Actually read in PROTECT/MODIFY	*/
						stredt(linein,templine,"");		/* Remove it.				*/
						write_log("WISP",'I',"ERASE","ERASE found.");
						break;
					}
				}
			}								/* end of if ptype == -1		*/
		} while ((kword != -1) && (ptype != -1));				/* quit when no matches			*/
	}


	write_log("WISP",'I',"CRTREWRITE","REWRITE of %s repaired.",crt_record[crt_num]); 	/* now fix it			*/

	if (acn_cobol)
	{
		write_log("WISP",'W',"NATIVE","Workstation REWRITE %s uses WISP Screens",crt_record[crt_num]);
	}

											/* Go till we go beyond it.		*/
	for ( cur_crt=0; cur_crt<crt_fcount; cur_crt++) 
	{
		if (crt_prime_rec[cur_crt] > crt_num) 
		{
			cur_crt--; 
			break;
		}
	}

	if (cur_crt == crt_fcount) cur_crt--;

	if (from_item[0])								/* Handle the REWRITE FROM phrase.	*/
	{
		tput_line_at(12, "MOVE %s TO",from_item);
		tput_clause (16, "%s,",crt_record[crt_num]);
	}

	if (crt_relative[cur_crt][0])
	{										/* write what line to start at		*/
		tput_line_at(12, "CALL \"xx2byte\" USING %s,",crt_relative[cur_crt]);
		tput_clause (16, "%s",crt_record[crt_num]);
	}

	tput_line_at(12, "MOVE %s TO WISP-CRT-RECORD,",crt_record[crt_num]);

	if (wcc_byte)									/* Do a special WCC function?		*/
	{
		tput_line_at(12, "MOVE WISP-SYMB-%d TO WISP-CRT-ORDER-AREA-2,",wcc_byte);
	}

	if (row_field[0])								/* Set the row byte			*/
	{
		if (isdigit(row_field[0]))						/* If it's numeric, move the DEC-BYTE.	*/
		{
			j = row_field[0] - '0';
			if (row_field[1]) j = (j * 10) + (row_field[1] - '0');		/* Scan out the digits.			*/
			tput_line_at(12, "MOVE WISP-SYMB-%d TO WISP-CRT-ORDER-AREA-4",j);
		}
		else									/* Must be a field name.		*/
		{
			tput_line_at(12, "MOVE %s TO WISP-DFIELD-0",row_field);
			tput_line_at(12, "CALL \"xx2byte\" USING WISP-DFIELD-0, WISP-CRT-ORDER-AREA-4");
		}
	}

	if (col_field[0])								/* Set the col byte.			*/
	{

		if (isdigit(col_field[0]))						/* If it's numeric, move the DEC-BYTE.	*/
		{
			j = col_field[0] - '0';
			if (col_field[1]) j = (j * 10) + (col_field[1] - '0');		/* Scan out the digits.			*/
			tput_line_at(12, "MOVE WISP-SYMB-%d TO WISP-CRT-ORDER-AREA-3",j);
		}
		else									/* Must be a field name.		*/
		{
			tput_line_at(12, "MOVE %s TO WISP-DFIELD-0",col_field);
			tput_line_at(12, "CALL \"xx2byte\" USING WISP-DFIELD-0, WISP-CRT-ORDER-AREA-3");
		}
	}

	if (wcc_byte || row_field[0] || col_field[0])
	{
		tput_line_at	(12, "MOVE 4 TO WISP-LONGWORD");
		tput_line_at	(12, "CALL \"wmemcpy\" USING %s,",crt_record[crt_num]);
		tput_clause	(16, "WISP-CRT-O-A, WISP-LONGWORD");
	}

	tput_line_at(12, "MOVE WISP-SYMB-%d TO VWANG-LINES,\n",(crt_record_size[crt_num]-4)/80);
	tput_line_at(12, "MOVE SPACES TO %s,\n",crt_status[cur_crt]);
	tput_line_at(12, "MOVE \"X\" TO WISP-ALLOWABLE-PF-KEYS,\n");
	tput_line_at(12, "CALL \"vwang\" USING VWANG-WRITE-ALL,");
	tput_clause (16, "WISP-CRT-RECORD,");
	tput_clause (16, "VWANG-LINES,");				/* Write full screen for now	*/
	tput_clause (16, "WISP-ALLOWABLE-PF-KEYS,");
	tput_clause (16, "%s,",crt_pfkey[cur_crt]);
	tput_clause (16, "%s",crt_status[cur_crt]);


	if (ptype == -1)									/* has a period in it...	*/
	{
		tput_clause(12, ".");
	}

	if ((ptype == 1) || (kword == -1)) hold_line();						/* Ended on a new line		*/
	return 0;
}

/*
**	wt_free.c
*/

p_free()
{
	int is_comit;

	ptype = get_param(o_parms[0]);							/* get next parameter (should be FREE)	*/

	write_log("WISP",'I',"PROCFRELOCK","Processing %s Statement.",o_parms[0]);

	if (!strcmp(o_parms[0],"FREE"))							/* Look for FREE.			*/
	{
		is_comit = 0;								/* Not a COMMIT statement.		*/
		ptype = get_param(o_parms[0]);						/* get next parameter (should be ALL)	*/
	}
	else
		is_comit = 1;								/* Assume it's a COMMIT statement.	*/

	set_lock(-1,12);

	tput_line("           CONTINUE");

	if (ptype == -1)								/* Had a period, all done!		*/
	{
		tput_clause(12, ".");
		return(0);
	}

	if (is_comit)
	{
		stredt(linein," COMMIT"," ");						/* Remove COMMIT.			*/
	}
	else
	{
		stredt(linein," FREE "," ");						/* Remove FREE.				*/
		stredt(linein," ALL"," ");						/* Remove ALL.				*/
	}

	ptype = get_param(o_parms[0]);							/* Looking for a possible ON ERROR.	*/

	if (!strcmp(o_parms[0],"ON"))							/* See if it's an ON keyword.		*/
	{
		write_log("WISP",'I',"ONERR","ON ERROR phrase found.");
		ptype = get_param(o_parms[0]);						/* Get ERROR phrase.			*/
		tput_line("                   MOVE \"000\" TO WISPRETURNCODE\n");
		tput_line("                   IF WISPRETURNCODE = \"000\" THEN\n");
		tput_line("                       CONTINUE\n");
		tput_line("                   ELSE\n");
		stredt(linein," ON "," ");						/* Remove ON				*/
		stredt(linein," ERROR"," ");						/* Remove ERROR				*/
	}

	hold_line();									/* Hold line, then quit.		*/
}

/*
**	wt_write.c
*/

p_write()
{
	int 	i,j,fnum,inv_key;
	int	lock_clause;
	char	recname[40];
	int	fd;
	char	fdname[40];

	write_log("WISP",'I',"PROCWRITE","Processing WRITE Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be WRITE)	*/
	j = peek_param(o_parms[1]);							/* peek at the record name		*/

	strcpy(recname, o_parms[1]);
	if (strchr(recname,'.')) *((char *)strchr(recname,'.')) = 0;			/* save the record name			*/

	fd = fd_record( recname );
	if (fd == -1)
	{
		write_log("WISP",'E',"WRITE","WRITE Unknown Record Name");
		strcpy(fdname,"????");
		fd=0;
	}
	else
	{
		strcpy(fdname, prog_files[fd]);
	}
	
	i = 0;
	o_parms[5][0] = '\0';								/* no status field yet			*/

	fnum = -1;									/* set fnum to error status		*/

	inv_key = 0;									/* no INVALID KEY yet...		*/
	lock_clause = 0;								/* no ALLOWING clause yet		*/

	if (vax_cobol && (prog_ftypes[fd] & AUTOLOCK))					/* If using automatic record locking	*/
	{										/* then don't supply an "ALLOWING" 	*/
		lock_clause = 1;							/* clause. (Trick it into not writing	*/
	}										/* the ALLOWING clause.)		*/

	tput_line("           MOVE \"WR\" TO WISP-DECLARATIVES-STATUS\n");

	if (vax_cobol && !(prog_ftypes[fd] & SEQ_FILE))
	{
		tput_line("           MOVE \"N\" TO WISP-TEST-BYTE\n");
	}

	tput_flush();

	do
	{
		tput_clause(12, "%s",o_parms[0]);					/* output parm				*/

		if (o_parms[0][0]) stredt(linein,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			tput_flush();
		}

		if (!strcmp(o_parms[0],"BEFORE") || !strcmp(o_parms[0],"AFTER"))	/* Look for BEFORE or AFTER ADVANCING.	*/
		{
			strcpy(o_parms[9],o_parms[0]);
			stredt(linein,o_parms[9]," ");					/* remove the word			*/
			ptype = get_param(o_parms[0]);					/* get "ADVANCING"			*/

			if (!strcmp(o_parms[0],"ADVANCING"))				/* Was it?				*/
			{
				stredt(linein," ADVANCING","");				/* remove it.				*/
				ptype = get_param(o_parms[0]);				/* Get it.				*/
			}

			if ( vax_cobol && !lock_clause )
			{
				tput_line("               ALLOWING NO OTHERS");
				lock_clause = 1;
			}

			if (fig_count)							/* There is a list of 2 byte fig cons.	*/
			{
				i = 0;
				do
				{
					if (!strcmp(o_parms[0],fig_cons[i]))		/* Is it one of them?			*/
					{
						if (fig_val[i][0] & 0x40)
							strcpy(o_parms[9],"BEFORE");	/* Check before/after bit.		*/
						else
							strcpy(o_parms[9],"AFTER");
						sprintf(o_parms[0],"%d",fig_val[i][1] & 0x7f);	/* Number of lines to skip	*/
						break;
					}
				} while(++i < fig_count);
			}

											/* write it out				*/
			tput_line("                    %s ADVANCING",o_parms[9]);

		}
		else if (!strcmp(o_parms[0],"INVALID"))					/* INVALID KEY phrase?			*/
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

			if ( vax_cobol && !lock_clause )
			{
				tput_line("               ALLOWING NO OTHERS");
				lock_clause = 1;
			}

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'I',"BADINVKEY",
					"Bad WRITE syntax, INVALID KEY followed by a period.");
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
				tput_line("           END-WRITE");
			}
			else
			{
				if (!strcmp(o_parms[0],"ELSE"))				/* INVALID KEY followed by ELSE		*/
				{
					tput_line("               CONTINUE ");
					tput_line("           END-WRITE ");
				}
			}
			inv_key = 1;							/* flag it				*/
		}
		else if (!strcmp(o_parms[0],"TIMEOUT"))
		{
			write_log("WISP",'E',"TIMEOUT",	"Write %s with TIMEOUT not supported.",o_parms[1]);

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

		if (inv_key)
		{
			tput_line("           IF WISP-TEST-BYTE = \"N\" THEN\n");
			tput_line("               MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fd]);
			tput_line("               UNLOCK %s\n", fdname );
			tput_line("               MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fd]);
			tput_line("           ELSE\n");
			if (!strcmp(o_parms[0],"ELSE"))
			{
				tput_line("               CONTINUE\n");
				tput_line("           END-IF\n");
			}
		}
		else if ( !(prog_ftypes[fd] & AUTOLOCK) )
		{
			tput_line("           MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fd]);
			tput_line("           UNLOCK %s\n", fdname );
#ifdef OLD
			if ( !(prog_ftypes[fd] & SEQ_FILE) || (prog_ftypes[fd] & SEQ_DYN) ) 
				tput_line("           UNLOCK %s\n", fdname );
			else
				tput_line("           UNLOCK %s ALL\n", fdname );
#endif
			tput_line("           MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fd]);
		}
	}

	if (ptype == -1)
	{
		tput_line  ("           CONTINUE.\n");
	}

	if (ptype != -1) hold_line();

	write_log("WISP",'I',"WRITEDONE","Completed WRITE analysys");
	return 0;
}

/*
**	wt_delet.c
*/
p_delete()
{
	int	i,j,fnum,inv_key;
	char	fdname[40];
	int	lock_clause;

	write_log("WISP",'I',"PROCDELETE","Processing DELETE Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be Delete)*/
	j = peek_param(o_parms[1]);							/* peek at the file name		*/

	strcpy(fdname, o_parms[1]);
	if (strchr(fdname,'.')) *((char *)strchr(fdname,'.')) = '\0';			/* save the fdname			*/

	i = 0;
	o_parms[5][0] = '\0';								/* no status field yet			*/

	fnum = file_index(o_parms[1]);							/* see if file exists.			*/

	if (fnum == -1)
	{
		write_log("WISP",'F',"NOTFOUND","File \"%s\" not found while processing DELETE Statement.",o_parms[1]);
	}

	predelete_locking(col);								/* Add locking logic			*/

	tput_line_at		    (12, "MOVE \"DE\" TO WISP-DECLARATIVES-STATUS\n");
	if (vax_cobol)	tput_line_at(12, "MOVE \"N\" TO WISP-TEST-BYTE\n");
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

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'I',"BADINVKEY",
				"Bad DELETE syntax, INVALID KEY followed by a period.");
				o_parms[0][0] = 0;
			}
			else
			{
				ptype = get_param(o_parms[0]);				/* what to do?				*/
			}

			tput_line_at(16, "INVALID KEY");				/* write it out				*/
			if (vax_cobol)
			{	
				tput_line_at(16, "MOVE \"Y\" TO WISP-TEST-BYTE");
				tput_line_at(12, "END-DELETE");
			}
			inv_key = 1;							/* flag it				*/
		}
	}	while ((ptype != -1) && !proc_keyword(o_parms[0]));			/* do till we hit a LAST parm or keyword*/


	if (ptype == -1)								/* a LAST parm				*/
	{
		tput_line_at(20, "%s",o_parms[0]); 
	}

	if ( vax_cobol )
	{
		if (inv_key)
		{
			tput_line("           IF WISP-TEST-BYTE = \"N\" THEN");
			tput_line("               MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fnum]);
			tput_line("               UNLOCK %s", fdname );
			tput_line("               MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fnum]);
			tput_line("           ELSE");
		}
		else
		{
			tput_line("           MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fnum]);
			tput_line("           UNLOCK %s", fdname );
			tput_line("           MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fnum]);
		}
	}

	if (ptype == -1)
	{
		tput_line("           CONTINUE.");
	}



	if (ptype != -1) hold_line();

	write_log("WISP",'I',"DELETEDONE","Completed DELETE analysys");
	return 0;

}

#endif /* OLD */
/*
**	History:
**	$Log: old.c,v $
**	Revision 1.4.2.1  2002/11/12 16:00:30  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.4  1998/03/27 19:11:52  gsl
**	updated
**	
**	Revision 1.3  1998-03-26 12:09:57-05  gsl
**	update
**
**	Revision 1.2  1998-03-23 13:55:02-05  gsl
**	update
**
**
**
**
*/
