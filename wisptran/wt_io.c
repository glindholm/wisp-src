static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	wisp_io.c	Routines to do I/O.

	hold_line()
	get_line()	Get the next line of input.
	get_param()	
	get_ppos()
	peek_param()
	skip_param()
	write_log()
	p_left()
	p_left_1()
*/

#include <stdio.h>
#include <ctype.h>

#ifdef VMS
#include <rmsdef.h>
#include <descrip.h>
#endif


#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "token.h"
#include "lines.h"
#include "input.h"

char *context_infile_name();
cob_file_context *get_curr_cob_context();

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

write_log(facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)				/* write a log line to the current log	*/
char *facil,sever,*mess;								/* facility, severity, message		*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7;						/* The format and parms for the text	*/
{
	write_tlog(NULL,facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7);
	return 0;
}

write_tlog(tokptr,facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)			/* write a log line to the current log	*/
TOKEN *tokptr;
char *facil,sever,*mess;								/* facility, severity, message		*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7;						/* The format and parms for the text	*/
{
	char 	*ptr;
	int	num;
	char	where[80];

	if (nowarnings && sever == 'W') return(0);

	if (tokptr && tokptr->context)
	{
		ptr = context_infile_name(tokptr->context);
		num = context_infile_count(tokptr->context);
		if (tokptr->column)
		{
			sprintf(where,"(%s:%d.%d)",ptr,num,tokptr->column);
		}
		else
		{
			sprintf(where,"(%s:%d)",ptr,num);
		}
	}
	else if (curr_cob_context)
	{
		ptr = context_infile_name(curr_cob_context);
		num = context_infile_count(curr_cob_context);
		sprintf(where,"(%s:%d)",ptr,num);
	}
	else
	{
		where[0] = (char)0;
	}


	if (logging)
	{
		if (sever != ' ')							/* if severity set to space, skip	*/
		     fprintf(logfile,"%%%s-%c-%s %s ",facil,sever,mess,where);		/* write facility, severity, message	*/
		fprintf(logfile,lform,p0,p1,p2,p3,p4,p5,p6,p7);				/* if logging, write it			*/
		fprintf(logfile,"\n");
	}
	else if (sever != 'I' && sever != ' ')						/* if not informational only, report it	*/
	{
		switch(sever)
		{
		case 'T': /* Trace */
		case 'I': /* Informational */
		case 'W': /* Warning */
		case 'E': /* Error */
		case 'F': /* Fatal */
		case 'S': /* Severe */
			printf("%%%s-%c-%s %s ",facil,sever,mess,where);		/* write facility, severity, message	*/
			break;
		case 'M': /* Message */
			printf("%%WISP ");
			break;
		}
		printf(lform,p0,p1,p2,p3,p4,p5,p6,p7);					/* and the text				*/
		printf("\n");
	}
}

static int linelength(line)
char	*line;
{
	char	*ptr;
	int	linelen;

	ptr = (char *)strchr(line,'\n');
	if ( ptr ) linelen = ptr - line;
	else	   linelen = strlen(line);
	return(linelen);
}

int iscomment(the_line)
char	*the_line;
{
	if ((linelength(the_line) > 6) && (the_line[6] == '*' || the_line[6] == '/')) return(1);
	return(0);
}

/*
**	History:
**	$Log: wt_io.c,v $
**	Revision 1.12  1997-02-24 09:58:14-05  gsl
**	Removed the mod_code[] processing. This was remnants of earlier
**	logic which has been removed.  A mod code greater then 11 chars was
**	overwritting memory onto twiddle_count and causing the "twiddle" error.
**
**	Revision 1.11  1996-08-30 21:56:21-04  gsl
**	drcs update
**
**
**
*/
