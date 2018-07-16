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
	check_line()
	get_line()	Get the next line of input.
	get_param()	
	get_kw()
	get_ppos()
	peek_param()
	skip_last()
	skip_param()
	write_log()
	write_line()
	put_com()
	put_line()
	put_char()
	wflush()
	p_left()
	p_left_1()
*/

#include <stdio.h>

#ifdef MSDOS
#include <string.h>
#endif

#define EXT extern
#include "wisp.h"
#ifdef VMS
#include <rmsdef.h>
#include <descrip.h>
#endif

char *wfgets();

#define GETNEXT		0
#define PROCESSIT	1

static int skip_fac = 0;								/* Flag to indicate FAC processing skip.*/
static int proc_code = 0;								/* Process the code in $xxx_CODE stmts.	*/
static int copy_in_copy_code = 0;							/* COPY stmt inside $xxx_CODE		*/

extern int mark_link_sect;
extern int mark_proc_div;

#define VAX_COPY	1
#define LPI_COPY	2
#define ACU_COPY	3
#define AIX_COPY	4
#define MF_COPY		5
#define UNIX_COPY	6
#define DOS_COPY	7
#define ELSE_COPY	8
#define COPY_COPY       9

static char *copy_else[] =	{							/* These are the strings for each COPY	*/
					"",
					"$VAX_ELSE",
					"$LPI_ELSE",
					"$ACU_ELSE",
					"$AIX_ELSE",
					"$MF_ELSE",
					"$UNIX_ELSE",
					"$DOS_ELSE",
					"$ELSE_ELSE",
					"$COPY_ELSE",
				};

static char *copy__else[] =	{							/* These are the strings for each COPY	*/
					"",
					"$VAX-ELSE",
					"$LPI-ELSE",
					"$ACU-ELSE",
					"$AIX-ELSE",
					"$MF-ELSE",
					"$UNIX-ELSE",
					"$DOS-ELSE",
					"$ELSE-ELSE",
					"$COPY_ELSE",
				};

char	copy_brkline[256];								/* Hold brkline until COPY is done	*/

char template[256];
char result[256];
char selecttext[400];
int	period_found= 0;								/* Has period been found.		*/

#ifdef VMS
$DESCRIPTOR(t_desc,template);
$DESCRIPTOR(r_desc,result);
char *context;
#endif

static int twiddle_count = 0;								/* Count how many times line is re-used.*/


/*
	hold_line:	Hold the current line for reuse by next get_line().
*/
int hold_line()										/* flag the last input line as held	*/
{
	save_inline = 1;
	if (twiddle_count > 100)							/* Have we re-used this line too much?!	*/
	{
		write_log("WISP",'F',"INTERR","Internal error processing line\n<%s>\nProbably not a COBOL program.\n",inline);
		exit_wisp(-1);								/* Error exit.				*/
	}
	twiddle_count++;
}

static int find_copy = 0;

check_line()
{
	find_copy = 1;									/* Set to see if has a copy statement.	*/
	get_line();
	if (find_copy == -1)
	{
		find_copy = 0;
		return(1);								/* FOund it.				*/
	}
	else
	{
		find_copy = 0;
		return(0);
	}
}




/*
**	get_line:	Get the next input line. Do all the COPY stmt processing plus handle $xxx_CODE etc.
*/
int get_line()										/* Read a line and extract the parms	*/
{
	int i,j,k,lstat,facpos,oapos,flen,fplen,pflag;
	unsigned long status;
	char save_char;
	char *scn_ptr,*prm_ptr,*p, *ptr;
	char tstr[132],outline[132],comstr[132];
	char	*split_line;								/* Where to split the line.		*/
	int	virgin_line;								/* Is this a new line or re-used.	*/
	int	scan_count;								/* Number of times line has been scanned*/

read_input:
	virgin_line = 0;								/* Assume not-virgin : re-used		*/
	if (save_inline)								/* recycle the last line		*/
	{
		lstat = 1;
		else_flag = 0;								/* clear the else flag.			*/
		save_inline = 0;
		skip_fac = 1;								/* Skip FAC processing.			*/
		skip_kw = 1;								/* Skip VAX keyword checks.		*/
		held_line = 1;								/* This is a recycled line.		*/
		goto process_code;							/* We can skip right to process_code	*/
	}
	else if (brkline[0])								/* Otherwise if there is a leftover line*/
	{
		strcpy(inline,brkline);							/* use it.				*/
		brkline[0] ='\0';
		lstat = 1;
		skip_kw = 0;								/* Do keyword checks.			*/
		skip_fac = 0;								/* make sure it's fac's are processed.	*/
		fac_flag = 0;								/* BUT cancel pending fac's		*/
		oa_flag = 0;								/* And cancel pending ORDER-AREAs	*/
		held_line = 1;								/* This is a recycled line.		*/
	}
	else
	{
		twiddle_count = 0;							/* No more twiddleing.			*/
		lstat = (int)wfgets(inline,132,infile);					/* Read a line				*/
		if (lstat)								/* If read succecced			*/
		{
	 		num_inlines++;							/* count it				*/
			if (copy_name[0])
				num_copy++;						/* In copybook line count		*/
			else
				num_main++;						/* In main source file line count.	*/
		}
		skip_fac = 0;								/* make sure it's fac's are processed.	*/
		skip_kw = 0;								/* Make sure it's VAX keywords are too.	*/
		held_line = 0;								/* This is not a recycled line.		*/
		virgin_line = 1;							/* This is a new line.			*/
	}

	if (!lstat) 									/* Read failed.				*/
	{
		i = ferror(infile);							/* See if there was an error.		*/
		if (i)									/* Any error is an exit.		*/
		{
			perror("%WISP-F-ERRORINFILE");
			exit(1);
		}

		close_input();								/* This file is empty, close it.	*/
		if ((copylib) && open_files) 						/* If writing to separate copybook then	*/
		{
			close_output();							/* close the output copybook.		*/
		}

		if (open_files)								/* If we were in a copybook then	*/
		{
			if (copy_brkline[0])
			{
				strcpy(brkline,copy_brkline);				/* Restore brkline from before COPY	*/
				copy_brkline[0] = '\0';
			}

			copy_in_copy_code = 0;						/* Were out of the copy file		*/
			goto read_input;						/* go and read next line from mainfile.	*/
		}
		else									/* No more input.			*/
		{									/* There are no more files or data.	*/
			copy_para = 0;							/* Alway reset the copy flags.		*/
			copy_decl = 0;
			copy_sect = 0;
			if (pr_count)
			{
				write_log("WISP",'I',"PARSREF",
							"Paragraphs referenced in the PROCEDURE DIVISION by DECLARATIVES.\n");
				new_para();						/* New paragraphs were discovered.	*/
			}
			exit_wisp(0);							/* All done, exit wisp.			*/
		}
	} /* End-IF read failed */

	if ((strlen(inline) > 6) && ((inline[6] == '*') || (inline[6] == '/')))		/* Is it a comment?			*/
	{

		if ( handle_comments() == GETNEXT )
			goto read_input;

	}										/* end of comments processing		*/
	else if (delete_code)								/* we are deleting all non comments	*/
	{
		goto read_input;
	}
	else if (!proc_code && !copy_in_copy_code && copy_code)		/* This will catch copy_code that doesn't have '*'	*/
	{
		inline[6] = ' ';					/* remove the comment character		*/
		inline[72] = '\n';					/* Set up for truncation past 72.	*/
		inline[73] = '\0';
		put_line(inline);
		goto read_input;
	}

/*	At this point, we scan the input line and extract all the parms from it, and also extract the area A data.		*/

process_code:

	memcpy(area_a,&inline[7],4);							/* Get the chars in area a (col 8-11)	*/
	area_a[4] = 0;
	area_a_num = 0;
	sscanf(area_a,"%d",&area_a_num);						/* And see if they are a number		*/
	mod_code[0] = '\0';								/* No mod code yet.			*/

	if (division == WORKING_STORAGE_SECTION && area_a_num == 77 && !copy_only)
	{
		write_log("WISP",'I',"CHNG77","Changed 77 level to 01 level.");
		stredt(&inline[7],"77","01");						/* 77's are changed to 01's		*/
		stredt(area_a,"77","01");
		area_a_num = 1;
	}

	i = strlen(inline);
	if (inline[i-1] == '\n')
	{
		inline[i-1] = 0;
		if (comments && i>72) strcpy(mod_code,&inline[72]);			/* Preserve the mod code		*/

		inline[72] = 0;								/* Discard the notes collumn		*/
		if (i > 72) trim(inline);						/* and trim the notes, if any		*/
		strcat(inline,"\n");							/* then put the newline back		*/
	}
	else
	{
		if (comments && i>72) strcpy(mod_code,&inline[72]);			/* Preserve the mod code		*/
		inline[72] = 0;
		if (i > 72) trim(inline);
	}
	i = strlen(inline);


	/*
	**	SCAN THE LINE
	*/

	scan_count = 0;
scan_line:
	scan_count++;


	if (isalit && !held_line)							/* Was last line an open literal?	*/
	{
		if (inline[6] != '-')
		{
			write_log("WISP",'W',"NOCONT","No continuation for open literal in column 7, added by WISP.");
			inline[6] = '-';
		}
	}

	isalit = 0;									/* Clear has an open literal flag	*/
	has_lit = 0;									/* Clear has a lit flag			*/
	has_cont = 0;									/* Clear has a continuation flag	*/

	period_found = 0;								/* A period has not yet been found	*/
	split_line = '\0';								/* Clear pointer where to split line	*/

	for (i=0; i<24; i++) parms[i][0] = 0;						/* Set parms to null string first	*/

	if (strlen(inline) > 7)								/* anything to scan?			*/
	{
		i = 0;
		j = 7;

		if (inline[6] == '-') has_cont = 1;					/* Does it have a continuation character*/

		scn_ptr = &inline[7];							/* Point to first char in margin A.	*/
		if (*scn_ptr) do							/* scan the line and extract the parms	*/
		{									/* This part of the 'if' is just used to*/
											/* skip over spaces, commas, newlines	*/
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == ';') ||
			    (*scn_ptr == '\n') || (*scn_ptr == '\t') || (*scn_ptr == '%'))
			{
				if (*scn_ptr == '\t')					/* Tabs should not be in this file.	*/
				{
					write_log("WISP",'W',"TABINLINE",
						"TAB character found in line, position %d. Counted as a space.\n%s",j,inline);
					*scn_ptr = ' ';
				}
				else if (*scn_ptr == '%')				/* The '%' means comments.		*/
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
							*scn_ptr++ = ' ';		/* Pad inline, so stredt's will work too*/
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
						 (*scn_ptr != '\n')&& (*scn_ptr != '\t'));
				}

				if (*scn_ptr == ',')
				{
					scn_ptr++;					/* skip over commas			*/
					j++;
				}
				else if (*scn_ptr == '\t')
				{
					write_log("WISP",'W',"TABINLINE",
						"TAB character found in line, position %d. Counted as a space.\n%s",j,inline);
					*scn_ptr = ' ';
				}
				*prm_ptr = 0;						/* end with a null			*/
			} /* End-IF NOT whitespace */
		} while (*scn_ptr);							/* till a null is found			*/

		if (*scn_ptr == '%')							/* Remove imbedded comments.		*/
		{
			strcpy(comstr,"      *  ");					/* Start a comment line.		*/
			strcat(comstr,scn_ptr);						/* Add the %comment.			*/
			put_com(comstr);						/* Now output it.			*/

			*scn_ptr++ = '\n';						/* Remove it from INLINE		*/
			*scn_ptr = '\0';
		}
	} /* End-IF something on line to scan */

	parm_num = 0;									/* Current parm for parsing		*/

	if (parms[0][0] == 0)								/* Must be a blank line			*/
	{										/* If blanks are kept, then just	*/
		if (blanklines) 							/* write a blank line			*/
		{
			i = strlen(inline);
			if (inline[i-1] == '\n')
			{
				inline[i-1] = 0;					/* remove the ending newline (if any)	*/
			}
			put_com(inline);						/* Blanks are treated as comments	*/
		}
		goto read_input;							/* Get another line			*/
	}

/*			 from here on we will process the parms and try to decipher what the program is doing.			*/
/* 			This section represents things to do irregardless of what division we are currently in, like		*/
/* 			comment and copy command processing is always done.							*/


	if (area_a_num) strcpy(last_field,parms[1]);					/* Save the field name.			*/

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
		strcpy(brkline,"000001     ");						/* Copy 2nd half to brkline.		*/
		strcat(brkline,split_line);
		split_line[0] = '\n';							/* Terminate 1st half of line.		*/
		split_line[1] = '\0';
		goto scan_line;								/* Rescan.				*/
	}


	j=0;
	if (strpos(inline," COPY ") != -1)						/* Is " COPY " on the line.		*/
	{
		for(j=1; parms[j][0]; j++)						/* Is "COPY" later on the line.		*/
		{
			if ( strcmp(parms[j],"COPY")==0 ) break;			/* Found one.				*/
		}
	}

	if ( j != 0 && strcmp(parms[j],"COPY")==0 )					/* COPY is later on line so split line	*/
	{
		strcpy(brkline,"000001     ");						/* Copy 2nd half to brkline.		*/
		strcat(brkline,&inline[p_parms[j]]);
		inline[p_parms[j]] = '\n';						/* Terminate 1st half of line.		*/
		inline[p_parms[j]+1] = '\0';
		goto scan_line;								/* Rescan.				*/
	}

	if ( strcmp(parms[0],"COPY")==0 )						/* Process COPY copybook commands	*/
	{
											/* Process file name			*/
		if (find_copy)
		{
			find_copy = -1;							/* We are just looking.			*/
			hold_line();
			return(0);
		}

		i = strlen(parms[1])-1;
		if (parms[1][i] == '.')							/* Remove trailing period.		*/
		{
			parms[1][i] = 0;
		}
		stredt(parms[1],"\"","");						/* Remove possible leading quote.	*/
		stredt(parms[1],"\"","");						/* Remove possible trailing quote.	*/
		stredt(parms[3],"\"","");						/* Remove possible leading quote.	*/
		stredt(parms[3],"\"","");						/* Remove possible trailing quote.	*/

		strcpy(cpy_lib,parms[3]);						/* Save it.				*/
		strcpy(cpy_file,parms[1]);						/* Save the file name too.		*/

											/* Process Library name			*/
		if (parms[3][0] == '\0')						/* left out the INLIB			*/
		{
			write_log("WISP",'I',"USEINLIB","Using INLIB for COPY of %s.",parms[1]);
			if (cli_ildir[0] == '\0')					/* no inlib, error			*/
			{
				write_log("WISP",'W',"NOINLIB","No INLIB specified for COPY of %s.",parms[1]);
			}
			else
			{
				sp_trunc(cli_ildir);					/* truncate at first space		*/
				strcpy(parms[3],cli_ildir);				/* copy the inlib into parm #3		*/
			}								/* then just continue			*/
		}
		else
		{
#ifdef unix
			{
				char temp[80];
				strcpy(temp, "../");
				strcat(temp, parms[3]);
				strcpy(parms[3],temp);
			}
#endif
#ifdef MSDOS
			{
				char temp[80];
				strcpy(temp, "..\\");
				strcat(temp, parms[3]);
				strcpy(parms[3],temp);
			}
#endif
		}

		i = strlen(parms[3])-1;
		if (parms[3][i] == '.')							/* Remove trailing period if need be	*/
		{
			parms[3][i] = 0;
		}

		fparse(parms[1],"wcb",parms[3]);					/* Call fparse to build correct file	*/

#ifndef unix
		uppercase(parms[1]);							/* Make uppercase			*/
#endif

		inline[6] = '*';							/* Make original command a comment	*/
		put_line(inline);							/* And write it out as such		*/

		sprintf(outline,"      *COPY %s\n",parms[1]);				/* Output the real COPY command		*/
		put_line(outline);

		open_input(parms[1]);							/* Open it and activate			*/

		if (!concat)								/* If not concat mode, put the COPY	*/
		{									/* command into the outfile		*/
			strcpy(template,parms[1]);					/* Template has the lib:file.wcb name	*/
#ifdef VMS
			if (trans_lib)							/* Should we translate the lib.		*/
			{								/* Before we open the output file.	*/
				t_desc.dsc$w_length = strlen(template);
       				status = LIB$FIND_FILE(&t_desc,&r_desc,&context,0,0,0,0);   /* look for a file			*/
       				if (status == RMS$_NORMAL)				/* got one...				*/
      				{
					result[strpos(result,";")] = '\0';		/* Null terminate it.			*/
	       			}
				else
				{
					strcpy(result,template);   			/* Set up for failure.			*/
				}
				status = LIB$FIND_FILE_END(&context);			/* free the file context		*/
		       		stredt(template,".WCB",".LIB");				/* Change .WCB to .LIB			*/
		       		stredt(result,".WCB",".LIB");				/* Change .WCB to .LIB			*/
			}
			else
			{
		       		stredt(template,".WCB",".LIB");				/* Change .WCB to .LIB			*/
				strcpy(result,template);
			}
#else
			stredt(template,".wcb",".lib");					/* Change .WCB to .LIB			*/
			strcpy(result,template);					/* Result is equal to template.		*/
#endif
			write_line("            COPY \042%s\042.\n",template);		/* Write a COPY command			*/

			if (copylib)
			{
				open_output(result);					/* Open the new output file		*/
			}
		}

		if ( copy_code )							/* A COPY stmt inside of $xxx_CODE	*/
		{
			copy_in_copy_code = 1;
		}

		if (brkline[0])							/* If there is stuff in brkline then we need to	*/
		{								/* hold the brkline until after the copybook	*/
			strcpy(copy_brkline,brkline);				/* has been processed.				*/
			brkline[0] = '\0';
		}
		else
		{
			copy_brkline[0] = '\0';					/* Nothing held in brkline			*/
		}

		goto read_input;							/* read another line from the input	*/
	}										/* End of COPY processing		*/

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
				stredt(inline," OF","");				/* remove the word			*/
			}
			stredt(templine,".","");					/* Remove period, if any from var name	*/

			make_fac(outline,templine);					/* make a FAC variable			*/

			stredt(inline,templine,outline);				/* and replace old var with new		*/
			wsqueeze(inline,72);						/* keep the line less than 73 chars	*/
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
				stredt(inline," OF","");				/* remove the word			*/
			}
			stredt(templine,".","");					/* Remove period, if any from var name	*/

			make_oa(outline,templine);					/* make an ORDER-AREA variable		*/
			stredt(inline,templine,outline);				/* and replace old var with new		*/
			wsqueeze(inline,72);						/* keep the line less than 73 chars	*/
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
					stredt(&inline[facpos],"(FAC","(");		/* Edit (FAC into spaces		*/
					flen = 2;
				}
				else
				{
					stredt(&inline[facpos]," FAC","    ");		/* Edit FAC into spaces			*/
					flen = 4;
					facpos += 3;					/* advance the pointer.			*/
				}

				p_left(i,flen);						/* Shift parms left one position.	*/

				j++;

				if (!strcmp(parms[i],"OF"))				/* is it followed by OF verb		*/
				{
					flen = -1;					/* Remove 4 chars during shift.		*/
					if (stredt(&inline[facpos]," OF ","") == -1)	/* Edit OF into null, leaving room for	*/
					{						/* F-O-	to be appended			*/
						stredt(&inline[facpos]," OF","");	/* it is at end of line			*/
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

#ifdef OLD
					k = strpos(outline,"(");
					j = i + 1;					/* If open paren and no close, or	*/

					if (((k != -1) && (strpos(outline,")") == -1)) ||
						(parms[j][0] == '('))			/* Next parm has subscript.		*/
					{
						if (k != -1)				/* If this parm already has open, need 	*/
						{					/* to put in a space before adding on	*/
							strcat(outline," ");		/* The next parm.			*/
						}
						strcat(outline,parms[j]);		/* add it on.				*/
						stredt(&inline[facpos+fplen],parms[j],"");	/* remove from input line	*/
											/* Scan for last paren.			*/
											/* But quit if not found on this line.	*/
						while (parms[j][0] && strpos(parms[j],")") == -1)
						{
							p_left_1(j,flen);		/* Shift parms to the left.		*/
							flen = 0;

							if (parms[j][0] != ')')		/* May be multi dimesioned, add a space.*/
							{
								strcat(outline," ");
							}
							strcat(outline,parms[j]);	/* And add the name.			*/
							stredt(&inline[facpos+fplen],parms[j],"");	/* Remove from input.	*/
						}
						if (!parms[j][0])
							write_log("WISP",'W',"NOCLOSEPAR",
									"Closing parentheses for FAC not found - %s.",outline);
						p_left_1(j,flen);			/* Remove last word.			*/
					}
#endif
					stredt(&inline[facpos],templine,outline);	/* And replace old var with new		*/

#ifdef OLD
					if (pflag)					/* Did it have an open paren?		*/
					{
						pflag = 0;				/* Clear parentheses flag		*/
						strcpy(parms[i],"(");			/* Start it out.			*/
						strcat(parms[i],outline);		/* Add the parm too.			*/
					}
					else
					{
						strcpy(parms[i],outline);		/* Copy the parm.			*/
					}
#endif
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
					stredt(&inline[oapos],"(ORDER-AREA","(");	/* Edit (ORDER-AREA into spaces		*/
					flen = 2;
				}
				else
				{
					stredt(&inline[oapos]," ORDER-AREA","           ");	/* Edit ORDER-AREA into spaces	*/
					flen = 11;
					oapos +=10;
				}

				p_left(i,flen);						/* Shift parms left one position.	*/

				j++;

				if (!strcmp(parms[i],"OF"))
				{
					flen = -1;
					if (stredt(&inline[oapos]," OF ","") == -1)	/* Edit OF into null, leaving room for	*/
					{						/* O-A-	to be appended			*/
						stredt(&inline[oapos]," OF","");	/* it is at end of line			*/
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

					stredt(&inline[oapos],templine,outline);	/* and replace old var with new		*/
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
				strcpy(brkline,inline);					/* save the current line.		*/
				strcpy(inline,"             CONTINUE\n");		/* Put a CONTINUE between the lines.	*/
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
	
			if (!skip_kw && kwproc && keyword(tstr,res_keywords))		/* Have we found one.			*/
			{
				write_log("WISP",'I',"REPLRESERVE","Reserved word %s renamed to W-%s.\n",tstr,tstr);
				strcpy(templine,"W-");
				strcat(templine,tstr);					/* build the new name for it.		*/
				stredt(&inline[p_parms[i]],tstr,templine);		/* replace it in the input line		*/

				if (wsqueeze(inline,72)) goto scan_line;		/* If it needed a squeeze, reparm it.	*/

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
					write_log("WISP",'I',"ELSEBROK","Else statement broken apart. Old line is:\n%s\n",inline);
					memset(brkline,' ',p_parms[i]+4);		/* Fill with spaces first.		*/
					brkline[p_parms[i]+4] = '\0';			/* Null terminate.			*/
					strcat(brkline,&inline[p_parms[i] + 4]);	/* save the current line.		*/
					inline[p_parms[i] + 4] = '\n';			/* Terminate the old line		*/
					inline[p_parms[i] + 5] = '\0';			/* Terminate the old line		*/
					parms[i+1][0] = '\0';				/* No more parms.			*/
				}
			}								/* After skipping first word,		*/
			else if (i != 0 && !strcmp(tstr,"FREE"))			/* Look for imbedded FREE ALL's.	*/
			{
				write_log("WISP",'I',"FREEBROK","FREE ALL statement broken apart. Old line is:\n%s\n",inline);
				memset(brkline,' ',p_parms[i]);				/* Fill with spaces first.		*/
				brkline[p_parms[i]] = '\0';				/* Null terminate.			*/
				strcat(brkline,&inline[p_parms[i]]);			/* save the current line.		*/
				inline[p_parms[i]] = '\n';				/* Terminate the old line		*/
				inline[p_parms[i] + 1] = '\0';				/* Terminate the old line		*/
				parms[i+1][0] = '\0';					/* No more parms.			*/
			}
			else if (i != 0 && !strcmp(tstr,"COMMIT"))			/* Look for imbedded COMMIT's.		*/
			{
				write_log("WISP",'I',"COMMITBROK","COMMIT statement broken apart. Old line is:\n%s\n",inline);
				memset(brkline,' ',p_parms[i]);				/* Fill with spaces first.		*/
				brkline[p_parms[i]] = '\0';				/* Null terminate.			*/
				strcat(brkline,&inline[p_parms[i]]);			/* save the current line.		*/
				inline[p_parms[i]] = '\n';				/* Terminate the old line		*/
				inline[p_parms[i] + 1] = '\0';				/* Terminate the old line		*/
				parms[i+1][0] = '\0';					/* No more parms.			*/
			}
			else if ( i && keyword(tstr,brk_keywords))			/* Is it a second keyword?		*/
			{
				memset(brkline,' ',p_parms[i]);				/* Fill with spaces first.		*/
				brkline[p_parms[i]] = '\0';				/* Null terminate.			*/
				strcat(brkline,&inline[p_parms[i]]);			/* save the current line.		*/
				inline[p_parms[i]] = '\n';				/* Terminate the old line		*/
				inline[p_parms[i] + 1] = '\0';				/* Terminate the old line		*/
				parms[i][0] = '\0';					/* No more parms.			*/
				write_log("WISP",'I',"SECKEWD","Second keyword on line.\nOld: %sNew: %s",inline,brkline);
				break;
			}
			i++;
		} while ( parms[i][0] );						/* Till all parms have been checked	*/
	}
	return(lstat);
}

/*
	extract_param	This routine will get the next parm and remove it from inline.
*/
int extract_param(the_parm)
char 	the_parm[];
{
	int	ptype;

	ptype = get_param(the_parm);
	stredt(&inline[7],the_parm,"");
	return(ptype);
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

int get_kw(the_parm)									/* Get next real keyword.		*/
char *the_parm;
{
	int i;
	i = get_param(the_parm);
	if (!strcmp(the_parm,"IS"))							/* Skip over IS.			*/
	{
		i = get_param(the_parm);
	}
	return(i);
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

skip_last()										/* skip to the last parm (with a period)*/
{
	int ptype;
	char a_line[133];
	do
	{
		ptype = get_param(a_line);
	}
	while (ptype != -1);								/* look for the parm that is last	*/
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
	char out[256];									/* scratch output line			*/
	char *ptr;
	int	num;

	if (nowarnings && sever == 'W') return(0);
	
	if (copy_name[0])
	{
		ptr = copy_name;
		num = num_copy;
	}
	else
	{
		ptr = main_name;
		num = num_main;
	}

	if (logging)
	{
		if (sever != ' ')							/* if severity set to space, skip	*/
		     fprintf(logfile,"%%%s-%c-%s (%s:%d) ",facil,sever,mess,ptr,num);	/* write facility, severity, message	*/
		fprintf(logfile,lform,p0,p1,p2,p3,p4,p5,p6,p7);				/* if logging, write it			*/
		fprintf(logfile,"\n");
	}
	if (sever != 'I' && sever != ' ' && !logging)					/* if not informational only, report it	*/
	{
		if (sever != ' ')							/* if severity set to space, skip	*/
			printf("%%%s-%c-%s (%s:%d) ",facil,sever,mess,ptr,num);	/* write facility, severity, message	*/
		printf(lform,p0,p1,p2,p3,p4,p5,p6,p7);					/* and the text				*/
		printf("\n");
	}
}

write_line(lform,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)					/* write a line to the current outfile	*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8,*p9,*p10,*p11;
{
	sprintf(sprtemp,lform,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,"{ERROR}");
	put_line(sprtemp);
}


static int is_a_com = 0;								/* Flag to indicate comments.		*/

put_com(the_line)									/* Write a comment line and a newline.	*/
char *the_line;
{
	if (!outlen)									/* Nothing pending in the output buffer.*/
	{
		is_a_com = 1;								/* Flag it.				*/
		put_line(the_line);							/* Simply output the line.		*/
		put_line("\n");								/* And a newline			*/
		is_a_com = 0;
	}
	else										/* Have to hold this line till we see	*/
	{										/* A newline in the output buffer.	*/
		do
		{
			*com_ptr++ = *the_line++;					/* Copy the line into the comment buffer*/
		} while (*the_line && ((com_ptr - com_buf) < COMMENT_BUFFER_SIZE-4));

		*com_ptr++ = '\n';
		*com_ptr = '\0';							/* Be sure to leave a null.		*/
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

static int iscomment(line)
char	*line;
{
	int	linelen;

	linelen = linelength(line);

	if ( linelen > 6 && (line[6] == '*' || line[6] == '/') ) return(1);
	else							 return(0);
}

put_line(the_line)									/* put a line into the output buffer	*/
char *the_line;
{
	char tstr[132],pstr[132],editbuff[512];
	char *save_line,*last_line;
	int i,comment_line;
	char	*ptr;

	save_line = the_line;

	if (outlen == 0)
		comment_line = iscomment(the_line);
 
	if (*the_line && ((open_files == 1) || (concat) || (copylib && !skiplib)))	/* If we are reading the main input 	*/
	{										/* and if not a null string		*/
		last_line = the_line;

		while (*the_line)							/* till we see a null			*/
		{
			if (fo_cnt > OUTPUT_BUFFER_SIZE) wflush();			/* Won't fit in buffer, flush it	*/

			fo_buff[fo_cnt++] = *the_line;					/* copy each character			*/

			if (*the_line == '\n')
			{
				out_number++;						/* count the output line numbers	*/

				if (mod_code[0])					/* Need to dump the mod code.		*/
				{
					fo_cnt--;					/* Back up over the newline.		*/
					if (outlen < 72) do
					{
						if (fo_cnt > OUTPUT_BUFFER_SIZE-1) wflush();
						fo_buff[fo_cnt++] = ' ';		/* Pad it with spaces.			*/
					} while (++outlen < 72);

					i = 0;
					do
					{
						if (fo_cnt > OUTPUT_BUFFER_SIZE-1) wflush();
						fo_buff[fo_cnt++] = mod_code[i++];	/* Copy the mod code			*/
					} while(mod_code[i]);				/* Till a null				*/
					mod_code[0] = '\0';				/* Clear it.				*/
					fo_buff[fo_cnt++] = '\n';			/* Add the newline.			*/
				}

				outlen = 0;						/* Reset line length counter.		*/
				last_line = the_line + 1;

				if (com_ptr != com_buf)					/* Need to dump the comment buffer.	*/
				{
					com_ptr = com_buf;				/* Set the pointers equal.		*/

					do
					{
						if (fo_cnt > OUTPUT_BUFFER_SIZE) wflush();	/* Won't fit in buffer, flush it*/

						if (*com_ptr == '\n')
						{
							out_number++;			/* count the output line numbers	*/
						}
						fo_buff[fo_cnt++] = *com_ptr++;		/* copy each character			*/
					} while (*com_ptr);
					com_ptr = com_buf;				/* Set the pointers equal.		*/
				}

				comment_line = iscomment(the_line+1);			/* check if next line is a comment	*/
			}
			else
			{
				outlen++;						/* Keep track of it's length.		*/
				if ((outlen == 73) && !copy_code && !copy_only && !is_a_com && !comment_line)
				{
					char *ptr;
					int	num;

					if (copy_name[0])
					{
						ptr = copy_name;
						num = num_copy;
					}
					else
					{
						ptr = main_name;
						num = num_main;
					}

					printf("%%WISP-E-LINELONG (%s:%d) Line too long\n%s\n",ptr,num,last_line);
				}
			}
			the_line++;							/* copy each character			*/
		}
	}

	/*
	**	This section writes out the relocated declarative paragraphs.  It needs to be done even if skiplib is
	**	set and we are not generating .lib files.
	*/

	if (*save_line)
	{
		if (copy_para)								/* Are we copying procedure div to decl	*/
		{
			int sec_flag;							/* Local flag, had a SECTION kwd.	*/
			sec_flag = 0;

			strcpy(editbuff, save_line);					/* Make a local copy to edit		*/
			save_line = editbuff;

			if (!dump_ifbuff)						/* If not an ifbuff, fix performs.	*/
			{
				if ((strlen(save_line) < 7) ||
				    ((strlen(save_line) >= 7) && save_line[6] != ' '))
				{
					/* blank or comment line */
				}
				else if (0 != strcmp(area_a,"    "))			/* something in area A			*/
				{
					strcpy(pstr,parms[0]);
					stredt(pstr,".","");
					make_fld(tstr,pstr,"D-");			/* It is a paragraph name, edit it.	*/
					stredt(save_line,pstr,tstr);
					if (stredt(save_line," SECTION","") != -1)	/* Remove possible SECTION.		*/
					{
						sec_flag = 1;				/* And flag it.				*/
					}
				}
				else if (!strcmp(parms[0],"PERFORM"))			/* Fix performs.			*/
				{
					strcpy(pstr,parms[1]);				/* get the name.			*/
					stredt(pstr,".","");				/* remove the period			*/

					for (i=0; i<par_count; i++)			/* See if it is a paragraph fix.	*/
					{
						if (!strcmp(pstr,par_name[i])) break;	/* found it.				*/
					}

					if ( i < par_count)
					{						/* now process it.			*/
						make_fld(tstr,pstr,"D-");
						stredt(save_line,pstr,tstr);		/* replace it in the line.		*/
					}
				}
				else if (!strcmp(parms[0],"GO"))			/* Fix performs.			*/
				{
					strcpy(pstr,parms[2]);				/* get the name.			*/
					stredt(pstr,".","");				/* remove the period			*/

					for (i=0; i<par_count; i++)			/* See if it is a paragraph fix.	*/
					{
						if (!strcmp(pstr,par_name[i])) break;	/* found it.				*/
					}

					if (i < par_count)
					{						/* now process it.			*/
						make_fld(tstr,pstr,"D-");
						stredt(save_line,pstr,tstr);		/* replace it in the line.		*/
					}
				}
				else 							/* Examine for WISP generated perform	*/
				{
					stredt(save_line," WDR-"," DWDR-");
					stredt(save_line," WGS-"," DWGS-");
					stredt(save_line," WISP-EXIT-PROGRAM"," D-WISP-EXIT-PROGRAM");
					stredt(save_line," WISP-STOP-RUN",    " D-WISP-STOP-RUN");
				}

				if (strlen(save_line) > 13 && save_line[6] == ' ')
				{
					for(ptr = &save_line[6]; *ptr == ' '; ptr++) {}	/* Scan thur the leading spaces		*/
					if ( 0==memcmp(ptr,"COPY ",5) )			/* If a COPY stmt			*/
					{
						save_line[6] = '*';			/* Comment out the COPY stmt		*/
					}
				}
			}
			fprintf(par_file,"%s",save_line);				/* Put line into DECLARATIVES lib.	*/
			if (sec_flag)							/* This was s SECTION header we copied.	*/
			{								/* Put a PERFORM THRU into the DECLAR.	*/
				fprintf(par_file,"           PERFORM WISP-SECTION-%d-BEGIN THRU\n",sect_num);
				fprintf(par_file,"                   WISP-SECTION-%d-END.\n",sect_num);
				fprintf(par_file,"       WISP-SECTION-%d-BEGIN.\n",sect_num);	/* And the start paragraph.	*/
			}
		}

		if (copy_decl)
		{

			if (!dump_ifbuff)						/* If not an ifbuff, fix performs.	*/
			{
				if ((strlen(save_line) < 7) ||
				    ((strlen(save_line) >= 7) && save_line[6] != ' '))
				{
					/* blank or comment line */
				}
				else if (0 != strcmp(area_a,"    "))			/* something in area A			*/
				{
					strcpy(pstr,parms[0]);
					stredt(pstr,".","");
					make_fld(tstr,pstr,"D-");			/* It is a paragraph name, edit it.	*/
					stredt(save_line,pstr,tstr);
				}
				else if (!strcmp(parms[0],"PERFORM"))			/* Fix performs.			*/
				{
					strcpy(pstr,parms[1]);				/* get the name.			*/
					stredt(pstr,".","");				/* remove the period			*/

					for (i=0; i<ppdiv_count; i++)			/* See if it is a paragraph fix.	*/
					{
						if (!strcmp(pstr,perf_pdiv[i])) break;	/* found it.				*/
					}

					if (i < ppdiv_count)
					{						/* now process it.			*/
						make_fld(tstr,pstr,"D-");
						stredt(save_line,pstr,tstr);		/* replace it in the line.		*/
					}
				}
				else if (!strcmp(parms[0],"GO"))			/* Fix performs.			*/
				{
					strcpy(pstr,parms[2]);				/* get the name.			*/
					stredt(pstr,".","");				/* remove the period			*/

					for (i=0; i<ppdiv_count; i++)			/* See if it is a paragraph fix.	*/
					{
						if (!strcmp(pstr,perf_pdiv[i])) break;	/* found it.				*/
					}

					if (i < ppdiv_count)
					{						/* now process it.			*/
						make_fld(tstr,pstr,"D-");
						stredt(save_line,pstr,tstr);		/* replace it in the line.		*/
					}
				}
				else
				{
					stredt(save_line," DWDR-"," WDR-");
					stredt(save_line," DWGS-"," WGS-");
				}
			}
			fprintf(decl_file,"%s",save_line);
		}
	}
}

put_char(the_char)									/* put a char into the output buffer	*/
char the_char;
{
	char one_char[2];

	one_char[0] = the_char;
	one_char[1] = '\0';
	put_line(one_char);
}

wflush()										/* flush WISP output buffer		*/
{
	int i;

	if (fo_cnt) i = write(outfile,fo_buff,fo_cnt);					/* write it out				*/
	else	    return(0);								/* or just say no			*/
	fo_cnt = 0;
	if (i == -1)
	{
/*		printf("\n%WISP-F-ERRORWRITE Error writing output file. Check Quotas or protections.\n"); */
		write_log("WISP",'F',"ERRORWRITE","Error writing output file.");
		exit(1);
	}
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
}

/*
	wfgets		This is a frontend to fgets
*/
char *wfgets(s, n, stream)
char	*s;
int	n;
FILE	*stream;
{
	char	*ptr;

	ptr = fgets(s, n, stream);
	return(ptr);
}



static int handle_comments()
{
	int	i;
	char	*ptr;

		num_comments++;								/* yes, count it.			*/

		if (copy_only)								/* just copy it.			*/
		{
			put_line(inline);						/* Comments are just copies		*/
			return(GETNEXT);						/* Now Read another line		*/
		}

		if (pmode == WS_SCREEN)							/* It is in a screen record, see if it	*/
		{									/* is a range count			*/
			if ((i = strpos(inline,"$RANGE_COUNT")) != -1)			/* look for the keyword			*/
			{
				i += 13;
				sscanf(&inline[i],"%d",&r_count);			/* now find the count			*/
				write_log("WISP",'I',"RANGESET","Range count set to %d.",r_count);
			}
		}

		if ((i = strpos(inline,"$WANG_PROC")) != -1)				/* look for the PROC keyword		*/
		{
			isaproc = 1;							/* Set the flag.			*/
			write_log("WISP",'I',"PROCSET","PROC flag set for next OPEN statement");
		}

		if ((i = strpos(inline,"$AUTOLOCKPRINT")) != -1)			/* On VMS use automatic locking on 	*/
		{									/* PRINT files.				*/
			autolockprint = 1;						/* Set the flag.			*/
			write_log("WISP",'I',"AUTOLOCKPRINT","On VMS print files will use automatic record locking.");
		}

		if ((i = strpos(inline,"$AUTOLOCKFILE")) != -1)				/* On VMS use automatic locking on 	*/
		{									/* the specified file.			*/
			autolockfile = 1;						/* Set the flag.			*/
			write_log("WISP",'I',"AUTOLOCKFILE","On VMS this file will use automatic record locking.");
		}

		if ((i = strpos(inline,"$COMPRESSFILE")) != -1)				/* Add COMPRESSION to next file. 	*/
		{
			compressfile = 1;
			write_log("WISP",'I',"COMPRESSFILE","Add COMPRESSION to next file.");
		}

		if ((i = strpos(inline,"$MULTIPLELOCK")) != -1)				/* Add LOCK MODE IS AUTOMATIC 		*/
		{									/*     WITH LOCK ON MULTIPLE RECORDS	*/
			multiplelock = 1;
			write_log("WISP",'I',"MULTIPLELOCK","Add WITH LOCK ON MULTIPLE RECORDS to next file.");
		}

#ifdef OLD
This has been disabled because bugs in Micro Focus support of nested programs.  Specifically SEARCH failed and occasionally IF
also failed because of addressing errors.  Additionally SUN COBOL would not compile nested programs.
1/31/92 GSL
		if ((i = strpos(inline,"$LINKMAIN")) != -1)
		{
			if (mf_cobol || aix_cobol)
			{
				linkmain = 1;
				write_log("WISP",'I',"LINKMAIN","Found $LINKMAIN directive");
			}
		}
#endif

		if ((i = strpos(inline,"$NOOPTIONAL")) != -1)
		{
			nooptional = 1;
			write_log("WISP",'I',"NOOPTIONAL","Suppress OPTIONAL clause on next file.");
		}

		if ((i = strpos(inline,"$SEQLINE")) != -1)
		{
			seqline = 1;
			write_log("WISP",'I',"SEQLINE","Force to LINE SEQUENTIAL.");
		}

		if ((i = strpos(inline,"$SEQBINARY")) != -1)
		{
			seqbinary = 1;
			write_log("WISP",'I',"SEQBINARY","Force to BINARY SEQUENTIAL.");
		}

		if ((i = strpos(inline,"$SEQRECORD")) != -1)
		{
			seqbinary = 1;
			write_log("WISP",'I',"SEQRECORD","Force to RECORD SEQUENTIAL.");
		}

		if ((i = strpos(inline,"$SORTFILE")) != -1 ||
		    (i = strpos(inline,"$SORT_FILE")) != -1   )
		{
			sortfile = 1;
			write_log("WISP",'I',"SORTFILE","Next file is a SORTFILE.");
		}

		if ((i = strpos(inline,"$SELECTTEXT")) != -1)
		{
			write_log("WISP",'I',"SELECTTEXT","Force TEXT into SELECT statement.");
			memset(inline,' ',7);
			stredt(inline,"$SELECTTEXT","     ");
			ptr = (char *)strchr(inline,'\n');
			if (ptr) *ptr = '\0';
			strcat(selecttext,"\n");					/* Start with a newline			*/
			strcat(selecttext,inline);
			strcpy(inline,"\n");
			if (strlen(selecttext) >= sizeof(selecttext))
			{
				write_log("WISP",'F',"SELECTTEXT","Too much $SELECTTEXT; buffer overflow!");
				exit_wisp(-1);
			}
		}


		/*
			The follow code handles these conditional wisp directives

							Delete_code    Copy_code
			*$WANG_CODE			
			  ...wang specific code		    ON            OFF
			*$WANG_ELSE
			* ...non-wang code		    OFF           VAX,LPI,ACU,AIX,MF,UNIX,DOS
			*$WANG_END
											Delete_code	Copy_code
			*$VAX_CODE				vax_cobol			not vax_cobol
			* ...vax specific code		    OFF           VAX		    ON             OFF    
			*$VAX_ELSE
			  ...non-vax code                   ON            OFF               OFF            LPI,ACU
			*$VAX_END

			*$LPI_CODE				lpi_cobol			not lpi_cobol
			* ...lpi specific code		    OFF		  LPI		    ON		   OFF
			*$LPI_ELSE
			  ...non-lpi code		    ON		  OFF		    OFF		   ACU,VAX
			*$LPI_END

			*$ACU_CODE				acu_cobol			not acu_cobol
			* ...acu specific code		    OFF		  ACU		    ON		   OFF
			*$ACU_ELSE
			  ...non-acu code		    ON		  OFF		    OFF		   VAX,LPI
			*$ACU_END

		*/

		if (delete_code)							/* Are we currently deleting code?	*/
		{
			if ((strpos(inline,"$WANG_END") != -1) || 			/* WANG code is deleted	*/
			    (strpos(inline,"$WANG-END") != -1) 	  )
			{
				write_log("WISP",'I',"WANGEND","End of WANG-only code.");	/* log it			*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$VAX_END") != -1) ||			/* NON-VAX code is deleted		*/
			         (strpos(inline,"$VAX-END") != -1)    )
			{
				write_log("WISP",'I',"VAXEND","End of NON-VAX code.");	/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$LPI_END") != -1) || 			/* NON-LPI code is deleted		*/
			         (strpos(inline,"$LPI-END") != -1)    )
			{
				write_log("WISP",'I',"LPIEND","End of NON-LPI code.");	/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$ACU_END") != -1) ||			/* NON-ACU code is deleted		*/
			         (strpos(inline,"$ACU-END") != -1)    )
			{
				write_log("WISP",'I',"ACUEND","End of NON-ACU code.");	/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$AIX_END") != -1) ||			/* NON-AIX code is deleted		*/
			         (strpos(inline,"$AIX-END") != -1)    )
			{
				write_log("WISP",'I',"AIXEND","End of NON-AIX code.");	/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$MF_END") != -1) ||			/* NON-MF code is deleted		*/
			         (strpos(inline,"$MF-END") != -1)    )
			{
				write_log("WISP",'I',"MFEND","End of NON-MF code.");	/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$UNIX_END") != -1) ||			/* NON-UNIX code is deleted		*/
			         (strpos(inline,"$UNIX-END") != -1)    )
			{
				write_log("WISP",'I',"UNIXEND","End of NON-UNIX code.");/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$DOS_END") != -1) ||			/* NON-DOS code is deleted		*/
			         (strpos(inline,"$DOS-END") != -1)    )
			{
				write_log("WISP",'I',"DOSEND","End of NON-DOS code.");	/* log it				*/
				delete_code = 0;					/* clear the delete flag		*/
				proc_code = 0;
			}
			else if ((strpos(inline,"$WANG_ELSE") != -1) ||			/* Non-WANG code must be copied.	*/
			         (strpos(inline,"$WANG-ELSE") != -1)    )
			{
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				write_log("WISP",'I',"WANGEND","End of WANG-only code.");	/* log it			*/
				write_log("WISP",'I',"WANGELSE","Start of WANG-else code.");	/* log it			*/
				delete_code = 0;					/* clear the delete flag		*/
				copy_code = ELSE_COPY;					/* Set the copy flag.			*/
				put_char('\n');
			}

			return(GETNEXT);						/* get next line			*/

		}


		if (!copy_code)								/* not currently copying, so check	*/
		{
			if ((strpos(inline,"$WANG_CODE") != -1) ||			/* WANG only code must be deleted	*/
			    (strpos(inline,"$WANG-CODE") != -1)    )
			{
				write_log("WISP",'I',"WANGDELETE","WANG-only code being deleted.");	/* log it		*/
				delete_code = 1;					/* set the delete flag			*/
				return(GETNEXT);					/* get next line			*/
			}
			else if ((strpos(inline,"$COPY_CODE") != -1) ||			/* COPY code verbatim			*/
			         (strpos(inline,"$COPY-CODE") != -1)    )
			{
				write_log("WISP",'I',"COPYCODE","Code being copied verbatim.");	/* log it		*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = COPY_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if (lpi_cobol && ( (strpos(inline,"$LPI_CODE") != -1) ||	/* A request to copy LPI commented code	*/
 						(strpos(inline,"$LPI-CODE") != -1)    ))
			{
				write_log("WISP",'I',"LPICOPY","Start Copy of LPI code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = LPI_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if (acu_cobol && ((strpos(inline,"$ACU_CODE") != -1) ||	/* A request to copy ACU commented code	*/
 			     		       (strpos(inline,"$ACU-CODE") != -1)    ))
			{
				write_log("WISP",'I',"ACUCOPY","Start Copy of ACU code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = ACU_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if (aix_cobol && ((strpos(inline,"$AIX_CODE") != -1) ||	/* A request to copy AIX commented code	*/
 			    		       (strpos(inline,"$AIX-CODE") != -1)    ))
			{
				write_log("WISP",'I',"AIXCOPY","Start Copy of AIX code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = AIX_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if ((mf_cobol || dmf_cobol) &&
						((strpos(inline,"$MF_CODE") != -1) ||	/* A request to copy MF commented code	*/
 			            		 (strpos(inline,"$MF-CODE") != -1)    ))
			{
				write_log("WISP",'I',"MFCOPY","Start Copy of MF code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = MF_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if (unix_cobol && ((strpos(inline,"$UNIX_CODE") != -1) ||	/* A request to copy UNIX commented code*/
 			      		        (strpos(inline,"$UNIX-CODE") != -1)    ))
			{
				write_log("WISP",'I',"UNIXCOPY","Start Copy of UNIX code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = UNIX_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if (dos_cobol && ((strpos(inline,"$DOS_CODE") != -1) ||	/* A request to copy DOS commented code	*/
 			        	       (strpos(inline,"$DOS-CODE") != -1)    ))
			{
				write_log("WISP",'I',"DOSCOPY","Start Copy of DOS code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = DOS_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
			else if (vax_cobol && ((strpos(inline,"$VAX_CODE") != -1) ||	/* A request to copy VAX commented code	*/
 			     		       (strpos(inline,"$VAX-CODE") != -1)    ))
			{
				write_log("WISP",'I',"VAXCOPY","Start Copy of VAX code.");/* log it			*/
				if (strpos(inline,"PROCESS") != -1) proc_code = 1;	/* They want it processed.		*/
				copy_code = VAX_COPY;					/* set the copy flag			*/
				put_char('\n');
				return(GETNEXT);					/* get next line			*/
			}
		}
		else if (copy_code && !copy_in_copy_code)			/* If in $xxx_CODE but not from COPY stmt	*/
		{
											/* Look for an "ELSE" directive.	*/
			if ((strpos(inline,copy_else[copy_code]) != -1) ||		/* Check for the specific ELSE.		*/
			    (strpos(inline,copy__else[copy_code]) != -1)   )
			{
				write_log("WISP",'I',"ELSE","$xxx_ELSE found."); 	/* log it			*/
				delete_code = 1;					/* Flag for deletion.			*/
				copy_code = 0;						/* No more copying.			*/
			}
			
			else if (lpi_cobol && (copy_code == LPI_COPY) && 
				(strpos(inline,"$LPI_END") != -1 || strpos(inline,"$LPI-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of LPI code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
											/* A request to stop copying code	*/
			else if (acu_cobol && (copy_code == ACU_COPY) && 
				(strpos(inline,"$ACU_END") != -1 || strpos(inline,"$ACU-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of ACU code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
											/* A request to stop copying code	*/
			else if (aix_cobol && (copy_code == AIX_COPY) && 
				(strpos(inline,"$AIX_END") != -1 || strpos(inline,"$AIX-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of AIX code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
											/* A request to stop copying code	*/
			else if ((mf_cobol || dmf_cobol) && (copy_code == MF_COPY) && 
				(strpos(inline,"$MF_END") != -1 || strpos(inline,"$MF-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of MF code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
											/* A request to stop copying code	*/
			else if (unix_cobol && (copy_code == UNIX_COPY) && 
				(strpos(inline,"$UNIX_END") != -1 || strpos(inline,"$UNIX-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of UNIX code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
											/* A request to stop copying code	*/
			else if (dos_cobol && (copy_code == DOS_COPY) && 
				(strpos(inline,"$DOS_END") != -1 || strpos(inline,"$DOS-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of DOS code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
											/* A request to stop copying code	*/
			else if (vax_cobol && (copy_code == VAX_COPY) &&
				(strpos(inline,"$VAX_END") != -1 || strpos(inline,"$VAX-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of VAX code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
			else if ((copy_code == ELSE_COPY) && 
				(strpos(inline,"$WANG_END") != -1 || strpos(inline,"$WANG-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy of Non-Wang code.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
			else if ((copy_code == COPY_COPY) && 
				(strpos(inline,"$COPY_END") != -1 || strpos(inline,"$COPY-END") != -1) )
			{
				write_log("WISP",'I',"ENDCOPY","End Copy code verbatim.");	/* log it			*/
				put_char('\n');
				copy_code = 0;						/* Clear the copy flag			*/
				proc_code = 0;
			}
			else if (proc_code)					/* PROCESS flag is set.				*/
			{
				inline[6] = ' ';					/* remove the comment character		*/
				inline[72] = '\n';					/* Set up for truncation past 72.	*/
				inline[73] = '\0';
				return(PROCESSIT);					/* Process this line			*/
			}
			else							/* Copy to output NO processing.		*/
			{
				inline[6] = ' ';					/* remove the comment character		*/
				inline[72] = '\n';					/* Set up for truncation past 72.	*/
				inline[73] = '\0';
				if (division == WORKING_STORAGE_SECTION)		/* looking for PROCEDURE stuff		*/
				{
					if (strpos(inline," LINKAGE ") != -1)		/* found LINKAGE hidden, do screens	*/
					{
						write_log("WISP",'I',"FOUNDLINKAGE","Found LINKAGE in $xxx_CODE.");
						mark_link_sect = 1;
						gen_missing_sections();
					}
					else if (strpos(inline," PROCEDURE ") != -1)	/* found PROCEDURE hidden, do screens	*/
					{
						write_log("WISP",'I',"FOUNDPROC","Found PROCEDURE in $xxx_CODE.");
						mark_proc_div = 1;
						gen_missing_sections();
						division = PROCEDURE_DIVISION;
					}
				}
				put_line(inline);
			}
			return(GETNEXT);						/* get next line			*/
		}

		if (comments)								/* not copying, are comments ok?	*/
		{
			i = strlen(inline);
			if (inline[i-1] == '\n')
			{
				inline[i-1] = 0;					/* remove the ending newline (if any)	*/
			}
			put_com(inline);						/* Comments are just copies		*/
		}

		return(GETNEXT);							/* get next line			*/
}

uppercase(str)										/* Shift string to uppercase		*/
char *str;
{
	for(;*str;str++)  *str = toupper(*str);
}

