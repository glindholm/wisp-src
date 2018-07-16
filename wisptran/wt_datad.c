			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wisp_datad.c
*/

#define EXT extern
#include "wisp.h"

static int did_fd = 0;
static int did_crt = 0;
static int crt_fd = 0;
static int fnum = -1;									/* Remember file number.		*/

char *strchr();

p_data()
{
	int i,is_sd, exit_valueof;
	char the_file[40];
	char t_mktemp[40];
	char tstr[80];
	short	fd_sd, changed_it;
	int	pos;
	char	bin_temp[8];
	int	notdone;


	if (!strcmp(parms[0],"SD"))							/* SD's are not crt files.		*/
	{
		crt_fd = 0;
	}
	else if (!strcmp(parms[0],"FD"))						/* If it's an FD, check to see if it is	*/
	{
		crt_fd = 0;								/* Assume is NOT a CRT file.		*/
		for (i=0; i<crt_fcount; i++)						/* A CRT file.				*/
		{
			if (!strcmp(crt_file[i],parms[1]))
			{
				crt_fd = 1;						/* Flag it.				*/
				cur_crt = i;
			}
		}
	}

	fd_sd = 0;									/* It's not a FD or SD			*/ 
	changed_it = 0;

	if ((!strcmp(parms[0],"SD") || !strcmp(parms[0],"FD")) && !crt_fd) 		/* it is a normal FD/SD			*/
	{
		fd_sd = 1;								/* It is an FD or SD			*/
		if (!strcmp(parms[0],"SD"))
		{
			write_log("WISP",'I',"PROCSD","Processing SD for %s.",parms[1]);
			is_sd = 1;							/* set the flag says this is an SD	*/
		}
		else
		{
			write_log("WISP",'I',"PROCFD","Processing FD for %s.",parms[1]);
			is_sd = 0;							/* clear the flag says this is an SD	*/
		}

		strcpy(the_file,parms[1]);						/* save the name of this file		*/
		stredt(the_file,".","");						/* Remove potential period.		*/

		fnum = file_index(the_file);

		if (fnum == -1)								/* Didn't find it.			*/
		{
			write_log("WISP",'F',"NOSELECT","FD for file %s, but no previous SELECT.\n",the_file);
			exit(-1);							/* Abort WISP.				*/
		}

		if (is_sd)
		{
			FSET(prog_ftypes[fnum],SORT_FILE);				/* flag it as a sort file		*/
			prog_sort++;							/* and count it				*/
		}

		prog_ftypes[fnum] |= HAD_FD;						/* Flag the FD statement.		*/

		write_line("       %s  %s\n",parms[0],parms[1]);			/* Write FD FILENAME			*/

		ptype = get_param(o_parms[0]);						/* Take the params off.			*/
		ptype = get_param(o_parms[0]);						/* Take the params off.			*/
		stredt(inline,parms[0],"  ");						/* Remove FD				*/
		stredt(inline,parms[1],"");						/* Remove filename			*/

		if (ptype != -1 )
			hold_line();							/* Re-parm it if needed			*/

		if (!is_sd)
		{
			make_fld(o_parms[9],prog_files[fnum],"N-");			/* start the value statement		*/
			if (lpi_cobol)
			{								/* Use FILE-ID for LPI COBOL.		*/
				write_line("           VALUE OF FILE-ID IS %s\n",o_parms[9]);
			}
			else if (vax_cobol)						/* Don't use ID for acu cobol		*/
			{								/* Use ID for VAX COBOL.		*/
				write_line("           VALUE OF ID IS %s\n",o_parms[9]);
			}
		}

		notdone=1;
		while (notdone)								/* till we see a period			*/
		{
			notdone=0;

			if (ptype == -1) break;

			get_line();							/* get a new input line			*/

			if (!strcmp(parms[0],"VALUE"))					/* it's a VALUE OF statement		*/
			{
				ptype = get_param(o_parms[0]);				/* "VALUE"				*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/
				ptype = get_param(o_parms[0]);				/* "OF"					*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				hold_line();

				exit_valueof = 0;
				while( ptype != -1 && !exit_valueof)
				{							/* loop through looking for file info	*/
					get_line();
					if (!strcmp(parms[0],"VALUE"))			/* Multiple VALUE keywords.		*/
					{
						ptype = get_param(o_parms[0]);		/* "VALUE"				*/
						stredt(inline,o_parms[0],"");		/* Edit it out of inline		*/
						ptype = get_param(o_parms[0]);		/* "OF"					*/
						stredt(inline,o_parms[0],"");		/* Edit it out of inline		*/

						hold_line();				/* Re-parm the line.			*/
						get_line();
					}

					if (!strcmp(parms[0],"FILENAME") ||
					    !strcmp(parms[0],"LIBRARY") ||
					    !strcmp(parms[0],"VOLUME") ||
					    !strcmp(parms[0],"SPACE") ||
					    !strcmp(parms[0],"SPACES") ||
					    !strcmp(parms[0],"POSITION") ||
					    !strcmp(parms[0],"INDEX") ||
					    !strcmp(parms[0],"RECOVERY-BLOCKS") ||
					    !strcmp(parms[0],"RECOVERY-STATUS") ||
					    !strcmp(parms[0],"DATABASE-NAME") ||
					    (!strcmp(parms[0],"DATA") && !strcmp(parms[1],"AREA")) ||
					    !strcmp(parms[0],"PRINT-CLASS") ||
					    !strcmp(parms[0],"PRINT-FORM") ||
					    !strcmp(parms[0],"FILE-CLASS") 
					   )
					{
						ptype = get_param(o_parms[1]);		/* get keyword	(save it)		*/
						stredt(&inline[7],o_parms[1],"");	/* Edit it out of inline		*/

						ptype = get_param(o_parms[0]);		/* get next token.			*/
						stredt(&inline[7],o_parms[0],"");	/* Edit it out of inline		*/
						if (!strcmp(parms[0],"INDEX") || !strcmp(parms[0],"DATA") )
						{
							if (!strcmp(o_parms[0],"AREA"))		/* Skip over AREA.		*/
							{
								ptype = get_param(o_parms[0]);	/* Get next			*/
								stredt(&inline[7],o_parms[0],"");/* Edit it out of inline	*/
							}

						}

						if (!strcmp(o_parms[0],"IS"))		/* Skip over IS.			*/
						{
							ptype = get_param(o_parms[0]);	/* Get next token			*/
							stredt(&inline[7],o_parms[0],"");/* Edit it out of inline		*/
						}
						hold_line();

						if (!strcmp(o_parms[1],"FILENAME"))
						{
							write_log("WISP",'I',"FILENAME","  File name is %s",o_parms[0]);

							strcpy(prog_fnames[fnum],o_parms[0]);	/* FD overrides SELECT specs	*/
							stredt(prog_fnames[fnum],".","");	/* remove periods		*/
						}
						else if (!strcmp(o_parms[1],"LIBRARY"))		/* Its the library.		*/
						{
							write_log("WISP",'I',"LIBRARY","  Library is %s.",o_parms[0]);

							strcpy(prog_lnames[fnum],o_parms[0]);	/* save the library name	*/
							stredt(prog_lnames[fnum],".","");	/* remove periods		*/
						}
						else if (!strcmp(o_parms[1],"VOLUME"))		/* its the volume		*/
						{
							write_log("WISP",'I',"VOLUME","  Volume is %s.",o_parms[0]);
	
							strcpy(prog_vnames[fnum],o_parms[0]);		/* save the volume name	*/
							stredt(prog_vnames[fnum],".","");		/* remove periods	*/
						}
					}
					else
					{
						hold_line();
						exit_valueof = 1;
					}

				} /* End while  doing VALUE OF clauses */

				if (ptype != -1)
				{
					notdone = 1;
				}

			}
			else if (!strcmp(parms[0],"RECORD"))				/* it's a RECORD CONTAINS statement	*/
			{
				char	recbuff[300];
				int	varying, tempint;

			/*	RECORD contains                 INT-1  [TO INT-2] [COMPRESSED] characters 			*/
			/*	RECORD is VARYING in size [from INT-1] [TO INT-2] [COMPRESSED] characters [DEPENDING on DATA-1]	*/

				recbuff[0] = '\0';

				write_log("WISP",'I',"RECORDCONTAINS","Processing RECORD CONTAINS from FD.\n");

				ptype = extract_param(o_parms[0]);			/* Get the "RECORD"			*/
				add2buff(recbuff,"RECORD",12,0);			/* Add RECORD				*/

				ptype = extract_param(o_parms[0]);
				if ( 0 == strcmp(o_parms[0],"CONTAINS") )
				{
					add2buff(recbuff,o_parms[0],19,0);		/* Add CONTAINS				*/
					ptype = extract_param(o_parms[0]);
					add2buff(recbuff,o_parms[0],19,0);		/* Add INT-1				*/
					varying = 0;
				}
				else if ( 0 == strcmp(o_parms[0],"IS") )
				{
					add2buff(recbuff,o_parms[0],19,0);		/* Add IS				*/
					ptype = extract_param(o_parms[0]);
					add2buff(recbuff,o_parms[0],19,0);		/* Add VARYING				*/
					varying = 1;
				}
				else if ( 0 == strcmp(o_parms[0],"VARYING") )
				{
					add2buff(recbuff,o_parms[0],19,0);		/* Add VARYING				*/
					varying = 1;
				}
				else
				{
					add2buff(recbuff,o_parms[0],19,0);		/* Add INT-1				*/
					varying = 0;
				}

				if (ptype != -1) next_param(o_parms[0]);

				if (ptype != -1 && varying)
				{
					if ( 0 == strcmp(o_parms[0],"IN") )
					{
						ptype = extract_param(o_parms[0]);
						add2buff(recbuff,o_parms[0],19,0);	/* Add IN				*/
						if (ptype != -1) next_param(o_parms[0]);
					}
					if ( ptype != -1 && 0 == strcmp(o_parms[0],"SIZE") )
					{
						ptype = extract_param(o_parms[0]);
						add2buff(recbuff,o_parms[0],19,0);	/* Add SIZE				*/
						if (ptype != -1) next_param(o_parms[0]);
					}
					if ( ptype != -1 && 1 == sscanf(o_parms[0],"%d",&tempint) && tempint > 0 )
					{
						ptype = extract_param(o_parms[0]);
						add2buff(recbuff,o_parms[0],19,0);	/* Add INT-1				*/
						if (ptype != -1) next_param(o_parms[0]);
					}
					if ( ptype != -1 && 0 == strcmp(o_parms[0],"FROM") )
					{
						ptype = extract_param(o_parms[0]);
						add2buff(recbuff,o_parms[0],19,0);	/* Add FROM				*/
						ptype = extract_param(o_parms[0]);
						add2buff(recbuff,o_parms[0],19,0);	/* Add INT-1				*/
						if (ptype != -1) next_param(o_parms[0]);
					}
				}

				if (ptype != -1 && !strcmp(o_parms[0],"TO"))
				{
					ptype = extract_param(o_parms[0]);
					add2buff(recbuff,o_parms[0],19,0);		/* Add TO				*/
					ptype = extract_param(o_parms[0]);
					add2buff(recbuff,o_parms[0],19,0);		/* Add INT-2				*/
					if (ptype != -1) next_param(o_parms[0]);
				}
				if (ptype != -1 && !strcmp(o_parms[0],"COMPRESSED"))
				{
					ptype = extract_param(o_parms[0]);		/* Remove COMPRESSED			*/
					if (ptype != -1) next_param(o_parms[0]);
				}
				if (ptype != -1 && !strcmp(o_parms[0],"CHARACTERS"))
				{
					ptype = extract_param(o_parms[0]);
					add2buff(recbuff,o_parms[0],19,0);		/* Add CHARACTERS			*/
					if (ptype != -1) next_param(o_parms[0]);
				}
				if (ptype != -1 && !strcmp(o_parms[0],"DEPENDING"))
				{
					ptype = extract_param(o_parms[0]);
					add2buff(recbuff,o_parms[0],19,0);		/* Add DEPENDING			*/
					ptype = extract_param(o_parms[0]);
					if (!strcmp(o_parms[0],"ON"))
					{
						add2buff(recbuff,o_parms[0],19,0);	/* Add ON				*/
						ptype = extract_param(o_parms[0]);
					}
					add2buff(recbuff,o_parms[0],19,0);		/* Add DATA-1				*/
				}

				if (!delrecon)  put_line(recbuff);
				else		put_line("      *    RECORD CONTAINS clause deleted");

				if (ptype == -1)
				{
					stredt(inline,"."," ");
					if (delrecon) put_line("\n           ");
					put_line(" .\n");
				}
				else
				{
					put_line(" \n");	
					notdone=1;
				}
				hold_line();
			}
			else if (!strcmp(parms[0],"BLOCK"))				/* it's a BLOCK CONTAINS statement	*/
			{
			/* BLOCK contains [int TO] int {RECORDS|CHARACTERS} */

				write_log("WISP",'I',"BLOCKCONTAINS","Removing BLOCK CONTAINS from FD.\n");

				ptype = get_param(o_parms[0]);				/* Get the "BLOCK"			*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				ptype = get_param(o_parms[0]);				/* Get the "CONTAINS" or size		*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				if (!strcmp(o_parms[0],"CONTAINS"))			/* Was it contains?			*/
				{
					ptype = get_param(o_parms[0]);			/* Get the size				*/
					stredt(inline,o_parms[0],"");			/* Edit it out of inline		*/
				}

				ptype = get_param(o_parms[0]);				/* Get the "TO" or RECORD/CHARACTERS	*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				if (!strcmp(o_parms[0],"TO"))				/* Was it "TO"?				*/
				{
					ptype = get_param(o_parms[0]);			/* Get the size				*/
					stredt(inline,o_parms[0],"");			/* Edit it out of inline		*/

					ptype = get_param(o_parms[0]);			/* Get the RECORD/CHARACTERS		*/
					stredt(inline,o_parms[0],"");			/* Edit it out of inline		*/
				}

				if (ptype != -1)
					notdone=1;

				hold_line();
			}
			else if (!strcmp(parms[0],"LABEL"))			/* LABEL RECORD/S IS/ARE STANDARD/OMITTED	*/
			{
				write_log("WISP",'I',"LABELRECORD","Removing LABEL RECORD from FD.\n");

				ptype = get_param(o_parms[0]);				/* Get the "LABEL"			*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				ptype = get_param(o_parms[0]);				/* Get the "RECORD/S" 			*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				ptype = get_param(o_parms[0]);				/* Get the "IS/ARE" or			*/
				stredt(inline,o_parms[0],"");				/* Edit it out of inline		*/

				if (!strcmp(o_parms[0],"IS") || !strcmp(o_parms[0],"ARE"))
				{
					ptype = get_param(o_parms[0]);			/* Get the "STANDARD/OMITTED"		*/
					stredt(inline,o_parms[0],"");			/* Edit it out of inline		*/
				}

				if (ptype != -1)
					notdone=1;

				hold_line();
			}
			else
			{
				put_line(inline);					/* write out the line			*/
				if ( strpos(inline,".") == -1 )
					notdone=1;
			}
		} 

		write_log("WISP",'I',"FDFINISH","Finished FD scan, terminator line was;\n%s",inline);
	}
	else if (!strcmp(parms[0],"FD") && crt_fd) 					/* it is the FD for the crt		*/
	{
		fd_sd = 1;								/* It is an FD or SD			*/
		crt_prec[cur_crt] = crt_count;						/* And the screen too.			*/

		write_log("WISP",'I',"CRTFD","processing FD for a crt file.");
		skip_last();								/* skip the fd definition		*/


		if (!did_crt)
		{
			strcpy(t_mktemp,CRT_FILE);
			strcpy(crt_name,mktemp(t_mktemp));				/* make a unique name			*/
#ifdef VMS
			strcat(crt_name,".SCR;");
#else
			strcat(crt_name,".scr");
#endif
			crt_temp_file = fopen(crt_name,"w");				/* open a scratch file			*/
			did_crt = 1;
		}
		else
		{
			crt_temp_file = fopen(crt_name,"a");				/* open a scratch file			*/
		}

		write_line("       FD  %s.\n",crt_file[cur_crt]);			/* include FD for dummy CRT file	*/
		write_line("       01  WISP-CRT-FILE-RECORD-%d  PIC X.\n",cur_crt);

		do_crtfile();								/* process the crt file			*/

		fclose(crt_temp_file);

		write_log("WISP",'I',"FINISHCRT","Finished processing crt file, current line is;\n  %s",inline);
	}

	if ( !fd_sd && !has_lit )
	{
		editcomp();							/* Edit COMP to packdec in inline.		*/
	}

	if (!fd_sd && !has_lit && chk_binary())
	{
		stredt(inline," USAGE "," ");						/* Try to remove USAGE IS		*/
		stredt(inline," IS "," ");
		if  ((stredt(inline," BINARY."," $COMP .")) == -1) 		/* Replace the BINARY string		*/
		{
			if  ((stredt(inline," BINARY "," $COMP ")) == -1)
			{
				if  ((stredt(inline," BINARY\n"," $COMP \n")) == -1)
				{
					write_log("WISP",'I',"BINARYERR","Error in BINARY statement. input line is\n<%s>",inline);
				}
			}
		}

		if (strpos(inline," PIC") != -1)					/* It already has a PIC.		*/
		{
			strcpy(bin_temp,bin4_type);
			stredt(inline,"$COMP",bin_temp);				/* Just keep the COMP.			*/
		}
		else
		{									/* Generate the PIC also.		*/
			strcpy(bin_temp,bin2_type);
			sprintf(tstr,"%s PIC S9(4)",bin_temp);
			stredt(inline,"$COMP",tstr);
		}

		write_log("WISP",'I',"REPLBINARY","Replaced BINARY with COMP PIC S9(4) in FILE SECTION.");

		if (wsqueeze(inline,72) == -1)						/* First try to squeeze it.		*/
		{
			char	tmpstr[20], tmpstr2[40];
			sprintf(tmpstr," %s ",bin_temp);
			sprintf(tmpstr2,"\n           %s ",bin_temp);
			stredt(inline,tmpstr,tmpstr2);					/* too long, break it			*/
		}

	}

	if ( !fd_sd && !has_lit && kl_count && ((pos = strpos(inline," REDEFINES")) != -1) )
	{
		char	tmp[40];
		char	tmp2[40];
		char	tmp3[40];

		for(pos += 10;inline[pos] == ' '; pos++) {};
		tmp[0] = inline[pos-1];
		for(i=1;inline[pos] != ' ' && inline[pos] != '.' && inline[pos] != '\n';i++, pos++) {tmp[i] = inline[pos];};
		tmp[i++] = inline[pos];
		tmp[i] = '\0';
		if (strlen(tmp) >2)
		{
			strcpy(tmp2, &tmp[1]);
			tmp2[ strlen(tmp)-2] = '\0';
			if ( key_name(tmp2,0) != -1 )
			{
				tmp3[0] = tmp[0];
				strcpy(&tmp3[1],tmp2);
				strcat(tmp3,&tmp[strlen(tmp)-1]);
				stredt(inline,tmp,tmp3);
			}
		}
	}

	if (!fd_sd && 0 == strcmp(parms[0],"01") )					/* Save a list of record names.		*/
	{
		char	recname[40];

		strcpy(recname,parms[1]);
		if (strchr(recname,'.')) *(strchr(recname,'.')) = 0;			/* Null the period			*/
		strcpy(record_list[record_cnt].name,recname);
		record_list[record_cnt].fd = fnum;
		record_cnt += 1;
		if ( record_cnt >= MAX_RECORD_CNT )
			write_log("WISP",'F',"MAXRECCNT","MAX_RECORD_CNT exceeded.\n");
		
	}

	if (!fd_sd && kl_count && isdigit(parms[0][0]))					/* Better see if it's a field to fix.	*/
	{
		if (fix_key()) return(0);						/* Try to fix a key field.		*/
	}

	if (!fd_sd)
	{
		if (!fdfiller) fix_filler(inline);
		put_line(inline);							/* For now, just copy it.		*/
	}

}

fix_key()
{
	char okey[40],nkey[40],tstr[40];
	char level[8],old_pic[40],new_pic[40],val[40],nlevel[5];
	int indent,i,j,kidx;

	strcpy(nkey,parms[1]);

	kidx = key_name(nkey,0);							/* Make a new name if necessary.	*/
	if (kidx == -1) return(0);							/* Not really a key, just finish up.	*/

	put_line("      *%WISP-I-MOD Key field modified by WISP\n");
	
	ptype = get_param(level);							/* Start by fetching the level.		*/
	indent = get_ppos();								/* Get the column position		*/
	memset(tstr,' ',indent);
	tstr[indent] = '\0';

	if ( key_list[kidx].type == KL_COMP )
	{
		write_line("%s%s  %.33s.\n",tstr,level,nkey);				/* Now write the new definition.	*/
		i = level[0] - '0';							/* Calc the level number		*/
		if ( isdigit(level[1]) ) i = i*10 + (level[1] - '0'); 
		sprintf(nlevel,"%02d",i+1);						/* New level is one greater		*/
		memcpy(&inline[indent],nlevel,2);					/* Shove the new level back into inline	*/
		write_line("        %.68s",&inline[6]);					/* Write out the original		*/
		return(1);
	}


#define DO_PIC		0
#define DO_LEADING	8
#define DO_SEPARATE	16
#define DO_VALUE	25

	val[0] = '\0';

	ptype = get_param(okey);							/* And the old keys name.		*/

	do
	{
		ptype = get_param(tstr);						/* Get the next word.			*/
/*			    012345678901234567890123456789		*/
		i = strpos("PICTURE LEADING SEPARATE VALUE",tstr);
		switch(i)
		{
			case DO_PIC:
			{
				ptype = get_kw(old_pic);
				break;
			}

			case DO_LEADING:
			case DO_SEPARATE:						/* Just lose this stuff.		*/
			{
				break;
			}

			case DO_VALUE:
			{
				ptype = get_kw(val);					/* Get the value clause.		*/
				break;
			}
			default:
			{
				break;
			}
		}

	} while (ptype != -1);

	memset(tstr,' ',indent);
	tstr[indent] = '\0';

	if ( key_list[kidx].type == KL_LEAD )
	{
		i = pic_size(old_pic);

		if (old_pic[0] == 'S')  sprintf(new_pic,"S9(%d)",i);			/* Does it have a sign?			*/
		else			sprintf(new_pic,"9(%d)",i);			/* Make the new pic.			*/

		j = i + 2;								/* Add 2				*/
		j = j / 2;								/* Now divide by 2.			*/
											/* This is the actual size.		*/
		j = i - j;								/* The difference is...			*/
		if (old_pic[0] == 'S') j++;						/* include the sign.			*/
	
		write_line("%s%s  %-33s PIC %s\n",tstr,level,nkey,new_pic);		/* Now write the new definition.	*/
		put_line(tstr);
		write_line("    %s.\n",packdec);
	
		write_line("%s%s  %-33s REDEFINES\n",tstr,level,okey);			/* Redefine it.				*/
		write_line("%s    %-33s PIC %s\n",tstr,nkey,old_pic);
		put_line(tstr);
		write_line("    %s.\n",packdec);
	
		write_line("%s%s  FILLER                   PIC X(%d).\n",tstr,level,j);	/* Pad it.				*/
	}


	return(1);									/* Signal we did it.			*/
}

int fd_record(recname)
char	*recname;
{
	int	i;

	for (i=0; i<record_cnt; i++)
	{
		if ( strcmp(recname,record_list[i].name) == 0 ) 
			return(record_list[i].fd);
	}
	return (-1);
}

editcomp()										/* Edit COMP to packdec	in inline	*/
{
	if (  (strcmp("COMP",packdec) != 0) && (strpos(inline," COMP") != -1) )
	{
		char	tmpstr[20];
		short	chg;

		chg = 0;

		if (!chg)
		{
			sprintf(tmpstr," %s ",packdec);
			if ( stredt(inline," COMP ",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s,",packdec);
			if ( stredt(inline," COMP,",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s.",packdec);
			if ( stredt(inline," COMP.",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s\n",packdec);
			if ( stredt(inline," COMP\n",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s ",packdec);
			if ( stredt(inline," COMPUTATIONAL ",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s,",packdec);
			if ( stredt(inline," COMPUTATIONAL,",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s.",packdec);
			if ( stredt(inline," COMPUTATIONAL.",tmpstr ) != -1) chg = 1;
		}
		if (!chg)
		{
			sprintf(tmpstr," %s\n",packdec);
			if ( stredt(inline," COMPUTATIONAL\n",tmpstr ) != -1) chg = 1;
		}


		if (wsqueeze(inline,72) == -1)						/* First try to squeeze it.		*/
		{
			char	tmpstr2[40];
			sprintf(tmpstr2,"\n          %s",tmpstr);
			stredt(inline,tmpstr,tmpstr2);					/* Too long, break it.			*/
		}

	}

}
