static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"
#include "crt.h"
#include "cobfiles.h"
#include "directiv.h"

#include "wcommon.h"

			/* Process OPEN statements for possible multiple file names amd CRT file opens	*/

/*
	OPEN { INPUT         } filename-1 [filename-2 ...]  ....
	     { OUTPUT        }
	     { I-O           }
	     { SHARED        }
	     { EXTEND        }
	     { SPECIAL-INPUT }


	Example:

	OPEN INPUT A, B, C,
             OUTPUT D, E.
*/


static char *open_mode_name[] = { "",	"WFOPEN-INPUT",
					"WFOPEN-SHARED",
					"WFOPEN-OUTPUT",
					"WFOPEN-EXTEND",
					"WFOPEN-SPECIAL-INPUT",
					"WFOPEN-I-O",
					"WFOPEN-SORT"};

p_open()
{
	char o_fnames[MAX_FILES][40];							/* a place to save the file names	*/
	char o_fmode[MAX_FILES][40];							/* And the open mode for the file.	*/
	char o_allow[MAX_FILES][40];							/* Also if ALLOWING needed.		*/
	int  o_fref[MAX_FILES];								/* ref nums of the files		*/
	int  o_write[MAX_FILES];							/* Allow writers.			*/
	int  o_omode[MAX_FILES];
	int  o_count;									/* a count of the file names saved	*/
	int i,done,is_crt,allow_write;
	int open_mode,syntax_ok;

	allow_write = 0;								/* No writers allowed.			*/
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

		i = 0;									/* It's not a file name yet		*/
		if (!strcmp(o_parms[0],"SHARED"))					/* handle SHARED phrase	    		*/
		{
			allow_write = 1;						/* Allow writers.			*/
			open_mode = OPEN_SHARED;
		}									/* Handle other modes			*/
		else if (!strcmp(o_parms[0],"SPECIAL-INPUT"))				/* handle SPECIAL-INPUT phrase		*/
		{
			allow_write = 1;						/* Allow writers.			*/
			open_mode = OPEN_SPECIAL_INPUT;
		}									/* Handle other modes			*/
		else if (!strcmp(o_parms[0],"I-O"))					/* handle I-O phrase			*/
		{
			allow_write = 0;						/* Disallow writers.			*/
			open_mode = OPEN_I_O;
		}									/* Handle other modes			*/
		else if (!strcmp(o_parms[0],"INPUT"))
		{
			allow_write = 0;						/* Disallow writers.			*/
			open_mode = OPEN_INPUT;
		}
		else if (!strcmp(o_parms[0],"OUTPUT"))
		{
			allow_write = 0;						/* Disallow writers.			*/
			open_mode = OPEN_OUTPUT;
		}
		else if (!strcmp(o_parms[0],"EXTEND"))
		{
			allow_write = 0;						/* Allow writers.			*/
			open_mode = OPEN_EXTEND;
		}
		else if (is_crt)							/* delete CRT file opens		*/
		{
			write_log("WISP",'I',"DELCRTOPEN","Removed OPEN of crt file, %s.",crt_file[cur_crt]);
		}
		else									/* see if it is a file name		*/
		{
			i = file_index(o_parms[0]);

			if (i != -1)
			{
				syntax_ok = 1;						/* Syntax is ok, there was a file.	*/
				if (o_count > MAX_FILES)				/* Exceeded file buffer.		*/
				{
					write_log("WISP",'F',"EXCFILECNT","Exceeded FILE count in OPEN statement.");
					exit_wisp(EXIT_WITH_ERR);
				}

				o_omode[o_count] = open_mode;
				o_fref[o_count] = i;					/* remember which one it is		*/
				prog_ref[i]++;						/* Say it was used.			*/

				o_write[o_count] = allow_write;				/* Save writers flag.			*/

				if (open_mode == OPEN_SHARED)				/* handle SHARED phrase	    		*/
				{							/* If Sequential, change to EXTEND	*/
					if (prog_ftypes[i] & (SEQ_DYN + SEQ_FILE))
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

					if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
					{
						write_log("WISP",'W',"AUTOLOCK","File %s OPEN SHARED using AUTOLOCK.",o_parms[0]);
					}

				}							/* Handle other modes			*/
				else if (open_mode == OPEN_SPECIAL_INPUT)		/* handle SPECIAL-INPUT phrase		*/
				{
					strcpy(o_fmode[o_count],"INPUT");		/* CHANGE IT				*/
					if (lpi_cobol||mf_aix)
						o_allow[o_count][0] = '\0';		/* LPI don't work like this.		*/
					else if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING ALL");	/* And the trailing mode	*/
				}							/* Handle other modes			*/
				else if (open_mode == OPEN_I_O)				/* handle I-O phrase			*/
				{
					strcpy(o_fmode[o_count],"I-O");			/* Set to I-O				*/

					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if ((vax_cobol || acu_cobol) && (prog_ftypes[i] & OPENIOX_FILE))
						strcpy(o_allow[o_count]," ALLOWING NO OTHERS");	/* OPEN I-O exclusive		*/
					else if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING READERS");	/* Manual record locking	*/
											/* Vax needs readers - READFDR		*/
				}							/* Handle other modes			*/
				else if (open_mode == OPEN_INPUT)
				{
					strcpy(o_fmode[o_count],"INPUT");		/* save the mode			*/
					o_allow[o_count][0] = 0;			/* Wang/VAX/LPI allow other readers	*/
					if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else if (vax_cobol || acu_cobol)		/* Manual record locking		*/
						strcpy(o_allow[o_count], " ALLOWING READERS");
				}
				else if (open_mode == OPEN_OUTPUT)
				{
					strcpy(o_fmode[o_count],"OUTPUT");		/* save the mode			*/
					strcpy(o_allow[o_count]," ");			/* Automatic record locking.		*/
					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
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
				else if (open_mode == OPEN_EXTEND)
				{
					strcpy(o_fmode[o_count],"EXTEND");		/* save the mode			*/
					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING READERS");	/* Manual record locking.	*/
				}

				strcpy(o_fnames[o_count],o_parms[0]);			/* save it				*/
				o_count++;
			}
		}									/* not found, i = prog_cnt		*/
		if ((ptype == -1) || (i == -1)) done = 1;				/* period or no file name means done.	*/
		if (!done) stredt(linein,o_parms[0],"");				/* Remove the last word.		*/
	} while (!done);								/* continue till done			*/
											/* If we ended on a keyword, then we set*/
	if (i == -1) ptype = 1;								/* up as if we ended on a new line.	*/

	if (o_count)									/* output the open statements		*/
	{
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

			if (x4dbfile && prog_ftypes[o_fref[i]] & INDEXED_FILE)
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
				preclose_locking(i);					/* Add DMS locking logic		*/
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
**	History:
**	$Log: wt_opcls.c,v $
**	Revision 1.13  1997-09-15 11:36:54-04  gsl
**	Removed CLOSE workstation for native screens
**
**	Revision 1.12  1997-09-12 14:03:42-04  gsl
**	change native warning
**
**	Revision 1.11  1997-09-12 13:28:02-04  gsl
**	Native screens warning for WS CLOSE
**
**	Revision 1.10  1996-08-30 21:56:22-04  gsl
**	drcs update
**
**
**
*/
