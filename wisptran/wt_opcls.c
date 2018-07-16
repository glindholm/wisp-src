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
	char o_next[40];
	int i,done,is_crt,allow_write;
	int open_mode,syntax_ok;

	allow_write = 0;								/* No writers allowed.			*/
	o_count = 0;									/* no files yet				*/
	done = 0;									/* not done yet				*/
	open_mode = 0;
	syntax_ok = 0;									/* Not sure if the syntax is ok yet.	*/

	ptype = get_param(o_parms[0]);							/* skip the OPEN verb			*/
	stredt(inline,o_parms[0],"");							/* Remove the word.			*/

	o_parms[5][0] = '\0';								/* Clear the OPEN MODE string		*/
	o_parms[6][0] = '\0';								/* And the trailing MODE string		*/

	do
	{
		ptype = get_param(o_parms[0]);						/* get a word				*/

		is_crt = 0;
		for (i=0; i<crt_fcount; i++) if (!strcmp(o_parms[0],crt_file[i])) {is_crt = 1; cur_crt = i; syntax_ok = 1;}

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
					exit_wisp(-1);
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
						write_log("WISP",'E',"AUTOLOCK","File %s OPEN SHARED.",o_parms[0]);

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
					if (lpi_cobol||mf_aix)
						strcpy(o_allow[o_count]," WITH LOCK");	/* LPI needs it locked.			*/
					else if (vax_cobol && (prog_ftypes[i] & AUTOLOCK))
						strcpy(o_allow[o_count]," ");		/* Automatic record locking.		*/
					else
						strcpy(o_allow[o_count]," ALLOWING READERS");	/* Manual record locking.	*/
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
		if (!done) stredt(inline,o_parms[0],"");				/* Remove the last word.		*/
	} while (!done);								/* continue till done			*/
											/* If we ended on a keyword, then we set*/
	if (i == -1) ptype = 1;								/* up as if we ended on a new line.	*/

	if (o_count)									/* output the open statements		*/
	{
		for (i=0; i<o_count; i++)
		{
			int	gen_bitset;

			make_fld(o_parms[9],o_fnames[i],"S-");				/* create the status byte		*/

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
				put_line          ("           MOVE ZERO TO WISP-BIT-CLEAR, WISP-BIT-SET\n");

				if (isaproc)							/* Was the procedure flag set?	*/
				{
					put_line  ("           ADD WISP-PROC    TO WISP-BIT-SET,\n");
				}
				put_line  ("           CALL \"wsetstat\" USING WISP-BIT-SET, WISP-BIT-CLEAR,\n");
				write_line("                                 %s\n",o_parms[9]);
			}

			if (!(prog_ftypes[o_fref[i]] & NORESPECIFY))				/* Was the NORESPECIFY flag set?*/
			{									/* If RESPECIFY, then PERFORM it.*/
				put_line  ("           PERFORM WITH TEST AFTER UNTIL\n");
				write_line("               %s < \"10\"\n",prog_fstats[o_fref[i]]);
			}

			put_line  ("               MOVE \"OP\" TO WISP-DECLARATIVES-STATUS\n");	/* Signal an open.		*/
			write_line("               CALL \"wfopen3\" USING %s,\n",o_parms[9]);	/* start with the status byte	*/

									/* literal or null volume, pass the created field */
			if ((prog_vnames[o_fref[i]][0] == '\"') || (!prog_vnames[o_fref[i]][0]))
			{
				make_fld(o_parms[9],o_fnames[i],"V-");
				write_line("                                   %s,\n",o_parms[9]);
			}
			else
			{								/* use the programs own field		*/
				write_line("                                   %s,\n",prog_vnames[o_fref[i]]);
			}

										/* literal or null lib, pass the created field */
			if ((prog_lnames[o_fref[i]][0] == '\042') || (!prog_lnames[o_fref[i]][0]))
			{
				make_fld(o_parms[9],o_fnames[i],"L-");
				write_line("                                   %s,\n",o_parms[9]);
			}
			else
			{								/* use the programs own field		*/
				write_line("                                   %s,\n",prog_lnames[o_fref[i]]);
			}

										/* literal or null name, pass the created field */
			if ((prog_fnames[o_fref[i]][0] == '\042') || (!prog_fnames[o_fref[i]][0]))
			{
				make_fld(o_parms[9],o_fnames[i],"F-");
				write_line("                                   %s,\n",o_parms[9]);
			}
			else
			{								/* use the programs own field		*/
				write_line("                                   %s,\n",prog_fnames[o_fref[i]]);
			}

											/* always pass the created name		*/
			make_fld(o_parms[9],o_fnames[i],"N-");
			write_line        ("                                   %s,\n",o_parms[9]);
			put_line	  ("                                   WISP-APPLICATION-NAME,\n");
			make_fld(o_parms[9],o_fnames[i],"PR-");
			write_line        ("                                   %s,\n",o_parms[9]);

			write_line        ("                                   %s\n",open_mode_name[o_omode[i]]);

			if (strlen(o_fnames[i]) > 25 )					/* If the name too long put allowing on */
											/* next line.				*/
				strcpy(o_next,"\n                  ");
			else
				*o_next = 0;

			write_line("               OPEN %s %s%s%s\n",o_fmode[i],o_fnames[i],o_next,o_allow[i]);
			if (vax_cobol)
			{
				make_fld(o_parms[9],o_fnames[i],"S-");			/* create the status byte		*/
				write_line("               CALL \"wdellock\" USING %s,\n",o_parms[9]);
				make_fld(o_parms[9],o_fnames[i],"N-");
	                        write_line("                                     %s\n",o_parms[9]);
			}
			put_line  ("               MOVE SPACES TO WISP-DECLARATIVES-STATUS");	/* Signal an open.		*/

			if (!(prog_ftypes[o_fref[i]] & NORESPECIFY))			/* Was the NORESPECIFY flag set?	*/
			{
				put_line("\n           END-PERFORM");			/* No, End of the perform.		*/
			}

			if ((i == o_count-1) && (ptype == -1)) put_line(".\n");		/* Last file name needs a period?	*/
			else				     put_line(",\n");


		}
		isaproc = 0;								/* Clear the proc flag if needed.	*/
	}
	else										/* There were no files, must have been	*/
	{										/* a CRT file only. write a place holder*/
		if (ptype == -1)
			put_line("           CONTINUE.\n");
		else
			put_line("           CONTINUE,\n");
	}
	if (ptype == 1) hold_line();							/* ended on a new line, hold it		*/

	if (!syntax_ok)
	{
		write_log("WISP",'F',"OPNSYN","Syntax Error in OPEN statement, No file name.\n%s",inline);
		exit_wisp(-1);
	}
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
	stredt(&inline[7],o_parms[0],"");

	do
	{
		ptype = get_param(o_parms[0]);						/* get a word				*/

		is_crt = 0;
		for (i=0; i<crt_fcount; i++) 
			if (!strcmp(o_parms[0],crt_file[i])) 
			{
				is_crt = 1; 
				cur_crt = i;
			}

		if (is_crt)								/* delete CRT file closes		*/
		{
			write_log("WISP",'I',"DELCRTCLOSE","Fixed CLOSE of crt file, %s.",crt_file[cur_crt]);
			put_line("                 CALL \042vwang\042 USING VWANG-CLOSE-WS,\n");
		}
		else									/* see if it is a file name		*/
		{
			i = file_index(o_parms[0]);

			if (i != -1)							/* Found the file.			*/
			{
				if (c_count > MAX_FILES)				/* Exceeded file buffer.		*/
				{
					write_log("WISP",'F',"EXCFILECNT","Exceeded FILE count in CLOSE statement.");
					exit_wisp(-1);
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
			stredt(&inline[7],o_parms[0],"");
		}

		if (ptype == -1) 
		{
			stredt(&inline[7],".","");
			done = 1;
		}

	} while (!done);								/* continue till done			*/

	if (c_count)									/* output the open statements		*/
	{
		put_line("           MOVE \"CL\" TO WISP-DECLARATIVES-STATUS\n");

		for (i=0; i<c_count; i++)
		{
			write_line("           CLOSE %s",c_fnames[i]);
			if ((i == c_count-1) && (ptype == -1))				/* handle last file, with a period.	*/
			{
				if (c_flags[i] & PRINTER_FILE)				/* If it is a printer file, call wfclose*/
				{
					write_line("\n           CALL \"wfclose\" USING N-%s.\n",c_fnames[i]);
				}
				else
				{
					put_line(".\n");
				}
			}
			else
			{
				if (c_flags[i] & PRINTER_FILE)				/* If it is a printer file, call wfclose*/
				{
					write_line("\n           CALL \"wfclose\" USING N-%s\n",c_fnames[i]);
				}
				else
				{
					put_line(",\n");
				}
			}
		}
	}
	else										/* There were no files, must have been	*/
	{										/* a CRT file only. write a place holder*/
		if (ptype == -1)
			put_line("           CONTINUE.\n");
		else
			put_line("           CONTINUE,\n");
	}

	hold_line();
}

