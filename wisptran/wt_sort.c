			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"

p_sort()
{
	char sortbuf[2048];
	char file1[40], file2[40];
	int i,fnum;

	strcpy(sortbuf,inline);							/* Put the line into the sort buffer.		*/

	ptype = get_param(o_parms[0]);						/* get the SORT verb.				*/
	ptype = get_param(file1);						/* Now the first files name.			*/

	if (ptype == 1) strcat(sortbuf,inline);					/* if a new line, add it on.			*/

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
				strcat(sortbuf,inline);				/* Add to the buffer.				*/
			}
		}

		if (!strcmp(o_parms[0],"USING"))				/* Is it a USING word?				*/
		{
			ptype = get_param(file1);				/* Get the file name.				*/
			if (ptype == 1) strcat(sortbuf,inline);
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
			if (ptype == 1) strcat(sortbuf,inline);
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
}


s_wfopen(fnum,fmode)
int fnum;
int fmode;
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
}




