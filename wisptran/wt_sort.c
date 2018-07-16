#define EXT extern
#include "wisp.h"

p_sort()
{
	char sortbuf[2048];
	char file1[40], file2[40];
	int i,fnum;

	strcpy(sortbuf,inline);							/* Put the line into the sort buffer.		*/

	ptype = get_param(o_parms[0]);						/* get the SORT verb.				*/
	ptype = get_param(file1);						/* Now the first files name.			*/

	if (ptype == 1) strcat(sortbuf,inline);					/* if a new line, add it on.			*/

	for (fnum=0; fnum<prog_cnt; fnum++)					/* look for the file spec			*/
	{
		if (!strcmp(prog_files[fnum],file1)) break;			/* found it					*/
	}

	if (fnum == prog_cnt) write_log("WISP",'F',"SORTNOTFND","SORT file %s not found.",file1);

	s_wfopen(fnum,1);							/* Generate a WFOPEN call.	(output)	*/

	do
	{
		ptype = get_param(o_parms[0]);					/* Next word to parse.				*/
		if (g_newline)
		{
			if (keyword(o_parms[0],proc_keywords))			/* If new line and a keyword.			*/
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
			for (fnum=0; fnum<prog_cnt; fnum++)			/* look for the file spec			*/
			{
				if (!strcmp(prog_files[fnum],file1)) break;	/* found it					*/
			}
			if (fnum == prog_cnt) write_log("WISP",'F',"SORTNOTFND","SORT file %s not found.",file1);
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


	put_line("           MOVE \"SO\" TO WISP-DECLARATIVES-STATUS\n");
	put_line(sortbuf);
}


s_wfopen(fnum,fmode)
int fnum;
int fmode;
{
	make_fld(o_parms[9],prog_files[fnum],"S-");				/* create the status byte			*/

#ifdef OLD
	put_line          ("           MOVE WISP-S-OUT TO WISP-LONGWORD,\n");	/* set sort and output bits			*/

	if (fmode == 1)								/* open for output only				*/
	{
		put_line  ("           CALL \"lbit_on\" USING WISP-LONGWORD,\n");/* set them in the status byte			*/
		write_line("                               %s\n",o_parms[9]);
	}
	else
	{
		put_line  ("           CALL \"lbit_off\" USING WISP-LONGWORD,\n");/* clear it in the status byte		*/
		write_line("                               %s\n",o_parms[9]);
	}

	put_line  	  ("           MOVE WISP-I-O TO WISP-LONGWORD,\n");	/* set I-O bit					*/
	put_line	  ("           CALL \"lbit_off\" USING WISP-LONGWORD,\n");/* clear it in the status byte		*/
	write_line	  ("                               %s\n",o_parms[9]);
#endif

	put_line  	  ("           CALL \"wfopen3\" USING\n");
	write_line	  ("                         %s,\n",o_parms[9]);	/* start with the status byte			*/

										/* literal or null vol, pass the created field */
	if ((prog_vnames[fnum][0] == '\042') || (!prog_vnames[fnum][0]))
	{
		make_fld(o_parms[9],prog_files[fnum],"V-");
		write_line("                         %s,\n",o_parms[9]);
	}
	else
	{									/* use the programs own field			*/
		write_line("                         %s,\n",prog_vnames[fnum]);
	}

										/* literal or null lib, pass the created field 	*/
	if ((prog_lnames[fnum][0] == '\042') || (!prog_lnames[fnum][0]))
	{
		make_fld(o_parms[9],prog_files[fnum],"L-");
		write_line("                         %s,\n",o_parms[9]);
	}
	else
	{									/* use the programs own field			*/
		write_line("                         %s,\n",prog_lnames[fnum]);
	}

										/* literal or null name, pass the created field */
	if ((prog_fnames[fnum][0] == '\042') || (!prog_fnames[fnum][0]))
	{
		make_fld(o_parms[9],prog_files[fnum],"F-");
		write_line("                         %s,\n",o_parms[9]);
	}
	else
	{									/* use the programs own field			*/
		write_line("                         %s,\n",prog_fnames[fnum]);
	}

										/* always pass the created name			*/
	make_fld(o_parms[9],prog_files[fnum],"N-");
	write_line("                         %s,\n",o_parms[9]);

	put_line  ("                         WISP-APPLICATION-NAME,\n");

	make_fld(o_parms[9],prog_files[fnum],"PR-");
	write_line("                         %s,\n",o_parms[9]);		/* And the prname.				*/
	write_line("                         %s\n\n", (fmode==1) ? "WFOPEN-SORT" : "WFOPEN-INPUT");
}




