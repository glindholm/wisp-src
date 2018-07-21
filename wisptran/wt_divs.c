/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/*
**	wisp_divs.c
*/

#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "cobfiles.h"
#include "directiv.h"


extern	char	decimal_is;


int check_decl(void)									/* Examine the tables of PERFORMs in	*/
											/* the DECLARATIVES division against the*/
{											/* Table of paragraph-names that were in*/
	int i,j;									/* the DECLARATIVES. This way we can	*/
	int unknown;									/* determine what PERFORMs were para's	*/
											/* that are in the procedure division.	*/

	unknown = decl_performs_cnt;							/* Remember how many are unaccounted for */

	for (i=0; i<decl_performs_cnt; i++)						/* for each PERFORM...			*/
	{
		for (j=0; j<decl_paras_cnt; j++)					/* examine each PARAGRAPH-NAME		*/
		{
			if (!strcmp(decl_performs[i],decl_paras[j]))			/* to see if they match. if they do, we	*/
			{								/* can delete the PERFORM from the list	*/
				decl_performs[i][0] ='\0';				/* of unknown performs.			*/
				unknown--;
				j = decl_paras_cnt;					/* Stop looking for this one.		*/
			}
		}
	}

	if (unknown)									/* If there are new unknowns, record em.*/
	{
		write_log("WISP",'I',"UNRESPERF","Performs found in the DECLARATIVES that were unresolved:");
		new_para();								/* Generate a new .PAR file.		*/
	}
	decl_performs_cnt = 0;								/* reset table for use in proc div.	*/
	return 0;
}


int new_para()										/* Generate a new .PAR file with a list	*/
{											/* of screens and paragraph names which	*/
	int i;										/* are referenced by the declaratives,	*/
	FILE *the_file;									/* but in the procedure division.	*/

	for (i=0; i<decl_performs_cnt; i++)					/* decl_performs_cnt is the count of how many	*/
	{										/* PERFORM's were in DECLARATIVES.	*/
		if (decl_performs[i][0])						/* If it's not a null string (resolved)	*/
		{									/* Then it must be an unresolved PERFORM*/
			write_log("WISP",'W',"RELOCATE","Paragraph %s not found in DECLARATIVES, will relocate.",decl_performs[i]);
			strcpy(proc_paras[proc_paras_cnt++],decl_performs[i]);		/* And add it to the original list.	*/
		}
	}

	the_file = fopen(par_fname,"w");						/* Now open a new file to put the new	*/
	if (!the_file)									/* list into.				*/
	{
		write_log("WISP",'F',"ERRCREPARA","Error opening new paragraph name file.");
		exit_wisp(EXIT_WITH_ERR);
	}

	for (i=0; i<proc_paras_cnt; i++)						/* For each paragraph...		*/
	{
		fprintf(the_file,"%s\n",proc_paras[i]);					/* Write it to the file.		*/
	}
	fclose(the_file);
	need_to_rerun("Relocate paragraphs into the DECLARATIVES.\n");
	exit_wisp(EXIT_AND_RUN); 							/* Now run WISP again, resolve the list.*/
	return 0;
}



/*
**	ROUTINE:	g_wfilechk()
**
**	FUNCTION:	Generate the call to "WFILECHK2" as part of declaritives handling.
**
**	DESCRIPTION:	Generate the code to get the extended file status then
**			generate the call to "WFILECHK2" to check the file status
**			code.
**
**	ARGUMENTS:	
**	i		The index for the file in the prog_xxx[i] tables.
**
**	GLOBALS:	?
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
int g_wfilechk(int i)
{
	if (!opt_native)
	{
		tput_line_at(12,     "MOVE \"%s\" TO WISP-CURRENT-FILE-ID.",prog_files[i]);
		tput_line_at(12,     "MOVE \"%s\" TO WISP-HAS-DECLARATIVES.", (prog_ftypes[i] & HAS_DECLARATIVES)?"Y":"N");
	}

	/*
	**	Get the extended file status code
	*/
	if (acu_cobol)
	{
		tput_line_at(12, "CALL \"C$RERR\" USING WISP-EXTENDED-FILE-STATUS.");
		if (!opt_nogetlastfileop)
		{
			tput_line_at(12, "CALL \"C$GETLASTFILEOP\" USING WISP-LASTFILEOP, NULL, NULL.");
		}
	}

	if (opt_native) 
	{
		/*
		**	CALL "W@FILESTATUSCHECK" USING
		**	     LastFileOp,
		**	     FileStatus,
		**	     ExtendedFileStatus
		**	     FileAttributes,
		**	     Vol,Lib,File,
		**	     NativePath,
		**	     CobolFileName,
		**	     CobolAppname,
		**	     WISP-SKIP-DECLARATIVES,
		**	     DeclarativesFlag.
		*/

		tput_line_at(12,	"CALL \"W@FILESTATUSCHECK\" USING");
		tput_clause(16,		    "WISP-LASTFILEOP,");
		tput_clause(16,		    "%s,",prog_fstats[i]);
		tput_clause(16,		    "WISP-EXTENDED-FILE-STATUS,");
		tput_clause(16,        	    "%s,", get_prog_status(i));
		tput_clause(16,		    "%s,", get_prog_vname(i));
		tput_clause(16,		    "%s,", get_prog_lname(i));
		tput_clause(16,		    "%s,", get_prog_fname(i));
		tput_clause(16,		    "%s,", get_prog_nname(i));
		tput_clause(16,             "\"%s\",", prog_files[i]);
		tput_clause(16,             "WISP-APPLICATION-NAME,");
		tput_clause(16,             "WISP-SKIP-DECLARATIVES,");
		tput_clause(16,             "\"%s\".", (prog_ftypes[i] & HAS_DECLARATIVES)?"Y":"N");
	}
	else
	{
		/*
		**	IF (	WISP-LASTFILEOP = "ReadLock" OR
		**		WISP-LASTFILEOP = "ReadNextLock") AND
		**		{file-status} = "{record-locked}"
		**	THEN
		**	    MOVE "Y" TO WISP-SKIP-DECLARATIVES
		**	ELSE
		**	    CALL "WFILECHK3" USING
		**		WISP-LASTFILEOP
		**		{file-status}
		**		WISP-EXTENDED-FILE-STATUS
		**		{FileAttributes}
		**		{file-vol}
		**		{file-lib}
		**		{file-file}
		**		{file-path}
		**		WISP-CURRENT-FILE-ID
		**		WISP-APPLICATION-NAME
		**		WISP-SKIP-DECLARATIVES
		**		WISP-HAS-DECLARATIVES
		**	END-IF.
		*/
		tput_line_at(12, "IF ((WISP-LASTFILEOP = \"ReadLock\" OR");
		tput_line_at(12, "     WISP-LASTFILEOP = \"ReadNextLock\") AND");
		tput_line_at(12, "     %s = \"%s\")", prog_fstats[i], hard_lock);
		tput_line_at(12, "THEN");
		tput_line_at(16,     "MOVE \"Y\" TO WISP-SKIP-DECLARATIVES");
		tput_line_at(12, "ELSE");
		tput_line_at(16,     "CALL \"WFILECHK3\" USING");
		tput_clause(20,		    "WISP-LASTFILEOP,");
		tput_clause(20,		    "%s,",prog_fstats[i]);
		tput_clause(20,		    "WISP-EXTENDED-FILE-STATUS,");
		tput_clause(20,        	    "%s,", get_prog_status(i));
		tput_clause(20,		    "%s,", get_prog_vname(i));
		tput_clause(20,		    "%s,", get_prog_lname(i));
		tput_clause(20,		    "%s,", get_prog_fname(i));
		tput_clause(20,		    "%s,", get_prog_nname(i));
		tput_clause(20,             "WISP-CURRENT-FILE-ID,");
		tput_clause(20,             "WISP-APPLICATION-NAME,");
		tput_clause(20,             "WISP-SKIP-DECLARATIVES,");
		tput_clause(20,             "WISP-HAS-DECLARATIVES");
		tput_line_at(12, "END-IF.");

	}
	return 0;
}

											/* Generate DECLARATIVES for files that	*/
int gen_defdecl(void)									/* don't have any.			*/
{
	int i;
	char	fld[50];

	for (i=0; i<prog_cnt; i++)							/* one for each file			*/
	{
		if (prog_ftypes[i] & (SORT_FILE + HAS_DECLARATIVES)) continue;		/* Skip SORT files, and files that may	*/

		make_fld(fld,prog_files[i],"WSECT-");

		tput_blank();
		tput_line_at(8, "%s SECTION.",fld);					/* Already have DECLARATIVES.		*/
		tput_line_at(12,"USE AFTER STANDARD ERROR PROCEDURE ON");
		tput_clause (16,"%s.",prog_files[i]);
		make_fld(fld,prog_files[i],"WDCL-");
		tput_line_at(8, "%s.",fld);
		g_wfilechk(i);								/* Generate a wfilechk call.		*/
		make_fld(fld,prog_files[i],"WDX-");
		tput_line_at(8,  "%s.",fld);
 		tput_line_at(12, "EXIT.");
	}
	return 0;
}

int fd_check(void)									/* Check for SELECT's with no FD's.	*/
{											/* This is illegal, so if they exist, we*/
	int i;										/* Generate a file with their names, and*/
	FILE *the_file;									/* re-run WISP, who will then delete the*/
											/* SELECT statements.			*/

	if (!prog_cnt) return(0);							/* Already did it, or no files.		*/

	the_file = NULL;

	for (i=0; i<prog_cnt; i++)							/* Check each file.			*/
	{
		if (!(prog_ftypes[i] & HAD_FD))						/* If it didn't have and FD...		*/
		{
			if (!the_file)
			{
				the_file = fopen(work_fname,"w");			/* Now open a new file to put the new	*/
				if (!the_file)						/* list into.				*/
				{
					write_log("WISP",'F',"ERRCREPARA","Error creating Pass 2 paragraph work file.");
					exit_with_err();
				}
			}

			fprintf(the_file,"%s\n",prog_files[i]);				/* Write it to the file.		*/

			write_tlog(NULL,"WISP",'W',"NOFD","Found SELECT %s with no FD, will delete.",prog_files[i]);
		}
	}

	if (the_file)
	{
		fclose(the_file);
		need_to_rerun("Delete SELECT's with no FD's.\n");
	}

	return(0);
}

/*
**	History:
**	$Log: wt_divs.c,v $
**	Revision 1.33  2003/03/17 17:22:43  gsl
**	Change to use  WFILECHK3
**	
**	Revision 1.32  2003/03/11 19:23:55  gsl
**	#NATIVE changes
**	
**	Revision 1.31  2003/03/10 17:44:28  gsl
**	Add Cobol logic to handler ReadLock with Record Locked without
**	calling  WFILECHK3
**	
**	Revision 1.30  2003/03/07 20:06:05  gsl
**	Fix call to WFILECHK3
**	
**	Revision 1.29  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.28  2003/03/06 21:52:02  gsl
**	WISP-DECLARATIVES-STATUS to WISP-LASTFILEOP
**	WFILECHK2 -> WFILECHK3
**	
**	Revision 1.27  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.26  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.25  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.24  2002/07/31 21:00:27  gsl
**	globals
**	
**	Revision 1.23  2002/07/31 20:24:26  gsl
**	globals
**	
**	Revision 1.22  2002/07/30 22:00:49  gsl
**	globals
**	
**	Revision 1.21  2002/07/26 19:20:43  gsl
**	
**	Revision 1.20  2002/07/02 04:09:49  gsl
**	Add WISP-SKIP-DECLARATIVES
**	
**	Revision 1.19  2002/06/21 20:49:32  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.18  2002/06/20 22:56:29  gsl
**	comments
**	
**	Revision 1.17  2002/06/20 02:38:48  gsl
**	native changes
**	W@FILESTATUSCHECK
**	
**	Revision 1.16  2002/05/16 21:48:48  gsl
**	getlastfileop logic
**	
**	Revision 1.15  1998-03-27 13:37:57-05  gsl
**	fix warning
**
**	Revision 1.14  1998-03-19 14:23:28-05  gsl
**	Change to use get_prog_status().
**	Move OLD to old.c
**
**	Revision 1.13  1998-03-03 15:42:12-05  gsl
**	update as part of cobol-85 changes
**
**	Revision 1.12  1997-09-09 17:53:58-04  gsl
**	Cange scrn_para() to gen_screen_paras() as part of ACN code
**
**	Revision 1.11  1997-04-29 12:54:53-04  gsl
**	Change to all WFILECHK2() which uses the longer extended file status
**	codes needed for acu4gl
**
**	Revision 1.10  1996-08-30 21:56:17-04  gsl
**	drcs update
**
**
**
*/
