static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wisp_divs.c
*/

#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "cobfiles.h"
#include "directiv.h"


extern	int	symbzero;
extern	char	decimal_is;

static int decl_pnum = 0;
static int call_initwisp();


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


new_para()										/* Generate a new .PAR file with a list	*/
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
**	FUNCTION:	Generate the call to "wfilechk2" as part of declaritives handling.
**
**	DESCRIPTION:	Generate the code to get the extended file status then
**			generate the call to "wfilechk2" to check the file status
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
	tput_line_at(12, "MOVE \"%s\" TO",prog_files[i]);
	tput_clause (16, "WISP-CURRENT-FILE-ID");

	/*
	**	Get the extended file status code
	*/
	if (vax_cobol)
	{
		tput_line_at(12, "MOVE RMS-STS OF %s",prog_files[i]);
		tput_clause (16, "TO WISP-EXTENDED-FILE-STATUS-1");
		tput_line_at(12, "MOVE RMS-STV OF %s",prog_files[i]);
		tput_clause (16, "TO WISP-EXTENDED-FILE-STATUS-2");
	}
	else if (acu_cobol)
	{
		tput_line_at(12, "CALL \"C$RERR\" USING WISP-EXTENDED-FILE-STATUS-1");
	}

	if (prog_ftypes[i] & HAS_DECLARATIVES)						/* If it has a DECLARATIVE, set bit.	*/
	{
		tput_line_at(12, "MOVE WISP-DECL TO WISP-LONGWORD");
		tput_line_at(12, "CALL \"lbit_on\" USING WISP-LONGWORD,");
		tput_clause (16, "%s,", get_prog_status(i));
	}

	/*
	**	CALL "wfilechk2" using  WISP-DECLARATIVES-STATUS,
	**				file-status,
	**				WISP-EXTENDED-FILE-STATUS-1,
	**				WISP-EXTENDED-FILE-STATUS-2,
	**				S-filename,
	**				vol-name, lib-name, file-name
	**				N-filename,
	**				WISP-CURRENT-FILE-ID,
	**				WISP-APPLICATION-NAME.
	*/

	tput_line_at(12,	"CALL \"wfilechk2\" USING");
	tput_clause(16,		    "WISP-DECLARATIVES-STATUS,");
	tput_clause(16,		    "%s,",prog_fstats[i]);
	tput_clause(16,		    "WISP-EXTENDED-FILE-STATUS-1,");
	tput_clause(16,		    "WISP-EXTENDED-FILE-STATUS-2,");
	tput_clause(16,        	    "%s,", get_prog_status(i));				/* Generate the S- field.		*/
	tput_clause(16,		    "%s,", get_prog_vname(i));				/* Generate the VOLUME field.		*/
	tput_clause(16,		    "%s,", get_prog_lname(i));
	tput_clause(16,		    "%s,", get_prog_fname(i));
	tput_clause(16,		    "%s,", get_prog_nname(i));
	tput_clause(16,             "WISP-CURRENT-FILE-ID,");
	tput_clause(16,             "WISP-APPLICATION-NAME.");
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
**	Change to all wfilechk2() which uses the longer extended file status
**	codes needed for acu4gl
**
**	Revision 1.10  1996-08-30 21:56:17-04  gsl
**	drcs update
**
**
**
*/
