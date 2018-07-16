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

check_decl()										/* Examine the tables of PERFORMs in	*/
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


check_section()										/* Examine the current line, which should*/
{											/* Be the first line in the PROCEDURE	*/
	if (strcmp(parms[1],"SECTION."))						/* If this is not a section name, add it*/
	{
		tput_line_at(8, "WISP-MAIN SECTION.");
		tput_line_at(8, "WISP-START-PROGRAM.");					/* Always start programs with our stuff.*/
		call_initwisp();
	}
	else
	{
		tput_line("%s", linein);			       			/* Write out their section.		*/
		tput_line_at(8, "WISP-START-PROGRAM.");					/* Always start programs with our stuff.*/
		call_initwisp();
		get_line();								/* And fetch a new one.			*/
	}
	return 0;
}

static call_initwisp()
{

	tput_line("           CALL \"initwisp2\" USING WISP-TRAN-VERSION,");
	tput_line("                                  WISP-LIB-VERSION,");
	tput_line("                                  WISP-COBOL-TYPE,");
	tput_line("                                  WISP-APPLICATION-NAME,");
	tput_line("                                  WISPRUNNAME,");
	tput_line("                                  WISP-SWAP-ON,");
	tput_line("                                  WISP-ERR-LOGGING.");
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
**	end_str		The string to use to terminate the CALL statement e.g ".\n"
**
**	GLOBALS:	?
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
int g_wfilechk(int i, char* end_str)
{
	char tstr[40];

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
		make_fld(tstr,prog_files[i],"S-");					/* Make a Status Flag field.		*/
		tput_line_at(12, "MOVE WISP-DECL TO WISP-LONGWORD");
		tput_line_at(12, "CALL \"lbit_on\" USING WISP-LONGWORD,");
		tput_clause (16, "%s,",tstr);
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

	make_fld(tstr,prog_files[i],"S-");						/* Generate the S- field.		*/
	tput_clause(16,        "%s,",tstr);
											/* Don't use a literal.			*/
	if (!prog_vnames[i][0] || (prog_vnames[i][0] == '\"'))
	{
		make_fld(tstr,prog_files[i],"V-");					/* Generate the VOLUME field.		*/
		tput_clause(16,"%s,",tstr);
	}
	else
	{
		tput_clause(16,"%s,",prog_vnames[i]);
	}
											/* Don't use a literal.			*/
	if (!prog_lnames[i][0] || (prog_lnames[i][0] == '\"'))
	{
		make_fld(tstr,prog_files[i],"L-");
		tput_clause(16,"%s,",tstr);
	}
	else
	{
		tput_clause(16,"%s,",prog_lnames[i]);
	}

	if (!prog_fnames[i][0] || (prog_fnames[i][0] == '\"'))	 			/* Don't use a literal.			*/
	{
		make_fld(tstr,prog_files[i],"F-");
		tput_clause(16,"%s,",tstr);
	}
	else
	{
		tput_clause(16,"%s,",prog_fnames[i]);
	}

	make_fld(tstr,prog_files[i],"N-");
	tput_clause(16,        "%s,",tstr);
	tput_clause(16,        "WISP-CURRENT-FILE-ID,");
	tput_clause(16,        "WISP-APPLICATION-NAME%s",end_str);
	return 0;
}

											/* Generate DECLARATIVES for files that	*/
gen_defdecl()										/* don't have any.			*/
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
		g_wfilechk(i,".\n");							/* Generate a wfilechk call.		*/
		make_fld(fld,prog_files[i],"WDX-");
		tput_line_at(8,  "%s.",fld);
 		tput_line_at(12, "EXIT.");
	}
	return 0;
}

fd_check()										/* Check for SELECT's with no FD's.	*/
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
**	History:
**	$Log: wt_divs.c,v $
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
