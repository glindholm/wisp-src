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

static int decl_exnum = 0;								/* Used to remember which exit.		*/
static int decl_parsing = 0;								/* Indicates currently parsing a decl.	*/
static char decl_sname[40];								/* The SECTION name of this one.	*/
chk_dpar()										/* Check for DECLARATIVES para's.	*/
{
	int i;
	char tstr[40];
	char wstr[40];
	char hline[88];

	if (in_decl)
	{
		if (decl_paras_cnt >= MAX_PARAGRAPHS)
		{
			write_log("WISP",'F',"MAXPAREXC",
						"WISP internal error, Maximum number of paragraphs in DECLARATIVES exceeded.");
			exit_wisp(EXIT_WITH_ERR);
		}
		strcpy(decl_paras[decl_paras_cnt],parms[0]);				/* in the table of paragraphs in the	*/
		stredt(decl_paras[decl_paras_cnt++],".","");				/* declaratives.  edit out the period.	*/

		if (proc_performs_cnt)							/* See if it is a paragraph referenced	*/
		{									/* by the procdiv, and flag it for	*/
			strcpy(tstr,parms[0]);						/* copying.				*/
			stredt(tstr,".","");

			if (paracmp(tstr,proc_performs,proc_performs_cnt))		/* Found it?				*/
			{
				copy_to_dtp_file = 1;					/* Set the mode.			*/
			}
		}

		if (!strncmp(parms[1],"SECTION",7))					/* See if it's a section.		*/
		{
			p_use();							/* Process the USE procedure.		*/
		}
	}
	else										/* We are in the procedure division.	*/
	{										/* If it is not in declaratives, see if	*/
		if (proc_paras_cnt)							/* it is a paragraph which is referenced*/
		{									/* by the declaratives, and flag it for	*/
			strcpy(tstr,parms[0]);						/* copying.				*/
			stredt(tstr,".","");
			if (paracmp(tstr,proc_paras,proc_paras_cnt))			/* Found it?				*/
			{
				if (!strncmp(parms[1],"SECTION",7))			/* It's a SECTION.			*/
					copy_sect = 1;					/* Set first flag for section.		*/
				copy_to_dcl_file = 1;					/* Set the mode.			*/
			}
		}
	}
}

p_use()
{
	char tstr[40],rstr[40],fname[40],hline[128],fld[50];
	int tnum,fnum,multi_file;
	int	first_is_crt;

	sprintf(hline,"       %s SECTION.",parms[0]);					/* Build an "XXX SECTION." phrase.	*/

	gen_dexit();									/* If we were doing one, gen the end.	*/

	skip_param(2);									/* Skip over SECTION info.		*/

	ptype = get_param(tstr);							/* Get first word.			*/

	if (strcmp(tstr,"USE"))								/* Not a USE phrase.			*/
	{
		write_log("WISP",'F',"ERRINUSE","Error parsing USE statement. USE missing.");
		write_log("",' ',"","%s\n",inline);
		exit_wisp(EXIT_WITH_ERR);
	}

	ptype = get_param(tstr);							/* Get next word.			*/

	if (!strcmp(tstr,"AFTER"))							/* Skip AFTER.				*/
	{
		ptype = get_param(tstr);
	}

	if (!strcmp(tstr,"DEADLOCK"))							/* If DEADLOCK, not valid, delete it.	*/
	{
		write_log("WISP",'E',"DEADLOCK","USE AFTER DEADLOCK is not supported (DELETED).");
		inline[0] = '\0';
		del_use = 1;								/* Set a flag to make it delete it.	*/
		return(0);
	}

	if (!strcmp(tstr,"STANDARD"))
	{
		ptype = get_param(tstr);						/* Skip over STANDARD.			*/
	}

	strcpy(rstr,tstr);								/* Save the reason (EXCEPTION, ERROR).	*/

	ptype = get_param(tstr);							/* Get next work (should be PROCEDURE)	*/

	if (strcmp(tstr,"PROCEDURE"))
	{
		write_log("WISP",'F',"ERRINUSE","Error parsing USE statement, PROCEDURE missing.");
		write_log("",' ',"","%s\n",inline);
		exit_wisp(EXIT_WITH_ERR);
	}

	ptype = get_param(tstr);							/* Get next word (may be ON)		*/

	if (!strcmp(tstr,"ON"))								/* Skip ON.				*/
	{
		ptype = get_param(tstr);						/* Get next word.			*/
	}

	if (!strcmp(tstr,"SHARED") || !strcmp(tstr,"SPECIAL-INPUT"))			/* If it's SHARED or SPECIAL-INPUT.	*/
	{										/* Then remove it.			*/
		inline[0] = '\0';							/* Empty string.			*/
		del_use = 1;								/* Set a flag to make it delete it.	*/
		write_log("WISP",'E',"USEAFTER","USE AFTER ERROR ON %s is Invalid (DELETED).",tstr);
		return(0);
	}
	else if (!strcmp(tstr,"INPUT") || !strcmp(tstr,"OUTPUT") || !strcmp(tstr,"I-O") || !strcmp(tstr,"EXTEND"))
	{										/* It's not a specific file.		*/
		inline[0] = '\0';							/* Empty string.			*/
		write_log("WISP",'E',"USEAFTER","USE AFTER ERROR ON %s not supported, causes conflict.",tstr);
		tput_line("%s", hline);							/* Write the SECTION name.		*/
		gen_use(rstr,tstr);							/* Generate the USE procedure.		*/
		return(0);								/* Let it go.				*/
	}

	tnum = file_index(tstr);							/* Find the file id.			*/
	first_is_crt = 0;

	if (tnum == -1)
	{
		tnum = crt_index(tstr);
		if ( tnum == -1 ) 
		{
			write_log("WISP",'E',"ERRINUSE","Error parsing USE statement, File \"%s\"not found.",tstr);
			exit_wisp(EXIT_WITH_ERR);
		}
		first_is_crt = 1;
	}
	else  /* A regular file */
	{
        	prog_ftypes[tnum] |= HAS_DECLARATIVES;					/* Flag it was there.			*/
	}

	multi_file = 0;

	while (ptype != -1)								/* If it wasn't alone on the line...	*/
	{
		int	iscrt;

		iscrt = 0;
		ptype = get_param(fname);						/* Scan for other file names.		*/
		fnum = file_index(fname);						/* Get file index.			*/

		if (fnum == -1)
		{
			fnum = crt_index(fname);
			if (fnum == -1)
			{
				write_log("WISP",'F',"ERRINUSE","Error parsing USE statement, File \"%s\"not found.",fname);
				exit_wisp(EXIT_WITH_ERR);
			}
			iscrt = 1;
		}

		
		make_fld(fld,fname,"WSECT-");
		tput_line_at(8, "%s SECTION.",fld);					/* Make a section for it.		*/
		gen_use(rstr,fname);							/* Give it a USE statement.		*/
		make_fld(fld,fname,"WDCL-");
		tput_line_at(8, "%s.",fld);						/* Generate a starting paragraph.	*/

		if (!iscrt)
		{
		        prog_ftypes[fnum] |= HAS_DECLARATIVES;				/* Flag it was there.			*/
			g_wfilechk(fnum,".\n");						/* Generate a call to WFILECHK.		*/

			tput_line_at(12, "IF WISP-DECLARATIVES-STATUS NOT = \"00\" THEN");	/* Do declaratives.		*/
		}

		make_fld(fld,tstr,"WDS-");
		tput_line_at(16, "PERFORM %s THRU",fld);
		make_fld(fld,tstr,"WDX-");
		tput_clause (20, "%s.",fld);
		make_fld(fld,fname,"WDX-");
		tput_line_at(8,  "%s.",fld);					/* Generate an ending paragraph.	*/
		tput_line_at(12, "EXIT.");
		multi_file = 1;
	}


	tput_line("%s", hline);								/* Write the SECTION.	*/
	gen_use(rstr,tstr);								/* Generate the USE phrase.		*/
	make_fld(fld,tstr,"WDCL-");
	tput_line_at(8,  "%s.",fld);							/* Generate an initial paragraph.	*/
	if (!first_is_crt)
	{
		g_wfilechk(tnum,".\n");							/* Generate a call to WFILECHK.		*/

		tput_line_at(12, "IF WISP-DECLARATIVES-STATUS = \"00\" THEN");		/* Skip declare.			*/
		make_fld(fld,tstr,"WDX-");
		tput_line_at(16, "GO TO %s.",fld);
	}

	if (multi_file)
	{										/* If this is used by more than one file*/
		make_fld(fld,tstr,"WDS-");
		tput_line_at(8,  "%s.",fld);						/* Generate a starting paragraph.	*/
	}

	strcpy(decl_sname,tstr);							/* Save this section name.		*/
	decl_parsing = 1;								/* Set a flag saying we did this.	*/

	inline[0] = '\0';								/* Empty string.			*/
}

gen_use(reason,filename)
char *reason,*filename;
{
	tput_line_at(12, "USE AFTER %s PROCEDURE ON",reason);
	tput_clause (16, "%s.",filename);
}

gen_dexit()										/* Generate the EXIT paragraph for decl.*/
{
	char	fld[50];

	if (!decl_parsing) return(0);							/* No need.				*/

	make_fld(fld,decl_sname,"WDX-");
	tput_line_at(8,  "%s.",fld);
	tput_line_at(12, "EXIT.");
	decl_parsing = 0;								/* Clear the flag.			*/
}
