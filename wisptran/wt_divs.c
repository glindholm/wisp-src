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

#ifdef MSDOS
#include <string.h>
#endif

#define EXT extern
#include "wisp.h"

int	ioc_found = 0;

extern	int	symbzero;
extern	char	decimal_is;

static int has_config = 0;								/* Flag it has CONFIGURATION SECTION	*/
static int decl_pnum = 0;
static int saw_ident = 0;

int	mark_env_div = 0;
int		mark_config_sect = 0;
int		mark_io_sect = 0;
int			mark_file_control = 0;
int			mark_io_control = 0;
int	mark_data_div = 0;
int		mark_file_sect = 0;
int		mark_ws_sect = 0;
int			mark_gen_screens = 0;
int		mark_link_sect = 0;
int	mark_proc_div = 0;

check_div()
{
	int i,j;
	char tstr[80],tstr1[80], *ptr;
	char	token[80];

start_proc:

	if (strcmp(area_a,"    "))							/* If area_a is not empty then there	*/
	{
		exit_pmode();								/* may be something new to parse.	*/
	}
	else
	{
		return(0);								/* blank area a means no divisions	*/
	}



	if (!saw_ident)									/* Have we seen the IDENTIFICATION DIV?	*/
	{
		if (!strcmp(parms[0],"IDENTIFICATION"))					/* Here it is.				*/
		{
			division = IDENTIFICATION_DIVISION;
			if (!linkmain)							/* If linkmain then we will write later */
			{
				put_line(inline);					/* Output it.				*/
			}
			get_line();							/* Get a new line.			*/
			saw_ident = 1;							/* Flag it.				*/
		}
	}


	if (division == IDENTIFICATION_DIVISION)
	{
		if (!strcmp(parms[0],"ENVIRONMENT"))					/* Look for ENVIRONMENT DIV.		*/
		{
			mark_env_div = 1;
			division = ENVIRONMENT_DIVISION;
			put_line(inline);						/* Output it.				*/
			get_line();							/* Get next line.			*/
		}
	}

	if (division == PROCEDURE_DIVISION)
	{
		if (!in_decl) return(0);						/* no divisions after proc. & declare.	*/

											/* Must be in DECLARATIVES.		*/
		nopunct(token,parms[1]);
		if (!strcmp(parms[0],"END") && !strcmp(token,"DECLARATIVES"))		/* look for end of declaratives		*/
		{									/* Found it, add our entries.		*/
			gen_dexit();							/* First terminate any unfinished one.	*/

			put_line("       WISP-DECLARATIVES-DISPLAY SECTION.\n");
			put_line("           USE AFTER STANDARD ERROR PROCEDURE ON WISP-DECLARATIVES-FILE.\n");

			if (par_count)							/* Were there any paragraphs to copy?	*/
			{								/* If there were, include a COPY state-	*/
				write_line("           COPY \"%s\".\n",lib_file);	/* ment for the wisp-generated LIB file.*/
			}
			scrn_para();							/* Now generate the screens used in DECL*/
			put_line("       END-WISP-DECLARATIVES.\n");
			if (decl_stop_exit) d_wisp_exit_para();
											/* Now see if the paragraphs to be	*/
			for (i=0; i<ppdiv_count; i++)					/* copied to the PROCEDURE DIVISION	*/
			{								/* really exist (from .OPT file)	*/
				for (j=0; j<pd_count; j++)				/* These are paragraphs which exist in	*/
				{							/* DECL, but are performed by proc div.	*/
					if (!strcmp(perf_pdiv[i],par_decl[j]))		/* See if they match. if they do, we	*/
					{						/* can skip the loop.		 	*/
						break;					/* and stop looking for this one.	*/
					}
				}
				if (j == pd_count) perf_pdiv[i][0] = '\0';		/* Wasn't in the list, delete it.	*/
			}
			if (pr_count) check_decl();					/* If there are any unaccounted for	*/
											/* performs, examine the declaratives	*/
											/* table.				*/

			if (prog_cnt -prog_sort)					/* If there were valid files...		*/
			{
				gen_defdecl();						/* Generate declaratives for files with	*/
			}								/* no declaratives.			*/
			put_line(inline);						/* Put the END DECLARATIVES out.	*/
			get_line();							/* Get a new line.			*/
			check_section();						/* See if it's in a section		*/
			in_decl = 0;
		}
	}

	nopunct(token,parms[1]);
	if (0==strcmp(parms[0],"CONFIGURATION") && 0==strcmp(token,"SECTION"))
	{
		mark_config_sect = 1;
		put_line(inline);							/* Write it out.			*/
		get_line();
		has_config = 1;								/* We saw a CONFIGURATION SECTION.	*/
	}

	if (!strcmp(parms[0],"INPUT-OUTPUT"))						/* Look for the INPUT-OUTPUT		*/
	{
		mark_io_sect = 1;
		gen_missing_sections();

		division = INPUT_OUTPUT_SECTION;
		write_log("WISP",'I',"BEGINIOSEC","Begin INPUT-OUTPUT SECTION.");

		put_line(inline);

		get_line();								/* Check next line.			*/
	}

	if ((division == INPUT_OUTPUT_SECTION))
	{
		nopunct(token,parms[0]);
		if (!strcmp(token,"FILE-CONTROL"))					/* ABSORB FILE-CONTROL			*/
		{
			mark_file_control = 1;
			write_log("WISP",'I',"BEGINIOSEC","Begin FILE-CONTROL.");
			gen_file_control();
			get_line();
		}
		else if (!strcmp(token,"I-O-CONTROL"))					/* Delete the i-o-control.		*/
		{
			mark_io_control = 1;
			gen_missing_sections();

			write_log("WISP",'I',"I-O-CONTROL","I-O-CONTROL found.");
			ioc_found = 1;							/* Flag - found I-O-CONTROL		*/
			put_line("       I-O-CONTROL.\n");
			stredt(inline,parms[0],"    ");
			hold_line();

			get_line();
			if (0 == strcmp(area_a,"    ")) 				/* If not empty I-O-CONTROL		*/
			{
				while( NULL == (ptr = (char *)strchr(inline,'.')) )	/* Write lines out until a period	*/
				{
					put_line(inline);
					get_line();
				}
				if (vax_cobol)
				{
					*ptr = ' ';					/* remove the period.			*/
					ioc_found = 2;					/* Flag - period is needed		*/
				}
				put_line(inline);					/* Output final line 			*/
				get_line();						/* Prime for next time.			*/
			}

			if (vax_cobol) io_control();					/* Generate lock-holding stmts		*/
			goto start_proc;						/* loop back				*/
		}
	}

	nopunct(token,parms[1]);
	if (0==strcmp(parms[0],"DATA") && 0==strcmp(token,"DIVISION"))
	{
		mark_data_div = 1;
		gen_missing_sections();

		division = DATA_DIVISION;
		write_log("WISP",'I',"BEGINDATADIV","Begin DATA DIVISION SECTION");

		put_line(inline);							/* Copy it to the output file.		*/
		get_line();
	}										/* Look for the identifier		*/

	if ((division == DATA_DIVISION) && (!strcmp(parms[0],"FILE")))			/* Detect FILE SECTION			*/
	{
		mark_file_sect = 1;

		write_log("WISP",'I',"BEGINIOSEC","Begin FILE SECTION.");

		gen_file_section();

		get_line();								/* get the next line...			*/
	}

	if ((division == DATA_DIVISION) && !strcmp(parms[0],"WORKING-STORAGE"))		/* Look for the identifier		*/
	{										/* it can follow DATA DIVISION immediatly*/
		mark_ws_sect = 1;
		gen_missing_sections();

		division = WORKING_STORAGE_SECTION;
		write_log("WISP",'I',"BEGINWSSEC","Begin WORKING-STORAGE SECTION");
		put_line(inline);							/* Copy it to the output file.		*/
		fd_check();								/* Check for bad fd-select logic.	*/
		ws_init();								/* Generate the working storage fields	*/
		get_line();
	}

	if (!strcmp(parms[0],"LINKAGE"))						/* found LINKAGE, write the screens	*/
	{
		mark_link_sect = 1;
		gen_missing_sections();

		write_log("WISP",'I',"LINKAGEFND","Found LINKAGE section.");
		fd_check();								/* Check for bad fd/select logic.	*/
	}

	if (!strcmp(parms[0],"PROCEDURE"))
	{
		mark_proc_div = 1;
		gen_missing_sections();

		division = PROCEDURE_DIVISION;
		write_log("WISP",'I',"BEGPROCDIV","Begin PROCEDURE DIVISION.");

		fd_check();								/* Check for bad fd/select logic.	*/

		put_line(inline);							/* Copy it to the output file.		*/
		if (strpos(inline,".") == -1)						/* It has no period, look for the USING */
		{									/* keyword.				*/
			if (!strcmp(parms[2],"USING"))					/* Found it, skip over the USING phrase.*/
			{
				skip_using();
			}
			else
			{
				get_line();						/* Not found yet, get a new line.	*/
				if (!strcmp(parms[0],"USING"))				/* Found the USING phrase on next line.	*/
				{
					hold_line();					/* Save this line.			*/
					skip_using();					/* Skip over the USING stuff.		*/
				}
				else
				{							/* Warn about a potential problem.	*/
					write_log("WISP",'E',"ERRINUSING","Syntax error parsing PROCEDURE DIVISION USING phrase.");
					hold_line();					/* Let the line be used.		*/
				}
			}
		}

		get_line();								/* get a new line.			*/

		if (strpos(inline,"DECLARATIVES") == -1)				/* no declaratives			*/
		{
			if (prog_cnt - prog_sort)					/* and there are valid files.		*/
			{								/* so put ours in place			*/
				write_log("WISP",'I',"INSERTDECL","Inserting default DECLARATIVES.");
				put_line("       DECLARATIVES.\n\n");
				gen_defdecl();						/* And generate the default declaratives.*/
				put_line("       END DECLARATIVES.\n\n");
			}								/* continue processing the line 	*/
			check_section();						/* See if there is a section name	*/

			for (i=0; i<ppdiv_count; i++)					/* Delete any procedure division copys	*/
			{								/* (from .OPT file)			*/
				perf_pdiv[i][0] = '\0';
			}
		}
		else									/* we do have declaratives, flag it	*/
		{
			in_decl = 1;
		}
	}

}

static int skip_using()
{
	write_log("WISP",'I',"SKIPUSING","Skipping USING phrase.");
	do
	{
		get_line();								/* get a line...			*/
		put_line(inline);							/* write it out...			*/
	} while (strpos(inline,".") == -1);						/* till we find a period		*/
}

#define DO_PICTURE	0
#define DO_OCCURS	8
#define DO_REDEFINES	15
#define DO_COMP		25

do_crtfile()										/* process the FD for the crt file	*/
{
	int i,j;
	int occ_count = 1;								/* current item OCCURS count		*/
	int done;									/* done flag				*/
	int redef = 0;									/* REDEFINES flag			*/
	int last_size = 0;								/* size of the last PIC			*/
	int pic_found = 0;								/* have we done a PIC for this item?	*/
	char last_level[4];								/* level number of last item.		*/
	char occ_level[4];								/* level number of last OCCURS		*/
	char redef_level[4];								/* level number of last REDEFINES	*/
	char tstr[80];
	int	crt_count_save;								/* First rec of the FD			*/

	crt_count_save = crt_count;

	ptype = get_param(last_level);							/* get the starting level		*/
	strcpy(occ_level,last_level);
	redef_level[0] = 0;

	ptype = get_param(crt_record[crt_count]);					/* get the first crt records name	*/

	write_log("WISP",'I',"CRTRECORD","CRT record -- %s",crt_record[crt_count]);


	fprintf(crt_temp_file,"%s",inline);						/* copy it to temp output file		*/


	done = 0;
	do
	{
		ptype = get_param(o_parms[0]);						/* get a word				*/

		if (ptype == 1)								/* New line, fix any BINARY fields.	*/
		{
			sprintf(tstr,"%s PIC S9(4)",bin2_type);
			if (stredt(inline,"BINARY",tstr) != -1)				/* remove the BINARY part, if any.	*/
			{
				hold_line();						/* There was, hold the line.		*/
				get_line();						/* Now re-parm it.			*/
				ptype = get_param(o_parms[0]);				/* Get the first item and continue.	*/
				crt_size[crt_count] -= (2 * occ_count);			/* remove half the size.		*/
			}
		}

			/*  0         1         2         3	*/
			/*  0123456789012345678901234567890	*/
		i = strpos("PICTURE OCCURS REDEFINES COMP",o_parms[0]);			/* see if it is in the list		*/

		switch (i)
		{
			case DO_PICTURE:
			{
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				ptype = get_kw(o_parms[1]);				/* get the pic				*/
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				pic_found = 1;						/* flag we found it.			*/
				if (!redef)						/* skip it if a REDEFINED field		*/
				{
					last_size = pic_size(o_parms[1]);		/* get the size of the field		*/
					if (ptype == -1)				/* no period? it's part of an OCCURS	*/
						crt_size[crt_count] += (last_size * occ_count);	/* if not, add the size		*/
				}
				break;
			}

			case DO_COMP:
			{
				break;							/* Just skip it.			*/
			}

			case DO_OCCURS:
			{
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				ptype = get_param(o_parms[1]);				/* get the OCCURS count			*/
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				sscanf(o_parms[1],"%d",&j);				/* convert to an integer		*/
				if (strcmp(last_level,occ_level) > 0)			/* current level is greater than last	*/
				{
					occ_count *= j;					/* multiply by the last OCCURS		*/
				}
				else							/* make this the new OCCURS		*/
				{
					occ_count = j;
				}
				strcpy(occ_level,last_level);				/* save the level number		*/
				if (ptype != -1)
					ptype = get_param(o_parms[1]);			/* skip over TIMES			*/
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				if (!redef && pic_found)				/* apply the occurs retroactively	*/
				{
					crt_size[crt_count] += (last_size * occ_count);	/* add the size				*/
				}
				break;
			}

			case DO_REDEFINES:
			{
				redef = 1;
				strcpy(redef_level,last_level);				/* remember this level			*/
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				ptype = get_param(o_parms[1]);				/* get the items name			*/
				if (ptype == 1) fprintf(crt_temp_file,"%s",inline);	/* copy it to temp output file		*/
				break;
			}

			default:
			{
				j = 0;
				sscanf(o_parms[0],"%d",&j);				/* if not a keyword, it's a new item	*/
				if (j)							/* it is a new item.			*/
				{
					strcpy(last_level,o_parms[0]);			/* save this level number		*/
					if (!strcmp(redef_level,last_level))		/* is it the same level as a current	*/
					{						/* REDEFINES?				*/
						redef = 0;				/* yes, clear the REDEFINES flag	*/
					}

					pic_found = 0;					/* haven't found the PIC		*/

					if (j == 1)					/* a new crt record,  count it		*/
					{
						write_log("WISP",'I',"CRTSIZE","%s size is %d.\n",
											crt_record[crt_count],crt_size[crt_count]);
						crt_count++;
						crt_size[crt_count] = 0;
						occ_count = 1;				/* only OCCURS 1 time			*/
						redef = 0;				/* not in a REDEFINES			*/
						ptype = get_param(crt_record[crt_count]);
						write_log("WISP",'I',"CRTRECORD","CRT record -- %s",crt_record[crt_count]);
						strcpy(tstr,crt_record[crt_count]);
						strcat(tstr,"\n           REDEFINES ");
						strcat(tstr,crt_record[crt_count_save]);
						stredt(inline,crt_record[crt_count],tstr);
						fprintf(crt_temp_file,"%s",inline);	/* copy it to temp file			*/
					}
					else
					{						/* a new item 				*/
						fprintf(crt_temp_file,"%s",inline);	/* copy it to temp file			*/
						ptype = get_param(o_parms[1]);		/* fetch the name			*/
						if (strcmp(last_level,occ_level) <= 0)	/* current level is less than last	*/
						{
							occ_count = 1;			/* reset OCCURS count			*/
						}
					}
				}
				else if (strcmp(area_a,"    "))				/* If something is in area a, must be all*/
				{
					done = 1;
					write_log("WISP",'I',"CRTSIZE","%s size is %d.\n",
											crt_record[crt_count],crt_size[crt_count]);
					hold_line();
					crt_count++;
				}
				break;
			}
		}									/* end of switch			*/
	}while (!done);
}

init_figcons()
{
	int i,j,l,k,offset;
	int	use_copy, output_copy;
	char	COPY_SYMBOLICS[40];

	use_copy = 0;									/* Use a "COPY" statement		*/
	output_copy = 0;								/* Write the copy code.			*/

	if (symbzero) 	strcpy(COPY_SYMBOLICS,"wc001x03.cpy");
	else    	strcpy(COPY_SYMBOLICS,"wc001003.cpy");

	write_log("WISP",'I',"INITFIGCON","Creating default FIGURATIVE-CONSTANTS.");

	if (!wrote_special_names)
	    put_line("       SPECIAL-NAMES.\n");					/* Output special names section		*/

	if ( open_files == 1 || concat )						/* If not writing to copy file		*/
	{
		write_line("000001     COPY \"%s\".\n",COPY_SYMBOLICS);			/* Write the copy statement		*/
		use_copy = 1;
		if ( access(COPY_SYMBOLICS,0) != 0 )					/* If copybook doesn't exist ...	*/
		{
			open_output(COPY_SYMBOLICS);					/* open it and start writing.		*/
			output_copy = 1;
		}
	}
	else
	{
		output_copy = 1;							/* Just output the code to the file	*/
	}
	
	if (output_copy)
	{
		put_line("\n");
		put_line("      ***********************************************\n");
		put_line("      *******          WISP SYMBOLICS         *******\n");
		put_line("      ***********************************************\n");

		if (symbzero)	offset=0;
		else		offset=1;

		write_line("           SYMBOLIC WISP-SCWCC   IS %d\n",(160+offset));
		write_line("           SYMBOLIC WFAC-MODIFY  IS %d\n",(129+offset));
		write_line("           SYMBOLIC WFAC-PROTECT IS %d\n",(140+offset));

		put_line("           SYMBOLIC\n");

		for (i=0; i<16; i++)
		{
			for (j=0; j<8; j++)
			{
				put_line("           ");
				for (k=0; k<2; k++)
				{
					l = j * 2 + k;					/* Low order nybble			*/
					write_line("DEC-BYTE-%d IS %d ",i*16+l,i*16+l+offset);
				}
				put_char('\n');
			}
		}
	}

	if (use_copy && output_copy) close_output();

	did_figcons = 1;								/* flag we did it			*/
}

finish_figcons()
{
	int	offset;

	if (symbzero)	offset=0;
	else		offset=1;

	write_line(	 "           SYMBOLIC EFFAC IS %d\n",(140+offset));
	if (decimal_is == ',')
	{
		put_line("           DECIMAL-POINT IS COMMA\n");
	}
	put_line(        "           .\n");
}


check_decl()										/* Examine the tables of PERFORMs in	*/
											/* the DECLARATIVES division against the*/
{											/* Table of paragraph-names that were in*/
	int i,j;									/* the DECLARATIVES. This way we can	*/
	int unknown;									/* determine what PERFORMs were para's	*/
											/* that are in the procedure division.	*/

	unknown = pr_count;								/* Remember how many are unaccounted for */

	for (i=0; i<pr_count; i++)							/* for each PERFORM...			*/
	{
		for (j=0; j<pd_count; j++)						/* examine each PARAGRAPH-NAME		*/
		{
			if (!strcmp(perf_decl[i],par_decl[j]))				/* to see if they match. if they do, we	*/
			{								/* can delete the PERFORM from the list	*/
				perf_decl[i][0] ='\0';					/* of unknown performs.			*/
				unknown--;
				j = pd_count;						/* Stop looking for this one.		*/
			}
		}
	}

	if (unknown)									/* If there are new unknowns, record em.*/
	{
		write_log("WISP",'I',"UNRESPERF","Performs found in the DECLARATIVES that were unresolved:");
		new_para();								/* Generate a new .PAR file.		*/
	}
	pr_count = 0;									/* reset table for use in proc div.	*/
}

new_para()										/* Generate a new .PAR file with a list	*/
{											/* of screens and paragraph names which	*/
	int i;										/* are referenced by the declaratives,	*/
	FILE *the_file;									/* but in the procedure division.	*/

	for (i=0; i<pr_count; i++)							/* pr_count is the count of how many	*/
	{										/* PERFORM's were in DECLARATIVES.	*/
		if (perf_decl[i][0])							/* If it's not a null string (resolved)	*/
		{									/* Then it must be an unresolved PERFORM*/
			write_log("WISP",' ',"    %s.\n",perf_decl[i]);			/* Write it out.			*/
			strcpy(par_name[par_count++],perf_decl[i]);			/* And add it to the original list.	*/
		}
	}

	the_file = fopen(par_fname,"w");						/* Now open a new file to put the new	*/
	if (!the_file)									/* list into.				*/
	{
		write_log("WISP",'F',"ERRCREPARA","Error opening new paragraph name file.");
		exit_wisp(-1);
	}

	for (i=0; i<par_count; i++)							/* For each paragraph...		*/
	{
		fprintf(the_file,"%s\n",par_name[i]);					/* Write it to the file.		*/
	}
	fclose(the_file);
	exit_wisp(EXIT_AND_RUN);							/* Now run WISP again, resolve the list.*/
}


check_section()										/* Examine the current line, which should*/
{											/* Be the first line in the PROCEDURE	*/
	if (strcmp(parms[1],"SECTION."))						/* If this is not a section name, add it*/
	{
		put_line("       WISP-MAIN SECTION.\n");
		put_line("       WISP-START-PROGRAM.\n");				/* Always start programs with our stuff.*/
		call_initwisp();
	}
	else
	{
		put_line(inline);							/* Write out their section.		*/
		put_line("       WISP-START-PROGRAM.\n");				/* Always start programs with our stuff.*/
		call_initwisp();
		get_line();								/* And fetch a new one.			*/
	}
}

static call_initwisp()
{

	put_line("           CALL \"initwisp2\" USING WISP-TRAN-VERSION,\n");
	put_line("                                  WISP-LIB-VERSION,\n");
	put_line("                                  WISP-COBOL-TYPE,\n");
	put_line("                                  WISP-APPLICATION-NAME,\n");
	put_line("                                  WISPRUNNAME,\n");
	put_line("                                  WISP-SWAP-ON,\n");
	put_line("                                  WISP-ERR-LOGGING.\n");
}

static char chr1 = '0';
static char chr2 = '0';

fix_filler(aline)
char *aline;
{
	char xname[9];

	xname[0] = ' ';
	xname[1] = 'W';									/* Create the replacement name		*/
	xname[2] = 'I';
	xname[3] = 'S';
	xname[4] = 'P';
	xname[5] = chr1;								/* and add the 2 char id.		*/
	xname[6] = chr2;
	xname[7] = ' ';
	xname[8] = '\0';

	if (stredt(aline," FILLER ",xname) == -1) return(0);				/* Now replace it			*/
											/* Just return if it's not there.	*/
	if (++chr2 > 'Z')								/* increment lower byte.		*/
	{
		chr2 = '0';								/* set to a zero.			*/

		if (++chr1 > 'Z') chr1 = '0';						/* Increment the tens digit.		*/
		if (chr1 == ':') chr1 = 'A';						/* Use alphabetic.			*/
	}
	else if (chr2 == ':') chr2 = 'A';						/* Use alphabetic			*/
}

check_config()
{
	if (has_config) return(0);

	put_line("       CONFIGURATION SECTION.\n");					/* Add it in.				*/
	has_config = 1;									/* Flag it.				*/
	mark_config_sect = 1;
}

g_wfilechk(i,end_str)									/* generate a call to wfilechk.		*/
int i;
char *end_str;
{
	char tstr[40];

	write_line("             MOVE \"%s\" TO\n",prog_files[i]);
	put_line(  "                  WISP-CURRENT-FILE-ID\n");
	if (vax_cobol)
	{
		write_line(  "             MOVE RMS-STS OF %s\n",prog_files[i]);
		put_line(  "               TO WISP-EXTENDED-FILE-STATUS-1\n");
		write_line(  "             MOVE RMS-STV OF %s\n",prog_files[i]);
		put_line(  "               TO WISP-EXTENDED-FILE-STATUS-2\n");
	}
	else if (acu_cobol)
	{
		put_line(  "             CALL \"C$RERR\" USING WISP-EXTENDED-FILE-STATUS\n");
		put_line(  "             MOVE 0 TO WISP-EXTENDED-FILE-STATUS-2\n");
	}
	else
	{
		put_line(  "             MOVE 0 TO WISP-EXTENDED-FILE-STATUS-1\n");
		put_line(  "             MOVE 0 TO WISP-EXTENDED-FILE-STATUS-2\n");
	}

	if (prog_ftypes[i] & HAS_DECLARATIVES)						/* If it has a DECLARATIVE, set bit.	*/
	{
		make_fld(tstr,prog_files[i],"S-");					/* Make a Status Flag field.		*/
		write_line("             MOVE WISP-DECL TO WISP-LONGWORD\n");
		write_line("             CALL \"lbit_on\" USING WISP-LONGWORD,\n");
		write_line("                                  %s\n\n",tstr);
	}

	put_line(		"             CALL \"wfilechk\" USING\n");
	put_line(		"                  WISP-DECLARATIVES-STATUS,\n");
	write_line(		"                  %s,\n",prog_fstats[i]);
	if (acu_cobol)
	{
		put_line(	"                  WISP-EXTENDED-FILE-STATUS,\n");
	}
	else
	{
		put_line(	"                  WISP-EXTENDED-FILE-STATUS-1,\n");
	}
	put_line(		"                  WISP-EXTENDED-FILE-STATUS-2,\n");

	make_fld(tstr,prog_files[i],"S-");						/* Generate the S- field.		*/
	write_line("                  %s,\n",tstr);
											/* Don't use a literal.			*/
	if (!prog_vnames[i][0] || (prog_vnames[i][0] == '\"'))
	{
		make_fld(tstr,prog_files[i],"V-");					/* Generate the VOLUME field.		*/
		write_line("                  %s,\n",tstr);
	}
	else
	{
		write_line("                  %s,\n",prog_vnames[i]);
	}
											/* Don't use a literal.			*/
	if (!prog_lnames[i][0] || (prog_lnames[i][0] == '\"'))
	{
		make_fld(tstr,prog_files[i],"L-");
		write_line("                  %s,\n",tstr);
	}
	else
	{
		write_line("                  %s,\n",prog_lnames[i]);
	}

	if (!prog_fnames[i][0] || (prog_fnames[i][0] == '\"'))	 			/* Don't use a literal.			*/
	{
		make_fld(tstr,prog_files[i],"F-");
		write_line("                  %s,\n",tstr);
	}
	else
	{
		write_line("                  %s,\n",prog_fnames[i]);
	}

	make_fld(tstr,prog_files[i],"N-");
	write_line("                  %s,\n",tstr);
	put_line(  "                  WISP-CURRENT-FILE-ID,\n");
	write_line("                  WISP-APPLICATION-NAME%s",end_str);

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
		write_line("       %s SECTION.\n",fld);					/* Already have DECLARATIVES.		*/
		put_line  ("           USE AFTER STANDARD ERROR PROCEDURE ON\n");
		write_line("               %s.\n",prog_files[i]);
		make_fld(fld,prog_files[i],"WDCL-");
		write_line("       %s.\n",fld);
		g_wfilechk(i,".\n");							/* Generate a wfilechk call.		*/
		make_fld(fld,prog_files[i],"WDX-");
		write_line("       %s.\n",fld);
 		put_line  ("           EXIT.\n");
	}
}

static int fd_already = 0;								/* Flag we haven't already done it.	*/
fd_check()										/* Check for SELECT's with no FD's.	*/
{											/* This is illegal, so if they exist, we*/
	int i;										/* Generate a file with their names, and*/
	FILE *the_file;									/* re-run WISP, who will then delete the*/
											/* SELECT statements.			*/

	if (fd_already || !prog_cnt) return(0);						/* Already did it, or no files.		*/

	fd_already = 1;									/* We are doing it now.			*/

	name_count = 0;									/* No names so far.			*/

	for (i=0; i<prog_cnt; i++)							/* Check each file.			*/
	{
		if (!(prog_ftypes[i] & HAD_FD))						/* If it didn't have and FD...		*/
		{
			strcpy(name_list[name_count++],prog_files[i]);			/* Save it in the list.			*/
		}
	}

	if (name_count)									/* There are files with no FD.		*/
	{
		the_file = fopen(work_fname,"w");					/* Now open a new file to put the new	*/
		if (!the_file)								/* list into.				*/
		{
			write_log("WISP",'F',"ERRCREPARA","Error opening Pass 2 work file.");
			exit_wisp(-1);
		}

		for (i=0; i<name_count; i++)						/* For each file...			*/
		{
			fprintf(the_file,"%s\n",name_list[i]);				/* Write it to the file.		*/
		}

		fclose(the_file);
		exit_wisp(EXIT_AND_RUN);						/* Now run WISP again, resolve the list.*/
	}
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
}

gen_missing_sections()
{
	if (mark_proc_div)
	{
		gen_missing_env();
		gen_missing_data();
	}
	else if (mark_data_div)
	{
		gen_missing_env();

		if (mark_link_sect)
		{
			gen_missing_file_sect();
			gen_missing_ws_sect();
		}
		else if (mark_ws_sect)
		{
			gen_missing_file_sect();
		}
	}
	else if (mark_io_sect)
	{
		gen_missing_config();

		if (mark_io_control)
		{
			gen_missing_file_control();
		}
	}
}

gen_missing_env()
{
	if (!mark_env_div)
	{
		put_line("       ENVIRONMENT DIVISION.\n");
		mark_env_div = 1;
	}

	gen_missing_config();

	if (!mark_io_sect)
	{
		mark_io_sect = 1;
		put_line  ("       INPUT-OUTPUT SECTION.\n");				/* OK to output it.			*/
	}

	gen_missing_file_control();

	if (!mark_io_control)
	{
		mark_io_control = 1;

		if ( vax_cobol ) io_control();						/* Generate I-O-CONTROL manual rec locks*/
	}
}

gen_missing_config()
{
	if (!mark_config_sect)
	{
		check_config();
		mark_config_sect = 1;
	}

	if (!did_figcons)							/* have we generated the figurative cons*/
	{
		init_figcons();							/* do the initialization		*/
		finish_figcons();						/* terminate it				*/
	}
}

gen_missing_file_control()
{
	if (!mark_file_control)
	{
		mark_file_control = 1;
		gen_file_control();
	}
}

gen_file_control()
{
	put_line  ("       FILE-CONTROL.\n");
	write_line("           SELECT WISP-DECLARATIVES-FILE\n");		/* And the dummy file for DISPLAY & READ*/
	put_line  ("               ASSIGN TO \"WISP-DISPFILE\".\n");
}

gen_missing_data()
{
	if (!mark_data_div)
	{
		mark_data_div = 1;
		put_line("       DATA DIVISION.\n");
	}

	gen_missing_file_sect();

	gen_missing_ws_sect();

	mark_link_sect = 1;
}

gen_missing_file_sect()
{
	if (!mark_file_sect)
	{
		mark_file_sect = 1;
		gen_file_section();
	}
}

gen_file_section()
{
	put_line  ("       FILE SECTION.\n");
	write_line("       FD  WISP-DECLARATIVES-FILE.\n");			/* Do FD for Declaratives display.	*/
	put_line  ("       01  WISP-DEC-FILE-DUMMY-RECORD  PIC X.\n");
}

gen_missing_ws_sect()
{
	if (!mark_ws_sect)
	{
		mark_ws_sect = 1;
		put_line("       WORKING-STORAGE SECTION.\n");
		ws_init();
	}

	if (!mark_gen_screens)
	{
		mark_gen_screens = 1;
		gen_screens();								/* Generate any screens.		*/
	}
}

