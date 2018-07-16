			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"
#include <stdio.h>

#ifdef VMS
#include <descrip.h>
#include <file.h>
#else
#include <sys/types.h>
#include <fcntl.h>
#endif

char	*using_ufb_ring = 0;
struct	using_ufb_struct
{
	char	name[40];
} using_ufb_item;
int using_ufb_cmp();

char *wfgets();

open_io()									/* Parse and open the i/o files			*/
{
	int	i;
#ifdef VMS
	char inp_file[256];
	char s_inp_file[256];

	int f_context;

	$DESCRIPTOR(i_file,inp_file);
	$DESCRIPTOR(si_file,s_inp_file);
	$DESCRIPTOR(d_file,"*.wcb");

	strcpy(inp_file,fname);							/* Copy the name gotten from the cli.		*/

	for (i = strlen(inp_file); i<256; i++)					/* Pad with spaces.				*/
		inp_file[i] = ' ';

	f_context = 0;

	lib$find_file(&i_file,&si_file,&f_context,&d_file,0,0,0);		/* Look for the file.				*/

	s_inp_file[strpos(s_inp_file," ")] = '\0';				/* Null terminate the name.			*/

	write_log("WISP",'I',"FILENAME","parsed file is %s",s_inp_file);

	strcpy(fname,s_inp_file);						/* Copy the name back to the global.		*/

	i = strpos(fname,"]");							/* Get the NAME portion.			*/
	if (i == -1)								/* Name didn't translate			*/
#else
	if ( strpos( fname, "." ) == -1 )					/* If no extension then add .wcb		*/
	{
		strcat( fname, ".wcb" );
	}
	if ( access( fname, 0 ) == -1 )						/* If file doesn't exist.			*/
#endif
	{
		out_fname[0] = '\0';
	}
	else
	{
#ifdef VMS
		strcpy(out_fname,&fname[i+1]);					/* Assemble the output file name.		*/
#else
		strcpy(out_fname,fname);					/* Assemble the output file name.		*/
#endif
		out_fname[strpos(out_fname,".")] = '\0';
		strcpy(par_fname,out_fname);					/* and the paragraph file name.			*/
		strcpy(lib_file,out_fname);					/* and the paragraph lib name.			*/
		strcpy(decl_fname,out_fname);					/* The declaratives lib file name.		*/
		strcpy(xref_fname,out_fname);					/* The cross ref file.				*/
		strcpy(work_fname,out_fname);					/* The work file.				*/

		if (do_keyfile == USE_GENERAL_FILE)
			strcpy(key_fname,out_fname);				/* The key list file.				*/

		if (do_optfile == USE_GENERAL_FILE)
			strcpy(opt_fname,out_fname);				/* The options file.				*/

#ifdef VMS
		if (copy_only)  strcat(out_fname,".TXT;");			/* Create .TXT if only copying.			*/
		else		strcat(out_fname,".COB;");			/* or .COB if really translating.		*/
		strcat(par_fname,".PAR;");
		strcat(lib_file,".DCL;");
		strcat(decl_fname,".DTP;");
		strcat(xref_fname,".XRF;");
		strcat(work_fname,".WRK;");

		if (do_keyfile == USE_GENERAL_FILE)
			strcat(key_fname,".KEY;");
		if (do_optfile == USE_GENERAL_FILE)
			strcat(opt_fname,".OPT;");
#else
		if (copy_only)  strcat(out_fname,".txt");			/* Create .TXT if only copying.			*/
		else		strcat(out_fname,".cob");			/* or .COB if really translating.		*/
		strcat(par_fname,".par");
		strcat(lib_file,".dcl");					/* Declaratives copybook of non-decl		*/
		strcat(decl_fname,".dtp");					/* Decl temp file.				*/
		strcat(xref_fname,".xrf");
		strcat(work_fname,".wrk");

		if (do_keyfile == USE_GENERAL_FILE)
			strcat(key_fname,".key");
		if (do_optfile == USE_GENERAL_FILE)
			strcat(opt_fname,".opt");
#endif
	}

	write_log("WISP",'I',"OUTFILE","output file is %s",out_fname);

	open_input(fname);							/* Open the first file for access.		*/

	par_file = fopen(par_fname,"r");					/* try to open the paragraph file		*/
	if (par_file)
	{
		while (wfgets(par_name[par_count],sizeof(par_name[par_count]),par_file))
		{								/* Read the names of the paragraphs.		*/
			i = strpos(par_name[par_count],"\n");
			if (i) par_name[par_count][i] = '\0';			/* remove newlines.				*/

			if (isdigit(par_name[par_count][0]))			/* Is it a number?				*/
			{
				if ( (isdigit(par_name[par_count][1]) && !par_name[par_count][2]) || !par_name[par_count][1])
				{
					sscanf(par_name[par_count],"%d",&i);
					scrn_flags[i] |= SCRN_IN_DECLARATIVES;	/* Set the screens declaratives flag.		*/
				}
			}
			par_count++;
		}
		fclose(par_file);

		par_file = fopen(lib_file,"w");					/* Now open the paragraph file for out		*/

		if (!par_file)
		{
			write_log("WISP",'E',"CANTOPENLIB","Unable to open paragraph copy lib for output.");
			exit_wisp(-1);
		}
	}

	p_workfile();								/* Process the work file.			*/
	p_optfile();								/* Process the options file.			*/
	p_keyfile();								/* Process the keylist file.			*/

	open_output(out_fname);							/* Open the output file				*/

}


int open_input(filename)							/* Open a file while remembering the Previous	*/
char *filename;
{

	FILE	*tempfile;
	char	*p;

#ifdef unix
	for( p=filename; *p; *p=tolower(*p),p++ );
#endif

	tempfile = fopen(filename,"r");						/* Try to open it.				*/

	if (open_files) 							/* See about putting it into the read list.	*/
		set_rlist(filename);

	if (!tempfile)								/* No good, report the error & return.		*/
	{
		write_log("WISP",'E',"CANTOPEN","Unable to open file %s for input.",filename);
		if (open_files) return(0);
		exit(1);							/* First file, error-exit.			*/
	}

	if (log_stats)
	{
		printf("Opened %s for input.\n",filename);			/* write status					*/
	}

	had_read = 0;								/* Didn't find a read yet.			*/

	if (open_files)
	{
		num_copy = 0;
		strcpy(copy_name,filename);					/* Save the copy file name			*/
		trimname(copy_name);
		cpy_seq = 0;							/* Reset the copy sequence counter.		*/
		file_ptrs[open_files-1] = infile;				/* Save the ptr to the current file structure	*/
	}
	else
	{
		num_main = 0;
		strcpy(main_name,filename);
		trimname(main_name);
	}
	open_files++;								/* Count it					*/
	infile = tempfile;							/* And set the infile pointer to it.		*/
	return(0);
}

close_input()
{

	if (open_files)								/* Make sure there is a file to close		*/
	{
		fclose(infile);							/* Yep, close the current file			*/
		open_files--;							/* One less to worry about			*/
		if (open_files)							/* Other files open?				*/
		{
			num_copy = 0;
			copy_name[0] = '\0';
			infile = file_ptrs[open_files-1];			/* Yes, return them to working status		*/
		}
	}
}
open_output(filename)								/* Open an output file & remember the last file	*/
char *filename;
{

	int tempfile,tskip;
	char *p;

#ifdef unix
	for( p=filename; *p; *p=tolower(*p),p++ );
#endif

	wflush();								/* flush the current file			*/

	tskip = 0;								/* Not skipping yet				*/

	if (out_files)								/* If there are already files, must be copy lib	*/
	{
		tempfile = open(filename,O_RDONLY);				/* First see if it exists.			*/
		if (tempfile == -1)
		{								/* Does not exist, so open it new.		*/
#ifdef VMS
			tempfile = creat(filename,0);				/* Try to open it.				*/
#else
			tempfile = creat(filename,00666);			/* Try to open it.				*/
#endif
		}
		else
		{
			tskip = 1;						/* Flag it as a skip.				*/
		}
	}
	else
	{
#ifdef VMS
		tempfile = creat(filename,0);					/* Try to open it.				*/
#else
		tempfile = creat(filename,00666);				/* Try to open it.				*/
#endif
	}

	if (tempfile == -1)							/* No good, report the error and return.	*/
	{
		write_log("WISP",'E',"CANTOPEN","Unable to open file %s for output.",filename);
		exit_wisp(-1);
		return(0);
	}

	if (log_stats && !tskip)
	{
		printf("Opened %s for output.\n",filename);			/* write status					*/
	}

	write_log("WISP",'I',"OPNOUTFILE","Opened file %s for output.\n",filename);

	if (out_files)
	{
		o_file_ptrs[out_files-1] = outfile;				/* Save the ptr to the current file structure	*/
		o_skip[out_files-1] = skiplib;					/* Save the skiplib status			*/
	}
	out_files++;								/* Count it					*/
	outfile = tempfile;							/* And set the infile pointer to it.		*/
	skiplib = tskip;							/* Set the correct skip flag			*/

}

close_output()
{

	if (out_files)								/* Make sure there is a file to close		*/
	{
		wflush();							/* flush our output buffer			*/
		close(outfile);							/* Yep, close the current file			*/
		write_log("WISP",'I',"CLOSEOUT","Closed output file.");
		out_files--;							/* One less to worry about			*/
		if (out_files)							/* Other files open?				*/
		{
			outfile = o_file_ptrs[out_files-1];			/* Yes, return them to working status		*/
			skiplib = o_skip[out_files-1];				/* skip flag too				*/
		}
	}
}




copy_file(the_file)								/* just copy a file to outfile			*/
char *the_file;
{
	FILE *infile;
	int lstat;
	char input_line[133];

	infile = fopen(the_file,"r");						/* open the file				*/

	if (infile)
	{
		do
		{
			lstat = (int)wfgets(input_line,132,infile);		/* Read a line					*/
			if (lstat)
			{
				put_line(input_line);				/* copy it to the output file			*/
			}
		}
		while(lstat);							/* till all done				*/
		fclose(infile);
	}
	else
	{
		write_log("WISP",'E',"ERRORCOPY","ERROR copying %s",the_file);
	}
}

p_optfile()									/* Read the option file.			*/
{
	int i,k,wisp_opt;
	char tstr[133],*scn_ptr,*prm_ptr;

	if (!do_optfile) return(1);

	opt_file = fopen(opt_fname,"r");					/* Try the local option file.			*/

	if (!opt_file)
	{
		write_log("WISP",'F',"CANTFINDOPT","Option file %s not found.",opt_fname);
		exit_wisp(-1);
	}

	if (!opt_file)
	{
		opt_file = fopen(WISP_OPTION_FILE,"r");				/* Try the global option file.			*/
	}



scan_opt:
	wisp_opt = 0;


	if (!opt_file) return(1);						/* No option file.				*/

	while (wfgets(tstr,132,opt_file))
	{									/* Read the names of the paragraphs.		*/
		scn_ptr = tstr;

		i = 0;
		do								/* scan the line and extract the parms		*/
		{								/* skip over spaces, commas, newlines , tabs	*/
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
			{
				scn_ptr++;
			}
			else							/* copy this parm				*/
			{
				prm_ptr = o_parms[i++];				/* Point to current parameter			*/
				do
				{						/* copy till next whitespace			*/
					*prm_ptr++ = *scn_ptr++;
				} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));
				*prm_ptr = '\0';				/* null terminate				*/
			}
		} while (*scn_ptr);						/* till a null 					*/

#define	COPY_DECLARATIVE	0
#define COPY_PROCEDURE		18
#define INCLUDE_FILE		34
#define BLANK_WHEN_ZERO		48
#define NO_FILE_STATUS		65
#define KEEP_STOP_RUN		70
#define DELETE_RECORD_CONTAINS	85
#define NO_WS_BLANK_WHEN_ZERO	109
#define TRAP_START_TIMEOUTS	132
#define NO_WORD_SWAP		150
#define NO_SEQ_SEQ_OPTIONAL	164
#define NO_DISPLAY_PROCESSING	185
#define SORTFILE		208
#define USING_UFB 		219

/*				      1         2         3         4         5         6         7				*/
/*			    01234567890123456789012345678901234567890123456789012345678901234567890				*/
		k = strpos("#COPY_DECLARATIVE #COPY_PROCEDURE #INCLUDE_WISP #BLANK_WHEN_ZERO #NO_FILE_STATUS ",o_parms[0]);
		if (k == -1)
		{		/*  7	      8		9	 100	   110	     120       130       140			*/
/*				    01234567890123456789012345678901234567890123456789012345678901234567890			*/
			k = strpos("#KEEP_STOP_RUN #DELETE_RECORD_CONTAINS #NO_WS_BLANK_WHEN_ZERO #TRAP_START_TIMEOUTS ",o_parms[0]);
			if (k != -1) k += 70;
		}

		if (k == -1)
		{		/* 150	     160       170	 180	   190	     200       210	 220			*/
/*				    01234567890123456789012345678901234567890123456789012345678901234567890			*/
			k = strpos("#NO_WORD_SWAP #NO_SEQ_SEQ_OPTIONAL #NO_DISPLAY_PROCESSING #SORT_FILE #USING_UFB ",o_parms[0]);
			if (k != -1) k += 150;
		}

		switch (k)
		{
			case COPY_DECLARATIVE:
			{
				if (ppdiv_count == MAX_PARAGRAPHS)
				{
					write_log("WISP",'E',"MAXPARDECL","Maximum DECLARATIVES paragraphs to copy exceeded.");
					break;
				}

				strcpy(perf_pdiv[ppdiv_count++],o_parms[1]);	/* Save the name of the performed procedure.	*/

				if (!decl_file)
				{
					decl_file = fopen(decl_fname,"w");	/* Now open the paragraph file for out		*/

					if (!decl_file)
					{
						write_log("WISP",'E',"CANTOPENLIB",
							"Unable to open DECLARATIVES copy lib for output.");
						exit_wisp(-1);
					}
				}
				break;
			}

			case BLANK_WHEN_ZERO:
			{
				if (blank_count == MAX_BLANK_ITEMS)
				{
					write_log("WISP",'E',"MAXBLANKITEMS","Maximum number of BLANK WHEN ZERO items exceeded.");
				}
				else
				{
					strcpy(blank_item[blank_count++],o_parms[1]);	/* Save the name of the blanked item.	*/
				}
				break;
			}

			case NO_WS_BLANK_WHEN_ZERO:
			{
				ws_blank = 0;						/* Clear the flag.			*/
				break;
			}

			case NO_FILE_STATUS:
			{
				if (nfs_count == MAX_STATUS_ITEMS)
				{
					write_log("WISP",'E',"MAXNFSTATITEMS","Maximum number of NO FILE STATUS items exceeded.");
				}
				else
				{
					strcpy(nfs_item[nfs_count++],o_parms[1]);	/* Save the name of the file.		*/
				}
				break;
			}

			case SORTFILE:
			{
				if (sf_count == MAX_SF_ITEMS)
				{
					write_log("WISP",'E',"MAXSFITEMS","Maximum number of #SORT_FILE items exceeded.");
				}
				else
				{
					strcpy(sf_item[sf_count++],o_parms[1]);		/* Save the name of the file.		*/
				}
				break;
			}

			case INCLUDE_FILE:
			{
				wisp_opt = 1;					/* Use the WISP option file.			*/
				break;
			}

			case KEEP_STOP_RUN:
			{
				keepstop = 1;					/* Don't change STOP RUN statements.		*/
				break;
			}

			case DELETE_RECORD_CONTAINS:				/* Delete the RECORD CONTAINS clause.		*/
			{
				delrecon = 1;
				break;
			}

			case TRAP_START_TIMEOUTS:				/* Insert code to trap when START's time out.	*/
			{
#ifdef OLD
				trap_start = 1;
#endif
				break;
			}

			case NO_WORD_SWAP:					/* Disable WSWAP function.			*/
			{
				swap_words = 0;
				break;
			}

			case NO_SEQ_SEQ_OPTIONAL:				/* Disable OPTIONAL in SELECT statements.	*/
			{
				use_optional = 0;
				break;
			}

			case NO_DISPLAY_PROCESSING:
			{
				proc_display = 0;				/* Disable processing of DISPLAY verb.		*/
				break;
			}

			case USING_UFB:
			{
				int	rc;
				char	ufb_buf[40];

				if ('"' == o_parms[1][0])
				{
					strcpy(using_ufb_item.name,o_parms[1]);
				}
				else
				{
					sprintf(using_ufb_item.name,"\"%s\"",o_parms[1]);
				}
#ifdef DEBUG
printf("USING_UFB=[%s]\n",using_ufb_item.name);
#endif


				if (!using_ufb_ring)
				{
					if(rc=ring_open(&using_ufb_ring,sizeof(struct using_ufb_struct),5,5,using_ufb_cmp,1))
					{
						write_log("WISP",'F',"RINGOPEN",
							"Unable to open ring [using_ufb_ring] rc=%d [%s]",rc,ring_error(rc));
						exit_wisp(-1);
					}
				}
				
				if (rc = ring_add(using_ufb_ring,0,&using_ufb_item))	/* Store the USING_UFB into the ring	*/
				{
					write_log("WISP",'F',"RINGADD",
						"Unable to add to ring [using_ufb_ring] rc=%d [%s]",rc,ring_error(rc));
					exit_wisp(-1);
				}
				break;
			}

			default:
			{
				write_log("WISP",'W',"OPTION_FILE","Error parsing option file, line is\n%s\n",tstr);
				break;
			}
		}
	}
	fclose(opt_file);

	if (wisp_opt)								/* Use the WISP option file too.		*/
	{
		opt_file = fopen(WISP_OPTION_FILE,"r");				/* Try the global option file.			*/
		goto scan_opt;							/* Go do it.					*/
	}
}

p_keyfile()									/* Read the key file.				*/
{
/*
	The keyfile now has several different enteries.

	#LEADING	keyname		- Correct keys with leading separate signs
	#COMPUTATIONAL	keyname		- Change COMP keys to PIC X
	#INCLUDE_WISP			- Include the global WISP key file
*/


	int i,k;
	char tstr[133],*scn_ptr,*prm_ptr;
	FILE *key_file;
	int use_wisp;

	if (!do_keyfile) return(1);						/* Skip it.					*/

	key_file = fopen(key_fname,"r");					/* Try the local key file.			*/

	if (!key_file && (do_keyfile == USE_SPECIFIC_FILE))
	{
		write_log("WISP",'F',"CANTFINDKEY","Key file %s not found.",key_fname);
		exit_wisp(-1);
	}

scan_key:

	use_wisp = 0;

	if (!key_file) return(1);						/* No key file.					*/

	while (wfgets(tstr,132,key_file))
	{									/* Read the names of the fields.		*/
		scn_ptr = tstr;
		for(i=0; tstr[i]; i++) tstr[i] = toupper(tstr[i]);		/* Shift whole string to uppercase		*/

		i = 0;
		do								/* scan the line and extract the parms		*/
		{								/* skip over spaces, commas, newlines , tabs	*/
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
			{
				scn_ptr++;
			}
			else							/* copy this parm				*/
			{
				short tflag;
				tflag = 0;

				if ( *scn_ptr == '#' )
				{
					if      (  0==memcmp(scn_ptr,"#LEAD",5) )
					{
						key_list[kl_count].type = KL_LEAD;
						tflag = 1;
					}
					else if (  0==memcmp(scn_ptr,"#COMP",5) )
					{
						key_list[kl_count].type = KL_COMP;
						tflag = 1;
					}
					else if (  0==memcmp(scn_ptr,"#INCL",5) )
					{
						use_wisp = 1;			/* Set flag to INCLUDE wisp global keys file	*/
					}
					else
					{
						write_log("WISP",'F',"INVALIDKEYFILE",
								"Unknown Control in Key file %5.5s.",scn_ptr);
						exit_wisp(-1);
					}
				
					if ( tflag )
					{
						for(scn_ptr +=5; 			/* Scan until white space		*/
						    	((*scn_ptr != ' ' ) && 
							 (*scn_ptr != ',' ) && 
							 (*scn_ptr != '\n') && 
							 (*scn_ptr != '\t'));  scn_ptr++);

						for(scn_ptr++; 				/* Scan thru white space		*/
						    	((*scn_ptr == ' ' ) || 
							 (*scn_ptr == ',' ) || 
							 (*scn_ptr == '\n') || 
							 (*scn_ptr == '\t'));  scn_ptr++);

					}
				}
				else						/* Assume old format & LEADING			*/
				{
					key_list[kl_count].type = KL_LEAD;
					tflag = 1;
				}

				if (tflag)					/* If we have a key				*/
				{
					prm_ptr = key_list[kl_count].name;	/* Point to current parameter			*/
					do
					{					/* copy till next whitespace			*/
						*prm_ptr++ = *scn_ptr++;
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));
					*prm_ptr = '\0';			/* null terminate				*/

					key_list[kl_count].qual[0] = '\0';	/* Get the OF qual if any.			*/
					sscanf(scn_ptr," OF %s",key_list[kl_count].qual);
					*scn_ptr = '\0';
					kl_count +=1;
				}

			}
		} while (*scn_ptr);						/* till a null 					*/
	}
	fclose(key_file);

	if (use_wisp)
	{
		key_file = fopen(WISP_KEY_FILE,"r");				/* Try the global key file.			*/
		goto scan_key;
	}
}

p_workfile()										/* Read the workfile.			*/
{
	int i,k;
	char tstr[133],*scn_ptr,*prm_ptr;
	FILE *wrk_file;

	prog_dscnt = 0;									/* Initilize data dependant on work file*/

	wrk_file = fopen(work_fname,"r");						/* Try the work file.			*/

	if (!wrk_file) return(0);							/* No work file, no problem.		*/

	while (wfgets(tstr,132,wrk_file))
	{										/* Read the names of the files.		*/
		scn_ptr = tstr;

		i = 0;
		do									/* Scan the line and extract the parms.	*/
		{									/* Skip spaces, commas, newlines, tabs.	*/
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
			{
				scn_ptr++;
			}
			else								/* copy this parm.			*/
			{
				prm_ptr = prog_dsel[prog_dscnt++];			/* Point to current parameter.		*/
				do
				{							/* copy till next whitespace.		*/
					*prm_ptr++ = *scn_ptr++;
				} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n') && (*scn_ptr != '\t'));
				*prm_ptr = '\0';					/* null terminate			*/
			}
		} while (*scn_ptr);							/* till a null 				*/
	}
	fclose(wrk_file);

	used_wrk = 1;									/* Signal we used the workfile.		*/
}



static int sav_outfile;
static int sav_skiplib;

set_main()
{
	if (out_files == 1)	return(0);						/* if there are other open files, do this	*/

	wflush();

	sav_outfile = outfile;							/* Save the file pointer.			*/
	sav_skiplib = skiplib;

	outfile = o_file_ptrs[0];						/* Set the file pointer to the main.		*/
	skiplib = o_skip[0];							/* Set the correct skip flag			*/

}

set_lib()
{
	if (out_files == 1)	return(0);					/* if there are other open files, do this	*/

	wflush();
	outfile = sav_outfile;							/* Restore the saved info.			*/
	skiplib = sav_skiplib;
}


int rcpy_compare(p1,p2)
struct rcpy_struct *p1, *p2;
{
	int	cmp;

	cmp = strcmp(p1->file, p2->file);
	if (cmp == 0)
		cmp = strcmp(p1->lib, p2->lib);
	return(cmp);
}

chk_rlist()
{										/* Check the list of all copy files 		*/
	int rc;									/* in them to see if this is a repeat.		*/

	re_copy = 0;								/* Set it to false now.				*/
	if (!rcpy_list_ring) return(0);						/* There is no list.				*/

	strcpy(rcpy_element.file,cpy_file);
	strcpy(rcpy_element.lib ,cpy_lib );

	rc = ring_find(rcpy_list_ring,&rcpy_element,0,0);
	if ( rc == 0 ) re_copy = 1;
}

set_rlist(filename)								/* Add a copy file to the list of copy files 	*/
char *filename;
{
	int rc;

	chk_rlist();
	if (re_copy) return(0);							/* It was a re-copy, don't add it.		*/

	if (!rcpy_list_ring) 							/* There is no list.				*/
	{
		if ( rc = ring_open(&rcpy_list_ring,sizeof(struct rcpy_struct),10,10,rcpy_compare,1) )
		{
			write_log("WISP",'F',"RINGOPEN","Unable to open ring [rcpy_list] rc=%d [%s]",rc,ring_error(rc));
			exit_wisp(-1);
		}
	}

	strcpy(rcpy_element.file,cpy_file);
	strcpy(rcpy_element.lib ,cpy_lib );
	strcpy(rcpy_element.native ,filename );

	if (rc = ring_add(rcpy_list_ring,0,&rcpy_element) )
	{
		write_log("WISP",'F',"RINGADD","Unable to add to ring [rcpy_list] rc=%d [%s]",rc,ring_error(rc));
		exit_wisp(-1);
	}
}

trimname(filename)								/* Trim the path etc off the file name.		*/
char	*filename;
{
	char	buff[100];
	int	pos;

#ifdef VMS
	for ( pos = strlen(filename) - 1; pos >= 0; pos-- )
	{
		if ( filename[pos] == ']' || filename[pos] == ':' ) break;
	}
	strcpy(buff,&filename[pos+1]);
	for ( pos=0; buff[pos] && buff[pos] != ';'; pos++ );
	buff[pos] = '\0';
#endif

#ifdef unix
	for ( pos = strlen(filename) - 1; pos >= 0; pos-- )
	{
		if ( filename[pos] == '/' ) break;
	}
	strcpy(buff,&filename[pos+1]);
#endif

#ifdef MSDOS
	for ( pos = strlen(filename) - 1; pos >= 0; pos-- )
	{
		if ( filename[pos] == '\\' ) break;
	}
	strcpy(buff,&filename[pos+1]);
#endif

	strcpy(filename,buff);
}


int using_ufb_cmp(p1,p2)
struct using_ufb_struct *p1, *p2;
{
	int	cmp;

	cmp = strcmp(p1->name, p2->name);
	return(cmp);
}

int using_ufb_test(item)
char	*item;
{
	int	rc;
	if (!using_ufb_ring) return(0);

	strcpy(using_ufb_item.name,item);
	rc = ring_find(using_ufb_ring,&using_ufb_item,0,0);
	return(!rc);
}
