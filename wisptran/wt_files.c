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
#include "scrn.h"
#include "wispfile.h"
#include "cobfiles.h"
#include "keylist.h"
#include "wmalloc.h"

#include <stdio.h>

#ifdef VMS
#include <descrip.h>
#include <file.h>
#else
#include <sys/types.h>
#include <fcntl.h>
#endif

#ifdef VMS
#define WISP_OPTION_FILE	"WISP.OPT"
#define WISP_KEY_FILE		"WISP.KEY"
#else
#define WISP_OPTION_FILE	"wisp.opt"
#define WISP_KEY_FILE		"wisp.key"
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
	FILE *par_file;								/* The file to hold paragraph names.	*/
	int	i;
#ifdef VMS
	char inp_file[256];
	char s_inp_file[256];

	int f_context;

	$DESCRIPTOR(i_file,inp_file);
	$DESCRIPTOR(si_file,s_inp_file);
	$DESCRIPTOR(d_file,"*.wcb");

	strcpy(inp_file,in_fname);						/* Copy the name gotten from the cli.		*/

	for (i = strlen(inp_file); i<256; i++)					/* Pad with spaces.				*/
		inp_file[i] = ' ';

	f_context = 0;

	lib$find_file(&i_file,&si_file,&f_context,&d_file,0,0,0);		/* Look for the file.				*/

	s_inp_file[strpos(s_inp_file," ")] = '\0';				/* Null terminate the name.			*/

	write_log("WISP",'I',"FILENAME","parsed file is %s",s_inp_file);

	strcpy(in_fname,s_inp_file);						/* Copy the name back to the global.		*/

	i = strpos(in_fname,"]");						/* Get the NAME portion.			*/
	if (i == -1)								/* Name didn't translate			*/
#else
	if ( strpos( in_fname, "." ) == -1 )					/* If no extension then add .wcb		*/
	{
		strcat( in_fname, ".wcb" );
	}
	if ( access( in_fname, 0 ) == -1 )					/* If file doesn't exist.			*/
#endif
	{
		out_fname[0] = '\0';
	}
	else
	{
#ifdef VMS
		strcpy(out_fname,&in_fname[i+1]);				/* Assemble the output file name.		*/
#else
		strcpy(out_fname,in_fname);					/* Assemble the output file name.		*/
#endif
		out_fname[strpos(out_fname,".")] = '\0';
		strcpy(par_fname,out_fname);					/* and the paragraph file name.			*/
		strcpy(dcl_fname,out_fname);					/* and the paragraph lib name.			*/
		strcpy(dtp_fname,out_fname);					/* The declaratives lib file name.		*/
		strcpy(xref_fname,out_fname);					/* The cross ref file.				*/
		strcpy(work_fname,out_fname);					/* The work file.				*/
		strcpy(crt_fname,out_fname);
		strcpy(read_fname,out_fname);

		if (do_keyfile == USE_GENERAL_FILE)
			strcpy(key_fname,out_fname);				/* The key list file.				*/

		if (do_optfile == USE_GENERAL_FILE)
			strcpy(opt_fname,out_fname);				/* The options file.				*/

#ifdef OLD_VMS
		if (copy_only)  strcat(out_fname,".TXT;");			/* Create .TXT if only copying.			*/
		else		strcat(out_fname,".COB;");			/* or .COB if really translating.		*/
		strcat(par_fname,".PAR;");
		strcat(dcl_fname,".DCL;");
		strcat(dtp_fname,".DTP;");
		strcat(xref_fname,".XRF;");
		strcat(work_fname,".WRK;");

		if (do_keyfile == USE_GENERAL_FILE)
			strcat(key_fname,".KEY;");
		if (do_optfile == USE_GENERAL_FILE)
			strcat(opt_fname,".OPT;");
#endif
		if (copy_only)  	strcat(out_fname,".txt");		/* Create .TXT if only copying.			*/
		else if (data_conv)	strcat(out_fname,".wdc");		/* od .WDC data conversion routine.		*/
		else			strcat(out_fname,".cob");		/* or .COB if really translating.		*/
		strcat(par_fname,".par");
		strcat(dcl_fname,".dcl");					/* Declaratives copybook of non-decl		*/
		strcat(xref_fname,".xrf");
		strcat(work_fname,".wrk");
		strcat(dtp_fname,".dtp");					/* Decl temp file.				*/
		strcat(crt_fname,".ctp");
		strcat(read_fname,".rtp");

		if (do_keyfile == USE_GENERAL_FILE)
			strcat(key_fname,".key");
		if (do_optfile == USE_GENERAL_FILE)
			strcat(opt_fname,".opt");
	}

	write_log("WISP",'I',"OUTFILE","output file is %s",out_fname);

	main_cob_context = open_cob_context(in_fname,out_fname);		/* Open the COBOL file context			*/
	curr_cob_context = main_cob_context;					/* Current context is the MAIN context		*/

	par_file = fopen(par_fname,"r");					/* try to open the paragraph file		*/
	if (par_file)
	{
		while (wfgets(proc_paras[proc_paras_cnt],sizeof(proc_paras[proc_paras_cnt]),par_file))
		{								/* Read the names of the paragraphs.		*/
			i = strpos(proc_paras[proc_paras_cnt],"\n");
			if (i) proc_paras[proc_paras_cnt][i] = '\0';		/* remove newlines.				*/

			if (isdigit(proc_paras[proc_paras_cnt][0]))		/* Is it a number?				*/
			{
				if ( (isdigit(proc_paras[proc_paras_cnt][1]) && !proc_paras[proc_paras_cnt][2]) || 
					!proc_paras[proc_paras_cnt][1] )
				{
					sscanf(proc_paras[proc_paras_cnt],"%d",&i);
					scrn_flags[i] |= SCRN_IN_DECLARATIVES;	/* Set the screens declaratives flag.		*/
				}
			}
			proc_paras_cnt++;
		}
		fclose(par_file);
	}

	p_workfile();								/* Process the work file.			*/
	p_optfile();								/* Process the options file.			*/
	p_keyfile();								/* Process the keylist file.			*/

}

/*
**	Routine:	open_cob_context()
**
**	Function:	To establish a new cobol file context by opening new input files.
**
**	Description:	The cobol context is a data structure that describes what file
**			COBOL lines are being read from and what file they should be
**			written to.
**
**			At program start and for COPY statements this routine is called
**			with a new input file and corresponding output file.  This routine
**			opens the files and creates a new context.  If we are not generating
**			copybooks then the output file is not opened and the context points
**			to the main output file.
**
**			If an open of a input copybook fails the context is set to the 
**			main cobol context. (If a open output fail WISP will abort.)
**
**			These contexts are "atomic", they are malloced here and are never
**			freed so can be pointed at by tokens.
**
**	Arguments:
**	infile		The native input file name.
**	outfile		The corresponding native output file name.
**
**	Globals:
**	main_cob_context
**
**	Return:		A new context pointer.
**
**	Warnings:	None
**
**	History:
**	05/28/93	Written by GSL
**
*/
cob_file_context *open_cob_context(infile,outfile)
char	*infile;
char	*outfile;
{
	cob_file_context *context;

	context = (cob_file_context *)wmalloc(sizeof(cob_file_context));

	if (!main_cob_context)
	{
		context->infile  = open_cob_file(infile,FOR_INPUT,0);
		if (!context->infile) exit_with_err();				/* Error opening main input file		*/
		context->outfile = open_cob_file(outfile,FOR_OUTPUT,0);
		if (!context->outfile) exit_with_err();				/* Error opening main output file		*/
	}
	else
	{
		context->infile  = open_cob_file(infile,FOR_INPUT,1);
		if (context->infile)
		{
			set_rlist(infile);
			cpy_seq = 0;						/* Reset the copy sequence counter.		*/

			if (copylib)
			{
				context->outfile = open_cob_file(outfile,FOR_OUTPUT,1);
				if (!context->outfile) exit_with_err();
			}
			else
			{
				context->outfile = main_cob_context->outfile;
			}
		}
		else
		{
			context->infile  = main_cob_context->infile;
			context->outfile = main_cob_context->outfile;
		}
	}
	return( context );
}

/*
**	Note On ATOMIC data elements:
**	============================
**	The memory for this data is atomic, it is malloced once and exists for the
**	duration of the process.  This means you can set a pointer to the memory
**	and know it will not be freed or changed ever.  These are used in tokens
**	to maintain which file the token came from.  The token may exist across
**	opening and closing of copybooks so we can't point to a "current state"
**	variable yet we don't want the overhead of the filename in each token.
**	Additionally we can determine if in the same file be comparing the pointers
**	instead of a full name comparison because tokens from the same file
**	will all point to the same atomic file data structure.
*/

int writing_cob_main()
{
	return(!writing_copybook());
}

int writing_copybook()
{
	return(curr_cob_context->outfile->is_copybook);
}

int reading_copybook()
{
	return(curr_cob_context->infile->is_copybook);
}

char *context_infile_name(context)
cob_file_context *context;
{
	if (context && context->infile && context->infile->a_file)
	{
		return(context->infile->a_file->name);
	}
	else
	{
		return("(UNKNOWN)");
	}
}

char *context_outfile_name(context)
cob_file_context *context;
{
	if (context && context->outfile && context->outfile->a_file)
	{
		return(context->outfile->a_file->name);
	}
	else
	{
		return("(UNKNOWN)");
	}
}

int context_infile_count(context)
cob_file_context *context;
{
	if (context && context->infile)
	{
		return(context->infile->line_count);
	}
	else
	{
		return(0);
	}
}

int curr_context_infile_count()
{
	return(context_infile_count(curr_cob_context));
}

cob_file_context *get_curr_cob_context()
{
	return(curr_cob_context);
}

copy_file(the_file)								/* just copy a file to the cob file		*/
char *the_file;
{
	cob_file *cob_file_ptr;
	char	input_line[128];

	cob_file_ptr = open_cob_file(the_file,FOR_INPUT,1);			/* open the file				*/

	if (cob_file_ptr)
	{
		while(0==get_clean_cobol_line(input_line,sizeof(input_line),cob_file_ptr->a_file))
		{
			cob_file_ptr->line_count++;
			tput_line(input_line);					/* copy it to the output file			*/
		}
		close_cob_file(cob_file_ptr);
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
	FILE *opt_file;								/* The option file.				*/

	if (!do_optfile) return(1);

	opt_file = fopen(opt_fname,"r");					/* Try the local option file.			*/

	if (!opt_file)
	{
		write_log("WISP",'F',"CANTFINDOPT","Option file %s not found.",opt_fname);
		exit_wisp(EXIT_WITH_ERR);
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

		for (i=0; i<MAX_PARMS; i++) o_parms[i][0] = (char)0;		/* Clear the o_parms				*/

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
#define CHANGE_STOP_RUN		250
#define DBFILE			267

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

		if (k == -1)
		{		/* 250	     260       270	 280	   290	     300       310	 320			*/
/*				    01234567890123456789012345678901234567890123456789012345678901234567890			*/
			k = strpos("#CHANGE_STOP_RUN #DBFILE ",o_parms[0]);
			if (k != -1) k += 250;
		}

		switch (k)
		{
			case COPY_DECLARATIVE:
			{
				if (proc_performs_cnt == MAX_PARAGRAPHS)
				{
					write_log("WISP",'E',"MAXPARDECL","Maximum DECLARATIVES paragraphs to copy exceeded.");
					break;
				}

				strcpy(proc_performs[proc_performs_cnt++],o_parms[1]);	
										/* Save the name of the performed procedure.	*/
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

			case CHANGE_STOP_RUN:
			{
				changestop = 1;					/* Change STOP RUN statements to EXIT PROGRAM	*/
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
						exit_wisp(EXIT_WITH_ERR);
					}
				}
				
				if (rc = ring_add(using_ufb_ring,0,&using_ufb_item))	/* Store the USING_UFB into the ring	*/
				{
					write_log("WISP",'F',"RINGADD",
						"Unable to add to ring [using_ufb_ring] rc=%d [%s]",rc,ring_error(rc));
					exit_wisp(EXIT_WITH_ERR);
				}
				break;
			}

			case DBFILE:
			{
				dbfile_add_item(o_parms[1], o_parms[2]);
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
		exit_wisp(EXIT_WITH_ERR);
	}

scan_key:

	use_wisp = 0;

	if (!key_file) return(1);						/* No key file.					*/

	while (wfgets(tstr,132,key_file))
	{									/* Read the names of the fields.		*/
		scn_ptr = tstr;
		uppercase(tstr);						/* Shift whole string to uppercase		*/

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
						exit_wisp(EXIT_WITH_ERR);
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
			exit_wisp(EXIT_WITH_ERR);
		}
	}

	strcpy(rcpy_element.file,cpy_file);
	strcpy(rcpy_element.lib ,cpy_lib );
	strcpy(rcpy_element.native ,filename );

	if (rc = ring_add(rcpy_list_ring,0,&rcpy_element) )
	{
		write_log("WISP",'F',"RINGADD","Unable to add to ring [rcpy_list] rc=%d [%s]",rc,ring_error(rc));
		exit_wisp(EXIT_WITH_ERR);
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

/*
	wfgets		This is a frontend to fgets
*/
char *wfgets(s, n, stream)
char	*s;
int	n;
FILE	*stream;
{
	char	*ptr;

	ptr = fgets(s, n, stream);
	return(ptr);
}

static cob_file *cob_file_list_start = NULL;

cob_file *open_cob_file(filename,openmode,copybook)
char	*filename;
int	openmode;
int	copybook;
{
	FILE		*tempfile;
	char		*open_mode_ptr;
	cob_file	*cob_file_ptr;

	switch(openmode)
	{
	case FOR_INPUT:
		open_mode_ptr = "r";
		break;
	case FOR_OUTPUT:
		open_mode_ptr = "w";
		break;
	default:
		write_log("WISP",'F',"COBOPEN","Invalid open mode %d",openmode);
		exit_with_err();
		break;
	}

	tempfile = fopen(filename,open_mode_ptr);
	if (!tempfile)								/* No good, report the error & return.		*/
	{
		write_log("WISP",'S',"CANTOPEN","Unable to open file %s for %s.",
						filename,(FOR_INPUT==openmode)?"INPUT":"OUTPUT");
		delayed_exit_with_err();
		return(NULL);
	}

	/*
	**	Create a cob_file area, add it to the list (in reverse order), then
	**	fill in all the data.
	*/
	cob_file_ptr = (cob_file *)wmalloc(sizeof(cob_file));
	cob_file_ptr->next = cob_file_list_start;
	cob_file_list_start = cob_file_ptr;

	cob_file_ptr->a_file = (A_file *)wmalloc(sizeof(A_file));
	strcpy(cob_file_ptr->a_file->name,filename);
	cob_file_ptr->a_file->file = tempfile;

	cob_file_ptr->line_count = 0;
	cob_file_ptr->is_open = openmode;
	cob_file_ptr->is_copybook = copybook;

	write_log("WISP",'I',"OPENCOB","Open %s file %s.",
				(FOR_INPUT==cob_file_ptr->is_open) ? "INPUT":"OUTPUT",
				cob_file_ptr->a_file->name);

	if (log_stats)
	{
		printf("Opened file %s for %s %s.\n",
					cob_file_ptr->a_file->name,
					(cob_file_ptr->is_copybook) ? "COPY":"MAIN",
					(FOR_INPUT==cob_file_ptr->is_open) ? "input":"output");
	}

	return(cob_file_ptr);
}

int close_cob_file(cob_file_ptr)
cob_file	*cob_file_ptr;
{
	if (!cob_file_ptr) return(0);

	if (cob_file_ptr->is_open)
	{
		if (cob_file_ptr->a_file && cob_file_ptr->a_file->file)
		{
			fclose(cob_file_ptr->a_file->file);
			cob_file_ptr->a_file->file = NULL;
			write_log("WISP",'I',"CLOSECOB","Closed %s file %s count=%d.",
				(FOR_INPUT==cob_file_ptr->is_open) ? "INPUT":"OUTPUT",
				cob_file_ptr->a_file->name, cob_file_ptr->line_count);

			if (log_stats)
			{
				printf("Closed file %s for %s %s count=%d.\n",
					cob_file_ptr->a_file->name,
					(cob_file_ptr->is_copybook) ? "COPY":"MAIN",
					(FOR_INPUT==cob_file_ptr->is_open) ? "input":"output",
					cob_file_ptr->line_count);
			}
		}

		cob_file_ptr->is_open = 0;
	}

	return(0);
}

int close_all_cob_files()
{
	cob_file	*cob_file_ptr;

	cob_file_ptr = cob_file_list_start;
	while(cob_file_ptr)
	{
		close_cob_file(cob_file_ptr);
		cob_file_ptr = cob_file_ptr->next;
	}
	return(0);
}

struct dbfile_item_struct
{
	struct dbfile_item_struct *next;
	char	*select;
	char	*table;
};

static struct dbfile_item_struct *dbfile_item_list = NULL;

int dbfile_add_item(select_name, table_name)
char	*select_name;
char	*table_name;
{
	struct dbfile_item_struct *item;

	item = wmalloc(sizeof(struct dbfile_item_struct));

	item->next = dbfile_item_list;
	item->select = wdupstr(select_name);
	uppercase(item->select);
	item->table = wdupstr(table_name);
	dbfile_item_list = item;
}

struct dbfile_item_struct *get_dbfile_item(select_name)
{
	struct dbfile_item_struct *item;

	item = dbfile_item_list;
	while(item)
	{
		if (noncase_eq(select_name,item->select)) return(item);
		item = item->next;
	}
	return(NULL);
}

int is_dbfile(select_name)
char	*select_name;
{
	if (get_dbfile_item(select_name)) return(1);
	return(0);
}

void dbfile_write_mark(select_name)
char	*select_name;
{
	struct dbfile_item_struct *item;

	item = get_dbfile_item(select_name);
	if (item)
	{
		if (item->table && item->table[0])
		{
			tput_scomment("$XFD FILE=%s",item->table);
		}
		else
		{
			tput_scomment("$XFD FILE=%s",item->select);
		}
	}
}
