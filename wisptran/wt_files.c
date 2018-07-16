/*
** $Id:$
** WISP - Wang Interchange Source Processor
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
*/


#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef unix
#include <unistd.h>
#endif

#ifdef WIN32
#include <direct.h>
#define chdir _chdir
#endif

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "wispfile.h"
#include "cobfiles.h"
#include "keylist.h"
#include "wmalloc.h"
#include "input.h"
#include "ring.h"



char *wfgets();


static void p_workfile(void);							/* Read the workfile.				*/
static void set_rlist(const char *filename);					/* Add a copy file to the list of copy files 	*/


static int hasext(const char *filepath)
{
	const char *ptr;

	ptr = strrchr(filepath,'.');
	if (!ptr) return(0);

	if (0==strchr(ptr,DSS_CHAR)) return(1);

	return(0);
}


int open_io()									/* Parse and open the i/o files			*/
{
	FILE *par_file;								/* The file to hold paragraph names.		*/
	int	i;

	if (NULL==getcwd( original_cwd, sizeof(original_cwd)-1 ))
	{
		printf("%%WISP-F-GETCWD Unable to get the current working directory [getcwd()].\n");
		exit_wisp(EXIT_FAST);
	}
	strcpy(new_cwd, original_cwd);

	if ( !hasext(in_fname) )		/* If no extension then add .wcb		*/
	{
		strcat( in_fname, ".wcb" );
	}

	if ( access( in_fname, 0 ) != -1 )	/* If file exists */
	{
		char* last_slash;


		/*
		** If in_fname has a path component then chdir() to the location 
		** and remove the path from in_fname.
		*/
		last_slash = strrchr(in_fname,DSS_CHAR);
		if (NULL != last_slash)
		{
			*last_slash = '\0';
			strcpy(new_cwd, in_fname);

			if (0 != chdir(new_cwd))
			{
				printf("%%WISP-F-CHDIR Unable to change directory chdir(\"%s\").\n", new_cwd);
				exit_wisp(EXIT_FAST);
			}
			strcpy(in_fname, ++last_slash);

			/* safety check */
			if ( access( in_fname, 0 ) == -1 )
			{
				printf("%%WISP-F-CHDIR Unable to access file [%s] in directory [%s].\n", 
					new_cwd, in_fname);
				exit_wisp(EXIT_FAST);
			}
		}

		strcpy(out_fname,in_fname);					/* Assemble the output file name.		*/
		out_fname[strpos(out_fname,".")] = '\0';
		strcpy(par_fname,out_fname);					/* and the paragraph file name.			*/
		strcpy(dcl_fname,out_fname);					/* and the paragraph lib name.			*/
		strcpy(dtp_fname,out_fname);					/* The declaratives lib file name.		*/
		strcpy(xref_fname,out_fname);					/* The cross ref file.				*/
		strcpy(xtab_fname,out_fname);					/* The cross ref file.				*/
		strcpy(work_fname,out_fname);					/* The work file.				*/
		strcpy(crt_fname,out_fname);
		strcpy(read_fname,out_fname);

		if (opt_keyfile_flag == USE_GENERAL_FILE)
			strcpy(opt_keyfile_fname,out_fname);			/* The key list file.				*/

		if (opt_optfile_flag == USE_GENERAL_FILE)
			strcpy(opt_optfile_fname,out_fname);			/* The options file.				*/

		if (opt_noprocess)  	strcat(out_fname,".txt");		/* Create .TXT if only copying.			*/
		else if (opt_data_conv)	strcat(out_fname,".wdc");		/* od .WDC data conversion routine.		*/
		else			strcat(out_fname,".cob");		/* or .COB if really translating.		*/
		strcat(par_fname,".par");
		strcat(dcl_fname,".dcl");					/* Declaratives copybook of non-decl		*/
		strcat(xref_fname,".xrf");
		strcat(xtab_fname,".tab");
		strcat(work_fname,".wrk");
		strcat(dtp_fname,".dtp");					/* Decl temp file.				*/
		strcat(crt_fname,".ctp");
		strcat(read_fname,".rtp");

		if (opt_keyfile_flag == USE_GENERAL_FILE)
			strcat(opt_keyfile_fname,".key");
		if (opt_optfile_flag == USE_GENERAL_FILE)
			strcat(opt_optfile_fname,".opt");
	}
	else
	{
		/*
		**	File does not exist.
		**	- don't generate any of the output files
		**	- fall thru and allow open_cob_context() to report error
		*/
		out_fname[0] = '\0';
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

			if (isdigit((int)proc_paras[proc_paras_cnt][0]))	/* Is it a number?				*/
			{
				if ( (isdigit((int)proc_paras[proc_paras_cnt][1]) && !proc_paras[proc_paras_cnt][2]) || 
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
	return 0;
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

			if (opt_gen_copylib)
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
	/*
	**	NOTE: This is not accurate because 'curr_cob_context' is updated
	**	as the INPUT files are opened and closed. 
	**	The OUTPUT files often lag behind as tokens remain in queues 
	**	and statements until the statements are written out.
	*/
	return(!writing_copybook());
}

int writing_copybook()
{
	/*
	**	NOTE: This is not accurate because 'curr_cob_context' is updated
	**	as the INPUT files are opened and closed. 
	**	The OUTPUT files often lag behind as tokens remain in queues 
	**	and statements until the statements are written out.
	*/
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

int copy_file(char* the_file)							/* just copy a file to the cob file		*/
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
	return 0;
}

static void p_workfile(void)								/* Read the workfile.			*/
{
	int i;
	char tstr[133],*scn_ptr,*prm_ptr;
	FILE *wrk_file;

	prog_dscnt = 0;									/* Initilize data dependant on work file*/

	wrk_file = fopen(work_fname,"r");						/* Try the work file.			*/

	if (!wrk_file) return;								/* No work file, no problem.		*/

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

static void chk_rlist(void)
{										/* Check the list of all copy files 		*/
	int rc;									/* in them to see if this is a repeat.		*/

	re_copy = 0;								/* Set it to false now.				*/
	if (!rcpy_list_ring) return;						/* There is no list.				*/

	strcpy(rcpy_element.file,cpy_file);
	strcpy(rcpy_element.lib ,cpy_lib );

	rc = ring_find(rcpy_list_ring,&rcpy_element,0,0);
	if ( rc == 0 ) re_copy = 1;
}

static void set_rlist(const char *filename)					/* Add a copy file to the list of copy files 	*/
{
	int rc;

	chk_rlist();
	if (re_copy) return;							/* It was a re-copy, don't add it.		*/

	if (!rcpy_list_ring) 							/* There is no list.				*/
	{
		if ( (rc = ring_open(&rcpy_list_ring,sizeof(struct rcpy_struct),10,10,rcpy_compare,1)) )
		{
			write_log("WISP",'F',"RINGOPEN","Unable to open ring [rcpy_list] rc=%d [%s]",rc,ring_error(rc));
			exit_wisp(EXIT_WITH_ERR);
		}
	}

	strcpy(rcpy_element.file,cpy_file);
	strcpy(rcpy_element.lib ,cpy_lib );
	strcpy(rcpy_element.native ,filename );

	if ((rc = ring_add(rcpy_list_ring,0,&rcpy_element)) )
	{
		write_log("WISP",'F',"RINGADD","Unable to add to ring [rcpy_list] rc=%d [%s]",rc,ring_error(rc));
		exit_wisp(EXIT_WITH_ERR);
	}
}

int trimname(char* filename)								/* Trim the path etc off the file name.		*/
{
	char	buff[100];
	int	pos;

#ifdef unix
	for ( pos = strlen(filename) - 1; pos >= 0; pos-- )
	{
		if ( filename[pos] == '/' ) break;
	}
	strcpy(buff,&filename[pos+1]);
#endif

#ifdef WIN32
	for ( pos = strlen(filename) - 1; pos >= 0; pos-- )
	{
		if ( filename[pos] == '\\' ) break;
	}
	strcpy(buff,&filename[pos+1]);
#endif

	strcpy(filename,buff);
	return 0;
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

	if (opt_log_stats)
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

			if (opt_log_stats)
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

int dbfile_add_item(char* select_name, char* table_name)
{
	struct dbfile_item_struct *item;

	item = wmalloc(sizeof(struct dbfile_item_struct));

	item->next = dbfile_item_list;
	item->select = wdupstr(select_name);
	uppercase(item->select);
	item->table = wdupstr(table_name);
	dbfile_item_list = item;
	return 0;
}

struct dbfile_item_struct *get_dbfile_item(select_name)
char *select_name;
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
/*
**	History:
**	$Log: wt_files.c,v $
**	Revision 1.34  2009/10/18 20:22:58  gsl
**	Copyright
**	
**	Revision 1.33  2003/12/03 16:18:48  gsl
**	Fix so native screen fields and screen sections don't get generated in a copybook file.
**	
**	Revision 1.32  2003/03/11 19:25:07  gsl
**	Move the load_option_file() and oad_key_file() calls to the main()
**	
**	Revision 1.31  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.30  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.29  2003/02/25 21:56:03  gsl
**	cleanup some global "parm" variables
**	
**	Revision 1.28  2003/02/04 18:43:32  gsl
**	fix -Wall warnings
**	
**	Revision 1.27  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.26  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.25  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.24  2003/01/15 18:23:33  gsl
**	add #SIGN_TRAILING_SEPARATE support for MF
**	
**	Revision 1.23  2002/12/12 21:31:29  gsl
**	When #NATIVE and Acubol then set the opt_native_acu and
**	the opt_getlastfileop options flags
**	
**	Revision 1.22  2002/09/04 18:12:17  gsl
**	LINUX unistd
**	
**	Revision 1.21  2002/08/13 18:47:28  gsl
**	#DELETE_DATA_RECORDS
**	
**	Revision 1.20  2002/08/12 20:13:51  gsl
**	quotes and literals
**	
**	Revision 1.19  2002/07/26 17:53:41  gsl
**	FIx filename extension test.
**	
**	Revision 1.18  2002/07/25 17:03:41  gsl
**	MSFS->WIN32
**	
**	Revision 1.17  2002/07/24 21:41:03  gsl
**	Change directory to the location of the source file if a path is given.
**	
**	Revision 1.16  2002/06/21 20:49:33  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.15  2002/05/22 20:25:59  gsl
**	#KEEP_CONFIG_COMPUTER and #TRANSLATE_CONFIG_COMPUTER options
**	
**	Revision 1.14  2002/05/16 21:49:13  gsl
**	add new opt file options
**	rework logic
**	
**	Revision 1.13  2001-09-13 10:06:02-04  gsl
**	Remove VSM ifdefs
**	add xtab file
**
**	Revision 1.12  1999-09-07 10:40:39-04  gsl
**	Make local routines static and fix prototypes
**
**	Revision 1.11  1996-08-30 21:56:18-04  gsl
**	drcs update
**
**
**
*/
