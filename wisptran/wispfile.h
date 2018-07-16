/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wispfile.h
**
**	Purpose:	To hold file headers used by WISP to do the translation.
**			This is not used for COBOL file processing.
**
**
**	History:
**	05/27/93	Split from wisp.h. GSL
**
*/

#ifndef WISPFILE_H
#define WISPFILE_H

#include <stdio.h>

#ifndef EXT
#define EXT extern
#endif

#ifdef unix
#define DSS_CHAR	'/'							/* Directory String Seperator (char)		*/
#define	DSS_STR		"/"							/* Directory String Seperator (string)		*/
#define PSS_CHAR	':'							/* Path String Seperator (char)			*/
#endif
#ifdef MSFS
#define DSS_CHAR	'\\'							/* Directory String Seperator (char)		*/
#define	DSS_STR		"\\"							/* Directory String Seperator (string)		*/
#define PSS_CHAR	';'							/* Path String Seperator (char)			*/
#endif

#define	USE_SPECIFIC_FILE		1
#define	USE_GENERAL_FILE		2

#define	MAX_FNAME	256
#define MAX_PATH	1024

struct A_file_struct
{
	char	name[128];
	FILE	*file;
};
typedef struct A_file_struct A_file;

struct cob_file_struct
{
	struct cob_file_struct	*next;
	A_file			*a_file;
	int			line_count;
	int			is_open;
#define FOR_INPUT	1
#define FOR_OUTPUT	2
	int			is_copybook;
};
typedef struct cob_file_struct cob_file;

/*
**	The cob_file_context represents what are the current input file and output file.
**	The context will change everytime a COPY statement is processed.
*/
struct cob_file_context_struct
{
	cob_file	*infile;
	cob_file	*outfile;
};
typedef struct cob_file_context_struct cob_file_context;

EXT cob_file_context *main_cob_context;						/* The MAIN cobol file context			*/
EXT cob_file_context *curr_cob_context;						/* The current file context			*/

EXT char in_fname[MAX_FNAME];							/* the name of the primary input file		*/
EXT char out_fname[MAX_FNAME];							/* the name of the primary output file		*/
EXT char key_fname[MAX_FNAME];							/* The file with key info.			*/
EXT char xref_fname[MAX_FNAME];							/* The file to receive cross ref information.	*/
EXT char dtp_fname[MAX_FNAME];							/* The file to hold DECLARATIVES copied paras.	*/
EXT char dcl_fname[MAX_FNAME];							/* The name of the file to hold copied paras.	*/
EXT char par_fname[MAX_FNAME];							/* The name of the file that has the para list.	*/
EXT char opt_fname[MAX_FNAME];							/* The name of the option file used.		*/
EXT char work_fname[MAX_FNAME];							/* The name of the work file.			*/
EXT char crt_fname[MAX_FNAME];							/* The name of the crt temp file		*/
EXT char read_fname[MAX_FNAME];							/* The name of the read temp file		*/

EXT char cpy_lib[18],cpy_file[18];						/* Current copy lib, file.			*/

EXT char cli_ildir[MAX_PATH];							/* the Directory name for INLIB			*/
EXT char cli_prefixpath[MAX_PATH];						/* -P prefixpath, used to find "lib/file"	*/

EXT struct rcpy_struct
{
	char	file[10];
	char	lib[10];
	char	native[80];
} rcpy_element;
EXT char *rcpy_list_ring;							/* List of copy files for xref			*/

EXT int used_wrk;								/* Flag that the workfile was used.		*/

EXT FILE *logfile;								/* Current logfile (stdout by default)	*/

EXT cob_file *dtp_file_ptr;							/* The lib file to hold declaratives.	*/
EXT cob_file *dcl_file_ptr;							/* The lib file to hold paragraphs.	*/

EXT cob_file *crt_file_ptr;							/* The temp scratch file for CRT	*/
EXT cob_file *read_file_ptr;							/* The temp scratch file for READ	*/

cob_file *open_cob_file();
cob_file_context *open_cob_context();

#endif /* WISPFILE_H */

/*
**	History:
**	$Log: wispfile.h,v $
**	Revision 1.6  1996-06-24 14:23:13-04  gsl
**	fic MSDOS for MSDOS and WINNT
**
**
**
*/
