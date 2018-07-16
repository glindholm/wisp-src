/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/*
**	File:		wperson.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Loading personality and other options.
**
**	Routines:	
**	WL_wpload()
**	WL_save_defaults()
**	WL_load_defaults()
** 	WL_get_defs();
** 	WL_set_defs();
** 	WL_write_defaults_to_file();
** 	WL_read_defaults_from_file();
** 	WL_load_options();
** 	WL_clear_progdefs();
** 	WL_set_progdefs_env();
** 	WL_save_progdefs();
** 	WL_restore_progdefs();
**	WL_get_dispfac_char()
*/

/* 		These routines are used for access to the various personality files for the WISP run-time library		*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#include <pwd.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <direct.h>
#include <io.h>
#include <process.h>
#endif

#include "idsistd.h"
#include "wperson.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wglobals.h"
#include "wanguid.h"
#include "vwang.h"
#include "osddefs.h"
#include "wexit.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wrunconf.h"
#include "wmalloc.h"
#include "paths.h"
#include "wispnt.h"
#include "link.h"
#include "wispcfg.h"
#include "submit.h"
#include "cobrun.h"
#include "assert.h"

#include <video.h>

/*
**	Structures and Defines
*/

#define		ROUTINE		83000

#define		WISP_SYSTEM_PERSON_FILE		"PERSONALITY"
#define 	WISP_USER_PERSON_FILE		"PERSONALITY"

#define WPATH_LEN	128
/*
83002	%%WPERSON-F-PID Missing WISP_PID_ENV
83004	%%WPERSON-F-TTY Missing WISP_TTY_ENV
83006	%%WPERSON-F-MALLOC Unable to malloc %s
83008	%%WPERSON-F-FACCESS Error accessing file %s    
83010	%%WPERSON-F-FOPEN wps_file unable to open %s
83012	%%WPERSON-F-FWRITE wps_file unable to write to %s
83014	%%WPERSON-F-FORMS Line too long
83016	%%WPERSON-F-LPMAP Line too long
83018	%%WPERSON-E-OPTIONS %s: [%s]
83020	%%WPERSON-E-FOPEN %s unable to open %s [errno=%d]
83022	%%WPERSON-E-FWRITE %s unable to write to %s [errno=%d]
83024	%%WPERSON-F-LOAD %s Line too long
83026	%%WPERSON-W-LOAD Found invalid FAC character [%x] - Ignored 
*/

/* NOTE: the libraries and volumes are padded to 8 or 6 chars then null terminated */

#define PF_VERSION  61591

typedef struct {
		char	prt_mode;						/* Default print mode, O, S, H, K	*/
		char	prt_class;						/* Default print class (A-Z)		*/
		int4	prt_num;						/* default printer number (000-999)	*/
		int4	prt_form;						/* Default form number (000-999)	*/
		char	inlib[9];						/* Default INLIB			*/
		char	invol[7];						/* Default INVOL			*/
		char	outlib[9];						/* Default OUTLIB			*/
		char	outvol[7];
		char	runlib[9];						/* Default RUNLIB			*/
		char	runvol[7];
		char	spoolib[9];						/* default SPOOL lib			*/
		char	spoolvol[7];
		char	proglib[9];						/* PL Program lib			*/
		char	progvol[7];						/* PV Program volume			*/
		char	worklib[9];						/* default WORK lib			*/
		char	workvol[7];
		char	proc_stat;						/* Procedure queue submit status.	*/
		char	proc_class;						/* Procedure queue class.		*/
		char	proc_cpu[7];						/* Cpu limit.				*/
		uint4   flags;							/* Help screen flags.			*/
		char	kb_map[7];						/* NO LONGER IN USE.			*/
		int4	pf_version;						/* Personality file version.		*/
		char	psb_select;						/* Default pseudo blank char selection. */
		int4	psb_charset;						/* Default pseudo blank char. set.	*/
		int4	psb_rendition;						/* Default rendition for pseudo blanks.	*/
		int4	prt_lines;						/* Default lines per page (000-255)	*/
		int4	mp_cursor;						/* Cursor flag for menu pick items.	*/
		int4	automove;						/* Flag set for auto_move functionality.*/
		int4	autotab;						/* Flag set for auto_tab functionality.	*/
		int4	bgchange;						/* Flag background to be changed.	*/
		int4	bgcolor;						/* Background color flag.		*/
		int4	excolor;						/* Exit background color flag.		*/
		} usr_defaults;

typedef struct	{
			struct forms_id *next;					/* pointer to the next one		*/
			int	form_num;					/* the form number			*/
			char	*form_string;					/* the string to  insert into the lp cmd*/
		} forms_id;

typedef struct	{
			struct  prmap_id *next;					/* pointer to the next one		*/
			int	prmap_num;					/* the Printer number			*/
			char	prmap_string[80];				/* the string to  insert into the lp cmd*/
		} prmap_id;

typedef struct	{
			struct scmap_id *next;					/* pointer to the next one		*/
			int	nice;						/* The NICE value			*/
			char 	class;						/* the job class			*/
		} scmap_id;


typedef struct {
		int		fac_font;
		unsigned char	fac_dchar;
	       } dispfac_item;

/*
**	Globals and Externals
*/
static char language_path[80] = { "" };						/* the path of the IVS translation file         */
const char *WL_language_path() { return language_path; }

static int opt_max_pages = 0;							/* Max prb pages				*/
static int opt_max_parms = 0;							/* Max prb parms				*/

int WL_get_opt_max_pages() { return opt_max_pages; }
int WL_get_opt_max_parms() { return opt_max_parms; }

/*
**	Static data
*/

static usr_defaults defaults;							/* the actual defaults record		*/
static int defaults_loaded = 0;							/* Have defaults been loaded		*/

static forms_id 	*forms_list = NULL;
static prmap_id 	*prmap_list = NULL;
static scmap_id 	*scmap_list = NULL;
static prt_id		*prt_list = NULL;
static logical_id 	*logical_list = NULL;

static scmap_id 	*scmap_ptr;
static forms_id 	*forms_ptr;
static prmap_id 	*prmap_ptr;

static char curr_progvol[7] = "      ";						/* The current in memory PROGVOL value		*/
static char curr_proglib[9] = "        ";					/* The current in memory PROGLIB value		*/

static int  paths_built = 0;
static char person_path[WPATH_LEN];
static char parent_path[WPATH_LEN];
static char temp_person_path[WPATH_LEN];
static char system_person_path[WPATH_LEN];



static int dispfac_toggle = FALSE;						/* Init display FAC chars as FALSE.		*/
static dispfac_item dispfac_array[256];						/* Define array for displayable FACs.		*/
static int load_df_first = TRUE;						/* First time in flag to load table.		*/

static char *batchqueue_name = "";
const char* WL_batchqueue_name() { return batchqueue_name; }

/*
**	Function Prototypes
*/

static int read_from_file(usr_defaults *the_def, char *the_name);		/* Load the personality from the file.	*/
static int write_to_file(usr_defaults *the_def, char *the_name);
static int load_defaults_temp(usr_defaults *the_def);				/* Read defaults from temp area			*/
static int save_defaults_temp(usr_defaults *the_def);				/* Store the usage constants in the sym	*/
static int build_config_paths(void);
static void genworklib(char *worklib);
static int genworkvol(char *workvol);
static int genspoollib(char *spoollib);
static void loadpadnull(char *dest, const char *src, int size);
static void getprogvol(char *result);
static void getproglib(char *result);
static void setprogvol(char *progvol);
static void setproglib(char *proglib);

static void load_lpmap(void);

static void load_lgmap(void);

static void load_scmap(void);

static void load_forms(void);
static void load_prmap(void);
static void load_dispfac(void);

static void set_pfkeys12(void);

static void add_option(const char *keyword, const char *trailing);

#ifdef WIN32
typedef struct	{
			struct cqmap_id *next;					/* pointer to the next one		*/
			char 	*queue;						/* The queue 				*/
			char 	class;						/* the job class			*/
		} cqmap_id;

static cqmap_id 	*cqmap_ptr;
static cqmap_id 	*cqmap_list = NULL;
static void load_cqmap(void);
#endif

int WL_wang_alphanumeric(const char* str);

/*==============================================================================================================================*/
/*
**	WL_wpload	This routine used to be responsible for loading all the device info lists
**		as well as the defaults structure and any other config files.
**		This is no longer required because each of these things will be loaded
**		dynamically if and only if they are used.
**		Currently only WL_load_options() is done.
*/
void WL_wpload(void)
{
	static int loaded = 0;							/* flag to indicate info is loaded		*/

	if (loaded) return;							/* already done					*/

	WL_load_options();							/* Load the runtime options file.	*/

	loaded = 1;								/* already loaded now			*/
}

/*
	Use the following routine to manipulate the defaults struct.

	WL_save_defaults()		Call after a WL_set_defs() to move defaults to temp-area.
	WL_load_defaults()		Call to load defaults from temp-area
	WL_write_defaults_to_file()	Call to write temp-area to a file
	WL_read_defaults_from_file()	Call to load temp-area from a file
*/
int WL_save_defaults(void)
{
	if (defaults_loaded)
	{
		save_defaults_temp(&defaults);					/* Save defaults in temp area			*/
	}
	return(0);
}

int WL_load_defaults(void)
{
	if ( !load_defaults_temp(&defaults) )					/* First try the temp defaults			*/
	{
		WL_read_defaults_from_file("");					/* Otherwise, read the file.			*/
	}
	defaults_loaded = 1;
	return(0);
}

int WL_write_defaults_to_file(char *file)
{
	if (!defaults_loaded)
	{
		WL_load_defaults();						/* First ensure defaults are loaded		*/
	}
	return write_to_file(&defaults,file);					/* write defaults to the file			*/
}

int WL_read_defaults_from_file(char *file)
{
	return read_from_file(&defaults,file);
}

static int raw_read_personality_file(usr_defaults *the_def, const char *filename)
{
	FILE *fp;
	size_t cnt;

	fp = fopen(filename, FOPEN_READ_BINARY);

	if (NULL == fp)
	{
		WL_wtrace("WPERSON", "READ", "Open PERSONALITY file [%s] FAILED", filename);
		return -1;	/* OPEN failed */
	}

	cnt = fread(the_def,sizeof(usr_defaults),1,fp);
	fclose(fp);

	if (cnt < 1)
	{
		WL_wtrace("WPERSON", "READ", "Read failed on PERSONALITY file [%s]", filename);
		return -2;	/* READ failed */
	}

	if ( the_def->pf_version != PF_VERSION )
	{
		return -3;
	}

	WL_wtrace("WPERSON", "READ", "PERSONALITY read from file [%s]", filename);

	return 0;
}


static int raw_write_personality_file(usr_defaults *the_def, const char *filename)
{
	FILE *fp;
	size_t cnt;

	fp = fopen(filename, FOPEN_WRITE_BINARY);

	if (NULL == fp)
	{
		werrlog(WERRCODE(83020),"raw_write_personality_file",filename,errno,0,0,0,0,0);
		return -1;	/* OPEN failed */
	}

	cnt = fwrite(the_def,sizeof(usr_defaults),1,fp);
	fclose(fp);

	if (cnt < 1)
	{
		werrlog(WERRCODE(83022),"raw_write_personality_file",filename,errno,0,0,0,0,0);
		return -2;	/* WRITE failed */
	}
	chmod( filename, 0666 );

	WL_wtrace("WPERSON", "WRITE", "Wrote PERSONALITY file [%s]", filename);
	return 0;
}

static void initialize_defaults(usr_defaults *the_def)
{
	memset(the_def,0,sizeof(usr_defaults));

	the_def->prt_mode = 'S';					/* print mode is spooled		*/
	the_def->prt_class = 'A';					/* class is A				*/
	the_def->prt_num = 0;						/* printer number 0			*/
	the_def->prt_form = 0;						/* printer form number is 0		*/

	strcpy(the_def->inlib,"        ");				/* blank out library names		*/
	strcpy(the_def->outlib,"        ");
	strcpy(the_def->runlib,"        ");
	strcpy(the_def->spoolib,"        ");
	strcpy(the_def->proglib,"        ");
	strcpy(the_def->worklib,"        ");

	strcpy(the_def->invol,"      ");				/* blank out volume names		*/
	strcpy(the_def->outvol,"      ");
	strcpy(the_def->runvol,"      ");
	strcpy(the_def->spoolvol,"      ");
	strcpy(the_def->progvol,"      ");
	strcpy(the_def->workvol,"      ");

	the_def->proc_stat = 'R';					/* Set procedure queue constants.	*/
	the_def->proc_class = 'A';
	strcpy(the_def->proc_cpu,"000000");
	the_def->flags = 0x0FFFFFFFF;					/* All things are enabled.		*/

	the_def->psb_select = 'B';					/* Use default characteristics for 	*/
	the_def->psb_charset = 0;					/*  pseudo blank.			*/
	the_def->psb_rendition = 2;

	the_def->prt_lines = 55;					/* printer default lines per page is 55	*/
	the_def->mp_cursor = TRUE;					/* Display cursor on menu pick flag	*/
	the_def->automove = FALSE;					/* Flag for auto_move default is FALSE.	*/
	the_def->autotab = TRUE;					/* Flag for auto_tab default is TRUE.	*/

#ifdef WIN32
	the_def->bgchange = TRUE;					/* Change background by default.	*/
	the_def->bgcolor = TRUE;					/* Default background color is Grey.	*/
#else
	the_def->bgchange = FALSE;					/* Don't change background by default.	*/
	the_def->bgcolor = FALSE;					/* Default background color is black.	*/
#endif
	the_def->excolor = FALSE;					/* Exit with a black background.	*/

	the_def->pf_version = PF_VERSION;				/* Set the file version.		*/	
}

static int read_from_file(usr_defaults *the_def, char *the_name)		/* Load the personality from the file.	*/
{
	int retfl;								/* Set flag if use defaults.		*/
	int read_rc;

	defaults_loaded = 1;							/* This routine will always succeed	*/

	if ( ! paths_built ) build_config_paths();
	retfl = 0;								/* Initialize flag.			*/
	if (the_name && *the_name)
	{
		read_rc = raw_read_personality_file(the_def, the_name);
	}
	else 
	{
		read_rc = raw_read_personality_file(the_def, person_path);
	}

	if (read_rc != 0)							/* not found, go for the system file	*/
	{
		read_rc = raw_read_personality_file(the_def, system_person_path);
	}

	if (read_rc != 0)							/* no file found			*/
	{									/* set our defaults			*/
		retfl = 1;							/* Set flag to detect use of defaults.	*/

		initialize_defaults(the_def);
	}


	genworklib(the_def->worklib);						/* Generate the worklib			*/
	genworkvol(the_def->workvol);						/* Generate the workvol			*/

	strcpy(the_def->proglib,"        ");					/* Always reset PV/PL			*/
	strcpy(the_def->progvol,"      ");

	WL_save_defaults();							/* create the symbol.			*/

	return(retfl);
}
										/* save a user default personality	*/
static int write_to_file(usr_defaults *the_def, char *the_name)
										/* NOTE the structure MUST be properly	*/
										/* loaded or there will be a problem!	*/
{
	int	write_rc;

	if ( ! paths_built ) build_config_paths();

	if (*the_name) 								/* Use the supplied name.		*/
	{
		makepath(the_name);
		write_rc =  raw_write_personality_file(the_def, the_name);
	}
	else
	{
		write_rc =  raw_write_personality_file(the_def, person_path);	/* Use the default.			*/
	}

	return write_rc;
}

static int load_defaults_temp(usr_defaults *the_def)				/* Read defaults from temp area			*/
{
	int	read_rc;

	if ( ! paths_built ) build_config_paths();

	read_rc = raw_read_personality_file(the_def, temp_person_path);

	if ( read_rc != 0 && wbackground() )					/* If temp not found && background		*/
	{
		read_rc = raw_read_personality_file(the_def, parent_path);	/* then  copy from parent			*/
	}

	if (read_rc != 0) return(0);						/* Not loaded					*/

	genworklib(the_def->worklib);						/* Generate the worklib				*/
	genworkvol(the_def->workvol);						/* Generate the workvol				*/

	return(1);								/* Successfully loaded				*/
}

static int save_defaults_temp( usr_defaults *the_def )				/* Write the PERSONALITY## file from memory.	*/
{
	if ( ! paths_built ) build_config_paths();

	return  raw_write_personality_file(the_def, temp_person_path);
}

void WL_delete_defaults_temp(void)
{
	if ( ! paths_built ) build_config_paths();
	wisp_unlink(temp_person_path);
}

char *WL_wforms(int num)	/* return the string for form#		*/
{
	if (!forms_list)
	{
		load_forms();
	}
	forms_ptr = forms_list;
	while( forms_ptr )
	{
		if ( forms_ptr->form_num == num ) return( forms_ptr->form_string );
		forms_ptr = (forms_id *)forms_ptr->next;
	}

	return("");
}

char *WL_getprmap(int num)	/* return the string for printer #	*/
{
	if (!prmap_list)
	{
		load_prmap();
	}
	prmap_ptr = prmap_list;
	while( prmap_ptr )
	{
		if ( prmap_ptr->prmap_num == num ) return( prmap_ptr->prmap_string );
		prmap_ptr = (prmap_id *)prmap_ptr->next;
	}

	return("");
}

char *WL_wlpclass(char lpclass)	/* return the string for lpclass#	*/
{
	prt_id	*prt_ptr;

	prt_ptr = WL_get_lpmap_list();
	while( prt_ptr )
	{
		if ( prt_ptr->class == lpclass ) return( prt_ptr->prt_string );
		prt_ptr = (prt_id *)prt_ptr->next;
	}

	return("");
}

int WL_ttyid5(char *tty)
{
	char	*ptr;

	if ( wbackground() )
	{
		if ( (ptr = getenv( WISP_TTY_ENV )) )
		{
			strncpy( tty, ptr, 5 );
			tty[5] = '\0';
			return(0);
		}
	}

	{
		const char *path = ttyname(0);

		if ( path )
		{
#ifdef unix
			/*
			**	Take the last 5 chars removing '/'s from the ttyname.
			*/
			int	i;
			tty[5] = '\0';
			path += strlen(path);
			tty  += 5;
			for(i=0; i <= 5; path--)
			{
				if (*path != '/')
				{
					*tty-- = *path;
					i++;
				}
			}
#else
			strncpy(tty, path, 5);
			tty[5] = '\0';
#endif
		}
		else
		{
			strcpy( tty, "ERROR" );
		}
	}
	return 0;
}

/*
**	ROUTINE:	WL_build_wisp_config_path()
**
**	FUNCTION:	Build path to a file in the wispconfig directory.
**
**	DESCRIPTION:	Takes a base filename (like "TTMAP") and creates
**			a full path to it by adding on the wispconfig
**			path.
**
**	ARGUMENTS:
**	file		The base filename for the file in wispconfig
**	path		The returned full path to the file.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void WL_build_wisp_config_path(char *file, char *path)
{
	buildfilepath(path, wispconfigdir(), file);
}

#define		WISP_PRINTER_FILE		"LPMAP"
#define		WISP_OPTIONS_FILE		"OPTIONS"
#define		WISP_DISPFAC_FILE		"DISPFAC"
#define		WISP_SUBCLASS_FILE		"SCMAP"
#define		WISP_QUEUECLASS_FILE		"CQMAP"
#define		WISP_LOGICAL_FILE		"LGMAP"
#define		WISP_FORMS_FILE			"FORMS"
#define		WISP_PRMAP_FILE			"PRMAP"

static int build_config_paths(void)
{
/*
	$WTMP defaults to /usr/tmp or C:\\TEMP for WIN32

	person_path			$HOME/$WISP_USER_PERSON_FILE
	temp_person_path		wisptmp/DEFS_{userid}_{tty}   	(unix forground)
					wisptmp/DEFS_{userid}_{gid}   	(WIN32 foreground)
					wisptmp/DEFS_BG_{userid}_{gid}  (background)
	parent_path			wisptmp/DEFS_{userid}_{tty}	(unix)
	                                wisptmp/DEFS_{userid}_{gid}   	(WIN32)
	system_person_path		$WISPCONFIG/$WISP_SYSTEM_PERSON_FILE
*/

	char	tname[WPATH_LEN];

	if ( paths_built ) return(0);

	WL_wtrace("WPERSON","ENTRY", "Entry build_config_paths() using $WISPCONFIG=[%s]", wispconfigdir());

	buildfilepath( system_person_path, wispconfigdir(), WISP_SYSTEM_PERSON_FILE );

	buildfilepath( person_path, wisphomedir(NULL), WISP_USER_PERSON_FILE );

#ifdef unix
	{
		char	tty[10];
		WL_ttyid5(tty);

		sprintf(tname, "DEFS_%s_%s", WL_longuid(), tty );
	}
#endif
#ifdef WIN32
	{
		sprintf(tname, "DEFS_%s_%u", WL_longuid(), WL_wgetpgrp() );
	}
#endif

	buildfilepath( parent_path, wispdefdir(NULL), tname );

	if (wbackground())
	{
		sprintf(tname, "DEFS_BG_%s_%u", WL_longuid(), WL_wgetpgrp() );
		buildfilepath(temp_person_path, wispdefdir(NULL), tname );
	}
	else /* foreground */
	{
		strcpy( temp_person_path, parent_path );
	}
	makepath(temp_person_path);

	paths_built = 1;
	return(0);
}


static void genworklib(char *worklib)
{
	char temp[20];

	sprintf(temp,"%08X", WL_wgetpgrp());					/* Format the PID			*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
	memcpy(worklib, "WK", 2);						/* Overlay the first to chars as "WK"	*/
}

static int genworkvol(char *workvol)
{

	if ( memcmp(workvol,"      ",6) != 0 ) return(0);
	memcpy(workvol, "WRKVOL", 6 );
	return(1);
}

static int genspoollib(char *spoollib)
{
	if ( memcmp(spoollib,"        ",8) != 0 ) return(0);
	spoollib[0] = '#';
	memcpy(&spoollib[1],WL_wanguid3(),3);
	if ( spoollib[2] == ' ' ) memcpy( &spoollib[2], "PRT", 3);
	else if ( spoollib[3] == ' ' ) memcpy( &spoollib[3], "PRT", 3);
	else memcpy( &spoollib[4], "PRT", 3);
	return(1);
}

static int opt_linkvectoroff_flag = 0;
int WL_opt_linkvectoroff(void)
{
	return opt_linkvectoroff_flag;
}

/*
	WL_load_options:	Load the runtime OPTIONS file

				ERRFLAG {num}
				SIGNALSOFF
				SIGNALSON		(default)
				NULLISDOT
				NULLISSPACE		(default)
				OUTPUTVERIFYOFF
				OUTPUTVERIFYON		(default)
				CREATEVOLUMEON
				CREATEVOLUMEOFF
				IDSIPRINTON
				IDSIPRINTOFF
				BATCHQUEUE {name}
				BATCHMAN {name}
				BATCHCMD command
				BATCHCMD95 command
				BATCHLOGVOL volume
				BATCHLOGLIB library
				BATCHPASS password
				BATCHUSER user
				BATCHSERVER server
				BATCHHOLD string
				BATCHRUN string
				PQILP
				PQLP
				PQNP
				PQUNIQUE		(default)
				IDNUMERIC
				IDALPHA			(default)
				IDFIVE			(return char 5-7 of user id)
				IDONE			(default return char 1-3 of user id)
				HELPSTYLE1		(default Wang style)
				HELPSTYLE2		(Non-Wang style)
				ALLSTATUSKEYS
				MAXPRBPARMS {num}	(default 128)
				MAXPRBPAGES {num}	(default 40)
				WISPLANGUAGE xxx
				LINKVECTOROFF
				USESOFTLINK
				USEHARDLINK
				NATIVESCREENS
				PFKEYS12
				EXTDISPLAY		(External DISPLAY utility)


	NOTE:	When adding new options do NOT create new global variables.
		Instead use LINKVECTOROFF as an example.
*/
int WL_load_options(void)					/* Load the runtime OPTIONS file.		*/
{
#define MAX_INLIN	2048
	static	int	first=1;
	FILE 	*the_file;
	char	inlin[MAX_INLIN], keyword[80], value[80];
	char	continued_inlin[MAX_INLIN];
	char	*trailing;
	int	cnt,i;
	int	len;
	const char *cptr;
	int	bDone;
	char options_path[WPATH_LEN];

	if (!first) return(0);
	first=0;
	WL_wtrace("WPERSON","ENTRY", "Entry into WL_load_options()");

	WL_build_wisp_config_path(WISP_OPTIONS_FILE, options_path);

	WL_opt_printqueue = PQ_DEFAULT;
	WL_opt_printqueue_manager = NULL;
	WL_batchman_name = "";

	the_file = fopen(options_path,"r");
	if (the_file)
	{
		bDone = 0;
		
		while(!bDone)
		{
			int bContinued;
			
			/*
			 * Build up the input line in continued_inlin
			 */
			inlin[0] = '\0';
			continued_inlin[0] = '\0';
			do
			{
				bContinued = 0;
				if (NULL!=fgets(inlin,sizeof(inlin)-1,the_file))
				{
					len=strlen(inlin);
					if (len>0 && '\n'==inlin[len-1]) 	/* Strip off the newline char */
					{
						inlin[--len] = '\0';
					}

				        /*
					 *	Check for continued line
					 */
					if (len>0 && '\\'==inlin[len-1])
					{
						bContinued = 1;
						inlin[--len] = '\0';
					}

					/*
					 *	If we are in a continued line situation then build up
					 *	continued_inlin and update inlin.
					 */
					if (bContinued || '\0' != continued_inlin[0])
					{
						strcat(continued_inlin, inlin);
						strcpy(inlin, continued_inlin);
					}
				}
				else
				{
					bDone = 1;
				}
				
			} while(!bDone && bContinued);
			
			if ('\0' == inlin[0]) continue;
			
			cnt = sscanf(inlin,"%s %s",keyword,value);
			if ( cnt < 1 ) continue;

			if (keyword[0] == '#') continue;
			WL_upper_string(keyword);

			trailing = inlin + strlen(keyword);
			while(' ' == *trailing) { trailing++; }

			add_option(keyword, trailing);
			
			if (strcmp(keyword,"WISPDEBUG") == 0)
			{
				if ( cnt >= 2 )
				{
					if (strcmp(value,"FULL")==0)
					{
						WL_set_wispdebug(WISPDEBUG_FULL);
					}
					else if (strcmp(value,"NONE")==0)
					{
						WL_set_wispdebug(WISPDEBUG_NONE);
					}
					else
					{
						WL_set_wispdebug(WISPDEBUG_ERRORS); /* Default */
					}
					WL_werr_override();	/* Check for error logging override.	*/
				}

			}
			else if (strcmp(keyword,"ERRFLAG") == 0)	/* Set the ERRFLAG (same as initwisp).		*/
			{
				/*
				**	This is the obsolete ERRFLAG bit pattern crap.
				**	Still supported but translated into WISPDEBUG mode.
				*/
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(WERRCODE(83018),"Invalid Value",inlin,0,0,0,0,0,0);
					continue;
				}
				wisp_set_werrlog_flag(i);

				WL_werr_override();				/* Check for error logging override.	*/
			}
#ifdef unix
			else if (strcmp(keyword,"IDSIPRINTON") == 0)		/* UNIX use the IDSI print spooler		*/
			{
				WL_opt_printqueue = PQ_UNIQUE;
			}
			else if (strcmp(keyword,"IDSIPRINTOFF") == 0)		/* UNIX use lp, not the IDSI print spooler	*/
			{
				WL_opt_printqueue = PQ_LP;
			}
			else if (strcmp(keyword,"PQILP") == 0)			/* UNIX use ILP					*/
			{
				WL_opt_printqueue = PQ_ILP;
			}
			else if (strcmp(keyword,"PQNP") == 0)			/* UNIX use NP					*/
			{
				WL_opt_printqueue = PQ_NP;
			}
			else if (strcmp(keyword,"PQLP") == 0)			/* UNIX use LP					*/
			{
				WL_opt_printqueue = PQ_LP;
			}
#endif
			else if (strcmp(keyword,"PQUNIQUE") == 0)		/* UNIX or WIN32 use UNIQUE			*/
			{
				WL_opt_printqueue = PQ_UNIQUE;
			}
			else if (strcmp(keyword,"BATCHQUEUE") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				if (strcmp(value,"none") != 0)
				{
					batchqueue_name = wisp_strdup(value);
				}
			}
			else if (strcmp(keyword,"BATCHMAN") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				if (strcmp(value,"none")==0)
				{
					WL_opt_batchman=0;
				}	
				else
				{
					WL_opt_batchman=1;
					cptr = getenv("BATCH_MAN");
					if (cptr && *cptr)
					{
						WL_batchman_name = wisp_strdup(cptr);
						WL_wtrace("OPTIONS","BATCHMAN","Overridden by $BATCHMAN = [%s]",WL_batchman_name);
					}
					else
					{
						WL_batchman_name = wisp_strdup(value);
					}
				}
			}
			else if (strcmp(keyword,"BATCHCMD") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchcmd(trailing);
			}
			else if (strcmp(keyword,"BATCHCMD95") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchcmd95(trailing);
			}
			else if (strcmp(keyword,"BATCHLOGVOL") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchlogvol(value);
			}
			else if (strcmp(keyword,"BATCHLOGLIB") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchloglib(value);
			}
			else if (strcmp(keyword,"BATCHPASS") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchpass(value);
			}
			else if (strcmp(keyword,"BATCHUSER") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchuser(value);
			}
			else if (strcmp(keyword,"BATCHSERVER") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				WL_setbatchserver(value);
			}
			else if (strcmp(keyword,"BATCHHOLD") == 0)
			{
				WL_setbatchhold(trailing);
			}
			else if (strcmp(keyword,"BATCHRUN") == 0)
			{
				WL_setbatchrun(trailing);
			}
			else if (strcmp(keyword,"HELPSTYLE1") == 0)		/* Help is Wang style				*/
			{
				WL_opt_helpstyle = 1;
			}
			else if (strcmp(keyword,"HELPSTYLE2") == 0)		/* Help is non-Wang style			*/
			{
				WL_opt_helpstyle = 2;
			}
			else if (strcmp(keyword,"LINKVECTOROFF") == 0)		/* Turn off use of WL_linkvector()		*/
			{
				opt_linkvectoroff_flag = 1;
			}
			else if (strcmp(keyword,"MAXPRBPARMS") == 0)		/* Set the MAX PRB (putparm) number of parms	*/
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(WERRCODE(83018),"Invalid Value",inlin,0,0,0,0,0,0);
					continue;
				}
				opt_max_parms = i;

			}
			else if (strcmp(keyword,"MAXPRBPAGES") == 0)		/* Set the MAX PRB (putparm) number of parms	*/
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(WERRCODE(83018),"Invalid Value",inlin,0,0,0,0,0,0);
					continue;
				}
				opt_max_pages = i;

			}
			else if (strcmp(keyword,"WISPLANGUAGE") == 0)		/* Set language xlation file       */
			{
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83018),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				buildfilepath(language_path,wispconfigdir(),value);
				if ( !fexists(language_path) )
				{
					werrlog(WERRCODE(83018),"Invalid Language File",inlin,0,0,0,0,0,0);
					continue;
				}
			}
			else if (strcmp(keyword,"USESOFTLINK") == 0)
			{
				USESOFTLINK(NULL);
			}
			else if (strcmp(keyword,"USEHARDLINK") == 0)
			{
				USEHARDLINK(NULL);
			}
			else if (strcmp(keyword,"NATIVESCREENS") == 0)
			{
				/* 
				   We are using COBOL native screens.
				   Call nativescreens to force initialization.
				*/
				wisp_nativescreens();
			}
			else if (strcmp(keyword,"PFKEYS12") == 0)
			{
				/* We only have 12 pfkeys */
				set_pfkeys12();
			}
			else
			{
				/* Not an error: new add_option() mechanism! */
				/* werrlog(WERRCODE(83019),"Unknown keyword",inlin,0,0,0,0,0,0); */
			}
		}
		fclose(the_file);
	}

	/*
	**	Resolve conflicting options
	*/

	if (WL_get_wisp_option("PQCMD"))
	{
		/*
		**	PQCMD overrides all other PQ options.
		*/
		WL_opt_printqueue = PQ_GENERIC;
	}

#ifdef unix
	if (PQ_DEFAULT == WL_opt_printqueue)
	{
		WL_opt_printqueue = PQ_UNIQUE;
	}

	WL_opt_printqueue_manager = wisp_strdup(WL_get_wisp_option("PQMANAGER"));
	if (NULL == WL_opt_printqueue_manager)
	{
		switch(WL_opt_printqueue)
		{
		case PQ_UNIQUE:
			if (getenv("UNIQUE_MAN"))
			{
				WL_opt_printqueue_manager = wisp_strdup(getenv("UNIQUE_MAN"));
			}
			else
			{
				WL_opt_printqueue_manager = "unique -q -w";
			}
			break;
			
		case PQ_ILP:
			WL_opt_printqueue_manager = "ilpman -q -w";
			break;

		case PQ_LP:
		case PQ_NP:
		case PQ_GENERIC:
		case PQ_DEFAULT:
			break;
		}

		if (NULL != WL_opt_printqueue_manager)
		{
			WL_wtrace("OPTIONS","PQMANAGER","Print Queue Manager command string is [%s]", WL_opt_printqueue_manager);
		}
	}
#endif /* unix */
	
	return(0);
}

/*
	WL_get_defs	Get a default based on the code.
*/
int WL_get_defs(int code, void *void_ptr)
{
	char	*ptr;

	ptr = (char *)void_ptr;

	if (!defaults_loaded)
	{
		WL_load_defaults();
	}

	switch(code)
	{
	case DEFAULTS_PM:
		*ptr = defaults.prt_mode;
		break;
	case DEFAULTS_PC:
		*ptr = defaults.prt_class;
		break;
	case DEFAULTS_PR:
		memcpy(ptr,&defaults.prt_num,sizeof(defaults.prt_num));
		break;
	case DEFAULTS_FN:
		memcpy(ptr,&defaults.prt_form,sizeof(defaults.prt_form));
		break;
	case DEFAULTS_IL:
		memcpy(ptr,defaults.inlib,8);
		break;
	case DEFAULTS_IV:
		memcpy(ptr,defaults.invol,6);
		break;
	case DEFAULTS_OL:
		memcpy(ptr,defaults.outlib,8);
		break;
	case DEFAULTS_OV:
		memcpy(ptr,defaults.outvol,6);
		break;
	case DEFAULTS_RL:
		memcpy(ptr,defaults.runlib,8);
		break;
	case DEFAULTS_RV:
		memcpy(ptr,defaults.runvol,6);
		break;
	case DEFAULTS_SL:
		genspoollib(defaults.spoolib);	/* Generate the spoollib		*/
		memcpy(ptr,defaults.spoolib,8);
		break;
	case DEFAULTS_SV:
		memcpy(ptr,defaults.spoolvol,6);
		break;
	case DEFAULTS_PL:
		getproglib(ptr);
		break;
	case DEFAULTS_PV:
		getprogvol(ptr);
		break;
	case DEFAULTS_WL:
		memcpy(ptr,defaults.worklib,8);
		break;
	case DEFAULTS_WV:
		memcpy(ptr,defaults.workvol,6);
		break;
	case DEFAULTS_JS:
		*ptr = defaults.proc_stat;
		break;
	case DEFAULTS_JC:
		*ptr = defaults.proc_class;
		break;
	case DEFAULTS_JL:
		memcpy(ptr,defaults.proc_cpu,6);
		break;
	case DEFAULTS_FLAGS:
		memcpy(ptr,&defaults.flags,sizeof(defaults.flags));
		break;
	case DEFAULTS_PSB_CHAR:
		*ptr = defaults.psb_select;
		break;
	case DEFAULTS_PSB_SET:
		memcpy(ptr,&defaults.psb_charset,sizeof(defaults.psb_charset));
		break;
	case DEFAULTS_PSB_REN:
		memcpy(ptr,&defaults.psb_rendition,sizeof(defaults.psb_rendition));
		break;
	case DEFAULTS_LI:
		memcpy(ptr,&defaults.prt_lines,sizeof(defaults.prt_lines));
		break;
	case DEFAULTS_MP_CURSOR:
		memcpy(ptr,&defaults.mp_cursor,sizeof(defaults.mp_cursor));
		break;
	case DEFAULTS_AUTOMOVE:
		memcpy(ptr,&defaults.automove,sizeof(defaults.automove));
		break;
	case DEFAULTS_AUTOTAB:
		memcpy(ptr,&defaults.autotab,sizeof(defaults.autotab));
		break;
	case DEFAULTS_BGCHANGE:
		memcpy(ptr,&defaults.bgchange,sizeof(defaults.bgchange));
		break;
	case DEFAULTS_BGCOLOR:
		memcpy(ptr,&defaults.bgcolor,sizeof(defaults.bgcolor));
		break;
	case DEFAULTS_EXCOLOR:	
		memcpy(ptr,&defaults.excolor,sizeof(defaults.excolor));
		break;
	}
	return(0);
}

/*
	WL_set_defs	Set a default based on the code.

	* * * * WARNING * * * *  
		Set_defs only loads the value into the internal structure "defaults" -- after you finish a series of
		calls to "WL_set_defs" you need to perform a "WL_save_defaults()" to save these values into a temp area.

*/
int WL_set_defs(int code, const void *void_ptr)
{
	const char *ptr;

	ptr = (const char *)void_ptr;

	if (!defaults_loaded)
	{
		WL_load_defaults();
	}

	switch(code)
	{
	case DEFAULTS_PM:
		defaults.prt_mode = *ptr;
		break;
	case DEFAULTS_PC:
		defaults.prt_class = *ptr;
		break;
	case DEFAULTS_PR:
		memcpy(&defaults.prt_num,ptr,sizeof(defaults.prt_num));
		break;
	case DEFAULTS_FN:
		memcpy(&defaults.prt_form,ptr,sizeof(defaults.prt_form));
		break;
	case DEFAULTS_IL:
		loadpadnull(defaults.inlib,ptr,8);
		break;
	case DEFAULTS_IV:
		loadpadnull(defaults.invol,ptr,6);
		break;
	case DEFAULTS_OL:
		loadpadnull(defaults.outlib,ptr,8);
		break;
	case DEFAULTS_OV:
		loadpadnull(defaults.outvol,ptr,6);
		break;
	case DEFAULTS_RL:
		loadpadnull(defaults.runlib,ptr,8);
		break;
	case DEFAULTS_RV:
		loadpadnull(defaults.runvol,ptr,6);
		break;
	case DEFAULTS_SL:
		loadpadnull(defaults.spoolib,ptr,8);
		break;
	case DEFAULTS_SV:
		loadpadnull(defaults.spoolvol,ptr,6);
		break;
	case DEFAULTS_PL:
		loadpadnull(defaults.proglib,ptr,8);
		strcpy(curr_proglib,defaults.proglib);
		break;
	case DEFAULTS_PV:

		loadpadnull(defaults.progvol,ptr,6);
		strcpy(curr_progvol,defaults.progvol);
		break;
	case DEFAULTS_WL:
		loadpadnull(defaults.worklib,ptr,8);
		break;
	case DEFAULTS_WV:
		loadpadnull(defaults.workvol,ptr,6);
		break;
	case DEFAULTS_JS:
		defaults.proc_stat = *ptr;
		break;
	case DEFAULTS_JC:
		defaults.proc_class = *ptr;
		break;
	case DEFAULTS_JL:
		memcpy(defaults.proc_cpu,ptr,6);
		defaults.proc_cpu[6] = '\0';
		break;
	case DEFAULTS_FLAGS:
		memcpy(&defaults.flags,ptr,sizeof(defaults.flags));
		break;
	case DEFAULTS_PSB_CHAR:
		defaults.psb_select = *ptr;
		break;
	case DEFAULTS_PSB_SET:
		memcpy(&defaults.psb_charset,ptr,sizeof(defaults.psb_charset));
		break;
	case DEFAULTS_PSB_REN:
		memcpy(&defaults.psb_rendition,ptr,sizeof(defaults.psb_rendition));
		break;
	case DEFAULTS_LI:
		memcpy(&defaults.prt_lines,ptr,sizeof(defaults.prt_lines));
		break;
	case DEFAULTS_MP_CURSOR:
		memcpy(&defaults.mp_cursor,ptr,sizeof(defaults.mp_cursor));
		break;
	case DEFAULTS_AUTOMOVE:
		memcpy(&defaults.automove,ptr,sizeof(defaults.automove));
		break;
	case DEFAULTS_AUTOTAB:
		memcpy(&defaults.autotab,ptr,sizeof(defaults.autotab));
		break;
	case DEFAULTS_BGCHANGE:
		memcpy(&defaults.bgchange,ptr,sizeof(defaults.bgchange));
		break;
	case DEFAULTS_BGCOLOR:
		memcpy(&defaults.bgcolor,ptr,sizeof(defaults.bgcolor));
		break;
	case DEFAULTS_EXCOLOR:	
		memcpy(&defaults.excolor,ptr,sizeof(defaults.excolor));
		break;
	}
	return(0);
}

/*
	loadpadnull	Load dest with src padding out to size with blanks then null terminate
*/
/*
**	ROUTINE:	loadpadnull()
**
**	FUNCTION:	Load dest with src padding out to size with blanks then null terminate
**
**	DESCRIPTION:	Convert a c str to a COBOL PIC X(size), then place '\0' at the last X.
**
**	ARGUMENTS:	(O)	dest
**			(I)	src
**			(I)	size
**
**	GLOBALS:	None
**
**	RETURN:		void
**
**	WARNINGS:	none
**
*/
static void loadpadnull(char *dest, const char *src, int size)
{
	cstr2cobx(dest,src,size);							/* This routine does ASSERT itself	*/
	dest[size] = '\0';
}

/*===============================================================================================================================*/
/*
**	The following routines deal with maintaining PROGLIB and PROGVOL.
*/
/*===============================================================================================================================*/
/*
	DESCRIPTION OF PROGLIB PROGVOL ON THE WANG:

		On the Wang PROGLIB and PROGVOL represent the location where the currently running program or procedure resides.
		These defaults are used by the VSSUBS LINK and SUBMIT, and by procedure verbs RUN and SUBMIT.  They are the
		default location to look for programs to execute.  This values automatically change when a new program is
		run and change back when the LINK completes.  It is however possible to SET these values manually.
		They are normally used as follows: the first program of an application is run by explicitly giving the
		lib and vol, then all other programs are run by using PROGLIB and PROGVOL as defaults.  If a program wants to
		run another program that is in a different library this can be done in two ways: 1) Give the lib and vol
		of the new program in the LINK or RUN commands.  2) Set PROGLIB and PROGVOL to the new location then do
		the LINK or RUN without specifing the lib or vol and allowing it to default to PROGLIB and PROGVOL.

		NOTE:	RUNLIB and RUNVOL are only used when starting a program from the COMMAND PROCESSOR.
			CURRLIB and CURRVOL give the location of the currently running program, so they are useless
			when running from Procedure because they will point to the system library where the procedure
			interrupter resides.

	IMPLEMENTATION:

		If PROGLIB or PROGVOL are not set the RUNLIB and RUNVOL are returned.
		On a LINK or SUBMIT the values are passed to the new process by setting shell variables after the fork.
		The value of PROGLIB and PROGVOL is determined as follows:
			1)  In memory save values.   (curr_proglib/curr_progvol)
			2)  The SYMBOL i.e. temp defaults
			3)  Shell variables
			4)  RUNLIB/RUNVOL values
		The SYMBOL will only be set by a call to SET (also update in memory save values).

		The big problem is making this all work in a shell script that is linked to from a cobol program that
		does a WUSAGE SET to change PROGLIB and PROGVOL.  It has to use the SYMBOL to store this value as it
		can not set shell variables that persist.

		For SUBMIT we must use shell variables because the SYMBOL may change before it is read.
		For shell scripts must use SYMBOL because wusage can't set shell variable.
		On a LINK or SUBMIT must clear the SYMBOL so the shell variables will be used.
		After reading from SYMBOL or shell variable must set save values.
		Save the values in SYMBOL on entry and reset them on exit of a cobol program. (initwisp2 & wispexit) This is
		in case a linked to program sets these values, you don't want them still around after you return from the link.
		Initwisp2() must do a WL_get_defs() for PROGVOL and PROGLIB to force an update of curr.

	SCENARIO:
		A-Program  LINKs to  B-Script
		B-Script   RUNs      C-Program
		C-Program  LINKs to  D-Program

		[A] -link-> [B] -run-> [C] -link-> [D]

		B:
		>wusage set proglib=abc
		>wrun C

	NOTES:
				
*/

/*
	getprogvol	
	getproglib	These routines return the current value of PROGLIB and PROGVOL. 
			It checks curr_progvol/lib first and if not set it checks symbol then shell variable.  If found in
			symbol or shell variable it will update curr_proglib/vol.
			If there is still not current values it will return RUNLIB RUNVOL.
			These routines are only called from WL_get_defs().
*/

static void getprogvol(char *result)
{
	char	*ptr;

	ptr = NULL;
	if (curr_progvol[0] == ' ')						/* If in memory save is not set check symbol	*/
	{
		if (defaults.progvol[0] != ' ')					/* If symbol is set then use it			*/
		{
			memcpy(curr_progvol,defaults.progvol,6);		/* Copy symbol in in memory save area		*/
		}
		else if ((ptr = getenv(WISP_PROGVOL_ENV)))			/* Check the shell variable			*/
		{
			if (*ptr != ' ')					/* if shell var exists and not null then use it	*/
			{
				int	len;
				len = strlen(ptr);
				len = (len < 6) ? len : 6;
				memset(curr_progvol,' ',6);
				memcpy(curr_progvol,ptr,len);			/* Copy symbol in in memory save area		*/
			}
		}
	}
	curr_progvol[6] = '\0';

	if (curr_progvol[0] != ' ')						/* If in memory save area is set then use it	*/
	{
		memcpy(result,curr_progvol,6);
	}
	else									/* Else return run values			*/
	{
		memcpy(result,defaults.runvol,6);
	}
}

static void getproglib(char *result)
{
	char	*ptr;

	ptr = NULL;

	if (curr_proglib[0] == ' ')						/* If in memory save is not set check symbol	*/
	{
		if (defaults.proglib[0] != ' ')					/* If symbol is set then use it			*/
		{
			memcpy(curr_proglib,defaults.proglib,8);		/* Copy symbol in in memory save area		*/
		}
		else if ((ptr = getenv(WISP_PROGLIB_ENV)))			/* Check the shell variable			*/
		{
			if (*ptr != ' ')					/* if shell var exists and not null then use it	*/
			{
				int	len;
				len = strlen(ptr);
				len = (len < 8) ? len : 8;
				memset(curr_proglib,' ',8);
				memcpy(curr_proglib,ptr,len);			/* Copy symbol in in memory save area		*/
			}
		}
	}
	curr_proglib[8] = '\0';

	if (curr_proglib[0] != ' ')						/* If in memory save area is set then use it	*/
	{
		memcpy(result,curr_proglib,8);
	}
	else									/* Else return run values			*/
	{
		memcpy(result,defaults.runlib,8);
	}
}

/*
	WL_clear_progdefs This routine clears the PROGLIB and PROGVOL symbol both in defaults and in temp.
			  This is called before a LINK or SUBMIT.
*/
int WL_clear_progdefs(void)
{
	WL_set_defs(DEFAULTS_PV,"      ");
	WL_set_defs(DEFAULTS_PL,"        ");
	WL_save_defaults();							/* Write changes to temp area			*/
	return(0);
}

/*
	WL_set_progdefs_env	Set the shell variables PROGVOL and PROGLIB.
				This is only called during a LINK, LINKPROC, and SUBMIT.
*/
int WL_set_progdefs_env(char *progvol, char *proglib)
{
	setprogvol(progvol);
	setproglib(proglib);
	return(0);
}
static void setprogvol(char *progvol)
{
	char	buff[80];
	sprintf(buff,"%s=%s",WISP_PROGVOL_ENV,progvol);
	WL_setenvstr(buff);
}
static void setproglib(char *proglib)
{
	char	buff[80];
	sprintf(buff,"%s=%s",WISP_PROGLIB_ENV,proglib);
	WL_setenvstr(buff);
}

/*
	WL_save_progdefs
	WL_restore_progdefs	These routines save the symbol values or PROGLIB and PROGVOL when a COBOL program
				is started and restore them on exit.
*/
static char	def_progvol[7] = "      ";
static char	def_proglib[9] = "        ";

int WL_save_progdefs(void)
{
	char	buff[80];

	WL_get_defs(DEFAULTS_PV,buff);						/* Force an update of curr			*/
	WL_get_defs(DEFAULTS_PL,buff);

	memcpy(def_progvol,defaults.progvol,6);					/* Save the entry values			*/
	memcpy(def_proglib,defaults.proglib,8);

	return(0);
}

int WL_restore_progdefs(void)
{
	WL_set_defs(DEFAULTS_PV,def_progvol);					/* Restore the entry values			*/
	WL_set_defs(DEFAULTS_PL,def_proglib);
	WL_save_defaults();							/* Write changes to temp area			*/
	return(0);
}

/*
**
**	LPMAP		printer definitions
**
*/

prt_id *WL_get_lpmap_list(void)
{
	if (!prt_list)
	{
		load_lpmap();
	}
	return(prt_list);
}

static void load_lpmap(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];
	prt_id	*prt_ptr;							/* Pointer to printer list structure		*/
	char	lpmap_path[WPATH_LEN];

	WL_build_wisp_config_path(WISP_PRINTER_FILE, lpmap_path);
	prt_list = NULL;							/* initialize the list			*/

	the_file = fopen(lpmap_path,"r");					/* now load the printer definitions	*/

	if (the_file)								/* no error opening file		*/
	{
		WL_wtrace("LPMAP","ENTRY", "Loading LPMAP file [%s]",lpmap_path);

		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (inlin[0]=='\n')
			{
				continue;
			}
			if (!prt_list)						/* first time?				*/
			{
				prt_list = (prt_id *)wisp_malloc(sizeof(prt_id));/* get some memory			*/
				prt_list->next = NULL;				/* set next pointer to zero		*/
				prt_ptr = prt_list;				/* set up local pointer			*/
			}
			else
			{
				prt_ptr->next = (struct prt_id *)wisp_malloc(sizeof(prt_id));/* get some memory		*/
				prt_ptr = (prt_id *)prt_ptr->next;		/* set pointer				*/
				prt_ptr->next = NULL;				/* set next pointer to zero		*/
			}

			if ('\n' == inlin[strlen(inlin)-1])
			{
				inlin[strlen(inlin)-1] = '\0';			/* remove trailing NL			*/
			}

			if ( strlen(inlin) < 4 )				/* 4 = class + space + code + NL 	*/
			{
				werrlog(WERRCODE(83028),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83028),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83028));
			}
			if ( strlen(&inlin[2]) > sizeof(prt_ptr->prt_string)-1 )
			{
				werrlog(WERRCODE(83024),lpmap_path,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83024),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83024));
			}
			
			prt_ptr->class = inlin[0];				/* Load the class			*/
			strcpy( prt_ptr->prt_string, &inlin[2] );		/* Get lp control string		*/

			WL_wtrace("LPMAP","LOAD","[%c] [%s]",prt_ptr->class, prt_ptr->prt_string);

		}

		fclose(the_file);						/* close the term file			*/
	}
	else
	{
		WL_wtrace("LPMAP","ENTRY", "Unable to open LPMAP file [%s]",lpmap_path);

		/* Create a dummy entry */
		prt_list = (prt_id *)wisp_malloc(sizeof(prt_id));
		prt_list->next = NULL;
		prt_list->class = ' ';
		prt_list->prt_string[0] = '\0';
	}
}

/*
**
**	LGMAP
**
*/
logical_id *WL_get_lgmap_list(void)
{
	if (!logical_list)
	{
		load_lgmap();
	}
	return(logical_list);
}

/*
**	ROUTINE:	add_logical_volume()
**
**	FUNCTION:	Add one logical volume mapping
**
**	DESCRIPTION:	
**			 			
**
**	ARGUMENTS:	
**	volume		The VOLUME name
**	path		The translation path
**
**	GLOBALS:	
**	logical_list	Linked list of logical volumes.
**
**	RETURN:		
**	0		Success
**	1		Failure
**
*/
static int add_logical_volume(const char* volume, const char* path)
{
	logical_id *new_logical = NULL;
	size_t len_vol, len_path;
	char up_volume[SIZEOF_VOL+1];

	/*
	**	Validate sizes
	*/
	len_vol = strlen(volume);
	len_path = strlen(path);
	if (len_vol < 1 || len_vol > SIZEOF_VOL)
	{
		WL_werrlog_error(WERRCODE(83102),"LGMAP","VOLSIZE",
			"Volume [%s] has invalid size", volume);
		return 1;
	}

	if ( len_path < 1 || len_path > MAX_TRANSLATE )
	{
		WL_werrlog_error(WERRCODE(83104),"LGMAP","PATHSIZE",
			"Path [%s] has invalid size (0 or greater then %d)", path, MAX_TRANSLATE);
		return 1;
	}

	/*
	**	Ensure VOLUME is uppercase
	*/
	strcpy(up_volume, volume);
	WL_upper_string( up_volume );

	/*
	**	Check for duplicates
	*/
	if (logical_list != NULL)
	{
		logical_id *p = logical_list;
		for(p=logical_list; p != NULL; p=p->next)
		{
			if (0==strcmp(up_volume,p->logical))
			{
				WL_werrlog_error(WERRCODE(83108),"LGMAP","DUPLICATE",
					"Duplicate Volume [%s]=[%s] not added", up_volume, path);
				return 1;
			}
		}
	}

	/*
	**	Add new logical volume
	*/
	new_logical = (logical_id *)wisp_malloc(sizeof(logical_id));
	strcpy(new_logical->logical, up_volume);
	strcpy(new_logical->translate, path);

	/*
	**	Link the new entry onto the end of the list
	*/
	new_logical->next = NULL;
	if (NULL == logical_list)
	{
		/* First time */
		logical_list = new_logical;
	}
	else
	{
		logical_id *tmp_logical = logical_list;

		while(tmp_logical->next != NULL)
		{
			tmp_logical = tmp_logical->next;
		}
		tmp_logical->next = new_logical;
	}


	WL_wtrace("LGMAP", "ADD", "Add logical volume [%-6s] = [%s]",
				new_logical->logical,
				new_logical->translate);

	return 0;
}

/*
**	ROUTINE:	load_lgmap()
**
**	FUNCTION:	Loads the lgmap file
**
**	DESCRIPTION:	Loads the lgmap file into memory as a link list.
**			This link list contains the WANG VOL and its Native Target Logical Volume Translation.
**			Lines initiated with # are disregarded.
**			Lines will be scanned for two fields. The first field could be SIZEOF_VOL long, and the
**                      second field could be MAX_TRANSLATE long. Fields can NOT span lines.
**			The two fields should be separated by spaces, but single or multiple \t delimiters are
**			also accepted. Lines should end with \n. The second field may contain embedded blanks!
**			 			
**
**	ARGUMENTS:	void
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:	Lines that violated the above mentioned rules will be loaded into the memory link list 
**			as if their fields were "(ERR) " and "(ERROR)"
**
*/

static void load_lgmap(void)
{
	FILE 	*the_file;							/* a file pointer			*/
	char	inlin[256];
	char	*ptr;
	char	*pfirst_field;
	char	*psecond_field;
	int	len;
	int	lin_cnt = 0;							/* line counter				*/
	char	lgmap_path[WPATH_LEN];

	WL_build_wisp_config_path(WISP_LOGICAL_FILE, lgmap_path);
	logical_list = NULL;							/* initialize the list			*/
	the_file = fopen(lgmap_path,"r");					/* now load the logicals 		*/

	if (the_file)								/* no error opening file		*/
	{
		WL_wtrace("LGMAP","ENTRY", "Loading LGMAP file [%s]",lgmap_path);

		while (fgets(inlin,sizeof(inlin),the_file))
		{
			lin_cnt++;						
			
			/*
			 *	Trim trailing whitespace (plus other control characters)
			 */
			len = strlen(inlin);
			while(len>0 && 
			      (' '==inlin[len-1] || 
			       '\t'==inlin[len-1] ||
			       '\n'==inlin[len-1] ||
			       '\v'==inlin[len-1] ||
			       '\f'==inlin[len-1] ||
			       '\b'==inlin[len-1] ||
			       '\a'==inlin[len-1] ||
			       '\r'==inlin[len-1]))
			{
				len--;
				inlin[len] = '\0';
			}

			/* 
			 *	Empty line
			 */
			if (len < 1)
			{
				continue;
			}
			
			/*
			 *	Comment line
			 */
			if ('#' == inlin[0])
			{
				continue;
			}

			/*
			 *	Short line
			 */
			if (len < SIZEOF_VOL+1+1)
			{
				WL_werrlog_error(WERRCODE(83106),"LGMAP", "LOAD", 
					"Line %d is too short to contain a valid definition [%s]",
					lin_cnt, inlin);
				continue;
			}

			/*
			 *	Mis-formed definition
			 */
			if (' ' != inlin[SIZEOF_VOL])
			{
				WL_werrlog_error(WERRCODE(83106),"LGMAP", "LOAD", 
					"Line %d is mis-formed, position 7 must be a space [%s]",
					lin_cnt, inlin);
				continue;
			}

			if (' ' == inlin[0])
			{
				WL_werrlog_error(WERRCODE(83106),"LGMAP", "LOAD", 
					"Line %d is mis-formed, volume can not have leading spaces [%s]",
					lin_cnt, inlin);
				continue;
			}
			

			/*
			**	Parse the first field (VOL), and check for acceptable length
			*/
			pfirst_field = inlin;
			for(len=SIZEOF_VOL; len>=0 && ' '==inlin[len]; len--)
			{
				inlin[len] = '\0';
			}
			
			/*
			**	Parse the second field (native mapping), and check for acceptable length
			*/
			psecond_field = &inlin[SIZEOF_VOL+1];
			for (; *psecond_field == ' '; psecond_field++) {};	/* ignore leading spaces		*/

			/*
			**	Shift the VOLUME name to uppercase and validate
			**
			**	Valid characters are Wang ALPHANUMERIC == A-Z 0-9 # $ @
			*/
			WL_upper_string( pfirst_field );
			if (!WL_wang_alphanumeric(pfirst_field))
			{
				WL_werrlog_error(WERRCODE(83108),"LGMAP","CHARS",
					"Volume [%s] contains invalid characters", pfirst_field);
				continue;
			}

			/*
			**	Add the logical volume
			*/
			if ('$' == psecond_field[0] && 
			    strlen(psecond_field) > 1 &&
			   (ptr = getenv(&psecond_field[1])))
			{
				/*
				**	A leading '$' indicates an environment variable.
				**		VOL100 $WANG_VOL100
				**	This means use the contents of $WANG_VOL100 if set.
				*/
				add_logical_volume(pfirst_field, ptr);
			}
			else
			{
				add_logical_volume(pfirst_field, psecond_field);
			}

		}

		fclose(the_file);						/* close the logical file		*/
	}
	else
	{									/* error opening the file		*/
		werrlog(WERRCODE(83020),"load_lgmap",lgmap_path,errno,0,0,0,0,0);
	}

	/* ADD (CFG) == $WISPCONFIG */
	{
		const char* cptr = wispconfigdir();
		if (strlen(cptr) < MAX_TRANSLATE)
		{
			add_logical_volume("(CFG)", cptr);
		}
		else
		{
			WL_wtrace("LGMAP", "TOOLONG", "Pseudo volume [(CFG)] is too long to enable.");
		}
	}

	/* ADD (CWD) == getcwd()  */
	{
		char buff[MAX_TRANSLATE];

		if ( NULL != getcwd( buff, sizeof(buff)) )
		{
			add_logical_volume("(CWD)", buff);
		}
		else
		{
			WL_wtrace("LGMAP", "TOOLONG", "Pseudo volume [(CWD)] is too long to enable.");
		}
	}

	/* ADD (HOME) == $HOME  */
	{
		const char* cptr = wisphomedir(NULL);
		if (strlen(cptr) < MAX_TRANSLATE)
		{
			add_logical_volume("(HOME)", cptr);
		}
		else
		{
			WL_wtrace("LGMAP", "TOOLONG", "Pseudo volume [(HOME)] is too long to enable.");
		}
	}

	/* ADD (ROOT) == root of file system */
	{
#ifdef unix
		add_logical_volume("(ROOT)", DIR_SEPARATOR_STR);
#endif
#ifdef WIN32
		add_logical_volume("(ROOT)", "C:\\");
#endif
	}


	/* ADD '.' to list of volumes. 		*/
	add_logical_volume(".", ".");
}

#ifdef unix
/*
**
**	SCMAP		Submit Class
**
*/
int WL_getscmapnice(char jobclass, int *nice_value)
{
	scmap_id *scmap_ptr;

	if (!scmap_list)
	{
		load_scmap();
	}

	scmap_ptr = scmap_list;							/* Get a ptr to the submit class list		*/

	while (scmap_ptr)
	{
		if (scmap_ptr->class == jobclass)				/* look for the class that matches		*/
		{
			*nice_value = scmap_ptr->nice;				/* set the nice value				*/
			return(0);						/* Succeeded.					*/
		}
		scmap_ptr = (scmap_id *) scmap_ptr->next;			/* next one					*/
	}
	return(1);								/* Failed.					*/
}

static void load_scmap(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];
	char	scmap_path[WPATH_LEN];

	WL_wtrace("WPERSON","ENTRY", "Entry into load_scmap()");
	WL_build_wisp_config_path(WISP_SUBCLASS_FILE, scmap_path);
	scmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(scmap_path,"r");					/* now load the scmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!scmap_list)					/* first time?				*/
			{
				scmap_list = (scmap_id *)wisp_malloc(sizeof(scmap_id));	/* get some memory			*/
				scmap_list->next = NULL;			/* set next pointer to zero		*/
				scmap_ptr = scmap_list;				/* set up local pointer			*/
			}
			else
			{
				scmap_ptr->next = (struct scmap_id *)wisp_malloc(sizeof(scmap_id));	/* get some memory	*/
				scmap_ptr = (scmap_id *)scmap_ptr->next;	/* set pointer				*/
				scmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(WERRCODE(83024),WISP_SUBCLASS_FILE,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83024),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83024));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			scmap_ptr->class = toupper(inlin[0]);			/* get the class			*/
			scmap_ptr->nice = atoi(&inlin[2]);			/* convert nice to int			*/
		}

		fclose(the_file);						/* close the scmap file			*/
	}
}
#endif /* unix */
#ifdef WIN32
/*
**
**	CQMAP		Submit Class to Queue mapping for WIN32 and Argent Queue Manager
**
*/
int WL_getcqmap(char jobclass, char *queue_value)
{
	cqmap_id *cqmap_ptr;
	
	if (!cqmap_list)
	{
		load_cqmap();
	}

	cqmap_ptr = cqmap_list;							/* Get a ptr to the submit class list		*/

	while (cqmap_ptr)
	{
		if (cqmap_ptr->class == jobclass)				/* look for the class that matches		*/
		{
			strcpy(queue_value,cqmap_ptr->queue);
			return(0);						/* Succeeded.					*/
		}
		cqmap_ptr = (cqmap_id *) cqmap_ptr->next;			/* next one					*/
	}
	return(1);								/* Failed.					*/
}

static void load_cqmap(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];
	char	cqmap_path[WPATH_LEN];

	WL_wtrace("WPERSON","ENTRY", "Entry into load_cqmap()");
	WL_build_wisp_config_path(WISP_QUEUECLASS_FILE, cqmap_path);
	cqmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(cqmap_path,"r");					/* now load the cqmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!cqmap_list)					/* first time?				*/
			{
				cqmap_list = (cqmap_id *)wisp_malloc(sizeof(cqmap_id));
				cqmap_list->next = NULL;			/* set next pointer to zero		*/
				cqmap_ptr = cqmap_list;				/* set up local pointer			*/
			}
			else
			{
				cqmap_ptr->next = (struct cqmap_id *)wisp_malloc(sizeof(cqmap_id));
				cqmap_ptr = (cqmap_id *)cqmap_ptr->next;	/* set pointer				*/
				cqmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(WERRCODE(83024),WISP_QUEUECLASS_FILE,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83024),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83024));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			cqmap_ptr->class = toupper(inlin[0]);			/* get the class			*/
			cqmap_ptr->queue = wisp_strdup(&inlin[2]);		/* dupe the queue name 			*/
		}

		fclose(the_file);						/* close the cqmap file			*/
	}
}
#endif

/*
**
**	FORMS
**
*/
static void load_forms(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];
	char	forms_path[WPATH_LEN];

	WL_build_wisp_config_path(WISP_FORMS_FILE, forms_path);

	forms_list = NULL;							/* initialize the list				*/
	the_file = fopen(forms_path,"r");					/* now load the forms	 			*/

	if (the_file)								/* no error opening file			*/
	{
		WL_wtrace("FORMS","ENTRY", "Loading FORMS file [%s]",forms_path);
		
		while (fgets(inlin,sizeof(inlin)-1,the_file))
		{
			if (!forms_list)					/* first time?					*/
			{
				forms_list = (forms_id *)wisp_malloc(sizeof(forms_id));
				forms_list->next = NULL;			/* set next pointer to zero		*/
				forms_ptr = forms_list;				/* set up local pointer			*/
			}
			else
			{
				forms_ptr->next = (struct forms_id *)wisp_malloc(sizeof(forms_id));
				forms_ptr = (forms_id *)forms_ptr->next;	/* set pointer				*/
				forms_ptr->next = NULL;				/* set next pointer to zero		*/
			}

			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			inlin[3] = '\0';					/* null term after form#		*/
			forms_ptr->form_num = atoi(inlin);			/* convert formnum to int		*/
			forms_ptr->form_string = wisp_strdup( &inlin[4] );	/* Get form control string		*/

			WL_wtrace("FORMS","LOAD","[%d] [%s]",forms_ptr->form_num, forms_ptr->form_string);
		}

		fclose(the_file);						/* close the forms file			*/
	}
	else
	{
		WL_wtrace("FORMS","ENTRY", "Unable to open FORMS file [%s]",forms_path);

		/* Create a dummy forms_list entry */
		forms_list = (forms_id *)wisp_malloc(sizeof(forms_id));
		forms_list->next = NULL;
		forms_list->form_num = 0;
		forms_list->form_string = wisp_strdup("");
	}
	
}

/*
**
**	PRMAP 	Printer number map
**
*/
static void load_prmap(void)
{
	FILE 	*the_file;							/* a file pointer			*/
	char	inlin[256];
	char	prmap_path[WPATH_LEN];

	WL_build_wisp_config_path(WISP_PRMAP_FILE, prmap_path);
	prmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(prmap_path,"r");					/* now load the prmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		WL_wtrace("PRMAP","ENTRY", "Loading PRMAP file [%s]",prmap_path);

		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (inlin[0]=='\n')
			{
				continue;
			}

			if (!prmap_list)					/* first time?				*/
			{
				prmap_list = (prmap_id *)wisp_malloc(sizeof(prmap_id));
				prmap_list->next = NULL;			/* set next pointer to zero		*/
				prmap_ptr = prmap_list;				/* set up local pointer			*/
			}
			else
			{
				prmap_ptr->next = (struct prmap_id *)wisp_malloc(sizeof(prmap_id));
				prmap_ptr = (prmap_id *)prmap_ptr->next;	/* set pointer				*/
				prmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(WERRCODE(83024),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83024),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83024));
			}
			if ( (int)strlen(inlin) < 6 )				/* 6 = 3 digit prt + space + code + NL	*/
			{
				werrlog(WERRCODE(83028),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83028),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83028));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			inlin[3] = '\0';					/* null term after printer #		*/
			prmap_ptr->prmap_num = atoi(inlin);			/* convert printer # to int		*/
			if (prmap_ptr->prmap_num == 0)
			{
				werrlog(WERRCODE(83030),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				WL_werrlog_error(WERRCODE(83030),"WPERSON", "TEXT", "%s", inlin);	/* Display the text.	*/
				wexit(WERRCODE(83030));
			}
			strcpy( prmap_ptr->prmap_string, &inlin[4] );		/* Get printer control string		*/
			
			WL_wtrace("PRMAP","LOAD","[%d] [%s]",prmap_ptr->prmap_num, prmap_ptr->prmap_string);
		}

		fclose(the_file);						/* close the prmap file			*/
	}
	else
	{
		WL_wtrace("PRMAP","ENTRY", "Unable to open PRMAP file [%s]",prmap_path);

		/* Create a dummy forms_list entry */
		prmap_list = (prmap_id *)wisp_malloc(sizeof(prmap_id));
		prmap_list->next = NULL;
		prmap_list->prmap_num = 0;
		prmap_list->prmap_string[0] ='\0';
	}
	
}

/*
**	Routine:	load_dispfac()
**
**	Function:	Load the translation table to display FACS on screens.
**
**	Description:	This routine creates an internal table which contains the
**			hex value of the FAC, a character set mode flag, and a displayable
**			character value.  The table is indexed through the decimal value of
**			the original hex value of the FAC, thus the first 127 (hex 7F) indices
**			are not used.  The maximum index value is 255.
**
**			All display values are initialized to 32 (space), then the data file
**			is read and sets the configured value. 
**
**	Arguments:	None
**
**	Globals:	dispfac_array[].fac_font
**			dispfac_array[].fac_dchar
**
**	Return:		Success or Failure
**
**	Warnings:	None
**
**	History:	
**	12/14/94	Written by SMC
**
*/
static void load_dispfac(void)
{
static	int	first=1;
	FILE 	*the_file;
	int	cnt, i, ndx, len;
	char	inlin[132], *ptr;
	int	t_dchar;
	char dispfac_path[WPATH_LEN];

	if (!first) return;
	first=0;
	WL_wtrace("WPERSON","ENTRY", "Entry into load_dispfac()");

	WL_build_wisp_config_path(WISP_DISPFAC_FILE, dispfac_path);

	for (i = 0; i < 256; i++)							/* Initialize the FAC display array.	*/
	{
		dispfac_array[i].fac_font = 0; /* DEFAULT */
		dispfac_array[i].fac_dchar = ' ';
	}
	the_file = fopen(dispfac_path,"r");
	if (the_file)
	{		
		while(fgets(inlin,sizeof(inlin),the_file))
		{
			if (*inlin != '#')						/* Check if a comment.			*/
			{
				len=strlen(inlin);
				if (len>0 && inlin[len-1] == '\n') inlin[len-1] = '\0';	/* Null out the newline char		*/
				cnt = sscanf(inlin,"%x",&ndx);

				if ( cnt < 1 ) continue;
				if (ndx < 128 || ndx > 190)				/* Encountered invalid FAC.		*/
				{
					werrlog(WERRCODE(83026),WISP_DISPFAC_FILE,ndx,0,0,0,0,0,0);
					continue;
				}
				ptr = inlin + 3;
				cnt = sscanf(ptr,"%d %x",
					&dispfac_array[ndx].fac_font, &t_dchar);
				if ( cnt < 2 )
				{
					werrlog(WERRCODE(83026),WISP_DISPFAC_FILE,ndx,0,0,0,0,0,0);
					continue;
				}
				dispfac_array[ndx].fac_dchar = t_dchar;
			}
		}
		fclose(the_file);
	}
	else
	{										/* error opening the file		*/
		werrlog(WERRCODE(83020),"load_dispfac",dispfac_path,errno,0,0,0,0,0);
	}
}

/*
**	Routine:	WL_get_dispfac_char()
**
**	Function:	Get the defined displayable character and font from user defined file.
**
**	Description:	Pass in the video mapped character and determine the user defined font
**			and character to display on the screen.  Pass this information back to
**			the caller. 
**
**	Arguments:
**			c_hex	hex value of FAC character
**			facchar	returned character to display
**			font	returned font to display character in
**
**	Globals:
**			load_df_first			first time ine flag
**			dispfac_array[].fac_font	font to display in array
**			dispfac_array[].fac_dchar	char to display array
**
**	Return:		Success or Failure
**
**	Warnings:	None
**
**	History:	
**	12/14/94	Written by SMC
**
*/
int WL_get_dispfac_char(unsigned char c_hex, unsigned char *facchar, int *font)		/* Get the char to display for FAC.	*/
{
	char	*tptr;

	if (load_df_first)								/* First time in flag to load table.	*/
	{
		tptr = getenv("DISPFAC_TOGGLE");
		if (tptr)								/* If environment var exists...		*/
		{
			if (strcmp(tptr,"YES") == 0)					/* If environment var = YES		*/
			{
				dispfac_toggle = 1;					/* Set flag so will toggle on display.	*/
				load_dispfac();
			}
			else	dispfac_toggle = 0;					/* Don't toggle or display lower or 	*/
		}									/*  upper.				*/
		else	dispfac_toggle = 0;						/* Don't toggle or display lower or 	*/

		load_df_first = FALSE;							/* Set so won't re-load.		*/
	}

	if (dispfac_toggle)
	{
		c_hex = c_hex & ~FAC_ALTERED;						/* Clear any altered bits that are set. */

		*facchar = dispfac_array[c_hex].fac_dchar;				/* Set character to display.		*/
		*font = dispfac_array[c_hex].fac_font;					/* Set font to display in.		*/
	}
	else
	{
		*facchar = ' ';								/* else use the defaults.		*/
		*font = 0; /* DEFAULT */
	}
	return(0);
}


const char *wisp_defaults_path(char *path)
{
	if ( !paths_built ) build_config_paths();

	if (path)
	{
		strcpy(path, person_path);
		return path;
	}
	else
	{
		return person_path;
	}
}

const char *wisp_temp_defaults_path(char *path)
{
	if ( !paths_built ) build_config_paths();

	if (path)
	{
		strcpy(path, temp_person_path);
		return path;
	}
	else
	{
		return temp_person_path;
	}
}

/*
**	ROUTINE:	USESOFTLINK()
**			USEHARDLINK()
**
**	FUNCTION:	Set flag for soft/hard LINK()
**
**	DESCRIPTION:	When soft links are enabled (and available) a LINK() from
**			one COBOL program to another COBOL program will be implemented
**			as an internal call within the same RTS and will not spawn a 
**			new RTS.
**
**			Softlinks are much faster and much more efficient then a hard LINK()
**			which spawns a new RTS.  However, because the program stays within the 

**			same RTS it is possible to get a recursive call which will fail.
**
**			A call to USESOFTLINK()/USEHARDLINK() will set the flag only within 
**			that current instance of the RTS.  A new instance of the RTS will 
**			have the flag set to it's default value.
**
**	ARGUMENTS:	
**	laststate	The prior state of the softlink flag.
**			'S'  Softlink was active
**			'H'  Hardlink was active
**
**	GLOBALS:        
**	do_soft_link	The flag that controls if soft links are enabled.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static int do_soft_link = 0;
void USESOFTLINK(char *laststate)
{
	if (laststate)
	{
		laststate[0] = (do_soft_link) ? 'S' : 'H';
	}

	do_soft_link = 1;
}

void USEHARDLINK(char *laststate)
{
	if (laststate)
	{
		laststate[0] = (do_soft_link) ? 'S' : 'H';
	}

	do_soft_link = 0;
}

int WL_softlink(void)
{
	return do_soft_link;
}

/*
**	Macro		Set			Get
**	-----------	------------		----------
**	%BATCHCMD%	WL_setbatchcmd()	WL_batchcmd()
**			WL_setbatchcmd95()	WL_batchcmd95()
**	%BATCHLOGVOL%	WL_setbatchlogvol()	WL_batchlogvol()
**	%BATCHLOGLIB%	WL_setbatchloglib()	WL_batchloglib()
**	%BATCHPASS%	WL_setbatchpass()	WL_batchpass()
**	%BATCHUSER%	WL_setbatchuser()	WL_batchuser()
**	%BATCHSERVER%	WL_setbatchserver()    	WL_batchserver()
**	%SUBSTAT%	WL_setbatchhold()	WL_batchhold()
**	%SUBSTAT%	WL_setbatchrun()	WL_batchrun()
*/
static char* the_batchcmd   = NULL;
static char* the_batchcmd95 = NULL;

void WL_setbatchcmd(const char* command)
{
	if (the_batchcmd) 
	{
		free(the_batchcmd);
	}
	
	the_batchcmd = wisp_strdup(command);
}
const char* WL_batchcmd(void)
{
	return the_batchcmd;
}

void WL_setbatchcmd95(const char* command)
{
	if (the_batchcmd95) 
	{
		free(the_batchcmd95);
	}
	
	the_batchcmd95 = wisp_strdup(command);
}
const char* WL_batchcmd95(void)
{
	return the_batchcmd95;
}

static char* the_batchlogvol = NULL;
static char* the_batchloglib = NULL;

void WL_setbatchlogvol(const char* value)
{
	if (the_batchlogvol) 
	{
		free(the_batchlogvol);
	}
	
	the_batchlogvol = wisp_strdup(value);
}
const char* WL_batchlogvol(void)
{
	if (!the_batchlogvol)
	{
		char	s_vol[6+1];
		
		/* If not set then default to SPOOLVOL */
		WL_get_defs(DEFAULTS_SV,s_vol);
		s_vol[6] = '\0';
		WL_setbatchlogvol(s_vol);
	}

	return the_batchlogvol;
}

void WL_setbatchloglib(const char* value)
{
	if (the_batchloglib) 
	{
		free(the_batchloglib);
	}
	
	the_batchloglib = wisp_strdup(value);
}
const char* WL_batchloglib(void)
{
	if (!the_batchloglib)
	{
		/* If not set then default to SUBMIT */
		WL_setbatchloglib("SUBMIT  ");
	}

	return the_batchloglib;
}

static char* the_batchpass = NULL;
static char* the_batchuser = NULL;

void WL_setbatchpass(const char* value)
{
	if (the_batchpass) 
	{
		free(the_batchpass);
	}
	
	the_batchpass = wisp_strdup(value);
}
const char* WL_batchpass(void)
{
	return the_batchpass;
}

void WL_setbatchuser(const char* value)
{
	if (the_batchuser) 
	{
		free(the_batchuser);
	}
	
	the_batchuser = wisp_strdup(value);
}
const char* WL_batchuser(void)
{
	if (!the_batchuser)
	{
		WL_setbatchuser(WL_longuid());
	}
	
	return the_batchuser;
}

static char* the_batchserver = NULL;

void WL_setbatchserver(const char* value)
{
	if (the_batchserver) 
	{
		free(the_batchserver);
	}
	
	the_batchserver = wisp_strdup(value);
}
const char* WL_batchserver(void)
{
	if (!the_batchserver)
	{
		WL_setbatchserver(wispserver());
	}
	
	return the_batchserver;
}


static char* the_batchhold = NULL;
static char* the_batchrun  = NULL;

void WL_setbatchhold(const char* value)
{
	if (the_batchhold) 
	{
		free(the_batchhold);
	}
	
	the_batchhold = wisp_strdup(value);
}
const char* WL_batchhold(void)
{
	return the_batchhold;
}
void WL_setbatchrun(const char* value)
{
	if (the_batchrun) 
	{
		free(the_batchrun);
	}
	
	the_batchrun = wisp_strdup(value);
}
const char* WL_batchrun(void)
{
	return the_batchrun;
}

/*
**	The wisp_nativescreens() flag tells the runtime that COBOL native screen IO 
**	is being used so video must share the screen.
**
**	wisp_nativescreens() returns true if the NATIVESCREENS option is set and
**	it is called from an Acucobol runtime. (Later may add Micro Focus)
**
**	If in background then use WISP screens instead of native screens
**	because WISP screens knows how to deal with screen IO in background.
*/
int wisp_nativescreens(void)
{
	static int flag = -1;
	
	if (-1 == flag ) /* first time only */
	{
		flag = 0;
		if (!wbackground() && WL_get_wisp_option("NATIVESCREENS"))
		{
			if (wisp_acu_cobol())
			{
				WL_wtrace("OPTIONS","NATIVESCREENS","Using ACUCOBOL native screens");

				flag = 1;
				VL_set_vsharedscreen_true();
			}
		}
	}

	return flag;
}
int wisp_acu_nativescreens(void)
{
	static int flag = -1;
	if (-1 == flag ) /* first time only */
	{
		flag = 0;
		if (wisp_nativescreens() && wisp_acu_cobol())
		{
			flag = 1;
		}
	}

	return flag;
}

/*
**	The pfkeys12() flag tells the runtime that WISP should limit itself
**	to using 12 pekeys for internal screens (instead of 32).
*/
static int is_pfkeys12 = 0;
static void set_pfkeys12(void)
{
	is_pfkeys12 = 1;
}
int WL_pfkeys12(void)
{
	return is_pfkeys12;
}

/*
**	top_option is a linked-list of the options file options
*/
struct s_option
{
	struct s_option *next;
	char *keyword;
	char *trailing;
};
static struct s_option *top_option = NULL;

/*
**	ROUTINE:	add_option()
**
**	FUNCTION:	Add an option to the s_option list
**
**	DESCRIPTION:	Create the list if needed.
**			Add this keyword to the end of the list.
**			Warn about duplicate keywords.
**
**	ARGUMENTS:	
**	keyword		Options file keyword
**	trailing	The trailing portion of the options file entry, 
**			this may be an empty string "" but not NULL.
**
**	GLOBALS:	
**	top_option	The s_options linked-list
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void add_option(const char *keyword, const char *trailing)
{
	struct s_option *next_option;

	ASSERT(keyword);
	ASSERT(trailing);
	
	if (top_option)
	{
		/* 
		**	Loop to end of list:
		**	Check each item for duplicates.
		**	At end next_option will point to the last item in the list.
		*/ 
		next_option = top_option; 
		for(;;)
		{ 
			if (0 == strcmp(keyword, next_option->keyword))
			{
				werrlog(WERRCODE(83019),"Duplicate keyword",keyword,0,0,0,0,0,0);
			}

			if ( next_option->next )
			{
				next_option = next_option->next;
			}
			else
			{
				break;
			}
		} 
		ASSERT(next_option);
		
		next_option->next = wisp_malloc(sizeof(struct s_option));
		next_option = next_option->next;
	}
	else
	{
		top_option = wisp_malloc(sizeof(struct s_option));
		next_option = top_option;
	}

	next_option->next = (struct s_option *)NULL;
	next_option->keyword = wisp_strdup(keyword);
	next_option->trailing = wisp_strdup(trailing);

	WL_wtrace("OPTIONS","ADD","%s %s", next_option->keyword, next_option->trailing);
}

/*
**	ROUTINE:	WL_get_wisp_option()
**
**	FUNCTION:	Get the requested option value
**
**	DESCRIPTION:	Search the linked-list of option to see if this keyword was in  
**			the options file.
**
**	ARGUMENTS:	
**	keyword		The options file option (UPPERCASE)
**
**	GLOBALS:	
**	top_option	The s_options linked-list
**
**	RETURN:		The trailing portion of the option line.
**			An empty string "" maybe returned if the keyword has no state value.
**			NULL will be returned if option not in the list.
**
**	WARNINGS:	None
**
*/
const char *WL_get_wisp_option(const char *keyword)
{
	struct s_option *next_option;

	ASSERT(keyword);
	
	WL_load_options();

	for (next_option = top_option; next_option; next_option = next_option->next) 
	{
		if (0 == strcmp(keyword, next_option->keyword))
		{
			return next_option->trailing;
		}
	}
	return (const char *)NULL;
}


/*
**	ROUTINE:	WL_get_wisp_option_env()
**
**	FUNCTION:	Get the requested option value
**
**	DESCRIPTION:	First check the environment then the OPTIONS file.
**
**	ARGUMENTS:	
**	keyword		The options file option (UPPERCASE)
**
**	GLOBALS:	
**	top_option	The s_options linked-list
**
**	RETURN:		The trailing portion of the option line.
**			An empty string "" maybe returned if the keyword has no state value.
**			NULL will be returned if option not in the list.
**
**	WARNINGS:	None
**
*/
const char *WL_get_wisp_option_env(const char *keyword)
{
	const char *option;
	ASSERT(keyword);

	option = getenv(keyword);
	if (NULL == option)
	{
		option = WL_get_wisp_option(keyword);
	}

	return option;
}

/*
**	ROUTINE:	WL_wang_alphanumeric()
**
**	FUNCTION:	Check if the string contains valid characters
**
**	DESCRIPTION:	Valid Wang ALPHANUMERIC characters are A-Z a-z 0-9 @ $ #
**
**	ARGUMENTS:	
**	str		The string to check
**
**	GLOBALS:	none
**
**	RETURN:		
**	1		TRUE   - Contains only valid characters
**	0		FALSE  - Contains invalid characters
**
**	WARNINGS:	None
**
*/
int WL_wang_alphanumeric(const char* str)
{
	size_t i;

	for( i=0; str[i] != '\0'; i++)
	{
		if ( !(isalnum((int)str[i]) || '@'==str[i] || '$'==str[i] || '#'==str[i]) )
		{
			return 0;
		}
	}

	return 1;
}


/*
**	History:
**	$Log: wperson.c,v $
**	Revision 1.82  2003/07/14 15:20:06  gsl
**	fix PV/PL env buff size
**	
**	Revision 1.81  2003/07/09 20:09:13  gsl
**	Change internal PROGLIB/PROGVOL vars
**	
**	Revision 1.80  2003/04/07 14:49:28  gsl
**	Wang Alphanumeric includes $ @ #
**	
**	Revision 1.79  2003/03/27 21:37:01  gsl
**	Fix warning
**	
**	Revision 1.78  2003/03/20 18:31:49  gsl
**	Create add_logical_volume() routine to add volumes.
**	Add error reporting for errors in LGMAP
**	Validate VOLUME for wang alphanumeric characters
**	
**	Revision 1.77  2003/03/20 14:26:31  gsl
**	Fix DDS warnings
**	
**	Revision 1.76  2003/03/19 18:17:31  gsl
**	Move the pseudo volumes from mngfile.c to load_lgmap().
**	Pseudo volumes (HOME) (CWD) (CFG) (ROOT) will be enabled if
**	there are can validly defined.
**	
**	Revision 1.75  2003/03/17 20:29:41  gsl
**	enhance trace
**	
**	Revision 1.74  2003/02/04 17:22:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.73  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.72  2003/01/29 16:20:47  gsl
**	change loadpadnull() and WL_set_defs() to use const for the source arg
**	
**	Revision 1.71  2002/12/11 17:03:10  gsl
**	use wisp_unlink()
**	
**	Revision 1.70  2002/12/10 17:09:13  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.69  2002/12/09 19:15:37  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.68  2002/12/05 22:06:49  gsl
**	Prep for converting PERSONALITY files to text files.
**	Move the low level raw file IO into two routines.
**	Drop support for very old versions.
**	
**	Revision 1.67  2002/12/04 19:33:09  gsl
**	Move the $WISPCONFIG/xxx file path creation into the routines that need them.
**	
**	Revision 1.66  2002/12/04 16:22:21  gsl
**	Fix unneeded changing of WISPDEBUG mode
**	
**	Revision 1.65  2002/12/03 22:15:12  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.64  2002/11/27 20:09:31  gsl
**	Add WL_get_wisp_option_env() which looks for option first in environment then
**	in the OPTIONS file.
**	
**	Revision 1.63  2002/07/18 21:04:30  gsl
**	Remove MSDOS code
**	
**	Revision 1.62  2002/07/15 17:10:00  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.61  2002/07/12 20:40:40  gsl
**	Global unique WL_ changes
**	
**	Revision 1.60  2002/07/12 19:10:21  gsl
**	Global unique WL_ changes
**	
**	Revision 1.59  2002/07/11 20:29:18  gsl
**	Fix WL_ globals
**	
**	Revision 1.58  2002/07/10 21:05:35  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.57  2002/07/09 04:13:52  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.56  2002/07/02 21:15:35  gsl
**	Rename wstrdup
**	
**	Revision 1.55  2002/07/02 04:00:37  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.54  2002/07/01 04:02:43  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.53  2001/10/31 20:39:09  gsl
**	Added wisp_temp_defaults_path()
**	Rework the logic to build parent_path and temp_person_path now uses
**	wisptmpdir() /usr/tmp/wisptmp/DEFS_ as a prefix
**	
**	Revision 1.52  2001-09-07 11:56:03-04  gsl
**	Remove all the VMS and MSDOS ifdefs
**
**	Revision 1.51  1999-09-15 09:18:56-04  gsl
**	Enhance WL_load_options() to support backslash continued long-lines.
**
**	Revision 1.50  1999-08-18 17:10:45-04  gsl
**	Re-do Jorge's changes to load_lgmap() he was using strtok() which was
**	behaving wrong on Solaris 2.6
**
**	Revision 1.49  1998-10-23 11:09:07-04  gsl
**	Add tracing to LPMAP, FORMS, PRMAP
**
**	Revision 1.48  1998-10-22 17:20:52-04  gsl
**	Clean up the batch and print queue options handling.
**	Add generic print queue support PQCMD.
**
**	Revision 1.47  1998-09-11 10:21:49-04  gsl
**	Change the wisp_nativescreens() routine to return FALSE if in background.
**
**	Revision 1.46  1998-08-25 13:48:28-04  gsl
**	Move the LGMAP trace statements to after $env values resolved and re-word text.
**
**	Revision 1.45  1998-08-25 10:12:40-04  gsl
**	Change load_forms() to strdup the form string so not limited to 80
**
**	Revision 1.44  1998-08-03 17:23:37-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.43  1998-03-16 11:44:12-05  gsl
**	Fixed set_nativescreens() to only call set_vsharedscreen() if
**	called from an Acucobol runtime.
**
**	Revision 1.42  1997-12-04 18:11:56-05  gsl
**	Changed winnt.h to wispnt.h
**
**	Revision 1.41  1997-10-23 16:24:01-04  gsl
**	Add WL_get_wisp_option() and add_option() to support easier adding
**	of new options.
**
**	Revision 1.40  1997-10-17 11:15:28-04  gsl
**	Changed nativescreens() so that it only returns true if both the option
**	is set and this is acu_cobol.
**
**	Revision 1.39  1997-10-13 15:54:20-04  gsl
**	Wrong error code was being issued for short lines in PRMAP and LPMAP
**
**	Revision 1.38  1997-09-30 14:06:58-04  gsl
**	Add include of video.h to fix warning
**
**	Revision 1.37  1997-09-22 13:13:44-04  gsl
**	Add nativescreens() and pfkey12() flags
**
**	Revision 1.36  1997-08-25 14:40:52-04  gsl
**	change load_lpmap() so that it tests first before it deletes trailing NL's
**
**	Revision 1.35  1997-08-23 18:36:45-04  gsl
**	Move config routines to wispcfg.c
**	Move the batch options routines from submit.c
**
**	Revision 1.34  1997-08-23 15:37:41-04  gsl
**	Add all the WIN32 BATCH options for support of AQM
**	Add PQLP as a replacement for IDSIPRINTOFF to indicate print by LP
**
**	Revision 1.33  1997-07-16 15:10:21-04  gsl
**	Add WL_wtrace() to report thr path of the personality files
**
**	Revision 1.32  1997-04-03 16:45:34-05  gsl
**	Removed the entry werrlog() for WL_wpload
**
**	Revision 1.31  1996-12-06 18:40:11-05  jockc
**	added CQMAP file and access functions. for win32
**	mapping of submit class to queue name
**
**	Revision 1.30  1996-11-08 13:47:53-08  gsl
**	Changed USESOFTLINK() and USEHARDLINK() to pass back the previous state
**	which enables the app to maintian the state.
**
**	Revision 1.29  1996-10-25 14:05:47-07  gsl
**	Fix call to wanguid3() which now returns a const.
**
**	Revision 1.28  1996-10-08 17:30:36-07  gsl
**	move wisphomedir() and wisptmpbasedir() to wispcfg.c
**
**	Revision 1.27  1996-09-13 11:36:58-07  jockc
**	added err check for LPMAP and PRMAP for badly formed records,
**	also added skip of blank lines
**
**	Revision 1.26  1996-09-10 10:45:46-07  gsl
**	Fix test in wisphomedir()  from "||" to "&&"
**	Fix compiler warnings
**
**	Revision 1.25  1996-09-05 16:40:35-07  jockc
**	added (forgotten) ifdef for WIN32
**
**	Revision 1.24  1996-09-05 10:51:28-07  jockc
**	add ifdef for WIN32 for functions that load some
**	$WISPCONFIG files, specifically FORMS
**
**	Revision 1.23  1996-09-04 17:19:11-07  gsl
**	moved the softlink stuff from link.c.
**	It was causing too much stuff to be pulled in on a link of utilities
**
**	Revision 1.22  1996-09-03 14:40:21-07  gsl
**	Add support for the OPTIONS file options USESOFTLINK/USEHARDLINK
**
**	Revision 1.21  1996-08-27 17:25:59-07  gsl
**	Share the unix PROGLIB/PROGVOL code with NT
**
**	Revision 1.20  1996-08-26 17:07:21-07  gsl
**	For NT all the temp personality file get written to wisptmpdir()
**	Added delete_defaults_temp() to delete the temp personality file
**
**	Revision 1.19  1996-08-23 14:09:32-07  gsl
**	Fix the buildconfigpaths() logic for NT for temp personality files.
**	Added wisptmpdir() wisphomedir() wispprbdir() wisptmpbasedir() routines
**	to centralize "/usr/tmp" hardcoded paths.
**	Wrote WIN32 version of the WL_ttyid5() routine
**
**	Revision 1.18  1996-08-22 17:29:37-07  gsl
**	in genvorkvol() NT now shares code with unix and the gid/pid is written
**	out as a hex number instead of a decimal because it can be negative
**
**	Revision 1.17  1996-07-17 14:58:08-07  gsl
**	Change to use wmalloc(), removed all the error checking logic
**	that was following each malloc() as it is done in wmalloc()
**
**	Revision 1.16  1996-07-17 09:27:07-07  gsl
**	Fix to use wispconfigdir()
**
**	Revision 1.15  1996-07-11 16:24:10-07  gsl
**	Fix includes and prototypes and reuse msdos and unix code for NT
**
**	Revision 1.14  1995-09-25 10:18:40-07  gsl
**	change to use osddefs.h
**
 * Revision 1.13  1995/04/25  09:54:55  gsl
 * drcs state V3_3_15
 *
 * Revision 1.12  1995/04/17  11:47:54  gsl
 * drcs state V3_3_14
 *
 * Revision 1.11  1995/03/20  13:28:32  gsl
 * For WL_get_defs() and WL_set_defs() use a void *.
 *
 * Revision 1.10  1995/03/20  13:18:43  gsl
 * prototyped everything
 *
 * Revision 1.9  1995/03/20  11:16:14  gsl
 * added routine WL_build_wisp_config_path() which is used to build a path
 * to a file in the wisp config directory.
 *
 * Revision 1.8  1995/03/10  11:15:39  gsl
 * removed video refs and reorged headers
 *
**
**
*/
