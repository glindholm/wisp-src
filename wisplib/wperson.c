static char copyright[]="Copyright (c) 1988-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
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
**	wpload()
**	save_defaults()
**	load_defaults()
** 	get_defs();
** 	set_defs();
** 	write_defaults_to_file();
** 	read_defaults_from_file();
** 	load_options();
** 	clearprogsymb();
** 	setprogdefs();
** 	saveprogdefs();
** 	restoreprogdefs();
**	get_dispfac_char()
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

#ifdef VMS
#include <descrip.h>
#include <jpidef.h>
#include <ssdef.h>
#endif

#ifdef unix
#include <unistd.h>
#endif

#ifndef VMS
#include <sys/types.h>
#include <sys/stat.h>
#endif /* !VMS */

#ifdef _MSC_VER
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

#ifdef VMS
#define 	WISP_USER_PERSON_FILE		"SYS$LOGIN:PERSONALITY.DAT"
#define		WISP_SYSTEM_PERSON_FILE		"WISP$CONFIG:PERSONALITY.DAT"
#endif	/* VMS */

#ifdef unix
#define		WISP_SYSTEM_PERSON_FILE		"PERSONALITY"
#define 	WISP_USER_PERSON_FILE		"PERSONALITY"
#endif	/* #ifdef unix */

#if defined(WIN32)
#define		WISP_SYSTEM_PERSON_FILE		"PERSONALITY"
#define 	WISP_USER_PERSON_FILE		"PERSONALITY"
#endif	/* WIN32 */

#if defined(MSDOS)
#define		WISP_SYSTEM_PERSON_FILE		"DEFAULTS.SYS"
#define 	WISP_USER_PERSON_FILE		"DEFAULTS.USR"
#endif	/* MSDOS */

/*
83001	%%WPERSON-I-ENTRY Entry into %s

83002	%%WPERSON-F-PID Missing WISP_PID_ENV
83003	%%WPERSON-I-LOADING Loading the personality
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

										/*						*/
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

typedef struct	{
			struct cqmap_id *next;					/* pointer to the next one		*/
			char 	*queue;						/* The queue 				*/
			char 	class;						/* the job class			*/
		} cqmap_id;


typedef struct {
		int		fac_font;
		unsigned char	fac_dchar;
	       } dispfac_item;

/*
**	Globals and Externals
*/
char language_path[80];								/* the path of the IVS translation file         */

int opt_max_pages = 0;								/* Max prb pages				*/
int opt_max_parms = 0;								/* Max prb parms				*/

/*
**	Static data
*/

static usr_defaults defaults;							/* the actual defaults record		*/
static defaults_loaded = 0;							/* Have defaults been loaded		*/

static forms_id 	*forms_list = NULL;
static prmap_id 	*prmap_list = NULL;
static scmap_id 	*scmap_list = NULL;
static cqmap_id 	*cqmap_list = NULL;
static prt_id		*prt_list = NULL;
static logical_id 	*logical_list = NULL;

#ifdef VMS
static pq_id		*pq_list = NULL;
static lat_id 		*lat_list = NULL;
static term_id 		*term_list = NULL;
#endif /* VMS */

static logical_id	*logical_ptr;
static scmap_id 	*scmap_ptr;
static cqmap_id 	*cqmap_ptr;
static forms_id 	*forms_ptr;
static prmap_id 	*prmap_ptr;

static char curr_progvol[7] = "      ";						/* The current in memory PROGVOL value		*/
static char curr_proglib[9] = "        ";					/* The current in memory PROGLIB value		*/

static int  paths_built = 0;
static char person_path[80];
static char parent_path[80];
static char temp_person_path[80];
static char system_person_path[80];

static char ttmap_path[80];
static char pqmap_path[80];
static char lpmap_path[80];
static char lgmap_path[80];
static char scmap_path[80];
static char cqmap_path[80];
static char forms_path[80];
static char prmap_path[80];
static char options_path[80];
static char dispfac_path[80];


#ifdef VMS
#define LIB$K_CLI_GLOBAL_SYM	2
static char	constr[sizeof(usr_defaults)+1];					/* A string to hold the usage constants.	*/
static $DESCRIPTOR(sym,"$W_USAGE_CONSTANTS");					/* The descriptor of the usage constant symbol.	*/
static $DESCRIPTOR(conres,constr);						/* A descriptor to hold the constant string.	*/
#endif

static int dispfac_toggle = FALSE;						/* Init display FAC chars as FALSE.		*/
static dispfac_item dispfac_array[256];						/* Define array for displayable FACs.		*/
static load_df_first = TRUE;							/* First time in flag to load table.		*/


/*
**	Function Prototypes
*/

static int read_from_file(usr_defaults *the_def, char *the_name);		/* Load the personality from the file.	*/
static int write_to_file(usr_defaults *the_def, char *the_name);
static int load_defaults_temp(usr_defaults *the_def);				/* Read defaults from temp area			*/
static int save_defaults_temp(usr_defaults *the_def);				/* Store the usage constants in the sym	*/
static int build_config_paths(void);
static void validate_defaults(usr_defaults *the_def);
static void genworklib(char *worklib);
static int genworkvol(char *workvol);
static int genspoollib(char *spoollib);
static void loadpadnull(char *dest, char *src, int size);
static void getprogvol(char *result);
static void getproglib(char *result);
static void setprogvol(char *progvol);
static void setproglib(char *proglib);
#ifdef VMS
static void alloc_term(term_id **term_ptr);
static void alloc_lat(lat_id **lat_ptr);
#endif

static load_ttmap(void);
static void load_lpmap(void);
static void load_pqmap(void);

#ifndef VMS
static void get_logical_list_item(void);
static void load_lgmap(void);
#endif

static void load_scmap(void);
static void load_cqmap(void);
static void load_forms(void);
static void load_prmap(void);
static void load_dispfac(void);

static void set_pfkeys12(void);

static void add_option(const char *keyword, const char *trailing);


/*==============================================================================================================================*/
/*
**	wpload	This routine used to be responsible for loading all the device info lists
**		as well as the defaults structure and any other config files.
**		This is no longer required because each of these things will be loaded
**		dynamically if and only if they are used.
**		Currently only load_options() is done.
*/
void wpload(void)
{
	static int loaded = 0;							/* flag to indicate info is loaded		*/

	if (loaded) return;							/* already done					*/

	load_options();								/* Load the runtime options file.	*/

	loaded = 1;								/* already loaded now			*/
}

/*
	Use the following routine to manipulate the defaults struct.

	save_defaults()			Call after a set_defs() to move defaults to temp-area.
	load_defaults()			Call to load defaults from temp-area
	write_defaults_to_file()	Call to write temp-area to a file
	read_defaults_from_file()	Call to load temp-area from a file
*/
int save_defaults(void)
{
	if (defaults_loaded)
	{
		save_defaults_temp(&defaults);					/* Save defaults in temp area			*/
	}
	return(0);
}

int load_defaults(void)
{
	if ( !load_defaults_temp(&defaults) )					/* First try the temp defaults			*/
	{
		read_defaults_from_file("");					/* Otherwise, read the file.			*/
	}
	defaults_loaded = 1;
	return(0);
}

int write_defaults_to_file(char *file)
{
	if (!defaults_loaded)
	{
		load_defaults();						/* First ensure defaults are loaded		*/
	}
	return write_to_file(&defaults,file);					/* write defaults to the file			*/
}

int read_defaults_from_file(char *file)
{
	return read_from_file(&defaults,file);
}

static int read_from_file(usr_defaults *the_def, char *the_name)		/* Load the personality from the file.	*/
{
	FILE *the_file;
	int retfl;								/* Set flag if use defaults.		*/

	defaults_loaded = 1;							/* This routine will always succeed	*/

	if ( ! paths_built ) build_config_paths();
	retfl = 0;								/* Initialize flag.			*/
	if (the_name && *the_name)
	{
#ifdef VMS
		the_file = fopen(the_name,"r", "dna=*.dat;0");			/* Open the requested one.		*/
#else
		the_file = fopen(the_name, FOPEN_READ_BINARY);			/* Open the requested one.		*/
#endif
		wtrace("WPERSON", "READ", "Open file [%s] %s", the_name, (the_file)? "OK":"FAILED");
	}
	else 
	{
		the_file = fopen(person_path,FOPEN_READ_BINARY);		/* try to load the local user's default	*/

		wtrace("WPERSON", "READ", "Open person_path [%s] %s", person_path, (the_file)? "OK":"FAILED");
	}

	if (!the_file)								/* not found, go for the system file	*/
	{
		the_file = fopen(system_person_path,FOPEN_READ_BINARY);		/* try to open it			*/

		wtrace("WPERSON", "READ", "Open system_person_path [%s] %s", system_person_path, (the_file)? "OK":"FAILED");
	}

	if (!the_file)								/* neither file found			*/
	{									/* set our defaults			*/
		retfl = 1;							/* Set flag to detect use of defaults.	*/
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
		the_def->kb_map[0] = '\0';					/* Clear keyboard map type name.	*/
		the_def->flags = 0x0FFFFFFFF;					/* All things are enabled.		*/

		the_def->pf_version = 0L;					/* Unset version.			*/
	}
	else
	{
		fread(the_def,sizeof(usr_defaults),1,the_file);			/* read the info.			*/
		fclose(the_file);
	}

	validate_defaults(the_def);						/* Validate the version info.		*/

	genworklib(the_def->worklib);						/* Generate the worklib			*/
	genworkvol(the_def->workvol);						/* Generate the workvol			*/

	strcpy(the_def->proglib,"        ");					/* Always reset PV/PL			*/
	strcpy(the_def->progvol,"      ");

	save_defaults();							/* create the symbol.			*/

	return(retfl);
}
										/* save a user default personality	*/
static int write_to_file(usr_defaults *the_def, char *the_name)
										/* NOTE the structure MUST be properly	*/
										/* loaded or there will be a problem!	*/
{
	FILE 	*the_file;
	int 	amt;
	char	*fptr;

	if ( ! paths_built ) build_config_paths();

	if (*the_name) 
	{
		fptr = the_name;						/* Use the supplied name.		*/

		wtrace("WPERSON", "WRITE", "Write to file [%s]", fptr);
	}
	else
	{
		fptr = person_path;						/* Use the default.			*/

		wtrace("WPERSON", "WRITE", "Write to person_path [%s]", fptr);
	}
	
#ifdef VMS
	the_file = fopen(fptr,"w","dna=*.dat;0");				/* Open the file.			*/
#else
	the_file = fopen(fptr, FOPEN_WRITE_BINARY);				/* Open the file.			*/
#endif

	if (!the_file)								/* Open failed.				*/
	{
		werrlog(ERRORCODE(20),"write_to_file",fptr,errno,0,0,0,0,0);
		return(-1);
	}
	amt = fwrite(the_def,sizeof(usr_defaults),1,the_file);			/* Write out the usage constants.	*/
	fclose(the_file);
	if (amt == 0)								/* Write failed.			*/
	{
		werrlog(ERRORCODE(22),"write_to_file",fptr,errno,0,0,0,0,0);
		return(-1);
	}
	return(0);
}

static int load_defaults_temp(usr_defaults *the_def)				/* Read defaults from temp area			*/
{
#ifdef VMS
	int4 status;
	int4 len,tabtyp;

	tabtyp = LIB$K_CLI_GLOBAL_SYM;
	status = lib$get_symbol(&sym,&conres,&len,&tabtyp);			/* Get it's contents.				*/
	if (status != SS$_NORMAL) return(0);					/* Not loaded					*/
	memcpy(the_def,constr,sizeof(usr_defaults));				/* Loaded					*/

#else /* !VMS */

	FILE	*fp;
	int	size;

	size = 0;
	if ( ! paths_built ) build_config_paths();
	if ( fp = fopen( temp_person_path, FOPEN_READ_BINARY ) )
	{
		wtrace("WPERSON", "READ", "Open temp_person_path [%s] OK", temp_person_path);

		size = fread( (char *)the_def, sizeof( *the_def ), 1, fp );
		fclose( fp );
	}
	if ( !size && wbackground() )						/* If temp not found && background		*/
	{
		if ( fp = fopen( parent_path, FOPEN_READ_BINARY ) )		/* then  copy from parent			*/
		{
			wtrace("WPERSON", "READ", "Open parent_path [%s] OK", parent_path);

			size = fread( (char *)the_def, sizeof( *the_def ), 1, fp );
			fclose( fp );
		}
	}
	if (!size) return(0);							/* Not loaded					*/
#endif /* !VMS */

	validate_defaults(the_def);						/* Validate the version info.			*/
	genworklib(the_def->worklib);						/* Generate the worklib				*/
	genworkvol(the_def->workvol);						/* Generate the workvol				*/

	return(1);								/* Successfully loaded				*/
}

#ifdef VMS
static int save_defaults_temp(usr_defaults *the_def)				/* Store the usage constants in the sym	*/
{
	int4 status;
	int4 len,tabtyp;

	tabtyp = LIB$K_CLI_GLOBAL_SYM;
	memcpy(constr,the_def,sizeof(usr_defaults));				/* Copy them into the descriptor.	*/
	status = lib$set_symbol(&sym,&conres,&tabtyp);				/* Now set the global symbol.		*/
	if (status == SS$_NORMAL)
	{
		return(1);							/* We were sucessfull.			*/
	}
	else
	{
		return(0);							/* Not sucessfull.			*/
	}
}
#endif /* VMS */

#if defined(unix) || defined(MSFS)
static int save_defaults_temp( usr_defaults *the_def )				/* unix equv to LIB$SET_SYMBOL.			*/
										/* Write the PERSONALITY## file from memory.	*/
{
	FILE	*fp;
	int	size;

	if ( ! paths_built ) build_config_paths();

	wtrace("WPERSON", "WRITE", "Write to temp_person_path [%s]", temp_person_path);

	fp = fopen( temp_person_path, FOPEN_WRITE_BINARY );			/* Open/Create the temp personality		*/

	if (!fp)								/* If Open fails then				*/
	{
		makepath(temp_person_path);					/* ensure path exists				*/
		fp = fopen( temp_person_path, FOPEN_WRITE_BINARY );		/* try the open again.				*/
	}

	if (!fp)
	{
		werrlog(ERRORCODE(20),"save_defaults_temp",temp_person_path,errno,0,0,0,0,0);
		return( 0 );
	}

	size = fwrite( (char *)the_def, sizeof( *the_def ), 1, fp );
	fclose( fp );
	chmod( temp_person_path, 0666 );
	return(size); 								/* size will eq 1 or 0				*/
}
#endif

void delete_defaults_temp(void)
{
#if defined(unix) || defined(WIN32) || defined(MSDOS)
	if ( ! paths_built ) build_config_paths();
	unlink(temp_person_path);
#endif
}

#if defined(unix) || defined(WIN32)
char *wforms(int num)								/* return the string for form#		*/
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

char *getprmap(int num)								/* return the string for printer #	*/
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

char *wlpclass(char lpclass)								/* return the string for lpclass#	*/
{
	prt_id	*prt_ptr;

	prt_ptr = get_prt_list();
	while( prt_ptr )
	{
		if ( prt_ptr->class == lpclass ) return( prt_ptr->prt_string );
		prt_ptr = (prt_id *)prt_ptr->next;
	}

	return("");
}
#endif /* unix */

#if defined(unix) || defined(WIN32)
int ttyid5(char *tty)
{
	char	*ptr;

	if ( wbackground() )
	{
		if ( ptr = getenv( WISP_TTY_ENV ) )
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
#endif	/* unix || WIN32 */

/*
**	ROUTINE:	build_wisp_config_path()
**
**	FUNCTION:	Build path to a file in the wispconfig directory.
**
**	DESCRIPTION:	Takes a base filename (like "TTMAP") and creates
**			a full path to it by adding on the wispconfig
**			path.  For VMS it also adds the ".DAT" extension.
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
void build_wisp_config_path(char *file, char *path)
{
#ifdef VMS
	strcpy(path,wispconfigdir());
	strcat(path,file);
	strcat(path,".DAT");
#else
	buildfilepath(path, wispconfigdir(), file);
#endif
}

#define		WISP_TERMINAL_FILE		"TTMAP"
#define		WISP_PRINTER_FILE		"LPMAP"
#define		WISP_PROC_FILE			"PQMAP"
#define		WISP_OPTIONS_FILE		"OPTIONS"
#define		WISP_DISPFAC_FILE		"DISPFAC"
#define		WISP_SUBCLASS_FILE		"SCMAP"
#define		WISP_QUEUECLASS_FILE		"CQMAP"
#define		WISP_LOGICAL_FILE		"LGMAP"
#define		WISP_FORMS_FILE			"FORMS"
#define		WISP_PRMAP_FILE			"PRMAP"

#if defined(unix) || defined(MSFS)
static int build_config_paths(void)
{
/*
	$WTMP defaults to /usr/tmp or C:\\TMP for DOS

	person_path			$HOME/$WISP_USER_PERSON_FILE
	temp_person_path		$WTMP/$WISP_TEMP_PERSON_PREFIX$TTY$UID		(forground unix)
					$WTMP/$WISP_TEMP_PERSON_PREFIX$UID$W_T_P_SUFFIX	(forground unix)
					$WTMP/$WISP_TEMP_PERSUB_PREFIX$WISP_PID_ENV	(background unix only)
	parent_path			$WTMP/$WISP_TEMP_PERSON_PREFIX$TTY$UID		(forground  unix only)
					$WTMP/$WISP_TEMP_PERSON_PREFIX$WISP_TTY_ENV$UID	(background unix only)
	system_person_path		$WISPCONFIG/$WISP_SYSTEM_PERSON_FILE
	forms_path			$WISPCONFIG/$WISP_FORMS_FILE
*/

	char	uid[4];
	char	gid[20];

	if ( paths_built ) return(0);

	werrlog(ERRORCODE(1),"build_config_paths",0,0,0,0,0,0,0);

	memcpy(uid,wanguid3(),3);
	if(	 uid[0] == ' ' )	uid[0] = '\0';
	else if( uid[1] == ' ' )	uid[1] = '\0';
	else if( uid[2] == ' ' )	uid[2] = '\0';
	else				uid[3] = '\0';

	sprintf(gid,"%08X", wgetpgrp());

	buildfilepath( system_person_path, wispconfigdir(), WISP_SYSTEM_PERSON_FILE );

	buildfilepath( person_path, wisphomedir(NULL), WISP_USER_PERSON_FILE );

	buildfilepath( parent_path, wispdefdir(NULL), WISP_TEMP_PERSON_PREFIX );

#ifdef unix
	{
		char	tty[10];
		ttyid5(tty);
		strcat( parent_path, tty );
	}
	strcat( parent_path, uid );
#endif
#ifdef WIN32
	strcat( parent_path, gid );
	strcat( parent_path, uid );
#endif
#ifdef MSDOS
	strcat( parent_path, uid );
	strcat( parent_path, ".TMP" );
#endif

	if (wbackground())
	{
		buildfilepath(temp_person_path, wispdefdir(NULL), WISP_TEMP_PERSUB_PREFIX );
		strcat(temp_person_path, gid);
	}
	else /* foreground */
	{
		strcpy( temp_person_path, parent_path );
	}
	
	build_wisp_config_path(WISP_TERMINAL_FILE, ttmap_path);
	build_wisp_config_path(WISP_PRINTER_FILE, lpmap_path);
	build_wisp_config_path(WISP_PROC_FILE, pqmap_path);
	build_wisp_config_path(WISP_SUBCLASS_FILE, scmap_path);
	build_wisp_config_path(WISP_QUEUECLASS_FILE, cqmap_path);
	build_wisp_config_path(WISP_LOGICAL_FILE, lgmap_path);
	build_wisp_config_path(WISP_FORMS_FILE, forms_path);
	build_wisp_config_path(WISP_PRMAP_FILE, prmap_path);
	build_wisp_config_path(WISP_OPTIONS_FILE, options_path);
	build_wisp_config_path(WISP_DISPFAC_FILE, dispfac_path);

	paths_built = 1;
	return(0);
}
#endif	/* unix  && MSFS */

#ifdef VMS
static int build_config_paths(void)
{
	if (paths_built) return(0);
	werrlog(ERRORCODE(1),"build_config_paths",0,0,0,0,0,0,0);

	strcpy( person_path, WISP_USER_PERSON_FILE );
	strcpy( parent_path, WISP_USER_PERSON_FILE );
	strcpy( system_person_path, WISP_SYSTEM_PERSON_FILE );

	build_wisp_config_path(WISP_TERMINAL_FILE, ttmap_path);
	build_wisp_config_path(WISP_PRINTER_FILE, lpmap_path);
	build_wisp_config_path(WISP_PROC_FILE, pqmap_path);
	build_wisp_config_path(WISP_OPTIONS_FILE, options_path);
	build_wisp_config_path(WISP_DISPFAC_FILE, dispfac_path);

	paths_built = 1;
	return(0);
}
#endif	/* VMS */

/* Validate the structure, and set up any initial values which aren't in the old structure versions.				*/

static void validate_defaults(usr_defaults *the_def)
{
										/****************************************/
										/* This switch/case mechanism was	*/
										/* changed to an "if" structure due	*/
										/* to MSDOS porting: a switch can not	*/
										/* use a int4, it must be a short.	*/
										/* Since the version numbers are int4,	*/
										/* ifs are needed instead of switches.	*/
										/****************************************/

	if ( the_def->pf_version != 61591 )					/* If before the version of 6/15/91	*/
	{
		if ( the_def->pf_version != 52490 )				/* If before the version of 5/24/90	*/
		{
			if ( the_def->pf_version != 103089 )			/* If before the version of 10/30/89	*/
			{
				if ( the_def->pf_version != 60889 )		/* If before the version of 6/08/89	*/
				{						/* Unrecognized version, set defaults.	*/
					the_def->kb_map[0] = '\0';		/* Clear keyboard map type name.	*/
				}
										/* If it's the version of 6/8/89	*/
				the_def->psb_select = 'B';			/* Use default characteristics for 	*/
				the_def->psb_charset = 0;			/*  pseudo blank.			*/
				the_def->psb_rendition = 2;
			}
										/* If it's the version of 10/30/89	*/
			the_def->prt_lines = 55;				/* printer default lines per page is 55	*/
			the_def->mp_cursor = TRUE;				/* Display cursor on menu pick flag	*/
			the_def->automove = FALSE;				/* Flag for auto_move default is FALSE.	*/
			the_def->autotab = TRUE;				/* Flag for auto_tab default is TRUE.	*/
		}
										/* If it's the version of 05/24/90	*/

#if defined(MSDOS) || defined(WIN32)
		the_def->bgchange = TRUE;					/* Change background by default.	*/
		the_def->bgcolor = TRUE;					/* Default background color is Grey.	*/
#else
		the_def->bgchange = FALSE;					/* Don't change background by default.	*/
		the_def->bgcolor = FALSE;					/* Default background color is black.	*/
#endif
		the_def->excolor = FALSE;					/* Exit with a black background.	*/
	}
										/* If it's the version of 06/15/91.	*/

/**************************************** MAKE SURE THIS VALUE IS CORRECT WHEN VERSIONS CHANGE **********************************/
	the_def->pf_version = 61591;						/* Set the file version.		*/
}


static void genworklib(char *worklib)
{
	char temp[20];

#ifdef VMS
	int4  pid;								/* Process id				*/
	unsigned status;
	unsigned short retlen;
	struct	{
		short unsigned int	buflen;					/* the length of the buffer		*/
		short unsigned int 	item_code;				/* the code for the request to GETDVI	*/
		char 			*bufptr;				/* a pointer to the buffer		*/
		short unsigned int	*retlen;				/* the return length of the buffer	*/
		int4 		endbuf;					/* the end of the buffer		*/
	} pidbuf;

	pidbuf.buflen = 4;							/* Now get process ID of the current 	*/
	pidbuf.item_code = JPI$_MASTER_PID;					/* process in the tree.			*/
	pidbuf.retlen = &retlen;
	pidbuf.bufptr = (char *) &pid;
	pidbuf.endbuf = 0;

	status = sys$getjpi((long) 0,(long) 0,(long) 0, &pidbuf,(long) 0,(long) 0,(long) 0);	/* Get the ID.			*/

	if (status != SS$_NORMAL) return(status);				/* Some error.				*/
	sprintf(temp,"%08X", pid);						/* Format the PID as a hex number	*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
	memcpy(worklib, "WK", 2);						/* Overlay the first to chars as "WK"	*/
#endif	/* VMS */

#if defined(unix) || defined(WIN32)
	sprintf(temp,"%08X", wgetpgrp());					/* Format the PID			*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
	memcpy(worklib, "WK", 2);						/* Overlay the first to chars as "WK"	*/
#endif	/* unix || WIN32 */

#ifdef MSDOS
	pid = 0;
	sprintf(temp,"WORK%s    ", longuid());					/* Format the worklib			*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
	upper_mem(worklib,8);							/* Make uppercase			*/
#endif	/* MSDOS */
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
	memcpy(&spoollib[1],wanguid3(),3);
	if ( spoollib[2] == ' ' ) memcpy( &spoollib[2], "PRT", 3);
	else if ( spoollib[3] == ' ' ) memcpy( &spoollib[3], "PRT", 3);
	else memcpy( &spoollib[4], "PRT", 3);
	return(1);
}

static int opt_linkvectoroff_flag = 0;
int opt_linkvectoroff(void)
{
	return opt_linkvectoroff_flag;
}

/*
	load_options:		Load the runtime OPTIONS file

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
int load_options(void)								/* Load the runtime OPTIONS file.		*/
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

	if (!first) return(0);
	first=0;
	werrlog(ERRORCODE(1),"load_options",0,0,0,0,0,0,0);

	if ( !paths_built ) build_config_paths();

	opt_printqueue = PQ_DEFAULT;
	opt_printqueue_manager = NULL;
	batchqueue_name = "";
	batchman_name = "";

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
			upper_string(keyword);

			trailing = inlin + strlen(keyword);
			while(' ' == *trailing) { trailing++; }

			add_option(keyword, trailing);
			
			if      (strcmp(keyword,"ERRFLAG") == 0)		/* Set the ERRFLAG (same as initwisp).		*/
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(ERRORCODE(18),"Invalid Value",inlin,0,0,0,0,0,0);
					continue;
				}
				opt_errflag_found = 1;
				opt_errflag = i;

			}
			else if (strcmp(keyword,"SIGNALSOFF") == 0)		/* Disable Unix signal trapping.		*/
			{
				opt_signalsoff = 1;
			}
			else if (strcmp(keyword,"SIGNALSON") == 0)		/* Enable Unix signal trapping.			*/
			{
				opt_signalsoff = 0;
			}
			else if (strcmp(keyword,"NULLISSPACE") == 0)		/* Set so NULLs will display as a space.	*/
			{
				opt_nulldisplay = 0;
			}
			else if (strcmp(keyword,"NULLISDOT") == 0)		/* Set so NULLs will display as a dot.		*/
			{
				opt_nulldisplay = 1;
			}
			else if (strcmp(keyword,"OUTPUTVERIFYOFF") == 0)	/* Turn off the PF3 to continue screens		*/
			{
				opt_outputverifyoff = 1;
			}
			else if (strcmp(keyword,"OUTPUTVERIFYON") == 0)		/* Turn on the PF3 to continue screens		*/
			{
				opt_outputverifyoff = 0;
			}
			else if (strcmp(keyword,"CREATEVOLUMEON") == 0)		/* UNIX auto create VOLUME if not found		*/
			{
				opt_createvolumeon = 1;
			}
			else if (strcmp(keyword,"CREATEVOLUMEOFF") == 0)	/* UNIX don't create VOLUME if not found	*/
			{
				opt_createvolumeon = 0;
			}
#ifdef unix
			else if (strcmp(keyword,"IDSIPRINTON") == 0)		/* UNIX use the IDSI print spooler		*/
			{
				opt_printqueue = PQ_UNIQUE;
			}
			else if (strcmp(keyword,"IDSIPRINTOFF") == 0)		/* UNIX use lp, not the IDSI print spooler	*/
			{
				opt_printqueue = PQ_LP;
			}
			else if (strcmp(keyword,"PQILP") == 0)			/* UNIX use ILP					*/
			{
				opt_printqueue = PQ_ILP;
			}
			else if (strcmp(keyword,"PQNP") == 0)			/* UNIX use NP					*/
			{
				opt_printqueue = PQ_NP;
			}
			else if (strcmp(keyword,"PQLP") == 0)			/* UNIX use LP					*/
			{
				opt_printqueue = PQ_LP;
			}
#endif
			else if (strcmp(keyword,"PQUNIQUE") == 0)		/* UNIX or WIN32 use UNIQUE			*/
			{
				opt_printqueue = PQ_UNIQUE;
			}
			else if (strcmp(keyword,"BATCHQUEUE") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				if (strcmp(value,"none")==0)
				{
					opt_batchqueue=0;
				}	
				else
				{
					opt_batchqueue=1;
					batchqueue_name = wstrdup(value);
				}
			}
			else if (strcmp(keyword,"BATCHMAN") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				if (strcmp(value,"none")==0)
				{
					opt_batchman=0;
				}	
				else
				{
					opt_batchman=1;
					cptr = getenv("BATCH_MAN");
					if (cptr && *cptr)
					{
						batchman_name = wstrdup(cptr);
						wtrace("OPTIONS","BATCHMAN","Overridden by $BATCHMAN = [%s]",batchman_name);
					}
					else
					{
						batchman_name = wstrdup(value);
					}
				}
			}
			else if (strcmp(keyword,"BATCHCMD") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchcmd(trailing);
			}
			else if (strcmp(keyword,"BATCHCMD95") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchcmd95(trailing);
			}
			else if (strcmp(keyword,"BATCHLOGVOL") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchlogvol(value);
			}
			else if (strcmp(keyword,"BATCHLOGLIB") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchloglib(value);
			}
			else if (strcmp(keyword,"BATCHPASS") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchpass(value);
			}
			else if (strcmp(keyword,"BATCHUSER") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchuser(value);
			}
			else if (strcmp(keyword,"BATCHSERVER") == 0)
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				setbatchserver(value);
			}
			else if (strcmp(keyword,"BATCHHOLD") == 0)
			{
				setbatchhold(trailing);
			}
			else if (strcmp(keyword,"BATCHRUN") == 0)
			{
				setbatchrun(trailing);
			}
			else if (strcmp(keyword,"IDNUMERIC") == 0)		/* UNIX EXTRACT ID returns Numeric user ID	*/
			{
				opt_idnumeric = 1;
			}
			else if (strcmp(keyword,"IDALPHA") == 0)		/* UNIX EXTRACT ID returns Alpha user ID	*/
			{
				opt_idnumeric = 0;
			}
			else if (strcmp(keyword,"IDFIVE") == 0)			/* UNIX EXTRACT ID returns chars 5-7 user ID	*/
			{
				opt_idfive = 1;
			}
			else if (strcmp(keyword,"IDONE") == 0)			/* UNIX EXTRACT ID returns char 1-3 user ID	*/
			{
				opt_idfive = 0;
			}
			else if (strcmp(keyword,"HELPSTYLE1") == 0)		/* Help is Wang style				*/
			{
				opt_helpstyle = 1;
			}
			else if (strcmp(keyword,"HELPSTYLE2") == 0)		/* Help is non-Wang style			*/
			{
				opt_helpstyle = 2;
			}
			else if (strcmp(keyword,"ALLSTATUSKEYS") == 0)		/* Pass All STATUS keys thru to user declaritive*/
			{
				opt_allstatuskeys = 1;
			}
			else if (strcmp(keyword,"LINKVECTOROFF") == 0)		/* Turn off use of linkvector()			*/
			{
				opt_linkvectoroff_flag = 1;
			}
			else if (strcmp(keyword,"MAXPRBPARMS") == 0)		/* Set the MAX PRB (putparm) number of parms	*/
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(ERRORCODE(18),"Invalid Value",inlin,0,0,0,0,0,0);
					continue;
				}
				opt_max_parms = i;

			}
			else if (strcmp(keyword,"MAXPRBPAGES") == 0)		/* Set the MAX PRB (putparm) number of parms	*/
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(ERRORCODE(18),"Invalid Value",inlin,0,0,0,0,0,0);
					continue;
				}
				opt_max_pages = i;

			}
			else if (strcmp(keyword,"WISPLANGUAGE") == 0)		/* Set language xlation file       */
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
#ifndef VMS
				buildfilepath(language_path,wispconfigdir(),value);
				if ( !fexists(language_path) )
				{
					werrlog(ERRORCODE(18),"Invalid Language File",inlin,0,0,0,0,0,0);
					continue;
				}
#endif /* !VMS */
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
				nativescreens();
			}
			else if (strcmp(keyword,"PFKEYS12") == 0)
			{
				/* We only have 12 pfkeys */
				set_pfkeys12();
			}
			else
			{
				/* Not an error: new add_option() mechanism! */
				/* werrlog(ERRORCODE(19),"Unknown keyword",inlin,0,0,0,0,0,0); */
			}
		}
		fclose(the_file);
	}

	/*
	**	Resolve conflicting options
	*/

	if (get_wisp_option("PQCMD"))
	{
		/*
		**	PQCMD overrides all other PQ options.
		*/
		opt_printqueue = PQ_GENERIC;
	}

#ifdef unix
	if (PQ_DEFAULT == opt_printqueue)
	{
		opt_printqueue = PQ_UNIQUE;
	}

	opt_printqueue_manager = wstrdup(get_wisp_option("PQMANAGER"));
	if (NULL == opt_printqueue_manager)
	{
		switch(opt_printqueue)
		{
		case PQ_UNIQUE:
			if (getenv("UNIQUE_MAN"))
			{
				opt_printqueue_manager = wstrdup(getenv("UNIQUE_MAN"));
			}
			else
			{
				opt_printqueue_manager = "unique -q -w";
			}
			break;
			
		case PQ_ILP:
			opt_printqueue_manager = "ilpman -q -w";
			break;

		case PQ_LP:
		case PQ_NP:
		case PQ_GENERIC:
		case PQ_DEFAULT:
			break;
		}

		if (NULL != opt_printqueue_manager)
		{
			wtrace("OPTIONS","PQMANAGER","Print Queue Manager command string is [%s]", opt_printqueue_manager);
		}
	}
#endif /* unix */
	
	return(0);
}

/*
	get_defs	Get a default based on the code.
*/
int get_defs(int code, void *void_ptr)
{
	char	*ptr;

	ptr = (char *)void_ptr;

	if (!defaults_loaded)
	{
		load_defaults();
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
		genspoollib(defaults.spoolib);					/* Generate the spoollib		*/
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
	set_defs	Set a default based on the code.

	* * * * WARNING * * * *  
		Set_defs only loads the value into the internal structure "defaults" -- after you finish a series of
		calls to "set_defs" you need to perform a "save_defaults()" to save these values into a temp area.

*/
int set_defs(int code, void *void_ptr)
{
	char *ptr;

	ptr = (char *)void_ptr;

	if (!defaults_loaded)
	{
		load_defaults();
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
static void loadpadnull(char *dest, char *src, int size)
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
		Save the values in SYMBOL on entry and reset them on exit of a cobol program. (initwisp & wispexit) This is
		in case a linked to program sets these values, you don't want them still around after you return from the link.
		Initwisp() must do a get_defs() for PROGVOL and PROGLIB to force an update of curr.

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

#define SHELL_PROGVOL	"_PROGVOL"
#define SHELL_PROGLIB	"_PROGLIB"

/*
	getprogvol	
	getproglib	These routines return the current value of PROGLIB and PROGVOL. 
			It checks curr_progvol/lib first and if not set it checks symbol then shell variable.  If found in
			symbol or shell variable it will update curr_proglib/vol.
			If there is still not current values it will return RUNLIB RUNVOL.
			These routines are only called from get_defs().
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
#if defined(unix) || defined(WIN32)
		else if (ptr = getenv(SHELL_PROGVOL))				/* Check the shell variable			*/
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
#endif
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
#if defined(unix) || defined(WIN32)
		else if (ptr = getenv(SHELL_PROGLIB))				/* Check the shell variable			*/
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
#endif
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
	clearprogsymb	This routine clears the PROGLIB and PROGVOL symbol both in defaults and in temp.
			This is called before a LINK or SUBMIT.
*/
int clearprogsymb(void)
{
	set_defs(DEFAULTS_PV,"      ");
	set_defs(DEFAULTS_PL,"        ");
	save_defaults();							/* Write changes to temp area			*/
	return(0);
}

/*
	setprogdefs	Set the shell variables PROGVOL and PROGLIB.
			This is only called during a LINK, LINKPROC, and SUBMIT.
*/
int setprogdefs(char *progvol, char *proglib)
{
	setprogvol(progvol);
	setproglib(proglib);
	return(0);
}
static void setprogvol(char *progvol)
{
#if defined(unix) || defined(WIN32)
	char	buff[20];
	sprintf(buff,"%s=%s",SHELL_PROGVOL,progvol);
	setenvstr(buff);
#endif /* unix || WIN32 */
}
static void setproglib(char *proglib)
{
#if defined(unix) || defined(WIN32)
	char	buff[20];
	sprintf(buff,"%s=%s",SHELL_PROGLIB,proglib);
	setenvstr(buff);
#endif /* unix || WIN32 */
}

/*
	saveprogdefs
	restoreprogdefs		These routines save the symbol values or PROGLIB and PROGVOL when a COBOL program
				is started and restore them on exit.
*/
static char	def_progvol[7] = "      ";
static char	def_proglib[9] = "        ";

int saveprogdefs(void)
{
#if defined(unix) || defined(WIN32)
	char	buff[20];

	get_defs(DEFAULTS_PV,buff);						/* Force an update of curr			*/
	get_defs(DEFAULTS_PL,buff);

	memcpy(def_progvol,defaults.progvol,6);					/* Save the entry values			*/
	memcpy(def_proglib,defaults.proglib,8);
#endif
	return(0);
}

int restoreprogdefs(void)
{
#if defined(unix) || defined(WIN32)
	set_defs(DEFAULTS_PV,def_progvol);					/* Restore the entry values			*/
	set_defs(DEFAULTS_PL,def_proglib);
	save_defaults();							/* Write changes to temp area			*/
#endif
	return(0);
}

#ifdef VMS
static void alloc_term(term_id **term_ptr)
{
	if (!term_list)								/* first time?				*/
	{
		term_list = (term_id *)wmalloc(sizeof(term_id));			/* get some memory			*/
		term_list->next = NULL;						/* set next pointer to zero		*/
		term_list->termname[0] = (char)0;
		term_list->termnum = 0;
		term_list->flags = 0;
		*term_ptr = term_list;						/* set up local pointer			*/
	}
	else
	{
		(*term_ptr)->next = (struct term_id *)wmalloc(sizeof(term_id));
		*term_ptr = (term_id *)(*term_ptr)->next;			/* set pointer				*/
		(*term_ptr)->next = NULL;					/* set next pointer to zero		*/
		(*term_ptr)->termname[0] = (char)0;
		(*term_ptr)->termnum = 0;
		(*term_ptr)->flags = 0;
	}
}

static void alloc_lat(lat_id **lat_ptr)
{
	if (!lat_list)								/* first time?				*/
	{
		lat_list = (lat_id *)wmalloc(sizeof(lat_id));			/* get some memory			*/
		lat_list->next = NULL;						/* set next pointer to zero		*/
		lat_list->latname[0] = (char)0;
		lat_list->termnum = 0;
		lat_list->flags = 0;
		*lat_ptr = lat_list;						/* set up local pointer			*/
	}
	else
	{
		(*lat_ptr)->next = (struct lat_id *)wmalloc(sizeof(lat_id));
		*lat_ptr = (lat_id *)(*lat_ptr)->next;				/* set pointer				*/
		(*lat_ptr)->next = NULL;					/* set next pointer to zero		*/
		(*lat_ptr)->latname[0] = (char)0;
		(*lat_ptr)->termnum = 0;
		(*lat_ptr)->flags = 0;
	}
}

term_id *get_term_list(void)
{
	if (!term_list)
	{
		load_ttmap();
	}
	return(term_list);
}

lat_id *get_lat_list(void)
{
	if (!lat_list)
	{
		load_ttmap();
	}
	return(lat_list);
}

/*
**
**	TTMAP		- Terminal definitions
**
*/
static load_ttmap(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256], tstr[32], buff[256];
	term_id *term_ptr;
	lat_id 	*lat_ptr;
	char 	*scn_ptr, *prm_ptr;
	char	tempc;
	int	lt_dev;
	int	i;
	int	flag;

	werrlog(ERRORCODE(1),"load_ttmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();

	term_list = NULL;							/* initialize the list				*/
	lat_list = NULL;

	the_file = fopen(ttmap_path,"r");					/* first load the terminal definitions		*/

	if (the_file)								/* no error opening file			*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			scn_ptr = &inlin[0];

			flag = 1;						/* switch to parse name, number			*/

			do							/* scan the line and extract the parms		*/
			{							/* skip over spaces, commas, newlines , tabs	*/
				if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
				{
					scn_ptr++;
				}
				else						/* copy this parm			*/
				{
					if (flag == 1)				/* copy term name			*/
					{
						flag = 2;			/* set the flag				*/
						prm_ptr = tstr;			/* load terminal name			*/
						*prm_ptr = *scn_ptr++;		/* get first char			*/
						if (*prm_ptr != '_')		/* have to put in a leading '_'		*/
						{
							tempc = *prm_ptr;	/* save char				*/
							*prm_ptr++ = '_';	/* put in '_'				*/
							*prm_ptr = tempc;	/* restore char				*/
						}
						prm_ptr++;			/* next char position			*/

						do
						{				/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						prm_ptr--;			/* need to look at last character	*/
						if (*prm_ptr != ':')		/* has to end with a colon		*/
						{
							prm_ptr++;		/* add one on...			*/
							*prm_ptr = ':';
						}
						prm_ptr++;			/* point to end				*/
						*prm_ptr = (char)0;		/* end with a null			*/

						if (!strncmp(tstr,"_LTA0:",6)) 	/* Is it a pseudo LT device?		*/
						{
							lt_dev = 1;
							alloc_lat(&lat_ptr);	/* Get mem for next lat device.		*/
						}
						else
						{
							lt_dev = 0;
							alloc_term(&term_ptr);
							strcpy(term_ptr->termname,tstr);	/* Save the name.		*/
						}
					}
					else if (flag == 2)			/* copy term number			*/
					{
						i = 0;
						do
						{				/* copy till no digits			*/
							i *= 10;
							i += (*scn_ptr++) - '0';
						} while (isdigit(*scn_ptr));	/* stop when no digits			*/

						if (lt_dev)			/* Is it a LAT device?			*/
						{

							lat_ptr->termnum = i;	/* Save the LAT term number.		*/
							prm_ptr = lat_ptr->latname;	/* point to where the name will go.	*/

							do 
							{
								scn_ptr++;	/* Skip over whitespace			*/
							}
							while ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\t'));

							while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
							      && (*scn_ptr != '\t'))
							{			/* copy till next whitespace		*/
								*prm_ptr++ = *scn_ptr++;
							}
							*prm_ptr = '\0';	/* Null terminate			*/
						}
						else
						{
							term_ptr->termnum = i;
						}

						flag = 0;			/* clear the flag			*/

						if (strpos(scn_ptr,"KEYPAD_OFF") != -1)		/* Set the keypad off bit	*/
						{
							flag |= KEYPAD_OFF;
						}

						if (strpos(scn_ptr,"SETUP_CORRECTLY") != -1)	/* User did it bit on.		*/
						{
							flag |= SETUP_CORRECTLY;
						}

						if (strpos(scn_ptr,"SPACES_BLINK") != -1)	/* Set the keypad off bit	*/
						{
							flag |= SPACES_BLINK;
						}

						if (strpos(scn_ptr,"PRO_RT") != -1)		/* Special Pro/RT-11 system.	*/
						{
							flag |= PRO_RT;
						}

						if (lt_dev) lat_ptr->flags = flag;
						else	    term_ptr->flags = flag;

						flag = 0;
					}
				}
			} while (*scn_ptr && flag);				/* Till a null or flag is clear.	*/
		}

		fclose(the_file);						/* close the term file			*/
	}
	else
	{									/* error opening the file		*/
		werrlog(ERRORCODE(20),"load_ttmap",ttmap_path,errno,0,0,0,0,0);
	}
}
#endif	/* VMS */
#if defined(unix) || defined(VMS) || defined(WIN32)
/*
**
**	LPMAP		printer definitions
**
*/

prt_id *get_prt_list(void)
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
#ifdef VMS
	int	pnum;
	int	flag;
	char	lptstr2[80], lptstr1[80];
	char 	*scn_ptr, *prm_ptr;
#endif

	if ( !paths_built ) build_config_paths();
	prt_list = NULL;							/* initialize the list			*/

	the_file = fopen(lpmap_path,"r");					/* now load the printer definitions	*/

	if (the_file)								/* no error opening file		*/
	{
		wtrace("LPMAP","ENTRY", "Loading LPMAP file [%s]",lpmap_path);

		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (inlin[0]=='\n')
			{
				continue;
			}
			if (!prt_list)						/* first time?				*/
			{
				prt_list = (prt_id *)wmalloc(sizeof(prt_id));	/* get some memory			*/
				prt_list->next = NULL;				/* set next pointer to zero		*/
				prt_ptr = prt_list;				/* set up local pointer			*/
			}
			else
			{
				prt_ptr->next = (struct prt_id *)wmalloc(sizeof(prt_id));/* get some memory			*/
				prt_ptr = (prt_id *)prt_ptr->next;		/* set pointer				*/
				prt_ptr->next = NULL;				/* set next pointer to zero		*/
			}
#ifdef VMS
			scn_ptr = &inlin[0];

			flag = 1;						/* switch to parse class, printer	*/
			*lptstr1 = '\0';					/* Initialize strings for number, qname	*/
			*lptstr2 = '\0';

			do							/* scan the line and extract the parms	*/
			{							/* skip over spaces, commas, newlines , tabs	*/
				if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
				{
					scn_ptr++;
				}
				else						/* copy this parm			*/
				{
					if (flag == 1)				/* copy class letter			*/
					{
						flag = 2;			/* set the flag				*/
						prt_ptr->class = *scn_ptr++;	/* get first char			*/
					}
					else if (flag == 2)
					{
						flag = 3;			/* set the flag				*/
						prm_ptr = lptstr1;		/* load next string			*/
						do
						{				/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						*prm_ptr = '\0';		/* null terminate			*/
						if ((*scn_ptr == '\n') || (*scn_ptr == '\0')) flag = 0;	/* signal we are done	*/
					}
					else if (flag == 3)
					{
						prm_ptr = lptstr2;		/* load next string			*/
						do
						{				/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						*prm_ptr = '\0';		/* null terminate			*/
						flag = 0;			/* signal we are done			*/
					}
					if (flag == 0)
					{
						if (*lptstr2 == '\0')		/* then lptstr1 is the qname		*/
						{
							prt_ptr->prtnum = 0;	/* Default to printer number 0.		*/
							strcpy(prt_ptr->qname,lptstr1);/* load queue name		*/
						}
						else
						{
							pnum = atoi(lptstr1);	/* Obtain the printer number		*/
										/* Error in atoi will default pnum = 0	*/
							prt_ptr->prtnum = pnum;	/* Load printer number			*/
							strcpy(prt_ptr->qname,lptstr2);/* load queue name		*/
						}
					}
				}
			} while (*scn_ptr && flag);				/* till a null or flag is clear		*/
#endif	/* VMS */

#if defined(unix) || defined(WIN32)
			if ('\n' == inlin[strlen(inlin)-1])
			{
				inlin[strlen(inlin)-1] = '\0';			/* remove trailing NL			*/
			}

			if ( strlen(inlin) < 4 )				/* 4 = class + space + code + NL 	*/
			{
				werrlog(ERRORCODE(28),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(28));
			}
			if ( strlen(&inlin[2]) > sizeof(prt_ptr->prt_string)-1 )
			{
				werrlog(ERRORCODE(24),lpmap_path,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
			}
			
			prt_ptr->class = inlin[0];				/* Load the class			*/
			strcpy( prt_ptr->prt_string, &inlin[2] );		/* Get lp control string		*/

			wtrace("LPMAP","LOAD","[%c] [%s]",prt_ptr->class, prt_ptr->prt_string);
#endif /* unix */

		}

		fclose(the_file);						/* close the term file			*/
	}
	else
	{
#ifdef VMS
		werrlog(ERRORCODE(20),"load_lpmap",lpmap_path,errno,0,0,0,0,0);
#else
		wtrace("LPMAP","ENTRY", "Unable to open LPMAP file [%s]",lpmap_path);

		/* Create a dummy entry */
		prt_list = (prt_id *)wmalloc(sizeof(prt_id));
		prt_list->next = NULL;
		prt_list->class = ' ';
		prt_list->prt_string[0] = '\0';
#endif
	}
}
#endif /* unix || VMS */
#ifdef VMS
/*
**
**	PQMAP			Procedure Queues
**
*/
pq_id *get_pq_list(void)
{
	if (!pq_list)
	{
		load_pqmap();
	}
	return(pq_list);
}

static void load_pqmap(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256], buff[256];
	pq_id	*pq_ptr;
	char 	*scn_ptr, *prm_ptr;
	int	flag;

	werrlog(ERRORCODE(1),"load_pqmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	pq_list = NULL;								/* initialize the list			*/
	the_file = fopen(pqmap_path,"r");					/* now load the procedure definitions	*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!pq_list)						/* first time?				*/
			{
				pq_list = (pq_id *)wmalloc(sizeof(pq_id));	/* get some memory			*/
				pq_list->next = NULL;				/* set next pointer to zero		*/
				pq_ptr = pq_list;				/* set up local pointer			*/
			}
			else
			{
				pq_ptr->next = (struct pq_id *)wmalloc(sizeof(pq_id));	/* get some memory			*/
				pq_ptr = (pq_id *)pq_ptr->next;			/* set pointer				*/
				pq_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			scn_ptr = &inlin[0];
			flag = 1;						/* switch to parse class, printer	*/
			do							/* scan the line and extract the parms	*/
			{							/* skip over spaces, commas, newlines , tabs	*/
				if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
				{
					scn_ptr++;
				}
				else						/* copy this parm			*/
				{
					if (flag == 1)				/* copy class letter			*/
					{
						flag = 2;			/* set the flag				*/
						pq_ptr->class = *scn_ptr++;	/* get first char			*/
					}
					else if (flag == 2)
					{
						prm_ptr = pq_ptr->qname;	/* load queue name			*/
						do
						{				/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						*prm_ptr = '\0';		/* null terminate			*/
						flag = 0;			/* signal we are done			*/
					}
				}
			} while (*scn_ptr && flag);				/* till a null or flag is clear		*/
		}

		fclose(the_file);						/* close the term file			*/
	}
	else
	{									/* error opening the file		*/
		werrlog(ERRORCODE(20),"load_pqmap",pqmap_path,errno,0,0,0,0,0);
	}
}
#endif	/* VMS */
#ifndef VMS
/*
**
**	LGMAP
**
*/
logical_id *get_logical_list(void)
{
	if (!logical_list)
	{
		load_lgmap();
	}
	return(logical_list);
}

static void get_logical_list_item(void)
{
	if (!logical_list)							/* first time?				*/
	{
		logical_list = (logical_id *)wmalloc(sizeof(logical_id));	/* get some memory			*/
		logical_list->next = NULL;					/* set next pointer to zero		*/
		logical_ptr = logical_list;					/* set up local pointer			*/
	}
	else
	{
		logical_ptr->next = (struct logical_id *)wmalloc(sizeof(logical_id));
		logical_ptr = (logical_id *)logical_ptr->next;			/* set pointer				*/
		logical_ptr->next = NULL;					/* set next pointer to zero		*/
	}
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
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];
	int	config;
	char	*ptr;
	char	*pfirst_field;
	char	*psecond_field;
	int	len;

	int lin_cnt = 0;							/* line counter				*/

	if ( !paths_built ) build_config_paths();
	logical_list = NULL;							/* initialize the list			*/
	config = 0;								/* CONFIG logical not found.		*/
	the_file = fopen(lgmap_path,"r");					/* now load the logicals 		*/

	if (the_file)								/* no error opening file		*/
	{
		wtrace("LGMAP","ENTRY", "Loading LGMAP file [%s]",lgmap_path);

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
				wtrace("LGMAP", "LOAD", "Line %d is too short to contain a valid definition [%s]",
					lin_cnt, inlin);
				continue;
			}

			/*
			 *	Mis-formed definition
			 */
			if (' ' != inlin[SIZEOF_VOL])
			{
				wtrace("LGMAP", "LOAD", "Line %d is mis-formed, position 7 must be a space [%s]",
					lin_cnt, inlin);
				continue;
			}

			if (' ' == inlin[0])
			{
				wtrace("LGMAP", "LOAD", "Line %d is mis-formed, volume can not have leading spaces [%s]",
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
			
			len = strlen(pfirst_field);
			if (  len > SIZEOF_VOL || len < 1 )
			{
				wtrace("LGMAP", "LOAD", "Line %d,"
					"the length [%d] of the first field is invalid",
					lin_cnt, len);
				continue;
			}
			
			/*
			**	Parse the second field (native mapping), and check for acceptable length
			*/
			psecond_field = &inlin[SIZEOF_VOL+1];
			for (; *psecond_field == ' '; psecond_field++) {};	/* ignore leading spaces		*/

			len = strlen(psecond_field);				/* get length				*/
			if ( 0 >= len || MAX_TRANSLATE < len  )			/* bad length?				*/
			{
				wtrace("LGMAP", "LOAD", "Line %d,"
					"the size of the second field is zero or greater than %d",
					lin_cnt, MAX_TRANSLATE);
				continue;
			}

			/*
			**	Populate the link list with VOL and LOGICAL VOL TRANSALATION
			*/
			get_logical_list_item( );				/* Set logical_ptr.			*/
			strcpy(logical_ptr->logical, pfirst_field);		/* place WANG VOL			*/
			strcpy(logical_ptr->translate, psecond_field);		/* place LOGICAL VOLUME TRANSLATION	*/

			if ('$' == logical_ptr->translate[0])
			{
				/*
				**	A leading '$' indicates an environment variable.
				**		VOL100 $WANG_VOL100
				**	This means use the contents of $WANG_VOL100 if set.
				*/
				if (ptr = getenv(&logical_ptr->translate[1]))
				{
					strcpy(logical_ptr->translate,ptr);
				}
			}
			upper_string( logical_ptr->logical );

			wtrace("LGMAP", "LOAD", "Define volume [%-6s] = [%s]",
				logical_ptr->logical,
				logical_ptr->translate);


			if ( strcmp( logical_ptr->logical, "CONFIG" ) == 0 ) config = 1; /* CONFIG logical found.		*/

		}

		fclose(the_file);						/* close the logical file		*/
	}
	else
	{									/* error opening the file		*/
		werrlog(ERRORCODE(20),"load_lgmap",lgmap_path,errno,0,0,0,0,0);
	}

	if ( ! config )								/* ADD 'CONFIG' to list of volumes.	*/
	{
		get_logical_list_item( );					/* Set logical_ptr.			*/
		strcpy( logical_ptr->logical, "CONFIG" );			/* VOLUME 'CONFIG'			*/
		strcpy( logical_ptr->translate, wispconfigdir() );		/* TRANSLATE $WISPCONFIG		*/
	}
										/* ADD '.' to list of volumes. 		*/
	get_logical_list_item( );						/* Set logical_ptr.			*/
	strcpy( logical_ptr->logical, "." );					/* VOLUME '.'				*/
	strcpy( logical_ptr->translate, "." );					/* TRANSLATES '.'			*/
}
#endif /* !VMS */

#ifdef unix
/*
**
**	SCMAP		Submit Class
**
*/
int getscmapnice(char jobclass, int *nice_value)
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

	werrlog(ERRORCODE(1),"load_scmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	scmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(scmap_path,"r");					/* now load the scmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!scmap_list)					/* first time?				*/
			{
				scmap_list = (scmap_id *)wmalloc(sizeof(scmap_id));	/* get some memory			*/
				scmap_list->next = NULL;			/* set next pointer to zero		*/
				scmap_ptr = scmap_list;				/* set up local pointer			*/
			}
			else
			{
				scmap_ptr->next = (struct scmap_id *)wmalloc(sizeof(scmap_id));	/* get some memory	*/
				scmap_ptr = (scmap_id *)scmap_ptr->next;	/* set pointer				*/
				scmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(ERRORCODE(24),WISP_SUBCLASS_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
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
int getcqmap(char jobclass, char *queue_value)
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

	werrlog(ERRORCODE(1),"load_cqmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	cqmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(cqmap_path,"r");					/* now load the cqmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!cqmap_list)					/* first time?				*/
			{
				cqmap_list = (cqmap_id *)wmalloc(sizeof(cqmap_id));	/* get some memory			*/
				cqmap_list->next = NULL;			/* set next pointer to zero		*/
				cqmap_ptr = cqmap_list;				/* set up local pointer			*/
			}
			else
			{
				cqmap_ptr->next = (struct cqmap_id *)wmalloc(sizeof(cqmap_id));	/* get some memory	*/
				cqmap_ptr = (cqmap_id *)cqmap_ptr->next;	/* set pointer				*/
				cqmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(ERRORCODE(24),WISP_QUEUECLASS_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			cqmap_ptr->class = toupper(inlin[0]);			/* get the class			*/
			cqmap_ptr->queue = wstrdup(&inlin[2]);			/* dupe the queue name 			*/
		}

		fclose(the_file);						/* close the cqmap file			*/
	}
}
#endif
#if defined(unix) || defined(WIN32)
/*
**
**	FORMS
**
*/
static void load_forms(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];

	if ( !paths_built ) build_config_paths();

	forms_list = NULL;							/* initialize the list				*/
	the_file = fopen(forms_path,"r");					/* now load the forms	 			*/

	if (the_file)								/* no error opening file			*/
	{
		wtrace("FORMS","ENTRY", "Loading FORMS file [%s]",forms_path);
		
		while (fgets(inlin,sizeof(inlin)-1,the_file))
		{
			if (!forms_list)					/* first time?					*/
			{
				forms_list = (forms_id *)wmalloc(sizeof(forms_id));
				forms_list->next = NULL;			/* set next pointer to zero		*/
				forms_ptr = forms_list;				/* set up local pointer			*/
			}
			else
			{
				forms_ptr->next = (struct forms_id *)wmalloc(sizeof(forms_id));	/* get some memory	*/
				forms_ptr = (forms_id *)forms_ptr->next;	/* set pointer				*/
				forms_ptr->next = NULL;				/* set next pointer to zero		*/
			}

			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			inlin[3] = '\0';					/* null term after form#		*/
			forms_ptr->form_num = atoi(inlin);			/* convert formnum to int		*/
			forms_ptr->form_string = wstrdup( &inlin[4] );		/* Get form control string		*/

			wtrace("FORMS","LOAD","[%d] [%s]",forms_ptr->form_num, forms_ptr->form_string);
		}

		fclose(the_file);						/* close the forms file			*/
	}
	else
	{
		wtrace("FORMS","ENTRY", "Unable to open FORMS file [%s]",forms_path);

		/* Create a dummy forms_list entry */
		forms_list = (forms_id *)wmalloc(sizeof(forms_id));
		forms_list->next = NULL;
		forms_list->form_num = 0;
		forms_list->form_string = wstrdup("");
	}
	
}
#endif /* unix */
#if defined(unix) || defined(WIN32)
/*
**
**	PRMAP 	Printer number map
**
*/
static void load_prmap(void)
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];

	if ( !paths_built ) build_config_paths();
	prmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(prmap_path,"r");					/* now load the prmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		wtrace("PRMAP","ENTRY", "Loading PRMAP file [%s]",prmap_path);

		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (inlin[0]=='\n')
			{
				continue;
			}

			if (!prmap_list)					/* first time?				*/
			{
				prmap_list = (prmap_id *)wmalloc(sizeof(prmap_id));	/* get some memory			*/
				prmap_list->next = NULL;			/* set next pointer to zero		*/
				prmap_ptr = prmap_list;				/* set up local pointer			*/
			}
			else
			{
				prmap_ptr->next = (struct prmap_id *)wmalloc(sizeof(prmap_id));	/* get some memory	*/
				prmap_ptr = (prmap_id *)prmap_ptr->next;	/* set pointer				*/
				prmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(ERRORCODE(24),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
			}
			if ( (int)strlen(inlin) < 6 )				/* 6 = 3 digit prt + space + code + NL	*/
			{
				werrlog(ERRORCODE(28),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(28));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			inlin[3] = '\0';					/* null term after printer #		*/
			prmap_ptr->prmap_num = atoi(inlin);			/* convert printer # to int		*/
			if (prmap_ptr->prmap_num == 0)
			{
				werrlog(ERRORCODE(30),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(30));
			}
			strcpy( prmap_ptr->prmap_string, &inlin[4] );		/* Get printer control string		*/
			
			wtrace("PRMAP","LOAD","[%d] [%s]",prmap_ptr->prmap_num, prmap_ptr->prmap_string);
		}

		fclose(the_file);						/* close the prmap file			*/
	}
	else
	{
		wtrace("PRMAP","ENTRY", "Unable to open PRMAP file [%s]",prmap_path);

		/* Create a dummy forms_list entry */
		prmap_list = (prmap_id *)wmalloc(sizeof(prmap_id));
		prmap_list->next = NULL;
		prmap_list->prmap_num = 0;
		prmap_list->prmap_string[0] ='\0';
	}
	
}
#endif /* unix */

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

	if (!first) return;
	first=0;
	werrlog(ERRORCODE(1),"load_dispfac",0,0,0,0,0,0,0);

	if ( !paths_built ) build_config_paths();

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
					werrlog(ERRORCODE(26),WISP_DISPFAC_FILE,ndx,0,0,0,0,0,0);
					continue;
				}
				ptr = inlin + 3;
				cnt = sscanf(ptr,"%d %x",
					&dispfac_array[ndx].fac_font, &t_dchar);
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(26),WISP_DISPFAC_FILE,ndx,0,0,0,0,0,0);
					continue;
				}
				dispfac_array[ndx].fac_dchar = t_dchar;
			}
		}
		fclose(the_file);
	}
	else
	{										/* error opening the file		*/
		werrlog(ERRORCODE(20),"load_dispfac",dispfac_path,errno,0,0,0,0,0);
	}
}

/*
**	Routine:	get_dispfac_char()
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
int get_dispfac_char(unsigned char c_hex, unsigned char *facchar, int *font)		/* Get the char to display for FAC.	*/
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

int softlink(void)
{
	return do_soft_link;
}

/*
**	Macro		Set			Get
**	-----------	------------		----------
**	%BATCHCMD%	setbatchcmd()		batchcmd()
**			setbatchcmd95()		batchcmd95()
**	%BATCHLOGVOL%	setbatchlogvol()	batchlogvol()
**	%BATCHLOGLIB%	setbatchloglib()	batchloglib()
**	%BATCHPASS%	setbatchpass()		batchpass()
**	%BATCHUSER%	setbatchuser()		batchuser()
**	%BATCHSERVER%	setbatchserver()       	batchserver()
**	%SUBSTAT%	setbatchhold()		batchhold()
**	%SUBSTAT%	setbatchrun()		batchrun()
*/
static char* the_batchcmd   = NULL;
static char* the_batchcmd95 = NULL;

void setbatchcmd(const char* command)
{
	if (the_batchcmd) 
	{
		free(the_batchcmd);
	}
	
	the_batchcmd = wstrdup(command);
}
const char* batchcmd(void)
{
	return the_batchcmd;
}

void setbatchcmd95(const char* command)
{
	if (the_batchcmd95) 
	{
		free(the_batchcmd95);
	}
	
	the_batchcmd95 = wstrdup(command);
}
const char* batchcmd95(void)
{
	return the_batchcmd95;
}

static char* the_batchlogvol = NULL;
static char* the_batchloglib = NULL;

void setbatchlogvol(const char* value)
{
	if (the_batchlogvol) 
	{
		free(the_batchlogvol);
	}
	
	the_batchlogvol = wstrdup(value);
}
const char* batchlogvol(void)
{
	if (!the_batchlogvol)
	{
		char	s_vol[6+1];
		
		/* If not set then default to SPOOLVOL */
		get_defs(DEFAULTS_SV,s_vol);
		s_vol[6] = '\0';
		setbatchlogvol(s_vol);
	}

	return the_batchlogvol;
}

void setbatchloglib(const char* value)
{
	if (the_batchloglib) 
	{
		free(the_batchloglib);
	}
	
	the_batchloglib = wstrdup(value);
}
const char* batchloglib(void)
{
	if (!the_batchloglib)
	{
		/* If not set then default to SUBMIT */
		setbatchloglib("SUBMIT  ");
	}

	return the_batchloglib;
}

static char* the_batchpass = NULL;
static char* the_batchuser = NULL;

void setbatchpass(const char* value)
{
	if (the_batchpass) 
	{
		free(the_batchpass);
	}
	
	the_batchpass = wstrdup(value);
}
const char* batchpass(void)
{
	return the_batchpass;
}

void setbatchuser(const char* value)
{
	if (the_batchuser) 
	{
		free(the_batchuser);
	}
	
	the_batchuser = wstrdup(value);
}
const char* batchuser(void)
{
	if (!the_batchuser)
	{
		setbatchuser(longuid());
	}
	
	return the_batchuser;
}

static char* the_batchserver = NULL;

void setbatchserver(const char* value)
{
	if (the_batchserver) 
	{
		free(the_batchserver);
	}
	
	the_batchserver = wstrdup(value);
}
const char* batchserver(void)
{
	if (!the_batchserver)
	{
		setbatchserver(wispserver());
	}
	
	return the_batchserver;
}


static char* the_batchhold = NULL;
static char* the_batchrun  = NULL;

void setbatchhold(const char* value)
{
	if (the_batchhold) 
	{
		free(the_batchhold);
	}
	
	the_batchhold = wstrdup(value);
}
const char* batchhold(void)
{
	return the_batchhold;
}
void setbatchrun(const char* value)
{
	if (the_batchrun) 
	{
		free(the_batchrun);
	}
	
	the_batchrun = wstrdup(value);
}
const char* batchrun(void)
{
	return the_batchrun;
}

/*
**	The nativescreens() flag tells the runtime that COBOL native screen IO 
**	is being used so video must share the screen.
**
**	nativescreens() returns true if the NATIVESCREENS option is set and
**	it is called from an Acucobol runtime. (Later may add Micro Focus)
**
**	If in background then use WISP screens instead of native screens
**	because WISP screens knows how to deal with screen IO in background.
*/
int nativescreens(void)
{
	static int flag = -1;
	
	if (-1 == flag ) /* first time only */
	{
		flag = 0;
		if (!wbackground() && get_wisp_option("NATIVESCREENS"))
		{
			if (acu_cobol)
			{
				wtrace("OPTIONS","NATIVESCREENS","Using ACUCOBOL native screens");

				flag = 1;
				set_vsharedscreen_true();
			}
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
int pfkeys12(void)
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
				werrlog(ERRORCODE(19),"Duplicate keyword",keyword,0,0,0,0,0,0);
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
		
		next_option->next = wmalloc(sizeof(struct s_option));
		next_option = next_option->next;
	}
	else
	{
		top_option = wmalloc(sizeof(struct s_option));
		next_option = top_option;
	}

	next_option->next = (struct s_option *)NULL;
	next_option->keyword = wstrdup(keyword);
	next_option->trailing = wstrdup(trailing);

	wtrace("OPTIONS","ADD","%s %s", next_option->keyword, next_option->trailing);
}

/*
**	ROUTINE:	get_wisp_option()
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
const char *get_wisp_option(const char *keyword)
{
	struct s_option *next_option;

	ASSERT(keyword);
	
	load_options();

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
**	History:
**	$Log: wperson.c,v $
**	Revision 1.51  1999-09-15 09:18:56-04  gsl
**	Enhance load_options() to support backslash continued long-lines.
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
**	Change the nativescreens() routine to return FALSE if in background.
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
**	Add get_wisp_option() and add_option() to support easier adding
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
**	Add wtrace() to report thr path of the personality files
**
**	Revision 1.32  1997-04-03 16:45:34-05  gsl
**	Removed the entry werrlog() for wpload
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
**	Wrote WIN32 version of the ttyid5() routine
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
 * For get_defs() and set_defs() use a void *.
 *
 * Revision 1.10  1995/03/20  13:18:43  gsl
 * prototyped everything
 *
 * Revision 1.9  1995/03/20  11:16:14  gsl
 * added routine build_wisp_config_path() which is used to build a path
 * to a file in the wisp config directory.
 *
 * Revision 1.8  1995/03/10  11:15:39  gsl
 * removed video refs and reorged headers
 *
**
**
*/
