			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* 		These routines are used for access to the various personality files for the WISP run-time library		*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>

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
#include <malloc.h>
#endif /* !VMS */

#include <v/video.h>

#include "idsistd.h"
#include "wperson.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wglobals.h"
#include "wanguid.h"

#ifdef MSDOS
#define READ_BINARY	"rb"
#define WRITE_BINARY	"wb"
#else /* !MSDOS */
#define READ_BINARY	"r"
#define WRITE_BINARY	"w"
#endif /* !MSDOS */

char *getenv();
static int read_from_file();
static int write_to_file();
static int load_defaults_temp();
static int save_defaults_temp();
static int loadpadnull();
static int getprogvol();
static int getproglib();
static int setprogvol();
static int setproglib();
static int load_lpmap();
static int load_lgmap();
static int load_scmap();
static int load_forms();
static int load_prmap();
static int build_config_paths();
static int validate_defaults();
static int genworklib();
static int genworkvol();

#define		ROUTINE		83000

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

static usr_defaults defaults;							/* the actual defaults record		*/
static defaults_loaded = 0;							/* Have defaults been loaded		*/

typedef struct	{
			struct forms_id *next;					/* pointer to the next one		*/
			int	form_num;					/* the form number			*/
			char	form_string[80];				/* the string to  insert into the lp cmd*/
		} forms_id;
static forms_id 	*forms_list = NULL;					/* this is the actual list		*/

typedef struct	{
			struct  prmap_id *next;					/* pointer to the next one		*/
			int	prmap_num;					/* the Printer number			*/
			char	prmap_string[80];				/* the string to  insert into the lp cmd*/
		} prmap_id;
static prmap_id 	*prmap_list = NULL;					/* this is the actual list		*/

typedef struct	{
			struct scmap_id *next;					/* pointer to the next one		*/
			int	nice;						/* The NICE value			*/
			char 	class;						/* the job class			*/
		} scmap_id;
static scmap_id 	*scmap_list = NULL;					/* this is the actual list		*/

#ifdef VMS
static pq_id		*pq_list = NULL;
static lat_id 		*lat_list = NULL;
static term_id 		*term_list = NULL;
#endif

static prt_id		*prt_list = NULL;

#ifndef VMS
static logical_id 	*logical_list = NULL;
#endif

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
static char forms_path[80];
static char prmap_path[80];
static char options_path[80];
char language_path[80];								/* the path of the IVS translation file         */

int opt_max_pages = 0;								/* Max prb pages				*/
int opt_max_parms = 0;								/* Max prb parms				*/

#ifndef VMS
static logical_id	*logical_ptr;
static scmap_id 	*scmap_ptr;
static forms_id 	*forms_ptr;
static prmap_id 	*prmap_ptr;
#endif /* !VMS */

#ifdef VMS
#define LIB$K_CLI_GLOBAL_SYM	2
static char	constr[sizeof(usr_defaults)+1];					/* A string to hold the usage constants.	*/
static $DESCRIPTOR(sym,"$W_USAGE_CONSTANTS");					/* The descriptor of the usage constant symbol.	*/
static $DESCRIPTOR(conres,constr);						/* A descriptor to hold the constant string.	*/
#endif

/*==============================================================================================================================*/
/*
**	wpload	This routine used to be responsible for loading all the device info lists
**		as well as the defaults structure and any other config files.
**		This is no longer required because each of these things will be loaded
**		dynamically if and only if they are used.
**		Currently only load_options() is done.
*/
void wpload()
{
	static int loaded = 0;							/* flag to indicate info is loaded		*/

	werrlog(ERRORCODE(1),"wpload",0,0,0,0,0,0,0);
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
int save_defaults()
{
	if (defaults_loaded)
	{
		save_defaults_temp(&defaults);					/* Save defaults in temp area			*/
	}
	return(0);
}

int load_defaults()
{
	if ( !load_defaults_temp(&defaults) )					/* First try the temp defaults			*/
	{
		read_defaults_from_file("");					/* Otherwise, read the file.			*/
	}
	defaults_loaded = 1;
	return(0);
}

int write_defaults_to_file(file)
char	*file;
{
	if (!defaults_loaded)
	{
		load_defaults();						/* First ensure defaults are loaded		*/
	}
	return write_to_file(&defaults,file);					/* write defaults to the file			*/
}

int read_defaults_from_file(file)
char	*file;
{
	return read_from_file(&defaults,file);
}

static int read_from_file(the_def,the_name)					/* Load the personality from the file.	*/
usr_defaults *the_def;
char *the_name;
{
	FILE *the_file;
	int amt, retfl;								/* Set flag if use defaults.		*/

	werrlog(ERRORCODE(1),"read_from_file",0,0,0,0,0,0,0);

	defaults_loaded = 1;							/* This routine will always succeed	*/

	if ( ! paths_built ) build_config_paths();
	retfl = 0;								/* Initialize flag.			*/
	if (the_name && *the_name)
	{
#ifdef VMS
		the_file = fopen(the_name,"r", "dna=*.dat;0");			/* Open the requested one.		*/
#else
		the_file = fopen(the_name, READ_BINARY);			/* Open the requested one.		*/
#endif
	}
	else 
	{
		the_file = fopen(person_path,READ_BINARY);			/* try to load the local user's default	*/
	}

	if (!the_file)								/* not found, go for the system file	*/
	{
		the_file = fopen(system_person_path,READ_BINARY);		/* try to open it			*/
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
static int write_to_file(the_def,the_name)
usr_defaults *the_def;								/* NOTE the structure MUST be properly	*/
char *the_name;									/* loaded or there will be a problem!	*/
{
	FILE 	*the_file;
	int 	amt;
	char	*fptr;

	werrlog(ERRORCODE(1),"write_to_file",0,0,0,0,0,0,0);

	if ( ! paths_built ) build_config_paths();

	if (*the_name) fptr = the_name;						/* Use the supplied name.		*/
	else           fptr = person_path;					/* Use the default.			*/

#ifdef VMS
	the_file = fopen(fptr,"w","dna=*.dat;0");				/* Open the file.			*/
#else
	the_file = fopen(fptr, WRITE_BINARY);					/* Open the file.			*/
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

static load_defaults_temp(the_def)						/* Read defaults from temp area			*/
usr_defaults *the_def;
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
	if ( fp = fopen( temp_person_path, READ_BINARY ) )
	{
		size = fread( (char *)the_def, sizeof( *the_def ), 1, fp );
		fclose( fp );
	}
	if ( !size && wbackground() )						/* If temp not found && background		*/
	{
		if ( fp = fopen( parent_path, READ_BINARY ) )			/* then  copy from parent			*/
		{
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
static save_defaults_temp(the_def)						/* Store the usage constants in the sym	*/
usr_defaults *the_def;
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

#ifndef VMS
static int save_defaults_temp( the_def )					/* unix equv to LIB$SET_SYMBOL.			*/
usr_defaults *the_def;								/* Write the PERSONALITY## file from memory.	*/
{
	FILE	*fp;
	int	size;

	if ( ! paths_built ) build_config_paths();

	fp = fopen( temp_person_path, WRITE_BINARY );				/* Open/Create the temp personality		*/

	if (!fp)								/* If Open fails then				*/
	{
		makepath(temp_person_path);					/* ensure path exists				*/
		fp = fopen( temp_person_path, WRITE_BINARY );			/* try the open again.				*/
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

#ifdef unix
char *wforms(num)								/* return the string for form#		*/
int	num;
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

char *getprmap(num)								/* return the string for printer #	*/
int	num;
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

char *wlpclass(lpclass)								/* return the string for lpclass#	*/
char	lpclass;
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

int ttyid5(tty)
char	*tty;
{
#ifdef MSDOS
	strcpy( tty, "conso" );
#endif

#ifdef unix
	char	*ptr;
	char	*path;
	struct stat buf;
	int	i;

	if ( wbackground() )
	{
		if ( ptr = getenv( WISP_TTY_ENV ) )
		{
			strcpy( tty, ptr );
			return(0);
		}
	}


	path = ttyname(0);
	if ( path )
	{
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
	}
	else
	{
		strcpy( tty, "ERROR" );
	}
#endif	/* #ifdef unix */
	return(0);
}

static int build_config_paths()
{
/*
					For the following paths, assume "\\" for "/" and "C:\\TMP" for "/usr/tmp" on MSDOS:

	person_path			$HOME/$WISP_USER_PERSON_FILE
	temp_person_path		/usr/tmp/$WISP_TEMP_PERSON_PREFIX$TTY$UID		(forground unix)
					/usr/tmp/$WISP_TEMP_PERSON_PREFIX$UID$W_T_P_SUFFIX	(forground unix)
					/usr/tmp/$WISP_TEMP_PERSUB_PREFIX$WISP_PID_ENV		(background unix only)
	parent_path			/usr/tmp/$WISP_TEMP_PERSON_PREFIX$TTY$UID		(forground  unix only)
					/usr/tmp/$WISP_TEMP_PERSON_PREFIX$WISP_TTY_ENV$UID	(background unix only)
	system_person_path		$WISPCONFIG/$WISP_SYSTEM_PERSON_FILE
	forms_path			$WISPCONFIG/$WISP_FORMS_FILE
*/

	char	*ptr;
	char	temp[20];
	char	tty[10];
	char	uid[4];
	int4	pid;

	if ( paths_built ) return(0);

	werrlog(ERRORCODE(1),"build_config_paths",0,0,0,0,0,0,0);

	*person_path = '\0';
	*parent_path = '\0';
	*temp_person_path = '\0';
	*system_person_path = '\0';
	*ttmap_path = '\0';
	*lpmap_path = '\0';
	*pqmap_path = '\0';
	*scmap_path = '\0';
	*lgmap_path = '\0';
	*forms_path = '\0';
	*prmap_path = '\0';
	*options_path = '\0';

	if ( !(ptr = getenv( WISP_CONFIG_ENV )) )
	{
		/*
		**	WISPCONFIG was not set so use a dummy value to generate the names.
		**	This will cause an error when a config file is trying to open, but if not
		**	used then no error message needed.
		*/
		ptr = "$WISPCONFIG";
	}
	strcpy( system_person_path, ptr );
	strcat( system_person_path, DSS );

	if ( !(ptr = getenv( WISP_HOME_ENV )) )
	{
		/*
		**	HOME was not set so on unix use a dummy value. 
		**	On MSDOS then defaults to "C:".
		*/
#ifdef unix
		ptr = "$HOME";
#endif
#ifdef MSDOS
		ptr = "C:";
#endif
	}
	strcpy( person_path, ptr );
	strcat( person_path, DSS );

	strcat( person_path, WISP_USER_PERSON_FILE );

	strcpy( parent_path, TMP_DIR );
	strcat( parent_path, DSS );
	strcat( parent_path, WISP_TEMP_PERSON_PREFIX );

	memcpy(uid,wanguid3(),3);
	if(	 uid[0] == ' ' )	uid[0] = '\0';
	else if( uid[1] == ' ' )	uid[1] = '\0';
	else if( uid[2] == ' ' )	uid[2] = '\0';
	else				uid[3] = '\0';

#ifdef MSDOS
	strcpy( temp_person_path, parent_path );
	strcat( temp_person_path, uid );
	strcat( temp_person_path, WISP_TEMP_PERSON_SUFFIX );
#endif

#ifdef unix
	ttyid5(tty);

	strcat( parent_path, tty );
	strcat( parent_path, uid );

	strcpy( temp_person_path, "/usr/tmp/" );

	if (wbackground())
	{
		strcat( temp_person_path, WISP_TEMP_PERSUB_PREFIX );
		if ( ptr = getenv( WISP_PID_ENV ) )
		{
			strcat( temp_person_path, ptr );
		}
		else /* WISP_PID_ENV not set */
		{
			pid = (int4)getpid();
			sprintf(temp,"%06d",pid);
			strcat( temp_person_path, temp );
		}
	}
	else /* foreground */
	{
		strcat( temp_person_path, WISP_TEMP_PERSON_PREFIX );
		strcat( temp_person_path, tty );
		strcat( temp_person_path, uid );
	}
#endif /* unix */
	
	strcpy( ttmap_path, system_person_path );
	strcat( ttmap_path, WISP_TERMINAL_FILE );
	strcpy( lpmap_path, system_person_path );
	strcat( lpmap_path, WISP_PRINTER_FILE );
	strcpy( pqmap_path, system_person_path );
	strcat( pqmap_path, WISP_PROC_FILE );
	strcpy( scmap_path, system_person_path );
	strcat( scmap_path, WISP_SUBCLASS_FILE );
	strcpy( lgmap_path, system_person_path );
	strcat( lgmap_path, WISP_LOGICAL_FILE );
	strcpy( forms_path, system_person_path );
	strcat( forms_path, WISP_FORMS_FILE );
	strcpy( prmap_path, system_person_path );
	strcat( prmap_path, WISP_PRMAP_FILE );
	strcpy( options_path, system_person_path );
	strcat( options_path, WISP_OPTIONS_FILE );
	strcat( system_person_path, WISP_SYSTEM_PERSON_FILE );
	paths_built = 1;
	return(0);
}
#endif	/* unix  && MSDOS */

#ifdef VMS
static int build_config_paths()
{
	if (paths_built) return(0);
	werrlog(ERRORCODE(1),"build_config_paths",0,0,0,0,0,0,0);

	strcpy( person_path, WISP_USER_PERSON_FILE );
	strcpy( parent_path, WISP_USER_PERSON_FILE );
	strcpy( system_person_path, WISP_SYSTEM_PERSON_FILE );
	strcpy( ttmap_path, WISP_TERMINAL_FILE );
	strcpy( lpmap_path, WISP_PRINTER_FILE );
	strcpy( pqmap_path, WISP_PROC_FILE );
	strcpy( options_path, WISP_OPTIONS_FILE );

	paths_built = 1;
	return(0);
}
#endif	/* VMS */

/* Validate the structure, and set up any initial values which aren't in the old structure versions.				*/

static validate_defaults(the_def)
usr_defaults *the_def;
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

#ifdef MSDOS
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


static genworklib(worklib)
char	*worklib;
{
	char temp[20];
	int4  pid;								/* Process id				*/
	char	*ptr;

#ifdef VMS
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
	sprintf(temp,"%08x", pid);						/* Format the PID as a hex number	*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
	memcpy(worklib, "WK", 2);						/* Overlay the first to chars as "WK"	*/
#endif	/* VMS */

#ifdef unix
	if ( wbackground() )
	{
		if ( ptr = getenv( WISP_PID_ENV ) )
		{
			strcpy(temp,ptr);
			pid = atol(temp);
		}
		else
		{
			pid = getpid();
		}
	}
	else
	{
		pid = wgetpgrp();
	}
	sprintf(temp,"%08d", pid);						/* Format the PID			*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
	memcpy(worklib, "WK", 2);						/* Overlay the first to chars as "WK"	*/
#endif	/* unix */

#ifdef MSDOS
	sprintf(temp,"WORK%s    ", longuid());					/* Format the worklib			*/
	memcpy(worklib,temp,8);							/* copy it into worklib			*/
#endif	/* MSDOS */


	upper_mem(worklib,8);							/* Make uppercase			*/
}

static int genworkvol(workvol)
char	*workvol;
{

	if ( memcmp(workvol,"      ",6) != 0 ) return(0);
	memcpy(workvol, "WRKVOL", 6 );
	return(1);
}

static int genspoollib(spoollib)
char	*spoollib;
{
	char *ptr;

	if ( memcmp(spoollib,"        ",8) != 0 ) return(0);
	spoollib[0] = '#';
	ptr = wanguid3();
	spoollib[1] = ptr[0];
	spoollib[2] = ptr[1];
	spoollib[3] = ptr[2];
	if ( spoollib[2] == ' ' ) memcpy( &spoollib[2], "PRT", 3);
	else if ( spoollib[3] == ' ' ) memcpy( &spoollib[3], "PRT", 3);
	else memcpy( &spoollib[4], "PRT", 3);
	return(1);
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
				MAXPRBPARMS {num}	(default 128)
				MAXPRBPAGES {num}	(default 40)
				HELPSTYLE1		(default Wang style)
				HELPSTYLE2		(Non-Wang style)
				IDALPHA			(default)
				IDNUMERIC
				IDONE			(default return char 1-3 of user id)
				IDFIVE			(return char 5-7 of user id)
*/
int load_options()								/* Load the runtime OPTIONS file.		*/
{
static	int	first=1;
	FILE 	*the_file;
	char	inlin[132], keyword[80], value[80];
	int	cnt,i;
	int	len;

	if (!first) return(0);
	first=0;
	werrlog(ERRORCODE(1),"load_options",0,0,0,0,0,0,0);

	if ( !paths_built ) build_config_paths();

	the_file = fopen(options_path,"r");
	if (the_file)
	{
		while(fgets(inlin,132,the_file))
		{
			len=strlen(inlin);
			if (len>0 && inlin[len-1] == '\n') inlin[len-1] = '\0';	/* Null out the newline char		*/
			cnt = sscanf(inlin,"%s %s",keyword,value);
			if ( cnt < 1 ) continue;
			if (keyword[0] == '#') continue;
			upper_string(keyword);

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
			else if (strcmp(keyword,"IDSIPRINTON") == 0)		/* UNIX use the IDSI print spooler		*/
			{
				opt_idsiprint = 1;
			}
			else if (strcmp(keyword,"IDSIPRINTOFF") == 0)		/* UNIX use lp, not the IDSI print spooler	*/
			{
				opt_idsiprint = 0;
			}
			else if (strcmp(keyword,"PQILP") == 0)			/* UNIX use ILP					*/
			{
				opt_idsiprint = 1;
				opt_pqilp = 1;
				opt_pqunique = 0;
			}
			else if (strcmp(keyword,"PQUNIQUE") == 0)		/* UNIX use UNIQUE				*/
			{
				opt_idsiprint = 1;
				opt_pqilp = 0;
				opt_pqunique = 1;
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
				char *getenv();
				
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inlin,0,0,0,0,0,0);
					continue;
				}
#ifndef VMS
				buildfilepath(language_path,getenv(WISP_CONFIG_ENV),value);
				if ( !fexists(language_path) )
				{
					werrlog(ERRORCODE(18),"Invalid Language File",inlin,0,0,0,0,0,0);
					continue;
				}
#endif /* !VMS */
			}
			else
			{
				werrlog(ERRORCODE(18),"Unknown keyword",inlin,0,0,0,0,0,0);
			}
		}
		fclose(the_file);
	}
	return(0);
}

/*
	get_defs	Get a default based on the code.
*/
int get_defs(code,ptr)
int	code;
char	*ptr;
{
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
int set_defs(code,ptr)
int	code;
char	*ptr;
{
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
static loadpadnull(dest,src,size)
char *dest;
char *src;
int  size;
{
	loadpad(dest,src,size);
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

static getprogvol(result)
char 	result[6];
{
	char	*ptr;
	int	len;

	if (curr_progvol[0] == ' ')						/* If in memory save is not set check symbol	*/
	{
		if (defaults.progvol[0] != ' ')					/* If symbol is set then use it			*/
		{
			memcpy(curr_progvol,defaults.progvol,6);		/* Copy symbol in in memory save area		*/
		}
#ifdef unix
		else if (ptr = getenv(SHELL_PROGVOL))				/* Check the shell variable			*/
		{
			if (*ptr != ' ')					/* if shell var exists and not null then use it	*/
			{
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
static getproglib(result)
char 	result[8];
{
	char	*ptr;
	int	len;

	if (curr_proglib[0] == ' ')						/* If in memory save is not set check symbol	*/
	{
		if (defaults.proglib[0] != ' ')					/* If symbol is set then use it			*/
		{
			memcpy(curr_proglib,defaults.proglib,8);		/* Copy symbol in in memory save area		*/
		}
#ifdef unix
		else if (ptr = getenv(SHELL_PROGLIB))				/* Check the shell variable			*/
		{
			if (*ptr != ' ')					/* if shell var exists and not null then use it	*/
			{
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
int clearprogsymb()
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
int setprogdefs(progvol,proglib)
char	*progvol;
char	*proglib;
{
	setprogvol(progvol);
	setproglib(proglib);
	return(0);
}
static setprogvol(progvol)
char	*progvol;
{
#ifdef unix
	char	buff[20];
	sprintf(buff,"%s=%s",SHELL_PROGVOL,progvol);
	setenvstr(buff);
#endif /* unix */
}
static setproglib(proglib)
char	*proglib;
{
#ifdef unix
	char	buff[20];
	sprintf(buff,"%s=%s",SHELL_PROGLIB,proglib);
	setenvstr(buff);
#endif /* unix */
}

/*
	saveprogdefs
	restoreprogdefs		These routines save the symbol values or PROGLIB and PROGVOL when a COBOL program
				is started and restore them on exit.
*/
static char	def_progvol[7] = "      ";
static char	def_proglib[9] = "        ";

int saveprogdefs()
{
#ifdef unix
	char	buff[20];

	get_defs(DEFAULTS_PV,buff);						/* Force an update of curr			*/
	get_defs(DEFAULTS_PL,buff);

	memcpy(def_progvol,defaults.progvol,6);					/* Save the entry values			*/
	memcpy(def_proglib,defaults.proglib,8);
#endif
	return(0);
}

int restoreprogdefs()
{
#ifdef unix
	set_defs(DEFAULTS_PV,def_progvol);					/* Restore the entry values			*/
	set_defs(DEFAULTS_PL,def_proglib);
	save_defaults();							/* Write changes to temp area			*/
#endif
	return(0);
}

#ifdef VMS
static alloc_term(term_ptr)
term_id **term_ptr;
{
	if (!term_list)								/* first time?				*/
	{
		term_list = (term_id *)malloc(sizeof(term_id));			/* get some memory			*/
		if (!term_list)
		{
			werrlog(ERRORCODE(6),"term_list",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		term_list->next = NULL;						/* set next pointer to zero		*/
		term_list->termname[0] = (char)0;
		term_list->termnum = 0;
		term_list->flags = 0;
		*term_ptr = term_list;						/* set up local pointer			*/
	}
	else
	{
		(*term_ptr)->next = (struct term_id *)malloc(sizeof(term_id));
		if (!(*term_ptr)->next)
		{
			werrlog(ERRORCODE(6),"term_ptr->next",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		*term_ptr = (term_id *)(*term_ptr)->next;			/* set pointer				*/
		(*term_ptr)->next = NULL;					/* set next pointer to zero		*/
		(*term_ptr)->termname[0] = (char)0;
		(*term_ptr)->termnum = 0;
		(*term_ptr)->flags = 0;
	}
}

static alloc_lat(lat_ptr)
lat_id **lat_ptr;
{
	if (!lat_list)								/* first time?				*/
	{
		lat_list = (lat_id *)malloc(sizeof(lat_id));			/* get some memory			*/
		if (!lat_list)
		{
			werrlog(ERRORCODE(6),"lat_list",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		lat_list->next = NULL;						/* set next pointer to zero		*/
		lat_list->latname[0] = (char)0;
		lat_list->termnum = 0;
		lat_list->flags = 0;
		*lat_ptr = lat_list;						/* set up local pointer			*/
	}
	else
	{
		(*lat_ptr)->next = (struct lat_id *)malloc(sizeof(lat_id));
		if (!(*lat_ptr)->next)
		{
			werrlog(ERRORCODE(6),"lat_ptr->next",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		*lat_ptr = (lat_id *)(*lat_ptr)->next;				/* set pointer				*/
		(*lat_ptr)->next = NULL;					/* set next pointer to zero		*/
		(*lat_ptr)->latname[0] = (char)0;
		(*lat_ptr)->termnum = 0;
		(*lat_ptr)->flags = 0;
	}
}

term_id *get_term_list()
{
	if (!term_list)
	{
		load_ttmap();
	}
	return(term_list);
}

lat_id *get_lat_list()
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
static load_ttmap()
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
#ifndef MSDOS
/*
**
**	LPMAP		printer definitions
**
*/

prt_id *get_prt_list()
{
	if (!prt_list)
	{
		load_lpmap();
	}
	return(prt_list);
}

static load_lpmap()
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256], lptstr1[80], lptstr2[80], buff[256];
	char 	*scn_ptr, *prm_ptr;
	prt_id	*prt_ptr;							/* Pointer to printer list structure		*/
	int	pnum;
	char	*ptr;
	int	flag;

	werrlog(ERRORCODE(1),"load_lpmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	prt_list = NULL;							/* initialize the list			*/
	the_file = fopen(lpmap_path,"r");					/* now load the printer definitions	*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!prt_list)						/* first time?				*/
			{
				prt_list = (prt_id *)malloc(sizeof(prt_id));	/* get some memory			*/
				if (!prt_list)
				{
					werrlog(ERRORCODE(6),"prt_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prt_list->next = NULL;				/* set next pointer to zero		*/
				prt_ptr = prt_list;				/* set up local pointer			*/
			}
			else
			{
				prt_ptr->next = (struct prt_id *)malloc(sizeof(prt_id));/* get some memory			*/
				if (!prt_ptr->next)
				{
					werrlog(ERRORCODE(6),"prt_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
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

#ifndef VMS
			if ( (int)strlen(inlin) > 82 )
			{
				werrlog(ERRORCODE(24),WISP_PRINTER_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			prt_ptr->class = inlin[0];				/* Load the class			*/
			strcpy( prt_ptr->prt_string, &inlin[2] );		/* Get lp control string		*/

#endif /* !VMS */

		}

		fclose(the_file);						/* close the term file			*/
	}
	else
	{									/* error opening the file		*/
#ifdef VMS
		werrlog(ERRORCODE(20),"load_lpmap",lpmap_path,errno,0,0,0,0,0);
#endif
	}
}
#endif /* !MSDOS */
#ifdef VMS
/*
**
**	PQMAP			Procedure Queues
**
*/
pq_id *get_pq_list()
{
	if (!pq_list)
	{
		load_pqmap();
	}
	return(pq_list);
}

static load_pqmap()
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
				pq_list = (pq_id *)malloc(sizeof(pq_id));	/* get some memory			*/
				if (!pq_list)
				{
					werrlog(ERRORCODE(6),"pq_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				pq_list->next = NULL;				/* set next pointer to zero		*/
				pq_ptr = pq_list;				/* set up local pointer			*/
			}
			else
			{
				pq_ptr->next = (struct pq_id *)malloc(sizeof(pq_id));	/* get some memory			*/
				if (!pq_ptr->next)
				{
					werrlog(ERRORCODE(6),"pq_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
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
logical_id *get_logical_list()
{
	if (!logical_list)
	{
		load_lgmap();
	}
	return(logical_list);
}

static	get_logical_list_item( )
{
	if (!logical_list)							/* first time?				*/
	{
		logical_list = (logical_id *)malloc(sizeof(logical_id));	/* get some memory			*/
		if (!logical_list)
		{
			werrlog(ERRORCODE(6),"logical_list",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		logical_list->next = NULL;					/* set next pointer to zero		*/
		logical_ptr = logical_list;					/* set up local pointer			*/
	}
	else
	{
		logical_ptr->next = (struct logical_id *)malloc(sizeof(logical_id));
		if (!logical_ptr->next)
		{
			werrlog(ERRORCODE(6),"logical_ptr->next",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		logical_ptr = (logical_id *)logical_ptr->next;			/* set pointer				*/
		logical_ptr->next = NULL;					/* set next pointer to zero		*/
	}
}

static load_lgmap()
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256], buff[256];
	int	config;
	char	*ptr;

	werrlog(ERRORCODE(1),"load_lgmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	logical_list = NULL;							/* initialize the list			*/
	config = 0;								/* CONFIG logical not found.		*/
	the_file = fopen(lgmap_path,"r");					/* now load the logicals 		*/

	if (the_file)								/* no error opening file		*/
	{
		char	lgmap_scan[40];

		sprintf(lgmap_scan,"%%6s %%%ds",MAX_TRANSLATE);

		while (fgets(inlin,sizeof(inlin),the_file))
		{
			get_logical_list_item( );				/* Set logical_ptr.			*/
			sscanf(inlin, lgmap_scan, logical_ptr->logical, logical_ptr->translate );
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
		if ( ptr = getenv( WISP_CONFIG_ENV ) )
		{
			get_logical_list_item( );				/* Set logical_ptr.			*/
			strcpy( logical_ptr->logical, "CONFIG" );		/* VOLUME 'CONFIG'			*/
			strcpy( logical_ptr->translate, ptr );			/* TRANSLATE $WISPCONFIG		*/
		}
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
int getscmapnice(jobclass,nice_value)
char 	jobclass;
int	*nice_value;
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

static load_scmap()
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
				scmap_list = (scmap_id *)malloc(sizeof(scmap_id));	/* get some memory			*/
				if (!scmap_list)
				{
					werrlog(ERRORCODE(6),"scmap_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				scmap_list->next = NULL;			/* set next pointer to zero		*/
				scmap_ptr = scmap_list;				/* set up local pointer			*/
			}
			else
			{
				scmap_ptr->next = (struct scmap_id *)malloc(sizeof(scmap_id));	/* get some memory	*/
				if (!scmap_ptr->next)
				{
					werrlog(ERRORCODE(6),"scmap_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
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
#ifdef unix
/*
**
**	FORMS
**
*/
static load_forms()
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];

	werrlog(ERRORCODE(1),"load_forms",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	forms_list = NULL;							/* initialize the list				*/
	the_file = fopen(forms_path,"r");					/* now load the forms	 			*/

	if (the_file)								/* no error opening file			*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!forms_list)					/* first time?					*/
			{
				forms_list = (forms_id *)malloc(sizeof(forms_id));
				if (!forms_list)
				{
					werrlog(ERRORCODE(6),"forms_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				forms_list->next = NULL;			/* set next pointer to zero		*/
				forms_ptr = forms_list;				/* set up local pointer			*/
			}
			else
			{
				forms_ptr->next = (struct forms_id *)malloc(sizeof(forms_id));	/* get some memory	*/
				if (!forms_ptr->next)
				{
					werrlog(ERRORCODE(6),"forms_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				forms_ptr = (forms_id *)forms_ptr->next;	/* set pointer				*/
				forms_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(ERRORCODE(24),WISP_FORMS_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			inlin[3] = '\0';					/* null term after form#		*/
			forms_ptr->form_num = atoi(inlin);			/* convert formnum to int		*/
			strcpy( forms_ptr->form_string, &inlin[4] );		/* Get form control string		*/
		}

		fclose(the_file);						/* close the forms file			*/
	}
}
#endif /* unix */
#ifdef unix
/*
**
**	PRMAP 	Printer number map
**
*/
static load_prmap()
{
	FILE 	*the_file;							/* a file pointer				*/
	char	inlin[256];

	werrlog(ERRORCODE(1),"load_prmap",0,0,0,0,0,0,0);
	if ( !paths_built ) build_config_paths();
	prmap_list = NULL;							/* initialize the list			*/
	the_file = fopen(prmap_path,"r");					/* now load the prmap	 		*/

	if (the_file)								/* no error opening file		*/
	{
		while (fgets(inlin,sizeof(inlin),the_file))
		{
			if (!prmap_list)					/* first time?				*/
			{
				prmap_list = (prmap_id *)malloc(sizeof(prmap_id));	/* get some memory			*/
				if (!prmap_list)
				{
					werrlog(ERRORCODE(6),"prmap_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prmap_list->next = NULL;			/* set next pointer to zero		*/
				prmap_ptr = prmap_list;				/* set up local pointer			*/
			}
			else
			{
				prmap_ptr->next = (struct prmap_id *)malloc(sizeof(prmap_id));	/* get some memory	*/
				if (!prmap_ptr->next)
				{
					werrlog(ERRORCODE(6),"prmap_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prmap_ptr = (prmap_id *)prmap_ptr->next;	/* set pointer				*/
				prmap_ptr->next = NULL;				/* set next pointer to zero		*/
			}
			if ( (int)strlen(inlin) > 84 )
			{
				werrlog(ERRORCODE(24),WISP_PRMAP_FILE,0,0,0,0,0,0,0);
				werrlog(102,inlin,0,0,0,0,0,0,0);		/* Display the text.			*/
				wexit(ERRORCODE(24));
			}
			inlin[strlen(inlin)-1] = '\0';				/* remove trailing NL			*/
			inlin[3] = '\0';					/* null term after printer #		*/
			prmap_ptr->prmap_num = atoi(inlin);			/* convert printer # to int		*/
			strcpy( prmap_ptr->prmap_string, &inlin[4] );		/* Get printer control string		*/
		}

		fclose(the_file);						/* close the prmap file			*/
	}
}
#endif /* unix */
