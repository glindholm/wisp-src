			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/* The WUSAGE utility. Makes it possible to set/extract WANG USAGE CONSTANTS from the users profile.				*/
/* Use is:															*/
/*	$ WUSAGE <<command> <item=value>>											*/
/*																*/
/*	$ WUSAGE SET INVOL="MYVOL"		! Will set the input volume to "MYVOL"						*/
/*	$ WUSAGE SET OUTVOL=&THE_OUTPUT_VOLUME	! Sets OUTVOL to the value contained in the symbol THE_OUTPUT_VOLUME		*/
/*	$ WUSAGE EXTRACT &MY_VOL=INVOL		! Sets the symbol MY_VOL to the value of INVOL					*/
/*	$ WUSAGE DISPLAY			! Puts up the Wang like shell (HELP screen).					*/
/*	$ WUSAGE SHELL				! Puts up the Wang like shell (HELP screen).					*/
/*	$ WUSAGE READ				! Read the PERSONALITY file.							*/
/*	$ WUSAGE WRITE				! Write the PERSONALITY file.							*/
/*	$ WUSAGE FLAGS				! Allows setting of the HELP flags via a screen					*/
/*	$ WUSAGE FLAGS SET THIS_FLAG="Y"	! Sets the flag THIS_FLAG to "Y" 						*/
/*	$ WUSAGE FLAGS SET THIS_FLAG=&SYM	! Sets the flag THIS_FLAG to the value contained in the symbol SYM		*/
/*	$ WUSAGE FLAGS SET "...0111101110"	! Sets the flags bit mask from low to high. Max 32 characters.			*/
/*	$ WUSAGE FLAGS SET &THE_FLAGS		! Sets the flags bit mask to the value contained in the symbol THE_FLAGS	*/
/*	$ WUSAGE HELP				! Display Help text								*/
/*      $ WUSAGE VERSION 			! Print the WISP VERSION number							*/

#include <stdio.h>
#include <setjmp.h>

#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
#endif

#include <v/video.h>
#include "idsistd.h"
#include "werrlog.h"
#include "scnfacs.h"
#include "wcommon.h"
#include "wperson.h"									/* Get the structure definitions.	*/
#include "wdefines.h"
#include "wanguid.h"

#ifndef VMS	/* unix or MSDOS */
#define EXT_FILEXT
#include "filext.h"
#endif


#define YESNO(x,y)  (x & y ? "Y" : "N")

static char symstr[256];
static char restr[256];
static long tabtyp,symlen;
static int  vol_flag;									/* Is this a lib or vol.		*/
static int  lib_flag;									/* Is this a lib or vol.		*/
extern char wisp_progname[9];
extern char wisp_screen[33];
extern int  noprogscrn;

#ifdef VMS
#define LIB$K_CLI_GLOBAL_SYM	2
static $DESCRIPTOR(sym,symstr);
static $DESCRIPTOR(res,restr);
#endif


/* Define the error codes for this program.											*/

#define GENERAL_ERROR		-1
#define MISSING_SOURCE_ERROR	-2
#define MISSING_OBJECT_ERROR	-3
#define INVALID_SOURCE_ERROR	-4
#define INVALID_OBJECT_ERROR	-5
#define EXTRA_ARGS_ERROR	-6
#define FILE_NOT_FOUND_ERROR	-7
#define INVAL_FLAGS_OBJ_ERROR	-8
#define INVAL_FLAG_TYPE_ERROR	-9
#define INVAL_FLAG_LOG_ERROR	-10

char err_str[256];									/* Hold the string that has error.	*/
static int helptext();

static jmp_buf	env_buf;								/* Hold the environment for error procs	*/

main(argc,argv)
int argc;
char *argv[];
{
#define		ROUTINE		65200

	char tstr[80], *ptr;
	int retcod,status;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

#ifndef VMS
	if (!(ptr=(char *)getenv(WISP_CONFIG_ENV)))
	{
		fprintf( stderr,"%%WUSAGE-W-WISPCONFIG Warning Environment Variable %s is not set.\n",WISP_CONFIG_ENV);
	}
	else if (0!=access(ptr,00))
	{
		fprintf( stderr,"%%WUSAGE-W-WISPCONFIG Warning %s=%s Directory not found.\n",WISP_CONFIG_ENV,ptr);
	}
#endif /* VMS */

	initglbs("WUSAGE  ");
	retcod = 1;
	noprogscrn=1;									/* No program screen to print.		*/
	strcpy(wisp_progname,"WUSAGE");
	strcpy(wisp_screen,"DISPLAY-SCREEN");
	if (!(status = setjmp(env_buf)))						/* Set up to return here.		*/
	{
		if (argc == 1)
		{
			retcod = helptext();						/* Display help text.			*/
		}
		else
		{
			retcod = us_parse(argc,argv);					/* Or just parse it.			*/
		}
	}
	else
	{
		errmsg(status);
		retcod = 0;								/* bad retcode				*/
	}
	if (retcod != 1) errmsg(retcod);

#ifdef VMS
	exit(retcod);
#endif

#ifndef VMS	/* unix or MSDOS */
	vexit();
	if ( retcod == 1 )
	{
		exit(0);
	}
	else if ( retcod == 0 )
	{
		exit(1);
	}
	else
	{
		exit(retcod);
	}
#endif
}

us_parse(argc,argv)									/* Parse the parms.			*/
int argc;
char *argv[];
{
	int 	i,status,retcod, ival, ffl, len;
	char 	src[80];
	char 	dst[80];
	char 	tmp[6];
	char	buff[80];
	long	tlong;

#define P_SET		0								/* Note that values are determined by	*/
#define P_EXTRACT	4								/* their position in the compare string	*/
#define P_DISPLAY	12								/* below (the leading space before).	*/
#define P_READ		20
#define P_WRITE		25
#define P_FLAGS		31
#define P_HELP		37
#define P_VERSION	42
#define P_SHELL		50

#define S_INLIB		0
#define S_INDIR		6
#define S_INVOL		12
#define S_OUTLIB	18
#define S_OUTDIR	25
#define S_OUTVOL	32
#define S_RUNLIB	39
#define S_RUNDIR	46
#define S_RUNVOL	53
#define S_SPOOLIB	60
#define S_SPOOLDIR	68
#define S_SPOOLVOL	77
#define S_WORKLIB	86
#define S_WORKDIR	94
#define S_WORKVOL	102
#define S_USERID	110
#define S_PRNTMODE	117
#define S_PRTCLASS	126
#define S_FORM_NUM	135
#define S_FORM		144
#define S_PRINTER	150
#define S_JOBQUEUE	158
#define S_JOBCLASS	167
#define S_JOBLIMIT	176
#define S_LUSERID	185
#define S_LINES		193
#define S_PROGVOL	199
#define S_PROGLIB	207

static char item_str[256];

#ifdef VMS
	static $DESCRIPTOR(item_desc,item_str);						/* A descriptor for getting symbols.	*/
#endif

	upper_string(argv[1]);

	retcod = 1;
/*		    012345678901234567890123456789012345678901234567890123456789						*/
	i = strpos("SET EXTRACT DISPLAY READ WRITE FLAGS HELP VERSION SHELL",argv[1]);	/* Look for a keyword.			*/
	switch(i)
	{
		case P_SET:								/* Process a SET command		*/
		{
			vol_flag = 0;
			lib_flag = 0;
			set_upper(argc,argv);						/* Make sure all upper case.		*/
			load_defaults();						/* First load in the constants.		*/
			status = us_equ(argc,argv,dst,src);				/* Parse out the equation.		*/

			if (status) longjmp(env_buf,status);				/* If error, say so.			*/

			item_str[0] = ' ';
			item_str[1] = '\0';						/* Start with a leading space.		*/

			strcat(item_str,dst);
			strcat(item_str," ");						/* Add the item name and a space.	*/
			i = strpos(" INLIB INDIR INVOL OUTLIB OUTDIR OUTVOL RUNLIB RUNDIR RUNVOL ",item_str);
			if (i == -1)
			{
				i = strpos(" SPOOLIB SPOOLDIR SPOOLVOL WORKLIB WORKDIR WORKVOL USERID ",item_str);
				if (i != -1) i += 60;
			}
			if (i == -1)
			{
				i=strpos(" PRNTMODE PRTCLASS FORM_NUM FORM# PRINTER JOBQUEUE JOBCLASS JOBLIMIT LUSERID ",item_str);
				if (i != -1) i += 117;
			}
			if (i == -1)
			{
				i = strpos(" LINES PROGVOL PROGLIB ",item_str);
				if (i != -1) i += 193;
			}

			switch (i)
			{
				case S_INLIB:
				case S_INDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,8);				/* Copy the value.			*/
					set_defs(DEFAULTS_IL,buff);
					break;
				}
				case S_INVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_IV,buff);
					break;
				}
				case S_OUTLIB:
				case S_OUTDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,8);				/* Copy the value.			*/
					set_defs(DEFAULTS_OL,buff);
					break;
				}
				case S_OUTVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_OV,buff);
					break;
				}
				case S_PROGLIB:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,8);				/* Copy the value.			*/
					set_defs(DEFAULTS_PL,buff);
					break;
				}
				case S_PROGVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_PV,buff);
					break;
				}
				case S_RUNLIB:
				case S_RUNDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,8);				/* Copy the value.			*/
					set_defs(DEFAULTS_RL,buff);
					break;
				}
				case S_RUNVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_RV,buff);
					break;
				}
				case S_SPOOLIB:
				case S_SPOOLDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,8);				/* Copy the value.			*/
					set_defs(DEFAULTS_SL,buff);
					break;
				}
				case S_SPOOLVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_SV,buff);
					break;
				}
				/*   S_WORKLIB is only available for EXTRACT.  Is defined so consistent. */
				/*   S_WORKDIR is only available for EXTRACT.  Is defined so consistent. */
				/*   S_USERID is only available for EXTRACT.  Is defined so consistent. */
				/*   S_LUSERID is only available for EXTRACT.  Is defined so consistent. */
				case S_WORKVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_WV,buff);
					break;
				}
				case S_PRNTMODE:
				{
					us_set(buff,src,1);				/* Copy the value.			*/
					set_defs(DEFAULTS_PM,buff);
					break;
				}
				case S_PRTCLASS:
				{
					us_set(buff,src,1);				/* Copy the value.			*/
					set_defs(DEFAULTS_PC,buff);
					break;
				}
				case S_FORM_NUM:
				case S_FORM:
				{
					us_iset(&tlong,src);				/* Copy the value.			*/
					set_defs(DEFAULTS_FN,&tlong);
					break;
				}
				case S_PRINTER:
				{
					us_iset(&tlong,src);				/* Copy the value.			*/
					set_defs(DEFAULTS_PR,&tlong);
					break;
				}
				case S_JOBQUEUE:
				{
					us_set(buff,src,1);				/* Copy the value.			*/
					set_defs(DEFAULTS_JS,buff);
					break;
				}
				case S_JOBCLASS:
				{
					us_set(buff,src,1);				/* Copy the value.			*/
					set_defs(DEFAULTS_JC,buff);
					break;
				}
				case S_JOBLIMIT:
				{
					us_set(buff,src,6);				/* Copy the value.			*/
					set_defs(DEFAULTS_JL,buff);
					break;
				}
				case S_LINES:
				{
					us_iset(&tlong,src);				/* Copy the value.			*/
					set_defs(DEFAULTS_LI,&tlong);
					break;
				}
				default:
				{
					return(INVALID_OBJECT_ERROR);
					break;
				}
			}
			save_defaults();						/* Save the defaults.			*/
			break;
		}

		case P_EXTRACT:								/* Process an EXTRACT command.		*/
		{
			lib_flag = 0;
			vol_flag = 0;
			set_upper(argc,argv);						/* Make sure all upper case.		*/
			load_defaults();						/* First load in the constants.		*/
#ifdef VMS
			status = us_equ(argc,argv,dst,src);				/* Parse out the equation.		*/
#endif
#ifndef VMS	/* unix or MSDOS */
			if ( argc < 3 ) status = MISSING_SOURCE_ERROR;			/* Error, no parms to parse.		*/
			if ( argc > 3 ) status = EXTRA_ARGS_ERROR;
			if ( argc == 3 )
			{
				strcpy( src, argv[2] );
				*dst = 0;
				status = 0;
			}
#endif
			if (status) longjmp(env_buf,status);				/* If error, say so.			*/

			item_str[0] = ' ';
			item_str[1] = '\0';						/* Start with a leading space.		*/

			strcat(item_str,src);
			strcat(item_str," ");						/* Add the item name and a space.	*/
			i = strpos(" INLIB INDIR INVOL OUTLIB OUTDIR OUTVOL RUNLIB RUNDIR RUNVOL ",item_str);
			if (i == -1)
			{
				i = strpos(" SPOOLIB SPOOLDIR SPOOLVOL WORKLIB WORKDIR WORKVOL USERID ",item_str);
				if (i != -1) i += 60;
			}
			if (i == -1)
			{
				i=strpos(" PRNTMODE PRTCLASS FORM_NUM FORM# PRINTER JOBQUEUE JOBCLASS JOBLIMIT LUSERID ",item_str);
				if (i != -1) i += 117;
			}
			if (i == -1)
			{
				i = strpos(" LINES PROGVOL PROGLIB ",item_str);
				if (i != -1) i += 193;
			}

			switch (i)
			{
				case S_INLIB:
				case S_INDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_IL,buff);
					us_ext(dst,buff,8);				/* Copy the value.			*/
					break;
				}
				case S_INVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_IV,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_OUTLIB:
				case S_OUTDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_OL,buff);
					us_ext(dst,buff,8);				/* Copy the value.			*/
					break;
				}
				case S_OUTVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_OV,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_PROGLIB:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_PL,buff);
					us_ext(dst,buff,8);				/* Copy the value.			*/
					break;
				}
				case S_PROGVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_PV,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_RUNLIB:
				case S_RUNDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_RL,buff);
					us_ext(dst,buff,8);				/* Copy the value.			*/
					break;
				}
				case S_RUNVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_RV,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_SPOOLIB:
				case S_SPOOLDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_SL,buff);
					us_ext(dst,buff,8);				/* Copy the value.			*/
					break;
				}
				case S_SPOOLVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_SV,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_WORKLIB:
				case S_WORKDIR:
				{
					lib_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_WL,buff);
					us_ext(dst,buff,8);				/* Copy the value.			*/
					break;
				}
				case S_WORKVOL:
				{
					vol_flag = 1;					/* This is a lib or vol.		*/
					get_defs(DEFAULTS_WV,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_USERID:						/* Will get the 3 letter abbreviation.	*/
				{
					char id[4];

					memcpy(id,wanguid3(),3);
					id[3] = '\0';					/* Null terminate the string.		*/
					us_ext(dst,id,3);				/* Copy the userid.			*/
					break;
				}
				case S_LUSERID:						/* Will get the full userid.		*/
				{
					char *longuid();
					char id[32];

					strcpy(id,longuid());
					us_ext(dst,id,strlen(longuid()));		/* Copy the userid.			*/
					break;
				}
				case S_PRNTMODE:
				{
					get_defs(DEFAULTS_PM,buff);
					us_ext(dst,buff,1);				/* Copy the value.			*/
					break;
				}
				case S_PRTCLASS:
				{
					get_defs(DEFAULTS_PC,buff);
					us_ext(dst,buff,1);				/* Copy the value.			*/
					break;
				}
				case S_FORM_NUM:
				case S_FORM:
				{
					get_defs(DEFAULTS_FN,&tlong);
					sprintf(buff,"%03ld",tlong);			/* Convert to a string.			*/
					us_ext(dst,buff,3);				/* Copy the value.			*/
					break;
				}
				case S_PRINTER:
				{
					get_defs(DEFAULTS_PR,&tlong);
					sprintf(buff,"%03ld",tlong);			/* Convert to a string.			*/
					us_ext(dst,buff,3);				/* Copy the value.			*/
					break;
				}
				case S_JOBQUEUE:
				{
					get_defs(DEFAULTS_JS,buff);
					us_ext(dst,buff,1);				/* Copy the value.			*/
					break;
				}
				case S_JOBCLASS:
				{
					get_defs(DEFAULTS_JC,buff);
					us_ext(dst,buff,1);				/* Copy the value.			*/
					break;
				}
				case S_JOBLIMIT:
				{
					get_defs(DEFAULTS_JL,buff);
					us_ext(dst,buff,6);				/* Copy the value.			*/
					break;
				}
				case S_LINES:
				{
					get_defs(DEFAULTS_LI,&tlong);
					sprintf(buff,"%03ld",tlong);			/* Convert to a string.			*/
					us_ext(dst,buff,3);				/* Copy the value.			*/
					break;
				}
				default:
				{
					return(INVALID_SOURCE_ERROR);
					break;
				}
			}
			break;
		}

		case P_DISPLAY:								/* Process a DISPLAY or SHELL command.	*/ 
		case P_SHELL:
		{
			w_err_flag = 1+4+8;						/* Turn on error processing		*/
			retcod = us_help();						/* Use help mode.			*/
			vexit();
			break;
		}

		case P_READ:								/* Process a READ command.		*/
		{
			if (argc > 2) strcpy(src,argv[2]);				/* See if they provided a file name.	*/
			else	      src[0] = '\0';					/* Or set to null			*/
			ffl = read_defaults_from_file(src);				/* Always load in the file.		*/
			if (argc > 2 && ffl) retcod = FILE_NOT_FOUND_ERROR;		/* Return error code.			*/
			save_defaults();						/* And replace the local symbol.	*/
			break;
		}

		case P_WRITE:								/* Process a WRITE command.		*/
		{
			if (argc > 2) strcpy(dst,argv[2]);				/* See if they provided a file name.	*/
			else	      dst[0] = '\0';					/* Or set to null			*/
			load_defaults();						/* Be sure they exist.			*/
			write_defaults_to_file(dst);					/* Store the file.			*/
			break;
		}

		case P_FLAGS:								/* Set the flags.			*/
		{
			if (argc == 2)							/* Change flags via menu options.	*/
			{
				load_defaults();
				us_flags();
				vexit();
			}
			else retcod = us_flmask(argc,argv);				/* Change one flag or complete bit mask.*/
			break;								/* from DCL prompt.			*/
		}

		case P_HELP:								/* Display help text.			*/
		{
			helptext();
			break;
		}

		case P_VERSION:
		{
			printf("\n\n");
			printf("WISP: Version=[%s] Library=[%d] Screen=[%d]\n\n",WISP_VERSION, LIBRARY_VERSION, SCREEN_VERSION);
			break;
		}

		default:								/* Some kind of error.			*/
		{
			return(-1);
		}
	}
	return(retcod);									/* Success.				*/
}

us_inter()										/* Read input lines and parse them.	*/
{
	return(1);									/* Sucessfull.				*/
}

us_help()										/* Display the Wang like shell		*/
{											/* (HELP screen).			*/
	char selection[81];
	char scrn[1924];
	char function,lines,term[2],no_mod[2];

	init_screen();
	wsc_init(scrn,0,0);

	function = 1;									/* Use WRITE_ALL.			*/
	lines = 24;
	vwang(&function,scrn,&lines,"0001X",term,no_mod);				/* Call Wang emulation to fill screen.	*/
											/* So HELP will have a screen to push.	*/
	wsh_help(0);									/* Do it.				*/
	return(1);									/* Always a success.			*/
}

us_set(dst,src,len)									/* Set a field to it's value.		*/
char *dst,*src;
int len;
{
	register int i;
	long status;

	memset(dst,' ',len);								/* Always fill the field with spaces.	*/
	i = strlen(src);
	if (i > len) i = len;								/* Never copy more than the field size.	*/
	if (i)
	{
		if (src[0] == '&')
		{
#ifdef VMS
			memset(symstr,' ',256);						/* Fill with spaces.			*/
			symstr[255] = '\0';						/* Null terminate.			*/
			memcpy(symstr,&src[1],strlen(&src[1]));				/* Copy the symbol name			*/
			status = lib$get_symbol(&sym,&res,&symlen,&tabtyp);	  	/* Get it's contents.			*/
			restr[symlen] = '\0';						/* Null terminate.			*/

			if (status == SS$_NORMAL)
			{
				upper(restr,&symlen);					/* Convert to upper case for COBOL.	*/
				strcpy(dst,restr);					/* Now store it.			*/
			}
			else
			{
				exit(status);						/* An error.				*/
			}
#endif
		}
		else
		{
			memcpy(dst,src,i);						/* Copy the data now.			*/
		}

	}
}

us_iset(dst,src)									/* Set a field to it's value.		*/
char *dst,*src;
{
	long status;
	int ival;

	memset(dst,'0',4);							/* Always fill the field with zeroes.	*/
	if (src[0] == '&')
	{
#ifdef VMS
		memset(symstr,' ',256);						/* Fill with spaces.			*/
		symstr[255] = '\0';						/* Null terminate.			*/
		memcpy(symstr,&src[1],strlen(&src[1]));				/* Copy the symbol name			*/
		status = lib$get_symbol(&sym,&res,&symlen,&tabtyp);	  	/* Get it's contents.			*/
		restr[symlen] = '\0';						/* Null terminate.			*/

		if (status == SS$_NORMAL)
		{
			ival = atoi(restr);					/* Convert to an integer.		*/
			memcpy(dst,&ival,sizeof(int));				/* Copy the data now.			*/
		}
		else
		{
			exit(status);						/* An error.				*/
		}
#endif
	}
	else
	{
		ival = atoi(src);							/* Convert to an integer.		*/
		memcpy(dst,&ival,4);							/* Copy the data now.			*/
	}
}

us_ext(dst,src,len)									/* Extract the value of a field.	*/
char *dst,*src;
int len;
{
#ifdef VMS
	/*
	**	This loads the value (src) into the symbol (dst)
	*/
	register int i,j,sl;
	long status;

	if (dst[0] == '&') j = 1; else j = 0;						/* if leading & then set offset		*/
	memset(symstr,' ',256);								/* Fill symbol string with spaces.	*/
	symstr[255] = '\0';								/* Null terminate.			*/
	sl = strlen(&dst[j]);
	memcpy(symstr,&dst[j],sl);							/* Copy the symbol name			*/
	sym.dsc$w_length = sl;								/* Set the length in the descriptor.	*/

	memset(restr,' ',len);								/* Set to spaces (src may be < len)	*/
	res.dsc$w_length = len;								/* Set the length in the descriptor.	*/
	for(i=0; i<len && src[i]; i++) {}						/* Calc actual length			*/
	memcpy(restr,src,i);								/* Copy the value to set it to.		*/
	status = lib$set_symbol(&sym,&res,0);						/* Set it's contents.			*/

	if (status != SS$_NORMAL)
	{
		exit(status);								/* An error.				*/
	}
#endif	/* #ifdef VMS */

#ifndef VMS	/* unix or MSDOS */                                                               
	int	i;
	char	out[80];

	memcpy(out,src,len);

	if (lib_flag)
	{
		if ( memcmp(out,"        ",8) == 0 ) strcpy(out,".       ");
	}
	if (vol_flag)
	{
		if ( memcmp(out,"      ",6)   == 0 ) strcpy(out,".     ");
	}
	for( i=0; i<len; i++) putchar( out[i] );
#endif
}

/* 			Display a screen to allow the user to set up the flags for the help screen.				*/

us_flags()
{
	char *field();									/* Field packing subroutine.		*/
	char *screen;									/* Pointer to working screen routine.	*/
	char function, lines, term[2], no_mod;						/* Working variables.			*/
	char yn[2];
	int valid;
	int	pos;									/* position on screen			*/
	unsigned long	defs_flags;
	int	i;
	int	xcol[32], xrow[32];

	yn[0] = ' ';									/* Init the field for wget function.	*/
	yn[1] = '\0';

	if ((screen = malloc(1924+8)) == 0) exit(0);					/* Able to get memory?			*/

	get_defs(DEFAULTS_FLAGS,&defs_flags);						/* Get the defaults flags		*/

	/*
	**	There will be 2 columns of 16 rows for a total of 32 possible flags.
	**	This loop initializes the row and col indexes.
	*/
#define ROWOFFSET	5
#define NUMROWS		16

	for(i=0; i<NUMROWS; i++)
	{
		xcol[i] = 2;
		xcol[i+NUMROWS] = 42;
		xrow[i] = i+ROWOFFSET;
		xrow[i+NUMROWS] = i+ROWOFFSET;
	}

	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/
	wput(screen, 1,20,PLAIN_TEXT,"*** Set General Usage Flags ***");		/* Layout the screen.			*/

	pos = 0;
	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Help Screen");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_ENABLED));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"SET File Usage Constants");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SET_FILES));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"SET Print Mode Defaults");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SET_PRINTER));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"SET Submit Procedure Defaults");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SET_PROC));
	pos += 1;

#ifdef unix
	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Manage PRINT QUEUE");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_QUEUE_MNGMNT));
	pos += 1;
#endif

#ifdef VMS
	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Manage QUEUES");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_QUEUE_MNGMNT));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Search of Batch Queues");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SBATCH_ENABLED));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Search of Generic Queues");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SGENERIC_ENABLED));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Search of Output Queues");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SOUTPUT_ENABLED));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"display of queue entries other than user's");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_RESTRJOBS_DISABLED));
	pos += 1;
#endif

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Manage SYSTEM");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_MANAGE_SYSTEM));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Use UTILITIES (DISPLAY)");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_DISPLAY));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Use UTILITIES (EDIT)");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_EDIT));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Use UTILITIES (DISPRINT)");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_DISPRINT));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Use UTILITIES (KCSI)");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_CRID));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Configure TERMINAL");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_TERMINAL));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"PSEUDO blank characteristics");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SETPSB));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"CURSOR Characteristics");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SETCURCHAR));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"SCREEN Characteristics");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_SCREEN));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Enter COMMANDS");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_COMMANDS));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"SAVE environment");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_USAGE_WRITE));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"PRINT Screens");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_PRINT_SCREEN));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"CANCEL Processing");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_CANCEL));
	pos += 1;

#ifndef VMS
	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Manage FILES/LIBRARIES");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_MANAGE_FILES_LIBS));
	pos += 1;

	wput(screen,xrow[pos],xcol[pos]+2,PLAIN_TEXT,"Manage FILES/LIBRARIES (Modify)");
	wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD,YESNO(defs_flags,HELP_CHANGE_FILES_LIBS));
	pos += 1;
#endif

	wput(screen,24,2,PLAIN_TEXT,"Change the information as appropriate and depress (RETURN), (1) to exit.");

disp_flags:

	function = 7;									/* Use display and read altered.	*/
	lines = 24;
	vwang(&function,screen,&lines,"0001X",term,&no_mod);				/* Call Wang emulation to fill screen.	*/
                                                       
	if ((no_mod == 'M') && !((term[0] == '0') && (term[1] == '1')))			/* Don't write unless they press enter	*/
	{										/* and we they've changed something.	*/
		valid = 1;

		pos = 0;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_ENABLED;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_ENABLED);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SET_FILES;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SET_FILES);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SET_PRINTER;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SET_PRINTER);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SET_PROC;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SET_PROC);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

#ifdef unix
		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_QUEUE_MNGMNT;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_QUEUE_MNGMNT);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;
#endif

#ifdef VMS
		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_QUEUE_MNGMNT;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_QUEUE_MNGMNT);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SBATCH_ENABLED;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SBATCH_ENABLED);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SGENERIC_ENABLED;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SGENERIC_ENABLED);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SOUTPUT_ENABLED;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SOUTPUT_ENABLED);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_RESTRJOBS_DISABLED;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_RESTRJOBS_DISABLED);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;
#endif

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_MANAGE_SYSTEM;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_MANAGE_SYSTEM);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_DISPLAY;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_DISPLAY);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_EDIT;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_EDIT);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_DISPRINT;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_DISPRINT);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_CRID;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_CRID);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_TERMINAL;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_TERMINAL);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SETPSB;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SETPSB);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SETCURCHAR;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SETCURCHAR);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_SCREEN;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_SCREEN);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_COMMANDS;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_COMMANDS);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_USAGE_WRITE;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_USAGE_WRITE);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_PRINT_SCREEN;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_PRINT_SCREEN);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_CANCEL;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_CANCEL);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

#ifndef VMS
		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_MANAGE_FILES_LIBS;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_MANAGE_FILES_LIBS);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;

		wget(screen,xrow[pos],xcol[pos],yn);
		if (yn[0] == 'Y') defs_flags |= HELP_CHANGE_FILES_LIBS;
		else if (yn[0] == 'N') defs_flags &= (~ HELP_CHANGE_FILES_LIBS);
		else { valid = 0; wput(screen,xrow[pos],xcol[pos],UPCASE_FIELD | BLINK_FAC,yn);}
		pos += 1;
#endif

		set_defs(DEFAULTS_FLAGS,&defs_flags);					/* Set the defaults flags		*/

		if (valid) save_defaults();						/* Write out the personality info.	*/
		else goto disp_flags;
	}
	free(screen);									/* Release the screen memory.		*/
	return(SUCCESS);								/* All done.				*/
}

int us_flmask(argc, argv)								/* Set the usage flags mask.		*/
int argc;
char *argv[];
{
	int i, j, status, retcod, setone, setfl, loopcnt;
	int flmsk[32];
	char src[80];
	char dst[80];
	static char item_str[256];
	unsigned long	defs_flags;

#ifdef VMS
	static $DESCRIPTOR(item_desc,item_str);						/* A descriptor for getting symbols.	*/
#endif

#define F_HELP		0								/* Define usage flags options.		*/
#define F_VOLDIR	5								/* Note the values are defined by the	*/
#define F_PRNTC		12								/* position of the flag in the compare	*/
#define F_PQUEC		18								/* string below (leading space before).	*/
#define F_PRNTPS	24
#define F_TERMC		31
#define F_KEYBM		37
#define F_PSBO		43
#define F_DISPF		48
#define F_WUSEC		54
#define F_CURCHAR	60

#define F_DCLCMDS	68
#define F_QMNGMNT	76
#define F_SBQ		84
#define F_SGQ		88
#define F_SOQ		92
#define F_SALLJ		100

#define F_SHELLCMDS	106
#define F_PRTQUE	116

#define F_SETFILE	129
#define F_SETPRINT	137
#define F_SETSUB	146
#define F_QUEUES	153
#define F_SYSTEM	160
#define F_UTILS		167
#define F_TERMINAL	173
#define F_COMMANDS	182
#define F_SAVE		191
#define F_PRINTSCR	200
#define F_DISPLAY	209
#define F_CRID		217
#define F_PSEUDO	222
#define F_CURSOR	229
#define F_SCREEN	236
#define F_FILES		243
#define F_CANCEL	249
#define F_EDIT		256
#define F_DISPRINT	261
#define F_MODFILES	270
#define F_KSCI		279

	upper_string(argv[2]);
	retcod = 1;
/*		    01234567890123456789012345678901234567890									*/
	i = strpos("SET  ",argv[2]);							/* Look for a keyword.			*/
	switch(i)
	{
		case P_SET:								/* Process a SET command		*/
		{
			set_upper(argc,argv);						/* Make sure all upper case.		*/
			load_defaults();						/* First load in the constants.		*/
			get_defs(DEFAULTS_FLAGS,&defs_flags);				/* Get the defaults flags		*/
			setone = us_flagequ(argc,argv,dst,src);				/* Parse out the equation.		*/
			if (setone)
			{
				item_str[0] = ' ';
				item_str[1] = '\0';					/* Start with a leading space.		*/

				strcat(item_str,dst);
				strcat(item_str," ");					/* Add the item name and a space.	*/

	/* 
		The following is searches for the flag keyword.
		It is indent this way to make it managable.
		We are continuing to support old flag keywords.
	*/


	/* (0)      0    5    |   15    |   25    |   35    |   45    |   55    |   65    |   75    |   85    |   95    |	*/
	i = strpos(" HELP VOLDIR PRNTC PQUEC PRNTPS TERMC KEYBM PSBO DISPF WUSEC CURCHAR DCLCMDS QMNGMNT SBQ SGQ SOQ ",item_str);
				if (i == -1)
				{
	/* (100)    0    5    |   15    |   25    |   35    |   45    |   55    |   65    |   75    |   85    |   95    |	*/
	i = strpos(" SALLJ SHELLCMDS PRTQUE       SETFILE SETPRINT SETSUB QUEUES SYSTEM UTILS TERMINAL COMMANDS SAVE ",item_str);
					if (i != -1) i += 100;
					else
					{
	/* (200)    0    5    |   15    |   25    |   35    |   45    |   55    |   65    |   75    |   85    |   95    |	*/
	i = strpos(" PRINTSCR DISPLAY CRID PSEUDO CURSOR SCREEN FILES CANCEL EDIT DISPRINT MODFILES KCSI ",item_str);
						if (i != -1) i+= 200;
					}

				}
				if (i == -1) return(INVAL_FLAG_TYPE_ERROR);
				flmsk[0] = i;						/* Put flag choosen into array.		*/
				loopcnt = 1;						/* Assign to go through loop once.	*/
			}
			else								/* Set up for mask assignment.		*/
			{
			}
			if ((strcmp(src,"Y") == 0) || (strcmp(src,"T") == 0) || (strcmp(src,"1") == 0)) setfl = 1;
			else if ((strcmp(src,"N") == 0) || (strcmp(src,"F") == 0) || (strcmp(src,"0") == 0)) setfl =0;
			else return(INVAL_FLAG_LOG_ERROR);
			for (j=0; j < 1; j++)						/* Cycle through for each bit in passed */
			{								/* mask or for the one flag choosen.	*/
				i = flmsk[j];						/* Set up for flag to adjust.		*/
				switch(i)
				{
					case F_HELP:
					{
						if (!setfl) 	defs_flags &= (~ HELP_ENABLED);
						else 		defs_flags |=    HELP_ENABLED;
						break;
					}
					case F_SETFILE:
					case F_VOLDIR:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SET_FILES);
						else 		defs_flags |=    HELP_SET_FILES;
						break;
					}
					case F_SETPRINT:
					case F_PRNTC:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SET_PRINTER);
						else 		defs_flags |=    HELP_SET_PRINTER;
						break;
					}
					case F_SETSUB:
					case F_PQUEC:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SET_PROC);
						else 		defs_flags |=    HELP_SET_PROC;
						break;
					}
					case F_PRINTSCR:
					case F_PRNTPS:
					{
						if (!setfl) 	defs_flags &= (~ HELP_PRINT_SCREEN);
						else 		defs_flags |=    HELP_PRINT_SCREEN;
						break;
					}
					case F_TERMINAL:
					case F_TERMC:
					{
						if (!setfl) 	defs_flags &= (~ HELP_TERMINAL);
						else 		defs_flags |=    HELP_TERMINAL;
						break;
					}
					case F_PSEUDO:
					case F_PSBO:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SETPSB);
						else 		defs_flags |=    HELP_SETPSB;
						break;
					}
					case F_UTILS:
					{
						if (!setfl) 	defs_flags &= (~ HELP_EDIT);
						else 		defs_flags |=    HELP_EDIT;
						if (!setfl) 	defs_flags &= (~ HELP_DISPRINT);
						else 		defs_flags |=    HELP_DISPRINT;
						break;
					}
					case F_DISPLAY:
					case F_DISPF:
					{
						if (!setfl) 	defs_flags &= (~ HELP_DISPLAY);
						else 		defs_flags |=    HELP_DISPLAY;
						break;
					}
					case F_SAVE:
					case F_WUSEC:
					{
						if (!setfl) 	defs_flags &= (~ HELP_USAGE_WRITE);
						else 		defs_flags |=    HELP_USAGE_WRITE;
						break;
					}
					case F_CURSOR:
					case F_CURCHAR:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SETCURCHAR);
						else 		defs_flags |=    HELP_SETCURCHAR;
						break;
					}

					case F_COMMANDS:
					case F_DCLCMDS:
					case F_SHELLCMDS:
					{
						if (!setfl) 	defs_flags &= (~ HELP_COMMANDS);
						else 		defs_flags |=    HELP_COMMANDS;
						break;
					}

					case F_QUEUES:
					case F_PRTQUE:
					case F_QMNGMNT:
					{
						if (!setfl) 	defs_flags &= (~ HELP_QUEUE_MNGMNT);
						else 		defs_flags |=    HELP_QUEUE_MNGMNT;
						break;
					}
					case F_SBQ:
					{
						if (!setfl)  	defs_flags &= (~ HELP_SBATCH_ENABLED);
						else 		defs_flags |=    HELP_SBATCH_ENABLED;
						break;
					}
					case F_SGQ:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SGENERIC_ENABLED);
						else 		defs_flags |=    HELP_SGENERIC_ENABLED;
						break;
					}
					case F_SOQ:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SOUTPUT_ENABLED);
						else 		defs_flags |=    HELP_SOUTPUT_ENABLED;
						break;
					}
					case F_SALLJ:
					{
						if (!setfl) 	defs_flags &= (~ HELP_RESTRJOBS_DISABLED);
						else 		defs_flags |=    HELP_RESTRJOBS_DISABLED;
						break;
					}

					case F_SCREEN:
					{
						if (!setfl) 	defs_flags &= (~ HELP_SCREEN);
						else 		defs_flags |=    HELP_SCREEN;
						break;
					}

					case F_CRID:
					case F_KSCI:
					{
						if (!setfl) 	defs_flags &= (~ HELP_CRID);
						else 		defs_flags |=    HELP_CRID;
						break;
					}

					case F_SYSTEM:
					{
						if (!setfl) 	defs_flags &= (~ HELP_MANAGE_SYSTEM);
						else 		defs_flags |=    HELP_MANAGE_SYSTEM;
						break;
					}

					case F_FILES:
					{
						if (!setfl) 	defs_flags &= (~ HELP_MANAGE_FILES_LIBS);
						else 		defs_flags |=    HELP_MANAGE_FILES_LIBS;
						break;
					}

					case F_CANCEL:
					{
						if (!setfl) 	defs_flags &= (~ HELP_CANCEL);
						else 		defs_flags |=    HELP_CANCEL;
						break;
					}

					case F_EDIT:
					{
						if (!setfl) 	defs_flags &= (~ HELP_EDIT);
						else 		defs_flags |=    HELP_EDIT;
						break;
					}

					case F_DISPRINT:
					{
						if (!setfl) 	defs_flags &= (~ HELP_DISPRINT);
						else 		defs_flags |=    HELP_DISPRINT;
						break;
					}

					case F_MODFILES:
					{
						if (!setfl) 	defs_flags &= (~ HELP_CHANGE_FILES_LIBS);
						else 		defs_flags |=    HELP_CHANGE_FILES_LIBS;
						break;
					}

					default:
					{
						return(INVAL_FLAG_TYPE_ERROR);
						break;
					}
				} /* end of switch */
			} /* end of for */
			set_defs(DEFAULTS_FLAGS,&defs_flags);				/* Set the defaults flags		*/
			save_defaults();						/* Write out the personality info.	*/
			break;
		} /* end of case SET */
		default:
		{
			return(INVAL_FLAGS_OBJ_ERROR);
			break;
		}
	} /* end of switch */
	return(1);
}

errmsg(num)
int num;
{
	switch(num)
	{
		case GENERAL_ERROR:
		           default:
		{
			werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case MISSING_SOURCE_ERROR:
		{
			werrlog(ERRORCODE(4),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case MISSING_OBJECT_ERROR:
		{
			werrlog(ERRORCODE(6),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case INVALID_SOURCE_ERROR:
		{
			werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case INVALID_OBJECT_ERROR:
		{
			werrlog(ERRORCODE(10),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case EXTRA_ARGS_ERROR:
		{
			werrlog(ERRORCODE(12),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case FILE_NOT_FOUND_ERROR:
		{
			werrlog(ERRORCODE(14),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case INVAL_FLAGS_OBJ_ERROR:
		{
			werrlog(ERRORCODE(16),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case INVAL_FLAG_TYPE_ERROR:
		{
			werrlog(ERRORCODE(18),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
		case INVAL_FLAG_LOG_ERROR:
		{
			werrlog(ERRORCODE(20),0,0,0,0,0,0,0,0);				/* Report the situation.		*/
			break;
		}
	}
	fflush(stderr);
}

set_upper(argc,argv)									/* Convert all the args to upper case.	*/
int argc;
char *argv[];
{
	int i,j,len;

	for (i = 1; i < argc; i++)
	{
		j=0;

		while(argv[i][j])
		{
			argv[i][j] = toupper(argv[i][j]);
			j++;
		}
	}
}

int us_equ(argc,argv,dst,src)								/* Parse an equation into it's parts.	*/
int argc;
char *argv[];
char *dst,*src;
{
	int i,offset;

	if (argc < 3) return(MISSING_OBJECT_ERROR);					/* Error, no parms to parse.		*/
	offset = 0;

	if ((i=strpos(argv[2],"=")) != -1)						/* The "=" is in the parm.		*/
	{
		memcpy(dst,argv[2],i);							/* Copy what we need from the parm	*/
		dst[i] = '\0';								/* Null terminate.			*/

		if (argv[2][i+1])							/* There is more after the "="		*/
		{
			strcpy(src,&argv[2][i+1]);					/* Copy into source string.		*/
		}
		else									/* The parm has to be in the next ptr.	*/
		{
			if (argc < 4) return(MISSING_SOURCE_ERROR);			/* There is no next pointer.		*/
			strcpy(src,argv[3]);						/* There is, copy the parm.		*/
		}
	}
	else										/* No "=" in the parm, scan for more.	*/
	{
		strcpy(dst,argv[2]);							/* Get the Destination.			*/

		if (argv[3][0] == '=')							/* This parm has the equals in it.	*/
		{
			if (argv[3][1])							/* Followed by the source string.	*/
			{
				strcpy(src,&argv[3][1]);				/* Save the string.			*/
			}
			else if (argc < 5)						/* If it's not in the next string.	*/
			{
				return(MISSING_SOURCE_ERROR);				/* Return the error			*/
			}
			else
			{
				strcpy(src,argv[4]);					/* Otherwise return the next arg.	*/
			}
		}
		else
		{
			if (argc < 5) return(MISSING_SOURCE_ERROR);			/* Get the value after the equals.	*/
			strcpy(src,argv[4]);
		}
	}

	return(0);
}

int us_flagequ(argc,argv,dst,src)							/* Parse an equation into it's parts.	*/
int argc;
char *argv[];
char *dst,*src;
{
	int i,offset;

	if (argc < 4) return(MISSING_OBJECT_ERROR);					/* Error, no parms to parse.		*/
	offset = 0;

	if ((i=strpos(argv[3],"=")) != -1)						/* The "=" is in the parm so setting one.*/
	{										/*  flag only.				*/
		memcpy(dst,argv[3],i);							/* Copy what we need from the parm	*/
		dst[i] = '\0';								/* Null terminate.			*/

		if (argv[3][i+1])							/* There is more after the "="		*/
		{
			strcpy(src,&argv[3][i+1]);					/* Copy into source string.		*/
		}
		else									/* The parm has to be in the next ptr.	*/
		{
			if (argc < 4) return(MISSING_SOURCE_ERROR);			/* There is no next pointer.		*/
			strcpy(src,argv[3]);						/* There is, copy the parm.		*/
		}
		return(1);								/* Return 1 for setting one flag.	*/
	}
	else										/* No "=" in the parm so setting	*/
	{										/* complet bit mask.			*/
		strcpy(dst,argv[3]);
		return(0);
	}
}

static int helptext()
{
#ifndef VMS	/* unix or MSDOS */
              /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	      /*         1         2         3         4         5         6         7         8*/
	printf("\n");
	printf("WUSAGE:    Allows setting and extracting of usage constants.\n");
	printf("   wusage shell                        Command Processor Shell.\n");
	printf("   wusage read [file]                  Load usage constants from PERSONALITY.\n");
	printf("   wusage write [file]                 Save usage constants to PERSONALITY.\n");
	printf("   wusage set <item>=<value>           Set a usage constant.\n");
	printf("   wusage extract <item>               Extract a usage constant.\n");
	printf("   wusage flags                        Enter full screen flag setting mode.\n");
	printf("   wusage flags set <flag>=<logical>   Set one flag in the bit mask.\n");
	printf("   wusage version                      Print the WISP VERSION info.\n");
	printf("\n");
	printf("   <item> = INLIB,INVOL,OUTLIB,OUTVOL,RUNLIB,RUNVOL,SPOOLIB,SPOOLVOL,\n");
	printf("            WORKLIB,WORKVOL,PROGLIB,PROGVOL,PRNTMODE,PRTCLASS,FORM#,PRINTER,\n");
	printf("            JOBQUEUE,JOBCLASS,JOBLIMIT,FLAGS,USERID,LUSERID,LINES\n");
	printf("   <flag> = HELP,SETFILE,SETPRINT,SETSUB,PRTQUE,SYSTEM,UTILS,DISPLAY,EDIT,\n");
	printf("            DISPRINT,KCSI,TERMINAL,PSEUDO,CURSOR,SCREEN,COMMANDS,SAVE,PRINTSCR,\n");
	printf("            CANCEL,FILES,MODFILES\n");
	printf("   <logical> = Y, N, T, F\n");
	printf("\n");
#ifdef unix
	printf("Examples:\n");
	printf("   wusage set inlib=mylib              SET INLIB  = \"MYLIB   \".\n");
	printf("   shvar=`wusage extract runvol`       EXTRACT RUNVOL into shell variable.\n");
	printf("\n");
#endif /* unix */
#endif /* !VMS */

#ifdef VMS
              /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	      /*         1         2         3         4         5         6         7         8*/
	printf("\n");
	printf("wusage:    Allows setting and extracting of usage constants.\n");
	printf("\n");
	printf("   wusage shell                        Enter full screen HELP mode.\n");
	printf("   wusage read [file]                  Load usage constants from PERSONALITY.\n");
	printf("   wusage write [file]                 Save usage constants to PERSONALITY.\n");
	printf("   wusage set <item>=<value>           Set a usage constant.\n");
	printf("   wusage extract <value>=<item>       Extract a usage constant.\n");
	printf("   wusage flags                        Enter full screen flag setting mode.\n");
	printf("   wusage flags set <flag>=<logical>   Set one flag in the bit mask.\n");
	printf("   wusage version                      Print the WISP VERSION info.\n");
	printf("\n");
	printf("   <item> = INLIB,INVOL,OUTLIB,OUTVOL,RUNLIB,RUNVOL,SPOOLIB,SPOOLVOL,\n");
	printf("            WORKLIB,WORKVOL,PROGLIB,PROGVOL,PRNTMODE,PRTCLASS,FORM#,PRINTER,\n");
	printf("            JOBQUEUE,JOBCLASS,JOBLIMIT,FLAGS,USERID,LUSERID,LINES\n");
	printf("   <flag> = HELP,SETFILE,SETPRINT,SETSUB,QUEUES,SBQ,SGQ,SOQ,SALLJ,SYSTEM,\n");
	printf("            UTILS,DISPLAY,EDIT,DISPRINT,KCSI,TERMINAL,PSEUDO,CURSOR,SCREEN,\n");
	printf("            COMMANDS,SAVE,PRINTSCR,CANCEL\n");
	printf("<logical> = Y, N, T, F\n");
	printf("\n"); 
#endif

	return(1);
}
