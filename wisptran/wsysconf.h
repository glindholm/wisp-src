			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	RECFMT  defines the wsysconfig record format. If it changes must also change following defines.
*/
#define RECFMT "%03d %2s %-15s %s\n"
#define WCLASS 		4		/* Based on RECFMT */
#define WTYPE 		7		/* Based on RECFMT */
#define WSPECFIL 	23		/* Based on RECFMT */

/*
**	CFGNAME  defines the name of the config file.
*/
#ifdef MSDOS
#define CFGFNAME "wsysconf"
#else	/* NOT #ifdef MSDOS (VMS & unix) */
#define CFGFNAME "wsysconfig"
#endif

