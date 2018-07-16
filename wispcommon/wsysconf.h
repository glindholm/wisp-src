/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

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
#ifdef MSFS
#define CFGFNAME "WSYSCONF.CFG"
#else	/* VMS & unix */
#define CFGFNAME "wsysconfig"
#endif

/*
**	History:
**	$Log: wsysconf.h,v $
**	Revision 1.9  1996-07-23 14:18:01-04  gsl
**	drcs update
**
**
**
*/
