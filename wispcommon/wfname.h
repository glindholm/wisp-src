/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wfname.h
**
**	Purpose:	Header for wfname.c
**
**
**	History:
**	08/23/94	Written by GSL
**
*/

#ifndef WFNAME_H
#define WFNAME_H

#include "intdef.h"

extern char *wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);
extern int wlgtrans( char *in_str, char *out_str );
extern void logworklib( char *worklib );
extern int wfexists(char *file, char *lib, char *vol);

void wfopen3(						/* WISP 3.0 and later			*/
	     int4 *mode,				/* the mode of opening			*/
	     char *vol,					/* the WANG volume name	(6 chars)	*/
	     char *lib,					/* The WANG library name (8 chars)	*/
	     char *file,				/* The file name	(8 chars)	*/
	     char *name,				/* The resultant name			*/
	     const char *appl,				/* The COBOL program id (8 chars)	*/
	     const char *prname,			/* The PRNAME 				*/
	     const int4 *openmode);			/* The open mode			*/

void wfopen2(						/* WISP 2.0C and later			*/
	int4 *mode,					/* the mode of opening			*/
	char *vol,					/* the WANG volume name	(6 chars)	*/
	char *lib,					/* The WANG library name (8 chars)	*/
	char *file,					/* The file name	(8 chars)	*/
	char *cob_name,					/* The resultant name			*/
	const char *appl,				/* The COBOL program id (8 chars)	*/
	const char *prname);				/* The PRNAME 				*/

void wfopen(						/* WISP 2.0B and earlier		*/
	int4 *mode,					/* the mode of opening			*/
	char *vol,					/* the WANG volume name	(6 chars)	*/
	char *lib,					/* The WANG library name (8 chars)	*/
	char *file,					/* The file name	(8 chars)	*/
	char *name,					/* The resultant name			*/
	const char *prname);				/* The PRNAME (optional).		*/

#endif /* WFNAME_H */
/*
**	History:
**	$Log: wfname.h,v $
**	Revision 1.6.2.1  2002/11/12 16:00:19  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.6  1996/07/23 18:17:57  gsl
**	drcs update
**	
**
**
*/
