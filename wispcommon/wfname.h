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

#endif /* WFNAME_H */
/*
**	History:
**	$Log: wfname.h,v $
**	Revision 1.6  1996/07/23 18:17:57  gsl
**	drcs update
**	
**
**
*/
