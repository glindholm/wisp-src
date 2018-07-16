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
**	File:		idsisubs.h
**
**	Purpose:	Headers for idsisubs.c
**
**
**	History:
**	07/20/93	Written by GSL
**
*/

#ifndef idsisubs_H
#define idsisubs_H

#include "idsistd.h"

extern char *upper_string(char *str);
extern char *upper_mem(char *str, int cnt);
extern char *lower_string(char *str);
extern char *lower_mem(char *str, int cnt);
extern char *compressit(char *str);
extern int upcase(char *ptr, int cnt);
extern int unnull(char *ptr, int cnt);
extern int dispchars(char *ptr, int cnt);
extern int isspaces(char *p);
extern int memccpyx(char *dest, const char *src, char ch, int cnt);
extern void leftjust(char *ptr, int cnt);
extern void safemove(char *dest, const char *src, int len);
extern void loadpad(char *dest, const char *src, int size);
extern void unloadpad(char *dest, const char *src, int size);
extern int strpos(const char *src, const char *srch);
extern int stredt(char *src, const char *srch, const char *repl);
extern char *splitpath(const char *filepath);
extern char *splitname(char *filepath);
extern char *splitext(char *filepath);
extern int hasext(char *filepath);
extern char *buildfilepath(char *dest, const char *path, const char *file);
extern int numeric2int4(int4 *result, char *source, int sourcelen);
extern int field2int4(char *str, int len, int4 *num);
extern void cstr2cobx(char *dest, const char *src, int size);				/* honor embedded spaces		*/
extern void cobx2cstr(char *dest, const char *src, int size);
extern void dqw_strcpy(char *dest, const char *src);
extern void dqw_strcat(char *dest, const char *src);

#endif /* idsisubs_H */
/*
**	History:
**	$Log: idsisubs.h,v $
**	Revision 1.10  1998/08/03 20:26:53  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**	
**	Revision 1.9  1997-08-18 15:59:27-04  gsl
**	Add const to prototypes where needed
**
**	Revision 1.8  1996-08-23 17:00:29-04  gsl
**	uildfilepath() madepath and file parms const
**
**	Revision 1.7  1996-07-23 11:17:47-07  gsl
**	drcs update
**
**
**
*/
