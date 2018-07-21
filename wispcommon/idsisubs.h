/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


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

#define upper_string	WL_upper_string
#define upper_mem	WL_upper_mem
#define lower_string	WL_lower_string
#define lower_mem	WL_lower_mem
#define compressit	WL_compressit
#define upcase		WL_upcase
#define unnull		WL_unnull
#define dispchars	WL_dispchars
#define isspaces	WL_isspaces
#define memccpyx	WL_memccpyx
#define leftjust	WL_leftjust
#define safemove	WL_safemove
#define loadpad		WL_loadpad
#define unloadpad	WL_unloadpad
#define strpos		WL_strpos
#define splitpath	WL_splitpath
#define splitname	WL_splitname
#define splitext	WL_splitext
#define hasext		WL_hasext
#define buildfilepath	WL_buildfilepath
#define numeric2int4	WL_numeric2int4
#define field2int4	WL_field2int4
#define cstr2cobx	WL_cstr2cobx
#define cobx2cstr	WL_cobx2cstr
#define dqw_strcpy	WL_dqw_strcpy
#define dqw_strcat	WL_dqw_strcat


extern char *WL_upper_string(char *str);
extern char *WL_upper_mem(char *str, int cnt);
extern char *WL_lower_string(char *str);
extern char *WL_lower_mem(char *str, int cnt);
extern char *WL_compressit(char *str);
extern int   WL_upcase(char *ptr, int cnt);
extern int   WL_unnull(char *ptr, int cnt);
extern int   WL_dispchars(char *ptr, int cnt);
extern int   WL_isspaces(char *p);
extern int   WL_memccpyx(char *dest, const char *src, char ch, int cnt);
extern void  WL_leftjust(char *ptr, int cnt);
extern void  WL_safemove(char *dest, const char *src, int len);
extern void  WL_loadpad(char *dest, const char *src, int size);
extern void  WL_unloadpad(char *dest, const char *src, int size);
extern int   WL_strpos(const char *src, const char *srch);
extern char *WL_splitpath(const char *filepath);
extern char *WL_splitname(char *filepath);
extern char *WL_splitext(char *filepath);
extern int   WL_hasext(char *filepath);
extern char *WL_buildfilepath(char *dest, const char *path, const char *file);
extern int   WL_numeric2int4(int4 *result, char *source, int sourcelen);
extern int   WL_field2int4(const char *str, int len, int4 *num);
extern void  WL_cstr2cobx(char *dest, const char *src, int size);	/* honor embedded spaces		*/
extern void  WL_cobx2cstr(char *dest, const char *src, int size);
extern void  WL_dqw_strcpy(char *dest, const char *src);
extern void  WL_dqw_strcat(char *dest, const char *src);

#endif /* idsisubs_H */
/*
**	History:
**	$Log: idsisubs.h,v $
**	Revision 1.15  2003/04/21 14:40:28  gsl
**	WL_field2int4() takes a const char*
**	
**	Revision 1.14  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/11 14:34:01  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.12  2002/07/09 04:14:04  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.11  2002/07/02 21:15:39  gsl
**	Rename wstrdup
**	
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
