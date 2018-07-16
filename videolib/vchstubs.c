static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#define __CHINESE__
#include <stdio.h>
#include <string.h>
#include "vchinese.h"

/* 
 * module: vchinese stubs
 *
 */

xlat_stream(instr, insz, out, outsz, ctx)
unsigned char *instr, *out;
int insz, *outsz;
char **ctx;
{
	memcpy(out,instr,insz);
	*outsz = insz;
	out[insz]=(char)0;
	return 0;
}
void vlanguage(path)
char *path;
{
}
/*
**	History:
**	$Log: vchstubs.c,v $
**	Revision 1.6  1996-10-11 18:16:01-04  gsl
**	drcs update
**
**
**
*/
