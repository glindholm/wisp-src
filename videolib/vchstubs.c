			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#define __CHINESE__
#include <stdio.h>
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
vlanguage(path)
char *path;
{
}
