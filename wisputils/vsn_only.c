/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
**
**
**
******************************************************************************
*/

/* these routines are needed to allow C programs to link against the ACUCOBOL runtime
   (to access the vision portion only) to prevent conflicts with the main() function
   that is present in runcbl.a.. do not modify
 */

#ifdef INCLUDE_VDISPIDX
#include <stdio.h>
char *Agetenv(name)
char *name;
{
	extern char *getenv();
	return getenv(name);
}
char *Abasename(pathname)
char *pathname;
{
	static char name[256];
	extern char *strrchr();
	char *p;

# ifdef unix	
	p = strrchr(pathname,'/');
# else
	p = strrchr(pathname,'\\');	
# endif
	if (p)
	{
		strcpy(name,p);
		return name;
	}
	else 
	{
		return NULL;
	}
}
#endif
/*
**	History:
**	$Log: vsn_only.c,v $
**	Revision 1.6  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.5  2002/07/18 21:04:23  gsl
**	Remove MSDOS code
**	
**	Revision 1.4  1996/07/23 18:13:03  gsl
**	drcs update
**	
**
**
*/
