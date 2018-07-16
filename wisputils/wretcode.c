static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wretcode.c
**
**	Project:	wisp/utils
**
**	RCS:		$Source:$
**
**	Purpose:	called by shell to get 3 digit status code back from COBOL
**
**	Routines:	
*/

#ifdef unix

/*
**	Includes
*/

#include <stdio.h>
#include <errno.h>

#define EXT_FILEXT
#include "filext.h"

main()
{
	int retval;
	char mtxt[4];
	
	RETCODE(mtxt);
	if (mtxt[0]==0&&mtxt[1]==0&&mtxt[2]==0) retval=0;
	else
	  if (mtxt[0]==' '&&mtxt[1]==' '&&mtxt[2]==' ') retval=0;
	else
	  retval = (mtxt[0]-0x30) * 100 + (mtxt[1]-0x30) * 10 + (mtxt[2]-0x30);
	printf("%d",retval);	
	
}

#include "wutils.h"

#endif


/*
**	History:
**	$Log: wretcode.c,v $
**	Revision 1.9  1996-07-26 13:49:37-04  gsl
**	Fix undefined
**
**	Revision 1.8  1995-04-25 02:58:58-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.7  1995/04/17  11:51:20  gsl
 * drcs state V3_3_14
 *
 * Revision 1.6  1995/04/07  15:22:13  gsl
 * remove the filext.h -- was the wrong solution.
 *
 * Revision 1.5  1995/04/07  14:34:53  gsl
 * add include of filext.h
 *
**
**
*/
