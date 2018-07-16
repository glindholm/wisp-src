static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	fixerr.c
*/

#include "idsistd.h"

#include <errno.h>

#define WNOTMTD 	4
#define WVOLBUSY	8
#define WDNOTFOUND 	16
#define WFNOTFOUND 	20
#define WLEXCEED 	24
#define WPERM 		28
#define WIOERR 		44
#define WINVPROG 	52
#define WNOMEM 		60


int fixerr(int code)
{
	switch (code)
	{
/*
**	Watcom does not support EBUSY, ENOTDIR, EPERM, ETXTBSY
**	EROFS and EFAULT error return code
*/
#ifndef WATCOM
		case EPERM: 
		case EACCES:	
				return WPERM;
		case EBUSY:	return WVOLBUSY;
#endif
		case ENOENT: 	return WFNOTFOUND;
		case EIO:	return WIOERR;
		case ENXIO:	return WNOTMTD;
		case ENOEXEC:	return WINVPROG;
		case EAGAIN:	return WLEXCEED;
		case ENOMEM:	return WNOMEM;
		case ENOTDIR:	return WDNOTFOUND;
		default:	return code;
	}
}




/*
**	History:
**	$Log: fixerr.c,v $
**	Revision 1.10  1996/08/19 22:32:21  gsl
**	drcs update
**	
**
**
*/
