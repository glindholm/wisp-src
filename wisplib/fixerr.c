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

#ifdef unix

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


int fixerr(code)
int code;
{
	switch (code)
	{
		case EPERM: case EACCES:	
				return WPERM;
		case ENOENT: 	return WFNOTFOUND;
		case EIO:	return WIOERR;
		case ENXIO:	return WNOTMTD;
		case ENOEXEC:	return WINVPROG;
		case EAGAIN:	return WLEXCEED;
		case ENOMEM:	return WNOMEM;
		case EBUSY:	return WVOLBUSY;
		case ENOTDIR:	return WDNOTFOUND;
		default:	return code;
	}
}

#endif



