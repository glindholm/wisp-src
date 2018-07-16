static char copyright[]="Copyright (c) 1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		mfstubs.c
**
**	Purpose:	To hold stub routines that are normally found in the MF rts.
**			This is to allow linking to resolve undefines that are only used
**			when then MF rts is included. 
**			I.e. It is used to build utilities that don't include the cobol rts.
**
**	Routines:	
**	call_mfcobol()		Stub used in LINK normally found in wispmf.c
**
*/

#include "idsistd.h"
#include "werrlog.h"

void call_mfcobol()
{
	werrlog(102,"STUB: call_mfcobol()",0,0,0,0,0,0,0);
	return;
}
/*
**	History:
**	$Log: mfstubs.c,v $
**	Revision 1.2  1998/12/14 16:42:15  gsl
**	Fix history
**	
**
**
*/
