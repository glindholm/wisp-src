static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include definitions.								*/

#ifdef WIN32
#include <windows.h>
#endif
#include "video.h"
#include "vlocal.h"
#include "vintdef.h"
#include "vdata.h"
#include "vmodules.h"
#include "vraw.h"

/*						Subroutine entry point.								*/

vbell()
{
	enum e_vop temp;								/* Working register.			*/

	vdefer_restore();								/* Bell is cause end deferred action.	*/
	temp = voptimize(VOP_OFF);							/* Remember the optimization level.	*/

#ifdef MSDOS
	vcontrol_flush();								/* Dump the buffer.			*/
	printf("\007");									/* Output a bell character.		*/
	fflush( stdout );
#endif
#ifdef WIN32
	if (vrawdirectio())
	{
		MessageBeep(MB_ICONASTERISK);
	}
	else
	{
		vcontrol("\007");							/* Output a bell character.		*/
		vcontrol_flush();							/* Dump the buffer.			*/
	}
#endif
#if defined(VMS) || defined(unix)
	vcontrol("\007");								/* Output a bell character.		*/
	vcontrol_flush();								/* Dump the buffer.			*/
#endif	/* VMS and unix */

	voptimize(temp);								/* Remember the optimization level.	*/
	return(SUCCESS);								/* What could go wrong.			*/
}
/*
**	History:
**	$Log: vbell.c,v $
**	Revision 1.15  1999-05-26 10:56:45-04  gsl
**	add vraw.h
**
**	Revision 1.14  1999-03-03 15:28:35-05  gsl
**	Fix to use windows bell
**	Fix to send ^G when using costar
**
**	Revision 1.13  1997-07-08 16:26:19-04  gsl
**	Change to use new video.h interfaces
**
**	Revision 1.12  1996-11-13 20:29:38-05  gsl
**	Use vcontrol_flush()
**
**	Revision 1.11  1996-11-11 15:02:14-08  jockc
**	change -1 to 0xFFFFFFFF in MessageBeep call (as per MS online
**	doc for MessageBeep)
**
**	Revision 1.10  1996-10-11 15:15:58-07  gsl
**	drcs update
**
**
**
*/
