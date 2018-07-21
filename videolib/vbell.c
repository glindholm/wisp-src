/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

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

int VL_vbell()
{
	enum e_vop temp;								/* Working register.			*/

	vdefer_restore();								/* Bell is cause end deferred action.	*/
	temp = voptimize(VOP_OFF);							/* Remember the optimization level.	*/

#ifdef WIN32
	if (vrawdirectio())
	{
		MessageBeep(MB_ICONASTERISK);
	}
	else
	{
		vcontrol("\007");							/* Output a bell character.		*/
		VL_vcontrol_flush();							/* Dump the buffer.			*/
	}
#endif
#if  defined(unix)
	vcontrol("\007");								/* Output a bell character.		*/
	VL_vcontrol_flush();								/* Dump the buffer.			*/
#endif	/* VMS and unix */

	voptimize(temp);								/* Remember the optimization level.	*/
	return(SUCCESS);								/* What could go wrong.			*/
}
/*
**	History:
**	$Log: vbell.c,v $
**	Revision 1.21  2003/01/31 20:18:47  gsl
**	Fix -Wall warnings
**	
**	Revision 1.20  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.19  2002/07/18 21:04:21  gsl
**	Remove MSDOS code
**	
**	Revision 1.18  2002/07/17 21:06:00  gsl
**	VL_ globals
**	
**	Revision 1.17  2002/07/15 20:16:06  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.16  2002/06/26 01:42:49  gsl
**	Remove VMS code
**	
**	Revision 1.15  1999/05/26 14:56:45  gsl
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
**	Use VL_vcontrol_flush()
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
