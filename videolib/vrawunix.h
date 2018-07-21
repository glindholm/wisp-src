/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
 * File:	vrawunix.h
 *
 * Author:	david s.a. stine
 *
 * Purpose:	This is the header file for the System V implementation
 *		of the vraw package.
 *
 * Notes:	The client of this package should not have to issue
 *		a call to vraw_getc() -- all calls to this routine
 *		should happen via the vraw_input() and vraw_check() calls.
 *		The access to the vraw_getc() routine is provided for low-level
 *		access to the single-character routine.
 *
 * Usage:	#include "vrawunix.h"
 *
 * Edits:
 * ----------------------------------------------------------------------------
 * 89.05.21	dsa	Created.
 * 89.06.28	dsa	Changed filename to vrawuinx.h. Made input routines
 *			return 'chars' rather than 'int's.
 * 89.07.01	dsa	Removed routine entry points that were not used
 *			by higher layers. Also removed all '_' characters
 *			from the entry point names.
 *
 */
#ifndef _vrawunix_
#define _vrawunix_

#include "vraw.h"

#endif

/*
**	History:
**	$Log: vrawunix.h,v $
**	Revision 1.16  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.15  1997/06/24 14:55:33  gsl
**	This routine has been replaced by a generic vraw.h
**	
**	Revision 1.14  1996-11-13 20:43:39-05  gsl
**	Added vrawflush() now external
**
**	Revision 1.13  1996-10-11 15:16:17-07  gsl
**	drcs update
**
**
**
*/
