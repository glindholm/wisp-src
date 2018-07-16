/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*
**	filext.h	This header defines variables that may or may not be defined by the COBOL.
**
**			NOTE: This file has been "hard" included into sub85.c
*/

#ifndef FILEXT_DEF
#define FILEXT_DEF
void WSETFILEXT(const char* wispfilext);
void WGETFILEXT(char* ptr);

#define WISP_FILE_EXT_SIZE	39

#endif

/*
**	History:
**	$Log: filext.h,v $
**	Revision 1.14  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/29 15:46:55  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.12  2002/07/12 17:17:03  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/06/25 18:18:38  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.10  2002/06/25 17:46:05  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 1.9  2002/06/21 03:12:04  gsl
**	add protos
**	
**	Revision 1.8  1996/07/23 18:17:46  gsl
**	drcs update
**	
**
**
*/
