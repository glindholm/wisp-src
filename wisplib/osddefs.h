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
**	File:		osddefs.h
**
**	Project:	wisp
**
**	RCS:		$Source:$
**
**	Purpose:	Operating System Dependant defines
**
*/

#ifndef osddefs_H
#define osddefs_H

#ifdef unix
#define FUNC_UNAME
#endif

#ifdef WIN32
#define FOPEN_READ_BINARY	"rb"
#define FOPEN_WRITE_BINARY	"wb"
#define FOPEN_READ_TEXT		"r"
#define FOPEN_WRITE_TEXT	"w"
#else
#define FOPEN_READ_BINARY	"r"
#define FOPEN_WRITE_BINARY	"w"
#define FOPEN_READ_TEXT		"r"
#define FOPEN_WRITE_TEXT	"w"
#endif

#endif /* osddefs_H */

/*
**	History:
**	$Log: osddefs.h,v $
**	Revision 1.4  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.3  2002/07/25 17:03:43  gsl
**	MSFS->WIN32
**	
**	Revision 1.2  1996/07/11 23:20:55  gsl
**	Share code with msdos for NT
**	
**	Revision 1.1  1995-09-25 10:06:29-07  gsl
**	Initial revision
**
**
*/
