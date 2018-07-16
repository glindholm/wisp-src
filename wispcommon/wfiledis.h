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
**	File:		wfiledis.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	DISPLAY a file
**
*/

#ifndef wfiledis_H
#define wfiledis_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void WL_wfile_disp(void);
int  WL_display_util_getparms(char *filename, int* recsize);
int  WL_utils_in_windows(void);
int  WL_use_internal_display(void);
int  WL_link_display(const char* filepath);
int  WL_internal_display(const char* filepath, int reclen);
const char* WL_custom_display_utility(void);

#endif /* wfiledis_H */

/*
**	History:
**	$Log: wfiledis.h,v $
**	Revision 1.7  2003/02/20 23:14:34  gsl
**	Add OPTIONS get to DISPLAY utility that gets the record size RECSIZE
**	
**	Revision 1.6  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.5  2002/07/10 21:06:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.4  1999/02/23 21:57:26  gsl
**	Move no_windows() to wispcfg.h
**	
**	Revision 1.3  1999-02-23 15:24:40-05  gsl
**	Add no_windows() routine.
**
**	Revision 1.2  1998-05-05 13:34:27-04  gsl
**	Add prototypes for new display frontend routines
**
**	Revision 1.1  1998-04-29 15:31:11-04  gsl
**	Initial revision
**
**
**
*/
