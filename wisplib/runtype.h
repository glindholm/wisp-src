/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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


/*
**	File:		runtype.h
**
**	Purpose:	To define types for runtype() and isexec().
**
**
**
*/

#ifndef runtype_H
#define runtype_H

#define		RUN_UNKNOWN	-2
#define		RUN_ACCESS	-1
#define		RUN_NOT		0

#define		RUN_EXEC	1
#define		RUN_ACUCOBOL	2
#define		RUN_MF		3
#define		RUN_SHELL	5
#define		RUN_PROC	6
#define		RUN_PROCOBJ	7

int WL_runtype(const char* filespec);

#define ACCERR -1
#define ISEXEC 	0
#define NOTEXEC 1
#define ISACU 	2
#define ISMFINT	3

int WL_isexec(const char* filespec);

#endif


/*
**	History:
**	$Log: runtype.h,v $
**	Revision 1.11  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.10  2002/08/20 16:09:49  gsl
**	Add support for Micro Focus Shared Object files .so/.sl
**	
**	Revision 1.9  2002/07/10 21:05:23  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.8  1998/10/14 20:09:22  gsl
**	Added the isexec() proto and it's defined return values
**	
**	Revision 1.7  1998-03-09 13:41:28-05  gsl
**	u[date
**
**	Revision 1.6  1996-08-19 18:32:51-04  gsl
**	drcs update
**
**
**
*/
