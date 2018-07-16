/* 
	Copyright (c) 1995-1998 NeoMedia Technologies, Inc. All rights reserved.
	$Id:$
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

int runtype(const char* filespec);

#define ACCERR -1
#define ISEXEC 	0
#define NOTEXEC 1
#define ISACU 	2
#define ISMFINT	3

int isexec(const char* filespec);

#endif


/*
**	History:
**	$Log: runtype.h,v $
**	Revision 1.8.2.1  2002/08/20 17:56:39  gsl
**	Add support for Micro Focus Shared Object files .so/.sl
**	V4_4_04
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
