/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		runtype.h
**
**	Purpose:	To define types for runtype() and isexec().
**
**
**	History:	
**			07/22/92	Written by GSL
**
*/

#ifndef runtype_H
#define runtype_H

#define		RUN_UNKNOWN	-2
#define		RUN_ACCESS	-1
#define		RUN_NOT		0

#define		RUN_EXEC	1
#define		RUN_ACUCOBOL	2
#define		RUN_MFINT	3
#define		RUN_MFGNT	4
#define		RUN_SHELL	5
#define		RUN_PROC	6
#define		RUN_PROCOBJ	7

int runtype(char* filespec);

#endif


/*
**	History:
**	$Log: runtype.h,v $
**	Revision 1.6  1996-08-19 18:32:51-04  gsl
**	drcs update
**
**
**
*/
