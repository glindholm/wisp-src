/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	filext.h	This header defines variables that may or may not be defined by the COBOL.
**
**			NOTE: This file has been "hard" included into sub85.c
*/

#ifndef FILEXT_DEF
#define FILEXT_DEF

void getwfilext(char* ptr);
void setwfilext(const char* wispfilext);
void setwispfilext(const char* wispfilext);

void WSETFILEXT(const char* wispfilext);
void WGETFILEXT(char* ptr);

#define WISP_FILE_EXT_SIZE	39

#endif

/*
**	History:
**	$Log: filext.h,v $
**	Revision 1.8.2.1  2002/11/14 21:12:29  gsl
**	Replace WISPFILEXT and WISPRETURNCODE with set/get calls
**	
**	Revision 1.8  1996/07/23 18:17:46  gsl
**	drcs update
**	
**
**
*/
