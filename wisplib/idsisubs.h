			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		idsisubs.h
**
**	Purpose:	Headers for idsisubs.c
**
**
**	History:
**	07/20/93	Written by GSL
**
*/

#ifndef idsisubs_H
#define idsisubs_H

char *upper_string();
char *upper_mem();
char *lower_string();
char *lower_mem();
char *compressit();
char *splitpath();
char *splitname();
char *splitext();
char *buildfilepath();

#endif /* idsisubs_H */
