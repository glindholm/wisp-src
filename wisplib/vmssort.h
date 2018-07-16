/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vmssort.h
**
**	Purpose:	Header for vmssort.c
**
**
**	History:
**	10/03/94	Written by GSL
**
*/

#ifndef vmssort_H
#define vmssort_H

#ifdef VMS
#include "idsistd.h"
#include "vssort.h"
extern
void vmssort(char flist[][9], char llist[][9], char vlist[][7], char infilename[][80], int4 infile_count,
		char ofile[], char olib[], char ovol[], char *outfilename, 
		char ofileorg, int4 maxrec, char outrectype, int replace_flag,
		struct select_criteria selinfo[], int4 selcnt, struct key_info keyinfo[], int4 keycnt, int4 stable, 
		int4 *wangcode, int4 *errcode, char *errmess);
#endif /* VMS */

#endif /* vmssort_H */
/*
**	History:
**	$Log: vmssort.h,v $
**	Revision 1.5  1996-08-19 18:33:06-04  gsl
**	drcs update
**
**
**
*/
