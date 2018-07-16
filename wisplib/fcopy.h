/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		fcopy.h
**
**	Purpose:	Header for function fcopy()
**
**
**	History:	
**	02/15/93	Written by GSL
**
*/

#ifndef FCOPY_H
#define FCOPY_H

int wisp_fcopy(const char* srcfile, const char* dstfile);

#endif /* FCOPY_H */



/*
**	History:
**	$Log: fcopy.h,v $
**	Revision 1.5.2.1  2002/10/09 21:03:00  gsl
**	Huge file support
**	
**	Revision 1.7  2002/10/07 20:54:48  gsl
**	Huge file support
**	
**	Revision 1.6  2002/06/26 01:41:12  gsl
**	Change fcopy() to wisp_fcopy()
**	
**	Revision 1.5  1996/08/19 22:32:19  gsl
**	drcs update
**	
**
**
*/
