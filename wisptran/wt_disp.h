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
**	File:		wt_disp.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef WT_DISP_H
#define WT_DISP_H

#ifdef __STDC__
extern NODE parse_display(NODE the_statement);
extern NODE parse_display_and_read(NODE the_statement);
#else
extern NODE parse_display();
extern NODE parse_display_and_read();
#endif

#endif /* WT_DISP_H */
/*
**	History:
**	$Log: wt_disp.h,v $
**	Revision 1.5  1996-08-30 21:56:17-04  gsl
**	drcs update
**
**
**
*/
