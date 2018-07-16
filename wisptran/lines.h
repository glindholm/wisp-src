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
**	File:		lines.h
**
**	Purpose:	COBOL line handling headers
**
**
**	History:
**	06/01/93	Written by GSL
**
*/

#ifndef LINES_H
#define LINES_H

#define NORMAL_LINE	0
#define PROCESS_LINE	1
#define	NOPROCESS_LINE	2
#define SPECIAL_COMMENT 3

int	get_cobol_inline();
int	get_cobol_line();
int	get_next_cobol_line();
int	get_conditional_cobol_line();
int	get_clean_cobol_line();
char 	*wfgets();

#endif /* LINES_H */
/*
**	History:
**	$Log: lines.h,v $
**	Revision 1.5  1996/08/31 01:56:05  gsl
**	drcs update
**	
**
**
*/
