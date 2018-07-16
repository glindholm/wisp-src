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
**	File:		prompt.h
**
**	Purpose:	To ...
**
**
**	History:
**	05/27/94	Written by GSL
**
*/

#ifndef PROMPT_H
#define PROMPT_H

#ifdef NOPROTO
extern int prompt_list(/* char *message, char *defstr, char *list, char *help */);
extern int prompt_text(/* char *message, char *defstr, int empty, char *help, char *text */);
extern int prompt_num (/* char *message, char *defstr, char *help, int4 *outnum */);
#else
extern int prompt_list(char *message, char *defstr, char *list, char *help);
extern int prompt_text(char *message, char *defstr, int empty, char *help, char *text);
extern int prompt_num (char *message, char *defstr, char *help, int4 *outnum);
#endif

#endif /* PROMPT_H */
/*
**	History:
**	$Log: prompt.h,v $
**	Revision 1.5  1996-07-23 14:17:49-04  gsl
**	drcs update
**
**
**
*/
