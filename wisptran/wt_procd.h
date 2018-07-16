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
**	File:		wt_procd.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef WT_PROCD_H
#define WT_PROCD_H

#ifdef __STDC__
extern void procedure_division(NODE the_statement);
extern NODE parse_verb_statement(NODE the_statement);
extern void add_perf(char *the_name);
extern NODE parse_stop(NODE the_statement);
extern NODE parse_exit(NODE the_statement);
extern NODE parse_move(NODE the_statement);
extern NODE parse_set(NODE the_statement);
#else
extern void procedure_division();
extern NODE parse_verb_statement();
extern void add_perf();
extern NODE parse_stop();
extern NODE parse_exit();
extern NODE parse_move();
extern NODE parse_set();
#endif


#endif /* WT_PROCD_H */
/*
**	History:
**	$Log: wt_procd.h,v $
**	Revision 1.5  1996-08-30 21:56:23-04  gsl
**	drcs update
**
**
**
*/
