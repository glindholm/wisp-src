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
**	File:		statment.h
**
**	History:
**	07/22/94	Written by GSL
**
*/

#ifndef STATMENT_H
#define STATMENT_H

#include "node.h"

#ifdef __STDC__
extern NODE get_statement(void);
extern NODE get_verb_statement(void);
extern NODE make_statement(char *line, void *the_context);
extern NODE make_clause(int column, char *clause, void *the_context);
extern NODE first_node_with_token(NODE the_tree);
extern NODE free_statement(NODE start);
extern NODE delint_statement(NODE start);
extern NODE strip_statement(NODE start);
extern NODE decontext_statement(NODE start);
#else
extern NODE get_statement();
extern NODE get_verb_statement();
extern NODE make_statement();
extern NODE make_clause();
extern NODE first_node_with_token();
extern NODE free_statement();
extern NODE delint_statement();
extern NODE strip_statement();
extern NODE decontext_statement();
#endif

#endif /* STATMENT_H */
/*
**	History:
**	$Log: statment.h,v $
**	Revision 1.5  1996-08-30 21:56:10-04  gsl
**	drcs update
**
**
**
*/
