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

extern NODE get_statement(void);
extern NODE get_statement_fragment(void);
extern NODE make_statement(char *line, void *the_context);
extern NODE make_clause(int column, char *clause, void *the_context);
extern NODE first_non_fluff_node(NODE the_tree);
extern NODE first_node_with_token(NODE the_tree);
extern NODE free_statement(NODE start);
extern NODE delint_statement(NODE start);
extern NODE strip_statement(NODE start);
extern NODE decontext_statement(NODE start);
extern NODE get_sentence_tree(void);
extern NODE get_statement_from_sentence(NODE the_sentence);
extern NODE unget_statement_from_sentence(NODE the_statement, NODE the_sentence);
extern NODE unhook_next_fragment(NODE the_statement, NODE curr_node);
extern NODE find_period_node(NODE the_statement);
extern NODE next_non_fluff_node_in_sentence(NODE the_tree, NODE curr_node);
extern NODE last_non_fluff_node(NODE the_tree);
extern NODE unhook_trailing_fluff(NODE the_tree);

#endif /* STATMENT_H */
/*
**	History:
**	$Log: statment.h,v $
**	Revision 1.7  1998-03-03 16:08:18-05  gsl
**	updated
**
**	Revision 1.6  1998-02-10 15:08:54-05  gsl
**	update with new routines
**
**	Revision 1.5  1996-08-30 21:56:10-04  gsl
**	drcs update
**
**
**
*/
