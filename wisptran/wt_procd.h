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

extern void procedure_division(NODE the_statement);
extern NODE parse_verb_statement(NODE the_statement, NODE the_sentence);
extern void add_perf(char *the_name);
extern NODE first_token_node(NODE the_statement);
extern NODE parse_imperative_statements(NODE the_statement, NODE the_sentence);
extern NODE is_verb_statement(NODE the_statement);

NODE parse_error_clause_verb(NODE the_statement, NODE the_sentence);
NODE parse_simple_verb(NODE the_statement, NODE the_sentence);

NODE is_section(NODE the_sentence);
NODE is_paragraph(NODE the_sentence);



/*
**	External parse routines.
*/

NODE parse_accept(NODE the_statement);
NODE parse_call(NODE the_statement, NODE the_sentence);
NODE parse_close(NODE the_statement);
NODE parse_commit(NODE the_statement, NODE the_sentence);
NODE parse_delete(NODE the_statement, NODE the_sentence);
NODE parse_free(NODE the_statement, NODE the_sentence);
NODE parse_hold(NODE the_statement, NODE the_sentence);
NODE parse_if(NODE the_statement, NODE the_sentence);
NODE parse_open(NODE the_statement);
NODE parse_read(NODE the_statement, NODE the_sentence);
NODE parse_rewrite(NODE the_statement, NODE the_sentence);
NODE parse_search(NODE the_statement, NODE the_sentence);
NODE parse_sort(NODE the_statement, NODE the_sentence);
NODE parse_start(NODE the_statement, NODE the_sentence);
NODE parse_write(NODE the_statement, NODE the_sentence);
NODE parse_rewrite_crt(int crt_num, NODE the_statement, NODE the_sentence);

#endif /* WT_PROCD_H */
/*
**	History:
**	$Log: wt_procd.h,v $
**	Revision 1.6  1998-03-03 16:20:15-05  gsl
**	update
**
**	Revision 1.5  1996-08-30 21:56:23-04  gsl
**	drcs update
**
**
**
*/
