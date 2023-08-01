/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/*
**	File:		wt_procd.h
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
**	Revision 1.8  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.7  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.6  1998/03/03 21:20:15  gsl
**	update
**	
**	Revision 1.5  1996-08-30 21:56:23-04  gsl
**	drcs update
**
**
**
*/
