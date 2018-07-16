/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/*
**	File:		tokenize.h
**
**	Purpose:	To ...
**
*/

#ifndef TOKENIZE_H
#define TOKENIZE_H

extern int tokenize(char *input_line, int linestatus);
extern int tokenize_cobol_line(char *the_cache, char *input_line, int linestatus, int *iddiv, int *iddiv_mode, 
			       int the_absline, int the_line, void *the_context, int dpcomma, int do_reswords);
extern int a_token_cache_count(char *the_cache);
extern int token_cache_count(void);
extern int a_token_cache_unque(char *the_cache, TOKEN *tokptr);
extern void invalidate_token_cache(void);
extern int token_cache_unque(TOKEN *tokptr);
extern int a_token_cache_get(char *the_cache, int num, TOKEN *tokptr);
extern int token_cache_get(int num, TOKEN *tokptr);
extern TOKEN *get_token(void);
extern int unget_token(TOKEN *tokptr);
extern int hold_token_cache(void);
extern TOKEN *make_token(int the_type, const char *the_data);
extern void clean_token(TOKEN *tokptr);
extern void free_token(TOKEN *tokptr);
extern int eq_token(TOKEN *tokptr, int the_type, const char *the_data);
extern int eq_token_literal(TOKEN *tokptr, const char *the_data_unquoted);
extern TOKEN *edit_token(TOKEN *tokptr, const char *the_data);
extern TOKEN *dup_token(TOKEN *tokptr);
extern int fluff_token(TOKEN *tokptr);
extern int lint_token(TOKEN *tokptr);
extern char *token_data(TOKEN *tokptr);
extern char *token_indata(TOKEN *tokptr);
extern char *token_type_mess(TOKEN *tokptr);

#endif /* TOKENIZE_H */
/*
**	History:
**	$Log: tokenize.h,v $
**	Revision 1.10  2004/10/14 19:51:45  gsl
**	added token_indata() for logging, comments, and errors as it should return the actual text from the file where token_data() may be changed for reserved words etc.
**	
**	Revision 1.9  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.8  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.7  2002/08/12 20:13:51  gsl
**	quotes and literals
**	
**	Revision 1.6  1998/03/23 18:54:53  gsl
**	update
**	
**	Revision 1.5  1996-08-30 21:56:11-04  gsl
**	drcs update
**
**
**
*/
