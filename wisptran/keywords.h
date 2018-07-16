/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
**	File:		keywords.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef KEYWORDS_H
#define KEYWORDS_H

extern int is_change_word(const char *the_word);
extern void load_change_words(const char *filename);
extern char* get_change_word(char* new_word, const char *orig_word);
extern void add_change_word(const char* new_word);
extern void do_change_word_token(TOKEN *the_token);

extern int verb_keyword(const char *the_word);
extern int cobol_keyword(const char *the_word);
extern int dd_keyword(const char *the_word);
extern int is_symbol(const char *name);
extern void add_user_symbol(const char* name);

#endif /* KEYWORDS_H */
/*
**	History:
**	$Log: keywords.h,v $
**	Revision 1.9  1999/09/07 14:34:07  gsl
**	Fix prototype of load_change_words)(
**	
**	Revision 1.8  1998-03-27 10:35:39-05  gsl
**	Change the reserved_keywords routines to change_word
**
**	Revision 1.7  1998-03-23 13:53:57-05  gsl
**	update with new routines
**
**	Revision 1.6  1996-08-30 21:56:05-04  gsl
**	drcs update
**
**
**
*/
