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

extern int keyword(char *the_word, char *the_list[]);
extern int proc_keyword(char *the_word);
extern int brk_keyword(char *the_word);
extern int reserved_keyword(char *the_word);
extern int load_res_keywords(char *filename);
extern int verb_keyword(char *the_word);
extern int cobol_keyword(char *the_word);

#endif /* KEYWORDS_H */
/*
**	History:
**	$Log: keywords.h,v $
**	Revision 1.6  1996-08-30 21:56:05-04  gsl
**	drcs update
**
**
**
*/
