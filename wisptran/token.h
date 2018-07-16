			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		token.h
**
**	Purpose:	To define the TOKEN structure.
**
**
**	History:
**	05/08/93	Written by GSL
**
*/

#ifndef TOKEN_H
#define TOKEN_H

struct Token
{
	int		type;
#define 	UNKNOWN		0
#define 	KEYWORD		1
#define 	LITERAL		2
#define 	NUMBER		3
#define 	IDENTIFIER	4
#define 	PERIOD		5
#define 	CONTINUATION	6
#define 	OPERATOR	7
#define 	PICTURE		8
#define 	PUNCT		9
#define 	LINECODE	10
#define 	MODCODE		11
#define 	PCOMMENT	12
#define		VERB		13
#define		COMMENT		14
#define		NOPROCESS	15
#define		IDCOMMENT	16
#define		SCOMMENT	17
	void		*context;
	int		absline;
	int		line;
	int		column;
	int		column_fixed;
	char		*indata;
	char		*data;
};
typedef struct Token TOKEN;

char *token_data();
char *token_type_mess();
TOKEN *dup_token();
TOKEN *edit_token();
TOKEN *make_token();
TOKEN *get_token();

#endif /* TOKEN_H */
