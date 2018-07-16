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

#include "tokenize.h"

#endif /* TOKEN_H */
/*
**	History:
**	$Log: token.h,v $
**	Revision 1.7  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.6  1996/08/31 01:56:10  gsl
**	drcs update
**	
**
**
*/
