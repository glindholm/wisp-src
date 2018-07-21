/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
Tokens returned from the token parser
------*/
#define	INVALID_TKN	-2
#define	LAST_TKN	-1
#define	NO_TKN		0
#define	LIST_TKN	1
#define	IF_TKN		2
#define	OCC_TKN		3
#define	OPERATOR_TKN	0x0100
#define	EQ_TKN		(1|OPERATOR_TKN)
#define	NEQ_TKN		(2|OPERATOR_TKN)
#define	LT_TKN		(3|OPERATOR_TKN)
#define	GT_TKN		(4|OPERATOR_TKN)
#define	LE_TKN		(5|OPERATOR_TKN)
#define GE_TKN		(6|OPERATOR_TKN)
#define NOT_TKN 	(7|OPERATOR_TKN)
#define	OPERAND_TKN	0x200
#define	FIELD_TKN	(1|OPERAND_TKN)
#define	LIT_TKN		(2|OPERAND_TKN)
#define	NUM_TKN		(3|OPERAND_TKN)
#define	CONNECT_TKN	0x400
#define	AND_TKN		(1|CONNECT_TKN)
#define	OR_TKN		(2|CONNECT_TKN)

/*----
These token values are used to indicate the type of token that is expected
next and are not directly returned by next_tkn.
------*/
#define	FIELD_LIST_TKN	100	/* Field only */

#define	INQ_FIELD_LEN	79
#define	INQ_FIELD_COUNT	7
/*
**	History:
**	$Log: itkn.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:10  gsl
**	drcs update
**	
**
**
*/
