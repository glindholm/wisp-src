static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <ctype.h>
#include "strtkn.h"
#include "itkn.h"
#include "iglb.h"
#include "kcsifunc.h"

/*----
Modified to allow NOT operators.
6/21/92 Allowed ',' as a field separator.
------*/

static char sccsid[]="@(#)itkn.c	1.4 8/3/93";


static int which_tkn(char *str);
static int not_inq_token();
static int isnumeric(char *str);
static int isquoted(char *str);
static int isoccurs(char *str);
static int isdelimited(char *str,char *delim);


/*----
Init the indexes and then get the next token
------*/
int first_inq_tkn()
{
	sline_idx = sidx = 0;
	return(next_inq_tkn());
}

/*----
Return a token type in token and the value extracted into tokens
------*/
int next_inq_tkn()
{
	int ch,skipblanks,matchquote,isinquotes;

/* Clear the token value and string */
	inq_token = NO_TKN;
	inq_token_idx =0;
	strcpy(inq_tokens,"");

/* Set flag to skip initial blanks and no in quotes*/
	skipblanks = 1;
	matchquote = isinquotes = 0;

/*
 * Pull character by character until word or the field is exhausted
 * If the field is exhausted with no token then continue
 * If all fields are exhausted with no token then return LAST_TKN
 */
	while(1)
		{
/* Stop if all fields are exhausted */
		if(sline_idx >= INQ_FIELD_COUNT)
			{
			break;
			} 
/*
 * If we hit the end of the line and something has been extracted
 * then break to process it, otherwise carry on with the next line.
 */
		if(sidx >= INQ_FIELD_LEN)
			{
			sidx = 0;
			++sline_idx;
			if(inq_token_idx != 0)
				break;
			else
				continue;
			}
		ch = inq_inqs[(sline_idx * INQ_FIELD_LEN) + sidx];
		++sidx;
/*
 * If the received character is a blank or comma then it is either a
 * separator before the next token in which case we plough on, or
 * inside quotes so we store it or a
 * separator after a token which signals the end of a complete token.
 */
		if( (ch == ' ') || (ch == ','))
			{
			if(matchquote)
			    ;		    /* Fall through and save it */
			else
			if(skipblanks)
				continue;   /* Skip it */
			else
				break;	    /* At end */
			}
/*
 * Break for an opening left paren if not in a quote or the first
 * character of the field. Back the index up so that the paren becomes
 * the first character of the next entry. This problem occurs on
 * fields where the occurs is jammed up AX(1) etc.
 */
		if (ch == '(')
			{
			if((!matchquote) && (inq_token_idx > 0))
				{
				--sidx;
				break;
				}
			}
/*
 * The selected character is not a blank. If it is a quote at the 
 * start of the field then
 * prepare to match quotes and turn off the skip flag.
 */
		if((ch == '\'') ||( ch == '\"'))
			{
			if(matchquote == ch)
				{
				matchquote = 0;
				isinquotes = 1;
				}
			else
			if( ( matchquote == 0 ) && (inq_token_idx == 0) )
				matchquote = ch;
			}
		inq_tokens[inq_token_idx++] = ch;
		skipblanks = 0;
		
		}
/* If no word was extracted, then we are at the end of the list */
	if(inq_token_idx == 0)
		return(inq_token = LAST_TKN);

/* Terminate the extracted token */
	inq_tokens[inq_token_idx] = 0;

/* Now we try to identify it */
	if (! isinquotes)
		strupr(inq_tokens);
	inq_token = which_tkn(inq_tokens);
/* If we got a NOT then we must pull the next token */
	if(inq_token == NOT_TKN)
	    return(not_inq_token());
/* If we got anything, then we can return it */
	if(inq_token != -1)
		return(inq_token);
/* Otherwise we have to figure out what it is */
	if(isnumeric(inq_tokens))
		return(inq_token = NUM_TKN);
	if(isquoted(inq_tokens))
		return(inq_token = LIT_TKN);
	if(isoccurs(inq_tokens))
		return(inq_token = OCC_TKN);

/*
 * At this point, the token is assumed to be the name of a field.
 * Check for certain invalid conditions and return.
 */
/*
	if (strlen(inq_tokens) > 8)
		return(inq_token = INVALID_TKN);
	if ((inq_tokens[0] < 'A') || (inq_tokens[0] > 'Z'))
		return(inq_token = INVALID_TKN);
*/
	if (strlen(inq_tokens) > 31)
		return(inq_token = INVALID_TKN);
	return(inq_token = FIELD_TKN);
}
/*----
This routine assumes that a 'NOT' has been found, and the rest of the operator
will be pulled.
------*/
static int not_inq_token()
{
    char not_string[31];

    next_inq_tkn();
/* Only operators are valid */
    if(! (inq_token & OPERATOR_TKN) )
	return(inq_token = INVALID_TKN);
/* NOT NOT is invalid just because I resist that sort of silliness */
    if (inq_token == NOT_TKN)
	return(inq_token = INVALID_TKN);
/* Invert any other operators */
    switch(inq_token)
	{
	case EQ_TKN:
	    inq_token = NEQ_TKN;
	    break;
	default:
	case NEQ_TKN:
	    inq_token = EQ_TKN;
	    break;
	case LE_TKN:
	    inq_token = GT_TKN;
	    break;
	case LT_TKN:
	    inq_token = GE_TKN;
	    break;
	case GE_TKN:
	    inq_token = LT_TKN;
	    break;
	case GT_TKN:
	    inq_token = LE_TKN;
	    break;
	}
/* The token has been resolved, but we also need a token string */
    sprintf(not_string,"NOT %s",inq_tokens);
    strcpy(inq_tokens,not_string);
    return(inq_token);
}


/*----
This is a list of legal tokens. The order for equality operators is
important. When the output is generated the token value is used to 
search this list for the first matching string which is used in the output.
In other words lookup_strtkn_str for NEQ_TKN will return "NE"
GE_TKN will return "GE" as these are the first entries for those
respective tokens.
------*/

STRTKN op[]={
	{"LIST",LIST_TKN},
	{"PRINT",LIST_TKN},
	{"SHOW",LIST_TKN},
	{"DISPLAY",LIST_TKN},
	{"SELECT",LIST_TKN},
	{"EXTRACT",LIST_TKN},
	{"FIND",LIST_TKN},
	{"WRITE",LIST_TKN},
	{"GIVE",LIST_TKN},
	{"EQ",EQ_TKN},
	{"=",EQ_TKN},
	{"NE",NEQ_TKN},
	{"<>",NEQ_TKN},
	{"LT",LT_TKN},
	{"<",LT_TKN},
	{"GT",GT_TKN},
	{">",GT_TKN},
	{"LE",LE_TKN},
	{"<=",LE_TKN},
	{"=<",LE_TKN},
	{"GE",GE_TKN},
	{">=",GE_TKN},
	{"=>",GE_TKN},
	{"WHEN",IF_TKN},
	{"WHENEVER",IF_TKN},
	{"IF",IF_TKN},
	{":",IF_TKN},
	{";",IF_TKN},
	{"AND",AND_TKN},
	{"&",AND_TKN},
	{"OR",OR_TKN},
	{"NOT",NOT_TKN},
/*
 * NOT followed by legal operators. The versions that contain a space followed
 * by a legal operator will have been built by the not_inq_token() routine.
 * versions without spaces will be picked up by standard token parsing.
 */
	{"NOT EQ",NEQ_TKN},
	{"NOT =",NEQ_TKN},
	{"NOT=",NEQ_TKN},
	{"NOT NE",EQ_TKN},
	{"NOT <>",EQ_TKN},
	{"NOT<>",EQ_TKN},
	{"NOT LT",GE_TKN},
	{"NOT <",GE_TKN},
	{"NOT<",GE_TKN},
	{"NOT GT",LE_TKN},
	{"NOT >",LE_TKN},
	{"NOT>",LE_TKN},
	{"NOT LE",GT_TKN},
	{"NOT <=",GT_TKN},
	{"NOT =<",GT_TKN},
	{"NOT=<",GT_TKN},
	{"NOT<=",GT_TKN},
	{"NOT GE",LT_TKN},
	{"NOT >=",LT_TKN},
	{"NOT =>",LT_TKN},
	{"NOT>=",LT_TKN},
	{"NOT=>",LT_TKN},

	{"",-1}		/*<--- end of the list */
	};

static int which_tkn(char *str)
{
	return(lookup_strtkn_tkn(&op[0],str));
}
char *get_inq_tkn_str(int tkn)
{
	return(lookup_strtkn_str(&op[0],tkn));
}

/*----
A value is numeric if all characters are legal numerics and it
contains no more than one sign or one decimal.
----*/
static int isnumeric(char *str)
{
	int sign,decimal;

	sign = decimal = 0;
	while(*str)
		{
		switch(*str)
			{
			case '+':
			case '-':
				if(sign)
					return(0);
				else
					++sign;
			case '.':
				if(decimal)
					return(0);
				else
					++decimal;
				break;
			default:
				if(isdigit(*str))
					break;
				else
					return(0);
			}
		++str;
		}
	return(1);
}

/*----
Qualifies if starting and ending char are both ' or "
------*/
static int isquoted(char *str)
{

	if ((isdelimited(str,"\'\'")) || (isdelimited(str,"\"\"")))
		return(1);
	return(0);
}

/*----
Test if the field starts and ends with delim[0] and delim[1]
------*/
static int isdelimited(char *str,char *delim)
{
	int len,openquote,closequote;
	len = strlen(str);
/* Can't be if it's too short */
	if(len < 2)
		return(0);
	openquote = *str;
	closequote = str[len-1];
	if((openquote == delim[0]) && (closequote == delim[1]))
		return(1);
	return(0);
}

/*----
Test for (1) or (01) thru (99).
------*/
static int isoccurs(char *str)
{
	int len;

	len = strlen(str);
	if(len > 4)
		return(0);
	if (! isdelimited(str,"()"))
		return(0);
	if( ! isdigit(str[1]))
		return(0);
	if(len > 3)
		{
		if(! isdigit(str[2]))
			return(0);
		}
	return(1);
}

char* strupr(char* str)
{
	char *rc;
	rc = str;

	while(*str)
		{
		*str = toupper(*str);
		++str;
		}
	return rc;
}

/*
**	History:
**	$Log: itkn.c,v $
**	Revision 1.2  1996-09-17 19:45:39-04  gsl
**	drcs update
**
**
**
*/
