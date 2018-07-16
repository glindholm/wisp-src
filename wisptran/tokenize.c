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
**	File:		tokenize.c
**
**	Project:	wisp/tran
**
**	RCS:		$Source:$
**
**	Purpose:	To hold routines that tokenize the input lines.
**
**	Routines:	
**	tokenize()		Tokenize the input line (frontend).
**	tokenize_cobol_line()	Tokenize the input line.
**	init_token()		Initialize a token.
**	clean_token()		Clean up a token.
**	a_init_token_cache()	Initialize the token cache.
**	got_token()		Recieve a token from tokenize().
**	get_token()		Get the next token.
**
**
*/

/*
**	Includes
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define EXT extern
#include "wisp.h"

#include "wmalloc.h"
#include "token.h"
#include "lines.h"
#include "tokenize.h"
#include "ring.h"
#include "input.h"
#include "output.h"
#include "keywords.h"
#include "proto.h"

/*
**	Structures and Defines
*/

#define EQ(c,s) (0==memcmp(c,s,strlen(c)))

/*
**	Globals and Externals
*/

/*
**	Static data
*/

static char *token_cache;							/* The internal token cache.			*/

static char is_alpha[256] = 
{
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
	'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',   0,   0,   0,   0,   0,
	  0, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
	'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0
};
static char is_digit[256] = 
{
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0
};

/*
**	Static Function Prototypes
*/

static int got_token(char *the_cache, TOKEN *tokptr);
static void a_init_token_cache(char *the_cache);
static int a_token_cache_que(char *the_cache, TOKEN *tokptr);
static void init_token(TOKEN *tokptr, int the_absline, int the_line, void *the_context);


/*
**	Routine:	tokenize()
**
**	Function:	To split a line of COBOL source into tokens.
**
**	Description:	The frontend to tokenize_cobol_line() used for the main WISP input stream.
**			This routine sets up and uses token_cache to hold the input_line's tokens.
**
**	Arguments:
**	input_line	The line of COBOL source.
**	linestatus	Flag to indicates special processing of line. (NOPROCESS_LINE,SPECIAL_COMMENT)
**
**	Globals:	
**	token_cache	The pointer to the token cache ring.
**
**	Return:		Number of tokens found.
**
**	Warnings:	See tokenize_cobol_line() warnings.
**
**	History:	
**	05/25/93	Split apart from tokenize_cobol_line(). GSL
**
*/
int tokenize(char *input_line, int linestatus)
{
	static	int	first = 1;
	static	int	iddiv = 1;						/* Not yet finished IDENTIFICATION DIVISION	*/
	static	int	iddiv_mode = 0;						/* Special mode to handle IDCOMMENT tokens	*/
	static	int	do_reswords = 1;

	int	rc;

	if (first)
	{
		first = 0;
		if ((rc = ring_open(&token_cache, sizeof(TOKEN), 50, 5, NULL, 0)))
		{
			write_log("WISP",'F',"RINGOPEN","Unable to open ring [token_cache] rc=%d [%s]",rc,ring_error(rc));
			exit_with_err();
		}
	}

	rc = tokenize_cobol_line(token_cache, input_line, linestatus, &iddiv, &iddiv_mode, 
				in_line_count(), curr_context_infile_count(), get_curr_cob_context(), is_dpcomma(), do_reswords);
	return(rc);
}

/*
**	Routine:	tokenize_cobol_line()
**
**	Function:	To split a line of COBOL source into tokens.
**
**	Description:	This routine scans the input line and builds tokens one at a time.
**			When it has a complete token it calls got_token() passing it the token.
**			The LINECODE and MODCODE are treated as separate tokens except on
**			comment lines.  A % comment token goes from the % to end of line, it
**			does not get transformed into a usable comment line except if the % is
**			in column 7.
**			All input lines are truncated to 80 columns.
**			After an IDENTIFIER is found it is further processed:
**				- a copy is made of token.indata to token.data
**				- it is shifted to uppercase
**				- check against the reserved word list and changed (W- added)
**				- check if a COBOL VERB
**				- check if a COBOL KEYWORD
**			(This processing goes beyond pure tokenizing but is done at this point
**			because we have to test for PICTURE keyword.)
**			When the PICTURE or PIC keyword is found we switch to a special mode
**			for tokenizing the picture which has different rules.
**			The original data is in token.indata, all modifications are in token.data.
**			If the linestatus=NOPROCESS_LINE/SPECIAL_COMMENT flag is given then the
**			line is treated line a COMMENT and is given a type of NOPROCESS/SCOMMENT.
**
**			There is a special mode for tokenizing the IDENTIFICATION DIVISON because
**			it has it's own free-form comments (IDCOMMENT).
**
**			Certain tokens are considered to have fixed columns and when writing out
**			there column location can't vary.  These are COMMENT, LINECODE, CONTINUATION,
**			MODCODE, also NOPROCESS and tokens that start in AREA-A (8-11). Other tokens
**			are not fixed and can be written out anywhere in AREA-B (12-72).
**
**	Arguments:
**	the_cache	The token cache to be used by got_token()
**	input_line	The line of COBOL source.
**	linestatus	Flag to indicates special processing of line. (NOPROCESS_LINE,SPECIAL_COMMENT)
**	iddiv		Flag to do special IDENTIFICATION DIVISION processing (what part of ID DIV are we in)
**	iddiv_mode	Flag to do special IDENTIFICATION DIVISION processing (are we processing IDCOMMENT)
**	the_absline	The absline number to put into the tokens
**	the_line	The line number to put into the tokens
**	the_context	The cobol file context
**	dpcomma		Is DECIMAL-POINT IS COMMA flag set.
**	do_reswords	Translate reserved works.
**
**	Globals:	None
**
**	Return:		Number of tokens found.
**
**	Warnings:	The input_line must be pre-processed to space out all tabs.
**
**			The a_init_token_cache() routine works with got_token() to maintain the token cache.		
**
**	History:	
**	05/12/93	Written by GSL
**	05/17/93	Added IDCOMMENT handling. GSL
**	05/25/93	Split from tokenize() to make more generic. GSL
**	06/01/93	Changed the noprocess argument into linestatus and added support for SPECIAL_COMMENT line. GSL
**	06/10/93	Added DECIMAL-POINT IS COMMA support. GSL
**
*/
int tokenize_cobol_line(char *the_cache, char *input_line, int linestatus, int *iddiv, int *iddiv_mode, 
			int the_absline, int the_line, void *the_context, int dpcomma, int do_reswords)
{
	static int	picture_mode = 0;
	static int	continued_literal = 0;
	register char *ptr;
	char	*start, *end;
	int	col, startcol;
	TOKEN	tmptoken;
	char	temp[256];
	int	tokcnt;
	char	decimal_point;

	debug3_print_line(input_line,the_absline,the_line,the_context);

	a_init_token_cache(the_cache);

	decimal_point = (dpcomma) ? ',':'.';

	/*
	**	Pad out the line to 80 characters.
	*/
	memset(temp,' ',80);
	memcpy(temp,input_line,strlen(input_line));
	temp[80] = (char)0;

	while((ptr = strchr(temp,'\n'))) *ptr = ' ';				/* Strip trailing newline			*/

	/*
	**	Initialize the ptr and col vars.
	*/
	col = 1;
	ptr = &temp[col-1];
	tokcnt = 0;

	/*
	**	Continued literal check
	*/
	if (continued_literal && '-' != ptr[6])
	{
		if (' ' == ptr[6])
		{
			ptr[6] = '-';
			write_log("WISP",'W',"CONTINUATION","Added missing CONTINUATION character.");
		}
		else
		{
			write_log("WISP",'E',"CONTINUATION","Missing CONTINUATION character.");
		}
	}
	continued_literal = 0;

	/*
	**	Handle a NOPROCESS && SPECIAL_COMMENT lines.
	*/
	if ( NOPROCESS_LINE==linestatus || SPECIAL_COMMENT==linestatus)
	{
		tokcnt++;
		init_token(&tmptoken,the_absline,the_line,the_context);
		tmptoken.type = (NOPROCESS_LINE==linestatus) ? NOPROCESS:SCOMMENT;
		tmptoken.column_fixed = 1;
		tmptoken.column = col;
		tmptoken.indata = (char *)wdupstr(ptr);
		got_token(the_cache,&tmptoken);
		return tokcnt;
	}

	/*
	**	Handle comment lines
	*/
	if ( '%' == ptr[6])
	{
		ptr[6] = '*';
	}

	if ( ' ' != ptr[6] && '-' != ptr[6] )
	{
		tokcnt++;
		init_token(&tmptoken,the_absline,the_line,the_context);
		tmptoken.column_fixed = 1;
		tmptoken.column = col;
		tmptoken.indata = (char *)wdupstr(ptr);

		if ( '*' == ptr[6] || '/' == ptr[6] )
		{
			tmptoken.type = COMMENT;
		}
		else if ( '$' == ptr[6] || 'd' == ptr[6] || 'D' == ptr[6] )
		{
			tmptoken.type = SCOMMENT;
		}
		else
		{
			write_log("WISP",'W',"TOKENIZE","Unrecognized Column 7 indicator [%c], assumed Special COMMENT.",ptr[6]);
			tmptoken.type = SCOMMENT;
		}

		got_token(the_cache,&tmptoken);
		return tokcnt;
	}

	/*
	**	Handle blanklines (treat as a comment line)
	*/
	if (strlen(temp) == strspn(temp," "))
	{
		tokcnt++;
		init_token(&tmptoken,the_absline,the_line,the_context);
		tmptoken.column_fixed = 1;
		tmptoken.column = col;
		tmptoken.indata = (char *)wdupstr(ptr);
		tmptoken.type = COMMENT;
		got_token(the_cache,&tmptoken);
		return tokcnt;
	}
	

	/*
	**	Get LINECODE
	*/
	if (!EQ("      ",ptr))
	{
		tokcnt++;
		init_token(&tmptoken,the_absline,the_line,the_context);
		tmptoken.type = LINECODE;
		tmptoken.column_fixed = 1;
		tmptoken.column = col;
		tmptoken.indata = wmalloc(7);
		memcpy(tmptoken.indata,ptr,6);
		tmptoken.indata[6] = (char)0;
		got_token(the_cache,&tmptoken);
	}

	col += 6;
	ptr += 6;

	/*
	**	Get any continuation char.
	**	(Comments have already been handled so won't be a comment char)
	*/
	if ( '-' == *ptr )
	{
		tokcnt++;
		init_token(&tmptoken,the_absline,the_line,the_context);
		tmptoken.type = CONTINUATION;
		tmptoken.column_fixed = 1;
		tmptoken.column = col;
		tmptoken.indata = wmalloc(2);
		memcpy(tmptoken.indata,ptr,1);
		tmptoken.indata[1] = (char)0;
		got_token(the_cache,&tmptoken);
	}

	col++;
	ptr++;

	/*
	**	Start tokenizing at col 8.
	*/
	start = NULL;
	end = NULL;

	init_token(&tmptoken,the_absline,the_line,the_context);

	while(*ptr && col <= 72)
	{
		startcol = col;

		if ( ' ' == *ptr ) 
		{
			col++;
			ptr++;
		}
		else if (*iddiv_mode && col >= 12)
		{
			/*
			**	If tokenizing the IDENTIFICATION division (after required tokens) and
			**	past AREA A then this is a special comment.
			*/
			start = ptr;
			col += strlen(ptr);
			ptr += strlen(ptr);
			end = ptr;
			tmptoken.type = IDCOMMENT;
			tmptoken.column_fixed = 1;
		}
		else if ( '%' == *ptr )
		{
			start = ptr;
			col += strlen(ptr);
			ptr += strlen(ptr);
			end = ptr;
			tmptoken.type = PCOMMENT;
		}
		else if ( ';' == *ptr || (',' == *ptr && !dpcomma) )
		{
			start = end = ptr;
			tmptoken.type = PUNCT;

			ptr++;
			col++;
		}
		else if ( dpcomma && ',' == ptr[0] && !is_digit[(unsigned char) ptr[1]] ) 
		{
			start = end = ptr;
			tmptoken.type = PUNCT;

			ptr++;
			col++;
		}
		else if (picture_mode && !('I'==toupper(ptr[0]) && 'S'==toupper(ptr[1])) )
		{
			/*
			**	PICTURE [IS] xxxxxx
			**
			**	The picture mode flag is set after the keyword "PICTURE"/"PIC" is found.
			**	This block handles the special tokenizing rules of a picture clause, the only
			**	querk is the optional "IS" keyword must be handled by the IDENTIFIER block.
			*/

			start = ptr;

			while(!end)
			{
				if ( 72 == col)
				{
					end = ptr;
				}
				else if (' ' == ptr[1])
				{
					end = ptr;
				}
				else if (',' == ptr[1] || ';' == ptr[1] || '.' == ptr[1])
				{
					if ( 72-1 == col )
					{
						end = ptr;
					}
					else if ( ' ' ==ptr[2] )
					{
						end = ptr;
					}
					else
					{
						ptr++;
						col++;
					}
				}
				else
				{
					ptr++;
					col++;
				}
			}

			tmptoken.type = PICTURE;
			picture_mode = 0;

			ptr++;
			col++;
		}
		else if (dpcomma && '.' == *ptr)
		{
			start = end = ptr;
			tmptoken.type = PERIOD;

			ptr++;
			col++;
		}
		else if ( DOUBLE_QUOTE == ptr[0] || SINGLE_QUOTE == ptr[0] ||	       		/* QUOTED LITERAL			*/
			( DOUBLE_QUOTE == ptr[1] && strchr("hHxX",ptr[0]) && col < 68 ) )	/* HEX LITERAL				*/
		{
			/*
			**	NON-NUMERIC LITERAL
			**		"...."		- literal
			**		'....'
			**		".......	- continued literal
			**		".."".."	- embedded delimiter
			**		'..''..'
			**		H"00FF"		- hex literal
			**		x"AC"
			*/

			char	delimiter;

			start = ptr;

			if (DOUBLE_QUOTE == ptr[0] || SINGLE_QUOTE == ptr[0])
			{
				delimiter = *ptr;
			}
			else
			{
				/*
				**	HEX literal: 
				**		- delimiter is always a quote
				**		- skip past the lead-in char.
				*/
				delimiter = DOUBLE_QUOTE;
				ptr++;
				col++;
			}

			if ( 72 == col )
			{
				end = ptr;
			}
			else
			{
				ptr++;
				col++;
			}

			while(!end)
			{
				if ( 72 == col )
				{
					end = ptr;
				}
				else if ( delimiter == ptr[0] && delimiter == ptr[1] )
				{
					ptr++;
					col++;
					if ( 72 == col )
					{
						end = ptr;
					}
					else
					{
						ptr++;
						col++;
					}
				}
				else if ( delimiter == ptr[0] && delimiter != ptr[1] )
				{
					end = ptr;
				}
				else
				{
					ptr++;
					col++;
				}
			}

			tmptoken.type = LITERAL;
			continued_literal = ( delimiter == *end ) ? 0 : 1;
			tmptoken.column_fixed = ( delimiter == *end ) ? 0 : 1;

			ptr++;
			col++;

		}
		else if ( is_alpha[(unsigned char)*ptr] || '#' == *ptr || '@' == *ptr ||
			  is_digit[(unsigned char)*ptr] || '-' == *ptr || '+' == *ptr || decimal_point == *ptr )
		{
			/*
			**	NUMBER
			**		123		(dpcomma)
			**		123.45		123,45
			**		.45		,45
			**		+.00		+,00
			**		-123
			**		+123.45		+123,45
			**		45.	(Wrong: decimal must be followed by digit. This is two token "45" ".")
			**		123-	(Wrong: Sign must be leftmost char.)
			**
			**	IDENTIFIER
			**		ABC
			**		000-ABC_000
			**		A-B-	(Wrong: dash or underscore can not be first or last character)
			**		#,@,$   These can be used in COPY filenames which are a special case.
			**		$	Dollar sign can not be the first char of identifier.
			*/
			int	not_number;
			int	found_decimal;

			not_number = (is_alpha[(unsigned char)*ptr] || '#' == *ptr || '@' == *ptr);
			found_decimal = 0;

			start = ptr;

			if ( 72 == col )
			{
				end = ptr;
				if (is_digit[(unsigned char)*ptr])
					tmptoken.type = NUMBER;
				else if (not_number)
					tmptoken.type = IDENTIFIER;
				else if ('-' == *ptr || '+' == *ptr)
					tmptoken.type = OPERATOR;
				else if ('.' == *ptr)
					tmptoken.type = PERIOD;
				else if (',' == *ptr)
					tmptoken.type = PUNCT;
			}
			else if ( decimal_point == *ptr && !is_digit[(unsigned char)ptr[1]] )
			{
				end = ptr;
				tmptoken.type = (dpcomma) ? PUNCT:PERIOD;
			}
			else if ( ('-' == *ptr || '+' == *ptr) && !is_digit[(unsigned char)ptr[1]] && decimal_point != ptr[1])
			{
				/*
				**	A sign must be followed by a digit or decimal point otherwise this sign
				**	is not part of a number but is a separate token.
				*/
				end = ptr;
				tmptoken.type = OPERATOR;
			} 

			if ( decimal_point == *ptr )
			{
				found_decimal = 1;
			}

			while(!end)
			{
				if ( 72 == col )
				{
					end = ptr;
					tmptoken.type = (not_number) ? IDENTIFIER : NUMBER;
				}
				else if ( is_alpha[(unsigned)ptr[1]] || '#' == ptr[1] || '@' == ptr[1] || '$' == ptr[1] )
				{
					not_number = 1;
					ptr++;
					col++;
				}
				else if ( is_digit[(unsigned)ptr[1]] )
				{
					ptr++;
					col++;
				}
				else if ( '-' == ptr[1] || '_' == ptr[1] )
				{
					not_number = 1;
					ptr++;
					col++;
				}
				else if ( decimal_point == ptr[1] && !not_number && !found_decimal )
				{
					if ( 72-1 == col )
					{
						end = ptr;
						tmptoken.type = NUMBER;
					}
					else if ( is_digit[(unsigned)ptr[2]] )
					{
						found_decimal = 1;
						ptr++;
						col++;
					}
					else
					{
						end = ptr;
						tmptoken.type = NUMBER;
					}
				}
				else
				{
					end = ptr;
					tmptoken.type = (not_number) ? IDENTIFIER : NUMBER;
				}
			}

			ptr++;
			col++;

		}
		else if ( col < 72 && (EQ("<=",ptr) || EQ(">=",ptr) || EQ("**",ptr)))
		{
			start = ptr;
			ptr++;
			col++;
			end = ptr;
			tmptoken.type = OPERATOR;

			ptr++;
			col++;
		}
		else if (  '+' == *ptr 
			|| '-' == *ptr
			|| '/' == *ptr
			|| '*' == *ptr
			|| '=' == *ptr
			|| '(' == *ptr
			|| ')' == *ptr
			|| '<' == *ptr
			|| '>' == *ptr
			|| ':' == *ptr )
		{
			start = end = ptr;
			tmptoken.type = OPERATOR;

			ptr++;
			col++;
		}
		else
		{
			/* UNKNOWN */
			start = ptr;

			while(!end)
			{
				if ( 72 == col)
				{
					end = ptr;
				}
				else if (' ' == ptr[1] || ',' == ptr[1] || ';' == ptr[1] || '.' == ptr[1] || '%' == ptr[1] )
				{
					end = ptr;
				}
				else
				{
					ptr++;
					col++;
				}
			}

			tmptoken.type = UNKNOWN;
			picture_mode = 0;

			ptr++;
			col++;
		}

		if (start && end)						/* A token was found				*/
		{
			int	size;

			tokcnt++;
			size = end - start + 1;
			tmptoken.column = startcol;
			if (startcol < 12) tmptoken.column_fixed = 1;
			tmptoken.indata = wmalloc(size+1);
			memcpy(tmptoken.indata,start,size);
			tmptoken.indata[size] = (char)0;

			if (IDENTIFIER == tmptoken.type)
			{
				tmptoken.data = (char *)wdupstr(tmptoken.indata);
				uppercase(tmptoken.data);

				if (do_reswords && is_change_word(tmptoken.data))
				{
					/* Change words */
					char	changed_word[80];

					get_change_word(changed_word, tmptoken.data);
					wfree(tmptoken.data);
					tmptoken.data = (char*)wdupstr(changed_word);
				}
				else if (verb_keyword(tmptoken.data))
				{
					tmptoken.type = VERB;
				}
				else if (cobol_keyword(tmptoken.data))
				{
					tmptoken.type = KEYWORD;
					if (EQ("PIC",tmptoken.data) || EQ("PICTURE",tmptoken.data))
					{
						picture_mode = 1;
					}
				}
			}
			else if (PICTURE == tmptoken.type)
			{
				tmptoken.data = (char *)wdupstr(tmptoken.indata);
				uppercase(tmptoken.data);
			}

			if (*iddiv)
			{
				/*
				**	Special IDENTIFICATION DIVISION processing.
				**
				**	Look for the required tokens:
				**		1              2       3
				**		IDENTIFICATION DIVISON .
				**		4          5 6        7
				**		PROGRAM-ID . Progname .
				**
				**	Once we've seen the 7 required tokens we switch into iddiv_mode.
				**	In this mode we process any AREA A (col 8-11) token normally plus
				**	its following period, then everything else is an IDCOMMENT. 
				**	The iddiv_mode is terminated when we enter another division.
				*/
				switch(*iddiv)
				{
				case 1:
					if (KEYWORD == tmptoken.type && EQ("IDENTIFICATION",tmptoken.data))
					{
						/* Look for DIVISION */
						*iddiv = 2;
					}
					break;
				case 2:
					if (KEYWORD == tmptoken.type && EQ("DIVISION",tmptoken.data))
					{
						/* Look for PERIOD */
						*iddiv = 3;
					}
					break;
				case 3:
					if (PERIOD == tmptoken.type)
					{
						/* Look for PROGRAM-ID */
						*iddiv = 4;
					}
					break;
				case 4:
					if (KEYWORD == tmptoken.type && EQ("PROGRAM-ID",tmptoken.data))
					{
						/* Look for PERIOD */
						*iddiv = 5;
					}
					break;
				case 5:
					if (PERIOD == tmptoken.type)
					{
						/* look for Program name */
						*iddiv = 6;
					}
					break;
				case 6:
					if (IDENTIFIER == tmptoken.type)
					{
						/* Look for PERIOD */
						*iddiv = 7;
					}
					break;
				case 7:
					if (PERIOD == tmptoken.type)
					{
						/* 
						**	Found all the required tokens. 
						**	Enter special mode to handle comments.
						*/
						*iddiv_mode = 1;
						*iddiv = 99;
					}
					break;
				case 99:
					if (KEYWORD == tmptoken.type)
					{
						/* Turn mode off for next token to get the PERIOD */
						*iddiv_mode = 0;
					}
					else if (VERB == tmptoken.type && 0==strcmp(tmptoken.data,"COPY"))
					{
						/* Turn mode off for COPY verb */
						*iddiv_mode = 0;
					}
					else if (PERIOD == tmptoken.type)
					{
						*iddiv_mode = 1;
					}
					break;
				}

				if (KEYWORD == tmptoken.type && (
					EQ("ENVIRONMENT",tmptoken.data) ||
					EQ("DATA",tmptoken.data) ||
					EQ("PROCEDURE",tmptoken.data) ))
				{
					/*
					**	We are now out of the IDENTIFICATION DIVISION.
					*/
					*iddiv = 0;
					*iddiv_mode = 0;
				}
			}

			got_token(the_cache,&tmptoken);

			start = end = NULL;
			init_token(&tmptoken,the_absline,the_line,the_context);
		}

	}

	if (*ptr && col == 73)
	{
		if (!EQ("        ",ptr))
		{
			tokcnt++;
			tmptoken.type = MODCODE;
			tmptoken.column_fixed = 1;
			tmptoken.column = col;
			tmptoken.indata = wmalloc(9);
			memcpy(tmptoken.indata,ptr,8);
			tmptoken.indata[8] = (char)0;
			got_token(the_cache,&tmptoken);
		}
	}

	return tokcnt;
}

/*
**	Routine:	got_token()
**
**	Function:	To recieve a token found by tokenize().
**
**	Description:	This routine is called by tokenize() when it has a token.
**			It will message the token a little and fill in some missing info
**			then add the token to the token_cache ring.
**
**			It makes sure that token.data contains the data to use by switching
**			pointers with token.indata if required.
**			It strips trailing spaces off of COMMENTS.
**
**	Arguments:
**	the_cache	The pointer to the token cache ring.
**	tokptr		Pointer to the token.
**
**	Globals:	
**
**	Return:		None
**
**	Warnings:	a_init_token_cache() must have been called first to setup the token_cache ring.
**
**	History:	
**	05/12/92	Written by GSL
**
*/
static int got_token(char *the_cache, TOKEN *tokptr)
{

	if (tokptr->indata && !tokptr->data)
	{
		tokptr->data = tokptr->indata;
		tokptr->indata = NULL;
	}

	if (COMMENT == tokptr->type || PCOMMENT == tokptr->type || IDCOMMENT == tokptr->type || SCOMMENT == tokptr->type)
	{
		char	*ptr;
		for(ptr=tokptr->data+strlen(tokptr->data)-1; *ptr == ' '; ptr--)
		{
			*ptr = (char)0;
		}
	}

	debug3_print_token(tokptr);

	a_token_cache_que(the_cache,tokptr);

	init_token(tokptr,0,0,NULL);

	return 0;
}


/*
**	Routine:	a_init_token_cache()
**
**	Function:	To initialize the token cache.
**
**	Description:	This routine will clean up the token cache.
**
**	Arguments:
**	the_cache	The pointer to the token cache ring.
**
**	Globals:	None	
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/12/93	Written by GSL
**	05/25/93	Change to pass in the_cache pointer. GSL
**
*/
static void a_init_token_cache(char *the_cache)
{
	TOKEN	tmptok;
	int	count;

	count = a_token_cache_count(the_cache);

	while(count--)
	{
		a_token_cache_unque(the_cache, &tmptok);
		clean_token(&tmptok);
	}
}

int a_token_cache_count(char *the_cache)
{
	int 	rc;
	int	count;

	if (!the_cache)
	{
		return 0;
	}
	
	if ((rc = ring_count(the_cache,&count)))
	{
		write_log("WISP",'F',"RINGCOUNT","Unable to count ring [a_token_cache] rc=%d [%s]",rc,ring_error(rc));
		exit_with_err();
	}

	return count;
}

int token_cache_count(void)
{
	return(a_token_cache_count(token_cache));
}

int a_token_cache_unque(char *the_cache, TOKEN *tokptr)
{
	int	rc;

	rc = ring_unque((ring_struct *)the_cache,(char *)tokptr);
	if (rc < 0)
	{
		write_log("WISP",'F',"RINGUNQUE","Unable to unque ring [a_token_cache] rc=%d [%s]",rc,ring_error(rc));
		exit_with_err();
	}
	return rc;
}

void invalidate_token_cache(void)
{
	a_init_token_cache(token_cache);
}

int token_cache_unque(TOKEN *tokptr)
{
	return(a_token_cache_unque(token_cache,tokptr));
}

static int a_token_cache_que(char *the_cache, TOKEN *tokptr)
{
	int	rc;

	rc = ring_que((ring_struct *)the_cache,(char*)tokptr);
	if (rc < 0)
	{
		write_log("WISP",'F',"RINGQUE","Unable to queue ring [a_token_cache] rc=%d [%s]",rc,ring_error(rc));
		exit_with_err();
	}
	return rc;
}

int a_token_cache_get(char *the_cache, int num, TOKEN *tokptr)
{
	int	rc;

	rc = ring_get((ring_struct *)the_cache, num, (char*)tokptr);
	if (rc < 0)
	{
		write_log("WISP",'F',"RINGGET","Unable to get %d ring [a_token_cache] rc=%d [%s]",num,rc,ring_error(rc));
		exit_with_err();
	}
	return rc;
}

int token_cache_get(int num, TOKEN *tokptr)
{
	return(a_token_cache_get(token_cache,num,tokptr));
}

/*
**	Routine:	get_token()
**
**	Function:	To return the next token
**
**	Description:	This return gets the next token off of the token_cache.
**			When cache is empty it call get_line() to reload it.
**
**	Arguments:
**	tokptr		Pointer to where to put token.
**
**	Globals:
**	token_cache	The pointer to the token cache ring.
**
**	Return:		The token or NULL
**
**	Warnings:	None
**
**	History:	
**	05/12/92	Written by GSL
**
*/
TOKEN *get_token(void)
{
	int	count;
	TOKEN	*tokptr;

	for(;;)
	{
		count = token_cache_count();

		if (count > 0) break;

		if (end_of_input()) return NULL;

		get_cobol_inline();
	}

	tokptr = (TOKEN *)make_token(UNKNOWN,NULL);
	token_cache_unque(tokptr);

	return tokptr;
}

/*
**	Routine:	unget_token()
**
**	Function:	To reverse a get_token() operation.
**
**	Description:	Push the last token from a get_token() call back onto the
**			the front of the que.
**
**	Arguments:
**	tokptr		Pointer to last token.
**
**	Globals:
**	token_cache	The pointer to the token cache ring.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/11/93	Written by GSL
**
*/
int unget_token(TOKEN *tokptr)
{
	int	rc;

	rc = ring_push((ring_struct *)token_cache,(char*)tokptr);
	if (rc < 0)
	{
		write_log("WISP",'F',"RINGUNQUE","Unable to push ring [a_token_cache] rc=%d [%s]",rc,ring_error(rc));
		exit_with_err();
	}
	return rc;
}

/*
**	Routine:	init_token()
**
**	Function:	To initialize a token.
**
**	Description:	This routine sets all the token variables to initial values.
**
**	Arguments:
**	tokptr		Pointer to the token to initialize.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/12/93	Written by GSL
**
*/
static void init_token(TOKEN *tokptr, int the_absline, int the_line, void *the_context)
{
	tokptr->type 		= UNKNOWN;
	tokptr->context		= the_context;
	tokptr->absline		= the_absline;
	tokptr->line		= the_line;
	tokptr->column		= 0;
	tokptr->column_fixed 	= 0;
	tokptr->indata 		= NULL;
	tokptr->data 		= NULL;
}

TOKEN *make_token(int the_type, const char *the_data)
{
	TOKEN	*tokptr;

	tokptr = (TOKEN *)wmalloc(sizeof(TOKEN));
	init_token(tokptr,0,0,NULL);

	tokptr->type = the_type;
	if (the_data)
	{
		tokptr->data = (char *)wdupstr(the_data);
	}
	return( tokptr );
}

/*
**	Routine:	clean_token()
**
**	Function:	To re-initialize a token that has been used
**
**	Description:	This routine frees any associated data then calls init_token(),
**
**	Arguments:
**	tokptr		Pointer to the token to cleaned.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/12/93	Written by GSL
**
*/
void clean_token(TOKEN *tokptr)
{
	if (tokptr->indata) wfree(tokptr->indata);
	if (tokptr->data)   wfree(tokptr->data);
	init_token(tokptr,0,0,NULL);
}

/*
**	Routine:	free_token()
**
**	Function:	To free a token
**
**	Description:	This routine frees any associated data then frees the token.
**
**	Arguments:
**	tokptr		Pointer to the token to free.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/19/93	Written by GSL
**
*/
void free_token(TOKEN *tokptr)
{
	if (tokptr)
	{
		if (tokptr->indata) wfree(tokptr->indata);
		if (tokptr->data)   wfree(tokptr->data);
		wfree(tokptr);
	}
}

int eq_token(TOKEN *tokptr, int the_type, const char *the_data)
{
	if (!tokptr) return(0);

	if (the_type && tokptr->type != the_type) return(0);

 	return( 0 == strcmp(tokptr->data, the_data) );
}

int eq_token_literal(TOKEN *tokptr, const char *the_data_unquoted)
{
	unsigned int len = strlen(the_data_unquoted);

	if (!tokptr) return(0);

	if (tokptr->type != LITERAL) return(0);
	
 	return( len+2 == strlen(tokptr->data) &&
		0 == memcmp(&tokptr->data[1], the_data_unquoted, len) );
}


TOKEN *edit_token(TOKEN *tokptr, const char *the_data)
{
	if (tokptr->data) wfree(tokptr->data);
	tokptr->data = wmalloc(strlen(the_data)+1);
	strcpy(tokptr->data,the_data);

	if (tokptr->indata) wfree(tokptr->indata);
	tokptr->indata = NULL;

	return(tokptr);
}

TOKEN *dup_token(TOKEN *tokptr)
{
	TOKEN	*newtok;

	newtok = make_token(UNKNOWN,NULL);
	memcpy(newtok, tokptr, sizeof(TOKEN));
	newtok->indata = wdupstr(tokptr->indata);
	newtok->data   = wdupstr(tokptr->data);
	return(newtok);
}

/*
**	Routine:	fluff_token()
**
**	Function:	To determine if a token is fluff.
**
**	Description:	A FLUFF token is one that the parser does not want to look at.
**			This does not mean the token can be discarded.
**
**			NOTE: 	IDCOMMENT is not fluff, the parsing logic of the
**				IDENTIFICATION division needs to see it.
**	Arguments:
**	tokptr		Pointer to the token
**
**	Globals:	None
**
**	Return:
**	0		Not a fluff token
**	1		Is a fluff token
**
**	Warnings:	None
**
**	History:	
**	05/25/93	Written by GSL
**	06/01/93	Removed IDCOMMENT from the fluff list. GSL
**
*/
int fluff_token(TOKEN *tokptr)
{
	switch(tokptr->type)
	{
	case PUNCT:
	case LINECODE:
	case MODCODE:
	case PCOMMENT:
	case COMMENT:
	case SCOMMENT:
	case NOPROCESS:
		return(1);
	}
	return(0);
}

/*
**	Routine:	lint_token()
**
**	Function:	To determine if a token is lint.
**
**	Description:	A LINT token is one that CAN be discarded without loss of integrity.
**
**	Arguments:
**	tokptr		Pointer to the token
**
**	Globals:	None
**
**	Return:
**	0		Not a lint token
**	1		Is a lint token
**
**	Warnings:	None
**
**	History:	
**	05/29/93	Written by GSL
**
*/
int lint_token(TOKEN *tokptr)
{
	switch(tokptr->type)
	{
	case PUNCT:
	case LINECODE:
	case MODCODE:
		return(1);
	}
	return(0);
}

char *token_data(TOKEN *tokptr)
{
	static char null_string[] = "";
	if (tokptr && tokptr->data) return(tokptr->data);
	return(null_string);
}

char *token_type_mess(TOKEN *tokptr)
{
	char	*mess;

	if (!tokptr) return("(NULL)");

	switch(tokptr->type)
	{
	case UNKNOWN:		mess = "UNKNOWN";	break;
	case KEYWORD:		mess = "KEYWORD";	break;
	case LITERAL:		mess = "LITERAL";	break;
	case NUMBER:		mess = "NUMBER";	break;
	case IDENTIFIER:	mess = "IDENTIFIER";	break;
	case PERIOD:		mess = "PERIOD";	break;
	case CONTINUATION:	mess = "CONTINUATION";	break;
	case OPERATOR:		mess = "OPERATOR";	break;
	case PICTURE:		mess = "PICTURE";	break;
	case PUNCT:		mess = "PUNCT";		break;
	case LINECODE:		mess = "LINECODE";	break;
	case MODCODE:		mess = "MODCODE";	break;
	case PCOMMENT:		mess = "PCOMMENT";	break;
	case VERB:		mess = "VERB";		break;
	case COMMENT:		mess = "COMMENT";	break;
	case NOPROCESS:		mess = "NOPROCESS";	break;
	case IDCOMMENT:		mess = "IDCOMMENT";	break;
	case SCOMMENT:		mess = "SCOMMENT";	break;
	default:		mess = "Default";	break;
	}
	return(mess);
}

/*
**	History:
**	$Log: tokenize.c,v $
**	Revision 1.17  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.16  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.15  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.14  2002/08/13 18:11:10  gsl
**	Fix missing CONTINUATION
**	
**	Revision 1.13  2002/08/12 21:19:20  gsl
**	Warn if missing CONTINUATION char
**	
**	Revision 1.12  2002/08/12 20:13:51  gsl
**	quotes and literals
**	
**	Revision 1.11  2001/10/18 15:12:51  gsl
**	Change to treat a blank line as a COMMENT token
**	
**	Revision 1.10  1998-03-27 10:40:13-05  gsl
**	change_words
**
**	Revision 1.9  1998-03-20 17:35:59-05  gsl
**	Change to use change_reserved_word() and make args const.
**
**	Revision 1.8  1998-03-17 16:58:31-05  gsl
**	Removed get_line() support logic
**
**	Revision 1.7  1996-06-24 14:10:03-04  gsl
**	fix warnings for NT
**
**	Revision 1.6  1995-06-14 03:21:35-07  gsl
**	Fixed so could handle "#" "@" "$" in copy file names.
**	These form a special case of IDENTIFIER.
**
**	Also added standard headers.
**
**
**
*/
