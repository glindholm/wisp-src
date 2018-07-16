/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
	cobpic.c
*/

#include <ctype.h>
#include <string.h>

#include "idsistd.h"
#include "cobpic.h"
#include "wisp_pic.h"


static int cobpic_alphaedit( char *xobj, const char *source, const char *xpicture, int size );
static int cobpic_numedit(char* xobj, const char* source, const char* xpic, int p_size, int p_dp, 
			  int blankdecimal, char suppchar, int suppidx, int floatidx, int psigned );
static int picedit_insert();
static int picfloatedit();
static int piczeroedit(const char* xpic, char* xobj, char suppchar, int suppidx, int blankdecimal, int value,int psize, int dp);

#define	ALPHA_FAC	0x81
#define NUM_FAC		0x82
#define PROT_FAC	0x8C


static char	char_decimal_point = '.';	/* The decimal_point character		*/
static char	char_comma = ',';		/* The comma character			*/

void WL_set_char_decimal_point(char dp)	{ char_decimal_point = dp; }
char WL_get_char_decimal_point()	{ return char_decimal_point; }

void WL_set_char_comma(char comma)	{ char_comma = comma; }
char WL_get_char_comma()		{ return char_comma; }

/*
	cobpic_edit:	COBOL PICTURE EDIT
			This routine will move the source field into the object field performing picture editing.
			The resulting object field will conform to the picture clause.
			If invalid data is detected the errcode will be set to non-zero and the object field will not be modifed.
			The source and picture fields are never modified.
			The source and object fields MUST be of size WL_pic_size(picture).
			If picture clause is considered to be of type alphabetic, alphanumeric, alphaedit or numeric.
			An alphabetic picture contains only A and B elements.
			An alphanumeric picture contains only A, X, and 9 elements.
			An alphaedit picture contains at least one A or X and at least one B, 0 or /.
			A numeric picture does not contain any A or X elements.
			A picture element of P, S, or V are ignored.
			All numeric pictures are treated a numeric-edited.
			If the picture is alphanumeric or the size is greater then 80 no editing is performed.
*/
void WL_cobpic_edit(char* object, const char* source, const char* picture,int* errcode )
{
	char	xpic[100], suppchar;
	int	xflag, psize, pic_type, pic_dp, blankdecimal, suppidx, floatidx, psigned;

	*errcode = 0;

	WL_parse_pic(picture, xpic, &xflag, &psize, &pic_type, &pic_dp, &blankdecimal, 
						 &suppchar, &suppidx, &floatidx, &psigned);

	WL_cobxpic_edit(object, source, xpic, psize, pic_type, pic_dp, blankdecimal,
						  suppchar, suppidx, floatidx, psigned, errcode);

}

void WL_cobxpic_edit(
		  char	*object,		/* Input and Output areas */
		  const char	*source, 
		  const char	*xpicture,	/* The expanded picture (also adjusted picture for Numeric only) */
		  int	size,			/* The number of character positions in pic */
		  int	pic_type,		/* The type of picture 0=noedit 1=Alphaedit 2=Numeric */
		  int	pic_dp,			/* Number of characters to the left of the decimal point 	(Numeric only)*/
		  int	blankdecimal,		/* Does the decimal portion contains no '9's	 		(Numeric only)*/
		  char	suppchar,		/* Zero suppression character 					(Numeric only)*/
		  int	suppidx,		/* Zero suppression index; offset of first 'Z' or '*' in pic	(Numeric only)*/
		  int	floatidx,		/* Floating character index; offset of start of float 		(Numeric only)*/
		  int	psigned,		/* Is the picture signed			 		(Numeric only)*/
		  int 	*errcode)		/* Error code 0=success */
{
	char	xobj[100];

	*errcode = 0;

	switch (pic_type)
	{
	case PIC_NOEDIT:
		memcpy(object,source,size);
		return;

	case PIC_ALPHAEDIT:
		*errcode = cobpic_alphaedit( xobj, source, xpicture, size);
		break;

	case PIC_NUMERIC:
		*errcode = cobpic_numedit( xobj, source, xpicture, size, pic_dp, blankdecimal,
						suppchar, suppidx, floatidx, psigned );
		break;
	}

	if ( !*errcode )
	{
		memcpy(object, xobj, size);
	}

	return;
}

/*
	cobpic_expand:	Expand (num) syntax and remove 'S', 'V', and 'P' characters

			"X(10)"		->	"XXXXXXXXXX"
			"Z(6).9(4)"	->	"ZZZZZZ.9999"
*/
static void cobpic_expand( const char* picture, char* xpic )
{
	int 	i;
	char	*xchar;

	while (*picture)								/* till we see the null			*/
	{
		if ((*picture == 'S') || (*picture == 'V') || (*picture == 'P'))
		{
			picture++;
			continue;							/* Don't count S or V or P		*/
		}
		if (*picture != '(')							/* if it's not an open paren, count it	*/
		{
			*xpic = *picture++;
			xchar = xpic++;
		}
		else									/* it was an open paren, get contents	*/
		{
			picture++;
			xpic--;
			i = 0;								/* temp counter				*/
			do
			{
				i = i * 10;						/* shift digits left			*/
				i = i + (*picture++ - '0');				/* add the next digit			*/
			} while (*picture != ')');					/* till we find the close paren		*/

			memset(xpic,*xchar,i);
			xpic += i;
			picture++;							/* point past the paren			*/
		}
	} 
	*xpic = '\0';
}

/*
	cobpic_alphaedit:	Do Alphanumeric editing.
*/
static int cobpic_alphaedit( char *xobj, const char *source, const char *xpicture, int size )
{
	char	*obj;
	const char *src;
	const char *pic;

	pic = xpicture;						/* Point to expanded picture.				*/
	obj = xobj;						/* Point to temp object buffer.				*/
	src = source;

	while(*pic)
	{
		switch(*pic)
		{
		case 'A':					/* If not A-Z errcode					*/
			if ( !isalpha((int)*src) && *src != ' ' )
			{
				return(1);
			}
			*obj++ = *src++;
			break;
		case '9':					/* If not 0,1-9 errcode					*/
			if ( !isdigit((int)*src) )
			{
				return(1);
			}
			*obj++ = *src++;
			break;
		case 'X':
			*obj++ = *src++;
			break;
		case 'B':
			if (*src == ' ') src++;
			*obj++ = ' ';
			break;
		case '0':
		case '/':
		default:
			if (*src == *pic) src++;
			*obj++ = *pic;
			break;
		}
		pic++;
	}

	return(0);
}


/*
	cobpic_numedit:		COBOL PICTURE NUMERIC EDIT

				Numeric Character		'9'
				Decimal Point Insertion		'.' 				(DECIMAL IS '.' or ',')
				Sign Insertion			'-', '+', 'CR', 'DB'
				Character Insertion		'/', 'B', '0', ','
				Zero-Suppresion			'Z', '*'
				Floating 			'--', '++', '$$'
				Ignore				'S', 'V', 'P'
*/
static int cobpic_numedit(char* xobj, const char* source, const char* xpic, int p_size, int p_dp, 
			  int blankdecimal, char suppchar, int suppidx, int floatidx, int psigned )
{
	const char	*src;
	int	e_size; 					/* Size of the element in source.				*/
	int	e_start; 					/* Starting offset of element in source.			*/
	int	e_end;						/* Ending offset of element in source (after last position)	*/
	int	e_dp;						/* Number of characters to the left of decimal point.		*/
	int	inc_src, e_idx, p_idx;
	int	negative, signfound, value;

	negative = 0;
	signfound = 0;


	/*
	**	Calculate e_start (element start)
	*/


	/* Skip over leading spaces */
	for (e_start=0; e_start<p_size && source[e_start] == ' '; e_start++);

	/* Get the leading sign and skip over leading spaces and zero suppression characters */
	if (e_start<p_size)
	{
		if (source[e_start] == '-') 					/* Strip leading '-' or '+' sign.		*/
		{
			negative = 1;
			signfound = 1;
			e_start++;
			if (e_start >= p_size) return(1);
		}
		else if (source[e_start] == '+') 
		{
			negative = 0;
			signfound = 1;
			e_start++;
		}

		if (source[e_start] == '$')					/* Strip leading '$' sign.			*/
		{
			e_start++;
		}
				 						/* Strip leading '*', ' ' and '0' 		*/
		while(e_start < p_size && (source[e_start] == '*' || 
					   source[e_start] == ' ' || 
					   source[e_start] == '0'   ) ) e_start++;

		if (e_start >= p_size && negative) return(1);

	}


	/*
	**	Calculate e_size and e_end
	*/

	for(e_end=p_size; e_end>e_start && source[e_end-1] == ' '; e_end--);
	e_size = e_end-e_start; 

	if ( e_size > 0 )
	{
		if ( source[e_end-1] == '-' )					/* Strip trailing '-' sign			*/
		{
			if (signfound) return(1);
			negative = 1;
			signfound = 1;
			e_size--;
			e_end--;
		}
		else if ( source[e_end-1] == '+' )				/* Strip trailing '+' sign			*/
		{
			if (signfound) return(1);
			negative = 0;
			signfound = 1;
			e_size--;
			e_end--;
		}
		else if ( e_size > 1 )						/* Strip trailing "CR" or "DB"			*/
		{
			if ( ( psigned == PIC_CR_SIGNED && source[e_end-2] == 'C' && source[e_end-1] == 'R' ) ||
			     ( psigned == PIC_DB_SIGNED && source[e_end-2] == 'D' && source[e_end-1] == 'B' )    )
			{
				if (signfound) return(1);
				negative = 1;
				signfound = 1;
				e_size -= 2;
				e_end  -= 2;
			}
		}

										/* Strip trailing blanks 			*/
		for (; e_end>e_start && source[e_end-1] == ' '; e_end--, e_size--);

		if (e_size<=0 && negative) 
		{
			return(1);
		}
	}

	if (negative && psigned == PIC_UNSIGNED) return(1);

	/* Check if we have digits that make up a value */
	for(value=0, e_idx=e_start; e_idx<e_end; e_idx++) 
	{
		if ( isdigit((int)source[e_idx]) && source[e_idx] != '0' ) 
		{
			value=1; 
			break;
		}
	}
	if (negative && !value) return(1);

	/*
	**	Calculate e_dp
	*/

	for(e_dp=0; e_start+e_dp<e_end && source[e_start+e_dp] != char_decimal_point; e_dp++);	

	/*
	**	If there is a decimal point in the source and no decimal in picture clause then error.
	*/
	if (source[e_start+e_dp] == char_decimal_point && p_dp >= p_size )
	{
		return(1);
	}

	/*
	**	Load left digits and fixed characters
	*/

	for ( e_idx=e_start+e_dp-1, p_idx=p_dp-1; p_idx >= 0; p_idx-- )	
	{
		if ( e_idx < e_start )	src = "0";
		else			src = &source[e_idx];

		if (picedit_insert(&xpic[p_idx],src,&xobj[p_idx],negative,&inc_src)) return(1);

		if ( inc_src ) 
		{
			e_idx--;
		}
	}

	for (;e_idx >= e_start; e_idx--)					/* If truncation of significate digits then error*/
	{
		if ( source[e_idx] >= '1' && source[e_idx] <= '9' ) return(1);
	}


	/*
	**	Load decimal point and right digits and fixed characters
	*/

	if ( p_dp < p_size )	
	{
		xobj[p_dp] = char_decimal_point;
		for ( e_idx=e_start+e_dp+1, p_idx=p_dp+1; p_idx < p_size; p_idx++) 
		{
			if ( e_idx >= e_end )	src = "0";
			else			src = &source[e_idx];

			if (picedit_insert(&xpic[p_idx],src,&xobj[p_idx],negative,&inc_src)) return(1);

			if ( inc_src ) 
			{
				e_idx++;
			}
		}

		for (;e_idx < e_end; e_idx++)					/* If truncation of significate digits then error*/
		{
			if ( source[e_idx] >= '1' && source[e_idx] <= '9' ) return(1);
		}
	}
	else /* No decimal in pic */
	{
		for(e_idx=e_start+e_dp+1; e_idx<e_end; e_idx++)			/* If truncation of significate digits then error*/
		{
			if ( source[e_idx] >= '1' && source[e_idx] <= '9' ) return(1);
		}
	}

	/*
	**	Perform floating character and zero suppression & replacement editing
	*/

	if (picfloatedit(xpic,xobj,floatidx,blankdecimal))			/* Floating insertion editing.			*/
	{
		piczeroedit(xpic,xobj,suppchar,suppidx,blankdecimal,value,p_size,p_dp);	/* Zero suppression editing.		*/
	}

	return( 0 );
}



/*
	adjustpic:	Replace floating chars with '#' and 'B' with 'b'.

			"---,---B.B99"  ->	"-##,###b.b99"
			"++,+++,+++"	->	"+#,###,###"
			"$$$$.99-"	->	"$###.99-"
			"9B999B999DB"	->	"9b999b999DB"
			"--,---.--"	->	"-##,###.##"
			"-$$$$$$$$"	->	"-$#######"
*/

static void adjustpic(char *xpic, int size)
{
	char	*ptr1, *ptr2;

	while((ptr1=strchr(xpic,'B')))
	{
		*ptr1 = 'b';
	}
	if (size>2 && memcmp(&xpic[size-2],"Db",2) == 0)
	{
		memcpy(&xpic[size-2],"DB",2);
	}

	if ( ((ptr1=strchr(xpic,'-')) && (ptr2=strchr(ptr1+1,'-'))) ||
	     ((ptr1=strchr(xpic,'+')) && (ptr2=strchr(ptr1+1,'+'))) ||
	     ((ptr1=strchr(xpic,'$')) && (ptr2=strchr(ptr1+1,'$')))    )
	{
		while(*ptr2)
		{
			if (*ptr1 == *ptr2) 
			{
				*ptr2 = '#';
			}
			else if ( *ptr2 != char_decimal_point && 
				  *ptr2 != char_comma && 
				  *ptr2 != '/' && 
				  *ptr2 != 'b' && 
				  *ptr2 != '0'   )
			{
				break;
			}
			ptr2++;
		}
	}
}

/*
	picedit_insert:		Insert a single character into object from pic or src.
*/
static int picedit_insert(char* pic, char* src, char* obj, int negative, int* inc_src)
{
	*inc_src = 0;
	switch(*pic)
	{
	case '9':
	case 'Z':
	case '*':
	case '#':
		if ( isdigit((int)*src) || 
		     ('*' == *pic && '*' == *src) )	/* Zero suppression */
		{
			*obj = *src;
			*inc_src = 1;
		}
		else
		{
			return(1);
		}
		break;
	case 'C':
	case 'R':
	case 'D':
	case 'B':
	case '-':
		if ( negative )
		{
			*obj = *pic;
		}
		else
		{
			*obj = ' ';
		}
		break;
	case '+':
		*obj = ( negative ) ? '-' : '+';
		break;
	case 'b':
		*obj = ' ';
		if (*src == ' ') *inc_src=1;
		break;
	case '$':
	case '0':
	case '/':
	case ',': /* note comma may be '.' or decimal may be ',' */
	case '.':
	default:
		*obj = *pic;
		if (*src == *pic) *inc_src=1;
		break;
	}

	return(0);
}

/*
	picfloatedit:	Perform floating character editing

			pic	"-####"		"$$,$$$.99"
			in	"-0024"		"$0,123.45"
			out	"  -24"		"  $123.45"
*/

static int picfloatedit(char* xpic, char* xobj, int floatidx, int blankdecimal)	/* Floating insertion editing.			*/
{
	int	idx, i, shift, decimal, done;

	if (floatidx >= 0)
	{
		for(done=0,idx=floatidx+1; !done; idx++)
		{
			shift=0;

			switch(xpic[idx])
			{
			case '\0':						/* No digits found so no float char.		*/
			case 'C':
			case 'R':
			case 'D':
			case 'B':
			case '-':
			case '+':
			case '$':
				xobj[idx-1] = ' ';
				done = 1;
				break;
			case '9':						/* Were done.					*/
			case 'Z':
			case '*':
				done = 1;
				break;
			case '.':	/* Don't know if this is a DECIMAL_POINT or a COMMA so take both then figure it out	*/
			case ',':
				if (xpic[idx] == char_comma)			/* This is a COMMA				*/
				{
					shift = 1;
					break;
				}

										/* This is a DECIMAL POINT			*/
				if (!blankdecimal)
				{
					done = 1;
					break;
				}
				decimal = 0;					/* If no digit to right then delete . etc 	*/
				for(i=idx+1; xpic[i]; i++)
				{
					if ( xpic[i] == '#' && xobj[i] != '0' )
					{
						decimal = 1;
						break;
					}
				}
				if (!decimal)	shift = 1;
				else		done = 1;

				break;
			case '#':
				if ( xobj[idx] == '0' ) shift = 1;
				else			done = 1;
				break;
			case '0':
			case '/':
			case 'b':
			default:
				shift=1;
				break;
			}

			if (shift)
			{
				xobj[idx] = xobj[idx-1];			/* Shift over the floating char.		*/
				xobj[idx-1] = ' ';
			}
		}
		
		return(0);
	}
	else
	{
		return(1);
	}
}

/*
	piczeroedit:	Perform zero suppression and replacement editing.

			xpic	"$bZZ,ZZZ.99bCR"	"$*,***,***.**+"
			in	"$ 00,012.34 CR"	"$0,000,123.45-"
			out	"$     12.23 CR"	"$******123.45-"


*/

static int piczeroedit(const char* xpic, char* xobj, char suppchar, int suppidx, int blankdecimal, int value,int psize, int dp)
{
	int	done, idx, decimal, suppress, i;

	if (suppidx >= 0)
	{
		if (!value && !strchr(xpic,'9'))
		{
			memset(xobj,suppchar,psize);
			if ( dp < psize && suppchar == '*' )
			{
				xobj[dp] = char_decimal_point;
			}

			return(0);
		}

		for(done=0,idx=suppidx; !done; idx++)
		{
			suppress = 0;

			switch(xpic[idx])
			{
			case 'Z':
			case '*':
				if (xobj[idx] == '0')	suppress = 1;
				else			done = 1;
				break;
			case '\0':						/* No digits found so no float char.		*/
			case '9':						/* Were done.					*/
				done = 1;
				break;

			case '.':	/* Don't know if this is a DECIMAL_POINT or a COMMA so take both then figure it out	*/
			case ',':
				if (xpic[idx] == char_comma)			/* This is a COMMA				*/
				{
					suppress = 1;
					break;
				}

										/* This is a DECIMAL POINT			*/
				if (!blankdecimal)
				{
					done = 1;
					break;
				}

				decimal = 0;					/* If no digit to right then delete . etc 	*/
				for(i=idx+1; xpic[i]; i++)
				{
					if ( (xpic[i] == 'Z' || xpic[i] == '*') && xobj[i] != '0' ) 
					{
						decimal = 1;
						break;
					}
				}

				if (decimal) done = 1;

				suppress = 0;					/* Don't suppress the decimal itself.		*/

				break;

			case 'b':
				suppress = 1;
				break;

			case 'C':
			case 'R':
			case 'D':
			case 'B':
			case '#':
			case '0':
			case '/':
				suppress = 1;
				break;

			case '-':
				break;

			case '+':
			case '$':
			default:
				suppress = 0;
				break;
			}

			if (suppress)
			{
				xobj[idx] = suppchar;
			}
		}		

		return(0);
	}
	else
	{
		return(1);
	}
}

/*
	WL_parse_pic:	Parse the cobol picture and return all editing info.
*/
void WL_parse_pic(const char *picture,	/* The COBOL picture clause. */
	       char	*xpic,		/* The expanded picture (also adjusted picture for Numeric only) */
	       int	*xflag,		/* Flag if xpic was created. */
	       int	*psize,		/* The number of character positions in pic */
	       int	*pic_type,	/* The type of picture 0=noedit 1=Alphaedit 2=Numeric */
	       int	*pic_dp,	/* Number of characters to the left of the decimal point 	(Numeric only)*/
	       int	*blankdecimal,	/* Does the decimal portion contains no '9's	 		(Numeric only)*/
	       char	*suppchar,	/* Zero suppression character 					(Numeric only)*/
	       int	*suppidx,	/* Zero suppression index; offset of first 'Z' or '*' in pic	(Numeric only)*/
	       int	*floatidx,	/* Floating character index; offset of start of float 		(Numeric only)*/
	       int	*psigned)	/* Is the picture signed			 		(Numeric only)*/
{
	int	alphabetic, alphanumeric, numeric, alphaedit, idx, done;
	char	*ptr;

	*xflag 		= 0;				/* Not expanded. */
	*psize 		= WL_pic_size(picture);
	*pic_type 	= PIC_NOEDIT;			/* No edit */
	*pic_dp		= *psize;			/* All chars */
	*blankdecimal	= 1;				/* No '9' in decimal */
	*suppchar	= ' ';				/* No suppress character */
	*suppidx	= -1;				/* None */
	*floatidx	= -1;				/* None */
	*psigned	= PIC_UNSIGNED;			/* Unsigned */

	if (*psize > 80)
	{
		*pic_type = PIC_NOEDIT;
	}
	else
	{
		cobpic_expand( picture, xpic );				/* Expand the pic clause.				*/
		*xflag = 1;						/* Note it is expanded.					*/

		alphabetic   = ( strchr(xpic,'A') != 0 );
		alphanumeric = ( strchr(xpic,'X') != 0 );

		numeric = (alphabetic || alphanumeric) ? 0 : 1;

		if (alphabetic && alphanumeric) alphabetic = 0;

		if ( alphanumeric && strpbrk(xpic,"B0/9") )
		{
			alphaedit = 1;
			alphanumeric = 0;
		}
		else
		{
			alphaedit = 0;
		}

		if ( numeric ) *pic_type = PIC_NUMERIC;
		else if ( alphaedit || alphabetic ) *pic_type = PIC_ALPHAEDIT;
		else *pic_type = PIC_NOEDIT;					/* Default to NOEDIT				*/
	}

	if ( *pic_type == PIC_NUMERIC )
	{
		adjustpic(xpic,*psize);						/* Chg 'B' -> 'b' and float to '#'		*/

		if ( (ptr = strchr(xpic,char_decimal_point)) )			/* Calc picture decimal point (p_dp).		*/
		{
			*pic_dp = ptr - xpic;
		}
		else
		{
			*pic_dp = *psize;
		}

		for(idx=(*pic_dp); xpic[idx]; idx++)
		{
			if (xpic[idx] == '9')
			{
				*blankdecimal = 0;				/* If '9' to right then blankdecimal not allowed*/
				break;
			}
		}

		if ((ptr = strchr(xpic,'Z')) || (ptr = strchr(xpic,'*')))	
		{
			*suppchar = (*ptr == 'Z') ? ' ' : '*';			/* Set the suppression character.		*/

			*suppidx = ptr - xpic;					/* Set the start of suppression index.		*/

			for(done=0;!done && (*suppidx)>0; (*suppidx) -=1)
			{
				switch(xpic[*suppidx])
				{
				case ',': /* Don't know if this is a DECIMAL_POINT or a COMMA so take both then figure it out	*/
				case '.':
					if (xpic[*suppidx] == char_decimal_point)	/* This is a DECIMAL POINT		*/
					{
						done = 1;
					}
					break;
				case 'b':
				case '/':
				case '0':
					break;
				default:
					done = 1;
					break;
				}
			}

		}

		if ((ptr = strchr(xpic,'#')))
		{
			for(idx = ptr-xpic-1; idx>=0; idx--)			/* Point to floating char.			*/
			{
				if (xpic[idx] == '-' ||
				    xpic[idx] == '+' ||
				    xpic[idx] == '$'   ) break;
			}
			*floatidx = idx;					/* Set the float index.				*/
		}

		if ((ptr = strpbrk(xpic,"+-CD")))
		{
			switch(*ptr)
			{
			case 'C':
				*psigned = PIC_CR_SIGNED;
				break;
			case 'D':
				*psigned = PIC_DB_SIGNED;
				break;
			default:
				*psigned = PIC_SIGNED;
				break;
			}
		}
	}
	else /* Not NUMERIC */
	{
		/*
		 *	These fields only apply to numeric pictures so set to "unset"
		 */
		*pic_dp 	= -1;
		*blankdecimal  	= -1;
		*suppchar	= ' ';
		*suppidx	= -1;
		*floatidx	= -1;
		*psigned	= -1;
	}
	

}



/*
**	History:
**	$Log: cobpic.c,v $
**	Revision 1.23  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.22  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.21  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.20  2002/10/18 19:14:11  gsl
**	Cleanup
**	
**	Revision 1.19  2002/07/11 20:29:06  gsl
**	Fix WL_ globals
**	
**	Revision 1.18  2002/07/10 21:05:15  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.17  2001/11/08 16:37:28  gsl
**	Remove unused ptr.
**	
**	Revision 1.16  2001-09-26 15:20:29-04  gsl
**	Fix '*' zero suppression to the right of the decimal.
**	IN parse_pic() for non-NUMERIC pictures set the numeric only fields to -1
**
**	Revision 1.15  2001-09-25 16:38:48-04  gsl
**	Replace constants with defines
**	Remove LPI cobol code
**
**	Revision 1.14  1997-12-17 15:32:22-05  gsl
**	fix prototypes for const
**
**	Revision 1.13  1997-02-17 10:22:25-05  gsl
**	fix prototype
**
**	Revision 1.12  1996-08-19 18:32:13-04  gsl
**	drcs update
**
**
**
*/
