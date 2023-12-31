/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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


#include <string.h>

#include <ctype.h>
#include "idsistd.h"
#include "cobrun.h"

/* Do a WANG style MOVE WITH CONVERSION.											*/

static char *signch;
static int c_just(char* src, char* idst, char* fdst, int len);

void MWCONV(
	char *src,	/* the source string				*/
	char *idst,	/* the integer destination string.		*/
	char *fdst,	/* The fractional destination string.		*/
	char *len,	/* the length of the string			*/
	int4 *retval)	/* The field for the return value.		*/
{

	int l_len;
	int rval;

						/*  positve|negative  */ 
						/*01234567890123456789*/
	if (wisp_mf_cobol())		signch = "0123456789pqrstuvwxy";
	else				signch = "{ABCDEFGHI}JKLMNOPQR";

	l_len = *len;
	rval = c_just(src,idst,fdst,l_len);					/* Do the integer number.			*/

	if (rval)
	{
		*retval = -1L;
	}
	else
	{
		*retval = 0;
	}
	return;
}



static int c_just(char* src, char* idst, char* fdst, int len)
{
	int 	tcnt,i;
	int 	neg, signfound, decimalfound, nullfound;
	char 	temp[80];								/* scratch string			*/

	neg = 0;									/* assume positive for now		*/
	signfound = 0;
	decimalfound = 0;
	nullfound = 0;

	for (i=0; i < len; i++) idst[i] = fdst[i] = '0';				/* Init destination			*/

	i = 0;

	do										/* Then find the first non-blank	*/
	{
		if (src[i] != ' ') break;
	} while (++i < len);

	if (i<len && ((src[i] == '-') || (src[i] == '+')))				/* found a sign, log it and go on	*/
	{
		signfound=1;
		if (src[i] == '-')							/* it is negative			*/
		{
			neg = 1;
		}
		i++;									/* skip it				*/
	}

	tcnt = 0;									/* pointer in temp array for dp.	*/

	while (i < len )								/* then scan until end or blank or '.'	*/
	{
		if (!src[i])								/* NULL					*/
		{
			nullfound=1;
			break;
		}

		if (src[i] == ' ')							/* Space				*/
		{
			break;
		}

		if (src[i] == '.')							/* Decimal point			*/
		{
			decimalfound = 1;
			break;
		}

		if (!signfound && ((src[i] == '-') || (src[i] == '+')))			/* Found trailing sign			*/
		{
			signfound=1;
			if (src[i] == '-')						/* it is negative			*/
			{
				neg = 1;
			}
			i++;								/* skip it				*/
			break;
		}

		if (isdigit((int)src[i]))							/* If it is a number.			*/
		{
			temp[tcnt++] = src[i++];					/* copy the integer portion		*/
		}
		else if (src[i] == ',')							/* else if it's a comma, skip over it.	*/
		{
			i++;
		}
		else
		{
			return(-1);
		}
	}

	memcpy(idst+len-tcnt,temp,tcnt);						/* copy the integer number		*/

	if (decimalfound)
	{
		i++;
		tcnt = 0;

		while (i < len)
		{
			if (!src[i])							/* NULL					*/
			{
				nullfound=1;
				break;
			}

			if (src[i] == ' ')						/* Space				*/
			{
				break;
			}

			if (!signfound && ((src[i] == '-') || (src[i] == '+')))		/* Found trailing sign			*/
			{
				signfound=1;
				if (src[i] == '-')					/* it is negative			*/
				{
					neg = 1;
				}
				i++;							/* skip it				*/
				break;
			}

			if (!isdigit((int)src[i]))
			{
				return(-1);
			}

			fdst[tcnt++] = src[i++];					/* copy the fractional portion		*/
		}
	}

	while (!nullfound && i < len)							/* The rest should be spaces		*/
	{
		if (!src[i]) nullfound = 1;
		else if (src[i] != ' ') return(-1);
		i++;
	}

	if (neg)									/* it is negative			*/
	{
		idst[len-1] = signch[(idst[len-1] - '0') + 10];				/* get the overpunch value from table	*/
		fdst[len-1] = signch[(fdst[len-1] - '0') + 10];				/* get the overpunch value from table	*/
	}
	else
	{
		idst[len-1] = signch[(idst[len-1] - '0')];
		fdst[len-1] = signch[(fdst[len-1] - '0')];
	}
	return(0);
}
/*
**	History:
**	$Log: mwconv.c,v $
**	Revision 1.14  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.12  2002/07/19 22:07:14  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.11  2002/07/02 04:00:38  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.10  1996/08/19 22:32:35  gsl
**	drcs update
**	
**
**
*/
