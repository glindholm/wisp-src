static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#if defined(unix) || defined(VMS)	/* MSDOS and WINNT does not support versions before 22, which use this module.	*/

#include <ctype.h>
#include "idsistd.h"
#include "werrlog.h"

#define A_BIT 	((uint4)0x80000000)

/* justify a numeric field around a Decimal point										*/

static char *signch = "{ABCDEFGHI}JKLMNOPQR";

struct mask_s
{
	uint4 cmask;                                    /* The comma edit mask for edited fields.				*/
	uint4 zmask;					/* The Z edit mask for edited fields.					*/
};
static int loadfld();
static int jzedit();

int juster(src,dst,len,dp,mask,retval)

#define		ROUTINE		26000

char *src;						/* the source string							*/
char *dst;						/* the destination string						*/
char *len;						/* the length of the string						*/
char *dp;						/* the location of the decimal point, and associated sign flags		*/
							/* The dp byte is used in the following manner:				*/
							/* 	The byte is defined as NSLPPPPP, where 				*/
							/*	N is the flag indicating whether or not the field is Edited	*/
							/*		or numeric. 0 = Edited, 1 = Numeric only.		*/

							/*	S is the flag indicating there is a sign present.		*/
							/*		0 = no sign, 1 = Sign is present.			*/

							/*	L is the flag indicating the sign is leading or trailing.	*/
							/*		0 = trailing, 1 = Leading				*/

							/* 	PPPPP are the bits indicating where the Decimal point is.	*/
struct mask_s *mask;


int *retval;						/* The field for the return value.					*/


{
	int idxt,idxd,idxs,idxt2,fldlen,dppos,dpoff,dptemp;
	int numeric,a_sign,leading,neg,is_zero;
	uint4 a_bit,cmask,zmask;
	char temp[100];									/* scratch string			*/

	werrlog(ERRORCODE(1),src,dst,len,dp,mask,0,0,0);				/* Say hello.				*/

	is_zero = 1;									/* Assume it's value is zero.		*/

	cmask = mask->cmask;								/* The mask for commas.			*/

	if (cmask & 1)
	{
		zmask = mask->zmask;							/* Set up the mask for Z's		*/
		cmask = cmask ^ 1;							/* Clear the low bit			*/
	}
	else	zmask = 0;

	fldlen = *len;									/* get length				*/
	dppos = *dp & '\037';								/* loc of dp				*/

	if (*dp & '\200') numeric = 1;							/* is it numeric or edited		*/
	else              numeric = 0;

	if (*dp & '\100') a_sign = 1;							/* is there a sign			*/
	else              a_sign = 0;

	if (*dp & '\040') leading = 1;							/* is the sign leading or trailing	*/
	else              leading = 0;

	neg = 0;									/* assume positive for now		*/

	a_bit = A_BIT;
	for (idxd=0; idxd < fldlen; idxd++)
	{										/* first fill destination with zero's	*/
		if ((idxd == dppos) && !numeric)
			dst[idxd] = '.';						/* put the dp in the correct position	*/
		else
		{
			if (a_bit & zmask)						/* If the z bit is set, use a space.	*/
				dst[idxd] = ' ';
			else if (!(a_bit & cmask))					/* If the comma bit is not set, use zero*/
				dst[idxd] = '0';
			else if (a_bit & cmask)
				dst[idxd] = ',';
		}									/* Otherwise leave the dest alone.	*/
		a_bit = a_bit >> 1;							/* rotate the bit.			*/
	}

	idxs = 0;

	do										/* first find the first non-blank	*/
	{
		if (src[idxs] != ' ') break;
	} while (++idxs < fldlen);

	if (idxs == fldlen)
	{
		if (zmask)
		{
			is_zero = 1;							/* Set the zero flag			*/
			jzedit(zmask,dst,*len,is_zero);					/* Do the z edit.			*/
		}

		*retval = 0;								/* Field is all spaces, return zeros.	*/
		return(*retval);
	}

	idxt = 0;									/* pointer in temp array		*/

	if (idxs < fldlen ) do								/* then scan until end or blank or '.'	*/
	{
		if ((src[idxs] == '-') || (src[idxs] == '+'))				/* found a sign, log it and go on	*/
		{
			if (src[idxs] == '-')						/* it is negative			*/
			{
				if (!a_sign)
				{
					*retval = -1;					/* but it isn't supposed to have a sign	*/
					return(*retval);
				}
				neg = 1;
			}
			idxs++;								/* skip it				*/
			continue;
		}

		if ((src[idxs] == ' ') || (src[idxs] == '.')) break;

		if (isdigit(src[idxs]))							/* If it is a number.			*/
		{
			if (src[idxs] != '0') is_zero = 0;				/* FLag that it's not zero.		*/
			temp[idxt++] = src[idxs++];					/* copy the integer portion		*/
		}
		else if (src[idxs] == ',')						/* else if it's a comma, skip over it.	*/
		{
			idxs++;
		}
		else
		{
			*retval = -1;
			return(*retval);
		}
	} while (idxs < fldlen && idxt < dppos);

	dpoff = dppos - idxt;								/* offset from dp			*/
	dptemp = idxt;									/* Remember where the dp is.		*/

	if ((src[idxs] == '.') && (idxt+dpoff < fldlen))				/* There is a fraction			*/
	{
		idxs++;
		if (!numeric) temp[idxt++] = '.';

		if (idxs < fldlen ) do							/* then scan until end or blank 	*/
		{
			if ((src[idxs] == '-') || (src[idxs] == '+'))			/* found a sign, log it and go on	*/
			{								/* it is negative			*/
				if (src[idxs] == '-')
				{
					if (!a_sign)
					{
						*retval = -1;				/* but it's not supposed to have a sign	*/
						return(*retval);
					}
					neg = 1;
				}
				idxs++;							/* skip it				*/
				continue;						/* do the next one			*/
			}
			if (src[idxs] == ' ') break;
			if (!isdigit(src[idxs]))
			{
				*retval = -1;
				return(*retval);					/* error, non numeric value		*/
			}
			temp[idxt++] = src[idxs++];					/* copy the fractional portion		*/
			if (src[idxs] != '0') is_zero = 0;				/* Flag that it's not zero.		*/
		} while ((idxs < fldlen) && (idxt+dpoff < fldlen));
	}

	if (cmask)									/* If there is a comma edit mask...	*/
	{										/* We need to copy with care.		*/
		idxt2 = dptemp;								/* starting at the src decimal point.	*/
		idxd = dppos;								/* And the dp in the destination.	*/
		a_bit = A_BIT;
		a_bit = a_bit >> idxd;							/* Get ready to mask the next position.	*/
		while (idxt2--)
		{
			if (!idxd)
			{
				*retval = -1;
				return(*retval);					/* Error, too much data in the field.	*/
			}
			idxd--;								/* Move the target byte.		*/
			a_bit = a_bit << 1;			
			if (cmask & a_bit)						/* Is it a special character?		*/
			{
				idxt2++;						/* Yes, save the source counter.	*/
			}
			else
			{								/* No.					*/
				dst[idxd] = temp[idxt2];				/* Don't need to preserve it, copy it.	*/
			}
		}
		if (dptemp < idxt)							/* Is there something after the dp?	*/
		{
			loadfld(dst,dppos,&temp[dptemp],idxt-dptemp,fldlen);		/* Load the field.			*/
		}
	}
	else										/* No mask means a simple number.	*/
	{
		loadfld(dst,dpoff,temp,idxt,fldlen);					/* Load the field.			*/
	}


	if (zmask) jzedit(zmask,dst,*len,is_zero);					/* If there was z mask, be sure to pad.	*/


	if (numeric && a_sign)								/* handle sign for numeric		*/
	{
		if (neg)								/* it is negative			*/
		{
			dst[fldlen-1] = signch[(dst[fldlen-1] - '0') + 10];		/* get the overpunch value from table	*/
		}
		else
		{
			dst[fldlen-1] = signch[(dst[fldlen-1] - '0')];
		}
	}
	else if (!numeric && a_sign)							/* handle sign for numeric edited	*/
	{										/* leading sign...			*/
		if (neg)								/* it is negative			*/
		{
			if (leading) dst[0] = '-';
			else         dst[fldlen-1] = '-';
		}
		else
		{
			if (leading) dst[0] = ' ';
			else         dst[fldlen-1] = ' ';
		}
	}
	*retval = 0;
	return(*retval);								/* success				*/
}

static loadfld(dst,dstoff,src,len,dstlen)
char	*dst, *src;
int	dstoff,len,dstlen;
{
	int	cpylen,room;

	room = dstlen - dstoff;
	if (room < 1 || len < 1) return;

	cpylen = (len > room) ? room:len;

	memcpy(dst+dstoff,src,cpylen);
}


static jzedit(mask,dst,len,flag)
uint4 mask;
char *dst;
int len;
int flag;
{											/* This makes sure a ZZZ.ZZ won't end up*/
	int i,j;									/* As A "  9.2 " string, but as a	*/
	j = 0;										/* "  9.20" string.			*/

	for (i=0; i<len; i++)
	{
		if  (dst[i] == ' ')							/* Is it a space?			*/
		{
			if (j)								/* Have we seen some numbers?		*/
			{
				dst[i] = '0';						/* Then it should be a zero.		*/
			}
		}
		else if (isdigit(dst[i]))						/* Is it a digit, flag we saw one.	*/
		{
			if (dst[i] == '0')
			{
				if (!j) dst[i] = ' ';					/* If digits yet, zeros are spaces.	*/
			}
			else j = 1;							/* Non zero, of course.			*/

		}
		else if  (dst[i] == ',')						/* Is it a comma?			*/
		{
			if (!j)								/* Have we seen no numbers?		*/
			{
				dst[i] = ' ';						/* Then it should be a space.		*/
			}
		}
		else if (dst[i] == '.')							/* Is it a period?			*/
		{
			if (flag)
			{
				dst[i] = ' ';						/* If it's zero, zap the period.	*/
			}
			else
			{
				j = 1;							/* Periods are like digits.		*/
			}
		}
	}
}

#endif		/* This entire module is ifndef MSDOS - NOT used for MSDOS port.	*/
/*
**	History:
**	$Log: juster.c,v $
**	Revision 1.10  1996-08-19 18:32:25-04  gsl
**	drcs update
**
**
**
*/
