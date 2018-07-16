			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	wisp_pic.c
*/

#include <ctype.h>

#include "idsistd.h"

char *strchr();
char *strrchr();
char *strpbrk();

#define PIC_NOEDIT	0
#define PIC_ALPHAEDIT	1
#define PIC_NUMERIC	2

#define	ALPHA_FAC	0x81
#define NUM_FAC		0x82
#define PROT_FAC	0x8C

int pic_size(the_pic)									/* Take a picture clause and determine	*/
char *the_pic;										/* It's size.				*/
{
	int psize;									/* the size of it			*/
	int i;

	psize = 0;									/* initially zero			*/

	while (*the_pic)								/* till we see the null			*/
	{
		if ((*the_pic == 'S') || (*the_pic == 'V') || (*the_pic == 'P'))
		{
			the_pic++;
			continue;							/* Don't count S or V or P		*/
		}
		if (*the_pic++ != '(')							/* if it's not an open paren, count it	*/
		{
			psize++;
		}
		else									/* it was an open paren, get contents	*/
		{
			psize = psize - 1;						/* first un-count the paren and its type*/
			i = 0;								/* temp counter				*/
			do
			{
				i = i * 10;						/* shift digits left			*/
				i = i + (*the_pic++ - '0');				/* add the next digit			*/
			} while (*the_pic != ')');					/* till we find the close paren		*/
			psize = psize + i;						/* all done, add the size		*/
			the_pic++;							/* point past the paren			*/
		}
	} 
	return(psize);
}

int pic_dp(the_pic)									/* Take a picture clause and determine	*/
char *the_pic;										/* It's dp position.			*/
{
	int dp;										/* the decimal point location		*/
	int i;
	int dpflag;

	dpflag = 0;									/* flags = Num_ed, No_sign, Trailing	*/
	dp = 0;										/* initially zero			*/

	if (*the_pic == 0) return(0);							/* no pic				*/
	else if (strchr(the_pic,'A')) return(0);					/* or if A or X it's alphanumeric	*/
	else if (strchr(the_pic,'X')) return(0);

											/* continue if really numeric		*/
	if (*the_pic == 'S')								/* Sign field				*/
	{
		the_pic++;								/* Skip it				*/
		dpflag |= 224;								/* set Numeric, Signed, Leading flags	*/
	}
	else if (*the_pic == '-' || *the_pic == '+')					/* a real signed field			*/
	{
		dpflag |= 96;								/* Set signed, leading flags		*/
	}

	do
	{
		if (*the_pic++ != '(')							/* if it's not an open paren, count it	*/
		{
			dp++;
		}
		else									/* it was an open paren, get contents	*/
		{
			dp--;								/* first un-count the paren and its type*/
			i = 0;								/* temp counter				*/
			do
			{
				i = i * 10;						/* shift digits left			*/
				i = i + (*the_pic++ - '0');				/* add the next digit			*/
			} while (*the_pic != ')');					/* till we find the close paren		*/
			dp += i;							/* all done, add the size		*/
			the_pic++;							/* point past the paren			*/
		}									/* till we see a null, '.','-','+'	*/
	} while ((*the_pic) && (*the_pic != '.') && (*the_pic != 'V'));
	dp++;										/* count this location			*/

	if (!(*the_pic))								/* We ended on a null			*/
	{
		if ((*(the_pic-1) == '-') || (*(the_pic-1) == '+'))
		{
			dp--;								/* Back 1 if a sign there.		*/
			dpflag |= 64;							/* Tag it here, cause no more chars.	*/
		}
	}

	if (*the_pic == 'V') dpflag |= 128;						/* It was an assumed DP			*/
	if (strchr(the_pic,'-'))							/* it has a sign			*/
	{
		dpflag |= 64;
	}

	dp |= dpflag;									/* put in the flags			*/
	return(dp);
}


int pic_fac(the_pic)									/* Take a picture clause for a mod	*/
char *the_pic;										/* Field and determine An appropriate 	*/
{											/* FAC for it.				*/
	if (*the_pic == 0) return(PROT_FAC);						/* no pic, return protect dim		*/
	else return(ALPHA_FAC);
#ifdef WRONG
This is not correct. The Wang always uses a '81' fac even for a "PIC 99999".
	else if (strchr(the_pic,'A')) return(ALPHA_FAC);				/* if A or X it's alphanumeric		*/
	else if (strchr(the_pic,'X')) return(ALPHA_FAC);
	return(NUM_FAC);								/* Otherwise numeric			*/
#endif
}

uint4 pic_edit(the_pic)									/* Generate a 32 bit field edit mask.	*/
char *the_pic;
{
	uint4 the_mask,the_bit;
	int i;

	if (!*the_pic || pic_fac(the_pic) != NUM_FAC) return(0);			/* Blank or non numeric.		*/

	the_mask = 0;
	the_bit  = 0x80000000;
	do
	{
		if (*the_pic == ',' |							/* Commas need to be preserved.		*/
		    *the_pic == 'B' |
		    *the_pic == '/' |
		    *the_pic == '0'  )
		{
			the_mask |= the_bit;						/* set the bit in the mask.		*/
		}
		else if (*the_pic == '(')
		{
			i = 0;								/* temp counter				*/
			the_pic++;							/* Point to first digit.		*/
			do
			{
				i = i * 10;						/* shift digits left			*/
				i = i + (*the_pic++ - '0');				/* add the next digit			*/
			} while (*the_pic != ')');					/* till we find the close paren		*/
			i -= 2;
			if (i > 0) the_bit = the_bit >> i;				/* Shift this many bits.		*/
		}
		the_pic++;								/* Next char.				*/
		the_bit = the_bit >> 1;							/* Rotate the bit.			*/
	} while (*the_pic);								/* till we see the null.		*/
	return(the_mask);
}

uint4 pic_zmask(the_pic)								/* Generate a 32 bit field Z edit mask.	*/
char *the_pic;
{
	uint4 the_mask,the_bit;
	int i,zflag;

	the_mask = 0;
	zflag = 0;
	the_bit  = 0x80000000;
	do
	{
		if (*the_pic == 'Z')							/* Commas need to be preserved.		*/
		{
			the_mask |= the_bit;						/* set the bit in the mask.		*/
			zflag = 1;
		}
		else if (*the_pic == '(')
		{
			i = 0;								/* temp counter				*/
			the_pic++;							/* Point to first digit.		*/
			do
			{
				i = i * 10;						/* shift digits left			*/
				i = i + (*the_pic++ - '0');				/* add the next digit			*/
			} while (*the_pic != ')');					/* till we find the close paren		*/

			if (zflag && i>1) the_mask |= the_bit;				/* Set the mask if need be.		*/

			i -= 2;
			if (i > 0)
			{
				for ( ; i; i--)						/* Now move the bit for each position.	*/
				{
					the_bit = the_bit >> 1;				/* Shift this many bits.		*/
					if (zflag) the_mask |= the_bit;			/* Set the mask if need be.		*/
				}
			}
		}
		else
		{
			zflag = 0;							/* Then be sure to clear the flag.	*/
		}
		the_pic++;								/* Next char.				*/
		the_bit = the_bit >> 1;							/* Rotate the bit.			*/
	} while (*the_pic);								/* till we see the null.		*/
	return(the_mask);
}
