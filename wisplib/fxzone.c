static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**      File:           fxzone.c
**
**      Purpose:        To convert Wang COBOL signed numeric values (ZONED) to the appropriate ACUCOBOL,
**			Micro Focus COBOL, LPI COBOL, or VAX COBOL signed numeric values.
**
**      Routines:       FXZONE()	The fxzone utility main routine.
**			do_conv_neg()	Do the convertion of the negative character
**			do_conv_pos()	Do the convertion of the positive character
**
**      History:
**			12/16/92	Written by SMC
*/

#include <stdio.h>

#define ACU	'A'
#define VAX	'V'
#define MF	'M'
#define LPI	'L'
#include "idsistd.h"

static void do_conv_neg(unsigned char *lang,unsigned char *cptr);		/* Do convertion of the negative char.	*/
static void do_conv_pos(unsigned char *lang,unsigned char *cptr);		/* Do convertion of the positive char.	*/

/*
**      Routine:        FXZONE()
**
**      Function:       To setup the fields and verify if need converting.
**
**      Description:    This routine sets the local pointers to the input fields and then steps
**			to the position in the field that needs converting.  Then calls
**			do_conv_neg() or do_conv_pos() to do the actual character convertion
**			depending on the language that is in use (p1).
**
**	Input:
**			p1	ptr to the 3 char language indicator (ACU, MF, or VAX)
**			p2	ptr to the zoned field name
**			p3	ptr to the length of the field.
**
**	Outout:
**			the "fixed" zoned byte of the field (p2)
**
**	Return:		None
**
**	Warnings:	None
**
**      History:        12/16/92        Written by SMC
**			12/29/92	Added other language specific changes.  SMC
*/

void FXZONE(unsigned char *clang,unsigned char *fld,unsigned short *len)									/* num field to convert, & length in	*/
{											/* bytes of num field.			*/
	unsigned char *spos;								/* Ptr to byte position in num field.	*/
	unsigned char *language;	
	int l_len;
	register int i;

	l_len = *len;									/* Set the local length of the field.	*/
	language = clang;
	spos = fld;									/* Set ptr to num field.		*/

	for (i = 0; i <= l_len; i++)							/* Step to the zoned position.		*/
	{
		if ( *spos >= 0xD0 && *spos <= 0xD9 )
		{
			do_conv_neg(language,spos);
		}
		else if (( *spos >= 0xF0 && *spos <= 0xF9 ) || 
			 ( *spos >= 0xC0 && *spos <= 0xC9 ))
		{
			do_conv_pos(language,spos);
		}

		spos++;
	}

}

static void do_conv_neg(unsigned char *lang,unsigned char *cptr)			/* Do convertion of the negative char.	*/
{
	switch (lang[0])								/* Select the appropriate language.	*/
	{
		case ACU: case VAX: case LPI:
		{
			switch(*cptr)							/* Check if sign byte set to negative	*/
			{								/* using Wang values.			*/
				case 0xD0:
					*cptr = '}';					/* convert negative 0, 0xD0 to 0x7D	*/
					break;
				case 0xD1:
					*cptr = 'J';					/* convert negative 1, 0xD1 to 0x4A	*/
					break;
				case 0xD2:
					*cptr = 'K';					/* convert negative 2, 0xD2 to 0x4B	*/
					break;
				case 0xD3:
					*cptr = 'L';					/* convert negative 3, 0xD3 to 0x4C	*/
					break;
				case 0xD4:
					*cptr = 'M';					/* convert negative 4, 0xD4 to 0x4D	*/
					break;
				case 0xD5:
					*cptr = 'N';					/* convert negative 5, 0xD5 to 0x4E	*/
					break;
				case 0xD6:
					*cptr = 'O';					/* convert negative 6, 0xD6 to 0x4F	*/
					break;
				case 0xD7:
					*cptr = 'P';					/* convert negative 7, 0xD7 to 0x50	*/
					break;
				case 0xD8:
					*cptr = 'Q';					/* convert negative 8, 0xD8 to 0x51	*/
					break;
				case 0xD9:
					*cptr = 'R';					/* convert negative 9, 0xD9 to 0x52	*/
					break;
				default:
					break;
			}
			break;
		}

		case MF:
		{
			switch(*cptr)							/* Check if sign byte set to negative	*/
			{								/* using Wang values.			*/
				case 0xD0:
					*cptr = 'p';					/* convert negative 0, 0xD0 to 0x70	*/
					break;
				case 0xD1:
					*cptr = 'q';					/* convert negative 1, 0xD1 to 0x71	*/
					break;
				case 0xD2:
					*cptr = 'r';					/* convert negative 2, 0xD2 to 0x72	*/
					break;
				case 0xD3:
					*cptr = 's';					/* convert negative 3, 0xD3 to 0x73	*/
					break;
				case 0xD4:
					*cptr = 't';					/* convert negative 4, 0xD4 to 0x74	*/
					break;
				case 0xD5:
					*cptr = 'u';					/* convert negative 5, 0xD5 to 0x75	*/
					break;
				case 0xD6:
					*cptr = 'v';					/* convert negative 6, 0xD6 to 0x76	*/
					break;
				case 0xD7:
					*cptr = 'w';					/* convert negative 7, 0xD7 to 0x77	*/
					break;
				case 0xD8:
					*cptr = 'x';					/* convert negative 8, 0xD8 to 0x78	*/
					break;
				case 0xD9:
					*cptr = 'y';					/* convert negative 9, 0xD9 to 0x79	*/
					break;
				default:
					break;
			}
			break;
		}

		default:
			printf("\nLanguage (%s) not supported",lang);
			break;
	}
}

static void do_conv_pos(unsigned char *lang,unsigned char *cptr)		/* Do convertion of the positive char.	*/
{
	switch (lang[0])								/* Select the appropriate language.	*/
	{
		case ACU: case VAX: case MF: case LPI:
		{
			switch(*cptr)							/* Check if sign byte set to positive	*/
			{								/* using Wang values.			*/
				case 0xF0:
				case 0xC0:
					*cptr = '0';					/* Convert positive 0, 0xF0 to 0x30	*/
					break;
				case 0xF1:
				case 0xC1:
					*cptr = '1';					/* Convert positive 1, 0xF1 to 0x31	*/
					break;
				case 0xF2:
				case 0xC2:
					*cptr = '2';					/* Convert positive 2, 0xF2 to 0x32	*/
					break;
				case 0xF3:
				case 0xC3:
					*cptr = '3';					/* Convert positive 3, 0xF3 to 0x33	*/
					break;
				case 0xF4:
				case 0xC4:
					*cptr = '4';					/* Convert positive 4, 0xF4 to 0x34	*/
					break;
				case 0xF5:
				case 0xC5:
					*cptr = '5';					/* Convert positive 5, 0xF5 to 0x35	*/
					break;
				case 0xF6:
				case 0xC6:
					*cptr = '6';					/* Convert positive 6, 0xF6 to 0x36	*/
					break;
				case 0xF7:
				case 0xC7:
					*cptr = '7';					/* Convert positive 7, 0xF7 to 0x37	*/
					break;
				case 0xF8:
				case 0xC8:
					*cptr = '8';					/* Convert positive 8, 0xF8 to 0x38	*/
					break;
				case 0xF9:
				case 0xC9:
					*cptr = '9';					/* Convert positive 9, 0xF9 to 0x39	*/
					break;
				default:
					break;
			}
			break;
		}

		default:
			printf("\nLanguage (%s) not supported",lang);
			break;
	}
}
/*
**	History:
**	$Log: fxzone.c,v $
**	Revision 1.7  1996/08/19 22:32:22  gsl
**	drcs update
**	
**
**
*/
