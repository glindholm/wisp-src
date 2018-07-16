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


#include <string.h>

#include "idsistd.h"
#include "day.h"
#include "movebin.h"
#include "werrlog.h"
#include "wisplib.h"

void DAY(char* date, int4* dow)							/* Return the day of the week for a given date.	*/
										/* The date YYMMDD				*/
										/* The day of the week. 1=sunday		*/
{
#define		ROUTINE		13000

	int4 tmp, rc;
	char	buff[20];

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we're here.				*/

	DATE("GD",date,buff,&rc);
	wswap(&rc);
	if (rc==0)
	{
		convert_day_to_number(buff,&tmp);
	}
	else
	{
		tmp = 0;
	}

	if (tmp==0)
	{
		werrlog(ERRORCODE(2),date,0,0,0,0,0,0,0);			/* Bad date passed.				*/
	}
	else
	{
		wswap(&tmp);
		PUTBIN(dow,&tmp,sizeof(int4));					/* Copy the day of week value			*/
	}
}

/*
**	Routine:	convert_day_to_number()
**
**	Function:	To convert a day to a number.
**
**	Description:	Will return a number for the day of the week for the date.
**			The value returned wis in the range from 1 (Sunday) to
**			7 ( Saturday).
**
**	Arguments:
**	inday		String of the day to convert
**	outnum		output number of the day (0-7)
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/19/97	Written by SMC
**
*/
void convert_day_to_number(char inday[], int4* outnum)
{
	int4 lnum = 0;
	
	switch(inday[0])
	{
	case 'S':
		if (inday[1] == 'A')	lnum = 7;	/* SATURDAY */
		else			lnum = 1;	/* SUNDAY */
		break;
	case 'M':
		lnum = 2;				/* MONDAY */
		break;
	case 'T':
		if (inday[1] == 'U')	lnum = 3;	/* TUESDAY */
		else			lnum = 5;	/* THURSDAY */
		break;
	case 'W':
		lnum = 4;				/* WEDNESDAY */
		break;
	case 'F':
		lnum = 6;				/* FRIDAY */
		break;
	default:
		lnum = 0;
		break;
	}

	*outnum = lnum;
}

/*
**	History:
**	$Log: day.c,v $
**	Revision 1.12  1997/05/20 13:49:14  scass
**	Changed convert_day_to_number() to use a local
**	variable.
**	
**	Revision 1.11  1997-05-20 07:45:46-04  scass
**	Moved some code into a seperate routine so could call
**	from date.c
**	routine convert_dat_to_number()
**
**	Revision 1.10  1996-08-19 18:32:15-04  gsl
**	drcs update
**
**
**
*/
