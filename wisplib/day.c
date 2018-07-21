/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/



#include <string.h>

#include "idsistd.h"
#include "day.h"
#include "werrlog.h"
#include "wisplib.h"
#include "vssubs.h"

void DAY(char* date, int4* dow)							/* Return the day of the week for a given date.	*/
										/* The date YYMMDD				*/
										/* The day of the week. 1=sunday		*/
{
	int4 tmp, rc;
	char	buff[20];

	WL_wtrace("DAY","ENTRY", "Date=[%6.6s]", date);

	if (WL_useoldvsdate())
	{
		WISPDATE("GD",date,buff,&rc);
		WL_wswap(&rc);
		if (rc==0)
		{
			WL_convert_day_to_number(buff,&tmp);
		}
		else
		{
			tmp = 0;
		}

		if (tmp==0)
		{
			werrlog(WERRCODE(13002),date,0,0,0,0,0,0,0);	
		}
		else
		{
			WL_put_swap(dow,tmp);		
		}
	}
	else
	{
		DATE6("G#",date, dow, &rc);
	}

}

/*
**	Routine:	WL_convert_day_to_number()
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
void WL_convert_day_to_number(char inday[], int4* outnum)
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
**	Revision 1.20  2003/03/27 21:23:17  gsl
**	DATE6 changes
**	
**	Revision 1.19  2003/03/20 19:05:14  gsl
**	Change references fo DATE to WISPDATE
**	
**	Revision 1.18  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.17  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.16  2002/12/10 17:09:20  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.15  2002/12/09 21:09:27  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.14  2002/07/12 17:00:54  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.13  2002/07/11 20:29:07  gsl
**	Fix WL_ globals
**	
**	Revision 1.12  1997/05/20 13:49:14  scass
**	Changed WL_convert_day_to_number() to use a local
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
