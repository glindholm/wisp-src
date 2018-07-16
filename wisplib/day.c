			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif
#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"

DAY(date,dow)									/* Return the day of the week for a given date.	*/
char *date;									/* The date YYMMDD				*/
int4 *dow;									/* The day of the week. 1=sunday		*/
{
#define		ROUTINE		13000

	int4 tmp, rc;
	char	buff[20];

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we're here.				*/

	DATE("GD",date,buff,&rc);
	wswap(&rc);
	if (rc==0)
	{
		switch(buff[0])
		{
		case 'S':
			if (buff[1] == 'A')     tmp = 7;	/* SATURDAY */
			else			tmp = 1;	/* SUNDAY */
			break;
		case 'M':
			tmp = 2;				/* MONDAY */
			break;
		case 'T':
			if (buff[1] == 'U')	tmp = 3;	/* TUESDAY */
			else			tmp = 5;	/* THURSDAY */
			break;
		case 'W':
			tmp = 4;				/* WEDNESDAY */
			break;
		case 'F':
			tmp = 6;				/* FRIDAY */
			break;
		default:
			tmp = 0;
			break;
		}
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
