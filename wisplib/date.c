			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		date.c
**
**	Purpose:	To hold DATE vssub routine
**
**	Routines:	DATE		The DATE entry point
**
**
**	History:
**	mm/dd/yy	Written by ...
**	11/24/89	revamped by Jock Cooper
**	09/22/92	Totaly rewritten by Greg Lindholm
**
*/

#include <stdio.h>
#include <math.h>

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#ifndef VMS	/* unix or MSDOS */
#include <sys/types.h>
#endif

#include <ctype.h>
#include <time.h>


#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"

static int4 diff_dates();
static int load_date();
static int unload_date();
static int adjust_date();

struct date_struct
{
	int4	year;
	int4    days;
};
typedef struct date_struct date_struct;

/* no_days[0] is for reg yrs, no_days[1] is for leap yrs */
static int no_days[2][12] = 
		{
		{  31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 },
		{  31,  29,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 }
		};
static int off_days[2][13] =
		{
		{   0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
		{   0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
		};


/*
**	Routine:	DATE()
**
**	Function:	To emulate the vssub DATE (with extensions)
**
**	Description:	This routine does various date calculations based on a function code.
**			On Wang it accepts dates in two formats
**				Gregorian	YYMMDD
**				Julian		YYDDD
**				X		YYYYMMDD
**				R		YYYYDDD
**
**			"HD"	Return date as UPPERCASE formatted string
**			"HL"	Return date as UP/low formatted string
**			"G-"	Find difference in days between two dates
**			"J-"	Find difference in days between two dates
**			"X-"	Find difference in days between two dates
**			"G+"	Calc new date by adding count number of days
**			"J+"	Calc new date by adding count number of days
**			"X+"	Calc new date by adding count number of days
**			"GD"	Return day of week
**			"JD"	Return day of week
**			"XD"	Return day of week
**			"GJ"	Convert Gregorian date to Julian date
**			"JG"	Convert Julian date to Gregorian date
**
**	Arguments:
**	"HD",receiver
**	"HL",receiver
**	"G-",G1,G2,diff,retcode
**	"J-",J1,J2,diff,retcode
**	"X-",X1,X2,diff,retcode
**	"G+",G1,count,G2,retcode
**	"J+",J1,count,J2,retcode
**	"X+",X1,count,X2,retcode
**	"GD",G1,day,retcode
**	"JD",J1,day,retcode
**	"XD",X1,day,retcode
**	"GJ",G1,J1,retcode
**	"JG",J1,G1,retcode
**	"XR",J1,G1,retcode
**	"RX",J1,G1,retcode
**	"XG",J1,G1,retcode
**	"GX",J1,G1,retcode
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	09/22/92	Written by GSL
**
*/
DATE(func,arg1,arg2,arg3,arg4)
char *func;
char *arg1,*arg2,*arg3,*arg4;
{
#define		ROUTINE		11000
	static char *weekday_string[7] = { "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday" };

	static char *dow_string[7] = { "SUNDAY   ","MONDAY   ","TUESDAY  ","WEDNESDAY","THURSDAY ","FRIDAY   ","SATURDAY " };

	static char *month_string[12] = { "January","February","March","April","May","June","July",
					  "August","September","October","November","December" };

	static char *am_pm_string[2] = { "AM","PM" };

	int i,j;
	char temp[80], temp2[80];
	int4 	rc, tlong1, tlong2;
	date_struct	date1, date2;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we're here				*/

	if (0==memcmp(func,"HL",2) ||
	    0==memcmp(func,"HD",2)   )						/* HL -- upper/lower case header, HD -- upper	*/
	{
		/*
		**	Display the current date/time in the following format.
		**
		**	AAAAAAAAA   BBBBBBBBBBBBBBBBBB       CCCCCCCC
		**	Wednesday   December 30, 1987         2:30 PM
		*/
		struct tm *time_struct;
		time_t 	time_val;
		int	am_pm;

		time(&time_val);						/* get current time 				*/
		time_struct = localtime(&time_val);				/* break it down into parts			*/

		if (time_struct->tm_hour == 12)					/* If the hour is 12 o'clock			*/
		{
			am_pm = 1;						/* use 'PM'					*/
		}
		else if (time_struct->tm_hour > 12)
		{
			time_struct->tm_hour -= 12;				/* convert to 12 hour clock			*/
			am_pm = 1;						/* use 'PM'					*/
		}
		else if (time_struct->tm_hour == 0)				/* If hour is zero, it's 12 AM			*/
		{
			time_struct->tm_hour = 12;				/* convert to 12 hour clock			*/
			am_pm = 0;						/* use 'AM'					*/
		}
		else
		{
			am_pm = 0;						/* use 'AM'					*/
		}
										/* build the day of the month string		*/
		sprintf(temp,"%s %d, %d",month_string[time_struct->tm_mon],time_struct->tm_mday,1900+time_struct->tm_year);
										/* write the actual time string			*/

		sprintf(temp2,"%9s   %-18s       %2d:%02d %2s",weekday_string[time_struct->tm_wday],temp,
								time_struct->tm_hour,time_struct->tm_min,am_pm_string[am_pm]);

		memcpy(arg1,temp2,45);

		if (func[1] == 'D') for (j=0;j<24;j++) arg1[j]=toupper(arg1[j]);    /* If they want upper case.			*/

	}
	else if (0==memcmp(func,"G-",2) ||
		 0==memcmp(func,"J-",2) ||
		 0==memcmp(func,"X-",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg4,&rc,4);
			return(0);
		}
		if (rc = load_date(func[0],arg2,&date2))
		{
			wswap(&rc);
			PUTBIN(arg4,&rc,4);
			return(0);
		}
		tlong1 = diff_dates(&date1,&date2);
		wswap(&tlong1);
		PUTBIN(arg3,&tlong1,4);

		PUTBIN(arg4,&rc,4);
		return(0);
	}
	else if (0==memcmp(func,"G+",2) ||
		 0==memcmp(func,"J+",2) ||
		 0==memcmp(func,"X+",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg4,&rc,4);
			return(0);
		}
		GETBIN(&tlong1,arg2,4);
		wswap(&tlong1);

		adjust_date(&date1,tlong1);
		rc = unload_date(func[0],arg3,&date1);
		wswap(&rc);

		PUTBIN(arg4,&rc,4);
		return(0);
	}
	else if (0==memcmp(func,"GD",2) ||
		 0==memcmp(func,"JD",2) ||
		 0==memcmp(func,"XD",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg3,&rc,4);
			return(0);
		}
		load_date('G',"010106",&date2);					/* Jan 06, 1901 is a Sunday			*/
		tlong1 = diff_dates(&date1,&date2) % 7;
		if (tlong1 < 0) tlong1 = -tlong1;
		memcpy(arg2,dow_string[tlong1],9);				/* Copy the DOW string.				*/

		PUTBIN(arg3,&rc,4);
		return(0);
	}
	else if (0==memcmp(func,"GJ",2) ||
		 0==memcmp(func,"GX",2) ||
		 0==memcmp(func,"GR",2) ||
		 0==memcmp(func,"JG",2) ||
		 0==memcmp(func,"JX",2) ||
		 0==memcmp(func,"JR",2) ||
		 0==memcmp(func,"XG",2) ||
		 0==memcmp(func,"XJ",2) ||
		 0==memcmp(func,"XR",2) ||
		 0==memcmp(func,"RG",2) ||
		 0==memcmp(func,"RJ",2) ||
		 0==memcmp(func,"RX",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg3,&rc,4);
			return(0);
		}
		rc = unload_date(func[1],arg2,&date1);
		wswap(&rc);

		PUTBIN(arg3,&rc,4);
		return(0);
	}
	else
	{
		werrlog(ERRORCODE(2),func[0],func[1],0,0,0,0,0,0);		/* Say not implemented.				*/
	}
}

/*
**	Routine:	leap_year()
**
**	Function:	To test if a given year is a leap year.
**
**	Description:	A leap year is divisable by 4 except century years unless divisable by 400.
**			The return value is meant to be used in no_days[][] and off_days[][].
**
**	Arguments:
**	year		The year to test
**
**	Globals:	None
**
**	Return:
**	0		Not a leap year
**	1		A leap year
**
**	Warnings:	None
**
**	History:	
**	09/22/92	Written by GSL
**
*/
static int leap_year(year)
int4	year;
{
	if (0 == year % 400) 	return 1;
	if (0 == year % 100) 	return 0;
	if (0 == year % 4)	return 1;
	return 0;
}

/*
**	Routine:	load_date()
**
**	Function:	To load a character string date into the date_struct.
**
**	Description:	This routine loads and validates the dates.
**
**	Arguments:
**	type		The type of date "G", "J", "X", "R"
**				"G"	YYMMDD
**				"J"	YYDDD
**				"X"	YYYYMMDD
**				"R"	YYYYDDD
**	string		The character string get the date from
**	date		The date_struct to be loaded
**
**	Globals:
**	no_days		Array of number of days in each month
**	off_days	Array of offset number of days in each month
**
**	Return:
**	0		Success
**	8		Invalid date
**	16		Invalid type
**
**	Warnings:	ON error the date field may have been modified.
**
**	History:	
**	09/22/92	Written by GSL
**
*/

static int load_date(type,string,date)
char	type;
char	*string;
date_struct *date;
{
	int	i, len, leap;
	int	month, day;

	switch(type)
	{
	case 'G': len=6; break;
	case 'J': len=5; break;
	case 'X': len=8; break;
	case 'R': len=7; break;
	default: return(16);
	}

	/*
	**	Check that each char is a digit
	*/
	for (i = 0; i < len; i++)
	{
		if (string[i] < '0' || string[i] > '9')
		{
			return(8);
		}
	}

	/*
	**	get the year
	*/
	switch(type)
	{
	case 'G':
	case 'J':
		date->year=	1900 + (string[0]-'0')*10 + (string[1]-'0');
		break;

	case 'X':	
	case 'R':
		date->year=	(string[0]-'0')*1000 + (string[1]-'0')*100 + (string[2]-'0')*10 + (string[3]-'0');
		break;
	}

	leap = leap_year(date->year);

	/*
	**	get the number of days
	*/
	switch(type)
	{
	case 'G':
	case 'X':	
		month=	(string[len-4]-'0')*10+(string[len-3]-'0');
		day=	(string[len-2]-'0')*10+(string[len-1]-'0');

		if (month < 1 || month > 12) 			return(8);
		if (day < 1 || day > no_days[leap][month-1]) 	return(8);

		date->days = off_days[leap][month-1] + day;
		break;

	case 'J':
	case 'R':
		date->days = 	(string[len-3]-'0')*100 + (string[len-2]-'0')*10 + (string[len-1]-'0');
		if (date->days < 1 || date->days > off_days[leap][12]) return(8);
		break;
	}

	return(0);
}

/*
**	Routine:	unload_date()
**
**	Function:	To unload a date_struct into character string date.
**
**	Description:	This routine unloads and validates the dates.
**
**	Arguments:
**	type		The type of date "G", "J", "X", "R"
**				"G"	YYMMDD
**				"J"	YYDDD
**				"X"	YYYYMMDD
**				"R"	YYYYDDD
**	string		The character string to load the date into.
**	date		The date_struct to get the date from.
**
**	Globals:
**	no_days		Array of number of days in each month
**	off_days	Array of offset number of days in each month
**
**	Return:
**	0		Success
**	4		Year not 1900 for "G" and "J". For "X" and "R" not 0 - 9999
**	8		Invalid date
**	16		Invalid type
**
**	Warnings:	ON error the date field may have been modified.
**
**	History:	
**	09/22/92	Written by GSL
**
*/

static int unload_date(type,string,date)
char	type;
char	*string;
date_struct *date;
{
	char	buff[80];
	int	i, len, leap;
	int	month, day;

	switch(type)
	{
	case 'G': len=6; break;
	case 'J': len=5; break;
	case 'X': len=8; break;
	case 'R': len=7; break;
	default: return(16);
	}

	memset(string,' ',len);
	leap = leap_year(date->year);

	if (date->days < 1 || date->days > off_days[leap][12]) return(8);

	/*
	**	unload the year
	*/
	switch(type)
	{
	case 'G':
	case 'J':
		if (date->year < 1900 || date->year > 1999) return(4);
		sprintf(buff,"%02d", date->year - 1900);
		string[0] = buff[0];
		string[1] = buff[1];
		break;

	case 'X':	
	case 'R':
		if (date->year < 0 || date->year > 9999) return(4);
		sprintf(buff,"%04d", date->year);
		string[0] = buff[0];
		string[1] = buff[1];
		string[2] = buff[2];
		string[3] = buff[3];
		break;
	}


	/*
	**	unload the number of days
	*/
	switch(type)
	{
	case 'G':
	case 'X':
		for(i=11; i>=0; i--)
		{
			if (date->days > off_days[leap][i]) break;
		}
		month = i+1;
		day = date->days - off_days[leap][i];

		sprintf(buff,"%02d%02d",month,day);
		string[len-4] = buff[0];
		string[len-3] = buff[1];
		string[len-2] = buff[2];
		string[len-1] = buff[3];
		break;

	case 'J':
	case 'R':
		sprintf(buff,"%03d", date->days);
		string[len-3] = buff[0];
		string[len-2] = buff[1];
		string[len-1] = buff[2];
		break;
	}

	return(0);
}


/*
**	Routine:	diff_dates()
**
**	Function:	To find the difference between two dates in days.
**
**	Description:	The difference is the number of days you must add to date1 to get date2. 
**			This number may be negative.
**
**	Arguments:
**	date1		The starting date
**	date2		The ending date
**
**	Globals:
**	off_days	Array of offset number of days in each month
**
**	Return:		The difference in days
**
**	Warnings:	None
**
**	History:	
**	09/22/92	Written by GSL
**
*/
static int4 diff_dates(date1,date2)
date_struct	*date1, *date2;
{
	int4	diff;
	date_struct	datex;

	memcpy((char *)&datex, (char *)date1, sizeof(datex));				/* Make working copy of starting date	*/

	/*
	**	Loop until the years match.   Keep a running total of diff.
	*/
	for(diff=0; datex.year != date2->year; )
	{
		if (datex.year < date2->year)		/* Diff is positive */
		{
			diff += off_days[leap_year(datex.year)][12];
			datex.year += 1;
		}
		else					/* Diff is negative */
		{
			datex.year -= 1;
			diff -= off_days[leap_year(datex.year)][12];
		}
	}

	diff += (date2->days - datex.days);						/* Calc the diff in days		*/
	return(diff);									/* Return the diff			*/
}

/*
**	Routine:	adjust_date()
**
**	Function:	To adjust date by count number of days
**
**	Description:	This routine adds count number of days to date. Count may be negative.
**
**	Arguments:
**	date		The date to adjust
**	count		The number of days to add
**
**	Globals:
**	off_days	Array of offset number of days in each month
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	09/22/92	Written by GSL
**
*/
static int adjust_date(date,count)
date_struct	*date;
int4		count;
{
	date->days += count;

	for(;;)
	{
		if (date->days < 1)
		{
			date->year -= 1;
			date->days += off_days[leap_year(date->year)][12];
		}
		else if (date->days > off_days[leap_year(date->year)][12])
		{
			date->days -= off_days[leap_year(date->year)][12];
			date->year += 1;
		}
		else
		{
			return(0);
		}
	}
}
