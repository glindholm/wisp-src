			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* sub: date -- various date functions												*/
/*
 * code revamp on 11/24/89 by Jock Cooper
 *
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

#include "movebin.h"
#include "werrlog.h"

long	dates_calc();
/* External variables for holding results of parsing of date values */
int month_1, day_1, year_1, month_2, day_2, year_2;
int new_century;

/* no_days[0] is for reg yrs, no_days[1] is for leap yrs */
int no_days[2][12] = 
		{
		{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
		{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
		};

/* 			Do date/time conversion functions for WANG cobol							*/

DATE(func,arg1,arg2,arg3,arg4)
char *func;
char *arg1,*arg2,*arg3,*arg4;
{
#define		ROUTINE		11000

	struct tm *time_struct;
	int i,j,mn1,dy1,yr1,mn2,dy2,yr2;
	time_t time_val;
	char temp[20];
	long int tlong1,tlong2;
	char *off_calc();

	static char *weekday[7] = { "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday" };

	static char *the_day[7] = { "SUNDAY   ","MONDAY   ","TUESDAY  ","WEDNESDAY","THURSDAY ","FRIDAY   ","SATURDAY " };

	static char *month[12] = { "January","February","March","April","May","June","July",
				   "August","September","October","November","December" };

	static char *hour[2] = { "A","P" };

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we're here				*/

	if (func[0] == 'H' && (func[1] == 'L' || func[1] == 'D'))		/* HL -- upper/lower case header, HD -- upper	*/
	{
		time(&time_val);						/* get current time 				*/
		time_struct = localtime(&time_val);				/* break it down into parts			*/

		if (time_struct->tm_hour == 12)					/* If the hour is 12 o'clock			*/
		{
			i = 1;							/* use 'PM'					*/
		}
		else if (time_struct->tm_hour > 12)
		{
			time_struct->tm_hour -= 12;				/* convert to 12 hour clock			*/
			i = 1;							/* use 'PM'					*/
		}
		else if (time_struct->tm_hour == 0)				/* If hour is zero, it's 12 AM			*/
		{
			time_struct->tm_hour = 12;				/* convert to 12 hour clock			*/
			i = 0;							/* use 'AM'					*/
		}
		else
		{
			i = 0;							/* use 'AM'					*/
		}
										/* build the day of the month string		*/
		sprintf(temp,"%s %d, 19%02d",month[time_struct->tm_mon],time_struct->tm_mday,time_struct->tm_year);
										/* write the actual time string			*/

		sprintf(arg1,"%9s   %-18s       %2d:%02d %s",weekday[time_struct->tm_wday],temp,
								time_struct->tm_hour,time_struct->tm_min,hour[i]);
		arg1[44] = 'M';							/* add on the 'M'				*/

		if (func[1] == 'D') for (j=0;j<24;j++) arg1[j]=toupper(arg1[j]);    /* If they want upper case.			*/

	}
	else if (func[0] == 'G')						/* Gregorian date functions			*/
	{
		if (func[1] == '-')						/* find difference between 2 dates		*/
		{
			if (valid_date(arg1,1)<0) 
			{
				tlong1 = 8;
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,(sizeof(long)));
				return(0);
			}
			if (valid_date(arg2,2)<0)
			{
				tlong1 = 8;
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,(sizeof(long)));
				return(0);
			}
			tlong1 = dates_calc();

			wswap(&tlong1);						/* swap the order of the words			*/

			PUTBIN(arg3,&tlong1,sizeof(long));

			memset(arg4,(char)0,sizeof(long));
			return(0);
		}
		else if (func[1] == '+')					/* add number of days and find date		*/
		{
			if (valid_date(arg1,1)<0) 
			{
				tlong1 = 8;
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,(sizeof(long)));
				return(0);
			}

			GETBIN(&tlong1,arg2,sizeof(long));
			wswap(&tlong1);			 			/* swap the words				*/
			sprintf(temp,"%d",tlong1);
			new_century=0;
			strncpy(arg3,off_calc(temp),6);

			if (!new_century)
				memset(arg4,0,sizeof(long));
			else
			{
				tlong1 = 4;
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,sizeof(long));
			}
			return(0);
		}
		else if (func[1] == 'D')					/* Gregorian day of the week.			*/
		{
			valid_date("010106",1);					/* Jan 06, 1901 is a Sunday			*/
			if (valid_date(arg1,2)<0) 
			{
				tlong1 = 8;
				wswap(&tlong1);
				PUTBIN(arg3,&tlong1,(sizeof(long)));
				return(0);
			}
			tlong1 = dates_calc() % 7;
			memcpy(arg2,the_day[tlong1],9);				/* Copy the DOW string.				*/
			memset(arg3,0,sizeof(long));
			return(0);
		}
		else if (func[1] == 'J')					/* Convert Gregorian to Julian.			*/
		{
			FILE *tmpf;

			if (valid_date(arg1,1)<0) 
			{
				tlong1 = 8;
				wswap(&tlong1);
				PUTBIN(arg3,&tlong1,(sizeof(long)));
				return(0);
			}

			for (tlong1=0, tlong2=0; tlong2 < month_1-1; ++tlong2)
				tlong1 += no_days[!((year_1%4)!=0)][tlong2];

			sprintf(temp,"%02d%03d",year_1,tlong1+day_1);

			memcpy(arg2,temp,5);					/* copy it to the output stream			*/

			memset(arg3,0,sizeof(long));				/* return code					*/
			return(0);
		}
	}
	else if (func[0] == 'J')						/* Julian day functions				*/
	{
		if (func[1] == 'G')						/* convert Julian to Gregorian			*/
		{
			tlong1 = jul2greg(arg1,arg2);
			wswap(&tlong1);
			PUTBIN(arg3,&tlong1,sizeof(long));			/* return code					*/
			return(0);
		}
		else if (func[1] == '-')					/* Subtract Julian dates			*/
		{
			char greg1[10], greg2[10];

			if (tlong1 = jul2greg(arg1,greg1))
			{
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,sizeof(long));		/* return code					*/
				return(0);
			}
			if (tlong1 = jul2greg(arg2,greg2))
			{
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,sizeof(long));		/* return code					*/
				return(0);
			}

			valid_date(greg1,1);
			valid_date(greg2,2);			

			tlong1 = dates_calc();					/* Get the diff.				*/

			wswap(&tlong1);

			PUTBIN(arg3,&tlong1,sizeof(long));

			memset(arg4,(char)0,sizeof(long));
			return(0);
		}
		else if (func[1] == '+')					/* Add Julian dates			*/
		{
			char greg1[10], greg2[10];

			memset(greg1,0,sizeof(greg1));
			memset(greg2,0,sizeof(greg2));
			if (tlong1=jul2greg(arg1,greg1))			/* convert starting date to Gregorian		*/
			{
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,sizeof(long));
				return(0);
			}
			valid_date(greg1,1);					/* and digest it				*/

			GETBIN(&tlong1,arg2,sizeof(long));			/* get days to add parm				*/
			wswap(&tlong1);
			sprintf(temp,"%d",tlong1);				/* make it an ascii string			*/
			strncpy(greg2,off_calc(temp),6);			/* add the dates				*/
			valid_date(greg2,2);					/* process the resulting date			*/
			sprintf(greg1,"%02d%02d%02d",year_2,1,1);		/* change date1 to be Jan 1 of resulting year	*/
			valid_date(greg1,1);				
			new_century=0;
			tlong1=dates_calc()+1;					/* to find julian days figure in resulting date	*/
			sprintf(temp,"%02d%03d",year_2,tlong1);			/* rebuild julian date				*/
			strncpy(arg3,temp,5);			

			if (!new_century)
				memset(arg4,(char)0,sizeof(long));
			else
			{
				tlong1 = 4;
				wswap(&tlong1);
				PUTBIN(arg4,&tlong1,sizeof(long));
			}
			return(0);
		}
		else if (func[1] == 'D')					/* Gregorian day of the week.			*/
		{
			char greg1[10], greg2[10];

			if (tlong1=jul2greg(arg1,greg1))			/* convert starting date to Gregorian		*/
			{
				wswap(&tlong1);
				PUTBIN(arg3,&tlong1,sizeof(long));
				return(0);
			}
			valid_date("010106",1);					/* Jan 06, 1901 is a Sunday			*/
			if (valid_date(greg1,2)<0) 
			{
				tlong1 = 8;
				wswap(&tlong1);
				PUTBIN(arg3,&tlong1,(sizeof(long)));
				return(0);
			}
			tlong1 = dates_calc() % 7;
			memcpy(arg2,the_day[tlong1],9);				/* Copy the DOW string.				*/
			memset(arg3,0,sizeof(long));
			return(0);
		}
	}
	else
	{
		werrlog(ERRORCODE(2),func[0],func[1],0,0,0,0,0,0);		/* Say not implemented.				*/
	}
}
int jul2greg(jul,greg)
char *jul,*greg;
{
	int dy,i;
	char temp[10], *off_calc();
	char julian[6];
	char tgreg[10];
	
	memset(julian,0,sizeof(julian));
	memcpy(julian,jul,5);

	for(i=0; i<5; i++)
	{
		if ( jul[i] < '0' || jul[i] > '9' ) return(8);
	}

	year_1 = (julian[0]-0x30)*10 + (julian[1]-0x30);
	dy = (julian[2]-0x30)*100 + (julian[3]-0x30)*10 + (julian[4]-0x30);

	if (dy < 1 || dy > 366) return(8);

	month_1 = 1; day_1 = 1;
	sprintf(temp,"%d",dy-1);
	strncpy(tgreg,off_calc(temp),6);

	if ( jul[0] != tgreg[0] || jul[1] != tgreg[1] ) return(8);

	memcpy(greg,tgreg,6);
	return(0);
}
/**
 ** The following code has been borrowed from:
 ** Author:		Gordon A. Runkle				ORI/Calculon
 **		...uunet!men2a!1solaria!gordon
 **
 ** (actual source: usenet archives -- site: uunet    [comp.sources.misc, volume 3, calcdate]
 **                 uunet!/usr/spool/ftp/comp.sources.misc/volume3/calcdate.Z
 ** )
 **/

/** var declarations moved to top of file 		**/

#define REG	0
#define LEAP	1
#define NEG	0
#define POS	1


/***************************************/
char *off_calc(offset)
char offset[];
{
extern int no_days[2][12];
extern int month_1, day_1, year_1;
extern char *prog;

int atoi();
int i_offset, n_month, n_day, n_year, month_bal;
int i, leap_flag;

static char newdate[7];


/* This checks for a valid offset value.  Negative values are allowed
   and checked for.  It stops at the first null. */
for (i = 0; i < 4; i++)
	{
	if (offset[i] == '\0')
		break;

	if (i == 0 && offset[i] == '-')
		continue;

	if (offset[i] < '0' || offset[i] > '9')
		{
		return((char *) -1);
		}
	}

i_offset = atoi(offset);

/* This is the beginning of the neat stuff.  I hope it works! */
/* leap year is when =>>  year % 4 == 0  */

n_year = year_1;	/* the *_1 is used, as this is the value of the */
n_month = month_1;	/* first date entered */
n_day = day_1;

if (i_offset >= 0)
	{
	while (i_offset > 0)
		{
		if (n_year % 4 == 0)
			leap_flag = LEAP;
		  else
			leap_flag = REG;

		month_bal = no_days[leap_flag][n_month - 1] - n_day;

		if (i_offset > month_bal)
			{
			i_offset -= month_bal;
			n_month++;

			if (n_month > 12)
				{
				n_month = 1;
				n_year++;

				if (n_year > 99)
				  {
					n_year = 0;
					++new_century;
				  }
				}

			n_day = 0;
			}
		  else
			{
			n_day += i_offset;
			i_offset = 0;
			}
		}
	}
else
	{
	while (i_offset < 0)		/* this loop processes neg offsets */
		{
		if (n_year % 4 == 0)
			leap_flag = LEAP;
		  else
			leap_flag = REG;

		month_bal = n_day - 1;

		if (abs(i_offset) > month_bal)
			{
			i_offset += month_bal;
			n_month--;

			if (n_month < 1)
				{
				n_month = 12;
				n_year--;

				if (n_year < 0)
				  {
					n_year = 99;
					++new_century;
				  }
				}

			n_day = no_days[leap_flag][n_month - 1] + 1;
			}
		  else
			{
			n_day += i_offset;
			i_offset = 0;
			}
		}
	}

sprintf(newdate, "%02d%02d%02d", n_year, n_month, n_day);
return newdate;
}


/***************************************/
long	dates_calc()
{
extern int no_days[2][12];
extern int month_1, day_1, year_1, month_2, day_2, year_2;

int first_rec = 0;
long	curr_offset = 0;
int leap_flag, sign_flag;
int start_day, start_month, start_year, end_day, end_month, end_year;


/****
	This section determines which date is later, so that the program
	may evaluate the earlier one first.  There is a flag set to indicate
	what sign the end result should have based on whether the first date
	entered is earlier or later than the second.
****/

/* set the default sign */
sign_flag = NEG;

if (year_1 < year_2)
	sign_flag = POS;
  else
	if (year_1 == year_2 && month_1 < month_2)
		sign_flag = POS;
	  else
		if (year_1 == year_2 && month_1 == month_2 && day_1 < day_2)
			sign_flag = POS;

/* This makes the earlier date be set to start_* */
if (sign_flag == POS)
	{
	start_day = day_1;
	start_month = month_1;
	start_year = year_1;
	end_day = day_2;
	end_month = month_2;
	end_year = year_2;
	}
  else
	{
	start_day = day_2;
	start_month = month_2;
	start_year = year_2;
	end_day = day_1;
	end_month = month_1;
	end_year = year_1;
	}

/* The calculations below keep incrementing curr_offset and start_* until
   start_* == end_*  */

for (;;)
	{
	if (start_year % 4 == 0)
		leap_flag = LEAP;
	  else
		leap_flag = REG;

	if (first_rec == 0)
		{
		/* This is for when the month and year start out the same, and 
		   the user just wants the days (ie.  051688 052688 */
		if (start_month == end_month && start_year == end_year)
			{
			curr_offset = end_day - start_day;
			break;
			}

		curr_offset = no_days[leap_flag][start_month - 1] - start_day;
		first_rec = 1;
		}
	  else if (start_month == end_month && start_year == end_year)
		{
		curr_offset += end_day;
		break;			/* This is the end of it */
		}
	  else
		curr_offset += no_days[leap_flag][start_month - 1];


	start_month++;

	if (start_month > 12)
		{
		start_month = 1;
		start_year++;

		if (start_year > 99)
			start_year = 0;
		}
	}

if (sign_flag == NEG)
	curr_offset = -curr_offset;

return curr_offset;
}

/***************************************/
valid_date(t_date, date_flag)
char t_date[];
int date_flag;
{
extern int month_1, day_1, year_1;
extern int month_2, day_2, year_2;
extern char *prog;

int i, leap_flag, month, day, year;
char tst_date[7];

memset(tst_date,0,sizeof(tst_date));
memcpy(tst_date,t_date,6);

for (i = 0; i < 6; i++)
	if (tst_date[i] < '0' || tst_date[i] > '9')
		{
		return(-1);
		}

#if 0
sscanf(tst_date, "%2d%2d%2d", &year, &month, &day);
#endif
year=(tst_date[0]-0x30)*10+(tst_date[1]-0x30);
month=(tst_date[2]-0x30)*10+(tst_date[3]-0x30);
day=(tst_date[4]-0x30)*10+(tst_date[5]-0x30);

if (!month || !day) return -1;

if (month > 12)
	{
	return(-1);
	}

if (year % 4 == 0)
	leap_flag = LEAP;
  else
	leap_flag = REG;

if (day > no_days[leap_flag][month - 1])
	{
	return(-1);
	}

/* This determines where our carefully-checked values are stored */
if (date_flag == 1)
	{
	day_1 = day;
	month_1 = month;
	year_1 = year;
	}
else if (date_flag == 2)
	{
	day_2 = day;
	month_2 = month;
	year_2 = year;
	}
else
	{
	return(-1);
	}

return(0);
}

/* end of calcdate.c  -=gordon=- */
