static char copyright[]="Copyright (c) 1995 NeoMedia Technologies, Inc. All rights reserved.";
static char rcsid[]="$Id:$";


/*
**	File:		date.c
**
**	Purpose:	To hold DATE vssub routine
**			and DATE2 vssub routine for Year 2000 compliance
**
**	Routines:	DATE		The DATE entry point
**			DATE2		The DATE2 entry point
**
**
**	History:
**	mm/dd/yy	Written by ...
**	11/24/89	revamped by Jock Cooper
**	09/22/92	Totaly rewritten by Greg Lindholm
**	05/12/97	Added DATE2 vssub by Suzette Cass
**
*/

#define DATE_C

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <ctype.h>
#ifdef unix
#include <sys/time.h>
#endif
#include <time.h>
#include <string.h>


#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wisplib.h"
#include "day.h"

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


void WISPDATE(const char* func,char* arg1,char* arg2,char* arg3,char* arg4);
void DATE(const char* func, char* arg1, char* arg2, char* arg3, char* arg4);
void DATE2(const char* func, char* arg1, char* arg2, char* arg3, char* arg4);
void DATE4(const char* func, char* arg1, char* arg2, char* arg3, char* arg4);
void DATE6(const char* func, char* arg1, char* arg2, char* arg3, char* arg4);


static int leap_year(int4 year);
static int load_date(char type, char* string, date_struct* date);
static int unload_date(char type, char* string, date_struct* date);
static int4 diff_dates(date_struct* date1, date_struct* date2);
static int adjust_date(date_struct* date, int4 count);
static void convert_greg_style(char intype, char outtype, char* indate, char* outdate);

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
**			"R-"	Find difference in days between two dates<
**			"X-"	Find difference in days between two dates
**			"G+"	Calc new date by adding count number of days
**			"J+"	Calc new date by adding count number of days
**			"R+"	Calc new date by adding count number of days
**			"X+"	Calc new date by adding count number of days
**			"GD"	Return day of week
**			"JD"	Return day of week
**			"RD"	Return day of week
**			"XD"	Return day of week
**			"GJ"	Convert Gregorian date to Julian date
**			"JG"	Convert Julian date to Gregorian date
**
**	Arguments:
**	"HD",receiver
**	"HL",receiver
**	"G-",G1,G2,diff,retcode
**	"J-",J1,J2,diff,retcode
**	"R-",J1,J2,diff,retcode
**	"X-",X1,X2,diff,retcode
**	"G+",G1,count,G2,retcode
**	"J+",J1,count,J2,retcode
**	"R+",J1,count,J2,retcode
**	"X+",X1,count,X2,retcode
**	"GD",G1,day,retcode
**	"JD",J1,day,retcode
**	"RD",J1,day,retcode
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
void DATE(const char* func,char* arg1,char* arg2,char* arg3,char* arg4)
{
	WISPDATE(func, arg1, arg2, arg3, arg4);
}

void WISPDATE(const char* func,char* arg1,char* arg2,char* arg3,char* arg4)
{
#define		ROUTINE		11000
	static char *weekday_string[7] = { "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday" };

	static char *dow_string[7] = { "SUNDAY   ","MONDAY   ","TUESDAY  ","WEDNESDAY","THURSDAY ","FRIDAY   ","SATURDAY " };

	static char *month_string[12] = { "January","February","March","April","May","June","July",
					  "August","September","October","November","December" };

	static char *am_pm_string[2] = { "AM","PM" };

	int j;
	char temp[80], temp2[80];
	int4 	rc, tlong1;
	date_struct	date1, date2;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we're here				*/

	if (0==memcmp(func,"HL",2) ||
	    0==memcmp(func,"HD",2) ||						/* HL -- upper/lower case header, HD -- upper	*/
	    0==memcmp(func,"2L",2) ||						/* Same as above except new format for DATE2	*/
	    0==memcmp(func,"2D",2)   )
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
		if ( 0==memcmp(func,"2L",2) ||
		     0==memcmp(func,"2D",2)   )
		{
			sprintf(temp2,"%-11s %-21s    %2d:%02d %2s",weekday_string[time_struct->tm_wday],temp,
								time_struct->tm_hour,time_struct->tm_min,am_pm_string[am_pm]);
		}
		else
		{
			sprintf(temp2,"%9s   %-18s       %2d:%02d %2s",weekday_string[time_struct->tm_wday],temp,
								time_struct->tm_hour,time_struct->tm_min,am_pm_string[am_pm]);
		}

		memcpy(arg1,temp2,45);

		if (func[1] == 'D') for (j=0;j<24;j++) arg1[j]=toupper(arg1[j]);    /* If they want upper case.			*/

	}
	else if (0==memcmp(func,"G-",2) ||
		 0==memcmp(func,"J-",2) ||
		 0==memcmp(func,"X-",2) ||
		 0==memcmp(func,"R-",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg4,&rc,4);
			return;
		}
		if (rc = load_date(func[0],arg2,&date2))
		{
			wswap(&rc);
			PUTBIN(arg4,&rc,4);
			return;
		}
		tlong1 = diff_dates(&date1,&date2);
		wswap(&tlong1);
		PUTBIN(arg3,&tlong1,4);

		PUTBIN(arg4,&rc,4);
		return;
	}
	else if (0==memcmp(func,"G+",2) ||
		 0==memcmp(func,"J+",2) ||
		 0==memcmp(func,"X+",2) ||
		 0==memcmp(func,"R+",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg4,&rc,4);
			return;
		}
		GETBIN(&tlong1,arg2,4);
		wswap(&tlong1);

		adjust_date(&date1,tlong1);
		rc = unload_date(func[0],arg3,&date1);
		wswap(&rc);

		PUTBIN(arg4,&rc,4);
		return;
	}
	else if (0==memcmp(func,"GD",2) ||
		 0==memcmp(func,"JD",2) ||
		 0==memcmp(func,"XD",2) ||
		 0==memcmp(func,"RD",2)   )
	{
		if (rc = load_date(func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg3,&rc,4);
			return;
		}
		load_date('G',"010106",&date2);					/* Jan 06, 1901 is a Sunday			*/
		tlong1 = diff_dates(&date1,&date2) % 7;
		if (tlong1 < 0) tlong1 = -tlong1;
		memcpy(arg2,dow_string[tlong1],9);				/* Copy the DOW string.				*/

		wswap(&rc);
		PUTBIN(arg3,&rc,4);
		return;
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
			return;
		}
		rc = unload_date(func[1],arg2,&date1);

		wswap(&rc);
		PUTBIN(arg3,&rc,4);
		return;
	}
	else
	{
		werrlog(ERRORCODE(2),func[0],func[1],0,0,0,0,0,0);		/* Say not implemented.				*/
	}
}

/*
**	Routine:	DATE2()
**
**	Function:	To emulate the vssub DATE2
**
**	Description:	The DATE2 vssub offers the same functionality as the DATE vssub and
**			the DAY vssub except it uses a four digit year in place of the two
**			digit year.  The four digit year allows the restrictions that confined
**			the use of DATE to January 1, 1900 to December 31, 1999 to be
**			removed.
**
**			On Wang it accepts dates in two formats
**				Gregorian - three styles
**				     System   = YYYYMMDD
**				     American = MMDDYYYY
**				     European = DDMMYYYY
**				Julian		YYYYDDD
**
**			Functions GJ, JG, G-, G+, GD and G# expect System style Gregorian dates.
**			Functions AJ, JA, A-, A+, AD and A# expect American style Gregorian dates.
**			Functions EJ, JE, E-, E+, ED and E# expect European style Gregorian dates.
**
**			"HD"	Return current date and time as UPPERCASE formatted string
**			"HL"	Return current date and time as UP/low formatted string
**			"HG"*	Return current date in G format
**			"HJ"*	Return current date in J format
**			"GJ"	Convert Modified Gregorian date to Modified Julian date in System style
**			"AJ"	same as above in American style
**			"EJ"	same as above in European style
**			"JG"	Convert Modified Julian date to Modified Gregorian date in System style
**			"JA"	same as above in American style
**			"JE"	same as above in European style
**			"G-"	Find difference in days between two dates in Modified Gregorian format
**			"A-"	same as above in American style
**			"E-"	same as above in European style
**			"J-"	Find difference in days between two dates in Modified Julian format
**			"G+"	Calc new date by adding count number of days to a Modified Gregorian date
**			"A+"	same as above in American style
**			"E+"	same as above in European style
**			"J+"	Calc new date by adding count number of days to a Modified Julian date
**			"GD"	Return day of week from a Modified Gregorian date
**			"AD"	same as above in American style
**			"ED"	same as above in European style
**			"JD"	Return day of week in Modified Julian format
**			"G#"	Return day of week number from a Modified Gregorian date
**			"A#"	same as above in American style
**			"E#"	same as above in European style
**			"J#"	Return day of week number from a Modified Julian date
**			"AE"	Convert Modified Gregorian date from American style to European style
**			"AS"	Convert Modified Gregorian date from American style to System style
**			"EA"	Convert Modified Gregorian date from European style to American style
**			"ES"	Convert Modified Gregorian date from European style to System style
**			"SA"	Convert Modified Gregorian date from System style to American style
**			"SE"	Convert Modified Gregorian date from System style to European style
**			"GG"	Get system date and time in system format
**			"AG"	Get system date and time in American format
**			"EG"	Get system date and time in European format
**
**	Arguments:
**	"HD",receiver
**	"HL",receiver
**	"HG",G1
**	"HJ",J1
**	"GJ",G1,J1,retcode
**	"AJ",G1,J1,retcode
**	"EJ",G1,J1,retcode
**	"JG",J1,G1,retcode
**	"JA",J1,G1,retcode
**	"JE",J1,G1,retcode
**	"G-",G1,G2,diff,retcode
**	"A-",G1,G2,diff,retcode
**	"E-",G1,G2,diff,retcode
**	"J-",J1,J2,diff,retcode
**	"G+",G1,count,G2,retcode
**	"A+",G1,count,G2,retcode
**	"E+",G1,count,G2,retcode
**	"J+",J1,count,J2,retcode
**	"GD",G1,day,retcode
**	"AD",G1,day,retcode
**	"ED",G1,day,retcode
**	"JD",J1,day,retcode
**	"G#",G1,day#,retcode
**	"A#",G1,day#,retcode
**	"E#",G1,day#,retcode
**	"J#",J1,day#,retcode
**	"AE",G1,G2
**	"AS",G1,G2
**	"EA",G1,G2
**	"ES",G1,G2
**	"SA",G1,G2
**	"SE",G1,G2
**	"GG",YYYYMMDD,HHMMSSHH
**	"AG",MMDDYYYY,HHMMSSHH
**	"EG",DDMMYYYY,HHMMSSHH
**
**	Globals:	None
**
**	Return:		0	Successful
**			8	Invalid input value or format
**
**	Warnings:	None
**
**
*/
void DATE4(const char* func, char* arg1, char* arg2, char* arg3, char* arg4)
{
	DATE2(func, arg1, arg2, arg3, arg4);
}

void DATE2(const char* func, char* arg1, char* arg2, char* arg3, char* arg4)
{
#undef		ROUTINE
#define		ROUTINE		11100

	int4 rc;
	date_struct	date1;
	char mod_func[2];
	struct tm *time_struct;
	time_t 	time_val;
	char	buff[20];
		
	wtrace("DATE2","ENTRY","Function = [%2.2s]", func);

	if ( 0==memcmp(func,"HD",2) )						/* HD -- Current date and time (Uppercase)	*/
	{
		/*
		**	Display the current date/time in the following format.
		**
		**	AAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBB    CCCCCCCC
		**	WEDNESDAY   DECEMBER 30, 1987         2:30 PM
		*/

		DATE("2D",arg1,NULL,NULL,NULL);
	}
	else if ( 0==memcmp(func,"HL",2) )					/* HL -- Current date and time			*/
	{									/*	 (Upper - and Lowercase)		*/
		/*
		**	Display the current date/time in the following format.
		**
		**	AAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBB    CCCCCCCC
		**	Wednesday   December 30, 1987         2:30 PM
		*/

		DATE("2L",arg1,NULL,NULL,NULL);
	}
	else if ( 0==memcmp(func,"HG",2) )					/* HG -- Current date in G format		*/
	{
		time(&time_val);						/* get current time 				*/
		time_struct = localtime(&time_val);				/* break it down into parts			*/

		sprintf(buff,"%04d%02d%02d", 
			time_struct->tm_year+1900, 
			time_struct->tm_mon+1,
			time_struct->tm_mday);
		memcpy(arg1,buff,8);
		wtrace("DATE2","RETURN","HG =  [%8.8s]", arg1);
	}
	else if ( 0==memcmp(func,"HJ",2) )					/* HJ -- Current date in J format		*/
	{
		time(&time_val);						/* get current time 				*/
		time_struct = localtime(&time_val);				/* break it down into parts			*/

		sprintf(buff,"%04d%03d", 
			time_struct->tm_year+1900, 
			time_struct->tm_yday+1);
		memcpy(arg1,buff,7);
		wtrace("DATE2","RETURN","HJ =  [%7.7s]", arg1);
	}
	else if ( 0==memcmp(func,"GG",2) ||					/* Get sustem time and date			*/
		  0==memcmp(func,"AG",2) ||
		  0==memcmp(func,"EG",2)    )
	{
		int hsec;

#if defined(WIN32)
#define USE_FTIME
#else
#define USE_GETTIMEOFDAY_BSD
#endif
		
#if defined(USE_FTIME)
		{
			#include <sys/timeb.h>
			struct timeb sTimeb;
			ftime(&sTimeb);
			time_struct = localtime(&sTimeb.time);
			hsec = sTimeb.millitm / 10;
		}
#elif defined(USE_GETTIMEOFDAY_BSD)
		{
			struct timeval sTimeOfDay;
			struct timezone sTimeZone;
			
			gettimeofday(&sTimeOfDay, &sTimeZone);
			time_struct = localtime(&sTimeOfDay.tv_sec);
			hsec = sTimeOfDay.tv_usec / 10000;
		}
#elif defined(USE_GETTIMEOFDAY_SYSV)
		{
			struct timeval sTimeOfDay;
			
			gettimeofday(&sTimeOfDay);
			time_struct = localtime(&sTimeOfDay.tv_sec);
			hsec = sTimeOfDay.tv_usec / 10000;
		}
#else
#error "TIME-OF-DAY FUNCTION NOT DEFINED"
#endif

		if ('G' == *func)	/* YYYYMMDD */
		{
			sprintf(buff,"%04d%02d%02d", 
				time_struct->tm_year+1900, 
				time_struct->tm_mon+1,
				time_struct->tm_mday);
		}
		else if ('A' == *func )	/* MMDDYYYY */
		{
			sprintf(buff,"%02d%02d%04d", 
				time_struct->tm_mon+1,
				time_struct->tm_mday,
				time_struct->tm_year+1900);
		}
		else			/* DDMMYYYY */
		{
			sprintf(buff,"%02d%02d%04d", 
				time_struct->tm_mday,
				time_struct->tm_mon+1,
				time_struct->tm_year+1900);
		}
		memcpy(arg1,buff,8);
		
		/* HHMMSSHH */
		sprintf(buff,"%02d%02d%02d%02d",
			time_struct->tm_hour,
			time_struct->tm_min,
			time_struct->tm_sec,
			hsec);
		memcpy(arg2,buff,8);

		wtrace("DATE2","RETURN","%2.2s =  [%8.8s] [%8.8s]", func, arg1, arg2);
	}
	else if ( 0==memcmp(func,"GJ",2) ||					/* Converting a date in Modified Gregorian	*/
		  0==memcmp(func,"AJ",2) ||					/*  format to Modified Julian format		*/
		  0==memcmp(func,"EJ",2) ||   
		  0==memcmp(func,"JG",2) ||					/* Converting a date in Modified Julian format	*/
		  0==memcmp(func,"JA",2) ||					/*  to Modified Gregorian format		*/
		  0==memcmp(func,"JE",2)    )
	{
		if ( func[0] == 'J' )						/* First need to convert function to extended	*/
		{								/*  DATE vssub functions.			*/
			strcpy(mod_func,"RX");
		}
		else
		{
			if ( func[0] == 'A' || func[0] == 'E' )
			{
				convert_greg_style(func[0],'S',arg1,arg1);
			}
			strcpy(mod_func,"XR");
		}

		if (rc = load_date(mod_func[0],arg1,&date1))
		{
			wswap(&rc);
			PUTBIN(arg3,&rc,4);
			return;
		}
		rc = unload_date(mod_func[1],arg2,&date1);

		if ( rc == 0 )
		{
			if (   func[0] == 'J' &&
			     ( func[1] == 'A' || func[1] == 'E' ) )
			{
				convert_greg_style('S',func[1],arg2,arg2);
			}
		}

		wswap(&rc);
		PUTBIN(arg3,&rc,4);
		return;
	}
	else if ( 0==memcmp(func,"G-",2) ||					/* Computing the difference between two dates	*/
		  0==memcmp(func,"A-",2) ||					/*  in Modified Gregorian format		*/
		  0==memcmp(func,"E-",2)    )
	{
		char ldate1[9], ldate2[9];

		strcpy(mod_func,"X-");						/* First need to convert function to extended	*/
										/*  DATE vssub functions.			*/
		if ( func[0] == 'A' || func[0] == 'E' )
		{
			convert_greg_style(func[0],'S',arg1,ldate1);
			convert_greg_style(func[0],'S',arg2,ldate2);
		}
		else
		{
			memcpy(ldate1,arg1,8);
			memcpy(ldate2,arg2,8);
		}

		DATE(mod_func,ldate1,ldate2,arg3,arg4);
	}
	else if ( 0==memcmp(func,"J-",2) )					/* Computing the difference between two dates	*/
	{									/*  in Modified Julian format			*/
		strcpy(mod_func,"R-");
		DATE(mod_func,arg1,arg2,arg3,arg4);
	}
	else if ( 0==memcmp(func,"G+",2) ||					/* Adding a specified number of days to a	*/
		  0==memcmp(func,"A+",2) ||					/*  Modified Gregorian date			*/
		  0==memcmp(func,"E+",2)    )
	{
		char ldate1[9];

		strcpy(mod_func,"X+");						/* First need to convert function to extended	*/
										/*  DATE vssub functions.			*/
		if ( func[0] == 'A' || func[0] == 'E' )
		{
			convert_greg_style(func[0],'S',arg1,ldate1);
		}
		else
		{
			memcpy(ldate1,arg1,8);
		}

		DATE(mod_func,ldate1,arg2,arg3,arg4);

		if ( func[0] == 'A' || func[0] == 'E' )
		{
		        /* Convert the system date back A or E */ 
			convert_greg_style('S', func[0],arg3,ldate1);
			memcpy(arg3, ldate1, 8);
		}
	}
	else if ( 0==memcmp(func,"J+",2) )					/* Adding a specified number of days to a 	*/
	{									/*  Modified Julian date			*/
		strcpy(mod_func,"R+");
		DATE(mod_func,arg1,arg2,arg3,arg4);
	}
	else if ( 0==memcmp(func,"GD",2) ||					/* Determining the day of the week from a date	*/
		  0==memcmp(func,"AD",2) ||					/*  in Modified Gregorian format		*/
		  0==memcmp(func,"ED",2)    )
	{
		char ldate1[9];

		strcpy(mod_func,"XD");						/* First need to convert function to extended	*/
										/*  DATE vssub functions.			*/
		if ( func[0] == 'A' || func[0] == 'E' )
		{
			convert_greg_style(func[0],'S',arg1,ldate1);
		}
		else
		{
			memcpy(ldate1,arg1,8);
		}
		DATE(mod_func,ldate1,arg2,arg3,NULL);
	}
	else if ( 0==memcmp(func,"JD",2) )					/* Determining the day of the week from a date	*/
	{									/*  in Modified Julian format			*/
		strcpy(mod_func,"RD");
		DATE(mod_func,arg1,arg2,arg3,NULL);
	}
	else if ( 0==memcmp(func,"G#",2) ||					/* Determining the day of the week number from 	*/
		  0==memcmp(func,"A#",2) ||					/*  a Modified Gregorian date			*/
		  0==memcmp(func,"E#",2)    )
	{
		char ldate1[9], daystr[20];
		int4 dow = 0;

		strcpy(mod_func,"XD");						/* First need to convert function to extended	*/
										/*  DATE vssub functions.			*/
		if ( func[0] == 'A' || func[0] == 'E' )
		{
			convert_greg_style(func[0],'S',arg1,ldate1);
		}
		else
		{
			memcpy(ldate1,arg1,8);
		}

		daystr[0] = ' ';	/* If DATE fails then daystr is un-initialized */
		DATE(mod_func,ldate1,daystr,arg3,NULL);

		convert_day_to_number(daystr,&dow);
		if (dow != 0)
		{
			wswap(&dow);
			PUTBIN(arg2,&dow,sizeof(int4));				/* Copy the day of week value			*/
		}
		else
		{
			rc = 8;
			wswap(&rc);
			PUTBIN(arg3,&rc,4);
			return;
		}
	}
	else if ( 0==memcmp(func,"J#",2) )					/* Determining the day of the week number from	*/
	{									/*  a Modified Julian date			*/
		int4 dow = 0;
		char daystr[20];

		daystr[0] = '\0';
		strcpy(mod_func,"RD");
		DATE(mod_func,arg1,daystr,arg3,NULL);

		convert_day_to_number(daystr,&dow);
		if (dow != 0)
		{
			wswap(&dow);
			PUTBIN(arg2,&dow,sizeof(int4));				/* Copy the day of week value			*/
		}
		else
		{
			rc = 8;
			wswap(&rc);
			PUTBIN(arg3,&rc,4);
			return;
		}
	}
	else if ( 0==memcmp(func,"AE",2) ||					/* Converting a Modified Gregorian date from	*/
		  0==memcmp(func,"AS",2) ||					/*  one format ot another			*/
		  0==memcmp(func,"EA",2) ||
		  0==memcmp(func,"ES",2) ||
		  0==memcmp(func,"SA",2) ||
		  0==memcmp(func,"SE",2)   )
	{
			convert_greg_style(func[0],func[1],arg1,arg2);
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
static int leap_year(int4 year)
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

static int load_date(char type, char* string, date_struct* date)
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

static int unload_date(char type, char* string, date_struct* date)
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
static int4 diff_dates(date_struct* date1, date_struct* date2)
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
static int adjust_date(date_struct* date, int4 count)
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

/*
**	Routine:	convert_greg_style()
**
**	Function:	To convert a modified Gregorian date from one format to another.
**
**	Description:	Will move appropriate positions of data to requested format so is
*			returned in one of three Gregorian styles: S = System (YYYYMMDD),
**			A = American (MMDDYYYY), or E = European (DDMMYYYY) 
**
**	Arguments:
**	intype		input style of date
**	outtype		output style of date
**	indate		input string with date as input stype
**	outdate		output string with date converted to output style
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	No validation will be performed on the dates passed to this routine.
**			The input values will be simply moved to the appropriate position in the
**			output receiver.
**
**	History:	
**	05/13/97	Written by SMC
**
*/
static void convert_greg_style(char intype, char outtype, char* indate, char* outdate)
{
	char buff1[9], buff2[9];

	memcpy(buff1,indate,8);
	buff1[8] = '\0';
	memset(buff2,' ',8);
	buff2[8] = '\0';

	switch(intype)
	{
	case 'S':
		switch(outtype)
		{
		case 'A':
			memcpy(buff2,buff1+4,4);
			memcpy(buff2+4,buff1,4);
			break;
		case 'E':
			memcpy(buff2,buff1+6,2);
			memcpy(buff2+2,buff1+4,2);
			memcpy(buff2+4,buff1,4);
			break;
		}
		break;
	case 'A':
		switch(outtype)
		{
		case 'S':
			memcpy(buff2,buff1+4,4);
			memcpy(buff2+4,buff1,4);
			break;
		case 'E':
			memcpy(buff2,buff1+2,2);
			memcpy(buff2+2,buff1,2);
			memcpy(buff2+4,buff1+4,4);
			break;
		}
		break;
	case 'E':
		switch(outtype)
		{
		case 'A':
			memcpy(buff2,buff1+2,2);
			memcpy(buff2+2,buff1,2);
			memcpy(buff2+4,buff1+4,4);
			break;
		case 'S':
			memcpy(buff2,buff1+4,4);
			memcpy(buff2+4,buff1+2,2);
			memcpy(buff2+6,buff1,2);
			break;
		}
		break;
	}

	memcpy(outdate,buff2,8);
}

/*
**	History:
**	$Log: date.c,v $
**	Revision 1.22  1999-09-24 18:58:48-04  gsl
**	Fix gettimeofday() hsec calc.
**
**	Revision 1.21  1999-09-24 09:02:46-04  gsl
**	Fix the SCO define as it turns out SCO uses the BSD style of gettimeofday()
**
**	Revision 1.20  1999-09-13 15:46:09-04  gsl
**	fix warning on WIN32
**
**	Revision 1.19  1999-09-08 16:38:32-04  gsl
**	Rework the gettimeofday logic to work on WIN32
**
**	Revision 1.18  1999-09-08 15:38:39-04  gsl
**	Add DATE4() as a frontend to DATE2()
**	Add GG AG and EG functions to return the system date and time
**
**	Revision 1.17  1999-01-18 14:10:21-05  gsl
**	In DATE2 A+ and E+ were returning the date in S format.
**	Fixed to convert date back to E or A format
**
**	Revision 1.16  1998-10-13 10:46:57-04  gsl
**	Fix warning in J# if invalid date was passed.
**
**	Revision 1.15  1998-07-10 11:01:49-04  gsl
**	In G#, if the call to DATE fails then the daystr was uninitialized.
**
**	Revision 1.14  1997-10-29 16:13:41-05  gsl
**	Add WISPDATE() as a frontend to DATE()
**	This was done to solve a problem on NT with Acucobol 3.2 which
**	now include windows.h which typedef's DATE
**
**	Revision 1.13  1997-10-02 17:45:48-04  gsl
**	Add to DATE2 function HG and HJ which return the current date
**	in extented G and J formats.
**
**	Revision 1.12  1997-05-20 14:02:03-04  scass
**	Corrected warnings for NT
**
**	Revision 1.11  1997-05-20 09:50:00-04  scass
**	Corrected convert_day-to_number() to use the address
**	so value gets updated.
**
**	Revision 1.10  1997-05-20 07:46:31-04  scass
**	Added the code for the DATE2 VSSUB.  This handles the
**	Y2000 enhancements that Wang has implemented.
**
**	Revision 1.9  1996-08-19 18:32:14-04  gsl
**	drcs update
**
**
**
*/
