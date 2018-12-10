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



/*
**	File:		date.c
**
**	Purpose:	To hold DATE vssub routine
**			and DATE2 vssub routine for Year 2000 compliance
**
**	Routines:	DATE		The DATE entry point
**			DATE2		The DATE2 entry point
**			DATE4		The DATE4 entry point
**			DATE6		The DATE6 entry point
**			WISPDATE	The DATE entry point
**
**
*/

#define DATE_C

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <ctype.h>
#ifdef unix
#include <sys/time.h>
#endif
#include <time.h>

#if defined(WIN32)
#define USE_FTIME
#include <sys/timeb.h>
#else
#define USE_GETTIMEOFDAY_BSD
#endif

#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"
#include "day.h"
#include "vssubs.h"
#include "wperson.h"

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


void WISPDATE(const char* func, ...);
void DATE2(const char* func, ...);
void DATE4(const char* func, ...);
void DATE6(const char* func, ...);


static void WISPDATEx(const char*routine, const char* func, char* arg1, char* arg2, char* arg3, char* arg4);
static void DATEx(const char*routine, const char* func, char* arg1, char* arg2, char* arg3, char* arg4);


static int leap_year(int4 year);
static int load_date(char type, char* string, date_struct* date);
static int unload_date(char type, char* string, date_struct* date);
static int4 diff_dates(date_struct* date1, date_struct* date2);
static int adjust_date(date_struct* date, int4 count);
static void convert_greg_style(char intype, char outtype, const char* indate, char* outdate);
static int yygetcc(const char yy[2], char cc[2]);

enum arg_type {
	ARG_NULL=0,
	ARG_DATE45,	/* Formatted date */
	ARG_G2,		/* G - YYMMDD	*/
	ARG_A2,		/* A - MMDDYY	*/
	ARG_E2,		/* E - DDMMYY	*/
	ARG_J2,		/* J - YYDDD	*/
	ARG_G4,		/* G - YYYYMMDD	*/
	ARG_A4,		/* A - MMDDYYYY	*/
	ARG_E4,		/* E - DDMMYYYY	*/
	ARG_J4,		/* J - YYYYDDD	*/
	ARG_INT4,
	ARG_DAY9,
	ARG_DAY11,
	ARG_TIME,	/* HHMMSSHH	*/
	ARG_RC
};

struct arg_types_struct
{
	char*		func;
	unsigned int	input_args;
	enum arg_type	types[4];
};

static struct arg_types_struct DATE_args[] = 
{
	{"HD", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HL", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"2D", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"2L", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},

	{"G-", 2,	{ARG_G2,	ARG_G2,		ARG_INT4,	ARG_RC}},
	{"J-", 2,	{ARG_J2,	ARG_J2,		ARG_INT4,	ARG_RC}},
	{"X-", 2,	{ARG_G4,	ARG_G4,		ARG_INT4,	ARG_RC}},
	{"R-", 2,	{ARG_J4,	ARG_J4,		ARG_INT4,	ARG_RC}},

	{"G+", 2,	{ARG_G2,	ARG_INT4,	ARG_G2,		ARG_RC}},
	{"J+", 2,	{ARG_J2,	ARG_INT4,	ARG_J2,		ARG_RC}},
	{"X+", 2,	{ARG_G4,	ARG_INT4,	ARG_G4,		ARG_RC}},
	{"R+", 2,	{ARG_J4,	ARG_INT4,	ARG_J4,		ARG_RC}},

	{"GD", 1,	{ARG_G2,	ARG_DAY9,	ARG_RC,		ARG_NULL}},
	{"JD", 1,	{ARG_J2,	ARG_DAY9,	ARG_RC,		ARG_NULL}},
	{"XD", 1,	{ARG_G4,	ARG_DAY9,	ARG_RC,		ARG_NULL}},
	{"RD", 1,	{ARG_J4,	ARG_DAY9,	ARG_RC,		ARG_NULL}},

	{"GJ", 1,	{ARG_G2,	ARG_J2,		ARG_RC,		ARG_NULL}},
	{"GX", 1,	{ARG_G2,	ARG_G4,		ARG_RC,		ARG_NULL}},
	{"GR", 1,	{ARG_G2,	ARG_J4,		ARG_RC,		ARG_NULL}},
	{"JG", 1,	{ARG_J2,	ARG_G2,		ARG_RC,		ARG_NULL}},
	{"JX", 1,	{ARG_J2,	ARG_G4,		ARG_RC,		ARG_NULL}},
	{"JR", 1,	{ARG_J2,	ARG_J4,		ARG_RC,		ARG_NULL}},
	{"XG", 1,	{ARG_G4,	ARG_G2,		ARG_RC,		ARG_NULL}},
	{"XJ", 1,	{ARG_G4,	ARG_J2,		ARG_RC,		ARG_NULL}},
	{"XR", 1,	{ARG_G4,	ARG_J4,		ARG_RC,		ARG_NULL}},
	{"RG", 1,	{ARG_J4,	ARG_G2,		ARG_RC,		ARG_NULL}},
	{"RJ", 1,	{ARG_J4,	ARG_J2,		ARG_RC,		ARG_NULL}},
	{"RX", 1,	{ARG_J4,	ARG_G4,		ARG_RC,		ARG_NULL}},

	{NULL, 0,	{ARG_NULL,	ARG_NULL,	ARG_NULL,	ARG_NULL}}
};

static struct arg_types_struct DATE6_args[] = 
{
	{"HD", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HL", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HG", 0,	{ARG_G2,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HJ", 0,	{ARG_J2,	ARG_NULL,	ARG_NULL,	ARG_NULL}},

	{"GJ", 1,	{ARG_G2,	ARG_J2,		ARG_RC,		ARG_NULL}},
	{"AJ", 1,	{ARG_A2,	ARG_J2,		ARG_RC,		ARG_NULL}},
	{"EJ", 1,	{ARG_E2,	ARG_J2,		ARG_RC,		ARG_NULL}},
	{"JG", 1,	{ARG_J2,	ARG_G2,		ARG_RC,		ARG_NULL}},
	{"JA", 1,	{ARG_J2,	ARG_A2,		ARG_RC,		ARG_NULL}},
	{"JE", 1,	{ARG_J2,	ARG_E2,		ARG_RC,		ARG_NULL}},

	{"AE", 1,	{ARG_A2,	ARG_E2,		ARG_NULL,	ARG_NULL}},
	{"AS", 1,	{ARG_A2,	ARG_G2,		ARG_NULL,	ARG_NULL}},
	{"EA", 1,	{ARG_E2,	ARG_A2,		ARG_NULL,	ARG_NULL}},
	{"ES", 1,	{ARG_E2,	ARG_G2,		ARG_NULL,	ARG_NULL}},
	{"SA", 1,	{ARG_G2,	ARG_A2,		ARG_NULL,	ARG_NULL}},
	{"SE", 1,	{ARG_G2,	ARG_E2,		ARG_NULL,	ARG_NULL}},

	{"G-", 2,	{ARG_G2,	ARG_G2,		ARG_INT4,	ARG_RC}},
	{"A-", 2,	{ARG_A2,	ARG_A2,		ARG_INT4,	ARG_RC}},
	{"E-", 2,	{ARG_E2,	ARG_E2,		ARG_INT4,	ARG_RC}},
	{"J-", 2,	{ARG_J2,	ARG_J2,		ARG_INT4,	ARG_RC}},

	{"G+", 2,	{ARG_G2,	ARG_INT4,	ARG_G2,		ARG_RC}},
	{"A+", 2,	{ARG_A2,	ARG_INT4,	ARG_A2,		ARG_RC}},
	{"E+", 2,	{ARG_E2,	ARG_INT4,	ARG_E2,		ARG_RC}},
	{"J+", 2,	{ARG_J2,	ARG_INT4,	ARG_J2,		ARG_RC}},

	{"GD", 1,	{ARG_G2,	ARG_DAY11,	ARG_RC,		ARG_NULL}},
	{"AD", 1,	{ARG_A2,	ARG_DAY11,	ARG_RC,		ARG_NULL}},
	{"ED", 1,	{ARG_E2,	ARG_DAY11,	ARG_RC,		ARG_NULL}},
	{"JD", 1,	{ARG_J2,	ARG_DAY11,	ARG_RC,		ARG_NULL}},

	{"G#", 1,	{ARG_G2,	ARG_INT4,	ARG_RC,		ARG_NULL}},
	{"A#", 1,	{ARG_A2,	ARG_INT4,	ARG_RC,		ARG_NULL}},
	{"E#", 1,	{ARG_E2,	ARG_INT4,	ARG_RC,		ARG_NULL}},
	{"J#", 1,	{ARG_J2,	ARG_INT4,	ARG_RC,		ARG_NULL}},

	{"GG", 0,	{ARG_G2,	ARG_TIME,	ARG_NULL,	ARG_NULL}},
	{"AG", 0,	{ARG_A2,	ARG_TIME,	ARG_NULL,	ARG_NULL}},
	{"EG", 0,	{ARG_E2,	ARG_TIME,	ARG_NULL,	ARG_NULL}},

	{NULL, 0,	{ARG_NULL,	ARG_NULL,	ARG_NULL,	ARG_NULL}}
};

static struct arg_types_struct DATE4_args[] = 
{
	{"HD", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HL", 0,	{ARG_DATE45,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HG", 0,	{ARG_G4,	ARG_NULL,	ARG_NULL,	ARG_NULL}},
	{"HJ", 0,	{ARG_J4,	ARG_NULL,	ARG_NULL,	ARG_NULL}},

	{"GJ", 1,	{ARG_G4,	ARG_J4,		ARG_RC,		ARG_NULL}},
	{"AJ", 1,	{ARG_A4,	ARG_J4,		ARG_RC,		ARG_NULL}},
	{"EJ", 1,	{ARG_E4,	ARG_J4,		ARG_RC,		ARG_NULL}},
	{"JG", 1,	{ARG_J4,	ARG_G4,		ARG_RC,		ARG_NULL}},
	{"JA", 1,	{ARG_J4,	ARG_A4,		ARG_RC,		ARG_NULL}},
	{"JE", 1,	{ARG_J4,	ARG_E4,		ARG_RC,		ARG_NULL}},

	{"AE", 1,	{ARG_A4,	ARG_E4,		ARG_NULL,	ARG_NULL}},
	{"AS", 1,	{ARG_A4,	ARG_G4,		ARG_NULL,	ARG_NULL}},
	{"EA", 1,	{ARG_E4,	ARG_A4,		ARG_NULL,	ARG_NULL}},
	{"ES", 1,	{ARG_E4,	ARG_G4,		ARG_NULL,	ARG_NULL}},
	{"SA", 1,	{ARG_G4,	ARG_A4,		ARG_NULL,	ARG_NULL}},
	{"SE", 1,	{ARG_G4,	ARG_E4,		ARG_NULL,	ARG_NULL}},

	{"G-", 2,	{ARG_G4,	ARG_G4,		ARG_INT4,	ARG_RC}},
	{"A-", 2,	{ARG_A4,	ARG_A4,		ARG_INT4,	ARG_RC}},
	{"E-", 2,	{ARG_E4,	ARG_E4,		ARG_INT4,	ARG_RC}},
	{"J-", 2,	{ARG_J4,	ARG_J4,		ARG_INT4,	ARG_RC}},

	{"G+", 2,	{ARG_G4,	ARG_INT4,	ARG_G4,		ARG_RC}},
	{"A+", 2,	{ARG_A4,	ARG_INT4,	ARG_A4,		ARG_RC}},
	{"E+", 2,	{ARG_E4,	ARG_INT4,	ARG_E4,		ARG_RC}},
	{"J+", 2,	{ARG_J4,	ARG_INT4,	ARG_J4,		ARG_RC}},

	{"GD", 1,	{ARG_G4,	ARG_DAY11,	ARG_RC,		ARG_NULL}},
	{"AD", 1,	{ARG_A4,	ARG_DAY11,	ARG_RC,		ARG_NULL}},
	{"ED", 1,	{ARG_E4,	ARG_DAY11,	ARG_RC,		ARG_NULL}},
	{"JD", 1,	{ARG_J4,	ARG_DAY11,	ARG_RC,		ARG_NULL}},

	{"G#", 1,	{ARG_G4,	ARG_INT4,	ARG_RC,		ARG_NULL}},
	{"A#", 1,	{ARG_A4,	ARG_INT4,	ARG_RC,		ARG_NULL}},
	{"E#", 1,	{ARG_E4,	ARG_INT4,	ARG_RC,		ARG_NULL}},
	{"J#", 1,	{ARG_J4,	ARG_INT4,	ARG_RC,		ARG_NULL}},

	{"GG", 0,	{ARG_G4,	ARG_TIME,	ARG_NULL,	ARG_NULL}},
	{"AG", 0,	{ARG_A4,	ARG_TIME,	ARG_NULL,	ARG_NULL}},
	{"EG", 0,	{ARG_E4,	ARG_TIME,	ARG_NULL,	ARG_NULL}},

	{NULL, 0,	{ARG_NULL,	ARG_NULL,	ARG_NULL,	ARG_NULL}}
};


static void trace_entry(const char* routine, struct arg_types_struct *arg_types,
			const char* func, char* args[]);

static void trace_result(const char* routine, struct arg_types_struct *arg_types,
			 const char* func, char* args[]);


/*
**	Routine:	load_var_args()
**
**	Function:	Load variable args
**
**	Description:	Load the args array with the variable arguments 
**			based on the arg_types.
**
**	Arguments:	
**	arg_types	The argument types
**	the_args	The variable args 
**	args		The array to load
**
**	Return:		None
**
**	Warnings:	None
**
*/
static void load_var_args(struct arg_types_struct *arg_types, va_list the_args, char* args[])
{
	unsigned int arg_idx;
	for(arg_idx=0; arg_idx<4; arg_idx++)
	{
		if (arg_types->types[arg_idx] != ARG_NULL)
		{
			args[arg_idx] = va_arg(the_args, char*);
		}
		else
		{
			args[arg_idx] = NULL;
		}
	}
}

/*
**	Routine:	find_func_arg_types()
**
**	Function:	Find the argument types for a given function
**
**	Description:	Load the args array with the variable arguments 
**			based on the arg_types.
**
**	Arguments:	
**	arg_types_array	The array of function argument types
**	func		The 2 character function
**
**	Return:		The argument types struct for this function 
**			or NULL if not found.
**
**	Warnings:	None
**
*/
static struct arg_types_struct* find_func_arg_types(
	struct arg_types_struct arg_types_array[], 
	const char* func)
{
	unsigned int func_idx;

	for(func_idx=0; arg_types_array[func_idx].func != NULL; func_idx++)
	{
		if (0==memcmp(func, arg_types_array[func_idx].func, 2))
		{
			/* Found a matching function */
			return &arg_types_array[func_idx];
		}
	}

	return NULL;
}

/*
**	Routine:	process_date_call()
**
**	Function:	Process a DATE call
**
**	Description:	Find the arg types based on function
**			Load the variable args 
**			trace function entry
**			Call the date function
**			trace result
**
**	Arguments:	
**	routine		The date routine name
**	arg_types_array	The array of function argument types
**	func		The 2 character function
**	the_args	The variable args 
**	date_routine	The date routine to call
**
**	Return:		The argument types struct for this function 
**			or NULL if not found.
**
**	Warnings:	None
**
*/
static void process_date_call(
	const char* routine, 
	struct arg_types_struct arg_types_array[], 
	const char* func,
	va_list the_args,
	void (*date_routine)())
{
	struct arg_types_struct *arg_types;
	char* args[4];

	arg_types = find_func_arg_types(arg_types_array, func);
	if (arg_types == NULL)
	{
		WL_werrlog_error(WERRCODE(11202), routine, "FUNC","Unknown function [%2.2s]", func);
		return;
	}

	/*
	**	Load the args
	*/
	load_var_args(arg_types, the_args, args);

	trace_entry(routine, arg_types, func,  args);

	(*date_routine)(routine, func,  args[0], args[1], args[2], args[3]);

	trace_result(routine, arg_types, func,  args);
}


/*
**	Routine:	DATE()
**			WISPDATE()
**			DATE2()
**			DATE4()
**
**	Function:	Date routine entry points
**
**	Description:	Front-ends to WISPDATEx() and DATEx()
**
**	Arguments:	variable
**
**	Return:		None
**
**	Warnings:	None
**
*/
void DATE(const char* func, char* arg1, char* arg2, char* arg3, char* arg4)
{
	WISPDATE(func, arg1, arg2, arg3, arg4);
}

void WISPDATE(const char* func, ...)
{
	va_list	the_args;
	const char* routine = "DATE";

	if (WL_useoldvsdate())
	{
		va_start(the_args, func);
		process_date_call(routine, DATE_args, func, the_args, WISPDATEx);
		va_end(the_args);
	}
	else
	{
		/*
		**	Used DATE6 instead of DATE 
		**	
		**	Exceptions:
		**	- "HD" and "HL" don't reference dates so use DATE
		**	- The X and J extensions are not it DATE6 so use DATE but
		**	  in the load_date() used the DATE6() century logic.
		**	- The xD DayOfWeek functions are Alpha(9) not Alpha(11)
		*/
		struct arg_types_struct *date_arg_types;
		struct arg_types_struct *date6_arg_types;
		char* args[4];

		/* First ensure the function is valid */
		date_arg_types = find_func_arg_types(DATE_args, func);
		if (date_arg_types == NULL)
		{
			WL_werrlog_error(WERRCODE(11202), routine, "FUNC","Unknown function [%2.2s]", func);
			return;
		}

		date6_arg_types = find_func_arg_types(DATE6_args, func);
		if ((date6_arg_types == NULL) ||
		    (func[0] == 'H' && (func[1] == 'D' || func[1] == 'L')))
		{
			/*
			**	Use DATE
			**
			**	Function is valid for DATE but not for DATE6 so must be a DATE extension.
			**	Func is "HD" or "HL" 
			*/
			va_start(the_args, func);
			process_date_call(routine, DATE_args, func, the_args, WISPDATEx);
			va_end(the_args);
			return;
		}

		/*
		**	Use DATE6
		*/
		va_start(the_args, func);
		load_var_args(date_arg_types, the_args, args);
		va_end(the_args);

		if ((func[0] == 'G' || func[0] == 'J') && func[1] == 'D')
		{
			/*
			**	For GD and JD convert the DayOfWeek from Alpha(11) to Alpha(9)
			*/
			char dow[11];
			DATE6(func, args[0], dow, args[2]);
			memcpy(args[1], dow, 9);
			return;
		}

		DATE6(func, args[0], args[1], args[2], args[3]);
	}
}

void DATE2(const char* func, ...)
{
	va_list	the_args;

	va_start(the_args, func);
	process_date_call("DATE2", DATE4_args, func, the_args, DATEx);
	va_end(the_args);
}

void DATE4(const char* func, ...)
{
	va_list	the_args;

	va_start(the_args, func);
	process_date_call("DATE4", DATE4_args, func, the_args, DATEx);
	va_end(the_args);
}

/*
**	Routine:	WISPDATEx()
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
static void WISPDATEx(const char*routine, const char* func, char* arg1, char* arg2, char* arg3, char* arg4)
{
	static char *weekday_string[7] = 
		{ "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday" };

	static char *dow_string[7] = 
		{ "SUNDAY   ","MONDAY   ","TUESDAY  ","WEDNESDAY","THURSDAY ","FRIDAY   ","SATURDAY " };

	static char *month_string[12] = { "January","February","March","April","May","June","July",
					  "August","September","October","November","December" };

	static char *am_pm_string[2] = { "AM","PM" };

	int j;
	char temp[80], temp2[80];
	int4 	rc, tlong1;
	date_struct	date1, date2;


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
		sprintf(temp,"%s %d, %d",
			month_string[time_struct->tm_mon],
			time_struct->tm_mday,
			1900+time_struct->tm_year);
										/* write the actual time string			*/

		/*
		**	123456789012345678901234567890123456789012345
		**	AAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBB    CCCCCCCC  - new
		**	AAAAAAAAA   BBBBBBBBBBBBBBBBBBB      CCCCCCCC  - old
		*/
		if ( 0==memcmp(func,"2L",2) ||
		     0==memcmp(func,"2D",2)   )
		{
			sprintf(temp2,"%-11s %-21s    %2d:%02d %2s",
				weekday_string[time_struct->tm_wday],temp,
				time_struct->tm_hour,time_struct->tm_min,am_pm_string[am_pm]);
		}
		else
		{
			/* NOTE: Day is right justified */
			sprintf(temp2,"%9s   %-19s      %2d:%02d %2s",
				weekday_string[time_struct->tm_wday],temp,
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
		if ((rc = load_date(func[0],arg1,&date1)))
		{
			WL_put_swap(arg4,rc);
			return;
		}
		if ((rc = load_date(func[0],arg2,&date2)))
		{
			WL_put_swap(arg4,rc);
			return;
		}
		tlong1 = diff_dates(&date1,&date2);
		WL_put_swap(arg3,tlong1);
		WL_put_swap(arg4,rc);
		return;
	}
	else if (0==memcmp(func,"G+",2) ||
		 0==memcmp(func,"J+",2) ||
		 0==memcmp(func,"X+",2) ||
		 0==memcmp(func,"R+",2)   )
	{
		if ((rc = load_date(func[0],arg1,&date1)))
		{
			WL_put_swap(arg4,rc);
			return;
		}
		tlong1 = WL_get_swap((int4*)arg2);

		adjust_date(&date1,tlong1);
		rc = unload_date(func[0],arg3,&date1);

		WL_put_swap(arg4,rc);
		return;
	}
	else if (0==memcmp(func,"GD",2) ||
		 0==memcmp(func,"JD",2) ||
		 0==memcmp(func,"XD",2) ||
		 0==memcmp(func,"RD",2)   )
	{
		if ((rc = load_date(func[0],arg1,&date1)))
		{
			WL_put_swap(arg3,rc);
			return;
		}
		load_date('X',"19010106",&date2);				/* Jan 06, 1901 is a Sunday			*/
		tlong1 = diff_dates(&date1,&date2) % 7;
		if (tlong1 < 0) tlong1 = -tlong1;
		memcpy(arg2,dow_string[tlong1],9);				/* Copy the DOW string.				*/

		WL_put_swap(arg3,rc);
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
		if ((rc = load_date(func[0],arg1,&date1)))
		{
			WL_put_swap(arg3,rc);
			return;
		}
		rc = unload_date(func[1],arg2,&date1);

		WL_put_swap(arg3,rc);
		return;
	}
	else
	{
		WL_werrlog(WERRCODE(11002),func[0],func[1],0,0,0,0,0,0);		/* Say not implemented.				*/
	}
}

/*
**	Routine:	DATE2()/DATE4()/DATEx()
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

static void DATEx(const char* routine, const char* func, char* arg1, char* arg2, char* arg3, char* arg4)
{
	int4 rc;
	date_struct	date1;
	char mod_func[3];  /* 2 + 1 for null with strcpy */
	struct tm *time_struct;
	time_t 	time_val;
	char	buff[20];

	if ( 0==memcmp(func,"HD",2) )						/* HD -- Current date and time (Uppercase)	*/
	{
		/*
		**	Display the current date/time in the following format.
		**
		**	AAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBB    CCCCCCCC
		**	WEDNESDAY   DECEMBER 30, 1987         2:30 PM
		*/

		WISPDATEx(routine, "2D",arg1,NULL,NULL,NULL);
	}
	else if ( 0==memcmp(func,"HL",2) )					/* HL -- Current date and time			*/
	{									/*	 (Upper - and Lowercase)		*/
		/*
		**	Display the current date/time in the following format.
		**
		**	AAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBB    CCCCCCCC
		**	Wednesday   December 30, 1987         2:30 PM
		*/

		WISPDATEx(routine, "2L",arg1,NULL,NULL,NULL);
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
	}
	else if ( 0==memcmp(func,"HJ",2) )					/* HJ -- Current date in J format		*/
	{
		time(&time_val);						/* get current time 				*/
		time_struct = localtime(&time_val);				/* break it down into parts			*/

		sprintf(buff,"%04d%03d", 
			time_struct->tm_year+1900, 
			time_struct->tm_yday+1);
		memcpy(arg1,buff,7);
	}
	else if ( 0==memcmp(func,"GG",2) ||					/* Get sustem time and date			*/
		  0==memcmp(func,"AG",2) ||
		  0==memcmp(func,"EG",2)    )
	{
		int hsec;
		
#if defined(USE_FTIME)
		{
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

	}
	else if ( 0==memcmp(func,"GJ",2) ||					/* Converting a date in Modified Gregorian	*/
		  0==memcmp(func,"AJ",2) ||					/*  format to Modified Julian format		*/
		  0==memcmp(func,"EJ",2) ||   
		  0==memcmp(func,"JG",2) ||					/* Converting a date in Modified Julian format	*/
		  0==memcmp(func,"JA",2) ||					/*  to Modified Gregorian format		*/
		  0==memcmp(func,"JE",2)    )
	{
		char arg1_buff[8];

		if ( func[0] == 'J' )						/* First need to convert function to extended	*/
		{								/*  DATE vssub functions.			*/
			strcpy(mod_func,"RX");
			memcpy(arg1_buff,arg1,7);
		}
		else
		{
			memcpy(arg1_buff,arg1,8);
			if ( func[0] == 'A' || func[0] == 'E' )
			{
				convert_greg_style(func[0],'S',arg1_buff,arg1_buff);
			}
			strcpy(mod_func,"XR");
		}

		if ((rc = load_date(mod_func[0],arg1_buff,&date1)))
		{
			WL_put_swap(arg3,rc);
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

		WL_put_swap(arg3,rc);
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

		WISPDATEx(routine, mod_func,ldate1,ldate2,arg3,arg4);
	}
	else if ( 0==memcmp(func,"J-",2) )					/* Computing the difference between two dates	*/
	{									/*  in Modified Julian format			*/
		strcpy(mod_func,"R-");
		WISPDATEx(routine, mod_func,arg1,arg2,arg3,arg4);
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

		WISPDATEx(routine, mod_func,ldate1,arg2,arg3,arg4);

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
		WISPDATEx(routine, mod_func,arg1,arg2,arg3,arg4);
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
		WISPDATEx(routine,mod_func,ldate1,arg2,arg3,NULL);

		/* Add 2 spaces to make DOW 11 characters */
		arg2[9]  = ' ';
		arg2[10] = ' ';
	}
	else if ( 0==memcmp(func,"JD",2) )					/* Determining the day of the week from a date	*/
	{									/*  in Modified Julian format			*/
		strcpy(mod_func,"RD");
		WISPDATEx(routine,mod_func,arg1,arg2,arg3,NULL);

		/* Add 2 spaces to make DOW 11 characters */
		arg2[9]  = ' ';
		arg2[10] = ' ';
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
		WISPDATEx(routine,mod_func,ldate1,daystr,arg3,NULL);

		WL_convert_day_to_number(daystr,&dow);
		if (dow != 0)
		{
			WL_put_swap(arg2,dow);					/* Copy the day of week value			*/
		}
		else
		{
			WL_put_swap(arg3,8);
			return;
		}
	}
	else if ( 0==memcmp(func,"J#",2) )					/* Determining the day of the week number from	*/
	{									/*  a Modified Julian date			*/
		int4 dow = 0;
		char daystr[20];

		daystr[0] = '\0';
		strcpy(mod_func,"RD");
		WISPDATEx(routine,mod_func,arg1,daystr,arg3,NULL);

		WL_convert_day_to_number(daystr,&dow);
		if (dow != 0)
		{
			WL_put_swap(arg2,dow);					/* Copy the day of week value			*/
		}
		else
		{
			WL_put_swap(arg3,8);
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
		WL_werrlog(WERRCODE(11102),func[0],func[1],0,0,0,0,0,0);		/* Say not implemented.				*/
	}
}



/*
**	Routine:	WL_convertyy2yyyy()
**
**	Function:	Convert a 2 digit year into a 4 digit year.
**
**	Description:	Convertion is based on pivot year and current year.
**
**	Arguments:	
**	yy_value	The 2 digit year value 0-99
**	currentyear	The current 4 digit year 
**	yy_pivot	The 2 digit pivot year 0-99
**
**	Return:		The converted 4 digit year
**
**	Warnings:	None
**
*/
unsigned int WL_convertyy2yyyy(unsigned int yy_value, unsigned int currentyear, unsigned int yy_pivot)
{
	unsigned int yy_current;
	unsigned int cc_value;

	yy_value %= 100;	/* Ensure yy is 0-99 */
	yy_pivot %= 100;	/* Ensure pivot is 0-99 */

	yy_current = currentyear % 100;
	cc_value = currentyear - yy_current;

	if ( (yy_current <= yy_pivot) && (yy_pivot < yy_value) )
	{
		cc_value -= 100;	/* Last century */
	}
	else if ( (yy_value <= yy_pivot) && (yy_pivot < yy_current) )
	{
		cc_value += 100;	/* Next century */
	}

	return cc_value + yy_value;
}

/*
**	Routine:	yygetcc()
**
**	Function:	Get the 2 digit century for this 2 digit year
**
**	Description:	Move the 2 digit centry into the 
**
**	Arguments:	
**	yy_value	The 2 digit year value 0-99
**	currentyear	The current 4 digit year 
**	yy_pivot	The 2 digit pivot year 0-99
**
**	Return:		The converted 4 digit year
**
**	Warnings:	None
**
*/
static int yygetcc(const char yy[2], char cc[2])
{
	char buff[10];
	unsigned int yy_value;
	unsigned int yyyy_value;

	if (!isdigit((int)yy[0]) || !isdigit((int)yy[0]))
	{
		return 1;
	}

	yy_value = (yy[0] - '0')*10 + (yy[1] - '0');	/* 00-99 */


	yyyy_value = WL_convertyy2yyyy(yy_value, WL_currentyear(), WL_yypivotyear());

	sprintf(buff,"%u",yyyy_value);
	cc[0] = buff[0];
	cc[1] = buff[1];
	return 0;
}


/*
**	Routine:	DATE6()
**
**	Function:	Same as DATE4 but with 2 digit years
**
**	Description:	Convert the arguments to 4 digit years
**			then call DATEx().
**
**	Arguments:	Variable.
**
**	Returns:	None
**
**
*/
void DATE6(const char* func, ...)
{
	va_list	the_args;

	int arg_idx;
	char* args[4];
	char* l_arg_ptr[4];
	char  l_arg_buf[4][45];

	struct arg_types_struct *arg_types;
	const char* routine = "DATE6";
	int century_error = 0;

	arg_types = find_func_arg_types(DATE6_args, func);
	if (arg_types == NULL)
	{
		WL_werrlog_error(WERRCODE(11202), routine, "FUNC","Unknown function [%2.2s]", func);
		return;
	}

	/*
	**	Load the args
	*/
	va_start(the_args, func);
	load_var_args(arg_types, the_args, args);
	va_end(the_args);

	trace_entry(routine, arg_types, func,  args);

	/*
	**	Convert YY to CCYY in dates
	*/
	for(arg_idx=0; arg_idx<4; arg_idx++)
	{
		char *this_arg = args[arg_idx];
		char *this_buf = l_arg_buf[arg_idx];

		this_buf[0] = '\0';
		l_arg_ptr[arg_idx] = this_buf;

		switch(arg_types->types[arg_idx])
		{
		case ARG_G2: /* YYMMDD -> CCYYMMDD */
			memcpy(&this_buf[2], &this_arg[0], 6);  /* Move YYMMDD */
			yygetcc(&this_arg[0], &this_buf[0]);	/* Insert CC */
			break;

		case ARG_A2: /* MMDDYY -> MMDDCCYY */
		case ARG_E2: /* DDMMYY -> DDMMCCYY */
			memcpy(&this_buf[0], &this_arg[0], 4);	/* Move MMDD/DDMM */
			yygetcc(&this_arg[4], &this_buf[4]);	/* Insert CC */
			memcpy(&this_buf[6], &this_arg[4], 2);	/* Move YY */
			break;

		case ARG_J2: /* YYDDD  -> CCYYDDD */
			yygetcc(&this_arg[0], &this_buf[0]);	/* Insert CC */
			memcpy(&this_buf[2], &this_arg[0], 5);	/* Move YYDDD */
			break;

		default:
			l_arg_ptr[arg_idx] = this_arg;
			break;
		}

	}

	/*
	**	Call DATEx with fixed dates
	*/
	DATEx(routine, func, l_arg_ptr[0], l_arg_ptr[1], l_arg_ptr[2], l_arg_ptr[3]);
	/* DATE4(func, l_arg_ptr[0], l_arg_ptr[1], l_arg_ptr[2], l_arg_ptr[3]); */

	/*
	**	Convert YYYY back to YY in dates
	*/
	for(arg_idx=0; arg_idx<4; arg_idx++)
	{
		char *this_arg = args[arg_idx];
		char *this_buf = l_arg_buf[arg_idx];
		char implied_century[2];

		switch(arg_types->types[arg_idx])
		{
		case ARG_G2: /* YYMMDD <- CCYYMMDD */
			yygetcc(&this_buf[2], implied_century);
			if (implied_century[0] != this_buf[0] ||
			    implied_century[1] != this_buf[1] )
			{
				century_error = 1;
			}
			else
			{
				memcpy(&this_arg[0], &this_buf[2], 2);	/* Move YY <- CCYY */
				memcpy(&this_arg[2], &this_buf[4], 4);  /* Move MMDD */
			}
			break;

		case ARG_A2: /* MMDDYY <- MMDDCCYY */
		case ARG_E2: /* DDMMYY <- DDMMCCYY */
			yygetcc(&this_buf[6], implied_century);
			if (implied_century[0] != this_buf[4] ||
			    implied_century[1] != this_buf[5] )
			{
				century_error = 1;
			}
			else
			{
				memcpy(&this_arg[0], &this_buf[0], 4);	/* Move MMDD/DDMM */
				memcpy(&this_arg[4], &this_buf[6], 2);	/* Move YY <- CCYY */
			}
			break;

		case ARG_J2: /* YYDDD  <- CCYYDDD */
			yygetcc(&this_buf[2], implied_century);
			if (implied_century[0] != this_buf[0] ||
			    implied_century[1] != this_buf[1] )
			{
				century_error = 1;
			}
			else
			{
				memcpy(&this_arg[0], &this_buf[2], 2);	/* Move YY <- CCYY */
				memcpy(&this_arg[2], &this_buf[4], 3);	/* Move DDD */
			}
			break;

		case ARG_RC:
			/*
			**	if there is a century error (and no other error)
			**	change the return code.
			*/
			if (century_error && 0 == WL_get_swap((int4*)this_arg))
			{
				WL_put_swap(this_arg, DATE_RC_4_INVALID_YEAR);
			}
			break;
			
		default:
			break;
			
		}
	}

	trace_result(routine, arg_types, func,  args);
}



/*
**	Routine:	WL_currentyear()
**
**	Function:	Returns the current year
**
**	Description:	Get current year
**
**	Arguments:	None
**
**	Return:		The current year
**
**	Warnings:	None
**
*/
unsigned int WL_currentyear(void)
{
	struct tm *time_struct;
	time_t 	time_val;

	time(&time_val);			/* get current time 		*/
	time_struct = localtime(&time_val);	/* break it down into parts	*/

	return time_struct->tm_year + 1900;
}

/*
**	Routine:	WL_yypivotyear()
**
**	Function:	Returns the pivot year used for converting 2 digit years 
**			into 4 digit years.
**
**	Description:	Get pivot year from YYPIVOTYEAR in OPTIONS file.
**			If not set use the current year + 50 as the pivot.
**
**			If the pivot year is greater then (or equal to) the 
**			current year then dates range into last century.
**
**			If the pivot year is less then the current year
**			then dates range into next century.
**
**			The date range always includes the current year.
**
**			In 2003 a pivot year of 53 will give you a valid 
**			date range of 1954-2053.
**
**			0	2001 - 2100
**			1	2002 - 2101
**			2	2003 - 2102
**			3	1904 - 2003  -- current year 2003
**			4	1905 - 2004
**			53	1954 - 2053
**			99	2000 - 2099
**
**	Arguments:	None
**
**	Return:		The pivot year (0-99)
**
**	Warnings:	None
**
*/
unsigned int WL_yypivotyear(void)
{
	static unsigned int pivotyear = 100;

	if (pivotyear == 100)  /* first time */
	{
		const char *ptr;
		
		if ((ptr = WL_get_wisp_option("YYPIVOTYEAR")) && *ptr)
		{
			sscanf(ptr, "%d", &pivotyear);
		}

		if (pivotyear > 99)
		{
			/*
			**	Default is current year + 50
			*/
			pivotyear = (WL_currentyear() + 50) % 100;
		}

		pivotyear = pivotyear % 100;  /* 0 - 99 */

		wtrace("DATE","YYPIVOTYEAR", "Using YY pivot year [%d]", pivotyear);
	}

	return pivotyear;
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
	default: return(DATE_RC_16_INVALID_TYPE);
	}

	/*
	**	Check that each char is a digit
	*/
	for (i = 0; i < len; i++)
	{
		if (string[i] < '0' || string[i] > '9')
		{
			return(DATE_RC_8_INVALID_INPUT);
		}
	}

	/*
	**	get the year
	*/
	switch(type)
	{
	case 'G':
	case 'J':
		if (WL_useoldvsdate())
		{
			date->year=	1900 + (string[0]-'0')*10 + (string[1]-'0');
		}
		else
		{
			char cc[2] = { '2', '0'};
			yygetcc(string,cc);
			date->year= (cc[0]-'0')*1000 + (cc[1]-'0')*100 + (string[0]-'0')*10 + (string[1]-'0');
		}
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

		if (month < 1 || month > 12) 			return(DATE_RC_8_INVALID_INPUT);
		if (day < 1 || day > no_days[leap][month-1]) 	return(DATE_RC_8_INVALID_INPUT);

		date->days = off_days[leap][month-1] + day;
		break;

	case 'J':
	case 'R':
		date->days = 	(string[len-3]-'0')*100 + (string[len-2]-'0')*10 + (string[len-1]-'0');
		if (date->days < 1 || date->days > off_days[leap][12]) return(DATE_RC_8_INVALID_INPUT);
		break;
	}

	return(DATE_RC_0_SUCCESS);
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
	default: return(DATE_RC_16_INVALID_TYPE);
	}

	memset(string,' ',len);
	leap = leap_year(date->year);

	if (date->days < 1 || date->days > off_days[leap][12]) return(DATE_RC_8_INVALID_INPUT);

	/*
	**	unload the year
	*/
	switch(type)
	{
	case 'G':
	case 'J':
		sprintf(buff,"%02d", date->year % 100);
		if (WL_useoldvsdate())
		{
			if (date->year < 1900 || date->year > 1999) return(DATE_RC_4_INVALID_YEAR);
		}
		else
		{
			char cc[2];

			if (date->year < 0 || date->year > 9999) return(DATE_RC_4_INVALID_YEAR);

			yygetcc(buff, cc);
			if (((cc[0]-'0')*1000 + (cc[1]-'0')*100) != 
			    (date->year - (date->year % 100)))
			{
				return(DATE_RC_4_INVALID_YEAR);
			}
		}
		string[0] = buff[0];
		string[1] = buff[1];
		break;

	case 'X':	
	case 'R':
		if (date->year < 0 || date->year > 9999) return(DATE_RC_4_INVALID_YEAR);
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

	return(DATE_RC_0_SUCCESS);
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
static void convert_greg_style(char intype, char outtype, const char* indate, char* outdate)
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

static void trace_arg_type(enum arg_type the_type, unsigned int arg_idx,  char *args[], char *arg_buff)
{
	int4 i4;

	arg_buff[0] = '\0';
	switch(the_type)
	{
	case ARG_DATE45:
		sprintf(arg_buff," Date=[%45.45s]", args[arg_idx]);
		break;

	case ARG_G2:
	case ARG_A2:
	case ARG_E2:
		sprintf(arg_buff," Arg%d=[%6.6s]", arg_idx+2, args[arg_idx]);
		break;

	case ARG_J2:
		sprintf(arg_buff," Arg%d=[%5.5s]", arg_idx+2, args[arg_idx]);
		break;

	case ARG_G4:
	case ARG_A4:
	case ARG_E4:
	case ARG_TIME:
		sprintf(arg_buff," Arg%d=[%8.8s]", arg_idx+2, args[arg_idx]);
		break;

	case ARG_J4:
		sprintf(arg_buff," Arg%d=[%7.7s]", arg_idx+2, args[arg_idx]);
		break;

	case ARG_DAY9:
		sprintf(arg_buff," Arg%d=[%9.9s]", arg_idx+2, args[arg_idx]);
		break;

	case ARG_DAY11:
		sprintf(arg_buff," Arg%d=[%11.11s]", arg_idx+2, args[arg_idx]);
		break;

	case ARG_INT4:
		i4 = WL_get_swap((int4*)args[arg_idx]);
		sprintf(arg_buff," Arg%d=[%d]", arg_idx+2, i4);
		break;

	case ARG_RC:
		i4 = WL_get_swap((int4*)args[arg_idx]);
		sprintf(arg_buff," RC=[%d]",  i4);
		break;

	case ARG_NULL:
	default:
		break;
	}
}

static void trace_entry(const char* routine, struct arg_types_struct *arg_types,
			const char* func, char* args[])
{
	unsigned int arg_idx;
	char buff[200];

	if (!WL_wtracing())
	{
		return;
	}

	/*
	**	Build the trace buffer
	*/

	buff[0] = '\0';

	for(arg_idx=0; arg_idx<arg_types->input_args; arg_idx++)
	{
		char arg_buff[100];
		trace_arg_type(arg_types->types[arg_idx], arg_idx, args, arg_buff);
		strcat(buff, arg_buff);
	}

	WL_wtrace(routine,"ENTRY"," Func=[%2.2s]%s",func,buff);
}

static void trace_result(const char* routine, struct arg_types_struct *arg_types,
			 const char* func, char* args[])
{
	unsigned int arg_idx;
	char buff[200];

	if (!WL_wtracing())
	{
		return;
	}

	/*
	**	Build the trace buffer
	*/

	buff[0] = '\0';

	for(arg_idx=0; arg_idx<4; arg_idx++)
	{
		char arg_buff[100];
		trace_arg_type(arg_types->types[arg_idx], arg_idx, args, arg_buff);
		strcat(buff, arg_buff);
	}

	WL_wtrace(routine,"RETURN","Func=[%2.2s]%s",func,buff);
}

int WL_useoldvsdate(void)
{
	static int flag = -1;
	if (-1 == flag) /* first time */
	{
		if (WL_get_wisp_option("USEOLDVSDATE") != NULL)
		{
			flag = 1;
		}
		else
		{
			flag = 0;
		}
	}
	return flag;
}

/*
**	History:
**	$Log: date.c,v $
**	Revision 1.38  2009/10/18 20:59:06  gsl
**	fix windows warnings
**	
**	Revision 1.37  2003/03/27 21:37:18  gsl
**	fix warnings
**	
**	Revision 1.36  2003/03/27 21:23:17  gsl
**	DATE6 changes
**	
**	Revision 1.35  2003/03/26 22:13:19  gsl
**	Fixed a bug in VSSUB DATE2 and DATE4 using functions "AJ" and
**	"EJ".  The input date argument was being changed into system
**	format.
**	
**	Revision 1.34  2003/03/25 22:13:23  gsl
**	Rework DATE APIs
**	
**	Revision 1.33  2003/03/24 22:43:37  gsl
**	Implement DATE6
**	Add arg tracing to DATE2/DATE4
**	
**	Revision 1.32  2003/03/20 22:24:09  gsl
**	Start work on DATE6
**	
**	Revision 1.31  2003/03/20 19:05:14  gsl
**	Change references fo DATE to WISPDATE
**	
**	Revision 1.30  2003/02/17 20:30:36  gsl
**	Define return codes in vssubs.h
**	
**	Revision 1.29  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.28  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.27  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.26  2002/12/10 20:54:15  gsl
**	use WERRCODE()
**	
**	Revision 1.25  2002/12/09 21:09:27  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.24  2002/07/12 17:00:54  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.23  2002/07/11 20:29:06  gsl
**	Fix WL_ globals
**	
**	Revision 1.22  1999/09/24 22:58:48  gsl
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
**	Add WISPDATEx(routine,) as a frontend to DATE()
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
**	11/24/89	revamped by Jock Cooper
**	09/22/92	Totaly rewritten by Greg Lindholm
**	05/12/97	Added DATE2 vssub by Suzette Cass
**
**
*/
