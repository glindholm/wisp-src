/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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


/*
	SET
		FC	Char(1)		File protection class 	(Ignored)
		FH	Char(1)		Force Help		(Ignored)
		FN	Int(4)		Form Number
		IL	Char(8)		Input Lib
		IV	Char(6)		Input Vol
		JC	Char(1)		Job Class		(Not yet implemented)
		JL	Int(4)		Job Limit		(Not yet implemented)
		JS	Char(1)		Job Status		(Not yet implemented)
		LI	Int(4)		Lines Per Page
		OL	Char(8)		Output Lib
		OV	Char(6)		Output Vol
		P#/PR	Int(4)		Printer Number
		PC	Char(1)		Printer Class
		PM	Char(1)		Print Mode
		PF	Char(1)		Print file protection	(Ignored)
		PL	Char(8)		Program Lib		(Not yet implemented)
		PV	Char(6)		Program Vol		(Not yet implemented)
		RL	Char(8)		Run Lib
		RV	Char(6)		Run Vol
		RR	Int(4)		Return Code		(Not Supported)
		RS	Char(8)		Remote Spooling		(Ignored)
		SL	Char(8)		Spool Lib
		SV	Char(6)		Spool Vol
		WV	Char(6)		Work Vol
*/

#include <stdio.h>									/* Include standard I/O module.		*/
#include <stdarg.h>									/* Function uses variable params.	*/

#include "idsistd.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wisplib.h"
#include "vssubs.h"

void SET(const char *keyword_arg1, const char* value_arg2, ...)
{
	va_list the_args;

	const char	*keyword;
	const char	*value;                            
	int	to_do;

	va_start(the_args, value_arg2);
	to_do = WL_va_count();

	WL_wtrace("SET","ENTRY","Entry into SET(\"%2.2s\",...) args=%d", keyword_arg1, to_do);

	SET2(keyword_arg1, value_arg2);

	to_do -= 2;

	while (to_do>1)
	{
		keyword = va_arg(the_args, char*);
		to_do--;
		value = va_arg(the_args, char*);
		to_do--;

		SET2(keyword, value);
	}

	va_end(the_args);

	if (to_do != 0)
	{
		werrlog(WERRCODE(58006),0,0,0,0,0,0,0,0);
	}

}       

void SET2(const char *keyword, const char* value)
{
	int4    long_value;
	int	value_len = 0;
	int	ignored = 0;
	int	is_int = 0;

	if (0==memcmp(keyword,"FC",2))		
	{
		ignored = 1;				/* FC File protection class (Ignored)	*/
		value_len = 1;
	}
	else if (0==memcmp(keyword,"FH",2))	
	{
		ignored = 1;				/* FH Force Help            (Ignored)	*/
		value_len = 1;
	}
	else if (0==memcmp(keyword,"FN",2))	
	{
		is_int = 1;
		long_value = WL_get_swap((int4*)value);
		WL_set_defs(DEFAULTS_FN,(char*)&long_value);	/* FN Set form number.			*/
	}
	else if (0==memcmp(keyword,"IL",2))
	{
		value_len = 8;
		WL_set_defs(DEFAULTS_IL,value);		/* IL INLIB.				*/
	}
	else if (0==memcmp(keyword,"IV",2))
	{
		value_len = 6;
		WL_set_defs(DEFAULTS_IV,value);		/* IV INVOL.				*/
	}
	else if (0==memcmp(keyword,"JC",2))
	{
		value_len = 1;
		WL_set_defs(DEFAULTS_JC,value);	
	}
	else if (0==memcmp(keyword,"JL",2))
	{
		/*
		**	Int(4) number of seconds.
		**	WL_set_defs() wants an PIC 9(6) with format HHMMSS
		*/
		int4 jl_hours, jl_mins, jl_secs;
		char jl_buff[20];
		
		is_int = 1;
		long_value = WL_get_swap((int4*)value);

		if (long_value < 0)
		{
			long_value = 0;
		}

		jl_hours = long_value / (60*60);
		long_value -= jl_hours * 60 * 60;
		jl_mins  = long_value / 60;
		jl_secs  = long_value - (jl_mins * 60);
		
		if (jl_hours > 99)
		{
			jl_hours = 99;
		}
		
		sprintf(jl_buff,"%02d%02d%02d", jl_hours, jl_mins, jl_secs);
		
		WL_set_defs(DEFAULTS_JL,jl_buff);
	}
	else if (0==memcmp(keyword,"JS",2))
	{
		value_len = 1;
		if ('R' == value[0] || 'H' == value[0])
		{
			WL_set_defs(DEFAULTS_JS,value);
		}
		else
		{
			ignored = 1;
		}
	}
	else if (0==memcmp(keyword,"LI",2))
	{
		is_int = 1;
		long_value = WL_get_swap((int4*)value);
		WL_set_defs(DEFAULTS_LI,(char*)&long_value); /* LI Set default lines per page printer.*/
	}
	else if (0==memcmp(keyword,"OL",2))
	{
		value_len = 8;
		WL_set_defs(DEFAULTS_OL,value);		/* OL OUTLIB.				*/
	}
	else if (0==memcmp(keyword,"OV",2))
	{
		value_len = 6;
		WL_set_defs(DEFAULTS_OV,value);		/* OV OUTVOL.				*/
	}
	else if (0==memcmp(keyword,"P#",2) ||
		 0==memcmp(keyword,"PR",2))
	{
		is_int = 1;
		long_value = WL_get_swap((int4*)value);
		WL_set_defs(DEFAULTS_PR,(char*)&long_value);
	}
	else if (0==memcmp(keyword,"PC",2))
	{
		value_len = 1;
		WL_set_defs(DEFAULTS_PC,value);		/* PC set the printer class		*/
	}
	else if (0==memcmp(keyword,"PF",2))
	{
		value_len = 1;
		ignored = 1;				/* PF Print file protection (Ignored)	*/
	}
	else if (0==memcmp(keyword,"PM",2))
	{
		value_len = 1;
		WL_set_defs(DEFAULTS_PM,value);		/* PM set the printer mode		*/
	}
	else if (0==memcmp(keyword,"PL",2))
	{
		value_len = 8;
		WL_set_defs(DEFAULTS_PL,value);		/* PL Program Lib			*/
	}
	else if (0==memcmp(keyword,"PV",2))
	{
		value_len = 6;
		WL_set_defs(DEFAULTS_PV,value);		/* PV Program Vol			*/
	}
	else if (0==memcmp(keyword,"RL",2))
	{
		value_len = 8;
		WL_set_defs(DEFAULTS_RL,value);		/* RL RUNLIB.				*/
	}
	else if (0==memcmp(keyword,"RV",2))
	{
		value_len = 6;
		WL_set_defs(DEFAULTS_RV,value);		/* RV RUNVOL.				*/
	}
	else if (0==memcmp(keyword,"RS",2))
	{
		ignored = 1;				/* RS Remote Spooling (Ignored)		*/
		value_len = 8;
	}
	else if (0==memcmp(keyword,"SL",2))
	{
		value_len = 8;
		WL_set_defs(DEFAULTS_SL,value);		/* Spool lib.				*/
	}
	else if (0==memcmp(keyword,"SV",2))
	{
		value_len = 6;
		WL_set_defs(DEFAULTS_SV,value);		/* Spool volume.			*/
	}
	else if (0==memcmp(keyword,"WV",2))
	{
		value_len = 6;
		WL_set_defs(DEFAULTS_WV,value);		/* Work volume.				*/
	}
	else
	{
		WL_werrlog(WERRCODE(58002),keyword);
		return;
	}

	if (is_int)
	{
		long_value = WL_get_swap((int4*)value);
		WL_wtrace("SET2", "KEYWORD", "Keyword=[%2.2s] Value=[%d] %s", 
			keyword, long_value, (ignored)?"(ignored)":"");
	}
	else
	{
		char buff[80];
		memcpy(buff,value,value_len);
		buff[value_len] = '\0';
		WL_wtrace("SET2", "KEYWORD", "Keyword=[%2.2s] Value=[%s] %s", 
			keyword, buff, (ignored)?"(ignored)":"");

	}


	WL_save_defaults();	/* Update defaults			*/

}       

/*
**	History:
**	$Log: set.c,v $
**	Revision 1.26  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.25  2003/02/14 20:52:21  gsl
**	Rework SET2() into a if/elseif instead of a switch and fix tracing
**	
**	Revision 1.24  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.23  2003/01/29 16:35:14  gsl
**	improve tracing
**	
**	Revision 1.22  2003/01/29 16:19:36  gsl
**	Add SET2() and call from SET to do actual SET
**	
**	Revision 1.21  2003/01/29 15:10:35  gsl
**	Fix SET() proto-type
**	
**	Revision 1.20  2002/12/10 17:09:17  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.19  2002/07/16 16:24:52  gsl
**	Globals
**	
**	Revision 1.18  2002/07/12 17:01:01  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.17  2002/07/11 20:29:13  gsl
**	Fix WL_ globals
**	
**	Revision 1.16  2002/07/10 21:05:24  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2001/11/27 20:54:29  gsl
**	Removed MSDOS code
**	
**	Revision 1.14  1998-08-26 17:25:41-04  gsl
**	Add support for SET JC,JL,JS
**
**	Revision 1.13  1997-07-16 15:07:57-04  gsl
**	Improve WL_wtrace() to report the value for LIB and VOL
**
**	Revision 1.12  1997-04-15 23:11:34-04  gsl
**	Update to use WL_wtrace()
**
**	Revision 1.11  1997-04-03 20:00:58-05  gsl
**	Changed the trace so that it shows the key word
**
**	Revision 1.10  1996-08-19 17:53:42-04  gsl
**	Fix set_cuserid() code to be MSDOS only - not NT
**
**
*/
