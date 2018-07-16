static char copyright[]="Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		kcsit.c
**
**	Project:	WISP/KCSI/COMMON
**
**	RCS:		$Source:$
**
**	Purpose:	Tracing and Error logging
**
**	Routines:	
**	kcsitrace()
**
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#ifdef unix
#include <unistd.h>
#endif
#include <string.h>
#include <sys/types.h>

#include "intdef.h"
#include "kcsit.h"
#include "vwang.h"
#include "werrlog.h"
#include "wperson.h"
#include "wexit.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Function Prototypes
*/


/*
**	Routine:	kcsitrace()
**
**	Function:	Write a Trace or Error message
**
**	Description:	The trace file is $HOME/kcsitrace.log.
**			If a message has a severity level greater then or equal to the tracing
**			level then the message is written out to the trace file.
**			The default tracing level is 4 (Fatal errors and porting exceptions only).
**			You can set the tracing level by setting environment variable KCSITRACE.
**
**				$ KCSITRACE=3; export KCSITRACE
**
**			A message with severity level 4 is also always written to stderr.
**
**	Arguments:
**	sever		Level of severity of error:
**				1 = Entry point into a routine.
**				2 = Informational
**				3 = Error condition (non-fatal)
**				4 = Fatal error will affect processing. (Porting exception)
**	routine		Name of routine where error occurred.
**	mess		Message to be displayed
**	lform		printf style varargs
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
*/
void kcsitrace(int sever, char *routine, char *mess, char *lform, ...)
{
	va_list add_ptr;

	int tracing_level = kcsi_tracelevel();

	/* Restrict sever to 1-4 */
	if (sever > 4)
	{
		sever = 4;
	}
	else if (sever < 1)
	{
		sever = 1;
	}


	/*
	**	If serverity is greater then the tracing level then 
	**	write this message to the trace log.
	*/
	if ( sever >= tracing_level )
	{
		char	buff[1024], msg[1024];

		va_start(add_ptr, lform);
		vsprintf(buff,lform,add_ptr);
		va_end(add_ptr);

		sprintf(msg,"(KCSI) %s-%d-%s %s",routine,sever,mess, buff);

		if (sever == 4)								/* if will effect processing, report it	*/
		{
			werrlog(104,msg);
		}
		else
		{
			strcat(msg,"\n");
			werr_write(msg);
		}
	}
}

int kcsi_tracelevel(void)
{
	static int first = 1;
	static int tracing_level = 4;

	if (first)
	{
		const char *ptr;
		first = 0;
		
		if ( ptr = getenv("KCSITRACE") )			       		/* Test if an environment var exists.   */
		{
			sscanf(ptr,"%d", &tracing_level);
		}
		else
		{
			if (ptr = get_wisp_option("KCSITRACE"))
			{
				sscanf(ptr,"%d", &tracing_level);
			}
		}

		/* Restrict tracing_level to 1-4 */
		if (tracing_level > 4)
		{
			tracing_level = 4;
		}
		else if (tracing_level < 1) 
		{
			/* Turn tracing off with KCSITRACE=0 which sets it to default=4 */
			tracing_level = 4; 
		}		
	}
	
	return tracing_level;	
}

/*
	kcsi_exit() - Common exit logic.
*/
void kcsi_exit(int num)
{
	WL_wexit(num);
}


/*
**	History:
**	$Log: kcsit.c,v $
**	Revision 1.6.2.3  2002/11/14 15:23:33  gsl
**	Change wexit() to WL_wexit()
**	
**	Revision 1.6.2.2  2002/11/13 22:50:42  gsl
**	set trace level with get_wisp_option("KCSITRACE")
**	
**	Revision 1.6.2.1  2002/11/12 15:56:28  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.8  2002/10/17 21:22:42  gsl
**	cleanup
**	
**	Revision 1.7  2002/07/10 21:06:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.6  2002/05/14 21:46:45  gsl
**	ifdef unix include
**	remove unused status var
**	
**	Revision 1.4  2002-04-23 13:34:13-04  gsl
**	Drop the % symbol from the front of messages
**
**	Revision 1.3  2002-04-22 14:28:36-04  gsl
**	Rework the tracing to use werr_write() and werrlog(104,msg)
**	Added kcsi_tracelevel()
**
**	Revision 1.2  1997-08-01 14:10:07-04  scass
**	Removed include filepaths.h
**
**	Revision 1.1  1997-08-01 12:00:49-04  scass
**	Initial revision
**
**
*/
