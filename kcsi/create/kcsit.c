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
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

#include "intdef.h"
#include "kcsit.h"

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
	static int first = 1;
	static int tracing_level;
	static FILE *logfile;
	static char tracefile[120];

	if (first)
	{
		char *ptr;
		int4 status;

		first = 0;
		tracing_level = 4;
		*tracefile = '\0';
		logfile = NULL;
		
		if ( ptr = getenv("KCSITRACE") )			       		/* Test if an environment var exists.   */
		{
			sscanf(ptr,"%d", &tracing_level);
		}

		ptr = getenv("HOME");
		if (!ptr) ptr = ".";
		
		buildfilepath(tracefile, ptr, "kcsitrace.log");
	}

	va_start(add_ptr, lform);

	/*
	**	If serverity is greater then the tracing level then 
	**	write this message to the trace log.
	*/
	if ( sever >= tracing_level )
	{
		if (!logfile)
		{
			/*
			**	Open the tracefile the first time in.
			*/
			logfile = fopen(tracefile,"a");
			if (!logfile)
			{
				/*
				**	If You can open the trace file use stderr.
				*/
				logfile = stderr;
				fprintf(logfile,"%%%s-%d-%s ","KCSITRACE", 4, "Unable to open trace file, using stderr\n");
			}
		}

		if (sever == 1) 
		{
			fprintf(logfile,"1  %%%s-%d-%s ",routine,sever,mess);
		}
		else 
		{
			time_t	the_clock;
			the_clock = time(0);
			if (sever >= 4) fprintf(logfile,"****\n");
			fprintf(logfile,"-  %s",ctime(&the_clock));
			fprintf(logfile,"%d  %%%s-%d-%s ",sever,routine,sever,mess);
		}
		vfprintf(logfile,lform,add_ptr);
		fprintf(logfile,"\n");
		if (sever >= 4) fprintf(logfile,"****\n");

		fflush(logfile);
	}

	if (sever >= 4)									/* if will effect processing, report it	*/
	{
		fprintf(stderr,"\r\n********************************************************************************\r\n");
		fprintf(stderr,"%%%s-%d-%s ",routine,sever,mess);
		vfprintf(stderr,lform,add_ptr);
		fprintf(stderr,"\r\n********************************************************************************\r\n");
		
		if ( isatty(2) ) 
		{
			fprintf(stderr, "(Pausing ...)\r\n");
			sleep( 2 );
		}
	}
	va_end(add_ptr);
}

/*
**	History:
**	$Log: kcsit.c,v $
**	Revision 1.2  1997-08-01 14:10:07-04  scass
**	Removed include filepaths.h
**
**	Revision 1.1  1997-08-01 12:00:49-04  scass
**	Initial revision
**
**
*/
