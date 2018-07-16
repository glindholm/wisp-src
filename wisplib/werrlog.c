static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		werrlog.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Standard error reporting routines
**
**	Routines:	
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef VMS	/* unix or MSDOS */
#include <sys/types.h>
#include <sys/stat.h>
#endif

#if defined(MSDOS) || defined(_MSC_VER)
#include <io.h>
#endif

#include <errno.h>
#include <time.h>

#include "idsistd.h"
#include "wperson.h"
#include "wdefines.h"
#include "wglobals.h"
#include "osddefs.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "wispcfg.h"
#include "wanguid.h"
#include "wispvers.h"
#include "platsubs.h"

#define INIT_ERR
#include "werrlog.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/
static char emsg[512] = { 0 };								/* The error message.			*/
static char eform[256] = { 0 };						       		/* The formatting string.		*/
static uint4 last_dbid = 0;								/* The last message from the database.	*/

/*
**	Function Prototypes
*/
static int write_err(void);								/* Write error to log file.		*/
static int print_err(void);
static int find_msg(uint4 id);								/* Find the message in the message db.	*/




void werrlog(uint4 id, ...)
{
	va_list ap;
	
	static	int	in_use = 0;

	wglobals();									/* Pull in wglobals for VMS		*/

	w_err_code = id;								/* Save the error code.			*/

	if (0 == (w_err_flag & ENABLE_LOGGING)) return;					/* Logging is off.			*/

	if (w_err_code % 100 == 1)							/* It's a Routine entry log.		*/
	{
		if (0 == (w_err_flag & LOG_SUBROUTINE_ENTRY)) return;			/* Are we logging them?			*/
	}


	if (w_err_flag & LOG_EXCEPTIONS_ONLY)
	{
	    if (w_err_code & 1)	return;							/* Don't log warnings (odd numbers)	*/
	}

	if (in_use)
	{
		/*
		**	werrlog is already in use - we have recursively entered this routine.
		**	While reporting an error we generated another error - just break out.
		*/
		return;
	}
	else
	{
		in_use = 1;

		/* Form the error message.		*/

		if (!find_msg(id))							/* Is there a message for this error?	*/
		{
			sprintf(emsg,"%%WISP-E-NOMSG WISP error, message number %ld. ",id);	/* No message found.		*/
		}
		else
		{
			va_start(ap, id);
			
			vsprintf(emsg, eform, ap);					/* Generate the message.		*/

			va_end(ap);
		}

		if (w_err_flag & LOG_LOGFILE) write_err();				/* Write error to log file.		*/

		if (w_err_code % 2 == 0) print_err();		/* Display even errors 	*/

		in_use = 0;
	}
}

void werr_write(const char* buff)								/* Write out a buffer to the error log.	*/
{
	static int first = 1;
	FILE *efile;									/* Pointer to error log file.		*/

	efile = fopen(werrpath(),"a");							/* First try to append.			*/
	if (!efile) efile = fopen(werrpath(),FOPEN_WRITE_TEXT);				/* Doesn't exist, create it.		*/

	if (!efile)									/* If can't open then use stderr	*/
	{
		fprintf(stderr,"%s",buff);
		return;
	}

	fprintf(efile,"%s",buff);							/* dump it out as-is.			*/

	fclose(efile);									/* All done!				*/

	if ( first )
	{
		first = 0;								/* Now turn off first.			*/
		chmod(werrpath(), 0666);						/* Allow all to write.			*/
	}

}

static int write_err(void)								/* Write error to log file.		*/
{
	time_t clock;
	char	buff[512];

	buff[0] = '\0';
	if (!(w_err_code & 1)) 								/* If exceptions then identify.		*/
	{
		clock = time(0);
		sprintf(buff,"\n%sWISPRUNNAME(%8.8s) WISP_VERSION=[%s] PLATFORM=[%s]\n",
			ctime(&clock),WISPRUNNAME, wisp_version(), WL_platform_name());
	}                                                           
	sprintf(&buff[strlen(buff)],"(%6ld) %s\n",w_err_code, emsg);			/* Pretty easy.				*/
	werr_write(buff);
	w_err_logged = 1;								/* Flag we did one.			*/
	return(1);
}

static int print_err(void)								/* Write error to stdout		*/
{

	if ( wbackground() ) printf("\n\r%s\n\r",emsg);					/* Pretty easy.				*/
	else
	{
		emsg[255] = '\0';
		werr_message_box(emsg);
	}
	return(1);
}


static char msgfname[256] = { 0 };

static int find_msg(uint4 id)								/* Find the message in the message db.	*/
{
static	int	msgfile_not_found = 0;
	FILE *msgfile;
	uint4 cur_id, lo_idx, hi_idx, cur_idx, msg_idx, num_msgs, idx_diff;

	if (msgfile_not_found) return(0);
	if (last_dbid == id) return(1);							/* already got it in buffer.		*/

	if (!msgfname[0])								/* Need to get the name of the msg file	*/
	{
		const char	*path = wispconfigdir();

#ifndef VMS
		if (0 == strcmp(path,"$WISPCONFIG"))
		{
			/*
			**	WISPCONFIG is not set.
			*/
			msgfile_not_found = 1;
			strcpy(emsg,"%%WERRLOG-E-WISPCONFIG WISPCONFIG is undefined.");
			print_err();
			return(0);
		}
#endif	/* !VMS */

		buildfilepath(msgfname, path, WISP_MESSAGE_FILE);
	}

	msgfile = fopen(msgfname,FOPEN_READ_BINARY);					/* Open the indexed file.		*/

	if (!msgfile)									/* Unable to open message file		*/
	{
		msgfile_not_found = 1;
		sprintf(emsg,"%%WERRLOG-E-OPEN Unable to open message file %s",msgfname);
		print_err();
		return(0);
	}

	cur_idx = 0;									/* Use to point to count.		*/

	fseek(msgfile,cur_idx,0);							/* Find out how many in the file.	*/
	fread(&num_msgs,4,1,msgfile);

	lo_idx = 4;									/* The first ID is in byte 4.		*/
	hi_idx = (8 * num_msgs) - 4;							/* The last message id is here.		*/

	idx_diff = ((hi_idx - lo_idx) >> 1) & 0xfffffff8;				/* Get Median between 2 items. mod 8	*/

	cur_idx = lo_idx + idx_diff;							/* Set current position.		*/

	msg_idx = 0;									/* No message found.			*/

	fseek(msgfile,cur_idx,0);							/* Seek to current value.		*/
	fread(&cur_id,4,1,msgfile);							/* And read it.				*/

	for (;;)
	{
		if (id == cur_id || (lo_idx >= hi_idx))					/* Found it, or not there.		*/
		{
			break;								/* GET OUT, EH!				*/
		}
		else if (id > cur_id)							/* If id is greater than current.	*/
		{									/* Need to advance to hi index.		*/
			idx_diff = ((hi_idx - cur_idx) >> 1) & 0xfffffff8;		/* Get Median between 2 items. Mod 8	*/
			lo_idx = cur_idx + 8;						/* This is the bottom now.		*/
			if (!idx_diff) idx_diff = 8;
			cur_idx = cur_idx + idx_diff;
		}
		else
		{
			idx_diff = ((cur_idx - lo_idx) >> 1) & 0xfffffff8;		/* Get Median between 2 items. Mod 8	*/
			hi_idx = cur_idx - 8;						/* This is the top now.			*/
			if (!idx_diff) idx_diff = 8;
			cur_idx = cur_idx - idx_diff;
		}

		fseek(msgfile,cur_idx,0);						/* Seek to current value.		*/
		fread(&cur_id,4,1,msgfile);						/* And read it.				*/
	}

	if (id == cur_id)
	{
		cur_idx = cur_idx + 4;							/* point to where message is.		*/
		fseek(msgfile,cur_idx,0);						/* Position the file.			*/
		fread(&msg_idx,4,1,msgfile);						/* Read the index value.		*/
		cur_idx = cur_idx + 8;							/* Get index value of next message.	*/
		fseek(msgfile,cur_idx,0);						/* Seek to it.				*/
		fread(&cur_idx,4,1,msgfile);						/* Get address of next message.		*/
		idx_diff = cur_idx - msg_idx;						/* This is the length of the message.	*/
	}
	else
	{
		fclose(msgfile);
		return(0);								/* No message found.			*/
	}
											/* At this point, msg_idx points to the	*/
											/* location where the message index is.	*/
											/* And idx_diff is the length.		*/
	fseek(msgfile,msg_idx,0);							/* Now point to the message.		*/
	fread(&eform[0],(int)idx_diff,1,msgfile);					/* Read the text.			*/

	eform[idx_diff] = 0;								/* Null terminate.			*/

	fclose(msgfile);

	last_dbid = id;									/* Remember it.				*/

	return(1);									/* All done.				*/
}

#ifdef VMS
void werrset(void)
{
}
#else /* !VMS */
void werrset(void)
{
	char	*ptr;
	char	buff[50];

	ptr = getenv("WISPDEBUG");

	if (! ptr) return;

	strcpy(buff,ptr);
	upper_string(buff);

	if ( strcmp(buff,"FULL") == 0 )
	{
		w_err_flag = ENABLE_LOGGING + LOG_LOGFILE + LOG_SUBROUTINE_ENTRY; /* 19 */
		return;
	}	

	if ( strcmp(buff,"ENTRY") == 0 )
	{
		w_err_flag = ENABLE_LOGGING + LOG_LOGFILE + LOG_SUBROUTINE_ENTRY + LOG_EXCEPTIONS_ONLY; /* 27 */
		return;
	}	
}
#endif	/* !VMS */

/*
**	ROUTINE:	wtrace()
**
**	FUNCTION:	Write a trace message to the wisp error log (wisperr.log)
**
**	DESCRIPTION:	%%ROUTINE-T-CODE format ..
**
**	ARGUMENTS:	
**	routine		The routine name
**	code		A code for header
**	format		A printf() style format message
**	...		arguments for the format message
**
**	GLOBALS:	
**	w_err_flag	The error flag mask	
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wtrace(const char* routine, const char* code, const char* format, ... /* args */)
{
	va_list ap;
	char	buff[1024], msg[1024];

	if (!wtracing())
	{
		return;
	}

	va_start(ap, format);
			
	vsprintf(buff, format, ap);

	va_end(ap);

	sprintf(msg, "(WTRACE) %%%s-T-%s %s\n", routine, code, buff);

	werr_write(msg);
}

/*
**	ROUTINE:	wtracing()
**
**	FUNCTION:	Test if we are tracing.
**
**	DESCRIPTION:	Check the flags to see if wtrace() is active.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	w_err_flag
**
**	RETURN:		
**	0		Not Tracing
**	1		Tracing
**
**	WARNINGS:	none
**
*/
int wtracing(void)
{
	if (!(w_err_flag & ENABLE_LOGGING) 	 ||
	    !(w_err_flag & LOG_SUBROUTINE_ENTRY) ||
	    !(w_err_flag & LOG_LOGFILE) 	    )
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

/*
**	ROUTINE:	wtrace_timestamp()
**
**	FUNCTION:	Write a timestamp if tracing.
**
**	DESCRIPTION:	If tracing then write a timestamp.
**
**	ARGUMENTS:	
**	routine		The routine name for wtrace()
**
**	GLOBALS:	None
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
void wtrace_timestamp(const char *routine)
{
	if (wtracing())
	{
		time_t	clock;
		char	timestamp[40];

		clock = time(0);
		strcpy(timestamp, ctime(&clock));
		timestamp[strlen(timestamp) - 1] = '\0';				/* Remove the trailing newline 		*/

		wtrace(routine, "TIMESTAMP", "%s %8.8s %s", longuid(), WISPRUNNAME, timestamp);
	}
}

/*
**	ROUTINE:	WL_strerror()
**
**	FUNCTION:	Get the errno message string
**
**	DESCRIPTION:	strerror() is not thread-safe on some systems
**
**	ARGUMENTS:	
**	errnum		the errno to get message for
**
**	RETURN:		Message string
**
**	WARNINGS:	none
**
*/
const char* WL_strerror(int errnum)
{
	const char* ptr = NULL;

#ifdef unix
#ifndef LINUX
	extern char *sys_errlist[];
	extern int   sys_nerr;
#endif

	if (errnum >= 0 &&
	    errnum < sys_nerr) 
	{
		ptr = sys_errlist[errnum];
	}
#endif

#ifdef WIN32
	ptr =  strerror(errnum);
#endif

	if (NULL == ptr)
	{
		static char mess[30];
		sprintf(mess,"Unknown errno=[%d]", errnum);
		ptr = mess;
	}

	return ptr;
}



/*
**	History:
**	$Log: werrlog.c,v $
**	Revision 1.18.2.4  2003/02/13 17:03:30  gsl
**	Add WISP version and platform to errors
**	
**	Revision 1.18.2.3  2003/02/13 16:46:34  gsl
**	Clean up error mode logic that determines if logging errors
**	Fix so all errors (even codes) are displayed on screen when tracing
**	
**	Revision 1.18.2.2  2002/11/12 16:00:22  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.18.2.1  2002/10/09 19:48:08  gsl
**	Added WL_strerror()
**	
**	Revision 1.18  1998/12/09 14:42:40  gsl
**	Use FOPEN mode defines
**	
**	Revision 1.17  1998-05-12 10:57:15-04  gsl
**	Add wtracing() and wtrace_timestamp()
**
**	Revision 1.16  1997-04-15 22:44:40-04  gsl
**	Add wtrace() for tracing to wisperr.log
**
**	Revision 1.15  1996-10-08 20:27:51-04  gsl
**	replaced getenv() with wispconfigdir()
**
**	Revision 1.14  1996-09-04 17:22:27-07  gsl
**	Removed extern var
**
**	Revision 1.13  1996-07-10 16:56:35-07  gsl
**	change to use werrpath() and fix includes and prototypes for NT
**
**	Revision 1.12  1996-07-08 11:15:40-07  gsl
**	Fix to use stdargs and use vsprintf() to form error message
**
**	Revision 1.11  1995-09-25 10:04:41-07  gsl
**	changed fopen() to use FOPEN_READ_BINARY
**	added include of osddefs.h
**
 * Revision 1.10  1995/04/25  09:54:40  gsl
 * drcs state V3_3_15
 *
 * Revision 1.9  1995/04/17  11:47:42  gsl
 * drcs state V3_3_14
 *
 * Revision 1.8  1995/02/17  13:07:11  gsl
 * change to use werr_message_box()
 *
 * Revision 1.7  1995/02/17  12:06:42  gsl
 * change to use vwang_message_box() instead of werrvre()
 *
# Revision 1.5  1995/02/14  15:59:00  gsl
# *** empty log message ***
#
**
**
*/
