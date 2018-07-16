/*
** $Id:$
** WISP - Wang Interchange Source Processor
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
*/

/*
**	File:		werrlog.c
**
**	Project:	wisp/lib
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

#include <sys/types.h>
#include <sys/stat.h>

#if defined(WIN32)
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include <errno.h>
#include <time.h>
#include <string.h>

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

#include "werrlog.h"

/*
**	Structures and Defines
*/
#define ERRBUFF_LEN 2048

/* Flag bits in "w_errflag" are defined as follows:										*/
/*																*/
/*	Bit		Meaning													*/
/*																*/
/*	 0		1 : Enable Logging, 0 : Disable logging.								*/
#define	    ENABLE_LOGGING		1
/*	 1		1 : Logging to log file, 0 : No logging to log file.							*/
#define	    LOG_LOGFILE			2
/*	 2		1 : Logging to screen (stderr), 0 : No logging to stderr.						*/
#define	    LOG_SCREEN			4
/*	 3		1 : Log exceptions only. 0 : Log all errors.								*/
#define	    LOG_EXCEPTIONS_ONLY		8
/*	 4		1 : Log Subroutine entry messages. 0 : Don't log entry messages.					*/
#define	    LOG_SUBROUTINE_ENTRY	16


/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Function Prototypes
*/
static void write_err(uint4 err, const char *mess);
static void print_err(const char *mess);
static const char* werrcode_lookup(uint4 err);

void  wisp_set_werrlog_flag(uint4 i)	
{
	if (i == LOG_EXCEPTIONS_ONLY)	/* 8 == special code means disable completely */
	{
		WL_set_wispdebug(WISPDEBUG_NONE);
	}
	else if ( (i & ENABLE_LOGGING) && (i & LOG_SUBROUTINE_ENTRY) )
	{
		WL_set_wispdebug(WISPDEBUG_FULL);
	}
	else
	{
		WL_set_wispdebug(WISPDEBUG_ERRORS); /* Default */
	}
}

/*
**	WISPDEBUG Modes
**		ERRORS	Log and display only exceptions (Default)
**		FULL	Log all messages 
**		NONE	No logging
*/
static int wispdebug_mode = WISPDEBUG_ERRORS;

void WL_set_wispdebug(int mode)
{
	switch(mode)
	{
	case WISPDEBUG_NONE:
	case WISPDEBUG_FULL:
	case WISPDEBUG_ERRORS:
		/* valid mode */
		break;
	default:
		/* invalid mode - use default */
		mode = WISPDEBUG_ERRORS;
		break;
	}

	if (mode != wispdebug_mode) /* If mode changes */
	{
		/*
		**	BEFORE changing mode
		*/
		if (WISPDEBUG_FULL == wispdebug_mode) /* Was FULL */
		{
			if (WISPDEBUG_ERRORS == mode)
			{
				WL_wtrace("WISPDEBUG","ERRORS","Changing logging mode to WISPDEBUG=ERRORS");
			}
			else
			{
				WL_wtrace("WISPDEBUG","NONE","Changing logging mode to WISPDEBUG=NONE");
			}
		}

		wispdebug_mode = mode;	/* Change the mode */

		/*
		**	AFTER changing mode
		*/
		if (WISPDEBUG_FULL == wispdebug_mode) /* Changed to FULL */
		{
			WL_wtrace("WISPDEBUG","FULL","Changing logging mode to WISPDEBUG=FULL");
		}
	}
}

int  WL_get_wispdebug(void)
{
	return wispdebug_mode;
}

static	int error_logging_in_use = 0;

void WL_werrlog(uint4 errcode, ...)
{
	va_list ap;

	WL_globals();

	if (WISPDEBUG_NONE == wispdebug_mode)
	{
		return;		/* Logging is off. */
	}

	if (errcode % 100 == 1 &&			/* It's a routine entry (ends in 01)	*/
	    WISPDEBUG_ERRORS == wispdebug_mode)		/* but only interested in errors	*/
	{
		return;		/* Not logging routine entry points */ 
	}

	if (errcode & 1 &&				/* It's a warning (odd numbers)		*/
	    WISPDEBUG_ERRORS == wispdebug_mode)		/* but only interested in errors	*/
	{
		return;		/* Not logging warnings */ 
	}

	if (error_logging_in_use != 0)
	{
		/*
		**	werrlog is already in use - we have recursively entered this routine.
		**	While reporting an error we generated another error - just break out.
		*/
		return;
	}
	else
	{
		char mess[ERRBUFF_LEN];
		const char* format;
		error_logging_in_use++;

		format = werrcode_lookup(errcode); /* Lookup the format string */

		if (NULL != format)	/* Is there a message for this error?	*/
		{
			va_start(ap, errcode);
			vsprintf(mess, format, ap);	/* Generate the message.		*/
			va_end(ap);

			/*
			**	If errno is part of the error string then
			**	tack on the errno message string
			*/
			if (NULL != strstr(format,"errno"))
			{
				strcat(mess, " (");
				strcat(mess, WL_strerror(errno));
				strcat(mess, ")");
			}
		}
		else
		{
			sprintf(mess,"%%WISP-E-NOMSG WISP error, message number %ld. ",(long)errcode);/* No message found.		*/
		}

		write_err(errcode, mess);	/* Write error to log file.		*/

		if (errcode % 2 == 0)		/* Even codes are errors		*/
		{
			print_err(mess);	/* Display errors 			*/
		}

		error_logging_in_use--;
	}
}

/*
**	ROUTINE:	WL_werrlog_error()
**
**	FUNCTION:	Write a error message to the wisp error log (wisperr.log)
**
**	DESCRIPTION:	(errcode) %%ROUTINE-E-CODE format ..
**
**	ARGUMENTS:
**	errcode		Error number (even)
**	routine		The routine name
**	code		A code for header
**	format		A printf() style format message
**	...		arguments for the format message
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void WL_werrlog_error(int errcode, const char* routine, const char* code, const char* format, ... /* args */)
{
	va_list ap;
	char	buff[ERRBUFF_LEN], mess[ERRBUFF_LEN];

	if (WISPDEBUG_NONE == wispdebug_mode)
	{
		return;		/* Errors are not being logged. */
	}

	if (0==error_logging_in_use)
	{
		error_logging_in_use++;

		va_start(ap, format);
		vsprintf(buff, format, ap);
		va_end(ap);

		sprintf(mess, "%%%s-E-%s %s\n", routine, code, buff);

		write_err(errcode, mess);

		if (WISPDEBUG_ERRORS == wispdebug_mode) 
		{
			print_err(mess);		/* Print error to stderr.		*/
		}

		error_logging_in_use--;
	}
}

/*
**	ROUTINE:	WL_werrlog_warn()
**
**	FUNCTION:	Write a warning message to the wisp error log (wisperr.log)
**
**	DESCRIPTION:	(errcode) %%ROUTINE-W-CODE format ..
**
**	ARGUMENTS:
**	errcode		Error number (odd)
**	routine		The routine name
**	code		A code for header
**	format		A printf() style format message
**	...		arguments for the format message
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void WL_werrlog_warn(int errcode, const char* routine, const char* code, const char* format, ... /* args */)
{
	va_list ap;
	char	buff[ERRBUFF_LEN], mess[ERRBUFF_LEN];

	if (WISPDEBUG_FULL != wispdebug_mode)
	{
		return;		/* Warnings are not being logged. */
	}

	if (0 == error_logging_in_use)
	{
		error_logging_in_use++;

		va_start(ap, format);
		vsprintf(buff, format, ap);
		va_end(ap);

		sprintf(mess, "%%%s-W-%s %s\n", routine, code, buff);

		write_err(errcode, mess);

		error_logging_in_use--;
	}
}



void WL_werr_write(const char* buff)								/* Write out a buffer to the error log.	*/
{
	static int first = 1;
	FILE *efile;									/* Pointer to error log file.		*/

	efile = fopen(WL_werrpath(),"a");							/* First try to append.			*/
	if (!efile) efile = fopen(WL_werrpath(),FOPEN_WRITE_TEXT);			/* Doesn't exist, create it.		*/

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
#ifdef unix
		chmod(WL_werrpath(), 0666);						/* Allow all to write.			*/
#endif
	}

}

static void write_err(uint4 err, const char *mess)					/* Write error to log file.		*/
{
	time_t clock;
	char	buff[ERRBUFF_LEN];

	buff[0] = '\0';
	if (!(err & 1)) 								/* If exceptions then identify.		*/
	{
		clock = time(0);
		sprintf(buff,"\n%sWISPRUNNAME(%8.8s) WISP_VERSION=[%s] PLATFORM=[%s]\n",
			ctime(&clock),wisp_get_runname(), wisp_version(), WL_platform_name());
	}                                                           
	sprintf(&buff[strlen(buff)],"(%6ld) %s\n",(long)err, mess);
	WL_werr_write(buff);
}

static void print_err(const char *mess)							/* Write error to stdout		*/
{

	if ( wbackground() ) 
	{
		fprintf(stderr, "\n\r%s\n\r",mess);					/* Pretty easy.				*/
	}
	else
	{
		WL_werr_message_box(mess);
	}
}


void WL_werr_override(void) /* Check for error logging override. */
{
	char	*ptr;

	ptr = getenv("WISPDEBUG");

	if (! ptr) return;

	if ( strcmp(ptr,"FULL") == 0 )
	{
		WL_set_wispdebug(WISPDEBUG_FULL); 
	}	
	else if ( strcmp(ptr,"NONE") == 0 )
	{
		WL_set_wispdebug(WISPDEBUG_NONE); 
	}
	else
	{
		WL_set_wispdebug(WISPDEBUG_ERRORS); /* Default */
	}
}


/*
**	ROUTINE:	WL_wtrace()
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
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void WL_wtrace(const char* routine, const char* code, const char* format, ... /* args */)
{
	va_list ap;
	char	buff[ERRBUFF_LEN], mess[ERRBUFF_LEN];

	if (!WL_wtracing())
	{
		return;
	}

	va_start(ap, format);
	vsprintf(buff, format, ap);
	va_end(ap);

	sprintf(mess, "(WTRACE) %%%s-T-%s %s", routine, code, buff);

	/*
	**	If errno is part of the message string then
	**	tack on the errno message string
	*/
	if (NULL != strstr(format,"errno"))
	{
		strcat(mess, " (");
		strcat(mess, WL_strerror(errno));
		strcat(mess, ")");
	}
	strcat(mess, "\n");

	WL_werr_write(mess);
}

void WL_wtrace_entry(const char* routine)
{
	WL_wtrace(routine,"ENTRY","Entry into %s", routine);
}

/*
**	ROUTINE:	WL_wtracing()
**
**	FUNCTION:	Test if we are tracing.
**
**	DESCRIPTION:	Check the flags to see if wtrace() is active.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	wispdebug_mode
**
**	RETURN:		
**	0		Not Tracing
**	1		Tracing
**
**	WARNINGS:	none
**
*/
int WL_wtracing(void)
{
	if ( WISPDEBUG_FULL == wispdebug_mode ) 
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/*
**	ROUTINE:	WL_wtrace_timestamp()
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
void WL_wtrace_timestamp(const char *routine)
{
	if (WL_wtracing())
	{
		time_t	clock;
		char	timestamp[40];

		clock = time(0);
		strcpy(timestamp, ctime(&clock));
		timestamp[strlen(timestamp) - 1] = '\0';				/* Remove the trailing newline 		*/

		WL_wtrace(routine, "TIMESTAMP", "%s %8.8s %s", WL_longuid(), wisp_get_runname(), timestamp);
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

#ifdef USE_SYS_ERRLIST
	extern char *sys_errlist[];
	extern int   sys_nerr;


	if (errnum >= 0 &&
	    errnum < sys_nerr) 
	{
		ptr = sys_errlist[errnum];
	}

	if (NULL == ptr)
	{
		static char mess[30];
		sprintf(mess,"Unknown errno=[%d]", errnum);
		ptr = mess;
	}
#endif

#ifndef USE_SYS_ERRLIST
	ptr =  strerror(errnum);
#endif

	return ptr;
}

static struct
{
	uint4 errcode;
	const char *mess;
} wispmsg_table[] = {
	{WERRCODE(101),   "%%COBOL-I-ENTRY PROGRAM-ID %8.8s TIME %s"},
	{WERRCODE(102),   "%%TEXT '%s'"},
	{WERRCODE(103),   "%%TEXT '%s'"},
	{WERRCODE(104),   "%s"},
	{WERRCODE(105),   "%s"},
	{WERRCODE(11002), "%%DATE-E-NOTYET Function %c%c not implemented"},
	{WERRCODE(11102), "%%DATE4-E-NOTYET Function %c%c not implemented"},
	{WERRCODE(11200), "%%DATE6-E-ERROR"},
	{WERRCODE(13002), "%%DAY-E-INVALID invalid date [%s]"},
	{WERRCODE(17002), "%%EXTRACT-E-NOTSUP %8.8s: EXTRACT of %2.2s is NOT SUPPORTED"},
	{WERRCODE(17004), "%%EXTRACT-E-NOTYET %8.8s: EXTRACT of %2.2s is NOT YET IMPLEMENTED"},
	{WERRCODE(17006), "%%EXTRACT-E-DEVTYPE (gettype) Error accessing device TYPES information"},
	{WERRCODE(17008), "%%EXTRACT-E-INVPARM %8.8s: EXTRACT of %2.2s has invalid number of parameters"},
	{WERRCODE(17010), "%%EXTRACT-E-CFGFILE Can't open config file: %s [errno=%d]."},
	{WERRCODE(18202), "%%FILECOPY-E-INVD Invalid arg list: File[%8.8s] Lib[%8.8s] Vol[%6.6s]"},
	{WERRCODE(18204), "%%FILECOPY-E-NOTYET  FILECOPY: File[%8.8s] Lib[%8.8s] Vol[%6.6s] no yet supported."},
	{WERRCODE(19002), "%%FIND-F-NOTYET Receiver type (F) not yet supported"},
	{WERRCODE(20004), "%%GETPARM-F-ARGUMENTS Unable to decipher.(%s - %d) (Add optional args) "},
	{WERRCODE(20006), "%%GETPARM-E-NOTSUP Function (%s) not supported"},
	{WERRCODE(20014), "%%GETPARM-E-MAXFIELDS Max 32 modifiable fields"},
	{WERRCODE(24002), "%%INITWISP-E-WSWAP Trying to turn word swap back on"},
	{WERRCODE(28002), "%%LINK-E-MAXDEPTH Maximum link depth reached %d"},
	{WERRCODE(28003), "%%LINK-I-BADPARAM Progname %8.8s Invalid parameter TYPE %1.1s"},
	{WERRCODE(28004), "%%LINK-E-EXECVE Exec to %.60s failed [errno=%d]"},
	{WERRCODE(28008), "%%LINK-F-MAXPARMS Maximum %d parms. Called with %d parms."},
	{WERRCODE(28010), "%%LINK-F-MKDIR Unable to mkdir %s [errno=%d]"},
	{WERRCODE(28012), "%%LINK-F-FOPEN Unable to fopen %s [errno=%d] in %s"},
	{WERRCODE(28014), "%%LINK-F-EXECVP Exec of %.60s failed while linking to %8.8s [errno=%d]"},
	{WERRCODE(28016), "%%LINK-E-RECURSIVE Recursive LINK to file %.60s"},
	{WERRCODE(28018), "%%LINK-E-FORK Fork failed [errno=%d]"},
	{WERRCODE(28020), "%%LINK-E-ERROR %s"},
	{WERRCODE(28022), "%%LINK-E-FRONTEND %s failed to run. Check for COBOL RTS error."},
	{WERRCODE(28502), "%%LINKPROC-F-PARMCNT Invalid parmcnt %d"},
	{WERRCODE(28504), "%%LINKPROC-F-EXEC Exec failed to %.60s [errno=%d]"},
	{WERRCODE(35002), "%%MESSAGE-E-TOOFEW Too few args for call (%d)"},
	{WERRCODE(35004), "%%MESSAGE-E-UNKNOWN Function code is unknown (%2.2s)"},
	{WERRCODE(35016), "%%MESSAGE-E-KEY Unable to %s file %s."},
	{WERRCODE(46404), "%%LP_PRINT-E-PRINTQUEUE System call failed to %s file=%s rc=%d [errno=%d]"},
	{WERRCODE(46502), "%%LP_PRINT-E-LP System call failed to LP %s rc=%d [errno=%d]"},
	{WERRCODE(48018), "%%PUTPARM-E-NOTYET Function (%c) not implemented"},
	{WERRCODE(51002), "%%READFDR-E-BLOCK Block request mode not supported"},
	{WERRCODE(51004), "%%READFDR-E-NOTSUP Field ID of %2.2s is NOT SUPPORTED"},
	{WERRCODE(52002), "%%READVTOC-E-NOTYET Option (%c) Not Yet Implemented"},
	{WERRCODE(52004), "%%READVTOC-E-NOTSUPP Option (%c) Not Supported"},
	{WERRCODE(53002), "%%RENAME-E-INVD Invalid arg list: Func[%c], File[%8.8s] Lib[%8.8s] Vol[%6.6s]"},
	{WERRCODE(53004), "%%RENAME-E-INVD Invalid arg list: Func[%c], New File[%8.8s] New Lib[%8.8s]"},
	{WERRCODE(54002), "%%SCRATCH-E-ERROR %s"},
	{WERRCODE(57002), "%%SCREEN-E-FUNCTION Unsupported function (%d)"},
	{WERRCODE(57004), "%%SCREEN-E-FILE Unable to open file %s [errno=%d]"},
	{WERRCODE(57006), "%%SCREEN-E-MALLOC Unable to malloc %d bytes."},
	{WERRCODE(57008), "%%SCREEN-E-PRNTING Error (%d) when printing screen"},
	{WERRCODE(58002), "%%SET-E-NOTSUP SET of %2.2s is NOT SUPPORTED"},
	{WERRCODE(58004), "%%SET-E-NOTYET SET of %2.2s is NOT YET IMPLEMENTED"},
	{WERRCODE(58006), "%%SET-E-ARGCNT Invalid argument count"},
	{WERRCODE(59004), "%%SETRETCODE-E-CREATE Unable to create %s [errno=%d]"},
	{WERRCODE(59802), "%%SHAREMEM-F-OPTIONS Invalid combination MAXPRBPARMS=%d MAXPRBPAGES=%d"},
	{WERRCODE(59804), "%%SHAREMEM-F-NOMEM Not enough global memory to store PUTPARM count=%d, size=%d "},
	{WERRCODE(59806), "%%SHAREMEM-F-MAXPPARMS Maximum PUTPARMs (%d) exceeded."},
	{WERRCODE(59808), "%%SHAREMEM-F-CORRUPT The PUTPARM blocks in shared memory have been corrupted."},
	{WERRCODE(59810), "%%SHAREMEM-F-GETGBLPTR Unable to access PUTPARM data in shared memory. Status=%x (hex)"},
	{WERRCODE(59812), "%%SHAREMEM-F-VERSION PUTPARM/GETPARM version mismatch file=%s, current=%s"},
	{WERRCODE(59814), "%%SHAREMEM-F-DEASSIGN Error when deassigning channel. status=%x (hex)"},
	{WERRCODE(59816), "%%SHAREMEM-F-UNLINK Error deleting PUTPARM temp file %s [errno=%d]"},
	{WERRCODE(59826), "%%SHAREMEM-F-NOTMAPPED Error in 'wax_table_entry', PUTPARM global area not mapped."},
	{WERRCODE(59828), "%%SHAREMEM-F-MKDIR Can't create PUTPARM tmp dir %s. [errno=%d]"},
	{WERRCODE(59830), "%%SHAREMEM-F-CHMOD Can't change protection of %s. [errno=%d]"},
	{WERRCODE(59832), "%%SHAREMEM-F-FOPEN Error opening PUTPARM shared memory file %s. [errno=%d]"},
	{WERRCODE(59834), "%%SHAREMEM-F-SHMGET Error getting PUTPARM shared memory id, shmkey=%d, [errno=%d]"},
	{WERRCODE(59836), "%%SHAREMEM-F-SHMAT Error getting PUTPARM shared memory address , shmid=%d, [errno=%d]"},
	{WERRCODE(59842), "%%SHAREMEM-E-UPDATE Failed to update PRB prname=%s label=%s"},
	{WERRCODE(59844), "%%SHAREMEM-E-UPDATE Memory calc size error PRB prname=%s label=%s"},
	{WERRCODE(59846), "%%SHAREMEM-F-BACKREF Backwards reference failed [%s]"},
	{WERRCODE(60002), "%%SORT-E-NOLOCAT Locator type sort not implemented"},
	{WERRCODE(60004), "%%SORT-E-SIZERR Specified element size [%d] too large"},
	{WERRCODE(60006), "%%SORT-E-BOUNDSERR Sort field [Start:%d,Len:%d] extends past end of record"},
	{WERRCODE(61502), "%%SORTLINK-F-ARGCNT Invalid argument count arg_count=%ld"},
	{WERRCODE(61504), "%%SORTLINK-F-MERGE Function [%c] not supported"},
	{WERRCODE(61506), "%%SORTLINK-F-OPTION %s=%c not supported"},
	{WERRCODE(61508), "%%SORTLINK-F-OPTION %s=%ld not supported"},
	{WERRCODE(61510), "%%SORTLINK-F-ARGUMENTS Unable to decode arguments [%s] arg=%ld"},
	{WERRCODE(63002), "%%STRING-E-NOTSUP STRING with %2.2s is NOT SUPPORTED"},
	{WERRCODE(63004), "%%STRING-E-INVALPARM Call to STRING with invalid parameters"},
	{WERRCODE(64002), "%%SUBMIT-F-MALLOC Malloc of %d bytes failed for envstring %s"},
	{WERRCODE(64004), "%%SUBMIT-F-PUTENV Putenv failed for %s"},
	{WERRCODE(64006), "%%SUBMIT-F-EXEC Exec failed for %s [errno=%d] retcode %d"},
	{WERRCODE(64008), "%%SUBMIT-F-FORK Fork failed [errno=%d]"},
	{WERRCODE(65112), "%%FINDTTY-E-CFGFILE Invalid device number = %d tty = %s"},
	{WERRCODE(65202), "%%WUSAGE-E-ERROR Error parsing command"},
	{WERRCODE(65204), "%%WUSAGE-E-MISSRC Missing Source item"},
	{WERRCODE(65206), "%%WUSAGE-E-MISOBJ Missing Object"},
	{WERRCODE(65208), "%%WUSAGE-E-INVSRC Invalid Source item"},
	{WERRCODE(65210), "%%WUSAGE-E-INVOBJ Invalid Object item"},
	{WERRCODE(65212), "%%WUSAGE-E-EXARGS Extra Arguments"},
	{WERRCODE(65214), "%%WUSAGE-E-FILENOTFOUND Specified file not found - Used defaults"},
	{WERRCODE(65216), "%%WUSAGE-E-INVFLAGOBJ Invalid flag object item "},
	{WERRCODE(65218), "%%WUSAGE-E-INVFLAG Invalid flag item"},
	{WERRCODE(65220), "%%WUSAGE-E-INVFLAGLOG Invalid flag logical item"},
	{WERRCODE(66502), "%%VDISPLAY-F-Invalid record size (%d), file not displayed"},
	{WERRCODE(66504), "%%VDISPLAY-F-Invalid buffer size (%d) (rec size - %d), file not displayed"},
	{WERRCODE(66506), "%%VDISPLAY-F-Error opening file %s, action aborted"},
	{WERRCODE(66508), "%%VDISPLAY-F-Run-time error, unable to allocate memory"},
	{WERRCODE(66510), "%%VDISPLAY-F-Error reading file %s, action aborted"},
	{WERRCODE(66512), "%%VDISPLAY-E-Empty file %s, action aborted"},
	{WERRCODE(66514), "%%VDISPLAY-E-Invalid record size (%s), action aborted"},
	{WERRCODE(67002), "%%WVIDEO-E-FUNCTION Invalid function (%d)"},
	{WERRCODE(67004), "%%WVIDEO-E-WRITE Invalid Row (%d) or Row+Lines (%d)"},
	{WERRCODE(67006), "%%WVIDEO-E-WRITE Scrolling Not Yet Implemented"},
	{WERRCODE(67008), "%%WVIDEO-E-READ Invalid Row (%d) or Row+Lines (%d)"},
	{WERRCODE(67010), "%%WVIDEO-E-(ws_tag_altered) No FAC for current field"},
	{WERRCODE(67012), "%%WVIDEO-E-(wpushscr) Malloc failed to get memory for screen maps"},
	{WERRCODE(67014), "%%WVIDEO-E-(wpopscr) Stack is empty, no screen to pop"},
	{WERRCODE(67016), "%%WVIDEO-E-(order-area) Invalid cursor column - %d in order area"},
	{WERRCODE(67018), "%%WVIDEO-E-(order-area) Invalid cursor row - %d in order area"},
	{WERRCODE(67020), "%%WVIDEO-E-(trigger key) Status = %d.  Error trying to activate"},
	{WERRCODE(67022), "%%WVIDEO-F-(wsdmp_scrn) error = %d.  Error trying to write to terminal"},
	{WERRCODE(67024), "%%WVIDEO-F-READ Error on VIDEO READ [error=%d]"},
	{WERRCODE(67026), "%%WVIDEO-E-TERMLIST Invalid PFKEY list entry [%c%c] (All keys activated)"},
	{WERRCODE(67028), "%%WVIDEO-E-TERMLIST PFKEY list is too long. (All keys activated)"},
	{WERRCODE(70002), "%%WCHAIN-E-NOTFOUND File %s not found"},
	{WERRCODE(70004), "%%WCHAIN-E-ISEXEC File %s is not executable"},
	{WERRCODE(70006), "%%WCHAIN-E-EXEC wchain to %s failed with errno=%d"},
	{WERRCODE(70008), "%%WCHAIN-E-EXEC wchain to %s %s %s failed with errno=%d"},
	{WERRCODE(74402), "%%WEXITINT-F-SIGNAL User signalled interrupt, (%d)"},
	{WERRCODE(74502), "%%WEXITBUG-F-SIGNAL Terminating on fatal signal (%d)"},
	{WERRCODE(77504), "%%WFNAME-E-LOGWORKLIB Unable to open %s errno=%d"},
	{WERRCODE(80502), "%%WISPSORT-E-ERROR %s"},
	{WERRCODE(83002), "%%WPERSON-F-PID Missing WISP_PID_ENV"},
	{WERRCODE(83004), "%%WPERSON-F-TTY Missing WISP_TTY_ENV"},
	{WERRCODE(83006), "%%WPERSON-F-MALLOC Unable to malloc %s"},
	{WERRCODE(83008), "%%WPERSON-F-FACCESS Error accessing file %s    "},
	{WERRCODE(83010), "%%WPERSON-F-FOPEN wps_file unable to open %s"},
	{WERRCODE(83012), "%%WPERSON-F-FWRITE wps_file unable to write to %s"},
	{WERRCODE(83014), "%%WPERSON-F-FORMS Line too long"},
	{WERRCODE(83016), "%%WPERSON-F-LPMAP Line too long"},
	{WERRCODE(83018), "%%WPERSON-E-OPTIONS %s: [%s]"},
	{WERRCODE(83019), "%%WPERSON-W-OPTIONS %s: [%s]"},
	{WERRCODE(83020), "%%WPERSON-E-FOPEN %s unable to open %s [errno=%d]"},
	{WERRCODE(83022), "%%WPERSON-E-FWRITE %s unable to write to %s [errno=%d]"},
	{WERRCODE(83024), "%%WPERSON-F-LOAD %s Line too long"},
	{WERRCODE(83026), "%%WPERSON-W-LOAD %s Found invalid FAC character [%x] - Ignored"},
	{WERRCODE(83028), "%%WPERSON-F-LOAD %s Line too short"},
	{WERRCODE(83030), "%%WPERSON-F-LOAD %s Printer number 000 not allowed."},
	{WERRCODE(83100), "%%LGMAP-E-ERROR"},
	{WERRCODE(83510), "%%WIN32PRT-E-FORMS form %d bad value for '%s': '%s'"},
	{WERRCODE(83512), "%%WIN32PRT-E-FORMS form %d bad keyword '%s'"},
	{WERRCODE(83514), "%%WIN32PRT-E-FORMS form %d bad syntax near position %d"},
	{WERRCODE(83516), "%%WIN32PRT-E-SYS System error: %s"},
	{WERRCODE(85002), "%%WSCREEN-F-SCRVERSION Screen version mismatch %d [Current=%d]"},
	{WERRCODE(85004), "%%WSCREEN-F-LEVELNOTFND Expecting [L] found [%5.5s...]"},
	{WERRCODE(85006), "%%WSCREEN-F-BADCONTROL Invalid control characters found [%5.5s...]"},
	{WERRCODE(85008), "%%WSCREEN-F-BADPIC Expecting [P{...}] found [%9.9s...]"},
	{WERRCODE(85010), "%%WSCREEN-F-MAXITEMS Too many screen items %d MAX=%d"},
	{WERRCODE(85502), "%%WSFNM-E-INVPARM Invalid %s call, check parameters. Passed %d params"},
	{WERRCODE(85504), "%%WSFNM-E-INSUFPARM %s call. Insufficient parameters specified. Only %d params"},
	{WERRCODE(85506), "%%WSFNM-E-INVCRSCOL %s call. Invalid cursor column (%d)"},
	{WERRCODE(85508), "%%WSFNM-E-INVCRSROW %s call. Invalid cursor row (%d)"},
	{WERRCODE(85702), "%%WSFNS-E-INVPARM Invalid %s call, check parameters. Passed %d params"},
	{WERRCODE(85704), "%%WSFNS-E-INSUFPARM %s call. Insufficient parameters specified. Passed %d params"},
	{WERRCODE(85706), "%%WSFNS-E-INVCRSCOL %s call. Invalid cursor column (%d)"},
	{WERRCODE(85708), "%%WSFNS-E-INVCRSCOL %s call. Invalid cursor row (%d)"},
	{WERRCODE(85710), "%%WSFNS-F-MALLOC Malloc of %d bytes failed for save screen"},
	{WERRCODE(86002), "%%WSHELP-E-PRNTING Error (%d) when printing screen"},
	{WERRCODE(88002), "%%WSXIO-E-NOTSUPP Unsupported XIO operation [%d]"},
	{WERRCODE(88004), "%%WSXIO-E-NOTSUPP Unsupported function [%c]"},
	{WERRCODE(88006), "%%WSXIO-E-BADLEN Invalid mapping area length [%d]"},
	{WERRCODE(88008), "%%WSXIO-E-NOTSUPP Feature not supported [%s]"},
	{WERRCODE(91002), "%%MANAGEFILES-F-RING [%s] [%s]"},
	{0,""}
};

static const char* werrcode_lookup(uint4 err)
{
	int i;

	for(i=0; wispmsg_table[i].errcode != 0; i++)
	{
		if (err == wispmsg_table[i].errcode)
		{
			return wispmsg_table[i].mess;
		}
	}

	return NULL; /* NOT FOUND */
}


int wisp_unlink(const char *filename)
{
	int rc;
	rc = unlink(filename);
	if (rc == 0)
	{
		WL_wtrace("REMOVE","OK","unlink(%s) succeeded", filename);
	}
	else if (errno != ENOENT)
	{
		WL_wtrace("REMOVE","FAILED","unlink(%s) failed errno=[%d]", filename, errno);
	}
	return rc;
}

/*
**	History:
**	$Log: werrlog.c,v $
**	Revision 1.46  2010/01/09 23:56:49  gsl
**	use strerror() instead of sys_errlist
**	
**	Revision 1.45  2009/10/18 20:55:32  gsl
**	fix windows warnings
**	
**	Revision 1.44  2003/04/07 16:00:14  gsl
**	Remove OLD logic
**	
**	Revision 1.43  2003/04/04 15:32:38  gsl
**	Remove old errlog flags stuff
**	Fix trace for unlink()
**	
**	Revision 1.42  2003/03/20 22:24:09  gsl
**	Start work on DATE6
**	
**	Revision 1.41  2003/03/20 18:28:04  gsl
**	LGMAP error
**	
**	Revision 1.40  2003/02/13 17:21:30  gsl
**	Add WISP version and platform to errors
**	
**	Revision 1.39  2003/02/13 16:12:42  gsl
**	Fix to display errors (even codes) with tracing
**	
**	Revision 1.38  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.37  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.36  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.35  2003/01/28 19:59:29  gsl
**	fix warnings
**	
**	Revision 1.34  2003/01/16 19:41:01  gsl
**	Add SCRATCH code
**	
**	Revision 1.33  2002/12/11 17:03:09  gsl
**	use wisp_unlink()
**	
**	Revision 1.32  2002/12/11 15:16:33  gsl
**	Add errno message string to all errors and trace messages that cantain "errno" string.
**	
**	Revision 1.31  2002/12/11 14:08:45  gsl
**	Removed wispmsg.dat/txt and makemsg
**	
**	Revision 1.30  2002/12/10 22:00:19  gsl
**	Use internal wispmsg_table[] instead of find_msg() and wispmsg.dat file for all error
**	message text.
**	
**	Revision 1.29  2002/12/10 20:54:08  gsl
**	use WERRCODE()
**	
**	Revision 1.28  2002/12/10 17:09:15  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.27  2002/12/06 22:56:42  gsl
**	add  WL_werrlog_error() and  WL_werrlog_warn()
**	remove globals
**	in find_msg() hardcode 101-104
**	
**	Revision 1.26  2002/12/04 15:59:37  gsl
**	Log changing WISPDEBUG mode
**	
**	Revision 1.25  2002/12/03 22:15:13  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.24  2002/10/01 19:17:34  gsl
**	fix warning
**	
**	Revision 1.23  2002/10/01 18:52:40  gsl
**	Add WL_strerror() a replacement for strerror()
**	
**	Revision 1.22  2002/07/10 21:05:29  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.21  2002/07/09 04:13:54  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.20  2002/07/01 04:02:42  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.19  2002/06/21 03:10:44  gsl
**	Remove VMS & MSDOS
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
