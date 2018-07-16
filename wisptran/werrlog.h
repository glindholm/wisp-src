			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/* 				werrlog.h, common error logger for WISP system.							*/

#ifdef INIT_ERR
#define INIT_ITEM_CLEAR		= 0
#define INIT_ITEM_SET		= 1

#	ifdef unix
#	define DEFAULT_LOG_MODE	= 11						/* Default logging flags for Unix.		*/
#	else
#	define DEFAULT_LOG_MODE	= 13						/* Default logginf for others.			*/
#	endif

#else
#define INIT_ERR extern
#define INIT_ITEM_CLEAR
#define INIT_ITEM_SET
#define DEFAULT_LOG_MODE
#endif

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

INIT_ERR unsigned long 	w_err_flag	DEFAULT_LOG_MODE;
INIT_ERR unsigned long	w_err_logged	INIT_ITEM_CLEAR;			/* Indicate no logs made to log file.		*/
INIT_ERR unsigned long	w_err_code	INIT_ITEM_CLEAR;			/* Last error code reported.			*/
INIT_ERR char 		*w_err_p1	INIT_ITEM_CLEAR;			/* Last parm set.				*/
INIT_ERR char 		*w_err_p2	INIT_ITEM_CLEAR;
INIT_ERR char 		*w_err_p3	INIT_ITEM_CLEAR;
INIT_ERR char 		*w_err_p4	INIT_ITEM_CLEAR;
INIT_ERR char 		*w_err_p5	INIT_ITEM_CLEAR;
INIT_ERR char 		*w_err_p6	INIT_ITEM_CLEAR;
INIT_ERR char 		*w_err_p7	INIT_ITEM_CLEAR;
INIT_ERR char 		*w_err_p8	INIT_ITEM_CLEAR;

#define ERRORCODE(x)	((long)(ROUTINE + x))					/* The macro for reporting errors.		*/

#include <errno.h>

