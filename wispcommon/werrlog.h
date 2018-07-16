/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*		WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		WERRLOG.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef WERRLOG_H
#define WERRLOG_H

#include "idsistd.h"

/*				werrlog.h, common error logger for WISP system.							*/

#ifdef INIT_ERR
#define INIT_ITEM_CLEAR		= 0
#define INIT_ITEM_SET		= 1

#define DEFAULT_LOG_MODE	= 15

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

INIT_ERR uint4		w_err_flag	DEFAULT_LOG_MODE;
INIT_ERR uint4		w_err_logged	INIT_ITEM_CLEAR;			/* Indicate no logs made to log file.		*/
INIT_ERR uint4		w_err_code	INIT_ITEM_CLEAR;			/* Last error code reported.			*/

#define ERRORCODE(x)	((int4)(ROUTINE + x))					/* The macro for reporting errors.		*/

#include <errno.h>

void werr_message_box(char *instr);
void werrlog(uint4 id, ...);
void werrset(void);

void werr_write(const char* buff);								/* Write out a buffer to the error log.	*/

void wtrace(const char* routine, const char* code, const char* format, ... /* args */);
int wtracing(void);
void wtrace_timestamp(const char *routine);
const char* WL_strerror(int errnum);

#endif /* WERRLOG_H */
/*
**	History:
**	$Log: werrlog.h,v $
**	Revision 1.14.2.2  2002/11/12 16:00:18  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.14.2.1  2002/10/09 19:48:08  gsl
**	Added WL_strerror()
**	
**	Revision 1.14  1998/05/12 14:54:02  gsl
**	Add wtrace_timestamp() and wtracing()
**	
**	Revision 1.13  1997-04-15 16:23:25-04  gsl
**	Add wtrace()
**
**	Revision 1.12  1996-07-23 14:17:55-04  gsl
**	drcs update
**
**
**
*/
