/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*
**	File:		werrlog.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Error logging and runtime tracing
**
*/

#ifndef WERRLOG_H
#define WERRLOG_H

#include "idsistd.h"

#define WISPDEBUG_NONE		0
#define WISPDEBUG_ERRORS	1
#define WISPDEBUG_FULL		2

void WL_set_wispdebug(int mode);
int  WL_get_wispdebug(void);

void  wisp_set_werrlog_flag(uint4 i);

#define WERRCODE(x)	((int4)(x))			/* Source code marker for error codes		*/

#include <errno.h>

#define werrlog	WL_werrlog
void WL_werrlog(uint4 id, ...);
void WL_werrlog_error(int errcode, const char* routine, const char* code, const char* format, ... /* args */);
void WL_werrlog_warn(int errcode, const char* routine, const char* code, const char* format, ... /* args */);
void WL_werr_message_box(const char *instr);
void WL_werr_override(void);
void WL_werr_write(const char* buff);							/* Write out a buffer to the error log.	*/

#define wtrace WL_wtrace
void WL_wtrace(const char* routine, const char* code, const char* format, ... /* args */);
void WL_wtrace_entry(const char* routine);
int  WL_wtracing(void);
void WL_wtrace_timestamp(const char *routine);
const char* WL_strerror(int errnum);

#endif /* WERRLOG_H */
/*
**	History:
**	$Log: werrlog.h,v $
**	Revision 1.24  2003/04/04 15:32:38  gsl
**	Remove old errlog flags stuff
**	Fix trace for unlink()
**	
**	Revision 1.23  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.22  2002/12/10 20:53:02  gsl
**	remove ERRORCODE() replaced by WERRCODE()
**	
**	Revision 1.21  2002/12/10 17:08:06  gsl
**	Add WL_wtrace_entry()
**	
**	Revision 1.20  2002/12/06 22:55:48  gsl
**	add  WL_werrlog_error() and  WL_werrlog_warn()
**	
**	Revision 1.19  2002/12/05 22:04:42  gsl
**	Add WERRCODE() macro as a marker
**	
**	Revision 1.18  2002/12/03 22:15:12  gsl
**	Replace the w_err_flag bitmask with wispdebug mode that can be set to "FULL"
**	"ERRORS" or "NONE" to simplify.
**	
**	Revision 1.17  2002/10/01 18:52:40  gsl
**	Add WL_strerror() a replacement for strerror()
**	
**	Revision 1.16  2002/07/10 21:06:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/07/01 04:02:45  gsl
**	Replaced globals with accessors & mutators
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
