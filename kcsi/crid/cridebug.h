/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

extern int crid_debug_level,crid_trace_level;

#define	TRACE(x)	if(crid_trace_level) crid_str_trace(x)
#define	TRACE_IN(x)	if(crid_trace_level) crid_func_trace_in(x)
#define	TRACE_OUT(x)	if(crid_trace_level) crid_func_trace_out(x)

/*
**	History:
**	$Log: cridebug.h,v $
**	Revision 1.3  1996-09-17 19:34:04-04  gsl
**	drcs update
**
**
**
*/
