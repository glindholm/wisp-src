/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		osddefs.h
**
**	Project:	wisp
**
**	RCS:		$Source:$
**
**	Purpose:	Operating System Dependant defines
**
*/

#ifndef osddefs_H
#define osddefs_H

#ifdef unix
#define FUNC_UNAME
#endif

#ifdef MSFS
#define FOPEN_READ_BINARY	"rb"
#define FOPEN_WRITE_BINARY	"wb"
#define FOPEN_READ_TEXT		"r"
#define FOPEN_WRITE_TEXT	"w"
#else
#define FOPEN_READ_BINARY	"r"
#define FOPEN_WRITE_BINARY	"w"
#define FOPEN_READ_TEXT		"r"
#define FOPEN_WRITE_TEXT	"w"
#endif

#endif /* osddefs_H */

/*
**	History:
**	$Log: osddefs.h,v $
**	Revision 1.2  1996-07-11 19:20:55-04  gsl
**	Share code with msdos for NT
**
**	Revision 1.1  1995-09-25 10:06:29-07  gsl
**	Initial revision
**
**
*/
