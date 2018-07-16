/*
**	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
**
**	Project:	WPROC
**	Id:		$Id:$
**	RCS:		$Source:$
**	
*/

/*
**	File:		miscsubs.h
**
**	Project:	wproc
**
**	RCS:		$Source:$
**
**	Purpose:	C++ prototypes for miscsubs C routines
**
*/

#ifndef miscsubs_H
#define miscsubs_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
extern "C" const char *globaldata();
extern "C" void delete_globaldata();
extern "C" void tempproc(char *file, char *lib, char *vol, char *filepath);

#endif /* miscsubs_H */

/*
**	History:
**	$Log: miscsubs.h,v $
**	Revision 1.6  1998-08-31 15:50:35-04  gsl
**	drcs update
**
**	Revision 1.5  1998-08-31 15:13:57-04  gsl
**	drcs update
**
**	Revision 1.4  1995-10-18 10:28:19-04  gsl
**	add headers and update prototypes
**
**	----------------------------
**	revision 1.3
**	date: 1995-04-25 06:00:05-04;  author: gsl;  state: V3_3_18;  lines: +0 -0
**	drcs state V3_3_15
**	----------------------------
**	revision 1.2
**	date: 1995-04-17 07:52:22-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
**	drcs state V3_3_14
**	----------------------------
**	revision 1.1
**	date: 1995-01-27 18:33:02-05;  author: gsl;  state: V3_3x12;
**	drcs load
**	=============================================================================
*/
