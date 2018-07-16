/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
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
**	Revision 1.4  1995-10-18 10:28:19-04  gsl
**	add headers and update prototypes
**
**
*/
