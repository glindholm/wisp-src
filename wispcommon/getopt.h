/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		getopt.h
**
**	Project:	wisp/common
**
**	RCS:		$Source:$
**
**	Purpose:	command line options
**
*/

#ifndef getopt_H
#define getopt_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int	getopt( int argc, char *const argv[], const char *flags);


#endif /* getopt_H */

/*
**	History:
**	$Log: getopt.h,v $
**	Revision 1.2  1997-02-17 10:49:46-05  gsl
**	Fixed prototype so same as ansi standard
**
**	Revision 1.1  1996-06-21 12:26:29-04  gsl
**	Initial revision
**
**
**
*/
