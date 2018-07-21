/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


/*
**	File:		getopt.h
**
**	Project:	wisp/common
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
**	Revision 1.3  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.2  1997/02/17 15:49:46  gsl
**	Fixed prototype so same as ansi standard
**	
**	Revision 1.1  1996-06-21 12:26:29-04  gsl
**	Initial revision
**
**
**
*/
