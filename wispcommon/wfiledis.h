/* 
	Copyright (c) 1998 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
**	File:		wfiledis.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	DISPLAY a file
**
*/

#ifndef wfiledis_H
#define wfiledis_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void wfile_disp(void);
int display_util_getparms(char *filename);
int utils_in_windows(void);
int use_internal_display(void);
int link_display(const char* filepath);
int internal_display(const char* filepath);
const char* custom_display_utility(void);

#endif /* wfiledis_H */

/*
**	History:
**	$Log: wfiledis.h,v $
**	Revision 1.4  1999/02/23 21:57:26  gsl
**	Move no_windows() to wispcfg.h
**	
**	Revision 1.3  1999-02-23 15:24:40-05  gsl
**	Add no_windows() routine.
**
**	Revision 1.2  1998-05-05 13:34:27-04  gsl
**	Add prototypes for new display frontend routines
**
**	Revision 1.1  1998-04-29 15:31:11-04  gsl
**	Initial revision
**
**
**
*/
