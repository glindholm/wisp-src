/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		win32spn.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	prototypes for win32spn routines
**
*/

#ifndef WIN32SPN_H
#define WIN32SPN_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#define SPN_HIDE_PARENT 1
#define SPN_HIDE_CHILD 2
#define SPN_WAIT_FOR_CHILD 4
#define SPN_DETACH_CHILD 8
#define SPN_NO_INHERIT 16

/*
**	Function Prototypes
*/
int win32spawnvp(char *sh_parm[], int Mode);
int win32spawnlp(char *cmd, char *args, int Mode);
void win32SetNewEnv(char *envstr);

#endif /* WIN32SPN_H */

/*
**	History:
**	$Log: win32spn.h,v $
**	Revision 1.2  1997-07-16 21:21:27-04  gsl
**	Add SPN_NO_INHERIT
**
**	Revision 1.1  1996-12-06 18:39:32-05  jockc
**	Initial revision
**
**
**
*/
