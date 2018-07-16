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
#define SPN_HIDE_PARENT 	0x01
#define SPN_HIDE_CHILD 		0x02
#define SPN_WAIT_FOR_CHILD 	0x04
#define SPN_SUBMIT_CHILD 	0x08
#define SPN_NO_INHERIT 		0x10
#define SPN_CAPTURE_OUTPUT	0x20
#define SPN_STANDALONE_CHILD	0x40
#define SPN_HIDDEN_CMD		0x80

/*
**	Function Prototypes
*/
int win32spawnvp(char *sh_parm[], int Mode);
int win32spawnlp(const char *cmd, const char *args, int Mode);
void win32SetNewEnv(char *envstr);

#endif /* WIN32SPN_H */

/*
**	History:
**	$Log: win32spn.h,v $
**	Revision 1.5  1998-05-05 17:36:22-04  gsl
**	Add HIDDEN_CMD mode flag
**
**	Revision 1.4  1998-05-05 13:53:33-04  gsl
**	Reworked mode flags
**
**	Revision 1.3  1998-03-16 14:14:56-05  gsl
**	Make args const
**
**	Revision 1.2  1997-07-16 21:21:27-04  gsl
**	Add SPN_NO_INHERIT
**
**	Revision 1.1  1996-12-06 18:39:32-05  jockc
**	Initial revision
**
**
**
*/
