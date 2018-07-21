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
**	File:		win32spn.h
**
**	Project:	WISPLIB
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
int  WL_win32spawnvp(const char *sh_parm[], int Mode);
int  WL_win32spawnlp(const char *cmd, const char *args, int Mode);
void WL_win32SetNewEnv(char *envstr);

#endif /* WIN32SPN_H */

/*
**	History:
**	$Log: win32spn.h,v $
**	Revision 1.8  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.7  2002/12/04 20:52:16  gsl
**	Add to OPTIONS file
**	WPROC
**	WPROCDEBUG
**	ACPCONFIG
**	ACPMAP
**	WISP_SCRATCH_MODE/WISPSCRATCHMODE
**	WISP_DISPLAY_8BIT/DISPLAY8BIT/WISPDISPLAY8BIT
**	WISPSYSADMIN
**	
**	Revision 1.6  2002/07/10 21:05:34  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.5  1998/05/05 21:36:22  gsl
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
