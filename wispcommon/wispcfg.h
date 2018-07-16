/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

/*
**	File:		wispcfg.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	WISP configuration routines
**
*/

#ifndef wispcfg_H
#define wispcfg_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/
#define DEF_WISPSERVER	"(LOCAL)"
#define WISPCONFIG_UNSET_VALUE	"$WISPCONFIG"

/*
**	Function Prototypes
*/
const char* wisphomedir(char *dir);
const char* wispconfigdir(void);
const char* wispserver(void);
const char* wispenvpath(void);
const char* wisplinkpath(void);
const char* wispdefdrive(void);
const char* wisptmpbasedir(char *dir);
const char* WL_systmpdir(char *dir);
int wispsortmemk(void);
const char* wispcpu(void);
const char* wispnetid(void);
const char* WL_weditorexe(void);
const char* WL_wprocexe(void);
const char* WL_wprocflags(void);
char* wispshellexe(void);
const char* WL_acpconfigdir(void);
const char* WL_acpmapfile(void);
const char* wispscratchmode(void);
int wispdisplay8bit(void);
const char* wispmenudir(void);
const char *wispterm(char *the_term);
const char *wisptermdir(char *the_dir);
const char* wisptermfilepath(char *the_path);
#ifdef WIN32
const char* wispmsgsharedir(char *the_dir);
#endif
const char* WL_acu_vutil_exe(void);
const char* wispdir(void);
int WL_no_windows(void);
int wisptelnet(void);
const char* wisprcfilepath(void);

#endif /* wispcfg_H */

/*
**	History:
**	$Log: wispcfg.h,v $
**	Revision 1.16  2003/02/14 16:12:07  gsl
**	Define a value for testing is $WISPCONFIG is not set
**	
**	Revision 1.15  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.14  2002/12/04 20:52:18  gsl
**	Add to OPTIONS file
**	WPROC
**	WPROCDEBUG
**	ACPCONFIG
**	ACPMAP
**	WISP_SCRATCH_MODE/WISPSCRATCHMODE
**	WISP_DISPLAY_8BIT/DISPLAY8BIT/WISPDISPLAY8BIT
**	WISPSYSADMIN
**	
**	Revision 1.13  2002/07/10 21:06:35  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  2002/07/09 04:14:02  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.11  2001/10/31 20:27:39  gsl
**	Add wisprcfilepath()
**	
**	Revision 1.10  1999-02-24 13:22:37-05  gsl
**	Add wisptelnet()
**
**	Revision 1.9  1999-02-23 16:57:00-05  gsl
**	Moved no_windows() to wispcfg.h
**
**	Revision 1.8  1997-12-04 15:18:22-05  gsl
**	add wisplinkpath()
**
**	Revision 1.7  1997-03-13 16:48:52-05  gsl
**	Add wispdir()
**
**	Revision 1.6  1997-02-28 16:40:20-05  gsl
**	Add wispserver()
**
**	Revision 1.5  1996-12-11 19:48:17-05  gsl
**	Add acu_vutil_exe()
**
**	Revision 1.4  1996-11-11 09:06:15-08  jockc
**	added proto for wispmsgsharedir
**
**	Revision 1.3  1996-10-11 17:24:10-07  gsl
**	Add prototypes
**
**	Revision 1.2  1996-10-08 17:29:39-07  gsl
**	add prototypes
**
**	Revision 1.1  1996-10-08 10:06:10-07  gsl
**	Initial revision
**
**
**
*/
