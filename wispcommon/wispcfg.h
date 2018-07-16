/* 
	Copyright (c) 1996-1997 NeoMedia Migrations, All rights reserved.
	$Id:$
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
const char* wtmpdir(char *dir);
int wispsortmemk(void);
const char* wispcpu(void);
const char* wispnetid(void);
char* weditorexe(void);
char* wprocexe(void);
char* wprocflags(void);
char* wispshellexe(void);
const char* acpconfigdir(void);
const char* acpmapfile(void);
const char* wispscratchmode(void);
int wispdisplay8bit(void);
const char* wispmenudir(void);
const char *wispterm(char *the_term);
const char *wisptermdir(char *the_dir);
const char* wisptermfilepath(char *the_path);
#ifdef WIN32
const char* wispmsgsharedir(char *the_dir);
#endif
const char* acu_vutil_exe(void);
const char* wispdir(void);
int no_windows(void);
int wisptelnet(void);
const char* wisprcfilepath(void);

#endif /* wispcfg_H */

/*
**	History:
**	$Log: wispcfg.h,v $
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
