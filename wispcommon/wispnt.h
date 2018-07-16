/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		winnt.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Windows NT only routines
**
*/

#ifndef winnt_H
#define winnt_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void hsleep(long hundredths);

char *computername(char *cname);
#define DEF_COMPUTERNAME "Windows"

int handle_stdout(void);
int get_registry_value(const char *key, const char *value, int maxsize, char *result);
int get_remote_registry_value(const char* machine, const char *key, const char *value, int maxsize, char *result);
char *wgetreg(const char *key, const char *value);

int win32_nt(void);
int win32_95(void);


#endif /* winnt_H */

/*
**	History:
**	$Log: wispnt.h,v $
**	Revision 1.9  1997-12-04 15:58:49-05  gsl
**	Renamed from winnt.h to wispnt.h
**
**	Revision 1.8  1997-12-04 11:13:54-05  gsl
**	Move from wisp/lib to wisp/common
**
**	Revision 1.7  1997-08-23 10:49:31-04  gsl
**	Add win32_nt() and win32_95()
**
**	Revision 1.6  1997-03-20 16:08:42-05  gsl
**	Add get_remote_registry_value()
**
**	Revision 1.5  1997-02-28 16:08:35-05  gsl
**	Added a define for a default computer name
**
**	Revision 1.4  1996-11-18 15:51:29-05  gsl
**	Add registry routines
**
**	Revision 1.3  1996-08-21 16:22:40-07  gsl
**	add handle_stdout()
**
**	Revision 1.2  1996-08-19 14:52:05-07  gsl
**	Add computername()
**
**	Revision 1.1  1996-07-18 15:35:59-07  gsl
**	Initial revision
**
**
**
*/
