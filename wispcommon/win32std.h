/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		win32std.h
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	WIN32 standard defines
**
*/

#ifndef win32std_H
#define win32std_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	WIN32 does not have defines for some of the standard C lib routines
*/
#define chmod 		_chmod
#define getcwd		_getcwd
#define getpid		_getpid
#define unlink		_unlink
#define rmdir		_rmdir
#define close		_close
#define	read		_read
#define	open		_open
#define	strdup		_strdup
#define	spawnvp		_spawnvp
#define	putenv		_putenv
#define access		_access
#define	creat		_creat
#define write		_write
#define stat		_stat

#define mkdir(dir,mode) _mkdir(dir)

/*
**	Function Prototypes
*/

#endif /* win32std_H */

/*
**	History:
**	$Log: win32std.h,v $
**	Revision 1.2  1996-09-10 11:50:36-04  gsl
**	Fix mkdir() and stat()
**
**	Revision 1.1  1996-09-09 13:02:59-07  gsl
**	Initial revision
**
**
**
*/
