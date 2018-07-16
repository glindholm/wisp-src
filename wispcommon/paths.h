/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		paths.h
**
**	Purpose:	Routines the operate on the PATH
**
*/

#ifndef PATHS_H
#define PATHS_H

#ifdef unix
#define PATH_SEPARATOR	':'
#define DIR_SEPARATOR	'/'
#define DIR_SEPARATOR_STR "/"
#endif

#ifdef MSFS
#define PATH_SEPARATOR	';'
#define DIR_SEPARATOR	'\\'
#define DIR_SEPARATOR_STR "\\"
#endif

char *link_path_seg(int pathnum);
char *env_path_seg(int pathnum);
char *osd_ext(char *filepath);
int whichenvpath(char *filename, char *fullpath);
int whichlinkpath(char *filename, char *fullpath);

#endif /* PATHS_H */
/*
**	History:
**	$Log: paths.h,v $
**	Revision 1.10  1997-12-04 17:35:46-05  gsl
**	added whichlinkpath()
**
**	Revision 1.9  1997-12-04 15:38:35-05  gsl
**	Split osd_path() into link_path_seg() and env_path_seg()
**
**	Revision 1.8  1996-07-23 14:17:49-04  gsl
**	drcs update
**
**
**
*/
