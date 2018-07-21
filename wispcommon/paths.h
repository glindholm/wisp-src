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

#ifdef WIN32
#define PATH_SEPARATOR	';'
#define DIR_SEPARATOR	'\\'
#define DIR_SEPARATOR_STR "\\"
#endif

#define link_path_seg	WL_link_path_seg
#define env_path_seg	WL_env_path_seg
#define osd_ext		WL_osd_ext
#define whichenvpath	WL_whichenvpath
#define whichlinkpath	WL_whichlinkpath

char *WL_link_path_seg(int pathnum);
char *WL_env_path_seg(int pathnum);
char *WL_osd_ext(char *filepath);
int   WL_whichenvpath(char *filename, char *fullpath);
int   WL_whichlinkpath(char *filename, char *fullpath);

#endif /* PATHS_H */
/*
**	History:
**	$Log: paths.h,v $
**	Revision 1.14  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/25 17:03:41  gsl
**	MSFS->WIN32
**	
**	Revision 1.12  2002/07/11 14:34:00  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.11  2002/07/09 04:14:04  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.10  1997/12/04 22:35:46  gsl
**	added whichlinkpath()
**	
**	Revision 1.9  1997-12-04 15:38:35-05  gsl
**	Split osd_path() into WL_link_path_seg() and WL_env_path_seg()
**
**	Revision 1.8  1996-07-23 14:17:49-04  gsl
**	drcs update
**
**
**
*/
