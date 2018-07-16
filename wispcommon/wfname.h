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
**	File:		wfname.h
**
**	Purpose:	Header for wfname.c
**
**
*/

#ifndef WFNAME_H
#define WFNAME_H

#include "intdef.h"
#include "wdefines.h"
#include "wcommon.h"

char *WFNAME2(char *attrstr, char *vol, char *lib, char *file, char *native_path);
char *WFNAME(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);

char *WL_wfname_backfill(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);
char *WL_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);

char *WL_wanglib2path(char *p_vol, char *p_lib, char *native_path);

int WL_wlgtrans( char *in_str, char *out_str );
int WL_wfexists(char *file, char *lib, char *vol);

void wisp_fileattr2mode(const char *attrstr, int4 *mode);
void wisp_mode2fileattr(int4 mode, char *attrstr);

void WFOPEN4(
	     char *attrstr,				/* File attributes (10 chars)		*/
	     char *vol,					/* the WANG volume name	(6 chars)	*/
	     char *lib,					/* The WANG library name (8 chars)	*/
	     char *file,				/* The file name	(8 chars)	*/
	     char *path,				/* The resultant name			*/
	     const char *appl,				/* The COBOL program id (8 chars)	*/
	     const char *prname,			/* The PRNAME (optional).		*/
	     const int4 *openmode);			/* The open mode			*/

void WFOPEN3(						/* WISP 3.0 and later			*/
	     int4 *mode,				/* the mode of opening			*/
	     char *vol,					/* the WANG volume name	(6 chars)	*/
	     char *lib,					/* The WANG library name (8 chars)	*/
	     char *file,				/* The file name	(8 chars)	*/
	     char *name,				/* The resultant name			*/
	     const char *appl,				/* The COBOL program id (8 chars)	*/
	     const char *prname,			/* The PRNAME 				*/
	     const int4 *openmode);			/* The open mode			*/

void WFOPEN2(						/* WISP 2.0C and later			*/
	int4 *mode,					/* the mode of opening			*/
	char *vol,					/* the WANG volume name	(6 chars)	*/
	char *lib,					/* The WANG library name (8 chars)	*/
	char *file,					/* The file name	(8 chars)	*/
	char *cob_name,					/* The resultant name			*/
	const char *appl,				/* The COBOL program id (8 chars)	*/
	const char *prname);				/* The PRNAME 				*/

void WFOPEN(						/* WISP 2.0B and earlier		*/
	int4 *mode,					/* the mode of opening			*/
	char *vol,					/* the WANG volume name	(6 chars)	*/
	char *lib,					/* The WANG library name (8 chars)	*/
	char *file,					/* The file name	(8 chars)	*/
	char *name,					/* The resultant name			*/
	const char *prname);				/* The PRNAME (optional).		*/

void WFILECHK3(
	const char file_operation[COB_FILEOP_SIZE],
	const char file_status[2],
	const char file_status_extended[COB_EXTENDED_FILE_STATUS_SIZE],
	char file_attributes[WISP_FILE_ATTR_SIZE],
	const char file_vol[6],
	const char file_lib[8],
	const char file_fil[8],
	const char cob_filepath[COB_FILEPATH_LEN],
	const char select_name[COB_SELECT_NAME_SIZE],
	const char app_name[40],
	char skip_declaratives[1],
	const char has_declaratives[1]);

void WFILECHK2(char   decl_stat[2], 
	       const char file_stat[2],
	       const char x_stat1[10],
	       const char x_stat2[10],
	       int4* sflag,
	       const char vol[6],
	       const char lib[8],
	       const char fil[8],
	       const char fname[COB_FILEPATH_LEN],
	       const char f_id[40],
	       const char appl[8]);

void WFILECHK( char   decl_stat[2], 
	       const char   file_stat[2],
	       const char*  x_stat1,
	       const char*  x_stat2,
	       int4* sflag,
	       const char   vol[6],
	       const char   lib[8],
	       const char   fil[8],
	       const char   fname[COB_FILEPATH_LEN],
	       const char   f_id[40],
	       const char   appl[8]);

#endif /* WFNAME_H */
/*
**	History:
**	$Log: wfname.h,v $
**	Revision 1.19  2003/04/03 20:27:53  gsl
**	WFNAME2()
**	
**	Revision 1.18  2003/03/17 17:21:28  gsl
**	Change to use  WFOPEN4
**	
**	Revision 1.17  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/07/31 21:00:28  gsl
**	globals
**	
**	Revision 1.15  2002/07/29 14:47:21  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.14  2002/07/23 20:49:50  gsl
**	globals
**	
**	Revision 1.13  2002/07/18 13:19:28  gsl
**	fix mode unit4 -> int4
**	
**	Revision 1.12  2002/07/12 19:10:24  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/07/10 21:06:35  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.10  2002/07/02 04:05:05  gsl
**	add wfilechk2() proto
**	
**	Revision 1.9  2002/07/01 04:02:45  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.8  2002/06/28 04:03:00  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.7  2002/06/27 04:12:42  gsl
**	Clean up status/mode bits
**	
**	Revision 1.6  1996/07/23 18:17:57  gsl
**	drcs update
**	
**
**
*/
