//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : fileinfo.hpp
// Author : George Soules
// Date   : 13 February 1991

#ifndef FILEINFO__HPP
#define FILEINFO__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"


#if DOS || DOS_HOST ||WIN32
#define SLASH_CHAR           '\\'
#define SLASH_STRING         "\\"
#define PATH_DELIMETER_CHAR  ';'
#define ALL_FILES_WILDCARD   "\\*.*"
#ifndef MAXPATH
#define MAXPATH 256
#endif
#endif

#if UNIX
#define SLASH_CHAR           '/'
#define SLASH_STRING         "/"
#define PATH_DELIMETER_CHAR  ':'
#define ALL_FILES_WILDCARD   "/*"
#ifdef MAXPATH
#undef MAXPATH
#endif
#define MAXPATH 256
#endif /* UNIX */

enum file_ext {
   ext_pcode,
   ext_src,
   ext_com,
   ext_exe,
   ext_bat,
   ext_unknown
};

char *file_dir(const char *a_path);

#if DOS || DOS_HOST
char *file_drive(const char *a_path);
#endif

file_ext file_extension(const char *a_path);

char *file_name(const char *a_path);

char *file_suffix(const char *a_path);

char *find_valid_input_file(char *a_name);

void get_dir(char *a_dir, const char *a_path);

char *pcode_filename(const char *a_path);

#endif


//
//	History:
//	$Log: fileinfo.hpp,v $
//	Revision 1.7  2002/07/25 17:03:43  gsl
//	MSFS->WIN32
//	
//	Revision 1.6  1998/08/31 19:13:49  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/fileinfo.hpp,v
//	Working file: fileinfo.hpp
//	head: 1.5
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 19:46:41-04;  author: gsl;  state: V4_3_00;  lines: +7 -4
//	NT
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:58-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:16-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:54-05;  author: gsl;  state: V3_3x12;  lines: +0 -10
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:09-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
