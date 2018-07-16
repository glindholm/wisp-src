// Copyright (c) Lexical Software, 1991.  All rights reserved.
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


#if DOS || DOS_HOST ||MSFS
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

