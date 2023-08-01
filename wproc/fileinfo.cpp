//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
//
// Module : fileinfo.cpp
// Author : George Soules
// Date   : 13 February 1991

// Specification
#include "fileinfo.hpp"

// Classes
#include "options.hpp"
#include "process.hpp"

// Definitions and subprograms
#include <ctype.h>
#if DOS || DOS_HOST
#include <dir.h>
#include <dos.h>
#include <io.h>
#endif
#if UNIX
#include <unistd.h>
#endif
#if WIN32 || UNIX
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef WIN32
#include <io.h>
#include <direct.h>
#endif
#include <string.h>
#include "debugaid.hpp"
#include "ext.hpp"
#include "memory.hpp"
#include "utility.hpp"
#if WANG
#include "wang_os.hpp"
#endif
#include "wisp_rts.h"

#ifdef WIN32
#define access(filename, mode)		_access(filename, mode)
#define getcwd(buffer, maxlen)		_getcwd(buffer, maxlen)
#endif


char *component(const char *a_path, int index, int len) {
  int i;
  
   if (index < 0)
      len = 0;
   char *text = new_string(len + 1);
   for (i = 0; i < len; i++)
      text[i] = a_path[index++];
   text[i] = '\0';
   return text;
}


char *file_dir(const char *a_path) {
   char *ext = file_suffix(a_path);
   char *name = file_name(a_path);
   int i = strlen(a_path) - strlen(ext) - strlen(name) - 1;
   delete_string(ext);
   delete_string(name);
   int len = 0;
   while (i >= 0) {
      if (a_path[i] == ':') {
         i++;
         len--;
         break;
      }
      else if (i == 0)
         break;
      i--;
      len++;
   }
   return component(a_path, i, ++len);
}


#if DOS || DOS_HOST
char *file_drive(const char *a_path) {
   int len = strlen(a_path) >= 2 ? 2 : 0;
   if (a_path[1] != ':')
      len = 0;
   return component(a_path, 0, len);
}
#endif


char *file_name(const char *a_path) {
   char *ext = file_suffix(a_path);
   int i = strlen(a_path) - strlen(ext) - 1;
   delete_string(ext);
   int len = 0;
   while (i >= 0) {
      if (a_path[i] == SLASH_CHAR || a_path[i] == ':') {
         i++;
         len--;
         break;
      }
      else if (i == 0)
         break;
      i--;
      len++;
   }
   return component(a_path, i, ++len);
}


char *file_suffix(const char *a_path) {
   int i   = strlen(a_path) - 1;
   int len = 0;
   while (i >= 0) {
      if (a_path[i] == '.')
         break;
      else if (a_path[i] == SLASH_CHAR || a_path[i] == ':' || i == 0) {
         len = -1;
         break;
      }
      i--;
      len++;
   }
   return component(a_path, i, ++len);
}


char* pcode_filename(const char *a_name) {
   char *name = file_name(a_name);
   char *new_name = new_string(strlen(name) + strlen(PCODE_FILE_EXT) + 1);
   strcpy(new_name, name);
   strcat(new_name, PCODE_FILE_EXT);
   delete_string(name);
   return new_name;
}


void get_dir(char *a_dir, const char *a_path) {
   char *dir = file_dir(a_path);
#if DOS || DOS_HOST
   char *drive = file_drive(a_path);
   strcpy(a_dir, drive);
   delete_string(drive);
   strcat(a_dir, dir);
#else
#if UNIX || WIN32
   strcpy(a_dir, dir);
#endif
#endif
   delete_string(dir);
}


void make_pathname(char *a_path, char* a_dir, char *a_file) {
   strcpy(a_path, a_dir);
   strcat(a_path, a_file);
}


file_ext file_extension(const char *a_path) {
   char *ext = file_suffix(a_path);
   file_ext the_ext;

   // Choose a default extension in case no match made below
#if WANG
   the_ext = ext_exe;
#else
   the_ext = ext_unknown;
#endif

   if (ext[0]) {
      if (same_string(ext, PCODE_FILE_EXT))
         the_ext = ext_pcode;
      else if (same_string(ext, SRC_FILE_EXT))
         the_ext = ext_src;
#if DOS
      else if (same_string(ext, COM_FILE_EXT))
         the_ext = ext_com;
      else if (same_string(ext, EXE_FILE_EXT))
         the_ext = ext_exe;
      else if (same_string(ext, BAT_FILE_EXT))
         the_ext = ext_bat;
#endif
   }
   delete_string(ext);
   return the_ext;
}


Boolean name_exists(char *a_path) {
   trace_ss(general, "Searching for ", a_path);
   if (access(a_path, 0) == 0) {
#if DOS || DOS_HOST
      int attrib = _chmod(a_path, 0);
      attrib = attrib & (FA_HIDDEN + FA_SYSTEM + FA_LABEL + FA_DIREC);
      if (! attrib)
         return true;
#else
#if UNIX || WIN32
      struct stat buf;
      if (stat(a_path, &buf) == 0)
         if (buf.st_mode & S_IFREG)
            return true;
#endif
#endif
   }
   return false;
}


Boolean find_name(char *a_name, char *a_path, Boolean has_extension) {
   Boolean found = false;
#if UNIX
   const int extensions = 2;
#else
   const int extensions = 5;
#endif
   int   len = strlen(a_path);
   char *extension[extensions] = {
#if RUNTIME
      PCODE_FILE_EXT,
#if ! UNIX
      COM_FILE_EXT,
      EXE_FILE_EXT,
      BAT_FILE_EXT,
#endif
      SRC_FILE_EXT

#else
      SRC_FILE_EXT,
#if UNIX
      PCODE_FILE_EXT
#else
      PCODE_FILE_EXT,
      COM_FILE_EXT,
      EXE_FILE_EXT,
      BAT_FILE_EXT
#endif
#endif
   };

   char full_name[MAXPATH];
   strcpy(full_name, a_path);

   // Try for exact match
   if (name_exists(full_name))
      found = true;

   if (! has_extension) {
      // Try each extension
      for (int i = 0; i < extensions && ! found; i++) {
         strcat(full_name, extension[i]);
         if (name_exists(full_name))
            found = true;
         else
            full_name[len] = '\0';
      }
   }

   if (found)
      strcpy(a_name, full_name);

   return found;
}


char *find_valid_input_file(char *a_name) {
   // Search directories as follows: procedure dir, current dir, path dirs.
   // Within each dir look for: exact match, SRC, PCODE, COM, EXE, BAT.
   // Ignore hidden and system files, volume labels and directory names.

   char  path[MAXPATH];
   char  dir[MAXPATH];
   char  name[MAXPATH];
   char *temp;

   temp = file_dir(a_name);
   Boolean is_qualified = BOOLEAN(temp[0] != '\0');
   delete_string(temp);

   temp = file_suffix(a_name);
   Boolean has_extension = BOOLEAN(temp[0] != '\0');
   delete_string(temp);

#if DOS
   temp = file_drive(a_name);
   Boolean has_drive_letter = BOOLEAN(temp[0] != '\0');
   delete_string(temp);

   if (is_qualified) {
      if (has_drive_letter)
         make_pathname(path, "", a_name);
      else {
         if (a_name[0] == SLASH_CHAR) {
            char drive[3] = " :";
            drive[0] = (char) getdisk() + 'A';
            make_pathname(path, drive, a_name);
         }
         else {
            getcwd(dir, MAXPATH);
            if (dir[strlen(dir) - 1] != SLASH_CHAR)
               strcat(dir, SLASH_STRING);
            make_pathname(path, dir, a_name);
         }
      }
   }
#elif WANG
   if (the_process->nesting_level > 1) {
      // Since nesting level > 1, file name must be from a RUN statement.

      char *ext = file_suffix(a_name);
      Boolean has_suffix = BOOLEAN(ext[0] != 0);
      delete_string(ext);

      if (has_suffix)
         ; // Fall through and treat as native file name
      else {
         int file_kind;
         char *native_name = wang_to_native_file_name(a_name, file_kind);
         trace_ss(general, "Wang name maps to ", native_name);
         trace_si(general, "Kind # = ", file_kind);

         // Set PROGLIB and PROGVOL to the lib and vol of the child about
         // to be run.  This is not the right place to do this because this
         // code isn't supposed to now what is happening with respect to
         // things getting run; however, it's the last place that the lib
         // and vol info is still known so...
         wang_filename name(a_name);
         if (*(name.libname) != '\0' && *(name.libname) != ' ')
	 {
	    wang_os_set_alpha("PL", name.libname);
	 }
         if (*(name.volname) != '\0' && *(name.volname) != ' ')
	 {
            wang_os_set_alpha("PV", name.volname);
	 }

         // File kinds: 0 = Not found; 1 = procedure; 2 = not procedure
         if (file_kind == 1)
            return native_name;
         else {
            delete_string(native_name);
            return file_kind == 0 ? (char*)NULL : strip(a_name);
         }
      }
   }

   if (is_qualified) {
 #ifdef UNIX
      if (a_name[0] != SLASH_CHAR) {
         getcwd(dir, MAXPATH);
         if (dir[strlen(dir) - 1] != SLASH_CHAR)
            strcat(dir, SLASH_STRING);
         make_pathname(path, dir, a_name);
      }
      else
         make_pathname(path, "", a_name);
#elif WIN32
      make_pathname(path, "", a_name);
#else
      path = "";
      assert(0);
#endif
   }
#else
   path = "";
   assert(0);
#endif

   if (is_qualified) {
      // Try extensions, but don't search directories
      if (find_name(name, path, has_extension))
         return strip(name);
      else
         return NULL;
   }

   if (the_process->the_parent_pathname) {
      // Look in the directory of the parent procedure
      get_dir(dir, the_process->the_parent_pathname);
      make_pathname(path, dir, a_name);
      if (find_name(name, path, has_extension))
         return strip(name);
   }

   // Now try the current directory
   getcwd(dir, MAXPATH);
   if (dir[strlen(dir) - 1] != SLASH_CHAR)
      strcat(dir, SLASH_STRING);
   make_pathname(path, dir, a_name);
   if (find_name(name, path, has_extension))
      return strip(name);

   // Finally, try each directory on the path

   const char *envpath = wispenvpath();
   int   i = 0;
   char  c = envpath[i];

   while (c) {
      int j = 0;
      while (c) {
         if (c == PATH_DELIMETER_CHAR) {
            while (c == PATH_DELIMETER_CHAR)
               c = envpath[++i];
            break;
         }
         dir[j++] = c;
         c = envpath[++i];
      }
      if (dir[j - 1] != SLASH_CHAR)
         dir[j++] = SLASH_CHAR;
      dir[j] = '\0';

      make_pathname(path, dir, a_name);
      if (find_name(name, path, has_extension))
         return strip(name);
   }
   return NULL;
}

//
//	History:
//	$Log: fileinfo.cpp,v $
//	Revision 1.16  2011/10/29 20:09:14  gsl
//	Fix ISO routine name warnins on WIN32
//	
//	Revision 1.15  2003/06/11 19:08:46  gsl
//	Fixed so only set PV/PL before a run if not blank
//	
//	Revision 1.14  2002/07/25 17:03:43  gsl
//	MSFS->WIN32
//	
//	Revision 1.13  1998/08/31 19:50:33  gsl
//	drcs update
//	
//	Revision 1.12  1998-08-31 15:13:49-04  gsl
//	drcs update
//
//

//	
//
//	Working file: fileinfo.cpp
//	head: 1.11
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
//	total revisions: 11;	selected revisions: 11
//	description:
//	----------------------------
//	revision 1.11
//	date: 1997-10-02 08:44:49-04;  author: gsl;  state: V4_3_00;  lines: +1 -2
//	fix warnings
//	----------------------------
//	revision 1.10
//	date: 1997-10-01 09:26:27-04;  author: gsl;  state: Exp;  lines: +2 -2
//	fix warnings
//	----------------------------
//	revision 1.9
//	date: 1996-10-09 19:36:56-04;  author: gsl;  state: V4_1_02;  lines: +2 -1
//	Add include wisp_rts.h
//	----------------------------
//	revision 1.8
//	date: 1996-10-09 12:41:14-04;  author: gsl;  state: Exp;  lines: +2 -2
//	replace getenv() with wispenvpath()
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 19:46:16-04;  author: gsl;  state: Exp;  lines: +25 -13
//	NT, and fix command line filename handling
//	----------------------------
//	revision 1.6
//	date: 1996-07-25 14:15:11-04;  author: gsl;  state: Exp;  lines: +1 -1
//	Renamed from fileinfo.cc to fileinfo.cpp
//	----------------------------
//	revision 1.5
//	date: 1996-04-18 12:59:42-04;  author: jockc;  state: Exp;  lines: +7 -1
//	moved declaration of for loop index from for loop to function
//	auto decl area
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:57-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:15-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:53-05;  author: gsl;  state: V3_3x12;  lines: +7 -52
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:09-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
