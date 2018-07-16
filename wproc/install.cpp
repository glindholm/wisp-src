// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : install.cpp
// Author : George Soules
// Date   : 26 February 1991

// Specification
#include "install.hpp"

// Classes
// (none)

// Definitions and subprograms
#if DOS || DOS_HOST
#include <dir.h>
#include <dos.h>
#include <io.h>
#endif
#if WANG && ! DOS_HOST
#include "crt_io.hpp"
#include "fileinfo.hpp"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <string.h>
#include "ext.hpp"
#include "memory.hpp"
#include "product.hpp"
#include "report.hpp"
#include "utility.hpp"
#include "wisp_rts.h"

char *installation_pathname(char *file, char *ext) {
   static char *path;
#if DOS || DOS_HOST
   char drive[MAXDRIVE];
   char dir[MAXDIR];
   fnsplit(_argv[0], drive, dir, NULL, NULL);
   path = new_string(strlen(drive) + strlen(dir) + strlen(file) + strlen(ext) + 1);
   strcpy(path, drive);
   strcat(path, dir);
   strcat(path, file);
   strcat(path, ext);
#else
#if WANG
   const char *s = wispconfigdir();
#else
   const char *s = getenv("LEXICAL");
#endif
   if (! s)
      s = "";
   path = new_string(strlen(s) + 1 + strlen(file) + strlen(ext) + 1);
   strcpy(path, s);
   strcat(path, SLASH_STRING);
   strcat(path, file);
   strcat(path, ext);
#endif
   return path;
}


Boolean installation_ok() {
#if DEMO
#if RUNTIME
#else
   char *date = formatted_date(25);
   int year  = date[1] - 48 + 90;
   int month = ((date[2] - 48) * 10) + (date[3] - 48);
   delete_string(date);

#if DOS
   if (year == 92 && month <= 12)
#else
   if (true) // No date check
#endif
      ;
   else {
      report_general_error(7);
      exit(0);
   }
#endif
#endif

   int ok;
#if WANG && ! DOS_HOST
   // Make sure the message file exists
   char *msg_file = installation_pathname(MESSAGES_NAME, MESSAGES_EXT);
   struct stat buf;
   ok = stat(msg_file, &buf) == 0;
   delete_string(msg_file);
   if (! ok) {
      write_stdout(product_copyright());
#if WANG
      write_stdout("\n\nWPROC is not installed properly.\nMessage file not found.\n");
#else
      write_stdout
         ("\n\nRun is not installed properly.  Check that the LEXICAL environment variable\n");
      write_stdout
         ("is set to the directory where Run is installed.\n");
#endif
      exit(0);
   }
#endif

#if ! DEMO && DOS && ! RUNTIME
   // Check that message file is protected to read-only
   char *msg_file = installation_pathname(MESSAGES_NAME, MESSAGES_EXT);
   int attrib = _chmod(msg_file, 0);
   ok = attrib == -1 ? 0 : (attrib & FA_RDONLY) != 0;
   delete_string(msg_file);
#endif
   return BOOLEAN(ok);
}


#if RUNTIME
#if DEMO
Boolean runtime_demo_ok(const char *source_name) {
   char name[MAXFILE];
   fnsplit(source_name, NULL, NULL, name, NULL);
   if (strcmp(name, "DEMO") != 0) {
      report_general_error(8);
      return false;
   }
   return true;
}
#endif
#endif
