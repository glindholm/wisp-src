//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//
// Module : builtin.cpp
// Author : George Soules
// Date   : 5 May 1991

// Specification
#include "builtin.hpp"

// Classes
#include "files.hpp"
#include "machine.hpp"

// Definitions and subprograms
#ifdef NOCTYPE
extern "C" {
#include <ctype.h>
}
// following definitions are a hack for SCO since it does not define
// these as functions
int (isalpha) (int c)  { return isalpha(c);  }
int (isalnum) (int c)  { return isalnum(c);  }
int (isdigit) (int c)  { return isdigit(c);  }
int (isxdigit)(int c)  { return isxdigit(c); }
int (isprint) (int c)  { return isprint(c);  }
int (islower) (int c)  { return islower(c);  }
int (isupper) (int c)  { return isupper(c);  }
#else
#include <ctype.h>
#endif
#if DOS || DOS_HOST
#include <alloc.h>
#include <dir.h>
#include <dos.h>
#include <io.h>
#include "critical.hpp"
#endif
#if UNIX
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
#if WIN32
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#include <direct.h>
#endif

#include "cancel.hpp"
#include "crt_io.hpp"
#include "fileinfo.hpp"
#include "keyboard.hpp"
#include "memory.hpp"
#if NETWORK
#include "network.hpp"
#endif
#include "process.hpp"
#include "sysenv.hpp"
#include "txt.hpp"
#include "utility.hpp"

#ifdef WIN32
#define chdir(path)			_chdir(path)
#define fileno(file)			_fileno(file)
#define rmdir(dir)			_rmdir(dir)
#define getcwd(buffer, maxlen)		_getcwd(buffer, maxlen)
#define filelength(file)		_filelength(file)
#define tempnam(dir, prefix)		_tempnam(dir, prefix)
#endif

char *make_pattern(const char *name) {
   char    *pattern;
   Boolean  is_dir = false;

#if DOS || DOS_HOST
   if (access(name, 0) == 0)
      is_dir = BOOLEAN((_chmod(name, 0) & FA_DIREC) != 0);
#else
#if UNIX || WIN32
   struct stat buf;
   if (stat(name, &buf) == 0)
      is_dir = BOOLEAN((buf.st_mode & S_IFDIR) != 0);
#endif
#endif

   if (is_dir) {
      pattern = new_string(strlen(name) + 5);
      strcpy(pattern, name);
      strcat(pattern, ALL_FILES_WILDCARD);
   }
   else
      pattern = dup_string(name);

   return pattern;
}


void machine::args_error(int args, int min_args, int max_args, int_32 a_range) {
   char s1[INT_32_STRING_SIZE];
   char s2[INT_32_STRING_SIZE];
   if (min_args == max_args) {
      int_32_to_ascii(args, s1);
      int_32_to_ascii(min_args, s2);
      fatal_error(a_range, 24, s1, s2);
   }
   else {
      int_32_to_ascii(min_args, s1);
      int_32_to_ascii(max_args, s2);
      fatal_error(a_range, 54, s1, s2);
   }
}


#if NETWORK
Boolean machine::verify_network_available(int_32 a_range) {
   if (nw_network_available())
      return true;
   fatal_error(a_range, 77);
   return false;
}


Boolean machine::verify_user_logged_in(int_32 a_range) {
   if (verify_network_available(a_range))
      if (nw_user_logged_in())
         return true;
   fatal_error(a_range, 78);
   return false;
}
#endif


// Integer builtins

Boolean machine::builtin_value(
   int_32       &value,
   int           args,
   builtin_data &a_builtin,
   int_32        a_range)
{
   if (args < a_builtin.min_args() || args > a_builtin.max_args()) {
      args_error(args, a_builtin.min_args(), a_builtin.max_args(), a_range);
      return false;
   }
   switch (a_builtin.kind()) {
      case bi_abs : {
         value = abs((int)(exp[1].integer(25, txt_parameter)));
         if (bad_exp(1)) return false;
         break;
      }

      case bi_alarm : {
         int_32 times = 1;
         if (args) {
            times = exp[1].integer(25, txt_parameter);
            if (bad_exp(1)) return false;
         }
         while (times-- > 0)
            ring_bell();
         value = 0;
         break;
      }

      case bi_args : {
         value = the_args->count();
         break;
      }

      case bi_change_dir : {
         char *dir = dup_string(exp[1].string());
         int   last = strlen(dir) - 1;
         if (last >= 1 && dir[last] == SLASH_CHAR)
            dir[last] = '\0';
         value = chdir(dir) == 0;
         delete_string(dir);
         break;
      }

#if DOS
      case bi_change_drive : {
         int drive = toupper(exp[1].string()[0]) - (int) 'A';
         if (drive >= 0 && drive <= 26) {
            setdisk(drive);
            value = drive == getdisk();
         }
         else
            value = false;
         break;
      }
#endif

      case bi_curcol : {
         value = where_x();
         break;
      }

      case bi_currow : {
         value = where_y();
         break;
      }

      case bi_cursor : {
         int_32 op1 = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         int_32 op2 = exp[2].integer(25, txt_parameter);
         if (bad_exp(2)) return false;
         goto_xy((int)op1, (int)op2);
         value = true;
         break;
      }

      case bi_delay : {
#if WIN32
	 value=0;
	 assert(0); // NOT IMPLEMENTED
#else
         value = abs((int)(exp[1].integer(25, txt_parameter)));
         if (bad_exp(1)) return false;
         sleep((unsigned int)value);
         value = 0;
#endif
         break;
      }

      case bi_dir_entries : {
         int done=0;
#if DOS || DOS_HOST
         char *pattern = make_pattern(exp[1].stripped_string());
         ffblk info;

         value = 0;
         done  = findfirst(pattern, &info, FA_DIREC + FA_HIDDEN + FA_SYSTEM);
         while (! done) {
            if (info.ff_name[0] != '.')
               value += 1;
            done = findnext(&info);
         }
         delete_string(pattern);
#elif defined(UNIX)
//         struct stat buf;
         DIR    *dirp;
         struct  dirent *direntp;
         char   *name;
         Boolean ok;

         value = 0;
         done = (dirp = opendir((char *) exp[1].stripped_string())) == NULL;
         if (! done)
            done = (direntp = readdir(dirp)) == NULL;
         while (! done) {
            ok = true;
            // Ignore '.' and '..'
            name = direntp->d_name;
            if (name[0] == '.') {
               if ((name[1] == '.' && name[2] == '\0') || name[1] == '\0')
                  ok = false;
            }
            if (ok)
               value += 1;
            done = (direntp = readdir(dirp)) == NULL;
         }
         if (dirp)
            closedir(dirp);
#else
	 value = 0;
	 assert(false); // NOT IMPLEMENTED
#endif
         break;
      }

      case bi_dir_exists :
      case bi_file_exists :
      {
         const char* name = exp[1].stripped_string();
#if DOS || DOS_HOST
         value = access(name, 0) == 0;
         if (value) {
            value = (_chmod(name, 0) & FA_DIREC) != 0;
            if (a_builtin.kind() == bi_file_exists)
               value = ! value;
         }
#else
#if UNIX || WIN32
         struct stat buf;
         value = stat(name, &buf) == 0;
         if (value) {
            value = (buf.st_mode & S_IFDIR) != 0;
            if (a_builtin.kind() == bi_file_exists)
               value = ! value;
         }
#endif
#endif
         break;
      }

      case bi_file_size : {
         const char* name = exp[1].stripped_string();
#if DOS || DOS_HOST || WIN32
         FILE *fp = fopen(name, "r");
         if (fp) {
            value = filelength(fileno(fp));
            fclose(fp);
         }
         else
            value = -1;
#else
#if UNIX
         struct stat buf;
         value = stat(name, &buf) == 0;
         if (value)
            value = buf.st_size;
         else
            value = -1;
#endif
#endif
         break;
      }

      case bi_disk_space : {
#if DOS 
         dfree free;
         int   drive = toupper(exp[1].string()[0]) - (int) 'A';
         value = -1;
         if (drive >= 0 && drive <= 26) {
            getdfree(drive + 1, &free);
            if (free.df_sclus != 0xFFFF)
               value = (int_32) free.df_avail *
                       (int_32) free.df_bsec *
                       (int_32) free.df_sclus;
         }
#else
         value = -1;
	 assert(0); // NOT IMPLEMENTED
#endif
         break;
      }

#if DOS
      case bi_drive_ready : {
         int drive = toupper(exp[1].string()[0]) - (int) 'A';
         if (drive >= 0 && drive <= 26)
            value = drive_ready(drive);
         else
            value = false;
         break;
      }
#endif

      case bi_false : {
         value = 0;
         break;
      }

      case bi_file_lines : {
         FILE *fp = fopen(exp[1].stripped_string(), "r");
         int   c  = 0;
         if (fp) {
            value = 0;
            while (c != EOF) {
               while ((c = fgetc(fp)) != EOF && c != '\n');
               if (c == '\n')
                  value += 1;
            }
            fclose(fp);
         }
         else
            value = -1;
         break;
      }

      case bi_get_key : {
         value = get_key_pressed();
         break;
      }

      case bi_integer : {
         value = exp[1].integer(38, txt_int_value);
         if (bad_exp(1))
            return false;
         break;
      }

      case bi_is_alpha :
      case bi_is_alphanum :
      case bi_is_digits :
      case bi_is_hex :
      case bi_is_lower :
      case bi_is_printable :
      case bi_is_upper :
      {
         int (*is)(int c);
         switch(a_builtin.kind()) {
            case bi_is_alpha     : is = isalpha;  break;
            case bi_is_alphanum  : is = isalnum;  break;
            case bi_is_digits    : is = isdigit;  break;
            case bi_is_hex       : is = isxdigit; break;
            case bi_is_printable : is = isprint;  break;
            case bi_is_lower     : is = islower;  break;
            case bi_is_upper     : is = isupper;  break;
            default              : assert(UNREACHABLE);
         }
         const char *s = exp[1].string();
         unsigned c;
         value = s[0] ? true : false;
         while (((c = s++[0]) != 0) && isascii(c) && (*(is))(c));
         if (c)
            value = false;
         break;
      }

#if DOS
      case bi_is_archive :
      case bi_is_dir :
      case bi_is_hidden :
      case bi_is_readonly :
      case bi_is_system :
      {
         int attrib = _chmod(exp[1].stripped_string(), 0);
         int bit;
         switch (a_builtin.kind()) {
            case bi_is_archive   : bit = FA_ARCH;   break;
            case bi_is_dir       : bit = FA_DIREC;  break;
            case bi_is_hidden    : bit = FA_HIDDEN; break;
            case bi_is_readonly  : bit = FA_RDONLY; break;
            case bi_is_system    : bit = FA_SYSTEM; break;
         }
         value = attrib == -1 ? 0 : (attrib & bit) != 0;
         break;
      }
#endif

      case bi_is_color : {
         color dummy;
         value = is_color(exp[1].string(), dummy);
         break;
      }

      case bi_is_integer : {
         int_32 i;
         value = string_to_int_32(exp[1].string(), i);
         break;
      }

      case bi_is_yesno : {
         Boolean dummy;
         value = is_yes_or_no(exp[1].string(), dummy);
         break;
      }

      case bi_key_ready : {
         value = key_pressed(0);
         break;
      }

      case bi_index : {
         value = str_index(exp[1].string(), exp[2].string());
         break;
      }

#if WANG
      case bi_label : {
         symbol *the_symbol = the_symbol_table->lookup(exp[1].string());
         value = 0;
         if (the_symbol)
            if (the_symbol->is.label)
               value = 1;
         break;
      }
#endif

      case bi_length : {
         if (! exp[1].is_lvalue()) {
            exp[1].fatal_error(63, exp[1].string(), NULL);
            return false;
         }
         lvalue &the_lvalue = exp[1].lvalue_ref();
         symbol &the_symbol = the_lvalue.symbol_ref();
         if (the_symbol.is.array && the_lvalue.args() == 0)
            value = ((array_data&) (the_symbol.data_ref())).dimension();
         else
            value = strlen(exp[1].string());
         break;
      }

#if NETWORK
      case bi_logged_in : {
         if (! verify_network_available(a_range))
            return false;
         value = nw_user_logged_in();
         break;
      }
#endif

      case bi_make_dir : {
         const char* name = exp[1].stripped_string();
#if DOS || DOS_HOST || WIN32
         value = _mkdir(name) == 0;
#else
#if UNIX
#define DEFAULT_DIRMODE 0777
         mode_t mode = DEFAULT_DIRMODE;
         value = mkdir(name, mode) == 0;
#endif
#endif
         break;
      }

      case bi_match : {
         char *s1 = trim(exp[1].string());
         char *s2 = trim(exp[2].string());
         value = same_string(s1, s2);
         delete_string(s1);
         delete_string(s2);
         break;
      }

      case bi_max :
      case bi_min :
      {
         int_32 op1 = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         int_32 op2 = exp[2].integer(25, txt_parameter);
         if (bad_exp(2)) return false;
         value = a_builtin.kind() == bi_max ?
            max(op1, op2) : min(op1, op2);
         break;
      }

#if DOS
      case bi_memory : {
         value = coreleft();
      }
#endif

      case bi_mod : {
         int_32 op1 = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         int_32 op2 = exp[2].integer(25, txt_parameter);
         if (bad_exp(2)) return false;
         // Mod is defined according to the Ada LRM 4.5.5(15)
         value = op2 ? op1 % op2 : op1;
         if (value && ((op1 < 0 && op2 > 0) || (op1 > 0 && op2 < 0)))
            value += op2;
         break;
      }

      case bi_monochrome : {
         value = ! color_monitor();
         break;
      }

#if NETWORK
      case bi_network_available : {
         value = nw_network_available();
         break;
      }
#endif

      case bi_random : {
#if DOS
         int_32 max = INT_MAX;
         static unsigned seed = 0;
         if (args) {
            max = exp[1].integer(1, INT_MAX, 25, txt_parameter);
            if (bad_exp(1)) return false;
         }
         if (! seed) {
            // Use the two hundreths digits times each other as a seed.
            char *time = formatted_time(5);
            // Digits 0 - 9 are mapped to 1 - 10 to avoid seed of 0.
            int digit1 = time[6] - 47;
            int digit2 = time[7] - 47;
            seed = digit1 * digit2;
            srand(seed * seed);
            delete_string(time);
         }
         value = random(max) + 1;
#else
         // NOT SUPPORTED
         value = 0;
#endif
         break;
      }

      case bi_rank :
         value = (usign_8) exp[1].string()[0];
         break;

      case bi_read_dir : {
         if (! exp[2].is_lvalue()) {
            exp[2].fatal_error(63, exp[2].string(), NULL);
            return false;
         }
         lvalue            &the_lvalue = exp[2].lvalue_ref();
         symbol            &the_symbol = the_lvalue.symbol_ref();
         data              &data_ref   = the_symbol.data_ref();
         string_array_data &the_data   = (string_array_data&) data_ref;
         int                index      = 0;
         int                done=0;

#if DOS || DOS_HOST
         ffblk              info;

         if (the_lvalue.args() == 0 &&
             the_symbol.is.array && the_symbol.is.string)
         {
            char *pattern = make_pattern(exp[1].stripped_string());
            value = 0;
            done = findfirst(pattern, &info, FA_DIREC + FA_HIDDEN + FA_SYSTEM);
            while (! done && index < the_data.dimension()) {
               if (info.ff_name[0] != '.') {
                  the_data[index++].set_contents(info.ff_name);
                  value += 1;
               }
               done = findnext(&info);
            }
            delete_string(pattern);
            break;
         }
#elif defined(UNIX)
         DIR *dirp;
         struct dirent *direntp;

         if (the_lvalue.args() == 0 &&
             the_symbol.is.array && the_symbol.is.string)
         {
            Boolean ok;
            char *name;
            value = 0;
            done = (dirp = opendir((char *) exp[1].stripped_string())) == NULL;
            if (! done)
               done = (direntp = readdir(dirp)) == NULL;
            while (! done && index < the_data.dimension()) {
               ok = true;
               // Ignore '.' and '..'
               name = direntp->d_name;
               if (name[0] == '.') {
                  if ((name[1] == '.' && name[2] == '\0') || name[1] == '\0')
                     ok = false;
               }
               if (ok) {
                  the_data[index++].set_contents(name);
                  value += 1;
               }
               done = (direntp = readdir(dirp)) == NULL;
            }
            if (dirp)
               closedir(dirp);
            break;
         }
#else
         if (the_lvalue.args() == 0 &&
             the_symbol.is.array && the_symbol.is.string)
         {
		value = 0;
		assert(0); // NOT IMPLEMENTED
	 }
#endif
         else {
            exp[2].fatal_error
               (the_lvalue.args() != 0 ? 57 : 43, the_symbol.name(), NULL);
            return false;
         }
      }

      case bi_remainder : {
         int_32 op1 = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         int_32 op2 = exp[2].integer(25, txt_parameter);
         if (bad_exp(2)) return false;
         if (op2) {
            ldiv_t lx;
            lx = ldiv(op1, op2);
            value = lx.rem;
         }
         else
            value = 0;
         break;
      }

      case bi_remove_dir : {
         value = rmdir(exp[1].string()) == 0;
         break;
      }

#if DOS
      case bi_set_file_date :
      case bi_set_file_time :
      {
         int_32  year;
         int_32  month;
         int_32  day;
         int_32  hour;
         int_32  min;
         int_32  sec;

         Boolean set_date = BOOLEAN(a_builtin.kind() == bi_set_file_date);

         int format_status = set_date ?
            parse_date(exp[2].string(), 1980, 2107, year, month, day) :
            parse_time(exp[2].string(), hour, min, sec);

         if (format_status == 0) {
            ffblk         info;
            int           done;
            Boolean       ok;
            int           len;
            const char   *pattern;
            char          dir[MAXPATH];
            struct ftime  ft;
            FILE         *fp;

            pattern = exp[1].stripped_string();
            get_dir(dir, pattern);
            len  = strlen(dir);
            done = findfirst(pattern, &info, 0);
            ok   = BOOLEAN(! done);

            while (! done) {
               fp = fopen(strcat(dir, info.ff_name), "r");
               dir[len] = '\0';
               if (fp) {
                  getftime(fileno(fp), &ft);
                  if (set_date) {
                     // Change date, but leave time alone
                     ft.ft_year  = (unsigned) (year - 1980);
                     ft.ft_month = month;
                     ft.ft_day   = day;
                  }
                  else {
                     // Change time, but leave date alone
                     ft.ft_hour  = hour;
                     ft.ft_min   = min;
                     ft.ft_tsec  = (unsigned) (sec / 2);
                  }
                  setftime(fileno(fp), &ft);
                  fclose(fp);
               }
               else
                  ok = false;
               done = findnext(&info);
            }
            value = ok ? 0 : 1;
         }
         else
            value = format_status;
         break;
      }
#endif

      case bi_sort : {
         if (! exp[1].is_lvalue()) {
            exp[1].fatal_error(63, exp[1].string(), NULL);
            return false;
         }
         lvalue &the_lvalue = exp[1].lvalue_ref();
         symbol &the_symbol = the_lvalue.symbol_ref();
         data   &data_ref   = the_symbol.data_ref();

         if (the_lvalue.args() == 0  && the_symbol.is.array)
            ((array_data&) data_ref).sort();
         else {
            exp[1].fatal_error
               (the_lvalue.args() != 0 ? 56 : 55, the_symbol.name(), NULL);
            return false;
         }
         value = 0;
         break;
      }

      case bi_true : {
         value = 1;
         break;
      }

      case bi_verify :
         value = str_verify(exp[1].string(), exp[2].string());
         break;

      case bi_verify_date :
      case bi_verify_time :
      {
         int_32 x;
         int_32 y;
         int_32 z;
         value = a_builtin.kind() == bi_verify_date ?
            parse_date(exp[1].string(), 0000, 9999, x, y, z) :
            parse_time(exp[1].string(), x, y, z);
         break;
      }

      default:
         assert(UNREACHABLE);
   }
   return true;
}


// String builtins

Boolean machine::builtin_value(
   char        *&value,
   int           args,
   builtin_data &a_builtin,
   int_32        a_range)
{
   if (args < a_builtin.min_args() || args > a_builtin.max_args()) {
      args_error(args, a_builtin.min_args(), a_builtin.max_args(), a_range);
      return false;
   }
   switch (a_builtin.kind()) {

      case bi_arg : {
         int_32 index = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         if (index < 1 || index > the_args->count())
            index = 0;
         value = dup_string(index ? the_args->value((int)index) : "");
         break;
      }

      case bi_byte : {
         int_32 the_byte = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         value = new_string(2);
         value[1] = '\0';
         if ((unsigned) the_byte > UCHAR_MAX)
            value[0] = '\0';
         else
            value[0] = (char)the_byte;
         break;
      }

      case bi_center :
      case bi_left :
      case bi_right :
      {
         int len    = strlen(exp[1].string());
         value      = new_string(len + 1);
         char *temp = strip(exp[1].string());
         int i;
         int pad;
         if (a_builtin.kind() == bi_left)
            pad = 0;
         else {
            pad = len - strlen(temp);
            if (a_builtin.kind() == bi_center)
               pad /= 2;
         }
         for (i = 0; i < len; i++)
            value[i] = ' ';
         value[i] = '\0';
         i = 0;
         while (temp[i]) {
            (value + pad)[i] = temp[i];
            i += 1;
         }
         delete temp;
         break;
      }

      case bi_color : {
         int_32 i = exp[1].integer(25, txt_parameter);
         if (bad_exp(1)) return false;
         value = number_to_color((int)i);
         break;
      }

      case bi_copy : {
         const char* the_string = exp[1].string();
         int_32 copies = exp[2].integer(25, txt_parameter);
         if (bad_exp(2)) return false;
         value = new_string((unsigned int)(copies * strlen(the_string) + 1));
         value[0] = '\0';
         while (copies--)
            strcat(value, the_string);
         break;
      }

      case bi_current_dir : {
         char pathname[MAXPATH];
         getcwd(pathname, MAXPATH);
         value = dup_string(pathname);
         break;
      }

#if DOS
      case bi_current_drive : {
         value = dup_string(" :");
         value[0] = (char) getdisk() + 'A';
         break;
      }
#endif

      case bi_date : {
         int_32 the_format = 1; // was 26
         if (args) {
            the_format = exp[1].integer(25, txt_parameter);
            if (bad_exp(1))
               return false;
         }
         if (args == 2) {
            int_32 year;
            int_32 month;
            int_32 day;
            if (parse_date(exp[2].string(), 0000, 9999, year, month, day) == 0)
               value = formatted_date((int)the_format, (int)year, (int)month, (int)day);
            else
               value = dup_string("");
         }
         else
            value = formatted_date((int)the_format);
         break;
      }

#if DOS
      case bi_dos_version : {
         char *version;
         value = dup_string(" .  ");
         version = int_32_to_string(_osmajor);
         value[0] = version[0];
         delete version;
         version = int_32_to_string(_osminor);
         value[2] = version[0];
         if (version[1])
            value[3] = version[1];
         delete version;
         break;
      }
#endif

      case bi_extract : {
         char *name = strip(exp[1].string());
         value = environment->get(name);
         delete name;
         break;
      }

      case bi_file_date :
      case bi_file_time :
      {
#if DOS || DOS_HOST
         FILE *fp = fopen(exp[1].stripped_string(), "r");
         if (fp) {
            int_32 the_format = -1;
            struct ftime ft;
            getftime(fileno(fp), &ft);
            if (args == 2) {
               the_format = exp[2].integer(25, txt_parameter);
               if (bad_exp(2))
                  return false;
            }
            if (a_builtin.kind() == bi_file_date) {
               the_format = the_format == -1 ? 26 : the_format;
               value = formatted_date
                  (the_format, ft.ft_year + 1980, ft.ft_month, ft.ft_day, 7);
            }
            else {
               the_format = the_format == -1 ? 3 : the_format;
               value = formatted_time
                  (the_format, ft.ft_hour, ft.ft_min, ft.ft_tsec * 2, 0);
            }
            fclose(fp);
         }
         else
            value = dup_string("");
#else
#if UNIX
         struct stat buf;
         struct tm timedate;
         struct tm *ptimedate = &timedate;
         int_32 the_format = -1;

         if (stat(exp[1].stripped_string(), &buf) == 0) {
            ptimedate = localtime(&buf.st_mtime);
            if (args == 2) {
               the_format = exp[2].integer(25, txt_parameter);
               if (bad_exp(2))
                  return false;
            }
            if (a_builtin.kind() == bi_file_date) {
               the_format = the_format == -1 ? 26 : the_format;
               value = formatted_date(
                   (int) the_format,
                   (int) ptimedate->tm_year + 1900,
                   (int) ptimedate->tm_mon + 1,
                   (int) ptimedate->tm_mday,
                   (int) 7);
            }
            else {
               the_format = the_format == -1 ? 3 : the_format;
               value = formatted_time(
                  (int) the_format,
                  (int) ptimedate->tm_hour,
                  (int) ptimedate->tm_min,
                  (int) ptimedate->tm_sec,
                  (int) 0);
            }
         }
         else
            value = dup_string("");
#endif
#endif
         break;
      }

      case bi_file_dir :
#if DOS
      case bi_file_drive :
#endif
      case bi_file_ext :
      case bi_file_name :
      {
         const char *s = exp[1].stripped_string();
         switch(a_builtin.kind()) {
            case bi_file_dir   : value = file_dir(s);    break;
#if DOS
            case bi_file_drive : value = file_drive(s);  break;
#endif
            case bi_file_ext   : value = file_suffix(s); break;
            case bi_file_name  : value = file_name(s);   break;
         }
         break;
      }

#if DOS || DOS_HOST
      case bi_file_search : {
         char *file = searchpath(exp[1].string());
         value = dup_string(file ? file : "");
         break;
      }
#endif

      case bi_get_char : {
         char *s = " ";
         s[0] = get_key_pressed();
         while (s[0] < 0) {
            ring_bell();
            s[0] = get_key_pressed();
         }
         if (s[0] == KEY_CTRLC)
            cancel_handler();
         value = dup_string(s);
         break;
      }

#if NETWORK || UNIX || WIN32
      case bi_login_name : {
#if NETWORK
         if (! verify_user_logged_in(a_range))
            return false;
         value = nw_user_login_name();
#else
         value = dup_string("NYI for UNIX");
#endif
         break;
      }
#endif

      case bi_lower : {
         value = lower_case(dup_string(exp[1].string()));
         break;
      }

#if NETWORK
      case bi_network_address : {
         if (! verify_network_available(a_range))
            return false;
         value = nw_network_address();
         break;
      }
#endif

      case bi_parse : {
         const char *source = exp[1].string();
         int         len    = strlen(source);
         int         count  = (int)(exp[2].integer(0, 255, 25, txt_parameter));
         if (bad_exp(2))
            return false;
         char        delim  = args == 3 ? exp[3].string()[0] : ' ';
         int         first  = 0;
         int         end    = 0;
         int         i = 1;
         if (delim) {
            if (delim != ' ' && source[0] == delim)
               i = 2;
            for (; i <= count; i++) {
               first = end;
               if (end >= len)
                  break;
               if (count > 1)
                  first += 1;
               if (delim == ' ') {
                  while (source[first] == ' ')
                     first += 1;
               }
               end = first;
               while (source[end]) {
                  if (source[end] == delim)
                     break;
                  end += 1;
               }
            }
         }
         value = new_string(end - first + 1);
         i = 0;
         for (; first < end; first++)
            value[i++] = source[first];
         value[i] = '\0';
         break;
      }

      case bi_proc_name : {
         value = dup_string(the_process->the_input_pathname);
         break;
      }

      case bi_strip : {
         value = strip(exp[1].string());
         break;
      }

      case bi_time : {
         int_32 the_format = 3;
         if (args) {
            the_format = exp[1].integer(25, txt_parameter);
            if (bad_exp(1))
               return false;
         }
         if (args == 2) {
            int_32 hour;
            int_32 min;
            int_32 sec;
            if (parse_time(exp[2].string(), hour, min, sec) == 0)
               value = formatted_time((int)the_format, (int)hour, (int)min, (int)sec, 0);
            else
               value = dup_string("");
         }
         else
            value = formatted_time((int)the_format);
         break;
      }

      case bi_trim : {
         value = trim(exp[1].string());
         break;
      }

      case bi_translate : {
         value = str_translate
            (exp[1].string(), exp[2].string(), exp[3].string());
         break;
      }

      case bi_unique : {
         char pathname[128];
         if (args)
            strcpy(pathname, exp[1].string());
         else
            getcwd(pathname, 128);
#if DOS || DOS_HOST
         if (pathname[strlen(pathname) - 1] != '\\')
            strcat(pathname, "\\");
         upper_case(pathname);
         int handle = creattemp(pathname, 0);
         value = dup_string(handle == -1 ? "" : pathname);
         if (handle != -1)
            close(handle);
#else
#if UNIX || WIN32
         value = tempnam((char *) pathname, 0);
#endif
#endif
         break;
      }

      case bi_upper : {
         value = upper_case(dup_string(exp[1].string()));
         break;
      }

#if NETWORK
      case bi_user_name : {
         if (! verify_user_logged_in(a_range))
            return false;
         value = nw_user_full_name();
         break;
      }
#endif

      case bi_yesno : {
         int_32 yes = exp[1].integer(25, txt_parameter);
         if (bad_exp(1))
            return false;
         value = dup_string(yes ? "YES" : "NO");
         break;
      }

      default:
         assert(UNREACHABLE);
   }
   return true;
}


void machine::exec_builtin() {
   pop_expressions(1);
   stmt_return_code =
      exp[1].kind() == expression::integer_kind ? exp[1].integer() : 0;
}

//
//	History:
//	$Log: builtin.cpp,v $
//	Revision 1.15  2011/10/29 20:09:14  gsl
//	Fix ISO routine name warnins on WIN32
//	
//	Revision 1.14  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.13  1998/08/31 19:13:33  gsl
//	drcs update
//	
//

//	
//
//	Working file: builtin.cpp
//	head: 1.12
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
//	total revisions: 12;	selected revisions: 12
//	description:
//	----------------------------
//	revision 1.12
//	date: 1997-10-01 18:26:15-04;  author: gsl;  state: V4_3_00;  lines: +4 -4
//	fixed warnings
//	----------------------------
//	revision 1.11
//	date: 1997-06-09 17:36:52-04;  author: scass;  state: V4_1_02;  lines: +3 -3
//	int4 -> int_32
//	----------------------------
//	revision 1.10
//	date: 1997-06-09 16:41:53-04;  author: scass;  state: Exp;  lines: +3 -3
//	Changed long to int4 for portability.
//	----------------------------
//	revision 1.9
//	date: 1996-09-10 11:39:15-04;  author: gsl;  state: V3_3_93;  lines: +1 -1
//	Change to use WIN32 style _mkdir()
//	----------------------------
//	revision 1.8
//	date: 1996-07-25 19:44:19-04;  author: gsl;  state: Exp;  lines: +33 -18
//	Fix for NT
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 14:14:20-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from builtin.cc to builtin.cpp
//	----------------------------
//	revision 1.6
//	date: 1995-06-02 11:26:36-04;  author: gsl;  state: V3_3_19;  lines: +19 -19
//	fixed a bunch of type mis-match warnings
//	----------------------------
//	revision 1.5
//	date: 1995-05-03 04:40:25-04;  author: gsl;  state: V3_3_16;  lines: +1 -1
//	changed to ifdef NOCTYPE for systems whose C++ does not
//	supply the C ctype.h routines.
//	These currently are SCO, SUNOS, and PYRAMID.
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:40-04;  author: gsl;  state: V3_3_15;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:57-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:35-05;  author: gsl;  state: V3_3x12;  lines: +20 -20
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:57-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
