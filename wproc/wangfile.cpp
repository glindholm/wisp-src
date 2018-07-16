#include <ctype.h>

#include "wangfile.hpp"

#include "utility.hpp"

#if WANG
wang_filename::wang_filename(const char *s) {
   int i;
   init_string_with_blank_pad(filename, WANG_FILENAME_SIZE);
   init_string_with_blank_pad(libname, WANG_LIBNAME_SIZE);
   init_string_with_blank_pad(volname, WANG_VOLNAME_SIZE);

   int index = 0;

   i = 0;
   while (s[index]) {
      if (s[index] == '\x01' || s[index] == '\x02')
         break;
      else {
         if (i < WANG_FILENAME_SIZE)
            filename[i++] = toupper(s[index]);
      }
      index++;
   }

   if (s[index] == '\x01') {
      index++;
      i = 0;
      while (s[index]) {
         if (s[index] == '\x02')
            break;
         else {
            if (i < WANG_LIBNAME_SIZE)
               libname[i++] = toupper(s[index]);
         }
         index++;
      }
   }

   if (s[index] == '\x02') {
      index++;
      i = 0;
      while (s[index] && i < WANG_VOLNAME_SIZE)
         volname[i++] = toupper(s[index++]);
   }
}
#endif


