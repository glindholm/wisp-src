
#ifndef WANGFILE__HPP
#define WANGFILE__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include "environ.hpp"

#if WANG
class wang_filename : public object {
   public:
      wang_filename(const char *s);
      char filename[WANG_FILENAME_SIZE + 1];
      char libname [WANG_LIBNAME_SIZE  + 1];
      char volname [WANG_VOLNAME_SIZE  + 1];
};
#endif

#endif
