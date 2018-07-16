// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : status.hpp
// Author : George Soules
// Date   : 17 July 1991

#ifndef STATUS__HPP
#define STATUS__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


enum status_kind {
   status_none,
   status_ok,
   status_cancel,
   status_destroy,
   status_file_not_found,
   status_memory_error,
   status_need_code,
   status_open_error,
   status_runtime_error,
#if RUNTIME
   status_source_input,
#endif
#if WANG
   status_logoff,
#endif
   status_swap_error,
   status_link_failed
};




#endif

