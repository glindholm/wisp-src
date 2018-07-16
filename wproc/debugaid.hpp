// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : debugaid.hpp
// Author : George Soules
// Date   : 24 February 1991

#ifndef DEBUGAID__HPP
#define DEBUGAID__HPP

// Classes
#include "options.hpp"

// Definitions and subprograms
#include "assert.hpp"
#include "environ.hpp"


#if DEBUG

Boolean valid_ptr(void *p);
void    debug_trace_begin(Boolean option, const char *msg, const char *kind);
void    debug_trace_end(Boolean option);
void    debug_trace(Boolean option, const char *msg, const char *kind);
void    debug_trace_i(Boolean option, int_32 number, const char *kind);
void    debug_trace_si(Boolean option, const char *msg, int_32 number, const char *kind);
void    debug_trace_sc(Boolean option, const char *msg, char the_char, const char *kind);
void    debug_trace_ss(Boolean option, const char *msg1, const char *msg2, const char *kind);
void    debug_trace_sq(Boolean option, const char *msg1, const char *msg2, const char *kind);
void    print_trace_id(const char *trace_kind);
void    debug_trace_level_save();
void    debug_trace_level_restore();

#define trace_begin(kind, arg)\
debug_trace_begin((Boolean) user_options.debug_trace_##kind(), arg, #kind)

#define trace_end(kind)\
debug_trace_end((Boolean) user_options.debug_trace_##kind())

#define trace(kind, arg)\
debug_trace((Boolean) user_options.debug_trace_##kind(), arg, #kind)

#define trace_i(kind, arg)\
debug_trace_i((Boolean) user_options.debug_trace_##kind(), arg, #kind)

#define trace_si(kind, arg1, arg2)\
debug_trace_si((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_sc(kind, arg1, arg2)\
debug_trace_sc((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_ss(kind, arg1, arg2)\
debug_trace_ss((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_sq(kind, arg1, arg2)\
debug_trace_sq((Boolean) user_options.debug_trace_##kind(), arg1, arg2, #kind)

#define trace_level_save()\
debug_trace_level_save()

#define trace_level_restore()\
debug_trace_level_restore()

#else

#define trace_begin(kind, arg)
#define trace_end(kind)
#define trace(kind, arg)
#define trace_i(kind, arg)
#define trace_si(kind, arg1, arg2)
#define trace_sc(kind, arg1, arg2)
#define trace_ss(kind, arg1, arg2)
#define trace_sq(kind, arg1, arg2)
#define trace_level_save()
#define trace_level_restore()

#endif
#endif

