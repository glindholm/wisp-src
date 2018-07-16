// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : cancel.cpp
// Author : George Soules
// Date   : 5 July 1991


// Specification
#include "cancel.hpp"

// Classes
// (none)

// Definitions and subprograms
#if DOS || DOS_HOST
#include <dos.h>
#include <process.h>
#else
#if UNIX || WIN32
#include <signal.h>
#endif
#endif

int cancel_requests = 0;


#if UNIX || WIN32
void signal_handler(int sig) {
   set_cancel_handler();
   cancel_handler();
}
#endif


void set_cancel_handler() {
#if DOS || DOS_HOST
   ctrlbrk(cancel_handler);
#else
#if UNIX || WIN32
   signal(SIGINT, signal_handler);
#endif
#endif
}


#if DOS || DOS_HOST
// This code is reached via DOS interrupt 28 (ctrl-c), not via a C++ call.
// Therefore it must not do a stack check and must only return.  Any attempt
// to exit() from this code will cause the C++ runtime to think something
// is wrong and issue a fatal runtime error.
#pragma option -N-
#endif

int cancel_handler() {
   cancel_requests += 1;
   return cancel_requests;
}


Boolean cancel_requested() {
   if (cancel_requests) {
      cancel_requests = 0;
      return true;
   }
   else
      return false;
}

