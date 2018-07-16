// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : range.hpp
// Author : George Soules
// Date   : 1 April 1991

#ifndef RANGE__HPP
#define RANGE__HPP

// Classes
#include "object.hpp"
#include "token.hpp"

// Definitions and subprograms
#include "environ.hpp"


class range : public object {
   public:
      range();
      range(int_32 a_packed_range);
      range(int_32 a_packed_range_1, int_32 a_packed_range_2);
      range(token *a_token);
      int_32 pack();
      struct {
         usign_16 first;
         usign_16 last;
      } row;
      struct {
         usign_8 first;
         usign_8 last;
      } col;
};

#endif

