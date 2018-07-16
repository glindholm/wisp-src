// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : range.cpp
// Author : George Soules
// Date   : 1 April 1991

// Specification
#include "range.hpp"

// Classes
// (none)

// Definitions and subprograms
#include "debugaid.hpp"


// Note that a packed range does not contain row.last.
// Packed ranges are used for lexical elements that
// cannot cross line boundaries (namely tokens).

union sub_range {
   struct {
      usign_16 row;
      struct {
         usign_8 first;
         usign_8 last;
      } col;
   } unpacked;
   int_32 packed;
};


range::range() {
// trace(object, "range");
   row.first = 0;
   row.last  = 0;
   col.first = 0;
   col.last  = 0;
}


range::range(int_32 a_packed_range) {
// trace(object, "range from packed range");
   sub_range sr;
   sr.packed = a_packed_range;
   row.first = sr.unpacked.row;
   row.last  = sr.unpacked.row;
   col.first = sr.unpacked.col.first;
   col.last  = sr.unpacked.col.last;
}


range::range(int_32 a_packed_range_1, int_32 a_packed_range_2) {
   range range_1(a_packed_range_1);
   range range_2(a_packed_range_2);
   row.first = range_1.row.first;
   col.first = range_1.col.first;
   row.last  = range_2.row.last;
   col.last  = range_2.col.last;
}


range::range(token *a_token) {
// trace(object, "range from token");
   row.first = a_token->row();
   row.last  = row.first;
   col.first = (usign_8)a_token->column();
   col.last  = (usign_8)a_token->last_column();
}


int_32 range::pack() {
   sub_range sr;
   sr.unpacked.row       = row.first;
   sr.unpacked.col.first = col.first;
   sr.unpacked.col.last  = col.last;
   return sr.packed;
}


