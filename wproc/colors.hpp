// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : colors.hpp
// Author : George Soules
// Date   : 24 May 1991

#ifndef COLORS__HPP
#define COLORS__HPP

// Classes
// (none)

// Definitions and subprograms
#include "environ.hpp"

#define FIRST_LT_COLOR 8  // first light color

enum color {
   color_black,
   color_blue,
   color_green,
   color_cyan,
   color_red,
   color_magenta,
   color_yellow,
   color_white,
   color_lt_black,
   color_lt_blue,
   color_lt_green,
   color_lt_cyan,
   color_lt_red,
   color_lt_magenta,
   color_lt_yellow,
   color_lt_white
};


struct colors {
   color foreground;
   color background;
};


Boolean is_color(const char *a_string, color &a_color);

char *number_to_color(int number);

void colors_cleanup();

#endif

