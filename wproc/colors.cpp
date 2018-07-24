//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : colors.cpp
// Author : George Soules
// Date   : 1 July 1991

// Specification
#include "colors.hpp"

// Classes
#include "tables.hpp"

// Definitions and subprograms
#include "debugaid.hpp"
#include "memory.hpp"
#include "utility.hpp"


table *color_table = NULL;

void init_color_table() {
   assert(! color_table);
   color_table = new table();
   color_table->add(new name_kind("black",      color_black));
   color_table->add(new name_kind("blue",       color_blue));
   color_table->add(new name_kind("green",      color_green));
   color_table->add(new name_kind("cyan",       color_cyan));
   color_table->add(new name_kind("red",        color_red));
   color_table->add(new name_kind("magenta",    color_magenta));
   color_table->add(new name_kind("yellow",     color_yellow));
   color_table->add(new name_kind("white",      color_white));
   color_table->add(new name_kind("lt_black",   color_lt_black));
   color_table->add(new name_kind("lt_blue",    color_lt_blue));
   color_table->add(new name_kind("lt_green",   color_lt_green));
   color_table->add(new name_kind("lt_cyan",    color_lt_cyan));
   color_table->add(new name_kind("lt_red",     color_lt_red));
   color_table->add(new name_kind("lt_magenta", color_lt_magenta));
   color_table->add(new name_kind("lt_yellow",  color_lt_yellow));
   color_table->add(new name_kind("lt_white",   color_lt_white));
}


void colors_cleanup() {
   if (color_table) {
      delete color_table;
      color_table = NULL;
   }
}


Boolean is_color(const char *a_string, color &a_color) {
   if (! color_table)
      init_color_table();
   char      *s         = strip(a_string);
   name_kind *the_color = (name_kind *) color_table->lookup(s);
   delete s;
   if (the_color) {
      a_color = (color) the_color->kind();
      return true;
   }
   else
      return false;
}


char *number_to_color(int number) {
   char *s;
   switch (number) {
      case 0  : s = "BLACK";      break;
      case 1  : s = "BLUE";       break;
      case 2  : s = "GREEN";      break;
      case 3  : s = "CYAN";       break;
      case 4  : s = "RED";        break;
      case 5  : s = "MAGENTA";    break;
      case 6  : s = "YELLOW";     break;
      case 7  : s = "WHITE";      break;
      case 8  : s = "LT_BLACK";   break;
      case 9  : s = "LT_BLUE";    break;
      case 10 : s = "LT_GREEN";   break;
      case 11 : s = "LT_CYAN";    break;
      case 12 : s = "LT_RED";     break;
      case 13 : s = "LT_MAGENTA"; break;
      case 14 : s = "LT_YELLOW";  break;
      case 15 : s = "LT_WHITE";   break;
      default : s = "";           break;
   }
   return dup_string(s);
}





//
//	History:
//	$Log: colors.cpp,v $
//	Revision 1.6  1998/08/31 19:13:35  gsl
//	drcs update
//	
//

//	
//
//	Working file: colors.cpp
//	head: 1.5
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
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 14:14:25-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from colors.cc to colors.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:42-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:58-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:37-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:58-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
