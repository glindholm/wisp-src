//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
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


//
//	History:
//	$Log: colors.hpp,v $
//	Revision 1.5  1998-08-31 15:13:35-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/colors.hpp,v
//	Working file: colors.hpp
//	head: 1.4
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
//	total revisions: 4;	selected revisions: 4
//	description:
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:42-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:51:58-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:38-05;  author: gsl;  state: V3_3x12;  lines: +1 -1
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:50:59-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
