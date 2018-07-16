//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Design, 1991
//
// Module : token.cpp
// Author : George Soules
// Date   : 28 January 1991

// Specification
#include "token.hpp"

// Classes
#ifdef EXT_COUT
#include <iostream.h>
#endif

// Definitions and subprograms
#include <stdio.h>
#include <string.h>
#include "debugaid.hpp"
#include "environ.hpp"
#include "memory.hpp"


token::token(token &a_token) {
   the_kind         = a_token.the_kind;
   the_lexeme_size  = a_token.the_lexeme_size;
   the_lexeme       = dup_string(a_token.the_lexeme);
   the_row          = a_token.the_row;
   the_column       = a_token.the_column;
   the_error_number = 0;
   next             = a_token.next;
}


token::token(token &token1, token &token2) {
   the_kind         = tk_other;
   the_lexeme_size  = token1.the_lexeme_size + token2.the_lexeme_size;
   the_lexeme       = new_string(the_lexeme_size + 1);
   strcpy(the_lexeme, token1.the_lexeme);
   strcat(the_lexeme, token2.the_lexeme);
   the_row          = token1.the_row;
   the_column       = token1.the_column;
   the_error_number = 0;
   next             = NULL;
}


token::token(
   token_kind a_kind,
   usign_16   a_row,
   usign_16   a_column,
   char      *a_lexeme,
   usign_8    an_error_number)
{
   the_kind         = a_kind;
   the_lexeme       = dup_string(a_lexeme);
   the_lexeme_size  = strlen(the_lexeme);
   the_row          = a_row;
   the_column       = a_column;
   the_error_number = an_error_number;
   next             = NULL;
}


token::~token() {
   delete_string(the_lexeme);
}


usign_16 token::last_column() {
   usign_16 last = the_column + the_lexeme_size - 1;
   if (the_kind == tk_string)
      last += 2; // for quotes
   return last;
}



char *kind_string(token_kind the_kind) {
   switch (the_kind) {
      case tk_comma      : return "tk_comma";
      case tk_colon      : return "tk_colon";
      case tk_concat     : return "tk_concat";
      case tk_delimiter  : return "tk_delimiter";
      case tk_div        : return "tk_div";
#if WANG
      case tk_dot        : return "tk_dot";
#endif
      case tk_eof        : return "tk_eof";
      case tk_eq         : return "tk_eq";
      case tk_ge         : return "tk_ge";
      case tk_gt         : return "tk_gt";
      case tk_identifier : return "tk_identifier";
      case tk_integer    : return "tk_integer";
      case tk_label      : return "tk_label";
      case tk_le         : return "tk_le";
      case tk_lt         : return "tk_lt";
      case tk_minus      : return "tk_minus";
      case tk_mult       : return "tk_mult";
      case tk_ne         : return "tk_ne";
      case tk_other      : return "tk_other";
      case tk_parenleft  : return "tk_parenleft";
      case tk_parenright : return "tk_parenright";
      case tk_pathname   : return "tk_pathname";
      case tk_plus       : return "tk_plus";
      case tk_semicolon  : return "tk_semicolon";
      case tk_string     : return "tk_string";
      case tk_undefined  : return "tk_undefined";
      case tk_variable   : return "tk_variable";
      default                   : return "tk_UNKNOWN";
   }
}

void token::dump() const {
	char mess[256];
   
	if (user_options.debug_trace_scanner())
	{
		if (the_kind == tk_string)
		{
			sprintf(mess, "row %d, col %d, size %d, %s \"%s\"", 
				the_row, (int) the_column, the_lexeme_size, kind_string(the_kind),the_lexeme);
		}
   		else
		{
			sprintf(mess, "row %d, col %d, size %d, %s %s", 
				the_row, (int) the_column, the_lexeme_size, kind_string(the_kind),the_lexeme);
		}

   		trace(scanner,mess);
	}
}


//
//	History:
//	$Log: token.cpp,v $
//	Revision 1.8  2003/02/11 19:05:26  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.7  1998/09/02 21:28:21  gsl
//	Changed to use trace() instead of printf()
//	
//	Revision 1.6  1998-08-31 15:14:22-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/token.cpp,v
//	Working file: token.cpp
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
//	date: 1996-07-25 14:16:30-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	Renamed from token.cc to token.cpp
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 06:00:30-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:45-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:30-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:31-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
