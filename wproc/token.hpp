//
//	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
//
//	Project:	WPROC
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : token.hpp
// Author : George Soules
// Date   : 28 January 1991

#ifndef TOKEN__HPP
#define TOKEN__HPP

// Classes
#include "object.hpp"

// Definitions and subprograms
#include <string.h>
#include "environ.hpp"
#include "utility.hpp"


enum token_kind {
   tk_colon,
   tk_comma,
   tk_concat,
   tk_delimiter,
   tk_div,
#if WANG
   tk_dot,
#endif
   tk_eof,
   tk_eq,
   tk_ge,
   tk_gt,
   tk_identifier,
   tk_integer,
   tk_label,
   tk_le,
   tk_lt,
   tk_minus,
   tk_mult,
   tk_ne,
   tk_other,
   tk_parenleft,
   tk_parenright,
   tk_pathname,
   tk_plus,
   tk_semicolon,
   tk_string,
   tk_undefined,
   tk_variable
};


class token : public object {
   public:
      token(token &a_token);
      token(token &token1, token &token2);

      token(
         token_kind a_kind,
         usign_16   a_row,
         usign_16   a_column,
         char      *a_lexeme,
         usign_8    an_error_number = 0);

      ~token();
      token_kind kind()         {return the_kind;}
      int        lexeme_size()  {return the_lexeme_size;}
      char      *lexeme()       {return the_lexeme;}
      usign_16   row()          {return the_row;}
      usign_16   column()       {return the_column;}
      usign_16   last_column();
      usign_8    error_number() {return the_error_number;}
      token     *next_token()   {return next;}
      void       set_next_token(token* a_token) {next = a_token;}
      void       set_error(int error_number) {the_error_number = error_number;}
#if WANG
      void       shift_to_upper_case() {upper_case(the_lexeme);}
#endif
      void       dump() const;
   private:
      token_kind the_kind;
      int        the_lexeme_size;
      char      *the_lexeme;
      usign_16   the_row;
      usign_16   the_column;
      usign_8    the_error_number;
      token     *next;
};

#endif

//
//	History:
//	$Log: token.hpp,v $
//	Revision 1.6  2003/02/11 19:05:27  gsl
//	Remove unneeded #ifdef's for DEBUG
//	
//	Revision 1.5  1998/08/31 19:14:23  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/token.hpp,v
//	Working file: token.hpp
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
//	date: 1995-04-25 06:00:30-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:45-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:31-05;  author: gsl;  state: V3_3x12;  lines: +0 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:31-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
