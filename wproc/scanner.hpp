//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : parser.hpp
// Author : George Soules
// Date   : 30 January 1991

#if RUNTIME
#else

#ifndef SCANNER__HPP
#define SCANNER__HPP

// Classes
#include "object.hpp"
#include "reader.hpp"
#include "token.hpp"

// Definitions and subprograms
// (none)


class scanner : public object {
   public:
      scanner(reader *a_reader);
      ~scanner();
      token  *next_token();
#if WANG
      token  *lookahead(int how_far = 1);
#endif
      void ignore_rest_of_line();  //FIX004

   private:
      token  *screen_token();
      token  *scan_token();
      reader *the_reader;
#if WANG
      token  *lookahead_token_1;
      token  *lookahead_token_2;
#endif
};

#endif
#endif

//
//	History:
//	$Log: scanner.hpp,v $
//	Revision 1.5  1998/08/31 19:14:13  gsl
//	drcs update
//	
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/scanner.hpp,v
//	Working file: scanner.hpp
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
//	date: 1995-04-25 06:00:21-04;  author: gsl;  state: V4_3_00;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:37-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:33:20-05;  author: gsl;  state: V3_3x12;  lines: +2 -0
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:25-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
