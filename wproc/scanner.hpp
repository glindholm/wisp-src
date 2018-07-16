// Copyright (c) Lexical Software, 1991.  All rights reserved.
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
