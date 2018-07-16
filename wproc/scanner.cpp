// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : scanner.cpp
// Author : George Soules
// Date   : 30 January 1991

#if RUNTIME
#else

// Specification
#include "scanner.hpp"

// Classes
#include "options.hpp"

// Definitions and subprograms
#include <ctype.h>
#include "debugaid.hpp"


enum state_type {
   state_arrow_left,
   state_arrow_right,
   state_comment,
   state_concat,
   state_decision,
   state_delimiter,
   state_eof,
   state_final,
   state_identifier,
   state_integer,
   state_label,
   state_operator,
   state_other,
   state_pathname,
   state_quote_double,
   state_quote_single,
   state_space,
   state_start,
   state_string,
   state_variable
};

scanner::scanner(reader *a_reader) {
   trace_begin(object, "scanner");
   the_reader = a_reader;
#if WANG
   lookahead_token_1 = NULL;
   lookahead_token_2 = NULL;
#endif
   trace_end(object);
}


scanner::~scanner() {
#if WANG
   if (lookahead_token_1)
      delete lookahead_token_1;
   if (lookahead_token_2)
      delete lookahead_token_2;
#endif
   trace(object, "~scanner");
}


token *scanner::next_token() {
#if WANG
   token *t;

   if (lookahead_token_2) {
      assert(lookahead_token_1);
      t = lookahead_token_1;
      lookahead_token_1 = lookahead_token_2;
      lookahead_token_2 = NULL;
      return t;
   }
   else if (lookahead_token_1) {
      t = lookahead_token_1;
      lookahead_token_1 = NULL;
      return t;
   }
   else
      return screen_token();

#else
   return screen_token();
#endif
}


token *scanner::screen_token() {
   token *t = scan_token();

   // Screen selected token types
   switch (t->kind()) {
      case tk_label:
         if (t->lexeme_size() > 32)  /* don't count ':' as part of 31 */
            t->set_error(28);
         break;
      case tk_variable:
         if (t->lexeme_size() == 1 || t->lexeme_size() > 31)
            t->set_error(34);
         break;
      default:
         break;
   }
   return t;
}


#if WANG
token *scanner::lookahead(int how_far) {
   trace_si(scanner, "lookahead ", how_far);
   assert(how_far >= 1 && how_far <= 2);
   if (lookahead_token_1 == NULL) {
      lookahead_token_1 = screen_token();
      assert(lookahead_token_2 == NULL);
   }
   if (how_far == 1)
      return lookahead_token_1;
   if (lookahead_token_2 == NULL)
      lookahead_token_2 = screen_token();
   return lookahead_token_2;
}
#endif


int is_id_char(int c)
{
   return(isalpha(c) || c == '@' || c == '$' || c == '#' || c == '_');
}


state_type determine_1st_state(int c)
{
   if (isalpha(c)) return(state_identifier);
   if (isdigit(c)) return(state_decision);

   switch (c) {
      case '<':
         return(state_arrow_left);
      case '>':
         return(state_arrow_right);
      case '[':
         return(state_comment);
      case '!':
         return(state_concat);
      case ',':
      case ':':
      case ';':
      case '(':
      case ')':
         return(state_delimiter);
      case EOF:
         return(state_eof);
      case '-':
      case '*':
      case '/':
      case '+':
      case '=':
         return(state_operator);
      case '?':
         return(state_pathname);
      case '.':
#if WANG
         return(state_delimiter);
      case '$':
      case '#':
      case '@':
         return(state_identifier);
#else
         return(state_pathname);
#endif
      case '\\':
#if DOS
         return(state_pathname);
#else
         return(state_other);
#endif
      case '"':
         return(state_quote_double);
      case '\'':
         return(state_quote_single);
      case ' ':
      case '\n':
      case '\t':
         return(state_space);
      case '&':
         return(state_variable);
      default:
         return(state_other);
   }
}


token *scanner::scan_token()
{
#define MAX_LEXEME_SIZE	1024
   char       lexeme[MAX_LEXEME_SIZE+1];
   token_kind kind;
   usign_16   row;
   usign_16   column;
   usign_8    error_number = 0;

   static struct {
      usign_16   row;
      usign_16   column;
      token_kind kind;
   } last_position;

   static int  c;
   char        closing_quote;
   int         text_index;
   state_type  state;

   #define append_to_token(c)  lexeme[text_index++] = (char)c

   c = the_reader->next_char();

   text_index = 0;
   state = state_start;

   while (state != state_final) {
      switch (state) {
         case state_arrow_left:
            append_to_token(c);
            switch(c = the_reader->next_char()) {
               case '=':
                  append_to_token(c);
                  kind = tk_le;
                  break;
               case '>':
                  append_to_token(c);
                  kind = tk_ne;
                  break;
               default:
                  kind = tk_lt;
                  the_reader->unread_last_char();
                  break;
            }
            state = state_final;
            break;

         case state_arrow_right:
            append_to_token(c);
            switch(c = the_reader->next_char()) {
               case '=':
                  append_to_token(c);
                  kind = tk_ge;
                  break;
               default:
                  kind = tk_gt;
                  the_reader->unread_last_char();
                  break;
            }
            state = state_final;
            break;

         case state_comment:
            {
               int      nesting_level     = 1;
               usign_16 comment_start_row = the_reader->current_row();
               usign_16 comment_start_col = the_reader->current_column();

               do {
                  switch(c = the_reader->next_char()) {
                     case ']':
                        nesting_level -= 1;
                        assert(nesting_level >= 0);
                        break;
                     case '[':
                        nesting_level += 1;
                        break;
                     case EOF:
                        nesting_level = 0;
                        error_number = 42;
                        last_position.row    = comment_start_row;
                        last_position.column = comment_start_col;
                        the_reader->unread_last_char();
                        break;
                     default:
                        break;
                  }
               } while (nesting_level > 0 && c != EOF);
               c = the_reader->next_char();
               state = state_start;
               break;
            }

         case state_concat:
            append_to_token(c);
            if ((c = the_reader->next_char()) == '!') {
               append_to_token(c);
               kind = tk_concat;
            }
            else {
               kind = tk_other;
               the_reader->unread_last_char();
            }
            state = state_final;
            break;

         case state_decision:
            append_to_token(c);
            c = the_reader->next_char();
            while (isdigit(c)) {
               append_to_token(c);
               c = the_reader->next_char();
            }
            if (is_id_char(c))
               state = state_identifier;
#if ! WANG
            else if (c == ':')
               state = state_label;
#endif
            else {
               the_reader->unread_last_char();
               state = state_integer;
            }
            break;

         case state_delimiter:
            append_to_token(c);
            switch(c) {
               case ',':
                  kind = tk_comma;
                  break;
               case ':':
                  kind = tk_colon;
                  break;
               case ';':
                  kind = tk_semicolon;
                  break;
               case '(':
                  kind = tk_parenleft;
                  break;
               case ')':
                  kind = tk_parenright;
                  break;
#if WANG
               case '.':
                  kind = tk_dot;
                  break;
#endif
            }
            state = state_final;
            break;

         case state_eof:
            append_to_token(c);
            kind = tk_eof;
            row = last_position.row;
            column = last_position.column;
            if (last_position.kind == tk_string)
               column += 2;
            state = state_final;
            break;

         case state_identifier:
            append_to_token(c);
            c = the_reader->next_char();
            if (is_id_char(c) || isdigit(c))
               state = state_identifier;
            else if (c == ':')
               state = state_label;
#if WANG
            // No concept of pathname for Wang
#else
#if MSFS
            else if (c == '\\' || c == '.' || c == '*' || c == '?')
               state = state_pathname;
#else
#if UNIX
            else if (c == '/' || c == '.' || c == '*' || c == '?')
               state = state_pathname;
#endif
#endif
#endif
            else {
               the_reader->unread_last_char();
               state = state_final;
               kind = tk_identifier;
            }
            break;

         case state_integer:
            kind = tk_integer;
            state = state_final;
            break;

         case state_label:
            append_to_token(c);
            kind = tk_label;
            state = state_final;
            break;

         case state_operator:
            append_to_token(c);
            switch(c) {
               case '-':
                  kind = tk_minus;
                  break;
               case '+':
                  kind = tk_plus;
                  break;
               case '*':
                  kind = tk_mult;
                  break;
               case '/':
                  kind = tk_div;
                  break;
               case '=':
                  kind = tk_eq;
                  break;
               default:
                  assert(UNREACHABLE);
                  break;
            }
            state = state_final;
            break;

         case state_other:
            if (isspace(c))
               state = state_space;
            else {
               append_to_token(c);
               kind = tk_other;
               state = state_final;
            }
            break;

         case state_pathname:
            append_to_token(c);
            c = the_reader->next_char();
            if (is_id_char(c) || isdigit(c) || c == '\\' ||
                c == '.' || c == '*' || c == '?')
               state = state_pathname;
            else {
               the_reader->unread_last_char();
               state = state_final;
               kind = tk_pathname;
            }
            break;

         case state_quote_double:
            closing_quote = '"';
            state = state_string;
            break;

         case state_quote_single:
            closing_quote = '\'';
            state = state_string;
            break;

         case state_space:
            while (isspace(c = the_reader->next_char()));
            state = state_start;
            break;

         case state_start:
            row    = the_reader->current_row();
            column = the_reader->current_column();
            state = determine_1st_state(c);
            break;

         case state_string: {
	    Boolean ignore_comments = the_reader->ignoring_line_comments(); //FIX002
	    the_reader->ignore_line_comments(false);

            while (true) {
               while ((c = the_reader->next_char()) != closing_quote) {
                  if (c == EOF || text_index >= MAX_LEXEME_SIZE - 1) {
                     error_number = 43;
                     break;
                  }
		  if ('\n' != c) 
		  {
			  /* 
			  ** Don't append the newline on a continued string
			  */
			  append_to_token(c);
		  }
		  
               }
	       if (error_number)
		  break;

               // Check for imbedded quote
               c = the_reader->next_char();
               if (c == closing_quote)
                  append_to_token(c);
               else {
                  the_reader->unread_last_char();
                  break;
               }
            }
	    the_reader->ignore_line_comments(ignore_comments); // FIX002
            state = state_final;
            kind = tk_string;
            break;
	 }
         case state_variable:
            append_to_token(c);
            c = the_reader->next_char();
            if (is_id_char(c) || isdigit(c))
               state = state_variable;
            else {
               the_reader->unread_last_char();
               kind = tk_variable;
               state = state_final;
            }
            break;

         default:
            assert(UNREACHABLE);
            break;
      }
   }
   append_to_token('\0');

   token *t;
   t = new token(kind, row, column, lexeme, error_number);
   last_position.row = row;
   last_position.column = column + text_index - 1;
   last_position.kind = kind;

#if DEBUG
   if (user_options.debug_trace_scanner())
      t->dump();
#endif

   return t;
}

void scanner::ignore_rest_of_line()
{
	int c;
	int cnt = 0;
	// Consume the rest of the line
	while ((c = the_reader->next_char()) != EOF && c != '\n')
	{
		cnt++;
	}
   	trace_si(scanner, "ignore_rest_of_line (chars) ", cnt);
}
#endif
