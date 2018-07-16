// Copyright (c) Lexical Software, 1991.  All rights reserved.
//
// Module : compiler.hpp
// Author : George Soules
// Date   : 18 February 1991

#if RUNTIME
#else

#ifndef COMPILER__HPP
#define COMPILER__HPP

// Classes
#include "fixup.hpp"
#include "object.hpp"
#include "pcode.hpp"
#include "range.hpp"
#include "scanner.hpp"
#include "scope.hpp"
#include "symbols.hpp"

// Definitions and subprograms
#include <limits.h>
#include <string.h>
#include "report.hpp"
#include "utility.hpp"


class compiler : public object {
   public:
      compiler(
         reader        *a_reader,
         pcode_emitter *an_emitter,
         symbol_table  *a_symbol_table,
         char          *a_source_name);
      ~compiler();
      void    compile_statements(int_32 how_many = INT_MAX);
      int     errors()   {return error_count;};
      int     warnings() {return warning_count;};

      // Statement parsers
      void    assign_stmt();
      void    block_stmt();
      void    call_stmt();
      void    call_builtin();
      void    case_stmt();
      void    close_stmt();
      void    declare_stmt();
      void    destroy_stmt();
      void    end_stmt();
      void    exit_stmt();
      void    for_stmt();
      void    goto_stmt();
      void    if_stmt();
      void    interpret_stmt();
      void    loop_stmt();
#if WANG
      void    logoff_stmt();
#endif
      void    leave_stmt();
      void    message_stmt();
      void    open_stmt();
#if ! WANG
      void    print_stmt();
#endif
      void    procedure_stmt();
      void    prompt_stmt();
#if ! WANG
      void    protect_stmt();
#endif
      void    read_stmt();
      void    rename_stmt();
      void    return_stmt();
      void    run_stmt();
      void    scratch_stmt();
      void    screen_stmt();
#if ! WANG
      void    set_stmt();
#endif
      void    subroutine_stmt();
      void    system_stmt();
      void    trace_stmt();
      void    using_stmt();
      void    while_stmt();
      void    write_stmt();

#if WANG
      void    delacl_stmt();
      void    dismount_stmt();
      void    extracl_stmt();
      void    extract_stmt();
      void    mount_stmt();
      void    operator_stmt();
      void    options_stmt();
      void    print_stmt();
      void    protect_stmt();
      void    set_stmt();
      void    setacl_stmt();
      void    submit_stmt();
      void    wang_trace_stmt();
#endif

   private:
      scanner       *the_scanner;
      pcode_emitter *the_emitter;
      symbol_table  *the_symbol_table;
      scope         *the_scope;
      char          *the_source_name;
      token         *the_token;
      token         *token_list_head;
      token         *token_list_tail;
      int            token_list_size;
      Boolean        in_error_state;
      int            in_block;
      int            in_subroutine;
      int            exits_allowed;
      int            error_count;
      int            warning_count;
      usign_16       stmt_count;
      fixup          fixup_chain;
      fixup          subroutine_fixup_chain;
      Boolean        first_compiler;

      // Parser support routines
      void      init_parsers();
      void      emit_range_of(token *a_token);
      void      syntax_error(int error_number);
      void      semantic_error(int error_number);
      void      semantic_warning(int error_number);
      void      save_range_start(range *a_range, token *a_token);
      void      get_next_token();
      void      discard();
      void      keep();
      void      delete_token_list();
      void      recover_from_error();
      void      report_bad_refs();

      // Expression nonterminal parsers
      void    expression(int error_number);
      void    simple_expression(int error_number);
      void    logical();
      void    relation();
      void    catenation();
      void    binary_add_subtract();
      void    binary_multiply_divide();
      void    unary_plus_minus();
      void    primary();
      void    parens();
      Boolean parse_attribute_condition();
      Boolean parse_color();
      Boolean parse_color_list(int &count);
      int     parse_comma_list();
      Boolean parse_corners_list(int &count);
      Boolean parse_declaration(Boolean declare_statement);
      Boolean parse_filename();
#if WANG
      Boolean parse_cobol_keyword();
      Boolean parse_char_option(int error, char first, char last);
      Boolean parse_keyword_option(
         int error,
         const char *s1,
         const char *s2 = NULL,
         const char *s3 = NULL);
      Boolean keyword_matches(
         const char *s1,
         const char *s2,
         const char *s3 = NULL,
         const char *s4 = NULL,
         const char *s5 = NULL);
      Boolean parse_file_component(
         Boolean allow_fbr,
         Boolean &is_fbr,
         int     size,
         int     size_warning,
         int     component_error);
      Boolean parse_libname(Boolean allow_fbr = true);
      Boolean parse_volname(Boolean allow_fbr = true);
      void    parse_full_backward_ref();
      Boolean is_full_backward_ref();
      Boolean parse_if_exists();
      Boolean parse_display_enter();
#endif
      Boolean parse_fkey_list(int &count);
      Boolean parse_identifier();
      int     parse_parameters(symbol *a_symbol);
      int     parse_variable(symbol *a_symbol, range &a_range);
      Boolean parse_yes_no();
      void    emit_string(token *a_token = NULL);

      struct {
         usign_16 row;
         usign_8  last_column;
      } previous_token;

      Boolean stmt();
      void end_error(token *a_token);
      Boolean statements(token *a_token);
      Boolean statements_in_case(token *a_token);
      Boolean parse_screen_field(opcode an_opcode);
      Boolean parse_screen_line(opcode an_opcode);
      void    parse_screen(opcode an_opcode);

      // Other support routines
      symbol *declare_label(const char *a_name, Boolean is_subroutine);
      symbol *lookup_or_create_symbol(char *a_name);

      Boolean label_accessible(
         block    a_block,
         scope   *a_scope,
         Boolean  is_subroutine,
         Boolean  called,
         Boolean  issue_error,
         token   &a_token);

      void resolve_label_offset(
         offset  an_offset,
         token  &a_token,
         Boolean called);

      void    resolve_label_fixups();
#if WANG
      Boolean resolve_static_label(fixup *a_fixup);
#endif
      void emit_labels_accessible_from(
         scope     *a_scope,
         Boolean    called,
         Boolean    source_order,
         symbol_id &boundary);

      void    p_scrnio_cleanup();
      void    p_os_cleanup();
#if WANG
      void    p_wang_cleanup();
#endif
};


// Macros (to minimize syntax within the parsing routines)

#define kind_is(k)                               \
   (the_token->kind() == k)

#define kind_is_expression()                     \
   (kind_is(tk_integer)    ||                    \
    kind_is(tk_string)     ||                    \
    kind_is(tk_variable)   ||                    \
    kind_is(tk_parenleft))

#define lexeme_is(s)                             \
    (same_string(the_token->lexeme(), s))

#define keyword_is(s)                            \
   (kind_is(tk_identifier) &&                    \
    same_string(the_token->lexeme(), s))

#define require_syntax(s, i)                     \
   if (! same_string(the_token->lexeme(), s)) {  \
      syntax_error(i);                           \
      return;                                    \
   }

#define improper_syntax(i)                       \
   {                                             \
   syntax_error(i);                              \
   return;                                       \
   }

#define allow_syntax(s)                          \
   if (same_string(the_token->lexeme(), s)) {    \
      discard();                                 \
   }

#define require_kind(k, i)                       \
   if (! kind_is(k)) {                           \
      syntax_error(i);                           \
      return;                                    \
   }

#define require_semantics(c, i)                  \
   if (!(c)) {                                   \
      semantic_error(i);                         \
      return;                                    \
   }

#define require_nonterminal(n)                   \
   {                                             \
      n;                                         \
      if (in_error_state) {                      \
         return;                                 \
      }                                          \
   }

#endif
#endif
