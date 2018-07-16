/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
**	File:		proto.h
**
**	Purpose:	This file is a HACK to quickly prototype WISP.
**			Do not add to this file, remove stuff from it as it gets done properly.
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef PROTO_H
#define PROTO_H

#include "idsistd.h"
#include "token.h"
#include "node.h"
#include "wispfile.h"

/* untabstr.c */
extern int untabstr(char *str, int maxsize);
/* wisp.c */
extern void need_to_rerun(const char* reason);
extern void delayed_exit_with_err(void);
extern void exit_with_err(void);
extern int exit_wisp(int err);
extern int wisp_exit_para(void);
extern void d_wisp_exit_para(void);
extern int delete(const char *path);
extern char *packed_decimal(void);

/* wisp_pic.c */
extern int pic_size(char *the_pic);
extern int pic_dp(char *the_pic);
extern int pic_fac(char *the_pic);
extern uint4 pic_edit(char *the_pic);
extern uint4 pic_zmask(char *the_pic);
/* wmalloc.c */
#include "wmalloc.h"

/* wt_cli.c */
extern int get_cli(int argc, char *argv[]);

/* wt_debug.c */
extern int debug_set_level(char *levels);
extern int debug_level(int level);
extern int debug3_print_token(TOKEN *tokptr);
extern int debug1_print_line(char *the_line);
extern int debug2_print_line(char *the_line);
extern int debug3_print_line(char *the_line, int absline, int linenum, cob_file_context *the_context);
extern int debug4_print_statement(NODE the_statement);
extern int debug_print_tree(NODE the_tree, int depth);
/* wt_decl.c */
NODE chk_dpar(NODE the_sentence);
NODE parse_use(NODE the_section);
void gen_dexit(void);
/* wt_divs.c */
extern int check_decl(void);
extern int new_para(void);
extern int check_section(void);
extern int g_wfilechk(int i);
extern int gen_defdecl(void);
extern int fd_check(void);
extern int nopunct(char *dst, char *src);
extern int check_proc_div(void);
/* wt_files.c */
extern int open_io(void);
extern cob_file_context *open_cob_context(char *infile, char *outfile);
extern int writing_cob_main(void);
extern int writing_copybook(void);
extern int reading_copybook(void);
extern char *context_infile_name(cob_file_context *context);
extern char *context_outfile_name(cob_file_context *context);
extern int context_infile_count(cob_file_context *context);
extern int curr_context_infile_count(void);
extern cob_file_context *get_curr_cob_context(void);
extern int copy_file(char *the_file);
extern int rcpy_compare(struct rcpy_struct *p1, struct rcpy_struct *p2);
extern int trimname(char *filename);
extern int using_ufb_test(char *item);
extern char *wfgets(char *s, int n, FILE *stream);
extern cob_file *open_cob_file(char *filename, int openmode, int copybook);
extern int close_cob_file(cob_file *cob_file_ptr);
extern int close_all_cob_files(void);
extern int dbfile_add_item(char *select_name, char *table_name);
extern struct dbfile_item_struct *get_dbfile_item(char *select_name);
extern int is_dbfile(char *select_name);
extern void dbfile_write_mark(char *select_name);

/* wt_ident.c */
extern NODE identification_division(NODE the_statement);
extern NODE environment_division(NODE the_statement);
extern NODE configuration_section(NODE the_statement);
extern int set_dpcomma(void);
extern int is_dpcomma(void);
extern int isafigcon1(const char *item);
extern void init_figcons(void);
extern void finish_figcons(void);

/* wt_input.c */
extern NODE input_output_section(NODE the_statement);
extern NODE file_control_para(NODE the_statement);
extern NODE io_control_para(NODE the_statement);
extern NODE parse_selects(NODE next_statement);
extern int key_name(char *the_name, const char *the_qual);
extern int crt_index(const char *name);
extern int file_index(const char *name);
extern int is_file_status(const char *dataname);

/* wt_io.c */
#ifndef WRITE_LOG
extern void write_log(const char* product, char serverity, const char* mess, const char *form, ...);
extern void write_tlog(TOKEN* tokptr, const char* product, char serverity, const char* mess, const char *form, ...);
#endif
int iscomment(const char* the_line);

/* wt_scrn.c */
extern NODE parse_display_ws_record(NODE next_statement);
extern void gen_screen_storage(void);
extern int gen_screens(void);
extern int gen_acn_screens(void);
extern int gen_acn_facs(void);
void gen_endstuff(void);
void gen_screen_paras(void);
extern int gen_label(char *newlabel);
extern int got_occurs(char *data_name, char *count);
extern int get_occurs(char *data_name, char *count);

/* wt_utils.c */
extern int strpos(const char *src, const char *srch);
extern int stredt(char *src, const char *srch, const char *repl);
extern char* make_fac(char *fac, const char *src);
extern char* make_oa(char *oa, const char *src);
extern char* make_fld(char *dst, const char* src, const char* prfx);
extern char* gen_data_name(char* dest, const char* prefix, const char* base, const char* suffix);
extern char* sp_trunc(char *text);
extern int uppercase(char *str);
extern int lowercase(char *str);
extern int paracmp(char *str, char strtab[512][40], int tabcnt);
extern int noncase_eq(const char *str1, const char *str2);
extern int strchrcnt(char *string, char chr);

const char *get_prog_vname(int fnum);
const char *get_prog_lname(int fnum);
const char *get_prog_fname(int fnum);
const char *get_prog_nname(int fnum);
const char *get_prog_prname(int fnum);
const char *get_prog_status(int fnum);

/* wt_wsdat.c */
extern int ws_init(void);
/* wt_wsdiv.c */
extern NODE working_storage_section(NODE the_statement);
extern NODE linkage_section(NODE the_statement);
extern NODE parse_data_description(NODE next_statement);
extern char *next_filler(void);

/* --- */
extern int increment_in_line_count(void);
extern int increment_out_line_count(void);
extern int increment_comment_line_count(void);
extern int in_line_count(void);
extern int out_line_count(void);
extern int comment_line_count(void);

extern void gen_data_conv(void);


#endif /* PROTO_H */

/*
**	History:
**	$Log: proto.h,v $
**	Revision 1.13  1999-09-07 10:33:17-04  gsl
**	Remove unneeded prototypes and fix.
**
**	Revision 1.12  1998-06-09 10:08:06-04  gsl
**	removed a lot of old routines
**
**	Revision 1.11  1998-03-23 13:55:38-05  gsl
**	update
**
**	Revision 1.10  1998-03-03 16:06:57-05  gsl
**	updated
**
**	Revision 1.9  1997-09-09 17:53:11-04  gsl
**	Add ACUCOBOL NATIVE SCREENS stuff
**
**	Revision 1.8  1997-02-18 10:25:11-05  gsl
**	Fix prototypes
**
**	Revision 1.7  1996-08-30 21:56:08-04  gsl
**	drcs update
**
**
**
*/
