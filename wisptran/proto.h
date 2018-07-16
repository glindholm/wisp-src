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
extern int need_to_rerun(char *reason);
extern int delayed_exit_with_err(void);
extern int exit_with_err(void);
extern int exit_wisp(int err);
extern int wisp_exit_para(void);
extern int d_wisp_exit_para(void);
extern int delete(char *path);
extern char *packed_decimal(void);
/* wisp_pic.c */
extern int pic_size(char *the_pic);
extern int pic_dp(char *the_pic);
extern int pic_fac(char *the_pic);
extern uint4 pic_edit(char *the_pic);
extern uint4 pic_zmask(char *the_pic);
/* wmalloc.c */
#include "wmalloc.h"
/* wt_acept.c */
extern int p_accept(void);
/* wt_call.c */
extern int p_call(void);
/* wt_cli.c */
extern int get_cli(int argc, char *argv[]);
extern int whodunit(void);
extern int printusage(void);
extern int fullusage(void);
extern int showflags(void);
/* wt_crtrw.c */
extern int crt_rewrite(int crt_num);
/* wt_datad.c */
extern NODE data_division(NODE the_statement);
extern NODE file_section(NODE the_statement);
extern int add_to_record_list(char *recname);
extern int fd_record(char *recname);
extern NODE parse_crt_records(void);
extern NODE delete_fd(NODE the_statement);
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
extern int chk_dpar(void);
extern int p_use(void);
extern int gen_use(char *reason, char *filename);
extern int gen_dexit(void);
/* wt_delet.c */
extern int p_delete(void);
/* wt_divs.c */
extern int check_decl(void);
extern int new_para(void);
extern int check_section(void);
extern int g_wfilechk(int i, char *end_str);
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
extern int p_optfile(void);
extern int p_keyfile(void);
extern int p_workfile(void);
extern int rcpy_compare(struct rcpy_struct *p1, struct rcpy_struct *p2);
extern int chk_rlist(void);
extern int set_rlist(char *filename);
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
/* wt_free.c */
extern int p_free(void);
/* wt_ident.c */
extern NODE identification_division(NODE the_statement);
extern NODE environment_division(NODE the_statement);
extern NODE configuration_section(NODE the_statement);
extern int set_dpcomma(void);
extern int is_dpcomma(void);
extern int isafigcon1(char *item);
extern int init_figcons(void);
extern int finish_figcons(void);
/* wt_if.c */
extern int parse_if(NODE the_statement);
/* wt_input.c */
extern NODE input_output_section(NODE the_statement);
extern NODE file_control_para(NODE the_statement);
extern NODE io_control_para(NODE the_statement);
extern NODE parse_selects(NODE next_statement);
extern int key_name(char *the_name, char *the_qual);
extern int chk_sort(char *fname);
extern int gen_io_control(void);
extern int crt_index(char *name);
extern int file_index(char *name);
extern int is_file_status(char *dataname);
/* wt_io.c */
extern void hold_line(void);
extern int get_line(void);
extern int p_left(int pnum, int p_off);
extern int p_left_1(int pnum, int p_off);
extern int extract_param(char the_parm[]);
extern int invalidate_parms(void);
extern int get_param(char the_parm[]);
extern int get_ppos(void);
extern int next_param(char the_parm[]);
extern int peek_param(char the_parm[]);
extern int skip_param(int num_parms);
extern int write_log();
extern int write_tlog();
extern int iscomment(char *the_line);
/* wt_locks.c */
extern int gen_unlocks(void);
extern int gen_wisp_free_all(void);
extern int set_lock(int fnum, int indent);
extern int preclose_locking(int filenum);
extern int predelete_locking(void);
extern int prerewrite_locking(void);
extern int clear_locking(void);
/* wt_opcls.c */
extern int p_open(void);
extern int p_close(void);
/* wt_read.c */
extern int p_read(void);
extern int add2buff(char *buff, char *addstr, int col, int newline);
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
extern void make_color(char* the_color, char* src);
extern void make_control(char* the_ctrl, char* src);

/* wt_sort.c */
extern int p_sort(void);
extern int s_wfopen(int fnum, int fmode);
/* wt_start.c */
extern int parse_start(NODE the_statement);
/* wt_utils.c */
extern int strpos(char *src, char *srch);
extern int stredt(char *src, char *srch, char *repl);
extern int make_fac(char *fac, char *src);
extern int make_oa(char *oa, char *src);
extern int make_fld(char *dst, char *src, char *prfx);
extern int trim(char string[]);
extern int strlast(char *string, char srch);
extern int sp_trunc(char *text);
extern int wsqueeze(char *the_line, int length);
extern int uppercase(char *str);
extern int lowercase(char *str);
extern int paracmp(char *str, char strtab[512][40], int tabcnt);
extern int noncase_eq(char *str1, char *str2);
extern int strchrcnt(char *string, char chr);
/* wt_write.c */
extern int p_write(void);
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
