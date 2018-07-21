/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/

/*
**	File:		proto.h
**
**	Purpose:	This file is a HACK to quickly prototype WISP.
**			Do not add to this file, remove stuff from it as it gets done properly.
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
void xtab_log(const char* fileName, int lineNum, const char* xtype, const char *tabData);

/* wisp_pic.c */
extern int WL_pic_size(char *the_pic);
extern int WL_pic_dp(char *the_pic);
extern int WL_pic_fac(char *the_pic);
/* wmalloc.c */
#include "wmalloc.h"

/* wt_cli.c */
extern int get_cli(int argc, char *argv[]);
extern void load_option_file(void);
extern void load_key_file(void);
extern void validate_options(void);

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
extern int using_ufb_test(const char *item);
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
extern int get_highest_serverity();
extern void write_log(const char* product, char serverity, const char* mess, const char *form, ...);
extern void write_tlog(TOKEN* tokptr, const char* product, char serverity, const char* mess, const char *form, ...);
#endif
int iscomment(const char* the_line);

/* wt_opcls.c */
void gen_wfopen(int col, int fnum, int open_mode);

/* wt_scrn.c */
extern NODE parse_display_ws_record(NODE next_statement);
extern NODE gen_screen_storage(NODE the_statement);
extern int gen_screens(void);
extern NODE gen_native_screen_section(NODE the_statement);
void gen_endstuff(void);
void gen_screen_paras(void);
extern int gen_label(char *newlabel);
extern int got_occurs(char *data_name, char *count);
extern int get_occurs(char *data_name, char *count);

/* wt_utils.c */
#define strpos WT_strpos
extern int strpos(const char *src, const char *srch);
extern int stredt(char *src, const char *srch, const char *repl);
extern char* make_fac(char *fac, const char *src);
extern char* make_oa(char *oa, const char *src);
extern char* make_fld(char *dst, const char* src, const char* prfx);
extern char* gen_data_name(char* dest, const char* prefix, const char* base, const char* suffix);
extern char* sp_trunc(char *text);
extern int uppercase(char *str);
extern int lowercase(char *str);
extern int paracmp(const char *str, char strtab[512][40], int tabcnt);
extern int instrlist(const char *test_str, char* strlist[], int list_cnt);
extern int noncase_eq(const char *str1, const char *str2);
extern int strchrcnt(char *string, char chr);

const char *get_prog_vname(int fnum);
const char *get_prog_lname(int fnum);
const char *get_prog_fname(int fnum);
const char *get_prog_nname(int fnum);
const char *get_prog_prname(int fnum);
const char *get_prog_status(int fnum);

void safemove(char *dest, const char *src, int len);
char *remove_quotes(char *literal);
int is_quote(char data);
int is_literal(const char* literal);

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

const char* copybookext(void);

#endif /* PROTO_H */

/*
**	History:
**	$Log: proto.h,v $
**	Revision 1.26  2005/12/02 15:22:47  gsl
**	Keep track of the highest severity level reported.
**	Ensure an non-zero exit status if severity is fatal or higher.
**	
**	Revision 1.25  2003/12/03 16:18:48  gsl
**	Fix so native screen fields and screen sections don't get generated in a copybook file.
**	
**	Revision 1.24  2003/12/02 21:23:21  gsl
**	Fix so native screen sections don't get generated in a copybook file.
**	Change generated copybooks (internal) to use same file extension rules
**	as translated copybooks. Default to .cob extension.
**	
**	Revision 1.23  2003/08/11 17:18:19  gsl
**	MF Native screens
**	
**	Revision 1.22  2003/03/17 17:22:15  gsl
**	Change to use  WFOPEN4
**	
**	Revision 1.21  2003/03/11 19:26:46  gsl
**	Add validate_options() routine which checks for option conficts.
**	
**	Revision 1.20  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.19  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.18  2002/08/12 20:13:52  gsl
**	quotes and literals
**	
**	Revision 1.17  2002/07/10 21:06:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.16  2002/07/02 21:15:40  gsl
**	Rename wstrdup
**	
**	Revision 1.15  2002/05/16 21:36:42  gsl
**	unneeded protos
**	
**	Revision 1.14  2001-09-13 09:57:20-04  gsl
**	xtab_log()
**
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
