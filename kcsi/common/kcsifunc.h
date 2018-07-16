/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		kcsifunc.h
**
**	Project:	KCSI
**
**	RCS:		$Source:$
**
**	Purpose:	Generic prototype header
**
*/

#ifndef kcsifunc_H
#define kcsifunc_H
/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#endif

#include "intdef.h"
#include "kcsio.h"
#include "dbsc.h"

/*
**	Structures and Defines
*/
#ifndef min
#define	min(x,y)	( ( (x) < (y) ) ? (x) : (y) )
#define	max(x,y)	( ( (x) > (y) ) ? (x) : (y) )
#endif /* min */

/*
**	Function Prototypes
*/
int brel_file_space(KCSIO_BLOCK *kfb);
void brel_open_shared(KCSIO_BLOCK *kfb);
void brel_open_input(KCSIO_BLOCK *kfb);
void brel_open_io(KCSIO_BLOCK *kfb);
void brel_open_output(KCSIO_BLOCK *kfb);
void brel_close(KCSIO_BLOCK *kfb);
void brel_read(KCSIO_BLOCK *kfb);
void brel_hold(KCSIO_BLOCK *kfb);
void brel_read_next(KCSIO_BLOCK *kfb);
void brel_read_previous(KCSIO_BLOCK *kfb);
void brel_hold_next(KCSIO_BLOCK *kfb);
void brel_read_keyed(KCSIO_BLOCK *kfb);
void brel_hold_keyed(KCSIO_BLOCK *kfb);
void brel_start_eq(KCSIO_BLOCK *kfb);
void brel_start_nlt(KCSIO_BLOCK *kfb);
void brel_start_gt(KCSIO_BLOCK *kfb);
void brel_start_eq_keyed(KCSIO_BLOCK *kfb);
void brel_start_nlt_keyed(KCSIO_BLOCK *kfb);
void brel_start_gt_keyed(KCSIO_BLOCK *kfb);
void brel_start_last(KCSIO_BLOCK *kfb);
void brel_write(KCSIO_BLOCK *kfb);
void brel_rewrite(KCSIO_BLOCK *kfb);
void brel_delete(KCSIO_BLOCK *kfb);
void brel_unlock(KCSIO_BLOCK *kfb);
void brel_file_info(KCSIO_BLOCK *kfb);

void bub_sort(char *base,int nel,int len,int (*comp)(),int (*swap)());
void kfberr(KFB *kfb);
void init_crid_debug(void);
void crid_func_trace(char *func,int level);
void crid_str_trace(char *str);
void KTRACE(char *str);
void crid_str(char *str);
void crid_error_trace(char *str);
void crid_func_trace_in(char *func);
void crid_func_trace_out(char *func);
void CTRLVERS(char *wvers,char *bin);
void RPTVERS(char *wvers,char *bin,char *style);
void INQVERS(char *wvers,char *bin);
void DATVERS(char *wvers,char *bin);
int use_binary();
void init_report_style(char *style);
int add_records();
void init_add_fields();
void clear_all_fields();
void clear_one_field(FIELD *fld);

void get_date_stamps();
void field_error(FIELD *fld,char *msg);
void make_error_message(char *msg);
void error_on(FIELD *fld);
void space_out(char *mem,int len);
void move_to_record();
void move_one_to_record(FIELD *fld);
void move_to_screen();
void init_dte_record();
void init_message_field();
int key_from_idx(char *idx);
void unprotect_all_fields();
void protect_all_fields();
void hide_all_fields();
void protect_nomod_fields();
void protect_key_fields();
void unprotect_idx_fields(char *idx);
int is_this_key(FIELD *fld,int key);
void fac_all_fields(int code);
void unprotect_rel_field();
void protect_rel_field();
void fac_one_field(FIELD *fld,int code);
void hide_all_prompts();
void display_all_prompts();
void display_idx_prompts(char *idx);
void fac_all_prompts(int code);
void hide_rel_prompt();
void display_rel_prompt();
void fac_one_prompt(FIELD *fld,int code);
void DBSC(char *cf_hdrs,char *cf_t1,char *cf_t2,char *io_block,char *dio_block,char *ret_code);
int dbscl(char *hdrs,char *t1,char *t2);
void bsc_load(char *hdr,char *t1,char *t2);
void bsc_bld_scr(char *hdr,FIELD *fld);
void bsc_fld_set(char *hdr,char *t1,char *t2,FIELD *fld);
void bsc_key_set(char *hdr,FIELD *fld);
void bsc_accum_set(FIELD *fld);
int set_a_key_element(FIELD *element,FIELD *key);
void bsc_fld_load(char *hdr,FIELD *fld);
int do_update(FIELD *fld);
int do_group(FIELD *fld,int *gnflds);
void bld_fld(FIELD *fld);
void get_edit_len(FIELD *fld);
void cvt_fld(char *t1,char *t2);
int set_a_key(char *hdr,FIELD *fld);
int insert_fld(FIELD *dest,FIELD *src);
void cpy_fld(FIELD *f1,FIELD *f2);
int make_fld_hole(FIELD *fld);
int kill_fld_hole(FIELD *fld);
int is_within(FIELD *outer,FIELD *inner);
int change_records(char *idx,char *mode);
int delete_records(char *idx,char *mode);
void vax_dte_globals();
int enter_the_key(char *idx,char *mode);
void read_rel_key_record();
void rel_rec_num_to_fld();
void load_first_record(char *idx);
void load_next_record(char *idx);
void load_previous_record(char *idx);
void DMNT(char *hdrs,char *t1,char *t2,char *cblock,char *dblock,char *mode,char *idx);
void start_up_code();
void exit_code();
void close_crt();
void dmntl(char *mode,char *idx);
void load_footer(char *prompt);
void add_text_prompt(int row,int col,char *prompt);
void end_main_screen();
void add_dte_scr_header();
void add_dte_logo();
void add_fld(FIELD *fld);
void add_fld_pic(FIELD *fld);
void add_prompt(int row,int col,char *prompt,char **fld,char **pfac);
void DTEDAT(char *ymd,char *mdy,char *tim,char *jul);
void DTEKEY(char *mode,char *idx);
void set_up_extra_pfs();
void set_gp_pf(uint4 *mask,int pf,int onoff);
void val_all_fields();
void init_gpint();
void GENINQ(char *gio,char *display,char *endrun,char *dio,char *change,char *option,char *cio,char *qtitle,char *qlines,char *eio);
int isnblank(char *mem,int len);
void vax_inq_globals();
void INIDIO(char *blk);
void PARSEINQ(char *rpt_rfl,char *rpt_dl,char *res_flds,char *scr_flds,char *msg,char *ret_code);
char *kstrupr(char* str);
int first_inq_tkn();
int next_inq_tkn();
char *get_inq_tkn_str(int tkn);
char *nullpad(),*wfname();
void KMATCH(char *rc,char *g1,char *g2);
void KCSIO(char *kio,char *ufb,char *recarea);
void KDISP(char* fspec,char* type, char* rc);
void KEXISTS(char *rc, char *name, char *lib, char *vol);
void ksam_file_space(KCSIO_BLOCK *kfb);
void ksam_open_shared(KCSIO_BLOCK *kfb);
void ksam_open_input(KCSIO_BLOCK *kfb);
void ksam_open_io(KCSIO_BLOCK *kfb);
void ksam_open_output(KCSIO_BLOCK *kfb);
void ksam_close(KCSIO_BLOCK *kfb);
void ksam_read(KCSIO_BLOCK *kfb);
void ksam_hold(KCSIO_BLOCK *kfb);
void ksam_read_next(KCSIO_BLOCK *kfb);
void ksam_read_previous(KCSIO_BLOCK *kfb);
void ksam_hold_next(KCSIO_BLOCK *kfb);
void ksam_read_keyed(KCSIO_BLOCK *kfb);
void ksam_hold_keyed(KCSIO_BLOCK *kfb);
void ksam_start_eq(KCSIO_BLOCK *kfb);
void ksam_start_nlt(KCSIO_BLOCK *kfb);
void ksam_start_gt(KCSIO_BLOCK *kfb);
void ksam_start_eq_keyed(KCSIO_BLOCK *kfb);
void ksam_start_nlt_keyed(KCSIO_BLOCK *kfb);
void ksam_start_gt_keyed(KCSIO_BLOCK *kfb);
void ksam_start_last(KCSIO_BLOCK *kfb);
void ksam_write(KCSIO_BLOCK *kfb);
void ksam_rewrite(KCSIO_BLOCK *kfb);
void ksam_delete(KCSIO_BLOCK *kfb);
void ksam_unlock(KCSIO_BLOCK *kfb);
void ksam_table_info(KCSIO_BLOCK *kfb);
void ksam_file_info(KCSIO_BLOCK *kfb);
int get_pic_len(char* dest,int type,int len,int dec,int bin);
void rptbld();
int read_next_rpt_record();
void strunc(char* dest);
void unstrunc(char* str,int len);
int atoilen(char* src, int len);
char	*cpsize(),*format_a_field();

int myisblank(char *str);
void vax_rpt_globals();
void rptpln(char *p_io,char *p_record,char *p_file,char *p_lib,char *p_vol);
int valspec(char *name, char *lib, char *vol);
int valspec_filled(char *name, char *lib, char *vol);
int valnam_filled(char *name);
int valvol_filled(char *name);
int val_nam(char *name);
int val_vol(char *name);
void VALNAM(char *name, char *rc);
void VALVOL(char *name, char *rc);
void VALFNM(char *name, char *rc);
int validch(int ch, char *valch);

int e_trans(int code);
void clear_keys(KFB *kfb);
int file_is_relative();
void call_lmg_log_count(long count);
void inq_write();
void set_inq_scratch(int code);
void kcsio_wfopen(int4 mode,KFB *kfb);
void ccsio(KFB *kfb,char *recarea);
void set_inq_org(int org);
void KFILTYP(char* typ);
void init_a_key(struct keydesc *k,int pos,int len,int dups);
void rpt_write(int code);
int rptsel();
int charat(char *record,char *base,int pos,int len,int type,int dec);
int fieldeq(char *record,char *base,int pos,int len,int type,int dec,char* m1);
int atoifield(char *record,char *base,int pos,int len,int type,int dec);
void tie_rpt();
void inschar(char* d, int c);
void KFORMAT(char* dec,char* rcvr,char* rflrec);
int count_lit_dec(char *str);
void rptphlp();
void full_gp_text(char *text[]);
void strfil(char* s);

void wargs(int4 count);
void clear_one_key(struct keydesc *k);

void seq_file_info(KCSIO_BLOCK *kfb);
int seq_file_space(KCSIO_BLOCK *kfb);
void seq_open_shared(KCSIO_BLOCK *kfb);
void seq_open_io(KCSIO_BLOCK *kfb);
void seq_open_input(KCSIO_BLOCK *kfb);
void seq_unlock(KCSIO_BLOCK *kfb);
void seq_delete(KCSIO_BLOCK *kfb);
void seq_rewrite(KCSIO_BLOCK *kfb);
void seq_open_output(KCSIO_BLOCK *kfb);
void seq_close(KCSIO_BLOCK *kfb);
void seq_read(KCSIO_BLOCK *kfb);
void seq_hold(KCSIO_BLOCK *kfb);
void seq_read_next(KCSIO_BLOCK *kfb);
void seq_read_previous(KCSIO_BLOCK *kfb);
void seq_hold_next(KCSIO_BLOCK *kfb);
void seq_read_keyed(KCSIO_BLOCK *kfb);
void seq_start_last(KCSIO_BLOCK *kfb);
void seq_hold_keyed(KCSIO_BLOCK *kfb);
void seq_start_eq(KCSIO_BLOCK *kfb);
void seq_start_nlt(KCSIO_BLOCK *kfb);
void seq_start_gt(KCSIO_BLOCK *kfb);
void seq_start_eq_keyed(KCSIO_BLOCK *kfb);
void seq_start_nlt_keyed(KCSIO_BLOCK *kfb);
void seq_start_gt_keyed(KCSIO_BLOCK *kfb);
void seq_write(KCSIO_BLOCK *kfb);

void gpwcsio(KFB *kfb,char *recarea);
void cr_fileio(KCSIO_BLOCK *kfb, char *io);
int ckexists(char *file, char *lib, char *vol);


/*
**	WISPLIB
*/
void wvaset(int4 *x);
void wscreen();
void wswap(void *lword);			/* swap the order of the words in a longword item (for WANG routines to use)	*/
void GETPARM();
void SCRATCH();
void wfopen(
	int4 *mode,		/* the mode of opening			*/
	char *vol,		/* the WANG volume name	(6 chars)	*/
	char *lib,		/* The WANG library name (8 chars)	*/
	char *file,		/* The file name	(8 chars)	*/
	char *name,		/* The resultant name			*/
	char *prname);		/* The PRNAME (optional).		*/
void wpload(void);
void setretcode(char* code);
void EXTRACT();
void wfclose(char* fname);								/* This routine is called after COBOL*/
int link_display(const char *file_name);
void FIND();
void SET();
void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode);
void vwang_shut(void);
int getopt( int argc, char *const argv[], const char *flags);
int initglbs(char *wisprunname);							/* Init GLOBALS				*/
void setwispfilext(char* wispfilext);

#endif /* kcsifunc_H */

/*
**	History:
**	$Log: kcsifunc.h,v $
**	Revision 1.10  2000-03-13 14:13:33-05  gsl
**	Change strupr() to kstrupr()
**
**	Revision 1.9  1997-10-30 15:55:12-05  scass
**	Corrected type.
**
**	Revision 1.8  1997-10-23 13:58:14-04  gsl
**	change to use link_display
**
**	Revision 1.7  1997-10-02 15:33:41-04  gsl
**	fix warnings
**
**	Revision 1.6  1997-02-17 13:05:15-05  gsl
**	fix getopt() prototype
**
**	Revision 1.5  1996-10-09 19:49:41-04  gsl
**	add prototype for setwispfilext()
**
**	Revision 1.4  1996-10-02 15:10:01-07  gsl
**	Add getopt() and initglbs() prototypes to WISPLIB section
**
**	Revision 1.3  1996-10-02 09:13:59-07  gsl
**	Add prototypes
**
**	Revision 1.2  1996-09-24 17:32:30-07  gsl
**	Fix warning
**
**	Revision 1.1  1996-09-17 16:35:08-07  gsl
**	Initial revision
**
**
**
*/
