/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
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
#include "kcsit.h"

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

int  rel_file_space(KCSIO_BLOCK *kfb);
void rel_open_shared(KCSIO_BLOCK *kfb);
void rel_open_input(KCSIO_BLOCK *kfb);
void rel_open_io(KCSIO_BLOCK *kfb);
void rel_open_output(KCSIO_BLOCK *kfb);
void rel_close(KCSIO_BLOCK *kfb);
void rel_read(KCSIO_BLOCK *kfb);
void rel_hold(KCSIO_BLOCK *kfb);
void rel_read_next(KCSIO_BLOCK *kfb);
void rel_read_previous(KCSIO_BLOCK *kfb);
void rel_hold_next(KCSIO_BLOCK *kfb);
void rel_read_keyed(KCSIO_BLOCK *kfb);
void rel_hold_keyed(KCSIO_BLOCK *kfb);
void rel_start_eq(KCSIO_BLOCK *kfb);
void rel_start_nlt(KCSIO_BLOCK *kfb);
void rel_start_gt(KCSIO_BLOCK *kfb);
void rel_start_eq_keyed(KCSIO_BLOCK *kfb);
void rel_start_nlt_keyed(KCSIO_BLOCK *kfb);
void rel_start_gt_keyed(KCSIO_BLOCK *kfb);
void rel_start_last(KCSIO_BLOCK *kfb);
void rel_write(KCSIO_BLOCK *kfb);
void rel_rewrite(KCSIO_BLOCK *kfb);
void rel_delete(KCSIO_BLOCK *kfb);
void rel_unlock(KCSIO_BLOCK *kfb);
void rel_file_info(KCSIO_BLOCK *kfb);


void KCSI_bub_sort(char *base,int nel,int len,int (*comp)(),int (*swap)());
void KCSI_kfberr(KFB *kfb);
void init_crid_debug(void);
void KTRACE(char *str);
void crid_func_trace_in(char *func);
void crid_func_trace_out(char *func);
void CTRLVERS(char *wvers,char *bin);
void RPTVERS(char *wvers,char *bin,char *style);
void INQVERS(char *wvers,char *bin);
void DATVERS(char *wvers,char *bin);
int  KCSI_use_binary();
void KCSI_init_report_style(char *style);
int  dte_add_records();
void KCSI_init_add_fields();
void KCSI_clear_all_fields();
void KCSI_clear_one_field(FIELD *fld);

void KCSI_get_date_stamps();
void KCSI_field_error(FIELD *fld,char *msg);
void KCSI_make_error_message(char *msg);
void dte_space_out(char *mem,int len);
void dte_move_to_record();
void dte_move_one_to_record(FIELD *fld);
void dte_move_to_screen();
void dte_init_record();
void dte_init_message_field();
int  dte_key_from_idx(char *idx);
void dte_unprotect_all_fields();
void dte_protect_all_fields();
void dte_hide_all_fields();
void dte_protect_nomod_fields();
void dte_protect_key_fields();
void dte_unprotect_idx_fields(char *idx);
int  dte_is_this_key(FIELD *fld,int key);
void dte_unprotect_rel_field();
void dte_protect_rel_field();
void dte_hide_all_prompts();
void dte_display_all_prompts();
void dte_display_idx_prompts(char *idx);
void dte_hide_rel_prompt();
void dte_display_rel_prompt();
void DBSC(char *cf_hdrs,char *cf_t1,char *cf_t2,char *io_block,char *dio_block,char *ret_code);
int  KCSI_dbscl(char *hdrs,char *t1,char *t2);
int  dte_change_records(char *idx,char *mode);
int  dte_delete_records(char *idx,char *mode);
int  dte_enter_the_key(char *idx,char *mode);
void dte_read_rel_key_record();
void dte_rel_rec_num_to_fld();
void dte_load_first_record(char *idx);
void dte_load_next_record(char *idx);
void dte_load_previous_record(char *idx);
void DMNT(char *hdrs,char *t1,char *t2,char *cblock,char *dblock,char *mode,char *idx);
void dte_load_footer(char *prompt);
void DTEDAT(char *ymd,char *mdy,char *tim,char *jul);
void DTEKEY(char *mode,char *idx);
void dte_val_all_fields();
void GP_init_gpint();
void GENINQ(char *gio,char *display,char *endrun,char *dio,char *change,char *option,char *cio,char *qtitle,char *qlines,char *eio);
int  KCSI_isnblank(char *mem,int len);
void INIDIO(char *blk);
void PARSEINQ(char *rpt_rfl,char *rpt_dl,char *res_flds,char *scr_flds,char *msg,char *ret_code);
char *kcsi_strupr(char* str);
int first_inq_tkn();
int next_inq_tkn();
char *get_inq_tkn_str(int tkn);
char *WL_wfname(int4 *mode, char *p_vol, char *p_lib, char *p_file, char *native_path);
void KMATCH(char *rc,char *g1,char *g2);
void KCSIO(char *kio,char *ufb,char *recarea);
void KDISP(char* fspec,char* type, char* rc);
void KEXISTS(char *rc, char *name, char *lib, char *vol);
void ksam_init();
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
int  KCSI_get_pic_len(char* dest,int type,int len,int dec,int bin);
void KCSI_rptbld();
int  rpt_read_next_rpt_record();
void KCSI_strunc(char* dest);
void KCSI_unstrunc(char* str,int len);
int  kcsi_atoilen(char* src, int len);
char *KCSI_cpsize();
void rpt_globals();
void KCSI_rptpln(char *p_io,char *p_record,char *p_file,char *p_lib,char *p_vol);
int KCSI_valspec_filled(char *name, char *lib, char *vol);
void VALNAM(char *name, char *rc);
void VALVOL(char *name, char *rc);
void VALFNM(char *name, char *rc);

int  KCSI_e_trans(int code);
void KCSI_clear_keys(KFB *kfb);
int  dte_file_is_relative();
void KCSI_call_lmg_log_count(long count);
void inq_write();
void set_inq_scratch(int code);
void kcsio_wfopen(int4 mode,KFB *kfb);
void KCSI_ccsio(KFB *kfb,char *recarea);
void set_inq_org(int org);
void KFILTYP(char* typ);
void rpt_write(int code);
int KCSI_rptsel();
int KCSI_charat(char *record,char *base,int pos,int len,int type,int dec);
int KCSI_fieldeq(char *record,char *base,int pos,int len,int type,int dec,char* m1);
int KCSI_atoifield(char *record,char *base,int pos,int len,int type,int dec);
void rpt_tie();
void KCSI_inschar(char* d, int c);
void KFORMAT(char* dec,char* rcvr,char* rflrec);
void KCSI_rptphlp();
void rpt_full_gp_text(char *text[]);
void KCSI_strfil(char* s);
void KCSI_init_a_key(struct keydesc *k,int pos,int len,int dups);


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

void KCSI_gpwcsio(KFB *kfb,char *recarea);
void cr_fileio(KCSIO_BLOCK *kfb, char *io);
int  KCSI_ckexists(char *file, char *lib, char *vol);


/*
**	WISPLIB
*/

#include "wfname.h"
#include "wisplib.h"
#include "vssubs.h"
#include "filext.h"
#include "wfiledis.h"
#include "getopt.h"

void WL_wpload(void);

#endif /* kcsifunc_H */

/*
**	History:
**	$Log: kcsifunc.h,v $
**	Revision 1.37  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.36  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.35  2003/01/29 15:10:57  gsl
**	Fix wisplib prototypes
**	
**	Revision 1.34  2003/01/28 20:11:06  gsl
**	fix prototype problems
**	
**	Revision 1.33  2002/10/24 15:48:32  gsl
**	Make globals unique
**	
**	Revision 1.32  2002/10/24 14:20:38  gsl
**	Make globals unique
**	
**	Revision 1.31  2002/10/23 21:07:26  gsl
**	make global name unique
**	
**	Revision 1.30  2002/10/23 20:39:08  gsl
**	make global name unique
**	
**	Revision 1.29  2002/10/22 21:10:20  gsl
**	Unique global sysmbols
**	
**	Revision 1.28  2002/10/21 16:07:05  gsl
**	Add ksam_init
**	
**	Revision 1.27  2002/10/17 21:22:41  gsl
**	cleanup
**	
**	Revision 1.26  2002/10/17 17:17:19  gsl
**	Removed VAX VMS code
**	
**	Revision 1.25  2002/07/30 19:12:43  gsl
**	SETRETCODE
**	
**	Revision 1.24  2002/07/29 21:13:29  gsl
**	
**	Revision 1.23  2002/07/29 15:46:54  gsl
**	getwfilext -> WGETFILEXT
**	setwfilext -> WSETFILEXT
**	setwispfilext -> WSETFILEXT
**	
**	Revision 1.22  2002/07/26 18:19:18  gsl
**	wscreen -> WSCREEN
**	
**	Revision 1.21  2002/07/25 15:48:42  gsl
**	Globals
**	
**	Revision 1.20  2002/07/25 15:20:27  gsl
**	Globals
**	
**	Revision 1.19  2002/07/23 20:49:51  gsl
**	globals
**	
**	Revision 1.18  2002/07/23 02:57:52  gsl
**	wfclose -> WFCLOSE
**	
**	Revision 1.17  2002/07/12 19:10:24  gsl
**	Global unique WL_ changes
**	
**	Revision 1.16  2002/07/12 17:17:01  gsl
**	Global unique WL_ changes
**	
**	Revision 1.15  2002/07/10 21:06:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.14  2002/05/14 21:39:53  gsl
**	include kcsit.h
**	
**	Revision 1.13  2002-04-22 14:27:59-04  gsl
**	Remove unused
**
**	Revision 1.12  2002-04-19 16:58:39-04  gsl
**	remove crid_str
**
**	Revision 1.11  2001-11-20 12:20:08-05  gsl
**	Add rel_xxx() routines
**
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
