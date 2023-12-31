/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

extern DTYPE rpt_old[],rpt_new[];
extern DTYPE wrk_dtype;
extern RPT_OPT rpt_opt;
extern RPT_RHD rpt_rhd;
extern RPT_RFL rpt_rfl[],wrk_rfl;
extern RPT_RCB rpt_rcb[],wrk_rcb;
extern RPT_RTT rpt_rtt[],wrk_rtt;
extern RPT_SRT rpt_srt[],wrk_srt;
extern RPT_DL  rpt_dl[] ,wrk_dl;
extern RPT_NF  rpt_nf[] ,wrk_nf;
extern RPT_DLO wrk_dlo;
extern RPT_NFO wrk_nfo;
extern RPT_DEF rpt_def;
extern DTYPE rpt_opt_dest[],rpt_opt_src[],rpt_rhd_dest[],rpt_rhd_src[],
	   rpt_rdf_dest[],rpt_rdf_src[],rpt_rcb_dest[],rpt_rcb_src[],
	   rpt_rcd_dest[],rpt_rcd_src[],rpt_rfl_dest[],rpt_rfl_src[],
	   rpt_crf_dest[],rpt_crf_src[],rpt_rtt_dest[],rpt_rtt_src[],
	   rpt_srt_dest[],rpt_srt_src[],rpt_dlo_dest[],rpt_dlo_src[],
	   rpt_nfo_dest[],rpt_nfo_src[],
	   rpt_dl_dest[],rpt_dl_src[],rpt_nf_dest[],rpt_nf_src[],
	   rpt_dlo_name_dest[],rpt_dlo_name_src[],
	   rpt_nfo_name_dest[],rpt_nfo_name_src[],
	   rpt_dlo_lit_dest[],rpt_dlo_lit_src[],
	   rpt_nfo_lit_dest[],rpt_nfo_lit_src[];

extern char rpt_inp_rec1[],rpt_inp_rec2[],rpt_inp_rec[];
extern int rpt_inp_rec_len;
extern int rpt_key_to_sec_idx;

extern RPT_ASORT rpt_sort[];

extern char rpt_bld_fld[];

extern char *rpt_PRT_FILE;
extern char *rpt_PRT_LIB;
extern char *rpt_PRT_VOL;
extern char *rpt_caller;
extern long rpt_total_records,rpt_record_count,rpt_sorted_records;

/*
**	History:
**	$Log: rptglb.h,v $
**	Revision 1.8  2003/02/17 22:05:58  gsl
**	Fix ambiguous SORT reference
**	
**	Revision 1.7  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.6  2002/10/24 14:20:34  gsl
**	Make globals unique
**	
**	Revision 1.5  2002/10/23 21:07:25  gsl
**	make global name unique
**	
**	Revision 1.4  2002/07/25 15:20:24  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:34:17  gsl
**	drcs update
**	
**
**
*/
