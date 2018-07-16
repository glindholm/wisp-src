/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#define			RAB$M_ASY	(1 << RAB$V_ASY)

struct	RAB	{
       unsigned char	rab$b_bid;		/* block identification */
#define			RAB$C_BID	1	/* block identification constant */

       unsigned char	rab$b_bln;		/* block length */
#define 		RAB$C_BLN	0x044	/* block length constant */
#define 		RAB$K_BLN	0x044

       unsigned short	rab$w_isi;				/* internal stream identifier */
#define			RAB$V_PPF_RAT	6			/* rat value for process-permanent files */
#define			RAB$S_PPF_RAT	8
#define			RAB$M_PPF_RAT	(0x0ff << RAB$V_PPF_RAT)
#define			RAB$V_PPF_IND	14			/* indirect access to process-permanent file */
#define			RAB$M_PPF_IND	(1 << RAB$V_PPF_IND)

       unsigned	int	rab$l_rop;			/* record processing options */
#define			RAB$V_ASY	0		/* asynchronous operations */
#define			RAB$M_ASY	(1 << RAB$V_ASY)
#define			RAB$V_TPT	1		/* truncate-on-put - allow sequential put not at eof, thus 
							   truncating file (sequential organization only) */
#define			RAB$M_TPT	(1 << RAB$V_TPT)
#define 		RAB$V_REA	2		/* lock record for read only, allow other readers */
#define			RAB$M_REA	(1 << RAB$V_REA)
#define			RAB$V_RRL	3		/* read record regardless of lock */
#define			RAB$M_RRL	(1 << RAB$V_RRL)
#define			RAB$V_UIF	4		/* update if existent */
#define			RAB$M_UIF	(1 << RAB$V_UIF)
#define			RAB$V_MAS	5		/* mass-insert mode */
#define			RAB$M_MAS	(1 << RAB$V_MAS)
#define			RAB$V_FDL	6		/* fast record deletion */
#define			RAB$M_FDL	(1 << RAB$V_FDL)
#define			RAB$V_HSH	7		/* use hash code in bkt */
#define			RAB$M_HSH	(1 << RAB$V_HSH)
#define			RAB$V_EOF	8		/* connect to end-of-file */
#define			RAB$M_EOF	(1 << RAB$V_EOF)
#define			RAB$V_RAH	9		/* read ahead */
#define			RAB$M_RAH	(1 << RAB$V_RAH)
#define			RAB$V_WBH	10		/* write behind */
#define			RAB$M_WBH	(1 << RAB$V_WBH)
#define			RAB$V_BIO	11		/* connect for block I/O only */
#define			RAB$M_BIO	(1 << RAB$V_BIO)
#define			RAB$V_LV2	12		/* level 2 RU lock consistency */
#define			RAB$M_LV2	(1 << RAB$V_LV2)
#define			RAB$V_LOA	13		/* load buckets according to the file size */
#define			RAB$M_LOA	(1 << RAB$V_LOA)
#define			RAB$V_LIM	14		/* compare for key limit reached on $get/$find seq.(idx only) */
#define			RAB$M_LIM	(1 << RAB$V_LIM)
#define			RAB$V_LOC	16		/* use locate mode */
#define			RAB$M_LOC	(1 << RAB$V_LOC)
#define			RAB$V_WAT	17		/* wait if record not available */
#define			RAB$M_WAT	(1 << RAB$V_WAT)
#define			RAB$V_ULK	18		/* manual unlocking */
#define			RAB$M_ULK	(1 << RAB$V_ULK)
#define			RAB$V_RLK	19		/* allow readers for this locked record */
#define			RAB$M_RLK	(1 << RAB$V_RLK)
#define			RAB$V_NLK	20		/* do not lock record */
#define			RAB$M_NLK	(1 << RAB$V_NLK)
#define			RAB$V_KGE	21		/* key is greater than or equal to */
#define			RAB$M_KGE	(1 << RAB$V_KGE)
#define			RAB$V_KGT	22		/* key is greater than */
#define			RAB$M_KGT	(1 << RAB$V_KGT)
#define			RAB$V_NXR	23		/* non-existent record processing */
#define			RAB$M_NXR	(1 << RAB$V_NXR)
#define			RAB$V_RNE	24		/* read no echo */
#define			RAB$M_RNE	(1 << RAB$V_RNE)
#define			RAB$V_TMO	25		/* use time-out period */
#define			RAB$M_TMO	(1 << RAB$V_TMO)
#define			RAB$V_CVT	26		/* convert to upper case */
#define			RAB$M_CVT	(1 << RAB$V_CVT)
#define			RAB$V_RNF	27		/* read no filter */
#define 		RAB$M_RNF	(1 << RAB$V_RNF)
#define			RAB$V_ETO	28		/* extended terminal operation */
#define			RAB$M_ETO	(1 << RAB$V_ETO)
#define			RAB$V_PTA	29		/* purge type ahead */
#define			RAB$M_PTA	(1 << RAB$V_PTA)
#define			RAB$V_PMT	30		/* use prompt buffer */
#define			RAB$M_PMT	(1 << RAB$V_PMT)
#define			RAB$V_CCO	31		/* cancel control O on output */
#define			RAB$M_CCO	(1 << RAB$V_CCO)

       unsigned int	rab$l_sts;		/* completion status code */
       unsigned int	rab$l_stv;		/* status value */
       unsigned short	rab$w_rfa[3];		/* record's file address */

		unsigned : 16;			/* spare */

       unsigned int	rab$l_ctx;		/* user context */

		unsigned : 16;			/* spare */

       unsigned char   	rab$b_rac;		/* record access mode */
#define			RAB$C_SEQ	0	/* sequential access */
#define			RAB$C_KEY	1	/* keyed access */
#define			RAB$C_RFA	2	/* rfa access */
#define			RAB$C_STM	3	/* stream access (valid only for sequential org) */

       unsigned char	rab$b_tmo;		/* time-out period */
       unsigned short	rab$w_usz;		/* user record area size */
       unsigned short	rab$w_rsz;		/* record size */
		char	*rab$l_ubf;		/* user record area address */
		char	*rab$l_rbf;		/* record buffer address */
		char	*rab$l_rhb;		/* record header buffer address */

		char	*rab$l_kbf;			/* key address */
#define 		rab$l_pbf	rab$l_kbf	/* prompt buffer address (same offset as rab$l_kbf) */

       unsigned char	rab$b_ksz;			/* key buffer size */
#define			rab$b_psz	rab$b_ksz	/* prompt buffer size (same offset as rab$b_ksz) */

       unsigned char	rab$b_krf;		/* key of reference */
		char	rab$b_mbf;		/* multi-buffer count */
       unsigned char	rab$b_mbc;		/* multi-block count */

       unsigned int	rab$l_bkt;			/* bucket hash code, vbn, or rrn */
#define			rab$l_dct	rab$l_bkt	/* duplicates count on key accessed on alternate key */

	 struct	FAB	*rab$l_fab;		/* related fab address for connect */

	 char		*rab$l_xab;		/* related XAB address */
		};


struct	{
	unsigned	: 32;
	unsigned	: 32;
	unsigned	: 32;

	unsigned short	rab$w_stv0;	/* low order word of stv */
	unsigned short	rab$w_stv2;	/* high order word of stv */
	unsigned int	rab$l_rfa0;
	unsigned short	rab$w_rfa4;
	};

struct {
	unsigned	: 32;
	unsigned	: 8;

	unsigned char	rab$b_rop1;	/* second byte of ROP */
	unsigned char	rab$b_rop2;	/* third  byte of ROP */
	unsigned char	rab$b_rop3;	/* fourth byte of ROP */
	};


globalref struct RAB cc$rms_rab;	/* Declare initialized prototype data structure */
/*
**	History:
**	$Log: rab.h,v $
**	Revision 1.3  1996-09-17 19:34:14-04  gsl
**	drcs update
**
**
**
*/
