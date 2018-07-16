/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/*	FAB - File Access Block Definitions	*/

struct	FAB	{
       unsigned char	fab$b_bid;			/* block identification */
#define			FAB$C_BID	3		/* block id constant */

       unsigned char 	fab$b_bln;			/* block length */
#define 		FAB$C_BLN	0x050		/* block length constant (80 bytes) */
#define 		FAB$K_BLN	0x050	

       unsigned short	fab$w_ifi;				/* internal file identifier */
#define			FAB$V_PPF_RAT	6			/* rat value for process-permanent files */
#define			FAB$S_PPF_RAT	8
#define			FAB$M_PPF_RAT	(0x0ff << FAB$V_PPF_RAT)
#define			FAB$V_PPF_IND	14			/* indirect access to process-permanent file */
#define			FAB$M_PPF_IND	(1 << FAB$V_PPF_IND)

       unsigned int	fab$l_fop;			/* file options */
#define			FAB$V_MXV	1		/* maximize version number */
#define			FAB$M_MXV	(1 << FAB$V_MXV)
#define			FAB$V_SUP	2		/* supersede existing file */
#define			FAB$M_SUP	(1 << FAB$V_SUP)
#define			FAB$V_TMP	3		/* create temporary file */
#define			FAB$M_TMP	(1 << FAB$V_TMP)
#define			FAB$V_TMD	4		/* temporary file marked for delete */
#define			FAB$M_TMD	(1 << FAB$V_TMD)
#define			FAB$V_DFW	5		/* deferred write (rel and idx) */
#define			FAB$M_DFW	(1 << FAB$V_DFW)
#define			FAB$V_SQO	6		/* sequential access only */
#define			FAB$M_SQO	(1 << FAB$V_SQO)
#define			FAB$V_RWO	7		/* rewind magnetic tape on open */
#define			FAB$M_RWO	(1 << FAB$V_RWO)
#define			FAB$V_POS	8		/* use next magnetic tape position */
#define			FAB$M_POS	(1 << FAB$V_POS)
#define			FAB$V_WCK	9		/* write checking */
#define			FAB$M_WCK	(1 << FAB$V_WCK)
#define			FAB$V_NEF	10		/* not end of file, inihibit eof positioning */
#define			FAB$M_NEF	(1 << FAB$V_NEF)
#define			FAB$V_RWC	11		/* rewind magnetic tape on close */
#define			FAB$M_RWC	(1 << FAB$V_RWC)
#define			FAB$V_DMO	12		/* dismount mt on close (not implemented) */
#define			FAB$M_DMO	(1 << FAB$V_DMO)
#define			FAB$V_SPL	13		/* spool file on close */
#define			FAB$M_SPL	(1 << FAB$V_SPL)
#define			FAB$V_SCF	14		/* submit command file on close */
#define			FAB$M_SCF	(1 << FAB$V_SCF)
#define			FAB$V_DLT	15		/* delete file */
#define			FAB$M_DLT	(1 << FAB$V_DLT)
#define			FAB$V_NFS	16		/* non-file-structured operation */
#define			FAB$M_NFS	(1 << FAB$V_NFS)
#define			FAB$V_UFO	17		/* user file open - no rms operation */
#define			FAB$M_UFO	(1 << FAB$V_UFO)
#define			FAB$V_PPF	18		/* process permanent file (pio segment) */
#define			FAB$M_PPF	(1 << FAB$V_PPF)
#define			FAB$V_INP	19		/* process permanent file is 'input' */
#define			FAB$M_INP	(1 << FAB$V_INP)
#define			FAB$V_CTG	20		/* contiguous extension */
#define			FAB$M_CTG	(1 << FAB$V_CTG)
#define			FAB$V_CBT	21		/* contiguous best try */
#define			FAB$M_CBT	(1 << FAB$V_CBT)
#define			FAB$V_JNL	22		/* explicit logging (not implemented) */
#define			FAB$M_JNL	(1 << FAB$V_JNL)
#define			FAB$V_RCK	23		/* read checking */
#define			FAB$M_RCK	(1 << FAB$V_RCK)
#define			FAB$V_NAM	24		/* use NAM block device, file and/or directory id */
#define			FAB$M_NAM	(1 << FAB$V_NAM)
#define			FAB$V_CIF	25		/* create if non-existent */
#define			FAB$M_CIF	(1 << FAB$V_CIF)
#define			FAB$V_UFM	26		/* user file open mode (user if 1, super if 0) 
							   enable only if esc and (ufo or nfs) are also on */
#define			FAB$M_UFM	(1 << FAB$V_UFM)
#define			FAB$V_ESC	27		/* 'escape' to non-standard functions ($modify) */
#define			FAB$M_ESC	(1 << FAB$V_ESC)
#define			FAB$V_TEF	28		/* truncate at end-of-file on close (write-accessed seq. 
							   disk file only) */
#define			FAB$M_TEF	(1 << FAB$V_TEF)
#define			FAB$V_OFP	29		/* output file parse (only name type sticky) */
#define			FAB$M_OFP	(1 << FAB$V_OFP)
#define			FAB$V_KFO	30		/* known file open (image activator only release 1) */
#define			FAB$M_KFO	(1 << FAB$V_KFO)

       unsigned int	fab$l_sts;			/* status */
       unsigned int	fab$l_stv;			/* status value */
       unsigned int	fab$l_alq;			/* allocation quantity */
       unsigned short	fab$w_deq;			/* default allocation quantity */

       unsigned	char	fab$b_fac;			/* file access */
#define			FAB$V_PUT	0		/* put access */
#define			FAB$M_PUT	(1 << FAB$V_PUT)
#define			FAB$V_GET	1		/* get access */
#define			FAB$M_GET	(1 << FAB$V_GET)
#define			FAB$V_DEL	2		/* delete access */
#define			FAB$M_DEL	(1 << FAB$V_DEL)
#define			FAB$V_UPD	3		/* update access */
#define			FAB$M_UPD	(1 << FAB$V_UPD)
#define			FAB$V_TRN	4		/* truncate access */
#define			FAB$M_TRN	(1 << FAB$V_TRN)
#define			FAB$V_BIO	5		/* block i/o access */
#define			FAB$M_BIO	(1 << FAB$V_BIO)
#define			FAB$V_BRO	6		/* block and record i/o access */
#define			FAB$M_BRO	(1 << FAB$V_BRO)
#define			FAB$V_EXE	7		/* execute access (caller must be exec or kernel mode, 
							   ufo must also be set) */
#define			FAB$M_EXE	(1 << FAB$V_EXE)

       unsigned	char	fab$b_shr;				/* file sharing */
#define			FAB$V_SHRPUT	0			/* put access */
#define			FAB$M_SHRPUT	(1 << FAB$V_SHRPUT)
#define			FAB$V_SHRGET	1			/* get access */
#define			FAB$M_SHRGET	(1 << FAB$V_SHRGET)
#define			FAB$V_SHRDEL	2			/* delete access */
#define			FAB$M_SHRDEL	(1 << FAB$V_SHRDEL)
#define			FAB$V_SHRUPD	3			/* update access */
#define			FAB$M_SHRUPD	(1 << FAB$V_SHRUPD)
#define			FAB$V_MSE	4			/* multi-stream connects enabled */
#define			FAB$M_MSE	(1 << FAB$V_MSE)
#define			FAB$V_NIL	5			/* no sharing */
#define			FAB$M_NIL	(1 << FAB$V_NIL)
#define			FAB$V_UPI	6			/* user provided interlocking (allows multiple */
								/* writers to seq. files) */
#define			FAB$M_UPI	(1 << FAB$V_UPI)

       unsigned int	fab$l_ctx;			/* user context */
		char	fab$b_rtv;			/* retrieval window size */

       unsigned char	fab$b_org;				/* file organization */
#define			FAB$V_ORG	4			/* file organization */
#define			FAB$S_ORG	4
#define			FAB$C_SEQ	0			/* sequential */
#define			FAB$C_REL	(1 << FAB$V_ORG)	/* relative */
#define			FAB$C_IDX	(2 << FAB$V_ORG)	/* indexed */
#define			FAB$C_HSH	(3 << FAB$V_ORG)	/* hashed */

       unsigned	char	fab$b_rat;			/* record attributes */
#define			FAB$V_FTN	0		/* FORTRAN carriage control character */
#define			FAB$M_FTN	(1 << FAB$V_FTN)
#define			FAB$V_CR	1		/* line feed - record -carriage return */
#define			FAB$M_CR	(1 << FAB$V_CR)
#define			FAB$V_PRN	2		/* print-file carriage control */
#define			FAB$M_PRN	(1 << FAB$V_PRN)
#define			FAB$V_BLK	3		/* records don't cross block boundaries */
#define			FAB$M_BLK	(1 << FAB$V_BLK)

       unsigned char	fab$b_rfm;			/* record format */
#define			FAB$C_RFM_DFLT	2		/* variable length is default */
#define			FAB$C_UDF	0		/* undefined */
#define			FAB$C_FIX	1		/* fixed-length record */
#define			FAB$C_VAR	2		/* variable-length record */
#define			FAB$C_VFC	3		/* variable-length with fixed-length control record */
#define 		FAB$C_STM	4		/* RMS-11 stream record (valid only for sequential org) */
#define			FAB$C_STMLF	5		/* stream record delimited by LF (sequential org only) */
#define 		FAB$C_STMCR	6		/* stream record delimited by CR (sequential org only) */
#define			FAB$C_MAXRFM	6		/* maximum rfm supported */
#define			FAB$M_RU	1		
#define			FAB$M_AI	2		
#define			FAB$M_BI	4	

       unsigned int	fab$l_jnl;			/* lcb address */
		char	*fab$l_xab;			/* XAB address */
	struct	NAM	*fab$l_nam;			/* NAM block address */
		char	*fab$l_fna;			/* file name string address */
		char	*fab$l_dna;			/* default name string address */
       unsigned char	fab$b_fns;			/* file name string size */
       unsigned char	fab$b_dns;			/* default name string size */
       unsigned short	fab$w_mrs;			/* maximum record size */
       unsigned int	fab$l_mrn;			/* maximum record number */
       unsigned short	fab$w_bls;			/* block size for tape */
       unsigned char	fab$b_bks;			/* bucket size */
       unsigned char	fab$b_fsz;			/* fixed header size */
       unsigned	int	fab$l_dev;			/* device characteristics */
       unsigned	int	fab$l_sdc;			/* spooling device characteristics */
       unsigned short	fab$w_gbc;			/* global buffer count */

       unsigned char	fab$b_acmodes;			/* agent access modes */
#define			fab$b_dsbmsk	fab$b_acmodes	/* saved for backwards compatibility */
#define			FAB$S_LNM_MODE	2		/* logical names */
#define			FAB$V_LNM_MODE	0
#define			FAB$S_CHAN_MODE	2		/* channel */
#define			FAB$V_CHAN_MODE	2
#define			FAB$S_FILE_MODE	2		/* files accessability */
#define			FAB$V_FILE_MODE	4

       unsigned	char	fab$b_rcf;			/* recovery control flags */
# define		FAB$V_RU	0		/* recovery unit recovery */
# define		FAB$V_AI	1		/* after image recovery */
# define		FAB$V_BI	2		/* before image recovery */

		unsigned : 32;         			/* spare */
		};


globalref struct FAB cc$rms_fab;	/* Declare initialized prototype data structure */
/*
**	History:
**	$Log: fab.h,v $
**	Revision 1.3  1996/09/17 23:34:07  gsl
**	drcs update
**	
**
**
*/
