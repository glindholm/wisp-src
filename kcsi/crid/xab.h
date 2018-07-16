/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/*	XABALL -- Allocation Controll Extended Attribute Block Definitions	*/

struct	XABALL	{
       unsigned char	xab$b_cod;		
#define			XAB$C_ALL	20		/* XABALL type code */

       unsigned char	xab$b_bln;		
#define			XAB$C_ALLLEN	0x20		/* XABALL block length (32 bytes) */
#define			XAB$K_ALLLEN	0x20

		unsigned	: 16;			/* spare */

		char 	*xab$l_nxt;			/* Next XAB in chain */

       unsigned char	xab$b_aop;			/* allocation options */
#define			XAB$V_HRD	0		/* fail if requestd alignmt cannot be performed */
#define			XAB$M_HRD	(1 << XAB$V_HRD)
#define			XAB$V_ONC	1		/* locate space on cylinder boundary */
#define 		XAB$M_ONC	(1 << XAB$V_ONC)
#define			XAB$V_CBT	5		/* contiguous allocation, best try */
#define			XAB$M_CBT	(1 << XAB$V_CBT)
#define			XAB$V_CTG	7		/* contiguous allocation */
#define			XAB$M_CTG	(1 << XAB$V_CTG)

       unsigned char	xab$b_aln;			/* alignment boundary type */
#define			XAB$C_ANY	0		/* any type of allocation */
#define			XAB$C_CYL	1		/* alignment starts at cylinder boundary */
#define			XAB$C_LBN	2		/* allocate at specified logical block number */
#define			XAB$C_VBN	3		/* allocate near specified virtual block number */
#define			XAB$C_RFI	4		/* allocate near related file */

       unsigned short	xab$w_vol;			/* relative volume number */
       unsigned long	xab$l_loc;			/* allocation location */
       unsigned long	xab$l_alq;			/* allocation quantity */
       unsigned short	xab$w_deq;			/* default extension quantity */
       unsigned char	xab$b_bkz;			/* bucket size used with the relative and indexed files */
       unsigned char	xab$b_aid;			/* area identification number */

							/* related file identification */
       unsigned short	xab$w_rfi0;			/* file number */
#define			xab$w_rfi	xab$w_rfi0

       unsigned short	xab$w_rfi2;			/* seq number */
       unsigned short	xab$w_rfi4;			/* rev number */

		unsigned	: 16;			/* spare */
		};


globalref struct XABALL cc$rms_xaball;	/* Declare initialized prototype data structures */


/*  struct representing a 64-bit binary value expressing the date and time */

#define	XAB_DATE_TIME 	{ unsigned	: 32; unsigned	: 32; }


/*	XABDAT -- Date and Time Extended Attribute Block Definitions	*/

struct	XABDAT	{
       unsigned char	xab$b_cod;			
#define			XAB$C_DAT	18	/* XABDAT type code */

       unsigned char	xab$b_bln;			
#define			XAB$C_DATLEN	0x2C	/* XABDAT block length constant - V3 (44 bytes) */
#define 		XAB$K_DATLEN	0x2C
#define			XAB$C_DATLEN_V2	0x24	/* XABDAT block length constant - V2 */
#define			XAB$K_DATLEN_V2	0x24

		unsigned	: 16;		/* spare */

		char	*xab$l_nxt;		/* Next XAB in chain */
       unsigned short	xab$w_rvn;		/* revision number */

		unsigned	: 16;		/* spare */

   struct XAB_DATE_TIME xab$q_rdt;		/* revision date and time */
   struct XAB_DATE_TIME xab$q_cdt;		/* creation date and time */
   struct XAB_DATE_TIME xab$q_edt;		/* expiration date and time */
   struct XAB_DATE_TIME xab$q_bdt;		/* backup date and time */
		};


globalref struct XABDAT cc$rms_xabdat;	/* Declare initialized prototype data structures */


/*	XABFHC -- File Header Characteristics Extended Attribute Block Definitions	*/

struct	XABFHC	{
       unsigned char	xab$b_cod;			
#define			XAB$C_FHC	29	/* XABFHC type code */

       unsigned char	xab$b_bln;			
#define			XAB$C_FHCLEN	0x2C	/* XABFHC block length (44 bytes) */
#define 		XAB$K_FHCLEN	0x2C

		unsigned	: 16;		/* spare */

	     	char	*xab$l_nxt;		/* Next XAB in chain */
       unsigned char	xab$b_rfo;		/* record format and file organization */
       unsigned char	xab$b_atr;		/* record attributes */
       unsigned short	xab$w_lrl;		/* longest record's length */
       unsigned long	xab$l_hbk;		/* high virtual block in the file */
       unsigned long	xab$l_ebk;		/* end-of-file block */
       unsigned short	xab$w_ffb;		/* first free byte in end-of-file block */
       unsigned char	xab$b_bkz;		/* bucket size */
       unsigned char	xab$b_hsz;		/* fixed length control header size */
       unsigned short	xab$w_mrz;		/* maximun record size */
       unsigned short	xab$w_dxq;		/* default file extension quantity */
       unsigned short	xab$w_gbc;		/* default global buffer count */

		unsigned	: 32;		/* spare */
		unsigned	: 32;		/* spare */

       unsigned short	xab$w_verlimit;		/* version limit for the file */
       unsigned long	xab$l_sbn;		/* starting logical block number if contiguous */
		};


globalref struct XABFHC cc$rms_xabfhc;	/* Declare initialized prototype data structures */


/*	XABKEY -- Key Definition Extended Attribute Block Definitions	*/

struct	XABKEY	{
       unsigned char	xab$b_cod;		
#define			XAB$C_KEY	21		/* XABKEY type code */

       unsigned char	xab$b_bln;		
#define			XAB$C_KEYLEN	0x4C		/* XABKEY block length (76 bytes) */
#define			XAB$K_KEYLEN	0x4C
#define			XAB$C_KEYLEN_V2	0x40
#define			XAB$K_KEYLEN_V2 0x40

		unsigned	: 16;			/* spare */

		char	*xab$l_nxt;			/* Next XAB address in chain */
       unsigned char	xab$b_ian;			/* index level area number */
       unsigned char	xab$b_lan;			/* lowest level of index area number */
       unsigned char	xab$b_dan;			/* data bucket area number */
       unsigned char	xab$b_lvl;			/* level of root bucket */
       unsigned char	xab$b_ibs;			/* index bucket size in virtual blocks */
       unsigned char	xab$b_dbs;			/* data bucket size in virtual blocks */
       unsigned long	xab$l_rvb;			/* root bucket start virtual block number */

	  unsigned char	xab$b_flg;				/* key option flag byte */
#define			XAB$V_DUP	0			/* duplicate key value allowed */
#define			XAB$M_DUP	(1 << XAB$V_DUP)
#define			XAB$V_CHG	1			/* may change on update (alternate keys only) */
#define			XAB$M_CHG	(1 << XAB$V_CHG)
#define			XAB$V_NUL	2			/* null key value enable (alternate keys only) */
#define			XAB$M_NUL	(1 << XAB$V_NUL)
#define			XAB$V_IDX_NCMPR	3			/* indicates index records are not compressed */
#define 		XAB$M_IDX_NCMPR	(1 << XAB$V_IDX_NCMPR)
#define			XAB$V_KEY_NCMPR	6			/* indicates primary key is not compressed */
#define			XAB$M_KEY_NCMPR (1 << XAB$V_KEY_NCMPR)
#define 		XAB$V_DAT_NCMPR	7			/* indicated data record is not compressed */
#define			XAB$M_DAT_NCMPR	(1 << XAB$V_DAT_NCMPR)

       unsigned char	xab$b_dtp;			/* key field data type */
#define			XAB$C_STG	0		/* left-justified string of unsigned bytes */
#define			XAB$C_IN2	1		/* signed 15 bit integer (2 bytes) */
#define			XAB$C_BN2	2		/* unsigned 2-byte binary */
#define			XAB$C_IN4	3		/* signed 31 bit integer (4 bytes) */
#define			XAB$C_BN4	4		/* unsigned 4-byte binary */
#define			XAB$C_PAC	5		/* packed decimal string (1-16 bytes) */
#define			XAB$C_IN8	6		/* signed 63 bit integer */
#define			XAB$C_BN8	7		/* 8 byte binary */
#define			XAB$C_MAXDTP	7		/* maximun legal data type */

       unsigned char	xab$b_nsg;			/* number of key segments */
       unsigned char	xab$b_nul;			/* null key character value */
       unsigned char	xab$b_tks;			/* total key field size (bytes) */
       unsigned char	xab$b_ref;			/* key of reference (0=primary key, 1-254=alternate keys) */
       unsigned short	xab$w_mrl;			/* minimun record length */
       unsigned short	xab$w_ifl;			/* index bucket fill size (bytes) */
       unsigned short	xab$w_dfl;			/* data bucket fill size (bytes) */

       unsigned short	xab$w_pos0;			/* key field record offset positions */
#define			xab$w_pos	xab$w_pos0

       unsigned short	xab$w_pos1;
       unsigned short	xab$w_pos2;
       unsigned short	xab$w_pos3;
       unsigned short	xab$w_pos4;
       unsigned short	xab$w_pos5;
       unsigned short	xab$w_pos6;
       unsigned short	xab$w_pos7;

       unsigned char	xab$b_siz0;			/* key field segment sizes (bytes) */
#define			xab$b_siz 	xab$b_siz0			

       unsigned char	xab$b_siz1;
       unsigned char	xab$b_siz2;
       unsigned char	xab$b_siz3;
       unsigned char	xab$b_siz4;
       unsigned char	xab$b_siz5;
       unsigned char	xab$b_siz6;
       unsigned char	xab$b_siz7;

	       	unsigned	: 16;			/* spare */

		char	*xab$l_knm;			/* key name buffer address */
       unsigned long	xab$l_dvb;			/* first data bucket virtual block number */

       unsigned char	xab$b_typ0;			/* key field segment types */
#define			xab$b_typ	xab$b_typ0			

       unsigned char	xab$b_typ1;
       unsigned char	xab$b_typ2;
       unsigned char	xab$b_typ3;
       unsigned char	xab$b_typ4;
       unsigned char	xab$b_typ5;
       unsigned char	xab$b_typ6;
       unsigned char	xab$b_typ7;

       unsigned char	xab$b_prolog;			/* prologue level */
#define			XAB$C_PRG1	1		/* prologue 1 */
#define			XAB$C_PRG2	2		/* prologue 2 */
#define			XAB$C_PRG3	3		/* prologue 3 */

		unsigned	: 24;			/* spare */
		};


globalref struct XABKEY cc$rms_xabkey;	/* Declare initialized prototype data structures */

#define		XAB_PROT_FIELDS 	{ unsigned : 32; unsigned : 32; }

/*	XABPRO -- File Protection Extended Attribute Block Definitions	*/

struct	XABPRO	{
       unsigned char	xab$b_cod;			
#define			XAB$C_PRO	19	/* XABPRO type code */

       unsigned char	xab$b_bln;			
#define			XAB$C_PROLEN_V3	0x10	/* XABPRO block length (16 bytes) version 3.0 VMS */
#define			XAB$K_PROLEN_V3	0x10
#define			XAB$C_PROLEN	0x58	/* XABPRO block length (88 bytes) */
#define			XAB$K_PROLEN	0x58

		unsigned	: 16;		/* spare */

		char	*xab$l_nxt;		/* Next XAB in chain */

	 unsigned short	xab$w_pro;				/* protection mask */
#define			XAB$V_SYS	0			/* system */
#define			XAB$S_SYS	4
#define			XAB$V_OWN	4			/* owner */
#define			XAB$S_OWN	4
#define			XAB$V_GRP	8			/* group */
#define			XAB$S_GRP	4
#define			XAB$V_WLD	12			/* world */
#define			XAB$S_WLD	4
#define			XAB$V_NOREAD	0			/* deny read access */
#define			XAB$M_NOREAD	(1 << XAB$V_NOREAD)
#define			XAB$V_NOWRITE	1			/* deny write access */
#define			XAB$M_NOWRITE	(1 << XAB$V_NOWRITE)
#define			XAB$V_NOEXE	2			/* deny execution access */
#define	        	XAB$M_NOEXE	(1 << XAB$V_NOEXE)
#define			XAB$V_NODEL	3			/* deny delete access */
#define			XAB$M_NODEL	(1 << XAB$V_NODEL)

       unsigned char	xab$b_mtacc;		/* magnetic tape accessibility */

       unsigned char	xab$b_prot_opt;		/* XABPRO options field */
#define			XAB$V_PROPOGATE	0	/* propogate security attributes on $ENTER and $RENAME */
#define			XAB$M_PROPOGATE	(1 << XAB$V_PROPOGATE)

	unsigned long	xab$l_uic;		/* uic code */

 struct XAB_PROT_FIELDS xab$q_prot_mode;	/* RWED/mode protection for file */

		char	*xab$l_aclbuf;		/* address of users ACL buffer */	
       unsigned short	xab$w_aclsiz;		/* size of user's ACL buffer */
       unsigned short	xab$w_acllen;		/* return lecngth of entire ACL */
       unsigned long	xab$l_aclctx;		/* ACL context field */
       unsigned long	xab$l_aclsts;		/* ACL return error status */

	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;
	unsigned 	: 32;

		};

struct	{
	unsigned	: 32;
	unsigned	: 32;
	unsigned	: 32;

	unsigned short	xab$w_mbm;		/* member number of file owner */
	unsigned short	xab$w_grp;		/* group number of file owner */
	unsigned char	xab$b_prot_mode;	/* first byte of protection mode field */

	};


globalref struct XABPRO cc$rms_xabpro;	/* Declare initialized prototype data structures */

#undef XAB_PROT_FIELDS


/*	XABRDT -- Revision Date and Time Extended Attribute Block Definitions	*/

struct	XABRDT	{
       unsigned char	xab$b_cod;			
#define			XAB$C_RDT	30	/* XABRDT type code */

       unsigned char	xab$b_bln;			
#define			XAB$C_RDTLEN	0x14	/* XABRDT block length (20 bytes) */
#define 		XAB$K_RDTLEN	0x14

		unsigned	: 16;		/* spare */

		char	*xab$l_nxt;		/* Next XAB in chain */
       unsigned short	xab$w_rvn;		/* revision number */

		unsigned 	: 16;		/* spare */

   struct XAB_DATE_TIME xab$q_rdt;		/* revision date and time */
		};


globalref struct XABRDT cc$rms_xabrdt;	/* Declare initialized prototype data structures */


#undef XAB_DATE_TIME

/*	XABSUM -- Summary Extended Attribute Block Definitions	*/

struct	XABSUM	{
       unsigned char	xab$b_cod;		
#define			XAB$C_SUM	22	/* XABSUM type code */

       unsigned char	xab$b_bln;		
#define			XAB$C_SUMLEN	0x0C	/* XABSUM block length (12 bytes) */
#define			XAB$K_SUMLEN	0x0C
	
		unsigned	: 16;		/* spare */

		char	*xab$l_nxt;		/* Next XAB in chain */
       unsigned char	xab$b_noa;		/* number of allocation areas defined for the file */
       unsigned char	xab$b_nok;		/* number of keys defined for the file */
       unsigned short 	xab$w_pvn;		/* prologue version number */
		};


globalref struct XABSUM cc$rms_xabsum;	/* Declare initialized prototype data structures */


/* XABTRM - Terminal Control XAB field definitions */

struct XABTRM {

       unsigned char	xab$b_cod;
#define			XAB$C_TRM	31	/* XABSUM type code */

       unsigned char	xab$b_bln;
#define			XAB$C_TRMLEN	36	/* length of XABTRM block */
#define			XAB$K_TRMLEN	36	/* length of XABTRM block */

		unsigned	: 16;		/* spare */

		char	*xab$l_nxt;		/* address of next block */
		char	*xab$l_itmlst;		/* item list address */
       unsigned short	xab$w_itmlst_len;	/* length of item list */

		unsigned 	: 16;
		unsigned 	: 32;
		unsigned 	: 32;
		unsigned 	: 32;
		unsigned 	: 32;
		unsigned 	: 32;
	};

globalref	struct XABTRM	cc$rms_xabtrm;	/* declare initialized prototype data structure */
/*
**	History:
**	$Log: xab.h,v $
**	Revision 1.3  1996-09-17 19:34:23-04  gsl
**	drcs update
**
**
**
*/
