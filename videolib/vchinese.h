/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		vchinese.h
**
**	Project:	video/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Chinese character support.
**
*/

#ifndef vchinese_H
#define vchinese_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

#define MAX_CODENAME_LEN 10
#define MAX_FILENAME_LEN 128

#define CS_IVS_CH 0
#define CS_GW 1
#define CS_BIG5 2
#define CS_HP 3
#define CS_CCDC 4
#define CS_ET 5

#define CS_TYPE_MAX 6

#define XL_F2_TO_F1 1
#define XL_F1_TO_F2 2

#define XL_TO_IVS    XL_F2_TO_F1
#define XL_FROM_IVS  XL_F1_TO_F2

#define ORDER_NORMAL 1
#define ORDER_REVERSE 2

struct xlat_s 
{
	char *name;
	int bytesper;
};

struct xfileheader
{
	int magic, version;
	int fmt1, fmt2;
	int f1_ind_offs, f1_ind_len;
	int f2_ind_offs, f2_ind_len;
	int f1_to_f2_offs, f1_to_f2_len; 
	int f2_to_f1_offs, f2_to_f1_len; 
};

#define HOLDSZ 10

struct xlcontext
{
	unsigned char *indbuf;
	unsigned char *xlatbuf;
	int *multvalues;
	int srcsize,dstsize;
	int multipos;
	int hold[HOLDSZ];
	int holdpos, holdstart;
};

#define CHINESEMAGIC 0xd5dea100 
#define CHINESEVERSION 1
#define CHTABLEPATH "WLANGUAGEPATH"
#define WISPLANGVAR "WISPLANG"

#ifdef __CHINESE__

char f1name[MAX_CODENAME_LEN+1];
char f2name[MAX_CODENAME_LEN+1];

/* for build table from HP tables*/
char srctablefile[MAX_FILENAME_LEN];
char interxlatfile[MAX_FILENAME_LEN];

char xlatfile[MAX_FILENAME_LEN];


unsigned char *tablebuf=NULL, *xlatbuf=NULL, *f1indbuf=NULL, *f2indbuf=NULL;
unsigned char *f1tof2buf=NULL, *f2tof1buf=NULL;

char errbuf[80];

int chtab_init=0;

char tablepath[128]={0};

struct xlat_s charsets[CS_TYPE_MAX]=
{
 { "ivs", 3 },
 { "gw",  2 },
 { "b5",  2 },
 { "hp",  2 },
 { "ccdc",2 },
 { "et",  2 },
};

/* these are general xlat_stream contexted used by video (vrawunix) for all I/O */
struct xlcontext *outctx=NULL, *inctx=NULL;
static char vlangfile[MAX_FILENAME_LEN];

static int xlat_init_flag = -1;
	
#else /* ! __CHINESE__ */

extern struct xlcontext *outctx, *inctx;
extern char tablepath[];

extern struct xlat_s charsets[];

extern char errbuf[];
extern char f1name[];
extern char f2name[];
extern char srctablefile[];
extern char interxlatfile[];

extern char xlatfile[];

#ifdef unix
extern int f1size, f2size;
#endif

extern unsigned char *tablebuf, *xlatbuf, *f1indbuf, *f2indbuf;
extern unsigned char *f1tof2buf, *f2tof1buf;

extern char errbuf[];

extern int chtab_init;

#endif /* ! __CHINESE__ */

/*
**	Function Prototypes
*/

#endif /* vchinese_H */

/*
**	History:
**	$Log: vchinese.h,v $
**	Revision 1.6  1995-04-25 05:50:24-04  gsl
**	drcs state V3_3_15
**
 * Revision 1.5  1995/04/17  11:44:02  gsl
 * drcs state V3_3_14
 *
 * Revision 1.4  1995/04/10  09:10:02  gsl
 * fixed compiler warnings and added headers
 *
**
*/
