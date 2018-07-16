/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
**	File:		vdispidx.h
**
**	Purpose:        header for defines and data of vdispidx
**
**
**	History:
**	04/29/93        created. JEC
**
*/

#ifndef VDISPIDX_H
#define VDISPIDX_H



#endif /* VDISPIDX_H */

/* defines used by file_type() */
#define VD_TYPE_VISION 1
#define VD_TYPE_MF 2
#define VD_TYPE_TEXT 3

#define VD_ERR_OPEN -1
#define VD_ERR_READ -2

#define ACU_B 0x10121416
#define ACU_L 0x16141210
#define MF_CISAM_B 0
#define MF_CISAM_L 1

/* the following macro allows easier prototyping for compatibility with non-ANSI C compilers */
#ifndef EXFUN
# ifdef __STDC__
#  define EXFUN(name, proto)              name proto
# else 
#  define EXFUN(name, proto)              name()
# endif /* __STDC__ */
#endif /* EXFUN */

#ifdef VDISPIDX_C

/* struct for management of index file types */
struct idxfile
{
	int4 fd;                                 /* handle used to refer to file (ie, int4 from i_open()) */
	int keyoffs;				 /* offset of primary key */
	int keylen;				 /* length of primary key */
	int reclen;				 /* length of records */
	int reccnt;				 /* # of records in file */
	int recs_per_page;			 /* computed records per page */

	int4 EXFUN((*open),  (char *filename));					 /* routine to open this idx file type */
        void EXFUN((*info),  (struct idxfile *file));				 /* routine to get file info */
	int  EXFUN((*next),  (struct idxfile *file, char *buffer));		 /* read next routine */
	int  EXFUN((*first), (struct idxfile *file, char *buffer));		 /* start first and read routine */
	int  EXFUN((*keyed), (struct idxfile *file, char *buffer,char *key));	 /* find and read a certain key */
};

static char *reclist=NULL;

/*
 *  vwang stuff 
 */
static unsigned char vwang_fkeys[66];
typedef unsigned char byte;
struct order_area 
{
	byte rownum;
	byte wcc;
	byte cursor_col;
	byte cursor_row;
};

struct vwang_screen
{
	struct order_area oa;
	unsigned char screen[80*24];
};

#define ST_OK 0
#define ST_NOT_FOUND 1
#define ST_EOF 2

#define VD_READ_FIRST 1
#define VD_READ_NEXT 2
#define VD_READ_KEYED 3
#define VD_GET_KEY 4
#define VD_DO_NOTHING 5
#define VD_EXIT_DO_DISP 6
#define VD_REREAD_CURRENT 7
#define VD_GET_TEXT 8
#define VD_SEARCH 9

#define STATE(status) (status & 0x000000ff)
#define SETSTATE(status,value) status &= ~0x000000ff ; status |= value 
#define CHKFLAG(status,value) ((status & value)!=0)
#define SETFLAG(status,value) status |= value
#define CLRFLAG(status,value) status &= ~value

#define KEY_INP_UPPER  0x00000100
#define KEY_INVALID    0x00000200
#define DISP_MODE_HEX  0x00000400
#define SEARCH_ANY     0x00000800
#define TEXT_NOT_FOUND 0x00001000

#define INSERTSCN(scr,line,col,str) memcpy(&(scr).screen[((line)*80)+col],str,strlen(str))

static struct vwang_screen  main_scr, get_key_scr;

static char *recbuf=NULL;

char vd_text_buf[65];

static int at_eof_flag=FALSE, at_first=TRUE;
static int rec_disp_start = 0;

#endif
/*
**	History:
**	$Log: vdispidx.h,v $
**	Revision 1.5  1996/08/19 22:33:03  gsl
**	drcs update
**	
**
**
*/
