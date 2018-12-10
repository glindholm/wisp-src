/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wisplib.h
**
**	Project:	WISPLIB
**
**	Purpose:	Function prototypes for WISPLIB subs
**
*/

#ifndef wisplib_H
#define wisplib_H
/*
**	Includes
*/
#include <time.h>
#include <sys/types.h>
#include "intdef.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int wisp_acu_cobol();
int wisp_mf_cobol();

/* backgrnd.c */
#define wbackground	WL_wbackground
int WL_wbackground(void);

/* bit_x.c */
void WSETSTAT(const uint4 *smask, const uint4 *cmask, uint4 *src);	/* Used to set/clear bits in a status field.	*/
void BIT_OFF(const unsigned char *mask, unsigned char *src);
void LBIT_OFF(const uint4 *mask, uint4 *src);
void BIT_ON(const unsigned char *mask,unsigned char *src);
void LBIT_ON(const uint4 *mask,uint4 *src);
void BIT_TEST(const unsigned char *mask, const unsigned char *src, char *value);
void W99TOX(const char *src,char *dst);
void WMEMCPY(char *dst, const char *src, const short *len); 		/* COBOL call able memcpy			*/
void WGETCURPOS(const unsigned char mybytes[],short int mywords[]);
void WSETFACBLINK(unsigned char *fac);

/* date.c */
int WL_useoldvsdate(void);
unsigned int WL_currentyear(void);
unsigned int WL_yypivotyear(void);
unsigned int WL_convertyy2yyyy(unsigned int yy_value, unsigned int currentyear, unsigned int yy_pivot);

/* errgparm.c */
void WL_err_getparm(char* prname, char* messid, char* issuer, 
		 char* msg1, char* msg2, char* msg3, char* msg4, 
		 char* msg5, char* msg6, char* msg7, char* msg8);

/* extract.c */
int4 WL_workstation(void);

/* fexists.c */
#define fexists		WL_fexists
int WL_fexists(const char* name);
int WL_fcanread(const char* name);
int WL_isafile(const char* name);
int WL_isadir(const char* name);
long WL_inode(const char* name);

#ifdef WIN32
typedef unsigned short mode_t;
#endif

int WL_stat_mode(const char* name, mode_t *mode);
int WL_stat_ctime(const char* name, time_t *create_time);
int WL_stat_mtime(const char* name, time_t *mod_time);
int WL_stat_size_long(const char* name, long *size);

#ifdef INT64_DEFINED
int WL_stat_size_int8(const char* name, INT64 *size);
#endif


/* filecopy.c */
void FILECOPY(const char* arg1_infile,
	      const char* arg2_inlib,
	      const char* arg3_invol,
	      const char* arg4_outfile,
	      ...);

/* findexts.c */
int WL_findexts(char* basename, char* base_ext);
int WL_matchnative(char *native_vol, char *native_lib, char *native_file, char *native_ext, int nomodext, int is_lib );

/* filesize.c */
long WL_filesize(const char* path);

/* getparm.c */
void WL_use_last_prb(void);

/* initglbs.c */
int  WL_initglbs(const char *wisprunname);							/* Init GLOBALS				*/
void WL_wexitint(int sig);
void WL_wexitbug(int sig);
void WL_wexitsig(int sig);
void wisp_signal_handler(void);

int WL_findrun(char file[8],char lib[8], char vol[6], char* native, char linktype[1]);
int WL_firstproc(char* filepath);

/* linksubs.c */
void LINKGARG();
void ISRUNUSING();
void LINKPARG();
void LINKNARG();
void ACUGARGS();
void ACUPARGS();
void ACUNARGS();


/* longarg.c */
int WL_longargtest(char *ptr, int sigbytes);

/* makepath.c */
#define makepath WL_makepath
int WL_makepath(const char* fullpath );

/* mngfile.c */
int WL_mngfile(void);

/* msdosfns.c */
#if defined(WIN32)
#define getuid	WL_getuid
#define ttyname	WL_ttyname
int		 WL_getuid(void);
const char	*WL_ttyname(int fd);
#endif

/* nextfile.c */
char *WL_nextfile(const char* path, char** context);
char *WL_s_nextfile(const char* path, int* first);

/* pwdname.c */
void WL_passwdname(char *name);

/* rename.c */
int WL_rename(const char* old_filename, const char* new_filename);

/* retcode.c */
#define WANG_RETCODE_LEN 3
void RETCODE(char code[WANG_RETCODE_LEN]);
void SETRETCODE(const char wispreturncode[WANG_RETCODE_LEN]);
void WL_delete_retcode(void);
int  WL_get_internal_retcode(void);
void WL_set_internal_retcode(unsigned int rc);

/* scratch.c */
int wisp_unlink(const char *filename);

/* setenvst.c */
#include "setenvst.h"

/* setprgid.c */
void WL_setprogid(const char *wisp_application_name);			/* Set global variable to current program id.	*/

/* sortlink.c */
void WL_getsortinfo(char* filetype, int4* recsize, int4** sortcode_ptr);

/* vdisplay.c */
int WL_vdisplay(const char *file_name,int record_size);			/* VIDEO file display subroutine.	*/


/* wcmatch.c */
int WL_wcmatch(char* pat, char* str, int sex, char MATCHONE, char MATCHMANY);

/* werrpath.c */
char* WL_werrpath(void);

/* wexith.c */
void WL_wexith(void);
void wispexit_cleanup(void);

void WISPEXIT(void);

/* wfclose.c */
void WFCLOSE(const char* fname);

/* wgetpgrp.c */
int WL_wgetpgrp(void);

/* wispsort.c */
void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode);
void WL_wangsort(char *sortparms, char *filetype, int4 *recsize, int dupinorder, int4 *sortcode, int4 *returncode);

/* wispsync.c */
void WISPSHUT(void);
void WISPSYNC(void);

/* wpause.c */
void WL_hpause(int4 hundredths);

/* wprint.c */
void WL_wprint(
	const char *native_fname,		/* The native filepath to print.			*/
	char	mode,				/* The printmode H, S, K, P, O.				*/
	const char *disposition,		/* The disposition DS, DX, RS (NULL if not used)	*/
						/*  if not NULL then only valid modes are H and S.	*/
	int	copies,				/* Number of copies.					*/
	char	prt_class,			/* The print class.					*/
	int	prt_form,			/* The form number.					*/
	int	*return_code);			/* The return code.					*/

/* wscreen.c */
void WSCREEN(
	unsigned char *screen,								/* Pointer to the screen structure.	*/
	unsigned char *function,							/* Function to execute (for vwang)	*/
	unsigned char *blockarg,							/* Screen structure block.		*/
	unsigned char *lines,								/* Number of lines to write.		*/
		 char *pfkeys,								/* Termination PF key string.		*/
		 char *on_pfkeys,							/* pfkeys used in ON PFKEY		*/
		 char *pfkey_ret,							/* Place to return key in.		*/
	unsigned char *file_stat);							/* place to return file status.		*/

/* wshelp.c */
int  WL_wsh_help(int prog_running);							/* Put up the shell screen.		*/
void WL_get_psb_char(char def_select,char *pb_char, char *pb_select);
int  WL_wsc_init(unsigned char *screen, int row, int col);					/* Initialize screen image for vwang().	*/
int  WL_put_screen_text(unsigned char *screen, int row, int col, unsigned int fac, char *text);		/* Put text and fields into screen map.	*/
int  WL_put_screen_text_centered(unsigned char *screen, int row, unsigned int fac, char *text);		/* Put text and fields into screen map.	*/
int  WL_get_screen_text(unsigned char *screen, int row, int col, char *text);			/* Retreive text from screen.		*/
int  WL_ishelpactive(void);
int  WL_sethelpactive(int flag);

/* wswap.c */
void WSWAP(void *lword);			/* wswap() -> WSWAP() -> WL_wswap()	*/
void WL_wswap(void *lword);			/* swap the order of the words in a longword item (for WANG routines to use)	*/
void WL_reversebytes(void *ptr, int len);	/* Reverse the bytes.			*/
int  WL_bytenormal(void);
int4 WL_get_swap(const int4 *src);
void WL_put_swap(void *dest, int4 value);

/* wsystem.c */
int WL_wsystem(const char* cmd);
int WL_run_unixcommand_silent(const char* command);

/* wvaset.c */
void WVASET(int *x);
void WL_set_va_count(int x);
int  WL_va_count();
void WVASETV(int4 x);	/* MF: CALL "WVASETV" USING VALUE nn */


#endif /* wisplib_H */

/*
**	History:
**	$Log: wisplib.h,v $
**	Revision 1.85  2007/07/31 16:51:07  gsl
**	Change INT8 to INT64 to avoid conflicts on WIN32
**	
**	Revision 1.84  2003/07/11 16:58:31  gsl
**	Add WSWAP() as a replacement for wswap()
**	
**	Revision 1.83  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.82  2003/05/22 14:08:40  gsl
**	Add WANG_RETCODE_LEN
**	
**	Revision 1.81  2003/04/21 20:02:29  gsl
**	WL_initglbs() takes a const char*
**	WL_use_last_prb() is void
**	
**	Revision 1.80  2003/03/27 21:21:54  gsl
**	Pivot year routines for DATE6 support
**	
**	Revision 1.79  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.78  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.77  2003/01/30 21:11:22  gsl
**	Change RENAME to use stdarg.h
**	
**	Revision 1.76  2003/01/30 19:42:10  gsl
**	change WL_file_rename() to WL_rename() cause also used for directories
**	
**	Revision 1.75  2003/01/30 15:20:57  gsl
**	WL_file_rename() change args to const and add tracing
**	
**	Revision 1.74  2003/01/29 21:50:08  gsl
**	Switch to use vssubs.h
**	
**	Revision 1.73  2003/01/29 16:21:12  gsl
**	Add SET2()
**	
**	Revision 1.72  2003/01/29 15:10:35  gsl
**	Fix SET() proto-type
**	
**	Revision 1.71  2003/01/28 20:15:01  gsl
**	Change MF to use WVASETV
**	
**	Revision 1.70  2003/01/17 18:00:48  gsl
**	Switch SCREEN to use stdarg.h
**	
**	Revision 1.69  2003/01/16 19:40:32  gsl
**	Change SCRTACH to use stdarg.h
**	
**	Revision 1.68  2003/01/16 17:27:45  gsl
**	Switch to stdarg
**	
**	Revision 1.67  2002/10/18 19:14:07  gsl
**	Cleanup
**	
**	Revision 1.66  2002/10/08 15:44:38  gsl
**	Change int8 to INT8 to avoid conficts
**	
**	Revision 1.65  2002/10/04 21:00:53  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.64  2002/08/01 15:07:37  gsl
**	type warnings
**	
**	Revision 1.63  2002/08/01 14:45:12  gsl
**	type warnings
**	
**	Revision 1.62  2002/08/01 02:44:53  gsl
**	fix type warning
**	
**	Revision 1.61  2002/07/31 20:24:28  gsl
**	globals
**	
**	Revision 1.60  2002/07/30 22:00:34  gsl
**	WSETFACBLINK
**	
**	Revision 1.59  2002/07/30 19:12:41  gsl
**	SETRETCODE
**	
**	Revision 1.58  2002/07/29 21:13:27  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.57  2002/07/26 19:20:46  gsl
**	bit routine globals (bit_on -> BIT_ON) etc.
**	
**	Revision 1.56  2002/07/26 18:19:17  gsl
**	wscreen -> WSCREEN
**	
**	Revision 1.55  2002/07/23 21:24:54  gsl
**	wrename -> RENAME
**	
**	Revision 1.54  2002/07/23 02:57:50  gsl
**	wfclose -> WFCLOSE
**	
**	Revision 1.53  2002/07/20 00:31:18  gsl
**	remove unused lbit_test
**	
**	Revision 1.52  2002/07/19 22:07:17  gsl
**	Renaming cobol api routines for global uniqueness
**	
**	Revision 1.51  2002/07/16 16:24:58  gsl
**	Globals
**	
**	Revision 1.50  2002/07/12 19:10:25  gsl
**	Global unique WL_ changes
**	
**	Revision 1.49  2002/07/12 17:17:03  gsl
**	Global unique WL_ changes
**	
**	Revision 1.48  2002/07/11 20:29:21  gsl
**	Fix WL_ globals
**	
**	Revision 1.47  2002/07/11 16:11:45  gsl
**	Fix warnings
**	
**	Revision 1.46  2002/07/10 21:06:35  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.45  2002/07/10 04:27:39  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.44  2002/07/10 01:36:00  gsl
**	fix wmemcpy to WMEMCPY
**	
**	Revision 1.43  2002/06/26 06:25:12  gsl
**	Proto for matchnative
**	
**	Revision 1.42  2002/06/26 04:25:02  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.41  2002/06/26 01:42:51  gsl
**	Remove VMS code
**	
**	Revision 1.40  2002/06/25 18:18:38  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.39  2002/06/25 15:20:56  gsl
**	Remove obsolete routines
**	
**	Revision 1.38  2001/11/27 21:34:46  gsl
**	remove cuserid()
**	
**	Revision 1.37  2001-10-26 15:38:51-04  gsl
**	Add run_unixcommand_silent()
**
**	Revision 1.36  2001-10-23 14:45:17-04  gsl
**	Add READFDR RC codes
**
**	Revision 1.35  2001-10-22 10:47:27-04  gsl
**	Remove wfcisam.c proto
**
**	Revision 1.34  2001-10-22 10:13:12-04  gsl
**	Add READFDR return codes
**
**	Revision 1.33  1999-09-08 19:43:20-04  gsl
**	Add READFDR4() and update READFDR()
**
**	Revision 1.32  1999-09-08 12:03:16-04  gsl
**	Add DATE4() DATE6() and READFDR4() prototypes
**
**	Revision 1.31  1999-01-29 15:24:01-05  gsl
**	Add wisp_unlink()
**
**	Revision 1.30  1999-01-04 17:47:37-05  gsl
**	update prototypes for EXTRACT and FIND
**
**	Revision 1.29  1998-11-04 10:15:56-05  gsl
**	Fix WL_get_swap()
**
**	Revision 1.28  1998-05-14 09:50:29-04  gsl
**	remove wfvision.h and wfiledis.h stuff
**
**	Revision 1.27  1998-03-13 14:42:34-05  gsl
**	Make WL_wsystem() a const
**
**	Revision 1.26  1997-10-23 15:23:00-04  gsl
**	Add use_internal_display() and link_display()
**	And add "const" where needed
**
**	Revision 1.25  1997-10-21 09:51:09-04  gsl
**	fix SETPROGID()
**
**	Revision 1.24  1997-10-17 11:35:03-04  gsl
**	Add WL_get_swap() and WL_put_swap()
**
**	Revision 1.23  1997-05-13 16:14:56-04  scass
**	Added DATE2 to library routines
**
**	Revision 1.22  1996-12-11 16:30:16-05  gsl
**	add isafile() and isadir() to fexists.c
**
**	Revision 1.21  1996-11-11 15:22:22-08  gsl
**	Add wispexit_cleanup()
**
**	Revision 1.20  1996-11-11 13:23:55-08  gsl
**	Added delete_retcode()
**
**	Revision 1.19  1996-10-14 10:19:50-07  gsl
**	Include setenvst.h
**
**	Revision 1.18  1996-10-08 17:29:55-07  gsl
**	Remove shell_var() as it is replaced by wispcfg.h
**
**	Revision 1.17  1996-08-26 17:13:43-07  gsl
**	Made ttyname() a const
**
**	Revision 1.16  1996-08-23 14:08:18-07  gsl
**	fix prototype to make const
**
**	Revision 1.15  1996-07-24 15:46:11-07  gsl
**	Add prototypes
**
**	Revision 1.14  1996-07-24 15:09:34-07  gsl
**	Add prototypes
**
**	Revision 1.13  1996-07-18 13:22:05-07  gsl
**	Move from wisp/lib to wisp/common
**
**	Revision 1.12  1996-07-17 09:26:42-07  gsl
**	Add prototypes
**
**	Revision 1.11  1996-07-15 10:15:02-07  gsl
**	add prototypes
**
**	Revision 1.9  1996-07-10 17:17:58-07  gsl
**	add missing prototypes
**
**	Revision 1.8  1996-07-09 17:02:08-07  gsl
**	Add prototypes.
**
**	Revision 1.7  1996-07-08 16:40:53-07  gsl
**	add prototypes for NT
**
**	Revision 1.6  1996-07-08 10:15:32-07  gsl
**	fix wswap() and WL_reversebytes()
**
**	Revision 1.5  1996-07-03 16:32:59-07  gsl
**	add prototypes
**
**	Revision 1.4  1996-06-28 16:55:51-07  gsl
**	added prototypes
**
**	Revision 1.3  1996-06-28 09:19:59-07  gsl
**	Add prototypes
**
**	Revision 1.2  1996-06-26 17:19:38-07  gsl
**	Building for NT
**
**	Revision 1.1  1996-06-26 16:10:07-07  gsl
**	Initial revision
**
**
*/
