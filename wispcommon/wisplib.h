/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		wisplib.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Function prototypes for WISPLIB subs
**
*/

#ifndef wisplib_H
#define wisplib_H
/*
**	Includes
*/
#include <sys/types.h>
#include "intdef.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/

/* acustubs.c */
void call_acucobol();

/* backgrnd.c */
int wbackground(void);

/* bit_x.c */
void wsetstat(uint4 *smask, uint4 *cmask, uint4 *src);			/* Used to set/clear bits in a status field.	*/
void bit_off(unsigned char *mask, unsigned char *src);
void lbit_off(uint4 *mask, uint4 *src);
void xx2byte(char *src,char *dst);
void WMEMCPY(char *dst,char *src,short *len);				/* COBOL call able memcpy			*/

/* bits.c */
void BITPACK(unsigned char *in_ptr,unsigned char *out_ptr, int4 *in_len);
void BITUNPK(unsigned char *in_ptr, unsigned char *out_ptr, int4 *in_len);

/* btransl.c */
int *btransl(char *in_string,short *len_flds,char *eb_as);

/* coblink.c */
void COBLINK(const char *progname);

/* date.c */
#ifndef DATE_C
void DATE(char* func, ...);
void DATE2(char* func, ...);
void DATE4(char* func, ...);
void DATE6(char* func, ...);
#endif

/* day.c */
void DAY(char* date, int4* dow);						/* Return the day of the week for a given date.	*/

/* edestubs.c */
int  gen_ncpfkey( int type, char** wsb, int num_chars, int* st_win, int* end_win);
int  ws_bar_menu( int curset, int vr, int vc, int ak, int ar, unsigned char* nm, int dp );
int  nc_pop_menu( int* filling, unsigned char* terminate_list, unsigned char* no_mod, unsigned char* pfkey );

/* errgparm.c */
void err_getparm(char* prname, char* messid, char* issuer, 
		 char* msg1, char* msg2, char* msg3, char* msg4, 
		 char* msg5, char* msg6, char* msg7, char* msg8);

/* extract.c */
void EXTRACT(const char* first, ...);
int4 workstation(void);

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

#ifdef INT8_DEFINED
int WL_stat_size_int8(const char* name, INT8 *size);
#endif

/* filecopy.c */
void FILECOPY();

/* filgparm.c */
void file_getparm2(int4 f_mode, char *file, char *lib, char *vol, char *prname, 
		char *issuer, int4 *entry_mode, char *getparm_type, char *native_path,
		char *msg1, char *msg2, char *pfkey_rcvr, char intv_type,	
		char *orig_file, char *prtclass, int4 *form, int4* copies);

/* find.c */
void FIND(char* the_file, char* the_lib, char* the_vol, int4 *starter, int4 *counter, char* receiver, ...);

/* findexts.c */
int findexts(char* basename, char* base_ext);

/* filesize.c */
long WL_filesize(const char* path);

/* getparm.c */
void GETPARM();
int use_last_prb();

/* greclen.c */
int greclen(const char* file_name);						/* Get record length of fixed_size record file. */

/* initglbs.c */
int initglbs(char *wisprunname);							/* Init GLOBALS				*/
int init_watcom(void);
void wexitint(int sig);
void wexitbug(int sig);
void wexitsig(int sig);
void wisp_signal_handler(void);

/* link.c */
#ifndef LINK_C
void LINK2(const char *program, int4 len1, ...);
#endif

int findrun(char file[8],char lib[8], char vol[6], char* native, char linktype[1]);
int firstproc(char* filepath);

/* linksubs.c */
void writeunixlink();
void readunixlink();
void LINKGARG();
void ISRUNUSING();
void LINKPARG();
void LINKNARG();
void ACUGARGS();
void ACUPARGS();
void ACUNARGS();
void writevmslink();
void readvmslink();
void VMSGARGS();
void VMSNARGS();
void wclink();
int4 vms_to_wang_codes ( int4 vms_code );				/* Convert VMS return code to WANG LINK return code	*/

void swap_put ( int4* destination, int4 new_value );


/* longarg.c */
int longargtest(char *ptr, int sigbytes);

/* makepath.c */
int makepath(const char* fullpath );

/* mngfile.c */
int mngfile(void);

/* msdosfns.c */
#if defined(MSDOS) || defined(WIN32)
int getuid(void);
const char	*ttyname(int fd);
void PARSECOM( char* com_line, char* com_link );
#endif

/* nextfile.c */
char *nextfile(char* path, char** context);
char *s_nextfile(char* path, int* first);

/* pwdname.c */
void passwdname(char *name);

/* readfdr.c */
void READFDR(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...);
void READFDR4(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...);

#define READFDR_RC_0_SUCCESS			0
#define READFDR_RC_4_VOLUME_NOT_FOUND		4
#define READFDR_RC_16_LIBRARY_NOT_FOUND		16
#define READFDR_RC_20_FILE_NOT_FOUND		20
#define READFDR_RC_24_NO_FILE_HEADER		24
#define READFDR_RC_32_STAT_ERROR		32
#define READFDR_RC_40_INVALID_INPUT_PARAM	40
#define READFDR_RC_44_IO_ERROR			44
#define READFDR_RC_68_UNKNOWN_FILE_FORMAT	68

/* rename.c */
void wrename();
int file_rename(char* old_filename, char* new_filename);

/* retcode.c */
void RETCODE(char code[3]);
void setretcode(char* wispreturncode);
void delete_retcode(void);

/* scratch.c */
int wisp_unlink(const char *filename);
void SCRATCH();

/* screen.c */
void strip_facs(		/*  with a space.			*/
	char *string_addr,	/* Address of string to be stripped.	*/
	int string_len,		/* Length of that string.		*/
	int type);		/* 0=Wang screen map (with FACs & 0x0b), 1=vchr_map */

int di_write_file(			/* Open up a printer output file.	*/
	char *text,			/* Pointer to the stuff to be printed.	*/
	int  text_len,			/* Length values.			*/
	int  rec_len,			/* Length values.			*/
	char *filelibvol, 		/* Pointer to the file to be opened.	*/
	char *def_filename);		/* Pointer to the default file name.	*/

void border_screen(
	char *work_area,   	/* Address of destination.		*/
	char *screen_image,	/* Address of screen image.		*/
	int  image_rec_size,	/* Size of the image on the screen.	*/
	int  screen_rec_size,	/* The size of the screen records.	*/
	int  screen_rec_count);	/* The number of records. (screen rows)	*/

/* setenvst.c */
#include "setenvst.h"

/* setprgid.c */
void setprogid(const char *wisp_application_name);			/* Set global variable to current program id.	*/

/* sortlink.c */
void getsortinfo(char* filetype, int4* recsize, int4** sortcode_ptr);
void SORTINFO(char* filetype, int4* recsize, int4* sortcode);

/* spawn.c */
int4 spawn2(int action, char* progname,char* message,uint4* status);

/* vdisplay.c */
int vdisplay(const char *file_name,int record_size);			/* VIDEO file display subroutine.	*/


/* wcmatch.c */
int wcmatch(char* pat, char* str, int sex, char	MATCHONE, char MATCHMANY);

/* werrpath.c */
char* werrpath(void);

/* wexith.c */
void wexith(void);
void wispexit_cleanup(void);

/* wfclose.c */
void wfclose(char* fname);

/* wgetpgrp.c */
int wgetpgrp(void);

/* wispsort.c */
void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode);
void wangsort(char *sortparms, char *filetype, int4 *recsize, int dupinorder, int4 *sortcode, int4 *returncode);

/* wispsync.c */
void WISPSHUT(void);
void WISPSYNC(void);

/* wpause.c */
void wpause(int4* hsec);
void hpause(int4 hundredths);

/* wprint.c */
void wprint(
	const char *native_fname,		/* The native filepath to print.			*/
	char	mode,				/* The printmode H, S, K, P, O.				*/
	const char *disposition,		/* The disposition DS, DX, RS (NULL if not used)	*/
						/*  if not NULL then only valid modes are H and S.	*/
	int	copies,				/* Number of copies.					*/
	char	prt_class,			/* The print class.					*/
	int	prt_form,			/* The form number.					*/
	int	*return_code);			/* The return code.					*/


/* wshelp.c */
int wsh_help(int prog_running);								/* Put up the shell screen.		*/
void get_psb_char(char def_select,char *pb_char, char *pb_select);
int wsh_progprnt(int scrn_seq_no);							/* The screen seq. no.  Starts at 1.	*/
int wsc_init(char *screen, int row, int col);						/* Initialize screen image for vwang().	*/
int wput(char *screen, int row, int col, int fac, char *text);				/* Put text and fields into screen map.	*/
int wpcen(char *screen, int row, int fac, char *text);					/* Put text and fields into screen map.	*/
int wget(char *screen, int row, int col, char *text);					/* Retreive text from screen.		*/
int ishelpactive(void);
int sethelpactive(int flag);

/* wspawn.c */
void WSPAWN(short *action,char *progname,short *name_len,char *msg,short *msg_len);

/* wswap.c */
void wswap(void *lword);			/* swap the order of the words in a longword item (for WANG routines to use)	*/
void reversebytes(void *ptr, int len);							/* Reverse the bytes.			*/
int bytenormal(void);
int4 get_swap(const int4 *src);
void put_swap(int4 *dest, int4 value);

/* wsystem.c */
int wsystem(const char* cmd);
int run_unixcommand_silent(const char* command);

/* wvaset.c */
void wvaset(int4 *x);
int va_count();


#endif /* wisplib_H */

/*
**	History:
**	$Log: wisplib.h,v $
**	Revision 1.38.2.3  2002/10/09 21:17:35  gsl
**	Huge file support
**	
**	Revision 1.38.2.2  2002/10/09 19:20:37  gsl
**	Update fexists.c to match HEAD
**	Rename routines WL_xxx for uniqueness
**	
**	Revision 1.38.2.1  2002/10/03 13:49:50  gsl
**	Change wmemcpy to WMEMCPY
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
**	Fix get_swap()
**
**	Revision 1.28  1998-05-14 09:50:29-04  gsl
**	remove wfvision.h and wfiledis.h stuff
**
**	Revision 1.27  1998-03-13 14:42:34-05  gsl
**	Make wsystem() a const
**
**	Revision 1.26  1997-10-23 15:23:00-04  gsl
**	Add use_internal_display() and link_display()
**	And add "const" where needed
**
**	Revision 1.25  1997-10-21 09:51:09-04  gsl
**	fi setprogid()
**
**	Revision 1.24  1997-10-17 11:35:03-04  gsl
**	Add get_swap() and put_swap()
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
**	fix wswap() and reversebytes()
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
