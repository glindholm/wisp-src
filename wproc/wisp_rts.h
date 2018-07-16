// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : wisp_rts.h
// Author : George Soules
// Date   : 17 March 1992

#ifndef WISP_RTS__H
#define WISP_RTS__H

#ifdef __cplusplus
extern "C" {
#endif

void initglbs(const char *);
void vwang_title(const char *);

void wvaset(int_32 *x);
void wswap(int_32 *arg);

void EXTRACT(char *keyword, void *receiver);

void FIND(
   char *file,
   char *library,
   char *volume,
   int_32 *start_count,
   int_32 *count,
   char *receiver,
   int_32 *f_count);

void LINK2(
   void *progname,  int_32 prognamelen,
   void *linktype,  int_32 linktypelen,
   void *library,   int_32 librarylen,
   void *volume,    int_32 volumelen,
   void *parmcnt,   int_32 parmcntlen,
   void *parm01,    int_32 parmlen01,
   void *parm02,    int_32 parmlen02,
   ...
   );
/*
   void *parm03,    int_32 parmlen03,
   void *parm04,    int_32 parmlen04,
   void *parm05,    int_32 parmlen05,
   void *parm06,    int_32 parmlen06,
   void *parm07,    int_32 parmlen07,
   void *parm08,    int_32 parmlen08,
   void *parm09,    int_32 parmlen09,
   void *parm10,    int_32 parmlen10,
   void *parm11,    int_32 parmlen11,
   void *parm12,    int_32 parmlen12,
   void *parm13,    int_32 parmlen13,
   void *parm14,    int_32 parmlen14,
   void *parm15,    int_32 parmlen15,
   void *parm16,    int_32 parmlen16,
   void *parm17,    int_32 parmlen17,
   void *parm18,    int_32 parmlen18,
   void *parm19,    int_32 parmlen19,
   void *parm20,    int_32 parmlen20,
   void *parm21,    int_32 parmlen21,
   void *parm22,    int_32 parmlen22,
   void *parm23,    int_32 parmlen23,
   void *parm24,    int_32 parmlen24,
   void *parm25,    int_32 parmlen25,
   void *parm26,    int_32 parmlen26,
   void *parm27,    int_32 parmlen27,
   void *parm28,    int_32 parmlen28,
   void *parm29,    int_32 parmlen29,
   void *parm30,    int_32 parmlen30,
   void *parm31,    int_32 parmlen31,
   void *parm32,    int_32 parmlen32,
   void *cancelexit, int_32 cancelexitlen,
   void *compcode,  int_32 compcodelen,
   void *retcode,   int_32 retcodelen);
*/

void LINKGARG(
   char  *pname,
   int_16 *pcount,
   char  *p1,  char *p2,  char *p3,  char *p4,
   char  *p5,  char *p6,  char *p7,  char *p8,
   char  *p9,  char *p10, char *p11, char *p12,
   char  *p13, char *p14, char *p15, char *p16,
   char  *p17, char *p18, char *p19, char *p20,
   char  *p21, char *p22, char *p23, char *p24,
   char  *p25, char *p26, char *p27, char *p28,
   char  *p29, char *p30, char *p31, char *p32);

void LINKPARG();

void LOGOFF();

void PRINT(
   char   *file,
   char   *library,
   char   *volume,
   char   *mode,
   char   *disposition,
   int_32 *copies,
   char   *fclass,
   int_32 *form,
   int_32 *retcode);

void PUTPARM(
   void *function,
   ...);
/*
   void *usagecnt,
   void *prname,
   void *keycnt,
   void *keyword01, void *keyval01, void *keylen01,
   void *keyword02, void *keyval02, void *keylen02,
   void *keyword03, void *keyval03, void *keylen03,
   void *keyword04, void *keyval04, void *keylen04,
   void *keyword05, void *keyval05, void *keylen05,
   void *keyword06, void *keyval06, void *keylen06,
   void *keyword07, void *keyval07, void *keylen07,
   void *keyword08, void *keyval08, void *keylen08,
   void *keyword09, void *keyval09, void *keylen09,
   void *keyword10, void *keyval10, void *keylen10,
   void *keyword11, void *keyval11, void *keylen11,
   void *keyword12, void *keyval12, void *keylen12,
   void *keyword13, void *keyval13, void *keylen13,
   void *keyword14, void *keyval14, void *keylen14,
   void *keyword15, void *keyval15, void *keylen15,
   void *keyword16, void *keyval16, void *keylen16,
   void *keyword17, void *keyval17, void *keylen17,
   void *keyword18, void *keyval18, void *keylen18,
   void *keyword19, void *keyval19, void *keylen19,
   void *keyword20, void *keyval20, void *keylen20,
   void *keyword21, void *keyval21, void *keylen21,
   void *keyword22, void *keyval22, void *keylen22,
   void *keyword23, void *keyval23, void *keylen23,
   void *keyword24, void *keyval24, void *keylen24,
   void *keyword25, void *keyval25, void *keylen25,
   void *keyword26, void *keyval26, void *keylen26,
   void *keyword27, void *keyval27, void *keylen27,
   void *keyword28, void *keyval28, void *keylen28,
   void *keyword29, void *keyval29, void *keylen29,
   void *keyword30, void *keyval30, void *keylen30,
   void *keyword31, void *keyval31, void *keylen31,
   void *keyword32, void *keyval32, void *keylen32,
   void *keyword33, void *keyval33, void *keylen33,
   void *keyword34, void *keyval34, void *keylen34,
   void *keyword35, void *keyval35, void *keylen35,
   void *keyword36, void *keyval36, void *keylen36,
   void *keyword37, void *keyval37, void *keylen37,
   void *keyword38, void *keyval38, void *keylen38,
   void *keyword39, void *keyval39, void *keylen39,
   void *keyword40, void *keyval40, void *keylen40,
   void *pfkey,
   void *putlabel,
   void *reflabel,
   void *cleanup,
   void *retcode);
*/

void wrename(
   char *type,
   char *file,
   char *library,
   char *volume,
   char *newfile,
   char *newlib,
   int_32 *retcode);

void SCRATCH(
   char *type,
   char *file,
   char *library,
   char *volume,
   int_32 *retcode);

void SET(char *keyword, void *value);

void SUBMIT(
   char *file,
   char *library,
   char *volume,
   char *name,
   char *status,
   char *disp,
   char *jclass,
   char *abort,
   int_32 *time,
   char *limit,
   int_32 *retcode);

void SETSUBMIT(
	int_16	*count,
	int_16	*len1,
	void	*arg1,
	int_16	*len2,
	void	*arg2,
	int_16	*len3,
	void	*arg3,
	int_16	*len4,
	void	*arg4,
	int_16	*len5,
	void	*arg5,
	int_16	*len6,
	void	*arg6,
	int_16	*len7,
	void	*arg7,
	int_16	*len8,
	void	*arg8);

int findrun(char *file, char *lib, char *vol, char *nativepath, char* linktype);

int runtype(char *nativepath);

int newlevel();
int oldlevel();

int firstproc(char *a_proc_name);

int wbackground();

int werrlog(int, ...);

int werr_write(const char *s);

const char *wispenvpath(void);
const char *wispconfigdir(void);

void wispexit_cleanup(void);

#ifdef __cplusplus
}
#endif

#endif

