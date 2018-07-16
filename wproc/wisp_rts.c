#ifdef WISP_LIB_STUBS

// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : wisp_rts.c
// Author : George Soules
// Date   : 17 March 1992

#include <stdio.h>
#include <stdlib.h>

char *global_p1;
char *global_p2;
char *global_p3;

#pragma argsused
void initwisp2(
   char *tranver,
   char *libver,
   char *cobtype,
   char *applname,
   char *runname,
   int_32 *swap,
   int_32 *errflag)
{
   printf("WISP version %c initialized\n", libver[0]);
}


void EXTRACT(char *keyword, void *receiver) {
   printf("EXTRACT(\"%s\", %p)\n", keyword, receiver);
   ((char *) receiver)[0] = 'W';
   ((char *) receiver)[1] = 'a';
   ((char *) receiver)[2] = 'n';
   ((char *) receiver)[3] = 'g';
   ((char *) receiver)[4] = '\0';
/***
   ((char *) receiver)[0] = '\x1';
   ((char *) receiver)[1] = '\x0';
   ((char *) receiver)[2] = '\x0';
   ((char *) receiver)[3] = '\0';
***/
}


#pragma argsused
void FIND(
   char *file,
   char *library,
   char *volume,
   in_t32 *start_count,
   int_32 *count,
   char *receiver,
   int_32 *f_count)
{
   printf("FIND(\"%s\", \"%s\", \"%s\", %ld, %ld, ...)\n",
      file, library, volume, *((long*) start_count), *(long*) count) );
}


#pragma argsused
void LINKMF(
   void *progname,
   void *prognamelen,
   void *linktype,
   void *linktypelen,
   void *library,
   void *librarylen,
   void *volume,
   void *volumelen,
   void *parmcnt,
   void *parmcntlen,
   void *parm01, void *parmlen01,
   void *parm02, void *parmlen02,
   ...)
/*
   void *parm03, void *parmlen03,
   void *parm04, void *parmlen04,
   void *parm05, void *parmlen05,
   void *parm06, void *parmlen06,
   void *parm07, void *parmlen07,
   void *parm08, void *parmlen08,
   void *parm09, void *parmlen09,
   void *parm10, void *parmlen10,
   void *parm11, void *parmlen11,
   void *parm12, void *parmlen12,
   void *parm13, void *parmlen13,
   void *parm14, void *parmlen14,
   void *parm15, void *parmlen15,
   void *parm16, void *parmlen16,
   void *parm17, void *parmlen17,
   void *parm18, void *parmlen18,
   void *parm19, void *parmlen19,
   void *parm20, void *parmlen20,
   void *parm21, void *parmlen21,
   void *parm22, void *parmlen22,
   void *parm23, void *parmlen23,
   void *parm24, void *parmlen24,
   void *parm25, void *parmlen25,
   void *parm26, void *parmlen26,
   void *parm27, void *parmlen27,
   void *parm28, void *parmlen28,
   void *parm29, void *parmlen29,
   void *parm30, void *parmlen30,
   void *parm31, void *parmlen31,
   void *parm32, void *parmlen32,
   void *cancelexit,
   void *cancelexitlen,
   void *compcode,
   void *compcodelen,
   void *retcode,
   void *retcodelen)
*/
{
   int_32 i = 987654321;
   char c = 'y';

   printf("Before LINKMF(\"%s\" ... %ld, %ld, %ld, %s, %ld ...)\n",
      progname, *((long *) parmcnt), *((long *) parm01), (long) parmlen01, (char *) parm02, (long) parmlen02);

   *((int_32 *) parm01) = i;
   ((char *) parm02)[1] = c;

   printf("After  LINKMF(\"%s\" ... %ld, %ld, %ld, %s, %ld ...)\n",
      progname, *((long *) parmcnt), *((long *) parm01), (long) parmlen01, (char *) parm02, (long) parmlen02);

/*
   char *ltype = " ";
   ltype[0] = ((char *) linktype)[0];
   printf("LINKMF(\"%s\", %ld, \"%s\", %ld, \"%s\", %ld, \"%s\", %ld, %ld, %ld, %ld, %ld, %s, %ld...)\n",
      progname, (long) prognamelen, ltype, (long) linktypelen, library,
      (long) librarylen, volume, (long) volumelen, *((long *) parmcnt),
      (long) parmcntlen, (long) parm01, (long) parmlen01, (char *) parm02, (long) parmlen02);
*/
}


#pragma argsused
void LINKGARG(
   char  *pname,
   short *pcount,
   char  *p1,  char *p2,  char *p3,  char *p4,
   char  *p5,  char *p6,  char *p7,  char *p8,
   char  *p9,  char *p10, char *p11, char *p12,
   char  *p13, char *p14, char *p15, char *p16,
   char  *p17, char *p18, char *p19, char *p20,
   char  *p21, char *p22, char *p23, char *p24,
   char  *p25, char *p26, char *p27, char *p28,
   char  *p29, char *p30, char *p31, char *p32)
{
   int_32 l;
   printf("Before LINKGARG(\"%s\", %d, \"%s\", \"%s\", \"%s\" ...)\n",
      pname, *pcount, p1, p2, p3);
   l = 5;
   *pcount = 3;
   strcpy(p1, "Arg 1"); /*GET RID OF NULL*/ p1[5] = ' ';
   *((int_32 *) p2) = l;
   strcpy(p3, "This is the last arg"); /*GET RID OF NULL*/ p3[20] = ' ';
   printf("After  LINKGARG(\"%s\", %d, \"%s\", %ld, \"%s\" ...)\n",
      pname, *pcount, p1, *((long *) p2), p3);
   global_p1 = p1;
   global_p2 = p2;
   global_p3 = p3;
}


void LINKPARG() {
   printf("LINKPARG(\"%s\", %ld, \"%s\" ...)\n",
      global_p1, *((long *) global_p2), global_p3);
}


void LOGOFF() {
   printf("LOGOFF()\n");
   exit(0);
}


void PRINT(
   char *file,
   char *library,
   char *volume,
   char *mode,
   char *disposition,
   int_32 *copies,
   char *fclass,
   int_32 *form,
   int_32 *retcode)
{
   *retcode = 0;
   printf("PRINT(\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %ld, \"%s\", %ld, %ld)\n",
      file, library, volume, mode, disposition, *(long*) copies), fclass, *(long*) form), 0L);
   *retcode = 123;
}

#pragma argsused
void PUTPARM(
   void *function,
   ...)
/* void *usagecnt,
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
   void *retcode)
*/
{
// printf("PUTPARM(\"%s\", %ld, \"%s\", %ld, ...)\n",
//    function, *((long *) usagecnt), prname, *((long *) keycnt));
   printf("PUTPARM(\"%s\", ...)\n", function);
}


void wrename(
   char *type,
   char *file,
   char *library,
   char *volume,
   char *newfile,
   char *newlib,
   int_32 *retcode)
{
   printf("wrename(\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %ld)\n",
      type, file, library, volume, newfile, newlib, 0L);
   *retcode = 101;
}


void SCRATCH(
   char *type,
   char *file,
   char *library,
   char *volume,
   int_32 *retcode)
{
   printf("SCRATCH(\"%s\", \"%s\", \"%s\", \"%s\", %ld)\n",
      type, file, library, volume, 0L);
   *retcode = 102;
}


void SET(char *keyword, void *value) {
   printf("SET(\"%s\", \"%s\")\n", keyword, value);
}


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
   int_32 *retcode)
{
   printf("SUBMIT(\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %ld, \"%s\", %ld)\n",
      file, library, volume, name, status, disp, jclass, abort, *(long*) time), limit, 0L);
   *retcode = 999;
}


#pragma argsused
int findrun(char *file, char *lib, char *vol, char *nativepath, char *linktype) {
   printf("findrun(\"%s\", \"%s\", \"%s\", ...)\n", file, lib, vol);
   strcpy(nativepath, "/usr/lexical/dummy.src");
   return 0;
}


int runtype(char *nativepath) {
   printf("runtype(\"%s\")\n", nativepath);
   return 1;
}


int newlevel() {
   printf("newlevel()\n");
   return 0;
}


int oldlevel() {
   printf("oldlevel()\n");
   return 0;
}


int firstproc(char *a_proc_name) {
   printf("firstproc(\"%s\")\n", a_proc_name);
   return 0;
}


void wvaset(long *arg_count) {
   printf("wvaset(%ld)\n", *(long*) arg_count) );
}

#endif
