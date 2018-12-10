/* 
	Copyright (c) Shell Stream Software LLC, All rights reserved.
*/

/*
**	File:		wisp4400.c
**
**	Purpose:	Obsolete interfaces for backwards compatibility
**
**
*/

/*
getparmbuild 
vwang 
*/

void wmemcpy(dst, src, len)
char *dst;
char *src;
short *len;
{
	extern void WMEMCPY();
	WMEMCPY(dst,src,len);
}



void wpause(hsec)		/* wpause -> PAUSE */
int *hsec;
{
	extern void PAUSE();
	PAUSE(hsec);
}



void wvaset(cnt)
int *cnt;
{
	extern void WVASET();
	WVASET(cnt);
}

void wfwait(status, timer)
char *status;
int  *timer;
{
	extern void WFWAIT();
	WFWAIT(status, timer);
}


void wsetstat(smask, cmask, src)
int *smask;
int *cmask;
int *src;
{
	extern void WSETSTAT();
	WSETSTAT(smask, cmask, src);
}

void mwconv(src, idst, fdst, len, retval)
char *src;	/* the source string				*/
char *idst;	/* the integer destination string.		*/
char *fdst;	/* The fractional destination string.		*/
char *len;	/* the length of the string			*/
int  *retval;	/* The field for the return value.		*/
{
	extern void MWCONV();
	MWCONV(src, idst, fdst, len, retval);
}

void wexith()			/* wexith -> WISPEXIT */
{
	extern void WISPEXIT();
	WISPEXIT();
}

void w2rowcol(oa3, curpos)	/* w2rowcol -> WGETCURPOS */
char *oa3;
char *curpos;
{
	extern void WGETCURPOS();
	WGETCURPOS(oa3, curpos);
}

void wfclose(name)		/* wfclose -> WFCLOSE */
char *name;
{
	extern void WFCLOSE();
	WFCLOSE(name);
}

char *wfname(mode, p_vol, p_lib, p_file, native_path) /* wfname -> WFNAME */
int  *mode;
char *p_vol;
char *p_lib;
char *p_file;
char *native_path;
{
	extern char* WFNAME();
	return WFNAME(mode, p_vol, p_lib, p_file, native_path);
}

void wfopen(mode, vol, lib, file, name, prname)		/* WISP 2.0B and earlier		*/
	int  *mode;					/* the mode of opening			*/
	char *vol;					/* the WANG volume name	(6 chars)	*/
	char *lib;					/* The WANG library name (8 chars)	*/
	char *file;					/* The file name	(8 chars)	*/
	char *name;					/* The resultant name			*/
	const char *prname;				/* The PRNAME .				*/
{
	extern void WFOPEN();
	WFOPEN(mode, vol, lib, file, name, prname);
}
 
void wfopen2(mode, vol, lib, file, name, appl, prname) 
	int  *mode;				/* the mode of opening			*/
	char *vol;				/* the WANG volume name	(6 chars)	*/
	char *lib;				/* The WANG library name (8 chars)	*/
	char *file;				/* The file name	(8 chars)	*/
	char *name;				/* The resultant name			*/
	const char *appl;			/* The COBOL program id (8 chars)	*/
	const char *prname;			/* The PRNAME 				*/
{
	extern void WFOPEN2();
	WFOPEN2(mode, vol, lib, file, name, appl, prname);
}

void wfopen3(mode, vol, lib, file, name, appl, prname, openmode) 
	int  *mode;				/* the mode of opening			*/
	char *vol;				/* the WANG volume name	(6 chars)	*/
	char *lib;				/* The WANG library name (8 chars)	*/
	char *file;				/* The file name	(8 chars)	*/
	char *name;				/* The resultant name			*/
	const char *appl;			/* The COBOL program id (8 chars)	*/
	const char *prname;			/* The PRNAME 				*/
	const int  *openmode;			/* The open mode			*/
{
	extern void WFOPEN3();
	WFOPEN3(mode, vol, lib, file, name, appl, prname, openmode);
}

#include <stdarg.h> 

void wrename(const char* type, char* file, char* lib, char* vol, ...)
{
	extern void WL_rename_va_list(const char* rtype, char* file, char* lib, char* vol, va_list the_args);

	va_list the_args;
	va_start(the_args, vol);
	WL_rename_va_list(type, file, lib, vol, the_args); /* wrename() -> RENAME() */
	va_end(the_args);
}

void wscreen(screen, function, blockarg, lines, pfkeys, on_pfkeys, pfkey_ret, file_stat)
	unsigned char *screen;
	unsigned char *function;
	unsigned char *blockarg;
	unsigned char *lines;
	unsigned char *pfkeys;
	unsigned char *on_pfkeys;
	unsigned char *pfkey_ret;
	unsigned char *file_stat;
{
	extern void WSCREEN();
	WSCREEN(screen, function, blockarg, lines, pfkeys, on_pfkeys, pfkey_ret, file_stat);
}

void bit_off(mask, src)			/* bit_off -> BIT_OFF */
	const unsigned char *mask; 
	unsigned char *src;
{
	extern void BIT_OFF();
	BIT_OFF(mask, src);
}

void bit_on(mask, src)			/* bit_on -> BIT_ON */
	const unsigned char *mask; 
	unsigned char *src;
{
	extern void BIT_ON();
	BIT_ON(mask, src);
}

void bit_test(mask, src, value)		/* bit_test -> BIT_TEST */
	const unsigned char *mask; 
	const unsigned char *src;
	char *value;
{
	extern void BIT_TEST();
	BIT_TEST(mask, src, value);
}

void lbit_off(mask, src)		/* lbit_off -> LBIT_OFF */
	const unsigned int *mask; 
	unsigned int *src;
{
	extern void LBIT_OFF();
	LBIT_OFF(mask, src);
}

void lbit_on(mask, src)			/* lbit_on -> LBIT_ON */
	const unsigned int *mask; 
	unsigned int *src;
{
	extern void LBIT_ON();
	LBIT_ON(mask, src);
}

void xx2byte(src, dst)			/* xx2byte -> W99TOX */
const char *src;
char *dst;
{
	extern void W99TOX();
	W99TOX(src, dst);
}

void getwfilext(ext)			/* getwfilext -> WGETFILEXT */
char ext[39];
{
	extern void WGETFILEXT();
	WGETFILEXT(ext);
}

void setwfilext(ext)			/* setwfilext -> WSETFILEXT */
const char* ext;
{
	extern void WSETFILEXT();
	WSETFILEXT(ext);
}

void setwispfilext(ext)			/* setwispfilext -> WSETFILEXT */
const char* ext;
{
	extern void WSETFILEXT();
	WSETFILEXT(ext);
}

void setretcode(rc)			/* setretcode -> SETRETCODE */
const char rc[3];
{
	extern void SETRETCODE();
	SETRETCODE(rc);
}

void wfilechk( decl_stat, file_stat, x_stat1, x_stat2, sflag, vol, lib, fil, fname, f_id, appl)
	char  decl_stat[2];
	const char   file_stat[2];
	const char*  x_stat1;
	const char*  x_stat2;
	int* sflag;
	const char   vol[6];
	const char   lib[8];
	const char   fil[8];
	const char   fname[80];
	const char   f_id[40];
	const char   appl[8];
{
	extern void WFILECHK();
	WFILECHK( decl_stat, file_stat, x_stat1, x_stat2, sflag, vol, lib, fil, fname, f_id, appl);
}

void wfilechk2( decl_stat, file_stat, x_stat1, x_stat2, sflag, vol, lib, fil, fname, f_id, appl)
	char		decl_stat[2];
	const char	file_stat[2];
	const char	x_stat1[10];
	const char	x_stat2[10];
	int*		sflag;
	const char	vol[6];
	const char	lib[8];
	const char	fil[8];
	const char	fname[80];
	const char	f_id[40];
	const char	appl[8];
{
	extern void WFILECHK2();
	WFILECHK2( decl_stat, file_stat, x_stat1, x_stat2, sflag, vol, lib, fil, fname, f_id, appl);
}

void initwisp2(	wisp_tran_version, wisp_lib_version, cobol_type, wisp_application_name, wisprunname, swap_on, err_flag)
	char	wisp_tran_version[20];						/* The TRANSLATOR version		*/
	char	wisp_lib_version[1];						/* The LIBRARY version			*/
	char	cobol_type[3];							/* The type of COBOL			*/
	char	wisp_application_name[8];					/* The name of the program		*/
	char	wisprunname[8];							/* The first appl name this run unit	*/
	int	*swap_on;							/* Swap_on == 0 forces swaping off	*/
	int	*err_flag;							/* Err_flag != 0 Changes error logging	*/
{
	extern void INITWISP2();
	INITWISP2( wisp_tran_version, wisp_lib_version, cobol_type, wisp_application_name, wisprunname, swap_on, err_flag);
}

void setprogid(wisp_application_name)
const char *wisp_application_name;
{
	extern void SETPROGID();
	SETPROGID(wisp_application_name);
}

/*
**	End of wisp4400.c
*/
