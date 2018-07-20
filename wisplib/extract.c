/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
**	File:		extract.c
**
**	Purpose:	To contain the emulation of the EXTRACT VSSUB
**
**	Routines:	EXTRACT()		The entry point to EXTRACT.
**			do_extract()		Do the extract of a single value.
**			dev_list()		Generates a list of devices
**			dev_info()		Gets info for a device number
**			gettype()		Get the type for a device
**			configfile()		Get the file name of the config file
**			osd_mode()		Returns B=background F=foreground
**			osd_jname()		Returns jobname (STUB)
**			osd_term()		Returns Wang style workstation number
**			findtty()		Read the wsysconfig file for matching tty
**
**
*/

/* 				get information from the operating system							*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>

#ifdef unix
#include <unistd.h>
#include <sys/times.h>
#include <dirent.h>
#include <termio.h>
#include <signal.h>
#endif	/* unix */

#include <sys/stat.h>

#ifdef WIN32
#include <process.h>
#endif

#include "wsysconf.h"
#include "idsistd.h"
#include "wdefines.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"
#include "wanguid.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "wrunconf.h"
#include "wispcfg.h"
#include "machid.h"
#include "setprgid.h"
#include "vssubs.h"

#ifdef WIN32
#include "isonames.h"
#endif

static char prid[8];

static char *configfile();
static void do_extract(const char* routine, const char* keywrd, char* recvr, int4 rlen);
static int gettype();
static int findtty();
static void dev_list(char* where, int4 rlen);					/* generate a list of devs			*/
static int dev_info();

static int osd_term(int4* tnum, int *tflags);
static void osd_mode(char *mode);
static void osd_jname(char* name);

#define NOT_SUPP 	{if ( full_err_mess ) WL_werrlog(WERRCODE(17002),prid,keywrd,0,0,0,0,0,0);}
#define NOT_YET		{if ( full_err_mess ) WL_werrlog(WERRCODE(17004),prid,keywrd,0,0,0,0,0,0);}

/*
**	Call routine using Keyword, Reciever [,length] ...
**
**	Old translation of EXTRACT call put the PROGID as the 3rd parameter and
**	split multiple keywords into multiple calls.  If 3 args and 3rd arg is 
**	character data then assume old style translation.  This is safe because
**	with new translation a 3 arg call would have the length as the 3rd arg
**	and this would not be character data.
*/

void EXTRACT(const char* first, ...)
{
	va_list	the_args;
	int	arg_count, ac;								/* Number of arguments.			*/
	const	char *next_arg;
	char	*recvr;
	char	ckw[3];
	int	gotk, poldw;								/* Flag to indicate key has been rec'd. */
	int4	rlen;

	memcpy(prid, WL_getprogid(),8);							/* Copy prog id from global to local.	*/
	va_start(the_args, first);							/* Set pointer to top of stack.		*/
	arg_count = WL_va_count();							/* Determine the number of arguments.	*/
	ac = arg_count;									/* Set a var to manipulate.		*/

	if (ac == 3) poldw = 1;								/* Test if possibly generated call from */
        else poldw = 0;									/*  previous version of translation.	*/

	next_arg = first;
	ac--;
	gotk = 1;
	
	while (ac > 0)									/* Required: keyword, receiver field,	*/
	{										/*  Optional: receiver length.		*/
		if (!gotk)								/* Do we have the keyword param?	*/
		{									/* No, so...				*/
			next_arg = va_arg(the_args, char*);				/* Get the extract keyword requested.	*/
			ac--;
			gotk = 0;
		}

		ckw[0] = next_arg[0];
		ckw[1] = next_arg[1];
		ckw[2] = '\0';
		
		if (ac > 0)								/* There are more params.		*/
		{
			recvr = va_arg(the_args, char*);				/* Get the returned receiver field.	*/
			ac--;								/* Decrement the argument count.	*/
		}
		else									/* Error: need receiver field.		*/
		{
			werrlog(WERRCODE(17008),prid,next_arg,0,0,0,0,0,0);
			va_end(the_args);
			return;								/* Return back to caller.		*/
		}

		rlen = 0L;								/* Set to use the default length.	*/
		if (ac > 0)
		{
			next_arg = va_arg(the_args, char*);				/* Get the next parameter.		*/
			ac--;								/* Decrement the argument count.	*/
			if ( isprint((int)next_arg[0]) && isprint((int)next_arg[1]) )		/* if 1st 2 chars are printable, 	*/
			   								/*  is next  keyword or the prog id.	*/
			{
				if (poldw)						/* Is the old wisp call so is the ID.	*/
				{
					memcpy(prid,next_arg,8);			/* Copy prog id from parameter to local.*/
				}
				else gotk = 1;						/* Set so don't screw up stack postn.	*/
			}								/* and will use temp value as keyword.	*/
			else
			{
				const int4 *plong;
				gotk = 0;
				plong = (const int4 *)next_arg;					/* Set to point to a int4eger.	*/
				rlen = WL_get_swap(plong);
			}
		}

		do_extract("EXTRACT", ckw,recvr,rlen);
	}

	va_end(the_args);
}

void EXTRACT2(const char* keywrd, void* recvr)
{
	do_extract("EXTRACT2", keywrd, (char*)recvr, 0);
}

static void do_extract(const char* routine, const char* keywrd, char* recvr, int4 rlen)
{
	char	tstr[80];								/* a temp string			*/
	int4	tlong;
	int	full_err_mess=0;							/* Report errors ?			*/

	full_err_mess = 1;

	switch (keywrd[0])								/* What do they want to know?		*/
	{
		case 'A':
			NOT_YET
			break;
		case 'B':
			NOT_SUPP
			break;
		case 'C':								/* 'C' type requests			*/
		{
			switch (keywrd[1])
			{
				case 'F':						/* CF request, current program file	*/
				{
					memcpy(recvr,wisp_get_runname(),WISP_RUNNAME_SIZE);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'L':						/* default proglib			*/
				{
					WL_get_defs(DEFAULTS_RL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'S':
					NOT_YET
					break;

				case 'V':						/* default progvol			*/
				{
					WL_get_defs(DEFAULTS_RV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				case '#':
				{
					memset(recvr,' ',4);
					if (wispcpu())
					{
						memcpy(recvr,wispcpu(),4);
					}
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%4.4s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'C'			*/
		}
		case 'D':
		{
			switch (keywrd[1])
			{
				case '$':						/* D$ request				*/
				case '+':						/* D+ request				*/
				case '-':						/* D- request				*/
				case 'F':						/* DF request				*/
				case 'P':						/* DP request				*/
				case 'S':						/* DS request				*/
				case 'V':						/* DV request				*/
					NOT_YET
					break;

				case 'L':
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s", keywrd);
					dev_list(recvr,rlen);
					break;
				case ' ':
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s", keywrd);
					dev_info(recvr);
					break;

				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'D'			*/
		}

		case 'E':
		{
			switch (keywrd[1])
			{
				case ':':						/* E: request				*/
				{
					int4 numsecs, numsecs100;
					time_t nowtime;

					time(&nowtime);
					numsecs = nowtime - wisp_get_WSTARTTIME();
					numsecs100 = numsecs * 100;

					WL_put_swap((int4*)recvr, numsecs100);

					tlong=numsecs100;
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				}
				case '?':						/* E? request				*/
					NOT_YET
					break;
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'E'			*/
		}

		case 'F':
		{
			switch (keywrd[1])
			{
				case 'N':						/* FN request				*/
				{
					WL_get_defs(DEFAULTS_FN, &tlong);
					WL_put_swap((int4*)recvr,tlong);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'E'			*/
		}
		case 'G':
		{
			switch (keywrd[1])
			{
				case '#':
					tlong = WL_wgetpgrp();
					WL_put_swap((int4*)recvr, tlong);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'G'			*/
		}
		case 'I':								/* 'I' type requests			*/
		{
			switch (keywrd[1])
			{
				case 'D':						/* ID request				*/
				{
					if ( OPTION_IDNUMERIC )				/* Numeric ID wanted (set in OPTIONS).	*/
					{
						memcpy(recvr, WL_numuid3(), 3 );	/* Get 3 character numeric ID.		*/
					}
					else						/* Alpha ID wanted (default).		*/
					{
						memcpy(recvr, WL_wanguid3(), 3 );	/* Get 3 character alpha ID.		*/
					}
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%3.3s]", keywrd, recvr);
					break;						/* end of case 'D'			*/
				}
				case '8':						/* I8 8 Char Userid request		*/
				{
					const char *uidptr = WL_longuid();
					int	ii;

					memset(recvr, ' ', 8 );
					ii=strlen(uidptr);
					memcpy(recvr, uidptr, ((ii < 8) ? ii : 8) );
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'X':						/* IX  Extended Userid (32 char)	*/
				{
					memset(recvr, ' ', 32 );
					memcpy(recvr, WL_longuid(), strlen(WL_longuid()) );
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%32.32s]", keywrd, recvr);
					break;
				}
				case 'L':						/* default INLIB			*/
				{
					WL_get_defs(DEFAULTS_IL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'V':						/* default INVOL			*/
				{
					WL_get_defs(DEFAULTS_IV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'I'			*/
		}
		case 'J':								/* case 'J'				*/
		{
			switch (keywrd[1])
			{
				case 'C':						/* Default job class.			*/
				{
					WL_get_defs(DEFAULTS_JC,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%1.1s]", keywrd, recvr);
					break;
				}
				case 'L':						/* Time limit				*/
				{
					WL_get_defs(DEFAULTS_JL,tstr);
					tlong  = (tstr[0] - '0') * 36000L;		/* Do tens of hours.			*/
					tlong += (tstr[1] - '0') * 3600L;		/* Do hours.				*/
					tlong += (tstr[2] - '0') * 600L;		/* Tens of minutes.			*/
					tlong += (tstr[3] - '0') * 60L;			/* Minutes.				*/
					tlong += (tstr[4] - '0') * 10L;			/* Tens of seconds.			*/
					tlong += (tstr[5] - '0');			/* Seconds.				*/
					WL_put_swap((int4*)recvr,tlong);

					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				}
				case 'S':						/* Default job submittal status.	*/
				{
					WL_get_defs(DEFAULTS_JS,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%1.1s]", keywrd, recvr);
					break;
				}
				case 'N':						/* Batch job name.			*/
				{
					osd_jname(recvr);				/* Ask the system.			*/
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;
		}									/* End of case 'J'			*/
		case 'L':
		{
			switch (keywrd[1])
			{
				case 'I':						/* LI request				*/
				{
					WL_get_defs(DEFAULTS_LI,&tlong);
					WL_put_swap((int4*)recvr, tlong);

					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				}
				case 'U':						/* LU LONG USERID request		*/
					strcpy(recvr, WL_longuid() );
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%s]", keywrd, recvr);
					break;
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'L'			*/
		}
		case 'N':
		{
			switch (keywrd[1])
			{
				case 'A':						/* NA request				*/
					WL_passwdname(tstr);

					memset(recvr, ' ', 24 );			/* Receiver expects 24 characters.	*/
					if (strlen(tstr) < 24)
					{
						memcpy(recvr,tstr,strlen(tstr));
					}
					else	memcpy(recvr,tstr,24);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%24.24s]", keywrd, recvr);
					break;
				case 'C':						/* NC request				*/
				case 'S':						/* NS request				*/
					NOT_YET
					break;
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'N'			*/
		}
		case 'O':
		{
			switch (keywrd[1])
			{
				case 'L':						/* default OUTLIB			*/
				{
					WL_get_defs(DEFAULTS_OL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'V':						/* default OUTVOL			*/
				{
					WL_get_defs(DEFAULTS_OV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;
		}
		case 'P':								/* 'P' type requests			*/
		{
			switch (keywrd[1])
			{
				case 'C':						/* default Print class			*/
				{
					WL_get_defs(DEFAULTS_PC,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%1.1s]", keywrd, recvr);
					break;
				}
				case 'L':						/* default RUNLIB			*/
				{
					WL_get_defs(DEFAULTS_PL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'M':						/* default print mode			*/
				{
					WL_get_defs(DEFAULTS_PM,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%1.1s]", keywrd, recvr);
					break;
				}
				case 'V':						/* default RUNVOL			*/
				{
					WL_get_defs(DEFAULTS_PV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				case '#':						/* 'P#' and 'PR' both return prt_num	*/
				case 'R':
					WL_get_defs(DEFAULTS_PR,&tlong);
					WL_put_swap((int4*)recvr, tlong);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				case ':':						/* P: request				*/
				{
					int4	numtics, numsecs100;
#ifdef unix
					struct tms tt;
					times(&tt);
					numtics = tt.tms_utime + 
						  tt.tms_stime + 
						  tt.tms_cutime + 
						  tt.tms_cstime;
					numsecs100 = (numtics * 100) / 60;
#endif	/* unix */
#ifdef WIN32
					numtics = clock();
					numsecs100 = (numtics * 100)/CLOCKS_PER_SEC;
#endif	/* WIN32 */

					WL_put_swap((int4*)recvr, numsecs100);

					tlong = numsecs100;
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				}
				case 'F':
				case 'T':
					NOT_YET
					break;
			
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'P'			*/
		}
		case 'R':								/* 'R' type requests			*/
		{
			switch (keywrd[1])
			{
				case 'L':						/* default RUNLIB			*/
				{
					WL_get_defs(DEFAULTS_RL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'V':						/* default RUNVOL			*/
				{
					WL_get_defs(DEFAULTS_RV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'R'			*/
		}
                case 'S':
		{
			switch (keywrd[1])
			{
				case 'L':						/* Spool Library request.		*/
				{
					WL_get_defs(DEFAULTS_SL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				case 'V':						/* Spool volume request.		*/
				{
					WL_get_defs(DEFAULTS_SV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				case '$':
				{
					WL_loadpad(recvr,WL_computername(NULL),16);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%16.16s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'S'			*/
		}
		case 'T':
		{
			switch (keywrd[1])
			{
				case 'T':						/* TT request				*/
				{
					osd_mode(recvr);				/* ask for the mode			*/
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%1.1s]", keywrd, recvr);
					break;
				}
				case '#':
					tlong = (int4) getpid();
					WL_put_swap((int4*)recvr,tlong);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
#ifdef unix
				case 'E':
					tlong = WL_get_swap((int4*)recvr);
					if (kill(tlong,0) == 0)
					{
						/* Process is running */
						recvr[4] = 'R';
					}
					else
					{
						/* Process NOT running */
						recvr[4] = 'N';
					}
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld-%c]", 
					       keywrd, (long)tlong, recvr[4]);
					break;					
#endif /* unix */
				case '$':
				case 'C':
				case 'V':
					NOT_YET
					break;
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'T'			*/
		}
		case 'W':
		{
			switch (keywrd[1])
			{
				case 'L':						/* WORK LIBRARY request			*/
				{
					WL_get_defs(DEFAULTS_WL,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;						/* end of case 'L'			*/
				}
				case 'V':						/* WORK volume request			*/
				{
					WL_get_defs(DEFAULTS_WV,recvr);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;						/* end of case 'V'			*/
				}
				case '#':						/* terminal id				*/
				{
					tlong = WL_workstation();
					WL_put_swap((int4*)recvr,tlong);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%ld]", keywrd, (long)tlong);
					break;
				}
				case '$':
				{
					const char* wangnet = wispnetid();

					memset(recvr,' ',8);

					if (wangnet)
					{
						int	len;
						len = strlen(wangnet);
						if (len > 8) len = 8;
						memcpy(recvr,wangnet,len);
					}
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%8.8s]", keywrd, recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'W'			*/
		}
		case 'X':
		{
			switch(keywrd[1])
			{
				case 'V':						/* Ipl volume.				*/
				{
					memcpy(recvr,"IPLVOL",(int)6);
					WL_wtrace(routine, "KEYWORD", "Keyword=%2.2s Value=[%6.6s]", keywrd, recvr);
					break;
				}
				default:
				{
					NOT_SUPP
					break;
				}
			}
			break;
		}

		default:
		{
			NOT_SUPP
			break;
		}
	}
}

/*
**	ROUTINE:	workstation()
**
**	FUNCTION:	Get the workstation number
**
**	DESCRIPTION:	The workstation number 0-254
**			255 = unable to identify
**			-1  = background
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The workstation number
**
**	WARNINGS:	none
**
*/
int4 WL_workstation(void)
{
	static int4 the_num = -2;

	if ( -2 == the_num) /* If FIRST */
	{
		if (wbackground())
		{
			the_num = -1;
		}
		else
		{
			int flags;

			osd_term(&the_num,&flags);
		}
	}
	return the_num;
}

static void dev_list(char* where, int4 rlen)					/* generate a list of devs			*/
{ 
	unsigned char list[256]; 						/* list can be 255 int4 (bytes)			*/
	char 	devtype;
	int 	listind;
	char 	cnfname[100];
	FILE 	*cnf;
	char 	inbuf[256];

	devtype = *where;							/* type to find					*/
	if (rlen == 0)
	{
		rlen  = (int4) *(where+1);					/* number to return				*/ 
	}

	strcpy(cnfname,configfile());
	if ((cnf=fopen(cnfname,"r"))==NULL)
	{
		werrlog(WERRCODE(17010),cnfname,errno,0,0,0,0,0,0);
		*where = (char)0;
		*(where+1) = (char)0;
		return;
	}

	listind=0;
	while (fgets(inbuf,sizeof(inbuf),cnf) && listind < 256-2)		/* find all of the matching files		*/
	{
		char	testtype;
		int	rc;
		int	dev_num;
		char	dev_class[10];
		char	dev_type[20];
		char	dev_name[80];

		if (!isdigit((int)inbuf[0])) continue;				/* skip dev type lines */
		rc = sscanf(inbuf,"%d %2s %20s %80s",&dev_num, dev_class, dev_type, dev_name);
		if (rc < 4) dev_name[0] = (char)0;
		if (rc < 3) dev_type[0] = (char)0;
		if (rc < 2) continue;						/* Badly formed line.				*/

		switch (toupper(dev_class[0]))					/* Check the class				*/
		{
		case 'W': /* ws - workstation */
			testtype = (char)1;
			break;
		case 'M': /* mt - magtape */
			testtype = (char)2;
			break;
		case 'D': /* dv - disk volume */
			testtype = (char)3;
			break;
		case 'L': /* lp - lineprinter */
			testtype = (char)4;
			break;
		default:
			testtype = (char)0;
			break;
		}

		if (!devtype || (testtype == devtype))
		{
			list[listind++] = (unsigned char) dev_num;
		}
	}
	*where = (char) listind;						/* return count					*/
	if (rlen > listind) rlen = listind;
	*(where+1) = (char) rlen;						/* and number returned				*/
	memcpy(where+2,list,(int)rlen);						/* and actual list				*/
	fclose(cnf);
}

static int dev_info(where)							/* get info for a device number			*/
char *where;									/* return area					*/
{
	int devnum; 								/* device number var				*/
	char cnfname[100];
	FILE *cnf;
	char inbuf[256];

	devnum = (int) *where;							/* init the dev number				*/
	where[0] = (char)0;
	where[1] = (char)0;

	strcpy(cnfname,configfile());
	if ((cnf=fopen(cnfname,"r"))==NULL)
	{
		werrlog(WERRCODE(17010),cnfname,errno,0,0,0,0,0,0);
		return(1);
	}

	while (fgets(inbuf,sizeof(inbuf),cnf))					/* will only match one file			*/
	{
		int	rc;
		int	dev_num;
		char	dev_class[10];
		char	dev_type[20];
		char	dev_name[80];

		if (!isdigit((int)inbuf[0])) continue;				/* Not a device line.				*/

		rc = sscanf(inbuf,"%d %2s %20s %80s",&dev_num, dev_class, dev_type, dev_name);
		if (rc < 4) dev_name[0] = (char)0;
		if (rc < 3) dev_type[0] = (char)0;
		if (rc < 2) continue;						/* Badly formed line.				*/

		if (dev_num != devnum) continue;				/* Device numbers don't match, try next device	*/

		/*
		**	We have matched device numbers.
		*/

		switch (toupper(dev_class[0]))					/* Check the class				*/
		{
		case 'W': /* ws - workstation */
			*where = (char)1;
			break;
		case 'M': /* mt - magtape */
			*where = (char)2;
			break;
		case 'D': /* dv - disk volume */
			*where = (char)3;
			break;
		case 'L': /* lp - lineprinter */
			*where = (char)4;
			break;
		default:
			*where = (char)0;
			break;
		}
		fclose(cnf);							/* Close the file before gettype()		*/
		*(where+1) = (char) gettype(dev_type);				/* and type byte				*/
		return(0);
	}
	fclose(cnf);
	return(1);
}

static int gettype(name)							/* return type number for a kind of device	*/
char *name;									/* device type name				*/
{
	FILE *types;
	char *p;
	char inbuf[256];

	types = fopen(configfile(),"r");					/* typefile setup by wispdev			*/
	if (!types)								/* oops, file isn't there			*/
	{
		/*
		**	This should never happen as we just managed to open the file.
		*/
		werrlog(WERRCODE(17006),0,0,0,0,0,0,0,0);			/* Error accessing device types.		*/
		return(255);
	}
	while (fgets(inbuf,sizeof(inbuf),types))				/* read the records in. record format is:	*/
	{									/* <devtype>=<integer>				*/
		if (inbuf[0]=='#' || !strchr(inbuf,'=')) continue;		/* skip #comment lines and non xxx=yyy lines    */
		
		p = strchr(inbuf,'='); 						/* find the equals				*/
		*p++ = (char)0;							/* replace it with a null			*/
		if (!strcmp(inbuf,name))					/* now examine first part			*/
		{
			fclose(types);						/* found match, cleanup and 			*/
			return atoi(p);						/* return second part				*/
		}
	}
	fclose(types);
	return(255);
}


static char *configfile()
{
	static int first=1;
	static char cfgpath[128];
	
	if (first)
	{
		buildfilepath(cfgpath,wispconfigdir(),CFGFNAME);
		first = 0;
	}
	return( cfgpath );
}


static void osd_mode(char *mode)
{
	if ( wbackground() )
	{
		*mode = 'B';								/* No terminal attached. 		*/
	}
	else
	{
		*mode = 'F';								/* Interactive task. 			*/
	}
}


static void osd_jname(char* name)
{
	const char *ptr;

	memset(name, ' ', 8);

	if (!wbackground())
	{
		return;
	}
		
#ifdef WIN32
	if ((ptr = getenv(AQM_JOB_NAME)))
	{
		WL_loadpad(name,ptr,8);
		return;
	}
#endif

	if ((ptr = getenv("WISPJOBNAME")))
	{
		WL_loadpad(name,ptr,8);
	}
}


/*
	osd_term	This routine returns the Wang style workstation device number or -1 if in background.
			On any error 255 is returned as the device number.
*/
static int osd_term(int4* tnum, int *tflags)
{
static	int	first=1;
static	int4	device_num;

	if (first)
	{
		first = 0;

		if (wbackground())
		{
			device_num = -1;
		}
		else
		{
			findtty(&device_num);
			if (device_num == -1 )
			{
				device_num = 255;					/* Correct a bad return from findtty 	*/
				WL_wtrace("OSD_TERM","TTY", "Invalid device number changed to 255");
			}
		}
	}

	*tnum = device_num;
	*tflags = 0;									/* Hardwire flag values. (not used)	*/
	return(0);
}


/*
	findtty		This routine returns the Wang style workstation device number as found in wsysconfig.
			If unable to find the tty for any reason it will return -1.
			A valid value is 0 - 254.
*/
static int findtty(int4 *where)
{
static	int 	first=1;
static	int4	device_num;


	if (first)
	{
		const char *tty = ttyname(0);
		FILE *cnf;

		first = 0;
	
		device_num = -1;						/* Assume not found 				*/

		if ( tty && (cnf=fopen(configfile(),"r")) )			/* If tty and able to open wsysconfig		*/
		{
			char inbuf[256];

			while (fgets(inbuf,sizeof(inbuf),cnf))			/* find all of the matching files		*/
			{
				int	cnt, num;
				char	termtype[80], device[80];

				cnt = sscanf(inbuf,"%d ws %s %s",&num,termtype,device);
				if ( cnt == 3 )
				{
					if ( 0 == strcmp(tty,device) )
					{
						device_num = num;
						if (device_num < 0 || device_num > 254)
						{
							werrlog(WERRCODE(65112),device_num,tty,0,0,0,0,0,0);
							device_num = -1;
						}
						break;
					}
				}
			}
			fclose(cnf);
		}
	}

	*where = device_num;
	return 0;
}


/*
**	History:
**	$Log: extract.c,v $
**	Revision 1.49  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.48  2003/03/28 20:15:57  gsl
**	Add EXTRACT2
**	
**	Revision 1.47  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.46  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.45  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.44  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.43  2003/01/29 20:45:36  gsl
**	comments
**	
**	Revision 1.42  2003/01/29 19:42:50  gsl
**	Fix -Wall warnings
**	
**	Revision 1.41  2002/12/10 20:54:15  gsl
**	use WERRCODE()
**	
**	Revision 1.40  2002/12/10 17:09:20  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.39  2002/07/16 16:24:57  gsl
**	Globals
**	
**	Revision 1.38  2002/07/12 17:00:55  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.37  2002/07/11 20:29:07  gsl
**	Fix WL_ globals
**	
**	Revision 1.36  2002/07/11 14:52:50  gsl
**	Fix WL_ globals
**	
**	Revision 1.35  2002/07/10 21:05:15  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.34  2002/07/10 04:27:39  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.33  2002/07/09 04:14:02  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.32  2002/07/01 04:02:37  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.31  2001/11/27 21:18:30  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.30  1998-11-04 10:12:30-05  gsl
**	Changed to use strarg.h vararg macros plus cleanup and doc.
**
**	Revision 1.29  1998-03-31 13:49:18-05  gsl
**	Move NISEEAST patch to wanguid.c
**
**	Revision 1.28  1998-03-31 11:12:13-05  gsl
**	Add the custom NISEEAST stuff in ifdef's
**
**	Revision 1.27  1997-10-21 10:06:12-04  gsl
**	Change WISPPROGID to getprogid()
**	Change GETBIN/PUTBIN to WL_get_swap() WL_put_swap()
**
**	Revision 1.26  1997-08-23 16:12:17-04  gsl
**	Add support for EXTRACT of "S$" (SYSTEM NAME)
**
**	Revision 1.25  1997-08-23 15:34:44-04  gsl
**	Add support for EXTRACT of "JN" - JOBNAME for unix and WIN32
**
**	Revision 1.24  1997-04-16 09:30:35-04  gsl
**	Change WL_wtrace() to display the values
**
**	Revision 1.23  1997-04-15 22:44:02-04  gsl
**	Change to use WL_wtrace()
**
**	Revision 1.22  1997-04-15 16:14:50-04  gsl
**	Change to use WL_wtrace()
**
**	Revision 1.21  1997-04-03 20:00:34-05  gsl
**	Change the trace so that it shows the keyword
**
**	Revision 1.20  1996-10-25 17:05:16-04  gsl
**	Fix call to WL_longuid() which now returns a constant
**
**	Revision 1.19  1996-10-08 17:20:13-07  gsl
**	replace getenv() calls with wispcpu() and wispnetid() calls
**
**	Revision 1.18  1996-09-10 08:41:20-07  gsl
**	move include of idsistd.h to after the system includes
**
**	Revision 1.17  1996-08-29 17:07:34-07  gsl
**	Split the MSDOS and WIN32 code for process time into separate peices
**
**	Revision 1.16  1996-08-27 08:43:32-07  gsl
**	Add missing header
**
**	Revision 1.15  1996-08-26 17:05:15-07  gsl
**	Fix osd_term() and findtty() for NT
**
**	Revision 1.14  1996-08-22 17:25:27-07  gsl
**	Change "G#" to use wgetpgrp() and remove the ifdef unix as it is
**	now available for everyone.
**
**	Revision 1.13  1996-08-19 15:32:18-07  gsl
**	drcs update
**
**	05/28/92	Added "C#" function GSL
**	08/12/93	Added "NA" function for VMS  SMC
**
**
*/
