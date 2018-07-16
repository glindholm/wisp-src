			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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
**	History:
**			mm/dd/yy	Written by OLD
**			05/28/92	Added "C#" function GSL
**
*/

/*
**	extract.c
*/

/* 				get information from the operating system							*/

#include <ctype.h>
#include <varargs.h>
#include <time.h>

#if defined (unix) && !defined(NCR32)
#include <sys/times.h>
#include <dirent.h>
#include <termio.h>
#endif	/* unix */

#ifdef MSDOS
#include <stdlib.h>
#endif

#ifndef VMS	/* unix or MSDOS */
#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "wsysconf.h"

#endif	/* unix or MSDOS */

#include "wdefines.h"
#include "wperson.h"
#include "movebin.h"
#include "werrlog.h"
#include "wglobals.h"

static char prid[8];

#ifndef VMS	/* unix and MSDOS */
char 	*cuserid();
char 	*configfile();
#endif	/* unix and MSDOS */


char *wanguid3();
char *numuid3();
char *longuid();
char *getenv();

#define NOT_SUPP 	{if ( full_err_mess ) werrlog(ERRORCODE(2),prid,keywrd,0,0,0,0,0,0);}
#define NOT_YET		{if ( full_err_mess ) werrlog(ERRORCODE(4),prid,keywrd,0,0,0,0,0,0);}

EXTRACT(va_alist)									/* Variable number of arguments.	*/
va_dcl											/* Set up call to do extract.		*/
{
#define		ROUTINE		17000
	va_list	the_args;								/* Define a pointer to the list.	*/
	int	arg_count, ac;								/* Number of arguments.			*/
	char	*keywrd, *recvr;							/* Address of call and return params.	*/
	char	ckw[3], *temp;								/* Holding var to determine what it is.	*/
	int	i, gotk, poldw;								/* Flag to indicate key has been rec'd. */
	long	*plong;
	long	rlen;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	memcpy(prid,WISPPROGID,8);							/* Copy prog id from global to local.	*/
	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* Determine the number of arguments.	*/
	gotk = 0;
	ac = arg_count;									/* Set a var to manipulate.		*/
	va_start(the_args);								/* Set pointer to top of stack.		*/
	if (ac == 3) poldw = 1;								/* Test if possibly generated call from */
        else poldw = 0;									/*  previous version of translation.	*/
	while (ac > 0)									/* Required: keyword, receiver field,	*/
	{										/*  Optional: receiver length.		*/
		if (!gotk)								/* Do we have the keyword param?	*/
		{									/* No, so...				*/
			keywrd = va_arg(the_args, char*);				/* Get the extract keyword requested.	*/
			ac--;								/* Decrement the argument count.	*/
			for (i = 0; i < 2; i++)  ckw[i] = *keywrd++;			
		}
		else for (i = 0; i < 2; i++)  ckw[i] = *temp++;				/* Get the keyword from the temp var.	*/
		if (ac > 0)								/* There are more params.		*/
		{
			recvr = va_arg(the_args, char*);				/* Get the returned receiver field.	*/
			ac--;								/* Decrement the argument count.	*/
		}
		else									/* Error: need receiver field.		*/
		{
			werrlog(ERRORCODE(8),prid,keywrd,0,0,0,0,0,0);
			return(0);							/* Return back to caller.		*/
		}
		rlen = 0L;								/* Set to use the default length.	*/
		if (ac > 0)
		{
			temp = va_arg(the_args, char*);					/* Get the next parameter.		*/
			ac--;								/* Decrement the argument count.	*/
			if ( isprint(temp[0]) && isprint(temp[1]) ) 			/* if 1st 2 chars are printable, 	*/
			   								/*  is next  keyword or the prog id.	*/
			{
				if (poldw)						/* Is the old wisp call so is the ID.	*/
				{
					memcpy(prid,temp,8);				/* Copy prog id from parameter to local.*/
				}
				else gotk = 1;						/* Set so don't screw up stack postn.	*/
			}								/* and will use temp value as keyword.	*/
			else
			{
				gotk = 0;
				plong = (long *)temp;					/* Set to point to a long integer.	*/
				GETBIN(&rlen,plong,4);					/* Put aligned value into local var.	*/
				wswap(&rlen);						/* swap the work order.			*/
			}
		}
		do_extract(ckw,recvr,rlen);						/* Call extract for each set of params.	*/
	}
}

static do_extract(keywrd,recvr,rlen)							/* Do the actual extract.		*/
char *keywrd, *recvr;
long rlen;
{
	char	tstr[80];								/* a temp string			*/
	char	*tptr;									/* a temp pointer			*/
	long	tlong;
	long	i;									/* Character count and index variables.	*/
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
#ifdef VMS
					memcpy(recvr,prid,8);				/* return the current program name	*/
#else	/* end VMS start unix and MSDOS */
					memcpy(recvr,WISPRUNNAME,8);
#endif	/* unix and MSDOS */
					break;						/* end of case 'D'			*/
				}
				case 'L':						/* default proglib			*/
				{
					get_defs(DEFAULTS_RL,recvr);
					break;
				}
				case 'S':
					NOT_YET
					break;

				case 'V':						/* default progvol			*/
				{
					get_defs(DEFAULTS_RV,recvr);
					break;
				}
#ifdef unix
				case '#':
					memset(recvr,' ',4);
					if (tptr = getenv("WISPCPU"))
					{
						memcpy(recvr,tptr,4);
					}
					break;
#endif
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
#ifdef VMS
				case 'L':						/* DL request				*/
				case ' ':						/* D  request				*/
#endif	/* VMS */
				case 'P':						/* DP request				*/
				case 'S':						/* DS request				*/
				case 'V':						/* DV request				*/
					NOT_YET
					break;
#ifndef VMS	/* unix and MSDOS */
				case 'L':
					dev_list(recvr);
					break;
				case ' ':
					dev_info(recvr);
					break;
#endif	/* unix and MSDOS */
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
					long numsecs, numsecs100;
					time_t nowtime;

					time(&nowtime);
					numsecs = nowtime - WSTARTTIME;
					numsecs100 = numsecs * 100;

					PUTBIN(recvr,&numsecs100,4);
					wswap((long *)recvr);
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
					get_defs(DEFAULTS_FN,recvr);
					wswap((long *)recvr);
					break;
				}
				default:
					NOT_SUPP
					break;
			}
			break;								/* end of case 'E'			*/
		}
		case 'I':								/* 'I' type requests			*/
		{
			switch (keywrd[1])
			{
				case 'D':						/* ID request				*/
				{
					if ( opt_idnumeric )				/* Numeric ID wanted (set in OPTIONS).	*/
					{
						memcpy(recvr, numuid3(), 3 );		/* Get 3 character numeric ID.		*/
					}
					else						/* Alpha ID wanted (default).		*/
					{
						memcpy(recvr, wanguid3(), 3 );		/* Get 3 character alpha ID.		*/
					}
					break;						/* end of case 'D'			*/
				}
				case '8':						/* I8 8 Char Userid request		*/
				{
					char	*uidptr;
					int	ii;

					memset(recvr, ' ', 8 );
					uidptr = longuid();
					ii=strlen(uidptr);
					memcpy(recvr, uidptr, ((ii < 8) ? ii : 8) );
					break;
				}
				case 'X':						/* IX  Extended Userid (32 char)	*/
				{
					memset(recvr, ' ', 32 );
					memcpy(recvr, longuid(), strlen(longuid()) );
					break;
				}
				case 'L':						/* default INLIB			*/
				{
					get_defs(DEFAULTS_IL,recvr);
					break;
				}
				case 'V':						/* default INVOL			*/
				{
					get_defs(DEFAULTS_IV,recvr);
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
					get_defs(DEFAULTS_JC,recvr);
					break;
				}
				case 'L':						/* Time limit				*/
				{
					get_defs(DEFAULTS_JL,tstr);
					tlong  = (tstr[0] - '0') * 36000L;		/* Do tens of hours.			*/
					tlong += (tstr[1] - '0') * 3600L;		/* Do hours.				*/
					tlong += (tstr[2] - '0') * 600L;		/* Tens of minutes.			*/
					tlong += (tstr[3] - '0') * 60L;			/* Minutes.				*/
					tlong += (tstr[4] - '0') * 10L;			/* Tens of seconds.			*/
					tlong += (tstr[5] - '0');			/* Seconds.				*/
					wswap(&tlong);
					memcpy(recvr,&tlong,(int)4);			/* return the value.			*/
					break;
				}
				case 'S':						/* Default job submittal status.	*/
				{
					get_defs(DEFAULTS_JS,recvr);
					break;
				}
				case 'N':						/* Batch job name.			*/
				{
					osd_jname(recvr);				/* Ask the system.			*/
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
					get_defs(DEFAULTS_LI,recvr);
					wswap((long *)recvr);
					break;
				}
				case 'U':						/* LU LONG USERID request		*/
					strcpy(recvr, longuid() );
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
#ifndef VMS	/* unix and MSDOS */
					passwdname(tstr);
					memcpy(recvr,tstr,strlen(tstr));
					break;
#endif	/* unix and MSDOS */
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
					get_defs(DEFAULTS_OL,recvr);
					break;
				}
				case 'V':						/* default OUTVOL			*/
				{
					get_defs(DEFAULTS_OV,recvr);
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
					get_defs(DEFAULTS_PC,recvr);
					break;
				}
				case 'L':						/* default RUNLIB			*/
				{
					get_defs(DEFAULTS_PL,recvr);
					break;
				}
				case 'M':						/* default print mode			*/
				{
					get_defs(DEFAULTS_PM,recvr);
					break;
				}
				case 'V':						/* default RUNVOL			*/
				{
					get_defs(DEFAULTS_PV,recvr);
					break;
				}
				case '#':						/* 'P#' and 'PR' both return prt_num	*/
				case 'R':
					get_defs(DEFAULTS_PR,recvr);
					wswap((long *)recvr);
					break;
				case ':':						/* P: request				*/
				{
#ifdef VMS
					tbuffer_t tt;
#endif	/* VMS */
#ifdef unix
					struct tms tt;
#endif	/* unix */
#ifdef MSDOS
					struct						/* the times struct created own because */
					{						/* names differ between systems		*/
						long pu_time;
						long ps_time;
						long cu_time;
						long cs_time;
					} tt;
#endif	/* MSDOS */
					long	numtics, numsecs100;
#ifdef MSDOS
					numtics = clock();
					numsecs100 = (numtics * 100)/CLK_TCK;
#else	/* end MSDOS start VMS and unix */
					times(&tt);
#ifdef unix
					numtics = tt.tms_utime + 
						  tt.tms_stime + 
						  tt.tms_cutime + 
						  tt.tms_cstime;
					numsecs100 = (numtics * 100) / 60;
#endif	/* unix */

#ifdef VMS
					numtics = tt.proc_user_time + 
						  tt.proc_system_time + 
						  tt.child_user_time + 
						  tt.child_system_time;
					numsecs100 = numtics;
#endif	/* VMS */
#endif	/* VMS and unix */

					PUTBIN(recvr,&numsecs100,4);
					wswap((long *)recvr);
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
					get_defs(DEFAULTS_RL,recvr);
					break;
				}
				case 'V':						/* default RUNVOL			*/
				{
					get_defs(DEFAULTS_RV,recvr);
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
					get_defs(DEFAULTS_SL,recvr);
					break;
				}
				case 'V':						/* Spool volume request.		*/
				{
					get_defs(DEFAULTS_SV,recvr);
					break;
				}
				case '$':
				case 'A':
					NOT_YET
					break;

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
					break;						/* end of case 'T'			*/
				}
				case '#':
#ifndef VMS	/* unix and MSDOS */
					tlong = PGRPID;
					PUTBIN(recvr,&tlong,4);
					wswap( (long *)recvr );
					break;
#endif	/* unix and MSDOS */
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
					get_defs(DEFAULTS_WL,recvr);
					break;						/* end of case 'L'			*/
				}
				case 'V':						/* WORK volume request			*/
				{
					get_defs(DEFAULTS_WV,recvr);
					break;						/* end of case 'V'			*/
				}
				case '#':						/* terminal id				*/
				{
					osd_term(&tlong,(int *)tstr);			/* ask for it by name			*/
					wswap(&tlong);					/* swap order of the words		*/
					PUTBIN(recvr,&tlong,(int)4);			/* copy it to the save area		*/
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

#ifndef VMS	/* unix and MSDOS */
static dev_list(where)								/* generate a list of devs			*/
char *where;
{ 
	unsigned char list[256]; 						/* list can be 255 long (bytes)			*/
	int devtype, devcnt, listind; 						/* working vars for relevant passed info	*/
	char cnfname[100];
	FILE *cnf;
	char inbuf[100];

	devtype = (int) *where;							/* type to find					*/
	devcnt  = (int) *(where+1);						/* number to return				*/ 
	strcpy(cnfname,configfile());
	if ((cnf=fopen(cnfname,"r"))==NULL)
	{
		werrlog(ERRORCODE(10),cnfname,errno,0,0,0,0,0,0);
		*where = (char)0;
		*(where+1) = (char)0;
		return(0);
	}
	listind=0;
	while (fgets(inbuf,80,cnf))						/* find all of the matching files		*/
	{
		char tmp[4];

		if (!isdigit(inbuf[0])) continue;				/* skip dev type lines */
		
		memset(tmp,(char)0,sizeof(tmp));
		strncpy(tmp,inbuf,3);						/* copy the number part				*/
		list[listind++] = (unsigned char) atoi(tmp);			/* change to int, cast to char, store in array	*/
	}
	*where = (char) listind;						/* return count					*/
	*(where+1) = (char) devcnt;						/* and number returned				*/
	memcpy(where+2,list,devcnt);						/* and actual list				*/
	fclose(cnf);
}
/*	#ifdef unix and MSDOS */
static dev_info(where)								/* get info for a device number			*/
char *where;									/* return area					*/
{
	int devnum; 								/* device number var				*/
	char cnfname[100];
	FILE *cnf;
	char inbuf[100];

	devnum = (int) *where;							/* init the dev number				*/
	strcpy(cnfname,configfile());
	if ((cnf=fopen(cnfname,"r"))==NULL)
	{
		werrlog(ERRORCODE(10),cnfname,errno,0,0,0,0,0,0);
		return(0);
	}
	while (fgets(inbuf,80,cnf))						/* will only match one file			*/
	{
		if (!isdigit(inbuf[0])) continue;
		
		switch (*(inbuf+WCLASS))					/* now return the appropriate code		*/
		{
			case 'w':
				*where = (char)1;
				break;
			case 'm':
				*where = (char)2;
				break;
			case 'd':
				*where = (char)3;
				break;
			case 'l':
				*where = (char)4;
				break;
		}
		*(where+1) = (char) gettype(inbuf+WTYPE);			/* and type byte				*/
	}
	fclose(cnf);
}
/*	#ifdef unix and MSDOS */
static int gettype(name)							/* return type number for a kind of device	*/
char *name;									/* device type name				*/
{
	FILE *types;
	char *p, *strchr();
	char inbuf[80];

	types = fopen(configfile(),"r");					/* typefile setup by wispdev			*/
	if (types==NULL)							/* oops, file isn't there			*/
	{
		werrlog(ERRORCODE(6),0,0,0,0,0,0,0,0);				/* Error accessing device types.		*/
		return(255);
	}
	while (fgets(inbuf,80,types))						/* read the records in. record format is:	*/
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

/*	#ifdef unix and MSDOS */
static char *configfile()
{
	static int notcalled=1;
	static char cfgpath[128];
	char *getenv();
	
	if (notcalled)
	{
		if (getenv(WISP_CONFIG_ENV)) 
			sprintf(cfgpath,"%s/%s",getenv(WISP_CONFIG_ENV),CFGFNAME);
		else no_wispconfig();
		notcalled=0;
	}
	return( cfgpath );
}

/*	#ifdef unix and MSDOS */
static osd_mode(mode)
unsigned char *mode;
{
#undef          ROUTINE
#define		ROUTINE		65600
	char	*p;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	if ( wbackground() )
	{
		*mode = 'B';								/* No terminal attached. 		*/
	}
	else
	{
		*mode = 'F';								/* Interactive task. 			*/
	}
}

/*	#ifdef unix and MSDOS */
static osd_jname(name)
char *name;
{
#undef          ROUTINE
#define		ROUTINE		65700
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);
	memset(name, ' ', 8);
}

/*	#ifdef unix and MSDOS */
/*
	osd_term	This routine returns the Wang style workstation device number or -1 if in background.
			On any error 255 is returned as the device number.
*/
static osd_term(tnum,tflags)
long *tnum;
int *tflags;
{
#undef          ROUTINE
#define		ROUTINE		65500
static	int	first=1;
static	long	device_num;
	char	*p;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	if (!first)
	{
		*tnum = device_num;
		*tflags = 0;
		return;
	}
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
			device_num = 255;						/* Correct a bad return from findtty 	*/
			werrlog(ERRORCODE(3),0,0,0,0,0,0,0,0);				/* Give a warning			*/
		}
	}

	*tnum = device_num;
	*tflags = 0;									/* Hardwire flag values. (not used)	*/
	return;
}

/*	#ifdef unix and MSDOS */
/*
	findtty		This routine returns the Wang style workstation device number as found in wsysconfig.
			If unable to find the tty for any reason it will return -1.
			A valid value is 0 - 254.
*/
static findtty(where)
long *where;
{
#undef          ROUTINE
#define		ROUTINE		65100

static	int 	first=1;
static	long	device_num;

	char *ttyname(), *tty;
	char *configfile();
	char cnfname[100];
	FILE *cnf;
	char inbuf[256];

	if (!first) 
	{
		*where = device_num;						/* Return stored value			*/
		return;
	}
	first = 0;
	
	if ((tty=ttyname(0))==NULL)						/* If unable to get the tty name		*/
	{
		device_num= -1;
		*where = device_num;
		return;
	}
 
	strcpy(cnfname,configfile());
	if ((cnf=fopen(cnfname,"r"))==NULL)					/* If unable to open wsysconfig			*/
	{
		werrlog(ERRORCODE(10),cnfname,errno,0,0,0,0,0,0);
		device_num = -1;
		*where = device_num;
		return;
	}

	device_num = -1;							/* Assume not found 				*/
	while (fgets(inbuf,sizeof(inbuf),cnf))					/* find all of the matching files		*/
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
					werrlog(ERRORCODE(12),device_num,tty,0,0,0,0,0,0);
					device_num = -1;
				}
				break;
			}
		}
	}
	fclose(cnf);
	*where = device_num;
	return;
}

#endif	/* unix and MSDOS */
