/*
 * Module:  utils.c
 * Program: IDSIprint 
 * Description:  functions used by both client and server
 *
 */
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern 
#include "defs.h"
#include "qpaths.h"

key_t myftok(char *file, char x)
                
            								/* For IBM - generate unique numbers.	*/
           										/* Replaces FTOK (system one doesn't 	*/
       											/*  work properly for IBM.)		*/
{
#ifdef _AIX
	struct stat buf;
	int	rc;

	rc = stat(file,&buf);
	if (rc == -1)
	{
		return( (key_t) -1 );
	}
	if (buf.st_ino&0xff000000) printf("warning %08x\n",buf.st_ino);
	return (key_t)(((key_t)x<<24)|(buf.st_ino&0x00ffffff));
#else
	return ftok(file,x);
#endif
}

#define VLEN 5
#define MDLEN 20
void make_vers_date(char *idstr, char *version, char *moddate)
{
	char *p,*e,*strchr(CONST char *, int);
	extern long version_num, revision_num;
	
	memset(version,0,VLEN);
	p=strchr(idstr,'V');
	if (p)
	{
		e=strchr(++p,' ');
		memcpy(version,p,e-p);
		p=strchr(version,'_');
		if (p) *p='.';
	}
	else
	{
		p=strchr(idstr,' ');
		p=strchr(++p,' ');
		e=strchr(++p,' ');
		version[0]='~';
		memcpy(version+1,p,e-p);
	}
	memset(moddate,0,MDLEN);
	p=strchr(idstr,' ');
	p=strchr(++p,' ');
	p=strchr(++p,' ');
	e=strchr(++p,' ');
	memcpy(moddate,p,e-p);
}
#define REPMAX 4096
char *repchar(cnt, ch)
int cnt,ch;
{
	static char repbuf[REPMAX];
	static int lastch= -1;
	static int lastcnt= -1;

	if (cnt>REPMAX)
	{
		cnt=REPMAX;
	}
	if (lastch != ch)
	{
		memset(repbuf,ch,cnt);
		lastch = ch;
		lastcnt = cnt;
		repbuf[cnt]='\0';
		return repbuf;
	}
	else
	{
		if (lastcnt == cnt)
		{
			return repbuf;
		}
		else if (lastcnt < cnt)
		{
			memset(repbuf+lastcnt,ch,cnt-lastcnt);
			lastch = ch;
			lastcnt = cnt;
			repbuf[cnt]='\0';
			return repbuf;
		}
		else
		{
			lastch = ch;
			lastcnt = cnt;
			repbuf[cnt]='\0';
			return repbuf;
		}
	}
}

/*
 * $Log: utils.c,v $
 * Revision 1.21  1993/09/13  15:28:29  jockc
 * 499
 * 503
 *
 * Revision 1.21  1993/09/10  18:28:03  jockc
 * added repchar function
 *
 * Revision 1.20  1993/08/13  22:08:51  jockc
 * const
 *
 * Revision 1.19  1993/08/13  20:14:21  jockc
 * changes for alpha/c89
 *
 * Revision 1.18  1993/01/12  02:09:58  jockc
 * moved many functions to more appropriate modules.  this module
 * is only for functions used in common between client and server
 *
 * Revision 1.17  1993/01/04  22:42:23  jockc
 * took out lockfour on dumpread
 *
 * Revision 1.16  1992/12/31  23:51:25  jockc
 * qpaths/ISPOOL.  fixed null action in truncspc.
 *
 * Revision 1.15  1992/10/28  22:14:11  jockc
 * fixed username() crash on nonexistant user
 *
 * Revision 1.14  1992/10/09  21:41:51  jockc
 * a slight adjustment to lockfour..  for loop would sometimes not execute
 *
 * Revision 1.13  1992/10/09  20:19:46  jockc
 * revised the locking scheme.  moved dosubst here, and added
 * more macros.
 * moved x_username here
 *
 * Revision 1.12  1992/07/07  23:07:07  jockc
 * added improved lock mech for daemon detect.  Still
 * need to rework entire lockfile logic
 *
 * Revision 1.11  1992/06/12  20:12:03  jockc
 * changed truncspc to void
 *
 * Revision 1.10  1992/06/09  23:14:57  jockc
 * posix fix
 *
 * Revision 1.9  1992/05/20  23:19:36  jockc
 * added version checking functions for dump files
 *
 * Revision 1.8  1992/04/30  19:04:46  jockc
 * added make_vers_date function to be used by all
 * ilp stuff to generate the master version number/date
 *
 * Revision 1.7  1992/03/27  23:15:31  jockc
 * chopped out unused iportnum()
 *
 * Revision 1.6  1991/10/11  00:06:32  jockc
 * null term the string in fn truncspc
 *
 * Revision 1.5  1991/10/10  22:40:42  jockc
 * truncspace now chops leading spaces
 *
 * Revision 1.4  1991/10/01  17:11:55  jockc
 * added ftok stuff for brain dead aix
 *
 * Revision 1.3  1991/04/19  00:47:36  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/19  00:23:08  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:52:19  jockc
 * Initial revision
 *
 *
 */
