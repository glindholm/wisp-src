/*
 * Module:  utils.c
 * Program: IDSIprint 
 *
 * $Log: utils.c,v $
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
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern 
#include "defs.h"

lockfile(type,val)
int type,val;
{
	int lockfd;
	int lockinfo;
	int ret;
	
	if (val==DAEMON_RUNNING && type==LK_STAT)
		return daemon_lock_check();
	lockfd=open(LOCKFILE,O_RDWR);
	if (lockfd== -1) return 0;
	lockf(lockfd,F_LOCK,4);
	read(lockfd,&lockinfo,sizeof(lockinfo));
	lseek(lockfd,0,0);
	
	switch (type)
	{
	      case LK_SET:
		if(lockinfo & val) ret=1;
		else
		{
			lockinfo |= val;
			write(lockfd,&lockinfo,sizeof(lockinfo));
			ret=0;
		}
		break;
	      case LK_CLEAR:
		lockinfo &= ~val;
		write(lockfd,&lockinfo,sizeof(lockinfo));
		ret=0;
		break;
	      case LK_STAT:
		ret = (lockinfo & val)!=0;
		break;
	}
	lseek(lockfd,0,0);
	lockf(lockfd,F_ULOCK,4);
	close(lockfd);
	return ret;
}
/*
  the next two routines were added before the 3.2 release to 
  improve the "daemon running" check.  When we have more time,
  the LOCKFILE scheme should be redone, but for now, we use
  these functions.  daemon_lock_set establishes a lock on 
  bytes 4-7 of the lockfile.  daemon_lock_check just checks
  for the lock 
*/
daemon_lock_check()
{
	int lockfd;
	int lockst;
	
	lockfd=open(LOCKTMP,O_RDWR);
	if (lockfd < 0)
	  return FALSE;
	lockst=lockf(lockfd,F_TEST,4);
	close(lockfd);
	if (lockst < 0)
	  return TRUE;
	else 
	  return FALSE;
}
daemon_lock_set()
{
	int lockfd;

	lockfd=open(LOCKTMP,O_RDWR|O_CREAT);
	chmod(LOCKTMP,0666);
	write(lockfd,&lockfd,4);
	lseek(lockfd,0,0);
	lockf(lockfd,F_LOCK,4);
}
char *gmem(sz)
int sz;
{
	char *calloc();
	static char *tmp;
	
	tmp=calloc(sz,1);
	return tmp;
}
void truncspc(p)
char *p;
{
	char *tmpp,*gmem();
	char *st,*end;
	
	if (strlen(p))
	{
		tmpp=gmem(strlen(p)+1);
		strcpy(tmpp,p);
		for(st=tmpp; *st && *st==' '; ++st);
		for(end=st;*end && *end!=' ';++end);
		if(*end) *end='\0';
		memcpy(p,st,end-st>0?end-st:0);
		p[end-st>0?end-st:0];
	}
}
#ifdef MQCODE
key_t myftok(file,x)								/* For IBM - generate unique numbers.	*/
char *file;										/* Replaces FTOK (system one doesn't 	*/
char x;											/*  work properly for IBM.)		*/
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
#endif
#define VLEN 5
#define MDLEN 20
make_vers_date(idstr,version,moddate)
char *idstr, *version, *moddate;
{
	char *p,*e,*strchr();
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
chk_vers(type)
int type;
{
	if (type&QVERS && chk_qvers()<0) 
	  return INCOMPAT_VERS;
	if (type&QVERS && chk_qvers()<0) 
	  return INCOMPAT_VERS;
	if (type&CVERS && chk_cvers()<0) 
	  return INCOMPAT_VERS;
	return 0;
}
chk_qvers()
{
	return chk_dump(QDUMPFILE);
}
chk_pvers()
{
	return chk_dump(PDUMPFILE);
}
chk_cvers()
{
	return chk_dump(CDUMPFILE);
}
chk_dump(path)
char *path;
{
	int dump,ret=0;
	unsigned long magic, version;
	
	dump=open(path,O_RDONLY);
	if (dump<0)
	  ret = INCOMPAT_VERS;
	if (read(dump,&magic,sizeof(magic))<0)
	  ret = INCOMPAT_VERS;
	if (read(dump,&version,sizeof(version))<0)
	  ret = INCOMPAT_VERS;
	if ((magic & 0xffffff00) != (QDUMPMAGIC & 0xffffff00))
	  ret = INCOMPAT_VERS;
	if (version != DUMPVERSION)
	  ret = INCOMPAT_VERS;
	if (dump>=0) close(dump);
	return ret;
}
