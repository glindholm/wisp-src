/*
 * Module:  daemon_misc
 * Program: IDSIprint
 * Purpose: misc routines for daemon
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern 
#include "defs.h"
#include "daemon.h"

FILE *errlogFILE = NULL;

extern char *sys_errlist[];

/*
 * get the time stamp of a file
 */
time_t filetime(char *path)
{
	struct stat buf;
	
	if (stat(path,&buf)==0)
	  return buf.st_ctime;
	else
	{
		sprintf(errmsg,"Error reading [stat] %s: %s\n",
			path, sys_errlist[errno]);
		logerr(errmsg,0);
		return 0;
	}
}
/*
 * this used to be logerr, but was renamed to reallogerr.  logerr is now a macro which calls
 * this routine.  This was done to allow the second arg to logerr to be any type 
 */
void reallogerr(char *buffer)
{
	static char errbuf[1024];
	char ctbuf[40];
	time_t t,time(time_t *);
	
	/* open if necessary */
	if (errlogFILE==NULL) 
	{
		errlogFILE=fopen(errlogfile,"a");
#ifdef UNIQUE
		chmod(errlogfile,0644);
		if (access(olderrlogfile,0)!=0)
		{
			link(errlogfile,olderrlogfile);
		}
#endif
	}
	t=time((time_t*)0);         /* make a time stamp */
	strcpy(ctbuf,ctime(&t));
	if (ctbuf[strlen(ctbuf)-1]=='\n') ctbuf[strlen(ctbuf)-1]=(char)0;
	
	fprintf(errlogFILE,"%s:%s",ctbuf,buffer); /* print the info into the log */
	fflush(errlogFILE);
}
/*
 * general memory grabber
 */
char *gmem(int cnt, int sz)
{
	char *calloc();
	static char *tmp;
	
	tmp=calloc(cnt,sz);
	if (tmp==NULL)
	{
		logerr("Out of memory (%d bytes requested)\n",sz*cnt);
		logerr("IDAEMON shutting down\n",0);
		daemon_shutdown(1);
	}
	else
	  return tmp;
}

/*
 * Semaphore code follows.  The semaphores are used to synchonize ops between the daemon and
 * client programs.  This prevents the clients from reading partially written shared memory
 * segments etc.  When writing, the daemon will wait for the READ sem to be zero, then increment
 * the WRITE sem.  These two actions are atomic.  The readers will wait for the WRITE sem to
 * be zero, then increment the READ sem.  There is also a semaphore which is used to make
 * the clients wait before contacting the daemon, and a semaphore which indicates that the
 * daemon is running.  
 *
 */

static int master_semid = -1;             /* id used to access the sem struct */

static struct sembuf SEM_ChkRunning[1];	  /* these structs are used for sem opts */
static struct sembuf SEM_SetRunning[1];
static struct sembuf SEM_ClrWait[1];

static struct sembuf SEM_SetWriting[2];
static struct sembuf SEM_ClrWriting[1];

union _semun 
{
	int val;
	struct semid_ds *buf;
	unsigned short *array;
} semctl_arg;                             /* a struct for use with the semctl() fn  */

/* 
 * synchronously set a flag (init if necessary)
 *
 */
int setflag(int flag, int setting)
{
	int status;
	unsigned short *sems;
	
	if (master_semid == -1)	          /* init if necessary */
	{
		status = init_semaphore();
		
		if (status != IERR_OK)	  /* return if error on init */
		  return status;
	}
	switch (flag)			  /* now decide what to do */
	{
	      case SEMRUNNING:		  /* set running flag? */
		if (setting == FLAG_SET)
		  status = semop(master_semid,SEM_SetRunning,1); /* yes, set it */
		else if (setting == FLAG_CHECK)	/* no check it */
		{
			status = semctl(master_semid,SEMRUNNING,GETVAL,0); /* call semctl to check flag value */
			return status;
		}
		break;
	      case SEMWRITING:		  /* set writing flag */
		if (setting == FLAG_SET)  /* use a different struct array for each command */
		  status = semop(master_semid,SEM_SetWriting,2);
		else
		  status = semop(master_semid,SEM_ClrWriting,1);
		break;
	      case SEMWAIT:		  /* set wait flag */
		status = semop(master_semid,SEM_ClrWait,1);
		break;
	}
	if (status)			  /* something bad happened during semop */
	{
		/* report the error */
		sprintf(errmsg,"Error during semaphore operation (%d/%d): %s\n",
			flag,setting,sys_errlist[errno]);
		logerr(errmsg,0);
		semctl(master_semid,8,GETALL,semctl_arg);
		sems=semctl_arg.array;
		sprintf(errmsg,"Running:%d Read:%d Write:%d %d Wait:%d\n",
			sems[0],sems[1],sems[2],sems[3],sems[4]);
		logerr(errmsg,0);
		return IERR_SEMAPHORE;
	}
	else
	  return IERR_OK;
}
/*
 * init the sem struct and the sem cmd arrays 
 *
 */
int init_semaphore(void)
{
	int semkey, semid;
	int fd;
	int status;
	unsigned short sems[8];

        SEM_ChkRunning[0].sem_num = SEMRUNNING;	/* running sem */
	SEM_ChkRunning[0].sem_op  = 0;		/* expecting value 0 */
	SEM_ChkRunning[0].sem_flg = IPC_NOWAIT;	/* don't wait */ 

        SEM_SetRunning[0].sem_num = SEMRUNNING;	/* running sem */
	SEM_SetRunning[0].sem_op  = 1;		/* add one to it */    
	SEM_SetRunning[0].sem_flg = SEM_UNDO;	/* undo if I exit */ 

        SEM_ClrWait[0].sem_num = SEMWAIT;	/* clear the wait flag */
	SEM_ClrWait[0].sem_op  = -1;		/* sub 1 from the flag */
	SEM_ClrWait[0].sem_flg = 0;
	
	SEM_SetWriting[0].sem_num = SEMREADING;	/* check reading flag */
	SEM_SetWriting[0].sem_op  = 0;		/* must be 0 */
	SEM_SetWriting[0].sem_flg = 0;		/* once the first cond is met, */
	SEM_SetWriting[1].sem_num = SEMWRITING;	/* we can mess with semwriting */
	SEM_SetWriting[1].sem_op  = 1;		/* add 1 */
	SEM_SetWriting[1].sem_flg = SEM_UNDO;	/* undo if I exit */
	
	SEM_ClrWriting[0].sem_num = SEMWRITING;	/* to clear the writing sem */
	SEM_ClrWriting[0].sem_op  = -1;		/* sub 1 from it */
	SEM_ClrWriting[0].sem_flg = SEM_UNDO;	/* this must be kept undo. this keeps the undo value
						   from getting too big */
	
	/* now get the sem key */
	semkey = getipckey(semkeyfile,"semaphore",IPCKEYMOD);
	if (semkey == -1)
	  return IERR_SEMAPHORE;
	else if (semkey == -2)
	  return IERR_NODIR;      /* this will happen if idaemon has never run on this system before. */
				  /* no problem.  caller will fix and re-call this routine */
	while (master_semid == -1)
	{
		/* now attempt to get a new sem segment, deleting old ones if necessary */
		if ((master_semid = semget(semkey,8,0777|IPC_CREAT|IPC_EXCL))<0)
		{
			if (errno==EEXIST)
			{
				/* it exists, so get its id */
				master_semid = semget(semkey,8,0777);
				if (master_semid == -1)
				{
					/* couldn't get it */
					sprintf(errmsg,"Error getting semaphore for key %x: %s\n",
						semkey,sys_errlist[errno]);
					logerr(errmsg,0);
					return IERR_SEMAPHORE;
				}	
				/* double check that another daemon isn't running */
				status = semop(master_semid,SEM_ChkRunning,1);
				if (status == -1 && errno == EAGAIN)
				{
					return IERR_RUNNING;
				}
				/* remove the stale sem struct */
				status = semctl(master_semid,8,IPC_RMID,0);
				if (status == -1)
				{
					sprintf(errmsg,"Error removing old semaphore id %d: %s\n",
						master_semid,sys_errlist[errno]);
					logerr(errmsg,0);
					return IERR_SEMAPHORE;
				}
				master_semid = -1;
				/* loop again and try to create the sem struct */
			}
			else
			{
				/* it was some other wierd sem error on the create. */
				sprintf(errmsg,"Error creating semaphore key %x: %s\n",
						semkey,sys_errlist[errno]);
				logerr(errmsg,0);
				return IERR_SEMAPHORE;
			}
		}
	}
	memset(sems,0,sizeof(sems));		/* now setup to init the sem values */
#ifdef SOCKCODE
	sems[SEMCOMMTYPE] = TYPESOCKET;		/* here is a sem for IPC type */
#endif
#ifdef MQCODE
	sems[SEMCOMMTYPE] = TYPEMQCODE;
#endif	
	sems[SEMWAIT] = 1;			/* set wait to 1, this prevents clients from talking to use */
	semctl_arg.array = sems;		/* till we're ready. */
	if (semctl(master_semid,8,SETALL,semctl_arg)<0)	/* set the values */
	{
		sprintf(errmsg,"Error initializing semaphore id %d: %s\n",
			master_semid,sys_errlist[errno]);
		logerr(errmsg,0);
		return IERR_SEMAPHORE;
	}
	return IERR_OK;
}
/*
 * given a path, generate a ipc usable key value
 *
 */
key_t getipckey(char *path, char *type, unsigned char mod)
{
	extern char errmsg[];
	key_t key;
	int fd;
	
	if (access(path,0)!=0)
	{
		if ((fd=creat(path,0777))<0)
		{
			if (errno != ENOENT)
			{
				sprintf(errmsg,"Error creating %s key file %s: %d\n",
					type,path,errno);
				logerr(errmsg,0);
			}
			return -2;
		}
		close(fd);
		chmod(path,0777);
	}
	key = myftok(path, mod);
	if (key == (key_t)-1)
	{
		sprintf(errmsg,"Error getting %s key file %s: %d\n",
			type,path,errno);
		logerr(errmsg,0);
	}
	return key;
}
/*
 * substitute macro values for printer and filter cmds. it's a mini parser, don't disrupt it
 */
char *dosubst(char *cmd, struct q_item *qitem, int plen)
{
	static char buf[1024];
	int backslash, dollar;
	int pos,npos;
	char *d_username(int uid);
	char macroname[33];
	char macroend;
	
	for (bzero(buf,sizeof(buf)),backslash=dollar=pos=0; *cmd; ++cmd)
	{
		if (backslash)
		{
			backslash=0;
			buf[pos++] = *cmd;
			continue;
		}
		if (dollar)
		{
			dollar=0;
			switch (*cmd)
			{
			      case '[':
				macroend=']';
				++cmd;
				break;
			      case '{':
				macroend='}';
				++cmd;
				break;
			      case '(':
				macroend=')';
				++cmd;
				break;
			      default:
				macroend=0;
				break;
			}
			npos=0;
			do
			{
				macroname[npos++] = *cmd;
				if (macroend && *(cmd+1) == macroend)
				{
					++cmd;
					break;
				}
				else
				  if (!isalpha(*(cmd+1)))
				    break;
				++cmd;
			} while (1);			
			macroname[npos]=(char)0;
			
			     if (!strcmp(macroname,"filename"))
			{
				char *p, *strrchr(CONST char *, int);
				
				p = strrchr(qitem->orig_path,(int)'/');
				if (p)
				  strcat(&buf[pos],p+1);
				else
				  strcat(&buf[pos],qitem->real_path);
			}
			else if (!strcmp(macroname,"fullpath"))
			{
				strcat(&buf[pos],qitem->real_path);
			}
			else if (!strcmp(macroname,"pagelen"))
			{
				sprintf(&buf[pos],"%d",plen);
			}
			else if (!strcmp(macroname,"userid") || !strcmp(macroname,"username"))
			{
				strcat(&buf[pos],d_username(qitem->owner_uid));
			}
			else if (!strcmp(macroname,"usernid"))
			{
				sprintf(&buf[pos],"%d",qitem->owner_uid);
			}
			else if (!strcmp(macroname,"printer"))
			{
				strcat(&buf[pos],qitem->actual_prtr);
			}
			else if (!strcmp(macroname,"form"))
			{
				strcat(&buf[pos],qitem->form);
			}
			else if (!strcmp(macroname,"class"))
			{
				sprintf(&buf[pos],"%c",qitem->class?qitem->class:' ');
			}
			else if (!strcmp(macroname,"copies"))
			{
				sprintf(&buf[pos],"%d",qitem->copies);
			}
			else if (!strcmp(macroname,"stpage"))
			{
				sprintf(&buf[pos],"%d",qitem->stpage);
			}
			else if (!strcmp(macroname,"endpage"))
			{
				sprintf(&buf[pos],"%d",qitem->endpage);
			}
			else if (!strcmp(macroname,"qid"))
			{
				sprintf(&buf[pos],"%d",qitem->q_id);
			}
			else if (!strncmp(macroname,"n",1))
			{
				strcat(&buf[pos],qitem->orig_path);
				if (npos > 1)
				{
					cmd -= --npos;
				}
			}
			else if (!strncmp(macroname,"l",1))
			{
				sprintf(&buf[pos],"%d",plen);
				if (npos > 1)
				{
					cmd -= --npos;
				}
			}
			else if (!strncmp(macroname,"u",1))
			{
				strcat(&buf[pos],d_username(qitem->owner_uid));
				if (npos > 1)
				{
					cmd -= --npos;
				}
			}
			pos = strlen(buf);
			continue;
		}
		switch (*cmd)
		{
		      case '\\':
			++backslash;
			break;
		      case '$':
			++dollar;
			break;
		      default:
			buf[pos++] = *cmd;
			break;
		}
	}
	buf[pos]=(char)0;
	return buf;
	
}	
int isnumeric(str)
char *str;
{
	int ret=1;
	while (*str)
	{
		if (!(*str >= '0' && *str <='9'))
		  ret=0;
		++str;
	}
	return ret;
}

/* daemon username routine.  */
static char nonuser[32];
char *d_username(int uid)
{
	struct passwd *pw, *getpwuid(uid_t);

	pw=getpwuid(uid);
	if (pw)
	{
		return pw->pw_name;
	}
	else
	{
		sprintf(nonuser,"%d",uid);
		return nonuser;
	}
}
int4 hextoi4(str)
char *str;
{
	int len,idx;
	int4 value=0;
	int base=1,dig;
	
	len=strlen(str);
	for (idx=len-1; idx>=0 && str[idx]!='x' && str[idx]!='X'; --idx, base *= 16)
	{
		dig = hexdig(str[idx]);
		if (dig== -1)
		{
			return 0;
		}
		value += dig*base;
	}
	return value;
}
int hexdig(ch)
int ch;
{
	if (ch >= '0' && ch <= '9')
	{
		return ch-'0';
	}
	if (ch >= 'a' && ch <= 'f')
	{
		return ch-'a'+10;
	}
	if (ch >= 'A' && ch <= 'F')
	{
		return ch-'A'+10;
	}
	return -1;
}


/*
 * $Log: d_misc.c,v $
 * Revision 1.17  1993/11/10  17:40:48  jockc
 * added simple func to test for numeric value
 *
 * Revision 1.16  1993/09/13  15:28:15  jockc
 * 499
 * 503
 *
 * Revision 1.16  1993/09/10  18:40:46  jockc
 * #ifdef UNIQUE for new error log names
 *
 * Revision 1.15  1993/08/13  22:17:36  jockc
 * const ->> CONST
 *
 * Revision 1.14  1993/08/13  18:39:00  jockc
 * added hex number converter to read udebug var, comments,
 * and started code to handle version checks.. need to finish
 *
 * Revision 1.13  1993/06/02  23:36:39  root
 * add qid to macro list
 *
 * Revision 1.12  1993/05/28  21:44:24  jockc
 * changed gmem(size) to gmem(count,size)
 *
 * Revision 1.11  1993/03/11  19:35:49  jockc
 * handle missing directory condition
 *
 * Revision 1.10  1993/03/01  23:33:06  jockc
 * change for Sun semaphore wierdness.. must use union as the third arg
 * to semctl, not the actual data.
 *
 * Revision 1.9  1993/01/13  23:34:17  jockc
 * add undo to other semwriting operation.  this change came
 * with a better understanding of how sem_undo is implemented
 *
 * Revision 1.8  1993/01/12  02:06:07  jockc
 * implemented semaphore ops here.  also moved some misc
 * stuff from utils to this module
 *
 * Revision 1.7  1992/12/31  23:42:41  jockc
 * qpath/ISPOOL support
 *
 * Revision 1.6  1992/10/09  18:24:30  jockc
 * explicit set permission of err log
 *
 * Revision 1.5  1992/06/09  23:14:41  jockc
 * posix fix
 *
 * Revision 1.4  1992/06/08  23:55:47  jockc
 * polish code to remove some warnings
 *
 * Revision 1.3  1991/10/01  17:06:09  jockc
 * open errlog in logerr if not already open
 *
 * Revision 1.2  1991/04/19  00:47:20  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:49:38  jockc
 * Initial revision
 *
 *
 */
