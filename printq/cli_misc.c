/*
 * Module:  cli_misc
 * Program: IDSIprint
 * Purpose: misc routines for clients, mostly just sem related
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern
#include "defs.h"
#include "qpaths.h"

static int master_semid = -1;

static struct sembuf SEM_ChkRunning[1];
static struct sembuf SEM_Wait[1];
static struct sembuf SEM_SetReading[2];
static struct sembuf SEM_ClrReading[1];

/*
 * attempt to set a flag.  Currently the only flag to set is READING
 *
 */

int init_semaphore (void);

setflag(int flag, int setting)
{
	int status;
	
	/* init if necessary */
	if (master_semid == -1)
	{
		status = init_semaphore();
		
		if (status != IERR_OK)
		  return status;
	}
	switch (flag)
	{
	      case SEMREADING:
		if (setting == FLAG_SET)
		  status = semop(master_semid,SEM_SetReading,2);      /* set or clear reading */
		else
		  status = semop(master_semid,SEM_ClrReading,1);
		break;
	}
	if (status)
	{
		return IERR_SEMAPHORE;
	}
	else
	  return IERR_OK;
}
/* 
 * see if semrunning flag is set, which indicates the presence of an idaemon
 */
isrunning(void)
{
	int status;

	if (master_semid == -1)
	{
		status = init_semaphore();
		
		if (status != IERR_OK)
		  return status;
	}
	status = semop(master_semid,SEM_ChkRunning,1);

	if (status == -1 && errno == EAGAIN)
	  return IERR_OK;
	else
	  return IERR_NODAEMON;
}
/*
 * see if this is a socket flavored daemon running 
 *
 */
issocket(void)
{
	int status;

	status = semctl(master_semid,SEMCOMMTYPE,GETVAL,0);
	if (status == TYPESOCKET)
	  return TRUE;
	else
	  return FALSE;
}
/*
 * see if this is an mq flavored daemon running 
 *
 */
ismqcode(void)
{
	int status,val;

	status = semctl(master_semid,SEMCOMMTYPE,GETVAL,0);
	if (status == TYPEMQCODE)
	  return TRUE;
	else
	  return FALSE;
}
static int waitto=FALSE;
void waittimeout(int dummy)
{
	++waitto;
}
/* 
 * wait until the daemon is ready to hear from us
 *
 */
waitready(void)
{
	int status;
	waitto = FALSE;
	signal(SIGALRM,waittimeout);
	alarm(5);
	status = semop(master_semid,SEM_Wait,1);
	alarm(0);
	if (waitto)
	  return FALSE;
	else
	  return TRUE;
}
/* 
 * init the semaphore structs, and the sem access variables 
 */
init_semaphore(void)
{
	int semid;
	key_t semkey, myftok(char *file, char x);
	int fd;
	int status;
	
        SEM_ChkRunning[0].sem_num = SEMRUNNING;   /* see d_misc for an brief explanation of how these are used */
	SEM_ChkRunning[0].sem_op  = 0;
	SEM_ChkRunning[0].sem_flg = IPC_NOWAIT;

        SEM_Wait[0].sem_num	  = SEMWAIT;
	SEM_Wait[0].sem_op	  = 0;
	SEM_Wait[0].sem_flg	  = 0;

	SEM_SetReading[0].sem_num = SEMWRITING;
	SEM_SetReading[0].sem_op  = 0;
	SEM_SetReading[0].sem_flg = 0;
	SEM_SetReading[1].sem_num = SEMREADING;
	SEM_SetReading[1].sem_op  = 1;
	SEM_SetReading[1].sem_flg = SEM_UNDO;
	
	SEM_ClrReading[0].sem_num = SEMREADING;
	SEM_ClrReading[0].sem_op  = -1;
	SEM_ClrReading[0].sem_flg = SEM_UNDO;

	/* look for key file */
	if (access(semkeyfile,0)!=0)
	{
		return IERR_NODAEMON;
	}
	/* gen a key from it */
	semkey = myftok(semkeyfile, IPCKEYMOD);
	if (semkey == -1)
	{
		return IERR_SEMAPHORE;
	}
	/* get the id from the key */
	if ((master_semid = semget(semkey,8,0777))<0)
	{
		return IERR_NODAEMON;
	}
	
	return IERR_OK;
}
/* closeout sem stuff */
shut_sems(void)
{
	master_semid= -1;
}
/*
 * grab a chunk of memory with checking
 *
 */
char *gmem(int cnt, int sz)
{
	char *calloc();
	static char *tmp;
	
	tmp=calloc(cnt,sz);
	if (tmp==NULL)
	{
		vexit(1);
		exit(1);
	}
	else
	  return tmp;
}
