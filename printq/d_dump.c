/*
 * Module: d_dump
 * Purpose: implement the share memory maps
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern 
#include "defs.h"
#include "daemon.h"

/*#define DBGSHDUMP*/

/*
**	Routine:	dump_stuff()
**
**	Function:	To write info about state of daemon into files and shmem
**
**	Description:	Calls appropriate functions for various dumps
**
**	Input:		None
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	5/93	Written by JEC
**
*/

extern int ring_add (ring_struct *ring_ptr, int position, char *element);

void dump_stuff(void)
{
	static time_t last_filedump= (time_t)-1;
	time_t curtime, time(time_t *);
	int status;
	
	curtime = time(&curtime);
	
	/* don't want to dump too often */
	if (curtime - last_filedump > FILEDUMP_FREQ)
	{
		dump_q();				/* dump queue data to dump file */
		dump_p();				/* dump printer data to dump file */
		last_filedump = curtime;
	}
	dump_conf();
#ifdef 	DUMP_SHMEM

	status = setflag(SEMWRITING,FLAG_SET);   	/* synchonize with readers, set writing flag */
	if (status == IERR_SEMAPHORE)			/* something went wrong with our semaphore resource */
	{
		logerr("Daemon shutdown due to semaphore failure\n",0);
		daemon_shutdown(1);
	}
	dump_q_shmem();					/* dump queue data to shmem */
	dump_p_shmem();					/* dump printer data to shmem */ 
	status = setflag(SEMWRITING,FLAG_CLEAR);	/* clear write flag */
	if (status == IERR_SEMAPHORE)
	{
		logerr("Daemon shutdown due to semaphore failure\n",0);
		daemon_shutdown(1);
	}
#endif
}
/*
 *  dump the queue into a file 
 */
void dump_q(void)
{
	int dump;
	int4 magic = QDUMPMAGIC;
	int4 version = DUMPVERSION;
	int qpos,qcnt;
	QITEM *cur;
	
	if (need_to_dump==FALSE) return;

	/* attempt to open the dump file */
	if ((dump=open(qdumpfile,O_WRONLY|O_TRUNC|O_CREAT,0666))<0)
	{
		sprintf(errmsg,"Error opening queue dump file %s: %s\n",
			qdumpfile,sys_errlist[errno]);
		logerr(errmsg,0);
		return;
	}
	/* write magic # and version # */
	write(dump,(char*)&magic,sizeof(magic));
	write(dump,(char*)&version,sizeof(version));

	ring_count(queuelist,&qcnt);                     /* get count of q items */
	for (qpos=0; qpos<qcnt; ++qpos)			 /* now loop and  */
	{
		ring_get(queuelist,qpos,(char*)&cur);	 /* get each item */
		
		write(dump,(char*)cur,sizeof(QITEM));	 /* and write it */
	}
	close(dump);					 /* close the file */
	chmod(qdumpfile,0666);				 /* set appropriate perms */
}
/* 
 * dump the printer list into a file. 
 *
 * works just like dump_q above
 */
void dump_p(void)
{
	int dump;
	int4 magic = PDUMPMAGIC;
	int4 version = DUMPVERSION;
	int ppos,pcnt;
	PITEM *cur;
	
	if (need_to_dump_p==FALSE) return;

	if ((dump=open(pdumpfile,O_WRONLY|O_TRUNC|O_CREAT,0666))<0)
	{
		sprintf(errmsg,"Error opening printer dump file %s: %s\n",
			pdumpfile,sys_errlist[errno]);
		logerr(errmsg,0);
		return;
	}
	write(dump,(char*)&magic,sizeof(magic));
	write(dump,(char*)&version,sizeof(version));
	ring_count(printerlist,&pcnt);
	
	for (ppos=0; ppos<pcnt; ++ppos)
	{
		ring_get(printerlist,ppos,(char*)&cur);
		
		write(dump,(char*)cur,sizeof(PITEM));
	}
	close(dump);
/*	need_to_dump_p=FALSE;*/
	chmod(pdumpfile,0666);
}
/*
 * dump general queue config info
 *
 */
void dump_conf(void)
{
	int dump;
	int4 magic = CDUMPMAGIC;
	int4 version = DUMPVERSION;
	
	if (need_to_dump_cfg==FALSE) return;
	if ((dump=open(cdumpfile,O_WRONLY|O_TRUNC|O_CREAT,0666))<0)
	{
		sprintf(errmsg,"Error opening ilpconfig dump file %s: %s\n",
			cdumpfile, sys_errlist[errno]);
		logerr(errmsg,0);
		return;
	}
	write(dump,(char*)&magic,sizeof(magic));
	write(dump,(char*)&version,sizeof(version));
	write(dump,(char*)&qconfig,sizeof(qconfig));
	close(dump);
	need_to_dump_cfg=FALSE;
	chmod(cdumpfile,0666);
}
/*
 * load queue dump info (at daemon startup)
 *
 */
void load_qdump(void)
{
	int dump;
	QITEM cur,*new;
	char *gmem(int cnt, int sz);
	int4 magic, version;
	int st;
	
	if ((dump=open(qdumpfile,O_RDONLY))<0)
	{
		return;
	}

	/* check magic and version first.  Wrong magic or version print warning and version */
	if ((st=read(dump,(char*)&magic,sizeof(magic)))!=sizeof(magic))
	{
		int saveerrno;
		
		if (st == -1)
		{
			saveerrno = errno;
		}
		logerr("Warning: Attempting to read Queue Dump file %s:\n",qdumpfile);
		sprintf(errmsg,"         Wanted %d bytes [magic], read %d.\n",sizeof(magic),st== -1?0:st);
		logerr(errmsg,0);
		if (st == -1)
		{
			logerr("         Possible reason: %s\n",sys_errlist[saveerrno]);
		}
		logerr("         Queue Dump file ignored.\n",0);
		goto exit_qdump;
	}
	if (magic != QDUMPMAGIC)
	{
		logerr("Warning: Reading Queue Dump file %s:\n",qdumpfile);
		sprintf(errmsg,"         Invalid magic number.  Expected %08X, read %08x\n",
			QDUMPMAGIC,magic);
		logerr(errmsg,0);
		logerr("         Queue Dump file ignored.\n",0);
		goto exit_qdump;
	}
	  
	if ((st=read(dump,(char*)&version,sizeof(version)))!=sizeof(version))
	{
		int saveerrno;
		
		if (st == -1)
		{
			saveerrno = errno;
		}
		logerr("Warning: Attempting to read Queue Dump file %s:\n",qdumpfile);
		sprintf(errmsg,"         Wanted %d bytes [version], read %d.\n",sizeof(version),st== -1?0:st);
		logerr(errmsg,0);
		if (st == -1)
		{
			logerr("         Possible reason: %s\n",sys_errlist[saveerrno]);
		}
		logerr("         Queue Dump file ignored.\n",0);
		goto exit_qdump;
	}
	if (version!=DUMPVERSION)
	{
		logerr("Warning: Read Queue Dump file %s:\n",qdumpfile);
		sprintf(errmsg,"         This Queue Dump file was created by older version spooler.  Expected %d, read %d\n",
			DUMPVERSION,version);
		logerr(errmsg,0);
		logerr("         Queue Dump file ignored.\n",0);
		goto exit_qdump;
	}

	/* now loop and build a queue list */
	while (read(dump,(char*)&cur,sizeof(QITEM)))
	{
		if (access(cur.real_path,F_OK)!=0)	 /* don't bother adding if file doesn't exist anymore */
		{
			continue;
		}
		cur.printer=NULL;			 /* clear out some outdated info */
		cur.filters=NULL;
		cur.status=QST_HOLDING;
		cur.mode |= QMD_HOLD;
		cur.fitem=NULL;
		
		new=(QITEM*)gmem(1,sizeof(QITEM));	 /* make a new item */

		memcpy(new,&cur,sizeof(QITEM));		 /* copy the current to it */
		new->q_pos = q_depth();			 /* set the pos */
		if (strlen(new->form))
		  new->fitem = findfitem(new->form);	 /* init the form pointer if a form is specified */
		new->q_id  = ++queue_id;		 /* give a new id */
		ring_add(queuelist,-1,(char*)&new);	 /* and finally add to the list */
	}
	
      exit_qdump:
	
	close(dump);					 /* close the dump file */
}
/*
 * load the printer dump file.  Currently the only thing that is saved is enable/disable status
 *
 */
void load_pdump(void)
{
	int dump;
	PITEM cur, *pi, *findpitem(char *name);
	int4 magic, version;
	int st;
	
	if ((dump=open(pdumpfile,O_RDONLY))<0)
	{
		return;
	}
	if ((st=read(dump,(char*)&magic,sizeof(magic)))!=sizeof(magic))
	{
		int saveerrno;
		
		if (st == -1)
		{
			saveerrno = errno;
		}
		logerr("Warning: Attempting to read Printer Dump file %s:\n",pdumpfile);
		sprintf(errmsg,"         Wanted %d bytes [magic], read %d.\n",sizeof(magic),st== -1?0:st);
		logerr(errmsg,0);
		if (st == -1)
		{
			logerr("         Possible reason: %s\n",sys_errlist[saveerrno]);
		}
		logerr("         Printer Dump file ignored.\n",0);
		goto exit_pdump;
	}
	if (magic != PDUMPMAGIC)
	{
		logerr("Warning: Reading Printer Dump file %s:\n",pdumpfile);
		sprintf(errmsg,"         Invalid magic number.  Expected %08X, read %08x\n",
			PDUMPMAGIC,magic);
		logerr(errmsg,0);
		logerr("         Printer Dump file ignored.\n",0);
		goto exit_pdump;
	}
	if ((st=read(dump,(char*)&version,sizeof(version)))!=sizeof(version))
	{
		int saveerrno;
		
		if (st == -1)
		{
			saveerrno = errno;
		}
		logerr("Warning: Attempting to read Printer Dump file %s:\n",pdumpfile);
		sprintf(errmsg,"         Wanted %d bytes [version], read %d.\n",sizeof(version),st== -1?0:st);
		logerr(errmsg,0);
		if (st == -1)
		{
			logerr("         Possible reason: %s\n",sys_errlist[saveerrno]);
		}
		logerr("         Printer Dump file ignored.\n",0);
		goto exit_pdump;
	}

	if (version!=DUMPVERSION)
	{
		logerr("Warning: Read Printer Dump file %s:\n",qdumpfile);
		sprintf(errmsg,"         This Printer Dump file was created by older version spooler.  Expected %d, read %d\n",
			DUMPVERSION,version);
		logerr(errmsg,0);
		logerr("         Printer Dump file ignored.\n",0);
		goto exit_pdump;
	}


	while (read(dump,(char*)&cur,sizeof(PITEM)))
	{
		pi = findpitem(cur.printer_name);
		if (!pi) continue;
		if (!(cur.status & PST_ENABLED))
		  pi->status &= ~PST_ENABLED;
	}

      exit_pdump:
	close(dump);
}
/*
 * write a dummy qdump file for very old versions of ilpman just in case
 *
 */
void write_old_qdump_file(void)
{
	int dump;
	OLDQITEM dummy_qitem;
	extern char VERSION[];
	
	if ((dump=open(oldqdump,O_WRONLY|O_TRUNC|O_CREAT,0666))<0)
	{
		sprintf(errmsg,"Warning: Error opening old queue dump file %s: %s\n",
			oldqdump,sys_errlist[errno]);
		logerr(errmsg,0);
		
		return;
	}
	memset(&dummy_qitem,0,sizeof(dummy_qitem));
	dummy_qitem.q_pos = 1;
	strcpy(dummy_qitem.real_path,"Currently running version");
	write(dump,(char*)&dummy_qitem,sizeof(OLDQITEM));

	++dummy_qitem.q_pos;
	sprintf(dummy_qitem.real_path,"%s of the %s",VERSION,DAEMONNAME);
	write(dump,(char*)&dummy_qitem,sizeof(OLDQITEM));
	++dummy_qitem.q_pos;
        strcpy(dummy_qitem.real_path,"program.  This version");
	write(dump,(char*)&dummy_qitem,sizeof(OLDQITEM));

	++dummy_qitem.q_pos;
	sprintf(dummy_qitem.real_path,"of %s is incompatible.",MANNAME);
	write(dump,(char*)&dummy_qitem,sizeof(OLDQITEM));

	++dummy_qitem.q_pos;
	strcpy(dummy_qitem.real_path,"Make sure you are running");
	write(dump,(char*)&dummy_qitem,sizeof(OLDQITEM));

	++dummy_qitem.q_pos;
	sprintf(dummy_qitem.real_path,"version %s of %s.",VERSION,MANNAME);
	write(dump,(char*)&dummy_qitem,sizeof(OLDQITEM));

	close(dump);
	chmod(oldqdump,0666);
}

#ifdef DUMP_SHMEM

static int shm_init = -1;
static int write_qfirst = 1;
static int write_pfirst = 1;
static int q_shmids[QDFILECNT];
static int p_shmid;

static key_t q_shkeys[QDFILECNT];
static key_t p_shkey;

static char *q_shmptrs[QDFILECNT] = {0};
static char *p_shmptr = {0};
	
int idx, the_key;

/* 
 * do general init for shared memory stuff
 *
 */
int init_shmem(void)
{
	extern char *shmkeypaths[];            /* the qpaths generated list of shm key file paths */

	/* loop thru the files to generate each key */
	for (idx = 0 ; idx<QDFILECNT ; ++idx)
	{
		/* make a key from the current file */
		the_key = getipckey(shmkeypaths[idx],"shmem",IPCKEYMOD);
		if (the_key == (key_t)-1)
		{
			return IERR_SHMEM;     /* generate an error */
		}
		else
		  q_shkeys[idx]=the_key;       /* otherwise save the key in the key array */
	}
	the_key = getipckey(prtkeyfile,"shmem",IPCKEYMOD); /* get a key for the printer key file */
	if (the_key == -1)
	{
		return IERR_SHMEM;	       /* generate an error */
	}
	else
	  p_shkey=the_key;		       /* save the key */

	return IERR_OK;			       /* return ok status */
}
/* 
 * get a single shared mem segment
 *
 */
char *getshseg(int size, key_t key)
{
	int id;
	char *att;
	
	/* get the segment id */
	id = shmget(key,size,IPC_CREAT|0777);
	if (id<0)
	{
		sprintf(errmsg,"Error getting shared memory segment: %s\n",
			sys_errlist[errno]);
		logerr(errmsg,0);
		daemon_shutdown(1);
	}
	/* link it to our address space */
	att = shmat(id,0,0);
	if (att == (char*)-1)
	{
		sprintf(errmsg,"Error attaching shared memory segment: %s\n",
			sys_errlist[errno]);
		logerr(errmsg,0);
		daemon_shutdown(1);
	}
	return att;
}
/*
 * dump the queue data into shared memory
 *
 */

/* IMPROVE_ME: could optimize this function for occasions when qitems change only slightly, 
 * like when a job is printed and every item moves up in the list 1 position..
 */

void dump_q_shmem(void)
{
	struct shmmapheader *header;
	static int shmpseq = 0;
	char *shptr;
	int cnt, blocknum, seq;
	int finished = FALSE;
	extern int need_to_dump;
	static time_t last_shdump= (time_t) -1;
	time_t curtime, time(time_t *);
	int qpos,qcnt;
	QITEM *q;

	if (need_to_dump==FALSE && !write_qfirst ) 	/* need to dump anything? */
	{
		return;
	}
	curtime = time(&curtime);

	if (curtime - last_shdump < 2) 	/* don't want to dump too often */
	{
		return;
	}
	last_shdump = curtime;        /* remember the time of the current dump */
	write_qfirst=0;
	
	cnt = blocknum = 0;
	seq = shmpseq++;

	/* get count and first item */
	ring_count(queuelist,&qcnt);
	ring_get(queuelist,qpos = 0,(char*)&q);

#ifdef DBGSHDUMP	
	logerr("dumping queue shmem\n",0);
#endif
	while (!finished)
	{
		if (q_shmptrs[blocknum]==NULL)
		  q_shmptrs[blocknum] = getshseg(QSHMSZ, q_shkeys[blocknum]);      /* these are init'd on the fly 1st time around*/

		header = (struct shmmapheader *)q_shmptrs[blocknum]; /* setup a local pointer to the shmem area */
	
		header->magic   = SHMMAGIC;    /* insert the magic num */
		header->version = DUMPVERSION; /* version */
		header->type    = SHMTYP_Q;    /* and type  */
		header->sequence = seq;	       /* sequence number */
	
		/* loop thru and fill current area */
		for (cnt = 0,  shptr = q_shmptrs[blocknum] + sizeof(struct shmmapheader); 
		     qpos<qcnt && cnt < 256;
		     ++cnt, ring_get(queuelist,++qpos,(char*)&q), shptr += sizeof(QITEM)
		     )
		{
			memcpy(shptr,(char*)q,sizeof(QITEM));
/*			printf("%x: %s\n",shptr,q->real_path);*/
		}
		header->item_cnt = cnt;	       /* add count value */
		if (qpos>=qcnt)		       /* more to dump? */
		  ++finished;
		++blocknum;		       /* if so, bump the count */
	}
	need_to_dump=FALSE;
}
/*
 *  dump printer info
 *
 * works just like dump_q_shmem()
 *
 */
void dump_p_shmem(void)
{
	struct shmmapheader *header;
	char *shptr;
	static int shmpseq = 0;
	int cnt;
	extern int need_to_dump_p;
	static time_t last_shdump= (time_t) -1;
	time_t curtime, time(time_t *);
	
	int ppos,pcnt;
	PITEM *p;

	if (need_to_dump_p==FALSE && !write_pfirst)
	{
		return;
	}
	if (p_shmptr==NULL)
	  p_shmptr = getshseg(PRTSHMSZ, p_shkey);

	write_pfirst = 0;
	curtime = time(&curtime);

	if (curtime - last_shdump <2)
	{
		return;
	}
	last_shdump = curtime;

#ifdef DBGSHDUMP
	logerr("dumping printer shmem\n",0);
#endif

	header = (struct shmmapheader *)p_shmptr;
	
	header->magic   = SHMMAGIC;
	header->version = DUMPVERSION;
	header->type    = SHMTYP_P;
	header->sequence = shmpseq++;
/*	printf("queue seq: %d\n",shmpseq-1);*/

	ring_count(printerlist,&pcnt);
	ring_get(printerlist,0,(char*)&p);
	
	for (cnt = 0,        ppos=0, shptr = p_shmptr + sizeof(struct shmmapheader); 
	     ppos<pcnt; 
	       ++cnt, ring_get(printerlist,++ppos,(char*)&p), shptr += sizeof(PITEM)
	     )
	{
		memcpy(shptr,(char*)p,sizeof(PITEM));
/*		printf("%x: %s\n",shptr,p->printer_name);*/
	}
	header->item_cnt = cnt;
	need_to_dump_p=FALSE;
}
/*
 * go thru and remove all shared mem resources 
 *
 */
void shut_shmem(void)
{
	int idx,id;
	
	for (idx=0; idx<QDFILECNT; ++idx)
	{
		id=shmget(q_shkeys[idx],0,0);
		shmctl(id,IPC_RMID,0);
	}
	id=shmget(p_shkey,0,0);
	shmctl(id,IPC_RMID,0);
}

#else
#endif

/*
 * $Log: d_dump.c,v $
 * Revision 1.7  1993/09/30  23:27:14  jockc
 * *** empty log message ***
 *
 * Revision 1.6  1993/09/30  23:19:39  jockc
 * adsf
 *
 * Revision 1.5  1993/09/30  23:11:13  jockc
 * fixed prognames
 *
 * Revision 1.4  1993/08/13  20:17:30  jockc
 * alpha/c89 and began adding comments
 *
 * Revision 1.3  1993/05/28  21:47:50  jockc
 * changes for new list logic
 *
 * Revision 1.2  1993/03/26  00:17:12  jockc
 * block sh dump to every 2 seconds or so
 *
 * Revision 1.1  1993/01/12  02:13:47  jockc
 * Initial revision
 *
 *
 */
