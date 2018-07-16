/*
 * Module:  daemon_main
 * Program: IDSIprint/UNIQUE
 * Purpose: main code for scheduler
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

char VERSION[5];
static char MODDATE[20];

/*#define FG */            /* for debugging, sometimes we like to run the daemon in foreground. */
                             /* #define FG to run in foreground */
#define _DAEMON_MAIN_

char logbuf[1024];

#include <fcntl.h>
#include "daemon.h"

#ifdef OSF1_ALPHA
#include <sys/sysinfo.h>
#include <sys/proc.h>
#endif

extern char *sys_errlist[];


extern int ring_add (ring_struct *ring_ptr, int position, char *element);
extern int ring_find (ring_struct *ring_ptr, char *match, int *position, char *element);
extern int ring_remove (ring_struct *ring_ptr, int position, char *element);
extern int tag_unsp_obsolete (struct p_item *printer);

int main(int argc, char **argv)
{
#ifdef BSDWAIT
	union wait waitstat;
#endif
	time_t filetime(char *path);
	char *debugval,*getenv(CONST char *);
	int status, once=1;

#ifdef OSF1_ALPHA	
	int buf[2];

	buf[0]=SSIN_UACPROC;
	buf[1]=UAC_SIGBUS|UAC_NOPRINT;
	
	setsysinfo(SSI_NVPAIRS,buf,1,0, 0);
#endif
	
	make_vers_date(rcsid,VERSION,MODDATE);

	if (argv[1]!=NULL && !strcmp(argv[1],"-dinf"))
	{
		char **p;
		for (p=dbgdesc;*p; ++p)
		  fprintf(stderr,"%s\n",*p);
		exit(0);
	}

#ifdef NICEPS
# ifndef FG
	if (argv[1]==NULL || strcmp(argv[1],FAKEARG))
	{
		execlp(argv[0],DAEMONNAME,FAKEARG,0);
	}
# endif
	progname_ptr = argv[1];
	if (progname_ptr)
	{
		memset(progname_ptr,' ',FAKEARGLEN);
		sprintf(progname_ptr,DMAINNAME,VERSION);
	}
#endif
	if (setuid(0)== -1)
	{
		fprintf(stderr,"%s: can't setuid root: %s (errno=%d)\n",argv[0],
			sys_errlist[errno],errno);
		exit(1);
	}
	if (initpaths() == -1)
	{
		fprintf(stderr,"%s: Can't access spool/work dir: %s\n",argv[0],spooldir);
		exit(1);
	}
	if (debugval=getenv(UDEBUG))
	{
		debug_flag = hextoi4(debugval);
		fprintf(stderr,"debug=%x\n",debug_flag);
	}
	else
	{
		debug_flag = 0;
	}

#ifdef FG
	do
	{
		status = setflag(SEMRUNNING,FLAG_SET);
		switch(status)
		{
		      case IERR_RUNNING:
			fprintf(stderr,"IDSI scheduler already running\n");
			exit(0);
			break;
		      case IERR_SEMAPHORE:
			fprintf(stderr,"Error creating semaphore:\nCheck error log or call Customer support\n");
			exit(0);
			break;
		      case IERR_NODIR:
			if (access(dumpdir,0)!=0)
			{
				mkdir(dumpdir,0777);
				chmod(dumpdir,0777);
			}		
			--once;
			break;
		      default:
			break;
		}

	} while (status != IERR_OK && once >= 0);

# ifdef DUMP_SHMEM
	status = init_shmem();
	if (status==IERR_SHMEM)
	{
		fprintf(stderr,"Error creating shared memory area:\nCheck error log or call Customer support\n");
		exit(0);
	}
# endif /*DUMP_SHMEM*/
#endif /*FG*/
#ifndef FG
	do
	{
		status = setflag(SEMRUNNING,FLAG_SET);
		switch(status)
		{
		      case IERR_RUNNING:
			fprintf(stderr,"IDSI scheduler already running\n");
			exit(0);
			break;
		      case IERR_SEMAPHORE:
			fprintf(stderr,"Error creating semaphore:\nCheck error log file [%s]\nor call Customer support.\n",
				errlogfile);
			exit(0);
			break;
		      case IERR_NODIR:
			if (access(dumpdir,0)!=0)
			{
				mkdir(dumpdir,0777);
				chmod(dumpdir,0777);
			}		
			--once;
			break;
		      default:
			break;
		}

	} while (status != IERR_OK && once >= 0);
	if (status != IERR_OK && once == -1)
	{
		fprintf(stderr,"Error creating work directories.\nCheck error log file [%s]\nor call Customer support.\n",
			errlogfile);
		exit(0);
	}
#endif
#ifdef SOCKCODE
	unlink(socketpath);
#endif
#if 0
	q_head=q_ptr=q_tail=ready_head=r_ptr=NULL;
	p_head=p_ptr=NULL;
	u_head=u_ptr=NULL;
#endif
	f_head=f_ptr=NULL;

	if (access(pfiledir,0)!=0)
	{
		mkdir(pfiledir,0777);
		chmod(pfiledir,0777);
	}
	if (access(initdir,0)!=0)
	{
		mkdir(initdir,0777);
		chmod(initdir,0777);
	}
	if (access(pstatusd,0)!=0)
	{
		mkdir(pstatusd,0777);
		chmod(pstatusd,0777);
	}
	if (access(dumpdir,0)!=0)
	{
		mkdir(dumpdir,0777);
		chmod(dumpdir,0777);
	}		
	if (access(remdir,0)!=0)
	{
		mkdir(remdir,0777);
		chmod(remdir,0777);
	}		
	ring_open(&unsplist,sizeof(ULIST*),16,1,0,0);
	ring_open(&readylist,sizeof(QITEM*),256,32,findqueueitem,0);
	ring_open(&queuelist,sizeof(QITEM*),256,32,findqueueitem,0);
	ring_open(&printerlist,sizeof(PITEM*),32,8,findprinter,0);

	formdef_time=filetime(formdef);
	loadformdef();
	classdef_time=filetime(classdef);
	loadclassdef();
	ilpdef_time=filetime(ilpdef);
	loadqconfig();
	need_to_dump_cfg=TRUE;
	dump_conf();

	write_old_qdump_file();
	printcap_time=filetime(prtdef);
	if (!access(pdumpfile,0)) load_pdump(); /* check enable/disable status */
	loadpcap();			

	if (fileerrs)
	{
		fprintf(stderr,"\nERRORS found in the following files:\n");
		if (fileerrs & PRTDEFERR)
		{
			fprintf(stderr,"   %s\n",prtdef);
		}
		if (fileerrs & FORMDEFERR)
		{
			fprintf(stderr,"   %s\n",formdef);
		}
		if (fileerrs & CLDEFERR)
		{
			fprintf(stderr,"   %s\n",classdef);
		}
		if (fileerrs & CONFERR)
		{
			fprintf(stderr,"   %s\n",ilpdef);
		}
		fprintf(stderr,"\nPlease correct the errors and run %s again.\n",argv[0]);
		logerr("%s is exiting due to configuration errors.\n",argv[0]);
		exit(1);
	}
	daemon();

	queue_modified();
	pstatus_changed();
	
	dump_stuff();

	signal(SIGUSR2,daemon_shutdown);
#ifdef FG
	signal(SIGINT,daemon_shutdown);
#endif	
#ifdef SYSVWAIT
	signal(SIGCLD, SIG_IGN);
#endif
#ifndef FG
	if (!getenv(IDEBUG) && !(debug_flag & DBGSIGCORE))
	{
		signal( SIGILL,  exitbug );
		signal( SIGEMT,  exitbug );
		signal( SIGBUS,  exitbug );
		signal( SIGSEGV, exitbug );
	}
#endif	
	init_comm();
	setflag(SEMWAIT,FLAG_CLEAR);
	while (1)									/* endless loop */
	{
		int rqpending=0;
		
		unspool();								/* unspool if necessary */

		do
		{
			if (receive_rq(&rqpending)>0)					/* check for RQs from clients */
			{
				service_rq();	
			}
		} while (rqpending>0);
#ifdef BSDWAIT
		wait3(&waitstat,WNOHANG,(struct rusage *)0);
#endif
		reorder_q();								/* adjust the queue if necessary */
		dump_stuff();								/* write queue and prt files */
		check_unspoolers();
		check_config_files();
		validate_q(); /* scan for deleted entries */
		validate_printers(); /* check out error'd printers */
		sleep(1);
	}										/* clients */
}
void service_rq(void)
{
	if (packetrq->magic != PACKETMAGIC)
	{
		logerr("WARNING: received packet that appears to be either corrupted\n",0);
		logerr("or from an earlier (incompatible) version of ilp or ilpman.\n",0);
		return;
	}
	if (packetrq->version != PACKETVERSION)
	{
		logerr("WARNING:received packet with an incompatible version number.\n",0);
		sprintf(errmsg,"Expected %d, received %d.  Verify ilp or ilpman version.\n",PACKETVERSION,packetrq->version);
		logerr(errmsg,0);
		return;
	}
	switch (packetrq->functype)
	{
	      case M_CHANGE:     
		change();
		break;
		
	      case M_QUEUE:      
		queue();
		break;

	      default:
		exception(BADTYPE);
		break;
	}
}
void queue(void)
{
	QITEM *newitem;
	char *gmem(int cnt, int sz);
	PITEM *findpitem(char *name);
	FITEM *findfitem(char *name);

	newitem=(QITEM*)gmem(1,sizeof(QITEM));
	ring_add(queuelist,-1,(char*)&newitem);

	memcpy(newitem,&(packetrq->data),sizeof(QITEM));
	newitem->q_pos = q_depth();
	newitem->q_id  = ++queue_id;
	if (queue_id == 9999) queue_id=0;
	
	newitem->owner_uid=packetrq->sender_uid;
	newitem->owner_pid=packetrq->sender_pid;
	
	if (strlen(newitem->requested_prtr))
	{
		strcpy(newitem->actual_prtr,newitem->requested_prtr);
		newitem->printer = findpitem(newitem->requested_prtr);
	}
	else
	{
		if (newitem->class == 0 && newitem->prtnum == 0)
		{
			strcpy(newitem->requested_prtr,qconfig.default_printer);
			newitem->printer = findpitem(newitem->requested_prtr);
		}
	}
	if (newitem->copies==0)
	{
		newitem->copies=1;
	}
	newitem->fitem = findfitem(newitem->form);
	newitem->class = newitem->class?toupper(newitem->class):' ';

	get_class_chars(newitem);
	
	if (newitem->mode & QMD_HOLD)
	{
		newitem->status |= QST_HOLDING;
	}
	queue_modified();

#ifdef DEBUGCODE
	D(DBGMAINPACK)
	{
		sprintf(logbuf,"queued file %s dest %s\n",newitem->real_path,newitem->requested_prtr);
		logerr(logbuf,0);
	}
#endif
}
void removeqitem(struct q_item *q)
{
	int pos,qid,st;
	QITEM *theitem;
	
	qid=q->q_id;

#ifdef DEBUGCODE
	D(DBGREMOVEQ)
	{
		sprintf(errmsg,"Removing qid %d, path %s\n",q->q_id,q->real_path);
		logerr(errmsg,0);
	}
#endif
	
	removereadyqitem(q);
	st=ring_find(queuelist,(char*)&q,&pos,NULL);
	if (st!=0)
	  return;

#ifdef DEBUGCODE
	D(DBGREMOVEQ)
	{
		logerr("   qpos is %d\n",pos);
	}
#endif
	
	st = ring_remove(queuelist,pos,(char*)&theitem);

#ifdef DEBUGCODE
	D(DBGREMOVEQ)
	{
		logerr("   remove status was %d\n",st);
	}
#endif

	free(theitem);
}
void removereadyqitem(struct q_item *q)
{
	int pos;
	QITEM *theitem;
	int st;
	
	st = ring_find(readylist,(char*)&q,&pos,(char*)&theitem);
	if (st!=0)
	{
		return;
	}
#ifdef DEBUGCODE
	D(DBGREMOVEQ)
	{
		logerr("   ready list qpos is %d\n",pos);
	}
#endif
	if (ISREADY(theitem))
	{
		st=ring_remove(readylist,pos,NULL);
#ifdef DEBUGCODE
		D(DBGREMOVEQ)
		{
			logerr("   ready remove status was %d\n",st);
		}
#endif
	}
	theitem->status &= ~QST_READY;
}
void queue_modified(void)
{
	need_to_dump=TRUE;
}
void pstatus_changed(void)
{
	need_to_dump_p=TRUE;
}
q_depth(void)										/* return size of queue */
{
	int queue_depth;

	ring_count(queuelist,&queue_depth);
	return queue_depth;
}

void change(void)
{
	PITEM *p, *findpitem(char *name);
	QITEM *q, *findqitem(int id), *findritem(int id);
	FITEM *findfitem(char *name);
	UMSG msg;
	void sigpipe(int dummy);
	void truncspc();
	extern int broken_upipe;
#ifdef SIGPIPE
	signal(SIGPIPE,sigpipe);
	broken_upipe=0;
#endif			

#ifdef DEBUGCODE
	D(DBGMAINPACK)
	{
		sprintf(logbuf,"RCVD CHANGE: user %d, AUX:%s\n",packetrq->sender_uid,auxstr[packetrq->aux]);
		logerr(logbuf,0);
	}
#endif
	
	switch (packetrq->aux)
	{
	      case C_PRTR:
		p=findpitem(packetrq->data.p.printer_name);
		if (!p) return;

#ifdef DEBUGCODE
		D(DBGMAINPACK)
		{
			sprintf(logbuf,"RCVD user %d, RQ PRTR %s:\n",packetrq->sender_uid,packetrq->data.p.printer_name);
			logerr(logbuf,0);
			sprintf(logbuf,"class [%s]==>[%s]   form [%s]==>[%s]\n",
				p->class,packetrq->data.p.class,
				p->current_form,packetrq->data.p.current_form);
			logerr(logbuf,0);
		}
#endif
		
		allcaps(packetrq->data.p.class);
		strcpy(p->class,packetrq->data.p.class);
		strcpy(p->current_form,packetrq->data.p.current_form);
		
		if (WRONGFORM(p))
		{
			if (!strcmp(p->current_form,p->unspooler->qitem->form))
			{
				int st;
			
				msg.status = UMSG_FORMCHANGED;
				st=write(p->unspooler->wfd,(char*)&msg,sizeof(UMSG));
				
#ifdef DEBUGCODE
				D(DBGMAINPACK)
				{
					sprintf(logbuf,"             wrote %d bytes to unspooler: %s\n",st,p->printer_name);
					logerr(logbuf,0);
				}
#endif
				
				if (broken_upipe) { finished_unspooler(p->unspooler); broken_upipe=FALSE; break; }
			}
		}

		strcpy(p->printer_dev,packetrq->data.p.printer_dev);
		p->time_stamp = packetrq->data.p.time_stamp;
		if (p->unspooler)
		{
			tag_unsp_obsolete(p);
		}
		pstatus_changed();
		break;

	      case C_ENABLE:
	      case C_DISABLE:
		p = findpitem(packetrq->data.p.printer_name);
		if (!p) return;

#ifdef DEBUGCODE
		D(DBGMAINPACK)
		{
			sprintf(logbuf,"RCVD user %d, RQ PRTR %s: %s\n",packetrq->sender_uid,packetrq->data.p.printer_name,
				packetrq->aux==C_ENABLE?"Enable":"Disable");
			logerr(logbuf,0);
		}
#endif
		
		/*if (ISBUSY(p)) return;*/
		if (packetrq->aux==C_ENABLE)
		  p->status |= PST_ENABLED;
		else
		  p->status &= ~PST_ENABLED;
		p->time_stamp = packetrq->data.p.time_stamp;
		if (p->unspooler)
		{
			tag_unsp_obsolete(p);
		}
		pstatus_changed();
		break;
			       
	      case C_ALIGN:
		p=findpitem(packetrq->data.p.printer_name);
		if (!p) return;
		if (WRONGFORM(p))
		{
			msg.status = UMSG_ALIGN;
			write(p->unspooler->wfd,(char*)&msg,sizeof(UMSG));
			if (broken_upipe) { finished_unspooler(p->unspooler); broken_upipe=FALSE; break; }
		}
		else
		{
			; /* do an align the hard way */
		}
		break;
		
	      case C_FORM:
		p=findpitem(packetrq->data.p.printer_name);
		if (!p) return;

		if (!WRONGFORM(p))
		{
			if (haspriv(packetrq->sender_uid))
			    strncpy(p->current_form,packetrq->data.p.current_form,FORMNAMEMAX-1);

		}
		else
		{
			int st;
			
			msg.status = UMSG_FORMCHANGED;
			st=write(p->unspooler->wfd,(char*)&msg,sizeof(UMSG));
		       
#ifdef DEBUGCODE
			D(DBGMAINPACK)
			{
				sprintf(logbuf,"             wrote %d bytes to unspooler: %s\n",st,p->printer_name);
				logerr(logbuf,0);
			}
#endif

			if (broken_upipe) { finished_unspooler(p->unspooler); broken_upipe=FALSE; break; }

		}
		p->time_stamp = packetrq->data.p.time_stamp;
		break;
		
	      case C_QITEM:
		q=findqitem(packetrq->data.q.q_id);

#ifdef DEBUGCODE
		D(DBGMAINPACK)
		{
			sprintf(logbuf,"RCVD user %d, RQ QITEM %d:\n",packetrq->sender_uid,packetrq->data.q.q_id);
			logerr(logbuf,0);
		}
#endif
	
		if (!q || !haspriv(packetrq->sender_uid)) return;
		if (ISUNSPOOLED(q)) return;

		strcpy(q->form,packetrq->data.q.form);
		q->fitem = findfitem(q->form);

		strcpy(q->requested_prtr,packetrq->data.q.requested_prtr);
		strcpy(q->actual_prtr,packetrq->data.q.requested_prtr);
		q->mode=packetrq->data.q.mode;
		q->copies=packetrq->data.q.copies;
		if (q->class != toupper(packetrq->data.q.class))
		{
			q->class=toupper(packetrq->data.q.class);
			get_class_chars(q);
		}
		
		q->stpage=packetrq->data.q.stpage;
		q->endpage=packetrq->data.q.endpage;
		q->prtnum = packetrq->data.q.prtnum;
		
		if (packetrq->data.q.q_pos != -1)
		{
			if (ISREADY(q))
			{
				QITEM *r;
				r=findritem(packetrq->data.q.q_id);
				move_item(readylist,r,packetrq->data.q.q_pos);
			}
			move_item(queuelist,q,packetrq->data.q.q_pos);
			reorder_q();
		}
		q->time_stamp = packetrq->data.q.time_stamp;
		queue_modified();
		
		break;
	      case C_DELETE:
	      case C_REMOVE:
		q=findqitem(packetrq->data.q.q_id);
		if (!q || !haspriv(packetrq->sender_uid)) return;
		if (ISUNSPOOLED(q))
		{
			msg.status = UMSG_ABORT;
			write(q->printer->unspooler->wfd,(char*)&msg,sizeof(UMSG));
			if (broken_upipe) { finished_unspooler(q->printer->unspooler); broken_upipe=FALSE; break; }
			q->status &= ~QST_UNSPOOLED;
			/* q->printer->status &= ~PST_BUSY; */
			pstatus_changed();
		}
		if (packetrq->aux == C_DELETE)
		{
			/* FIXME -- check for delete perm should be handled here, not in the man_misc code */
			/*          but for now it's easier to handle it there */
			if (unlink(q->real_path)<0)
			{
				int save_errno=errno;
				logerr("RCVD DELETE, calling unlink(%s)\n",q->real_path);
				logerr("             unlink returned %d\n",save_errno);
			}
		}
		removeqitem(q);
		queue_modified();
		break;
	      case C_UNHOLD:
		q=findqitem(packetrq->data.q.q_id);
		if (!q || !haspriv(packetrq->sender_uid)) return;
		q->mode &= ~QMD_HOLD;
		q->status &= ~QST_HOLDING;
		q->time_stamp = packetrq->data.q.time_stamp;
		queue_modified();
		break;
	      case C_HOLD:
		q=findqitem(packetrq->data.q.q_id);
		if (!q || !haspriv(packetrq->sender_uid)) return;
		q->mode |= QMD_HOLD;
		q->status |= QST_HOLDING;
		if (ISUNSPOOLED(q) && !ISQSTOPPED(q))
		{
			msg.status = UMSG_ABORT;
			write(q->printer->unspooler->wfd,(char*)&msg,sizeof(UMSG));
			if (broken_upipe) { finished_unspooler(q->printer->unspooler); broken_upipe=FALSE; break; }
			
			q->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT);
			/* q->printer->status &= ~(PST_FORMCHANGE|PST_BUSY); */
			q->printer = NULL;
			strcpy(q->actual_prtr,q->requested_prtr);
			pstatus_changed();
		}
		if (ISQSTOPPED(q))
		{
			msg.status = UMSG_HOLD;
			write(q->printer->unspooler->wfd,(char*)&msg,sizeof(UMSG));
			if (broken_upipe) { finished_unspooler(q->printer->unspooler); broken_upipe=FALSE; break; }
			q->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
			q->printer = NULL;
			pstatus_changed();
		}
		removereadyqitem(q);
		queue_modified();
		q->time_stamp = packetrq->data.q.time_stamp;
		break;
	      case C_STOPQ:
	      case C_STARTQ:
		q=findqitem(packetrq->data.q.q_id);
		if (!q || !haspriv(packetrq->sender_uid)) return;
		if (packetrq->aux == C_STOPQ)
		{
			int st;
			
			msg.status = UMSG_STOP;
			st=write(q->printer->unspooler->wfd,(char*)&msg,sizeof(UMSG));
			D(DBGMAINPACK)
			{
				logerr("STOP: wrote %d bytes \n",st);
			}
			if (broken_upipe) { finished_unspooler(q->printer->unspooler); broken_upipe=FALSE; break; }
			q->status |= QST_STOPPED;
			if (q->printer) q->printer->status |= PST_STOPPED;
		}
		else
		{
			msg.status = UMSG_START;
			write(q->printer->unspooler->wfd,(char*)&msg,sizeof(UMSG));
			if (broken_upipe) { finished_unspooler(q->printer->unspooler); broken_upipe=FALSE; break; }
			q->status &= ~QST_STOPPED;
			if (q->printer) q->printer->status &= ~PST_STOPPED;
		}
		queue_modified();
		pstatus_changed();
		q->time_stamp = packetrq->data.q.time_stamp;
		break;
	      case C_CLRERR:
		p=findpitem(packetrq->data.p.printer_name);
		if (!p) return;
		else
		{
			p->status &= ~PST_ERROR;
			pstatus_changed();
		}
		p->time_stamp = packetrq->data.p.time_stamp;
		break;
	}
}
haspriv(int id)			/* check for privleges (for form change, queue change, etc) */
       
{
	return TRUE;
}
void exception(int type)
{
	;
}
void exitbug(int sig)
{
	logerr("Terminating on signal %d.\n",sig);
	logerr("Please contact Customer Support.\n",0);
	daemon_shutdown(1);
}

void daemon_shutdown(int status)
{
	extern FILE *errlogFILE;
	
	need_to_dump=need_to_dump_p = TRUE;
	
	dump_p();
	dump_q();
	
	shut_comm();
	shut_shmem();
	logerr("Master process shutting down.\n",0);
	if (errlogFILE) fclose(errlogFILE);

	kill(0,SIGHUP);
	exit(status);
}
void daemon(void)										/* put us in daemon mode */
{
	int lockfd,lockinfo;
	int pid;
	int dp,status;
	char tmp[100],*gmem(int cnt, int sz);

#ifdef OSF1_ALPHA
	int buf[2];
#endif

#ifndef FG
	if (pid=fork())									/* fork a background process */
	{
		dp=open(daemonpid,O_WRONLY|O_CREAT,0666);
		chmod(daemonpid,0666);
		sprintf(tmp,"%d\n",pid);
		write(dp,tmp,strlen(tmp));
		close(dp);
		fprintf(stderr,"UniQue Background Daemon Release V%s (c)IDSI %s [pid=%d]\n",VERSION,MODDATE,pid);
		exit(0);
	}

#ifdef OSF1_ALPHA
	buf[0]=SSIN_UACPROC;
	buf[1]=UAC_SIGBUS|UAC_NOPRINT;
	
	setsysinfo(SSI_NVPAIRS,buf,1,0, 0);
#endif

# if defined(SIGTSTP) && !defined(NCR486) && !defined(NCR32)
#  ifdef ULTRIX
	setpgrp(0,getpid());
#   else
	setpgrp();
#  endif /*ULTRIX*/
	if ((dp=open("/dev/tty",O_RDWR)) >=0)
	{
#  if !defined(SCO) && !defined(HPUX) && !defined(MOTOROLA) && !defined(SEQUENT)
		ioctl(dp,TIOCNOTTY,NULL);
#  endif
		close(dp);
	}
# else
	setpgrp();
	signal(SIGHUP,SIG_IGN);
# endif /*defined(SIGTSTP) && !defined(NCR486) && !defined(NCR32)*/

#if defined(i386) || defined(HPUX) || defined(u3b2) || defined(ICL) || defined(MOTOROLA) || defined(SOLARIS)
	nice(qconfig.priority);
#else
	setpriority(PRIO_PROCESS,getpid(),qconfig.priority);
#endif

	status = setflag(SEMRUNNING,FLAG_SET);
	switch(status)
	{
	      case IERR_RUNNING:
		fprintf(stderr,"IDSI scheduler already running\n");
		exit(0);
		break;
	      case IERR_SEMAPHORE:
		fprintf(stderr,"Error creating semaphore:\nCheck error log or call Customer support\n");
		exit(0);
		break;
	      default:
		break;
	}
#ifdef DUMP_SHMEM
	status = init_shmem();
	if (status==IERR_SHMEM)
	{
		fprintf(stderr,"Error creating semaphore:\nCheck error log or call Customer support\n");
		exit(0);
	}
#endif
	close(0);	
	close(1);

	open("/dev/null",O_RDONLY);
	open("/dev/null",O_WRONLY);

#else
	fprintf(stderr,"IDSI Print Queue Daemon Release V%s (c)IDSI %s %s\n",VERSION,MODDATE
# ifdef SOCKCODE
			,"<BSD IPC>"
# endif
# ifdef MQCODE
			,"<SYSV IPC>"
# endif
		);
#endif /*FG*/
	if (access(spooldir,0))							/* make sure spooldir is there */
	{
		mkdir(spooldir,0777);
		chmod(spooldir,0777);
	}
#ifdef OLD
	lockfd=open(lockfilep,O_WRONLY|O_CREAT|O_TRUNC,0666);
	lockinfo=0;
	write(lockfd,(char*)&lockinfo,sizeof(lockinfo));
	close(lockfd);
	chmod(lockfilep,0666);
#endif
	unlink(lockfilep);
	unlink(oldlockfile);
	unlink(oldlocktmp);

	if (packetrq == NULL)							
	{
		packetrq = (PACKET *)gmem(1,PACKET_SIZE);
	}
	
	ring_open(&clients,sizeof(int),32,1,findcli,0);

	queue_id=0;
	unspooling=FALSE;
	
	if (!access(qdumpfile,0)) load_qdump();
	queue_modified(); dump_q();
}
PITEM *findpitem(char *name)									/* given name, return pitem for a prtr */
           
{
	PITEM *ret, *pp, pi;
	int pos;
	int st;
	
	pp = &pi;
	strncpy(pi.printer_name,name,PRTNAMEMAX);
	st=ring_find(printerlist,(char*)&pp,&pos,(char*)&ret);
	if (st==1)
	{
		ret=NULL;
	}
	return ret ;
}
#if 0
struct p_item *findrdyprinter(char class)						/* given class, return pitem for a prtr */
{
	PITEM *item,*ret;
	char *strchr(CONST char *, int);
	int ppos,pcnt;
	
	ring_count(printerlist,&pcnt);
	for (ppos=0; ppos<pcnt; ++ppos)
	{
		ring_get(printerlist,ppos,(char*)&item);
		if (strchr(item->class,class) 					/* class must match, X class matches all?*/
		    && ISIDLE(item) 
		    && ISENABLED(item)
		    && !ISERROR(item))
		{
			ret=item;
			break;
		}
	}
	return ret;
}
#endif
struct q_item *findrdyqitem(char class,char *name,int num) 
{
	QITEM *curitem,*retitem;
	PITEM *findpitem(char *name);

	int rpos, rcnt, retpos;
	ring_count(readylist,&rcnt);
	
#ifdef DEBUGCODE
	D(DBGFINDRDYQ)
	{
		sprintf(errmsg,"findrdyqitem: checking for [%c] [%s] [%d]\n",
			class,name,num);
		logerr(errmsg,0);
	}
#endif
	for (rpos=0, retitem=NULL; rpos<rcnt; ++rpos)
	{
		ring_get(readylist,rpos,(char*)&curitem);

#ifdef DEBUGCODE
		D(DBGFINDRDYQ)
		{
			sprintf(errmsg,"     file[%s] qid[%d]  class[%c]  rq_prtr[%s]\n",
				curitem->real_path, curitem->q_id, curitem->class, curitem->requested_prtr);
			logerr(errmsg,0);
		}
#endif
		if (!strcmp(curitem->requested_prtr,name))  /* job's rq'd printer takes priority */
		{
			retitem=curitem;
			retitem->printer = findpitem(name);
			retpos = rpos;
			break;
		}
		if (curitem->prtnum > 0) /* then his rq'd pnum */
		{
			if (curitem->prtnum == num) 
			{ 
				retitem=curitem; 
				retpos = rpos;
				break; 
			}
			else
			{
				continue;
			}
		}
		if (!strlen(curitem->requested_prtr) || curitem->requested_prtr[0] == ' ')  /* none rq'd, so look at class */
		{
			if (curitem->class == class)  
			{
				retitem=curitem; 
				retpos = rpos;
				break; 
			}
		}
	}
	if (!retitem)  /* no jobs found, so take the first job with a space class (if any) */
	{
		for (rpos=0; rpos<rcnt; ++rpos)
		{
			ring_get(readylist,rpos,(char*)&curitem);
			if (curitem->class == ' '
			    && strlen(curitem->requested_prtr)==0
			    && curitem->prtnum == 0)  		  
			{
				retitem=curitem; 
				retpos = rpos;
				break; 
			}  
		}
	}
	if (!retitem) /* still none */
	  return NULL;

	ring_remove(readylist,rpos,NULL);
	retitem->status &= ~QST_READY;
	return retitem;
}
void addprtr(struct ipcapstruct *pcap)									/* add printer to active printer list */
                         
{
	char *gmem(int cnt, int sz);
	PITEM *pptr;
	static int printer_id = 1;
	
	pptr = (PITEM*)gmem(1,sizeof(PITEM));
	ring_add(printerlist,-1,(char*)&pptr);
	
	pptr->printer_id = printer_id++;
	pptr->baud = pcap->baud;
	pptr->databits = pcap->databits;
	pptr->stopbits = pcap->stopbits;
	pptr->parity = pcap->parity;
	pptr->prtnum = pcap->prtnum;
	strcpy(pptr->printer_name,pcap->printer);
	strcpy(pptr->printer_dev,pcap->device);
	strcpy(pptr->default_form,pcap->form);
	strcpy(pptr->current_form,pcap->form);
	strcpy(pptr->printer_model,pcap->model);
	strcpy(pptr->class,pcap->class);
	pptr->status = PST_ENABLED;
	if (pcap->sizepri) pptr->mode = SIZEPRI;

	if (strlen(pcap->type)==0) pptr->mode |= PMD_DEVICE;
	else
	{
		if (strcmp(pcap->type,PRT_TYPE_DEVICE)==0) 
		{
			pptr->mode |= PMD_DEVICE;
		}
		else 
		{
			pptr->mode &= ~PMD_DEVICE;
		}
		if (strcmp(pcap->type,PRT_TYPE_PROGRAM)==0)
		{
			pptr->mode |= PMD_PROGRAM;
			pptr->mode |= ((ipcap.inputtype==PMD_PROGFILE) ? ipcap.inputtype : PMD_PROGSTDIN);	
		}
		else
		{
			pptr->mode &= ~PMD_PROGRAM;	
		}

	}

}
void reorder_q(void)                                                                             /* arrange queue, build ready queue */
{
	int realpos=1;
	int qpos, qcnt;
	int rpos, rcnt;
	QITEM *curitem;
	
	ring_count(queuelist,&qcnt);
	ring_count(readylist,&rcnt);
	
#ifdef DBGREORDER
	sprintf(errmsg,"Enter reorder: queue size %d, ready list size %d\n",
		qcnt,rcnt);
	logerr(errmsg,0);
#endif
	if(qcnt==0)
	{
		return;
	}
	for (qpos = 0; qpos<qcnt; ++qpos)
	{
		ring_get(queuelist,qpos,(char*)&curitem);
		
#ifdef DBGREORDER
		sprintf(errmsg,"reorder: %s job: %d %s %s printer: %s %s",curitem->orig_path,curitem->q_id,
		       ISHOLDING(curitem)?"Hold":"Wait",
		       ISREADY(curitem)?"Ready":"Not ready",
		       curitem->printer?curitem->printer:"No printer",
		       curitem->printer?(ISIDLE(curitem->printer)?"Idle":"Busy"):"No printer");
		logerr(errmsg,0);
#endif
		
		if (ISUNSPOOLED(curitem)||ISHOLDING(curitem)||ISREADY(curitem)) 
		  continue;								/* item held or already ready */

		addready(curitem);
	}

	for (qpos = 0; qpos<qcnt; ++qpos)
	{
		ring_get(queuelist,qpos,(char*)&curitem);
		if(ISUNSPOOLED(curitem)) 
		{
			curitem->q_pos = realpos++;
		}
	}
	for (qpos = 0; qpos<qcnt; ++qpos)
	{
		ring_get(queuelist,qpos,(char*)&curitem);
		if(ISREADY(curitem)) 
		{
			curitem->q_pos = realpos++;
		}
	}
	for (qpos = 0; qpos<qcnt; ++qpos)
	{
		ring_get(queuelist,qpos,(char*)&curitem);
		if(!ISREADY(curitem) && !ISUNSPOOLED(curitem))
		{
			curitem->q_pos = realpos++;
		}
	}
	
#ifdef DBGREORDER
	logerr("exiting reorder\n",0);
#endif
}
void addready(struct q_item *pqitem)							/* add item to ready list */
{
	ring_add(readylist,-1,(char*)&pqitem);
	pqitem->status |= QST_READY;							/* flag it ready */
}
QITEM *findqitem(int id)
{
	int pos;
	QITEM *found, *qp,qi;
	int st;
	
	qp= &qi;
	qi.q_id = id;
	st=ring_find(queuelist,(char*)&qp,&pos,(char*)&found);
	if (st==1)
	{
		found=NULL;
	}
	return found;
}	 
QITEM *findritem(int id)
{
	int pos;
	QITEM *found, *qp,qi;
	int st;
	
	qp= &qi;
	qi.q_id = id;
	st=ring_find(queuelist,(char*)&qp,&pos,(char*)&found);
	if (st==1)
	{
		found=NULL;
	}
	return found;
}	 
int findprinter(struct p_item **d1, struct p_item **d2)
{
	return strcmp((*d1)->printer_name,(*d2)->printer_name);
}
int findqueueitem(struct q_item **d1, struct q_item **d2)
{
	return (*d1)->q_id - (*d2)->q_id;
}
FITEM *findfitem(char *name)
{
	if (strlen(name)==0) return NULL;
	
	for (f_ptr=f_head; f_ptr; f_ptr=f_ptr->next) 
	{
		if (!strcmp(f_ptr->data->form_name,name))
		{
			return f_ptr->data;
		}
	}
	return NULL;
}
void addfitem(struct f_item *data)
{
	FLIST *newlist;
	char *gmem(int cnt, int sz);
	
	newlist=(FLIST*)gmem(1,sizeof(FLIST));
	if (f_head==NULL)
	{
		f_head=newlist;
		f_head->next=NULL;
	}
	else
	{
		for (f_ptr = f_head; f_ptr->next; f_ptr = f_ptr->next);
		f_ptr->next = newlist;
		newlist->next = NULL;
	}
	newlist->data = data;
}
void updatefitem(struct f_item *old, struct f_item *new)
{
	old->lpp = new->lpp;
	strcpy(old->filterstr,new->filterstr);
	strcpy(old->stock,new->stock);
	memcpy(old->initdata,new->initdata,new->initlen);
	old->initlen = new->initlen;
	old->idatatype = new->idatatype;
	freeinits(old->p_init); /* free up old space for init strings */
	freefilts(old->filts); /* and per-model-filters */
	old->p_init = new->p_init;
	old->filts = new->filts;
	old->binary = new->binary;
	old->time = new->time;
#ifdef DEBUGCODE
	D(DBGRELOADFORM)
	{
		sprintf(errmsg,"   UPD ==> %d  Form[%s]  stock[%s]  init[%s]  filt[%s]\n",
			old->time,
			old->form_name,
			old->stock,
			old->initdata,
			old->filterstr);
		logerr(errmsg,0);
		if (old->p_init)
		{
			sprintf(errmsg,"               1st init[%s/%s]\n",old->p_init->model,old->p_init->initdata);
			logerr(errmsg,0);
		}
	}
#endif

}
void new_op(struct master_config *qc, char *name)
{
	int nuid;
	
	if ((nuid=getnuid(name))>=0)
	  qc->operators[qc->opcount++] = nuid;
}
void freeinits(init_s *p)
{
	if (p==NULL) return;
	if (p->next) freeinits(p->next);
	free(p);
}
init_s *findinititem(struct f_item *fi, char *model)
{
	init_s *init_p;
	
	for (init_p = fi->p_init; init_p; NEXT(init_p))
	{
		if (!strcmp(init_p->model,model))
		{
			return init_p;
		}
	}
	return NULL;
}
init_s *newinititem(struct f_item *fi, char *model)
{
	init_s *init_p;
	char *gmem(int cnt, int sz);
	
	if (!fi->p_init) 
	{
		fi->p_init = (init_s*)gmem(1,sizeof(init_s));
		return fi->p_init;
	}
	for (init_p = fi->p_init; init_p->next; NEXT(init_p));
	init_p->next = (init_s*)gmem(1,sizeof(init_s));
	return init_p->next;
}
/* yet another case of copied code that should be shared */
void freefilts(filt_s *f)
{
	if (f==NULL) return;
	if (f->next) freefilts(f->next);
	free(f);
}
filt_s *findfiltitem(struct f_item *fi, char *model)
{
	filt_s *filt_p;
	
	for (filt_p = fi->filts; filt_p; NEXT(filt_p))
	{
		if (!strcmp(filt_p->model,model))
		{
			return filt_p;
		}
	}
	return NULL;
}
filt_s *newfiltitem(struct f_item *fi, char *model)
{
	filt_s *filt_p;
	char *gmem(int cnt, int sz);
	
	if (!fi->filts) 
	{
		fi->filts = (filt_s*)gmem(1,sizeof(filt_s));
		return fi->filts;
	}
	for (filt_p = fi->filts; filt_p->next; NEXT(filt_p));
	filt_p->next = (filt_s*)gmem(1,sizeof(filt_s));
	return filt_p->next;
}
void move_item(char *list, struct q_item *q, int newqpos)
{
	QITEM *poscur;
	int lqpos, qcnt;
	int oldqpos;
	int newlistpos= -1 , oldlistpos= -1;

	if (ISUNSPOOLED(q))
	{
		return;
	}
	oldqpos=q->q_pos;
	
	ring_count(list,&qcnt);
	for (lqpos = 0; lqpos < qcnt; ++lqpos)
	{
		ring_get(list,lqpos,(char*)&poscur);

		if (newqpos == poscur->q_pos)
		{
			newlistpos = lqpos;
		}
		if (oldqpos == poscur->q_pos)
		{
			oldlistpos = lqpos;
		}
		if (newlistpos != -1 && oldlistpos != -1)
		{
			break;
		}
	}
	if (newqpos<=0)
	{
		newlistpos = 0;
	}
	if (newqpos > qcnt)
	{
		newlistpos = qcnt;
	}
	ring_remove(list,oldlistpos,NULL);
	if (oldlistpos < newlistpos)
	{
		--newlistpos;
	}
	ring_add(list,newlistpos,(char*)&q);
}
getnuid(char *p)
{
	struct passwd *pw;
	
	pw=getpwnam(p);
	endpwent();
	if (pw)
	  return pw->pw_uid;
	else 
	  return -1;
}

DEFUN( void,  check_class, (class,before,after,banner,between),
char class AND
int *before AND
int *after AND 
int *banner AND 
int *between END)
{
	int  index;
	index = (int) ( class == ' '?0: toupper(class) - '@' );

	if (index)
	{
		*before = class_info.classlist[index].ffbefore;
		*banner = class_info.classlist[index].header;
		*after = class_info.classlist[index].ffafter;
		*between = class_info.classlist[index].ffbetween;
	}
	else
	{
		*before = FALSE;
		*banner = FALSE;
		*after = TRUE;
		*between = TRUE;
	}
#ifdef DEBUGCODE
	D(DBGCLASSINFO)
	{
		sprintf(errmsg,"class \'%c\', index=%d: before=%s, banner=%s, after=%s, between=%s\n",
			class,index,*before?"TRUE":"FALSE",*banner?"TRUE":"FALSE",*after?"TRUE":"FALSE",
			*between?"TRUE":"FALSE");
		logerr(errmsg,0);
	}
#endif
}	
void check_config_files(void)
{
	int tmp;
	time_t filetime(char *path);
	if ((tmp=filetime(prtdef))>printcap_time)
	{
		loadpcap();							/* it's changed, so reload */
		/*updateprtrs();	*/						/* update what we have */
		printcap_time=tmp;
		need_to_dump_p=TRUE;
	}
	if ((tmp=filetime(formdef))>formdef_time)
	{
#ifdef DEBUGCODE
		D(DBGRELOADFORM)
		{
			logerr("Formfile changed.  Reloading:\n",0);
		}
#endif		
		loadformdef();	
		formdef_time=tmp;
	}
	if ((tmp=filetime(prtdef))>classdef_time)
	{
		loadclassdef();	
		classdef_time=tmp;
	}
	if ((tmp=filetime(ilpdef))>ilpdef_time)
	{
		loadqconfig();	
		ilpdef_time=tmp;
		need_to_dump_cfg=TRUE;
	}
}

void sigpipe(int dummy)
{
	++broken_upipe;
}
void allcaps(char *p)
{
	while (*p)
	{
		*p = toupper(*p);
		++p;
	}
}
void validate_q(void)
{
	int qpos, qcnt;
	QITEM *q;
	
	time_t tmp,time(time_t *);
	
	tmp=time(&tmp);
	if (tmp-last_qvalidate < VALIDATE_GRANULARITY)
	  return;
	
	last_qvalidate=tmp;

	ring_count(queuelist,&qcnt);

#ifdef DEBUGCODE
	D(DBGVALIDATE)
	{
		logerr("Validate Queue: Count is %d\n",qcnt);
	}
#endif
	
	for (qpos=0; qpos<qcnt; ++qpos)
	{
		int st;
		
		st=ring_get(queuelist,qpos,(char*)&q);
		
#ifdef DEBUGCODE
		D(DBGVALIDATE)
		{
			if (st != 0)
			{
				sprintf(errmsg,"    Pos: %3d, err: %5d\n",qpos,st);
			}
			else
			{
				sprintf(errmsg,"    Pos: %3d, Id: %5d, Path: %s\n",
					qpos,q?q->q_id:-1,q?q->real_path:"PANIC: ring_get returned bad pointer");
				logerr(errmsg,0);
			}
		}
#endif
		if (validate_qitem(q)==FALSE)
		{
			removeqitem(q);
			--qpos;
			--qcnt;
		}
	}
	need_to_dump=TRUE;
}
validate_qitem(struct q_item *qi)
{
#ifdef DEBUGCODE
	D(DBGVALIDATE)
	{
		logerr("    access returns %d\n",access(qi->real_path,0));
	}
#endif

	if (access(qi->real_path,0)==0) return TRUE;
	else return FALSE;
}
void validate_printers(void)
{
	PITEM *p;
	int tmp,ppos,pcnt;
	time_t time(time_t *);
	
	tmp=time((time_t*)&tmp);
	if (tmp-last_pvalidate < VALIDATE_GRANULARITY)
	  return;
	
	last_pvalidate=tmp;
	ring_count(printerlist,&pcnt);
	for (ppos=0; ppos<pcnt; ++ppos)
	{
		ring_get(printerlist,ppos,(char*)&p);
		if (!(p->mode & PMD_PROGRAM) &&
		    (p->status & PST_ERROR || p->status & PST_OTHER_BUSY))
		{
			validate_pitem(p);
		}
	}
}
int validate_pitem(struct p_item *pi)
{
	int save_errno,tmp_fd;
	extern int open_timeout;
	
	open_timeout=0;
	signal(SIGALRM,opento);
	alarm(5);
	tmp_fd=open(pi->printer_dev,O_WRONLY|O_CREAT,0666);
	save_errno=errno;
	alarm(0);
	if( tmp_fd != -1 && open_timeout==0)
	{
		pi->status &= ~(PST_ERROR|PST_OTHER_BUSY);
		need_to_dump_p = TRUE;
	}
	if (tmp_fd != -1)
	  close(tmp_fd);
}
void get_class_chars(struct q_item *qi)
{
	int ffa, ffb, ban, bet;
	char class;

	if (qi->class==' ' || qi->class==(char)0)
	{
		if (qi->printer)
		  class=qi->printer->class[0];
		if (class==0)
		{
			class=' ';
		}
	}
	else
	{
		class=qi->class;
	}
	check_class(class,&ffb,&ffa,&ban,&bet);
	if (ffa) qi->mode |= QMD_FFAFTER;
	else     qi->mode &= ~QMD_FFAFTER;
	if (ffb) qi->mode |= QMD_FFBEFORE;
	else     qi->mode &= ~QMD_FFBEFORE;
	if (!ban) qi->mode |= QMD_NOBANNER;
	else     qi->mode &= ~QMD_NOBANNER;
	if (!bet) qi->mode &= ~QMD_FFBETWEEN;
	else      qi->mode |= QMD_FFBETWEEN;
}
#ifdef OLD
parseargs(c,v)
int c;
char **v;
{
        int i,num,type,val;
        char *destval;
        
        for (i=1;i<c;)
        {
                matchopt(v[i],&num,&type,&destval,&val);
                if (num == -1) fprintf(stderr,"bad option \"%s\"\n",v[i]);
                if (type==OPT_INT) *((int*)destval)=val;
                else if (type==OPT_STRING) 
                {
                        if (strlen(v[i])!=strlen(optlist[num].opt)) strcpy(destval,v[i]+strlen(optlist[num].opt));
                        else strcpy(destval,v[++i]);
                }
                ++i;
        }
}
matchopt(opt,num,type,destval,val)
char *opt,**destval;
int *num,*type,*val;
{
        struct optstruct *p;
        int i;
        
        for(i=0,p=optlist;p->opt;++p,++i)
        {
                if (!strncmp(opt,p->opt,strlen(p->opt)))
                {
                        *num=i;
                        *type=p->type;
                        *destval=p->optdest;
                        *val=p->optval;
                        return;
                        
                }
                
        }
        *num= -1;
        *type = OPT_BAD;
}
#endif
/*
 * $Log: d_main.c,v $
 * Revision 1.88  1993/11/17  17:24:23  jockc
 * still losing bits.. fixed again
 *
 * Revision 1.87  1993/11/17  17:22:13  jockc
 * addprtr losing mode bits
 *
 * Revision 1.86  1993/11/10  23:39:51  jockc
 * add auto create of remote dir
 *
 * Revision 1.85  1993/11/10  22:40:32  jockc
 * improve err message when startup fails creating sem and work dirs
 *
 * Revision 1.84  1993/11/10  17:41:14  jockc
 * added priority read from [iu]lpdef
 *
 * Revision 1.83  1993/09/28  18:04:18  jockc
 * left in a debug message
 *
 * Revision 1.82  1993/09/13  23:01:12  jockc
 * use #define for daemon name
 *
 * Revision 1.81  1993/09/13  15:28:14  jockc
 * 499
 * 503
 *
 * Revision 1.81  1993/09/10  18:41:06  jockc
 * fixed up null class wierdness.  fixed up queue validate
 * wierdness.  added some new debug code.  change so that
 * simple CHANGE of form works just like FORMCHANGE on printers
 * with wrong form mounted.  print config errors at startup.
 *
 * Revision 1.80  1993/08/13  22:17:27  jockc
 * const --> CONST
 *
 * Revision 1.79  1993/08/13  22:05:21  jockc
 * const replaced by CONST
 *
 * Revision 1.78  1993/08/13  20:43:05  jockc
 * changed ifdef hpux to HPUX
 *
 * Revision 1.77  1993/08/13  18:34:39  jockc
 * major changes, incl. c89 changes, alpha fixes, better move pos code,
 * etc
 *
 * Revision 1.76  1993/06/04  18:24:17  jockc
 * add kill HUP to shutdown slaves
 *
 * Revision 1.75  1993/06/03  17:44:46  jockc
 * status check of ring funcs in removeq funcs
 *
 * Revision 1.74  1993/06/03  00:40:07  jockc
 * slight change to read unspooler logic.. read is now nonblocked.
 *
 * Revision 1.73  1993/06/02  23:35:17  root
 * use ULIST* in unsplist ring instead of ULIST itself.
 * validate queue had QITEM q instead of QITEM *q
 *
 * Revision 1.72  1993/06/01  23:23:06  root
 * change loadprinters to use less memory\
 *
 * Revision 1.71  1993/05/28  21:43:56  jockc
 * reworked printer and queue list logic.  now using ring.c
 *
 * Revision 1.70  1993/03/25  23:48:49  jockc
 * improved request handling and added some log stuff
 *
 * Revision 1.69  1993/03/11  19:37:28  jockc
 * improved handling of exceptions on startup
 *
 * Revision 1.68  1993/03/01  23:32:14  jockc
 * added something for sequent.. ioctl TIOCNOTTY no good on sequent .. ifdef out
 *
 * Revision 1.67  1993/01/12  23:16:41  jockc
 * forgot to ifdef out FG
 *
 * Revision 1.66  1993/01/12  02:03:54  jockc
 * mostly revised the synchronization stuff.  no longer using
 * lockfile, using semaphores.. attempted to fine tune it.
 * moved the dump stuff into a new module, d_dump
 *
 * Revision 1.65  1993/01/06  23:37:53  jockc
 * allow for class of (char)0
 *
 * Revision 1.64  1993/01/06  22:38:10  jockc
 * change def class from ' ' to 'A'
 *
 * Revision 1.63  1993/01/04  22:42:43  jockc
 * look out lock four on dump
 *
 * Revision 1.62  1993/01/04  21:35:40  jockc
 * check printer element of qitem before derefing it in get_class_chars
 *
 * Revision 1.61  1993/01/04  19:36:56  jockc
 * slight revision for SUNOS FAKEARGS
 *
 * Revision 1.60  1992/12/31  23:41:32  jockc
 * support for qpath/ISPOOL. improved dump and load
 * by using tmp file.. added version stamp to NICEPS.
 * added ffbetween to class for ff's between mult copies
 *
 * Revision 1.59  1992/11/13  18:54:41  jockc
 * added motorola change
 *
 * Revision 1.58  1992/10/27  23:23:36  root
 * added root trick
 *
 * Revision 1.57  1992/10/09  18:23:56  jockc
 * new locking scheme incorporated
 * move_item fixed.  IDEBUG added
 *
 * Revision 1.56  1992/10/02  20:06:31  root
 * fixed bogus usage of pointer p in change() code.
 *
 * Revision 1.55  1992/08/04  00:17:33  jockc
 * changed fo fq and maxc to stop data bits and parity
 *
 * Revision 1.54  1992/07/14  21:35:31  jockc
 * added code to not adjust position if qpos is -1
 *
 * Revision 1.53  1992/07/10  21:18:58  jockc
 * create all dirs first.
 *
 * Revision 1.52  1992/07/10  17:27:41  jockc
 * fixed seg violation if operator not found
 *
 * Revision 1.51  1992/07/07  23:06:45  jockc
 * added improved lock for daemon detection.  fixed err message if cant
 * setuid
 *
 * Revision 1.50  1992/07/02  18:17:18  jockc
 * must allow disable of busy printer.
 *
 * Revision 1.49  1992/07/02  16:42:56  jockc
 * added init of printer_id in PITEM
 *
 * Revision 1.48  1992/06/29  21:49:56  jockc
 * *** empty log message ***
 *
 * Revision 1.47  1992/06/26  22:35:51  jockc
 * change log of errno to log of sys_errlist[errno]
 *
 * Revision 1.46  1992/06/12  20:11:32  jockc
 * added void decl for truncspc
 *
 * Revision 1.45  1992/06/09  00:05:01  jockc
 * *** empty log message ***
 *
 * Revision 1.44  1992/06/08  23:55:27  jockc
 * polish code to remove some warnings
 *
 * Revision 1.43  1992/06/04  22:38:27  jockc
 * sleep on unblocked read of unspooler pipe, to prevent tight loops
 *
 * Revision 1.42  1992/05/22  21:25:59  jockc
 * change set perms on dirs to include +x
 *
 * Revision 1.41  1992/05/20  23:14:32  jockc
 * check dumpfile versions before loading
 * changed locations of the dump files. write
 * dummy dump file for old ilpmans
 *
 * Revision 1.40  1992/05/07  22:37:23  jockc
 * NCR fix for setpgrp()
 *
 * Revision 1.39  1992/05/04  20:46:49  jockc
 * remember to clear the dump ilpconf flag
 *
 * Revision 1.38  1992/04/30  18:52:32  jockc
 * added NICEPS, ilpdef:operator list, default printer
 * pnum 0 behavior changed back, version number gen moved
 * to utils.c
 *
 * Revision 1.37  1992/04/29  21:05:31  jockc
 * fixed formfeed reversal, added filter-per-model logic
 * fixed warnings generated by GCC
 *
 * Revision 1.36  1992/03/27  22:52:52  jockc
 * validation of busy/error printers
 * prtnum 0 allowed, allow printer init to come from file
 * create dirs init and pstatus if not exist.  clear ready
 * bit when ready item is reheld.. (IP's bug)
 *
 * Revision 1.35  1992/03/13  22:29:49  jockc
 * fix printer name spec that was broke when code added to handle class
 * ' ' printing
 *
 * Revision 1.34  1992/03/13  21:29:15  jockc
 *  make VERSION avail to other modules, return if mod on unspooled
 * job, move class check to: queue, change, respool.  set flags for
 * ff and banner in qitem.  fixup print with blank class. support
 * for printer is program/device
 *
 * Revision 1.33  1992/02/13  23:35:52  jockc
 * fixed autoload on config file change, support
 * no select machines()
 *
 * Revision 1.32  1992/01/06  19:37:21  jockc
 * change delete/unlink to use real_path member of qitem
 *
 * Revision 1.31  1991/12/17  22:36:57  jockc
 * validate queue functionality, new prt num and mode accepted from client
 *
 * Revision 1.30  1991/10/12  00:57:38  jockc
 * zero fitem on dumpload
 *
 * Revision 1.29  1991/10/12  00:07:04  jockc
 * toupper on queue item change
 *
 * Revision 1.28  1991/10/11  23:08:46  jockc
 * set daemon_pid permissions properly
 *
 * Revision 1.27  1991/10/10  22:38:56  jockc
 * added code for hold when stopped
 * upcase class specs
 * enable/disable printers
 *
 * Revision 1.26  1991/10/10  16:16:25  jockc
 * init unspooling flag to false
 *
 * Revision 1.25  1991/10/08  23:34:52  jockc
 * don't let unspoolers that crash clear daemon running flag
 *
 * Revision 1.24  1991/10/08  23:26:10  jockc
 * limited support for printer model
 *
 * Revision 1.23  1991/10/07  18:25:36  jockc
 * allow hold of stopped job
 *
 * Revision 1.22  1991/10/01  17:04:49  jockc
 * add support for new prt, class, form def.
 * simplify communications.  tore out client handling
 * stuff.  added file load and dump for q and prt info
 * massive changes throughout the system
 *
 * Revision 1.21  1991/09/12  03:22:40  jockc
 * fixed hang on del printer bug
 *
 * Revision 1.20  1991/08/07  18:35:15  jockc
 * turn on fork before monitor thing again,
 * add dump queue into file, read at startup, for
 * fault tolerance
 *
 * Revision 1.19  1991/08/05  23:01:43  jockc
 * added printer number logic
 *
 * Revision 1.18  1991/08/05  19:59:13  jockc
 * roll queue_id over at 9999
 *
 * Revision 1.17  1991/08/02  23:49:40  jockc
 * fixed bogus wait on client shutdown. (CST_CLOSING added)
 * fixed held file still prints bug (remove from ready list)
 * fixed behavior of delete (added unlink)
 *
 * Revision 1.16  1991/07/22  23:00:41  jockc
 * added stuff to handle printer errors
 * fixed delete of qitems from ready queue
 *
 * Revision 1.15  1991/06/27  18:24:55  jockc
 * changes related to UNIXDOMAIN socket shtuff
 *
 * Revision 1.14  1991/05/22  17:05:12  jockc
 * changes for sco: missing decl for filetime() added,
 * TIOCNOTTY missing, ioctl removed
 *
 * Revision 1.13  1991/05/13  18:11:23  jec
 * add x perm to spool dir, so procs can search it
 *
 * Revision 1.12  1991/05/07  23:43:11  jockc
 * ifdef bsdwait for union wait decl
 *
 * Revision 1.11  1991/05/07  17:01:39  jockc
 * moved setuid root to start of prog..  when non root user started
 * daemon, isrunning fails with EPERM...
 *
 * Revision 1.10  1991/05/02  19:34:07  jockc
 * many changes here:
 * 	fork daemons before monitor and query ops
 * 	implemented scheme to wait() for said procs
 * 	disconnect from controlling tty
 * 	check validity of c_head in monitor
 * 	stop using communication lock
 *
 * Revision 1.9  1991/05/01  22:45:28  jockc
 * checks for already running daemon
 *
 * Revision 1.8  1991/04/30  23:30:24  jockc
 * misc stuff:
 * daemon correctly handles form specs in iprintcap that are
 * numeric (yacc grammar expected alpha).  No delay in wisp
 * screen manager at startup. also reentry into ilpwisp routines
 * are now ok
 *
 * Revision 1.7  1991/04/30  17:43:57  jockc
 * misc cleanup
 *
 * Revision 1.6  1991/04/25  21:24:58  jockc
 * *** empty log message ***
 *
 * Revision 1.5  1991/04/23  18:57:44  jockc
 * add pid to startup message
 *
 * Revision 1.4  1991/04/19  00:56:08  jockc
 * *** empty log message ***
 *
 * Revision 1.3  1991/04/19  00:47:17  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/19  00:22:24  jockc
 * fixed version stuff
 *
 * Revision 1.1  1991/04/18  23:49:33  jockc
 * Initial revision
 *
 *
 */
