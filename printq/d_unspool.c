/*
 * Module:  daemon_unspool
 * Program: IDSIprint
 * Purpose: routines for unspooling
 *
 */
static char rcsid[] = "$Id:$";

#define _DAEMON_UNSPOOL_
#define EXT extern 
#include "daemon.h"

FILE *test;

extern char *sys_errlist[];
extern char VERSION[];
static char self[1024];

static char *prtname;

/* FIXME -- the entire module could use a rewrite, esp q_unspool() which has grown to be
   gigantic.  
   */

/* 
 * unspool(): general purpose routine for unspooling files.  examine structures to determine
 *            what can be printed and where.. call q_unspool() to do the work.
 */

extern int ring_add (ring_struct *ring_ptr, int position, char *element);
int shut_if_obsolete (struct unspooler_list *unsp);
extern int ring_remove (ring_struct *ring_ptr, int position, char *element);
extern int ring_find (ring_struct *ring_ptr, char *match, int *position, char *element);

extern int broken_upipe;

void unspool(void)
{
	QITEM *q;
	char *p;
	FITEM *findfitem(char *name);
	static long last=0;
	time_t curt,time();
	long cur;
	int rcnt;
	int ppos,pcnt;
	PITEM *curprinter;
	
	curt = time(&curt);                          /* prevent rapid unspooling that consumes too much */
	cur = (long) curt;
	if (cur - last < 10)			   /* system resource */
	  return;
	
	last = cur;
	
	ring_count(readylist,&rcnt);
	ring_count(printerlist,&pcnt);
	
	if (rcnt==0) 
	{
		return;				   /* no files ready to print, so return */
	}
	
	for (ppos=0; ppos<pcnt; ++ppos) /* now look for idle printers */
	{
		ring_get(printerlist,ppos,(char *)&curprinter);
		
#ifdef DEBUGCODE
		D(DBGUNSPOOL)
		{
			logerr("printer %s\n",curprinter->printer_name);
		}
#endif
		
                /* skip busy, errored, disabled, or busy (other program) printers */
		if (!ISIDLE(curprinter) || ISERROR(curprinter) || !ISENABLED(curprinter) || 
		    ISINUSEBYOTHER(curprinter) || ISWAITSHUT(curprinter) )
		{
			continue;
		}
		if (strlen(curprinter->class)==0) strcpy(curprinter->class," ");  /* correct null class */
		
		for (p=curprinter->class; *p ; ++p) /* found a printer.. see what files could print on it */
		{
                        /* call findrdyqitem() to find jobs that fit this printer */
			if (q=findrdyqitem(*p,curprinter->printer_name,curprinter->prtnum)) 
			{
#ifdef DEBUGCODE
				D(DBGUNSPOOL)
				{
					logerr("\tprinting %s\n",q->orig_path);
				}
#endif
				
				q->printer = curprinter;          /* attach printer struct to qitem */
				q->status |= QST_UNSPOOLED;	   /* flag as unspooled */
				q->status &= ~QST_READY;	   /* unflag as ready to print */
				removereadyqitem(q);
				
                                /* now decide which form to use */
				if (!q->fitem)			   /* if qitem had no form specified */
				{
					if (strlen(curprinter->default_form))  /* does printer have a default form? */
					{
                                                /* yes, so the the fitem struct for the prtrs def form */
						q->fitem = findfitem(curprinter->default_form);
						if (q->fitem) 
						{
							/* if found, also copy the formname */
							strcpy(q->form,curprinter->default_form);
						}
						else
						{
							/* otherwise, take the first form on the list */
							if (f_head)
							{
								q->fitem = f_head->data;
								strcpy(q->form,f_head->data->form_name);
							}
						}
					}
					else
					{       /* no printer def form, so take first in form list */
						q->fitem = f_head->data;
						strcpy(q->form,f_head->data->form_name);
					}
				}
				if (q->fitem)    
				{
					/* now get the fitem for the currently mounted form */
					if (fitemp=findfitem(curprinter->current_form))
					{
						/* decide if we need to ChgFrm */
						if (strcmp(q->fitem->stock,fitemp->stock))
						{
							/* yep, flag formchange and chkprt */
							q->printer->status |= PST_FORMCHANGE;
							q->status |= QST_CHKPRT;
							queue_modified();        /* flag queue as modified */
						}
						else
						{       /* stock is the same, just mount the new form */
							strcpy(curprinter->current_form,q->fitem->form_name);
						}
					}
					else
					{       /* no form mounted, so flag for a formchange */
						q->printer->status |= PST_FORMCHANGE;
						q->status |= QST_CHKPRT;
						queue_modified();
					}
				}
				q->printer->status |= PST_BUSY;   /* flag printer as in use */
				pstatus_changed();
				queue_modified();
				if (q_unspool(q) == FALSE)   /* now dispatch to print.. q_unspool will fork() */
				{
					/* it didn't work, so undo all this stuff */
					q->printer->status &= ~(PST_BUSY|PST_FORMCHANGE|QST_STOPPED);
					q->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
				}
				break;
			}
			else
			{
#ifdef DEBUGCODE
				D(DBGUNSPOOL)
				{
					logerr("\tnothing in class %c\n",*p);
				}
#endif
                        }
		}
		
	}
}
/*
 * open_timeout(): flag timeout on open of printer 
 */
int open_timeout;
void opento(int sig)
{
	++open_timeout;
}
/*
 * q_unspool(): setup comm link between master and slave.. fork and begin printing.. 
 *
 */
int q_unspool(struct q_item *qitem)
{
	
	int pid, pipe1[2], pipe2[2];                  /* pipes for comm with master */
	int status;
	FILE *in,*out;
	int outfd;
#ifndef NO_SELECT	
	fd_set rdfds,wrfds;
#endif
	int x, cnt, stopped, rd, wrt, aligning;
	struct termio p;
	char inbuf[2048],*inbufp, init[200];
	char formpath[200];
	int plen=66;
	int save_errno;
	char ifilter[2000],ofilter[2000],usersfilter[2000];
	int pagesel,pcretstat;
	filt_s *filt_p;
	char ff[2];
	QITEM newqitem;
	int have_job;
	ULIST *unsp;
	UMSG msg;
	FITEM tempform;
	char statname[2048];
	
#if defined(_AIX) || defined(AIX)
	struct lprmod slpmod,nlpmod;
	struct lprio slpio,nlpio;
	struct lpr232 slp232,nlp232;
#endif
	
	ff[0]=(char)12;
	ff[1]=(char)0;
	/* check of class removed hear, class checked on queue rcpt and change in d_main */
	
	if (!(unsp=find_unspooler(qitem->printer->printer_name)))
	{
		int st1, st2;
#ifdef DEBUGCODE
		D(DBGUNSPOOLP)
		{
			logerr("creating unspooler for printer %s\n",qitem->printer->printer_name);
		}
#endif
		
		st1=pipe(pipe1);		/* pipe for child->parent communication */
		st2=pipe(pipe2);		/* pipe for parent->child communication */
		
		if (st1!=0)
		{
			sprintf(errmsg,"Warning: while creating Slave Process the following condition was encountered:\n%s (errno=%d)\n",
				sys_errlist[errno],errno);
			logerr(errmsg,0);
			return FALSE;
		}
		if (st2!=0)
		{
			sprintf(errmsg,"Warning: while creating Slave Process the following condition was encountered:\n%s (errno=%d)\n",
				sys_errlist[errno],errno);
			logerr(errmsg,0);
			close(pipe1[0]);
			close(pipe1[1]);
			return FALSE;
		}

		/* #ifdef NO_SELECT*/
# ifdef O_NONBLOCK
		fcntl(pipe1[0],F_SETFL,O_NONBLOCK);
		fcntl(pipe1[1],F_SETFL,O_NONBLOCK);
		fcntl(pipe2[0],F_SETFL,O_NONBLOCK);
		fcntl(pipe2[1],F_SETFL,O_NONBLOCK);
# endif
# ifdef O_NDELAY
		fcntl(pipe1[0],F_SETFL,O_NDELAY);
		fcntl(pipe1[1],F_SETFL,O_NDELAY);
		fcntl(pipe2[0],F_SETFL,O_NDELAY);
		fcntl(pipe2[1],F_SETFL,O_NDELAY);
# endif
		/* #endif */
		wrt=pipe1[1];
		rd =pipe2[0];
		
		pid=fork();
		if (pid == -1)
		{
			sprintf(errmsg,"Panic: Could not fork: %s (errno=%d)\n",
				sys_errlist[errno],errno);
			logerr(errmsg,0);
			close(pipe1[0]); close(pipe1[1]);
			close(pipe2[0]); close(pipe2[1]);
			return FALSE;
		}
		if (pid)
		{
			close(wrt);
			close(rd);
			addulist(pipe1[0],pipe2[1],pid,qitem->printer,qitem);
			strcpy(qitem->actual_prtr,qitem->printer->printer_name);
			return TRUE;
		}
	}
	else
	{
#ifdef DEBUGCODE
		D(DBGUNSPOOLP)
		{
			logerr("MSG to unspooler for printer %s:\n",unsp->printer->printer_name);
			sprintf(errmsg,"    PRINT q_id %d, %s.\n",qitem->q_id,qitem->real_path);
			logerr(errmsg,0);
		}
#endif
		
		memcpy(msg.message,qitem,sizeof(QITEM));
		msg.status = UMSG_PRINT;
		write(unsp->wfd,(char*)&msg,sizeof(UMSG));
		unsp->printer->qid=qitem->q_id;
		unsp->qitem = qitem;
		strcpy(qitem->actual_prtr,unsp->printer->printer_name);
		return TRUE;
	}
	have_job=TRUE;
	
	signal(SIGCHLD,SIG_DFL);
	signal(SIGHUP,master_shutdown);
	prtname = qitem->printer->printer_name;
	sprintf(self,"prtr[%s] id[%d]",qitem->printer->printer_name,qitem->q_id);
	
	unspooling=TRUE;	
#ifdef NICEPS
	if (progname_ptr)
	{
		sprintf(errmsg,DUNSPNAME,VERSION,qitem->printer->printer_name);
		memcpy(progname_ptr,errmsg,FAKEARGLEN);
	}
#endif	
#ifdef DEBUGCODE
	D(DBGCHILD)
	{
		logerr("UNSP %s: Ready\n",self);
	}
#endif
	
      top_q_unspool:
	
	if (!have_job)
	{
		int bytes=0;
		
#ifdef DEBUGCODE
		D(DBGCHILD)
		{
			logerr("UNSP %s: Waiting next command...\n",self);
		}
#endif
		
		while (bytes!=sizeof(UMSG))
		{
			bytes=read(rd,(char*)&msg,sizeof(UMSG));
			sleep(3);
		}
#ifdef DEBUGCODE
		D(DBGCHILD)
		{
			sprintf(errmsg,"UNSP %s: read %d bytes [%s]:\n",
				self,bytes,unspcmds[msg.status]);
			logerr(errmsg,0);
		}
#endif
		
		if (bytes==sizeof(UMSG) && msg.status==UMSG_PRINT)
		{
			memcpy(&newqitem,msg.message,sizeof(QITEM));
			qitem = &newqitem;
			
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				sprintf(errmsg,"UNSP %s: cmd  PRINT %s\n",qitem->real_path);
				logerr(errmsg,0);
			}
#endif
			
			if (qitem->status & QST_CHKPRT)
			{
				qitem->printer->status |= PST_FORMCHANGE;
			}
		}
		else if (msg.status==UMSG_SHUTDOWN)
		{
			int st;
			
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				logerr("UNSP %s: Shutting down on request\n",self);
			}
#endif
			
			msg.status = UMSG_EXITING;
			st=write(wrt,(char*)&msg,sizeof(UMSG));
			
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				sprintf(errmsg,"UNSP %s: Sent EXITING packet, status %d\n",self,st);
				logerr(errmsg,0);
			}
#endif
			close(rd);
			close(wrt);
			exit(0);
		}
	}
	
	sprintf(self,"prtr[%s] id[%d]",qitem->printer->printer_name,qitem->q_id);
	
#ifdef DEBUGCODE
	D(DBGCHILD)
	{
		if (WRONGFORM(qitem->printer))
		{
			sprintf(errmsg,"UNSP %s: FORM mounted %s need %s\n",self,
				qitem->printer->current_form,qitem->form);
		}
	}
#endif
	
	aligning=FALSE;
	
	if (WRONGFORM(qitem->printer))
	{
		int bytesread;
		int cnt;
		
#ifdef DEBUGCODE
		D(DBGCHILD)
		{
			logerr("UNSP %s: awaiting form change command..\n",
			       self);
			cnt=0;
		}
#endif
		
		while ((bytesread=read(rd,(char*)&msg,sizeof(UMSG)))<=0) 
		{
			sleep(2);
			
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				++cnt;
				if ((cnt%10)==0)
				{
					logerr("UNSP %s: still waiting..\n",self);
				}
			}
#endif
			
		}
		
#ifdef DEBUGCODE
		D(DBGCHILD)
		{
			sprintf(errmsg,"UNSP %s read %d bytes:\n",self,bytesread);
			logerr(errmsg,0);
			sprintf(errmsg,"UNSP %s received %s command\n",self,unspcmds[msg.status]);
			logerr(errmsg,0);
		}
#endif
		
		if (msg.status==UMSG_ALIGN) aligning=TRUE;
		if (msg.status==UMSG_FORMCHANGED)
		{
			int st;
			
			qitem->printer->status &= ~PST_FORMCHANGE;
			msg.status = UMSG_NEWFORM;
			strcpy(msg.message,qitem->fitem->form_name);
			st=write(wrt,(char*)&msg,sizeof(UMSG));
			strcpy(qitem->printer->current_form,qitem->fitem->form_name);
			
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				sprintf(errmsg,"UNSP %s: Using form %s\n",self,qitem->fitem->form_name);
				logerr(errmsg,0);
				logerr("wrote %d bytes to master\n",st);
			}
#endif
			
		}
		if (msg.status==UMSG_ABORT)
		{
			msg.status=UMSG_UNUNSPOOL;
			write(wrt,(char*)&msg,sizeof(UMSG));
			
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				logerr("UNSP %s: No formchange, abort instead\n",self);
			}
#endif
			
			qitem->printer->status &= ~PST_FORMCHANGE;
			have_job=FALSE;
			goto top_q_unspool;
		}
	}

	sprintf(statname,"%s/%s",pstatusd,qitem->printer->printer_name);
	x=open(statname,O_WRONLY|O_CREAT|O_TRUNC,0666);
	if (x<0)
	{
		sprintf(errmsg,"Error opening printer status file %s: %s\n",
			statname,sys_errlist[errno]);
		logerr(errmsg,0);
	}
	chmod(statname,0666);
	if (x!=2)
	{
		close(2);
		if (dup2(x,2)<0)
		{
			sprintf(errmsg,"Error opening [dup2] printer status file %s: %s\n",
				statname,sys_errlist[errno]);
			logerr(errmsg,0);
		}
		close(x);
	}

	if (qitem->printer->mode & PMD_PROGRAM)
	{
		strcpy(ofilter,dosubst(qitem->printer->printer_dev,qitem,qitem->fitem->lpp));
#ifdef DEBUGCODE
		D(DBGCHILD)
		{
			sprintf(errmsg,"UNSP %s: Printer[%s]\n",self,ofilter);
			logerr(errmsg,0);
		}
#endif
		
		if ((qitem->printer->mode & PMD_PROGSTDIN)!=0)
		{
			out=popen(ofilter,"w");
			outfd=fileno(out);
#ifdef DEBUGCODE
			D(DBGCHILD)
			{
				sprintf(errmsg,"UNSP %s: pipe[%x/%d] status file %s\n",self,out,outfd,statname);
				logerr(errmsg,0);
			}
#endif
		}
		else 
		{
			pcretstat = system(ofilter);
			goto skipioloop;
		}
	}
	else
	{
		signal(SIGALRM,opento);
		open_timeout=0;
		alarm(20);
		outfd=open(qitem->printer->printer_dev,O_WRONLY|O_CREAT,0666);
		save_errno=errno;
		alarm(0);
		if( outfd == -1 || open_timeout)
		{
			msg.status=UMSG_ERR;
			msg.aux=save_errno;
			sprintf(msg.message,"error opening printer %s: %s (errno=%d)",qitem->printer->printer_dev,
				sys_errlist[save_errno],save_errno);
			write(wrt,(char*)&msg,sizeof(UMSG));
			sprintf(errmsg,"error opening printer %s: %s (errno=%d)\n",qitem->printer->printer_dev,
				sys_errlist[save_errno],save_errno);
			write(2,errmsg,strlen(errmsg));
			have_job=FALSE;
			close(2);
			goto top_q_unspool;
		}
		out=fdopen(outfd,"w");
#ifdef DEBUGCODE
		D(DBGCHILD)
		{
			sprintf(errmsg,"UNSP %s: Printer[%s] file[%x/%d]\n",
				self,qitem->printer->printer_dev,out,outfd);
			logerr(errmsg,0);
		}
#endif
		
	}
	if (isatty(outfd))
	{
		int baud, sbit, dbit, par;
#if !defined(AIX) && !defined(_AIX)
		if (ioctl(outfd,TCGETA,&p)== -1)	
		  logerr("ioctl TCGETA failed: %s\n",sys_errlist[errno]);
#else
		if (ioctl(outfd,LPRGETA,&slp232) == -1)
		  logerr("ioctl LPRGETA failed: %s\n",sys_errlist[errno]);
		nlp232 = slp232;
#endif
		p.c_iflag |= IXON;
		p.c_oflag=OPOST|ONLCR;
		baud =
#ifdef B38400
		  qitem->printer->baud==38400?B38400:
#endif
#ifdef B19200
		  qitem->printer->baud==19200?B19200:
#endif
		  qitem->printer->baud==9600?B9600:
		  qitem->printer->baud==4800?B4800:
		  qitem->printer->baud==2400?B2400:
		  qitem->printer->baud==1200?B1200:
		  qitem->printer->baud==300?B300:
		  B9600;
		
		dbit = 
		  qitem->printer->databits==8? CS8:
		  qitem->printer->databits==7? CS7:
		  qitem->printer->databits==6? CS6:
		  qitem->printer->databits==5? CS5:
		  CS8;
		
		sbit = 
		  qitem->printer->stopbits==2? CSTOPB:0;
		
		par = 
		  qitem->printer->parity==PAR_NONE? 0:
		  qitem->printer->parity==PAR_EVEN? PARENB:
		  qitem->printer->parity==PAR_ODD? (PARENB|PARODD) :
		    0;

#if !defined(AIX) && !defined(_AIX)
		p.c_cflag= (baud|dbit|sbit|par)|CREAD;
		if (ioctl(outfd,TCSETA,&p)== -1) 
		  logerr("ioctl TCSETA failed: %s\n",sys_errlist[errno]);
#else
		nlp232.c_cflag= (baud|dbit|sbit|par)|CREAD;
		if (ioctl(outfd,LPRSETA,&nlp232) == -1)
		  logerr("ioctl LPRSETA failed: %s\n",sys_errlist[errno]);
		
		if (ioctl(outfd,LPRGET,&slpio) == -1)
		  logerr("ioctl LPRGET failed: %s\n",sys_errlist[errno]);
		nlpio=slpio;
		nlpio.ind=0;
		nlpio.col=1024;
		nlpio.line=1024;
		if (ioctl(outfd,LPRSET,&nlpio) == -1)
		  logerr("ioctl LPRSET failed: %s\n",sys_errlist[errno]);

		if (ioctl(outfd,LPRMODG,&slpmod) == -1)
		  logerr("ioctl LPRMODG failed: %s\n",sys_errlist[errno]);
		nlpmod = slpmod;
		nlpmod.modes = WRAP;
		if (ioctl(outfd,LPRMODS,&nlpmod) == -1)
		  logerr("ioctl LPRMODS failed: %s\n",sys_errlist[errno]);
#endif
	}
	
	pagesel=FALSE;
#ifdef DEBUGCODE
	D(DBGCHILD)
	{
		sprintf(errmsg,"UNSP %s: Form is %s.  Fitem is %x\n",self,qitem->form,qitem->fitem);
		logerr(errmsg,0);
	}
#endif
	
	if (strlen(qitem->form)) 
	{
		fitemp = qitem->fitem;
		if (fitemp)
		{
			char *the_initstr;
			extern char initbuf[];
			int x,type;
			
			int the_len;
			plen = fitemp->lpp;
			if (qitem->stpage != 1 || qitem->endpage != 99999) pagesel=TRUE;
			x=getinitstr(fitemp,qitem->printer,&the_initstr,&the_len,&type);
			if (x)
			{
				if (type==INITDFILE)
				{
					char initfilename[200];
					FILE *ifile;
					
					sprintf(initfilename,"%s/%s",initdir,the_initstr);
					if (access(initfilename,0)==0)
					{
						ifile = fopen(initfilename,"r");
						if (ifile)
						{
							the_len = fread(initbuf,1,INITSIZE,ifile);
							the_initstr = initbuf;
							fclose(ifile);
						}
						else
						  the_len = 0;
					}
					else the_len = 0;
				}
			}
			if (the_len) 
			  write(outfd,the_initstr,the_len);
			if (filt_p = findfiltitem(fitemp,qitem->printer->printer_model))
			{
				strcpy(usersfilter,filt_p->filter);
			}
			else
			  strcpy(usersfilter,fitemp->filterstr);
		}
		else
		{
			fitemp = qitem->fitem = &tempform;
			memset(&tempform,0,sizeof(tempform));
			fitemp->binary=0;
			fitemp->lpp=66;
			strcpy(usersfilter,"cat");
		}
	}
	if (!aligning)
	{
		char **tmp,**banner(struct q_item *q);
		int i=0;
		
		if (FFBEFORE(qitem))
		  write(outfd,ff,1);
		if (WANTBANNER(qitem))			 /* no longer check class banner status here, now do */
		{					 /* in d_main, when queue command received */
			tmp=banner(qitem);
			while (tmp[i])
			{
				write(outfd,tmp[i],strlen(tmp[i]));
				++i;
			}
			write(outfd,ff,1);		 /* banner always needs ff after it */
		}
	}
	
	if (qitem->stpage==0) qitem->stpage=1;
	
	build_filtercmd(ifilter,usersfilter,qitem,plen);
	
      mult_copies:
	
	if (qitem->fitem->binary)
	{
		in=fopen(qitem->real_path,"r");
#ifdef DEBUGCODE
		D(DBGFILT)
		{
			sprintf(errmsg,"UNSP %s: Input file [%s] FILE %x\n",
				self,qitem->real_path,in);
			logerr(errmsg,0);
		}
#endif
		
	}		      
	else
	{
		in=popen(ifilter,"r");
#ifdef DEBUGCODE
		D(DBGFILT)
		{
			sprintf(errmsg,"UNSP %s: Input file [%s] FILE %x\n",
				self,ifilter,in);
			logerr(errmsg,0);
		}
#endif
		
	}
	
	if( !in )
	{
		msg.status=UMSG_ERR;
		sprintf(msg.message,"can't open %s",qitem->real_path);
		write(wrt,(char*)&msg,sizeof(UMSG));
		have_job=FALSE;
		close(2);
		goto top_q_unspool;
	}
	
	if (aligning)
	{
		int i;
		
		for (i=0; i<8; ++i)
		{
			inbufp=fgets(inbuf,2048,in);
			if (inbufp==NULL) break;
			x=write(outfd,inbuf,strlen(inbuf));
		}

#if defined(AIX) || defined(_AIX)
		if (ioctl(outfd,LPRSETA,&slp232) == -1)
		  logerr("ioctl LPRSETA failed: %s\n",sys_errlist[errno]);
		if (ioctl(outfd,LPRSET,&slpio) == -1)
		  logerr("ioctl LPRSET failed: %s\n",sys_errlist[errno]);
		if (ioctl(outfd,LPRMODS,&slpmod) == -1)
		  logerr("ioctl LPRMODS failed: %s\n",sys_errlist[errno]);
#endif

		close(outfd);
		fclose(in);
		close(2);
		goto top_q_unspool;
	}
	
	stopped=FALSE;

	myfgets(NULL,0,NULL,0,1);
	
	for (pagecount=1;;)
	{
#ifndef NO_SELECT
		struct timeval to;
		
		FD_ZERO(&rdfds);
		FD_ZERO(&wrfds);

		if (!stopped)
		{
			FD_SET(outfd,&wrfds);
		}
		FD_SET(rd,&rdfds);
		
# ifdef DEBUGCODE
		D(DBGCHILD2)
		{
			logerr("UNSP %s: Top of main loop\n",self);
		}
# endif
		
		to.tv_sec = 2;	
		to.tv_usec = 0;

		/*FDSETCAST defined in defs.h */		
		cnt = select(64,FDSETCAST &rdfds,FDSETCAST &wrfds,0,&to);
		
# ifdef DEBUGCODE
		D(DBGCHILD2)
		{
			sprintf(errmsg,"UNSP %s: %d fds ready; out:%s daemon:%s\n",
				self,cnt,FD_ISSET(outfd,&wrfds)?"Yes":"No",
				FD_ISSET(rd,&rdfds)?"Yes":"No");
			logerr(errmsg,0);
		}
# endif
		
		if (FD_ISSET(rd,&rdfds))
		{
			read(rd,(char*)&msg,sizeof(UMSG));
#else
		if (read(rd,&msg,sizeof(UMSG))>0)
		{
#endif	/* NO_SELECT */
			switch(msg.status)
			{
			      case UMSG_ABORT:
#ifdef DEBUGCODE
				D(DBGCHILD)
				{
					logerr("UNSP %s: RCVD Abort\n",self);
				}
#endif
				
				msg.status=UMSG_DONE;
				write(wrt,(char*)&msg,sizeof(UMSG));
				abortinfo(qitem,outfd);
				have_job=FALSE;
				if (qitem->fitem->binary)
				  fclose(in);
				else
				  pclose(in);
				if (qitem->printer->mode & PMD_PROGRAM)
				  pcretstat = pclose(out);
				else
				  fclose(out);
				close(2);
				goto top_q_unspool;
				break;
			      case UMSG_STOP:
#ifdef DEBUGCODE
				D(DBGCHILD)
				{
					logerr("UNSP %s: RCVD STOP\n",self);
				}
#endif
				
				stopped=TRUE;
				break;
			      case UMSG_START:
#ifdef DEBUGCODE
				D(DBGCHILD)
				{
					logerr("UNSP %s: RCVD STOP\n",self);
				}
#endif
				
				stopped=FALSE;
				break;
			      case UMSG_HOLD:
#ifdef DEBUGCODE
				D(DBGCHILD)
				{
					logerr("UNSP %s: RCVD HOLD\n",self);
				}
#endif
				
				msg.status=UMSG_UNUNSPOOL;
				write(wrt,(char*)&msg,sizeof(UMSG));
				abortinfo(qitem,outfd);
				have_job=FALSE;
				if (qitem->fitem->binary)
				  fclose(in);
				else
				  pclose(in);
				if (qitem->printer->mode & PMD_PROGRAM)
				  pcretstat = pclose(out);
				else
				  fclose(out);
				close(2);
				goto top_q_unspool;
				break;
			}
		}
#ifndef NO_SELECT
		else if (FD_ISSET(outfd,&wrfds) && !stopped)
#else
		else if (!stopped)
#endif
		{
			int len;
			
			if (qitem->fitem->binary)
			{
				len=fread(inbuf,1,2048,in);
			}
			else
			{
				len=myfgets(inbuf,2048,in,qitem->fitem->lpp,0);
			}
#ifdef DEBUGCODE
			D(DBGCHILD2)
			{
				sprintf(errmsg,"UNSP %s: I/O read %d bytes (%s) page %s %d %d/%d\n",self,len,
					qitem->fitem->binary?"fread":"myfgets",
					pagesel?"PAGESEL":"!PAGESEL",pagecount,qitem->stpage,qitem->endpage);
				logerr(errmsg,0);
			}
#endif
			
			if (len==0) break;
			if (pagesel && !(qitem->fitem->binary))
			{
				if (pagecount > qitem->endpage)
				  break;
				
				if (!(pagecount >= qitem->stpage && pagecount <= qitem->endpage))
				  continue;
			}
			x=write(outfd,inbuf,len);
#ifdef DEBUGCODE
			D(DBGCHILD2)
			{
				sprintf(errmsg,"UNSP %s: wrote %d bytes to output stream\n",self,x);
				logerr(errmsg,0);
			}
#endif
			
			if (x <= 0 && len)
			{
				msg.status=UMSG_ERR;
				if ((qitem->printer->mode & PMD_PROGRAM) && (errno==EPIPE))
				{
					sprintf(msg.message,"printer [%s]: command failed",
						qitem->printer->printer_dev);
				}
				else
				{
					sprintf(msg.message,"error writing printer %s: %s (errno=%d)",qitem->printer->printer_dev,
						sys_errlist[errno],errno);

					write(2,msg.message,strlen(msg.message));
					close(2);
				}
				write(wrt,(char*)&msg,sizeof(UMSG));
				have_job=FALSE;
				close(2);
				goto top_q_unspool;
			}
		}
		}
#if 0
	} /* to trick the emacs c formatter */
#endif	
	
#ifdef DEBUGCODE
	D(DBGCHILD)
	{
		logerr("UNSP %s: finished r/w loop\n",self);
	}
#endif
	
	
	if (qitem->fitem->binary)
	  fclose(in);
	else
	  pclose(in);
#ifdef DEBUGCODE
	D(DBGCHILD)
	{
		logerr("UNSP %s: closed input stream\n",self);
	}
#endif
	
	if ( -- qitem->copies) 
	{
		if (FFBETWEEN(qitem))
		{
			write(outfd,ff,1);
		}
		goto mult_copies;
	}
	
	if (FFAFTER(qitem))
	  write(outfd,ff,1);
	
	if (qitem->printer->mode & PMD_PROGRAM)
	  pcretstat = pclose(out);
	else
	{
#if defined(AIX) || defined(_AIX)
		if (ioctl(outfd,LPRSETA,&slp232) == -1)
		  logerr("ioctl LPRSETA failed: %s\n",sys_errlist[errno]);
		if (ioctl(outfd,LPRSET,&slpio) == -1)
		  logerr("ioctl LPRSET failed: %s\n",sys_errlist[errno]);
		if (ioctl(outfd,LPRMODS,&slpmod) == -1)
		  logerr("ioctl LPRMODS failed: %s\n",sys_errlist[errno]);
#endif
		fclose(out);
	}
#ifdef DEBUGCODE
	D(DBGCHILD)
	{
		logerr("UNSP %s: closed output stream\n",self);
	}
#endif
	
	/*	printer type=prog input file must jump here */
      skipioloop:
	
	if ((qitem->printer->mode & PMD_PROGRAM)
	    && WEXITSTATUS(pcretstat) )
	{
		msg.status=UMSG_ERR;
		sprintf(msg.message,"printer [%s]: (return code=%d)",qitem->printer->printer_dev,
			WEXITSTATUS(pcretstat));
		
		write(wrt,(char*)&msg,sizeof(UMSG));
		
#ifdef DEBUGCODE
		D(DBGUNSPOOLP)
		{
			sprintf(errmsg,"UNSPOOLER prtr:%s qid %d finished. sending ERROR message\n",
				qitem->printer->printer_name,qitem->q_id);
			logerr(errmsg,0);
		}
#endif
		
	}
	else
	{
		int st;
#ifdef DEBUGCODE
		D(DBGUNSPOOLP)
		{
			sprintf(errmsg,"UNSPOOLER prtr:%s qid %d finished. sending DONE message\n",
				qitem->printer->printer_name,qitem->q_id);
			logerr(errmsg,0);
		}
#endif
		
		msg.status=UMSG_DONE;
		st = write(wrt,(char*)&msg,sizeof(UMSG));
#ifdef DEBUGCODE
		D(DBGUNSPOOLP)
		{
			logerr("status of write was %d\n",st);
		}
#endif
	}
	
	have_job=FALSE;
	close(2);
	goto top_q_unspool;
}
void master_shutdown(int dummy)
{
	logerr(" Slave process handling printer %s has shut down.\n",prtname);
	exit(0);
}

void abortinfo(struct q_item *q, int fd)
{
	char buf[200];
	
	sprintf(buf,"\n\n **** Job %d  File %s	aborted at operator's request\n%c",q->q_id,q->real_path,12);
	write(fd,buf,strlen(buf));
}	
void addulist(int rfd, int wfd, int pid, struct p_item *printer, struct q_item *qitem)
{
	ULIST *new;
	char *gmem(int cnt, int sz);
	
	new=(ULIST*)gmem(1,sizeof(ULIST));
	new->rfd=rfd;
	new->wfd=wfd;
	new->pid=pid;
	new->printer=printer;
	printer->waitpid=pid;
	printer->qid=qitem->q_id;
	printer->unspooler = new;
	new->qitem=qitem;
	
	ring_add(unsplist,-1,(char*)&new);
}
char **banner(struct q_item *q)
{
	static char *bbuf[20];
	static char info1[200],info2[200],info3[200];
	char vbuf[200], vers[200];
	char *d_username(int uid),*ctime();
	int i=0;
	extern char VERSION[];
	int pos;
	time_t now,time();
	
	bbuf[i++]="********************************************************************************\n";
	bbuf[i++]=vbuf;
	bbuf[i++]="********************************************************************************\n";
	bbuf[i++]="\n";
	bbuf[i++]=info1;
	bbuf[i++]=info2;
	bbuf[i++]=info3;
	bbuf[i++]="\n";
	bbuf[i++]="********************************************************************************\n";
	bbuf[i++]="********************************************************************************\n";
	bbuf[i++]="********************************************************************************\n";
	bbuf[i]=NULL;
	
	sprintf(vers," IDSI Print Queue Version %s ",VERSION);
	strcpy(vbuf,"********************************************************************************\n");
	pos= (((int)strlen(vbuf))-1)/2 - ((int)strlen(vers))/2;
	memcpy(vbuf+pos,vers,strlen(vers));
	
	now=time(NULL);
	sprintf(info1," Job: %d	 User: %s  Printer: %s\n",q->q_id,d_username(q->owner_uid),q->printer->printer_name);
	info1[80]='\0';
	sprintf(info2," Path: %s      Spooled at: %s\n",q->orig_path,ctime(now));
	sprintf(info3," Printed at: %s\n",ctime(&(q->time_stamp)));
	return bbuf;
}
/* build the filter cmd, do substitution, etc */

void build_filtercmd(char *destcmd, char *usercmd, struct q_item *qitem, int plen)
{
	char tmp[200], *dosubst(char *cmd, struct q_item *qitem, int plen);
	
	sprintf(tmp,"cat %s",qitem->real_path);
	strcpy(destcmd,tmp);
	
	if (strlen(usercmd))
	{
		sprintf(tmp,"| %s",dosubst(usercmd,qitem,plen));
		strcat(destcmd,tmp);
	}
}
ULIST *find_unspooler(char *name)
{
	int upos,ucnt;
	ULIST *unsp;
	
	ring_count(unsplist,&ucnt);
	for (upos=0; upos<ucnt; ++upos)
	{
		ring_get(unsplist,upos,(char*)&unsp);
		
		if (!strcmp(unsp->printer->printer_name,name))
		{
			return unsp;
		}
	}
	if (upos == ucnt)
	{
		return NULL;
	}
}

/* dosubst() was moved to utils.c */
void check_unspoolers(void)
{
	ULIST *unsp;
	UMSG msg;
	int bytes;
	int deleted=FALSE;
	int ucnt,upos;
#ifdef DEBUGCODE
	char chklist[2000];
#endif
	time_t time(), curtime;
	
#ifndef NO_SELECT
	struct timeval to;
	fd_set rfds;
	int cnt;
	int maxfd;
	
	FD_ZERO(&rfds);
#endif
	ring_count(unsplist,&ucnt);
	if (ucnt==0)
	{
		return;									/* and spooling processes  */
	}
	upos = 0;
	(void)time(&curtime);
	
#ifndef NO_SELECT
	maxfd = -1;
	
	for (upos=0; upos<ucnt; ++upos)								/* setup fds for clients */
	{
		ring_get(unsplist,upos,(char*)&unsp);
		FD_SET(unsp->rfd,&rfds);
		if (unsp->rfd > maxfd)
		{
			maxfd =  unsp->rfd;
		}
	}
	to.tv_sec = 0;
	to.tv_usec = 100;
	
	/* FDSETCAST defined in defs.h */
	cnt = select(maxfd+1,FDSETCAST &rfds,NULL,NULL,&to);
	if (cnt== -1)
	{
		logerr("error on unsp select:%s\n",sys_errlist[errno]);
	}
	
#ifdef DEBUGCODE
	D(DBGCHKUNSP)
	{
		for (upos=0; upos<ucnt; ++upos)								/* setup fds for clients */
		{
			ring_get(unsplist,upos,(char*)&unsp);
			if (FD_ISSET(unsp->rfd,&rfds))
			{
				sprintf(errmsg,"unsp [%s:%d] has data\n",unsp->printer->printer_name,
					unsp->qitem?unsp->qitem->q_id:-1);
				logerr(errmsg,0);
				
			}
			else
			{
				sprintf(errmsg,"unsp [%s:%d] does not have data\n",unsp->printer->printer_name,
					unsp->qitem?unsp->qitem->q_id:-1);
				logerr(errmsg,0);
			}
		}
	}
#endif
	
#endif
	for (upos=0; upos<ucnt; )		/* this stuff should be moved to another module */
	{
		ring_get(unsplist,upos,(char*)&unsp);
#ifndef NO_SELECT
		if (FD_ISSET(unsp->rfd,&rfds))
		{
			FD_CLR(unsp->rfd,&rfds);
#ifdef DEBUGCODE
			D(DBGCHKUNSP)
			{
				sprintf(errmsg,"chk:unspooler %s qid %d has data\n",
					unsp->printer->printer_name,unsp->qitem?unsp->qitem->q_id:-1);
				logerr(errmsg,0);
			}
#endif
			
			bytes=read(unsp->rfd,(char*)&msg,sizeof(UMSG));
			
			if (bytes == -1)
			{
#ifdef DEBUGCODE
				D(DBGCHKUNSP)
				{
					sprintf(errmsg,"error reading unspooler #%d [q_id %d]: %s\n",
						upos,unsp->qitem?unsp->qitem->q_id:-1,sys_errlist[errno]);
				}
#endif
				
				++upos;
				continue;
			}
#ifdef DEBUGCODE
			D(DBGCHKUNSP)
			{
				sprintf(errmsg,"unspooler #%d [qid %d]: %d bytes: status %s aux %d message %s\n",
					upos,unsp->qitem?unsp->qitem->q_id:-1, bytes, unspcmds[msg.status], 
					msg.aux, strlen(msg.message)?msg.message:"(null)");
				logerr(errmsg,0);
			}
#endif
			
#else /* ifndef NOSELECT */
			if ((bytes=read(unsp->rfd,&msg,sizeof(UMSG)))!=0)
			{
#endif
#if 0
			} /* this is to trick the emacs c formatter */
#endif
#ifndef NO_SELECT
			if (bytes==0)
#else
			if (bytes== -1)
#endif
			{
				if (errno!=EAGAIN 
#ifdef EWOULDBLOCK
				    && errno!=EWOULDBLOCK
#endif
				    )
				{
					sprintf(errmsg,"unspooler process handling job %d appears to have aborted abnormally [errno=%d]\n",
					       unsp->qitem?unsp->qitem->q_id:-1,errno);
					logerr(errmsg,0);
					if (unsp->qitem)
					{
						sprintf(errmsg,"  file=%s, form=%s, class=%c, printer=%s\n",
							unsp->qitem->real_path,
							unsp->qitem->form,
							unsp->qitem->class?unsp->qitem->class:' ',
							unsp->qitem->actual_prtr);
						logerr(errmsg,0);
					}
					msg.status=0;
					unsp->printer->status &= ~PST_BUSY;
					if (unsp->qitem)
					{
						unsp->qitem->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
						unsp->qitem->status |= QST_HOLDING;
						unsp->qitem->mode |= QMD_HOLD;
						queue_modified();
					}
					pstatus_changed();
					msg.status = UMSG_EXITING;
				}
				else
				{
					continue;
				}
			}
			switch (msg.status)
			{
			      case UMSG_ERR:
			      {
				      logerr("message from unspooler process:\n%s\n",msg.message);
				      unsp->printer->status &= ~(PST_BUSY|PST_FORMCHANGE|QST_STOPPED);
				      unsp->qitem->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
				      if (msg.aux == EBUSY)
				      {
					      unsp->printer->status |= PST_OTHER_BUSY;
				      }
				      else
				      {
					      unsp->printer->status |= PST_ERROR;
				      }
				      unsp->qitem = NULL;
				      queue_modified();
				      pstatus_changed();
				      break;
			      }
			    case UMSG_DONE:
			    {
				    finished_unspooler(unsp);
				    break;
			    }
			    case UMSG_UNUNSPOOL:
			    {
				    time_t time();
				    
				    unsp->printer->status &= ~(PST_BUSY|PST_FORMCHANGE|QST_STOPPED);
				    unsp->qitem->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
				    unsp->qitem->actual_prtr[0]=(char)0;
				    unsp->qitem = NULL;
				    pstatus_changed();
				    (void)time(&unsp->idlesince);
				    break;
			    }
			    case UMSG_EXITING:
			    {
				    remove_unspooler(unsp,1);
				    deleted=TRUE;
				    --ucnt;
				    break;
			    }
			    case UMSG_NEWFORM: 
			    {
				int qpos;
				QITEM *qp, qi, *q;
				PITEM *p = unsp->printer;
				
				strncpy(p->current_form,msg.message,FORMNAMEMAX-1);
				
				p->status &= ~PST_FORMCHANGE;
				pstatus_changed();
				qp = &qi;
				qp->q_id = p->qid;
				ring_find(queuelist,(char*)&qp,&qpos,(char*)&q);
				q->status &= ~QST_CHKPRT;
				queue_modified();
				break;
			    }
		      }
		      queue_modified();
		}
		else
		{
			deleted=FALSE;
		}
		
#ifdef DEBUGCODE
		D(DBGCHKUNSP)
		{
			sprintf(errmsg,"curtime %d, unsp %s idle since %d, diff %d\n",
				curtime,unsp->printer->printer_name,
				unsp->idlesince,	
				curtime- (unsp->idlesince));
			logerr(errmsg,0);
		}
#endif
		
		if (!deleted && !unsp->qitem && (curtime - unsp->idlesince > UNSPOOLER_PERSISTANCE) &&
		    !(unsp->status&UST_WAITINGSHUT))
		{
			int st;
			
			msg.status = UMSG_SHUTDOWN;
			broken_upipe=0;
			st=write(unsp->wfd,(char*)&msg,sizeof(UMSG));
			unsp->status |= UST_WAITINGSHUT;
#ifdef DEBUGCODE
			D(DBGCHKUNSP)
			{
				logerr("Send shutdown message, status %d\n",st);
			}
#endif
			
			if (broken_upipe) 
			{ 
				remove_unspooler(unsp,0);
				broken_upipe=FALSE; 
				deleted=TRUE;
				--ucnt;
				break; 
			}
		}
		
		if (!deleted)
		{
			++upos;
		}
	}
}
tag_unsp_obsolete(struct p_item *printer)
{
	UMSG msg;
	
	if (ISBUSY(printer))
	{
		printer->unspooler->status |= UST_OBSOLETE;
	}
	else
	{
		msg.status = UMSG_SHUTDOWN;
		write(printer->unspooler->wfd,(char*)&msg,sizeof(UMSG));
		printer->unspooler->status |= UST_WAITINGSHUT;
#ifdef DEBUGCODE
		D(DBGCHKUNSP)
		{
			logerr("Master shutting down obsolete unspooler %s\n",printer->printer_name);
		}
#endif
		
	}
}
shut_if_obsolete(struct unspooler_list *unsp)
{
	UMSG msg;
	
	if (unsp->status & UST_OBSOLETE)
	{
		msg.status = UMSG_SHUTDOWN;
		write(unsp->wfd,(char*)&msg,sizeof(UMSG));
		unsp->status |= UST_WAITINGSHUT;
#ifdef DEBUGCODE
		D(DBGCHKUNSP)
		{
			logerr("Master shutting down obsolete unspooler %s\n",unsp->printer->printer_name);
		}
#endif
		
	}	
}
/*
 * finished_unspooler: set the unspooler into an idle state
 *
 */
void finished_unspooler(UITEM *u)
{
	u->printer->status &= ~(PST_BUSY|PST_FORMCHANGE);
	pstatus_changed();
	if (!u->qitem)
	{
		sprintf(errmsg,"Warning: unspooler is idle [%s]\n",u->printer->printer_name);
	}
#ifdef DEBUGCODE
	D(DBGCHKUNSP)
	{
		sprintf(errmsg,"finished_unspooler: [%s:%d]\n",u->printer->printer_name,
			u->qitem->q_id);
		logerr(errmsg,0);
	}
#endif
	
	if (u->qitem->mode & QMD_DELETEAFTER)
	{
		if (unlink(u->qitem->real_path)<0) 
		{
			sprintf(errmsg,"Error: unlinking %s after print: %s\n",
				u->qitem->real_path,sys_errlist[errno]);
			logerr(errmsg,0);
		}
	}
	if (u->qitem->mode & QMD_RESPOOLHOLD)
	{
		u->qitem->mode = QMD_HOLD|QMD_RESPOOLHOLD;
		get_class_chars(u->qitem);
		u->qitem->status = QST_HOLDING;
		u->qitem = NULL;
	}
	else
	{
		int pos,st;
		
		st=ring_find(queuelist,(char*)&(u->qitem),&pos,(char*)NULL);
#ifdef DEBUGCODE
		D(DBGCHKUNSP)
		{
			sprintf(errmsg,"Removing q_item %d %s [st=%d]\n",u->qitem->q_id,u->qitem->real_path,st);
		        logerr(errmsg,0);
		}
#endif
		
		st=ring_remove(queuelist,pos,NULL);
#ifdef DEBUGCODE
		D(DBGCHKUNSP)
		{
			logerr("status of ring_remove: %d\n",st);
		}
#endif
		
		free(u->qitem);
		u->qitem = NULL;
	}
	
	(void)time(&u->idlesince);
}
/*
 * remove an idle unspooler
 * 
 */
remove_unspooler(punsp,clflag)
UITEM *punsp;
int clflag;
{
	int ucnt,upos,st;
	UITEM *unsp;
	
	ring_count(unsplist,&ucnt);
	for (upos=0; upos<ucnt; ++upos)
	{
		st=ring_get(unsplist,upos,(char*)&unsp);
		if (st!=0)
		{
			sprintf(errmsg,"Error removing unspooler: ucnt:%d, upos:%d, unsp:%d, st:%d\n",
				ucnt,upos,unsp,st);
			logerr(errmsg,0);
		}
		if (unsp==punsp)
		{
			break;
		}
	}
	if (upos == ucnt && unsp != punsp)
	{
		return FALSE;
	}
	if (clflag)
	{
		close(unsp->wfd);
		close(unsp->rfd);
	}
	unsp->printer->unspooler = NULL;
#ifdef DEBUGCODE
	D(DBGCHKUNSP)
	{
		logerr("removing unspooler %s\n",unsp->printer->printer_name);
	}
#endif
	
	ring_remove(unsplist,upos,NULL);
	return TRUE;
}
int getinitstr(struct f_item *fi, struct p_item *pi, char **str, int *len, int *type)
{
	init_s *init_p, *findinititem(struct f_item *fi, char *model);
	
	if ((init_p = findinititem(fi,pi->printer_model)))
	{
		*str = init_p->initdata;
		*len = init_p->initlen;
		*type = init_p->idatatype;
		return TRUE;
	}
	else
	{
		if (fi->initlen)
		{
			*str = fi->initdata;
			*len = fi->initlen;
			*type = fi->idatatype;
			return TRUE;
		}
		else
		{
			*str = NULL;
			*len = 0;
			return FALSE;
		}
	}
}

/******************************************************************/
int myfgets(char *buf, int cnt, FILE *file, int pagesize, int reset)
{
	static char localbuf[1024];
	static int lpos=0, end=0;
	
	static int curline = 0;
	static int eopage = 0;
	static int absline = 0;
	
	extern int pagecount;
	
	int pos, done;
	
	if (reset)
	{
		lpos=0; end=0;
		curline = 0;
		eopage = 0;
		absline = 0;
		return 0;
	}
	
	D(DBGCHILD2)
	{
		sprintf(errmsg,"in myfgets: pagesize=%d, lpos=%d, end=%d, curline=%d, pagecount=%d eopage=%s\n",
		       pagesize,lpos,end,curline,pagecount,eopage?"TRUE":"FALSE");
		logerr(errmsg,0);
	}
	
	for (done=pos=0;!done;)
	{
		if (end - lpos == 0)
		{
			lpos=0;
			memset(localbuf,0,sizeof(localbuf));
			end = fread(localbuf,1,1024,file);
			if (end==0)
			  return pos;
		}
		while (lpos < end)
		{
			buf[pos++] = localbuf[lpos++];
			
			if (localbuf[lpos-1] == '\f' && absline)
			{
				if (eopage)
				{
					--pos;
					eopage=FALSE;
					++pagecount;
					continue;
				}
				curline = 0;
				++pagecount;
				done=TRUE;
				break;
			}
			else if (eopage)
			{
				eopage=FALSE;
				++pagecount;
			}
			else if ((localbuf[lpos-1] == '\n')||(pos >= cnt))
			{
				++curline; ++absline;
				if (curline >= pagesize)
				{
					eopage=TRUE;
					curline = 0;
				}
				else
				  eopage=FALSE;
				done=TRUE;
				break;
			}
		}
	}
	return pos;
}

/*
 * $Log: d_unspool.c,v $
 * Revision 1.72  1993/11/10  23:13:27  jockc
 * declare time() in unspool() to avoid later complaint from SCO
 *
 * Revision 1.71  1993/11/10  17:39:39  jockc
 * enhanced banner and unspooler input=file mode
 *
 * Revision 1.70  1993/10/21  21:57:58  jockc
 * page range on formfeed slightly busted
 *
 * Revision 1.69  1993/10/15  22:18:30  jockc
 * small change for NO_SELECT machines.. unblocked reads return -1
 * and errno=EAGAIN.. didn't handle
 *
 * Revision 1.68  1993/10/12  23:21:23  jockc
 * cast strlen to int for sequent ansi.  declare curtime, time() outside
 * of ndef NO_SELECT where it belongs
 *
 * Revision 1.67  1993/09/30  16:56:29  jockc
 * check status of pipe() calls when starting slave
 *
 * Revision 1.66  1993/09/29  23:42:50  jockc
 * close status log at loop end
 *
 * Revision 1.65  1993/09/27  23:33:11  jockc
 * changed to use LP* ioctl calls on AIX.. need testing.
 *
 * Revision 1.64  1993/09/13  23:01:37  jockc
 * add printer name to unspooler ps line
 *
 * Revision 1.63  1993/09/13  15:28:15  jockc
 * 499
 * 503
 *
 * Revision 1.63  1993/09/10  18:35:21  jockc
 * changed myfgets to work better, improved page ranging.
 * revised check_unspoolers a bit.  daemon now decides when to
 * shut down idle unspoolers, and has a flag to indicate daemon
 * is waiting for the unspooler to comply and shut down.
 * stopped unspoolers using too much CPU fixed.  daemon/unspooler
 * communication fixed up, it was flakey in some places.
 * pstatus file for both type=program and type=device printers.
 *
 * Revision 1.62  1993/08/13  20:43:46  jockc
 * cast fdsets for HPUX to int *
 *
 * Revision 1.61  1993/08/13  18:48:04  jockc
 * improved unspooler stuff.... persistant spoolers were a little
 * flaky
 *
 * Revision 1.60  1993/06/03  00:41:39  jockc
 * slight change to formchange thingy to support persistant unspoolers
 *
 * Revision 1.59  1993/06/02  23:53:12  root
 * forgot to remove a debug stmt
 *
 * Revision 1.58  1993/06/02  23:37:47  jockc
 * change unspooler scheme : persist and unsplist change
 *
 * Revision 1.57  1993/05/28  21:45:06  jockc
 * revised list logic for printer, queue, and unspooler lists.
 * using ring.c now
 *
 * Revision 1.56  1993/04/27  17:27:17  jockc
 * added comments
 *
 * Revision 1.55  1993/03/26  00:04:43  jockc
 * improved unspooler handling
 *
 * Revision 1.54  1993/01/12  02:07:16  jockc
 * moved the nice (setpriority) into d_main.
 *
 * Revision 1.53  1993/01/04  19:42:40  jockc
 * check progname_ptr before sprintfing to it.. if FG is defined
 * in d_main progname_ptr could be NULL
 *
 * Revision 1.52  1992/12/31  23:44:50  jockc
 * qpaths/ISPOOL.  ffbetween added
 *
 * Revision 1.51  1992/11/13  21:30:00  jockc
 * added stuff for motorola
 *
 * Revision 1.50  1992/10/28  01:43:12  root
 * fixed status check of pclose printer program
 *
 * Revision 1.49  1992/10/27  23:24:49  root
 * added CREAD flag for cflag
 *
 * Revision 1.48  1992/10/09  20:16:34  jockc
 * fixed perm on pstatus file.  moved subst routine to utils
 * took out logerr on failed ioctl for AIX
 *
 * Revision 1.47  1992/08/10  20:42:26  jockc
 * added bzero and nice for ICL
 *
 * Revision 1.46  1992/08/06  18:27:04  jockc
 * fixed lost last line if no '\n' trailing
 *
 * Revision 1.45  1992/08/06  18:17:59  jockc
 * revised scheme for page counting.
 *
 * Revision 1.44  1992/08/04  00:21:54  jockc
 * added support for ioctl set of comm params (bits stop parity)
 * changed read/write loop to exit of pcount exeeds max page range
 * to save time on selected page printouts
 *
 * Revision 1.43  1992/07/14  20:45:23  jockc
 * changed fclose back to pclose.  pclose failed
 * because SIGCLD ignored on SYSVWAIT systems.  fixed by
 * SIG_DFL'ing SIGCLD.  also check for failed fork call.
 *
 * Revision 1.42  1992/07/02  18:24:30  jockc
 * changed all pcloses to fcloses (cross fingers)
 *
 * Revision 1.41  1992/07/01  17:38:05  jockc
 * had to reset lpos in myfgets to zero for subsequent
 * usage of that function on another file
 *
 * Revision 1.40  1992/06/29  21:50:27  jockc
 * changed err log to use sys_errlist, added ioctl for
 * rs6000 plot mode
 *
 * Revision 1.39  1992/06/25  20:03:42  jockc
 * watch for formfeed in page counting
 *
 * Revision 1.38  1992/06/23  23:09:53  jockc
 * fixup null in record behavior
 *
 * Revision 1.37  1992/06/09  23:14:48  jockc
 * posix fix
 *
 * Revision 1.36  1992/06/09  00:05:08  jockc
 * *** empty log message ***
 *
 * Revision 1.35  1992/06/08  23:55:56  jockc
 * polish code to remove some warnings
 *
 * Revision 1.34  1992/06/04  22:38:56  jockc
 * sleep on unblocked read of daemon pipe, to prevent tight loops
 *
 * Revision 1.33  1992/05/20  23:15:49  jockc
 * cleaned up some debugging stuff.
 * fixed situation that occurs when user specifies
 * bad or no form, printer has as default a form
 * that does not exist
 *
 * Revision 1.32  1992/05/07  23:08:21  jockc
 * more info on failed printer open
 * don't choke on print files with nulls in them
 *
 * Revision 1.31  1992/05/07  22:41:46  jockc
 * fixes for ncr
 *
 * Revision 1.30  1992/04/30  18:55:17  jockc
 * clear stopped bit on ununspool, add code for filters-per-model
 *
 * Revision 1.29  1992/04/21  17:05:56  jockc
 * add set of ixon to port setting
 *
 * Revision 1.28  1992/04/16  21:06:19  jockc
 * use fclose instead of pclose on aix, also ff after abort message
 *
 * Revision 1.27  1992/04/02  01:17:22  jockc
 * changed to fclose or pclose depending, also
 * different copies behavior
 *
 * Revision 1.26  1992/03/30  23:47:16  jockc
 * stupid mistake mixing fwrites with write fixed
 *
 * Revision 1.25  1992/03/27  23:00:10  jockc
 * change popen status name back to pname not pname.status
 * chopped out the logerrs from the unspool write loop that
 * I forgot to take out before
 *
 * Revision 1.24  1992/03/27  22:55:04  jockc
 * added init = filename support for printer init,
 * added support for busy (in use by other non-idsi software)
 * printers.  capture of stderr from output popen to pstatus dir
 *
 * Revision 1.23  1992/03/24  00:55:32  jockc
 * setup u3b2 to use nice()
 *
 * Revision 1.22  1992/03/24  00:48:03  jockc
 * fixup no_select problem related to precedence
 *
 * Revision 1.21  1992/03/13  23:04:27  jockc
 * allow 19200 baud
 *
 * Revision 1.20  1992/03/13  21:31:39  jockc
 * if no form specified, use first form in formdef
 * revamped unspool loop.. now use popen for input filter,
 * popen for output to device or program.. out is FILE*
 * support for page range now provided here, not selectpg
 * program.  ff between banner and job.  check banner, ff
 * flag in qitem, not class list.  handle forms that specify
 * binary file print.  print id and version in header
 *
 * Revision 1.19  1992/02/13  23:34:53	jockc
 * support for no select(), fixed forms flakiness, handle unspoolers
 * that abort, increased timeout on open
 *
 * Revision 1.18  1992/01/06  19:37:44	jockc
 * if printer has no classes, give it a " " class. otherwise
 * loop terminates, disallowing print my number or name on a
 * printer with no class
 *
 * Revision 1.17  1991/10/12  00:04:11	jockc
 * moved set of unspooling to after fork
 *
 * Revision 1.16  1991/10/10  22:39:35	jockc
 * check to see if prt enabled before using
 *
 * Revision 1.15  1991/10/08  23:33:06	jockc
 * flag when unspooling
 *
 * Revision 1.14  1991/10/08  23:26:38	jockc
 * limited support for initstr per model
 *
 * Revision 1.13  1991/10/07  18:25:51	jockc
 * allow hold of stopped job
 *
 * Revision 1.12  1991/10/07  17:44:25	jockc
 * slightly off default form change behavior fixed
 *
 * Revision 1.11  1991/10/01  17:07:25	jockc
 * moved unspooler processing here from d_comm
 * better form handling.   ff control by class added.
 * forms have stock type
 *
 * Revision 1.10  1991/09/12  03:24:18	jockc
 * added forms handling code
 *
 * Revision 1.9	 1991/08/23  18:40:05  jockc
 * properly handle baudrate and also set word size
 *
 * Revision 1.8	 1991/08/05  23:02:01  jockc
 * added printer number logic
 *
 * Revision 1.7	 1991/07/22  23:01:33  jockc
 * added support for timeout on printer open, and check of printer status
 * for error bit
 *
 * Revision 1.6	 1991/05/22  17:06:09  jockc
 * for sco, use nice() instead of setpri...
 *
 * Revision 1.5	 1991/05/14  22:40:37  jockc
 * moved goto top (for mult copies) to after
 * close(printer) and fclose(input file)...
 * 2nd open on printer dev was failing due
 * to already open... only 1st copy printed
 *
 * Revision 1.4	 1991/05/03  17:49:54  jockc
 * include/exclude banner page based on class
 *
 * Revision 1.3	 1991/04/23  18:57:27  jockc
 * handle multiple copies
 *
 * Revision 1.2	 1991/04/19  00:47:23  jockc
 * *** empty log message ***
 *
 * Revision 1.1	 1991/04/18  23:49:49  jockc
 * Initial revision
 *
 *
 */
