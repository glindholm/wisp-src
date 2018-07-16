/*
 * Module:  daemon_unspool
 * Program: IDSIprint
 * Purpose: routines for unspooling
 *
 * $Log: d_unspool.c,v $
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
static char rcsid[] = "$Id:$";

#define _DAEMON_UNSPOOL_
#define EXT extern 
#include "daemon.h"

#ifdef DBGFILT
FILE *test;
#endif

extern char *sys_errlist[];

unspool()
{
	QITEM *q, *findrdyqitem();
	char *p;
	FITEM *findfitem();
	
	if (!ready_head) return;
	
	for (p_ptr=p_head; p_ptr; p_ptr=p_ptr->next)
	{
#ifdef DBGUNSPOOL
		logerr("printer %s\n",p_ptr->data->printer_name);
#endif		
		if (!ISIDLE(p_ptr->data) || ISERROR(p_ptr->data) || !ISENABLED(p_ptr->data) || 
		    ISINUSEBYOTHER(p_ptr->data))
		{
			continue;
		}
		if (strlen(p_ptr->data->class)==0) strcpy(p_ptr->data->class," ");
		for (p=p_ptr->data->class; *p ; ++p)
		{
			if (q=findrdyqitem(*p,p_ptr->data->printer_name,p_ptr->data->prtnum)) 
			{
#ifdef DBGUNSPOOL
				logerr("\tprinting %s\n",q->orig_path);
#endif		
				
				q->printer = p_ptr->data;
				q->status |= QST_UNSPOOLED;
				q->status &= ~QST_READY;
				if (!q->fitem) 
				{
					if (strlen(p_ptr->data->default_form))
					{
						q->fitem = findfitem(p_ptr->data->default_form);
						if (q->fitem)
						  strcpy(q->form,p_ptr->data->default_form);
						else
						{
							if (f_head)
							{
								q->fitem = f_head->data;
								strcpy(q->form,f_head->data->form_name);
							}
						}
					}
					else
					{
						q->fitem = f_head->data;
						strcpy(q->form,f_head->data->form_name);
					}
				}
				if (q->fitem) 
				{
					if (fitemp=findfitem(p_ptr->data->current_form))
					{
						if (strcmp(q->fitem->stock,fitemp->stock))
						{
							q->printer->status |= PST_FORMCHANGE;
							q->status |= QST_CHKPRT;
							queue_modified();
						}
						else
						{
							strcpy(p_ptr->data->current_form,q->fitem->form_name);
						}
					}
					else
					{
						q->printer->status |= PST_FORMCHANGE;
						q->status |= QST_CHKPRT;
						queue_modified();
					}
				}

				q->printer->status |= PST_BUSY;
				pstatus_changed();
				queue_modified();
				if (q_unspool(q) == FALSE)
				{
					q->printer->status &= ~(PST_BUSY|PST_FORMCHANGE|QST_STOPPED);
					q->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
				}
				break;
			}
#ifdef DBGUNSPOOL
			else
			  logerr("\tnothing in class %c\n",*p);
#endif
			
		}
		
	}
	
#if 0	
	for(r_ptr=ready_head;r_ptr; )
	{
		q_unspool(r_ptr->data);							/* unspool the item */
		r_ptr->data->status |=QST_UNSPOOLED;					/* tag it unspooled */
		tmp=r_ptr;
		NEXT(r_ptr);
		free(tmp);
	}
	ready_head=NULL;
#endif
}
int open_timeout;
void opento(sig)
int sig;
{
	++open_timeout;
}
q_unspool(qitem)
QITEM *qitem;
{

	int pid, pipe1[2], pipe2[2];
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
	char *dosubst();
	FITEM *findfitem();
	int pagesel;
	filt_s *filt_p, *findfiltitem(); /* use for model lookup */
	char ff[2];
	char *gmem();
	void opento();
	int myfgets();

	UMSG msg;

#if defined(_AIX) || defined(AIX)
	struct lprmod lpmod;
#endif

	ff[0]=(char)12;
	ff[1]=(char)0;
	/* check of class removed hear, class checked on queue rcpt and change in d_main */
	
	pipe(pipe1);		/* pipe for child->parent communication */
	pipe(pipe2);		/* pipe for parent->child communication */
	
#ifdef NO_SELECT
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
#endif
	wrt=pipe1[1];
	rd =pipe2[0];

	pid=fork();
	if (pid == -1)
	{
		char errbuf[200];
		
		sprintf(errbuf,"Panic: Could not fork: %s (errno=%d)\n",
			sys_errlist[errno],errno);
		logerr(errbuf,0);
		close(pipe1[0]); close(pipe1[1]);
		close(pipe2[0]); close(pipe2[1]);
		return FALSE;
	}
	if (pid)
	{
		close(wrt);
		close(rd);
		addulist(pipe1[0],pipe2[1],pid,qitem->printer,qitem);
		return TRUE;
	}
	
	signal(SIGCHLD,SIG_DFL);

	unspooling=TRUE;	
#ifdef NICEPS
	strcpy(progname_ptr,DUNSPNAME);
#endif	
#ifdef DBGCHILD
	logerr("child after fork\n",0);
#endif
#if defined(i386) || defined(hpux) || defined(u3b2)
	nice(5);
#else
	setpriority(PRIO_PROCESS,getpid(),5);
#endif

#ifdef DBGCHILD
	if (WRONGFORM(qitem->printer)) logerr("need a formchange\n",0);
#endif

      top_q_unspool:

	aligning=FALSE;
	
	if (WRONGFORM(qitem->printer))
	{
		while (read(rd,&msg,sizeof(UMSG))<=0) 
		  sleep(2);
		
		if (msg.status==UMSG_ALIGN) aligning=TRUE;
		if (msg.status==UMSG_FORMCHANGED)
		{
			qitem->printer->status &= ~PST_FORMCHANGE;
			msg.status = UMSG_NEWFORM;
			strcpy(msg.message,qitem->fitem->form_name);
			write(wrt,&msg,sizeof(UMSG));
#ifdef DBGCHILD
			logerr("got it\n",0);
#endif
			
		}
		if (msg.status==UMSG_ABORT)
		{
			msg.status=UMSG_UNUNSPOOL;
			write(wrt,&msg,sizeof(UMSG));
#ifdef DBGCHILD
			logerr("got abort instead\n",0);
#endif
			exit(0);
		}
	}

	if (qitem->printer->mode & PMD_PROGRAM)
	{
		int x;
		char statname[200];
#ifdef DBGCHILD
		logerr("printer is a program\n");
#endif		
		strcpy(ofilter,dosubst(qitem->printer->printer_dev,qitem,qitem->fitem->lpp));
#ifdef DBGCHILD
		logerr("ofilter is [%s]\n",ofilter);
#endif		
		sprintf(statname,"%s/%s",PSTATUSLOGS,qitem->printer->printer_name);
		x=open(statname,O_WRONLY|O_CREAT);
		close(2);
		dup2(x,2);
		out=popen(ofilter,"w");
		outfd=fileno(out);
#ifdef DBGCHILD
		logerr("pipe opened %x\n",out);
		logerr("fd is %d\n",outfd);
		logerr("status file is %s\n",statname);
#endif		
	}
	else
	{
		signal(SIGALRM,opento);
		open_timeout=0;
		alarm(20);
		outfd=open(qitem->printer->printer_dev,O_WRONLY|O_CREAT);
		save_errno=errno;
		alarm(0);
		if( outfd == -1 || open_timeout)
		{
			msg.status=UMSG_ERR;
			msg.aux=save_errno;
			sprintf(msg.message,"error opening printer %s: %s (errno=%d)",qitem->printer->printer_dev,
				sys_errlist[save_errno],save_errno);
			write(wrt,&msg,sizeof(UMSG));
			exit(0);
		}
		out=fdopen(outfd,"w");
	}
	if (isatty(outfd))
	{
		int baud, sbit, dbit, par;
		
		if (ioctl(outfd,TCGETA,&p)== -1)	
		  logerr("ioctl TCGETA failed: %s\n",sys_errlist[errno]);
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

		p.c_cflag= (baud|dbit|sbit|par);
 
		if (ioctl(outfd,TCSETA,&p)== -1) 
		  logerr("ioctl TCSETA failed: %s\n",sys_errlist[errno]);

#if defined(_AIX) || defined(AIX)
		lpmod.modes = PLOT;
		ioctl(outfd,LPRMODS,&lpmod);
#endif
	}

	pagesel=FALSE;
#ifdef DBGCHILD
	logerr("getting form info. form is %s\n",qitem->form);
	logerr("fitem is %x\n",qitem->fitem);
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
					
					sprintf(initfilename,"%s/%s",INITDIR,the_initstr);
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
			fitemp = qitem->fitem = (FITEM*)gmem(sizeof(FITEM));
			fitemp->binary=0;
			fitemp->lpp=66;
			strcpy(usersfilter,"cat");
		}
	}
	if (!aligning)
	{
		char **tmp,**banner();
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
	  in=fopen(qitem->real_path,"r");
	else
	  in=popen(ifilter,"r");
#ifdef DBGFILT
	logerr("ifilter is %s\n",ifilter);
	logerr("in is %x\n",in);
	logerr("ofilter is %s\n",ofilter);
	logerr("out is %x\n",out);
	logerr("type is %s\n",qitem->fitem->binary?"binary":"text");
#endif	

#ifdef DBGCHILD
	logerr("in is %x\n",in);

#endif
	if( !in )
	{
		msg.status=UMSG_ERR;
		sprintf(msg.message,"can't open %s",qitem->real_path);
		write(wrt,&msg,sizeof(UMSG));
		exit(0);
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
		fclose(out);
		close(outfd);
		fclose(in);
		goto top_q_unspool;
	}
	
	stopped=FALSE;

	for (pagecount=1;;)
	{
#ifndef NO_SELECT
		FD_ZERO(&rdfds);
		FD_ZERO(&wrfds);
		FD_SET(outfd,&wrfds);
		FD_SET(rd,&rdfds);

		cnt = select(64,&rdfds,&wrfds,0,0);
		if (FD_ISSET(rd,&rdfds))
		{
			read(rd,&msg,sizeof(UMSG));
#else
		if (read(rd,&msg,sizeof(UMSG))>0)
		{
#endif				
			switch(msg.status)
			{
			      case UMSG_ABORT:
#ifdef DBGCHILD
				printf("\t  got abort\n");
#endif				
				msg.status=UMSG_DONE;
				write(wrt,&msg,sizeof(UMSG));
				abortinfo(qitem,outfd);
				exit(0);
				break;
			      case UMSG_STOP:
#ifdef DBGCHILD
				printf("\t  got stop\n");
#endif				
				stopped=TRUE;
				break;
			      case UMSG_START:
#ifdef DBGCHILD
				printf("\t  got start\n");
#endif				
				stopped=FALSE;
				break;
			      case UMSG_HOLD:
				msg.status=UMSG_UNUNSPOOL;
				write(wrt,&msg,sizeof(UMSG));
				abortinfo(qitem,outfd);
				exit(0);
				break;
			}
		}
#ifndef NO_SELECT
		else if (FD_ISSET(outfd,&wrfds) && !stopped)
#else
		else if (!stopped)
#endif
		{
			int len,se;
			char foo[200];
			
			if (qitem->fitem->binary)
			{
				len=fread(inbuf,1,2048,in);
			}
			else
			{
				len=myfgets(inbuf,2048,in,qitem->fitem->lpp);
			}
			if (len==0) break;
			if (pagesel && !(qitem->fitem->binary))
			{
				if (pagecount > qitem->endpage)
				  break;
				
				if (!(pagecount >= qitem->stpage && pagecount <= qitem->endpage))
				  continue;
			}
			x=write(outfd,inbuf,len);
			if (x <= 0 && len)
			{
				msg.status=UMSG_ERR;
				sprintf(msg.message,"error writing printer %s: %s (errno=%d)",qitem->printer->printer_dev,
					sys_errlist[errno],errno);
				write(wrt,&msg,sizeof(UMSG));
				exit(1);
			}
		}
	}

	if (FFAFTER(qitem))
	  write(outfd,ff,1);

	if (qitem->fitem->binary)
	  fclose(in);
	else
	  pclose(in);

	if ( -- qitem->copies) 
	  goto mult_copies;

	if (qitem->printer->mode & PMD_PROGRAM)
	  pclose(out);
	else
	  fclose(out);

	fclose(in);

	msg.status=UMSG_DONE;
	write(wrt,&msg,sizeof(UMSG));

	exit(0);
}
abortinfo(q,fd)
QITEM *q;
int fd;
{
	char buf[200];
	
	sprintf(buf,"\n\n **** Job %d  File %s	aborted at operator's request\n%c",q->q_id,q->real_path,12);
	write(fd,buf,strlen(buf));
}	
openprinter(name)
char *name;
{
}
addulist(rfd,wfd,pid,printer,qitem)
int rfd,wfd,pid;
PITEM *printer;
QITEM *qitem;
{
	char *gmem();
	
	if (u_head==NULL)
	{
		u_head=(ULIST*)gmem(sizeof(ULIST));
		u_ptr=u_head;
	}
	else
	{
		for(u_ptr=u_head; u_ptr->next; NEXT(u_ptr));
		u_ptr->next=(ULIST*)gmem(sizeof(ULIST));
		NEXT(u_ptr);
	}
	u_ptr->next=NULL;
	u_ptr->rfd=rfd;
	u_ptr->wfd=wfd;
	u_ptr->pid=pid;
	u_ptr->printer=printer;
	printer->waitpid=pid;
	printer->qid=qitem->q_id;
	printer->unspooler = u_ptr;
	u_ptr->qitem=qitem;
}
char **banner(q)
QITEM *q;
{
	static char *bbuf[20];
	static char info1[200],info2[200],info3[200];
	char vbuf[200], vers[200];
	static char *username();
	int i=0;
	extern char VERSION[];
	char *gmem();
	
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
	memcpy(vbuf+((strlen(vbuf)-1)/2)-(strlen(vers)/2),vers,strlen(vers));

	sprintf(info1," Job: %d	 User: %s  \n",q->q_id,username(q->owner_uid));
	info1[80]='\0';
	sprintf(info2," Path: %s\n",q->orig_path);
	strcpy(info3,"\n");
	return bbuf;
}
static char *username(uid)
int uid;
{
	struct passwd *pw, *getpwuid();
	
	pw=getpwuid(uid);
	return pw->pw_name;
	
}
/* build the filter cmd, do substitution, etc */

build_filtercmd(destcmd,usercmd,qitem,plen)
char destcmd[];
char *usercmd;
QITEM *qitem;
int plen;
{
	char tmp[200], *dosubst();
	
	sprintf(tmp,"cat %s",qitem->real_path);
	strcpy(destcmd,tmp);
	
	if (strlen(usercmd))
	{
		sprintf(tmp,"| %s",dosubst(usercmd,qitem,plen));
		strcat(destcmd,tmp);
	}
}
char *dosubst(cmd,qitem,plen)
char *cmd;
QITEM *qitem;
int plen;
{
	static char buf[200];
	int backslash, dollar;
	int pos;
	static char *username();

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
			      case 'n':
			      case 'N':
				strcat(&buf[pos],qitem->orig_path);
				break;
			      case 'l':
			      case 'L':
				sprintf(&buf[pos],"%d",plen);
				break;
			      case 'u':
			      case 'U':
				strcat(&buf[pos],username(qitem->owner_uid));
				break;
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
	return buf;
	
}	
check_unspoolers()
{
	ULIST *u_prev;
	UMSG msg;
	int bytes;
#ifndef NO_SELECT
	struct timeval to;
	fd_set rfds;
	
	FD_ZERO(&rfds);
#endif
	if ((u_ptr = u_head)==NULL)
	  return;									/* and spooling processes  */
#ifndef NO_SELECT
	while (u_ptr)									/* setup fds for clients */
	{
		FD_SET(u_ptr->rfd,&rfds);
		NEXT(u_ptr);
	}
	to.tv_sec = 0;
	to.tv_usec = 0;

	select(20,&rfds,NULL,NULL,&to);
#endif
	u_prev=NULL;
	u_ptr = u_head;
	while (u_ptr)		/* this stuff should be moved to another module */
	{
#ifndef NO_SELECT
		if (FD_ISSET(u_ptr->rfd,&rfds))
		{
			bytes=read(u_ptr->rfd,&msg,sizeof(UMSG));
#else
		if ((bytes=read(u_ptr->rfd,&msg,sizeof(UMSG)))!=0)
		{
#endif
#ifndef NO_SELECT
			if (bytes==0)
#else
			if (bytes== -1)
#endif
			{
				char errbuf[200];
				logerr("unspooler process handling job %d appears to have aborted abnormally\n",
				       u_ptr->qitem->q_id);
				sprintf(errbuf,"  file=%s, form=%s, class=%c, printer=%s\n",
					u_ptr->qitem->real_path,
					u_ptr->qitem->form,
					u_ptr->qitem->class?u_ptr->qitem->class:' ',
					u_ptr->qitem->actual_prtr);
				logerr(errbuf,0);
				msg.status=0;
				u_ptr->printer->status &= ~PST_BUSY;
				u_ptr->qitem->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
				u_ptr->qitem->status |= QST_HOLDING;
				u_ptr->qitem->mode |= QMD_HOLD;
				queue_modified();
				pstatus_changed();
			}
			else if (msg.status == UMSG_ERR)
			{
				logerr("message from unspooler process:\n%s\n",msg.message);
				msg.status=UMSG_UNUNSPOOL;
				if (msg.aux == EBUSY)
				{
					u_ptr->printer->status |= PST_OTHER_BUSY;
					u_ptr->printer->status &= ~PST_BUSY;
				}
				else
				{
					u_ptr->printer->status |= PST_ERROR;
					u_ptr->printer->status &= ~PST_BUSY;
				}
				u_ptr->qitem->status &= ~(QST_UNSPOOLED|QST_READY|QST_CHKPRT|QST_STOPPED);
#if 0
				u_ptr->qitem->status |= QST_HOLDING;
				u_ptr->qitem->mode |= QMD_HOLD;
#endif
				queue_modified();
				pstatus_changed();
			}
			else if (msg.status == UMSG_DONE)
			{
				finished_unspooler(u_ptr);
			}
			else if (msg.status == UMSG_UNUNSPOOL)
			{
				u_ptr->printer->status &= ~(PST_BUSY|PST_FORMCHANGE|QST_STOPPED);
				pstatus_changed();
			}
			
			close(u_ptr->wfd);
			close(u_ptr->rfd);
			if (u_prev)
			{
				u_prev->next = u_ptr->next;
				free(u_ptr);
			}
			else
			{
				u_head = u_ptr->next;
				free(u_ptr);
			}
			queue_modified();
			return -1;
		}
		u_prev=u_ptr;
		NEXT(u_ptr);
	}
}
finished_unspooler(u)
ULIST *u;
{
	u->printer->status &= ~(PST_BUSY|PST_FORMCHANGE);
	pstatus_changed();
	if (u->qitem->mode & QMD_DELETEAFTER)
	{
		if (unlink(u->qitem->real_path)<0) perror("unlink");
	}
	if ((u->qitem->mode & QMD_RESPOOLHOLD)==0)
	{
		for (q_ptr=q_head; q_ptr; q_ptr=q_ptr->next)
		{
			QLIST *n,*p;
			
			if (u->qitem == q_ptr->data)
			{
				n = q_ptr->next;
				p = q_ptr->prev;
				if (q_ptr==q_head) q_head=n;
				if (q_ptr==q_tail) q_tail=p;
				if (p) p->next = q_ptr->next;
				if (n) n->prev = q_ptr->prev;
				
				free(q_ptr->data);
				free(q_ptr);
				
			}
		}
	}
	else
	{
		u->qitem->mode = QMD_HOLD|QMD_RESPOOLHOLD;
		get_class_chars(u->qitem);
		u->qitem->status = QST_HOLDING;
	}
}
getinitstr(fi,pi,str,len,type)
FITEM *fi;
PITEM *pi;
char **str;
int *len,*type;
{
	init_s *init_p, *findinititem();

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
int myfgets(buf,cnt,file,pagesize)
char buf[];
int cnt,pagesize;
FILE *file;
{
	static char localbuf[1024];
	static int lpos=0, end=0;

	static int curline = 0;
	static int eopage = 0;

	extern int pagecount;

	int pos, done;
	
	
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
			
			if (localbuf[lpos-1] == '\f')
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
			else if ((localbuf[lpos-1] == '\n')||(pos >= cnt))
			{
				++curline;
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
			else if (eopage)
			{
				eopage=FALSE;
				++pagecount;
			}
		}
	}
	return pos;
}
