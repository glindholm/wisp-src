/*
 * Module:  ipman_main
 * Program: IDSIprint ipman
 *
 * $Log: man_misc.c,v $
 * Revision 1.28  1992/07/30  22:25:55  jockc
 * swap vstate and vrefresh for ibm3151
 *
 * Revision 1.27  1992/07/14  21:36:20  jockc
 * if user did not change the pos of the qitem, pass
 * idaemon a -1 there
 *
 * Revision 1.26  1992/07/02  18:18:03  jockc
 * allow disable of busy printer
 *
 * Revision 1.25  1992/06/29  21:51:44  jockc
 * added a vstate 0 after display of file
 *
 * Revision 1.24  1992/06/22  22:23:12  jockc
 * fixed disappearing 15
 *
 * Revision 1.23  1992/06/08  23:56:24  jockc
 * polish code to remove some warnings
 *
 * Revision 1.22  1992/05/20  23:46:04  jockc
 * slight fix of incompat message.
 *
 * Revision 1.20  1992/05/07  22:28:38  jockc
 * root is automatically an operator
 * init edmaxarray
 *
 * Revision 1.19  1992/04/30  19:02:15  jockc
 * add operator checking, fixed mem leek, err msg if display failed
 * not allow to delete if no perms.  void cast in signal fn
 *
 * Revision 1.18  1992/03/27  23:15:53  jockc
 * check validity of pos_on_screen before defer'ing first printer
 * pointer (was crashing if no printers defined), cursor back
 * on after display
 * allow user to enter 0 for pnum
 *
 * Revision 1.17  1992/02/13  23:33:43  jockc
 * added support for machines without select() and fixed funny
 * behavior when not editing and hit the return key
 *
 * Revision 1.16  1991/12/17  22:35:11  jockc
 * fix changed printer num/name thing
 *
 * Revision 1.15  1991/10/11  00:32:25  jockc
 * add size to send_daemon call
 *
 * Revision 1.14  1991/10/10  22:42:46  jockc
 * on display wait on right pid now
 * enable/disable stuff added, f14 removed from
 * prt edit
 *
 * Revision 1.11  1991/10/08  23:28:17  jockc
 * take out init of users_or_all
 *
 * Revision 1.10  1991/10/07  17:43:34  jockc
 * don't allow delete or remove twice
 *
 * Revision 1.9  1991/10/04  21:02:19  jockc
 * prevent weirdness when loads called twice
 *
 * Revision 1.8  1991/10/01  17:12:39  jockc
 * stuff for reading dump files, new comm , monitor
 * code removed, wait_input redone, etc
 *
 * Revision 1.7  1991/08/06  22:25:47  jockc
 * use real_path instead of orig_path when displaying file
 *
 * Revision 1.6  1991/08/02  23:53:35  jockc
 * added display and showall/mine keys, functionality
 * turn off monitor during edit
 * added display file code
 *
 * Revision 1.5  1991/07/22  23:04:49  jockc
 * added function key to clear printer error
 * added code to save item pos before rcving
 * incoming stuff from daemon
 *
 * Revision 1.4  1991/05/13  18:11:56  jec
 * fix reversed pointers in memcpy moddate
 *
 * Revision 1.3  1991/04/30  23:55:04  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/30  23:41:06  jockc
 * needed def EXT extern
 *
 * Revision 1.1  1991/04/30  23:37:08  jockc
 * Initial revision
 *
 * Revision 1.4  1991/04/25  21:27:11  jockc
 * improved tcp init error handling
 *
 * Revision 1.3  1991/04/19  00:47:29  jockc
 * *** empty log message ***
 *
 * Revision 1.2  1991/04/19  00:22:50  jockc
 * *** empty log message ***
 *
 * Revision 1.1  1991/04/18  23:51:49  jockc
 * Initial revision
 *
 *
 */
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#define  _IPMAN_MAIN_

#include "c_comm.h"
#include "manage.h"


init_me()
{
	char *gmem();
	FKEY_ACTION *wp, key;
	int cangotop(), cangobot(), cangoup(), cangodn();
	char *fmtqitem(), *fmtpitem();
	int doup(), dodn(), dotop(), dobot(), doholdrel(), dodel(), togglemode(),
	    dorem(), doendis(), dochgfrm(), doalign(), dopup(), dopdn(), dostopstart(),
	    doedit(), notedit(), stopedit(), shalign(), prterrp(), doclrerr(), candisp(), dodisp();
	int toggleshow();
	char *type;
	
	make_vers_date(rcsid,VERSION,MODDATE);

#ifdef SOCKCODE
	type="<BSD>";
#endif
#ifdef MQCODE
	type="<SYSV>";
#endif	
	sprintf(vers_info,"V%s %s",VERSION,type);

	if (chk_vers(ALLVERS)==INCOMPAT_VERS)
	{
		fprintf(stderr,"Incompatible version.  This version of ilpman (%s) is\n",VERSION);
		fprintf(stderr,"not compatible with the currently running version of idaemon\n");
		exit(2);
	}

	load_qdump();
	load_pdump();
	
	initscr();
	move(0,0);
	noecho();
	raw();
	refresh();

	my_uid=getuid();
	screen_mode=MODE_NORM;
	edit_line= -1;

	main_screen.fmtline = fmtqitem;
	prtr_screen.fmtline = fmtpitem;

	main_screen.fkeys = (FKEY_ACTION*)gmem(sizeof(FKEY_ACTION)*32+1);
	prtr_screen.fkeys = (FKEY_ACTION*)gmem(sizeof(FKEY_ACTION)*32+1);
	
	wp = main_screen.fkeys;

        setupfkey(wp++, fn15_key,       ACT_SCREEN     , 14, "(15)Prtrs ",               0, (char*) &prtr_screen);
        setupfkey(wp++, fn1_key,        ACT_RETVAL     ,  1, "(1) Exit          ",       0, (char*) LEAVE       );
        setupfkey(wp++, up_arrow_key,   ACT_FUNCALL    , -1,                 NULL,       0, (char*) doup        );
        setupfkey(wp++, down_arrow_key, ACT_FUNCALL    , -1,                 NULL,       0, (char*) dodn        );
        setupfkey(wp++, fn2_key,        ACT_FUNCALL    ,  2, "(2) Top           ",cangotop, (char*) dotop       );
        setupfkey(wp++, fn3_key,        ACT_FUNCALL    ,  3, "(3) Bottom        ",cangobot, (char*) dobot       );
        setupfkey(wp++, fn4_key,        ACT_FUNCALL    ,  4, "(4) Up Pg         ", cangoup, (char*) dopup       );
        setupfkey(wp++, fn5_key,        ACT_FUNCALL    ,  5, "(5) DownPg        ", cangodn, (char*) dopdn       );  
        setupfkey(wp++, fn6_key,        ACT_FUNCALLARGS,  6, "(6) Display       ", candisp, (char*) dodisp      );  
        setupfkey(wp++, fn7_key,        ACT_FUNCALLARGS,  7, "(7) Hold/Release  ",       0, (char*) doholdrel   );
        setupfkey(wp++, fn8_key,        ACT_FUNCALLARGS,  8, "(8) Delete        ",       0, (char*) dodel       );
        setupfkey(wp++, fn9_key,        ACT_FUNCALLARGS,  9, "(9) Stop/Start    ",       0, (char*) dostopstart );
        setupfkey(wp++, fn10_key,       ACT_FUNCALLARGS, 10, "(10) Change   ",	   notedit, (char*) doedit      );
        setupfkey(wp++, fn11_key,       ACT_FUNCALL    , 11, "(11) All/Mine ",		 0, (char*) toggleshow  );
        setupfkey(wp++, fn12_key,       ACT_FUNCALLARGS, 12, "(12) Remove       ",       0, (char*) dorem       );  
        setupfkey(wp++, fn14_key,       ACT_FUNCALL    , 13, "(14)Other ",               0, (char*) togglemode  );  
        setupfkey(wp++, return_key,     ACT_FUNCALLARGS, -1,                 NULL,       0, (char*) stopedit    );  

        wp = prtr_screen.fkeys;

        setupfkey(wp++, fn15_key,       ACT_SCREEN     , 14, "(15)Queue ",               0, (char*) &main_screen);
        setupfkey(wp++, fn1_key,        ACT_RETVAL     ,  1, "(1) Exit          ",       0, (char*) LEAVE       );
        setupfkey(wp++, up_arrow_key,   ACT_FUNCALL    , -1,                 NULL,       0, (char*) doup        );  
        setupfkey(wp++, down_arrow_key, ACT_FUNCALL    , -1,                 NULL,       0, (char*) dodn        );  
        setupfkey(wp++, fn2_key,        ACT_FUNCALL    ,  2, "(2) Top           ",cangotop, (char*) dotop       );  
        setupfkey(wp++, fn3_key,        ACT_FUNCALL    ,  3, "(3) Bottom        ",cangobot, (char*) dobot       );  
        setupfkey(wp++, fn4_key,        ACT_FUNCALL    ,  4, "(4) Up Pg         ", cangoup, (char*) dopup       );  
        setupfkey(wp++, fn5_key,        ACT_FUNCALL    ,  5, "(5) DownPg        ", cangodn, (char*) dopdn       );  
        setupfkey(wp++, fn6_key,        ACT_FUNCALLARGS,  6, "(6) Enable/Disable",       0, (char*) doendis     );
        setupfkey(wp++, fn7_key,        ACT_FUNCALLARGS,  7, "(7) Change Form   ", shalign, (char*) dochgfrm    );
        setupfkey(wp++, fn8_key,        ACT_FUNCALLARGS,  8, "(8) Align  Form   ", shalign, (char*) doalign     );
        setupfkey(wp++, fn9_key,        ACT_FUNCALLARGS,  9, "(9) Clear Error   ", prterrp, (char*) doclrerr    );
        setupfkey(wp++, fn10_key,       ACT_FUNCALLARGS, 10, "(10) Modify ",             0, (char*) doedit      ); 
        setupfkey(wp++, fn14_key,       ACT_FUNCALL,     13, "(14)Other ",               0, (char*) togglemode  );
        setupfkey(wp++, return_key,     ACT_FUNCALLARGS, -1,                 NULL,       0, (char*) stopedit    );  

}
setupfkey(key,keyval,func,slot,desc,condvar,data)
FKEY_ACTION *key;
int keyval,func,slot;
char *desc,*data;
int (*condvar)();
{
	key->keyval   = keyval;
	key->func     = func;
	key->slot     = slot;
	key->desc     = desc;
	key->showcond = condvar;
	memcpy(&key->data,&data,sizeof(union act));
}		

addqitem(ptr)			/* binary insertion into q list based on value of q_pos member */
QLIST *ptr;
{
	QLIST *p,*n;
	
	if (q_head==NULL)
	{
		q_head=ptr;
		ptr->prev=NULL;
		ptr->next=NULL;
	}
	else
	{
		q_ptr=q_head;
		if (ptr->data->q_pos < q_ptr->data->q_pos) 
		{
			ptr->prev=NULL;
			ptr->next = q_head;
			q_head->prev = ptr;
			q_head = ptr;
			return;
		}

		while ((ptr->data->q_pos > q_ptr->data->q_pos) && q_ptr->next) q_ptr = q_ptr->next;
		
		if (ptr->data->q_pos < q_ptr->data->q_pos)
		{
			p = q_ptr->prev;
			n = q_ptr;
			ptr->prev = p;
			ptr->next = n;
			p->next = ptr;
			n->prev = ptr;
		}
		else
		{
			q_ptr->next = ptr;
			ptr->prev = q_ptr;
			ptr->next = NULL;
		}

	}
	
}
delqitem(q)
QLIST *q;
{
	QLIST *n,*p;
	
	p=q->prev;
	n=q->next;
	if (p) p->next=n;
	if (n) n->prev=p;
	if (q_head==q) q_head=n;
	free(q->data);
	free(q);
}

deletequeue()
{
	if (q_head==NULL) return;
	deletequeue_(q_head);
	q_head=NULL;
}
deletequeue_( ql )
QLIST *ql;
{
	if (ql->next) 
	if (ql->next) deletequeue_(ql->next);
	free(ql->data);
	free(ql);
}

catch()
{
	extern void graceful_exit();
	
	signal( SIGINT,  graceful_exit );
	signal( SIGQUIT, graceful_exit );
	signal( SIGILL,  graceful_exit );
	signal( SIGEMT,  graceful_exit );
	signal( SIGBUS,  graceful_exit );
	signal( SIGSEGV, graceful_exit );
	signal( SIGPIPE, graceful_exit );
}
wait_input()
{
#ifndef NO_SELECT
	fd_set fds;
	struct timeval inp_timeout;
#endif
	int vraw_fildes,cnt;
	int ret,ch;
	static int count=0;
	
	vraw_fildes=fileno(stdin);

	do
	{
		ret=0;
#ifndef NO_SELECT
		FD_ZERO(&fds);
		FD_SET(vraw_fildes,&fds);
		inp_timeout.tv_sec = 1;
		inp_timeout.tv_usec = 0;
		
		cnt=select(20,&fds,0,0,&inp_timeout);
		if (cnt==0)
		{
			++count;
			if (!(count%60)) ret |= ITIME;
		}
		if (FD_ISSET(vraw_fildes,&fds)) ret |= IUSER;
#else
		ch=vgetcto(1);
		if (ch) 
		{
			ret |= IUSER;
			vpushc(ch);
		}
		else
		{
			++count;
			if (!(count%60)) ret |= ITIME;
		}			
#endif
		if (has_changed(IQUEUE)) ret |= IQUEUE;
		if (has_changed(IPRT)) ret |= IPRT;
		if (has_changed(ICONF)) ret |= ICONF;
	}
	while (!ret);
	return ret ;
}
addpitem(ptr)
PLIST *ptr;
{
	if (p_head==NULL)
	{
		p_head=ptr;
		ptr->next=NULL;
	}
	else
	{
		p_ptr=p_head;
		while (p_ptr->next) p_ptr = p_ptr->next;
		p_ptr->next = ptr;
		ptr->next = NULL;
	}
}
deleteprinters()
{
	if (p_head==NULL) return;
	deleteprinters_(p_head);
	p_head=NULL;
}
deleteprinters_( pl )
PLIST *pl;
{
	if (pl->next) 
	if (pl->next) deleteprinters_(pl->next);
	free(pl->data);
	free(pl);
}
beep()
{
}
doholdrel(q)
QITEM *q;
{
	if (screen_mode != MODE_NORM) return;
	if ((q->status & QST_UNSPOOLED) && !ISPRTRERR(q) && !ISQSTOPPED(q)) 
	{
		statusl("Item already printing");
		return;
	}
	
	if ((my_uid != q->owner_uid) && !isoperator(my_uid)) 
	{
		statusl("Not owner");
		return;
	}
	
	if (ISHOLDING(q))
	{
		send_daemon(M_CHANGE,C_UNHOLD,q,sizeof(QITEM));
		q->status &= ~QST_HOLDING;
		q->mode &= ~QMD_HOLD;
	}
	else
	{
		send_daemon(M_CHANGE,C_HOLD,q,sizeof(QITEM));
		q->status |= QST_HOLDING;
		q->mode |= QMD_HOLD;
	}
	show_item(q);
}
candelete(path)
char *path;
{
	if (access(path,W_OK)<0)
	  return FALSE;
	else
	  return TRUE;
}
dodel(q)
QITEM *q;
{
	int i;
	if (screen_mode != MODE_NORM) return;
	if (q->status & QST_DELETE) return;
	if ((my_uid != q->owner_uid) && !isoperator(my_uid)) 
	{
		statusl("Not owner");
		return;
	}
	if (!candelete(q->real_path)) 
	{
		statusl("Item removed from queue but file not deleted (no permission)");
		dorem(q); /* do remove instead, current user does not have permission to */
		          /* delete the file, but does have permission to remove its entry from queue */
		return;
	}
	send_daemon(M_CHANGE,C_DELETE,q,sizeof(QITEM));
	q->status |= QST_DELETE;
	show_item(q);
}
dorem(q)
QITEM *q;
{
	int i;
	
	if (screen_mode != MODE_NORM) return;
	if (q->status & QST_REMOVE) return;
	
	if ((my_uid != q->owner_uid) && !isoperator(my_uid)) 
	{
		statusl("Not owner");
		return;
	}
	send_daemon(M_CHANGE,C_REMOVE,q,sizeof(QITEM));
	q->status |= QST_REMOVE;
	show_item(q);
} 
doendis(p)
PITEM *p;
{
	if (the_screen == &main_screen) return;
	/*if (ISBUSY(p)) return;*/
	if (ISENABLED(p)) 
	{
		send_daemon(M_CHANGE,C_DISABLE,p,sizeof(PITEM));
		p->status &= ~PST_ENABLED;
	}
	else
	{
		send_daemon(M_CHANGE,C_ENABLE,p,sizeof(PITEM));
		p->status |= PST_ENABLED;
	}
} 

dochgfrm(p)
PITEM *p;
{
	if (screen_mode != MODE_NORM) return;
	send_daemon(M_CHANGE,C_FORM,p,sizeof(PITEM));
} 
doalign(p)
PITEM *p;
{
	if (screen_mode != MODE_NORM) return;
	send_daemon(M_CHANGE,C_ALIGN,p,sizeof(PITEM));
}
dostopstart(q)
QITEM *q;
{
	if (screen_mode != MODE_NORM) return;
	
	if ((my_uid != q->owner_uid) && !isoperator(my_uid)) 
	{
		statusl("Not owner");
		return;
	}
	if (ISUNSPOOLED(q))
	{
		if (q->status & QST_STOPPED)
		{
			send_daemon(M_CHANGE,C_STARTQ,q,sizeof(QITEM));
			q->status &= ~QST_STOPPED;
		}
		else
		{
			send_daemon(M_CHANGE,C_STOPQ,q,sizeof(QITEM));
			q->status |= QST_STOPPED;
		}
		show_item(q);
	}
}
doedit(x)
char *x;
{
	screen_mode=MODE_EDIT;
	edit_line=pos_on_screen+ITEM_ST_LINE;
	show_item(x);

	vmode(BOLD);
	vmove(20,1);
	if (screen_mode==MODE_NORM) 
	  vprint("Press RETURN when finished or PF14 to edit other info                 ");
	else
	  vprint("Press RETURN when finished                                            ");
	vmove(21,1);
	verase(TO_EOL);
	vmove(22,1);
	verase(TO_EOL);
	vmove(23,1);
	verase(TO_EOL);
	field=0;
	if (the_screen== &main_screen)
	{
		edfunarray = the_screen->dmode?qfun2:qfun1;
		edposarray = the_screen->dmode?qfpos2:qfpos1;
		edlenarray = the_screen->dmode?qflen2:qflen1;
		eddatarray = the_screen->dmode?qfdat2:qfdat1;
		edmaxarray = the_screen->dmode?qfmax2:qfmax1;
		if (((QITEM*)x)->mode & QMD_NOBANNER) ((QITEM*)x)->banner=FALSE;
		else ((QITEM*)x)->banner=TRUE;
		edit_saveqpos = ((QITEM*)x)->q_pos;
	}
	else
	{
		edfunarray = pfun1;
		edposarray = pfpos1;
		edlenarray = pflen1;
		eddatarray = pfdat1;
		edmaxarray = pfmax1;
	}
	return 0;
}
notedit()
{
	if (screen_mode==MODE_NORM) return TRUE;
	else return FALSE;
}
shalign()
{
	if (screen_mode!=MODE_NORM) return FALSE;
	if (pos_on_screen == -1 || !(((PITEM*)items[pos_on_screen])->status & PST_FORMCHANGE)) return FALSE;
	return TRUE;
}
prterrp()
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (ISERROR((PITEM*)items[pos_on_screen])) return TRUE;
	else return FALSE;
}
doclrerr(p)
PITEM *p;
{
	if (!ISERROR(p)) return;
	else
	  send_daemon(M_CHANGE,C_CLRERR,p,sizeof(PITEM));
}
candisp()
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (((((QITEM*)items[top_line_pos + pos_on_screen])->owner_uid) != my_uid)  && !isoperator(my_uid)) 
	  return FALSE;
	else 
	  return TRUE;
}
static int wwaitpid(pid,rc)						/* Wait for process pid to complete		*/
int *rc,pid;
{
	int	stat_loc;

	do
	{
		wait(&stat_loc);
	} while( kill(pid,0) == 0 );

        *rc=WEXITSTATUS(stat_loc);
}
dodisp(q)
QITEM *q;
{
	extern char *sys_errlist[];
	int pid,dummy;
	
	if ((q->owner_uid != my_uid) && !isoperator(my_uid)) return;
	if ((pid=fork())==0)
	{
		if (execlp("display","display",q->real_path,0)<0)
		{
			vre_window("Unable to run program \"display\": %s",sys_errlist[errno]);
		}
		exit(0);
	}
	else
	  wwaitpid(pid,&dummy);
	vset(CURSOR,ON);
	vdefer(RESTORE);
	vstate(0);
	vrefresh(HARD_REFRESH);
}
togglemode()
{
	the_screen->dmode = 1-the_screen->dmode;
	if (screen_mode==MODE_EDIT)
	{
		if (the_screen== &main_screen)
		{
			edfunarray = the_screen->dmode?qfun2:qfun1;
			edposarray = the_screen->dmode?qfpos2:qfpos1;
			edlenarray = the_screen->dmode?qflen2:qflen1;
			eddatarray = the_screen->dmode?qfdat2:qfdat1;
			edmaxarray = the_screen->dmode?qfmax2:qfmax1;
		}
		else
		{
			edfunarray = pfun1;
			edposarray = pfpos1;
			edlenarray = pflen1;
			eddatarray = pfdat1;
			edmaxarray = pfmax1;
		}
	}
	show_list();
}

stopedit(x)
char *x;
{
	int tmp;
	char xxx[100];
	
	if (screen_mode==MODE_NORM) return;
	screen_mode=MODE_NORM;
	edit_line= -1;

	mvaddstr(20,1,the_screen->prompt_line);
	if (the_screen == &main_screen)
	{
		if( ((QITEM*)x)->banner) ((QITEM*)x)->mode &= ~QMD_NOBANNER;
		else ((QITEM*)x)->mode |= QMD_NOBANNER;
		if( (tmp=atoi(((QITEM*)x)->requested_prtr) )
		   || !strcmp(((QITEM*)x)->requested_prtr,"0"))
		{
			((QITEM*)x)->prtnum = tmp;
			((QITEM*)x)->requested_prtr[0]=(char)0;
		}
		vmode(0);

		strcpy( ((QITEM*)x)->actual_prtr,((QITEM*)x)->requested_prtr);
		
		show_item(x);
		vmode(A_HI_UND);

		/* if they did not change the pos, send the daemon a -1 */
		if (edit_saveqpos == ((QITEM*)x)->q_pos)
		  ((QITEM*)x)->q_pos = -1;
		send_daemon(M_CHANGE,C_QITEM,x,sizeof(QITEM));
	}
	else
	{
		vmode(0);
		
		show_item(x);
		vmode(A_HI_UND);
		send_daemon(M_CHANGE,C_PRTR,x,sizeof(PITEM));
	}
}
fixuppath(src,dest)   /* cut out /./'s and /../'s in a path */
char *src,*dest;
{
	char *p, *strchr(), *backup;
	char *x=dest;
	
	do
	{
		p=strchr(src,'/');
		if (!p) continue;
		else if (!strncmp(p,"/./",3))
		{
			memcpy(dest,src,p-src+1);
			dest += p-src+1;
			src = p+3;
			continue;
		}
		else if (!strncmp(p,"/../",4))
		{
			src = p+4;

			continue;
		}
		memcpy(dest,src,p-src+1);
		dest += p-src+1;
		src=p+1; 

	} while (p);
	memcpy(dest,src,strlen(src));
	
}
toggleshow()
{
	if (users_or_all==SHOW_ALL) users_or_all=SHOW_USERS;
	else users_or_all=SHOW_ALL;
	the_screen->list_changed = TRUE;
	
}
load_qdump()
{
	int dump;
	QITEM cur;
	QLIST *newlist, *q_tail;
	char *gmem();
	PITEM *findpitem();

	if (q_head) deletequeue();
	while(lockfile(LK_STAT,DUMPING));
	if (chk_vers(QVERS)==INCOMPAT_VERS)
	{
		incompat_qdump(); /* incompatible qdump detected.  inform user */
		return;
	}
	if ((dump=open(QDUMPFILE,O_RDONLY))<0)
	{
		return;
	}
	lseek(dump,8,0);
	while (read(dump,&cur,sizeof(QITEM)))
	{

		newlist=(QLIST*)gmem(sizeof(QLIST));

		if (q_head==NULL)
		{
			q_head=newlist;
			q_head->prev=NULL;
			q_head->next=NULL;
			q_tail=newlist;
		}
		else
		{
			q_tail->next=newlist;
			newlist->prev = q_tail;
			newlist->next = NULL;
			q_tail = newlist;
		}
	
		q_tail->data = (QITEM*)gmem(sizeof(QITEM));
		memcpy(q_tail->data,&cur,sizeof(QITEM));
	}
	close(dump);
	if (the_screen && the_screen == &main_screen)
	{
		the_screen->list_changed = TRUE;
	}
}
load_pdump()
{
	int dump;
	PITEM cur;
	PLIST *newlist;
	char *gmem();

	if (p_head) deleteprinters();
	
	while(lockfile(LK_STAT,DUMPING));
	if (chk_vers(PVERS)==INCOMPAT_VERS)
	{
		incompat_pdump(); /* incompatible qdump detected.  inform user */
		return;
	}
	
	if ((dump=open(PDUMPFILE,O_RDONLY))<0)
	{
		return;
	}
	lseek(dump,8,0);
	while (read(dump,&cur,sizeof(PITEM)))
	{
		newlist=(PLIST*)gmem(sizeof(PLIST));

		if (p_head==NULL)
		{
			p_ptr=p_head=newlist;
			p_head->next=NULL;
		}
		else
		{
			p_ptr->next = newlist;
			NEXT(p_ptr);
		}
		p_ptr->data = (PITEM*)gmem(sizeof(PITEM));
		memcpy(p_ptr->data,&cur,sizeof(PITEM));
	}
	close(dump);
	if (the_screen && the_screen != &main_screen)
	{
		the_screen->list_changed = TRUE;
	}
}
load_qconfig()
{
	int conf,sz;
	MASTCONFIG newconfig;
	
	while(lockfile(LK_STAT,DUMPING));
	if (chk_vers(CVERS)==INCOMPAT_VERS)
	{
		statusl("Warning: idaemon version appears to have changed");
		return;
	}
	if ((conf=open(CDUMPFILE,O_RDONLY))<0)
	{
		return;
	}
	lseek(conf,8,0);
	sz = read(conf,&newconfig,sizeof(MASTCONFIG));
	if (sz == sizeof(MASTCONFIG))
	  qconfig = newconfig;
	close(conf);
}     
has_changed(type)
int type;
{
	static time_t queue_time= -1, prt_time = -1, config_time = -1;
	time_t filetime(), tmp;
	
	switch (type)
	{
	      case IQUEUE:
		if ((tmp=filetime(QDUMPFILE)) > queue_time)
		{
			queue_time = tmp;
			return TRUE;
		}
		break;
	      case IPRT:
		if ((tmp=filetime(PDUMPFILE)) > prt_time)
		{
			prt_time = tmp;
			return TRUE;
		}
		break;
	      case ICONF:
		if ((tmp=filetime(CDUMPFILE)) > config_time)
		{
			config_time = tmp;
			return TRUE;
		}
		break;
	}
	return FALSE;
}
time_t filetime(path)
char *path;
{
	struct stat buf;
	
	stat(path,&buf);
	return buf.st_ctime;
}
isoperator( id )
int id;
{
	int i;
	
	if (id==0)
	  return TRUE;
	for (i=0; i<qconfig.opcount; ++i)
	  if (qconfig.operators[i] == id) 
	    return TRUE;

	return FALSE;
}
static char *incompat_msg[] =
{ "This ilpman is incompatible",
  "with the currently running",
  "idaemon.  Make sure you",
  "are running the most recent",
  "version of ilpman you have.",
  0
};
incompat_qdump()
{
	char **p;
	QLIST *newlist, *q_tail;
	int pos;
	char *gmem();
	
	for (pos=1,p=incompat_msg; *p; ++p)
	{
		newlist=(QLIST*)gmem(sizeof(QLIST));

		if (q_head==NULL)
		{
			q_head=newlist;
			q_head->prev=NULL;
			q_head->next=NULL;
			q_tail=newlist;
		}
		else
		{
			q_tail->next=newlist;
			newlist->prev = q_tail;
			newlist->next = NULL;
			q_tail = newlist;
		}
		q_tail->data = (QITEM*)gmem(sizeof(QITEM));
		strcpy(q_tail->data->real_path,*p);
		q_tail->data->owner_uid = my_uid;
		q_tail->data->q_pos = pos++;
	}
	if (the_screen && the_screen == &main_screen)
	{
		the_screen->list_changed = TRUE;
	}
}
incompat_pdump()
{
	char **p;
	PLIST *newlist;
	char *gmem();
	
	for (p=incompat_msg; *p; ++p)
	{
		newlist=(PLIST*)gmem(sizeof(PLIST));

		if (p_head==NULL)
		{
			p_ptr=p_head=newlist;
			p_head->next=NULL;
		}
		else
		{
			p_ptr->next = newlist;
			NEXT(p_ptr);
		}
		p_ptr->data = (PITEM*)gmem(sizeof(PITEM));
		strcpy(p_ptr->data->printer_dev,*p);
	}
}
