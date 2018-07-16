/*
 * Module:  ipman_main
 * Program: IDSIprint ipman
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#define  _IPMAN_MAIN_

#include "c_comm.h"
#include "manage.h"
#include "qpaths.h"
#include "v/vcap.h"

int setupfkey (struct fkey_action *key, int keyval, int altkey, int func, int slot, char *desc, int (*condvar) (/* ??? */), char *data);
int findqueueitem(struct q_item **d1, struct q_item **d2);
int findprinter(struct p_item **d1, struct p_item **d2);
int getipckey(char *path, char *type, unsigned char mod);


extern void make_vers_date (char *idstr, char *version, char *moddate);
int init_shmem (void);
int load_qdump (void);
int load_pdump (void);
int load_qconfig (void);
extern int initscr (void);
extern int move (int row, int col);
extern int noecho (void);
extern int raw (void);
extern int refresh (void);
int has_changed (int type);
extern int statusl (char *p);
int isoperator (int id);
extern int show_item (char *ptr);
int delremconfirm (char *type);
int dorem (struct q_item *q);
extern int attron (int x);
extern int mvaddstr (int r, int c, char *s);
extern int attroff (int x);
extern int show_list (void);
extern int setflag (int flag, int setting);
int tag_queue_old (void);
int chop_queue_old (void);
int tag_printers_old (void);
int chop_printers_old (void);
extern key_t myftok (char *file, char x);
extern void link_shutdown(int);
extern void graceful_exit(int);

int underline_attr;

init_me(void)
{
	char *gmem(int cnt, int sz);
	FKEY_ACTION *wp, key;
	int keyidx;
	int4 keyval;
	char *fmtqitem(char *data, int row, int col), *fmtpitem(char *data, int row, int col);
	char *type, *term_t;
	int status;
	int4 upkeyval,dnkeyval;
	int kmval[5];
	int *actkeyvals[5];
	int searchidx;			
	char *wc,*getenv();
	char wterm_t[128], vcfile[128], vcpath[128], vcdir[128], *envvar;
	
	make_vers_date(rcsid,VERSION,MODDATE);

	sprintf(vers_info,"V%s",VERSION);

	ring_open(&queue_list,sizeof(QITEM*),256,32,findqueueitem,0);
	ring_open(&printer_list,sizeof(PITEM*),32,8,findprinter,0);

	if ((status=init_shmem())!=IERR_OK)
	{
		return status;
	}
	if ((status=load_qdump())!=IERR_OK)
	{
		return status;
	}
	if ((status=load_pdump())!=IERR_OK)
	{
		return status;
	}
	load_qconfig();

	initscr();
	memset(erasefield,' ',80);
	
	vcloadsetup(VCAP_NOWARN_RETSTAT);
	vcloadterminfo();
	vc_add_stddefs();

	if (qconfig.default_scrmode == SCRMODE_WANG)
	{
		wang_mode=TRUE;
	}
	else if (qconfig.default_scrmode == SCRMODE_UNIQUE)	
	{
		/*wang_mode=FALSE;*/
	}
	if (wc=getenv(WANGVAR))
	{
		if (*wc == 't' || *wc == 'T')
		{
			wang_mode=TRUE;
		}
		else if (*wc == 'f' || *wc=='F')
		{
			wang_mode=FALSE;
		}
	}
	if (wang_mode)
	{
		/* borrowed from vcap.c */
		if (wc=getenv("VIDEOINFO"))
		{
			strcpy(vcdir,wc);
		}
		else if (wc=getenv("VIDEOCAP"))
		{
			strcpy(vcdir,wc);
		}
		else if (wc=getenv("WISPCONFIG"))
		{
			vbldfilepath(vcdir,wc,"videocap");
		}
		else
		{
			strcpy(vcdir,VIDEOINFODIR);
		}
		if (!(term_t=getenv("WISPTERM")))
		{
			if (!(term_t=getenv("TERM")))
			{
				term_t=DEFTERM;
			}
		}
		strcpy(wterm_t,term_t);
		
		vbldfilepath(vcpath,vcdir,wterm_t);						/* build the vc path */
		
		if (access(vcpath,R_OK)!=0)
		{
			fprintf(stderr,"Can't access VIDEOINFO file:\n%s\n",vcpath);
			fprintf(stderr,"Please check your environment and run %s again.\n",
				MANNAME);
			exit(1);
		}
		
		vcloadvideocap(vcpath,wterm_t);
		main_screen.fkeys = &wang_qfkeys[0];
		prtr_screen.fkeys = &wang_pfkeys[0];
		edit_delleft_main = GENERIC_DELETE+VMBIAS;
		edit_delleft_alt  = 0;
		edit_delrt_main   = GENERIC_REMOVE+VMBIAS;
		edit_delrt_alt    = 0;
		edit_ins_main     = GENERIC_INSERT+VMBIAS;
		edit_ins_alt      = 0;
		edit_left_main    = GENERIC_LEFT+VMBIAS;
		edit_left_alt     = 0;
		edit_right_main   = GENERIC_RIGHT+VMBIAS;
		edit_right_alt    = 0;

	}
	else
	{
		for (keyidx=0; qfkeys[keyidx].function; ++keyidx)
		{
			int keyval;
			
			if (!strlen(qfkeys[keyidx].function))
			{
				continue;
			}
			vkeymap("UNIQUE",
				qfkeys[keyidx].function,
				&keyval);
			
			if (keyval == KM_UNKNOWN)
			{
				keyval = qfkeys[keyidx].keyval;
			}
			for (searchidx=0; xlatkeylist[searchidx].kmval != -1 ; ++searchidx)
			{
				if (keyval == xlatkeylist[searchidx].kmval)
				{
					if (vc_gotkey(xlatkeylist[searchidx].vkeyval))
					{
						if (!vkeydef[xlatkeylist[searchidx].vkeyval].eval_to)
						{
							vc_map_key(xlatkeylist[searchidx].vkeyval, 
								   xlatkeylist[searchidx].defaultmapto );
							qfkeys[keyidx].keyval = xlatkeylist[searchidx].defaultmapto + VMBIAS;
						}
						else
						{
							VKEY *vp;
							vp= (&vkeydef[xlatkeylist[searchidx].vkeyval]);
							while (vp->eval_to)
							{
								vp = vp->eval_to;
							}
							qfkeys[keyidx].keyval = vp->id + VMBIAS;
						}
						if (xlatkeylist[searchidx].labeliffound)
						{
							strcpy(qfkeys[keyidx].flabel,xlatkeylist[searchidx].labeliffound);
						}
					}
					else
					{
						if (xlatkeylist[searchidx].labelgeneric)
						{
							strcpy(qfkeys[keyidx].flabel,xlatkeylist[searchidx].labelgeneric);
						}
						qfkeys[keyidx].keyval = xlatkeylist[searchidx].defaultmapto + VMBIAS;
					}
				}
			}
			
		}
		vkeymap("UNIQUE","EDITDELLEFT",&kmval[0]);    
		if (kmval[0]==KM_UNKNOWN)
		{
			kmval[0]=EDITDELLEFT_DEFAULT;
		}
		actkeyvals[0] = &edit_delleft_main;
		
		vkeymap("UNIQUE","EDITDELRIGHT",&kmval[1]);    
		if (kmval[1]==KM_UNKNOWN)
		{
			kmval[1]=EDITDELRIGHT_DEFAULT;
		}
		actkeyvals[1] = &edit_delrt_main;
		
		vkeymap("UNIQUE","EDITINSERT",&kmval[2]);      
		if (kmval[2]==KM_UNKNOWN)
		{
			kmval[2]=EDITINSERT_DEFAULT;
		}
		actkeyvals[2] = &edit_ins_main;
		
		vkeymap("UNIQUE","EDITLEFT",&kmval[3]);        
		if (kmval[3]==KM_UNKNOWN)
		{
			kmval[3]=EDITLEFT_DEFAULT;
		}
		actkeyvals[3] = &edit_left_main;
		
		vkeymap("UNIQUE","EDITRIGHT",&kmval[4]);       
		if (kmval[4]==KM_UNKNOWN)
		{
			kmval[4]=EDITRIGHT_DEFAULT;
		}
		actkeyvals[4] = &edit_right_main;
		
		for (keyidx = 0; keyidx < 5; ++keyidx)
		{
			keyval = kmval[keyidx];
			for (searchidx=0; xlatkeylist[searchidx].kmval != -1 ; ++searchidx)
			{
				if (keyval == xlatkeylist[searchidx].kmval)
				{
					if (vc_gotkey(xlatkeylist[searchidx].vkeyval))
					{
						if (!vkeydef[xlatkeylist[searchidx].vkeyval].eval_to)
						{
							vc_map_key( xlatkeylist[searchidx].vkeyval, 
								   xlatkeylist[searchidx].defaultmapto );
							*(actkeyvals[keyidx]) = xlatkeylist[searchidx].defaultmapto + VMBIAS;
						}
						else
						{
							VKEY *vp;
							vp= &vkeydef[xlatkeylist[searchidx].vkeyval];
							while (vp->eval_to)
							{
								vp = vp->eval_to;
							}
							*(actkeyvals[keyidx]) = vp->id + VMBIAS;
							
						}
					}
				}
			}
			
		}
		for (keyidx=0; pfkeys[keyidx].function; ++keyidx)
		{
			int keyval;
			
			if (!strlen(pfkeys[keyidx].function))
			{
				continue;
			}
			vkeymap("UNIQUE",
				pfkeys[keyidx].function,
				&keyval);
			if (keyval == KM_UNKNOWN)
			{
				keyval = pfkeys[keyidx].keyval;
			}
			for (searchidx=0; xlatkeylist[searchidx].kmval != -1 ; ++searchidx)
			{
				if (keyval == xlatkeylist[searchidx].kmval)
				{
					if (vc_gotkey(xlatkeylist[searchidx].vkeyval))
					{
						if (!vkeydef[xlatkeylist[searchidx].vkeyval].eval_to)
						{
							vc_map_key( xlatkeylist[searchidx].vkeyval, 
								   xlatkeylist[searchidx].defaultmapto );
							pfkeys[keyidx].keyval = xlatkeylist[searchidx].defaultmapto + VMBIAS;
						}
						else
						{
							VKEY *vp;
							vp= &vkeydef[xlatkeylist[searchidx].vkeyval];
							while (vp->eval_to)
							{
								vp = vp->eval_to;
							}
							pfkeys[keyidx].keyval = vp->id + VMBIAS;
							
						}
						if (xlatkeylist[searchidx].labeliffound)
						{
							strcpy(pfkeys[keyidx].flabel,xlatkeylist[searchidx].labeliffound);
						}
					}
					else
					{
						if (xlatkeylist[searchidx].labelgeneric)
						{
							strcpy(pfkeys[keyidx].flabel,xlatkeylist[searchidx].labelgeneric);
						}
						pfkeys[keyidx].keyval = xlatkeylist[searchidx].defaultmapto + VMBIAS;
					}
				}
			}
			
		}
		main_screen.fkeys = &qfkeys[0];
		prtr_screen.fkeys = &pfkeys[0];
	}
	
	main_screen.fmtline = fmtqitem;
	prtr_screen.fmtline = fmtpitem;

	main_screen.scroll = 0;
	prtr_screen.scroll = 0;
	
	build_vc_meta();
	
	vcloaded();

	if (envvar=getenv(DISPVAR))
	{
		strcpy(dispprog,envvar);
	}
	else
	{
		if (wang_mode==TRUE)
		{
			strcpy(dispprog,DEFDISPPROGW);
		}
		else
		{
			strcpy(dispprog,DEFDISPPROGN);
		}
	}

	underline_attr = (vcapdef[ENTER_UNDERLINE_MODE] && strlen(vcapdef[ENTER_UNDERLINE_MODE]))? UNDERSCORE|BOLD :
	                 (vcapdef[ENTER_REVERSE_MODE] && strlen(vcapdef[ENTER_REVERSE_MODE])) ?    REVERSE: 0;

	vstate(0);
	verase(FULL_SCREEN);
	move(0,0);
	noecho();
	raw();
	refresh();
/*	voptimize(DEFER_MODE);*/

	my_uid=getuid();
	screen_mode=MODE_NORM;
	edit_line= -1;

	return 0;
}

addqitem(struct q_item *ptr)
                        			/* binary insertion into q list based on value of q_pos member */
           
{
	int qidx,qpos,qcnt;
	QITEM *qcur;
	int st;
	
	if ((st=ring_count(queue_list,&qcnt))!=0)
	{
		fprintf(stderr,"ERROR: addqitem: ring_count returned %d\n",st);
		graceful_exit(0);
	}
	if (qcnt)
	{
		for (qpos=0; qpos<qcnt; ++qpos)
		{
			st=ring_get(queue_list,qpos,&qcur);
			if (st!=0)
			{
				fprintf(stderr,"ERROR: addqitem: ring_get returned %d\n",st);
				graceful_exit(0);
			}
			if (ptr->q_pos < qcur->q_pos)
			{
				ring_add(queue_list,qpos,&ptr);
				return;
			}
		}
		ring_add(queue_list,-1,&ptr);
	}
	else
	{
		ring_add(queue_list,0,&ptr);
	}
}
catch(void)
{
	signal( SIGINT,  graceful_exit );
	signal( SIGQUIT, graceful_exit );
	signal( SIGILL,  graceful_exit );
	signal( SIGEMT,  graceful_exit );
	signal( SIGBUS,  graceful_exit );
	signal( SIGSEGV, graceful_exit );
	signal( SIGPIPE, link_shutdown );
}
int kbdto=FALSE;
void kbdinp(int dummy)
{
	kbdto=TRUE;
}
wait_input(void)
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
		
		cnt=select(20,FDSETCAST &fds,0,0,&inp_timeout);   /* FDSETCAST defined in defs.h */
		if (cnt==0)
		{
			++count;
			if (!(count%60)) ret |= ITIME;
		}
		if (FD_ISSET(vraw_fildes,&fds)) ret |= IUSER;
#else
		signal(SIGALRM,kbdinp);
		kbdto=FALSE;
		alarm(1);
		ret=read(vraw_fildes,&ch,1);
		alarm(0);
		if (ret==1 && kbdto!=TRUE) 
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
addpitem(struct p_item *ptr)
{
	ring_add(printer_list,-1,&ptr);
}
doholdrel(struct q_item *q)
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
		send_daemon(M_CHANGE,C_UNHOLD,(char*)q,sizeof(QITEM),q->time_stamp=time((time_t*)0));
		q->status &= ~QST_HOLDING;
		q->mode &= ~QMD_HOLD;
		vmode(0);
		statusl("Request sent to daemon                                          ");
	}
	else
	{
		send_daemon(M_CHANGE,C_HOLD,(char*)q,sizeof(QITEM),q->time_stamp=time((time_t*)0));
		q->status |= QST_HOLDING;
		q->mode |= QMD_HOLD;
		vmode(0);
		statusl("Request sent to daemon                                          ");
	}
	show_item((char*)q);
}
candelete(char *path)
{
	if (access(path,W_OK)<0)
	  return FALSE;
	else
	  return TRUE;
}
doscrlf(void)
{
	++the_screen->scroll;
	the_screen->list_changed=TRUE;
}
doscrrt(void)
{
	--the_screen->scroll;
	if (the_screen->scroll <0)
	{
		the_screen->scroll=0;
	}
	the_screen->list_changed=TRUE;
}
dodel(struct q_item *q)
{
	int i;
	if (screen_mode != MODE_NORM) return;
	if (q->status & QST_DELETE) return;
	if ((my_uid != q->owner_uid) && !isoperator(my_uid)) 
	{
		statusl("Not owner");
		return;
	}
	if (delremconfirm("delete")==0)
	  return;
	if (!candelete(q->real_path)) 
	{
		statusl("Item removed from queue but file not deleted (no permission)");
		dorem(q); /* do remove instead, current user does not have permission to */
		          /* delete the file, but does have permission to remove its entry from queue */
		return;
	}
	send_daemon(M_CHANGE,C_DELETE,(char*)q,sizeof(QITEM),q->time_stamp=time((time_t*)0));
	vmode(0);
	statusl("Request sent to daemon                                          ");
	q->status |= QST_DELETE;
	show_item((char*)q);
}
dorem(struct q_item *q)
{
	int i;
	
	if (screen_mode != MODE_NORM) return;
	if (q->status & QST_REMOVE) return;
	
	if ((my_uid != q->owner_uid) && !isoperator(my_uid)) 
	{
		statusl("Not owner");
		return;
	}
	if (delremconfirm("remove")==0)
	  return;
	send_daemon(M_CHANGE,C_REMOVE,(char*)q,sizeof(QITEM),q->time_stamp=time((time_t*)0));
	vmode(0);
	statusl("Request sent to daemon                                          ");
	q->status |= QST_REMOVE;
	show_item((char*)q);
} 
delremconfirm(char *type)
{
	char line[81];
	int ch;
	
	vmove(21,1);
	verase(TO_EOL);
	vmove(22,1);
	verase(TO_EOL);
	vmove(23,1);
	verase(TO_EOL);
	vmove(20,1);
	verase(TO_EOL);
	sprintf(line,"Press (ENTER) to %s, or press (PF1) to return to display                    ",type);
	vprint(line);
	do 
	{
		ch=vgetm();
	} while (ch != return_key && ch != fn1_key);
	vmove(21,1);
	verase(TO_EOL);
	vmove(22,1);
	verase(TO_EOL);
	vmove(23,1);
	verase(TO_EOL);
	vmove(20,1);
	verase(TO_EOL);
	attron(A_HI_UND);
	mvaddstr(20,1,the_screen->prompt_line);
	attroff(0);
	if (ch==return_key)
	  return 1;
	else 
	  return 0;
}
doendis(struct p_item *p)
{
	if (the_screen == &main_screen) return;
	/*if (ISBUSY(p)) return;*/
	if (ISENABLED(p)) 
	{
		send_daemon(M_CHANGE,C_DISABLE,(char*)p,sizeof(PITEM),p->time_stamp=time((time_t*)0));
		p->status &= ~PST_ENABLED;
		vmode(0);
		statusl("Request sent to daemon                                          ");
	}
	else
	{
		send_daemon(M_CHANGE,C_ENABLE,(char*)p,sizeof(PITEM),p->time_stamp=time((time_t*)0));
		p->status |= PST_ENABLED;
		vmode(0);
		statusl("Request sent to daemon                                          ");
	}
} 

dochgfrm(struct p_item *p)
{
	if (screen_mode != MODE_NORM) return;
	send_daemon(M_CHANGE,C_FORM,(char*)p,sizeof(PITEM),p->time_stamp=time((time_t*)0));
	vmode(0);
	statusl("Request sent to daemon                                          ");
} 
doalign(struct p_item *p)
{
	if (screen_mode != MODE_NORM) return;
	send_daemon(M_CHANGE,C_ALIGN,(char*)p,sizeof(PITEM),p->time_stamp=time((time_t*)0));
	vmode(0);
	statusl("Request sent to daemon                                          ");
}
dostopstart(struct q_item *q)
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
			send_daemon(M_CHANGE,C_STARTQ,(char*)q,sizeof(QITEM),q->time_stamp=time((time_t*)0));
			q->status &= ~QST_STOPPED;
			vmode(0);
			statusl("Request sent to daemon                                          ");
		}
		else
		{
			send_daemon(M_CHANGE,C_STOPQ,(char*)q,sizeof(QITEM),q->time_stamp=time((time_t*)0));
			q->status |= QST_STOPPED;
			vmode(0);
			statusl("Request sent to daemon                                          ");
		}
		show_item((char*)q);
	}
}
doedit(char *x)
{
	screen_mode=MODE_EDIT;
	edit_line=pos_on_screen+ITEM_ST_LINE;
	show_item(x);

	vmode(BOLD);
	vmove(20,1);
	if (screen_mode==MODE_NORM) 
	  vprint("Press RETURN when finished or PF14 to edit other info                           ");
	else
	  vprint("Press RETURN when finished                                                      ");
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
		ededitallowed = the_screen->dmode?qfedal2:qfedal1;
		edspcallowed  = the_screen->dmode?qfspc2:qfspc1;
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
		ededitallowed = pfedal;
		edspcallowed = pfspc;
	}
	return 0;
}
notedit(void)
{
	if (screen_mode!=MODE_NORM) return FALSE;
	
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (the_screen != &main_screen && the_screen->dmode) return FALSE;
	else if (((((QITEM*)items[top_line_pos + pos_on_screen])->owner_uid) != my_uid)  && !isoperator(my_uid)) 
	  return FALSE;
	else 
	{
		if (ISUNSPOOLED(((QITEM*)items[top_line_pos + pos_on_screen])))
		{
			return FALSE;
		}
		else
		  return TRUE;
	}
}
shalign(void)
{
	if (screen_mode!=MODE_NORM) return FALSE;
	if (pos_on_screen == -1 || !(((PITEM*)items[top_line_pos+pos_on_screen])->status & PST_FORMCHANGE)) return FALSE;
	return TRUE;
}
prterrp(void)
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (ISERROR((PITEM*)items[top_line_pos+pos_on_screen])) return TRUE;
	else return FALSE;
}
doclrerr(struct p_item *p)
{
	if (!ISERROR(p)) return;
	else
	{
		send_daemon(M_CHANGE,C_CLRERR,(char*)p,sizeof(PITEM),p->time_stamp=time((time_t*)0));
		vmode(0);
		statusl("Request sent to daemon                                          ");
	}
}
static int wwaitpid(int pid, int *rc)
             
             						/* Wait for process pid to complete		*/
            
{
	int	stat_loc;

	do
	{
		wait(&stat_loc);
	} while( kill(pid,0) == 0 );

        *rc=WEXITSTATUS(stat_loc);
}
doshowerr(struct p_item *p)
{
	char path[200];
	int pid,dummy;
	extern SCREEN *last_screen;
	
	if (!ISERROR(p)) return;
	
	sprintf(path,"%s/%s",pstatusd,p->printer_name);

	verase(FULL_SCREEN);
	vraw_stty_save();
	
	vexit(0);
	
	if ((pid=fork())==0)
	{
		if (execlp(dispprog,dispprog,path,0)<0)
		{
			vre_window("Can't run display program \"%s\": %s",dispprog,sys_errlist[errno]);
		}
		exit(0);
	}
	else
	  wwaitpid(pid,&dummy);

	vraw_stty_restore();

	vset(CURSOR,ON);
/*	vdefer(RESTORE);*/
	vstate(0);
	vrefresh(HARD_REFRESH);
	last_screen=NULL;
	reentered=TRUE;
	return LEAVE;
}

candisp(void)
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (((((QITEM*)items[top_line_pos + pos_on_screen])->owner_uid) != my_uid)  && !isoperator(my_uid)) 
	  return FALSE;
	else 
	  return TRUE;
}
canhold(void)
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (((((QITEM*)items[top_line_pos + pos_on_screen])->owner_uid) != my_uid)  && !isoperator(my_uid)) 
	  return FALSE;
	else 
	{
		if (ISUNSPOOLED(((QITEM*)items[top_line_pos + pos_on_screen])))
		{
			if (ISPRTRERR(((QITEM*)items[top_line_pos + pos_on_screen])) ||
			    ISQSTOPPED((((QITEM*)items[top_line_pos + pos_on_screen]))))
			{
				return TRUE;
			}
			return FALSE;
		}
		else
		{
			return TRUE;
		}
	}
}
canrem(void)
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (((((QITEM*)items[top_line_pos + pos_on_screen])->owner_uid) != my_uid)  && !isoperator(my_uid)) 
	  return FALSE;
	else 
	{
		if (ISUNSPOOLED(((QITEM*)items[top_line_pos + pos_on_screen])))
		{
			return FALSE;
		}
		else
		  return TRUE;
	}
}
canstop(void)
{
	if (pos_on_screen == -1) 
	  return FALSE;
	else if (!items[0]) return FALSE;
	else if (((((QITEM*)items[top_line_pos + pos_on_screen])->owner_uid) != my_uid)  && !isoperator(my_uid)) 
	  return FALSE;
	else 
	{
		if (!ISUNSPOOLED(((QITEM*)items[top_line_pos + pos_on_screen])))
		{
			return FALSE;
		}
		else
		{
			if (ISPRTRERR(((QITEM*)items[top_line_pos + pos_on_screen])))
			{
				return FALSE;
			}
			else
			{
				return TRUE;
			}
		}
	}
}
canpmod(void)
{
	return TRUE;
}

dodisp(struct q_item *q)
{
	extern char *sys_errlist[];
	int pid,dummy;
	extern SCREEN *last_screen;
	
	vraw_stty_save();
	verase(FULL_SCREEN);
	
	vexit(0);
	
	if ((q->owner_uid != my_uid) && !isoperator(my_uid)) return;
	if ((pid=fork())==0)
	{
		if (execlp(dispprog,dispprog,q->real_path,0)<0)
		{
			vre_window("Unable to run display program \"%s\": %s",dispprog,sys_errlist[errno]);
		}
		exit(0);
	}
	else
	  wwaitpid(pid,&dummy);

	vraw_stty_restore();

	vset(CURSOR,ON);
	vdefer(RESTORE);
#if 0
	vcloadsetup(VCAP_TERMINFO_ONLY);
#endif
	vstate(0);
	vrefresh(HARD_REFRESH);
	reentered=TRUE;
	last_screen=NULL;
	return LEAVE;
}
togglemode(void)
{
	the_screen->dmode = 1-the_screen->dmode;
	if (the_screen== &main_screen)
	{
		int field_width_diff = 34-15;
		
		if (the_screen->dmode == 1)
		{
			if (the_screen->scroll > field_width_diff)
			{
				the_screen->scroll -= field_width_diff;
			}
		}
		else
		{
			if (the_screen->scroll)
			{
				the_screen->scroll += field_width_diff;
			}
		}
	}		

	if (screen_mode==MODE_EDIT)
	{
		if (the_screen== &main_screen)
		{
			edfunarray = the_screen->dmode?qfun2:qfun1;
			edposarray = the_screen->dmode?qfpos2:qfpos1;
			edlenarray = the_screen->dmode?qflen2:qflen1;
			eddatarray = the_screen->dmode?qfdat2:qfdat1;
			edmaxarray = the_screen->dmode?qfmax2:qfmax1;
			ededitallowed = the_screen->dmode?qfedal2:qfedal1;
			edspcallowed  = the_screen->dmode?qfspc2:qfspc1;

		}
		else
		{
			edfunarray = pfun1;
			edposarray = pfpos1;
			edlenarray = pflen1;
			eddatarray = pfdat1;
			edmaxarray = pfmax1;
			ededitallowed = pfedal;
			edspcallowed = pfspc;
		}
	}
	show_list();
}

stopedit(char *x)
{
	int tmp;
	char xxx[100];
	int restore_pos= -1;

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
		{
			restore_pos=((QITEM*)x)->q_pos;
			((QITEM*)x)->q_pos = -1;
		}
		send_daemon(M_CHANGE,C_QITEM,x,sizeof(QITEM),((QITEM*)x)->time_stamp=time((time_t*)0));
		vmode(0);
		statusl("Request sent to daemon                                          ");
		if (restore_pos != -1)
		{
			((QITEM*)x)->q_pos = restore_pos;
		}
	}
	else
	{
		vmode(0);
		
		show_item(x);
		vmode(A_HI_UND);
		send_daemon(M_CHANGE,C_PRTR,x,sizeof(PITEM),((PITEM*)x)->time_stamp=time((time_t*)0));
		vmode(0);
		statusl("Request sent to daemon                                          ");
	}
}
fixuppath(char *src, char *dest)
               
                   /* cut out /./'s and /../'s in a path */
                
{
	char *p, *strchr(CONST char *, int), *backup;
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
toggleshow(void)
{
	if (users_or_all==SHOW_ALL) 
	  users_or_all=SHOW_USERS;
	else
	  users_or_all=SHOW_ALL;
	the_screen->list_changed = TRUE;
}
static int shm_init = -1;
static int q_shmids[QDFILECNT];
static int p_shmid;

static int q_shkeys[QDFILECNT];
static int p_shkey;

static char *q_shmptrs[QDFILECNT] = {0};
static char *p_shmptr = {0};
	
int idx, the_key;

init_shmem(void)
{
	extern char *shmkeypaths[];
	int the_key;

	if (shm_init == -1)
	{
		for (idx = 0 ; idx<QDFILECNT ; ++idx)
		{
			the_key = getipckey(shmkeypaths[idx],"shmem",IPCKEYMOD);
			if (the_key == -1)
			{
				return IERR_SHMEM;
			}
			else
			  q_shkeys[idx]=the_key;
		}
		the_key = getipckey(prtkeyfile,"shmem",IPCKEYMOD);
		if (the_key == -1)
		{
			return IERR_SHMEM;
		}
		else
		  p_shkey=the_key;
		shm_init = 0;
	}
	return IERR_OK;
}
shut_shmem(void)
{
	for (idx=0; idx<QDFILECNT; ++idx)
	{
		q_shkeys[idx]=0;
		shmdt(q_shmptrs[idx]);
		q_shmptrs[idx]=NULL;
	}
	p_shkey=0;
	shmdt(p_shmptr);
	p_shmptr = NULL;
	shm_init = -1;
}
char *getshseg(int size, int key)
{
	int id;
	char *att;
#ifdef SEQUENT
	char *shmat();
#endif
	
	id = shmget(key,0,0);
	if (id<0)
	{
		return NULL;
	}
	att = shmat(id,0,0);
	if (att == (char*)-1)
	{
		return NULL;
	}
	return att;
}
queue_changed(void)
{
	static int lastseq = -1;
	struct shmmapheader *header;
	int ret;
	
	if (q_shmptrs[0]==NULL)
	  return TRUE;
	header = (struct shmmapheader *)q_shmptrs[0];
	setflag(SEMREADING,FLAG_SET);
	if (header->sequence == lastseq)
	{
		ret=FALSE;
	}
	else
	{
		lastseq = header->sequence;
		ret=TRUE;
	}
	setflag(SEMREADING,FLAG_CLEAR);
	return ret;
}
prtr_changed(void)
{
	static int lastseq = -1;
	struct shmmapheader *header;
	int ret;
       
	if (p_shmptr == NULL)
	  return TRUE;
	header = (struct shmmapheader *)p_shmptr;
	setflag(SEMREADING,FLAG_SET);
	if (header->sequence == lastseq)
	{
		ret=FALSE;
	}
	else
	{
		lastseq = header->sequence;
		ret=TRUE;
	}
	setflag(SEMREADING,FLAG_CLEAR);
	return ret;
}
load_qdump(void)
{
	struct shmmapheader *header;
	QITEM *cur, *mine;
	char *shptr, *gmem(int cnt, int sz);
	int cnt, blocknum;
	int finished = FALSE;
	int status;
	int findstatus,pos;
	
	cnt = blocknum = 0;

	if ((status = init_shmem())!=IERR_OK)
	{
		return status;
	}
	if ((status = setflag(SEMREADING,FLAG_SET)) != IERR_OK)
	{
		return status;
	}
	tag_queue_old();
	cur=NULL;
	
	while (!finished)
	{
		if (q_shmptrs[blocknum]==NULL)
		  q_shmptrs[blocknum] = getshseg(QSHMSZ, q_shkeys[blocknum]);

		if (q_shmptrs[0]==NULL)
		{
			return IERR_NODAEMON;
		}
		if (!q_shmptrs[blocknum])
		{
			++finished;
			continue;
		}
		
		header = (struct shmmapheader *)q_shmptrs[blocknum];
	
		if (header->magic != SHMMAGIC)
		{
			return IERR_DUMPMAG;
		}
		if (header->version != DUMPVERSION)
		{
			return IERR_DUMPVERS;
		}
		if (header->type    != SHMTYP_Q)
		{
			return IERR_DUMPFMT;
		}
		for (cnt = 0,  shptr = q_shmptrs[blocknum] + sizeof(struct shmmapheader); 
		     cnt < header->item_cnt;
		     ++cnt, shptr += sizeof(QITEM)
		     )
		{
			if (cur==NULL)
			{
				cur=(QITEM *)gmem(1,sizeof(QITEM));
			}
			memcpy(cur,shptr,sizeof(QITEM));
			findstatus = ring_find(queue_list,&cur,&pos,&mine);
			if (findstatus==0)
			{
				if (cur->time_stamp >= mine->time_stamp)
				{
					mine->q_pos = cur->q_pos;
					mine->status = cur->status;
					strncpy(mine->requested_prtr,cur->requested_prtr,QNAMEMAX);
					strncpy(mine->actual_prtr,cur->actual_prtr,QNAMEMAX);
					strncpy(mine->form,cur->form,FORMNAMEMAX);
					mine->class = cur->class;
					mine->copies = cur->copies;
					mine->mode  = cur->mode;
					mine->stpage = cur->stpage;
					mine->endpage = cur->endpage;
					mine->banner = cur->banner;
					mine->prtnum = cur->prtnum;
				}
				else
				{
					mine->status &= ~QST_OLD;
				}
			}
			else
			{
				addqitem(cur);
				cur=NULL;
			}
		}
		++blocknum;
	}
	setflag(SEMREADING,FLAG_CLEAR);

	chop_queue_old();
	if (the_screen && the_screen == &main_screen)
	{
		the_screen->list_changed = TRUE;
	}
	return IERR_OK;
}
tag_queue_old(void)
{
	int qpos,qcnt;
	QITEM *cur;

	ring_count(queue_list,&qcnt);
	for (qpos=0; qpos<qcnt; ++qpos)
	{
		ring_get(queue_list,qpos,&cur);
		cur->status |= QST_OLD;
	}
}
chop_queue_old(void)
{
	int qpos,qcnt;
	QITEM *cur;

	ring_count(queue_list,&qcnt);
	for (qpos=0; qpos<qcnt; )
	{
		ring_get(queue_list,qpos,&cur);
		if (cur->status & QST_OLD)
		{
			ring_remove(queue_list,qpos,NULL);
			free(cur);
			--qcnt;
		}
		else
		{
			++qpos;
		}
	}
}
load_pdump(void)
{
	struct shmmapheader *header;
	PITEM *cur, *mine;
	char *shptr, *gmem(int cnt, int sz);
	int cnt;
	int status,findstatus,pos;
	
	if ((status=init_shmem())!=IERR_OK)
	{
		return status;
	}
	if (p_shmptr==NULL)
	  p_shmptr = getshseg(PRTSHMSZ, p_shkey);
	if (p_shmptr==NULL)
	{
		return IERR_NODAEMON;
	}
	if ((status=setflag(SEMREADING,FLAG_SET))!=IERR_OK)
	{
		return status;
	}
	header = (struct shmmapheader *)p_shmptr;
	if (header->magic != SHMMAGIC)
	{
		return IERR_DUMPMAG;
	}
	if (header->version != DUMPVERSION)
	{
		return IERR_DUMPVERS;
	}
	if (header->type    != SHMTYP_P)
	{
		return IERR_DUMPFMT;
	}
	tag_printers_old();
	cur=NULL;
	for (cnt = 0, shptr = p_shmptr + sizeof(struct shmmapheader); 
	     cnt < header->item_cnt; 
	       ++cnt, shptr += sizeof(PITEM)
	     )
	{
		if (cur==NULL)
		{
			cur=(PITEM*)gmem(1,sizeof(PITEM));
		}
		memcpy(cur,shptr,sizeof(PITEM));
		findstatus = ring_find(printer_list,&cur,&pos,&mine);
		if (findstatus==0)
		{
			if (cur->time_stamp >= mine->time_stamp)
			{
				strncpy(mine->printer_dev,cur->printer_dev,PIPEMAX);
				strncpy(mine->default_form,cur->default_form,FORMNAMEMAX);
				strncpy(mine->current_form,cur->current_form,FORMNAMEMAX);
				strncpy(mine->class,cur->class,26);
				mine->status = cur->status;
				strncpy(mine->printer_model,cur->printer_model,MODNAMEMAX);
				mine->qid = cur->qid;
			}
			else
			{
				mine->status &= ~PST_OLD;
			}
		}
		else
		{
			addpitem(cur);
			cur=NULL;
		}
	}
      load_pdump_exit:

	setflag(SEMREADING,FLAG_CLEAR);
	chop_printers_old();
	if (the_screen && the_screen != &main_screen)
	{
		the_screen->list_changed = TRUE;
	}
	return IERR_OK;
}
tag_printers_old(void)
{
	int ppos,pcnt;
	PITEM *cur;

	ring_count(printer_list,&pcnt);
	for (ppos=0; ppos<pcnt; ++ppos)
	{
		ring_get(printer_list,ppos,&cur);
		cur->status |= PST_OLD;
	}
}
chop_printers_old(void)
{
	int ppos,pcnt;
	PITEM *cur;

	ring_count(printer_list,&pcnt);
	for (ppos=0; ppos<pcnt; )
	{
		ring_get(printer_list,ppos,&cur);
		if (cur->status & PST_OLD)
		{
			ring_remove(printer_list,ppos,NULL);
			free(cur);
			--pcnt;
		}
		else
		{
			++ppos;
		}
	}
}

load_qconfig(void)
{
	int conf,sz;
	MASTCONFIG newconfig;
	int4 magic=0,version=0;

	if ((conf=open(cdumpfile,O_RDONLY))<0)
	{
		return IERR_NODAEMON;
	}
	read(conf,(char*)&magic,sizeof(magic));
	read(conf,(char*)&version,sizeof(version));
	if (magic != CDUMPMAGIC)
	{
		return IERR_DUMPMAG;
	}
	if (version != DUMPVERSION)
	{
		return IERR_DUMPVERS;
	}
	sz = read(conf,(char*)&newconfig,sizeof(MASTCONFIG));
	if (sz == sizeof(MASTCONFIG))
	  qconfig = newconfig;
	close(conf);
	return IERR_OK;
}     
has_changed(int type)
{
	static time_t queue_time= -1, prt_time = -1, config_time = -1;
	time_t filetime(char *path), tmp;
	
	switch (type)
	{
	      case IQUEUE:
		if (queue_changed())
		  return TRUE;
#ifdef OLD
		if ((tmp=filetime(qdumpfile)) > queue_time)
		{
			queue_time = tmp;
			return TRUE;
		}
#endif
		break;
	      case IPRT:
		if (prtr_changed())
		  return TRUE;
#ifdef OLD
		if ((tmp=filetime(pdumpfile)) > prt_time)
		{
			prt_time = tmp;
			return TRUE;
		}
#endif
		break;
	      case ICONF:
		if ((tmp=filetime(cdumpfile)) > config_time)
		{
			config_time = tmp;
			return TRUE;
		}
		break;
	}
	return FALSE;
}
time_t filetime(char *path)
{
	struct stat buf;
	
	if (stat(path,&buf)!= -1)
	  return buf.st_ctime;
	else 
	  return 0;
}
isoperator(int id)
{
	int i;
	
	if (id==0)
	  return TRUE;
	for (i=0; i<qconfig.opcount; ++i)
	  if (qconfig.operators[i] == id) 
	    return TRUE;

	return FALSE;
}
void truncspc(char *p)
{
	char *tmpp,*gmem(int cnt, int sz);
	char *st,*end;
	
	if (strlen(p))
	{
		tmpp=gmem(strlen(p)+1,1);
		strcpy(tmpp,p);
		for(st=tmpp; *st && *st==' '; ++st);
		for(end=st;*end && *end!=' ';++end);
		if(*end) *end='\0';
		if (end-st)
		{
			memcpy(p,st,end-st);
			p[end-st]=(char)0;
		}
		free(tmpp);
	}
}
/* client username routine. */
static char nonuser[32];
char *c_username(int uid)
{
#ifdef SEQUENT
	struct passwd *getpwuid();	
#else
	struct passwd *getpwuid(uid_t);
#endif
	static int last_uid = -1;
	static struct passwd *pw;
	
	if (last_uid != uid) /* this should cut calls to getpwuid a bit */
	{
		pw=getpwuid(uid);
	}
	last_uid = uid;
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
getipckey(char *path, char *type, unsigned char mod)
{
	extern char errmsg[];
	int key;
	
	if (access(path,0)!=0)
	{
		return -1;
	}
	key = myftok(path, mod);
	return key;
}
int findqueueitem(struct q_item **d1, struct q_item **d2)
{
	return (*d1)->q_id - (*d2)->q_id;
}
int findprinter(struct p_item **d1, struct p_item **d2)
{
	return strcmp((*d1)->printer_name,(*d2)->printer_name);
}

/*
 * $Log: man_misc.c,v $
 * Revision 1.49  1993/10/12  23:46:52  jockc
 * two small changes to fix warnings on Sequent
 *
 * Revision 1.48  1993/09/30  23:11:06  jockc
 * fixed paths.
 *
 * Revision 1.47  1993/09/29  23:43:30  jockc
 * use other attr if _ unavail
 *
 * Revision 1.46  1993/09/22  20:39:37  jockc
 * parenthesize &'d var to keep gcc from breaking
 *
 * Revision 1.45  1993/09/15  18:30:00  jockc
 * don't override the -w flag
 *
 * Revision 1.44  1993/09/13  23:04:38  jockc
 * cleaned up header/footer lines
 *
 * Revision 1.43  1993/09/13  15:28:24  jockc
 * 499
 * 503
 *
 * Revision 1.44  1993/09/10  21:09:55  jockc
 * slight change to vcloadvideocap for VMS
 *
 * Revision 1.43  1993/09/10  18:47:08  jockc
 * new keys added.  version checking added.  statusl("request sent") added.
 * scroll function added.   some improvement in label-display-bool functions.
 * display program changeable. show error function added.  NO_SELECT
 * input loop improved.
 *
 * Revision 1.42  1993/08/13  22:08:56  jockc
 * const
 *
 * Revision 1.41  1993/08/13  20:42:45  jockc
 * cast fdsets to int * for HPUX
 *
 * Revision 1.40  1993/08/13  20:15:34  jockc
 * using ring now.  alpha/c89 changes. attempt to delete obsolete
 * printers.  began code that checks versions
 *
 * Revision 1.39  1993/05/28  21:46:38  jockc
 * changed the client gmem
 *
 * Revision 1.38  1993/04/27  18:15:43  jockc
 * changed sync to save/restore.. sync didn't work
 *
 * Revision 1.37  1993/04/27  17:31:36  jockc
 * added vraw_stty_sync() after exec of display to restore terminal state
 *
 * Revision 1.36  1993/03/12  22:35:19  jockc
 * forgot to return OK status from load_[qp]dump.. added
 *
 * Revision 1.35  1993/03/12  19:24:35  jockc
 * fixed up handling of exception during init
 *
 * Revision 1.34  1993/01/12  02:11:04  jockc
 * rewrote load_xdump to read shared memory..
 *
 * Revision 1.33  1993/01/06  19:02:55  jockc
 * revise the "improved" load_xdump functions a tad
 *
 * Revision 1.32  1993/01/05  01:33:05  jockc
 * improved dump file loading a bit:  old data is not deleted
 * until the new data has been successfully loaded.  This tends
 * to prevent the occasion screen blank in the data area of
 * manage screen, which would happen if the file load failed
 * temporarily for some reason.
 *
 * Revision 1.31  1992/12/31  23:52:44  jockc
 * qpaths/ISPOOL, and took out check of version every time.
 * may want to add it back in the future.
 *
 * Revision 1.30  1992/10/27  23:36:27  jockc
 * added load_qconfig for internet address lookup.
 *
 * Revision 1.29  1992/10/09  20:21:37  jockc
 * added new code to support new locking stuff.  and
 * added confirm on delete and remove
 *
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
