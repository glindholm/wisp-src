/*
 * Module:  ipman_screen
 * Program: IDSIprint
 * Purpose: main module for full screen interface to daemon
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";
#define EXT extern 
#include "defs.h"
#include "manage.h"


extern int mvaddstr (int r, int c, char *s);
int show_td (void);
extern int attron (int x);
int load_items (void);
int show_list (void);
int process_input (char *data);
extern int shut_sems (void);
int statusl (char *p);
int show_pstatus (void);
extern int attroff (int x);
int show_fkeys (void);
int save_pos (void);
extern int refresh (void);
char *showfield (char *buf, int bufsz, int wpos, int wsize, char *showbuf);
int clearstatusl (void);
extern int getch (void);
char *repchar(int,int);
static int lowerstring();

extern int shut_shmem (void);
extern int stopedit (char *x);
extern int togglemode (void);
extern int wait_input (void);
extern int load_qdump (void);
extern int load_pdump (void);
extern int load_qconfig (void);
extern void truncspc (char *p);

int link_down=FALSE, wrong_vers=FALSE;
SCREEN *last_screen=NULL;
extern int underline_attr;

in_screen(char *data)
                
           			/* pointer to displayable info */
{
	extern SCREEN *the_screen;
	extern int comm_status;
	static int need_u_td=TRUE;
	int action;
	extern int showtop,showbot,canup,candn;
	char errmsg[200];

	showtop=showbot=canup=candn=1;

	vset(CURSOR,INVISIBLE);
	if (need_u_td || reentered)		/* need to print username and date */
	{
		mvaddstr(1,1,str_u());	   /* print user once */
		show_td();		   /* setup date thingy */
		need_u_td=FALSE;	   /* only needs doing once */
	}
	
	if (last_screen != the_screen || reentered)  /* has screen changed? or has the routine been reentered */
	{
		char tmpbuf[200];
		extern char vers_info[];
		
		reentered=FALSE;
		sprintf(tmpbuf,the_screen->title_line,vers_info);
		
		mvaddstr(0,(screen_width/2)-(((int)strlen(tmpbuf))/2),tmpbuf);
		vmode(underline_attr);

		mvaddstr(20,1,the_screen->prompt_line);
		the_scrid = the_screen->scrid; /* and global scrid */
		if (last_screen)
		{
			last_screen->tlp=top_line_pos;
			last_screen->pos=pos_on_screen;
			last_screen->pil=pos_in_list;
		}
		last_screen = the_screen;      /* and my screen pointer */
		pos_on_screen = the_screen->pos;
		pos_in_list   = the_screen->pil;
		top_line_pos  = the_screen->tlp;
		load_items();
		show_list();
	}
	
	do			/* loop in screen doing stuff */
	{

		if (!link_down && (isrunning()!=IERR_OK))
		{
			shut_sems();
			shut_comm();
			shut_shmem();
			
			sprintf(errmsg,"WARNING: Master process %s is no longer running       ",DAEMONNAME);
			vbell();
			statusl(errmsg);

 			link_down = TRUE;
			wrong_vers;
			continue;
		}
		if (link_down)
		{
			int st;
			char tmp[100];
			
			if (isrunning()==IERR_OK)
			{
				int init;
				int ready;
				
				ready = waitready();
				if (!ready)
				{
					shut_sems();
					continue;
				}
				if ((init=init_comm())==IERR_OK)
				{
					init_shmem();
					statusl("Connection reestablished with master process               ");
					vbell();
					link_down=FALSE;
				}
				else
				{
					switch(init&0xff)
					{
					      case IERR_NODAEMON:
						sprintf(errmsg,"WARNING: Master process %s is no longer running       ",
							DAEMONNAME);
						statusl(errmsg);
						break;
					      case IERR_SOCKET:
						statusl("WARNING: Error allocating socket.  Contact Customer Support.   ");
						break;
					      case IERR_BIND:
						statusl("WARNING: Error binding socket.  Contact Customer Support.      ");
						break;
					      default:
						statusl("WARNING: Unknown problem reestablishing contact with daemon    ");
						break;
					}
					sleep(1);
					continue;
				}
			}
			else
			{
				sprintf(errmsg,"WARNING: Master process %s is no longer running       ",DAEMONNAME);
				statusl(errmsg);

				shut_sems();
			}
		}

		if(the_screen->list_changed) 
		{
			load_items();
			show_list();
		}
		vset(CURSOR,VISIBLE);
		action=process_input(data);
		if (action==LEAVE || action==QUITILPMAN)
		{
			return action;
		}
		switch(action)
		{
		       
		}
		if (comm_status!=COMM_OK)
		{
			extern int broken_pipe,daemon_d;
			
			if (comm_status != EPIPE && broken_pipe==FALSE)
			  sprintf(errmsg,"WARNING: Can't contact %s [%d/%d]. Call Customer Support",
				  DAEMONNAME,daemon_d,comm_status);
			else
			{
				shut_sems();
				shut_comm();
				shut_shmem();
				

				sprintf(errmsg,"WARNING: Master process %s is no longer running       ",DAEMONNAME);
				statusl(errmsg);
				link_down = TRUE;
				wrong_vers = FALSE;
			}
			vmode(NORMAL);
			statusl(errmsg);
		}
	}
	while (action != LEAVE);
}

show_td(void)
{
	char *str_td(void);
	static int last=0;
	
	vmode(0);
	mvaddstr(1,43,str_td());
}
process_input(char *data)
{
	int input_source,found;
	extern int global_key;
	FKEY_ACTION *ptr;
	int tmp, st;
	extern int optimization;
	char errmsg[200];
	int retval=0;
	
	if (screen_mode==MODE_NORM) show_fkeys();
	if (screen_mode==MODE_EDIT)
	{
		int ch;
		
		ch= (*(edfunarray[field])) (items[top_line_pos+pos_on_screen]+eddatarray[field],
					    edlenarray[field],
					    edit_line, 
					    edposarray[field],
					    edmaxarray[field],
					    ededitallowed[field],
					    edspcallowed[field]);
		if (ch=='\t' || ch==tab_key)
		{
			field++;
			if (edposarray[field]==0) field=0;
		}
		if (ch==backtab_key)
		{
			--field;
			if (field<0) 
			{
				while (edposarray[++field]);
				--field;
			}
		}
		if (ch=='\n' || ch=='\r' || ch==return_key || ch==enter_key)
		{
			stopedit(items[top_line_pos+pos_on_screen]);
		}
#if 0
		if (ch==fn14_key && the_screen== &main_screen)
		{
			togglemode();
		}
#endif
		return 0;
	}

	tmp=optimization;
	optimization=TRACKING_ONLY;
	if (screen_mode==MODE_NORM) vmove(ITEM_ST_LINE+pos_on_screen,0);
	optimization=tmp;
	
	input_source=wait_input();
	if (input_source & IUSER)
	{
		  if (screen_mode==MODE_NORM)
		  {
			  found=FALSE;
			  
			  while (!found)
			  {
				  global_key=vgetm();
				  for (found=FALSE,ptr = the_screen->fkeys; ptr->altkeyval; ++ptr)
				  {
					  if (ptr->keyval == global_key ||
					      ((global_key < VMBIAS) && (ptr->altkeyval == tolower(global_key))))
					  { 
						  found=TRUE; 
						  break; 
					  }
				  }
			  }
			  if (!(ptr->showcond) || ((ptr->showcond != NULL) &&  (ptr->showcond)()==TRUE))
			  {
				  switch (ptr->func)
				  {
					case ACT_RETVAL:
					  retval=((int)(ptr->retval));
					  break;
					case ACT_FUNCALLARGS:
					  if (!items[top_line_pos+pos_on_screen]) return;
					  if (the_screen== &main_screen && 
					      access(((QITEM*)items[top_line_pos+pos_on_screen])->real_path,0))
					  {
						  statusl("File not found				      ");
						  return 0;
					  }
					  retval= (*(ptr->fn))(items[top_line_pos+pos_on_screen]);
					  break;
					case ACT_FUNCALL:
					  retval= (*(ptr->fn))();
					  break;
					case ACT_SCREEN:
					  if(screen_mode==MODE_NORM)
					    the_screen = (SCREEN *)ptr->scrptr;
					  retval= LEAVE;
				  }
			  }
		  }
	  }
	if (!link_down && !wrong_vers)
	{
		if (input_source & IQUEUE || input_source & IPRT)
		  save_pos();
		if (input_source & IQUEUE)
		{
			st = load_qdump();
			if (st!= IERR_OK)
			{
				sprintf(errmsg,"WARNING: Unknown version of Master process %s   ",DAEMONNAME);
				statusl(errmsg);
				vbell();
				sleep(3);
				wrong_vers=TRUE;
			}
			else
			{
				wrong_vers=FALSE;
			}
		}
		if (input_source & IPRT)
		{
			st = load_pdump();
			if (st!= IERR_OK)
			{
				sprintf(errmsg,"WARNING: Unknown version of Master process %s   ",DAEMONNAME);
				statusl(errmsg);
				vbell();
				sleep(3);
				wrong_vers=TRUE;
			}
			else
			{
				wrong_vers=FALSE;
			}
		}
		if (input_source & ICONF)
		{
			st = load_qconfig();
			if (st!= IERR_OK)
			{
				sprintf(errmsg,"WARNING: Unknown version of Master process %s   ",DAEMONNAME);
				statusl(errmsg);
				vbell();
				sleep(3);
				wrong_vers=TRUE;
			}
			else
			{
				wrong_vers=FALSE;
			}
		}
	}
	if (input_source & ITIME)
	{
		vset(CURSOR,INVISIBLE);
		show_td();
		vset(CURSOR,VISIBLE);
	}
	return retval;
	
}
load_items(void)
{
	int qpos, qcnt;
	QITEM *qcur;
	int ppos, pcnt;
	PITEM *pcur;
	
	int tmp,i,cmpqitems(char **pi1, char **pi2);

	if (screen_mode==MODE_EDIT)
	  return;
	if (the_screen== &main_screen)
	{
		ring_count(queue_list,&qcnt);
	
		if (users_or_all==SHOW_USERS)
		{
			for (i=item_cnt=qpos=0; qpos<qcnt; ++qpos)
			{
				ring_get(queue_list,qpos,&qcur);
				if (qcur->owner_uid==my_uid)
				{
					items[i]=(char*)qcur;
					++i;
					++item_cnt;
				}
			}
		}
		else
		{
			for (i=item_cnt=qpos=0; qpos<qcnt; ++qpos,++i,++item_cnt)
			{
				ring_get(queue_list,qpos,&qcur);
				items[i]=(char*)qcur;
			}
		}
		items[i]=NULL;
		if (top_line_pos > i)
		{
			top_line_pos = i-(ITEM_END_LINE-ITEM_ST_LINE);
			if (top_line_pos<0) 
			  top_line_pos=0;
		}
		qsort(items,i,sizeof(char*),cmpqitems);
	}
	else
	{
		ring_count(printer_list,&pcnt);

		for(i=item_cnt=ppos=0; ppos<pcnt; ++ppos,++i,++item_cnt) 
		{
			ring_get(printer_list,ppos,&pcur);
			items[i]=(char*)pcur;
		}
		items[i]=NULL;
	}
	
	if (item_cnt && pos_on_screen== -1) pos_on_screen=0;
	
	for (i = top_line_pos; i<top_line_pos+ITEM_END_LINE-ITEM_ST_LINE && items[i]; ++i)
	{
		if (the_screen == &main_screen)
		{
			if (saveqpos == ((QITEM*)items[i])->q_id)
			{
				pos_on_screen = i-top_line_pos;
				break;
			}
		}
		else
		{
			if (saveppos == ((PITEM*)items[i])->printer_id)
			{
				pos_on_screen = i-top_line_pos;
				break;
			}
		}
	}
#if 0
	tmp=optimization;
	optimization=TRACKING_ONLY;
#endif
	vmove(ITEM_ST_LINE+pos_on_screen,0);
#if 0
	optimization=tmp;
#endif
}
cmpqitems(char **pi1, char **pi2)
{
	QITEM *i1, *i2;
	
	i1 = (QITEM*)*pi1;
	i2 = (QITEM*)*pi2;

	return i1->q_pos - i2->q_pos;
	
}

show_item(char *ptr)
{
	int i;
	char *disp;
       
	vmove(ITEM_ST_LINE+pos_on_screen,1);
	(*the_screen->fmtline)(ptr,ITEM_ST_LINE+pos_on_screen,1);
}
show_list(void)
{
	
	QITEM *qi;
	PITEM *pi;
	int line,i;
	int ipos;

	if (screen_mode==MODE_EDIT)
	  return;
	
	vset(CURSOR,INVISIBLE);
	vmode(underline_attr);
	if (the_screen->dmode==0)
		mvaddstr(3,1,the_screen->header1_line);
	else
		mvaddstr(3,1,the_screen->header2_line);
	vmode(0);
	if (the_screen == &main_screen)
	{
		int max= -1;
		int len;
		
		for (line=ITEM_ST_LINE,i=top_line_pos; line<=ITEM_END_LINE && items[i]; ++line,++i)	
		{
			qi = (QITEM *)items[i];
			if ((len=strlen(qi->real_path)) > max)
			{
				max = len;
			}
		}
		the_screen->maxlen = max;
	}
	else
	{
		int max= -1;
		int len;
		
		for (line=ITEM_ST_LINE,i=top_line_pos; line<=ITEM_END_LINE && items[i]; ++line,++i)	
		{
			pi = (PITEM *)items[i];
			if ((len=strlen(pi->printer_dev)) > max)
			{
				max = len;
			}
		}
		the_screen->maxlen = max;
	}
	for (line=ITEM_ST_LINE,i=top_line_pos; line<=ITEM_END_LINE && items[i]; ++line,++i)	
	{
		(*the_screen->fmtline)(items[i],line,1);
	}
	if((line-ITEM_ST_LINE-1)<pos_on_screen) pos_on_screen=(line-ITEM_ST_LINE-1);
	
	while (line<=ITEM_END_LINE)
	  mvaddstr(line++,1,"										    ");

	refresh();
	the_screen->list_changed=FALSE;
	vset(CURSOR,VISIBLE);
}
char *fmtqitem(char *data, int row, int col)
{
	static char dispbuf[200];
	char *status, *uname, *dprinter, *c_username(int uid), pnum[10];
	char *banner,*disposition, *approx_size(struct q_item *qi);
	QITEM *qd;
	int plen;
	char fixedpath[35], fixedform[9], fixedprt[9], fixedname[9];
	char epstr[10];
	
	qd=(QITEM*)data;
	plen = strlen(qd->real_path);

	if (qd->endpage==99999)
	{
		strcpy(epstr,"ALL  ");
	}
	else
	{
		sprintf(epstr,"%-5d",qd->endpage);
	}
	
	if (the_screen->dmode==0)
	{
		
		if	(qd->status & QST_DELETE) status="Deleted ";
		else if (qd->status & QST_REMOVE) status="Removed ";
		else if (ISPRTRERR(qd))		  status="ChgForm ";
		else if (ISHOLDING(qd))		  status="Hold	  ";
		else if (ISQSTOPPED(qd))	  status="Stopped ";
		else if (ISUNSPOOLED(qd))	  status="Printing";
		else				  status="	  ";

		uname = c_username(qd->owner_uid);
		
		if (strlen(qd->actual_prtr))
		  dprinter = qd->actual_prtr;
		else if (strlen(qd->requested_prtr))
		  dprinter = qd->requested_prtr;
		else
		{
			if (qd->prtnum>0)
			{
				sprintf(pnum,"#%d",qd->prtnum);
				dprinter = pnum;
			}
			else 
			  dprinter = " ";
		}
		if (the_screen->maxlen < (34+the_screen->scroll))
		{
			the_screen->scroll = the_screen->maxlen - 34;
		}
		sprintf(dispbuf,Q_FMT1_STR,
			qd->q_pos,
			showfield(qd->real_path, plen, plen-(34+the_screen->scroll) ,34,fixedpath),
			showfield(uname,0,0,8,fixedname),
			showfield(qd->form,0,0,8,fixedform),
			qd->class?qd->class:' ',
			showfield(dprinter,0,0,8,fixedprt),
			status);
	}
	else
	{
		if (qd->mode & QMD_NOBANNER)
		  banner = "No";
		else 
		  banner = "Yes";

		if (qd->mode & QMD_RESPOOLHOLD)	     disposition="ReQueue";
		else if (qd->mode & QMD_DELETEAFTER) disposition="Delete ";
		else				     disposition="	 ";
		
		if (the_screen->maxlen < (15+the_screen->scroll))
		{
			the_screen->scroll = the_screen->maxlen - 15;
		}
		sprintf(dispbuf,Q_FMT2_STR,
			qd->q_pos,
			showfield(qd->real_path,plen, plen-(15+the_screen->scroll), 15, fixedpath),
			qd->q_id,
			qd->copies, 
			approx_size(qd), 
			qd->stpage, 
			epstr,
			banner,
			disposition);
	}		
	vmove(row,col);
	if (((QITEM*)data)->owner_uid==my_uid) vmode(BOLD);
	else vmode(NORMAL);
	vprint("%s",dispbuf);
	verase(TO_EOL);
	if (((QITEM*)data)->owner_uid==my_uid) vmode(NORMAL);
	if (screen_mode==MODE_EDIT && row==edit_line)
	{
		vmode(underline_attr);
		if (the_screen->dmode==0)
		{
			vmove(row,qfpos1[0]);
			vprint("%-3d",qd->q_pos);
			vmove(row,qfpos1[1]);
			vprint("%-34s", showfield(qd->real_path,0,0,34,fixedpath));
			vmove(row,qfpos1[2]);
			vprint("%-8.8s", showfield(qd->form,0,0,8,fixedform));
			vmove(row,qfpos1[3]);
			vprint("%c",qd->class?qd->class:' ');
			vmove(row,qfpos1[4]);
			vprint("%-8s",showfield(dprinter,0,0,8,fixedprt));
			vmode(0);
		}
		else
		{
			vmove(row,qfpos2[0]);
			vprint("%-3d",qd->q_pos);
			vmove(row,qfpos2[1]);
			vprint("%-15s", showfield(qd->real_path,0,0,15,fixedpath));
			vmove(row,qfpos2[2]);
			vprint("%-5d",qd->copies);
			vmove(row,qfpos2[3]);
			vprint("%-5d",qd->stpage);
			vmove(row,qfpos2[4]);
			vprint("%5.5s",epstr);
			vmove(row,qfpos2[5]);
			vprint("%3.3s",banner);
			vmove(row,qfpos2[6]);
			vprint("%7.7s",disposition);
		}		
		vmode(NORMAL);
	}
}
char *fmtpitem(char *data, int row, int col)
{
	static char dispbuf[200];
	char dev[40];
	PITEM *pd;
	int line,i,plen;
	char *status, *showfield(char *buf, int bufsz, int wpos, int wsize, char *showbuf), pnum[10];
	char fixedprt[9],fixeddev[23],fixeddform[9],fixedcform[9],fixedmodel[13];
	pd=(PITEM*)data;
		
	if	(!ISENABLED(pd))     status="Disabled ";
	else if (ISERROR(pd))	     status="Error    ";
	else if (WRONGFORM(pd))	     status="WrongForm";
	else if (ISINUSEBYOTHER(pd)) status="Busy     ";
	else if (ISIDLE(pd))	     status="Idle     ";
	else if (ISBUSY(pd))	     status="Printing ";
	else if (ISPSTOPPED(pd))     status="Stopped  ";
	else			     status="	      ";
	plen = strlen(pd->printer_dev);
	if (pd->prtnum >0 ) sprintf(pnum,"#%d",pd->prtnum);
	else strcpy(pnum,"    ");
	if (screen_mode==MODE_NORM)
	{
		if (the_screen->dmode==0)
		{
			if (the_screen->maxlen < (22+the_screen->scroll))
			{
				the_screen->scroll = the_screen->maxlen - 22;
			}
			sprintf(dispbuf,P_FMT1_STR,
				showfield(pd->printer_name,0,0,8,fixedprt),
				pd->mode & PMD_DEVICE ? ' ':
				pd->mode & PMD_PROGRAM ? '|': ' ',
				showfield(pd->printer_dev,plen,plen-(22+the_screen->scroll),22,fixeddev),
				showfield(pd->default_form,0,0,8,fixeddform),
				showfield(pd->current_form,0,0,8,fixedcform),
				pd->class,
				status);
		}
		else
		{
			char qidstr[25];

			if (WRONGFORM(pd) || ISBUSY(pd) || ISPSTOPPED(pd))
			  sprintf(qidstr,"%-5d",pd->qid);
			else
			  strcpy(qidstr,"     ");
			if (the_screen->maxlen < (22+the_screen->scroll))
			{
				the_screen->scroll = the_screen->maxlen - 22;
			}
			sprintf(dispbuf,P_FMT2_STR,
				showfield(pd->printer_name,0,0,8,fixedprt),
				pd->mode & PMD_DEVICE ? ' ':
				pd->mode & PMD_PROGRAM ? '|': ' ',
				showfield(pd->printer_dev,plen,plen-(22+the_screen->scroll),22,fixeddev),
				pnum,
				qidstr,
				showfield(pd->printer_model,0,0,12,fixedmodel));
		}
		vmove(row,col);
		vprint("%s",dispbuf);
		verase(TO_EOL);
	}
	if (screen_mode==MODE_EDIT && row==edit_line)
	{
		vmode(underline_attr);
		if (the_screen->dmode==0)
		{
			vmove(row,pfpos1[0]);
			vprint("%-23s",showfield(pd->printer_dev,0,0,23,fixedcform));
			vmove(row,pfpos1[1]);
			vprint("%-8s",showfield(pd->current_form,0,0,8,fixedcform));
			vmove(row,pfpos1[2]);
			vprint("%-15s",pd->class);
		}
		vmode(NORMAL);
	}
}
mvaddcen(int line, char *str)
{
	int len;
	len=strlen(str);
	
}
char *time12(int hour, int min)
{
	char aorp;
	static char timebuf[10];
	
	if (hour>=0 && hour<12) 
	{
		aorp='a';
		if (hour==0) hour=12;
	}
	else if (hour>=12)
	{
		aorp='p';
		if (hour!=12) hour -= 12;
	}
	sprintf(timebuf,"%2d:%02d %cm",hour,min,aorp);
	return timebuf;
}
char *str_td(void)
{
	time_t clock,time();
	struct tm *curtime,*localtime(CONST time_t *);
	extern char *months[],*days[];
	char *time12(int hour, int min);
	static char tdbuf[50];
	
	clock	= time(0);
	curtime = localtime(&clock);
	sprintf(tdbuf,"%9s %9s %2d, 19%2d %s",
		days[curtime->tm_wday],
		months[curtime->tm_mon],
		curtime->tm_mday, curtime->tm_year,
		time12(curtime->tm_hour,curtime->tm_min));
	return tdbuf;
}
char *str_u(void)
{
	static char ubuf[20];
	char *cuserid(char *);
	
	sprintf(ubuf,"User: %-8.8s",cuserid(0));
	return ubuf;
}
show_fkeys(void)
{
	FKEY_ACTION *ptr;
	int pos,i,x,y;
	char showtmp[200],show[200];
	
	vmode(0);
	vset(CURSOR,INVISIBLE);
	pos = 0;
	ptr = the_screen->fkeys;

	vstate(-1);
	voptimize(DEFER_MODE);

	while (ptr->altkeyval)
	{
		y=ptr->row;
		x=ptr->col;
  
		if (ptr->showcond && strlen(ptr->flabel)) 
		{
			if (y != -1) 
			{
				if ((*ptr->showcond)()) 
				{
					if (strcmp(ptr->flabel,ptr->nlabel))
					{
						if (!wang_mode)
						{
							sprintf(showtmp,"%s=%s",ptr->flabel,ptr->nlabel);
						}
						else
						{
							sprintf(showtmp,"%s%s",ptr->flabel,ptr->nlabel);
						}
						sprintf(show,"%-*s",ptr->width,showtmp);
					}
					else
					{
						sprintf(show,"%-*s",ptr->width,ptr->flabel);
					}
					mvaddstr(y,x,show);
				}
				else
				{
					mvaddstr(y,x,repchar(ptr->width,(int)' '));
				}
			}
		}
		else		
		{
			if (y != -1)
			{
				if (ptr->flabel && strlen(ptr->flabel) && strcmp(ptr->flabel,ptr->nlabel))
				{
					if (!wang_mode)
					{
						sprintf(showtmp,"%s=%s",ptr->flabel,ptr->nlabel);
					}
					else
					{
						sprintf(showtmp,"%s%s",ptr->flabel,ptr->nlabel);
					}
					sprintf(show,"%-*s",ptr->width,showtmp);
				}
				else
				{
					sprintf(show,"%-*s",ptr->width,ptr->flabel);
				}
				mvaddstr(y,x,show);
			}
			
		}
		++ptr;
	}

	voptimize(DEFER_MODE);
	vstate(1);
	vset(CURSOR,VISIBLE);
}

void graceful_exit(dummy)
int dummy;
{
	vexit();
	exit(1);
	
}
cangotop(void)
{
	if (top_line_pos && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
cangobot(void)
{
	if ((item_cnt-1)-top_line_pos > ITEM_LINES  && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
cangoup(void)
{
	if (top_line_pos > 0  && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
cangodn(void)
{
	if (top_line_pos + ITEM_LINES < (item_cnt-1) && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
doup(void)
{
	if (screen_mode != MODE_NORM) return;
	
	clearstatusl();
	if (pos_on_screen > 0) 
	{
		--pos_on_screen;
	}
	else
	if (top_line_pos > 0)
	{
		extern int vcapnull();/* in video vcap.c -- check whether cap is defined */
		extern char *scrarea_esc;
		
		--top_line_pos;
		if (vcapnull(scrarea_esc,NULL,0)==0)
		{
			vroll(ITEM_ST_LINE,ITEM_END_LINE);
			vmove(ITEM_ST_LINE,0);
			/*vprint("\n");*/
			
			vlinefeed(REVERSE);
			
			if (the_screen == &main_screen) 
			  if (((QITEM*)items[top_line_pos])->owner_uid == my_uid) vmode(BOLD);

			(*the_screen->fmtline)(items[top_line_pos],ITEM_ST_LINE,1);

			if (the_screen == &main_screen) 
			  vmode(0);
		
			vroll(0,23);
		}
		else
		{
			show_list();
		}
	}
}
dodn(void)
{
	clearstatusl();
	if (screen_mode != MODE_NORM) return;
	
	if (pos_on_screen < ITEM_LINES && (top_line_pos + pos_on_screen < (item_cnt-1)))
	{
		++pos_on_screen;
	}
	else
	if (top_line_pos + ITEM_LINES < item_cnt-1)
	{
		extern int vcapnull();/* in video vcap.c -- check whether cap is defined */
		extern char *scrarea_esc;
		
		++top_line_pos;
		if (vcapnull(scrarea_esc,NULL,0)==0)
		{
			vroll(ITEM_ST_LINE,ITEM_END_LINE);
			mvaddstr(ITEM_END_LINE,screen_width-1,"\n");
			
			if (the_screen == &main_screen) 
			  if (((QITEM*)items[top_line_pos+ITEM_LINES])->owner_uid == my_uid) vmode(BOLD);
			
			(*the_screen->fmtline)(items[top_line_pos+ITEM_LINES],ITEM_END_LINE,1);
			if (the_screen == &main_screen) 
			  vmode(0);
			
			vroll(0,23);
		}
		else
		{
			show_list();
		}
	}
} 
dopup(void)
{
	if (!cangoup()) return;
	
	clearstatusl();
	if (pos_on_screen) 
	{
		pos_on_screen=0;
		return;
	}
	if (top_line_pos >= ITEM_LINES) 
		top_line_pos-=ITEM_LINES;
	else
		top_line_pos=0;

	
	show_list();
} 
dopdn(void)
{
	if (!cangodn()) return;
	
	clearstatusl();
	if (pos_on_screen < ITEM_LINES)
	{
		pos_on_screen=ITEM_LINES;
		return;
	}
	if ((item_cnt-1)-(top_line_pos+ITEM_LINES) >= ITEM_LINES)
		top_line_pos += ITEM_LINES;
	else
	{
		top_line_pos= (item_cnt-1) - (ITEM_LINES+0) ;
	}

	
	show_list();
} 
dotop(void)
{
	int needtoshow;
	
	if (!cangotop()) return;
	
	clearstatusl();
	if (top_line_pos) needtoshow=TRUE;
	else needtoshow=FALSE;
	
	pos_on_screen=0;
	pos_in_list=0;
	top_line_pos=0;

	if (needtoshow) 
	  show_list();
} 
dobot(void)
{
	int savepos;
	
	if (!cangobot()) return;
	
	clearstatusl();
	
	pos_on_screen = ITEM_LINES;

	savepos = top_line_pos;
	top_line_pos = (item_cnt-1);
	top_line_pos -= ITEM_LINES;

	if (savepos != top_line_pos)
	  show_list();
	
}
save_pos(void)
{
	if (pos_on_screen == -1) 
	  return;
	if (the_screen == &main_screen)
	{
		saveqpos = ((QITEM*)items[pos_on_screen+top_line_pos])->q_id;
	}
	else
	{
		saveppos = ((PITEM*)items[pos_on_screen+top_line_pos])->printer_id;
	}
}
int get_bool(int *pval, int size, int row, int col, int dummy, int RO, int spaceok)
{
    int c;
    int val,
	where_y,
	where_x;

    where_y=row;
    where_x=col;
    vmode(BOLD);

    val = *pval;
    
    while (1)
    {
	if (val)
	{
		vmode(underline_attr);
		mvaddstr(where_y, where_x, "Yes");
		vmode(0);
	}
	else
	{
		vmode(underline_attr);
		mvaddstr(where_y, where_x, "No ");
		vmode(0);
	}
	vmove(row,col);
	
	c =vgetm();
	if (c=='\n' || c=='\r' ||
	    c=='\t' || c==return_key || c==enter_key ||
	    c==tab_key || c==backtab_key)
	{
		if (val)
		{
			vmode(underline_attr);
			mvaddstr(where_y, where_x, "Yes");
			vmode(0);
		}
		else
		{
			vmode(underline_attr);
			mvaddstr(where_y, where_x, "No ");
			vmode(0);
		}
		*pval=val;
		return c;
	}
	else if (c== ' ')
	{
	    val = !val;
	}
	else if (c=='y' ||
	    c=='Y')
	{
	    val = 1;
	}
	else if (c=='n' ||
	    c=='N')
	{
	    val = 0;
	}
	else
	  vbell();
		
    }
}
int get_disp(int *pval, int size, int row, int col, int dummy, int RO, int spaceok)
               
              
             
             
                /* get disposition */
               
                  
{
    int c;
    int val,
	where_y,
	where_x;

    where_y=row;
    where_x=col;
    vmode(BOLD);

    val = *pval;
    
    while (1)
    {
	if (val&QMD_RESPOOLHOLD)
	{
		vmode(underline_attr);
		mvaddstr(where_y, where_x, "ReQueue");
		vmode(0);
	}
	else if (val&QMD_DELETEAFTER)
	{
		vmode(underline_attr);
		mvaddstr(where_y, where_x, "Delete ");
		vmode(0);
	}
	else
	{
		vmode(underline_attr);
		mvaddstr(where_y, where_x, "       ");
		vmode(0);
	}
	vmove(row,col);
	
	c =vgetm();
	if (c=='\n' || c=='\r' ||
	    c=='\t' || c==return_key || c==enter_key ||
	    c==tab_key || c==backtab_key)
	{
		if (val&QMD_RESPOOLHOLD)
		{
			vmode(underline_attr);
			mvaddstr(where_y, where_x, "ReQueue");
			vmode(0);
		}
		else if (val&QMD_DELETEAFTER)
		{
			vmode(underline_attr);
			mvaddstr(where_y, where_x, "Delete ");
			vmode(0);
		}
		else
		{
			vmode(underline_attr);
			mvaddstr(where_y, where_x, "	     ");
			vmode(0);
		}
		*pval=val;
		return c;
	}
	else if (c== ' ')
	{
		if (val&QMD_RESPOOLHOLD)
		{
			val &= ~QMD_RESPOOLHOLD;
			val |= QMD_DELETEAFTER;
		}
		else if (val&QMD_DELETEAFTER)
		{
			val &= ~(QMD_RESPOOLHOLD|QMD_DELETEAFTER);
		}
		else 
		{
			val |= QMD_RESPOOLHOLD;
		}
	}
	else if (c=='r'|| c== 'R')
	{
		val &= ~QMD_DELETEAFTER;
		val |= QMD_RESPOOLHOLD;
	}
	else if (c=='d'|| c== 'D')
	{
		val &= ~QMD_RESPOOLHOLD;
		val |= QMD_DELETEAFTER;
	}
	else
	   vbell();
    }
}
#define DECPOS { if (wsize==1) ;        	 \
                 else				 \
                 {				 \
		  if (pos == (wpos?1:0))	 \
		  {				 \
		     if (wpos) --wpos;		 \
		  }				 \
		  else				 \
		    --pos;			 \
                 } 				 \
	       } 
#define INCPOS { if (wsize==1) ;		 \
                 else                            \
                 {                               \
		  if (pos == (wpos+wsize<bufsz?wsize-2:wsize-1))\
		  {				 \
		      if (wpos + wsize < bufsz+1)\
			++wpos;			 \
		  }				 \
		  else				 \
		    ++pos;			 \
                 }                               \
	       }
get_string(char *string, int wsize, int row, int col, int max, int RO, int spaceok)
{
	int c;
	int pos, wpos;
	int opt;
	int bufsz;
	static char gsbuf[255];
	static char copybuf[255];
/*#define DEBUGGS*/
#ifdef DEBUGGS	
	FILE *tmp;
	tmp=fopen("/dev/ttyp0","w");	/* use appropriate tty */
#endif

#if 0
	opt=optimization;
	optimization=TRACKING_ONLY;
#endif	
	wpos=pos=0;
	bufsz=strlen(string);

	vmode(underline_attr);
	vmove(row,col);
	vprint(showfield(string,bufsz,wpos,wsize,gsbuf));

	while (1)
	{
		vmove(row,col+pos);
		
		c=getch();
		if (c=='\n' || c=='\r' ||
		    c=='\t' || c==return_key || c==enter_key ||
		    c==tab_key || c==backtab_key)
		{	
#if 0
			optimization=opt;
#endif
			if (max>1)
			{
				string[max]=(char)0;
				if (!spaceok)
				{
					truncspc(string);
				}
			}
			return c;
		}
		else if (c==edit_left_main || c==edit_left_alt)
		{
			DECPOS;
		}
		else if (c==edit_right_main || c==edit_right_alt)
		{	
			INCPOS;
		}
		else if (RO)
		{
			vbell();
		}
		else if (c==edit_delleft_main || c==edit_delleft_alt)
		{
			if (pos+wpos)
			{
				memcpy(copybuf,string+pos+wpos,bufsz-(pos+wpos-1));
				memcpy(string+pos+wpos-1,copybuf,bufsz-(pos+wpos-1));
				--bufsz;
				string[bufsz]=(char)0;
			}
			DECPOS;
		}
		else if (c==edit_delrt_main || c==edit_delrt_alt)
		{
			if (bufsz && bufsz>wpos+pos) 
			{
				memcpy(copybuf,string+pos+wpos+1,bufsz-(pos+wpos-1));
				memcpy(string+pos+wpos,copybuf,bufsz-(pos+wpos+1));
				--bufsz;
				string[bufsz]=(char)0;
			}
		}
		else if (c==edit_ins_main || c==edit_ins_alt)
		{	
			if (bufsz < max)
			{
				memcpy(copybuf,string+pos+wpos,bufsz-pos);
				string[pos+wpos]=' ';
				memcpy(string+pos+wpos+1,copybuf,bufsz-pos);
				++bufsz;
			}
		}
		else 
		{	
			if (c>=' ' && c<='~') 
			{
				if (pos+wpos < max)
				{
					string[pos+wpos]=c;
					if (pos+wpos >= bufsz) 
					  ++bufsz;
					INCPOS;
				}
			}
		}
		mvaddstr(row,col,showfield(string,bufsz,wpos,wsize,gsbuf));
#ifdef DEBUGGS
		fprintf(tmp,"pos=%5d, wpos=%5d, bufsz=%5d, wsize=%5d, max=%5d, buf=%s\n",
			pos,wpos,bufsz,wsize,max,string);
		fflush(tmp);
#endif
	}
#ifdef DEBUGGS
	fclose(tmp);
#endif
}

char *showfield(char *buf, int bufsz, int wpos, int wsize, char *showbuf)
               
               
              
               
                   
           /* contains data to be displayed */
           /* size of this data */
           /* starting point in the data to display */
           /* size of the 'window' to display */
              
{
	static char myshowbuf[255];
	int tocopy;
	
	if (showbuf==NULL)
	  showbuf=myshowbuf;
	
	if (bufsz==0)
	{
		bufsz=strlen(buf);
	}
	if (wpos<0 || wpos>bufsz) 
	{
		wpos=0;
	}
	memset(showbuf,' ',wsize);
	if (bufsz - wpos< wsize)
	  tocopy = bufsz - wpos;
	else
	  tocopy = wsize;
	memcpy(showbuf,&buf[wpos],tocopy);
	showbuf[wsize]=(char)0;

	if (wpos) showbuf[0]='+';
	if (bufsz > wpos+wsize) showbuf[wsize-1]='+';
	return showbuf;
}
statusl(char *p)
{
	static int status=FALSE;
	
	if (!p)
	{
		if (status)
		{
			p=erasefield;
			status=FALSE;
		}
	}
	else
	{
		status=TRUE;
	}
	if (p)
	{
		vset(CURSOR,INVISIBLE);
		vstate(-1);
		vmove(19,1);
		vprint(p);
		vstate(1);
	}
}

int get_int(int *pval, int field_width, int row, int col, int max, int RO, int spaceok)
{
	char tmp[200];
	int key;
	
	do
	{
		if (*pval == 99999)
		{
			strcpy(tmp,"ALL  ");
		}
		else
		{
			sprintf(tmp,"%d",*pval);
		}
		key=get_string(tmp,field_width,row,col,max,RO,spaceok);
		lowerstring(tmp);
		if (!strncmp(tmp,"all",3))
		{
			strcpy(tmp,"99999");
		}			  
		if (atoi(tmp)==0) vbell();
		
	} while (atoi(tmp)==0);
	
	*pval=atoi(tmp);
	return key;
}
static int lowerstring(p)
char *p;
{	
	while (*p)
	{
		*p = tolower(*p);
		++p;
	}
}
clearstatusl(void)
{	
	statusl(NULL);
}
char *approx_size(struct q_item *qi)
{
	struct stat sbuf;
	static char abuf[100];
	int size;
	
	if (qi->size)
	  size = qi->size;
	else
	{
		stat(qi->real_path,&sbuf);
		qi->size = size = sbuf.st_size;
	}
	sprintf(abuf,"%dk",size/1000);
	return abuf;
}

/*
 * $Log: man_scr.c,v $
 * Revision 1.47  1993/10/21  17:37:43  jockc
 * fix bomb on other info if filenames are short
 *
 * Revision 1.46  1993/10/12  23:23:36  jockc
 * cast a strlen for sequent.  change so that empty function key slots
 * are erased (when changing screens.)  do not look at global_key unless
 * it is less than VMBIAS.  tolower() on sequent returned bogus values.
 *
 * Revision 1.45  1993/09/30  23:00:15  jockc
 * don't show qid # for printer if ERROR status
 *
 * Revision 1.44  1993/09/30  00:38:38  jockc
 * adjusted null term back
 *
 * Revision 1.43  1993/09/30  00:35:34  jockc
 * slight adjust to null term on get_string
 *
 * Revision 1.42  1993/09/29  23:42:13  jockc
 * use other attributes if underline is not avail
 *
 * Revision 1.41  1993/09/15  01:12:23  jockc
 * change edit behavior to handle ALL in place of 99999
 *
 * Revision 1.40  1993/09/15  00:24:25  jockc
 * make letter commands case insensitive
 *
 * Revision 1.39  1993/09/14  16:30:43  jockc
 * improved statusl line to prevent cursor jumping
 *
 * Revision 1.38  1993/09/13  23:03:05  jockc
 * small change attempting to fix occasional missed update.
 * user input processed first caused the queue update to be bypassed.
 * time was being updated every time.. fixed to work properly.
 *
 * Revision 1.37  1993/09/13  15:28:24  jockc
 * 499
 * 503
 *
 * Revision 1.37  1993/09/10  18:29:44  jockc
 * massive changes to support new key defs.. shutdown and relink
 * of comm link if daemon shuts down and restarts.
 *
 * Revision 1.36  1993/08/13  22:08:46  jockc
 * const
 *
 * Revision 1.35  1993/08/13  20:48:39  jockc
 * change int to time_t for clock
 *
 * Revision 1.34  1993/08/13  19:03:07  jockc
 * add proto, begin change for terminfo, use ring() instead old list mgmt
 *
 * Revision 1.33  1993/05/14  23:08:07  jockc
 * change to underline banner and disposition settings to make it
 * more apparent to the user that they can be changed
 *
 * Revision 1.32  1993/03/01  23:34:16  jockc
 * replace refs to VMBIAS and other VCAP stuff to the variables
 * xxxx_key to allow compatibility with various levels of video
 *
 * Revision 1.31  1993/01/12  02:09:10  jockc
 * attempting to better handle a shut down daemon
 *
 * Revision 1.30  1993/01/05  01:32:42  jockc
 * improved loading of the data files a bit
 *
 * Revision 1.29  1992/12/31  23:48:56  jockc
 * moved the truncspc from d_main:change() to here:get_string().
 * this should fix the intermittent AIX3.2 shutdown bug.  Apparently
 * the daemon would occasionally receive a buffer from the client
 * containing parts that were not properly null terminated.  truncspace
 * in get_string should do the trick
 *
 * Revision 1.28  1992/10/28  18:02:08  root
 * took out needless check in dobot(). sometimes pf3 didn't work
 *
 * Revision 1.27  1992/10/27  23:34:07  jockc
 * reposition top_line_pos if all/mine toggle puts
 * top_line_pos past end of valid items
 *
 * Revision 1.26  1992/10/09  20:18:35  jockc
 * just renamed a few functions
 *
 * Revision 1.25  1992/07/02  18:18:22  jockc
 * added code to repos cursor if items shift, fixed messy
 * behavior if you try to edit while the screen is moving
 * a lot
 *
 * Revision 1.24  1992/06/29  21:50:59  jockc
 * fixed bogus return in INCPOS and DECPOS macros
 * that was leaving optimization off.. took out
 * vprint \n that was messing with scroll down
 * .,
 *
 * Revision 1.23  1992/06/08  23:56:17  jockc
 * polish code to remove some warnings
 *
 * Revision 1.22  1992/05/22  21:08:02  jockc
 * overwritten byte in pnum in fmtpitem due to
 * bounds err on model char[]
 *
 * Revision 1.21  1992/05/08  20:16:41  jockc
 * slight change to get string routine.  1 char fields (no movement allowed).
 * in the default (normal char input) case, check wpos+pos, not bufsz for
 * input allowed condition
 *
 * Revision 1.20  1992/05/07  22:27:17  jockc
 * changes to handle all fields whose length can exceed the screen field size
 * and editing of such fields
 *
 * Revision 1.19  1992/04/30  19:19:28	jockc
 * stat qi->real_path, not "file"
 * in the size compute code
 *
 * Revision 1.18  1992/04/30  19:12:20	jockc
 * change to show size in k instead of lines,
 * add printer model; back to printer#0 = not specified
 *
 * Revision 1.17  1992/04/29  21:09:00	jockc
 * change a vprintf (?) to a vprint.  add function clearstatusl to
 * clear the status line
 *
 * Revision 1.16  1992/03/27  23:14:26	jockc
 * changed chkprt to chgform, formchng to wrongform, added busy status
 * support for printers with no pnum
 *
 * Revision 1.15  1992/03/13  21:39:40	jockc
 * add code to support long printer devices (programs), prefix with '|'
 * if it's a program, '+' if it's too long for the field
 * fixed up scroll problem by positioning cursor to top line of scroll range
 *
 * Revision 1.14  1991/12/17  22:36:09	jockc
 * added disp change, clear of statusline, check for
 * file's existence before allowing operation
 *
 * Revision 1.13  1991/10/11  19:58:55	jockc
 * none
 *
 * Revision 1.12  1991/10/11  19:23:40	jockc
 * detected bad comm status = status line message
 *
 * Revision 1.11  1991/10/10  22:40:01	jockc
 * no f14 available from printer edit mode. show mode disabled if so.
 *
 * Revision 1.10  1991/10/04  20:48:27	jockc
 * fixup scroll thing for terminals without set scroll reg
 *
 * Revision 1.9	 1991/10/01  17:10:39  jockc
 * support for reading dump files instead of rcving info from daemon
 *
 * Revision 1.8	 1991/08/05  23:03:00  jockc
 * added display logic for printer num
 *
 * Revision 1.7	 1991/08/05  20:00:22  jockc
 * add in copies to q format
 *
 * Revision 1.6	 1991/08/02  23:55:35  jockc
 * added '+' if file too big
 * added auto pos cursor if empty queue fills
 * added support for showall/mine functionality
 * fixed cursor on line -1 abort thing checking items[] before deref'ing
 * check current item validity before passing to function key handler
 *
 * Revision 1.5	 1991/07/22  23:03:27  jockc
 * fixed percent in out string bug
 * changed fmtpitem to show printer error
 * fixed edit of items not on first page bug
 * added code to repos cursor when items shift
 *
 * Revision 1.4	 1991/05/22  20:27:47  jockc
 * fixed stupid mistake in dodn and doup.. was calling
 * fmtitem not expecting display sideeffect
 *
 * Revision 1.3	 1991/04/30  17:44:05  jockc
 * misc cleanup
 *
 * Revision 1.2	 1991/04/19  00:47:32  jockc
 * *** empty log message ***
 *
 * Revision 1.1	 1991/04/18  23:51:57  jockc
 * Initial revision
 *
 *
 */
