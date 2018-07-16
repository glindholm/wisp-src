/*
 * Module:  ipman_screen
 * Program: IDSIprint
 * Purpose: main module for full screen interface to daemon
 *
 * $Log: man_scr.c,v $
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
static char rcsid[] = "$Id:$";
#define EXT extern 
#include "defs.h"
#include "manage.h"

in_screen(data)
char *data;			/* pointer to displayable info */
{
	extern SCREEN *the_screen;
	static SCREEN *last_screen=NULL;
	extern int comm_status;
	static int need_u_td=TRUE;
	int action;
	extern int showtop,showbot,canup,candn;

	showtop=showbot=canup=candn=1;

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
		
		mvaddstr(0,(screen_width/2)-(strlen(tmpbuf)/2),tmpbuf);
		attron(A_HI_UND);
/*		mvaddstr(3,1,the_screen->header_line); */ /* yes, update these lines */
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
		if(the_screen->list_changed) 
		{
			load_items();
#if 0
			if (the_screen== &main_screen)
			{
				int save;
				
				save=pos_on_screen;
				
				for(pos_on_screen=0; 
				    items[pos_on_screen+top_line_pos] 
				    && saveqpos != (((QITEM*)items[pos_on_screen + top_line_pos])->q_id)
				    && pos_on_screen < ITEM_LINES; 
				    ++pos_on_screen);
				if (! items[pos_on_screen+top_line_pos] 
				    || (((QITEM*)items[pos_on_screen+top_line_pos])->q_id != saveqpos)) pos_on_screen=save;
				
			}
			else
			{
				int save;
				
				save=pos_on_screen;
				
				for(pos_on_screen=0; 
				    items[pos_on_screen+top_line_pos] 
				    && saveppos != (((PITEM*)items[pos_on_screen + top_line_pos])->printer_id)
				    && pos_on_screen < ITEM_LINES; 
				    ++pos_on_screen);
				if (! items[pos_on_screen+top_line_pos] 
				    || (((PITEM*)items[pos_on_screen+top_line_pos])->printer_id != saveppos)) 
				  pos_on_screen=save;
			}
#endif			
			show_list();
		}
		action=process_input(data);
		switch(action)
		{
		       
		}
		if (comm_status!=COMM_OK)
		{
			char errmsg[81];
			sprintf(errmsg,"Warning: unable to contact daemon [errno=%d]",comm_status);
			statusl(errmsg);
		}
		show_pstatus(); /* show a printer's status if necessary */
		show_td();
	}
	while (action != LEAVE);
}
show_pstatus()
{
}

show_td()
{
	char *str_td();
	static int last=0;

	attroff(A_HI_UND);
	mvaddstr(1,43,str_td());
}
process_input(data)
char *data;
{
	int input_source,found;
	extern int global_key;
	FKEY_ACTION *ptr;
	int tmp;
	extern int optimization;
	
	if (screen_mode==MODE_NORM) show_fkeys();
	if (screen_mode==MODE_EDIT)
	{
		int ch;
		
		ch= (*(edfunarray[field])) (items[top_line_pos+pos_on_screen]+eddatarray[field],
					    edlenarray[field],
					    edit_line, 
					    edposarray[field],
					    edmaxarray[field]);
		if (ch==tab_key)
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
		if (ch==return_key)
		{
			stopedit(items[top_line_pos+pos_on_screen]);
		}
		if (ch==fn14_key && the_screen== &main_screen)
		{
			togglemode();
		}
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
				  for (found=FALSE,ptr = the_screen->fkeys; ptr->keyval; ++ptr)
				  {
					  if (ptr->keyval == global_key){ found=TRUE; break; }
				  }
			  }
			  switch (ptr->func)
			  {
				case ACT_RETVAL:
				  return (ptr->data).retval;
				  break;
				case ACT_FUNCALLARGS:
				  if (!items[top_line_pos+pos_on_screen]) return;
				  if (the_screen== &main_screen && 
				      access(((QITEM*)items[top_line_pos+pos_on_screen])->real_path,0))
				  {
					  statusl("File not found				      ");
					  return;
				  }
				  return (*ptr->data.func)(items[top_line_pos+pos_on_screen]);
				  break;
				case ACT_FUNCALL:
				  return (*ptr->data.func)();
				  break;
				case ACT_SCREEN:
				  if(screen_mode==MODE_NORM)
				    the_screen = ptr->data.screen;
				  return LEAVE;
			  }
		  }
	  }
	if (input_source & IQUEUE || input_source & IPRT)
	  save_pos();
	if (input_source & IQUEUE)
	{
		deletequeue();
		load_qdump();
	}
	if (input_source & IPRT)
	{
		deleteprinters();
		load_pdump();
	}
	if (input_source & ICONF)
	{
		load_qconfig();
	}
	else
	if (input_source & ITIME)	 return 0;
}
load_items()
{
	int tmp,i,cmpqitems();

	if (screen_mode==MODE_EDIT)
	  return;
	if (the_screen== &main_screen)
	{
		if (users_or_all==SHOW_USERS)
		{
			for(item_cnt=i=0, q_ptr=q_head; q_ptr; q_ptr=q_ptr->next) 
			  if (q_ptr->data->owner_uid==my_uid)
			  {
				  items[i]=(char*)q_ptr->data;
				  ++i;
				  ++item_cnt;
			  }
			
		}
		else
		{
			for(item_cnt=i=0, q_ptr=q_head; q_ptr; q_ptr=q_ptr->next,++i,++item_cnt) 
			    items[i]=(char*)q_ptr->data;
		}
		items[i]=NULL;
		qsort(items,i,sizeof(char*),cmpqitems);
	}
	else
	{
		for(item_cnt=i=0, p_ptr=p_head; p_ptr; p_ptr=p_ptr->next,++i,++item_cnt) items[i]=(char*)p_ptr->data;
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
	tmp=optimization;
	optimization=TRACKING_ONLY;
	vmove(ITEM_ST_LINE+pos_on_screen,0);
	optimization=tmp;
}
cmpqitems(pi1,pi2)
char **pi1, **pi2;
{
	QITEM *i1, *i2;
	
	i1 = (QITEM*)*pi1;
	i2 = (QITEM*)*pi2;

	return i1->q_pos - i2->q_pos;
	
}

show_item(ptr)
char *ptr;
{
	int i;
	char *disp;
       
	vmove(ITEM_ST_LINE+pos_on_screen,1);
	(*the_screen->fmtline)(ptr,ITEM_ST_LINE+pos_on_screen,1);
}
show_list()
{
	
	QITEM *qd;
	int line,i;
	int ipos;

	if (screen_mode==MODE_EDIT)
	  return;
	
	attron(A_HI_UND);
	if (the_screen->dmode==0)
		mvaddstr(3,1,the_screen->header1_line);
	else
		mvaddstr(3,1,the_screen->header2_line);
	attroff(0);
	for (line=ITEM_ST_LINE,i=top_line_pos; line<=ITEM_END_LINE && items[i]; ++line,++i)	
	{
		(*the_screen->fmtline)(items[i],line,1);
	}
	if((line-ITEM_ST_LINE-1)<pos_on_screen) pos_on_screen=(line-ITEM_ST_LINE-1);
	
	while (line<=ITEM_END_LINE)
	  mvaddstr(line++,1,"										    ");

	refresh();
	the_screen->list_changed=FALSE;
}
char *fmtqitem(data,row,col)
char *data;
{
	static char dispbuf[200];
	char *showfield();
	char *status, *uname, *dprinter, *username(), pnum[10];
	char *banner,*disposition, *approx_size();
	QITEM *qd;
	int plen;
	char fixedpath[35], fixedform[9], fixedprt[9], fixedname[9];
	qd=(QITEM*)data;
	plen = strlen(qd->real_path);
		
	if (the_screen->dmode==0)
	{
		
		if	(qd->status & QST_DELETE) status="Deleted ";
		else if (qd->status & QST_REMOVE) status="Removed ";
		else if (ISPRTRERR(qd))		  status="ChgForm ";
		else if (ISHOLDING(qd))		  status="Hold	  ";
		else if (ISQSTOPPED(qd))	  status="Stopped ";
		else if (ISUNSPOOLED(qd))	  status="Printing";
		else				  status="	  ";

		uname = username(qd->owner_uid);
		
		if (strlen(qd->requested_prtr))
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
		sprintf(dispbuf,Q_FMT1_STR,
			qd->q_pos,
			showfield(qd->real_path, plen, plen-34,34,fixedpath),
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
		
		sprintf(dispbuf,Q_FMT2_STR,
			qd->q_pos,
			showfield(qd->real_path,plen, plen-15, 15, fixedpath),
			qd->q_id,
			qd->copies, 
			approx_size(qd), 
			qd->stpage, 
			qd->endpage,
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
		vmode(BOLD|UNDERSCORE);
		if (the_screen->dmode==0)
		{
			vmove(row,qfpos1[0]);
			vprint("%-3d",qd->q_pos);
			vmove(row,qfpos1[1]);
			vprint("%-8.8s", showfield(qd->form,0,0,8,fixedform));
			vmove(row,qfpos1[2]);
			vprint("%c",qd->class?qd->class:' ');
			vmove(row,qfpos1[3]);
			vprint("%-8s",showfield(dprinter,0,0,8,fixedprt));
			vmode(0);
		}
		else
		{
			vmove(row,qfpos2[0]);
			vprint("%-3d",qd->q_pos);
			vmove(row,qfpos2[1]);
			vprint("%-5d",qd->copies);
			vmove(row,qfpos2[2]);
			vprint("%-5d",qd->stpage);
			vmove(row,qfpos2[3]);
			vprint("%-5d",qd->endpage);
		}		
		vmode(NORMAL);
	}
}
char *fmtpitem(data,row,col)
char *data;
{
	static char dispbuf[200];
	char dev[40];
	PITEM *pd;
	int line,i,plen;
	char *status, *showfield(), pnum[10];
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
			sprintf(dispbuf,P_FMT1_STR,
				showfield(pd->printer_name,0,0,8,fixedprt),
				pd->mode & PMD_DEVICE ? ' ':
				pd->mode & PMD_PROGRAM ? '|': ' ',
				showfield(pd->printer_dev,plen,plen-22,22,fixeddev),
				showfield(pd->default_form,0,0,8,fixeddform),
				showfield(pd->current_form,0,0,8,fixedcform),
				pd->class,
				status);
		}
		else
		{
			char qidstr[25];

			if (ISERROR(pd) || WRONGFORM(pd) ||
			    ISBUSY(pd) || ISPSTOPPED(pd))
			  sprintf(qidstr,"%-5d",pd->qid);
			else
			  strcpy(qidstr,"     ");
			sprintf(dispbuf,P_FMT2_STR,
				showfield(pd->printer_name,0,0,8,fixedprt),
				pd->mode & PMD_DEVICE ? ' ':
				pd->mode & PMD_PROGRAM ? '|': ' ',
				showfield(pd->printer_dev,plen,plen-22,22,fixeddev),
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
		vmode(BOLD|UNDERSCORE);
		if (the_screen->dmode==0)
		{
			vmove(row,pfpos1[0]);
			vprint("%-8s",showfield(pd->current_form,0,0,8,fixedcform));
			vmove(row,pfpos1[1]);
			vprint("%-15s",pd->class);
		}
		vmode(NORMAL);
	}
}
char *
repstr(ch,cnt)
char ch;
int cnt;
{
	static char repbuf[500];
	
	memset(repbuf,ch,cnt);
	repbuf[cnt]=(char)0;
	return repbuf;
}
mvaddcen(line,str)
int line;
char *str;
{
	int len;
	len=strlen(str);
	
}
char *time12(hour,min)
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
char *str_td()
{
	int clock;
	struct tm *curtime,*localtime();
	extern char *months[],*days[];
	char *time12();
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
char *str_u()
{
	static char ubuf[20];
	char *cuserid();
	
	sprintf(ubuf,"User: %-8.8s",cuserid(0));
	return ubuf;
}
show_fkeys()
{
	FKEY_ACTION *ptr;
	char *repstr();
	int pos,i,x,y;
	
	attroff(A_HI_UND);
	vmode(0);
	
	pos = 0;
	ptr = the_screen->fkeys;
	while (ptr->keyval)
	{
		y=sloty[ptr->slot];
		x=slotx[ptr->slot];
  
		if (ptr->showcond) 
		{
			if (ptr->slot != -1) 
			  if ((*ptr->showcond)()) 
			  {
				  mvaddstr(y,x,ptr->desc);
				  pos |= 1<< (ptr->slot-1);
			  }
		}
		else		
		{
			if (ptr->slot != -1) 
			{
				mvaddstr(y,x,ptr->desc);
				pos |= 1<< (ptr->slot-1);
			}
			
		}
		++ptr;
	}
	for(i=0; i<16; ++i)
	{
		y=sloty[i+1];
		x=slotx[i+1];
		
		if (!(pos & (1<<i)))
		{
			mvaddstr(y,x,repstr(' ',slotw[i+1]));
		}
	}
	
}
char *username(uid)
int uid;
{
	struct passwd *getpwuid();
	static int last_uid = -1;
	static struct passwd *pw;
	
	if (last_uid != uid) /* this should cut calls to getpwuid a bit */
	{
		pw=getpwuid(uid);
	}
	last_uid = uid;
	return pw->pw_name;
}

void graceful_exit()
{
	vexit();
	exit(1);
	
}
cangotop()
{
	if (top_line_pos && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
cangobot()
{
	if ((item_cnt-1)-top_line_pos > ITEM_LINES  && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
cangoup()
{
	if (top_line_pos > 0  && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
cangodn()
{
	if (top_line_pos + ITEM_LINES < (item_cnt-1) && screen_mode==MODE_NORM) return TRUE;
	return FALSE;
}
doup()
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
			  if (((QITEM*)items[top_line_pos])->owner_uid == my_uid) attron(A_HI);

			(*the_screen->fmtline)(items[top_line_pos],ITEM_ST_LINE,1);

			if (the_screen == &main_screen) 
			  attroff(0);
		
			vroll(0,23);
		}
		else
		{
			show_list();
		}
	}
}
dodn()
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
			  if (((QITEM*)items[top_line_pos+ITEM_LINES])->owner_uid == my_uid) attron(A_HI);
			
			(*the_screen->fmtline)(items[top_line_pos+ITEM_LINES],ITEM_END_LINE,1);
			if (the_screen == &main_screen) 
			  attroff(0);
			
			vroll(0,23);
		}
		else
		{
			show_list();
		}
	}
} 
dopup()
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
dopdn()
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
dotop()
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
dobot()
{
	int needtoshow;

	if (!cangobot()) return;
	
	clearstatusl();
	if (top_line_pos != ((item_cnt-1)-ITEM_LINES)) needtoshow=TRUE;
	else needtoshow=FALSE;
	
	pos_on_screen = ITEM_LINES;

	top_line_pos = (item_cnt-1);
	top_line_pos -= ITEM_LINES;

	if (needtoshow)
	  show_list();
	
}
save_pos()
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
int get_bool(pval,size,row,col,dummy)
int *pval,size;
int row,col;
{
    int c;
    int val,
	where_y,
	where_x;

    where_y=row;
    where_x=col;
    attron(A_HI);

    val = *pval;
    
    while (1)
    {
	if (val)
	    mvaddstr(where_y, where_x, "Yes");
	else
	    mvaddstr(where_y, where_x, "No ");
	vmove(row,col);
	
	switch (c =vgetm())
	{
	      case GENERIC_RETURN+VMBIAS:
	      case GENERIC_ENTER+VMBIAS:
	      case GENERIC_TAB+VMBIAS:
	      case GENERIC_BACKTAB+VMBIAS:
	      case GENERIC_PF14+VMBIAS:
		if (val)
		  mvaddstr(where_y, where_x, "Yes");
		else
		  mvaddstr(where_y, where_x, "No ");
		*pval=val;
		return c;
		break;
	case ' ':
	    val = !val;
	    break;
	case 'y':
	case 'Y':
	    val = 1;
	    break;
	case 'n':
	case 'N':
	    val = 0;
	    break;
	default:
		vbell();
		
	}
    }
}
int get_disp(pval,size,row,col,dummy) /* get disposition */
int *pval,size;
int row,col,dummy;
{
    int c;
    int val,
	where_y,
	where_x;

    where_y=row;
    where_x=col;
    attron(A_HI);

    val = *pval;
    
    while (1)
    {
	if (val&QMD_RESPOOLHOLD)
	    mvaddstr(where_y, where_x, "ReQueue");
	else if (val&QMD_DELETEAFTER)
	    mvaddstr(where_y, where_x, "Delete ");
	else
	    mvaddstr(where_y, where_x, "       ");
	vmove(row,col);
	
	switch (c =vgetm())
	{
	      case GENERIC_RETURN+VMBIAS:
	      case GENERIC_ENTER+VMBIAS:
	      case GENERIC_TAB+VMBIAS:
	      case GENERIC_BACKTAB+VMBIAS:
	      case GENERIC_PF14+VMBIAS:
		if (val&QMD_RESPOOLHOLD)
		  mvaddstr(where_y, where_x, "ReQueue");
		else if (val&QMD_DELETEAFTER)
		  mvaddstr(where_y, where_x, "Delete ");
		else
		  mvaddstr(where_y, where_x, "	     ");
		*pval=val;
		return c;
		break;
	case ' ':
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
	    break;
	case 'r': case 'R':
		val &= ~QMD_DELETEAFTER;
		val |= QMD_RESPOOLHOLD;
		break;
	case 'd':
	case 'D':
		val &= ~QMD_RESPOOLHOLD;
		val |= QMD_DELETEAFTER;
		break;
	default:
		vbell();
		
	}
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
get_string(string, wsize,  row, col, max)
char *string;
int wsize;
int row,col,max;
{
	int ch;
	int pos, wpos;
	int opt;
	int bufsz;
	char *repstr(),*showfield();
	static char gsbuf[255];
	static char copybuf[255];
/*#define DEBUGGS*/
#ifdef DEBUGGS	
	FILE *tmp;
	tmp=fopen("/dev/ttyp0","w");	/* use appropriate tty */
#endif
	opt=optimization;
	optimization=TRACKING_ONLY;
	
	wpos=pos=0;
	bufsz=strlen(string);

	vmode(BOLD|UNDERSCORE);
	vmove(row,col);
	vprint(showfield(string,bufsz,wpos,wsize,gsbuf));

/*	if (strlen(string)<size) vprint(repstr(' ',size-strlen(string)));*/

	while (1)
	{
		vmove(row,col+pos);
		
		switch (ch=getch())
		{
		      case GENERIC_RETURN+VMBIAS:
		      case GENERIC_ENTER+VMBIAS:
		      case GENERIC_TAB+VMBIAS:
		      case GENERIC_BACKTAB+VMBIAS:
		      case GENERIC_PF14+VMBIAS:
			
			optimization=opt;
			string[bufsz]=(char)0;
			while (string[bufsz-1]==' ') --bufsz;
			string[bufsz]=(char)0;
			return ch;
			break;
		      case GENERIC_DELETE+VMBIAS:
			if (pos+wpos)
			{
				memcpy(copybuf,string+pos+wpos,bufsz-(pos+wpos-1));
				memcpy(string+pos+wpos-1,copybuf,bufsz-(pos+wpos-1));
				--bufsz;
				string[bufsz]=(char)0;
			}
			DECPOS;
			break;
			
		      case GENERIC_REMOVE+VMBIAS:
			if (bufsz && bufsz>wpos+pos) 
			{
				memcpy(copybuf,string+pos+wpos+1,bufsz-(pos+wpos-1));
				memcpy(string+pos+wpos,copybuf,bufsz-(pos+wpos+1));
				--bufsz;
				string[bufsz]=(char)0;
			}
			break;
			
		      case GENERIC_INSERT+VMBIAS:
			if (bufsz < max)
			{
				memcpy(copybuf,string+pos+wpos,bufsz-pos);
				string[pos+wpos]=' ';
				memcpy(string+pos+wpos+1,copybuf,bufsz-pos);
				++bufsz;
			}
			break;

		      case GENERIC_LEFT+VMBIAS:
			DECPOS;
			break;
			
		      case GENERIC_RIGHT+VMBIAS:
			INCPOS;
			break;
			
		      default:
			if (ch>=' ' && ch<='~') 
			{
				if (pos+wpos < max)
				{
					string[pos+wpos]=ch;
					if (pos+wpos >= bufsz) 
					  ++bufsz;
					INCPOS;
				}
			}
			break;
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

char *showfield(buf,bufsz,wpos,wsize,showbuf)
char *buf; /* contains data to be displayed */
int bufsz; /* size of this data */
int wpos;  /* starting point in the data to display */
int wsize; /* size of the 'window' to display */
char *showbuf;
{
	static char myshowbuf[255];
	int tocopy;
	
	if (showbuf==NULL)
	  showbuf=myshowbuf;
	
	if (bufsz==0)
	{
		bufsz=strlen(buf);
	}
	if (wpos<0) 
	{
		wpos=0;
		wsize = bufsz;
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
statusl(p)
{
	vmove(19,1);
	vprint(p);
}
int get_int(pval, field_width, row,col, max)
int *pval, field_width;
int row,col,max;
{
	char tmp[200];
	int key;
	
	do
	{
		sprintf(tmp,"%-3d",*pval);
		key=get_string(tmp,field_width,row,col,max);
		if (atoi(tmp)==0) vbell();
		
	} while (atoi(tmp)==0);
	
	*pval=atoi(tmp);
	return key;
}
clearstatusl()
{	
	statusl("								       ");
}
char *approx_size(qi)
QITEM *qi;
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

