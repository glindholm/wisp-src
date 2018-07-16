/*
 * Module:  ilp_wisp
 * Program: IDSIprint
 * Purpose: interface routines for wisp
 *
 * $Log: ilp_wisp.c,v $
 * Revision 1.18  1992/04/30  19:25:15  jockc
 * added decl for sbuf
 *
 * Revision 1.17  1992/04/30  19:11:54  jockc
 * stat to get size of file before sending packet
 *
 * Revision 1.16  1991/10/11  19:23:29  jockc
 * detect bad comm status = retcode 99
 *
 * Revision 1.15  1991/10/11  00:32:17  jockc
 * add size to send_daemon call
 *
 * Revision 1.14  1991/10/04  20:49:01  jockc
 * fixup error handling
 *
 * Revision 1.13  1991/10/04  00:40:51  jockc
 * took out extra load_xdumps
 *
 * Revision 1.12  1991/10/01  17:12:15  jockc
 * new comm stuff
 *
 * Revision 1.11  1991/08/05  23:03:59  jockc
 * added prt num parm to ilpwisp
 *
 * Revision 1.10  1991/08/02  23:52:47  jockc
 * fixed noopt from help
 * return 99 if daemon not running or
 * give err message
 *
 * Revision 1.9  1991/07/19  18:18:32  jockc
 * return if file not exists
 *
 * Revision 1.8  1991/07/19  18:07:57  jockc
 * fixed bogus ret code on success
 *
 * Revision 1.7  1991/07/18  21:13:44  jockc
 * close conn with daemon at end of func
 * added MCONNECT and DISCONNECT to spool file func
 *
 * Revision 1.6  1991/05/22  17:06:36  jockc
 * stub in case to handle bad hostaddr
 *
 * Revision 1.5  1991/05/02  19:48:25  jockc
 * turn of monitor while still CONNECTed to daemon, in
 * ilpwispmanager function
 *
 * Revision 1.4  1991/04/30  23:30:26  jockc
 * misc stuff:
 * daemon correctly handles form specs in iprintcap that are
 * numeric (yacc grammar expected alpha).  No delay in wisp
 * screen manager at startup. also reentry into ilpwisp routines
 * are now ok
 *
 * Revision 1.3  1991/04/30  17:44:01  jockc
 * misc cleanup
 *
 * Revision 1.2  1991/04/26  18:54:08  jockc
 * changed ilp() to ilpwisp()
 *
 * Revision 1.1  1991/04/23  22:06:43  jockc
 * Initial revision
 *
 *
 */
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

#define EXT extern 
#include "defs.h"
#include "manage.h"
#include "c_comm.h"

ilpmanage()
{
	int init,pid;
	PITEM pitem;
	QITEM qitem;
	extern SCREEN *the_screen;
	extern SCREEN main_screen, prtr_screen;
	extern int reentered;
	int saveopt;
	
	char *passed_data;
	saveopt=voptimize(DEFER_MODE);
      top:
	
	if ((init=init_comm())!=INIT_OK)
	{
		switch (init & 0xff)
		{
		      case IERR_NODAEMON:
			vre_window("IDSI Print Queue Daemon (idaemon) not running.                  Contact system administrator");
			goto leaveilpman;
			
			break;
		      case IERR_INCOMPATDS:
			vre_window("idaemon incompatible: client=socket, server=message queue.      Contact system administrator");
			goto leaveilpman;
			
			break;
		      case IERR_INCOMPATDM:
			vre_window("idaemon incompatible: client=message queue, server=socket.      Contact system administrator");
			goto leaveilpman;
			
			break;
		      case IERR_SERVUNDEF:
			goto leaveilpman;
			
			break;
		      case IERR_SOCKET:
			goto leaveilpman;

			break;
		      case IERR_CONNECT:
			goto leaveilpman;

			break;
		      case IERR_HOSTADDR:
			goto leaveilpman;

			break;
		      default:
			break;
		}
	}
	init_me();


	the_screen = &main_screen;
	reentered = TRUE;
	global_key = -1;
	do
	{
		switch(the_scrid)
		{
		      case QSCR:
			passed_data=(char*)q_head;
			break;
		      case PSCR:
			passed_data=(char*)p_head;
			break;
		}
		
		in_screen(passed_data);

	}while (global_key != fn1_key || screen_mode==MODE_EDIT);

	vdefer(RESTORE);
	shut_comm();
	
      leaveilpman:
	voptimize(saveopt);
}
ilpwisp(srcfile, pmode, disposition, copies, class, form, prtnum, retcode)
char srcfile[], form[];
char pmode, disposition, class;
int copies, *retcode, prtnum;
{
	QITEM the_item;
	char path[200];
	char formpath[200],buf[200];
	FILE *in;
	int plen=66;
	char *p,*lines;
	int init,pid;
	struct stat sbuf;

      top:
	
	if ((init=init_comm())!=INIT_OK)
	switch(init & 0xff)
	{
	      case IERR_NODAEMON:
	      case IERR_INCOMPATDS:
	      case IERR_INCOMPATDM:
		*retcode = 99;
		return;
		break;
	      case IERR_SERVUNDEF:
		*retcode = 40;
		return;
		break;
	      case IERR_SOCKET:
		*retcode = 41 | ((init & 0xff00) >> 8);
		return;
		break;
	      case IERR_CONNECT:
		*retcode = 42 | ((init & 0xff00) >> 8);
		return;
		break;
	      default:
		break;
	}
	memset(&the_item,0,sizeof(QITEM));
	
	strcpy(the_item.orig_path,srcfile);
	if (srcfile[0] != '/')
	{
		char tmpbuf[200];
		
		getcwd(path,198);
		sprintf(tmpbuf,"%s/%s",path,srcfile);
		fixuppath(tmpbuf,the_item.real_path);
	}
	else
	  strcpy(the_item.real_path,srcfile);

	if (access(the_item.real_path,0))
	{
		*retcode=20;
		goto leave;
	}
	if (access(the_item.real_path,R_OK))
	{
		*retcode=28;
		goto leave;
	}
	strcpy(the_item.requested_prtr,"");
	strcpy(the_item.form,form);

	stat(srcfile,&sbuf);
	the_item.size=sbuf.st_size;

	the_item.stpage=1;
	the_item.endpage=99999;
	the_item.copies=copies;
	the_item.class=class;
	the_item.prtnum=prtnum;
	switch(disposition)
	{
	      case 'D':
	      case 'd':
		the_item.mode = QMD_DELETEAFTER;
		break;
	      case 'R':
	      case 'r':
		the_item.mode = QMD_RESPOOLHOLD;
		break;
	      case 'S':
	      case 's':
	      case ' ':
	      case 0:
		the_item.mode = 0;
		break;
	}		
	switch(pmode)
	{
	      case 'S':
	      case 's':
	      case ' ':
	      case 0:
		break;
	      case 'H':
	      case 'h':
		the_item.mode |= QMD_HOLD;
		break;
	}
	send_daemon(M_QUEUE,0,&the_item,sizeof(the_item));
	if (comm_status!=COMM_OK)
	  *retcode=99;
	else
	  *retcode=0;

      leave:
	shut_comm();
	
}
