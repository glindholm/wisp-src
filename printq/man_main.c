/*
 * Module:  ipman_main
 * Program: IDSIprint ipman
 *
 */
static char copyright[] = "Copyright 1991,1992,1993 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#define EXT extern 

#include "c_comm.h"
#include "manage.h"
#include "qpaths.h"
#include "wlicense.h"

static int quiet_mode = FALSE;

int parseopts (int cnt, char **v);


extern int initpaths (void);
extern int isrunning (void);
extern int waitready (void);
extern int issocket (void);
extern int in_screen (char *data);


extern int catch (void);
extern int init_me (void);

main(int argc, char **argv)
{
	char tmp[100];
	int ch,input_source;
	char *passed_data;
	int init,st,action;
	void license_warning(void);
	
	PITEM pitem;
	QITEM file;
	
	catch();

	license_warning();
 	parseopts(argc,argv);

	if (initpaths() == -1)
	{
		if (!quiet_mode) fprintf(stderr,"%s: can't access spool dir: %s\n",argv[0],
			spooldir);
		exit(10);
	}

	st=isrunning();
	if (st == IERR_NODAEMON)
	{
		if (!quiet_mode) fprintf(stderr,"IDSI scheduler [%s] not running\n",DAEMONNAME);
		exit(1);
	}
	else
	  if (st == IERR_SEMAPHORE)
	  {
		  if (!quiet_mode) fprintf(stderr,"Error accessing master semaphore.\nContact Customer Support\n");
		  exit(2);
	  }
	waitready();
#ifdef SOCKCODE
	if (!issocket())
	{
		if (!quiet_mode) fprintf(stderr,"Incompatible version.  This is socket version of %s.\n",MANNAME);
		if (!quiet_mode) fprintf(stderr,"The currently running %s is a message queue version.\n",DAEMONNAME);
		exit(3);
	}
#endif	
#ifdef MQCODE
	if (!ismqcode())
	{
		if (!quiet_mode) fprintf(stderr,"Incompatible version.  This is message queue version of %s.\n",MANNAME);
		if (!quiet_mode) fprintf(stderr,"The currently running %s is a socket version.\n",DAEMONNAME);
		exit(4);
	}
#endif
	init = init_me();
	switch(init)
	{
	        case IERR_OK:
		  break;
		case IERR_DUMPVERS:
		  if (!quiet_mode)
		  {
			  fprintf(stderr,"The currently running %s Master Process is incompatible\n",DAEMONNAME);
			  fprintf(stderr,"with this version of %s.  Check your $PATH variable to insure\n",MANNAME);
			  fprintf(stderr,"that your are running the correct versions of these programs.\n");
			  exit(7);
		  }
		  break;
		  
		case IERR_DUMPFMT:
		  if (!quiet_mode)
		  {
			  fprintf(stderr,"The currently running %s Master Process is incompatible\n",DAEMONNAME);
			  fprintf(stderr,"with this version of %s.  Check your $PATH variable to insure\n",MANNAME);
			  fprintf(stderr,"that your are running the correct versions of these programs.\n");
			  exit(8);
		  }
		  break;
		  
		case IERR_DUMPMAG:
		  if (!quiet_mode)
		  {
			  fprintf(stderr,"The currently running %s Master Process is incompatible\n",DAEMONNAME);
			  fprintf(stderr,"with this version of %s.  Check your $PATH variable to insure\n",MANNAME);
			  fprintf(stderr,"that your are running the correct versions of these programs.\n");
			  exit(9);
		  }
		  break;
	  }	      

	if ((init=init_comm())!=INIT_OK)
	{
		extern char *sys_errlist[];
		int ret;
		
		switch(init&0xff)
		{
		      case IERR_NODAEMON:
			vexit();
			if (!quiet_mode) fprintf(stderr,"IDSI scheduling daemon not running.\nContact system administrator.\n");
			ret=1;
			break;
		      case IERR_SOCKET:
			vexit();
			if (!quiet_mode) fprintf(stderr,"Error allocating socket: %s.\nContact Customer Support.\n",
				sys_errlist[(init&0xff00)>>8]);
			ret=5;
			break;
		      case IERR_BIND:
			vexit();
			if (!quiet_mode) fprintf(stderr,"Error binding socket: %s.\nContact Customer Support.\n",
				sys_errlist[(init&0xff00)>>8]);
			ret=6;
			break;
		      default:
			break;
		}
		exit(ret);
	}
	the_screen = &main_screen;
	do
	{
		switch(the_scrid)
		{
		      case QSCR:
			passed_data=queue_list;
			break;
		      case PSCR:
			passed_data=printer_list;
			break;
		}
		
		action = in_screen(passed_data);

	}while (action != QUITILPMAN || screen_mode==MODE_EDIT);

	verase(FULL_SCREEN);
	vexit();
	exit(0);
	
}
#define OPTSTR "qw"
parseopts(int cnt, char **v)
{
	int c;
	extern int optind,opterr;
	extern char *optarg;

      top:
	
	while ((c=getopt(cnt,v,OPTSTR))!=EOF)
	{
		switch (c)
		{
		      case 'q':
			quiet_mode = TRUE;
			break;
		      case 'w':
			wang_mode = TRUE;
			break;
		}
	}
	if (optind < cnt) goto top;
}
static char *center(char *mess)
{
	static char	buff[81];
	int len, col;
	
	len = strlen(mess);
	col = 40 - len/2;

	memset(buff,' ',80);
	buff[80]=(char)0;
	memcpy(buff+col,mess,len);
	return buff;
}
void license_warning(void)
{
	int license;
	char *center();
	char buf[80];
	
	license=validate_license();
	if (license!=LICENSE_OK)
	{
		fprintf(stderr,"\n%s\n",center("****  UniQue License Information  ****"));
		fprintf(stderr,"\n");
		fprintf(stderr,"%s\n",center("Copyright (c) 1988,89,90,91,92,93 International Digital Scientific Inc."));
		fprintf(stderr,"%s\n",center("28460 Avenue Stanford Suite 100, Valencia CA 91355  (805) 295-1155"));
		fprintf(stderr,"\n\n\n\n\n");
		switch (license)
		{
		      case LICENSE_MISSING:
			fprintf(stderr,"%s\n",center("****  WARNING  ****"));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("This machine has not been licensed to use the UniQue Print Queue."));
			fprintf(stderr,"%s\n",center("Please run the ulicense program to install the UniQue license."));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("****  WARNING  ****"));
			fprintf(stderr,"\n\n\n\n\n\n\n");
			fprintf(stderr,center("Press ENTER to continue."));
			fflush(stderr);
			fgets(buf,80,stdin);
			break;
		      case LICENSE_TIMEDOUT:
			fprintf(stderr,"%s\n",center("****  TIMED OUT  ****"));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("The UniQue demo license for this machine has timed out."));
			fprintf(stderr,"%s\n",center("Please contact I.D.S.I. at the above number for assistance."));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("****  TIMED OUT  ****"));
			fprintf(stderr,"\n\n\n\n\n\n\n");
			fprintf(stderr,center("Press ENTER to EXIT."));
			break;
		      case LICENSE_INVALID:
		      case LICENSE_UNKNOWN:
		      default:
			fprintf(stderr,"%s\n",center("****  INVALID LICENSE  ****"));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("An invalid UniQue runtime license has been installed on this machine."));
			fprintf(stderr,"%s\n",center("Please run the ulicense program to install the correct UniQue license."));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("****  INVALID LICENSE  ****"));
			fprintf(stderr,"\n\n\n\n\n\n\n");
			fprintf(stderr,center("Press ENTER to EXIT."));
			break;
		}
		if (license!=LICENSE_MISSING)
		  exit(99);
	}
}
ilpmanage(void)
{}
ilpwisp(void)
{}
/*
 * $Log: man_main.c,v $
 * Revision 1.18  1993/09/17  18:53:32  jockc
 * change license message to be uniquish
 *
 * Revision 1.17  1993/09/13  15:28:23  jockc
 * 499
 * 503
 *
 * Revision 1.17  1993/09/10  18:29:01  jockc
 * version checks finished.. wang option added.. some new key stuff in here also
 *
 * Revision 1.16  1993/08/13  20:12:28  jockc
 * added meaningful return codes, alpha/c89 changes, quiet flag,
 * using of ring() instead of old list scheme
 *
 * Revision 1.15  1993/01/12  02:09:33  jockc
 * integrated new semaphore scheme
 *
 * Revision 1.14  1992/12/31  23:50:25  jockc
 * qpaths/ISPOOL
 *
 * Revision 1.13  1992/10/27  23:34:53  jockc
 * added some improved error checking and reporting
 *
 * Revision 1.12  1992/10/09  20:18:55  jockc
 * update for new lock names
 *
 * Revision 1.11  1992/07/07  23:07:55  jockc
 * added license validate
 *
 * Revision 1.10  1991/10/01  17:11:13  jockc
 * change for comm scheme , checks for daemon presence/type
 *
 * Revision 1.9  1991/08/02  23:51:41  jockc
 * added clear screen at exit
 *
 * Revision 1.8  1991/04/30  23:54:58  jockc
 * *** empty log message ***
 *
 * Revision 1.7  1991/04/30  23:37:27  jockc
 * chopped out everything but main and put it in man_misc
 * so that ilpwisp could use it
 *
 * Revision 1.6  1991/04/30  23:30:29  jockc
 * misc stuff:
 * daemon correctly handles form specs in iprintcap that are
 * numeric (yacc grammar expected alpha).  No delay in wisp
 * screen manager at startup. also reentry into ilpwisp routines
 * are now ok
 *
 * Revision 1.5  1991/04/30  17:44:03  jockc
 * misc cleanup
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
