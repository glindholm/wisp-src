/*
 * Module:  ipman_main
 * Program: IDSIprint ipman
 *
 * $Log: man_main.c,v $
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
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#define EXT extern 

#include "c_comm.h"
#include "manage.h"

#include "wlicense.h"

main(argc,argv)
int argc;
char *argv[];
{
	char tmp[100];
	int ch,input_source;
	char *passed_data;
	int init;
	void license_warning();
	
	PITEM pitem;
	QITEM file;
	
	catch();

	license_warning();
	
	if (!lockfile(LK_STAT,DAEMON_RUNNING))
	{
		fprintf(stderr,"IDSI scheduler [idaemon] not running\n");
		exit(1);
	}
#ifdef SOCKCODE
	if (!lockfile(LK_STAT,SOCKVERSION))
	{
		fprintf(stderr,"Incompatible version.  This is socket version of ilpman\n");
		fprintf(stderr,"The currently running idaemon is a message queue version\n");
		exit(1);
	}
#endif	
#ifdef MQCODE
	if (!lockfile(LK_STAT,MQVERSION))
	{
		fprintf(stderr,"Incompatible version.  This is message queue version of ilpman\n");
		fprintf(stderr,"The currently running idaemon is a socket version\n");
		exit(1);
	}
#endif

	if ((init=init_comm())!=INIT_OK)
	{
		switch(init&0xff)
		{
		      case IERR_NODAEMON:
			fprintf(stderr,"IDSI scheduling daemon not running.\nContact system administrator.\n");
			break;
		      case IERR_SOCKET:
			fprintf(stderr,"Errno %d allocating socket.\nContact IDSI.\n",(init&0xff00)>>8);
			break;
		      case IERR_CONNECT:
			fprintf(stderr,"Errno %d requesting connection with daemon.\nContact IDSI.\n",(init&0xff00)>>8);
			break;
		      default:
			break;
		}
		exit(0);
	}
	
	init_me();

	the_screen = &main_screen;
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

	verase(FULL_SCREEN);
	vexit();
	
}
static char *center(mess)
char	*mess;
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
void license_warning()
{
	int license;
	char *center();
	char buf[80];
	
	license=validate_license();
	if (license!=LICENSE_OK)
	{
		fprintf(stderr,"\n%s\n",center("****  WISP License Information  ****"));
		fprintf(stderr,"\n");
		fprintf(stderr,"%s\n",center("Copyright (c) 1988,89,90,91,92 International Digital Scientific Inc."));
		fprintf(stderr,"%s\n",center("28460 Avenue Stanford Suite 100, Valencia CA 91355  (805) 295-1155"));
		fprintf(stderr,"\n\n\n\n\n");
		switch (license)
		{
		      case LICENSE_MISSING:
			fprintf(stderr,"%s\n",center("****  WARNING  ****"));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("This machine has not been licensed to use the WISP runtime system."));
			fprintf(stderr,"%s\n",center("Please run the wlicense program to install the WISP license."));
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
			fprintf(stderr,"%s\n",center("The WISP demo license for this machine has timed out."));
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
			fprintf(stderr,"%s\n",center("An invalid WISP runtime license has been installed on this machine."));
			fprintf(stderr,"%s\n",center("Please run the wlicense program to install the correct WISP license."));
			fprintf(stderr,"\n");
			fprintf(stderr,"%s\n",center("****  INVALID LICENSE  ****"));
			fprintf(stderr,"\n\n\n\n\n\n\n");
			fprintf(stderr,center("Press ENTER to EXIT."));
			break;
		}
		if (license!=LICENSE_MISSING)
		  exit(0);
	}
}
ilpmanage(){}
ilpwisp(){}
