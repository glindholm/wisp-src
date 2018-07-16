/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/** 
 **	Program:	wsysinit
 **
 **	CVS:		"$Source:$"
 ** 
 **	Purpose:	clean out tmp files and shared memory used by wisp
 **/

#ifdef unix
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <time.h>

#include "idsistd.h"
#include "wdefines.h"
#include "filext.h"
#include "wperson.h"
#include "wispvers.h"
#include "wisplib.h"
#include "platsubs.h"

extern char *WL_nextfile(const char* path, char** context);
extern	key_t  WL_wftok(const char *file);

static void verbose_printf();

static	int	verbose=0;
static	int	cluster=0;

int main(argc,argv)
int	argc;
char	*argv[];
{
	char *  filename;
	char *  context;
	key_t 	key;
	int 	id;
	time_t	clock;
	char    command[200];
	char    fullpath[200];
	int	ok2delete;

	setuid(0);
	if (0 != geteuid())
	{
		fprintf(stderr,"SORRY - you must be root to continue.\n");
		exit(0);
	}

	if (argc > 1)
	{
		int	c;
		extern int optind;

		while((c = getopt(argc,argv,"VvCcHh?")) != -1)
		{
			switch(c)
			{
			case 'v':
			case 'V':
				verbose = 1;
				break;
			case 'c':
			case 'C':
				cluster = 1;
				break;
			default:
				fprintf(stderr,"Usage:   wsysinit [-vch]\n");
				fprintf(stderr," -v   Verbose mode, print all messages.\n");
				fprintf(stderr," -c   Cluster mode, delete key file only if SM/MQ found.\n");
				fprintf(stderr," -h   Help, print this screen.\n");
				exit(0);
			}
		}
		if (optind < argc)
		{
			fprintf(stderr,"wsysinit: Invalid argument %s\n",argv[optind]);
			exit(0);
		}
	}

	clock=time(0);
	verbose_printf("wsysinit: Version %s [%s]\n", wisp_version(), WL_platform_name());
	verbose_printf("Begin %s\n",ctime(&clock));

	/*
	**	Removing wisptmp files:
	**		/usr/tmp/wisptmp/RC_*
	**		/usr/tmp/wisptmp/WPROC*
	*/

	verbose_printf("Removing known temporary files.\n");
	if (0!=access(wisptmpdir(NULL),00))
	{
		verbose_printf("The temp dir [%s] was not found.\n",wisptmpdir(NULL));
	}
	else	/* RETCOD file was found */
	{
		context=0; 
		while ((filename=WL_nextfile(wisptmpdir(NULL),&context)))
		{
			if (!strcmp(filename,".") || !strcmp(filename,"..")) continue;
			if (0==memcmp(filename,"RC_",3) ||
			    0==memcmp(filename,"WPROC",5)  ||
			    0==memcmp(filename,"TMP_",4)  ||
			    0==memcmp(filename,"DEFS_",5)  ||
			    0==memcmp(filename,"SM_",3)  )
			{
				sprintf(fullpath,"%s/%s",wisptmpdir(NULL),filename);
				verbose_printf("Deleting file %s.\n",fullpath);
				unlink(fullpath);
			}
		}
		WL_nextfile(NULL,&context);
	}

	verbose_printf("\nRemoving WISP Shared Memory & Message Queue Areas.\n");
	
	context=0; 
	while ((filename=WL_nextfile(wispprbdir(NULL),&context)))
	{
		if (!strcmp(filename,".") || !strcmp(filename,"..")) continue;

		ok2delete = 0;
		sprintf(fullpath,"%s/%s",wispprbdir(NULL),filename);
		verbose_printf("\nKey file is %s.\n",fullpath);

		key = WL_wftok(fullpath);
		if (key == -1)
		{
			verbose_printf("*** Unable to get key for %s\n",fullpath);
		}
		else	/* Got the key */
		{
			verbose_printf("Key is %x (%d).\n",key,key);
			if (!strncmp(filename,"i",1) || !strncmp(filename,"MSG_",4))
			{
				if ((id = msgget(key,0))<0)
				{
					verbose_printf("*** Message queue not found for key %x.\n",key);
				}
				else	/* Found the message queue */
				{
					verbose_printf("Id is %d.\n",id);
					if (msgctl(id,IPC_RMID,0)<0)
					{
						verbose_printf("*** Unable to delete message queue Id = %d.\n",id);
					}
					else 	/* Deleted message queue */
					{
						verbose_printf("Deleted Message Queue.\n");
						ok2delete = 1;
					}
				}
			}
			else
			{
				if ((id = shmget(key,0,0))<0)
				{
					verbose_printf("*** Shared memory not found for key %x.\n",key);
				}
				else	/* Found the shared memory */
				{
					verbose_printf("Id is %d.\n",id);
					if (shmctl(id,IPC_RMID,0)<0)
					{
						verbose_printf("*** Unable to delete shared memory Id = %d.\n",id);
					}
					else	/* Deleted shared memory */
					{
					  	verbose_printf("Deleted Shared Memory.\n");
						ok2delete = 1;
					}
				}
			}
		}

		if (ok2delete || !cluster)
		{	
			if (unlink(fullpath)<0)
			{
				verbose_printf("*** Unable to delete file %s.\n",fullpath);
			}
			else	/* Deleted key file */
			{
				verbose_printf("Deleted Key File %s.\n",fullpath);
			}
		}
	}
	WL_nextfile(NULL,&context);

	verbose_printf("\nDeleting WISP temporary files.\n");

	sprintf(command,"rm -f %s/* >/dev/null 2>&1",wisptmpdir(NULL));
	verbose_printf("%s\n",command);
	system(command);

	sprintf(command,"rm -f %s/* >/dev/null 2>&1",wisplinkdir(NULL));
	verbose_printf("%s\n",command);
	system(command);

	clock=time(0);
	verbose_printf("\nFinished %s\n",ctime(&clock));
	return 0;
}

static void verbose_printf(format,a1,a2,a3,a4,a5,a6,a7,a8)
char	*format;
char	*a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8;
{
	if (verbose)
	{
		printf(format,a1,a2,a3,a4,a5,a6,a7,a8);
		fflush(stdout); 
	}
}

#endif /* unix */
/*
**	History:
**	$Log: wsysinit.c,v $
**	Revision 1.25  2003/02/07 20:45:14  gsl
**	Add platform to version display
**	
**	Revision 1.24  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.23  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.22  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.21  2003/01/15 16:24:34  gsl
**	add version header
**	
**	Revision 1.20  2003/01/15 16:13:05  gsl
**	add version number
**	
**	Revision 1.19  2003/01/14 19:15:57  gsl
**	Unix, change to use the same File maped shared memory as on WIN32.
**	Optionally switch with USESHAREMEMORY option.
**	
**	Revision 1.18  2002/07/12 19:10:24  gsl
**	Global unique WL_ changes
**	
**	Revision 1.17  2002/07/11 16:11:45  gsl
**	Fix warnings
**	
**	Revision 1.16  2002/07/11 16:04:52  gsl
**	Fix warnings
**	
**	Revision 1.15  2002/07/10 04:27:31  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.14  2002/07/08 21:08:04  gsl
**	add missing include
**	
**	Revision 1.13  2002/06/25 18:18:38  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.12  2002/04/12 14:54:08  gsl
**	fix nexted comment warning
**	
**	Revision 1.11  2001-10-31 15:28:48-05  gsl
**	Removed the deleting temp person files
**	Added rm -f wisptmpdir()
**	/usr/tmp/wisptmp/ *
**
**	Revision 1.10  1996-08-23 17:25:54-04  gsl
**	Change to use wispxxxdir() routines
**
**	Revision 1.9  1996-07-23 11:13:13-07  gsl
**	drcs update
**
**
**
*/
