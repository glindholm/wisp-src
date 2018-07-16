static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/** 
 ** program: wsysinit
 **
 ** ident: "@(#)wsysinit.c        1.0      ULTRIX/SYS5/AIX  3/23/90"
 ** 
 ** purpose: clean out tmp files and shared memory used by wisp
 **
 ** Copyright 1990  International Digital Scientific, Inc.
 **
 **/

#ifdef unix

static char *idsi_copyright = "(c)1990,1993 International Digital Scientific, Inc.";

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <stdio.h>
#include <time.h>

#include "idsistd.h"
#include "wdefines.h"
#define EXT_FILEXT
#include "filext.h"

static	int	verbose=0;
static	int	cluster=0;

main(argc,argv)
int	argc;
char	*argv[];
{
	char *  filename, *nextfile();
	char ** context;
	key_t 	key, wftok();
	int 	id;
	char	buff[80];
	time_t	clock;
	char    command[200];
	char    fullpath[200];
	int	rc;
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
	verbose_printf("wsysinit: Begin %s\n",ctime(&clock));

	/*
	**	Removing wisptmp files:
	**		/usr/tmp/wisptmp/RC_*
	**		/usr/tmp/wisptmp/WPROC*
	*/

	verbose_printf("Removing Return Code temporary files.\n");
	if (0!=access(wisptmpdir(NULL),00))
	{
		verbose_printf("%s file was not found.\n",wisptmpdir(NULL));
	}
	else	/* RETCOD file was found */
	{
		context=0; 
		while (filename=nextfile(wisptmpdir(NULL),&context))
		{
			if (!strcmp(filename,".") || !strcmp(filename,"..")) continue;
			if (0==memcmp(filename,"RC_",3) ||
			    0==memcmp(filename,"WPROC",5)  )
			{
				sprintf(fullpath,"%s/%s",wisptmpdir(NULL),filename);
				verbose_printf("Deleting file %s.\n",fullpath);
				unlink(fullpath);
			}
		}
		nextfile(NULL,&context);
	}

	verbose_printf("\nRemoving WISP Shared Memory & Message Queue Areas.\n");
	
	context=0; 
	while (filename=nextfile(wispprbdir(NULL),&context))
	{
		if (!strcmp(filename,".") || !strcmp(filename,"..")) continue;

		ok2delete = 0;
		sprintf(fullpath,"%s/%s",wispprbdir(NULL),filename);
		verbose_printf("\nKey file is %s.\n",fullpath);

		key = wftok(fullpath);
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
	nextfile(NULL,&context);

	verbose_printf("\nDeleting WISP temporary files.\n");

	sprintf(command,"rm -f /usr/tmp/%s* >/dev/null 2>&1",WISP_TEMP_PERSON_PREFIX);
	verbose_printf("%s\n",command);
	system(command);

	sprintf(command,"rm -f /usr/tmp/%s* >/dev/null 2>&1",WISP_TEMP_PERSUB_PREFIX);
	verbose_printf("%s\n",command);
	system(command);

	sprintf(command,"rm -f %s/* >/dev/null 2>&1",wisplinkdir(NULL));
	verbose_printf("%s\n",command);
	system(command);

	clock=time(0);
	verbose_printf("\nwsysinit: Finished %s\n",ctime(&clock));
}

verbose_printf(format,a1,a2,a3,a4,a5,a6,a7,a8)
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
**	Revision 1.10  1996-08-23 17:25:54-04  gsl
**	Change to use wispxxxdir() routines
**
**	Revision 1.9  1996-07-23 11:13:13-07  gsl
**	drcs update
**
**
**
*/
