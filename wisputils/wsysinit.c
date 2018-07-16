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

static char *_copyright = "(c)1990 Int'l Digital Scientific, Inc.";
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <stdio.h>
#include <time.h>

#include "wdefines.h"
#define EXT_FILEXT
#include "filext.h"

static	int	verbose=0;

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

	setuid(0);

	if (argc > 1)
	{
		if (argc == 2 && 0==strcmp(argv[1],"-v"))
		{
			verbose = 1;
		}
		else
		{
			printf("Usage:   wsysinit [-v]\n");
			exit(0);
		}
	}
	
	clock=time(0);
	sprintf(buff,"wsysinit: Begin %s",ctime(&clock));
	verbose_print(buff);

	verbose_print("Removing WISP Message Queue.");
	key = wftok(WISP_RETCOD_FILE);
	if (key != -1)
	{
		if (verbose) { printf("Key is %x. ",key); fflush(stdout); }
		if ((id = msgget(key,0))>=0)
		  if (verbose) { printf("Id is %d.\n",id); fflush(stdout); }
		if (msgctl(id,IPC_RMID,0)>=0)
		  verbose_print("Deleted Message Queue.");
	}
	
	if (unlink(WISP_RETCOD_FILE)>=0)
	{
		sprintf(buff,"Deleted Key File %s.\n",WISP_RETCOD_FILE);
		verbose_print(buff);
	}
	verbose_print("Removing WISP Shared Memory Areas.\n");
	
	context=0; 
	while (filename=nextfile(WISP_PRB_DIR,&context))
	{
		if (!strcmp(filename,".") || !strcmp(filename,"..")) continue;
		sprintf(fullpath,"%s/%s",WISP_PRB_DIR,filename);
		key = wftok(fullpath);
		if (key != -1)
		{
			if (verbose) { printf("Key is %x. ",key); fflush(stdout); }
			if (!strncmp(filename,"i",1) || !strncmp(filename,"MSG_",4))
			{
				if ((id = msgget(key,0))>=0)
				  if (verbose) { printf("Id is %d.\n",id); fflush(stdout); }
				if (msgctl(id,IPC_RMID,0)>=0)
				  if (verbose) { printf("Deleted Message Queue.\n"); fflush(stdout); }
			}
			else
			{
				if ((id = shmget(key,0,0))>=0)
				  if (verbose) { printf("Id is %d.\n",id); fflush(stdout); }
				if (shmctl(id,IPC_RMID,0)>=0)
				  if (verbose) { printf("Deleted Shared Memory.\n"); fflush(stdout); }
			}
		}
		if (unlink(fullpath)<0)
		{
			sprintf(buff,"unlink %s",fullpath);
			perror(buff);
		}
		else
		{
			sprintf(buff,"Deleted Key File %s.\n",filename);
			verbose_print(buff);
		}
	}
	nextfile(NULL,&context);

	verbose_print("Deleting WISP temporary files.");

	sprintf(command,"rm -f /usr/tmp/%s* >/dev/null 2>&1",WISP_TEMP_PERSON_PREFIX);
	verbose_print(command);
	system(command);

	sprintf(command,"rm -f /usr/tmp/%s* >/dev/null 2>&1",WISP_TEMP_PERSUB_PREFIX);
	verbose_print(command);
	system(command);

	sprintf(command,"rm -f %s/* >/dev/null 2>&1",WISP_LINK_DIR);
	verbose_print(command);
	system(command);

	clock=time(0);
	sprintf(buff,"\nwsysinit: Finished %s",ctime(&clock));
	verbose_print(buff);
}

verbose_print(message)
char	*message;
{
	if (verbose)
		printf("%s\n",message);
}

#endif
