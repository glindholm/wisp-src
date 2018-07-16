/*
 * purgeipc: purge messages from retcode queue,
 *           delete tmp putparm files, and shared mem areas
 *
 */

#ifdef unix

#include <sys/types.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <errno.h>

#define LOG if (dolog)

extern char *sys_errlist[];

main(c,v)
char **v;
{
	FILE *logfile,*tmpfile;
	char logname[30];
	char tmpname[30];
	int pgid;
	int dolog=0,msgid,shmid;
	key_t key;
	char cmd[100];
	char buf[100];
	struct {
		long mtype;
		char mtext[3];
	} msg;
	char mtxt[4];
	int retval;

	if (c>1) ++dolog;							/* any arg will force logging			*/

	pgid=getpgrp();								/* get process group id				*/

	LOG {									/* start logging				*/
		sprintf(logname,"/tmp/purge%d",pgid);
		logfile=fopen(logname,"w");
	}
	
	key = ftok("/usr/tmp/RETCOD",(char)0xd5);				/* get message queue key			*/
	LOG	{ fprintf(logfile,">>purging messages: key is %08x\n",key); 
		}
	if ((msgid = msgget(key,0))<0)						/* get message queue id				*/
	{
		LOG	{ fprintf(logfile,"ERR>can't get id: %s\n",
			sys_errlist[errno]);
			}
	}
	else
	{
		int i=0,status;

		memset(&msg,0,sizeof(msg));
		memset(mtxt,0,sizeof(mtxt));
		strncpy(mtxt,"000",3);
		LOG	{ fprintf(logfile,"   >reading queue for messages:");
			}
		do 
		{
			if (msgrcv(msgid,&msg,3,pgid,IPC_NOWAIT)<0)			/* start reading messages		*/
				status=errno;
			else 	status=0;

			LOG	{ fprintf(logfile,"%d ",++i);
				}

		} while (status != ENOMSG);						/* till no more				*/

		LOG	{ fprintf(logfile,"\n   >messages purged\n");
			}
	}
	sprintf(tmpname,"/tmp/Tmp%d",pgid);						/* build a tmp file name		*/
	LOG	{ fprintf(logfile,">>purging putparms. tmp file is %s\n",tmpname);
		}
	sprintf(cmd,"/bin/ls /usr/tmp/????????%04X >%s 2>/dev/null",pgid,tmpname);	/* build a command			*/
	LOG	{ fprintf(logfile,"   >cmd:[%s]\n",cmd);
		}
	system(cmd);									/* execute it				*/
	sprintf(cmd,"/bin/ls /usr/tmp/????????%04X?? >>%s 2>/dev/null",pgid,tmpname);	/* build a command			*/
	LOG	{ fprintf(logfile,"   >cmd:[%s]\n",cmd);
		}
	system(cmd);									/* execute it				*/
	tmpfile = fopen(tmpname,"r");
	while (fgets(buf,80,tmpfile))							/* read in the tmp file names		*/
	{
		buf[strlen(buf)-1]=(char)0;						/* null the newline			*/
		key=ftok(buf,0);							/* get the key				*/
		shmid=shmget(key,0,0);							/* and the id				*/
		LOG	{ fprintf(logfile,"   >utmpfil:%s, key=%08x, shmid=%d\n",buf,key,shmid);
			}
		if (shmctl(shmid,IPC_RMID,0)<0)						/* sys call to remove shm area		*/
		{
			LOG	{ fprintf(logfile,"ERR>can't remove shared mem segment: %s\n",
					sys_errlist[errno]);
				}
		}
		else
		{
			LOG	{ fprintf(logfile,"   >removed segment ***\n");
				}
		}
		unlink(buf);
		unlink(tmpname);							/* unlink all tmp files			*/
	}
	LOG	{ fprintf(logfile,">>purge completed\n");
		  fclose(logfile);
		}
}
#endif
