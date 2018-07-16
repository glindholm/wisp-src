#include "idsistd.h"

#ifdef VMS
#include <descrip.h>
#include <sjcdef.h>
#include <ssdef.h>
#include <jbcmsgdef.h>
#include "que_jobs.h"
#include <v/video.h>
#include "werrlog.h"

/* These are defined in SUBMIT, and must be declared the same.									*/

#define MAX_SUBMIT_PARMS	8

extern short subp_len[MAX_SUBMIT_PARMS];						/* The length of each parm.		*/
extern char  subp_text[MAX_SUBMIT_PARMS][256];						/* The parm data.			*/
extern int   subp_num;									/* The number of parms.			*/


que_job(qtype,qname,fname,jname,cpulim,qform,qflags)					/* Put a job in a queue.		*/
int  qtype;										/* The type of queue.			*/
char *qname;										/* The name of the queue to go in.	*/
char *fname;										/* The file to queue.			*/
char *jname;										/* The jobname.				*/
int  cpulim;										/* Cpu limit.				*/
int  qform;										/* The form number for print queue	*/
int  qflags;										/* Flags to control the job.		*/
{
#define		ROUTINE		49000

	item_list	*prt_items,*curr_item;						/* Use this to construct the item list	*/
	int		num_items;							/* The number of items in the list	*/
	unsigned long 	status, stat_sjc;
	long 		formnum;
	long 		outbuf;
	short		mlen;
	char msgstr[256];
#include "que_jobs.d"

	struct	{									/* The io status block			*/
			long	sts;
			long	xtra;
		} the_iosb;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
											/* Batch queue has to handle the log	*/
	num_items = 8;									/* Always have QUEUE, FILENAME, and end	*/
											/* and form number			*/
	if (qflags & Q_DELETE_FILE) num_items++;					/* We'll need to add a delete item	*/
	if (qflags & Q_HOLD_JOB) num_items++;						/* Add a hold item			*/
	if (*jname) num_items++;							/* Need to give a jobname.		*/
	if (qtype == BATCH_QUEUE) num_items += subp_num;				/* Handle parameter passing.		*/

	prt_items = (item_list *) malloc(sizeof(item_list) * num_items);		/* Get the memory for the items		*/
	curr_item = prt_items;

	curr_item->item.buflen = strlen(qname);						/* set size of qname buffer		*/
	curr_item->item.itemcode = SJC$_QUEUE;						/* this is the queue...			*/
	curr_item->item.bufadr = qname;							/* here is it's name			*/
	curr_item->item.retadr = (int *) 0;						/* no return value			*/

	curr_item++;									/* next item				*/

	curr_item->item.buflen = strlen(fname);						/* Size of the file name		*/
	curr_item->item.itemcode = SJC$_FILE_SPECIFICATION;				/* This is the file name.		*/
	curr_item->item.bufadr = fname;							/* The buffer				*/
	curr_item->item.retadr = (int *) 0;

	if (qtype == PRINT_QUEUE)
	{
		curr_item++;								/* next item				*/

		curr_item->item.buflen = 4;						/* Size of the file copies.		*/
		curr_item->item.itemcode = SJC$_FILE_COPIES;				/* This a file copies request.		*/
		curr_item->item.bufadr = (char *)&cpulim;				/* The buffer (stored as cpulim)	*/
		curr_item->item.retadr = (int *) 0;

		curr_item++;								/* next item				*/

		formnum = qform;
		curr_item->item.buflen = 4;						/* Size of the form number		*/
		curr_item->item.itemcode = SJC$_FORM_NUMBER;				/* This is the form number		*/
		curr_item->item.bufadr = (char *)&formnum;				/* The buffer				*/
		curr_item->item.retadr = (int *) 0;
	}


	if (qtype == BATCH_QUEUE)							/* Handle the log file			*/
	{
		curr_item++;
		curr_item->item.buflen = 0;

		if (qflags & Q_PRINT_LOG) curr_item->item.itemcode = SJC$_LOG_SPOOL;	/* Print the log file			*/
		else 			  curr_item->item.itemcode = SJC$_NO_LOG_SPOOL;	/* Or don't print it			*/

		curr_item->item.bufadr = (char *) 0;
		curr_item->item.retadr = (int *) 0;

		if (cpulim)								/* Also has a cpu limit			*/
		{
			curr_item++;
			curr_item->item.buflen = 4;
			curr_item->item.itemcode = SJC$_CPU_LIMIT;
			curr_item->item.bufadr = (char *)&cpulim;
			curr_item->item.retadr = (int *) 0;
		}

		while (subp_num)							/* If (while) there are parameters.	*/
		{
			subp_num--;							/* Count them downwards.		*/
			curr_item++;							/* Pointer to next item in itemlist.	*/
			curr_item->item.buflen = subp_len[subp_num];			/* Pointer to this parms length.	*/
			curr_item->item.itemcode = SJC$_PARAMETER_1 + subp_num;		/* Generate the item code.		*/
			curr_item->item.bufadr = subp_text[subp_num];			/* pointer to the parm itself.		*/
			curr_item->item.retadr = (int *) 0;
		}
	}

	if (qflags & Q_HOLD_JOB)							/* wants to hold the job		*/
	{
		curr_item++;
		curr_item->item.buflen = 0;
		curr_item->item.itemcode = SJC$_HOLD;					/* ask to hold				*/
		curr_item->item.bufadr = (char *) 0;
		curr_item->item.retadr = (int *) 0;                        
	}

	if (qflags & Q_DELETE_FILE)							/* wants to delete the file after job	*/
	{
		curr_item++;
		curr_item->item.buflen = 0;
		curr_item->item.itemcode = SJC$_DELETE_FILE;				/* ask to delete			*/
		curr_item->item.bufadr = (char *) 0;
		curr_item->item.retadr = (int *) 0;
	}

	if (*jname)									/* wants to Give it a job name.		*/
	{
		curr_item++;
		curr_item->item.buflen = strlen(jname);
		curr_item->item.itemcode = SJC$_JOB_NAME;
		curr_item->item.bufadr = jname;
		curr_item->item.retadr = (int *) 0;
	}

	curr_item++;
	curr_item->endlist = 0;								/* end of the list			*/

	status = SYS$SNDJBCW(0,SJC$_ENTER_FILE,0,prt_items,&the_iosb,0,0);		/* do it!				*/
	stat_sjc = the_iosb.sts;							/* get the message from the status word	*/
	if ((status != SS$_NORMAL) || (stat_sjc != JBC$_NORMAL))			/* some kind of error			*/
	{
		if (status != SS$_NORMAL)						/* If was a failure on the system call	*/
		{									/*  then get the error message.		*/
			status = sys$getmsg(status,&mlen,&msg_desc,15,&outbuf);
		}
		else									/* else was a failure on the queue job	*/
		{									/*  system service so get error message.*/
			status = sys$getmsg(stat_sjc,&mlen,&msg_desc,15,&outbuf);
		}
		msgstr[mlen] = '\0';							/* Null terminate.			*/
		werrlog(ERRORCODE(4),qname,fname,0,0,0,0,0,0);
		if (qtype == PRINT_QUEUE)
		{
			werrlog(ERRORCODE(6),cpulim,formnum,jname,0,0,0,0,0);
		}
		else if (qtype == BATCH_QUEUE)
		{
			werrlog(ERRORCODE(8),cpulim,jname,0,0,0,0,0,0);
		}
		else if (*jname) werrlog(ERRORCODE(2),jname,0,0,0,0,0,0,0);

		werrlog(ERRORCODE(2),msgstr,0,0,0,0,0,0,0);				/* Display system error message.	*/
		if (status != SS$_NORMAL) lib$signal(status);
	}
	free(prt_items);								/* Free the memory.			*/

	if ((status != SS$_NORMAL) || (stat_sjc != JBC$_NORMAL)) return(stat_sjc);	/* some kind of error			*/
	else return(SS$_NORMAL);
}
#endif

#ifdef unix
#include "werrlog.h"

que_job()
{
#define		ROUTINE		49000

	werrlog(ERRORCODE(3),0,0,0,0,0,0,0,0);						/* Not implemented.			*/
}
#endif
