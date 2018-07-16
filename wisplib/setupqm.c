			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#ifdef VMS				/* This entire source file is in this #ifdef and is only valid on the VMS system	*/

#include <ssdef.h>
#include <quidef.h>
#include <jbcmsgdef.h>

#include "wdefines.h"
#include "wperson.h"
#include "werrlog.h"
#include "quemgmt.h"

static char *quenameptr;
static long numq, orig_numq;
static struct que {									/* Array of queue names and class.	*/
		char class;
		char name[32];
	};

int strcmp();

setup_qm()										/* Sets up all parameters passed to the	*/
{											/* queue management subroutine.		*/
#define		ROUTINE		59500
	long queflags;									/* Hold disabeled queue function flags.	*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	get_defs(DEFAULTS_FLAGS,&queflags);						/* Set apart the need queue flags.	*/
	queflags &= (HELP_SBATCH_ENABLED | HELP_SGENERIC_ENABLED | HELP_SOUTPUT_ENABLED | HELP_RESTRJOBS_DISABLED);

	sort_que_names();								/* Get and sort possible queues.	*/
	quemngmnt(queflags,quenameptr,numq);						/* Call the queue management program.	*/
	return;
}

sort_que_names()									/* If called from WISP then use LPMAP	*/
{											/* and PQMAP data files else no		*/
	int i, j, contfl;								/* restrictions on queues and pass null	*/
	char *tcpt, quename[100][32];
	struct que qd[100];
	prt_id *prt_ptr;								/* Local ptrs to WISP$CONFIG data.	*/
	pq_id *pq_ptr;

	prt_ptr = get_prt_list();							/* Point to LPMAP data.			*/
	pq_ptr = get_pq_list();								/* Point to PQMAP data.			*/
	quenameptr = 0;
	numq = orig_numq = 0;								/* Init to no queues to search.		*/
	while (pq_ptr)									/* While have data from PQMAP.		*/
	{
		if (verify_queue(pq_ptr->qname))
		{
			qd[numq].class = pq_ptr->class;
			strcpy(qd[numq].name,pq_ptr->qname);				/* Copy queue name into array.		*/ 
			strcpy(quename[numq],pq_ptr->qname);				/* Copy queue name into array.		*/ 
			numq++;								/* Increment counter.			*/
			orig_numq++;							/* Increment counter.			*/
		}
		pq_ptr = (pq_id *)pq_ptr->next;
	}
	while (prt_ptr)									/* While have data from LPMAP.		*/
	{
		if (verify_queue(prt_ptr->qname))
		{
			qd[numq].class = prt_ptr->class;
			strcpy(qd[numq].name,prt_ptr->qname);				/* Copy queue name into array.		*/ 
			strcpy(quename[numq],prt_ptr->qname);				/* Copy queue name into array.		*/ 
			numq++;								/* Increment counter.			*/
			orig_numq++;							/* Increment counter.			*/
		}
		prt_ptr = (prt_id *)prt_ptr->next;
	}
	if (numq == 0)									/* If no files in LPMAP or PQMAP or	*/
	{										/* standalone queue management.		*/
		return;
	}
	if (numq > 1)									/* If more than one queue.		*/
	{
		qsort(quename,numq,32,strcmp);						/* Sort the array.			*/
		for (i = 0; i < (numq-1); i++)						/* Check for duplicates.		*/
		{
			while (strcmp(quename[i],quename[i+1]) == 0)			/* If two are the same.			*/
			{
				strcpy(quename[i+1],"zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");
				qsort(quename,numq,32,strcmp);				/* Sort the array.			*/
				numq--;							/* Decrement number of queues.		*/
			}
		}
	}
	if (!(quenameptr = calloc(numq,32*sizeof(char))))				/* Get some memory for the array.	*/
	{
		werrlog(ERRORCODE(2),32*numq,0,0,0,0,0,0,0);
		vexit();								/* Unconditional exit.			*/
	}
	memset(quenameptr,' ',32*numq);							/* Init the array to spaces.		*/
	tcpt = quenameptr;
	for ( i = 0; i < numq; i++)							/* Copy queues into contiquous memory.	*/
	{
		int len, k, used;

		used = copy_class(tcpt,qd,quename[i]);					/* First copy any classes associated.	*/
		for (j = 0; j < used; j++) tcpt++;					/* Move ptr to start position of name.	*/
		len = strlen(quename[i]);						/* Get the length of the queue name.	*/
		for (j = 0; j < len; j++)
		{
			*tcpt++ = quename[i][j];					/* Copy queue name into array.		*/
		}
		for (k = j+used; k < 31; k++)  *tcpt++ = ' ';				/* Pad with spaces.			*/
	}
	return;
}

static int verify_queue(qname)								/* Verify if the queue exists.		*/
char *qname;										/* Return TRUE if it does.		*/
{
	struct qui_itmlst_struct quibuf[3];						/* Return queue info for each QUI call.	*/
	struct io_status_block {							/* $GETQUI iosb parameter structure.	*/
		long status;								/* Completion status of func call.	*/
		long fill;								/* Condition value returned.		*/
	} iosb;
	int ret, i;
	char	srchname[32];								/* Search queue specifier.		*/
	long 	stat_ss, stat_qui, flgs;

	ret = TRUE;									/* Assume success.			*/
	for (i = 0; i < 31; i++) srchname[i] = *qname++;				/* Copy name into variable.		*/
	srchname[i] = '\0';								/* Null terminate the string.		*/

	quibuf[0].item_code = QUI$_SEARCH_NAME;						/* Init the buffer structure.		*/
	quibuf[0].buflen = strlen(srchname);
	quibuf[0].bufptr = (long *)srchname;						/* Pass the qname.			*/
	quibuf[0].retlen = 0;

	quibuf[1].item_code = QUI$_QUEUE_FLAGS;						/* Set up call to get the queue type.	*/
	quibuf[1].buflen = sizeof(long);
	quibuf[1].bufptr = (long *)&flgs;
	quibuf[1].retlen = 0;

	quibuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	quibuf[2].buflen = 0;
	quibuf[2].bufptr = (long *)0;
	quibuf[2].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);
											/* Check if queue is available.		*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;

	if (stat_qui != JBC$_NORMAL || stat_ss != SS$_NORMAL)
	{
		werrlog(ERRORCODE(4),srchname,stat_ss,stat_qui,0,0,0,0,0);
		werrlog(ERRORCODE(6),0,0,0,0,0,0,0,0);
		ret = FALSE;								/* Set so doesn't generate in list.	*/
	}
	return(ret);
}

static int copy_class(tcpt,qd,name)							/* First copy classes associated.	*/
char *tcpt, *name;									/* Return the number of chars used.	*/
struct que *qd;
{
	int i, nlen, first;
	int num, match_cnt;

	first = TRUE;
	match_cnt = 0;
	num = 0;
	for (i = 0; i < orig_numq; i++)							/* Test each queue in struct for match.	*/
	{
		if (match_cnt > 4)     							/* Too many to keep track of so		*/
		{									/* display ...				*/
			*tcpt++ = '.';
			*tcpt++ = '.';
			*tcpt++ = '.';
			num += 3;
			break;
		}
		else if (strcmp(qd[i].name,name) == 0)					/* If found a match.			*/
		{
			if (!first)                               
			{
				*tcpt++ = ',';						/* Seperate classes with a comma.	*/
				num++;
			}
			else first = FALSE;
			*tcpt++ = qd[i].class;
			match_cnt++;
			num++;
		}
	}
	*tcpt++ = ' ';									/* Seperate class info from name with	*/
	num++;										/*  a space.				*/
	return(num);	
}

#else	/* VMS */
static int dummy_setupqm;
#endif
