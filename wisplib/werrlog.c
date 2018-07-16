			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	NAME:	werrlog.c 
*/

#define INIT_ERR

#include <stdio.h>

#ifndef VMS	/* unix or MSDOS */
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#ifdef MSDOS
#include <io.h>
#endif

#include <errno.h>
#include <time.h>

#include "wperson.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wglobals.h"

char *getenv();

static char emsg[512] = { 0 };								/* The error message.			*/
static char eform[256] = { 0 };						       		/* The formatting string.		*/
static unsigned long last_dbid = 0;							/* The last message from the database.	*/

extern char WISPRUNNAME[8];

void werrlog(id,p1,p2,p3,p4,p5,p6,p7,p8)
unsigned long id;                                                 
char *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8;
{
	int log_it;

	wglobals();									/* Pull in wglobals for VMS		*/

	log_it = 0;

	if (id % 100 == 1)								/* It's a Routine entry log.		*/
	{
		if (w_err_flag & LOG_SUBROUTINE_ENTRY) log_it = 1;			/* Are we logging them?			*/
		else return;								/* if not, just go.			*/
	}

	w_err_code = id;								/* Copy the error code.			*/

	w_err_p1 = p1;									/* And all the parms.			*/
	w_err_p2 = p2;
	w_err_p3 = p3;
	w_err_p4 = p4;
	w_err_p5 = p5;
	w_err_p6 = p6;
	w_err_p7 = p7;
	w_err_p8 = p8;

	if (!(w_err_flag & ENABLE_LOGGING)) return;					/* Logging is off.			*/

	if (w_err_flag & LOG_EXCEPTIONS_ONLY)
	{
	    if (!(w_err_code & 1)) log_it = 1;						/* Only exceptions & this is one.	*/
	}
	else
	{
	    log_it = 1;									/* Otherwise log any error.		*/
	}

	if (!log_it) return;

	form_error(id,p1,p2,p3,p4,p5,p6,p7,p8);						/* Form the error message.		*/

	if (w_err_flag & LOG_LOGFILE) write_err();					/* Write error to log file.		*/

	if (w_err_flag & LOG_SCREEN) print_err();					/* Print error to stdout.		*/

}

void werr_write(buff)									/* Write out a buffer to the error log.	*/
char *buff;
{
	FILE *efile;									/* Pointer to error log file.		*/
	static	int	first=1;

	if (first)									/* Need to get the file name.		*/
	{
		first=1; 								/* Don't undo first yet.		*/
		werrpath();								/* Init werrlog_path			*/
	}

	efile = fopen(werrlog_path,"a");						/* First try to append.			*/
	if (!efile) efile = fopen(werrlog_path,"w");					/* Doesn't exist, create it.		*/

	if (!efile) return;								/* Bad news, can't do it.		*/ 

	fprintf(efile,"%s",buff);							/* dump it out as-is.			*/

	fclose(efile);									/* All done!				*/

	if ( first )
	{
		first = 0;								/* Now turn off first.			*/
		chmod(werrlog_path, 0666);						/* Allow all to write.			*/
	}

}

static int write_err()									/* Write error to log file.		*/
{
	time_t clock;
	char	buff[512];

	buff[0] = '\0';
	if (!(w_err_code & 1)) 								/* If exceptions then identify.		*/
	{
		clock = time(0);
		sprintf(buff,"\n%sWISPRUNNAME(%8.8s)\n",ctime(&clock),WISPRUNNAME);
	}                                                           
	sprintf(&buff[strlen(buff)],"(%6ld) %s\n",w_err_code, emsg);			/* Pretty easy.				*/
	werr_write(buff);
	w_err_logged = 1;								/* Flag we did one.			*/
	return(1);
}

static int print_err()									/* Write error to stdout		*/
{

	if ( wbackground() ) printf("\n\r%s\n\r",emsg);					/* Pretty easy.				*/
	else
	{
		emsg[255] = '\0';
		werrvre(emsg);
	}
	return(1);
}

static int form_error(id,p1,p2,p3,p4,p5,p6,p7,p8)					/* Format the error message.		*/
unsigned long id;
char *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8;
{
	int i;

	if (!find_msg(id))								/* Is there a message for this error?	*/
	{
		sprintf(emsg,"%%WISP-E-NOMSG WISP error, message number %ld. ",id);	/* No message found.			*/
	}
	else
	{
		sprintf(emsg,eform,p1,p2,p3,p4,p5,p6,p7,p8);				/* Generate the message.		*/
	}
}

static char msgfname[256] = { 0 };

static int find_msg(id)									/* Find the message in the message db.	*/
unsigned long id;
{
	FILE *msgfile, *fopen();
	char *ptr;
	unsigned long lo_id, hi_id, cur_id, lo_idx, hi_idx, cur_idx, msg_idx, num_msgs, idx_diff;

	if (last_dbid == id) return(1);							/* already got it in buffer.		*/

	if (!msgfname[0])								/* Need to get the name of the msg file	*/
	{
#ifdef VMS
		strcpy(msgfname,"wisp$config:");
		strcat(msgfname,WISP_MESSAGE_FILE);
#endif	/* VMS */
#ifdef unix
		if ( ptr = getenv( WISP_CONFIG_ENV ) )					/* Find out the path to wisp$config.	*/
		{									/* Unix version, of course.		*/
			strcpy( msgfname, ptr );
			strcat( msgfname, "/" );
			strcat( msgfname, WISP_MESSAGE_FILE);
		}
		else no_wispconfig();
#endif	/* unix */
#ifdef MSDOS
		if ( ptr = getenv( WISP_CONFIG_ENV ) )					/* Find out the path to wisp$config.	*/
		{									/* MSDOS version, of course.		*/
			strcpy( msgfname, ptr );
			strcat( msgfname, "\\" );
			strcat( msgfname, WISP_MESSAGE_FILE);
		}
		else no_wispconfig();
#endif	/* MSDOS */
	}

#ifndef ultrix
	msgfile = fopen(msgfname,"rb");							/* Open the indexed file.		*/
#else
	msgfile = fopen(msgfname,"r");							/* Open the indexed file.		*/
#endif

	if (!msgfile)									/* Unable to open message file		*/
	{
		sprintf(emsg,"%%WERRLOG-E-OPEN Unable to open message file %s",msgfname);
		print_err();
		return(0);
	}

	cur_idx = 0;									/* Use to point to count.		*/

	fseek(msgfile,cur_idx,0);							/* Find out how many in the file.	*/
	fread(&num_msgs,4,1,msgfile);

	lo_idx = 4;									/* The first ID is in byte 4.		*/
	hi_idx = (8 * num_msgs) - 4;							/* The last message id is here.		*/

	idx_diff = ((hi_idx - lo_idx) >> 1) & 0xfffffff8;				/* Get Median between 2 items. mod 8	*/

	cur_idx = lo_idx + idx_diff;							/* Set current position.		*/

	msg_idx = 0;									/* No message found.			*/

	fseek(msgfile,cur_idx,0);							/* Seek to current value.		*/
	fread(&cur_id,4,1,msgfile);							/* And read it.				*/

	for (;;)
	{
		if (id == cur_id || (lo_idx >= hi_idx))					/* Found it, or not there.		*/
		{
			break;								/* GET OUT, EH!				*/
		}
		else if (id > cur_id)							/* If id is greater than current.	*/
		{									/* Need to advance to hi index.		*/
			idx_diff = ((hi_idx - cur_idx) >> 1) & 0xfffffff8;		/* Get Median between 2 items. Mod 8	*/
			lo_idx = cur_idx + 8;						/* This is the bottom now.		*/
			if (!idx_diff) idx_diff = 8;
			cur_idx = cur_idx + idx_diff;
		}
		else
		{
			idx_diff = ((cur_idx - lo_idx) >> 1) & 0xfffffff8;		/* Get Median between 2 items. Mod 8	*/
			hi_idx = cur_idx - 8;						/* This is the top now.			*/
			if (!idx_diff) idx_diff = 8;
			cur_idx = cur_idx - idx_diff;
		}

		fseek(msgfile,cur_idx,0);						/* Seek to current value.		*/
		fread(&cur_id,4,1,msgfile);						/* And read it.				*/
	}

	if (id == cur_id)
	{
		cur_idx = cur_idx + 4;							/* point to where message is.		*/
		fseek(msgfile,cur_idx,0);						/* Position the file.			*/
		fread(&msg_idx,4,1,msgfile);						/* Read the index value.		*/
		cur_idx = cur_idx + 8;							/* Get index value of next message.	*/
		fseek(msgfile,cur_idx,0);						/* Seek to it.				*/
		fread(&cur_idx,4,1,msgfile);						/* Get address of next message.		*/
		idx_diff = cur_idx - msg_idx;						/* This is the length of the message.	*/
	}
	else
	{
		fclose(msgfile);
		return(0);								/* No message found.			*/
	}
											/* At this point, msg_idx points to the	*/
											/* location where the message index is.	*/
											/* And idx_diff is the length.		*/
	fseek(msgfile,msg_idx,0);							/* Now point to the message.		*/
	fread(&eform[0],(int)idx_diff,1,msgfile);					/* Read the text.			*/

	eform[idx_diff] = 0;								/* Null terminate.			*/

	fclose(msgfile);

	last_dbid = id;									/* Remember it.				*/

	return(1);									/* All done.				*/
}
#ifdef VMS
void werrset()
{
}
#endif	/* VMS */

#ifdef unix
void werrset()
{
	char	*ptr;
	char	buff[50];

	ptr = getenv("WISPDEBUG");

	if (! ptr) return;

	strcpy(buff,ptr);
	upper_string(buff);

	if ( strcmp(buff,"FULL") == 0 )
	{
		w_err_flag = 19;
		return;
	}	

	if ( strcmp(buff,"ENTRY") == 0 )
	{
		w_err_flag = 27;
		return;
	}	
}
#endif	/* unix */

#ifdef MSDOS
void werrset()
{
	char	*ptr;
	char	buff[50];

	ptr = getenv("WISPDEBUG");

	if (! ptr) return;

	strcpy(buff,ptr);
	upper_string(buff);

	if ( strcmp(buff,"FULL") == 0 )
	{
		w_err_flag = 19;
		return;
	}	

	if ( strcmp(buff,"ENTRY") == 0 )
	{
		w_err_flag = 27;
		return;
	}	
}
#endif	/* MSDOS */
