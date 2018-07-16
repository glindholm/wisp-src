			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/



#include <stdio.h>
#include <varargs.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "vwang.h"

#define		ROUTINE		35000

#ifdef VMS

#include <ssdef.h>
#include <descrip.h>
#include <psldef.h>
#include <iodef.h>
#include <lnmdef.h>
#include <v/video.h>
#include <v/vlocal.h>

extern int DestroyAllPorts();								/* Declare the exit handler routine.	*/

static struct {
		int4 flink;								/* The forward link (VMS only)		*/
		char *exproc;								/* The pointer to the proc.		*/
		int4 argcnt;							/* The arg count (0)			*/
		int4 *condit;							/* Pointer to the condition value.	*/
	      } exit_handler;

static unsigned int mbx_ef, timr_ef;							/* The event flags.			*/
extern unsigned int vmbx_ef;
static unsigned int ef_mask = 0;

static int4 conval;									/* A place to hold the condition value	*/

#define CR_nargs  3
#define DE_nargs  3
#define XM_nargs  5
#define CH_nargs  7
#define MIN_ARGS  3
#define TIMR_REQID 39

/* Message port buffer defn's */
#define MIN_BUFSIZE 1
#define MAX_BUFSIZE 2014
#define DEF_BUFSIZE 2014

/* Various field length defn's */
#define PORTNAME_SIZE   4
#define MBX_STRLENGTH  12
#define PREFIX_LENGTH   8

/* Internal Message return codes */
#define SS_ERROR       -1
#define MYPORT          0
#define OTHERPORT       1
#define FUBAR          -2

/* Return codes for CR service */
#define PORTINUSE_BYOTHER 4
#define PORTINUSE_BYME    8

/* Return codes for DE  service */
#define PORTNOTEMPTY 4
#define NOSUCHPORT 8

/* Return codes for XW, XM service */
#define PORTNOTCREATED  4
#define BUFFERFULL      8
#define PRIVDPORT      12

/* Return codes for CH service */
#define TIMEDOUT     8
#define KEYBDLOCK   12
#define CHECKCANCEL 16
#define NOTMYPORT   16

/* Globals to be used internally to figure out what went wrong, etc... */

int MESSAGE_ss_status = SS$_NORMAL;
short MESSAGE_iosb_status = SS$_NORMAL;
#include "message1.d"

/* Structure for internal port list:

    port_name = 4 character port name specified by user
    chan = Channel # assigned for created MBX
    lognam = Logical name associated with this port name
    next = Next entry in list
*/
struct mbx_list {
                char *port_name;
                short chan;
                struct dsc$descriptor_s *lognam;
                struct mbx_list *next;
                };

struct mbx_list *port_list = NULL;

struct mbx_iosb_blk {
                short status;
                short byte_count;
                int pid;
                    };

	/* #ifdef VMS	*/
void message( va_alist )
va_dcl
{

	char *function_type;
	char *port_name;
	va_list argp;
	int argc;

	va_count(argc);
	va_start(argp);

	if (argc < MIN_ARGS )
	{
		werrlog(ERRORCODE(2),argc,0,0,0,0,0,0,0);
		return;
	}
	function_type = va_arg(argp, char*);
	port_name = va_arg(argp, char*);

	werrlog(ERRORCODE(1),function_type,port_name,0,0,0,0,0,0);

	if (strncmp(function_type, "CR", 2)==0)
	{
	        int *buffer_size,l_bufsiz;
	        char *keep_flag;
	        int *return_code;


	        if (argc < CR_nargs)
		{
			werrlog(ERRORCODE(2),argc,0,0,0,0,0,0,0);
			return;
		}

		if (argc > CR_nargs)							/* Must have included buffer size.	*/
		{
			buffer_size = va_arg(argp, int*);
			l_bufsiz = *buffer_size;
			wswap(&l_bufsiz);
		}
		else
		{
			l_bufsiz = 2014;						/* Otherwise, set default.		*/
		}

		if (argc > CR_nargs + 1)						/* Also included keep flag.		*/
		{
			keep_flag = va_arg(argp, char*);
		}

		keep_flag = "N";
	        return_code = va_arg(argp, int*);
	        *return_code = CreateMessagePort( port_name, l_bufsiz, *keep_flag);
		wswap(return_code);
	}


	/* #ifdef VMS	*/
    
	else if (strncmp(function_type, "DE", 2)==0) 
	{
	        int *return_code;

	        if (argc != DE_nargs)
		{
			werrlog(ERRORCODE(2),argc,0,0,0,0,0,0,0);
			return;
		}

	        return_code = va_arg(argp, int*);
	        *return_code = DestroyMessagePort( port_name );
		wswap(return_code);
	}

	else if (strncmp(function_type, "X", 1)==0) 
	{
		char *msg;
	        int *msg_length,l_len;
	        int *return_code;
	        int wait_flag;

	        wait_flag = *(function_type+1) == 'W' ? TRUE : FALSE;
	        if (argc != XM_nargs)
		{
			werrlog(ERRORCODE(2),argc,0,0,0,0,0,0,0);
			return;
		}

	        msg = va_arg(argp, char*);
	        msg_length = va_arg(argp, int*);
		l_len = *msg_length;
		wswap(&l_len);
	        return_code = va_arg(argp, int*);
	        *return_code = XmitMessage( port_name, msg, l_len, wait_flag);
		wswap(return_code);
	}


	/* #ifdef VMS	*/
	else if (strncmp(function_type, "CH", 2)==0) 
	{
	        char *check_type;
	        int *time_interval,l_time;
	        char *msg;
	        int *msg_length;
	        char *disable_keys = " "; 							/* init disable keys to default */
	        int *return_code;
        
	        if (argc < CH_nargs)
		{
			werrlog(ERRORCODE(2),argc,0,0,0,0,0,0,0);
			return;
		}

	        check_type = va_arg(argp, char*);
	        time_interval = va_arg(argp, int*);
		l_time = *time_interval;
		wswap(&l_time);
	        msg = va_arg(argp, char*);
	        msg_length = va_arg(argp, int*);
		wswap(msg_length);
	        if (argc == CH_nargs+1)         /* optional arg (arg7) present */
	            disable_keys = va_arg(argp, char*);
		else
		    disable_keys = " ";
	        return_code = va_arg(argp, int*);
	        *return_code = CheckMessages( port_name, *check_type, l_time,
	                                      msg, msg_length, *disable_keys);
		wswap(msg_length);
		wswap(return_code);
	}

	else /* Someone isn't playing with a full deck...*/
	{
		werrlog(ERRORCODE(4),function_type,0,0,0,0,0,0,0);
	}
}


	/* #ifdef VMS	*/
int CreateMessagePort( char *port_name, unsigned int buf_size, char keep_flag)
{
    	unsigned char prmflg = '\0';								/* Temporary mailbox 		*/
    	unsigned short chan;
    	unsigned int maxmsg;
    	unsigned int bufquo;
    	static   int did_exit_proc = 0;
    	unsigned int promsk = 0; /* Default protection: S:RWLP, O:RWLP, G:RWLP, W:RWLP */
    	unsigned int acmode = PSL$C_USER; 								/* access mode = USER */
    	unsigned int status;
    	char lognam[PREFIX_LENGTH+PORTNAME_SIZE+1];
    	struct mbx_list *ptr;
#include "message2.d"

    	status = LookUpPort(port_name, &ptr);
    	if (status == MYPORT) 
    		return(PORTINUSE_BYME);
    	else if (status == OTHERPORT)
        	return(PORTINUSE_BYOTHER);

    	status = FormLogical(port_name, &lognam_desc);
    	if (status != SS$_NORMAL) {
        	MESSAGE_ss_status = status;
		werrlog(ERRORCODE(6),port_name,0,0,0,0,0,0,0);
        	return(SS_ERROR);
    	}

    	if (buf_size < MIN_BUFSIZE || buf_size > MAX_BUFSIZE )
        	maxmsg = DEF_BUFSIZE;
    	else
        	maxmsg = buf_size;
    	bufquo = maxmsg;
    
    	status = sys$crembx( prmflg, &chan, maxmsg, bufquo, promsk, acmode, &lognam_desc);
    	if (status != SS$_NORMAL) 
	{
        	MESSAGE_ss_status = status;
		werrlog(ERRORCODE(8),status,0,0,0,0,0,0,0);
        	return(SS_ERROR);
    	}

    	AddtoPortList( port_name, chan, &lognam_desc);

    	if (!did_exit_proc)
	{
		did_exit_proc = 1;
		exit_handler.exproc = (char *)DestroyAllPorts;				/* Install the exit handler on VMS.	*/
		exit_handler.argcnt = 0;
		exit_handler.condit = &conval;
		sys$dclexh(&exit_handler);
	}
    	return(0);
}


	/* #ifdef VMS	*/
int DestroyMessagePort( char *port_name )
{
    	struct mbx_list *ptr;
    	unsigned int status;
    	unsigned char acmode = PSL$C_USER;								/* access mode = USER */
    

    	if (LookUpPort(port_name, &ptr) == MYPORT) 
	{
        	status = sys$delmbx( ptr->chan );
        	status = sys$dellnm( &mbxlnm_table, ptr->lognam, &acmode);
        	RemovePort(ptr);
        	return(0);
    	}
    	else
	{
        	return(NOSUCHPORT);
	}
}


	/* #ifdef VMS	*/
int XmitMessage( char *port_name, char *msg, unsigned int msg_size, int wait_flag )
{
    	struct mbx_list *ptr;
    	unsigned int status;
    	char mbx[MBX_STRLENGTH+1];
    	short   chan;
    	struct mbx_iosb_blk iosb;
    	unsigned int mbx_ef;
#include "message3.d"

	if (!(msg_size > 0)) 								/* if message size is not > 0 then some-*/
		return(FUBAR);								/* thing is wrong 			*/

	FormLogical(port_name, &mbx_desc);
	status = sys$assign(&mbx_desc, &chan, PSL$C_USER, 0);

	if (status == SS$_NOPRIV)							/* if we got a 'no priv...' error, the	*/
		return(PRIVDPORT);							/* port owner must be anti-social.	*/

	if (status == SS$_IVDEVNAM) 							/* Invalid device name, port don't exist*/
		return(PORTNOTCREATED);

	MESSAGE_ss_status = status;

	if (status != SS$_NORMAL)							/* if its some other error status, then */
	{
		werrlog(ERRORCODE(10),status,0,0,0,0,0,0,0);
	        return(SS_ERROR);       /* something else is wrong... */
	}

	status = lib$get_ef(&mbx_ef);
	if (status != SS$_NORMAL)
	{
		MESSAGE_ss_status = status;
		werrlog(ERRORCODE(12),status,0,0,0,0,0,0,0);
		return(SS_ERROR);
	}
    	if (wait_flag) 
	{     /* Function is XW */
        	status = sys$qiow(mbx_ef, chan, IO$_WRITEVBLK,
                          &iosb, 0, 0, msg, msg_size, 0, 0, 0, 0);
        	sys$dassgn(chan);
        	lib$free_ef(&mbx_ef);
    	}
    	else 
	{               /* Function is XM */
        	status = sys$qiow(mbx_ef, chan, IO$_WRITEVBLK|IO$M_NOW|IO$M_NORSWAIT,
                          &iosb, 0, 0, msg, msg_size, 0, 0, 0, 0);
        	sys$dassgn(chan);
        	lib$free_ef(&mbx_ef);
        	if (status == SS$_MBFULL) /* receiving port is full */
            		return(BUFFERFULL);
    	}

/* check returns... */

    	if (status == SS$_NOPRIV) /* this is a hell of a time to find out */
        	return(PRIVDPORT);    /* that we can't write to the port, eh? */

    	if (status == SS$_MBTOOSML) /* buffer space of receiving mbx is too */
        	return(BUFFERFULL);     /* small to receive a msg of this size */
                                /* Should we return this on a XW? dunno */

    	if ((status == SS$_NORMAL) && (iosb.status == SS$_NORMAL))
        	return(0);

    	MESSAGE_ss_status = status;
    	MESSAGE_iosb_status = iosb.status;

	werrlog(ERRORCODE(14),status,iosb.status,0,0,0,0,0,0);

    	return(SS_ERROR);           /* catch-all; something other than the   */
                                /* above went wrong, but we don't know   */
                                /* what (well, we do, but the VS codes   */
                                /* that are defined don't cover 'other'  */
                                /* errors, so this is the best we can do */
}


	/* #ifdef VMS	*/
int CheckMessages( char *port_name, char check_type, int time_interval,
                  char *msg, unsigned int *msg_size, char disable_keys )
{
	unsigned qio_status, status;
	struct mbx_iosb_blk iosb;
	struct mbx_list *ptr;
	char *temp;
	int inchar;
	int wait_time[2];
	int return_status = 0;
	unsigned ef_cluster;

	if (0==time_interval) time_interval = 1;					/* Patch for time=0, change to 1/100	*/
	
	wait_time[0] = -time_interval*100000;						/* Initialize the wait time.		*/
	wait_time[1] = 0xffffffff;
	temp = (char *)malloc(*msg_size+1);    						/* Setup temp buffer to hold message.	*/

	if (LookUpPort(port_name, &ptr) != MYPORT)
	        return(NOTMYPORT);							/* This isn't our port to check 	*/

	status = lib$get_ef(&mbx_ef);
	if (status != SS$_NORMAL)
	{
		MESSAGE_ss_status = status;
		werrlog(ERRORCODE(12),status,0,0,0,0,0,0,0);
		return(CHECKCANCEL);
	}
	ef_mask |= 1<<(mbx_ef > 31 ? mbx_ef - 32 : mbx_ef);

	status = lib$get_ef(&timr_ef);
	if (status != SS$_NORMAL)
	{
		MESSAGE_ss_status = status;
		werrlog(ERRORCODE(12),status,0,0,0,0,0,0,0);
		return(CHECKCANCEL);
	}

    	switch(check_type) 
	{

    	case 'W':									/* wait until message is received */

        	qio_status = sys$qiow(mbx_ef, ptr->chan, IO$_READVBLK, &iosb, 0, 0, temp, *msg_size, 0, 0, 0, 0);
        	break;

    	case 'T':							/* wait until timer expires or message is received */

		qio_status = sys$qio(mbx_ef, ptr->chan, IO$_READVBLK, &iosb, 0, 0, temp, *msg_size, 0, 0, 0, 0);

		if (qio_status != SS$_NORMAL) break;					/* Error starting mailbox.		*/

		status = sys$setimr(timr_ef, &wait_time[0], 0, TIMR_REQID, 0);		/* Start timer.				*/
		if (status != SS$_NORMAL)
		{
			sys$cancel( ptr->chan);						/* cancel pending mbx read if something */
			break;								/* went wrong */
		}

		ef_mask |= 1<<(timr_ef > 31 ? timr_ef - 32 : timr_ef);			/* add timer event to event mask */

		sys$wflor(mbx_ef, ef_mask);					/* wait for the mbx read or the timer to expire */

		if (sys$readef(mbx_ef, &ef_cluster) == SS$_WASCLR)			/* Did the mailbox read?		*/
		{									/* No it did not.			*/
			sys$cancel(ptr->chan);						/* Cancel pending mbx read 		*/
		}
		else									/* Yes, it read.			*/
		{
			sys$cantim(TIMR_REQID, PSL$C_USER);				/* Cancel the timer			*/
			return_status = 0;
			break;
		}

		if (sys$readef(timr_ef, &ef_cluster) == SS$_WASCLR)			/* Did the timer expire?		*/
		{
			sys$cantim(TIMR_REQID, PSL$C_USER);				/* No, cancel the timer			*/
		}
		else
		{
			return_status = TIMEDOUT;					/* Yes, say so.				*/
		}

		break;

    	case 'K':								/* wait until key press or message is received */

check_k:

		if (vwang_aid() != AID_UNLOCKED)
		{
			return_status = KEYBDLOCK;					/* Yes, so return it.			*/
			break;
		}
		else if (inchar = vcheck())						/* See if there is a keypress in bufr.	*/
		{
			char	aid;
			
			vpushc(inchar);						/* Push the char.			*/
			inchar = vgetm();					/* Get its meta value.			*/
			aid = meta_aid(inchar);					/* Translate to AID char		*/
			if (aid)						/* If it is an AID char then set it	*/
			{
				set_aid(aid);
			}
			else
			{
				goto check_k;						/* Not a pfkey, try again.		*/
			}
			return_status = KEYBDLOCK;
			break;
		}

        	ef_mask |= 1<<(vmbx_ef > 31 ? vmbx_ef - 32 : vmbx_ef);			/* Add pfkey ef to ef list.		*/

wait_k:
											/* Start up the message read.		*/
        	qio_status = sys$qio(mbx_ef, ptr->chan, IO$_READVBLK, &iosb, 0, 0, temp, *msg_size, 0, 0, 0, 0);
        	if (qio_status != SS$_NORMAL) break;

        	sys$wflor(mbx_ef, ef_mask);						/* wait for the mbx read or a keypress */

        	if (sys$readef(mbx_ef, &ef_cluster) == SS$_WASCLR)			/* Did the mailbox read?		*/
		{									/* No it did not.			*/
			sys$cancel(ptr->chan); 						/* Cancel pending mbx read 		*/
		}
		else
		{									/* The mailbox did read.		*/
			break;								/* Done with it.			*/
		}

		if (!(sys$readef(vmbx_ef, &ef_cluster) == SS$_WASCLR))			/* Some key was pressed.		*/
		{
			char	aid;

			inchar = vcheck();						/* See if there is a keypress in bufr.	*/
			
			vpushc(inchar);						/* Push the char.			*/
			inchar = vgetm();					/* Get its meta value.			*/
			aid = meta_aid(inchar);					/* Translate to AID char		*/
			if (aid)						/* If it is an AID char then set it	*/
			{
				set_aid(aid);
			}
			else
			{
				goto wait_k;						/* Nothing usefull, Wait for more.	*/
			}
			return_status = KEYBDLOCK;
		}

	        break;

	case 'B':							/* wait until timer expires, key is pressed or msg 	*/
                     									/* is received */ 
check_b:

		if (vwang_aid() != AID_UNLOCKED)					/* Is the keyboard locked		*/
		{
			return_status = KEYBDLOCK;					/* Yes, so return it.			*/
			break;
		}
		else if (inchar = vcheck())						/* See if there is a keypress in bufr.	*/
		{
			char	aid;
			
			vpushc(inchar);						/* Push the char.			*/
			inchar = vgetm();					/* Get its meta value.			*/
			aid = meta_aid(inchar);					/* Translate to AID char		*/
			if (aid)						/* If it is an AID char then set it	*/
			{
				set_aid(aid);
			}
			else
			{
				goto check_b;						/* Not a pfkey, try again.		*/
			}
			return_status = KEYBDLOCK;
			break;
		}

        	ef_mask |= 1<<(timr_ef > 31 ? timr_ef - 32 : timr_ef);			/* add timer event to event mask 	*/

        	ef_mask |= 1<<(vmbx_ef > 31 ? vmbx_ef - 32 : vmbx_ef);			/* Add keyboard event too.		*/

wait_b:
											/* Start the message port read.		*/
        	qio_status = sys$qio(mbx_ef, ptr->chan, IO$_READVBLK, &iosb, 0, 0, temp, *msg_size, 0, 0, 0, 0);
        	if (qio_status != SS$_NORMAL) break;

        	status = sys$setimr(timr_ef, &wait_time[0], 0, TIMR_REQID, 0);		/* Start the timer too.			*/

        	if (status != SS$_NORMAL)
		{
			sys$cancel(ptr->chan);						/* cancel pending mbx read if something */
			break;								/* went wrong 				*/
        	}

        	sys$wflor(mbx_ef, ef_mask);     					/* wait for the mbx read or the timer	*/
											/* or a keypress.			*/
        	if (sys$readef(mbx_ef, &ef_cluster) == SS$_WASCLR)			/* Did the mailbox read?		*/
		{
			sys$cancel(ptr->chan); 						/* No, Cancel pending mbx read 		*/
		}
		else
		{									/* Yes, get out.			*/
			sys$cantim(TIMR_REQID, PSL$C_USER); 				/* cancel the timer 			*/
			return_status = 0;
			break;
		}

	        if (sys$readef(timr_ef, &ef_cluster) == SS$_WASCLR)			/* Did the timer expire?		*/
		{									/* No it did not.			*/
			sys$cantim(TIMR_REQID, PSL$C_USER); 				/* cancel the timer 			*/
		}
	        else
		{									/* Yes it did, get out now.		*/
			return_status = TIMEDOUT;
			break;
		}

	        if (!(sys$readef(vmbx_ef, &ef_cluster) == SS$_WASCLR))			/* Was a key pressed?			*/
		{
			char	aid;

			inchar = vcheck();						/* See if there is a keypress in bufr.	*/
			
			vpushc(inchar);							/* Push the char.			*/
			inchar = vgetm();						/* Get its meta value.			*/
			aid = meta_aid(inchar);						/* Translate to AID char		*/
			if (aid)							/* If it is an AID char then set it	*/
			{
				set_aid(aid);
			}
			else
			{
				goto wait_b;						/* Go wait for more.			*/
			}
			return_status = KEYBDLOCK;
		}

	        break;

	default: 
	        qio_status = SS$_BADPARAM;
    	}

	lib$free_ef(&mbx_ef);
	lib$free_ef(&timr_ef);

	if ((qio_status == SS$_NORMAL) && (iosb.status == SS$_NORMAL))
	{
		memcpy(msg, temp, iosb.byte_count);					/* copy message into user buffer	*/
		msg_size = (unsigned int *)iosb.byte_count;				/* set message size to actual		*/
		free(temp);
		return(0);							/* bytes read				*/
	}

        free(temp);
	if (return_status != 0) return(return_status);
	return(CHECKCANCEL);								/* if all else fails, return this... */
}


	/* #ifdef VMS	*/
int AddtoPortList ( char *port_name, short chan, 
                    struct dsc$descriptor_s *lognam )
{
    	struct mbx_list *newmbx;
    	char *newport;

    	newmbx = (struct mbx_list *)malloc(sizeof(struct mbx_list));				/* grab new mbx list struct */

/* Stuff portname, channel, and logical name into new structure */

    	newmbx->port_name = (char *)malloc(PORTNAME_SIZE);
    	strncpy(newmbx->port_name, port_name, PORTNAME_SIZE);
    	newmbx->chan = chan;
    	newmbx->lognam = (struct dsc$descriptor_s *)malloc(sizeof(struct dsc$descriptor_s));
    	newmbx->lognam->dsc$w_length = lognam->dsc$w_length;
    	newmbx->lognam->dsc$b_dtype = lognam->dsc$b_dtype;
    	newmbx->lognam->dsc$b_class = lognam->dsc$b_class;
    	newmbx->lognam->dsc$a_pointer = (char *)malloc(lognam->dsc$w_length);
    	strncpy(newmbx->lognam->dsc$a_pointer, lognam->dsc$a_pointer,
            lognam->dsc$w_length);

    	newmbx->next = port_list;              /* insert new port at head of */
    	port_list = newmbx;                    /* port list */
    	return(SS$_NORMAL);
}


	/* #ifdef VMS	*/
int DestroyAllPorts()
{
    	unsigned status;
    	unsigned char acmode = PSL$C_USER;								/* access mode = USER */

    	while(port_list != NULL) 
	{
        	status = sys$delmbx( port_list->chan );
        	status = sys$dellnm( &mbxlnm_table, port_list->lognam, &acmode);
        	port_list = port_list->next;
    	}
    	return(SS$_NORMAL);
}


	/* #ifdef VMS	*/
int FormLogical( char *port_name, struct dsc$descriptor_s *lognam_desc )
{
    	unsigned status;
    	char temp_port[PORTNAME_SIZE+1];
#include "message4.d"

    	temp_desc.dsc$w_length = PORTNAME_SIZE;
    	strncpy(temp_port, port_name, PORTNAME_SIZE);
    	status = str$concat( lognam_desc, &lognam_prefix, &temp_desc);

    	return(status);
}


	/* #ifdef VMS	*/
int LookUpPort( char *port_name, struct mbx_list *(*node) )
{
    	struct mbx_list *ptr;
    	char templog[PREFIX_LENGTH+PORTNAME_SIZE+1];
    	char tempequiv[2]; 						/* don't really need to get the translation in full */
    	unsigned status;
    	uint4 lnmatt = LNM$M_CASE_BLIND;
    	uint4 itemlist;
    	unsigned char lnmaccmod = PSL$C_USER;
#include "message5.d"

/* First, look for given port in our list of ports. If there, then we're
   already using it
*/
    	*node = NULL;
    	ptr = port_list;
    	while (ptr != NULL) 
	{
        	if (strncmp(ptr->port_name, port_name, PORTNAME_SIZE) == 0) 
		{
            		*node = ptr;
            		return(MYPORT);
        	}
        	ptr = ptr->next;
	}

/* Next, form the standard logical name from the port_name and look for the
    the logical name. If its found, then someone else is using it (because
    we already looked in our list, we wouldn't have gotten this far if it
    were ours.
*/
    	FormLogical( port_name, &temp_desc);

    	itemlist = 0;
    	status = sys$trnlnm(&lnmatt,&mbxlnm_table,&temp_desc,&lnmaccmod,&itemlist);
    	if (status == SS$_NORMAL)
        	return(OTHERPORT);
    	else 
        	return(NOSUCHPORT);
}


	/* #ifdef VMS	*/
int RemovePort( struct mbx_list *node)
{
    	struct mbx_list *ptr, *prev;

    	ptr = port_list;
    	prev = NULL;

    	while (ptr != NULL) 
	{
        	if (ptr == node) 
		{ /* we found it! */
            		if (prev != NULL) 
			{ /* if we're not at the head of the list */
                		prev->next = ptr->next; 
            		}
            		else 
			{  /* delete entry at head */
                		port_list = ptr->next;  /* re-adjust listhead ptr */
            		}     
			free(node);
			return(0);
        	}
        	prev = ptr;
        	ptr = ptr->next;
    	}

/* If we made it to here, we were either given a bogus entry to delete or
   something is wrong internally and we lost track of the entry somehow
*/

    	return(FUBAR);
}

#endif	/* #ifdef VMS	*/

#ifdef unix

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <termio.h>
#include "movebin.h"

char *funcs[] = { "CR", "DE", "XM", "XW", "CH", 0 };

#define M_CREATE 0
#define M_DELETE 1
#define M_XMIT  2
#define M_XMTWT 3
#define M_CHECK 4
#define M_INV -1

#define SUCCESS 0
#define FAILURE 64
#define CR_PORT_BUSY 4
#define CR_PORT_OURS 8
#define DE_MSG_LOST 4
#define DE_NO_PORT 8
#define XM_NO_PORT 4
#define XM_PORT_FULL 8
#define XM_PORT_PRIV 12
#define CH_PORT_TIMEOUT 8
#define CH_NO_PORT 16
#define CH_PORT_FKEYLOCK 12

#define WMSGMAX 2014
#define MSGTYPE 1L

static int timeout;
int msqid;
struct {
	long mtype;
	char mtext[WMSGMAX];
} msg;
struct msqid_ds mctlbuf;
static struct termio t_save,t_new;
static int creport();
static int delport();
static int sndmsg();
static int get_msg();
static int makemsgpath();
static int functype();
static void timed_out();
static key_t keyval();

void MESSAGE( va_alist )
va_dcl
{
	va_list arg_list;
	int arg_count;
	char *chptr, *intptr;
	char funcstr[3], prname[5];
	int4 wait, func, ourret, *retcode;
	char *receiver;
	int4 check_t, time_int, mess_len;
	int4 *time_int_p, *mess_len_p;
	char *mtext;

	va_start(arg_list);
	arg_count = va_count(arg_list);
	va_start(arg_list);

	wait = 0;
	memset(funcstr,0,sizeof(funcstr));						/* init char arrays			*/
	memset(prname,0,sizeof(prname));
	chptr = va_arg(arg_list,char *);						/* get function and prname		*/
	strncpy(funcstr,chptr,2);
	chptr = va_arg(arg_list,char *);
	strncpy(prname,chptr,4);
	arg_count -= 2;

	func = functype(funcstr);							/* get integer function type		*/
	switch (func)
	{
		case M_CREATE:	while (arg_count > 1) 
				{
					chptr = va_arg(arg_list,char*), --arg_count;	/* ignore all but return code		*/
				}
				retcode	= va_arg(arg_list,int4 *);			/* get retcode pointer			*/
				ourret	= creport(prname);				/* dispatch 				*/
				break;
		case M_DELETE:	retcode	= va_arg(arg_list,int4 *);			/* get retcode pointer			*/
				ourret	= delport(prname);				/* dispatch to handler			*/
				break;
		case M_XMTWT:	wait = TRUE;						/* set wait flag, then do normal xmit	*/
		case M_XMIT:	mtext	= va_arg(arg_list,char *); 			/* get text pointer			*/
				mess_len_p= va_arg(arg_list,int4 *);			/* and length				*/
				GETBIN(&mess_len,mess_len_p,sizeof(int4));
				wswap(&mess_len);
				retcode	= va_arg(arg_list,int4 *);			/* and retcode				*/
				ourret	= sndmsg(prname,mtext,mess_len,wait);		/* call handler for message send op	*/
				break;	
		case M_CHECK:	check_t    = *va_arg(arg_list,char*);			/* get check type			*/
				time_int_p = va_arg(arg_list,int4 *);			/* and timeout value			*/
				GETBIN(&time_int,time_int_p,sizeof(int4));		/* copy to our int 			*/
				wswap(&time_int);					/* swap if necessary			*/
				receiver   = va_arg(arg_list,char*);			/* pointer to receiving area		*/
				mess_len_p = va_arg(arg_list,int4 *); 			/* pointer to message len		*/
				arg_count -= 4;
				if (arg_count == 2) chptr = va_arg(arg_list,char*);	/* if two args left ignore first	*/
				retcode    = va_arg(arg_list,int4 *);
				ourret     = get_msg(prname,check_t,time_int,mess_len_p,receiver);	/* get the message	*/
				break;
		case M_INV:	ourret = FAILURE;
				break;
		default:	break;
	}
	PUTBIN(retcode,&ourret,sizeof(int4));						/* copy our ret code to their area*/
	wswap(retcode);									/* swap bytes if necessary	*/
}
static creport(portname)
char *portname;
{
	key_t port_key, keyval();
	char	tmpbuf[80];

	makemsgpath(portname,tmpbuf);
	if ( fexists(tmpbuf) )
	{
		return CR_PORT_BUSY;
	}

	port_key = keyval(portname,1);
	if (port_key == (key_t) -1)
	{
		return FAILURE;
	}
	msqid = msgget(port_key,IPC_EXCL|IPC_CREAT|0777);				/* get and create		*/
	if (msqid == -1)								/* system call failed		*/
	{
		if (errno==EEXIST)							/* exists already		*/
		{
			msqid = msgget(port_key,0);					/* get id for queue		*/
			/*if (msqid < 0) return CR_PORT_BUSY;*/				/* couldn't get id, not owner	*/
			/*else return CR_PORT_OURS;*/					/* must be ours			*/
			return CR_PORT_BUSY;
		}
		else
			return FAILURE;							/* generic failure		*/
	}
	else
		return SUCCESS;
}
static int delport(portname)
char *portname;
{
	key_t port_key, keyval();
	char tmpbuf[80];
	
	makemsgpath(portname,tmpbuf);
	if ( !fexists(tmpbuf) )
	{
		return DE_NO_PORT;
	}
	port_key = keyval(portname,0);							/* get key for this ipc structure	*/
	if (port_key == (key_t) -1)
	{
		return DE_NO_PORT;
	}
	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1) 
	{
		unlink(tmpbuf);
		return DE_NO_PORT;							/* queue not exist			*/
	}

	if (msgctl(msqid,IPC_STAT,&mctlbuf) == -1)					/* cannot stat, not owner		*/
	{
		unlink(tmpbuf);
		return DE_NO_PORT;
	}

	if (msgctl(msqid,IPC_RMID,0) == -1) 						/* only the owner can delete.		*/
	{
		char	newbuf[80];
		struct	stat sbuf;

		stat(tmpbuf,&sbuf);
		strcpy(newbuf,tmpbuf);							/* Rename file to inode			*/
		sprintf(newbuf,"%s/i%d",WISP_PRB_DIR,sbuf.st_ino);
		link(tmpbuf,newbuf);
	}

	unlink(tmpbuf);

	if (mctlbuf.msg_qnum) return DE_MSG_LOST;					/* deleted, but msgs on queue lost	*/
	else return SUCCESS;								/* normal deletion			*/
}
static sndmsg(prname,mtext,mlen,wait)
char 	*prname, *mtext;
int4	mlen;
int4	wait;
{
	key_t port_key, keyval();

	port_key = keyval(prname,0);
	if (port_key == (key_t) -1)
	{
		return XM_NO_PORT;
	}
	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1) 
	{
		return (errno==EACCES)?XM_PORT_PRIV:XM_NO_PORT;				/* queue not exist or not privileged	*/
	}

	memset(&msg,0,sizeof(msg));							/* zed message struct			*/
	mlen = (mlen > WMSGMAX) ? WMSGMAX : mlen;					/* Trim the length to max		*/
	memcpy(msg.mtext,mtext,mlen);							/* copy callers message into our struct	*/
	msg.mtype = (long)MSGTYPE;							/* assign arbitrary type		*/

	if (msgsnd(msqid,&msg,(size_t)mlen,wait?0:IPC_NOWAIT)<0)			/* call msg send routine		*/
	{
		if (!wait && errno==EAGAIN) 
		{
			return XM_PORT_FULL;						/* call failed, queue must be full	*/
		}
	}

	if (wait)									/* now wait for queue to empty		*/
	{
		do
		{
			sleep(1); 
			msgctl(msqid,IPC_STAT,&mctlbuf); 

		} while (mctlbuf.msg_qnum); 
	}
	return SUCCESS;
}

static get_msg(prname,check_t,time_int,mess_len,receiver)
char *prname, *receiver;
int4 check_t,time_int,*mess_len;
{
	key_t port_key, keyval();
	int wait_time, wait_key, gotmsg; 
	void timed_out(); 
	int status, sav_errno, key;
	int4 mlen, len_read;

	key = wait_time = wait_key = timeout = gotmsg = 0;
	GETBIN(&mlen,mess_len,sizeof(int4));
	port_key = keyval(prname,0);
	if (port_key == (key_t) -1)
	{
		return CH_NO_PORT;
	}
	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1) return CH_NO_PORT;						/* queue not exist			*/
	memset(&msg,0,sizeof(msg));							/* zed message struct			*/
	switch (check_t)
	{
		case 'T': 
			++wait_time;
			break;	
		case 'K': 	
			++wait_key;
			break;
		case 'B': 
			++wait_time;
			++wait_key;
			break;
		case 'W': 
		default:
			break;
	}
	if (wait_time) 
	{
		time_int /= 100;							/* figure actual seconds		*/
		signal(SIGALRM,timed_out);						/* setup to catch alarm signal		*/
		alarm(time_int+1);							/* turn on alarm			*/
	}
	if (wait_key)
	{
#ifdef OLD
		int keybd;

		keybd = open("/dev/tty",O_RDONLY|O_NDELAY);				/* open the tty 			*/
		if (keybd<0) 
		{
			werrvre("Error reading tty for keypress [%d]\n",errno);
			return FAILURE;
		}
		raw_mode(keybd);
#endif /* OLD */
		do
		{
			status = msgrcv(msqid,&msg,WMSGMAX,MSGTYPE,IPC_NOWAIT); 	/* call with nodelay on			*/
			if (key = vcheck())						/* look for keypress			*/
			{
				int	c;
				char	aid;

				vpushc(key);
				c=vgetm();

				aid = meta_aid(c);

				if (aid)
				{
					set_aid(aid);
				}
			}
			else
			{
				sleep(1);
			}
		} while (status<0 && (key==0) && !timeout);				/* loop till bytes read || key press || */
											/* time out				*/
		if (status>0) ++gotmsg;							/* status is actually bytes read	*/
#ifdef OLD
		no_raw_mode(keybd);							/* normal mode				*/
#endif /* OLD */
	}
	else
	{
		if ((status = msgrcv(msqid,&msg,WMSGMAX,MSGTYPE,0))<0) sav_errno = errno;	/* get message			*/
		else gotmsg=TRUE;							/* no timeout occurred			*/
	}
	if (wait_time)
	{
		alarm(0);								/* kill any pending alarm		*/
		signal(SIGALRM,SIG_DFL);						/* and reset sig table			*/
	}
	if (timeout && !gotmsg) return CH_PORT_TIMEOUT;
	if (key!=0 && !gotmsg) return CH_PORT_FKEYLOCK;

	if (status < mlen)
	{
		mlen = status;								/* get actual length			*/
		PUTBIN(mess_len,&mlen,sizeof(int4));					/* pass length back			*/
		wswap(mess_len);
	}

	memcpy(receiver,msg.mtext,mlen);						/* copy message to receiver		*/

	return SUCCESS;
}
static void timed_out(sig)
int sig;
{
	++timeout;
}
static key_t keyval(p,create)
char *p;
int create;
{
	key_t tmp,wftok();
	char keypath[64];
	FILE *keyfile;
		
	makemsgpath(p,keypath);
	if (!create && !fexists(keypath)) return (key_t) -1;
	
	keyfile=fopen(keypath,"w");
	if( !keyfile )
	{
		werrlog(ERRORCODE(16),"create",keypath,0,0,0,0,0,0);
		return( -1 );
	}
	fprintf(keyfile,"\n");
	fclose(keyfile);
	chmod(keypath,0666);
	tmp=wftok(keypath);
#if 0	
	PUTBIN(&tmp,p,sizeof(key_t));
#endif
	return tmp;
}
static makemsgpath(port,path)
char *port,*path;
{
	char tmpbuf[5];
	
	memcpy(tmpbuf,port,4);
	tmpbuf[4]=(char)0;
	sprintf(path,"%s/MSG_%s",WISP_PRB_DIR,tmpbuf);

	if (!fexists(WISP_PRB_DIR))
	{
		/* 
		**	If the /usr/tmp/wpparms directory doesn't exist then create it here.
		**	(This is kind of sloppy but any error will be picked up later when we go to use it.)
		*/
		mkdir(WISP_PRB_DIR,0777);
		chmod(WISP_PRB_DIR,0777);
	}
}
static functype(ptr)
char *ptr;
{
	int i;

	for ( i=0; funcs[i]; ++i)
		if (!strcmp(ptr,funcs[i])) return i;
	return M_INV;
}
static raw_mode(f)
int f;
{
	ioctl(f,TCGETA,&t_save);							/* save tty settings			*/
	ioctl(f,TCGETA,&t_new);								/* and get a copy we can muck with	*/
	t_new.c_lflag &= ~ICANON;							/* turn off canonical mode		*/
	t_new.c_cc[VMIN] = 0;								/* get '1' char at a time		*/
	t_new.c_cc[VTIME] = 0;								/* or after 0 ms (immediately)		*/
	ioctl(f,TCSETA,&t_new);								/* put message in this mode		*/
}
static no_raw_mode(f)
int f;
{
	ioctl(f,TCSETA,&t_save);							/* restore previous settings		*/
}
#endif	/* #ifdef unix	*/
#ifdef MSDOS
MESSAGE()
{
	werrlog(102,"MESSAGE: Not Yet Implemented",0,0,0,0,0,0,0);
}
#endif /* MSDOS */


