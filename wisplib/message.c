static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/



#include <stdio.h>
#include <string.h>
#include <varargs.h>


#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <termio.h>
#endif

#if defined(unix) || defined(WIN32)
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <time.h>
#endif

#ifdef WIN32
#include <signal.h>
#include <direct.h>
#include <io.h>
#include "wperson.h"
#include "win32msg.h"
#include "wispnt.h"
#include "wispcfg.h"

#define msgget win32msgget
#define msgsnd win32msgsnd
#define msgrcv win32msgrcv
#define msgctl win32msgctl
#endif


#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"
#include "vwang.h"
#include "wmalloc.h"
#include "wperson.h"
#include "movebin.h"
#include "wisplib.h"

#define		ROUTINE		35000

#define PORTNAME_SIZE   4

/* Message port buffer defn's */
#define MIN_MSG_BUFSIZE 1
#define MAX_MSG_BUFSIZE 2014
#define DEF_MSG_BUFSIZE 2014



#ifdef VMS

#include <ssdef.h>
#include <descrip.h>
#include <psldef.h>
#include <iodef.h>
#include <lnmdef.h>

#include "video.h"
#include "vlocal.h"


extern int DestroyAllPorts();								/* Declare the exit handler routine.	*/

static struct {
		int4 flink;								/* The forward link (VMS only)		*/
		char *exproc;								/* The pointer to the proc.		*/
		int4 argcnt;								/* The arg count (0)			*/
		int4 *condit;								/* Pointer to the condition value.	*/
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

/* Various field length defn's */
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

int CheckMessages( char *port_name, char check_type, int time_interval,
                  char *msg, unsigned int *msg_size, char disable_keys );

int create_message_port( char *port_name, unsigned int buf_size, char keep_flag);

void MESSAGE( va_alist )
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
			l_bufsiz = DEF_MSG_BUFSIZE;					/* Otherwise, set default.		*/
		}

		if (argc > CR_nargs + 1)						/* Also included keep flag.		*/
		{
			keep_flag = va_arg(argp, char*);
		}

		keep_flag = "N";
	        return_code = va_arg(argp, int*);
	        *return_code = create_message_port( port_name, l_bufsiz, *keep_flag);
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
	        *return_code = destroy_message_port( port_name );
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
int AddtoPortList ( char *port_name, short chan, struct dsc$descriptor_s *lognam );

int create_message_port( char *port_name, unsigned int buf_size, char keep_flag)
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

    	if (buf_size < MIN_MSG_BUFSIZE || buf_size > MAX_MSG_BUFSIZE )
        	maxmsg = DEF_MSG_BUFSIZE;
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
int destroy_message_port( char *port_name )
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
	temp = (char *)wmalloc(*msg_size+1);    					/* Setup temp buffer to hold message.	*/

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
		else if (vwang_keypressed(0))						/* See if there is a keypress in bufr.	*/
		{
			char	aid;
			
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
		else if (vwang_keypressed(0))						/* See if there is a keypress in bufr.	*/
		{
			char	aid;
			
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
		return(0);								/* bytes read				*/
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

    	newmbx = (struct mbx_list *)wmalloc(sizeof(struct mbx_list));				/* grab new mbx list struct */

/* Stuff portname, channel, and logical name into new structure */

    	newmbx->port_name = (char *)wmalloc(PORTNAME_SIZE);
    	strncpy(newmbx->port_name, port_name, PORTNAME_SIZE);
    	newmbx->chan = chan;
    	newmbx->lognam = (struct dsc$descriptor_s *)wmalloc(sizeof(struct dsc$descriptor_s));
    	newmbx->lognam->dsc$w_length = lognam->dsc$w_length;
    	newmbx->lognam->dsc$b_dtype = lognam->dsc$b_dtype;
    	newmbx->lognam->dsc$b_class = lognam->dsc$b_class;
    	newmbx->lognam->dsc$a_pointer = (char *)wmalloc(lognam->dsc$w_length);
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

#if defined(unix) || defined(WIN32)


#define M_CREATE 0
#define M_DELETE 1
#define M_XMIT  2
#define M_XMTWT 3
#define M_CHECK 4
#define M_INV -1

#define SUCCESS 	0
#define FAILURE 	64

#define CR_PORT_BUSY 	4
#define CR_PORT_OURS 	8
#define CR_NO_SPACE	24

#define DE_MSG_LOST 	4
#define DE_NO_PORT 	8

#define XM_NO_PORT 	4
#define XM_PORT_FULL 	8
#define XM_PORT_PRIV 	12

#define CH_PORT_TIMEOUT 8
#define CH_NO_PORT 	16
#define CH_PORT_FKEYLOCK 12

#define MSGTYPE 1L

static int timeout;

static struct {
	long mtype;
	char mtext[MAX_MSG_BUFSIZE];
} msg;
static struct msqid_ds mctlbuf;

static int create_message_port(char port_name[PORTNAME_SIZE], int4 buf_size, char keep_flag);
static int destroy_message_port(char port_name[PORTNAME_SIZE]);
static int transmit_message(char portname[PORTNAME_SIZE], char* mtext, int4 mlen, int4 wait);
static int check_message(char port_name[PORTNAME_SIZE],
			 char check_t,
			 int4 time_int,
			 int4 *mess_len,
			 char *receiver);

static key_t keyval(char p[PORTNAME_SIZE], int create);
static void makemsgkeyfile(char port[PORTNAME_SIZE], char* path);
static int portownergid(char port[PORTNAME_SIZE]);
static int functype(char* func);

static void signal_timed_out(int sig);

extern void WSXIO(char*, ...);

struct port_s
{
	struct port_s	*next;
	char		port_name[5];
};
static struct port_s	*port_list = NULL;

/*
	MESSAGE	("CR", Port, [Bufsize, [Keep,]] Retcode)
		("DE", Port, Retcode)
		("XM", Port, Message, Messlen, Retcode)
		("XW", Port, Message, Messlen, Retcode)
		("CH", Port, Chtype, Time, Message, Messlen, [Nohelp,] Retcode)

	Func	Alpha(2)	
			"CR"	Create message port
			"DE"	Destroy message port
			"XM"	Transmit a message
			"XW"	Transmit a message (wait if port full)
			"CH"	Check for message

	Port	Alpha(4)
			Port name

	Bufsize	Int(4)
			Max bytes port can hold until checked

	Keep	Alpha(1)
			Keep port on unlink
			"Y"	Yes
			"N"	No
			" "	Default (No)

	Message	Alpha(var)
	MessLen	Int(4)
	Chtype	Alpha(1)
			Type of check
			"W"	Wait until message received
			"T"	Wait until message received or time-out
			"K"	Wait until message received or PFkey pressed
			"B"	Wait until message received or time-out or PFkey pressed

	Time	Int(4)
			Time to wait in hundreds of seconds

	Nohelp	Alpha(1)
			Disable help
	Retcode	Int(4)
			Return code
			"CR"
				0	port created
				4	Another task is using this port
				8	This task is using this port
			"DE"
				0	Port destroyed
				4	Port destroyed (messages lost)
				8	No such port was created by this task
			"XM", "XW"
				0	Message queued
				4	Port not created
				8	Buffer full ("XM" only)
				12	Port is privileged
			"CH"
				0	Message received
				8	Time-out
				12	Keyboard locked
				16	No such port was created by this task
*/

void MESSAGE( va_alist )
va_dcl
{
	va_list arg_list;
	int 	arg_count;
	char 	*chptr;
	char 	funcstr[3], portname[5];
	int4 	wait, ourret, *retcode;
	int	i=0;

	wait = 0;

	va_start(arg_list);
	arg_count = va_count(arg_list);
	va_start(arg_list);

	/*
	**	Get the Function
	*/
	chptr = va_arg(arg_list,char *);						/* get function and portname		*/
	funcstr[0] = chptr[0];
	funcstr[1] = chptr[1];
	funcstr[2] = (char)0;
	arg_count -= 1;

	/*
	**	Get the port name and correct any unusable characters
	*/
	chptr = va_arg(arg_list,char *);
	memcpy(portname, chptr, PORTNAME_SIZE);
	portname[PORTNAME_SIZE] = (char)0;
	arg_count -= 1;

#ifdef unix
	for(i=0; i<PORTNAME_SIZE; i++)
	{
		if (' ' == chptr[i] || '/' == chptr[i] || (char)0 == chptr[i])
		{
			/*
			**	SPACE, NULL and SLASH are invalid characters for a portname, 
			**	replace with '#'.
			*/
			portname[i] = '#';
		}
	}
#endif

	wtrace("MESSAGE","ENTRY","Function=[%2.2s] Port=[%4.4s]", funcstr, portname);

	switch(functype(funcstr))
	{
	case M_CREATE:
		{
			int4	l_bufsize;
			char	l_keep;
			int4	*buffer_size;

			l_bufsize = DEF_MSG_BUFSIZE;
			l_keep = 'N';

			if (arg_count > 1)
			{
				buffer_size = va_arg(arg_list, int4 *);
				arg_count -= 1;
				l_bufsize = get_swap(buffer_size);

				if (l_bufsize < MIN_MSG_BUFSIZE || l_bufsize >MAX_MSG_BUFSIZE)
				{
					l_bufsize = DEF_MSG_BUFSIZE;
				}
			}

			if (arg_count > 1)
			{
				l_keep = *(va_arg(arg_list, char *));
				arg_count -= 1;

				if (l_keep != 'Y')
				{
					l_keep = 'N';
				}
			}

			retcode	= va_arg(arg_list,int4 *);
			ourret	= create_message_port(portname,l_bufsize,l_keep);
		}
		break;

	case M_DELETE:
		retcode	= va_arg(arg_list,int4 *);				/* get retcode pointer			*/
		ourret	= destroy_message_port(portname);			/* dispatch to handler			*/
		break;

	case M_XMTWT:
		wait = TRUE;							/* set wait flag, then do normal xmit	*/
	case M_XMIT:
		{
			char 	*mtext;
			int4 	mess_len, *mess_len_p;

			mtext = va_arg(arg_list,char *); 			/* get text pointer			*/
			mess_len_p = va_arg(arg_list,int4 *);			/* and length				*/
			mess_len = get_swap(mess_len_p);
			retcode	= va_arg(arg_list,int4 *);			/* and retcode				*/
			ourret = transmit_message(portname,mtext,mess_len,wait);/* call handler for message send op	*/
		}
		break;	

	case M_CHECK:
		{
			char	check_t;
			char 	*receiver;
			int4 	time_int, *time_int_p;
			int4 	mess_len, *mess_len_p;

			check_t    = *va_arg(arg_list,char*);			/* get check type			*/
			time_int_p = va_arg(arg_list,int4 *);			/* and timeout value			*/
			time_int   = get_swap(time_int_p);
			receiver   = va_arg(arg_list,char*);			/* pointer to receiving area		*/
			mess_len_p = va_arg(arg_list,int4 *); 			/* pointer to message len		*/
			mess_len   = get_swap(mess_len_p);
			arg_count -= 4;
			if (arg_count == 2) chptr = va_arg(arg_list,char*);	/* if two args left ignore first	*/
			retcode    = va_arg(arg_list,int4 *);
			ourret     = check_message(portname,check_t,time_int,&mess_len,receiver);	/* get the message	*/

			if (SUCCESS == ourret)
			{
				PUTBIN(mess_len_p,&mess_len,sizeof(int4))
				wswap(mess_len_p);
			}
		}
		break;

	case M_INV:
		ourret = FAILURE;
		break;

	default:
		break;
	}

	PUTBIN(retcode,&ourret,sizeof(int4));						/* copy our ret code to their area*/
	wswap(retcode);									/* swap bytes if necessary	*/
}

static int create_message_port(char port_name[PORTNAME_SIZE], int4 buf_size, char keep_flag)
{
	key_t 	port_key;
	char	keyfilepath[256];
	int    	msqid;

try_again:
	makemsgkeyfile(port_name,keyfilepath);
	if ( fexists(keyfilepath) )
	{
		if (wgetpgrp() == portownergid(port_name))
		{
			return CR_PORT_OURS;
		}
		else
		{
			return CR_PORT_BUSY;
		}
	}

	port_key = keyval(port_name,1);
	if (port_key == (key_t) -1)
	{
		unlink(keyfilepath);
		return FAILURE;
	}
	msqid = msgget(port_key,IPC_EXCL|IPC_CREAT|0777);				/* get and create		*/
	if (msqid == -1)								/* system call failed		*/
	{
		if (EEXIST==errno)
		{
			/*
			**	We hit a dangling message queue. 
			**	Don't mess with it!
			**	Just rename the keyfile so no one else uses this inode,
			**	then we try again (with a new inode/key).
			*/

#ifdef unix			
			char	newbuf[80];
			struct	stat sbuf;

			stat(keyfilepath,&sbuf);
			sprintf(newbuf,"%s/i%d",wispprbdir(NULL),sbuf.st_ino);
			if (-1 == link(keyfilepath,newbuf))
			{
				return CR_PORT_BUSY;
			}
			if (-1 == unlink(keyfilepath))
			{
				return CR_PORT_BUSY;
			}
#endif
			goto try_again;
		}
		else if (ENOSPC==errno)
		{
			unlink(keyfilepath);
			return CR_NO_SPACE;
		}
		else
		{
			unlink(keyfilepath);
			return FAILURE;
		}
	}

	if ('Y' != keep_flag)
	{
		/*
		**	Add the port to the list of ports to be deleted.
		*/
		struct port_s	*port_ptr;

		port_ptr = (struct port_s *)wmalloc(sizeof(struct port_s));
		port_ptr->next = port_list;
		memcpy(port_ptr->port_name,port_name,5);
		port_list = port_ptr;
	}

	return SUCCESS;
}

static int destroy_message_port(char port_name[PORTNAME_SIZE])
{
	key_t 	port_key;
	char 	keyfilepath[256];
	int 	msqid;
	
	makemsgkeyfile(port_name,keyfilepath);
	if ( !fexists(keyfilepath) )
	{
		return DE_NO_PORT;
	}
#ifdef unix	
	else if (woperator())
	{
		/* Allow operator to delete port */
	}
#endif	
	else if (wgetpgrp() != portownergid(port_name))
	{
		return DE_NO_PORT;
	}

	{
		/*
		**	Remove this port from the delete list.
		*/
		struct port_s	*port_ptr;

		port_ptr = port_list;
		while(port_ptr)
		{
			if (0==strcmp(port_ptr->port_name,port_name))
			{
				/*
				**	Found port in the list.
				**	"Remove" it by nulling the name.
				**	We don't muck with the list as message_unlink() may be doing this.
				*/
				port_ptr->port_name[0] = (char)0;
				break;
			}
			port_ptr = port_ptr->next;
		}
	}

	port_key = keyval(port_name,0);							/* get key for this ipc structure	*/
	if (port_key == (key_t) -1)
	{
		/*
		**	There is a problem, but if we can delete the keyfile
		**	assume were OK else report failure.
		*/
		if (0==unlink(keyfilepath)) return SUCCESS;
		return FAILURE;
	}

	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1) 
	{
		/*
		**	There is a problem, but if we can delete the keyfile
		**	assume were OK else report failure.
		*/
		if (0==unlink(keyfilepath)) return SUCCESS;
		return FAILURE;								/* queue not exist			*/
	}

	if (msgctl(msqid,IPC_STAT,&mctlbuf) == -1)					/* cannot stat, not owner		*/
	{
		/*
		**	There is a problem, but if we can delete the keyfile
		**	assume were OK else report failure.
		*/
		if (0==unlink(keyfilepath)) return SUCCESS;
		return FAILURE;
	}

	if (msgctl(msqid,IPC_RMID,0) == -1) 						/* only the owner can delete.		*/
	{
#ifdef unix		
		char	newbuf[80];
		struct	stat sbuf;

		/*
		**	We couldn't remove the message queue so we will 
		**	rename the keyfile (same inode) so no one else
		**	will run into a dangling message queue.
		*/
		stat(keyfilepath,&sbuf);
		sprintf(newbuf,"%s/i%d",wispprbdir(NULL),sbuf.st_ino);
		link(keyfilepath,newbuf);
#endif
	}

	if (0!=unlink(keyfilepath)) return FAILURE;
	if (mctlbuf.msg_qnum) return DE_MSG_LOST;					/* deleted, but msgs on queue lost	*/
	else return SUCCESS;								/* normal deletion			*/
}

static int transmit_message(char portname[PORTNAME_SIZE], char* mtext, int4 mlen, int4 wait)
{
	key_t 	port_key;
	int 	msqid;

	port_key = keyval(portname,0);
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
	mlen = (mlen > MAX_MSG_BUFSIZE) ? MAX_MSG_BUFSIZE : mlen;			/* Trim the length to max		*/
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
#ifdef unix
			sleep(1);
#endif
#ifdef WIN32
			hsleep(100);
#endif
			msgctl(msqid,IPC_STAT,&mctlbuf); 

		} while (mctlbuf.msg_qnum); 
	}
	return SUCCESS;
}

static int check_message(char port_name[PORTNAME_SIZE],
			 char check_t,
			 int4 time_int,
			 int4 *mess_len,
			 char *receiver)
{
	key_t 	port_key;
	int 	wait_time, wait_key, gotmsg, gotlen, gotkey;
	int	rc;
	int4	secs_to_wait;
	char	keyfilepath[256];
	int msqid;

	timeout = gotkey = gotmsg = 0;

	makemsgkeyfile(port_name,keyfilepath);
	if ( !fexists(keyfilepath) )
	{
		/*
		**	Port doesn't exist.
		*/
		return CH_NO_PORT;
	}
	else if (wgetpgrp() != portownergid(port_name))
	{
		/*
		**	Port exists but not created by this task.
		*/
		return CH_NO_PORT;
	}

	port_key = keyval(port_name,0);
	if (port_key == (key_t) -1)
	{
		/*
		**	Port is corrupted.
		*/
		unlink(keyfilepath);
		return FAILURE;
	}

	msqid = msgget(port_key,0);							/* get identifier for queue		*/
	if (msqid == -1)
	{
		/*
		**	Associated message queue not found.
		**	Port is corrupted.
		*/
		unlink(keyfilepath);
		return FAILURE;								/* queue not exist			*/
	}

	memset(&msg,0,sizeof(msg));							/* zed message struct			*/

	switch (check_t)
	{
	case 'T': 
		wait_time = 1;
		wait_key = 0;
		break;	
	case 'K': 	
		wait_time = 0;
		wait_key = 1;
		break;
	case 'B': 
		wait_time = 1;
		wait_key = 1;
		break;
	case 'W': 
	default:
		wait_time = 0;
		wait_key = 0;
		break;
	}

	/*
	**	Keyboard interaction is not supported with native screens
	*/
	if (wait_key && nativescreens())
	{
		char	msg[80];

		sprintf(msg, "%%MESSAGE-F-NATIVE MESSAGE check type '%c' not supported with Native Screens", check_t);
		werrlog(104,msg,0,0,0,0,0,0,0);
		wait_key = 0;
		wait_time = 1;
		time_int = 0;
	}

	if (wait_time) 
	{
		/*
		**	Calc the number of seconds to wait. 
		**	If less the 1/2 then don't wait.
		*/
		if (time_int < 50)
		{
			secs_to_wait = 0;
			timeout = 1;
		}
		else
		{
			/*
			**	Round up to a minimum of 1 sec.
			*/
			secs_to_wait = (time_int+50) / 100;
		}
	}

	if (wait_key)
	{
		/*
		**	Loop until a message received
		**		or a aid-key is pressed
		**		[or a timeout occurs]
		*/

		time_t	curr_time, stop_time;

		if (wait_time)
		{
			/*
			**	Calculate the stop time for a timeout.
			*/
			time(&curr_time);
			stop_time = curr_time + secs_to_wait;
		}

		for(;;)
		{
			/*
			**	Check for a message (nodelay)
			*/
			rc = msgrcv(msqid,&msg,MAX_MSG_BUFSIZE,MSGTYPE,IPC_NOWAIT); 	/* call with nodelay on			*/

			if (rc >= 0)
			{
				/*
				**	Got a message
				*/
				gotlen = rc;
				gotmsg = 1;
				timeout = 0;
				break;
			}

			if (wait_time)
			{
				time(&curr_time);
				if (curr_time >= stop_time)
				{
					/*
					**	A timeout occurred
					*/
					timeout = 1;
					break;
				}
			}

			/*
			**	We are waiting for a AID key to be pressed.
			**	The workstation MUST be unlocked.
			**	This WSXIO/WAIT call will do a 1 sec timed read.
			**	This will allow the user to also enter regular data on the screen.
			*/
			{
				int4	one_sec = 100;
				int4	four_args = 4;
				char	iosw[8];

				wswap(&one_sec);
				memset(iosw,' ',8);

				wvaset(&four_args);
				WSXIO("W", 0, &one_sec, iosw);

				if (0 != memcmp(iosw,"        ",8))
				{
					/*
					**	A AID key was pressed
					*/
					gotkey = 1;
					break;
				}
			}
		} 
	}
	else if (wait_time && 0==secs_to_wait)
	{
		/*
		**	Check for a message with nowait
		*/
		rc = msgrcv(msqid,&msg,MAX_MSG_BUFSIZE,MSGTYPE,IPC_NOWAIT);
		if (rc >= 0)
		{
			gotlen = rc;
			gotmsg = 1;
			timeout = 0;
		}
	}
	else
	{
		/*
		**	Check for a message and wait for it to arrive.
		**	(may be interrupted by a timeout)
		*/

		if (wait_time && secs_to_wait)
		{
#ifdef unix
			signal(SIGALRM,signal_timed_out);				/* setup to catch alarm signal		*/
			alarm(secs_to_wait);						/* turn on alarm			*/
#endif
#ifdef WIN32
			win32msgtimeout(secs_to_wait);
#endif
		}

		rc = msgrcv(msqid,&msg,MAX_MSG_BUFSIZE,MSGTYPE,0);
		if (rc >= 0)
		{
			gotlen = rc;
			gotmsg = 1;
			timeout = 0;
		}
#ifdef WIN32
		/*
		** WIN32 does not support the SIGALRM, so we can't rely on the sighandler
		** to set this flag for us.  However a -1 return from msgrcv on a timed read
		** means the read timed out (or an actual fatal error occurred, in which
		** case it probably no longer matters if we misinterperet it as a timeout)
		*/
		if (rc == -1 )
		{
			timeout = 1;
		}
#endif

		if (wait_time && secs_to_wait)
		{
#ifdef unix			
			alarm(0);							/* kill any pending alarm		*/
			signal(SIGALRM,SIG_DFL);					/* and reset sig table			*/
#endif			
		}
	}

	if (gotmsg)
	{
		if (gotlen < *mess_len)
		{
			*mess_len = gotlen;						/* get actual length			*/
		}

		memcpy(receiver,msg.mtext,*mess_len);					/* copy message to receiver		*/

		return SUCCESS;
	}
	else if (gotkey) 
	{
		return CH_PORT_FKEYLOCK;
	}
	else if (timeout) 
	{
		return CH_PORT_TIMEOUT;
	}

	return FAILURE;
}

/*
**	Routine:	message_unlink()
**
**	Function:	Delete all the message ports created by this task.
**
**	Description:	Step thru the port_list and delete each port and free the list.
**
**	Arguments:	None
**
**	Globals:
**	port_list	The pointer to the list of ports to be deleted.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	11/30/93	Written by GSL
**
*/
int message_unlink(void)
{
	struct port_s	*port_ptr;

	while(port_list)
	{
		port_ptr = port_list;
		if (port_ptr->port_name[0])
		{
			char	port_name[PORTNAME_SIZE+1];

			/*
			**	Move port_name to a tmp variable because destroy_message_port
			**	will manipulate the port_list, and null the port_name.
			*/
			memcpy(port_name,port_ptr->port_name,PORTNAME_SIZE+1);
			destroy_message_port(port_name);
		}
		port_list = port_ptr->next;
		free(port_ptr);
	}
	return 0;
}

#ifdef unix
static void signal_timed_out(int sig)
{
	timeout=1;
}
#endif

static key_t keyval(char p[PORTNAME_SIZE], int create)
{
	key_t 	wftok();
	char 	keypath[256];
	FILE 	*keyfile;
#ifdef WIN32
	int4   thekey;
#endif
		
	makemsgkeyfile(p,keypath);
	if (!create && !fexists(keypath)) return (key_t) -1;
	
	if (create)
	{
		keyfile=fopen(keypath,"w");
		if( !keyfile )
		{
			werrlog(ERRORCODE(16),"create",keypath,0,0,0,0,0,0);
			return( -1 );
		}
		fprintf(keyfile,"%d\n",wgetpgrp());
		fclose(keyfile);
		chmod(keypath,0666);
	}
#ifdef unix
	return wftok(keypath);
#endif
#ifdef WIN32
	memcpy((void *)&thekey,(void *)p,sizeof(thekey));
	return (key_t)thekey;
#endif
}

static void makemsgkeyfile(char port[PORTNAME_SIZE], char* path)
{
#ifdef unix
	const char *msgdir = wispprbdir(NULL);
	char tmpbuf[PORTNAME_SIZE+1];

	memcpy(tmpbuf,port,PORTNAME_SIZE);
	tmpbuf[PORTNAME_SIZE]=(char)0;
	sprintf(path,"%s/MSG_%s",msgdir,tmpbuf);
#endif	
#ifdef WIN32
	const char *msgdir = wispmsgsharedir(NULL);
	long portval;

	memcpy(&portval, port, PORTNAME_SIZE);
	sprintf(path,"%s\\M_%08X.mkey",msgdir,portval);
#endif
	if (!fexists(msgdir))
	{
		/* 
		**	If the PRB directory doesn't exist then create it here.
		**	(This is kind of sloppy but any error will be picked up later when we go to use it.)
		*/
		mkdir(msgdir,0777);
		chmod(msgdir,0777);
	}
}

/*
**	Routine:	portownergid()
**
**	Function:	Get the gid of the owner of the port.
**
**	Description:	This routine reads the PORT key file which contains
**			the GID of the owner of the port and returns it.
**
**	Arguments:
**	port		The 4 char port name.
**
**	Globals:	None
**
**	Return:		The GID from the port key file (Owner's gid) or
**			-1 if an error.
**
**	Warnings:	None
**
**	History:	
**	11/30/93	Written by GSL
**
*/
static int portownergid(char port[PORTNAME_SIZE])
{
	char	path[256];
	FILE	*keyfile;
	int	gid;

	makemsgkeyfile(port,path);

	gid = -1;

	if ( fexists(path) )
	{
		keyfile=fopen(path,"r");
		if( !keyfile )
		{
			return( -1 );
		}
		fscanf(keyfile,"%d\n",&gid);
		fclose(keyfile);
		return gid;
	}
	return -1;
}

static int functype(char* func)
{
	if      (0==strcmp(func,"CR")) return M_CREATE;
	else if (0==strcmp(func,"DE")) return M_DELETE;
	else if (0==strcmp(func,"XM")) return M_XMIT;
	else if (0==strcmp(func,"XW")) return M_XMTWT;
	else if (0==strcmp(func,"CH")) return M_CHECK;
	else			       return M_INV;
}
#endif	/* #ifdef unix	*/

#if defined(MSDOS) 
void MESSAGE()
{
	werrlog(102,"MESSAGE: Not Yet Implemented",0,0,0,0,0,0,0);
}
#endif /* MSDOS */


/*
**	History:
**	$Log: message.c,v $
**	Revision 1.24  1997-12-04 18:12:59-05  gsl
**	changed to wispnt.h
**
**	Revision 1.23  1997-10-17 17:00:10-04  gsl
**	Add nativescreens() logic
**	Replace the GETBIN()s with get_swap()
**
**	Revision 1.22  1997-05-21 08:30:03-04  gsl
**	fix a warning message
**
**	Revision 1.21  1997-05-19 13:58:21-04  gsl
**	Fix for WIN32
**	Prototyped all the routines.
**	Add wtrace() call
**	Fix to use #defines instead of hardcoded numbers
**	Fix the creation of the key file for WIN32
**
**	Revision 1.20  1997-02-17 10:40:12-05  gsl
**	Fix initialization of const var msgdir
**
**	Revision 1.19  1996-11-11 17:48:14-05  jockc
**	a few small changes for message.c
**
**	Revision 1.18  1996-11-11 09:04:25-08  jockc
**	changed WIN32 to use wispmsgsharedir() for message file dir
**
**	Revision 1.17  1996-11-04 17:08:25-08  gsl
**	Fix the order of the include files and move them all
**	to the top of the file
**
**	Revision 1.16  1996-10-30 15:11:40-08  jockc
**	change mkdir call back to unix style.. redef is in win32std.h
**
**	Revision 1.15  1996-10-15 16:12:10-07  jockc
**	support for windows 32.. reused as much existing code as
**	possible by emulating the unix message queues with win32msg.c ..
**
**	Revision 1.14  1996-08-23 14:02:41-07  gsl
**	Changed to use wispprbdir()
**
**	Revision 1.13  1996-08-19 15:32:31-07  gsl
**	drcs update
**
**
**
*/
