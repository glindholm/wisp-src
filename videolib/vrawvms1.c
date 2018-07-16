#ifdef VMS
/********************************************************************************************************************************
*																*
*			VRAWVMS1.C   C- version of vax i/o's for VIDEO.								*
*																*
*********************************************************************************************************************************/

#include <trmdef.h>
#include <ssdef.h>
#include <descrip.h>
#include <iodef.h>
#include <msgdef.h>
#include <stdio.h>
#include <dvidef.h>

struct vms_iosb
{
	unsigned short status;
	unsigned short len;
	unsigned long pid;
};

extern int exit_proc();

static struct {
		long int flink;								/* The forward link (VMS only)		*/
		int *exproc;								/* The pointer to the proc.		*/
		long int argcnt;							/* The arg count (0)			*/
		long int *condit;							/* Pointer to the condition value.	*/
	      } exit_handler;

static long int conval;									/* A place to hold the condition value	*/

extern unsigned char check_buff();

static unsigned long	inited = 0;							/* Flag to indicate I/O is inited.	*/
static unsigned long	inputting = 0;							/* Flag to indicate input is on.	*/
static unsigned long	ctrl_c_flag = 0;						/* Flag if a control-c was done.	*/

static unsigned short	vmbx_chan = 0;							/* The i/o chan for the input mbx.	*/
       unsigned long	vmbx_ef = 0;							/* The event flag for the mailbox.	*/
       struct vms_iosb	vmbx_iosb;							/* The iosb for the mailbox.		*/
       unsigned short	vinput_chan = 0;						/* The i/o chan for the terminal.	*/
       unsigned long	vinput_ef = 0;							/* The event flag for the terminal.	*/
static struct vms_iosb	vinput_iosb;							/* The iosb for the terminal.		*/
#pragma nostandard
globaldef unsigned short VINPUT_CHAN_NUM;						/* Used by old macro routine.		*/
#pragma standard

static struct
{
	unsigned short	x0;
	unsigned short desc0;
	unsigned long mod0;
	unsigned long done0;
	unsigned short	x1;
	unsigned short desc1;
	unsigned long mod1;
	unsigned long done1;
	unsigned short x2;
	unsigned short desc2;
	unsigned long mod2;
	unsigned long done2;
} input_itemlist = {	0,
			TRM$_MODIFIERS,
			TRM$M_TM_NOECHO | TRM$M_TM_NOFILTR | TRM$M_TM_NOEDIT | TRM$M_TM_NORECALL | TRM$M_TM_TRMNOECHO,
			0,
			0,
			TRM$_TERM,
			0,
			0,
			0,
			TRM$_TIMEOUT,
			0,
			0 };

static unsigned long itemlist_len = 24;							/* Length of itemlist, minus timer stuff*/

static struct
{
	unsigned short 	type;
	unsigned short 	unit;
	unsigned char	dnam_length;
	unsigned char	cname[15];
	unsigned short	msg_len;
	unsigned char text[512];
} mbx_message;

static unsigned long	mbx_msg_size;
static unsigned char	vinput_buf[1024];						/* The input buffer.			*/
static unsigned long	buf_in;								/* Where the next input will go.	*/
static unsigned long	buf_out;							/* Where the next character comes from.	*/
static unsigned long	read_len;							/* How many to read.			*/

#pragma nostandard
$DESCRIPTOR(chan_desc,"SYS$COMMAND");
#pragma standard

vkbtimer(wait_time)									/* Set's the wait time between chars.	*/
unsigned long wait_time;
{
	input_itemlist.mod2 = wait_time;						/* Set the timer wait period.		*/
}

unsigned char vrawinput()								/* Get a character. wait if needed.	*/
{
	unsigned long status;
	unsigned char ch;

	if (!inputting) start_input();							/* Start it up first.			*/

	if (ch = check_buff()) return(ch);						/* Check the buffer first.		*/

	do										/* Repeat till we get a buffer.		*/
	{
		if (check_mbx() == 1)							/* Make sure the mailbox is ok.		*/
		{									/* If there is already data in the inp.	*/
			read_len = 1024;						/* Try to read it all.			*/
			input_itemlist.mod0 |= TRM$M_TM_TIMED;				/* Do a timed read to get it all.	*/
			itemlist_len = 24;						/* Don't use timer value.		*/
		}
		else
		{
			read_len = 1;							/* Buffer is dead.			*/
			if (input_itemlist.mod2)					/* If they want a timer, use it.	*/
			{
				input_itemlist.mod0 |= TRM$M_TM_TIMED;			/* Do a timed read.			*/
				itemlist_len = 36;					/* Use timer value.			*/
			}
			else
			{
				input_itemlist.mod0 &= ~TRM$M_TM_TIMED;			/* Do a non timed read to get just one	*/
				itemlist_len = 24;					/* Don't use timer value.		*/
			}
		}
	} while(!get_vbuf());								/* Get the buffer now.			*/

	return(vinput_buf[buf_out++]);							/* And return the character.		*/
}


unsigned char vrawcheck()								/* Get a character, don't wait if none.	*/
{
	unsigned long status;
	int i;
	unsigned char ch;

	if (!inputting) start_input();

	if (ch = check_buff()) return(ch);						/* Check the buffer first.		*/

	if (check_mbx() == 0)								/* Make sure the mailbox is ok.		*/
	{										/* If there is already data in the inp.	*/
		return(0);								/* None, return a zero.			*/
	}
	else
	{
		read_len = 1024;							/* Try to read it all.			*/
		input_itemlist.mod0 |= TRM$M_TM_TIMED;					/* Do a timed read to get it all.	*/
		itemlist_len = 24;							/* Don't use timer value.		*/
	}
											/* Now do the qio to see what is there.	*/
											/* wait for it to finish.		*/
	get_vbuf();

	return(vinput_buf[buf_out++]);
}



static char mbx_lname[80];

#pragma nostandard
static $DESCRIPTOR(mbx_ldesc,mbx_lname);
#pragma standard

static int start_input()								/* Start up the input stuff.		*/
{
	unsigned long status;

	unsigned long i;

	if (!inited)									/* Do the initialization.		*/
	{
		buf_in = buf_out = 0;							/* Clear buffer pointers.		*/
#pragma nostandard
		exit_handler.exproc = exit_proc;					/* Install the exit handler on VMS.	*/
#pragma standard
		exit_handler.argcnt = 0;
		exit_handler.condit = &conval;
		status = sys$dclexh(&exit_handler);
		if (status != SS$_NORMAL) vraw_err("VRAW -- Error installing exit handler. status is %x hex\n",status);

		i = getpid();								/* Get the process id.			*/

		sprintf(mbx_lname,"VIDEO_MBX$%08x",i);					/* Now make a logical for i/o's		*/
		mbx_ldesc.dsc$w_length = strlen(mbx_lname);				/* Set the descriptor.			*/

		status = sys$crembx(0,&vmbx_chan,512L,512L,0x0FF00L,0L,&mbx_ldesc);	/* Get a mailbox.			*/

		if (status != SS$_NORMAL)
		{
			vraw_err("VRAW - Error assigning mailbox to terminal. error code is %x (hex).\n",status);
			return(0);
		}

		status = lib$get_ef(&vmbx_ef);						/* Get an event flag for the mailbox.	*/
		status = lib$get_ef(&vinput_ef);					/* Get an event flag for the terminal.	*/

		inited = 1;
	}

	status = sys$assign(&chan_desc,&vinput_chan,0L,&mbx_ldesc);			/* Now get a terminal channel.		*/
	if (status != SS$_NORMAL)
	{
		vraw_err("VRAW - Error assigning channel to terminal. error code is %x (hex).\n",status);
		exit(0);
	}

	inputting = 1;									/* Flag we are inputting now.		*/

	set_ctrl_c();									/* Trap control-c's			*/

	VINPUT_CHAN_NUM = vinput_chan;
	status = sys$clref(vmbx_ef);							/* Clear the mailbox ef.		*/
	start_mbx();									/* This will start it up.		*/
}



static int check_mbx()									/* Check the mailbox for the terminal.	*/
{											/* If it's got something in it, see if	*/
	unsigned long status;								/* it's a message that there's some data*/
	unsigned long ef_cluster;							/* in the buffer. If so, restart the mbx*/
											/* And return a 1, if no message, return*/
											/* A zero.				*/

	if ((sys$readef(vmbx_ef,&ef_cluster) == SS$_WASSET) && vmbx_iosb.status)	/* If the event flag is on, and there's	*/
	{										/* something in the status field.	*/
		if (vmbx_iosb.status == MSG$_TRMUNSOLIC)				/* Is there unsolicited data?		*/
		{
			start_mbx();							/* Restart the mailbox.			*/
			return(1);							/* Tell the caller.			*/
		}
		else
		{
			vraw_err("CHK_MBX -- Mailbox code was %d\n",vmbx_iosb.status);
			start_mbx();							/* Nothing for us, restart.		*/
			return(0);							/* Tell the caller.			*/
		}
	}
	else
	{
		return(0);								/* Nothing happening on the term.	*/
	}
}


static int start_mbx()
{
	unsigned long status;

	mbx_msg_size = 512;
	status = sys$qio(vmbx_ef, vmbx_chan, IO$_READVBLK, &vmbx_iosb, 0, 0, &mbx_message, mbx_msg_size, 0, 0, 0, 0);
	if (status != SS$_NORMAL)
	{
		vraw_err("START_MBX - Error starting terminal mailbox qio, error code is %x (hex).\n",status);
		return(0);
	}
	return(1);
}


static unsigned char check_buff()
{
	unsigned char ch;

	if (ctrl_c_flag)								/* Was there a control c recently?	*/
	{
		ctrl_c_flag = 0;							/* Yes, clear the flag.			*/
		buf_out = buf_in = 0;							/* Zap the input buffer.		*/
		return(3);								/* Return the control c.		*/
	}

	if (buf_out >= buf_in)								/* Nothing in the buffer?		*/
	{
		buf_out = buf_in = 0;
		return(0);
	}

	ch = vinput_buf[buf_out++];							/* Yes, get it.				*/
	if (buf_out >= buf_in) buf_in = buf_out = 0;					/* Buffer is done.			*/

	return(ch);									/* return the char.			*/
}


static int get_vbuf()									/* Get a buffer.			*/
{											/* Either because there is some already	*/
	unsigned long status;								/* in the type-ahead, or wait for one.	*/

	status = sys$qiow(vinput_ef,							/* Event flag to use.			*/
			vinput_chan,							/* I/O channel.				*/
			IO$_READVBLK | IO$M_EXTEND,					/* Read flags.				*/
			&vinput_iosb,							/* IOSB.				*/
			0,								/* AST stuff.				*/
			0,								/* ...					*/
			vinput_buf,							/* Buffer to read into.			*/
			read_len,							/* Length of the buffer.		*/
			0,								/* Timer.				*/
			0,
			&input_itemlist,						/* Itemlist.				*/
			itemlist_len);							/* Length of itemlist.			*/

	buf_out = 0;									/* Reset buffer output pointer.		*/

	if (vinput_iosb.status == SS$_TIMEOUT)						/* The timer ran out.			*/
	{
		if (itemlist_len == 36)							/* It was the users timer, signal them.	*/
			vinput_buf[vinput_iosb.len++] = 0;				/* Add a zero to the buffer.		*/
	}										/* Otherwise, just proceed.		*/
	else if (vinput_iosb.status == SS$_PARTESCAPE)					/* Allowed errors, just return to callr	*/
	{
		printf("Partial escape error detected, terminal should be set to /NOESCAPE \n");
	}
/*	Remove the following error check - unexplained inconsequential error messages at client sites.  08/03/90		*/
/*	else if ((vinput_iosb.len == 0) && !ctrl_c_flag)				   Got no input, not control-c		*/
/*	{										   And wasn't a timeout. ERROR!		*/
/*		vraw_err("GET_VBUF - Buffer size is 0, status %x, iostat %x, pid %x\n",status,vinput_iosb.status,vinput_iosb.pid);*/
/*		if (input_itemlist.mod0 & TRM$M_TM_TIMED)									*/
/*		{														*/
/*			vraw_err("TIMER was on\n");										*/
/*		}														*/
/*	}															*/

	buf_in = vinput_iosb.len;							/* Got something, set up buffering.	*/

	if (status != SS$_NORMAL && status != SS$_TIMEOUT)
	{
		vraw_err("GET_VBUF - Error reading terminal. error code is %x (hex).\n",status);
		return(0);
	}

	if (ctrl_c_flag)								/* Did a control c come in?		*/
	{
		ctrl_c_flag = 0;							/* Clear the flag			*/
		buf_in = 1;
		buf_out = 0;
		vinput_buf[0] = 3;							/* Put it in the buffer.		*/
	}

	return(vinput_iosb.len);							/* Return number of chars read.		*/
}


static int exit_proc()
{
	vuserexit();									/* Call the users exit handler.		*/
	vexit();									/* And video's exit handler.		*/


/************* Also , do any ctrl-c trapping, see the macro stuff for info.		*********				*/

}


int vshut()										/* Shut down video i/o's		*/
{
	unsigned long status;

	if (!inited || !inputting) return(0);						/* Nothing to do yet.			*/

	ctrl_c_flag = 0;								/* Drop all control c info.		*/

	sys$cancel(vinput_chan);							/* Cancel the terminal.			*/
	status = sys$dassgn(vinput_chan);						/* Close the terminal channel.		*/

	if (status != SS$_NORMAL)
	{
		vraw_err("VSHUT - Error deassigning channel to terminal. error code is %x (hex).\n",status);
		return(0);
	}

	sys$cancel(vmbx_chan);								/* Cancel the mailbox.			*/
											/* But we'll keep the event flags.	*/
	inputting = 0;									/* Flag it.				*/

	return(1);
}


static int ctrl_c_ast()
{
	ctrl_c_flag = 1;								/* Flag it.				*/
	set_ctrl_c();
}

static int set_ctrl_c()
{
	unsigned long status;

	status = sys$qiow(vinput_ef,vinput_chan,IO$_SETMODE | IO$M_CTRLCAST,&vinput_iosb,0,0,ctrl_c_ast,0,3,0,0,0);

	if (status != SS$_NORMAL)
	{
		vraw_err("SET_CTRL_C - Error setting ctrl-c ast, error code is %x (hex).\n",status);
		return(0);
	}

}

static int vraw_err(format,p1,p2,p3,p4,p5,p6,p7,p8)
char *format,*p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8;
{
	printf("\n         ");
	printf(format,p1,p2,p3,p4,p5,p6,p7,p8);						/* Print the message.			*/
	printf("      \n");
}

unsigned long osd_ttype()								/* Return terminal type -- VMS.		*/
{
	char ch;
	unsigned long dtype;
	unsigned short length;
	static struct	{
				short unsigned int	buflen;				/* The length of the buffer.		*/
				short unsigned int	item_code;			/* The code for the request to GETDVI	*/
				char			*bufptr;			/* A pointer to the buffer.		*/
				short unsigned int	*retlen;			/* The return length of the buffer.	*/
				long int		endbuf;				/* The end of the buffer.		*/
			} mybuf;

	ch = vcheck();									/* Initialize the input channels.	*/
	if (ch) vpushc(ch);								/* If got a character then put it back.	*/

	length = 4;
	dtype = 0;
	mybuf.item_code = DVI$_DEVTYPE;							/* ask for the device type.		*/
	mybuf.buflen = 4;								/* in a 4 byte buffer			*/
	mybuf.bufptr = (char *)&dtype;							/* which is dtype			*/
	mybuf.retlen = &length;								/* return length address		*/
	mybuf.endbuf = 0;								/* mark end of buffers			*/

	sys$getdviw((long) 0,(short) VINPUT_CHAN_NUM,0,&mybuf,0,0,0,0);			/* get the physical terminal type	*/

	return(dtype);
}
#endif

