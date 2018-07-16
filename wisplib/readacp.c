static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#if defined(unix) || defined(VMS)
/********************************************************************************************************************************
*																*
*	readacp.c	Obtain data from specified line.									*
*			input:	rel_line	relative TC line number, (1-6), in WANG 4-byte binary format			*
*				length		length of input buffer (up to 2048), WANG 4-byte binary format			*
*				acp_wait	amount of time in .01 second units to wait					*
*																*
*			output:	ret_code	WANG 4-byte binary format return code						*
*				inarea		data received									*
*																*
********************************************************************************************************************************/
#include "idsistd.h"

#ifdef VMS
#include <iodef.h>							/* I/O function definitions.				*/
#include "trmdef.h"							/* Terminal function modifier definitions.		*/
#endif

#ifdef unix
#include <signal.h>
#include <termio.h>
#include <sys/types.h>
extern struct termio ntermacp;
#include <fcntl.h>
static int matchreor();
static void eortimeout();
#endif

#include <varargs.h>							/* Allow variable number of arguments.			*/
#include "acp.h"							/* Header file containing global variable definitions.	*/
#include "movebin.h"

static int io_error();

#define INVALIDLINE	20000						/* For io_error invalid relative line number.		*/
#define INVALIDRECLEN	20001						/* For io_error invalid record length.			*/
#define ACPTIMEOUT	20002						/* For io_error time out.				*/
#define ONECHAR		1						/* To indicate one character QIO read.			*/
#define FOREVER		while(1)					/* Define indeterminately repeating loop.		*/

#ifdef VMS
typedef	struct	{							/* Definition of item list descriptor for IO$READVBLK	*/
			short	item_buf_len;				/* Buffer length.					*/
			short	item_code;				/* Item code.						*/
			int4	item_imm_data;				/* Buffer address or immediate data.			*/
			int4	item_ret_address;			/* Return address.					*/
		} itemlist_desc;
#endif
/********************************************************************************************************************************
*							Subroutine entry point							*
********************************************************************************************************************************/
READACP(va_alist)

va_dcl

{
	va_list	arg_stack;						/* Pointer to traverse stack of input arguments.	*/
	int	arg_count;						/* Argument counter.					*/

	int4	*rel_line;						/* Index from 1 to 6 for later access to acp_term[].	*/
	int4	*length;						/* Length of inarea.					*/
	char	*inarea;						/* Input buffer for read request.			*/
	int4	*acp_wait;						/* Amount of time to wait before returning to user.	*/
	int4	*ret_code;						/* WANG ACP return code.				*/

	int4	l_rel_line;						/* Local copy of rel_line to be wswapped.		*/

	int4	l_length;						/* Local copy of length to be wswapped.			*/
	char	*l_inarea;						/* Local copy of pointer to input buffer.		*/

	int4	l_acp_wait;						/* Local copy of acp_wait to be wswapped.		*/
#ifdef unix
	int cnt,eor_found;
	char ch;
	int eor1pos,eor2pos,eor3pos;
	void eortimeout();
	int starttime;
#endif	
#ifdef VMS
	int	item_list_length;					/* Number of descriptors in item list for QIO read.	*/

	itemlist_desc	item_list[3];					/* Item list descriptor for QIO IO$_READVBLK		*/
	itemlist_desc	*ptr_item_list;					/* Pointer to item list descriptor.			*/

	char	qio_term_mask[32];					/* Bit mask, length 256, for QIO terminator characters.	*/
	char	qio_byte_mask;						/* Scratch byte used as bit mask.			*/
	struct	{							/* QIO terminator mask quadword.			*/
			short	qio_mask_size;				/* Mask size in bytes.					*/
			short	qio_not_used;				/* Not used.						*/
			int4	qio_mask_address;			/* Address of mask.					*/
		} qio_mask_qword;
	int	qio_length;						/* Length parameter for $QIO read.			*/
#endif
	int	i,j,k,x;						/* Array indeces.					*/
	int4	status;							/* Return status from system services.			*/
	int	eor_flag;						/* Flag to direct processing for read of EOR sequence.	*/

	/********************************************************
	*	Receive variable number of arguments		*
	********************************************************/
	va_start(arg_stack);						/* Set pointer to top of stack.				*/
	arg_count = va_count(arg_stack);				/* How many args are there ?				*/

	va_start(arg_stack);						/* Go back to the top of the stack.			*/

	rel_line = va_arg(arg_stack, int4*);				/* Get relative line from arg stack.			*/
	arg_count--;							/* One less argument.					*/
	l_rel_line = *rel_line;						/* Initialize contents of l_rel_line.			*/
	wswap(&l_rel_line);						/* Swap words from WANG format, so the VAX can read it.	*/

	length = va_arg(arg_stack, int4*);				/* Get buffer length from arg stack.			*/
	arg_count--;							/* One less argument.					*/
	l_length = *length;						/* Initialize contents of l_length.			*/
	wswap(&l_length);						/* Swap words from WANG format, so the VAX can read it.	*/

	inarea = va_arg(arg_stack, char*);				/* Get pointer to buffer from arg stack.		*/
	arg_count--;							/* One less argument.					*/

	if (arg_count > 1)						/* If more than one argument left, then get acp_wait.	*/
	{
		acp_wait = va_arg(arg_stack, int4*);			/* Get wait time from arg stack.			*/
		arg_count--;						/* One less argument.					*/
		l_acp_wait = *acp_wait;					/* Initialize contents of l_acp_wait.			*/
		wswap(&l_acp_wait);					/* Swap words from WANG format, so the VAX can read it.	*/
		l_acp_wait = l_acp_wait/100;				/* Convert from .01 second units to 1 second units.	*/
	}
	else	l_acp_wait = -1;					/* No time out specified.				*/

	ret_code = va_arg(arg_stack, int4*);				/* Get pointer to return code from arg stack.		*/
	arg_count--;							/* One less argument.					*/

	*ret_code = 0;							/* Initialize the return value.				*/

	if ((l_rel_line < 1) || (l_rel_line > ACPMAXDEV))
	{
#ifdef VMS
		io_error(INVALIDLINE,ret_code);				/* Invalid relative line number.			*/
#endif
#ifdef unix
		*ret_code=22;
		wswap(ret_code);
#endif
		return(0);						/* Return to caller.					*/
	}
#ifdef VMS
	if ((l_length < 0) || (l_length > ACPMAXRECLEN))
	{
		io_error(INVALIDRECLEN,ret_code);			/* Invalid record length.				*/
		return(0);						/* Return to caller.					*/
	}
#endif

	/********************************************************
	*	Set up parameters for QIO IO$_READVBLK		*
	********************************************************/
	i = l_rel_line-1;						/* Compute array index.					*/
	acp_ef[i] = 0;							/* No flag availiable, use flag 0.			*/

#ifdef VMS
	/****************************************
	*	Itemlist read descriptors.	*
	****************************************/
	ptr_item_list = item_list;					/* Initialize pointer to itemlist read descriptors.	*/

	ptr_item_list->item_buf_len = 0;				/* Modifiers descriptor.				*/
	ptr_item_list->item_code = TRM$_MODIFIERS;
	if (l_acp_wait > 0)						/* Time out specified.					*/
	{
		ptr_item_list->item_imm_data = TRM$M_TM_NOECHO | TRM$M_TM_NOFILTR | TRM$M_TM_NOEDIT | TRM$M_TM_NORECALL |
					       TRM$M_TM_TIMED | TRM$M_TM_TRMNOECHO;
	}
	else								/* No time out specified.				*/
	{
		ptr_item_list->item_imm_data = TRM$M_TM_NOECHO | TRM$M_TM_NOFILTR | TRM$M_TM_NOEDIT | TRM$M_TM_NORECALL |
					       TRM$M_TM_TRMNOECHO;
	}
	ptr_item_list->item_ret_address = 0;

	ptr_item_list++;
	ptr_item_list->item_buf_len = 32;				/* Descriptor for I/O terminator mask.			*/
	ptr_item_list->item_code = TRM$_TERM;
	ptr_item_list->item_imm_data = (int4)qio_term_mask;
	ptr_item_list->item_ret_address = 0;

	if (l_acp_wait > 0)						/* If time out specified...				*/
	{
		item_list_length = 36;					/* Length of descriptor list includes time out block.	*/
		ptr_item_list++;
		ptr_item_list->item_buf_len = 0;			/* Descriptor for time out.				*/
		ptr_item_list->item_code = TRM$_TIMEOUT;
		ptr_item_list->item_imm_data = l_acp_wait;
		ptr_item_list->item_ret_address = 0;
	}
	else	item_list_length = 24;					/* No time out specified, do not include time out block.*/

	/****************************************
	*	QIO terminator mask quadword.	*
	****************************************/
	for (j=0;j<32;j++) qio_term_mask[j]=0;				/* Clear out the bit mask for QIO terminator chars.	*/
	for (j=0;j<3;j++)						/* Generate bit mask for 3 EOR terminator characters.	*/
	{								/* Use only first char of each of 3 given EOR sequences.*/
		x = acp_reor[i][j][1];					/* Obtain value of EOR char.				*/
		if (acp_reor[i][j][0] != 0)				/* Only if there is an EOR sequence here.		*/
		{
			qio_byte_mask = 1;				/* Set low order bit in scratch byte on, rest off.	*/
			qio_byte_mask = qio_byte_mask << (x%8);		/* Shift left by remainder after dividing by 8.		*/
			k = x/8;					/* Calculate qio_term_mask array offset, truncate.	*/
			qio_term_mask[k] = qio_term_mask[k] | qio_byte_mask;	/* Bitwise OR to set specified bit on.		*/
		}
	}
	qio_mask_qword.qio_mask_size = 32;				/* Mask size in bytes.					*/
	qio_mask_qword.qio_not_used = 0;				/* Not used.						*/
	qio_mask_qword.qio_mask_address = (int4)qio_term_mask;		/* Address of mask.					*/


	/************************************************************************
	*	VMS info:								*
	*	The following loop does the $QIO read.  The read request	*
	*	is completed when one of the following conditions occurs:	*
	*		1.)  l_length characters are read.			*
	*		2.)  A time out occurs.					*
	*		3.)  A complete EOR sequence is found.			*
	*									*
	************************************************************************/
	/********************************************************
	*  For your information, the following QIOs yield:	*
	*	acp_iosb[i][0]   == condition value		*
	*	acp_iosb[i][1]   == transfer count		*
	*	acp_iosb[i][2,3] == device specific		*
	********************************************************/
	l_inarea = inarea;						/* Set local pointer to input buffer.			*/
	for(j=0;j<l_length;j++) *l_inarea++ = 0;			/* Clear out the input buffer.				*/
	l_inarea = inarea;						/* Set local pointer to input buffer.			*/
	do
	{
		if (l_length > 0)					/* Issue synchronous write request.			*/
		{
			status = sys$qiow(acp_ef[i],acp_ch[i],IO$_READVBLK | IO$M_EXTEND,acp_iosb[i],0,0,
					  l_inarea,l_length,0,0,item_list,item_list_length);
		}
		else return(0);						/* Return normal successful completion.			*/

		if (status != SS$_NORMAL)
		{
			io_error(status,ret_code);			/* Handle $QIO error.					*/
			return(0);					/* Return to caller.					*/
		}
		if (acp_iosb[i][0] == 0x022C)
		{
			io_error(ACPTIMEOUT,ret_code);			/* I/O timed out.					*/
			return(0);					/* Return to caller.					*/
		}
		if (l_length == acp_iosb[i][1]) return(0);		/* All characters received, return to caller.		*/

									/* Here, we KNOW I/O terminated with an EOR char.	*/
		l_inarea +=  acp_iosb[i][1];				/* Set pointer to last char received, (the EOR char).	*/
		l_length -= (acp_iosb[i][1] + 1);			/* Reduce length for next QIO by (chars read + 1 EOR)	*/
		j=0; k=1;						/* Initialize array indeces to first EOR sequence.	*/
		while (*l_inarea != acp_reor[i][j][k] && j<3) j++;	/* Point [i][j][k] to correct EOR sequence.		*/
		if (acp_reor[i][j][0] == 1) break;			/* EOR seq is one character int4, and we've found it.	*/

		for (k=2;k<=acp_reor[i][j][0];k++)			/* Loop from second EOR char to last.			*/
		{
			if (acp_reor[i][j][0] == 1) break;		/* EOR seq is one character int4, and we've found it.	*/
			l_inarea++;					/* Increment to next available buffer position.		*/
			status = sys$qiow(acp_ef[i],acp_ch[i],IO$_READVBLK | IO$M_EXTEND,acp_iosb[i],0,0,	/* Read 1 char	*/
					  l_inarea,ONECHAR,0,0,item_list,item_list_length);			/* at a time.	*/
			if (status != SS$_NORMAL)
			{
				io_error(status,ret_code);		/* Handle $QIO error.					*/
				return(0);				/* Return to caller.					*/
			}
			if (acp_iosb[i][0] == 0x022C)
			{
				io_error(ACPTIMEOUT,ret_code);		/* I/O timed out.					*/
				return(0);				/* Return to caller.					*/
			}
			l_length--;					/* Decrement for one character read.			*/
			if (*l_inarea != acp_reor[i][j][k])		/* If not in EOR seq...					*/
			{
				j=0; k=1;				/* Initialize array indeces to first EOR sequence.	*/
				while (*l_inarea != acp_reor[i][j][k] && j<3) j++;	/* Find out if it starts another EOR.	*/
				if (j >= 3) break;			/* If not EOR sequence, then go back to normal read.	*/
									/* Otherwise, it is a new EOR sequence, continue loop.	*/
			}
		}
		if (j < 3)						/* (j >= 3) indicates no EOR, go back to normal read.	*/
			if (k > acp_reor[i][j][0]) break;		/* Found completed EOR sequence.			*/
		l_inarea++;						/* Increment to next available buffer position.		*/
	}FOREVER;

	/********************************************************************************
	*	Proceed from the above loop only when complete EOR sequence is found.	*
	********************************************************************************/
	*ret_code = j+1;						/* j+1 == EOR 1, 2, or 3 detected			*/
	wswap(ret_code);						/* Swap words so the WANG can read it.			*/
	return(0);							/* Return to caller.					*/
#endif 
#ifdef unix
	if (l_acp_wait != -1)
	{
		if (acp_blockmode[i]==BLOCKING)
		{
			int	oflag;

			acp_blockmode[i]=NONBLOCKING;
			close(acp_ch[i]);
#ifdef O_NDELAY
			oflag = O_RDWR|O_NDELAY;
#else
			oflag = O_RDWR|O_NONBLOCK;
#endif
			acp_ch[i]=open(acp_devname[i],oflag);
			ioctl(acp_ch[i],TCSETA,&ntermacp);			/* satisfied */
		}
		ioctl(acp_ch[i],TCGETA,&ntermacp);
		ntermacp.c_cc[VTIME] = l_acp_wait;			/* VTIME is mininum time to wait for request to be  */
	        ntermacp.c_cc[VMIN] = 0;
		ioctl(acp_ch[i],TCSETA,&ntermacp);			/* satisfied */
	}
	else
	{
		if (acp_blockmode[i]==NONBLOCKING)
		{
			acp_blockmode[i]=BLOCKING;
			close(acp_ch[i]);
			acp_ch[i]=open(acp_devname[i],O_RDWR);
			ioctl(acp_ch[i],TCSETA,&ntermacp);			/* satisfied */
		}
		ioctl(acp_ch[i],TCGETA,&ntermacp);
		ntermacp.c_cc[VTIME] = 0;				/* VTIME is mininum time to wait for request to be  */
	        ntermacp.c_cc[VMIN] = 1;
		ioctl(acp_ch[i],TCSETA,&ntermacp);			/* satisfied */
	}

	cnt=eor_found=eor_timeout=0;
	l_inarea=inarea;
	if (l_acp_wait != -1) 
	{
		signal(SIGALRM,eortimeout);				/* setup routine to handle alarm */
		alarm(l_acp_wait);					/* tell kernel to send alarm */
	}
	*ret_code=0;
	for (eor1pos=eor2pos=eor3pos=eor_found=0;cnt<l_length && !eor_found;)
	{
		status=read(acp_ch[i],&ch,1);

		if (status<1)                                          /* no chars read */
		{
			if (eor_timeout) break;

			if (-1 == status && EAGAIN != errno)
			{
				/* 
				**	An errno of EAGAIN is the only one we can handle.
				*/
				*ret_code = 99;
				break;
			}

			sleep(1);
			continue;
		}
		
		++cnt;
		*l_inarea++ = ch;
		if (matchreor(acp_term_struct.acp_reorseq[0],&eor1pos,ch)) 
		{
			*ret_code=eor_found=1; 
		}
		else if (matchreor(acp_term_struct.acp_reorseq[1],&eor2pos,ch))
		{
			*ret_code=eor_found=2; 
		}
		else if (matchreor(acp_term_struct.acp_reorseq[2],&eor3pos,ch))
		{
			*ret_code=eor_found=3; 
		}
	}
	alarm(0);
	if (!(*ret_code==1 || *ret_code==2 || *ret_code==3))
	{
		if (eor_timeout)
		{
			if (cnt) *ret_code=4;
			else *ret_code=6;
		}
	}
	PUTBIN(length,&cnt,4);
	wswap(length);
	wswap(ret_code);
#endif 
}
#ifdef unix
static int matchreor(seq,pos,ch)
char *seq;
int *pos,ch;
{
	if (seq[0]==(char)0) return 0;					/* no sequence to match */
	if (seq[*pos+1]!=ch)						/* match current char in sequence? */
	{
		*pos=0;							/* no, reset this sequence's position */
		return 0;
	}
	else ++(*pos);							/* else point to next */
	if (*pos>=seq[0]) return 1;					/* if matched all chars, return true value */
	else return 0;							/* else not */
}
static void eortimeout(sig)
int	sig;
{
	++eor_timeout;
}
#endif

/********************************************************************************************************************************
*	io_error	Generate appropriate return code values according to return status of $QIO service.			*
*			input:	status		return status from $QIO system service.						*
*				ret_code	pointer to return code for application program.					*
*																*
*			output:	ret_code	value placed into *ret_code.							*
********************************************************************************************************************************/
static int io_error(status,ret_code)
int4	status;
int	*ret_code;
{
#ifdef VMS
	switch (status)							/* Finish processing according to possible statuses.	*/
	{
	        case SS$_NORMAL:					/* Successful write request.				*/
	        { 
			return(0); 
			break; 
		}
		case SS$_NOPRIV:					/* Specified channel does not exist.			*/
		case SS$_SHUT:						/* Node is no longer accepting connections.		*/
		case SS$_THIRDPARTY:					/* Logical link terminated by third party.		*/
		{
			*ret_code = 23;					/* 23 = Line not open.					*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		case SS$_IVCHAN:					/* Invalid channel number.				*/
		{
			*ret_code = 22;					/* 22 = Invalid relative line number.			*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		case INVALIDLINE:
		{
			*ret_code = 22;					/* 22 = Invalid relative line number.			*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		case INVALIDRECLEN:
		{
			*ret_code = 20;					/* 20 = Invalid record length.				*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		case ACPTIMEOUT:
		{
			*ret_code = 04;					/* 04 = EOR timeout occurred.				*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		default:						/* SS$_ABORT and everything else.			*/
		{
			*ret_code = 14;					/* 14 = Connection Failure				*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	}
#endif
	return(0);							/* Return to caller.					*/
}
#endif /* unix || VMS */
/*
**	History:
**	$Log: readacp.c,v $
**	Revision 1.11  1997/06/05 16:08:42  scass
**	Moved return outside of #ifdef to resolve warning.
**	
**	Revision 1.10  1996-08-19 18:32:46-04  gsl
**	drcs update
**
**
**
*/
