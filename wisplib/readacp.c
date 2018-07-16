/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

#if defined(unix) 
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

#include <string.h>
#include <signal.h>
#include <termio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdarg.h>							/* Allow variable number of arguments.			*/

#include "idsistd.h"
#include "acp.h"							/* Header file containing global variable definitions.	*/
#include "werrlog.h"
#include "wisplib.h"

#define INVALIDLINE	20000						/* For io_error invalid relative line number.		*/
#define INVALIDRECLEN	20001						/* For io_error invalid record length.			*/
#define ACPTIMEOUT	20002						/* For io_error time out.				*/
#define ONECHAR		1						/* To indicate one character QIO read.			*/
#define FOREVER		while(1)					/* Define indeterminately repeating loop.		*/

static int eor_timeout;

static int matchreor();
static void eortimeout();

/********************************************************************************************************************************
*							Subroutine entry point							*
********************************************************************************************************************************/
void READACP(int4 *rel_line, int4 *length, char *inarea, ...)
{
	va_list	arg_stack;						/* Pointer to traverse stack of input arguments.	*/
	int	arg_count;						/* Argument counter.					*/

	/* int4	*rel_line; */						/* Index from 1 to 6 for later access to acp_term[].	*/
	/* int4	*length; */						/* Length of inarea.					*/
	/* char	*inarea; */						/* Input buffer for read request.			*/
	int4	*acp_wait;						/* Amount of time to wait before returning to user.	*/
	int4	*ret_code;						/* WANG ACP return code.				*/

	int4	l_rel_line;						/* Local copy of rel_line to be wswapped.		*/

	int4	l_length;						/* Local copy of length to be wswapped.			*/
	char	*l_inarea;						/* Local copy of pointer to input buffer.		*/

	int4	l_acp_wait;						/* Local copy of acp_wait to be wswapped.		*/

	int cnt,eor_found;
	char ch;
	int eor1pos,eor2pos,eor3pos;
	void eortimeout();

	int	i;							/* Array indeces.					*/
	int4	status;							/* Return status from system services.			*/

	/********************************************************
	*	Receive variable number of arguments		*
	********************************************************/
	va_start(arg_stack, inarea);					/* Set pointer to top of stack.				*/
	arg_count = WL_va_count();					/* How many args are there ?				*/

	WL_wtrace("READACP","ENTRY", "Entry in READACP args=%d", arg_count);

	/* rel_line = va_arg(arg_stack, int4*);	*/			/* Get relative line from arg stack.			*/
	arg_count--;							/* One less argument.					*/
	l_rel_line = *rel_line;						/* Initialize contents of l_rel_line.			*/
	WL_wswap(&l_rel_line);						/* Swap words from WANG format, so the VAX can read it.	*/

	/* length = va_arg(arg_stack, int4*); */			/* Get buffer length from arg stack.			*/
	arg_count--;							/* One less argument.					*/
	l_length = *length;						/* Initialize contents of l_length.			*/
	WL_wswap(&l_length);						/* Swap words from WANG format, so the VAX can read it.	*/

	/* inarea = va_arg(arg_stack, char*); */			/* Get pointer to buffer from arg stack.		*/
	arg_count--;							/* One less argument.					*/

	if (arg_count > 1)						/* If more than one argument left, then get acp_wait.	*/
	{
		acp_wait = va_arg(arg_stack, int4*);			/* Get wait time from arg stack.			*/
		arg_count--;						/* One less argument.					*/
		l_acp_wait = *acp_wait;					/* Initialize contents of l_acp_wait.			*/
		WL_wswap(&l_acp_wait);					/* Swap words from WANG format, so the VAX can read it.	*/
		l_acp_wait = l_acp_wait/100;				/* Convert from .01 second units to 1 second units.	*/
	}
	else	l_acp_wait = -1;					/* No time out specified.				*/

	ret_code = va_arg(arg_stack, int4*);				/* Get pointer to return code from arg stack.		*/
	arg_count--;							/* One less argument.					*/

	*ret_code = 0;							/* Initialize the return value.				*/

	if ((l_rel_line < 1) || (l_rel_line > ACPMAXDEV))
	{
		*ret_code=22;
		WL_wswap(ret_code);
		return;							/* Return to caller.					*/
	}

	/********************************************************
	*	Set up parameters for QIO IO$_READVBLK		*
	********************************************************/
	i = l_rel_line-1;						/* Compute array index.					*/
	acp_ef[i] = 0;							/* No flag availiable, use flag 0.			*/

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
			ioctl(acp_ch[i],TCSETA,&acp_nterm);			/* satisfied */
		}
		ioctl(acp_ch[i],TCGETA,&acp_nterm);
		acp_nterm.c_cc[VTIME] = l_acp_wait;			/* VTIME is mininum time to wait for request to be  */
	        acp_nterm.c_cc[VMIN] = 0;
		ioctl(acp_ch[i],TCSETA,&acp_nterm);			/* satisfied */
	}
	else
	{
		if (acp_blockmode[i]==NONBLOCKING)
		{
			acp_blockmode[i]=BLOCKING;
			close(acp_ch[i]);
			acp_ch[i]=open(acp_devname[i],O_RDWR);
			ioctl(acp_ch[i],TCSETA,&acp_nterm);			/* satisfied */
		}
		ioctl(acp_ch[i],TCGETA,&acp_nterm);
		acp_nterm.c_cc[VTIME] = 0;				/* VTIME is mininum time to wait for request to be  */
	        acp_nterm.c_cc[VMIN] = 1;
		ioctl(acp_ch[i],TCSETA,&acp_nterm);			/* satisfied */
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
	WL_put_swap(length, cnt);
	WL_wswap(ret_code);
}
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
#endif /* unix  */
/*
**	History:
**	$Log: readacp.c,v $
**	Revision 1.21  2003/02/21 20:10:03  gsl
**	Switch READACP to stdarg.h
**	
**	Revision 1.20  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.19  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.18  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.17  2002/12/10 17:09:18  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.16  2002/07/16 16:24:53  gsl
**	Globals
**	
**	Revision 1.15  2002/07/12 17:00:59  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.14  2002/07/11 20:29:12  gsl
**	Fix WL_ globals
**	
**	Revision 1.13  2002/07/11 14:33:19  gsl
**	Cleanup ACP globals
**	
**	Revision 1.12  2002/06/26 01:42:46  gsl
**	Remove VMS code
**	
**	Revision 1.11  1997/06/05 16:08:42  scass
**	Moved return outside of #ifdef to resolve warning.
**	
**	Revision 1.10  1996-08-19 18:32:46-04  gsl
**	drcs update
**
**
**
*/
