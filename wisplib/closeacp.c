static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#if defined(unix) || defined(VMS)

/********************************************************************************************************************************
*																*
*	closeacp.c	Terminate and break logical connection between application program and TC device.  Use input		*
*			rel_line to access global acp_ch[] array to obtain channel number.  Call SYS$DASSGN to close		*
*			I/O channel, then clear corresponding entries from global arrays acp_term[], acp_ch[], acp_ef[],	*
*			and acp_iosb[].												*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*																*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int)				*
*																*
********************************************************************************************************************************/
#include "idsistd.h"
#include "acp.h"							/* Header file containing global variable definitions.	*/
#include "werrlog.h"

#ifdef unix
/*#include <termio.h>*/
extern struct termio otermacp;
#endif

void CLOSEACP(rel_line,ret_code)

int4	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
int4	*ret_code;							/* WANG ACP return code.				*/

{
#define		ROUTINE		9000

	int4	l_rel_line;						/* Local copy of rel_line to be wswapped.		*/
	int4	i,j,k;							/* Array indeces.					*/
	int4	status;							/* Return status from SYS$ASSIGN			*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	l_rel_line = *rel_line;						/* Initialize contents of l_rel_line.			*/
	wswap(&l_rel_line);						/* Swap words from WANG format, so the VAX can read it.	*/
	*ret_code = 0;							/* Initialize the return value.				*/

	if ((l_rel_line < 1) || (l_rel_line > ACPMAXDEV))
	{
		*ret_code = 22;						/* 22 = Invalid relative line number.			*/
		wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return;							/* Return to application program.			*/
	}

	i = l_rel_line - 1;						/* Set array index, point to requested line.		*/

#ifdef VMS
	status = SYS$DASSGN(acp_ch[i]);					/* Deassign the I/O channel.				*/
#endif
#ifdef unix
	ioctl(acp_ch[i],TCSETA,&otermacp);
	status = close(acp_ch[i]);
#endif     

	switch (status)							/* Finish processing according to possible statuses.	*/
	{
#ifdef VMS
		case SS$_NORMAL:					/* I/O channel successfully deassigned.			*/
#endif
#ifdef unix
	        case 0:
#endif		
	        {
			acp_term[i]=0;					/* Clear corresponding terminal id.			*/
			for(j=0;j<4;j++) acp_weor[i][j] = 0;		/* Clear Write EOR sequence.				*/
			for(j=0;j<3;j++)				/* Clear Read EOR sequences.				*/
				for(k=0;k<4;k++)
					 acp_reor[i][j][k] = 0;
			acp_ch[i]=0;					/* Clear corresponding I/O channel.			*/
			for(j=0;j<4;j++) acp_iosb[i][j]=0;		/* Clear corresponding I/O status block.		*/
			acp_ef[i]=0;					/* Clear corresponding event flag.			*/
			acp_allocated--;				/* Decrement number of lines allocated.			*/
			*ret_code = 0;					/* Return to application program successful completion.	*/
			break;
		}
#ifdef VMS
		case SS$_IVCHAN:					/* Invalid channel number.				*/
#endif
#ifdef unix
	        case EBADF:
#endif
	        {
			*ret_code = 22;					/* 22 = Invalid relative line number.			*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		default:						/* SS$_NOPRIV and everything else.			*/
		{
			*ret_code = 23;					/* 23 = Line was not open.				*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	}
}
#endif /* unix || VMS */
/*
**	History:
**	$Log: closeacp.c,v $
**	Revision 1.10  1996-08-19 18:32:12-04  gsl
**	drcs update
**
**
**
*/
