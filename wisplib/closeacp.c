/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

#if defined(unix) 

/********************************************************************************************************************************
*																*
*	closeacp.c	Terminate and break logical connection between application program and TC device.  Use input		*
*			rel_line to access global acp_ch[] array to obtain channel number.  Call SYS$DASSGN to close		*
*			I/O channel, then clear corresponding entries from global arrays acp_term[], acp_ch[], acp_ef[],	*
*			and acp_iosb[].												*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (WL_wswap an int)	*
*																*
*			output:	ret_code	WANG 4-byte binary format return code (WL_wswap an int)				*
*																*
********************************************************************************************************************************/
#include "idsistd.h"
#include "acp.h"							/* Header file containing global variable definitions.	*/
#include "werrlog.h"
#include "wisplib.h"


void CLOSEACP(rel_line,ret_code)

int4	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
int4	*ret_code;							/* WANG ACP return code.				*/

{

	int4	l_rel_line;						/* Local copy of rel_line to be wswapped.		*/
	int4	i,j,k;							/* Array indeces.					*/
	int4	status;							/* Return status from SYS$ASSIGN			*/

	WL_wtrace("CLOSEACP", "ENTRY", "Entry into CLOSEACP");

	l_rel_line = *rel_line;						/* Initialize contents of l_rel_line.			*/
	WL_wswap(&l_rel_line);						/* Swap words from WANG format, so the VAX can read it.	*/
	*ret_code = 0;							/* Initialize the return value.				*/

	if ((l_rel_line < 1) || (l_rel_line > ACPMAXDEV))
	{
		*ret_code = 22;						/* 22 = Invalid relative line number.			*/
		WL_wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return;							/* Return to application program.			*/
	}

	i = l_rel_line - 1;						/* Set array index, point to requested line.		*/

	ioctl(acp_ch[i],TCSETA,&acp_oterm);
	status = close(acp_ch[i]);

	switch (status)							/* Finish processing according to possible statuses.	*/
	{
	        case 0:
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
	        case EBADF:
	        {
			*ret_code = 22;					/* 22 = Invalid relative line number.			*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		default:						/* SS$_NOPRIV and everything else.			*/
		{
			*ret_code = 23;					/* 23 = Line was not open.				*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	}
}
#endif /* unix  */
/*
**	History:
**	$Log: closeacp.c,v $
**	Revision 1.16  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.15  2003/01/29 19:42:50  gsl
**	Fix -Wall warnings
**	
**	Revision 1.14  2002/12/09 21:09:26  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.13  2002/07/12 17:00:54  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.12  2002/07/11 14:33:19  gsl
**	Cleanup ACP globals
**	
**	Revision 1.11  2002/06/26 01:42:45  gsl
**	Remove VMS code
**	
**	Revision 1.10  1996/08/19 22:32:12  gsl
**	drcs update
**	
**
**
*/
