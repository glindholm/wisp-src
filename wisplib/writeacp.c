/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*	writeacp.c	Transmit data on specified telecommunications line.							*
*			input:	rel_line	relative TC line number, (1-6), in WANG 4-byte binary format (WL_wswap an int)	*
*				length		length of data to be transmitted (up to 2048), WANG 4-byte binary		*
*				outarea		data to be transmitted								*
*																*
*			output:	ret_code	WANG 4-byte binary format return code (WL_wswap an int)				*
*																*
********************************************************************************************************************************/
#include "idsistd.h"
#include "acp.h"							/* Header file containing global variable definitions.	*/
#include "werrlog.h"
#include "wisplib.h"

void WRITEACP(rel_line,length,outarea,ret_code)

int	*rel_line;							/* Index from 1 to 6 for later access to acp_term[].	*/
int	*length;							/* Length of outarea.					*/
char	*outarea;							/* Buffer of data to be transmitted.			*/
int	*ret_code;							/* WANG ACP return code.				*/

{
	int	l_rel_line;						/* Local copy of rel_line to be wswapped.		*/
	int	l_length;						/* Local copy of length to be wswapped.			*/
	char	*ptr_outarea;						/* Working pointer for outarea.				*/
	char	*save_ptr;						/* Pointer to add WEOR sequence to outarea.		*/
	char	save_area[3];						/* Save area for data to be replaced by WEOR sequence.	*/
	int	i,j;							/* Indeces.						*/
	int4	status;							/* Return status from system services.			*/

	WL_wtrace_entry("WRITEACP");

	l_rel_line = *rel_line;						/* Initialize contents of l_rel_line.			*/
	WL_wswap(&l_rel_line);						/* Swap words from WANG format, so the VAX can read it.	*/
	l_length = *length;						/* Initialize contents of l_length.			*/
	WL_wswap(&l_length);						/* Swap words from WANG format, so the VAX can read it.	*/
	*ret_code = 0;							/* Initialize the return value.				*/

	if ((l_rel_line < 1) || (l_rel_line > ACPMAXDEV))
	{
		*ret_code = 22;						/* 22 = Invalid relative line number.			*/
		WL_wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return;							/* Return to application program.			*/
	}
	if ((l_length < 0) || (l_length > ACPMAXRECLEN))
	{
		*ret_code = 20;						/* 20 = Invalid record length.				*/
		WL_wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return;							/* Return to application program.			*/
	}

	i = l_rel_line-1;						/* Compute array index.					*/
	acp_ef[i] = 0;							/* No flag availiable, use flag 0.			*/

	/************************  Insert logic to write WEOR sequence at end of buffer.  ***************************************/
									/* acp_weor[i][0] contains length of WEOR sequence.	*/
	if (acp_weor[i][0] > 0)						/* If we need to write an end of record sequence...	*/
	{
		save_ptr = outarea;						/* Initialize pointer to output buffer.		*/
		for (j=0;j<l_length;j++) save_ptr++;				/* Point to character just past end of outarea.	*/
		ptr_outarea = save_ptr;						/* Set working pointer to end of output buffer.	*/
		for (j=0;j<3;j++) save_area[j] = *ptr_outarea++;		/* Save data to be replaced by WEOR sequence.	*/
		ptr_outarea = save_ptr;						/* Set working pointer to end of output buffer.	*/
		for (j=1;j<=acp_weor[i][0];j++) *ptr_outarea++ = acp_weor[i][j];	/* Append WEOR sequence to buffer.	*/
		l_length += acp_weor[i][0];					/* Add length of WEOR sequence to buffer length.*/
	}

	status = write(acp_ch[i],outarea,l_length)<0?errno:0;
	if (acp_weor[i][0] > 0)						/* If we used an end of record sequence...		*/
	{
		ptr_outarea = save_ptr;						/* Set working pointer to end of output buffer.	*/
		for (j=0;j<3;j++) *ptr_outarea++ = save_area[j];		/* Replace the data overwritten by WEOR.	*/
	}

	switch (status)							/* Finish processing according to possible statuses.	*/
	{
	        case 0:
	        {
			return;						/* Successful write request.				*/
		}
	        case EBADF:
	        case EIO:
		{
			*ret_code = 23;					/* 23 = Line not open.					*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	        case -1:/*never*/
	        {
			*ret_code = 22;					/* 22 = Invalid relative line number.			*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
		default:						/* SS$_ABORT and everything else.			*/
		{
			*ret_code = 14;					/* 14 = Connection Failure				*/
			WL_wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
	}
}
#endif /* unix  */
/*
**	History:
**	$Log: writeacp.c,v $
**	Revision 1.16  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.15  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.14  2002/12/10 17:09:12  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.13  2002/12/09 21:45:45  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.12  2002/07/12 17:01:05  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.11  2002/06/26 01:42:47  gsl
**	Remove VMS code
**	
**	Revision 1.10  1996/08/19 22:33:21  gsl
**	drcs update
**	
**
**
*/
