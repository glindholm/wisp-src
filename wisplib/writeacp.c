/********************************************************************************************************************************
*																*
*	writeacp.c	Transmit data on specified telecommunications line.							*
*			input:	rel_line	relative TC line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*				length		length of data to be transmitted (up to 2048), WANG 4-byte binary		*
*				outarea		data to be transmitted								*
*																*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int)				*
*																*
********************************************************************************************************************************/
#include "idsistd.h"
#include "acp.h"							/* Header file containing global variable definitions.	*/
#ifdef VMS
#include <iodef.h>							/* I/O function definitions.				*/
#endif

WRITEACP(rel_line,length,outarea,ret_code)

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

	l_rel_line = *rel_line;						/* Initialize contents of l_rel_line.			*/
	wswap(&l_rel_line);						/* Swap words from WANG format, so the VAX can read it.	*/
	l_length = *length;						/* Initialize contents of l_length.			*/
	wswap(&l_length);						/* Swap words from WANG format, so the VAX can read it.	*/
	*ret_code = 0;							/* Initialize the return value.				*/

	if ((l_rel_line < 1) || (l_rel_line > ACPMAXDEV))
	{
		*ret_code = 22;						/* 22 = Invalid relative line number.			*/
		wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return(0);						/* Return to application program.			*/
	}
	if ((l_length < 0) || (l_length > ACPMAXRECLEN))
	{
		*ret_code = 20;						/* 20 = Invalid record length.				*/
		wswap(ret_code);					/* Swap words so the WANG can read it.			*/
		return(0);						/* Return to application program.			*/
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

#ifdef VMS
									/* Issue synchronous write request.			*/
	status = sys$qiow(acp_ef[i],acp_ch[i],IO$_WRITEVBLK | IO$M_NOFORMAT,acp_iosb[i],0,0,outarea,l_length,0,0,0,0);
#endif
#ifdef unix
	status = write(acp_ch[i],outarea,l_length)<0?errno:0;
#endif
	if (acp_weor[i][0] > 0)						/* If we used an end of record sequence...		*/
	{
		ptr_outarea = save_ptr;						/* Set working pointer to end of output buffer.	*/
		for (j=0;j<3;j++) *ptr_outarea++ = save_area[j];		/* Replace the data overwritten by WEOR.	*/
	}

	switch (status)							/* Finish processing according to possible statuses.	*/
	{
#ifdef VMS
		case SS$_NORMAL:
#endif
#ifdef unix
	        case 0:
#endif
	        {
			return 0;					/* Successful write request.				*/
		}
#ifdef VMS
		case SS$_NOPRIV:					/* Specified channel does not exist.			*/
		case SS$_SHUT:						/* Node is no longer accepting connections.		*/
		case SS$_THIRDPARTY:					/* Logical link terminated by third party.		*/
#endif
#ifdef unix  
	        case EBADF:
	        case EIO:
#endif
		{
			*ret_code = 23;					/* 23 = Line not open.					*/
			wswap(ret_code);				/* Swap words so the WANG can read it.			*/
			break;
		}
#ifdef VMS
		case SS$_IVCHAN:					/* Invalid channel number.				*/
#endif
#ifdef unix  
	        case -1:/*never*/
#endif		
	        {
			*ret_code = 22;					/* 22 = Invalid relative line number.			*/
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
}

