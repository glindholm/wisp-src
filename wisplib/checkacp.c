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

/********************************************************************************************************************************
*																*
*	checkacp.c	This program always returns 0 for success.  Checking the status of operations pending on a		*
*			given terminal line is only necessary on the WANG.  For the VAX, all I/O operations in the		*
*			ACP emulation are done synchronously, completing upon return.						*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*				length		length of input buffer								*
*				inarea		input buffer									*
*				acp_wait	(optional) amount of time in .01 second units to wait before timeout		*
*				ws_interrupt	(optional) Y or N to allow or disallow detection of workstation interrupts	*
*																*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int) -- always 0 here.		*
*																*
********************************************************************************************************************************/
#include "werrlog.h"

void CHECKACP()
{
	WL_wtrace("CHECKACP","ENTRY","Entry into CHECKACP");
	return;
}
/*
**	History:
**	$Log: checkacp.c,v $
**	Revision 1.13  2003/01/31 17:23:48  gsl
**	Fix  copyright header
**	
**	Revision 1.12  2002/12/09 21:09:26  gsl
**	Use WL_wtrace(ENTRY)
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
