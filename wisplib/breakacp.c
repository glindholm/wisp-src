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

/********************************************************************************************************************************
*																*
*	breakacp.c	This program always returns 0 for success.  It is not necessary to send break sequences on		*
*			the VAX.												*
*			input:	rel_line	relative line number, (1-6), in WANG 4-byte binary format (wswap an int)	*
*			output:	ret_code	WANG 4-byte binary format return code (wswap an int) -- always 0 here.		*
*																*
********************************************************************************************************************************/

#include "idsistd.h"
#include "werrlog.h"

#if defined(unix)
void BREAKACP(int4* rel_line,int4 *ret_code)
{
	WL_wtrace("BREAKACP","ENTRY","Entry into BREAKACP");
	return;
}
#endif
/*
**	History:
**	$Log: breakacp.c,v $
**	Revision 1.13  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.12  2002/12/09 21:09:26  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.11  2002/06/26 01:42:45  gsl
**	Remove VMS code
**	
**	Revision 1.10  1996/08/19 22:32:10  gsl
**	drcs update
**	
**
**
*/
