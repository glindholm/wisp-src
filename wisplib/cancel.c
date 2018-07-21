/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/
#include "idsistd.h"
#include "werrlog.h"

void CANCEL(void)
{
	WL_werrlog_error(WERRCODE(6004),"CANCEL","ENTRY","Routine CANCEL is not supported");
}

/*
**	History:
**	$Log: cancel.c,v $
**	Revision 1.11  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.10  2002/12/09 21:09:26  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.9  1996/08/19 22:32:11  gsl
**	drcs update
**	
**
**
*/
