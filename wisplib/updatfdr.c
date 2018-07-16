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


#include "idsistd.h"
#include "werrlog.h"

void UPDATFDR()
{
	WL_werrlog_error(WERRCODE(65000),"UPDATFDR", "ENTRY", 
		"UPDATFDR not supported");
}
/*
**	History:
**	$Log: updatfdr.c,v $
**	Revision 1.12  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/12/10 17:09:16  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.10  2002/12/09 19:15:35  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.9  1996/08/19 22:33:02  gsl
**	drcs update
**	
**
**
*/
