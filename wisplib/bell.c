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
#include "wisplib.h"
#include "vwang.h"
#include "wperson.h"

#ifdef	BELL
#undef	BELL
#endif

void BELL(int4 *bell_count)							/* WANG BELL function				*/
{
	int4	l_times;

	l_times = WL_get_swap(bell_count);

	WL_wtrace("BELL","ENTRY","Entry into BELL(%d)", l_times);

	if (!wisp_nativescreens())
	{
		vwang_bell(l_times);
	}
}
/*
**	History:
**	$Log: bell.c,v $
**	Revision 1.15  2003/01/31 17:23:49  gsl
**	Fix  copyright header
**	
**	Revision 1.14  2002/12/09 21:09:26  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.13  2002/07/12 17:00:53  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.12  2002/07/10 21:05:14  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.11  1997/10/23 20:05:55  gsl
**	Add missing  include
**	
**	Revision 1.10  1997-10-17 14:07:01-04  gsl
**	Add nativescreens() conditionals
**
**	Revision 1.9  1996-08-19 18:32:09-04  gsl
**	drcs update
**
**
**
*/
