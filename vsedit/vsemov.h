/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		vsemov.h
**
**	Purpose:	To ...
**
*/

#ifndef vsemov_H
#define vsemov_H

#include "idsistd.h"
#include "vseglb.h"

void vse_mov_lin(void);
void vse_cpy_lin(void);
int vse_renumberer(int4 number, int4 incr, int4 start, int4 end, int4 *count);
int do_renumber(TEXT *txt_start, TEXT *txt_end, int4 number, int4 incr);
int validate_numincr(char *number_field, int4 *number, char *incr_field, int4 *incr);
int vse_xcopy(void);

#endif /* vsemov_H */
/*
**	History:
**	$Log: vsemov.h,v $
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:09  gsl
**	drcs update
**	
**
**
*/
