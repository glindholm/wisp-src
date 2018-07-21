/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		vsebasic.h
**
**	Purpose:	Header for vsebasic.c
**
**
**	History:
**	10/26/94	Written by GSL
**
*/

#ifndef vsebasic_H
#define vsebasic_H

#include "idsistd.h"
#include "vseglb.h"

void init_hash(void);
void find_linenum( TEXT *txt );
void update_linenum( int4 from, int4 to );
void delete_linenum( int4 lineno );
void update_branch(void);
void free_linenum(void);

#endif /* vsebasic_H */
/*
**	History:
**	$Log: vsebasic.h,v $
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:00  gsl
**	drcs update
**	
**
**
*/
