/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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


/*
**	File:		reduce.h
**
**
**	History:
**	09/20/93	Written by GSL
**
*/

#ifndef REDUCE_H
#define REDUCE_H

#include "node.h"

extern NODE reduce_data_item(NODE start);
extern NODE reduce_parens(NODE start);
extern NODE reduce_one(NODE start);


#endif /* REDUCE_H */
/*
**	History:
**	$Log: reduce.h,v $
**	Revision 1.7  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.6  1996/08/31 01:56:09  gsl
**	drcs update
**	
**
**
*/
