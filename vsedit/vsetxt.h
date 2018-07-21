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
**	File:		vsetxt.h
**
**	Purpose:	To ...
**
*/

#ifndef vsetxt_H
#define vsetxt_H

#include "vseglb.h"

TEXT *new_text(char *str);
TEXT *over_text(TEXT *txt, char *str);
void insert_text(TEXT *txt1, TEXT *txt2, TEXT *newtxt);
void del_text(TEXT *txt);
void append_text(TEXT *txt);
void free_text(void);
void free_text_list(TEXT *first);
void free_one_text(TEXT *txt);


#endif /* vsetxt_H */
/*
**	History:
**	$Log: vsetxt.h,v $
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:12  gsl
**	drcs update
**	
**
**
*/
