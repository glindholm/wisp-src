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
**	File:		vsedfnd.h
**
**	Purpose:	To ...
**
*/

#ifndef vsedfnd_H
#define vsedfnd_H

#include "idsistd.h"

void restart_find(void);
void vse_ed_find(void);
int vse_ed_change(void);
int validate_range(char *start_field, int4 *start_line, char *end_field, int4 *end_line);
int validate_linenum(char *num_string, int4 *num);
int get_line_and_index(int row, int4 *line);

#endif /* vsedfnd_H */
/*
**	History:
**	$Log: vsedfnd.h,v $
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:01  gsl
**	drcs update
**	
**
**
*/
