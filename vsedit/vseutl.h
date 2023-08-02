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
**	File:		vseutl.h
**
*/

#ifndef vseutl_H
#define vseutl_H

void vse_trunc(char *str);
void vse_untrunc(char *s, int len);
int isblankstr(char *str, int len);
int vse_exists(char *name);
void vse_untabify(char *str, int size);

#endif /* vseutl_H */
/*
**	History:
**	$Log: vseutl.h,v $
**	Revision 1.7  2010/01/10 00:36:15  gsl
**	refactor utils to add vse_ prefix to avoid conflicts with trunc
**	vse_trunc
**	
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:13  gsl
**	drcs update
**	
**
**
*/
