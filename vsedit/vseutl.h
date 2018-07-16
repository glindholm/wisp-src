/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		vseutl.h
**
**	Purpose:	To ...
**
*/

#ifndef vseutl_H
#define vseutl_H

void trunc(char *str);
void untrunc(char *s, int len);
int isblankstr(char *str, int len);
int exists(char *name);
void vse_untabify(char *str, int size);

#endif /* vseutl_H */
/*
**	History:
**	$Log: vseutl.h,v $
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:13  gsl
**	drcs update
**	
**
**
*/
