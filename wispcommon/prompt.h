/*
******************************************************************************
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
******************************************************************************
*/


/*
**	File:		prompt.h
**
**	Purpose:	To ...
**
**
**	History:
**	05/27/94	Written by GSL
**
*/

#ifndef PROMPT_H
#define PROMPT_H

int prompt_list(const char *message, const char *defstr, const char *list, const char *help);
int prompt_text(const char *message, const char *defstr, int empty, const char *help, char *text);
int prompt_num (const char *message, const char *defstr, const char *help, int4 *outnum);

#define PROMPT_RC_EXIT		-1
#define PROMPT_RC_DEFAULT	0
#define PROMPT_RC_USER_VALUE	1
#define PROMPT_RC_EMPTY		2

#define PROMPT_EMPTY_ALLOWED	1
#define PROMPT_EMPTY_NOTALLOWED	0

#endif /* PROMPT_H */
/*
**	History:
**	$Log: prompt.h,v $
**	Revision 1.7  2003/05/27 20:53:08  gsl
**	Update PROMPT prototypes and add defines for return codes
**	
**	Revision 1.6  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.5  1996/07/23 18:17:49  gsl
**	drcs update
**	
**
**
*/
