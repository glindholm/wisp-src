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
**	File:		vsedmod.h
**
**	Purpose:	To ...
**
**
*/

#ifndef vsedmod_H
#define vsedmod_H

#include "idsistd.h"
#include "vseglb.h"

void vse_ed_mod(void);
int mode_upper(void);
int language_case(void);
void vse_ed_mod_col(void);
int add_modcode(TEXT *txt);
void spaceout(char *str, int4 len);

#endif /* vsedmod_H */
/*
**	History:
**	$Log: vsedmod.h,v $
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:03  gsl
**	drcs update
**	
**
**
*/
