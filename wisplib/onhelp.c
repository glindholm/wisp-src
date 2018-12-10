/*
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
*/


#include "idsistd.h"
#include "wperson.h"

void ONHELP(void)									/* Turn help on.				*/
{
	int4	def_flags;

	WL_get_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	def_flags |= HELP_ENABLED;
	WL_set_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	WL_save_defaults();
}

void NOHELP(void)									/* Turn help off.				*/
{
	int4	def_flags;

	WL_get_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	def_flags &= (~HELP_ENABLED);
	WL_set_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	WL_save_defaults();
}
/*
**	History:
**	$Log: onhelp.c,v $
**	Revision 1.11  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.10  2002/07/10 21:05:21  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  1996/08/19 22:32:36  gsl
**	drcs update
**	
**
**
*/
