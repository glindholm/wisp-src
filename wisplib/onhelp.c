static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include "idsistd.h"
#include "wperson.h"

void ONHELP(void)									/* Turn help on.				*/
{
	int4	def_flags;

	get_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	def_flags |= HELP_ENABLED;
	set_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	save_defaults();
}

void NOHELP(void)									/* Turn help off.				*/
{
	int4	def_flags;

	get_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	def_flags &= (~HELP_ENABLED);
	set_defs(DEFAULTS_FLAGS,(char*)&def_flags);
	save_defaults();
}
/*
**	History:
**	$Log: onhelp.c,v $
**	Revision 1.9  1996/08/19 22:32:36  gsl
**	drcs update
**	
**
**
*/
