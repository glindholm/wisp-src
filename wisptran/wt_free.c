static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/* Routines to process the FREE ALL and COMMIT statements.									*/

#define EXT extern
#include "wisp.h"

p_free()
{
	int is_comit;

	ptype = get_param(o_parms[0]);							/* get next parameter (should be FREE)	*/

	write_log("WISP",'I',"PROCFRELOCK","Processing %s Statement.",o_parms[0]);

	if (!strcmp(o_parms[0],"FREE"))							/* Look for FREE.			*/
	{
		is_comit = 0;								/* Not a COMMIT statement.		*/
		ptype = get_param(o_parms[0]);						/* get next parameter (should be ALL)	*/
	}
	else
		is_comit = 1;								/* Assume it's a COMMIT statement.	*/

	set_lock(-1,12);

	tput_line("           CONTINUE");

	if (ptype == -1)								/* Had a period, all done!		*/
	{
		tput_clause(12, ".");
		return(0);
	}

	if (is_comit)
	{
		stredt(linein," COMMIT"," ");						/* Remove COMMIT.			*/
	}
	else
	{
		stredt(linein," FREE "," ");						/* Remove FREE.				*/
		stredt(linein," ALL"," ");						/* Remove ALL.				*/
	}

	ptype = get_param(o_parms[0]);							/* Looking for a possible ON ERROR.	*/

	if (!strcmp(o_parms[0],"ON"))							/* See if it's an ON keyword.		*/
	{
		write_log("WISP",'I',"ONERR","ON ERROR phrase found.");
		ptype = get_param(o_parms[0]);						/* Get ERROR phrase.			*/
		tput_line("                   MOVE \"000\" TO WISPRETURNCODE\n");
		tput_line("                   IF WISPRETURNCODE = \"000\" THEN\n");
		tput_line("                       CONTINUE\n");
		tput_line("                   ELSE\n");
		stredt(linein," ON "," ");						/* Remove ON				*/
		stredt(linein," ERROR"," ");						/* Remove ERROR				*/
	}

	hold_line();									/* Hold line, then quit.		*/
}

/*
**	History:
**	$Log: wt_free.c,v $
**	Revision 1.10  1996-08-30 21:56:19-04  gsl
**	drcs update
**
**
**
*/
