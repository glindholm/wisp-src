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
	int on_err,is_comit;

	ptype = get_param(o_parms[0]);							/* get next parameter (should be FREE)	*/

	write_log("WISP",'I',"PROCFRELOCK","Processing %s Statement.",o_parms[0]);

	if (!strcmp(o_parms[0],"FREE"))							/* Look for FREE.			*/
	{
		is_comit = 0;								/* Not a COMMIT statement.		*/
		ptype = get_param(o_parms[0]);						/* get next parameter (should be ALL)	*/
	}
	else
		is_comit = 1;								/* Assume it's a COMMIT statement.	*/

	set_lock(-1,11);

	put_line("           CONTINUE");

	if (ptype == -1)								/* Had a period, all done!		*/
	{
		put_line(".\n");
		return(0);
	}
	else
		put_line("\n");								/* Terminate the line.			*/

	if (is_comit)
	{
		stredt(inline," COMMIT"," ");						/* Remove COMMIT.			*/
	}
	else
	{
		stredt(inline," FREE "," ");						/* Remove FREE.				*/
		stredt(inline," ALL"," ");						/* Remove ALL.				*/
	}

	ptype = get_param(o_parms[0]);							/* Looking for a possible ON ERROR.	*/

	if (!strcmp(o_parms[0],"ON"))							/* See if it's an ON keyword.		*/
	{
		write_log("WISP",'I',"ONERR","ON ERROR phrase found.");
		ptype = get_param(o_parms[0]);						/* Get ERROR phrase.			*/
		put_line("                   MOVE \"000\" TO WISPRETURNCODE\n");
		put_line("                   IF WISPRETURNCODE = \"000\" THEN\n                      CONTINUE\n");
		put_line("                   ELSE\n");
		stredt(inline," ON "," ");						/* Remove ON				*/
		stredt(inline," ERROR"," ");						/* Remove ERROR				*/
	}

	hold_line();									/* Hold line, then quit.		*/
}

