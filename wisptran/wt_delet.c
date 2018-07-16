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

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"

p_delete()
{
	int	i,j,fnum,inv_key;
	char	fdname[40];
	int	lock_clause;

	write_log("WISP",'I',"PROCDELETE","Processing DELETE Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be Delete)*/
	j = peek_param(o_parms[1]);							/* peek at the file name		*/

	strcpy(fdname, o_parms[1]);
	if (strchr(fdname,'.')) *((char *)strchr(fdname,'.')) = '\0';			/* save the fdname			*/

	i = 0;
	o_parms[5][0] = '\0';								/* no status field yet			*/

	fnum = file_index(o_parms[1]);							/* see if file exists.			*/

	if (fnum == -1)
	{
		write_log("WISP",'F',"NOTFOUND","File \"%s\" not found while processing DELETE Statement.",o_parms[1]);
	}

	predelete_locking();								/* Add locking logic			*/

	tput_line_at		    (12, "MOVE \"DE\" TO WISP-DECLARATIVES-STATUS\n");
	if (vax_cobol)	tput_line_at(12, "MOVE \"N\" TO WISP-TEST-BYTE\n");
	tput_flush();
	
	inv_key = 0;									/* no INVALID KEY yet...		*/
	lock_clause = 0;

	do
	{
		tput_clause(12, "%s",o_parms[0]);					/* output parm				*/

		if (o_parms[0][0]) stredt(linein,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			tput_flush();
		}

		if (!strcmp(o_parms[0],"INVALID"))					/* INVALID KEY phrase?			*/
		{
			stredt(linein," INVALID"," ");					/* remove INVALID			*/
			if (ptype != -1)
			{
				peek_param(o_parms[7]);					/* get "KEY"				*/
				if (!strcmp(o_parms[7],"KEY"))				/* Was it KEY?				*/
				{
					ptype = get_param(o_parms[0]);			/* Get it, and remove it.		*/
					stredt(linein," KEY"," ");			/* remove KEY				*/
				}
			}

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'I',"BADINVKEY",
				"Bad DELETE syntax, INVALID KEY followed by a period.");
				o_parms[0][0] = 0;
			}
			else
			{
				ptype = get_param(o_parms[0]);				/* what to do?				*/
			}

			tput_line_at(16, "INVALID KEY");				/* write it out				*/
			if (vax_cobol)
			{	
				tput_line_at(16, "MOVE \"Y\" TO WISP-TEST-BYTE");
				tput_line_at(12, "END-DELETE");
			}
			inv_key = 1;							/* flag it				*/
		}
	}	while ((ptype != -1) && !proc_keyword(o_parms[0]));			/* do till we hit a LAST parm or keyword*/


	if (ptype == -1)								/* a LAST parm				*/
	{
		tput_line_at(20, "%s",o_parms[0]); 
	}

	if ( vax_cobol )
	{
		if (inv_key)
		{
			tput_line("           IF WISP-TEST-BYTE = \"N\" THEN");
			tput_line("               MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fnum]);
			tput_line("               UNLOCK %s", fdname );
			tput_line("               MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fnum]);
			tput_line("           ELSE");
		}
		else
		{
			tput_line("           MOVE %s TO WISP-SAVE-FILE-STATUS",prog_fstats[fnum]);
			tput_line("           UNLOCK %s", fdname );
			tput_line("           MOVE WISP-SAVE-FILE-STATUS TO %s",prog_fstats[fnum]);
		}
	}

	if (ptype == -1)
	{
		tput_line("           CONTINUE.");
	}



	if (ptype != -1) hold_line();

	write_log("WISP",'I',"DELETEDONE","Completed DELETE analysys");
	return 0;

}




/*
**	History:
**	$Log: wt_delet.c,v $
**	Revision 1.10  1996-08-30 21:56:16-04  gsl
**	drcs update
**
**
**
*/
