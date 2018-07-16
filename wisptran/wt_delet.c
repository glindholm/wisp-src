			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
#ifdef MSDOS
#include <string.h>
#endif

#define EXT extern
#include "wisp.h"

p_delete()
{
	int	i,j,hold,fnum,inv_key,n_alt;
	char	tstr[132];
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

	put_line		("           MOVE \"DE\" TO WISP-DECLARATIVES-STATUS\n");
	if (vax_cobol)	put_line("           MOVE \"N\" TO WISP-TEST-BYTE\n");
	put_line("          ");								/* start a new line			*/

	inv_key = 0;									/* no INVALID KEY yet...		*/
	lock_clause = 0;

	do
	{
		put_line(" ");
		write_line("%s",o_parms[0]);						/* output parm				*/

		if (o_parms[0][0]) stredt(inline,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			put_line("\n");							/* end the previous line		*/
			put_line("          ");  					/* start a new line 			*/
		}

		if (!strcmp(o_parms[0],"INVALID"))					/* INVALID KEY phrase?			*/
		{
			stredt(inline," INVALID"," ");					/* remove INVALID			*/
			if (ptype != -1)
			{
				peek_param(o_parms[7]);					/* get "KEY"				*/
				if (!strcmp(o_parms[7],"KEY"))				/* Was it KEY?				*/
				{
					ptype = get_param(o_parms[0]);			/* Get it, and remove it.		*/
					stredt(inline," KEY"," ");			/* remove KEY				*/
				}
			}

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'W',"BADINVKEY",
				"Bad DELETE syntax, INVALID KEY followed by a period.");
				o_parms[0][0] = 0;
			}
			else
			{
				ptype = get_param(o_parms[0]);				/* what to do?				*/
			}

			put_line        ("\n               INVALID KEY ");		/* write it out				*/
			if (vax_cobol)
			{	
				put_line("\n               MOVE \"Y\" TO WISP-TEST-BYTE");
				put_line("\n           END-DELETE");
			}
			inv_key = 1;							/* flag it				*/
		}
	}	while ((ptype != -1) && !keyword(o_parms[0],proc_keywords));		/* do till we hit a LAST parm or keyword*/


	put_line("\n");									/* finish this line			*/

	if (ptype == -1)								/* a LAST parm				*/
	{
		write_line("                   %s\n",o_parms[0]); 
	}

	if ( vax_cobol )
	{
		if (inv_key)
		{
			put_line  ("           IF WISP-TEST-BYTE = \"N\" THEN\n");
			write_line("               MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fnum]);
			write_line("               UNLOCK %s ALL\n", fdname );
			write_line("               MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fnum]);
			put_line  ("           ELSE\n");
		}
		else
		{
			write_line("           MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fnum]);
			write_line("           UNLOCK %s ALL\n", fdname );
			write_line("           MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fnum]);
		}
	}

	if (ptype == -1)
	{
		put_line  ("           CONTINUE.\n");
	}



	if (ptype != -1) hold_line();

	write_log("WISP",'I',"DELETEDONE","Completed DELETE analysys");

}




