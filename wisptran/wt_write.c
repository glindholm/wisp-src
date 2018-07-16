static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"

p_write()
{
	int 	i,j,fnum,inv_key;
	int	lock_clause;
	char	recname[40];
	int	fd;
	char	fdname[40];

	write_log("WISP",'I',"PROCWRITE","Processing WRITE Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be WRITE)	*/
	j = peek_param(o_parms[1]);							/* peek at the record name		*/

	strcpy(recname, o_parms[1]);
	if (strchr(recname,'.')) *((char *)strchr(recname,'.')) = 0;			/* save the record name			*/

	fd = fd_record( recname );
	if (fd == -1)
	{
		write_log("WISP",'E',"WRITE","WRITE Unknown Record Name");
		strcpy(fdname,"????");
		fd=0;
	}
	else
	{
		strcpy(fdname, prog_files[fd]);
	}
	
	i = 0;
	o_parms[5][0] = '\0';								/* no status field yet			*/

	fnum = -1;									/* set fnum to error status		*/

	inv_key = 0;									/* no INVALID KEY yet...		*/
	lock_clause = 0;								/* no ALLOWING clause yet		*/

	if (vax_cobol && (prog_ftypes[fd] & AUTOLOCK))					/* If using automatic record locking	*/
	{										/* then don't supply an "ALLOWING" 	*/
		lock_clause = 1;							/* clause. (Trick it into not writing	*/
	}										/* the ALLOWING clause.)		*/

	tput_line("           MOVE \"WR\" TO WISP-DECLARATIVES-STATUS\n");

	if (vax_cobol && !(prog_ftypes[fd] & SEQ_FILE))
	{
		tput_line("           MOVE \"N\" TO WISP-TEST-BYTE\n");
	}

	tput_flush();

	do
	{
		tput_clause(12, "%s",o_parms[0]);					/* output parm				*/

		if (o_parms[0][0]) stredt(linein,o_parms[0],"");			/* Remove it from the input line	*/
		ptype = get_param(o_parms[0]);						/* get a new parm....			*/

		if (ptype == 1)								/* first parm on a new line		*/
		{
			tput_flush();
		}

		if (!strcmp(o_parms[0],"BEFORE") || !strcmp(o_parms[0],"AFTER"))	/* Look for BEFORE or AFTER ADVANCING.	*/
		{
			strcpy(o_parms[9],o_parms[0]);
			stredt(linein,o_parms[9]," ");					/* remove the word			*/
			ptype = get_param(o_parms[0]);					/* get "ADVANCING"			*/

			if (!strcmp(o_parms[0],"ADVANCING"))				/* Was it?				*/
			{
				stredt(linein," ADVANCING","");				/* remove it.				*/
				ptype = get_param(o_parms[0]);				/* Get it.				*/
			}

			if ( vax_cobol && !lock_clause )
			{
				tput_line("               ALLOWING NO OTHERS");
				lock_clause = 1;
			}

			if (fig_count)							/* There is a list of 2 byte fig cons.	*/
			{
				i = 0;
				do
				{
					if (!strcmp(o_parms[0],fig_cons[i]))		/* Is it one of them?			*/
					{
						if (fig_val[i][0] & 0x40)
							strcpy(o_parms[9],"BEFORE");	/* Check before/after bit.		*/
						else
							strcpy(o_parms[9],"AFTER");
						sprintf(o_parms[0],"%d",fig_val[i][1] & 0x7f);	/* Number of lines to skip	*/
						break;
					}
				} while(++i < fig_count);
			}

											/* write it out				*/
			tput_line("                    %s ADVANCING",o_parms[9]);

		}
		else if (!strcmp(o_parms[0],"INVALID"))					/* INVALID KEY phrase?			*/
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

			if ( vax_cobol && !lock_clause )
			{
				tput_line("               ALLOWING NO OTHERS");
				lock_clause = 1;
			}

			if (ptype == -1)						/* Premature period!			*/
			{
				write_log("WISP",'I',"BADINVKEY",
					"Bad WRITE syntax, INVALID KEY followed by a period.");
				o_parms[0][0] = 0;
			}
			else
			{
				ptype = get_param(o_parms[0]);				/* what to do?				*/
			}

			tput_line        ("               INVALID KEY ");		/* write it out				*/
			if (vax_cobol)
			{	
				tput_line("               MOVE \"Y\" TO WISP-TEST-BYTE");
				tput_line("           END-WRITE");
			}
			else
			{
				if (!strcmp(o_parms[0],"ELSE"))				/* INVALID KEY followed by ELSE		*/
				{
					tput_line("               CONTINUE ");
					tput_line("           END-WRITE ");
				}
			}
			inv_key = 1;							/* flag it				*/
		}
		else if (!strcmp(o_parms[0],"TIMEOUT"))
		{
			write_log("WISP",'E',"TIMEOUT",	"Write %s with TIMEOUT not supported.",o_parms[1]);

		}
	}	while ((ptype != -1) && !proc_keyword(o_parms[0]));			/* do till we hit a LAST parm or keyword*/


	if (ptype == -1)								/* a LAST parm				*/
	{
		tput_line("                   %s\n",o_parms[0]); 
	}

	if ( vax_cobol && !lock_clause )
	{
		tput_line("               ALLOWING NO OTHERS\n");
		lock_clause = 1;
	}



	if ( vax_cobol )
	{

		if (inv_key)
		{
			tput_line("           IF WISP-TEST-BYTE = \"N\" THEN\n");
			tput_line("               MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fd]);
			tput_line("               UNLOCK %s\n", fdname );
			tput_line("               MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fd]);
			tput_line("           ELSE\n");
			if (!strcmp(o_parms[0],"ELSE"))
			{
				tput_line("               CONTINUE\n");
				tput_line("           END-IF\n");
			}
		}
		else if ( !(prog_ftypes[fd] & AUTOLOCK) )
		{
			tput_line("           MOVE %s TO WISP-SAVE-FILE-STATUS\n",prog_fstats[fd]);
			tput_line("           UNLOCK %s\n", fdname );
#ifdef OLD
			if ( !(prog_ftypes[fd] & SEQ_FILE) || (prog_ftypes[fd] & SEQ_DYN) ) 
				tput_line("           UNLOCK %s\n", fdname );
			else
				tput_line("           UNLOCK %s ALL\n", fdname );
#endif
			tput_line("           MOVE WISP-SAVE-FILE-STATUS TO %s\n",prog_fstats[fd]);
		}
	}

	if (ptype == -1)
	{
		tput_line  ("           CONTINUE.\n");
	}

	if (ptype != -1) hold_line();

	write_log("WISP",'I',"WRITEDONE","Completed WRITE analysys");
	return 0;
}

/*
**	History:
**	$Log: wt_write.c,v $
**	Revision 1.10  1996-08-30 21:56:26-04  gsl
**	drcs update
**
**
**
*/
