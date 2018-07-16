			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* Routines to process the START statements.										*/

#define EXT extern
#include "wisp.h"

p_start()										/* Process START statements.		*/
{
	int i,j,hold,fnum,inv_key,n_alt,is_crt,strt_equal;
	char tstr[132];
	char t_mktemp[40];
	short	keeplooping;
	int	lock_clause;

	write_log("WISP",'I',"PROCSTART","Processing START Statement.");

	ptype = get_param(o_parms[0]);							/* get next parameter (should be START)	*/
	j = peek_param(o_parms[1]);							/* peek at the file name		*/

											/* is it a crt record?			*/
	is_crt = 0;
	for (i=0; i<crt_fcount; i++) 
	{
		if (!strcmp(crt_file[i],o_parms[1]))	
		{
			is_crt = 1; 
			cur_crt = i;
		}
	}

	if (is_crt)
	{
		stredt(inline," START "," ");						/* Remove start.			*/
		stredt(inline,o_parms[1],"CONTINUE");					/* Replace crt file with n-s.		*/
		put_line(inline);							/* Output the line.			*/
		return(0);
	}
	else
	{
		lock_clause = 0;
		i = 0;
		had_read = 1;								/* Flag it.				*/
		o_parms[5][0] = '\0';							/* no status field yet			*/
		strt_equal = 1;								/* Assume KEY EQUALS phrase.		*/

		fnum = file_index(o_parms[1]);

		if (fnum == -1)								/* no file matched, error		*/
		{
			write_log("WISP",'F',"STARTFNF",
			"Error -- File %s, referenced by START statement but not Declared.",o_parms[1]);
			exit_wisp(-1);
		}
		else
		{
			write_log("WISP",'I',"HANDLESTART","Handling START of %s.",prog_files[fnum]);
			strcpy(o_parms[5],prog_fstats[fnum]);				/* copy the status field		*/
		}

		if (trap_start)
		{
			write_line("           MOVE \"%s\" TO\n",o_parms[1]);
			put_line  ("               WISP-CURRENT-FILE-ID\n");
		}

		if (copy_para || in_decl)
			write_log("WISP",'W',"STARTINDECL","START statement in DECLARATIVES, record locking not applied.");


		put_line("           MOVE \"ST\" TO WISP-DECLARATIVES-STATUS\n");
		put_line("           START");						/* start a new line			*/


		hold = 0;								/* no hold yet...			*/
		inv_key = 0;								/* no INVALID KEY yet...		*/

		keeplooping = 1;
		do
		{
			if (o_parms[0][0]) stredt(inline,o_parms[0],"");		/* Remove it from the input line	*/
			ptype = get_param(o_parms[0]);					/* get a new parm....			*/
			if (ptype == -1) keeplooping = 0;

			if (ptype == 1)							/* first parm on a new line		*/
			{
				put_line("\n");						/* end the previous line		*/
				put_line("              ");  				/* start a new line 			*/
			}

			if (!strcmp(o_parms[0],"KEY"))					/* handle KEY.				*/
			{

				put_line(" KEY");					/* output parm				*/

				i = strpos(inline," KEY");				/* Remove the KEY word.			*/
				stredt(inline," KEY","");
				ptype = get_param(o_parms[0]);				/* get the key name.			*/
				if (ptype == -1) keeplooping = 0;

				if (strpos(" IS EQUAL = GREATER > NOT LESS < ",o_parms[0]) == -1) /* It must be a variable	*/
				{
					strcpy(templine," ");
					strcat(templine,o_parms[0]);

					if (ptype == 1) 				/* First on line?			*/
					{
						i = 0;
						put_line("\n");				/* end the previous line		*/
						put_line("          ");  		/* start a new line 			*/
					}

					stredt(&inline[i],templine,"");			/* remove the field name		*/
					write_log("WISP",'I',"STARTFLDDEL","Field name %s removed from START.",o_parms[0]);
					o_parms[0][0] = '\0';				/* Clear it out.			*/
				}
				else
				{
					write_log("WISP",'I',"NOMODS","No mods made to START.");
					if (strpos(" GREATER > NOT LESS < ",&inline[i]) != -1) /* It's not an EQUAL test.	*/
						strt_equal = 0;				/* Clear the flag			*/
					if (ptype == 1)					/* first parm on a new line		*/
					{
						put_line("\n");				/* end the previous line		*/
						put_line("          ");  		/* start a new line 			*/
					}

				}
			}
			else if (!strcmp(o_parms[0],"INVALID"))				/* INVALID KEY phrase?			*/
			{
				int	add_cont;
				add_cont = 0;
				stredt(inline," INVALID"," ");				/* remove INVALID			*/
				if (ptype != -1)
				{
					peek_param(o_parms[7]);				/* get "KEY"				*/
					if (!strcmp(o_parms[7],"KEY"))			/* Was it KEY?				*/
					{
						ptype = get_param(o_parms[0]);		/* Get it, and remove it.		*/
						if (ptype == -1) keeplooping = 0;
						stredt(inline," KEY"," ");		/* remove KEY				*/
					}
				}

				if (ptype == -1)					/* Premature period!			*/
				{
					keeplooping = 0;
					write_log("WISP",'W',"BADINVKEY",
					"Bad START syntax, INVALID KEY followed by a period.");
					o_parms[0][0] = '\0';
					add_cont = 1;
				}
				else
				{
					ptype = get_param(o_parms[0]);			/* what to do?				*/
					if (ptype == -1) keeplooping = 0;
				}

				if ( vax_cobol )
				{
					put_line("\n               REGARDLESS OF LOCK");
					lock_clause = 1;
				}
				put_line        ("\n               INVALID KEY"); 	/* write it out				*/
				if (add_cont) 
					put_line("\n               CONTINUE");		/* Invalid key followed by period	*/ 

				inv_key = 1;						/* flag it				*/
			}

			if (keyword(o_parms[0],proc_keywords))				/* If we hit a keyword	terminate loop	*/
			{
				keeplooping = 0;
			}
			else
			{
				/*
					Output the param
				*/

				key_name(o_parms[0],0);					/* Do key name translation		*/

				write_line(" %s",o_parms[0]);				/* output parm				*/

			}

		} while (keeplooping);		/* do till we hit a LAST parm or keyword*/


		if ( vax_cobol && !lock_clause )
		{
			put_line("\n               REGARDLESS OF LOCK");
			lock_clause = 1;
		}

		if (ptype == -1) put_line(".\n"); 				/* a LAST parm				*/
		else             put_line("\n");				/* finish this line			*/

		if (ptype != -1) hold_line();

		write_log("WISP",'I',"STARTDONE","Completed START analysys");
	}

}

