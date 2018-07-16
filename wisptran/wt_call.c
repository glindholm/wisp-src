			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* Subroutine to process CALL statements in Wang COBOL. */


#define EXT extern
#include "wisp.h"

#define PTR1	((char *)1)
#define CALL_BUFFER_SIZE	256

static int add_wvaset;

p_call()
{
	int i,itype;
	char buf[CALL_BUFFER_SIZE];
	char tstr[80],src[40];

	if (vax_cobol || acu_cobol) add_wvaset = 0;
	else add_wvaset = 1;

	track_call(parms[1],xref_ptr);							/* Track it.				*/


											/* If the subroutine is a VARARGS type	*/
											/* call, just call "_p_call(buf)" and	*/
											/* It will be taken care of.		*/
											/* If it requires editing, call		*/
											/* "get_call(sub_name, new_name, repl_kw*/
											/* buf);				*/
											/* Where:				*/
											/* sub_name - is the original name of	*/
											/*            the subroutine. ex. PAUSE	*/
											/* new_name - The name to use instead.	*/
											/*	      ex. WPAUSE.		*/
											/* repl_kw -  A keyword to replace in	*/
											/* 	      the parm list. A dummy 	*/
											/* 	      field will be put in its 	*/
											/*	      place.			*/
											/*	      Usefull for UFB's.	*/
											/* buf -      A buffer to use.		*/
											/* It returns the number of parms found.*/
											/* Then call put_call(buf,num_args);	*/
	if (!strcmp(parms[1],"\"COBLINK\""))
	{										/* call COBLINK 			*/
		write_log("WISP",'I',"COBLINKFOUND","CALL to COBLINK Detected.");
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"LINK\""))
	{										/* call LINK 				*/
		write_log("WISP",'I',"LINKFOUND","CALL to LINK Detected.");
		has_link = 1;								/* Flag it.				*/
		if (vax_cobol && do_dlink)
		{
			write_log("WISP",'I',"LINKDESC","Call \"LINK\" changed to \"LINKDESC\".");
			strcpy(src,parms[1]);
			i = get_call(src,"\"LINKDESC\"",NULL,buf);			/* Count and fixup the line.		*/
			stredt(buf,"USING","USING BY DESCRIPTOR");
			put_call(buf,i);
		}
		else if (aix_cobol)
		{
			write_log("WISP",'I',"LINKAIX","Call \"LINK\" changed to \"LINKAIX\".");
			strcpy(src,parms[1]);
			i = get_call(src,"\"LINKAIX\"",NULL,buf);			/* Count and fixup the line.		*/
			put_callmflink(buf,i);
		}
		else if (mf_cobol || dmf_cobol)
		{
			write_log("WISP",'I',"LINKMF","Call \"LINK\" changed to \"LINKMF\".");
			strcpy(src,parms[1]);
			i = get_call(src,"\"LINKMF\"",NULL,buf);				/* Count and fixup the line.		*/
			put_callmflink(buf,i);
		}
		else 
		{
			_p_call(buf);							/* Really do it.			*/
		}
	}
	else if (!strcmp(parms[1],"\"WSXIO\""))
	{										/* call WSXIO				*/
		write_log("WISP",'I',"WSXIOCALL","Call to WSXIO detected.");
		strcpy(src,parms[1]);
		i = get_call(src,src,PTR1,buf);						/* Count and fixup the line.		*/
		put_call(buf,i);
	}
	else if (!strcmp(parms[1],"\"SETFILE\"") || !strcmp(parms[1],"\"FILESET\""))
	{										/* call SETFILE.			*/
		strcpy(src,parms[1]);
		itype = get_call(src,src,NULL,buf);					/* Count and fixup the line.  		*/

		for (i = name_count-1; i > 0; i--)					/* Move names forward 3			*/
		{
			strcpy(name_list[i+3],name_list[i]);				/* Copy each one.			*/
		}									/* But skip the first one.		*/
		name_count = name_count+3;						/* Count them.				*/

		i = findfile(name_list[0]);						/* search for the file			*/

		if (-1 == i)
		{
			write_log("WISP",'F',"SETFILE","SETFILE call, file %s not found.",name_list[0]);
			exit_wisp(-1);
		}


		if ((prog_vnames[i][0] == '"') || (!prog_vnames[i][0]))			/* Volume name is a literal/null 	*/
		{
			make_fld(name_list[0],prog_files[i],"V-");			/* put the V on				*/
		}
		else
		{
			strcpy(name_list[0],prog_vnames[i]);				/* Use the actual field.		*/
		}

		if ((prog_lnames[i][0] == '"') || (!prog_lnames[i][0]))			 /* Library name is a literal/null	 */
		{
			make_fld(name_list[1],prog_files[i],"L-");			/* put the L on				*/
		}
		else
		{
			strcpy(name_list[1],prog_lnames[i]);				/* Use the actual field.		*/
		}

		if ((prog_fnames[i][0] == '"') || (!prog_fnames[i][0]))			/* file name is a literal/null		*/
		{
			make_fld(name_list[2],prog_files[i],"F-");			/* put the F on				*/
		}
		else
		{
			strcpy(name_list[2],prog_fnames[i]);				/* Use the actual field.		*/
		}
		make_fld(name_list[3],prog_files[i],"S-");				/* put the S on				*/

		put_call(buf,itype);
	}
	else if (!strcmp(parms[1],"\"SCREEN\""))
	{										/* call SCREEN				*/
		write_log("WISP",'I',"SCREENCALL","Call to SCREEN detected.");
		strcpy(src,parms[1]);
		i = get_call(src,src,PTR1,buf);						/* Count and fixup the line.		*/
		put_call(buf,i);
	}
	else if (!strcmp(parms[1],"\"PAUSE\""))
	{										/* call PAUSE				*/
		write_log("WISP",'I',"PAUSEFIX","PAUSE call altered.");
		strcpy(src,parms[1]);
		i = get_call(src,"\"wpause\"",NULL,buf);					/* Count and fixup the line.		*/
		put_call(buf,i);
	}
	else if (!strcmp(parms[1],"\"RENAME\""))
	{										/* call RENAME				*/
		if (add_wvaset)
		{
			strcpy(src,parms[1]);
			i = get_call(src,"\"wrename\"",NULL,buf);				/* Count and fixup the line.		*/
			write_line("                   MOVE %d TO WISP-LONGWORD\n",name_count);
			put_line  ("                   CALL \"wvaset\" USING WISP-LONGWORD\n");	/* Set the varargs for UNIX.	*/
			put_call(buf,i);						/* Write out the buffer now.		*/
			write_log("WISP",'I',"RENAMEFIX","RENAME call altered.");
		}
		else
		{
			stredt(inline,"RENAME","wrename");				/* Fix the name.			*/
			put_line(inline);						/* No processing needed.		*/
		}
	}
	else if (!strcmp(parms[1],"\"CHECKACP\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"FIND\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"GETPARM\""))
	{
		if (!vax_cobol)
		{

			strcpy(src,parms[1]);

			i = get_call(src,src,"",buf);					/* Count and fixup the line.		*/

			if ( name_count < 63 )
			{
				if (add_wvaset)
				{
											/* Set the varargs for UNIX.		*/
					write_line("                   MOVE %d TO WISP-LONGWORD\n",name_count);

					put_line  ("                   CALL \"wvaset\" USING WISP-LONGWORD\n");
				}
				put_call(buf,i);					/* Write out the buffer now.		*/
			}
			else								/* Too many parms for LPI, do this up.	*/
			{
				_put_getparm(buf,i);					/* Do it for GETPARM.			*/
			}
			write_log("WISP",'I',"GETPARMFIX","GETPARM call altered.");
		}
		else
		{
			_p_call(buf);							/* Normal for VMS			*/
		}
	}
	else if (!strcmp(parms[1],"\"MESSAGE\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"LINKPROC\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"EXTRACT\""))
	{
		put_line("           CALL \"setprogid\" USING WISP-APPLICATION-NAME\n"); /* Move name of current appl.		*/
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"FILECOPY\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"PRINT\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"PUTPARM\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"READACP\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"READFDR\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SCRATCH\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SET\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SEARCH\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SUBMIT\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"UPDATFDR\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SORT\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SORTINFO\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SORTLINK\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"STRING\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"WSFNS\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"WSFNM\""))
	{
		_p_call(buf);								/* Really do it.			*/
	}
	else if (!strcmp(parms[1],"\"SPECLIO\""))
	{										/* call SPECLIO				*/
		ptype = get_param(o_parms[1]);
		i = get_ppos();

		memset(tstr,' ',i);
		tstr[i] = '\0';

		skip_param(2);								/* skip CALL SPECLIO USING		*/
		ptype = get_param(o_parms[1]);						/* Get the file name.			*/

		sprintf(inline,"%sOPEN SPECIAL-INPUT %s",tstr,o_parms[1]);
		if (strlen(inline) > 72)						/* Too long, fix it.			*/
		{
			sprintf(inline,"           OPEN SPECIAL-INPUT %s",tstr,o_parms[1]);
		}
		ptype = get_param(o_parms[1]);						/* Get the parameter name.		*/

		if (ptype == -1)
			strcat(inline,".\n");
		else
			strcat(inline,"\n");
		hold_line();								/* Hold the line to be re-parmed.	*/
		write_log("WISP",'I',"SPECLIOFIX","SPECLIO call altered.");
	}
	else if (!strcmp(parms[1],"\"@WSOPEN\""))
	{										/* call WSOPEN 				*/
		skip_param(5);								/* skip CALL WSOPEN USING, CRT, STATUS	*/
		ptype = get_param(o_parms[1]);
		if (ptype == -1)
			put_line("             CONTINUE.\n");
		else
			put_line("             CONTINUE,\n");
		write_log("WISP",'I',"WSOPENFIX","WSOPEN call altered.");
	}
	else if (!strcmp(parms[1],"\"@WSCLOSE\""))
	{										/* Call WSCLOSE.			*/
		put_line("                 CALL \"vwang\" USING VWANG-CLOSE-WS,\n");
		skip_param(4);
		ptype = get_param(o_parms[1]);
		if (ptype == -1)
			put_line("             CONTINUE.\n");
		else
			put_line("             CONTINUE,\n");
		write_log("WISP",'I',"WSCLOSEFIX","WSCLOSE call altered.");
	}
	else if (!strcmp(parms[1],"\"@WSTIME\""))
	{										/* call WSTIME 				*/
		skip_param(3);								/* skip CALL @WSTIME USING		*/

		do
		{
			ptype = get_param(o_parms[0]);					/* get parms and see if they are	*/
		}									/* last or a keyword			*/
		while ((ptype != -1) && !keyword(o_parms[0],proc_keywords));
		if (keyword(o_parms[0],proc_keywords))					/* was a keyword			*/
			hold_line();							/* save it				*/

		if (ptype != -1)
			put_line("                   CONTINUE,\n");
		else
			put_line("                   CONTINUE.\n");

		write_log("WISP",'I',"WSTIMEFIX","WSTIME call altered.");
	}
	else if (!strcmp(parms[1],"\"@MSNAME\""))
	{										/* call MSNAME				*/
		stredt(inline,"@MSNAME","dummy");
		put_line(inline);
		write_log("WISP",'I',"MSNAMEFIX","MSNAME call altered.");
	}
	else if (!strcmp(parms[1],"\"@MSTIME\""))
	{										/* call MSTIME 				*/
		stredt(inline,"@MSTIME","dummy");
		put_line(inline);
		write_log("WISP",'I',"MSTIMEFIX","MSTIME call altered.");
	}
	else if (!strcmp(parms[1],"\"@MSVALUE\""))
	{										/* call MSVALUE 			*/
		stredt(inline,"@MSVALUE","dummy");
		put_line(inline);
		write_log("WISP",'I',"MSVALUEFIX","MSVALUE call altered.");
	}
	else if (using_ufb_test(parms[1]))
	{
		write_log("WISP",'I',"USINGUFB","Call to a USING-UFB routine detected.");
		strcpy(src,parms[1]);
		i = get_call(src,src,PTR1,buf);						/* Count and fixup the line.		*/
		put_call(buf,i);
	}
	else
	{
		put_line(inline);
	}

}

/*
	get_call:	This routine loads up a CALL "program" USING args.. into buf.  In the process it substitutes the
			program name (sname) with the translated name (tname).  It also searches the arguments for subval
			and replaces any found with WISP-SCRATCH-BYTE-1.  This is useful for relacing UFB parameters.
			Subval can be NULL -- no substitution.  Subval==1 means workstation UFBs.

*/
static int get_call(sname,tname,subval,buf)						/* Count and fixup the line.		*/
char *sname;										/* The subroutine name in quotes.	*/
char *tname;										/* The translated name in quotes.	*/
char *subval;										/* A field name to substitute for.	*/
char *buf;										/* The call buffer			*/
{
	char *tptr;
	char *svbuf;
	int rtype;
	int	substituted;

	rtype = 0;									/* Return type (ptype) is 0 for now.	*/
	svbuf = buf;
	name_count = 0;									/* Set count of names = 0.		*/

	skip_param(3);									/* skip CALL "SCREEN" USING		*/
	tptr = "           CALL ";
	while (*tptr) *buf++ = *tptr++;							/* Append the call			*/
	tptr = tname;
	while (*tptr) *buf++ = *tptr++;							/* Append the name			*/
	tptr = " USING\n";
	while (*tptr) *buf++ = *tptr++;							/* Append the USING			*/
	*buf = '\0';									/* null terminate the buffer.		*/

	do
	{
		ptype = get_param(o_parms[0]);						/* get first parm			*/
		if (ptype == -1 && !o_parms[0][0])					/* A detached period!!			*/
		{									/* Just skip it.			*/
			rtype = ptype;							/* Save the type.			*/
		}
		else if (keyword(o_parms[0],proc_keywords))				/* is it a keyword?			*/
		{									/* it is keyword, quit parsing		*/
			ptype = 9;							/* signal keyword found			*/
		}
		else
		{
			if (name_count == MAX_NAME_LIST)
			{
				write_log("WISP",'F',"CALLTOOLONG","Maximum CALL buffer size exceeded.");
				exit_wisp(-1);
			}

			substituted = 0;
			if (subval)							/* test for argument substitution	*/
			{
				if ( 1==(long)subval )					/* test UFB substitution		*/
				{
					if (-1 != findcrt(o_parms[0]) ||		/* test for CRT files			*/
					    -1 != findfile(o_parms[0])  )		/* test for non-CRT files		*/
					{
						substituted = 1;
					}
				}
				else if ( !strcmp(o_parms[0],subval) )			/* test for explicit substitution	*/
				{
					substituted = 1;
				}

				if (substituted)					/* Do the substitution			*/
				{
					strcpy(name_list[name_count],"WISP-SCRATCH-BYTE-1");
				}
			}

			if (!substituted)
			{
				strcpy(name_list[name_count],o_parms[0]);		/* Save it in the list.			*/
				if (strpos(o_parms[0],"(") != -1)			/* It has a subscript.			*/
				{
					if (o_parms[0][0] == '(')			/* All by itself.			*/
					{
						name_count--;				/* It belongs on the previous item.	*/
						strcat(name_list[name_count],o_parms[0]);/* Add it on.				*/
					}

					while(strpos(o_parms[0],")") == -1)		/* Copy till we see the closing paren.	*/
					{
						ptype = get_param(o_parms[0]);		/* Get another parm.			*/
						strcat(name_list[name_count],o_parms[0]);/* Add it on.				*/
					}
				}
			}
			name_count++;							/* Count it.				*/
			rtype = ptype;							/* Save return type.			*/
		}
	} while ((ptype != -1) && (ptype != 9));					/* quit if period (or keyword)		*/


	if ((buf - svbuf) > CALL_BUFFER_SIZE)
	{
		write_log("WISP",'F',"CALLTOOLONG","Maximum CALL buffer size exceeded.");
		exit_wisp(-1);
	}

	if (ptype == 9) hold_line();							/* keyword found, save the line		*/

	return(rtype);									/* Return the count.			*/
}

static track_call(pname,tptr)								/* Keep track of the modules called.	*/
char *pname;
c_list *tptr;
{
	int i;
	char tstr[40];

	if (!do_xref) return(0);							/* Skip it.				*/

	if (tptr == xref_ptr && pname[0] != '"') return(0);				/* First level call with no ""		*/

	strcpy(tstr,pname);

	stredt(tstr,"\"","");								/* Remove the quotes.			*/
	stredt(tstr,"\"","");

	if (tptr->call_count)								/* Scan the list.			*/
	{
		for (i=0; i<tptr->call_count; i++)
		{
			if (!strcmp(tptr->call_list[i],tstr))
			{
				tptr->ref_count[i]++;					/* Now count it.			*/
				break;
			}
		}

		if (i == tptr->call_count)						/* It's a new one, add it.		*/
		{
			if (tptr->call_count == 100)					/* Check the next list			*/
			{
				if (!tptr->next_list)					/* No next list, make one.		*/
				{
					tptr->next_list = (struct c_list *)malloc(sizeof(c_list)); /* Alloc the mem.		*/
					tptr = tptr->next_list;
					tptr->next_list = 0;
					tptr->call_count = 0;
				}
				track_call(pname,tptr);					/* Re-call the procedure.		*/
			}
			else
			{
				strcpy(tptr->call_list[tptr->call_count],tstr);		/* Save it.				*/
				tptr->ref_count[tptr->call_count] = 1;
				tptr->call_count++;
			}
		}
	}
	else										/* New list.				*/
	{
		strcpy(tptr->call_list[tptr->call_count],tstr);				/* Save it.				*/
		tptr->ref_count[tptr->call_count] = 1;
		tptr->call_count++;
	}
}

static _p_call(buf)									/* General count and process of a call	*/
char *buf;
{
	int i,j;
	char tstr[80],src[40];

	if (add_wvaset)
	{
		strcpy(src,parms[1]);

		j = get_call(src,src,NULL,buf);						/* Count and fixup the line.		*/
		write_line("                   MOVE %d TO WISP-LONGWORD\n",name_count);

		put_line  ("                   CALL \"wvaset\" USING WISP-LONGWORD\n");	/* Set the varargs for UNIX.		*/
		put_call(buf,j);
	}
	else
	{
		put_line(inline);
	}
}

put_call(buf,rtype)
char *buf;
int rtype;
{
	int i;

	put_line(buf);									/* Write out the call buffer now.	*/

	for (i = 0; i < (name_count - 1); i++)						/* Now do the list of parms.		*/
	{
		write_line("                   %s\n",name_list[i]);
	}

	if (rtype == -1)								/* Need a period?			*/
	{
		write_line("                   %s.\n",name_list[i]);
	}
	else
	{
		write_line("                   %s\n",name_list[i]);			/* Don't need a period.			*/
	}
}

static put_callmflink(buf,rtype)
char *buf;
int rtype;
{
	int i;

	write_line	  ("           MOVE %d TO WISP-LONGWORD\n",name_count);
	put_line  	  ("           CALL \"wvaset\" USING WISP-LONGWORD\n");	/* Set the varargs for UNIX.	*/

	put_line(buf);									/* Write out the call buffer now.	*/

	for (i = 0; i < name_count; i++)						/* Now do the list of parms.		*/
	{
		if (i > 0 ) put_line("\n");
		write_line("               REFERENCE       %s\n",name_list[i]);
		write_line("               VALUE LENGTH OF %s",name_list[i]);
	}

	if (rtype == -1) put_line(".\n");						/* Need a period?			*/
	else		 put_line("\n");						/* Don't need a period.			*/

}

static _put_getparm(buf,rtype)
char *buf;
int rtype;
{
	int i,j;
	int plmt;

	if      (lpi_cobol) plmt = 62;
	else if (acu_cobol) plmt = 62;
	else if (aix_cobol) plmt = 62;
	else if (mf_cobol)  plmt = 62;
	else if (dmf_cobol) plmt = 62;
	else                plmt = 32;

	for (	i = 0, j = plmt;
		name_count != 0;)							/* Now do the list of parms.		*/
	{
		if (j == plmt)
		{
			if (add_wvaset)
			{
				write_line("                   MOVE %d TO WISP-LONGWORD\n",
								(name_count > plmt) ? plmt : name_count);

				put_line  ("                   CALL \"wvaset\" USING WISP-LONGWORD\n");
			}
			put_line  ("                   CALL \"getparmbuild\" USING\n");
			j = 0;
		}
		else
		{
			write_line("                        %s\n",name_list[i]);
			i++;
			j++;
			name_count--;
		}
	}

	if (add_wvaset)
	{
		write_line("                   MOVE 0 TO WISP-LONGWORD\n");

		put_line("                   CALL \"wvaset\" USING WISP-LONGWORD\n");
	}

	if (rtype == -1)								/* Need a period?			*/
	{
		put_line("                   CALL \"getparmbuild\".\n");
	}
	else
	{
		put_line("                   CALL \"getparmbuild\"\n");
	}
}

/*
	findcrt:	Search the list of crt files and returns the position in the list, -1 == not found
*/
int findcrt(name)
char	*name;
{
	int	i;

	for(i=0; i<crt_fcount; i++)
	{
		if (0==strcmp(crt_file[i],name)) 
		{
			return(i);							/* Found it in the list			*/
			break;
		}
	}

	return(-1);
}

/*
	findfile:	Search the list of files and returns the position in the list, -1 == not found
*/
int findfile(name)
char	*name;
{
	int	i;

	for(i=0; i<prog_cnt; i++)
	{
		if (0==strcmp(prog_files[i],name)) 
		{
			return(i);							/* Found it in the list			*/
			break;
		}
	}

	return(-1);
}
