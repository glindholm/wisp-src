static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wt_call.c
**
**	Project:	wisp/tran
**
**	RCS:		$Source:$
**
**	Purpose:	Subroutine to process CALL statements in Wang COBOL.
**
**	Routines:	
*/

/*
**	Includes
*/

#include <stdlib.h>
#include <string.h>

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "wmalloc.h"
#include "reduce.h"
#include "statment.h"
#include "input.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

static int add_wvaset;		/* Do we need to add a call to wvaset() before the call flag */

/*
**	Static Function Prototypes
*/

static void put_wvaset(int argcnt, int col);
static void track_call(const char *pname, c_list *tptr);

/*
**	ROUTINE:	parse_call()
**
**	FUNCTION:	Handle a CALL statement.
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	the_statement	The CALL statement tree
**
**	GLOBALS:	
**	add_wvaset	Flag to add call to "wvaset"
**
**	RETURN:		NULL or the unprocessed statement
**
**	WARNINGS:	None
**
*/

NODE parse_call(NODE the_statement)
{
	NODE	call_node, program_node, temp_node, using_node, first_node, last_node;
	int	col;
	char	program_name[80];
	int	argcnt;
	int	cnt;
	int	found_period;

	call_node = the_statement->next;

	if (!eq_token(call_node->token,VERB,"CALL"))
	{
		write_log("WISP",'E',"CALL","Not a CALL statement.");
		return(the_statement);
	}

	write_log("WISP",'I',"CALL","Processing CALL %s statement.",token_data(call_node->next->token));

        add_wvaset = (vax_cobol || acu_cobol) ? 0 : 1;

	/* Get the column of the CALL token */
	col = call_node->token->column;

	/* Point to the program name node */
	program_node = call_node->next;

	/* Reduce the program name, it is usually a literal but can be a data name */
	reduce_data_item(program_node);

	/* Point to USING node */
	using_node = program_node->next;

	/* Reduce and count the arguments */
	argcnt = 0;
	found_period = 0;
	if ( eq_token(using_node->token,KEYWORD,"USING"))
	{
		for(temp_node=using_node->next; 
		    temp_node && NODE_END != temp_node->type && (temp_node->token && PERIOD != temp_node->token->type); 
		    temp_node=temp_node->next)
		{
			reduce_data_item(temp_node);
			argcnt++;
		}

		if (temp_node && temp_node->token && PERIOD == temp_node->token->type)
		{
			found_period = 1;
		}

		/* Point to the first argument */
		first_node = using_node->next;
	}

	if (program_node->down->token->type != LITERAL)
	{
		write_log("WISP",'I',"CALL","Non-literal CALL statement.");

		/*
		**	For a non-literal call always add a wvaset() because we don't
		**	know what is being called and it may need it.
		*/
		put_wvaset(argcnt,col);
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}

	/* Get the program name */
	strcpy(program_name,token_data(program_node->down->token));

	/* Track it for the cross reference table */
	track_call(program_name,xref_ptr);
	
	/*
	**	Take care of CALLs that need vaset() calls only
	*/
	if (0==strcmp(program_name,"\"CHECKACP\"") ||
	    0==strcmp(program_name,"\"COBLINK\"") ||
	    0==strcmp(program_name,"\"FILECOPY\"") ||
	    0==strcmp(program_name,"\"FIND\"") ||
	    0==strcmp(program_name,"\"LINKPROC\"") ||
	    0==strcmp(program_name,"\"MESSAGE\"") ||
	    0==strcmp(program_name,"\"PRINT\"") ||
	    0==strcmp(program_name,"\"PUTPARM\"") ||
	    0==strcmp(program_name,"\"READACP\"") ||
	    0==strcmp(program_name,"\"READFDR\"") ||
	    0==strcmp(program_name,"\"SCRATCH\"") ||
	    0==strcmp(program_name,"\"SCREEN\"") ||
	    0==strcmp(program_name,"\"SEARCH\"") ||
	    0==strcmp(program_name,"\"SET\"") ||
	    0==strcmp(program_name,"\"SORT\"") ||
	    0==strcmp(program_name,"\"SORTINFO\"") ||
	    0==strcmp(program_name,"\"SORTLINK\"") ||
	    0==strcmp(program_name,"\"STRING\"") ||
	    0==strcmp(program_name,"\"SUBMIT\"") ||
	    0==strcmp(program_name,"\"UPDATFDR\"") ||
	    0==strcmp(program_name,"\"WSFNM\"") ||
	    0==strcmp(program_name,"\"WSFNS\""))
	{
		if (acn_cobol)
		{
			if (0==strcmp(program_name,"\"SCREEN\""))
			{
				write_log("WISP",'W',"NATIVE","Call %s not supported with Native Screens",program_name);
			}
			else if (0==strcmp(program_name,"\"MESSAGE\""))
			{
				write_log("WISP",'W',"NATIVE",
					  "Features of %s are not compatible with Native Screens",program_name);
			}
			else if (0==strcmp(program_name,"\"WSFNM\"") ||
				 0==strcmp(program_name,"\"WSFNS\""))
			{
				write_log("WISP",'W',"NATIVE","Call %s uses WISP Screens",program_name);
			}
		}

		if (0==strcmp(program_name,"\"READFDR\""))
		{
			write_log("WISP",'W',"YEAR2000","Features of %s are not YEAR2000 compliant",program_name);
		}

		put_wvaset(argcnt,col);
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"BELL\"") && acn_cobol)
	{
		write_log("WISP",'W',"NATIVE","Call %s changed to DISPLAY OMITTED BELL.",program_name);

		/*
		**	Morph:	CALL    "BELL"  USING arg1	...
		**		|	|	|     |		|	
		**	into:	DISPLAY OMITTED BELL  (delete)	...
		*/
		if (1 == argcnt)
		{
			edit_token(call_node->token,"DISPLAY");
			edit_token(program_node->down->token,"OMITTED");
			edit_token(using_node->token,"BELL");
			free_statement(first_node->down);
			first_node->down = NULL;

			decontext_statement(the_statement);
		}
		else
		{
			write_log("WISP",'W',"CALL","Call %s invalid argument count (%d).", program_name, argcnt);
		}
		
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"DATE\"") ||
		 0==strcmp(program_name,"\"DAY\"")	)
	{
		write_log("WISP",'W',"YEAR2000","Features of %s are not YEAR2000 compliant",program_name);
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"EXTRACT\""))
	{
		write_log("WISP",'I',"CALL","Call %s proceeded by Call \"setprogid\".",program_name);
		tput_line_at(col, "CALL \"setprogid\" USING WISP-APPLICATION-NAME");
		tput_flush();
		put_wvaset(argcnt,col);
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"GETPARM\""))
	{
		/*
		**	CALL "GETPARM" with more then 62 args need to be split
		**	into multiple calls to "getparmbuild".
		*/

#define		MAX_GETPARM_ARGS	62

		if (vax_cobol || argcnt <= MAX_GETPARM_ARGS)
		{
			put_wvaset(argcnt,col);
			tput_statement(12,the_statement);
			return(free_statement(the_statement));
		}

		/*
		**	Split into multiple calls to "getparmbuild"
		*/
		write_log("WISP",'I',"CALL","Call %s split into multiple Call \"getparmbuild\".",program_name);

		/* Point at the first argument */
		first_node = using_node->next;

		/* Break the statement after the USING node */
		using_node->next = NULL;

		/* Throw away the beginning of the statement */
		free_statement(the_statement);

		while(argcnt > 0)
		{
			/*
			**	Find the first and last args in the chunk.
			*/
			for(temp_node=first_node, cnt=1; 
			    cnt<argcnt && cnt<MAX_GETPARM_ARGS; 
			    cnt++, temp_node=temp_node->next) {/* empty */};
			
			/* last_node now points at the last arg in the chunk */
			last_node = temp_node;

			/* temp_node points to the first arg in the next chunk */
			temp_node = last_node->next;

			/* break the statement after the last arg */
			last_node->next = NULL;


			/* print this chunk */
			put_wvaset(cnt,col);
			tput_line_at(col, "CALL \"getparmbuild\" USING");
			tput_flush();
			tput_statement(12, first_node);

			/* Free this used part of the statement */
			free_statement(first_node);

			/* Setup for next loop thru */
			first_node = temp_node;
			argcnt -= cnt;
		}

		/* Print the final zero arg call to getparmbuild */
		put_wvaset(0,col);
		tput_line_at(col, "CALL \"getparmbuild\"");

		/* Print what left of the statement, possible PERIOD plus fluff tokens */
		tput_statement(12, first_node);
		return(free_statement(first_node));
	}
	else if (0==strcmp(program_name,"\"LINK\""))
	{
		if (vax_cobol && do_dlink)
		{
			write_log("WISP",'I',"CALL","Call %s changed to \"LINKDESC\".",program_name);

			edit_token(program_node->down->token,"\"LINKDESC\"");
			edit_token(using_node->token,"USING BY DESCRIPTOR");
			tput_statement(12,the_statement);
			return(free_statement(the_statement));
		}
		else if (aix_cobol || mf_cobol || dmf_cobol)
		{
			write_log("WISP",'I',"CALL","Call %s changed to \"LINKMF\".",program_name);

			edit_token(program_node->down->token,"\"LINKMF\"");
			put_wvaset(argcnt,col);

			/* Point at the first argument */
			first_node = using_node->next;

			/* Break the statement after the USING node */
			using_node->next = NULL;

			/* Print the first part of the statement */
			tput_statement(12,the_statement);
			
			free_statement(the_statement);

			/*
			**	For each argument print:
			**
			**	REFERENCE arg1	VALUE LENGTH OF arg1
			**	REFERENCE arg2	VALUE LENGTH OF arg2
			*/
			for(temp_node = first_node, cnt=0; 
			    cnt<argcnt; 
			    cnt++, temp_node=temp_node->next)
			{
				decontext_statement(temp_node->down);
				delint_statement(temp_node->down);

				tput_flush();

				tput_clause(col+4, "REFERENCE");
				tput_statement(col+4, temp_node->down);

				tput_clause(col+4, "VALUE LENGTH OF");
				tput_statement(col+4, temp_node->down);
			}

			/* Print any trailing stuff */
			decontext_statement(temp_node);				
			tput_statement(col+4, temp_node);
			return(free_statement(first_node));
		}
		else
		{
			put_wvaset(argcnt,col);
			tput_statement(12,the_statement);
			return(free_statement(the_statement));
		}
	}
	else if (0==strcmp(program_name,"\"PAUSE\""))
	{
		write_log("WISP",'I',"CALL","Call %s changed to \"wpause\".",program_name);
		edit_token(program_node->down->token,"\"wpause\"");
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"RENAME\""))
	{
		write_log("WISP",'I',"CALL","Call %s changed to \"wrename\".",program_name);
		edit_token(program_node->down->token,"\"wrename\"");
		put_wvaset(argcnt,col);
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"SETFILE\""))
	{
		/*
		**	The first arg is a UFB which we convert into 4 args
		**	which are the Volume, Library, File, and Status names
		**
		**	CALL "SETFILE" USING MYFILE, x, y, ...
		**
		**	CALL "SETFILE" USING V-MYFILE, L-MYFILE, F-MYFILE, S-MYFILE, x, y, ...
		*/
		int	idx;
		char	ufb_name[80];
		char	ufb_vol[80], ufb_lib[80], ufb_file[80], ufb_status[80];
		
		write_log("WISP",'I',"CALL","Call %s corrected.",program_name);

		/* Point to the first arg - the UFB name */
		first_node = using_node->next;
		strcpy(ufb_name, token_data(first_node->down->token));
		idx = file_index(ufb_name);
		if (-1 == idx)
		{
			write_log("WISP",'F',"CALL","Call \"SETFILE\", file %s not found.",ufb_name);
			exit_wisp(EXIT_WITH_ERR);
		}

		if ((prog_vnames[idx][0] == '"') || (!prog_vnames[idx][0]))		/* Volume name is a literal/null 	*/
		{
			make_fld(ufb_vol,prog_files[idx],"V-");				/* put the V on				*/
		}
		else
		{
			strcpy(ufb_vol,prog_vnames[idx]);				/* Use the actual field.		*/
		}

		if ((prog_lnames[idx][0] == '"') || (!prog_lnames[idx][0]))		/* Library name is a literal/null	 */
		{
			make_fld(ufb_lib,prog_files[idx],"L-");				/* put the L on				*/
		}
		else
		{
			strcpy(ufb_lib,prog_lnames[idx]);				/* Use the actual field.		*/
		}

		if ((prog_fnames[idx][0] == '"') || (!prog_fnames[idx][0]))		/* file name is a literal/null		*/
		{
			make_fld(ufb_file,prog_files[idx],"F-");			/* put the F on				*/
		}
		else
		{
			strcpy(ufb_file,prog_fnames[idx]);				/* Use the actual field.		*/
		}

		make_fld(ufb_status,prog_files[idx],"S-");				/* put the S on				*/

		/* We are replacing the first arg with 4 args so add 3 to the argcnt */
		argcnt += 3;
		put_wvaset(argcnt,col);

		/* Print out the first part of the statement up to the USING */
		using_node->next = NULL;
		tput_statement(12,the_statement);
		free_statement(the_statement);
		
		/* Print the 4 generated args */
		tput_clause(col+4,"%s,", ufb_vol);
		tput_clause(col+4,"%s,", ufb_lib);
		tput_clause(col+4,"%s,", ufb_file);
		tput_clause(col+4,"%s,", ufb_status);

		/* Print the remaining args after the first one. */
		tput_statement(col+4,first_node->next);
		return(free_statement(first_node));
	}
	else if (0==strcmp(program_name,"\"SPECLIO\""))
	{
		/*
		**	CALL "SPECLIO" USING myfile
		**
		**	OPEN SPECIAL-INPUT myfile
		*/
		char	buf[80];
		
		write_log("WISP",'W',"CALL","Call %s can be replaced with OPEN SPECIAL-INPUT.",program_name);

		/* Point to the first arg - the UFB name */
		first_node = using_node->next;

		memset(buf,' ',12);
		sprintf(&buf[12], "OPEN SPECIAL-INPUT %s", token_data(first_node->down->token));
		if (found_period)
		{
			strcat(buf,".");
		}

		/*
		**	Hold this line so it can be re-processed
		*/
		hold_this_line(buf);

		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"WSCLOSE\"")  ||
		 0==strcmp(program_name,"\"EDLOAD\"")   ||
		 0==strcmp(program_name,"\"MENULOAD\"") ||
		 0==strcmp(program_name,"\"W4WAPI\"")     )
	{
		if (acn_cobol)
		{
			write_log("WISP",'W',"NATIVE","Call %s uses WISP Screens",program_name);
		}
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"@WSCLOSE\""))
	{
		write_log("WISP",'W',"CALL","@WSCLOSE can be replaced with a CLOSE crt statement.");
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"@WSOPEN\""))
	{
		write_log("WISP",'W',"CALL","@WSOPEN not required - it can be removed.");
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"@WSTIME\"") ||
		 0==strcmp(program_name,"\"@MSNAME\"") ||
		 0==strcmp(program_name,"\"@MSTIME\"") ||
		 0==strcmp(program_name,"\"@MSVALUE\""))
	{
		write_log("WISP",'W',"CALL","Call %s routine not supplied by WISP", program_name);
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else if (0==strcmp(program_name,"\"WSXIO\"") ||
		 using_ufb_test(program_name))
	{
		write_log("WISP",'I',"CALL","Call %s fixing UFB arguments.",program_name);

		if (acn_cobol)
		{
			if (0==strcmp(program_name,"\"WSXIO\""))
			{
				write_log("WISP",'W',"NATIVE","Call %s uses WISP Screens",program_name);
			}
		}

		/* Point to the first arg */
		first_node = using_node->next;

		/* Search the args for UFB names and replace */
		for(temp_node = first_node, cnt=0; 
		    cnt<argcnt; 
		    cnt++, temp_node=temp_node->next)
		{
			/*
			**	Test for CRT and non-CRT files
			*/
			if (-1 != crt_index (token_data(temp_node->down->token)) ||
			    -1 != file_index(token_data(temp_node->down->token))    )
			{
				edit_token(temp_node->down->token,"WISP-SCRATCH-BYTE-1");
			}
		}
		
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	else
	{
		tput_statement(12,the_statement);
		return(free_statement(the_statement));
	}
	
	
}

static void put_wvaset(int argcnt, int col)
{
	if (add_wvaset)
	{
		tput_line_at(col, "MOVE %d TO WISP-LONGWORD",argcnt);
		tput_line_at(col, "CALL \"wvaset\" USING WISP-LONGWORD");
		tput_flush();
	}
}

static void track_call(const char *pname, c_list *tptr)					/* Keep track of the modules called.	*/
{
	int i;
	char tstr[40];

	if (!do_xref) return;								/* Skip it.				*/

	if (tptr == xref_ptr && pname[0] != '"') return;				/* First level call with no ""		*/

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
					tptr->next_list = (struct c_list *)wmalloc(sizeof(c_list)); /* Alloc the mem.		*/
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


/*
**	History:
**	$Log: wt_call.c,v $
**	Revision 1.20  1997-09-30 10:56:18-04  gsl
**	Add headers to fix warnings
**
**	Revision 1.19  1997-09-24 15:47:07-04  gsl
**	Add YEAR2000 warnings for DATE, DAY, and READFDR.
**	Remove native screens warnings from GETPARM
**
**	Revision 1.18  1997-09-18 16:07:03-04  gsl
**	Fix the native screens warning messages
**
**	Revision 1.17  1997-09-18 15:36:29-04  gsl
**	Add more native screens warnings
**
**	Revision 1.16  1997-09-18 12:08:10-04  gsl
**	Removed all the old p_call() code.
**	Add Native Screens stuff.
**
**	Revision 1.15  1997-09-17 17:11:43-04  gsl
**	Complete rewrite to use the token/node logic
**
**	Revision 1.14  1997-09-12 15:49:50-04  gsl
**	fix native screens warnings
**
**	Revision 1.13  1997-09-12 14:00:08-04  gsl
**	Add warnings for Native Screens with SCREEN and WSXIO
**
**	Revision 1.12  1996-12-12 20:14:43-05  gsl
**	Fixed some OLD TIME wisp translator "crap".
**	The CALL processing logic was recognizing and doing special processing
**	on the following routines.  It now issues a warning and recommends an action
**	to fix.
**	"SPECLIO" was being changed to an OPEN SPECIAL-INPUT.
**	"@WSOPEN" was being removed
**	"@WSCLOSE" was being changed to a CLOSE crt
**	"@WSTIME" was being removed
**	"@MSNAME" was changed to "dummy"
**	"@MSVALUE" was changed to "dummy"
**	"@MSTIME" was changed to "dummy"
**
**	Revision 1.11  1996-06-24 11:11:49-07  gsl
**	remove unused data items
**	\.
**
**	Revision 1.10  1995-08-03 06:23:36-07  gsl
**	Fixed problem where a CALL with >500 args would exceed internal limit.
**	Changed name_list to carg_list and and malloc or realloc it as needed
**	so there is now no limit on the number of args a call can have.
**
**
**
*/
