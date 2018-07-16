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

/* Routines to process the START statements.										*/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "token.h"
#include "node.h"
#include "reduce.h"
#include "statment.h"


/*
**	Routine:	parse_start()
**
**	Function:	To process the START statement.
**
**	Description:	Change the Wang START syntax to the target COBOL syntax.
**			The main difference is on Wang the data-name-1 before the relational operator specifies
**			the key to use and if not specified the primary key is used.  Data-name-2 is usually
**			the same as data-name-1 but can be any data item that starts at the same byte location
**			as the active key (data-name-1 or primary).  This data-name-2 is not allowed in ANSI COBOL.
**			To translate the KEY clause we assume that data-name-1 == data-name-2 and delete data-name-1
**			if it was found.  On VAX we add the "REGARDLESS OF LOCK" clause.  Also add "CONTINUE" to 
**			the INVALID KEY clause if followed by a period.
**
**			WCB:	START file-name-1 [KEY [data-name-1] {IS = ...} data-name-2] 
**
**			COB:	MOVE "ST" TO WISP-DECLARATIVES-STATUS
**				START file-name-1 [KEY {IS = ...} data-name-1]
**
**			VAX:	START file-name-1 [KEY {IS = ...} data-name-1] REGARDLESS OF LOCK
**
**	Arguments:	
**	the_statement	The START statement
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	09/20/93	Written by GSL
**
*/
parse_start(the_statement)
NODE the_statement;
{
	NODE	curr_node, start_node, file_node, data_name_1_node, invalid_node;
	int	col,fnum;

	write_log("WISP",'I',"PROCSTART","Processing START Statement.");

	data_name_1_node = NULL;
	invalid_node = NULL;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"START"))
	{
		write_log("WISP",'W',"NOTSTART","NOT A START Statement. STATEMENT NOT PROCESSED");
		tput_statement(12,the_statement);
		return(0);
	}

	start_node = curr_node;
	col = start_node->token->column;

	curr_node = curr_node->next;
	file_node = curr_node;

	if (crt_index(token_data(file_node->token)) != -1)
	{
		/*
		**	Replace a START crt-file with a CONTINUE.
		*/

		if (acn_cobol)
		{
			write_log("WISP",'W',"NATIVE","Workstation START %s is removed for Native Screens",
				  token_data(file_node->token));
			tput_scomment("*>>> Workstation START removed for Native Screens.");
			tput_scomment("*    START %s",token_data(file_node->token));
		}

		write_log("WISP",'I',"STARTCRT","START crt-file replaced with CONTINUE.");
		edit_token(start_node->token,"CONTINUE");
		edit_token(file_node->token,"");
		tput_statement(12,the_statement);
		return(0);
	}

	fnum = file_index(token_data(file_node->token));

	if (fnum == -1)								/* no file matched, error		*/
	{
		write_log("WISP",'F',"STARTFNF",
			"Error -- File %s, referenced by START statement but not Declared.",token_data(file_node->token));
		exit_wisp(EXIT_WITH_ERR);
	}

	write_log("WISP",'I',"HANDLESTART","Handling START of %s.",prog_files[fnum]);

	if (trap_start)
	{
		tput_line_at(col, "MOVE \"%s\" TO",token_data(file_node->token));
		tput_clause (col+4, "WISP-CURRENT-FILE-ID");
	}

	if (copy_to_dcl_file || in_decl)
	{
		write_log("WISP",'W',"STARTINDECL","START statement in DECLARATIVES, record locking not applied.");
	}

	tput_line_at(col,"MOVE \"ST\" TO WISP-DECLARATIVES-STATUS");
	tput_flush();

	if (eq_token(curr_node->next->token,KEYWORD,"KEY"))
	{
		/*
		**	START file-name KEY data-name-1 IS = data-name-2 
		**	START file-name KEY IS = data-name-2
		*/
		curr_node = curr_node->next;
		if (curr_node->next->token && IDENTIFIER == curr_node->next->token->type)
		{
			curr_node = curr_node->next;
			data_name_1_node = curr_node;
			reduce_data_item(data_name_1_node);
		}

		if (data_name_1_node)
		{
			write_log("WISP",'I',"STARTFLDDEL","Field name %s removed from START.",
				token_data(data_name_1_node->down->token));
			free_statement(data_name_1_node->down);
			curr_node->down = NULL;
		}
	}

	/*
	**	Now look for the place to add the "REGARDLESS OF LOCK" clause, it goes after the KEY clause
	**	and before the INVALID clause.  Also find INVALID clause.
	**	
	**	START file-name [KEY clause] [INVALID ...] [END-START]
	**	START file-name [KEY clause] [NOT INVALID ...] [END-START]
	*/
	while( curr_node->next && NODE_END != curr_node->next->type )
	{
		if (IDENTIFIER == curr_node->next->token->type)
		{
			char	buff[80];

			strcpy(buff,token_data(curr_node->next->token));
			key_name(buff,0);					/* Do key name translation		*/
			if (0 != strcmp(buff,token_data(curr_node->next->token)))
			{
				edit_token(curr_node->next->token,buff);
			}
		}

		if (PERIOD == curr_node->next->token->type)
		{
			break;
		}

		if ( eq_token(curr_node->next->token,KEYWORD,"END-START") )
		{
			break;
		}

		if ( eq_token(curr_node->next->token,KEYWORD,"INVALID") )
		{
			invalid_node = curr_node->next;
			break;
		}

		if ( eq_token(curr_node->next->token,KEYWORD,"NOT") &&
		     eq_token(curr_node->next->next->token,KEYWORD,"INVALID") )
		{
			invalid_node = curr_node->next->next;
			break;
		}

		curr_node = curr_node->next;
	}

	if (vax_cobol && !(prog_ftypes[fnum] & AUTOLOCK))
	{
		/*
		**	add REGARDLESS OF LOCK clause (tie down in reverse order)
		*/
		tie_down(curr_node,maketoknode(make_token(KEYWORD,"LOCK")));
		tie_down(curr_node,maketoknode(make_token(KEYWORD,"OF")));
		tie_down(curr_node,maketoknode(make_token(KEYWORD,"REGARDLESS")));
	}

	if (invalid_node)
	{
		/*
		**	... INVALID.
		**	... INVALID KEY.
		*/
		if ( eq_token(invalid_node->next->token,KEYWORD,"KEY") )
		{
			invalid_node = invalid_node->next;
		}

		if (invalid_node->next && invalid_node->next->token && 
		    PERIOD == invalid_node->next->token->type)
		{
			write_log("WISP",'I',"BADINVKEY","Bad START syntax, INVALID KEY followed by a period.");
			tie_next(invalid_node,maketoknode(make_token(VERB,"CONTINUE")));
		}
	}

	tput_statement(col+4,the_statement);

	write_log("WISP",'I',"STARTDONE","Completed START statement.");

	return(0);
}

/*
**	History:
**	$Log: wt_start.c,v $
**	Revision 1.13  1997-09-15 13:56:51-04  gsl
**	Fix warning
**
**	Revision 1.12  1997-09-12 14:01:32-04  gsl
**	change native warning
**
**	Revision 1.11  1997-09-12 13:32:43-04  gsl
**	Add Native Screens warning for WS START
**
**	Revision 1.10  1996-08-30 21:56:26-04  gsl
**	drcs update
**
**
**
*/
