/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/


/* Routines to process the START statements.										*/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "token.h"
#include "node.h"
#include "reduce.h"
#include "statment.h"
#include "wt_procd.h"


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
**			if it was found.   Also add "CONTINUE" to 
**			the INVALID KEY clause if followed by a period.
**
**			WCB:	START file-name-1 [KEY [data-name-1] {IS = ...} data-name-2] 
**
**			COB:	MOVE "Start" TO WISP-LASTFILEOP
**				START file-name-1 [KEY {IS = ...} data-name-1]
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
NODE parse_start(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node, file_node;
	int	col;
	NODE 	data_name_1_node = NULL;
	NODE 	data_name_2_node = NULL;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"START"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected START found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing START Statement.");

	tput_leading_fluff(the_statement);

	col = verb_node->token->column;

	file_node = verb_node->next;
	curr_node = file_node->next;

	if (crt_index(token_data(file_node->token)) != -1)
	{
		/*
		**	Replace a START crt-file with a CONTINUE.
		*/

		if (opt_native_screens)		/* START CRT - WARNING */
		{
			write_log("WISP",'W',"NATIVESCREENS","Workstation START %s is removed for Native Screens",
				  token_data(file_node->token));
			tput_scomment("*>>> Workstation START removed for Native Screens.");
			tput_scomment("*    START %s",token_data(file_node->token));
		}

		edit_token(verb_node->token,"CONTINUE");
		edit_token(file_node->token,"");
		tput_statement(col,the_statement);
		return free_statement(the_statement);
	}

	if (eq_token(curr_node->token,KEYWORD,"KEY"))
	{
		/*
		**	START file-name KEY data-name-1 IS = data-name-2 
		**	START file-name KEY IS = data-name-2
		*/
		curr_node = curr_node->next;
		if (curr_node->token && IDENTIFIER == curr_node->token->type)
		{
			data_name_1_node = reduce_data_item(curr_node);
			curr_node->down = NULL;
			curr_node = curr_node->next;
		}
	}

	/*
	**	
	**	START file-name [KEY clause] [INVALID ...] [END-START]
	**	START file-name [KEY clause] [NOT INVALID ...] [END-START]
	*/
	while( curr_node && NODE_END != curr_node->type )
	{
		if (curr_node->token && IDENTIFIER == curr_node->token->type)
		{
			char	buff[80];

			data_name_2_node = reduce_data_item(curr_node);
			if (!data_name_2_node)
			{
				write_tlog(curr_node->token, "WISP",'E',"START","Unable to reduce data-name-2 [%s]",
					   token_data(curr_node->token));
			}
			else
			{
				if (data_name_1_node)
				{
					if (0!=strcmp(token_data(data_name_1_node->token),token_data(data_name_2_node->token)))
					{
						write_tlog(data_name_2_node->token,"WISP",'E',"START",
							   "Possible error translating KEY clause:" 
							   "data-name-1 [%s] is NOT data-name-2 [%s] using data-name-2",
							   token_data(data_name_1_node->token),
							   token_data(data_name_2_node->token));
					}
				}
				
				strcpy(buff,token_data(data_name_2_node->token));
				key_name(buff,0);					/* Do key name translation		*/
				if (0 != strcmp(buff,token_data(data_name_2_node->token)))
				{
					edit_token(data_name_2_node->token,buff);
				}
			}
		}

		curr_node = curr_node->next;
	}

	if (data_name_1_node)
	{
		write_log("WISP",'I',"STARTFLDDEL","Field name %s removed from START.",
			  token_data(data_name_1_node->token));
		data_name_1_node = free_statement(data_name_1_node);
	}

	if (opt_nogetlastfileop)
	{
		/* tput_line_at(col,"MOVE \"ST\" TO WISP-DECLARATIVES-STATUS"); */
		tput_line_at(col,"MOVE \"Start\" TO WISP-LASTFILEOP");
		tput_flush();
	}

	tput_statement(col,the_statement);
	the_statement = free_statement(the_statement);


	/*
	**	Check for INVALID KEY clause.
	*/

	the_statement = get_statement_from_sentence(the_sentence);

	curr_node = the_statement->next;
	
	if (eq_token(curr_node->token, KEYWORD, "INVALID"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	/*
	**	Check for NOT INVALID KEY clause.
	*/

	if (eq_token(curr_node->token, KEYWORD, "NOT"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	if (eq_token(curr_node->token, KEYWORD, "END-START"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);
	}

	return the_statement;
}

/*
**	History:
**	$Log: wt_start.c,v $
**	Revision 1.23  2003/09/08 19:43:27  gsl
**	Change log entries for Native Screens
**	
**	Revision 1.22  2003/08/08 19:52:46  gsl
**	Add native screens comments
**	
**	Revision 1.21  2003/08/06 18:12:10  gsl
**	
**	Revision 1.20  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.19  2003/03/06 21:47:10  gsl
**	Change WISP-DECLARATIVES-STATUS to WISP-LASTFILEOP
**	
**	Revision 1.18  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.17  2002/06/20 23:05:31  gsl
**	remove obsolete code
**	
**	Revision 1.16  2002/05/16 21:53:21  gsl
**	getlastfileop logic
**	
**	Revision 1.15  1998-03-03 15:16:06-05  gsl
**	Changed flush to fixed
**
**	Revision 1.14  1998-03-02 13:21:24-05  gsl
**	Update for cobol-85
**
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
