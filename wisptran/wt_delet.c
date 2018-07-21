/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

/*
**	File:		wt_delet.c
**
**	Project:	WISP/TRAN
**
**	Purpose:	DELETE verb
**			REWRITE verb
**
**			The DELETE and REWRITE verbs are together in this file 
**			because their processing is almost identical.
**
**	Routines:	
**	parse_delete()
**	parse_rewrite()
*/

/*
**	Includes
*/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"
#include "wt_locks.h"
#include "wt_datad.h"
#include "crt.h"

#define OP_REWRITE 1
#define OP_DELETE 2

static NODE parse_local_common(NODE the_statement, NODE the_sentence, int op);

/*
**	DELETE file-mame [RECORD]
**		[INVALID key statement-1]
**		[NOT INVALID key statement-2]
**		[END-DELETE]
*/
NODE parse_delete(NODE the_statement, NODE the_sentence)
{
	NODE	verb_node, file_node;
	int	fnum;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"DELETE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected DELETE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	file_node = verb_node->next;
	
	fnum = file_index(token_data(file_node->token));
	if (-1 == fnum)
	{
		write_tlog(file_node->token,"WISP",'E',"NOTFOUND",
				  "Error -- File %s, referenced by DELETE statement but not Declared.",
				  token_data(file_node->token));
	}

	return parse_local_common(the_statement, the_sentence, OP_DELETE);
}


/*
**	REWRITE record-name-1 [FROM identifier-1]
**		[INVALID KEY imperative-statement-1]
**		[NOT INVALID KEY imperative-statement-2]
**		[END-REWRITE]
**
*/
NODE parse_rewrite(NODE the_statement, NODE the_sentence)
{
	NODE	verb_node, record_node;
	int	crt_num, fnum;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"REWRITE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected REWRITE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	record_node = verb_node->next;

	/*
	**	If it is a CRT record call parse_rewrite_crt()
	*/
	for (crt_num=0; crt_num<crt_record_count; crt_num++)
	{
		if (0 == strcmp(crt_record[crt_num], token_data(record_node->token)))
		{
			return parse_rewrite_crt(crt_num, the_statement, the_sentence);
		}
	}

	fnum = fd_record(token_data(record_node->token));
	if (-1 == fnum)
	{
		write_tlog(record_node->token,"WISP",'E',"NOTFOUND",
				  "REWRITE [%s], unknown record name.",
				  token_data(record_node->token));
	}
	
	return parse_local_common(the_statement, the_sentence, OP_REWRITE);
}

/*
**	ROUTINE:	parse_local_common()
**
**	FUNCTION:	Handle the common parts of DELETE and REWRITE.
**
**	DESCRIPTION:	These are standard VERB's with possible INVALID and END-xxx clauses.
**			The only thing special to handle is the record locking logic.
**			If opt_manual_locking then ensure there is an END-xxxx clause.
**
**	ARGUMENTS:	
**	the_statement	The first fragment
**	the_sentence 	The rest of the sentence
**	op		the operation REWRITE or DELETE
**
**	GLOBALS:	None
**
**	RETURN:		Next statement fragment
**
**	WARNINGS:	None
**
*/
static NODE parse_local_common(NODE the_statement, NODE the_sentence, int op)
{
	int	col;
	char	end_verb[40];
	NODE	trailing_fluff_node = NULL;
	NODE	curr_node, verb_node;
	int	need_end_verb = 0;

	verb_node = first_token_node(the_statement);

	sprintf(end_verb, "END-%s",token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_leading_fluff(the_statement);
	if (opt_nogetlastfileop)
	{
		if (op == OP_REWRITE)
		{
			/* tput_line_at(col,"MOVE \"RW\" TO WISP-DECLARATIVES-STATUS"); */
			tput_line_at(col,"MOVE \"Rewrite\" TO WISP-LASTFILEOP");
		}
		else
		{
			/* tput_line_at(col,"MOVE \"DE\" TO WISP-DECLARATIVES-STATUS"); */
			tput_line_at(col,"MOVE \"Delete\" TO WISP-LASTFILEOP");
		}
		tput_flush();
	}
	if (!opt_manual_locking)
	{
		clear_lock_holder_id(col);
	}


	/*
	**	Need to process the trailing fluff because the opt_manual_locking 
	**	inserts logic at the end (before the trailing fluff).
	*/
	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	tput_statement(col, the_statement);

	the_statement = free_statement(the_statement);
	verb_node = NULL;

	/*
	**	Check for INVALID KEY clause.
	**
	**	NOTE: VAX had some additional logic to unlock a record on a failed delete.
	*/
	the_statement = get_statement_from_sentence(the_sentence);

	curr_node = the_statement->next;
	
	if (eq_token(curr_node->token, KEYWORD, "INVALID"))
	{
		need_end_verb = 1;
		
		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

		curr_node->token->column_fixed = 1;
		tput_statement(curr_node->token->column, the_statement);
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
		need_end_verb = 1;

		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

		curr_node->token->column_fixed = 1;
		tput_statement(curr_node->token->column, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	if (eq_token(curr_node->token, KEYWORD, end_verb))
	{
		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

		curr_node->token->column_fixed = 1;
		tput_statement(curr_node->token->column, the_statement);
		the_statement =  free_statement(the_statement);
	}
	else
	{
		if (need_end_verb && opt_manual_locking)
		{
			/*
			**	Insert an END-xxx if not already there so the locking
			**	logic doesn't end up in an INVALID KEY clause.
			*/
			tput_clause(col,  end_verb);
		}
		
	}

	if (opt_manual_locking)
	{
		/*
		**	For opt_manual_locking we have to unlock the record after the statement.
		*/
		unlock_record(col);
	}
	
	if (trailing_fluff_node)
	{
		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);
	}

	return the_statement;
}


/*
**	History:
**	$Log: wt_delet.c,v $
**	Revision 1.20  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.19  2003/03/06 21:47:10  gsl
**	Change WISP-DECLARATIVES-STATUS to WISP-LASTFILEOP
**	
**	Revision 1.18  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.17  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.16  2002/05/16 21:48:32  gsl
**	getlastfileop logic
**	
**	Revision 1.15  1998-06-09 13:14:28-04  gsl
**	Combined DELETE and REWRITE logic into a common routine.
**	Added support for manual record locking
**
**	Revision 1.14  1998-03-27 14:10:56-05  gsl
**	Move OLD to old.c
**
**	Revision 1.13  1998-03-03 11:17:24-05  gsl
**	Add fluff logic
**	Add column_fixed logic for invalid clauses
**
**	Revision 1.12  1998-02-20 18:07:08-05  gsl
**	Add handling INVALID clauses
**
**	Revision 1.11  1998-02-11 12:01:56-05  gsl
**	re-write to use new statement parsing logic
**
**	Revision 1.10  1996-08-30 21:56:16-04  gsl
**	drcs update
**
**
**
*/
