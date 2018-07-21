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



/* Routines to process the FREE ALL and COMMIT statements.									*/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"

/*
**	FREE ALL
**		[on ERROR statement-1]
**		[NOT on ERROR statement-2]
**		[END-FREE]
**
**
**      PERFORM WISP-FREE-ALL
**
**      (on ERROR clause)
**      MOVE "000" TO WISP-RETURN-CODE
**      IF WISP-RETURN-CODE NOT = "000" THEN
**		statement-1
**	END-IF
**
**      (NOT on ERROR clause)
**      MOVE "000" TO WISP-RETURN-CODE
**      IF WISP-RETURN-CODE  = "000" THEN
**		statement-2
**	END-IF
**
*/
NODE parse_free(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"FREE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected FREE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_leading_fluff(the_statement);

	curr_node = verb_node->next;
	if (!eq_token(curr_node->token,KEYWORD,"ALL"))
	{
		write_tlog(curr_node->token,"WISP",'E',"PARSE","Expecting ALL found [%s].", token_data(curr_node->token));

		tput_statement(col, the_statement);
		return free_statement(the_statement);
	}

	if (!opt_gen_dms_locking && acu_cobol) /* /NODMS option */
	{
		write_log("WISP",'I',"FREEALL","FREE ALL changed to UNLOCK ALL.");
		/* tput_scomment("*** FREE ALL changed to UNLOCK ALL. ***"); */

		edit_token(verb_node->token, "UNLOCK");
	}
	else
	{
		/*
		**      PERFORM WISP-FREE-ALL
		*/

		edit_token(verb_node->token, "PERFORM");
		edit_token(curr_node->token, "WISP-FREE-ALL");
	}
	
	tput_statement(col, the_statement);
	the_statement = free_statement(the_statement);

	lock_clear_para = 1;

	/*
	**	Check for ON ERROR clause.
	*/

	the_statement = get_statement_from_sentence(the_sentence);

	curr_node = the_statement->next;
	
	if (eq_token(curr_node->token, KEYWORD, "ON") ||
	    eq_token(curr_node->token, KEYWORD, "ERROR"))
	{
		the_statement =  free_statement(the_statement);

		tput_line_at(col, "MOVE \"000\" TO WISP-RETURN-CODE");
		tput_line_at(col, "IF WISP-RETURN-CODE NOT = \"000\" THEN");

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		tput_line_at(col, "END-IF");

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	/*
	**	Check for NOT ON ERROR clause.
	*/

	if (eq_token(curr_node->token, KEYWORD, "NOT"))
	{
		the_statement =  free_statement(the_statement);

		tput_line_at(col, "MOVE \"000\" TO WISP-RETURN-CODE");
		tput_line_at(col, "IF WISP-RETURN-CODE = \"000\" THEN");

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		tput_line_at(col, "END-IF");

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	if (eq_token(curr_node->token, KEYWORD, "END-FREE"))
	{
		free_token_from_node(curr_node);	/* Remove END-FREE */

		decontext_statement(the_statement);
		tput_statement(col, the_statement);
		return free_statement(the_statement);
	}
	
	return the_statement;
}

/*
**	COMMIT [identifier-1 | literal-1]
**	   [on ERROR statement-1]
**	   [NOT on ERROR statement-2]
**	   [END-COMMIT] 
**
**	If identifier-1 or literal-1 is specified then this is a "transaction level" 
**	commit and is not supported.
*/
NODE parse_commit(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	int	col;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"COMMIT"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected COMMIT found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_leading_fluff(the_statement);

	curr_node = verb_node->next;
	if (NODE_END != curr_node->type)
	{
		/*
		**	Transaction level commits have no equivalence and are not supported.
		*/
		write_tlog(curr_node->token,"WISP",'E',"COMMIT","Deleting transaction COMMIT, not supported.");
		tput_scomment("*** Deleted COMMIT %s. ", token_data(curr_node->token));
		the_statement = free_statement(the_statement);
		curr_node = verb_node = NULL;
	}
	else if (acu_cobol && opt_x4dbfile)
	{
		/* Leave it as a COMMIT */
	}
	else if (!opt_gen_dms_locking) /* /NODMS option */
	{
		if (acu_cobol) 
		{
			write_log("WISP",'W',"COMMIT","COMMIT changed to UNLOCK ALL.");
			tput_scomment("*** COMMIT changed to UNLOCK ALL. ***");

			edit_token(verb_node->token, "UNLOCK ALL");
		}
		else 
		{
			write_log("WISP",'E',"UNSUPPORTED","Verb [%s] is not supported.",token_data(verb_node->token));
			tput_scomment("****** WISP: Verb [%s] is not supported. ******",token_data(verb_node->token));
		}
	}
	else
	{
		edit_token(verb_node->token, "PERFORM WISP-FREE-ALL");
	}
	
	decontext_statement(the_statement);
	tput_statement(col, the_statement);
	the_statement = free_statement(the_statement);

	lock_clear_para = 1;

	/*
	**	Check for ON ERROR clause.
	*/

	the_statement = get_statement_from_sentence(the_sentence);

	curr_node = the_statement->next;

	if (eq_token(curr_node->token, KEYWORD, "ON") ||
	    eq_token(curr_node->token, KEYWORD, "ERROR"))
	{
		the_statement =  free_statement(the_statement);

		tput_line_at(col, "MOVE \"000\" TO WISP-RETURN-CODE");
		tput_line_at(col, "IF WISP-RETURN-CODE NOT = \"000\" THEN");

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		tput_line_at(col, "END-IF");

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	/*
	**	Check for NOT ON ERROR clause.
	*/

	if (eq_token(curr_node->token, KEYWORD, "NOT"))
	{
		the_statement =  free_statement(the_statement);

		tput_line_at(col, "MOVE \"000\" TO WISP-RETURN-CODE");
		tput_line_at(col, "IF WISP-RETURN-CODE = \"000\" THEN");

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		tput_line_at(col, "END-IF");

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}
	
	if (eq_token(curr_node->token, KEYWORD, "END-COMMIT"))
	{
		free_token_from_node(curr_node);

		decontext_statement(the_statement);
		tput_statement(col, the_statement);
		return free_statement(the_statement);
	}

	return the_statement;
}

/*
**	History:
**	$Log: wt_free.c,v $
**	Revision 1.19  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.18  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.17  2002/08/13 18:45:49  gsl
**	Change FREE ALL to an informational message
**	
**	Revision 1.16  2002/07/30 19:12:39  gsl
**	SETRETCODE
**	
**	Revision 1.15  2002/07/29 21:13:24  gsl
**	
**	Revision 1.14  1998/12/03 20:44:25  gsl
**	In parse_commit() if ACU4GL then leave it a COMMIT instead of
**	changing to an UNLOCK ALL
**	
**	Revision 1.13  1998-03-27 14:11:25-05  gsl
**	Move OLD to old.c
**
**	Revision 1.12  1998-03-03 10:45:16-05  gsl
**	Handle fluff logic
**
**	Revision 1.11  1998-02-20 16:59:22-05  gsl
**	Finished FREE and added COMMIT
**
**	Revision 1.10  1996-08-30 21:56:19-04  gsl
**	drcs update
**
**
**
*/
