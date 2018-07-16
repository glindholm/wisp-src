static char copyright[]="Copyright (c) 1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wt_read.c
**
**	Project:	WISP/TRAN
**
**	RCS:		$Source:$
**
**	Purpose:	READ verb
**
*/

/*
**	Includes
*/

#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "crt.h"
#include "cobfiles.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"
#include "wt_locks.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

static NODE parse_read_crt(NODE the_statement, NODE the_sentence);


/*
**	READ file [NEXT|PREVIOUS] 
**                       record [with HOLD ] [INTO record]
**				[MODIFIABLE]
**				[ALTERED   ]
**		[KEY is dataitem]
**		[timeout_clause]
**		[fail_clause]
**		[notfail_clause]
**		[END-READ]
**
**	timeout_clause:	TIMEOUT of {data-item|number}[second|seconds]
**				[HOLDER-ID in data-item]
**				{NEXT SENTENCE|statement}
**
**	fail_clause:	{INVALID key|at END} statement
**
**	notfail_clause:	NOT fail_clause
**
**
**	Translation:
**
**     		MOVE "RD" TO WISP-DECLARATIVES-STATUS
**		MOVE "N" TO WISP-TEST-BYTE
**		COMPUTE WISP-FILE-TIMEOUT = 100 * (timeout-seconds)
**		MOVE "file" TO WISP-NEW-LOCK-ID
**		PERFORM WISP-LOCK-START THRU WISP-LOCK-END
**		PERFORM WITH TEST AFTER
**                  UNTIL (file-status IS NOT = "99")
**                  OR (WISP-FILE-TIMEOUT IS < 1)
**                 READ file [INTO record] [KEY clause]
**                    INVALID KEY
**                      MOVE "Y" TO WISP-TEST-BYTE
**                 END-READ
**                 CALL "wfwait" USING file-status WISP-FILE-TIMEOUT
**		END-PERFORM
**		IF ( file-status = "99" AND WISP-FILE-TIMEOUT IS < 1)
**		   MOVE SPACES TO WISP-LOCK-ID
**		   {timeout-statement}
**		ELSE
**		   IF WISP-TEST-BYTE = "Y" THEN
**		       MOVE SPACES TO WISP-LOCK-ID
**		       {invalid-statement}
**		   END-IF
**		END-IF
*/
NODE parse_read(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node, file_node;
	NODE	into_node = NULL;
	NODE	key_node = NULL;
	NODE	timeout_node = NULL;
	NODE	frag2_statement = NULL;
	NODE	frag3_statement = NULL;
	NODE	frag4_statement = NULL;
	NODE	last_statement = NULL;
	NODE	trailing_fluff_node = NULL;
	char	next_clause[10] = "";
	int	with_hold = 0;
	char	*before_into_lock = NULL;
	char	*after_into_lock = NULL;
	int	at_end_found = 0;
	int	not_at_end_found = 0;
	int	invalid_key_found = 0;
	int	not_invalid_key_found = 0;
	int	fnum;
	int	col;
	const char *file_status;
	int	timeout_clause = 0;
	int	timeout_next_sentence = 0;
	static int timeout_cnt = 0;
	cob_file *timeout_file_ptr = NULL;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"READ"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected READ found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	tput_leading_fluff(the_statement);

	file_node = verb_node->next;

	write_tlog(verb_node->token,"WISP",'I',"READ","Processing READ %s Statement.", token_data(file_node->token));

	if (-1 != crt_index(token_data(file_node->token)))
	{
		return parse_read_crt(the_statement, the_sentence);
	}

	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	/*
	**	READ disk-file
	*/
	fnum = file_index(token_data(file_node->token));

	if (fnum == -1)								/* no file matched, error		*/
	{
		write_log("WISP",'F',"READFNF",
			"Error -- File %s, referenced by READ statement but not Declared.",token_data(file_node->token));
		exit_wisp(EXIT_WITH_ERR);
	}
	file_status = prog_fstats[fnum];					/* save the status field		*/
	
	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;


	tput_line_at(col, "MOVE \"RD\" TO WISP-DECLARATIVES-STATUS");


	/*
	**	Step through the tokens gathering info and making changes.
	*/
	curr_node = file_node->next;

	if (eq_token(curr_node->token,KEYWORD,"NEXT") ||
	    eq_token(curr_node->token,KEYWORD,"PREVIOUS"))
	{
		strcpy(next_clause, token_data(curr_node->token));
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"RECORD"))
	{
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"WITH"))
	{
		curr_node = curr_node->next;
	}

	/*
	**	HOLD is a verb and will break the fragment.
	*/
	if (NODE_END == curr_node->type)
	{
		NODE temp_node;

		frag2_statement = get_statement_from_sentence(the_sentence);
		temp_node = first_token_node(frag2_statement);

		if (eq_token(temp_node->token,VERB,"HOLD"))
		{
			if (temp_node->next && 
			    (eq_token(temp_node->next->token,KEYWORD,"LIST") ||
			     eq_token(temp_node->next->token,KEYWORD,"RECORDS")))
			{
				/*
				**	This is NOT the HOLD clause it is actually
				**	the next statement which is a HOLD verb.
				*/
			}
			else
			{
				/* 
				**	Found the "with HOLD" clause
				*/
				with_hold = 1;

				curr_node = temp_node->next;

				trailing_fluff_node = free_statement(trailing_fluff_node);
				trailing_fluff_node = unhook_trailing_fluff(frag2_statement);
			}
		}
	}

	/*
	**	MODIFIABLE or ALTERED should not be specified, but if they are then ignore them.
	*/
	if (eq_token(curr_node->token,KEYWORD,"MODIFIABLE"))
	{
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"ALTERED"))
	{
		curr_node = curr_node->next;
	}

	/*
	**	ACU wants the lock clause before the INTO clause.
	*/
	if (acu_cobol)
	{
		if (with_hold)
		{
			if (manual_locking)
			{
				before_into_lock = "WITH LOCK";
			}
		}
		else
		{
			before_into_lock = "NO LOCK";
		}
	}

	if (eq_token(curr_node->token,KEYWORD,"INTO"))
	{
		curr_node = curr_node->next;
		into_node = reduce_data_item(curr_node);

		if (!into_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"READ","Unable to reduce INTO clause, clause will be ignored");
		}
		else
		{
			delint_statement(into_node);
			decontext_statement(into_node);
		}

		curr_node = curr_node->next;
	}

	/*
	**	MF & VAX  wants the lock clause after the INTO clause.
	*/
	if (mf_cobol)
	{
		if (with_hold)
		{
			if (manual_locking)
			{
				after_into_lock = "WITH KEPT LOCK";
			}
		}
		else
		{
			if (prog_ftypes[fnum] & INDEXED_FILE)	/* For indexed files use "IGNORE LOCK" */
			{
				after_into_lock = "IGNORE LOCK";
			}
			else
			{
				after_into_lock = "NO LOCK";
			}
		}
	}

	if (vax_cobol)
	{
		if (prog_ftypes[fnum] & AUTOLOCK)
		{
			/* If using automatic record locking	*/
			/* then don't supply an "ALLOWING" 	*/
			/* clause. 				*/
		}
		else if (with_hold)
		{
			after_into_lock = "ALLOWING NO OTHERS";
		}
		else
		{
			after_into_lock = "REGARDLESS OF LOCK";
		}
	}

	if (eq_token(curr_node->token,KEYWORD,"KEY"))
	{
		curr_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"IS"))
		{
			curr_node = curr_node->next;
		}
		
		key_node = reduce_data_item(curr_node);

		if (!key_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"READ","Unable to reduce KEY IS clause, clause will be ignored");
		}
		else
		{
			delint_statement(key_node);
			decontext_statement(key_node);
		}

		curr_node = curr_node->next;
	}

	/*
	**	Anything else should be in the next fragment
	*/
	if (NODE_END != curr_node->type)
	{
		write_tlog(curr_node->token,"WISP",'E',"READ",
			   "Error parsing READ, found token [%s]", token_data(curr_node->token));
	}
	
	if (frag2_statement && !with_hold)
	{
		last_statement = frag2_statement;
	}
	else
	{
		frag3_statement = get_statement_from_sentence(the_sentence);
		last_statement = frag3_statement;
	}
	curr_node = first_token_node(last_statement);


	/*
	**	timeout_clause:	TIMEOUT of {data-item|number}[second|seconds]
	**				[HOLDER-ID in data-item]
	**				{NEXT SENTENCE|statement}
	*/
	if (eq_token(curr_node->token,KEYWORD,"TIMEOUT"))
	{
		trailing_fluff_node = free_statement(trailing_fluff_node);
		trailing_fluff_node = unhook_trailing_fluff(last_statement);

		if (timeout_cnt > 0)
		{
			write_tlog(curr_node->token,"WISP",'E',"READ", "Nested TIMEOUT clauses are not supported.");
		}

		curr_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"OF"))
		{
			curr_node = curr_node->next;
		}
		
		timeout_node = reduce_data_item(curr_node);

		if (!timeout_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"READ",
				   "Unable to reduce TIMEOUT IS clause, clause will be ignored");
		}
		else
		{
			/*
			**	Unhook timeout count data item
			*/
			curr_node->down = NULL;
			decontext_statement(timeout_node);
			timeout_clause = 1;
		}

		if (!with_hold)
		{
			write_log("WISP",'E',"READ","TIMEOUT clause is invalid without HOLD clause");
			with_hold = 1;
		}

		curr_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"SECOND") ||
		    eq_token(curr_node->token,KEYWORD,"SECONDS")  )
		{
			curr_node = curr_node->next;
		}

		if (eq_token(curr_node->token,KEYWORD,"HOLDER-ID"))
		{
			NODE holderid_node;
			
			write_tlog(curr_node->token,"WISP",'W',"READ","HOLDER-ID clause not supported, will be ignored");

			curr_node = curr_node->next;

			if (eq_token(curr_node->token,KEYWORD,"IN"))
			{
				curr_node = curr_node->next;
			}

			holderid_node = reduce_data_item(curr_node);

			curr_node = curr_node->next;
		}
		
		if (eq_token(curr_node->token,KEYWORD,"NEXT"))
		{
			curr_node = curr_node->next;

			if (eq_token(curr_node->token,KEYWORD,"SENTENCE"))
			{
				curr_node = curr_node->next;
			}

			timeout_next_sentence = 1;

			if (NODE_END != curr_node->type)
			{
				write_tlog(curr_node->token,"WISP",'E',"READ","Error parsing TIMEOUT clause");
			}
		}
		else if (NODE_END == curr_node->type)
		{
			char	timeout_fname[MAX_FNAME];
			
			trailing_fluff_node = free_statement(trailing_fluff_node);

			tput_flush();

			/*
			**	Next is the TIMEOUT clause imperative-sentence.
			**
			**	Were going to need it but not until after we 
			**	process the INVALID KEY or AT END clauses.
			**	Redirect the output stream to a temp file while
			**	it is being processed.
			**	We will copy it back in and delete the temp file
			**	when we need it.
			*/
			timeout_cnt++;
			sprintf(timeout_fname, "%s%d", read_fname, timeout_cnt);
			timeout_file_ptr = open_cob_file(timeout_fname,FOR_OUTPUT,1);
			override_output_stream(timeout_file_ptr);
			
			frag4_statement = parse_imperative_statements(NULL, the_sentence);

			tput_flush();
			release_output_stream();
			timeout_cnt--;
		}
		else
		{
			/* Parse error */
			write_tlog(curr_node->token,"WISP",'E',"READ","Error parsing TIMEOUT clause");
			timeout_next_sentence = 1;
		}

		if (!frag4_statement)
		{
			frag4_statement = get_statement_from_sentence(the_sentence);
		}
		last_statement = frag4_statement;
		curr_node = first_token_node(last_statement);
	}


	/*
	**	Next clause should be one of
	**		[NOT] [AT] END
	**		[NOT] INVALID [KEY]
	**		END-READ
	**		(next statement)
	*/
	if (eq_token(curr_node->token,KEYWORD,"INVALID"))
	{
		curr_node->token->column_fixed = 1;
		invalid_key_found = 1;
	}
	else if (eq_token(curr_node->token,KEYWORD,"AT") ||
		 eq_token(curr_node->token,KEYWORD,"END"))
	{
		curr_node->token->column_fixed = 1;
		at_end_found = 1;
	}
	else if (eq_token(curr_node->token,KEYWORD,"NOT"))
	{
		curr_node->token->column_fixed = 1;
		curr_node = curr_node->next;
		
		if (eq_token(curr_node->token,KEYWORD,"INVALID"))
		{
			not_invalid_key_found = 1;
		}
		else if (eq_token(curr_node->token,KEYWORD,"AT") ||
			 eq_token(curr_node->token,KEYWORD,"END"))
		{
			not_at_end_found = 1;
		}
	}
	
	

	/*
	**	Generate results
	*/

	if ( with_hold && (invalid_key_found || not_invalid_key_found || at_end_found || not_at_end_found))
	{
		trailing_fluff_node = free_statement(trailing_fluff_node);

		/* Set up the INVALID KEY flag.		*/
		tput_line_at(col, "MOVE \"N\" TO WISP-TEST-BYTE\n");
	}

	if (timeout_node)
	{
		/* Compute the TIMEOUT value. */
		tput_line_at(col, "COMPUTE WISP-FILE-TIMEOUT = 100 * (");
		tput_statement (col+4, timeout_node);
		tput_clause(col+4, ")");
	}
	else if (with_hold)
	{
		tput_line_at(col,"MOVE 0 TO WISP-FILE-TIMEOUT");
	}

	if (with_hold)				/* Insert all the special logic to handle WITH HOLD			*/
						/*    - if INVALID KEY or AT END then put READ into separate para	*/
						/*    - keep trying to read until you get the record			*/
						/*    - handle TIMEOUT logic						*/
						/*    - add UNLOCK logic						*/

	{
		set_lock_holder_id(fnum,col);
		tput_line_at(col, "PERFORM WITH TEST AFTER");
		tput_line_at(col+8,   "UNTIL (%s IS NOT = \"%s\")", file_status, hard_lock);

		if (timeout_clause)
		{
			tput_line_at(col+8, "OR (WISP-FILE-TIMEOUT IS < 1)");
		}

		col += 4;
	}
	else
	{
		if (!manual_locking)
		{
			/*
			**	Automatic record locking will clear the lock if this file has it.
			*/
			if_file_clear_lock_holder_id(fnum, col);
		}
	}
	
	
	tput_line_at(col, "READ %s", token_data(file_node->token));

	if (next_clause[0])
	{
		tput_clause(col+4, next_clause);
	}

	if (before_into_lock)
	{
		tput_clause(col+4, "%s", before_into_lock);
	}

	if (into_node)
	{
		tput_clause(col+4, "INTO");
		tput_statement(col+4, into_node);
	}
		
	if (after_into_lock)
	{
		tput_clause(col+4, "%s", after_into_lock);
	}

	if (key_node)
	{
		tput_clause(col+4, "KEY");
		tput_statement(col+4, key_node);
	}

	if (with_hold)
	{
		if (at_end_found || not_at_end_found)
		{
			tput_line_at(col+4, "AT END");
			tput_line_at(col+4, "MOVE \"Y\" TO WISP-TEST-BYTE");
			tput_line_at(col, "END-READ");
		}

		if (invalid_key_found || not_invalid_key_found)
		{
			tput_line_at(col+4, "INVALID KEY");
			tput_line_at(col+4, "MOVE \"Y\" TO WISP-TEST-BYTE");
			tput_line_at(col, "END-READ");
		}

		tput_line_at(col, "CALL \"wfwait\" USING");
		tput_clause(col+4,    "%s", file_status);
		tput_clause(col+4,    "WISP-FILE-TIMEOUT");
		
		col -= 4;

		tput_line_at(col, "END-PERFORM");

		if (timeout_clause)
		{
			tput_line_at(col,   "IF ( %s = \"%s\" AND", file_status, hard_lock);
			tput_clause(col+4,      "WISP-FILE-TIMEOUT IS < 1)");

			clear_lock_holder_id(col+4);

			if (timeout_next_sentence)
			{
				tput_line_at(col+4, "CONTINUE");
			}
			else
			{
				/*
				**	Copy the timeout clause imperative-statement
				*/
				tput_flush();
				close_cob_file(timeout_file_ptr);
				copy_file(timeout_file_ptr->a_file->name);
				delete(timeout_file_ptr->a_file->name);
			}
			
			if (invalid_key_found || not_invalid_key_found || at_end_found || not_at_end_found)
			{
				tput_line_at(col, "ELSE");
			}
		}
	}

	/*
	**	Can now delete held statements, except for last_statement which gets moved to the_statement.
	*/
	the_statement = free_statement(the_statement);
	if (frag2_statement)
	{
		if (frag2_statement == last_statement)
		{
			the_statement = last_statement;
			last_statement = NULL;
			frag2_statement = NULL;
		}
		else
		{
			frag2_statement = free_statement(frag2_statement);
		}
	}
	if (frag3_statement)
	{
		if (frag3_statement == last_statement)
		{
			the_statement = last_statement;
			last_statement = NULL;
			frag3_statement = NULL;
		}
		else
		{
			frag3_statement = free_statement(frag3_statement);
		}
	}
	if (frag4_statement)
	{
		if (frag4_statement == last_statement)
		{
			the_statement = last_statement;
			last_statement = NULL;
			frag4_statement = NULL;
		}
		else
		{
			frag4_statement = free_statement(frag4_statement);
		}
	}
	file_node = NULL;
	into_node = NULL;
	key_node = NULL;
	timeout_node = NULL;
	

	/*
	**	Handle INVALID/END clauses
	*/
	if (invalid_key_found || not_invalid_key_found || at_end_found || not_at_end_found)
	{
		if (timeout_clause)
		{
			col += 4;
		}
		
		if (with_hold)
		{
			tput_line_at(col, "IF WISP-TEST-BYTE = \"Y\" THEN");
			clear_lock_holder_id(col+4);
			tput_flush();
		}
		else
		{
			tput_statement(col, trailing_fluff_node);
			trailing_fluff_node = free_statement(trailing_fluff_node);

			tput_statement(col, the_statement);

		}
		the_statement =  free_statement(the_statement);

		if (invalid_key_found || at_end_found)
		{
			the_statement = parse_imperative_statements(the_statement, the_sentence);
		}

		if (not_invalid_key_found || not_at_end_found)
		{
			if (with_hold)
			{
				tput_line_at(col, "ELSE");
			}

			tput_flush();
			the_statement = parse_imperative_statements(the_statement, the_sentence);
		}
		else
		{
			/*
			**	Check for NOT INVALID/END clause
			*/
			if (!the_statement)
			{
				the_statement = get_statement_from_sentence(the_sentence);
			}

			curr_node = the_statement->next;
			if (eq_token(curr_node->token,KEYWORD,"NOT"))
			{

				/*
				**	Found NOT INVALID/END clasue
				*/
				if (with_hold)
				{
					tput_line_at(col, "ELSE");
				}
				else
				{
					curr_node->token->column_fixed = 1;
					tput_statement(col+4, the_statement);
				}
				the_statement =  free_statement(the_statement);

				tput_flush();
				the_statement = parse_imperative_statements(the_statement, the_sentence);
			}
		}

		if (with_hold)
		{
			tput_line_at(col, "END-IF");
		}
		
		if (timeout_clause)
		{
			col -= 4;
		}
	}

	if (timeout_clause)
	{
		tput_line_at(col, "END-IF");
	}

	curr_node = the_statement->next;
	if (eq_token(curr_node->token,KEYWORD,"END-READ"))
	{
		if (!with_hold)
		{
			curr_node->token->column_fixed = 1;
			tput_statement(col, the_statement);
		}
		the_statement =  free_statement(the_statement);
	}

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);
	
	return the_statement;
}

/*
**	ROUTINE:	parse_read_crt()
**
**	FUNCTION:	Handle a READ of a workstation file.
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	the_statement	The READ crt verb-statement.
**
**	GLOBALS:	
**
**	RETURN:		NULL
**
**	WARNINGS:	none
**
*/
static NODE parse_read_crt(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node, file_node, into_node;
	char  	*crt_type;
	int	col, vwang_lines;
	int	altered_flag = 0;
	NODE	frag2_statement = NULL;
	NODE	next_statement = NULL;
	NODE	trailing_fluff_node = NULL;

	int	at_end_found = 0;
	int	not_at_end_found = 0;
	int	invalid_key_found = 0;
	int	not_invalid_key_found = 0;

	verb_node = first_token_node(the_statement);
	file_node = verb_node->next;
	curr_node = file_node->next;
	into_node = NULL;
	
	cur_crt = crt_index(token_data(file_node->token));
	
	if (acn_cobol)
	{
		write_tlog(verb_node->token,"WISP",'W',"NATIVE","Workstation READ %s uses WISP Screens",crt_file[cur_crt]);
	}

	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	/*
	**	Default read type is ALL
	*/
	crt_type = "VWANG-READ-ALL";

	/*
	**	Skip over fluff
	*/
	if (eq_token(curr_node->token,KEYWORD,"NEXT"))
	{
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"RECORD"))
	{
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"WITH"))
	{
		curr_node = curr_node->next;
	}

	/*
	**	HOLD is a verb and will break the fragment.
	*/
	if (NODE_END == curr_node->type)
	{
		NODE temp_node;

		next_statement = get_statement_from_sentence(the_sentence);
		temp_node = first_token_node(next_statement);

		if (eq_token(temp_node->token,VERB,"HOLD"))
		{
			if (temp_node->next && 
			    (eq_token(temp_node->next->token,KEYWORD,"LIST") ||
			     eq_token(temp_node->next->token,KEYWORD,"RECORDS")))
			{
				/*
				**	This is NOT the HOLD clause it is actually
				**	the next statement which is a HOLD verb.
				*/
			}
			else
			{
				/* 
				**	Found the "with HOLD" clause
				*/
				frag2_statement = next_statement;
				next_statement = NULL;

				curr_node = temp_node->next;

				trailing_fluff_node = free_statement(trailing_fluff_node);
				trailing_fluff_node = unhook_trailing_fluff(frag2_statement);
			}
		}
	}

	/*
	**	If MODIFIABLE or ALTERED then change read type
	*/
	if (eq_token(curr_node->token,KEYWORD,"MODIFIABLE"))
	{
		crt_type = "VWANG-READ-MODIFIABLE";
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"ALTERED"))
	{
		altered_flag = 1;
		
		crt_type = "VWANG-READ-ALTERED";
		curr_node = curr_node->next;
	}

	if (eq_token(curr_node->token,KEYWORD,"INTO"))
	{
		curr_node = curr_node->next;
		into_node = reduce_data_item(curr_node);

		if (!into_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"READ","Unable to reduce INTO clause, clause will be ignored");
		}
		else
		{
			delint_statement(into_node);
			decontext_statement(into_node);
		}

		curr_node = curr_node->next;
	}



	/*
	**	We've parsed all the info now start generating the results.
	**
	**	READ file-name-1 [NEXT] Record [With HOLD ] [INTO into-record] 
	**				       [MODIFIABLE]
	**				       [ALTERED   ]
	**		[{INVALID Key} imperative-statement-1]
	**		 {At END     }                       
	**
	**		[{NOT INVALID Key} imperative-statement-2]
	**		 {NOT At END     }       
	**
	**
	**	       [CALL "xx2byte" USING crt-rel-key, crt-record]		- if crt has a relative key
	**		MOVE crt-record TO WISP-CRT-ORDER-AREA
	**	       [MOVE WISP-SYMB-xx TO VWANG-LINES]			- if lines not equal 24
	**		CALL "vwang" USING {VWANG-READ-ALL       },
	**				   {VWANG-READ-MODIFIABLE}
	**				   {VWANG-READ-ALTERED   }
	**			WISP-CRT-RECORD, {VWANG-FULL-SCREEN},
	**					 {VWANG-LINES      }
	**			WISP-ALL-PF-KEYS, crt-pfkey, crt-status
	**		MOVE WISP-CRT-RECORD TO crt-record
	**	       [MOVE crt-record TO into-record]				- if INTO clause
	**	       [CALL "w2rowcol" USING WISP-CRT-ORDER-AREA-3 crt-cursor]	- if crt has cursor 
	**
	**	       [IF crt-status (1:1) = 'N' THEN
	**			imperative-statement-1]
	**	       [ELSE
	**			imperative-statement-2]
	**		END-IF]
	**
	**	       [IF crt-relkey < 0 OR > 24 THEN
	**			imperative-statement-1]
	**	       [ELSE
	**			imperative-statement-2]
	**		END-IF]
	*/

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	/* Load relative key into order-area	*/
	if (crt_relative[cur_crt][0])
	{
		tput_line_at	(col, "CALL \"xx2byte\" USING");
		tput_clause	(col+4, "%s,",crt_relative[cur_crt]);
		tput_clause  	(col+4, "%s",crt_record[crt_prime_rec[cur_crt]]);
	}
	/* Copy the Order-Area for the read	*/
	tput_line_at		(col, "MOVE %s",crt_record[crt_prime_rec[cur_crt]]);
	tput_clause		(col+4, "TO WISP-CRT-ORDER-AREA\n");
	/* Load number of lines			*/
	vwang_lines = (crt_record_size[crt_prime_rec[cur_crt]]-4)/80;
	if (24 != vwang_lines)
	{
		tput_line_at	(col, "MOVE WISP-SYMB-%d TO VWANG-LINES,\n", vwang_lines);
	}
#ifdef OLD
	tput_line_at		(col, "MOVE SPACES TO %s,\n",crt_status[cur_crt]);	/* Must clear status field	*/
#endif
	tput_line_at		(col, "CALL \"vwang\" USING");
	tput_clause		(col+4, "%s,",crt_type);
	tput_clause		(col+4, "WISP-CRT-RECORD,");
	if (24 == vwang_lines)
	{
		tput_clause	(col+4, "VWANG-FULL-SCREEN,");
	}
	else
	{
		tput_clause	(col+4, "VWANG-LINES,");
	}
	tput_clause		(col+4, "WISP-ALL-PF-KEYS,");
	tput_clause		(col+4, "%s,",crt_pfkey[cur_crt]);
	tput_clause		(col+4, "%s",crt_status[cur_crt]);
	tput_line_at		(col, "MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[cur_crt]]);

	/* Handle an INTO phrase.			*/
	if (into_node)
	{
		tput_line_at	(col, "MOVE %s TO",crt_record[crt_prime_rec[cur_crt]]);
		tput_statement	(col+4, into_node);
	}

	/* If this file has a CURSOR clause */
	if (crt_cursor[cur_crt][0])
	{
		tput_line_at  	(col, "CALL \"w2rowcol\" USING WISP-CRT-ORDER-AREA-3,");
		tput_clause	(col+4, "%s",crt_cursor[cur_crt]);
		tput_clause     (col, "END-CALL");
	}

	/*
	**	Anything else should be in the next fragment
	*/
	if (NODE_END != curr_node->type)
	{
		write_tlog(curr_node->token,"WISP",'E',"READ",
			   "Error parsing READ, found token [%s]", token_data(curr_node->token));
	}

	/*
	**	Free up the statements
	*/
	the_statement = free_statement(the_statement);
	if (frag2_statement)
	{
		frag2_statement = free_statement(frag2_statement);
	}
	
	if (next_statement)
	{
		the_statement = next_statement;
		next_statement = NULL;
	}
	else
	{
		the_statement = get_statement_from_sentence(the_sentence);
	}
	curr_node = first_token_node(the_statement);
	

	/*
	**	Next clause should be one of
	**		[NOT] [AT] END
	**		[NOT] INVALID [KEY]
	**		END-READ
	**		(next statement)
	*/

	if (eq_token(curr_node->token,KEYWORD,"INVALID"))
	{
		invalid_key_found = 1;
	}
	else if (eq_token(curr_node->token,KEYWORD,"AT") ||
		 eq_token(curr_node->token,KEYWORD,"END"))
	{
		at_end_found = 1;
	}
	else if (eq_token(curr_node->token,KEYWORD,"NOT"))
	{
		curr_node = curr_node->next;
		
		if (eq_token(curr_node->token,KEYWORD,"INVALID"))
		{
			not_invalid_key_found = 1;
		}
		else if (eq_token(curr_node->token,KEYWORD,"AT") ||
			 eq_token(curr_node->token,KEYWORD,"END"))
		{
			not_at_end_found = 1;
		}
	}

	if (invalid_key_found || not_invalid_key_found || at_end_found || not_at_end_found)
	{
		trailing_fluff_node = free_statement(trailing_fluff_node);

		if (invalid_key_found || not_invalid_key_found)
		{
			if (!crt_relative[cur_crt][0])
			{
				write_tlog(curr_node->token,"WISP",'E',"READ",
					   "INVALID KEY cause is invalid without a RELATIVE KEY");
				strcpy(crt_relative[cur_crt],"UNKNOWN-KEY");
			}
		
			/*
			**	INVALID KEY clause so check if key is valid (0-24)
			*/
			tput_line_at	(col, "IF %s", crt_relative[cur_crt]);
			tput_clause	(col+4, ">= 0 AND <= 24 THEN");
		}
		else if (at_end_found || not_at_end_found)
		{
			if (!altered_flag)
			{
				write_tlog(curr_node->token,"WISP",'E',"READ",
					   "AT END clause found with no ALTERED clause.");
			}

			/*
			**	If no fields were modified then do AT END logic
			*/
			tput_line_at	(col, "IF %s (1:1)",crt_status[cur_crt]); 
			tput_clause	(col+4, "= 'N' THEN");
		}

		the_statement =  free_statement(the_statement);

		if (not_invalid_key_found || not_at_end_found)
		{
			tput_line_at(col+4, "CONTINUE");
			tput_line_at(col, "ELSE");
		}
		tput_flush();
		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (invalid_key_found || at_end_found)
		{
			/*
			**	Look for the NOT clause
			*/
			if (!the_statement)
			{
				the_statement = get_statement_from_sentence(the_sentence);
			}
			curr_node = first_token_node(the_statement);

			if (eq_token(curr_node->token,KEYWORD,"NOT"))
			{
				curr_node = curr_node->next;
		
				if (eq_token(curr_node->token,KEYWORD,"INVALID"))
				{
					not_invalid_key_found = 1;

					if (at_end_found)
					{
						write_tlog(curr_node->token,"WISP",'E',"READ",
							   "AT END - NOT INVALID KEY clause mismatch.");
					}
				}
				else if (eq_token(curr_node->token,KEYWORD,"AT") ||
					 eq_token(curr_node->token,KEYWORD,"END"))
				{
					not_at_end_found = 1;

					if (invalid_key_found)
					{
						write_tlog(curr_node->token,"WISP",'E',"READ",
							   "INVALID KEY - NOT AT END clause mismatch.");
					}
				}

				if (not_invalid_key_found || not_at_end_found)
				{
					tput_line_at(col, "ELSE");
					the_statement =  free_statement(the_statement);
					the_statement = parse_imperative_statements(the_statement, the_sentence);
				}
			}
		}
		
		tput_line_at(col, "END-IF");
		
		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = first_token_node(the_statement);
	}

	if (eq_token(curr_node->token,KEYWORD,"END-READ"))
	{
		the_statement =  free_statement(the_statement);
	}

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);
	
	return the_statement;
}		

/*
**	History:
**	$Log: wt_read.c,v $
**	Revision 1.20  1998-08-28 10:56:03-04  gsl
**	Add support for READ PREVIOUS
**
**	Revision 1.19  1998-06-09 13:09:29-04  gsl
**	Add support for manual record locking
**
**	Revision 1.18  1998-03-26 14:24:53-05  gsl
**	Change to use WISP-SYMB-xx
**
**	Revision 1.17  1998-03-20 17:37:10-05  gsl
**	Fix calls to write_tlog() and moved OLD to old.c
**
**	Revision 1.16  1998-03-04 12:55:24-05  gsl
**	Add END-CALL
**
**	Revision 1.15  1998-03-03 14:38:27-05  gsl
**	Add fluff logic
**
**	Revision 1.14  1998-02-27 11:55:11-05  gsl
**	Finixh cobol-85 rewite
**
**	Revision 1.13  1997-09-15 13:57:09-04  gsl
**	fix warning
**
**	Revision 1.12  1997-09-12 14:02:13-04  gsl
**	change native warning
**
**	Revision 1.11  1997-09-12 13:19:18-04  gsl
**	Add warning for Native Screens with WOrkstation READ
**
**	Revision 1.10  1996-08-30 21:56:23-04  gsl
**	drcs update
**
**
**
*/
