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


#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"
#include "wt_datad.h"

/*
**	WRITE record-name-1 [FROM identifier-1]
**		[advancing_clause]
**		[at_end_of_page_clause]
**		[not_at_end_of_page_clause]
**		[END-WRITE]
**
**	WRITE record-name-1 [FROM identifier-1]
**		[timeout_clause]
**		[invalid_key_clause]
**		[not_invalid_key_clause]
**		[END-WRITE]
*/
NODE parse_write(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node, record_node;
	NODE	from_node = NULL;
	NODE	next_statement = NULL;
	int	col;
	int	add_after_advancing_clause = 0;
	int	fnum = -1;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"WRITE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected WRITE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"WRITE","Processing WRITE Statement.");

	tput_leading_fluff(the_statement);

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	curr_node = verb_node->next;
	record_node = curr_node;	
	
	fnum = fd_record(token_data(record_node->token));
	if (-1 == fnum)
	{
		write_tlog(record_node->token,"WISP",'W',"NOTFOUND",
			  "WRITE [%s], unknown record",
			   token_data(record_node->token));
	}

	curr_node = curr_node->next;
	if (eq_token(curr_node->token,KEYWORD,"FROM"))
	{
		curr_node = curr_node->next;
		from_node = reduce_data_item(curr_node);

		curr_node = curr_node->next;
	}

	/*
	**	advancing_clause:
	**	
	**	{BEFORE|AFTER} advancing  { data-item [line|lines] }
	**				  { 2-byte-fig-constant    }
	**				  { PAGE                   }
	**				  { mnemonic-name          }
	*/
	
	if (eq_token(curr_node->token,KEYWORD,"BEFORE") ||
	    eq_token(curr_node->token,KEYWORD,"AFTER")     )
	{
		NODE prep_node = curr_node;
		
		curr_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"ADVANCING"))
		{
			curr_node = curr_node->next;
		}

		/*
		**	Check if 2-byte figcon
		*/
		if (curr_node->token && IDENTIFIER == curr_node->token->type && fig_count)
		{
			int 	idx;
			char	num_lines[40];
			
			for(idx=0; idx<fig_count; idx++)
			{
				if (0 == strcmp(token_data(curr_node->token),fig_cons[idx]))
				{
					write_tlog(curr_node->token,"WISP",'E',"WRITE",
						   "2-byte-figcon [%s] unsupported in ADVANCING phrase",
						   token_data(curr_node->token));
					
					if (fig_val[idx][0] & 0x40)
					{
						edit_token(prep_node->token,"BEFORE");
					}
					else
					{
						edit_token(prep_node->token,"AFTER");
					}
					
					if (fig_val[idx][0] & 0x80)
					{	/* Number of lines to skip	*/
						sprintf(num_lines,"%d",(int)(fig_val[idx][1] & 0x7f));	

						edit_token(curr_node->token, num_lines);
					}
					
					break;
				}
			}
		}

	}
	else
	{
		/*
		**	There was no advancing clause.  If this is a PRINTER file
		**	then we must add an "AFTER ADVANCING 1 LINE" clause.
		**	This is automatic for true printer files but WISP changes
		**	printer files into sequentilal files so we need to check.
		*/
		if ((-1 != fnum) && (prog_ftypes[fnum] & PRINTER_FILE))
		{
			add_after_advancing_clause = 1;
		}
	}
	

	/*
	**	Anything remaining will be in the next fragment
	*/
	next_statement = get_statement_from_sentence(the_sentence);
	curr_node = next_statement->next;
	
	if (eq_token(curr_node->token, KEYWORD, "TIMEOUT"))
	{
		int next_sentence_found = 0;

		write_tlog(curr_node->token,"WISP",'E',"WRITE","TIMEOUT clause is not supported on WRITE statement");

		for(curr_node=curr_node->next; NODE_END != curr_node->type; curr_node=curr_node->next)
		{
			if (eq_token(curr_node->token,KEYWORD,"SENTENCE"))
			{
				next_sentence_found = 1;
				break;
			}
		}

		next_statement = free_statement(next_statement);

		/*
		**	If not NEXT SENTENCE then need to parse out the imperative-statement
		*/
		if (!next_sentence_found)
		{
			tput_scomment("*** TIMEOUT clause removed from WRITE statement");
			tput_line_at(col, "MOVE 1 TO WISP-LOCK-TIMEOUT");
			tput_line_at(col, "IF WISP-LOCK-TIMEOUT < 1");
			tput_flush();
			next_statement = parse_imperative_statements(next_statement, the_sentence);
			tput_line_at(col, "END-IF");
		}
		
		if (!next_statement)
		{
			next_statement = get_statement_from_sentence(the_sentence);
		}
	}

	if (opt_nogetlastfileop)
	{
		/* tput_line_at(col,"MOVE \"WR\" TO WISP-DECLARATIVES-STATUS\n"); */
		tput_line_at(col,"MOVE \"Write\" TO WISP-LASTFILEOP");
		tput_flush();
	}

	tput_statement(col,the_statement);
	the_statement = free_statement(the_statement);
	record_node = NULL;

	if (add_after_advancing_clause)
	{
		tput_clause(col+4, "AFTER 1");
	}
	

	the_statement = next_statement;
	next_statement = NULL;
	curr_node = the_statement->next;

	/*
	**	INVALID [KEY] statement
	**	[AT] {END-OF-PAGE|EOP} statement
	*/
	if (eq_token(curr_node->token, KEYWORD, "INVALID") ||
	    eq_token(curr_node->token, KEYWORD, "END-OF-PAGE") ||
	    eq_token(curr_node->token, KEYWORD, "EOP") ||
	    (eq_token(curr_node->token, KEYWORD, "AT") && 
	     (eq_token(curr_node->next->token, KEYWORD, "END-OF-PAGE") || 
	      eq_token(curr_node->next->token, KEYWORD, "EOP")))
	    )
	{
		if (eq_token(curr_node->token, KEYWORD, "INVALID") &&
		    (-1 != fnum) && (prog_ftypes[fnum] & SEQ_FILE))
		{
			/* INVALID KEY is not allowed on a SEQ file */
			write_tlog(curr_node->token,"WISP",'E',"WRITEINVALIDKEY",
				   "INVALID KEY clause is not allowed on WRITE for sequential file.");
		}

		curr_node->token->column_fixed = 1;
		tput_statement(col+4, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	/*
	**	NOT INVALID [KEY] statement
	**	NOT [AT] {END-OF-PAGE|EOP} statement
	*/
	if (eq_token(curr_node->token, KEYWORD, "NOT") && 
	    (
	     eq_token(curr_node->next->token, KEYWORD, "INVALID") ||
	     eq_token(curr_node->next->token, KEYWORD, "END-OF-PAGE") ||
	     eq_token(curr_node->next->token, KEYWORD, "EOP") ||
	     (eq_token(curr_node->next->token, KEYWORD, "AT") && 
	      (eq_token(curr_node->next->next->token, KEYWORD, "END-OF-PAGE") || 
	       eq_token(curr_node->next->next->token, KEYWORD, "EOP")))
	     ))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col+4, the_statement);
		the_statement =  free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;
	}

	if (eq_token(curr_node->token, KEYWORD, "END-WRITE"))
	{
		curr_node->token->column_fixed = 1;
		tput_statement(col, the_statement);
		the_statement =  free_statement(the_statement);
	}

	return the_statement;

	/*
	**	NOTE: There is additional VAX/VMS logic which has not been added.
	*/
}

/*
**	History:
**	$Log: wt_write.c,v $
**	Revision 1.22  2003/03/10 18:55:44  gsl
**	Added nosleep option and for ACU default to using C$SLEEP instead
**	of WFWAIT on a READ with HOLD
**	
**	Revision 1.21  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.20  2003/03/06 21:47:10  gsl
**	Change WISP-DECLARATIVES-STATUS to WISP-LASTFILEOP
**	
**	Revision 1.19  2003/02/04 17:33:18  gsl
**	fix copyright header
**	
**	Revision 1.18  2002/08/13 18:11:59  gsl
**	Report Invalid use of INVALID KEY clauses
**	
**	Revision 1.17  2002/06/20 23:05:46  gsl
**	remove obsolete code
**	
**	Revision 1.16  2002/05/16 21:53:31  gsl
**	getlastfileop logic
**	
**	Revision 1.15  1998-12-15 14:44:52-05  gsl
**	Add support for the END-OF-PAGE clauses
**
**	Revision 1.14  1998-10-13 09:37:29-04  gsl
**	Add missing include file
**
**	Revision 1.13  1998-08-28 17:14:09-04  gsl
**	For PRINTER files a WRITE statement without an advancing clause
**	needs to have a AFTER ADVANCING 1 LINE clause added.
**
**	Revision 1.12  1998-03-27 14:11:12-05  gsl
**	Move OLD to old.c
**
**	Revision 1.11  1998-03-03 15:34:29-05  gsl
**	rewrote for cobol-85
**
**	Revision 1.10  1996-08-30 21:56:26-04  gsl
**	drcs update
**
**
**
*/
