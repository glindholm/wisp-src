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
#include "keywords.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"

/*
	p_accept:	Process the ACCEPT verb.

			ACCEPT xxx FROM {DATE|DATE4|DAY|DAY-OF-WEEK|TIME}	- unchanged.

	(before)	ACCEPT field1 field2 ... field16.

	(after)		MOVE "field1" TO WISP-BUFF(1)				- load the field tags
			MOVE "field2" TO WISP-BUFF(2)
			. . .
			MOVE "fieldx" TO WISP-BUFF(x)
			MOVE x TO WISP-LONGWORD					- Set the argument count
			CALL "WACCEPT" USING WISP-LONGWORD WISP-BUFF(1)		- call WACCEPT
			   WISP-BUFF(2) . . . WISP-BUFF(x)
			UNSTRING WISP-BUFF(1) DELIMITED WISP-SYMB-0 INTO field1	- Unload the ACCEPTed values
			UNSTRING WISP-BUFF(2) DELIMITED WISP-SYMB-0 INTO field2
			. . .
			UNSTRING WISP-BUFF(x) DELIMITED WISP-SYMB-0 INTO fieldx
*/

NODE parse_accept(NODE the_statement)
{
	NODE	curr_node, verb_node;
	NODE	next_statement = NULL;
	NODE	trailing_fluff_node = NULL;
	int	col;
	int	item_cnt;
	int	idx;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"ACCEPT"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected ACCEPT found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"ACCEPT","Processing ACCEPT Statement.");

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	/*
	**	Loop thru reducing identifiers
	*/
	item_cnt = 0;
	curr_node = verb_node->next;
	for(;;)
	{
		if (!curr_node)
		{
			break;
		}
		if (!curr_node->token)
		{
			break;
		}

		if (IDENTIFIER == curr_node->token->type)
		{
			reduce_data_item(curr_node);
			item_cnt++;

			curr_node = curr_node->next;
		}
		else
		{
			break;
		}
	}

	if (item_cnt < 1)
	{
		write_log("WISP",'E',"PARSE", "Error parsing ACCEPT statement, found no identifiers");
		tput_statement(col, the_statement);
		return(free_statement(the_statement));
	}
	if (item_cnt > 16)
	{
		write_log("WISP",'E',"PARSE", "Error parsing ACCEPT statement, found more then 16 identifiers");
		item_cnt = 16;
	}
	
	if (eq_token(curr_node->token, KEYWORD,"FROM"))
	{
		curr_node = curr_node->next;
			
		if (eq_token(curr_node->token, KEYWORD,"DATE") ||
		    eq_token(curr_node->token, IDENTIFIER,"DATE4") ||		/* NOTE: DATE4 is an identifier */
		    eq_token(curr_node->token, KEYWORD,"DAY") ||
		    eq_token(curr_node->token, KEYWORD,"DAY-OF-WEEK") ||
		    eq_token(curr_node->token, KEYWORD,"TIME"))
		{
			/*
			**	These are standard COBOL syntax.
			*/
			if (eq_token(curr_node->token, KEYWORD,"DATE") ||
			    eq_token(curr_node->token, KEYWORD,"DAY") )
			{
				write_log("WISP",'W',"YEAR2000","ACCEPT FROM %s is not YEAR2000 compliant",
					  token_data(curr_node->token));
			}	
			else if (eq_token(curr_node->token, IDENTIFIER,"DATE4"))
			{
				if (acu_cobol)
				{
					write_tlog(curr_node->token, "WISP",'I',"ACCEPT",
						   "ACCEPT FROM DATE4 translated to CENTURY-DATE");
					edit_token(curr_node->token,"CENTURY-DATE");
				}
				else /* mf_cobol - note this is also valid for Acucobol 5.x */
				{
					write_tlog(curr_node->token, "WISP",'I',"ACCEPT",
						   "ACCEPT FROM DATE4 translated to ACCEPT FROM DATE YYYYMMDD");
					edit_token(curr_node->token,"DATE YYYYMMDD");
				}
			}

			/*
			**	If there is anything trailing other then a PERIOD then return it.
			*/
			curr_node = curr_node->next;
			if (NODE_END != curr_node->type)
			{
				next_statement = unhook_next_fragment(the_statement, curr_node);
			}
			else
			{
				next_statement = NULL;
			}
			
			tput_statement(12, the_statement);
			free_statement(the_statement);
			
			return next_statement;
		}
		else if (IDENTIFIER == curr_node->token->type) 		/* Assume special-names mnemonic */
		{
			curr_node = curr_node->next;
		}
		else
		{
			write_tlog(curr_node->token,"WISP",'E',"PARSE",
				   "Error parsing ACCEPT statement, found [%s]",
				   token_data(curr_node->token));
		}

	}

	if (NODE_END != curr_node->type)
	{
		next_statement = unhook_next_fragment(the_statement, curr_node);
	}
	else
	{
		next_statement = NULL;
	}

	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	/* 
	**	Generate the ACCEPT logic
	*/
	curr_node = verb_node->next;
	for(idx=1; idx<=item_cnt; idx++)	/* NOTE: loop 1 to N */
	{
		tput_line_at(col, "MOVE \"%-8.8s\" TO", token_data(curr_node->down->token));
		tput_clause(col+4, "WISP-BUFF(%d)", idx);

		curr_node = curr_node->next;
	}

	tput_line_at(col, "MOVE %d TO WISP-LONGWORD", item_cnt);

	tput_line_at(col, "CALL \"WACCEPT\" USING WISP-LONGWORD");
	for(idx=1; idx<=item_cnt; idx++)	/* NOTE: loop 1 to N */
	{
		tput_clause(col+4, "WISP-BUFF(%d)", idx);
	}
	
	curr_node = verb_node->next;
	for(idx=1; idx<=item_cnt; idx++)	/* NOTE: loop 1 to N */
	{
		tput_line_at(col, "UNSTRING WISP-BUFF(%d) DELIMITED", idx);
		tput_clause(col+4, "WISP-SYMB-0 INTO");
		decontext_statement(curr_node->down);
		delint_statement(curr_node->down);
		tput_statement(col+4, curr_node->down);

		curr_node = curr_node->next;
	}

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);

	the_statement = free_statement(the_statement);
			
	return next_statement;
}

/*
**	History:
**	$Log: wt_acept.c,v $
**	Revision 1.18  2001-09-13 14:40:16-04  gsl
**	For MF ACCEPT FROM DATE4 now translated to ACCEPT FROM DATE YYYYMMDD
**
**	Revision 1.17  1998-03-26 14:23:30-05  gsl
**	Change to use WISP-SYMB-xx
**
**	Revision 1.16  1998-03-03 09:53:51-05  gsl
**	Add trailing fluff logic
**
**	Revision 1.15  1998-02-20 17:15:56-05  gsl
**	Fix NODE_END logic
**
**	Revision 1.14  1998-02-19 08:53:12-05  gsl
**	Change to use first_token_node()
**
**	Revision 1.13  1998-02-11 10:57:31-05  gsl
**	Re-wrote to use new statement parse logic.
**
**	Revision 1.12  1997-09-24 15:45:14-04  gsl
**	Remove native screen warning.
**	Add YEAR2000 warnings for DAY and DATE.
**	Add Acucobol support for DATE4
**
**	Revision 1.11  1997-09-18 14:58:49-04  gsl
**	Add native warnings
**
**	Revision 1.10  1996-08-30 21:56:13-04  gsl
**	drcs update
**
**
**
*/
