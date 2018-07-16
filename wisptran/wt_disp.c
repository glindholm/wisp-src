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
/*
**	File:		wt_disp.c
**
**	Purpose:	DISPLAY and DISPLAY AND READ statements
**
**	Routines:	
**	parse_display()			DISPLAY statements
**	parse_display_and_read()	DISPLAY AND READ statements
**
**	dnr_pfkeys_clause()		Handle PFKEY clauses
**
*/

#include <assert.h>

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "statment.h"
#include "wt_procd.h"
#include "wt_disp.h"
#include "reduce.h"

static NODE dnr_pfkeys_clause(NODE first_node, char* target, int col, int enter_key, int* key_cnt);


/*
**	Routine:	parse_display()
**
**	Function:	Handle the DISPLAY statement
**
**	Description:	Translate and write out the DISPLAY statement.
**			If actually a DISPLAY AND READ then call parse_display_and_read().
**
**	Arguments:
**	the_statement	A verb statement
**
**	Globals:	None
**
**	Return:		The unprocessed statement or NULL
**
**	Warnings:	None
**
*/

NODE parse_display(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, display_node, next_statement, verb_node;
	NODE	trailing_fluff_node = NULL;
	int	col;

	verb_node = first_token_node(the_statement);
	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"DISPLAY"))
	{
		/*
		**	NOT a DISPLAY statement, return it.
		*/
		write_tlog(curr_node->token,"WISP",'E',"VERB","Expected DISPLAY found [%s].", token_data(curr_node->token));
		return(the_statement);
	}

	display_node = curr_node;

	if (eq_token(display_node->next->token,KEYWORD,"AND"))
	{
		return parse_display_and_read(the_statement, the_sentence);
	}
	
	if (!proc_display)
	{
		write_log("WISP",'I',"SKIPDISP","Skipped processing of DISPLAY statement.");
		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);

		return the_statement;
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));


	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	/*
	**	DISPLAY {identifier|literal}... [UPON WORKSTATION][WITH NO ADVANCING]
	**
	**	MOVE SPACES TO WISP-CRT-SCREEN-AREA
	**	STRING {identifier|literal}... DELIMITED BY SIZE INTO WISP-CRT-SCREEN-AREA
	**	CALL \"WDISPLAY\" USING WISP-CRT-RECORD
	*/
	

	/*
	**	Scan thru reducing the data-items looking for the end of the list.
	*/
	
	for(curr_node = display_node->next; NODE_END != curr_node->type;curr_node = curr_node->next)
	{
		if (curr_node->token)
		{
			if (KEYWORD==curr_node->token->type)
			{
				break;
			}

			if (!reduce_data_item(curr_node))
			{
				write_log("WISP",'W',"DISPLAY","Error parsing DISPLAY, unrecognized data item.[%s]",
							token_data(curr_node->token));
				reduce_one(curr_node);
			}
		}
	}

	if (NODE_END != curr_node->type)
	{
		/*
		**	The phrases [UPON WORKSTAION] [WITH NO ADVANCING] are comments so remove.
		*/
		if (eq_token(curr_node->token,KEYWORD,"UPON"))
		{
			free_token_from_node(curr_node);
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token,IDENTIFIER,"WORKSTATION")) /* NOTE: WORKSTATION is an IDENTIFIER */
		{
			free_token_from_node(curr_node);
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token,KEYWORD,"WITH"))
		{
			free_token_from_node(curr_node);
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token,KEYWORD,"NO"))
		{
			free_token_from_node(curr_node);
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token,KEYWORD,"ADVANCING"))
		{
			free_token_from_node(curr_node);
			curr_node = curr_node->next;
		}

	}
	
	if (curr_node && NODE_END != curr_node->type)
	{
		write_tlog(curr_node->token, "WISP",'E',"PARSE","Error parsing DISPLAY, found [%s]",
				   token_data(curr_node->token));
		next_statement = unhook_sub_tree(the_statement, curr_node);
	}
	else
	{
		next_statement = NULL;
	}
	
	/*
	**	Process the DISPLAY
	*/
	col = display_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	edit_token(display_node->token,"STRING");
	display_node->token->column = col;

	tput_leading_fluff(the_statement);

	if (acn_cobol)
	{
		tput_line_at(col,"MOVE SPACES TO WISP-DISPLAY-FIELDS-DATA");
	}
	else
	{
		tput_line_at(col,"MOVE SPACES TO WISP-CRT-SCREEN-AREA");
	}

	tput_flush();

	tput_statement(col,the_statement);

	tput_line_at(col+4, "DELIMITED BY SIZE INTO");
	if (acn_cobol)
	{
		tput_clause (col+8, "WISP-DISPLAY-FIELDS-DATA");
		tput_line_at(col,   "CALL \"WACUDISPLAY\" USING");
		tput_clause (col+4,     "WISP-APPLICATION-NAME");
		tput_clause (col+4,     "WISP-DISPLAY-FIELDS-DATA");
		tput_line_at(col,   "END-CALL");
	}
	else
	{
		tput_clause (col+8, "WISP-CRT-SCREEN-AREA");
	/*	tput_line_at(col,   "MOVE \"Press RETURN to proceed.\" TO");	*/
	/*	tput_clause (col+4, "WISP-SCREEN-LINE(24)");			*/
	/*	tput_line_at(col,   "MOVE WISP-FULL-SCREEN-ORDER-AREA TO");	*/
	/*	tput_clause (col+4, "WISP-CRT-ORDER-AREA");			*/
		tput_line_at(col,   "CALL \"WDISPLAY\" USING WISP-CRT-RECORD");
		tput_clause (col,   "END-CALL");
	}

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);
	
	the_statement = free_statement(the_statement);
	return next_statement;
}

/*
**	Routine:	parse_display_and_read()
**
**	Function:	Handle DISPLAY AND READ statement
**
**	Description:	Translate and write out the DISPLAY AND READ statement.
**
**	Arguments:
**	the_statement	A verb statement
**
**	Globals:	None
**
**	Return:		The unprocessed statement or NULL
**
**	Warnings:	None
**
**	History:	
**	07/22/94	Written GSL
**
*/

/*
Before:
	DISPLAY AND READ [ALTERED] record-name ON crt-file
		[ [ONLY] {PFKEY|PFKEYS} {ident} [ident ...]                  ]
		[ [ON    {PFKEY|PFKEYS} {ident} [ident ...] imperative-stmt] ]
		[NO-MOD imperative-stmt]
		[END-DISPLAY]

After:
	Set WISP-ALLOWABLE-PF-KEYS
	Set WISP-ON-PF-KEYS
	PERFORM WDR-record-name
	IF on-pfkey
		{on-pfkeys imperative-stmts}
	ELSE
		IF no-mod
			{no-mod imperative-stmts}
		ELSE
			PERFORM WGS-record-name
			MOVE WISP-CRT-RECORD TO crt-record
		END-IF
	END-IF

Acucobol Native Screens:
	Set WISP-ALLOWABLE-PF-KEYS
	Set WISP-ON-PF-KEYS
	PERFORM DNR-record-name
	IF on-pfkey
		{on-pfkeys imperative-stmts}
	END-IF
	IF no-mod   (dead block - never executed)
		{no-mod imperative-stmts}
	END-IF
*/
NODE parse_display_and_read(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node;
	NODE	trailing_fluff_node = NULL;
	int	col;
	char	*record_name, *crt_name;
	int	enter_key;
	char	wdr[48], wgs[48];
	int	on_pfkeys, no_mod, altered;
	int	key_cnt, on_key_cnt;
	int	period_found;
	int	scrn_idx, crt_idx;

	write_log("WISP",'I',"DISPANDREAD","DISPLAY AND READ statement.");

	/*
	**	Get the column of the DISPLAY verb.
	*/
	col = the_statement->next->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	/*
	**	Free the DISPLAY AND portion.
	*/
	tput_leading_fluff(the_statement);
	the_statement = free_statement(the_statement);

	/*
	**	Get the READ ... portion.
	*/
	the_statement = get_statement_from_sentence(the_sentence);
	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	curr_node = the_statement->next;
	if (!eq_token(curr_node->token,VERB,"READ"))
	{
		write_log("WISP",'E',"DISPANDREAD","Expecting keyword READ found [%s].", token_data(curr_node->token));
		return(the_statement);
	}

	curr_node = curr_node->next;
	altered = 0;
	if (eq_token(curr_node->token,KEYWORD,"ALTERED"))
	{
		if (acn_cobol)
		{
			write_tlog(curr_node->token, "WISP",'E',"NATIVE",
				   "ALTERED clause on DISPLAY AND READ not supported with Native Screens");
		}

		altered = 1;
		curr_node = curr_node->next;
	}

	record_name = token_data(curr_node->token);
	write_log("WISP",'I',"DISPANDREAD","DISPLAY AND READ statement of screen record %s.",record_name);

	tput_scomment("*    DISPLAY AND READ %s.",record_name); 

											/* Try to find a match in the known	*/
											/* screen list				*/
	for(scrn_idx=0; scrn_idx < num_screens; scrn_idx++)
	{
		if ( 0 == strcmp(record_name,scrn_name[scrn_idx]) )  break;
	}
	if (scrn_idx == num_screens)
	{
		write_log("WISP",'F',"CANTFINDSCRN","Could not find screen named %s",record_name);
		exit_with_err();
	}

	if (in_decl)									/* if we are currently in declaratives	*/
	{
		scrn_flags[scrn_idx] |= SCRN_IN_DECLARATIVES;				/* flag it as such			*/
	}
	if (!in_decl || copy_to_dtp_file)						/* If not, or copying.			*/
	{
		scrn_flags[scrn_idx] |= SCRN_IN_PROCDIV;				/* Otherwise flag procedure division	*/
	}

	curr_node = curr_node->next;							/* skip to the ON keyword		*/
	curr_node = curr_node->next;							/* skip to the file-name node		*/

	crt_name = token_data(curr_node->token);

	crt_idx = crt_index(crt_name);
	if (-1 == crt_idx)
	{
		write_log("WISP",'E',"NOTWS","DISPLAY AND READ on unknown Workstation file [%s]",crt_name);
		crt_idx = 0;
	}
	scrn_crt[scrn_idx] = crt_idx;							/* Remember which one it was.		*/

	if (acn_cobol)
	{
		/*
		**	Native Screens does not support the ALTERED clause
		*/
	}
	else
	{
		if (altered)
		{
			tput_line_at(col,"MOVE \"Y\" TO WISP-DNR-ALT-FLAG,");
		}
		else
		{
			tput_line_at(col,"MOVE \"N\" TO WISP-DNR-ALT-FLAG,");
		}
	}

	curr_node = curr_node->next;							/* Point past the file name	*/

	enter_key = 1;
	if (eq_token(curr_node->token,KEYWORD,"ONLY"))
	{
		enter_key = 0;
		curr_node = curr_node->next;
	}

	if (eq_token(curr_node->token,KEYWORD,"PFKEY") || eq_token(curr_node->token,KEYWORD,"PFKEYS"))
	{
		curr_node = curr_node->next;

		curr_node = dnr_pfkeys_clause(curr_node,"WISP-ALLOWABLE-PF-KEYS",col,enter_key,&key_cnt);
	}
	else
	{
		/*
		**	No PFKEYS clause.
		*/
		if (acn_cobol)
		{
			tput_line_at(col,"MOVE \"00\" TO WISP-ALLOWABLE-PF-KEYS,");
		}
		else
		{
			tput_line_at(col,"MOVE \"00X\" TO WISP-ALLOWABLE-PF-KEYS,");
		}

		key_cnt = 1;
	}

	if (acn_cobol)
	{
		/*
		**	Native screens uses a pfkey count instead of a trailing "X"
		*/
		tput_line_at(col,"MOVE %d TO WISP-ALLOWABLE-PF-KEYS-CNT", key_cnt);
	}

	if (eq_token(curr_node->token,KEYWORD,"ON"))
	{
		if (acn_cobol)
		{
			write_tlog(curr_node->token, "WISP",'W',"NATIVE",
				   "ON PFKEY(S) clause causes data to be transferred with Native Screens");
		}

		on_pfkeys = 1;
		curr_node = curr_node->next;
		curr_node = curr_node->next;
		enter_key = 0;
		curr_node = dnr_pfkeys_clause(curr_node,"WISP-ON-PF-KEYS",col,enter_key,&on_key_cnt);

		/*
		**	At this point curr_node is the...
		**		NODE_END	End of statement
		**		KEYWORD		Error
		*/

		if (0 == on_key_cnt)
		{
			/*
			**	Special case of an ON PFKEYS clause with no keys specified.
			*/
			tput_line_at(col,"MOVE WISP-ALLOWABLE-PF-KEYS TO WISP-ON-PF-KEYS,");
			on_key_cnt = key_cnt;
		}

	}
	else
	{
		/*
		**	No ON PFKEYS clause.
		*/
		on_pfkeys = 0;
		if (acn_cobol)
		{
			/* Native screens uses a count instead of a trailing "X" */
		}
		else
		{
			tput_line_at(col,"MOVE \"X\" TO WISP-ON-PF-KEYS,");
		}
		on_key_cnt = 0;
	}

	if (acn_cobol)
	{
		/* Native screens uses a count instead of a trailing "X" */
		tput_line_at(col,"MOVE %d TO WISP-ON-PF-KEYS-CNT", on_key_cnt);
	}

	if (in_decl)
	{
		if (acn_cobol)
		{
			make_fld(wdr,scrn_name[scrn_idx],"DWDNR-");
		}
		else
		{
			make_fld(wdr,scrn_name[scrn_idx],"DWDR-");
		}
		make_fld(wgs,scrn_name[scrn_idx],"DWGS-");
	}
	else
	{
		if (acn_cobol)
		{
			make_fld(wdr,scrn_name[scrn_idx],"WDNR-");
		}
		else
		{
			make_fld(wdr,scrn_name[scrn_idx],"WDR-");
		}
		make_fld(wgs,scrn_name[scrn_idx],"WGS-");
	}

	tput_line_at(col,"PERFORM %s",wdr);

	/* chk_decl();	*/								/* Check DECLARATIVES interaction.	*/
	{
		/*
		**	I don't think this code works correctly.
		*/
		char tstr[16];

		if (copy_to_dcl_file && !(scrn_flags[scrn_idx] & SCRN_IN_DECLARATIVES))	/* If currently copying paragraphs.	*/
		{									/* And this screen is not in the DECLAR	*/
			sprintf(tstr,"%d",scrn_idx);					/* Add it to the paragraph list.	*/
			add_perf(tstr);							/* S we can fix it later.		*/
		}
	}

	period_found = 0;
	if (on_pfkeys)
	{
		if (acn_cobol)
		{
			tput_line_at(col, "IF WISP-DNR-ON-PFKEY = \"Y\" THEN");
		}
		else
		{
			tput_line_at(col, "IF WISP-ON-PF-KEYS(1:1) IS = \"*\" THEN");
		}
		tput_flush();

		/*
		**	curr_node should point to the NODE_END token of the first fragment.
		**	Get the next fragment
		*/

		if (NODE_END != curr_node->type)
		{
			write_tlog(curr_node->token, "WISP",'E',"DISPANDREAD","Error parsing ON PFKEY clause, found [%s]",
				   token_data(curr_node->token));
		}

		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

		the_statement = free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (acn_cobol)
		{
			tput_line_at(col, "END-IF");
		}
		else
		{
			tput_line_at(col, "ELSE");
			tput_flush();
		}
	}
	else
	{

		if (NODE_END != curr_node->type)
		{
			write_tlog(curr_node->token, "WISP",'E',"DISPANDREAD","Error parsing DISPLAY AND READ, found [%s]",
				   token_data(curr_node->token));
		}
		
		the_statement = free_statement(the_statement);
	}

	/*
	** 	Next is either NO-MOD or END-DISPLAY
	**	both are a new fragment.
	*/
	if (!the_statement)
	{
		the_statement = get_statement_from_sentence(the_sentence);
	}
	curr_node = the_statement->next;

	no_mod = 0;
	if (eq_token(curr_node->token,KEYWORD,"NO-MOD"))
	{
		no_mod = 1;

		if (on_pfkeys)
		{
			col+= 4;
		}

		if (acn_cobol)
		{
			write_log("WISP",'E',"NATIVE","NO-MOD clause of DISPLAY AND READ not supported with Native Screens");
		}

		if (acn_cobol)
		{
			/*
			**	This will enclose the NO-MOD code in an IF which is never true (dead code)
			*/
			tput_scomment("*>>> The NO-MOD clause is not supported with Native Screens.");
			tput_scomment("*>>> This condition will never be TRUE so the code must be reworked.");
			tput_line_at(col, "IF WISP-DNR-NO-MOD = \"Y\" THEN");
		}
		else
		{
			tput_line_at(col, "IF %s IS GREATER THAN \"N?\"", crt_status[crt_idx]);
		}

		tput_flush();

		tput_statement(col, trailing_fluff_node);
		trailing_fluff_node = free_statement(trailing_fluff_node);

		the_statement = free_statement(the_statement);

		the_statement = parse_imperative_statements(the_statement, the_sentence);

		if (!the_statement)
		{
			the_statement = get_statement_from_sentence(the_sentence);
		}
		curr_node = the_statement->next;


		if (acn_cobol)
		{
			tput_line_at(col, "END-IF");
		}
		else
		{
			tput_line_at(col, "ELSE");
			tput_flush();
		}
	}

	if (acn_cobol)
	{
		/*
		**	With ACUCOBOL native screens all the work is done in the one PERFORM statement.
		*/
	}
	else
	{
		if (on_pfkeys || no_mod)
		{
			tput_line_at(col+4,"PERFORM %s,",wgs);
			tput_line_at(col+4,"MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[crt_idx]]);
		}
		else
		{
			tput_line_at(col,"PERFORM %s,",wgs);
			tput_line_at(col,"MOVE WISP-CRT-RECORD TO %s",crt_record[crt_prime_rec[crt_idx]]);
		}

		if (no_mod)
		{
			tput_line_at(col, "END-IF");

			if (on_pfkeys)
			{
				col -= 4;
			}
		}

		if (on_pfkeys)
		{
			tput_line_at(col, "END-IF");
		}
	}

#ifdef OLD
	if (no_mod || on_pfkeys)
	{
		tput_flush();
	}
#endif /* OLD */

	if (eq_token(curr_node->token,KEYWORD,"END-DISPLAY"))
	{
		decontext_statement(curr_node);
		clean_token(curr_node->token);					/* Remove the END-DISPLAY		*/

		tput_statement(col, the_statement);
		the_statement = free_statement(the_statement);
	}

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);

	return the_statement;
}

static NODE dnr_pfkeys_clause(NODE first_node, char* target, int col, int enter_key, int* key_cnt)
{
	char	pfkeys[80];
	int	pfkeys_cnt;
	NODE	temp_node, return_node;
	int	nonnumeric_keys;

	if (enter_key)
	{
		pfkeys_cnt = 1;
		strcpy(pfkeys,"\"00");
	}
	else
	{
		pfkeys_cnt = 0;
		strcpy(pfkeys,"\"");
	}

	nonnumeric_keys = 0;

	/*
	**	Take a pass thru the pfkeys and collect all the numeric ones.
	*/
	for(temp_node=first_node; NODE_END != temp_node->type; temp_node=temp_node->next)
	{
		if (!temp_node->token) continue;

		if (KEYWORD==temp_node->token->type) break;

		if (NUMBER==temp_node->token->type)
		{
			char	*key_data;

			key_data = token_data(temp_node->token);
			if (!key_data[1])
			{
				strcat(pfkeys,"0");
			}
			strcat(pfkeys,key_data);

			pfkeys_cnt += 1;
		}
		else
		{
			nonnumeric_keys = 1;
			reduce_data_item(temp_node);
		}
	}
	return_node = temp_node;

	if (!nonnumeric_keys && !acn_cobol)
	{
		strcat(pfkeys,"X");
	}
	strcat(pfkeys,"\"");

	if (pfkeys_cnt > 0 || !nonnumeric_keys)
	{
		int	boundary;

		/*
		**	Simple case: All numeric pfkeys.
		*/
		/* tput_line("123456*8901234567890123456789012345678901234567890123456789012345678901234567890\n"); */

		boundary= 72 - (col+5) + 1;	/* pfkeys string starts at col + strlen("MOVE ")=5 */
		if ((int)strlen(pfkeys) > boundary)				/* Won't fit on one line		*/
		{
			char	savchr;
			savchr = pfkeys[boundary];				/* save the char			*/
			pfkeys[boundary] = '\0';
			tput_line_at(col,"MOVE %s",pfkeys);			/* write first part			*/
			pfkeys[boundary] = savchr;
			tput_line("      -         \"%s",&pfkeys[boundary]);	/* write the rest			*/
		}
		else
		{
			tput_line_at(col,"MOVE %s",pfkeys);			/* write first part			*/
		}
		tput_clause(col+4, "TO %s,",target);				/* just finish the MOVE			*/
	}

	/*
	**	If non-numeric keys where found take a second pass thur.
	*/
	for(temp_node=first_node; nonnumeric_keys && NODE_END != temp_node->type; temp_node=temp_node->next)
	{
		if (temp_node->token && KEYWORD==temp_node->token->type) break;

		if (temp_node->token && NUMBER==temp_node->token->type)
		{
			/* do nothing */
		}
		else
		{
			/*
			**	Add each pfkey field to the list.
			*/

			tput_line_at(col,"MOVE");

			if (NODE_DATAITEM == temp_node->type)
			{
				strip_statement(temp_node->down);
				tput_statement(col,temp_node->down);
			}
			else
			{
				tput_token(col,temp_node->token);
			}
			pfkeys_cnt += 1;
			tput_clause(col, "TO %s-SUB(%d),", target, pfkeys_cnt);
		}
	}

	if (acn_cobol)
	{
		/*
		**	ACN code does not require a trailing "X"
		*/
	}
	else if (nonnumeric_keys)
	{
		tput_line_at(col,"MOVE \"X\" TO %s(%d:1),", target, pfkeys_cnt*2+1);
	}

	*key_cnt = pfkeys_cnt;
	return return_node;
}
/*
**	History:
**	$Log: wt_disp.c,v $
**	Revision 1.23  1998/04/03 19:38:45  gsl
**	Changed DISPLAY for ACN to call WACUDISPLAY routines
**	
**	Revision 1.22  1998-03-23 12:53:22-05  gsl
**	Change ACN DISPLAY to a PERFORM WISP-DISPLAY-PARA
**
**	Revision 1.21  1998-03-23 10:28:31-05  gsl
**	For ACN DISPLAY verb add END-ACCEPT to ACCEPT OMITTED
**
**	Revision 1.20  1998-03-05 16:27:57-05  gsl
**	Fix the boundary condition for spliting long pfkey strings across
**	two lines.
**
**	Revision 1.19  1998-03-04 12:50:59-05  gsl
**	Add END-CALL
**
**	Revision 1.18  1998-03-03 11:45:29-05  gsl
**	Rework the next-node logic.
**	Added fluff handling
**
**	Revision 1.17  1998-02-24 18:06:07-05  gsl
**	Fix leading fluff and flushing
**
**	Revision 1.16  1998-02-24 10:55:33-05  gsl
**	Rewrote to fully handle cobol-85 syntax plus full scope of statement.
**
**	Revision 1.15  1998-02-10 15:16:05-05  gsl
**	Change to use get_statement_fragment()
**	Add support to handle UPON WORKSTATION WITH NO ADVANCING.
**	Add support to return any unused tokens which are not part of
**	this DISPLAY statement.
**
**	Revision 1.14  1997-09-15 11:15:54-04  gsl
**	Add native screens translation for DISPLAY verb
**
**	Revision 1.13  1997-09-12 14:13:07-04  gsl
**	Fix native warnings
**
**	Revision 1.12  1997-09-09 18:20:44-04  gsl
**	First pass of ACUCOBOL Native Screens
**
**	Revision 1.11  1996-10-09 12:31:37-04  gsl
**	fix warnings
**
**	Revision 1.10  1996-08-30 18:56:16-07  gsl
**	drcs update
**
**
**
*/
