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

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "statment.h"
#include "wt_procd.h"
#include "wt_disp.h"
#include "reduce.h"

static NODE dnr_pfkeys_clause();

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
**	History:	
**	07/22/94	Updated GSL
**
*/

NODE parse_display(the_statement)
NODE the_statement;
{
	NODE	curr_node, display_node, period_node;
	int	col;

	curr_node = the_statement->next;

	if (!eq_token(curr_node->token,VERB,"DISPLAY"))
	{
		/*
		**	NOT a DISPLAY statement, return it.
		*/
		return(the_statement);
	}

	if (eq_token(curr_node->next->token,KEYWORD,"AND"))
	{
		return parse_display_and_read(the_statement);
	}

	write_log("WISP",'I',"PROCDISP","Processing DISPLAY statement.");

	display_node = curr_node;

	if (!proc_display)
	{
		write_log("WISP",'I',"SKIPDISP","Skipped processing of DISPLAY statement.");
		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);
		return(the_statement);
	}

	col = display_node->token->column;
	if (col < 12) col = 12;
	else if (col > 24) col = 24;

	if (acn_cobol)
	{
		tput_line_at(col,"MOVE SPACES TO WISP-DISPLAY-FIELDS-DATA");
	}
	else
	{
		tput_line_at(col,"MOVE SPACES TO WISP-CRT-SCREEN-AREA");
	}
	
	tput_flush();
	edit_token(display_node->token,"STRING");
	display_node->token->column = col;

	curr_node = curr_node->next;
	for(period_node=NULL; NODE_END != curr_node->type; curr_node=curr_node->next)
	{
		if (PERIOD==curr_node->token->type)
		{
			period_node = curr_node;
			free_token_from_node(period_node);
			break;
		}
	}

	tput_statement(12,the_statement);

	tput_line_at(col+4, "DELIMITED BY SIZE INTO");
	if (acn_cobol)
	{
		tput_clause (col+8, "WISP-DISPLAY-FIELDS-DATA");
		tput_line_at(col,   "DISPLAY WINDOW ERASE");
		tput_line_at(col,   "DISPLAY WISP-DISPLAY-SCREEN");
		tput_line_at(col,   "ACCEPT OMITTED");
	}
	else
	{
		tput_clause (col+8, "WISP-CRT-SCREEN-AREA");
		tput_line_at(col,   "MOVE \"Press RETURN to proceed.\" TO");
		tput_clause (col+4, "WISP-SCREEN-LINE(24)");
		tput_line_at(col,   "MOVE WISP-FULL-SCREEN-ORDER-AREA TO");
		tput_clause (col+4, "WISP-CRT-ORDER-AREA");
		tput_line_at(col,   "CALL \"WDISPLAY\" USING WISP-CRT-RECORD");
	}
	
	if (period_node)
	{
		tput_clause(col,".");
	}
	tput_flush();

	the_statement = free_statement(the_statement);
	return( the_statement );
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
NODE parse_display_and_read(the_statement)
NODE the_statement;
{
	NODE	curr_node, temp_node;
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

	/*
	**	Free the DISPLAY AND portion.
	*/
	the_statement = free_statement(the_statement);

	/*
	**	Get the READ ... portion.
	*/
	the_statement = get_verb_statement();

	curr_node = the_statement->next;
	if (!eq_token(curr_node->token,VERB,"READ"))
	{
		write_log("WISP",'E',"DISPANDREAD","Expecting keyword READ found [%s].", token_data(curr_node->token));
		tput_statement(12,the_statement);
		the_statement = free_statement(the_statement);
		return(the_statement);
	}

	curr_node = curr_node->next;
	altered = 0;
	if (eq_token(curr_node->token,KEYWORD,"ALTERED"))
	{
		altered = 1;
		curr_node = curr_node->next;

		if (acn_cobol)
		{
			write_log("WISP",'E',"NATIVE","ALTERED clause on DISPLAY AND READ not supported with Native Screens");
		}
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

	curr_node = curr_node->next;							/* skip over the ON keyword		*/
	curr_node = curr_node->next;							/* skip over the ON keyword		*/

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

	curr_node = curr_node->next;

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
		on_pfkeys = 1;
		curr_node = curr_node->next;
		curr_node = curr_node->next;
		enter_key = 0;
		curr_node = dnr_pfkeys_clause(curr_node,"WISP-ON-PF-KEYS",col,enter_key,&on_key_cnt);
		/*
		**	At this point curr_node is the...
		**		NODE_END	End of statement
		**		PERIOD		End of statement (almost)
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
		tput_line_at(col, "    CONTINUE,");

		if (unhook_sub_tree(the_statement,curr_node))
		{
			free_statement(the_statement);
			the_statement = curr_node;
		}

		if (!eq_token(curr_node->token,KEYWORD,"NO-MOD"))
		{
		    NODE no_mod_node;
		    no_mod_node = NULL;

		    for(;;)
		    {
			if (eq_token(curr_node->token,KEYWORD,"END-DISPLAY")) break;
			if (eq_token(curr_node->token,KEYWORD,"ELSE")) break;
			if (eq_token(curr_node->token,KEYWORD,"WHEN")) break;

			for(temp_node=curr_node; NODE_END != temp_node->type; temp_node=temp_node->next)
			{
				if (PERIOD==temp_node->token->type)
				{
					period_found = 1;
					clean_token(temp_node->token);			/* Remove the period			*/
				}

				if (eq_token(temp_node->token,KEYWORD,"NO-MOD"))
				{
					no_mod_node = temp_node;
					unhook_sub_tree(the_statement,no_mod_node);
				}
			}

			the_statement = parse_verb_statement(the_statement);

			if (period_found)
			{
				curr_node = NULL;
				break;
			}
			if (no_mod_node)
			{
				the_statement = no_mod_node;
				curr_node = no_mod_node;
				break;
			}
			the_statement = get_verb_statement();
			curr_node = the_statement->next;
		    }
		}

		if (acn_cobol)
		{
			tput_line_at(col, "END-IF");
		}
		else
		{
			tput_line_at(col, "ELSE");
		}
	}

	no_mod = 0;
	if (!period_found && eq_token(curr_node->token,KEYWORD,"NO-MOD"))
	{
		no_mod = 1;

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
		tput_line_at(col, "    CONTINUE,");

		clean_token(curr_node->token);						/* Remove the NO-MOD			*/

		if (unhook_sub_tree(the_statement,curr_node))
		{
			free_statement(the_statement);
			the_statement = curr_node;
		}

		for(;;)
		{
			if (eq_token(curr_node->token,KEYWORD,"END-DISPLAY")) break;
			if (eq_token(curr_node->token,KEYWORD,"ELSE")) break;
			if (eq_token(curr_node->token,KEYWORD,"WHEN")) break;

			for(temp_node=curr_node; NODE_END != temp_node->type; temp_node=temp_node->next)
			{
				if (PERIOD==temp_node->token->type)
				{
					period_found = 1;
					clean_token(temp_node->token);			/* Remove the period			*/
				}
			}

			the_statement = parse_verb_statement(the_statement);

			if (period_found)
			{
				curr_node = NULL;
				break;
			}
			the_statement = get_verb_statement();
			curr_node = the_statement->next;
		}

		if (acn_cobol)
		{
			tput_line_at(col, "END-IF");
		}
		else
		{
			tput_line_at(col, "ELSE");
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
		}

		if (on_pfkeys)
		{
			tput_line_at(col, "END-IF");
		}
	}

	if (period_found)
	{
		tput_clause(col,".");
		tput_flush();
	}
	else
	{
		if (no_mod || on_pfkeys)
		{
			tput_flush();
		}

		if (eq_token(curr_node->token,KEYWORD,"END-DISPLAY"))
		{
			decontext_statement(curr_node);
			clean_token(curr_node->token);					/* Remove the END-DISPLAY		*/
		}
		else if (eq_token(curr_node->token,KEYWORD,"ELSE")) {}
		else if (eq_token(curr_node->token,KEYWORD,"WHEN")) {}
		else
		{
			decontext_statement(curr_node);
		}

		if (the_statement != curr_node)
		{
			if (unhook_sub_tree(the_statement,curr_node))
			{
				free_statement(the_statement);
				the_statement = curr_node;
			}
		}
		the_statement = parse_verb_statement(the_statement);
	}

	return(the_statement);
}

static NODE dnr_pfkeys_clause(first_node,target,col,enter_key,key_cnt)
NODE 	first_node;
char 	*target;
int	col;
int	enter_key;
int	*key_cnt;
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

		if (PERIOD==temp_node->token->type) break;
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

		boundary=72 - 16;
		if ((int)strlen(pfkeys) > boundary)					/* Won't fit on one line		*/
		{
			char	savchr;
			savchr = pfkeys[boundary];				/* save the char			*/
			pfkeys[boundary] = '\0';
			tput_line("           MOVE %s",pfkeys);			/* write first part			*/
			pfkeys[boundary] = savchr;
			tput_line("      -         \"%s",&pfkeys[boundary]);	/* write the rest			*/
		}
		else
		{
			tput_line("           MOVE %s",pfkeys);			/* write first part			*/
		}
		tput_clause(col, "TO %s,",target);				/* just finish the MOVE			*/
	}

	/*
	**	If non-numeric keys where found take a second pass thur.
	*/
	for(temp_node=first_node; nonnumeric_keys && NODE_END != temp_node->type; temp_node=temp_node->next)
	{
		if (temp_node->token && PERIOD==temp_node->token->type) break;
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
