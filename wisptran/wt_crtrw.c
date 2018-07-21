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


#include <ctype.h>

#define EXT extern
#include "wisp.h"
#include "crt.h"
#include "scrn.h"
#include "cobfiles.h"
#include "statment.h"
#include "wt_procd.h"
#include "reduce.h"


/*
**	REWRITE crt-record 
**		[FROM identifier-1]
**		[after_clause]
**		[END-REWRITE]
**
**	after_clause:
**		[AFTER] 
**			ALARM
**			[SETTING] CURSOR [COLUMN] col-num [ROW|LINE] row-num
**			ROLL DOWN
**			ROLL UP
**			ERASE PROTECT
**			ERASE MODIFY
*/
NODE parse_rewrite_crt(int crt_num, NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	NODE	record_node;
	NODE	from_node = NULL;
	NODE	column_node = NULL;
	NODE	row_node = NULL;
	NODE	trailing_fluff_node = NULL;
	int	col;
	int	wcc_byte = 0;
	int	vwang_lines = 24;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"REWRITE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected REWITE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	if (opt_native_screens)		/* REWRITE CRT WARNING */
	{
		write_log("WISP",'W',"NATIVESCREENS","Workstation REWRITE %s uses WISP Screens",crt_record[crt_num]);
	}

	record_node = verb_node->next;

	tput_leading_fluff(the_statement);
	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	delint_statement(the_statement);
	decontext_statement(the_statement);
	
	curr_node = record_node->next;
	if (eq_token(curr_node->token,KEYWORD,"FROM"))
	{
		curr_node = curr_node->next;
		from_node = reduce_data_item(curr_node);
		if (!from_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"REWRITE",
				   "Unable to reduce FROM clause, clause will be ignored");
		}
		curr_node = curr_node->next;
	}

	if (eq_token(curr_node->token,KEYWORD,"AFTER"))
	{
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"SETTING"))
	{
		curr_node = curr_node->next;
	}

	if (eq_token(curr_node->token,KEYWORD,"ALARM"))
	{
		wcc_byte = 0xC0;						/* Set ALARM bit to WCC.		*/
		curr_node = curr_node->next;
	}
	else if (eq_token(curr_node->token,KEYWORD,"CURSOR"))
	{
		wcc_byte = 0xA0;						/* Set CURSOR-POS bit.			*/
		curr_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"COLUMN"))
		{
			curr_node = curr_node->next;
		}

		column_node = reduce_data_item(curr_node);
		if (!column_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"REWRITE",
				   "Unable to reduce CURSOR COLUMN clause, clause will be ignored");
		}
		curr_node = curr_node->next;

		if (eq_token(curr_node->token,KEYWORD,"ROW") ||
		    eq_token(curr_node->token,KEYWORD,"LINE"))
		{
			curr_node = curr_node->next;
		}

		row_node = reduce_data_item(curr_node);
		if (!row_node)
		{
			write_tlog(curr_node->token,"WISP",'E',"REWRITE",
				   "Unable to reduce CURSOR ROW clause, clause will be ignored");
		}
		curr_node = curr_node->next;

	}
	else if (eq_token(curr_node->token,KEYWORD,"ROLL"))
	{
		curr_node = curr_node->next;
		if (eq_token(curr_node->token,KEYWORD,"DOWN"))
		{
			wcc_byte = 0x90;						/* Set ROLL DOWN bit.			*/
			write_tlog(curr_node->token,"WISP",'W',"REWRITE",
				   "ROLL DOWN clause not supported at runtime");
		}
		else if (eq_token(curr_node->token,KEYWORD,"UP"))
		{
			wcc_byte = 0x88;						/* Set ROLL UP bit.			*/
			write_tlog(curr_node->token,"WISP",'W',"REWRITE",
				   "ROLL UP clause not supported at runtime");
		}
		else
		{
			write_tlog(curr_node->token,"WISP",'E',"REWRITE",
				   "ROLL clause expecting UP or DOWN found [%s]", token_data(curr_node->token));
		}
		curr_node = curr_node->next;
	}
	else if (eq_token(curr_node->token,KEYWORD,"ERASE"))
	{
		curr_node = curr_node->next;
		if (eq_token(curr_node->token,KEYWORD,"PROTECT"))
		{
			wcc_byte = 0x82;						/* Set ERASE PROTECT bit.		*/
		}
		else if (eq_token(curr_node->token,KEYWORD,"MODIFY"))
		{
			wcc_byte = 0x84;						/* Set ERASE MODIFY bit.		*/
		}
		else
		{
			write_tlog(curr_node->token,"WISP",'E',"REWRITE",
				   "ERASE clause expecting PROTECT or MODIFY found [%s]", token_data(curr_node->token));
		}
		curr_node = curr_node->next;
	}



											/* Go till we go beyond it.		*/
	for ( cur_crt=0; cur_crt<crt_fcount; cur_crt++) 
	{
		if (crt_prime_rec[cur_crt] > crt_num) 
		{
			cur_crt--; 
			break;
		}
	}

	if (cur_crt == crt_fcount) cur_crt--;

	if (from_node)
	{
		tput_line_at(col, "MOVE");
		tput_statement(col+4, from_node);
		tput_clause (col+4, "TO %s,",crt_record[crt_num]);
	}
	
	if (crt_relative[cur_crt][0])
	{
		tput_line_at(col, "CALL \"W99TOX\" USING %s,",crt_relative[cur_crt]);
		tput_clause (col+4, "%s",crt_record[crt_num]);
	}

	tput_line_at(col, "MOVE %s TO WISP-CRT-RECORD,",crt_record[crt_num]);

	if (wcc_byte)
	{
		tput_line_at(col, "MOVE WISP-SYMB-%d TO WISP-CRT-ORDER-AREA-2,",wcc_byte);
	}
	if (row_node)
	{
		tput_line_at(col, "MOVE");
		tput_statement(col+4, row_node);
		tput_clause(col+4, "TO WISP-DFIELD-0");
		tput_line_at(col, "CALL \"W99TOX\" USING WISP-DFIELD-0, WISP-CRT-ORDER-AREA-4");
	}
	if (column_node)
	{
		tput_line_at(col, "MOVE");
		tput_statement(col+4, column_node);
		tput_clause(col+4, "TO WISP-DFIELD-0");
		tput_line_at(col, "CALL \"W99TOX\" USING WISP-DFIELD-0, WISP-CRT-ORDER-AREA-3");
	}

	if (wcc_byte || row_node || column_node)
	{
		/*
		**  In WISP 4.4.02 and earlier "wmemcpy" -- conflicts with wide-characters on AIX
		**
		**  MOVE 4 TO WISP-LONGWORD
		**  CALL "WMEMCPY" USING {crt-record}, WISP-CRT-O-A, WISP-LONGWORD 
		**
		**  New:
		**  MOVE WISP-CRT-O-A TO {crt-record}(1:4)
		**
		*/
/*
**		tput_line_at	(col, "MOVE 4 TO WISP-LONGWORD");
**		tput_line_at	(col, "CALL \"WMEMCPY\" USING %s,",crt_record[crt_num]);
**		tput_clause	(col+4, "WISP-CRT-O-A, WISP-LONGWORD");
*/

		tput_line_at	(col, "MOVE WISP-CRT-O-A TO ");
		tput_clause	(col+4, "%s(1:4)", crt_record[crt_num]);
	}

	/*
	**	CALL "WS_REWRITE" USING WISP-CRT-RECORD, lines, status END-CALL
	*/
	vwang_lines = (crt_record_size[crt_num]-4)/80;
	if (vwang_lines != 24)
	{
		tput_line_at(col, "MOVE WISP-SYMB-%d TO VWANG-LINES,\n", vwang_lines);
	}
	tput_line_at(col, "CALL \"WS_REWRITE\" USING ");
	tput_clause (col+4, "WISP-CRT-RECORD,");
	if (vwang_lines != 24)
	{
		tput_clause (col+4, "VWANG-LINES,");
	}
	else
	{
		tput_clause (col+4, "VWANG-FULL-SCREEN,");
	}
	tput_clause (col+4, "%s",crt_status[cur_crt]);
	tput_clause (col, "END-CALL");

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);
	
	/*
	**	Anything else should be in the next fragment
	*/
	if (NODE_END != curr_node->type)
	{
		write_tlog(curr_node->token,"WISP",'E',"REWRITE",
			   "Error parsing REWRITE, found token [%s]", token_data(curr_node->token));
	}

	the_statement =  free_statement(the_statement);
	the_statement = get_statement_from_sentence(the_sentence);
	
	curr_node = the_statement->next;
	if (eq_token(curr_node->token,KEYWORD,"END-REWRITE"))
	{
		the_statement =  free_statement(the_statement);
	}
	
	return the_statement;
}


/*
**	History:
**	$Log: wt_crtrw.c,v $
**	Revision 1.26  2003/09/08 19:43:27  gsl
**	Change log entries for Native Screens
**	
**	Revision 1.25  2003/08/08 19:52:47  gsl
**	Add native screens comments
**	
**	Revision 1.24  2003/08/06 18:12:10  gsl
**	
**	Revision 1.23  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.22  2002/08/01 02:46:28  gsl
**	Replace vwang calls with WS_CLOSE WS_READ WS_READ_ALT WS_REWRITE
**	
**	Revision 1.21  2002/07/30 22:00:49  gsl
**	globals
**	
**	Revision 1.20  2002/07/26 19:20:43  gsl
**	
**	Revision 1.19  2002/07/10 01:35:59  gsl
**	fix wmemcpy to WMEMCPY
**	
**	Revision 1.18  1998/03/26 19:24:21  gsl
**	Change to use WISP-SYMB-xx
**	Moved OLD to old.c
**	
**	Revision 1.17  1998-03-04 15:23:05-05  gsl
**	set write control byte per the wang manual
**
**	Revision 1.16  1998-03-04 12:58:08-05  gsl
**	Add END-CALL
**
**	Revision 1.15  1998-03-03 14:56:44-05  gsl
**	Add fluff logic
**
**	Revision 1.14  1998-02-27 16:48:26-05  gsl
**	Rewrote for cobol-85
**
**	Revision 1.13  1997-09-15 13:57:38-04  gsl
**	fix warning
**
**	Revision 1.12  1997-09-12 14:02:52-04  gsl
**	make
**	change native warning
**
**	Revision 1.11  1997-09-12 13:24:38-04  gsl
**	Add Native Screens warning for WS REWRITE
**
**	Revision 1.10  1996-08-30 21:56:13-04  gsl
**	drcs update
**
**
**
*/
