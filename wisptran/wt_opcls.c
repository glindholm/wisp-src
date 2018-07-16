static char copyright[]="Copyright (c) 1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wt_opcls.c
**
**	Project:	WISP/TRAN
**
**	RCS:		$Source:$
**
**	Purpose:	OPEN and CLOSE verbs
**
*/

/*
**	Includes
*/

#define EXT extern
#include "wisp.h"
#include "crt.h"
#include "cobfiles.h"
#include "directiv.h"
#include "wcommon.h"
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


static char *open_mode_name[] = { "",	"WFOPEN-INPUT",
					"WFOPEN-SHARED",
					"WFOPEN-OUTPUT",
					"WFOPEN-EXTEND",
					"WFOPEN-SPECIAL-INPUT",
					"WFOPEN-I-O",
					"WFOPEN-SORT"};

/*
**	Static Function Prototypes
*/



/*
	OPEN { INPUT         } filename-1 [with NO REWIND] [filename-2 ...]  ....
	     { OUTPUT        }
	     { I-O           }
	     { SHARED        }
	     { EXTEND        }
	     { SPECIAL-INPUT }


	Example:

	OPEN INPUT A, B, C,
             OUTPUT D, E.
*/

NODE parse_open(NODE the_statement)
{
	NODE	curr_node, open_node, file_node, access_node;
	NODE	trailing_fluff_node = NULL;
	int	col, fnum;
	int	is_crt;
	int	found_crt = 0;
	int	file_cnt = 0;
	int	with_no_rewind = 0;
	int	open_mode;

	curr_node = first_token_node(the_statement);

	if (!eq_token(curr_node->token,VERB,"OPEN"))
	{
		write_tlog(curr_node->token,"WISP",'E',"VERB","Expected OPEN found [%s].", token_data(curr_node->token));
		return(the_statement);
	}

	open_node = curr_node;
	write_tlog(open_node->token,"WISP",'I',"OPEN","Processing OPEN Statement.");

	col = open_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_leading_fluff(the_statement);
	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	curr_node = open_node->next;
	for(;;)
	{
		/*
		**	Check for end conditions.
		*/
		if (!curr_node)
		{
			break;
		}
		if (NODE_END == curr_node->type)
		{
			break;
		}
		if (!curr_node->token)
		{
			curr_node = curr_node->next;
			continue;
		}

		fnum = -1;

		/*
		**	Check if next token is the open mode.
		*/

		if (KEYWORD == curr_node->token->type)
		{
			access_node = curr_node;
			
			if (eq_token(curr_node->token, KEYWORD,"SHARED"))
			{
				open_mode = WFOPEN_SHARED;
			}
			else if (eq_token(curr_node->token, KEYWORD,"SPECIAL-INPUT"))
			{
				open_mode = WFOPEN_SPECIAL_INPUT;
			}
			else if (eq_token(curr_node->token, KEYWORD,"I-O"))
			{
				open_mode = WFOPEN_I_O;
			}
			else if (eq_token(curr_node->token, KEYWORD,"INPUT"))
			{
				open_mode = WFOPEN_INPUT;
			}
			else if (eq_token(curr_node->token, KEYWORD,"OUTPUT"))
			{
				open_mode = WFOPEN_OUTPUT;
			}
			else if (eq_token(curr_node->token, KEYWORD,"EXTEND"))
			{
				open_mode = WFOPEN_EXTEND;
			}
			else
			{
				write_tlog(curr_node->token,"WISP",'F',"PARSE",
					   "Error parsing OPEN statement. Expecting open mode found [%s]",
					   token_data(curr_node->token));

				tput_statement(12, the_statement);
				return(free_statement(the_statement));
			}

			curr_node = curr_node->next;

		}
		else if (IDENTIFIER == curr_node->token->type)
		{
			/*
			**	Next token should be a file name(s).
			*/

			file_node = curr_node;
			is_crt = 0;

			if (do_xtab)
			{
				char tabData[80];	/* FILE \t ACCESS */
				sprintf(tabData,"%s\t%s", 
					token_data(file_node->token),
					token_data(access_node->token));

				xtab_log(context_infile_name(file_node->token->context), 
					file_node->token->line, "OPEN", tabData);
			}

			if (-1 != crt_index(token_data(file_node->token)))
			{
				is_crt = 1;
				found_crt = 1;

				tput_line_at(col,"CONTINUE");
			}
			else if (-1 != (fnum = file_index(token_data(file_node->token))))
			{
				prog_ref[fnum]++;					/* Say it was used.			*/
				file_cnt++;
			}
			else
			{
				file_cnt++;

				write_log("WISP",'E',"NOTFOUND",
				  "Error -- File %s, referenced by OPEN statement but not Declared.",
				  token_data(file_node->token));
			}

			curr_node = file_node->next;

			/*
			**	Step thru optonal keywords.
			*/
			with_no_rewind = 0;
			if (curr_node && eq_token(curr_node->token, KEYWORD, "WITH"))
			{
				curr_node = curr_node->next;
			}
			if (curr_node && eq_token(curr_node->token, KEYWORD, "NO"))
			{
				curr_node = curr_node->next;
			}
			if (curr_node && eq_token(curr_node->token, KEYWORD, "REWIND"))
			{
				curr_node = curr_node->next;
				with_no_rewind = 1;
			}


		}
		else
		{
			write_tlog(curr_node->token,"WISP",'E',"PARSE","Error parsing OPEN statement [%s].", 
				   token_data(curr_node->token));
		}
		
		/*
		**	Generate the OPEN logic.
		*/
		if (-1 != fnum)
		{
			char 	*open_access;
			char	*open_sharing_phrase;
			char	*open_lock;

			open_access = "UNKNOWN";
			open_sharing_phrase = "";
			open_lock = "";
			
			switch(open_mode)
			{
			case WFOPEN_SHARED:
				if (prog_ftypes[fnum] & (SEQ_DYN + SEQ_FILE))
				{
					/*
					 *  	A SEQ file OPEN SHARED is only allowed to WRITE to the end.
					 *	This is the same as OPEN EXTEND allowing multiple writers.
					 */
					open_access = "EXTEND";

					if (mfse_cobol)
					{
						open_sharing_phrase = "SHARING ALL";
					}

					if (mfoc_cobol)
					{
						write_log("WISP",'W',"OPEN-SHARED-SEQ",
							  "Micro Focus Object Cobol does not support shared access on SEQUENTIAL files. [OPEN SHARED %s]", 
							  token_data(file_node->token));
					}
				}
				else
				{
					open_access = "I-O";
				}

				if (acu_cobol||vax_cobol)
				{
					open_lock = "ALLOWING ALL";
				}
				      
				if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
				{
					write_log("WISP",'W',"AUTOLOCK","File %s OPEN SHARED using AUTOLOCK.",
						  token_data(file_node->token));
				}
				break;
				
			case WFOPEN_SPECIAL_INPUT:
				open_access = "INPUT";

				if (lpi_cobol||mf_cobol)
				{
					open_lock = "";
				}
				else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
				{
					open_lock = "";
				}
				else
				{
					open_lock = "ALLOWING ALL";
				}
				break;
				
			case WFOPEN_I_O:
				open_access = "I-O";

				if (lpi_cobol||mf_cobol)
					open_lock = "WITH LOCK";
				else if ((vax_cobol || acu_cobol) && (prog_ftypes[fnum] & OPENIOX_FILE))
					open_lock = "ALLOWING NO OTHERS";	/* OPEN I-O exclusive		*/
				else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
					open_lock = "";
				else
					open_lock = "ALLOWING READERS";		/* Manual record locking	*/
										/* Vax needs readers - READFDR	*/
				break;
				
			case WFOPEN_INPUT:
				open_access = "INPUT";

				if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
					open_lock = "";
				else if (vax_cobol || acu_cobol)		/* Manual record locking	*/
					open_lock = "ALLOWING READERS";
				break;
				
			case WFOPEN_OUTPUT:
				open_access = "OUTPUT";

				if (lpi_cobol||mf_cobol)
					open_lock = "WITH LOCK";
				else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
					open_lock = "";
				else
					open_lock = "ALLOWING NO OTHERS";
#ifdef OLD
					/*
					**	The "ALLOWING READERS" clause on the VAX was causing
					**	it to be very slow.  On the Wang an OPEN OUTPUT is exclusive,
					**	so the only reason for this must have been the "READFDR" problem.
					*/
				else
					open_lock = "ALLOWING READERS";
#endif
				break;
				
			case WFOPEN_EXTEND:
				open_access = "EXTEND";

				if (lpi_cobol||mf_cobol)
					open_lock = "WITH LOCK";
				else if (vax_cobol && (prog_ftypes[fnum] & AUTOLOCK))
					open_lock = "";
				else
					open_lock = "ALLOWING READERS";
				break;
				
			default:
				write_log("WISP",'F',"OPEN","Invalid OPEN mode %d for file %s",open_mode,
					  token_data(file_node->token));
				break;
			}

			if (isaproc)							/* Was the procedure flag set?		*/
			{
				tput_line_at(col, "MOVE ZERO      TO WISP-BIT-CLEAR");
				tput_line_at(col, "MOVE WISP-PROC TO WISP-BIT-SET");
				tput_line_at(col, "CALL \"wsetstat\" USING WISP-BIT-SET,");
				tput_clause(col+4, "WISP-BIT-CLEAR, %s",get_prog_status(fnum));
			}

			if (prog_ftypes[fnum] & PRINTER_FILE)
			{
				if (!prog_fnames[fnum][0])			/* If no FD FILENAME given then...		*/
				{
					tput_line_at(col, "MOVE SPACES TO %s",get_prog_fname(fnum));
				}
				else if (prog_fnames[fnum][0] == '\"')		/* If FD FILENAME is literal then...		*/
				{
					tput_line_at(col, "MOVE %s TO %s", prog_fnames[fnum], get_prog_fname(fnum));
				}
			}

			if (!(prog_ftypes[fnum] & NORESPECIFY))					/* Was the NORESPECIFY flag set?*/
			{									/* If RESPECIFY, then PERFORM it.*/
				tput_line_at(col, "PERFORM WITH TEST AFTER UNTIL");
				tput_clause(col+4, "%s < \"10\"", prog_fstats[fnum]);
				col += 4;
			}

			tput_line_at(col, "MOVE \"OP\" TO WISP-DECLARATIVES-STATUS");

			if (x4dbfile && (prog_ftypes[fnum] & INDEXED_FILE))
			{
				/*
				**	If an INDEXED  file and the x4dbfile flag set then
				**	add runtime logic to set the IS_DBFILE flag.
				*/
				tput_line_at(col, "CALL \"x4dbfile\" USING");
				tput_clause(col+4, "\"%s\",", token_data(file_node->token));
				tput_clause(col+4, "%s", get_prog_status(fnum));
			}

			tput_line_at(col, "CALL \"wfopen3\" USING");
			tput_clause(col+4,  "%s,", get_prog_status(fnum));	/* STATUS	*/
			tput_clause(col+4,  "%s,", get_prog_vname(fnum));	/* VOLUME	*/
			tput_clause(col+4,  "%s,", get_prog_lname(fnum));	/* LIBRARY	*/
			tput_clause(col+4,  "%s,", get_prog_fname(fnum));	/* FILE		*/
			tput_clause(col+4,  "%s,", get_prog_nname(fnum));	/* NATIVE	*/
			tput_clause(col+4,  "WISP-APPLICATION-NAME,");		/* Application	*/
			tput_clause(col+4,  "%s,", get_prog_prname(fnum));	/* PRNAME	*/
			tput_clause(col+4,  "%s", open_mode_name[open_mode]);	/* open mode	*/

			tput_line_at(col, "OPEN %s", open_access);

			if (strlen(open_sharing_phrase) > 0)
			{
				tput_clause(col+4, "%s", open_sharing_phrase);
			}
			
			tput_clause(col+4,  "%s", token_data(file_node->token));

			if (with_no_rewind)
			{
				if (mf_cobol && strlen(open_lock)>0)
				{
					/* MF doesn't allow both a LOCK and NO REWIND clause, NO REWIND is commentary */
					write_log("WISP",'W',"OPEN","NO REWIND clause has been removed from OPEN %s",
						  token_data(file_node->token));
				}
				else
				{
					tput_clause(col+4,  "WITH NO REWIND");
				}
			}

			tput_clause(col+4,  "%s", open_lock);

			if (vax_cobol)
			{
				tput_line_at(col, "CALL \"wdellock\" USING");
				tput_clause(col+4,  "%s", get_prog_status(fnum));	/* STATUS	*/
				tput_clause(col+4,  "%s", get_prog_nname(fnum));	/* NATIVE	*/
			}

			tput_line_at(col, "MOVE SPACES TO WISP-DECLARATIVES-STATUS");

			if (!(prog_ftypes[fnum] & NORESPECIFY))				/* Was the NORESPECIFY flag set?	*/
			{
				col -= 4;
				tput_line_at(col,"END-PERFORM");			/* No, End of the perform.		*/
			}
			
		}
		
	}

	isaproc = 0;									/* Clear the proc flag if needed.	*/

	the_statement = free_statement(the_statement);

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);
	
	return NULL;
}

/*
**	Seqential files:
**	CLOSE {filename [{REEL|UNIT} [for REMOVAL] with {NO REWIND|LOCK}]}...
**
**	Indexed and Relative files:
**	CLOSE {filename [with LOCK]} ...
**
*/
NODE parse_close(NODE the_statement)
{
	NODE	curr_node, close_node, file_node;
	NODE	printer_list = NULL;
	NODE	trailing_fluff_node = NULL;
	int	col, fnum;
	int	is_crt;
	int	found_crt = 0;
	int	file_cnt = 0;

	curr_node = first_token_node(the_statement);

	if (!eq_token(curr_node->token,VERB,"CLOSE"))
	{
		write_tlog(curr_node->token,"WISP",'E',"VERB","Expected CLOSE found [%s].", token_data(curr_node->token));
		return(the_statement);
	}

	close_node = curr_node;
	write_tlog(close_node->token,"WISP",'I',"CLOSE","Processing CLOSE Statement.");

	col = close_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_leading_fluff(the_statement);
	trailing_fluff_node = unhook_trailing_fluff(the_statement);

	curr_node = close_node->next;
	for(;;)
	{
		/*
		**	Check for end conditions.
		*/
		if (!curr_node)
		{
			break;
		}
		if (NODE_END == curr_node->type)
		{
			break;
		}
		if (!curr_node->token)
		{
			continue;
		}

		/*
		**	Next token should be a file name.
		*/
		file_node = curr_node;
		is_crt = 0;
		
		if (-1 != crt_index(token_data(file_node->token)))
		{
			is_crt = 1;

			if (acn_cobol)
			{
				write_log("WISP",'I',"NATIVE","Workstation CLOSE %s removed for Native Screens",
					  token_data(file_node->token));
				tput_line_at(col, "CONTINUE");
			}
			else
			{
				found_crt = 1;
			}

			free_token_from_node(file_node);
		}
		else if (-1 != (fnum = file_index(token_data(file_node->token))))
		{
			file_cnt++;
			
			if (PRINTER_FILE & prog_ftypes[fnum])
			{
				printer_list = makenode(NODE_STATEMENT, file_node, printer_list, NULL);
			}

			if_file_clear_lock_holder_id(fnum,col);				/* Add DMS locking logic		*/

		}
		else if (IDENTIFIER == file_node->token->type)
		{
			file_cnt++;

			write_tlog(file_node->token, "WISP",'E',"NOTFOUND",
				  "Error -- File %s, referenced by CLOSE statement but not Declared.",
				  token_data(file_node->token));
		}
		else
		{
			write_tlog(file_node->token,"WISP",'F',"PARSE",
				   "Error parsing CLOSE statement. Expecting file name found [%s]",
				   token_data(file_node->token));

			tput_statement(12, the_statement);
			return(free_statement(the_statement));
		}

		curr_node = file_node->next;

		/*
		**	Step thru optonal keywords.
		**
		**	Acucobol doesn't support "FOR REMOVAL" so remove.
		**	Remove all keywords for CRT's.
		*/
		if (curr_node && eq_token(curr_node->token, KEYWORD, "REEL"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "UNIT"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "FOR"))
		{
			if (is_crt || acu_cobol)
			{
				free_token_from_node(curr_node);
			}
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "REMOVAL"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			else if (acu_cobol)
			{
				write_tlog(curr_node->token,"WISP",'W',"CLOSE","FOR REMOVAL clause removed.");
				free_token_from_node(curr_node);
			}
			
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "WITH"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "NO"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "REWIND"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			curr_node = curr_node->next;
		}
		if (curr_node && eq_token(curr_node->token, KEYWORD, "LOCK"))
		{
			if (is_crt)
			{
				free_token_from_node(curr_node);
			}
			curr_node = curr_node->next;
		}

	}

	if (file_cnt)
	{
		tput_line_at(col,"MOVE \"CL\" TO WISP-DECLARATIVES-STATUS");
	}
	else
	{
		/*
		**	No files. Must have been a CRT.
		**	Remove the CLOSE verb.
		*/
		free_token_from_node(close_node);
	}
	
	/*
	**	Print the statement
	*/
	tput_statement(col,the_statement);

	if (found_crt)
	{
		tput_line_at(col,"CALL \"vwang\" USING VWANG-CLOSE-WS");
		tput_clause(col, "END-CALL");
	}

	/*
	**	Add wfclose calls for each printer file.
	*/
	while(printer_list)
	{
		NODE temp_node;

		/*
		**	NOTE: printer_list points to nodes in "the_statement"
		*/

		fnum = file_index(token_data(printer_list->down->token));
		if (-1 != fnum)
		{
			tput_line_at(col, "CALL \"wfclose\" USING %s", get_prog_nname(fnum));
			tput_clause(col,  "END-CALL");
		}
		else
		{
			write_tlog(printer_list->down->token,"WISP",'E',"CLOSE", "Printer file number not found [%s]",
				   token_data(printer_list->down->token));
		}

		temp_node = printer_list;

		printer_list = printer_list->next;

		wfree(temp_node);
	}

	the_statement = free_statement(the_statement);

	tput_statement(col, trailing_fluff_node);
	trailing_fluff_node = free_statement(trailing_fluff_node);
	
	return NULL;
	
}

/*
**	History:
**	$Log: wt_opcls.c,v $
**	Revision 1.24.2.1  2002/11/12 16:00:31  gsl
**	Applied global unique changes to be compatible with combined KCSI
**	
**	Revision 1.24  2002/03/21 23:26:40  gsl
**	FIx format
**	
**	Revision 1.23  2002-03-21 17:05:20-05  gsl
**	Add MFSE open sharing clause and MFOC warning on a OPEN SHARED seq-file.
**
**	Revision 1.22  2001-09-13 10:14:04-04  gsl
**	Add xtab_log of OPEN statements
**
**	Revision 1.21  1998-06-09 13:10:20-04  gsl
**	Fix header and rename record locking function for manual locking
**
**	Revision 1.20  1998-03-20 17:41:08-05  gsl
**	change to use get_prog_nname()
**	moved OLD to old
**	.c
**
**	Revision 1.19  1998-03-17 14:53:44-05  gsl
**	For ACN when we remove the CLOSE workstation replace it with a CONTINUE
**
**	Revision 1.18  1998-03-04 12:47:17-05  gsl
**	Add needed END-CALL's to generated code
**
**	Revision 1.17  1998-03-03 16:24:14-05  gsl
**	fix warnings
**
**	Revision 1.16  1998-03-03 10:17:14-05  gsl
**	remove flushes and add trailing fluff logic
**
**	Revision 1.15  1998-02-20 17:46:32-05  gsl
**	Fix period handling
**
**	Revision 1.14  1998-02-10 14:48:01-05  gsl
**	Re-write OPEN and CLOSE logic to use new-style.
**
**	Revision 1.13  1997-09-15 11:36:54-04  gsl
**	Removed CLOSE workstation for native screens
**
**	Revision 1.12  1997-09-12 14:03:42-04  gsl
**	change native warning
**
**	Revision 1.11  1997-09-12 13:28:02-04  gsl
**	Native screens warning for WS CLOSE
**
**	Revision 1.10  1996-08-30 21:56:22-04  gsl
**	drcs update
**
**
**
*/
