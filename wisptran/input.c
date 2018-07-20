/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/*
**	File:		input.c
**
**	Purpose:	COBOL input line routines
**
**	Routines:
**	get_cobol_inline()		Get next COBOL line and place it into global inline.
**	get_cobol_line()		Get next COBOL line (handle held lines being reinserted into input stream)
**	get_next_cobol_line()		Get next COBOL line (handle COPY statements in the input stream)
**	get_conditional_cobol_line()	Get next COBOL line from a given file (handle conditional code)
**	get_clean_cobol_line()		Get next COBOL line from a given file (handle TABs and embedded NULLs)
**
**	gen_native_copybooks()		Generate native copybook paths.
**	handle_directives()		Scan a given comment line for WISP directives and handle.
**
**	hold_this_line()		Reinsert a line back into the COBOL input stream.
**	get_held_line()			Get a held line.
**	held_line()			Test if a line is held.
**
**
**	History:
**	05/27/93	Separated out of wt_io.c.  GSL
**
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifdef WIN32
#include <io.h>
#endif

#define EXT extern
#include "wisp.h"
#include "directiv.h"
#include "wispfile.h"
#include "token.h"
#include "node.h"
#include "lines.h"
#include "wmalloc.h"
#include "input.h"

#ifdef WIN32
#define access(filename, mode)	_access(filename, mode)
#endif


static void gen_native_copybooks(char *wcb, char *copy_cob, char *open_cob, char *file, char *lib, char *vol);
static void osd_gen_native_copybooks(char *wcb, char *copy_cob, char *open_cob, char *file, char *lib, char *vol);
static int handle_directives(char *the_line);

/*
**	Routine:	get_cobol_inline()
**
**	Function:	To get a cobol line into the global inline.
**
**	Description:	Get the next cobol line to be processed and load it into
**			the global inline buffer.  This will return held lines
**			that are re-inserted into the input stream.
**
**	Arguments:	None
**
**	Globals:
**	linein		The global COBOL input line buffer.
**
**	Return:
**	NORMAL_LINE	Line was not in conditional code, no special processing needed.
**	PROCESS_LINE	Line was in conditional code and is to be processed as a normal line.
**	NOPROCESS_LINE	Line was in conditional code and is NOT to be processed.
**	SPECIAL_COMMENT	Comment line that must always be written out.
**	EOF		End of ALL input, we are done!
**
**	Warnings:	None
**
**	History:	
**	05/18/93	Written by GSL
**
*/
int get_cobol_inline(void)
{
	int	line_status;

	line_status = get_cobol_line(linein,sizeof(linein));

	if (EOF == line_status)
	{
		set_end_of_input();
	}

	return(line_status);
}

/*
**	Routine:	get_cobol_line()
**
**	Function:	To get a cobol line.
**
**	Description:	This layer handles held lines being re-inserted into the input stream.
**			If there is a held line it returns it otherwise it gets the next new line.
**			When a new line is gotten it handles the directives.
**			A held line will return the last line_status.
**
**	Arguments:
**	the_line	The line to load
**	linesize	Max size of line
**
**	Globals:
**
**	Return:
**	NORMAL_LINE	Line was not in conditional code, no special processing needed.
**	PROCESS_LINE	Line was in conditional code and is to be processed as a normal line.
**	NOPROCESS_LINE	Line was in conditional code and is NOT to be processed.
**	SPECIAL_COMMENT	Comment line that must always be written out.
**	EOF		End of ALL input, we are done!
**
**	Warnings:	None
**
**	History:	
**	05/18/93	Written by GSL
**
*/
int get_cobol_line(char *the_line, int linesize)
{
	static int	line_status;

	if ( get_held_line(the_line) )					/* First check for externally held line		*/
	{
		tokenize(the_line, line_status);			/* Re-tokenize it, (use last line_status)	*/
	}
	else
	{
		line_status = get_next_cobol_line(the_line, linesize);	/* Get a new line				*/

		debug2_print_line(the_line);

		if (NORMAL_LINE==line_status || PROCESS_LINE==line_status)
		{
			if ( iscomment(the_line) )
			{
				handle_directives(the_line);
			}
		}
	}

	return line_status;
}

/*
**	Routine:	get_next_cobol_line()
**
**	Function:	To get the next cobol line from the input stream.
**
**	Description:	This is the layer that handles COPY statements.
**
**			It tokenizes as this is needed to process the COPY statements.
**
**			This layer keeps the line count statistics.
**
**			A line with a COPY statement will be returned as a COMMENT.
**			The native COPY statement (.wcb) will also be returned as a COMMENT.
**			(It will be SPECIAL_COMMENT to force it to be included in the .cob file.)
**			If generating copybooks the native COPY statement will
**			be returned as a NOPROCESS_LINE.
**
**			If a line contains data before the COPY keyword the line will be split
**			with the first part returning immediately, and the COPY statement
**			handled next time in.  Likewise if there is data after the COPY 
**			statement the line will be split and the COPY handled first with
**			the remaining data held until after the copybook is finished.
**
**			This layer also handles % COMMENTS by spliting them into two lines.
**
**	Arguments:
**	the_line	The line to load
**	linesize	Max size of line
**
**	Globals:
**	curr_cob_context
**
**	Return:
**	NORMAL_LINE	Line was not in conditional code, no special processing needed.
**	PROCESS_LINE	Line was in conditional code and is to be processed as a normal line.
**	NOPROCESS_LINE	Line was in conditional code and is NOT to be processed.
**	SPECIAL_COMMENT	Comment line that must always be written out.
**	EOF		End of ALL input, we are done!
**
**	Warnings:	None
**
**	History:	
**	05/15/93	Written by GSL
**
*/
int get_next_cobol_line(char *the_line, int linesize)
{
	static 	char	split_saveline[256];					/* Save rest of line until next time.		*/
	static	int	split_line_status = NORMAL_LINE;			/* Save line_status until next time.		*/
	static 	char	postcopy_saveline[256];					/* Save line until after copybook finished	*/
	static	int	postcopy_line_status = NORMAL_LINE;			/* Save line_status until after copybook	*/

	static	int 	copy_on_process_line = 0;				/* COPY stmt inside PROCESS $xxx_CODE		*/

	static	int	open_the_copybook = 0;					/* Flag to open the copybook			*/
	static	char	file_name[80], library_name[80], volume_name[80];	/* Wang style file name of copybook		*/
	static	char	wcb_copybook[80], lib_copybook[80], libopen_copybook[80];
	static	int	parse_step = 1;						/* COPY statement parsing step			*/

	static	int	copy_line_num = 0;					/* The line number of the COPY token		*/

	int		line_status;
	int		got_a_line;
	int		is_a_comment;
	int		tokcnt;
	int		fluff;
	int		found_copy;
	int		cache_index;
	TOKEN		tmptok;
	int		found_period;

	if (open_the_copybook)
	{
		if ( 1==open_the_copybook ) /* step 1 */
		{

			/*
			**	Setup split_saveline for processing this time thru
			*/

			if (!opt_gen_copylib)
			{
				/*
				**	Ready to open copybook but first put out the native COPY statement as a comment.
				*/
				sprintf(split_saveline, "      *COPY \"%s\".\n", wcb_copybook);
				split_line_status = SPECIAL_COMMENT;
			}
			else
			{
				/*
				**	If generating copybooks then write the native COPY copybook.lib statement.
				*/
				sprintf(split_saveline, "           COPY \"%s\".\n", lib_copybook);
				split_line_status = NOPROCESS_LINE;	
			}
			open_the_copybook = 2;

		}
		else /* step 2 */
		{
			/*
			**	The last time thru has set everything up for the copybook to be opened.
			*/
			open_the_copybook = 0;

			curr_cob_context = open_cob_context(wcb_copybook,libopen_copybook);
		}
	}

	is_a_comment = 0;
	got_a_line = 0;

	if ( split_saveline[0] )						/* If the line was split then			*/
	{
		strcpy(the_line, split_saveline);				/* Use it as the next input line		*/
		split_saveline[0] = (char)0;
		line_status = split_line_status;				/* Restore the line status also.		*/

		got_a_line = 1;							/* Process this COPY statement.			*/
	}

	while(!got_a_line)
	{
		/*
		**	Get the next cleaned and valid line from the current input file.
		*/
		line_status = get_conditional_cobol_line(the_line, linesize, 
							curr_cob_context->infile->a_file, copy_on_process_line);	

		if (EOF == line_status) 					/* If no more lines in current file.		*/
		{
			int	was_reading_copybook;

			was_reading_copybook = curr_cob_context->infile->is_copybook;	/* Record if we were in a copybook	*/

			close_cob_file(curr_cob_context->infile);		/* This file is empty, close it.		*/

			copy_on_process_line = 0;				/* Were out of the copy file			*/

			if (was_reading_copybook) 				/* If reading from a copybook			*/
			{
				curr_cob_context = main_cob_context;		/* Switch back to main context			*/

				if ( postcopy_saveline[0] )			/* If we were in a copybook and something left	*/
				{						/* from before the copybook.			*/
					strcpy(the_line, postcopy_saveline);	/* Use it as the next input line		*/
					postcopy_saveline[0] = '\0';
					line_status = postcopy_line_status;	/* Restore the line status also.		*/

					got_a_line = 1;				/* Process line for COPY statements		*/
				}
				else
				{
					/* 
					**	We were reading from a copybook, now loop around
					**	and read the next line from the main wcb file.
					*/
				}
			}
			else
			{
				/*
				**	NO MORE INPUT
				*/

				the_line[0] = (char)0;				/* clear the line				*/
				return( EOF );					/* Return with EOF flag				*/
			}
		}
		else
		{
			/*
			**	We got a new line.
			**	Keep the line count statistics.
			*/
			got_a_line = 1;

			increment_in_line_count();				/* Keep count of lines read			*/

			curr_cob_context->infile->line_count++;


			if ( iscomment(the_line) )
			{							/* Is it a COMMENT?				*/
				is_a_comment = 1;
				increment_comment_line_count();
			}
		}
	}


/*	copy_code = (line_status == PROCESS_LINE); */	/* ?????? */

	/*
	**	TOKENIZE the line.
	*/
	tokenize(the_line, line_status);


	/*
	**	If line does not need to be processed then return.
	*/
	if (is_a_comment || NOPROCESS_LINE == line_status)
	{
		return(line_status);
	}

	/*
	**	Handle % comment lines by spliting them into 2 lines.
	*/

	fluff = 1;
	tokcnt = token_cache_count();

	for(cache_index=0; cache_index<tokcnt; cache_index++)
	{
		/*
		**	Loop thru the token cache from tokenize checking each token
		*/
		token_cache_get(cache_index, &tmptok);

		if (PCOMMENT == tmptok.type)
		{
			if (fluff)
			{
				/*
				**	There was nothing significant on the line before the % COMMENT, 
				**	don't need to split the line.
				**	Make the whole line a comment, re-tokenize, and return.
				*/
				the_line[6] = '*';
				tokenize(the_line, line_status);
				return(line_status);
			}

			/*
			**	Split the line at the % COMMENT.
			**	The COMMENT portion is returned FIRST otherwise the COPY logic may also try to
			**	use the split_saveline buffer.
			*/
			strcpy(split_saveline, the_line);
			memset(the_line,' ',tmptok.column-1);			/* Blank the beginning up to the '%' char.	*/
			the_line[6] = '*';					/* Make '%' line a COMMENT			*/
			split_saveline[tmptok.column - 1] = '\n';		/* Remove the end starting at the '%' char.	*/
			split_saveline[tmptok.column]     = (char)0;
			split_line_status = line_status;

			tokenize(the_line, line_status);			/* Re-tokenize the split line.			*/

			return(line_status);					/* Return the COMMENT line			*/
		}
	
		if (!fluff_token(&tmptok))
		{
			fluff = 0;						/* There in non-fluff on the line		*/
		}
	}

	/*
	**	Handle COPY statements.
	*/

	if ( curr_cob_context->infile->is_copybook )				/* There are no COPY stmts in copybooks		*/
	{
		return(line_status);
	}

	/*
	**	We now try to parse out a COPY statement.
	**
	**	Parse step	1	2	3	4	5		6	7		8
	**			COPY	file	OF/IN	lib	OF/IN/ON	volume	SUPRESS		.
	**
	**	The COPY statement may be split over several lines so we have to keep
	**	track of which step we are on and restart on that step.
	*/

	tokcnt = token_cache_count();
	cache_index = 0;

	/*
	**	STEP (1) find the COPY verb.
	*/
	if ( 1 == parse_step )
	{
		found_copy = 0;
		fluff = 1;

		for(; cache_index<tokcnt; cache_index++)
		{
			/*
			**	Loop thru the token cache from tokenize checking each token
			*/
			token_cache_get(cache_index, &tmptok);

			if (eq_token(&tmptok,VERB,"COPY"))
			{
				found_copy = 1;
				copy_line_num = tmptok.line;
				break;
			}
	
			if (!fluff_token(&tmptok))
			{
				fluff = 0;
			}
		}
	
		if (!found_copy)
		{
			/*
			**	No COPY verb on this line.
			*/
			return(line_status);
		}

		/*
		**	A COPY VERB WAS FOUND
		*/

		if (PROCESS_LINE == line_status)
		{
			/*
			**	A COPY statement was found on a PROCESS_LINE.
			**	This info is needed for proper handling of the $xxx_CODE conditional code.
			*/
			copy_on_process_line = 1;
		}

		if (!fluff)
		{
			/*
			**	If non fluff was found on the line before the COPY verb
			**	then split the line and return the first half.
			*/

			strcpy(split_saveline, the_line);
			the_line[tmptok.column - 1] = '\n';			/* Remove the end starting at the COPY verb	*/
			the_line[tmptok.column]     = (char)0;
			memset(split_saveline,' ',tmptok.column-1);		/* Blank the beginning up to the COPY verb	*/
			split_line_status = line_status;

			tokenize(the_line, line_status);			/* Re-tokenize the split line.			*/
			return(line_status);
		}

		parse_step = 2;							/* Prepare for next parse step			*/
		cache_index++;
		file_name[0] = (char)0;
		library_name[0] = (char)0;
		volume_name[0] = (char)0;
	}

	the_line[6] = '*';							/* Make this a COMMENT line			*/

	/*
	**	STEP (2) 	COPY filename
	*/
	if ( 2 == parse_step )
	{
		for(; cache_index<tokcnt; cache_index++)
		{
			/*
			**	Loop thru the token cache from tokenize checking each token
			*/
			token_cache_get(cache_index, &tmptok);

			if ( IDENTIFIER == tmptok.type)
			{
				strcpy(file_name,tmptok.data);
				parse_step = 3;
				break;
			}
			else if ( LITERAL == tmptok.type )
			{
				remove_quotes(tmptok.data);
				sscanf(tmptok.data,"%s",file_name);
				parse_step = 3;
				break;
			}
			else if ( KEYWORD == tmptok.type || VERB == tmptok.type )
			{
				write_log("WISP",'I',"RESERVED","Using reserved word %s as a COPY filename.",tmptok.data);
				strcpy(file_name,tmptok.data);
				parse_step = 3;
				break;
			}
			else if (!fluff_token(&tmptok) && CONTINUATION != tmptok.type)
			{
				write_log("WISP",'F',"COPY", "Invalid COPY syntax, filename not found [%s].",tmptok.data);
				exit_with_err();
			}
		}

		if (3 == parse_step)						/* Prepare for next parse step			*/
		{
			cache_index++;
		}
		else								/* Return this COMMENT line			*/
		{
			tokenize(the_line, line_status);			/* Re-tokenize this COMMENT line.		*/
			return(line_status);
		}
	}

	/*
	**	All further parse steps are optional.
	**
	**	STEP (3 - 8) 	COPY filename OF/IN
	*/
	found_period = 0;

	for(; cache_index<tokcnt; cache_index++)
	{
		/*
		**	Loop thru the token cache from tokenize checking each token
		*/
		token_cache_get(cache_index, &tmptok);

		if ( PERIOD == tmptok.type)
		{
			found_period = 1;
			parse_step = 9;
			break;
		}

		if (eq_token(&tmptok,KEYWORD,"SUPPRESS"))
		{
			parse_step = 8;
		}
		else if ( (3==parse_step || 5==parse_step) &&
		          (eq_token(&tmptok,KEYWORD,"OF") ||
			   eq_token(&tmptok,KEYWORD,"IN") ||
			   eq_token(&tmptok,KEYWORD,"ON")   ) )
		{
			parse_step++;
		}
		else if ( (4==parse_step || 6==parse_step) && IDENTIFIER == tmptok.type)
		{
			if (4==parse_step)
				strcpy(library_name,tmptok.data);
			else
				strcpy(volume_name,tmptok.data);
			parse_step++;
		}
		else if ( (4==parse_step || 6==parse_step) && LITERAL == tmptok.type )
		{
			remove_quotes(tmptok.data);
			if (4==parse_step)
				sscanf(tmptok.data,"%s",library_name);
			else
				sscanf(tmptok.data,"%s",volume_name);
			parse_step++;
		}
		else if ( (4==parse_step || 6==parse_step) && (KEYWORD == tmptok.type || VERB == tmptok.type) )
		{
			if (4==parse_step)
			{
				write_log("WISP",'I',"RESERVED","Using reserved word %s as a COPY library.",tmptok.data);
				strcpy(library_name,tmptok.data);
			}
			else
			{
				write_log("WISP",'I',"RESERVED","Using reserved word %s as a COPY volume.",tmptok.data);
				strcpy(volume_name,tmptok.data);
			}
			parse_step++;
		}
		else if ( (3==parse_step || 5==parse_step || 7==parse_step || 8==parse_step) && 
		          (!fluff_token(&tmptok) && CONTINUATION != tmptok.type) )
		{
			/*
			** 	We got a problem. We should have seen a PERIOD or a valid keyword.
			**	We'll issue an error and go with what we got.
			*/
			write_log("WISP",'E',"COPY", "Invalid COPY syntax, missing period separator.");
			parse_step = 9;
			break;
		}
		else if (!fluff_token(&tmptok) && CONTINUATION != tmptok.type)
		{
			write_log("WISP",'F',"COPY", "Invalid COPY syntax, unrecognized token [%s].",tmptok.data);
			exit_with_err();
		}
	}

	if (9 != parse_step)							/* We are not done parsing.			*/
	{
		tokenize(the_line, line_status);				/* Re-tokenize this COMMENT line.		*/
		return(line_status);
	}

	/*
	**	We are DONE parsing the COPY statement.
	**	See if there is anything left on the line we need to save.
	*/

	if (found_period)
	{
		/*
		**	Search for the next significant token on the line.
		*/
		fluff = 1;

		for(cache_index++; cache_index<tokcnt; cache_index++)
		{
			token_cache_get(cache_index, &tmptok);
	
			if (!fluff_token(&tmptok))
			{
				fluff = 0;
				break;
			}
		}
	}
	else
	{
		/* We already have the next significant token loaded. */
		fluff = 0;
	}

	if ( !fluff )
	{
		/*
		**	Split the line for processing after the copybook.
		*/

		strcpy(postcopy_saveline, the_line);
		the_line[tmptok.column - 1] = '\n';				/* Remove the end starting after the COPY stmt	*/
		the_line[tmptok.column]     = (char)0;
		memset(postcopy_saveline,' ',tmptok.column-1);			/* Blank the beginning up thru the COPY stmt	*/
		postcopy_line_status = line_status;
	}

	strcpy(cpy_file,file_name);						/* Save the file name too.			*/
	strcpy(cpy_lib, library_name);						/* Save it.					*/

	gen_native_copybooks(wcb_copybook, lib_copybook, libopen_copybook, file_name, library_name, volume_name);

	parse_step = 1;								/* Reset the parse_step to look for COPY	*/
	open_the_copybook = 1;							/* Next time we open the copybook		*/

	if (opt_xtab)
	{
		xtab_log(context_infile_name(curr_cob_context), 
			copy_line_num, "COPY", wcb_copybook);
	}

	tokenize(the_line, line_status);					/* Re-tokenize this COMMENT line.		*/
	return(line_status);

}

/*
**	Routine:	get_conditional_cobol_line()
**
**	Function:	To get the next cobol line that belongs in the input stream 
**			from the given input file.
**
**	Description:	This routine handles the WISP conditional code layer of input.
**			It handles the $WISP_CODE, $UNIX_CODE conditional statements
**			and returns the next cobol line that belongs in the input stream.
**
**			This layer reverses the comment character (*) in column 7 for
**			conditional code so it is ready to use.	Deleted lines lines get 
**			changed into comments.  The '$' in the conditional code gets changed
**			to a '!' to indicate it has been done and so it won't be done
**			twice in case the code gets reprocessed with a TXT file.
**
**			This routine DOES return comment lines and NOPROCESS lines, including
**			comment lines containing conditional code statements.
**
**			This layer does NOT handle COPY statements, it is up to a higher
**			layer to switch the input stream.
**
**	Arguments:
**	the_line	The line to load
**	linesize	Max size of line
**	the_file	Open file handle to use.
**	copy_within_conditional_code
**			We are processing a COPY stmt that was in conditional code.
**
**	Globals:
**	acu_cobol
**	mf_cobol
**
**	Return:
**	NORMAL_LINE	Line was not in conditional code, no special processing needed.
**	PROCESS_LINE	Line was in conditional code and is to be processed as a normal line.
**	NOPROCESS_LINE	Line was in conditional code and is NOT to be processed.
**	EOF		End Of File
**
**	Warnings:	None
**
**	History:	
**	05/13/93	Written by GSL
**
*/
int get_conditional_cobol_line(char *the_line, int linesize, A_file *the_file, int copy_within_conditional_code)
{
#define WANG_CODE	1
#define VAX_CODE	2
#define LPI_CODE	3
#define ACU_CODE	4
#define AIX_CODE	5
#define MF_CODE		6
#define UNIX_CODE	7
#define DOS_CODE	8
#define COPY_CODE       9
#define DMF_CODE	10
#define ACN_CODE	11

	static char *code_else[] ={ 
		"",	
		"$WANG_ELSE",	
		"$VAX_ELSE",	
		"$LPI_ELSE",	
		"$ACU_ELSE",	
		"$AIX_ELSE",
		"$MF_ELSE",	
		"$UNIX_ELSE",	
		"$DOS_ELSE",	
		"$COPY_ELSE",	
		"$DMF_ELSE",
		"$ACN_ELSE"};

	static char *code__else[] ={ 
		"",
		"$WANG-ELSE",	
		"$VAX-ELSE",
		"$LPI-ELSE",
		"$ACU-ELSE",
		"$AIX-ELSE",
		"$MF-ELSE",
		"$UNIX-ELSE",
		"$DOS-ELSE",
		"$COPY-ELSE",
		"$DMF-ELSE",
		"$ACN-ELSE"};

	static char *code_end[] ={
		"",
		"$WANG_END",
		"$VAX_END",
		"$LPI_END",
		"$ACU_END",
		"$AIX_END",
		"$MF_END",
		"$UNIX_END",
		"$DOS_END",
		"$COPY_END",
		"$DMF_END",
		"$ACN_END"};

	static char *code__end[] ={
		"",
		"$WANG-END",
		"$VAX-END",
		"$LPI-END",
		"$ACU-END",
		"$AIX-END",
		"$MF-END",
		"$UNIX-END",
		"$DOS-END",
		"$COPY-END",
		"$DMF-END",
		"$ACN-END"};

	static	int	xxx_code = 0;						/* Flag and index while in CODE/ELSE/END	*/
	static	int	xxx_else = 0;						/* Flag while in ELSE/END			*/
	static	int	conditional_mode = 0;
#define	MODE_DELETE	1
#define MODE_PROCESS	2
#define	MODE_NOPROCESS	3

	int	comment_line;
	int	rc;


	rc = get_clean_cobol_line(the_line, linesize, the_file);		/* Get the next line (sanitized)		*/

	if (EOF == rc) return(rc);						/* Return EOF					*/

	if (strlen(the_line) > 6 && the_line[6] == '%')				/* Special case comment of % in col=7		*/
	{
		the_line[6] = '*';						/* Change to be a standard comment		*/
	}

	comment_line = iscomment(the_line);					/* Is it a comment?				*/


	if (!xxx_code)								/* Not inside of CODE/ELSE/END conditionals	*/
	{
		if (!comment_line)						/* Not a comment line				*/
		{
			return(NORMAL_LINE);					/* Regular line to be processed			*/
		}

		/* 
		**	This is a COMMENT line so Look for $xxx_CODE 
		*/

		if ((strpos(the_line,"$WANG_CODE") != -1) ||
		    (strpos(the_line,"$WANG-CODE") != -1)    )
		{
			write_log("WISP",'I',"WANGDELETE","WANG-only code being deleted.");
			xxx_code = WANG_CODE;
		}
		else if ((strpos(the_line,"$COPY_CODE") != -1) ||
		         (strpos(the_line,"$COPY-CODE") != -1)    )
		{
			write_log("WISP",'I',"COPYCODE","Start code being copied verbatim.");
			xxx_code = COPY_CODE;
		}
		else if (acu_cobol && ((strpos(the_line,"$ACU_CODE") != -1) ||
		     		       (strpos(the_line,"$ACU-CODE") != -1)    ))
		{
			write_log("WISP",'I',"ACUCOPY","Start Copy of ACU code.");
			xxx_code = ACU_CODE;
		}
		else if (acu_cobol && opt_native_screens &&	/* $ACN-CODE */
				((strpos(the_line,"$ACN_CODE") != -1) ||
		     		 (strpos(the_line,"$ACN-CODE") != -1)    ))
		{
			write_log("WISP",'I',"ACNCOPY","Start Copy of ACN code.");
			xxx_code = ACN_CODE;
		}
		else if (mf_cobol  &&
					((strpos(the_line,"$MF_CODE") != -1) ||
		            		 (strpos(the_line,"$MF-CODE") != -1)    ))
		{
			write_log("WISP",'I',"MFCOPY","Start Copy of MF code.");
			xxx_code = MF_CODE;
		}
		else if (unix_cobol && ((strpos(the_line,"$UNIX_CODE") != -1) ||
		      		        (strpos(the_line,"$UNIX-CODE") != -1)    ))
		{
			write_log("WISP",'I',"UNIXCOPY","Start Copy of UNIX code.");
			xxx_code = UNIX_CODE;
		}

		if (xxx_code)							/* Start of conditional code was found		*/
		{
			if (WANG_CODE == xxx_code)
			{
				conditional_mode = MODE_DELETE;
			}
			else
			{
				conditional_mode = (strpos(the_line,"PROCESS") != -1) ? MODE_PROCESS : MODE_NOPROCESS;
			}
			*((char *)strchr(the_line,'$')) = '!';			/* Change the '$' to a '!'			*/

			return(PROCESS_LINE);
		}

		return(NORMAL_LINE);						/* Regular COMMENT line to be processed		*/
	}


	/*
	**	Currently within CODE/ELSE/END conditionals.
	*/

	if (comment_line)							/* This is a comment line 			*/
	{
		if (!xxx_else)							/* Look for $xxx_ELSE				*/
		{
			if ((strpos(the_line, code_else[xxx_code]) != -1) ||
			    (strpos(the_line,code__else[xxx_code]) != -1)   )
			{
				write_log("WISP",'I',"ELSECODE","%s found.",code_else[xxx_code]);
				xxx_else = xxx_code;

				if (WANG_CODE == xxx_code)
				{
					conditional_mode = (strpos(the_line,"PROCESS") != -1) ? MODE_PROCESS : MODE_NOPROCESS;
				}
				else
				{
					conditional_mode = MODE_DELETE;
				}
				*((char *)strchr(the_line,'$')) = '!';		/* Change the '$' to a '!'			*/

				return(PROCESS_LINE);
			}
		}


		if ((strpos(the_line, code_end[xxx_code]) != -1) ||		/* look for $xxx_END 				*/
		    (strpos(the_line,code__end[xxx_code]) != -1)   )
		{
			write_log("WISP",'I',"ENDCODE","%s found.",code_end[xxx_code]);
			xxx_else = 0;
			xxx_code = 0;
			conditional_mode = 0;

			*((char *)strchr(the_line,'$')) = '!';			/* Change the '$' to a '!'			*/

			return(PROCESS_LINE);
		}

	}


	if (MODE_DELETE == conditional_mode)					/* Delete conditional lines.			*/
	{
		if (strlen(the_line) < 7)
		{
			strcat(the_line,"       ");				/* Pad out short line to at least column 7	*/
		}
		the_line[6] = '*';						/* Make line a comment				*/

		return(PROCESS_LINE);
	}

	if (copy_within_conditional_code)
	{
		/*
		**	We are processing a copybook that was included by a COPY statement
		**	that was in conditional code.  So we don't have to worry about
		**	reversing comment '*' characters.
		*/
		return( (MODE_PROCESS==conditional_mode) ? PROCESS_LINE : NOPROCESS_LINE );
	}


	if (strlen(the_line) < 7)
	{
		/*
		**	Line is too short to process comment '*' characters.
		*/
		return( (MODE_PROCESS==conditional_mode) ? PROCESS_LINE : NOPROCESS_LINE );
	}


	/*
	**	If this is a regular comment then uncomment it. 
	**	If not a regular comment then don't change anything -- assume what it's correct as it.
	**
	**	To allow comments and continuation inside of $xxx_CODE you put the '*', '-', or '/'
	**	into column 8 and WISP will move it into column 7 after it removes the '*'.
	*/
        if ('*' == the_line[6])
	{
	         the_line[6] = ' ';						/* remove the comment character			*/

		 if ('*'==the_line[7] || '-'==the_line[7] || '/'==the_line[7])
		 {
		        the_line[6] = the_line[7];				/* Shift the '*', '-', or '/' to col 7		*/
			the_line[7] = ' ';					/* blank out column 8.				*/
		 }
	}

	return( (MODE_PROCESS==conditional_mode) ? PROCESS_LINE : NOPROCESS_LINE );
}

/*
**	Routine:	get_clean_cobol_line()
**
**	Function:	To get the next line of cobol source and sanitize it.
**
**	Description:	This routine should return the next line of cobol source.
**			The line should have a NL as the last character before the NULL,
**			it should have no embedded nulls, TAB characters should be
**			expanded into spaces.
**
**			If if no NL is found before the NULL then the line is cut-off
**			at 80 and any NULLs before 80 are changed to spaces.  (we initialize
**			the first 80 chars to spaces prior to the read.)
**
**	Arguments:
**	the_line	The line to load
**	linesize	Max size of line
**	the_file	Open file handle to use.
**
**	Globals:	None
**
**	Return:
**	0		Success
**	EOF		End of file
**
**	Warnings:	On an error this routine will log an error message and exit.
**
**	History:	
**	05/13/93	Written by GSL
**
*/
int get_clean_cobol_line(char *the_line, int linesize, A_file *the_file)
{
#ifdef unix
extern	int	errno;
#endif

	char	*ptr;

	memset(the_line,' ',80);						/* Init the first 80 chars to spaces		*/

	if (NULL == wfgets(the_line,linesize,the_file->file))			/* Read next line				*/
	{
		if (feof(the_file->file))					/* Check for End Of File			*/
		{
			return EOF;						/* Return EOF					*/
		}

		write_log("WISP",'F',"WFGETS","file=%s: %s [errno=%d]",the_file->name,strerror(errno),errno);
		exit_with_err();
	}

	if (!strchr(the_line,'\n'))						/* if no NL before the NULL			*/
	{									/* We have NULL's in the data			*/
		the_line[80] = '\n';						/* Cut off line at 80.				*/
		the_line[81] = (char)0;

		for(ptr=the_line; *ptr != '\n'; ptr++)
		{
			if (! *ptr) *ptr = ' ';					/* Change all NULL's to spaces			*/
		}
		ptr++;								/* Point past the NL				*/
		*ptr = (char)0;
	}

	untabstr(the_line,linesize);						/* Space out TAB characters			*/

	debug1_print_line(the_line);

	return 0;								/* Success					*/
}

/*
**	Routine:	gen_native_copybooks()
**
**	Function:	To generate the native copybook names.
**
**	Description:	This routine generates the native filepath for copybooks from the
**			Wang style names.
**			The name used in the COPY statement may be different then
**			the name used to open the file.  On VMS this happens if we were requested
**			to pre-translate the logicals, and on UNIX if the -P prefixpath option
**			is used.
**
**	Arguments:
**	wcb		The native wcb filepath to generate.
**	copy_cob	The native cob filepath to generate for use in the COPY statement.
**	open_cob	The native cob filepath to generate for use in opening the cob file.
**	file		Wang style file name
**	lib		Wang style library name (may be blank)
**	vol		Wang style volume name  (NOT USED)
**
**	Globals:
**	opt_cli_ildir	/INLIB	-I path
**	opt_cli_prefixpath		-P prefixpath
**	t_desc
**	r_desc
**
**	Return:		None
**
**	Warnings:	The VAX translate logicals logic was copied verbatem from earlier code.
**
**	History:	
**	05/15/93	Written by GSL
**	05/17/94	Split into osd routines and added the -P logic. GSL
**
*/
static void gen_native_copybooks(char *wcb, char *copy_cob, char *open_cob, char *file, char *lib, char *vol)
{
	write_log("WISP",'I',"COPYBOOK", "file=%s lib=%s vol=%s",file,lib,vol);

	if (!lib[0] && !opt_cli_ildir[0])
	{
		write_log("WISP",'I',"NOINLIB","No INLIB specified for COPY %s.",file);
	}

	osd_gen_native_copybooks(wcb, copy_cob, open_cob, file, lib, vol);

	write_log("WISP",'I',"COPYBOOK", "wcb=%s copy_cob=%s open_cob=%s",wcb,copy_cob,open_cob);
	return;
}

const char* copybookext(void)
{
	static char *the_ext = NULL;

	if (the_ext == NULL)
	{
		if (opt_copybook_ext_cpy) 
		{
			the_ext = "cpy";
		}
		else if (opt_copybook_ext_lib) 
		{
			the_ext = "lib";
		}
		else
		{
			the_ext = "cob";
		}
	}
	return the_ext;
}

static void osd_gen_native_copybooks(char *wcb, char *copy_cob, char *open_cob, char *xfile, char *xlib, char *xvol)
{
	char	lfile_wcb[80], lfile_lib[80]; /* lowercase */
	char	ufile_wcb[80], ufile_lib[80]; /* uppercase */
	char	ufile[80], ulib[80];
	char	lfile[80], llib[80];

	char	pathprefix[80];
	char	*pathptr;
	int	found_copybook = 0;

	strcpy(ufile, xfile);
	strcpy(ulib, xlib);
	uppercase(ufile);
	uppercase(ulib);

	strcpy(lfile, xfile);
	strcpy(llib, xlib);
	lowercase(lfile);
	lowercase(llib);

	if (xlib[0])
	{
		sprintf(ufile_wcb,"%s%s%s.wcb",ulib,DSS_STR,ufile);
		sprintf(ufile_lib,"%s%s%s.%s",ulib,DSS_STR,ufile,copybookext());

		sprintf(lfile_wcb,"%s%s%s.wcb",llib,DSS_STR,lfile);
		sprintf(lfile_lib,"%s%s%s.%s",llib,DSS_STR,lfile,copybookext());
	}
	else
	{
		sprintf(ufile_wcb,"%s.wcb",ufile);
		sprintf(ufile_lib,"%s.%s",ufile,copybookext());

		sprintf(lfile_wcb,"%s.wcb",lfile);
		sprintf(lfile_lib,"%s.%s",lfile,copybookext());
	}		

	/*
	**	Generate the WCB copybook name.
	**
	**	lib	opt_cli_ildir	opt_cli_prefixpath		wcb			open			copy
	**	N	N		x			file.wcb		file.lib		file.lib
	**	Y	x		N			../lib/file.wcb		../lib/file.lib		../lib/file.lib
	**	N	Y		x			Ipath/file.wcb		Ipath/file.lib		Ipath/file.lib
	**	Y	x		Y			Ppath/lib/file.wcb	Ppath/lib/file.lib	lib/file.lib
	*/

	if (!xlib[0] && !opt_cli_ildir[0])
	{
		/* Assume upper */
		strcpy(wcb,ufile_wcb);
		strcpy(open_cob,ufile_lib);
		strcpy(copy_cob,ufile_lib);
		if (0==access(wcb,0))
		{
			return;
		}

		/* Try lower */
		strcpy(wcb,lfile_wcb);
		if (0==access(wcb,0))
		{
			strcpy(open_cob,lfile_lib);
			strcpy(copy_cob,lfile_lib);
			return;
		}

		/* Default to upper */
		strcpy(wcb,ufile_wcb);
		return;
	}

	if (xlib[0] && !opt_cli_prefixpath[0])
	{
		/* Assume upper */
		sprintf(wcb,     "..%s%s",DSS_STR,ufile_wcb);
		sprintf(open_cob,"..%s%s",DSS_STR,ufile_lib);
		sprintf(copy_cob,"..%s%s",DSS_STR,ufile_lib);
		if (0==access(wcb,0))
		{
			return;
		}

		/* Try lower */
		sprintf(wcb,     "..%s%s",DSS_STR,lfile_wcb);
		if (0==access(wcb,0))
		{
			sprintf(open_cob,"..%s%s",DSS_STR,lfile_lib);
			sprintf(copy_cob,"..%s%s",DSS_STR,lfile_lib);
			return;
		}

		/* Default to upper */
		sprintf(wcb,     "..%s%s",DSS_STR,ufile_wcb);
		return;
	}

	/*
	**	If no library then search the -I path else search the -P path.
	*/

	found_copybook = 0;

	if (!xlib[0])
	{
		pathptr = opt_cli_ildir;
	}
	else
	{
		pathptr = opt_cli_prefixpath;
	}

	/*
	**	Loop thru the path checking each directory until we find a match or
	**	run out of dirs.
	*/
	while(*pathptr)
	{
		int	i;

		/*
		**	Extract the directories out of the path and into pathprefix.
		*/
		for(i=0; *pathptr && *pathptr != PSS_CHAR; pathptr++, i++)
		{
			pathprefix[i] = *pathptr;
		}
		pathprefix[i] = (char)0;
		pathptr++;			/* Skip over the path seperator char		*/
		if (0==i)
		{
			strcpy(pathprefix,".");	/* Empty prefix use "."				*/
		}

		sprintf(wcb,"%s%s%s",pathprefix,DSS_STR,ufile_wcb);

		/*
		**	Break if we find a match.
		*/
		if (0==access(wcb,0)) 
		{
			/* Default is uppercase so just break */
			found_copybook = 1;
			break;
		}

		/* Try lowercase */
		sprintf(wcb,"%s%s%s",pathprefix,DSS_STR,lfile_wcb);
		if (0==access(wcb,0)) 
		{
			/* Copy lower unto upper so end logic works using the upper fields */
			strcpy(ufile_lib, lfile_lib);
			found_copybook = 1;
			break;
		}
	}

	if (!found_copybook)
	{
		/* Assume upper */
		strcpy(wcb,ufile_wcb);
		strcpy(open_cob,ufile_lib);
		strcpy(copy_cob,ufile_lib);
		if (0==access(wcb,0))
		{
			return;
		}

		/* Try lower */
		strcpy(wcb,lfile_wcb);
		if (0==access(wcb,0))
		{
			strcpy(open_cob,lfile_lib);
			strcpy(copy_cob,lfile_lib);
			return;
		}

		/* Default to upper */
		strcpy(wcb,ufile_wcb);
		return;
	}

	sprintf(open_cob,"%s%s%s",pathprefix,DSS_STR,ufile_lib);

	if (!xlib[0])
	{
		sprintf(copy_cob,"%s%s%s",pathprefix,DSS_STR,ufile_lib);
	}
	else
	{
		/*
		**	When using the prefixpath we don't use the full path in
		**	the COPY statements only the lib/file.lib part.
		*/
		strcpy(copy_cob, ufile_lib);
	}

	return;
}


/*
**	Routine:	handle_directives()
**
**	Function:	To scan a comment line for WISP directives and set the global flags.
**
**	Description:	This routine is passed a comment line from the input stream, it scans
**			it for directives and if found it set the global flags.
**
**	Arguments:
**	the_line	The input comment line
**
**	Globals:
**	range_count
**	isaproc
**	autolockprint
**	autolockfile
**	compressfile
**	multiplelock
**	nooptional
**	seqline
**	seqbinary
**	sortfile
**	selecttext
**
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	05/27/93	Separated from wt_io.c. GSL
**
*/
static int handle_directives(char *the_line)
{
	int	i;
	char	*ptr;
	char	buff[80];

	if ((i = strpos(the_line,"$RANGE_COUNT")) != -1)			/* Get screen record range count	*/
	{
		i += 13;
		sscanf(&the_line[i],"%d",&range_count);				/* now find the count			*/
		write_log("WISP",'I',"RANGESET","Range count set to %d.",range_count);
	}

	if ((i = strpos(the_line,"$WANG_PROC")) != -1)				/* look for the PROC keyword		*/
	{
		/* isaproc = 1; */
		write_log("WISP",'W',"$WANG_PROC","$WANG_PROC is an obsolete directive.");
	}

	if ((i = strpos(the_line,"$AUTOLOCKPRINT")) != -1)			/* On VMS use automatic locking on 	*/
	{									/* PRINT files.				*/
		autolockprint = 1;						/* Set the flag.			*/
		write_log("WISP",'I',"AUTOLOCKPRINT","On VMS print files will use automatic record locking.");
	}

	if ((i = strpos(the_line,"$NOAUTOLOCKPRINT")) != -1)			/* On VMS don't use automatic locking  	*/
	{									/* on PRINT files.			*/
		autolockprint = 0;						/* Clear the flag.			*/
		write_log("WISP",'I',"NOAUTOLOCKPRINT","On VMS print files will NOT use automatic record locking.");
	}

	if ((i = strpos(the_line,"$AUTOLOCKFILE")) != -1)			/* On VMS use automatic locking on 	*/
	{									/* the specified file.			*/
		autolockfile = 1;						/* Set the flag.			*/
		write_log("WISP",'I',"AUTOLOCKFILE","On VMS this file will use automatic record locking.");
	}

	if ((i = strpos(the_line,"$NOAUTOLOCKFILE")) != -1)			/* On VMS don't use automatic locking 	*/
	{									/* on the specified file.		*/
		noautolockfile = 1;						/* Set the flag.			*/
		write_log("WISP",'I',"NOAUTOLOCKFILE","On VMS this file will NOT use automatic record locking.");
	}

	if ((i = strpos(the_line,"$COMPRESSFILE")) != -1)			/* Add COMPRESSION to next file. 	*/
	{
		compressfile = 1;
		write_log("WISP",'I',"COMPRESSFILE","Add COMPRESSION to next file.");
	}

	if ((i = strpos(the_line,"$MULTIPLELOCK")) != -1)			/* Add LOCK MODE IS AUTOMATIC 		*/
	{									/*     WITH LOCK ON MULTIPLE RECORDS	*/
		multiplelock = 1;
		write_log("WISP",'I',"MULTIPLELOCK","Add WITH LOCK ON MULTIPLE RECORDS to next file.");
	}

	if ((i = strpos(the_line,"$NOOPTIONAL")) != -1)
	{
		nooptional = 1;
		write_log("WISP",'I',"NOOPTIONAL","Suppress OPTIONAL clause on next file.");
	}

	if ((i = strpos(the_line,"$SEQLINE")) != -1)
	{
		seqline = 1;
		write_log("WISP",'I',"SEQLINE","Force to LINE SEQUENTIAL.");
	}

	if ((i = strpos(the_line,"$SEQBINARY")) != -1)
	{
		seqbinary = 1;
		write_log("WISP",'I',"SEQBINARY","Force to BINARY SEQUENTIAL.");
	}

	if ((i = strpos(the_line,"$SEQRECORD")) != -1)
	{
		seqbinary = 1;
		write_log("WISP",'I',"SEQRECORD","Force to RECORD SEQUENTIAL.");
	}

	if ((i = strpos(the_line,"$SORTFILE")) != -1 ||
	    (i = strpos(the_line,"$SORT_FILE")) != -1   )
	{
		sortfile = 1;
		write_log("WISP",'I',"SORTFILE","Next file is a SORTFILE.");
	}

	if ((i = strpos(the_line,"$SELECTTEXT")) != -1)
	{
		write_log("WISP",'I',"SELECTTEXT","Force TEXT into SELECT statement.");
		memcpy(buff,the_line,72);
		buff[72] = (char)0;
		memset(buff,' ',7);
		stredt(buff,"$SELECTTEXT","     ");
		ptr = (char *)strchr(buff,'\n');
		if (ptr) *ptr = '\0';
		strcat(selecttext,"\n");					/* Start with a newline			*/
		strcat(selecttext,buff);
		if (strlen(selecttext) >= sizeof(selecttext))
		{
			write_log("WISP",'F',"SELECTTEXT","Too much $SELECTTEXT; buffer overflow!");
			exit_wisp(EXIT_WITH_ERR);
		}
	}

	if ((i = strpos(the_line,"$DBFILE")) != -1)
	{
		dbfile = 1;
		memcpy(buff,the_line,72);
		buff[72] = (char)0;
		memset(buff,' ',7);
		stredt(buff,"$DBFILE","       ");
		ptr = (char *)strchr(buff,'\n');
		if (ptr) *ptr = '\0';
		dbfile_tabname[0] = (char)0;
		sscanf(buff, "%s", dbfile_tabname);
		write_log("WISP",'I',"DBFILE","Next file is a DBFILE.");
	}

	if ((i = strpos(the_line,"$OPENIOEXCLUSIVEFILE")) != -1)
	{
		openioxfile = 1;
		write_log("WISP",'I',"OPENIOX","Next file is OPENIOEXCLUSIVE.");
	}

	if ((i = strpos(the_line,"$OPENIOEXCLUSIVEALL")) != -1)
	{
		openioxall = 1;
		write_log("WISP",'I',"OPENIOX","All files are OPENIOEXCLUSIVE.");
	}

	return 0;
}



/*
**	Routine:	hold_this_line()
**
**	Function:	To reinsert a cobol line back into the input stream.
**
**	Description:	Copy the line into a holding area and flag that a line is now held.
**			Flush the token cache to force it to re-get the held line next
**			time get_token() is called.
**			Call hold_get_param() to reset the param buffers to force a get_line().
**
**	Arguments:
**	the_line	The line to hold
**
**	Globals:
**	the_held_line	The holding area
**	is_held_line	The flag to indicate is a line is being held
**
**	Return:		None
**
**	Warnings:	Only one line can be held. Holding a second line will overwrite the first.
**
**	History:	
**	05/27/93	Written by GSL
**	06/02/93	Added call to tokenize() to flush the token cache. GSL
**	06/11/93	Added call to hold_get_param(). GSL
**
*/
static int is_held_line = 0;
static char *the_held_lines[10];

int hold_this_line(char *the_line)
{
	the_held_lines[is_held_line] = wdupstr(the_line);
	is_held_line++;

	return 0;
}

/*
**	Routine:	get_held_line()
**
**	Function:	To retrieve the held line.
**
**	Description:	Copies holding area to the passed line and flags
**			that the holding area is now empty.
**
**	Arguments:
**	the_line	The line to retrieve
**
**	Globals:
**	the_held_line	The holding area
**	is_held_line	The flag to indicate is a line is being held
**
**	Return:
**	0		There was no held line.
**	1		Got the held line.
**
**	Warnings:	None
**
**	History:	
**	05/17/93	Written by GSL
**
*/
int get_held_line(char *the_line)
{
	if (is_held_line)
	{
		is_held_line--;
		strcpy(the_line,the_held_lines[is_held_line]);
		wfree(the_held_lines[is_held_line]);
		return(1);
	}
	else
	{
		return(0);
	}

}

/*
**	Routine:	held_line()
**
**	Function:	To test if there is a held line.
**
**	Description:	Return the held line flag.
**
**	Arguments:	None
**
**	Globals:
**	is_held_line	The flag to indicate is a line is being held
**
**	Return:	
**	0		No held line.
**	1		There is a held line.
**
**	Warnings:	None
**
**	History:	
**	05/27/93	Written by GSL
**
*/
int held_line(void)
{
	return(is_held_line);
}


static int end_of_input_flag = 0;

int end_of_input(void)
{
	return end_of_input_flag;
}

int set_end_of_input(void)
{
	end_of_input_flag = 1;
	return 1;
}

/*
**	History:
**	$Log: input.c,v $
**	Revision 1.29  2011/10/29 20:09:14  gsl
**	Fix ISO routine name warnins on WIN32
**	
**	Revision 1.28  2010/01/09 23:46:08  gsl
**	use strerror() instead of sys_errlist
**	
**	Revision 1.27  2003/12/02 21:23:21  gsl
**	Fix so native screen sections don't get generated in a copybook file.
**	Change generated copybooks (internal) to use same file extension rules
**	as translated copybooks. Default to .cob extension.
**	
**	Revision 1.26  2003/08/08 19:52:47  gsl
**	Add native screens comments
**	
**	Revision 1.25  2003/08/06 18:12:10  gsl
**	
**	Revision 1.24  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.23  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.22  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.21  2002/09/05 14:33:53  gsl
**	fix const warning
**	
**	Revision 1.20  2002/09/04 18:11:46  gsl
**	LINUX sys_errlist
**	
**	Revision 1.19  2002/08/12 20:13:52  gsl
**	quotes and literals
**	
**	Revision 1.18  2002/08/09 20:43:25  gsl
**	FIx recent bug in open COPY book logic related to not generating the native path comment
**	
**	Revision 1.17  2002/07/18 21:04:25  gsl
**	Remove MSDOS code
**	
**	Revision 1.16  2002/07/02 04:08:44  gsl
**	Cleanup cobol types
**	
**	Revision 1.15  2002/06/21 20:49:32  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.14  2002/06/21 03:13:18  gsl
**	Remove VAX
**	
**	Revision 1.13  2002/06/20 22:53:35  gsl
**	remove obsolete code
**	
**	Revision 1.12  2002/05/16 21:35:23  gsl
**	Allow uppercase copybooks
**	Change copybook ext to .cob
**	
**	Revision 1.11  2001-10-18 11:11:00-04  gsl
**	Changed conditional code uncomment logic so it only modifies regular comment
**	lines that have a '*' in col 7. If no comment in col 7 then it is passed thru
**	unchanged.
**
**	Revision 1.10  2001-09-13 10:13:03-04  gsl
**	Remove VMS ifdefs
**	Add xtab_log() of COPY statments
**
**	Revision 1.9  1998-05-07 16:37:40-04  gsl
**	Add $ACN_CODE $ACN_ELSE $ACN_END conditional support
**
**	Revision 1.8  1998-03-20 17:16:23-05  gsl
**	Changed error message to informational.
**	Using a keyword for a copybook name.
**
**	Revision 1.7  1996-08-30 21:56:03-04  gsl
**	drcs update
**
**
**
*/
