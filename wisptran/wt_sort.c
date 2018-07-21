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
#include "wcommon.h"
#include "cobfiles.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"
static void tput_sort_fwopen(NODE curr_node, int col, int open_mode);

/*
**	SORT file-name-1 {ON {ASCENDING|DESCENDING} KEY {data-name-1}...}...
**	    [WITH DUPLICATES IN ORDER]
**	    [COLLATING SEQUENCE IS alphabet-1]
**	     {INPUT PROCEDURE IS proc-name-1 [THROUGH proc-name-2] }
**	     {USING file-name-2...                                 }
**
**	     {OUTPUT PROCEDURE IS proc-name-3 [THROUGH proc-name-4] }
**	     {GIVING file-name-3...                                 }
**
**
**
*/
NODE parse_sort(NODE the_statement, NODE the_sentence)
{
	NODE	curr_node, verb_node;
	int	col;
#define MAX_USING_FILES	255
	char	*using_files[MAX_USING_FILES];
	int	using_files_cnt = 0;
	int	idx;

	verb_node = first_token_node(the_statement);

	if (!eq_token(verb_node->token,VERB,"SORT") &&
	    !eq_token(verb_node->token,VERB,"MERGE"))
	{
		write_tlog(verb_node->token,"WISP",'E',"VERB","Expected SORT/MERGE found [%s].", token_data(verb_node->token));
		return(the_statement);
	}

	write_tlog(verb_node->token,"WISP",'I',"VERB","Processing %s Statement.", token_data(verb_node->token));

	col = verb_node->token->column;
	if (col > 36) col = 36;
	if (col < 12) col = 12;

	tput_leading_fluff(the_statement);

	curr_node = verb_node->next;

	tput_sort_fwopen(curr_node, col, WFOPEN_SORT);
	
	/*
	**	Scan for a "USING" and "GIVING" clauses
	*/
	curr_node = curr_node->next;
	while (curr_node && NODE_END != curr_node->type)
	{
		if (eq_token(curr_node->token,KEYWORD,"USING"))
		{
			for (curr_node = curr_node->next; 
			     curr_node->token && IDENTIFIER == curr_node->token->type;
			     curr_node = curr_node->next)
			{
				tput_sort_fwopen(curr_node, col, WFOPEN_INPUT);

				using_files[using_files_cnt] = wdupstr(token_data(curr_node->token));
				using_files_cnt++;
			}
		}
		else if (eq_token(curr_node->token,KEYWORD,"GIVING"))
		{
			for (curr_node = curr_node->next; 
			     curr_node->token && IDENTIFIER == curr_node->token->type;
			     curr_node = curr_node->next)
			{
				/*
				**	Check if this GIVING file was also a USING file
				*/
				for(idx=0; idx<using_files_cnt; idx++)
				{
					if (0==strcmp(using_files[idx], token_data(curr_node->token)))
					{
						idx = -1;	/* Yes is was a USING file */
						break;
					}
				}
				
				/*
				**	If this GIVING file was also a USING file then don't 
				**	generate a fopen() call for it.
				*/
				if (idx != -1)
				{
					tput_sort_fwopen(curr_node, col, WFOPEN_SORT);
				}
			}
		}
		else
		{
			curr_node = curr_node->next;
		}
	}
	
	if (opt_nogetlastfileop)
	{
		/* tput_line_at(col,"MOVE \"SO\" TO WISP-DECLARATIVES-STATUS"); */
		tput_line_at(col,"MOVE \"Sort\" TO WISP-LASTFILEOP");
		tput_flush();
	}
	tput_statement(col,the_statement);
	the_statement = free_statement(the_statement);

	/*
	**	Cleanup the using_files list.
	*/
	for(idx=0; idx<using_files_cnt; idx++)
	{
		free(using_files[idx]);
	}
	
	return the_statement;
}

static void tput_sort_fwopen(NODE curr_node, int col, int open_mode)
{
	int	fnum;
	char	file_name[40];
	
	strcpy(file_name,token_data(curr_node->token));
	
	fnum = file_index(file_name);
	if (-1 == fnum)
	{
		write_tlog(curr_node->token,"WISP",'E',"FILE", "SORT/MERGE file not found [%s].", file_name);
		return;
	}
	
	gen_wfopen(col, fnum, open_mode);
}


/*
**	History:
**	$Log: wt_sort.c,v $
**	Revision 1.24  2003/03/17 17:21:17  gsl
**	Change to use  WFOPEN4
**	
**	Revision 1.23  2003/03/07 17:00:07  gsl
**	For ACU default to using "C$GETLASTFILEOP" to retrieve the last file op.
**	Add option #NOGETLASTFILEOP to use if not C$GETLASTFILEOP is
**	not available.
**	
**	Revision 1.22  2003/03/06 21:48:19  gsl
**	Change WISP-DECLARATIVES-STATUS to WISP-LASTFILEOP
**	Change WFOPEN3 to WISP_FILEOPEN
**	
**	Revision 1.21  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.20  2002/07/29 14:47:19  gsl
**	wfopen2 ->WFOPEN2
**	wfopen3 ->WFOPEN3
**	
**	Revision 1.19  2002/07/17 15:03:27  gsl
**	MF doesn't like underscores in COBOL names
**	
**	Revision 1.18  2002/06/28 04:02:56  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.17  2002/06/21 20:49:34  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.16  2002/06/20 23:07:32  gsl
**	native
**	
**	Revision 1.15  2002/05/16 21:53:09  gsl
**	getlastfileop logic
**	
**	Revision 1.14  1999-01-05 14:55:11-05  gsl
**	Fix so if USING and GIVING files are the same then don't generate wfopen() fo
**	for the GIVING, this prevents PF3 screen.
**
**	Revision 1.13  1998-03-20 17:38:36-05  gsl
**	moved OLD to old.c
**
**	Revision 1.12  1998-03-04 14:53:41-05  gsl
**	Change parse_sort() to also handle MERGE verb
**
**	Revision 1.11  1998-03-02 11:13:36-05  gsl
**	Finish cobol-85 logic
**
**	Revision 1.10  1996-08-30 21:56:25-04  gsl
**	drcs update
**
**
**
*/
