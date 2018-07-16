static char copyright[]="Copyright (c) 1995-1999 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "statment.h"
#include "reduce.h"
#include "wt_procd.h"
static void tput_sort_fwopen(NODE curr_node, int col, const char *openmode);

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

	tput_sort_fwopen(curr_node, col, "WFOPEN-SORT");
	
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
				tput_sort_fwopen(curr_node, col, "WFOPEN-INPUT");

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
					tput_sort_fwopen(curr_node, col, "WFOPEN-SORT");
				}
			}
		}
		else
		{
			curr_node = curr_node->next;
		}
	}
	
	tput_line_at(col,"MOVE \"SO\" TO WISP-DECLARATIVES-STATUS");
	tput_flush();
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

static void tput_sort_fwopen(NODE curr_node, int col, const char *openmode)
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
	
	tput_line_at(col, "CALL \"wfopen3\" USING");
	tput_clause(col+4,  "%s,", get_prog_status(fnum));	/* STATUS	*/
	tput_clause(col+4,  "%s,", get_prog_vname(fnum));	/* VOLUME	*/
	tput_clause(col+4,  "%s,", get_prog_lname(fnum));	/* LIBRARY	*/
	tput_clause(col+4,  "%s,", get_prog_fname(fnum));	/* FILE		*/
	tput_clause(col+4,  "%s,", get_prog_nname(fnum));	/* NATIVE	*/
	tput_clause(col+4,  "WISP-APPLICATION-NAME,");		/* Application	*/
	tput_clause(col+4,  "%s,", get_prog_prname(fnum));	/* PRNAME	*/
	tput_clause(col+4,  "%s", openmode);			/* open mode	*/
}


/*
**	History:
**	$Log: wt_sort.c,v $
**	Revision 1.14  1999/01/05 19:55:11  gsl
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
