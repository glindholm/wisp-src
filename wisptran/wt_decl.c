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
#include "crt.h"
#include "cobfiles.h"
#include "statment.h"
#include "wt_procd.h"
#include "reduce.h"


static int decl_parsing = 0;								/* Indicates currently parsing a decl.	*/
static char decl_sname[40];								/* The SECTION name of this one.	*/

NODE chk_dpar(NODE the_sentence)							/* Check for DECLARATIVES para's.	*/
{
	NODE para_node;
	char tstr[40];

	para_node = is_paragraph(the_sentence);
	strcpy(tstr, token_data(para_node->token));

	if (in_decl)
	{
		if (decl_paras_cnt >= MAX_PARAGRAPHS)
		{
			write_log("WISP",'F',"MAXPAREXC",
						"WISP internal error, Maximum number of paragraphs in DECLARATIVES exceeded.");
			exit_wisp(EXIT_WITH_ERR);
		}
		strcpy(decl_paras[decl_paras_cnt],tstr);				/* Add para name to table		*/
		decl_paras_cnt++;

		if (opt_copy_declarative_para_cnt)					/* See if it is a paragraph referenced	*/
		{									/* by the procdiv, and flag it for	*/
											/* copying.				*/

			if (instrlist(tstr,opt_copy_declarative_para,opt_copy_declarative_para_cnt))		/* Found it?				*/
			{
				copy_to_dtp_file = 1;					/* Set the mode.			*/
			}
		}

		if (is_section(the_sentence))						/* See if it's a section.		*/
		{
			return parse_use(the_sentence);					/* Process the USE procedure.		*/
		}
	}
	else										/* We are in the procedure division.	*/
	{										/* If it is not in declaratives, see if	*/
		if (proc_paras_cnt)							/* it is a paragraph which is referenced*/
		{									/* by the declaratives, and flag it for	*/

			if (paracmp(tstr,proc_paras,proc_paras_cnt))			/* Found it?				*/
			{
				if (is_section(the_sentence))				/* It's a SECTION.			*/
				{
					copy_sect = 1;					/* Set first flag for section.		*/
				}
				
				copy_to_dcl_file = 1;					/* Set the mode.			*/
			}
		}
	}

	return the_sentence;
}
/*
**	SECTION section-name.
**
**	    USE AFTER [STANDARD] {EXCEPTION|ERROR} PROCEDURE [ON] 
**		{{file-name}...|INPUT|OUTPUT|I-O|SHARED|EXTEND|SPECIAL-INPUT}.
**
**
**	Note: Keyword ERROR will break a fragment.
*/
NODE parse_use(NODE the_section)
{
	NODE	next_sentence;
	NODE	curr_node;
	char 	first_fname[40];
	char 	next_fname[40];
	
	char	fld[50];
	int 	tnum,fnum,multi_file;
	int	first_is_crt;
	
	gen_dexit();									/* If we were doing one, gen the end.	*/

	next_sentence = get_sentence_tree();
	curr_node = first_non_fluff_node(next_sentence);

	if (curr_node && eq_token(curr_node->token, VERB, "USE"))
	{
		curr_node = curr_node->next;
	}
	else	/* Not a USE phrase.			*/
	{
		write_log("WISP",'F',"ERRINUSE","Error parsing USE statement. USE missing.");
		exit_wisp(EXIT_WITH_ERR);
	}

	if (eq_token(curr_node->token, KEYWORD, "AFTER"))
	{
		curr_node = curr_node->next;
	}


	if (eq_token(curr_node->token, KEYWORD, "DEADLOCK"))				/* If DEADLOCK, not valid, delete it.	*/
	{
		write_log("WISP",'E',"DEADLOCK","USE AFTER DEADLOCK is not supported (DELETED).");
		the_section = free_statement(the_section);
		next_sentence = free_statement(next_sentence);
		del_use = 1;								/* Set a flag to make it delete it.	*/

		return NULL;
	}

	if (eq_token(curr_node->token, KEYWORD, "STANDARD"))
	{
		curr_node = curr_node->next;
	}

	/* Keyword ERROR would break this sentence into fragments. */
	if (NODE_END == curr_node->type)
	{
		/* 
		**	Find the next node in the next fragment
		**	(Sentence)
		**	|	   |		|
		**	(USE frag) (ERROR frag) (PERIOD frag)	
		*/

		curr_node = next_non_fluff_node_in_sentence(next_sentence, curr_node);
		if (!curr_node)
		{
			write_log("WISP",'F',"ERRINUSE","Error parsing USE statement, broken sentence tree.");
			exit_wisp(EXIT_WITH_ERR);
		}
	}
	
	if (eq_token(curr_node->token, KEYWORD, "ERROR") ||
	    eq_token(curr_node->token, KEYWORD, "EXCEPTION"))
	{
		curr_node = curr_node->next;	/* ERROR | EXCEPTION */
	}
	else
	{
		write_tlog(curr_node->token, "WISP",'F',"ERRINUSE","Error parsing USE statement, missing ERROR or EXCEPTION.");
	}

	if (!eq_token(curr_node->token, KEYWORD, "PROCEDURE"))
	{
		write_tlog(curr_node->token,"WISP",'F',"ERRINUSE","Error parsing USE statement, PROCEDURE missing.");
		exit_wisp(EXIT_WITH_ERR);
	}

	curr_node = curr_node->next;

	if (eq_token(curr_node->token, KEYWORD, "ON"))
	{
		curr_node = curr_node->next;
	}

	if (eq_token(curr_node->token, KEYWORD, "SHARED") ||
	    eq_token(curr_node->token, KEYWORD, "SPECIAL-INPUT"))			/* If it's SHARED or SPECIAL-INPUT.	*/
	{										/* Then remove it.			*/
		write_log("WISP",'E',"USEAFTER","USE AFTER ERROR ON %s is Invalid (DELETED).",token_data(curr_node->token));
		the_section = free_statement(the_section);
		next_sentence = free_statement(next_sentence);
		del_use = 1;								/* Set a flag to make it delete it.	*/

		return NULL;
	}

	if (eq_token(curr_node->token, KEYWORD, "INPUT") ||
	    eq_token(curr_node->token, KEYWORD, "OUTPUT") ||
	    eq_token(curr_node->token, KEYWORD, "I-O") ||
	    eq_token(curr_node->token, KEYWORD, "EXTEND"))
	{										/* It's not a specific file.		*/
		tput_statement(8, the_section);
		the_section = free_statement(the_section);
		
		write_log("WISP",'E',"USEAFTER","USE AFTER ERROR ON %s not supported, causes conflict.",
			  token_data(curr_node->token));

		tput_statement(12, next_sentence);
		next_sentence = free_statement(next_sentence);

		return NULL;
	}

	/*
	**	If the USE statement lists multiple files then multiple USE statements
	**	are generated one for each file.  The first file in the list is generated
	**	last and it's section holds the original paragraphs.
	**
	**--- Before ---
	**
	**	{section-name} SECTION.
	**	    USE AFTER ERROR PROCEDURE {1st-file}, {2nd-file}, {nth-file}.
	**      {use-paragraphs}
	**
	**--- After ---
	**
	**	WSECT-{2nd-file} SECTION.
	**	    USE AFTER ERROR PROCEDURE {2nd-file}.
	**	WDCL-{2nd-file}.
	**	    CALL "WFILECHK3" ...
	**	    IF WISP-SKIP-DECLARATIVES = "N" THEN
	**	        PERFORM WDS-{1st-file} THRU WDX-{1st-file}.
	**	WDX-{2nd-file}.
	**	    EXIT.
	**
	**	WSECT-{nth-file} SECTION.
	**	    USE AFTER ERROR PROCEDURE {nth-file}.
	**	WDCL-{nth-file}.
	**	    CALL "WFILECHK3" ...
	**	    IF WISP-SKIP-DECLARATIVES = "N" THEN
	**	        PERFORM WDS-{1st-file} THRU WDX-{1st-file}.
	**	WDX-{nth-file}.
	**	    EXIT.
	**
	**	{section-name} SECTION.
	**	    USE AFTER ERROR PROCEDURE {1st-file}.
	**	WDCL-{1st-file}.
	**	    CALL "WFILECHK3" ...
	**	    IF WISP-SKIP-DECLARATIVES = "Y" THEN
	**	        GO TO WDX-{1st-file}.
	**
	**	WDS-{1st-file}.
	**      {use-paragraphs}
	**	WDX-{1st-file}.
	**	    EXIT.
	**
	*/

	strcpy(first_fname,token_data(curr_node->token));
	tnum = file_index(first_fname);
	first_is_crt = 0;

	if (tnum == -1)
	{
		tnum = crt_index(first_fname);
		if ( tnum == -1 ) 
		{
			write_log("WISP",'E',"ERRINUSE","Error parsing USE statement, File \"%s\"not found.",first_fname);
			exit_wisp(EXIT_WITH_ERR);
		}
		first_is_crt = 1;
	}
	else  /* A regular file */
	{
        	prog_ftypes[tnum] |= HAS_DECLARATIVES;					/* Flag it was there.			*/
	}

	multi_file = 0;

	/*
	**	Loop for each additional file (past the first one) on the USE statement.
	*/
	while (NODE_END != curr_node->next->type)
	{
		int	iscrt;

		curr_node = curr_node->next;

		iscrt = 0;
		strcpy(next_fname, token_data(curr_node->token));
		fnum = file_index(next_fname);	

		if (fnum == -1)
		{
			fnum = crt_index(next_fname);
			if (fnum == -1)
			{
				write_log("WISP",'F',"ERRINUSE","Error parsing USE statement, File \"%s\"not found.",next_fname);
				exit_wisp(EXIT_WITH_ERR);
			}
			iscrt = 1;
		}

		/*
		**	WSECT-{nth-file} SECTION.
		**	    USE AFTER ERROR PROCEDURE {nth-file}.
		**	WDCL-{nth-file}.
		**	    CALL "WFILECHK3" ...
		**	    IF WISP-SKIP-DECLARATIVES = "N" THEN
		**	        PERFORM WDS-{1st-file} THRU WDX-{1st-file}.
		**	WDX-{nth-file}.
		**	    EXIT.
		*/
		make_fld(fld,next_fname,"WSECT-");
		tput_line_at(8, "%s SECTION.",fld);					/* Make a section for it.		*/
		tput_line_at(12, "USE AFTER ERROR PROCEDURE %s.", next_fname);		/* Give it a USE statement.		*/
		make_fld(fld,next_fname,"WDCL-");
		tput_line_at(8, "%s.",fld);						/* Generate a starting paragraph.	*/

		if (!iscrt)
		{
		        prog_ftypes[fnum] |= HAS_DECLARATIVES;				/* Flag it was there.			*/
			g_wfilechk(fnum);						/* Generate a call to WFILECHK3.		*/

			/* Insert check to do user declaratives. */
			/* IF condition THEN PERFORM user-declaratives */
			/* tput_line_at(12, "IF WISP-DECLARATIVES-STATUS NOT = \"00\" THEN"); */
			tput_line_at(12, "IF WISP-SKIP-DECLARATIVES = \"N\" THEN");
		}

		make_fld(fld,first_fname,"WDS-");
		tput_line_at(16, "PERFORM %s THRU",fld);
		make_fld(fld,first_fname,"WDX-");
		tput_clause (20, "%s.",fld);

		make_fld(fld,next_fname,"WDX-");
		tput_line_at(8,  "%s.",fld);					/* Generate an ending paragraph.	*/
		tput_line_at(12, "EXIT.");
		multi_file = 1;
	}

	/*
	**	Generate First file logic...
	**
	**	{section-name} SECTION.
	**	    USE AFTER ERROR PROCEDURE {1st-file}.
	**	WDCL-{1st-file}.
	**	    CALL "WFILECHK3" ...
	**	    IF WISP-SKIP-DECLARATIVES = "Y" THEN
	**	        GO TO WDX-{1st-file}.
	**
	**	WDS-{1st-file}.
	**      {use-paragraphs}
	**	WDX-{1st-file}.
	**	    EXIT.
	*/

	tput_statement(8, the_section);
	the_section = free_statement(the_section);
	next_sentence = free_statement(next_sentence);

	tput_line_at(12, "USE AFTER ERROR PROCEDURE %s.", first_fname);			/* Generate the USE phrase.		*/
	make_fld(fld,first_fname,"WDCL-");
	tput_line_at(8,  "%s.",fld);							/* Generate an initial paragraph.	*/
	if (!first_is_crt)
	{
		g_wfilechk(tnum);							/* Generate a call to WFILECHK3.		*/

		/*tput_line_at(12, "IF WISP-DECLARATIVES-STATUS = \"00\" THEN"); */	/* Skip declare.			*/
		tput_line_at(12, "IF WISP-SKIP-DECLARATIVES = \"Y\" THEN");

		make_fld(fld,first_fname,"WDX-");
		tput_line_at(16, "GO TO %s.",fld);
	}

	if (multi_file)
	{										/* If this is used by more than one file*/
		make_fld(fld,first_fname,"WDS-");
		tput_line_at(8,  "%s.",fld);						/* Generate a starting paragraph.	*/
	}

	strcpy(decl_sname,first_fname);							/* Save this section name.		*/
	decl_parsing = 1;								/* Set a flag saying we did this.	*/

	return NULL;
}

void gen_dexit(void)									/* Generate the EXIT paragraph for decl.*/
{
	char	fld[50];

	if (!decl_parsing) return;							/* No need.				*/

	make_fld(fld,decl_sname,"WDX-");
	tput_line_at(8,  "%s.",fld);
	tput_line_at(12, "EXIT.");
	decl_parsing = 0;								/* Clear the flag.			*/
}
/*
**	History:
**	$Log: wt_decl.c,v $
**	Revision 1.20  2003/03/17 17:22:43  gsl
**	Change to use  WFILECHK3
**	
**	Revision 1.19  2003/03/10 17:38:00  gsl
**	comments
**	
**	Revision 1.18  2003/03/06 21:52:55  gsl
**	change WISP-DECLARATIVES-STATUS to  WISP-SKIP-DECLARATIVES
**	
**	Revision 1.17  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.16  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.15  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.14  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.13  2002/07/02 04:09:49  gsl
**	Add WISP-SKIP-DECLARATIVES
**	
**	Revision 1.12  1998/03/27 19:10:26  gsl
**	Remove ref to linein in error
**	
**	Revision 1.11  1998-03-03 15:49:11-05  gsl
**	Rewrote for cobol-85 changes
**
**	Revision 1.10  1996-08-30 21:56:15-04  gsl
**	drcs update
**
**
**
*/
