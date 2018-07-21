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

/*
**	File:		dataconv.c
**
**	Purpose:	Generation of data conversion programs
**
**	Routines:	
**	gen_data_conv()		Main routine for running in /DATACONV mode.
**	dcv_process_selects()	Process all SELECT statements.
**	dcv_process_fd()	Process all FD and records statements.
**	dcv_gen_ws_data()	To generate the canned WORKING-STORAGE data items.
**	dcv_gen_procedure()	Generate the data conversion PROCEDURE DIVISION.
**	is_data_conv()		Report if WISP is in data converion mode.
**	seq_name()		Generate a valid data name with "SEQ-" prefix.
**
**
*/

#define EXT extern
#include "wisp.h"
#include "cobfiles.h"
#include "token.h"
#include "node.h"
#include "statment.h"
#include "wt_datad.h"
#include "output.h"

static NODE dcv_process_selects(void);
static NODE dcv_process_fd(NODE the_statement);
static int dcv_gen_ws_data(void);
static int dcv_gen_procedure(void);
static int seq_name(char *outname, char *inname);

static char prog_firstrec[MAX_FILES][40];

/*
**	Routine:	gen_data_conv()
**
**	Function:	To generate Wang COBOL data conversion routines for
**			loading INDEXED and RELATIVE files from sequential files.
**
**	Description:	This routine is called when the /DATACONV -q option is used
**			It must generate a complete Wang COBOL program.
**
**			The basic plan is to identify each of the INDEXED and RELATIVE files
**			used by the program and generate matching sequential files.  In the
**			procedure division we generate routines to convert each file. 
**			A file is converted by reading a record from the sequential file
**			then writing it out to the indexed file.
**
**			It will delete any SEQUENTIAL files it finds as these do not need
**			to be converted.  It adds in a "DISPLAY" files for use in prompting
**			the user.  It added working storage variables that it uses. It deletes
**			the LINKAGE SECTION and PROCEDURE DIVISION.  It generates it's own
**			PROCEDURE DIVISION that prompts the users with each file to convert.
**
**			When it generates the matching sequential file it duplicates the
**			entire FD and all records.  All data names are prefixed with "SEQ-"
**			and the name truncated at 30 characters.
**
**	Arguments:	None
**
**	Globals:	Normal wisp.h variables.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/25/93	Written by GSL
**
*/
void gen_data_conv (void)
{
	NODE	the_statement;

	/*
	**	Set options to include comments etc.
	**	Remember we are generating Wang COBOL code.
	*/
	opt_noprocess = 1;

	/*
	**	Pass through everything up to FILE-CONTROL.
	*/
	while((the_statement = get_statement()))
	{
		if (eq_token(the_statement->next->token,KEYWORD,"FILE-CONTROL")) break;

		tput_statement(12,the_statement);
		free_statement(the_statement);
	}
	if (!the_statement)
	{
		write_log("WISP",'F',"NOFILECTL","No FILE-CONTROL found");
		exit_with_err();
	}

	/*
	**	PROCESS FILE-CONTROL.
	**		- write out the "FILE-CONTROL." 
	**		- generate a "DISPLAY" file for our use.
	**		- call dcv_process_selects() to process all the SELECT statements.
	**		- check if we have anything to process.
	*/
	tput_statement(12,the_statement);
	free_statement(the_statement);

	tput_line_at(12, "SELECT ASK-CRT ASSIGN \"ASKCRT\" \"DISPLAY\" PFKEY ASK-PFKEY.");
	the_statement = dcv_process_selects();

	if (0==prog_cnt)
	{
		write_log("WISP",'F',"NOFILES","No INDEXED of RELATIVE files were found.");
		exit_with_err();
	}

	/*
	**	Pass through everything up to FD.
	*/
	while(the_statement)
	{
		if (eq_token(the_statement->next->token,KEYWORD,"FD")) break;
		if (eq_token(the_statement->next->token,KEYWORD,"SD")) break;

		tput_statement(12,the_statement);
		free_statement(the_statement);
		the_statement = get_statement();
	}
	if (!the_statement)
	{
		write_log("WISP",'F',"NOFILECTL","No FD/SD found");
		exit_with_err();
	}

	/*
	**	PROCESS FD's.
	*/
	tput_line_at(8,  "FD  ASK-CRT.");
	tput_line_at(8,  "01  ASK-CRT-RECORD  PIC X(1924).");

	the_statement = dcv_process_fd(the_statement);

	/*
	**	Next statement should be WORKING-STORAGE SECTION.
	**	If not then generate it.
	**	Add our working data items.
	*/
	if (eq_token(the_statement->next->token,KEYWORD,"WORKING-STORAGE"))
	{
		tput_statement(12,the_statement);
		free_statement(the_statement);
		the_statement = get_statement();
	}
	else
	{
		tput_line_at(8, "WORKING-STORAGE SECTION.");
	}

	dcv_gen_ws_data();

	/*
	**	Pass thru everything up to LINKAGE or PROCEDURE.
	**	When we find either one we stop copying statements.
	*/
	while(the_statement)
	{
		if (eq_token(the_statement->next->token,KEYWORD,"LINKAGE")) break;
		if (eq_token(the_statement->next->token,KEYWORD,"PROCEDURE")) break;
		tput_statement(12,the_statement);
		free_statement(the_statement);
		the_statement = get_statement();
	}
	free_statement(the_statement);

	/*
	**	Generate our PROCEDURE DIVISION.
	*/
	dcv_gen_procedure();

	exit_wisp(EXIT_OK);
}

/*
**	Routine:	dcv_process_selects()
**
**	Function:	To process all the SELECT statements for data conversion.
**
**	Description:	Inspect each SELECT statement and if it is INDEXED or RELATIVE
**			this generate a matching sequential SELECT otherwise delete it.
**
**	Arguments:	None
**
**	Globals:	Normal ones.
**
**	Return:		The next statement.
**
**	Warnings:	None
**
**	History:	
**	05/25/93	Written by GSL
**
*/
static NODE dcv_process_selects (void)
{
	NODE	the_statement;
	NODE	curr_node, org_node, acc_node, name_node;
	int	this_file;
	char	buff[80];
	static unsigned int seqcnt = 0;

	the_statement = get_statement();

	/*
	**	Loop through all the SELECT statements.
	**
	**	For each SELECT statement determine if it is an INDEXED or RELATIVE
	**	file that needs to be converted. If it needs to be converted then
	**	generate a matching sequential file SELECT with "SEQ-name".  If it
	**	doesn't need to be converted then delete it be not writing it out.
	*/
	while(the_statement && eq_token(the_statement->next->token,KEYWORD,"SELECT"))
	{
		org_node = acc_node = NULL;
		curr_node = the_statement->next->next;
		if (eq_token(curr_node->token,KEYWORD,"OPTIONAL")) 	curr_node = curr_node->next;
		name_node = curr_node;
		curr_node = curr_node->next;
		if (eq_token(curr_node->token,KEYWORD,"ASSIGN")) 	curr_node = curr_node->next;
		if (eq_token(curr_node->token,KEYWORD,"TO")) 		curr_node = curr_node->next;
		curr_node = curr_node->next;
		if (eq_token_literal(curr_node->token,"DISPLAY"))
		{
			/*
			**	Don't convert "DISPLAY" device files.
			*/
			curr_node = NULL;
		}

		/*
		**	Loop through the nodes in the statement looking for the ORGANIZATION of it.
		**	Note that "ORGANIZATION" is an optional keyword.
		**	Also look for the access mode.
		*/
		while(curr_node && curr_node->type != NODE_END)
		{
			if (eq_token(curr_node->token,KEYWORD,"ORGANIZATION"))
			{
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,KEYWORD,"IS")) 	curr_node = curr_node->next;
				org_node = curr_node;
			}
			else if (eq_token(curr_node->token,KEYWORD,"ACCESS"))
			{
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,KEYWORD,"MODE")) 	curr_node = curr_node->next;
				if (eq_token(curr_node->token,KEYWORD,"IS")) 	curr_node = curr_node->next;
				acc_node = curr_node;

				if (eq_token(curr_node->next->token,KEYWORD,"RELATIVE")) curr_node = curr_node->next;
			}
			else if (!org_node && eq_token(curr_node->token,KEYWORD,"SEQUENTIAL"))
			{
				org_node = curr_node;
			}
			else if (!org_node && eq_token(curr_node->token,KEYWORD,"INDEXED"))
			{
				org_node = curr_node;
			}
			else if (!org_node && eq_token(curr_node->token,KEYWORD,"RELATIVE"))
			{
				if (!eq_token(curr_node->next->token,KEYWORD,"KEY"))
				{
					org_node = curr_node;
				}
			}

			curr_node = curr_node->next;
		}

		if (org_node &&
		    (eq_token(org_node->token,KEYWORD,"INDEXED") ||
		     eq_token(org_node->token,KEYWORD,"RELATIVE")   ) )
		{
			/*
			**	We found a SELECT for a file that we need to process.
			*/

			write_log("WISP",'M',"DATACONV", "Generating conversion routines for file %s.", 
				  token_data(name_node->token));

			this_file = prog_cnt;					/* Remember this file index.			*/
			prog_cnt++;
			strcpy(prog_files[this_file],token_data(name_node->token));	/* save it in the table			*/

			if (acc_node)
			{
				/*
				**	If ACCESS was found the change to DYNAMIC.
				*/
				if (!eq_token(acc_node->token,KEYWORD,"DYNAMIC"))
				{
					edit_token(acc_node->token,"DYNAMIC");
				}
			}
			else
			{
				write_log("WISP",'W',"NOACCMODE","No ACCESS MODE found for SELECT [%s].",
					  token_data(name_node->token));
			}

			tput_statement(12,the_statement);			/* Write on the original SELECT			*/

			/*
			**	Generate a new SELECT statement for the sequential file.
			**
			**		SELECT SEQ-name ASSIGN TO "SEQxxxx" "DISK".
			*/
			seq_name(buff,prog_files[this_file]);
			tput_line_at(12,"SELECT %s",buff);
			sprintf(buff, "ASSIGN TO \"SEQ%04u\" \"DISK\".", seqcnt++);
			tput_clause (16, buff);
		}

		free_statement(the_statement);
		the_statement = get_statement();
	}

	return(the_statement);
}

/*
**	Routine:	dcv_process_fd()
**
**	Function:	To process all the FD statements for data conversion.
**
**	Description:	Check each FD and if it's for a file to convert then
**			write it out and generate a matching one for the seqential file.
**
**	Arguments:	None
**
**	Globals:	Normal ones.
**
**	Return:		The next statement.
**
**	Warnings:	None
**
**	History:	
**	05/25/93	Written by GSL
**
*/
static NODE dcv_process_fd ( NODE the_statement )
{
	NODE	curr_node, name_node;
	int	this_file;
	char	buff[80];
	int	first_record;

	/*
	**	Loop thru all the FD's
	*/
	while(the_statement && 
		( eq_token(the_statement->next->token,KEYWORD,"FD") ||
		  eq_token(the_statement->next->token,KEYWORD,"SD")   ) )
	{
		name_node = the_statement->next->next;
		
		this_file = file_index(token_data(name_node->token));
		if (-1 == this_file)
		{
			/*
			**	This FD is not for a convert file so delete it and it's records.
			*/
			the_statement = delete_fd(the_statement);
		}
		else
		{
			NODE	fd_statements, curr_statement;

			/*
			**	Load up the whole FD plus 01 record into on structure.
			**	This creates a linked list of statements.
			*/
			fd_statements = makenode(NODE_STATEMENT,the_statement,NULL,NULL);
			curr_statement = fd_statements;

			while((the_statement = get_statement()))
			{
				curr_node = the_statement->next;

				if (eq_token(curr_node->token,KEYWORD,"FD") ||
				    eq_token(curr_node->token,KEYWORD,"SD") ||
				    eq_token(curr_node->token,KEYWORD,"WORKING-STORAGE") ||
				    eq_token(curr_node->token,KEYWORD,"LINKAGE") ||
				    eq_token(curr_node->token,KEYWORD,"PROCEDURE"))
				{
					/*
					**	We have found the next statement following the FD and records.
					*/
					break;
				}

				curr_statement->next = makenode(NODE_STATEMENT,the_statement,NULL,NULL);
				curr_statement = curr_statement->next;
			}

			/*
			**	Write out the original FD and records.
			*/
			tput_statement(12,fd_statements);

			/*
			**	Convert for use as sequential file then
			**	write out the sequential.
			*/

			curr_statement = fd_statements->down;			/* Point to FD statement			*/
			curr_node = curr_statement->next->next;			/* Point at name node				*/
			seq_name(buff,token_data(curr_node->token));		/* Generate SEQ-name				*/
			edit_token(curr_node->token,buff);			/* Replace with new name			*/

			curr_node = curr_node->next;
			while(curr_node && NODE_END != curr_node->type)
			{
				if (eq_token(curr_node->token,KEYWORD,"DATA"))
				{
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,KEYWORD,"RECORD") ||
					    eq_token(curr_node->token,KEYWORD,"RECORDS")  )
					{
						/*
						**	DATA RECORDS ARE xxx, xxx, xxx, ....
						**
						**	Change each record name to SEQ-xxx
						*/

						curr_node = curr_node->next;
						if (eq_token(curr_node->token,KEYWORD,"IS") ||
						    eq_token(curr_node->token,KEYWORD,"ARE")   ) curr_node = curr_node->next;

						while(IDENTIFIER==curr_node->token->type)
						{
							seq_name(buff,token_data(curr_node->token));
							edit_token(curr_node->token,buff);
							curr_node = curr_node->next;
						}

						curr_node = NULL;
					}
				}
				else if ( eq_token(curr_node->token,0,"LIBRARY") )
				{
					/*
					**	If present change the VALUE OF LIBRARY to ASK-INLIB
					*/
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,KEYWORD,"IS") ) curr_node = curr_node->next;
					edit_token(curr_node->token,"ASK-INLIB");
					curr_node = curr_node->next;
				}
				else if ( eq_token(curr_node->token,0,"VOLUME") )
				{
					/*
					**	If present change the VALUE OF VOLUME to ASK-INVOL
					*/
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,KEYWORD,"IS") ) curr_node = curr_node->next;
					edit_token(curr_node->token,"ASK-INVOL");
					curr_node = curr_node->next;
				}
				else
				{
					curr_node = curr_node->next;
				}
			}

			/*
			**	Convert all the data names to "SEQ-name"
			*/
			first_record = 1;
			curr_statement = fd_statements->next;			/* Point to next statement			*/
			while(curr_statement)
			{
				curr_node = curr_statement->down->next;		/* Point at level node				*/
				if (curr_node && curr_node->token && NUMBER == curr_node->token->type)
				{
					curr_node = curr_node->next;		/* Point to the name				*/

					if (eq_token(curr_node->token,KEYWORD,"FILLER"))
					{
						curr_node = curr_node->next;
					}
					else if (IDENTIFIER == curr_node->token->type)
					{
						if (first_record)
						{
							first_record = 0;
							strcpy(prog_firstrec[this_file],token_data(curr_node->token));
						}
						seq_name(buff,token_data(curr_node->token));
						edit_token(curr_node->token,buff);
						curr_node = curr_node->next;
					}

					if (eq_token(curr_node->token,KEYWORD,"REDEFINES"))
					{
						curr_node = curr_node->next;
						seq_name(buff,token_data(curr_node->token));
						edit_token(curr_node->token,buff);
					}
				}
				curr_statement = curr_statement->next;
			}

			/*
			**	Write out the matching FD and records for the sequential file.
			*/
			tput_statement(12,fd_statements);

			free_statement(fd_statements);
		}
	}

	return(the_statement);
}

/*
**	Routine:	dcv_gen_ws_data()
**
**	Function:	To generate the canned WORKING-STORAGE data items.
**
**	Description:	Write out the items.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/25/93	Written by GSL
**
*/
static int dcv_gen_ws_data (void)
{
	tput_blank();
	tput_line_at(8, "01  CONVERT-INREC   PIC 9(9) VALUE 0.");
	tput_line_at(8, "01  CONVERT-OUTREC  PIC 9(9) VALUE 0.");
	tput_line_at(8, "01  CONVERT-INVALID PIC 9(9) VALUE 0.");
	tput_line_at(8, "01  CONVERT-STATUS  PIC 99   VALUE 0.");
	tput_line_at(8, "    88  CONVERT-DONE         VALUE 10.");

	tput_blank();
	tput_line_at(8, "01  ASK-FILE    PIC X(50).");
	tput_line_at(8, "01  ASK-PFKEY   PIC 99 VALUE 0.");
	tput_line_at(8, "01  ASK-ANSWER  PIC 99 VALUE 0.");
	tput_line_at(8, "    88  ASK-YES        VALUE 1.");
	tput_line_at(8, "    88  ASK-NO         VALUE 2.");
	tput_line_at(8, "    88  ASK-EXIT       VALUE 16.");

	tput_blank();
	tput_line_at(8, "01  ASK-IL      PIC XX   VALUE \"IL\".");
	tput_line_at(8, "01  ASK-IV      PIC XX   VALUE \"IV\".");
	tput_line_at(8, "01  ASK-OL      PIC XX   VALUE \"OL\".");
	tput_line_at(8, "01  ASK-OV      PIC XX   VALUE \"OV\".");
	tput_line_at(8, "01  ASK-INLIB   PIC X(8) VALUE SPACES.");
	tput_line_at(8, "01  ASK-INVOL   PIC X(6) VALUE SPACES.");
	tput_line_at(8, "01  ASK-OUTLIB  PIC X(8) VALUE SPACES.");
	tput_line_at(8, "01  ASK-OUTVOL  PIC X(6) VALUE SPACES.");

	tput_blank();
	tput_line_at(8, "01  ASK-SCREEN DISPLAY-WS.");
	tput_line_at(8, "    05  FILLER PIC X(30) ROW 1 COLUMN 25");
	tput_line_at(8, "        VALUE \"**** WISP DATA CONVERSION ****\".");
	tput_line_at(8, "    05  FILLER PIC X(13) ROW 8 COLUMN 2");
	tput_line_at(8, "        VALUE \"Next file is:\".");
	tput_line_at(8, "    05  FILLER PIC X(50) ROW 8 COLUMN 17");
	tput_line_at(8, "        SOURCE ASK-FILE.");
        tput_line_at(8, "    05  FILLER PIC X(50) ROW 11 COLUMN 2");
	tput_line_at(8, "        VALUE \"Do you want to convert this file?\".");

        tput_line_at(8, "    05  FILLER PIC X(7) ROW 16 COLUMN 2");
	tput_line_at(8, "        VALUE \"INLIB: \".");
        tput_line_at(8, "    05  FILLER PIC X(8) ROW 16 COLUMN 10");
	tput_line_at(8, "        SOURCE ASK-INLIB OBJECT ASK-INLIB.");
        tput_line_at(8, "    05  FILLER PIC X(7) ROW 16 COLUMN 22");
	tput_line_at(8, "        VALUE \"INVOL: \".");
        tput_line_at(8, "    05  FILLER PIC X(8) ROW 16 COLUMN 30");
	tput_line_at(8, "        SOURCE ASK-INVOL OBJECT ASK-INVOL.");

        tput_line_at(8, "    05  FILLER PIC X(7) ROW 17 COLUMN 2");
	tput_line_at(8, "        VALUE \"OUTLIB:\".");
        tput_line_at(8, "    05  FILLER PIC X(8) ROW 17 COLUMN 10");
	tput_line_at(8, "        SOURCE ASK-OUTLIB OBJECT ASK-OUTLIB.");
        tput_line_at(8, "    05  FILLER PIC X(7) ROW 17 COLUMN 22");
	tput_line_at(8, "        VALUE \"OUTVOL:\".");
        tput_line_at(8, "    05  FILLER PIC X(8) ROW 17 COLUMN 30");
	tput_line_at(8, "        SOURCE ASK-OUTVOL OBJECT ASK-OUTVOL.");

	tput_line_at(8, "    05  FILLER PIC X(50) ROW 24 COLUMN 2");
	tput_line_at(8, "        VALUE \"(1) YES  (2) NO  (16) EXIT\".");

	tput_blank();
	return(0);
}


/*
**	Routine:	dcv_gen_procedure()
**
**	Function:	To generate the data conversion PROCEDURE DIVISION
**
**	Description:	For each file found it will query the user.  If the user
**			says to convert it then the convertion paragraph is performed.
**			The convert para's open the files then read and write until done
**			then close the files and report the results.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/25/93	Written by GSL
**
*/
static int dcv_gen_procedure (void)
{
	int	this_file;
	char	seqfile[80], seqrec[80];

	tput_line_at(8, "PROCEDURE DIVISION.");
	tput_line_at(8, "MAIN-PARA.");

	for(this_file=0; this_file<prog_cnt; this_file++)
	{
		tput_blank();
		tput_line_at(12,"MOVE \"%s\" TO ASK-FILE.", prog_files[this_file]);
		tput_line_at(12,"PERFORM ASK-PARA.");
		tput_line_at(12,"IF ASK-YES THEN PERFORM %04d-CONVERT-PARA.",this_file);
	}

	tput_blank();
	tput_line_at(8, "MAIN-EXIT-STOP.");
	tput_line_at(8, "    EXIT PROGRAM.");
	tput_line_at(8, "    STOP RUN.");

	tput_blank();
	tput_line_at(8, "ASK-PARA.");
	tput_line_at(8, "    CALL \"EXTRACT\" USING");
	tput_line_at(8, "        ASK-IL, ASK-INLIB,  ASK-IV, ASK-INVOL");
	tput_line_at(8, "        ASK-OL, ASK-OUTLIB, ASK-OV, ASK-OUTVOL.");
	tput_line_at(8, "    DISPLAY AND READ ASK-SCREEN ON ASK-CRT");
	tput_line_at(8, "        ONLY PFKEYS 1, 2, 16.");
	tput_line_at(8, "    MOVE ASK-PFKEY TO ASK-ANSWER.");
	tput_line_at(8, "    IF ASK-EXIT THEN PERFORM MAIN-EXIT-STOP.");
	tput_line_at(8, "    CALL \"SET\" USING");
	tput_line_at(8, "        ASK-IL, ASK-INLIB,  ASK-IV, ASK-INVOL");
	tput_line_at(8, "        ASK-OL, ASK-OUTLIB, ASK-OV, ASK-OUTVOL.");

	for(this_file=0; this_file<prog_cnt; this_file++)
	{
		tput_blank();
		tput_blank();
		tput_line_at(7,"******");
		tput_line_at(7,"****** CONVERT FILE %s", prog_files[this_file]);
		tput_line_at(7,"******");
		tput_line_at(8, "%04d-CONVERT-PARA.",this_file);

/*		tput_line_at(12, "MOVE \"filename\" to  %s.", filename-field);    initialize the file name field */
		seq_name(seqfile, prog_files[this_file]);
		tput_line_at(12, "OPEN INPUT  %s.", seqfile);
		tput_line_at(12, "OPEN OUTPUT %s.", prog_files[this_file]);

		tput_blank();
		tput_line_at(12, "MOVE 0 TO CONVERT-STATUS.");
		tput_line_at(12, "MOVE 0 TO CONVERT-INREC.");
		tput_line_at(12, "MOVE 0 TO CONVERT-OUTREC.");
		tput_line_at(12, "MOVE 0 TO CONVERT-INVALID.");
		tput_line_at(12, "PERFORM %04d-READ-PARA.", this_file);
		tput_line_at(12, "PERFORM UNTIL CONVERT-DONE");
		tput_line_at(12, "    ADD 1 TO CONVERT-INREC");
		tput_line_at(12, "    PERFORM %04d-MOVE-PARA", this_file);
		tput_line_at(12, "    PERFORM %04d-WRITE-PARA", this_file);
		tput_line_at(12, "    PERFORM %04d-READ-PARA", this_file);
		tput_line_at(12, "END-PERFORM.");

		tput_blank();
		tput_line_at(12, "CLOSE %s.", seqfile);
		tput_line_at(12, "CLOSE %s.", prog_files[this_file]);
		tput_line_at(12, "DISPLAY \"RECORDS IN \"  CONVERT-INREC");
		tput_line_at(12, "    \"    RECORDS OUT \" CONVERT-OUTREC");
		tput_line_at(12, "    \"    INVALID KEYS \" CONVERT-INVALID.");

		tput_blank();
		tput_line_at(8,  "%04d-READ-PARA.",this_file);
		tput_line_at(8,  "    READ %s", seqfile);
		tput_line_at(8,  "        AT END MOVE 10 TO CONVERT-STATUS.");

		seq_name(seqrec, prog_firstrec[this_file]);
		tput_blank();
		tput_line_at(8,  "%04d-MOVE-PARA.",this_file);
		tput_line_at(8,  "    MOVE %s TO", seqrec);
		tput_clause (16, "%s.", prog_firstrec[this_file]);

		tput_blank();
		tput_line_at(8,  "%04d-WRITE-PARA.",this_file);
		tput_line_at(8,  "    ADD 1 TO CONVERT-OUTREC.");
		tput_line_at(8,  "    WRITE %s INVALID", prog_firstrec[this_file]);
		tput_line_at(8,  "        ADD 1 TO CONVERT-INVALID");
		tput_line_at(8,  "        SUBTRACT 1 FROM CONVERT-OUTREC.");
	}
	return(0);
}

/*
**	Routine:	is_data_conv()
**
**	Function:	To report if WISP is in data conversion mode.
**
**	Description:	To return opt_data_conv.
**
**	Arguments:	None
**
**	Globals:
**	opt_data_conv	The data converion mode flag.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/25/93	Written by GSL
**
*/
int is_data_conv (void)
{
	return(opt_data_conv);
}

/*
**	Routine:	seq_name()
**
**	Function:	To generate a valid data name with a "SEQ-" prefix.
**
**	Description:	This will prefix the name with "SEQ-" and then truncate it to 30 chars.
**			If the last char is now a "-" which is illegal it changes it to an 'X'.
**
**	Arguments:
**	outname		The new SEQ- name
**	inname		The base name.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	06/29/93	Written by GSL
**
*/
static int seq_name ( char *outname, char *inname )
{
	int	len;

	sprintf(outname,"SEQ-%s",inname);
	len = strlen(outname);
	if (len > 30)
	{
		outname[30] = (char)0;
		if ('-' == outname[29]) 
		{
			outname[29] = 'X';
		}
	}
	return 0;
}
/*
**	History:
**	$Log: dataconv.c,v $
**	Revision 1.13  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.12  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.10  2002/08/12 20:13:53  gsl
**	quotes and literals
**	
**	Revision 1.9  1999/09/07 14:51:03  gsl
**	Change dataconv to use SEQxxxx as the PRNAME for the sequential files.
**	
**	Revision 1.8  1998-07-07 09:42:35-04  gsl
**	FIx SEQENTIAL -> SEQUENTIAL
**
**	Revision 1.7  1996-08-30 21:56:02-04  gsl
**	drcs update
**
**
**
*/
