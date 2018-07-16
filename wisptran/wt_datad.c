static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		wt_data.c
**
**	Purpose:	Data division/ file section parsing routines.
**
**	Routines:
**	parse_file_section()
**
**
**	History:
**	05/18/93	Changed to use parse_data_description(). GSL
**	05/21/93	Changed to use get_statement(). GSL
**
*/

#include <string.h>

#define EXT extern
#include "wisp.h"
#include "crt.h"
#include "wispfile.h"
#include "cobfiles.h"
#include "keylist.h"
#include "token.h"
#include "node.h"
#include "statment.h"

static int g_curr_file_num = -1;							/* Remember file number.		*/

NODE get_statement();

NODE data_division();
NODE file_section();

NODE parse_crt_records();
NODE delete_fd();

/*
**	Routine:	data_division()
**
**	Function:	To process the DATA DIVISION.
**
**	Description:	Get and put statements until out of this division then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not DATA DIVISION then one will be generated.
**
**	Arguments:
**	the_statement	The pre-loaded first statement.
**
**	Globals:	None
**
**	Return:		The next statement that is not part of this division.
**
**	Warnings:	None
**
**	History:	
**	06/02/93	Written by GSL
**
*/
NODE data_division(the_statement)
NODE	the_statement;
{
	if (!comments) tput_blank();

	if (eq_token(the_statement->next->token,KEYWORD,"DATA"))
	{
		write_log("WISP",'I',"DATADIV","Processing DATA DIVISION.");
		division = DATA_DIVISION;

		tput_statement(12,the_statement);
		free_statement(the_statement);

		the_statement = get_statement();
	}
	else if (eq_token(the_statement->next->token,KEYWORD,"PROCEDURE"))
	{
		write_log("WISP",'I',"DATADIV","DATA DIVISION not found.");
		tput_line_at(8, "DATA DIVISION.");
	}
	else
	{
		write_tlog(the_statement->next->token, 
				"WISP",'F',"PARSEDATA","Expecting DATA, or PROCEDURE DIVISION found %s.",
				token_data(the_statement->next->token));
		exit_with_err();
	}

	/*
	**	FILE SECTION
	*/
	the_statement = file_section(the_statement);

	/*
	**	WORKING-STORAGE SECTION
	*/
	the_statement = (NODE)working_storage_section(the_statement);

	/*
	**	LINKAGE SECTION
	*/
	the_statement = (NODE)linkage_section(the_statement);

	/*
	**	SCREEN SECTION (ACUCOBOL)
	*/
	if (acn_cobol)
	{
		gen_acn_screens();
	}
	
	return(the_statement);
}

/*
**	Routine:	file_section()
**
**	Function:	To process the FILE SECTION.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not FILE SECTION then one will be generated.
**
**	Arguments:
**	the_statement	The pre-loaded first statement.
**
**	Globals:	None
**
**	Return:		The next statement that is not part of this section.
**
**	Warnings:	None
**
**	History:	
**	06/01/93	Written by GSL
**
*/
NODE file_section(NODE the_statement)
{
	static 	int 	crt_fd = 0;

	NODE	next_statement;
	NODE	curr_node, file_name_node;
	char 	the_file[40];
	int	is_fd, is_sd, is_dd;

	if (eq_token(the_statement->next->token,KEYWORD,"FILE"))
	{
		write_log("WISP",'I',"FILESECT","Processing FILE SECTION.");

		tput_statement(12,the_statement);
		free_statement(the_statement);

		tput_line_at(8, "FD  WISP-DECLARATIVES-FILE.");			/* Do FD for Declaratives display.		*/
		tput_line_at(8, "01  WISP-DEC-FILE-DUMMY-RECORD  PIC X.");
	}
	else
	{
		write_log("WISP",'I',"FILESECT","FILE SECTION not found.");

		tput_line_at(8, "FILE SECTION.");
		tput_line_at(8, "FD  WISP-DECLARATIVES-FILE.");			/* Do FD for Declaratives display.		*/
		tput_line_at(8, "01  WISP-DEC-FILE-DUMMY-RECORD  PIC X.");

		return(the_statement);
	}

	next_statement = NULL;

get_next_statement:
	if (next_statement)
	{
		the_statement = next_statement;
		next_statement = NULL;
	}
	else
	{
		the_statement = get_statement();				/* Load the statement				*/
	}

	is_fd = is_sd = is_dd = 0;						/* Assume not a FD or SD			*/ 

	curr_node = the_statement->next;					/* Point to first significant token		*/

	if ( eq_token(curr_node->token, KEYWORD, "FD") )
	{
		is_fd = 1;							/* It is an FD 					*/
	}
	else if ( eq_token(curr_node->token, KEYWORD, "SD") )
	{
		is_sd = 1;							/* It is an SD					*/
	}
	else if ( NUMBER==curr_node->token->type )				/* It is a Data definition			*/
	{
		is_dd = 1;
	}
	else if ( eq_token(curr_node->token, KEYWORD, "WORKING-STORAGE") 	||
		  eq_token(curr_node->token, KEYWORD, "LINKAGE") 		||
		  eq_token(curr_node->token, KEYWORD, "PROCEDURE") 		  )
	{
		/*
		**	We've got the next statement following the FILE SECTION.
		**	Return it to higher level to handle.
		*/
		return(the_statement);
	}
	else
	{
		/*
		**	Statement is not recognized.
		*/
		write_tlog(the_statement->next->token, 
				"WISP",'F',"PARSEDATA","Unrecognized token found in FILE SECTION [%s].",
				token_data(the_statement->next->token));
		exit_with_err();
	}

	if (is_fd || is_sd)
	{
		curr_node = curr_node->next;					/* Advance to the filename node			*/
		file_name_node = curr_node;
		strcpy(the_file,file_name_node->token->data);			/* Save the file name				*/

		delint_statement(file_name_node);				/* Remove lint from filename on			*/

		crt_fd = 0;							/* Assume is NOT a CRT file.			*/
		if (is_fd)
		{
			if ((cur_crt = crt_index(the_file)) != -1)
			{
				crt_fd = 1;					/* Found a CRT file FD				*/
			}
		}
	}

	if ((is_fd || is_sd) && !crt_fd) 					/* It is a normal file FD/SD			*/
	{
		int	fd_parse_mode;
#define	FD_BLOCK	1
#define FD_RECORD	2
#define FD_LABEL	3
#define FD_VALUE	4
#define FD_DATA		5
#define FD_CODESET	6
#define FD_EXTERNAL	7

		g_curr_file_num = file_index(the_file);

		if (g_curr_file_num == -1)					/* Didn't find it.				*/
		{
			write_log("WISP",'E',"NOSELECT","FD for file %s, but no previous SELECT. (deleted)",the_file);
			next_statement = delete_fd(the_statement);
			goto get_next_statement;
		}

		if (is_sd)	write_log("WISP",'I',"PROCSD","Processing SD for %s.",the_file);
		else		write_log("WISP",'I',"PROCFD","Processing FD for %s.",the_file);

		if (is_sd)
		{	
			FSET(prog_ftypes[g_curr_file_num],SORT_FILE);		/* flag it as a sort file			*/
			prog_sort++;						/* and count it					*/
		}

		prog_ftypes[g_curr_file_num] |= HAD_FD;				/* Flag the FD statement.			*/

		if ( is_fd && (lpi_cobol || vax_cobol) )
		{
			/*
			**	VAX:	VALUE OF ID IS N-filename
			**	LPI:	VALUE OF FILE-ID IS N-filename
			*/
			char	temp[40];

			make_fld(temp,prog_files[g_curr_file_num],"N-");	/* start the value statement			*/
			tie_bottom(file_name_node, maketoknode(make_token(KEYWORD,"VALUE")));
			tie_bottom(file_name_node, maketoknode(make_token(KEYWORD,"OF")));
			if (lpi_cobol)
				tie_bottom(file_name_node, maketoknode(make_token(KEYWORD,"FILE-ID")));
			if (vax_cobol)
				tie_bottom(file_name_node, maketoknode(make_token(KEYWORD,"ID")));
			tie_bottom(file_name_node, maketoknode(make_token(KEYWORD,"IS")));
			tie_bottom(file_name_node, maketoknode(make_token(IDENTIFIER,temp)));
		}

		curr_node = curr_node->next;					/* Advance past the filename node		*/
		fd_parse_mode = 0;

		while (NODE_END != curr_node->type)				/* Loop until end of statement.			*/
		{
			if ( !curr_node->token)
			{
				/* No token on this node, do nothing */
			}
			else if ( eq_token(curr_node->token, KEYWORD, "EXTERNAL") )
			{
				fd_parse_mode = FD_EXTERNAL;
			}
			else if ( eq_token(curr_node->token, KEYWORD, "BLOCK") )
			{
				fd_parse_mode = FD_BLOCK;
				write_log("WISP",'I',"BLOCKCONTAINS","Removing BLOCK CONTAINS from FD.");
			}
			else if ( eq_token(curr_node->token, KEYWORD, "RECORD") )
			{
				fd_parse_mode = FD_RECORD;
				if (delrecon)
				{
					write_log("WISP",'I',"RECORDCONTAINS","Removing RECORD CONTAINS from FD.");
				}
				else
				{
					write_log("WISP",'I',"RECORDCONTAINS","Processing RECORD CONTAINS from FD.");
					curr_node->token->column_fixed = 1;
				}
			}
			else if ( eq_token(curr_node->token, KEYWORD, "COMPRESSED") )
			{
				/*
				**	Always remove the COMPRESSED clause
				*/
				cleartoknode(curr_node);
			}
			else if ( eq_token(curr_node->token, KEYWORD, "LABEL") )
			{
				fd_parse_mode = FD_LABEL;
				write_log("WISP",'I',"LABELRECORD","Removing LABEL RECORD from FD.");

				/*
				**	We are deleting "LABEL RECORD ..." clauses.
				**	Need to position past the RECORD keyword so it is not confused
				**	for a "RECORD CONTAINS ..." clause.
				*/
				cleartoknode(curr_node);			/* remove the LABEL keyword			*/
				curr_node = curr_node->next;			/* Skip over RECORD keyword			*/
			}
			else if ( eq_token(curr_node->token, KEYWORD, "VALUE") )
			{
				fd_parse_mode = FD_VALUE;
			}
			else if ( eq_token(curr_node->token, KEYWORD, "DATA") )
			{
				if ( eq_token(curr_node->next->token, KEYWORD, "RECORD") ||
				     eq_token(curr_node->next->token, KEYWORD, "RECORDS")  )
				{
					fd_parse_mode = FD_DATA;
					curr_node->token->column_fixed = 1;
					curr_node = curr_node->next;		/* Skip over RECORD keyword			*/
				}
				curr_node->token->line   = 0;
			}
			else if ( eq_token(curr_node->token, KEYWORD, "CODE-SET") )
			{
				fd_parse_mode = FD_CODESET;
				curr_node->token->column_fixed = 1;
				curr_node->token->line   = 0;
			}
			else if ( PERIOD == curr_node->token->type )
			{
				fd_parse_mode = 0;
				curr_node->token->column = 0;
				curr_node->token->line   = 0;
			}
			else
			{
				curr_node->token->line   = 0;
			}

			switch(fd_parse_mode)
			{
			case FD_BLOCK:
				cleartoknode(curr_node);
				break;

			case FD_RECORD:
				if (delrecon)
				{
					cleartoknode(curr_node);
				}
				break;

			case FD_LABEL:
				cleartoknode(curr_node);
				break;

			case FD_VALUE:
				if ( eq_token(curr_node->token, 0, "FILENAME") )
				{
					cleartoknode(curr_node);
					curr_node = curr_node->next;
					if ( eq_token(curr_node->token, KEYWORD, "IS") )
					{
						cleartoknode(curr_node);
						curr_node = curr_node->next;
					}
					strcpy(prog_fnames[g_curr_file_num],curr_node->token->data);
					write_log("WISP",'I',"FILENAME","VALUE OF FILENAME is %s",prog_fnames[g_curr_file_num]);
					cleartoknode(curr_node);
				}
				else if ( eq_token(curr_node->token, 0, "LIBRARY") )
				{
					cleartoknode(curr_node);
					curr_node = curr_node->next;
					if ( eq_token(curr_node->token, KEYWORD, "IS") )
					{
						cleartoknode(curr_node);
						curr_node = curr_node->next;
					}
					strcpy(prog_lnames[g_curr_file_num],curr_node->token->data);
					write_log("WISP",'I',"LIBRARY","VALUE OF LIBRARY is %s",prog_lnames[g_curr_file_num]);
					cleartoknode(curr_node);
				}
				else if ( eq_token(curr_node->token, 0, "VOLUME") )
				{
					cleartoknode(curr_node);
					curr_node = curr_node->next;
					if ( eq_token(curr_node->token, KEYWORD, "IS") )
					{	
						cleartoknode(curr_node);
						curr_node = curr_node->next;
					}
					strcpy(prog_vnames[g_curr_file_num],curr_node->token->data);
					write_log("WISP",'I',"VOLUME","VALUE OF VOLUME is %s",prog_vnames[g_curr_file_num]);
					cleartoknode(curr_node);
				}
				else
				{
					cleartoknode(curr_node);
				}
				break;
			}

			/*
			**	Advance to next node in the statement
			*/
			curr_node = curr_node->next;
		}

		if (!comments) tput_blank();

		if (is_dbfile(the_file)) 
		{
			dbfile_write_mark(the_file);
		}

		tput_statement(12,the_statement);
		free_statement(the_statement);

	}
	else if (is_fd && crt_fd) 						/* Found a FD for the crt			*/
	{
		/*
		**	Handle CRT FILES.
		*/

		write_log("WISP",'I',"CRTFD","processing FD for a crt file %s.",the_file);

		tput_fluff(file_name_node->next);
		free_statement(file_name_node->next);				/* Free end of statement.			*/
		file_name_node->next = maketoknode(make_token(PERIOD,"."));	/* Terminate "FD filename" with a period.	*/

		if (!comments) tput_blank();
		tput_statement(8,the_statement);				/* Write the dummy FD for the dummy CRT file	*/
		free_statement(the_statement);					/* Free the FD statement, not needed		*/

		tput_line("       01  WISP-CRT-FILE-RECORD-%d  PIC X.",cur_crt);/* Add "01 WISP-CRT-FILE-RECORD-x PIC X."	*/

		next_statement = parse_crt_records();				/* process the CRT file records			*/

		write_log("WISP",'I',"FINISHCRT","Finished processing crt file %s",crt_file[cur_crt]);
	}
	else
	{
		/*
		**	Process the data description records.
		*/

		next_statement = (NODE)parse_data_description(the_statement);
	}

	goto get_next_statement;
}


#define MAX_RECORD_CNT		500							/* The max number of record names	*/

static struct
	{
		char	name[40];
		short	fd;
	} record_list[MAX_RECORD_CNT];							/* List of every record name & it's fd	*/
static int record_cnt = 0;								/* Count of records in record_list	*/

int add_to_record_list(char *recname)
{
	strcpy(record_list[record_cnt].name,recname);
	record_list[record_cnt].fd = g_curr_file_num;
	record_cnt += 1;
	if ( record_cnt >= MAX_RECORD_CNT )
		write_log("WISP",'F',"MAXRECCNT","MAX_RECORD_CNT exceeded.\n");
	return 0;
}

int fd_record(recname)
char	*recname;
{
	int	i;

	for (i=0; i<record_cnt; i++)
	{
		if ( strcmp(recname,record_list[i].name) == 0 ) 
			return(record_list[i].fd);
	}
	return (-1);
}


/*
**	Routine:	parse_crt_records()
**
**	Function:	To ...
**
**	Description:	To ...
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
**	mm/dd/yy	Written by xxx
**
*/
NODE parse_crt_records()
{
	NODE	the_statement;
	NODE	curr_node;
	NODE	level_node, data_name_node, occurs_node, picture_node, redefines_node, binary_node, pic_node;
	NODE	period_node, comp_node;
	int	i;
	int	redefines_level;
	int	curr_level;
	int	first_crt_record;
	int	first_crt_record_count;
	int	occurs_depth;
	int	occurs_level[10];
	int	occurs_count[10];

	if (!crt_file_ptr)
	{
		/*
		**	CRT file FD records must be relocated into Working-Storage.
		**	They are written now to a temp file which later will be loaded.
		**
		**	NOTE: 	We open output to create this file once and leave it open
		**		so that next time thru we append to it. The close will
		**		be done when we go to copy the file back into the cob file.
		*/
		crt_file_ptr = open_cob_file(crt_fname,FOR_OUTPUT,1);		/* open a scratch file				*/
	}

	/*
	**	Switch the output stream to use the CRT temp file.
	*/
	override_output_stream(crt_file_ptr);

	crt_prime_rec[cur_crt] = crt_record_count;				/* Save primary record index			*/

	occurs_depth = 0;
	redefines_level = 0;
	first_crt_record = 1;

	for(;;)
	{
		the_statement = get_statement();

		level_node = data_name_node = occurs_node = picture_node = redefines_node = binary_node = NULL;
		pic_node = period_node = comp_node = NULL;

		curr_node = the_statement->next;

		if ( NUMBER == curr_node->token->type )
		{
			level_node = curr_node;
			sscanf(level_node->token->data,"%d",&curr_level);

			curr_node = curr_node->next;
			if ( eq_token(curr_node->token, KEYWORD, "FILLER") ||
			     IDENTIFIER == curr_node->token->type 		)
			{
				data_name_node = curr_node;
				curr_node = curr_node->next;
			}
		}
		else
		{
			curr_level = 0;
		}

		if (1==curr_level || 0==curr_level)
		{
			if (!first_crt_record)
			{
				/*
				**	We have finished the previous record.
				*/

				write_log("WISP",'I',"CRTSIZE","%s size is %d.", 
					crt_record[crt_record_count],crt_record_size[crt_record_count]);
				crt_record_count++;
			}

			if (0==curr_level)
			{
				/*
				**	We are done with this CRT file FD records.
				**	Switch the output stream back.
				**
				**	NOTE:	We don't close the crt_file, it is left open so it can be appended
				**		to later.
				*/

				release_output_stream();

				/*
				**	Return with the already loaded statement.
				*/
				return(the_statement);
			}

			strcpy(crt_record[crt_record_count],data_name_node->token->data);

			write_log("WISP",'I',"CRTRECORD","CRT record -- %s",crt_record[crt_record_count]);
			crt_record_size[crt_record_count] = 0;

			if (first_crt_record)
			{
				first_crt_record_count = crt_record_count;
				first_crt_record = 0;
			}
			else
			{
				/*
				**	This CRT file FD has multiple 01 records.  Multiple 01's under an FD 
				**	implicitly redefine each other.  Since we are relocating these to 
				**	working-storage we have to add explicit REDEFINES on each additional 01.
				**
				**	01  REC-01.
				**	    05  xxx ...
				**	    ....
				**	01  REC-02 REDEFINES REC-01.
				**	    05  xxx ...
				**	    ....
				**	01  REC-03 REDEFINES REC-01.
				**	    05  xxx ...
				**
				*/
				tie_next(tie_next(data_name_node,
					maketoknode(make_token(KEYWORD,"REDEFINES"))),
					maketoknode(make_token(IDENTIFIER,crt_record[first_crt_record_count])));
			}

			redefines_level = 0;
			occurs_depth = 0;
		}


		if (redefines_level)
		{
			/*
			**	Currently in a REDEFINES, check if stepped out of it.
			*/
			if (curr_level <= redefines_level)
			{
				redefines_level = 0;
			}
		}

		while (occurs_depth)
		{
			/*
			**	Currently in an OCCURS, check if stepped out of it to higher level.
			*/
			if (curr_level <= occurs_level[occurs_depth-1])
			{
				occurs_depth--;
			}
			else
			{
				break;
			}
		}

		while (NODE_END != curr_node->type)				/* Loop until end of statement.			*/
		{
			if ( eq_token(curr_node->token, KEYWORD, "OCCURS") )
			{
				occurs_node = curr_node;
			}
			else if ( eq_token(curr_node->token, KEYWORD, "BINARY") )
			{
				binary_node = curr_node;
			}
			else if ( eq_token(curr_node->token, KEYWORD, "COMP") ||
			          eq_token(curr_node->token, KEYWORD, "COMPUTATIONAL") )
			{
				comp_node = curr_node;
				edit_token(curr_node->token,packed_decimal());	/* Edit the computational clause in place	*/
			}
			else if ( eq_token(curr_node->token, KEYWORD, "PIC") ||
			          eq_token(curr_node->token, KEYWORD, "PICTURE") )
			{
				pic_node = curr_node;
			}
			else if ( PICTURE == curr_node->token->type )
			{
				picture_node = curr_node;
			}
			else if ( eq_token(curr_node->token, KEYWORD, "REDEFINES") )
			{
				redefines_node = curr_node;
			}
			else if ( PERIOD == curr_node->token->type )
			{
				period_node = curr_node;
			}

			curr_node = curr_node->next;
		}

		if (!redefines_level && redefines_node)
		{
			redefines_level = curr_level;
		}

		if (!redefines_level && occurs_node)
		{
			occurs_depth++;
			occurs_level[occurs_depth-1] = curr_level;
			sscanf(occurs_node->next->token->data,"%d",&occurs_count[occurs_depth-1]);
		}

		if (!redefines_level && (picture_node || binary_node))
		{
			int	dd_size;

			dd_size = 0;

			if (picture_node)
			{
				dd_size = pic_size(picture_node->token->data);
			}

			if (binary_node)
			{
				if (picture_node)
				{
					if (dd_size > 5) dd_size = 2;
					else		 dd_size = 4;
				}
				else
				{
					dd_size = 2;
				}
			}

			if (comp_node)
			{
				dd_size = (dd_size + 2) / 2;
			}

			for(i=0; i<occurs_depth; i++)
			{
				dd_size *= occurs_count[i];
			}

			crt_record_size[crt_record_count] += dd_size;
		}

		if (binary_node)
		{
			if (pic_node)
			{
				/*
				**	There was a picture on this item so edit the binary but
				**	no additional cleanup is needed.
				*/
				edit_token(binary_node->token, bin4_type);
				write_log("WISP",'I',"REPLBINARY","Replaced USAGE BINARY with %s.",bin4_type);
			}
			else
			{
				/*
				**	Found a binary with no picture clause. Add a picture clause.
				**	NOTE: We are NOT doing multilevel binary group fixup!
				*/
				edit_token(binary_node->token, bin2_type);
				write_log("WISP",'I',"REPLBINARY","Replaced BINARY with %s PIC S9(4).",bin2_type);

				pic_node = maketoknode(make_token(KEYWORD,"PIC"));
				picture_node = maketoknode(make_token(PICTURE,"S9(4)"));
				tie_next(tie_next(binary_node,pic_node), picture_node);
			}
		}

		/*
		**	Write the statement to the CRT temp file.
		*/

		tput_statement(12,the_statement);
		free_statement(the_statement);
	}
}

NODE delete_fd(the_statement)
NODE the_statement;
{
	NODE	curr_node;

	for(;;)
	{
		free_statement(the_statement);
		the_statement = get_statement();

		if (!the_statement) return(the_statement);

		curr_node = the_statement->next;

		if (eq_token(curr_node->token,KEYWORD,"FD") ||
		    eq_token(curr_node->token,KEYWORD,"SD") ||
		    eq_token(curr_node->token,KEYWORD,"WORKING-STORAGE") ||
		    eq_token(curr_node->token,KEYWORD,"LINKAGE") ||
		    eq_token(curr_node->token,KEYWORD,"PROCEDURE"))
		{
			return(the_statement);
		}
	}
}
/*
**	History:
**	$Log: wt_datad.c,v $
**	Revision 1.10  1997-08-28 17:46:23-04  gsl
**	Add SCREEN SECTION
**
**	Revision 1.9  1996-08-30 21:56:14-04  gsl
**	drcs update
**
**
**
*/
