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

/*
**	File:		wt_input.c
**
**	Project:	WISP/TRAN
**
**	Purpose:	INPUT-OUTPUT SECTION
*/

/*
**	Includes
*/


#include <string.h>

#define EXT extern
#include "wisp.h"
#include "crt.h"
#include "cobfiles.h"
#include "keylist.h"
#include "directiv.h"
#include "node.h"
#include "token.h"
#include "wmalloc.h"
#include "statment.h"

/*
**	Structures and Defines
*/
#define MAX_ALT_KEYS	64

#define RELATIVE	0x0001
#define	INDEX		0x0002
#define SEQUENTIAL	0x0004
#define RANDOM		0x0008
#define DYNAMIC		0x0010


/*
**	Globals and Externals
*/

/*
**	Static data
*/


static	int	ioc_found;								/* Was I-O-CONTROL found		*/

static	int	mf_compress = 0;

/*
**	Static Function Prototypes
*/

static int write_sel(int printer, int this_file, int org, int accmod, int optional_found);	/* Write initial part of SELECT.	*/
static int del_sel(char* the_file);							/* Determine if delete current SELECT.	*/
static NODE get_record_key(NODE curr_node, char* reckey, char* reckeyqual, char** splitkey);
static int add2buff(char *buff, char *addstr, int col, int newline);
static int chk_sort(const char* fname);							/* Check list of SORT files.		*/

#ifdef NOT_USED
static int gen_io_control(void);
#endif

/*
**	Routine:	input_output_section()
**
**	Function:	To process the INPUT-OUTPUT SECTION.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not INPUT-OUTPUT SECTION then one will be generated.
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
NODE input_output_section(NODE the_statement)
{
	if (!eq_token(the_statement->next->token,KEYWORD,"INPUT-OUTPUT"))
	{
		write_log("WISP",'I',"IOSECT","INPUT-OUTPUT SECTION not found.");

		tput_line_at(8, "INPUT-OUTPUT SECTION.");
	}
	else
	{
		write_log("WISP",'I',"IOSECT","Processing INPUT-OUTPUT SECTION.");
		division = INPUT_OUTPUT_SECTION;

		tput_statement(12,the_statement);
		free_statement(the_statement);

		the_statement = get_statement();
	}

	/*
	**	FILE-CONTROL paragraph.
	*/
	the_statement = file_control_para(the_statement);

	/*
	**	I-O-CONTROL paragraph.
	*/
	the_statement = io_control_para(the_statement);

	return(the_statement);
}

/*
**	Routine:	file_control_para()
**
**	Function:	To process the FILE-CONTROL paragraph.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not FILE-CONTROL paragraph then one will be generated.
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
NODE file_control_para(NODE the_statement)
{
	if (!eq_token(the_statement->next->token,KEYWORD,"FILE-CONTROL"))
	{
		write_log("WISP",'I',"FILECTRL","FILE-CONTROL paragraph not found.");

		tput_line_at(8,  "FILE-CONTROL.");
		tput_line_at(12, "SELECT WISP-DECLARATIVES-FILE");		/* And the dummy file for DISPLAY & READ	*/
		tput_line_at(12, "    ASSIGN TO \"WISP-DISPFILE\".");

		return(the_statement);
	}

	write_log("WISP",'I',"FILECTRL","Processing FILE-CONTROL paragraph.");

	tput_statement(12,the_statement);
	free_statement(the_statement);

	tput_line_at(12, "SELECT WISP-DECLARATIVES-FILE");			/* And the dummy file for DISPLAY & READ	*/
	tput_line_at(12, "    ASSIGN TO \"WISP-DISPFILE\".");

	the_statement = get_statement();
	the_statement = parse_selects(the_statement);

	return(the_statement);
}

/*
**	Routine:	io_control_para()
**
**	Function:	To process the I-O-CONTROL paragraph.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not I-O-CONTROL paragraph then one will be generated.
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
**	10/12/93	Add pre-printing of fluff for vax_cobol.
**			This was to fix Sterling code problems. GSL
**
*/
NODE io_control_para(NODE the_statement)
{
	if (!eq_token(the_statement->next->token,KEYWORD,"I-O-CONTROL"))
	{
		write_log("WISP",'I',"IOCTRL","I-O-CONTROL paragraph not found.");

		return(the_statement);
	}

	write_log("WISP",'I',"IOCTRL","Processing I-O-CONTROL paragraph.");

	tput_statement(12,the_statement);
	free_statement(the_statement);
	ioc_found = 1;								/* Found I-O-CONTROL paragraph			*/

	the_statement = get_statement();

	if (!eq_token(the_statement->next->token,KEYWORD,"DATA") &&
	    !eq_token(the_statement->next->token,KEYWORD,"PROCEDURE")   )
	{
		tput_statement(12,the_statement);
		free_statement(the_statement);

		the_statement = get_statement();
	}

	return(the_statement);
}

/*
**	Routine:	parse_selects()
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
NODE parse_selects(NODE next_statement)
{
	int 	i;
	char	reckey[40],								/* The RECORD KEY.			*/
		reckeyq[40],								/* The reckey qualification		*/
		areas[8],								/* RESERVE AREAS.			*/
		relkey[40],								/* The RELATIVE KEY.			*/
		relkeyq[40];								/* The relkey qual			*/
	char	altkey[MAX_ALT_KEYS][40];						/* Table of alternate record keys.	*/
	char	altkeyq[MAX_ALT_KEYS][40];						/* Table of altkey qual.		*/
	char	*splitkey, *altsplitkey[MAX_ALT_KEYS];					/* Splitkey ptrs.			*/
	int	duplic[MAX_ALT_KEYS];							/* Table of duplicates.			*/
	int	org;									/* Organization.			*/
	int	accmod;									/* Access Mode.				*/
	int	num_alt;
	int	printer;
	int	this_file;
	int	seq_org_flag, seq_acc_flag;						/* Did we really see the SEQ clause ?	*/
	char	seqtype[10];
	int	optional_found;
	int	was_seq_dyn;
	char	buff[128], *ptr;

	NODE	the_statement;
	NODE	curr_node, name_node, prname_node;

get_next_statement:
	if (next_statement)
	{
		the_statement = next_statement;
		next_statement = NULL;
	}
	else
	{
		the_statement = get_statement();
	}

	curr_node = the_statement->next;
	if (!eq_token(curr_node->token,KEYWORD,"SELECT"))
	{
		tput_set_context(NULL);

		return(the_statement);
	}

	/*
	**	We found a SELECT statement.
	*/

	tput_fluff(the_statement->down);

	tput_set_context(curr_node->token->context);

	if (prog_cnt > MAX_FILES)
	{
		write_log("WISP",'F',"FILEBUFEXC","Maximum number of files exceeded in SELECT statement.");
		exit_with_err();
	}

	prog_ftypes[prog_cnt] = DISK_FILE;					/* assume it is a disk file		*/
	printer = 0;
	optional_found = 0;

	/*
	**	Parse the SELECT clause portion of the statement.
	**
	**	SELECT [OPTIONAL] file-name ASSIGN [TO] { "prname"  } [Device] 
	**						      { data-name }
	**
	**		[DISPLAY|NODISPLAY] [[NO] RESPECIFY|NORESPECIFY]
	**
	**	Device = "STANDARD-DISK" | "DISK" | "DISPLAY" | "PRINTER" | "TAPE"
	*/

	curr_node = curr_node->next;
	if (eq_token(curr_node->token,KEYWORD,"OPTIONAL"))
	{
		optional_found = 1;
		prog_ftypes[prog_cnt] |= NORESPECIFY;	/* Specifing OPTIONAL is like specifing NORESPECIFY	*/
		curr_node = curr_node->next;
	}

	name_node = curr_node;
	if (IDENTIFIER != name_node->token->type)
	{
		if (KEYWORD == name_node->token->type)
		{
			write_tlog(name_node->token,"WISP",'W',"KEYWORD",
				   "Keyword [%s] used as filename in SELECT, adding to keyword list",
				   token_data(name_node->token));
			add_change_word(token_data(name_node->token));
			do_change_word_token(name_node->token);
		}
		else
		{
			write_tlog(name_node->token,"WISP",'E',"FILENAME","Error parsing SELECT, invalid filename [%s]",
				token_data(name_node->token));
		}
	}

	/*
	**	Add the SELECT name to the symbol table
	*/
	add_user_symbol(token_data(name_node->token));

	if (sortfile)
	{
		if (opt_sort_file_cnt == MAX_SF_ITEMS)
		{
			write_log("WISP",'E',"MAXSFITEMS","Maximum number of SORT_FILE items exceeded.");
		}
		else
		{
			opt_sort_file_item[opt_sort_file_cnt++] = (char*)wdupstr(token_data(name_node->token));	/* Save the name of the file.		*/
		}

		sortfile = 0;
	}

	if (dbfile)
	{
		/*
		**	A $DBFILE directive was found before this SELECT so add this one to the list
		**	of database files.
		*/
		dbfile_add_item(token_data(name_node->token),dbfile_tabname);
		dbfile = 0;
		dbfile_tabname[0] = (char)0;
	}

	if (is_dbfile(token_data(name_node->token))) 
	{
		prog_ftypes[prog_cnt] |= DBFILE_FILE;
	}

	if (openioxfile || openioxall)
	{
		prog_ftypes[prog_cnt] |= OPENIOX_FILE;
		openioxfile = 0;
	}

	curr_node = curr_node->next;
	if (!eq_token(curr_node->token,KEYWORD,"ASSIGN"))
	{
		write_tlog(curr_node->token,"WISP",'F',"ASSIGN","Error parsing SELECT, expecting ASSIGN found [%s]",
			token_data(curr_node->token));
		exit_with_err();
	}

	curr_node = curr_node->next;
	if (eq_token(curr_node->token,KEYWORD,"TO"))
	{
		curr_node = curr_node->next;
	}

	prname_node = curr_node;

	curr_node = curr_node->next;
	if ( LITERAL == curr_node->token->type )
	{
		if (eq_token_literal(curr_node->token,"DISPLAY"))
		{
			prog_ftypes[prog_cnt] = -1;
		}
		else if (eq_token_literal(curr_node->token,"PRINTER"))
		{
			FSET(prog_ftypes[prog_cnt],PRINTER_FILE);		/* it is a print file			*/
			printer = 1;
			/*
			**	autolockprint is now the default unless $NOAUTOLOCKPRINT is found
			*/
			if (autolockprint) prog_ftypes[prog_cnt] |= AUTOLOCK;	/* Turn on automatic locking flag	*/
		}
		else if (eq_token_literal(curr_node->token,"TAPE"))
		{
			prog_ftypes[prog_cnt] = TAPE_FILE;
		}

		curr_node = curr_node->next;
	}

	if (eq_token(curr_node->token,KEYWORD,"DISPLAY") ||
	    eq_token(curr_node->token,KEYWORD,"NODISPLAY")  )
	{
		curr_node = curr_node->next;					/* Skip over NODISPLAY			*/
	}

	if (eq_token(curr_node->token,KEYWORD,"NORESPECIFY") ||
	    eq_token(curr_node->token,KEYWORD,"NO")		)
	{
		write_log("WISP",'I',"RESPEC","Flagged NORESPECIFY phrase.");
		prog_ftypes[prog_cnt] |= NORESPECIFY;				/* set the flag bit			*/
		curr_node = curr_node->next;
	}
	if (eq_token(curr_node->token,KEYWORD,"RESPECIFY"))
	{
		curr_node = curr_node->next;
	}

	*seqtype = '\0';

	if ( printer || seqline) 
	{
		strcpy(seqtype,"LINE ");
	}
	if ( seqbinary )
	{
		if (mf_cobol) 	strcpy(seqtype,"RECORD ");
		else 		strcpy(seqtype,"BINARY ");
	}
	seqline = 0;
	seqbinary = 0;

	if (prog_ftypes[prog_cnt] == -1)					/* is it a DISPLAY?			*/
	{
		if (opt_data_conv)
		{
			free_statement(the_statement);
			goto get_next_statement;
		}

		cur_crt = crt_fcount;
		crt_cursor[crt_fcount][0] = '\0';				/* may not be a cursor field		*/
		crt_pfkey[crt_fcount][0] ='\0';					/* Or a pfkey field.			*/
		crt_status[crt_fcount][0] = '\0';				/* Or a file status field.		*/
		crt_relative[crt_fcount][0] = '\0';				/* Or a relative key field.		*/

		write_log("WISP",'I',"SELDISPLAY","SELECT for the display, name is %s",token_data(name_node->token));
		strcpy(crt_file[crt_fcount],token_data(name_node->token));	/* copy the screen file name		*/

		tput_line_at(12, "SELECT %s",crt_file[crt_fcount]);		/* include SELECT for dummy CRT file	*/
		tput_line_at(16, "ASSIGN TO \"WISP-CRTFILE\".");

		while( curr_node && PERIOD != curr_node->token->type )
		{
			if (!curr_node->token)
			{
				/* Skip over */
			}
			else if (eq_token(curr_node->token,0,"PFKEY"))		/* it's the PFKEY keyword		*/
			{
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,0,"IS"))		curr_node = curr_node->next;
				strcpy(crt_pfkey[crt_fcount],token_data(curr_node->token)); /* get pfkey name			*/
				write_log("WISP",'I',"PFKEYVAL","  PFKEY is %s",crt_pfkey[crt_fcount]);
			}
			else if (eq_token(curr_node->token,0,"CURSOR"))		/* it's the CURSOR keyword		*/
			{							/* find out what the field name is	*/
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,0,"POSITION"))	curr_node = curr_node->next;
				if (eq_token(curr_node->token,0,"IS"))		curr_node = curr_node->next;
				strcpy(crt_cursor[crt_fcount],token_data(curr_node->token));
				write_log("WISP",'I',"CURSORVAL","  CURSOR POSITION is %s",crt_cursor[crt_fcount]);
			}
			else if (eq_token(curr_node->token,0,"RELATIVE"))		/* it's the RELATIVE keyword		*/
			{						/* find out what the field name is	*/
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,0,"KEY"))		curr_node = curr_node->next;
				if (eq_token(curr_node->token,0,"IS"))		curr_node = curr_node->next;
				strcpy(crt_relative[crt_fcount],token_data(curr_node->token));
				write_log("WISP",'I',"RELKEYVAL","  RELATIVE KEY is %s",crt_relative[crt_fcount]);
			}
			else if (eq_token(curr_node->token,0,"STATUS"))		/* it's the STATUS keyword		*/
			{
				curr_node = curr_node->next;
				if (eq_token(curr_node->token,0,"IS"))		curr_node = curr_node->next;
				strcpy(crt_status[crt_fcount],token_data(curr_node->token));
				write_log("WISP",'I',"STATVAL","  STATUS is %s",crt_status[crt_fcount]);
			}

			curr_node = curr_node->next;
		}
						
		if (!crt_pfkey[crt_fcount][0]) 					/* Make sure we have a pfkey value	*/
		{
			strcpy(crt_pfkey[crt_fcount],"WISP-PFKEY-VALUE");
		}
										/* And a file status.			*/
		if (!crt_status[crt_fcount][0]) 
		{
			strcpy(crt_status[crt_fcount],"WISP-FILE-STATUS");
		}
		crt_fcount++;							/* Now count up one.			*/
	}
	else									/* was not a display, fix it up		*/
	{
		/*
		**	Handle a real file (non-crt)
		*/

		strcpy(prog_files[prog_cnt],token_data(name_node->token));	/* save it in the table			*/

		if (del_sel(prog_files[prog_cnt])) 				/* Does this SELECT need to be deleted	*/
		{
			free_statement(the_statement);
			goto get_next_statement;
		}

		this_file = prog_cnt;						/* Remember this file.			*/
		prog_cnt++;

		if (chk_sort(prog_files[this_file]))
		{
			/*
			**	This is a sort file so default to automatic locking.
			*/
			autolockfile = 1;
		}

		if (autolockfile)						/* if autolockflag is set then...	*/
		{
			prog_ftypes[this_file] |= AUTOLOCK;			/* mark this file for automatic locking */
			autolockfile = 0;					/* Clear the flag.			*/
		}
		if (noautolockfile)						/* if noautolockflag is set then...	*/
		{
			if (prog_ftypes[this_file] & AUTOLOCK)			/* If AUTOLOCK is set			*/
			{
				prog_ftypes[this_file] ^= AUTOLOCK;		/* XOR the autolocking flag off.	*/
			}
			noautolockfile = 0;					/* Clear the flag.			*/
		}
		prog_vnames[this_file][0] = '\0';				/* clear volume name			*/
		prog_lnames[this_file][0] = '\0';				/* clear library name			*/
		prog_fnames[this_file][0] = '\0';				/* clear file name			*/
		prog_fstats[this_file][0] = '\0';				/* clear file status field		*/
		prog_ref[this_file] = 0;					/* not opened yet.			*/


		/*
		**	Add the generated file field names to the symbol table 
		**	and to the reserved keywords list to prevent conflicts with user data items.
		**
		**	Note: We add the F-, L-, V- names even though they may not be needed because
		**	we won't know until we process the FD which may be too late. The prog values
		**	are are null at this point so the get_prog_Xname() routines will generate
		**	the desired names.
		*/
		add_user_symbol(get_prog_nname(this_file));
		add_user_symbol(get_prog_fname(this_file));
		add_user_symbol(get_prog_lname(this_file));
		add_user_symbol(get_prog_vname(this_file));
		add_user_symbol(get_prog_prname(this_file));
		add_user_symbol(get_prog_status(this_file));

		add_change_word(get_prog_nname(this_file));
		add_change_word(get_prog_fname(this_file));
		add_change_word(get_prog_lname(this_file));
		add_change_word(get_prog_vname(this_file));
		add_change_word(get_prog_prname(this_file));
		add_change_word(get_prog_status(this_file));

		strcpy(buff,token_data(prname_node->token));

		remove_quotes(buff);	/* Remove any quotes.			*/

		if ((ptr = strchr(buff,' ')))
		{
			*ptr = (char)0;
		}
		if (strlen(buff) > 8)
		{
			write_tlog(prname_node->token,"WISP",'W',"PRNAME",
				   "Long PRNAME \"%s\" truncated to 8 characters.", buff);
			buff[8] = '\0';
		}
		
		strcpy(prog_prname[this_file],buff);				/* Save the prname.			*/

		{
			reckey[0] = '\0';					/* Doesn't have a record key.		*/
			reckeyq[0] = '\0';					/* Doesn't have a record key.		*/
			splitkey = NULL;					/* No splitkey 				*/
			areas[0] = '\0';					/* No areas reserved.			*/
			relkey[0] = '\0';					/* No relative key defined.		*/
			relkeyq[0] = '\0';					/* No relative key defined.		*/
			org = 0;						/* Organization is not known yet.	*/
			accmod = 0;						/* Access mode is not known yet.	*/
			seq_org_flag = 0;					/* Have not seen SEQ clause		*/
			seq_acc_flag = 0;					/* Have not seen SEQ clause		*/
			num_alt = 0;						/* There are no alternate record keys.	*/


			while( curr_node && PERIOD != curr_node->token->type )
			{

				if (eq_token(curr_node->token,0,"ALTERNATE"))		/* alternate record key			*/
				{
					write_log("WISP",'I',"BEGALTRECKEY","Begin ALTERNATE RECORD KEY");
					curr_node = curr_node->next;			/* token RECORD				*/
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,0,"KEY"))	curr_node = curr_node->next;

					for(;;)
					{
						if (NUMBER==curr_node->token->type) curr_node = curr_node->next;

						if (num_alt == MAX_ALT_KEYS)
						{
							write_log("WISP",'F',"ALTKEYEXC","Number of alternate keys exceeded.");
							exit_with_err();
						}
											/* Get the record key			*/
						curr_node = get_record_key(curr_node, altkey[num_alt],altkeyq[num_alt],
									&altsplitkey[num_alt]);

						key_name(altkey[num_alt],altkeyq[num_alt]); /* Weve got a key, test for edits	*/

						duplic[num_alt] = 0;			/* No duplicates.			*/
						num_alt++;

						if (eq_token(curr_node->next->token,KEYWORD,"WITH"))	
							curr_node = curr_node->next;
						if (eq_token(curr_node->next->token,KEYWORD,"DUPLICATES"))
						{
							curr_node = curr_node->next;
							duplic[num_alt-1] = 1;
						}

						/*
						**	Look ahead and decide if another key.
						*/
						if (NUMBER==curr_node->next->token->type ||
						    IDENTIFIER==curr_node->next->token->type ||
						    eq_token(curr_node->next->token,KEYWORD,"IS") )
						{
							curr_node = curr_node->next;
						}
						else
						{
							break;
						}
					}
					write_log("WISP",'I',"COMALTRECKEY","Completed ALTERNATE RECORD KEY");
				}
				else if (eq_token(curr_node->token,KEYWORD,"SEQUENTIAL"))	/* Process organization.	*/
				{
					seq_org_flag = 1;				/* We really found the SEQ keyword	*/
					org = SEQUENTIAL;
				}
				else if (eq_token(curr_node->token,KEYWORD,"INDEXED"))	/* Process organization.	*/
				{
					write_log("WISP",'I',"ISINDEX","INDEXED file detected.");
					FSET(prog_ftypes[this_file],INDEXED_FILE);    	/* It is an indexed file.	*/
					org = INDEX;					/* Set the mode.			*/
				}
				else if (eq_token(curr_node->token,KEYWORD,"RELATIVE"))	
				{
					if (eq_token(curr_node->next->token,0,"KEY"))
					{
						write_tlog(curr_node->token, "WISP",'W',"RELKEYVAL",
							"RELATIVE KEY found not on ACCESS clause");
						curr_node = curr_node->next;
						curr_node = curr_node->next;
						curr_node = get_record_key(curr_node,relkey,relkeyq,&splitkey);
						key_name(relkey,relkeyq);
						write_log("WISP",'I',"RELKEYVAL","  RELATIVE KEY is %s",relkey);
					}
					else
					{
						org = RELATIVE;
					}
				}
				else if (eq_token(curr_node->token,KEYWORD,"ACCESS"))	/* Process access mode.			*/
				{
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,0,"MODE"))	curr_node = curr_node->next;
					if (eq_token(curr_node->token,0,"IS"))		curr_node = curr_node->next;

					if (eq_token(curr_node->token,0,"SEQUENTIAL"))
					{
						seq_acc_flag = 1;			/* We really found the SEQ keyword	*/
						accmod = SEQUENTIAL;			/* Set the mode.			*/
					}
					else if (eq_token(curr_node->token,0,"RANDOM"))
					{
						accmod = RANDOM;
					}
					else if (eq_token(curr_node->token,0,"DYNAMIC"))
					{
						accmod = DYNAMIC;
					}

					if (eq_token(curr_node->next->token,KEYWORD,"RELATIVE")) /* Get the relative key.	*/
					{
						curr_node = curr_node->next;
						curr_node = curr_node->next;
						if (eq_token(curr_node->token,0,"KEY"))
						{
							curr_node = curr_node->next;
						}
	
						curr_node = get_record_key(curr_node,relkey,relkeyq,&splitkey);
						key_name(relkey,relkeyq);
						write_log("WISP",'I',"RELKEYVAL","  RELATIVE KEY is %s",relkey);
					}

				}
				else if (eq_token(curr_node->token,KEYWORD,"STATUS"))	/* FILE STATUS				*/
				{
					write_log("WISP",'I',"BEGFILESTATUS","Begin FILE STATUS");
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,0,"IS"))		curr_node = curr_node->next;

					strcpy(prog_fstats[this_file],token_data(curr_node->token));
					write_log("WISP",'I',"COMFILESTATUS","Completed FILE STATUS.");
				}
				else if (eq_token(curr_node->token,KEYWORD,"BUFFER"))	/* BUFFER SIZE IS NN BLOCKS phrase	*/
				{
					write_log("WISP",'I',"REMBUFSIZ","Remove BUFFER SIZE phrase.");
				}
				else if (eq_token(curr_node->token,KEYWORD,"RECORD"))	/* Get the record key.			*/
				{
					curr_node = curr_node->next;
					if (eq_token(curr_node->token,0,"KEY"))		curr_node = curr_node->next;

					curr_node = get_record_key(curr_node,reckey,reckeyq,&splitkey);
					key_name(reckey,reckeyq);

					write_log("WISP",'I',"RECKEYVAL","  RECORD KEY is %s",reckey);
				}
				else if (eq_token(curr_node->token,KEYWORD,"RESERVE"))	/* RESERVE NN AREAS phrase		*/
				{
					write_log("WISP",'I',"RESERVENN","Copy RESERVE NN AREAS phrase.");
					curr_node = curr_node->next;
					strcpy(areas,token_data(curr_node->token));
				}

				curr_node = curr_node->next;
			}

			if (!prog_fstats[this_file][0])				/* was there no STATUS?			*/
			{

				for (i=0; i<opt_no_file_status_cnt; i++)			/* If there are files not needing status	*/
				{
					if (!strcmp(prog_files[this_file],opt_no_file_status_item[i])) break;
				}

				if (i >= opt_no_file_status_cnt)				/* It was not there.			*/
				{
					write_log("WISP",'I',"NOSTATUS",
						"No file status for file %s, WISP-FILE-STATUS used.",
						prog_files[this_file]);
					strcpy(prog_fstats[this_file],"WISP-FILE-STATUS");	/* save the name	*/
				}
			}

			if (!org) org = SEQUENTIAL;
			if (!accmod) accmod = SEQUENTIAL;

			if (opt_data_conv)
			{
				if (SEQUENTIAL==org)
				{
					prog_cnt--;
					free_statement(the_statement);
					goto get_next_statement;
				}
				accmod = DYNAMIC;
			}

			if (org == SEQUENTIAL && relkey[0]) 			/* Is it SEQUENTIAL with RELATIVE KEY	*/
			{
				write_log("WISP",'E',"SEQRELKEY", 
					  "RELATIVE KEY invalid on SEQUENTIAL file [%s], changed to ORGANIZATION RELATIVE",
					  prog_files[this_file]);
				tput_scomment("*%%WISP-I-CHANGED File [%s] ORGANIZATION changed to RELATIVE",
					  prog_files[this_file]);

				org = RELATIVE;
			}

			if (org == SEQUENTIAL && accmod == SEQUENTIAL) 
			{
				prog_ftypes[this_file] |= SEQ_FILE;
				prog_ftypes[this_file] |= SEQ_SEQ;
			}
			if (org == RELATIVE ) 
			{
				prog_ftypes[this_file] |= RELATIVE_FILE;
			}

			was_seq_dyn = 0;
			if (org == SEQUENTIAL && accmod != SEQUENTIAL && !relkey[0]) /* Is it SEQUENTIAL/DYNAMIC/no key?	*/
			{
				write_log("WISP",'W',"CHANGTOSEQ",
					  "Invalid ACCESS mode for SEQUENTIAL file [%s], changed to SEQUENTIAL access.",
					  prog_files[this_file]);
				tput_scomment("*%%WISP-I-CHANGED File [%s] ACCESS mode changed to SEQUENTIAL",
					  prog_files[this_file]);
				accmod = SEQUENTIAL;				/* Change to SEQUENTIAL.		*/
				prog_ftypes[this_file] |= SEQ_FILE;
				prog_ftypes[this_file] |= SEQ_DYN;		/* set the flag bit			*/
				was_seq_dyn = 1;
			}

			write_sel(printer,this_file,org,accmod,optional_found);		/* Write the SELECT statement		*/
											/* Now generated the statements.	*/

			if (chk_sort(prog_files[this_file]))			/* if a SORT file were done		*/
			{
				tput_fluff(the_statement->next);
				free_statement(the_statement);
				tput_clause(12, ".");				/* End it all				*/
				goto get_next_statement;
			}

			if (areas[0]) tput_line_at(16, "RESERVE      %s AREAS",areas);

											/* Write ORGANIZATION.			*/
			if (org == SEQUENTIAL && (seq_org_flag || *seqtype))			
				tput_line_at(16, "ORGANIZATION IS %sSEQUENTIAL",seqtype);
			else if (org == INDEX)
				tput_line_at(16, "ORGANIZATION IS INDEXED");
			else if (org == RELATIVE)
				tput_line_at(16, "ORGANIZATION IS RELATIVE");

											/* Write ACCESS.			*/
			if (accmod == SEQUENTIAL && seq_acc_flag)
				tput_line_at(16, "ACCESS MODE  IS SEQUENTIAL");
			else if (accmod == RANDOM)
				tput_line_at(16, "ACCESS MODE  IS RANDOM");
			else if (accmod == DYNAMIC)
				tput_line_at(16, "ACCESS MODE  IS DYNAMIC");

			if (mf_cobol)
			{
				/*
				**	MF: The LOCK clause comes before the KEY clauses.
				*/
				if (opt_manual_locking)
				{
					tput_line_at(16, "LOCK MODE MANUAL WITH LOCK ON MULTIPLE RECORDS");
				}
				else if (multiplelock)
				{
					tput_line_at(16, "LOCK MODE AUTOMATIC WITH LOCK ON MULTIPLE RECORDS");
				}
				else if (org == INDEX || was_seq_dyn || mfse_cobol)
				{
					/* 
					 * MF SE allows sharing of seq files so always need the LOCK clause.
					 */
					tput_line_at(16, "LOCK MODE AUTOMATIC");
				}
				multiplelock = 0;
			}

			if (reckey[0]) 	tput_line_at(16, "RECORD KEY   IS %s",reckey);
			if (reckeyq[0]) tput_clause (20, "OF %s",reckeyq);

			if (relkey[0])  tput_line_at(16, "RELATIVE KEY IS %s",relkey);
			if (relkeyq[0]) tput_clause (20, "OF %s",relkeyq);

			if (splitkey) 
			{
				tput_block(splitkey);
				wfree(splitkey);
				splitkey = NULL;
			}

			if (num_alt)
			{
				for (i=0; i<num_alt; i++)
				{
					tput_line_at(16, "ALTERNATE RECORD KEY IS");
					tput_clause (20, "%s",altkey[i]);
					if (altkeyq[i][0])
						tput_clause(20, "OF %s",altkeyq[i]);
					if (altsplitkey[i])
					{
						tput_block(altsplitkey[i]);
						wfree(altsplitkey[i]);
						altsplitkey[i] = NULL;
					}
					if (duplic[i])
						tput_clause(20, "WITH DUPLICATES");
				}
			}

			if (reckey[0])						/* Put it into the altkey list		*/
			{
				strcpy(altkey[num_alt++],reckey);
			}

			if (acu_cobol)
			{
				/*
				**	ACU: The LOCK clause comes after the KEY clauses.
				*/
				if (opt_manual_locking)
				{
					tput_line_at(16, "LOCK MODE MANUAL WITH LOCK ON MULTIPLE RECORDS");
				}
				else if (multiplelock)
				{
					tput_line_at(16, "LOCK MODE AUTOMATIC WITH LOCK ON MULTIPLE RECORDS");
				}
				multiplelock = 0;
			}

			if (prog_fstats[this_file][0])				/* was there STATUS?			*/
			{
				tput_line_at(16, "FILE STATUS  IS %s",prog_fstats[this_file]);
			}

			tput_fluff(the_statement->next);
			tput_clause(12, ".");					/* End it all				*/

			if ( mf_compress )					/* If MF file compression is on		*/
			{
				tput_noprocess("      $SET DATACOMPRESS\"0\"");	/* Turn it off				*/
				mf_compress = 0;
			}
		}
	}

	free_statement(the_statement);
	goto get_next_statement;
}


int key_name(char* the_name, const char* the_qual)					/* Change the name if on key_list	*/
{
	char tstr[40];
	int i;

	for( i=0; i < kl_count; i++ )							/* Search the key_list			*/
 	{
		if ( !strcmp(key_list[i].name,the_name) )				/* Is it in the list?			*/
		{
			if ( !the_qual || (the_qual && !strcmp(key_list[i].qual,the_qual) ) )
			{
				make_fld(tstr,the_name,"K-");				/* prefix it with a K-			*/
				strcpy(the_name,tstr);					/* Now put it back.			*/
				return(i);						/* Say it's so.				*/
			}
		}
	}
	return(-1);
}

static int write_sel(int printer, int this_file, int org, int accmod, int optional_found)	/* Write initial part of SELECT.	*/
{
#ifdef OLD
	if (prog_ftypes[this_file] & AUTOLOCK)
	{
		if ( !(prog_ftypes[this_file] & SEQ_SEQ) )				/* If not SEQ/SEQ then			*/
		{
			prog_ftypes[this_file] ^= AUTOLOCK;				/* XOR the AUTOLOCK bit OFF		*/
			write_log("WISP",'E',"AUTOLOCK","File %s NOT SEQ/SEQ.",prog_files[this_file]);
		}

	}
#endif


	mf_compress = 0;
	if ( compressfile && mf_cobol )							/* Turn on MF file compression		*/
	{
		tput_noprocess("      $SET DATACOMPRESS\"1\"");
		mf_compress = 1;							/* We are in a MF compress		*/
	}
											/* SEQ-SEQ are OPTIONAL.		*/
	if ( (	!nooptional &&
		!chk_sort(prog_files[this_file]) && 
		!opt_no_seq_seq_optional && 
		!printer && 
		(org == SEQUENTIAL) && 
		(accmod == SEQUENTIAL)			) || optional_found )
	{
		tput_line_at(12, "SELECT OPTIONAL %s",prog_files[this_file]);
	}
	else
	{
		tput_line_at(12, "SELECT %s",prog_files[this_file]);
	}

	nooptional = 0;									/* Reset the nooptional flag		*/

	if (acu_cobol || mf_cobol)
	{										/* If it's acucobol, write the gen name.*/
		tput_line_at(16, "ASSIGN TO %s", get_prog_nname(this_file));
		if ( compressfile && acu_cobol )
		{
			tput_clause(16, "WITH COMPRESSION");
		}
	}
	else
	{										/* write the name			*/
		tput_line_at(16, "ASSIGN TO \"%s\"",prog_prname[this_file]);
	}
	compressfile = 0;								/* Reset the flag.			*/

	if (*selecttext)
	{
		tput_block(selecttext);
		selecttext[0] = '\0';
	}
	return 0;
}

static int del_sel(char* the_file)							/* Determine if delete current SELECT.	*/
{
	int	i;

	if (!prog_dscnt) return(0);							/* none to delete.			*/

	for (i=0; i<prog_dscnt; i++)							/* Scan the list.			*/
	{
		if (!strcmp(the_file,prog_dsel[i]))					/* Compare this file with the list.	*/
		{
			write_log("WISP",'W',"DELSEL","Delete SELECT for file %s because no FD.",prog_dsel[i]);
			return(1);
		}
	}

	return(0);							/* Didn't find it.			*/
}

static int chk_sort(const char* fname)							/* Check list of SORT files.		*/
{
	int i;

	if (opt_sort_file_cnt)
	{
		for (i=0; i<opt_sort_file_cnt; i++)
		{
			if (!strcmp(fname,opt_sort_file_item[i])) return(1);		/* Found it.				*/
		}
	}

	return(0);									/* Not found.				*/
}

#ifdef NOT_USED
static int gen_io_control(void)								/* Add an I-O-CONTROL section and insert*/
											/* APPLY LOCK-HOLDING statements to do	*/
											/* manual record locking.		*/
{
	int	i;
	int	wrote_io_control;
	int	need_period;

	wrote_io_control = 0;								/* haven't wrote paragraph name		*/
	need_period = (ioc_found == 2);							/* Do we need a period			*/

	for(i=0; i<prog_cnt; i++)							/* loop thru all files			*/
	{
		if ( !(prog_ftypes[i] & AUTOLOCK) )					/* if not auto-lock files then		*/
		{
			if (!ioc_found && !wrote_io_control)				/* If not yet written then		*/
			{
				tput_line_at(8, "I-O-CONTROL.");			/* write the paragraph header		*/
				wrote_io_control = 1;
			}
											/* write the lock-holding clause	*/
			tput_line_at(12, "APPLY LOCK-HOLDING ON %s", prog_files[i]);
			need_period = 1;
		}
	}

	if (need_period)								/* if period is needed			*/
	{
		tput_clause(12,".");
	}
	return 0;
}
#endif /* NOT_USED */


/*
**	Routine:	get_record_key
**
**	Function:	To extract the record key from the input stream and return it as individual components.
**			It can handle qualified keys and split keys.
**
**				IS key-name
**				IS key-name {OF|IN} qual-name
**				IS split-key = s1 s2 s3
**
**	Arguments:
**	curr_node	- current node pointer.
**	reckey		- the record key (unqualified)
**	reckeyqual	- the qualification for reckey or NULL
**	splitkey	- pointer to malloc memory that contains a printable split key string or NULL
**
**	Return:		Modified curr_node pointing AT the last key token.
**
**	Warnings:	If splitkey is not NULL then the calling program is responsible for freeing the memory.
**
**	History:
**	05/15/92	Written by GSL
**	06/03/93	Changed to use curr_node. GSL
**
*/
static NODE get_record_key(NODE curr_node, char* reckey, char* reckeyqual, char** splitkey)
{
	char	buff[80];

	reckeyqual[0] = '\0';							/* Initialize these variables			*/
	*splitkey = NULL;

	if (eq_token(curr_node->token,KEYWORD,"IS")) curr_node = curr_node->next;

	strcpy(reckey,token_data(curr_node->token));				/* Get the record key (skipping opt_IS)		*/

	if (curr_node->next && PERIOD != curr_node->next->token->type)
	{
		if (eq_token(curr_node->next->token,KEYWORD,"OF")||		/* If next token is "OF" then is qualified	*/
		    eq_token(curr_node->next->token,KEYWORD,"IN")  )		/* If next token is "IN" then is qualified	*/
		{
			curr_node = curr_node->next;
			curr_node = curr_node->next;
			strcpy(reckeyqual,token_data(curr_node->token));	/* Get the qualification			*/
		}
		else if (eq_token(curr_node->next->token,0,"="))		/* Split key   SK = K1 K2 K3 ...		*/
		{
			curr_node = curr_node->next;
			*splitkey = wmalloc(80*6);				/* Malloc some space for split key		*/
			**splitkey = '\0';
			add2buff(*splitkey,"=",16,0);

			for(;;)
			{
				if (!curr_node->next || PERIOD==curr_node->next->token->type)
				{
					break;
				}

				strcpy(buff,token_data(curr_node->next->token));/* See if next token terminates the splitkey	*/
				if (	0==strcmp(buff,"ALTERNATE") 	||
					0==strcmp(buff,"WITH") 		||
					0==strcmp(buff,"DUPLICATES") 	||
					0==strcmp(buff,"FILE") 		||
					0==strcmp(buff,"STATUS") 	||
					isdigit((int)buff[0])		  )	/* a second alternate record key		*/
				{
					break;					/* end of split-key segment list		*/
				}
				curr_node = curr_node->next;
				add2buff(*splitkey,buff,16,0);			/* Add it to the print buffer			*/
			}
		}
	}

	return(curr_node);							/* Pointer to last node we used.		*/
}

/*
	crt_index:	Search the list of crt files and returns the position in the list, -1 == not found
*/
int crt_index(const char* name)
{
	int	i;

	for(i=0; i<crt_fcount; i++)
	{
		if (0==strcmp(crt_file[i],name)) 
		{
			return(i);							/* Found it in the list			*/
		}
	}

	return(-1);
}

/*
	file_index:	Search the list of files and returns the position in the list, -1 == not found
*/
int file_index(const char* name)
{
	int	i;

	for(i=0; i<prog_cnt; i++)
	{
		if (0==strcmp(prog_files[i],name)) 
		{
			return(i);							/* Found it in the list			*/
		}
	}

	return(-1);
}

int is_file_status(const char* dataname)
{
	int	i;

	for(i=0; i<prog_cnt; i++)
	{
		if (0==strcmp(prog_fstats[i],dataname))
		{
			return(1);
		}
	}
	return(0);
}

/*
	add2buff:	Add a string to a buffer keeping track of the last line size (from newline) and spliting the line
			if it would go over 72.  If the size is less then col it will pad out to col. The addstr does not 
			need to have spaces surrounding it, they are added automatically.
*/
static int add2buff(
	     char	*buff,					/* The buffer -- it can hold multiple lines.			*/
	     char	*addstr,				/* The string to add to buffer.					*/
	     int	col,					/* The column to start in if line is split.			*/
	     int	newline)				/* Force to a new line.						*/
{
	char	spaces[80];
	int	size, i, len;

	len = strlen(addstr);

	if (len < 1) return 0;

	for ( size = 0, i = strlen(buff)-1; i >= 0 && buff[i] != '\n'; size++, i--);

	if ( col + len > 71 )
	{
		col = 12;
	}

	if ( newline || (size + len + 1 > 71) )
	{
		strcat(buff,"\n");							/* Start a new line			*/
		memset(spaces,' ',col-1);
		spaces[col-1] = '\0';
	}
	else if ( size < col-1 )
	{
		i = col - 1 - size;							/* pad out to col			*/
		memset(spaces, ' ', i);
		spaces[i] = '\0';
	}
	else
	{
		strcpy(spaces," ");							/* A one space between tokens		*/
	}

	strcat(buff,spaces);

	strcat(buff,addstr);								/* Append the string			*/

	return 0;
}

/*
**	History:
**	$Log: wt_input.c,v $
**	Revision 1.32  2003/03/17 20:33:16  gsl
**	remove commented old code
**	
**	Revision 1.31  2003/03/06 21:43:01  gsl
**	Add RELATIVE file flag
**	
**	Revision 1.30  2003/03/03 22:08:40  gsl
**	rework the options and OPTION file handling
**	
**	Revision 1.29  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.28  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.27  2003/02/04 18:29:12  gsl
**	fix -Wall warnings
**	
**	Revision 1.26  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.25  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.24  2002/10/14 19:08:02  gsl
**	Remove the -c options as obsolete since comments are now always
**	included in the output.
**	
**	Revision 1.23  2002/09/30 21:02:00  gsl
**	update
**	
**	Revision 1.22  2002/08/13 20:20:31  gsl
**	RELATIVE KEY not on ACCESS clause
**	
**	Revision 1.21  2002/08/12 20:13:50  gsl
**	quotes and literals
**	
**	Revision 1.20  2002/06/20 23:04:30  gsl
**	remove obsolete code
**	
**	Revision 1.19  2002/03/21 23:27:08  gsl
**	Remove the simple SELECT short-cut to avoid duplicating code
**	
**	Revision 1.18  2002-03-21 17:06:55-05  gsl
**	For MFSE add LOCK MODE clause to sequential files.
**
**	Revision 1.17  1998-07-07 09:40:29-04  gsl
**	Fix typo in warning message
**
**	Revision 1.16  1998-06-09 13:12:39-04  gsl
**	Add support for manual record locking
**
**	Revision 1.15  1998-03-27 10:37:44-05  gsl
**	change_words
**
**	Revision 1.14  1998-03-26 09:27:15-05  gsl
**	Add the get_prog_xxx() values to the symbol table and to the reserved
**	words list to prevent generated names from conflicting with
**	user defined names.
**
**	Revision 1.13  1998-03-23 13:22:53-05  gsl
**	Add select name to user symbols.
**	change to use get_prog_nname()
**	change to allow IN or OF in qualified key name.
**	Make proto's const
**
**	Revision 1.12  1998-03-04 15:55:31-05  gsl
**	Truncate PRNAME to 8 characters
**
**	Revision 1.11  1998-02-10 09:46:58-05  gsl
**	Fix the SEQ-DYN/RAN and SEQ with RELATIVE key processing.
**	SEQ file with RELATIVE key is now changes to a RELATIVE file.
**	SEQ file without a RELATIVE key is changed to ACCESS SEQUENTIAL.
**
**	Revision 1.10  1996-08-30 21:56:20-04  gsl
**	drcs update
**
**
**
*/
