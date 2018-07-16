			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	wisp_wsdiv.c
*/

#define EXT extern
#include "wisp.h"
#include "keylist.h"
#include "token.h"
#include "node.h"

char *next_filler();

NODE get_statement();

NODE working_storage_section();
NODE linkage_section();

NODE parse_data_description();

/*
**	Routine:	working_storage_section()
**
**	Function:	To process the WORKING-STORAGE SECTION.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not WORKING-STORAGE SECTION then one will be generated.
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
NODE working_storage_section(the_statement)
NODE the_statement;
{
	fd_check();

	if (!comments) tput_blank();

	if (eq_token(the_statement->next->token,KEYWORD,"WORKING-STORAGE"))
	{
		write_log("WISP",'I',"WSSECT","Processing WORKING-STORAGE SECTION.");
		division = WORKING_STORAGE_SECTION;

		tput_statement(12,the_statement);
		free_statement(the_statement);
		ws_init();
	}
	else
	{
		write_log("WISP",'I',"WSSECT","WORKING-STORAGE SECTION not found.");

		tput_line_at(8, "WORKING-STORAGE SECTION.");
		ws_init();

		return(the_statement);
	}


	/*
	**	This will parse all of working storage.
	**	It will return the last statement which it couldn't recognize,
	**	this should be the "LINKAGE SECTION" or "PROCEDURE DIVISION" statement.
	*/

	the_statement = parse_data_description(NULL);

	if (eq_token(the_statement->next->token,KEYWORD,"LINKAGE") ||
	    eq_token(the_statement->next->token,KEYWORD,"PROCEDURE")  )
	{
		/*
		**	Finished WORKING-STORAGE SECTION. 
		**	Perform any end logic.
		*/

		gen_screens();

		return(the_statement);
	}
	else
	{
		write_tlog(the_statement->next->token, 
				"WISP",'F',"PARSEWS","Expecting LINKAGE SECTION, or PROCEDURE DIVISION found %s.",
				token_data(the_statement->next->token));
		exit_with_err();
	}
}

/*
**	Routine:	linkage_section()
**
**	Function:	To process the LINKAGE SECTION.
**
**	Description:	Get and put statements until out of this section then pass that statement up
**			to be handled by a higher routine.
**
**			If the first statement is not LINKAGE SECTION then this section will be skipped.
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
NODE linkage_section(the_statement)
NODE the_statement;
{

	if (eq_token(the_statement->next->token,KEYWORD,"LINKAGE"))
	{
		write_log("WISP",'I',"LINKSECT","Processing LINKAGE SECTION.");

		tput_statement(12,the_statement);
		free_statement(the_statement);
	}
	else
	{
		write_log("WISP",'I',"WSSECT","LINKAGE SECTION not found.");
		return(the_statement);
	}


	/*
	**	This will parse all of LINKAGE SECTION.
	**	It will return the last statement which it couldn't recognize,
	**	this should be the "PROCEDURE DIVISION" statement.
	*/

	the_statement = parse_data_description(NULL);

	if (eq_token(the_statement->next->token,KEYWORD,"PROCEDURE"))
	{
		return(the_statement);
	}
	else
	{
		write_tlog(the_statement->next->token, 
				"WISP",'F',"PARSELINK","Expecting PROCEDURE DIVISION found %s.",
				token_data(the_statement->next->token));
		exit_with_err();
	}
}

NODE parse_data_description(next_statement)
NODE next_statement;
{
	static	int	binary_cleanup_mode = 0;
	static	int	binary_group_level = 1;
	static	int	last_was_file_status = 0;

	NODE	the_statement;
	NODE	curr_node, temp_node;
	NODE	level_node, data_name_node, binary_node, pic_node, occurs_node, blank_node, display_ws_node;
	NODE	comp_node, redefines_node, picture_node, value_node, period_node;
	int	parse_phase;
	int	reuse;
	int	was_level77;
	int	curr_level;
	char	buff[80];

get_next_statement:
	if (next_statement)
	{
		the_statement = next_statement;					/* Use the already loaded next statement	*/
		next_statement = NULL;
	}
	else
	{
		the_statement = get_statement();				/* Load the statement				*/
	}

	was_level77 = 0;

	curr_node = the_statement->next;					/* Point to first significant token		*/

	level_node = data_name_node = binary_node = pic_node = occurs_node = blank_node = display_ws_node = NULL;
	comp_node = redefines_node = picture_node = value_node = period_node = NULL;

	parse_phase = 1;

	while (NODE_END != curr_node->type)					/* Loop until end of statement.			*/
	{
		reuse = 0;

		if ( !curr_node->token)
		{
			/* No token on this node, do nothing */
		}
		else if ( 1 == parse_phase )					/* First token must be a level number		*/
		{
			if ( NUMBER == curr_node->token->type )
			{
				if ( eq_token(curr_node->token, NUMBER, "77") )
				{
					/*
					**	Wang allows 77's anywhere in working-storage,
					**	ansi demands they occur only at the top (or bottom).
					*/
					write_log("WISP",'I',"CHNG77","Changed 77 level to 01 level.");
					edit_token(curr_node->token,"01");
					was_level77 = 1;
				}

				level_node = curr_node;
				sscanf(level_node->token->data,"%d",&curr_level);
			}
			else
			{
				/*
				**	This statement is not a Data Definition, return it so it can be processed.
				*/
				binary_cleanup_mode = 0;
				return(the_statement);
			}
			parse_phase = 2;
		}
		else if ( 2 == parse_phase )					/* Second token should be dataname or filler	*/
		{
			if ( eq_token(curr_node->token, KEYWORD, "FILLER") )
			{
				if ( (WORKING_STORAGE_SECTION==division && !wsfiller) ||
				               (DATA_DIVISION==division && !fdfiller)    )
				{
					edit_token(curr_node->token,next_filler());
				}
				data_name_node = curr_node;
			}
			else if ( IDENTIFIER == curr_node->token->type )
			{
				data_name_node = curr_node;
			}
			else							/* No data name supplied.			*/
			{
				reuse = 1;
			}
			parse_phase = 3;
		}
		else if ( eq_token(curr_node->token, KEYWORD, "DISPLAY-WS") )
		{
			display_ws_node = curr_node;
		}
		else if ( eq_token(curr_node->token, KEYWORD, "OCCURS") )
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
			edit_token(curr_node->token,packed_decimal());		/* Edit the computational clause in place	*/
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
		else if ( eq_token(curr_node->token, KEYWORD, "BLANK") )
		{
			blank_node = curr_node;
		}
		else if ( eq_token(curr_node->token, KEYWORD, "REDEFINES") )
		{
			redefines_node = curr_node;
		}
		else if ( eq_token(curr_node->token, KEYWORD, "VALUE") )
		{
			value_node = curr_node;
		}
		else if ( PERIOD == curr_node->token->type )
		{
			period_node = curr_node;
		}

		if (!reuse)
		{
			curr_node = curr_node->next;
		}
	}

	if (display_ws_node)
	{
		/*
		**	Found start of DISPLAY-WS record.
		*/
		next_statement = (NODE)parse_display_ws_record(the_statement);
		goto get_next_statement;
	}

	if (DATA_DIVISION==division && 1==curr_level && data_name_node)
	{
		add_to_record_list(data_name_node->token->data);
	}

	if (blank_node && !ws_blank)
	{
		/*
		**	Remove:	BLANK [WHEN] ZERO
		*/
		write_log("WISP",'I',"REPLBLANK","Removed BLANK WHEN ZERO in WORKING STORAGE.");

		free_token_from_node(blank_node);
		blank_node = blank_node->next;

		if (eq_token(blank_node->token,KEYWORD,"WHEN"))
		{
			free_token_from_node(blank_node);
			blank_node = blank_node->next;
		}

		if (eq_token(blank_node->token,KEYWORD,"ZERO")   ||
		    eq_token(blank_node->token,KEYWORD,"ZEROS")  ||
		    eq_token(blank_node->token,KEYWORD,"ZEROES")    )
		{
			free_token_from_node(blank_node);
		}
		else
		{
			write_log("WISP",'E',"BLANKZERO", "Error parsing BLANK WHEN ZERO clause");
		}
	}

	if (binary_node)
	{
		/*
		**	In COBOL you can specifiy a USAGE clause at the group level and have it apply to all elementary
		**	items in that group. This does NOT work for the PIC clause. We translate BINARY, which is a USAGE, into
		**	a USAGE plus a PIC.  If this was at the group level we need to fix it up.
		**
		**	WANG:	01  A-ITEM  USAGE IS BINARY.
		**		    05  B-ITEM.
		**		    05  C-ITEM  PIC S9(9).
		**		        88 C-OFF   VALUE 0;
		**		        88 C-ON    VALUE 1;
		**		    05  D-ITEM.
		**		        10  E-ITEM.
		**
		**	WISP:	01  A-ITEM  USAGE IS COMPx PIC S9(4).	<---- This is WRONG!!  Needs to be fixed up.
		**		    05  B-ITEM.
		**		    05  C-ITEM  PIC S9(9).
		**		        88 C-OFF   VALUE 0;
		**		        88 C-ON    VALUE 1;
		**		    05  D-ITEM.
		**		        10  E-ITEM.
		**
		**	FIXED:	01  A-ITEM  USAGE IS COMPx.
		**		    05  B-ITEM  PIC S9(4).		<---- Move the PIC to the elementary level.
		**		    05  C-ITEM  PIC S9(9).		<---- Already has a PIC don't add another.
		**		        88 C-OFF   VALUE 0;		<---- 88 levels don't apply
		**		        88 C-ON    VALUE 1;
		**		    05  D-ITEM.				<---- Remove from sub-group levels
		**		        10  E-ITEM PIC S9(4).
		*/
		if (pic_node)
		{
			/*
			**	There was a picture on this item so edit the binary but
			**	no additional cleanup is needed.
			*/
			edit_token(binary_node->token, bin4_type);
			if (data_name_node)
				write_log("WISP",'I',"REPLBINARY","Replaced USAGE BINARY with %s. [%s]",
										bin4_type,data_name_node->token->data);
			else
				write_log("WISP",'I',"REPLBINARY","Replaced USAGE BINARY with %s.",bin4_type);
			binary_cleanup_mode = 0;
		}
		else
		{
			/*
			**	Found a binary with no picture clause. It may need additional fixup.
			*/
			edit_token(binary_node->token, bin2_type);
			if (data_name_node)
				write_log("WISP",'I',"REPLBINARY","Replaced USAGE BINARY with %s PIC S9(4). [%s]",
									bin2_type,data_name_node->token->data);
			else
				write_log("WISP",'I',"REPLBINARY","Replaced USAGE BINARY with %s PIC S9(4).",bin2_type);

			binary_cleanup_mode = 1;
			sscanf(level_node->token->data,"%d",&binary_group_level);
		}
	}

	if (binary_cleanup_mode)
	{
		int	add_the_picture;
		int	next_level;

		add_the_picture = 0;

		if (was_level77)
		{
			curr_level = 77;
			next_level = 0;
			add_the_picture = 1;
			binary_cleanup_mode = 0;
			goto binary_add_picture;
		}

		/*
		**	We now have to look ahead to determine if further binary cleanup must be done.
		**	We need to know what is the next level number.  If the next statement is not
		**	a data definition then set next_level equal to 0.
		*/

		if (!next_statement)
		{
			next_statement = get_statement();			/* Load the next statement			*/
		}
		temp_node = next_statement->next;				/* Point to first significant token		*/
		next_level = 0;							/* Default to next level of zero		*/
		if ( temp_node->token && NUMBER == temp_node->token->type )
		{
			sscanf(temp_node->token->data,"%d",&next_level);	/* Load the next level number			*/
		}


		if (77==curr_level)
		{
			/*
			**	All 77's are elementary so add the picture and end cleanup.
			*/
			add_the_picture = 1;
			binary_cleanup_mode = 0;
		}
		else if (88==curr_level)
		{
			/*
			**	Never add a picture to an 88.
			*/
			add_the_picture = 0;
		}
		else if (77==next_level)
		{
			/*
			**	If next is a 77 then current must be elementary so add picture and end cleanup.
			*/
			add_the_picture = 1;
			binary_cleanup_mode = 0;
		}
		else if (88==next_level)
		{
			/*
			**	If next level is 88 then current must be elementary,
			**	so add the the picture clause to current statement.
			*/
			add_the_picture = 1;
		}
		else if (next_level > curr_level)
		{
			/*
			**	Next statement has a higher level number so this the
			**	current statement is a group.
			*/
			add_the_picture = 0;
		}
		else if (next_level <= curr_level)
		{
			/*
			**	Next level is lower so this must be an elementary item.
			*/
			add_the_picture = 1;
		}

		if (next_level <= binary_group_level)
		{
			/*
			**	Check if ready to end binary cleanup mode.
			*/
			binary_cleanup_mode = 0;
		}

binary_add_picture:
		if (!pic_node && add_the_picture)
		{
			/*
			**	Add the picture clause PIC S9(4).
			**	First figure out where in the statement to add it.
			*/

			if (binary_node)
			{
				/*
				**	Trivial case, this statement had the BINARY on it.
				**	Add it after the BINARY.
				*/
				temp_node = binary_node;
			}
			else
			{
				/* 
				**	Scan thru the statement until at node before the period.
				*/
				temp_node = level_node;
				while(temp_node->next && temp_node->next->token && PERIOD != temp_node->next->token->type)
				{
					temp_node = temp_node->next;
				}
			}
			pic_node = maketoknode(make_token(KEYWORD,"PIC"));
			tie_next(tie_next(temp_node,pic_node), maketoknode(make_token(PICTURE,"S9(4)")));
		}

	}

	if (occurs_node && data_name_node)
	{
		/*
		**	If there was an OCCURS clause save the dataname for later use as a replacement
		**	for *$RANGE_COUNT directives.
		*/
		temp_node = occurs_node->next;
		if (eq_token(temp_node->next->token,KEYWORD,"TO"))
		{
			/*
			**	Have a more complex OCCURS clause.
			**
			**		OCCURS Num TO Num times DEPENDING on Data_item ...
			*/
			temp_node = temp_node->next->next->next;
			if (eq_token(temp_node->token,KEYWORD,"TIMES"))
			{
				temp_node = temp_node->next;
			}
			temp_node = temp_node->next;
			if (eq_token(temp_node->token,KEYWORD,"ON"))
			{
				temp_node = temp_node->next;
			}
		}
		got_occurs(data_name_node->token->data,temp_node->token->data);
	}

	if (last_was_file_status)
	{
		if (88==curr_level && data_name_node)
		{
			/*
			**	This handles correction of VALUE clauses on 88 levels of FILE STATUSes.
			**
			**	01  MY-FILE-STATUS   PIC 9(2).
			**	    88  MY-FILE-OK    VALUE IS 0.    -->   VALUE IS "00".
			**          88  MY-FILE-EOF   VALUE IS 10.   -->   VALUE IS "10".
			**	    88  MY-FILE-INVALID VALUES 11 THRU 19, 23, 17, 45.
			**
			**	01  MY-FILE-STATUS   PIC X.
			**	    88  MY-FILE-OK   VALUE IS "0".   -->   VALUE IS "00".
			*/

			temp_node = data_name_node->next;
			while (temp_node != period_node)
			{
				if (NUMBER==temp_node->token->type)
				{
					int	value;
					value = 0;
					sscanf(temp_node->token->data,"%d",&value);
					sprintf(buff,"\"%02d\"",value);
					edit_token(temp_node->token,buff);
				}
				else if (LITERAL==temp_node->token->type)
				{
					int	value;
					if (1 == sscanf(temp_node->token->data,"\"%d\"",&value))
					{
						sprintf(buff,"\"%02d\"",value);
						edit_token(temp_node->token,buff);
					}
				}
				temp_node = temp_node->next;
			}
		}
		else
		{
			last_was_file_status = 0;
		}
	}

	if (DATA_DIVISION!=division && data_name_node && picture_node)
	{
		if (is_file_status(token_data(data_name_node->token)))
		{
			int	change_fs;

			/*
			**	This corrects file status codes picture clauses.
			**	Wang COBOL often has "PIC 99" for file status codes these are changed to "PIC XX"
			**
			**		PIC 99 VALUE IS 0.	->	PIC XX VALUE IS "00".
			**		PIC 9.			->	PIC XX.
			**		PIC X  VALUE IS "0".	->	PIC XX VALUE IS "00".
			*/

			last_was_file_status = 1;

			strcpy(buff,picture_node->token->data);

			if(	0==strcmp(buff,"XX")	||
				0==strcmp(buff,"X(2)") 	||
				0==strcmp(buff,"X(02)") ||
				0==strcmp(buff,"X(002)")  )
			{
				/* File status picture is OK, don't change it. */
				change_fs = 0;
			}
			else if (	
				0==strcmp(buff,"9") 	|| 
				0==strcmp(buff,"9(1)") 	||
				0==strcmp(buff,"9(01)") ||
				0==strcmp(buff,"9(001)") || 
				0==strcmp(buff,"X") 	||
				0==strcmp(buff,"X(1)") 	||
				0==strcmp(buff,"X(01)") ||
				0==strcmp(buff,"X(001)")  )
			{
				write_tlog(picture_node->token, "WISP",'E',"FILESTAT",
					"Found one byte FILE STATUS [%s], changed PIC %s to XX.",
					token_data(data_name_node->token), buff);
				change_fs = 1;
			}
			else /* if('9'==buff[0]) */
			{
				write_tlog(picture_node->token, "WISP",'W',"FILESTAT",
					"Changed FILE STATUS [%s] PIC %s to XX.", token_data(data_name_node->token), buff);
				change_fs = 2;
			}

			if (change_fs)
			{
				edit_token(picture_node->token,"XX");
			}

			if (value_node)
			{
				temp_node = value_node->next;
				if (eq_token(temp_node->token,KEYWORD,"IS"))
				{
					temp_node = temp_node->next;
				}

				if (NUMBER==temp_node->token->type)
				{
					int	value;
					value = 0;
					sscanf(temp_node->token->data,"%d",&value);
					sprintf(buff,"\"%02d\"",value);
					edit_token(temp_node->token,buff);
				}
				else if (LITERAL==temp_node->token->type)
				{
					int	value;
					if (1 == sscanf(temp_node->token->data,"\"%d\"",&value))
					{
						sprintf(buff,"\"%02d\"",value);
						edit_token(temp_node->token,buff);
					}
				}
			}
		}
	}

	if (kl_count && redefines_node)
	{
		char	newkey[40];
		int	kidx;

		strcpy(newkey, redefines_node->next->token->data);
		kidx = key_name(newkey, 0);
		if (kidx != -1)
		{
			/*
			**	Found a REDEFINES of a key that needs to be translated.
			**
			**		05  MY-KEY   PIC 999 COMP.
			**      OLD	05  XXX  REDEFINES MY-KEY ....
			**
			**	FIX	05  XXX  REDEFINES K-MY-KEY ...
			*/
			edit_token(redefines_node->next->token,newkey);
			write_log("WISP",'I',"KEY","Fixed KEY %s in a REDEFINES",newkey);
		}
	}

	if (kl_count && data_name_node)
	{
		char	oldkey[40],newkey[40];
		int	kidx;
		TOKEN	*tokptr;
		char	level_str[8];

		strcpy(oldkey, data_name_node->token->data);
		strcpy(newkey,oldkey);
		kidx = key_name(newkey, 0);
		if (kidx != -1)
		{
			tokptr = make_token(SCOMMENT,"      *%WISP-I-KEYMOD Key field modified by WISP");
			tokptr->column = 1;
			tokptr->column_fixed = 1;
			tie_down(the_statement,maketoknode(tokptr));

			if (KL_COMP == key_list[kidx].type)
			{
				/*
				**	Fix COMPUTATIONAL keys.
				**
				**	OLD:	05  FILE-A-KEY       PIC S9(8) COMP.
				**
				**	FIX:	05  K-FILE-A-KEY.
				**		  06  FILE-A-KEY     PIC S9(8) COMP-3.
				*/

				/*
				**	Going to add 3 tokens after the level number.
				*/
				level_node->token->column_fixed = 1;

				/*	Add translated key name */
				tokptr = edit_token(dup_token(data_name_node->token),newkey);
				tie_bottom(level_node,maketoknode(tokptr));

				/*	Add the period */
				tie_bottom(level_node,maketoknode(make_token(PERIOD,".")));

				/*	Add new level */
				sprintf(level_str,"%02d",curr_level+1);
				tokptr = edit_token(dup_token(level_node->token),level_str);
				tokptr->column = level_node->token->column + 2;
				tie_bottom(level_node,maketoknode(tokptr));

				write_log("WISP",'I',"KEY","Translated COMP KEY %s",newkey);
			}
			else if (KL_LEAD == key_list[kidx].type && pic_node)
			{
				char	old_pic[40], new_pic[40], filler_pic[40];
				int	picture_size, filler_size;

				/*
				**	Fix LEADING SEPARATE keys
				**
				**	OLD:	05  FILE-A-KEY      PIC S99V99 LEADING SEPARATE VALUE 0.
				**
				**	FIX:	05  K-FILE-A-KEY    PIC S9(4)  COMP-3 VALUE 0.
				**		05  FILE-A-KEY  REDEFINES
				**		    K-FILE-A-KEY    PIC S99V99 COMP-3.
				**		05  FILLER          PIC X(2).
				*/

				/*	First remove LEADING SEPARATE */
				curr_node = the_statement->next;
				while(NODE_END != curr_node->type)
				{
					if ( !curr_node->token ) { /* do nothing */ }
					else if ( eq_token(curr_node->token, KEYWORD, "LEADING") ||
					          eq_token(curr_node->token, KEYWORD, "SEPARATE")  )
					{
						free_token_from_node(curr_node);
					}
					curr_node = curr_node->next;
				}

				/*	Construct new picture and filler clause values. */
				strcpy(old_pic,picture_node->token->data);
				picture_size = pic_size(old_pic);
				filler_size = picture_size - ((picture_size + 2) / 2);
				if ('S' == toupper(old_pic[0]))	
				{
					sprintf(new_pic,"S9(%d)",picture_size);
					filler_size++;
				}
				else
				{
					sprintf(new_pic,"9(%d)",picture_size);
				}
				sprintf(filler_pic,"X(%d)",filler_size);

				/*	Edit the new key */
				level_node->token->column_fixed = 1;
				temp_node = maketoknode(dup_token(data_name_node->token));
				edit_token(data_name_node->token,newkey);
				edit_token(picture_node->token,new_pic);
				tie_next(picture_node,maketoknode(make_token(KEYWORD,packdec)));

				/*	Hang remaining onto the period->down list */

				tie_bottom(period_node, maketoknode(dup_token(level_node->token)));
				tie_bottom(period_node, temp_node);
				tie_bottom(period_node, maketoknode(make_token(KEYWORD,"REDEFINES")));
				tokptr = dup_token(data_name_node->token);
				tokptr->column_fixed = 1;
				tie_bottom(period_node, maketoknode(tokptr));
				tie_bottom(period_node, maketoknode(dup_token(pic_node->token)));
				tie_bottom(period_node, maketoknode(make_token(PICTURE,old_pic)));
				tie_bottom(period_node, maketoknode(make_token(KEYWORD,packdec)));
				tie_bottom(period_node, maketoknode(make_token(PERIOD,".")));

				tie_bottom(period_node, maketoknode(dup_token(level_node->token)));
				tie_bottom(period_node, maketoknode(make_token(KEYWORD,"FILLER")));
				tie_bottom(period_node, maketoknode(dup_token(pic_node->token)));
				tie_bottom(period_node, maketoknode(make_token(PICTURE,filler_pic)));
				tie_bottom(period_node, maketoknode(make_token(PERIOD,".")));
				write_log("WISP",'I',"KEY","Translated LEAD KEY %s",newkey);
			}
		}
	}

	tput_statement(12,the_statement);
	free_statement(the_statement);

	goto get_next_statement;
}

char *next_filler()
{
	static char 	chr1 = '0';
	static char 	chr2 = '0';
	static char 	filler[] = "WISPxx";

	filler[4] = chr1;
	filler[5] = chr2;

	if (++chr2 > 'Z')								/* increment lower byte.		*/
	{
		chr2 = '0';								/* set to a zero.			*/

		if (++chr1 > 'Z') chr1 = '0';						/* Increment the tens digit.		*/
		if (chr1 == ':') chr1 = 'A';						/* Use alphabetic.			*/
	}
	else if (chr2 == ':') chr2 = 'A';						/* Use alphabetic			*/

	return(filler);
}
