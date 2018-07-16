			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/
/*
**	File:		wt_scrn.c
**
**	Purpose:	DISPLAY-WS screen handling routines
**
**	Routines:	
**	parse_display_ws_record()
**	gen_screens()
**	gen_screens22()
**	item_fac()
**	gen_mwconv()
**	scrn_para()
**	move_source()
**	move_object()
**	gen_move()
**	chk_range()
**	gen_range()
**	p_occurs()
**	item_blank()
**	clear_item()
**	gen_label()
**	find_item()
**
**
**	History:
**
*/

/*
**	wisp_scrn.c
*/

#include <string.h>

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "token.h"
#include "node.h"
#include "directiv.h"
#include "wmalloc.h"

#include "wcommon.h"

NODE get_statement();

char	decimal_is = '.';

static item_record *occurs_item = { 0 };						/* A ptr to the first item in a group	*/

static int cur_v1 = 0;									/* the current vertical 1 occurs count	*/
static int cur_v2 = 0;									/* the current vertical 2 occurs count	*/
static int cur_h  = 0;									/* the current horizontal occurs count	*/

static int gen_screens22();
static int move_source();
static int move_object();
static int chk_range();
static int p_occurs();
static int item_blank();
static int clear_item();
static int item_fac();
static int gen_move();
static int gen_range();

/*
**	Routine:	parse_display_ws_record()
**
**	Function:	To parse a USAGE IS DISPLAY-WS record.
**
**	Description:	To ...
**
**	Arguments:
**	next_statement	The next statement to use (already loaded).
**
**	Globals:
**
**	Return:		The next statement after the DISPLAY-WS record.
**
**	Warnings:	None
**
**	History:	
**	05/24/93	Re-written to use tokenize. GSL
**
*/
NODE parse_display_ws_record(next_statement)
NODE next_statement;
{
	static  int 	fillernum = 0;							/* Current number to append to FILLER	*/

	int	curr_level;
	int	cur_v1_level;
	int	cur_v2_level;
	int 	highest_sub_level;							/* the level number of screen items	*/

	NODE	the_statement;
	NODE	curr_node, temp_node;
	NODE	level_node, data_name_node, column_node, row_node, pic_node, picture_node, range_node;
	NODE	object_node, value_node, source_node, occurs_node, period_node;

	int	first_time;

	first_time = 1;

	for(;;)
	{
		range_count = 0;						/* set current range count to 0.		*/

		if (next_statement)
		{
			the_statement = next_statement;				/* Use the already loaded next statement	*/
			next_statement = NULL;
		}
		else
		{
			the_statement = get_statement();			/* Load the statement				*/
		}

		level_node = data_name_node = column_node = row_node = pic_node = picture_node = range_node = NULL;
		object_node = value_node = source_node = occurs_node = period_node = NULL;

		curr_node = the_statement->next;				/* Point to first significant token		*/

		if ( NUMBER == curr_node->token->type )
		{
			level_node = curr_node;
			curr_level = 0;
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

		if (first_time && 1 != curr_level)
		{
			write_log("WISP",'F',"DISPLAY-WS","Unable to parse DISPLAY-WS record, expecting level 01 got %02d",
				curr_level);
			exit_with_err();
		}


		if (0==curr_level || 1==curr_level || curr_level>49)
		{
			if (!first_time)
			{
				/*
				**	Finished parsing a DISPLAY-WS record.
				*/
											/* See if there was a group OCCURS.	*/
				if (occurs_item) p_occurs(this_item);			/* If there was, process it up to the	*/
											/* Last item.				*/

				return(the_statement);
			}
			first_time = 0;

			tput_fluff(the_statement->down);

			write_log("WISP",'I',"PROCSCREEN","Processing screen %s",data_name_node->token->data);

			if (num_screens > MAX_SCREENS)				/* do we have room for more?		*/
			{
				write_log("WISP",'F',"NOSCRNROOM","Fatal Error -- maximum number of screens exceeded.");
				exit_wisp(EXIT_WITH_ERR);
			}

			strcpy(scrn_name[num_screens],data_name_node->token->data);	/* Save the name for this screen	*/
			screen_item[num_screens] = NULL;
			num_screens++;							/* Count this screen			*/

			highest_sub_level = 0;
		}
		else
		{
			item_record 	*temp_ptr;

			/*
			**	This section is done for all sub levels (not for 01 level)
			*/

			temp_ptr = (item_record *) wmalloc(sizeof(item_record));	/* Get a ptr to a new item record	*/
			clear_item(temp_ptr);						/* Init the fields in the item		*/

			if (0==highest_sub_level)
			{
				/* 
				**	First time into sub-levels
				*/
				highest_sub_level = curr_level;				/* Mark the highest sub level		*/

				screen_item[num_screens-1] = temp_ptr;			/* Add first item to chain		*/
				last_item = temp_ptr;

			}
			else
			{
				last_item = this_item;					/* Save this item ptr			*/
				this_item->next_item = temp_ptr;			/* Chain another item on.		*/
			}

			this_item = temp_ptr;						/* Point to new item.			*/

			if (curr_level < highest_sub_level)
			{
				/*
				**	This is to catch mis-matched level numbers
				**
				**		01  aaa DISPLAY-WS.
				**		    05 ....		<- highest_sub_level
				**		    03 ....
				*/
				write_log("WISP",'E',"SUBLEVEL","Error parsing DISPLAY-WS LEVEL number %02d less then %02d",
						curr_level,highest_sub_level);
				highest_sub_level = curr_level;
			}

			if (curr_level == highest_sub_level)
			{
											/* See if there was a group OCCURS.	*/
				if (occurs_item) p_occurs(last_item);			/* If there was, process it up to the	*/
											/* Last item.				*/
				cur_v1 = 0;
				cur_v2 = 0;
				cur_h  = 0;
				cur_v1_level = 0;
				cur_v2_level = 0;
			}
			else
			{
				if (curr_level <= cur_v1_level)
				{
					cur_v1 = 0;
					cur_v2 = 0;
					cur_v1_level = 0;
					cur_v2_level = 0;
				}
				if (curr_level <= cur_v2_level)
				{
					cur_v2 = 0;
					cur_v2_level = 0;
				}
			}

			clear_item(this_item);						/* Init the fields in the item		*/

			strcpy(this_item->item_num,level_node->token->data);		/* Parse out the item_number		*/
			if (data_name_node)
			{
				strcpy(this_item->name,data_name_node->token->data);	/* get the variable name		*/
			}
			else
			{
				strcpy(this_item->name,"FILLER");			/* If no data name use FILLER		*/
			}
			this_item->x_level = curr_level;				/* get the level number of this item	*/

			this_item->col = last_item->col  + pic_size(last_item->pic) + 1;/* initially set the column to 1 + size	*/
			this_item->row = last_item->row;				/* initially set the row to last row	*/


			while (NODE_END != curr_node->type)			/* Loop until end of statement.			*/
			{
				if (      eq_token(curr_node->token, KEYWORD, "COL") ||
				          eq_token(curr_node->token, KEYWORD, "COLUMN") )
				{
					column_node = curr_node;
				}
				else if ( eq_token(curr_node->token, KEYWORD, "ROW") ||
				          eq_token(curr_node->token, KEYWORD, "LINE") )
				{
					row_node = curr_node;
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
				else if ( eq_token(curr_node->token, KEYWORD, "RANGE") )
				{
					range_node = curr_node;
				}
				else if ( eq_token(curr_node->token, KEYWORD, "OBJECT") )
				{
					object_node = curr_node;
				}
				else if ( eq_token(curr_node->token, KEYWORD, "VALUE") )
				{
					value_node = curr_node;
				}
				else if ( eq_token(curr_node->token, KEYWORD, "SOURCE") )
				{
					source_node = curr_node;
				}
				else if ( eq_token(curr_node->token, KEYWORD, "OCCURS") )
				{
					occurs_node = curr_node;
				}
				else if ( PERIOD == curr_node->token->type )
				{
					period_node = curr_node;
				}

				curr_node = curr_node->next;
			}

			if (picture_node)
			{
				strcpy(this_item->pic,picture_node->token->data);
			}

			if (row_node)						/* "ROW/LINE [NUMBER IS] number"		*/
			{
				temp_node = row_node->next;
				if ( NUMBER != temp_node->token->type)
					temp_node = temp_node->next;
				if ( NUMBER != temp_node->token->type)
					temp_node = temp_node->next;
				if ( NUMBER == temp_node->token->type)
				{
					int	num;
					sscanf(temp_node->token->data,"%d",&num);
					this_item->row   = num;
					this_item->x_row = num;
				}
				else
				{
					write_log("WISP",'E',"Error parsing ROW number, using previous value");
				}
			}

			if (column_node)					/* "COLUMN [NUMBER IS] number"			*/
			{
				temp_node = column_node->next;
				if ( NUMBER != temp_node->token->type)
					temp_node = temp_node->next;
				if ( NUMBER != temp_node->token->type)
					temp_node = temp_node->next;
				if ( NUMBER == temp_node->token->type)
				{
					int	num;
					sscanf(temp_node->token->data,"%d",&num);
					this_item->col   = num;
					this_item->x_col = num;
				}
				else
				{
					write_log("WISP",'E',"Error parsing COLUMN number, using previous value");
				}
			}

			if (range_node && !object_node)
			{
				write_log("WISP",'W',"RANGE","RANGE clause found with no OBJECT clause. RANGE ignored. [%s]",
					this_item->name);
				range_node = NULL;
			}

			if (range_node)
			{
				/*
				**	RANGE is NEGATIVE		|
				**	RANGE is POSITIVE		|	Store in the low range data area
				**	RANGE is Table-name		|
				**	RANGE is from Low to High
				*/

				this_item->num_range = range_count;			/* use current range count		*/

				temp_node = range_node->next;
				if ( eq_token(temp_node->token,KEYWORD,"IS"))		/* skip over IS				*/
					temp_node = temp_node->next;

				if ( eq_token(temp_node->token,KEYWORD,"FROM"))		/* skip over FROM			*/
					temp_node = temp_node->next;

				if (!reduce_data_item(temp_node))			/* Reduce the low range data item	*/
				{
					write_log("WISP",'E',"RANGE","Error parsing RANGE clause, unrecognized data item.[%s]",
							token_data(temp_node->token));
					reduce_one(temp_node);
				}
				strip_statement(temp_node->down);
				this_item->lo_range_clause = temp_node->down;		/* Get the lo range			*/
				temp_node->down = NULL;

				write_log("WISP",'I',"RANGEVALLOW", "RANGE for item %s, Low value is %s.",
						this_item->name,token_data(this_item->lo_range_clause->token));

				temp_node = temp_node->next;
				if ( eq_token(temp_node->token,KEYWORD,"TO"))		/* If TO, skip it, get hi range		*/
				{
					temp_node = temp_node->next;

					if (!reduce_data_item(temp_node))		/* Reduce the low range data item	*/
					{
						write_log("WISP",'E',"RANGE",
								"Error parsing RANGE clause, unrecognized data item.[%s]",
								token_data(temp_node->token));
						reduce_one(temp_node);
					}
					strip_statement(temp_node->down);
					this_item->hi_range_clause = temp_node->down;	/* Get the hi range			*/
					temp_node->down = NULL;

					write_log("WISP",'I',"RANGEVALHI","High value is %s.",
						token_data(this_item->hi_range_clause->token));
				}
			}

			if (source_node || value_node)
			{
				if (source_node && value_node)
				{
					write_log("WISP",'E',"SOURCE","Both SOURCE and VALUE specified for %s, using SOURCE",
						this_item->name);
				}

				temp_node = (source_node) ? source_node : value_node;
				temp_node = temp_node->next;
				if ( eq_token(temp_node->token,KEYWORD,"IS"))		/* skip over IS				*/
					temp_node = temp_node->next;

				if (!reduce_data_item(temp_node))			/* Reduce the source data item		*/
				{
					write_log("WISP",'E',"SOURCE","Error parsing SOURCE clause, unrecognized data item.[%s]",
							token_data(temp_node->token));
					reduce_one(temp_node);
				}
				strip_statement(temp_node->down);
				this_item->source_clause = temp_node->down;
				temp_node->down = NULL;
			}

									/* If it is a filler  convert it into a variable of the	*/
									/* form SCREEN-FILLER-NNNN if the following is true:	*/
									/* It has an OBJECT, or it has a non-literal source	*/

			if ( 0==strcmp(this_item->name,"FILLER") &&
			     ( object_node || (this_item->source_clause && LITERAL != this_item->source_clause->token->type)))
			{
				sprintf(this_item->name,"SCREEN-FILLER-%d",fillernum++);
				write_log("WISP",'I',"CONVFILLER","Converted FILLER to %s.",this_item->name);
			}

			if (object_node)
			{
				temp_node = object_node->next;
				if ( eq_token(temp_node->token,KEYWORD,"IS"))		/* skip over IS				*/
					temp_node = temp_node->next;

				if (!reduce_data_item(temp_node))			/* Reduce the object data item		*/
				{
					write_log("WISP",'E',"OBJECT","Error parsing OBJECT clause, unrecognized data item.[%s]",
							token_data(temp_node->token));
					reduce_one(temp_node);
				}
				strip_statement(temp_node->down);
				this_item->object_clause = temp_node->down;
				temp_node->down = NULL;
			}

			if (occurs_node)
			{
				int 	occurs_val;

				sscanf(occurs_node->next->token->data,"%d",&occurs_val);
				this_item->x_occurs = occurs_val;
				if (!occurs_item) occurs_item = this_item;		/* Save ptr to this item if first.	*/

				if (this_item->pic[0])					/* there was a PIC, horizontal OCCURS	*/
				{
					this_item->horiz = occurs_val;			/* save it				*/
				}
				else if (cur_v1)					/* already a vertical value		*/
				{
					cur_v2 = occurs_val;				/* must be the second one		*/
					cur_v2_level = curr_level;
					this_item->vert_1 = -1;				/* This is really a group item, not used*/
				}
				else
				{							/* it's the first vertical value	*/
					cur_v1 = occurs_val;
					cur_v1_level = curr_level;
					this_item->vert_1 = -1;				/* This is really a group item, not used*/
				}
			}
			else if (!this_item->pic[0])
			{
				this_item->vert_1 = -1;					/* No OCCURS and no PIC means group item.*/
			}

			if (!picture_node && !occurs_node && !row_node && !column_node && 
			    !object_node && !source_node && !value_node)
			{
				/*
				**	This item has only a LEVEL and a NAME.
				**	Previously we would delete this item from the chain (re-use the item).
				*/
			}

		} 	/* Process a sub-level */

		tput_fluff(the_statement->next);
		free_statement(the_statement);

	}
}



/* 		Generate the WORKING STORAGE entrys for all the screens.  Called when PROCEDURE DIVISION is detected		*/

static int scrns_done = 0;								/* flag to say we did it		*/

int gen_screens()
{
	if (data_conv) return(0);
	
	gen_screens22();
	return(0);
}

static gen_screens22()
{
	int 	i;
	char	s_level[5], s_occurs[5], s_row[5], s_col[5], s_pic[50], s_value[80]; 


	if (scrns_done) return(0);							/* already did it			*/
	scrns_done = 1;
	if (!num_screens) return(0);							/* no screens.				*/

	write_log("WISP",'I',"BEGINSCRN","Begin generation of screen records.");

	for (i=0; i<num_screens; i++)					
	{										/* Output a corrected screen record	*/
		write_log("WISP",'I',"DOSCREEN","Generating screen number %d.",i);
											/* First output an order area for it.	*/
		tput_blank();
		tput_scomment("******* Definition of screen %s *******", scrn_name[i]);

		make_oa(templine,scrn_name[i]);
		tput_line_at(8,  "01  %s.",templine);
		tput_line_at(12, "05  FILLER         PIC X(1) VALUE DEC-BYTE-1.");
		tput_line_at(12, "05  FILLER         PIC X(1) VALUE WISP-SCWCC.");
		tput_line_at(12, "05  FILLER         PIC X(1) VALUE DEC-BYTE-0.");
		tput_line_at(12, "05  FILLER         PIC X(1) VALUE DEC-BYTE-0.");

											/* Now the structure describing it	*/
		tput_line_at(8,  "01  %s.",scrn_name[i]);				/* output the screen name		*/


		this_item = screen_item[i];						/* Get ptr to first item		*/
		last_item = this_item;

		tput_line_at(12,"05  WISP-SCREEN-VERSION PIC X VALUE DEC-BYTE-%d.",SCREEN_VERSION);
		tput_line_at(12,"05  FILLER PIC X(8)  VALUE \"%-8s\".",prog_id);
		tput_line_at(12,"05  FILLER PIC X(32) VALUE");
		tput_line_at(33,"\"%-32s\".",scrn_name[i]);
		tput_line_at(12,"05  STATIC-DYNAMIC PIC X     VALUE \"S\".");
		tput_line_at(12,"05  DECIMAL-IS     PIC X     VALUE \"%c\".",decimal_is);
		tput_line_at(12,"05  RESERVED-SPACE PIC X(9)  VALUE SPACES.");

		do									/* loop for each item in the screen	*/
		{
			if (!compress)
			{
				tput_scomment("*** Field %s of %s ***",this_item->name, scrn_name[i]);
			}

			s_level[0] 	= '\0';
			s_row[0] 	= '\0';
			s_col[0] 	= '\0';
			s_occurs[0] 	= '\0';
			s_pic[0] 	= '\0';

			sprintf(s_level,"L%d",this_item->x_level);
			if ( this_item->x_row    ) sprintf(s_row,    "R%d",   this_item->x_row);
			if ( this_item->x_col    ) sprintf(s_col,    "C%d",   this_item->x_col);
			if ( this_item->x_occurs ) sprintf(s_occurs, "O%d",   this_item->x_occurs);
			if ( this_item->pic[0]   ) sprintf(s_pic,    "P{%s}", this_item->pic);

			if (compress)							/* Compress the output if they ask.	*/
			{
				sprintf(s_value,"\"%s%s%s%s%s;\"",s_level,s_row,s_col,s_occurs,s_pic);

				tput_line_at(12, "05  FILLER PIC X(%d) VALUE",strlen(s_value)-2);
				tput_clause (16, "%s.", s_value);
			}
			else								/* Otherwise, be verbose.		*/
			{
				tput_line_at        (12, "05  FILLER.");
				tput_line_at        (16, "10  DISP-LEVEL  PIC X(%d) VALUE \"%s\".",strlen(s_level),s_level);
				if ( this_item->x_row    )
					tput_line_at(16, "10  DISP-ROW    PIC X(%d) VALUE \"%s\".",strlen(s_row),s_row);
				if ( this_item->x_col    )
					tput_line_at(16, "10  DISP-COL    PIC X(%d) VALUE \"%s\".",strlen(s_col),s_col);
				if ( this_item->x_occurs )
					tput_line_at(16, "10  DISP-OCCURS PIC X(%d) VALUE \"%s\".",strlen(s_occurs),s_occurs);
				if ( this_item->pic[0]   )
				{
					tput_line_at(16, "10  DISP-PIC    PIC X(%d)",strlen(s_pic));
					tput_clause (20, "VALUE \"%s\".",s_pic);
				}
				tput_line_at        (16, "10  DISP-END    PIC X    VALUE \";\"."); 

			}

			if ( this_item->pic[0]   )
				item_fac();						/* generate the FAC information		*/

			last_item = this_item;
			this_item = this_item->next_item;				/* And point to the next item		*/

		} while (this_item);							/* 'till there are no more...		*/


/* -----------------------------	Write a hex FF to signal this is the last item		------------------------------	*/

		tput_line_at  (12, "05  DISP-END-SCREEN PIC X VALUE \".\".");
	}
	write_log("WISP",'I',"SCREENDONE","Finished generating screen records.");
}


/* ------------------------ 		Write the FAC for this item into the record		-------------------------------	*/

static char *fac_name[] = { 	"WFAC-PROTECT",
				"WFAC-MODIFY"   };
static item_fac()
{
	int i,j,k,m;
	int	fac_idx;
	char the_level[32];
	char item_str[256];

	static int facnum = 0;								/* the number of the last fac done	*/

/* 				Initialize the FAC's according to their field types						*/

	if (this_item->vert_1 || this_item->vert_2 || this_item->horiz)			/* if the item is a multiple item...	*/
	{
		facnum++;								/* need a new fac number		*/
											/* write the fac info			*/

		item_str[0] = '\0';							/* init item string			*/

		if (this_item->object_clause)					/* if the object is null, protect the field	*/
		{
			k = pic_fac(this_item->pic);				/* not null, generate a fac			*/
			if ( 0x81 == k ) fac_idx = 1;
			else 		 fac_idx = 0;
		}
		else
		{
			k = 0x8C;						/* generate a protected field FAC		*/
			fac_idx = 0;
		}

		m  = (this_item->vert_1 == 0) ? 1 : this_item->vert_1;		/* get vert count				*/
		m *= (this_item->vert_2 == 0) ? 1 : this_item->vert_2;		/* times the vert 2 count			*/
		m *= (this_item->horiz  == 0) ? 1 : this_item->horiz;		/* times the horizontal count			*/

										/* now write the fac memory and the item memory	*/
		tput_line_at(12, "05  FAC-NUMBER-%d.",facnum);

		tput_line_at(16, "10  FILLER PIC X(%d) VALUE ALL %s.",m,fac_name[fac_idx]);
		tput_line_at(16, "10  FILLER PIC X(%d).",m * pic_size(this_item->pic));
										/* compute the dimensions			*/
		if (this_item->vert_1)						/* has a vertical dimension			*/
		{

			tput_line                       ("           05  FILLER REDEFINES FAC-NUMBER-%d.\n",facnum);
											/* write the first vertical dimension	*/
			sprintf(templine,                "             07  FILLER OCCURS %d TIMES.\n", this_item->vert_1);
			tput_line("%s", templine);
			strcat(item_str,templine);					/* build up the info for the item too	*/

			if (this_item->vert_2)
			{								/* write the second vertical dimension	*/
				sprintf(templine,        "               10  FILLER OCCURS %d TIMES.\n",this_item->vert_2);
				tput_line("%s", templine);
				strcat(item_str,templine);				/* build up the info for the item too	*/

				if (this_item->horiz)					/* and the horizontal count (3D)	*/
				{
					sprintf(templine,"                 15  FILLER OCCURS %d TIMES.\n",this_item->horiz);
					tput_line("%s", templine);
					strcat(item_str,templine);			/* build up the info for the item too	*/
					strcpy(the_level,"        20");
				}
				else
				{
					strcpy(the_level,"      15");
				}
			}
			else if (this_item->horiz)
			{								/* write horizontal dimension		*/
				sprintf(templine,        "               10  FILLER OCCURS %d TIMES.\n",this_item->horiz);
				tput_line("%s", templine);
				strcat(item_str,templine);				/* build up the info for the item too	*/
				strcpy(the_level,        "      15");
			}
			else
			{
				strcpy(the_level,        "    10");			/* only 1 dimension			*/
			}
		}
		else
		{									/* horizontal dimension only		*/

			tput_line(       "           05  FILLER REDEFINES FAC-NUMBER-%d.\n",facnum);
											/* write the horizontal dimension	*/
			sprintf(templine,"           07  FILLER OCCURS %d TIMES.\n", this_item->horiz);
			tput_line("%s", templine);
			strcat(item_str,templine);					/* build up the info for the item too	*/
			strcpy(the_level,"    10");					/* 10 is the level of the item		*/
		}

		make_fac(templine,this_item->name);					/* make a FAC				*/
		tput_line("           %s  %s PIC X.\n",the_level,templine);		/* write it out				*/
		tput_block(item_str);							/* set up nesting of the item too	*/

		tput_line_at(12, "%s  %s",the_level,this_item->name);
		if (item_blank(this_item->name))
		{
			tput_clause (16, "PIC %s BLANK WHEN ZERO.",this_item->pic);
		}
		else									/* see if it fits on one line		*/
		{
			tput_clause (16, "PIC %s.",this_item->pic);
		}
	}
	else										/* the item is singular			*/
	{
		if (this_item->object_clause)						/* has an object			*/
		{
			k = pic_fac(this_item->pic);					/* generate a fac value			*/
			if ( 0x81 == k ) fac_idx = 1;
			else 		 fac_idx = 0;
		}
		else
		{
			k = 0x8C;							/* no object, protect it		*/
			fac_idx = 0;
		}

		if ( strcmp(this_item->name,"FILLER") == 0 )
		{
			strcpy(templine,"FILLER");
		}
		else
		{
			make_fac(templine,this_item->name);				/* make a FAC				*/
		}

		tput_line_at(12, "05  %s PIC X",templine);
		tput_clause (16, "VALUE %s.",fac_name[fac_idx]);

		if ( !strcmp(this_item->name,"FILLER"))					/* process fillers special		*/
		{
			if (this_item->source_clause)					/* if it has a source, use it		*/
			{
				if (LITERAL != this_item->source_clause->token->type)	/* a literal source			*/
				{
					/*
					**	This case must never happen because a non-literal value will
					**	be used in a MOVE statement so the target has to be a unique
					**	name not FILLER.
					*/
					write_log("WISP",'E',"SCRNFILL","Internal error, non-literal value on FILLER.");
				}
				tput_line_at  (12, "05  %s PIC %s VALUE", this_item->name,this_item->pic);
				tput_statement(16,this_item->source_clause);
				tput_clause   (16,".");
			}
			else
			{									/* otherwise use spaces		*/
				tput_line_at(12, "05  %s PIC %s VALUE SPACES.", this_item->name, this_item->pic);
			}
		}										/* not a filler, but has a lit-	*/
												/* eral as a source. & no object*/
		else if ( this_item->source_clause && LITERAL == this_item->source_clause->token->type && 
			  !this_item->object_clause  )
		{										
			write_log("WISP",'I',"LITERALSRC",
							"Processing Field %s with Literal source and no object.",this_item->name);
			tput_line_at  (12, "05  %s PIC %s VALUE",this_item->name,this_item->pic);
			tput_statement(16,this_item->source_clause);
			tput_clause   (16,".");
		}
		else									/* not a filler, use the item	*/
		{
			tput_line_at(12, "05  %s",this_item->name);
			if (item_blank(this_item->name))
			{
				tput_clause (16, "PIC %s BLANK WHEN ZERO.",this_item->pic);
			}
			else
			{
				tput_clause (16, "PIC %s.",this_item->pic);
			}
		}
	}
}

static gen_mwconv()
{
extern int mwconv_flag;

	if (mwconv_flag) 
	{
		tput_blank();
		tput_line_at( 8, "CONVERT-ALPHA-VALUE-TO-NUMERIC.");			/* procedure to do MOVE WITH CONVERSION	*/
		tput_line_at(12, "CALL \"mwconv\" USING");				/* first let mwconv do it		*/
		tput_line_at(16, "WISP-ALPHA-CONVERSION-FIELD,");
		tput_line_at(16, "WISP-CONVERTED-INTEGER-FIELD,");
		tput_line_at(16, "WISP-CONVERTED-FRACTION-FIELD,");
		tput_line_at(16, "WISP-CONVERSION-LENGTH,");
		tput_line_at(16, "WISP-CONVERSION-ERROR-FIELD.");

		tput_line_at(12, "IF WISP-CONVERSION-ERROR-FIELD IS = ZERO");
		tput_line_at(12, "    MOVE \"N\" TO WISP-NUMERIC-CONVERSION-FLAG,");
		tput_line_at(12, "ELSE");
		tput_line_at(12, "    MOVE \"Y\" TO WISP-NUMERIC-CONVERSION-FLAG.");
	}
}

scrn_para()									/* write the screen paragraphs for all screens	*/
{
	int scrnum;
	char wdr[48],wgs[48],wpd[48],wdc[48],wrc[48],wrx[48];

	write_log("WISP",'I',"GENWISPSCRN","Begin generation of WISP screen and support procedures.");

	if (!in_decl)									/* be sure we aren't in the declaratives*/
	{
		tput_blank();
		tput_scomment("****** The following paragraphs were insert by WISP ******");
		tput_blank();
		tput_line_at(8, "WISP-PROCEDURES SECTION.\n\n");			/* start the procedure section		*/
		gen_mwconv();
	}

	for (scrnum=0; scrnum<num_screens; scrnum++)					/* now write the procs for each screen	*/
	{
		cur_crt = scrn_crt[scrnum];						/* Find out which crt file it is.	*/

		if (in_decl && !(scrn_flags[scrnum] & SCRN_IN_DECLARATIVES)) continue;	/* skip if in decl and not referenced	*/
		if (!in_decl && !(scrn_flags[scrnum] & SCRN_IN_PROCDIV)) continue;	/* skip it if it's not referenced	*/

		if (in_decl)
		{
			make_fld(wdr,scrn_name[scrnum],"DWDR-");
			make_fld(wgs,scrn_name[scrnum],"DWGS-");
			make_fld(wpd,scrn_name[scrnum],"DWPD-");
			make_fld(wdc,scrn_name[scrnum],"DWDC-");
			make_fld(wrc,scrn_name[scrnum],"DWRC-");
			make_fld(wrx,scrn_name[scrnum],"DWRX-");
		}
		else
		{
			make_fld(wdr,scrn_name[scrnum],"WDR-");
			make_fld(wgs,scrn_name[scrnum],"WGS-");
			make_fld(wpd,scrn_name[scrnum],"WPD-");
			make_fld(wdc,scrn_name[scrnum],"WDC-");
			make_fld(wrc,scrn_name[scrnum],"WRC-");
			make_fld(wrx,scrn_name[scrnum],"WRX-");
		}

		tput_blank();
		tput_scomment("***** PROCEDURES FOR SCREEN %s",scrn_name[scrnum]);
		tput_line_at(8,  "%s.",wdr);						/* Gen DISPLAY AND READ paragraph.	*/
		tput_line_at(12, "PERFORM %s.",wpd);					/* Perform Put Screen Data.		*/
		tput_line_at(12, "MOVE SPACE TO %s.",crt_status[cur_crt]);
		tput_line_at(12, "PERFORM %s",wdc);					/* Perform Display and check.		*/
		tput_line_at(20, "UNTIL WISP-ON-PF-KEYS IS EQUAL TO \"*\"");
		tput_line_at(20, "AND   %s IS NOT EQUAL TO SPACE",crt_status[cur_crt]);
		tput_line_at(20, "AND   %s IS NOT EQUAL TO \"E\".",crt_status[cur_crt]);
		if (crt_cursor[cur_crt][0])						/* if there is a cursor			*/
		{
			tput_line_at  (12, "CALL \"w2rowcol\" USING WISP-CRT-ORDER-AREA-3,");
			tput_clause   (16, "%s.",crt_cursor[cur_crt]);
		}

		tput_line("       %s.\n",wdc);						/* Wisp display and check.		*/
		tput_line("           CALL \"wscreen\" USING %s,\n",scrn_name[scrnum]);
		tput_line("                                VWANG-DISP-AND-READ-ALT,\n");
		tput_line("                                WISP-CRT-RECORD,\n");
		tput_line("                                VWANG-FULL-SCREEN,\n");
		tput_line("                                WISP-ALLOWABLE-PF-KEYS,\n");
		tput_line("                                WISP-ON-PF-KEYS,\n");
		tput_line("                                %s,\n",crt_pfkey[cur_crt]);	/* output the PFKEY variable name 	*/
		tput_line("                                %s.\n\n",crt_status[cur_crt]);/* output the STATUS variable name	*/
		tput_line_at(12, "IF WISP-ON-PF-KEYS IS NOT = \"*\" THEN");
		tput_line_at(16, "PERFORM %s THRU",wrc);			/* Perform wisp range check.			*/
		tput_clause (20, "%s.",wrx);

		tput_line_at(8,  "%s.",wrc);					/* Gen range check para.			*/

		chk_range(screen_item[scrnum]);					/* generate the range checking code		*/

		tput_line_at(8,  "%s.",wrx);					/* Gen range check exit para 			*/
		tput_line_at(12, "EXIT.");

		tput_scomment("***** PUT PROCEDURE - SCREEN %s",scrn_name[scrnum]);
		tput_line_at(8,  "%s.",wpd);						/* Put screen data.			*/

		move_source(screen_item[scrnum]);				/* move the source fields			*/
										/* move the screen order area			*/
		make_oa(templine,scrn_name[scrnum]);				/* create one					*/
		tput_line_at(12, "MOVE %s TO WISP-CRT-ORDER-AREA.",templine);

		tput_scomment("***** GET PROCEDURE - SCREEN %s",scrn_name[scrnum]);
		tput_line_at(8,  "%s.",wgs);					/* Get screen data.				*/

		if (0==move_object(screen_item[scrnum]))			/* Move all the screen fields into their objects*/
		{
			tput_line_at(12, "CONTINUE.");
		}
	}

	write_log("WISP",'I',"ENDSCRNGEN","Finish generation of screen and support procedures.");
}

static move_source(the_item)							/* move sources to their objects		*/
item_record *the_item;
{
	int i,a,b,c,i1,i2,i3;
	int dim;

	while (the_item)							/* till there are no more...			*/
	{									/* if there is a source, do it			*/
										/* if it has a source and it's not a FILLER...	*/
		if ((the_item->vert_1 != -1) && the_item->source_clause && strcmp(the_item->name,"FILLER"))
		{
			dim = the_item->vert_1 ? 1 : 0;
			dim += the_item->vert_2 ? 1 : 0;
			dim += the_item->horiz ? 1 : 0;

			if (dim == 0)							/* no repeats				*/
			{
				a = 0; b = 0; c = 0;
			}
			else if (dim == 1)						/* one dimension			*/
			{
				a = the_item->vert_1 ? the_item->vert_1 : the_item->horiz;	/* vertical or horizontal	*/
				b = 0; c = 0;
			}
			else if (dim == 2)						/* two dimensions			*/
			{
				a = the_item->vert_1;					/* vertical plus one other		*/
				b = the_item->vert_2 ? the_item->vert_2 : the_item->horiz;	/* second vertical or horizontal*/
				c = 0;
			}
			else								/* three dimensions			*/
			{
				a = the_item->vert_1;					/* first vertical			*/
				b = the_item->vert_2;					/* second vertical			*/
				c = the_item->horiz;					/* and the horizontal			*/
			}

			if (LITERAL != the_item->source_clause->token->type || the_item->object_clause)
			{
				/*
				**	Generate MOVE source-field TO screen-field 
				**	for everything except a literal with no object clause.
				*/
				gen_move(maketoknode(make_token(IDENTIFIER,the_item->name)),the_item->source_clause,dim,a,b,c);
			}
		}
		last_item = the_item;
		the_item = the_item->next_item;					/* point to the next item...			*/
	} 
}


static int move_object(the_item)
item_record *the_item;
{
	int num_moved,i;
	int i1,i2,i3,dim;

	num_moved = 0;

	while (the_item)							/* till there are no more...			*/
	{										/* if there is a object, do it		*/
		if ((the_item->vert_1 != -1) && the_item->object_clause) 		/* skip over group items		*/
		{
			dim = the_item->vert_1 ? 1 : 0;
			dim += the_item->vert_2 ? 1 : 0;
			dim += the_item->horiz ? 1 : 0;

			i1 = i2 = i3 = 0;
			if (dim == 1)							/* one dimension			*/
			{
				i1 = the_item->vert_1 ? the_item->vert_1 : the_item->horiz;	/* vertical or horizontal	*/
				i2 = 0; 
				i3 = 0;
			}
			else if (dim == 2)						/* two dimensions			*/
			{
				i1 = the_item->vert_1;					/* vertical plus one other		*/
				i2 = the_item->vert_2 ? the_item->vert_2 : the_item->horiz;	/* second vertical or horiz	*/
				i3 = 0;
			}
			else								/* three dimensions			*/
			{
				i1 = the_item->vert_1;					/* first vertical 			*/
				i2 = the_item->vert_2;					/* second vertical			*/
				i3 = the_item->horiz;					/* and the horizontal			*/
			}

			num_moved += gen_move(the_item->object_clause,maketoknode(make_token(IDENTIFIER,the_item->name)),
						dim,i1,i2,i3);

		}
		last_item = the_item;
		the_item = the_item->next_item;					/* point to the next item...			*/
	} 

	return(num_moved);							/* tell them how many				*/
}

static int gen_move(dest_node,src_node,num,i1,i2,i3)
NODE	dest_node, src_node;
int	num,i1,i2,i3;
{
	char	index[40];
	int	movecnt;

	movecnt = 0;

	switch(num)
	{
	default:
		movecnt = 1;
		break;
	case 1:
		movecnt = i1;
		break;
	case 2:
		movecnt = i1 * i2;
		break;
	case 3:
		movecnt = i1 * i2 * i3;
		break;
	}

	if (num==0)
	{
		tput_line_at  (12, "MOVE");
		tput_statement(16, src_node);
		tput_clause   (16, "TO");
		tput_statement(16, dest_node);
		tput_clause   (16, ".");
	}
	else
	{
		if (i3) 
		{
			strcpy(index,"(WIDX-1 WIDX-2 WIDX-3)");
		}
		else if (i2) 
		{
			strcpy(index,"(WIDX-1 WIDX-2)");
		}
		else if (i1) 
		{
			strcpy(index,"(WIDX-1)");
		}

		tput_line_at(12, "PERFORM VARYING WIDX-1 FROM 1 BY 1 UNTIL WIDX-1 > %d",i1);
		if (i2)
		{
			tput_line_at(13, "PERFORM VARYING WIDX-2 FROM 1 BY 1 UNTIL WIDX-2 > %d",i2);
			if (i3)
			{
				tput_line_at(14, "PERFORM VARYING WIDX-3 FROM 1 BY 1 UNTIL WIDX-3 > %d",i3);
			}
		}
#ifdef OLD1
		if (i2)
		{
			tput_line_at(20, "AFTER   WIDX-2 FROM 1 BY 1 UNTIL WIDX-2 > %d",i2);
		}
		if (i3)
		{
			tput_line_at(20, "AFTER   WIDX-3 FROM 1 BY 1 UNTIL WIDX-3 > %d",i3);
		}
#endif
		tput_line_at  (16, "MOVE");
		tput_token    (20, src_node->token);
		tput_clause   (20, index);
		tput_statement(20, src_node->next);
		tput_line_at  (18, "TO");
		tput_token    (20, dest_node->token);
		tput_clause   (20, index);
		tput_statement(20, dest_node->next);
		if (i2)
		{
			if (i3)
			{
				tput_line_at  (14, "END-PERFORM");
			}
			tput_line_at  (13, "END-PERFORM");
		}
		tput_line_at  (12, "END-PERFORM.");
	}

	return movecnt;
}

static int chk_range(the_item)							/* scan a screen record and create range checks	*/
item_record *the_item;
{
	int i,i1,i2,i3,dim;

	while (the_item)							/* till there are no more...			*/
	{										/* if there is a range, do it		*/
		if ((the_item->vert_1 != -1) && the_item->lo_range_clause)		/* skip over group items		*/
		{
			dim = the_item->vert_1 ? 1 : 0;
			dim += the_item->vert_2 ? 1 : 0;
			dim += the_item->horiz ? 1 : 0;

			switch (dim)
			{
			default:
				i1 = 0;
				i2 = 0;
				i3 = 0;
				break;
			case 1:								/* one dimension			*/
				i1 = the_item->vert_1 ? the_item->vert_1 : the_item->horiz;	/* vertical or horizontal	*/
				i2 = 0; 
				i3 = 0;
				break;
			case 2:								/* two dimensions			*/
				i1 = the_item->vert_1;					/* vertical plus one other		*/
				i2 = the_item->vert_2 ? the_item->vert_2 : the_item->horiz;	/* second vertical or horiz	*/
				i3 = 0;
				break;
			case 3:								/* three dimensions			*/
				i1 = the_item->vert_1;					/* first vertical 			*/
				i2 = the_item->vert_2;					/* second vertical			*/
				i3 = the_item->horiz;					/* and the horizontal			*/
				break;
			}

			gen_range(the_item,i1,i2,i3);
		}
		last_item = the_item;
		the_item = the_item->next_item;					/* point to the next item...			*/
	} 

	tput_line_at(12, "IF %s IS NOT = \"E\"",crt_status[cur_crt]);
	tput_line_at(16, "MOVE \"*\" TO WISP-ON-PF-KEYS.");
}

static int gen_range(the_item,i1,i2,i3)
int i1,i2,i3;
item_record *the_item;
{
	char	label1[40], label2[40], label3[40], label4[40], label5[40];
	char	index[40];
	char	the_fac[80];

	if (i3) 
	{
		strcpy(index,"(WIDX-1 WIDX-2 WIDX-3)");
	}
	else if (i2) 
	{
		strcpy(index,"(WIDX-1 WIDX-2)");
	}
	else if (i1) 
	{
		strcpy(index,"(WIDX-1)");
	}
	else 
	{
		index[0] = (char)0;
	}

	if (the_item->hi_range_clause)							/* Type 1, lo and hi ranges provided	*/
	{
		tput_scomment("* OBJECT %s RANGE %s TO %s", token_data(the_item->object_clause->token), 
			token_data(the_item->lo_range_clause->token), token_data(the_item->hi_range_clause->token));

		if (i1)
		{
			tput_line_at(12, "MOVE 1 TO WIDX-1.");
			gen_label(label1);
			tput_line_at(8,  "%s.",label1);
		}
		if (i2)
		{
			tput_line_at(12, "MOVE 1 TO WIDX-2.");
			gen_label(label2);
			tput_line_at(8,  "%s.",label2);
		}
		if (i3)
		{
			tput_line_at(12, "MOVE 1 TO WIDX-3.");
			gen_label(label3);
			tput_line_at(8,  "%s.",label3);
		}

		make_fac(the_fac,the_item->name);

		tput_line_at  (12, "MOVE WISP-MOD-BIT TO WISP-SET-BYTE.");
		tput_line_at  (12, "CALL \"bit_test\" USING WISP-SET-BYTE,");
		tput_clause   (16, "%s%s,", the_fac,index);
		tput_clause   (16, "WISP-TEST-BYTE.");

		tput_line_at  (12, "IF WISP-TEST-BYTE = \"Y\" THEN");
		tput_line_at  (16, "MOVE %s%s TO",	the_item->name,index);
		tput_clause   (20, "%s%s", token_data(the_item->object_clause->token), index);
		tput_statement(20, the_item->object_clause->next);

		tput_line_at  (16, "IF %s", token_data(the_item->object_clause->token));
		tput_clause   (20, index);
		tput_statement(20, the_item->object_clause->next);
		tput_clause   (20, "IS < ");
		tput_statement(20, the_item->lo_range_clause);
		tput_clause   (20, "OR IS > ");
		tput_statement(20, the_item->hi_range_clause);
		tput_clause   (20, "THEN");

		tput_line_at  (20, "MOVE");
		tput_statement(24, the_item->lo_range_clause);
		tput_clause   (24, "TO %s", token_data(the_item->object_clause->token));
		tput_clause   (24, index);
		tput_statement(24, the_item->object_clause->next);

		tput_line_at  (20, "MOVE \"E\" TO %s,\n",crt_status[cur_crt]);
		tput_line_at  (20, "MOVE WISP-ERROR-BIT TO WISP-SET-BYTE,");
		tput_line_at  (20, "CALL \"bit_on\" USING WISP-SET-BYTE,");
		tput_clause   (24, "%s%s.", the_fac,index);

		if (i3)
		{
			tput_line_at(12, "ADD 1 TO WIDX-3.");
			tput_line_at(12, "IF WIDX-3 <= %d THEN",i3);
			tput_clause (16, "GO TO %s.",label3);
		}
		if (i2)
		{
			tput_line_at(12, "ADD 1 TO WIDX-2.");
			tput_line_at(12, "IF WIDX-2 <= %d THEN",i2);
			tput_clause (16, "GO TO %s.",label2);
		}
		if (i1)
		{
			tput_line_at(12, "ADD 1 TO WIDX-1.");
			tput_line_at(12, "IF WIDX-1 <= %d THEN",i1);
			tput_clause (16, "GO TO %s.",label1);
		}
	}										/* type 2, constant phrase		*/
	else if (eq_token(the_item->lo_range_clause->token,KEYWORD,"POSITIVE") ||
	         eq_token(the_item->lo_range_clause->token,KEYWORD,"NEGATIVE")   )
	{
		free_statement(the_item->lo_range_clause);				/* Juster will handle this		*/
		the_item->lo_range_clause = NULL;
	}
	else										/* has to be type 3, array check	*/
	{
		char	table_occurs_count[80];

		if (the_item->num_range)
		{
			sprintf(table_occurs_count,"%d",the_item->num_range);
		}
		else if (get_occurs(token_data(the_item->lo_range_clause->token),table_occurs_count))
		{
			write_log("WISP",'E',"RANGEOCCURS", "Unable to find OCCURS for RANGE table %s.",
					token_data(the_item->object_clause->token));
			strcpy(table_occurs_count,"(UNKNOWN)");
		}

		tput_scomment("* OBJECT %s RANGE TABLE %s OCCURS %s TIMES", 
					token_data(the_item->object_clause->token), 
					token_data(the_item->lo_range_clause->token),
					table_occurs_count);

		if (i1)
		{
			tput_line_at(12, "MOVE 1 TO WIDX-1.");
			gen_label(label1);
			tput_line_at(8,  "%s.",label1);
		}
		if (i2)
		{
			tput_line_at(12, "MOVE 1 TO WIDX-2.");
			gen_label(label2);
			tput_line_at(8,  "%s.",label2);
		}
		if (i3)
		{
			tput_line_at(12, "MOVE 1 TO WIDX-3.");
			gen_label(label3);
			tput_line_at(8,  "%s.",label3);
		}

		gen_label(label4);
		gen_label(label5);
		make_fac(the_fac,the_item->name);

		tput_line_at  (12, "MOVE WISP-MOD-BIT TO WISP-SET-BYTE.");
		tput_line_at  (12, "CALL \"bit_test\" USING WISP-SET-BYTE,");
		tput_clause   (16, "%s%s,", the_fac,index);
		tput_clause   (16, "WISP-TEST-BYTE.");

		tput_line_at  (12, "IF WISP-TEST-BYTE NOT = \"Y\" THEN");
		tput_clause   (16, "GO TO %s.",label5);

		tput_line_at  (12, "MOVE %s%s", the_item->name,index);
		tput_clause   (16, "TO %s", token_data(the_item->object_clause->token));
		tput_clause   (20, index);
		tput_statement(20, the_item->object_clause->next);
		tput_clause   (20, ".");

		tput_line_at  (12, "MOVE 1 TO WIDX-4.");
                tput_line_at  (8,  "%s.", label4);

		tput_line_at  (12, "IF %s", token_data(the_item->object_clause->token));
		tput_clause   (16, index);
		tput_statement(16, the_item->object_clause->next);
		tput_clause   (16, "IS = %s(WIDX-4)", token_data(the_item->lo_range_clause->token));
		tput_clause   (16, "THEN");
		tput_clause   (16, "GO TO %s.", label5);

		tput_line_at  (12, "ADD 1 TO WIDX-4.");
		tput_line_at  (12, "IF WIDX-4 <= %s THEN",table_occurs_count);
		tput_clause   (16, "GO TO %s.",label4);

		tput_line_at  (12, "MOVE \"E\" TO %s.",crt_status[cur_crt]);
		tput_line_at  (12, "MOVE WISP-ERROR-BIT TO WISP-SET-BYTE.");
		tput_line_at  (12, "CALL \"bit_on\" USING WISP-SET-BYTE,");
		tput_clause   (16, "%s%s.",the_fac,index);
		tput_line_at  (8,  "%s.",label5);

		if (i3)
		{
			tput_line_at(12, "ADD 1 TO WIDX-3.\n");
			tput_line_at(12, "IF WIDX-3 <= %d THEN",i3);
			tput_clause (16, "GO TO %s.\n",label3);
		}
		if (i2)
		{
			tput_line_at(12, "ADD 1 TO WIDX-2.\n");
			tput_line_at(12, "IF WIDX-2 <= %d THEN",i2);
			tput_clause (16, "GO TO %s.\n",label2);
		}
		if (i1)
		{
			tput_line_at(12, "ADD 1 TO WIDX-1.\n");
			tput_line_at(12, "IF WIDX-1 <= %d THEN",i1);
			tput_clause (16, "GO TO %s.\n",label1);
		}
	}

}

static p_occurs(exit_item)								/* Process the end of a group of occurs.*/
item_record *exit_item;									/* Up to the item pointed to.		*/
{
	item_record *temp_ptr;
	int low_row;
	int hi_row;
	int i;

	low_row = 25;									/* Set up to find the hi and low row.	*/
	hi_row = 0;

	if (!occurs_item) return(0);

	temp_ptr = occurs_item;								/* Point to first occurs item.		*/

	for(;;)										/* Do all till we process the exit item	*/
	{
		if (temp_ptr->vert_1 != -1) 						/* Skip over junk.			*/
		{
			low_row = (temp_ptr->row < low_row) ? temp_ptr->row : low_row;	/* See if its lower.			*/
			hi_row = (temp_ptr->row > hi_row) ? temp_ptr->row : hi_row;	/* See if its higher.			*/
		}
		if (temp_ptr == exit_item) break;
		temp_ptr = temp_ptr->next_item;
	}

	i = hi_row - low_row + 1;							/* Actual vertical increment value.	*/

	i = (i < 1) ? 1 : i;								/* Make sure we did something.		*/

	temp_ptr = occurs_item;								/* Point to first occurs item.		*/
	do
	{
		temp_ptr->vert_off = i;							/* Now set the vertical offsets.	*/
	} while (temp_ptr = temp_ptr->next_item);					/* Check them all			*/

	occurs_item = 0;								/* Zero the pointer.			*/
}


static int item_blank(the_name)								/* See if this item is in the blank list*/
char *the_name;
{
	int i;

	for(i=0; i < blank_count; i++)
	{
		if (!strcmp(blank_item[i],the_name)) return(1);				/* Found it!!				*/
	}
	return(0);									/* Didn't find it.			*/
}

static clear_item(the_item)
item_record *the_item;
{
	strcpy(the_item->item_num,"05  ");						/* default item num is 05		*/
	the_item->name[0] = 0;								/* Clear the variable name		*/
	the_item->pic[0] = 0;								/* The pic definition for the var	*/
	the_item->source_clause = NULL;							/* The var that is the source for output*/
	the_item->object_clause = NULL;							/* The var that is the target for input */
	the_item->lo_range_clause = NULL;						/* The low end of the RANGE		*/
	the_item->hi_range_clause = NULL;						/* The high end of the RANGE		*/
	the_item->num_range = 0;							/* no OCCURing range items		*/
	the_item->row = 0;
	the_item->col = 0;								/* the row and collumn of the display	*/
	the_item->vert_1 = cur_v1;							/* the item does not occur vertically	*/
	the_item->vert_2 = cur_v2;							/* The item does not repeat twice	*/
	the_item->horiz = cur_h;							/* the item does not occur horizontally	*/
	the_item->vert_off = 1;								/* Default distance for vertical items.	*/
	the_item->next_item = 0;							/* a ptr to the next item		*/
	the_item->x_level = 0;
	the_item->x_occurs = 0;
	the_item->x_row = 0;
	the_item->x_col = 0;
	the_item->vert_off2 = 1;
}

int gen_label(newlabel)
char *newlabel;
{
	static cnt;

	cnt++;
	sprintf(newlabel,"WLABEL-%04d",cnt);
	return 0;
}



/* 	Search through the various field lists until you find the given field, then return it's pointer.			*/

item_record *find_item(the_name)
char *the_name;
{
	int i;
	item_record *the_item;

	for (i=0; i<num_screens; i++)							/* first search each screen...		*/
	{
		the_item = screen_item[i];						/* Get ptr to first field		*/
		do
		{
			if (!strcmp(the_name,the_item->name))				/* found it!				*/
			{
				return(the_item);					/* Return the pointer.			*/
			}
			the_item = the_item->next_item;					/* check next item			*/
		} while (the_item);							/* until no more items			*/
	}
	return(0);									/* Not found.				*/
}


static char *occurs_cache = NULL;						/* Cache or items with OCCURS clauses		*/

struct occurs_s
{
	char	name[40];
	char	count[40];
};

static int occurs_cmp(p1,p2)							/* Compare routine for occurs_cache ring.	*/
struct occurs_s *p1, *p2;
{
	return(strcmp(p1->name, p2->name));
}

got_occurs(data_name,count)
char	*data_name;
char	*count;
{
	static int first=1;
	struct occurs_s tmpocc;
	int	rc;

	if (first)
	{
		first = 0;
		if (rc = ring_open(&occurs_cache, sizeof(struct occurs_s), 50, 50, occurs_cmp, 0))
		{
			write_log("WISP",'F',"RINGOPEN","Unable to open ring [occurs_cache] rc=%d [%s]",rc,ring_error(rc));
			exit_with_err();
		}
	}

	strcpy(tmpocc.name,data_name);
	strcpy(tmpocc.count,count);

	rc = ring_add(occurs_cache,0,&tmpocc);
	if (rc < 0)
	{
		write_log("WISP",'F',"RINGADD","Unable to add to ring [occurs_cache] rc=%d [%s]",rc,ring_error(rc));
		exit_with_err();
	}
}

int get_occurs(data_name,count)
char	*data_name;
char	*count;
{
	struct occurs_s tmpocc,matchocc;
	int	rc;

	if (!occurs_cache) return(1);

	strcpy(matchocc.name,data_name);

	rc = ring_find(occurs_cache,&matchocc,NULL,&tmpocc);
	if (rc < 0)
	{
		write_log("WISP",'F',"RINGFIND","Unable to find ring [occurs_cache] rc=%d [%s]",rc,ring_error(rc));
		exit_with_err();
	}
	if (0==rc)
	{
		strcpy(count,tmpocc.count);
	}

	return(rc);
}
