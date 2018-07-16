static char copyright[]="Copyright (c) 1995-1997 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";

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
**	item_blank()
**	init_screen_item()
**	gen_label()
**	find_item()
**
**
*/

/*
**	wisp_scrn.c
*/

#include <string.h>
#include <assert.h>

#define EXT extern
#include "wisp.h"
#include "scrn.h"
#include "crt.h"
#include "token.h"
#include "node.h"
#include "directiv.h"
#include "wmalloc.h"

#include "wcommon.h"
#include "reduce.h"
#include "ring.h"
#include "statment.h"

NODE get_statement();

char	decimal_is = '.';

static int cur_v1 = 0;									/* the current vertical 1 occurs count	*/
static int cur_v2 = 0;									/* the current vertical 2 occurs count	*/
static int cur_h  = 0;									/* the current horizontal occurs count	*/

static int gen_screens22();
static int move_source();
static int move_object();
static void chk_range(item_record* the_item);
static int item_blank();
static void init_screen_item(item_record *the_item);
static int item_fac();
static int gen_move();
static void gen_range(item_record* the_item, int i1, int i2, int i3);
static void gen_acn_range(item_record* the_item, int i1, int i2, int i3);
static void gen_wisp_screen_paras(void);
static void gen_acn_screen_paras(void);

static void calc_occurs_offsets(item_record* the_item);
char* addsuffix(const char* fld, const char* suff);

#define MAX_ITEM_TABLE 400



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
NODE parse_display_ws_record(NODE next_statement)
{
	static  int 	fillernum = 0;							/* Current number to append to FILLER	*/

	int	curr_level;
	int	curr_level_col;
	int	cur_v1_level;
	int	cur_v2_level;
	int 	highest_sub_level;							/* the level number of screen items	*/

	NODE	the_statement;
	NODE	curr_node, temp_node;
	NODE	level_node, data_name_node, column_node, row_node, pic_node, picture_node, range_node;
	NODE	object_node, value_node, source_node, occurs_node, period_node;

	int	has_occurs;
	int	first_time;

	has_occurs = 0;
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

			curr_level_col = level_node->token->column;

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

				if (has_occurs)
				{
					calc_occurs_offsets(screen_item[num_screens-1]);
				}

				return(the_statement);
			}
			first_time = 0;

			tput_fluff(the_statement->down);

			write_log("WISP",'I',"PROCSCREEN","Processing screen %s",data_name_node->token->data);

			if (num_screens >= MAX_SCREENS)					/* do we have room for more?		*/
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
			init_screen_item(temp_ptr);					/* Init the fields in the item		*/

			temp_ptr->vert_1 = cur_v1;
			temp_ptr->vert_2 = cur_v2;
			temp_ptr->horiz = cur_h;

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

			init_screen_item(this_item);					/* Init the fields in the item		*/

			this_item->vert_1 = cur_v1;
			this_item->vert_2 = cur_v2;
			this_item->horiz = cur_h;

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
			this_item->x_level_col = curr_level_col;

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
					has_occurs = 1;
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

			/*
			**	If it is a filler  convert it into a variable of the	
			**	form SCREEN-FILLER-NNNN if the following is true:   
			**	It has an OBJECT, or it has a non-literal source
			**	(This is not needed for native screens unless there is a range clause)
			*/
			if ( 0==strcmp(this_item->name,"FILLER") )
			{
				if (acn_cobol)
				{
					if (range_node)
					{
						/*
						**	Native screen need a named item for RANGE clauses so
						**	the fac, color, and control get generated so the field
						**	can be marked as an error if the range is invalid.
						*/
						sprintf(this_item->name,"RANGE-FILLER-%d",fillernum++);
						write_log("WISP",'I',"CONVFILLER","Converted FILLER to %s.",this_item->name);
					}
				}
				else if ( object_node || 
					 (this_item->source_clause && LITERAL != this_item->source_clause->token->type))
				{
					sprintf(this_item->name,"SCREEN-FILLER-%d",fillernum++);
					write_log("WISP",'I',"CONVFILLER","Converted FILLER to %s.",this_item->name);
				}
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


/*
**	Generate the WORKING STORAGE screen fields.
**	This is called at end of working storage before LINKAGE and PROCEDURE DIVISION
*/
void gen_screen_storage(void)
{
	if (acn_cobol)
	{
		/*
		**	For Native Screens generate the FAC, COLOR, and CONTROL fields
		*/
		gen_acn_facs();
	}
	else 
	{
		/*
		**	Generate the WISP screen structures
		*/
		gen_screens();
	}
}


static int scrns_done = 0;								/* flag to say we did it		*/

int gen_screens(void)
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


		tput_line_at(12,"05  WISP-SCREEN-VERSION PIC X VALUE DEC-BYTE-%d.",SCREEN_VERSION);
		tput_line_at(12,"05  FILLER PIC X(8)  VALUE \"%-8s\".",prog_id);
		tput_line_at(12,"05  FILLER PIC X(32) VALUE");
		tput_line_at(33,"\"%-32s\".",scrn_name[i]);
		tput_line_at(12,"05  STATIC-DYNAMIC PIC X     VALUE \"S\".");
		tput_line_at(12,"05  DECIMAL-IS     PIC X     VALUE \"%c\".",decimal_is);
		tput_line_at(12,"05  RESERVED-SPACE PIC X(9)  VALUE SPACES.");

		this_item = screen_item[i];						/* Get ptr to first item		*/
		last_item = this_item;

		while(this_item)							/* loop for each item in the screen	*/
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

		}									/* 'till there are no more...		*/


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
	int k,m;
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
	return 0;
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
	return 0;
}

void gen_endstuff(void)
{
	tput_blank();
	tput_scomment("****** The following paragraphs were insert by WISP ******");
	tput_blank();
	tput_line_at(8, "WISP-PROCEDURES SECTION.\n\n");			/* start the procedure section		*/

	tput_blank();
	tput_line_at(8, "WISP-CATCH-FALL-OFF-END.");
	tput_line_at(8, "    PERFORM WISP-EXIT-PROGRAM.");
	tput_line_at(8, "    PERFORM WISP-STOP-RUN.");

	gen_mwconv();

	gen_screen_paras();
}

/*
**	Generate the DISPLAY AND READ screen paragraphs.
**	This gets called at end of DECLARATIVES and at end of PROCEDURE DIVISION
*/
void gen_screen_paras(void)
{
	if (num_screens < 1) return;
	
	if (acn_cobol)
	{
		gen_acn_screen_paras();
	}
	else
	{
		gen_wisp_screen_paras();
	}
}

/*
**	Generate the ACUCOBOL Native Screens DISPLAY AND READ paragraphs
*/
static void gen_acn_screen_paras(void)
{
	int 	scrnum;
	char	main_para[48];
	char	*check_para;
	char	*noitem_para;
	char	*setup_para;
	char	fac_para[48];
	char	range_para[48];
	char	the_orderarea[80];
	char	the_fac[80];
	char	the_color[80];
	char	the_control[80];
	int	has_object_fields, has_fac_fields, has_range_fields;
	
	write_log("WISP",'I',"GENACNSCRN","Begin generation of Acucobol screen procedures.");

	if (in_decl)
	{
		check_para  = "DWISP-DNR-CHECK-PFKEY";
		noitem_para = "DWISP-DNR-ACCEPT-NOFIELDS";
		setup_para  = "DWISP-DNR-SETUP";
	}
	else
	{
		check_para  = "WISP-DNR-CHECK-PFKEY";
		noitem_para = "WISP-DNR-ACCEPT-NOFIELDS";
		setup_para  = "WISP-DNR-SETUP";
	}

	/*
	**	Loop thru each screen and generate the paragraphs
	*/
	for (scrnum=0; scrnum<num_screens; scrnum++)
	{
		cur_crt = scrn_crt[scrnum];						/* Find out which crt file it is.	*/

		if (in_decl && !(scrn_flags[scrnum] & SCRN_IN_DECLARATIVES)) continue;	/* skip if in decl and not referenced	*/
		if (!in_decl && !(scrn_flags[scrnum] & SCRN_IN_PROCDIV)) continue;	/* skip it if it's not referenced	*/

		if (in_decl)
		{
			make_fld(main_para,   scrn_name[scrnum],"DWDNR-");
			make_fld(fac_para,    scrn_name[scrnum],"DWFAC-");
			make_fld(range_para,  scrn_name[scrnum],"DWRNG-");
		}
		else
		{
			make_fld(main_para,   scrn_name[scrnum],"WDNR-");
			make_fld(fac_para,    scrn_name[scrnum],"WFAC-");
			make_fld(range_para,  scrn_name[scrnum],"WRNG-");
		}

		/*
		**	Find out if this screen has OBJECT clause, RANGE clause, and FAC fields
		*/
		has_object_fields = 0;
		has_fac_fields = 0;
		has_range_fields = 0;
		for(this_item = screen_item[scrnum]; this_item; this_item = this_item->next_item)
		{
			if (this_item->object_clause)
			{
				has_object_fields = 1;
			}

			if (this_item->lo_range_clause)
			{
				has_range_fields = 1;
			}

			if (!has_fac_fields && this_item->pic[0] && strcmp(this_item->name,"FILLER") != 0 )
			{
				has_fac_fields = 1;
			}
		}

		tput_blank();
		tput_scomment("***** DISPLAY AND READ %s",scrn_name[scrnum]);
		tput_line_at( 8, "%s.", main_para);
		tput_blank();

		/*
		**	Generate cursor positioning logic
		*/
		make_oa(the_orderarea,scrn_name[scrnum]);
		tput_line_at(12, "MOVE %s TO WISP-DNR-ORDER-AREA.",the_orderarea);

		/*     123456789012345678901234567890123456789012345678901234567890123456789012 */

		/*
		**	If the screen has FACs then perform the fac para to convert 
		**	FAC values into COLOR and CONTROL values.
		*/
		if (has_fac_fields)
		{
			tput_blank();
			tput_line_at(12, "PERFORM %s.", fac_para);
		}

		tput_line_at(12, "PERFORM %s.", setup_para);

		/*
		**	Perform a ACCEPT loop until a valid PFKEY was pressed and it
		**	passed RANGE clause checking.
		*/
		tput_blank();
		tput_line_at(12, "PERFORM UNTIL WISP-DNR-DONE");

		tput_line_at(12, "    DISPLAY %s", scrn_name[scrnum]);
		
		if (has_object_fields)
		{
			/*
			**	The screen has OBJECT fields so perform a normal ACCEPT
			**	of the screen.
			**	If all the fields are protected then do a "noitem" ACCEPT.
			*/
			tput_line_at(16, "ACCEPT %s", scrn_name[scrnum]);
			tput_line_at(16, "    EXCEPTION CONTINUE");
			tput_line_at(16, "END-ACCEPT");
			tput_line_at(16, "IF WISP-CRT-STATUS-NOITEM THEN");
			tput_line_at(16, "    PERFORM %s", noitem_para);
			tput_line_at(16, "END-IF");
		}
		else
		{
			tput_line_at(16, "PERFORM %s", noitem_para);
		}

		/*
		**	Check for a valid PFKEY press
		*/
		tput_line_at(16, "PERFORM %s", check_para);

		if (has_range_fields)
		{
			/*
			**	This screen has RANGE clause.
			**	If a valid PFKEY was pressed and it wasn't an ON-PFKEY then
			**	check the RANGE clauses.
			**	If the RANGE check fails then re-display the screen.
			*/
			tput_line_at(16, "IF WISP-DNR-DONE AND WISP-DNR-ON-PFKEY = \"N\" THEN");
			tput_line_at(16, "    PERFORM %s", range_para);
			tput_line_at(16, "    IF NOT WISP-DNR-DONE THEN");
			tput_line_at(16, "        MOVE 0 TO WISP-CURSOR-LINE, WISP-CURSOR-COL");
			tput_line_at(16, "        DISPLAY %s", scrn_name[scrnum]);
			tput_line_at(16, "    END-IF");
			tput_line_at(16, "END-IF");
		}
		
		tput_line_at(12, "END-PERFORM.");
		tput_blank();

		/*
		**	If the CRT has a PFKEY clause then update it.
		*/
		if (0 != strcmp(crt_pfkey[cur_crt],"WISP-PFKEY-VALUE"))
		{
			tput_line_at(12, "MOVE WISP-PFKEY TO %s.",crt_pfkey[cur_crt]);
		}

		/*
		**	If the CRT has a CURSOR clause then update it.
		*/
		if (crt_cursor[cur_crt][0])
		{
			tput_line_at(12, "MOVE WISP-CURSOR-LINE TO WISP-CURSOR-POSITION-ROW.");
			tput_line_at(12, "MOVE WISP-CURSOR-COL  TO WISP-CURSOR-POSITION-COL.");
			tput_line_at(12, "MOVE WISP-CURSOR-POSITION TO");
			tput_clause (12, "%s.",crt_cursor[cur_crt]);
		}

		/*
		**	Generate the RANGE handling para
		*/
		if (has_range_fields)
		{
			tput_blank();
			tput_line_at(8,  "%s.", range_para);
			chk_range(screen_item[scrnum]);
		}

		/*
		**	Generate the FAC handling paragraphs.
		**
		**	For each FAC field generate a CALL "WACUFAC2SCREEN" to convert
		**	the FAC into a COLOR and CONTROL.
		*/
		if (has_fac_fields)
		{
			tput_blank();
			tput_line_at(8,  "%s.", fac_para);
		}
		for(this_item = screen_item[scrnum]; has_fac_fields && this_item; this_item = this_item->next_item)
		{
			if (!this_item->pic[0])
			{
				/* Group items don't need FAC fields */
				continue;
			}
			if ( strcmp(this_item->name,"FILLER") == 0 )
			{
				/* FILLER items don't need FAC fields */
				continue;
			}

			tput_blank();

			make_fac(the_fac,this_item->name);
			make_color(the_color,this_item->name);
			make_control(the_control,this_item->name);
			
			/*
			**	PERFORM VARYING WIDX-1 FROM 1 BY 1 UNTIL WIDX-1 > i1
			**	 PERFORM VARYING WIDX-2 FROM 1 BY 1 UNTIL WIDX-2 > i2
			**	  PERFORM VARYING WIDX-3 FROM 1 BY 1 UNTIL WIDX-3 > i3
			**	   CALL "WACUFAC2SCREEN" fac( x x x), color(x x x), control(x x x)
			**	  END-PERFORM
			**	 END-PERFORM
			**	END-PERFORM
			*/
			/* if the item is a multiple item...	*/
			if (this_item->vert_1 || this_item->vert_2 || this_item->horiz)
			{
				int i1, i2, i3;
				char	index[40];

				i2 = 0;
				i3 = 0;
				if (this_item->vert_1)
				{
					i1 = this_item->vert_1;

					if (this_item->vert_2)
					{
						i2 = this_item->vert_2;

						if (this_item->horiz)
						{
							i3 = this_item->horiz;
						}
					}
					else if (this_item->horiz)
					{
						i2 = this_item->horiz;
					}
				}
				else
				{
					i1 = this_item->horiz;
				}

				if (i3) 
				{
					strcpy(index,"(WIDX-1 WIDX-2 WIDX-3)");
				}
				else if (i2) 
				{
					strcpy(index,"(WIDX-1 WIDX-2)");
				}
				else
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
				tput_line_at(16, "CALL \"WACUFAC2SCREEN\" USING");
				tput_line_at(20, "%s%s,", the_fac, index);
				tput_line_at(20, "%s%s,", the_color, index);
				tput_line_at(20, "%s%s" , the_control, index);

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
			else
			{
				tput_line_at(12, "CALL \"WACUFAC2SCREEN\" USING");
				tput_clause (16, "%s,", the_fac);
				tput_clause (16, "%s,", the_color);
				tput_clause (16, "%s.", the_control);
			}

			/* END OF FAC PARA */
		}

		/* END OF THIS SCREEN */
	}

	/*
	**	Generate "canned" paragraphs
	*/
	tput_blank();
	tput_scomment("***** SETUP FOR DISPLAY AND READ STATEMENTS");
	tput_line_at(8,  "%s.", setup_para);

	tput_line_at(12, "DISPLAY WINDOW ERASE.");

	tput_blank();
	tput_line_at(12, "IF WISP-DNR-WCC-SOUND-ALARM THEN");
	tput_line_at(12, "    DISPLAY OMITTED BELL");
	tput_line_at(12, "END-IF.");

	tput_blank();
	tput_line_at(12, "IF WISP-DNR-WCC-POSITION-CURSOR THEN");
	tput_line_at(12, "    MOVE WISP-DNR-CURSOR-ROW TO WISP-CURSOR-LINE");
	tput_line_at(12, "    MOVE WISP-DNR-CURSOR-COL TO WISP-CURSOR-COL");
	tput_line_at(12, "ELSE");
	tput_line_at(12, "    MOVE 0 TO WISP-CURSOR-COL, WISP-CURSOR-LINE");
	tput_line_at(12, "END-IF.");

	tput_blank();
	tput_line_at(12, "IF WISP-DNR-WCC-UNLOCK-KEYBOARD THEN");
	tput_line_at(12, "    MOVE \"N\" TO WISP-DNR-DONE-FLAG");
	tput_line_at(12, "ELSE");
	tput_line_at(12, "    MOVE \"Y\" TO WISP-DNR-DONE-FLAG");
	tput_line_at(12, "END-IF.");

	tput_blank();
	tput_scomment("***** ACCEPT A SCREEN WITH NO FIELDS");
	tput_line_at(8,  "%s.", noitem_para);
	tput_line_at(12, "ACCEPT OMITTED LINE 1 COL 1");
	tput_line_at(12, "    EXCEPTION CONTINUE");
	tput_line_at(12, "END-ACCEPT.");


	tput_blank();
	tput_scomment("***** CHECK PF KEYS AFTER AN ACCEPT");
	tput_line_at(8,  "%s.", check_para);

	/*     123456789012345678901234567890123456789012345678901234567890123456789012 */
	tput_blank();
	tput_line_at(12, "SET WISP-PFKEY-INVALID TO TRUE.");
	tput_line_at(12, "IF WISP-CRT-STATUS-TERMINATED AND");
	tput_line_at(12, "    (WISP-CRT-KEY-TAB OR WISP-CRT-KEY-AUTOSKIP) THEN");
	tput_line_at(12, "    MOVE 0 TO WISP-CURSOR-LINE, WISP-CURSOR-COL");
	tput_line_at(12, "ELSE IF WISP-CRT-STATUS-TERMINATED AND WISP-CRT-KEY-ENTER");
	tput_line_at(12, "    MOVE 0 TO WISP-PFKEY");
	tput_line_at(12, "ELSE IF WISP-CRT-STATUS-EXCEPTION AND WISP-CRT-EX-GETKEY");
	tput_line_at(12, "    CALL \"WACUGETPFKEY\" USING WISP-PFKEY");
	tput_line_at(12, "ELSE IF WISP-CRT-STATUS-EXCEPTION");
	tput_line_at(12, "    MOVE WISP-CRT-KEY-EXCEPTION TO WISP-PFKEY");
	tput_line_at(12, "ELSE");
	tput_line_at(12, "    DISPLAY WINDOW ERASE");
	tput_line_at(12, "    DISPLAY WISP-INVALID-CRT-STATUS-SCREEN");
	tput_line_at(12, "    ACCEPT OMITTED BELL");
	tput_line_at(12, "    DISPLAY WINDOW ERASE");
	tput_line_at(12, "END-IF.");

	tput_blank();
	tput_line_at(12, "IF WISP-PFKEY-HELP THEN");
	tput_line_at(12, "    CALL \"WACUHELP\"");
	tput_line_at(12, "ELSE IF NOT WISP-PFKEY-INVALID THEN");
	tput_line_at(12, "    PERFORM VARYING WIDX-1 FROM 1 BY 1 UNTIL WISP-DNR-DONE");
	tput_line_at(12, "        OR WIDX-1 > WISP-ALLOWABLE-PF-KEYS-CNT");
	tput_line_at(12, "        IF WISP-PFKEY = WISP-ALLOWABLE-PF-KEYS-SUB(WIDX-1)");
	tput_line_at(12, "            MOVE \"Y\" TO WISP-DNR-DONE-FLAG");
	tput_line_at(12, "        END-IF");
	tput_line_at(12, "    END-PERFORM");
	tput_blank();
	tput_line_at(12, "    MOVE \"N\" TO WISP-DNR-ON-PFKEY");
	tput_line_at(12, "    PERFORM VARYING WIDX-1 FROM 1 BY 1 UNTIL");
	tput_line_at(12, "        WIDX-1 > WISP-ON-PF-KEYS-CNT");
	tput_line_at(12, "        OR WISP-DNR-DONE-FLAG = \"N\"");
	tput_line_at(12, "        OR WISP-DNR-ON-PFKEY = \"Y\"");
	tput_line_at(12, "        IF WISP-PFKEY = WISP-ON-PF-KEYS-SUB(WIDX-1) THEN");
	tput_line_at(12, "            MOVE \"Y\" TO WISP-DNR-ON-PFKEY");
	tput_line_at(12, "        END-IF");
	tput_line_at(12, "    END-PERFORM");
	tput_blank();
	tput_line_at(12, "    IF WISP-DNR-DONE-FLAG = \"N\" THEN");
	tput_line_at(12, "        DISPLAY OMITTED BELL");
	tput_line_at(12, "    END-IF");
	tput_line_at(12, "END-IF.");
	
	write_log("WISP",'I',"ENDSCRNGEN","Finish generation of Acucobol screen procedures.");
}

static void gen_wisp_screen_paras(void)
{
	int scrnum;
	char wdr[48],wgs[48],wpd[48],wdc[48],wrc[48],wrx[48];

	write_log("WISP",'I',"GENWISPSCRN","Begin generation of WISP screen and support procedures.");


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
		tput_line_at(12, "MOVE \"N\" TO WISP-DNR-DONE-FLAG.");
		tput_line_at(12, "IF WISP-DNR-ALT THEN");
		tput_line_at(12, "    MOVE VWANG-DISP-AND-READ-ALT TO WISP-DNR-FUNCTION");
		tput_line_at(12, "ELSE");           
		tput_line_at(12, "    MOVE VWANG-DISP-AND-READ     TO WISP-DNR-FUNCTION.");
		tput_line_at(12, "PERFORM %s.",wpd);					/* Perform Put Screen Data.		*/
		tput_line_at(12, "MOVE SPACE TO %s.",crt_status[cur_crt]);
		tput_line_at(12, "PERFORM %s",wdc);					/* Perform Display and check.		*/
		tput_line_at(20, "UNTIL WISP-DNR-DONE.");
#ifdef OLD
		tput_line_at(20, "UNTIL WISP-ON-PF-KEYS IS EQUAL TO \"*\"");
		tput_line_at(20, "AND   %s IS NOT EQUAL TO SPACE",crt_status[cur_crt]);
		tput_line_at(20, "AND   %s IS NOT EQUAL TO \"E\".",crt_status[cur_crt]);
#endif
		if (crt_cursor[cur_crt][0])						/* if there is a cursor			*/
		{
			tput_line_at  (12, "CALL \"w2rowcol\" USING WISP-CRT-ORDER-AREA-3,");
			tput_clause   (16, "%s.",crt_cursor[cur_crt]);
		}

		tput_line_at( 8, "%s.\n",wdc);						/* Wisp display and check.		*/
		tput_line_at(12, "CALL \"wscreen\" USING %s,\n",scrn_name[scrnum]);
		tput_line_at(12, "                     VWANG-DISP-AND-READ-ALT,\n");
/*		tput_line_at(12, "                     WISP-DNR-FUNCTION,"); */
		tput_line_at(12, "                     WISP-CRT-RECORD,\n");
		tput_line_at(12, "                     VWANG-FULL-SCREEN,\n");
		tput_line_at(12, "                     WISP-ALLOWABLE-PF-KEYS,\n");
		tput_line_at(12, "                     WISP-ON-PF-KEYS,\n");
		tput_line_at(12, "                     %s,\n",crt_pfkey[cur_crt]);	/* output the PFKEY variable name 	*/
		tput_line_at(12, "                     %s.\n\n",crt_status[cur_crt]);	/* output the STATUS variable name	*/
		tput_line_at(12, "IF WISP-ON-PF-KEYS(1:1) IS = \"*\" THEN");
		tput_line_at(16, "MOVE \"Y\" TO WISP-DNR-DONE-FLAG");
		tput_line_at(12, "ELSE");
#ifdef OLD
		tput_line_at(12, "IF WISP-ON-PF-KEYS IS NOT = \"*\" THEN");
#endif
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
	int a,b,c;
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
	return 0;
}


static int move_object(the_item)
item_record *the_item;
{
	int num_moved;
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

/* scan a screen record and create range checks	*/
static void chk_range(item_record* the_item)
{
	int i1,i2,i3,dim;

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

			if (acn_cobol)
			{
				/*
				**	Generate Acucobol Native Screens RANGE checking paras
				*/
				gen_acn_range(the_item,i1,i2,i3);
			}
			else
			{
				gen_range(the_item,i1,i2,i3);
			}
		}
		last_item = the_item;
		the_item = the_item->next_item;					/* point to the next item...			*/
	} 

	if (!acn_cobol)
	{
		tput_line_at(12, "IF %s IS NOT = \"E\"",crt_status[cur_crt]);
		tput_line_at(12, "    MOVE \"Y\" TO WISP-DNR-DONE-FLAG.");
	}
}

static void gen_range(item_record* the_item, int i1, int i2, int i3)
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

		tput_line_at  (12, "CALL \"bit_test\" USING WISP-PROTECT-BIT,");
		tput_clause   (16, "%s%s,", the_fac,index);
		tput_clause   (16, "WISP-FAC-PROTECT-FLAG.");
		tput_line_at  (12, "CALL \"bit_test\" USING WISP-ALTERED-BIT,");
		tput_clause   (16, "%s%s,", the_fac,index);
		tput_clause   (16, "WISP-FAC-ALTERED-FLAG.");

		tput_line_at  (12, "IF (WISP-DNR-NOT-ALT AND WISP-FAC-MODIFIABLE) OR");
		tput_line_at  (12, "   (WISP-DNR-ALT     AND WISP-FAC-ALTERED)  THEN");
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

		tput_line_at  (20, "CALL \"bit_on\" USING WISP-ERROR-BIT,");
		tput_clause   (24, "%s%s,", the_fac,index);
		tput_line_at  (20, "CALL \"bit_off\" USING WISP-XERROR-BIT,");
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

		tput_line_at  (12, "CALL \"bit_test\" USING WISP-PROTECT-BIT,");
		tput_clause   (16, "%s%s,", the_fac,index);
		tput_clause   (16, "WISP-FAC-PROTECT-FLAG.");
		tput_line_at  (12, "CALL \"bit_test\" USING WISP-ALTERED-BIT,");
		tput_clause   (16, "%s%s,", the_fac,index);
		tput_clause   (16, "WISP-FAC-ALTERED-FLAG.");

		tput_line_at  (12, "IF (WISP-DNR-NOT-ALT AND WISP-FAC-PROTECTED)     OR");
		tput_line_at  (12, "   (WISP-DNR-ALT     AND WISP-FAC-NOT-ALTERED) THEN");
		tput_line_at  (12, "    GO TO %s.",label5);

		tput_line_at  (12, "MOVE %s%s", the_item->name,index);
		tput_clause   (16, "TO %s", token_data(the_item->object_clause->token));
		tput_clause   (20, index);
		tput_statement(20, the_item->object_clause->next);
		tput_clause   (20, ".");

		tput_line_at  (12, "MOVE 1 TO WIDX-4.");
/* LABEL4 */	tput_line_at  (8,  "%s.", label4);

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

		tput_line_at  (12, "CALL \"bit_on\" USING WISP-ERROR-BIT,");
		tput_clause   (16, "%s%s.",the_fac,index);
		tput_line_at  (12, "CALL \"bit_off\" USING WISP-XERROR-BIT,");
		tput_clause   (16, "%s%s.",the_fac,index);
/* LABEL5 */	tput_line_at  (8,  "%s.",label5);

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

/*
**	Generate a RANGE clause checking paragraph for this screen item.
*/
static void gen_acn_range(item_record* the_item, int i1, int i2, int i3)
{
	int 	col;
	char	index[40];
	char	the_color[80];
	char	table_occurs_count[80];

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

	make_color(the_color,the_item->name);

	/*
	**	Three types of range clause:
	**	1) from low to high
	**	2) NEGATIVE|POSITIVE
	**	3) in table
	**
	**	PERFORM VARYING WIDX-1 FROM 1 BY 1 UNTIL WIDX-1 > i1
	**	 PERFORM VARYING WIDX-2 FROM 1 BY 1 UNTIL WIDX-2 > i2
	**	  PERFORM VARYING WIDX-3 FROM 1 BY 1 UNTIL WIDX-3 > i3
	**
	** 1)	   IF object-field IS < low OR IS > high THEN error
	**	   
	** 2)	   IF object-field <= 0 THEN error   (POSITIVE)
	**	   
	** 	   IF object-field >= 0 THEN error   (NEGATIVE)
	**
	** 3)	   assume error
	**	   PERFORM VARYING WIDX-4 FROM 1 BY 1 UNTIL WIDX-4 > tablesize OR DONE
	**             IF object-fiels = table(WIDX-4) THEN not-error & DONE
	**	   END-PERFORM
	**
	**	   IF error THEN
	**		MOVE "N" TO WISP-DNR-DONE-FLAG
	**		MOVE WCLR-ERROR TO color-field
	**	   END-IF
	**
	**	  END-PERFORM
	**	 END-PERFORM
	**	END-PERFORM
	*/

	tput_blank();
	
	if (the_item->hi_range_clause)							/* Type 1, lo and hi ranges provided	*/
	{
		tput_scomment("*    CHECK RANGE OF %s: %s TO %s", token_data(the_item->object_clause->token), 
			token_data(the_item->lo_range_clause->token), token_data(the_item->hi_range_clause->token));
	}
	else if (eq_token(the_item->lo_range_clause->token,KEYWORD,"POSITIVE"))
	{
		tput_scomment("*    CHECK RANGE OF %s: POSITIVE", token_data(the_item->object_clause->token));
	}
	else if (eq_token(the_item->lo_range_clause->token,KEYWORD,"NEGATIVE"))
	{
		tput_scomment("*    CHECK RANGE OF %s: NEGATIVE", token_data(the_item->object_clause->token));
	}
	else										/* has to be type 3, array check	*/
	{
		/*
		**	Get the table occurs count for this RANGE IS table clause.
		*/
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

		tput_scomment("*    CHECK RANGE OF %s: TABLE %s OCCURS %s TIMES", 
					token_data(the_item->object_clause->token), 
					token_data(the_item->lo_range_clause->token),
					table_occurs_count);
	}
	
	col = 12;
	
	if (i1)
	{
		col = 16;
		
		tput_line_at(12, "PERFORM VARYING WIDX-1 FROM 1 BY 1 UNTIL WIDX-1 > %d",i1);
		if (i2)
		{
			tput_line_at(13, "PERFORM VARYING WIDX-2 FROM 1 BY 1 UNTIL WIDX-2 > %d",i2);
			if (i3)
			{
				tput_line_at(14, "PERFORM VARYING WIDX-3 FROM 1 BY 1 UNTIL WIDX-3 > %d",i3);
			}
		}
	}


	if (the_item->hi_range_clause)
	{
		/*
		**	Both lo and hi ranges provided:
		**
		**	IF object-field IS < low OR IS > high THEN error
		*/

		tput_line_at  (col,   "IF %s", token_data(the_item->object_clause->token));
		tput_clause   (col+4, index);
		tput_statement(col+4, the_item->object_clause->next);
		tput_clause   (col+4, "IS < ");
		tput_statement(col+4, the_item->lo_range_clause);
		tput_clause   (col+4, "OR IS > ");
		tput_statement(col+4, the_item->hi_range_clause);
		tput_clause   (col+4, "THEN");

		tput_line_at  (col+4, "MOVE \"N\" TO WISP-DNR-DONE-FLAG");
		tput_line_at  (col+4, "MOVE WCLR-ERROR TO %s", addsuffix(the_color,index));

		tput_line_at  (col,   "END-IF");

	}										/* type 2, constant phrase		*/
	else if (eq_token(the_item->lo_range_clause->token,KEYWORD,"POSITIVE"))
	{
		tput_line_at  (col, "IF %s", token_data(the_item->object_clause->token));
		tput_clause   (col+4, index);
		tput_statement(col+4, the_item->object_clause->next);
		tput_clause   (col+4, "IS NOT GT 0 ");
		tput_clause   (col+4, "THEN");

		tput_line_at  (col+4, "MOVE \"N\" TO WISP-DNR-DONE-FLAG");
		tput_line_at  (col+4, "MOVE WCLR-ERROR TO %s", addsuffix(the_color,index));

		tput_line_at  (col, "END-IF");
	}
	else if (eq_token(the_item->lo_range_clause->token,KEYWORD,"NEGATIVE"))
	{
		tput_line_at  (col, "IF %s", token_data(the_item->object_clause->token));
		tput_clause   (col+4, index);
		tput_statement(col+4, the_item->object_clause->next);
		tput_clause   (col+4, "IS NOT LT 0 ");
		tput_clause   (col+4, "THEN");

		tput_line_at  (col+4, "MOVE \"N\" TO WISP-DNR-DONE-FLAG");
		tput_line_at  (col+4, "MOVE WCLR-ERROR TO %s", addsuffix(the_color,index));

		tput_line_at  (col, "END-IF");
	}
	else										/* has to be type 3, array check	*/
	{
		tput_line_at  (col,   "MOVE \"Y\" TO WISP-DNR-RANGE-ERROR");
		tput_line_at  (col,   "PERFORM VARYING WIDX-4 FROM 1 BY 1 UNTIL WIDX-4 > %s",table_occurs_count);
		tput_line_at  (col,   "    OR WISP-DNR-RANGE-ERROR = \"N\"");

		tput_line_at  (col+4, "IF %s", token_data(the_item->object_clause->token));
		tput_clause   (col+8, index);
		tput_statement(col+8, the_item->object_clause->next);
		tput_clause   (col+8, "IS = %s(WIDX-4)", token_data(the_item->lo_range_clause->token));
		tput_clause   (col+8, "THEN");

		tput_line_at  (col+4, "    MOVE \"N\" TO WISP-DNR-RANGE-ERROR");
		tput_line_at  (col+4, "END-IF");

		tput_line_at  (col,   "END-PERFORM");

		tput_line_at  (col,  "IF WISP-DNR-RANGE-ERROR = \"Y\" THEN");
		tput_line_at  (col,  "    MOVE \"N\" TO WISP-DNR-DONE-FLAG");
		tput_line_at  (col,  "    MOVE WCLR-ERROR TO %s", addsuffix(the_color,index));
		tput_line_at  (col,  "END-IF");

	}


	if (i1)
	{
		if (i2)
		{
			if (i3)
			{
				tput_line_at  (14, "END-PERFORM");
			}
			tput_line_at  (13, "END-PERFORM");
		}
		tput_line_at  (12, "END-PERFORM");
	}

	tput_clause(12, ".");
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

static void init_screen_item(item_record *the_item)
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
	the_item->vert_1 = 0;								/* the item does not occur vertically	*/
	the_item->vert_2 = 0;								/* The item does not repeat twice	*/
	the_item->horiz = 0;								/* the item does not occur horizontally	*/
	the_item->next_item = 0;							/* a ptr to the next item		*/
	the_item->x_level_col = 0;
	the_item->x_level = 0;
	the_item->x_occurs = 0;
	the_item->x_row = 0;
	the_item->x_col = 0;
	the_item->vert_off1 = 1;							/* Default distance for vertical items.	*/
	the_item->vert_off2 = 1;
	the_item->size = 0;
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

item_record *find_item(char *the_name)
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
	return 0;
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

/*
**	Generate the Native Acucobol SCREEN SECTION screens.
**
**	Any OCCURS clauses are "un-rolled" into individual items.
**	All the items are loaded into a temporary table so they can be
**	generated in screen field order.
**
**	The level structure is not perserved, this was needed in order to
**	write the fields out in screen order.  WHen the level number was
**	preserved then acucobol would tab down vertical columns instead 
**	of across as expected from the Wang.
*/
int gen_acn_screens(void)
{
	int 	scrn_idx;
	int	need_clr;
	char	color_clause[80];
	char	ctrl_clause[80];
	int	ov1, ov2, oh;
	int	ov1x, ov2x, ohx;
	int	dim;
	int	items_processed;
	int	last_row, last_col;
	int	last_match, best_match;
	int	i;
	char	index[40], cindex[40];
	int	use_using_clause;

	if (data_conv) return(0);

	if (scrns_done) return(0);							/* already did it			*/
	scrns_done = 1;

	write_log("WISP",'I',"SCREEN","Generating SCREEN SECTION.");

	tput_blank();
	tput_blank();
	tput_line_at(8, "SCREEN SECTION.");

	/*
	**	Canned screens
	*/
	tput_blank();
	tput_scomment("*    Native translation of DISPLAY verb.");
	/*       123456789012345678901234567890123456789012345678901234567890123456789012 */
	tput_line_at(8,"01  WISP-DISPLAY-SCREEN.");
	tput_line_at(8,"    05  COL 2 LINE 1 VALUE \"DISPLAY FROM PROGRAM %-8.8s\".",prog_id);
	tput_line_at(8,"    05  LINE 7.");
	tput_line_at(8,"        10  COL 2 LINE PLUS 1 PIC X(79) OCCURS 15");
	tput_line_at(8,"            FROM WISP-DISPLAY-FIELDS.");
	tput_line_at(8,"    05  COL 2 LINE 24 VALUE \"PRESS (ENTER) TO CONTINUE PROGRAM.\".");

	tput_blank();
	tput_scomment("*    Invalid CRT status.");
	tput_line_at(8,"01  WISP-INVALID-CRT-STATUS-SCREEN.");
	tput_line_at(8,"    05  LINE 12 COL 30  VALUE \"INVALID CRT STATUS\" BOLD.");
	tput_line_at(8,"    05  LINE + 2 PIC X   FROM WISP-CRT-KEY-1 BOLD.");
	tput_line_at(8,"    05  LINE + 2 PIC 999 FROM WISP-CRT-KEY-EXCEPTION BOLD.");
	tput_line_at(8,"    05  LINE + 2 PIC 999 FROM WISP-CRT-KEY-3 BOLD.");
	tput_line_at(8,"    05  LINE 24 COL 25 VALUE \"PRESS (ENTER) TO CONTINUE.\" BOLD.");
	/*       123456789012345678901234567890123456789012345678901234567890123456789012 */

	/*
	**	Loop for each screen 
	*/
	for (scrn_idx=0; scrn_idx<num_screens; scrn_idx++)					
	{
		int	xitemcnt = 0;
		struct 
		{
			item_record*	ir;
			int		row;
			int		col;
			int		dim;
			int		ov1;
			int		ov2;
			int		oh;
			int		ov1x;
			int		ov2x;
			int		ohx;
			int		done;
		} xitems[MAX_ITEM_TABLE];
			
		write_log("WISP",'I',"DOSCREEN","Generating screen %d %s.",scrn_idx,scrn_name[scrn_idx]);

		tput_blank();
		tput_scomment("******* Definition of screen %s *******", scrn_name[scrn_idx]);

		tput_line_at(8,  "01  %s.",scrn_name[scrn_idx]);

		/*
		**	Load screen item array so screen items can be generated in screen order
		*/
		for(this_item = screen_item[scrn_idx]; this_item; this_item = this_item->next_item)
		{
			if (!this_item->pic[0])
			{
				/*
				**	If no PIC then go to next.
				*/
				assert(!this_item->source_clause);
				assert(!this_item->object_clause);
				continue;
			}
			
			dim = 0;
			ov1 = ov2 = oh = 1;
			
			if (this_item->vert_1) 
			{
				dim++;
				ov1 = this_item->vert_1;
			}
			if (this_item->vert_2) 
			{
				dim++;
				ov2 = this_item->vert_2;
			}
			if (this_item->horiz) 
			{
				dim++;
				oh = this_item->horiz;
			}
			assert(ov1 > 0);
			assert(ov2 > 0);
			assert(oh  > 0);

			for( ov1x=1; ov1x<=ov1; ov1x++)
			{
				for( ov2x=1; ov2x<=ov2; ov2x++)
				{
					for( ohx=1; ohx<=oh; ohx++)
					{	
						int	the_col, the_row;

						/* Calculate the row and col */
						the_col = this_item->col;
						the_row = this_item->row;

						if (dim)
						{
							if (this_item->horiz)
							{
								the_col += (ohx - 1) * (this_item->size + 1);
							}
							if (this_item->vert_1)
							{
								the_row += (ov1x - 1) * this_item->vert_off1;

								if (this_item->vert_2)
								{
									the_row += (ov2x - 1) * this_item->vert_off2;
								}
							}
						}

						/*
						**	Add every un-rolled item to the xitems table
						*/
						xitems[xitemcnt].ir   = this_item;
						xitems[xitemcnt].row  = the_row;
						xitems[xitemcnt].col  = the_col;
						xitems[xitemcnt].dim  = dim;
						xitems[xitemcnt].ov1  = ov1;
						xitems[xitemcnt].ov2  = ov2;
						xitems[xitemcnt].oh   = oh;
						xitems[xitemcnt].ov1x = ov1x;
						xitems[xitemcnt].ov2x = ov2x;
						xitems[xitemcnt].ohx  = ohx;
						xitems[xitemcnt].done = 0;
						xitemcnt++;
						if (xitemcnt >= MAX_ITEM_TABLE)
						{
							write_log("WISP",'F',"MAXSCREENFIELDS",
								  "Exceeded MAX (%d) number of screen fields.",MAX_ITEM_TABLE);
							return 0;
						}
						
					}
				}
			}
		}

		/*
		**	Process each screen item (un-rolled) in screen order
		**	Screen order is top to bottom, left to right (normal page order).
		*/
		last_row = 0;
		last_col = 0;
		last_match = -1;
		items_processed = 0;

		while(items_processed < xitemcnt)
		{
			/*
			**	Find the next item to process in screen order
			**	We are looking for the next field which is past the
			**	last field written.
			*/
			best_match = -1;
			for (i = 0; i < xitemcnt; i++)
			{
				if (xitems[i].done)
				{
					continue;
				}

				/*
				**	If past the current position
				*/
				if ((xitems[i].row >  last_row) ||
				   ((xitems[i].row == last_row) && (xitems[i].col >= last_col)))
				{
					/*
					**	If better then best match so far - make this one the best
					*/
					if ((-1 == best_match) ||
					    (xitems[i].row <  xitems[best_match].row) ||
					   ((xitems[i].row == xitems[best_match].row) && 
					    (xitems[i].col <= xitems[best_match].col)))
					{
						best_match = i;
					}
				}
			}
			
			last_match = best_match;
			items_processed++;
			
			if (last_row == xitems[last_match].row &&
			    last_col == xitems[last_match].col)
			{
				write_log("WISP",'E',"DUPSCREENPOS", "Duplicate positions (%d,%d) in screen %s",
					  last_row, last_col, scrn_name[scrn_idx]);
			}

			xitems[last_match].done = 1;

			last_row = xitems[last_match].row;
			last_col = xitems[last_match].col;

			this_item = xitems[last_match].ir;
			dim = xitems[last_match].dim;
			ov1 = xitems[last_match].ov1;
			ov2 = xitems[last_match].ov2;
			oh  = xitems[last_match].oh;
			ov1x = xitems[last_match].ov1x;
			ov2x = xitems[last_match].ov2x;
			ohx  = xitems[last_match].ohx;

			switch(dim)
			{
			case 0:
				index[0] = '\0';
				cindex[0] = '\0';
				break;
			case 1:
				if (this_item->horiz)
				{
					sprintf(index, "(%d)", ohx);
					sprintf(cindex, "-%d", ohx);
				}
				else
				{
					sprintf(index, "(%d)", ov1x);
					sprintf(cindex, "-%d", ov1x);
				}
				break;
			case 2:
				if (this_item->horiz)
				{
					sprintf(index, "(%d %d)", ov1x, ohx);
					sprintf(cindex, "-%d-%d", ov1x, ohx);
				}
				else
				{
					sprintf(index, "(%d %d)", ov1x, ov2x);
					sprintf(cindex, "-%d-%d", ov1x, ov2x);
				}
				break;
			case 3:
				sprintf(index, "(%d %d %d)", ov1x, ov2x, ohx);
				sprintf(cindex, "-%d-%d-%d", ov1x, ov2x, ohx);
				break;
			}
				
			need_clr = 0;

			/*
			**	Write out the level number (always "05")
			*/
			tput_line_at(12, "05");

			/*
			**	The screen name field is not needed with Acucobol.
			**	If there was one on the Wang we write it out unless dim.
			*/
			if (this_item->name[0])
			{
				if ( 0==strcmp(this_item->name,"FILLER") )
				{
					/* Don't need to gen a FILLER name */
				}
				else
				{
					if (dim)
					{
						/*
						**	If this has a dimension then don't write the name out
						**	as the dim will cause duplicates.
						*/
					}
					else
					{
						tput_clause(16,"%s", this_item->name);
					}

					/*
					**	If named then need to generate COLOR & CONTROL clauses
					**	to replace FAC OF logic
					*/
					need_clr = 1;
					make_color(color_clause,this_item->name);
					make_control(ctrl_clause,this_item->name);
				}
			}

			tput_clause (16, "LINE %d", last_row);
			tput_clause (16, "COL %d",  last_col);

			if (this_item->source_clause && LITERAL == this_item->source_clause->token->type)
			{
				/* 
				**	VALUE clause can not have a PIC clause so don't write it out.
				**	(We could generate a SIZE clause?)
				*/
			}
			else
			{
				tput_clause (16, "PIC %s", this_item->pic);
			}

			/*
			**	If SOURCE and OBJECT are the same then generate a USING clause.
			*/
			use_using_clause = 0;
			if (this_item->source_clause && this_item->object_clause)
			{
				if (0 == strcmp(token_data(this_item->source_clause->token),
						token_data(this_item->object_clause->token)))
				{
					/*
					**	Only test the simple case of un-qualified names
					*/
					if (
					    /* NULL == this_item->source_clause->down &&
					       NULL == this_item->object_clause->down && */
					    NODE_END == this_item->source_clause->next->type &&
					    NODE_END == this_item->object_clause->next->type)
					{
						use_using_clause = 1;

						tput_line_at (16, "USING");

						if (dim)
						{
							tput_clause   (16, "%s", token_data(this_item->source_clause->token));
							tput_clause   (16, index);
							tput_statement(16, this_item->source_clause->next);
						}
						else
						{
							tput_statement(16,this_item->source_clause);
						}
					}
				}
			}
			
			if (this_item->source_clause && !use_using_clause)
			{
				/*
				**	If SOURCE clause is a literal then gen VALUE clause
				*/
				if (LITERAL == this_item->source_clause->token->type)
				{
					tput_clause (16, "VALUE");
				}
				else
				{
					tput_line_at (16, "FROM");
				}

				if (dim)
				{
					tput_clause   (16, "%s", token_data(this_item->source_clause->token));
					tput_clause   (16, index);
					tput_statement(16, this_item->source_clause->next);
				}
				else
				{
					tput_statement(16,this_item->source_clause);
				}
			}

			if (this_item->object_clause && !use_using_clause)
			{
				tput_line_at (16, "TO");

				if (dim)
				{
					tput_clause   (16, "%s", token_data(this_item->object_clause->token));
					tput_clause   (16, index);
					tput_statement(16, this_item->object_clause->next);
				}
				else
				{
					tput_statement(16,this_item->object_clause);
				}
			}

			/*
			**	If has FACs then generate the COLOR and CONTROL clause.
			*/
			if (need_clr)
			{
				tput_line_at (16, "COLOR   IS %s", addsuffix(color_clause, cindex));
				tput_line_at (16, "CONTROL IS %s", addsuffix(ctrl_clause, cindex));
			}
			else
			{
				if (this_item->object_clause)
				{
					/*
					**	No FAC field so hard-code a CONTROL clause
					*/
					tput_line_at (16, "CONTROL IS \"HIGH,UPPER,AUTO\"");
				}
			}

			tput_clause (16, ".");

		}
		
		
		
#ifdef OLD
/*
**	This was the first attempt that preserved the screen level structure.
*/
	int	scol;

		/*
		**	Loop for each item in this screen
		*/
		for(this_item = screen_item[scrn_idx]; this_item; this_item = this_item->next_item)
		{

			dim = 0;
			ov1 = ov2 = oh = 1;
			
			if (this_item->vert_1) 
			{
				dim++;
				ov1 = this_item->vert_1;

				/*
				**	vert_1 is set to -1 for a group item
				*/
				if (-1 == ov1) ov1 = 1;
			}
			if (this_item->vert_2) 
			{
				dim++;
				ov2 = this_item->vert_2;
			}
			if (this_item->horiz) 
			{
				dim++;
				oh = this_item->horiz;
			}
			assert(ov1 > 0);
			assert(ov2 > 0);
			assert(oh  > 0);

			for( ov1x=1; ov1x<=ov1; ov1x++)
			{
			for( ov2x=1; ov2x<=ov2; ov2x++)
			{
			for( ohx=1; ohx<=oh; ohx++)
			{
				switch(dim)
				{
				case 0:
					index[0] = '\0';
					cindex[0] = '\0';
					break;
				case 1:
					if (this_item->horiz)
					{
						sprintf(index, "(%d)", ohx);
						sprintf(cindex, "-%d", ohx);
					}
					else
					{
						sprintf(index, "(%d)", ov1x);
						sprintf(cindex, "-%d", ov1x);
					}
					break;
				case 2:
					if (this_item->horiz)
					{
						sprintf(index, "(%d %d)", ov1x, ohx);
						sprintf(cindex, "-%d-%d", ov1x, ohx);
					}
					else
					{
						sprintf(index, "(%d %d)", ov1x, ov2x);
						sprintf(cindex, "-%d-%d", ov1x, ov2x);
					}
					break;
				case 3:
					sprintf(index, "(%d %d %d)", ov1x, ov2x, ohx);
					sprintf(cindex, "-%d-%d-%d", ov1x, ov2x, ohx);
					break;
				}
				
				need_clr = 0;
			
				scol = this_item->x_level_col;
				tput_line_at(scol, "%02d", this_item->x_level);
			
				if (this_item->name[0])
				{
					if (dim && !this_item->pic[0])
					{
						/* Generate FILLER for group item if in OCCURS */
						tput_clause(scol+4,"FILLER");
					}

					if ( 0==strcmp(this_item->name,"FILLER") )
					{
						/* Don't need to gen a FILLER name unless this a group level occurs */
					}
					else
					{
						if (!dim)
						{
							tput_clause(scol+4,"%s", this_item->name);
						}

						/*
						**	If named then need to generate COLOR & CONTROL clauses
						**	to replace FAC OF logic
						*/
						need_clr = 1;
						make_color(color_clause,this_item->name);
						make_control(ctrl_clause,this_item->name);
					}
				}
			
				if (this_item->pic[0])
				{
					int	the_col, the_row;

					the_col = this_item->col;
					the_row = this_item->row;

					if (dim)
					{
						if (this_item->horiz)
						{
							the_col += (ohx - 1) * (this_item->size + 1);
						}
						if (this_item->vert_1)
						{
							the_row += (ov1x - 1) * this_item->vert_off1;

							if (this_item->vert_2)
							{
								the_row += (ov2x - 1) * this_item->vert_off2;
							}
						}
					}

					tput_clause (scol+4, "LINE %d", the_row);
					tput_clause (scol+4, "COL %d",  the_col);

					if (this_item->source_clause && LITERAL == this_item->source_clause->token->type)
					{
						/* VALUE can not have a PIC clause */
					}
					else
					{
						tput_clause (scol+4, "PIC %s", this_item->pic);
					}
				}

				if (this_item->x_occurs)
				{
					/*
					**	Don't need to gen OCCURS clause as we are un-rolling the repeated items.
					*/
				}

				if (this_item->source_clause)
				{
					if (LITERAL == this_item->source_clause->token->type)
					{
						tput_clause (scol+4, "VALUE");
					}
					else
					{
						tput_line_at (scol+4, "FROM");
					}

					if (dim)
					{
						tput_clause   (scol+4, "%s", token_data(this_item->source_clause->token));
						tput_clause   (scol+4, index);
						tput_statement(scol+4, this_item->source_clause->next);
					}
					else
					{
						tput_statement(scol+4,this_item->source_clause);
					}
				}

				if (this_item->object_clause)
				{
					tput_line_at (scol+4, "TO");

					if (dim)
					{
						tput_clause   (scol+4, "%s", token_data(this_item->object_clause->token));
						tput_clause   (scol+4, index);
						tput_statement(scol+4, this_item->object_clause->next);
					}
					else
					{
						tput_statement(scol+4,this_item->object_clause);
					}
				}

				if (this_item->pic[0])
				{
					if (need_clr)
					{
						tput_line_at (scol+4, "COLOR   IS %s", addsuffix(color_clause, cindex));
						tput_line_at (scol+4, "CONTROL IS %s", addsuffix(ctrl_clause, cindex));
					}
					else
					{
						if (this_item->object_clause)
						{
							tput_line_at (scol+4, "CONTROL IS \"HIGH,UPPER,AUTO\"");
						}
					}
				}
			
				tput_clause (scol+4, ".");

			}
			}
			}

		}
#endif /* OLD */

	}

	tput_blank();

	write_log("WISP",'I',"SCREENDONE","Finished generating screen records.");
	return 0;
}

/*
**	Sub 0 is default for protected fields
**	Sub 1 is default for modifiable fields
*/
static char *acu_color_name[]   = { "WCLR-PROTECT", "WCLR-BRIGHT" };
static char *acu_control_name[] = { "SPACES",       "\"UPPER,AUTO\"" };


/*
**	Generate the WORKING-STORAGE screen control fields.
**	This includes some canned fields plus the FAC, COLOR, and CONTROL fields.
*/
int gen_acn_facs(void)
{
	static  int facnum = 0;
	int 	i,fac_idx,m;
	char	the_orderarea[80];
	char	the_fac[80];
	char	the_color[80];
	char	the_control[80];
	int	col,level;

	int	ov1, ov2, oh;
	int	ov1x, ov2x, ohx;
	int	dim;
	char	index[40];


	write_log("WISP",'I',"SCREENFAC","Generating SCREEN FACs.");

	tput_blank();
	tput_scomment("*******");
	tput_scomment("******* WISP generated screen control fields.");
	tput_scomment("*******");

	tput_blank();
	tput_scomment( "*    Special-names CURSOR clause.");
	tput_line_at(8, "01  WISP-CURSOR.");
	tput_line_at(8, "    05  WISP-CURSOR-LINE    PIC 999 VALUE 1.");
	tput_line_at(8, "    05  WISP-CURSOR-COL     PIC 999 VALUE 1.");

	tput_blank();
	tput_scomment( "*    Special-names CRT STATUS clause.");
	tput_line_at(8, "01  WISP-CRT-STATUS.");
	tput_line_at(8, "    05  WISP-CRT-KEY-1               PIC X.");
	tput_line_at(8, "        88  WISP-CRT-STATUS-TERMINATED  VALUE '0'.");
	tput_line_at(8, "        88  WISP-CRT-STATUS-EXCEPTION   VALUE '1'.");
	tput_line_at(8, "        88  WISP-CRT-STATUS-EOF         VALUE '2'.");
	tput_line_at(8, "        88  WISP-CRT-STATUS-TIMEOUT     VALUE '3'.");
	tput_line_at(8, "        88  WISP-CRT-STATUS-NOITEM      VALUE '9'.");
	tput_line_at(8, "    05  WISP-CRT-KEY-EXCEPTION       PIC X COMP-X.");
	tput_line_at(8, "        88  WISP-CRT-EX-HELP            VALUE 33.");
	tput_line_at(8, "        88  WISP-CRT-EX-GETKEY          VALUE 34.");
	tput_line_at(8, "        88  WISP-CRT-KEY-AUTOSKIP       VALUE 49.");
	tput_line_at(8, "    05  WISP-CRT-KEY-3               PIC X COMP-X.");		       
	tput_line_at(8, "        88  WISP-CRT-KEY-TAB            VALUE  9.");
	tput_line_at(8, "        88  WISP-CRT-KEY-ENTER          VALUE 13.");

	tput_blank();
	tput_scomment( "*    Special-names SCREEN CONTROL clause.");
	tput_line_at(8, "01  WISP-SCREEN-CONTROL.");
	tput_line_at(8, "    05  WISP-ACCEPT-CONTROL     PIC 9   VALUE 0.");
	tput_line_at(8, "    05  WISP-CONTROL-VALUE      PIC 999 VALUE 1.");
	tput_line_at(8, "    05  WISP-CONTROL-HANDLE     USAGE HANDLE.");
	tput_line_at(8, "    05  WISP-CONTROL-ID         PIC XX COMP-X.");

	tput_blank();
	tput_scomment( "*    Fields for DISPLAY statements.");
	tput_line_at(8, "01  WISP-DISPLAY-FIELDS-DATA PIC X(1185).");
	tput_line_at(8, "01  FILLER REDEFINES WISP-DISPLAY-FIELDS-DATA.");
	tput_line_at(8, "    05  WISP-DISPLAY-FIELDS OCCURS 15 PIC X(79).");

	if (num_screens < 1)
	{
		return(0);
	}

	tput_blank();
	tput_scomment( "*    Screen items COLOR phrase values.");
	tput_line_at(8, "78  WCLR-REVERSE     VALUE  1024.");
	tput_line_at(8, "78  WCLR-BRIGHT      VALUE  4096.");
	tput_line_at(8, "78  WCLR-UNDERLINE   VALUE  8192.");
	tput_line_at(8, "78  WCLR-BLINK       VALUE 16384.");
	tput_line_at(8, "78  WCLR-PROTECT     VALUE 32768.");
	tput_line_at(8, "78  WCLR-ERROR       VALUE 17408.");

	tput_blank();
	tput_scomment( "*    WISP workstation working items.");
	tput_line_at(8, "01  WISP-PFKEY                 PIC 99.");
	tput_line_at(8, "    88  WISP-PFKEY-ENTER       VALUE  0.");
	tput_line_at(8, "    88  WISP-PFKEY-HELP        VALUE 33.");
	tput_line_at(8, "    88  WISP-PFKEY-INVALID     VALUE 99.");
	tput_line_at(8, "01  WISP-CURSOR-POSITION.");
	tput_line_at(8, "    05  WISP-CURSOR-POSITION-COL COMP-1 PIC S9(4).");
	tput_line_at(8, "    05  WISP-CURSOR-POSITION-ROW COMP-1 PIC S9(4).");

	tput_blank();
	tput_scomment( "*    WISP DISPLAY AND READ working items.");
	tput_line_at(8, "01  WISP-ALLOWABLE-PF-KEYS-CNT PIC 99.");
	tput_line_at(8, "01  WISP-ON-PF-KEYS-CNT        PIC 99.");
	tput_line_at(8, "01  WISP-DNR-ON-PFKEY          PIC X VALUE \"N\".");
	tput_line_at(8, "01  WISP-DNR-NO-MOD            PIC X VALUE \"N\".");
	tput_line_at(8, "01  WISP-DNR-RANGE-ERROR       PIC X VALUE \"N\".");

	tput_line_at(8, "01  WISP-DNR-ORDER-AREA.");
	tput_line_at(8, "    05  WISP-DNR-ROW         PIC X COMP-X VALUE 1.");
	tput_line_at(8, "    05  WISP-DNR-WCC         PIC X COMP-X VALUE 160.");
	tput_line_at(8, "        88  WISP-DNR-WCC-UNLOCK-KEYBOARD  VALUES");
	tput_line_at(8, "            128 THRU 255.");
	tput_line_at(8, "        88  WISP-DNR-WCC-SOUND-ALARM      VALUES ");
	tput_line_at(8, "             64 THRU 127, 192 THRU 255.");
	tput_line_at(8, "        88  WISP-DNR-WCC-POSITION-CURSOR  VALUES");
	tput_line_at(8, "             32 THRU  63,  96 THRU 127,");
	tput_line_at(8, "            160 THRU 171, 224 THRU 255.");
	tput_line_at(8, "    05  WISP-DNR-CURSOR-COL  PIC X COMP-X VALUE 0.");
	tput_line_at(8, "    05  WISP-DNR-CURSOR-ROW  PIC X COMP-X VALUE 0.");

	tput_line_at(8, "01  WISP-DNR-WCC-CURSOR-BIT  PIC X VALUE X\"20\".");
	tput_line_at(8, "01  WISP-DNR-WCC-ALARM-BIT   PIC X VALUE X\"40\".");

	/*
	**	Loop for each screen 
	*/
	for (i=0; i<num_screens; i++)					
	{
		write_log("WISP",'I',"FACS","Generating FACs for screen %d %s.",i,scrn_name[i]);

		tput_blank();
		tput_scomment("******* Generated fields for screen %s *******", scrn_name[i]);

		tput_blank();

		make_oa(the_orderarea,scrn_name[i]);
		tput_line_at(8,  "01  %s.",the_orderarea);
		tput_line_at(12, "05  FILLER         PIC X  VALUE DEC-BYTE-1.");
		tput_line_at(12, "05  FILLER         PIC X  VALUE WISP-SCWCC.");
		tput_line_at(12, "05  FILLER         PIC X  VALUE DEC-BYTE-0.");
		tput_line_at(12, "05  FILLER         PIC X  VALUE DEC-BYTE-0.");

		last_item = this_item;

		/*
		**	Loop for each item in this screen
		*/
		for(this_item = screen_item[i]; this_item; this_item = this_item->next_item)
		{
			if (!this_item->pic[0])
			{
				/* Group items don't need FAC fields */
				continue;
			}
			if ( strcmp(this_item->name,"FILLER") == 0 )
			{
				/* FILLER items don't need FAC fields */
				continue;
			}

			tput_blank();

			make_fac(the_fac,this_item->name);
			make_color(the_color,this_item->name);
			make_control(the_control,this_item->name);

			if (this_item->object_clause)
			{
				fac_idx = 1;
			}
			else
			{
				fac_idx = 0;
			}

			dim = 0;
			ov1 = ov2 = oh = 1;
			
			if (this_item->vert_1) 
			{
				dim++;
				ov1 = this_item->vert_1;
			}
			if (this_item->vert_2) 
			{
				dim++;
				ov2 = this_item->vert_2;
			}
			if (this_item->horiz) 
			{
				dim++;
				oh = this_item->horiz;
			}
			
			/*
			**
			**	01  FAC-OF-XXX  PIC X(v1*v2*h) ALL WFAC-MODIFY.
			**	01  REDEFINES FAC-OF-XXX.
			**	    05  OCCURS v1.
			**		10  OCCURS v2.
			**		    15  OCCURS h.
			**		        20  F-XXX PIC X.
			**	
			*/
			/* if the item is a multiple item...	*/
			if (dim)
			{
				facnum++;							/* need a new fac number	*/

				m = ov1 * ov2 *oh;
				
				/*
				**	Generate FAC fields
				*/
				tput_line_at(8, "01  FAC-NUMBER-%d PIC X(%d) VALUE ALL %s.",facnum,m,fac_name[fac_idx]);
				tput_line_at(8, "01  FILLER REDEFINES FAC-NUMBER-%d.",facnum);

				col = 12;
				level = 5;

				if (this_item->vert_1)				/* has a vertical dimension			*/
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->vert_1);
					col += 4;
					level += 5;
				}
				
				if (this_item->vert_2)				/* write the second vertical dimension	*/
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->vert_2);
					col += 4;
					level += 5;
				}

				if (this_item->horiz)
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->horiz);
					col += 4;
					level += 5;
				}

				tput_line_at(col, "%02d  %s PIC X.", level, the_fac);


				/*
				**	Generate COLOR fields
				*/

				tput_line_at(8, "01  W-COLOR-NUMBER-%d.", facnum);

				for( ov1x=1; ov1x<=ov1; ov1x++)
				{
					for( ov2x=1; ov2x<=ov2; ov2x++)
					{
						for( ohx=1; ohx<=oh; ohx++)
						{
							switch(dim)
							{
							case 0:
								index[0] = '\0';
								break;
							case 1:
								if (this_item->horiz)
								{
									sprintf(index, "-%d", ohx);
								}
								else
								{
									sprintf(index, "-%d", ov1x);
								}
								break;
							case 2:
								if (this_item->horiz)
								{
									sprintf(index, "-%d-%d", ov1x, ohx);
								}
								else
								{
									sprintf(index, "-%d-%d", ov1x, ov2x);
								}
								break;
							case 3:
								sprintf(index, "-%d-%d-%d", ov1x, ov2x, ohx);
								break;
							}
							tput_line_at(12, "05  %s PIC 9(5)", addsuffix(the_color, index));
							tput_clause(16,  "VALUE %s.", acu_color_name[fac_idx]);
						}
					}
				}
				
				tput_line_at(8, "01  FILLER REDEFINES W-COLOR-NUMBER-%d.", facnum);

				col = 12;
				level = 5;

				if (this_item->vert_1)				/* has a vertical dimension			*/
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->vert_1);
					col += 4;
					level += 5;
				}
				
				if (this_item->vert_2)				/* write the second vertical dimension	*/
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->vert_2);
					col += 4;
					level += 5;
				}

				if (this_item->horiz)
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->horiz);
					col += 4;
					level += 5;
				}

				tput_line_at(col, "%02d  %s PIC 9(5).", level, the_color);


				/*
				**	Generate CONTROL fields
				*/
				tput_line_at(8, "01  W-CONTROL-NUMBER-%d.", facnum);
				for( ov1x=1; ov1x<=ov1; ov1x++)
				{
					for( ov2x=1; ov2x<=ov2; ov2x++)
					{
						for( ohx=1; ohx<=oh; ohx++)
						{
							switch(dim)
							{
							case 0:
								index[0] = '\0';
								break;
							case 1:
								if (this_item->horiz)
								{
									sprintf(index, "-%d", ohx);
								}
								else
								{
									sprintf(index, "-%d", ov1x);
								}
								break;
							case 2:
								if (this_item->horiz)
								{
									sprintf(index, "-%d-%d", ov1x, ohx);
								}
								else
								{
									sprintf(index, "-%d-%d", ov1x, ov2x);
								}
								break;
							case 3:
								sprintf(index, "-%d-%d-%d", ov1x, ov2x, ohx);
								break;
							}
							tput_line_at(12, "05  %s PIC X(20)", addsuffix(the_control, index));
							tput_clause( 16, "VALUE %s.", acu_control_name[fac_idx]);
						}
					}
				}
				tput_line_at(8, "01  FILLER REDEFINES W-CONTROL-NUMBER-%d.", facnum);

				col = 12;
				level = 5;

				if (this_item->vert_1)				/* has a vertical dimension			*/
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->vert_1);
					col += 4;
					level += 5;
				}
				
				if (this_item->vert_2)				/* write the second vertical dimension	*/
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->vert_2);
					col += 4;
					level += 5;
				}

				if (this_item->horiz)
				{
					tput_line_at(col, "%02d  FILLER OCCURS %d.", level, this_item->horiz);
					col += 4;
					level += 5;
				}

				tput_line_at(col, "%02d  %s PIC X(20).", level, the_control);


			}
			else
			{
				tput_line_at(8, "01  %-20s PIC X     VALUE %s.", the_fac,     fac_name[fac_idx]);
				tput_line_at(8, "01  %-20s PIC 9(5)  VALUE %s.", the_color,   acu_color_name[fac_idx]);
				tput_line_at(8, "01  %-20s PIC X(20) VALUE %s.", the_control, acu_control_name[fac_idx]);
			}

			last_item = this_item;
		}
	}

	tput_blank();

	write_log("WISP",'I',"FACDONE","Finished generating screen FACs.");
	return 0;
}

void make_color(char* the_color, char* src)
{
	make_fld(the_color,src,"WCLR-");
}

void make_control(char* the_ctrl, char* src)
{
	make_fld(the_ctrl,src,"WCTR-");
}

/*---*/

/*
**	Calculate the vertical offsets for OCCURS items in a screen
**
**	This is called after a full screen has been parsed.
**	It loads the vertical offsets and the size into the item_records
**	for each screen field that is part of an OCCURS.
**
**	This code was copied from wscreen.c.
*/
static void calc_occurs_offsets(item_record* the_item)
{
	struct
	{
		item_record* it_ir;
	
		short	it_v1;						/* Vertical(1) occurs.					*/
		short	it_v1_off;					/* Vertical(1) offset. (Number of rows)			*/
		short	it_v2;						/* Vertical(2) occurs.					*/
		short	it_v2_off;					/* Vertical(2) offset. (Number of rows)			*/
		short	it_h;						/* Horizontal occurs.					*/
		short	it_size;					/* Size of one element.					*/
		short	it_row;						/* Starting row.					*/
		short	it_col;						/* Starting column					*/

	} item_table[MAX_ITEM_TABLE];

	int 	num_items;
	int	i;

	struct v_struct
	{
		int	v_occurs;					/* Number of occurs.					*/
		int	v_level;					/* Level number of occurs.				*/
		int	v_item;						/* Item number of start of occurs.			*/
		int	v_low;						/* Lowest row number.					*/
		int	v_high;						/* Highest row number.					*/
	};

	struct v_struct v1, v2;

	int	this_level;					/* Current level number.					*/
	int	this_row;					/* Current row number.						*/
	int	this_col;					/* Current col number.						*/
	int	this_occurs;					/* Current occurs number.					*/
	char	this_pic[80];					/* Ptr to current pic.						*/

	int	curr_row, curr_col;				/* The current calculated row & col.				*/
	int	offset;


	/*
	**	Initialize variables
	*/

	num_items = 0;

	v1.v_occurs = 0;
	v2.v_occurs = 0;

	curr_row = 1;
	curr_col = 1;

	this_level = the_item->x_level;

	while ( the_item )							/* Loop until SCREEN-END.			*/
	{

		this_row = the_item->x_row;
		this_col = the_item->x_col;
		this_occurs = the_item->x_occurs;
		strcpy(this_pic, the_item->pic);
		
		/* Process occurs */

		if ( this_occurs && !this_pic[0] )
		{
			if ( v1.v_occurs )
			{
				v2.v_occurs = this_occurs;
				v2.v_level  = this_level;
				v2.v_item   = num_items;
				v2.v_low    = 25;
				v2.v_high   = 0;
			}
			else
			{
				v1.v_occurs = this_occurs;
				v1.v_level  = this_level;
				v1.v_item   = num_items;
				v1.v_low    = 25;
				v1.v_high   = 0;
			}
		}

		/* Process row & col */

		if ( this_row )
		{
			curr_row = this_row;
			if ( v1.v_occurs )
			{
				v1.v_low  = (v1.v_low  < curr_row) ? v1.v_low  : curr_row;
				v1.v_high = (v1.v_high > curr_row) ? v1.v_high : curr_row;
			}
			if ( v2.v_occurs )
			{
				v2.v_low  = (v2.v_low  < curr_row) ? v2.v_low  : curr_row;
				v2.v_high = (v2.v_high > curr_row) ? v2.v_high : curr_row;
			}
		}
		if ( this_col )
		{
			curr_col = this_col;
		}

		if ( this_pic[0] )
		{
			item_table[num_items].it_ir      = the_item;
			
			item_table[num_items].it_v1      = (v1.v_occurs) ? v1.v_occurs : 1;
			item_table[num_items].it_v2      = (v2.v_occurs) ? v2.v_occurs : 1;
			item_table[num_items].it_v1_off  = 1;
			item_table[num_items].it_v2_off  = 1;
			item_table[num_items].it_h       = (this_occurs) ? this_occurs : 1;
			item_table[num_items].it_row     = (curr_row<1 || curr_row>24) ? 1 : curr_row;
			item_table[num_items].it_col     = (curr_col<1 || curr_col>80) ? 1 : curr_col;

			item_table[num_items].it_size    = pic_size(this_pic);

			num_items += 1;
			if ( num_items >= MAX_ITEM_TABLE )
			{
				write_log("WISP",'F',"MAXSCREENFIELDS","Exceeded %d number of screen fields.",MAX_ITEM_TABLE);
				return;
			}
		}

		the_item = the_item->next_item;					/* point to the next item...			*/

		if (the_item)
		{
			this_level = the_item->x_level;
		}
		else
		{
			this_level = 0;						/* Force closure of open occurs.		*/
		}

		/* close_open_occurs( this_level, &v1, &v2 ); */		/* Close any open vert occurs.			*/

		if ( v2.v_occurs && this_level <= v2.v_level )
		{
			offset = v2.v_high - v2.v_low + 1;				/* Calc the v2 offset.			*/
			while( v2.v_item < num_items )
			{
				item_table[v2.v_item++].it_v2_off = offset;		/* Load the offset into item table.	*/
			}
			v2.v_high = (offset * v2.v_occurs) + v2.v_low - 1;		/* Calc the true v2 high row.		*/
			v1.v_high = (v1.v_high > v2.v_high) ? v1.v_high : v2.v_high; 	/* Adjust v1 high row.			*/
			v2.v_occurs = 0;						/* Finished with v2 occurs.		*/
		}

		if ( v1.v_occurs && this_level <= v1.v_level )
		{
			offset = v1.v_high - v1.v_low + 1;				/* Calc the v1 offset.			*/
			while( v1.v_item < num_items )
			{
				item_table[v1.v_item++].it_v1_off = offset;		/* Load the offset into item table.	*/
			}
			v1.v_occurs = 0;						/* Finished with v1 occurs.		*/
		}

	}

	/*
	**	Load the occurs offsets into the item_records
	*/
	for(i=0; i<num_items; i++)
	{
		item_table[i].it_ir->vert_off1 = item_table[i].it_v1_off;
		item_table[i].it_ir->vert_off2 = item_table[i].it_v2_off;
		item_table[i].it_ir->size      = item_table[i].it_size;

		if (item_table[i].it_ir->vert_1 != 0 && item_table[i].it_v1 != 1 &&
		    item_table[i].it_ir->vert_1 != item_table[i].it_v1)
		{
			write_log("WISP",'E',"VERT1","Vert 1 offset disagrees");
		}
		if (item_table[i].it_ir->vert_2 != 0 && item_table[i].it_v2 != 1 &&
		    item_table[i].it_ir->vert_2 != item_table[i].it_v2)
		{
			write_log("WISP",'E',"VERT2","Vert 2 offset disagrees");
		}
	}
}

/*
**	Add the suffix to the field name without going over 30 characters.
*/
char* addsuffix(const char* fld, const char* suff)
{
	static 	char newfld[80];
	int	sufflen, fldlen;

	strcpy(newfld,fld);
	
	sufflen = strlen(suff);
	fldlen  = strlen(fld);


	if (fldlen+sufflen > 30)
	{
		/*
		**	Truncate the field so the suffix will fit.
		*/
		newfld[30-sufflen] = '\0';
	}
	strcat(newfld,suff);
	assert(strlen(newfld)<=30);

	return newfld;
}


/*
**	History:
**	$Log: wt_scrn.c,v $
**	Revision 1.17  1997-09-30 10:56:38-04  gsl
**	Fix warnings
**
**	Revision 1.16  1997-09-22 09:47:07-04  gsl
**	ACN: Improve POSITION CURSOR logic
**	Add support for SOUND ALARM and UNLOCK KEYBOARD bits in WCC
**	Moved canned DISPLAY and READ setup logic into a SETUP para.
**
**	Revision 1.15  1997-09-19 16:14:41-04  gsl
**	ACN: Add support of AUTOSKIP terminate code
**	ACN: Add HELP key and generic function key entry support
**	ACN: Detect and report unrecognized CRT status codes
**
**	Revision 1.14  1997-09-15 13:44:00-04  gsl
**	Fix duplicate screen positions warning
**	Fix the DISPLAY fields
**
**	Revision 1.13  1997-09-15 11:16:25-04  gsl
**	Add native screens translation for DISPLAY verb
**
**	Revision 1.12  1997-09-09 17:51:46-04  gsl
**	Add first pass at ACUCOBOL Native Screens
**
**	Revision 1.11  1997-01-20 18:55:54-05  gsl
**	Handle an empty DISPLAY-WS screen condition.
**
**	Revision 1.10  1996-08-30 18:56:24-07  gsl
**	drcs update
**
**
**
*/
