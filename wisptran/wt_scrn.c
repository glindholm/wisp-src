			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wisp_scrn.c
*/

#include <string.h>

#define EXT extern
#include "wisp.h"

#define PROC_PICTURE		0
#define PROC_ROW		8
#define PROC_LINE		12
#define PROC_COLUMN		17
#define PROC_RANGE		24
#define PROC_SOURCE		30
#define PROC_VALUE		37
#define PROC_OBJECT		43
#define PROC_OCCURS		50

char	decimal_is = '.';

extern long pic_edit();
extern int mwconv_flag;

static int mem_cnt = OUTPUT_BUFFER_SIZE;						/* keep track of all malloc's		*/

p_item()										/* parse out parms in a screen item.	*/
{
	int i,j,ii,num;
	int occurs_val;
	item_record *temp_ptr;
	unsigned m_size;
	static	int	cur_v1_level;
	static  int	cur_v2_level;
	static	int	cur_level;

	occurs_val = 0;									/* No OCCURS value found yet		*/

	if (!pmode)
	{
		write_log("WISP",'I',"PROCSCREEN","Processing screen %s",parms[1]);

		if (parms[1][strlen(parms[1])-1] == '.')
		{
			parms[1][strlen(parms[1])-1] = 0;				/* erase period				*/
		}

		if (num_screens > MAX_SCREENS)						/* do we have room for more?		*/
		{
			write_log("WISP",'F',"NOSCRNROOM","Fatal Error -- maximum number of screens exceeded.");
			exit_wisp(-1);
		}

		strcpy(scrn_name[num_screens],parms[1]);				/* Save the name for this screen	*/
											/* Get mem for first record		*/
		m_size = sizeof(item_record);
		temp_ptr = (item_record *) malloc(m_size);
		if (temp_ptr == 0)
		{
			printf("%WISP-F-ERRORALLOC  Error allocating %d bytes of memory for first screen item.\n",m_size);
			printf("                    Actual allocated so far = %d.\n",mem_cnt);
			exit(0);
		}
		clear_item(temp_ptr);							/* Init the fields in the item		*/
		mem_cnt += m_size;
		screen_item[num_screens] = temp_ptr;
		this_item = screen_item[num_screens];					/* Pointer to current item to fill	*/
		last_item = this_item;
		num_screens++;								/* Count this screen			*/
		pmode = WS_SCREEN;							/* Set the mode so we can be re-called 	*/
		
		cur_row = 1;
		cur_col = 1;

		get_line();								/* read in a new line			*/
		sscanf(&inline[7],"%d",&item_level);					/* this is the level number of an item	*/
	}
	else
	{										/* Get a ptr to a new item record	*/
		m_size = sizeof(item_record);
		temp_ptr = (item_record *) malloc(m_size);
		if (temp_ptr == 0)
		{
			printf("%WISP-F-ERRORALLOC  Error allocating %d bytes of memory for screen item.\n",m_size);
			printf("                    Actual allocated so far = %d.\n",mem_cnt);
			exit(0);
		}
		clear_item(temp_ptr);							/* Init the fields in the item		*/
		mem_cnt += m_size;
		this_item->next_item = temp_ptr;
		last_item = this_item;							/* Save this item ptr			*/
		this_item = this_item->next_item;					/* and point to it !!			*/
	}

	sscanf(&inline[7],"%d",&i);							/* get the level number of this item	*/

	if (item_level == i)								/* It's a new item, reset OCCURS	*/
	{
											/* See if there was a group OCCURS.	*/
		if (occurs_item) p_occurs(last_item);					/* If there was, process it up to the	*/
											/* Last item.				*/
		cur_v1 = 0;
		cur_v2 = 0;
		cur_h  = 0;
		cur_v1_level = 0;
		cur_v2_level = 0;
	}
	else
	{
		if (cur_v1_level >= i)
		{
			cur_v1 = 0;
			cur_v2 = 0;
			cur_v1_level = 0;
			cur_v2_level = 0;
		}
		if (cur_v2_level >= i)
		{
			cur_v2 = 0;
			cur_v2_level = 0;
		}
	}

	clear_item(this_item);								/* Init the fields in the item		*/
	r_count = 0;									/* set current range count to 0.	*/

	get_param(this_item->item_num);							/* Parse out the item_number		*/
	ptype = get_param(this_item->name);						/* get the variable name		*/
	sscanf(this_item->item_num,"%d",&cur_level);					/* get the level number of this item	*/
	this_item->x_level = cur_level;

	if (ptype == -1)								/* this is the start of a record, for	*/
	{										/* now we will discard it		*/
		get_line();								/* get a new line			*/
		get_param(this_item->item_num);						/* Parse out the item_number		*/
		ptype = get_param(this_item->name);					/* get the variable name		*/
		sscanf(this_item->item_num,"%d",&cur_level);				/* get the level number of this item	*/
	}

	this_item->col = last_item->col  + pic_size(last_item->pic) + 1;		/* initially set the column to 1 + size	*/
	this_item->row = last_item->row;						/* initially set the row to last row	*/
	ptype = get_param(templine);							/* get the first keyword		*/

	do
	{
			 /*           1         2         3         4         5         6	*/
                         /* 0123456789012345678901234567890123456789012345678901234567890	*/
		i = strpos("PICTURE ROW LINE COLUMN RANGE SOURCE VALUE OBJECT OCCURS",templine);

		switch (i)								/* do the appropriate thing		*/
		{
			case PROC_PICTURE:						/* process "PICTURE			*/
			{
				ptype = get_param(this_item->pic);			/* Get actual PIC parameter		*/
				if (!strcmp(this_item->pic,"IS")) ptype = get_param(this_item->pic);	/* skip over IS		*/

								/* this section of code will extract a PIC from an input line	*/
				i = get_ppos();						/* find out where the PIC is		*/
				j = 0;							/* index into pic string		*/
				while (inline[i] && (inline[i] != '\n') && (inline[i] != ' '))	/* scan till newline or space	*/
				{
					if (inline[i] == ',') ptype = get_param(templine);	/* if a comma, get next parm	*/
					this_item->pic[j++] = inline[i++];		/* copy the PIC				*/
				}
				this_item->pic[j] = 0;					/* null terminate			*/
								/* end of PIC extraction code					*/

				if (ptype != -1) ptype = get_param(templine);		/* and get a new parm			*/
				break;
			}

			case PROC_ROW:							/* Process "ROW"			*/
			case PROC_LINE:							/* or "LINE"				*/
			{
				ptype = get_param(templine);				/* Actually read in a ROW value		*/
				sscanf(templine,"%d",&num);
				this_item->row   = num;
				this_item->x_row = num;
				if (ptype != -1) ptype = get_param(templine);		/* and get a new parm			*/
				break;
			}

			case PROC_COLUMN:						/* Process "COLUMN"			*/
			{
				ptype = get_param(templine);				/* Actually read in the COLUMN value	*/
				sscanf(templine,"%d",&num);
				this_item->col   = num;
				this_item->x_col = num;
				if (ptype != -1) ptype = get_param(templine);		/* and get a new parm			*/
				break;
			}

			case PROC_RANGE:						/* process RANGE			*/
			{
				this_item->num_range = r_count;				/* use current range count		*/

				ptype = get_param(templine);				/* Get a parameter			*/

				if (!strcmp(templine,"IS"))				/* skip over IS				*/
					ptype = get_param(templine);

				if (!strcmp(templine,"FROM"))				/* skip over FROM			*/
				{
					ptype = get_param(templine);		
				}

				strcpy(this_item->lo_range,templine);			/* this must be the low range now..	*/

				if (templine[0] == '\"' && (strpos(&templine[1],"\"") == -1))	/* Is it an open literal?	*/
				{
					do						/* Get strings till we see the end.	*/
					{
						ptype = get_param(templine);
						strcat(this_item->lo_range,templine);	/* Add it on...				*/
					} while(strpos(templine,"\"") == -1);
				}

				if (ptype != -1) ptype = get_param(templine);		/* and get a new parm			*/

				if (!strcmp(templine,"TO"))				/* If TO, skip it, get hi range		*/
				{
					ptype = get_param(this_item->hi_range);

											/* Is it an open literal?		*/
					if (this_item->hi_range[0] == '\"' && (strpos(&this_item->hi_range[1],"\"") == -1))
					{
						do					/* Get strings till we see the end*/
						{
							ptype = get_param(templine);
							strcat(this_item->hi_range,templine);	/* Add it on...			*/
						} while(strpos(templine,"\"") == -1);
					}
					if (ptype != -1) ptype = get_param(templine);	/* and get a new parm			*/
				}

				write_log("WISP",'I',"RANGEVALLOW",
						"RANGE for item %s, Low value is %s.",this_item->name,this_item->lo_range);
				write_log("WISP",'I',"RANGEVALHI","High value is %s.",this_item->hi_range);
				break;
			}

			case PROC_SOURCE:						/* Do SOURCE or VALUE			*/
			case PROC_VALUE:
			{
				int	len_source, max_source, tmp_len;

				max_source = sizeof(this_item->source) - 1;
				len_source = 0;

				ptype = get_param(templine);				/* First parm is first word in constant	*/
											/* or actual variable name		*/
				if (!strcmp(templine,"IS"))				/* look for IS and discard it.		*/
				{
					ptype = get_param(templine);
				}

				strcpy(this_item->source,templine);			/* Copy it into source string		*/
				i = strlen(templine);					/* Get length of parm.			*/
				if (templine[0] == QUOTE_CHAR)				/* It is a literal.			*/
				{
					ptype = 0;
					i = strpos(inline,templine);			/* look for param in inline		*/
					j = i;						/* Start of active line			*/
					memset(this_item->source,' ',i);		/* set up spaces			*/
					strcpy(&this_item->source[i],&inline[i]);
					len_source = strlen(this_item->source);

					for(ii=1;;ii++)
					{
						if (!this_item->source[i+ii])		/* End of line QUOTE not found		*/
						{
							get_line();			/* read in next line			*/

							tmp_len = strlen(inline);
							if (len_source+tmp_len > max_source)
							{
								inline[max_source - len_source] = '\0';
							}
							strcat(this_item->source,inline);	/* get the rest			*/
							len_source += strlen(inline);
							j = strpos(inline,QUOTE_STR);		/* look for the open quote	*/
							i += j+ii;			/* Offset into source			*/
							ii = 1;				/* Offset from j in active line		*/
						}

						if (this_item->source[i+ii] == QUOTE_CHAR)
						{
							if (this_item->source[i+ii+1] == QUOTE_CHAR)    /* Embedded quote	*/
							{
								ii += 1;
							}
							else						/* Ending quote		*/
							{
								this_item->source[i+ii+1] = 0;
								len_source = strlen(this_item->source);
								memset(inline,' ',j+ii+1);	/* Now clear from the inline.	*/
								hold_line();			/* Hold line in case more data.	*/
								break;
							}
						}
					}

					i = strlen(this_item->source)-1;		/* pointer to last char position	*/
											/* remove newline			*/
					if (this_item->source[i] == '\n') this_item->source[i--]= '\0';

					if (this_item->source[i] == '.')
					{
						ptype = -1;				/* flag last parm, remove the period.	*/
						this_item->source[i--] = '\0';
					}
					else						/* No period? must not be last parm	*/
					{						/* get another line			*/
						get_line();
					}

					if (this_item->source[i] == ',')
					{
						this_item->source[i--] = '\0';
					}
				}
				if (ptype != -1) ptype = get_param(templine);		/* and get a new parm		*/
				break;
			}

			case PROC_OBJECT:						/* do OBJECT				*/
			{
				ptype = get_param(this_item->object);
				if (!strcmp(this_item->object,"IS"))
				{
					ptype = get_param(this_item->object);
				}
				if (ptype != -1) ptype = get_param(templine);		/* and get a new parm		*/
				break;
			}

			case PROC_OCCURS:						/* do OCCURS				*/
			{
				ptype = get_param(o_parms[0]);				/* get the count as a string		*/
				sscanf(o_parms[0],"%d",&occurs_val);			/* convert it to an integer		*/
				this_item->x_occurs = occurs_val;
				if (ptype != -1)
				{
					ptype = get_param(templine);			/* and get the TIMES keyword		*/
					if ( !(strcmp(templine,"TIMES") && strcmp(templine,"TIME")))	/* It was there.	*/
					{
						if (ptype != -1) ptype = get_param(templine);	/* so get a new parm		*/
					}
				}
				if (!occurs_item) occurs_item = this_item;		/* Save ptr to this item if first.	*/
				break;
			}

			default:							/* some kind of error			*/
			{
				if (templine[0])					/* be sure it wasn't a null string	*/
				{
					write_log("WISP",'F',"ERRINITEM","Error in screen item, <%s>, line is\n%s",
									templine,inline);
					exit_wisp(-1);
				}
			}
		}
	} while (ptype != -1);								/* quit when no matches			*/

	if (occurs_val)
	{
		if (this_item->pic[0])							/* there was a PIC, horizontal OCCURS	*/
		{
			this_item->horiz = occurs_val;					/* save it				*/
		}
		else if (cur_v1)							/* already a vertical value		*/
		{
			cur_v2 = occurs_val;						/* must be the second one		*/
			cur_v2_level = cur_level;
			this_item->vert_1 = -1;						/* This is really a group item, not used*/
		}
		else
		{									/* it's the first vertical value	*/
			cur_v1 = occurs_val;
			cur_v1_level = cur_level;
			this_item->vert_1 = -1;						/* This is really a group item, not used*/
		}
	}
	else if (!this_item->pic[0])
	{
		this_item->vert_1 = -1;							/* No OCCURS and no PIC means group item.*/
	}
									/* If it is a filler  convert it into a variable of the	*/
									/* form SCREEN-FILLER-NNNN if the following is true:	*/
									/* It has an OBJECT, or it has a non-literal source	*/

	if ( !strcmp(this_item->name,"FILLER") &&
	     (this_item->object[0] || 
	     ( this_item->source[0] && (strpos(this_item->source,"\042") == -1))))
	{
		sprintf(this_item->name,"SCREEN-FILLER-%d",fillernum++);
		write_log("WISP",'I',"CONVFILLER","Converted FILLER to %s.",this_item->name);
	}

}

/* 		Generate the WORKING STORAGE entrys for all the screens.  Called when PROCEDURE DIVISION is detected		*/

static int scrns_done = 0;								/* flag to say we did it		*/

gen_screens()
{
	if ( SCREEN_VERSION == 21 )
	{
		gen_screens21();
	}
	else
	{
		gen_screens22();
	}
}

gen_screens22()
{
	int 	i, value_len;
	char	s_level[5], s_occurs[5], s_row[5], s_col[5], s_pic[50], s_value[80]; 


	if (scrns_done) return(0);							/* already did it			*/
	scrns_done = 1;
	if (!num_screens) return(0);							/* no screens.				*/

	write_log("WISP",'I',"BEGINSCRN","Begin generation of screen records.");

	for (i=0; i<num_screens; i++)					
	{										/* Output a corrected screen record	*/
		write_log("WISP",'I',"DOSCREEN","Generating screen number %d.",i);
											/* First output an order area for it.	*/
		write_line("\n\n******* Definition of screen %s *******\n", scrn_name[i]);

		make_oa(templine,scrn_name[i]);
		write_line("       01  %s.\n",templine);
		put_line  ("           05  FILLER         PIC X(1) VALUE DEC-BYTE-1.\n");
		put_line  ("           05  FILLER         PIC X(1) VALUE WISP-SCWCC.\n");
		put_line  ("           05  FILLER         PIC X(1) VALUE DEC-BYTE-0.\n");
		put_line  ("           05  FILLER         PIC X(1) VALUE DEC-BYTE-0.\n\n");

											/* Now the structure describing it	*/
		write_line("       01  %s.\n",scrn_name[i]);				/* output the screen name		*/


		this_item = screen_item[i];						/* Get ptr to first item		*/
		last_item = this_item;

		write_line("           05  WISP-SCREEN-VERSION PIC X VALUE DEC-BYTE-%d.\n",SCREEN_VERSION);
		write_line("           05  FILLER PIC X(8)  VALUE \"%-8s\".\n",prog_id);
		write_line("           05  FILLER PIC X(32) VALUE\n                                \"%-32s\".\n",scrn_name[i]);
		write_line("           05  STATIC-DYNAMIC PIC X     VALUE \"S\".\n");
		write_line("           05  DECIMAL-IS     PIC X     VALUE \"%c\".\n",decimal_is);
		write_line("           05  RESERVED-SPACE PIC X(9)  VALUE SPACES.\n\n");

		do									/* loop for each item in the screen	*/
		{
			if (!compress)
			{
				write_line("\n      *** Field %s of %s ***\n",this_item->name, scrn_name[i]);
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
				value_len = strlen(s_value) - 2;

				if ( value_len < 32 )
				{
					write_line("           05  FILLER PIC X(%d) VALUE %s.\n",value_len, s_value);
				}
				else
				{
					write_line("           05  FILLER PIC X(%d) VALUE\n",value_len);
					write_line("               %s.\n", s_value);
				}
			}
			else								/* Otherwise, be verbose.		*/
			{
				put_line  ("           05  FILLER.\n");
				write_line("               10  DISP-LEVEL  PIC X(%d) VALUE \"%s\".\n",strlen(s_level),s_level);
				if ( this_item->x_row    )
					write_line("               10  DISP-ROW    PIC X(%d) VALUE \"%s\".\n",strlen(s_row),s_row);
				if ( this_item->x_col    )
					write_line("               10  DISP-COL    PIC X(%d) VALUE \"%s\".\n",strlen(s_col),s_col);
				if ( this_item->x_occurs )
					write_line("               10  DISP-OCCURS PIC X(%d) VALUE \"%s\".\n",
													strlen(s_occurs),s_occurs);
				if ( this_item->pic[0]   )
				{
				    if ((int)strlen(s_pic) < 20)
				    {
					write_line("               10  DISP-PIC    PIC X(%d) VALUE \"%s\".\n",strlen(s_pic),s_pic);
				    }
				    else
				    {
					write_line("               10  DISP-PIC    PIC X(%d) VALUE\n",strlen(s_pic));
					write_line("                   \"%s\".\n",s_pic);
				    }
				}
				put_line  ("               10  DISP-END    PIC X    VALUE \";\".\n"); 

			}

			if ( this_item->pic[0]   )
				item_fac();						/* generate the FAC information		*/

			last_item = this_item;
			this_item = this_item->next_item;				/* And point to the next item		*/

		}
		while (this_item);							/* 'till there are no more...		*/


/* -----------------------------	Write a hex FF to signal this is the last item		------------------------------	*/

		put_line  ("\n           05  DISP-END-SCREEN PIC X VALUE \".\".\n\n");
	}
	write_log("WISP",'I',"SCREENDONE","Finished generating screen records.");
}

gen_screens21()
{
	int i;

        unsigned char	  edit_mask_item[4];
	unsigned long int  temp_mask;

	if (scrns_done) return(0);							/* already did it			*/
	scrns_done = 1;
	if (!num_screens) return(0);							/* no screens.				*/

	write_log("WISP",'I',"BEGINSCRN","Begin generation of screen records.");

	for (i=0; i<num_screens; i++)					
	{										/* Output a corrected screen record	*/
		write_log("WISP",'I',"DOSCREEN","Generating screen number %d.",i);
											/* First output an order area for it.	*/
		make_oa(templine,scrn_name[i]);
		write_line("       01  %s.\n",templine);
		put_line  ("           05  FILLER         PIC X(1) VALUE DEC-BYTE-1.\n");
		put_line  ("           05  FILLER         PIC X(1) VALUE WISP-SCWCC.\n");
		put_line  ("           05  FILLER         PIC X(1) VALUE DEC-BYTE-0.\n");
		put_line  ("           05  FILLER         PIC X(1) VALUE DEC-BYTE-0.\n\n");

											/* Now the structure describing it	*/
		write_line("       01  %s.\n",scrn_name[i]);				/* output the screen name		*/


		this_item = screen_item[i];						/* Get ptr to first item		*/
		last_item = this_item;

		write_line("           05  WISP-SCREEN-VERSION PIC X VALUE DEC-BYTE-%d.\n",SCREEN_VERSION);
		write_line("           05  FILLER PIC X(8) VALUE \"%-8s\".\n",prog_id);
		write_line("           05  FILLER PIC X(32) VALUE\n                                \"%-32s\".\n",scrn_name[i]);

		do									/* loop for each item in the screen	*/
		{
			if (this_item->vert_1 == -1) 					/* skip group items			*/
			{
				last_item = this_item;
				this_item = this_item->next_item;			/* And point to the next item		*/
				continue;
			}

			write_line("\n******* Field %s of %s\n",this_item->name, scrn_name[i]);

			if (compress)							/* Compress the output if they ask.	*/
			{
				put_line  ("           05  FILLER.\n");
				write_line("               10  FILLER PIC X(8) VALUE X\"%02X%02X%02X%02X%02X%02X%02X%02X\".\n",
						this_item->vert_1,
						this_item->vert_2,
						this_item->vert_off,
						this_item->horiz,
						this_item->row,
						this_item->col,
					   	pic_size(this_item->pic),
						pic_dp(this_item->pic)
					);

				temp_mask = pic_edit(this_item->pic);
				edit_mask_item[0] = (temp_mask      ) & 0xFF;
				edit_mask_item[1] = (temp_mask >>  8) & 0xFF;
				edit_mask_item[2] = (temp_mask >> 16) & 0xFF;
				edit_mask_item[3] = (temp_mask >> 24) & 0xFF;
				

				if (strpos(this_item->pic,"Z") != -1)			/* If it has a z-field, set the bit	*/
				{
					edit_mask_item[0] |= 1;				/* Set the low order bit on.		*/
				}

				write_line("               10  FILLER PIC X(4) VALUE X\"%02X%02X%02X%02X\".\n",
								edit_mask_item[0],
								edit_mask_item[1],
								edit_mask_item[2],
								edit_mask_item[3]
					);

				if (edit_mask_item[0] & 1)				/* If bit is set, need the z mask too.	*/
				{
					temp_mask = pic_zmask(this_item->pic);
					edit_mask_item[0] = (temp_mask      ) & 0xFF;
					edit_mask_item[1] = (temp_mask >>  8) & 0xFF;
					edit_mask_item[2] = (temp_mask >> 16) & 0xFF;
					edit_mask_item[3] = (temp_mask >> 24) & 0xFF;


					write_line("               10  FILLER PIC X(4) VALUE X\"%02X%02X%02X%02X\".\n",
								edit_mask_item[0],
								edit_mask_item[1],
								edit_mask_item[2],
								edit_mask_item[3]
						);
				}

			}
			else								/* Otherwise, be verbose.		*/
			{
				put_line  ("           05  FILLER.\n");
				write_line("               10  DISP-OCCURS-VERTICAL-1     PIC X VALUE DEC-BYTE-%d.\n",
													this_item->vert_1);
				write_line("               10  DISP-OCCURS-VERTICAL-2     PIC X VALUE DEC-BYTE-%d.\n",
													this_item->vert_2);
				write_line("               10  DISP-VERTICAL-OFFSET       PIC X VALUE DEC-BYTE-%d.\n",
													this_item->vert_off);
				write_line("               10  DISP-OCCURS-HORIZONTAL     PIC X VALUE DEC-BYTE-%d.\n",
													this_item->horiz);
				write_line("               10  DISP-ROW-POSITION          PIC X VALUE DEC-BYTE-%d.\n",
													this_item->row);
				write_line("               10  DISP-COLUMN-POSITION       PIC X VALUE DEC-BYTE-%d.\n",
													this_item->col);
				write_line("               10  DISP-ITEM-LENGTH           PIC X VALUE DEC-BYTE-%d.\n",
													pic_size(this_item->pic));
				write_line("               10  DISP-ITEM-DECIMAL-POINT    PIC X VALUE DEC-BYTE-%d.\n",
													pic_dp(this_item->pic));
				put_line  ("               10  DISP-ITEM-EDIT-MASK.\n");

				temp_mask = pic_edit(this_item->pic);
				edit_mask_item[0] = (temp_mask      ) & 0xFF;
				edit_mask_item[1] = (temp_mask >>  8) & 0xFF;
				edit_mask_item[2] = (temp_mask >> 16) & 0xFF;
				edit_mask_item[3] = (temp_mask >> 24) & 0xFF;

				if (strpos(this_item->pic,"Z") != -1)			/* If it has a z-field, set the bit	*/
				{
					edit_mask_item[0] |= 1;				/* Set the low order bit on.		*/
				}

				write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[0]);
				write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[1]);
				write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[2]);
				write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[3]);

				if (edit_mask_item[0] & 1)				/* If bit is set, need the z mask too.	*/
				{
					put_line  ("               10  DISP-ITEM-Z-EDIT-MASK.\n");

					temp_mask = pic_zmask(this_item->pic);
					edit_mask_item[0] = (temp_mask      ) & 0xFF;
					edit_mask_item[1] = (temp_mask >>  8) & 0xFF;
					edit_mask_item[2] = (temp_mask >> 16) & 0xFF;
					edit_mask_item[3] = (temp_mask >> 24) & 0xFF;
					write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[0]);
					write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[1]);
					write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[2]);
					write_line("                   15  FILLER                 PIC X VALUE DEC-BYTE-%d.\n",
													edit_mask_item[3]);
				}
			}
			item_fac();							/* generate the FAC information		*/

			last_item = this_item;
			this_item = this_item->next_item;				/* And point to the next item		*/

		}
		while (this_item);							/* 'till there are no more...		*/


/* -----------------------------	Write a hex FF to signal this is the last item		------------------------------	*/

		put_line  ("\n           05  FILLER PIC X VALUE DEC-BYTE-255.\n\n");
	}
	write_log("WISP",'I',"SCREENDONE","Finished generating screen records.");
}


/* ------------------------ 		Write the FAC for this item into the record		-------------------------------	*/

static char *fac_name[] = { 	"WFAC-PROTECT",
				"WFAC-MODIFY"   };
item_fac()
{
	int i,j,k,m;
	int	fac_idx;
	char the_level[32];
	char item_str[256];

/* 				Initialize the FAC's according to their field types						*/

	if (this_item->vert_1 || this_item->vert_2 || this_item->horiz)			/* if the item is a multiple item...	*/
	{
		facnum++;								/* need a new fac number		*/
											/* write the fac info			*/

		item_str[0] = '\0';							/* init item string			*/

		if (this_item->object[0])					/* if the object is null, protect the field	*/
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

		m = (this_item->vert_1 == 0) ? 1 : this_item->vert_1;		/* get vert count				*/
		m *= this_item->vert_2 == 0 ? 1 : this_item->vert_2;		/* times the vert 2 count			*/
		m *= this_item->horiz == 0 ? 1 : this_item->horiz;		/* times the horizontal count			*/

										/* now write the fac memory and the item memory	*/
		write_line("           05  FAC-NUMBER-%d.\n",facnum);

		write_line("               10  FILLER PIC X(%d) VALUE ALL %s.\n",m,fac_name[fac_idx]);
		write_line("               10  FILLER PIC X(%d).\n",m * pic_size(this_item->pic));
										/* compute the dimensions			*/
		if (this_item->vert_1)						/* has a vertical dimension			*/
		{

			write_line                      ("           05  FILLER REDEFINES FAC-NUMBER-%d.\n",facnum);
											/* write the first vertical dimension	*/
			sprintf(templine,                "             07  FILLER OCCURS %d TIMES.\n", this_item->vert_1);
			put_line(templine);
			strcat(item_str,templine);					/* build up the info for the item too	*/

			if (this_item->vert_2)
			{								/* write the second vertical dimension	*/
				sprintf(templine,        "               10  FILLER OCCURS %d TIMES.\n",this_item->vert_2);
				put_line(templine);
				strcat(item_str,templine);				/* build up the info for the item too	*/

				if (this_item->horiz)					/* and the horizontal count (3D)	*/
				{
					sprintf(templine,"                 15  FILLER OCCURS %d TIMES.\n",this_item->horiz);
					put_line(templine);
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
				put_line(templine);
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

			write_line("           05  FILLER REDEFINES FAC-NUMBER-%d.\n",facnum);
											/* write the horizontal dimension	*/
			sprintf(templine,"           07  FILLER OCCURS %d TIMES.\n", this_item->horiz);
			put_line(templine);
			strcat(item_str,templine);					/* build up the info for the item too	*/
			strcpy(the_level,"    10");					/* 10 is the level of the item		*/
		}

		make_fac(templine,this_item->name);					/* make a FAC				*/
		write_line("           %s  %s PIC X.\n",the_level,templine);		/* write it out				*/
		put_line(item_str);							/* set up nesting of the item too	*/

		if (item_blank(this_item->name))
		{
			if ((int)(strlen(this_item->pic)+strlen(this_item->name)) < 50)
			{
				write_line("           %s  %s PIC %s\n",the_level,this_item->name,this_item->pic);
				put_line  ("               BLANK WHEN ZERO.\n");
			}
			else
			{
				write_line("           %s  %s\n",the_level,this_item->name);
				write_line("               PIC %s BLANK WHEN ZERO.\n",this_item->pic);
			}
		}
		else if ((int)(strlen(the_level) + strlen(this_item->name) + strlen(this_item->pic)) < 50)	
											/* see if it fits on one line		*/
		{
			write_line("           %s  %s PIC %s.\n",the_level,this_item->name,this_item->pic);
		}
		else
		{
			write_line("           %s  %s PIC\n                 %s.\n",the_level,this_item->name,this_item->pic);
		}
	}
	else										/* the item is singular			*/
	{
		if (this_item->object[0])						/* has an object			*/
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

		if ( (int)strlen(templine) < 28 )
		{
			write_line("           05  %s PIC X VALUE %s.\n",templine,fac_name[fac_idx]);
		}
		else
		{
			write_line("           05  %s PIC X\n",templine);
			write_line("               VALUE %s.\n",fac_name[fac_idx]);
		}

		if ( !strcmp(this_item->name,"FILLER"))					/* process fillers special		*/
		{
			if (this_item->source[0])					/* if it has a source, use it		*/
			{
				if (strpos(this_item->source,"\"") != -1)		/* a literal source			*/
				{
					write_line("           05  %s PIC %s VALUE\n%s",
										this_item->name,this_item->pic,this_item->source);
					if ((int)strlen(this_item->source) > 71)	/* multi line?				*/
					{
						put_line("\n                   .\n");	/* yes, put period on next line		*/
					}
					else
					{
						put_line(".\n");			/* single line, put preiod here		*/
					}
				}
			}
			else
			{									/* otherwise use spaces		*/
				write_line("           05  %s PIC %s VALUE SPACES.\n",
												this_item->name,this_item->pic);
			}
		}										/* not a filler, but has a lit-	*/
												/* eral as a source. & no object*/
		else if ( (strpos(this_item->source,"\"") != -1) && (!this_item->object[0]))
		{										
			write_log("WISP",'I',"LITERALSRC",
							"Processing Field %s with Literal source and no object.",this_item->name);
			write_line("           05  %s PIC %s VALUE\n%s",this_item->name,this_item->pic,this_item->source);

			if (whatcol(this_item->source) < 73)
			{
				write_line(".\n");
			}
			else
			{
				write_line("\n           .\n");
			}
		}
		else									/* not a filler, use the item	*/
		{
			if (item_blank(this_item->name))
			{
				if ((int)(strlen(this_item->pic)+strlen(this_item->name)) < 50)
				{
					write_line("           05  %s PIC %s\n",this_item->name,this_item->pic);
					put_line  ("               BLANK WHEN ZERO.\n");
				}
				else
				{
					write_line("           05  %s\n",this_item->name);
					write_line("               PIC %s BLANK WHEN ZERO.\n",this_item->pic);
				}
			}
			else if ((int)(strlen(this_item->pic)+strlen(this_item->name)) < 50)
			{
				write_line("           05  %s PIC %s.\n",this_item->name,this_item->pic);
			}
			else
			{
				write_line("           05  %s PIC\n                    %s.\n",this_item->name,this_item->pic);
			}
		}
	}
}

static gen_mwconv()
{
		put_line("       CONVERT-ALPHA-VALUE-TO-NUMERIC.\n\n");			/* procedure to do MOVE WITH CONVERSION	*/
		put_line("           CALL \"mwconv\" USING\n");				/* first let mwconv do it		*/
		put_line("               WISP-ALPHA-CONVERSION-FIELD,\n");
		put_line("               WISP-CONVERTED-INTEGER-FIELD,\n");
		put_line("               WISP-CONVERTED-FRACTION-FIELD,\n");
		put_line("               WISP-CONVERSION-LENGTH,\n");
		put_line("               WISP-CONVERSION-ERROR-FIELD.\n\n");

		put_line("           IF WISP-CONVERSION-ERROR-FIELD IS = ZERO\n");
		put_line("               MOVE \"N\" TO WISP-NUMERIC-CONVERSION-FLAG,\n");
		put_line("           ELSE\n");
		put_line("               MOVE \"Y\" TO WISP-NUMERIC-CONVERSION-FLAG.\n\n");	/* that's it!			*/
}

scrn_para()									/* write the screen paragraphs for all screens	*/
{
	int scrnum;
	char wdr[48],wgs[48],wpd[48],wdc[48],wrc[48];

	write_log("WISP",'I',"GENWISPSCRN","Begin generation of WISP screen and support procedures.");

	if (!in_decl)									/* be sure we aren't in the declaratives*/
	{
		put_line("       WISP-PROCEDURES SECTION.\n\n");			/* start the procedure section		*/
		if (mwconv_flag) 
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
		}
		else
		{
			make_fld(wdr,scrn_name[scrnum],"WDR-");
			make_fld(wgs,scrn_name[scrnum],"WGS-");
			make_fld(wpd,scrn_name[scrnum],"WPD-");
			make_fld(wdc,scrn_name[scrnum],"WDC-");
			make_fld(wrc,scrn_name[scrnum],"WRC-");
		}

		write_line("\n      ***** PROCEDURES FOR SCREEN %s\n\n",scrn_name[scrnum]);
		write_line("       %s.\n",wdr);						/* Gen DISPLAY AND READ paragraph.	*/
		write_line("           PERFORM %s.\n",wpd);				/* Perform Put Screen Data.		*/
		write_line("           MOVE SPACE TO %s.\n",crt_status[cur_crt]);
		write_line("           PERFORM %s\n",wdc);				/* Perform Display and check.		*/
		put_line  ("                   UNTIL WISP-ON-PF-KEYS IS EQUAL TO \042*\042\n");
		write_line("                   AND   %s IS NOT EQUAL TO SPACE\n",crt_status[cur_crt]);
		write_line("                   AND   %s IS NOT EQUAL TO \042E\042.\n\n",crt_status[cur_crt]);
		if (crt_cursor[cur_crt][0])						/* if there is a cursor			*/
		{
			put_line  ("           CALL \042w2rowcol\042 USING WISP-CRT-ORDER-AREA-3,\n");
			write_line("                          %s.\n",crt_cursor[cur_crt]);
		}

		write_line("       %s.\n",wdc);						/* Wisp display and check.		*/
		write_line("           CALL \042wscreen\042 USING %s,\n",scrn_name[scrnum]);
		put_line  ("                                VWANG-DISP-AND-READ-ALT,\n");
		put_line  ("                                WISP-CRT-RECORD,\n");
		put_line  ("                                VWANG-FULL-SCREEN,\n");
		put_line  ("                                WISP-ALLOWABLE-PF-KEYS,\n");
		put_line  ("                                WISP-ON-PF-KEYS,\n");
		write_line("                                %s,\n",crt_pfkey[cur_crt]);	/* output the PFKEY variable name 	*/
		write_line("                                %s.\n\n",crt_status[cur_crt]);/* output the STATUS variable name	*/
		put_line  ("           IF WISP-ON-PF-KEYS IS NOT = \042*\042\n");
		write_line("              PERFORM %s.\n\n",wrc);			/* Perform wisp range check.		*/
		write_line("       %s.\n",wrc);						/* Gen range check para.		*/

		this_item = screen_item[scrnum];				/* pointer to first item in the current screen	*/

		chk_range();							/* generate the range checking code		*/


		write_line("\n      ***** PUT PROCEDURE - SCREEN %s\n\n",scrn_name[scrnum]);
		write_line("       %s.\n",wpd);						/* Put screen data.			*/
		this_item = screen_item[scrnum];				/* pointer to first item in the current screen	*/
		move_source();							/* move the source fields			*/
										/* move the screen order area			*/
		make_oa(templine,scrn_name[scrnum]);				/* create one					*/
		write_line("           MOVE %s TO WISP-CRT-ORDER-AREA.\n",templine);
		write_line("\n      ***** GET PROCEDURE - SCREEN %s\n\n",scrn_name[scrnum]);
		write_line("       %s.\n",wgs);					/* Get screen data.				*/
		this_item = screen_item[scrnum];
		if (0==move_object())						/* Move all the screen fields into their objects*/
		{
			put_line("           CONTINUE.\n");
		}
	}

	write_log("WISP",'I',"ENDSCRNGEN","Finish generation of screen and support procedures.");
}

move_source()									/* move sources to their objects		*/
{
	int i,a,b,c,i1,i2,i3;
	int dim;


	do
	{									/* if there is a source, do it			*/
										/* if it has a source and it's not a FILLER...	*/
		if ((this_item->vert_1 != -1) && this_item->source[0] && strcmp(this_item->name,"FILLER"))
		{
			dim = this_item->vert_1 ? 1 : 0;
			dim += this_item->vert_2 ? 1 : 0;
			dim += this_item->horiz ? 1 : 0;

			if (dim == 0)							/* no repeats				*/
			{
				a = 0; b = 0; c = 0;
			}
			else if (dim == 1)						/* one dimension			*/
			{
				a = this_item->vert_1 ? this_item->vert_1 : this_item->horiz;	/* vertical or horizontal	*/
				b = 0; c = 0;
			}
			else if (dim == 2)						/* two dimensions			*/
			{
				a = this_item->vert_1;					/* vertical plus one other		*/
				b = this_item->vert_2 ? this_item->vert_2 : this_item->horiz;	/* second vertical or horizontal*/
				c = 0;
			}
			else								/* three dimensions			*/
			{
				a = this_item->vert_1;					/* first vertical			*/
				b = this_item->vert_2;					/* second vertical			*/
				c = this_item->horiz;					/* and the horizontal			*/
			}

			if (dim)						/* it is a multi-dimensional item		*/
			{
				i1 = a;
				do						/* do the indexed moves				*/
				{
					if (b)					/* if it has 2 dimensions...			*/
					{
						i2 = b;
						do
						{
							if (c)
							{
								i3 = c;
								do
								{			/* three dimensions.			*/
											/* write the move statement		*/
									d_move(this_item->name,this_item->source,3,i1,i2,i3);
								} while (--i3);
							}
							else
							{				/* only 2 dimensions			*/
								d_move(this_item->name,this_item->source,2,i1,i2,0);
							}
						} while (--i2);
					}
					else
					{						/* only one dimension			*/
						d_move(this_item->name,this_item->source,1,i1,0,0);
					}
				} while (--i1);					/* decrement outer dimension			*/
			}
			else
			{
				if (strpos(this_item->source,"\042") != -1)	/* A literal source				*/
				{
					if (this_item->object[0])		/* if it has an object, do the move		*/
					{
						write_line("           MOVE \n%s\n              TO %s.\n",
							this_item->source,this_item->name);
					}
				}
				else if ((int)(strlen(this_item->source) + strlen(this_item->name)) > 45)
				{						/* Can't fit on one line			*/
					write_line("           MOVE %s\n              TO %s.\n",
						this_item->source,this_item->name);
				}
				else						/* It fits on one line				*/
				{
					write_line("           MOVE %s TO",this_item->source);
					write_line(" %s.\n",this_item->name);
				}

			}
		}
		last_item = this_item;
		this_item = this_item->next_item;			/* point to the next item...			*/
	}
	while (this_item);							/* till there are no more...			*/
}

									/* write the source move statement for dimensioned items */

int d_move(dest,src,num,i1,i2,i3)
int 	num,i1,i2,i3;
char	*src, *dest;
{
	char	*brk;

	brk = " ";

	switch(num)
	{
		case 1:								/* one dimensional item				*/
		{
			if ((int)(strlen(src) + strlen(dest)) > 40) brk = "\n             ";

			write_line("           MOVE %s (%d)%sTO %s (%d).\n",src,i1,brk,dest,i1);

			break;
		}

		case 2:								/* two dimensional item				*/
		{
			if ((int)(strlen(src) + strlen(dest)) > 34) brk = "\n             ";

			write_line("           MOVE %s (%d %d)%sTO %s (%d %d).\n",src,i1,i2,brk,dest,i1,i2);

			break;
		}

		case 3:								/* three dimensional item			*/
		{
			if ((int)(strlen(src) + strlen(dest)) > 28) brk = "\n             ";

			write_line("           MOVE %s (%d %d %d)%sTO %s (%d %d %d).\n",src,i1,i2,i3,brk,dest,i1,i2,i3);

			break;
		}
	}
}

int move_object()
{
	int num_moved,i;
	register int i1,i2,i3,a,b,c,dim;

	num_moved = 0;

	do
	{										/* if there is a object, do it		*/
		if ((this_item->vert_1 != -1) && this_item->object[0])			/* skip over group items		*/
		{
			dim = this_item->vert_1 ? 1 : 0;
			dim += this_item->vert_2 ? 1 : 0;
			dim += this_item->horiz ? 1 : 0;

			if (dim == 1)							/* one dimension			*/
			{
				a = this_item->vert_1 ? this_item->vert_1 : this_item->horiz;	/* vertical or horizontal	*/
				b = 0; c = 0;
			}
			else if (dim == 2)						/* two dimensions			*/
			{
				a = this_item->vert_1;					/* vertical plus one other		*/
				b = this_item->vert_2 ? this_item->vert_2 : this_item->horiz;	/* second vertical or horiz	*/
				c = 0;
			}
			else								/* three dimensions			*/
			{
				a = this_item->vert_1;					/* first vertical 			*/
				b = this_item->vert_2;					/* second vertical			*/
				c = this_item->horiz;					/* and the horizontal			*/
			}

			if (dim)							/* it is a multi-dimensional item	*/
			{
				i1 = a;
				do							/* do the indexed moves			*/
				{
					if (b)						/* if it has 2 dimensions...		*/
					{
						i2 = b;
						do
						{
							if (c)
							{
								i3 = c;
								do
								{			/* three dimensions.			*/
									d_move(this_item->object,this_item->name,3,i1,i2,i3);
								} while (--i3);
							}
							else
							{				/* only 2 dimensions			*/
								d_move(this_item->object,this_item->name,2,i1,i2,0);
							}
						} while (--i2);
					}
					else
					{						/* only one dimension			*/
						d_move(this_item->object,this_item->name,1,i1,0,0);
					}
				} while (--i1);					/* decrement outer dimension			*/
			}
			else							/* singular item				*/
			{
				if ((int)(strlen(this_item->name) + strlen(this_item->object)) > 45)	
				{						/* Can't fit on one line			*/
					write_line("           MOVE %s\n              TO %s.\n",
						this_item->name,this_item->object);
				}
				else						/* It fits on one line				*/
				{
					write_line("           MOVE %s TO",this_item->name);
					write_line(" %s.\n",this_item->object);
				}
					num_moved++;				/* count it					*/
			}
		}
		last_item = this_item;
		this_item = this_item->next_item;				/* point to the next item...			*/
	}
	while (this_item);							/* till there are no more...			*/

	return(num_moved);							/* tell them how many				*/
}


int chk_range()									/* scan a screen record and create range checks	*/
{
	register int i,i1,i2,i3,a,b,c,dim;

	do
	{										/* if there is a range, do it		*/
		if ((this_item->vert_1 != -1) && this_item->lo_range[0])		/* skip over group items		*/
		{
			dim = this_item->vert_1 ? 1 : 0;
			dim += this_item->vert_2 ? 1 : 0;
			dim += this_item->horiz ? 1 : 0;

			if (dim == 1)							/* one dimension			*/
			{
				a = this_item->vert_1 ? this_item->vert_1 : this_item->horiz;	/* vertical or horizontal	*/
				b = 0; c = 0;
			}
			else if (dim == 2)						/* two dimensions			*/
			{
				a = this_item->vert_1;					/* vertical plus one other		*/
				b = this_item->vert_2 ? this_item->vert_2 : this_item->horiz;	/* second vertical or horiz	*/
				c = 0;
			}
			else								/* three dimensions			*/
			{
				a = this_item->vert_1;					/* first vertical 			*/
				b = this_item->vert_2;					/* second vertical			*/
				c = this_item->horiz;					/* and the horizontal			*/
			}

			if (dim)							/* it is a multi-dimensional item	*/
			{
				i1 = a;
				do							/* do the indexed moves			*/
				{
					if (b)						/* if it has 2 dimensions...		*/
					{
						i2 = b;
						do
						{
							if (c)
							{
								i3 = c;
								do
								{			/* three dimensions.			*/
									d_range(3,i1,i2,i3);   /* write the needed statements	*/
								} while (--i3);
							}
							else
							{				/* only 2 dimensions			*/
								d_range(2,i1,i2,0);	/* write the needed statements		*/
							}
						} while (--i2);
					}
					else
					{						/* only one dimension			*/
						d_range(1,i1,0,0);			/* write the needed statements		*/
					}
				} while (--i1);					/* decrement outer dimension			*/
			}
			else							/* singular item				*/
			{
				if (this_item->hi_range[0])			/* Type 1, lo and hi ranges provided		*/
				{
					r_fac(this_item->name,0,0,0);		/* see if modified field			*/
					write_line("           MOVE %s TO %s\n",this_item->name,this_item->object);
					write_line("           IF %s IS <\n              %s\n",
						this_item->object,this_item->lo_range);
					write_line("           OR %s IS >\n              %s\n",
						this_item->object,this_item->hi_range);
					write_line("              MOVE %s TO %s\n",this_item->lo_range,this_item->object);
					r_err(this_item->name,0,0,0);		/* write the error lines			*/
				}							/* type 2, constant phrase		*/
				else if (!strcmp(this_item->lo_range,"POSITIVE") || !strcmp(this_item->lo_range,"NEGATIVE"))
				{
					this_item->lo_range[0] = '\0';			/* Juster will handle this		*/
				}
				else if (!this_item->num_range)				/* type 3 but no range count		*/
				{
					write_log("WISP",'E',"NORANGECNT","Range specified for field %s but no count found.\n",
														this_item->name);
				}
				else							/* has to be type 3, array check	*/
				{
					r_fac(this_item->name,0,0,0);			/* see if modified field		*/
					write_line("           IF %s IS NOT =\n              %s(1)\n",
						this_item->name,this_item->lo_range);

					for (i=1; i < this_item->num_range; i++)
					{
						write_line("           AND %s IS NOT =\n              %s(%d)\n",
							this_item->name,this_item->lo_range,i+1);
					}
					r_err(this_item->name,0,0,0);		/* write the error lines			*/

				}
			}
		}
		last_item = this_item;
		this_item = this_item->next_item;				/* point to the next item...			*/
	}
	while (this_item);							/* till there are no more...			*/

	write_line("           IF %s IS NOT = \042E\042\n",crt_status[cur_crt]);
	put_line  ("              MOVE \042*\042 TO WISP-ON-PF-KEYS.\n");
}

									/* write the range statements for dimensioned items 	*/

int d_range(num,i1,i2,i3)
int num,i1,i2,i3;
{
	int i,j;
	char itemname[60];
	char itemobject[60];

	if (i3) 
	{
		sprintf(itemname,"%s(%d,%d,%d)",this_item->name,i1,i2,i3);
		sprintf(itemobject,"%s(%d,%d,%d)",this_item->object,i1,i2,i3);
	}
	else if (i2) 
	{
		sprintf(itemname,"%s(%d,%d)",this_item->name,i1,i2);
		sprintf(itemobject,"%s(%d,%d)",this_item->object,i1,i2);
	}
	else if (i1) 
	{
		sprintf(itemname,"%s(%d)",this_item->name,i1);
		sprintf(itemobject,"%s(%d)",this_item->object,i1);
	}
	else 
	{
		strcpy(itemname,this_item->name);
		strcpy(itemobject,this_item->object);
	}

	if (this_item->hi_range[0])							/* Type 1, lo and hi ranges provided	*/
	{
		r_fac(this_item->name,i1,i2,i3);					/* see if modified field		*/
		write_line("           MOVE %s TO %s\n",itemname,itemobject);
		write_line("           IF %s IS <\n              %s\n",itemobject,this_item->lo_range);
		write_line("           OR %s IS >\n              %s\n",itemobject,this_item->hi_range);
		write_line("              MOVE %s TO %s\n",this_item->lo_range,itemobject);
		r_err(this_item->name,i1,i2,i3);					/* write the error lines		*/
	}										/* type 2, constant phrase		*/
	else if (!strcmp(this_item->lo_range,"POSITIVE") || !strcmp(this_item->lo_range,"NEGATIVE"))
	{
		this_item->lo_range[0] = '\0';						/* Juster will handle this		*/
	}
	else if (!this_item->num_range)							/* type 3 but no range count		*/
	{
		write_log("WISP",'E',"NORANGECNT","Range specified for field %s but no count found.",this_item->name);
	}
	else										/* has to be type 3, array check	*/
	{
		r_fac(this_item->name,i1,i2,i3);					/* see if modified field		*/
		write_line("           IF %s IS NOT =\n              %s(1)\n",itemname,this_item->lo_range);

		for (j=1; j < this_item->num_range; j++)
		{
			write_line("           AND %s IS NOT =\n              %s(%d)\n",
											itemname,this_item->lo_range,j+1);
		}
		r_err(this_item->name,i1,i2,i3);					/* write the error lines		*/
	}
}




r_fac(the_name,i1,i2,i3)							/* write the FAC test for ranges		*/
char *the_name;
int  i1,i2,i3;
{
	char tfac[34];
	char ltemp[80];

	make_fac(tfac,the_name);						/* need a FAC					*/
	if (i3) 	sprintf(ltemp,"%s(%d,%d,%d)",tfac,i1,i2,i3);		/* put in the index if need be			*/
	else if (i2) 	sprintf(ltemp,"%s(%d,%d)",tfac,i1,i2);			/* put in the index if need be			*/
	else if (i1) 	sprintf(ltemp,"%s(%d)",tfac,i1);			/* put in the index if need be			*/
	else		strcpy(ltemp,tfac);

	put_line  ("           MOVE WISP-MOD-BIT TO WISP-SET-BYTE.\n");
	put_line  ("           CALL \042bit_test\042 USING WISP-SET-BYTE,\n");
	write_line("                               %s\n",ltemp);
	put_line  ("                               WISP-TEST-BYTE.\n\n");
	put_line  ("           IF WISP-TEST-BYTE = \042Y\042\n");
}

r_err(the_name,i1,i2,i3)							/* write the error lines for ranges		*/
char *the_name;
int  i1,i2,i3;
{
	char tfac[34];
	char ltemp[80];

	make_fac(tfac,the_name);						/* need a FAC					*/
	if (i3) 	sprintf(ltemp,"%s(%d,%d,%d)",tfac,i1,i2,i3);		/* put in the index if need be			*/
	else if (i2) 	sprintf(ltemp,"%s(%d,%d)",tfac,i1,i2);			/* put in the index if need be			*/
	else if (i1) 	sprintf(ltemp,"%s(%d)",tfac,i1);			/* put in the index if need be			*/
	else		strcpy(ltemp,tfac);

	write_line("              MOVE \042E\042 TO %s,\n",crt_status[cur_crt]);
	put_line  ("              MOVE WISP-ERROR-BIT TO WISP-SET-BYTE,\n");
	put_line  ("              CALL \042bit_on\042 USING WISP-SET-BYTE,\n");
	write_line("                                  %s.\n\n",ltemp);
}



p_occurs(exit_item)									/* Process the end of a group of occurs.*/
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

	do
	{
		if (temp_ptr->vert_1 != -1) 						/* Skip over junk.			*/
		{
			low_row = (temp_ptr->row < low_row) ? temp_ptr->row : low_row;	/* See if its lower.			*/
			hi_row = (temp_ptr->row > hi_row) ? temp_ptr->row : hi_row;	/* See if its higher.			*/
		}
		if (temp_ptr == exit_item) break;
		temp_ptr = temp_ptr->next_item;
	} while (1);									/* Do all till we process the exit item	*/

	i = hi_row - low_row + 1;							/* Actual vertical increment value.	*/

	i = (i < 1) ? 1 : i;								/* Make sure we did something.		*/

	temp_ptr = occurs_item;								/* Point to first occurs item.		*/
	do
	{
		temp_ptr->vert_off = i;							/* Now set the vertical offsets.	*/
	} while (temp_ptr = temp_ptr->next_item);					/* Check them all			*/

	occurs_item = 0;								/* Zero the pointer.			*/
}


exit_pmode()										/* Finish a parsing mode		*/
{
	int i;

	if (pmode == FIG_CONS)								/* see if exiting FIGURATIVE CONSTANTS	*/
	{
		put_line("            .\n");						/* yes, make sure a period goes in.	*/
		pmode = 0;								/* Pmode is now finished		*/
	}
	else if (pmode == WS_SCREEN)							/* see if trying to exit a screen	*/
	{
		if ((strcmp(area_a,"    ") && (area_a_num == 0)) || (area_a_num == 1))	/* only exit if area a is not a space	*/
		{									/* or area a is equal to 1.		*/
			if (occurs_item) p_occurs(this_item);				/* Check Occurs items before exit.	*/
			pmode = 0;
		}
	}
	else if (copy_sect)								/* Have we been copying a SECTION?	*/
	{
		if (!strncmp(parms[1],"SECTION",7))					/* Is this a section?			*/
		{
			copy_sect = 0;							/* Only stop on a new section.		*/
			copy_para = 0;
			fprintf(par_file,"       WISP-SECTION-%d-END.\n",sect_num++);	/* Put in the ending paragraph.		*/
		}
	}
	else if (copy_para)								/* Have we been copying paragraphs?	*/
	{
		copy_para = 0;								/* reset the mode.			*/
	}
	else if (copy_decl)								/* Have we been copying paragraphs?	*/
	{
		copy_decl = 0;								/* reset the mode.			*/
	}

	if (division == PROCEDURE_DIVISION)						/* If in the procedure division, when	*/
	{										/* something is in area a, it has to	*/
		chk_dpar();								/* be a paragraph, or declarative sect.	*/
	}
}

int item_blank(the_name)								/* See if this item is in the blank list*/
char *the_name;
{
	int i;

	if (!blank_count) return(0);							/* None on the list.			*/

	i = 0;

	do
	{
		if (!strcmp(blank_item[i],the_name)) return(1);				/* Found it!!				*/
	} while (i++ < blank_count);
	return(0);									/* Didn't find it.			*/
}

clear_item(the_item)
item_record *the_item;
{
	strcpy(the_item->item_num,"05  ");						/* default item num is 05		*/
	the_item->name[0] = 0;								/* Clear the variable name		*/
	the_item->pic[0] = 0;								/* The pic definition for the var	*/
	the_item->source[0] = 0;							/* The var that is the source for output*/
	the_item->object[0] = 0;							/* The var that is the target for input */
	the_item->lo_range[0] = 0;							/* The low end of the RANGE		*/
	the_item->hi_range[0] = 0;							/* The high end of the RANGE		*/
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

/*
	whatcol		Return the column number we will be in if this string was written.
*/
int whatcol(str)
char	*str;
{
	char	*ptr;

	if ( ptr=(char *)strrchr(str,'\n') )				/* Find the last newline in the string			*/
	{
		str = ptr;						/* point to last newline				*/
		str++;							/* point past the newline				*/
	}

	return(strlen(str)+1);						/* Return length plus one.				*/
}

