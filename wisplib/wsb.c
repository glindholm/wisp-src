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
**	File:		wsb.c
**
**	Project:	WISPLIB
**
**	Purpose:	Construct, display, and retrieve screen buffers
**
**			These routines provide a common front-end which can
**			be used with vwang or with wisp_nativescreens.
**
*/

/*
**	Includes
*/
#include <string.h>

#include "wsb.h"
#include "wmalloc.h"
#include "scnfacs.h"
#include "assert.h"
#include "werrlog.h"
#include "wexit.h"
#include "wperson.h"
#include "wisplib.h"
#include "vchinese.h"
#include "link.h"
#include "idsisubs.h"

/*
**	Structures and Defines
*/
#define MAX_WSB			32	/* Max open WSB handles */

#define MAX_WSB_FIELDS		40	/* Max number of entry fields */
#define MAX_WSB_FIELD_LEN	79	/* Max length of entry fields */

/*
**	field_t		Screen field definition for COBOL
*/
typedef struct s_field
{
	uint2 row;			/* UNSIGNED-SHORT 0-23	*/
	uint2 col;			/* UNSIGNED-SHORT 0-79	*/
	uint2 len;			/* UNSIGNED-SHORT 1-79	*/
	fac_t fac;			/* PIC X COMP-X 	*/
	char field[MAX_WSB_FIELD_LEN];	/* PIC X(79) 		*/
} field_t;

/*
**	wsbnative_t	Screen definition for native screens
*/
typedef struct s_wsbnative
{
	char	data[WSB_ROWS][WSB_COLS];	/* Static text data 24x80 */
	uint2	currow;				/* Cursor row */
	uint2	curcol;				/* Cursor column */
	uint2 	fields;				/* Number of fields in field_list 0-40 */
	field_t field_list[MAX_WSB_FIELDS];	/* Array of fields */
} wsbnative_t;

/*
**	wsb_t		Generic screen definition block
*/
#define NUM_PFKEYS	34
typedef struct s_wsb 
{
	wsbvwang_t *vw;			/* Vwang struct for WISP screens */
	wsbnative_t *na;		/* Native struct for Native screens */
	int (*menumap)[NUM_PFKEYS];	/* Map of screen position to pfkey value for menu items (0=enter, 1-32, 33=help) */
	int sound_alarm;		/* Flag to ring the bell */
} wsb_t;


/*
**	Globals and Externals
*/

/*
**	Static data
*/
static wsb_t *wsb[MAX_WSB];		/* Array of screen blocks */

/*
**	Static Function Prototypes
*/

/*
**	ROUTINE:	wsb_first()
**
**	FUNCTION:	Initialize WSB internals
**
**	DESCRIPTION:	This needs to be called first.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
static void wsb_first(void)
{
	static int first = 1;
	if (first)
	{
		int idx;
		
		/*
		**	Mark all the screen blocks as unused (NULL)
		*/
		for(idx=0; idx<MAX_WSB; idx++)
		{
			wsb[idx] = (wsb_t *)NULL;
		}
		first = 0;
	}
}

/* Check if a WSB handle is invalid */
static int check_hwsb(HWSB hWsb)
{
	wsb_first();

	if (hWsb >= MAX_WSB || !wsb[hWsb])
	{
		ASSERT(0);
		WL_werrlog_error(WERRCODE(104),"WSB", "HWSB", 
			"Invalid handle to WSB");
		return 1;
	}

	return 0;
}

/* Validate a row value */
static int check_row(int row)
{
	if (row < 1 || row > WSB_ROWS)
	{
		WL_wtrace("WSB","ROW","Invalid row value %d",row);
		row = 1;
	}

	return row;
}

/* Validate a column value */
static int check_col(int col, int len)
{
	/*
	**	Calc the col needed to Center the text.
	*/
	if (0 == col)
	{
		col = (WSB_COLS - len) / 2;
	}
	
	if (col < 1 || col > WSB_COLS)
	{
		WL_wtrace("WSB","COLUMN","Invalid column value %d",col);
		col = 1;
	}

	return col;
}

/* Validate the length of a static text field */
static int check_text_len(int len, int row, int col)
{
	int	start_pos, end_pos, max_pos;

	/*
	**	Text can't wrap past end of screen so truncate it if needed
	*/
	start_pos = (row-1)*WSB_COLS + col;
	end_pos   = start_pos + len - 1;
	max_pos   = (WSB_ROWS * WSB_COLS);
	if (end_pos > max_pos)
	{
		WL_wtrace("WSB","LENGTH","Invalid text length value %d",len);
		len = max_pos - start_pos;
	}

	return len;
}

/* Validate the length of a data field */
static int check_field_len(int len, int col)
{
	/*
	**	A field can't wrap past the end of the line.
	*/
	if ((len+col-1) > WSB_COLS)
	{
		WL_wtrace("WSB","LENGTH","Invalid field length value %d",len);
		len = WSB_COLS - col;
	}

	return len;
}

/*
**	ROUTINE:	wsb_new()
**
**	FUNCTION:	Create a new WSB structure
**
**	DESCRIPTION:	Find and empty slot in the wsb table and initialize it.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		The handle to the WSB structure.
**
**	WARNINGS:	None
**
*/
HWSB wsb_new(void)
{
	HWSB	idx;
	
	wsb_first();
	
	/*
	**	Search for an open slot in the handle table.
	*/
	for(idx=0; idx<MAX_WSB; idx++)
	{
		if (!wsb[idx])
		{
			break;
		}
	}
	if (idx >= MAX_WSB)
	{
		WL_werrlog_error(WERRCODE(104),"WSB", "MAX", 
			"Maximum number of WSB handles exceeded");
		wexit(1);
	}

	/*
	**	Allocate and initialize the WSB structure
	*/
	wsb[idx] = (wsb_t *)wisp_malloc(sizeof(wsb_t));
	wsb[idx]->vw = (wsbvwang_t *)NULL;
	wsb[idx]->na = (wsbnative_t *)NULL;
	wsb[idx]->menumap = NULL;
	wsb[idx]->sound_alarm = 0;
	
	if (wisp_nativescreens())
	{
		int	i;

		wsb[idx]->na = (wsbnative_t *)wisp_malloc(sizeof(wsbnative_t));
		memset(wsb[idx]->na, ' ', sizeof(wsbnative_t));
		wsb[idx]->na->currow = 1;
		wsb[idx]->na->curcol = 1;
		wsb[idx]->na->fields = 0;

		for(i=0; i<MAX_WSB_FIELDS;i++)
		{
			/*
			**	Initialize the values to a location on the screen 
			**	which is unlikely to be used.
			*/
			if (wisp_acu_cobol())
			{
				wsb[idx]->na->field_list[i].row = 24;
				wsb[idx]->na->field_list[i].col = 80;
				wsb[idx]->na->field_list[i].len =  1;
			}
			else if (wisp_mf_cobol())
			{
				wsb[idx]->na->field_list[i].row = 0;
				wsb[idx]->na->field_list[i].col = 0;
				wsb[idx]->na->field_list[i].len = 0;
			}
		}
	}
	else
	{
		wsb[idx]->vw = (wsbvwang_t *)wisp_malloc(sizeof(wsbvwang_t));
		wsb[idx]->vw->oa.row = 1;
		wsb[idx]->vw->oa.wcc = POSITION_CURSOR|UNLOCK_KEYBOARD; /* 0xA0 = 0x20 + 0x80  */
		wsb[idx]->vw->oa.curcol = 0;
		wsb[idx]->vw->oa.currow = 0;
		memset(wsb[idx]->vw->data, ' ', sizeof(wsb[idx]->vw->data));
	}

	return idx;
}

/*
**	ROUTINE:	wsb_delete()
**
**	FUNCTION:	Delete a WSB structure
**
**	DESCRIPTION:	Undo the wsb_new() operation.
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		none
**
**	WARNINGS:	None
**
*/
void wsb_delete(HWSB hWsb)
{
	if (check_hwsb(hWsb))
	{
		return;
	}

	if (wisp_nativescreens())
	{
		free(wsb[hWsb]->na);
	}
	else
	{
		free(wsb[hWsb]->vw);
	}

	if (wsb[hWsb]->menumap)
	{
		free(wsb[hWsb]->menumap);
	}
	
	free(wsb[hWsb]);
	wsb[hWsb] = (wsb_t *)NULL;
}


/*
**	ROUTINE:	wsb_add_text()
**
**	FUNCTION:	Add a text string to the screen
**
**	DESCRIPTION:	Copy the text into the static text area of the screen.
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**	row		The fields row position (1-24)
**	col		The fields column position (1-80, 0=Center)
**	text		The field value (NULL terminated)
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wsb_add_text(HWSB hWsb, int row, int col, const char *text)
{
	int 	len;

	if (check_hwsb(hWsb))
	{
		return;
	}
	
	len = strlen(text);

	row = check_row(row);
	col = check_col(col, len);
	len = check_text_len(len, row, col);

	/*
	**	Convert row and col to be zero based.
	*/
	col--;
	row--;
	
	if (wisp_nativescreens())
	{
		memcpy(&wsb[hWsb]->na->data[row][col], text, len);
	}
	else
	{
		memcpy(&wsb[hWsb]->vw->data[row][col], text, len);

		/*
		**	If chinese then change '[' and ']' into '(' and )'.
		*/
		if (IVS_outctx)
		{
			char *ptr;
			
			while((ptr = memchr(&wsb[hWsb]->vw->data[row][col], '[', len)))
			{
				*ptr = '(';
			}
			while((ptr = memchr(&wsb[hWsb]->vw->data[row][col], ']', len)))
			{
				*ptr = ')';
			}
		}

		/*
		**	Add the surrounding FACs
		*/
		if (col > 0)
		{
			wsb[hWsb]->vw->data[row][col-1] = (char)PLAIN_TEXT;
		}
		
		if ((col+len < WSB_COLS) && !FAC_FAC(wsb[hWsb]->vw->data[row][col+len]))
		{
			wsb[hWsb]->vw->data[row][col+len] = (char)PLAIN_TEXT;
		}		
	}
}

/*
**	ROUTINE:	wsb_add_field()
**
**	FUNCTION:	Add a field to the screen
**
**	DESCRIPTION:	If the field already exists them replace it.
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**	row		The fields row position (1-24)
**	col		The fields column position (1-80, 0=Center)
**	fac		The FAC for this field
**	field		The field value
**	len		The field length
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		The handle to the WSB structure.
**
**	WARNINGS:	None
**
*/
void wsb_add_field(HWSB hWsb, int row, int col, fac_t fac, const char *field, int len)
{
	if (check_hwsb(hWsb))
	{
		return;
	}

	row = check_row(row);
	col = check_col(col, len);
	len = check_field_len(len, col);

	/*
	**	Convert row and col to be zero based offsets.
	*/
	col--;
	row--;
	
	if (wisp_nativescreens())
	{
		int	idx;
		
		/*
		**	Put the text into the static area
		*/
		if (FAC_BLANK(fac))
		{
			memset(&wsb[hWsb]->na->data[row][col], ' ', len);
		}
		else
		{
			memcpy(&wsb[hWsb]->na->data[row][col], field, len);
		}

		/*
		**	Check if the field exists
		*/
		for(idx=0; idx < wsb[hWsb]->na->fields; idx++)
		{
			if (wsb[hWsb]->na->field_list[idx].row == (uint2)row+1 &&
			    wsb[hWsb]->na->field_list[idx].col == (uint2)col+1)
			{
				break;
			}
		}

		if (idx >= wsb[hWsb]->na->fields)
		{
			/*
			**	Field doesn't exist so add a new one
			*/
			
			if (wsb[hWsb]->na->fields >= MAX_WSB_FIELDS)
			{
				WL_werrlog_error(WERRCODE(104),"WSB", "MAXFIELDS", 
					"Max %d modifiable fields", MAX_WSB_FIELDS);
			}
			else
			{
				idx = wsb[hWsb]->na->fields;
				wsb[hWsb]->na->fields++;
			}
		}

		if (idx < MAX_WSB_FIELDS)
		{
			wsb[hWsb]->na->field_list[idx].row = (uint2)row+1;
			wsb[hWsb]->na->field_list[idx].col = (uint2)col+1;
			wsb[hWsb]->na->field_list[idx].len = (uint2)len;
			wsb[hWsb]->na->field_list[idx].fac = fac;
			
			memcpy(wsb[hWsb]->na->field_list[idx].field, field, len);
		}
	}
	else
	{
		memcpy(&wsb[hWsb]->vw->data[row][col], field, len);

		/*
		**	Add the surrounding FACs
		*/
		if (col > 0)
		{
			wsb[hWsb]->vw->data[row][col-1] = (char)fac;
		}
		
		if ((col+len < WSB_COLS) && !FAC_FAC(wsb[hWsb]->vw->data[row][col+len]))
		{
			wsb[hWsb]->vw->data[row][col+len] = (char)PLAIN_TEXT;
		}		
	}
}

/*
**	ROUTINE:	wsb_add_menu_item()
**
**	FUNCTION:	Add a menu item to the screen
**
**	DESCRIPTION:	Menu items consist of a tabstop followed by the text.
**			Each menu item has a pfkey associated with it.
**			If the user pressed enter while the cursor is positioned
**			at a menu item tabstop then the associated pfkey is returned.
**			Each menu item on a screen must have a unique pfkey.
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**	row		The row position (1-24)
**	col		The column position of the cursor tab character (2-78)
**	pfkey		The pseudo pfkey for this menu item (0=enter, 1-32, 33=help)
**	text		The menu text (NULL terminated)
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wsb_add_menu_item(HWSB hWsb, int row, int col, int pfkey, const char *text)
{
	int 	len;

	if (check_hwsb(hWsb))
	{
		return;
	}
	
	len = strlen(text);

	row = check_row(row);
	if (col < 2 || col > WSB_COLS-3) 
	{
		WL_wtrace("WSB","COLUMN","Invalid column value %d (using 2)",col);
		col = 2;
	}
	len = check_field_len(len, col+3);

	if (pfkey < 0 || pfkey > 33)
	{
		WL_wtrace("WSB","PFKEY","Invalid pfkey value %d (using 0)", pfkey);
		pfkey = 0;
	}

	/*
	**	If this is the first menu item then initialize the menumap
	*/
	if (!wsb[hWsb]->menumap)
	{
		wsb[hWsb]->menumap = (int(*)[NUM_PFKEYS])wisp_malloc(sizeof(*wsb[hWsb]->menumap));
		memset(wsb[hWsb]->menumap, '\0', sizeof(*wsb[hWsb]->menumap));
	}
	
	/*
	**	Store the screen position (1-1920) into the menumap indexed by the pfkey value
	*/
	if ((*wsb[hWsb]->menumap)[pfkey])
	{
		WL_wtrace("WSB","MENU","Duplicate pfkey [%d] is invalid.",pfkey);
		return;
	}			
	(*wsb[hWsb]->menumap)[pfkey] = (row-1)*WSB_COLS + col;
	
	/*
	**	Convert row and col to be zero based.
	*/
	col--;
	row--;

	if (wisp_nativescreens())
	{
		wsb_add_tabstop(hWsb, row+1, col+1);
		memcpy(&wsb[hWsb]->na->data[row][col+3], text, len);
	}
	else
	{
		/*
		**	Add the menu pick FACs
		*/
		wsb[hWsb]->vw->data[row][col-1] = (char)NUMPROT_FIELD;
		wsb[hWsb]->vw->data[row][col] = (char)WANG_MENU_PICK;

		memcpy(&wsb[hWsb]->vw->data[row][col+2], text, len);

		if ((col+2+len < WSB_COLS) && !FAC_FAC(wsb[hWsb]->vw->data[row][col+2+len]))
		{
			wsb[hWsb]->vw->data[row][col+2+len] = (char)PLAIN_TEXT;
		}		
	}
}

/*
**	ROUTINE:	wsb_add_tabstop()
**
**	FUNCTION:	Add a tabstop to the screen
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**	row		The row position (1-24)
**	col		The column position of the cursor tab character (2-77)
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wsb_add_tabstop(HWSB hWsb, int row, int col)
{
	if (check_hwsb(hWsb))
	{
		return;
	}

	row = check_row(row);
	if (col < 2 || col > WSB_COLS-3) 
	{
		WL_wtrace("WSB","COLUMN","Invalid column value %d (using 2)",col);
		col = 2;
	}
	
	/*
	**	Convert row and col to be zero based.
	*/
	col--;
	row--;

	if (wisp_nativescreens())
	{
		memcpy(&wsb[hWsb]->na->data[row][col-1], "[ ]", 3);
		wsb_add_field(hWsb, row+1, col+1, FAC_MOD_BLANK_LINE, " ", 1);
	}
	else
	{
		/*
		**	Add the menu pick FACs
		*/
		wsb[hWsb]->vw->data[row][col-1] = (char)NUMPROT_FIELD;
		wsb[hWsb]->vw->data[row][col]   = '-';
		wsb[hWsb]->vw->data[row][col+1] = (char)PLAIN_TEXT;
	}
}

/*
**	ROUTINE:	wsb_get_field()
**
**	FUNCTION:	Get a field from the screen
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**	row		The fields row position (1-24)
**	col		The fields column position (1-80, 0=Center)
**	field		The field value
**	len		The field length
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void wsb_get_field(HWSB hWsb, int row, int col, char *field, int len)
{
	if (check_hwsb(hWsb))
	{
		return;
	}

	row = check_row(row);
	col = check_col(col, len);
	len = check_field_len(len, col);

	/*
	**	Convert row and col to be zero based offsets.
	*/
	col--;
	row--;
	
	if (wisp_nativescreens())
	{
		memcpy(field, &wsb[hWsb]->na->data[row][col], len);
	}
	else
	{
		memcpy(field, &wsb[hWsb]->vw->data[row][col], len);
	}
}

void wsb_set_alarm(HWSB hWsb)
{
	if (check_hwsb(hWsb))
	{
		return;
	}

	wsb[hWsb]->sound_alarm = 1;
}


/*
**	ROUTINE:	wsb_display_and_read()
**
**	FUNCTION:	Display and read the screen.
**
**	DESCRIPTION:	WISP screens: 
**			Call vwang() to display and read the screen buffer.
**
**			Native screens:
**			Call "WACUWSB" to display the screen.
**
**			After screen is read then test for menu items and replace pfkey.
**
**			To do a DISPLAY without a READ pass a NULL for the pKeylist argument.
**
**
**	ARGUMENTS:	
**	hWsb		The handle to the WSB structure to be deleted
**	pKeylist	The valid function keys in ascii pairs (eg "00010316X") 
**			00=Enter, 01-32, 33=Help, NULL=Display Only with no read.
**	piPfkey		The returned pfkey that ended the read of the screen  0-33
**	piCurrow	Set/return the cursor row (0,1-24)
**	piCurcol	Set/return the cursor col (0,1-80)
**
**	GLOBALS:	
**	wsb		The WSB table.
**
**	RETURN:		None
**
**	WARNINGS:	If called while HELP is active then a pfkey 33 (HELP) can be returned
**			even if 33 is not in the termlist.
**
*/
void wsb_display_and_read(HWSB hWsb, const char* pKeylist, int *piPfkey, int *piCurrow, int *piCurcol)
{
	char	l_keylist[80];
	
	if (check_hwsb(hWsb))
	{
		return;
	}

	if (pKeylist)
	{
		strcpy(l_keylist,pKeylist);
	}
	else
	{
		/*
		**	No keys so this is a DISPLAY only without a READ.
		*/
		strcpy(l_keylist,"X");
	}

	if (wisp_nativescreens())
	{
		/*
		**	CALL "WACUWSB"/"WMFNWSB" USING
		**	[0]	static_text		PIC X(24*80)
		**	[1]	field_cnt		PIC 99			0-40
		**	[2]	field_table		PIC X(86) * 40	
		**	[3]	key_list		PIC X(80)
		**	[4]	key_cnt			UNSIGNED-SHORT		1-33
		**	[5]	term_key		UNSIGNED-SHORT		0-33
		**	[6]	cur_row			UNSIGNED-SHORT		1-24
		**	[7]	cur_col			UNSIGNED-SHORT		1-80
		**	[8]	sound_alarm		UNSIGNED-SHORT		0-1
		**	[9]	timeout			UNSIGNED-SHORT		0-65536 (0=No Timeout, n=Hundreds of seconds)
		*/
#define WACUWSB_ARGCNT	10
		char	*parms[WACUWSB_ARGCNT];
		int4	lens[WACUWSB_ARGCNT];
		int4	rc;
		uint2	field_cnt;
		uint2	key_cnt;
		uint2	term_key;
		uint2	cur_row;
		uint2	cur_col;
		uint2	alarm_flag;
		uint2	timeout_hsecs;
		char	*ptr;

		field_cnt = wsb[hWsb]->na->fields;

		cur_row = *piCurrow;
		cur_col = *piCurcol;
		
		/*
		**	Count number of keys in the key list
		*/
		key_cnt = 0;
		if (pKeylist)
		{
			if ((ptr = strchr(l_keylist,'X')))
			{
				*ptr = (char)0;
				key_cnt = strlen(l_keylist) / 2;
			}
			if (key_cnt < 1)
			{
				WL_wtrace("WSB","KEYLIST","Invalid keylist [%s]",l_keylist);
				key_cnt = 1;
				strcpy(l_keylist,"00X");
			}
		}

		alarm_flag = (wsb[hWsb]->sound_alarm) ? 1:0;
		timeout_hsecs = 0;

		parms[0] = (char *)wsb[hWsb]->na->data;
		lens[0]  = WSB_ROWS*WSB_COLS;

		parms[1] = (char*)&field_cnt;
		lens[1]  = sizeof(field_cnt);

		parms[2] = (char*)wsb[hWsb]->na->field_list;
		lens[2]  = sizeof(wsb[hWsb]->na->field_list);
		
		parms[3] = l_keylist;
		lens[3]  = 80;
		
		parms[4] = (char*)&key_cnt;
		lens[4]  = sizeof(key_cnt);
		
		parms[5] = (char*)&term_key;
		lens[5]  = sizeof(term_key);
		
		parms[6] = (char*)&cur_row;
		lens[6]  = sizeof(cur_row);
		
		parms[7] = (char*)&cur_col;
		lens[7]  = sizeof(cur_col);
		
		parms[8] = (char*)&alarm_flag;
		lens[8]  = sizeof(alarm_flag);
		
		parms[9] = (char*)&timeout_hsecs;
		lens[9]  = sizeof(timeout_hsecs);

		if (wisp_acu_cobol())
		{
			WL_call_acucobol("WACUWSB", WACUWSB_ARGCNT, parms, lens, &rc);
		}
		else if (wisp_mf_cobol())
		{
			WL_call_mfcobol("WMFNWSB", WACUWSB_ARGCNT, parms, lens, &rc);
		}

		if (rc)
		{
			int4	wrc, wcc;
			
			if (wisp_acu_cobol())
			{
				WL_call_acucobol_error(rc, &wrc, &wcc, "WACUWSB");
			}
			else if (wisp_mf_cobol())
			{
				/* trace error */
				WL_wtrace("DISPLAYANDREAD","WMFNWSB","RC = [%d]", rc);
			}

			*piPfkey = 0;
			*piCurcol = 1;
			*piCurrow = 1;
		}
		else
		{
			int i;
			
			/*
			**	Load the field data into the screen buffer so it can
			**	be retrieved with wsb_get_field().
			*/
			for(i=0; i<field_cnt; i++)
			{
				int	row,col,len;
				fac_t	fac;

				row = wsb[hWsb]->na->field_list[i].row - 1;
				col = wsb[hWsb]->na->field_list[i].col - 1;
				len = wsb[hWsb]->na->field_list[i].len;
				fac = wsb[hWsb]->na->field_list[i].fac;

				/*
				**	If UPPER fac then ensure the text is uppercase.
				*/
				if ( FAC_UPPER(fac) && !FAC_PROTECTED(fac))
				{
					WL_upper_mem(wsb[hWsb]->na->field_list[i].field,len);
				}
				
				memcpy(&wsb[hWsb]->na->data[row][col], wsb[hWsb]->na->field_list[i].field, len);
			}

			/*
			**	Prepare return values
			*/
			*piPfkey = term_key;
			*piCurcol = cur_col;
			*piCurrow = cur_row;
		}
	}
	else
	{
		unsigned char	function, lines, no_mod[2];
		char term[2];

		no_mod[0] = no_mod[1] = ' ';
		
		wsb[hWsb]->vw->oa.row = 1;
		wsb[hWsb]->vw->oa.currow = *piCurrow;
		wsb[hWsb]->vw->oa.curcol = *piCurcol;
		
		if (pKeylist)
		{
			wsb[hWsb]->vw->oa.wcc = POSITION_CURSOR|UNLOCK_KEYBOARD; /* 0xA0 = 0x20 + 0x80  */
			function = DISPLAY_AND_READ_ALTERED;
		}
		else
		{
			/*
			**	No keys so do a DISPLAY only.
			*/
			wsb[hWsb]->vw->oa.wcc = POSITION_CURSOR;
			function = WRITE_ALL;
		}

		if (wsb[hWsb]->sound_alarm)
		{
			wsb[hWsb]->vw->oa.wcc |= SOUND_ALARM;
		}
		
		lines = 24;

		/* 
		**	Display and read the screen	
		*/
		vwang(&function, (unsigned char*)(wsb[hWsb]->vw), &lines, l_keylist, term, no_mod);	


		/*
		**	Prepare return values
		*/
		if (AID_HELP == no_mod[1])
		{
			*piPfkey = 33;
		}
		else
		{
			*piPfkey = ((term[0] - '0') * 10) + (term[1] - '0');
		}
		*piCurcol = wsb[hWsb]->vw->oa.curcol;
		*piCurrow = wsb[hWsb]->vw->oa.currow;
	}

	/*
	**	Check for menu items if there is a menumap and the enter key was pressed
	*/
	if (wsb[hWsb]->menumap && 0==*piPfkey)
	{
		int pos;
		int pfkey;
		
		pos = (*piCurrow-1)*WSB_COLS + *piCurcol;
		
		for(pfkey=0; pfkey<NUM_PFKEYS; pfkey++)
		{
			if (pos == (*wsb[hWsb]->menumap)[pfkey])
			{
				*piPfkey = pfkey;
				break;
			}
		}
		
	}

	/*
	**	Validate the results
	*/
	if (*piPfkey < 0 || *piPfkey > 33) 
	{
		WL_wtrace("WSB","TERMKEY","Invalid pfkey [%d] (using 0)", *piPfkey);
		*piPfkey = 0;
	}

	if (*piCurrow < 1 || *piCurrow > WSB_ROWS) 
	{
		WL_wtrace("WSB","CURROW","Invalid cursor row [%d] (using 1)", *piCurrow);
		*piCurrow = 1;
	}

	if (*piCurcol < 1 || *piCurcol > WSB_COLS) 
	{
		WL_wtrace("WSB","CURCOL","Invalid cursor column [%d] (using 1)", *piCurcol);
		*piCurcol = 1;
	}

	/*
	**	Clear one time flags
	*/
	wsb[hWsb]->sound_alarm = 0;
}

/*
**	History:
**	$Log: wsb.c,v $
**	Revision 1.20  2003/08/28 15:13:44  gsl
**	For native screens ensure UPPER fields are converted to uppercase
**	
**	Revision 1.19  2003/08/27 17:55:42  gsl
**	MF Native Screens
**	
**	Revision 1.18  2003/08/25 21:10:17  gsl
**	MF Native Screens
**	
**	Revision 1.17  2003/02/04 17:22:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.16  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.15  2002/12/09 19:15:38  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.14  2002/08/01 14:09:09  gsl
**	type warnings
**	
**	Revision 1.13  2002/08/01 02:41:38  gsl
**	fix type warning
**	
**	Revision 1.12  2002/07/15 13:28:59  gsl
**	IVS_ globals
**	
**	Revision 1.11  2002/07/12 20:40:41  gsl
**	Global unique WL_ changes
**	
**	Revision 1.10  2002/07/10 21:05:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/02 21:15:36  gsl
**	Rename wstrdup
**	
**	Revision 1.8  1998/10/13 18:57:46  gsl
**	hWsb is unsigned so can't be < 0
**	
**	Revision 1.7  1998-07-09 16:16:56-04  gsl
**	Initialize the no_mod arg to vwang()
**
**	Revision 1.6  1998-03-16 13:22:30-05  gsl
**	Moved the IVS_outctx vchinese logic from wshelp to wsb_add_text()
**
**	Revision 1.5  1998-03-13 18:01:06-05  gsl
**	Add wsb_set_alarm()
**	Increase number of fields to 40
**	add wsb_add_tabstop()
**
**	Revision 1.4  1997-12-05 16:05:33-05  gsl
**	Add missing includes
**
**	Revision 1.3  1997-12-01 15:46:08-05  gsl
**	Add support to DISPLAY only without a READ.
**
**	Revision 1.2  1997-10-29 12:38:36-05  gsl
**	Write
**
**	Revision 1.1  1997-10-24 15:08:01-04  gsl
**	Initial revision
**
**
**
**
*/
