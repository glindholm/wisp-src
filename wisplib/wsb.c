static char copyright[]="Copyright (c) 1997 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		wsb.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Construct, display, and retrieve screen buffers
**
**			These routines provide a common front-end which can
**			be used with vwang or with nativescreens.
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
	char fac;			/* PIC X COMP-X 	*/
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
extern void call_acucobol_error(int rc, int4 *wang_retcode, int4 *wang_compcode, char *link_filespec);

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
		werrlog(104,"%%WSB-E-HWSB Invalid handle to WSB",0,0,0,0,0,0,0);
		return 1;
	}

	return 0;
}

/* Validate a row value */
static int check_row(int row)
{
	if (row < 1 || row > WSB_ROWS)
	{
		wtrace("WSB","ROW","Invalid row value %d",row);
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
		wtrace("WSB","COLUMN","Invalid column value %d",col);
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
		wtrace("WSB","LENGTH","Invalid text length value %d",len);
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
		wtrace("WSB","LENGTH","Invalid field length value %d",len);
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
		werrlog(104,"%%WSB-F-MAX Maximum number of WSB handles exceeded",0,0,0,0,0,0,0);
		wexit(1);
	}

	/*
	**	Allocate and initialize the WSB structure
	*/
	wsb[idx] = (wsb_t *)wmalloc(sizeof(wsb_t));
	wsb[idx]->vw = (wsbvwang_t *)NULL;
	wsb[idx]->na = (wsbnative_t *)NULL;
	wsb[idx]->menumap = NULL;
	wsb[idx]->sound_alarm = 0;
	
	if (nativescreens())
	{
		int	i;

		wsb[idx]->na = (wsbnative_t *)wmalloc(sizeof(wsbnative_t));
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
			wsb[idx]->na->field_list[i].row = 24;
			wsb[idx]->na->field_list[i].col = 80;
			wsb[idx]->na->field_list[i].len =  1;
		}
	}
	else
	{
		wsb[idx]->vw = (wsbvwang_t *)wmalloc(sizeof(wsbvwang_t));
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

	if (nativescreens())
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
	
	if (nativescreens())
	{
		memcpy(&wsb[hWsb]->na->data[row][col], text, len);
	}
	else
	{
		memcpy(&wsb[hWsb]->vw->data[row][col], text, len);

		/*
		**	If chinese then change '[' and ']' into '(' and )'.
		*/
		if (outctx)
		{
			char *ptr;
			
			while(ptr = memchr(&wsb[hWsb]->vw->data[row][col], '[', len))
			{
				*ptr = '(';
			}
			while(ptr = memchr(&wsb[hWsb]->vw->data[row][col], ']', len))
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
	
	if (nativescreens())
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
				werrlog(104,"%%WSB-E-MAXFIELDS Max 40 modifiable fields",0,0,0,0,0,0,0);
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
		wtrace("WSB","COLUMN","Invalid column value %d (using 2)",col);
		col = 2;
	}
	len = check_field_len(len, col+3);

	if (pfkey < 0 || pfkey > 33)
	{
		wtrace("WSB","PFKEY","Invalid pfkey value %d (using 0)", pfkey);
		pfkey = 0;
	}

	/*
	**	If this is the first menu item then initialize the menumap
	*/
	if (!wsb[hWsb]->menumap)
	{
		wsb[hWsb]->menumap = (int(*)[NUM_PFKEYS])wmalloc(sizeof(*wsb[hWsb]->menumap));
		memset(wsb[hWsb]->menumap, '\0', sizeof(*wsb[hWsb]->menumap));
	}
	
	/*
	**	Store the screen position (1-1920) into the menumap indexed by the pfkey value
	*/
	if ((*wsb[hWsb]->menumap)[pfkey])
	{
		wtrace("WSB","MENU","Duplicate pfkey [%d] is invalid.",pfkey);
		return;
	}			
	(*wsb[hWsb]->menumap)[pfkey] = (row-1)*WSB_COLS + col;
	
	/*
	**	Convert row and col to be zero based.
	*/
	col--;
	row--;

	if (nativescreens())
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
		wtrace("WSB","COLUMN","Invalid column value %d (using 2)",col);
		col = 2;
	}
	
	/*
	**	Convert row and col to be zero based.
	*/
	col--;
	row--;

	if (nativescreens())
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
	
	if (nativescreens())
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

	if (nativescreens())
	{
		/*
		**	CALL "WACUWSB" USING
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
			if (ptr = strchr(l_keylist,'X'))
			{
				*ptr = (char)0;
				key_cnt = strlen(l_keylist) / 2;
			}
			if (key_cnt < 1)
			{
				wtrace("WSB","KEYLIST","Invalid keylist [%s]",l_keylist);
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

		call_acucobol("WACUWSB", WACUWSB_ARGCNT, parms, lens, &rc);

		if (rc)
		{
			int4	wrc, wcc;
			
			call_acucobol_error(rc, &wrc, &wcc, "WACUWSB");

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

				row = wsb[hWsb]->na->field_list[i].row - 1;
				col = wsb[hWsb]->na->field_list[i].col - 1;
				len = wsb[hWsb]->na->field_list[i].len;
				
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
		unsigned char	function, lines, term[2], no_mod[2];

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
		vwang(&function, wsb[hWsb]->vw, &lines, l_keylist, term, no_mod);	


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
		wtrace("WSB","TERMKEY","Invalid pfkey [%d] (using 0)", *piPfkey);
		*piPfkey = 0;
	}

	if (*piCurrow < 1 || *piCurrow > WSB_ROWS) 
	{
		wtrace("WSB","CURROW","Invalid cursor row [%d] (using 1)", *piCurrow);
		*piCurrow = 1;
	}

	if (*piCurcol < 1 || *piCurcol > WSB_COLS) 
	{
		wtrace("WSB","CURCOL","Invalid cursor column [%d] (using 1)", *piCurcol);
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
**	Revision 1.8  1998-10-13 14:57:46-04  gsl
**	hWsb is unsigned so can't be < 0
**
**	Revision 1.7  1998-07-09 16:16:56-04  gsl
**	Initialize the no_mod arg to vwang()
**
**	Revision 1.6  1998-03-16 13:22:30-05  gsl
**	Moved the outctx vchinese logic from wshelp to wsb_add_text()
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