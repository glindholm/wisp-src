static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		costar.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Wisp For Windows COSTAR routines
**
**			This file contains both COSTAR only routines plus
**			generic mouse handling/hotspot routines.
**
**	Routines:	
**	use_costar()			Test if using costar terminal emulator.
**	costar_fac()			Map FAC's for use with COSTAR
**	costar_enable_mouse()		To enable/disable the costar mouse script.
**	costar_get_mouse_position()	Read the mouse position
**	costar_ctrl_api()			Send API control strings to COSTAR.
**	costar_errtext()		Set the COSTAR status error text box message.
*/

#if defined(unix) || defined(VMS) || defined(WIN32)

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "costar.h"
#include "idsistd.h"
#include "vwang.h"
#include "wperson.h"
#include "werrlog.h"
#include "idsisubs.h"
#include "wmalloc.h"
#include "wisplib.h"

#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vcap.h"
#include "vraw.h"

/*
**	Structures and Defines
*/
#define COSTAR_MOUSE_SCRIPT	"w4w\\mouse.scr"
#define COSTAR_FLAG_ENV		"W4W"

#define COSTAR_MAGIC_STRING 	"\177"
#define COSTAR_MAGIC_CHAR	'\177'

struct w4w_pfkey_map_s
{
	struct w4w_pfkey_map_s	*next;
	char	*string;
	int	code;				/* 0 = ENTER, 1-32 = PFKEY, 33 = HELP */
};
typedef struct w4w_pfkey_map_s w4w_pfkey_map_s;

/*
**	Globals and Externals
*/

/*
**	Static data
*/
static char magic_char = COSTAR_MAGIC_CHAR;

/*
**	Function Prototypes
*/
#ifdef COSTAR
static void costar_raw_api(const char *buff);
#endif /* COSTAR */

static w4w_pfkey_map_s *load_w4wmap(void);

#ifdef COSTAR
/*
**	ROUTINE:	use_costar()
**
**	FUNCTION:	Test if using costar terminal emulator.
**
**	DESCRIPTION:	Check the environment variable $COSTAR and if
**			set then we are using costar.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:
**	1		Using COSTAR version 1
**	0		Not using COSTAR
**
**	WARNINGS:	None
**
*/
int use_costar(void)
{
	static int first = 1;
	static int costar_flag = 0;

	if (first)
	{
		if (!wbackground())  /* No COSTAR in background */
		{
#ifdef WIN32
			costar_flag = !vrawdirectio();
#else
			char	*ptr;
			if (ptr = getenv(COSTAR_FLAG_ENV))
			{
				if (0==strcmp(ptr,"1"))
				{
					costar_flag = 1;
				}
				else
				{
					/* Unknown version */
				}
			}
#endif
		}
		first = 0;
	}

	return costar_flag;
}

/*
**	ROUTINE:	costar_fac()
**
**	FUNCTION:	Map FAC's for use with COSTAR
**
**	DESCRIPTION:	Map all modifiable FAC's to have the UNDERSCORE bit on.
**			Map all protected FAC's to have the UNDERSCORE bit off.
**			Also make DIM+UNDERSCORE into BRIGHT.
**
**			The purpose for this is to provide a consistent set of
**			attributes for the COSTAR emulator.
**
**	ARGUMENTS:
**	the_fac		The FAC before mapping.
**
**	GLOBALS:	None
**
**	RETURN:		The mapped FAC
**
**	WARNINGS:	None
**
*/
unsigned char costar_fac(int the_fac)
{
	int	new_fac;

	new_fac = the_fac;

	if (use_costar() && the_fac >= 128 && the_fac <= 255)
	{
		if ( FAC_PROTECTED(the_fac) )
		{
			/*
			**	TEXT Field: Turn off the underscore.
			*/
			new_fac = FAC_CLEAR_UNDERSCORED(new_fac);

			if ( FAC_UNDERSCORED(the_fac) && FAC_DIM(the_fac) )
			{
				/*
				**	If FAC had an UNDERSCORED and DIM then make it BRIGHT.
				**
				**	DIM	->	BRIGHT
				**	BRIGHT	->	BRIGHT
				**	BLINK	->	BLINK
				**	BLANK	->	BLANK
				*/
				new_fac = FAC_SET_BRIGHT(new_fac);
			}
		}
		else
		{
			/*
			**	EDIT Field: Turn on the underscore.
			*/
			new_fac = FAC_SET_UNDERSCORED(new_fac);

			if ( FAC_UNDERSCORED(the_fac) && FAC_DIM(the_fac) )
			{
				/*
				**	If FAC had an UNDERSCORED and DIM then make it BRIGHT
				**
				**	DIM	->	BRIGHT
				**	BRIGHT	->	BRIGHT
				**	BLINK	->	BLINK
				**	BLANK	->	BLANK
				*/
				new_fac = FAC_SET_BRIGHT(new_fac);
			}
		}
	}

	return new_fac;
}

/*
**	ROUTINE:	costar_enable_mouse()
**
**	FUNCTION:	To enable/disable the costar mouse script.
**
**	DESCRIPTION:	This routine is used to enable the mouse before
**			a read then disable it after a read.
**
**	ARGUMENTS:
**	flag		1=enable, 2=disable
**
**	GLOBALS:
**	vkeyvalue(GENERIC_MOUSE)
**			The mouse prefix to use which is either loaded from vcap file or defaults.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void costar_enable_mouse(int flag)
{
	static int first = 1;
	static char mouse_on[80], mouse_off[80];

	if (first)
	{
		/*
		**	The first time in we build the control strings
		*/
		sprintf(mouse_on,  "%cUWSCRIPT%s,%s%c",  magic_char, COSTAR_MOUSE_SCRIPT, vkeyvalue(GENERIC_MOUSE), magic_char);
		sprintf(mouse_off, "%cUWSCRIPTSTOP%s%c", magic_char, COSTAR_MOUSE_SCRIPT, magic_char);
		first = 0;
	}

	if (flag)
	{
		costar_raw_api(mouse_on);
	}
	else
	{
		costar_raw_api(mouse_off);
	}
}

/*
**	ROUTINE:	costar_get_mouse_position()
**
**	FUNCTION:	Read the mouse position
**
**	DESCRIPTION:	This routines is called after the GENERIC_MOUSE prefix meta 
**			character is read to retieve the row and column of the mouse.
**
**			The input string:	{prefix}ROW,COLUMN<CR>
**
**			The prefix string has already been removed when this routine
**			is called.
**
**			Read until a <CR> then scan in the row and col.  The row and
**			col will be one based so convert to zero based.
**
**	ARGUMENTS:
**	m_row		The mouse row (0-23)
**	m_col		The mouse col (0-79)
**
**	GLOBALS:	None
**
**	RETURN:
**	0		Success
**	1		Unrecognized input string.
**
**	WARNINGS:	None
**
*/
int costar_get_mouse_position(int *m_row, int *m_col)
{
	char	buff[40];
	int	the_meta_char;
	int	i;

	for(i=0;i<8;i++)
	{
		the_meta_char = vgetm();
		if ((the_meta_char == return_key) || (the_meta_char == enter_key)) 
		{
			break;
		}
		buff[i] = (char)the_meta_char;
	}
	buff[i] = (char)0;

	if ( sscanf(buff,"%d,%d",m_row,m_col) != 2)
	{
		/*
		**	Error - Unrecognized input string
		*/
		*m_row = 0;
		*m_col = 0;
		return 1;
	}
	else
	{
		/*
		**	Convert to zero based
		*/
		*m_row -= 1;
		*m_col -= 1;
		return 0;
	}
}

/*
**	ROUTINE:	costar_raw_api()
**
**	FUNCTION:	Send the raw control sequence to costar.
**
**	DESCRIPTION:	Use the video routines to send the control string to costar.
**
**	ARGUMENTS:
**	buff		The control string.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void costar_raw_api(const char *buff)
{
	if (!use_costar()) return;

	vwang_flush();
	vcontrol((char*)buff);
}

/*
**	ROUTINE:	costar_ctrl_api()
**
**	FUNCTION:	Send API control strings to COSTAR.
**
**	DESCRIPTION:	Take a COSTAR API control string and and the COSTAR
**			magic numbers then transmit it.
**
**	ARGUMENTS:
**	control		The API string (without magic numbers)
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void costar_ctrl_api(const char *control)
{
	if (!use_costar()) return;

	costar_raw_api(COSTAR_MAGIC_STRING);
	costar_raw_api(control);
	costar_raw_api(COSTAR_MAGIC_STRING);
}

/*
**	ROUTINE:	costar_errtext()
**
**	FUNCTION:	Set the COSTAR status error text box message.
**
**	DESCRIPTION:	Accept a message string and build and send the API control string.
**
**	ARGUMENTS:
**	message		The new message (pass "" to clear)
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void costar_errtext(char *message)
{
	char	buff[80];

	sprintf(buff,"%cUWERR%s,0%c", magic_char, message, magic_char);
	costar_raw_api(buff);
}

/*
**	ROUTINE:	costar_messtext()
**
**	FUNCTION:	Set the COSTAR status message text box message.
**
**	DESCRIPTION:	Accept a message string and build and send the API control string.
**
**	ARGUMENTS:
**	message		The new message (pass "" to clear)
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void costar_messtext(char *message)
{
	char	buff[80];

	sprintf(buff,"%cUWMSG%s%c", magic_char, message, magic_char);
	costar_raw_api(buff);
}
#endif /* COSTAR */


#ifdef W4W
/*
**	ROUTINE:	load_hotspots()
**
**	FUNCTION:	Load the hotspots table
**
**	DESCRIPTION:	Read the hotspots file and load the table.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		The hotspots table.
**
**	WARNINGS:	None
**
*/
static char **load_hotspots(void)
{
#ifdef COSTAR
#define MAX_HOTSPOTS	100
	static char *hotspots[MAX_HOTSPOTS+1];
	static int first=1;
	
	if (first)
	{
		/*
		**	The first time in load all the hotspots from the config file
		*/
		char	path[256];
		FILE	*fh;
		int	i;

		i = 0;
		
		build_wisp_config_path("HOTSPOTS",path);

		if (fh = fopen(path,"r"))
		{
			for(;i<MAX_HOTSPOTS;)
			{
				char	buff[256];

				if ( 1 == fscanf(fh,"%s",buff) )
				{
					hotspots[i++] = wstrdup(buff);
				}
				else if (feof(fh))
				{
					break;
				}
			}

			fclose(fh);
		}

		hotspots[i++] = NULL;

		first = 0;
	}
	
	return hotspots;
#else
	return NULL;
#endif
}

/*
**	ROUTINE:	W4WAPI()
**
**	FUNCTION:	Raw W4W API routine callable from COBOL
**
**	DESCRIPTION:	This is the lowest level (raw) API support.
**
**	ARGUMENTS:
**	buff		The buffer with the API string. It must	be null terminated.
**			The <127> control characters must be included.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	No validation is preformed.
**
*/
void W4WAPI(char *buff)
{
#ifdef COSTAR
	costar_raw_api(buff);
#endif /* COSTAR */
}


/*
**	ROUTINE:	load_w4wmap()
**
**	FUNCTION:	Load the W4WMAP config file into the internal structure.
**
**	DESCRIPTION:	This routine should be called once at the beginning of
**			screen processing to load the table.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	???
**
**	RETURN:		The loaded w4wmap linked list head.
**
**	WARNINGS:	None
**
*/
static w4w_pfkey_map_s *load_w4wmap(void)
{
	static int first = 1;
	static w4w_pfkey_map_s	*w4w_pfkey_head = NULL;

	char	errbuff[256];
	
	if (first)
	{
		char	path[256];
		FILE	*fh;
		
		first = 0;
		
		build_wisp_config_path("W4WMAP",path);
		if (fh = fopen(path,"r"))
		{
			char	buff[256];
			char	keyword[256];
			char	string[256];
			int	cnt;
			int	pfkey_code;
			w4w_pfkey_map_s *pfk;

			while(fgets(buff,sizeof(buff),fh))
			{
				/*
				**	A leading # or blank line is treated as a comment.
				*/
				if ('#'  == buff[0] ||
				    '\n' == buff[0]    ) continue;

				cnt = sscanf(buff,"%s \"%[^\"]\"",keyword,string);
				if (0 == cnt) continue;
				if (2 != cnt)
				{
					sprintf(errbuff,"%%W4W-W-MAP Invalid entry [%s]\n",buff);
					werr_write(errbuff);
					continue;
				}

			        pfkey_code = -1;
				upper_string(keyword);
				if (0==strcmp("ENTER",keyword))
				{
					pfkey_code = 0;
				}
				else if (0==strcmp("HELP",keyword))
				{
					pfkey_code = 33;
				}
				else if (0==memcmp("PF-",keyword,3))
				{
					sscanf(keyword,"PF-%d",&pfkey_code);
					if (pfkey_code < 1 || pfkey_code > 32)
					{
						pfkey_code = -1;
					}
				}

				if (-1 == pfkey_code)
				{
					sprintf(errbuff,"%%W4W-W-MAP Invalid keyword [%s]\n",buff);
					werr_write(errbuff);
					continue;
				}

				pfk = wmalloc(sizeof(w4w_pfkey_map_s));
				pfk->next = w4w_pfkey_head;
				pfk->code = pfkey_code;
				pfk->string = wstrdup(string);

				w4w_pfkey_head = pfk;

/* printf("Keyword=%s pfk->code=%d pfk->string=[%s]\n\r",keyword,pfk->code,pfk->string); */
				
			}
			fclose(fh);
		}
	}

	return w4w_pfkey_head;
}

/*
**	ROUTINE:	w4w_mask_row()
**
**	FUNCTION:	Return a mask of the row identifing which columns a mouse clickable.
**
**	DESCRIPTION:	This routine scans a row and returns a mask which identifies
**			which columns should be raised (reversed) to identify as mouse
**			clickable.  This routine uses both the W4WMAP and HOTSPOTS files.
**
**			The variable the_row must contain a copy of the row with all
**			FACs and non-printables changed to spaces and all modifiable
**			fields also changed to spaces.  The returned mask will be a
**			string the same length as the_row consisting of blanks and 'X's
**			with the 'X's representing columns that are mouse clickable.
**			The mask can then be used to identify which columns to 
**			print in reversed video.
**
**	ARGUMENTS:
**	rownum		The number of the row
**	the_row		The stripped and cleaned row. (null terminated)
**	the_mask	The mask to return consisting of Blanks and 'X's (same length as the_row)
**
**	GLOBALS:	The HOTSPOTS and W4WMAP structures.
**
**	RETURN:
**	0		Nothing to mask
**	1		At least one 'X' in the mask.
**
**	WARNINGS:	None
**
*/
int w4w_mask_row(int rownum, char *the_row, char *the_mask)
{
	static int first = 1;
	static char **hotspots = NULL;
	static w4w_pfkey_map_s	*w4w_pfkey_head = NULL;

	int	rc;

	if (first)
	{
		first = 0;
		
		/*
		**	The first time in load all the hotspots from the config file
		*/
		hotspots = load_hotspots();
		w4w_pfkey_head = load_w4wmap();
	}

	/*
	**	Initialize the mask to spaces.
	*/
	memset(the_mask, ' ', strlen(the_row));
	rc = 0;	
	
	if (hotspots)
	{
		int	hot_idx;
		char	*row_ptr, *hot_ptr;
		int	offset;

		for(hot_idx=0; hotspots[hot_idx] && *hotspots[hot_idx]; hot_idx++)
		{
			/*
			**	For each hotspot search the line
			*/
	
			for(row_ptr = the_row; hot_ptr = strstr(row_ptr,hotspots[hot_idx]); row_ptr = hot_ptr + 1)
			{
				/*
				**	Find each occurance of the hotspot on the line
				*/
	
				offset = hot_ptr - the_row;
	
				/*
				**	Hotspot must be preceeded with a space. (Left-match)
				*/
				if ( 0 == offset || ' ' == the_row[offset-1] )
				{
					rc = 1;
					memset(&the_mask[offset], 'X', strlen(hotspots[hot_idx]));
				}
			}
		}
	}

	if (w4w_pfkey_head)
	{
		w4w_pfkey_map_s	*pfkey_curr;
		char	*row_ptr, *hot_ptr;
		int	offset;

		for(pfkey_curr=w4w_pfkey_head; pfkey_curr; pfkey_curr = pfkey_curr->next)
		{
			/*
			**	For each pfkey string search the line
			*/
	
			for(row_ptr = the_row; hot_ptr = strstr(row_ptr,pfkey_curr->string); row_ptr = hot_ptr + 1)
			{
				/*
				**	Find each occurance of the hotspot on the line
				*/
	
				offset = hot_ptr - the_row;
	
				/*
				**	Hotspot must be preceeded with a space. (Left-match)
				*/
				if ( 0 == offset || ' ' == the_row[offset-1] )
				{
					rc = 1;
					memset(&the_mask[offset], 'X', strlen(pfkey_curr->string));
				}
			}
		}
		
	}
	
	return rc;
}

/*
**	ROUTINE:	w4w_click_row()
**
**	FUNCTION:	Scan a row that was mouse clicked on to see if a hotspot was hit.
**
**	DESCRIPTION:	Scan the row for w4wmap pfkey strings and if one is found
**			on the click column offset then return the pfkey code.
**
**	ARGUMENTS:
**	rownum		The number of the row
**	click_offset	The column offset in the row where the mouse was clicked.
**	the_row		The stripped and cleaned row. (null terminated) (See w4w_mask_row())
**
**	GLOBALS:	None
**
**	RETURN:
**	-1		Not a hotspot
**	0		Enter-Key
**	1-32		The Pf-Key
**	33		Help-Key
**
**	WARNINGS:	None
**
*/
int w4w_click_row(int rownum, int click_offset, char *the_row)
{
	static int first = 1;
	static w4w_pfkey_map_s	*w4w_pfkey_head = NULL;

	int	rc;

	if (first)
	{
		/*
		**	The first time in load all the hotspots from the config file
		*/
		w4w_pfkey_head = load_w4wmap();
		first = 0;
	}

	rc = -1;

	if (w4w_pfkey_head)
	{
		w4w_pfkey_map_s	*pfkey_curr;
		char	*row_ptr, *hot_ptr;
		int	offset;

		for(pfkey_curr=w4w_pfkey_head; pfkey_curr; pfkey_curr = pfkey_curr->next)
		{
			/*
			**	For each pfkey string search the line
			*/
	
			for(row_ptr = the_row; hot_ptr = strstr(row_ptr,pfkey_curr->string); row_ptr = hot_ptr + 1)
			{
				/*
				**	Find each occurance of the hotspot on the line
				*/
	
				offset = hot_ptr - the_row;
				if (click_offset < offset) 
				{
					/*
					**	We have gone past the click column so break out.
					*/
					break;
				}
				
				
				/*
				**	Hotspot must be preceeded with a space. (Left-match)
				*/
				if ( 0 == offset || ' ' == the_row[offset-1] )
				{
					if (click_offset < (offset + (int)strlen(pfkey_curr->string)))
					{
						return pfkey_curr->code;
					}
				}
			}
		}
		
	}

	return rc;
}
#endif /* W4W */

#endif /* unix || VMS || WIN32 */

/*
**	History:
**	$Log: costar.c,v $
**	Revision 1.21  1997-08-25 16:14:48-04  gsl
**	Make costar_ctrl_api() external so it can be used from vwang()
**
**	Revision 1.20  1997-07-15 21:53:24-04  gsl
**	Added check if in background use_costar() will always return false.
**	Probable cause of bug when a submitted routine fails doing a link
**	when W4W=1.
**
**	Revision 1.19  1997-07-12 18:59:47-04  gsl
**	Add support for WIN32
**
**	Revision 1.18  1996-08-27 20:24:57-04  gsl
**	The HOTSPOTS file is only processed for COSTAR not for NT (W4W)
**
**	Revision 1.17  1996-08-16 11:10:43-07  gsl
**	Split into COSTAR and W4W code
**
**	Revision 1.16  1996-08-15 17:22:05-07  gsl
**	Include with WIN32 code for mouse handling
**
**	Revision 1.15  1996-07-17 14:49:56-07  gsl
**	Change to use wmalloc() and wdupstr()
**
**	Revision 1.14  1996-07-17 09:16:25-07  gsl
**	Fix for NT
**
**	Revision 1.13  1996-07-16 11:30:52-07  gsl
**	Fix the video includes
**
**	Revision 1.12  1995-07-06 10:01:58-07  gsl
**	add w4w_click_row() which will scan a row and return a pfkey code
**	if the mouse clicked on a hotspot.
**
 * Revision 1.11  1995/07/05  16:54:30  gsl
 * add w4w_mask_row() and load_w4wmap() as the first step in W4WMAP
 * screen scraping stuff - start with PFKEY support only.
 *
 * Revision 1.10  1995/06/26  11:00:40  gsl
 * added the costar_load_hotspots() routine and the costar_raw_api()
 *
 * Revision 1.9  1995/06/21  12:56:22  gsl
 * add W4WAPI()
 *
 * Revision 1.8  1995/06/13  09:12:12  gsl
 * fix ambiquous syntax x=&y --> x = &y
 *
 * Revision 1.7  1995/05/01  09:45:42  gsl
 * change costar_message() to costar_errtext() and add costar_messtext()
 *
 * Revision 1.6  1995/04/25  09:52:33  gsl
 * drcs state V3_3_15
 *
 * Revision 1.5  1995/04/17  11:45:58  gsl
 * drcs state V3_3_14
 *
 * Revision 1.4  1995/04/13  13:55:06  gsl
 * Change to use env var W4W=1
 *
 * Revision 1.3  1995/04/05  11:51:14  gsl
 * Use w4w\mouse.scr file
 *
 * Revision 1.2  1995/04/05  10:01:43  gsl
 * Add some doc.
 *
 * Revision 1.1  1995/04/05  09:34:40  gsl
 * Initial revision
 *
**
**
*/
