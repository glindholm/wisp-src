/*
** Copyright (c) Shell Stream Software LLC. All Rights Reserved.
** WISP - Wang Interchange Source Processor
** $Id:$
**
*/


/*						Local data definitions								*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>									/* Include character typing functions.	*/
#include <string.h>

#include "idsistd.h"
#include "wcommon.h"
#include "cobrun.h"
#include "werrlog.h"
#include "wexit.h"
#include "vwang.h"
#include "cobpic.h"
#include "wisplib.h"
#include "wperson.h"
#include "costar.h"
#include "wmalloc.h"
#include "wglobals.h"

#define MOD_BIT   64

static int  ver20;
static int  ver21;
static int  ver22;
static int  ver90;									/* 5-BYTE FACs				*/

static int  size_of_fac;								/* 1 (normal) or 5 (ver90)		*/

static int	static_screen;
static const unsigned char *reserved_space;

static int nativecharmap = -1;		/* Flag to do automatic CHARMAP translation to native charset */


struct pic_struct
{
	int	ps_size;
	int	ps_type;
	int	ps_dp;
	int	ps_blankdecimal;
	int	ps_signed;
	int	ps_suppidx;
	int	ps_floatidx;
	char	ps_suppchar;
	char	ps_xpic[80+1];
};

#define MAX_ITEM_TABLE 400

static struct
{
	unsigned char 	*it_dataptr;				/* Ptr to FAC & DATA						*/
	short	it_v1;						/* Vertical(1) occurs.						*/
	short	it_v1_off;					/* Vertical(1) offset. (Number of rows)				*/
	short	it_v2;						/* Vertical(2) occurs.						*/
	short	it_v2_off;					/* Vertical(2) offset. (Number of rows)				*/
	short	it_h;						/* Horizontal occurs.						*/
	short	it_size;					/* Size of one element.						*/
	short	it_row;						/* Starting row.						*/
	short	it_col;						/* Starting column						*/
	struct pic_struct it_ps;				/* Decoded picture data.					*/
} item_table[MAX_ITEM_TABLE];

static int 	num_items;

struct v_struct
{
	int	v_occurs;					/* Number of occurs.						*/
	int	v_level;					/* Level number of occurs.					*/
	int	v_item;						/* Item number of start of occurs.				*/
	int	v_low;						/* Lowest row number.						*/
	int	v_high;						/* Highest row number.						*/
};

struct mask_struct
{
	uint4 cmask;						/* Comma mask.					*/
	uint4 zmask;						/* Z mask.					*/
};

struct screen_block 
{
	unsigned char order[4];						/* The order area,				*/
	unsigned char scrn[1920];					/* and the screen data area,			*/
};									/* in the screen block.				*/


static void wputscr22(struct screen_block *block);
static int wgetscr22(struct screen_block *block);
static void wgetfac22(struct screen_block *block);
static void putfacdata(int item, int xv1, int xv2, int xh, unsigned char *scrn);
static int getfacdata(int item, int xv1, int xv2, int xh, unsigned char *scrn);
static void calcdata(int item, int xv1, int xv2, int xh, 
		     fac_t **fac_ptr, unsigned char **data_ptr, 
		     int *o_row, int *o_col, int *o_pos, int *x_size);
static void load_item_table22(unsigned char *screen);
static void close_open_occurs( int this_level, struct v_struct *v1, struct v_struct *v2);

/*
85002	%%WSCREEN-F-SCRVERSION Screen version mismatch %d [Current=%d]
85004	%%WSCREEN-F-LEVELNOTFND Expecting [L] found [%5.5s...] at offset [%u]
85006	%%WSCREEN-F-BADCONTROL Invalid control characters found [%5.5s...]
85008	%%WSCREEN-F-BADPIC Expecting [P{...}] found [%9.9s...]
85010	%%WSCREEN-F-MAXITEMS Too many screen items %d MAX=%d
*/

/********************************************************************************************************************************/
void WSCREEN(
	unsigned char *screen,								/* Pointer to the screen structure.	*/
	unsigned char *function,							/* Function to execute (for vwang)	*/
	unsigned char *blockarg,							/* Screen structure block.		*/
	unsigned char *lines,								/* Number of lines to write.		*/
		 char *pfkeys,								/* Termination PF key string.		*/
		 char *on_pfkeys,							/* pfkeys used in ON PFKEY		*/
		 char *pfkey_ret,							/* Place to return key in.		*/
	unsigned char *file_stat)							/* place to return file status.		*/
{
	static char *costar_screenname_api = NULL;
	static int use_costar_flag = 0;
	static int first = 1;

	struct screen_block *block = (struct screen_block *) blockarg;
	int key_exit,s_err,i;


	/* First time thru set nativecharmap flag */
	if (-1 == nativecharmap)
	{
		nativecharmap = 0;
		if (WL_get_wisp_option("NATIVECHARMAP"))
		{
			nativecharmap = 1;
			/*
			**	If using NATIVECHARMP then ensure the charmap is loaded
			*/
			vwang_load_charmap(0);
		}
	}

	if (first)
	{
		if ((use_costar_flag = use_costar()))
		{
			char *ptr;
			
			costar_screenname_api  = (ptr = getenv("W4WSCREENNAME"))   ? wisp_strdup(ptr) : NULL;
		}
		first = 0;
	}
	

	/* Dropping support for pre-ver22 screens (before release 3.2 of WISP)	*/
	/* WIN32; support starts at version 22.	*/

	ver20 = (*screen == 20);		/* DISP-ITEM-LENGTH  COMP PIC 9999 						*/
	ver21 = (*screen == 21);		/* DISP-ITEM-LENGTH  PIC X							*/
	ver22 = (*screen == 22);		/* New Method  "L5R1C1O2P{X(20)};"						*/
	ver90 = (*screen == 90);		/* Custom 5-BYTE FACs								*/

	if ( !ver22 && !ver90 )								/* Is there a version match?		*/
	{
		WL_werrlog(WERRCODE(85002),*screen,WISP_SCREEN_VERSION,0,0,0,0,0,0);
		wexit(WERRCODE(85002));							/* Take fatal error.			*/
	}

	size_of_fac = (ver90) ? 5 : 1;

	wtrace("WSCREEN","ENTRY","Prog=[%8.8s] Screen=[%32.32s]", &screen[1], &screen[9]);

	load_item_table22(screen);

	if (use_costar_flag)
	{
		if (costar_screenname_api)
		{
			char buff[256];

			sprintf(buff, "%s%s:%s", costar_screenname_api, 
				wisp_get_progname(), wisp_get_screenname());
			costar_ctrl_api(buff);
		}
	}
	

	wputscr22(block);

	do										/* repeat until we get a valid screen	*/
	{
		if (*file_stat == 'E')							/* calling on error, clear row+col	*/
		{
			block->order[2] = 0;
			block->order[3] = 0;
		}

		vwang(function,(unsigned char*)block,lines,pfkeys,pfkey_ret,file_stat);	/* Read/write the screen.		*/
		s_err = 0;								/* hope no errors			*/
		key_exit = 0;								/* didn't find a key yet		*/
		i = 0;
		if (*on_pfkeys != 'X') do						/* need to check pfkeys			*/
		{									/* determine if the key is an exit key	*/
			if (on_pfkeys[i] == 'X') break;					/* x means stop				*/
			if ((on_pfkeys[i] == pfkey_ret[0]) && (on_pfkeys[i+1] == pfkey_ret[1]))
			{
				key_exit = 1;						/* found a key, set the flag		*/
			}
			i += 2;								/* otherwise keep looking		*/
		} while (key_exit == 0);

		if (*file_stat == ' ') *file_stat = '*';				/* Say we have been here before		*/

		if (!key_exit)								/* not exiting, do the moves		*/
		{
			s_err = wgetscr22(block);					/* Copy screen into the structure.	*/

			if (s_err)							/* flag the error			*/
			{
				*file_stat = 'E';
				file_stat[1] = ' ';
			}
		}
		else
		{									/* a key was found, tell COBOL		*/
			wgetfac22(block);						/* Tailor the FAC's			*/
			*on_pfkeys = '*';						/* set the first char to a *		*/
			for (i=1; i<64; i++) on_pfkeys[i] = ' ';			/* pad with spaces			*/
		}
	}
	while (s_err != 0);								/* and check for errors			*/

}

/*
	ver90	This is a custom mod for RSS to support there 5 byte FAC fields.
		We treat a ver90 just like a ver22 except we use these two
		routines to translate between the one byte fac in the vwang
		screen block and the 5 byte fac in the wscreen data structure.
*/

struct five_byte_fac_s
{
	char	altered_byte;		/* 0-off 1-on				*/
	char	underline_byte;		/* 0-off 1-on				*/
	char	rendition_byte;		/* 0-bright 1-dim 2-error(blink) 3-hide */
	char	protect_byte;		/* 0-off 1-on 				*/
	char	input_byte;		/* 0-all 1-uppercase 2-numeric 		*/
};

static fac_t wscreen2fac(unsigned char *ptr)
{
	fac_t fac;
	struct five_byte_fac_s *five_byte;

	if (ver90)
	{
		five_byte = (struct five_byte_fac_s *)ptr;

		fac = FAC_FAC_ON;
		if ( '1' == five_byte->altered_byte )	fac |= FAC_MODIFIED_ON;
		if ( '1' == five_byte->underline_byte )	fac |= FAC_UNDERSCORED_ON;
		if ( '1' == five_byte->rendition_byte )	fac |= FAC_RENDITION_DIM;
		if ( '2' == five_byte->rendition_byte )	fac |= FAC_RENDITION_BLINK;
		if ( '3' == five_byte->rendition_byte )	fac |= FAC_RENDITION_BLANK;
		if ( '1' == five_byte->protect_byte )	fac |= FAC_PROTECTED_ON;
		if ( '1' == five_byte->input_byte )	fac |= FAC_DATATYPE_UPPER;
		if ( '2' == five_byte->input_byte )	fac |= FAC_DATATYPE_NUMERIC;
		
	}
	else
	{
		fac = *ptr;
	}

	return fac;
}

static void fac2wscreen(unsigned char *ptr, fac_t fac)
{
	struct five_byte_fac_s *five_byte;

	if (ver90)
	{
		memset(ptr, '0', 5);
		five_byte = (struct five_byte_fac_s *)ptr;

		if ( FAC_MODIFIED(fac) ) 	five_byte->altered_byte   = '1';
		if ( FAC_UNDERSCORED(fac) ) 	five_byte->underline_byte = '1';
		if ( FAC_DIM(fac) ) 		five_byte->rendition_byte = '1';
		if ( FAC_BLINK(fac) ) 		five_byte->rendition_byte = '2';
		if ( FAC_BLANK(fac) ) 		five_byte->rendition_byte = '3';
		if ( FAC_PROTECTED(fac) ) 	five_byte->protect_byte   = '1';
		if ( FAC_UPPER(fac) ) 		five_byte->input_byte     = '1';
		if ( FAC_NUMERIC(fac) ) 	five_byte->input_byte     = '2';
	}
	else
	{
		*ptr = fac;
	}
}

/********************************************************************************************************************************/
static void wputscr22(struct	screen_block	*block)				/* Copy a screen structure into a block		*/
{
	int	item, xv1, xv2, xh;

	memset( &(block->scrn[0]),' ',1920);					/* Set the screen area to spaces		*/

	for( item=0; item<num_items; item++)					/* loop for each item in the table.		*/
	{
		for(xv1=0; xv1 < item_table[item].it_v1; xv1++)			/* Loop thru vert-1 occurs.			*/
		{
			for(xv2=0; xv2 < item_table[item].it_v2; xv2++)		/* Loop thru vert-2 occurs.			*/
			{
				for(xh=0; xh < item_table[item].it_h; xh++)	/* Loop thru horz occurs.			*/
				{
					putfacdata(item, xv1, xv2, xh, block->scrn);
				}
			}
		}
	}

}


/********************************************************************************************************************************/
static int wgetscr22(struct	screen_block	*block)				/* Copy a block into a screen structure 	*/
{
	int	item, xv1, xv2, xh, errcode, rc;

	errcode = 0;

	for( item=0; item<num_items; item++)					/* loop for each item in the table.		*/
	{
		for(xv1=0; xv1 < item_table[item].it_v1; xv1++)			/* Loop thru vert-1 occurs.			*/
		{
			for(xv2=0; xv2 < item_table[item].it_v2; xv2++)		/* Loop thru vert-2 occurs.			*/
			{
				for(xh=0; xh < item_table[item].it_h; xh++)	/* Loop thru horz occurs.			*/
				{
					rc = getfacdata(item, xv1, xv2, xh, block->scrn);
					if ( rc )
					{
						errcode = 1;
					}
				}
			}
		}
	}
	return( errcode );
}


/********************************************************************************************************************************/
static void wgetfac22(struct screen_block *block)				/* Copy the FAC's from a block into a structure.*/
{
	int	item, xv1, xv2, xh;
	int	o_row, o_col, o_pos;
	int	x_size;
	fac_t *fac_ptr;
	unsigned char *data_ptr;

	for( item=0; item<num_items; item++)					/* loop for each item in the table.		*/
	{
		for(xv1=0; xv1 < item_table[item].it_v1; xv1++)			/* Loop thru vert-1 occurs.			*/
		{
			for(xv2=0; xv2 < item_table[item].it_v2; xv2++)		/* Loop thru vert-2 occurs.			*/
			{
				for(xh=0; xh < item_table[item].it_h; xh++)	/* Loop thru horz occurs.			*/
				{
					calcdata(item, xv1, xv2, xh, &fac_ptr, &data_ptr, &o_row, &o_col, &o_pos, &x_size);

					if ( o_col != 0 )
					{
						fac2wscreen(fac_ptr, block->scrn[o_pos-1]);	/* Copy out the FAC		*/
					}
				}
			}
		}
	}
}


/********************************************************************************************************************************/
/*
	putfacdata:	Insert into a 1920 byte area the FAC and DATA plus the trailing FAC.
*/
static void putfacdata(int item, int xv1, int xv2, int xh, unsigned char *scrn)
{
	int	o_row, o_col, o_pos;
	int	x_size;
	fac_t *fac_ptr;
	unsigned char *data_ptr;

	calcdata(item, xv1, xv2, xh, &fac_ptr, &data_ptr, &o_row, &o_col, &o_pos, &x_size);

	if ( o_col != 0 )
	{
		scrn[o_pos-1] = wscreen2fac(fac_ptr);				/* Insert the FAC before the DATA		*/
	}

	memcpy(scrn+o_pos, data_ptr, x_size);					/* Copy in the DATA				*/

	if (nativecharmap)
	{
		/*
		**	Translate ansi charset into wang charset in preparation of vwang call.
		*/
		vwang_ansi2wang(scrn+o_pos, x_size);
	}

	o_pos += x_size;							/* Point to end-fac				*/
	o_col = (o_col + x_size) % WSB_COLS;					/* Point to ending col				*/

	if ( o_pos < 1920 && o_col != 0 && 
	    !FAC_FAC(vwang_fac_pre_filter(scrn[o_pos])) )			/* if !eod-of-scrn && col!=0 && not-fac then	*/
	{
		scrn[o_pos] = vwang_unfac_pre_filter(FAC_PROT_DIM);
	}
}


/********************************************************************************************************************************/
/*
	getfacdata:	Extract from a 1920 byte area the FAC and DATA.
*/
static int getfacdata(int item, int xv1, int xv2, int xh, unsigned char *scrn)
{
	int	o_row, o_col, o_pos;
	int	x_size,i;
	unsigned char	*data_ptr;
	fac_t 	*fac_ptr;
	unsigned char	mod_fac;
	int	errcode;

	mod_fac = 0;
	errcode = 0;

	calcdata(item, xv1, xv2, xh, &fac_ptr, &data_ptr, &o_row, &o_col, &o_pos, &x_size);

	if ( o_col != 0 )
	{
		mod_fac = FAC_MODIFIED(vwang_fac(scrn[o_pos-1]));               /* Was field modified ?				*/ 
		fac2wscreen(fac_ptr, scrn[o_pos-1]);				/* Copy out the FAC				*/
	}

	if ( !mod_fac )							/* If there are any embedded facs that have the	*/
	{								/* mod bit set (0xC0 = 0x80 + 0x40) then turn	*/
		for(i=0;i<x_size;i++)					/* on the mod flag and process the data.	*/
		{
			if ( (fac_t)(vwang_fac(scrn[o_pos+i]) & 0xC0) == 
			     (fac_t)(vwang_fac((fac_t)0xC0)))
			{
				mod_fac = 1;
				break;
			}
		}
	}

	if (mod_fac)
	{
		WL_cobxpic_edit((char *)data_ptr,(char *)(scrn+o_pos),
				item_table[item].it_ps.ps_xpic,
				item_table[item].it_ps.ps_size,
				item_table[item].it_ps.ps_type,
				item_table[item].it_ps.ps_dp,
				item_table[item].it_ps.ps_blankdecimal,
				item_table[item].it_ps.ps_suppchar,
				item_table[item].it_ps.ps_suppidx,
				item_table[item].it_ps.ps_floatidx,
				item_table[item].it_ps.ps_signed,
				&errcode);

		if (nativecharmap)
		{
			/*
			**	Translate wang charset into ansi charset following a vwang read.
			*/
			vwang_wang2ansi(data_ptr, x_size);
		}

		if ( errcode )
		{
			scrn[o_pos-1] |= FAC_RENDITION_BLINK;
		}

	}
	return( errcode );
}

/********************************************************************************************************************************/
/*
**	ROUTINE:	calcdata()
**
**	FUNCTION:	Calculate screen position data for one screen element
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	item		Index into item_table
**	xv1		Vert-1 index for this item [0 - (item_table[item].it_v1-1)]
**	xv2		Vert-2 index for this item [0 - (item_table[item].it_v2-1)]
**	xh		Hort   index for this item [0 - (item_table[item].it_h-1)]
**	fac_ptr		Returned pointer to fac in WSCREEN structure
**	data_ptr	Returned pointer to element data in WSCREEN structure
**	o_row		Returned row offset (0-23)
**	o_col		Returned col offset (0-79)
**	o_pos		Returned position offset into vwang screen block (0-1919)
**	x_size		Returned element screen size (may have been truncated)
**
**	GLOBALS:	
**	item_table	Sructure of screen items.
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/

static void calcdata(int item, int xv1, int xv2, int xh, 
		     fac_t **fac_ptr, unsigned char **data_ptr, 
		     int *o_row, int *o_col, int *o_pos, int *x_size)
{
	int	o_idx, elmts;

	elmts = item_table[item].it_v1 * 					/* Calc the number of elements in this item	*/
		item_table[item].it_v2 * 
		item_table[item].it_h;
	o_idx = xv1 * item_table[item].it_v2 * item_table[item].it_h +		/* Calc the index (offset) into elmts		*/
		xv2 * item_table[item].it_h +
		xh;

	*fac_ptr  = item_table[item].it_dataptr + (o_idx * size_of_fac);
	*data_ptr = item_table[item].it_dataptr + (elmts * size_of_fac) + (o_idx * item_table[item].it_size);

	*o_row = item_table[item].it_row +					/* Calc the row offset (0-23)			*/
		 xv1 * item_table[item].it_v1_off +
		 xv2 * item_table[item].it_v2_off - 1;

	*o_col = item_table[item].it_col +					/* Calc the col offset (0-79)			*/
		xh * (item_table[item].it_size + 1) - 1;

	/*
	**	Special case: 
	**	05 item PIC X(79) OCCURS n
	**	Treat horizontal repeats as vertical repeats when you cannot fit
	**	multiple elements across the screen.
	*/
	if (item_table[item].it_h > 1 &&	/* Horizontal occurs */
	    elmts == item_table[item].it_h &&	/* no vertical occurs */
	    item_table[item].it_size == 79)	/* PIC X(79) */
	{
		*o_col = item_table[item].it_col - 1;
		*o_row = item_table[item].it_row + xh - 1;
	}

	/*
	**	Ajust o_row and o_col to ensure they are valid
	*/

	if (*o_col > 79 && xh > 0 &&
	    item_table[item].it_h > 1 &&	/* Horizontal occurs */
	    elmts == item_table[item].it_h)	/* no vertical occurs */
	{
		/*
		**	Horitzonal array element is off the screen.
		**	Step it back one element at a time until on the screen, each step
		**	drops down to the next row.
		**	Once back on the screen reset the col.
		**	NOT TRYING TO BE PRETTY - VISUAL CLUE THAT SCREEN IS MISSED UP.
		*/
		while (*o_col > 79)
		{
			*o_col -= (item_table[item].it_size + 1);
			*o_row += 1;
		}
		*o_col = 1;
	}

	if (*o_row <  0) 
	{
		*o_row = 0;
	}
	if (*o_row > 23) 
	{
		*o_row = 23;
	}
	if (*o_col < 0 || *o_col > 79)
	{
		*o_col = 1;
	}

	*o_pos = *o_row * WSB_COLS + *o_col;					/* Calc the offset into block->scrn		*/

										/* Calc the actual size.			*/
	*x_size = (*o_pos + item_table[item].it_size > 1920) ? (1920-*o_pos) : item_table[item].it_size;
}

/********************************************************************************************************************************/
/* Load in an integer number starting at ptr. Return after ptr */
static unsigned char *getnumber(unsigned char *ptr, int *num)	
{
	int	i;

	i = 0;

	while( *ptr >= '0' && *ptr <= '9' )
	{
		i = (i * 10) + (*ptr - '0');
		ptr++;
	}

	*num = i;

	return( ptr );
}

/********************************************************************************************************************************/
/*
	load_item_table22:	Load the item descriptions from the screen struct into the item_table (it).
				This includes calulating vertical occurs offsets.

		screen ->	WISP-SCREEN-VERSION	PIC X
				PROGRAM-ID		PIC X(8)
				SCREEN-NAME		PIC X(32)
				STATIC-DYNAMIC		PIC X   "S" or "D"
				DECIMAL-IS		PIC X   "." or ","
				RESERVED-SPACE		PIC X(9)
				ITEM-STRUCT  OCCURS x
				    ITEM-CONTROL		PIC X(variable)  VALUE "LxRxCxOxP{x}"
				    ITEM-END			PIC X		 VALUE ";"
				    ITEM-FAC-DATA
					ITEM-FAC		PIC X(v1*v2*h)
					ITEM-DATA		PIC X(v1*v2*h*size)
				SCREEN-END		PIC X VALUE "."


*/
static void load_item_table22(unsigned char *screen)
{
	static const unsigned char *last_screen;

	struct v_struct v1, v2;

	int	this_level;					/* Current level number.					*/
	int	this_row;					/* Current row number.						*/
	int	this_col;					/* Current col number.						*/
	int	this_occurs;					/* Current occurs number.					*/
	char	this_pic[50];					/* Ptr to current pic.						*/

	int	curr_row, curr_col;				/* The current calculated row & col.				*/

	unsigned char *scrn_ptr;				/* Ptr to current position in screen.				*/
	unsigned char	*u_ptr;					/* Temp ptr.							*/


	if ( static_screen && screen == last_screen ) return;	/* Check if already loaded.			*/

	/*
	**	Initialize variables
	*/

	num_items = 0;

	v1.v_occurs = 0;
	v2.v_occurs = 0;

	curr_row = 1;
	curr_col = 1;

	scrn_ptr = screen;
	scrn_ptr++;								/* Skip over version.				*/

	wisp_set_progname((char*)scrn_ptr);	/* Load the program name.			*/
	scrn_ptr += 8;

	wisp_set_screenname((char*)scrn_ptr);	/* Load the screen name.			*/
	scrn_ptr += 32;

	/*
	** static_screen = (*scrn_ptr++ == 'S') ? 1 : 0; 
	** This failed to increment scrn_ptr on AIX 5.3 with IBM xlC compiler version 11.1.0.0
	*/
	static_screen = (*scrn_ptr == 'S') ? 1 : 0;				/* Is this screen Static or Dynamic.		*/
	scrn_ptr += 1;

	WL_set_char_decimal_point('.');
	WL_set_char_comma(',');

	if ( *scrn_ptr == ',' )							/* Decimal point is comma			*/
	{
		WL_set_char_decimal_point(',');
		WL_set_char_comma('.');
	}
	scrn_ptr += 1;

	reserved_space = scrn_ptr;						/* Point to reserved space.			*/
	scrn_ptr += 9;

	while ( *scrn_ptr != '.' )						/* Loop until SCREEN-END.			*/
	{

		if ( *scrn_ptr != 'L' )						/* LEVEL not found - fatal error		*/
		{
			WL_werrlog(WERRCODE(85004), scrn_ptr, scrn_ptr-screen,0,0,0,0,0,0);
			wexit(WERRCODE(85004));
		}

		this_level = 0;
		this_row = 0;
		this_col = 0;
		this_occurs = 0;
		this_pic[0] = '\0';

		scrn_ptr++;
		scrn_ptr = getnumber( scrn_ptr, &this_level );			/* Get this_level.				*/

		close_open_occurs( this_level, &v1, &v2 );			/* Close any open vert occurs.			*/

		while( *scrn_ptr != ';' )					/* Process ITEM-CONTROL until ITEM-END		*/
		{
			switch ( *scrn_ptr )
			{
			case 'R':						/* Load the this_row				*/
				scrn_ptr++;
				scrn_ptr = getnumber( scrn_ptr, &this_row );
				break;
			case 'C':						/* Load the this_col				*/
				scrn_ptr++;
				scrn_ptr = getnumber( scrn_ptr, &this_col );
				break;
			case 'O':						/* Load the this_occurs				*/
				scrn_ptr++;
				scrn_ptr = getnumber( scrn_ptr, &this_occurs );
				break;
			case 'P':
				scrn_ptr += 2;
				u_ptr = (unsigned char *)memchr(scrn_ptr,'}',40);
				if ( !u_ptr )
				{
					WL_werrlog(WERRCODE(85008),scrn_ptr-2,0,0,0,0,0,0,0);
					wexit(WERRCODE(85008));
				}
				memcpy(this_pic,scrn_ptr,u_ptr-scrn_ptr);
				this_pic[u_ptr-scrn_ptr] = '\0';
				scrn_ptr = u_ptr+1;
				break;
			default:
				WL_werrlog(WERRCODE(85006),scrn_ptr,0,0,0,0,0,0,0);
				wexit(WERRCODE(85006));
				break;
			}
		}

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

		scrn_ptr++;							/* Point past the ITEM-END			*/

		if ( this_pic[0] )
		{
			int	xflag, psize, pic_type, pic_dp, blankdecimal, suppidx, floatidx, psigned;
			char	xpic[80+1], suppchar;

			item_table[num_items].it_v1      = (v1.v_occurs) ? v1.v_occurs : 1;
			item_table[num_items].it_v2      = (v2.v_occurs) ? v2.v_occurs : 1;
			item_table[num_items].it_v1_off  = 1;
			item_table[num_items].it_v2_off  = 1;
			item_table[num_items].it_h       = (this_occurs) ? this_occurs : 1;
			item_table[num_items].it_dataptr = scrn_ptr;
			item_table[num_items].it_row     = (curr_row<1 || curr_row>24) ? 1 : curr_row;
			item_table[num_items].it_col     = (curr_col<1 || curr_col>80) ? 1 : curr_col;

			WL_parse_pic(this_pic, xpic, &xflag, &psize, &pic_type, &pic_dp, &blankdecimal, &suppchar, 
					&suppidx, &floatidx, &psigned);

			item_table[num_items].it_size    = psize;

			item_table[num_items].it_ps.ps_size = psize;
			item_table[num_items].it_ps.ps_type = pic_type;
			item_table[num_items].it_ps.ps_dp = pic_dp;
			item_table[num_items].it_ps.ps_blankdecimal = blankdecimal;
			item_table[num_items].it_ps.ps_signed = psigned;
			item_table[num_items].it_ps.ps_suppidx = suppidx;
			item_table[num_items].it_ps.ps_floatidx = floatidx;
			item_table[num_items].it_ps.ps_suppchar = suppchar;
			if (xflag && psize<=80) 
				strcpy(item_table[num_items].it_ps.ps_xpic,xpic);
			else	strcpy(item_table[num_items].it_ps.ps_xpic,this_pic);


			scrn_ptr += item_table[num_items].it_v1 * 
				    item_table[num_items].it_v2 * 
				    item_table[num_items].it_h  *
				    (item_table[num_items].it_size + size_of_fac);

			num_items += 1;
			if ( num_items >= MAX_ITEM_TABLE )
			{
				WL_werrlog(WERRCODE(85010),num_items+1,MAX_ITEM_TABLE,0,0,0,0,0,0);
				wexit(WERRCODE(85010));
			}
		}
	}

	this_level = 0;								/* Force closure of open occurs.		*/
	close_open_occurs( this_level, &v1, &v2 );				/* Close any open vert occurs.			*/

	last_screen = screen;							/* Save screen pointer.				*/
}

static void close_open_occurs( int this_level, struct v_struct *v1, struct v_struct *v2)
{
	int	offset;

	if ( v2->v_occurs && this_level <= v2->v_level )
	{
		offset = v2->v_high - v2->v_low + 1;					/* Calc the v2 offset.			*/
		while( v2->v_item < num_items )
		{
			item_table[v2->v_item++].it_v2_off = offset;			/* Load the offset into item table.	*/
		}
		v2->v_high = (offset * v2->v_occurs) + v2->v_low - 1;			/* Calc the true v2 high row.		*/
		v1->v_high = (v1->v_high > v2->v_high) ? v1->v_high : v2->v_high;	/* Adjust v1 high row.			*/
		v2->v_occurs = 0;							/* Finished with v2 occurs.		*/
	}

	if ( v1->v_occurs && this_level <= v1->v_level )
	{
		offset = v1->v_high - v1->v_low + 1;					/* Calc the v1 offset.			*/
		while( v1->v_item < num_items )
		{
			item_table[v1->v_item++].it_v1_off = offset;			/* Load the offset into item table.	*/
		}
		v1->v_occurs = 0;							/* Finished with v1 occurs.		*/
	}
}
/*
**	History:
**	$Log: wscreen.c,v $
**	Revision 1.42  2011/08/14 14:21:23  gsl
**	Fix *pointer++ increment bug in IMB xlC compiler
**	
**	Revision 1.41  2003/02/24 21:58:03  gsl
**	Remove support for screen versions 20 and 21 (unix only) and juster
**	
**	Revision 1.40  2003/02/04 16:02:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.39  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.38  2002/12/10 20:54:07  gsl
**	use WERRCODE()
**	
**	Revision 1.37  2002/12/09 21:09:34  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.36  2002/11/27 17:26:05  gsl
**	Special case of treating a horizontally repeating field as a vertically repeating field.
**	
**	Revision 1.35  2002/08/01 14:45:10  gsl
**	type warnings
**	
**	Revision 1.34  2002/08/01 02:41:22  gsl
**	fix type warning
**	
**	Revision 1.33  2002/07/26 18:19:15  gsl
**	wscreen -> WSCREEN
**	
**	Revision 1.32  2002/07/17 17:50:09  gsl
**	Fix unsigned char warnings
**	
**	Revision 1.31  2002/07/15 14:07:00  gsl
**	vwang globals
**	
**	Revision 1.30  2002/07/12 20:40:41  gsl
**	Global unique WL_ changes
**	
**	Revision 1.29  2002/07/12 17:01:05  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.28  2002/07/11 20:29:18  gsl
**	Fix WL_ globals
**	
**	Revision 1.27  2002/07/11 15:03:35  gsl
**	Fix WL_ globals
**	
**	Revision 1.26  2002/07/10 21:05:37  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.25  2002/07/09 04:13:52  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.24  2002/07/02 21:15:37  gsl
**	Rename wstrdup
**	
**	Revision 1.23  2002/07/02 04:00:36  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.22  2002/06/21 20:49:30  gsl
**	Rework the IS_xxx bit flags and the WFOPEN_mode flags
**	
**	Revision 1.21  2001/09/25 15:01:59  gsl
**	Remove unnneded ifdefs
**	
**	Revision 1.20  1999-09-13 15:56:27-04  gsl
**	Fix return codes from load_item_table22()
**
**	Revision 1.19  1998-03-06 11:42:27-05  gsl
**	Add the W4WSCREENNAME api hook for Costar.
**
**	Revision 1.18  1998-01-18 15:03:33-05  gsl
**	Fix prototype errors
**
**	Revision 1.17  1997-12-18 21:00:53-05  gsl
**	FIx CHARMAP processing
**
**	Revision 1.16  1997-12-18 09:13:26-05  gsl
**	fix getnumber() prototype
**
**	Revision 1.15  1997-12-17 20:44:09-05  gsl
**	Fix test for NATIVECHARMAP option
**
**	Revision 1.14  1997-12-17 19:58:23-05  gsl
**	Add support for NATIVECHARMAP option
**	Fix prototypes
**
**	Revision 1.13  1997-05-08 16:37:43-04  gsl
**	Changed to use wtrace()
**
**	Revision 1.12  1996-08-19 18:33:22-04  gsl
**	drcs update
**
**
**
*/
