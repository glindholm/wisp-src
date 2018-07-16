			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*						Local data definitions								*/

#include <ctype.h>									/* Include character typing functions.	*/
#include <v/video.h>									/* Include the VIDEO definitions.	*/
#ifdef unix
#include <memory.h>
#endif
#include <string.h>
#include "idsistd.h"
#include "wcommon.h"
#include "movebin.h"
#include "cobrun.h"
#include "werrlog.h"


#define ERROR_BIT 16
#define MOD_BIT   64

extern char wisp_progname[9];								/* The name of the current program.	*/
extern char wisp_screen[33];								/* The name of the current screen.	*/

static int  ver20;
static int  ver21;
static int  ver22;

static int	static_screen;
static unsigned char *reserved_space;

static int wputscr22();
static int wgetscr22();
static int wgetfac22();
static int calcdata();
static int wputscr();
static int wgetscr();
static int wgetfac();
static int load_item_table22();
static int putfacdata();
static int getfacdata();
static int loadbytes_long();
static int close_open_occurs();

char	char_decimal_point = '.';							/* The decimal_point character		*/
char	char_comma = ',';								/* The comma character			*/

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

#define	ROUTINE		85000

/*
85001	%%WSCREEN-I-ENTRY Entry into WSCREEN
85002	%%WSCREEN-F-SCRVERSION Screen version mismatch %d [Current=%d]
85004	%%WSCREEN-F-LEVELNOTFND Expecting [L] found [%5.5s...]
85006	%%WSCREEN-F-BADCONTROL Invalid control characters found [%5.5s...]
85008	%%WSCREEN-F-BADPIC Expecting [P{...}] found [%9.9s...]
85010	%%WSCREEN-F-MAXITEMS Too many screen items %d MAX=%d
*/

/********************************************************************************************************************************/
wscreen(screen,function,block,lines,pfkeys,on_pfkeys,pfkey_ret,file_stat)

	unsigned char *screen;								/* Pointer to the screen structure.	*/
	unsigned char *function;							/* Function to execute (for vwang)	*/
	struct	screen_block	*block;							/* Screen structure block.		*/
	unsigned char *lines;								/* Number of lines to write.		*/
	unsigned char *pfkeys;								/* Termination PF key string.		*/
	unsigned char *on_pfkeys;							/* pfkeys used in ON PFKEY		*/
	unsigned char *pfkey_ret;							/* Place to return key in.		*/
	unsigned char *file_stat;							/* place to return file status.		*/

{
	int key_exit,s_err,i;

#ifdef	MSDOS										/* Debug hook for MS-DOS.		*/
	v_modeflag( 3, 15, 'S' );							/* Cyan box, white 'S'.			*/
#endif
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

											/* We will continue to support screen	*/
											/* version 20 & 21.			*/
											/* Except on MSDOS; support starts at	*/
											/* version 22.				*/
	ver20 = (*screen == 20);		/* DISP-ITEM-LENGTH  COMP PIC 9999 						*/
	ver21 = (*screen == 21);		/* DISP-ITEM-LENGTH  PIC X							*/
	ver22 = (*screen == 22);		/* New Method  "L5R1C1O2P{X(20)};"						*/


#ifdef MSDOS
	if ( !ver22 )									/* Is there a version match?		*/
#else
	if ( !ver20 && !ver21 && !ver22 )						/* Is there a version match?		*/
#endif
	{
		werrlog(ERRORCODE(2),*screen,SCREEN_VERSION,0,0,0,0,0,0);
		wexit(ERRORCODE(2));							/* Take fatal error.			*/
	}

	if ( ver22 )
	{
		load_item_table22(screen);
	}

	if ( ver22 )
	{
		wputscr22(screen,block);
	}
#ifndef	MSDOS		/* if NOT MSDOS	*/						/* MSDOS only supports ver22.		*/
	else
	{
		wputscr(screen,block);							/* Move structure into the block.	*/
	}
#endif

	do										/* repeat until we get a valid screen	*/
	{
		if (*file_stat == 'E')							/* calling on error, clear row+col	*/
		{
			block->order[2] = 0;
			block->order[3] = 0;
		}

		vwang(function,block,lines,pfkeys,pfkey_ret,file_stat);			/* Read/write the screen.		*/
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
			if ( ver22 )
			{
				s_err = wgetscr22(block,screen);			/* Copy screen into the structure.	*/
			}
#ifndef MSDOS			/* if NOT MSDOS	*/					/* MSDOS only supports ver22.		*/
			else
			{
				s_err = wgetscr(block,screen);				/* Copy screen into the structure.	*/
			}
#endif

			if (s_err)							/* flag the error			*/
			{
				*file_stat = 'E';
				file_stat[1] = ' ';
			}
		}
		else
		{									/* a key was found, tell COBOL		*/
			if ( ver22 )
			{
				wgetfac22(block,screen);				/* Tailor the FAC's			*/
			}
#ifndef MSDOS			/* if NOT MSDOS	*/					/* MSDOS only supports ver22.		*/
			else
			{
				wgetfac(block,screen);					/* Tailor the FAC's			*/
			}
#endif
			*on_pfkeys = '*';						/* set the first char to a *		*/
			for (i=1; i<64; i++) on_pfkeys[i] = ' ';			/* pad with spaces			*/
		}
	}
	while (s_err != 0);								/* and check for errors			*/

#ifdef	MSDOS										/* Debug hook for MS-DOS.		*/
	v_modeflag( 6, 11, 'C' );							/* Gold box, cyan 'C'.			*/
#endif
}


/********************************************************************************************************************************/
static wputscr22(screen,block)							/* Copy a screen structure into a block		*/
unsigned char *screen;								/* a pointer to the screen structure		*/
struct	screen_block	*block;							/* Screen structure block.			*/
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
static int wgetscr22(block,screen)						/* Copy a block into a screen structure 	*/
struct	screen_block	*block;							/* Screen structure block.			*/
unsigned char *screen;								/* a pointer to the screen structure		*/
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
static wgetfac22(block,screen)							/* Copy the FAC's from a block into a structure.*/
struct	screen_block	*block;							/* Screen structure block.			*/
unsigned char *screen;								/* a pointer to the screen structure		*/
{
	int	item, xv1, xv2, xh;
	int	o_row, o_col, o_pos;
	int	x_size;
	unsigned char *fac_ptr, *data_ptr;

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
						*fac_ptr = block->scrn[o_pos-1]; /* Copy out the FAC				*/
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
static putfacdata(item, xv1, xv2, xh, scrn)
int item, xv1, xv2, xh;
unsigned char *scrn;
{
	int	o_row, o_col, o_pos;
	int	x_size;
	unsigned char *fac_ptr, *data_ptr;

	calcdata(item, xv1, xv2, xh, &fac_ptr, &data_ptr, &o_row, &o_col, &o_pos, &x_size);

	if ( o_col != 0 )
	{
		scrn[o_pos-1] = *fac_ptr;					/* Insert the FAC before the DATA		*/
	}

	memcpy(scrn+o_pos, data_ptr, x_size);					/* Copy in the DATA				*/

	o_pos += x_size;							/* Point to end-fac				*/
	o_col = (o_col + x_size) % 80;						/* Point to ending col				*/

	if ( o_pos < 1920 && o_col != 0 && 
	    !(fac_pre_vwang(scrn[o_pos]) & '\200') )				/* if !eod-of-scrn && col!=0 && not-fac then	*/
	{
		scrn[o_pos] = unfac_pre_vwang((unsigned char)'\214');
	}
}


/********************************************************************************************************************************/
/*
	getfacdata:	Extract from a 1920 byte area the FAC and DATA.
*/
static int getfacdata(item, xv1, xv2, xh, scrn)
int item, xv1, xv2, xh;
unsigned char *scrn;
{
	int	o_row, o_col, o_pos;
	int	x_size,i;
	unsigned char 	*fac_ptr, *data_ptr;
	unsigned char	mod_fac, a_byte, b_byte;
	int	errcode;
	struct mask_struct mask;

	mod_fac = 0;
	errcode = 0;

	calcdata(item, xv1, xv2, xh, &fac_ptr, &data_ptr, &o_row, &o_col, &o_pos, &x_size);

	if ( o_col != 0 )
	{
		mod_fac = fac(scrn[o_pos-1]) & MOD_BIT;	                        /* Was field modified ?				*/ 
		*fac_ptr = scrn[o_pos-1];					/* Copy out the FAC				*/
	}

	if ( !mod_fac )							/* If there are any embedded facs that have the	*/
	{								/* mod bit set (0xC0 = 0x80 + 0x40) then turn	*/
		for(i=0;i<x_size;i++)					/* on the mod flag and process the data.	*/
		{
			if ( (unsigned char)(fac(scrn[o_pos+i]) & 0xC0) == 
			     (unsigned char)(fac((unsigned char)0xC0)))
			{
				mod_fac = 1;
				break;
			}
		}
	}

	if (mod_fac)
	{

		cobxpic_edit(data_ptr,scrn+o_pos,
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

		if ( errcode )
		{
			scrn[o_pos-1] |= ERROR_BIT;
		}

	}
	return( errcode );
}

/********************************************************************************************************************************/
static calcdata(item, xv1, xv2, xh, fac_ptr, data_ptr, o_row, o_col, o_pos, x_size)
int	item, xv1, xv2, xh;
unsigned char **fac_ptr, **data_ptr;
int	*o_row, *o_col, *o_pos, *x_size;
{
	int	o_idx, elmts;

	elmts = item_table[item].it_v1 * 					/* Calc the number of elements in this item	*/
		item_table[item].it_v2 * 
		item_table[item].it_h;
	o_idx = xv1 * item_table[item].it_v2 * item_table[item].it_h +		/* Calc the index (offset) into elmts		*/
		xv2 * item_table[item].it_h +
		xh;

	*fac_ptr  = item_table[item].it_dataptr + o_idx;
	*data_ptr = item_table[item].it_dataptr + elmts + (o_idx * item_table[item].it_size);

	*o_row = item_table[item].it_row +					/* Calc the row offset (0-23)			*/
		 xv1 * item_table[item].it_v1_off +
		 xv2 * item_table[item].it_v2_off - 1;
	*o_row = (*o_row < 0 || *o_row > 23) ? 0 : *o_row;

	*o_col = item_table[item].it_col +					/* Calc the col offset (0-79)			*/
		xh * (item_table[item].it_size + 1) - 1;
	*o_col = (*o_col < 0 || *o_col > 79) ? 1 : *o_col;

	*o_pos = *o_row * 80 + *o_col;						/* Calc the offset into block->scrn		*/

										/* Calc the actual size.			*/
	*x_size = (*o_pos + item_table[item].it_size > 1920) ? (1920-*o_pos) : item_table[item].it_size;
}

/********************************************************************************************************************************/
#ifndef MSDOS			/* if NOT MSDOS	*/					/* MSDOS only supports ver22.		*/
/********************************************************************************************************************************/

static wputscr(screen,block)							/* Copy a screen structure into a block		*/
unsigned char *screen;								/* a pointer to the screen structure		*/
struct	screen_block	*block;							/* Screen structure block.			*/
{
	int num_chars,dp_loc,row,col,cpos;
	int cur_v1,cur_v2,cur_h,cur_voff,nxt_col;
	short	temp_short;
	struct
	{
		uint4 cmask;						/* Comma mask.					*/
		uint4 zmask;						/* Z mask.					*/
	} mask;
	register int i,j,i1,i2,i3;
	unsigned char *inf_ptr;							/* pointer to informational stuff		*/
	unsigned char *fac_ptr;							/* pointer to FAC stuff				*/

	memset( &(block->scrn[0]),' ',1920);					/* Set the screen area to spaces		*/
	screen++;								/* Move past the version match byte.		*/
	memcpy(wisp_progname,screen,8);						/* Save the program name.			*/
	wisp_progname[8] = '\0';

	for (i=7; i; i--)
	{
		if (wisp_progname[i] == ' ') wisp_progname[i] = '\0';		/* Scan backwards over spaces.			*/
		else break;
	}

	screen += 8;

	memcpy(wisp_screen,screen,32);
	wisp_screen[32] = '\0';

	for (i=31; i; i--)
      	{
		if (wisp_screen[i] == ' ') wisp_screen[i] = '\0';		/* Scan backwards over spaces.			*/
		else break;
	}

	screen += 32;								/* Save the screen name.			*/

	for (;;)								/* repeat this part				*/
	{
		if (*screen == 255) break;
#ifdef OLD
		if ( lpi_cobol && ((int4)screen & 1)) screen++;			/* Be sure it is an even address		*/
										/* For LPI COBOL on UNIX ugh!			*/
#endif
		cur_v1 = ( *screen++);						/* get the occurs for this item			*/

		cur_v1 = cur_v1 ? cur_v1 : 1;					/* at least 1					*/
		cur_v2 = (*screen++);						/* get second vertical occurs			*/
		cur_v2 = cur_v2 ? cur_v2 : 1;					/* at least 1					*/
		cur_voff = (*screen++) - 1;					/* get vertical offset amount			*/
		cur_h	= (*screen++);						/* get horizontal offset amt			*/
		cur_h = cur_h ? cur_h : 1;					/* at least 1					*/
		row = (*screen++) - 1;						/* get row					*/
		if (row < 0 || row > 23) row=0;					/* Fix a bogus row				*/
		nxt_col = (*screen++) - 1;					/* get column					*/

		if (ver20)
		{
			GETBIN(&temp_short,screen,2);				/* number of bytes in it			*/
			if (acu_cobol && !bytenormal()) reversebytes(&temp_short,2);
			num_chars = temp_short;
			screen += 2;						/* Skip 2 bytes.				*/
		}
		else
		{
			num_chars = (short) *screen++;				/* DISP-ITEM-LENGTH				*/
		}

		dp_loc = (*screen++);						/* location of the decimal point		*/
		loadbytes_long(&mask.cmask,screen);				/* Load the Cmask bytes				*/
		screen += 4;							/* add the size of it.				*/

		if (mask.cmask & 1)						/* If lo bit set, has a Z mask too.		*/
		{
			loadbytes_long(&mask.zmask,screen);			/* Load the Zmask bytes				*/
			screen += 4;						/* add the size of it.				*/
		}
		else
		{
			mask.zmask = 0;						/* No z mask.					*/
		}

		fac_ptr = screen;						/* The FAC is after the info			*/
		screen = screen + (cur_v1 * cur_v2 * cur_h);			/* the screen data is after the info and fac	*/

		i1 = cur_v1;
		do								/* do this for each first level OCCURS		*/
		{
			i2 = cur_v2;
			do							/* repeat this for each second level OCCURS	*/
			{
				i3 = cur_h;
				col = nxt_col;						/* reset collumn counter		*/
				do							/* repeat for each horizontal occurence	*/
				{
					cpos = row * 80 + col;				/* calculate the char pos in the array	*/

					if (col)					/* OK to output a FAC			*/
					{
						cpos--;					/* position of FAC			*/
						block->scrn[cpos++] = *fac_ptr++;	/* copy the FAC for this item		*/
					}
					else
					{
						fac_ptr++;			/* skip the FAC					*/
					}

					for (i=0; i<num_chars; i++)
					{					
						block->scrn[cpos++] = *screen++;		/* Copy the character.		*/
					}

					col += num_chars + 1;			/* update the collumn offset count		*/
										/* If not a Fac already, and not col 0, in map.	*/
					if ( !(fac_pre_vwang(block->scrn[cpos]) & '\200') && col<81 && (cpos < 1920))
					{
						block->scrn[cpos] = unfac_pre_vwang((unsigned char)'\214');
						                                /* put an EFFAC there		*/
					}


				} while (--i3);					/* loop for each horizontal OCCURS		*/
			row++;							/* add 1 to row for vertical OCCURS		*/
			} while (--i2);						/* loop for each second vertical OCCURS		*/
		row += cur_voff;						/* Add current offset for groups.		*/
		} while (--i1);							/* loop for each first vertical OCCURS		*/
	} 									/* keep going while there are items		*/
}


/********************************************************************************************************************************/
static int wgetscr(block,screen)						/* Copy a block into a screen structure 	*/
struct	screen_block	*block;							/* Screen structure block.			*/
unsigned char *screen;								/* a pointer to the screen structure		*/
{
	int num_chars,dp_loc,row,col,cpos,errstat;
	int cur_v1,cur_v2,cur_h,cur_voff,nxt_col;
	short	temp_short;
	struct
	{
		uint4 cmask;							/* Comma mask.					*/
		uint4 zmask;							/* Z mask.					*/
	} mask;

	register int i,j,i1,i2,i3;
	int jret;
	unsigned char *inf_ptr;							/* pointer to informational stuff		*/
	unsigned char *fac_ptr;							/* pointer to FAC stuff				*/
	unsigned char mod_fac,a_byte,b_byte;

	errstat = 0;

	screen += 41;								/* Move past the version match byte.		*/
										/* And the screen name info.			*/
	for (;;)								/* repeat this part				*/
	{
		if (*screen == 255) break;
#ifdef OLD
		if (lpi_cobol && ((int4)screen & 1)) screen++;			/* Need even address on LPI/AT&T/UNIX		*/
#endif
		cur_v1 = ( *screen++);						/* get the occurs for this item			*/

		cur_v1 = cur_v1 ? cur_v1 : 1;					/* at least 1					*/
		cur_v2 = (*screen++);						/* get second vertical occurs			*/
		cur_v2 = cur_v2 ? cur_v2 : 1;					/* at least 1					*/
		cur_voff = (*screen++) - 1;					/* get vertical offset amount			*/
		cur_h	= (*screen++);						/* get horizontal offset amt			*/
		cur_h = cur_h ? cur_h : 1;					/* at least 1					*/
		row = (*screen++) - 1;						/* get row					*/
		if (row < 0 || row > 23) row=0;					/* Fix a bogus row				*/
		nxt_col = (*screen++) - 1;					/* get column					*/

		if (ver20)
		{
			GETBIN(&temp_short,screen,2);				/* number of bytes in it			*/
			if (acu_cobol && !bytenormal()) reversebytes(&temp_short,2);
			num_chars = temp_short;
			screen += 2;						/* Skip 2 bytes					*/
		}
		else
		{
			num_chars = (short) *screen++;				/* DISP-ITEM-LENGTH				*/
		}

		dp_loc = (*screen++);						/* location of the decimal point		*/
		loadbytes_long(&mask.cmask,screen);				/* Load the Cmask bytes				*/
		screen += 4;							/* add the size of it.				*/

		if (mask.cmask & 1)						/* If lo bit set, has a Z mask too.		*/
		{
			loadbytes_long(&mask.zmask,screen);			/* Load the Zmask bytes				*/
			screen += 4;						/* add the size of it.				*/
		}
		else
		{
			mask.zmask = 0;						/* No z mask.					*/
		}

		fac_ptr = screen;						/* The FAC is after the info			*/
		screen = screen + (cur_v1 * cur_v2 * cur_h);			/* the screen data is after the info and fac	*/

		i1 = cur_v1;
		do								/* do this for each first level OCCURS		*/
		{
			i2 = cur_v2;
			do							/* repeat this for each second level OCCURS	*/
			{
				i3 = cur_h;
				col = nxt_col;						/* reset collumn counter		*/
				do							/* repeat for each horizontal occurence	*/
				{
					cpos = row * 80 + col;				/* calculate the char pos in the array	*/

					if (col)					/* There is a FAC			*/
					{
						cpos--;					/* position of FAC			*/
											/* see if the field was modified	*/
						mod_fac  = fac(block->scrn[cpos]) & MOD_BIT;	

						*fac_ptr++ = block->scrn[cpos++];	/* copy the FAC for this item		*/
					}
					else
					{
						fac_ptr++;				/* skip the FAC				*/
						mod_fac = 0;				/* no fac, not modified			*/
					}

					if (dp_loc)					/* it is a numeric field		*/
					{						/* copy the num back with justification	*/
						if (mod_fac)				/* if modified...			*/
						{
							a_byte = num_chars;
							b_byte = dp_loc-1;		/* let Juster examine the field		*/

							juster(&block->scrn[cpos], screen, &a_byte, &b_byte,&mask,&jret);

							if (jret != 0)
							{				/* error in conversion			*/
								block->scrn[cpos-1] |= ERROR_BIT;	/* set the error bit	*/
								errstat = -1;			/* return there is an error	*/
							}
						}
					}
					else							/* It is a literal		*/
					{
						memcpy(screen,&block->scrn[cpos],num_chars);	/* copy the text back		*/
					}
					screen += num_chars;			/* update the screen block pointer		*/
					col += num_chars + 1;			/* update the collumn offset count		*/
				} while (--i3);					/* loop for each horizontal OCCURS		*/
			row++;							/* add 1 to row for vertical OCCURS		*/
			} while (--i2);						/* loop for each second vertical OCCURS		*/
		row += cur_voff;						/* add row offset value				*/
		} while (--i1);							/* loop for each first vertical OCCURS		*/
	} 									/* keep going while there are items		*/
	return(errstat);
}


/********************************************************************************************************************************/
static wgetfac(block,screen)							/* Copy the FAC's from a block into a structure.*/
struct	screen_block	*block;							/* Screen structure block.			*/
unsigned char *screen;								/* a pointer to the screen structure		*/
{
	int num_chars,dp_loc,row,col,cpos,errstat;
	int cur_v1,cur_v2,cur_h,cur_voff,nxt_col;
	short	temp_short;
	struct
	{
		uint4 cmask;						/* Comma mask.					*/
		uint4 zmask;						/* Z mask.					*/
	} mask;

	register int i,j,i1,i2,i3;
	unsigned char *fac_ptr;							/* pointer to FAC stuff				*/

	errstat = 0;

	screen += 41;								/* Move past the version match byte.		*/
										/* And the screen name info.			*/
	for (;;)								/* repeat this part				*/
	{
		if (*screen == 255) break;
#ifdef OLD
		if (lpi_cobol && ((int4)screen & 1)) screen++;			/* Need even address on LPI/AT&T/UNIX		*/
#endif
		cur_v1 = ( *screen++);						/* get the occurs for this item			*/

		cur_v1 = cur_v1 ? cur_v1 : 1;					/* at least 1					*/
		cur_v2 = (*screen++);						/* get second vertical occurs			*/
		cur_v2 = cur_v2 ? cur_v2 : 1;					/* at least 1					*/
		cur_voff = (*screen++) - 1;					/* get vertical offset amount			*/
		cur_h	= (*screen++);						/* get horizontal offset amt			*/
		cur_h = cur_h ? cur_h : 1;					/* at least 1					*/
		row = (*screen++) - 1;						/* get row					*/
		if (row < 0 || row > 23) row=0;					/* Fix a bogus row				*/
		nxt_col = (*screen++) - 1;					/* get column					*/

		if (ver20)
		{
			GETBIN(&temp_short,screen,2);				/* number of bytes in it			*/
			if (acu_cobol && !bytenormal()) reversebytes(&temp_short,2);
			num_chars = temp_short;
			screen += 2;						/* Skip 2 bytes					*/
		}
		else
		{
			num_chars = (short) *screen++;				/* DISP-ITEM-LENGTH				*/
		}

		dp_loc = (*screen++);						/* location of the decimal point		*/

		loadbytes_long(&mask.cmask,screen);				/* Load the Cmask bytes				*/
		screen += 4;							/* add the size of it.				*/

		if (mask.cmask & 1)						/* If lo bit set, has a Z mask too.		*/
		{
			loadbytes_long(&mask.zmask,screen);			/* Load the Zmask bytes				*/
			screen += 4;						/* add the size of it.				*/
		}
		else
		{
			mask.zmask = 0;						/* No z mask.					*/
		}

		fac_ptr = screen;						/* The FAC is after the info			*/
		screen = screen + (cur_v1 * cur_v2 * cur_h);			/* the screen data is after the info and fac	*/

		i1 = cur_v1;
		do								/* do this for each first level OCCURS		*/
		{
			i2 = cur_v2;
			do							/* repeat this for each second level OCCURS	*/
			{
				i3 = cur_h;
				col = nxt_col;						/* reset collumn counter		*/
				do							/* repeat for each horizontal occurence	*/
				{
					cpos = row * 80 + col;				/* calculate the char pos in the array	*/

					if (col)					/* There is a FAC			*/
					{
						cpos--;					/* position of FAC			*/
						*fac_ptr++ = block->scrn[cpos++];	/* copy the FAC for this item		*/
					}
					else
					{
						fac_ptr++;				/* skip the FAC				*/
					}

					screen += num_chars;			/* update the screen block pointer		*/
					col += num_chars + 1;			/* update the collumn offset count		*/
				} while (--i3);					/* loop for each horizontal OCCURS		*/
			row++;							/* add 1 to row for vertical OCCURS		*/
			} while (--i2);						/* loop for each second vertical OCCURS		*/
		row += cur_voff;						/* add row offset value				*/
		} while (--i1);							/* loop for each first vertical OCCURS		*/
	} 									/* keep going while there are items		*/
	return(errstat);
}

/********************************************************************************************************************************/
static loadbytes_long( dst, src )
char *dst, *src;
{
											/* WISP generates the codes swaped so	*/
											/* if on a byte-swapped machine then	*/
											/* just load them. Else we must un-swap */
											/* them.				*/
	if (!bytenormal())
	{
		dst[0] = src[0];
		dst[1] = src[1];
		dst[2] = src[2];
		dst[3] = src[3];
	}
	else
	{
		dst[0] = src[3];
		dst[1] = src[2];
		dst[2] = src[1];
		dst[3] = src[0];
	}
}


/********************************************************************************************************************************/
#endif		/* Above section is only for NOT MSDOS systems.	*/
/********************************************************************************************************************************/
static unsigned char *getnumber(ptr,num)			/* Load in an integer number starting at ptr. Return after ptr */
unsigned char *ptr;
int  *num;
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
static load_item_table22(screen)
unsigned char *screen;
{
	static unsigned char *last_screen;

	struct v_struct v1, v2;

	int	this_level;					/* Current level number.					*/
	int	this_row;					/* Current row number.						*/
	int	this_col;					/* Current col number.						*/
	int	this_occurs;					/* Current occurs number.					*/
	char	this_pic[50];					/* Ptr to current pic.						*/

	int	curr_row, curr_col;				/* The current calculated row & col.				*/

	unsigned char	*scrn_ptr;				/* Ptr to current position in screen.				*/
	unsigned char	*u_ptr;					/* Temp ptr.							*/
	char	*ptr;

	struct mask_struct mask;

	if ( static_screen && screen == last_screen ) return(0);		/* Check if already loaded.			*/

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

	memcpy(wisp_progname,scrn_ptr,8);					/* Load the program name.			*/
	wisp_progname[8] = '\0';
	scrn_ptr += 8;
	ptr = strchr(wisp_progname,' ');
	if ( ptr ) *ptr = '\0';

	memcpy(wisp_screen,scrn_ptr,32);					/* Load the screen name.			*/
	wisp_screen[32] = '\0';
	scrn_ptr += 32;
	ptr = strchr(wisp_screen,' ');
	if ( ptr ) *ptr = '\0';

	static_screen = (*scrn_ptr++ == 'S') ? 1 : 0;				/* Is this screen Static or Dynamic.		*/

	char_decimal_point = '.';
	char_comma = ',';

	if ( *scrn_ptr == ',' )							/* Decimal point is comma			*/
	{
		char_decimal_point = ',';
		char_comma = '.';
	}
	scrn_ptr += 1;

	reserved_space = scrn_ptr;						/* Point to reserved space.			*/
	scrn_ptr += 9;

	while ( *scrn_ptr != '.' )						/* Loop until SCREEN-END.			*/
	{

		if ( *scrn_ptr != 'L' )						/* LEVEL not found - fatal error		*/
		{
			werrlog(ERRORCODE(4),scrn_ptr,0,0,0,0,0,0,0);
			wexit(ERRORCODE(4));
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
					werrlog(ERRORCODE(8),scrn_ptr-2,0,0,0,0,0,0,0);
					wexit(ERRORCODE(8));
				}
				memcpy(this_pic,scrn_ptr,u_ptr-scrn_ptr);
				this_pic[u_ptr-scrn_ptr] = '\0';
				scrn_ptr = u_ptr+1;
				break;
			default:
				werrlog(ERRORCODE(6),scrn_ptr,0,0,0,0,0,0,0);
				wexit(ERRORCODE(6));
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

			parse_pic(this_pic, xpic, &xflag, &psize, &pic_type, &pic_dp, &blankdecimal, &suppchar, 
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
				    (item_table[num_items].it_size + 1);

			num_items += 1;
			if ( num_items >= MAX_ITEM_TABLE )
			{
				werrlog(ERRORCODE(10),num_items+1,MAX_ITEM_TABLE,0,0,0,0,0,0);
				wexit(ERRORCODE(10));
			}
		}
	}

	this_level = 0;								/* Force closure of open occurs.		*/
	close_open_occurs( this_level, &v1, &v2 );				/* Close any open vert occurs.			*/

	last_screen = screen;							/* Save screen pointer.				*/
}

static close_open_occurs( this_level, v1, v2 )
int	this_level;
struct v_struct *v1, *v2;
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
