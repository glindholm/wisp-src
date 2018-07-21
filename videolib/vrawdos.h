/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/


/*
	VRAWDOS.H - Definitions for video applications on MS-DOS.
*/

#ifdef	MSDOS

#ifndef VRAWDOS_INCLUDED
#define VRAWDOS_INCLUDED

#ifdef	EXT_VRAWDOS
#define EXTERN_DEF
#define INIT_VAL(val)	=val
#else
#define EXTERN_DEF	extern
#define INIT_VAL(val)
#endif

#define INIT_ZERO	INIT_VAL(0)
#define INIT_ONE	INIT_VAL(1)

extern void v_modeflag(int bg, int fg, int ch);

/*
	MSDOS DIRECT SCREEN MEMORY I/O:

	Each character takes 2 bytes, one byte for attribute (0xFF00) and
	one for the ascii character (0x00FF).

	Color Character Attributes
	==========================
	0.......	Normal character
	1.......	Blinking character
	.xxx....	Background color 0-7
	....xxxx 	Foreground color 0-F

	Note:	It is possible to change the blink bit into a brite bit for
		background but requires special magic.

	Screen Attribute Components.
	Select a component from each group and "OR" together
	to create a video character attribute.
*/
				/* Group 1; Blink Characteristic	*/
#define ATTR_STEADY	0x0000
#define ATTR_BLINK	0x8000
				/* Group 2; Background Color		*/
#define BACK_BLACK	0x0000	/* BLACK                       		*/
#define BACK_BLUE	0x1000	/* BLUE                      		*/
#define BACK_GREEN	0x2000	/* GREEN                    		*/
#define BACK_CYAN	0x3000	/* CYAN                      		*/
#define BACK_RED	0x4000	/* RED                        		*/
#define BACK_MAGENTA	0x5000	/* MAGENTA                          	*/
#define BACK_BROWN	0x6000	/* BROWN                          	*/
#define BACK_WHITE	0x7000	/* WHITE	                      	*/
				/* Group 3; Forground Intensity 	*/
#define FORE_DIM	0x0000
#define FORE_BRIGHT	0x0800	/*	Video calls this "BOLD"		*/
				/* Group 4; Forground Color		*/
#define FORE_BLACK	0x0000  /* BLACK	GRAY			*/
#define FORE_BLUE	0x0100	/* BLUE         LIGHTBLUE		*/
#define FORE_GREEN	0x0200	/* GREEN        LIGHTGREEN		*/
#define FORE_CYAN	0x0300	/* CYAN         LIGHTCYAN		*/
#define FORE_RED	0x0400	/* RED          LIGHTRED		*/
#define FORE_MAGENTA	0x0500	/* MAGENTA      LIGHTMAGENTA		*/
#define FORE_BROWN	0x0600	/* BROWN        YELLOW                  */
#define FORE_WHITE	0x0700	/* WHITE	BRIGHTWHITE		*/

#define FORE_GRAY	  0x0800 /* These are the "bright" fore colors	*/
#define FORE_LIGHTBLUE	  0x0900
#define FORE_LIGHTGREEN	  0x0A00
#define FORE_LIGHTCYAN	  0x0B00
#define FORE_LIGHTRED	  0x0C00
#define FORE_LIGHTMAGENTA 0x0D00
#define FORE_YELLOW	  0x0E00
#define FORE_BRITEWHITE	  0x0F00


		/* IDSI Default Attributes:	*/
#define ATTRIBUTE_NORMAL		(BACK_BLUE	| FORE_WHITE)
#define ATTRIBUTE_UNDERSCORE		(BACK_BLACK	| FORE_WHITE)
#define ATTRIBUTE_REVERSE		(BACK_WHITE	| FORE_BLUE)
#define ATTRIBUTE_REVERSE_UNDERSCORE	(BACK_CYAN	| FORE_BLUE)

#define AFBR	FORE_BRIGHT
#define AFBL	ATTR_BLINK

#define ANOR	ATTRIBUTE_NORMAL
#define AUND	ATTRIBUTE_UNDERSCORE
#define AREV	ATTRIBUTE_REVERSE
#define ARUS	ATTRIBUTE_REVERSE_UNDERSCORE

#ifdef EXT_VRAWDOS
/*
	Video Attributes and Bit Values:

		Bold		0x01	...x
		Underscore	0x02	..x.
		Blink		0x04	.x..
		Reverse 	0x08	x...
*/
EXTERN_DEF int attributes[16] =
{
	( ANOR ),	 	/* 0	Normal 					*/
	( ANOR | AFBR ),	/* 1	Bold					*/
	( AUND ),	 	/* 2	Underscore				*/
	( AUND | AFBR ),	/* 3	Bold + Underscore			*/
	( ANOR | AFBL ), 	/* 4	Blink					*/
	( ANOR | AFBR | AFBL ),	/* 5	Blink + Bold				*/
	( AUND | AFBL ), 	/* 6	Blink + Underscore			*/
	( AUND | AFBR | AFBL ), /* 7	Blink + Underscore + Bold		*/
	( AREV ),	 	/* 8	Reverse					*/
	( AREV | AFBR ),	/* 9	Reverse + Bold	 			*/
	( ARUS ),	 	/* A	Reverse + Underscore			*/
	( ARUS | AFBR ),	/* B	Reverse + Underscore + Bold		*/
	( AREV | AFBL ), 	/* C	Reverse + Blink				*/
	( AREV | AFBR | AFBL ), /* D	Reverse + Blink + Bold	 		*/
	( ARUS | AFBL ),	/* E	Reverse + Blink + Underscore		*/
	( ARUS | AFBR | AFBL )  /* F	Reverse + Blink + Underscore + Bold	*/
} ;
#else
EXTERN_DEF long attributes[16];
#endif

/*
	The following items are used for direct screen memory access.
*/

#ifdef _MSC_VER
/*
**	Microsoft C 600, 700
*/
#define BASE_VIDEO_SEG	0xB800					/* Segment address for VGA or EGA mode 3 or 7	*/
#define BASE_VIDEO_ADDR	((unsigned short *)(segvar:>vp))	/* Screen Memory Segment Offset	*/
EXTERN_DEF _segment		 segvar	INIT_VAL(BASE_VIDEO_SEG);
EXTERN_DEF char	_based(void)	*vp	INIT_ZERO;
#endif /* _MSC_VER */

#if defined (_INTELC32_) || defined (WATCOM)
/*
**	Intel 386/486 C Code Builder
*/
#define BASE_VIDEO_ADDR	((unsigned short *)0xB8000)
#endif /* _INTELC32_ */

#define VT(r,c)		(screen_memory[(80*(r))+(c)])		/* Video Token */

#define	VA(r,c)		(VT(r,c) & 0xFF00)			/* Attribute */
#define VC(r,c)		(VT(r,c) & 0x00FF)			/* Character */
#define SET_VT(r,c,v,a)	(VT(r,c) = ((v) & 0x00FF) | (a))	/* Set Token */
#define SET_VC(r,c,v)	(SET_VT(r,c,v,VA(r,c))		/* Set Video Character	*/
#define SET_VA(r,c,a)	(SET_VT(r,c,VC(r,c),a))		/* Set Video Attribute	*/
#define SET_CVT(v)	(SET_VT(out_row,out_col,(v),out_atr))
#define NEXT_COL	(out_col = ((out_col + 1) % 80))

#define SET_MF(b)	{v_modeflag(b,15,' ');}

EXTERN_DEF	unsigned	short	*screen_memory	INIT_ZERO;

					/* Working values used during run.   */
EXTERN_DEF	int	out_row		INIT_ZERO;	/* Output row.	     */
EXTERN_DEF	int	out_col		INIT_ZERO;	/* Output column.    */
EXTERN_DEF	int	out_atr		INIT_VAL(ANOR);	/* Output attribute. */

EXTERN_DEF	int	scroll_top	INIT_ZERO;	/* Scroll window.    */
EXTERN_DEF	int	scroll_bottom	INIT_VAL(24);	/* Scroll window.    */

EXTERN_DEF	int	mode_flag	INIT_ZERO;	/* Display flag?     */
/*
	Cursor section.
*/

#define	CURSOR_OFF		0x2000
#define CURSOR_UNDERLINE	0x0707
#define CURSOR_FULL		0x0007

#undef	EXTERN_DEF
#endif	/* VRAWDOS_INCLUDED */

#endif	/* MSDOS */

/*
**	History:
**	$Log: vrawdos.h,v $
**	Revision 1.10  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.9  1996/10/11 22:16:17  gsl
**	drcs update
**	
**
**
*/
