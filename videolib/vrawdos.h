/*
	IDSI:1991
	International Digital Scientific, Incorporated
	Copyright, 1991
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

/*
	Screen Attribute Components.
	Select a component from each group and "OR" together
	to create a video character attribute.
*/
				/* Group 1; Blink Characteristic	*/
#define FORE_STEADY	0x0000
#define FORE_BLINK	0x8000
				/* Group 2; Background Color		*/
#define BACK_BLACK	0x0000
#define BACK_BLUE	0x1000
#define BACK_GREEN	0x2000
#define BACK_CYAN	0x3000
#define BACK_RED	0x4000
#define BACK_MAGENTA	0x5000
#define BACK_BROWN	0x6000	/* This looks more like Orange.		*/
#define BACK_WHITE	0x7000
				/* Group 3; Forground Intensity 	*/
#define FORE_DIM	0x0000
#define FORE_BRIGHT	0x0800	/*	Video calls this "BOLD"		*/
				/* Group 4; Forground Color		*/
#define FORE_BLACK	0x0000
#define FORE_BLUE	0x0100
#define FORE_GREEN	0x0200
#define FORE_CYAN	0x0300
#define FORE_RED	0x0400
#define FORE_MAGENTA	0x0500
#define FORE_BROWN	0x0600	/* This is Yellow when Bright.		*/
#define FORE_WHITE	0x0700

		/* IDSI Default Attributes:	*/
#define ATTRIBUTE_NORMAL		(BACK_BLUE	| FORE_WHITE)
#define ATTRIBUTE_UNDERSCORE		(BACK_CYAN	| FORE_WHITE)
#define ATTRIBUTE_REVERSE		(BACK_WHITE	| FORE_BLACK)
#define ATTRIBUTE_REVERSE_UNDERSCORE	(BACK_WHITE	| FORE_BROWN)
#define ATTR_REV_US	ATTRIBUTE_REVERSE_UNDERSCORE

			/* Attribute Variables: 	*/
EXTERN_DEF int attribute_normal		INIT_VAL(ATTRIBUTE_NORMAL);
EXTERN_DEF int attribute_underscore 	INIT_VAL(ATTRIBUTE_UNDERSCORE);
EXTERN_DEF int attribute_reverse	INIT_VAL(ATTRIBUTE_REVERSE);
EXTERN_DEF int attribute_reverse_underscore	INIT_VAL(ATTR_REV_US);

/*
	Video Attributes and Bit Values:

		Clear		0x00
		Bold		0x01
		Underscore	0x02
		Blink		0x04
		Reverse 	0x08
*/

#define ANOR	ATTRIBUTE_NORMAL
#define AFBR	FORE_BRIGHT
#define AUND	ATTRIBUTE_UNDERSCORE
#define AFBL	FORE_BLINK
#define AREV	ATTRIBUTE_REVERSE
#define ARUS	ATTRIBUTE_REVERSE_UNDERSCORE

#ifdef EXT_VRAWDOS
EXTERN_DEF	int	attributes[16] =
{
	( ANOR ),	 /*0*/		( ANOR | AFBR ),	/*1*/
	( AUND ),	 /*2*/		( AUND | AFBR ),	/*3*/
	( ANOR | AFBL ), /*4*/		( ANOR | AFBR | AFBL ),	/*5*/
	( AUND | AFBL ), /*6*/		( AUND | AFBR | AFBL ), /*7*/
	( AREV ),	 /*8*/		( AREV | AFBR ),	/*9*/
	( ARUS ),	 /*A*/		( ARUS | AFBR ),	/*B*/
	( AREV | AFBL ), /*C*/		( AREV | AFBR | AFBL ), /*D*/
	( ARUS | AFBL ), /*E*/		( ARUS | AFBR | AFBL )  /*F*/
} ;
#else
EXTERN_DEF	int	attributes[16];
#endif

/*
	The following items are used for direct screen memory access.
*/

#define BASE_VIDEO	0xB800					/* Segment address for VGA or EGA mode 3 or 7	*/
#define SM_SO		((unsigned int *)(segvar:>vp))		/* Screen Memory Segment Offset	*/
#define VT(r,c)		(screen_memory[(80*(r))+(c)])		/* Video Token */

#define	VA(r,c)		(VT(r,c) & 0xFF00)			/* Attribute */
#define VC(r,c)		(VT(r,c) & 0x00FF)			/* Character */
#define SET_VT(r,c,v,a)	(VT(r,c) = ((v) & 0x00FF) | (a))	/* Set Token */
#define SET_VC(r,c,v)	(SET_VT(r,c,v,VA(r,c))		/* Set Video Character	*/
#define SET_VA(r,c,a)	(SET_VT(r,c,VC(r,c),a))		/* Set Video Attribute	*/
#define SET_CVT(v)	(SET_VT(out_row,out_col,(v),out_atr))
#define NEXT_COL	(out_col = ((out_col + 1) % 80))

#define SET_MF(b)	((mode_flag) ? v_modeflag(b,15,' ') : 0)

EXTERN_DEF	_segment		 	segvar	INIT_VAL(BASE_VIDEO);
EXTERN_DEF	char	_based(void)	*vp	INIT_ZERO;

EXTERN_DEF	unsigned	int	*screen_memory	INIT_ZERO;

					/* Start-up values restored on exit. */
EXTERN_DEF	int	orig_screen[2000];	/* Original screen memory.   */
EXTERN_DEF	char	orig_cpr[12];		/* Original cursor position report.      */

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

