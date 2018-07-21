/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/


#ifndef VCAP_HEADER
#define VCAP_HEADER

#if defined(WIN32)
/*
**	Don't ever ever change this as \035 is hardcoded into the internal msdos vcap definitions.
*/
#define	EXT_PREFIX	((unsigned char)'\035')	/* Extended key code prefix	*/
#endif

/* these next 3 are used to specify where a key came from, so the load code can decide whether or
   no to override something */
#define SRC_TERMINFO 1
#define SRC_VIDEOINFO 2
#define SRC_STDDEFS 3
#define SRC_EMPTY 0

#define NOMAPPING -1

#define ISKEY  1
#define ISCAP  2
#define ISBOOL 3
#define ISNUM  4

/*	Define the indexes into the graphics string graphstr definition. Never remove, only and to the end.			*/

#define SINGLE_VERTICAL_BAR		0
#define SINGLE_HORIZONTAL_BAR		1
#define SINGLE_UPPER_LEFT_CORNER	2
#define SINGLE_UPPER_RIGHT_CORNER	3
#define SINGLE_LOWER_LEFT_CORNER	4
#define SINGLE_LOWER_RIGHT_CORNER	5
#define SINGLE_UPPER_TEE		6
#define SINGLE_LOWER_TEE		7
#define SINGLE_LEFT_TEE			8
#define SINGLE_RIGHT_TEE		9
#define SINGLE_CROSS		       10
#define LEFT_POINTER		       11
#define RIGHT_POINTER		       12
#define GRAPHSTR_LEN		       13

/*	The defines for video CAPABILITIES ***** Only add to the end. Don't delete any. *****					*/

#define VCAP_BACKSPACE 		1
#define AUTO_MARGINS 		2
#define HAS_HW_TABS 		3
#define NL_IGN_AFTER_WRAP 	4
#define MOVE_STANDOUT_MODE 	5
#define INIT_FILE 	   	6
#define RESET_TERMINAL 		7
#define ENTER_KP_XMIT 		8
#define EXIT_KP_XMIT 		9
#define VCAP_TAB 		10
#define VIRTUAL_TERM_NUM 	11
#define SAVE_CURSOR 		12
#define RESTORE_CURSOR 		13
#define VCAP_BELL 		14
#define BACK_TAB 		15
#define CARRIAGE_RETURN 	16
#define CHANGE_SCROLL_REGION 	17
#define CLEAR_ALL_TABS 		18
#define CLR_SCREEN 		19
#define CLEAR_BOL 		20
#define CLEAR_EOL 		21
#define CLEAR_EOS 	     	22
#define CURSOR_ADDRESS 		23
#define CURSOR_DOWN 		24
#define CURSOR_HOME 		25
#define CURSOR_INVISIBLE      	26
#define CURSOR_VISIBLE 		27
#define CURSOR_LEFT 		28
#define CURSOR_NORMAL 		29
#define CURSOR_RIGHT 		30	
#define CURSOR_UP 		31
#define DELETE_CHAR 		32	
#define DELETE_LINE 		33	
#define ENTER_ALT_CHARSET 	34
#define ENTER_AM_MODE 	     	35
#define ENTER_BLINK_MODE 	36
#define ENTER_BOLD_MODE 	37
#define ENTER_DELETE_MODE 	38
#define ENTER_DIM_MODE 		39
#define ENTER_INSERT_MODE 	40
#define ENTER_PROTECTED_MODE 	41
#define ENTER_REVERSE_MODE 	42
#define ENTER_SECURE_MODE 	43
#define ENTER_STANDOUT_MODE 	44
#define ENTER_UNDERLINE_MODE 	45
#define EXIT_ALT_CHARSET 	46
#define EXIT_AM_MODE 		47
#define EXIT_ATTRIBUTE_MODE 	48
#define EXIT_DELETE_MODE 	49
#define EXIT_INSERT_MODE      	50
#define EXIT_STANDOUT_MODE 	51
#define EXIT_UNDERLINE_MODE 	52
#define INIT_TERMINAL 		53
#define INSERT_CHAR 		54
#define INSERT_LINE 		55
#define OTHER_KEYS 		56
#define VCAP_NEWLINE 		57
#define SCROLL_REVERSE 		58
#define VCAP_COLUMNS 		59
#define VCAP_LINES 		60
#define SO_BLANKS 		61
#define US_BLANKS 		62
#define GRAPHSTR                63
#define ENTER_GRAPHICS_MODE     64
#define EXIT_GRAPHICS_MODE      65
#define VCAP_PAD                66
#define PSEUDO_BLANKS           67
#define VCAP_INDEX              68
#define REVERSE_INDEX           69
#define WIDE_MODE               70
#define NARROW_MODE             71
#define SCREEN_NORMAL_MODE	72
#define SCREEN_REVERSE_MODE	73
#define COLOR_LOOKUP_TABLE_0	74
#define COLOR_LOOKUP_TABLE_1	75
#define COLOR_LOOKUP_TABLE_2	76
#define COLOR_LOOKUP_TABLE_3	77
#define COLOR_LOOKUP_TABLE_4	78
#define COLOR_LOOKUP_TABLE_5	79
#define COLOR_LOOKUP_TABLE_6	80
#define COLOR_LOOKUP_TABLE_7	81
#define COLOR_LOOKUP_TABLE_8	82
#define COLOR_LOOKUP_TABLE_9	83

#define VC_CAP_COUNT	COLOR_LOOKUP_TABLE_9

/*
**	Default values
*/
#define DEF_GENERIC_MOUSE	"\376@"

extern int VL_vcap_padding;

#define VCAP_NEED_NOWRAP      0x00000008
#define VCAP_NEED_FKEYS1_10   0x00000010
#define VCAP_NEED_FKEYS1_16   0x00000020
#define VCAP_NEED_FKEYS1_32   0x00000040
#define VCAP_WARN_PRMSG       0x00000080
#define VCAP_NOWARN_RETSTAT   0x00000100

#define VCAP_WARN_AUTOWRAP    0x00000001
#define VCAP_WARN_NOFKEYS     0x00000002
#define VCAP_WARN_NOVCAPFIL   0x00000004
#define VCAP_WARN_NOWCONFIG   0x00000008


#define PBLANK1 VL_vcapvalue(PSEUDO_BLANKS)[0]
#define PBLANK2 VL_vcapvalue(PSEUDO_BLANKS)[1]
#define PBLANK3 VL_vcapvalue(PSEUDO_BLANKS)[2]
#define PBLANK4 VL_vcapvalue(PSEUDO_BLANKS)[3]
#define PSEUDO_BLANKS_LEN 4

/*
**	Functions and Prototypes
*/

char* VL_vkeyvalue(int key);
char* VL_vcapvalue(int cap);

#endif /* VCAP_HEADER */

/*
**	History:
**	$Log: vcap.h,v $
**	Revision 1.27  2003/06/20 15:48:03  gsl
**	VL_ globals
**	
**	Revision 1.26  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.25  2003/01/28 21:39:52  gsl
**	Change vcap.h defines to make unique BELL -> VCAP_BELL etc.
**	
**	Revision 1.24  2002/07/18 21:04:21  gsl
**	Remove MSDOS code
**	
**	Revision 1.23  2002/07/15 20:16:07  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.22  1997/07/08 20:50:14  gsl
**	change to use VL_vcapvalue()
**	Add new prototypes
**	Remove non-global data variables
**	
**	Revision 1.21  1997-05-21 12:52:29-04  gsl
**	Move the metakey defines to video.h
**
**	Revision 1.20  1996-10-11 18:16:00-04  gsl
**	drcs update
**
**
**
*/
