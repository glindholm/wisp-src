/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		vwang.h
**
**	Project:	wisp/common
**
**	RCS:		$Source:$
**
**	Purpose:	Header for vwang.c
**
*/

#ifndef VWANG_H
#define VWANG_H
/*
**	Includes
*/
#include "idsistd.h"
#include "scnfacs.h"

/*
**	Structures and Defines
*/

#define WSB_LENGTH	1924
#define WSB_COLS	80
#define WSB_ROWS	24

#define OA_LENGTH	4
#define OA_ROW		0
#define OA_WCC		1
#define OA_CURSOR_COL	2
#define OA_CURSOR_ROW	3
                                                      
#define WS_MAX_LINES_PER_SCREEN     24							/* Maximum lines per screen.		*/
#define WS_MAX_COLUMNS_ON_A_LINE   132							/* Maximum characters per line.		*/
#define WS_DEFAULT_COLUMNS_PER_LINE 80							/* Default characters per line.		*/
/*					VWANG function definitions								*/
#define DISPLAY			   0							/* COBOL DISPLAY verb.			*/
#define WRITE_ALL		   1							/* Write to screen.			*/
#define WRITE_ALL_PB		  11							/* Write to screen with pseudo blanks.	*/
#define READ_ALL		   2							/* Read from screen.			*/
#define READ_ALL_PB		  12							/* Read with pseudo blank processing.	*/
#define DISPLAY_AND_READ	   3							/* Write then read screen.		*/
#define CLOSE_WORK_STATION	   4							/* Close the workstation.		*/
#define WRITE_SELECTED		   5							/* Write selected fields.		*/
#define READ_ALTERED		   6							/* Read altered fields.			*/
#define READ_MODIFIABLE           10							/* Read modifiable fields.		*/
#define DISPLAY_AND_READ_ALTERED   7							/* Write then read altered.		*/
#define LOAD_SUB_TABLE		   8							/* Load user defined substitution table.*/
#define INIT_DEFAULT_TABLE	   9							/* Init the sub table to default values.*/
#define SET_TABS                  13 
#define TAB_MODE_OFF              14
/*					WCC (Write Control Character) definitions						*/
#define UNLOCK_KEYBOARD		((unsigned char)0x80)					/* Unlock the keyboard.			*/
#define SOUND_ALARM		((unsigned char)0x40)					/* Ring the bell.			*/
#define POSITION_CURSOR		((unsigned char)0x20)					/* Position cursor.			*/
#define ROLL_DOWN	    	((unsigned char)0x10)					/* Scroll screen down.			*/
#define ROLL_UP			((unsigned char)0x08)					/* Scroll screen up.			*/
#define ERASE_FIELDS		((unsigned char)0x04)					/* Output pseudoblanks.			*/
#define ERASE_AND_PROTECT	((unsigned char)0x02)					/* Erase and protect.			*/
/* *** Reserved (bit 7 must be zero) 												*/ 
/*					Indeces for VWANG screen Order Area							*/
#define ROW_NUMBER_BYTE		0							/* Starting row to display.		*/
#define WCC_BYTE		1							/* Hold WCC for screen.			*/
#define CURSOR_COL_BYTE		2							/* Position cursor column.		*/
#define CURSOR_ROW_BYTE		3							/* Position cursor row.			*/

#define AID_LOCKED		0x21							/* AID char for locked keyboard		*/
#define AID_UNLOCKED		0x20							/* AID char for unlocked keyboard	*/
#define AID_HELP		0x30							/* AID char for HELP			*/
#define AID_ENTER		0x40							/* AID char for ENTER			*/

/********************************************************************************************************************************/
/*					DEC substitution characters.								*/
/********************************************************************************************************************************/

#define PSEUDO_BLANK_CHAR	((char) 0x0b )						/* Explicit Psuedo blank character.	*/
#define WANG_MENU_PICK		0x05							/* Input from Wang for menu pick item.	*/
#define SPACE_BAR		0x20							/* Space bar input from keyboard.	*/
#define DEC_MENU_PICK		0x5F							/* Underscore for menu pick.		*/

#ifdef OLD
/* this has been replaced with a variable which can be adjusted */
#define MAX_DISP_RANGE	0xFF								/* All displayable chars < this value.	*/
#endif

#define EIGHT_BIT_DATA 0xFF
#define SEVEN_BIT_DATA 0x7F

#ifdef VMS 
#define CURRENT_MENU_PICK	0xBB							/* Double right arrow.			*/
#endif
#ifdef unix
#define CURRENT_MENU_PICK	0x2A							/* Asterisk.				*/
#endif
#if defined(MSDOS) || defined(WIN32)
#define CURRENT_MENU_PICK	0xAF							/* Double right arrow.			*/
#endif

#if defined(MSDOS) || defined(WIN32)							/* MS-DOS only - substitution chars:	*/

#define DEC_GRAPHIC_DIAMOND	0x04							/* Graphic diamond.			*/
#define DEC_GRAPHIC_BOX		0xB0							/* Graphic box.				*/
#define DEC_GRAPHIC_DEGREE	0xF8							/* Graphic degree symbol.		*/
#define DEC_PLUS_MINUS		0xF1							/* Graphic plus/minus sign.		*/
#define DEC_BOTM_RIGHT_BOX	0xD9							/* Graphic lower right corner of box.	*/
#define DEC_TOP_RIGHT_BOX	0xBF							/* Graphic top right corner of box.	*/
#define DEC_TOP_LEFT_BOX	0xDA							/* Graphic top left corner of box.	*/
#define DEC_BOTM_LEFT_BOX	0xC0							/* Graphic lower left corner of box.	*/
#define DEC_FOUR_CORNER		0xC5							/* Graphic (plus) four corners meet.	*/
#define DEC_GRAPHIC_HORIZ	0xC4							/* Graphic horizontal.			*/
#define DEC_VERT_RHORIZ		0xC3							/* Graphic |- .				*/
#define DEC_VERT_LHORIZ		0xB4							/* Graphic -| .				*/
#define DEC_HORIZ_TVERT		0xC1							/* Graphic horiz. with top vertical.	*/
#define DEC_HORIZ_BVERT		0xC2							/* Graphic horiz. with bottom vertical.	*/
#define DEC_GRAPHIC_VERT	0xB3							/* Graphic | .				*/
#define DEC_GRAPHIC_PI		0xE3							/* Graphic pi.				*/
#define DEC_VERTICAL_BAR	0x7C							/* DEC vertical bar or not equal sign.	*/
#define DEC_GRAPHIC_UK_POUND	0x9C							/* Graphic U.K. - pound sign.		*/
#define DEC_CENTERED_PERIOD	0xF9							/* Graphic centered period.		*/
#define DEC_EXCLAMATION		0xAD							/* Sp.graphic upside dowm exclamation.	*/
#define DEC_CENTS_SIGN		0x9B							/* Special graphic cents sign.		*/
#define DEC_GRAPHIC_SQUARE	0xFE							/* Special graphic square box.		*/
#define DEC_DBL_LEFT_ARROW	0xAE							/* Special graphic double left arrow.	*/
#define DEC_SUPSCPT_2		0xFD							/* Special graphic superscript 2.	*/
#define DEC_SUPSCPT_3		0xFC							/* Special graphic superscript 3.	*/
#define DEC_PARAGRAPH		0x14							/* Special graphic paragraph symbol.	*/
#define DEC_DBL_RIGHT_ARROW	0xAF							/* Special graphic double right arrow.	*/
#define DEC_ONE_FOURTH		0xAC							/* Special graphic One fourth symbol.	*/
#define DEC_ONE_HALF		0xAB							/* Special graphic One half symbol.	*/
#define DEC_QUESTION_MARK	0xA8							/* Sp.graphic upside down question mark.*/

#else	/* VMS and unix */								/* DEC terminal substitution chars:	*/

#define DEC_GRAPHIC_DIAMOND	0x60							/* Graphic diamond.			*/
#define DEC_GRAPHIC_BOX		0x61							/* Graphic box.				*/
#define DEC_GRAPHIC_DEGREE	0x66							/* Graphic degree symbol.		*/
#define DEC_PLUS_MINUS		0x67							/* Graphic plus/minus sign.		*/
#define DEC_BOTM_RIGHT_BOX	0x6A							/* Graphic lower right corner of box.	*/
#define DEC_TOP_RIGHT_BOX	0x6B							/* Graphic top right corner of box.	*/
#define DEC_TOP_LEFT_BOX	0x6C							/* Graphic top left corner of box.	*/
#define DEC_BOTM_LEFT_BOX	0x6D							/* Graphic lower left corner of box.	*/
#define DEC_FOUR_CORNER		0x6E							/* Graphic (plus) four corners meet.	*/
#define DEC_GRAPHIC_HORIZ	0x71							/* Graphic horizontal.			*/
#define DEC_VERT_RHORIZ		0x74							/* Graphic |- .				*/
#define DEC_VERT_LHORIZ		0x75							/* Graphic -| .				*/
#define DEC_HORIZ_TVERT		0x76							/* Graphic horiz. with top vertical.	*/
#define DEC_HORIZ_BVERT		0x77							/* Graphic horiz. with bottom vertical.	*/
#define DEC_GRAPHIC_VERT	0x78							/* Graphic | .				*/
#define DEC_GRAPHIC_PI		0x7B							/* Graphic pi.				*/
#define DEC_VERTICAL_BAR	0x7C							/* DEC vertical bar or not equal sign.	*/
#define DEC_GRAPHIC_UK_POUND	0x7D							/* Graphic U.K. - pound sign.		*/
#define DEC_CENTERED_PERIOD	0x7E							/* Graphic centered period.		*/
#define DEC_EXCLAMATION		0xA1							/* Sp.graphic upside dowm exclamation.	*/
#define DEC_CENTS_SIGN		0xA2							/* Special graphic cents sign.		*/
#define DEC_GRAPHIC_SQUARE	0xA8							/* Special graphic square box.		*/
#define DEC_DBL_LEFT_ARROW	0xAB							/* Special graphic double left arrow.	*/
#define DEC_SUPSCPT_2		0xB2							/* Special graphic superscript 2.	*/
#define DEC_SUPSCPT_3		0xB3							/* Special graphic superscript 3.	*/
#define DEC_PARAGRAPH		0xB6							/* Special graphic paragraph symbol.	*/
#define DEC_DBL_RIGHT_ARROW	0xBB							/* Special graphic double right arrow.	*/
#define DEC_ONE_FOURTH		0xBC							/* Special graphic One fourth symbol.	*/
#define DEC_ONE_HALF		0xBD							/* Special graphic One half symbol.	*/
#define DEC_QUESTION_MARK	0xBF							/* Sp.graphic upside down question mark.*/

#endif	/* VMS and unix */

/********************************************************************************************************************************/

/*
**	Function Prototypes
*/
int vwang();

char vwang_aid(void);
char vwang_aid_read_unlocked(void);
void vwang_timeout(int4 seconds);
void set_aid(char aid);
char meta_aid(int metachar);
int ws_fkey(int metachar_key,int *filling,unsigned char *terminate_list,unsigned char *pfkey,unsigned char *no_mod);
int ws_mod(int cursor_row, int cursor_col);						/* Determine if char is modifyable.	*/
int ws_bad_char(void);									/* Process invalid characters.		*/
int ws_cut(int row, int col);								/* Cut the current field.		*/
int ws_paste(int row, int col, int vr, int vc, int alt_read, unsigned char *no_mod, int do_pseudo);
int ws_sof(int cursor_row, int cursor_col);						/* Get start of current field.		*/ 
int ws_eof(int cursor_row, int cursor_col);						/* Get end of current field.		*/
void ws_erap(int amount);								/* Erase & protect screen sect. */
int ws_help(int curset);								/* Simulate Wang help function.		*/
int wpushscr(void);									/* A function to save the screen addrs	*/
int wpopscr(void);									/* A function to restore the screen and */
int ws80(void);										/* Select 80 column screen.		*/
int ws132(void);									/* Select a 132 column screen.		*/
char wscharat(int r, int c);								/* Get a character from a row and col.	*/
int SETFACS(unsigned char *new_table,unsigned char *toupper_table,unsigned char *numeric_table);
void SET8BIT(unsigned char *faclist,unsigned char *lowuplist,unsigned char *numlist);
int valid_char_data(char value);
fac_t fac(fac_t pseudo_FAC);
fac_t unfac(fac_t true_FAC);
fac_t fac_pre_vwang(fac_t pseudo_FAC);
fac_t unfac_pre_vwang(fac_t true_FAC);
void vwang_flush(void);
void vwang_write_bell(void);
int vwang_set_synch(int synch);
void vwang_shut(void);
void vwang_clear_on_shut(void);
void vwang_noclear_on_shut(void);
void vwang_synch(void);
void vwang_stty_save(void);
void vwang_stty_restore(void);
void vwang_stty_sane(void);
void vwang_stty_sync(void);
int vwang_set_reinitialize(int flag);
void vwang_pre_init(void);
void vwang_init_term(void);
int vwang_wcurwidth(void);
void vwang_bell(int cnt);
int vwang_keypressed(int discard);
void vwang_init_video(void);
void vwang_title(const char *the_title);
void vwang_load_charmap(int force);
void vwang_ansi2wang(unsigned char *buff, int len);
void vwang_wang2ansi(unsigned char *buff, int len);
void vwang_term2wang(unsigned char *buff, int len);
void vwang_wang2term(unsigned char *buff, int len);
void vwang_subtable(unsigned char *buff, int len);
void WANSI2WANG(unsigned char *buff, int2 *len);
void WWANG2ANSI(unsigned char *buff, int2 *len);
int terminal_control_char(unsigned char the_char);

int init_screen(void);

extern int use_custom_vwang(void);
extern int custom_vwang(unsigned char *function,unsigned char *wsb,unsigned char *lines,unsigned char *terminate_list,
	unsigned char *term,unsigned char *no_mod);

#endif /* VWANG_H */

/*
**	History:
**	$Log: vwang.h,v $
**	Revision 1.41  2001/10/15 13:48:40  gsl
**	Change vwang_set_videocap() to vwang_init_video()
**	
**	Revision 1.40  1997-12-19 10:00:57-05  gsl
**	Add terminal_control_character
**
**	Revision 1.39  1997-12-18 19:49:01-05  gsl
**	Fix CHARMAP prototypes
**
**	Revision 1.38  1997-12-15 10:40:37-05  gsl
**	add vwang_subtable()
**
**	Revision 1.37  1997-12-10 10:40:03-05  gsl
**	change args to vwang_load_charmap()
**
**	Revision 1.36  1997-12-08 16:20:28-05  gsl
**	Add CHARMAP routines
**
**	Revision 1.35  1997-12-08 16:10:59-05  gsl
**	Add the charmap routines
**
**	Revision 1.34  1997-10-29 11:29:39-05  gsl
**	Move all the FAC macros to scnfacs.h and then include it
**
**	Revision 1.33  1997-01-09 14:43:44-05  gsl
**	remove wsmove() and check_scrn() as these are now static
**
**	Revision 1.32  1996-11-18 15:53:30-08  jockc
**	added proto for vwang_title()
**
**	Revision 1.31  1996-11-12 16:18:30-08  gsl
**	Cleanup for NT
**
**	Revision 1.30  1996-11-04 15:43:39-08  gsl
**	Add vwang_set_videocap()
**
**	Revision 1.29  1996-08-16 14:45:45-07  gsl
**	Change to use typedef "fac_t" for a fac dataitem
**
**	Revision 1.28  1996-07-18 13:29:09-07  gsl
**	Add vwang_stty_sync()
**
**	Revision 1.27  1996-07-15 10:12:06-07  gsl
**	Add prototypes
**
**	Revision 1.26  1996-07-09 17:01:11-07  gsl
**	add vwang_keypressed()
**
**	Revision 1.25  1996-06-28 16:52:45-07  gsl
**	add routine vwang_pre_init()
**
**	Revision 1.24  1996-06-28 09:13:08-07  gsl
**	Adde prototypes from initscrn.c
**
**	Revision 1.23  1996-06-26 17:17:29-07  gsl
**	Add vwang_bell()
**
**	Revision 1.22  1996-06-24 11:27:59-07  gsl
**	Fix for both MSDOS and WIN NT
**
**	Revision 1.21  1995-06-13 07:41:16-07  gsl
**	made WCC defines unsigned char
**
 * Revision 1.20  1995/04/25  09:51:52  gsl
 * drcs state V3_3_15
 *
 * Revision 1.19  1995/04/17  11:45:24  gsl
 * drcs state V3_3_14
 *
 * Revision 1.18  1995/04/05  14:09:05  gsl
 * add include of idsistd.h
 *
 * Revision 1.17  1995/03/24  15:37:32  gsl
 * made ws_clear() static  by removing ref from EDE
 * Add prototypes for ws_sof() and ws_eof() - moved from edehelp.c
 *
 * Revision 1.16  1995/03/17  11:13:37  gsl
 * Added a full set of FAC manipulation macros.
 *
 * Revision 1.15  1995/03/15  13:28:06  gsl
 * Flip the OA row and col as I had them reversed
 *
 * Revision 1.14  1995/03/10  09:07:49  gsl
 * add video front-end routines
 *
 * Revision 1.13  1995/03/09  17:02:01  gsl
 * added vwang routines as front-ends to video routines
 *
 * Revision 1.12  1995/03/07  10:53:21  gsl
 * add defines for 1924, 80 and 24
 *
 * Revision 1.11  1995/02/17  12:43:55  gsl
 * remove vwang_message_box()
 *
 * Revision 1.10  1995/02/17  11:59:10  gsl
 * add vwang_write_bell() to ring the bell.
 * and vwang_message_box() to replace vre_window().
 *
 * Revision 1.9  1995/02/14  16:55:39  gsl
 * add vwang_aid_read_unlocked
 *
 * Revision 1.8  1995/02/14  15:51:56  gsl
 * *** empty log message ***
 *
 * Revision 1.7  1995/02/14  15:49:19  gsl
 * add vwang_flush() and standard comment headers
 *
**
*/
