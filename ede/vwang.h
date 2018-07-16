/********************************************************************************************************************************/
/*						VWANG definitions								*/
/********************************************************************************************************************************/
                                                       
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
/*					WCC (Write Control Character) definitions						*/
#define UNLOCK_KEYBOARD		0x80 							/* Unlock the keyboard.			*/
#define SOUND_ALARM		0x40							/* Ring the bell.			*/
#define POSITION_CURSOR		0x20							/* Position cursor.			*/
#define ROLL_DOWN	    	0x10							/* Scroll screen down.			*/
#define ROLL_UP			0x08							/* Scroll screen up.			*/
#define ERASE_FIELDS		0x04							/* Output pseudoblanks.			*/
#define ERASE_AND_PROTECT	0x02							/* Erase and protect.			*/
/* *** Reserved (bit 7 must be zero) 												*/ 
/*					Indeces for VWANG screen Order Area							*/
#define ROW_NUMBER_BYTE		0							/* Starting row to display.		*/
#define WCC_BYTE		1							/* Hold WCC for screen.			*/
#define CURSOR_COL_BYTE		2							/* Position cursor column.		*/
#define CURSOR_ROW_BYTE		3							/* Position cursor row.			*/

#define FAC_CHARACTER		0x80 							/* FAC ID bit.				*/
#define FAC_ALTERED		0x40 							/* Altered read bit.			*/
#define FAC_SELECTED		0x40 							/* Selected write bit.			*/
#define FAC_UNDERSCORE  	0x20 							/* Underscore bit.			*/
#define FAC_BLINKING		0x10 							/* Blinking bit.			*/
#define FAC_LOW_INTENSITY	0x08 							/* Low intensity bit.			*/
#define FAC_PROTECT		0x04 							/* Protected char bit.			*/
#define FAC_NUMERIC_ONLY	0x02 							/* Numeric only bit.			*/
#define FAC_UPPERCASE_ONLY	0x01 							/* Uppercase only bit.			*/

/********************************************************************************************************************************/
/*					DEC substitution characters.								*/
/********************************************************************************************************************************/
#define PSEUDO_BLANK_CHAR	((char) 0x0b )						/* Explicit Psuedo blank character.	*/
#define WANG_MENU_PICK		0x05							/* Input from Wang for menu pick item.	*/
#define SPACE_BAR		0x20							/* Space bar input from keyboard.	*/
#define DEC_MENU_PICK		0x5F							/* Underscore for menu pick.		*/

#define MAX_DISP_RANGE	0x7F								/* All displayable chars < this value.	*/

#ifdef VMS 
#define CURRENT_MENU_PICK	0xBB							/* Double right arrow.			*/
#endif
#ifdef unix
#define CURRENT_MENU_PICK	0x2A							/* Asterisk.				*/
#endif
#ifdef MSDOS
#define CURRENT_MENU_PICK	0xAF							/* Double right arrow.			*/
#endif

#ifdef	MSDOS										/* MS-DOS only - substitution chars:	*/

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
