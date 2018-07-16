/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*			Copyright (c) 1988, 1989, 1990			*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

/************************************************************************/
/*	Standard definitions						*/
/************************************************************************/

#ifndef CHAR_NULL			/* Use so VMS compiler likes it.*/
#define CHAR_NULL	'\000'		/* Standard null character def.	*/
#endif

#ifndef FALSE
#define FALSE		0		/* Standard false definition.	*/
#endif

#ifndef TRUE
#define TRUE		!FALSE		/* Standard true definition.	*/
#endif

#define FAILURE		0		/* Standard failure definition.	*/
#define SUCCESS		1		/* Standard success definition.	*/
#define OPTIMIZED	2		/* Action was optimized def.	*/

/************************************************************************/
/*	Define terminal geometry					*/
/************************************************************************/

#define	MAX_LINES_PER_SCREEN	24	/* Standard VT220 screen.	*/
#ifdef MSDOS
#define MAX_COLUMNS_PER_LINE	80	/* Normally in 80 column mode.	*/
#else	/* VMS and unix */
#define MAX_COLUMNS_PER_LINE   132	/* Normally in 80 column mode.	*/
#endif	/* VMS and unix */

/************************************************************************/
/*	vbuffering() parameters.					*/
/************************************************************************/

#define LOGICAL   1	/* Logical buffering (under program control).	*/
#define AUTOMATIC 2	/* Automatic buffering (WOW will do it).	*/

/************************************************************************/
/*	verase and vrefresh parameters.					*/
/************************************************************************/

#define FULL_SCREEN   0			/* Also define as 0 for vset	*/
#define FROM_BOS      1			/* Erase beginning of screen	*/
#define TO_EOS	      2			/* Erase to end of screen	*/
#define CURRENT_LINE  3			/* Erase all of current line	*/
#define FROM_BOL      4			/* Erase from beginning of line	*/
#define TO_EOL        5			/* Erase to end of line		*/
#define HARD_REFRESH  6			/* Erase full screen (hard).	*/
#define ALL_MENUS     7			/* Erase all menus.		*/
#define TOP_MENU      8			/* Erase the top menu level.	*/
#define TO_BOTTOM_MENU 9		/* Erase all but bottom menu.	*/

/************************************************************************/
/*	vsize parameters						*/
/************************************************************************/

#define SINGLE_WIDTH  0		      /* Single width and single height	*/
#define DOUBLE_WIDTH  256	      /* Double width but single height	*/
#define	DOUBLE_HEIGHT 512	      /* Double width and double height	*/

/************************************************************************/
/*	vmode parameters						*/
/************************************************************************/

#define CLEAR      0				/* Clear char rendition	*/
#define	BOLD       1				/* Select bold rend	*/
#define	UNDERSCORE 2				/* Underscore rendition */
#define	BLINK      4				/* Blinking rendition	*/
#define	REVERSE    8				/* Reverse video rend	*/

/************************************************************************/
/*	vcharset parameters						*/
/************************************************************************/

#define DEFAULT	      0		/* Default (US or UK) 			*/
#define UK            1		/* Character set for UK			*/
#define US            2		/* Character set for US			*/
#define GRAPHICS      16	/* Special graphics character set	*/
#define ROM_STANDARD  32	/* Alternate character ROM standard	*/
#define	ROM_GRAPHICS  64	/* Alternate ROM special grapics	*/
#define DOWN_LOADED  128	/* Down loaded character set.		*/

/************************************************************************/
/*	vline parameters						*/
/************************************************************************/

#define VERTICAL 0				/* Draw vertical line	*/
#define	HORIZONTAL	3			/* Horizontal line.	*/
#define FAT_VERTICAL    6			/* Reverse space vert.	*/
#define FAT_HORIZONTAL  7			/* Reverse space horiz.	*/

/************************************************************************/
/*	vclass character classification codes				*/
/************************************************************************/

#define	UPPER_CASE_ALPHABETIC	 1	/* Upper case alphabetic	*/
#define	LOWER_CASE_ALPHABETIC	 2	/* Lower case alphabetic	*/
#define	SPACE_CHARACTER		 3	/* Space character		*/
#define	NUMERIC			 4	/* Numeric digit		*/
#define PRINTING_SYMBOL		 5	/* Any symbol			*/
#define	TERMINATOR		 6	/* A CR, LF, BS or TAB		*/
#define	RUBOUT_OR_CTRL_U	 7	/* Rubout or control U		*/
#define	CTRL_W_CHARACTER	 8	/* Control W			*/
#define	ESCAPE_CHARACTER	 9	/* Escape character		*/
#define	CTRL_C_CHARACTER	10	/* Control C			*/
#define	OTHER_CONTROL_CHARACTER	11	/* Control not explicit recog.	*/
#define NON_PRINTING_SYMBOL	12	/* Some non-printing character  */

/************************************************************************/
/*      vstate() parameter definitions.					*/
/************************************************************************/

/* #define DEFAULT	0		   		As per vchset()	*/
#define	SAVE	       -1		/* Save the current term state.	*/
#define RESTORE		1		/* Restore a pushed term state.	*/
#define SAVE_DEBUG     -2		/* Save debugger's terminal.	*/
#define RESTORE_DEBUG	2		/* Restore from a debugger.	*/

/************************************************************************/
/*	video get string vgets(string,count,filter,tc) definitions 	*/
/************************************************************************/

			/* Filter values */

/* #define UPPER_CASE_ALPHABETIC   1	   As per vclass()		*/
/* #define LOWER_CASE_ALPHABETIC   2	   As per vclass()		*/
/* #define NUMERIC		   4	   As per vclass()		*/
#define	OCTAL			   8	/* Octal digits are ok		*/
#define HEXADECIMAL		  16	/* Hex digits are ok            */
#define	Y_OR_N			  32	/* Yes or no (Y or N) ok	*/
#define	SPACE			  64	/* Space character is ok	*/
#define	LEADING_SIGN		 128	/* Leading + or - sign ok	*/
#define	SINGLE_PERIOD		 256	/* Single embedded period ok	*/
#define	SINGLE_COLON		 512	/* Single embedded colin ok	*/
#define	ANY_SYMBOL		1024	/* All printing symbols ok	*/
#define	CHARACTER_MASK		2048	/* Any character in mask ok	*/
#define	ANY_CHARACTER		4096	/* Any character is allowed	*/

		/* Termination control values	*/

#define	SPECIAL_TERMINATORS	   1	/* Terminate on HT,VT,BS or LF	*/
#define	END_ON_FIELD_FULL	   2	/* Terminate when field is full	*/
#define	RESPONSE_REQUIRED	   4	/* Null response not allowed	*/
#define	FIELD_MUST_BE_FILLED	   8	/* Field must be filled		*/
#define	PAD_WITH_SPACES		  16	/* Set string to spaces on end	*/
#define	RIGHT_JUSTIFY		  32	/* Right justify (leading spcs)	*/
#define	NO_ECHO			  64	/* Do not echo the character	*/
#define	DISPLAY_ONLY		 128	/* Do not accept input here	*/
#define	NO_PRE_CLEAR		 256	/* Don't clr field before entry	*/
#define	PARTIAL_KB_APPLICATIONS	 512	/* Convert KP keys to digits	*/
#define	NONSTANDARD_PF_KEYS	1024	/* PF keys don't abort input	*/

		  /* Values returned from vgets	*/

#define	NO_INPUT		   0	/* No input was given		*/
#define	TERM_VT			   1	/* Input was terminated with VT	*/
#define	TERM_LF			   2	/*   "    "      "       "   LF	*/
#define	TERM_HT			   3	/*   "    "      "       "   HT	*/
#define	TERM_BS			   4	/*   "    "      "       "   BS	*/
#define	TERM_CR			   5	/*   "    "      "       "   CR	*/
#define	TERM_FIELD_FULL		   6	/* Full field termination	*/

/************************************************************************/
/*	vdefer() parameter definitions.					*/
/************************************************************************/

#define PURGE		0		       /* Purge deferred state. */
/* #define SAVE	       -1				As per vstate()	*/
/* #define RESTORE	1				As per vstate()	*/

/************************************************************************/
/*	video setup (vset) parameters					*/
/************************************************************************/

#define	KEYPAD		0				/* Special.	*/
#define	CURSOR_KEYS	1				/* Type 1.1	*/
#define				NORMAL		0
#define				APPLICATIONS	1

#define	TERMINAL	2				/* Type 1.2	*/
#define				VT52		0
#define				ANSI		1	/* Special.	*/

#define	SCROLL		3				/* Type 1.4	*/
#define				JUMP		0
#define				SMOOTH		1

#define	ORIGIN		4				/* Type 1.6	*/
#define				TOP_OF_SCREEN	0
#define				SCROLL_AREA	1

#define	AUTO_WRAP	5				/* Type 1.7	*/
#define	AUTO_REPEAT	6				/* Type 1.8	*/
#define	INTERLACE	7				/* Type 1.9	*/
#define	AUTO_PRINT	8				/* Type 2.4	*/
#define	PRINTER		9				/* Type 3.4	*/
#define	HOST_ECHO	10				/* Type 4.12	*/
#define	NEW_LINE_MODE	11				/* Type 4.20	*/
#define				OFF		0
#define				ON		1

#define	KEYBOARD	12				/* Type 4.2	*/
#define				UNLOCKED	0
#define				LOCKED		1

#define	INSERT_MODE	13				/* Type 4.4	*/
#define				REPLACE		0
#define				INSERT		1

#define	CURSOR		14				/* Type 1.25	*/
#define				INVISIBLE	0
#define				VISIBLE		1

#define	PRINT_TERMINATOR 15				/* Type 1.18	*/
#define				NONE		0
#define				FORM_FEED	1

#define	PRINT_EXTENT	16				/* Type 1.19	*/
/* #define 			FULL_SCREEN	0 	As per verase()	*/
/* #define			SCROLL_AREA	1	As per vorigin	*/

/************************************************************************/
/*	vscreen definitions.						*/
/************************************************************************/

#define NARROW		1			/* Set screen narrow.	*/
#define WIDE		2			/* Set screen wide.	*/
#define	LIGHT		4			/* Set screen light.	*/
#define DARK		8			/* Set screen dark.	*/

/************************************************************************/
/*	vnewline and vlinefeed definitions.				*/
/************************************************************************/

#define	FORWARD		0	/* Do a new-line in forward direction.	*/
/* #define REVERSE	8 */	/* Do a new-line in reverse direction.	*/

/************************************************************************/
/*	voptimize() control values.					*/
/************************************************************************/

/* #define OFF		  0 */	/* As per vset.				*/
#define TRACKING_ONLY     1	/* Only track output, no optimization.	*/
#define DATA_ONLY	  2	/* Data only optimization.		*/
#define DATA_AND_CONTROLS 3	/* Optimize data and controls.		*/
#define	DATA_CONTROLS_AND_MOTION 4	/* Optimize data and movement.	*/
#define DEFER_MODE	  5	/* Deferred action optimization.	*/
#define BLOCK_MODE	  6	/* Transparent block mode optimization.	*/

/************************************************************************/
/*	vcontrol parameters.						*/
/************************************************************************/

#define DUMP_OUTPUT (unsigned char *) 0	  /* Flag buffer to be dumped.	*/

/************************************************************************/
/*	vterminal response parameters					*/
/************************************************************************/

#define UNKNOWN 	0		/* Unknown terminal type.	*/
#define VT100   	1		/* VT100 class terminal.	*/
#define VT200   	2		/* VT200 class terminal.	*/
#define VT300   	3 		/* VT300 class terminal.	*/
#define PRO350_RT	5		/* Pro350 running RT-11.	*/
#define ATT605		6		/* AT&T 605 terminal.		*/
#define OTHER_ANSI_TERMINAL 9		/* Some other ANSI terminal.	*/
#define IBM_PC		10		/* IBM PC (direct).		*/
#define GENERIC_TERMINAL	20	/* Generic terminal.		*/
#define UNSUPPORTED_TERMINAL	99	/* Unsupported terminal.	*/

/************************************************************************/
/*	vlist function call definitions.				*/
/************************************************************************/

#define DISPLAY_LIST	0
#define INIT_LIST	1
#define ADD_HEADER	2
#define ADD_FOOTER	3
#define ADD_COLUMN	4
#define INSERT_COLUMN	5
#define DELETE_COLUMN	6
#define REPLACE_COLUMN	7
#define RESIZE_LIST	8
#define SET_FUNCTIONS	9
#define FREE_LIST	10
#define NO_DISPLAY_LIST 11
#define DISP_ROW_ITEM	12

/************************************************************************/
/*	vlist finction key definition.					*/
/************************************************************************/

#define ALLOW_KEY	0		/* Allow termination of scan.	*/
#define UP_PAGE		1
#define DOWN_PAGE	2
#define VLIST_TOP	3
#define VLIST_BOTTOM	4
#define RIGHT_COL	5
#define LEFT_COL	6
#define RIGHT_PAGE	7
#define LEFT_PAGE	8
#define SELECT_ROW	9
#define DESELECT_ROW	10

/************************************************************************/
/*	vlist column type definitions.					*/
/************************************************************************/

#define RSHORT		0		/* Regular short type def.	*/
#define USHORT		1		/* Unsigned short type def.	*/
#define RLONG		2		/* Regular long type def.	*/
#define ULONG		3		/* Unsigned long type def.	*/
#define TEXTIT		4		/* String text type def.	*/
#define SEPARATOR	5		/* String separator type def.	*/

/************************************************************************/
/*	vkbmap() function definitions					*/
/************************************************************************/

#define INITIALIZE	0		/* Initialize keyboard mapping.	*/
#define LOAD_FROM_FILE	1		/* Load keyboard map from file.	*/
#define UNLOCK		2		/* Unlock the current map.	*/

/************************************************************************/
/*	verror() option codes.						*/
/************************************************************************/

/* #define CLEAR	0		   Clear error (as per vmode())	*/
#define WAIT_FOR_INPUT	1		/* Output an error and wait.	*/
/* #define AUTOMATIC	2   Automatically clear error on on next input.	*/

/************************************************************************/
/*	vonexit()							*/
/************************************************************************/

/* #define NARROW	1			   Set screen narrow.	*/
/* #define WIDE		2			   Set screen wide.	*/
/* #define LIGHT	4			   Set screen light.	*/
/* #define DARK		8			   Set screen dark.	*/
#define CLEAR_SCREEN	16			/* Clear the screen.	*/
#define MOVE_HOME	32			/* Move home.		*/
#define MOVE_AND_SCROLL	64			/* Move and scroll.	*/
#define NORMALIZE	128			/* Normalize terminal.	*/
#define MOVE_BOTTOM	256			/* Bottom (no scroll)	*/

/************************************************************************/
/*	vmacro()							*/
/************************************************************************/

#define START_SAVE	1		/* Start keystroke macro.	*/
#define END_SAVE	2		/* End saving keystroke macro.	*/
#define START_RESTORE	3		/* Start restoring a macro.	*/

/************************************************************************/
/*	vform()								*/
/************************************************************************/

#define TOGGLE_KEY_DISPLAY	 10

#define MAX_NUMBER_OF_FORMS	128
#define MAX_FORM_FIELDS		256
#define MAX_FORM_LINES		 64
#define MAX_PROCESSING_CHARS   8192
#define MAX_INIT_CHARS	       8192
#define HP_FORM_NAME_SIZE	 15

#define FORM_READ_ONLY		  1
#define FORM_WRITE_ONLY		  2

#define FIELD_DISPLAY_ONLY	  0
#define FIELD_REQUIRED		  1
#define FIELD_PROCESSED		  2
#define FIELD_OPTIONAL		  4

#define FIELD_DATA_CHAR		  0
#define FIELD_DATA_MDY		  1
#define FIELD_DATA_DMY		  2
#define FIELD_DATA_YMD		  3
#define FIELD_DATA_DIG		  4
#define FIELD_DATA_NUM		  5
#define FIELD_DATA_IMP		  6

#define HP_ENHANCEMENT	       0100
#define HP_BLINK		  1
#define HP_INVERSE		  2
#define HP_UNDERLINE		  4
#define HP_HALF_BRIGHT		  8

#define COLLECT_MODE		  0
#define NO_REPEAT_OR_APPEND	  0
#define CLEAR_CURRENT_FORM	  0
