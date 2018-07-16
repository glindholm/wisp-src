/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*		Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

#ifndef VMENU_INCLUDED
#define VMENU_INCLUDED

/* #define LOAD_FROM_FILE	1	   As per vloadchar.		*/
#define NUMERIC_MENU		2	/* Numeric selection menu.	*/
#define ALPHABETIC_MENU		3	/* Highlighted alphabetic.	*/
#define HIGHLIGHTED_MENU	4	/* Highlighted position menu.	*/
#define BAR_MENU		5	/* Memu bar.			*/
#define PULL_DOWN_MENU		6	/* Mac style pull down menu.	*/
#define POP_UP_MENU		7	/* Pop-up window menu.		*/
#define DISPLAY_ONLY_MENU	8	/* Menu has text only.		*/

#define MAX_MENU_ITEMS  20
#define MAX_MENU_WIDTH  64

#define DYNAMIC_MENU		-1	/* Menus can be dynamic.	*/
#define STATIC_MENU		-2	/* Menus are statically linked.	*/

#define LEFT_HANDED		512	/* Chain to the left.		*/
#define RIGHT_HANDED	       1024	/* Chain to the right.		*/
#define EXIT_RIGHT	       2048	/* Exit to the right.		*/
#define CENTER_MENU	       4096	/* Position by centering.	*/



struct video_menu
{
	int4 type;
	int4 options;
	int4 row;
	int4 column;
	int4 width;
	int4 items;
	int4 item;
	int4 backitem;
	int4 code[MAX_MENU_ITEMS];
	unsigned char text[MAX_MENU_ITEMS][MAX_MENU_WIDTH];

        /*
         * everything after this point is lumped together  
         * in the COBOL FILLER PIC X(184) area, since COBOL
         * doesn't need to see these items.
         *
         */
	unsigned char *save;
	struct video_menu *path;
	struct video_menu *backlink;
	struct video_menu *link[MAX_MENU_ITEMS];
#if !defined(OSF1_ALPHA)
	/* 
         * on 4-byte pointer machines (everything but Alpha so far)
	 * we need some dummy space to correspond with the rest of 
         * the COBOL  FILLER PIC X(184).  Sizes are as follows:
         *
         * item       size on alpha:        size on other:
         * ----       -------------         -------------
         * save            8                     4
         * path            8                     4
         * backlink        8                     4
         * link[]         160                    80
         *           ---------------     ------------------
         *         TOTAL: 184                    92
         *
         */ 
         char filler[92];
#endif
};

#define DYNAMIC_LINK (struct video_menu *) -1	/* Dynamic link code.	*/
#endif	/*  VMENU_INCLUDED	*/

