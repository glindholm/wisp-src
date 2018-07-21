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

/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*		Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

#ifndef VMENU_INCLUDED
#define VMENU_INCLUDED

#include "vintdef.h"

#define UNKNOWN_MENU		0	/* UNKNOWN			*/
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
	char text[MAX_MENU_ITEMS][MAX_MENU_WIDTH];

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

int4 VL_vmenugo(struct video_menu *mdata);							/* Display a menu.			*/
int4 VL_vmenucont(struct video_menu *mdata);						/* Continue displaying menu's.		*/
void VL_vdynalink(struct video_menu *mdroot, struct video_menu *mdlink);			/* Dynamically continue processing.	*/
void VL_vdynaunlink(struct video_menu *md);						/* Unlink a dynamic linked structure.	*/
int VL_vmenuinit(struct video_menu *md, int t, int o, int r, int c, int w);		/* Get the menu data.			*/
int VL_vmenuitem(struct video_menu *md, char *s, int v, struct video_menu *nm);
int VL_vmenumode(int mode);								/* Change the menu mode.		*/
int VL_vdetpos(int scrpos, int *r, int *c, int rs, int cs);				/* Suggest a row and column.		*/
int VL_vmenu_pfkeys(int state);
int VL_vmenustatus(int *r1, int *r2);
int VL_vmenusave(struct video_menu *md);							/* Save a menu's choice path.		*/
int VL_vmenurestore(void);									/* Restore a choice path.		*/

int  VL_vlastlink(struct video_menu *md);							/* Get if the last selection links.	*/
int  VL_vlastlevel(struct video_menu *md);							/* Return the current path level.	*/
int4 VL_vlastitem(struct video_menu *md);							/* Return the current path level.	*/

#define DYNAMIC_LINK (struct video_menu *) -1	/* Dynamic link code.	*/
#endif	/*  VMENU_INCLUDED	*/

/*
**	History:
**	$Log: vmenu.h,v $
**	Revision 1.15  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.14  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/15 20:16:11  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  1997/07/09 16:40:30  gsl
**	Add include of vintdef.h
**	
**	Revision 1.11  1996-10-11 18:16:12-04  gsl
**	drcs update
**
**
**
*/
