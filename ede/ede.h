/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** EDE 
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

/*
**	File:		ede.h
**
**	Project:	EDE
**
**	Purpose:	Function prototypes for EDE API
**
*/

#ifndef ede_H
#define ede_H

#include "vmenu.h"

int EDE_using(void);	/* Is EDE being used */

void A_WSLINK(unsigned char *ets_link_program_definition, unsigned char *on_line_doc_passing_values);
void DYLINK(struct video_menu *base, struct video_menu *dyna);
void DYUNLINK(struct video_menu *md);
void EDCLRSCR(void);
void EDDRKSCR(void);
void EDEXIT(void);
void EDLOAD(void);
void EDLTESCR(void);
void EDNARSCR(void);
void EDWIDSCR(void);			/* Make the screen wide.		*/
int  gcalc(void);
int  gcalend(void);
int  gclock(void);
void GENVEC(char *program, char *inparms, char *outparms);
int  gen_ncpfkey( int type, char** wsb, int num_chars, int* st_win, int* end_win);
int  gnotepad(void);
int  gpuzzle(void);
void MENUCONT(struct video_menu *md, int *v);		/* Continue the menu.			*/
void MENUEXIT(struct video_menu *md);			/* Erase all menus.			*/
void MENUGO(struct video_menu *md, int *v);		/* Run the menu.			*/
void EDE_MENUINFO(struct video_menu *mcb, int *level, int *item, int *link);	/* Return the menu level and item.	*/
void MENUITEM(struct video_menu *md, char *t, int *v, void *p);			/* Get the item data.			*/
void MENUKILL(struct video_menu *md, int *how);					/* Erase menus.				*/
void MENULOAD(struct video_menu *md, int *t, int *o, int *r, int *c, int *w);	/* Get the menu data.			*/
void MENUMODE(int *mode);		/* Menu mode.				*/
void MENUREST(void);			/* Start restoration.			*/
void MENUSAVE(struct video_menu *md);	/* Save the current menu choice path.	*/
int  nc_pop_menu( int* filling, const char* terminate_list, unsigned char* no_mod, char* pfkey );
void NOPFKEYS(void);			/* No PF keys.				*/
void PFKEYSON(void);			/* Turn on the PF keys.			*/
void POPAREA(unsigned char **s);	/* Restore the screen area.		*/
void PUSHAREA(int *r, int *c, int *rs, int *cs, unsigned char **s);	/* Push screen area.			*/
void PUSHSCRN(unsigned char **s);					/* Push full screen.			*/
void RETRACE(void);			/* Retrace what happened.		*/
void TRACEEND(void);			/* End a trace.				*/
void TRACEGO(void);			/* Start a trace.			*/
void VIDLINE(int *type, int *length);	/* Draw a line.				*/
void VIDMODE(int *mode);		/* Select the character rendition.	*/
void VIDMOVE(int *row, int *col);	/* Move to a position on the screen.	*/
void VIDTEXT(char *text, int *length);	/* Output text.				*/
int  ws_bar_menu( int curset, int vr, int vc, int ak, int ar, unsigned char* nm, int dp );

#endif /* ede_H */

/*
**	History:
**	$Log: ede.h,v $
**	Revision 1.3  2005/02/17 20:44:00  gsl
**	Fix MENUINFO problem on Windows.
**	
**	Revision 1.2  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.1  2003/06/27 13:59:44  gsl
**	no message
**	
**
*/