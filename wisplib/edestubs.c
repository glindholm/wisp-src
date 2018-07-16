static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*					Stubs to make EDE modules defined in regular WISPLIB					*/

#include "idsistd.h"
#include "vwang.h"
#include "werrlog.h"
#include "wexit.h"

static int edemissing();

int  ws_bar_menu( curset, vr, vc, ak, ar, nm, dp )					/* Stub so doesn't try to put up a bar	*/
int  curset, vr, vc, ak, ar, dp;
unsigned	char	*nm;
{											/*  menu.				*/
	ws_help(curset);								/* Just do regular WISP help.		*/
	return(1);									/* Return success.			*/
}

void A_WSLINK(ets_link_program_definition, on_line_doc_passing_values)
unsigned char *ets_link_program_definition;
unsigned char *on_line_doc_passing_values;
{
	return;
}

int  gen_ncpfkey( type, wsb, num_chars, st_win, end_win)				/* Stub so doesn't try to generate a   	*/
int  type;
char  **wsb;
int  num_chars, *st_win, *end_win;
{											/*  pop-up PFkey menu window.		*/
	return(0);									/* Just do regular vwang display.	*/
}

int  nc_pop_menu( filling, terminate_list, no_mod, pfkey )				/* Stub so doesn't try to pop-up window	*/
int  *filling;
unsigned char  *terminate_list, *no_mod, *pfkey;
{
	ws_bad_char();									/* Ring bell invalid trigger.		*/
	return(0);									/* Return FAILURE.			*/
}

/*
	EDE COBOL stubs
*/


void EDLOAD() 	{ edemissing(1); }							/* Enable the EDE environment.		*/
void EDEXIT() 	{ edemissing(1); }							/* Disable the EDE environment.		*/

void EDCLRSCR() 	{ edemissing(1); }							/* Erase the full screen.		*/
void EDDRKSCR() 	{ edemissing(1); }							/* Make the screen background dark.	*/
void EDLTESCR() 	{ edemissing(1); }							/* Make the screen background light.	*/
void EDWIDSCR() 	{ edemissing(1); }							/* Make the screen wide.		*/
void EDNARSCR() 	{ edemissing(1); }							/* Make the screen narrow.		*/

void TRACEGO()  	{ edemissing(1); }							/* Start a trace.			*/
void TRACEEND() 	{ edemissing(1); }							/* End a trace.				*/
void RETRACE()  	{ edemissing(1); }							/* Retrace what happened.		*/

void PFKEYSON() 	{ edemissing(1); }							/* Turn on the PF keys.			*/
void NOPFKEYS() 	{ edemissing(1); }							/* No PF keys.				*/

typedef	int4			*PL;							/* Pointer to Long.			*/
typedef	char			*PC;							/* Pointer to Char.			*/
typedef	unsigned char		*PUC, **HUC;						/* Pointer and Handle to Unsigned Char.	*/
typedef struct	video_menu	*PSV;							/* Pointer to Structure Video_menu.	*/

void MENUSAVE(PSV md)			{ edemissing(1); }			/* Save the current menu choice path.	*/
void MENUREST()				{ edemissing(1); }			/* Start restoration.			*/
void MENULOAD(md,t,o,r,c,w) 	PSV md; PL t,o,r,c,w; 	{ edemissing(1); }			/* Get the menu data.			*/
void MENUITEM(md,t,v,p) 	PSV md; PUC t; PL v,p;	{ edemissing(1); }			/* Get the item data.			*/
void MENUGO(md,v) 		PSV md; PL v;		{ edemissing(1); }			/* Run the menu.			*/
void MENUCONT(md,v) 		PSV md; PL v;		{ edemissing(1); }			/* Continue the menu.			*/

void DYLINK(base,dyna)	PSV base,dyna;	{ edemissing(1); }				/* Link for dynamic operation.		*/
void DYUNLINK(md)		PSV md;		{ edemissing(1); }				/* Unlink a menu structure.		*/

void MENUKILL(md,how)	PSV md; PL how;	{ edemissing(1); }				/* Erase menus.				*/
void MENUEXIT(md)		PSV md;		{ edemissing(1); }				/* Erase all menus.			*/
void MENUMODE(mode)		PL mode;	{ edemissing(1); }				/* Menu mode.				*/

void VIDMODE(mode)		PL mode;		{ edemissing(1); }			/* Select a character mode		*/
void VIDMOVE(row,col)	PL row,col;		{ edemissing(1); }			/* Move to a position on the screen.	*/
void VIDLINE(type,length)	PL type,length;		{ edemissing(1); }			/* Draw a line.				*/
void VIDTEXT(text,length)	PC text; PL length;	{ edemissing(1); }			/* Output text.				*/

void PUSHSCRN(s)		HUC s;			{ edemissing(1); }			/* Push screen.				*/
void PUSHAREA(r,c,rs,cs,s)	PL r,c,rs,cs; HUC s;	{ edemissing(1); }			/* Push screen area.			*/
void POPAREA(s)		HUC s;			{ edemissing(1); }			/* Pop screen area.			*/

void MENUINFO(mcb,level,item,link)	PSV mcb; PL level,item,link;	{ edemissing(1); }	/* Give menu information.		*/

void GENVEC(program,inparms,outparms) PC program,inparms,outparms;	{ edemissing(1); }	/* General vectoring routine		*/

static int edemissing(doexit) int doexit;
{
#ifdef VMS
	werr_message_box("%EDE-E-MISSING: EDE is not installed [Remove EDESTUBS from WISPLIB]");
#else
	werr_message_box("%EDE-E-MISSING: EDE is not installed");
#endif
	if (doexit) wexit(0L);

	return 0;
}

/*
**	History:
**	$Log: edestubs.c,v $
**	Revision 1.12  1996/08/19 22:32:17  gsl
**	drcs update
**	
**
**
*/
