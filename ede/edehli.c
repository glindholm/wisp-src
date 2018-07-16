			/************************************************************************/
			/*		  WISP - Wang Interchange Source Processor		*/
			/*		    Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <stdio.h>
#include <v/video.h>
#include <v/vintdef.h>
#include <v/vmenu.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include "idsistd.h"
#include "wglobals.h"

/*				High Level Interface for the Extended Development Library					*/

EDLOAD() {

#ifdef OSF1_ALPHA
#include <sys/sysinfo.h>
#include <sys/proc.h>
	int buf[2];

	buf[0]=SSIN_UACPROC;
	buf[1]=UAC_SIGBUS|UAC_NOPRINT;
	
	setsysinfo(SSI_NVPAIRS,buf,1,0, 0);
#endif



	init_screen(); 
	ede_synch = TRUE;
}											/* Enable the EDE environment.		*/
EDEXIT() {ede_synch = FALSE; vexit();}							/* Disable the EDE environment.		*/

EDCLRSCR() {verase(FULL_SCREEN);}							/* Erase the full screen.		*/
EDDRKSCR() {verase(FULL_SCREEN); vscreen(DARK);}					/* Make the screen background dark.	*/
EDLTESCR() {verase(FULL_SCREEN); vscreen(LIGHT);}					/* Make the screen background light.	*/
EDWIDSCR() {verase(FULL_SCREEN); vscreen(WIDE);}					/* Make the screen wide.		*/
EDNARSCR() {verase(FULL_SCREEN); vscreen(NARROW);}					/* Make the screen narrow.		*/

TRACEGO()  {vmacro(START_SAVE); }							/* Start a trace.			*/
TRACEEND() {vmacro(END_SAVE);   }							/* End a trace.				*/
RETRACE()  {vmacro(START_RESTORE); }							/* Retrace what happened.		*/

PFKEYSON() {vmenu_pfkeys(ON); }								/* Turn on the PF keys.			*/
NOPFKEYS() {vmenu_pfkeys(OFF); }							/* No PF keys.				*/

MENUSAVE(md) struct video_menu *md;							/* Save the current menu choice path.	*/
{
	vmenusave(md);									/* Save it.				*/
}

MENUREST() {vmenurestore(); }								/* Start restoration.			*/

MENULOAD(md, t, o, r, c, w) struct video_menu *md; int4 *t, *o, *r, *c, *w;		/* Get the menu data.			*/
{
	vmenuinit(md, (int)*t, (int)*o, (int)*r, (int)*c, (int)*w);			/* Do the call.				*/
}

MENUITEM(md, t, v, p) struct video_menu *md; unsigned char *t; int4 *v, *p;		/* Get the item data.			*/
{
	if (*v == STATIC_MENU) vmenuitem(md, t, 0, (struct video_menu *)p);		/* Initialize linking.			*/
	else if (*v == DYNAMIC_MENU) vmenuitem(md, t, (int)*p, DYNAMIC_LINK);		/* Initialize dynamically.		*/
	else vmenuitem(md, t, (int)*v, NULL);						/* Initialize the value only.		*/
}

MENUGO(md, v) struct video_menu *md; int4 *v;						/* Run the menu.			*/
{
	*v = (int4)vmenugo(md);								/* Call the menu manager.		*/
}

MENUCONT(md, v) struct video_menu *md; int4 *v;						/* Continue the menu.			*/
{
	*v = (int4)vmenucont(md);							/* Call the menu manager.		*/
}

DYLINK(base, dyna) struct video_menu *base, *dyna;					/* Link for dynamic operation.		*/
{
	vdynalink(base,dyna);								/* Link them together.			*/
}

DYUNLINK(md) struct video_menu *md;							/* Unlink a menu structure.		*/
{
	vdynaunlink(md);								/* Unlink...				*/
}

MENUKILL(md, how) struct video_menu *md; int4 *how;					/* Erase menus.				*/
{
	verase_menu((int)*how,md);							/* Erase the menus as requested.	*/
}

MENUEXIT(md) struct video_menu *md;							/* Erase all menus.			*/
{
	verase_menu(ALL_MENUS, md);							/* Get em all.				*/
}

MENUMODE(mode) int4 *mode;								/* Menu mode.				*/
{
	vmenumode((int)*mode);								/* Select the menu mode.		*/
}

VIDMODE(mode) int4 *mode;								/* Select the character rendition.	*/
{
	vmode((int)*mode);								/* Select the mode.			*/
}

VIDMOVE(row, col) int4 *row, *col;							/* Move to a position on the screen.	*/
{
	vmove((int)(*row)-1, (int)(*col)-1 );						/* Do the move.				*/
}

VIDLINE(type, length) int4 *type, *length;						/* Draw a line.				*/
{
	vline((int)*type, (int)*length);						/* Do the draw.				*/
}

VIDTEXT(text, length) char *text; int4 *length;						/* Output text.				*/
{
	char string[65];								/* Working string.			*/
	register int i;									/* Working register.			*/
	int len;

	if (*length > 64)
	{
		vre("VIDTEXT-E-INVALID Invalid string length [%d].  Max is 64.",(int4)*length);
		return(0);
	}
	for (i = 0; i < 64; i++) string[i] = text[i];					/* Copy the string.			*/
	string[64] = CHAR_NULL;								/* Terminate the string.		*/
	vtrim(string);									/* Now trim up the string.		*/
	len = strlen(string);								/* Get the length of the string.	*/
	for (i = len; i <= *length; i++) string[i] = ' ';				/* Pad str with spaces to given length.	*/
	string[i] = CHAR_NULL;								/* Force to display given length.	*/
	vprint("%s", string);								/* Print it.				*/
}

PUSHSCRN(s) unsigned char **s;								/* Push full screen.			*/
{
	*s = (unsigned char *)vsss(0,0,MAX_LINES_PER_SCREEN,vscr_wid);			/* Save the full screen.		*/
	ede_synch = TRUE;								/* Low level synch now required.	*/
}

PUSHAREA(r, c, rs, cs, s) int4 *r, *c, *rs, *cs; unsigned char **s;			/* Push screen area.			*/
{
	*s = (unsigned char *)vsss((int)(*r)-1,(int)(*c)-1,(int)*rs,(int)*cs);		/* Save the screen segment.		*/
	ede_synch = TRUE;								/* Low level synch now required.	*/
}

POPAREA(s) unsigned char **s;								/* Restore the screen area.		*/
{
	vrss(*s);									/* Restore the screen area.		*/
	synch_required = TRUE;								/* Force a synchronization.		*/
}

MENUINFO(mcb,level,item,link) struct video_menu *mcb; int4 *level, *item, *link;	/* Return the menu level and item.	*/
{
	*level = (int4)vlastlevel(mcb) + 1;						/* Get the current level.		*/
	*item = (int4)vlastitem(mcb) + 1;						/* Get the current item.		*/
        *link = (int4)vlastlink(mcb);							/* Get the current link.		*/
}

