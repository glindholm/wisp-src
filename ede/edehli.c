static char copyright[]="Copyright (c) 1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		edehli.c
**
**	Project:	WISP/EDE
**
**	RCS:		$Source:$
**
**	Purpose:	EDE HLI interface
**
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "video.h"
#include "vintdef.h"
#include "vmenu.h"
#include "vlocal.h"
#include "vdata.h"
#include "vutil.h"
#include "vline.h"
#include "verase.h"
#include "vprint.h"
#include "vscreen.h"
#include "vmove.h"
#include "vtrim.h"

#include "idsistd.h"
#include "wglobals.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
int vmode(int control);
int vmacro(int action);
void vexit(void);
int init_screen(void);


/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

/*				High Level Interface for the Extended Development Library					*/

void EDLOAD(void) 
{
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

void EDEXIT(void) 									/* Disable the EDE environment.		*/
{
	ede_synch = FALSE; 
	vexit();
}

void EDCLRSCR(void) 									/* Erase the full screen.		*/
{
	verase(FULL_SCREEN);
}
void EDDRKSCR(void) 					/* Make the screen background dark.	*/
{
	verase(FULL_SCREEN); 
	vscreen(DARK);
}
void EDLTESCR(void) 					/* Make the screen background light.	*/
{
	verase(FULL_SCREEN); 
	vscreen(LIGHT);
}
void EDWIDSCR(void) 					/* Make the screen wide.		*/
{
	verase(FULL_SCREEN); 
	vscreen(WIDE);
}
void EDNARSCR(void) 					/* Make the screen narrow.		*/
{
	verase(FULL_SCREEN); 
	vscreen(NARROW);
}

void TRACEGO(void)  							/* Start a trace.			*/
{
	vmacro(START_SAVE); 
}
void TRACEEND(void) 							/* End a trace.				*/
{
	vmacro(END_SAVE);   
}
void RETRACE(void)  							/* Retrace what happened.		*/
{
	vmacro(START_RESTORE); 
}

void PFKEYSON(void) 								/* Turn on the PF keys.			*/
{
	vmenu_pfkeys(ON); 
}
void NOPFKEYS(void) 							/* No PF keys.				*/
{
	vmenu_pfkeys(OFF); 
}

void MENUSAVE(struct video_menu *md)							/* Save the current menu choice path.	*/
{
	vmenusave(md);									/* Save it.				*/
}

void MENUREST(void)									/* Start restoration.			*/
{
	vmenurestore(); 
}

void MENULOAD(struct video_menu *md, int4 *t, int4 *o, int4 *r, int4 *c, int4 *w)	/* Get the menu data.			*/
{
	vmenuinit(md, *t, *o, *r, *c, *w);						/* Do the call.				*/
}

void MENUITEM(struct video_menu *md, char *t, int4 *v, void *p)				/* Get the item data.			*/
{
	if (*v == STATIC_MENU) vmenuitem(md, t, 0, (struct video_menu *)p);		/* Initialize linking.			*/
	else if (*v == DYNAMIC_MENU) vmenuitem(md, t, *((int4 *)p), DYNAMIC_LINK);	/* Initialize dynamically.		*/
	else vmenuitem(md, t, *v, NULL);						/* Initialize the value only.		*/
}

void MENUGO(struct video_menu *md, int4 *v)						/* Run the menu.			*/
{
	*v = vmenugo(md);								/* Call the menu manager.		*/
}

void MENUCONT(struct video_menu *md, int4 *v)						/* Continue the menu.			*/
{
	*v = vmenucont(md);								/* Call the menu manager.		*/
}

void DYLINK(struct video_menu *base, struct video_menu *dyna)				/* Link for dynamic operation.		*/
{
	vdynalink(base,dyna);								/* Link them together.			*/
}

void DYUNLINK(struct video_menu *md)							/* Unlink a menu structure.		*/
{
	vdynaunlink(md);								/* Unlink...				*/
}

void MENUKILL(struct video_menu *md, int4 *how)						/* Erase menus.				*/
{
	verase_menu(*how,md);								/* Erase the menus as requested.	*/
}

void MENUEXIT(struct video_menu *md)							/* Erase all menus.			*/
{
	verase_menu(ALL_MENUS, md);							/* Get em all.				*/
}

void MENUMODE(int4 *mode)								/* Menu mode.				*/
{
	vmenumode(*mode);								/* Select the menu mode.		*/
}

void VIDMODE(int4 *mode)								/* Select the character rendition.	*/
{
	vmode(*mode);									/* Select the mode.			*/
}

void VIDMOVE(int4 *row, int4 *col)							/* Move to a position on the screen.	*/
{
	vmove((*row)-1, (*col)-1 );		       					/* Do the move.				*/
}

void VIDLINE(int4 *type, int4 *length)							/* Draw a line.				*/
{
	vline(*type, *length);								/* Do the draw.				*/
}

void VIDTEXT(char *text, int4 *length)							/* Output text.				*/
{
	char string[65];								/* Working string.			*/
	register int i;									/* Working register.			*/
	int len;

	if (*length > 64)
	{
		vre("VIDTEXT-E-INVALID Invalid string length [%d].  Max is 64.", *length);
		return;
	}
	for (i = 0; i < 64; i++) string[i] = text[i];					/* Copy the string.			*/
	string[64] = CHAR_NULL;								/* Terminate the string.		*/
	vtrim(string);									/* Now trim up the string.		*/
	len = strlen(string);								/* Get the length of the string.	*/
	for (i = len; i <= *length; i++) string[i] = ' ';				/* Pad str with spaces to given length.	*/
	string[i] = CHAR_NULL;								/* Force to display given length.	*/
	vprint("%s", string);								/* Print it.				*/
}

void PUSHSCRN(unsigned char **s)							/* Push full screen.			*/
{
	*s = vsss(0,0,MAX_LINES_PER_SCREEN,vscr_wid);					/* Save the full screen.		*/
	ede_synch = TRUE;								/* Low level synch now required.	*/
}

void PUSHAREA(int4 *r, int4 *c, int4 *rs, int4 *cs, unsigned char **s)			/* Push screen area.			*/
{
	*s = vsss((*r)-1, (*c)-1, *rs, *cs);						/* Save the screen segment.		*/
	ede_synch = TRUE;								/* Low level synch now required.	*/
}

void POPAREA(unsigned char **s)								/* Restore the screen area.		*/
{
	vrss(*s);									/* Restore the screen area.		*/
	synch_required = TRUE;								/* Force a synchronization.		*/
}

void MENUINFO(struct video_menu *mcb, int4 *level, int4 *item, int4 *link)		/* Return the menu level and item.	*/
{
	*level = vlastlevel(mcb) + 1;							/* Get the current level.		*/
	*item = vlastitem(mcb) + 1;							/* Get the current item.		*/
        *link = vlastlink(mcb);								/* Get the current link.		*/
}

/*
**	History:
**	$Log: edehli.c,v $
**	Revision 1.8  1996-09-13 13:53:50-04  gsl
**	Add missing includes
**
**	Revision 1.7  1996-03-28 14:50:03-08  gsl
**	Removed all the (cast *)'ing.
**	Fix all the prototypes
**	Include all the video header files with the function prototypes
**	All this was to fix a SIG 11 on the Alpha in PUSHSCREEN() on the
**	call to vsss(), an 8 byte pointer was compressed into an int (4 bytes).
**
**
**
*/
