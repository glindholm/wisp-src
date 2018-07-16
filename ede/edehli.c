/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

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
int vwang_init_screen(void);


/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

int EDE_using(void)	/* Is EDE being used */
{
	return 1;
}

/*				High Level Interface for the Extended Development Library					*/

void EDLOAD(void)					/* Enable the EDE environment.		*/
{
#ifdef OSF1_ALPHA
#include <sys/sysinfo.h>
#include <sys/proc.h>
	int buf[2];

	buf[0]=SSIN_UACPROC;
	buf[1]=UAC_SIGBUS|UAC_NOPRINT;
	
	setsysinfo(SSI_NVPAIRS,buf,1,0, 0);
#endif

	vwang_init_screen(); 
	ede_synch = TRUE;
}							

void EDEXIT(void) 					/* Disable the EDE environment.		*/
{
	ede_synch = FALSE; 
	VL_vexit();
}

void EDCLRSCR(void) 					/* Erase the full screen.		*/
{
	VL_verase(FULL_SCREEN);
}
void EDDRKSCR(void) 					/* Make the screen background dark.	*/
{
	VL_verase(FULL_SCREEN); 
	VL_vscreen(DARK);
}
void EDLTESCR(void) 					/* Make the screen background light.	*/
{
	VL_verase(FULL_SCREEN); 
	VL_vscreen(LIGHT);
}
void EDWIDSCR(void) 					/* Make the screen wide.		*/
{
	VL_verase(FULL_SCREEN); 
	VL_vscreen(WIDE);
}
void EDNARSCR(void) 					/* Make the screen narrow.		*/
{
	VL_verase(FULL_SCREEN); 
	VL_vscreen(NARROW);
}

void TRACEGO(void)  							/* Start a trace.			*/
{
	VL_vmacro(START_SAVE); 
}
void TRACEEND(void) 							/* End a trace.				*/
{
	VL_vmacro(END_SAVE);   
}
void RETRACE(void)  							/* Retrace what happened.		*/
{
	VL_vmacro(START_RESTORE); 
}

void PFKEYSON(void) 							/* Turn on the PF keys.			*/
{
	VL_vmenu_pfkeys(ON); 
}
void NOPFKEYS(void) 							/* No PF keys.				*/
{
	VL_vmenu_pfkeys(OFF); 
}

void MENUSAVE(struct video_menu *md)							/* Save the current menu choice path.	*/
{
	VL_vmenusave(md);								/* Save it.				*/
}

void MENUREST(void)									/* Start restoration.			*/
{
	VL_vmenurestore(); 
}

void MENULOAD(struct video_menu *md, int *t, int *o, int *r, int *c, int *w)		/* Get the menu data.			*/
{
	VL_vmenuinit(md, *t, *o, *r, *c, *w);						/* Do the call.				*/
}

void MENUITEM(struct video_menu *md, char *t, int4 *v, void *p)				/* Get the item data.			*/
{
	if (*v == STATIC_MENU) VL_vmenuitem(md, t, 0, (struct video_menu *)p);		/* Initialize linking.			*/
	else if (*v == DYNAMIC_MENU) VL_vmenuitem(md, t, *((int4 *)p), DYNAMIC_LINK);	/* Initialize dynamically.		*/
	else VL_vmenuitem(md, t, *v, NULL);						/* Initialize the value only.		*/
}

void MENUGO(struct video_menu *md, int4 *v)						/* Run the menu.			*/
{
	*v = VL_vmenugo(md);								/* Call the menu manager.		*/
}

void MENUCONT(struct video_menu *md, int4 *v)						/* Continue the menu.			*/
{
	*v = VL_vmenucont(md);								/* Call the menu manager.		*/
}

void DYLINK(struct video_menu *base, struct video_menu *dyna)				/* Link for dynamic operation.		*/
{
	VL_vdynalink(base,dyna);							/* Link them together.			*/
}

void DYUNLINK(struct video_menu *md)							/* Unlink a menu structure.		*/
{
	VL_vdynaunlink(md);								/* Unlink...				*/
}

void MENUKILL(struct video_menu *md, int4 *how)						/* Erase menus.				*/
{
	VL_verase_menu(*how,md);							/* Erase the menus as requested.	*/
}

void MENUEXIT(struct video_menu *md)							/* Erase all menus.			*/
{
	VL_verase_menu(ALL_MENUS, md);							/* Get em all.				*/
}

void MENUMODE(int *mode)								/* Menu mode.				*/
{
	VL_vmenumode(*mode);								/* Select the menu mode.		*/
}

void VIDMODE(int *mode)									/* Select the character rendition.	*/
{
	VL_vmode(*mode);								/* Select the mode.			*/
}

void VIDMOVE(int *row, int *col)							/* Move to a position on the screen.	*/
{
	VL_vmove((*row)-1, (*col)-1 );		       					/* Do the move.				*/
}

void VIDLINE(int *type, int *length)							/* Draw a line.				*/
{
	VL_vline(*type, *length);							/* Do the draw.				*/
}

void VIDTEXT(char *text, int *length)							/* Output text.				*/
{
	char string[65];								/* Working string.			*/
	register int i;									/* Working register.			*/
	int len;

	if (*length > 64)
	{
		VL_vre("VIDTEXT-E-INVALID Invalid string length [%d].  Max is 64.", *length);
		return;
	}
	for (i = 0; i < 64; i++) string[i] = text[i];					/* Copy the string.			*/
	string[64] = CHAR_NULL;								/* Terminate the string.		*/
	VL_vtrim(string);									/* Now trim up the string.		*/
	len = strlen(string);								/* Get the length of the string.	*/
	for (i = len; i <= *length; i++) string[i] = ' ';				/* Pad str with spaces to given length.	*/
	string[i] = CHAR_NULL;								/* Force to display given length.	*/
	VL_vprint("%s", string);							/* Print it.				*/
}

void PUSHSCRN(unsigned char **s)							/* Push full screen.			*/
{
	*s = VL_vsss(0,0,MAX_LINES_PER_SCREEN,VL_vscr_wid);					/* Save the full screen.		*/
	ede_synch = TRUE;								/* Low level synch now required.	*/
}

void PUSHAREA(int *r, int *c, int *rs, int *cs, unsigned char **s)			/* Push screen area.			*/
{
	*s = VL_vsss((*r)-1, (*c)-1, *rs, *cs);						/* Save the screen segment.		*/
	ede_synch = TRUE;								/* Low level synch now required.	*/
}

void POPAREA(unsigned char **s)								/* Restore the screen area.		*/
{
	VL_vrss(*s);									/* Restore the screen area.		*/
	VL_synch_required = TRUE;							/* Force a synchronization.		*/
}

/*
** On Windows WinUser.h defines MENUINFO as a typedef.
** WinUser.h is not include here but is referenced in building the ACU62 runtime.
** 
*/
void EDE_MENUINFO(struct video_menu *mcb, int4 *level, int4 *item, int4 *link)		/* Return the menu level and item.	*/
{
	*level = VL_vlastlevel(mcb) + 1;						/* Get the current level.		*/
	*item = VL_vlastitem(mcb) + 1;							/* Get the current item.		*/
        *link = VL_vlastlink(mcb);							/* Get the current link.		*/
}
void MENUINFO(struct video_menu *mcb, int4 *level, int4 *item, int4 *link)		/* Return the menu level and item.	*/
{
	EDE_MENUINFO(mcb,level,item,link);
}


/*
**	History:
**	$Log: edehli.c,v $
**	Revision 1.18  2005/02/17 20:44:00  gsl
**	Fix MENUINFO problem on Windows.
**	
**	Revision 1.17  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.16  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.15  2003/02/04 18:57:01  gsl
**	fix copyright header
**	
**	Revision 1.14  2002/07/16 14:11:51  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/16 13:40:24  gsl
**	VL_ globals
**	
**	Revision 1.12  2002/07/15 20:16:04  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  2002/07/15 17:52:53  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  2002/07/12 20:40:38  gsl
**	Global unique WL_ changes
**	
**	Revision 1.9  2002/07/09 04:14:07  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.8  1996/09/13 17:53:50  gsl
**	Add missing includes
**	
**	Revision 1.7  1996-03-28 14:50:03-08  gsl
**	Removed all the (cast *)'ing.
**	Fix all the prototypes
**	Include all the video header files with the function prototypes
**	All this was to fix a SIG 11 on the Alpha in PUSHSCREEN() on the
**	call to VL_vsss(), an 8 byte pointer was compressed into an int (4 bytes).
**
**
**
*/
