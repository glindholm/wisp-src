/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
*/


#include <string.h>

#include "idsistd.h"
#include "menu.h"
#include "wexit.h"
#include "vwang.h"
#include "wisplib.h"

#include "video.h"
#include "vlocal.h"


int WL_menu_go(char* menuname, char* menuvalue)
{
	int result, action;
	char progname[80];
	char retval[81];
	struct menu *themenu;

	strcpy(menuvalue,"");

	themenu = WL_menu_read(menuname);
	if (themenu == (struct menu *) -1)
	{
		VL_vmove(10,20);
		VL_vbell();
		VL_vprint("  Menu file %s Not found.  ",menuname); 
		VL_vmove(11,20);
		VL_vprint("              Press any key.       ");
		VL_vgetc();
		menuvalue[0] = '\0';								/* Set termination value null.	*/
		menuvalue[1] = '\0';								/* Add for COBOL "LOW VALUES".	*/
		menuvalue[2] = '\0';
 		return(-1);
	}

	vwang_init_screen();										/* Initialize the screen.	*/

	for (;;)
	{
		result = WL_menu_scan(themenu);

		if (result == -1)								/* They pressed the exit key.	*/
		{
			menuvalue[0] = '\0';							/* Set termination value null.	*/
			menuvalue[1] = '\0';							/* For COBOL "LOW VALUES".	*/
			menuvalue[2] = '\0';
			menuvalue[3] = '\0';
			free(themenu);								/* Release the menu memory.	*/
			return(0);
		}	

		action = themenu->menulist[result].action;

		switch(action)
		{
#ifdef unix			
			case 1:
			      strcpy(progname,themenu->menulist[result].filename);
				VL_vexit();
			      system(progname);

				vwang_init_screen();
				
			      break;
#endif
			case 2:
			case 3:									/* Execute a command.		*/
			{
				strcpy(menuvalue,themenu->menulist[result].message);		/* Copy the result string.	*/
				strcpy(progname,&(themenu->menulist[result].filename[0]));	/* Get the command string.	*/
				themenu->menustat = 0;						/* Menu is not on the screen.	*/
				break;
			}

			case 7:
			{									/* Just return a value .	*/
				strcpy(menuvalue,themenu->menulist[result].message);
				return(0);
			}	

			case 8:
			{									/* Display a new menu.		*/
				WL_menu_go( &(themenu->menulist[result].filename[0]) , retval);	/* Just do it again.		*/
				strcpy(menuvalue,retval);					/* We're Back!			*/
				themenu->menustat = 0;						/* Signal this as a new menu.	*/
				vwang_init_screen();							/* The screen is corrupted.	*/
				break;
			}

			case 9:									/* Terminate the program.	*/
			{
				VL_vmove(23,0);
				VL_vprint("Exiting menu system . . .");
				wexit(0L);
			}

			case 10:								/* Log the user off.		*/
			{
			}
		}										/* End of switch.		*/
		VL_vmove(23,0);									/* Clear status information	*/
		VL_vprint("                                                ");
	}											/* Repeat forever...		*/
  
}
/*
**	History:
**	$Log: menu_go.c,v $
**	Revision 1.18  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.16  2002/07/16 14:11:47  gsl
**	VL_ globals
**	
**	Revision 1.15  2002/07/15 20:16:01  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  2002/07/15 17:52:49  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/07/10 21:05:20  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  2002/07/09 04:13:59  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.11  2002/06/21 03:10:37  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.10  1996/08/19 22:32:29  gsl
**	drcs update
**	
**
**
*/
