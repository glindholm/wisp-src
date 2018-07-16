static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <string.h>

#include "idsistd.h"
#include "menu.h"
#include "wexit.h"
#include "vwang.h"
#include "wisplib.h"

#include "video.h"
#include "vlocal.h"

#ifdef VMS
#include <descrip.h>
#endif

struct menu *menu_read();

int menu_go(char* menuname, char* menuvalue)
{
	static char command[200];
	int result, action;
	char progname[80];
	char retval[81];
	struct menu *themenu;
	uint4 vms_status;

#ifdef VMS
	static $DESCRIPTOR(icom,command);							/* For DCL commands.		*/
#endif
	strcpy(menuvalue,"");

	themenu = menu_read(menuname);
	if (themenu == (struct menu *) -1)
	{
		vmove(10,20);
		vbell();
		vprint("  Menu file %s Not found.  ",menuname); 
		vmove(11,20);
		vprint("              Press any key.       ");
		vgetc();
		menuvalue[0] = '\0';								/* Set termination value null.	*/
		menuvalue[1] = '\0';								/* Add for COBOL "LOW VALUES".	*/
		menuvalue[2] = '\0';
 		return(-1);
	}

	init_screen();										/* Initialize the screen.	*/

	for (;;)
	{
		result = menu_scan(themenu);

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
				vexit();
			      system(progname);

				init_screen();
				
			      break;
#endif
			case 2:
			case 3:									/* Execute a command.		*/
			{
				strcpy(menuvalue,themenu->menulist[result].message);		/* Copy the result string.	*/
				strcpy(progname,&(themenu->menulist[result].filename[0]));	/* Get the command string.	*/
				spawn2 (action,progname,"Please wait...",&vms_status);		/* Spawn to do the command.	*/
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
				menu_go( &(themenu->menulist[result].filename[0]) , retval);	/* Just do it again.		*/
				strcpy(menuvalue,retval);					/* We're Back!			*/
				themenu->menustat = 0;						/* Signal this as a new menu.	*/
				init_screen();							/* The screen is corrupted.	*/
				break;
			}

			case 9:									/* Terminate the program.	*/
			{
				vmove(23,0);
				vprint("Exiting menu system . . .");
				wexit(0L);
			}

			case 10:								/* Log the user off.		*/
			{
#ifdef VMS
				strcpy(command,"logoff");					/* VMS VERSION			*/
				vmove(23,0);
				vprint("\n");
				lib$do_command(&icom);
#endif
			}
		}										/* End of switch.		*/
		vmove(23,0);									/* Clear status information	*/
		vprint("                                                ");
	}											/* Repeat forever...		*/
  
}
/*
**	History:
**	$Log: menu_go.c,v $
**	Revision 1.10  1996-08-19 18:32:29-04  gsl
**	drcs update
**
**
**
*/
