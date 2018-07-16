

						/* menu structures header file */

struct menuitem	{						/* The structure of an individual menu item		*/
			int    action;				/* Action code						*/
			char   title[81];			/* Title description of the item			*/
			char   filename[81];			/* The filename or command syntax for the item		*/
			char   message[81];			/* A message string to return to the menu caller	*/
		};

struct menu	{						/* The structure of a complete menu			*/
			int    curritem;			/* The Currently highlited item				*/
			int    menustat;			/* The status of this menu 0=new, 1=currently displayed	*/
			char   menutitle[81];			/* The title of the menu				*/
			struct menuitem menulist[30];		/* The list of menu items in the menu			*/
		};


									/* These are the action item codes			*/

#define	RUN_PROGRAM			1				/* Run a program					*/
#define EXECUTE_AND_PAUSE		2				/* Execute a command, wait for a keypress		*/
#define	EXECUTE_COMMAND			3				/* Execute a command					*/
#define DISPLAY_SUBTITLE		4				/* Display a subtitle					*/
#define	RETURN_A_VALUE			7				/* Only return a menu value				*/
#define DISPLAY_MENU			8				/* Display a new menu					*/
#define EXIT_PROGRAM			9				/* Exit the program					*/
#define LOGOFF				10				/* Log off the system					*/

