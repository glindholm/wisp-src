static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef MSDOS
#include <io.h>
#endif

#include "idsistd.h"
#include "menu.h"
#include "paths.h"
#include "wispcfg.h"

/* This procedure locates a menu file and loads it into a menu structure to be used by the menu system to display the menu	*/
/* If the menufile name has a hex -1 as its first int, it is assumed to actually be a structure.				*/

struct menu *menu_read(char* pmenufile)
{
	int i,j;
	char *fstat;                                                            
	char temp[132];
	FILE *infile;
	struct menu *themenu;
	struct menu *menufile;
	char *menuname;
	
	menuname=(char*)pmenufile;
	menufile=(struct menu *)pmenufile;
	
	if (menufile->curritem == -1)						/* Is it really a structure?		*/
	{
		themenu = (struct menu *)malloc(sizeof(struct menu));			/* get a menu structure ready 		*/
		memcpy((char *)themenu,menufile,sizeof(struct menu));		/* copy the passed structure into it	*/
		themenu->menustat = 0;							/* And the menu hasn't been drawn yet	*/
		i = 0;
		do
		{
			if ((themenu->menulist[i].action) && (themenu->menulist[i].action != DISPLAY_SUBTITLE))
			{								/* found an available item		*/
				themenu->curritem = i;					/* make it the current item		*/
				return(themenu);					/* get out of here			*/
			}
			i++;
		} while (i<30);

		return(themenu);  							/* end of menu file, return pointer	*/
	}
	if (menuname[0] != 0)    							/* a filename was passed to us 		*/
	{
		infile = fopen(menuname,"r");						/* try to read it			*/
		if (!infile)
		{									/* If its not found, look for it	*/
#ifdef VMS
			strcpy(temp,"APP$MENU:");					/* Start with the application directory	*/
			strcat(temp,menuname);						/* Then add the file name.		*/
			infile = fopen(temp,"r");					/* Try to read it. 			*/
#else	/* VMS */
			strcpy(temp,wispmenudir());
			strcat(temp,DIR_SEPARATOR_STR);
			strcat(temp,menuname);
			infile = fopen(temp,"r");
#endif
			if (!infile) return((struct menu *) -1);			/* Not there either, return a -1 	*/
		}
	}
	else
	{
							/*	no filename was passed to us, so let's go through the list of   */
							/*	default menus until we find one (should at least find sys$menu) */
		infile = fopen("default.menu","r");
		if (!infile)   infile = fopen("sys$login:default.menu","r");
		if (!infile)   infile = fopen("user$menu","r");
		if (!infile)   infile = fopen("group$menu","r");
		if (!infile)   infile = fopen("sys$menu","r");
		if (!infile)    return((struct menu *) -1);				/* no luck, weird system so return -1 	*/
	}

	themenu = (struct menu *)malloc(sizeof(struct menu));				/* get a menu structure ready 		*/

	for (i=0; i<30; i++)								/* init all the menu items to be empty	*/
	{
		themenu->menulist[i].action = 0;					/* no action item			*/
		themenu->menulist[i].title[0] = 0;					/* the title of the menu item		*/
		themenu->menulist[i].filename[0] = 0;					/* The associated filename		*/
		themenu->menulist[i].message[0] = 0;					/* message to return to caller		*/
	}

	themenu->curritem = -1;								/* can't be any default item to select 	*/
	themenu->menustat = 0;								/* And the menu hasn't been drawn yet	*/
	fgets( &(themenu->menutitle[0]),80,infile);  					/* read in the menu title 		*/
	j = strlen( &(themenu->menutitle[0]))-1;
	if (themenu->menutitle[j] == 10) themenu->menutitle[j] = 0;			/* get rid of the newline		*/

	for (;;)
	{
		fstat = fgets(temp,80,infile);						/* read the lines from the file		*/
		sscanf(temp,"%d",&i);     						/* read in the menu item number 	*/

		if (i == -1 || fstat == 0 )						/* all done?				*/
		{
			fclose(infile);
			return(themenu);  						/* end of menu file, return pointer	*/
		}

		fgets(temp,80,infile);   
		sscanf(temp,"%d",&(themenu->menulist[i].action)); 			/* item action code 			*/
											/* first encountered item is the default*/
		if ((themenu->curritem == -1) && (themenu->menulist[i].action != DISPLAY_SUBTITLE)) themenu->curritem = i;

		fgets( &(themenu->menulist[i].title[0]),80,infile);  			/* item title 				*/
		fgets( &(themenu->menulist[i].filename[0]),80,infile);			/* read item filename 			*/
		fgets( &(themenu->menulist[i].message[0]),80,infile);			/* read item return message 		*/  

		j = strlen( &(themenu->menulist[i].title[0]))-1;
		if (themenu->menulist[i].title[j] == 10) themenu->menulist[i].title[j] = 0;

		j = strlen( &(themenu->menulist[i].filename[0]))-1;
		if (themenu->menulist[i].filename[j] == 10) themenu->menulist[i].filename[j] = 0;

		j = strlen( &(themenu->menulist[i].message[0]))-1;
		if (themenu->menulist[i].message[j] == 10) themenu->menulist[i].message[j] = 0;
	}
}
/*
**	History:
**	$Log: menuread.c,v $
**	Revision 1.11  1996/10/09 00:22:05  gsl
**	Replace getenv() with wispmenudir()
**	
**	Revision 1.10  1996-08-19 15:32:30-07  gsl
**	drcs update
**
**
**
*/
