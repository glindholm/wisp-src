			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* WFILE_DISP.C ... 	This is the front end of the Wang style DISPLAY routine.  It directs the user to enter a Wang VS style	*/
/*	      		file, library, and volume specification.  The target system filename is then constructed and the	*/
/*			vdisplay() routine is called to actually show the file.							*/


/*			Include required header files.										*/
                
#include <string.h>
#include "wperson.h"
#include <v/video.h>                                                                    /* Get Video interface definitions.	*/
#include "wfiles.h"									/* Interface with WFNAME.		*/
#include "wcommon.h"
#include "wangkeys.h"
#include "wfaccess.h"
#include "werrlog.h"

extern char WISPFILEXT[39];


/*			Subroutine entry point.											*/

wfile_disp()
{
#define		ROUTINE		77100
	char file[8], library[8], volume[6], filename[80], entry_message[80], *end_name;
	char tempname[80], orig_file[8];
	long mode;
	long native_mode;
	int  access_status;                                                                                      
	char getparm_type[3];
	char *wfname();
	char pf_key;
	int displaying = TRUE;								/* Flag set to loop until PF16 pushed.	*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	wpload();									/* Load personality stuff.		*/
	native_mode = 0L;                                           			/* Start off in WANG_VS mode.		*/

	displaying = TRUE;								/* Keep displaying.			*/
	while (displaying)								/* Loop until no more to display.	*/
	{

		displaying = FALSE;							/* Only do one display.			*/

		memset(file, ' ', 8);							/* Initialize variables goin' to	*/
		memset(orig_file, ' ', 8);						/* file_getparm().			*/
		memset(tempname, ' ', 80);
		memcpy(volume, defaults.invol, 6);
		memcpy(library, defaults.inlib, 8);

		strcpy(entry_message,"To display a file, enter the name and location of the file. ");
		getparm_type[0] = 'I';							/* Start off as an initial getparm.	*/
		getparm_type[1] = ' ';
		getparm_type[2] = '\0';
		mode = 0;
                         
get_name_loop:						      				/* A label to go to.			*/

		file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",		/* Allow the user to enter the name.	*/
			&native_mode,getparm_type,tempname,
			entry_message,0,&pf_key,'E',orig_file,NULL,NULL,NULL);
											/* If they pressed PF16 to get out of	*/
											/* get_name, these variables will all	*/
											/* be equal to spaces.			*/

		if (pf_key == PFKEY_16_PRESSED || (file[0] == ' ' && library[0] == ' ' && volume[0] == ' ' && tempname[0] == ' '))
		{
			displaying = FALSE;						/* Set to stop display loop.		*/
		}                   
		else
		{
			int i;

			if (native_mode)
			{
				memcpy(filename,tempname,80);
				*strchr(filename,' ') = '\0';				/* Null terminate.			*/
			}
			else		  						/* Are we NOT in native mode?		*/
			{								/* Yup.					*/
				mode = IS_PRINTFILE;					/* Tell wfname what type of file.	*/
				SAVE_WISPFILEXT;					/* Save the file extension.		*/
				end_name = wfname(&mode, volume, library, file, filename);	/* Construct native filename.	*/
				*end_name = '\0';					/* Null terminate.			*/
				memcpy(tempname,filename,strlen(filename));
			}
                          
			access_status = 0;						/* Initial access_status value.		*/
			mode = IS_PRINTFILE;						/* Open mode and file type.		*/
			access_status = wfaccess(filename, &mode);			/* See if we can get at the file.	*/
			if (access_status != ACC_ALLOWED)				/* Did we get it ?			*/
			{								/* Nope.  Give a new entry message.	*/
				acc_message(access_status,entry_message);
				getparm_type[0] = 'R';					/* Now its a respecify getparm.		*/
				getparm_type[1] = ' ';
				RESTORE_WISPFILEXT;					/* Restore the extension.		*/
				goto get_name_loop;					/* And loop back up.			*/
			}
			wpushscr();	      						/* Save the screen map and state.	*/
			if ((i = greclen(filename)) >= 0) vdisplay(filename,i);		/* Get the record length.		*/
			else if (i == -2) werrlog(ERRORCODE(2),filename,0,0,0,0,0,0,0);	/* Protection violation.		*/
			else if (i == -1) werrlog(ERRORCODE(4),filename,0,0,0,0,0,0,0);	/* Error on OPEN.			*/
			else vdisplay(filename,255);  					/* Display the file using max buffersize.*/
			wpopscr();							/* Restore the screen and map.		*/
		}
	}

	return(SUCCESS); 	      							/* All done mate.			*/
}
