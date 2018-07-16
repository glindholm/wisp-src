			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* WFILE_DISP.C ... 	This is the front end of the Wang style DISPLAY routine.  It directs the user to enter a Wang VS style	*/
/*	      		file, library, and volume specification.  The target system filename is then constructed and the	*/
/*			vdisplay() routine is called to actually show the file.							*/


/*			Include required header files.										*/
                
#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "wperson.h"
#include <v/video.h>                                                                    /* Get Video interface definitions.	*/
#include "wfiles.h"									/* Interface with WFNAME.		*/
#include "wcommon.h"
#include "wangkeys.h"
#include "werrlog.h"

extern char WISPFILEXT[39];


/*			Subroutine entry point.											*/

wfile_disp()
{
#define		ROUTINE		77100
	char file[8], library[8], volume[6], filename[80], entry_message[80], *end_name;
	char tempname[80], orig_file[8];
	int4 mode;
	int4 native_mode;
	char getparm_type[3];
	char *wfname();
	char pf_key;
	FILE *fh;
	int displaying = TRUE;								/* Flag set to loop until PF16 pushed.	*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	newlevel();									/* Increment the link-level		*/

	wpload();									/* Load personality stuff.		*/
	native_mode = 0L;                                           			/* Start off in WANG_VS mode.		*/

	displaying = TRUE;								/* Keep displaying.			*/
	while (displaying)								/* Loop until no more to display.	*/
	{

		displaying = FALSE;							/* Only do one display.			*/

		memset(file, ' ', 8);							/* Initialize variables goin' to	*/
		memset(orig_file, ' ', 8);						/* file_getparm().			*/
		memset(tempname, ' ', 80);
		get_defs(DEFAULTS_IV,volume);
		get_defs(DEFAULTS_IL,library);

		strcpy(entry_message,"To display a file, enter the name and location of the file. ");
		getparm_type[0] = 'I';							/* Start off as an initial getparm.	*/
		getparm_type[1] = ' ';
		getparm_type[2] = '\0';
		mode = 0;
                         
get_name_loop:						      				/* A label to go to.			*/

		file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",		/* Allow the user to enter the name.	*/
			&native_mode,getparm_type,tempname,
			entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL);
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

			if (fh = fopen(filename,"r"))					/* See if we can open file		*/
			{
				fclose(fh);						/* Success - we have access		*/
			}
			else
			{								/* Nope.  Give a new entry message.	*/
				strcpy(entry_message,"UNABLE TO OPEN FILE, ACCESS DENIED or FILE MISSING");
				getparm_type[0] = 'R';					/* Now its a respecify getparm.		*/
				getparm_type[1] = ' ';
				RESTORE_WISPFILEXT;					/* Restore the extension.		*/
				goto get_name_loop;					/* And loop back up.			*/
			}

			/*
			**	Update the PRB for backwards referencing.
			*/
			if ( !native_mode && 'R' == getparm_type[0] )
			{
				use_last_prb();
				getparm_type[0] = 'R';
				getparm_type[1] = 'D';
				file_getparm2(mode,file,library,volume,"INPUT   ","DISP  ",
					&native_mode,getparm_type,tempname,
					entry_message,NULL,&pf_key,'E',orig_file,NULL,NULL,NULL);
			}

			/*
			**	Do the DISPLAY
			*/
			wpushscr();	      						/* Save the screen map and state.	*/
			if ((i = greclen(filename)) >= 0) vdisplay(filename,i);		/* Get the record length.		*/
			else if (i == -2) werrlog(ERRORCODE(2),filename,0,0,0,0,0,0,0);	/* Protection violation.		*/
			else if (i == -1) werrlog(ERRORCODE(4),filename,0,0,0,0,0,0,0);	/* Error on OPEN.			*/
			else vdisplay(filename,255);  					/* Display the file using max buffsize.	*/
			wpopscr();							/* Restore the screen and map.		*/
		}
	}

	oldlevel();									/* Decrement the link-level		*/
	ppunlink(linklevel());								/* Putparm UNLINK			*/
	return(SUCCESS); 	      							/* All done mate.			*/
}
