			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	File:		wrunconf.c
**
**	Purpose:	To hold the routines to do wrunconfig processing.
**
**	Routines:	wrunconfig()	Read the wrunconfig file and load a struct with it's info.
**
**
**	History:
**
*/

#ifndef VMS			/* NOT valid on VMS system */

/*
	wrunconfig	Load $WISPCONFIG/wrunconfig into the structure cfg.
*/

#ifdef MSDOS
#include <stdlib.h>
#endif

#include <stdio.h>
#include "idsistd.h"
#include "wrunconf.h"
#include "wdefines.h"

int wrunconfig(cfg)
struct wruncfg *cfg;
{
	static struct	wruncfg	CFG;							/* Static structure to save.		*/
	static int	FIRST=1;
	char	*ptr;
	char	wrunpath[80];
	FILE	*the_file;
	char	inlin[80], upline[80];
	int	found_runcbl, found_cobtype;
	int	rc;

	rc = 0;

	if (FIRST)									/* First time load file data into struct */
	{
		FIRST=0;

		found_runcbl  = 0;
		found_cobtype = 0;

		strcpy(CFG.wrun_options,"");						/* Default to no options		*/
		strcpy(CFG.wrun_runcbl,"runcbl");					/* Default to runcbl			*/
		strcpy(CFG.wrun_cobtype,"ACU");						/* Default to ACUCOBOL			*/

		if ( ptr = (char *)getenv( WISP_CONFIG_ENV ) )				/* Get $WISPCONFIG			*/
		{
			buildfilepath( wrunpath, ptr, WRUNCONFIG );			/* Build path to $WISPCONFIG/wrunconfig */

			if (the_file = fopen( wrunpath, "r" ))				/* Open wrunconfig			*/
			{
				while(fgets(inlin,132,the_file))			/* Read a line				*/
				{
					if ( strlen(inlin) > 0 )			
					{
						if ( inlin[strlen(inlin)-1] < ' ' )
							inlin[strlen(inlin)-1] = '\0'; 	/* remove trailing NL		*/
					}

					strcpy(upline,inlin);				/* Take a copy and make uppercase	*/
					upper_string(upline);

					if ( strncmp(upline,"OPTIONS=",8) == 0 )
					{
						strcpy(CFG.wrun_options,&inlin[8]);
					}
					else if ( strncmp(upline,"RUNCBL=",7) == 0 )
					{
						found_runcbl = 1;
						sscanf(&inlin[7],"%s",CFG.wrun_runcbl);
					}
					else if ( strncmp(upline,"COBOL=ACU",9) == 0 )
					{
						found_cobtype = 1;
						strcpy(CFG.wrun_cobtype,"ACU");
					}
					else if ( strncmp(upline,"COBOL=AIX",9) == 0 )
					{
						found_cobtype = 1;
						strcpy(CFG.wrun_cobtype,"AIX");
					}
					else if ( strncmp(upline,"COBOL=MF",8) == 0 )
					{
						found_cobtype = 1;
						strcpy(CFG.wrun_cobtype,"MF");
					}
					else if ( strncmp(upline,"COBOL=",6) == 0 )
					{
						found_cobtype = 1;
						strcpy(CFG.wrun_cobtype,&inlin[6]);
					}
				}
				fclose(the_file);					/* Close the file			*/
			}
			else
			{
				rc = 2;							/* Unable to read wrunconfig		*/
			}
		}
		else
		{
			rc = 1;								/* No WISPCONFIG			*/
		}

		if ( ptr = (char *)getenv( "RUNCBL" ) )					/* Override runcbl with env variable	*/
		{
			strcpy(CFG.wrun_runcbl,ptr);
		}

		if ( ptr = (char *)getenv(WRUNOPTIONS_ENV) )
		{
			strcpy(CFG.wrun_options,ptr);
		}
	}

	strcpy(cfg->wrun_options,CFG.wrun_options);
	strcpy(cfg->wrun_runcbl, CFG.wrun_runcbl);
	strcpy(cfg->wrun_cobtype,CFG.wrun_cobtype);

	return( rc );
}

#endif	/* !VMS */


