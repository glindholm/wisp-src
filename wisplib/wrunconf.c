static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "idsistd.h"
#include "wrunconf.h"
#include "wdefines.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "wmalloc.h"
#include "wispcfg.h"

int wrunconfig(struct wruncfg *cfg)
{
	static struct	wruncfg	CFG;							/* Static structure to save.		*/
	static int	FIRST=1;
	char	*ptr;
	char	wrunpath[256];
	FILE	*the_file;
	char	inlin[512], upline[512];
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

		{
			buildfilepath( wrunpath, wispconfigdir(), WRUNCONFIG );		/* Build path to $WISPCONFIG/wrunconfig */

			if (the_file = fopen( wrunpath, "r" ))				/* Open wrunconfig			*/
			{
				while(fgets(inlin,sizeof(inlin),the_file))		/* Read a line				*/
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
						ptr = &inlin[8];
						if (strlen(ptr) >= sizeof(CFG.wrun_options))
						{
							ptr = "(WRUN_OPTIONS_TOO_LONG)";
						}
						strcpy(CFG.wrun_options, ptr);
					}
					else if ( strncmp(upline,"RUNCBL=",7) == 0 )
					{
						found_runcbl = 1;
						ptr = &inlin[7];
						if (strlen(ptr) >= sizeof(CFG.wrun_runcbl))
						{
							ptr = "(WRUN_RUNCBL_TOO_LONG)";
						}
						sscanf(ptr,"%s",CFG.wrun_runcbl);
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
						ptr = &inlin[6];
						if (strlen(ptr) >= sizeof(CFG.wrun_cobtype))
						{
							ptr = "(INVALID)";
						}
						strcpy(CFG.wrun_cobtype, ptr);
					}
				}
				fclose(the_file);					/* Close the file			*/
			}
			else
			{
				rc = 2;							/* Unable to read wrunconfig		*/
			}
		}

		if ( ptr = getenv( "RUNCBL" ) )						/* Override runcbl with env variable	*/
		{
			if (strlen(ptr) >= sizeof(CFG.wrun_runcbl))
			{
				ptr = "(WRUN_RUNCBL_ENV_TOO_LONG)";
			}
			strcpy(CFG.wrun_runcbl,ptr);
		}

		if ( ptr = getenv(WRUNOPTIONS_ENV) )
		{
			if (strlen(ptr) >= sizeof(CFG.wrun_options))
			{
				ptr = "(WRUN_OPTIONS_ENV_TOO_LONG)";
			}
			
			strcpy(CFG.wrun_options,ptr);
		}
	}

	strcpy(cfg->wrun_options,CFG.wrun_options);
	strcpy(cfg->wrun_runcbl, CFG.wrun_runcbl);
	strcpy(cfg->wrun_cobtype,CFG.wrun_cobtype);

	return( rc );
}

#endif	/* !VMS */


/*
**	History:
**	$Log: wrunconf.c,v $
**	Revision 1.13  1997/02/25 14:51:18  gsl
**	Add error checking to handle too long of fields
**	
**	Revision 1.12  1996-10-08 20:31:55-04  gsl
**	move wispconfigdir() to wispcfg.h
**
**	Revision 1.11  1996-08-19 15:33:21-07  gsl
**	drcs update
**
**
**
*/
