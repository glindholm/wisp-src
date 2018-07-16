			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifndef VMS			/* NOT valid on VMS system */

/*
	wrunconfig	Load $WISPCONFIG/wrunconfig into the structure cfg.
*/

#ifdef MSDOS
#include <stdlib.h>
#endif

#include <stdio.h>
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
	char	inline[80], upline[80];
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
			strcpy( wrunpath, ptr );					/* Build path to $WISPCONFIG/wrunconfig */
			strcat( wrunpath, "/" );
			strcat( wrunpath, WRUNCONFIG );

			if (the_file = fopen( wrunpath, "r" ))				/* Open wrunconfig			*/
			{
				while(fgets(inline,132,the_file))			/* Read a line				*/
				{
					if ( strlen(inline) > 0 )			
					{
						if ( inline[strlen(inline)-1] < ' ' )
							inline[strlen(inline)-1] = '\0'; 	/* remove trailing NL		*/
					}

					strcpy(upline,inline);				/* Take a copy and make uppercase	*/
					upper_string(upline);

					if ( strncmp(upline,"OPTIONS=",8) == 0 )
					{
						strcpy(CFG.wrun_options,&inline[8]);
					}
					else if ( strncmp(upline,"RUNCBL=",7) == 0 )
					{
						found_runcbl = 1;
						sscanf(&inline[7],"%s",CFG.wrun_runcbl);
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
						strcpy(CFG.wrun_cobtype,&inline[6]);
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
	}

	strcpy(cfg->wrun_options,CFG.wrun_options);
	strcpy(cfg->wrun_runcbl, CFG.wrun_runcbl);
	strcpy(cfg->wrun_cobtype,CFG.wrun_cobtype);

	return( rc );
}

#endif	/* #ifndef VMS */

