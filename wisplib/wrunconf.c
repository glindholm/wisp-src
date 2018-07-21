/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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


/*
**	File:		wrunconf.c
**
**	Purpose:	To hold the routines to do wrunconfig processing.
**
**	Routines:	WL_wrunconfig()	Read the wrunconfig file and load a struct with it's info.
**
**
**	History:
**
*/


/*
	wrunconfig	Load $WISPCONFIG/wrunconfig into the structure cfg.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "idsistd.h"
#include "wrunconf.h"
#include "wdefines.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "wmalloc.h"
#include "wispcfg.h"
#include "werrlog.h"

int WL_wrunconfig(struct wruncfg *cfg)
{
	static struct	wruncfg	CFG;						/* Static structure to save.		*/
	static int	FIRST=1;
	static int	rc = 0;


	if (FIRST)								/* First time load file data into struct */
	{
		char	*ptr;
		char	wrunpath[256];
		FILE	*the_file;
		char	inlin[512], upline[512];
		FIRST=0;

		strcpy(CFG.wrun_options,"");					/* Default to no options		*/
		strcpy(CFG.wrun_runcbl,"runcbl");				/* Default to runcbl			*/
		strcpy(CFG.wrun_cobtype,WRUNCOBTYPE_DEF);			/* Default to ACU on WIN32 and MF on UNIX*/

		buildfilepath( wrunpath, wispconfigdir(), WRUNCONFIG );		/* Build path to $WISPCONFIG/wrunconfig */

		if ((the_file = fopen( wrunpath, "r" )))			/* Open wrunconfig			*/
		{
			while(fgets(inlin,sizeof(inlin),the_file) != NULL)	/* Read a line				*/
			{
				int len;

				/*
				 *	Trim trailing whitespace (plus other control characters)
				 */
				len = strlen(inlin);
				while(len>0 && 
				      (' '==inlin[len-1] || 
				       '\t'==inlin[len-1] ||
				       '\n'==inlin[len-1] ||
				       '\f'==inlin[len-1] ||
				       '\r'==inlin[len-1]))
				{
					len--;
					inlin[len] = '\0';
				}

				if (0 == len)		/* Skip empty lines */
				{
					continue;
				}
				if ( '#' == inlin[0])	/* Skip comment lines */
				{
					continue;
				}

				strcpy(upline,inlin);				/* Take a copy and make uppercase	*/
				upper_string(upline);

				if (0 == memcmp(upline,"OPTIONS=",8))
				{
					ptr = &inlin[8];
					if (strlen(ptr) >= sizeof(CFG.wrun_options))
					{
						ptr = "(WRUN_OPTIONS_TOO_LONG)";
					}
					strcpy(CFG.wrun_options, ptr);
				}
				else if (0 == memcmp(upline,"RUNCBL=",7))
				{
					ptr = &inlin[7];
					if (strlen(ptr) >= sizeof(CFG.wrun_runcbl))
					{
						ptr = "(WRUN_RUNCBL_TOO_LONG)";
					}
					sscanf(ptr,"%s",CFG.wrun_runcbl);
				}
				else if (0 == memcmp(upline,"COBOL=",6))
				{
					ptr = &inlin[6];
					if (0 == strcmp(ptr, WRUNCOBTYPE_ACU))
					{
						strcpy(CFG.wrun_cobtype, WRUNCOBTYPE_ACU);
					}
					else if (0 == strcmp(ptr, WRUNCOBTYPE_MF))
					{
						strcpy(CFG.wrun_cobtype, WRUNCOBTYPE_MF);
					}
					else if (strlen(ptr) >= sizeof(CFG.wrun_cobtype))
					{
						ptr = "(INVALID)";
					}
					else
					{
						strcpy(CFG.wrun_cobtype, ptr);
					}
				}
				else
				{
					WL_wtrace("WRUNCONFIG","INVALID", 
						"Invalid entry found [%s] in file [%s]", 
						inlin, wrunpath);
				}
			}
			fclose(the_file);					/* Close the file			*/
		}
		else
		{
			rc = 2;							/* Unable to read wrunconfig		*/
			WL_wtrace("WRUNCONFIG","FILE", 
				"Unable to open WRUNCONFIG file [%s] errno=[%d] msg=[%s]",
				wrunpath, errno, WL_strerror(errno));

		}

		if ( (ptr = getenv( "RUNCBL" )) )						/* Override runcbl with env variable	*/
		{
			if (strlen(ptr) >= sizeof(CFG.wrun_runcbl))
			{
				ptr = "(WRUN_RUNCBL_ENV_TOO_LONG)";
			}
			strcpy(CFG.wrun_runcbl,ptr);
		}

		if ( (ptr = getenv(WRUNOPTIONS_ENV)) )
		{
			if (strlen(ptr) >= sizeof(CFG.wrun_options))
			{
				ptr = "(WRUN_OPTIONS_ENV_TOO_LONG)";
			}
			
			strcpy(CFG.wrun_options,ptr);
		}

		WL_wtrace("WRUNCONFIG","VALUES", "COBOL=[%s] RUNCBL=[%s] OPTIONS=[%s]",
			CFG.wrun_cobtype, CFG.wrun_runcbl, CFG.wrun_options);
	}

	strcpy(cfg->wrun_options,CFG.wrun_options);
	strcpy(cfg->wrun_runcbl, CFG.wrun_runcbl);
	strcpy(cfg->wrun_cobtype,CFG.wrun_cobtype);

	return( rc );
}



/*
**	History:
**	$Log: wrunconf.c,v $
**	Revision 1.19  2003/02/04 17:22:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.18  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.17  2002/10/14 14:49:08  gsl
**	cleanup add tracing
**	
**	Revision 1.16  2002/10/11 20:39:52  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.15  2002/07/10 21:05:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.14  2002/06/21 03:10:45  gsl
**	Remove VMS & MSDOS
**	
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
