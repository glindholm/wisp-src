			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	NAME:	setretcode.c
*/

/* subroutine: setretcode													*/
/* called by COBOL to pass PIC 999 status code back to calling shell								*/

#ifdef unix
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"

#define		ROUTINE		59000
void setretcode(code)
char 	code[3];
{
	extern char WISPRETURNCODE[3];
	extern int4 LINKRETCODE;
	int	gid;
	int	rc;
	char	rcfilename[80];
	char	rcbuff[10];
	FILE	*fh;

	werrlog(ERRORCODE(1),code,0,0,0,0,0,0,0);

	memcpy(WISPRETURNCODE,code,(size_t)3);					/* Copy cobol data area to C data area		*/

	memcpy(rcbuff,code,(size_t)3);
	rcbuff[3] = (char)0;
	LINKRETCODE = ATOI4(rcbuff);						/* Convert to int4				*/
	rc = LINKRETCODE;

	if (!fexists(WISP_TEMP_DIR))						/* Ensure WISP_TEMP_DIR exists			*/
	{
		if (mkdir ( WISP_TEMP_DIR, 0777))
		{
			werrlog(ERRORCODE(4),WISP_TEMP_DIR, errno,0,0,0,0,0,0);
			return;
		}
		chmod ( WISP_TEMP_DIR,0777);
	}

	gid = wgetpgrp();

	sprintf(rcfilename,"%s/RC_%04X",WISP_TEMP_DIR,gid);

	if (0==rc)
	{
		/*
		**	Special case for RC of ZERO, no file is created.
		**	Remove pre-existing file.
		*/
		unlink(rcfilename);
		return;
	}

	if (fh = fopen(rcfilename,"w"))
	{
		fprintf(fh,"%03d\n",rc);
		fclose(fh);
	}
	else
	{
		werrlog(ERRORCODE(4),rcfilename, errno,0,0,0,0,0,0);
	}

	return;
}
#endif /* unix */

				/* The MSDOS section of this routine has NOT been properly coded and will NOT work until it is.	*/
				/* What is here is the remnants of the unix version which will compile under MSDOS.		*/
				/* The logic, however, is questionable at best and needs some major attention.			*/
#ifdef MSDOS

#include "werrlog.h"

#define		ROUTINE		59000
void setretcode(wispreturncode)
char *wispreturncode;
{
}

#endif	/* MSDOS */

#ifdef VMS
#include "werrlog.h"

#define 	LIB$K_CLI_GLOBAL_SYM	2
#define		ROUTINE		59000
setretcode(rc)
char *rc;  /* rc[3] */
{
	int4	size;

	werrlog(ERRORCODE(1),rc,0,0,0,0,0,0,0);

	size=3;
	if (*rc == '0')								/* Remove leading zeros				*/
	{
		rc++;
		size--;
	}
	if (*rc == '0')
	{
		rc++;
		size--;
	}

	setsymb("$W_RETURN_CODE",rc,size,LIB$K_CLI_GLOBAL_SYM);
}
#endif


