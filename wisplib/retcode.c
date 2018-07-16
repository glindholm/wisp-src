			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	NAME:	retcode.c
*/

/* module: retcode														*/
/* called by COBOL to get 3 digit status code back from linked COBOL programs							*/ 

#ifdef unix
#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "werrlog.h"
#include "wdefines.h"

#define		ROUTINE		54000
void RETCODE(code)
char 	code[3];
{
	int	gid;
	int	rc;
	char	rcfilename[80];
	char	rcbuff[10];
	FILE	*fh;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	gid = wgetpgrp();

	sprintf(rcfilename,"%s/RC_%04X",WISP_TEMP_DIR,gid);

	strcpy(rcbuff,"000");
	if (fh = fopen(rcfilename,"r"))
	{
		if (1==fscanf(fh,"%d",&rc))
		{
			sprintf(rcbuff,"%03d",rc);
		}
		fclose(fh);
	}
	memcpy(code,rcbuff,(size_t)3);

	return;
}
#endif /* unix */

#ifdef MSDOS
void RETCODE(code)
char *code;
{
	werrlog(102,"(RETCODE) Function NOT-SUPPORTED",0,0,0,0,0,0,0);
}
#endif /* MSDOS */

#ifdef VMS
#include "werrlog.h"
#include "idsistd.h"

void RETCODE(code)
char code[3];
{
#define		ROUTINE		54000
	char	buff[80];
	int4	len;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	memcpy(code,"000",3);							/* Assume OK					*/

	if ( 0 == getsymb("$W_RETURN_CODE",buff,&len) )
	{
		switch(len)
		{
		case 0:	break;
		case 1:	code[2] = buff[0];
			break;
		case 2:	code[1] = buff[0];
			code[2] = buff[1];
			break;
		default:
			memcpy(code,buff,3);
		}
	}
}
#endif
