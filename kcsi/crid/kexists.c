static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include "intdef.h"
#include "kcsifunc.h"

static char sccsid[]= "@(#)kexists.c	1.4 1/23/93";

static int4 starter, count, found;

static char receiver[23];


void KEXISTS(char *rc, char *name, char *lib, char *vol)
{
	int4 seven;

	seven = 7;
	strcpy(receiver,"                      ");
	starter = 1;
	count = 1;
	found = 0;
	wswap(&starter);
	wswap(&count);
	wswap(&found);
	wvaset(&seven);
	FIND(name, lib, vol, &starter, &count, receiver,&found);
	if(found == 0)
		memcpy(rc,"096",3);
	else
		memcpy(rc,"000",3);
}

/*----
C Version
-----*/
int ckexists(char *file, char *lib, char *vol)
{
	char rc[4];

	memcpy(rc,"000",3);

	KEXISTS(rc,file,lib,vol);
	if(memcmp(rc,"000",3))
		return(0);
	return(1);
}

/*
**	History:
**	$Log: kexists.c,v $
**	Revision 1.6  1996-10-02 12:14:33-04  gsl
**	Fix prototypes
**
**	Revision 1.5  1996-09-17 16:34:11-07  gsl
**	drcs update
**
**
**
*/
