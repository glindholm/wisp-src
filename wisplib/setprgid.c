static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	setprogid()
*/

#include <string.h>
#include "setprgid.h"

static char WISPPROGID[9];

/* Set global variable to current program id.	*/
void setprogid(const char *wisp_application_name)
{
	memcpy(WISPPROGID,wisp_application_name,8);
	WISPPROGID[8] = '\0';
}

const char* getprogid(void)
{
	return WISPPROGID;
}



/*
**	History:
**	$Log: setprgid.c,v $
**	Revision 1.10  1997/10/21 14:16:32  gsl
**	Removed global WISPPROGID and addded getprogid()
**	
**	Revision 1.9  1996-08-19 18:32:54-04  gsl
**	drcs update
**
**
**
*/
