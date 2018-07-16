static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "kcsifunc.h"

static char sccsid[]="@(#)valflv.c	1.6 1/27/93";

/*----
Validate file, library
or volume or a field name
------*/
/*
static char invchar[]=
"!%^&*()-_=+[],\";:/?<>.\\{}|~`";
*/

static char valid_flv_char[] = "@#$";
static char valid_fnm_char[] = "@#$-";

static void valit(char *name,char *rc,int len,char *valchar);


int valspec(char *name, char *lib, char *vol)
{
	int rc;
	rc = 0;
	rc = 	val_nam(name) +
		val_nam(lib) +
		val_vol(vol);
	if(rc == 3)
		return(1);
	return(0);
}

int valspec_filled(char *name, char *lib, char *vol)
{
	int rc;
	rc = 0;
	rc = 	valnam_filled(name) +
		valnam_filled(lib) +
		valvol_filled(vol);
	if(rc == 3)
		return(1);
	return(0);
}

int valnam_filled(char *name)
{
	if(!val_nam(name))
		return(0);
	if(!memcmp(name,"        ",8))
		return(0);
	return(1);
}
int valvol_filled(char *name)
{
	if(!val_vol(name))
		return(0);
	if(!memcmp(name,"        ",6))
		return(0);
	return(1);
}
int val_nam(char *name)
{
	char rc[3];

	memcpy(rc,"00",2);
	VALNAM(name,rc);
	if(memcmp(rc,"00",2))
		return(0);
	else
		return(1);
}


int val_vol(char *name)
{
	char rc[3];

	memcpy(rc,"00",2);
	VALVOL(name,rc);
	if(memcmp(rc,"00",2))
		return(0);
	else
		return(1);
}

void VALNAM(char *name, char *rc)
{
	valit(name,rc,8,valid_flv_char);
}

void VALVOL(char *name, char *rc)
{
	valit(name,rc,6,valid_flv_char);
}

void VALFNM(char *name, char *rc)
{
	valit(name,rc,8,valid_fnm_char);
}

static void valit(char *name,char *rc,int len,char *valchar)
{
	int idx,space;

	memcpy(rc,"00",2);

	for(idx=0, space = 0; idx < len; ++idx, ++name)
		{
		if(*name == ' ')
			{
			space = 1;
			continue;
			}
		if(*name != ' ')
			{
			if(space)
				{
				memcpy(rc,"99",2);
				return;
				}
			}
		if( (*name <='Z') && (*name >= 'A'))  /* = OK */
			continue; 
		if( (*name <= '9') && (*name >= '0')) /* = OK */
			continue;
		/* Else It must be on the valid chars list */
		if( validch(*name,valchar) )
			continue;
		memcpy(rc,"99",2);
		return;
		} 
	
	
}

/*----
Return true if ch appears in valch
------*/
int validch(int ch, char *valch)
{
	for( ; *valch; ++valch)
		{
		if( ch == *valch)
			return(1);
		}
	return(0);
}

/*
**	History:
**	$Log: valflv.c,v $
**	Revision 1.5  1996-09-17 19:34:20-04  gsl
**	drcs update
**
**
**
*/
