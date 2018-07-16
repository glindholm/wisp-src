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
static int validch(int ch, char *valch);
static int valnam_filled(char *name);
static int valvol_filled(char *name);
static int val_nam(char *name);
static int val_vol(char *name);



int KCSI_valspec_filled(char *name, char *lib, char *vol)
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

static int valnam_filled(char *name)
{
	if(!val_nam(name))
		return(0);
	if(!memcmp(name,"        ",8))
		return(0);
	return(1);
}
static int valvol_filled(char *name)
{
	if(!val_vol(name))
		return(0);
	if(!memcmp(name,"        ",6))
		return(0);
	return(1);
}
static int val_nam(char *name)
{
	char rc[3];

	memcpy(rc,"00",2);
	VALNAM(name,rc);
	if(memcmp(rc,"00",2))
		return(0);
	else
		return(1);
}


static int val_vol(char *name)
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
static int validch(int ch, char *valch)
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
**	Revision 1.5.2.1  2002/11/12 15:56:38  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.7  2002/10/24 15:48:30  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/10/23 20:39:05  gsl
**	make global name unique
**	
**	Revision 1.5  1996/09/17 23:34:20  gsl
**	drcs update
**	
**
**
*/
