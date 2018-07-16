/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/*----
Some shorthand.
------*/
#define	Clear(x)	memset(&x,0,sizeof(x))
#define StrClear(x)	memset(x,0,sizeof(x))
#define _ClearStr(x)	memset(x,' ',sizeof(x)); x[sizeof(x) - 1] = 0
#define	ClearStr(x)	do{_ClearStr(x);}while(0)
#define MemSpaceFill(x) memset(x,' ',sizeof(x))
#define SpaceFill(x)	KCSI_unstrunc(x,sizeof(x) - 1)
#define SpaceOut(x)	x[0] = 0; SpaceFill(x)
#define	Streq(x,y)	(!strcmp(x,y))
#define	Strneq(x,y)	(strcmp(x,y))
#define	Memeq(x,y,z)	(!memcmp(x,y,z))
#define DestCopy(x,y)	memcpy(x,y,sizeof(x))
#define SrcCopy(x,y)	memcpy(x,y,sizeof(y))

/*
**	History:
**	$Log: shrthand.h,v $
**	Revision 1.3.2.1  2002/11/12 15:56:38  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.4  2002/07/25 15:20:23  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:34:19  gsl
**	drcs update
**	
**
**
*/
