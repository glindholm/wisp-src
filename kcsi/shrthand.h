/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

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
**	Revision 1.5  2003/02/05 15:50:11  gsl
**	Fix copyright headers
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
