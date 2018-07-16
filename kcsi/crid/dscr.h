/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifdef OLD

#include "kplatfrm.h"

/*----
Definitions of structures used in controlling a screen.
------*/

/*----
Maximum number of fields on a screen.
Maximum length of a field.
Maximum number of enabled screen terminators.
------*/

#define MAXLNGTH 80

typedef struct  _field
	{
	int      len;		/* field length                  */
	char     *picture;
	int      row;		/* field row position            */
	int      col;		/* field column position         */
	char     *src;		/* ptr to source for the field   */
	char     *obj;		/* ptr to object for field       */
	int      fac ;		/* the amazing FAC               */
	}FIELD;

typedef struct _screen
	{
	int      pfcode;		/* contains code of return pfkey */
	int	 *pfs;
	int      lock;			/* 'n'ormal 'l'ock 'w'aitlock    */
	FIELD    *afld;			/* active field on exit          */
	int      ismod;			/* on exit 'm'odified or 'n'ot   */
	FIELD    *fld;			/* fields                        */
	}SCR;

/*----
definitions used for fac control and macros for defining a screen.
------*/

#define BELL		'\07'
#define ALLBITS		0x7F
#define FACBIT		0x80
#define BRITEBIT	0x01
#define MODBIT		0x02
#define RVRSBIT		0x04
#define BLINKBIT	0x08
#define ERRORBIT	BLINKBIT
#define ULINEBIT	0x10
#define TABBIT		0x20
#define ATTRBITS	(BRITEBIT|RVRSBIT|BLINKBIT|ULINEBIT)
#define ISMODBIT	0x40


#define	SCREEN_PFS(x)		static int	x[]
#define	SELECT(x)		(x + 256)

#define	SCREEN_FIELDS(x)	static FIELD	x[]
#define	ITEM			{
#define LENGTH(x)		x,
#define	PICX(x)			x,
#define	ROW(x)			x,
#define COLUMN(x)		x,
#define COL(x)			x,
#define SOURCE(x)		x,
#define OBJECT(x)		x,FACBIT|MODBIT|BRITEBIT},
#define USING(x)		SOURCE(x) OBJECT(x)
#define VALUE(x)		x,NULL,FACBIT},
#define FROM(x)			VALUE(x)
#define	FILLER			{
#define	LASTITEM		{0,NULL,0,0,NULL,NULL,0}
#define ISAFAC(x)		(x->fac & FACBIT)
#define ISMODABLE(x)		(x->fac & MODBIT)
#define ISBLNK(x)		(x->fac & BLINKBIT)
#define ISBMOD(x)		(ISMODABLE(x) && ISBLNK(x))
#define	READ_CRT		0
#define	LOCK_CRT		1
#define	READ_FKEY_ONLY		2
#define	SCREEN_MODIFIED		1
#define	SCREEN_NOT_MODIFIED	0
#define	IS_MODIFIED(x)		(x.ismod==SCREEN_MODIFIED)
#define	IS_NOT_MODIFIED(x)	(x.ismod==SCREEN_NOT_MODIFIED)

#define	ESC			27
#ifdef	KCSI_DOS
#define	ENTER_KEY		13
#else
#define	ENTER_KEY		10
#endif

#define	NULK			255
#define	TAB			9

#endif /* OLD */
/*
**	History:
**	$Log: dscr.h,v $
**	Revision 1.3  1996-09-17 19:34:06-04  gsl
**	drcs update
**
**
**
*/
