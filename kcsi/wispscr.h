/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef _WISPSCR_H
#define _WISPSCR_H

/*----
Definitions of structures used in building a screen.
------*/

typedef struct	_wispfld
	{
	int	len;		/* field length				*/
	int	row;		/* field row position			*/
	int	col;		/* field column position		*/
	char	*src;		/* ptr to source for the field		*/
	char	*obj;		/* ptr to object for field		*/
	int	fac;		/* the amazing FAC			*/
	}WISPFLD;

#define FAC_BIT 		(0x80)  /* this byte is a fac */
#define MOD_BIT 		(0x40)  /* was modified       */
#define	PROTECT_BIT		(0x04)	/* not modifiable     */
#define BLINK_BIT		(0x10)  /* starts here        */
#define	ERROR_BIT		BLINK_BIT
#define	UPPER_ENTRY_FAC		(0x81)
#define	UPPER_FAC		UPPER_ENTRY_FAC
#define	NUMERIC_ENTRY_FAC	(0x82)
#define	UPLOW_ENTRY_FAC		(0x80)
#define	DIM_FAC			(0x8C)
#define	HIDE_FAC		(0x9C)
#define	BRITE_FAC		(0x84)
#define	TAB_FAC			(0x86)
#define	PSEUDO_BLANK		(0x0B)
#define	NO_FAC			(0x00)

#define WISPSCRF(x)		WISPFLD   x
#define WISPSCR_FLDS(x)		WISPFLD   x[]
#define	LENGTH(x)		x,
#define	LEN			LENGTH
#define	COL(x)			x,
#define	COLUMN			COL
#define	ROW(x)			x,
#define	SOURCE(x)		x,
#define VALUE(x)		x,NULL,DIM_FAC
#define OBJECT(x)		x,UPPER_ENTRY_FAC
#define	USING(x)		x,OBJECT(x)
#define UPLOW(x)		x,x,UPLOW_ENTRY_FAC
#define UPPER(x)		x,x,UPPER_ENTRY_FAC
#define	NUMERIC(x)		x,x,NUMERIC_ENTRY_FAC
#define	FROM(x)			VALUE(x)
#define BRIGHT(x)		x,NULL,BRITE_FAC
#define LASTITEM		0,0,0,NULL,NULL,0

#ifdef EXTERN_DEF
#undef EXTERN_DEF
#endif

#ifdef	_WISPSCR_C
#define	EXTERN_DEF
#define	INIT_DEF_OA	={0x01,0xa0,0x00,0x00}
#define	INIT_LOCKED_OA	={0x01,0x00,0x00,0x00}
#define	INIT_ALL_LINES  ={24}
#else
#define	EXTERN_DEF	extern
#define	INIT_DEF_OA
#define	INIT_LOCKED_OA
#define	INIT_ALL_LINES
#endif

EXTERN_DEF	char default_oa[4]	INIT_DEF_OA;
EXTERN_DEF	char locked_oa[4]	INIT_LOCKED_OA;
EXTERN_DEF	char all_lines[1]	INIT_ALL_LINES;

#endif	/* _WISPSCR_H */
/*
**	History:
**	$Log: wispscr.h,v $
**	Revision 1.3  1996/09/17 23:34:22  gsl
**	drcs update
**	
**
**
*/
