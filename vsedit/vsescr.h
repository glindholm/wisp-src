/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

#ifndef _VSESCR_H
#define _VSESCR_H

/*----
Definitions of structures used in building a screen.
------*/

typedef struct	_vsefld
	{
	int	len;		/* field length				*/
	int	row;		/* field row position			*/
	int	col;		/* field column position		*/
	char	*src;		/* ptr to source for the field		*/
	char	*obj;		/* ptr to object for field		*/
	int	fac;		/* the amazing FAC			*/
	}VSEFLD;

#define FAC_BIT 		(0x80)  /* this byte is a fac */
#define MOD_BIT 		(0x40)  /* was modified       */
#define	PROTECT_BIT		(0x04)	/* not modifiable     */
#define BLINK_BIT		(0x10)  /* starts here        */
#define	ERROR_BIT		BLINK_BIT
#define	UPPER_ENTRY_FAC		(0x81)
#define	NUMERIC_ENTRY_FAC	(0x82)
#define	UPLOW_ENTRY_FAC		(0x80)
#define	DIM_FAC			((char)0x8C)
#define DIM_UNDERLINE_FAC	(0xAC)
#define	HIDE_FAC		(0x9C)
#define	BRITE_FAC		(0x84)

#define VSESCRF(x)		VSEFLD   x
#define VSESCR_FLDS(x)		VSEFLD   x[]
#define	LENGTH(x)		x,
#define	LEN			LENGTH
#define	COL(x)			x,
#define	COLUMN			COL
#define	ROW(x)			x,
#define	SOURCE(x)		x,
#define VALUE(x)		x,NULL,DIM_FAC
#define ULINEVALUE(x)	x,NULL,DIM_UNDERLINE_FAC

#define BLINK(x)		x,x,BLINK_FAC
#define ERRORN(x)               x,NULL,ERROR_FAC

#define OBJECT(x)		x,UPPER_ENTRY_FAC
#define	USING(x)		x,OBJECT(x)
#define UPLOW(x)		x,x,UPLOW_ENTRY_FAC
#define UPPER(x)		x,x,UPPER_ENTRY_FAC
#define	NUMERIC(x)		x,x,NUMERIC_ENTRY_FAC
#define	FROM(x)			VALUE(x)
#define BRIGHT(x)		x,NULL,BRITE_FAC
#define LASTITEM		0,0,0,NULL,NULL,0

void vsescr(VSEFLD *fld, unsigned char *scr);
void vsescr_init(unsigned char *scr);
void vsefld(VSEFLD *fld, unsigned char *scr);
void vseunscr(VSEFLD *fld, unsigned char *scr);
void vseunfld(VSEFLD *fld, unsigned char *scr);

#endif	/* _VSESCR_H */
/*
**	History:
**	$Log: vsescr.h,v $
**	Revision 1.12  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.11  2002/08/01 15:49:36  gsl
**	type warnings
**	
**	Revision 1.10  1996/09/03 22:24:10  gsl
**	drcs update
**	
**
**
*/
