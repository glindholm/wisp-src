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
#define	DIM_FAC			(0x8C)
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
#define ERRORA(x)

#define OBJECT(x)		x,UPPER_ENTRY_FAC
#define	USING(x)		x,OBJECT(x)
#define UPLOW(x)		x,x,UPLOW_ENTRY_FAC
#define UPPER(x)		x,x,UPPER_ENTRY_FAC
#define	NUMERIC(x)		x,x,NUMERIC_ENTRY_FAC
#define	FROM(x)			VALUE(x)
#define BRIGHT(x)		x,NULL,BRITE_FAC
#define LASTITEM		0,0,0,NULL,NULL,0

#endif	/* _VSESCR_H */
