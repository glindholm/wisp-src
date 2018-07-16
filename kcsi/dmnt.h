/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

#define	Vert_off_of(x)		(x[2])
#define	Row_of(x)		(x[4])
#define	Col_of(x)		(x[5])
#define	Dec_of(x)		(x[8])
#define	Fac_of(x)		(x[13])
#define	Fac_of_numeric		(x[17])
#ifdef	BACKWORDS
#define	Len_of(x)		(x[6])
#else
#define	Len_of(x)		(x[7])
#endif

/* Edit masks must be passed to other routines as pointers*/
#define	Edit_of(x)		(&x[9])
#define	Zero_edit_of(x)		(&x[13])

/*----
Some facs
------*/
typedef unsigned char fac_t;

#define DISPLAY_FAC	(fac_t)0x8c
#define DIM_FAC 	(fac_t)0x8c
#define ALPHA_FAC	(fac_t)0x81
#define NUMERIC_FAC	(fac_t)0x82
#define BRIGHT_FAC	(fac_t)0x84
#define	HIDE_FAC	(fac_t)0x9C

/* FAC Bits */
#define FAC_BIT 	0x80
#define MOD_BIT 	0x40
#define ULINE_BIT	0x20
#define BLINK_BIT	0x10
#define DIM_BIT 	0x08
#define PROTECT_BIT	0x04
#define NUMERIC_BIT	0x02
#define UPPER_BIT	0x01

#define ERROR_BIT	BLINK_BIT
/*----
On the Wang there is no such thing as a DIM BLINK, only BRIGHT BLINK
When both bits
are on, the field is treated as hidden. Hence Hide FAC is 80|10|8|4
Invisible fac is 80|10|8.   (ie for uplow passwords).
The numeric upper combination is reserved and not used.
Ie the last two bits may be 00 (uplow), 01 (upper) or 10 (numeric).
11 is not used and Wang refers to it as reserved.
------*/

#define MESSAGE_LEN	70
#define	IDX_LEN		3

/*----
Codes for calls to fac_all_fields() and fac_one_field
------*/
#define	PROTECT_ALL		1
#define	UNPROTECT_ALL		0
#define	PROTECT_KEY		-1
#define	PROTECT_NOMOD		2
#define	HIDE_FIELD		3
#define	DIM_FIELD		4
/*
**	History:
**	$Log: dmnt.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:06  gsl
**	drcs update
**	
**
**
*/
