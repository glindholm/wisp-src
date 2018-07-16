/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
#ifndef DATATYPES
#define	DATATYPES
/*----
Data types for conversions.
All numeric types are or'd with the hi-bit allowing an easy is numeric
test.
------*/

#define	IS_NUM	0x1000
#define	IS_UNS	0x2000
/*Hard types*/
/*COBOL types*/
#define	ACHR	1		/* COBOL char array not null terminated*/
#define	APCK	(2|IS_NUM)	/* Digit per nybble, sign in last nybble*/
#define	AZON	(3|IS_NUM)	/* ASCII digits sign in low byte high nybble*/
#define	AUNS	(4|IS_NUM|IS_UNS)	/* Zoned but Sign disregarded */
#define	ABIN	(5|IS_NUM)	/* 1 - 4 byte binary number */
#define	ABHL	ABIN 		/* Binary ordered hi mid mid lo */
#define	ABLH	(11|IS_NUM)	/* Binary ordered lo mid mid hi */
#define	ABMN	(12|IS_NUM)	/* Binary in Machine order */
/*C types*/
#define	CSTR	6		/* NUL terminated STR */
#define	CINT	(7|IS_NUM)	/* Integer */
#define	CLNG	(8|IS_NUM)	/* Long */
#define	CFLT	(9|IS_NUM)	/* Float */
#define	CDBL	(10|IS_NUM)	/* Double */

/*Soft types - logical types within a hard type*/

#define	CCHR	101		/* Single character stored as an int*/
#define	BCHR	126		/* As above, space not converted to 0 */

/* Artificial Types created by or for conversions */
#define	BZON	(102|IS_NUM)	/* Sign byte plus 15 ASCII digits */
#define	BUNS	(103|IS_NUM|IS_UNS)	/* As BZON but sign ignored */
#define	BYES	104		/* A Yes/No field converted to TRUE/FALSE*/
#define	BLNK	105		/* True if not blank */
#define	BLIT	106		/* A quoted Literal */
#define	BIDX	107		/* A parenthized occurence number */
#define	BMSK	108		/* Bit mask as an integer len = bit 1 - 16*/
#define	BCTP	109		/* CONTROL 1 character data type */
#define	BDTP	110		/* One of these data types */
#define	BONE	(111|IS_NUM)	/* 1 if zero after conversion*/
#define	BTRN	112		/* null truncate to last non blank */
#define	BPOS    (113|IS_NUM)	/* Convert to 0 based offset */

/*Display types  or edited types */

#define	DHEX	121		/*HEX digits*/
#define	DZON	(122|IS_NUM)	/*Displayable zoned (with decimal)*/
#define	DUNS	(123|IS_NUM|IS_UNS)	/*Same for unsigned*/
#define NLIT	(124|IS_NUM)	/*Numeric Literal*/
#define	TZON	(125|IS_NUM)	/*Display with decimal and trailing sign*/
#define	BZON_LEN	16
#define	BUNS_LEN	16


/*----
The decimal field will hold values 0 - 9 (for now) so the high order
byte can be used for flags. 
------*/
#define	DESCENDING_SORT	0x0100

typedef struct _dtype{
	char *_base;
	int _pos;
	int _len;
	int _type;
	int _dec;
	}DTYPE;

int comp_zero(DTYPE *lop);
int comp_space(DTYPE *lop);
void rcvt_pair(DTYPE *dest,DTYPE *src);
void cvt_list(DTYPE *dest,DTYPE *src);
void cvt_record(DTYPE *dest,DTYPE *src,char *record);
void cvt_to_record(DTYPE *d,DTYPE *src,char *record);
void cvt_data(DTYPE *dest,DTYPE *src);
void dbltob(char *dest,double dbl,int len,int type);
void nltoza(char *dest,char *src,int dlen,int ddec);
void zatoza(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
void uatoza(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
void zatoua(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
void uatoua(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
void zatoz(char *dest,char *src,int dlen,int slen,int sign_code);
void zatou(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
void btoeh(char *dest,char *src,int dlen,int slen,int type);
void eutob(char *dest,char *src,int dlen,int slen,int sdec,int type);
char *cvt_type(DTYPE *dt);
int rptcmb(DTYPE *lop,int op,DTYPE *rop);
void cvt_one_rfl(DTYPE *d,DTYPE *s,char *record);
int comp_rec_num(long num,DTYPE *lop,DTYPE *rop);
int rptcmp(DTYPE *lop,char *op,DTYPE *rop);

#endif
/*
**	History:
**	$Log: dtype.h,v $
**	Revision 1.4  1997-06-05 13:01:58-04  scass
**	Corrected prototype for dtob so is named dbltob
**
**	Revision 1.3  1996-09-17 19:34:06-04  gsl
**	drcs update
**
**
**
*/
