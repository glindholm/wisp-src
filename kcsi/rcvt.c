/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

#include <stdio.h>
#include <ctype.h>
#include "dtype.h"
#include "shrthand.h"
#include "kcsifunc.h"


/*----
See under LPI below for description of ASCII OVREPUNCH
PACKED_C defines whether the packed positive value is 0x0c or
0x0f as on some systems

LPI and MF use PACKED_C for positive values
ACU_COBOL apparently uses packed F
------*/


#define	PACKED_C_POSITIVE

#ifdef	KCSI_ACU
#undef	PACKED_C_POSITIVE
#endif	/* KCSI_ACU */

/*----
Three different zoned sign schemes
------*/
#ifdef	KCSI_ACU
#define	make_zoned_ascii	make_zoned_acu
#endif
#ifdef KCSI_MFX
#define	make_zoned_ascii	make_zoned_mf
#endif
static void make_zoned_ascii(int digit, int sign, char *dest);

/*----
Jan 25 1992 eutob() error assumes the machine is byte ordered 1234 and
converts a long directly to an integer. fixed.
v. 2.00 changes handling of a packed sign so that only the low order
nybble is converted to a sign.
------*/
/*----
Some Shorthand
------*/
#define	SRC_ADDRESS	(src->_base + src->_pos)
#define	DEST_ADDRESS	(dest->_base + dest->_pos)
#define	SRC_INT		(*(int*)(SRC_ADDRESS))
#define	DEST_INT	(*(int*)(DEST_ADDRESS))
#define	DEST_DOUBLE	(*(double*)(DEST_ADDRESS))
#define	SRC_DOUBLE	(*(double*)(SRC_ADDRESS))

/*----
Sign codes
These are a bit odd, but it's the way the thing developed so I've
left them this way (do not change).
------*/

#define	FRONT_SIGN	0
#define	BACK_SIGN	1
#define	NO_SIGN		-1


static double btod(char *src,int slen,int type);
static double ptod(char *src,int slen,int dec);
static double antod(char *src,int slen,int pdec);
static double untod(char *src,int slen,int pdec);
static long btol(char *src,int slen,int type);
static void cvt_from_double(DTYPE *dest,DTYPE *src);
static void double_to_binary(DTYPE *dest,DTYPE *src);
static void double_to_packed(DTYPE *dest,DTYPE *src);
static void double_to_zoned(DTYPE *dest,DTYPE *src);
static void double_to_unsigned(DTYPE *dest,DTYPE *src);
static void cvt_from_edited_zoned(DTYPE *dest,DTYPE *src);
static void cvt_from_edited_unsigned(DTYPE *dest,DTYPE *src);
static void cvt_from_edited_hex(DTYPE *dest,DTYPE *src);
static void cvt_from_trailing_zoned(DTYPE *dest,DTYPE *src);
static void edited_hex_to_binary(DTYPE *dest,DTYPE *src);
static void trailing_zoned_to_binary(DTYPE *dest,DTYPE *src);
static void edited_zoned_to_zoned(DTYPE *dest,DTYPE *src);
static void edited_zoned_to_packed(DTYPE *dest,DTYPE *src);
static void trailing_zoned_to_zoned(DTYPE *dest,DTYPE *src);
static void edited_unsigned_to_zoned(DTYPE *dest,DTYPE *src);
static void edited_unsigned_to_packed(DTYPE *dest,DTYPE *src);
static void edited_unsigned_to_unsigned(DTYPE *dest,DTYPE *src);
static void edited_unsigned_to_binary(DTYPE *dest,DTYPE *src);
static void cvt_from_str(DTYPE *dest,DTYPE *src);
static void cvt_from_char(DTYPE *dest,DTYPE *src);
static void trailing_zoned_to_packed(DTYPE *dest,DTYPE *src);
static void char_to_str(DTYPE *dest,DTYPE *src);
static void char_to_char(DTYPE *dest,DTYPE *src);
static void achar_to_achar(DTYPE *dest,DTYPE *src);
static void str_to_char(DTYPE *dest,DTYPE *src);
static void char_to_trunc(DTYPE *dest,DTYPE *src);
static void k_trunc(DTYPE *dest);
static void char_to_int(DTYPE *dest,DTYPE *src);
static void char_to_cchar(DTYPE *dest,DTYPE *src);
static void char_to_bchar(DTYPE *dest,DTYPE *src);
static void char_to_one(DTYPE *dest,DTYPE *src);
static void char_to_0_pos(DTYPE *dest,DTYPE *src);
static void cvt_from_packed(DTYPE *dest,DTYPE *src);
static void packed_to_double(DTYPE *dest,DTYPE *src);
static void packed_to_edited_zoned(DTYPE *dest,DTYPE *src);
static void packed_to_trailing_zoned(DTYPE *dest,DTYPE *src);
static void packed_to_edited_unsigned(DTYPE *dest,DTYPE *src);
static void packed_to_int(DTYPE *dest,DTYPE *src);
static void packed_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void yes_no_to_int(DTYPE *dest,DTYPE *src);
static void non_blank_to_int(DTYPE *dest,DTYPE *src);
static void cvt_from_literal(DTYPE *dest,DTYPE *src);
static void literal_to_str(DTYPE *dest,DTYPE *src);
static void literal_to_trunc(DTYPE *dest,DTYPE *src);
static void literal_to_trailing_zoned(DTYPE *dest,DTYPE *src);
static void literal_to_edited_unsigned(DTYPE *dest,DTYPE *src);
static void literal_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void cvt_from_format_index(DTYPE *dest,DTYPE *src);
static void format_index_to_int(DTYPE *dest,DTYPE *src);
static void format_index_to_one(DTYPE *dest,DTYPE *src);
static void control_type_to_dtype(DTYPE *dest,DTYPE *src);
static void cvt_from_zoned(DTYPE *dest,DTYPE *src);
static void zoned_to_double(DTYPE *dest,DTYPE *src);
static void zoned_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void zoned_to_edited_zoned(DTYPE *dest,DTYPE *src);
static void zoned_to_edited_unsigned(DTYPE *dest,DTYPE *src);
static void zoned_to_trailing_zoned(DTYPE *dest,DTYPE *src);
static void cvt_from_unsigned(DTYPE *dest,DTYPE *src);
static void unsigned_to_double(DTYPE *dest,DTYPE *src);
static void unsigned_to_unsigned_ascii(DTYPE *dest,DTYPE *src);
static void unsigned_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void unsigned_to_edited_unsigned(DTYPE *dest,DTYPE *src);
static void cvt_from_unsigned_ascii(DTYPE *dest,DTYPE *src);
static void unsigned_ascii_to_unsigned(DTYPE *dest,DTYPE *src);
static void unsigned_ascii_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void u_ascii_to_u_ascii(DTYPE *dest,DTYPE *src);
static void cvt_from_zoned_ascii(DTYPE *dest,DTYPE *src);
static void zoned_ascii_to_zoned(DTYPE *dest,DTYPE *src);
static void zoned_ascii_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void zoned_ascii_to_unsigned_ascii(DTYPE *dest,DTYPE *src);
static void zoned_ascii_to_unsigned(DTYPE *dest,DTYPE *src);
static void zoned_ascii_to_packed(DTYPE *dest,DTYPE *src);
static void zoned_ascii_to_binary(DTYPE *dest,DTYPE *src);
static void cvt_from_binary(DTYPE *dest,DTYPE *src);
static void binary_to_double(DTYPE *dest,DTYPE *src);
static void binary_to_zoned_ascii(DTYPE *dest,DTYPE *src);
static void binary_to_edited_unsigned(DTYPE *dest,DTYPE *src);
static void binary_to_trailing_zoned(DTYPE *dest,DTYPE *src);
static void binary_to_edited_hex(DTYPE *dest,DTYPE *src);
static void nsrc_to_ndest(DTYPE *dest,DTYPE *src);
static void dtoascii(char *dest,double dbl,int len,int dec,int sign_code);
static int machine_order(void);
static void dtop(char *dest,double dbl,int dlen,int ddec);
static void dtoan(char *dest,double dbl,int len,int dec,int dec_exp,int sign_code,int sign_in);
static void eztoz(char *dest,char *src,int dlen,int slen,int sign_code);
static void eztop(char *dest,char *src,int dlen,int slen,int sign_code);
static void eutou(char *dest,char *src,int dlen,int slen);
static void btoza(char *dest,char *src,int dlen,int slen,int sign_code,int type);
static void ptoza(char *dest,char *src,int dlen,int slen);
static void ptoez(char *dest,char *src,int dlen,int slen,int dec,int sign_code);
static void ztoza(char *dest,char *src,int dlen,int slen);
static void ztoez(char *dest,char *src,int dlen,int slen,int dec,int sign_code);
static void zatop(char *dest,char *src,int dlen,int slen,int sign_code);
static int force_nl_sign(char *str,int sign);
static void uatou(char *dest,char *src,int dlen,int slen);
static void utoua(char *dest,char *src,int dlen,int slen);
static void nltotz(char *dest,char *src,int dlen,int slen,int ddec);
static int nltoeu(char *dest,char *src,int dlen,int slen,int ddec);
static void make_h_packed(int ch,char *dest);
static void make_l_packed(int ch,char *dest);
static void insr(char *dest,int ch,int len);
static void insl(char *dest,int ch,int len);
static void utoeu(char *dest,char *src,int dlen,int slen,int dec);
static void ehtob(char *dest,char *src,int dlen,int slen,int type);
static void litcpy(char *dest,char *src,int len);
static void cvt_illegal(DTYPE *dest,DTYPE *src);
static int ptoilen(char *p,int len);
static int format_index_toi(char *src);
static int get_packed_h_num(int ch);
static int get_packed_h_digit(int ch);
static int get_zoned_l_num(int ch);
static int get_packed_l_num(int ch);
static int get_packed_l_digit(int ch);
static int get_packed_l_sign(int ch);
static int get_zoned_sign(int ch);
static int make_zoned_sign(int digit,int sign,char *dest);
static int get_zoned_l_digit(int ch);

static void btoeh(char *dest,char *src,int dlen,int slen,int type);

static void eutob(char *dest,char *src,int dlen,int slen,int sdec,int type);
static void zatou(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
static void zatoua(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
static void zatoza(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
static void zatoz(char *dest,char *src,int dlen,int slen,int sign_code);
static void dbltob(char *dest,double dbl,int len,int type);
static void nltoza(char *dest,char *src,int dlen,int ddec);
static void uatoza(char *dest,char *src,int dlen,int slen,int ddec,int sdec);
static void uatoua(char *dest,char *src,int dlen,int slen,int ddec,int sdec);


void cvt_list(DTYPE *dest,DTYPE *src)
{
	while(src->_len)
		{
		if(src->_len > 0)
			cvt_data(dest,src);
		++dest;
		++src;
		}
}

/*----
Converts all fields in a record. s and d point to an array
of DTYPE items that all relate to the record. The last entry is nulls.
The source pos is assumed to be an offset into the record.
------*/
void cvt_record(DTYPE *dest,DTYPE *src,char *record)
{
	DTYPE nsrc;

	memset(&nsrc,0,sizeof(DTYPE));
	while(src->_len)
		{
		nsrc._len = src->_len;
		nsrc._type = src->_type;
		nsrc._base = record;
		nsrc._pos = src->_pos;
		cvt_data(dest,&nsrc);
		++dest;
		++src;
		}
}


/*----
Converts all fields into a record. s and d point to an array
of DTYPE items that all relate to the record. The last entry is nulls.
The dest pos is assumed to be an offset into the output record.
------*/
void cvt_to_record(DTYPE *d,DTYPE *src,char *record)
{
	DTYPE ndest;

	memset(&ndest,0,sizeof(DTYPE));
	while(src->_len)
		{
		ndest._len = d->_len;
		ndest._type = d->_type;
		ndest._base = record;
		ndest._pos = d->_pos;
		cvt_data(&ndest,src);
		++d;
		++src;
		}
}

/*----
Convert data from one struct to another.
------*/
void cvt_data(DTYPE *dest,DTYPE *src)
{
	if(src->_len < 1)
		return;
	if((src->_type == dest->_type) && 
	   (src->_dec == dest->_dec)   &&
	   (src->_len == dest->_len)    )
		char_to_char(dest,src);
	else
	switch(src->_type)
		{
		case CDBL:
			cvt_from_double(dest,src);
			break;
		case ACHR:
			cvt_from_char(dest,src);
			break;
		case CSTR:
			cvt_from_str(dest,src);
			break;
		case APCK:
			cvt_from_packed(dest,src);
			break;
		case BYES:
			yes_no_to_int(dest,src);
			break;
		case BLNK:
			non_blank_to_int(dest,src);
			break;
		case NLIT:
		case BLIT:
			cvt_from_literal(dest,src);
			break;
		case BIDX:
			cvt_from_format_index(dest,src);
			break;
		case BCTP:
			control_type_to_dtype(dest,src);
			break;
		case AZON:
			cvt_from_zoned(dest,src);
			break;
		case AUNS:
			cvt_from_unsigned(dest,src);
			break;
		case ABMN:
		case ABLH:
		case ABIN:
			cvt_from_binary(dest,src);
			break;
		case BUNS:
			cvt_from_unsigned_ascii(dest,src);
			break;
		case BZON:
			cvt_from_zoned_ascii(dest,src);
			break;
		case DZON:
			cvt_from_edited_zoned(dest,src);
			break;
		case TZON:
			cvt_from_trailing_zoned(dest,src);
			break;
		case DUNS:
			cvt_from_edited_unsigned(dest,src);
			break;
		case DHEX:
			cvt_from_edited_hex(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}

static void cvt_from_double(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case ABIN:
		case ABLH:
		case ABMN:
			double_to_binary(dest,src);
			break;
		case AZON:
			double_to_zoned(dest,src);
			break;
		case APCK:
			double_to_packed(dest,src);
			break;
		case AUNS:
			double_to_unsigned(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}

static void double_to_binary(DTYPE *dest,DTYPE *src)
{
	dbltob(DEST_ADDRESS,SRC_DOUBLE,dest->_len,dest->_type);
}
static void double_to_packed(DTYPE *dest,DTYPE *src)
{
	dtop(DEST_ADDRESS,SRC_DOUBLE,dest->_len,dest->_dec);
}
static void double_to_zoned(DTYPE *dest,DTYPE *src)
{

	dtoan(DEST_ADDRESS,SRC_DOUBLE,dest->_len,dest->_dec,
		0,		/* Implicit decimal */
		BACK_SIGN,	/* Sign code */
		1		/* Sign overpunch */
		);
}
static void double_to_unsigned(DTYPE *dest,DTYPE *src)
{

	dtoan(DEST_ADDRESS,SRC_DOUBLE,dest->_len,dest->_dec,
		0,		/* Implicit decimal */
		NO_SIGN,	/* Sign code */
		0		/* Sign overpunch */
		);
}
static void cvt_from_edited_zoned(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case APCK:
			edited_zoned_to_packed(dest,src);
			break;
		case AZON:
			edited_zoned_to_zoned(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void cvt_from_trailing_zoned(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case APCK:
			trailing_zoned_to_packed(dest,src);
			break;
		case AZON:
			trailing_zoned_to_zoned(dest,src);
			break;
		case ABMN:
		case ABLH:
		case ABIN:
			trailing_zoned_to_binary(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void trailing_zoned_to_binary(DTYPE *dest,DTYPE *src)
{
	eutob(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0,dest->_type);
}

static void edited_zoned_to_zoned(DTYPE *dest,DTYPE *src)
{
	eztoz(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0);
}

static void edited_unsigned_to_zoned(DTYPE *dest,DTYPE *src)
{
	eztoz(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,-1);
}

static void edited_zoned_to_packed(DTYPE *dest,DTYPE *src)
{
	eztop(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0);
}

static void edited_unsigned_to_packed(DTYPE *dest,DTYPE *src)
{
	eztop(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,-1);
}

static void trailing_zoned_to_zoned(DTYPE *dest,DTYPE *src)
{
	eztoz(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,1);
}

static void trailing_zoned_to_packed(DTYPE *dest,DTYPE *src)
{
	eztop(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,1);
}

static void cvt_from_edited_unsigned(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case AUNS:
			edited_unsigned_to_unsigned(dest,src);
			break;
		case APCK:
			edited_unsigned_to_packed(dest,src);
			break;
		case AZON:
			edited_unsigned_to_zoned(dest,src);
			break;
		case ABMN:
		case ABLH:
		case ABIN:
			edited_unsigned_to_binary(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void edited_unsigned_to_unsigned(DTYPE *dest,DTYPE *src)
{
	eutou(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len);
}

static void edited_unsigned_to_binary(DTYPE *dest,DTYPE *src)
{
	eutob(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0,dest->_type);
}

static void cvt_from_edited_hex(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case ABMN:
		case ABLH:
		case ABIN:
			edited_hex_to_binary(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void edited_hex_to_binary(DTYPE *dest,DTYPE *src)
{
	ehtob(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_type);
}


static void cvt_from_str(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case ACHR:
			str_to_char(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}


/*----
COBOL character array conversions.
------*/
static void cvt_from_char(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case ACHR:
			achar_to_achar(dest,src);
			break;
		case CSTR:
			char_to_str(dest,src);
			break;
		case CINT:
			char_to_int(dest,src);
			break;
		case CCHR:
			char_to_cchar(dest,src);
			break;
		case BCHR:
			char_to_bchar(dest,src);
			break;
		case BONE:
			char_to_one(dest,src);
			break;
		case BTRN:
			char_to_trunc(dest,src);
			break;
		case BPOS:
			char_to_0_pos(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}

static void char_to_str(DTYPE *dest,DTYPE *src)
{
	memcpy(DEST_ADDRESS,SRC_ADDRESS,src->_len);
	DEST_ADDRESS[src->_len] = 0;
	dest->_len = src->_len;
}
static void char_to_char(DTYPE *dest,DTYPE *src)
{
	memcpy(DEST_ADDRESS,SRC_ADDRESS,src->_len);
	dest->_len = src->_len;
}
/*----
Character array copy when not of equal length.
------*/
static void achar_to_achar(DTYPE *dest,DTYPE *src)
{
	memset(DEST_ADDRESS,' ',dest->_len);
	memcpy(DEST_ADDRESS,SRC_ADDRESS,min(src->_len,dest->_len));
}
static void str_to_char(DTYPE *dest,DTYPE *src)
{
	int len;
	len = strlen(SRC_ADDRESS);
	++len;
	memcpy(DEST_ADDRESS,SRC_ADDRESS,(min(len,dest->_len)));
	KCSI_unstrunc(DEST_ADDRESS,dest->_len);
}

static void char_to_trunc(DTYPE *dest,DTYPE *src)
{

	char_to_str(dest,src);
	k_trunc(dest);
}
static void k_trunc(DTYPE *dest)
{
	KCSI_strunc(DEST_ADDRESS);
	dest->_len = strlen(DEST_ADDRESS);
}

static void char_to_int(DTYPE *dest,DTYPE *src)
{
	DEST_INT = kcsi_atoilen(SRC_ADDRESS,src->_len);
	dest->_len = sizeof(int);
}

/*----
Single character converted to an int, SPACE is converted to 0.
------*/
static void char_to_cchar(DTYPE *dest,DTYPE *src)
{
	char_to_bchar(dest,src);
	if(DEST_INT == 32)
		DEST_INT = 0;
}

/*----
Single character converted to an int, SPACE is not converted to 0.
------*/
static void char_to_bchar(DTYPE *dest,DTYPE *src)
{
	DEST_INT = *SRC_ADDRESS;
	dest->_len = sizeof(int);
}


/* Must be 1 if < 1 */
static void char_to_one(DTYPE *dest,DTYPE *src)
{
	char_to_int(dest,src);

	if(DEST_INT < 1)
		DEST_INT = 1;
}
static void char_to_0_pos(DTYPE *dest,DTYPE *src)
{
	char_to_int(dest,src);

	--DEST_INT;
}

/*----
COBOL packed conversions.
------*/
static void cvt_from_packed(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case CDBL:
			packed_to_double(dest,src);
			break;
		case CINT:
			packed_to_int(dest,src);
			break;
		case BZON:
			packed_to_zoned_ascii(dest,src);
			break;
		case DZON:
			packed_to_edited_zoned(dest,src);
			break;
		case TZON:
			packed_to_trailing_zoned(dest,src);
			break;
		case DUNS:
			packed_to_edited_unsigned(dest,src);
			break;
		case ABMN:
		case ABLH:
		case ABIN:
		case APCK:
		case AZON:
		case AUNS:
			nsrc_to_ndest(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}

static void packed_to_double(DTYPE *dest,DTYPE *src)
{
	DEST_DOUBLE = ptod(SRC_ADDRESS,src->_len,src->_dec);
}
static void packed_to_edited_zoned(DTYPE *dest,DTYPE *src)
{
	ptoez(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,0);
}
static void packed_to_trailing_zoned(DTYPE *dest,DTYPE *src)
{
	ptoez(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,1);
}
static void packed_to_edited_unsigned(DTYPE *dest,DTYPE *src)
{
	ptoez(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,-1);
}
static void packed_to_int(DTYPE *dest,DTYPE *src)
{
	DEST_INT = ptoilen(SRC_ADDRESS,src->_len);
	dest->_len = sizeof(int);
}
static void packed_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
/*
	ptoza(DEST_ADDRESS,SRC_ADDRESS,BZON_LEN,src->_len);
	dest->_len = BZON_LEN;
*/
	ptoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len);
}


/*----
There are several fields that are YES NO or
other fields. The the other is usually a positive integer. This conversion
assigns YES to -1 NO = 0 and other = -2. A -2 return would indicate
that some other type of data is in the field.
------*/
static void yes_no_to_int(DTYPE *dest,DTYPE *src)
{
	if(dest->_type != CINT)
		{
		cvt_illegal(dest,src);
		return;
		}
	if(*SRC_ADDRESS == 'Y')
		DEST_INT = -1;
	else
	if(*SRC_ADDRESS == 'N')
		DEST_INT = 0;
	else
		DEST_INT = -2;
	dest->_len = sizeof(int);
}

/*----
Blank = FALSE, non-BLANK = true
------*/
static void non_blank_to_int(DTYPE *dest,DTYPE *src)
{
	if(dest->_type != CINT)
		{
		cvt_illegal(dest,src);
		return;
		}
	if(*SRC_ADDRESS > ' ')
		DEST_INT = -1;
	else
		DEST_INT = 0;
	dest->_len = sizeof(int);
}
/*----
Literal conversions.
------*/
static void cvt_from_literal(DTYPE *dest,DTYPE *src)
{
	char lwork[101];
	DTYPE tdest;

	memset(&tdest,0,sizeof(DTYPE));
	tdest._base = lwork;
	tdest._pos = 0;
	tdest._len = 16;
	tdest._type = TZON;
	tdest._dec = 0;
	switch(dest->_type)
		{
		case CSTR:
			literal_to_str(dest,src);
			break;
		case BTRN:
			literal_to_trunc(dest,src);
			break;
		case TZON:
			literal_to_trailing_zoned(dest,src);
			break;
		case BZON:
			literal_to_zoned_ascii(dest,src);
			break;
		case DUNS:
			literal_to_edited_unsigned(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void literal_to_str(DTYPE *dest,DTYPE *src)
{
	litcpy(DEST_ADDRESS,SRC_ADDRESS,src->_len);
	dest->_len = strlen(DEST_ADDRESS);
}
static void literal_to_trunc(DTYPE *dest,DTYPE *src)
{
	literal_to_str(dest,src);
	k_trunc(dest);
}
static void literal_to_trailing_zoned(DTYPE *dest,DTYPE *src)
{
	nltotz(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec);
}
static void literal_to_edited_unsigned(DTYPE *dest,DTYPE *src)
{
	nltoeu(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec);
}

static void literal_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	nltoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,dest->_dec);
}


static void cvt_from_format_index(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case CINT:
			format_index_to_int(dest,src);
			break;
		case BONE:
			format_index_to_one(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
/*----
The format index data type is 4 characters containing a 0 thru 99
index enclosed by optional parenthesis.
"(01)" or "(1)" or "1" or "01"  are all legal
------*/
static void format_index_to_int(DTYPE *dest,DTYPE *src)
{
	DEST_INT = format_index_toi(SRC_ADDRESS);
	dest->_len = sizeof(int);
}
static void format_index_to_one(DTYPE *dest,DTYPE *src)
{
	format_index_to_int(dest,src);
	if(DEST_INT < 1)
		DEST_INT = 1;
}

static void control_type_to_dtype(DTYPE *dest,DTYPE *src)
{
	int r;

	if(dest->_type != BDTP)
		{
		cvt_illegal(dest,src);
		return;
		}
	r = 0;
	switch(*SRC_ADDRESS)
		{
		case 'P':
			r = APCK; break;
		case 'C':
			r = ACHR; break;
		case 'L':
			r = BLIT; break;
		case 'Z':
			r = AZON; break;
		case 'B':
/* Default for most conversions */
			r = ABIN;
#ifdef KCSI_MFX
			r = ABMN;
#endif 
			break;
		case 'U':
			r = AUNS; break;
		}
	DEST_INT = r;
}

/*----
Zoned conversions.
------*/
static void cvt_from_zoned(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case CDBL:
			zoned_to_double(dest,src);
			break;
		case DZON:
			zoned_to_edited_zoned(dest,src);
			break;
		case TZON:
			zoned_to_trailing_zoned(dest,src);
			break;
		case BZON:
			zoned_to_zoned_ascii(dest,src);
			break;
		case DUNS:
			zoned_to_edited_unsigned(dest,src);
			break;
		case ABIN:
		case APCK:
		case AZON:
		case AUNS:
			nsrc_to_ndest(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void zoned_to_double(DTYPE *dest,DTYPE *src)
{
	DEST_DOUBLE = antod(SRC_ADDRESS,src->_len,src->_dec);
}
static void zoned_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	ztoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len);
}
static void zoned_to_edited_zoned(DTYPE *dest,DTYPE *src)
{
	ztoez(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,0);
}
static void zoned_to_edited_unsigned(DTYPE *dest,DTYPE *src)
{
	ztoez(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,-1);
}
static void zoned_to_trailing_zoned(DTYPE *dest,DTYPE *src)
{
	ztoez(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,1);
}
/*----
Unsigned conversions.
------*/
static void cvt_from_unsigned(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case CDBL:
			unsigned_to_double(dest,src);
			break;
		case BZON:
			unsigned_to_zoned_ascii(dest,src);
			break;
		case BUNS:
			unsigned_to_unsigned_ascii(dest,src);
			break;
		case DUNS:
			unsigned_to_edited_unsigned(dest,src);
			break;
		case ABIN:
		case APCK:
		case AZON:
		case AUNS:
			nsrc_to_ndest(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void unsigned_to_double(DTYPE *dest,DTYPE *src)
{
	DEST_DOUBLE=untod(SRC_ADDRESS,src->_len,src->_dec);
}

static void unsigned_to_unsigned_ascii(DTYPE *dest,DTYPE *src)
{
	utoua(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len);
}

static void unsigned_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	ztoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len);
}

static void unsigned_to_edited_unsigned(DTYPE *dest,DTYPE *src)
{
	utoeu(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec);
}
static void cvt_from_unsigned_ascii(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case BZON:
			unsigned_ascii_to_zoned_ascii(dest,src);
			break;
		case BUNS:
			u_ascii_to_u_ascii(dest,src);
			break;
		case AUNS:
			unsigned_ascii_to_unsigned(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void unsigned_ascii_to_unsigned(DTYPE *dest,DTYPE *src)
{
	uatou(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len);
}
static void unsigned_ascii_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	uatoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec,src->_dec);
}
static void u_ascii_to_u_ascii(DTYPE *dest,DTYPE *src)
{
	uatoua(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec,src->_dec);
}
static void cvt_from_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case BZON:
			zoned_ascii_to_zoned_ascii(dest,src);
			break;
		case BUNS:
			zoned_ascii_to_unsigned_ascii(dest,src);
			break;
		case AUNS:
			zoned_ascii_to_unsigned(dest,src);
			break;
		case AZON:
			zoned_ascii_to_zoned(dest,src);
			break;
		case APCK:
			zoned_ascii_to_packed(dest,src);
			break;
		case ABMN:
		case ABLH:
		case ABIN:
			zoned_ascii_to_binary(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}
static void zoned_ascii_to_zoned(DTYPE *dest,DTYPE *src)
{
	zatoz(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0);
}
/*----
Assumes a difference in decimals.
------*/
static void zoned_ascii_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	zatoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec,src->_dec);
}

static void zoned_ascii_to_unsigned_ascii(DTYPE *dest,DTYPE *src)
{
	zatoua(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec,src->_dec);
}

static void zoned_ascii_to_unsigned(DTYPE *dest,DTYPE *src)
{
	zatou(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,dest->_dec,src->_dec);
}

static void zoned_ascii_to_packed(DTYPE *dest,DTYPE *src)
{
	zatop(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0);
}

static void zoned_ascii_to_binary(DTYPE *dest,DTYPE *src)
{
	eutob(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_dec,dest->_type);
}

/*----
Binary conversions.
------*/
static void cvt_from_binary(DTYPE *dest,DTYPE *src)
{
	switch(dest->_type)
		{
		case CDBL:
			binary_to_double(dest,src);
			break;
		case BZON:
			binary_to_zoned_ascii(dest,src);
			break;
		case TZON:
			binary_to_trailing_zoned(dest,src);
			break;
		case DUNS:
			binary_to_edited_unsigned(dest,src);
			break;
		case DHEX:
			binary_to_edited_hex(dest,src);
			break;
		case ABIN:
		case APCK:
		case AZON:
		case AUNS:
			nsrc_to_ndest(dest,src);
			break;
		default:
			cvt_illegal(dest,src);
			break;
		}
}

static void binary_to_double(DTYPE *dest,DTYPE *src)
{
	DEST_DOUBLE = btod(SRC_ADDRESS, src->_len, src->_type);
}

/*----
This is machine spcific based on the assumption that a long is
four bytes long.
------*/
static void binary_to_zoned_ascii(DTYPE *dest,DTYPE *src)
{
	btoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,0,src->_type);

}

static void binary_to_edited_unsigned(DTYPE *dest,DTYPE *src)
{
	btoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,-1,src->_type);

}

static void binary_to_trailing_zoned(DTYPE *dest,DTYPE *src)
{
	btoza(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,1,src->_type);

}

static void binary_to_edited_hex(DTYPE *dest,DTYPE *src)
{
	btoeh(DEST_ADDRESS,SRC_ADDRESS,dest->_len,src->_len,src->_type);

}

/*----
Converts numerics by going thru a common type BZON.
Requires that conversions be available in cvt_data from src->_type
to BZON and from BZON to dest->_type;
------*/
static void nsrc_to_ndest(DTYPE *dest,DTYPE *src)
{
	double dbl;

	DTYPE work;

	memset(&work,0,sizeof(DTYPE));

	work._base = (char*)&dbl;
	work._pos = 0;
	work._type = CDBL;
	work._len = sizeof(double);
	work._dec = src->_dec;
	cvt_data(&work,src);
	cvt_data(dest,&work);
}

/*----
General conversion utilities.
------*/
/*----
These were brought back from the improved dcvt routines and modified
to fit this situation
------*/
static double btod(char *src,int slen,int type)
{
	long temp;
	double result;

	temp=btol(src,slen,type);
	result = temp;
	return(result);
}

static long btol(char *src,int slen,int type)
{
	int dtype, srcinc, len, destinc,byte ;
	long result;
	char *dest;

	
	dtype = machine_order();
	if(type == ABMN)		/* Machine order */
		type = dtype;

	switch(type)
		{
		case ABHL:		/* high to low*/
			srcinc = -1;
			src += slen;
			--src;
			break;
		case ABLH:		/* low to high*/
			srcinc = 1;
			break;
		}

	result = 0;

	dest = (char*) &result;

	switch(dtype)
		{
		case ABHL:
			destinc = -1;
			dest += 3;
			break;
		case ABLH:
			destinc = 1;
			break;
		}
	for(len = 0; len < sizeof(long); ++len,dest += destinc)
		{
		byte = *src;
		byte &= 0xff;
		if(slen == 0)
			{
			if(byte & 0x80)		/* we have a sign to extend */
				byte = 0xff;
			else
				byte = 0;
			}
		*dest = byte;
		if(slen > 0)
			{
			--slen;
			src += srcinc;
			if(slen == 0)		/* at end of src so keep */
				src -= srcinc;	/* pointing to high order byte*/
			}
		}

	return(result);
}

static int machine_order()
{
	long test;
	char *tests;

	test = 1;
	
	tests = (char*) &test;

	if(*tests == 1)
		return(ABLH);
	else
		return(ABHL);
}


/*----
This has to set up a binary in the correct order
------*/
static void dbltob(char *dest,double dbl,int len,int type)
{
	int destinc;
	long hex;
	int byte;

	if(type == ABMN)		/* Machine order */
		type = machine_order();
	switch(type)
		{
		case ABHL:		/* High to low */
			destinc = -1;
			dest += len;
			--dest;
			break;
		case ABLH:		/* Low to high */
			destinc = 1;
			break;
		}
	hex = (long)dbl;
	for( ;len;--len, dest += destinc)
		{
		byte = hex & 0x000000ff;
		hex >>= 8;
		*dest = byte;
		}
}


/*----
Convert packed to double.
------*/
static double ptod(char *src,int slen,int dec)
{
	double result;
	int num,high_digit;

	result = 0;

	slen *= 2;
/*
 * Step through the field making a digit from
 * each nybble except the last which is sign.
 */
	for(high_digit = 1; slen ; --slen)
		{
		if(slen == 1)
			{
			if(( (*src) & (0x0f) ) == 0x0d)
				result *= -1;
			break;
			}
		else
		if(high_digit)		/* If high then step source back one*/
			{
			num = get_packed_h_num(*src);
			}
		else
			{
			num = get_packed_l_num(*src++);
			}
		result *= 10;
		result += num;
		high_digit = !high_digit;
		}

	while(dec)
		{
		result /= 10;
		--dec;
		}
	return(result);
}

/*----
Convert double_to_packed
------*/
static void dtop(char *dest,double dbl,int dlen,int ddec)
{
	int hch,lch;
	char *src;
	char wrk_str[101];

	while(ddec)
		{
		dbl *= 10;
		--ddec;
		}

/* the temporary field created will be 2 times the final packed field */
	dtoascii(wrk_str,dbl,(dlen * 2),0,BACK_SIGN);
/* 
 * We now have the bugger unformatted with a sign
 * cram 2 digits into each byte
 */
	src = wrk_str;

	while(*src)
		{
		hch = *src++;
		lch = *src++;
		hch &= 0x0f;
		hch <<= 4;
		if(lch == '-')
			lch = 0x0d;
		else
		if(lch == '+')
#ifdef	PACKED_C_POSITIVE
			lch = 0x0c;
#else
			lch = 0x0f;
#endif	/* PACKED_C_POSITIVE */
		else
			lch &= 0x0f;
		lch |= hch;
		*dest = lch;
		++dest;
		}
}

static void dtoascii(char *dest,double dbl,int len,int dec,int sign_code)
{
	static char fformat[20] = "%%s%%0%d.%df%%s";
	char format[20];
	int wlen;

	char lsign[2],tsign[2];

/* First create leading and trailing signs */
	if(dbl < 0.0)
		{
		strcpy(lsign,"-");
		strcpy(tsign,"-");
		dbl *= -1;
		}
	else
		{
		strcpy(lsign,"+");
		strcpy(tsign,"+");
		}
/* Then decide which ones will be used (or neither) */
	if(sign_code == FRONT_SIGN)
		{
		strcpy(tsign,"");
		}
	else
	if(sign_code == BACK_SIGN)
		{
		strcpy(lsign,"");
		}
	else
		{
		strcpy(tsign,"");
		strcpy(lsign,"");
		}
/* Create the format string */
	sprintf(format,fformat,len,dec);
/* Print the number */
	sprintf(dest,format,lsign,dbl,tsign);
/* Force the string to be the right size */
	wlen = strlen(dest);
	if(FRONT_SIGN)
		{
		++dest;
		--len;
		--wlen;
		}
	while(wlen > len)
		{
		strcpy(dest,(dest + 1));
		--wlen;
		}
	
}

static double untod(char *src,int slen,int pdec)
{
	double result;

	result = antod(src,slen,pdec);
	if(result < 0.0)
		result *= -1;
	return(result);
}
/*----
Convert ASCII numerics to double. This is a real loose conversion
for input. It accepts just about any characters and tries to make
sense of them. Accepts sign overpunch or displayable sign types
and decimal points.
------*/
static double antod(char *src,int slen,int pdec)
{
	int sign,ch,dec;
	double result;

	result = 0.0;

	sign = 1;
	dec = -1;

	while(slen)
		{
/* If its a character, just use it */
		ch = (*src);
		ch &= 0xff;
		if(isdigit(ch))
			{
			result *= 10;
			result += (ch - '0');
			if(dec != -1)
				++dec;
			}
		else
/* If its a sign use it for sign value */
		if( ch == '+' )
			;
		else
		if(ch == '-')
			sign = -1;
		else
/* If its a decimal just start counting decimals */
		if(ch == '.')
			{
			if(dec == -1)
				dec = 0;
			}
		else
/* Ignore COBOL editing chars */
		if(ch == '$')
			;
		else
		if(ch == ',')
			;
		else
		if(ch == '*')
			;
		else
		if(ch == ' ')
			;
		else
/* Handle COBOL CR and DB signs */
		if(ch == 'D')
			{
			if( ( (* src) & 0xff) == 'B')
				{
				sign = -1;
				++src;
				--slen;
				}
			}
		else
		if( ch == 'C')
			{
			if( ( (* src) & 0xff) == 'R')
				{
				sign = -1;
				++src;
				--slen;
				}
			}
/* Assume some sort of packing */
		else
			{
			ch = get_zoned_l_num(ch);
			result *= 10;
			result += ch;
			if(dec != -1)
				++dec;
			ch = (*src) & 0xff;
			ch = get_zoned_sign(ch);
			if(ch == '-')
				sign = -1;
			else
				sign = 1;
			}
		++src;
		--slen;
		}
/* 
 * The only conflict will be around decimals. If a decimal is passed
 * and one was counted then use the counted one.
 */
	if(dec == -1)
		dec = pdec;
	while(dec--)
		result /= 10;

	result *= sign;
	
	return(result);
			
}

static void dtoan(char *dest,double dbl,int len,int dec,int dec_exp,int sign_code,int sign_in)
{
	int digit,sign;
	int tlen;
	char *wdest,*sdest;
	char wrk_str[101];

/* 
 * If the dec is implicit then 
 * adjust the decimal
 */
	if(!dec_exp)
		{
		while(dec)
			{
			dbl *= 10;
			--dec;
			}
		}

/* If the sign is to be embedded then temp work will be +1 longer */
	tlen = len;
	if(sign_in)
		++tlen;

/* 
 * Do we have a sign and where will it be. sdest points to the sign
 * at begininng or end
 */
	sdest = wdest = wrk_str;
	if(sign_code == BACK_SIGN)
		{
		sdest = wdest + tlen;
		--sdest;
		}

/* Convert to a string */

	dtoascii(wrk_str,dbl,tlen,dec,sign_code);

/*
 * Pick up the sign and the digit for combination. In the course
 * of this, sdest ends up pointing to the target for the
 * combined field
 * wdest which points to the final string is incremented
 * for a leading embedded sign so that it
 * points to the right place.
 */
	if(sign_code == BACK_SIGN)
		{
		sign = *sdest--;
		digit = *sdest;
		}
	if(sign_code == FRONT_SIGN)
		{
		sign = *sdest++;
		digit = *sdest;
		if(sign_in)
			++wdest;
		}
	if(sign_code)
		{
		if(sign_in)
			{
			*sdest = make_zoned_sign(digit,sign,sdest);
			}
		}
/* Finally move it to its resting place */
	memcpy(dest,wdest,len);

}

/*----
These edit conversion routines are sufficient for now, because there will be a
consistency in decimal alignment for src and dest, but they will
need to be changed for a full blown effort.
------*/
static void eztoz(char *dest,char *src,int dlen,int slen,int sign_code)
{
	zatoz(dest,src,dlen,slen,sign_code);
}
static void eztop(char *dest,char *src,int dlen,int slen,int sign_code)
{
	zatop(dest,src,dlen,slen,sign_code);
	
}
static void eutou(char *dest,char *src,int dlen,int slen)
{
	uatou(dest,src,dlen,slen);

}

/*----
Convert binary to zoned ascii.
Machine specific, assumes a long is four bytes.
The result is not null terminated
------*/
static void btoza(char *dest,char *src,int dlen,int slen,int sign_code,int type)
{
	long result;
	int inc;
	char format[10];
	char buf[32],*bufptr;

	inc = 1;


	result = btol(src,slen,type);

/*
 * Copy in all but the sign
 */
	bufptr = buf;
	memset(bufptr,0,32);
	sprintf(format,"%%+0%dld",dlen);
	sprintf(bufptr,format,result);
	if((sign_code == 1) || (sign_code == -1))
		{
		++bufptr;
		--dlen;
		}
	memcpy(dest,bufptr,dlen);
/*
 * Tack on a trailing sign if indicated
 */

	if(sign_code == 1)
		{
		--bufptr;
		dest[dlen] = *bufptr;
		}
}
/*----
Convert packed to zoned ASCII.
------*/
static void ptoza(char *dest,char *src,int dlen,int slen)
{
	ptoez(dest,src,dlen,slen,0,0);
}

/*----
Convert packed to zoned ASCII.
------*/
static void ptoez(char *dest,char *src,int dlen,int slen,int dec,int sign_code)
{
	int ch,high_digit;
	static char dummy[] = {0x0f};

	if(dlen < 1)			/* Can't convert */
		return;
	if(dlen < 2)
		*dest = '0';		/* A compromise */
	if(slen < 1)
		{
		slen = 1;
		src = dummy;
		}
	if(dec < 1)
		dec = -1;

	src += (slen - 1);		/*Last byte of src*/
/*
 * extract the sign and store it
 */
	ch = get_packed_l_sign(*src);
	if(ch != '-')
		ch = '+';
/*
 * Sign goes to the start or end of the field
 */
	if(sign_code == 1)
		{
		dest += (dlen - 1);
		*dest-- = ch;
		--dlen;
		}
	else
	if(sign_code == 0)
		{
		*dest = ch;
		dest += (dlen - 1);
		--dlen;
		}
	else
	if(sign_code == -1)		/*nosign*/
		{
		dest += (dlen - 1);
		}
	
/*
 * Step backward through the field making a digit from
 * each nybble.
 */
	high_digit = 1;
	while( (slen) || (dlen))
		{
		if(dlen == 0)
			break;		/* out of dest */
		if(dec == 0)		/* We hit the decimal pos */
			{
			*dest-- = '.';
			}
		else
		if(!slen)		/* we're out of source so pad it */
			{
			*dest-- = '0';
			}
		else			/*High or low nybble & toggle flag*/
		if(high_digit)		/* If high then step source back one*/
			{
			*dest-- = get_packed_h_digit(*src--);
			--slen;
			high_digit = !high_digit;
			}
		else
			{
			*dest-- = get_packed_l_digit(*src);
			high_digit = !high_digit;
			}
		--dlen;
		--dec;
		}
}


/*----
Convert zoned to zoned ASCII.
------*/
static void ztoza(char *dest,char *src,int dlen,int slen)
{
	ztoez(dest,src,dlen,slen,0,0);	/* Like edited but zero decimals */
}

/*----
Convert zoned to zoned edited. Convert as 
unsigned then add the sign.
------*/
static void ztoez(char *dest,char *src,int dlen,int slen,int dec,int sign_code)
{

	int ch;

	if(sign_code != -1)
		--dlen;
	if((sign_code == 1) || (sign_code == -1 ))
		utoeu(dest,src,dlen,slen,dec);
	else
		utoeu(dest + 1,src,dlen,slen,dec);
	src += (slen - 1);
/*
 * extract the sign and store it
 */
	ch = get_zoned_sign(*src);
	if(ch != '-')
		ch = '+';
	if(sign_code == 1)
		dest += dlen;
	if(sign_code != -1)
		*dest = ch;

}

/*----
Convert zoned ASCII to packed.
------*/
static void zatop(char *dest,char *src,int dlen,int slen,int sign_code)
{
	int high_digit;
	static char dummy[] = "+0";
	char *save_sign;
	int sign;

	if( (dlen < 1) )
		return;

	if(slen < 1)
		{
		slen = 2;
		src = dummy;
		}
	save_sign = src;		/*catch sign at the start*/
	if(sign_code == 1)		/* or end if needed */
		save_sign += (slen - 1);
	sign = *save_sign;		/* should be the sign */
	if(sign != '-')			/*make it one or t'uther*/
		sign = '+';
	if(!isdigit((int)*save_sign))	/*If there is a sign then shorten*/
		--slen;
	dest += (dlen - 1);		/* save sign at the end of dest */
	make_l_packed(sign,dest);	/* into the low nybble */

	src += slen - 1;		/*end of src*/
	high_digit = 1;			/*set digit flag*/
	while((slen) || (dlen))
		{
		if(!dlen)		/* we ran out of dest so truncate*/
			break;
		if(slen == 0)		/* we ran out of source so pad */
			{
			src = &dummy[1];
			++slen;
			}
		if(*src == '.')		/*Skip any decimal (quickie for edited)*/
			{
			--src;
			}
		else			/*Process one or the other half*/
		if(high_digit)
			{
			make_h_packed(*src--,dest--);
			--dlen;
			high_digit = !high_digit;
			}
		else
			{
			make_l_packed(*src--,dest);
			high_digit = !high_digit;
			}
		--slen;
		}
}

/*----
Converted numeric lit to zoned ASCII
-------*/
static void nltoza(char *dest,char *src,int dlen,int ddec)
{
	int sign;
	double result;
	char format[15];

	sign = force_nl_sign(src,'+');
	result = 0.0;

	sscanf(src,"%lf",&result);
	while(ddec)
		{
		result *= 10.0;
		--ddec;
		}
	if(sign == '-')
		result *= -1;
	sprintf(format,"%%+0%d.0lf",dlen);
	sprintf(dest,format,result);
	force_nl_sign(src,sign);
}

/*----
Search a lit for a sign. If found replace with passed sign and
return the original sign.
------*/
static int force_nl_sign(char *str,int sign)
{
	int osign,maxl;
	
	osign = '+';

	for(maxl = 0; maxl < 16; ++maxl,++str)
		{
		if( (*str == '-') || (*str == '+') )
			{
			osign = *str;
			*str = sign;
			break;
			}
		else
		if(*str == '.')
			continue;
		else
		if(isdigit((int)*str))
			continue;
		else
			break;
		}
	return(osign);
}


/*----
Convert zoned ASCII to zoned ASCII. (Should also work for unsigned to zoned).
Do an unsigned conversion and then add the sign. These conversions
assume that decimal places do not match.
------*/
static void zatoza(char *dest,char *src,int dlen,int slen,int ddec,int sdec)
{
	int sign;

	sign = *src;
	if(sign != '-')
		sign = '+';
	if(!isdigit((int)*src))
		{
		++src;
		--slen;
		}
	uatoua(dest + 1,src,dlen - 1,slen,ddec,sdec);
	if(dlen > 0)
		{
		*dest = sign;
		}

}
static void uatoza(char *dest,char *src,int dlen,int slen,int ddec,int sdec)
{

	if(!isdigit((int)*src))
		{
		++src;
		--slen;
		}
	uatoua(dest + 1,src,dlen - 1,slen,ddec,sdec);
	if(dlen > 0)
		*dest = '+';

}
static void zatoua(char *dest,char *src,int dlen,int slen,int ddec,int sdec)
{

	if(!isdigit((int)*src))
		{
		++src;
		--slen;
		}
	uatoua(dest,src,dlen,slen,ddec,sdec);

}
/*----
This routine is basically adjusting the source field to
lie correctly within the dest field. It requires two adjustments
One to handle a difference in sizes and the other to handle a difference
in decimality.
This is done by adjusting a start and end pointer over the src field.
The pointer is allowed  to move before the beginning or after the end
of src. Data is pulled from src by testing where the pointer is.
------*/

static void uatoua(char *dest,char *src,int dlen,int slen,int ddec,int sdec)
{
	char *start,*end;

	start = src;
	end = src + (slen - 1);

/*Decimal adjustment executes only one or the other*/
	while(sdec > ddec)
		{
		--sdec;
		--slen;
		}
	while(sdec < ddec)
		{
		++sdec;
		++slen;
		}
/* Length adjustment executes only one or the other */
	while(slen > dlen)
		{
		++start;
		--slen;
		}
	while(slen < dlen)
		{
		--start;
		++slen;
		}
/* The start and end pointers could be outside either limit of the original
 * src field so the copy is done with that it mind.
 */
	while(dlen)
		{
		if( (start < src) || (start > end) )
			*dest = '0';
		else
			*dest = *start;
		++start;
		++dest;
		--dlen;
		}
}



/*----
Convert zoned ASCII to zoned. (Should also work for unsigned to zoned).
Do an unsigned conversion and then add the sign.
------*/
static void zatoz(char *dest,char *src,int dlen,int slen,int sign_code)
{
	int sign;

	char *save_sign;

	save_sign = src;			/*point to sign at start*/

	if(sign_code == 1)		/* or end if necessary */
		save_sign += (slen - 1);
	sign = *save_sign;		/* create the sign */
	if(sign != '-')
		sign = '+';
	if(!isdigit((int)*save_sign))	/* If it is a sign */
		{
		--slen;
		if(!sign_code)	/* Skip it if not trailing */
			++src;
		}
	uatou(dest,src,dlen,slen);

	if(dlen > 0)			/* Add the sign */
		{
		dest += (dlen - 1);
		make_zoned_sign(*dest,sign,dest);
		}
}
/*----
Zoned ascii to unsigned
Skip any initial sign.
------*/
static void zatou(char *dest,char *src,int dlen,int slen,int ddec,int sdec)
{
	if(!(isdigit((int)*src)))
		{
		++src;
		--slen;
		}
	uatoua(dest,src,dlen,slen,ddec,sdec);
}


/*----
Unsigned ASCII to unsigned.
------*/
static void uatou(char *dest,char *src,int dlen,int slen)
{
	static char dummy[] = "0";

	if( (dlen < 1) )
		return;

	if(slen < 1)
		{
		slen = 2;
		src = dummy;
		}
					/* Force positive */
					/* should be the sign */
					/* at the end of dest */
					/* last digit into the low nybble*/
	dest += (dlen - 1);

	src += (slen - 1);
/* No sign output at all */
/*
	make_h_nybble('+',dest);
	make_l_nybble(*src,dest);
*/
/* And so no need to step back */
/*
	--slen;
	--dlen;
	--src;
	--dest;
*/
	while((slen) || (dlen))
		{
		if(!dlen)		/* we ran out of dest so truncate*/
			break;
		if(slen == 0)		/* We're out of source */
			{
			*dest-- = '0';	/* no source so zero fill */
			--dlen;
			}
		else
		if(!isdigit((int)*src))	/*Skip any non numerics */
			{
			--src;
			--slen;
			}
		else
			{
			*dest-- = *src--;
			--slen;
			--dlen;
			}
		}
}

/*----
Convert unsigned to unsigned ASCII. Same as unsigned edited, but force
zero decimals.
------*/
static void utoua(char *dest,char *src,int dlen,int slen)
{
	utoeu(dest,src,dlen,slen,0);
}

/*----
Converted unformatted numeric lit to edited sign trailing
------*/
static void nltotz(char *dest,char *src,int dlen,int slen,int ddec)
{
	int sign;

	sign = nltoeu(dest,src,dlen - 1,slen,ddec);
	if(sign != '-')
		sign = '+';
	*(dest + (dlen - 1)) = sign;

}
/*----
Convert unformatted numeric literal to edited unsigned.
Returns a sign if one is found.
------*/
static int nltoeu(char *dest,char *src,int dlen,int slen,int ddec)
{
	int sign,ldec,rdec,iddec,isdec,ch;
	char *dstart,*last_digit;

	sign = iddec = isdec = 0;
	dstart = dest;

	if(dlen < 1)			/* No dest */
		return(sign);
	if(ddec >= dlen)		/* Too many decimals */
		ddec = dlen - 1;
	memset(dest,'0',dlen);
	if((ddec) &&(dlen < 2))		/* Not enough dest with decimals*/
		return(sign);

	rdec = ddec;			/*right of the decimal*/
	ldec = dlen;			/*left of the decimal*/
	if(rdec)
		ldec -= (rdec + 1);	/*one position for the decimal*/
	if(rdec)
		dest[ldec] = '.';	/* Drop in the decimal*/

/*
 * This routine must match numerous conditions as it marches through
 * the src field. The character is selected from src and then must
 * be placed in dest. Dest is left or right shifted around the decimal
 * to account for the incoming data.
 */
	last_digit = NULL;
	while(slen || dlen)
		{
/* Decimal in dest (or end of dest). Skip it,set flag for in decimals */
		if((!dlen) || (*dest == '.'))
			{
			iddec = 1;
			if(last_digit == NULL)
				last_digit = (dest - 1);
			if(dlen)
				{
				++dest;
				--dlen;
				}
			}
/* Source is exhausted so use a '0' pad and flag src dec as on*/
		if(slen)
			ch = *src;
		else
			{
			ch = '0';
			isdec = 1;
			}
/*
 * Handle a digit.
 * 1. If both fields are on the same side of the decimal then drop it in.
 * 2. If src field is still left then there are more incoming digits than
 *    will fit in dest left of the decimal. The left hand side of dest is
 *    rolled left and high order digits are pushed off the left side. Dest
 *    will be pointing to the first digit right of the decimal. And the
 *    left shift only occurs if there are digits left of the dec. dest
 *    is backed up one.
 * 3. If dest field is still left then the left hand side of src was too
 *    short. dest is padded by adding '0' at the front of the field and
 *    src is backed up one.
 */
		if(isdigit(ch))
			{
			if(iddec == isdec)	/* 1 */
				{
				if(dlen)
					*dest = ch;
				}
			else
			if(iddec)		/* 2 */
				{
				if(ldec)
					insl(last_digit,ch,ldec);
				--dest;
				++dlen;
				}
			else
			if(isdec)		/* 3 */
				{
				if(ldec)
					insr(dstart,'0',ldec);
				--src;
				++slen;
				}
			}
/*
 * Non digits do not cause output to dest so dest is backed up one. Sign
 * or decimal in src in saved. All other characters are ignored.
 */
		else
			{
			++dlen;
			--dest;
			switch(ch)
				{
				case '+':
				case '-':
					sign = ch;
					break;
				case '.':
					isdec = 1;
					break;
				default:
					break;
				}
			}
		if(slen)
			{
			--slen;
			++src;
			}
		if(dlen)
			{
			--dlen;
			++dest;
			}
		}
	return(sign);	
}

/*----
Insert a character and right shift for length.
------*/
static void insr(char *dest,int ch,int len)
{
	int sch;

	sch = *dest;
	while(len--)
		{
		*dest++ = ch;
		ch = sch;
		sch = *dest;
		}
}
/*----
Insert a character and left shift for length.
------*/
static void insl(char *dest,int ch,int len)
{
	int sch;

	sch = *dest;
	while(len--)
		{
		*dest-- = ch;
		ch = sch;
		sch = *dest;
		}
}

/*----
Convert unsigned to unsigned edited.
------*/
static void utoeu(char *dest,char *src,int dlen,int slen,int dec)
{
	static char dummy[] = {(char)0xf0};

	if(dlen < 1)		/* Can't convert Needs sign and 1 digit*/
		return;
	if(dlen < 2)
		*dest = '0';
	if(slen < 1)		/* No source so use a dummy zoned 0 */
		{
		slen = 1;
		src = dummy;
		}
	if(dec < 1)		/* This forces zero and illegal decimals*/
		dec = -1;	/* to be ignored */
	src += (slen - 1);
	dest += (dlen - 1);
/*
 * Step backward through the field making a digit from
 * each low order nybble.
 */
	while((slen) || (dlen))
		{
		if(!dlen)
			break;	
		if(dec == 0)
			{
			*dest-- = '.';
			--dlen;
			}
		else
		if(!slen)
			{
			*dest-- = '0';	/* Out of source, zero pad */
			--dlen;
			}
		else
			{
			*dest-- = get_zoned_l_digit(*src--);
			--slen;
			--dlen;
			}
		--dec;
		}
}

/*----
Convert formatted index "(01)" to an integer.
-------*/
static int format_index_toi(char *src)
{
	int result,i;

	for(i = result = 0 ; i < 4 ; ++i)
		{
		if(isdigit((int)*src))
			{
			result *= 10;
			result += (*src - '0');
			}
		++src;
		}
	return(result);
}
/*----
Packed to integer for specfied length.
------*/
static int ptoilen(char *p,int len)
{
	int result;
	int ch;

	result = 0;
	while(len)
		{
		ch = get_packed_h_digit(*p);
		result *= 10;
		result += (ch - '0');
		if(len == 1)
			ch = get_packed_l_sign(*p);
		else
			ch = get_packed_l_digit(*p);
		if(ch == '-')
			{
			result *= -1;
			break;
			}
		else
		if( ch == '+' )
			break;
		else
		if(len == 1)
			break;
		else
			{
			result *= 10;
			result += (ch - '0');
			++p;
			}
		--len;
		}
	return(result);
		
}
/*----
These binary conversions are a little loose.
------*/
static void ehtob(char *dest,char *src,int dlen,int slen,int type)
{
	int hex,inc;

	inc = 1;
	if(type == ABMN)		/* Machine order */
		type = machine_order();
	switch(type)
		{
		case ABHL:
			break;
		case ABLH:
			dest += (dlen - 1);
			inc = -1;
		}

	while((slen > 0) || (dlen))
		{
		hex = 0;
		sscanf(src,"%2x",&hex);
		*dest = hex;
		dest += inc;
		--dlen;
		if((dlen != 0) && (Memeq(src,"  ",2)))
			{
			*dest = *(dest  + inc);
			*(dest  + inc ) = 0;
			}
		slen -= 2;
		src += 2;
		}
}

static void btoeh(char *dest,char *src,int dlen,int slen,int type)
{
	long result;
	char format[21];
	char work[101];

	result = btol(src,slen,type);

	sprintf(format,"%%0%dlx",dlen);
	sprintf(work,format,result);
	kcsi_strupr(work);
	while((int)strlen(work) > dlen)
		{
		strcpy(work,&work[1]);
		}
	memcpy(dest,work,dlen);
}

/*----
Convert an unformatted input field to a binary. The input may have the
sign anywhere in it.
------*/
static void eutob(char *dest,char *src,int dlen,int slen,int sdec,int type)
{

	long lw;
	int bufi,ch;
	int inc;
	char buf[32];

	bufi = 0;
	buf[bufi++]='+';
	slen -= sdec;
	while(slen--)
		{
		ch = *src;
		if(isdigit(ch))
			buf[bufi++]=ch;
		else
			{
			switch(ch)
				{
				case '-':
					buf[0] = ch;
					break;
				default:
					break;
				}
			}
		++src;
		}
	buf[bufi] = 0;

	lw = 0;
	sscanf(buf,"%ld",&lw);
/*
 * Pick off the bytes one by one from lowest to highest and plug into
 * dest.
 */
	inc = 1;
	if(type == ABMN)		/* Machine order */
		type = machine_order();
	switch(type)
		{
		case ABHL:
			dest += (dlen - 1);
			inc = -1;
			break;
		case ABLH:
			break;
		}
		
	while(dlen--)
		{
		ch = lw & 0x00ff;
		*dest = ch;
		dest += inc;
		lw >>= 8;
		}
}

/*----
strcpy delimited by enclosing quotes.
------*/
static void litcpy(char *dest,char *src,int len)
{
	int q,ch;

	q = 0;
	while(len--)
		{
		ch = *src++;
		if((ch == '\'')||(ch == '\"'))
			{
			if(q == ch)
				break;
			else
			if(q == 0)
				q = ch;
			else
			*dest++ = ch;
			}
		else
			*dest++ = ch;
		}
	*dest = 0;
}
static void cvt_illegal(DTYPE *dest,DTYPE *src)
{
	char hex_buf[64];
	char str_buf[20];
	char *cvt_type();
	char *base;
	int stridx, hexidx,ch;

	base = SRC_ADDRESS;
	hexidx = 0;
	for(stridx = 0 ; stridx < 16 ; ++stridx)
		{
		ch = *base++;
		sprintf(&hex_buf[hexidx],"%02x ",ch);
		hexidx +=3;
		if( ( ch > '~') || (ch < ' '))
			ch = '.';
		str_buf[stridx] = ch;
		}
	hex_buf[hexidx] = 0;
	str_buf[stridx] = 0;

	kcsitrace(4,"DATA","CONVERSION","Illegal conversion from %s to %s, %s - %s len %d",
		  cvt_type(src), cvt_type(dest), str_buf, hex_buf, src->_len);
}

struct _type_table{
	int type;
	char *desc;
	};

char *cvt_type(DTYPE *dt)
{
	static char format[20];

	static struct _type_table t[] = {
		{ACHR,"characters"},
		{APCK,"packed"},
		{AZON,"zoned"},
		{AUNS,"unsigned"},
		{ABIN,"binary"},
		{ABLH,"L->H binary"},
		{ABHL,"H->L binary"},
		{ABMN,"mach ord binary"},
		{CSTR,"string"},
		{CINT,"integer"},
		{CLNG,"long"},
		{CFLT,"float"},
		{CDBL,"double"},
		{CCHR,"char as int"},
		{BZON,"zoned ASCII"},
		{BUNS,"unsigned ASCII"},
		{BYES,"YES/NO"},
		{BLNK,"non-blank"},
		{BIDX,"formatted index"},
		{BMSK,"bit mask"},
		{BCTP,"CONTROL data type"},
		{BDTP,"program data type"},
		{BONE,"forced 1"},
		{BTRN,"truncated string"},
		{BPOS,"0 base pos"},
		{BLIT,"quoted literal"},
		{DZON,"edited zoned"},
		{DHEX,"HEX digits"},
		{DUNS,"edited unsigned"},
		{NLIT,"numeric literal"},
		{TZON,"edited sign trailing"},
		{0,NULL}};

	int i;

	for(i = 0; t[i].type ; ++i)
		{
		if(t[i].type == dt->_type)
			return(t[i].desc);
		}

	sprintf(format,"unknown %d",dt->_type);
	return(format);
}

/*----
Convert the incoming value into an overpunch. If the incoming
value for a high nybble is a sign, then assume that this is
to be an overpunch for zoned field.
LPI COBOL uses a totally oddball scheme for sign over punching
	0 = 7B = {		-0 = 7D = }
	1 = 41 = A		-1 = 4A = J
	2 = 42 = B		-2 = 4B = K
	3 = 43 = C		-3 = 4C = L
	4 = 44 = D		-4 = 4D = M
	5 = 45 = E		-5 = 4E = N
	6 = 46 = F		-6 = 4F = O
	7 = 47 = G		-7 = 50 = P
	8 = 48 = H		-8 = 51 = Q
	9 = 49 = I		-9 = 52 = R
The LPI scheme is not so oddball after all, since VAX COBOL uses
it too.

 ACUCOBOL uses the same scheme on DISPLAY items, but only for
negative values.

------*/
/*----
Combine a digit and a sign into the zoned overpunch
The make_zoned_ascii is a macro that evaluates to one
of the types below
------*/
static int make_zoned_sign(int digit,int sign,char *dest)
{
	if(( digit < '0') || (digit > '9'))
		digit = '0';
	make_zoned_ascii(digit,sign,dest);
	/* not used ??? make_zoned_overpunch(digit,sign,dest); */
	return(*dest);
}
/*----
in acucobol, only the negative values are modified
------*/
#ifdef	KCSI_ACU
static void make_zoned_acu(int digit,int sign,char *dest)
{
	if(digit == '0')
		{
		if(sign == '-')
			digit = 0x7d;
		}
	else
	if(sign == '-')
		digit += 0x19;	/* 31-39 = 4A-52 */
	*dest = digit;
}
#endif
/*----
in MF, only the negative values are modified but differently
------*/
#ifdef KCSI_MFX
static void make_zoned_mf(int digit, int sign, char *dest)
{
	if(sign == '-')
		digit += 0x40;		/* 30 - 39 = 70 - 79 */
	*dest = digit;
}
#endif

#ifdef NOT_USED
static void make_zoned_lpi(digit,sign,dest)
int digit,sign;
char *dest;
{
	if(digit == '0')
		{
		if(sign == '-')
			digit = 0x7d;
		else
			digit = 0x7b;
		}
	else
	if(sign == '-')
		digit += 0x19;	/* 31-39 = 4A-52 */
	else
		digit += 0x10;	/* 31-39 = 41-49 */
	*dest = digit;
}

/*----
This is not used, but retained as it is Wang style overpunching
and may be needed some day
------*/
static void make_zoned_overpunch(digit,sign,dest)
int digit,sign;
char *dest;
{
	if(sign == '-')
		digit += 0xa0;	/* 31-39 = D1-D9 */
	else
		digit += 0xc0;	/* 31-39 = F1-F9 */
	*dest = digit;
}
#endif /* NOT_USED */

static void make_h_packed(int ch,char *dest)
{
/* If it's not a digit then make it zero*/
	if (( ch > '9') || (ch < '0'))
		ch = '0';
/* Convert it to a high nybble */
	ch = (ch - '0') * 16;

	*dest &= 0x0f;
	*dest |= ch;
}

/*----
Convert an ASCII digit to a low order nybble.
------*/
static void make_l_packed(int ch,char *dest)
{
	if(ch == '-')
		ch = 0x0d;
	else
	if(ch == '+')
#ifdef	PACKED_C_POSITIVE
		ch = 0x0c;
#else
		ch = 0x0f;
#endif	/* PACKED_C_POSITIVE */
	else
	if(( ch < '1') || (ch > '9'))
		ch = 0x00;
	else
		ch -= '0';
	*dest &= 0xf0;
	*dest |= ch;
}

static int get_packed_l_num(int ch)
{
	ch = get_packed_l_digit(ch);
	return(ch - '0');
}
/*----
Extract a packed digit or sign from a low nybble.
Make a digit out of a low order nybble. If the nybble is not a digit
then return it as a sign.
------*/
static int get_packed_l_digit(int ch)
{
	ch &= 0x0f;
	if(ch == 0x0d)
		return('-');
	else
	if(ch > 9)
		return('+');
	return(ch += '0');
}

static int get_packed_l_sign(int ch)
{
	ch &= 0x0f;
	if(ch == 0x0d)
		return('-');
	else
		return('+');
}

static int get_packed_h_num(int ch)
{
	ch = get_packed_h_digit(ch);
	return(ch - '0');
}

/*----
Make a digit out of a high order nybble. Force it to be a digit on return.
------*/
static int get_packed_h_digit(int ch)
{
	ch >>= 4;
	ch = get_packed_l_digit(ch);
	if( (ch > '9') || (ch < '0') )
		ch = '0';
	return(ch);
}

static int get_zoned_l_num(int ch)
{
	ch = get_zoned_l_digit(ch);
	return(ch - '0');
}


/*----
Extract a zoned digit from a low nybble.
Make a digit out of a low order nybble. If the nybble is not a digit
then return it as a sign.
LPI uses
	0 = 7B = {		-0 = 7D = }
	1 = 41 = A		-1 = 4A = J
	2 = 42 = B		-2 = 4B = K
	3 = 43 = C		-3 = 4C = L
	4 = 44 = D		-4 = 4D = M
	5 = 45 = E		-5 = 4E = N
	6 = 46 = F		-6 = 4F = O
	7 = 47 = G		-7 = 50 = P
	8 = 48 = H		-8 = 51 = Q
	9 = 49 = I		-9 = 52 = R
Acu COBOL uses the above for negatives, and the straight
digit for positives

Microfocus uses the character for positives values and

	-0 = 0x70 = p
	-1 = 0x71 = q
	-2 = 0x72 = r
	-3 = 0x73 = s
	-4 = 0x74 = t
	-5 = 0x75 = u
	-6 = 0x76 = v
	-7 = 0x77 = w
	-8 = 0x78 = x
	-9 = 0x79 = y
------*/
static int get_zoned_l_digit(int ch)
{
/* MF scheme for negatives */
	if( ( ch <= 0x79) && (ch >= 0x70))
		return(ch - 0x40);
/* LPI scheme for negative or positive 0 */
/* ACU scheme for neg 0 */
	if( ( ch == 0x7b ) || (ch == 0x7d) )
		return('0');
/* Just the digit used by ACU and MF for positives */
	if( ( ch <= '9') && (ch >= '0'))
		return(ch);

/* Out of range */
	if( (ch > 0x52) || (ch < 0x41))
		return('0');
/* LPI scheme for poitive and negative */
/* ACU scheme for negatives */
	if( (ch <= 'I') && (ch >= 'A'))
		return( ch - 0x10 );
	if( (ch <= 'R') && (ch >= 'J') )
		return( ch - 0x19);

	return(get_packed_l_digit(ch));

}

static int get_zoned_sign(int ch)
{
	if(ch == 0x7b)
		return('+');
	if(ch == 0x7d)
		return('-');
	if( (ch <= 'y') && (ch >= 'p'))
		return( '-' );
	if( (ch <= 'I') && (ch >= 'A'))
		return( '+' );
	if( (ch <= 'R') && (ch >= 'J') )
		return( '-');
	if( (ch > 0x52) || (ch < 0x4a))
		return('+');
	return(get_packed_l_sign(ch >>= 4));
}

/*
**	History:
**	$Log: rcvt.c,v $
**	Revision 1.17  2010/01/10 00:58:33  gsl
**	fix LINUX warnings
**	trunc
**	isblank
**	sys_errlist
**	
**	Revision 1.16  2003/02/20 19:29:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.15  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.14  2002/10/23 20:39:07  gsl
**	make global name unique
**	
**	Revision 1.13  2002/10/22 21:10:20  gsl
**	Unique global sysmbols
**	
**	Revision 1.12  2002/10/17 21:22:42  gsl
**	cleanup
**	
**	Revision 1.11  2002/10/17 17:17:21  gsl
**	Removed VAX VMS code
**	
**	Revision 1.10  2002/07/25 15:20:25  gsl
**	Globals
**	
**	Revision 1.9  2002/04/22 15:36:02  gsl
**	replace crid_error_trace() with kcsitrace()
**	
**	Revision 1.8  2002-04-19 16:14:31-04  gsl
**	Remove ifdefed code
**
**	Revision 1.7  2000-03-13 14:15:09-05  gsl
**	Change strupr() to kstrupr()
**
**	Revision 1.6  1999-09-13 15:51:47-04  gsl
**	fix double to long conversion
**
**	Revision 1.5  1997-06-05 12:57:15-04  scass
**	Changed subroutine name from dtob to dbltob.
**	Was getting a conflict.
**
**	Revision 1.4  1997-01-11 17:15:48-05  gsl
**	Added prototype for make_zoned_ascii()
**
**	Revision 1.3  1996-09-17 16:45:46-07  gsl
**	drcs update
**
**
**
*/
