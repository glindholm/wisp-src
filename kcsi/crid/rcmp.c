static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>

#include "dtype.h"
#include "shrthand.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)rcmp.c	1.9 9/6/93";

/*----
Bug in UNIX memcmp always returns 0 for equality and -1 for
inequality
------*/
#ifdef memcmp
#undef memcmp
#endif
#define memcmp(x,y,z)	mc(x,y,z)
/*----
Shorthand
------*/
#define	LOP_ADDRESS	(lop->_base + lop->_pos)
#define	ROP_ADDRESS	(rop->_base + rop->_pos)
#define	OP_ADDRESS	(op->_base + op->_pos)


static mc(char* mem1,char* mem2,int len);
static int comp_eq(DTYPE *lop,DTYPE *rop);
static int comp_mem(DTYPE *lop,DTYPE *rop);
static int comp_lt(DTYPE *lop,DTYPE *rop);
static int opcmp(DTYPE *lop,DTYPE *rop);
static int comp_gt(DTYPE *lop,DTYPE *rop);
static void cmbmath(DTYPE *lop,int op,DTYPE *rop);
static void cmbcat(DTYPE *lop,DTYPE *rop);
static double doubleit(DTYPE *op);
static void undoubleit(double fl,DTYPE *op);

static int comp_blank(DTYPE *lop);

/*----
Main for testing comparison logic
main(argc,argv)
int argc;
char *argv[];
{
	DTYPE *lop,*rop,l,r;
	char *op;
	int rc;

	lop = &l;
	rop = &r;
	memset(lop,0,sizeof(DTYPE));
	memset(rop,0,sizeof(DTYPE));

	lop->_dec = rop->_dec = 0;
	lop->_pos = rop->_pos = 0;
	lop->_base = (char*)strdup(argv[1]);
	lop->_len = strlen(LOP_ADDRESS);
	lop->_type = BZON;
')
		{
		lop->_type = ACHR;
		}
	op = (char*) strdup(argv[2]);
	rop->_base = (char*)strdup(argv[3]);
	rop->_len = strlen(ROP_ADDRESS);
	rop->_type = lop->_type;
	if(argc > 4)
		lop->_dec = atoi(argv[4]);
	if(argc > 5)
		rop->_dec = atoi(argv[5]);
	rc = rptcmp(lop,op,rop);
	printf(" = %s\n",rc?"TRUE":"FALSE");

}
------*/
/*----
Main for testing combining logic
char *strdup();

main(argc,argv)
int argc;
char *argv[];
{
	DTYPE l,r,*lop,*rop;
	int op;
	char buf[512];

	lop = &l;
	rop = &r;
	memset(lop,0,sizeof(DTYPE));
	memset(rop,0,sizeof(DTYPE));

	lop->_dec = rop->_dec = 0;
	lop->_pos = rop->_pos = 0;
	lop->_base = buf;
	strcpy(buf,argv[1]);
	lop->_len = strlen(buf);
')
		{
		lop->_type = ACHR;
		lop->_len = strlen(LOP_ADDRESS);
		}
	else
		lop->_type = BZON;
	op = argv[2][0];
	rop->_base = (char*)strdup(argv[3]);
	rop->_len = strlen(ROP_ADDRESS);
	rop->_type = lop->_type;
	if(rop->_type & IS_NUM)
		rop->_type = NLIT;
	rptcmb(lop,op,rop);
	printf(" = %s\n",LOP_ADDRESS);

}
------*/
/*----
Compare two operands
------*/
int rptcmp(DTYPE *lop,char *op,DTYPE *rop)
{

	if(Streq(op,"EQ"))
		return(comp_eq(lop,rop));
	else
	if(Streq(op,"GT"))
		return(comp_gt(lop,rop));
	else
	if(Streq(op,"LT"))
		return(comp_lt(lop,rop));
	else
	if(Streq(op,"NE"))
		return(!(comp_eq(lop,rop)));
	else
	if(Streq(op,"GE"))
		return(!(comp_lt(lop,rop)));
	else
	if(Streq(op,"LE"))
		return(!(comp_gt(lop,rop)));
	return(0);
}
/*----
Strings are equal and have the same number of decimal places
------*/
static int comp_eq(DTYPE *lop,DTYPE *rop)
{

	if(lop->_type & IS_NUM)
		return(!(opcmp(lop,rop)));
	else
		return(!(comp_mem(lop,rop)));
}

/*----
Pad area to create a blank terminated version of the shorter operand
------*/
static char comp_work1[513];
static char comp_work2[513];

static int comp_mem(DTYPE *lop,DTYPE *rop)
{

	int rc,len;

	memset(comp_work1,' ',512);
	memset(comp_work2,' ',512);
	memcpy(comp_work1,LOP_ADDRESS,lop->_len);
	memcpy(comp_work2,ROP_ADDRESS,rop->_len);
	len = max(lop->_len,rop->_len);
	rc = memcmp(comp_work1,comp_work2,len);

	return(rc);
}

/*----
Specialized compares to check for blank, zeroes or space
------*/
static int comp_blank(DTYPE *lop)
{
	if(lop->_type & IS_NUM)
		return(comp_zero(lop));
	else
		return(comp_space(lop));
}

int comp_zero(DTYPE *lop)
{
	double left, doubleit();
	left = doubleit(lop);
	if(left == 0.0)
		return(1);
	return 0;
}

int comp_space(DTYPE *lop)
{
	int len;
	char *mem;

	mem = LOP_ADDRESS;	
	for(len = 0; len < lop->_len; ++len, ++mem)
		{
		if(*mem != ' ')
			return(0);
		}
	return(1);
}

int comp_rec_num(long num,DTYPE *lop,DTYPE *rop)
{
	long temp,lo,hi;

	lo = atol(LOP_ADDRESS);
	hi = atol(ROP_ADDRESS);
	if(hi < lo)
		{
		temp = lo;
		lo = hi;
		hi = temp;
		}
	if( num < lo)
		return(0);
	if(num > hi)
		return(0);
	return(1);
}


/*----
If strings are characters then a memcmp can be used.
Otherwise returns and compares depend upon the condition
of the various signs.
------*/
static int comp_lt(DTYPE *lop,DTYPE *rop)
{
	if(lop->_type & IS_NUM)
		return(opcmp(lop,rop) < 0);
	else
		return(comp_mem(lop,rop) < 0);
}

static int opcmp(DTYPE *lop,DTYPE *rop)
{
	double left,right,test,doubleit();

	left = doubleit(lop);
	right = doubleit(rop);
	

	test = left - right;

	if(test == 0.0)
		return(0);
	else
	if(test < 0.0)
		return(-1);
	else
		return(1);

}

/*----
comp_gt is comp_lt with the operands reversed.
------*/
static int comp_gt(DTYPE *lop,DTYPE *rop)
{
	return(comp_lt(rop,lop));
}

int rptcmb(DTYPE *lop,int op,DTYPE *rop)
{
	int rc;
/*
 * Validate that types are compatible. A chr may only combine with
 * a chr using the concatenate '&' operator.
 */
	switch(lop->_type)
		{
		case ACHR:
			if((op != '&')||(rop->_type != ACHR))
				return(0);
			break;
		default:
			if((op == '&')||(rop->_type == ACHR))
				return(0);
			break;
		}
	switch(op)
		{
		case '&':
			cmbcat(lop,rop);
			rc = 0;
			break;
		case '+':
		case '-':
		case '/':
		case '*':
			cmbmath(lop,op,rop);
			rc = 0;
			break;
		default:
			return(0);
		}
	return(rc);
}

static void cmbcat(DTYPE *lop,DTYPE *rop)
{
	memcpy(&LOP_ADDRESS[lop->_len],ROP_ADDRESS,rop->_len);
	lop->_len += rop->_len;
	LOP_ADDRESS[lop->_len] = 0;
}

/*----
Modified to include a check for divide by zero.
------*/
static void cmbmath(DTYPE *lop,int op,DTYPE *rop)
{
	double left,right;

	left = doubleit(lop);
	right = doubleit(rop);
	switch(op)
		{
		case '+':
			left += right;
			break;
		case '-':
			left -= right;
			break;
		case '*':
			left *= right;
			break;
		case '/':
			if(right == 0.0)
				left = 0.0;
			else
				left /= right;
			break;
		}
	undoubleit(left,lop);
}

static double doubleit(DTYPE *op)
{
	double fl;
	int dec;
	char format[10];

	sprintf(format,"%%%dlf",op->_len);
	fl = 0;
	sscanf(OP_ADDRESS,format,&fl);
	dec = op->_dec;
	if(op->_type != NLIT)
		{
		while(dec--)
			fl /= 10;
		}
	return(fl);
}

static void undoubleit(double fl,DTYPE *op)
{
	char format[10], buf[32];
	int dec;

	dec = op->_dec;
	while(dec--)
		fl *= 10;
	sprintf(format,"%%+0%d.0lf",op->_len);
	sprintf(buf,format,fl);
	strcpy(OP_ADDRESS,buf);
}

/*----
To handle UNIX memcmp bug. Fancy shmancy to removed
side effects if char is unsigned.
------*/
static mc(char* mem1,char* mem2,int len)
{
	int ch1, ch2;

    for( ; len; ++mem1, ++mem2, --len )
	{
	ch1 = (*mem1) & 0xff;
	ch2 = (*mem2) & 0xff;
	ch1 -= ch2;
	if(ch1 < 0)
	    return(-1);
	if(ch1 > 0)
	    return(1);
	}
    return(0);
}

/*
**	History:
**	$Log: rcmp.c,v $
**	Revision 1.5  1999-09-13 15:51:35-04  gsl
**	fix missing return codes
**
**	Revision 1.4  1997-10-02 15:24:21-04  gsl
**	fix warnings
**
**	Revision 1.3  1996-09-24 20:34:38-04  gsl
**	'fix waring
**
**	Revision 1.2  1996-09-17 16:45:44-07  gsl
**	drcs update
**
**
**
*/
