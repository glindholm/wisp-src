static char copyright[]="Copyright (c) 1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		qapic.c
**
**	Project:	WISP/SAMPLE
**
**	RCS:		$Source:$
**
**	Purpose:	QA cobpic.c
**
*/

/*
**	Includes
*/
#include <stdio.h>
#include <string.h>
#include "cobpic.h"

#define EXT_FILEXT
#include "filext.h"



/*
**	ROUTINE:	test_xxx()
**
**
**	RETURN:		
**	1		Passed
**	0		Failed
**
**
*/

int test_parse_pic(
	int testnum,
	const char* picture,
	const char* t_xpic,	/* The expanded picture (also adjusted picture for Numeric only) */
	int t_xflag,		/* Flag if xpic was created. */
	int t_psize,		/* The number of character positions in pic */
	int t_pic_type,		/* The type of picture 0=noedit 1=Alphaedit 2=Numeric */
	int t_pic_dp,		/* Number of characters to the left of the decimal point 	(Numeric only)*/
	int t_blankdecimal,	/* Does the decimal portion contains no '9's	 		(Numeric only)*/
	char t_suppchar,	/* Zero suppression character 					(Numeric only)*/
	int t_suppidx,		/* Zero suppression index; offset of first 'Z' or '*' in pic	(Numeric only)*/
	int t_floatidx,		/* Floating character index; offset of start of float 		(Numeric only)*/
	int t_psigned)		/* Is the picture signed 0,1,2,3		 		(Numeric only)*/
{
	char	xpic[100];
	int	xflag = -1;
	int 	psize = -1;
	int	pic_type = -1;
	int 	pic_dp = -1;
	int 	blankdecimal = -1;
	char	suppchar = '@';
	int	suppidx = -1;
	int 	floatidx = -1;
	int	psigned = -1;

	memset(xpic, '@', sizeof(xpic));

	parse_pic(picture, xpic, &xflag, &psize, &pic_type, &pic_dp, &blankdecimal, 
						 &suppchar, &suppidx, &floatidx, &psigned);


	if (0 != strcmp(xpic, t_xpic) ||
	    xflag 	!= t_xflag ||
	    psize 	!= t_psize ||
	    pic_type 	!= t_pic_type ||
	    pic_dp 	!= t_pic_dp ||
	    blankdecimal != t_blankdecimal ||
	    suppchar	!= t_suppchar ||
	    suppidx	!= t_suppidx ||
	    floatidx	!= t_floatidx ||
	    psigned	!= t_psigned)
	{
		/* Error */
		printf("qapic: test %d failed picture=[%s]\n",testnum, picture);
		printf("  xpic         [%s] \t [%s]\n",	t_xpic, 	xpic);
		printf("  xflag        %d \t %d\n", 	t_xflag,       	xflag);
		printf("  psize        %d \t %d\n", 	t_psize,       	psize);
		printf("  pic_type     %d \t %d\n",	t_pic_type, 	pic_type);
		printf("  pic_dp       %d \t %d\n", 	t_pic_dp,      	pic_dp);
		printf("  blankdecimal %d \t %d\n", 	t_blankdecimal,	blankdecimal);
		printf("  suppchar     [%c] \t [%c]\n", t_suppchar, 	suppchar);
		printf("  suppidx      %d \t %d\n", 	t_suppidx, 	suppidx);
		printf("  floatidx     %d \t %d\n", 	t_floatidx, 	floatidx);
		printf("  psigned      %d \t %d\n", 	t_psigned, 	psigned);
		
		return 0;
	}
	else
	{
		return 1;
	}
	
}

int test_cobpic_edit(
	int testnum,
	const char* picture,
	const char* source,
	const char* t_object,
	int t_errcode)
{
	int	failed = 0;
	char	object[80];
	int	errcode = -1;

	/*
	**	Fill the object with '@' then use the trailing '@' to null terminate.
	*/
	memset(object,'@',sizeof(object));

	cobpic_edit(object, source, picture, &errcode);

	*(strchr(object, '@')) = '\0';	/* Null terminate object */

	if (0 != t_errcode) /*  Expected fail condition */
	{
		if (0 != errcode) 
		{
			return 1;	/* Pass */
		}

		failed = 1;		/* Failed */
	}

	if (!failed)
	{
		if (0 != errcode || 0 != strcmp(object, t_object))
		{
			failed = 1;	/* Failed */
		}
		
	}

	if (failed)
	{
		printf("qapic: test %d failed\n",testnum);
		printf("  picture   = %s\n", picture);
		printf("  source    = %s\n", source);
		printf("  t_object  = [%s]\n", t_object);
		printf("  object    = [%s]\n", object);
		printf("  t_errcode = %d\n", t_errcode);
		printf("  errcode   = %d\n", errcode);
	
		return 0; /* Failed */
	}
	
	/*
	 *	Reverse test, pass the object back in as the source.
	 */
	memset(object,'@',sizeof(object));

	cobpic_edit(object, t_object, picture, &errcode);

	*(strchr(object, '@')) = '\0';	/* Null terminate object */

	if (0 != errcode || 0 != strcmp(object, t_object))
	{
		printf("qapic: test %d failed (Reverse)\n",testnum);
		printf("  picture   = %s\n", picture);
		printf("  source    = %s\n", source);
		printf("  reverse   = [%s]\n", t_object);
		printf("  object    = [%s]\n", object);
		printf("  errcode   = %d\n", errcode);
	
		return 0; /* Failed */
	}

	return 1;	/* Pass */
}


int main()
{
	char	*source;
	int	testnum;
	int	pass;
	
	testnum=0;
	pass=0;
	
	/*
	 *	Test parse_pic()
	 *
	 *	psize is simply strlen(xpic)
	 *	pic_dp is the decimal point offset from into xpic, if no dp the same as psize.
	 *	If there is a '9' after the decimal point then the decimal portion can not be blank.
	 *	'Z' and '*' are Zero suppression characters in the Picture, 'Z' is replaced with a space.
	 *	I have not been able to figure out how suppidx (si) is calculated.
	 *	Multiple leading '-', '+', '$' characters float to the front and remove leading zeros.
	 *	Floatidx is the offset into xpic of the float character before the first '#' float pad character.
	 *
	 *	Rules for determining type.
	 *
	 *		A picture clause is considered to be of type alphabetic, alphanumeric, alphaedit or numeric.
	 *		An alphabetic picture contains only A and B elements.
	 *		An alphanumeric picture contains only A, X, and 9 elements.
	 *		An alphaedit picture contains at least one A or X and at least one B, 0 or /.
	 *		A numeric picture does not contain any A or X elements.
	 *		A picture element of P, S, or V are ignored.
	 *		All numeric pictures are treated a numeric-edited.
	 *		If the picture is alphanumeric or the size is greater then 80 no editing is performed.
	 */

	/* 				  Picture		xpic			xflag,  psize, 	type		dp  bd zsc  zsi fi  signed */
	pass += test_parse_pic(++testnum, "9(5)", 		"99999", 		1,	5, 	PIC_NUMERIC, 	5,  1, ' ', -1, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "999V99", 		"99999", 		1,	5, 	PIC_NUMERIC, 	5,  1, ' ', -1, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "999P99", 		"99999", 		1,	5, 	PIC_NUMERIC, 	5,  1, ' ', -1, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "ZZZS99", 		"ZZZ99", 		1,	5, 	PIC_NUMERIC, 	5,  1, ' ',  0, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "Z(5)", 		"ZZZZZ", 		1,	5, 	PIC_NUMERIC, 	5,  1, ' ',  0, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "-(6)", 		"-#####", 		1,	6, 	PIC_NUMERIC, 	6,  1, ' ', -1,  0, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "99,999.99+",		"99,999.99+", 		1,	10, 	PIC_NUMERIC, 	6,  0, ' ', -1, -1, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "$ZZ,ZZZ.ZZB-",	"$ZZ,ZZZ.ZZb-", 	1,	12, 	PIC_NUMERIC, 	7,  1, ' ',  0, -1, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "+Z(9)",		"+ZZZZZZZZZ", 		1,	10, 	PIC_NUMERIC,   	10, 1, ' ',  0, -1, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "$B*(9)BCR",		"$b*********bCR",	1,	14, 	PIC_NUMERIC,   	14, 1, '*',  1, -1, PIC_CR_SIGNED);
	pass += test_parse_pic(++testnum, "$$,$$$B.B$$BDB",	"$#,###b.b##bDB",	1,	14, 	PIC_NUMERIC,    7,  1, ' ', -1,  0, PIC_DB_SIGNED);
	pass += test_parse_pic(++testnum, "+++,+++.99/99",	"+##,###.99/99",	1,	13, 	PIC_NUMERIC,    7,  0, ' ', -1,  0, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "-$B**,***.**",	"-$b**,***.**",		1,	12, 	PIC_NUMERIC,    9,  1, '*',  2, -1, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "----.--",		"-###.##",		1,	7, 	PIC_NUMERIC,    4,  1, ' ', -1,  0, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "--,---.--",		"-#,###.##",		1,	9, 	PIC_NUMERIC,    6,  1, ' ', -1,  0, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "$B--,---.--",	"$b-#,###.##",		1,	11, 	PIC_NUMERIC,    8,  1, ' ', -1,  2, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "----.99",		"-###.99",		1,	7, 	PIC_NUMERIC,    4,  0, ' ', -1,  0, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "+++,+++.99/99",	"+##,###.99/99",	1,	13, 	PIC_NUMERIC,    7,  0, ' ', -1,  0, PIC_SIGNED);
	pass += test_parse_pic(++testnum, "ZZZBZZBZZZZ",	"ZZZbZZbZZZZ",		1,	11, 	PIC_NUMERIC,   	11, 1, ' ',  0, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "***B**B****",	"***b**b****",		1,	11, 	PIC_NUMERIC,   	11, 1, '*',  0, -1, PIC_UNSIGNED);
	pass += test_parse_pic(++testnum, "ZZZ/ZZ/ZZZZ",	"ZZZ/ZZ/ZZZZ",		1,	11, 	PIC_NUMERIC,   	11, 1, ' ',  0, -1, PIC_UNSIGNED);

	pass += test_parse_pic(++testnum, "AAA",		"AAA",			1,	3, 	PIC_ALPHAEDIT,  -1, -1, ' ', -1, -1, -1);
	pass += test_parse_pic(++testnum, "ABA",		"ABA",			1,	3, 	PIC_ALPHAEDIT,  -1, -1, ' ', -1, -1, -1);
	pass += test_parse_pic(++testnum, "A9AB9A9BXXX",	"A9AB9A9BXXX",		1,	11, 	PIC_ALPHAEDIT, 	-1, -1, ' ', -1, -1, -1);
	pass += test_parse_pic(++testnum, "A9A",		"A9A",			1,	3, 	PIC_ALPHAEDIT,  -1, -1, ' ', -1, -1, -1);
	pass += test_parse_pic(++testnum, "A9A-9A9",		"A9A-9A9",		1,	7, 	PIC_ALPHAEDIT,  -1, -1, ' ', -1, -1, -1);

	pass += test_parse_pic(++testnum, "AXA",		"AXA",			1,	3, 	PIC_NOEDIT,  	-1, -1, ' ', -1, -1, -1);
	pass += test_parse_pic(++testnum, "A(3)X(3)",		"AAAXXX",		1,	6, 	PIC_NOEDIT,  	-1, -1, ' ', -1, -1, -1);
	pass += test_parse_pic(++testnum, "XXX XXX",		"XXX XXX",		1,	7, 	PIC_NOEDIT,  	-1, -1, ' ', -1, -1, -1);



	/*
	**	0 tests
	*/
	source = "0                        ";

	/*									 1234567890123	*/
	pass += test_cobpic_edit(++testnum, "9(5)", 		source, 	"00000", 		0);
	pass += test_cobpic_edit(++testnum, "99.99", 		source, 	"00.00", 		0);
	pass += test_cobpic_edit(++testnum, "ZZ.99", 		source, 	"  .00", 		0);
	pass += test_cobpic_edit(++testnum, "**.99", 		source, 	"**.00", 		0);
	pass += test_cobpic_edit(++testnum, "Z(5)", 		source, 	"     ", 		0);
	pass += test_cobpic_edit(++testnum, "-(6)", 		source, 	"      ", 		0);
	pass += test_cobpic_edit(++testnum, "----.--", 		source, 	"       ", 		0);
	pass += test_cobpic_edit(++testnum, "99,999.99+", 	source, 	"00,000.00+",		0);
	pass += test_cobpic_edit(++testnum, "$ZZ,ZZZ.ZZB-", 	source, 	"            ",		0);
	pass += test_cobpic_edit(++testnum, "+Z(9)", 		source, 	"          ",		0);
	pass += test_cobpic_edit(++testnum, "$B*(9)BCR",	source, 	"**************",	0);
	pass += test_cobpic_edit(++testnum, "$$,$$$B.B$$BDB",	source, 	"              ",	0);
	pass += test_cobpic_edit(++testnum, "+++,+++.99/99",	source, 	"      +.00/00",	0);
	pass += test_cobpic_edit(++testnum, "-$B**,***.**",	source, 	"*********.**",		0);
	pass += test_cobpic_edit(++testnum, "**.**",		source, 	"**.**",		0);
	

	/*
	**	9.9 tests
	*/
	source = "9.9                        ";

	/*									 1234567890123	*/
	pass += test_cobpic_edit(++testnum, "9(5)", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "99.99", 		source, 	"09.90", 		0);
	pass += test_cobpic_edit(++testnum, "ZZ.99", 		source, 	" 9.90", 		0);
	pass += test_cobpic_edit(++testnum, "**.99", 		source, 	"*9.90", 		0);
	pass += test_cobpic_edit(++testnum, "Z(5)", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "-(6)", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "----.--", 		source, 	"   9.90", 		0);
	pass += test_cobpic_edit(++testnum, "99,999.99+", 	source, 	"00,009.90+",		0);
	pass += test_cobpic_edit(++testnum, "$ZZ,ZZZ.ZZB-", 	source, 	"$     9.90  ",		0);
	pass += test_cobpic_edit(++testnum, "+Z(9)", 		source, 	"ERROR",		1);
	pass += test_cobpic_edit(++testnum, "$B*(9)BCR",	source, 	"ERROR",		1);
	pass += test_cobpic_edit(++testnum, "$$,$$$B.B$$BDB",	source, 	"    $9 . 90   ",	0);
	pass += test_cobpic_edit(++testnum, "+++,+++.99/99",	source, 	"     +9.90/00",	0);
	pass += test_cobpic_edit(++testnum, "-$B**,***.**",	source, 	" $******9.90",		0);
	pass += test_cobpic_edit(++testnum, "**.**",		source, 	"*9.90",		0);

	/*
	**	-1 tests
	*/
	source = "-1                         ";

	/*									 1234567890123	*/
	pass += test_cobpic_edit(++testnum, "9(5)", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "99.99", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "ZZ.99", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "Z(5)", 		source, 	"ERROR", 		1);
	pass += test_cobpic_edit(++testnum, "-(6)", 		source, 	"    -1", 		0);
	pass += test_cobpic_edit(++testnum, "----.--", 		source, 	"  -1.00", 		0);
	pass += test_cobpic_edit(++testnum, "99,999.99+", 	source, 	"00,001.00-",		0);
	pass += test_cobpic_edit(++testnum, "$ZZ,ZZZ.ZZB-", 	source, 	"$     1.00 -",		0);
	pass += test_cobpic_edit(++testnum, "+Z(9)", 		source, 	"-        1",		0);
	pass += test_cobpic_edit(++testnum, "$B*(9)BCR",	source, 	"$*********1 CR",	0);
	pass += test_cobpic_edit(++testnum, "$$,$$$B.B$$BDB",	source, 	"    $1 . 00 DB",	0);
	pass += test_cobpic_edit(++testnum, "+++,+++.99/99",	source, 	"     -1.00/00",	0);
	pass += test_cobpic_edit(++testnum, "-$B**,***.**",	source, 	"-$******1.00",		0);

	/*
	**	Simple editing
	*/
	
	pass += test_cobpic_edit(++testnum, "ZZZBZZBZZZZ",	
				            "123456789  ", 	
				            "123 45 6789", 		0);

	pass += test_cobpic_edit(++testnum, "ZZZ/ZZ/ZZZZ",	
				            "123456789  ", 	
				            "123/45/6789", 		0);

	pass += test_cobpic_edit(++testnum, "ZZZZ,ZZZ,ZZZ,ZZZ",	
				            "1234567890123   ", 	
				            "1234,567,890,123", 	0);

	pass += test_cobpic_edit(++testnum, "A9AB9A9BXXX",	
				            "A9A 9A9 A1$",
				            "A9A 9A9 A1$", 		0);
	
	/*
	**	END
	*/
	printf("qapic: passed %d of %d tests\n",pass,testnum);
	return 0;	
}

/*
**	History:
**	$Log: qapic.c,v $
**	Revision 1.8  2001/11/08 18:32:16  gsl
**	remove unused vars
**	
**	Revision 1.7  2001-09-26 15:19:32-04  gsl
**	add tests
**
**	Revision 1.6  2001-09-26 11:14:51-04  gsl
**	Rework all the tests to simplify and to reverse the test to ensure
**	the object is valid.
**
**	Revision 1.5  2001-09-25 16:39:49-04  gsl
**	Add parse_pic() tests
**
**	Revision 1.4  1998-08-28 09:28:03-04  gsl
**	fix warnings
**
**	Revision 1.3  1998-06-27 14:46:23-04  gsl
**	fix for hp
**
**	Revision 1.2  1998-06-27 14:39:34-04  gsl
**	Move from wisp/sample to wisp/utils
**
**	Revision 1.1  1998-06-27 14:23:57-04  gsl
**	Initial revision
**
**	Revision 1.11  1998-05-14 14:31:14-04  gsl
**	Change to 1998
**
**
**
**
*/

