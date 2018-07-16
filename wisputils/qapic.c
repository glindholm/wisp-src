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
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/


/*
**	ROUTINE:	test_result()
**
**	FUNCTION:	Test the results from a cobpic_edit()
**
**	DESCRIPTION:	If expecting a fail and it fails then this is a pass.
**			If expect success  and oject matches the result then pass.
**
**	ARGUMENTS:	
**	object		The result of cobpic_edit()
**	source		The input data
**	pic		The picture clause
**	errcode		The errcode for cobpic_edit()
**	result		The expected result (ignores if xerr != 0)
**	xerr		The expected errcode result ( 0 or not 0)
**	testnum		The test number for the error message
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		Passed
**	0		Failed
**
**	WARNINGS:	None
**
*/
int test_result(const char *object, 
		 const char *source,
		 const char *pic,   
		 int errcode,
		 const char *result,
		 int xerr,
		 int testnum)
{
	if (0 != xerr && 0 != errcode)
	{
		/*  Expected fail condition */
		return 1;
	}
	
	if (0==xerr && 0==errcode && 0==memcmp(object, result, strlen(result)))
	{
		return 1;
	}

	printf("qapic: test %d failed pic=%s source=%s\n",testnum, pic, source);
	return 0;
}

int main()
{
	char	object[80];
	char	*pic, *source;
	int	errcode, testnum;
	int	pass;
	
	testnum=0;
	pass=0;
	
	/*
	**	Fill the object with '@' then test for a trailing '@' in the result
	**	one char past the end.
	*/

	/*
	**	0 tests
	*/
	source = "0                        ";

	pic = "9(5)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "00000@",0,++testnum);

	pic = "Z(5)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "     @",0,++testnum);

	pic = "-(6)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "      @",0,++testnum);

	pic = "----.--";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "       @",0,++testnum);

	pic = "99,999.99+";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "00,000.00+@",0,++testnum);
	
	pic = "$ZZ,ZZZ.ZZB-";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "            @",0,++testnum);
	
	pic = "+Z(9)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "          @",0,++testnum);
	
	pic = "$B*(9)BCR";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "**************@",0,++testnum);
	
	pic = "$$,$$$B.B$$BDB";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "              @",0,++testnum);
	
	pic = "+++,+++.99/99";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "      +.00/00@",0,++testnum);
	
	pic = "-$B**,***.**";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "*********.**@",0,++testnum);

	/*
	**	9.9 tests
	*/
	source = "9.9                        ";

	pic = "9(5)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "00000@",1,++testnum);

	pic = "Z(5)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "     @",1,++testnum);

	pic = "-(6)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "      @",1,++testnum);

	pic = "----.--";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "   9.90@",0,++testnum);

	pic = "99,999.99+";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "00,009.90+@",0,++testnum);
	
	pic = "$ZZ,ZZZ.ZZB-";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "$     9.90  @",0,++testnum);
	
	pic = "+Z(9)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "          @",1,++testnum);
	
	pic = "$B*(9)BCR";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "**************@",1,++testnum);
	
	pic = "$$,$$$B.B$$BDB";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "    $9 . 90   @",0,++testnum);
	
	pic = "+++,+++.99/99";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "     +9.90/00@",0,++testnum);
	
	pic = "-$B**,***.**";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, " $******9.90@",0,++testnum);

	/*
	**	-1 tests
	*/
	source = "-1                         ";

	pic = "9(5)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "00000@",1,++testnum);

	pic = "Z(5)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "     @",1,++testnum);

	pic = "-(6)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "    -1@",0,++testnum);

	pic = "----.--";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "  -1.00@",0,++testnum);

	pic = "99,999.99+";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "00,001.00-@",0,++testnum);
	
	pic = "$ZZ,ZZZ.ZZB-";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "$     1.00 -@",0,++testnum);
	
	pic = "+Z(9)";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "-        1@",0,++testnum);
	
	pic = "$B*(9)BCR";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "$*********1 CR@",0,++testnum);
	
	pic = "$$,$$$B.B$$BDB";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "    $1 . 00 DB@",0,++testnum);
	
	pic = "+++,+++.99/99";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "     -1.00/00@",0,++testnum);
	
	pic = "-$B**,***.**";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "-$******1.00@",0,++testnum);

	/*
	**	Simple editing
	*/
	source = "123456789          ";
	
	pic = "ZZZBZZBZZZZ";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "123 45 6789@",0,++testnum);
	
	pic = "ZZZ/ZZ/ZZZZ";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "123/45/6789@",0,++testnum);
	
	source = "1234567890123          ";
	pic = "ZZZZ,ZZZ,ZZZ,ZZZ";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "1234,567,890,123@",0,++testnum);

	source = "A9A 9A9 A1$         ";	
	pic = "A9AB9A9BXXX";
	memset(object,'@',sizeof(object));
	cobpic_edit(object, source, pic, &errcode);
	pass += test_result(object, source, pic, errcode, "A9A 9A9 A1$@",0,++testnum);

	/*
	**	END
	*/
	printf("qapic: passed %d of %d tests\n",pass,testnum);
	return 0;	
}

/*
**	History:
**	$Log: qapic.c,v $
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

