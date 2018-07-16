			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	wsort		Utility which is a frontend to wispsort()
*/

#include <stdio.h>

#include "wcommon.h"
#include "wangkeys.h"
#include "wperson.h"
#include "sortseqf.h"

#define EXT_FILEXT
#include "filext.h"

static	int	verbose;
static	int	vexitdone;

main(argc,argv)
int	argc;
char	*argv[];
{
	struct argst { char *ptrs[500]; } args;
	long 	two;
	long 	cnt;
	long	recsize, sortcode,retcode;
	int	issue_getparms, i;
	char	filetype[1], in_recsize[6], in_numkeys[1];
	char	*message;

	struct
	{
		char	infile[8];
		char	inlib[8];
		char	invol[6];
		char	outfile[8];
		char	outlib[8];
		char	outvol[6];
		char	 post1[4];
		char	 length1[3];
		char	 type1[1];
		char	 order1[1];
		char	 post2[4];
		char	 length2[3];
		char	 type2[1];
		char	 order2[1];
		char	 post3[4];
		char	 length3[3];
		char	 type3[1];
		char	 order3[1];
		char	 post4[4];
		char	 length4[3];
		char	 type4[1];
		char	 order4[1];
		char	 post5[4];
		char	 length5[3];
		char	 type5[1];
		char	 order5[1];
		char	 post6[4];
		char	 length6[3];
		char	 type6[1];
		char	 order6[1];
		char	 post7[4];
		char	 length7[3];
		char	 type7[1];
		char	 order7[1];
		char	 post8[4];
		char	 length8[3];
		char	 type8[1];
		char	 order8[1];
	} sort;

	verbose = 0;
	vexitdone = 0;

	initglbs("WSORT   ");
      	wpload();		 							/* load the usage constants		*/

	if (argc>1)
	{
		if (0 == memcmp(argv[1],"-v",2))
		{
			verbose = 1;
		}
	}

	issue_getparms = 1;
	two=2;

	memset(&sort, ' ', 116);

	memset(sort.infile,' ',8);
	memcpy(sort.inlib,defaults.inlib,8);
	memcpy(sort.invol,defaults.invol,6);

	memset(sort.outfile,' ',8);
	memcpy(sort.outlib,defaults.outlib,8);
	memcpy(sort.outvol,defaults.outvol,6);

	filetype[0] = 'N';
	strcpy(in_recsize,"    0");
	in_numkeys[0] = '1';

	sort.type1[0] = 'C';
	sort.type2[0] = 'C';
	sort.type3[0] = 'C';
	sort.type4[0] = 'C';
	sort.type5[0] = 'C';
	sort.type6[0] = 'C';
	sort.type7[0] = 'C';
	sort.type8[0] = 'C';

	sort.order1[0] = 'A';
	sort.order2[0] = 'A';
	sort.order3[0] = 'A';
	sort.order4[0] = 'A';
	sort.order5[0] = 'A';
	sort.order6[0] = 'A';
	sort.order7[0] = 'A';
	sort.order8[0] = 'A';

	if (issue_getparms)
	{
		char	pfkeyrecvr[1];
		long 	N[256];
		long	pfkey;

		for (i=0; i<256; ++i) 
		{
			N[i]=i; 
			wswap(&N[i]);
		}

		pfkey = PFKEY_16_ENABLED;						/* Enable pf16				*/
		wswap( &pfkey );							/* Do system dependent swap		*/

		/*
			INPUT getparm
		*/

		cnt = 0;
		args.ptrs[cnt++] = (char *) "I ";
		args.ptrs[cnt++] = (char *) "R";
		args.ptrs[cnt++] = (char *) "INPUT   ";
		args.ptrs[cnt++] = (char *) pfkeyrecvr;
		args.ptrs[cnt++] = (char *) "0001";
		args.ptrs[cnt++] = (char *) "WSORT ";
		args.ptrs[cnt++] = (char *) &N[1];
					   /*123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
		args.ptrs[cnt++] = (char *) "                   Enter the INPUT file for WSORT                               ";
		args.ptrs[cnt++] = (char *) &N[79];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "FILE    ";
		args.ptrs[cnt++] = (char *) sort.infile;
		args.ptrs[cnt++] = (char *) &N[8];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "IN ";
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[26];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LIBRARY ";
		args.ptrs[cnt++] = (char *) sort.inlib;
		args.ptrs[cnt++] = (char *) &N[8];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[29];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "ON ";
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[49];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "VOLUME ";
		args.ptrs[cnt++] = (char *) sort.invol;
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[52];
		args.ptrs[cnt++] = (char *) "L";

#ifdef unix
		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "FILETYPE";
		args.ptrs[cnt++] = (char *) filetype;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[15];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "I=INDEXED F=FIXED N=NEWLINE";
		args.ptrs[cnt++] = (char *) &N[27];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[15];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[27];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "RECSIZE ";
		args.ptrs[cnt++] = (char *) in_recsize;
		args.ptrs[cnt++] = (char *) &N[5];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[17];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "(Supply only when FILETYPE=F)";
		args.ptrs[cnt++] = (char *) &N[29];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[17];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[27];
#endif /* unix */

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "     Press PF16 to Terminate WSORT            ";
		args.ptrs[cnt++] = (char *) &N[40];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];

		args.ptrs[cnt++] = (char *) "E";

		args.ptrs[cnt++] = (char *) "P";
		args.ptrs[cnt++] = (char *) &pfkey;

		wvaset(&two);
		GETPARM(&args,&cnt);							/* Use the new method			*/

		if (pfkeyrecvr[0] != '@') exitsort(16,"Terminated by PF16");

		/*
			KEYS getparm
		*/

		cnt = 0;
		args.ptrs[cnt++] = (char *) "I ";
		args.ptrs[cnt++] = (char *) "R";
		args.ptrs[cnt++] = (char *) "KEYS    ";
		args.ptrs[cnt++] = (char *) pfkeyrecvr;
		args.ptrs[cnt++] = (char *) "0002";
		args.ptrs[cnt++] = (char *) "WSORT ";
		args.ptrs[cnt++] = (char *) &N[1];
					   /*123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
		args.ptrs[cnt++] = (char *) "                     Enter the KEYS for WSORT                                    ";
		args.ptrs[cnt++] = (char *) &N[79];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "KEYS    ";
		args.ptrs[cnt++] = (char *) in_numkeys;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[10];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "(BCDFLPZN2TOU)";
		args.ptrs[cnt++] = (char *) &N[14];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "(A,D)";
		args.ptrs[cnt++] = (char *) &N[5];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[12];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[66];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST1   ";
		args.ptrs[cnt++] = (char *) sort.post1;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[13];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH1 ";
		args.ptrs[cnt++] = (char *) sort.length1;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[13];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE1   ";
		args.ptrs[cnt++] = (char *) sort.type1;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[13];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER1  ";
		args.ptrs[cnt++] = (char *) sort.order1;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[13];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST2   ";
		args.ptrs[cnt++] = (char *) sort.post2;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[14];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH2 ";
		args.ptrs[cnt++] = (char *) sort.length2;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[14];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE2   ";
		args.ptrs[cnt++] = (char *) sort.type2;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[14];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER2  ";
		args.ptrs[cnt++] = (char *) sort.order2;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[14];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST3   ";
		args.ptrs[cnt++] = (char *) sort.post3;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[15];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH3 ";
		args.ptrs[cnt++] = (char *) sort.length3;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[15];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE3   ";
		args.ptrs[cnt++] = (char *) sort.type3;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[15];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER3  ";
		args.ptrs[cnt++] = (char *) sort.order3;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[15];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST4   ";
		args.ptrs[cnt++] = (char *) sort.post4;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH4 ";
		args.ptrs[cnt++] = (char *) sort.length4;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE4   ";
		args.ptrs[cnt++] = (char *) sort.type4;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER4  ";
		args.ptrs[cnt++] = (char *) sort.order4;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST5   ";
		args.ptrs[cnt++] = (char *) sort.post5;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[17];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH5 ";
		args.ptrs[cnt++] = (char *) sort.length5;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[17];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE5   ";
		args.ptrs[cnt++] = (char *) sort.type5;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[17];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER5  ";
		args.ptrs[cnt++] = (char *) sort.order5;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[17];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST6   ";
		args.ptrs[cnt++] = (char *) sort.post6;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[18];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH6 ";
		args.ptrs[cnt++] = (char *) sort.length6;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[18];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE6   ";
		args.ptrs[cnt++] = (char *) sort.type6;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[18];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER6  ";
		args.ptrs[cnt++] = (char *) sort.order6;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[18];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST7   ";
		args.ptrs[cnt++] = (char *) sort.post7;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[19];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH7 ";
		args.ptrs[cnt++] = (char *) sort.length7;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[19];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE7   ";
		args.ptrs[cnt++] = (char *) sort.type7;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[19];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER7  ";
		args.ptrs[cnt++] = (char *) sort.order7;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[19];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "POST8   ";
		args.ptrs[cnt++] = (char *) sort.post8;
		args.ptrs[cnt++] = (char *) &N[4];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LENGTH8 ";
		args.ptrs[cnt++] = (char *) sort.length8;
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "I";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "TYPE8   ";
		args.ptrs[cnt++] = (char *) sort.type8;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[41];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "ORDER8  ";
		args.ptrs[cnt++] = (char *) sort.order8;
		args.ptrs[cnt++] = (char *) &N[1];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[56];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "     Press PF16 to Terminate WSORT            ";
		args.ptrs[cnt++] = (char *) &N[40];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];

		args.ptrs[cnt++] = (char *) "E";

		args.ptrs[cnt++] = (char *) "P";
		args.ptrs[cnt++] = (char *) &pfkey;

		wvaset(&two);
		GETPARM(&args,&cnt);							/* Use the new method			*/

		if (pfkeyrecvr[0] != '@') exitsort(16,"Terminated by PF16");


		/*
			OUTPUT getparm
		*/

		cnt = 0;
		args.ptrs[cnt++] = (char *) "I ";
		args.ptrs[cnt++] = (char *) "R";
		args.ptrs[cnt++] = (char *) "OUTPUT  ";
		args.ptrs[cnt++] = (char *) pfkeyrecvr;
		args.ptrs[cnt++] = (char *) "0003";
		args.ptrs[cnt++] = (char *) "WSORT ";
		args.ptrs[cnt++] = (char *) &N[1];
					   /*123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 */
		args.ptrs[cnt++] = (char *) "                   Enter the OUTPUT file for WSORT                              ";
		args.ptrs[cnt++] = (char *) &N[79];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "FILE    ";
		args.ptrs[cnt++] = (char *) sort.outfile;
		args.ptrs[cnt++] = (char *) &N[8];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "IN ";
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[26];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "LIBRARY ";
		args.ptrs[cnt++] = (char *) sort.outlib;
		args.ptrs[cnt++] = (char *) &N[8];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[29];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "ON ";
		args.ptrs[cnt++] = (char *) &N[3];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[49];

		args.ptrs[cnt++] = (char *) "K";
		args.ptrs[cnt++] = (char *) "VOLUME ";
		args.ptrs[cnt++] = (char *) sort.outvol;
		args.ptrs[cnt++] = (char *) &N[6];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[16];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[52];
		args.ptrs[cnt++] = (char *) "L";

		args.ptrs[cnt++] = (char *) "T";
		args.ptrs[cnt++] = (char *) "     Press PF16 to Terminate WSORT            ";
		args.ptrs[cnt++] = (char *) &N[40];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[24];
		args.ptrs[cnt++] = (char *) "A";
		args.ptrs[cnt++] = (char *) &N[20];

		args.ptrs[cnt++] = (char *) "E";

		args.ptrs[cnt++] = (char *) "P";
		args.ptrs[cnt++] = (char *) &pfkey;

		wvaset(&two);
		GETPARM(&args,&cnt);							/* Use the new method			*/

		if (pfkeyrecvr[0] != '@') exitsort(16,"Terminated by PF16");

	}

	if (verbose)
	{
		vexit();
		vexitdone = 1;
		printf("WSORT: Version=[%s] Library=[%d] Screen=[%d]\n\n",WISP_VERSION, LIBRARY_VERSION, SCREEN_VERSION);
		printf("Sorting input  file %8.8s in %8.8s on %6.6s\n",sort.infile, sort.inlib, sort.invol);
		printf("        output file %8.8s in %8.8s on %6.6s\n",sort.outfile, sort.outlib, sort.outvol);
	}

#ifdef unix
	recsize = 0;
	sscanf(in_recsize,"%d",&recsize);
	wswap(&recsize);

	WISPSORT(&sort,filetype,&recsize,&sortcode,&retcode);
#endif
#ifdef VMS
	SORTCALL(&sort,&retcode);
#endif

	wswap(&retcode);
	switch(retcode)
	{
	case  0:	message = "";						break;
	case  4:	message	= "Input file empty";				break;
	case  8:	message = "Insufficient buffer sapce";			break;
	case 12:	message = "Record size greater then 2024 bytes";	break;
	case 16:	message = "Invalid sort key";				break;
#ifdef VMS
	case 20:	message = "Program Check";				break;
#endif
#ifdef unix
	case 40:	message = "Invalid file type";				break;
	case 41:	message	= "No INPUT file or access denied";		break;
	case 42:	message = "Not an ACUCOBOL/Vision file";		break;
	case 43:	message = "Unable to get recsize from Vision";		break;
	case 44:	message = "Unable to unload Vision file";		break;
	case 45:	message = "Not a CISAM file";				break;
	case 46:	message = "Unable to get recsize from CISAM";		break;
	case 47:	message = "Unable to unload CISAM file";		break;
	case 20:
		wswap(&sortcode);
		switch(sortcode)
		{
		case ERR_NOINFILE:	message = "No INFILE";					break;
		case ERR_NOOUTFILE:	message = "No OUTFILE";					break;
		case ERR_BADRECSIZE:	message = "Invalid record size";			break;
		case ERR_BADFILETYPE:	message = "Invalid record type";			break;
		case ERR_BADNUMKEYS:	message = "Invalid number of sort keys";		break;
		case ERR_NOSORTKEYS:	message = "No sort keys";				break;
		case ERR_BADOFFSET:	message = "Invalid sort key offset";			break;
		case ERR_BADLENGTH:	message = "Invalid sort key length";			break;
		case ERR_BADDIRECTION:	message = "Invalid sort key direction";			break;
		case ERR_BADTYPE:	message = "Invalid sort key type";			break;
		case ERR_BINLEN:	message = "Invalid sort key length (BINARY 2 or 4)";	break;
		case ERR_FLOATLEN:	message = "Invalid sort key length (FLOAT 4 or 8)";	break;
		case ERR_OPENINPUT:	message = "Open for input failed";			break;
		case ERR_OPENOUTPUT:	message = "Open for output failed";			break;
		case ERR_MALLOC:	message = "Unable to malloc enough memory";		break;
	 	case ERR_READ:		message = "Read failed";				break;
		case ERR_WRITE:		message = "Write failed.";				break;
		case ERR_SIZEINT:	message = "Invalid size of integer";			break;
		case ERR_SIZEFLOAT:	message = "Invalid size of float";			break;
		case ERR_NORECEND:	message = "No recend supplied.";			break;
		case ERR_BADSIZEEND:	message = "Invalid size of recend";			break;
		case ERR_NORECORDS:	message = "Infile contains no records";			break;
		default: 		message = "UNKNOWN ERROR";				break;
		}
		break;
#endif
	default: 		message = "UNKNOWN ERROR";				break;
	}

	exitsort(retcode,message);
}

static exitsort(num,message)
int	num;
char	*message;
{
	if (!vexitdone)
	{
		vexit();
	}

	if (num && verbose)
	{
		printf("WSORT: [%d] %s\n",num,message);
	}

#ifdef unix
	exit(num);
#endif
#ifdef VMS
	if ( num == 0 )	exit(1);
	else		exit(num*1000);
#endif
}



