/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
**
** $Author: gsl $
**
**
******************************************************************************
*/

/*
	wcrypt		A file encryption and decryption utility. This file is linked (ln) to two entry points "wencyypt" and
			"wdecrypt".

	wencrypt	A file encryption utility.

			Creates a -W file that is an encrypted version of the input file.
			The encryption is based on two keys that are read from shell variables. The keys are simply random
			character strings that are used internally, can be up to 80 chars but must be at least 4 chars, and must
			not be the same length.

			Use routine wdecrypt to decrypt the file. You must of course use the same key.

	wdecrypt	A file de-encryption utility.

			De-encrypts a -W file that was encrypted with wencrypt.
*/

#include <stdio.h>
#include <errno.h>

char *getenv();
char *what;

#define KEY1 "WCRYPT1"
#define KEY2 "WCRYPT2"

main(argc,argv)
int	argc;
char	*argv[];
{
	FILE	*fp_in;							/* The input file pointer				*/
	FILE	*fp_out;						/* The output file pointer				*/
	char	*kp1, *kp2;						/* The KEY pointer					*/
	char	outfile[256];
	int	klen1, klen2, offset1, offset2, c, x, fcnt, flen, i, rc;
	int	WENCRYPT, parity, bytecnt;
	int	TESTING;
	char	*ptr;

	TESTING=0;
	ptr = getenv( "TESTING" );
	if ( ptr )
	{
		if (0==strcmp(ptr,"ON")) 
		{
			TESTING=1;
			printf("\n**** TESTING=ON ****\n\n");
		}
	}

	what = argv[0];

	what = (char *) strrchr(argv[0],'/');
	if ( what ) 	what += 1;
	else		what = argv[0];

	if      ( 0==strcmp(what,"wcrypt"  ) ) wcrypt();
	else if ( 0==strcmp(what,"wencrypt") ) WENCRYPT=1;
	else if ( 0==strcmp(what,"wdecrypt") ) WENCRYPT=0;
	else printusage(12,0);

	if ( argc < 2 ) printusage(1,0);

	kp1 = getenv( KEY1 );
	if ( ! kp1 ) printusage(3,KEY1);

	klen1 = strlen(kp1);
	if ( klen1 < 4  ) printusage(4,KEY1);
	if ( klen1 > 80 ) printusage(5,KEY1);

	kp2 = getenv( KEY2 );
	if ( ! kp2 ) printusage(3,KEY2);

	klen2 = strlen(kp2);
	if ( klen2 < 4  ) printusage(4,KEY2);
	if ( klen2 > 80 ) printusage(5,KEY2);

	if ( klen1 == klen2 ) printusage(9,0);

	if (TESTING)
	{
		printf("%s=%s [len=%d]\n",KEY1,kp1,klen1);
		printf("%s=%s [len=%d]\n",KEY2,kp2,klen2);
	}

	parity=0;
	for ( i=0; i<klen1; i++ ) parity += (int) kp1[i];
	for ( i=0; i<klen2; i++ ) parity += (int) kp2[i];
	parity = parity % 128;

	if (TESTING)
	{
		printf("parity=%d [%x]\n",parity,parity);
	}

	for( fcnt=1; fcnt<argc; fcnt++ )
	{
		strcpy(outfile, argv[fcnt]);
		if (WENCRYPT)
		{
			strcat(outfile, "-W");
		}
		else
		{
			flen = strlen( outfile );
			if ( outfile[flen-2] != '-' ||
			     outfile[flen-1] != 'W'   ) printusage(8,outfile);
			outfile[flen-2] = '\0';
		}

		printf("%s: %s ==> %s\n",what,argv[fcnt],outfile);

		fp_in = fopen( argv[fcnt], "r" );
		if ( ! fp_in ) printusage(2,argv[fcnt]);

		fp_out = fopen( outfile, "w" );
		if ( ! fp_out ) printusage(2,outfile);

		if (WENCRYPT)
		{
			x = parity;
			if ( EOF == fputc( x, fp_out) ) printusage(6,outfile);
		}
		else
		{
			if ( EOF == (c=fgetc(fp_in)) ) 	printusage(10,argv[fcnt]);
			if ( parity != c ) 		printusage(11,argv[fcnt]);
		}

		offset1=0;
		offset2=0;
		bytecnt=0;

		while( (int)EOF != (c=fgetc(fp_in)) )
		{
			bytecnt +=1;
			if (WENCRYPT)	x = (c + 256 + kp1[offset1] - kp2[offset2]) % 256;
			else		x = (c + 256 - kp1[offset1] + kp2[offset2]) % 256;

			if (TESTING)
			{
				printf("[%08d] c=0x%08x kp1[%02d]=0x%08x kp2[%02d]=0x%08x x=0x%08x\n",
					bytecnt,c,offset1,kp1[offset1],offset2,kp2[offset2],x);
			}  

			offset1 = (offset1+1) % klen1;
			offset2 = (offset2+1) % klen2;

			if ( (int)EOF == (rc=fputc( x, fp_out)) ) 
			{
				if (x != 255) printusage(6,outfile);
			}
		}

		if ( EOF == fclose(fp_in)  ) printusage(7,argv[fcnt]);
		if ( EOF == fclose(fp_out) ) printusage(7,outfile);
	}
	exit(0);
}

static printusage(err,buff)
int	err;
char	*buff;
{

	switch(err)
	{
	case  0: break;
	case  1: printf("\n%s: Invalid number of arguments\n", what); break;
	case  2: printf("\n%s: Unable to open [%s]\n", what, buff); break;
	case  3: printf("\n%s: %s not set\n", what, buff); break;
	case  4: printf("\n%s: %s must be at least 4 characters\n", what, buff); break;
	case  5: printf("\n%s: %s is greater then 80 characters\n", what, buff); break;
	case  6: printf("\n%s: Error writing [%s] [errno=%d]\n", what, buff, errno); break;
	case  7: printf("\n%s: Error closing [%s] [errno=%d]\n", what, buff, errno); break;
	case  8: printf("\n%s: File is not a -W file [%s]\n", what, buff); break;
	case  9: printf("\n%s: %s and %s can not be the same length\n", what, KEY1, KEY2); break;
	case 10: printf("\n%s: File is empty [%s]\n", what, buff); break;
	case 11: printf("\n%s: Incorrect key(s) for file [%s]\n", what, buff); break;
	case 12: printf("\nwcrypt: Unknown link [%s]\n", what); break;
	default: printf("\n%s: Unknown error [%d]\n", what, err); break;
	}

	printf("\n");
	printf("Usage:	%s {filename} ...\n", what);
	printf("\n");
	printf("        %s={key1}\n", KEY1);
	printf("        %s={key2}\n", KEY2);
	exit(1);
}

wcrypt()
{
	printf("\n\n");
	printf("wcrypt:  This utility will encrypt and decrypt files using a\n");
	printf("         two key algorithm. The keys are read from shell variables\n");
	printf("         so you do not have to enter them on the command line.\n");
	printf("         wcrypt must first be linked (ln) to the file names wencrypt\n");
	printf("         and wdecrypt, these are the entry points.\n");
	printf("\n");
	printf("             $ ln wcrypt wencrypt\n");
	printf("             $ ln wcrypt wdecrypt\n");
	printf("\n");
	printf("         The keys are set as follows:\n");
	printf("\n");
	printf("             $ %s={key1}\n", KEY1);
	printf("             $ %s={key2}\n", KEY2);
	printf("             $ export %s %s\n", KEY1, KEY2);
	printf("\n");
	printf("         The keys are simple stings of 4 to 80 characters. The two\n");
	printf("         keys can not be the same length, this would greatly reduce\n");
	printf("         the security.\n");
	printf("\n");
	exit(0);
}


/*
**	History:
**	$Log: wcrypt.c,v $
**	Revision 1.9  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.8  1996/07/23 18:13:06  gsl
**	drcs update
**	
**
**
*/
