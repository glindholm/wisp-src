/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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

/*
	wencrypt	A file encryption utility.

			Creates a -W file that is an encrypted version of the input file.
			The encryption is based on a KEY that is read from shell variable WCRYPT. WCRYPT is simply a random
			character string that is used internally,can be up to 80 chars but must be at least 4 chars.

			Use routine wdecrypt to decrypt the file. You must of course use the same key.
*/

#include <stdio.h>

char *getenv();

main(argc,argv)
int	argc;
char	*argv[];
{
	FILE	*fp_in;							/* The input file pointer				*/
	FILE	*fp_out;						/* The output file pointer				*/
	char	*kp;							/* The KEY pointer					*/
	char	outfile[256];
	int	klen, offset,c,x,fcnt;

	if ( argc < 2 ) printusage(1,0);

	kp = getenv( "WCRYPT" );
	if ( ! kp ) printusage(3,0);

	klen = strlen(kp);
	if ( klen < 4  ) printusage(4,0);
	if ( klen > 80 ) printusage(5,0);

	for( fcnt=1; fcnt<argc; fcnt++ )
	{
		strcpy(outfile, argv[fcnt]);
		strcat(outfile, "-W");

		printf("wencrypt: %s ==> %s\n",argv[fcnt],outfile);

		fp_in = fopen( argv[fcnt], "r" );
		if ( ! fp_in ) printusage(2,argv[fcnt]);

		fp_out = fopen( outfile, "w" );
		if ( ! fp_out ) printusage(2,outfile);

		offset=0;
	
		while( EOF != (c=fgetc(fp_in)) )
		{
			x = (c + kp[offset]) % 256;
			offset = (offset+1) % klen;
			if ( EOF == fputc( x, fp_out) ) printusage(6,outfile);
		}

		if ( EOF == fclose(fp_in)  ) printusage(7,argv[fcnt]);
		if ( EOF == fclose(fp_out) ) printusage(7,outfile);
	}
}

static printusage(err,fname)
int	err;
char	*fname;
{

	switch(err)
	{
	case 1:	printf("\nwencrypt: Invalid number of arguments\n"); break;
	case 2:	printf("\nwencrypt: Unable to open [%s]\n", fname); break;
	case 3:	printf("\nwencrypt: WCRYPT not set\n"); break;
	case 4:	printf("\nwencrypt: WCRYPT must be at least 4 characters\n"); break;
	case 5:	printf("\nwencrypt: WCRYPT is greater then 80 characters\n"); break;
	case 6:	printf("\nwencrypt: Error writing [%s]\n", fname); break;
	case 7:	printf("\nwencrypt: Error closing [%s]\n", fname); break;
	}

	printf("\n");
	printf("Usage:	wencrypt {filename}\n");
	printf("\n");
	printf("        WCRYPT={key}\n");
	exit(0);
}

/*
**	History:
**	$Log: wencrypt.c,v $
**	Revision 1.8  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.7  1996/07/23 18:13:08  gsl
**	drcs update
**	
**
**
*/
