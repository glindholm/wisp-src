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
	wdecrypt	A file de-encryption utility.

			De-encrypts a -W file that was encrypted with wencrypt.
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
	int	klen, flen, offset, c, x, fcnt;

	if ( argc < 2 ) printusage(1,0);

	kp = getenv( "WCRYPT" );
	if ( ! kp ) printusage(3,0);

	klen = strlen(kp);
	if ( klen < 4  ) printusage(4,0);
	if ( klen > 80 ) printusage(5,0);

	for(fcnt=1; fcnt<argc; fcnt++)
	{
		strcpy(outfile, argv[fcnt]);

		flen = strlen( outfile );
		if ( outfile[flen-2] != '-' ||
		     outfile[flen-1] != 'W'   ) printusage(8,outfile);
		outfile[flen-2] = '\0';

		printf("wdecrypt: %s ==> %s\n",argv[fcnt],outfile);

		fp_in = fopen( argv[fcnt], "r" );
		if ( ! fp_in ) printusage(2,argv[fcnt]);

		fp_out = fopen( outfile, "w" );
		if ( ! fp_out ) printusage(2,outfile);

		offset=0;

		while( EOF != (c=fgetc(fp_in)) )
		{
			x = (c + 256 - kp[offset]) % 256;
			offset = (offset+1) % klen;
			if ( EOF == fputc( x, fp_out) ) printusage(6,outfile);
		}

		if ( EOF == fclose(fp_in)  ) printusage(7,argv[fcnt]);
		if ( EOF == fclose(fp_out) ) printusage(7,outfile);
	}
	exit(0);
}

static printusage(err,fname)
int	err;
char	*fname;
{

	switch(err)
	{
	case 1:	printf("\nwdecrypt: Invalid number of arguments\n"); break;
	case 2:	printf("\nwdecrypt: Unable to open [%s]\n", fname); break;
	case 3:	printf("\nwdecrypt: WCRYPT not set\n"); break;
	case 4:	printf("\nwdecrypt: WCRYPT must be at least 4 characters\n"); break;
	case 5:	printf("\nwdecrypt: WCRYPT is greater then 80 characters\n"); break;
	case 6:	printf("\nwdecrypt: Error writing [%s]\n", fname); break;
	case 7:	printf("\nwdecrypt: Error closing [%s]\n", fname); break;
	case 8:	printf("\nwdecrypt: File is not a -W file [%s]\n", fname); break;
	}

	printf("\n");
	printf("Usage:	wdecrypt {filename}\n");
	printf("\n");
	printf("        WCRYPT={key}\n");
	exit(1);
}

/*
**	History:
**	$Log: wdecrypt.c,v $
**	Revision 1.8  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.7  1996/07/23 18:13:06  gsl
**	drcs update
**	
**
**
*/
