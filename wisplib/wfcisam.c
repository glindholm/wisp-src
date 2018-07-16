			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	cisaminfo:	Returns readfdr info for cisam files.
			Also partially supports Micro Focus 1.3 "FH ISAM" type files.  They look like cisam because they
			use 2 files "file" and "file.idx".
*/

/*
	cisam index header layout

	offset	length
	  0	  2	Magic number "FE53"
	  2	  4	?
	  6	  2	Blocksize-1
	  8	  2	Number of index keys (0=relative file)
	 10	  3	?
	 13	  2	Record Length
	 15	  4	?
	 19	  6	?
	 25	  4	? (Next index record)
	 29	  4	?
	 33	  4	Number of records (before deletes)
	 37	  4	Number of index records used


	Micro Focus "FH ISAM" type files.  These files support data compression and variable length records.

	offset	length
	  0	  2	Magic number "31FE" (.idx)   "307E" (.dat)
	 41	  1	Data compression 00=not compressed
	 56	  2	Max record size
	 60	  2	Min record size

	200	  	Key info
	200	  2     number of keys = x-6/12   (first key is 18 bytes each alt key is 12 bytes)
	206	 12     Primary key
	218      12     Alternate key 1
	230      12     Alternate key 2 ...

			0  1  2  3  4  5  6  7  8  9  10 11
			00 0C 00 00 04 00 00 00 xx yy yy 00
			00 0C 00 00 06 00 00 dd xx yy yy 00
			00 0C 00 00 08 00 00 dd xx yy yy 00
			      a1    a2       b  c  d

			a1)	+2  4\	Maybe the starting block for key data +4:2 or +2:4
			a2)	+4  2/
			b)	+7  1	dupilcates allowed  00-no  80-yes
			c)	+8  1	key length
			d)	+9  2   key offset

	I did some investigation into the internals of a FH ISAM file.
	There doesn't seem to be a record count available.
	Starting a x400 is the primary keys data. The first 2 bytes of every block is a "used size" for that block, the high bit is
	not part of the size and is often set on - reason unknown.
	While writing it leaves half the block empty before starting next block.  When keys exceed 1 block then goes to
	a multi-level index scheme with the first block (x400) pointing to later blocks. Was not able to tell if there
	is maintained a index-levels counter or if there is an indicator that says the pointer is to another other index
	or data block.  Probably an indicator - maybe the last four bytes of block.  The 4th last byte of block
	seems to indicate the key x00-primary  x01- first alternate  x02- second alternate.
	An x80 in the 3rd last byte seem to mean that the block contain data pointers.  A x00 in the third last byte seems to mean
	a single level index scheme.

	Note:   If there are more index key definitions then fit in the x200 block then the key data probably start later then
		block x400. 
		It is possible that other block sizes are supported.
*/

#ifdef unix
#define CISAM
#endif
#ifdef DMF
#define CISAM
#endif

#include "idsistd.h"

#ifdef CISAM

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef	PROTOTYPES_ALLOWED						/* Prototypes are not part of K&R C.			*/
static	int4	chars_to_int( unsigned char *cp, int sz );		/* Convert string from cisam file into integer.		*/
#else
static	int4	chars_to_int( );					/* Convert string from cisam file into integer.		*/
#endif
										/* The following 2 macros are used to convert	*/
										/* multiple byte values in a cisam index header	*/
										/* into machine byte-order independent short	*/
										/* or 4-byte integers.  In a cisam index file,	*/
										/* the high-order byte always comes first	*/
										/* regardless of the machine architecture.	*/
#define	CTOS(p) ( (short) chars_to_int( (p), 2 ) )				/* 2 character bytes to short integer.		*/
#define CTOL(p) ( (int4)  chars_to_int( (p), 4 ) )				/* 4 character bytes to 4 byte integer.		*/

static int cisamRC();

int4 cisaminfo( path, code, field )					/* CISAM file system interface				*/
char 	*path;								/* File path						*/
char	*code;								/* Function code					*/
char	*field;								/* Return info						*/
{
	char	l_path[132];						/* local path						*/
	int	f;							/* File handle						*/
	int	i1, i2;
	int4	*size;
	int	is_cisam;
	int	is_mffh;
	unsigned char	fheader[256];

	is_mffh = 0;

	size = (int4 *) field;

	strcpy(l_path,path);
	strcat(l_path,".idx");

	if (!fexists(l_path))
	{
		is_cisam = 0;						/* No .idx so must be seq file				*/
		strcpy(l_path,path);
	}
	else
	{
		is_cisam = 1;
	}

	f = open( l_path, O_RDONLY );					/* Open the file					*/

	if ( f == -1 )
	{
		return( 44 );
	}

	i1 = read( f, fheader, sizeof(fheader) );			/* read the header					*/
	close(f);

	if ( i1 == -1 )
	{
		return( 44 );
	}

	if ( is_cisam )
	{
		if      ( i1 > 2 && (fheader[0] == 0xFE && fheader[1] == 0x53) ) is_cisam = 1;
		else if ( i1 > 2 && (fheader[0] == 0x31 && fheader[1] == 0xFE) ) { is_mffh = 1; is_cisam = 0; }
		else return(68);
	}

	if ( memcmp( code, "RC", 2 ) == 0 )				/* If looking for record count				*/
	{
		if ( is_cisam ) 
		{
			return( cisamRC(l_path,size) );
		}
		if ( is_mffh)
		{
			return( 40 );					/* Record count is not available			*/
		}
		else
		{
			*size = (i1 >= 1) ? 1 : 0;			/* return 1 or 0					*/
			return( 0 );
		}
	}

	if ( memcmp( code, "RS", 2 ) == 0 ||				/* If looking for record size/lenght			*/
	     memcmp( code, "RL", 2 ) == 0    )
	{
		if ( is_cisam )
		{
			*size = CTOS( &fheader[13] );
		}
		else if ( is_mffh )
		{
			*size = CTOS( &fheader[56] );
		}
		else	*size = 256;
		return( 0 );
	}

	if ( memcmp( code, "FT", 2 ) == 0 )				/* If looking for file type				*/
	{
		if ( is_cisam )
		{
			int numkeys;
			numkeys = CTOS( &fheader[8] );
			if (numkeys > 0) *field = 'I';
			else 		 *field = 'R';
		}
		else if ( is_mffh )
		{
			*field = 'I';
		}
		else			 *field = 'C';
		return( 0 );
	}

	if ( memcmp( code, "RT", 2 ) == 0 )				/* If looking for record type				*/
	{
		if ( is_cisam ) 
		{
			*field = 'F';
		}
		else if ( is_mffh )
		{
			if ( fheader[41] != '\0' )
				*field = 'C';
			else if ( CTOS( &fheader[56] ) == CTOS( &fheader[60] ) )
				*field = 'F';
			else
				*field = 'V';
		}
		else	*field = 'V';
		return( 0 );
	}

	return( 40 );
}

static int cisamRC(path,cnt)							/* Return the record count for a cisam file. */
char	*path;
int4	*cnt;
{
	FILE 	*fp;
	short	blocksize;
	unsigned char	buff[256];
	int	rc;
	int4	rcnt;									/* temp record count.			*/
	int4	nrec;									/* next record				*/
	int4	pos;									/* position to seek to			*/
	int4	ndel;									/* Position of next deleted record num.	*/

	fp = fopen(path,"r");	
	if ( !fp )
	{
		return(44);
	}

	rc = fread(buff,64,1,fp);
	if ( rc != 1 ) 
	{
		fclose(fp);
		return(44);
	}

	blocksize = 1 + CTOS( &buff[6] );
	rcnt = CTOL( &buff[33] );						/* The rec cnt before any deletes.		*/
	nrec = CTOL( &buff[25] );

	while(nrec)
	{
		pos = (nrec-1)*blocksize;
		if (rc = fseek(fp,pos,0))
		{
			fclose(fp);
			return(44);
		}

		rc = fread(buff,6,1,fp);
		if ( rc != 1 )
		{
			fclose(fp);
			return(44);
		}

		ndel = CTOS( &buff[0] );					/* Next deleted record number location.		*/
		if( ndel >= 6 )							/* Untrue sometimes if file is still open.	*/
		{
			rcnt -= (CTOS( &buff[0] ) - 6) / 4;			/* Subtract the deletes.			*/
		}
		nrec = CTOL( &buff[2] );
	}

	*cnt = rcnt;
	fclose(fp);
	return(0);
}

static	int4	chars_to_int( cp, sz )					/* Convert string from cisam file into integer.		*/
unsigned	char	*cp;						/* Pointer to first byte (high order) of string.	*/
int	sz;								/* Numer of bytes to convert; 2 = short, 4 = int4.	*/
{
	int4	val;							/* Running total of characters processed so far.	*/
	int	i;							/* Index to each character to process in for loop.	*/

	val = 0;							/* Always start val at zero.				*/
	for( i = 0 ; i < sz ; ++i )					/* look at each character one at a time.		*/
	{
		val = ( val << 8 ) | ( 255 & (int4)cp[i] );		/* Shift previous val by 1 byte, then add next char.	*/
	}								/* "And" by 255L to insure getting just one byte.	*/

	return( val );							/* val holds the converted value of the characters.	*/
}
#else /* !CISAM */

int4 cisaminfo()
{
	werrlog(102,"cisaminfo() not implemented",0,0,0,0,0,0,0);
	return -1;
}

#endif /* !CISAM */
