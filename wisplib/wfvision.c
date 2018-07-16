			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	ACUCOBOL is defined for unix and MSDOS (when DACU defined).
*/

#ifdef unix
#define ACUCOBOL
#endif
#ifdef DACU
#define ACUCOBOL
#endif

#include "idsistd.h"

#ifdef ACUCOBOL

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef SCO
/*	XENIX_386  is used by ACUCOBOL for SC0 386 machines */
#define XENIX_386
#endif /* SCO */

#include "visint.h"

struct vision3_struct
{
	char	magic[4];
	char	version[2];
	char	blk_mult[2];
	char	pre_alloc[2];
	char	extension[2];
	char	filesize[4];
	char	next_blk[4];
	char	next_rec[4];
	char	first_rec[4];
	char	free_rec[4];
	char	root_vers[4];
	char	free_node[4];
	char	rec_count[4];
	char	del_rec[4];
	char	next_uniq[4];
	char	intern_vers[4];
	char	collate[4];
	char	node_usage[4];
	char	free_fails[2];
	char	open_cnt[2];
	char	nodes_used[2];
	char	nodes_free[2];
	char	rec_overhead[2];
	char	num_dups[2];
	char	_reserved1_[20];
	char	max_rec[2];
	char	min_rec[2];
	char	num_keys[1];
	char	compress[1];
	char	encrypt[1];
	char	max_key[1];
	char	_reserved2_[24];
	char	comment[32];
	char	keys[352];
};

int4 acuvision( path, code, field )					/* ACUCOBOL Vision file system interface		*/
char 	*path;								/* File path						*/
char	*code;								/* Function code					*/
char	*field;
{
	struct	_phys_hdr phdr;						/* Struct for first header record.			*/
	struct	_log_hdr  lhdr;						/* Struct for second header record.			*/
	struct	vision3_struct v3;					/* Vision3 struct					*/
	int	f;							/* File handle						*/
	int	acumagic;						/* is this an acucobol indexed file			*/
	int	i0, i1, i2;
	int4	*size;
	int	vision3;
	char	buff[256];
	short	tshort;

	vision3 = 0;

	size = (int4 *) field;


	f = open( path, O_RDONLY );					/* Open the file					*/

	if ( f == -1 )
	{
		return( 44 );
	}

	i0 = read( f, buff, 6);						/* Read the magic number plus version			*/
	if (i0 == -1)
	{
		close(f);
		return(44);
	}

	if ( i0 < 6 )
	{
		acumagic = 0;
		vision3 = 0;
	}
	else
	{
		if (buff[0] == 0x10 && buff[1] == 0x12 && buff[2] == 0x14 && buff[3] == 0x16 &&
		    buff[4] == 0x00 && buff[5] == 0x03 )
		{
			acumagic = 1;
			vision3 = 1;
			i1 = read( f, &((char *) &v3)[6], sizeof(v3)-6 );		/* read the rest of the header		*/
		}
		else
		{
			vision3 = 0;
			memcpy((char *)&phdr,buff,6);
			if ( phdr.magic == 0x10121416 || phdr.magic == 0x16141210 ) 
			{
				acumagic = 1;
				i1 = read( f, &((char *) &phdr)[6], sizeof(phdr)-6 );	/* read the rest of the header		*/

				i2 = read( f, (char *) &lhdr, sizeof( lhdr ) );		/* read the second header		*/

			}
			else 
			{
				acumagic = 0;
			}
		}
	}



	if ( memcmp( code, "RC", 2 ) == 0 )				/* If looking for record count				*/
	{
		if ( vision3 )
		{
			if ( !bytenormal() ) reversebytes(v3.rec_count,4);
			memcpy(size,v3.rec_count,4);
		}
		else if ( acumagic ) 	*size = phdr.total_records;
		else			*size = (i0 >= 1) ? 1 : 0;	/* return 1 or 0					*/
		close(f);
		return( 0 );
	}

	if ( memcmp( code, "RS", 2 ) == 0 ||				/* If looking for record size/lenght			*/
	     memcmp( code, "RL", 2 ) == 0    )
	{
		if ( vision3 )
		{
			if ( !bytenormal() ) reversebytes(v3.max_rec,2);
			memcpy(&tshort,v3.max_rec,2);
			*size = tshort;
		}
		else if ( acumagic ) *size = lhdr.max_rec_size;
		else		     *size = 256;
		close(f);
		return( 0 );
	}

	if ( memcmp( code, "FT", 2 ) == 0 )				/* If looking for file type				*/
	{
		if ( acumagic ) *field = 'I';
		else		*field = 'C';
		close(f);
		return( 0 );
	}

	if ( memcmp( code, "RT", 2 ) == 0 )				/* If looking for record type				*/
	{
		if (vision3)
		{
			if ( memcmp(v3.max_rec,v3.min_rec,2) == 0 ) *field = 'F';
			else					    *field = 'V';

			if ( v3.compress[0] ) *field = 'C';
		}
		else if ( acumagic ) 
		{
			if ( lhdr.var_recsize ) *field = 'V';
			else			*field = 'F';

			if ( lhdr.compressed ) 	*field = 'C';
		}
		else		*field = 'V';
		close(f);
		return( 0 );
	}

	close(f);
	return( 40 );
}
#else /* !ACUCOBOL */
int4 acuvision()
{
	werrlog(102,"acuvision() not implemented",0,0,0,0,0,0,0);
	return -1;
}
#endif /* !ACUCOBOL */



