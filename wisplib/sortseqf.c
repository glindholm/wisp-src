static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		sortseqf.c
**
**	Project:	WISP
**
**	RCS:		$Source:$
**
**	Purpose:	Sort a sequential file.
**
**	Routines:	
*/

#include <stdio.h>
#ifdef unix
#include <fcntl.h>
#endif

#include <errno.h>
#include <string.h>
#include <stdlib.h>

#if defined(WATCOM) || defined(_MSC_VER)
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <io.h>
#endif

#include "idsistd.h"
#include "sortseqf.h"
#include "idsisubs.h"
#include "wisplib.h"
#include "wispcfg.h"



#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif


/*
	sortseqf:	Sort a sequential file.


		- Validate data.
		- Open files.
		- Allocate large memory block
		- Load memory block
		- Sort memory block
		- Write/Merge memory block to temp file
		- Continue to Load Sort and Write until EOF
		- Merge temp files into outfile.
		- Cleanup 

	PARAMETERS:

	infile		The name of the file to sort.

	outfile		The name of the sorted output file.

	tmpdir		The name of the directory to use for temporary files. If NULL then "/tmp" is used.

	memsizek	The size in K (1024) to use for the in-memory portion of the sort. The mininum is 16K the max is
			8192K (8 Meg). If NULL then the default of 512K is used. Routine will error if it cannot malloc
			a minimun of 16K.

	filetype	This specifies the type of file: 
				'F' 	Fixed length.		Recsize must be specified. Recend is ignored.
				'N'	Newline terminated.	Recsize and recend are ignored.
				'V' 	Variable length. 	Recend must be specifed. Recsize is the size of recend.


	recsize		The record size for filetype = 'F'.

	recend		The record end character(s) for filetype ='V'. E.g. "\n" for standard text files.

	numkeys		The number of keys specified in sortkeys.

	sortkeys	Pointer to an array of struct s_sortkeys. The size of the array is based on numkeys. Each element
			contains 4 ints, an offset, length, direction, and type of data. Each element defines one key to
			sort on.
			
			Type of Data
			============
			KT_ASCII		0	Native character set collating sequence.
			KT_BINARY		1	Native binary (2 & 4 bytes only).
			KT_FLOAT		2	Floating point (4 & 8 bytes only).
			KT_DECIMAL		3	Decimal data (Leading spaces, decimal point, comas, sign, etc)
			KT_ZONED_LEAD		4	Zoned with leading separate sign.
			KT_ZONED_TRAIL		5	Zoned with trailing separate sign.
			KT_ZONED_RIGHT		6	Zoned with right overpunch sign.
			KT_ZONED_LEFT		7	Zoned with left overpunch sign.
			KT_PACKED		8	Packed signed.
			KT_PACKED_UNSIGNED	9	Packed unsigned.
			KT_BINARY_NORMAL	10	Binary in byte normal order.
			KT_BINARY_2N2		11	Binary (4 byte) with half-words swapped.

	errlevel	The level of error messages to report: 0 = report all errors, 1 = don't report warnings.

	errfile		The FILE pointer for reporting errors. If NULL then no error messages are written.

	errbuff		A buffer to place the last error message. If NULL then no errors are stored. This is used if you
			don't want error messages written to a file but you do want access to the message text.


	NOTES:

	(1)	The maximum record size is 16K
	(2)	If memsizek is 0 then 512 is assumed.
	(3)	If filetype is FIXED the recsize is used.
	(4)	If filetype is VARIABLE then recend is used and recsize is the size of recend.
	(5)	If errfile is NULL then NO error messages are written. 
	(6)	If type is BINARY then length must be 2 or 4.
	(7)	If type is FLOAT then length must be 4 or 8.
	(8)	If invalid data is in a sort key then the result in unpredictable
	(9)	For KT_DECIMAL the value must fit in a double float.
	(10)	Infile & Outfile can be the same as outfile is not opened until infile is closed.
	(11)	For KT_ASCII & KT_DECIMAL on a variable length file they will sort on less then the lenght if the full length
		is not available.
*/

#define DEF_SORTINDEX	5000
#define DEF_SMALL_REC	50
#define MAX_ERRSIZE	256
#define MAX_MEMSIZEK	65535
#define MIN_MEMSIZEK	16

#ifdef unix
#define DEF_MEMSIZEK	512
#endif /* unix */

#if defined (_INTELC32_) || defined (WATCOM)
#define DEF_MEMSIZEK	256
#endif

#ifdef WIN32
#define DEF_MEMSIZEK	512
#endif

#define DEF_ONE_K	1024
#define DEF_READSIZE	1024
#define DEF_SORTNAME	"sortseqf"

#define BIN_NATURAL	0
#define BIN_NORMAL	1
#define BIN_2N2		2

#define MAXPACKEDLENGTH	18

typedef float 	FLOAT4;
typedef double 	FLOAT8;

struct s_recindex
{
	int4	size;
	char	*rec;
};

static int	g_init_load;							/* Global Initialize loadmem()			*/
static int	g_init_cmp;							/* Global Init compare()			*/
static char	g_filetype;							/* Global filetype				*/
static int	g_recsize;							/* Global recsize				*/
static int	g_minrecsize;							/* Global mininum recsize			*/
static int	g_maxrecsize;							/* Global maxrecsize recsize			*/
static char	*g_recend;							/* Global recend				*/
static int	g_sizerecend;							/* Global size of recend string			*/
static int 	g_numkeys;							/* Global numkeys				*/
static struct s_recindex **g_sortindex, **curr_sndx;				/* Ptr to table of Index to recindex 		*/
static struct s_recindex *g_recindex, *curr_rndx;				/* Ptr to table of Index to records in memblock	*/
static struct s_sortkeys *g_sortkeys;						/* Global sortkeys				*/
static int	g_errlevel;							/* Global errlevel				*/
static FILE 	*g_errfile;							/* Global errfile				*/
static char	*g_errbuff;							/* Global errbuff				*/
static int	g_cmp_rc;							/* Global error code from cmp routines		*/
static int4	g_numelements;							/* Number of records in memblock		*/
static int4	g_total_rec;							/* Total number of records in infile.		*/
static int	g_infileopen;							/* Flag if infile is open.			*/
static int	g_outfileopen;							/* Flag if outfile is open.			*/
static int4	g_sortindex_size;						/* Local size of sort index table.		*/
static int4	g_sortindex_bytes;						/* Byte size of g_sortindex			*/
static int4	g_recindex_bytes;						/* Byte size of g_recindex			*/

static int loadmem( char *memblock, int4 memsize, char *infile, int f_infile, int *p_eof_infile, int *p_fully_loaded );
static char *memchrs(char *p1, int l1, char *p2, int l2);
static int sizerec(char *ptr, char *endptr);
static int checklen( struct s_recindex *index1, struct s_recindex *index2, int offset, int length, int *rc);
static int compare( /* struct s_recindex **p1, struct s_recindex **p2 */);
static int cmp_ascii(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);
static int cmp_binary(struct s_recindex *index1, struct s_recindex *index2, int offset, int length, int bintype);
static int cmp_float(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);
static int cmp_zonelead(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);
static int cmp_zonetrail(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);
static int cmp_zoneright(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);
static int cmp_zoneleft(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);
static int cmp_packed(struct s_recindex *index1, struct s_recindex *index2, int offset, int length, int issigned);
static int cmp_decimal(struct s_recindex *index1, struct s_recindex *index2, int offset, int length);

static void load_decimal(double *d, char *p, int len);
static int writemem(char *filename, int f_file);
static int mergemem(char *inname, int f_in, char *outname, int f_out);
static int reporterr(int errnum, char *messstr);
static void l_reversebytes(char *ptr, int len);
static int ssbytenormal(void);


/*
	The g_sortindex is a table of pointers, each pointer points to an entry in g_recindex which is a table
	if s_recindex structures.  Each item is g_recindex consists of a size and a data pointer.  When a qsort is
	performed it is passed g_sortindex, this table is sorted so that what it points to comes back sorted.


				g_recindex
	g_sortindex---->+-+     +-+-+
			| |---> | | |--->[data...]
			+-+     +-+-+
			| |---> | | |--->[data...]
			+-+     +-+-+
			| |---> | | |--->[data...]
			+-+     +-+-+
			| |---> | | |--->[data...]
			+-+     +-+-+
			| |---> | | |--->[data...]
			+-+     +-+-+


*/

/*
**	Routine:	sortseqf()
**
**	Function:	Sort a sequential file
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	infile		Input file to sort
**	outfile		Output file					
**	tmpdir		Temporary directory for work files		
**	memsizek	Size of memory block to alloc in K		
**	filetype	Type of file [F-Fixed, N-Newline,V-Variable]	
**	recsize;	Size of each record	(Fixed)			
**	recend;		Record terminator    (Variable)		
**	numkeys;	Number of sort keys			
**	sortkeys	The sort keys				
**	errlevel;	Error level 0-all 1-errors only		
**	errfile;	Where to report errors			
**	errbuff;	Store last error here  (Min 256 bytes)	
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by xxx
**
*/

int sortseqf( char *infile, char *outfile, char *tmpdir, int memsizek, char filetype, int recsize, char *recend, 
		int numkeys, struct s_sortkeys sortkeys[], int errlevel, FILE *errfile, char *errbuff )
{
	const char* l_tmpdir = (tmpdir) ? tmpdir : wtmpdir(NULL);		/* Local tmpdir.				*/
	int	l_memsizek;							/* Local memsizek.				*/
	char	messstr[80];							/* Error message string buffer.			*/
	int	i;								/* Temp loop counter index.			*/
	int	rc;								/* Return code.					*/
	char	tmpfile[80];							/* Temp filename buff				*/
	char	mrgfile[80];							/* Merge filename buff				*/
	char	swapfile[80];							/* Buffer used to swap the temp & merge files	*/
	int	f_infile;							/* File ptr for infile.				*/
	int	f_outfile;							/* File ptr for outfile.			*/
	int	f_tmpfile;							/* File ptr for tmpfile.			*/
	int	f_mrgfile;							/* File ptr for mrgfile.			*/
	int	f_swapfile;							/* File ptr used to swap the tmp & mrg files	*/
	int4	memsize;							/* Size of memory block allocated.		*/
	char	*memblock;							/* Large memblock for sort.			*/
	int	eof_infile;							/* Have we reached EOF for infile.		*/
	int	tmpfile_active;							/* Is there data in the tmpfile.		*/
	int	mrgfile_active;							/* Is there data in the mrgfile.		*/
	int	fully_loaded;							/* Flag if all records have been loaded & sorted*/ 
	char*	ptr;

	g_init_load = 1;							/* Initalize the loadmem() routine.		*/
	g_init_cmp = 1;								/* Initalize the compare() routine.		*/

	tmpfile_active = 0;							/* No temp file open				*/
	mrgfile_active = 0;							/* No merge file open				*/

	g_errfile = errfile;							/* Make errfile available to reporterr()	*/
#ifdef TESTING
	g_errfile = stderr;
#endif
	g_errbuff = errbuff;
	g_errlevel = errlevel;

	g_minrecsize = 1;
	g_maxrecsize = 1;

	/*
	**	Validate the parameters
	*/

	if (!infile)								/* If no infile then error.			*/
	{
		return(reporterr(ERR_NOINFILE,0));
	}
	g_infileopen = 0;

	if (!outfile)								/* If no outfile then error.			*/
	{
		return(reporterr(ERR_NOOUTFILE,0));
	}
	g_outfileopen = 0;

#ifdef TESTING
printf("tmpdir=%s\n\n\n\n",l_tmpdir);
#endif

	l_memsizek = (!memsizek) ? DEF_MEMSIZEK : memsizek;			/* If no memsizek then use default		*/
	l_memsizek = (l_memsizek < MIN_MEMSIZEK) ? MIN_MEMSIZEK : l_memsizek;
	l_memsizek = (l_memsizek > MAX_MEMSIZEK) ? MAX_MEMSIZEK : l_memsizek;
#ifdef TESTING
printf("sortseqf:l_memsizek=%d\n",l_memsizek);
#endif

	switch(filetype)							/* Validate filetype.				*/
	{
	case 'F':								/* If Fixed the validate recsize.		*/
		if (recsize < 1)
		{
			sprintf(messstr,"[recsize = %d]",recsize);
			return(reporterr(ERR_BADRECSIZE,messstr));
		}
		g_recsize = recsize;
		break;
	case 'N':
		g_recend = "\n";
		g_sizerecend = 1;
		break;
	case 'V':								/* If Variable then set "record-end".		*/
		if (!recend)
		{
			return(reporterr(ERR_NORECEND,0));
		}
		if (recsize < 1)
		{
			sprintf(messstr,"[recsize = %d]",recsize);
			return(reporterr(ERR_BADSIZEEND,messstr));
		}
		g_recend = recend;
		g_sizerecend = recsize;
		break;
	default:								/* Invalid filetype				*/
		sprintf(messstr,"[filetype = %c]",filetype);
		return(reporterr(ERR_BADFILETYPE,messstr));
	}
	g_filetype = filetype;							/* Make filetype global				*/

	if (numkeys < 1 || numkeys > SSF_MAX_NUMKEYS)				/* Validate the number of sort keys.		*/
	{
		sprintf(messstr,"[numkeys = %d]",numkeys);
		return(reporterr(ERR_BADNUMKEYS,messstr));
	}
	g_numkeys = numkeys;							/* Make numkeys global				*/

	if (!sortkeys)								/* Validate the sortkeys			*/
	{
		return(reporterr(ERR_NOSORTKEYS,0));
	}

	for ( i=0; i<numkeys; i++)
	{
		if ( sortkeys[i].offset < 0 )
		{
			sprintf(messstr,"[key = %d, offset = %d]", i+1, (sortkeys)[i].offset);
			return(reporterr(ERR_BADOFFSET,messstr));
		}

		if ( filetype == 'F' && (sortkeys)[i].offset >= recsize )
		{
			sprintf(messstr,"[key = %d, offset = %d, recsize = %d]", i+1, (sortkeys)[i].offset, recsize);
			return(reporterr(ERR_BADOFFSET,messstr));
		}

		if ((sortkeys)[i].length < 1)
		{
			sprintf(messstr,"[key = %d, length = %d]", i+1, (sortkeys)[i].length);
			return(reporterr(ERR_BADLENGTH,messstr));
		}

		if (filetype == 'F' && (sortkeys)[i].offset + (sortkeys)[i].length > recsize)
		{
			sprintf(messstr,"[key = %d, offset+length = %d, recsize = %d]", i+1, 
				(sortkeys)[i].length+(sortkeys)[i].offset,recsize);
			return(reporterr(ERR_BADLENGTH,messstr));
		}

		if ( (sortkeys)[i].offset + (sortkeys)[i].length > g_minrecsize )	/* Set g_minrecsize			*/
		{
			g_minrecsize = (sortkeys)[i].offset + (sortkeys)[i].length;
		}

		if ((sortkeys)[i].direction != KD_ASCENDING &&
		    (sortkeys)[i].direction != KD_DESCENDING  )
		{
			sprintf(messstr,"[key = %d, direction = %d]", i+1, (sortkeys)[i].direction);
			return(reporterr(ERR_BADDIRECTION,messstr));
		}
		switch((sortkeys)[i].type)
		{
		case KT_ASCII:
			break;
		case KT_BINARY:
		case KT_BINARY_NORMAL:
		case KT_BINARY_2N2:
			if ((sortkeys)[i].length != 2 && (sortkeys)[i].length != 4)
			{
				sprintf(messstr,"[key = %d, length = %d]", i+1, (sortkeys)[i].length);
				return(reporterr(ERR_BINLEN,messstr));
			}
			break;
		case KT_FLOAT:
			if ((sortkeys)[i].length != 4 && (sortkeys)[i].length != 8)
			{
				sprintf(messstr,"[key = %d, length = %d]", i+1, (sortkeys)[i].length);
				return(reporterr(ERR_FLOATLEN,messstr));
			}
			break;
		case KT_PACKED:
		case KT_PACKED_UNSIGNED:
			if ((sortkeys)[i].length > MAXPACKEDLENGTH)
			{
				sprintf(messstr,"[key = %d, length = %d]", i+1, (sortkeys)[i].length);
				return(reporterr(ERR_BADLENGTH,messstr));
			}
			break;
		case KT_DECIMAL:
		case KT_ZONED_LEAD:
		case KT_ZONED_TRAIL:
		case KT_ZONED_RIGHT:
		case KT_ZONED_LEFT:
			break;
		default:
			sprintf(messstr,"[key = %d, type = %d]", i+1, (sortkeys)[i].type);
			return(reporterr(ERR_BADTYPE,messstr));
			break;
		}
	}
	g_sortkeys = sortkeys;							/* Make sortkeys global				*/

	/*
	**	Open input file
	*/

	f_infile = open(infile,O_RDONLY|O_BINARY,0400);				/* Open infile.					*/

	if (f_infile == -1)
	{
		sprintf(messstr,"[infile = \"%s\" errno=%d]",infile,errno);
		return(reporterr(ERR_OPENINPUT,messstr));
	}
	eof_infile = 0;
	g_infileopen = 1;
	g_total_rec = 0;

#ifdef OLD
	sprintf(buff,"%s%06d.1",DEF_TMPPREFIX,pid);				/* Build temp file filename			*/
	buildfilepath(tmpfile, l_tmpdir, buff);
	sprintf(buff,"%s%06d.2",DEF_TMPPREFIX,pid);				/* Build merge file filename			*/
	buildfilepath(mrgfile, l_tmpdir, buff);
#endif /* OLD */

	ptr = tempnam(l_tmpdir,"sort");
	strcpy(tmpfile,ptr);
	strcpy(mrgfile,ptr);
	free(ptr);
	
	strcat(tmpfile, ".1");
	strcat(mrgfile, ".2");

	makepath(tmpfile);							/* Ensure the directory path exists		*/

	/*
	**	Allocate large memory block
	*/

	memsize = l_memsizek * DEF_ONE_K;

	for(;;)									/* Loop until malloc succeeds.			*/
	{

		memblock = (char *)malloc((size_t)memsize);
		if ( memblock )
		{
#ifdef TESTING
printf("sortseqf:memsize=%d\n",memsize);
#endif
			break;
		}

		if ( memsize <= MIN_MEMSIZEK * DEF_ONE_K )			/* Malloc failed.				*/
		{
			sprintf(messstr,"[memsize = %d]",memsize);
			rc = reporterr(ERR_MALLOC,messstr);
			close(f_infile);
			return(rc);
		}

		memsize = memsize / 2;
		if ( memsize < MIN_MEMSIZEK * DEF_ONE_K )			/* Try minimum size				*/
		{
			memsize = MIN_MEMSIZEK * DEF_ONE_K;
		}
	}

	/*
	**	Allocate memory for sort index table.
	*/

	if (filetype == 'F')								/* Set the needed table size.		*/
	{
		if (recsize < DEF_SMALL_REC)
			g_sortindex_size = ((l_memsizek * DEF_ONE_K)/DEF_SMALL_REC);	/* Handle small record case		*/
		else
			g_sortindex_size = ((l_memsizek * DEF_ONE_K)/recsize);
	}
	else g_sortindex_size = DEF_SORTINDEX;						/* Else use the default value.		*/

#ifdef TESTING
printf("sortseqf:g_sortindex_size=%d\n",g_sortindex_size);
#endif

	g_sortindex_bytes = sizeof(struct s_recindex *)*g_sortindex_size;		/* Calc bytes to alloc			*/

#ifdef TESTING
printf("sortseqf:g_sortindex_bytes=%d\n",g_sortindex_bytes);
#endif

	g_sortindex = (struct s_recindex **)malloc((size_t)g_sortindex_bytes); 		/* Ptr to Index to recindex 		*/
	if (!g_sortindex)								/* If malloc failed.			*/
	{
		sprintf(messstr,"[g_sortindex size = %d]",g_sortindex_size);
		rc = reporterr(ERR_MALLOC,messstr);
		return(rc);
	}

	g_recindex_bytes = sizeof(struct s_recindex)*g_sortindex_size;			/* Calc bytes to alloc			*/

#ifdef TESTING
printf("sortseqf:g_recindex_bytes=%d\n",g_recindex_bytes);
#endif

	g_recindex  = (struct s_recindex *)malloc((size_t)g_recindex_bytes);		/* Array of struct that point		*/
											/*  to records in memblock		*/
	if (!g_recindex)								/* If malloc failed.			*/
	{
		sprintf(messstr,"[g_recindex size = %d]",g_sortindex_size);
		rc = reporterr(ERR_MALLOC,messstr);
		return(rc);
	}
	g_numelements = 0;                   
	rc = 0;

	/*
	**	Loop until EOF on infile and all record loaded & sorted.
	*/

	fully_loaded = 0;
	while(!fully_loaded)							/* Loop until all records loaded & sorted	*/
	{
		/*
		** Load memory block
		*/

		rc = loadmem( memblock, memsize, infile, f_infile, &eof_infile, &fully_loaded );

		if ( rc > WARNING )
		{
			break;
		}

		g_total_rec += g_numelements;					/* Keep a running total cnt of records.		*/

		if (fully_loaded)						/* If EOF on infile then we are ready to open	*/
		{								/* and write to outfile. We don't open outfile	*/
										/* until infile is closed because they may be 	*/
										/* the same file.				*/

			if ( g_total_rec == 0 )					/* No records in infile.			*/
			{
				rc = reporterr(ERR_NORECORDS,0);
				break;
			}

			f_outfile = open(outfile,O_WRONLY|O_CREAT|O_TRUNC|O_BINARY,0666); /* Open outfile.			*/

			if (f_outfile == -1)
			{
				sprintf(messstr,"[outfile = \"%s\" errno=%d]",outfile,errno);
				rc = reporterr(ERR_OPENOUTPUT,messstr);
				break;
			}
			g_outfileopen = 1;
		}

		g_cmp_rc = 0;

		/*
		**	Sort the in memory block of data
		*/
#ifdef TESTING
printf("qsort: %d items\n",g_numelements);
#endif

		qsort( (char *)g_sortindex, (size_t)g_numelements, sizeof(struct s_recindex *), (int(*)(const void*,const void*))compare);

		if ( g_cmp_rc > WARNING )
		{
			rc = g_cmp_rc;
			break;
		}

		/*
		**	Write it out.
		**
		**	(1) Full file sorted.		Write to outfile.
		**	(2) first chunk.		Write to tmpfile.
		**	(3) middle chunk.		Merge with tmpfile to mergefile, switch mergefile to tmpfile.
		**	(4) last chunk.			Merge with tmpfile to outfile.
		*/

		if (fully_loaded && !tmpfile_active)				/* (1) Full file sorted				*/
		{
			rc = writemem(outfile,f_outfile);				/* Write memblock to outfile sorted.	*/

			if ( rc > WARNING )
			{
				break;
			}
		}
		else if (!fully_loaded && !tmpfile_active)			/* (2) First chunk				*/
		{

			f_tmpfile = open(tmpfile,O_WRONLY|O_CREAT|O_TRUNC|O_BINARY,0666); /* Open tmpfile for output.		*/

			if (f_tmpfile == -1)
			{
				sprintf(messstr,"[tmpfile = \"%s\" errno=%d]",tmpfile,errno);
				rc = reporterr(ERR_OPENOUTPUT,messstr);
				break;
			}
			tmpfile_active = 1;						/* Data in tmpfile.			*/

			rc = writemem(tmpfile,f_tmpfile);				/* Write memblock to tmpfile sorted.	*/

			close(f_tmpfile);

			if ( rc > WARNING )
			{
				break;
			}
		}
		else if (!fully_loaded && tmpfile_active)			/* (3) Middle chunk				*/
		{
			f_tmpfile = open(tmpfile,O_RDONLY|O_BINARY,0400);		/* Open tmpfile for input.		*/

			if (f_tmpfile == -1)
			{
				sprintf(messstr,"[tmpfile = \"%s\" errno=%d]",tmpfile,errno);
				return(reporterr(ERR_OPENINPUT,messstr));
			}

			f_mrgfile = open(mrgfile,O_WRONLY|O_CREAT|O_TRUNC|O_BINARY,0666); /* Open mrgfile for output.		*/

			if (f_mrgfile == -1)
			{
				sprintf(messstr,"[mrgfile = \"%s\" errno=%d]",mrgfile,errno);
				rc = reporterr(ERR_OPENOUTPUT,messstr);
				break;
			}
			mrgfile_active = 1;						/* Merge file exists			*/

			rc = mergemem(tmpfile,f_tmpfile,mrgfile,f_mrgfile);		/* Merge memblock & tmpfile to mrgfile	*/

			close(f_tmpfile);
			close(f_mrgfile);

			if ( rc > WARNING )
			{
				break;
			}

			strcpy(swapfile,tmpfile);					/* Swap the temp & merge files		*/
			strcpy(tmpfile,mrgfile);
			strcpy(mrgfile,swapfile);

			f_swapfile = f_tmpfile;
			f_tmpfile  = f_mrgfile;
			f_mrgfile  = f_swapfile;

		}
		else if (fully_loaded && tmpfile_active)			/* (4) Last chunk				*/
		{
			f_tmpfile = open(tmpfile,O_RDONLY|O_BINARY,0400);		/* Open tmpfile for input.		*/

			if (f_tmpfile == -1)
			{
				sprintf(messstr,"[tmpfile = \"%s\" errno=%d]",tmpfile,errno);
				return(reporterr(ERR_OPENINPUT,messstr));
			}

			rc = mergemem(tmpfile,f_tmpfile,outfile,f_outfile);		/* Merge memblock & tmpfile to outfile	*/

			close(f_tmpfile);

			if ( rc > WARNING )
			{
				break;
			}
		}

	}

	/*
	**	Clean up
	*/
	free(memblock);
	free(g_sortindex);
	free(g_recindex);
	if (g_infileopen)
	{
		close(f_infile);
	}
	if (g_outfileopen)
	{
		close(f_outfile);
		if ( rc > WARNING )
		{
			unlink(outfile);
		}
	}
	if ( tmpfile_active )
	{
		unlink(tmpfile);
	}
	if ( mrgfile_active )
	{
		unlink(mrgfile);
	}

	return(rc);
}


/*
	loadmem:	Load the memory block from infile until full or EOF.
			Build the sortindex and return the numelements.
*/

static int loadmem( char *memblock, int4 memsize, char *infile, int f_infile, int *p_eof_infile, int *p_fully_loaded )
{
	static int4	memused;
	static char	*memptr;
	static char	*usedptr;
	char	messstr[80];
	int	readsize;
	int	readcnt;
	int	i;
	char	*ptr;
	int4	leftover;
	

	if (g_init_load)
	{
		g_init_load = 0;
		memused = 0;
		memptr = memblock;						/* Position to begining of block		*/
	}
	else
	{
		leftover = memptr - usedptr;					/* Leftover UNSORTED data in memblock		*/
		leftover = (leftover < 0) ? 0: leftover;			/* Correct leftover if less then 0		*/
		for (memptr=memblock, memused=0; memused < leftover; memused++) *memptr++ = *usedptr++;
										/* Move the leftover data to the beginning	*/
	}

	readsize = DEF_READSIZE;

	while(!(*p_eof_infile))
	{
		readcnt = read(f_infile, memptr, readsize);			/* read in 1k at a time.			*/

		if (readcnt == -1)						/* Error on READ				*/
		{
			sprintf(messstr,"[file = %s, errno = %d]",infile,errno);
			close(f_infile);
			g_infileopen = 0;
			return(reporterr(ERR_READ,messstr));
		}

		if (readcnt == 0)						/* EOF on infile.				*/
		{
			*p_eof_infile = 1;
			close(f_infile);
			g_infileopen = 0;
			break;
		}

		memused += readcnt;
		memptr += readcnt;

		if ( memused + DEF_READSIZE > memsize )				/* If next read will overflow memblock then	*/
		{
			readsize = memsize - memused;				/* reduce readsize.				*/
			if (readsize < 256) 					/* Too small to bother with.			*/
			{
				break;
			}
		}
	}

	if (memused < 1)							/* Catch the trival case of no data.		*/
	{
		g_numelements = 0;
		if (*p_eof_infile)
		{
			*p_fully_loaded = 1;
		}
		return(0);
	}

	/*
	**	Build sortindex
	*/

	curr_sndx = g_sortindex;						/* Set pointers for current table element.	*/
	curr_rndx = g_recindex;
	if (g_filetype == 'F')							/* Fixed length records				*/
	{
		g_numelements = memused / g_recsize;				/* Calculate number of records in memblock	*/
		g_maxrecsize = g_recsize;
		if (g_numelements > g_sortindex_size)
		{
			g_numelements = g_sortindex_size;
		}

		ptr = memblock;
		for( i = 0; i < g_numelements; i++ )				/* Initialize sortindex.			*/
		{
			curr_rndx->rec = ptr;
			curr_rndx->size = g_recsize;
			*curr_sndx = curr_rndx;
			curr_sndx++;						/* Step to next index.				*/
			curr_rndx++;
			ptr += g_recsize;
		}
		usedptr = ptr;							/* Point to used data in memblock		*/
	}
	else if ( g_filetype == 'V' || g_filetype == 'N' )
	{

		ptr = memblock;
		for( i = 0; i < g_sortindex_size; i++ )				/* Initialize sortindex.			*/
		{
			int size;

			size = sizerec(ptr,memptr);
			if ( size == 0 )
			{
				break;
			}
			curr_rndx->rec = ptr;
			curr_rndx->size = size;
			*curr_sndx = curr_rndx;
			curr_sndx++;						/* Step to next index.				*/
			curr_rndx++;
			ptr += size;
			if (size > g_maxrecsize)
			{
				g_maxrecsize = size;
			}
		}
		g_numelements = i;
		usedptr = ptr;							/* Point to used data in memblock		*/
	}

	if (*p_eof_infile && g_numelements < g_sortindex_size)			/* If EOF and sortindex not full then usedptr 	*/
	{									/* should equal memptr.				*/
		*p_fully_loaded = 1;
		if ( usedptr != memptr )					/* If not equal then something left in buffer	*/
		{
			return(reporterr(WARN_LASTLINE,0));
		}
	}
	return( 0 );
}

static char *memchrs(char *p1, int l1, char *p2, int l2)			/* Find substring p2 in p1.			*/
{
	char	*ptr;

	while( l1 >= l2 )
	{
		ptr = memchr(p1,*p2,l1-l2+1);					/* Look for first char of p2 in p1.		*/
		if (!ptr)
		{
			return(0);						/* Didn't find it.				*/
		}
		if ( 0 == memcmp(ptr,p2,l2) )
		{
			return( ptr );						/* Found it.					*/
		}
		l1 -= ptr - p1 + 1;						/* Point after current position.		*/
		p1 = ptr + 1;
	}
	return(0);								/* Ran out of string, didn't find it.		*/
}

static int sizerec(char *ptr, char *endptr)					/* Find the size of the record.			*/
{
	int 	size;
	char	*p;

	size = 0;

	if ( g_filetype == 'F' )
	{
		size = g_recsize;
	}
	else if ( g_filetype == 'N' )
	{
		p = memchr(ptr,'\n',endptr-ptr);
		if ( p )
		{
			size = p - ptr + 1;
		}
	}
	else if ( g_filetype == 'V' )
	{
		p = memchrs(ptr,endptr-ptr,g_recend,g_sizerecend);
		if ( p )
		{
			size = p - ptr + g_sizerecend;
		}
	}

	return( size );

}
									/* check the length of the variable record		*/
static int checklen( struct s_recindex *index1, struct s_recindex *index2, int offset, int length, int *rc)
{
	int 	end, len1, len2;

	len1 = length;
	len2 = length;
	end = offset+length;

	if ( end > index1->size )
	{
		len1 = index1->size - offset;
	}
	if ( end > index2->size )
	{
		len2 = index2->size - offset;
	}

	if ( len1 != length || len2 != length )
	{
		if ( len1 != length && len2 != length )
		{
			*rc = 0;
		}
		else if ( len1 < len2 )
		{
			*rc = -1;
		}
		else
		{
			*rc = 1;
		}
		return( 1 );
	}
	return( 0 );
}

/*
	Compare:	This routine does the comparison between 2 records for qsort().
			It returns an int that is greater-then, lesser-then or equal to zero depending if p1 is >, < or = p2.
			P1 and p2 are int ptrs from sortindex that point to recindex in memblock.
			It uses the sort keys that are in g_sortkeys.

*/

static int compare( struct s_recindex **p1, struct s_recindex **p2)
{
	static int bad;
	char	messstr[80];
	int	key;
	int	rc;

	if (g_init_cmp)
	{
		g_init_cmp = 0;
		bad = 0;
	}

	if (bad)
	{
		return(0);
	}

#ifdef TESTING2
printf("compare(%d,%d) ",(*p1-g_recindex), (*p2-g_recindex));
#endif

	for(key=0; key<g_numkeys; key++)
	{
		switch((g_sortkeys)[key].type)
		{
		case KT_ASCII:
			rc = cmp_ascii(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		case KT_BINARY:
			rc = cmp_binary(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length,BIN_NATURAL);
			break;
		case KT_BINARY_NORMAL:
			rc = cmp_binary(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length,BIN_NORMAL);
			break;
		case KT_BINARY_2N2:
			rc = cmp_binary(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length,BIN_2N2);
			break;
		case KT_FLOAT:
			rc = cmp_float(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		case KT_ZONED_LEAD:
			rc = cmp_zonelead(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		case KT_ZONED_TRAIL:
			rc = cmp_zonetrail(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		case KT_ZONED_RIGHT:
			rc = cmp_zoneright(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		case KT_ZONED_LEFT:
			rc = cmp_zoneleft(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		case KT_PACKED:
			rc = cmp_packed(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length,1);
			break;
		case KT_PACKED_UNSIGNED:
			rc = cmp_packed(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length,0);
			break;
		case KT_DECIMAL:
			rc = cmp_decimal(*p1,*p2,(g_sortkeys)[key].offset,(g_sortkeys)[key].length);
			break;
		default:
			sprintf(messstr,"[key = %d, type = %d]", key+1, (g_sortkeys)[key].type);
			g_cmp_rc = reporterr(ERR_BADTYPE,messstr);
			bad = 1;
			return(0);
			break;
		}

		if (rc != 0)							/* If rc not zero then return			*/
		{
			if ( (g_sortkeys)[key].direction == KD_DESCENDING )
			{
				rc = 0 - rc;
			}
			break;
		}
	}

	return( rc );
}

/*
	cmp_ascii:	Compare ASCII text using natural byte order.
*/
static int cmp_ascii(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	int rc;
	unsigned char	*p1, *p2;
	int	end, len1, len2;

	p1 = (unsigned char *)index1->rec;
	p2 = (unsigned char *)index2->rec;

	len1 = length;
	len2 = length;

	if ( g_filetype != 'F' )
	{
		end = offset+length;

		if ( end > index1->size )
		{
			len1 = index1->size - offset;
		}
		if ( end > index2->size )
		{
			len2 = index2->size - offset;
		}

		length = (len1 < len2) ? len1 : len2;
	}

	p1 += offset;
	p2 += offset;

	rc = 0;
	while(length-- > 0)
	{
		if ( !(*p1 == *p2) )
		{
			rc = (*p1 < *p2) ? -1 : 1;
			break;
		}
		p1++;
		p2++;
	}

	if ( len1 != len2 )
	{
		rc = (len1 < len2) ? -1 : 1;
	}
 
	return( rc );
}


/*
	cmp_binary:	Compare Binary data
*/
static int cmp_binary(struct s_recindex *index1, struct s_recindex *index2, int offset, int length, int bintype)
{
	static int first = 1;
	static int bad = 0;
	char	*p1, *p2;
	int 	rc;
	int2	short1, short2;
	int4	long1, long2;
	char	messstr[80];
	char	buff[80];

	if (first)
	{
		first = 0;
		if ( sizeof(int2) != 2 )
		{
			bad = 1;
			sprintf(messstr,"[sizeof(int2) = %d]",sizeof(int2));
			g_cmp_rc = reporterr(ERR_SIZEINT,messstr);
			
		}
		if ( sizeof(int4) != 4 )
		{
			bad = 1;
			sprintf(messstr,"[sizeof(int4) = %d]",sizeof(int4));
			g_cmp_rc = reporterr(ERR_SIZEINT,messstr);
		}
	}

	if (bad)
	{
		return(0);
	}

	p1 = index1->rec;
	p2 = index2->rec;

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	if ( length == sizeof(int2) )
	{
		memcpy(&short1,p1+offset,sizeof(int2));
		memcpy(&short2,p2+offset,sizeof(int2));

		if ( !ssbytenormal() && bintype != BIN_NATURAL )
		{
			l_reversebytes((char*)&short1,sizeof(int2));
			l_reversebytes((char*)&short2,sizeof(int2));
		}

		if (short1 != short2)
		{
			rc = (short1 < short2) ? -1 : 1;
		}
		else
		{
			rc = 0;
		}
	}	
	else if ( length == sizeof(int4) )
	{
		memcpy(&long1,p1+offset,sizeof(int4));
		memcpy(&long2,p2+offset,sizeof(int4));

		if ( !ssbytenormal() && bintype != BIN_NATURAL )
		{
			if ( bintype == BIN_NORMAL )
			{
				l_reversebytes((char*)&long1,sizeof(int4));
				l_reversebytes((char*)&long2,sizeof(int4));
			}
			if ( bintype == BIN_2N2 )
			{
				buff[0] = ((char *)&long1)[2];
				buff[1] = ((char *)&long1)[3];
				buff[2] = ((char *)&long1)[0];
				buff[3] = ((char *)&long1)[1];
				memcpy((char *)&long1,buff,sizeof(int4));

				buff[0] = ((char *)&long2)[2];
				buff[1] = ((char *)&long2)[3];
				buff[2] = ((char *)&long2)[0];
				buff[3] = ((char *)&long2)[1];
				memcpy((char *)&long2,buff,sizeof(int4));
			}
		}

		if (long1 != long2)
		{
			rc = (long1 < long2) ? -1 : 1;
		}
		else
		{
			rc = 0;
		}
	}	
	
	return( rc );
}


/*
	cmp_float:	Compare
*/
static int cmp_float(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	static int first = 1;
	static int bad = 0;
	char	*p1, *p2;
	int 	rc;
	FLOAT4	s1, s2;
	FLOAT8	l1, l2;
	char	messstr[80];

	if (first)
	{
		first = 0;
		if ( sizeof(FLOAT4) != 4 )
		{
			bad = 1;
			sprintf(messstr,"[sizeof(FLOAT4) = %d]",sizeof(FLOAT4));
			g_cmp_rc = reporterr(ERR_SIZEFLOAT,messstr);
			
		}
		if ( sizeof(FLOAT8) != 8 )
		{
			bad = 1;
			sprintf(messstr,"[sizeof(FLOAT8) = %d]",sizeof(FLOAT8));
			g_cmp_rc = reporterr(ERR_SIZEFLOAT,messstr);
		}
	}

	if (bad)
	{
		return(0);
	}

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	if ( length == sizeof(FLOAT4) )
	{
		memcpy(&s1,p1+offset,sizeof(FLOAT4));
		memcpy(&s2,p2+offset,sizeof(FLOAT4));

		if (s1 != s2)
		{
			rc = (s1 < s2) ? -1 : 1;
		}
		else
		{
			rc = 0;
		}
	}	
	else if ( length == sizeof(FLOAT8) )
	{
		memcpy(&l1,p1+offset,sizeof(FLOAT8));
		memcpy(&l2,p2+offset,sizeof(FLOAT8));

		if (l1 != l2)
		{
			rc = (l1 < l2) ? -1 : 1;
		}
		else
		{
			rc = 0;
		}
	}	
	
	return( rc );
}


/*
	cmp_zonelead:	Compare Zoned Leading Separate Sign.
			{+}99999
			{-}

*/
static int cmp_zonelead(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	char	*p1, *p2;
	int 	rc;
	int	pos1, pos2;

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	p1 += offset;
	p2 += offset;

	pos1 = ( *p1 == '-' ) ? 0 : 1;
	pos2 = ( *p2 == '-' ) ? 0 : 1;

	if ( pos1 != pos2 )
	{
		rc = (pos1) ? 1 : -1;
		return( rc );
	}

	rc=0;

	while(length-- > 0)
	{
		if ( *p1 != *p2 )
		{
			rc = (*p1 < *p2) ? -1 : 1;
			if (!pos1)
			{
				rc = 0 - rc;
			}
			break;
		}
		p1++;
		p2++;
	}

	return( rc );
}


/*
	cmp_zonetrail:	Compare Zoned Trailing Separate Sign.
			99999{+}
			     {-}

*/
static int cmp_zonetrail(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	char	*p1, *p2;
	int 	rc;
	int	pos1, pos2;

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	p1 += offset;	
	p2 += offset;

	pos1 = ( *(p1+length-1) == '-' ) ? 0 : 1;
	pos2 = ( *(p2+length-1) == '-' ) ? 0 : 1;

	if ( pos1 != pos2 )
	{
		rc = (pos1) ? 1 : -1;
		return( rc );
	}

	rc=0;

	while(length-- > 0)
	{
		if ( *p1 != *p2 )
		{
			rc = (*p1 < *p2) ? -1 : 1;
			if (!pos1)
			{
				rc = 0 - rc;
			}
			break;
		}
		p1++;
		p2++;
	}
	return( rc );
}


/*
	cmp_zoneright:	Compare Zoned Sign Right-overpunch.
			9999x    ( x = 0123456789 )
			         (     }JKLMNOPQR )

*/
static int cmp_zoneright(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	char	*p1, *p2;
	int 	rc;
	int	pos1, pos2;

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	p1 += offset + (length-1);						/* Point to sign digit				*/
	p2 += offset + (length-1);

	pos1 = ( *p1 >= '0' && *p1 <= '9' ) ? 1 : 0;
	pos2 = ( *p2 >= '0' && *p2 <= '9' ) ? 1 : 0;

	if ( pos1 != pos2 )
	{
		rc = (pos1) ? 1: -1;
		return( rc );
	}

	p1 -= length-1;								/* Point to first digit				*/
	p2 -= length-1;

	rc=0;

	while(length-- > 0)
	{
		if ( *p1 != *p2 )
		{
			if ( !pos1 && *p1 == '}' )
			{
				rc = 1;
			}
			else if ( !pos2 && *p2 == '}' )
			{
				rc = -1;
			}
			else
			{
				rc = (*p1 < *p2) ? -1 : 1;

				if (!pos1)
				{
					rc = 0 - rc;
				}
			}
			break;
		}
		p1++;
		p2++;
	}
	return( rc );
}


/*
	cmp_zoneleft:	Compare Zoned Sign Left-overpunch.
			x9999    ( x = 0123456789 )
			         (     }JKLMNOPQR )

*/
static int cmp_zoneleft(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	char	*p1, *p2;
	int 	rc;
	int	pos1, pos2;

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	p1 += offset;
	p2 += offset;

	pos1 = ( *p1 >= '0' && *p1 <= '9' ) ? 1 : 0;
	pos2 = ( *p2 >= '0' && *p2 <= '9' ) ? 1 : 0;

	if ( pos1 != pos2 )
	{
		rc = (pos1) ? 1: -1;
		return( rc );
	}

	rc=0;

	while(length-- > 0)
	{
		if ( *p1 != *p2 )
		{
			if ( !pos1 && *p1 == '}' )
			{
				rc = 1;
			}
			else if ( !pos2 && *p2 == '}' )
			{
				rc = -1;
			}
			else
			{
				rc = (*p1 < *p2) ? -1 : 1;

				if (!pos1)
				{
					rc = 0 - rc;
				}
			}

			break;
		}
		p1++;
		p2++;
	}
	return( rc );
}


/*
	cmp_packed:	Compare Packed data.
			99.99.9x   x = (F,C = '+', D = '-')


*/
static int cmp_packed(struct s_recindex *index1, struct s_recindex *index2, int offset, int length, int issigned)
{
	char	*p1, *p2;
	int 	rc;
	int	pos1, pos2;
	char	c1, c2;
	char	t1[MAXPACKEDLENGTH], t2[MAXPACKEDLENGTH];

	if ( g_filetype != 'F' )
	{
		if ( checklen(index1,index2,offset,length, &rc) )
		{
			return( rc );
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	memcpy(t1,p1+offset,length);
	memcpy(t2,p2+offset,length);

	p1 = t1;
	p2 = t2;

	if (issigned)
	{
		c1 = 0x0F & *(p1 + length - 1);						/* Mask off the sign into c		*/
		c2 = 0x0F & *(p2 + length - 1);

		pos1 = ( c1 == 0x0D ) ? 0 : 1;
		pos2 = ( c2 == 0x0D ) ? 0 : 1;

		if ( pos1 != pos2 )
		{
			rc = (pos1) ? 1: -1;
			return( rc );
		}

		*(p1 + length - 1) &= 0xF0;						/* Zero the sign nibble.		*/
		*(p2 + length - 1) &= 0xF0;						
	}
	else
	{
		pos1 = 1;
		pos2 = 2;
	}

	rc=0;

	while(length-- > 0)
	{
		if ( *p1 != *p2 )
		{

			rc = ((unsigned)*p1 < (unsigned)*p2) ? -1 : 1;

			if (!pos1)
			{
				rc = 0 - rc;
			}

			break;
		}
		p1++;
		p2++;
	}
	return( rc );
}


/*
	cmp_decimal:	Compare Decimal data.
			Leading or trailing sign.
			Leading spaces, astrisk, dollar sign.
			Embedded comas.
			Embedded decimal point.
*/
static int cmp_decimal(struct s_recindex *index1, struct s_recindex *index2, int offset, int length)
{
	int 	rc;
	double	d1, d2;
	char	*p1, *p2;
	int	len1, len2;

	len1 = length;
	len2 = length;

	if ( g_filetype != 'F' )
	{
		int 	end;

		end = offset+length;

		if ( end > index1->size )
		{
			len1 = index1->size - offset;
		}
		if ( end > index2->size )
		{
			len2 = index2->size - offset;
		}
	}

	p1 = index1->rec;
	p2 = index2->rec;

	p1 += offset;
	p2 += offset;

	load_decimal(&d1,p1,len1);
	load_decimal(&d2,p2,len2);

	if ( d1 == d2 )
	{
		rc = 0;
	}
	else if ( d1 < d2 )
	{
		rc = -1;
	}
	else
	{
		rc = 1;
	}
	return( rc );
}

static void load_decimal(double *d, char *p, int len)
{
	double	digit,lfact,rfact;
	int	decimal, neg, lead;

	*d = 0;
	decimal = 0;
	neg = 0;
	lfact = 10;
	rfact = 1;
	lead = 1;

	while(len-- > 0)
	{
		if ( *p >= '0' && *p <= '9' )
		{
			lead = 0;
			digit = *p - '0';
			if (decimal)
			{
				rfact *= (double).1;
			}
			*d = (*d * lfact) + (digit * rfact);
		}
		else if ( *p == '.' )
		{
			if (decimal)
			{
				break;
			}
			decimal = 1;
			lfact = 1;
			lead = 0;
		}
		else if ( *p == '+' )
		{
			neg = 0;
			if (!lead)
			{
				break;
			}
			lead = 0;
		}
		else if ( *p == '-' )
		{
			neg = 1;
			if (!lead)
			{
				break;
			}
			lead = 0;
		}
		else if ( *p == ' ' || *p == '$' || *p == '*' )
		{
			if (!lead)
			{
				break;
			}
		}
		else if ( *p == ',' )
		{
			if (decimal)
			{
				break;
			}
		}
		else
		{
			if (!lead)
			{
				break;
			}
		}

		p++;

	}

	if (neg)
	{
		*d *= -1;
	}
}


/*
	writemem:	This routine writes the contents of memblock (sorted) out to a file.
*/

static int writemem(char *filename, int f_file)
{
	int	i,rc;
	char	messstr[80];

	curr_sndx = g_sortindex;						/* Point to beginning of the table.		*/
	for(i=0; i<g_numelements; i++)
	{
		rc = write(f_file,(*curr_sndx)->rec,(*curr_sndx)->size);
		if (rc == -1)
		{
			sprintf(messstr,"[file = %s, errno = %d]",filename,errno);
			return(reporterr(ERR_WRITE,messstr));
		}
		else if (rc != (*curr_sndx)->size)
		{
			sprintf(messstr,"[Wrote only %d of %d bytes. File = %s]",
					rc,(*curr_sndx)->size,filename);
			return(reporterr(ERR_WRITE,messstr));
		}
		curr_sndx++;
	}

	return( 0 );
}

/*
	mergemem:	Merge the sorted memblock with file f_in and write to file f_out.
*/

static int mergemem(char *inname, int f_in, char *outname, int f_out)
{
	int	i,rc;
	char	messstr[80];
	int	readcnt;							/* Number bytes read.				*/
	char	*tmprec;							/* Temporary record buff.			*/
	char	*writerec;							/* Buffer to write.				*/
	int	eof_in, eof_mem;						/* End of input flags.				*/
	int	file_used, mem_used;						/* Record used (written) flags.			*/
	int	file_used_cnt, mem_used_cnt;					/* Count of how many records from each		*/
	int	cmp;								/* Result of compare()				*/
	int	pos;								/* Position in sortindex.			*/
	int4	leftover;							/* Number bytes leftover in tmprec.		*/
	int	size;								/* Size of record in tmprec.			*/
	int	gotcnt;								/* Number bytes in tmprec.			*/
	int	writesize;							/* Number of bytes to write from writerec.	*/

	eof_in = 0;								/* Assume file f_in not EOF			*/
	eof_mem = 0;
	leftover = 0;
	size = 0;
	gotcnt = 0;
	file_used_cnt = 0;
	mem_used_cnt = 0;

	tmprec = (char *)malloc(g_maxrecsize*2);			/* Malloc a temp record				*/

	if (!tmprec)							/* Malloc failed.				*/
	{
		sprintf(messstr,"[tmprec: size = %d]",g_maxrecsize*2);
		return(reporterr(ERR_MALLOC,messstr));
	}

	pos = -1;							/* Prime pos for first time thru.		*/
	mem_used = 1;							/* Force load first mem record.			*/
	file_used = 1;							/* Force load first file record.		*/
	curr_sndx = g_sortindex;

	rc =0;

	for(;;)
	{
		if ( mem_used )						/* Setup next record from memblock		*/
		{
			pos += 1;					/* Point to next element.			*/

			if (pos >= g_numelements)
			{
				eof_mem = 1;
			}
			mem_used = 0;
		}

		if ( file_used )					/* Setup next record from f_in			*/
		{
			if (leftover)					/* If mem leftover in tmprec then shift it down	*/
			{
				char *ptr1, *ptr2;

				ptr1 = tmprec;
				ptr2 = &tmprec[size];
				for(i=size; i<gotcnt; i++) *ptr1++ = *ptr2++;

				size = sizerec(tmprec,tmprec+leftover); /* See if we have a full record already loaded.	*/
			}
			else
			{
				size = 0;
			}
			gotcnt = leftover;

			if ( !size )						/* If a full record not loaded then read.	*/
			{
				readcnt = read(f_in, &tmprec[leftover], g_maxrecsize);	/* Read in at least 1 record.		*/

				if (readcnt == -1)				/* Error on READ				*/
				{
					sprintf(messstr,"[file = %s, errno = %d]",inname,errno);
					rc = reporterr(ERR_READ,messstr);
					break;
				}

				if (readcnt == 0)				/* EOF on infile.				*/
				{
					eof_in = 1;
				}

				gotcnt = leftover+readcnt;
				size = sizerec(tmprec,tmprec+gotcnt);
				if ( !size && gotcnt ) 
				{
					sprintf(messstr,"Partial record found in merge file");
					rc = reporterr(ERR_TEXT,messstr);
					break;
				}
				leftover = gotcnt - size;
			}
			else
			{
				readcnt = 0;
			}

			file_used = 0;
		}

		if ( eof_in && eof_mem )				/* Used all of f_in and memblock, DONE		*/
		{
			rc = 0;
			break;
		}
		else if ( eof_in )					/* If EOF f_in then write from mem		*/
		{
			cmp = 1;
		}
		else if ( eof_mem )					/* If used all mem then write from f_in		*/
		{
			cmp = -1;
		}
		else							/* Compare the two records			*/
		{
			struct s_recindex t, *p;
			t.rec = tmprec;
			t.size = size;
			p = &t;
			cmp = compare( &p, curr_sndx );
		}

		if ( cmp == 0 )						/* If equal write from f_in			*/
		{
			writerec = tmprec;
			writesize = size;
			file_used = 1;
			file_used_cnt++;
		}
		else if ( cmp < 0 )					/* Write from f_in				*/
		{
			writerec = tmprec;
			writesize = size;
			file_used = 1;
			file_used_cnt++;
		}
		else							/* Write from mem				*/
		{
			writerec = (*curr_sndx)->rec;
			writesize = (*curr_sndx)->size;
			mem_used = 1;
			mem_used_cnt++;
			curr_sndx++;							/* Step to next element.		*/
		}

		rc = write(f_out,writerec,writesize);
		if (rc == -1)
		{
			sprintf(messstr,"[file = %s, errno = %d]",outname,errno);
			rc = reporterr(ERR_WRITE,messstr);
			break;
		}
		else if (rc != writesize)
		{
			sprintf(messstr,"[Wrote only %d of %d bytes. File = %s]",rc,writesize,outname);
			rc = reporterr(ERR_WRITE,messstr);
			break;
		}
	}

	if (tmprec)
	{
		free(tmprec);
	}

	return( rc );
}


/*
	reporterr:	This routine reports error messages.
			It prints the error message to file g_errfile if one is supplied.
			It moves the error message to g_errbuff if one is supplied.
			It then returns the errnum.
*/

static int reporterr(int errnum, char *messstr)
{
	char	*errptr;
	char	buff[MAX_ERRSIZE+MAX_ERRSIZE];

	if (g_errlevel == 1 && !(errnum > WARNING))
	{
		return( errnum );
	}

	if (!g_errfile && !g_errbuff)						/* If no error file then just return	*/
	{
		return( errnum );
	}

	if (!messstr)								/* If no message then set to NULL	*/
	{
		messstr = "";
	}

	switch(errnum)								/* Get the message for this errnum	*/
	{
	case WARN_LASTLINE:
		errptr = "Incomplete last line was discarded";
		break;
	case ERR_TEXT:
		errptr = "TEXT:";
		break;
	case ERR_NOINFILE:
 		errptr = "No INFILE";
		break;
	case ERR_NOOUTFILE:
 		errptr = "No OUTFILE";
		break;
	case ERR_BADRECSIZE:
 		errptr = "Invalid record size";
		break;
	case ERR_BADFILETYPE:
 		errptr = "Invalid record type";
		break;
	case ERR_BADNUMKEYS:
 		errptr = "Invalid number of sort keys";
		break;
	case ERR_NOSORTKEYS:
 		errptr = "No sort keys";
		break;
	case ERR_BADOFFSET:
		errptr = "Invalid sort key offset";
		break;
	case ERR_BADLENGTH:
		errptr = "Invalid sort key length";
		break;
	case ERR_BADDIRECTION:
		errptr = "Invalid sort key direction";
		break;
	case ERR_BADTYPE:
		errptr = "Invalid sort key type";
		break;
	case ERR_BINLEN:
		errptr = "Invalid sort key length (BINARY 2 or 4)";
		break;
	case ERR_FLOATLEN:
		errptr = "Invalid sort key length (FLOAT 4 or 8)";
		break;
	case ERR_OPENINPUT:
		errptr = "Open for input failed";
		break;
	case ERR_OPENOUTPUT:
		errptr = "Open for output failed";
		break;
	case ERR_MALLOC:
		errptr = "Unable to malloc enough memory";
		break;
 	case ERR_READ:
		errptr = "Read failed";
		break;
	case ERR_WRITE:
		errptr = "Write failed.";
		break;
	case ERR_SIZEINT:
		errptr = "Invalid size of integer";
		break;
	case ERR_SIZEFLOAT:
		errptr = "Invalid size of float";
		break;
	case ERR_NORECEND:
		errptr = "No recend supplied.";
		break;
	case ERR_BADSIZEEND:
		errptr = "Invalid size of recend";
		break;
	case ERR_NORECORDS:
		errptr = "Infile contains no records";
		break;
	default:
 		errptr = "UNKNOWN ERROR";
		break;
	}

	if ( errnum > WARNING )
	{
		sprintf(buff,"Error: (%d) %s %s",errnum,errptr,messstr);
	}
	else
	{
		sprintf(buff,"Warning: (%d) %s %s",errnum,errptr,messstr);
	}

	if (strlen(buff) > MAX_ERRSIZE-1)
	{
		buff[MAX_ERRSIZE-1] = '\0';
	}

	if (g_errfile)
	{
		fprintf(g_errfile,"%s %s\n",DEF_SORTNAME,buff);
	}

	if (g_errbuff)
	{
		strcpy(g_errbuff,buff);
	}
	
	return(errnum);								/* Return the errnum.				*/
}




static void l_reversebytes(char *ptr, int len)						/* Reverse the bytes.			*/
{
	char	temp[80];

	memcpy(temp,ptr,len);

	while(len>0)
	{
		*ptr++ = temp[--len];
	}			
}

static int ssbytenormal(void)
{
	static int first = 1;
	static int normal;
	int4	l;
	char	*p;
	char	messstr[80];

	if (first)
	{
		first = 0;
		l = (int4) 0x01020304;
		p = (char *)&l;
		if      ( *p == (char) 0x01 )
		{
			normal = 1;
		}
		else if ( *p == (char) 0x04 )
		{
			normal = 0;
		}
		else
		{
			sprintf( messstr,"UNKNOWN BYTE-ORDER <%8.8x> = <%2.2x> <%2.2x> <%2.2x> <%2.2x>",
				l, p[0], p[1], p[2], p[3] );
			reporterr(ERR_TEXT,messstr);
		}
	}

	return( normal );
}

/*
**	History:
**	$Log: sortseqf.c,v $
**	Revision 1.16  1996-10-10 12:26:06-04  gsl
**	Free ptr after call to tempnam()
**
**	Revision 1.15  1996-10-08 17:25:53-07  gsl
**	replaced getenv() with wtmpdir() call
**
**	Revision 1.14  1996-09-26 10:14:21-07  gsl
**	Change MAX_NUMKEYS to SSF_MAX_NUMKEYS and move to header
**
**	Revision 1.13  1996-09-10 08:47:05-07  gsl
**	ensure system includes are before wisp include
**
**	Revision 1.12  1996-07-10 16:49:05-07  gsl
**	Fix prototype for arg sortkey, fix the level of indirection for the arg.
**
**	Revision 1.11  1996-07-09 16:54:53-07  gsl
**	Fix prototypes for NT
**	Rework temp filename generation.
**
**	Revision 1.10  1996-01-02 07:35:36-08  gsl
**	Change DEBUG --> TESTING defines
**
**
**
*/

