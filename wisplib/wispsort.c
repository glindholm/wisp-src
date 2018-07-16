/*
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
*/


/*
**	File:		wispsort.c
**
**	Purpose:	To hold the WANG/WISP interface to sortseqf()
**
**	Routines:	WISPSORT()		The SORTCALL replacement
**			WL_wangsort()		The underlining wang style sort interface (with stable sort option)
**			addseqnum()		Add a sequence number to each record in a file
**			delseqnum()		Remove the added sequence number
**
**
*/

/*
	WISPSORT:	Unix replacement for the VS SUB "SORTCALL".

			This routine is very similar to SORTCALL except you must supply a couple of extra parameters.
			WISPSORT uses an the routine sortseqf() to perform the actual sort of a sequential file. If
			you want to sort an Indexed file then it will first "unload" it to a sequential file.
			For ACUCOBOL supplied utility "vutil" is used to unload indexed files so it must be 
			available on the $PATH.
			For Micro Focus FH ISAM files the utility "fhconvert" is used to unload the files.
			

		PARAMETERS:	There are 5 parameters, all are manditory.

		sortparms	Alpha(116) same as SORTCALL.
					1-8	Input file	Alpha(8)
					9-16	Input library	Alpha(8)
					17-22	Input volume	Alpha(6)
					23-30	Output file	Alpha(8)
					31-38	Output library	Alpha(8)
					39-44	Output volume	Alpha(6)

					Sort specifications, repeat 8 times:
					45-48	Field position  Alpha(4) PIC Z(4)
					49-51	Field length	Alpha(3) PIC Z(3)
					52	Field datatype	Alpha(1)
							'B' Binary data  (Assumes byte normal order)
							'C' Character data
							'D' External decimal data
							'F' Floating point data
							'L' Zoned decimal data, sign leading separate
							'P' Packed decimal data, sign trailing included
							'Z' Zoned decimal data, sign trailing overpunched
						(The following are extension to SORTCALL.)
							'N' Binary data in machine order
							'2' 2+2 Binary data in machine order
							'T' Zoned decimal data, sign trailing separate
							'O' Zoned decimal data, sign leading overpunched
							'U' Packed decimal data, unsigned
							'Y' YY PIC 99 two digit year
					53	Sort order	Alpha(1)
							'A' Ascending
							'D' Descending

		filetype	Alpha(1) the file type.
					'A'	Acucobol indexed file.
					'C'	CISAM indexed file.
					'I'	Indexed (Vision, CISAM, or FH-ISAM)
					'F'	Fixed length file.       (Binary Sequential)
					'N'	Newline terminated file. (Line Sequential)

		recsize		INT(4) the record size for fixed length 'F' files.
					
		sortcode	INT(4) the code returned from sortseqf()

					WARN_LASTLINE		1	Last line of file was incomplete and truncated.
					ERR_TEXT		20	Misc. serious error.
					ERR_NOINFILE		21	No infile specified.
					ERR_NOOUTFILE		22	No outfile specified.
					ERR_BADRECSIZE		23	Invalid record size.
					ERR_BADFILETYPE		24	Invalid filetype.
					ERR_BADNUMKEYS		25	Invalid number of sort keys.
					ERR_NOSORTKEYS		26	No sort keys.
					ERR_BADOFFSET		27	Invalid sort key offset.
					ERR_BADLENGTH		28	Invalid sort key length.
					ERR_BADDIRECTION	29	Invalid sort key direction.
					ERR_BADTYPE		30	Invalid sort key data type.
					ERR_BINLEN		31	Invalid length for a binary.
					ERR_FLOATLEN		32	Invalid length for a float.
					ERR_OPENINPUT		33	Error opening input file.
					ERR_OPENOUTPUT		34	Error opening output file.
					ERR_MALLOC		35	Malloc fail to allocate needed space.
					ERR_READ		36	Error on read.
					ERR_WRITE		37	Error on write.
					ERR_SIZEINT		38	Invalid int size for this machine.
					ERR_SIZEFLOAT		39	Invalid float size for this machine.
					ERR_NORECEND		40	No recend specified.
					ERR_BADSIZEEND		41	Invalid size for recend.
					ERR_NORECORDS		42	No records found in infile.


		returncode	INT(4) the return code.
					0	Success
					4	Input file was empty
					8	Insufficient buffer space (or other internal error)
					12	Record size greater then 9999 bytes  (Was 2024 on the Wang)
					16	Invalid sort key
					20	Program check (see sortcode for reason).
				(The following are extensions to SORTCALL)
					40	Invalid filetype
					41	No input file or access denied
					42	Not an ACUCOBOL/Vision file.
					43	Unable to get recordsize from Vision file
					44	Unable to unload Vision file
					45	Not a CISAM file
					46	Unable to get recordsize from CISAM/FH-ISAM file
					47	Unable to unload CISAM file
					48	Unable to unload FH ISAM file (check for fhconvert)
*/

#if defined(AIX) || defined(HPUX) || defined(SOLARIS) || defined(LINUX)
#define _LARGEFILE64_SOURCE
#define USE_FILE64
#endif

#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef unix
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#include <process.h>
#include "wispnt.h"
#endif

#include "idsistd.h"
#include "wcommon.h"
#include "sortseqf.h"
#include "wfname.h"
#include "osddefs.h"
#include "wisplib.h"
#include "wmalloc.h"
#include "vwang.h"
#include "wispcfg.h"
#include "wfcisam.h"
#include "wfvision.h"
#include "assert.h"
#include "fcopy.h"

#include "werrlog.h"

void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode);
void WL_wangsort(char *sortparms, char *filetype, int4 *recsize, int dupinorder, int4 *sortcode, int4 *returncode);

static int addseqnum(char *infile, char *outfile, int recsize);
static int delseqnum(char *infile, char *outfile, int recsize);



void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode)
{
	WL_wangsort(sortparms,filetype,recsize,0,sortcode,returncode);
}

void WL_wangsort(char *sortparms, char *filetype, int4 *recsize, int dupinorder, int4 *sortcode, int4 *returncode)
{
	int	l_sortcode, l_returncode, l_recsize;
	char	l_filetype;
	struct s_sortkeys sortkeys[SSF_MAX_NUMKEYS]; 
	char	infile[80], outfile[80], unloadfile[80], inseqfile[80], outseqfile[80];
	int	numkeys;
	char	*ptr, *inptr, *outptr;
	int4	mode;
	char	errbuff[256];
	char	messstr[80];
	int	rc;

	l_sortcode = 0;
	l_returncode = 0;

	unloadfile[0] 	= '\0';							/* The file used to unload indexed infile	*/
	inseqfile[0]	= '\0';							/* The infile with seq nums			*/
	outseqfile[0]	= '\0';							/* The outfile with seq nums			*/

	l_recsize = WL_get_swap(recsize);

	wtrace("WANGSORT", "ENTRY", "Infile=[%22.22s] Outfile=[%22.22s] Recsize=%d Filetype=%c", 
	       &sortparms[0], &sortparms[22], l_recsize, *filetype);

	/* 
	**	Validate the filetype.
	**
	**	NOTE:	If filetype is C,A, or I it will be treated only as generic "indexed" and the file
	**		will be read to determine what type of file it really is.
	*/

	l_filetype = *filetype;

	switch(*filetype)
	{
	case 'A':
	case 'a':
		l_filetype = 'A';
		break;
	case 'C':
	case 'c':
		l_filetype = 'C';
		break;
	case 'F':
	case 'f':
		if ( l_recsize < 1 || l_recsize > 9999 ) /* Wang allows up to 2024 only */
		{
			sprintf(messstr,"Invalid record size [recsize = %d]",l_recsize);
			werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
			l_returncode = 12;
			goto return_label;
		}
		l_filetype = 'F';
		break;
	case 'I':
	case 'i':
		l_filetype = 'I';
		break;
	case 'N':
	case 'n':
		l_filetype = 'N';
		break;
	default:
		sprintf(messstr,"Invalid file type [filetype = \'%c\']",*filetype);
		werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
		l_returncode = 40;
		goto return_label;
	}

	/*
	**	Extract the filenames.
	*/

	mode = 0;
	ptr = WL_wfname(&mode, &sortparms[16], &sortparms[8], &sortparms[0], infile);
	*ptr = '\0';

	mode = IS_OUTPUT;
	ptr = WL_wfname_backfill(&mode, &sortparms[38], &sortparms[30], &sortparms[22], outfile);
	*ptr = '\0';

	makepath(outfile);							/* this is generate the intermediate dirs	*/

	if ( l_filetype == 'F' || l_filetype == 'N' )				/* Don't check ISAM here as may have .dat	*/
	{
		if (!fexists(infile))
		{
			l_returncode = 41;
			goto return_label;
		}
	}
	else
	{
		char	ixtype;
		
		/*
		**	For all Indexed files test against the actual file for the type.
		*/

		if (0 == WL_visioninfo(infile,"IX",&ixtype) && 'V' == ixtype)
		{
			/*
			 *	V = Vision
			 */
			l_filetype = 'A';
		}
		else if (0 == WL_cisaminfo(infile,"IX",&ixtype) && ('C'==ixtype || 'M'==ixtype))
		{
			/*
			 *	C = C-ISAM
			 *	M = Micro Focus FHISAM
			 */
			l_filetype = ixtype;
		}
		else
		{
			switch(l_filetype)
			{
			case 'A':	l_returncode = 42;	break;
			case 'C':	l_returncode = 45;	break;
			case 'I':	l_returncode = 41;	break;
			default:	l_returncode = 41;	break;
			}
			goto return_label;
		}
	} /* indexed file */

	/*
	**	NOTE:	At this point the filetype has been validated as one of the following.
	**			F	fixed
	**			N	Newline
	**			A	Acucobol
	**			C	Cisam
	**			M	Micro Focus FHISAM
	*/

	wtrace("WANGSORT","FILETYPE", "Filetype=%c", l_filetype);

	/*
	**	Unload ACUCOBOL indexed file
	*/

	if ( l_filetype == 'A' )
	{

		rc = WL_visioninfo(infile,"RS",&l_recsize);
		if ( rc )
		{
			l_returncode = 43;
			goto return_label;
		}

		mode = IS_OUTPUT;
		ptr = WL_wfname(&mode, "      ", "        ", "##SORT  ", unloadfile);
		*ptr = '\0';

		rc = WL_unloadvision(infile,unloadfile);
		if ( rc )
		{
			l_returncode = 44;
			goto return_label;
		}

		l_filetype = 'F';
	}


	/*
	**	Unload CISAM or FH-ISAM indexed file
	*/

	if ( l_filetype == 'C' || l_filetype == 'M' )
	{
		rc = WL_cisaminfo(infile,"RS",&l_recsize);
		if ( rc )
		{
			l_returncode = 46;
			goto return_label;
		}

		mode = IS_OUTPUT;
		ptr = WL_wfname(&mode, "      ", "        ", "##SORT  ", unloadfile);
		*ptr = '\0';

		if (l_filetype == 'C')
		{
			rc = WL_unloadcisam(infile,unloadfile,l_recsize);
			if ( rc )
			{
				l_returncode = 47;
				goto return_label;
			}
		}
		else
		{
			rc = WL_unloadfhisam(infile,unloadfile,l_recsize);
			if ( rc )
			{
				l_returncode = 48;
				goto return_label;
			}
		}

		l_filetype = 'F';
	}

	/*
	**	Load the sortkeys
	*/

	numkeys = 0;
	ptr = &sortparms[44];
	while( numkeys < 8 )
	{
		int	p,l,i;
		char	*keyptr;
		
		keyptr = ptr;

		p=0;
		l=0;
		
		for( i=0; i < 4; i++, ptr++)
		{
			if (*ptr != ' ')
			{
				p = (p * 10) + (*ptr - '0');
			}
		}	

		if ( p < 1 )
		{
			break;
		}

		wtrace("WANGSORT","KEYS","POST%d=%4.4s LENGTH%d=%3.3s TYPE%d=%c ORDER%d=%c",
		       numkeys+1, keyptr, 
		       numkeys+1, keyptr+4, 
		       numkeys+1, keyptr[7], 
		       numkeys+1, keyptr[8]);
		
		for( i=0; i < 3; i++, ptr++)
		{
			if (*ptr != ' ')
			{
				l = (l * 10) + (*ptr - '0');
			}
		}

		sortkeys[numkeys].offset = p-1;
		sortkeys[numkeys].length = l;

		switch(*ptr)
		{
		case 'B': 						/* Binary data  (Assumes byte normal order) */
		case 'b':
			sortkeys[numkeys].type = KT_BINARY_NORMAL;
			break;
		case 'C':						/* Character data */
		case 'c':
			sortkeys[numkeys].type = KT_ASCII;
			break;
		case 'D':						/* External decimal data */
		case 'd':
			sortkeys[numkeys].type = KT_DECIMAL;
			break;
		case 'F':						/* Floating point data */
		case 'f':
			sortkeys[numkeys].type = KT_FLOAT;
			break;
		case 'L':						/* Zoned decimal data, sign leading separate */
		case 'l':
			sortkeys[numkeys].type = KT_ZONED_LEAD;
			break;
		case 'P':						/* Packed decimal data, sign trailing included */
		case 'p':
			sortkeys[numkeys].type = KT_PACKED;
			break;
		case 'Z':						/* Zoned decimal data, sign trailing overpunched */
		case 'z':
			sortkeys[numkeys].type = KT_ZONED_RIGHT;
			break;
		case 'N':						/* Binary data in machine order */
		case 'n':
			sortkeys[numkeys].type = KT_BINARY;
			break;
		case '2':						/* 2+2 Binary data in machine order */
			sortkeys[numkeys].type = KT_BINARY_2N2;
			break;
		case 'T':						/* Zoned decimal data, sign trailing separate */
		case 't':
			sortkeys[numkeys].type = KT_ZONED_TRAIL;
			break;
		case 'O':						/* Zoned decimal data, sign leading overpunched */
		case 'o':
			sortkeys[numkeys].type = KT_ZONED_LEFT;
			break;
		case 'U':						/* Packed decimal data, unsigned */
		case 'u':
			sortkeys[numkeys].type = KT_PACKED_UNSIGNED;
			break;
		case 'Y':						/* YY PIC 99 - based on YYPIVOTYEAR */
		case 'y':
			sortkeys[numkeys].type = KT_YY_PIVOTYEAR;
			break;
		default:											      
			sprintf(messstr,"Invalid data type [%c]",*ptr);
			werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
			l_returncode = 16;
			goto return_label;
			break;
		}

		ptr++;

		switch(*ptr)
		{
		case 'A':
		case 'a':
			sortkeys[numkeys].direction = KD_ASCENDING;
			break;
		case 'D':
		case 'd':
			sortkeys[numkeys].direction = KD_DESCENDING;
			break;
		default:
			sprintf(messstr,"Invalid sort order [%c]",*ptr);
			werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
			l_returncode = 16;
			goto return_label;
			break;
		}

		numkeys += 1;	

		ptr++;
	}

	/*
	**	Point to the input and output file.
	*/

	if ( unloadfile[0] )
	{
		inptr = unloadfile;
	}
	else
	{
		inptr = infile;
	}

	outptr = outfile;


	/*
	**	Add sequence numbers to handle Duplicate in order (stable)
	*/

	if (dupinorder)
	{
	 	if ('F' != l_filetype)							/* Only valid for Fixed len records	*/
		{
			sprintf(messstr,"Stable sort not supported for filetype=%c",l_filetype);
			werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
			l_returncode = 40;
			goto return_label;
		}

		mode = IS_OUTPUT;
		ptr = WL_wfname(&mode, "      ", "        ", "##SORT  ", inseqfile);	/* Temp file for adding sequence nums	*/
		*ptr = '\0';

		if ((l_returncode = addseqnum(inptr,inseqfile,l_recsize)))		/* Copy to inseqfile adding seq num	*/
		{
			inseqfile[0] = '\0';
			goto return_label;
		}

		sortkeys[numkeys].offset = l_recsize;					/* Add seq num as a sort key		*/
		sortkeys[numkeys].length = sizeof(int4);
		sortkeys[numkeys].type = KT_BINARY;
		sortkeys[numkeys].direction = KD_ASCENDING;
		numkeys += 1;
		
		l_recsize += sizeof(int4);						/* adjust recsize to include seq num	*/

		if (unloadfile[0])							/* If temp file already active then	*/
		{
			wisp_unlink(unloadfile);						/* delete currect temp file		*/
			unloadfile[0] = '\0';						/* Mark as not used			*/
		}

		inptr = inseqfile;							/* Point inptr to inseqfile		*/

		mode = IS_OUTPUT;
		ptr = WL_wfname(&mode, "      ", "        ", "##SORT  ", outseqfile);	/* Temp file output (with seq nums)	*/
		*ptr = '\0';

		outptr = outseqfile;							/* Point outptr to outseqfile		*/
	}

	/*
	**	Call sortseqf() to do the sort.
	*/

	if (0 == numkeys)							/* Special case: No keys then copy file		*/
	{
		if (wisp_fcopy(inptr,outptr))
		{
			sprintf(errbuff,"Copy failed infile=%s outfile=%s [errno=%d]",
				inptr, outptr, errno);
			werrlog(WERRCODE(80502),errbuff,0,0,0,0,0,0,0);

			l_sortcode = ERR_OPENOUTPUT;				/* this is the most likely reason for cp fail	*/
			outptr[0] = '\0';
		}
		else	l_sortcode = 0;
	}
	else
	{
		int	memsizek = wispsortmemk();
		char	trace_keys[500];
		int	i;

		strcpy(trace_keys,"(Offset,Length,Direction,Type)");
		for(i=0; i<numkeys; i++)
		{
			char x[40];
			sprintf(x,"(%d,%d,%d,%d)", 
				sortkeys[i].offset,
				sortkeys[i].length,
				sortkeys[i].direction,
				sortkeys[i].type);
			strcat(trace_keys,x);
		}


		wtrace("WANGSORT","sortseqf","In=[%s] Out=[%s] MemSizeK=[%d] FileType=[%c] RecSize=[%d] NumKeys=[%d] SortKeys=[%s]", 
			inptr, outptr, memsizek, l_filetype, l_recsize, numkeys, trace_keys);

		l_sortcode = WL_sortseqf( inptr, outptr, NULL, memsizek, l_filetype, l_recsize, NULL, 
				      numkeys, sortkeys, 0, NULL, errbuff);

		if (l_sortcode != 0)
		{
			wtrace("WANGSORT","sortseqf","Sortcode=[%ld] Message=[%s]", (long)l_sortcode, errbuff);
		}
	}

	switch(l_sortcode)
	{
	case 0:
		l_returncode = 0;
		break;

	case WARN_LASTLINE:
		l_returncode = 0;
		break;

	case ERR_TEXT:		
	case ERR_NOINFILE:		
	case ERR_NOOUTFILE:	
	case ERR_OPENINPUT:		
	case ERR_OPENOUTPUT:
	case ERR_BADSIZEEND:
	case ERR_NORECEND:
		l_returncode = 20;
		break;
	
	case ERR_BADRECSIZE:
	case ERR_BADFILETYPE:
	case ERR_BADNUMKEYS:
	case ERR_NOSORTKEYS:
	case ERR_BADOFFSET:
	case ERR_BADLENGTH:
	case ERR_BADDIRECTION:
	case ERR_BADTYPE:
	case ERR_BINLEN:
	case ERR_FLOATLEN:
		l_returncode = 16;
		break;

	case ERR_MALLOC:		
		l_returncode = 8;
		break;

	case ERR_READ:		
	case ERR_WRITE:
		l_returncode = 20;
		break;

	case ERR_SIZEINT:		
	case ERR_SIZEFLOAT:
		werrlog(WERRCODE(80502),errbuff,0,0,0,0,0,0,0);
		l_returncode = 20;
		break;

	case ERR_NORECORDS:		
		l_returncode = 4;
		break;

	default:
		l_returncode = 20;
		break;
	}

	/*
	**	remove the sequence number and write the outfile
	*/
	if (dupinorder && l_returncode == 0)
	{
		wisp_unlink(inseqfile);							/* Delete inseqfile first (make space)	*/
		inseqfile[0] = '\0';

		l_recsize -= sizeof(int4);
		if ((l_returncode = delseqnum(outseqfile,outfile,l_recsize)))		/* Copy to outfile deleting seq num	*/
		{
			goto return_label;
		}

		wisp_unlink(outseqfile);							/* Delete outseqfile			*/
		outseqfile[0] = '\0';
	}

	/*
	**	Cleanup
	*/
return_label:
	if (unloadfile[0])
	{
		wisp_unlink(unloadfile);
	}
	if (dupinorder)
	{
		if (inseqfile[0]) wisp_unlink(inseqfile);
		if (outseqfile[0]) wisp_unlink(outseqfile);
	}

	wtrace("WANGSORT","RETURN","Sortcode=%ld Returncode=%ld", (long)l_sortcode, (long)l_returncode);
	
	WL_put_swap(sortcode, l_sortcode);
	WL_put_swap(returncode, l_returncode);
}

#ifdef USE_FILE64
#define FOPEN64 fopen64
#else
#define FOPEN64 fopen
#endif

/*
	addseqnum	Copy records from infile to outfile adding a sequence number to the end of each record.
			A non-zero return code indicates an error.
*/
static int addseqnum(char *infile, char *outfile, int recsize)
{
	FILE	*i_fp, *o_fp;
	char	*buff;
	int4	seq;
	char	messstr[80];

	if (!(i_fp = FOPEN64(infile,FOPEN_READ_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",infile,errno);
		werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
		return(41);
	}

	if (!(o_fp = FOPEN64(outfile,FOPEN_WRITE_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",outfile,errno);
		werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
		fclose(i_fp);
		return(41);
	}

	buff = (char *) wisp_malloc(recsize+4+2);

	seq = 0;
	while( fread(buff, recsize, 1, i_fp) )
	{
		seq++;
		memcpy(&buff[recsize],&seq,sizeof(int4));
		if (!fwrite(buff, recsize+sizeof(int4), 1, o_fp))
		{
			sprintf(messstr,"Write failed file=%s [errno=%d]",outfile,errno);
			werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
			fclose(i_fp);
			fclose(o_fp);
			free(buff);
			wisp_unlink(outfile);
			return(41);
		}
	}
	
	fclose(i_fp);
	fclose(o_fp);
	free(buff);

	return(0);
}

/*
	delseqnum	Copy records from infile to outfile deleting the sequence number from the end of each record.
			A non-zero return code indicates an error.
*/
static int delseqnum(char *infile, char *outfile, int recsize)
{
	FILE	*i_fp, *o_fp;
	char	*buff;
	char	messstr[80];

	if (!(i_fp = FOPEN64(infile,FOPEN_READ_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",infile,errno);
		werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
		return(41);
	}

	if (!(o_fp = FOPEN64(outfile,FOPEN_WRITE_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",outfile,errno);
		werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
		fclose(i_fp);
		return(41);
	}

	buff = (char *) wisp_malloc(recsize+4+2);

	while( fread(buff, recsize+sizeof(int4), 1, i_fp) )
	{
		if (!fwrite(buff, recsize, 1, o_fp))
		{
			sprintf(messstr,"Write failed file=%s [errno=%d]",outfile,errno);
			werrlog(WERRCODE(80502),messstr,0,0,0,0,0,0,0);
			fclose(i_fp);
			fclose(o_fp);
			free(buff);
			wisp_unlink(outfile);
			return(41);
		}
	}
	
	fclose(i_fp);
	fclose(o_fp);
	free(buff);

	return(0);
}

/*
**	History:
**	$Log: wispsort.c,v $
**	Revision 1.48  2003/02/04 17:22:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.47  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.46  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.45  2002/12/11 17:03:10  gsl
**	use wisp_unlink()
**	
**	Revision 1.44  2002/12/10 17:09:14  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.43  2002/10/07 21:07:59  gsl
**	Huge file support
**	
**	Revision 1.42  2002/10/07 20:54:47  gsl
**	Huge file support
**	
**	Revision 1.41  2002/08/19 21:03:36  gsl
**	tracing
**	
**	Revision 1.40  2002/07/12 19:10:21  gsl
**	Global unique WL_ changes
**	
**	Revision 1.39  2002/07/12 17:01:04  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.38  2002/07/09 04:13:52  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.37  2002/07/02 21:15:34  gsl
**	Rename wstrdup
**	
**	Revision 1.36  2002/06/28 04:02:58  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.35  2002/06/27 04:12:40  gsl
**	Clean up status/mode bits
**	
**	Revision 1.34  2001/11/08 18:09:33  gsl
**	remove unused var
**	
**	Revision 1.33  2001-10-30 15:22:14-05  gsl
**	versionvesrion() replaced by visioninfo()
**	unloadvision() moved to wfvision.c
**
**	Revision 1.32  2001-10-26 15:40:10-04  gsl
**	Move unloadcisam ans unloadfhisam to wfcisam.c
**
**	Revision 1.31  2001-10-22 17:05:27-04  gsl
**	Removed MSDOS.
**	Call WL_cisaminfo IX to determine if CISAM vs MF FHISAM file.
**
**	Revision 1.30  1998-09-09 15:49:53-04  gsl
**	Add trace of keys
**
**	Revision 1.29  1998-09-08 16:38:50-04  gsl
**	Add support for key type="Y" for a YY based on YYPIVOTYEAR
**
**	Revision 1.28  1998-05-14 16:55:07-04  gsl
**	Move isvision() to wfvision.c
**	Fix wtrace
**
**	Revision 1.27  1998-05-05 17:40:10-04  gsl
**	WIN32 change to new spawn flags
**
**	Revision 1.26  1998-04-17 14:53:14-04  gsl
**	Change the max recsize to 9999, it was 2024 on the Wang
**
**	Revision 1.25  1997-12-04 18:13:43-05  gsl
**	changed to wispnt.h
**
**	Revision 1.24  1997-07-29 15:37:55-04  gsl
**	Add support for ID3 and ID4 files for Micro Focus 3.2
**
**	Revision 1.23  1997-07-16 21:20:20-04  gsl
**	Add SPN_NO_INHERIT to the spawn flags for VUTIL on WIN32.
**	With COSTAR VUTIL was overwritting screen.
**
**	Revision 1.22  1997-05-09 15:12:47-04  gsl
**	Split unloadacu() out for WIN32 and use win32spawnlp() to run vutil
**
**	Revision 1.21  1997-03-12 13:27:18-05  gsl
**	changed to use WIN32 define
**
**	Revision 1.20  1996-12-11 19:57:00-05  gsl
**	Changed hardcoded "vutil" into acu_vutil_exe() calls as the name can
**	be "vutil", "vutil.exe", "vutil32.exe" or "vutilext.exe" depending on
**	the release and the platform
**
**	Revision 1.19  1996-10-08 17:30:19-07  gsl
**	replace getenv() with wispsortmemk()
**
**	Revision 1.18  1996-09-26 10:15:05-07  gsl
**	Changed the sortkeys[] array size from 8 to SSF_MAK_KEYS (16).
**	When STABLE sort was used an extra key was added so the max
**	number of keys was 8+1=9 which overflowed the array.
**
**	Revision 1.17  1996-09-10 08:51:20-07  gsl
**	ensure system includes are before wisp includes
**	,
**
**	Revision 1.16  1996-08-22 11:28:17-07  gsl
**	Fix unloadacu() to shutdown and restore vwang outside of the
**	munging with stdout
**
**	Revision 1.15  1996-08-21 16:36:06-07  gsl
**	Add missing include
**
**	Revision 1.14  1996-08-21 16:19:14-07  gsl
**	Fix redirecting of stdout for NT
**
**	Revision 1.13  1996-07-17 14:55:00-07  gsl
**	change to use wmalloc()
**
**	Revision 1.12  1996-07-10 17:18:40-07  gsl
**	Reuse MSDOS code for NT
**
**	Revision 1.11  1996-01-02 07:38:39-08  gsl
**	changed ifdef DEBUG --> TESTING
**
**
**			mm/dd/yy	Written by GSL
**			05/29/92	Added makepath(outfile) to generate intermmediate directories. GSL
**
*/
