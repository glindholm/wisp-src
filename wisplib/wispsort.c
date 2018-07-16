static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		wispsort.c
**
**	Purpose:	To hold the WANG/WISP interface to sortseqf()
**
**	Routines:	WISPSORT()		The SORTCALL replacement
**			wangsort()		The underlining wang style sort interface (with stable sort option)
**			unloadacu()		Unload a Acucobol VISION file to a flat file.
**			unloadcisam()		Unload a CISAM file to a flatfile
**			unloadfhisam()		Unload a Micro Focus FHISAM file to a flatfile
**			isvision()		Check if file is a VISION file
**			addseqnum()		Add a sequence number to each record in a file
**			delseqnum()		Remove the added sequence number
**			copyfile()		Copy a file
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
					12	Record size greater then 2024 bytes
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


#include <stdio.h>
#include <fcntl.h>

#if defined(MSDOS) || defined(unix) || defined(WIN32)
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#if defined(MSDOS) || defined(WIN32)
#include <io.h>
#include <process.h>
#include "wispnt.h"
#endif

#include "idsistd.h"
#include "wcommon.h"
#include "sortseqf.h"
#include "wfname.h"
#include "movebin.h"
#include "osddefs.h"
#include "wisplib.h"
#include "wmalloc.h"
#include "vwang.h"
#include "wispcfg.h"
#include "assert.h"

#include "werrlog.h"
#define		ROUTINE		80500


#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif


void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode);
void wangsort(char *sortparms, char *filetype, int4 *recsize, int dupinorder, int4 *sortcode, int4 *returncode);

static int unloadacu(char *inname, char *outname);
static int unloadcisam(char *inname, char *outname, int4 recsize);
static int unloadfhisam(char *inname, char *outname, int4 recsize);
static int isvision(char *filename);
static int addseqnum(char *infile, char *outfile, int recsize);
static int delseqnum(char *infile, char *outfile, int recsize);
static int copyfile(char *infile, char *outfile);



void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode)
{
	wangsort(sortparms,filetype,recsize,0,sortcode,returncode);
}

void wangsort(char *sortparms, char *filetype, int4 *recsize, int dupinorder, int4 *sortcode, int4 *returncode)
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
	char	buff[80];
	int	rc;

	l_sortcode = 0;
	l_returncode = 0;

	unloadfile[0] 	= '\0';							/* The file used to unload indexed infile	*/
	inseqfile[0]	= '\0';							/* The infile with seq nums			*/
	outseqfile[0]	= '\0';							/* The outfile with seq nums			*/

	GETBIN(&l_recsize,recsize,sizeof(int));
	wswap(&l_recsize);

	wtrace("WANGSORT", "ENTRY", "Infile=[%22s] Outfile=[%22s] Recsize=%d Filetype=%c", 
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
		if ( l_recsize < 1 || l_recsize > 2024 )
		{
			sprintf(messstr,"Invalid record size [recsize = %d]",l_recsize);
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
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
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		l_returncode = 40;
		goto return_label;
	}

	/*
	**	Extract the filenames.
	*/

	mode = 0;
	ptr = wfname(&mode, &sortparms[16], &sortparms[8], &sortparms[0], infile);
	*ptr = '\0';

	mode = IS_OUTPUT | IS_BACKFILL;
	ptr = wfname(&mode, &sortparms[38], &sortparms[30], &sortparms[22], outfile);
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
		char	v_filetype;

		v_filetype = '?';						/* Validated file type unknown			*/

		/*
		**	For all Indexed files test against the actual file for the type.
		*/

		if ( 0 == isvision(infile) )
		{
			v_filetype = 'A';
		}

		if (v_filetype == '?')
		{
			strcpy(buff,infile);
			strcat(buff,".idx");

			if (fexists(buff))					/* CISAM or FH-ISAM				*/
			{
				int	fh, i1;
				unsigned char	magicnum[2];

				fh = open( buff, O_RDONLY | O_BINARY );		/* Open the file				*/

				if ( fh == -1 )
				{
					l_returncode = 41;
					goto return_label;
				}

				i1 = read( fh, (char *)magicnum, 2 );		/* read the magic number			*/
				close(fh);

				if ( i1 == -1 )
				{
					l_returncode = 41;
					goto return_label;
				}

				if      ( i1 >= 2 && (magicnum[0] == 0xFE && magicnum[1] == 0x53) )
				{
					v_filetype = 'C';
				}
				else if ( i1 >= 2 && (magicnum[0] == 0x31 && magicnum[1] == 0xFE) )
				{
					v_filetype = '3'; /* This is the older MF-ISAM files */
				}
				else if ( i1 >= 2 && (magicnum[0] == 0x33 && magicnum[1] == 0xFE) )
				{
					v_filetype = '3'; /* This could actually be ID3 or ID4 */
				}
			} /* CISAM or FH-ISAM */
		} /* type unknown */

		if (v_filetype == '?')
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

		l_filetype = v_filetype;
	} /* indexed file */

	/*
	**	NOTE:	At this point the filetype has been validated as one of the following.
	**			F	fixed
	**			N	Newline
	**			A	Acucobol
	**			C	Cisam
	**			3	FH-Isam
	*/

	wtrace("WANGSORT","FILETYPE", "Filetype=%c", l_filetype);

	/*
	**	Unload ACUCOBOL indexed file
	*/

	if ( l_filetype == 'A' )
	{

		rc = acuvision(infile,"RS",&l_recsize);
		if ( rc )
		{
			l_returncode = 43;
			goto return_label;
		}

		mode = IS_OUTPUT;
		ptr = wfname(&mode, "      ", "        ", "##SORT  ", unloadfile);
		*ptr = '\0';

		rc = unloadacu(infile,unloadfile);
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

	if ( l_filetype == 'C' || l_filetype == '3' )
	{
		rc = cisaminfo(infile,"RS",&l_recsize);
		if ( rc )
		{
			l_returncode = 46;
			goto return_label;
		}

		mode = IS_OUTPUT;
		ptr = wfname(&mode, "      ", "        ", "##SORT  ", unloadfile);
		*ptr = '\0';

		if (l_filetype == 'C')
		{
			rc = unloadcisam(infile,unloadfile,l_recsize);
			if ( rc )
			{
				l_returncode = 47;
				goto return_label;
			}
		}
		else
		{
			rc = unloadfhisam(infile,unloadfile,l_recsize);
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
		default:											      
			sprintf(messstr,"Invalid data type [%c]",*ptr);
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
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
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
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
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
			l_returncode = 40;
			goto return_label;
		}

		mode = IS_OUTPUT;
		ptr = wfname(&mode, "      ", "        ", "##SORT  ", inseqfile);	/* Temp file for adding sequence nums	*/
		*ptr = '\0';

		if (l_returncode = addseqnum(inptr,inseqfile,l_recsize))		/* Copy to inseqfile adding seq num	*/
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
			unlink(unloadfile);						/* delete currect temp file		*/
			unloadfile[0] = '\0';						/* Mark as not used			*/
		}

		inptr = inseqfile;							/* Point inptr to inseqfile		*/

		mode = IS_OUTPUT;
		ptr = wfname(&mode, "      ", "        ", "##SORT  ", outseqfile);	/* Temp file output (with seq nums)	*/
		*ptr = '\0';

		outptr = outseqfile;							/* Point outptr to outseqfile		*/
	}

	/*
	**	Call sortseqf() to do the sort.
	*/

	if (0 == numkeys)							/* Special case: No keys then copy file		*/
	{
		if (copyfile(inptr,outptr))
		{
			l_sortcode = ERR_OPENOUTPUT;				/* this is the most likely reason for cp fail	*/
			outptr[0] = '\0';
		}
		else	l_sortcode = 0;
	}
	else
	{
		int	memsizek = wispsortmemk();
#ifdef TESTING
printf("wispsort:memsizek = %d\n",memsizek);
#endif
		l_sortcode = sortseqf( inptr, outptr, NULL, memsizek, l_filetype, l_recsize, NULL, 
				      numkeys, sortkeys, 0, NULL, errbuff);
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
		werrlog(ERRORCODE(2),errbuff,0,0,0,0,0,0,0);
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
		unlink(inseqfile);							/* Delete inseqfile first (make space)	*/
		inseqfile[0] = '\0';

		l_recsize -= sizeof(int4);
		if (l_returncode = delseqnum(outseqfile,outfile,l_recsize))		/* Copy to outfile deleting seq num	*/
		{
			goto return_label;
		}

		unlink(outseqfile);							/* Delete outseqfile			*/
		outseqfile[0] = '\0';
	}

	/*
	**	Cleanup
	*/
return_label:
	if (unloadfile[0])
	{
		unlink(unloadfile);
	}
	if (dupinorder)
	{
		if (inseqfile[0]) unlink(inseqfile);
		if (outseqfile[0]) unlink(outseqfile);
	}

	wtrace("WANGSORT","RETURN","Sortcode=%ld Returncode=%ld", (long)l_sortcode, (long)l_returncode);
	
	wswap(&l_sortcode);
	wswap(&l_returncode);
	PUTBIN(sortcode,&l_sortcode,sizeof(int));
	PUTBIN(returncode,&l_returncode,sizeof(int));
}

#ifdef unix
static int unloadacu(char *inname, char *outname)			/* Unload the ACUCOBOL file to a tempfile		*/
{
	char	command[256];
	int	rc;

	sprintf(command,"%s -unload %s %s >/dev/null 2>&1", acu_vutil_exe(), inname, outname );
	rc = wsystem(command);

	return( rc );
}
#endif /* unix */


#ifdef WIN32
#include "win32spn.h"

static int unloadacu(char *inname, char *outname)			/* Unload the ACUCOBOL file to a tempfile		*/
{
	char 	cmd[512];
	int 	rc;

	/*
	**	Spawn "vutil32.exe -unload <inname> <outname>"
	*/

	sprintf(cmd, "%s -unload %s %s", acu_vutil_exe(), inname, outname);
	ASSERT(strlen(cmd) < sizeof(cmd));
	
	rc = win32spawnlp(NULL, cmd, SPN_HIDE_CHILD|SPN_WAIT_FOR_CHILD|SPN_NO_INHERIT);

	return( rc );
}
#endif /* WIN32 */

#ifdef MSDOS
static int unloadacu(char *inname, char *outname)			/* Unload the ACUCOBOL file to a tempfile		*/
{
	char	buff[256];
	int	rc;
#define	STD_OUT	1
	int	old_std_out, new_std_out;

	/*
	**	To unload the acucobol file we spawn a VUTIL to do it.
	**	However VUTIL writes some informational messages to stdout that
	**	we don't want to see so we must redirect stdout to NUL.
	*/

	/*
	**	First shutdown vwang before munging with stdout
	*/
	vwang_shut();

	/*
	**	Re-direct stdout to the NUL device
	*/
	old_std_out = dup(STD_OUT);

	if ((new_std_out = open("NUL", O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, S_IWRITE)) == -1)
	{
		sprintf(buff,"Error on open() of new_std_out [errno=%d]",errno);
		werrlog(ERRORCODE(2),buff,0,0,0,0,0,0,0);
	}

	if (dup2(new_std_out, STD_OUT) == -1)
	{
		sprintf(buff,"Error on dup2() of new_std_out [errno=%d]",errno);
		werrlog(ERRORCODE(2),buff,0,0,0,0,0,0,0);
	}

	/*
	**	Spawn "vutil.exe -unload <inname> <outname>"
	*/

	rc = spawnlp(P_WAIT, acu_vutil_exe(), "vutil", "-unload", inname, outname, NULL);

	/*
	**	Restore stdout
	*/
	if ( -1 != old_std_out)
	{
		dup2(old_std_out,STD_OUT);
		close(old_std_out);
	}
	else
	{
		dup2(handle_stdout(), STD_OUT);
	}
	close(new_std_out);

	/*
	**	Restore vwang
	*/
	vwang_synch();

	/*
	**	Check the results of the spawn of vutil
	*/
	if (rc == -1)
	{
		if (errno == ENOENT)
		{
			sprintf(buff,"VUTIL not found (%s)",acu_vutil_exe());
			werrlog(ERRORCODE(2),buff,0,0,0,0,0,0,0);
		}
		else
		{
			sprintf(buff,"Error on spawn of %s [errno=%d]", acu_vutil_exe(), errno);
			werrlog(ERRORCODE(2),buff,0,0,0,0,0,0,0);
		}
	}

	return( rc );
}
#endif /* MSDOS */

static int unloadcisam(char *inname, char *outname, int4 recsize)
{
	FILE	*fpin, *fpout;
	unsigned char	*buff;
	int	rc;
	char	t_inname[132];

	strcpy(t_inname,inname);
	if (fexists(t_inname))
	{
		fpin = fopen(t_inname,FOPEN_READ_BINARY);
		if ( !fpin ) return(-1);
	}
	else
	{
		strcat(t_inname,".dat");
		if (fexists(t_inname))
		{
			fpin = fopen(t_inname,FOPEN_READ_BINARY);
			if ( !fpin ) return(-1);
		}
		else return(-1);	 
	}

	fpout = fopen(outname,FOPEN_WRITE_BINARY);
	if ( !fpout ) 
	{
		fclose(fpin);
		return(-1);
	}

	buff = (unsigned char *)wmalloc((size_t)recsize+2);
		
	for(;;)
	{
		rc = fread(buff,(size_t)recsize+1,1,fpin);
		if ( rc != 1 )
		{
			if (feof(fpin)) break;
			fclose(fpin);
			fclose(fpout);
			free(buff);
			return(-1);
		}

		if ( buff[recsize] == 0x0A )
		{
			rc = fwrite(buff, (size_t)recsize, 1, fpout);
			if ( rc != 1 )
			{
				fclose(fpin);
				fclose(fpout);
				free(buff);
				return(-1);
			}
		}
		else if ( buff[recsize] )
		{
			fclose(fpin);
			fclose(fpout);
			free(buff);
			return(-1);
		}
	}

	fclose(fpin);
	fclose(fpout);
	free(buff);
	return(0);
}

#if defined(MSDOS) || defined(WIN32)
static int unloadfhisam(char *inname, char *outname, int4 recsize)
{
	werrlog(102, "(unloadfhism) Not Implemented",0,0,0,0,0,0,0);
	return 1;
}
#endif /* MSDOS */

#ifdef unix
static int unloadfhisam(char *inname, char *outname, int4 recsize)
{
	char	parms[512];
	char	cmd[512];
	char	buff[80];
	int	fh, i1;
	char	*filetype = "I3";		/* Default to ID3 files */

	fh = open( inname, O_RDONLY | O_BINARY );
	if ( fh != -1 )
	{
		unsigned char	fileheader[50];		/* Only need byte 44 (offset 43) by get extra */

		memset(fileheader, '\0', sizeof(fileheader));
		read( fh, (char *)fileheader, sizeof(fileheader) );
		close(fh);
		if (0x04 == fileheader[43])  	/* Check byte that tells 3 or 4 */
		{
			filetype = "I4";	/* If 4 then it is ID4 */
		}
	}

	if (fexists(inname))
	{
		/*
		**	No .dat extension
		*/
		sprintf(parms,"IN %s\nIE\nIT %s\nON %s\nOE\nOT S0\nOF %d\n",inname, filetype, outname, recsize);
	}
	else
	{
		sprintf(parms,"IN %s\nIT %s\nON %s\nOT S0\nOF %d\n",inname, filetype, outname, recsize);
	}

	sprintf(cmd,"echo \"%s\" | fhconvert -e -c - >/dev/null 2>&1",parms);

	unlink(outname);		/* Delete old output file */

	wsystem(cmd);			/* Issue the fhconvert command */

	/* Delete temp .con files that fhconvert creates */
	strcpy(buff,inname);
	strcat(buff,".con");
	unlink(buff);

	strcpy(buff,outname);
	strcat(buff,".con");
	unlink(buff);

	if (fexists(outname))
	{
		return 0;
	}
	else
	{
		return 1;
	}
}
#endif /* unix */

static int isvision(char *filename)					/* Test if file is an ACUCOBOL Vision file.		*/
									/* 	ReturnCode:	 0	Vision file		*/
									/*			 1	Not Vision file		*/
									/*			-4	Can't open		*/
									/*			-8	Can't read		*/
{
	int	fh;
	char	buff[20];
	int4	lg;
	int	rc;

	fh = open(filename,O_RDONLY|O_BINARY,0);
	if ( fh == -1 )
	{
		rc = -4;
		return(rc);
	}
	rc = read(fh,buff,4);
	close(fh);
	if (rc != 4)
	{
		if ( rc == -1 )
		{
			rc = -8;
		}
		else
		{
			rc = 1;
		}
		return(rc);
	}

	memcpy(&lg,buff,sizeof(int4));

	if ( 0x10121416 == lg || 0x16141210 == lg )
	{
		return( 0 );
	}
	else
	{
		return( 1 );
	}
}

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

	if (!(i_fp = fopen(infile,FOPEN_READ_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",infile,errno);
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		return(41);
	}

	if (!(o_fp = fopen(outfile,FOPEN_WRITE_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",outfile,errno);
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		fclose(i_fp);
		return(41);
	}

	buff = (char *) wmalloc(recsize+4+2);

	seq = 0;
	while( fread(buff, recsize, 1, i_fp) )
	{
		seq++;
		memcpy(&buff[recsize],&seq,sizeof(int4));
		if (!fwrite(buff, recsize+sizeof(int4), 1, o_fp))
		{
			sprintf(messstr,"Write failed file=%s [errno=%d]",outfile,errno);
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
			fclose(i_fp);
			fclose(o_fp);
			free(buff);
			unlink(outfile);
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

	if (!(i_fp = fopen(infile,FOPEN_READ_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",infile,errno);
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		return(41);
	}

	if (!(o_fp = fopen(outfile,FOPEN_WRITE_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",outfile,errno);
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		fclose(i_fp);
		return(41);
	}

	buff = (char *) wmalloc(recsize+4+2);

	while( fread(buff, recsize+sizeof(int4), 1, i_fp) )
	{
		if (!fwrite(buff, recsize, 1, o_fp))
		{
			sprintf(messstr,"Write failed file=%s [errno=%d]",outfile,errno);
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
			fclose(i_fp);
			fclose(o_fp);
			free(buff);
			unlink(outfile);
			return(41);
		}
	}
	
	fclose(i_fp);
	fclose(o_fp);
	free(buff);

	return(0);
}

/*
	copyfile	Copy infile to outfile.
			A non-zero return code indicates an error.
*/
static int copyfile(char *infile, char *outfile)
{
	FILE	*i_fp, *o_fp;
	int	c;
	char	messstr[80];

	if (!(i_fp = fopen(infile,FOPEN_READ_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",infile,errno);
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		return(41);
	}

	if (!(o_fp = fopen(outfile,FOPEN_WRITE_BINARY)))
	{
		sprintf(messstr,"Open failed file=%s [errno=%d]",outfile,errno);
		werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
		fclose(i_fp);
		return(41);
	}

	while( EOF != (c=getc(i_fp)))
	{
		if (EOF == putc(c, o_fp))
		{
			sprintf(messstr,"Write failed file=%s [errno=%d]",outfile,errno);
			werrlog(ERRORCODE(2),messstr,0,0,0,0,0,0,0);
			fclose(i_fp);
			fclose(o_fp);
			unlink(outfile);
			return(41);
		}
	}
	
	fclose(i_fp);
	fclose(o_fp);

	return(0);
}

/*
**	History:
**	$Log: wispsort.c,v $
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
