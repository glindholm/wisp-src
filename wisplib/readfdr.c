static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* READFDR.C ... This routine simulates some of the Wang-VS READFDR subroutine operation.					*/

#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <stdarg.h>
#include "idsistd.h"
#include "wcommon.h"
#include "movebin.h"
#include "cobrun.h"
#include "werrlog.h"
#include "wisplib.h"
#include "wfname.h"
#include "idsisubs.h"
#include "filext.h"
#include "wfcisam.h"
#include "wfvision.h"
#include "wdefines.h"
#include "wperson.h"


#define		ROUTINE		51000

static int bFromReadfdr4 = 0;
static char *pReadfdr;

static void READFDRX(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, va_list the_args);
static int4 fileinfo(const char* path, const char* code, void* field);
static int4 unstructured_fileinfo(const char* path, const char* code, void* raw_field);


/*
 *	READFDR4	This routine is the same as READFDR except CD, ED, and MD all
 *			return dates in YYYYMMDD format.
 */
void READFDR4(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...)
{
	va_list the_args;

	pReadfdr = "READFDR4";
	bFromReadfdr4 = 1;
	
	va_start(the_args, mode);
	READFDRX( cpFile, cpLib, cpVol, mode, the_args);
	va_end(the_args);
	
	bFromReadfdr4 = 0;
}

void READFDR(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, ...)
{
	va_list the_args;

	pReadfdr = "READFDR";
	bFromReadfdr4 = 0;
	
	va_start(the_args, mode);
	READFDRX( cpFile, cpLib, cpVol, mode, the_args);
	va_end(the_args);

	bFromReadfdr4 = 0;
}

static void READFDRX(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, va_list the_args)
{
	char	l_file[SIZEOF_FILE+1];
	char	l_lib[SIZEOF_LIB+1];
	char	l_vol[SIZEOF_VOL+1];
	
	int 	arg_count;
	int4	l_long, access_code, l_mode;
	char 	*end_name, *temp_ptr;
	char 	*l_field;
	char 	filespec[132];
	int4 	wfname_mode;
	int	rc;
	struct stat sbuf;
	struct tm *tm_ptr;
	char 	tmp[20];
	char 	tmp_bs[19];

	wtrace(pReadfdr, "ENTRY", "File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s]", cpFile, cpLib, cpVol);

	GETBIN(&l_mode, mode, sizeof(l_mode) );						/* Move to local var.			*/
	if (l_mode != 0 )
	{
		werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);
		return;
	}

	memcpy(l_file, cpFile, SIZEOF_FILE);
	memcpy(l_lib, cpLib, SIZEOF_LIB);
	memcpy(l_vol, cpVol, SIZEOF_VOL);
	
/*	va_start(the_args, mode); */
	arg_count = va_count(the_args);

	arg_count -= 4;
	wfname_mode = 0;

	end_name = wfname(&wfname_mode,l_vol,l_lib,l_file,filespec);			/* generate a file name			*/
	*end_name = '\0';								/* Null terminate			*/

	access_code = READFDR_RC_20_FILE_NOT_FOUND;					/* Assume not found			*/

	if (fexists(filespec))
	{
		access_code = READFDR_RC_0_SUCCESS;					/* File was found			*/
	}

	while ( arg_count > 1 )
	{
		l_field  = va_arg(the_args, char*);					/* Get the field indicator.		*/
		temp_ptr = va_arg(the_args, char*);					/* Get the receiver.			*/
		arg_count -= 2;

		if (access_code != READFDR_RC_0_SUCCESS ) 
		{
			wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Unable to access file.", l_field);
			continue;
		}
		

		if (0==memcmp(l_field,"BS",2))				/* Byte size PIC 9(18) */
		{
			rc=stat(filespec,&sbuf);

			if ( rc != 0 )
			{
				access_code = READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				l_long = sbuf.st_size;
				memset(tmp_bs,0,sizeof(tmp_bs));
				sprintf(tmp_bs,"%018d",l_long);
				strncpy(temp_ptr,tmp_bs,18);
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%18.18s]", l_field, temp_ptr);
			}
		}
		else if (0==memcmp(l_field,"CD",2) ||			/* Creation date.	    YYMMDD	*/
			 0==memcmp(l_field,"CX",2)   )			/* Creation date extended.  YYYYMMDD	*/
		{
			rc=stat(filespec,&sbuf);

			if ( rc != 0 )
			{
				access_code = READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				
				tm_ptr=localtime(&(sbuf.st_ctime));

				if ('D' == l_field[1] && !bFromReadfdr4)
				{
					sprintf(tmp,"%02d%02d%02d",
						tm_ptr->tm_year % 100,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,6);
					wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%6.6s]", l_field, temp_ptr);
				}
				else /* "CX" */
				{
					sprintf(tmp,"%04d%02d%02d",
						tm_ptr->tm_year + 1900,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,8);
					wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%8.8s]", l_field, temp_ptr);
				}
			}
		}
		
		else if (0==memcmp(l_field,"DP",2))			/* Date packing int(4) always 0		*/
		{
			memset(temp_ptr,0,4);
		}
		else if (0==memcmp(l_field,"DT",2))			/* DMS type Alpha(1) always 'D'		*/
		{
			*temp_ptr = 'D';
		}
		else if (0==memcmp(l_field,"EA",2))			/* Number of extents Int(4) always 1	*/
		{
			l_long = 1;
			wswap(&l_long);					/* swap the words			*/
			PUTBIN(temp_ptr, &l_long, sizeof(l_long));
		}
		else if (0==memcmp(l_field,"ED",2))			/* Expiration date Alpha(6) always 991231	*/
		{
			if (bFromReadfdr4)
			{
				memcpy(temp_ptr,"99991231",8);
			}
			else
			{
				memcpy(temp_ptr,"991231",6);
			}
		}
		else if (0==memcmp(l_field,"FT",2))			/* File Type Alpha(1)			*/
		{
			access_code = fileinfo( filespec, "FT", temp_ptr );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
		}
		else if (0==memcmp(l_field,"MD",2) ||			/* Mod date YYMMDD 			*/
			 0==memcmp(l_field,"MX",2))			/* Mod date YYYYMMDD 			*/
		{
			rc=stat(filespec,&sbuf);

			if ( rc != 0 )
			{
				access_code = READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				tm_ptr=localtime(&(sbuf.st_mtime));

				if ('D' == l_field[1] && !bFromReadfdr4)
				{
					sprintf(tmp,"%02d%02d%02d",
						tm_ptr->tm_year % 100,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,6);
					wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%6.6s]", l_field, temp_ptr);
				}
				else /* "MX" */
				{
					sprintf(tmp,"%04d%02d%02d",
						tm_ptr->tm_year + 1900,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,8);
					wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%8.8s]", l_field, temp_ptr);
				}
			}
		}
		else if (0==memcmp(l_field,"RC",2))			/* Record count Int(4) 			*/
		{
			access_code = fileinfo( filespec, "RC", &l_long );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%ld]", l_field, l_long);
				wswap(&l_long);				/* swap the words			*/
				PUTBIN(temp_ptr, &l_long, sizeof(l_long));
			}
		}
		else if (0==memcmp(l_field,"RL",2) ||			/* Record Len/Size Int(4)		*/
			 0==memcmp(l_field,"RS",2))
		{
			access_code = fileinfo( filespec, "RS", &l_long );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%ld]", l_field, l_long);
				wswap(&l_long);				/* swap the words			*/
				PUTBIN(temp_ptr, &l_long, sizeof(l_long));
			}
		}
		else if (0==memcmp(l_field,"RT",2))			/* Record type Alpha(1) 		*/
		{
			access_code = fileinfo( filespec, "RT", temp_ptr );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
			
		}
		else if (0==memcmp(l_field,"IX",2))			/* ISAM Type Alpha(1) 		*/
		{
			access_code = fileinfo( filespec, "IX", temp_ptr );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
		}
		else if (0==memcmp(l_field,"IV",2))			/* ISAM Version Alpha(2) 		*/
		{
			/*
			**	C0	CISAM Indexed
			**	R0	FHISAM Relative
			**	S0	FHISAM Sequential
			**	I3	FHISAM Indexed 3
			**	I4	FHISAM Indexed 4
			**	I8	FHISAM Indexed 8
			**	V2	Vision 2
			**	V3	Vision 3
			**	V4	Vision 4
			**	U0	Unknown
			*/
			access_code = fileinfo( filespec, "IV", temp_ptr );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%2.2s]", l_field, temp_ptr);
			}
		}
		else
		{
			werrlog(ERRORCODE(4),l_field,0,0,0,0,0,0,0);
			access_code = READFDR_RC_40_INVALID_INPUT_PARAM;
		}

		if (READFDR_RC_0_SUCCESS != access_code)
		{
			wtrace(pReadfdr, "FIELD", "Field=[%2.2s] failed with access_code=[%ld]", l_field, access_code);
		}
	}
       
	wtrace(pReadfdr, "RETURN", "Retcode=[%ld]", access_code);
	if ( arg_count == 1 )
	{
		temp_ptr = va_arg(the_args, char *);					/* Get the return code pointer.		*/
		wswap(&access_code);							/* swap the words			*/
		PUTBIN(temp_ptr, &access_code, sizeof(access_code) );
	}
/*	va_end(the_args); */
}

/*
**	ROUTINE:	fileinfo()
**
**	FUNCTION:	Front-end to MF and ACU file info routines
**
**	DESCRIPTION:	First call cisaminfo() and if not a cisam file call 
**			visioninfo() which also handles non-index files.
**
**	ARGUMENTS:	
**	path		The path to the file
**	code		The code requested FT, RC, RS, RT, IX
**	raw_field	The return field
**
**	GLOBALS:	None
**
**	RETURN:		READFDR return code
**
**
*/

static int4 fileinfo(const char* path, const char* code, void* raw_field)
{
	int4 rc;

	rc = cisaminfo( path, code, raw_field );

	if (READFDR_RC_24_NO_FILE_HEADER == rc ||
	    READFDR_RC_68_UNKNOWN_FILE_FORMAT == rc)
	{
		rc = visioninfo( path, code, raw_field );
	}

	if (READFDR_RC_24_NO_FILE_HEADER == rc ||
	    READFDR_RC_68_UNKNOWN_FILE_FORMAT == rc)
	{
		rc = unstructured_fileinfo( path, code, raw_field );
	}

	return rc;
}


/*
**	ROUTINE:	unstructured_fileinfo()
**
**	FUNCTION:       File info for an unstructured file.
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**	path		The path to the file
**	code		The code requested FT, RC, RS, RT, IX
**	raw_field	The return field
**
**	GLOBALS:	None
**
**	RETURN:		READFDR return code
**
**
*/
static int4 unstructured_fileinfo(const char* path, const char* code, void* raw_field)
{
	static int style = -1;

	char	*field;
	int4	*size;

	field = (char*) raw_field;
	size = (int4 *) raw_field;

	if (-1 == style)	/* First time set the style */
	{
		if (get_wisp_option("READFDRV1"))
		{
			style = 1;
		}
		else
		{
			style = 0;
		}
		
	}
	
	if ( memcmp( code, "IX", 2 ) == 0 )				/* File Index Type */
	{
		*field = 'U';	/* Unknown */
		return READFDR_RC_0_SUCCESS;
	}

	if ( memcmp( code, "IV", 2 ) == 0 )				/* File Index Version */
	{
		field[0] = 'U';	/* Unknown */
		field[1] = '0';	
		return READFDR_RC_0_SUCCESS;
	}

	if ( memcmp( code, "RC", 2 ) == 0 )				/* If looking for record count				*/
	{
		if (1==style)
		{
			struct stat sbuf;

			if ( 0 != stat(path,&sbuf) )
			{
				return READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				*size = (sbuf.st_size >= 1) ? 1 : 0;	/* return 1 or 0					*/
				return READFDR_RC_0_SUCCESS;
			}
			
		}
		else
		{
			return READFDR_RC_24_NO_FILE_HEADER;
		}
	}

	if ( memcmp( code, "RS", 2 ) == 0 ||				/* If looking for record size/lenght			*/
	     memcmp( code, "RL", 2 ) == 0    )
	{
		if (1==style)
		{
			*size = 256;
			return READFDR_RC_0_SUCCESS;
		}
		else
		{
			return READFDR_RC_24_NO_FILE_HEADER;
		}
	}

	if ( memcmp( code, "FT", 2 ) == 0 )				/* If looking for file type				*/
	{
		if (1==style)
		{
			*field = 'C';	/* Consecutive */
			return READFDR_RC_0_SUCCESS;
		}
		else
		{
			return READFDR_RC_24_NO_FILE_HEADER;
		}
	}

	if ( memcmp( code, "RT", 2 ) == 0 )				/* If looking for record type				*/
	{
		if (1==style)
		{
			*field = 'V';	/* Variable */
			return READFDR_RC_0_SUCCESS;
		}
		else
		{
			return READFDR_RC_24_NO_FILE_HEADER;
		}
	}

	return( READFDR_RC_40_INVALID_INPUT_PARAM );

}

                                                                                       
/*
**	History:
**	$Log: readfdr.c,v $
**	Revision 1.22  2001-11-08 11:53:20-05  gsl
**	fix warnings
**
**	Revision 1.21  2001-10-30 15:26:16-05  gsl
**	change acuvision() to visioninfo()
**
**	Revision 1.20  2001-10-29 10:46:32-05  gsl
**	Add function IV Index Version
**
**	Revision 1.19  2001-10-23 19:21:18-04  gsl
**	Move all the unstructured_fileinfo() to routine.
**	Add support for 'IX'
**
**	Revision 1.18  2001-10-22 17:03:55-04  gsl
**	Rework for MF FHISAM files.
**	Add fileinfo() which checks both cisam/fhisam and vision
**
**	Revision 1.17  2001-09-20 14:39:39-04  gsl
**	fix ifdesf
**
**	Revision 1.16  2001-09-20 13:50:27-04  gsl
**	Remove VMS ifdefs
**
**	Revision 1.15  1999-09-13 15:52:05-04  gsl
**	removed unused local vars
**
**	Revision 1.14  1999-09-08 19:41:33-04  gsl
**	Added READFDR4 plus reorged from nexted switched to if-elseif-else.
**	Added wtrace() throughout
**
**	Revision 1.13  1998-05-14 17:12:58-04  gsl
**	Add header
**
**	Revision 1.12  1997-10-03 12:14:45-04  gsl
**	YEAR2000 support:
**	Changed CD and MD to do a mod 100 on the year so these will
**	continue to report year of century correctly (YY).
**	Added CX and MX which are modified versions of CD and MD that
**	return a 4 digit year in the format YYYYMMDD.
**
**	Revision 1.11  1996-08-19 18:32:46-04  gsl
**	drcs update
**
**
**
*/
