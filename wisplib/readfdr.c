/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/


/* READFDR.C ... This routine simulates some of the Wang-VS READFDR subroutine operation.					*/

#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <stdarg.h>

#include "idsistd.h"
#include "wcommon.h"
#include "werrlog.h"
#include "wisplib.h"
#include "wfname.h"
#include "idsisubs.h"
#include "filext.h"
#include "wfcisam.h"
#include "wfvision.h"
#include "wdefines.h"
#include "wperson.h"
#include "vssubs.h"



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
	struct tm *tm_ptr;
	char 	tmp[20];
	char 	tmp_bs[19];

	WL_wtrace(pReadfdr, "ENTRY", "File=[%8.8s] Lib=[%8.8s] Vol=[%6.6s]", cpFile, cpLib, cpVol);

	l_mode = WL_get_swap(mode);
	if (l_mode != 0 )
	{
		werrlog(WERRCODE(51002),0,0,0,0,0,0,0,0);
		return;
	}

	memcpy(l_file, cpFile, SIZEOF_FILE);
	memcpy(l_lib, cpLib, SIZEOF_LIB);
	memcpy(l_vol, cpVol, SIZEOF_VOL);
	
/*	va_start(the_args, mode); */
	arg_count = WL_va_count();

	arg_count -= 4;
	wfname_mode = 0;

	end_name = WL_wfname(&wfname_mode,l_vol,l_lib,l_file,filespec);			/* generate a file name			*/
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
			WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Unable to access file.", l_field);
			continue;
		}
		

		if (0==memcmp(l_field,"BS",2))				/* Byte size PIC 9(18) */
		{
#ifdef INT64_DEFINED
			INT64 stat_size;
			rc = WL_stat_size_int8(filespec, &stat_size);
#else
			long stat_size;
			rc = WL_stat_size_long(filespec, &stat_size);
#endif
			if ( rc != 0 )
			{
				access_code = READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				memset(tmp_bs,0,sizeof(tmp_bs));
#if defined(WIN32)
				sprintf(tmp_bs,"%018I64d",stat_size);
#else
				if (sizeof(stat_size) == sizeof(long))
				{
					sprintf(tmp_bs,"%018ld",(long)stat_size);
				}
				else
				{
					sprintf(tmp_bs,"%018lld",stat_size);
				}
#endif
				strncpy(temp_ptr,tmp_bs,18);
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%18.18s]", l_field, temp_ptr);
			}
		}
		else if (0==memcmp(l_field,"CD",2) ||			/* Creation date.	    YYMMDD	*/
			 0==memcmp(l_field,"CX",2)   )			/* Creation date extended.  YYYYMMDD	*/
		{
			time_t create_time;
			rc=WL_stat_ctime(filespec,&create_time);

			if ( rc != 0 )
			{
				access_code = READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				
				tm_ptr=localtime(&create_time);

				if ('D' == l_field[1] && !bFromReadfdr4)
				{
					sprintf(tmp,"%02d%02d%02d",
						tm_ptr->tm_year % 100,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,6);
					WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%6.6s]", l_field, temp_ptr);
				}
				else /* "CX" */
				{
					sprintf(tmp,"%04d%02d%02d",
						tm_ptr->tm_year + 1900,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,8);
					WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%8.8s]", l_field, temp_ptr);
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
			WL_put_swap(temp_ptr, 1);
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
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
		}
		else if (0==memcmp(l_field,"MD",2) ||			/* Mod date YYMMDD 			*/
			 0==memcmp(l_field,"MX",2))			/* Mod date YYYYMMDD 			*/
		{
			time_t mod_time;
			rc=WL_stat_mtime(filespec,&mod_time);

			if ( rc != 0 )
			{
				access_code = READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				tm_ptr=localtime(&mod_time);

				if ('D' == l_field[1] && !bFromReadfdr4)
				{
					sprintf(tmp,"%02d%02d%02d",
						tm_ptr->tm_year % 100,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,6);
					WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%6.6s]", l_field, temp_ptr);
				}
				else /* "MX" */
				{
					sprintf(tmp,"%04d%02d%02d",
						tm_ptr->tm_year + 1900,
						tm_ptr->tm_mon + 1,
						tm_ptr->tm_mday);

					memcpy(temp_ptr,tmp,8);
					WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%8.8s]", l_field, temp_ptr);
				}
			}
		}
		else if (0==memcmp(l_field,"RC",2))			/* Record count Int(4) 			*/
		{
			access_code = fileinfo( filespec, "RC", &l_long );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%ld]", l_field, l_long);
				WL_put_swap(temp_ptr, l_long);
			}
		}
		else if (0==memcmp(l_field,"RL",2) ||			/* Record Len/Size Int(4)		*/
			 0==memcmp(l_field,"RS",2))
		{
			access_code = fileinfo( filespec, "RS", &l_long );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%ld]", l_field, l_long);
				WL_put_swap(temp_ptr, l_long);
			}
		}
		else if (0==memcmp(l_field,"RT",2))			/* Record type Alpha(1) 		*/
		{
			access_code = fileinfo( filespec, "RT", temp_ptr );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
			
		}
		else if (0==memcmp(l_field,"IX",2))			/* ISAM Type Alpha(1) 		*/
		{
			access_code = fileinfo( filespec, "IX", temp_ptr );

			if (READFDR_RC_0_SUCCESS == access_code)
			{
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
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
				WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%2.2s]", l_field, temp_ptr);
			}
		}
		else
		{
			werrlog(WERRCODE(51004),l_field,0,0,0,0,0,0,0);
			access_code = READFDR_RC_40_INVALID_INPUT_PARAM;
		}

		if (READFDR_RC_0_SUCCESS != access_code)
		{
			WL_wtrace(pReadfdr, "FIELD", "Field=[%2.2s] failed with access_code=[%ld]", l_field, access_code);
		}
	}
       
	WL_wtrace(pReadfdr, "RETURN", "Retcode=[%ld]", access_code);
	if ( arg_count == 1 )
	{
		temp_ptr = va_arg(the_args, char *);					/* Get the return code pointer.		*/
		WL_put_swap(temp_ptr, access_code);
	}
/*	va_end(the_args); */
}

/*
**	ROUTINE:	fileinfo()
**
**	FUNCTION:	Front-end to MF and ACU file info routines
**
**	DESCRIPTION:	Checks for Vision files, Cisam/MF FHisam files, unstructured files
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

	rc = WL_visioninfo( path, code, raw_field );

	if (READFDR_RC_24_NO_FILE_HEADER == rc ||
	    READFDR_RC_68_UNKNOWN_FILE_FORMAT == rc)
	{
		rc = WL_cisaminfo( path, code, raw_field );
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
		if (WL_get_wisp_option("READFDRV1"))
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
			int rc;
#ifdef INT64_DEFINED
			INT64 stat_size;
			rc = WL_stat_size_int8(path, &stat_size);
#else
			long stat_size;
			rc = WL_stat_size_long(path, &stat_size);
#endif

			if ( 0 != rc)
			{
				return READFDR_RC_32_STAT_ERROR;
			}
			else
			{
				*size = (stat_size >= 1) ? 1 : 0;	/* return 1 or 0					*/
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
**	Revision 1.37  2012/07/09 02:38:46  gsl
**	WISP 5.1.11
**	Patch READFDR bug with Shared Vision files on Windows.
**	
**	Revision 1.36  2007/07/31 16:51:07  gsl
**	Change INT8 to INT64 to avoid conflicts on WIN32
**	
**	Revision 1.35  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.34  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.33  2003/01/29 21:50:08  gsl
**	Switch to use vssubs.h
**	
**	Revision 1.32  2002/12/10 17:09:17  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.31  2002/10/18 19:14:10  gsl
**	Cleanup
**	
**	Revision 1.30  2002/10/08 15:44:39  gsl
**	Change int8 to INT8 to avoid conficts
**	
**	Revision 1.29  2002/10/04 21:04:35  gsl
**	fix typo
**	
**	Revision 1.28  2002/10/04 21:00:54  gsl
**	Change to use WL_stat_xxx() routines
**	
**	Revision 1.27  2002/07/16 16:24:53  gsl
**	Globals
**	
**	Revision 1.26  2002/07/12 19:10:15  gsl
**	Global unique WL_ changes
**	
**	Revision 1.25  2002/07/12 17:00:59  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.24  2002/07/11 20:29:12  gsl
**	Fix WL_ globals
**	
**	Revision 1.23  2002/07/10 21:05:22  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.22  2001/11/08 16:53:20  gsl
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
**	Added WL_wtrace() throughout
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
