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

#ifdef VMS
#include <rms.h>
#include <ssdef.h>
#include <stat.h>
#endif

#ifndef VMS
#include <sys/types.h>
#include <sys/stat.h>
#endif

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
#include "wfvision.h"
#include "wdefines.h"

#ifdef VMS
#define	FILE_TYPE	001
#define	RECORD_COUNT	002
#define	RECORD_SIZE	003
#endif


#define		ROUTINE		51000

static int bFromReadfdr4 = 0;
static char *pReadfdr;

static void READFDRX(const char* cpFile, const char* cpLib, const char* cpVol, const int4* mode, va_list the_args);


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
	char 	filespec[132], filespec_idx[132], filespec_noext[132];
	int4 	wfname_mode;
	int	rc;
	struct stat sbuf;
	struct tm *tm_ptr;
	char 	tmp[20];
	char 	tmp_bs[19];
	int	is_cisam;

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

#ifdef VMS
	if (!memcmp(WISPFILEXT,"LIS ",4)) wfname_mode = IS_PRINTFILE;			/* Special case for printfiles.		*/
#endif

	end_name = wfname(&wfname_mode,l_vol,l_lib,l_file,filespec);			/* generate a VMS file name		*/
	*end_name = '\0';								/* Null terminate			*/

	/*
	**	filespec	- the full filespec  (CISAM - data file)
	**	filespec_idx	- (CISAM - index file)
	**	filespec_noext	- the filespec with any extension stripped off (CISAM - generic name)
	*/

	access_code = 20;								/* Assume not found			*/
	is_cisam = 0;									/* Assume not CISAM			*/

	if (fexists(filespec))
	{
		access_code = 0;							/* File was found			*/
	}

#if defined(unix) || defined(MSFS)
	if ( !hasext(filespec) )							/* If no extension (maybe CISAM)	*/
	{
		strcpy(filespec_noext,filespec);
		strcpy(filespec_idx,filespec);
		strcat(filespec_idx, ".idx");					

		if (fexists(filespec_idx))						/* Found index file			*/
		{
			if (0 == access_code)						/* Data portion already found		*/
			{
				access_code = 0;					/* Found data & index			*/
				is_cisam = 1;						/* This is a CISAM file(s)		*/
			}
			else								/* Data file has not yet been found	*/
			{
				strcat(filespec, ".dat");
				if (fexists(filespec))					/* Try .dat file extension.		*/
				{
					access_code = 0;				/* Found data & index			*/
					is_cisam = 1;					/* This is a CISAM file(s)		*/
				}
			}
		}
	}
#endif /* unix || MSFS */

	while ( arg_count > 1 )
	{
		l_field  = va_arg(the_args, char*);					/* Get the field indicator.		*/
		temp_ptr = va_arg(the_args, char*);					/* Get the receiver.			*/
		arg_count -= 2;

		if (access_code != 0 ) 
		{
			wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Unable to access file.", l_field);
			continue;
		}
		

		if (0==memcmp(l_field,"BS",2))				/* Byte size PIC 9(18) */
		{
			rc=stat(filespec,&sbuf);

			if ( rc )
			{
				access_code = rc;
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
#ifdef VMS
			strcat(filespec,";-0");				/* we want the earliest version		*/
#endif

			if (is_cisam)
			{
				rc=stat(filespec_idx,&sbuf);
			}
			else
			{
				rc=stat(filespec,&sbuf);
			}

			if ( rc < 0 )
			{
				access_code = 32;
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
#ifdef VMS
			read_fab(&l_long, filespec, FILE_TYPE);	    	/* Get the file organization.		*/
			*temp_ptr=l_long;			    	/* FILE_TYPE returns a character.	*/
			if (wfname_mode == IS_PRINTFILE) *temp_ptr='P';	/* Redundancy, just in case.		*/
#else /* !VMS */
			if (is_cisam)
			{
				rc = cisaminfo( filespec_noext, "FT", temp_ptr );
				access_code = rc;
			}
			else
			{

				rc = acuvision( filespec, "FT", temp_ptr );
				access_code = rc;
			}
#endif /* !VMS */
			if (0==access_code)
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
		}
		else if (0==memcmp(l_field,"MD",2) ||			/* Mod date YYMMDD 			*/
			 0==memcmp(l_field,"MX",2))			/* Mod date YYYYMMDD 			*/
		{
#ifdef VMS
			strcat(filespec,";");				/* we want the latest version		*/
#endif

			if (is_cisam)
			{
				rc=stat(filespec_idx,&sbuf);
			}
			else
			{
				rc=stat(filespec,&sbuf);
			}
			if ( rc < 0 )
			{
				access_code = 32;
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
#ifdef VMS
			read_fab(&l_long, filespec, RECORD_COUNT);      /* Get the record count.		*/
			wswap(&l_long);				    	/* swap the words			*/
			PUTBIN(temp_ptr, &l_long, sizeof(l_long));
#else
			if (is_cisam)
			{
				rc = cisaminfo( filespec_noext, "RC", (char*)&l_long );
			}
			else
			{
				rc = acuvision( filespec, "RC", (char*) &l_long );
			}

			if ( rc )
			{
				access_code = rc;
			}
			else
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%ld]", l_field, l_long);
				wswap(&l_long);				/* swap the words			*/
				PUTBIN(temp_ptr, &l_long, sizeof(l_long));
			}
			
#endif
		}
		else if (0==memcmp(l_field,"RL",2) ||			/* Record Len/Size Int(4)		*/
			 0==memcmp(l_field,"RS",2))
		{
#ifdef VMS
			read_fab(&l_long, filespec, RECORD_SIZE);	/* Get the record size.			*/
			wswap(&l_long);					/* swap the words			*/
			PUTBIN(temp_ptr, &l_long, sizeof(l_long));
#else
			if (is_cisam)
			{
				rc = cisaminfo( filespec_noext, "RS", (char*)&l_long );
			}
			else
			{
				rc = acuvision( filespec, "RS", (char*)&l_long );
			}

			if ( rc )
			{
				access_code = rc;
			}
			else
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%ld]", l_field, l_long);
				wswap(&l_long);				/* swap the words			*/
				PUTBIN(temp_ptr, &l_long, sizeof(l_long));
			}
#endif
		}
#ifndef VMS
		else if (0==memcmp(l_field,"RT",2))			/* Record type Alpha(1) 		*/
		{
			if (is_cisam)
			{
				rc = cisaminfo( filespec_noext, "RT", temp_ptr );
			}
			else
			{
				rc = acuvision( filespec, "RT", temp_ptr );
			}

			if ( rc )
			{
				access_code = rc;
			}
			else
			{
				wtrace(pReadfdr, "FIELD", "Field=[%2.2s] Result=[%1.1s]", l_field, temp_ptr);
			}
			
		}
#endif
		else
		{
			werrlog(ERRORCODE(4),l_field,0,0,0,0,0,0,0);
			access_code = 40;
		}

		if (0 != access_code)
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
                                                                                       
#ifdef VMS
static int read_fab(recvr, file_name, action)

char *file_name;
int *recvr;
short action;

{
	struct FAB file_block;
        struct RAB record_block;

	int rms_status;
        char *byte_buffer;

	file_block = cc$rms_fab;
	file_block.fab$l_fna = file_name;
	file_block.fab$b_bks = 4;
	file_block.fab$l_dna = ".DAT";
	file_block.fab$b_dns = 4;
	file_block.fab$b_fac = FAB$M_GET;
	file_block.fab$b_fns = 80;                                                     
	file_block.fab$l_fop = FAB$M_CIF;
	file_block.fab$b_shr = FAB$V_SHRPUT | FAB$V_SHRGET | FAB$V_SHRDEL | FAB$V_SHRUPD;
	record_block = cc$rms_rab;
	record_block.rab$l_fab = &file_block;
	
	rms_status = sys$open(&file_block);
	if (rms_status != RMS$_NORMAL) return;

	switch(action)
	{
	    case FILE_TYPE:
	    {
		switch(file_block.fab$b_org)
		{
		    case FAB$C_SEQ:							/* File organization is sequential.	*/
		    {
			    *recvr='C';							/* 'C' for Consecutive.			*/
			    break;
		    }
		    case FAB$C_REL:							/* File organization is relative.	*/
		    {
			    *recvr='R';
			    break;
		    }
		    case FAB$C_IDX:							/* File organization is indexed.	*/
		    {
			    *recvr='I';
			    break;
		    }
		    case FAB$C_HSH:							/* File organization is hashed.		*/
		    {
			    *recvr='U';							/* 'U' for Unknown to the WANG.		*/
			    break;
		    }
		    default:								/* VAX doesn't give any others.		*/
		    {
			    *recvr='U';
			    break;
		    }
		}
		break;
	    }
	    case RECORD_COUNT:
	    {
	    	byte_buffer = malloc(file_block.fab$w_mrs);
	    	record_block.rab$l_ubf = byte_buffer;
	    	record_block.rab$w_usz = file_block.fab$w_mrs;
	    	record_block.rab$l_rop = RAB$V_NLK;

	    	rms_status = sys$connect(&record_block);
	    	rms_status = sys$get(&record_block);

	    	if (rms_status == RMS$_NORMAL ||				/* Just verify successful retrieval of RAB info.*/
		    rms_status == RMS$_OK_RLK ||				/* Record was locked but we read it.		*/
		    rms_status == RMS$_RLK ||					/* Record was locked, so it must exist.		*/
		    rms_status == RMS$_OK_RRL)
	    	{
	    		*recvr = 1;						/* Return 1 record if records exist.		*/
	    	}
	    	else
	    	{
	    		*recvr = 0;						/* Return 0 records, if not.			*/
	    	}
		break;
	    }
	    case RECORD_SIZE:
	    {
		*recvr = file_block.fab$w_mrs;					/* Return the maximum record size.		*/
		break;
	    }
	    default:	break;
	}
	rms_status = sys$close(&file_block);

}
#endif	/* #ifdef VMS	*/

/*
**	History:
**	$Log: readfdr.c,v $
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
