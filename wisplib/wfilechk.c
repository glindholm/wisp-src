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
**	File:		wfilechk.c
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	WISP FILE STATUS CHECK (called from declaratives)
** 			Check file status in declaratives, and take appropriate actions.
**
**	Routines:	
**	wfilechk()
**	WFILECHK2()
*/

/*
**	Includes
*/

#include <errno.h>
#include <string.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wcommon.h"
#include "cobrun.h"
#include "wdefines.h"
#include "wglobals.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "wexit.h"
#include "wfname.h"
#include "wperson.h"


/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
static void wisp_set_last_filecheckstatus(const char status[2]);
static void wisp_set_last_filecheckstatus_ext(const char *status);

void WL_cobfileop2wispfileop(const char acufileop[COB_FILEOP_SIZE], char wispfileop[2]);
void WL_wispfileop2cobfileop(const char wispfileop[2], char cobfileop[COB_FILEOP_SIZE]);


/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
static int x_wispfilecheck(
		const char wispfileop[2],
		const char *cobfileop,
	       char   decl_stat[2], 
	       const char file_stat[2],
	       const char file_status_extended[COB_EXTENDED_FILE_STATUS_SIZE],
	       int is_norespecify,
	       int is_declare,
	       const char file_vol[6],
	       const char file_lib[8],
	       const char file_fil[8],
	       const char cob_filepath[COB_FILEPATH_LEN],
	       const char* select_name_str,
	       const char app_name[8]);


static const char *msg_filestat(const char* file_stat, const char* x_stat);



/*
**	ROUTINE:	WFILECHK3()
**
**	FUNCTION:	Check file status codes as part of declaratives handling.
**
**	DESCRIPTION:	- OPEN respecify logic
**			- READ with HOLD retry
**			- Error reporting
**
**	ARGUMENTS:
**	file_operation		Alpha(20)	Last file operation (Acucobol sytle op names)	
**	file_stat		Alpha(2)	The file status code 
**	file_status_extended	Alpha(10)	The extended file status code(s)
**	file_attributes		Alpha(10)	The file attributes string (ATTR-xxx)
**	file_vol		Alpha(6)	The volume name
**	file_lib		Alpha(8)	The library name
**	file_fil		Alpha(8)	The file name
**	cob_filepath		Alpha(80)	The native filepath
**	select_name		Alpha(40)	The SELECT name
**	app_name		Alpha(40)	The application name
**	skip_declaratives	Alpha(1)	Return "Y"/"N" to skip declaratives 
**	has_declaratives	Alpha(1)	Is there user coded declaratives "Y"/"N"
**
**
*/
void WFILECHK3(
	const char file_operation[COB_FILEOP_SIZE],
	const char file_status[2],
	const char file_status_extended[COB_EXTENDED_FILE_STATUS_SIZE],
	char file_attributes[WISP_FILE_ATTR_SIZE],
	const char file_vol[6],
	const char file_lib[8],
	const char file_fil[8],
	const char cob_filepath[COB_FILEPATH_LEN],
	const char select_name[COB_SELECT_NAME_SIZE],
	const char app_name[40],
	char skip_declaratives[1],
	const char has_declaratives[1])
{
	char l_cobfileop[COB_FILEOP_SIZE+1];
	char wispfileop[2];
	char decl_status[2];
	int4 mode;
	int err;
	char select_name_str[COB_SELECT_NAME_SIZE+1];

	WL_cobx2cstr(l_cobfileop, file_operation, COB_FILEOP_SIZE);
	WL_cobfileop2wispfileop(file_operation, wispfileop);
	WL_cobx2cstr(select_name_str,select_name,COB_SELECT_NAME_SIZE);

	wtrace("WFILECHK3", "ENTRY", 
	       "Op=[%s (%2.2s)] File=[%s] (%8.8s in %8.8s on %6.6s) App=[%8.8s] Status=[%2.2s][%10.10s] Attr=[%10.10s] Decl=[%c]",
	       l_cobfileop, wispfileop, select_name_str, file_fil, file_lib, file_vol, app_name, 
	       file_status, file_status_extended, file_attributes, has_declaratives[0]);

	wisp_fileattr2mode(file_attributes, &mode);

	if (has_declaratives[0] == 'Y')
	{
		/* Added to 5.0.01 to fix bug */
		mode |= IS_DECLARE;
	}


	memcpy(decl_status,wispfileop,2);

	err = x_wispfilecheck(
		wispfileop, 
		l_cobfileop,
		decl_status,
		file_status,
		file_status_extended,
		(mode & IS_NORESPECIFY),
		(mode & IS_DECLARE),
		file_vol,
		file_lib,
		file_fil,
		cob_filepath,
		select_name_str,
		app_name);

	if (err) 
	{
		mode = mode | IS_ERROR;	/* Set the error bit for wfopen().	*/
	}

	wisp_mode2fileattr(mode, file_attributes);

	if (0==memcmp(decl_status,"00",2))
	{
		skip_declaratives[0] = 'Y';
	}
	else
	{
		skip_declaratives[0] = 'N';
	}

	wtrace("WFILECHK3", "RETURN", "File=[%s] Attr=[%10.10s] SkipDecl=[%c]",
		select_name_str, file_attributes, skip_declaratives[0]);
}



/*
**	ROUTINE:	WFILECHK2()
**
**	FUNCTION:	Check file status codes as part of declaratives handling.
**
**	DESCRIPTION:	- OPEN respecify logic
**			- READ with HOLD retry
**			- Error reporting
**
**	ARGUMENTS:	
**	decl_stat	Coded I/O operation  (reset to "00" if skipping user declartives)
**				OP	OPEN
**				RD	READ
**				RW	REWRITE
**				DE	DELETE
**				ST	START
**				WR	WRITE
**				CL	CLOSE
**				SO	SORT
**	file_stat	The file status code 
**	file_status_extended		The extended file status code(s)
**	x_stat2		The extended file status code(s)
**	sflag		The S-filename status flag  (IS_ERROR bit set on open error)
**	file_vol	The volume name
**	file_lib	The library name
**	file_fil	The file name
**	cob_filepath	The native filepath
**	select_name	The SELECT name
**	app_name	The application name
**
**	GLOBALS:	?
**
**	RETURN:		?
**
**	WARNINGS:	?
**
*/
void WFILECHK2(char   decl_stat[2], 
	       const char file_stat[2],
	       const char file_status_extended[COB_EXTENDED_FILE_STATUS_SIZE],
	       const char x_stat2[COB_EXTENDED_FILE_STATUS_SIZE],
	       int4* sflag,
	       const char file_vol[6],
	       const char file_lib[8],
	       const char file_fil[8],
	       const char cob_filepath[COB_FILEPATH_LEN],
	       const char select_name[COB_SELECT_NAME_SIZE],
	       const char app_name[8])
{
	int err;
	char wispfileop[2];
	char l_cobfileop[COB_FILEOP_SIZE+1];
	char file_attributes[WISP_FILE_ATTR_SIZE];
	char select_name_str[COB_SELECT_NAME_SIZE+1];

	memcpy(wispfileop,decl_stat,2);
	WL_wispfileop2cobfileop(wispfileop, l_cobfileop);
	wisp_mode2fileattr(*sflag, file_attributes);
	WL_cobx2cstr(select_name_str,select_name,COB_SELECT_NAME_SIZE);

	wtrace("WFILECHK2", "ENTRY", "Op=[%s (%2.2s)] File=[%s] (%8.8s in %8.8s on %6.6s) App=[%8.8s] Status=[%2.2s][%10.10s] Attr=[%10.10s]",
	       l_cobfileop, wispfileop, select_name_str, file_fil, file_lib, file_vol, app_name, file_stat, file_status_extended, file_attributes);

	err = x_wispfilecheck(
		wispfileop, 
		l_cobfileop,
		decl_stat, 
		file_stat,
		file_status_extended,
		(*sflag & IS_NORESPECIFY),
		(*sflag & IS_DECLARE),
		file_vol,
		file_lib,
		file_fil,
		cob_filepath,
		select_name_str,
		app_name);

	if (err) 
	{
		*sflag = *sflag | IS_ERROR;	/* Set the error bit for wfopen().	*/
	}

}

static int x_wispfilecheck(
		const char wispfileop[2],
		const char *cobfileop,
		char   decl_stat[2], 
	       const char file_stat[2],
	       const char file_status_extended[COB_EXTENDED_FILE_STATUS_SIZE],
	       int is_norespecify,
	       int is_declare,
	       const char file_vol[6],
	       const char file_lib[8],
	       const char file_fil[8],
	       const char cob_filepath[COB_FILEPATH_LEN],
	       const char* select_name_str,
	       const char app_name[8])
{
	short	wang_filestatus, open_error, reportit, force_error;
	int	temp_errno;
	char	msg1[80],msg2[80],msg3[80],msg4[80],msg5[80],msg6[80],msg7[80],msg8[80];
	char	acufilestat[COB_EXTENDED_FILE_STATUS_SIZE+1] = "00";

	WL_setprogid(app_name);								/* Set the PROGID global variable.	*/
	temp_errno = errno;

	wisp_set_last_filecheckstatus(file_stat);					/* And store it away for 'C'.		*/

	if ( wisp_acu_cobol() )
	{
		/* Save the acucobol extended filestat as a string */
		char *ptr;
		
		memcpy(acufilestat, file_status_extended, COB_EXTENDED_FILE_STATUS_SIZE);
		acufilestat[10] = '\0';
		if ((ptr = strchr(acufilestat,' ')))
		{
			*ptr = '\0';
		}
		/* Minimum 4 chars, pad with spaces */
		if (strlen(acufilestat) < 4)
		{
			strcat(acufilestat,"    ");
			acufilestat[4] = '\0';
		}
		wisp_set_last_filecheckstatus_ext(acufilestat);
	}
	wang_filestatus = TRUE;								/* Assume a WANG file status.		*/
	reportit = TRUE;								/* Always report non-wang errors.	*/
	force_error = FALSE;								/* Don't force				*/

	if (0==memcmp("OP",wispfileop,2)) open_error = TRUE;				/* This is an OPEN error.		*/
	else		 		  open_error = FALSE;				/* Not true.				*/

	switch (file_stat[0])
	{
	case '0':
		wang_filestatus = FALSE;					
		reportit = FALSE;							/* NEVER report '0' level errors.	*/
		break;

	case '1':
		switch(file_stat[1])
		{
		case '0':								/* "10" AT END				*/
		case '1':								/* "11" AT END of tape, no next tape	*/
			wang_filestatus = TRUE;
			break;
		case '3':								/* "13" No next logical record (at end)	*/
		case '4':								/* "14" Relative record number too large*/
		case '5':								/* "15" Optional file not present 	*/
		case '6':								/* "16" No valid next record		*/
		default:
			wang_filestatus = FALSE;					/* No equivalent wang status.		*/
			break;
		}
		break;
	case '2':
		switch(file_stat[1])
		{
		case '0':								/* "20" Invalid Key			*/
		case '1':								/* "21" Key out of sequence		*/
		case '2':								/* "22" Duplicate Key			*/
		case '3':								/* "23" Key doesn't exist		*/
		case '4':								/* "24" Boundary Violation		*/
		case '5':								/* "25" Optional file not present 	*/
			wang_filestatus = TRUE;
			break;
		default:
			wang_filestatus = FALSE;
			break;
		}
		break;
	case '3':
		switch(file_stat[1])
		{
		case '4':								/* "34" WRITE Boundary violation.	*/
		case '5':								/* "35" FILE not found (NORESPECIFY)	*/
		case '7':								/* "37" OPEN Inappropriate device type.	*/
		case '8':								/* "38" OPEN File previously closed lock*/
		case '9':								/* "39" OPEN Conflict of file attrib.	*/
			wang_filestatus = TRUE;
			break;
		default:
			wang_filestatus = FALSE;
			break;
		}
		break;
	case '4':
		switch(file_stat[1])
		{
		case '1':								/* "41" File already open		*/
		case '2':								/* "42" File not open			*/
		case '3':								/* "43" DEL/REW No previous read.	*/
		case '4':								/* "44" REWRITE Invalid rec size.	*/
		case '6':								/* "46" NO Current record.		*/
		case '7':								/* "47" READ File not open/incomp mode.	*/
		case '8':								/* "48" WRITE ""       ""      "" 	*/
		case '9':								/* "49" DELETE/REWRITE  ""     ""	*/
			wang_filestatus = FALSE;
			force_error = TRUE;						/* OPEN cannot recover.			*/
			break;
		default:
			wang_filestatus = FALSE;
			break;
		}
		break;
	case '9':
 		if (wisp_acu_cobol())
		{
			switch(file_stat[1])
			{
			case '0':
			case '1':							/* "91" File Not Open			*/
			case '7':							/* "97" Invalid rec len			*/
			case '8':							/* "98" Index is corrupt		*/
			case 'A':							/* "9A" Out of memory on SORT		*/
			case 'B':							/* "9B" Not Supported operation		*/
				wang_filestatus = FALSE;
				break;
			case '3':							/* "93" File locked 			*/
			case '9':							/* "99" Read on LOCKED record		*/
				wang_filestatus = FALSE;
				reportit =FALSE;
				break;
			case '2':							/* "92" File already open		*/
			case '4':							/* "94" Bad open			*/
			case '5':							/* "95" OPEN No space on device.	*/
				force_error = TRUE;					/* OPEN cannot recover.			*/
				wang_filestatus = FALSE;
				break;
			case '6':							/* "96" Undefined record ptr		*/
				wang_filestatus = TRUE;
				break;
			default:
				wang_filestatus = FALSE;
				break;
			}
		}
 		else if (wisp_mf_cobol())
		{
			switch((unsigned char)file_stat[1])
			{
			case 'A':							/* "9A" File locked 			*/
			case 'D':							/* "9D" Read on LOCKED record		*/
				wang_filestatus = FALSE;
				reportit =FALSE;
				break;
			case 213:							/* "9(213)" Too many locks.		*/
				force_error = TRUE;					/* OPEN cannot recover.			*/
				wang_filestatus = FALSE;
				break;
			default:
				wang_filestatus = FALSE;
				break;
			}
		}
		else 
		{
			wang_filestatus = FALSE;
		}
		break;
	default:
		wang_filestatus = FALSE;
		break;
	}

	/*
	**	Special code for record locked status codes.
	**	If one occurs on a READ it should be returned to the COBOL.
	**		RD READ		Return to COBOL
	**		RW REWRITE	Declaratives
	**		DE DELETE	Declaratives
	**		OP OPEN		N/A
	**		ST START	N/A (doesn't generate a record locked)
	**		WR WRITE	N/A
	**		CL CLOSE	N/A
	**		SO SORT		??? 
	*/

	if (0==memcmp(file_stat, wisp_get_hardlock(), 2))				/* If a record lock on a ...		*/
	{
		if (0==memcmp("RD",wispfileop,2))					/* READ					*/
		{
			wang_filestatus = FALSE;					/*	return to COBOL			*/
			reportit = FALSE;
		}
		else if (0==memcmp("RW",wispfileop,2) ||				/* REWRITE				*/
			 0==memcmp("DE",wispfileop,2)   )				/* DELETE				*/
		{
			wang_filestatus = FALSE;
			reportit = TRUE;						/* 	do declaratives			*/
		}
	}

	if (OPTION_ALLSTATUSKEYS)						/* If set then we want to pass all status keys	*/
	{									/* thru to the user define declaratives, so we	*/
		wang_filestatus = TRUE;						/* always say it is a Wang filestatus code.	*/
	}
											/* Here we report the error directly to	*/
											/* the user, because there is no way the*/
											/* Wang program could handle this.	*/

	if (open_error)
	{
		if (is_norespecify) force_error = FALSE;	/* Open, NORESPECIFY == don't error	*/
	}
	else /* !open_error */
	{
		if (wang_filestatus)
		{
			if (!is_declare) force_error = TRUE;	/* Non-Open, no declaratives == do error*/
		}
		else /* !wang_filestatus */
		{
			if (reportit) force_error = TRUE;	/* Non-Open, non-Wang  == do error	*/
		}
	}

	if ( force_error ) 
	{
		char cob_filepath_str[COB_FILEPATH_LEN + 40];

		msg1[0] = '\0';
		msg2[0] = '\0';
		msg3[0] = '\0';
		msg4[0] = '\0';
		msg5[0] = '\0';
		msg6[0] = '\0';
		msg7[0] = '\0';
		msg8[0] = '\0';


		WL_cobx2cstr(cob_filepath_str,cob_filepath,COB_FILEPATH_LEN);

		sprintf(msg7,"File [%8.8s] Lib [%8.8s] Vol [%6.6s]",file_fil,file_lib,file_vol);
		sprintf(msg8,"File [%s]",cob_filepath_str);					/* Display native file name in message.	*/


		sprintf( msg1, "RUN PROGRAM = %8.8s SUB-PROGRAM = %8.8s FILE = %1.25s", wisp_get_runname(), app_name,select_name_str); 
		sprintf( msg2, "ERROR DETECTED AND USER ERROR EXIT NOT IN USE");
		if ( wisp_acu_cobol() )
		{
			sprintf( msg3, "FILE STATUS = %c%c [%s]   (%s)", 
				file_stat[0], file_stat[1], &acufilestat[2], cobfileop);
			strcpy(  msg4, msg_filestat(file_stat,&acufilestat[2]) );
		}
		else if (wisp_mf_cobol())
		{
			if ('9' != file_stat[0])
				sprintf( msg3, "FILE STATUS = %c%c   (%s)", file_stat[0], file_stat[1], cobfileop);
			else
				sprintf( msg3, "FILE STATUS = %c/RT%03d   (%s)", file_stat[0], (unsigned)file_stat[1], cobfileop);
			strcpy(  msg4, msg_filestat(file_stat,"00") );
		}
		else
		{
			sprintf( msg3, "FILE STATUS = %c%c   (%s)", file_stat[0], file_stat[1], cobfileop);
			strcpy(  msg4, msg_filestat(file_stat,"00") );
		}


		WL_err_getparm("FILESTAT","0001", "ERROR ",msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8 );
		wisp_set_LINKCOMPCODE(16);
		SETRETCODE("016");
		wexit(16L);
	}

	/* Don't report error */
										/* Here we decide if DECLARATIVES should*/
										/* Be executed.				*/
	if (open_error)								/* If it's an open error...		*/
	{
		if (is_norespecify)						/* And NORESPECIFY, execute DECLARATIVES*/
		{
			memcpy(decl_stat,file_stat,2);				/* Signal with file status.		*/
		}
		else								/* RESPECIFY is allowed, Don't do decl.	*/
		{
			memcpy(decl_stat,"00",2);				/* Clear WISP-DECLARATIVES-STATUS field.*/
			return 1;
		}
	}
	else if (wang_filestatus)						/* Not an open error.			*/
	{									/* A valid Wang error.			*/
		memcpy(decl_stat,file_stat,2);					/* Must have DECLARATIVES so use them.	*/
	}
	else									/* It must be a 'safe' non-wang error.	*/
	{
		memcpy(decl_stat,"00",2);					/* Clear WISP-DECLARATIVES-STATUS field.*/
	}

	return 0;
}

/*
**	ROUTINE:	WFILECHK()
**
**	FUNCTION:	A front-end to WFILECHK2()
**
**	DESCRIPTION:	Convert the older format for the extended file statuses
**			then call WFILECHK2().
**
**	ARGUMENTS:	See WFILECHK2()
**
**	GLOBALS:	See WFILECHK2()
**
**	RETURN:		See WFILECHK2()
**
**	WARNINGS:	See WFILECHK2()
**
*/
void WFILECHK( char   decl_stat[2], 
	       const char   file_stat[2],
	       const char*  file_status_extended,
	       const char*  x_stat2,
	       int4* sflag,
	       const char   file_vol[6],
	       const char   file_lib[8],
	       const char   file_fil[8],
	       const char   cob_filepath[COB_FILEPATH_LEN],
	       const char   select_name[COB_SELECT_NAME_SIZE],
	       const char   app_name[8])
{
	char	x_stat_x1[COB_EXTENDED_FILE_STATUS_SIZE], x_stat_x2[COB_EXTENDED_FILE_STATUS_SIZE];
	
	memset(x_stat_x1, ' ', sizeof(x_stat_x1));
	memset(x_stat_x2, ' ', sizeof(x_stat_x2));
	
	if (wisp_acu_cobol())
	{
		memcpy(x_stat_x1, file_status_extended, 4);	/* PIC X(4) */
	}

	WFILECHK2(decl_stat,file_stat,x_stat_x1,x_stat_x2,sflag,file_vol,file_lib,file_fil,cob_filepath,select_name,app_name);
	
}

static const char *msg_filestat(const char* file_stat, const char* x_stat)
{
	char *ptr;

	ptr = " ";

	switch (file_stat[0])
	{
	case '0':
		switch(file_stat[1])
		{
		case '0':	ptr = "Success";			break;
		case '2':	ptr = "Duplicate alternate key";	break;
		case '4':	ptr = "Wrong Record length";		break;
		case '5':	ptr = "Optional file not present";	break;
		case '7':	ptr = "Invalid option for device";	break;
		case 'M':	ptr = "Optional feature not supported";	break;
		default:	ptr = "UNKNOWN (Success)";		break;
		}
		break;
	case '1':
		switch(file_stat[1])
		{
		case '0':	ptr = "End of File reached";				break;
		case '1':	ptr = "No next tape (AT END)";				break;
		case '3':	ptr = "READ No next logical record (AT END)";		break;
		case '4':	ptr = "Relative record number too large (AT END)"; 	break;
		case '5':	ptr = "READ Optional file not present (AT END)";	break;
		case '6':	ptr = "READ No valid next record (AT END)";		break;
		default:	ptr = "UNKNOWN (AT END)";				break;
		}
		break;
	case '2':
		switch(file_stat[1])
		{
		case '0':	ptr = "Invalid Key";					break;
		case '1':	ptr = "Key out of sequence (INVALID KEY)";		break;
		case '2':	ptr = "Duplicate Key (INVALID KEY)";			break;
		case '3':	ptr = "Record not found (INVALID KEY)";			break;
		case '4':
			if (wisp_acu_cobol() && 0==memcmp("00",x_stat,2))
				ptr = "Disk full (INVALID KEY)";
			else if (wisp_acu_cobol() && 0==memcmp("01",x_stat,2))
				ptr = "Relative record number too large (INVALID KEY)";
			else
				ptr = "Boundary Violation (INVALID KEY)";
			break;
		case '5':	ptr = "Optional file not present (INVALID KEY)"; 	break;
		default:	ptr = "UNKNOWN (INVALID KEY)";				break;
		}
		break;
	case '3':
		switch(file_stat[1])
		{
		case '0':	ptr = "Permanent error";				break;
		case '4':	ptr = "WRITE Boundary violation (Disk Full)";		break;
		case '5':	ptr = "FILE not found";					break;
		case '7':
			if (wisp_acu_cobol() && 0==memcmp("07",x_stat,2))
				ptr = "Access denied (OPEN)";
			else if (wisp_acu_cobol() && 0==memcmp("09",x_stat,2))
				ptr = "Invalid file type (OPEN)";
			else
				ptr = "OPEN mode/access not supported";
			break;
		case '8':	ptr = "File previously closed with lock (OPEN)";	break;
		case '9':	ptr = "OPEN Conflicts with file attributes";		break;
		default:	ptr = "UNKNOWN";					break;
		}
		break;
	case '4':
		switch(file_stat[1])
		{
		case '1':	ptr = "File already open (OPEN)";			break;
		case '2':	ptr = "File not open (CLOSE)";				break;
		case '3':	ptr = "No previous read";				break;
		case '4':	ptr = "Invalid record size (REWRITE)";			break;
		case '6':	ptr = "No Current record";				break;
		case '7':	ptr = "File not open, or incompatable open mode";	break;
		case '8':	ptr = "File not open, or incompatable open mode";	break;
		case '9':	ptr = "File not open, or incompatable open mode";	break;
		default:	ptr = "UNKNOWN";					break;
		}
		break;
	case '9':
		if (wisp_mf_cobol())
		{
			switch((unsigned char)file_stat[1])
			{
			case 35:	ptr = "Access denied";				break;
			case 'A':	ptr = "File locked";				break;
			case 'D':	ptr = "Record locked by another program";	break;
			case 213:	ptr = "Too many locks";				break;
			default:	ptr = "UNKNOWN";				break;
			}
		}
		else if (wisp_acu_cobol())
		{
			switch(file_stat[1])
			{
			case '0':
				if (0==memcmp("01",x_stat,2))
					ptr = "File not open, or incompatable open mode";
				else if (0==memcmp("02",x_stat,2))
					ptr = "No current record defined";
				else if (0==memcmp("07",x_stat,2))
					ptr = "Access denied (OPEN)";
				else
					ptr = "FILE incompatable open mode";
				break;
			case '1':	ptr = "File not open";			break;
			case '2':	ptr = "File already open";		break;
			case '3':
				if (0==memcmp("03",x_stat,2))
					ptr = "File closed with LOCK";
				else
					ptr = "File locked by another program";	
				break;
			case '4':
				if (0==memcmp("00",x_stat,2))
					ptr = "Open error";
				else if (0==memcmp("10",x_stat,2))
					ptr = "Too many files open";
				else if (0==memcmp("20",x_stat,2))
					ptr = "File not found";
				else if (0==memcmp("62",x_stat,2))
					ptr = "LINAGE value out of range";
				else
					ptr = "OPEN Conflicts with file attributes";
				break;
			case '5':
				if (0==memcmp("09",x_stat,2))
					ptr = "Invalid file type (OPEN)";
				else
					ptr = "OPEN mode/access not supported";
				break;
			case '6':	ptr = "No Current record";			break;
			case '7':	ptr = "Invalid record size (REWRITE)";		break;
			case '8':	ptr = "Index file corrupt";			break;
			case '9':	ptr = "Record locked by another program";	break;
			case 'A':	ptr = "Inadequate memory for operation";	break;
			case 'B':	ptr = "Operation not supported";		break;
			case 'C':	ptr = "Lock table is full";			break;
			case 'D':	ptr = "Host file system error";			break;
			default:	ptr = "UNKNOWN";				break;
			}
		}
		else 
		{
			switch(file_stat[1])
			{
			case '0':	ptr = "FILE incompatable open mode";	break;
			case '1':	ptr = "File Not Open";			break;
			case '2':	ptr = "File already open";		break;
			case '3':	ptr = "File locked";			break;
			case '4':	ptr = "Invalid open";			break;
			case '5':	ptr = "Invalid device";			break;
			case '6':	ptr = "Undefined record pointer";	break;
			case '7':	ptr = "Invalid record lenght";		break;
			case '9':	ptr = "Read on LOCKED record";		break;
			default:	ptr = "UNKNOWN";			break;
			}
		}
		break;
	default:		ptr = "UNKNOWN";			break;
	}

	return(ptr);
}


static char last_filestatus[23] = "00"; /* Wisp file status of last wfilechk	*/
static void wisp_set_last_filecheckstatus(const char status[2]) 
{
	last_filestatus[0] = status[0];
	last_filestatus[1] = status[1];
}
const char *wisp_get_last_filecheckstatus()	{ return last_filestatus; }

static char last_filestatus_extended[11] = "00";
static void wisp_set_last_filecheckstatus_ext(const char *status)
{
	strcpy(last_filestatus_extended, status);
}
const char *wisp_get_last_filecheckstatus_ext()	{ return last_filestatus_extended; }

static struct
{
	char *cob_fileop;
	char *wisp_fileop;
} fileop_table[] = 
{
	{"Close",		"CL"},
	{"Commit",		"CO"},
	{"Delete",		"DE"},
	{"DeleteFile",		"DF"},
	{"Open",		"OP"},
	{"Read",		"RD"},	/* Generic READ for backwards compatibility */
	{"ReadLock",		"RD"},
	{"ReadNextLock",	"RD"},
	{"ReadNextNoLock",	"RD"},
	{"ReadNoLock",		"RD"},
	{"ReadPreviousLock",	"RD"},
	{"ReadPreviousNoLock",	"RD"},
	{"Rewrite",		"RW"},
	{"Rollback",		"RO"},
	{"Start",		"ST"},
	{"StartTransaction",	"SX"},
	{"Unlock",		"UL"},
	{"UnlockAll",		"UA"},
	{"Write",		"WR"},
	{"Sort",		"SO"},  /* Acucobol does not have a code for SORT */
	{ 0, 0}
};

void WL_cobfileop2wispfileop(const char cobfileop[COB_FILEOP_SIZE], char wispfileop[2])
{
	int i;
	char l_cobfileop[COB_FILEOP_SIZE+1];

	WL_cobx2cstr(l_cobfileop, cobfileop, COB_FILEOP_SIZE);

	for(i=0; fileop_table[i].cob_fileop != 0; i++)
	{
		if (0==strcmp(l_cobfileop, fileop_table[i].cob_fileop ))
		{
			memcpy(wispfileop, fileop_table[i].wisp_fileop, 2);
			return;
		}
	}
	wispfileop[0] = '?';
	wispfileop[1] = '?';
}

void WL_wispfileop2cobfileop(const char wispfileop[2], char cobfileop[COB_FILEOP_SIZE])
{
	int i;

	for(i=0; fileop_table[i].cob_fileop != 0; i++)
	{
		if (0==memcmp(wispfileop, fileop_table[i].wisp_fileop, 2 ))
		{
			strcpy(cobfileop, fileop_table[i].cob_fileop);
			return;
		}
	}

	cobfileop[0] = '\0'; /* Unknown - return empty string */
}

/*
**	History:
**	$Log: wfilechk.c,v $
**	Revision 1.37  2003/11/10 17:18:50  gsl
**	Fix WFILECHK3 has declaratives bug and add WC0005 test case
**	
**	Revision 1.36  2003/11/10 17:09:11  gsl
**	Fix WFILECHK3 has declaratives bug and add WC0005 test case
**	
**	Revision 1.35  2003/07/28 20:54:04  gsl
**	fix c++ style comments
**	
**	Revision 1.34  2003/03/17 17:22:43  gsl
**	Change to use  WFILECHK3
**	
**	Revision 1.33  2003/03/07 20:10:56  gsl
**	Standardize param names between the multiple entry points.
**	Fix the displaying of the select_name.
**	Use defines for all the field sizes passed from cobol
**	
**	Revision 1.32  2003/03/07 16:51:50  gsl
**	Fix problems in translating between wispfileop and cobfileop.
**	On error getparms display the cobfileop
**	fix tracing for WFILECHK2
**	
**	Revision 1.31  2003/03/06 21:40:55  gsl
**	CHange WISP_ACU_FILESTATUSCHECK to WISP_FILESTATUSCHECK
**	Fix trace to show ATTR.
**	Fix buff overflow in WL_cobfileop2wispfileop()
**	
**	Revision 1.30  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.29  2003/01/31 19:08:37  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.28  2002/12/10 20:54:08  gsl
**	use WERRCODE()
**	
**	Revision 1.27  2002/09/30 21:02:01  gsl
**	update
**	
**	Revision 1.26  2002/07/31 21:00:28  gsl
**	globals
**	
**	Revision 1.25  2002/07/30 19:12:40  gsl
**	SETRETCODE
**	
**	Revision 1.24  2002/07/29 21:13:26  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.23  2002/07/18 13:19:29  gsl
**	fix mode unit4 -> int4
**	
**	Revision 1.22  2002/07/17 17:50:10  gsl
**	Fix unsigned char warnings
**	
**	Revision 1.21  2002/07/12 20:40:40  gsl
**	Global unique WL_ changes
**	
**	Revision 1.20  2002/07/12 19:10:19  gsl
**	Global unique WL_ changes
**	
**	Revision 1.19  2002/07/12 17:01:02  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.18  2002/07/11 20:29:17  gsl
**	Fix WL_ globals
**	
**	Revision 1.17  2002/07/10 21:05:31  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.16  2002/07/02 04:07:23  gsl
**	Add WISP_ACU_FILESTATUSCHECK
**	
**	Revision 1.15  2002/07/01 04:02:42  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.14  2002/06/21 03:10:44  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.13  1998/08/03 21:18:28  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**	
**	Revision 1.12  1997-10-21 10:18:06-04  gsl
**	removed WISPPROGID
**
**	Revision 1.11  1997-04-29 13:42:05-04  gsl
**	Renamed wfilechk() to WFILECHK2() and added support for long file
**	status codes from acucobol. Wrote wfilechk() as a frontend to WFILECHK2()
**	which changes to use long file status codes.
**	Documented the routines.
**
**	Revision 1.10  1996-08-19 18:33:14-04  gsl
**	drcs update
**
**
**
*/
