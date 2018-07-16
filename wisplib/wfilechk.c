static char copyright[]="Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
**	wfilechk2()
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

#define ROUTINE	77000

#ifdef VMS
#include <descrip.h>
static char msgtxt[260];
static $DESCRIPTOR(msgdsc,msgtxt);
#endif

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/


extern int werrno;									/* Wisp errno field.			*/
extern uint4 wextstat1;									/* Global place to put extended status.	*/
extern uint4 wextstat2;
extern char wfilestat[2];								/* Global file status			*/
extern char WISPRUNNAME[8];
extern int4 LINKCOMPCODE;
extern int4 LINKRETCODE;

void wfilechk2(char   decl_stat[2], 
	       char   file_stat[2],
	       char   x_stat1[10],
	       char   x_stat2[10],
	       uint4* sflag,
	       char   vol[6],
	       char   lib[8],
	       char   fil[8],
	       char   fname[80],
	       char   f_id[40],
	       char   appl[8]);

/*
**	Static data
*/
static char acufilestat[11] = "0000";

/*
**	Static Function Prototypes
*/

static char *msg_filestat(char* file_stat, char* x_stat);



/*
**	ROUTINE:	wfilechk()
**
**	FUNCTION:	A front-end to wfilechk2()
**
**	DESCRIPTION:	Convert the older format for the extended file statuses
**			then call wfilechk2().
**
**	ARGUMENTS:	See wfilechk2()
**
**	GLOBALS:	See wfilechk2()
**
**	RETURN:		See wfilechk2()
**
**	WARNINGS:	See wfilechk2()
**
*/
void wfilechk( char   decl_stat[2], 
	       char   file_stat[2],
	       char*  x_stat1,
	       char*  x_stat2,
	       uint4* sflag,
	       char   vol[6],
	       char   lib[8],
	       char   fil[8],
	       char   fname[80],
	       char   f_id[40],
	       char   appl[8])
{
	char	x_stat_x1[10], x_stat_x2[10];
	
	memset(x_stat_x1, ' ', sizeof(x_stat_x1));
	memset(x_stat_x2, ' ', sizeof(x_stat_x2));
	
	if (acu_cobol)
	{
		memcpy(x_stat_x1, x_stat1, 4);	/* PIC X(4) */
	}
	else if (vax_cobol)
	{
		memcpy(x_stat_x1, x_stat1, 10); /* PIC S9(9) LEADING SEPARATE */
		memcpy(x_stat_x2, x_stat2, 10); /* PIC S9(9) LEADING SEPARATE */
	}

	wfilechk2(decl_stat,file_stat,x_stat_x1,x_stat_x2,sflag,vol,lib,fil,fname,f_id,appl);
	
}

/*
**	ROUTINE:	wfilechk2()
**
**	FUNCTION:	Check file status codes as part of declaratives handling.
**
**	DESCRIPTION:	?
**
**	ARGUMENTS:	
**	decl_stat	Coded I/O operation
**				OP	OPEN
**				RD	READ
**				RW	REWRITE
**				DE	DELETE
**				ST	START
**				WR	WRITE
**				CL	CLOSE
**				SO	SORT
**	file_stat	The file status code 
**	x_stat1		The extended file status code(s)
**	x_stat2		The extended file status code(s)
**	sflag		The S-filename status flag
**	vol		The volume name
**	lib		The library name
**	fil		The file name
**	fname		The native filepath
**	f_id		The SELECT name
**	appl		The application name
**
**	GLOBALS:	?
**
**	RETURN:		?
**
**	WARNINGS:	?
**
*/
void wfilechk2(char   decl_stat[2], 
	       char   file_stat[2],
	       char   x_stat1[10],
	       char   x_stat2[10],
	       uint4* sflag,
	       char   vol[6],
	       char   lib[8],
	       char   fil[8],
	       char   fname[COB_FILEPATH_LEN],
	       char   f_id[40],
	       char   appl[8])
{
	char tstr1[COB_FILEPATH_LEN + 40];
	short	wang_filestatus, open_error, reportit, force_error;
	int	temp_errno;
	char	msg1[80],msg2[80],msg3[80],msg4[80],msg5[80],msg6[80],msg7[80],msg8[80];
#ifdef VMS
	unsigned char info[4];
	uint4 status;
	int i;
	short msglen;
#endif
	setprogid(appl);								/* Set the PROGID global variable.	*/
	temp_errno = errno;

	wtrace("WFILECHK2", "ENTRY", "%2.2s %8.8s (%8.8s in %8.8s on %6.6s) in %8.8s Status=%2.2s Xstat=%10.10s",
	       decl_stat, f_id, fil, lib, vol, appl, file_stat, x_stat1);

	wfilestat[0] = file_stat[0];							/* And store it away for 'C'.		*/
	wfilestat[1] = file_stat[1];
	if ( acu_cobol )
	{
		/* Save the acucobol extended filestat as a string */
		char *ptr;
		
		memcpy(acufilestat, x_stat1, 10);
		acufilestat[10] = '\0';
		if (ptr = strchr(acufilestat,' '))
		{
			*ptr = '\0';
		}
		/* Minimum 4 chars, pad with spaces */
		if (strlen(acufilestat) < 4)
		{
			strcat(acufilestat,"    ");
			acufilestat[4] = '\0';
		}
		
	}
	wang_filestatus = TRUE;								/* Assume a WANG file status.		*/
	reportit = TRUE;								/* Always report non-wang errors.	*/
	force_error = FALSE;								/* Don't force				*/

	if (0==memcmp("OP",decl_stat,2)) open_error = TRUE;				/* This is an OPEN error.		*/
	else		 		 open_error = FALSE;				/* Not true.				*/

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
		if (vax_cobol)
		{
			switch(file_stat[1])
			{
			case '1':							/* "91" OPEN File locked		*/
				wang_filestatus = FALSE;
				break;
			case '5':							/* "95" OPEN No space on device.	*/
			case '4':							/* "94" UNLOCK incompatible open	*/
			case '8':							/* "98" CLOSE error			*/
				force_error = TRUE;					/* OPEN cannot recover.			*/
				wang_filestatus = FALSE;
				break;
			case '0':							/* "90" Record locked (Soft)		*/
			case '3':							/* "93" UNLOCK no record		*/
			case '2':							/* "92" Record locked (Hard)		*/
				wang_filestatus = FALSE;
				reportit = FALSE;					/* Don't report these.			*/
				break;
			default:
				wang_filestatus = FALSE;
				break;
			}
		}
 		else if (acu_cobol || lpi_cobol)
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
 		else if (aix_cobol || mf_cobol)
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

	if (file_stat[0] == hardlock[0] &&
	    file_stat[1] == hardlock[1]   )						/* If a record lock on a ...		*/
	{
		if (0==memcmp("RD",decl_stat,2))					/* READ					*/
		{
			wang_filestatus = FALSE;					/*	return to COBOL			*/
			reportit = FALSE;
		}
		else if (0==memcmp("RW",decl_stat,2) ||					/* REWRITE				*/
			 0==memcmp("DE",decl_stat,2)   )				/* DELETE				*/
		{
			wang_filestatus = FALSE;
			reportit = TRUE;						/* 	do declaratives			*/
		}
	}

#ifdef VMS

	wextstat1 = 0;									/* Get extended file status 1.		*/

	for (i=1; i<10; i++)								/* Copy 9 digits.			*/
	{
		wextstat1 = wextstat1 * 10;						/* Shift left.				*/
		wextstat1 = wextstat1 + (x_stat1[i] - '0');				/* Get next digit.			*/
	}

	wextstat2 = 0;									/* Get extended file status 2.		*/

	for (i=1; i<10; i++)								/* Copy 9 digits.			*/
	{
		wextstat2 = wextstat2 * 10;						/* Shift left.				*/
		wextstat2 = wextstat2 + (x_stat2[i] - '0');				/* Get next digit.			*/
	}
#else
	wextstat1 = 0;
	wextstat2 = 0;
#endif

	if (opt_allstatuskeys)							/* If set then we want to pass all status keys	*/
	{									/* thru to the user define declaratives, so we	*/
		wang_filestatus = TRUE;						/* always say it is a Wang filestatus code.	*/
	}
											/* Here we report the error directly to	*/
											/* the user, because there is no way the*/
											/* Wang program could handle this.	*/

	if (  open_error && (*sflag & IS_NORESPECIFY) ) force_error = FALSE;		/* Open, NORESPECIFY == don't error	*/
	if ( !open_error && reportit && !wang_filestatus ) force_error = TRUE;		/* Non-Open, non-Wang  == do error	*/
	if ( !open_error && wang_filestatus && !(*sflag & IS_DECLARE)) force_error=TRUE;/* Non-Open, no declaratives == do error*/

	if ( force_error ) 
	{
		char	verb[10];

		if      (0==memcmp("DE",decl_stat,2))	strcpy(verb,"DELETE");
		else if (0==memcmp("OP",decl_stat,2))	strcpy(verb,"OPEN");
		else if (0==memcmp("RD",decl_stat,2))	strcpy(verb,"READ");
		else if (0==memcmp("RW",decl_stat,2))	strcpy(verb,"REWRITE");
		else if (0==memcmp("ST",decl_stat,2))	strcpy(verb,"START");
		else if (0==memcmp("WR",decl_stat,2))	strcpy(verb,"WRITE");
		else if (0==memcmp("CL",decl_stat,2))	strcpy(verb,"CLOSE");
		else if (0==memcmp("SO",decl_stat,2))	strcpy(verb,"SORT");
		else 					strcpy(verb,"");

		msg1[0] = '\0';
		msg2[0] = '\0';
		msg3[0] = '\0';
		msg4[0] = '\0';
		msg5[0] = '\0';
		msg6[0] = '\0';
		msg7[0] = '\0';
		msg8[0] = '\0';


		cobx2cstr(tstr1,fname,COB_FILEPATH_LEN);

		sprintf(msg7,"File [%8.8s] Lib [%8.8s] Vol [%6.6s]",fil,lib,vol);
		sprintf(msg8,"File [%s]",tstr1);					/* Display native file name in message.	*/

#ifdef VMS
		if (wextstat1)
		{
			sys$getmsg(wextstat1,&msglen,&msgdsc,1,info);			/* Get the system message.		*/
			msgtxt[msglen] = '\0';
			strcpy(msg5,msgtxt);
		}

		if (wextstat2)
		{
			sys$getmsg(wextstat2,&msglen,&msgdsc,1,info);			/* Get the system message.		*/
			msgtxt[msglen] = '\0';
			strcpy(msg6,msgtxt);
		}
#endif

		unloadpad(tstr1,f_id,30);

		sprintf( msg1, "RUN PROGRAM = %8.8s SUB-PROGRAM = %8.8s FILE = %1.25s", WISPRUNNAME, appl,tstr1); 
		sprintf( msg2, "ERROR DETECTED AND USER ERROR EXIT NOT IN USE");
		if ( acu_cobol )
		{
			sprintf( msg3, "FILE STATUS = %c%c [%s]   (%s)", 
				file_stat[0], file_stat[1], &acufilestat[2], verb);
			strcpy(  msg4, msg_filestat(file_stat,&acufilestat[2]) );
		}
		else if (mf_cobol || aix_cobol)
		{
			if ('9' != file_stat[0])
				sprintf( msg3, "FILE STATUS = %c%c   (%s)", file_stat[0], file_stat[1], verb);
			else
				sprintf( msg3, "FILE STATUS = %c/RT%03d   (%s)", file_stat[0], (unsigned)file_stat[1], verb);
			strcpy(  msg4, msg_filestat(file_stat,"00") );
		}
		else
		{
			sprintf( msg3, "FILE STATUS = %c%c   (%s)", file_stat[0], file_stat[1], verb);
			strcpy(  msg4, msg_filestat(file_stat,"00") );
		}


		err_getparm("FILESTAT","0001", "ERROR ",msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8 );
		LINKCOMPCODE = 16;
		setretcode("016");
		wexit(16L);								/* Blow program away !!			*/
	}
	else										/* Don't report it 			*/
	{
											/* Here we decide if DECLARATIVES should*/
											/* Be executed.				*/
		if (open_error)								/* If it's an open error...		*/
		{
			if (*sflag & IS_NORESPECIFY)					/* And NORESPECIFY, execute DECLARATIVES*/
			{
				memcpy(decl_stat,file_stat,2);				/* Signal with file status.		*/
			}
			else								/* RESPECIFY is allowed, Don't do decl.	*/
			{
				memcpy(decl_stat,"00",2);				/* Clear WISP-DECLARATIVES-STATUS field.*/
				*sflag = *sflag | IS_ERROR;				/* Set the error bit for wfopen().	*/
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
	}

#ifdef VMS
	if ( file_stat[0] == '9' &&
	     file_stat[1] == '0'    ) file_stat[0] = '0';				/* Clear soft-lock 90 --> 00		*/
#endif

}

static char *msg_filestat(char* file_stat, char* x_stat)
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
			if (acu_cobol && 0==memcmp("00",x_stat,2))
				ptr = "Disk full (INVALID KEY)";
			else if (acu_cobol && 0==memcmp("01",x_stat,2))
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
			if (acu_cobol && 0==memcmp("07",x_stat,2))
				ptr = "Access denied (OPEN)";
			else if (acu_cobol && 0==memcmp("09",x_stat,2))
				ptr = "Invalid file type (OPEN)";
			else if (vax_cobol)
				ptr = "Inappropriate device type (OPEN)"; 
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
		if (vax_cobol)
		{
			switch(file_stat[1])
			{
			case '0':	ptr = "Record locked by another program";	break;
			case '1':	ptr = "File locked by another program (OPEN)";	break;
			case '2':	ptr = "Record locked by another program";	break;
			case '3':	ptr = "No current record (UNLOCK)";		break;
			case '4':	ptr = "File not open, or incompatible mode"; 	break;
			case '5':	ptr = "OPEN No space on device";		break;
			case '7':	ptr = "OPEN File not found";			break;
			case '8':	ptr = "CLOSE Error";				break;
			default:	ptr = "UNKNOWN";				break;
			}
		}
		else if (aix_cobol || mf_cobol)
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
		else if (acu_cobol)
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

void lastacufilestat(char *buff)
{
	strcpy(buff, acufilestat);
}


/*
**	History:
**	$Log: wfilechk.c,v $
**	Revision 1.13  1998-08-03 17:18:28-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.12  1997-10-21 10:18:06-04  gsl
**	removed WISPPROGID
**
**	Revision 1.11  1997-04-29 13:42:05-04  gsl
**	Renamed wfilechk() to wfilechk2() and added support for long file
**	status codes from acucobol. Wrote wfilechk() as a frontend to wfilechk2()
**	which changes to use long file status codes.
**	Documented the routines.
**
**	Revision 1.10  1996-08-19 18:33:14-04  gsl
**	drcs update
**
**
**
*/
