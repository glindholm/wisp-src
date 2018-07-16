/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*----
Error logging for ccsio
------*/

#include <stdio.h>
#include <ctype.h>
#include "iocode.h"
#include "kcsio.h"
#include "shrthand.h"
#include "kcsifunc.h"

const char* WL_strerror(int errnum);

static char *err_lit[]={
	"Duplicate key",			/*100*/
	"File not open",			/*101*/
	"Bad Arg(s)",				/*102*/
	"Inv. key values",			/*103*/
	"Too many files",			/*104*/
	"File corrupted",			/*105*/
	"No excl. access",			/*106*/
	"Record Locked",			/*107*/
	"Key value set",			/*108*/
	"Inv. on Prim. Key",			/*109*/
	"EOF or BOF",				/*110*/
	"Record Not Found",			/*111*/
	"No Current ptr",			/*112*/
	"Locked/in use",			/*113*/
	"Name too long",			/*114*/
	"Can't cr Lck file",			/*115*/
	"Can't malloc mem."			/*116*/
	};

static char *io_list[]={
	 OPEN_SHARED,
	 OPEN_OUTPUT,
	 OPEN_INPUT,
	 OPEN_IO,
	 CLOSE_FILE,
	 READ_RECORD,
	 READ_NEXT_RECORD,
	 HOLD_RECORD,
	 HOLD_NEXT_RECORD,
	 WRITE_RECORD,
	 REWRITE_RECORD,
	 DELETE_RECORD,
	 START_EQ,
	 START_NLT,
	 START_GT,
	 TEST_FOR_FILE,
	 FILE_INFO,
	 FILE_SET,
	 READ_KEYED,
	 HOLD_KEYED,
	 START_EQ_KEYED,
	 START_NLT_KEYED,
	 START_GT_KEYED,
	 TABLE_INFO,
	""
	};
static char *io_lit[]={
	 "OPEN_SHR",
	 "OPEN_OUT",
	 "OPEN_IN",
	 "OPEN_IO",
	 "CLOSE",
	 "READ",
	 "READ_NXT",
	 "HOLD",
	 "HOLD_NXT",
	 "WRITE",
	 "REWRITE",
	 "DELETE",
	 "STRT_EQ",
	 "STRT_NLT",
	 "STRT_GT",
	 "TEST_FOR",
	 "FILE_INF",
	 "FILE_SET",
	 "READ_KEY",
	 "HOLD_KEY",
	 "EQ_KEY",
	 "NLT_KEY",
	 "GT_KEY",
	 "TBL_INF",
	""
	};

static int ccsioerr(int status,char *io,char *name);
#ifdef KCSI_MFX
static void add_mf_error(KFB *kfb);
#endif /* KCSI_MFX */

/*----
Some statuses are legal such as at end on a read next or a start
duplicate key on a write etc.
------*/
static int ccsioerr(int status,char *io,char *name)
{

	char *sys_md(),*sys_hms();
	const char *err_msg;
	const char *io_msg;
	int io_idx;

	if((Streq(io,READ_RECORD)) && (status == ENOREC))
		return(0);
	if((Streq(io,HOLD_RECORD)) && (status == ENOREC))
		return(0);
	if((Streq(io,WRITE_RECORD)) && (status == EDUPL))
		return(0);
	if((Streq(io,REWRITE_RECORD)) && (status == EDUPL))
		return(0);
	if((Streq(io,READ_PREVIOUS_RECORD)) && (status == EENDFILE))
		return(0);
	if((io[1] == 'N') && (status == EENDFILE))	/*read and hold next*/
		return(0);
	if((io[1] == 'N') && (status == ENOREC))	/*read and hold next*/
		return(0);
	if((io[1] == 'K') && (status == ENOREC))	/*keyed read or hold*/
		return(0);
	if((io[0] == 'S') && (status == EENDFILE))	/*starts*/
		return(0);
	if((io[0] == 'S') && (status == ENOREC))	/*starts*/
		return(0);
	if((io[0] == 'K') && (status == EENDFILE))	/*keyed starts*/
		return(0);
	if((io[1] == 'E') && (status == ENOREC))	/* start equal */
		return(0);
	if((Streq(io,CLOSE_FILE)) && (status == ENOTOPEN))
		return(0);

/*Otherwise we're gonna log it*/

/* A literal for the io */
	for(io_idx = 0; io_list[io_idx][0] ; ++io_idx)
		{
		if(Streq(io,io_list[io_idx]))
			break;
		}
	io_msg = io_lit[io_idx];
/* A literal for the error */
	if(status >= 100 && status <= 116)
	{
		err_msg = err_lit[status - 100];	/* KSCI errors */
	}
	else if(status >= 0)
	{
		err_msg = WL_strerror(status);		/* System errors */
	}
	else
	{
		err_msg = "Unknown";
	}

/* And append it to the log */
	
	kcsitrace(4, "IOERR", io_msg, "%03d-%-17s %s", status, err_msg, name);
	return(1);
}

void KCSI_kfberr(KFB *kfb)
{
	if(ccsioerr(kfb->_status, kfb->_io, kfb->_sys_name))
	{
#ifdef KCSI_MFX
		add_mf_error(kfb);
#endif /* KCSI_MFX */
	}
}

#ifdef KCSI_MFX

typedef struct {
	char *ecode;
	char *emsg;
	} MFERR;

static MFERR mf[]={
	{"10","No next logical record exists"},
	{"21","Sequence error"},
	{"22","Duplicate key"},
	{"23","No record found"},
	{"24","Boundary violation"},
	{"30","Boundary Violation"},
	{"34","Boundary Violation"},
	{"35","File does not exist"},
	{"37","Open Mode not supported for this file type"},
	{"38","File closed with LOCK"},
	{"39","File attribute conflict"},
	{"41","File already open"},
	{"42","File already closed"},
	{"43","DELETE/REWRITE without READ"},
	{"44","Boundary violation (record length error)"},
	{"46","READ with no valid next record"},
	{"47","Not open or wrong open mode for READ or START"},
	{"48","Not open or wrong open mode for WRITE"},
	{"49","Not open or wrong open mode for REWRITE/DELETE"},

	{"9/001","Out of buffer space"},
	{"9/002","File not open"},
	{"9/013","File not found"},
	{"9/014","Too many files open"},
	{"9/031","Not owner of file"},
	{"9/035","Incorrect file permission"},
	{"9/037","File access denied"},
	{"9/041","Corrupt index file"},
	{"9/043","File information missing"},
	{"9/065","File locked"},
	{"9/066","Duplicate record key"},
	{"9/067","Index file not open"},
	{"9/068","Record locked"},
	{"9/078","Illegal key description"},
	
	{NULL,NULL}};

/*----
Additional error logic for Microfocus COBOL with EXTFH
------*/
static void add_mf_error(KFB *kfb)
{
	char *msg;
	int idx;

	msg = "Consult Manual";

	for(idx=0;mf[idx].ecode;++idx)
		{
		if(!(strcmp(mf[idx].ecode,kfb->_x_status)))
			{
			msg = mf[idx].emsg;
			break;
			}
		}

	kcsitrace(4, "MICROFOCUS", "IOERR", "%-6s-%-50s", kfb->_x_status, msg);
}

#endif /* KCSI_MFX */

/*
**	History:
**	$Log: ccsioerr.c,v $
**	Revision 1.17  2010/01/10 00:58:33  gsl
**	fix LINUX warnings
**	trunc
**	isblank
**	sys_errlist
**	
**	Revision 1.16  2003/03/20 15:23:21  gsl
**	FIx warning
**	
**	Revision 1.15  2003/03/20 15:01:05  gsl
**	Fix -Wall warnings
**	
**	Revision 1.14  2003/02/13 15:30:12  gsl
**	Only add MF error if KCSI_MFX defined
**	
**	Revision 1.13  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.12  2002/10/24 14:20:41  gsl
**	Make globals unique
**	
**	Revision 1.11  2002/10/17 17:17:15  gsl
**	Removed VAX VMS code
**	
**	Revision 1.10  2002/10/17 16:35:17  gsl
**	comments
**	
**	Revision 1.9  2002/09/04 18:09:49  gsl
**	LINUX
**	
**	Revision 1.8  2002/04/23 19:56:25  gsl
**	9/067
**	
**	Revision 1.7  2002-04-23 13:30:15-04  gsl
**	Add MF error status text
**
**	Revision 1.6  2002-04-22 11:54:43-04  gsl
**	Replace crid_err_trace with kcsitrace
**
**	Revision 1.5  1997-08-12 14:10:50-04  scass
**	Removed return for MFX, is working properly now.
**
**	Revision 1.4  1996-09-17 19:34:01-04  gsl
**	drcs update
**
**
**
*/
