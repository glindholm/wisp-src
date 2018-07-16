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
iocode.h
Two character io codes passed from COBOL
------*/
#define OPEN_SHARED         "OS"
#define OPEN_OUTPUT         "OO"
#define OPEN_INPUT          "OI"
#define OPEN_IO             "OX"
#define CLOSE_FILE          "CL"
#define READ_RECORD         "RD"
#define READ_NEXT_RECORD    "RN"
#define READ_PREVIOUS_RECORD	"RP"
#define HOLD_RECORD         "HL"
#define HOLD_NEXT_RECORD    "HN"
#define WRITE_RECORD        "WR"
#define REWRITE_RECORD      "RW"
#define DELETE_RECORD       "DL"
#define START_EQ            "SE"
#define START_NLT           "SN"
#define START_GT            "SG"
#define	START_LAST	    "SL"
#define TEST_FOR_FILE       "TF"
#define FILE_INFO           "FI"
#define FILE_SET            "FS"
#define READ_KEYED          "RK"
#define HOLD_KEYED          "HK"
#define START_EQ_KEYED      "KE"
#define START_NLT_KEYED     "KN"
#define START_GT_KEYED      "KG"
#define	TABLE_INFO	    "TI"
/*
**	History:
**	$Log: iocode.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:10  gsl
**	drcs update
**	
**
**
*/
