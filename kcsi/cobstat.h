/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

/*----
cobstat.h
Cobol file-statuses
------*/
#define I_O_OK                   "00"
#define RECORD_ON_FILE           "00"
#define DUP_RECORD_OK            "02"
#define AT_END                   "10"
#define INVALID_KEY              "21"
#define DUPLICATE_KEY            "22"
#define RECORD_NOT_FOUND         "23"
#define BOUNDARY_ERROR           "24"
#define HARDWARE_ERROR           "30"
#define BOUNDARY_SEQUENCE_ERROR  "34"
#define RECORD_LOCKED            "70"
#define INVALID_FUNCTION         "95"
#define FILE_NOT_FOUND           "96"
#define ACCESS_DENIED            "85"
#define RECORD_NOT_AVAILABLE     "AO"
/*
**	History:
**	$Log: cobstat.h,v $
**	Revision 1.4  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.3  1996/09/17 23:34:02  gsl
**	drcs update
**	
**
**
*/
