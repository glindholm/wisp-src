/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		wlicense.h
**
**	Function:	Provide defines for the license routines.
**
*/

#ifndef WLICENSE_H
#define WLICENSE_H

#include "intdef.h"

#define		LICENSE_SINGLE		1
#define		LICENSE_UNLIMITED	2
#define		LICENSE_TIMED		3
#define		LICENSE_CLUSTER		4
#define		LICENSE_NETWORK		5
#define		LICENSE_ENTERPRISE	6

#define		LICENSE_KEY_SIZE	16
#define		VALIDATION_CODE_SIZE	3

#define		LICENSE_CHECK_OK	1000
#define		LICENSE_CHECK_MISSING	1001
#define		LICENSE_CHECK_TIMEDOUT	1002
#define		LICENSE_CHECK_INVALID	1003
#define		LICENSE_CHECK_UNKNOWN	1004
#define		LICENSE_CHECK_VERSION	1005
#define		LICENSE_CHECK_UNLIMITED	1006

const char *WLIC_product_name(void);
const char *WLIC_license_filepath(void);
const char *WLIC_lic_trantable(void);

/* wauthsub.c */
char WLIC_entran(int innum);
int  WLIC_detran(char inchar);
int  WLIC_checksummem(char* instr,int size,int mod);
int  WLIC_packdate(char* yyyymmdd, char* ymd);
int  WLIC_unpkdate(char* yyyymmdd,char* ymd);
char *WLIC_lictypename(int lictype);
int WLIC_validate_license(void);
int WLIC_check_timeout(char lowdate[8], char highdate[8]);
int WLIC_get_license_info(char* flickey, char* valcode, char* appcode);

/* wlickey.c */
#define WLIC_MAXTRAN		34

int	WLIC_bklickey(
		int4	*custnum,
		char	*platform,
		int	*lictype,
		char	licdate[8],
		char	expdate[8],
		int4	*version_number,
		char	licensekey[LICENSE_KEY_SIZE]);
void WLIC_formatkey(const char* lickey, char* formkey);
void WLIC_unformatkey(char* lickey, const char* formkey);
int  WLIC_ckvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3]);

#endif /* WLICENSE_H */
/*
**	History:
**	$Log: wlicense.h,v $
**	Revision 1.22  2003/06/13 17:36:12  gsl
**	ENTERPRISE License
**	
**	Revision 1.21  2003/06/12 20:54:29  gsl
**	Add support for ENTERPRISE licenses with a version number and remove
**	support for UNLIMITED license.
**	
**	Revision 1.20  2003/02/13 20:45:03  gsl
**	On unix change the license file from /lib/wisp.license to
**	$WISPCONFIG/wisp.{machineid}.license
**	
**	Revision 1.19  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.18  2003/01/08 17:30:44  gsl
**	Move WLIC_authlogfile to wauthorize.c
**	
**	Revision 1.17  2003/01/03 16:48:53  gsl
**	Move routines to write license file to wlicense.c
**	
**	Revision 1.16  2002/12/31 16:25:43  gsl
**	Move the license key generation stuff to wauthoize.c
**	
**	Revision 1.15  2002/10/14 15:41:39  gsl
**	Expose WLIC_get_license_info()
**	
**	Revision 1.14  2002/07/10 21:06:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  1998/12/18 18:28:41  gsl
**	fix templates
**	
**	Revision 1.12  1997-03-17 08:21:20-05  gsl
**	Change SERVER to NETWORK
**
**	Revision 1.11  1997-03-06 16:40:39-05  gsl
**	move machid.c prototypes to machid.h
**
**	Revision 1.10  1997-03-06 15:11:11-05  gsl
**	update
**
**	Revision 1.9  1996-07-23 14:18:00-04  gsl
**	drcs update
**
**	05/19/92	Written GSL
**	07/31/92	Added WISP_LICENSE_FILE_X the temp inode file. GSL
**	09/25/92	Added LICENSE_CLUSTER. GSL
**	09/13/93	Remove path defines. Generalize for UniQue. GSL
**
**
*/
