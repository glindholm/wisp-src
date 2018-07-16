/* 
	Copyright (c) 1995-1997 NeoMeida Migrations, All rights reserved.
	$Id:$
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

#define		LICENSE_KEY_SIZE	16
#define		VALIDATION_CODE_SIZE	3

#define		LICENSE_OK		1000
#define		LICENSE_MISSING		1001
#define		LICENSE_TIMEDOUT	1002
#define		LICENSE_INVALID		1003
#define		LICENSE_UNKNOWN		1004

const char *product_name(void);
const char *license_filepath(void);
const char *lic_trantable(void);
const char *authlogfile(void);

/* wauthsub.c */
char entran(int innum);
int detran(char inchar);
int checksummem(char* instr,int size,int mod);
int packdate(char* yyyymmdd, char* ymd);
int unpkdate(char* yyyymmdd,char* ymd);
char *lictypename(int lictype);
int validate_license(void);
int check_timeout(char lowdate[8], char highdate[8]);
int get_license_info(char* flickey, char* valcode);

/* wlickey.c */
#define WLIC_MAXTRAN		34
int	bklickey(
		int4	*custnum,
		char	*platform,
		int	*lictype,
		char	licdate[8],
		char	expdate[8],
		char	licensekey[LICENSE_KEY_SIZE]);
void formatkey(const char* lickey, char* formkey);
void unformatkey(char* lickey, const char* formkey);
int ckvalcode(char lickey[LICENSE_KEY_SIZE],char* machineid,char valcode[3]);

#endif /* WLICENSE_H */
/*
**	History:
**	$Log: wlicense.h,v $
**	Revision 1.13.2.4  2003/02/14 18:20:24  gsl
**	make const prototypes for license routines
**	
**	Revision 1.13.2.3  2003/01/03 16:41:38  gsl
**	move routines to write licenses to wlicense.c
**	
**	Revision 1.13.2.2  2003/01/03 15:09:02  gsl
**	Move the license gen stuff out of runtime into wauthorize.c
**	
**	Revision 1.13.2.1  2002/11/14 18:03:52  gsl
**	expose get_license_info() for use in wlicense
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
