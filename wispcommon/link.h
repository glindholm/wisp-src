/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		link.h
**
**	Purpose:	To hold defines and structs used in linking.
**
**
**	History:	
**	12/22/92	Written by GSL
**
*/

#ifndef LINK_H
#define LINK_H

#include "idsistd.h"

#define 	MAX_LINK_PARMS		32						/* Maximum number of parameters to LINK	*/
#define 	NULL_FILE_NAME		"*NULL*"					/* No LINK arg file, See VMSLINKSUB.WCB	*/

struct	str_parm	{ char *parm[MAX_LINK_PARMS]; };			/* Structure for *parm_list function argument.	*/
struct	str_len		{ int4   len[MAX_LINK_PARMS]; };			/* Structure for  *len_list function argument.	*/

void writeunixlink(const char *pname, int parmcnt, struct str_parm *parm_list, struct str_len *len_list, char *linkkey);
void readunixlink(int parmcnt, struct str_parm *parm_list, struct str_len *len_list, const char *linkkey, 
		  int4 *compcode, int4 *returncode);

#endif /* LINK_H */



/*
**	History:
**	$Log: link.h,v $
**	Revision 1.8  1996-09-04 20:20:22-04  gsl
**	moved softlink prototypes to wperson.h
**
**	Revision 1.7  1996-09-03 14:42:25-07  gsl
**	Add prototypes for USESOFTLINK() and USEHARDLINK()
**
**	Revision 1.6  1996-08-28 17:54:50-07  gsl
**	Add prototypes
**
**	Revision 1.5  1996-07-23 11:17:48-07  gsl
**	drcs update
**
**
**
*/
