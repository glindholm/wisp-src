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

#endif /* LINK_H */



