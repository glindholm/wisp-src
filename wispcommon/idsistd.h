/*
** $Id:$
** WISP - Wang Interchange Source Processor
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
*/


/*
**	File:		idsistd.h
**
**	Purpose:	This header can and should be included in all IDSI C files.
**			It includes the INT typedefs plus standard headers.
**
**			Only add things to this that a truely GLOBAL to absolutely
**			everything.
**
**
*/

#ifndef IDSISTD_H
#define IDSISTD_H

#include "intdef.h"

#ifndef NOSTDLIB
#include <stdlib.h>
#endif

#ifdef NOSIZE_T
typedef unsigned int size_t;
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

#endif /* IDSISTD_H */
/*
**	History:
**	$Log: idsistd.h,v $
**	Revision 1.9  2009/10/18 20:24:56  gsl
**	Remove obsolete win32std.h
**	
**	Revision 1.8  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.7  1996/09/10 15:43:49  gsl
**	Add include of wisp32std.h in ifdef WIN32 code
**	
**	Revision 1.6  1996-07-23 11:17:47-07  gsl
**	drcs update
**
**
**
*/
