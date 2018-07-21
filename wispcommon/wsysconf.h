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
**	RECFMT  defines the wsysconfig record format. If it changes must also change following defines.
*/
#define RECFMT "%03d %2s %-15s %s\n"
#define WCLASS 		4		/* Based on RECFMT */
#define WTYPE 		7		/* Based on RECFMT */
#define WSPECFIL 	23		/* Based on RECFMT */

/*
**	CFGNAME  defines the name of the config file.
*/
#ifdef WIN32
#define CFGFNAME "WSYSCONF.CFG"
#else	/* unix */
#define CFGFNAME "wsysconfig"
#endif

/*
**	History:
**	$Log: wsysconf.h,v $
**	Revision 1.11  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.10  2002/07/25 17:03:41  gsl
**	MSFS->WIN32
**	
**	Revision 1.9  1996/07/23 18:18:01  gsl
**	drcs update
**	
**
**
*/
