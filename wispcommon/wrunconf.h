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
**	File:		wrunconf.h
**
**	Purpose:	To hold the structs and defines for wrunconfig processing.
**
**
*/

#ifndef WRUNCONF_H
#define WRUNCONF_H

#define WRUNCOBTYPE_ACU	"ACU"
#define WRUNCOBTYPE_MF	"MF"

#define WRUNOPTIONS_ENV "WRUNOPTIONS"

#ifdef unix
#define WRUNCONFIG	"wrunconfig"
#define WRUNCOBTYPE_DEF	"MF"
#endif /* unix */

#ifdef WIN32
#define WRUNCONFIG	"wrun.cfg"
#define WRUNCOBTYPE_DEF	"ACU"
#endif /* WIN32 */

struct wruncfg
{
	char	wrun_options[512];
	char	wrun_runcbl[256];
	char	wrun_cobtype[40];
};

int WL_wrunconfig(struct wruncfg *cfg);

#endif /* WRUNCONF_H */

/*
**	History:
**	$Log: wrunconf.h,v $
**	Revision 1.18  2003/07/09 20:08:34  gsl
**	for 5.0 both Unix and WIN32 use WRUNOPTIONS envvar
**	
**	Revision 1.17  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/10/11 20:39:52  gsl
**	Detect runtime Cobol type without needing INITWISP call.
**	For ACU set in sub85.c,
**	For utils set via WRUNCONFIG
**	Default to MF on UNIX
**	
**	Revision 1.15  2002/07/25 17:03:41  gsl
**	MSFS->WIN32
**	
**	Revision 1.14  2002/07/18 21:04:23  gsl
**	Remove MSDOS code
**	
**	Revision 1.13  2002/07/10 21:06:36  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  1997/02/20 20:44:01  gsl
**	Increase the sizes for run options and rts path name
**	
**	Revision 1.11  1996-10-08 20:32:18-04  gsl
**	move prototypes to wispcfg.h
**
**	Revision 1.10  1996-07-23 11:18:01-07  gsl
**	drcs update
**
**
**
*/
