/* 
	Copyright (c) 1995-1997 NeoMedia Migrations, All rights reserved.
	$Id:$
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

#ifdef unix
#define WRUNOPTIONS_ENV "WRUNOPTIONS"
#define WRUNCONFIG	"wrunconfig"
#endif /* unix */

#ifdef MSFS
#define WRUNOPTIONS_ENV "WRUNOPTS"
#define WRUNCONFIG	"wrun.cfg"
#endif /* MSDOS */

struct wruncfg
{
	char	wrun_options[512];
	char	wrun_runcbl[256];
	char	wrun_cobtype[40];
};

int wrunconfig(struct wruncfg *cfg);

#endif /* WRUNCONF_H */

/*
**	History:
**	$Log: wrunconf.h,v $
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
