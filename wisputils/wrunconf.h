			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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

#ifdef MSDOS
#define WRUNOPTIONS_ENV "WRUNOPTS"
#define WRUNCONFIG	"wrun.cfg"
#endif /* MSDOS */

struct wruncfg
{
	char	wrun_options[80];
	char	wrun_runcbl[80];
	char	wrun_cobtype[10];
};

#endif /* WRUNCONF_H */

