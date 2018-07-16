/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vsegps.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vsegps_H
#define vsegps_H

#include "idsistd.h"

int vse_output(char *file, char *library, char *volume, char *sysname, int4 *start_line, int4 *end_line);
int vse_renumber_gp(char number_field[7], int4 *number, char incr_field[7], int4 *incr, 
		char start_field[17], int4 *start, char end_field[17], int4 *end, int resp);
int vse_badnums_gp(char number_field[7], char incr_field[7], char *err_msg);
int vse_longline_gp(char *sysname);
int vse_readonly_gp(void);
int vse_defaults_gp(int hidden);
int vse_options_gp(int hidden);
int vse_no_file_created(int rest);
int vse_xcopy_gp(char *file_field, char *library_field, char *volume_field, char *sysname_field, 
		 char *start_field, char *end_field, char *target_field, char *modcode_field,
		 char *message_field, int message_code, int resp);
int vse_file_changed_gp(char *sysname);

#endif /* vsegps_H */
/*
**	History:
**	$Log: vsegps.h,v $
**	Revision 1.5  1996-09-03 18:24:07-04  gsl
**	drcs update
**
**
**
*/
