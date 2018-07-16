/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*
**	File:		vsegps.h
**
**	Purpose:	To ...
**
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
**	Revision 1.6  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.5  1996/09/03 22:24:07  gsl
**	drcs update
**	
**
**
*/
