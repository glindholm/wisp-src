/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
**	File:		wt_datad.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef WT_DATAD_H
#define WT_DATAD_H

extern NODE data_division(NODE the_statement);
extern NODE file_section(NODE the_statement);
extern int add_to_record_list(char *recname);
extern int fd_record(char *recname);
extern NODE parse_crt_records(void);
extern NODE delete_fd(NODE the_statement);

#endif /* WT_DATAD_H */
/*
**	History:
**	$Log: wt_datad.h,v $
**	Revision 1.5  1996/08/31 01:56:15  gsl
**	drcs update
**	
**
**
*/
