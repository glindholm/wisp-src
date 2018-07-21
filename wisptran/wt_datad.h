/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
*/

/*
**	File:		wt_datad.h
**
**	Purpose:	To ...
**
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
**	Revision 1.7  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.6  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/08/31 01:56:15  gsl
**	drcs update
**	
**
**
*/
