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
**	File:		lines.h
**
**	Purpose:	COBOL line handling headers
**
**
**	History:
**	06/01/93	Written by GSL
**
*/

#ifndef LINES_H
#define LINES_H

#define NORMAL_LINE	0
#define PROCESS_LINE	1
#define	NOPROCESS_LINE	2
#define SPECIAL_COMMENT 3

int	get_cobol_inline();
int	get_cobol_line();
int	get_next_cobol_line();
int	get_conditional_cobol_line();
int	get_clean_cobol_line();
char 	*wfgets();

#endif /* LINES_H */
/*
**	History:
**	$Log: lines.h,v $
**	Revision 1.6  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/08/31 01:56:05  gsl
**	drcs update
**	
**
**
*/
