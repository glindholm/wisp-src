/*
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
*/

/*
**	File:		input.h
**
*/

#ifndef INPUT_H
#define INPUT_H

#include "wispfile.h"

extern int get_cobol_inline(void);
extern int get_cobol_line(char *the_line, int linesize);
extern int get_next_cobol_line(char *the_line, int linesize);
extern int get_conditional_cobol_line(char *the_line, int linesize, A_file *the_file, int copy_within_conditional_code);
extern int get_clean_cobol_line(char *the_line, int linesize, A_file *the_file);
extern int hold_this_line(char *the_line);
extern int get_held_line(char *the_line);
extern int held_line(void);
extern int end_of_input(void);
extern int set_end_of_input(void);


#endif /* INPUT_H */
/*
**	History:
**	$Log: input.h,v $
**	Revision 1.7  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.6  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/08/31 01:56:04  gsl
**	drcs update
**	
**
**
*/
