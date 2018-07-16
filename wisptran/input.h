/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/*
**	File:		input.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
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
**	Revision 1.5  1996/08/31 01:56:04  gsl
**	drcs update
**	
**
**
*/
