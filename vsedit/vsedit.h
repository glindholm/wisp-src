/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
**	File:		vsedit.h
**
**	Purpose:	To ...
**
*/

#ifndef vsedit_H
#define vsedit_H

#include <stdio.h>
#include <string.h>
#include "idsistd.h"
#include "idsisubs.h"
#include "vseglb.h"

int vsedit(char *name);
void int4_from_str2(char *str, int4 *value);
int load_txt_line(FILE *ff, TEXT **txtptr);
int4 next_lineno(int4 num);
void out_of_space(void);
int4 save_file(char *sysname, int4 start, int4 end);
int was_line_too_long(void);
int myfgets(char *buf, int size, FILE *file);
int init_lang(char *lang_string);
int lang_type(void);
char *lang_ext(void);
char *vse_err(int err_flag);
int wang_style_work_file(void);
int set_wang_style(int style);
void vse_set(void);

void vse_loading_menu(void);
void vse_standard_menu(void);
void vse_top_messages(void);
int translate_name(char *wang_file, char *wang_lib, char *wang_vol, char *native_name);
int is_read_only(void);
void vse_special_menu(void);
void vse_set(void);
int set_errfile_active(int active);
void vse_ed_ins(void);
int ask_renumber(void);
void vse_naf(void);
void vsedit_globals(void);
int vse_input(char *emsg);

#endif /* vsedit_H */
/*
**	History:
**	$Log: vsedit.h,v $
**	Revision 1.7  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.6  1996/09/03 22:24:02  gsl
**	drcs update
**	
**
**
*/
