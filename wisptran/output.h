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
**	File:		output.h
**
**	Purpose:	To ...
**
**
*/

#ifndef OUTPUT_H
#define OUTPUT_H

#include "token.h"
#include "node.h"
#include "wispfile.h"

extern NODE tput_statement(int column, NODE tree);
extern int tput_fluff(NODE tree);
extern void tput_leading_fluff(NODE the_statement);
extern int tput_set_context(void *the_context);
extern void *tput_use_context(void);
extern int tput_block(char *the_block);
extern int tput_clause();
extern int tput_line_at();
extern int tput_line();
extern int tput_scomment();
extern int tput_noprocess(char *line);
extern int tput_blank(void);
extern int tput_flush(void);
extern int tput_close_copybook(void);
extern int tput_is_copybook(void);
extern int tput_token_cache(void);
extern int override_output_stream(cob_file *cob_file_ptr);
extern int release_output_stream(void);
extern int tput_token(int mincol, TOKEN *tokptr);
extern int put_cobol_line(cob_file *cob_file_ptr, char *the_line);
extern int write_file(A_file *the_file, char *buff);
extern void split_token_to_dcl_file(int mincol, TOKEN *tokptr);
extern void split_token_to_dtp_file(int mincol, TOKEN *tokptr);

#endif /* OUTPUT_H */
/*
**	History:
**	$Log: output.h,v $
**	Revision 1.11  2003/12/03 16:18:48  gsl
**	Fix so native screen fields and screen sections don't get generated in a copybook file.
**	
**	Revision 1.10  2003/12/02 21:23:21  gsl
**	Fix so native screen sections don't get generated in a copybook file.
**	Change generated copybooks (internal) to use same file extension rules
**	as translated copybooks. Default to .cob extension.
**	
**	Revision 1.9  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.8  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.7  1999/09/07 14:34:45  gsl
**	Fix prototypes.
**	
**	Revision 1.6  1998-03-03 16:04:52-05  gsl
**	Add tput_leading_fluff()
**
**	Revision 1.5  1996-08-30 21:56:07-04  gsl
**	drcs update
**
**
**
*/
