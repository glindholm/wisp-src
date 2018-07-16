/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		create.h
**
**	Project:	KCSI
**
**	RCS:		$Source:$
**
**	Purpose:	Generic prototype header
**
*/

#ifndef create_H
#define create_H
/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#endif

#include "intdef.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void cr_io(char *io);
void cr_name_and_type_help(void);
void cr_primary_key_help(void);
void cr_split_help(void);
void cr_alt_keys_help(void);
void cr_exit(int code);
int cr_get_output_spec(char *vers, char *platform);
void cr_get_blocks(void);
void cr_create_file(void);
int cr_eoj(void);
void cr_print_the_spec(void);
void cr_file_field_help(void);
void save_cr_in_file(void);
void cr_get_blocks(void);
int cr_get_field_type(void);
void cr_free_blks(void);
void cr_blk_and_fld(int row, int col);
int cr_get_file_field(void);


/*
**	WISPLIB
*/
void wvaset(int4 *x);
void wscreen();
void wswap(void *lword);			/* swap the order of the words in a longword item (for WANG routines to use)	*/
void GETPARM();
void SCRATCH();
void wfopen(
	int4 *mode,		/* the mode of opening			*/
	char *vol,		/* the WANG volume name	(6 chars)	*/
	char *lib,		/* The WANG library name (8 chars)	*/
	char *file,		/* The file name	(8 chars)	*/
	char *name,		/* The resultant name			*/
	char *prname);		/* The PRNAME (optional).		*/
void wpload(void);
void setretcode(char* code);
void EXTRACT();
void wfclose(char* fname);								/* This routine is called after COBOL*/
int link_display(const char *file_name);
void FIND();
void SET();
void WISPSORT(char *sortparms, char *filetype, int4 *recsize, int4 *sortcode, int4 *returncode);
void vwang_shut(void);

#endif /* create_H */

/*
**	History:
**	$Log: create.h,v $
**	Revision 1.4  1997/10/30 20:52:01  scass
**	Corrected type
**	
**	Revision 1.3  1997-10-23 13:59:20-04  gsl
**	change to use link_display()
**
**	Revision 1.2  1996-10-02 18:09:35-04  gsl
**	remove squeeze() prototype - made local
**
**	Revision 1.1  1996-10-02 09:13:11-07  gsl
**	Initial revision
**
**
**
**
*/
