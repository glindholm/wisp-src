/* Copyright (c) 1988-1997 NeoMedia Technologies Inc., All rights reserved. */
/* $Id:$ */
/*
**	File:		pgglobal.h
**
**	Purpose:	To define global variables used throughout the utility.
**
**
**	History:	mm/dd/yy	Written by Suzette Cass
**
*/

EXT FILE *infile, *outfile;								/* Ptrs to source file and output file.	*/
EXT FILE *logfile;									/* Current logfile (stdout by default)	*/

EXT char linein[STRBUFF], rline[STRBUFF];
EXT char cobline[COBBUF];
EXT char *aptr, *next_ptr;								/* Tells the pointer to next string.	*/

EXT int logging;									/* Flag to enable logging.		*/
EXT int log_stats;									/* Flag to enable statistic logging.	*/
EXT int num_lineins;									/* Number of input lines read.		*/
EXT int num_outlines;									/* Number of output lines written.	*/
EXT int lncnt_type;									/* Flag to say print in or out line cnt.*/
EXT int genslink;									/* Flag to enable generation of "S" type*/
											/*  links for " " type links for ALL.	*/
EXT int genccall;									/* Flag to enable generation CALL verify*/
											/*  logic as a comment.			*/
EXT int gendisplay;									/* Flag to enable generation of DISPLAY	*/
											/*  when error on subroutines.		*/
EXT int in_assign, in_print, in_run, in_submit, in_if, in_set;				/* Define of all "in" flags.		*/
EXT int in_extract, in_return, in_scratch, in_rename, in_message;
EXT int in_prompt, in_pfkey, in_curcol, in_currow, in_erase;
EXT int in_alarm, in_declare, in_proc, in_using, in_logoff;
EXT int in_library, in_row, in_putparm, in_readfdr, in_label;
EXT int in_global, in_trace;
EXT int in_np_putparm;

EXT int spif;										/* Spacing for if flag.			*/
EXT long cur_fac_msk;									/* FAC ATRIBUTES FOR A FIELD.		*/
EXT char cur_mod_fac;
EXT int num_sf_var;									/* Num for generated screen fields.	*/
EXT int set_string;
EXT int cnt_sf;

EXT int got_rename, got_find, got_print, got_run, got_submit;				/* Define of all "got" flags.		*/
EXT int got_set, got_extract, got_link, got_logoff, got_scratch;
EXT int got_prompt, got_name, got_message, got_putparm, got_string;
EXT int got_readfdr, got_backref;

/*  Define the constant literals that will be used throughout the utility.							*/
EXT char util[9], paren_sym[2], loc_str_typ[3], loc_int_typ[3];
EXT char int_compute[3], int_return[3], str_literal[3];
EXT char len_one[4], len_two[4];
EXT char *captr;
EXT int lcnt;
/*
**	History:
**	$Log: pgglobal.h,v $
**	Revision 1.9  1997-04-21 10:55:44-04  scass
**	Corrected copyright.
**
**	Revision 1.8  1996-09-12 19:22:33-04  gsl
**	Add drcs headers
**
**
**
*/
