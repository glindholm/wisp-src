/*
******************************************************************************
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
******************************************************************************
*/

/* PGCOMMON.H															*/
/*		 Common variable definitions											*/

#define PROCTRAN_VERSION	"V3.1b"							/* Procedure Translatore Version.	*/

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef unix
#include <unistd.h>
#endif

#define HT		'\t'								/* Define horizontal tab.		*/
#define SP		' '								/* Define space character.		*/
#define LNFD		(char)0x0a							/* Define line feed character.		*/

#ifndef NULL
#define NULL		'\0'								/* Standard null character def.		*/
#endif

#ifndef CHAR_NULL									/* Use so VMS compiler likes it.	*/
#define CHAR_NULL	'\000'								/* Standard null character def.		*/
#endif

#ifndef FALSE
#define FALSE		0								/* Standard false definition.		*/
#endif

#ifndef TRUE
#define TRUE		!FALSE								/* Standard true definition.		*/
#endif

#define FAILURE		0								/* Standard failure definition.		*/
#define SUCCESS		1								/* Standard success definition.		*/

#define WPLMAX		71								/* Max len of procedure source line.	*/
#define COBBUF		74								/* Maximum length of COBOL buffer.	*/
#define STRBUFF		256								/* Maximum length of STRING.		*/
#define FLDLEN		32								/* Maximum length of variable field.	*/
#define STRLNGTH	513								/* Maximum length of string value.	*/
#define CNTTAB		15								/* Nul name tab for pic.		*/

/* Define some CASE symbols for finding key words.										*/

#define ASSIGN		0
#define CALL		2
#define DECLARE		4
#define END		9
#define EXTRACT		12
#define GOTO		13
#define IF		14
#define LOGOFF		17
#define MESSAGE		18
#define PRINT		23
#define PROMPT		24
#define RENAME		26
#define RETURN		27
#define RUN		28
#define SCRATCH		29
#define SET		30
#define SUBMIT		33
#define PROCEDURE	35
#define PROC		36

#define CODE		0
#define DISPLAY		1
#define ENTER		2
#define ERROR		3
#define CANCEL		4

#define CLASS		0
#define DISP		1
#define STATUS		2
#define DUMP		3
#define CPULIMIT	4
#define ACTION		5
#define ENVIRONMENT	6
#define GLOBALS		7

#define USING		0 
#define INTEGER		1
#define STRING		2
#define INITIAL		3
#define INIT		4
#define GLOBAL		5

#define IN		1
#define ON		2
#define USE		3

#define AND		0
#define OR		1
#define NOT		2
#define EXISTS		3
#define FILENAME	4
#define LIBRARY		5
#define VOLUME		6
#define IFIN		7
#define IFON		8
#define IFLT		9
#define IFGT		10
#define IFNE		11
#define IFEQ		12
#define IFGE		13
#define IFLE		14
#define IFNEQ		15
#define IFNLT		16
#define IFNGT		17

#define RSLIBRARY	0
#define RSIN		1
#define RSON		2
#define RSTO		3
#define RSLIB		4

#define CENTER		0
#define BLANK		1
#define BLINK		2
#define BRIGHT		3
#define DIM		4
#define LINE		5
#define NUMERIC		6
#define TAB		7
#define UPLOW		8
#define UPPER		9

#define ROW		10
#define PFKEY		11
#define CURCOL		12
#define CURROW		13
#define ALARM		14
#define ERASE		15

/*		 Define FAC bits for mask											*/

#define FMCENTER	0x0001 
#define FMBLANK		0x0002
#define FMBLINK		0x0004
#define FMBRIGHT	0x0008
#define FMDIM		0x0010
#define FMLINE		0x0020
#define FMNUMERIC	0x0040
#define FMTAB		0x0080
#define FMUPLOW		0x0100
#define FMUPPER		0x0200

#include "pgstruct.h"

void write_log();
int len_to_int(char *strlen);								/* Convert string length field to int.	*/
int operand(char c);									/* Test if character ia an operand.	*/
void indent_line(void);									/* Add the correct indentation.		*/
void concat_var_prefix(char type);							/* Concatenate the variable prefix for	*/
void end_prt_line(int pfl);								/* Add period if needed and line feed,	*/
int strpos(char* src, char* srch);							/* search a string for the occurence of	*/
int chk_num(char *instr);								/* Return FALSE if instr contains 	*/
int find_keyword(char *keystr, char *keylist[]);					/* Find the keyword in the list.	*/
void wrt_keyword_vars(void);								/* Write the working storage variables	*/
void check_after_bump(char **value,char *text_info);					/* Write COBOL source to output file.	*/
int number(char c);									/* Test if character is a number.	*/
void get_type_len(char*fld, char* rcvr,int* num_var,int* num_val,char getop);		/* Search table for fld match.		*/
void wrt_subscript(int nfl,int caller);							/* Process the subscript.		*/
void sp_trunc(char* text);								/* Truncate a string at the first space	*/
int tststrt(char* cptr);								/* Test if valid start for token pson.	*/
void get_next_line(void);								/* Read the next line for input source.	*/
void go_upper(char *strp);								/* Converts a string to upper case.	*/
int trim(char* string);									/* Trim a string of trailing blanks.	*/
int strlast(char* string, char srch);							/* determine if the srch char is the	*/
int nexttok(void);									/* Get psn of next token from linein,	*/
int find_char(char keyc, char* keylist[]);						/* Find the keychar in array.		*/
int find_colin(char** cptr);								/* Look for : to define proc-heading.	*/
int letter(char c);									/* Test if character is a letter.	*/
void set_value(char* num_buf,int* num_asn,int current_row);				/* Init the screen variable and get	*/
void makeint(char* equate1, char* name_buf, int* num_asn);
void setup_line(char* lptr);								/* Put linein into expectd format.	*/
char *upper_string(char* str);								/* Convert a string to uppercase.	*/
int is_backref(void);									/* Determine if field is backwards	*/
void init_using(void);									/* Standard init of program using.	*/
void init_current(int* num_var);							/* Standard init of command buffer.	*/
void init_assign(int* num);								/* Standard init of command buffer.	*/
void p_prg_kw(int tndx, int* num_var, int* num_val);					/* Process program keywords.		*/
void save_using_item(int* num_asn,int* num_var,int* filler_num,int* current_row,int* num_val);	/* Process using item.		*/
void p_link_kw(int tndx, int* num_link_var);						/* Process link keywords.		*/
void init_link(int* num_link_var);							/* Standard init of program link.	*/
void save_link_var(int* num_link_var);							/* Save the variable in link_item.	*/
void init_para(int* num_cmds);							/* Standard init of command buffer.	*/
void init_cmd(int* num_cmds);								/* Standard init of command buffer.	*/
void init_screen(int* screen_num);							/* Standard init of screen buffer.	*/
void init_screen_field(int* current_row);						/* Standard init of screen buffer.	*/
void init_string_param(void);								/* Standard init of STRING parameter	*/
void setscreen(void);									/* Initialize the screen flags.		*/
void do_start_screen(void);
void process_sub_var(char type,
		     int* num_var,
		     int* num_val,
		     int  caller,
		     int* num_asn,
		     int* filler_num,
		     int* current_row,
		     int* num_cmds);							/* Generate working storage for substr.	*/
void p_rs_kw(int  tndx,
	     int* num_var,
	     int* num_val,
	     int* filler_num,
	     int* current_row,
	     int* num_cmds);
void setflag(void);									/* Init all of the "in" flags.		*/
program_item *init_prg(void);								/* Standard init of program buffer.	*/
void init_return(void);									/* Standard init of return buffer.	*/
rs_item *init_rs(void);									/* Standard init of program buffer.	*/
void init_pparm(int func);								/* Standard init of putparm buffer.	*/
void p_run_clause_kw(int ndx);								/* Process RUN Clause keyword.		*/
void p_print_submit_kw(int caller,int ndx);						/* Process PRINT/SUBMIT keyword.	*/
int p_pl_kw(int  ndx,
	    int* num_val,
	    int* num_asn,
	    int* num_cmds,
	    int* scn_num,
	    int* current_row,
	    int* num_var);								/* Process the WPL keywords.		*/
void usage(int prnt);
void get_cli(int argc,char *argv[]);							/* Note: argc, argv used for unix code.*/
void init_literals(void);								/* Initialize the common literal strings.*/
int doit(void);										/* Do the COBOL generation.		*/
void init_if(void);									/* Standard init of command buffer.	*/
void init_se(void);									/* Standard init of command buffer.	*/
void p_assign(int* num_asn,
	      int* num_var,
	      int* filler_num,
	      int* current_row,
	      int* num_val,
	      int* num_cmds);								/* Process the current assign equation.	*/
void wrt_init_assign(void);
void wrt_call(void);									/* Handles syntax for call verbs.	*/
void wrt_goto(void);									/* Handles syntax for goto statement.	*/
void wrt_if(void);									/* Writes syntax for if statements.	*/
void wrt_program(void);									/* Writes out syntax for the run,	*/
void wrt_return(void);									/* Handles syntax for the RETURN verb.	*/
void wrt_rs(void);									/* Write out renames and scratchs.	*/
void wrt_set_extract(void);								/* Write out set and extract syntax.	*/
void wrt_readfdr(void);									/* Write out readfdr syntax.		*/
void p_declare(int tndx,int *num_val,int *num_var,int *num_asn);
void p_extract_kw(int tndx);
void save_set_ext_var(int* num_var,
		      int* num_val,
		      int* num_asn,
		      int* filler_num,
		      int* current_row,
		      int* num_cmds);
void init_rfdr(void);									/* Standard init of command buffer.	*/
void p_readfdr(int* num_var,int* num_val);
void chk_cmd(int *num_cmds,int *num_var);						/* Routine checks tables for a valid	*/
void setgot(void);									/* Init all of the "got" flags.		*/
int test_para_name(int num_par, int* num_cmds);						/* See if it is a paragraph name.	*/
void save_declare_var(int *num_asn,int *num_var,int *num_val, int *num_cmds);		/* Save the variable in declare_item.	*/
void p_if_kw(int tndx,
	     int* num_var,
	     int* num_val,
	     int* filler_num,
	     int* current_row,
	     int* num_cmds);								/* Proccess IF keyword.			*/
void save_if_var(int* num_var,
		 int* num_val,
		 int* filler_num,
		 int* current_row,
		 int* num_cmds)	;							/* Proccess IF variable.		*/
void p_scn_kw(int tndx);								/* Process screen keywords.		*/
void save_screen_item(int* num_asn,int* num_var,int* num_val,int* current_row);		/* Save the screen expression.		*/
void init_ppkw(void);									/* Standard init of putparm keyword.	*/


/*
**	History:
**	$Log: pgcommon.h,v $
**	Revision 1.9  2003/02/04 21:51:17  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  1997/04/21 14:49:03  scass
**	Corrected copyright
**	
**	Revision 1.7  1996-09-12 19:12:57-04  gsl
**	Add prototypes for everything
**	Include pgstruct.h as is needed for some of the prototypes.
**	Done to built on NT
**
**
**
*/
