/* 
	Copyright (c) 1995-1997 NeoMedia Technologies Inc., All rights reserved.
	$Id:$
*/

/*
**	File:		pgstruct.h
**
**	Project:	proctran
**
**	RCS:		$Source:$
**
**	Purpose:	Common structure deinitions
**
*/

#ifndef pgstruct_H
#define pgstruct_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

typedef struct assign_item {
		int	sub_flag;							/* Subscript field flag.		*/
		int	br_flag;							/* Flags to indicate backwards ref.	*/
		char	field1[STRLNGTH];						/* Output name.				*/
		char	type[3];							/* Type of assign string.		*/
		char	length[6];							/* Length of input string.		*/
		char	len_type[3];							/* Type of length variable.		*/
		char	dlmtr;								/* Delimeter character.			*/
		char	start_pos[FLDLEN];						/* The start of input string.		*/
		char	stpos_type[3];							/* Type of start_pos variable.		*/
		char	literal[10];
		struct	assign_item *prev_item;						/* A ptr to the previous item		*/
		struct	assign_item *next_item;						/* A ptr to the next item		*/
	} assign_item;

typedef struct declare_item {
		char	field1[FLDLEN];							/* Field name.				*/
		char	type[3];							/* The variable type.			*/
		char	length[6];							/* The length of string.		*/
		char	value[STRLNGTH];						/* The value of string.			*/
		struct	declare_item *prev_item;					/* A ptr to previous item.		*/
		struct	declare_item *next_item;					/* A ptr to the next item.		*/
	} declare_item;

typedef struct link_item {
		char	field1[FLDLEN];							/* Field name.				*/
		char	type[3];							/* The variable type.			*/
		char	length[6];							/* The length of string.		*/
		struct	link_item *prev_item;						/* A ptr to Previous item.		*/
		struct	link_item *next_item;						/* A ptr to the next item.		*/
	} link_item;

typedef struct paragraph_item {
		char	name[FLDLEN];							/* Name of paragraph I am in.		*/
		char	rlbl;								/* Set if RUN paragraph label.		*/
		struct	command_item *all_command;					/* Pointer to all commands in para.	*/
		struct  paragraph_item *prev_item;					/* A ptr to Previous item.		*/
		struct	paragraph_item *next_item;					/* A ptr to the next paragraphs.	*/
	} paragraph_item;

typedef struct command_item {
		char	command[FLDLEN];						/* Command that I am examining.		*/
		char	goto_para[FLDLEN];						/* GOTO paragraph.			*/
		char	gotop_type[3];							/* GOTO paragraph type.			*/
		struct	return_item *return_area;					/* A ptr to the return code logic	*/
		char	call_para[FLDLEN];						/* call paragraph.			*/
		char	*end_cmd;							/* address that points to end.		*/
		char	*logoff_cmd;							/* address that points to end.		*/
		struct	assign_item  *assign_var;					/* A ptr to the assign logic		*/
		struct	if_item *if_area;						/* Point to if names.			*/
		struct	set_extract_item *set_extract_area;				/* Point to calls to set and extract.	*/
		struct	readfdr_item *rfdr_area;					/* Point to calls to readfdr.		*/
		struct	program_item *program_area;					/* Point to run,print,submit,find.	*/
		struct	rs_item *rs_area;						/* Point to call paragraph.		*/
		struct	screen_item *screen_area;					/* Point to Screen statement.		*/
		struct  command_item *prev_item;					/* A ptr to previous item.		*/
		struct	command_item *next_item;					/* A ptr to the next item.		*/
	} command_item;

typedef struct screen_item {
		int	num_screen;							/* Number of screen i'm in for ws sec.	*/
		int	num_rows;							/* Number of rows defines in screen.	*/
		char	pf_key[FLDLEN];							/* Variable for PF key.			*/
		char	pfk_type[3];
		char	cur_row[FLDLEN];						/* Variable for cursor row.		*/
		char	cr_type[3];
		char	cur_col[FLDLEN];						/* Variable for cursor column.		*/
		char	cc_type[3];
		char	screen_erase;							/* Variable for erase the screen.	*/
		char	screen_alarm;							/* Variable for bell screen.		*/
		struct	scn_fld_item *variable_area;					/* Hold the variables in the screen.	*/
		struct	screen_item *next_item;						/* Store the hold screens.		*/
	} screen_item;

typedef struct scn_fld_item {
		int	row;								/* where do i display the field.	*/
		int	col;								/* where do i display the field.	*/
		int	sub_flag;							/* Subscript field flag.		*/
		long	fac_msk;							/* FAC bit mask for field display.	*/
		char	screen_fld[STRLNGTH];						/* Value or field to display.		*/
		char	modf;								/* Modifiable FAC (if any).		*/
		char	scn_fld_type[3];						/* Type of screen field.		*/
		char	map_fld[STRLNGTH];						/* Holds the value in the map.		*/
		char	len[FLDLEN];							/* How big is it.			*/
		char	dlmtr;								/* Delimeter character.			*/
		char	start_pos[FLDLEN];						/* The start of input string.		*/
		char	stpos_type[3];							/* Type of start_pos variable.		*/
		char	length[6];							/* The length of input string.		*/
		char	len_type[3];							/* Type of length variable.		*/
		struct	string_item  *str_params;					/* Ptr to list of STRING parameters.	*/
		struct	scn_fld_item *prev_item;					/* Ptr to previous item			*/
		struct	scn_fld_item *next_item;					/* Do I have another?			*/
	} scn_fld_item;

typedef struct string_item {
		char	field1[STRLNGTH];						/* String of var STRING parameters.	*/
		struct	string_item *next_item;						/* Do I have another?			*/
	} string_item;

typedef struct using_item {
		int	br_flag;							/* Is a backwards referenced field.	*/
		char	var[FLDLEN];							/* Varible in using clause.		*/
		char	type[3];							/* Type of variable.			*/
		struct	using_item *next_item;						/* Point to next Item.			*/
	} using_item;

typedef struct set_extract_item {
		int	sub_flag;							/* Subscript field flag.		*/
		int	br_flag;							/* Backwards reference flag.		*/
		char	var[FLDLEN];							/* Varible in SET/EXTRACT clause.	*/
		char 	type[3];							/* Type of variable.			*/
		char	key[FLDLEN];							/* Keyword for extract.			*/
		char	dlmtr;								/* Delimeter character.			*/
		char	start_pos[FLDLEN];						/* The start of input string.		*/
		char	stpos_type[3];							/* Type of start_pos variable.		*/
		char	length[6];							/* The length of input string.		*/
		char	len_type[3];							/* Type of length variable.		*/
		struct	set_extract_item *next_item;					/* Point to next Item.			*/
	} set_extract_item;

typedef struct readfdr_item {
		int	br_flag;							/* Flags to indicate backwards ref.	*/
		int	name_br;							/* Set if file referenced.		*/
		int	lib_br;								/* Set if library referenced.		*/
		int	vol_br;								/* Set if volume referenced.		*/
		char	recvr[FLDLEN];							/* Variable for info.			*/
		char	type[3];							/* Type of variable.			*/
		char	file[FLDLEN];							/* File to retrive info. on.		*/
		char	file_type[3];							/* Type of file.			*/
		char	lib[FLDLEN];							/* Library.				*/
		char	lib_type[3];							/* Type of library.			*/
		char	vol[FLDLEN];							/* Volume.				*/
		char	vol_type[3];							/* Type of volume.			*/
		char	id[3];								/* Keyword id for info. to retrieve.	*/
	} readfdr_item;

typedef struct if_item {
		int	br_flag;							/* Backwards reference flag.		*/
		int	sub_flag;							/* Subscript field flag.		*/
		int	file_br;							/* Flag if filename back. ref.		*/
		int	lib_br;								/* Flag if library back. ref.		*/
		int	vol_br;								/* Flag if volume back. ref.		*/
		char	paren_value[STRLNGTH];						/* What is the value in the parenthesis.*/
		char	var[STRLNGTH];							/* Varible or string in the if clause.	*/
		char	type[3];							/* Type of variable.			*/
		char	cond[3];							/* What if the cond or connection.	*/
		char	file[FLDLEN];							/* Point to find file.			*/
		char	file_type[3];							/* Type of file.			*/
		char	lib[FLDLEN];							/* Point to find file library.		*/
		char	lib_type[3];							/* Type of file.			*/
		char	vol[FLDLEN];							/* Point to find file volume.		*/
		char	vol_type[3];							/* Type of file.			*/
		char	dlmtr;								/* Delimeter character.			*/
		struct	if_item *next_item;						/* Point to next Item.			*/
	} if_item;

typedef struct program_item {
		int	number_using;							/* Count number in using table.		*/
		int	sub_flag;							/* Subscript field flag.		*/
		int	br_flag;							/* Flags to indicate backwards ref.	*/
		int	name_br;							/* Set if file referenced.		*/
		int	lib_br;								/* Set if library referenced.		*/
		int	vol_br;								/* Set if volume referenced.		*/
		char	func;								/* P=print,S=submit,R=run,F=find	*/
		char	name[FLDLEN];							/* Program name.			*/
		char	name_type[3];							/* Type of name.			*/
		char	as_name[FLDLEN];						/* job name.				*/
		char	as_name_type[3];						/* Type of job name.			*/
		char	lib[FLDLEN];							/* Program Library.			*/
		char	lib2[FLDLEN];							/* Use if concatenation of name.	*/
		char	lib_type[3];							/* Type of library.			*/
		char	lib2_type[3];							/* Type of variable if concatenated.	*/
		char	vol[FLDLEN];							/* Program Volume.			*/
		char	vol2[FLDLEN];							/* Use if concatenation of name.	*/
		char	vol_type[3];							/* Type of volume.			*/
		char	vol2_type[3];							/* Type of variable if concatenated.	*/
		char	error_lbl[FLDLEN];						/* Label to goto for Error Exit clause.	*/
		char	cancel_lbl[FLDLEN];						/* Label to goto for Cancel Exit clause.*/
		char	rtrn_cd_lbl[FLDLEN];						/* Label var to assign RETURN-CODE.	*/
		struct	putparm_item *pparm_area;					/* Point to calls to putparm.		*/
		struct	using_item *using_name;						/* Address that points to Prameters.	*/
	} program_item;

typedef struct rs_item {
		int	br_flag;							/* Flags to indicate backwards ref.	*/
		int	name_br;							/* Set if file referenced.		*/
		int	lib_br;								/* Set if library referenced.		*/
		int	vol_br;								/* Set if volume referenced.		*/
		int	sub_flag;							/* Subscript field flag.		*/
		char	func;								/* S=scratch,R=rename.			*/
		char	opt;								/* L for library functions.		*/
		char	name[FLDLEN];							/* Program name.			*/
		char	name_type[3];							/* Type of name variable.		*/
		char	lib[FLDLEN];							/* Program Library.			*/
		char	lib_type[3];							/* Type indicator of variable.		*/
		char	vol[FLDLEN];							/* Program Volume.			*/
		char	vol_type[3];							/* Type indicator of variable.		*/
		char	to_file[FLDLEN];						/* Program to file.			*/
		char	to_file_type[3];						/* Type of file variable.		*/
		char	to_lib[FLDLEN];							/* Program to lib.			*/
		char	to_lib_type[3];							/* Type of lib variable.		*/
		char	lbl[FLDLEN];							/* Label associated with function.	*/
		char	dlmtr;								/* Delimeter character.			*/
		char	start_pos[FLDLEN];						/* The start of input string.		*/
		char	stpos_type[3];							/* Type of start_pos variable.		*/
		char	length[6];							/* The length of input string.		*/
		char	len_type[3];							/* Type of length variable.		*/
	} rs_item;

typedef struct putparm_item {
		int	kwcnt;								/* Number of keywords in putparm.	*/
		int	br_flag;							/* Flag to indicate backwards refernece.*/
		char 	func;								/* Display or Enter putparm.		*/
		char	prname[9];							/* Parameter Reference name for putparm.*/
		char    label[FLDLEN];							/* Label for backwards reference.	*/
		char	pfkey[FLDLEN];							/* Key to satisfy putparm.		*/
		struct	pp_keywords *kwlist;						/* Point to keyword list.		*/
		struct	putparm_item *prev_item;					/* Point to previous Item.		*/
		struct	putparm_item *next_item;					/* Point to next Item.			*/
	} putparm_item;

typedef struct pp_keywords {
		int	len;								/* Length of keyword.			*/
		int	len2;								/* Length of keyword.			*/
		int	br_flag;							/* Flag if used in backwards reference.	*/
		int	sub_flag;							/* Flag if used as subscripted field.	*/
		char 	keywrd[FLDLEN];							/* Putparm keyword.			*/
		char	val[STRLNGTH];							/* Keyword value.			*/
		char	type[3];							/* Type of value.			*/
		char	val2[STRLNGTH];							/* Concatenation Keyword value.		*/
		char	type2[3];							/* Type of value.			*/
		struct	pp_keywords *next_item;						/* Point to next keyword.		*/
	} pp_keywords;

typedef struct return_item {
		char	var[FLDLEN];							/* Varible in RETURN clause.		*/
		char 	type[3];							/* Type of variable.			*/
		struct	return_item *next_item;						/* Point to next Item.			*/
	} return_item;

/********************************************************************************************************************************/
/*		 Global definitions of pointers to structures used throughout PROCTRAN						*/
/********************************************************************************************************************************/

EXT paragraph_item *cur_para, *st_para;							/* Allocate ptrs to paragraph inst.	*/
EXT command_item *cur_cmd;								/* Allocate command stuff for para.	*/
EXT assign_item	 *cur_assign, *hld_assign;						/* Allocate assign instructions.	*/
EXT rs_item	 *cur_ren_sctch;							/* Allocate rename/scratch information.	*/
EXT program_item *cur_prg;								/* Allocate Program information.	*/
EXT if_item	 *cur_if;								/* Allocate command stuff for ifs.	*/
EXT set_extract_item *cur_set_extr;							/* Allocate set extract information.	*/
EXT screen_item	 *cur_scn, *start_screen;						/* Allocate command stuff for screen.	*/
EXT declare_item *cur_decl, *hld_table;							/* Allocate Variable Table.		*/
EXT link_item	 *cur_link, *stlinklst, *hld_link;					/* Allocate Link Variable Table.	*/
EXT scn_fld_item *cur_scn_fld;								/* Allocate stuff for screen field.	*/
EXT string_item  *cur_str_param;							/* Allocate stuff for STRING field.	*/
EXT using_item	 *cur_using;								/* Allocate using information.		*/
EXT putparm_item *cur_pp;								/* Allocate putparm instructions.	*/
EXT pp_keywords *cur_pp_key;								/* Allocate putparm keyword values.	*/
EXT readfdr_item *cur_rfdr;								/* Allocate readfdr information.	*/
EXT return_item *cur_rtrn;								/* Allocate return code information.	*/

/*
**	Function Prototypes
*/

#endif /* pgstruct_H */

/*
**	History:
**	$Log: pgstruct.h,v $
**	Revision 1.8  1997-04-21 10:58:05-04  scass
**	Corrected copyright.
**
**	Revision 1.7  1996-09-12 19:14:01-04  gsl
**	Add standard headers to allow multiple includes
**
**
**
*/
