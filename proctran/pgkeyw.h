/*
******************************************************************************
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
******************************************************************************
*/

/* PGKEYW.H															*/
/*		 Initialization of keyword arrays										*/

#ifdef INIT_COMMON

EXT char *proc_keywords[] = {	"ASSIGN",
				"BACKUP",
				"CALL",
				"COPY",
				"DECLARE",
				"DELACL",
				"DESTROY",
				"DISKINIT",
				"DISMOUNT",
				"END",
				"EZFORMAT",
				"EXTRACL",
				"EXTRACT",
				"GOTO",
				"IF",
				"LINKER",
				"LISTVTOC",
				"LOGOFF",
				"MESSAGE",
				"MOUNT",
				"OPERATOR",
				"OPTION",
				"OPTIONS",
				"PRINT",
				"PROMPT",
				"PROTECT",
				"RENAME",
				"RETURN",
				"RUN",
				"SCRATCH",
				"SET",
				"SETACL",
				"SORT",
				"SUBMIT",
				"TRACE",
				"PROCEDURE",
				"PROC",
				""
			};

EXT char *run_clause[] = {	"CODE",
				"DISPLAY",
				"ENTER",
				"ERROR",
				"CANCEL",
				""
			};

EXT char *print_keyword[] = {	"CLASS",
				"DISP",
				""
			};

EXT char *submit_keyword[] = {	"CLASS",
				"DISP",
				"STATUS",
				"DUMP",
				"CPULIMIT",
				"ACTION",
				"ENVIRONMENT",
				"GLOBALS",
				""
			};

EXT char *link_keywords[] = {	"USING",
				"INTEGER",
				"STRING",
				""
			};

EXT char *declare_keywords[] = {	"USING",
					"INTEGER",
					"STRING",
					"INITIAL",
					"INIT",
					"GLOBAL",
					""
				};

EXT char *program_keywords[] = {	"USING",
					"IN",
					"ON",
					""
				};

EXT char *extract_keywords[] = {	"USED",
					"IN",
					"ON",
					"USE",
					""
				};

EXT char *if_keywords[] = {	"AND",
				"OR",
				"NOT",
				"EXISTS",
				"FILE",
				"LIBRARY",
				"VOLUME",
				"IN",
				"ON",
				"LT",
				"GT",
				"NE",
				"EQ",
				"GE",
				"LE",
				"NEQ",
				"NLT",
				"NGT",
				""
			};

EXT char *rs_keywords[] = {	"LIBRARY",
				"IN",
				"ON",

				"TO",
				"LIB",
				""
			};

EXT char *screen_keywords[] = {		"CENTER",
					"BLANK",
					"BLINK",
					"BRIGHT",
					"DIM",
					"LINE",
					"NUMERIC",
					"TAB",
					"UPLOW",
					"UPPER",
					"ROW",
					"PFKEY",
					"CURCOL",
					"CURROW",
					"ALARM",
					"ERASE",
					""
				};

EXT char *built_in_funcs[] = {	"&BYTE",
				"&COPY",
				"&DATE",
				"&INDEX",
				"&LABEL",
				"&LENGTH",
				"&MAX",
				"&MIN",
				"&MOD",
				"&RANK",
				"&TIME",
				"&TRANSLATE",
				"&VERIFY",
				""
			};

EXT char *vs_sys_util[] = {	"BACKUP",
				"COPY",
				"DISKINIT",
				"DISMOUNT",
				"EZFORMAT",
				"LINKER",
				"LISTVTOC",
				"MOUNT",
				"PROTECT",
				"SORT",
				"TRACE",
				""
			};

EXT char *trace_keywords[] = {	"RESOURCES",
				"SCRATCH",
				"STATEMENTS",
				"VARIABLES",
				""
			};
#else

EXT char *proc_keywords[];
EXT char *run_clause[];
EXT char *print_keyword[];
EXT char *submit_keyword[];
EXT char *link_keywords[];
EXT char *program_keywords[];
EXT char *extract_keywords[];
EXT char *if_keywords[];
EXT char *rs_keywords[];
EXT char *screen_keywords[];
EXT char *declare_keywords[];
EXT char *built_in_funcs[];
EXT char *vs_sys_util[];
EXT char *trace_keywords[];

#endif
/*
**	History:
**	$Log: pgkeyw.h,v $
**	Revision 1.9  2003/02/04 21:51:16  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  1997/04/21 14:56:51  scass
**	Corected copyright.
**	
**	Revision 1.7  1996-09-12 19:22:42-04  gsl
**	Add drcs headers
**
**
**
*/
