/* direct.c - direct interface between ACUCOBOL and 'C' */

/* Copyright (c) 1995-2006 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application.  */

/* THIS FILE IS #INCLUDED FROM sub.c.  BECAUSE SYSTEM HEADER FILES	*/
/* SHOULD BE INCLUDED BEFORE sub.h, AND BECAUSE THIS FILE IS INCLUDED	*/
/* AFTER sub.h, YOU REALLY SHOULDN'T INCLUDE ANY SYSTEM HEADER FILES	*/
/* FROM THIS FILE.  */

/* The following table can be used to link 'C' external variables with	*/
/* COBOL EXTERNAL data items.  To do this, place the COBOL name of the 	*/
/* data item in the first column and the address of the corresponding 	*/
/* 'C' item in the second column.  The 'C' data item must be declared 	*/
/* first and its address should be cast to a "char *".  For example, 	*/
/* if you had a 'C' short integer called "my_short", you could access	*/
/* this data item from COBOL by adding the following lines to this file	*/

/* extern short my_short;						*/
/* struct EXTRNTABLE EXTDATA[] = { 					*/
/* 	  { "MY-SHORT",		(char *) &my_short },			*/
/*	  { NULL,		NULL }					*/
/* };									*/
/*									*/
/* In COBOL, this data item would be declared:				*/
/*									*/
/* 77  MY-SHORT			PIC S9(4) COMP-5 EXTERNAL.		*/

/* Note that for portability reasons, it is preferable to avoid using	*/
/* "int" data items from COBOL since the size of an "int" depends on	*/
/* the machine that is being used.  This is also true about "short"	*/
/* and "long" variables, but these can fairly safely be assumed to be	*/
/* equivalent to S9(4) and S9(9) COMP-5 on most machines today.		*/

struct	EXTRNTABLE EXTDATA[] = {
	{ NULL,			NULL }
	};


/* The next table allows you to enter the names of 'C' functions that 	*/
/* you would like to call directly from COBOL without any intervening	*/
/* handling.  In the first column, you place the name you want to use	*/
/* in the COBOL CALL statement.  In the second column, you place the	*/
/* address of the routine to be called.  In the third column, you place	*/
/* the type of the function (these are declared in "sub.h").  Note	*/
/* that the CALL name should be all uppercase.  */

/* For example, to directly call the 'C' function "open", you would 	*/
/* do the following:	*/

/* extern int open();							*/
/* struct DIRECTTABLE LIBDIRECT[] = { 					*/
/* 	  { "OPEN",		FUNC open,	C_int },		*/
/*	  { NULL,		NULL,		0 }			*/
/*	  };								*/

/* The "FUNC" define is in "sub.h".  It casts the function name to the	*/
/* appropriate type.  To use this function in COBOL, you would do	*/
/* something like:							*/
/*									*/
/* 77  FILE-NAME		PIC X(20).				*/
/* 77  OPEN-MODE		PIC S9(4) COMP-5.			*/
/*									*/
/* MOVE NAME-TO-OPEN TO FILE-NAME.					*/
/* INSPECT FILE-NAME REPLACING TRAILING SPACES BY LOW-VALUES.		*/
/* MOVE ZERO TO OPEN-MODE.						*/
/* CALL "OPEN" USING BY REFERENCE FILE-NAME, BY VALUE OPEN-MODE.	*/
/*									*/
/* Note that strings passed to 'C' should have LOW-VALUE terminators.	*/
/* Also note that variables that are not passed by address should have	*/
/* the BY VALUE qualifier in COBOL and should be COMP-5.  Finally note	*/
/* that "int" parameters are best passed as S9(4) data items instead of	*/
/* S9(9) data items.  This is because an S9(4) data item is either a 	*/
/* "short" or an "int" depending on the host machine.  If it is a 	*/
/* "short", then the 'C' passing mechanism will promote this to an 	*/
/* "int".  On the other hand, S9(9) data items are "long" on some	*/
/* machines and will not match an "int" parameter.  */

struct	DIRECTTABLE LIBDIRECT[] = {
	{ NULL,			NULL,		0 }
	};

/* */
