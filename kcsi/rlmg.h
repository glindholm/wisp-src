/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

#ifndef	_RLMG_H
#define	_RLGM_H

/*----
IO codes used by listmgt only
------*/

#define		WRITE_FIELD	"LL"
#define		END_RECORD	"RR"

/*----
IO CODES used by report and listmgmt
------*/

#define		OPEN_PRINTER_FILE	"OO"
#define		CLOSE_PRINTER_FILE	"CC"

/*----
IO CODES used only by report
------*/
#define		FORM_FEED	"FF"
#define		PRINT_LINE	"WW"
#define		DISPLAY_FILE	"DD"
#define		CLOSE_AND_KEEP	"KK"


/*----
File types for field separators
------*/

#define		DATA_CODE	1
#define		DOC_CODE	2
#define		SEC_CODE	3
#define		MERGE_CODE	4
#define		SDF_CODE	5
#define		DISP_SDF_CODE	6


/*----
Field and record terminators
------*/
#define		FIELD_TERMINATOR	"{FS}"
#define		RECORD_TERMINATOR	"{RS}"

/*---
UPPER/lower case codes
No change must = 0;
------*/

#define	NO_CASE_CHANGE		0
#define	UPPER_CASE_CHANGE	1
#define	LOWER_CASE_CHANGE	2
#define	UPLOW_CASE_CHANGE	3
#define	TABLE_CASE_CHANGE	4


#define	INCLUDE_THEM		-1
#define	EXCLUDE_THEM		1


#endif


/*
**	History:
**	$Log: rlmg.h,v $
**	Revision 1.3  1996/09/17 23:34:16  gsl
**	drcs update
**	
**
**
*/
