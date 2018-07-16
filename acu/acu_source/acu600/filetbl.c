/* filetbl - table of available file systems */
/*  $Id:$  */

/* Copyright notice: Copyright (c) 1991-2003, Acucorp, Inc. */

/* This file gets modified to indicate which host file system(s) are 	*/
/* being used to manage indexed files.  Selecting any file system other	*/
/* than Vision (or RMS for VAX systems) requires the appropriate file	*/
/* system interface from ACUCOBOL.  Instructions are provided with 	*/
/* those interfaces on how to use this file.  */

#include <stdio.h>

/*  On windows, the following interfaces are DLLs.  If the DLL is not  */
/*  found at runtime, then the file system is automatically removed    */
/*  from the list of available file systems.  If you want to, you can  */
/*  slightly speed up initial loading by relinking your runtime after  */
/*  setting any of the following to 0 (which will cause the runtime to */
/*  ignore any DLL for that file system.)  */
#ifdef	_WINDOWS
#define	USE_BTRIEVE	1
#define	USE_DBMAKER	1
#define	USE_DB2		1
#define	USE_MSSQL	1
#define	USE_ODBC	1
#define	USE_ORACLE	1
#define	USE_SYBASE	1
#define	USE_XML		1
#endif	/* _WINDOWS */

#ifndef	USE_VISION
#define	USE_VISION	1
#endif	/* USE_VISION */
#ifndef	USE_RMS
#define	USE_RMS		0
#endif	/* USE_RMS */
#ifndef	USE_CISAM
#define	USE_CISAM	0
#endif	/* USE_CISAM */
#ifndef	USE_BTRIEVE
#define	USE_BTRIEVE	0
#endif	/* USE_BTRIEVE */
#ifndef	USE_INFORMIX
#define	USE_INFORMIX	0
#endif	/* USE_INFORMIX */
#ifndef	USE_ORACLE
#define	USE_ORACLE	0
#endif	/* USE_ORACLE */
#ifndef	USE_SYBASE
#define	USE_SYBASE	0
#endif	/* USE_SYBASE */
#ifndef	USE_MSSQL
#define	USE_MSSQL	0
#endif	/* USE_MSSQL */
#ifndef	USE_ODBC
#define	USE_ODBC	0
#endif	/* USE_ODBC */
#ifndef	USE_DB2
#define	USE_DB2	0
#endif	/* USE_DB2 */
#ifndef	USE_DBMAKER
#define	USE_DBMAKER	0
#endif	/* USE_DBMAKER */
#ifndef	USE_WIN_SPOOL
#define	USE_WIN_SPOOL	1
#endif	/* USE_WIN_SPOOL */
#ifndef	USE_KSAM
#define USE_KSAM	0
#endif	/* USE_KSAM */
#ifndef	USE_XML
#define	USE_XML		0
#endif	/* USE_XML */

typedef	long		DISPATCH_TBL;
typedef	DISPATCH_TBL	*TBLPTR;

typedef	struct _tbl_ent {
	TBLPTR		dispatch;
	char		name[6];
} TABLE_ENTRY;

extern	DISPATCH_TBL	v5_dispatch, ci_dispatch, bt_dispatch;
extern	DISPATCH_TBL	rms_dispatch, ifx_dispatch;
extern	DISPATCH_TBL	ora_dispatch, syb_dispatch, ms_dispatch;
extern	DISPATCH_TBL	odbc_dispatch, a4db2_dispatch, dbm_dispatch;
extern	DISPATCH_TBL	ws_dispatch, k_dispatch, xml_dispatch;

TABLE_ENTRY	file_table[] = {
#if	USE_VISION
	{	&v5_dispatch,	"VISIO"	},
#endif	/* USE_VISION */
#if	USE_RMS
	{	&rms_dispatch,	"RMS"	},
#endif	/* USE_RMS */
#if	USE_CISAM
	{	&ci_dispatch,	"C-ISA"	},
#endif	/* USE_CISAM */
#if	USE_BTRIEVE
	{	&bt_dispatch,	"BTRIE"	},
#endif	/* USE_BTRIEVE */
#if	USE_INFORMIX
	{	&ifx_dispatch,	"INFOR"	},
#endif	/* USE_INFORMIX */
#if	USE_ORACLE
	{	&ora_dispatch,	"ORACL"	},
#endif	/* USE_ORACLE */
#if	USE_SYBASE
	{	&syb_dispatch,	"SYBAS"	},
#endif	/* USE_SYBASE */
#if	USE_MSSQL
	{	&ms_dispatch,	"MSSQL"	},
#endif	/* USE_MSSQL */
#if	USE_ODBC
	{	&odbc_dispatch,	"ODBC"	},
#endif	/* USE_ODBC */
#if	USE_DB2
	{	&a4db2_dispatch,	"DB2"	},
#endif	/* USE_DB2 */
#if	USE_DBMAKER
	{	&dbm_dispatch,	"DBMAK"	},
#endif	/* USE_DBMAKER */
#if	USE_WIN_SPOOL
	{	&ws_dispatch,	"WSPL"	},
#endif	/* USE_WIN_SPOOL */
#if	USE_KSAM
	{	&k_dispatch,	"KSAM"	},
#endif	/* USE_KSAM */
#if	USE_XML
	{	&xml_dispatch,	"XML"	},
#endif	/* USE_XML */
	{	NULL,		"\0"	}
};

short	num_file_systems = ( sizeof(file_table) / sizeof(TABLE_ENTRY) ) - 1;

/* */
