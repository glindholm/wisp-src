/* filetbl - table of available file systems */
/*  $Id: filetbl.c 45102 2006-01-12 19:46:07Z merge $  */

/* Copyright notice: Copyright (c) 1991-2006, Acucorp, Inc. */

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
#define	USE_OLEDB	1
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
#ifndef	USE_MPE
#define USE_MPE	0
#endif	/* USE_MPE */
#ifndef	USE_XML
#define	USE_XML		0
#endif	/* USE_XML */
#if     !defined(O_VMS) && !defined(HP3000)
#ifndef	USE_EXTFH
#define	USE_EXTFH	1
#endif	/* USE_EXTFH */
#endif	/* O_VMS && HP3000 */
#ifndef	USE_OLEDB
#define	USE_OLEDB	0
#endif	/* USE_OLEDB */

typedef	long		DISPATCH_TBL;
typedef	DISPATCH_TBL	*TBLPTR;

typedef	struct _tbl_ent {
	TBLPTR		dispatch;
	char		name[6];
	char		defer_init;	/* Set to 1 to defer initialization */
} TABLE_ENTRY;

extern	DISPATCH_TBL	v5_dispatch, ci_dispatch, bt_dispatch;
extern	DISPATCH_TBL	rms_dispatch, ifx_dispatch;
extern	DISPATCH_TBL	ora_dispatch, syb_dispatch, ms_dispatch;
extern	DISPATCH_TBL	odbc_dispatch, a4db2_dispatch, dbm_dispatch;
extern	DISPATCH_TBL	ws_dispatch, mpe_dispatch, xml_dispatch;
extern	DISPATCH_TBL	mf_dispatch, oledb_dispatch;

TABLE_ENTRY	file_table[] = {
#if	USE_VISION
	{	&v5_dispatch,	"VISIO", 0	},
#endif	/* USE_VISION */
#if	USE_RMS
	{	&rms_dispatch,	"RMS", 0	},
#endif	/* USE_RMS */
#if	USE_CISAM
	{	&ci_dispatch,	"C-ISA", 0	},
#endif	/* USE_CISAM */
#if	USE_BTRIEVE
	{	&bt_dispatch,	"BTRIE", 0	},
#endif	/* USE_BTRIEVE */
#if	USE_INFORMIX
	{	&ifx_dispatch,	"INFOR", 0	},
#endif	/* USE_INFORMIX */
#if	USE_ORACLE
	{	&ora_dispatch,	"ORACL", 0	},
#endif	/* USE_ORACLE */
#if	USE_SYBASE
	{	&syb_dispatch,	"SYBAS", 0	},
#endif	/* USE_SYBASE */
#if	USE_MSSQL
	{	&ms_dispatch,	"MSSQL", 0	},
#endif	/* USE_MSSQL */
#if	USE_ODBC
	{	&odbc_dispatch,	"ODBC", 0	},
#endif	/* USE_ODBC */
#if	USE_DB2
	{	&a4db2_dispatch,	"DB2", 0	},
#endif	/* USE_DB2 */
#if	USE_DBMAKER
	{	&dbm_dispatch,	"DBMAK", 0	},
#endif	/* USE_DBMAKER */
#if	USE_WIN_SPOOL
	{	&ws_dispatch,	"WSPL", 0	},
#endif	/* USE_WIN_SPOOL */
#if	USE_MPE
	{	&mpe_dispatch,	"MPE", 0	},
#endif	/* USE_MPE */
#if	USE_XML
	{	&xml_dispatch,	"XML", 0	},
#endif	/* USE_XML */
#if	USE_EXTFH
	{	&mf_dispatch,	"EXTFH", 1	},
#endif	/* USE_EXTFH */
#if	USE_OLEDB
	{	&oledb_dispatch,	"OLEDB", 0	},
#endif	/* USE_OLEDB */
	{	NULL,		"\0", 0	}
};

short	num_file_systems = ( sizeof(file_table) / sizeof(TABLE_ENTRY) ) - 1;

/* */
