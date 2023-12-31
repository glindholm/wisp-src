/* filetbl - table of available file systems */
/*  $Id: filetbl.c 72641 2017-12-22 17:24:40Z mark $  */

/*
** Copyright (C) 1991-1998,2000-2004,2006-2008,2010,2013-2017 Micro Focus.
** All rights reserved.
*/

/* This file gets modified to indicate which host file system(s) are	*/
/* being used to manage indexed files.  Selecting any file system other	*/
/* than Vision (or RMS for VAX systems) requires the appropriate file	*/
/* system interface from ACUCOBOL.  Instructions are provided with	*/
/* those interfaces on how to use this file.  */

#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/filetbl.c"
const char what_lib_filetbl_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date: 2017-12-22 17:24:40 +0000 (Fri, 22 Dec 2017) $$Rev: 72641 $";

#include <stdio.h>

/*  On windows, the following interfaces are DLLs.  If the DLL is not  */
/*  found at runtime, then the file system is automatically removed    */
/*  from the list of available file systems.  If you want to, you can  */
/*  slightly speed up initial loading by relinking your runtime after  */
/*  setting any of the following to 0 (which will cause the runtime to */
/*  ignore any DLL for that file system.)  */
#ifdef	_MSC_VER
#define	USE_BTRIEVE	1
#define	USE_DBMAKER	1
#define	USE_SYBASE	1
#define	USE_XML		1
#define	USE_RMFM	1
#define	USE_WIN_SPOOL	1
#define	USE_PDF		1
#endif	/* _MSC_VER */

/* On all systems, the following interfaces are callable shared		*/
/* libraries.  If the library is not found at runtime, then the file	*/
/* system is automatically removed from the list of available file	*/
/* systems.  See the comment above about DLLs for info for speeding	*/
/* up runtime initialization.  */
#define	USE_DB2		1
#define	USE_MSSQL	1
#define	USE_ORACLE	1
#define	USE_ODBC	1

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
#define	USE_DB2		0
#endif	/* USE_DB2 */
#ifndef	USE_DBMAKER
#define	USE_DBMAKER	0
#endif	/* USE_DBMAKER */
#ifndef	USE_WIN_SPOOL
#define	USE_WIN_SPOOL	1
#endif	/* USE_WIN_SPOOL */
#ifndef	USE_PDF
#define	USE_PDF	1
#endif	/* USE_PDF */
#ifndef	USE_MPE
#define USE_MPE		0
#endif	/* USE_MPE */
#ifndef	USE_XML
#define	USE_XML		0
#endif	/* USE_XML */
#ifndef	USE_RMFM
#define	USE_RMFM	0
#endif	/* USE_RMFM */
#if     !defined(O_VMS)
#ifndef	USE_EXTFH
#define	USE_EXTFH	1
#endif	/* USE_EXTFH */
#endif	/* O_VMS */

typedef	long		DISPATCH_TBL;
typedef	DISPATCH_TBL	*TBLPTR;

typedef	struct _tbl_ent {
	TBLPTR		dispatch;
	char		name[6];
	char		defer_init;	/* Set to 1 to defer initialization */
} TABLE_ENTRY;

extern	DISPATCH_TBL	v6_dispatch, ci_dispatch, bt_dispatch;
extern	DISPATCH_TBL	rms_dispatch, ifx_dispatch;
extern	DISPATCH_TBL	ora_dispatch, syb_dispatch, ms_dispatch;
extern	DISPATCH_TBL	odbc_dispatch, a4db2_dispatch, dbm_dispatch;
extern	DISPATCH_TBL	ws_dispatch, mpe_dispatch, xml_dispatch;
extern	DISPATCH_TBL	mf_dispatch, rmfm_dispatch, pdf_dispatch;

TABLE_ENTRY	file_table[] = {
#if	USE_VISION
	{	&v6_dispatch,	"VISIO", 0	},
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
#if	USE_PDF
	{	&pdf_dispatch,	"PDF", 0	},
#endif	/* USE_PDF */
#if	USE_MPE
	{	&mpe_dispatch,	"MPE", 0	},
#endif	/* USE_MPE */
#if	USE_XML
	{	&xml_dispatch,	"XML", 0	},
#endif	/* USE_XML */
#if	USE_RMFM
	{	&rmfm_dispatch,	"RMFM", 0	},
#endif	/* USE_RMFM */
#if	USE_EXTFH
	{	&mf_dispatch,	"EXTFH", 1	},
#endif	/* USE_EXTFH */
	{	NULL,		"\0", 0	}
};

short	num_file_systems = ( sizeof(file_table) / sizeof(TABLE_ENTRY) ) - 1;

/* */
