			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/
#ifndef WCOMMON_INCLUDED
#define WCOMMON_INCLUDED

/*
	WCOMMON.H		Define common defines for WISP and WISPLIB
*/

#define WISP_VERSION "V3.3c"			 					/* WISPTRAN version (char[20])		*/
#define LIBRARY_VERSION	30								/* WISPLIB version number.		*/
#define SCREEN_VERSION  22								/* SCREEN version number.		*/

/* 
**	NOTE: Screen version 20 and 21 are still supported.
**
** SCREEN_VERSION 20	- DISP-ITEM-LENGTH was PIC S9(4) binary.
** SCREEN_VERSION 21	- DISP-ITEM-LENGTH was PIC X, and compression greatly improved.
** SCREEN_VERSION 22	- New screen structure.
*/


#define IS_OUTPUT	0x00000001							/* output only file			*/
#define IS_PRINTFILE	0x00000002							/* printerfile				*/
#define IS_SCRATCH	0x00000004							/* scratch file				*/
#define IS_SUBMIT       0x00000008							/* This file will be submitted.		*/
#define IS_IO		0x00000010							/* This file is opened IO.		*/
#define IS_SORT		0x00000020							/* This file is s SORT file.		*/
#define IS_LIB		0x00000040							/* Generate a LIB only.			*/
#define IS_NORESPECIFY	0x00000080							/* already done				*/
#define IS_INDEXED	0x00000100							/* an indexed file			*/
#define IS_CASE_SEN	0x00000200							/* name is case sensitive don't change	*/
#define IS_NOWRITE	0x00000400							/* File is opened allowing no writers.	*/
#define IS_PRNAME	0x00000800							/* Its a call with a prname ref.	*/
#define IS_BACKFILL	0x00001000							/* Backfill file/lib/vol.		*/
#define IS_SEQSEQ	0x00002000							/* It's a Sequential/Sequential file.	*/
#define IS_GETPARM	0x00004000							/* Has the initial hidden GP been done?	*/
#define IS_NODISPLAY	0x00008000							/* Select file "NODISPLAY".		*/
#define IS_SEQDYN	0x00010000							/* File is SEQ/DYN without relative key.*/
#define IS_WORK		0x00020000							/* A WORK file.				*/
#define IS_TEMP		0x00040000							/* A temporary ## file			*/
#define IS_ERROR	0x00080000							/* Set to force a GETPARM.		*/
#define IS_DECLARE	0x00100000							/* There are DECLARITIVES for this file */
#define IS_EXTEND	0x00200000							/* File opened EXTEND.			*/
#define IS_NOEXTENSION	0x00400000							/* A file without an extension.		*/
#define IS_DBFILE	0x00800000							/* A DATABASE file			*/

#define		OPEN_INPUT		1
#define		OPEN_SHARED		2
#define		OPEN_OUTPUT		3
#define		OPEN_EXTEND		4
#define		OPEN_SPECIAL_INPUT	5
#define		OPEN_I_O		6
#define		OPEN_SORT		7

#endif	/* WCOMMON_INCLUDED */
