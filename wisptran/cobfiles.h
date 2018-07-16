/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
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
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/*
**	File:		cobfiles.h
**
**	Purpose:	To defines items needed for WISP to process COBOL files.
**
**
**	History:
**	05/07/93	Split these apart from wisp.h. GSL
**
*/

#ifndef COBFILES_H
#define COBFILES_H

											/* These are used by prog_ftypes.	*/
#define DISK_FILE		0x0001							/* The file is a normal disk file	*/
#define PRINTER_FILE		0x0002							/* The file is a printer file		*/
#define SORT_FILE		0x0004							/* The file is a SORT file		*/
#define HAS_DECLARATIVES	0x0008							/* The file has a DECLARATIVES section	*/
#define NORESPECIFY		0x0010							/* The file has a NORESPECIFY option.	*/
#define INDEXED_FILE		0x0020							/* The file is indexed.			*/
#define TAPE_FILE		0x0040							/* The file is a tape file.		*/
#define SEQ_DYN			0x0080							/* The file was SEQUENTIAL/DYNAMIC.	*/
#define SEQ_FILE		0x0100							/* The file is SEQUENTIAL.		*/
#define HAD_FD			0X0200							/* The file had an FD statement.	*/
#define AUTOLOCK		0X0400							/* The file to use automatic locking.	*/
#define SEQ_SEQ			0x0800							/* The file was SEQUENTIAL/SEQUENTIAL.	*/
#define DBFILE_FILE		0x1000							/* The file is a DATABASE file		*/
#define OPENIOX_FILE		0x2000							/* OPEN I-O exclusive			*/
#define RELATIVE_FILE		0x4000							/* The file is RELATIVE.		*/
#define FILE_MASK		~(DISK_FILE + PRINTER_FILE + SORT_FILE + TAPE_FILE)	/* The mask used to remove file bits.	*/

#define FSET(F,V) 		(F = (F & FILE_MASK) | V)				/* Macro to set a file value.		*/

#define MAX_FILES		250							/* The maximum number of files		*/

EXT int  prog_ftypes[MAX_FILES];							/* the file type flags			*/
EXT char prog_files[MAX_FILES][40];							/* the program files in SELECT's	*/
EXT char prog_vnames[MAX_FILES][40];							/* the program file volume names	*/
EXT char prog_lnames[MAX_FILES][40];							/* the program file library names	*/
EXT char prog_fnames[MAX_FILES][40];							/* the program file names in SELECT's	*/
EXT char prog_fstats[MAX_FILES][40];							/* the program file status fields	*/
EXT char prog_prname[MAX_FILES][40];							/* The prnames.				*/
EXT char prog_dsel[MAX_FILES][40];							/* The files to delete their SELECT's.	*/
EXT int prog_ref[MAX_FILES];								/* Were they actually opened?		*/

EXT int  prog_cnt INIT_FALSE;								/* the count of them			*/
EXT int  prog_sort INIT_FALSE;								/* how many were sort files		*/
EXT int  prog_dscnt INIT_FALSE;								/* How many need their SELECT deleted.	*/

#endif /* COBFILES_H */
/*
**	History:
**	$Log: cobfiles.h,v $
**	Revision 1.8  2003/03/06 21:30:44  gsl
**	Add RELATIVE file flag
**	
**	Revision 1.7  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.6  1996/08/31 01:56:01  gsl
**	drcs update
**	
**
**
*/
