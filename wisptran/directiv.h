/*
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
*/


/*
**	File:		directiv.h
**
**	Purpose:	WISP directives globals
**
**
**	History:
**	05/27/93	Written by GSL
**
*/

#ifndef DIRECTIV_H
#define DIRECTIV_H

EXT int range_count	INIT_ZERO;			/* $RANGE_COUNT			The RANGE clause table size		*/
EXT int isaproc		INIT_FALSE;			/* $WANG_PROC			Is the next OPEN for a procedure?	*/
EXT int autolockprint	INIT_TRUE;			/* $AUTOLOCKPRINT		VMS use automatic locking for print	*/
EXT int autolockfile	INIT_FALSE;			/* $AUTOLOCKFILE		VMS use automatic locking next file	*/
EXT int noautolockfile	INIT_FALSE;			/* $NOAUTOLOCKFILE		VMS don't automatic locking next file	*/
EXT int compressfile	INIT_FALSE;			/* $COMPRESSFILE		Add file compression.			*/
EXT int multiplelock	INIT_FALSE;			/* $MULTIPLELOCK		Flag to add WITH LOCK ON MULTIPLE	*/
EXT int nooptional	INIT_FALSE;			/* $NOOPTIONAL			Don't add OPTIONAL clause.		*/
EXT int seqline		INIT_FALSE;			/* $SEQLINE			Force to LINE SEQUENTIAL		*/
EXT int seqbinary	INIT_FALSE;			/* $SEQBINARY/$SEQRECORD	Force to BINARY SEQUENTIAL		*/
EXT int sortfile	INIT_FALSE;			/* $SORTFILE/$SORT_FILE		Next file is a SORT file		*/
EXT char selecttext[400];				/* $SELECTTEXT			Text to add to the SELECT		*/
EXT int dbfile		INIT_FALSE;			/* $DBFILE [table]		Next file is a DATABASE file		*/
EXT char dbfile_tabname[80];				/* 				The table name from $DBFILE		*/
EXT int openioxfile	INIT_FALSE;			/* $OPENIOEXCLUSIVEFILE		Next file is openiox			*/
EXT int openioxall	INIT_FALSE;			/* $OPENIOEXCLUSIVEALL		All files are openiox			*/



#endif /* DIRECTIV_H */
/*
**	History:
**	$Log: directiv.h,v $
**	Revision 1.8  2003/02/04 17:33:20  gsl
**	fix copyright header
**	
**	Revision 1.7  2002/06/20 22:53:15  gsl
**	remove obsolete code
**	
**	Revision 1.6  1996/08/31 01:56:03  gsl
**	drcs update
**	
**
**
*/
