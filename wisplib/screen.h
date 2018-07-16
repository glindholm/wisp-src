/*
******************************************************************************
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
******************************************************************************
*/

/*
**	File:		screen.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Header for screen.c
**
*/

#ifndef screen_H
#define screen_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

#define WANG_TYPE	0
#define VIDEO_TYPE	1

/*
**	Function Prototypes
*/

void WL_strip_facs(		/*  with a space.			*/
	char *string_addr,	/* Address of string to be stripped.	*/
	int string_len,		/* Length of that string.		*/
	int type);		/* 0=Wang screen map (with FACs & 0x0b), 1=vchr_map */
int WL_di_write_file(			/* Open up a printer output file.	*/
	char *text,			/* Pointer to the stuff to be printed.	*/
	int  text_len,			/* Length values.			*/
	int  rec_len,			/* Length values.			*/
	char *filelibvol, 		/* Pointer to the file to be opened.	*/
	char *def_filename);		/* Pointer to the default file name.	*/
void WL_border_screen(
	char *work_area,   	/* Address of destination.		*/
	char *screen_image,	/* Address of screen image.		*/
	int  image_rec_size,	/* Size of the image on the screen.	*/
	int  screen_rec_size,	/* The size of the screen records.	*/
	int  screen_rec_count);	/* The number of records. (screen rows)	*/
void WL_screen_print(void);

#endif /* screen_H */

/*
**	History:
**	$Log: screen.h,v $
**	Revision 1.5  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.4  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.3  2003/01/20 17:12:10  gsl
**	Fix SCREEN prototype
**	
**	Revision 1.2  2002/07/10 21:05:24  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.1  1997/12/18 14:04:07  gsl
**	Initial revision
**	
**
**
**
**
*/
