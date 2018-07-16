/* 
	Copyright (c) 1997 NeoMedia Technologies, All rights reserved.
	$Id:$
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

void SCREEN();
void strip_facs(		/*  with a space.			*/
	char *string_addr,	/* Address of string to be stripped.	*/
	int string_len,		/* Length of that string.		*/
	int type);		/* 0=Wang screen map (with FACs & 0x0b), 1=vchr_map */
int di_write_file(			/* Open up a printer output file.	*/
	char *text,			/* Pointer to the stuff to be printed.	*/
	int  text_len,			/* Length values.			*/
	int  rec_len,			/* Length values.			*/
	char *filelibvol, 		/* Pointer to the file to be opened.	*/
	char *def_filename);		/* Pointer to the default file name.	*/
void border_screen(
	char *work_area,   	/* Address of destination.		*/
	char *screen_image,	/* Address of screen image.		*/
	int  image_rec_size,	/* Size of the image on the screen.	*/
	int  screen_rec_size,	/* The size of the screen records.	*/
	int  screen_rec_count);	/* The number of records. (screen rows)	*/
void screen_print(void);

#endif /* screen_H */

/*
**	History:
**	$Log: screen.h,v $
**	Revision 1.1  1997-12-18 09:04:07-05  gsl
**	Initial revision
**
**
**
**
**
*/
