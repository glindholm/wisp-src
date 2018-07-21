/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*			Copyright (c) 1988, 1989, 1990			*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

#ifndef VLOCAL_H
#define VLOCAL_H

/*			Local (Internal) Header File			*/

#define TAG_AS_OLD		 1	/* vmap to tag as old data.	*/
#define SCROLL_UP		 2	/* vmap to scroll the map up.	*/
#define SCROLL_DOWN		 3	/* vmap to scroll the map down.	*/

#define REFRESH_CHARACTER    '\027'	/* Refresh screen on control W.	*/
#define TRIGGER_CHARACTER    '\002'	/* Trigger on control B.	*/

#define DOUBLE_TOP		 1	/* Top of double high line.	*/
#define DOUBLE_BOTTOM		 2	/* Bottom of double high line.	*/
#define PRINT_BUFFER_SIZE      256	/* Maximum print buffer size.	*/
#define MAP_SIZE	      3168	/* Bytes in a screen map.	*/

/* save_screen - A structure to contain saved addresses and variables	*/
/*		 of the VIDEO screen database.				*/

struct save_screen 
{ 
	char *xchr_map; 
	char *xatr_map; 
	int xcur_lin;
	int xcur_col; 
	int xcur_atr; 
	int xchr_set;
	int *xcur_set; 
	int xscr_atr;
	int xrol_top; 
	int xrol_bot; 
	int xmap_top;
	struct save_screen *prev_ptr; 
};


/************************************************************************/
/*		Internal forms error codes and parameters.		*/
/************************************************************************/

#ifdef unix
#define VIDEOINFODIR 	"/usr/local/lib/videoinfo"
#endif
#ifdef WIN32
#define VIDEOINFODIR 	"C:\\video"
#endif

#endif /* VLOCAL_H */

/*
**	History:
**	$Log: vlocal.h,v $
**	Revision 1.15  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.14  2002/07/25 17:03:43  gsl
**	MSFS->WIN32
**	
**	Revision 1.13  1997/07/08 21:15:01  gsl
**	removed unused items
**	
**	Revision 1.12  1997-05-21 13:52:27-04  gsl
**	Removed a lot of old obsolete defines plus move some to vdata.h
**
**	Revision 1.11  1996-10-11 18:16:10-04  gsl
**	drcs update
**
**
**
*/
