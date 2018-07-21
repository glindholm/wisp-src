/* 
	Copyright (c) 1998 NeoMedia Technologies, All rights reserved.
	$Id:$
*/

/*
**	File:		wfvision.h
**
**	Project:	WISP/LIB
**
**
**
**	Purpose:	Vision file routines
**
*/

#ifndef wfvision_H
#define wfvision_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

#define VISION_MAGIC_LEN 	6
#define VISION2BE_MAGIC 	"\x10\x12\x14\x16\x00\x02"	/* Big Endian */
#define VISION2LE_MAGIC 	"\x16\x14\x12\x10\x02\x00"	/* Little Endian */
#define VISION3_MAGIC 		"\x10\x12\x14\x16\x00\x03"
#define VISION4I_MAGIC 		"\x10\x12\x14\x18\x00\x04"
#define VISION4D_MAGIC 		"\x10\x12\x14\x19\x00\x04"
#define VISION5I_MAGIC 		"\x10\x12\x14\x18\x00\x05"
#define VISION5D_MAGIC 		"\x10\x12\x14\x19\x00\x05"

/*
**	Function Prototypes
*/
int WL_visioninfo( const char* path, const char* code, void* raw_field );
int WL_visioninfo_header( const char* path, const char* code, void* raw_field, const char* raw_header, int header_len );

int WL_unloadvision(const char *inname, const char *outname);

#endif /* wfvision_H */

/*
**	History:
**	$Log: wfvision.h,v $
**	Revision 1.5  2003/05/28 18:07:14  gsl
**	Acucobol 6.0, Vision 5 support
**	
**	Revision 1.4  2002/07/12 19:10:20  gsl
**	Global unique WL_ changes
**	
**	Revision 1.3  2001/11/12 21:26:50  gsl
**	VISION2 has 2 magic numbers (Big Endian & Little Endian)
**	
**	Revision 1.2  2001-10-30 15:20:53-05  gsl
**	Add defines for VISION magic numbers
**	rename to visioninfo()
**	move unloadvision() from wispsort
**
**	Revision 1.1  1998-05-14 14:29:31-04  gsl
**	Initial revision
**
**
**
**
*/
