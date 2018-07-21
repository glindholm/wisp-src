/*
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
*/


/*
	findcase:	This routine finds the correct case for a given filepath.
			The o_name is the original filepath, c_name is the actual path as found on disk by changing
			the case of file and library. The volume portion and the optional extention is never varyied.
			Return 0=found 1=not found. If not found then c_name is unchanged.


			o_name:		volume/library/file[.ext]

			c_name:		volume/library/file[.ext]		lowercase lib and file
					volume/library/FILE[.ext]		lowercase lib and uppercase file
					volume/LIBRARY/FILE[.ext]		uppercase lib and file
					volume/LIBRARY/file[.ext]		uppercase lib and lowercase file
*/

#include <string.h>
#include <stdio.h>
#include "idsistd.h"
#include "wisplib.h"
#include "idsisubs.h"

#ifdef unix

int WL_findcase(char* o_name, char* c_name)
{
	char	buff[128], volume[80], library[20], file[20], ext[45], *ptr;
	int	i;

	strcpy(buff,o_name);

	volume[0]  = '\0';
	library[0] = '\0';
	file[0]	   = '\0';
	ext[0]     = '\0';

	if ((ptr = strrchr(buff,'/')))
	{
		*ptr++ = '\0';
		strcpy(file,ptr);

		if ((ptr = strrchr(buff,'/')))
		{
			*ptr++ = '\0';
			strcpy(library,ptr);
			strcat(library,"/");
			strcpy(volume,buff);
			strcat(volume,"/");
		}
		else
		{
			strcpy(library,buff);
			strcat(library,"/");
		}
	}
	else
	{
		strcpy(file,buff);
	}

	if ((ptr = strchr(file,'.')))
	{
		strcpy(ext,ptr);
		*ptr = '\0';
	}

	for(i=0;i<4;i++)
	{
		switch(i)
		{
		case 0:
			WL_lower_string(file);						/* library file				*/
			WL_lower_string(library);
			break;
		case 1:
			WL_upper_string(file);						/* library FILE				*/
			break;
		case 2:
			WL_upper_string(library);						/* LIBRARY FILE				*/
			break;
		case 3:
			WL_upper_string(file);						/* LIBRARY file				*/
			break;
		}

		sprintf(buff,"%s%s%s%s",volume,library,file,ext);
		if ( fexists(buff) )
		{
			strcpy(c_name,buff);
			return(0);  /* FOUND */
		}
	}
	return(1); /* Not found */
}

#else

/* 
	On WIN32 the filepaths are not case sensitive so just check access.
*/

int WL_findcase(char* o_name, char* c_name)
{
	if ( fexists(o_name) )
	{
		strcpy(c_name,o_name);
		return(0);  /* FOUND */
	}
	return(1); /* Not found */
}

#endif


/*
**	History:
**	$Log: findcase.c,v $
**	Revision 1.15  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.14  2003/01/31 17:33:56  gsl
**	Fix  copyright header
**	
**	Revision 1.13  2003/01/29 19:42:50  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2002/07/25 17:03:44  gsl
**	MSFS->WIN32
**	
**	Revision 1.11  2002/07/11 20:29:08  gsl
**	Fix WL_ globals
**	
**	Revision 1.10  2002/07/11 14:33:58  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.9  1996/08/19 22:32:21  gsl
**	drcs update
**	
**
**
*/
