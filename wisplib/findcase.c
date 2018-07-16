			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

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

#ifdef unix

char *strrchr();
char *strchr();

int findcase(o_name, c_name)
char	*o_name, *c_name;
{
	char	buff[128], volume[80], library[20], file[20], ext[45], *ptr;
	int	i;

	strcpy(buff,o_name);

	volume[0]  = '\0';
	library[0] = '\0';
	file[0]	   = '\0';
	ext[0]     = '\0';

	if (ptr = strrchr(buff,'/'))
	{
		*ptr++ = '\0';
		strcpy(file,ptr);

		if (ptr = strrchr(buff,'/'))
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

	if (ptr = strchr(file,'.'))
	{
		strcpy(ext,ptr);
		*ptr = '\0';
	}

	for(i=0;i<4;i++)
	{
		switch(i)
		{
		case 0:
			lower_string(file);						/* library file				*/
			lower_string(library);
			break;
		case 1:
			upper_string(file);						/* library FILE				*/
			break;
		case 2:
			upper_string(library);						/* LIBRARY FILE				*/
			break;
		case 3:
			lower_string(file);						/* LIBRARY file				*/
			break;
		}

		sprintf(buff,"%s%s%s%s",volume,library,file,ext);
		if ( 0 == access(buff,0) )
		{
			strcpy(c_name,buff);
			return(0);  /* FOUND */
		}
	}
	return(1); /* Not found */
}

#else

/* 
	On VMS and MSDOS the filepaths are not case sensitive so just check access.
*/

int findcase(o_name, c_name)
char	*o_name, *c_name;
{
	if ( 0 == access(o_name,0) )
	{
		strcpy(c_name,o_name);
		return(0);  /* FOUND */
	}
	return(1); /* Not found */
}

#endif


