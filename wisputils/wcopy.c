			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*	Copy a file from the unix shell					*/
/*									*/
/*      wcopy oldfile oldlib oldvol newfile newlib newvol		*/
/*	wcopy LIBRARY oldlib oldvol newlib newvol			*/

#ifdef unix

#include <stdio.h>

#define EXT_FILEXT
#include "filext.h"
#include "wcommon.h"

char *wfname();

main(argc,argv)
int	argc;
char	*argv[];
{
	int	retcode;
	char	key[20], type[1];
	char	oldfile[20], oldlib[20], oldvol[20];
	char	newfile[20], newlib[20], newvol[20];

	initglbs("WCOPY   ");

	if (argc < 6) badusage();

	strcpy( key, argv[1] );
	upper_string( key );

	if ( 0 == strcmp( "LIBRARY", key ) )
	{
		if (argc != 6) badusage();
		strcpy(oldfile, "        ");
		strcpy(oldlib,  argv[2]);
		strcpy(oldvol,  argv[3]);
		strcpy(newfile, "        ");
		strcpy(newlib,  argv[4]);
		strcpy(newvol,  argv[5]);
		type[0] = 'L';
	}
	else
	{
		if (argc != 7) badusage();
		strcpy(oldfile, argv[1]);
		strcpy(oldlib,  argv[2]);
		strcpy(oldvol,  argv[3]);
		strcpy(newfile, argv[4]);
		strcpy(newlib,  argv[5]);
		strcpy(newvol,  argv[6]);
		type[0] = 'F';
	}
	 
	if ( strlen(oldfile)>8 ||
	     strlen(oldlib)>8  ||
	     strlen(oldvol)>6  ||
	     strlen(newvol)>6  ||
	     strlen(newfile)>8 ||
	     strlen(newlib)>8   ) badusage();
 
	retcode = 0;

	copy(type, oldfile, oldlib, oldvol,
                   newfile, newlib, newvol, &retcode);

	exit(retcode);
}

copy(type,oldfile,oldlib,oldvol,newfile,newlib,newvol,status)
char	*type;
char	*oldfile, *oldlib, *oldvol, *newfile, *newlib, *newvol;
int	*status;
{
	char old_filename[132], new_filename[132];					/* Strings to contain the filenames.	*/
	char libpath[80];
	long mode, savemode;
	char *name_end;
	char *strchr();									/* return pointer to char in string	*/
	char cmd[100];									/* buffer to hold cmd string 		*/
	int found;

	*status = 0;

	mode = 0;
	if ( *type == 'L' ) mode |= IS_LIB;						/* Doing a library copy.		*/
	savemode = mode;

	name_end = wfname(&mode, oldvol, oldlib, oldfile, old_filename);		/* Construct the old filename.		*/
	*name_end = '\0';
	if ( *(--name_end) == '/' ) *name_end = '\0';					/* separator not needed			*/

	mode = savemode;
	
	name_end = wfname(&mode, newvol, newlib, newfile, new_filename);		/* Construct the new filename.		*/
	*name_end = '\0';
	if ( *(--name_end) == '/' ) *name_end = '\0';					/* separator not needed			*/

	strcpy(libpath,new_filename);
	if ( *type != 'L')
	{
		int x;

		for( x=strlen(libpath)-1; x>=0 && libpath[x]!='/'; x--);
		if (x>=0) libpath[x] = '\0'; 
		else      libpath[0] = '\0';		
	}



	if ( 0!=access(libpath,0) )							/* If new_lib doesn't exist.		*/
	{
		if(mkdir(libpath,0777))							/* Create the new lib.			*/
		{
			*status=24;
			return;
		}
	}

	if ( *type == 'L' )
	{
		sprintf(cmd,"cp %s/* %s >/dev/null 2>&1",old_filename,new_filename);
		if (system(cmd)) 
		{
			*status=20;
		}
		return;
	}

	found = 0;

	if (0==access(old_filename,0))							/* file does exist in this form	*/
	{
		if (0==access(new_filename,0))						/* New file already exists.		*/
		{
			*status = 52;
			return;
		}

		sprintf(cmd,"cp %s %s >/dev/null 2>&1",old_filename,new_filename);
		if (system(cmd)) 
		{
			*status=24;
			return;
		}
		found = 1;
	}
	else
	{
		if (strchr(old_filename,'.')) 						/* does it already have extension?	*/
		{
			*status=20;							/* yes, so return file not found	*/
			return;
		}
	}

	strcat(old_filename,".idx");							/* else try it with a .idx extension	*/
	strcat(new_filename,".idx");
	if (0 == access(old_filename,0))						/* idx found				*/
	{

		if (0==access(new_filename,0))						/* Does new file already exists?	*/
		{
			*status = 52;
			return;
		}

		sprintf(cmd,"cp %s %s >/dev/null 2>&1",old_filename,new_filename);	/* build the copy cmd, direct all output*/
		if (system(cmd))							/* to /dev/null.  if system() returns	*/
		{									/* non-zero, gen an error		*/
			*status=24;
			return;
		}
		found = 1;
	}

	strcpy(strchr(old_filename,'.'),".dat");					/* replace the '.idx' with a '.dat'	*/
	strcpy(strchr(new_filename,'.'),".dat");
	if (0 == access(old_filename,0))						/* dat found				*/
	{
		if (0==access(new_filename,0))						/* Does new file already exists?	*/
		{
			*status = 52;
			return;
		}

		sprintf(cmd,"cp %s %s >/dev/null 2>&1",old_filename,new_filename);
		if (system(cmd))
		{
			*status=48;							/* Serious error if only half worked.	*/
			return;
		}
		found = 1;
	}

	if (!found)
	{
		*status = 20;
		return;
	}

}

badusage()
{
	printf("\n");
	printf("Usage: wcopy         oldfile oldlib oldvol newfile newlib newvol\n");
	printf("       wcopy LIBRARY         oldlib oldvol         newlib newvol\n");
	printf("\n");
	exit(-1);
}


#ifdef unix
#include "wutils.h"
#endif


#endif
