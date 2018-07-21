/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	File:		rvmap.c
**
**	Project:	WISP
**
**	Purpose:	Handle Remote Volume mapping for ACUServer & File Share support.
**
**	Routines:	
**	remote_volume()		Translate a local volume to a remote volume
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "idsistd.h"
#include "assert.h"
#include "wmalloc.h"
#include "rvmap.h"
#include "wdefines.h"
#include "werrlog.h"
#include "wperson.h"

/*
**	Structures and Defines
*/
#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

#define MAX_CONFIG_PATH_LEN	132			/* Max len of config file paths		*/
#define MAX_CONFIG_REC_LEN	132			/* Max len of config file records	*/
#define MAX_PREFIX_LEN		64			/* Max len of RVMAP file prefixes	*/
#define MIN_PREFIX_LEN		3			/* Min len of RVMAP file prefixes	*/

struct rvmap_struct
{
	struct rvmap_struct *next;			/* The next item in the list.	*/
	char	local_prefix[MAX_PREFIX_LEN];		/* The local prefix		*/
	char	remote_prefix[MAX_PREFIX_LEN];		/* The remote prefix		*/
};

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/
static struct rvmap_struct *rvmap(void);


/*
**	ROUTINE:	WL_remote_volume()
**
**	FUNCTION:	Preform Remote Volume Translation
**
**	DESCRIPTION:	Check if local_path has a matching local_prefix in RVMAP and
**			if it does create a remote_path by replacing the local_prefix
**			with the remote_prefix.
**
**			If no match is found then simply copy local_path to remote_path.
**
**   		RVMAP example:
**			/central/   @central:/
**			/mnt/dp001/ @dp001:/
**
**			Local filepath / Remote filepath
**			--------------------------------
**			/central/disk1/myvol/mylib/myfile
**			@central:/disk1/myvol/mylib/myfile
**
**			/mnt/dp001/usr/vol300/mylib/myfile
**			@dp001:/usr/vol300/mylib/myfile
**
**			/disk2/vol100/mylib/myfile
**			/disk2/vol100/mylib/myfile
**
**
**	ARGUMENTS:
**	local_path	The local filepath to test (Blank padded - **NOT NULL TERMINATED**)
**	remote_path	The returned remote filepath (Blank padded- **NOT NULL TERMINATED**)
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		No match found (remote_path == local_path)		
**	1		Found a match, this is a remote volume.
**
**	WARNINGS:	local_path and remote_path must point to different locations
**			and must not overlap.
**
*/
int WL_remote_volume(const char *local_path, char *remote_path)
{
	struct rvmap_struct *rvmap_item;

	ASSERT(local_path && remote_path);
	ASSERT(local_path != remote_path);
	
	rvmap_item = rvmap();
	while(rvmap_item)
	{
		if (0 == memcmp(rvmap_item->local_prefix, local_path, strlen(rvmap_item->local_prefix)))
		{
			/*
			**	Found a match so build remote_path from the remote_prefix + (local_path - local_prefix)
			*/
			int	remote_prefix_len, local_prefix_len, local_remainder;
			
			remote_prefix_len = strlen(rvmap_item->remote_prefix);
			local_prefix_len  = strlen(rvmap_item->local_prefix);
			local_remainder	  = COB_FILEPATH_LEN - MAX(remote_prefix_len, local_prefix_len);
			ASSERT(remote_prefix_len + local_remainder <= COB_FILEPATH_LEN);
			
			memset(remote_path,' ',COB_FILEPATH_LEN);
			memcpy(remote_path, rvmap_item->remote_prefix, remote_prefix_len);
			memcpy(&remote_path[remote_prefix_len], &local_path[local_prefix_len], local_remainder);
			return 1;
		}
		
		rvmap_item = rvmap_item->next;
	}
	
	/*
	**	No match was found
	*/
	memcpy(remote_path,local_path,COB_FILEPATH_LEN);
	return 0;
}

/*
**	ROUTINE:	rvmap()
**
**	FUNCTION:	Load the Remote Volume map (RVMAP) and return a pointer to the linked list.
**
**	DESCRIPTION:	If the RVAMP exists read it and build the rvmap_list linked list.
**
**			RVMAP: 	Local-Prefix Remote-Prefix
**				/server/     @server:/
**				/mnt/dp001/  @dp001:/
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		Pointer to the linked list.
**
**	WARNINGS:	None
**
*/
static struct rvmap_struct *rvmap(void)
{
	static int	first = 1;
	static struct rvmap_struct *rvmap_list = NULL;		/* The Remote Volume map linked list	*/
	
	if (first)
	{
		FILE	*the_file;
		char	rvmap_path[MAX_CONFIG_PATH_LEN];

		first = 0;
	
		rvmap_list = NULL;

		WL_build_wisp_config_path("RVMAP", rvmap_path);
		ASSERT(strlen(rvmap_path) < sizeof(rvmap_path));
	
		the_file = fopen(rvmap_path,"r");
		if (the_file)
		{
			char	inbuf[MAX_CONFIG_REC_LEN];
			struct rvmap_struct *rvmap_end;
			int	linenum = 0;

			while (fgets(inbuf,sizeof(inbuf),the_file))
			{
				int	cnt;
				char	local_prefix[MAX_CONFIG_REC_LEN], remote_prefix[MAX_CONFIG_REC_LEN];
				char	*ptr;

				linenum++;
				if (strlen(inbuf) <= 0) continue;
				if ('#' == inbuf[0]) continue;

				if ((ptr = strchr(inbuf,'\n'))) *ptr = (char)0;
				
				cnt = sscanf(inbuf, "%s %s", local_prefix, remote_prefix);
			
				if (0 == cnt) continue;
				if (-1 == cnt) continue;  /* -1 == End of string before first match (all whitespace) */
			
				if (cnt != 2)
				{
					WL_werrlog_error(WERRCODE(104),"RVMAP", "INVALID", 
						"RVMAP entry invalid line=%d [%s]",linenum,inbuf);
					continue;
				}
				if (strlen(local_prefix) >= MAX_PREFIX_LEN || strlen(remote_prefix) >= MAX_PREFIX_LEN)
				{
					WL_werrlog_error(WERRCODE(104),"RVMAP", "LONG", 
						"RVMAP prefix too long (MAX=%d) line=%d [%s]",
						MAX_PREFIX_LEN, linenum,inbuf);
					continue;
				}
				if (strlen(local_prefix) < MIN_PREFIX_LEN || strlen(remote_prefix) < MIN_PREFIX_LEN)
				{
					WL_werrlog_error(WERRCODE(104),"RVMAP", "SHORT", 
						"RVMAP prefix too short (MIN=%d) line=%d [%s]",
						MIN_PREFIX_LEN,linenum,inbuf);
					continue;
				}
			
				if (!rvmap_list)
				{
					rvmap_list = (struct rvmap_struct *)wisp_malloc(sizeof(struct rvmap_struct));
					rvmap_end = rvmap_list;
				}
				else
				{
					ASSERT(rvmap_end);
					rvmap_end->next = (struct rvmap_struct *)wisp_malloc(sizeof(struct rvmap_struct));
					rvmap_end = rvmap_end->next;
				}

				strcpy(rvmap_end->local_prefix, local_prefix);
				strcpy(rvmap_end->remote_prefix, remote_prefix);
				rvmap_end->next = NULL;

				wtrace("RVMAP","ADD", "local=[%s] remote=[%s]",	
				       rvmap_end->local_prefix, rvmap_end->remote_prefix);

			}

			fclose(the_file);
		}
	}
	
	return rvmap_list;
}


/*
**	History:
**	$Log: rvmap.c,v $
**	Revision 1.13  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.12  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.11  2002/12/09 19:15:33  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.10  2002/07/10 21:05:23  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/07/02 21:15:28  gsl
**	Rename wstrdup
**	
**	Revision 1.8  1998/05/22 18:49:47  gsl
**	Change to use wtrace()
**	
**	Revision 1.7  1997-06-05 12:49:29-04  scass
**	Added #ifdef #undef for MAX to resolve warning.
**
**	Revision 1.6  1996-07-17 17:52:16-04  gsl
**	change to use wmalloc()
**
**	Revision 1.5  1996-07-10 16:47:50-07  gsl
**	Changed include wassert.h to assert.h
**
**	Revision 1.4  1996-07-09 16:42:56-07  gsl
**	fix headers
**
**	Revision 1.3  1996-01-04 02:20:02-08  gsl
**	Change so args to remote_volume() are blank padded instead of null terminated.
**	Also improved the error reporting in rvmap()
**
 * Revision 1.2  1996/01/03  11:34:36  gsl
 * Finished
 *
 * Revision 1.1  1996/01/03  10:52:30  gsl
 * Initial revision
 *
**
**
*/
