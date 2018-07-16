static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		sharemem.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	WISP shared memory access routines.
**
**	Routines:	
*/

/*
	The layout of shared memory:


	shr_header	pr_table[MAX_PARMS]	pr_data									end_data
	|		|			|									|
	|		|			|									|
	**********************************....***********************************************************.....***********
	*		*  |  |	 |  |  |	*    *		*key*value*	     *key*value*    *			*
	*		*  |  |	 |  |  |	*    *		***********	     ***********    *			*
	*		******************....	*    ************	  ************	       *    *****.....***********
	*****************			******		|	  |	     |	       ******
						|    |		data	  |	     data      |    
						|    KEYWSHM		  KEYWSHM	       |
						SHMH					       SHMH

	- each entry in pr_table points to a SHMH
	- each SHMH has multiple KEYWSHMH following, one for each keyword
	- each KEYWSHMH contains a pointer to the next KEYWSHMH and is followed by KEY and VALUE

*/

/*
	OVERVIEW OF SHARED MEMORY

	On UNIX you create a share memory area directly but you need a key to identify it, the key is obtained by creating
	a dummy "key" file and using it's inode as the key (ftok).  The memory is identified by the key and can be attached
	detached or deleted.
		shmget()	- get a shared memory segment
		shmat()		- attach (map) segment into memory
		shmdt()		- detach (unmap) segment from memory
		shmctl(IPC_RMID)- remove (delete) shared memory segment

	On WIN32 we dummy it up by mallocing regular memory then copying it back and forth to a file after every change.
*/

/*
	Routines:

static	max_pages()		Return the size in pages.
static	max_parms()		Return max number of parms.
static	roundup4()		Round a number up to the next 4 divisable number.
	a_int2()		Takes a pointer to an unaligned int2 and returns a int2.
static	get_sh_raw()		Get a chunk of shared mem data area.
	get_sh_seg()		Get a shared segment of memory.
	finish_sh_seg()		This is called to update the shr_header counters after the PRB has been loaded.
	get_prb_id()		This routine returns a pointer to the PRB identified by id or NULL if not found.
static	off_prbid()		Returns the offset into the data area of a prb with the given id.
static	compress_data()		This routine will compress the data area of shared memory by removing all deleted prbs
static	map_shr_ptrs()		Map shared memory pointers into the shared memory area/file.
				*** This is the high level routine you call to set-up shared memory. ***
static	init_parm_area()	Initialize the shared memory area.
static	map_global_ptrs()	Map the global shared memory pointers.
	cleanup_shrfil()	Cleanup and close the shared memory file.
				*** This is the high level routine you call to cleanup shared memory. ***
	show_parm_area()	Print a report showing the current putparm status.
	ishow_parm_area()	Print a report showing the internal status of putparms
	dump_parm_area()	Copy the contents of the shared memory area to a file.
	wax_table_entry()	This routine removes a PRB entry from the pr_table. (Also ensures status=P_DELETED)
	erase_prb_level()	Erase all PRB's at this link-level or higher
	erase_prb()		Erase this PRB
	delete_prb()		Delete this PRB (if labeled then mark as USED)
	use_prb()		Use one instance of this PRB
	get_prb_area()		Search the pr_table for a PRB by either prname or label.
	load_keywshm()		Load the keyword=value at the given address
static	getlinklevel()		Return the current linklevel. 
	backwards_reference()	Perform GETPARM time backwards referencing
	update_prb()		Update (merge) a fmtlist into a PRB
static	rewrite_prb()		Rewrite a PRB with an updated fmtlist.
	ppunlink()		To unlink the putparms (delete or mark as used).

OSD:				*** These are the low-level Operating System Dependent routines that control shared memory.
				*** These have no knowledge of what shared memory is being used for and could easily be made
				*** into generalized routines.

static	osd_open_seg()		Open/create a shared memory segment
static	osd_delete_seg()	Delete a shared memory segment
static	osd_map_seg()		Map the shared memory segment into the programs address space
static	osd_unmap_seg()		Unmap the segment
static	osd_exists_seg()	Check if the shared memory segment exists
static	osd_sync_seg()		Synchronize the external segment to match the internal segment
	wisp_sm_ctlfile()	Generate control file name.

*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#if defined(_MSC_VER)
#include <direct.h>
#include <io.h>
#endif

#include "idsistd.h"
#include "werrlog.h"
#include "sharemem.h"
#include "putparm.h"
#include "wdefines.h"
#include "wanguid.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "level.h"
#include "wexit.h"
#include "wperson.h"
#include "osddefs.h"
#include "wmalloc.h"

#define		ROUTINE		59800

/*
59801	%%SHAREMEM-I-ENTRY %s
59802	%%SHAREMEM-F-OPTIONS Invalid combination MAXPRBPARMS=%d MAXPRBPAGES=%d
59804	%%SHAREMEM-F-NOMEM Not enough global memory to store PUTPARM count=%d, size=%d 
59806	%%SHAREMEM-F-MAXPPARMS Maximum PUTPARMs (%d) exceeded.
59808	%%SHAREMEM-F-CORRUPT The PUTPARM blocks in shared memory have been corrupted.
59810	%%SHAREMEM-F-GETGBLPTR Unable to access PUTPARM data in shared memory. Status=%x (hex)
59812	%%SHAREMEM-F-VERSION PUTPARM/GETPARM version mismatch file=%s, current=%s
59814	%%SHAREMEM-F-DEASSIGN Error when deassigning channel. status=%x (hex)
59816	%%SHAREMEM-F-UNLINK Error deleting PUTPARM temp file %s errno=%d
59818	%%SHAREMEM-F-SYS$ERASE Error when deleting PUTPARM shared memory. status=%x (hex)
59820	%%SHAREMEM-F-SYS$UPDSECW Error updating the PUTPARM section file. status=%x (hex)
59822	%%SHAREMEM-F-SYS$UPDSECW Error updating the PUTPARM section file. iosb=%d
59824	%%SHAREMEM-F-SYS$DELTVA Error deleting PUTPARM virtual address. status=%x (hex)
59826	%%SHAREMEM-F-NOTMAPPED Error in "wax_table_entry", PUTPARM global area not mapped.
59828	%%SHAREMEM-F-MKDIR Can't create PUTPARM tmp dir %s. errno=%d
59830	%%SHAREMEM-F-CHMOD Can't change protection of %s. errno=%d
59832	%%SHAREMEM-F-FOPEN Error opening PUTPARM shared memory file %s. errno=%d
59834	%%SHAREMEM-F-SHMGET Error getting PUTPARM shared memory id, shmkey=%d, errno=%d
59836	%%SHAREMEM-F-SHMAT Error getting PUTPARM shared memory address , shmid=%d, errno=%d
59838	%%SHAREMEM-F-SYS$GETJPI Error getting system information. Status=%x (hex)
59840	%%SHAREMEM-F-SYS$CREATE Error creating PUTPARM shared memory area. Status=%x (hex)
59842	%%SHAREMEM-E-UPDATE Failed to update PRB prname=%s label=%s
59844	%%SHAREMEM-E-UPDATE Not enough memory to update PRB prname=%s label=%s
59846	%%SHAREMEM-F-BACKREF Backwards reference failed [%s]
*/

/*
**	Structures and Defines
*/

#define W_PAGE_SIZE	512
#define DEF_MAX_PARMS	128
#define DEF_MAX_PAGES	40

#define PRNAME_SIZE	8
#define LABEL_SIZE	8
#define KEYWORD_SIZE	8

/*
** OLD		SHM_VERSION	"V2.0_2"	Very old method
** LAST 	SHM_VERSION	"V3.0_1"	Curent structure without link-level tracking.
** CURRENT 	SHM_VERSION	"V3.3_1"	Added link-level tracking.
*/

#define SHM_VERSION	"V3.3_1"
#define THE_VERSION_SIZE 6


typedef struct sh_mem_header
	{
		char	gp_pp_ver[8];	/* THE_VERSION_SIZE + 2 */			/* The version number			*/
		int2	cur_prb_id;							/* Current prb id number		*/
		int2	num_parms;							/* The current number of parms stored.	*/
		int2	max_num_parms;							/* The maximum number of parms		*/
		int2	max_num_pages;							/* The maximum number of pages		*/
		int4	unused_off;							/* Offset of unused data area		*/
		char	filler[24];							/* Set up for futrue use.		*/
	} gp_pp_header;

struct gbl_parm_table
	{
		int4	proff;								/* Offset of the data in the file.	*/
		int2	prb_id;								/* Id to link with data area		*/
		char	prname[PRNAME_SIZE + 1];					/* prname				*/
		char	label[LABEL_SIZE];						/* The PUTPARM label.			*/
		char	linklevel;							/* The link level			*/
		char	filler[4];							/* Filler for future use.		*/
	};
typedef struct gbl_parm_table gbl_parm_table;

/*
**	Globals and Externals
*/
extern	int opt_max_pages;								/* OPTIONS file max pages.		*/
extern	int opt_max_parms;								/* OPTIONS file max parms.		*/

/*
**	Static data
*/

static char gp_pp_version[THE_VERSION_SIZE + 1] = SHM_VERSION;				/* Version indicator for GETPARM/PUTPARM*/
static char cur_version[THE_VERSION_SIZE + 1];						/* Current version indicator.		*/

static gp_pp_header *shr_header;							/* Pointer to header of shared memory.	*/

static char *gbl_ptr = (char *) 0;							/* The base pointer of the global mem.	*/
											/* Initial part of data in global area. */
static gbl_parm_table *pr_table;							/* The table. It has MAX_PARMS items.	*/
static char *pr_data;									/* The pointer to the data.		*/
static char *end_data;									/* A pointer to the end of all the data.*/

static new_file;									/* Flag indicating a new file.		*/
static int found_shr_mem;								/* Found the shared memory file flag.	*/

/*
**	Static Function Prototypes
*/

static int 	max_pages(void);
static int 	max_parms(void);
static int 	roundup4(int x);
static char 	*get_sh_raw(int size);
static int4 	off_prbid(int2 id);
static void	compress_data(void);
static int	map_shr_ptrs(void);
static int	init_parm_area(void);
static void	map_global_ptrs(void);
static char 	getlinklevel(void);
static int 	rewrite_prb(SHMH **prb_ptr, FMTLIST *fmtlist);

static int 	osd_open_seg(int *new);
static int 	osd_delete_seg(void);
static char 	*osd_map_seg(void);
static int 	osd_unmap_seg(void);
static int 	osd_exists_seg(void);
static int 	osd_sync_seg(void);



/*==============================================================================================================================*/

/*
**	max_pages()	Return the number of pages
*/
static	int	max_pages(void)
{
static	int pages=DEF_MAX_PAGES;							/* Max number of pages in the sec file. */
static	int	first=1;

	if (first)
	{
		first = 0;
		load_options();
		if (opt_max_pages >= 10 && opt_max_pages <= 200)
			pages = opt_max_pages;
	}
	return(pages);
}
/*
**	max_parms()	Return the number of parms
*/
static	int	max_parms(void)
{
static	int parms=DEF_MAX_PARMS;							/* Maximum number of prb's in the sect. */
static	int	first=1;

	if (first)
	{
		first = 0;
		load_options();
		if (opt_max_parms >= 32 && opt_max_parms <= 512)
			parms = opt_max_parms;
	}
	return(parms);
}

/*
**	roundup4()	Round a number up to the next 4 divisable number.
**			This is used when allocating memory, so as to always get 4 byte chunks.
*/
static int roundup4(int x)
{
	return( (x+3) & ~(3l) );
}

/*
	a_int2		Takes a pointer to an unaligned int2 and returns a int2.
*/
int2 a_int2(void *ptr)
{
	int2	tmp;

	memcpy(&tmp,ptr,sizeof(int2));
	return(tmp);
}

int4 a_int4(void *ptr)
{
	int4	tmp;

	memcpy(&tmp,ptr,4);
	return(tmp);
}

/*
	get_sh_raw	Get a chunk of shared mem data area.
			This routine will return a pointer size bytes of data.
			It will also update the unused_off (unused offset) variable in the header.
*/
static char *get_sh_raw(int size)
{
	int	cur_item;
	char	*dat_ptr;

	wtrace("SHAREMEM","GET_SH_RAW","Get a chuck of the shared memory data area (size=%d)",size);

	size = roundup4(size);

	if (map_shr_ptrs()) return(NULL);						/* Unsuccessful map to shared memory.	*/

	cur_item = shr_header->num_parms;						/* This is the index of the current item*/

	if ((pr_data + shr_header->unused_off + size) >= end_data)			/* Can't store, too much data.		*/
	{
		compress_data();
	}

	if ((pr_data + shr_header->unused_off + size) >= end_data)			/* Can't store, too much data.		*/
	{
		werrlog(ERRORCODE(4),cur_item,size,0,0,0,0,0,0);			/* Not enough global memory.		*/
		wexit(ERRORCODE(4));
	}


	dat_ptr = pr_data + shr_header->unused_off;					/* Get a pointer to the data.		*/
	shr_header->unused_off += size;

	return(dat_ptr);								/* Return a pointer to the caller.	*/
}

/*
	get_sh_seg	Get a shared segment of memory.
			This routine returns a pointer to a chunk of shared memory of length=size.
			It also loads the prname and label into the pr_table and sets the offset to also point to
			the memory.

			The memory is always taken from the end of the data area.  When the data area is full we do a compress
			to remove all deleted prbs.  The enteries in the data area are NOT in the same order as the entries
			in the table -- we use a prb_id to match the two.  A "memory-order" linked list is maintained of all
			the prbs (alive, deleted and kept) in the data area (via size). 

			*** NOTE ***	The header counters are NOT updated until finish_sh_seg() is called.

			*** NOTE ***	The position in the data area of the PRBs will be changed if by compress.

*/
char *get_sh_seg(char *prname, char *label, int size, int *id, int *rsize)
{
	int cur_item;
	char *dat_ptr;

	wtrace("SHAREMEM","GET_SH_SEG","Get a segment of shared memory and setup pr_table. prname=[%8.8s] label=[%8.8s] size=%d",
	       prname, label, size);

	size = roundup4(size);
	*rsize = size;

	if (map_shr_ptrs()) return(NULL);						/* Unseccussful map to shared memory.	*/

	if (shr_header->num_parms == shr_header->max_num_parms)				/* Max parms exceeded.			*/
	{
		werrlog(ERRORCODE(6),shr_header->max_num_parms,0,0,0,0,0,0,0);
		wexit(ERRORCODE(6));
	}

	cur_item = shr_header->num_parms;						/* This is the index of the current item*/
	memcpy(pr_table[cur_item].prname,prname,PRNAME_SIZE);				/* Copy the prname.			*/
	memcpy(pr_table[cur_item].label,label,LABEL_SIZE);				/* Copy the PUTPARM label.		*/
	pr_table[cur_item].prb_id = shr_header->cur_prb_id;
	*id			  = shr_header->cur_prb_id;
	pr_table[cur_item].proff = shr_header->unused_off;
	pr_table[cur_item].linklevel = getlinklevel();

	if ((pr_data + pr_table[cur_item].proff + size) >= end_data)			/* Can't store, too much data.		*/
	{
		compress_data();
	}

	pr_table[cur_item].proff = shr_header->unused_off;

	if ((pr_data + pr_table[cur_item].proff + size) >= end_data)			/* Can't store, too much data.		*/
	{
		werrlog(ERRORCODE(4),cur_item,size,0,0,0,0,0,0);			/* Not enough global memory.		*/
		wexit(ERRORCODE(4));
	}

	dat_ptr = pr_data + pr_table[cur_item].proff;					/* Get a pointer to the data.		*/

	return(dat_ptr);								/* Return a pointer to the caller.	*/
}

/*
	finish_sh_seg	This is called to update the shr_header counters after the PRB has been loaded.
*/
int finish_sh_seg(int size)
{
	wtrace("SHAREMEM","FINISH_SH_SEG","Update the shr_header counters after the PRB has been loaded (size=%d)",size);

	shr_header->num_parms += 1;							/* Add 1.				*/
	shr_header->cur_prb_id += 1;
	shr_header->unused_off += size;
	return 0;
}

/*
	get_prb_id	This routine returns a pointer to the PRB identified by id or NULL if not found.
*/
SHMH *get_prb_id(int2 id)
{
	int4	off;
	SHMH	*prb_ptr;

	wtrace("SHAREMEM","GET_PRB_ID","Find PRB for id=%d",(int)id);

	if (map_shr_ptrs()) return(NULL);					/* Map into shared memory			*/

	off = off_prbid(id);							/* Get offset into data area			*/
	if (off == -1) return(NULL);

	prb_ptr = (SHMH *)(pr_data + off);					/* Calc the PRB address				*/
	return(prb_ptr);
}

/*
	off_prbid		Returns the offset into the data area of a prb with the given id.
				A return = -1 means the prb was not found in the data area.
*/
static int4 off_prbid(int2 id)
{
	SHMH	*ptr_shmh;
	char	*ptr;
	int4	rc;

	ptr = pr_data;
	for(;;)
	{
		ptr_shmh = (SHMH *)ptr;
		if (ptr_shmh >= (SHMH *)(pr_data + shr_header->unused_off))
		{
			rc = -1;
			break;
		}
		if (ptr_shmh->prb_id == id )
		{
			rc =  (char *)ptr_shmh - pr_data;
			break;
		}
		ptr += ptr_shmh->prb_size;
	}

	wtrace("SHAREMEM","OFF_PRBID","Offset for PRB id=%d is %ld", (int)id, (long)rc);

	return rc;
}

/*
	compress_data		This routine will compress the data area of shared memory by removing all deleted prbs
				and shifting all remaining prbs to lower memory. It will also update the pr_table offsets
				to point to the new locations.
*/
static void compress_data(void)
{
	char	*ptr_source, *ptr_dest;
	int4	len,i;
	SHMH	*ptr_shmh;

	wtrace("SHAREMEM","COMPRESS_DATA","Compress the data area to remove deleted PRBs");

	/*
	**	Compress the data area by shifting all non-deleted prbs to lower memory
	*/

	ptr_source = pr_data;
	ptr_dest   = pr_data;

	for(;;)
	{
		if (ptr_source >= (pr_data + shr_header->unused_off))
		{
			shr_header->unused_off = ptr_dest - pr_data;
			break;
		}

		ptr_shmh = (SHMH *) ptr_source;
		len = roundup4(ptr_shmh->prb_size);

		if ( ptr_shmh->status == P_DELETED )
		{
			ptr_source += len;
		}
		else if ( ptr_source == ptr_dest )
		{
			ptr_source += len;
			ptr_dest = ptr_source;
		}
		else
		{
			safemove(ptr_dest, ptr_source, len);
			ptr_source += len;
			ptr_dest += len;
		}
	}

	/*
	**	Update the pr_table offsets
	*/

	ptr_shmh = (SHMH *)pr_data;

	for(i=0;i<shr_header->num_parms;i++)
	{
		pr_table[i].proff = off_prbid(pr_table[i].prb_id);
		if (pr_table[i].proff == -1)
		{
			werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);			/* PRBs are corrupt				*/
			wexit(ERRORCODE(8));
		}
	}
}

/*
**	map_shr_ptrs()		Map shared memory pointers into the shared memory area/file.
**				The shared memory file will be created if needed.
**				The global shared memory pointers will then be maped.
*/
static int map_shr_ptrs(void)								/* This routine maps the pointers	*/
{											/* in the shared memory file.		*/
	int	new;

	wtrace("SHAREMEM","MAP_SHR_PTRS","Map shared memory pointers into the shared memory area");

	osd_open_seg(&new);								/* Open the shared memory segment	*/
	gbl_ptr = osd_map_seg();							/* Map the segment into memory		*/

	if (new)									/* If new then init the table		*/
	{
		init_parm_area();							/* (this also maps the globals)		*/
	}
	else										/* If old then check version numbers	*/
	{
											/* All ok, map the needed pointers.	*/
		shr_header = (gp_pp_header *)gbl_ptr;					/* Pointer to the header section.	*/

		memcpy(cur_version,shr_header->gp_pp_ver,THE_VERSION_SIZE);		/* GETPARM/PUTPARM version indicator.	*/
		cur_version[THE_VERSION_SIZE] = '\0';					/* Null terminate.			*/
		if (strcmp(cur_version,gp_pp_version) != 0)				/* Test the version number.		*/
		{
			werrlog(ERRORCODE(12),cur_version,gp_pp_version,0,0,0,0,0,0);	/* Version mismatch.			*/
			wexit(ERRORCODE(12));
		}
		map_global_ptrs();							/* Map the global pointers		*/
	}
	return(0);
}


/*
**	init_parm_area		Initialize the shared memory area.
**				This is called once after shared memory area is created.
*/
static int init_parm_area(void)								/* Init the parm area.			*/
{
	wtrace("SHAREMEM","INIT_PARM_AREA","Initializing the shared memory area");

	memset(gbl_ptr,'\0',(max_pages() * W_PAGE_SIZE));				/* Init to nulls			*/

	shr_header = (gp_pp_header *)gbl_ptr;						/* Pointer to the header section.	*/

	memcpy(shr_header->gp_pp_ver,gp_pp_version,THE_VERSION_SIZE);			/* Copy version to gbl file.		*/
	shr_header->cur_prb_id = 1;							/* Starting prb_id			*/
	shr_header->num_parms = 0;							/* init number of parms.		*/
	shr_header->max_num_parms = max_parms();					/* Record the sizes			*/
	shr_header->max_num_pages = max_pages();
	shr_header->unused_off = 0;							/* Point to start of data area		*/

	map_global_ptrs();

	if ( pr_data > end_data )
	{
		werrlog(ERRORCODE(2),max_parms(),max_pages(),0,0,0,0,0,0);		/* Not enough global memory.		*/
		wexit(ERRORCODE(2));
	}

	return(0);
}

/*
	map_global_ptrs()	Map the global shared memory pointers.
				This routines requires glb_ptr to already point to the start of shared memory.
*/
static void map_global_ptrs(void)
{
	wtrace("SHAREMEM","MAP_GLOBAL_PTRS","Mapping the global shared memory pointers");

	shr_header = (gp_pp_header *)gbl_ptr;						/* Pointer to the header section.	*/

	pr_table = (gbl_parm_table *)(gbl_ptr + roundup4(sizeof(gp_pp_header)));	/* Pointer to where the table is.	*/
											/* After the table is the actual data.	*/
	pr_data =  (char *)((char *)pr_table + 
		   (roundup4(sizeof(gbl_parm_table) * shr_header->max_num_parms)));	/* Pointer to where the actual data is. */

	end_data = (char *) (gbl_ptr + (shr_header->max_num_pages * W_PAGE_SIZE));	/* Pointer to end of the mess.	(+1)	*/
}

/*
	cleanup_shrfil		Cleanup and unmap the shared memory file.
*/
int cleanup_shrfil(void)								/* Cleanup after users of the shared dat*/
{
	wtrace("SHAREMEM","CLEARUP_SHRFIL","Cleanup and unmap the shared memory area");

	if (!osd_exists_seg()) return(0);						/* If doesn't exist then just return	*/

	if (map_shr_ptrs()) return(0);							/* Unsuccessful map to shared memory.	*/

	if (0 == shr_header->num_parms)							/* Check to see if the file is empty.	*/
	{
		osd_delete_seg();							/* Unmap and delete the shared memory	*/
	}
	else
	{
		osd_sync_seg();								/* Sync the shared memory		*/
		osd_unmap_seg();							/* unmap it				*/
	}

	return(0);
}

/*
	show_parm_area		Print a report showing the current putparm status.
				Used by WPUTPARM SHOW
*/
int show_parm_area(void)								/* Display the available PUTPARMs.	*/
{
	int 	i;
	int	first=1;
	char	func[5];

	werrlog(ERRORCODE(1),"show_parm_area",0,0,0,0,0,0,0);

	if (!osd_exists_seg())
	{
		printf("There are currently no PUTPARM's.\n");
		return(0);
	}

	if (map_shr_ptrs())
	{
		printf("Unable to map shared pointers.\n");
		return(0);
	}

	for (i = 0; i < shr_header->num_parms; i++)
	{										/* Print name and size.			*/
		SHMH	*pp;
		char	buff[256], *bptr , *tptr;
		int	j,k;
		KEYWSHM tkh, *kh;

		bptr = buff;
		*bptr = '\0';

		tptr = (char *)pr_data;
		tptr += pr_table[i].proff;
		pp = (SHMH *)tptr;


		if (pp->status == P_DELETED) continue;

		if (pp->keyw_cnt)
		{
			tptr = (char *)pp;
			tptr +=sizeof(SHMH);
			kh = (KEYWSHM *)tptr;
		}

		if (pp->reflbl[0] && 0 != memcmp(pp->reflbl,"        ",LABEL_SIZE))	/* If there is a reference label then	*/
		{
			char	tbuf[20];
			cobx2cstr(tbuf,pp->reflbl,LABEL_SIZE);				/* Display the reference label		*/
			sprintf(bptr,"(%s) ",tbuf);
			bptr += strlen(bptr);
		}

		/*
		**	Load the keyword/value pairs into the data buff.
		*/
		for(j=0; j<pp->keyw_cnt; j++)
		{
			memcpy(&tkh,kh,sizeof(KEYWSHM));				/* Get temp align copy of *kh		*/

			if (bptr+a_int2(&tkh.value_len)+8+3 >= buff+sizeof(buff)-5)
			{
				*bptr++ = '.';
				*bptr++ = '.';
				*bptr++ = '.';
				*bptr++ = '\0';
				break;
			}

			tptr = (char *)kh;						/* Load "key=data" into buff		*/
			tptr += sizeof(KEYWSHM);
			for( k=8; k; k--)
			{
				if (*tptr == '\0' || *tptr == ' ' ) break;
				*bptr++ = *tptr++;
			}
			*bptr++ = '[';
			tptr = (char *)kh;
			tptr += sizeof(KEYWSHM);
			tptr += 8;

			memcpy(bptr,tptr, (int)a_int2(&tkh.value_len));
			bptr += a_int2(&tkh.value_len);
			*bptr++ = ']';
			*bptr++ = ' ';

			tptr = (char *)kh;
			tptr += tkh.next_offs;
			kh = (KEYWSHM *)tptr;

		}
		*bptr = '\0';
		

		if (pp->status == P_USED) strcpy(func,"USED");
		else
		{
			strcpy(func,"    ");
			func[1]=pp->type;
		}

		if (first)
		{
			first = 0;
		              /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	 	              /*XX XXXXXXXX XXXXXXXX  X   XXXXX   X  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX	 */
			printf("LL PRNAME   LABEL    FUNC  COUNT AID DATA\n\n");
		}
		printf("%2d %8.8s %8.8s %s %5d   %c  %s\n",
			(int)pr_table[i].linklevel,
			    pr_table[i].prname,
				  pr_table[i].label,
				       func,
					    pp->usage_cnt,
						   pp->pfkey,
						       buff);
	}
	
	if (first)									/* If no global memory file found or	*/
	{										/* no PUTPARMS found.			*/
		printf("No PUTPARM's found in shared memory.\n");
	}
	return(0);
}

/*
	ishow_parm_area		Print a report showing the internal status of putparms
				Used by WPUTPARM ISHOW

*/
int ishow_parm_area(void)	/* Do an internals SHOW */
{
	int 	i;
	SHMH	*ptr_shmh;
	char	*ptr;

	werrlog(ERRORCODE(1),"ishow_parm_area",0,0,0,0,0,0,0);

	if (!osd_exists_seg())
	{
		printf("There are currently no PUTPARM's.\n");
		return(0);
	}

	if (map_shr_ptrs())
	{
		printf("Unable to map shared pointers.\n");
		return(0);
	}

	printf("*** INTERNALS ***\n\n");
	printf("shr_header = %08x\n",shr_header);
	printf("pr_table   = %08x\n",pr_table);
	printf("pr_data    = %08x\n",pr_data);
	printf("end_data   = %08x\n",end_data);
	printf("Total area bytes = %d\n", end_data - (char *)shr_header);
	printf("Table area bytes = %d\n", pr_data - (char *)pr_table);
	printf("Data  area bytes = %d\n", end_data - (char *)pr_data);
	printf("\n");

	printf("shr_header->gp_pp_ver     =%s\n",shr_header->gp_pp_ver);
	printf("shr_header->cur_prb_id    =%d\n",shr_header->cur_prb_id);
	printf("shr_header->num_parms     =%d\n",shr_header->num_parms);
	printf("shr_header->max_num_parms =%d\n",shr_header->max_num_parms);
	printf("shr_header->max_num_pages =%d\n",shr_header->max_num_pages);
	printf("shr_header->unused_off    =%d\n",shr_header->unused_off);
	printf("\n");

	      /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	      /*[XXX] XXXXX XXXXX  XXXXXXXX XXXXXXXX    XX*/
	printf("pr_table\n");
	printf("[POS] proff prb_id PRNAME   LABEL    LinkLevel\n");
	for (i = 0; i < shr_header->num_parms; i++)
	{
		printf("[%3d] %5d %5d  %8.8s %8.8s    %d\n",
			i, pr_table[i].proff, pr_table[i].prb_id, pr_table[i].prname, pr_table[i].label, 
			(int)pr_table[i].linklevel);
	}

	      /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	      /*[XXX] XXXXXX XXXXXX XXXX XXXXX   X  XXXXXXXX XXXXXXXX XXXXXXXX XXXX XXXX   X  X */
	printf("\n");
	printf("pr_data\n");
	printf("[POS] offset status   ID  size type PRNAME   LABEL    REFLABEL ucnt kwcnt AID C\n");
	ptr = pr_data;
	for(i=0;i>=0;i++)
	{
		char	stat[10];
		int4	offset;

		ptr_shmh = (SHMH *)ptr;
		if (ptr_shmh >= (SHMH *)(pr_data + shr_header->unused_off))
		{
			break;
		}

		if	( ptr_shmh->status == P_OK )	  strcpy(stat,"OK    ");
		else if ( ptr_shmh->status == P_DELETED ) strcpy(stat,"DELETE");
		else if ( ptr_shmh->status == P_USED )	  strcpy(stat,"USED  ");

		offset = (char *)ptr_shmh - pr_data;

		printf("[%3d] %6d %6.6s %4d %5d   %c  %8.8s %8.8s %8.8s %4d %4d   %c  %c\n",
			i, offset, stat, ptr_shmh->prb_id, ptr_shmh->prb_size, ptr_shmh->type,
			ptr_shmh->prname, ptr_shmh->label, ptr_shmh->reflbl,
			ptr_shmh->usage_cnt, ptr_shmh->keyw_cnt, ptr_shmh->pfkey, ptr_shmh->cleanup);

		ptr += ptr_shmh->prb_size;
		if (ptr_shmh->prb_size < sizeof(SHMH))
		{
			printf("*** SHMH is corrupted: prb_size=%d\n",ptr_shmh->prb_size);
			werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);			/* PRBs are corrupt				*/
			wexit(ERRORCODE(8));
		}
	}

	return(0);
}

/*
	dump_parm_area		Copy the contents of the shared memory area to a file.
				Used by WPUTPARM DUMP filename
*/
int dump_parm_area(char *filename)
{
	FILE	*fp;
	char	*ptr;

	werrlog(ERRORCODE(1),"dump_parm_area",0,0,0,0,0,0,0);

	if (!osd_exists_seg())
	{
		printf("No PRB's in global memory.\n");
		return(0);
	}

	if (map_shr_ptrs())
	{
		printf("Unable to map shared pointers.\n");
		return(0);
	}

	makepath(filename);
	fp = fopen(filename,FOPEN_WRITE_BINARY);
	if (!fp)
	{
		printf("Unable to open file=[%s] for DUMP\n",filename);
		return(1);
	}

	ptr = gbl_ptr;
	for(ptr=gbl_ptr; ptr<end_data;ptr++) fputc(*ptr,fp);

	fclose(fp);
	return(0);
}

/*
	wax_table_entry		This routine removes a PRB entry from the pr_table. (Also ensures status=P_DELETED)
				Previously called "wax_parm_area".

		parm_area	Ptr to SHMH (PRB) area.

*/
int wax_table_entry(SHMH *parm_area)
{
	int	cur_item;
	int	dat_off;

	if (!gbl_ptr)									/* Not mapped.				*/
	{
		werrlog(ERRORCODE(26),0,0,0,0,0,0,0,0);					/* Error in WAX_PARM, global area not	*/
		wexit(ERRORCODE(26));							/*  mapped.				*/
	}

	if (!shr_header->num_parms) return(0);						/* Nothing there			*/

	dat_off = (char *)parm_area - pr_data;						/* Calculate the offset.		*/
											/* Look for the item with that offset.	*/
	for (cur_item=0; (cur_item < shr_header->num_parms) && (pr_table[cur_item].proff != dat_off); cur_item++);

	if (cur_item == shr_header->num_parms) return(0);				/* Was not found.			*/

	parm_area->status = P_DELETED;							/* Mark the SHMH as deleted.		*/

	wtrace("SHAREMEM","WAX_TABLE_ENTRY","Remove a PRB entry from the pr_table prname=[%8.8s] id=%d", 
	       parm_area->prname, parm_area->prb_id);

	shr_header->num_parms -= 1;							/* Decrememt the list.			*/
	while (cur_item < shr_header->num_parms)					/* Now move all the parms up one.	*/
	{
		memcpy((char *)&pr_table[cur_item],(char *)&pr_table[cur_item+1],sizeof(gbl_parm_table));
		cur_item++;
	}
	return 0;
}

/*
**	Routine:	erase_prb_level()
**
**	Function:	To erase all PRB's at this link-level or higher.
**
**	Description:	This routine will loop thru the pr_table checking each entries link-level.
**			It will then erase each PRB at this level or higher.
**
**	Arguments:	None
**
**	Globals:
**	pr_table	The PRB table
**	pr_data		Pointer to the Data area
**
**	Return:		0
**
**	Warnings:	The pr_table may be adjusted and any pointers into it are made invalid.
**
**	History:	
**	09/04/92	Written by GSL
*/
int	erase_prb_level(void)
{
	int	cur_item;
	SHMH	*prb;
	int 	done;
	int	level;

	if (!osd_exists_seg()) return(0);						/* If shrfil doesn't exist		*/
	if (map_shr_ptrs()) return 0;							/* Unsuccessful map to shared mem area. */

	level = (int)getlinklevel();

	wtrace("SHAREMEM","ERASE_PRB_LEVEL","Erase all PRBs at this linklevel=%d",level);
	
	done = 0;
	while(!done)
	{
		done = 1;								/* Assume done				*/

		for (cur_item=shr_header->num_parms - 1; cur_item >= 0; cur_item--)	/* Loop (backwards) for each PUTPARM 	*/
		{
			if ((int)(pr_table[cur_item].linklevel) >= level)		/* If link-level >= current		*/
			{
				prb = (SHMH *)(pr_data + pr_table[cur_item].proff); 	/* Get a pointer to the data.		*/

				erase_prb(prb);						/* Erase this PRB			*/
				done = 0;						/* Set not done flag			*/
				break;
				/*
				**	NOTE:	erase_prb() has side-effects; it alters the pr_table.
				**		We have to break out of the loop and restart after it is called.
				*/
			}
		}
	}

	return 0;
}

/*
**	Routine:	erase_prb()
**
**	Function:	To erase (completely) a PRB.
**
**	Description:	This routine will always mark the PRB as deleted and wax the table entry.
**			After this the PRB is completely gone and can not be backwards referenced.
**
**	Arguments:
**	prb		The PRB to erase.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The table entry may be adjusted and any pointers into it are made invalid.
**
**	History:	
**	09/04/92	Written by GSL
*/
int	erase_prb(SHMH *prb)
{
	if (!prb) return(0);

	prb->status = P_DELETED;
	wax_table_entry(prb);

	return(0);
}

/*
**	Routine:	delete_prb()
**
**	Function:	To delete a PRB.
**
**	Description:	This is like use_prb() except it doesn't care about usage counts.
**			If the PRB is labeled it will mark it as used.
**			We set the usage_cnt to 1 so that use_prb() will do the delete.
**
**	Arguments:
**	prb		The PRB to delete.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The table entry may be adjusted and any pointers into it are made invalid.
**
**	History:	
**	09/04/92	Written by GSL
**
*/
int delete_prb(SHMH *prb)
{
	if (!prb) return(0);

	wtrace("SHAREMEM","DELETE_PRB","Delete PRB prname=[%8.8s] id=%d", prb->prname, (int)prb->prb_id);

	prb->usage_cnt = 1;							/* Rig the usage_cnt so it will delete.		*/
	return( use_prb(prb) );							/* Call use_prb to do the delete		*/
}

/*
**	Routine:	use_prb()
**
**	Function:	To "use" a PRB by a GETPARM.
**
**	Description:	This routine will use a PRB.
**			It will decrement the usage count.
**			If out of count it will delete it or mark as used if labeled.
**			If an unlimited usage count then will not change.
**
**	Arguments:
**	prb		The PRB to use.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	The table entry may be adjusted and any pointers into it are made invalid.
**
**	History:	
**	09/04/92	Written by GSL
**
*/
int	use_prb(SHMH *prb)
{
	if (!prb) return(0);
	if (P_OK != prb->status) return(0);						/* Already used or deleted		*/
	if (0 == prb->usage_cnt) return(0);						/* Unlimited usage count		*/

	wtrace("SHAREMEM","USE_PRB","Mark this PRB as used prname=[%8.8s] id=%d",prb->prname, prb->prb_id);
	
	prb->usage_cnt--;								/* Use one instance of it.		*/
	if (0 < prb->usage_cnt) return(0);						/* If usages remain then return		*/
	
	/*
	**	This PRB is all used up so DELETE it or mark it as USED.
	*/
	if (prb->label[0] && 0 != memcmp(prb->label,"        ",LABEL_SIZE))
	{
		prb->status = P_USED;
	}
	else
	{
		prb->status = P_DELETED;
		wax_table_entry(prb);
	}
	
	return(0);
}

/*
	get_prb_area		Search the pr_table for a PRB by either prname or label.
				This will map the pointers if needed.
				If PRB is found it returns a pointer to the SHMH structure.
				If a label is supplied it searches by label else it searches by prname.
				If used_ok is set then it will match even if PRB is marked as USED. (keep).

	m_prname;		The prname to match (or NULL)
	m_label;		The label to match (or NULL)
	used_ok;

*/

SHMH *get_prb_area(char *m_prname, char *m_label, int used_ok)
{
	int	cur_item, found_item;
	SHMH	*ptr_shmh;
	int	bylabel;
	int	wantlevel, found_level, cur_level;

	if (!osd_exists_seg()) return(NULL);						/* If shrfil doesn't exist		*/

	if (map_shr_ptrs()) return(NULL);						/* Unsuccessful map to shared mem area. */

	if (shr_header->num_parms == 0) return(NULL);					/* No parms.				*/

	if (m_label && memcmp(m_label,"        ",LABEL_SIZE) != 0)			/* If label and not blank		*/
	{
		bylabel = 1;
		wantlevel = linklevel();						/* Want current link-level.		*/
	}
	else if (m_prname && memcmp(m_prname,"        ",PRNAME_SIZE) != 0)		/* else match the prname.		*/
	{
		bylabel = 0;
		wantlevel = linklevel() - 1;						/* Want previous link-level.		*/
	}
	else
	{
		return(NULL);
	}

	wtrace("SHAREMEM","GET_PRB_AREA","Search pr_table for PRB prname=[%8.8s] label=[%8.8s]", 
	       (m_prname)? m_prname:"(nil)", (m_label)?m_label:"(nil)");

	found_item = -1;
	found_level = -1;

	for (cur_item=0; cur_item < shr_header->num_parms; cur_item++)			/* Loop each PUTPARM and test prname.	*/
	{
		if ( ( bylabel && memcmp(pr_table[cur_item].label,m_label,LABEL_SIZE)==0) ||
		     (!bylabel && memcmp(pr_table[cur_item].prname,m_prname,PRNAME_SIZE)==0)  )	
											/* found a match			*/
		{
			ptr_shmh = (SHMH *)(pr_data + pr_table[cur_item].proff);	/* Get a pointer to the data.		*/
			if (used_ok || ptr_shmh->status != P_USED)			/* Is it valid to return		*/
			{
				/*
				**	We have found a match.  If it has the right linklevel then break otherwise
				**	keep looking for a better match.  If multiple matches of the same linklevel then
				**	take the first one of that linklevel.
				*/

				cur_level = (int)pr_table[cur_item].linklevel;
				if (found_item == -1 ||					/* None found yet (or)			*/
				    found_level < cur_level)				/* found a higher link level		*/
				{
					found_item = cur_item;
					found_level = cur_level;

					if (wantlevel == found_level) break;		/* Found exactly what we wanted		*/
				}
			}
		}
	}

	if (found_item == -1)								/* Nothing was found			*/
	{
		return(NULL);								/* Was not found.			*/
	}

	ptr_shmh = (SHMH *)(pr_data + pr_table[found_item].proff);			/* Get a pointer to the data.		*/

	return(ptr_shmh);								/* Return the pointer.			*/
}

/*
**	ROUTINE:	erase_prb_label_level()
**
**	FUNCTION:	Check for and erase a prb with the given label at the current linklevel.
**
**	DESCRIPTION:	Try to find a prb at the current linklevel with the given label.
**			If one is found then erase it - as only one is allowed - and we are 
**			about to add a new one.
**
**			This is called near the begining of write_putparm().
**
**	ARGUMENTS:
**	m_label		The putparm label
**
**	GLOBALS:	yes
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void erase_prb_label_level(char *m_label)
{
	int	cur_item;
	int	wantlevel;

	if (!osd_exists_seg()) return;							/* If shrfil doesn't exist		*/

	if (map_shr_ptrs()) return;							/* Unsuccessful map to shared mem area. */

	if (shr_header->num_parms == 0) return;						/* No parms.				*/

	if (!m_label || memcmp(m_label,"        ",LABEL_SIZE) == 0)			/* No label				*/
	{
		return;
	}

	wantlevel = linklevel();							/* Want current link-level.		*/

	wtrace("SHAREMEM","ERASE_PRB_LABEL_LEVEL","Erase PRB with label=[%8.8s] at this linklevel=%d",m_label,wantlevel);

	for (cur_item=0; cur_item < shr_header->num_parms; cur_item++)			/* Loop each PUTPARM and test prname.	*/
	{
		if ( memcmp(pr_table[cur_item].label,m_label,LABEL_SIZE)==0 )		/* found a match			*/
		{
			if (wantlevel == (int)pr_table[cur_item].linklevel)
			{
				/*
				**	Found a putparm at this linklevel with this label so delete it.
				*/
				SHMH	*ptr_shmh;
				ptr_shmh = (SHMH *)(pr_data + pr_table[cur_item].proff);	/* Get a pointer to the data.	*/

				erase_prb(ptr_shmh);
				return;
			}
		}
	}

	return;										/* Was not found.			*/
}


/*
**	Routine:	get_prb_prname_level()
**
**	Function:	Get the PRB for a given prname and linklevel
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	m_prname	The prname to find.
**	wantlevel	The link level to look at.
**
**	Globals:	None
**
**	Return:		The SHMH pointer or NULL if not found.
**
**	Warnings:	None
**
**	History:	
**	09/02/94	Written by GSL
**
*/
SHMH *get_prb_prname_level(char *m_prname, int wantlevel)
{
	int	cur_item, found_item;
	SHMH	*ptr_shmh;
	int	cur_level;

	if (!osd_exists_seg()) return(NULL);						/* If shrfil doesn't exist		*/

	if (map_shr_ptrs()) return(NULL);						/* Unsuccessful map to shared mem area. */

	if (shr_header->num_parms == 0) return(NULL);					/* No parms.				*/

	if (!m_prname || memcmp(m_prname,"        ",PRNAME_SIZE) != 0)			/* else match the prname.		*/
	{
		return(NULL);
	}

	wtrace("SHAREMEM","GET_PRB_PRNAME_LEVEL","Get PRB for prname=[%8.8s] at linklevel=%d",m_prname,wantlevel);
	
	found_item = -1;

	for (cur_item=0; cur_item < shr_header->num_parms; cur_item++)			/* Loop each PUTPARM and test prname.	*/
	{
		if (0==memcmp(pr_table[cur_item].prname,m_prname,PRNAME_SIZE))		/* found a match			*/
		{
			ptr_shmh = (SHMH *)(pr_data + pr_table[cur_item].proff);	/* Get a pointer to the data.		*/
			if (ptr_shmh->status != P_USED)					/* If Not used				*/
			{
				cur_level = (int)pr_table[cur_item].linklevel;
				if (wantlevel == cur_level)				/* Found exactly what we wanted		*/
				{
					found_item = cur_item;
					break;
				}
			}
		}
	}

	if (found_item == -1)								/* Nothing was found			*/
	{
		return(NULL);								/* Was not found.			*/
	}

	ptr_shmh = (SHMH *)(pr_data + pr_table[found_item].proff);			/* Get a pointer to the data.		*/

	return(ptr_shmh);								/* Return the pointer.			*/
}

/*
**	Routine:	get_chained_prb()
**
**	Function:	If passed a chained PRB it will return the next PRB up the chain.
**
**	Description:	If the starting PRB is labeled the look for a PUTPARM at the
**			previous link-level with a prname that matches this label.
**			If one is found return it.
**
**	Arguments:
**	start_prb	The starting prb.
**
**	Globals:	None
**
**	Return:		The next prb up the chain (working backwards) or NULL.
**
**	Warnings:	This assumes that the shared memory stuff is already mapped in etc.
**
**	History:	
**	09/06/94	Written by GSL
**
*/
SHMH *get_chained_prb(SHMH *start_prb)
{
	int	start_idx, curr_idx;
	SHMH	*ptr_shmh;

	werrlog(ERRORCODE(1),"get_chained_prb",0,0,0,0,0,0,0);

	if (!start_prb) return NULL;

	if (!start_prb->label[0] || 0 == memcmp(start_prb->label,"        ",LABEL_SIZE)) return NULL;

	/*
	**	Find the index into the pr_table for the starting prb.
	*/
	for (start_idx=0; start_idx < shr_header->num_parms; start_idx++)
	{
		if ( start_prb->prb_id == pr_table[start_idx].prb_id )
		{
			break;
		}
	}
	if (start_idx >= shr_header->num_parms)
	{
		/*
		**	This should never happen.
		*/
		return NULL;
	}

	/*
	**	Search the pr_table for an entry at the previous linklevel
	**	with a prname that matches the label.
	*/
	for (curr_idx=0; curr_idx < shr_header->num_parms; curr_idx++)
	{
		if (	pr_table[curr_idx].linklevel == pr_table[start_idx].linklevel - 1
		    &&	0 == memcmp(pr_table[curr_idx].prname, start_prb->label, LABEL_SIZE)	   )
		{
			/*
			**	Found a match for this chain.
			**	If status is OK then were done.
			*/
			ptr_shmh = (SHMH *)(pr_data + pr_table[curr_idx].proff);
			if ( P_OK == ptr_shmh->status )
			{
				return ptr_shmh;
			}
		}
	}

	return NULL;
}

/*
	load_keywshm	Load the keyword=value at the given address
			return the offset to next keyword.
*/
int load_keywshm(KEYWSHM *keywshm_ptr, FMTLIST *p)
{
	int2	tmp;
	int	off;
	char	*data_ptr;

	wtrace("SHAREMEM","LOAD_KEYWSHM","Load keyword=[%8.8s]", p->keyword);

	off = sizeof(KEYWSHM)+KEYWORD_SIZE+p->len+1;				/* setup offset for next			*/
	tmp = (int2)off;
	memcpy(&(keywshm_ptr->next_offs),&tmp,sizeof(int2));
	tmp = (int2)p->len;
	memcpy(&(keywshm_ptr->value_len),&tmp,sizeof(int2));
	keywshm_ptr->special = p->special;
	data_ptr = (char *)keywshm_ptr + sizeof(KEYWSHM);
	cstr2cobx( data_ptr, p->keyword, KEYWORD_SIZE);				/* copy in keyword				*/
	memcpy( data_ptr+KEYWORD_SIZE, p->value, (int)p->len);			/* and value					*/
	*(data_ptr+KEYWORD_SIZE+p->len) = '\0';					/* place a null at the end			*/
	return(off);
}

/*
	getlinklevel	Return the current linklevel. 
*/
static char getlinklevel(void)
{
	return((char)linklevel());
}

/*
**	Routine:	backwards_reference()
**
**	Function:	To perform any required backwards referencing.
**
**	Description:	This routine is given a pointer to a PRB that needs to be checked for backwards referencing.
**			This is only called from GETPARM to ensure that this PRB has been updated with the latest values.
**			This routine will handle:
**				SPECIAL KEYWORDS	Individual keyword backwards reference from PROCEDURE.
**				REFERENCE LABEL		As supplied by the PUTPARM. (With cleanup option)
**
**	Arguments:
**	prb_ptr		Pointer to the PRB that may need backwards referencing.
**
**	Globals:	None
**
**	Return:		0
**
**	Warnings:	The location of the PRB may change if backwards referencing causes the size or number of keywords
**			to increase such that it can't be updated in place.
**			NOTE: if the cleanup option is used the a erase_prb() is done and this can alter the pr_table.
**
**	History:	
**	08/28/92	Written by GSL
**
*/

int backwards_reference(SHMH **prb_ptr)
{
	int	ret, had_special;
	FMTLIST	*fmtlist, *p;
	char	buff[80];

	werrlog(ERRORCODE(1),"backwards_reference",0,0,0,0,0,0,0);

	ret = 0;

	had_special = 0;
	load_fmtlist(*prb_ptr,&fmtlist);
	for(p=fmtlist; p; p=p->next)
	{
		if (SPECIAL_KEY == p->special)
		{
			/*
			**	Need to backwards reference this individual keyword.
			*/
			char	label[LABEL_SIZE], keyword[KEYWORD_SIZE];
			SHMH	*ref_prb;
			KEYWSHM	*kw;

			had_special = 1;

			memcpy(label,p->value,LABEL_SIZE);
			memcpy(keyword,p->value+LABEL_SIZE,KEYWORD_SIZE);
			ref_prb = get_prb_area(NULL,label,OK_USED);	/* Get the referenced PRB			*/
			if (!ref_prb)
			{
				sprintf(buff,"Label=%8.8s Not found",label);
				werrlog(ERRORCODE(46),buff,0,0,0,0,0,0,0);
				wexit(ERRORCODE(46));
			}
			kw = find_prb_keyword(ref_prb,keyword,0);
			if (!kw)
			{
				sprintf(buff,"Keyword=%8.8s Not found",keyword);
				werrlog(ERRORCODE(46),buff,0,0,0,0,0,0,0);
				wexit(ERRORCODE(46));
			}
			if (SPECIAL_KEY == kw->special)
			{
				sprintf(buff,"Keyword=%8.8s Not resolved",keyword);
				werrlog(ERRORCODE(46),buff,0,0,0,0,0,0,0);
				wexit(ERRORCODE(46));
			}
			free(p->value);							/* Free the old VALUE			*/
			p->len = a_int2(&kw->value_len);				/* Set the new length			*/
			p->value = (char *)wcalloc((int)(p->len+1),(int)sizeof(char));	/* Get space for new VALUE		*/
			memcpy(p->value, (char *)kw + sizeof(KEYWSHM) + KEYWORD_SIZE, (int)p->len);/* Load new VALUE.		*/
			p->special = SPECIAL_NOT;					/* No longer special			*/
		}
	}

	if (had_special)
	{
		/*
		**	There was SPECIAL keywords found so rewite the PRB.
		*/
		rewrite_prb(prb_ptr,fmtlist);
	}
	free_fmtlist(fmtlist);

	if ((*prb_ptr)->reflbl[0] && 0 != memcmp((*prb_ptr)->reflbl,"        ",LABEL_SIZE))	
											/* If there is a reference label then	*/
	{
		SHMH	*ref_prb;

		/*
		**	Find the reference PRB and update the PRB with it's fmtlist.
		**	Don't worry if not found as it may have been deleted. (this could happen if usage_cnt > 1)
		**	If cleanup then delete the referenced PRB.
		*/
		ref_prb = get_prb_area(NULL,(*prb_ptr)->reflbl,OK_USED);	/* Get the referenced PRB			*/
		if (ref_prb)
		{
			FMTLIST	*ref_fmtlist;
			ret = load_fmtlist(ref_prb,&ref_fmtlist);		/* Load the reference fmtlist			*/

			if (0==ret && ref_fmtlist)				/* If all ok then UPDATE the PRB		*/
			{
				update_prb(prb_ptr,ref_fmtlist);
			}
			free_fmtlist(ref_fmtlist);

			if ('C' == (*prb_ptr)->cleanup || 'c' == (*prb_ptr)->cleanup)
			{
				/* NOTE: we have to get the ref_prb again because the update_prb() invalidated the pointer	*/
				ref_prb = get_prb_area(NULL,(*prb_ptr)->reflbl,OK_USED);
				erase_prb(ref_prb);
			}
		}
	}
	return ret;
}

/*
**	Routine:	update_prb()
**
**	Function:	To merge a fmtlist into a PRB updating the PRB in shared memory.
**
**	Description:	This routine is passed a PRB pointer and a fmtlist.
**			Load the PRB values into a fmtlist and merge the two lists then write the result to the PRB.
**			If PRB has no values then use the complete fmtlist. 
**			If the fmtlist is null or no values have changed then don't modify the PRB
**
**	Arguments:
**	prb_ptr		Pointer to the PRB.
**	fmtlist		The fmtlist to merge into the PRB.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	08/28/92	Written by GSL
**
*/

int update_prb(SHMH **prb_ptr, FMTLIST *fmtlist)
{
	int4	ret;

	werrlog(ERRORCODE(1),"update_prb",0,0,0,0,0,0,0);

	if ( !prb_ptr || ! *prb_ptr ) return(0);				/* No PRB ptr					*/
	if ( !fmtlist ) return(0);						/* Nothing to merge				*/

	ret = 0;

	if ((*prb_ptr)->keyw_cnt > 0)
	{
		/*
		**	If the PRB has keywords then all we want to do is merge the fmtlist into this PRB.
		*/

		FMTLIST	*prb_fmtlist;

		ret = load_fmtlist(*prb_ptr,&prb_fmtlist);			/* Load the fmtlist from the PRB		*/
		if (0==ret)
		{
			ret = merge_fmtlist(fmtlist,&prb_fmtlist);		/* Merge the fmtlist into the PRB fmtlist	*/
			/* if something is merged then ret == 0 */
		}
		if (0==ret)							/* Update the PRB directly in memory		*/
		{
			rewrite_prb(prb_ptr,prb_fmtlist);
		}

		free_fmtlist(prb_fmtlist);
	}
	else
	{	
		/*
		**	This PRB doesn't have any keywords so we are going to use all the keywords from this fmtlist.
		*/
		rewrite_prb(prb_ptr,fmtlist);
	}

	return(ret);
}

/*
**	Routine:	rewrite_prb()
**
**	Function:	To rewrite a PRB with an updated fmtlist.
**
**	Description:	This routine is passed a PRB pointer and a fmtlist.
**			If the fmtlist can be written in the existing space it will be, otherwise a new space will
**			be used and the PRB moved to the new location.
**
**	Arguments:
**	prb_ptr		Pointer to the PRB.
**	fmtlist		The updated fmtlist.
**
**	Globals:
**	shr_header	The shared memory header pointer.
**	pr_table	The shared memory lookup table
**	pr_data		The shared memory data pointer
**	
**
**	Return:		0
**
**	Warnings:	The PRB may be moved to a new location. This may cause a compress to occur and this will invalidate
**			any PRB pointers.
**
**	History:	
**	09/01/92	Written by GSL
**
*/

static int rewrite_prb(SHMH **prb_ptr, FMTLIST *fmtlist)
{
	int	cnt,mem;

	werrlog(ERRORCODE(1),"rewrite_prb",0,0,0,0,0,0,0);

	size_fmtlist(fmtlist,&cnt,&mem);					/* Get the size of the fmtlist			*/
	if ((*prb_ptr)->prb_size >= mem)					/* If updated fmtlist will fit in PRB memory	*/
	{
		/*
		**	Rewrite in place.
		*/
		write_fmtlist((*prb_ptr),fmtlist);				/* Write out the fmtlist into this PRB		*/
		(*prb_ptr)->keyw_cnt = cnt;					/* These should already be equal.		*/
	}
	else
	{
		/*
		**	Move PRB to new location and expand.
		*/
		SHMH	*new_prb;
		int	off;
		int	i;
		int2	update_id;

		update_id = (*prb_ptr)->prb_id;					/* Get the ID for this PRB			*/

		size_fmtlist(fmtlist,&cnt,&mem);				/* Get the size of the fmtlist			*/
		mem = roundup4(mem);						/* Round up the memory needed			*/

		/*
		**	Get a new PRB data area for the updated PRB.
		**
		**	NOTE:	A get_sh_raw() can cause a compress_data() to occur so any pointers into the data area are
		**		made invalid and must be recalculated.
		*/

		new_prb = (SHMH *)get_sh_raw(mem);
		off = off_prbid(update_id);					/* Get offset into data area			*/
		if (off == -1)
		{
			werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);			/* PRBs are corrupt				*/
			wexit(ERRORCODE(8));
		}
		*prb_ptr = (SHMH *)(pr_data + off);				/* Recalculate PRB pointer			*/

		/*
		**	Load the data into the new PRB area.
		**
		**	Load the PRB header and update the header values.
		*/

		memcpy(new_prb,*prb_ptr,sizeof(SHMH));				/* Copy the PRB header				*/

		new_prb->prb_size = mem;
		new_prb->keyw_cnt = cnt;

		write_fmtlist(new_prb,fmtlist);					/* Write the fmtlist to this PRB		*/

		/*
		**	Unhook the old PRB
		*/

		(*prb_ptr)->prb_id = 0;
		(*prb_ptr)->status = P_DELETED;

		/*
		**	Update the table to point to new PRB
		*/

		for (i=0; i < shr_header->num_parms; i++)
		{
			if ( update_id == pr_table[i].prb_id )
			{
				pr_table[i].proff = off_prbid(update_id);
				break;
			}
		}

		*prb_ptr = new_prb;						/* Update the PRB pointer			*/
	}

	return(0);
}

/*
**	Routine:	ppunlink()
**
**	Function:	To unlink the putparms (delete or mark as used).
**
**	Description:	This routine is called when a link-level terminates.
**			It will delete all putparms at this link-level.
**			And erase all putparms at higher link-levels.  
**			An unlabeled putparm is deleted and a labeled
**			putparm is marked as used.
**
**	Arguments:
**	level		The link-level to delete from.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	08/18/92	Written by GSL
**
*/

int ppunlink(int level)
{
	int	cur_item;
	SHMH	*ptr_shmh;
	int 	done;

	wtrace("PPUNLINK","ENTER","Unlink the putparms for level=%d",level);

	done = 0;
	while(!done)
	{
		done = 1;								/* Assume done				*/

		if (!osd_exists_seg()) return 0;					/* If shrfil doesn't exist		*/
		if (map_shr_ptrs()) return 0;						/* Unsuccessful map to shared mem area. */

		for (cur_item = shr_header->num_parms - 1; cur_item >= 0; cur_item--)	/* Loop (backwards) for each PUTPARM 	*/
		{
			if ((int)(pr_table[cur_item].linklevel) >= level)		/* If link-level >= current		*/
			{
				ptr_shmh = (SHMH *)(pr_data + pr_table[cur_item].proff); /* Get a pointer to the data.		*/

				if ((int)(pr_table[cur_item].linklevel) == level)	/* Current levels get deleted		*/
				{
					if (P_OK == ptr_shmh->status)
					{
						delete_prb(ptr_shmh);
						done = 0;				/* Set not done flag			*/
						break;
					}
				}
				else							/* Higher numbered levels get erased	*/
				{
					erase_prb(ptr_shmh);
					done = 0;					/* Set not done flag			*/
					break;
				}
				/*
				**	NOTE:	erase_prb() and delete_prb() have side-effects; they alter the pr_table.
				**		We have to break out of the loop and restart after they are called.
				*/
			}
		}
	}

	cleanup_shrfil();
	return 0;
}

/********************* Begin UNIX specific shared memory routines ***************************************************************/
#ifdef unix

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>

static	int 	osd_shmid = -1;							/* The shared memory segment identifier		*/
static	char	*osd_shmaddr = NULL;						/* The shared memory mapped address		*/


/*
**	Routine:	osd_open_seg()		UNIX
**
**	Function:	To create/open the shared memory segment.
**
**	Description:	This routine will ensure that a shared memory segment exists.
**			This is indicated by "osd_shmid" being set (not equal to -1).
**
**	Arguments:
**	new		Flag if new segment was created. (Segment needs to be initialized.)
**
**	Globals:
**	osd_shmid	The shared memory segment identifier.
**
**	Return:		0	Shared memory segment now exists.
**			-1	Failure  (No return - wexit called)
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static int osd_open_seg(int *new)
{
	FILE 	*shrtmpfile;
	key_t shmkey, wftok();
	char	buff[256];
	const char *shr_file;
	int	created_ctlfile;
	int	retry_cnt;
	int	save_errno = 0;	

	*new = 0;								/* Flag it as an old file.			*/
	if (osd_shmid != -1) return(0);						/* If already exists then just return		*/

	/*
	 *	It is very important to keep the ctlfile and the shared memory segment
	 *	in sync.
	 *
	 *	If the ctlfile doesn't exist then the shared memory segment MUST not exist,
	 *	if it does exist then we are out of sync.
	 *
	 *	If we get out of sync we can fix it by renaming the ctlfile (this will
	 *	preserve the inode/shmkey) then create a new ctlfile (new inode).
	 *
	 *	Likewise if the ctlfile exists then so must the segment or we are out of sync.
	 *	In this case attempt to delete the ctlfile.
	 */

	shr_file = wisp_sm_ctlfile();						/* Get pointer to key file name			*/

	retry_cnt = 5;	
  retry_create:
	if (0 >= retry_cnt--)
	{
		/* Give up */
		werrlog(ERRORCODE(32),shr_file,save_errno,0,0,0,0,0,0);
		werrlog(104,"%SHAREMEM-F-RETRY Giving up after 5 retries.",0,0,0,0,0,0,0);
		wexit(ERRORCODE(32));
	}	
	
	*new = 0;
	created_ctlfile = 0;

	if (!osd_exists_seg())							/* If key file doesn't exist then create it.	*/
	{
		if ((shrtmpfile = fopen(shr_file,FOPEN_WRITE_BINARY)) == NULL)	/* The creation of a file is only used		*/
		{								/* to get a unique inode number to use		*/
			werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);	/* as an address into the unix shared		*/
			wexit(ERRORCODE(32));					/* memory area.					*/
		}
		fprintf(shrtmpfile,"\n");
		fclose(shrtmpfile);						/* Need to keep around so doesn't use		*/
										/* same inode.					*/
		*new = 1;							/* Mark as new.					*/
		created_ctlfile = 1;
	}

	shmkey = wftok(shr_file);						/* Generate the shared memory key		*/

	if (shmkey == -1)
	{
		sprintf(buff,"errno %d/shr_file=%s\n\015",errno,shr_file);	/* should *Never* happen			*/
		werr_message_box(buff);

		if (0 == rename(shr_file, tempnam(wispprbdir(NULL),"TMP_")))
		{
			goto retry_create;
		}

		unlink(shr_file);
		werrlog(104,"%SHAREMEM-F-FTOK Unable to create key from file.",0,0,0,0,0,0,0);
		wexit(ERRORCODE(32));		
	}

	if (created_ctlfile)
	{
		osd_shmid = shmget(shmkey,max_pages()*W_PAGE_SIZE,IPC_CREAT|IPC_EXCL|0600);/* Get a shared memory segment.	*/
		if (osd_shmid == -1)
		{
			save_errno = errno;
			werrlog(ERRORCODE(34),shmkey,save_errno,0,0,0,0,0,0);

			if (save_errno == EEXIST)				/* Shared memory segment exists already		*/
			{
				/*
				 *	Ctlfile was created but segment already existed.
				 *	Rename and try again.
				 */
				if (0 == rename(shr_file, tempnam(wispprbdir(NULL),"TMP_")))
				{
					goto retry_create;
				}
			}

			/* Give up */
			unlink(shr_file);
			wexit(ERRORCODE(34));
		}
	}
	else 
	{
		/* 
		 * 	ctlfile already existed so the segment must exist
		 */
		osd_shmid  = shmget(shmkey,0,0);			/* Get the id associated with key.		*/
		if (osd_shmid == -1)					/* Error getting segment.			*/
		{
			save_errno = errno;
			werrlog(ERRORCODE(34),shmkey,save_errno,0,0,0,0,0,0);

			if (0 == unlink(shr_file))
			{
				goto retry_create;
			}

			/* Give up */
			unlink(shr_file);
			wexit(ERRORCODE(34));
		}
	}

	/*
	 *	Go ahead and do the osd_map_seg() logic so we can do the retry.
	 */
	osd_shmaddr = (char *)shmat(osd_shmid,(char *)0,0);
	if ( (int4)osd_shmaddr == -1 )
	{
		save_errno = errno;
		werrlog(ERRORCODE(36),osd_shmid,errno,0,0,0,0,0,0);

		osd_shmaddr = NULL;

		if (0 == rename(shr_file, tempnam(wispprbdir(NULL),"TMP_")))
		{
			goto retry_create;
		}
	}

	return(0);
}

/*
**	Routine:	osd_delete_seg()	UNIX
**
**	Function:	To delete the shared memory segment.
**
**	Description:	This routine deletes the shared memory segment.
**			It first unmaps the segment then removes it then deletes the key file.
**			It ensures the osd globals are maintained.
**
**	Arguments:	None
**
**	Globals:	
**	osd_shmid	The shared memory segment identifier.
**	osd_shmaddr	The shared memory segment mapped address.
**
**	Return:
**	0		Success
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**	09/01/93	Replaced the call to osd_unmap_seg() with inline code. GSL
**
*/
static int osd_delete_seg(void)
{
	int	new;
	char	errmsg[256];

	if (osd_exists_seg())
	{
		if (osd_shmid == -1)
		{
			osd_open_seg(&new);					/* Get the osd_shmid set			*/
		}
#ifdef OLD
		This was causing problems because it set osd_shmid==-1 which caused the shmctl() to fail.
		Replaced it with the call to shmdt().

		osd_unmap_seg();						/* Ensure it is unmapped			*/
#endif
		if (osd_shmaddr != NULL) 					/* if not mapped			*/
		{
			if (0 != shmdt(osd_shmaddr))				/* Detach the segment			*/
			{
				sprintf(errmsg,"%%SHAREMEM-F-SHMDT Unable to detach shared memory. errno=%d",errno);
				werrlog(104,errmsg,0,0,0,0,0,0,0);
			}
		}
				
		if (0 != shmctl(osd_shmid,IPC_RMID,0))				/* Remove the shared memory			*/
		{
			sprintf(errmsg,"%%SHAREMEM-F-SHMCTL Unable to remove shared memory. shmid=%d errno=%d", osd_shmid, errno);
			werrlog(104,errmsg,0,0,0,0,0,0,0);

			if ( 0 != rename(wisp_sm_ctlfile(), tempnam(wispprbdir(NULL),"TMP_")))
			{
				werrlog(ERRORCODE(16),wisp_sm_ctlfile(),errno,0,0,0,0,0,0);
			}
		}
		else
		{
			if (0 != unlink(wisp_sm_ctlfile()))			/* Delete the key file.				*/
			{
				werrlog(ERRORCODE(16),wisp_sm_ctlfile(),errno,0,0,0,0,0,0);
			}
		}
	}
	osd_shmid = -1;
	osd_shmaddr = NULL;
	return(0);
}

/*
**	Routine:	osd_map_seg()		UNIX
**
**	Function:	To map the shared memory segment into program address space.
**
**	Description:	This routine maps the shared memory segment into then address space.
**			It uses the shmat() routine to do this.
**
**	Arguments:	None
**
**	Globals:	
**	osd_shmid	The shared memory segment identifier.
**	osd_shmaddr	The shared memory segment mapped address.
**	
**	Return:		Success:	The address of the mapped segment.  
**			Failure:	NULL 	(No return, wexit called)
**
**	Warnings:	The osd_shmaddr variable must be maintained for all to work correctly.
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static char *osd_map_seg(void)
{
	if (osd_shmaddr == NULL) 							/* if not mapped			*/
	{
		osd_shmaddr = (char *)shmat(osd_shmid,(char *)0,0);			/* Get the address of requested id.	*/
		if ( (int4)osd_shmaddr == -1 )
		{
			osd_shmaddr = NULL;
			osd_shmid = -1;
			
			werrlog(ERRORCODE(36),osd_shmid,errno,0,0,0,0,0,0);
			wexit(ERRORCODE(36));
		}
	}
	return(osd_shmaddr);
}

/*
**	Routine:	osd_unmap_seg()		UNIX
**
**	Function:	To unmap the shared memory segment from program address space.
**
**	Description:	This routine unmaps the shared memory segment.
**			It uses the shmdt() routine to do this.
**			It sets osd_shmid=-1 to force the next access to look it up.
**
**	Arguments:	None
**
**	Globals:
**	osd_shmaddr	The shared memory segment mapped address.
**	osd_shmid	The shared memory segment identifier.
**	
**	Return:
**	0		Success
**
**	Warnings:	The osd_shmaddr variable must be maintained for all to work correctly.
**
**	History:	
**	08/21/92	Written by GSL
**	03/02/93	Added setting osd_shmid=-1. GSL
**
*/

static int osd_unmap_seg(void)
{
	if (osd_shmaddr != NULL) 							/* if not mapped			*/
	{
		if ( 0 != shmdt(osd_shmaddr))						/* Detach the segment			*/
		{
			char errmsg[256];
			
			sprintf(errmsg,"%%SHAREMEM-F-SHMDT Unable to detach shared memory. errno=%d",errno);
			werrlog(104,errmsg,0,0,0,0,0,0,0);
		}
	}
	osd_shmaddr = NULL;
	osd_shmid = -1;
	return(0);
}

/*
**	Routine:	osd_exists_seg()	UNIX
**
**	Function:	To check if the shared memory segment exists.
**
**	Description:	This checks if the shared memory segment key file exists.
**			
**
**	Arguments:	None
**
**	Globals:
**	osd_shmaddr	The shared memory segment mapped address.
**	
**	Return:
**	1		Shared memory segment exists
**	0		Doesn't exist.
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static int osd_exists_seg(void)
{
	return(fexists(wisp_sm_ctlfile()));
}

/*
**	Routine:	osd_sync_seg()		UNIX
**
**	Function:	To synchronize the external shared memory area with the internal area.
**
**	Description:	This routine ensures that the external area is updated to match the internal area.
**			On UNIX this function has no effect as they are always synchronized.
**
**	Arguments:	None
**
**	Globals:	None
**	
**	Return:		0
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static int osd_sync_seg(void)
{
	return 0;
}

/*
	wisp_sm_ctlfile()	Generate control file name.	UNIX
*/
const char *wisp_sm_ctlfile(void)
{
static	char	buff[256];
static	int	first = 1;
	if (first)
	{
		first = 0;

		if (get_wisp_option("SHAREDMEMORYV1"))
		{
			sprintf(buff,"%s/w2$sh%04X",wispprbdir(NULL), wgetpgrp());	/* Construct V1 style filepath to use. 	*/
		}
		else
		{
			sprintf(buff,"%s/shmemkey_%s_%u.shm",wispprbdir(NULL), longuid(), (unsigned)wgetpgrp());
		}
		
		wtrace("SHAREMEM","CTLFILE", "Control file=[%s]", buff);
		makepath(buff);							/* Ensure the directory exists			*/
	}
	return(buff);
}

#endif	/* #ifdef unix	*/								/* End of UNIX specific memory code.	*/

/********************* Begin WIN32 specific shared memory routines *******************************************************/
#if defined(WIN32)

static	char	*osd_shmaddr = NULL;

/*
**	Routine:	osd_open_seg()		WIN32
**
**	Function:	To create/open the shared memory segment.
**
**	Description:	This routine will ensure that a shared memory segment exists.
**			We use a file to hold the shared memory seg and we malloc memory to copy the file into memeory.
**
**	Arguments:
**	new		Flag if new segment was created. (Segment needs to be initialized.)
**
**	Globals:
**	osd_shaddr	The shared memory segment address pointer.
**
**	Return:		0	Shared memory segment now exists.
**			-1	Failure  (No return - wexit called)
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**
*/
static int osd_open_seg(int *new)
{
	FILE 	*shrtmpfile;
	const char *shr_file;

	*new = 0;								/* Flag it as an old file.			*/
	if (osd_shmaddr) return(0);						/* If already exists then just return		*/

	shr_file = wisp_sm_ctlfile();						/* Get pointer to key file name			*/

	if ( !osd_exists_seg() )						/* If segment file doesn't exist then create.	*/
	{
		if ((shrtmpfile = fopen(shr_file,FOPEN_WRITE_BINARY)) == NULL)
		{
			werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);
			wexit(ERRORCODE(32));
		}
		fclose(shrtmpfile);
		*new = 1;
	}

	return(0);
}

/*
**	Routine:	osd_delete_seg()	WIN32
**
**	Function:	To delete the shared memory segment.
**
**	Description:	This routine deletes the shared memory segment.
**			It first unmaps the segment then removes it then deletes the key file.
**			It ensures the osd globals are maintained.
**
**	Arguments:	None
**
**	Globals:	
**	osd_shmaddr	The shared memory segment mapped address.
**
**	Return:
**	0		Success
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**
*/
static int osd_delete_seg(void)
{
	if (osd_exists_seg())
	{
		osd_unmap_seg();						/* Ensure it is unmapped			*/

		if (unlink(wisp_sm_ctlfile()))					/* Delete the key file.				*/
		{
			werrlog(ERRORCODE(16),wisp_sm_ctlfile(),errno,0,0,0,0,0,0);	/* Error when deleting key file.	*/
			wexit(ERRORCODE(16));
		}
	}
	osd_shmaddr = NULL;
	return(0);
}

/*
**	Routine:	osd_map_seg()		WIN32
**
**	Function:	To map the shared memory segment into program address space.
**
**	Description:	This routine mallocs memory then reads the file into it.
**
**	Arguments:	None
**
**	Globals:
**	osd_shmaddr	The shared memory segment mapped address.
**	
**	Return:		Success:	The address of the mapped segment.  
**			Failure:	NULL 	(No return, wexit called)
**
**	Warnings:	The osd_shmaddr variable must be maintained for all to work correctly.
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static char *osd_map_seg(void)
{
	FILE 	*shrtmpfile;
	const char *shr_file;

	if (osd_shmaddr == NULL) 						/* if not mapped				*/
	{
		shr_file = wisp_sm_ctlfile();					/* Get the file name				*/
		osd_shmaddr = wmalloc( max_pages() * W_PAGE_SIZE );		/* Malloc the space				*/

		if ((shrtmpfile = fopen(shr_file,FOPEN_READ_BINARY)) == NULL)	/* Open file for reading			*/
		{
			werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);
			unlink(shr_file);
			wexit(ERRORCODE(32));
		}
		fread( osd_shmaddr, W_PAGE_SIZE, max_pages(), shrtmpfile );
		fclose(shrtmpfile);
	}
	return(osd_shmaddr);
}

/*
**	Routine:	osd_unmap_seg()		WIN32
**
**	Function:	To unmap the shared memory segment from program address space.
**
**	Description:	This routine frees the malloced memory.
**
**	Arguments:	None
**
**	Globals:
**	osd_shmaddr	The shared memory segment mapped address.
**	
**	Return:
**	0		Success
**
**	Warnings:	The osd_shmaddr variable must be maintained for all to work correctly.
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static int osd_unmap_seg(void)
{
	if (osd_shmaddr != NULL) 						/* if not mapped				*/
	{
		free(osd_shmaddr);						/* free the memory				*/
	}
	osd_shmaddr = NULL;
	return(0);
}

/*
**	Routine:	osd_exists_seg()	WIN32
**
**	Function:	To check if the shared memory segment exists.
**
**	Description:	This checks if the shared memory segment file exists.
**			
**	Arguments:	None
**
**	Globals:	
**	
**	Return:
**	1		Shared memory segment exists
**	0		Doesn't exist.
**
**	Warnings:	None
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static int osd_exists_seg(void)
{
	return(fexists(wisp_sm_ctlfile()));
}

/*
**	Routine:	osd_sync_seg()		WIN32
**
**	Function:	To synchronize the external shared memory area with the internal area.
**
**	Description:	This routine ensures that the external area is updated to match the internal area.
**			Write the contents of the memory area to the file.
**
**	Arguments:	None
**
**	Globals:
**	osd_shmaddr	The shared memory segment mapped address.
**	
**	Return:		0
**
**	Warnings:	Must be mapped!
**
**	History:	
**	08/21/92	Written by GSL
**
*/

static int osd_sync_seg(void)
{
	FILE 	*shrtmpfile; 
	const char *shr_file;

	shr_file = wisp_sm_ctlfile();
	if ((shrtmpfile = fopen(shr_file,FOPEN_WRITE_BINARY)) == NULL)
	{
		werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);
		unlink(shr_file);
		wexit(ERRORCODE(32));
	}
	fwrite( osd_shmaddr, W_PAGE_SIZE, max_pages(), shrtmpfile );
	fclose(shrtmpfile);
	return 0;
}

/*
	wisp_sm_ctlfile()	Generate control file name.	WIN32
*/
const char *wisp_sm_ctlfile(void)
{
static	char	*the_name = NULL;

	if (NULL==the_name)
	{
		char buff[256];

		/* Construct WIN32 filepath to use.	*/
		if (get_wisp_option("SHAREDMEMORYV1"))
		{
			sprintf(buff, "%s\\W%04X.shm", wispprbdir(NULL), wgetpgrp());		
		}
		else
		{
			sprintf(buff, "%s\\shmemkey_%s_%u.shm",wispprbdir(NULL), longuid(), (unsigned)wgetpgrp());
		}

		the_name = wstrdup(buff);

		wtrace("SHAREMEM","CTLFILE", "Control file=[%s]", the_name);
		makepath(the_name);
	}
	return(the_name);
}

#endif	/*  WIN32 */


/*
**	History:
**	$Log: sharemem.c,v $
**	Revision 1.26  2001-10-31 15:42:49-05  gsl
**	Rename ctlfile() to wisp_sm_ctlfile() and make global.
**	replace mkdir() with makepath()
**
**	Revision 1.25  2001-10-10 15:45:15-04  gsl
**	Change the sharemem key file to include both userid and pgid
**
**	Revision 1.24  2001-10-10 15:06:23-04  gsl
**	Remove VMS & MSDOS
**
**	Revision 1.23  2000-06-19 11:49:00-04  gsl
**	ALways report a problem even if going to retry.
**	Add SHAREDMEMORYV1 for compatibility.
**
**	Revision 1.22  2000-06-16 19:23:43-04  gsl
**	For UNIX, add error detection and correction and retry logic for creating
**	and attaching to shared memory.
**	For the ctlfile and the segment to be in sync.
**	If the ctlfile needed to be created then the segment must not already exist.
**	If an out-of-sync condition is detected then rename the ctlfile and
**	retry the operation, this will create a new shared memory key (if we just
**	deleted the file the inode code be reused).
**
**	Revision 1.21  1998-08-03 17:12:58-04  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**
**	Revision 1.20  1997-07-16 15:08:33-04  gsl
**	Add wtrace() to report the control file name
**
**	Revision 1.19  1997-05-02 12:00:20-04  gsl
**	Replaced the werrlog() stuff with wtrace() with useful info
**
**	Revision 1.18  1997-04-03 20:00:00-05  gsl
**	Remove unneeded traces
**
**	Revision 1.17  1996-09-10 11:46:35-04  gsl
**	fix mkdir() code
**
**	Revision 1.16  1996-08-23 14:04:25-07  gsl
**	Changed to use wispprbdir()
**
**	Revision 1.15  1996-08-22 17:27:25-07  gsl
**	Change cntfile() for DOS and NT to use wgetpgrp()
**
**	Revision 1.14  1996-07-17 14:52:57-07  gsl
**	Change to use wmalloc()
**
**	Revision 1.13  1996-07-11 16:21:40-07  gsl
**	Fix to use FOPEN mode defines for NT
**
**	Revision 1.12  1996-07-09 16:49:26-07  gsl
**	Fix missing includes, reuse MSDOS code for NT.
**	Fix get_prb_id() to take an int2 arg.
**
**	Revision 1.11  1995-08-25 08:18:38-07  gsl
**	In backwards_reference() the logic that handles SPECIAL_KEYs that
**	come from backwards reference of individual keywords (this feature
**	is only accessable from wproc) was ref value_len which was
**	an unaligned int2.  This would sometimes cause a SIGNAL 10 on
**	the second keyword (the first keyword was always aligned).
**
 * Revision 1.10  1995/05/15  13:05:42  gsl
 * Added routine erase_prb_label_level() which is called from write_putparm()
 * to delete an existing putparm with the same label at the same linklevel.
 * Also replace numeric literals with meaningful defines LABEL_SIZE, KEYWORD_SIZE, PRNAME_SIZE.
 *
**
**
*/
