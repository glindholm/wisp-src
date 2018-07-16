			/************************************************************************/
			/*									*/
			/*		WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	NAME:	sharemem.c
*/

/* WISP shared memory access routines.												*/

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

#include <stdio.h>

#ifndef VMS	/* unix and MSDOS */

#include <errno.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef unix
#include <sys/ipc.h>
#include <sys/shm.h>
#endif

#ifdef MSDOS
#include <direct.h>
#include <io.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#endif

#endif	/* unix and MSDOS */

#ifdef VMS
#include <psldef.h>
#include <secdef.h>
#include <rms.h>
#include <ssdef.h>
#include <jpidef.h>
#include <descrip.h>
#include <stdlib.h>
#endif

#include "werrlog.h"
#include "wshmem.h"
#include "wdefines.h"

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
*/

#define PAGE_SIZE	512

#ifdef MSDOS
#define DEF_MAX_PARMS	64
#define DEF_MAX_PAGES	20
#else
#define DEF_MAX_PARMS	128
#define DEF_MAX_PAGES	40
#endif

static	int max_parms=DEF_MAX_PARMS;							/* Maximum number of prb's in the sect. */
static	int max_pages=DEF_MAX_PAGES;							/* Max number of pages in the sec file. */

extern	int opt_max_parms;								/* OPTIONS file max parms.		*/
extern	int opt_max_pages;								/* OPTIONS file max pages.		*/

extern int create_gbl;									/* Indicate creation of shared mem file.*/

#define OLD_VERSION	"V2.0_2"
#define SHM_VERSION	"V3.0_1"

char gp_pp_version[7] = SHM_VERSION;							/* Version indicator for GETPARM/PUTPARM*/
static char cur_version[7];								/* Current version indicator.		*/


typedef struct sh_mem_header
	{
		char	gp_pp_ver[8];							/* The version number			*/
		short	cur_prb_id;							/* Current prb id number		*/
		short	num_parms;							/* The current number of parms stored.	*/
		short	max_num_parms;							/* The maximum number of parms		*/
		short	max_num_pages;							/* The maximum number of pages		*/
		long	unused_off;							/* Offset of unused data area		*/
		char	filler[24];							/* Set up for futrue use.		*/
	} gp_pp_header;
static gp_pp_header *shr_header;							/* Pointer to header of shared memory.	*/

struct gbl_parm_table
	{
		long	proff;								/* Offset of the data in the file.	*/
		short	prb_id;								/* Id to link with data area		*/
		char	prname[9];							/* prname				*/
		char	label[8];							/* The PUTPARM label.			*/
		char	linklevel;							/* The link level			*/
		char	filler[4];							/* Filler for future use.		*/
	};
typedef struct gbl_parm_table gbl_parm_table;

static char *gbl_ptr = (char *) 0;							/* The base pointer of the global mem.	*/
											/* Initial part of data in global area. */
static gbl_parm_table *pr_table;							/* The table. It has MAX_PARMS items.	*/
static char *pr_data;									/* The pointer to the data.		*/
static char *end_data;									/* A pointer to the end of all the data.*/
static new_file;									/* Flag indicating a new file.		*/
static short shrchan = 0;
static char *parm_ptr;									/* A scratch pointer.			*/
static int found_shr_mem;								/* Found the shared memory file flag.	*/


KEYWSHM *find_prb_keyword();
char	*strchr();
char	*get_prb_area();
short	a_short();
static	long	off_prbid();

#ifndef VMS	/* unix and MSDOS */
static FILE *shrtmpfile;
extern int PGRPID;
#endif	/* unix and MSDOS */

#ifdef VMS
static char filename[128], secname[128];						/* Output from $PARSE and $SEARCH	*/
static $DESCRIPTOR(gsdnam,secname);							/* Descriptor of section name.		*/
static struct FAB fab;									/* RMS data structures.			*/
static unsigned long master_pid;							/* The PID of the master process.	*/
static struct	{
		short unsigned int	buflen;						/* the length of the buffer		*/
		short unsigned int	item_code;					/* the code for the request to GETDVI	*/
		char			*bufptr;					/* a pointer to the buffer		*/
		short unsigned int	*retlen;					/* the return length of the buffer	*/
		long int		endbuf;						/* the end of the buffer		*/
	} mybuf;

static unsigned long map_range[2] = { 0x0200, 0x0200 };					/* Map to the end of the P0 region.	*/
static char *ret_range[2];								/* Return address of where it is.	*/
#endif	/* VMS */

int roundup4(x)
int	x;
{
	return( (x+3) & ~(3l) );
}

/*
**	init_parm_area		Initialize the shared memory area.
**				This is called once after shared memory area is created.
*/
static int init_parm_area()								/* Init the parm area.			*/
{
	werrlog(ERRORCODE(1),"init_parm_area",0,0,0,0,0,0,0);

	memset(gbl_ptr,'\0',(max_pages * PAGE_SIZE));					 /* Init to nulls			 */

	shr_header = (gp_pp_header *)gbl_ptr;						/* Pointer to the header section.	*/

	memcpy(shr_header->gp_pp_ver,gp_pp_version,6);					/* Copy version to gbl file.		*/
	shr_header->cur_prb_id = 1;							/* Starting prb_id			*/
	shr_header->num_parms = 0;							/* init number of parms.		*/
	shr_header->max_num_parms = max_parms;						/* Record the sizes			*/
	shr_header->max_num_pages = max_pages;
	shr_header->unused_off = 0;							/* Point to start of data area		*/

	map_global_ptrs();

	if ( pr_data > end_data )
	{
		werrlog(ERRORCODE(2),max_parms,max_pages,0,0,0,0,0,0);			/* Not enough global memory.		*/
		wexit(ERRORCODE(2));
	}

#ifdef MSDOS
	write_parms_to_file();								/* Save all parms to MSDOS file.	*/
#endif
	return(0);
}

/*
	get_sh_raw	Get a chunk of shared mem data area.
*/
char *get_sh_raw(size)
int size;
{
	int	status, cur_item;
	char	*dat_ptr;

	werrlog(ERRORCODE(1),"get_sh_raw",0,0,0,0,0,0,0);

	size = roundup4(size);

	if (status = map_shr_ptrs()) return(NULL);					/* Unsuccessful map to shared memory.	*/

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
char *get_sh_seg(prname,label,size,id,rsize)						/* Get a pointer to shared mem area and */
char *prname, *label;									/* Allocate the memory needed.		*/
int size;
int *id;
int *rsize;
{
	int status, i, cur_item, ptr_offset;
	char *dat_ptr, *ptr1, *ptr2;

	werrlog(ERRORCODE(1),"get_sh_seg",0,0,0,0,0,0,0);

	size = roundup4(size);
	*rsize = size;

	if (status = map_shr_ptrs()) return(NULL);					/* Unseccussful map to shared memory.	*/

	if (shr_header->num_parms == shr_header->max_num_parms)				/* Max parms exceeded.			*/
	{
		werrlog(ERRORCODE(6),shr_header->max_num_parms,0,0,0,0,0,0,0);
		wexit(ERRORCODE(6));
	}

	cur_item = shr_header->num_parms;						/* This is the index of the current item*/
	memcpy(pr_table[cur_item].prname,prname,8);					/* Copy the prname.			*/
	memcpy(pr_table[cur_item].label,label,8);					/* Copy the PUTPARM label.		*/
	pr_table[cur_item].prb_id = shr_header->cur_prb_id;
	*id			  = shr_header->cur_prb_id;
	pr_table[cur_item].proff = shr_header->unused_off;
	pr_table[cur_item].linklevel = (char) getlinklevel();

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
finish_sh_seg(size)
int	size;
{
	werrlog(ERRORCODE(1),"finish_sh_seg",0,0,0,0,0,0,0);

	shr_header->num_parms += 1;							/* Add 1.				*/
	shr_header->cur_prb_id += 1;
	shr_header->unused_off += size;
}

/*
	get_prb_id	This routine returns a pointer to the PRB identified by id or NULL if not found.
*/
SHMH *get_prb_id(id)
int id;
{
	int	off;
	SHMH	*prb_ptr;

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
static long off_prbid(id)
short	id;
{
	SHMH	*ptr_shmh;
	char	*ptr;

	ptr = pr_data;
	for(;;)
	{
		ptr_shmh = (SHMH *)ptr;
		if (ptr_shmh >= (SHMH *)(pr_data + shr_header->unused_off))
		{
			return(-1);
		}
		if (ptr_shmh->prb_id == id )
		{
			return( (char *)ptr_shmh - pr_data );
		}
		ptr += ptr_shmh->prb_size;
	}
}

/*
	compress_data		This routine will compress the data area of shared memory by removing all deleted prbs
				and shifting all remaining prbs to lower memory. It will also update the pr_table offsets
				to point to the new locations.
*/
static compress_data()
{
	char	*ptr_source, *ptr_dest;
	long	len,i;
	SHMH	*ptr_shmh;

	werrlog(ERRORCODE(1),"compress_data",0,0,0,0,0,0,0);

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


static int map_shr_ptrs()								/* This routine maps the pointers	*/
{											/* in the shared memory file.		*/

#ifdef MSDOS
	int status;
#else
	unsigned long status;
#endif

	werrlog(ERRORCODE(1),"map_shr_ptrs",0,0,0,0,0,0,0);

	load_options();

	if (opt_max_parms >= 32 && opt_max_parms <= 512)
		max_parms = opt_max_parms;

	if (opt_max_pages >= 10 && opt_max_pages <= 200)
		max_pages = opt_max_pages;

	status = open_shrfil();								/* Open the shareable file.		*/
	if (status) return(status);							/* Was no file to open or problem.	*/

	status = get_gblptr();								/* Now get the global section pointer.	*/
	if (status)
	{
		werrlog(ERRORCODE(10),status,0,0,0,0,0,0,0);				/* Some kind of error.			*/
		wexit(ERRORCODE(10));
	}

											/* All ok, map the needed pointers.	*/
	shr_header = (gp_pp_header *)gbl_ptr;						/* Pointer to the header section.	*/

	memcpy(cur_version,shr_header->gp_pp_ver,6);					/* GETPARM/PUTPARM version indicator.	*/
	cur_version[6] = '\0';								/* Null terminate.			*/
	if (strcmp(cur_version,gp_pp_version) != 0)					/* Test the version number.		*/
	{
		werrlog(ERRORCODE(12),cur_version,gp_pp_version,0,0,0,0,0,0);		/* Version mismatch.			*/
		wexit(ERRORCODE(12));
	}										/* After that comes the table.		*/
	map_global_ptrs();
	return(0);
}

static map_global_ptrs()
{
	werrlog(ERRORCODE(1),"map_global_ptrs",0,0,0,0,0,0,0);

	shr_header = (gp_pp_header *)gbl_ptr;						/* Pointer to the header section.	*/

	pr_table = (gbl_parm_table *)(gbl_ptr + roundup4(sizeof(gp_pp_header)));	/* Pointer to where the table is.	*/
											/* After the table is the actual data.	*/
	pr_data =  (char *)((char *)pr_table + 
		   (roundup4(sizeof(gbl_parm_table) * shr_header->max_num_parms)));	/* Pointer to where the actual data is. */

	end_data = (char *) (gbl_ptr + (shr_header->max_num_pages * PAGE_SIZE));	/* Pointer to end of the mess.	(+1)	*/
}

int cleanup_shrfil()									/* Cleanup after users of the shared dat*/
{
	int status;

	werrlog(ERRORCODE(1),"cleanup_shrfil",0,0,0,0,0,0,0);

	if (!shrchan) return(0);							/* The file isn't open.			*/

	if (status = map_shr_ptrs()) return(0);						/* Unsuccessful map to shared memory.	*/

	if (0 == shr_header->num_parms)							/* Check to see if the file is empty.	*/
	{
		free_gblptr();								/* If it's empty, free the pointer.	*/
		close_shrfil(1);							/* Close the file and delete it.	*/
	}
	else
	{
		free_gblptr();								/* Now free the pointer.		*/
		close_shrfil(0);							/* Close with no delete.		*/
	}
#ifdef MSDOS
	write_parms_to_file();								/* Save all parms to MSDOS file.	*/
#endif
}

int close_shrfil(delflag)								/* Close the file.			*/
int delflag;										/* Flag to delete it.			*/
{											/* Really only have to deassign the I/O */
#ifdef MSDOS
	return(0);
#else	/* end MSDOS, start unix and VMS */

	unsigned long status;

	werrlog(ERRORCODE(1),"close_shrfil",0,0,0,0,0,0,0);

	if (!shrchan) return(0);							/* The file isn't open.			*/

#ifdef VMS
	status = sys$dassgn(shrchan);							/* Deassign it.				*/
	if (status != RMS$_NORMAL && status != SS$_NORMAL)
	{
		werrlog(ERRORCODE(14),status,0,0,0,0,0,0,0);				/* Die on error.			*/
		wexit(ERRORCODE(14));
	}
#endif
	shrchan = 0;									/* Deassign the channel.		*/

	if (delflag)									/* If no parms in file then delete.	*/
	{
#ifdef unix
		delete_shrfil();
#endif	
#ifdef VMS
		status = sys$erase(&fab);						/* Erase the file.			*/
		if (status == RMS$_FLK);						/* File currently locked by another user.*/
		else if (status != RMS$_NORMAL && status != SS$_NORMAL)
		{
			werrlog(ERRORCODE(18),status,0,0,0,0,0,0,0);			/* Error when deleteing memory area.	*/
			wexit(ERRORCODE(18));
		}
#endif
	}
	return(0);
#endif	/* #ifdef MSDOS #else (unix and VMS)   */
}

static int get_gblptr()									/* Map the Global Section file to the	*/
{											/* pointer "gbl_ptr".			*/
#ifdef VMS
	unsigned long status;

	if (gbl_ptr) return(0);								/* Already got it.			*/
	status = sys$crmpsc(	map_range,						/* Range to map to. (use end of p0)	*/
				ret_range,						/* Range actually mapped to.		*/
				PSL$C_USER,						/* Access mode (USER mode).		*/
				SEC$M_GBL | SEC$M_EXPREG | SEC$M_WRT,			/* It's GLOBAL, end of P0, WRITEable.	*/
				&gsdnam,						/* The name of the global section.	*/
				(long) 0,						/* Ident/version is 0.			*/
				(long) 0,						/* Relative page is 0.			*/
				shrchan,						/* The I/O channel to use. From $CREATE */
				max_pages,						/* Number of pages to map.		*/
				(long) 0,						/* Virtual block number is 0.		*/
				(long) 0x0330,						/* File protection, allow g:rw, o:rw.	*/
				(long) 3						/* Page fault cluster is 3.		*/
			);
	if (status != SS$_NORMAL && status != RMS$_NORMAL && status != SS$_CREATED) return(status); /* ERROR!!			*/

	gbl_ptr = ret_range[0];								/* Got it, set the global pointer.	*/
#endif
	if (new_file) init_parm_area();							/* If it's new, init the table.		*/
	new_file = 0;									/* Clear the "new" flag.		*/
	return(0);									/* All ok.				*/
}

free_gblptr()										/* Unmap from the global section.	*/
{
#ifdef VMS
	unsigned long status;
	struct
	{
		unsigned short c1;
		unsigned short c2;
		unsigned long vaddr;
	} my_iosb;
#endif

	werrlog(ERRORCODE(1),"free_gblptr",0,0,0,0,0,0,0);

#ifdef unix
	shmdt(gbl_ptr);
	gbl_ptr = 0;									/* Clear the pointer.			*/
	shrchan = 0;
#endif

#ifdef MSDOS
	return(0);
#endif

#ifdef VMS
	if (!gbl_ptr) return(0);							/* Already free.			*/
											/* Update the section file.		*/
	status = sys$updsecw(ret_range,(long) 0,(long) 0,(long) 0,(long) 0,&my_iosb,(long) 0,(long) 0);

	if (status != SS$_NORMAL && status != RMS$_NORMAL && status != SS$_NOTMODIFIED)
	{
		werrlog(ERRORCODE(20),status,0,0,0,0,0,0,0);
		wexit(ERRORCODE(20));							/* Error!!				*/
	}
	if (my_iosb.c1 != SS$_NORMAL && my_iosb.c1 != SS$_NOTMODIFIED)
	{
		werrlog(ERRORCODE(22),my_iosb.c1,0,0,0,0,0,0,0);
		wexit(ERRORCODE(22));							/* Error!!				*/
	}

	status = sys$deltva(ret_range,(long) 0, PSL$C_USER);				/* Delete the virtual addresses.	*/

	gbl_ptr = NULL;									/* Clear the pointer.			*/

	if (status == SS$_NORMAL || status == RMS$_NORMAL) return(0);
	else
	{
		werrlog(ERRORCODE(24),status,0,0,0,0,0,0,0);
		wexit(ERRORCODE(24));							/* Error!!				*/
	}
#endif	/* #ifdef VMS	*/
}

show_parm_area()									/* Display the available PUTPARMs.	*/
{
	int i, status;
	int	first=1;
	char	func[5];

	if (!shrfil_exists())
	{
		printf("No PRB's in global memory.\n");
		return(0);
	}

	status = map_shr_ptrs();
	if ( status )
	{
		if (found_shr_mem)
			printf("Unable to map shared pointers.\n");
		else
			printf("No PRB's in global memory.\n");
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

		for(j=0; j<pp->keyw_cnt; j++)
		{
			memcpy(&tkh,kh,sizeof(KEYWSHM));				/* Get temp align copy of *kh		*/

			if (bptr+a_short(&tkh.value_len)+8+3 >= buff+sizeof(buff)-5)
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

			memcpy(bptr,tptr, (int)a_short(&tkh.value_len));
			bptr += a_short(&tkh.value_len);
			*bptr++ = ']';
			*bptr++ = ' ';

			tptr = (char *)kh;
			tptr += tkh.next_offs;
			kh = (KEYWSHM *)tptr;

		}
		*bptr = '\0';
		

		if (pp->status == P_KEEP) strcpy(func,"KEEP");
		else
		{
			strcpy(func,"    ");
			func[1]=pp->type;
		}

		if (first)
		{
		first = 0;
		      /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
		      /*  XXXXXXXX XXXXXXXX  X	 XXXXX	  X  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX	 */
		printf("  PRNAME   LABEL    FUNC   COUNT AID DATA\n\n");
		}
		printf("  %8.8s %8.8s %s %5d    %c  %-42s\n",
			  pr_table[i].prname,
				pr_table[i].label,
				       func,
					    pp->usage_cnt,
						   pp->pfkey,
						       buff);
	}
	
	if (first)									/* If no global memory file found or	*/
	{										/* no PUTPARMS found.			*/
		printf("No PRB's in global memory.\n");
	}
	return(0);
}

ishow_parm_area()	/* Do an internals SHOW */
{
	int i, status;
	SHMH	*ptr_shmh;
	char	*ptr;

	if (!shrfil_exists())
	{
		printf("No PRB's in global memory.\n");
		return(0);
	}

	status = map_shr_ptrs();
	if ( status )
	{
		if (found_shr_mem)
			printf("Unable to map shared pointers.\n");
		else
			printf("Shared memory file does not exist. (No PRB's.)\n");
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
	      /*[XXX] XXXXX XXXXX  XXXXXXXX XXXXXXXX		*/
	printf("pr_table\n");
	printf("[POS] proff prb_id PRNAME   LABEL   \n");
	for (i = 0; i < shr_header->num_parms; i++)
	{
		printf("[%3d] %5d %5d  %8.8s %8.8s\n",
			i, pr_table[i].proff, pr_table[i].prb_id, pr_table[i].prname, pr_table[i].label);
	}

	      /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	      /*[XXX] XXXXXX XXXXXXXX XXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXX XXXX	   */
	printf("\n");
	printf("pr_data\n");
	printf("[POS] offset status   prb_id prb_size PRNAME   LABEL    ucnt kwcnt\n");
	ptr = pr_data;
	for(i=0;i>=0;i++)
	{
		char	stat[10];
		long	offset;

		ptr_shmh = (SHMH *)ptr;
		if (ptr_shmh >= (SHMH *)(pr_data + shr_header->unused_off))
		{
			break;
		}

		if	( ptr_shmh->status == P_OK )	  strcpy(stat,"OK     ");
		else if ( ptr_shmh->status == P_DELETED ) strcpy(stat,"DELETED");
		else if ( ptr_shmh->status == P_KEEP )	  strcpy(stat,"KEEP   ");

		offset = (char *)ptr_shmh - pr_data;

		printf("[%3d] %6d %8.8s %6d %8d %8.8s %8.8s %4d %4d\n",
			i, offset, stat, ptr_shmh->prb_id, ptr_shmh->prb_size, ptr_shmh->prname, ptr_shmh->label,
			ptr_shmh->usage_cnt, ptr_shmh->keyw_cnt);

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

dump_parm_area(filename)
char	*filename;
{
	FILE	*fp;
	int	status;
	char	*ptr;

	status = map_shr_ptrs();
	if ( status )
	{
		printf("Unable to map shared pointers.\n");
		return(0);
	}
	fp = fopen(filename,"w");
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
*/
wax_table_entry(parm_area)
char *parm_area;									/* Ptr to SHMH (PRB) area.		*/
{
	int	status,cur_item;
	int	dat_off;
	SHMH	*ptr_shmh;

	werrlog(ERRORCODE(1),"wax_table_entry",0,0,0,0,0,0,0);

	if (!gbl_ptr)									/* Not mapped.				*/
	{
		werrlog(ERRORCODE(26),0,0,0,0,0,0,0,0);					/* Error in WAX_PARM, global area not	*/
		wexit(ERRORCODE(26));							/*  mapped.				*/
	}

	if (!shr_header->num_parms) return(0);						/* Nothing there			*/

	dat_off = parm_area - pr_data;							/* Calculate the offset.		*/
											/* Look for the item with that offset.	*/
	for (cur_item=0; (cur_item < shr_header->num_parms) && (pr_table[cur_item].proff != dat_off); cur_item++);

	if (cur_item == shr_header->num_parms) return(0);				/* Was not found.			*/

	ptr_shmh = (SHMH *)parm_area;							/* Mark the SHMH as deleted.		*/
	ptr_shmh->status = P_DELETED;

	shr_header->num_parms -= 1;							/* Decrememt the list.			*/
	while (cur_item < shr_header->num_parms)					/* Now move all the parms up one.	*/
	{
		memcpy((char *)&pr_table[cur_item],(char *)&pr_table[cur_item+1],sizeof(gbl_parm_table));
		cur_item++;
	}
}

/*
	delete_parm	This routine is called to delete PRBs.
			If label is not null then delete the PRB with this label -- if label is blank then delete all PRBs.
			If label is null then delete the first PRB with prname.
			If called from GETPARM then parm_a is not null and is a ptr to the parm_area (SHMH).
*/
long delete_parm(prname,label,parm_a)							/* Delete (or tag) a putparm		*/
char *prname, *label, *parm_a;
{
	SHMH *tmp;
	char *parm_area;
	int found = FALSE;								/* Set found label flag.		*/
	long ret;
	int bylabel, gpfl;

	ret = 0L;									/* Assume SUCCESS.			*/
	gpfl = FALSE;
	if (label)									/* If label specified then clear the	*/
	{										/* one PUTPARM indicated by the label.	*/
		if (memcmp(label,"        ",8) != 0)					/* Is the label blanks?			*/
		{									/* No.					*/
			bylabel = 1;							/* Set so searches for PUTPARM label.	*/
		}
		else									/* Yes, is a CLEAR all.			*/
		{
			delete_all_parms();						/* Call delete all routine.		*/
			cleanup_shrfil();
			return(0L);
		}
	}
	else
	{
		bylabel = 0;								/* Search for prname match.		*/
		if (parm_a)								/* If parm_a specified then called from */
		{									/* GETPARM, delete one PUTPARM indicated*/
			gpfl = TRUE;							/* by the prname.			*/
		}
	}

	found = FALSE;									/* Assume not found.			*/
	if (gpfl) parm_area = parm_a;							/* parm_area passed in from GETPARM.	*/
	else if (bylabel) parm_area = get_prb_area(NULL,label,1);			/* else get ptr to shared memory area.	*/
	else		  parm_area = get_prb_area(prname,NULL,1);	

	if (parm_area)									/* did we find match?			*/
	{
		tmp = (SHMH *)parm_area;
		if (bylabel && (strncmp(tmp->label,label,8) == 0))			/* If match PUTPARM label.		*/
		{
			found = TRUE;
			prname = (char *)calloc(8,sizeof(char));			/* Get memory for prname.		*/
			memcpy(prname,tmp->prname,8);					/* Copy prname into local var.		*/
		}
		else if (strncmp(tmp->prname,prname,8) == 0) found = TRUE;		/* If match prname.			*/

		if (found)								/* Found it - try to delete it.		*/
		{
			if (tmp->status == P_DELETED)					/* Is it already marked for deletion?	*/
			{
				if (bylabel && (tmp->label == label)) ret = 16L;	/* label already deleted.		*/
				else if (tmp->prname == prname) ret = 16L;		/* PUTPARM already deleted.		*/
			}
			else if (tmp->usage_cnt != 0)					/* Not infinite usage.			*/
			{
				tmp->usage_cnt--;					/* Decrement the usage count		*/
				if (tmp->usage_cnt == 0)				/* Is it zero now?			*/
				{
					if (gpfl && tmp->label[0] && strncmp(tmp->label,"        ",8) != 0)
					{
						tmp->status = P_KEEP;			/* If gpfl and labeled then KEEP it.	*/
					}
					else	
					{
						tmp->status = P_DELETED;		/* DELETE it.				*/
						wax_table_entry(parm_area);		/* Adjust global memory.		*/
					}
				}
			}
			else if (bylabel && tmp->usage_cnt == 0)			/* If clear LABEL and count=0		*/
			{
				tmp->status = P_DELETED;				/* Mark as deleted.			*/
				wax_table_entry(parm_area);				/* Adjust global memory.		*/
			}
			det_sh_seg();							/* Detach from global memory.		*/
		}
	}
	if (!found && bylabel) ret = 16L;						/* If specified label and not found.	*/
	if (bylabel && found) free(prname);						/* free up memory used.			*/
	cleanup_shrfil();
	return(ret);
}

det_sh_seg()										/* Detatch from the shared memory area. */
{
	free_gblptr();
#ifdef MSDOS
	write_parms_to_file();								/* Save all parms to MSDOS file.	*/
#endif
}

/*
	get_prb_area		Search the pr_table for a PRB by either prname or label.
*/
char *get_prb_area(m_prname, m_label, keep_ok)
char	*m_prname;									/* The prname to match (or NULL)	*/
char	*m_label;									/* The label to match (or NULL)		*/
int	keep_ok;
{
	int	status,cur_item;
	char	*dat_ptr;
	SHMH	*ptr_shmh;
	int	bylabel;

	if (!shrfil_exists()) return(NULL);						/* If shrfil doesn't exist		*/

	if (status = map_shr_ptrs()) return(NULL);					/* Unsuccessful map to shared mem area. */

	if (shr_header->num_parms == 0) return(NULL);					/* No parms.				*/

	if (m_label && memcmp(m_label,"        ",8) != 0)				/* If label and not blank		*/
	{
		bylabel = 1;
	}
	else if (m_prname && memcmp(m_prname,"        ",8) != 0)			/* else match the prname.		*/
	{
		bylabel = 0;
	}
	else
	{
		return(NULL);
	}

	for (cur_item=0; cur_item < shr_header->num_parms; cur_item++)			/* Loop for each PUTPARM and test prname.*/
	{
		if ( ( bylabel && memcmp(pr_table[cur_item].label,m_label,8)==0) ||
		     (!bylabel && memcmp(pr_table[cur_item].prname,m_prname,8)==0)  )	/* found a match			*/
		{
			ptr_shmh = (SHMH *)(pr_data + pr_table[cur_item].proff);	/* Get a pointer to the data.		*/
			if (keep_ok || ptr_shmh->status != P_KEEP)			/* Is it valid to return		*/
			{
				break;
			}
		}
	}

	if (cur_item == shr_header->num_parms)
	{
		return(NULL);								/* Was not found.			*/
	}

	dat_ptr = pr_data + pr_table[cur_item].proff;					/* Get a pointer to the data.		*/

	return(dat_ptr);								/* Return the pointer.			*/
}


static int delete_all_parms()								/* Delete (or tag) ALL putparms.	*/
{
	int status;

	if (!shrfil_exists()) return(0);						/* No share file			*/

	if (status = map_shr_ptrs()) return(0);						/* Unsuccessful map to shared mem area. */

	shr_header->num_parms = 0;
	shr_header->cur_prb_id = 1;							/* Starting prb_id			*/
	shr_header->unused_off = 0;							/* Point to start of data area		*/
	memset(pr_data,'\0',end_data-(char *)pr_data);

#ifdef unix
	delete_shrfil();
#endif
	return(0);									/* Return SUCCESS.			*/
}

/********************* Begin UNIX specific shared memory routines ***************************************************************/
#ifdef unix

int shmid;
char shr_file[64];

static delete_shrfil()
{
	int	status;

	if (!shrfil_exists()) return;							/* Doesn't exist			*/

	if (shmid)
	{
		shmctl(shmid,IPC_RMID,0);						/* Remove the shared memory		*/
		shmid = 0;
	}

	status = unlink(shr_file);							/* Delete the key file.			*/
	if (status)
	{
		werrlog(ERRORCODE(16),shr_file,errno,0,0,0,0,0,0);			/* Error when deleteing temp file.	*/
		wexit(ERRORCODE(16));
	}
}


static int shrfil_exists()								/* Test if shrfil exists		*/
{
	if (shrchan) return(TRUE);							/* Already open. 			*/

	ctlfile(shr_file);								/* Get shared memory file name.		*/
	if ( access(shr_file,0) == 0 )	return(TRUE);					/* File exist.				*/

	return(FALSE);									/* Doesn't exist			*/
}

static int open_shrfil()								/* Open the temp file, if needed.	*/
{

	key_t shmkey, wftok();
	char *shmaddr;
	char temp[10], temp2[10], *p;
	werrlog(ERRORCODE(1),"open_shrfil",0,0,0,0,0,0,0);

	found_shr_mem = TRUE;								/* Assume SUCCESS.			*/
	if (shrchan) return(0);								/* Already open.			*/

	if (access(WISP_PRB_DIR,0))
	{
		if (mkdir(WISP_PRB_DIR,0777))
		{
			werrlog(ERRORCODE(28),WISP_PRB_DIR,errno,0,0,0,0,0,0);		/* Can't create temp file.		*/
			wexit(ERRORCODE(28));
		}
		if (chmod(WISP_PRB_DIR,0777))
		{
			werrlog(ERRORCODE(30),WISP_PRB_DIR,errno,0,0,0,0,0,0,0);	/* Can't change protection.		*/
			wexit(ERRORCODE(30));
		}
	}

	ctlfile(shr_file);								/* Get shared memory file name.		*/

	if ( access(shr_file,0) != 0 )							/* If file doesn't exist then create.	*/
	{
		if ((shrtmpfile = fopen(shr_file,"w")) == NULL)				/* The creation of a file is only used	*/
		{									/* to get a unique inode number to use	*/
			werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);		/* as an address into the unix shared	*/
			wexit(ERRORCODE(32));						/* memory area.				*/
		}
		fprintf(shrtmpfile,"\n");
		fclose(shrtmpfile);							/* Need to keep around so doesn't use	*/
											/* same inode.				*/
	}

	shmkey = wftok(shr_file);							/* Generate unique number.		*/

	if (shmkey == -1)
	{
		printf("errno %d/shr_file=%s\n\015",errno,shr_file);			/* should *Never* happen		*/
	}
	shmid  = shmget(shmkey,max_pages*PAGE_SIZE,IPC_CREAT|IPC_EXCL|0666);		/* Get the id associated with key	*/
	if (shmid == -1)								/* and allocate memory.			*/
	{
		if (errno == EEXIST)							/* If file already exists.		*/
		{
			new_file = 0;							/* Flag it as an old file.		*/
			shmid  = shmget(shmkey,0,0);					/* Get the id associated with key.	*/
			if (shmid == -1)
			{
				werrlog(ERRORCODE(34),shmkey,errno,0,0,0,0,0,0);	/* Error getting id.			*/
				wexit(ERRORCODE(34));
			}
		}
		else
		{
			werrlog(ERRORCODE(34),shmkey,errno,0,0,0,0,0,0);		/* Unhandled error from shmget.		*/
			wexit(ERRORCODE(34));
		}
	}
	else new_file = 1;								/* Flag it as a new file.		*/

	shmaddr = shmat(shmid,(char *)0,0);						/* Get the address of requested id.	*/
	if ( (int)shmaddr != -1 )
	{
		gbl_ptr = shmaddr;
		shrchan = 1;								/* Was shmid but not used for UNIX.	*/
		return(0);
	}
	else
	{
		werrlog(ERRORCODE(36),shmid,errno,0,0,0,0,0,0);				/* Some error getting address.		*/
		return(ERRORCODE(36));
	}
}

key_t wftok(file)									/* WISP ftok				*/
char *file;
{
	key_t myftok(), ftok();
#ifdef _AIX
	return( myftok(file,0xd5) );
#else
	return( ftok(file,0xd5) );
#endif
}
static key_t myftok(file,x)								/* For IBM - generate unique numbers.	*/
char *file;										/* Replaces FTOK (system one doesn't	*/
char x;											/*  work properly for IBM.)		*/
{
	struct stat buf;
	int	rc;

	rc = stat(file,&buf);
	if (rc == -1)
	{
		return( (key_t) -1 );
	}
	if (buf.st_ino&0xff000000) printf("warning %08x\n",buf.st_ino);
	return (key_t)(((key_t)x<<24)|(buf.st_ino&0x00ffffff));
}

static ctlfile(dest)									/* Generate a temp file name for the	*/
char *dest;										/* master control file.			*/
{
	sprintf(dest,"%s/w2$sh%04X",WISP_PRB_DIR,PGRPID);				/* Construct filepath to use.		*/
}

#endif	/* #ifdef unix	*/								/* End of UNIX specific memory code.	*/

/********************* Begin MSDOS specific shared memory routines **************************************************************/
#ifdef MSDOS

static char shr_file[64];

static int shrfil_exists()								/* Test if shrfil exists		*/
{
	if (shrchan) return(TRUE);							/* Already open. 			*/

	ctlfile(shr_file);								/* Get shared memory file name.		*/
	if ( access(shr_file,0) == 0 )	return(TRUE);					/* File exist.				*/

	return(FALSE);									/* Doesn't exist			*/
}

static int open_shrfil()								/* Open the temp file, if needed.	*/
{
	werrlog(ERRORCODE(1),"open_shrfil",0,0,0,0,0,0,0);

	found_shr_mem = TRUE;								/* Assume SUCCESS.			*/
	if (shrchan) return(0);								/* Already open.			*/

	if (access(WISP_PRB_DIR,0))
	{
		if (mkdir(WISP_PRB_DIR))
		{
			werrlog(ERRORCODE(28),WISP_PRB_DIR,errno,0,0,0,0,0,0,0);	/* Can't create temp file.		*/
			wexit(ERRORCODE(28));
		}
#ifdef	MSDOS_PROT	/* if need to change protection */
		if (chmod(WISP_PRB_DIR,0777))
		{
			werrlog(ERRORCODE(30),WISP_PRB_DIR,errno,0,0,0,0,0,0,0);	/* Can't change protection.		*/
			wexit(ERRORCODE(30));
		}
#endif
	}

	ctlfile(shr_file);								/* Get shared memory file name.		*/

	gbl_ptr = malloc( max_pages * PAGE_SIZE );
	shrchan = 1;

	if ( access(shr_file,0) != 0 )							/* If file doesn't exist then create.	*/
	{
		if ((shrtmpfile = fopen(shr_file,"wb")) == NULL)
		{
			free( gbl_ptr );
			werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);
			wexit(ERRORCODE(32));
		}
		fclose(shrtmpfile);
		unlink(shr_file);
		new_file = 1;
	}
	else
	{
		if ((shrtmpfile = fopen(shr_file,"rb")) == NULL)
		{									/* to get a unique inode number to use	*/
			free( gbl_ptr );
			werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);
			wexit(ERRORCODE(32));
		}
		fread( gbl_ptr, PAGE_SIZE, max_pages, shrtmpfile );
		new_file = 0;
	}
	return(0);
}

static ctlfile(dest)									/* Generate a temp file name for the	*/
char *dest;										/* master control file.			*/
{
	char *wanguid3();

	sprintf( dest, "%s\\W3SM-%.3s.gbl", WISP_PRB_DIR, wanguid3() );			/* Construct MSDOS filepath to use.	*/
}

write_parms_to_file()									/* Save all parms to MSDOS file.	*/
{
	if ((shrtmpfile = fopen(shr_file,"wb")) == NULL)
	{
		free( gbl_ptr );
		werrlog(ERRORCODE(32),shr_file,errno,0,0,0,0,0,0);
		wexit(ERRORCODE(32));
	}
	fwrite( gbl_ptr, PAGE_SIZE, max_pages, shrtmpfile );
	fclose(shrtmpfile);
}

#endif	/* #ifdef MSDOS */								/* End of MSDOS specific memory code.	*/

/********************* Begin VAX specific shared memory routines ****************************************************************/
#ifdef VMS

static int shrfil_exists()								/* Test if shrfil exists		*/
{
	return(TRUE);	/* assume that the share file does exist (open_shrfil will create it anyway) */
}

static int open_shrfil()								/* Open the section file, if needed.	*/
{
											/* This routine will create the name,	*/
	int status, return_stat;							/* and open the file if the create_gbl	*/
	unsigned short retlen;								/* is set to TRUE.			*/

	werrlog(ERRORCODE(1),"open_shrfil",0,0,0,0,0,0,0);

	found_shr_mem = TRUE;								/* Assume SUCCESS.			*/
	if (shrchan) return(0);								/* Already open.			*/

	mybuf.buflen = 4;								/* Now get the process ID of the master */
	mybuf.item_code = JPI$_MASTER_PID;						/* or parent process in the process tree*/
	mybuf.retlen = &retlen;
	mybuf.bufptr = (char *) &master_pid;
	mybuf.endbuf = 0;

	status = sys$getjpi((long) 0,(long) 0,(long) 0, &mybuf,(long) 0,(long) 0,(long) 0);	/* Get the ID.			*/

	if (status != SS$_NORMAL && status != RMS$_NORMAL)
	{
		werrlog(ERRORCODE(38),status,0,0,0,0,0,0,0);
		wexit(ERRORCODE(38));							/* Some error.				*/
	}
	sprintf(secname,"W2$SH%08x",master_pid);					/* Create section name.			*/
	gsdnam.dsc$w_length = strlen(secname);						/* Set the gsdnam descriptor.		*/

	sprintf(filename,"SYS$LOGIN:%s.GBL",secname);					/* Section files go into SYS$LOGIN, and */
											/* Are named W2$SHPPPPPPPP.GBL, where*/
											/* PPPPPPPP is the HEX PID of the master*/
	if (!create_gbl)								/* If don't need to create .GBL file,	*/
	{										/* then test if exists first.		*/
		char *context, fnam[128];						/* filename returned by system		*/
$DESCRIPTOR(f_desc, fnam);
$DESCRIPTOR(p_desc, filename);

		context=0;
		status = LIB$FIND_FILE(&p_desc,&f_desc,&context,0,0,0,0);		/* Get system file name.		*/
		if (status != RMS$_NORMAL)
		{
			found_shr_mem = FALSE;						/* Did not find shared memory area.	*/
			return(status);							/* If not found then return status,	*/
		}
	}

	create_gbl = TRUE;								/* else create the .GBL file.		*/

	fab = cc$rms_fab;								/* Intialize the FAB structure.		*/
	fab.fab$l_dna = 0;								/* No default name.			*/
	fab.fab$b_dns = 0;								/* No size either.			*/
	fab.fab$l_fna = filename;							/* Set address of filename string.	*/
	fab.fab$b_fns = strlen(filename);						/* Set the size of the filename string. */
	fab.fab$l_fop = FAB$M_UFO | FAB$M_CIF | FAB$M_CBT;				/* User mode, create, contig best try.	*/
	fab.fab$b_rtv = -1;								/* Map file pointer to mem.		*/
	fab.fab$l_alq = max_pages;							/* Allocate some blocks.		*/
	fab.fab$l_nam = 0;								/* Address of the NAM structure block.	*/
	fab.fab$b_shr = FAB$M_SHRPUT+FAB$M_UPI;						/* Set access.				*/
	status = sys$create(&fab);							/* Attempt to open the file.		*/

	if (status == RMS$_NORMAL || status == RMS$_CREATED)				/* RMS services open was successful.	*/
	{
		if (status == RMS$_CREATED)	new_file = 1;				/* Flag it as a new file.		*/
		else				new_file = 0;
		shrchan = fab.fab$l_stv;						/* Save channel id.			*/
		return(0);
	}
	else
	{
		werrlog(ERRORCODE(40),status,0,0,0,0,0,0,0);				/* Unsuccessful open of shared mem area.*/
		wexit(ERRORCODE(40));
	}
}

#endif	/* #ifdef VMS	*/								/* End of VMS specific memory code.	*/

/********************* END VAX specific shared memory routines ******************************************************************/

#ifdef OLD
/*
	tag_prb		Tag a PRB to be updated later with update_prb().
				0	Tag the next PRB
				-1	Return tagged PRB
				#	PRB id
*/
int tag_prb(id)
int id;
{
#define SAVE_NEXT	1
#define GOT_PRB		2
	static int	tagged=0;
	static int	tagged_shmh;

	if ( id == 0 )								/* Set up to tag next PRB			*/
	{
		tagged = SAVE_NEXT;
		return(0);
	}

	if ( id == -1 )								/* Return tagged PRB (or 0 if none)		*/
	{
		if ( tagged == GOT_PRB ) return(tagged_shmh);
		return(0);
	}

	if ( tagged == SAVE_NEXT )						/* Save this PRB pointer			*/
	{
		tagged_shmh = id;
		tagged = GOT_PRB;
		return(0);
	}
}

/*
	update_prb		Update the tagged PRB with FILE LIB and VOL info.
*/
update_prb(file,lib,vol)
char	file[8], lib[8], vol[6];
{
	SHMH	*shmh_ptr, *new_shmh;
	KEYWSHM *ptr_keywshm;
	int	foundallkeys, change_needed, change_inplace;
	char	buff[80];
	int	id;
	char	*ptr_new, *ptr_old;
	int	mem_needed,i,off;

	if (map_shr_ptrs()) return(0);						/* Map into shared memory			*/

	id = tag_prb(-1);							/* Get the tagged PRB pointer			*/

	if ( id == 0 ) return(0);						/* No PRB tagged then return			*/

	off = off_prbid(id);							/* Get offset into data area			*/
	if (off == -1)
	{
		return(0);
	}

	shmh_ptr = (SHMH *)(pr_data + off);

	if ( shmh_ptr->status == P_DELETED ||					/* If DELETED or not labeled return		*/
	     shmh_ptr->label[0] == '\0' || shmh_ptr->label[0] == ' '	 )
	{
		return(0);
	}

	/*
	**	Do the update
	*/

	foundallkeys  = 1;								/* Assume all keywords exist		*/
	change_needed = 0;								/* Assume no changes needed		*/
	change_inplace = 1;								/* Assume we can change in place	*/

	if (search_parm_area(buff,"FILE    ",NULL,8L,(char *)shmh_ptr,0))
	{
		if ( memcmp(buff,file,8) != 0 ) 
		{
			change_needed = 1;						/* FILE needs updating			*/

			if ( change_inplace )
				change_inplace = update_value(shmh_ptr,"FILE    ",file,8);
		}

		if (search_parm_area(buff,"LIBRARY ",NULL,8L,(char *)shmh_ptr,0))
		{
			if ( memcmp(buff,lib,8) != 0 )
			{
				change_needed = 1;					/* LIBRARY needs updating		*/

				if ( change_inplace )
					change_inplace = update_value(shmh_ptr,"LIBRARY ",lib,8);
			}

			if (search_parm_area(buff,"VOLUME  ",NULL,6L,(char *)shmh_ptr,0))
			{
				if ( memcmp(buff,vol,6) != 0 )
				{
					change_needed = 1;				/* VOLUME needs updating		*/

					if ( change_inplace )
						change_inplace = update_value(shmh_ptr,"VOLUME  ",vol,6);
				}
			}
			else
			{
				foundallkeys = 0;					/* VOLUME not found			*/
			}
		}
		else
		{
			foundallkeys = 0;						/* LIBRARY not found			*/
		}
	}
	else
	{
		foundallkeys = 0;							/* FILE not found			*/
	}

	if (foundallkeys && !change_needed)						/* Nothing to update			*/
	{
		return(0);
	}

	if (foundallkeys && change_inplace)						/* Try to update inplace		*/
	{
		return(0);								/* done					*/
	}

	/*
	**	We have to create a new PRB area and add/replace FILE, LIBRARY and VOLUME keywords
	*/


	mem_needed = shmh_ptr->prb_size;					/* Calc mem needed as old size ...		*/
	mem_needed += (sizeof(KEYWSHM)+8+1)*3 + 8 + 8 + 6;			/* plus FILE LIBRARY VOLUME keywords		*/
	mem_needed = roundup4(mem_needed);

	/*
	**	Get a new PRB data area.
	**	NOTE:	A get_sh_raw() can cause a compress_data() to occur so any pointers into the data area are
	**		made invalid and must be recalculated.
	*/

	new_shmh = (SHMH *)get_sh_raw(mem_needed);
	off = off_prbid(id);							/* Get offset into data area			*/
	if (off == -1)
	{
		werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);				/* PRBs are corrupt				*/
		wexit(ERRORCODE(8));
	}
	shmh_ptr = (SHMH *)(pr_data + off);					/* Recalculate PRB pointer			*/

	memcpy(new_shmh,shmh_ptr,sizeof(SHMH));					/* Copy the PRB header				*/

	ptr_new = (char *)new_shmh + sizeof(SHMH);
	ptr_old = (char *)shmh_ptr + sizeof(SHMH);

										/* Generate FILE LIBRARY and VOLUME keywords	*/
	ptr_new += load_keywshm((struct keyw_st_shmem *)ptr_new,"FILE    ",file,8);
	ptr_new += load_keywshm((struct keyw_st_shmem *)ptr_new,"LIBRARY ",lib,8);
	ptr_new += load_keywshm((struct keyw_st_shmem *)ptr_new,"VOLUME  ",vol,6);

	memcpy(ptr_new,ptr_old,shmh_ptr->prb_size - sizeof(SHMH));		/* copy rest of keywords			*/

	new_shmh->prb_size = mem_needed;					/* update the size				*/
	new_shmh->keyw_cnt += 3;						/* Update the keyword count			*/

	shmh_ptr->prb_id = 0;							/* Unhook the old data area from table		*/
	shmh_ptr->status = P_DELETED;						/* Delete the old data area			*/

	for (i=0; i < shr_header->num_parms; i++)
	{
		if ( id == pr_table[i].prb_id )
		{
			pr_table[i].proff = off_prbid(id);			/* Update table to point to new PRB		*/
			break;
		}
	}

}

#endif /* OLD */

/*
	load_keywshm	Load the keyword=value at the given address
			return the offset to next keyword.
*/
int load_keywshm(keywshm_ptr,key,value,len)
KEYWSHM *keywshm_ptr;
char	key[8];
char	value[];
int	len;
{
	short	tmp;
	int	off;
	char	*data_ptr;

	off = sizeof(KEYWSHM)+8+len+1;						/* setup offset for next			*/
	tmp = off;
	memcpy(&(keywshm_ptr->next_offs),&tmp,sizeof(short));
	tmp = len;
	memcpy(&(keywshm_ptr->value_len),&tmp,sizeof(short));
	data_ptr = (char *)keywshm_ptr + sizeof(KEYWSHM);
	loadpad( data_ptr, key, 8);						/* copy in keyword				*/
	memcpy( data_ptr+8, value, len);					/* and value					*/
	*(data_ptr+8+len) = '\0';						/* place a null at the end			*/
	return(off);
}

#ifdef OLD
static int update_value(shmh_ptr,keyword,value,maxlen)
SHMH	*shmh_ptr;								/* PRB pointer					*/
char	keyword[8];								/* Keyword (padded with spaces)			*/
char	value[];								/* Value (padded with spaces)			*/
int	maxlen;									/* Max length of value.				*/
{
	KEYWSHM *ptr_keywshm;
	short	vlen;
	char	n_value[80], *p;

	ptr_keywshm = find_prb_keyword((char *)shmh_ptr,keyword,0);
	if (!ptr_keywshm) return(0);

	memcpy(n_value,value,maxlen);
	n_value[maxlen] = '\0';
	p = strchr(n_value,' ');							/* Find first occurance of space & null */
	if (p) *p=(char)0;								/* terminate the string.		*/

	memcpy(&vlen,&(ptr_keywshm->value_len),sizeof(short));
	if (vlen < strlen(n_value)) return(0);

	memset((char *)ptr_keywshm + sizeof(KEYWSHM) + 8,' ',vlen);
	memcpy((char *)ptr_keywshm + sizeof(KEYWSHM) + 8,n_value,strlen(n_value));

	return(1);
}
#endif /*  OLD */

/*
	update_prb_list		This routine is given a ptr to a PRB and a list of keyword=values pairs that are to be updated
				in this PRB.  This may require the PRB be expanded by creating a new PRB with the updated values.
*/
update_prb_list(prb_ptr,update_cnt,update_list)
SHMH	**prb_ptr;
int	update_cnt;
prb_keyword_struct update_list[];
{
	int	i,j;
	int	update_inplace;							/* Can we update the PRB in place		*/
	int	mem_needed;							/* How much memory do we need for new PRB	*/
	char	buff[256];
	KEYWSHM	*prb_keystruct;							/* Pointer to keyword struct in PRB		*/
	short	update_id;							/* The ID of this PRB we are updateing		*/
	int	new_kw_cnt;							/* The new keyword count for the new PRB	*/
	SHMH	*new_prb;							/* The ptr to the new updated PRB		*/
	int	off;								/* The offset of the PRB into the data area	*/
	char	*ptr_new;

	if ( !prb_ptr || ! *prb_ptr ) return(0);				/* No PRB ptr					*/

	update_id = (*prb_ptr)->prb_id;						/* Get the ID for this PRB			*/

	/*
	**	See if the PRB can be updated in place.	 This can be done if it contains all the keywords in the update
	**	list and the size of the values will fit in the existing space.
	*/
	update_inplace = 1;							/* Assume we can updated in place.		*/ 

	for(i=0; i<update_cnt && update_inplace; i++)
	{
		if (prb_keystruct = (KEYWSHM *)find_prb_keyword((char *)*prb_ptr,update_list[i].keyword,0))
		{
			if (a_short(&prb_keystruct->value_len) != update_list[i].len) /* If different sizes then update needed	*/
			{
				update_inplace = 0;
			}
			else							/* Just do the update in place			*/
			{
				memcpy((char *)prb_keystruct + sizeof(KEYWSHM) + 8, update_list[i].value, update_list[i].len);
			}
		}
		else
		{
			update_inplace = 0;
		}
	}

	if (update_inplace)							/* Update was done in place then were done.	*/
	{
		return;
	}

	/*
	**	We are going to create a new PRB that can hold the old PRB plus updates.
	**
	**	Compute the size of the new PRB.
	*/

	mem_needed = sizeof(SHMH);
	new_kw_cnt = 0;
	for(i=1; i<=(*prb_ptr)->keyw_cnt; i++)					/* Loop thru the existing keywords		*/
	{
		int	count_existing;

		count_existing = 1;						/* Assume we will count this keyword		*/
		if (prb_keystruct = (KEYWSHM *)find_prb_keyword((char *)*prb_ptr,NULL,i))	/* get next keyword		*/
		{
			for(j=0; j<update_cnt && count_existing; j++)		/* Loop thru the update list			*/
			{
				if (0==memcmp((char *)prb_keystruct + sizeof(KEYWSHM),update_list[j].keyword,8))
				{
					count_existing = 0;			/* If keyword is in update list then don't count*/
				}
			}
		}
		else
		{
			count_existing = 0;
		}

		if (count_existing)						/* If this keyword is not in update list	*/
		{
			mem_needed += sizeof(KEYWSHM) + 8 + 1 + a_short(&prb_keystruct->value_len);
			new_kw_cnt++;
		}
	}

	for( i=0; i<update_cnt; i++ )						/* Add up the mem needed for update list	*/
	{
		mem_needed += sizeof(KEYWSHM) + 8 + 1 + update_list[i].len;
		new_kw_cnt++;
	}

	mem_needed = roundup4(mem_needed);					/* Round up the memory needed			*/

	/*
	**	Get a new PRB data area for the updated PRB.
	**
	**	NOTE:	A get_sh_raw() can cause a compress_data() to occur so any pointers into the data area are
	**		made invalid and must be recalculated.
	*/

	new_prb = (SHMH *)get_sh_raw(mem_needed);
	off = off_prbid(update_id);						/* Get offset into data area			*/
	if (off == -1)
	{
		werrlog(ERRORCODE(8),0,0,0,0,0,0,0,0);				/* PRBs are corrupt				*/
		wexit(ERRORCODE(8));
	}
	*prb_ptr = (SHMH *)(pr_data + off);					/* Recalculate PRB pointer			*/

	/*
	**	Load the data into the new PRB area.
	**
	**	Load the PRB header and update the header values.
	*/

	memcpy(new_prb,*prb_ptr,sizeof(SHMH));					/* Copy the PRB header				*/

	new_prb->prb_size = mem_needed;
	new_prb->keyw_cnt = new_kw_cnt;

	/*
	**	Load the keyword=value pairs
	**
	**	First load the update pairs then the old pairs.
	*/

	ptr_new = (char *)new_prb + sizeof(SHMH);				/* ptr to new PRB keywords area			*/

	for( i=0; i<update_cnt; i++ )						/* load all the update keywords			*/
	{
		ptr_new += load_keywshm((KEYWSHM *)ptr_new,update_list[i].keyword,update_list[i].value,update_list[i].len);
		new_kw_cnt--;
	}

	for(i=1; i<=(*prb_ptr)->keyw_cnt; i++)					/* Loop thru the existing keywords		*/
	{
		int	count_existing;

		count_existing = 1;						/* Assume we will count this keyword		*/
		if (prb_keystruct = (KEYWSHM *)find_prb_keyword((char *)*prb_ptr,NULL,i))	/* get next keyword		*/
		{
			for(j=0; j<update_cnt && count_existing; j++)		/* Loop thru the update list			*/
			{
				if (0==memcmp((char *)prb_keystruct + sizeof(KEYWSHM),update_list[j].keyword,8))
				{
					count_existing = 0;			/* If keyword is in update list then don't count*/
				}
			}
		}
		else
		{
			count_existing = 0;
		}

		if (count_existing)						/* If this keyword is not in update list	*/
		{
			prb_keyword_struct	old;

			old.keyword = (char *)prb_keystruct + sizeof(KEYWSHM);
			old.value = old.keyword + 8;
			old.len = a_short(&prb_keystruct->value_len);

			ptr_new += load_keywshm((KEYWSHM *)ptr_new,old.keyword,old.value,old.len);
			new_kw_cnt--;
		}
	}

	/*
	**	This is a safety test to see if the number of keywords written to the new PRB matched the what we calculated.
	**	This value should be zero. Any other value is an error.
	*/
	if (new_kw_cnt)
	{
		werrlog(ERRORCODE(42),(*prb_ptr)->prname,(*prb_ptr)->label,0,0,0,0,0,0);
		new_prb->prb_id = 0;
		new_prb->status = P_DELETED;
		return;
	}

	if (ptr_new > (char *)new_prb + mem_needed)
	{
		werrlog(ERRORCODE(44),(*prb_ptr)->prname,(*prb_ptr)->label,0,0,0,0,0,0);
		new_prb->prb_id = 0;
		new_prb->status = P_DELETED;
		return;
	}

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

	*prb_ptr = new_prb;							/* Update the PRB pointer			*/
	return;
}

/*
	getlinklevel	Return the current linklevel. 
*/
int getlinklevel()
{
	return(1);	/* Dummy value for now */
}

/*
	a_short		Takes a pointer to an unaligned short and returns a short.
*/
short a_short(ptr)
char	*ptr;
{
	short	tmp;

	memcpy(&tmp,ptr,sizeof(short));
	return(tmp);
}
