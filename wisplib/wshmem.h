			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/**
 ** header : wshmem.h
 ** contains: struct definitions for putparm & getparm
 **
 **/

#ifndef wshmem_H
#define wshmem_H

#define SHMH struct shm_header

SHMH
{
	short	prb_id;								/* ID to link with pr_table			*/
	short 	prb_size;							/* The total size of this prb			*/
	short 	usage_cnt;							/* Usage count of PUTPARM.			*/
	short 	keyw_cnt;							/* Number of keywords in PUTPARM.		*/
	short 	status;								/* Status of PUTPARM.				*/
	char 	type;								/* Type of PUTPARM.				*/
	char 	prname[9];							/* Parameter reference name.			*/
	char 	pfkey;								/* PF key indicated.				*/
	char 	label[9];							/* PUTPARM label of PUTPARM.			*/
	char 	reflbl[9];							/* PUTPARM reference label.			*/
	char 	cleanup;							/* Clean up options indicator.			*/
	char 	filler[8];							/* Set aside for future use.			*/
};


/* Used with SHMH->status */
#define P_OK 		0							/* PUTPARM is ok for use in call.		*/
#define P_DELETED	1							/* Marked for deletion, can't use in call.	*/
#define P_USED		2							/* Need to keep around, can't use in call.	*/

#define OK_USED		1
#define OK_ONLY		0

/*
**	The KEYW is used to build fmtlist.
*/
#define KEYW struct keyw_st
KEYW {
	KEYW	*next;								/* Pointer to next node or NULL			*/
	char	*value;								/* Pointer to calloced value data area		*/
	int4	len;								/* The length of value				*/
	char	keyword[8];							/* The KEYWORD (blank padded)			*/
#define SPECIAL_NOT	((char)0)
#define SPECIAL_KEY	((char)1)
	char	special;							/* 0=Normal 1=special				*/
										/* Special==Individual keyword backwards-ref	*/
};

#define KEYWSHM struct keyw_st_shmem
KEYWSHM 
{
	short 	next_offs;							/* Offset of next KEYWSHM from here.		*/
	short 	value_len;							/* Length of value.				*/
	char	special;							/* Special flag					*/
	char 	filler[3];							/* Set aside for future use.			*/
};


/*
**	Global sharemem.c routines
*/
char	*get_sh_seg();		/* Get a shared segment of memory.								*/
int	finish_sh_seg();	/* This is called to update the shr_header counters after the PRB has been loaded.		*/
SHMH	*get_prb_id();		/* This routine returns a pointer to the PRB identified by id or NULL if not found.		*/
int	cleanup_shrfil();	/* Cleanup and close the shared memory file.							*/
int	show_parm_area();	/* Print a report showing the current putparm status.						*/
int	ishow_parm_area();	/* Print a report showing the internal status of putparms					*/
int	dump_parm_area();	/* Copy the contents of the shared memory area to a file.					*/
int	wax_table_entry();	/* This routine removes a PRB entry from the pr_table. (Also ensures status=P_DELETED)		*/
int	erase_prb_level();	/* Erase all PRB's at this link-level (or higher)						*/
int	erase_prb();		/* Erase this PRB										*/
int	delete_prb();		/* Delete this PRB (mark as used if labeled)							*/
int	use_prb();		/* Use one instantance of this PRB (delete if used up)						*/
char 	*get_prb_area();	/* Search the pr_table for a PRB by either prname or label.					*/
int	load_keywshm();		/* Load the keyword=value at the given address							*/
int	backwards_reference();	/* Perform GETPARM time backwards referencing							*/
int	update_prb();		/* Update (merge) a fmtlist into a PRB.								*/
int	ppunlink();		/* To unlink the putparms (delete or mark as used).						*/

#endif


