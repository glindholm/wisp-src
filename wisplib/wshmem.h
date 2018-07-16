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
#define P_KEEP		2							/* Need to keep around, can't use in call.	*/

#define KEYW struct keyw_st

KEYW {
	KEYW *next;										/* ignored here			*/
	char *keyword;
	char *value;
	short len;
};

#define KEYWSHM struct keyw_st_shmem

KEYWSHM 
{
	short 	next_offs;							/* Offset of next KEYWSHM from here.		*/
	short 	value_len;							/* Length of value.				*/
	char 	filler[4];							/* Set aside for future use.			*/
};

struct prb_keyword_struct
{
	char	*keyword;
	int	len;
	char	*value;
};

typedef struct prb_keyword_struct prb_keyword_struct;

