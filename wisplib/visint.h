/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/* visint - internal structures for Vision v2 */

#define	VISION_INTERNALS
#define	V4

#ifdef ORIGINAL
#include "machine.h"
#endif
#define NEAR

#include "visn2.h"

#ifndef	reg
#define	reg	register
#endif
#ifndef	TRUE
#define	TRUE	1
#define	FALSE	0
#endif
#ifndef	NULLP
#define	NULLP	((char *) NULL)
#endif
#ifndef	CPTR
#define	CPTR	(char *)
#endif
#ifndef	Min
#define	Min(a,b)	( (a) < (b) ? (a) : (b) )
#define	Max(a,b)	( (a) > (b) ? (a) : (b) )
#endif

/*
** 	For SCO Vision 2:
**	- 'Unsigned' is 2 bytes
**	- 'FILEPTR' gets aligned on a 2 byte boundary
*/

#ifndef	FILEPTR
#define	FILEPTR		long
#endif
#define	FPTR_SIZE	sizeof( FILEPTR )

#define	U_Short		unsigned short		/* just unsigned ok too */


/* The following line is used to allow 80286 file to be moved to a 80386 */

#if defined(WIN32) || defined(SCO) || defined(LINUX)	/* On Windows & SCO & LINUX "Unsigned" is 2 bytes */
#define INTEL
#endif

#ifdef	INTEL
#define	Unsigned	unsigned short
#else
#define	Unsigned	unsigned
#endif

#ifndef	SECTOR_SIZE
#define	SECTOR_SIZE	512
#endif

#define	VERSION			2
#define	MAX_LOG_FILES		16
#define	MAX_BLOCK_MULT		2
#define	MAX_COMMENT_SIZE	30
#define	MAX_KEY_SIZE		126		/* increase w/caution! */
#define	MAX_FAILS		30
#define	SEQ_SET_SIZE		10

#define	FILE_LCK_FLAG		10000
#define	STOPPER_KEY		0x7f		/* last key on each level */
#define	COMPRESS_FLAG		0xfe
#define	PHYS_DEF_NAME	"<DEFAULT>"
#define	DELETED_REC		0x7f		/* marks a deleted record */
#define	BINARY_REC		0x7e


			/* Physical File Header */

#ifdef	SCO
/* This is included on SCO 386 machines to make structures align on 2-byte boundaries */
/* NOTE: This doesn't work with GNU compiler so SCO changes were made below */
#include "p2x386.h"	
#endif

/*
 *	All the offset below in the struct comments are incorrect for SCO.
 *	These defines are correct (but unsed).
 */
#define V2_SCO_REC_COUNT_OFF 	40	/* phdr.total_records */
#define V2_SCO_MAX_REC_OFF 	156	/* lhdr.max_rec_size */
#define V2_SCO_MIN_REC_OFF 	158	/* lhdr.min_rec_size */
#define V2_SCO_NUM_KEYS_B	237	/* lhdr.num_keys */

typedef	struct _phys_hdr 
{ 
	long		magic;							/*	  0	4	1-4	*/
	short		version;						/*	  4	2	5-6	*/
#ifndef SCO
	char		filler_6[2];						/*	  6	2	7-8	*/
	FILEPTR		file_size;						/*	  8	4	9-12	*/
#else
	char		x_file_size[4];						/*	  6	4		*/
#endif
	short		blk_mult;		/* # of sectors / block */	/*	 12	2	13-14	*/
#ifndef SCO
	char		filler_14[2];						/*	 14	2	15-16	*/
	FILEPTR		nxt_blk;		/* -> next unused block */	/*	 16	4	17-20	*/
#else
	char		x_nxt_blk[4];		/* -> next unused block */	/*	 12	4		*/
#endif
	FILEPTR		nxt_rec;		/* -> free space in rec blk */	/*	 20	4	21-24	*/
	FILEPTR		first_rec;		/* -> first record */		/*	 24	4	25-28	*/
	FILEPTR		free_rec;		/* -> first deleted record */	/*	 28	4	29-32	*/
	FILEPTR		first_node;		/* -> first node of tree */	/*	 32	4	33-36	*/
	FILEPTR		free_node;		/* -> first deleted node */	/*	 36	4	37-40	*/
	short		num_log_entries;	/* total # of logical entries */ /*	 40	2	41-42	*/
	short		total_keys;		/* # of keys in logicals */	/*	 42	2	43-44	*/
	long		total_records;		/* for all logical files */	/*	 44	4	45-48	*/
	long		del_records;		/* total # deleted records */	/*	 48	4	49-52	*/
	long		num_records[ MAX_LOG_FILES ];				/*	 52	64(4*16)53-116	*/
	long		next_uniq;		/* next uniq_id for dups */	/*	116	4	117-120 */
	long		intern_version;		/* version # of tree */		/*	120	4	121-124 */
	short		free_failures;						/*	124	2	125-126 */
	short  		open_cnt;		/* # processes updating */	/*	126	2	127-128	*/

	/* Statistics only */

	short		height;							/*	128	2	129-130	*/
	U_Short		nodes_used;						/*	130	2	131-132 */
	U_Short		nodes_free;						/*	132	2	133-134 */
#ifndef SCO
	char		filler_134[2]; 						/*	134	2	135-136 */
	long		node_usage;		/* bytes used in nodes */	/*	136	4	137-140	*/
#else
	char		x_node_usage[4];	/* bytes used in nodes */	/*	130	4		*/
#endif
	long		lost_recs;						/*	140	4	141-144 */
} PHYSICAL_HDR;

			/* Logical File Header */

/* For SCO this logical header starts at offset 138 (144 - 6) */
typedef struct _log_hdr {
	char		hdr_type;						/*	144	1	145	*/
	char		log_name[ LOG_NAME_SIZE+1 ];				/*	145	13	146-158	*/
	short		log_index;						/*	158	2	159-160	*/
	short		init_key_id;						/*	160	2	161-162	*/
	U_Short		max_rec_size;						/*	162	2	163-164	*/
	U_Short		min_rec_size;						/*	164	2	165-166	*/
	short		open_cnt;						/*	166	2	167-168	*/
	short		key_size[ MAX_KEYS ];					/*	168	30(2*15)169-198	*/
#ifndef INTEL /*  not SCO or WIN32 */
	char		filler_198[2];						/*U	198	2	199-200 */
#endif
	Unsigned	key_off[ MAX_KEYS ];					/*U	200	60(4*15)201-260	*/
										/*I 	198	30(2*15)199-228 */
	char		key_dup[ MAX_KEYS ];					/*U	260	15	261-275 */
										/*I	228	15	229-243 */
	char		num_keys;						/*U	275	1	276	*/
										/*I	243	1	244	*/
	unsigned	compressed : 1;
	unsigned	var_recsize : 1;
	unsigned	encrypted : 1;
	char		comment[ MAX_COMMENT_SIZE+1 ];
} LOGICAL_HDR;


typedef	struct _link {
	char		type;
	FILEPTR		next_blk;
} LINK;

#ifdef	XENIX_386
#include "p0x386.h"
#endif

#define	EMPTY_TYPE	0
#define	LINK_TYPE	1
#define	USED_TYPE	2
#define	OBSOLETE_TYPE	3
#define	FREE_NODE_TYPE	4


			/* Tree Nodes */

typedef	struct _node {
	short		used;			/* key area in use */
	char		ents[1];		/* entry buffer */
} NODE;

typedef	struct _entry {	
	FILEPTR		leftptr;		/* left sub-tree */
	long		uniq;			/* make dup key unique */
	char		key[ MAX_KEY_SIZE + 1 ];
} ENTRY;

/* NOTE: On the leaf nodes, the left pointer field becomes the record	*/
/* address.  This is indicated by storing the negative value of the	*/
/* record offset.  Negative leftptr's indicate that the node is a leaf.	*/

#define	recaddr		leftptr


			/* Record Layout */


/* Each record is preceeded by a packed structure containing the 	*/
/* following fields.  This structure must be unpacked before it is used.*/

/* Note: uniq_id holds the next record address when the record is part	*/
/* of a chain of deleted records.  */

typedef	struct _rec_hdr {
	short		size;		/* size of record area */
	short		used;		/* amount used by current record */
	long		uniq_id;
	char		rec_id;		/* index # of file or DELETED */
} RECORD_HDR;

#define	next_del	uniq_id


			/* File Handle */


typedef	struct _v_file {
	LOGICAL_HDR	log;
	PHYSICAL_HDR	hdr;
	FILEPTR		rd_next_blk;	/* last block read */
	FILEPTR		seq_set[ SEQ_SET_SIZE ];
	long		cur_uniq;	/* unique value of current record */
	long		nxt_version;	/* internal version # of rd_nxt_blk */
	char		*cur_reckey;	/* key value of current record */
	char		*cur_primekey;	/* same for primary key */
	FILEPTR		cur_addr;	/* cur_primekey's record address */
	int		fileds;
	short		blk_size;
	short		o_mode;		/* open mode */
	short		lock_mode;
	char		curkey;		/* current key of reference */
	char		use_on_next;	/* TRUE if current key = next rec */
	char		hdr_locked;	/* TRUE if hdr current and locked */
	unsigned char	max_locks;
	FILEPTR		locks[1];	/* must be last */
} V_FILE;


			/* Miscellaneous */


typedef	struct _ent_id {
	FILEPTR		block_id;	/* file address of node */
	int		ndx;		/* offset in node's entry buffer */
	int		depth;
	int		prev_ndx;	/* previous node entry (-1 = none) */
} ENTRY_ID;

/* ENTRY fields cannot be directly accessed in a node (except for the 	*/
/* "key" field), because of word addressing problems on some machines.	*/
/* The following macros convert entry indexes into ENTRY pointers and	*/
/* also unload various ENTRY fields into a local variable.		*/
/* For byte addressed machines, we can have the compiler do some of 	*/
/* the work for us.  For other machines, we must compute the offsets	*/
/* manaully.  */

#ifdef	BYTE_ADDRESSING

#define	ENT(nod,ndx)		((ENTRY *) &((nod).ents[ndx]))
#define	ENTKEY(nod,ndx)		( ENT(nod,ndx)->key )
#define	ENTSIZE(nod,ndx)	( entry_overhead + ENT(nod,ndx)->key[0] )
#define	GETUNIQ(nod,ndx,u)	movel( CPTR &u, CPTR &ENT(nod,ndx)->uniq, \
						sizeof( long ) )
#define	GETLEFT(nod,ndx,l)	movel( CPTR &l, CPTR &ENT(nod,ndx)->leftptr,\
						FPTR_SIZE )

#else

#define	ENTKEY(nod,ndx)		( &(nod).ents[(ndx) + v_key_off] )
#define	ENTSIZE(nod,ndx)	( entry_overhead + \
				  (nod).ents[(ndx) + v_key_off] )
#define	GETUNIQ(nod,ndx,u)	movel( &u, &(nod).ents[(ndx) + v_uniq_off], \
						sizeof( long ) )
#define	GETLEFT(nod,ndx,l)	movel( &l, &(nod).ents[ndx], FPTR_SIZE )

#endif

#define	GETADDR			GETLEFT


typedef	unsigned char		Uchar;

extern	char		NEAR key_found[];
extern	NODE		NEAR * NEAR node;
extern	char		NEAR v_key_match;
extern	int		NEAR entry_overhead;
extern	int		NEAR v_uniq_off, NEAR v_key_off;
extern	int		v_errno, v_supl_err;


#define	FND_FULL_MATCH	5	/* match key and uniq id */
#define	FND_KEY_MATCH	4	/* next key matches key but not uniq id */
#define	FND_MATCH_NEXT	3	/* no match, but next key found */
#define	FND_NO_MATCH	2	/* no match or greater key */
#define	FND_EMPTY	1	/* tree is empty */
#define	FND_ERROR	0


#define ret_err(code)	{ v_errno = code; return 0; }
#define	ret_brk(code)	{ v_errno = V_BROKEN; v_supl_err = code; return 0; }
#define	clr_ret		{ unlck_hdr(); return 0; }
#define	clr_err(code)	{ unlck_hdr(); v_errno = code; return 0; }

/* */

/*
**	History:
**	$Log: visint.h,v $
**	Revision 1.12  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.11  2003/01/24 18:28:49  gsl
**	fix for LINUX and VISION2
**	
**	Revision 1.10  2002/01/09 22:12:58  gsl
**	Update for SCO - THe Vision2 structs were all messed up.
**	
**	Revision 1.9  2001-11-13 10:50:01-05  gsl
**	Define INTEL for WIN32 so type "Unsigned" is 2 bytes instead of 4.
**	Document the position and offests of all the VISION2 header fields.
**
**	Revision 1.8  1996-08-19 18:33:04-04  gsl
**	drcs update
**
**
**
*/
