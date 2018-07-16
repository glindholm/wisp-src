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

#ifndef	FILEPTR
#define	FILEPTR		long
#endif
#define	FPTR_SIZE	sizeof( FILEPTR )

#define	U_Short		unsigned short		/* just unsigned ok too */


/* The following line is used to allow 80286 file to be moved to a 80386 */

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

#ifdef	XENIX_386
#include "p2x386.h"
#endif

typedef	struct _phys_hdr { 
	long		magic;
	short		version;
	FILEPTR		file_size;
	short		blk_mult;		/* # of sectors / block */
	FILEPTR		nxt_blk;		/* -> next unused block */
	FILEPTR		nxt_rec;		/* -> free space in rec blk */
	FILEPTR		first_rec;		/* -> first record */
	FILEPTR		free_rec;		/* -> first deleted record */
	FILEPTR		first_node;		/* -> first node of tree */
	FILEPTR		free_node;		/* -> first deleted node */
	short		num_log_entries;	/* total # of logical entries */
	short		total_keys;		/* # of keys in logicals */
	long		total_records;		/* for all logical files */
	long		del_records;		/* total # deleted records */
	long		num_records[ MAX_LOG_FILES ];
	long		next_uniq;		/* next uniq_id for dups */
	long		intern_version;		/* version # of tree */
	short		free_failures;
	short  		open_cnt;		/* # processes updating */

	/* Statistics only */

	short		height;
	U_Short		nodes_used;
	U_Short		nodes_free;
	long		node_usage;		/* bytes used in nodes */
	long		lost_recs;
} PHYSICAL_HDR;

			/* Logical File Header */

typedef struct _log_hdr {
	char		hdr_type;
	char		log_name[ LOG_NAME_SIZE+1 ];
	short		log_index;
	short		init_key_id;
	U_Short		max_rec_size;
	U_Short		min_rec_size;
	short		open_cnt;
	short		key_size[ MAX_KEYS ];
	Unsigned	key_off[ MAX_KEYS ];
	char		key_dup[ MAX_KEYS ];
	char		num_keys;
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

