/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		sharemem.h
**
**	Purpose:	Header for sharemem.c
**			contains struct definitions for putparm & getparm
**
**	History:
**	09/02/94	Updated by GSL
**
*/

#ifndef SHAREMEM_H
#define SHAREMEM_H

#define SHMH struct shm_header

SHMH
{
	int2	prb_id;								/* ID to link with pr_table			*/
	int2 	prb_size;							/* The total size of this prb			*/
	int2 	usage_cnt;							/* Usage count of PUTPARM.			*/
	int2 	keyw_cnt;							/* Number of keywords in PUTPARM.		*/
	int2 	status;								/* Status of PUTPARM.				*/
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
**	The FMTLIST is used to build fmtlist.
*/
#define FMTLIST struct fmtlist_struct
FMTLIST {
	FMTLIST	*next;								/* Pointer to next node or NULL			*/
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
	int2 	next_offs;							/* Offset of next KEYWSHM from here.		*/
	int2 	value_len;							/* Length of value.				*/
	char	special;							/* Special flag					*/
	char 	filler[3];							/* Set aside for future use.			*/
};


/*
**	Global sharemem.c routines
*/

extern char 	*get_sh_seg(char *prname, char *label, int size, int *id, int *rsize);
extern int 	finish_sh_seg(int size);
extern SHMH 	*get_prb_id(int2 id);
extern int 	cleanup_shrfil(void);
extern int 	show_parm_area(void);
extern int 	ishow_parm_area(void);
extern int 	dump_parm_area(char *filename);
extern int 	wax_table_entry(SHMH *parm_area);
void erase_prb_label_level(char *m_label);
extern int	erase_prb_level(void);
extern int	erase_prb(SHMH *prb);
extern int 	delete_prb(SHMH *prb);
extern int	use_prb(SHMH *prb);
extern SHMH 	*get_prb_area(char *m_prname, char *m_label, int used_ok);
extern SHMH 	*get_prb_prname_level(char *m_prname, int wantlevel);
extern SHMH 	*get_chained_prb(SHMH *start_prb);
extern int 	load_keywshm(KEYWSHM *keywshm_ptr, FMTLIST *p);
extern int 	backwards_reference(SHMH **prb_ptr);
extern int 	update_prb(SHMH **prb_ptr, FMTLIST *fmtlist);
extern int 	ppunlink(int level);

extern int2 	a_int2(void *ptr);
extern int4 	a_int4(void *ptr);

const char 	*wisp_sm_ctlfile(void);


#endif /* SHAREMEM_H */
/*
**	History:
**	$Log: sharemem.h,v $
**	Revision 1.8  2001-10-31 15:26:46-05  gsl
**	Add wisp_sm_ctlfile()
**
**	Revision 1.7  1996-07-23 14:17:51-04  gsl
**	drcs update
**
**
**
*/
