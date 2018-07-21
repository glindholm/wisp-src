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

char 	*WL_get_sh_seg(char *prname, char *label, int size, int *id, int *rsize);
int 	WL_finish_sh_seg(int size);
SHMH 	*WL_get_prb_id(int2 id);
int 	WL_cleanup_shrfil(void);
int 	WL_show_parm_area(void);
int 	WL_ishow_parm_area(void);
int 	WL_dump_parm_area(char *filename);
int 	WL_wax_table_entry(SHMH *parm_area);
void	WL_erase_prb_label_level(char *m_label);
int	WL_erase_prb_level(void);
int	WL_erase_prb(SHMH *prb);
int 	WL_delete_prb(SHMH *prb);
int	WL_use_prb(SHMH *prb);
SHMH 	*WL_get_prb_area(char *m_prname, char *m_label, int used_ok);
SHMH 	*WL_get_prb_prname_level(char *m_prname, int wantlevel);
SHMH 	*WL_get_chained_prb(SHMH *start_prb);
int 	WL_load_keywshm(KEYWSHM *keywshm_ptr, FMTLIST *p);
int 	WL_backwards_reference(SHMH **prb_ptr);
int 	WL_update_prb(SHMH **prb_ptr, FMTLIST *fmtlist);
int 	WL_ppunlink(int level);

int2 	WL_a_int2(void *ptr);
int4 	WL_a_int4(void *ptr);

const char *WL_sm_ctlfile(void);


#endif /* SHAREMEM_H */
/*
**	History:
**	$Log: sharemem.h,v $
**	Revision 1.10  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.9  2002/07/09 04:14:03  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.8  2001/10/31 20:26:46  gsl
**	Add WL_sm_ctlfile()
**	
**	Revision 1.7  1996-07-23 14:17:51-04  gsl
**	drcs update
**
**
**
*/
