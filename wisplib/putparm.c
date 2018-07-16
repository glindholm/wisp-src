			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/* 
	PUTPARM.C 	This routine emmulates the WANG VSSUB  PUTPARM.
*/

#ifdef unix 
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif

#ifdef MSDOS
#include <stdlib.h>
#include <malloc.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <varargs.h>									/* This routine uses variable args.	*/

#include "movebin.h"
#include "werrlog.h"
#include "wshmem.h"
#include "wdefines.h"
#include "wglobals.h"

				/* Function prototypes required by MSDOS:	*/
static long write_shrinfo();								/* Do the write to the shared memory.	*/
static long retrieve_info();								/* Test if the PUTPARM label exists	*/
static long process_reflbl();								/* Set up needed keywords and values.	*/

static KEYW *head, *p, *p2;
static char *dest;
static SHMH *pputparm;
static KEYWSHM *keywshm;
static char *data;
static int offs;

static char *scratch;
static char *nameptr;
static char *valptr;
static long *lenptr;
static long len;
static int  kw_cnt_sav;
static char pp_label[9], pp_reflbl[9];
static char *recvr;									/* Variables for 'R' function.		*/
static long *rlptr, rcvr_len, tot_len;
static char *pfkey_rcvr, assign_pfk, cleanup_opt;
#define MAXKW	50									/* Variables for 'M' function.		*/
static char *m_kwdata[MAXKW];

PUTPARM(va_alist)
va_dcl
{
#define		ROUTINE		48000

	va_list the_args;								/* A pointer to traverse the stack.	*/
	int	arg_count;								/* Called with this many arguments.	*/
	long	usage_count, keyword_count;
	long	*long_item;
	long	swap_me, *return_code;
	char	*putparm_type, *prname, *pplabel_arg, *byte_pointer;			/* Pointers to args off the stack.	*/
	int	mem_needed, j, rc_fl;
	long	ret;
	struct	keyw_st *ks;
	long	process_ref;

	werrlog( ERRORCODE(1),0,0,0,0,0,0,0,0 );

	return_code = (long *)NULL;							/* initialize ret code pointer		*/
	rc_fl = FALSE;									/* Init flag, if have return code addrs.*/
	strcpy(pp_label, "        ");							/* Init label.				*/
	strcpy(pp_reflbl,"        ");							/* Init label.				*/

	va_start(the_args);    		 						/* Init. pointer to top of stack.	*/
	arg_count = va_count(the_args);							/* How many args are there ?	      	*/
                                                                                                                                  
	va_start(the_args);								/* Init. pointer to top of stack.	*/
                                                                                                                                  
	putparm_type = va_arg(the_args, char*);						/* What type of putparm are we doin' ?	*/
	arg_count -= 1;									/* One less argument.			*/

	switch (*putparm_type)
	{
		case 'D':
		case 'E':
		case 'C':
		case 'R':
		case 'M':
			break;
		default:
		{
			werrlog(ERRORCODE(18),*putparm_type,0,0,0,0,0,0,0);		/* Function not implemented.		*/
			return(0);
		}
	}										/* Test for USAGE COUNT or PRNAME.	*/
											/* If C,R,M test for PUTPARM label.	*/
	long_item     = va_arg(the_args, long*);					/* Address of the usage count, maybe.	*/
	arg_count -= 1;									/* One less argument.			*/
	byte_pointer = (char *) long_item;

	usage_count = 1;								/* Default to this value.		*/
	if ((*putparm_type == 'D' || *putparm_type == 'E') &&
	    longargtest(byte_pointer, 2) 			)			/* Did they specify the usage count ?	*/
	{
		GETBIN(&swap_me,long_item,sizeof(long));
		wswap(&swap_me);							/* Swap the byte order.			*/
		usage_count = swap_me;				   			/* Set local variable.			*/

		prname = va_arg(the_args, char*);					/* Address of the prname.		*/
		arg_count -= 1;								/* One less argument.			*/
	}
	else
	{                                                               	        /* Nope.  First byte is not a null.	*/
		prname = (char *) long_item;						/* This argument is the prname.		*/
		pplabel_arg = prname;							/* pplabel for "R", "M", and "C"	*/
	}

	if (*putparm_type == 'C')							/* Delete and set return code.		*/
	{										/* PUTPARM label is in prname for 'C'.	*/
		if ( arg_count == 1 )
		{
			long_item = va_arg(the_args,long*);				/* Get the PUTPARM return code address.	*/
			return_code = long_item;					/* Save the address of the return code.	*/
			ret = (long)delete_parm(NULL,pplabel_arg,NULL);			/* delete the area.			*/
		}
		else
		{
			return_code = long_item;
			ret = (long)delete_parm(NULL,"        ",NULL);			/* Delete all				*/
		}
		wswap(&ret);
		PUTBIN(return_code,&ret,sizeof(long));
		return(0);								/* Done with function 'C'.		*/
	}

	ret = 0;									/* Continue processing.			*/

	long_item = va_arg(the_args, long*);						/* Addr. of the no. of keywords, maybe.	*/
	arg_count -= 1;									/* One less argument.			*/
	GETBIN(&swap_me,long_item,sizeof(long));					/* Get a local copy to be swapped.	*/
	wswap(&swap_me);								/* Swap the byte order.			*/
	if (test_next_arg(the_args,arg_count))						/* Are there any more arguments ?	*/
	{										/* Yes, so test which function.		*/
		if (*putparm_type == 'R')						/* Is 'R' so is the receiver and no	*/
		{									/*  keywords to be processed,		*/
			recvr  = (char *)long_item;					/* Set local copy of this value.	*/
			kw_cnt_sav = keyword_count = 0;
			long_item = va_arg(the_args, long*);				/* Addr. of the receiver length.	*/
			rlptr = long_item;						/* Save the pointer.			*/
			GETBIN(&rcvr_len,long_item,sizeof(long));
			wswap(&rcvr_len);						/* Swap the byte order.			*/
			long_item = va_arg(the_args, long*);				/* Addr. of the total length, maybe.	*/
			arg_count -= 1;							/* One less argument.			*/
			if (test_next_arg(the_args,arg_count))				/* Are there any more arguments ?	*/
			{								/* Yes, so test which function.		*/
				GETBIN(&tot_len,long_item,sizeof(long));		/* Set local copy of this value.	*/
				wswap(&tot_len);					/* Swap the byte order.			*/
			}
			else								/* Nope.  This is the return code.	*/
			{
				return_code = long_item;				/* Save the address of the return code.	*/
				rc_fl = TRUE;
			}
		} 									/* else is the keyword count.		*/
		else kw_cnt_sav = keyword_count = swap_me;				/* Set local copy of this value.	*/
	}
	else
	{										/* Nope.  This is the return code.	*/
		return_code = long_item;						/* Save the address of the return code.	*/
		rc_fl = TRUE;
		kw_cnt_sav = keyword_count = 0;						/* No keywords to be processed.		*/
	}
                        
	j = 0;										/* Set for index into 'M' func array.	*/
	for (head=p=NULL,dest=NULL, mem_needed=sizeof(SHMH); keyword_count; --keyword_count)
	{
		if (!head) 
		{
			p = head = (KEYW *)calloc(1,sizeof(KEYW));
		}
		else
		{
			p->next = (KEYW *)calloc(1,sizeof(KEYW));
			p = p->next;
		}
		nameptr	= va_arg(the_args,char*);
		valptr	= va_arg(the_args,char*);
		if (*putparm_type == 'M') m_kwdata[j++] = valptr;			/* Save ptr to keyword data receiver.	*/
		lenptr	= va_arg(the_args,long*);
		arg_count -= 3;
		GETBIN(&len,lenptr,sizeof(long));
		wswap(&len);
		p->keyword = (char *)calloc(8,sizeof(char));				/* grab storage */
		memcpy(p->keyword,nameptr,8);						/* copy the keyword in (incl. spaces) */
		p->value = (char *)calloc((int)len+1,(int)sizeof(char));		/* grab space */
		if (*putparm_type != 'M') memcpy(p->value,valptr,(int)len);		/* copy len bytes */
		p->len = len;								/* set len */
		mem_needed += sizeof(KEYWSHM) + 8 + len + 1;				/* need enough bytes for shm struct */
	}										/* plus 8 for keyword plus len of value */
	assign_pfk = '@';
	pfkey_rcvr = &assign_pfk;
	if (!rc_fl)									/* If more parameters.			*/
	{
		scratch = va_arg(the_args, char*);					/* Get the address of PFkey, maybe.	*/
		--arg_count;
		if (test_next_arg(the_args,arg_count))					/* Are there any more arguments ?	*/
		{									/* Yes, so set pfkey address.		*/
			pfkey_rcvr = scratch;
			assign_pfk = *scratch;
		}
		else									/* No, so set return code address.	*/
		{
			return_code = (long *)scratch;					/* Save the address of the return code.	*/
			rc_fl = TRUE;
		}
	}

	if ((*putparm_type == 'D' || *putparm_type == 'E') && !rc_fl)			/* Continue processing 'D' and 'E'.	*/
	{
		scratch = va_arg(the_args, char*);					/* Get the address of the label, maybe.	*/
		--arg_count;
		if (test_next_arg(the_args,arg_count))					/* Are there any more arguments ?	*/
		{									/* Yes, so set label address.		*/
			memcpy(pp_label,scratch,8);
			pp_label[8] = '\0';						/* Null terminate the string.		*/
		}
		else									/* No, so set return code address.	*/
		{
			return_code = (long *)scratch;					/* Save address of the return code.	*/
			rc_fl = TRUE;
		}
		process_ref = FALSE;
		if (!rc_fl)								/* Get address of reference label, maybe.*/
		{
			scratch = va_arg(the_args, char*);
			--arg_count;
			if (test_next_arg(the_args,arg_count))				/* Are there any more arguments ?	*/
			{								/* Yes, so set reference label address.	*/
				memcpy(pp_reflbl,scratch,8); 				/* Set up kws & vals from reference.	*/
				pp_reflbl[8] = '\0';					/* null terminate the string.		*/
				if (strcmp(pp_reflbl,"        "))			/* If a reference label has been	*/
				{							/* specified then set flag so will try	*/
					process_ref = TRUE; 				/*  to access it.			*/
				}
			}
			else								/* No, so set return code address.	*/
			{
				return_code = (long *)scratch;				/* Save address of the return code.	*/
				rc_fl = TRUE;
			}
		}
	}
	cleanup_opt = ' ';								/* Set the default cleanup options.	*/
	if (!rc_fl)									/* Get address of cleanup opts, maybe.	*/
	{
		scratch = va_arg(the_args, char*);
		--arg_count;
		if (test_next_arg(the_args,arg_count))					/* Are there any more arguments ?	*/
		{									/* Yes, so set clean up options address.*/
			cleanup_opt = *scratch;
		}
		else									/* No, so set return code address.	*/
		{
			return_code = (long *)scratch;					/* Save address of the return code.	*/
			rc_fl = TRUE;
		}
	}
	
	if (!rc_fl) return_code = va_arg(the_args, long*);				/* Last arg is the return code receiver.*/

	if (process_ref) ret = process_reflbl(prname,pp_reflbl,&mem_needed);

	if (*putparm_type == 'R' || *putparm_type == 'M')				/* Retreive info for 'R' and 'M'.	*/
	{
		ret = retrieve_info(*putparm_type,pplabel_arg);				/* Retrieve info. from PUTPARM requested.*/
	}
	else 										/* Only ENTER and DISPLAY generate a 	*/
	{										/* new PUTPARM.				*/
		ret = write_shrinfo(prname,mem_needed,putparm_type,usage_count);
	}
 	det_sh_seg();
	wswap(&ret);
	PUTBIN(return_code,&ret,sizeof(long));						/* Set the return code for PUTPARM.	*/
}                                                                                                                                 

static int test_next_arg(args,cnt)							/* Return TRUE if there is another arg.	*/
va_list args;										/* on the stack.			*/
int cnt;										/* NOTE: This does not adjust the ptr	*/
{											/*       to the arguments above!	*/
	char *tst_scratch;

	if (!cnt) return(FALSE);							/* No more arguments to get.		*/

	tst_scratch = va_arg(args, char*);						/* Address of the next argument.	*/
	if (tst_scratch) return(TRUE);							/* Return TRUE, have an address.	*/
	else		 return (FALSE);						/* Return FALSE, no address available.	*/
}

static long write_shrinfo(prname,mem_needed,pp_typ,u_cnt)				/* Do the write to the shared memory.	*/
char *prname, *pp_typ;
int mem_needed;
long u_cnt;
{
	char	*shmaddr;
	char *get_sh_seg();
	int	id,size;
	SHMH	*shmh_ptr;

	if (*prname == ' ' || *prname == 0)						/* Didn't supply a prname!		*/
	{
		return(8L);								/* Does NOT generate a PUTPARM!!!	*/
	}

	if ( pp_label[0] != '\0' && pp_label[0] != ' ' )				/* If Labeled then delete previous	*/
	{										/* (Only 1 PRB with a given label.)	*/
		delete_parm(NULL,pp_label,NULL);
	}

	create_gbl = TRUE;								/* Set flag so .GBL file will be created*/
	shmaddr = get_sh_seg(prname,pp_label,mem_needed,&id,&size);			/* get the memory if first write.	*/
	if (!shmaddr)									/* Some kind of error when trying to	*/
	{										/* the shared memory address.		*/
		return(12L);
	}
	pputparm = (SHMH *) shmaddr;							/* cast our header struct onto it 	*/
	pputparm->prb_id = id;
	pputparm->prb_size = size;
	pputparm->type = *pp_typ;							/* insert the type  			*/
	pputparm->usage_cnt = u_cnt;							/* and usage count			*/
	memcpy(pputparm->prname,prname,8);						/* and prname				*/
	pputparm->keyw_cnt = kw_cnt_sav;						/* and keyword count 			*/
	pputparm->status = P_OK;
	for (p = head, dest=(char *)(shmaddr+sizeof(SHMH)); p; p = p->next, dest += offs)
	{	
		offs = load_keywshm((struct keyw_st_shmem *)dest,p->keyword,p->value,p->len);
		free(p->keyword);							/* free these guys (calloc'd above) 	*/
		free(p->value);
	}
	for (p=head; p; p=p2)								/* now loop thru and free up the  	*/
	{										/* linked list 				*/
		p2 = p->next;
		free(p);
	}
	pputparm->pfkey = assign_pfk;
	if (strcmp(pp_label,"        ")) memcpy(pputparm->label,pp_label,8);		/* Copy the PUTPARM label to structure	*/
	else				 memset(pputparm->label,0,8);			/*  else set it to NULL.		*/
	if (strcmp(pp_reflbl,"        ")) memcpy(pputparm->reflbl,pp_reflbl,8);		/* Copy reference label to structure.	*/
	else				  memset(pputparm->reflbl,0,8);			/*  else set it to NULL.		*/
	pputparm->cleanup = cleanup_opt;

	finish_sh_seg(size);								/* Update counters			*/

	return(0L);
}

static long retrieve_info(type,label)							/* Test if the PUTPARM label exists	*/
char type, *label;									/* and return needed info.		*/
{
	char	*parm_area;
	int	found_keyword, i, j;
	long	ret;
	char	templine[255];
	char	*trptr, *tmp_rcvr, *rptr;
	long	kw_fld_len, kwl_sav, rl_sav;
	char 	*rlenptr, *tl;

	parm_area = (char *)get_prb_area(NULL,label,1);					/* Search for label match.		*/
	if (!parm_area) return(4L);							/* Backwards reference label not found.	*/

	ret = 0L;									/* Assume success.			*/
	pputparm = (SHMH *) parm_area;							/* cast our header struct onto it 	*/

	if ((type == 'R' && rcvr_len != 0) || (type == 'M' && kw_cnt_sav != 0))		/* Only getting PF key value?		*/
	{										/* No.					*/
		if (type == 'R')
		{
			if (!(tmp_rcvr = (char *)malloc((int)tot_len)))			/* Cannot allocat memory.		*/
			{
				werrlog(ERRORCODE(20),type,tot_len,0,0,0,0,0,0);
				return(20L);
			}
			rl_sav = rcvr_len;
			memset(tmp_rcvr,' ',(int)rl_sav);				/* Blank out the temp receiver.		*/
			trptr = tmp_rcvr;						/* Set ptr to temp receiver field.	*/
			rptr = recvr;							/* Set ptr to receiver field.		*/
			rcvr_len = 0;							/* Init # bytes used.			*/
			for (i = 1; i <= pputparm->keyw_cnt; i++)			/* Load each keyword info. into recvr.	*/
			{
				nameptr = trptr;					/* Set ptr to keyword position.		*/
				rlenptr = trptr+8;					/* Set ptr to length position.		*/
				tl = rptr+8;
				lenptr = (long *) tl;					/* Cast char ptr into a long ptr.	*/
				GETBIN(&kwl_sav,lenptr,sizeof(long));			/* Get passed in field length.		*/
				wswap(&kwl_sav);					/* Swap the byte order.			*/
				valptr = trptr+12;					/* Set ptr to data position.		*/
				found_keyword = search_parm_area(templine,nameptr,valptr,kwl_sav,parm_area,i);
				if (found_keyword)
				{
					for (j = 0; j < kwl_sav; j++) *valptr++ = templine[j]; /* Copy value to receiver.	*/
					rcvr_len += 12 + kwl_sav;			/* Increment # bytes used.		*/
					kw_fld_len = kwl_sav;
					wswap(&kw_fld_len);
					PUTBIN(rlenptr,&kw_fld_len,sizeof(long));	/* Write field length to receiver.	*/
				}
				trptr += 12 + kwl_sav;					/* Set ptr in temp receiver to next key.*/
				rptr += 12 + kwl_sav;					/* Set ptr in receiver to next key.	*/
			}
			rptr = recvr;							/* Point to the address of the receiver.*/
			trptr = tmp_rcvr;						/* Point to the start of temp_rcvr.	*/
			for (i = 0; i < rcvr_len; i++) *rptr++ = *trptr++;		/* Copy temp receiver to receiver.	*/
			wswap(&rcvr_len);						/* Return # bytes used by receiver.	*/
			PUTBIN(rlptr,&rcvr_len,sizeof(long));
		}
		else
		{
			p = head;
			for (i = 0; i < kw_cnt_sav; i++)				/* Load each keyword specified.		*/
			{
				nameptr = p->keyword;
				kwl_sav = p->len;					/* Save allowed number of characters.	*/
				valptr = p->value;
				found_keyword = search_parm_area(templine,nameptr,valptr,kwl_sav,parm_area,0);
				if (found_keyword)
				{
					memcpy(m_kwdata[i],templine,(int)kwl_sav);	/* Copy data field into receiver.	*/
				}
				else ret = 20L;						/* Invalid parameter.			*/
				free(p->keyword);					/* free these guys (calloc'd above) 	*/
				free(p->value);
				p = p->next;
			}
			for (p=head; p; p=p2)						/* now loop thru and free up the  	*/
			{								/* linked list 				*/
				p2 = p->next;
				free(p);
			}
		}
	}
	*pfkey_rcvr = pputparm->pfkey;							/* Get aid char from referenced PUTPARM.*/
	if (cleanup_opt == 'C' || cleanup_opt == 'c')					/* Delete the referenced PUTPARM.	*/
	{
		ret = delete_parm(NULL,label,NULL);					/* delete the area.			*/
	}

	return(ret);
}

static long process_reflbl(prname,reflbl,mem_needed)					/* Set up needed keywords and values.	*/
char *prname, *reflbl;
int *mem_needed;
{
	SHMH	*ref;
	char	*parm_area;
	int	found_keyword, i, j, cnt;
	long	ret;
	char	templine[255];
	int	kw_fld_len;

	parm_area = (char *)get_prb_area(NULL,reflbl,1);				/* Search for reference label match.	*/
	if (!parm_area) return(4L);							/* Backwards reference label not found.	*/

	ret = 0L;									/* Assume success.			*/
	ref = (SHMH *) parm_area;							/* cast our header struct onto it 	*/

	if (kw_cnt_sav == 0)								/* Use keywords and value fields from	*/
	{										/* referenced PUTPARM label to generate	*/
		kw_cnt_sav = cnt = ref->keyw_cnt;					/* a new PUTPARM.			*/
		i = 1;									/* Set so gets first keyword info.	*/
		for (head=p=NULL,dest=NULL, *mem_needed=sizeof(SHMH); cnt; --cnt)
		{
			if (!head) 
			{
				p = head = (KEYW *)calloc(1,sizeof(KEYW));
			}
			else
			{
				p->next = (KEYW *)calloc(1,sizeof(KEYW));
				p = p->next;
			}
			p->keyword = (char *)calloc(8,sizeof(char));			/* grab storage 			*/
			nameptr = p->keyword;
			search_parm_area(templine,nameptr,valptr,255L,parm_area,i);	/* Get the info from reference.		*/
			kw_fld_len = 0;
			while (templine[kw_fld_len] != ' ') kw_fld_len++;
			memcpy(p->keyword,nameptr,8);					/* copy the keyword in (incl. spaces) 	*/
			p->value = (char *)calloc(kw_fld_len+1,sizeof(char));		/* grab space 				*/
			memcpy(p->value,templine,kw_fld_len);				/* copy the data into structure.	*/
			p->len = kw_fld_len;						/* set data length.			*/
			*mem_needed += sizeof(KEYWSHM) + 8 + kw_fld_len + 1;		/* need enough bytes for shm struct 	*/
			i++;								/* Increment so finds next keyword.	*/
		}
	}										/* else create PUTPARM with keywords &	*/
	else										/* values specified and then update	*/
	{										/* values matching keywords in 		*/
		p = head;								/* referenced PUTPARM.			*/
		for (i = 0; i < kw_cnt_sav; i++)					/* Load each keyword specified.		*/
		{
			nameptr = p->keyword;
			kw_fld_len = p->len;
			valptr = p->value;						/* Search for match of keyword.		*/
			found_keyword = search_parm_area(templine,nameptr,valptr,kw_fld_len,parm_area,0);
			if (found_keyword) memcpy(valptr,templine,kw_fld_len);		/* Copy returned value into new PUTPARM.*/
			p = p->next;
		}
	}
	if (cleanup_opt == 'C' || cleanup_opt == 'c')					/* Delete the referenced PUTPARM.	*/
	{
		ret = delete_parm(NULL,reflbl,NULL);					/* delete the area.			*/
	}

	return(ret);
}

/*
	find_prb_keyword	Return a pointer to the keyword in a PRB.
*/

KEYWSHM *find_prb_keyword(parm_a,key,kw_num)
char	*key; 								/* Keyword to search for (if kw_num==0)			*/
char	*parm_a;							/* Parm area ptr (SHMH *)				*/
int 	kw_num;								/* Relative keyword to find (if kw_num != 0)		*/
{
	char 	l_keyword[9], 						/* Local keyword (Null terminated)			*/
		d_keyword[9];						/* Keyword from PRB (Null terminated)			*/
	char 	*data, 							/* Ptr to keyword in PRB				*/
		*strchr();
	char 	*str,							/* Ptr for going thru PRB				*/
		*p; 							/* Temp ptr						*/
	int 	kw_cnt; 						/* Which keyword are we looking at in the loop		*/
	short 	offs,							/* Offset to next keyword in PRB			*/
		cnt;							/* Number of keywords in PRB 				*/
	KEYWSHM	*ptr_shm;						/* Pointer to keyword in PRB				*/

	if ( kw_num == 0 )								/* if scan by KEY match			*/
	{
		if (!key) return(0);							/* no key passed			*/

		memcpy(l_keyword,key,8);						/* Copy key into local variable.	*/
		l_keyword[8] = '\0';	
		p = strchr(l_keyword,' ');						/* Find first occurance of space & null	*/
		if (p) *p=(char)0;							/* terminate the string.		*/
	}

	memcpy(&cnt,&(((SHMH*)parm_a)->keyw_cnt),sizeof(short));			/* Get the number of keywords in PRB	*/

	str = parm_a+sizeof(SHMH);							/* Point to first keyword		*/
	for (kw_cnt=1; kw_cnt<=cnt; kw_cnt++, str = str+offs) 
	{
		ptr_shm = (KEYWSHM *)str;
		data = (char *)ptr_shm + sizeof(KEYWSHM);				/* get ptr to keyword in PRB		*/

		memcpy(d_keyword,data,8);						/* Load & null terminate the keyword	*/
		d_keyword[8] = '\0';
		p=strchr(d_keyword,' ');
		if (p) *p=(char)0;

		if ( kw_num == kw_cnt || 						/* If requested keyword # or		*/
		    (kw_num == 0 && !strncmp(d_keyword,l_keyword,8)))			/* a keyword match.			*/
		{
			return( ptr_shm );						/* Return the ptr			*/
		}
	        memcpy(&offs,&(ptr_shm->next_offs),sizeof(short));			/* Get offset to next keyword		*/
	}
	return( 0 );
}


/*
	search_parm_area	Search a given PRB for a keyword (or by number -- kw_num) and return the value in dest.
				If keyword found, dest is set to spaces (for maxlen) then loaded with the value found.
				Returns 0 if not found.  If kw_num != 0 then it also loads the keyword into key.
*/

int search_parm_area(dest,key,val,maxlen,parm_a,kw_num)
char 	*dest,								/* Destination to load value found.			*/
	*key, 								/* Keyword to search for (if kw_num==0)			*/
	*val, 								/* UNUSED						*/
	*parm_a;							/* Parm area ptr (SHMH *)				*/
long 	maxlen;								/* Maxlen of dest					*/
int 	kw_num;								/* Relative keyword to find (if kw_num != 0)		*/
{
	KEYWSHM	*prb_keystruct;						/* Pointer to keyword struct in PRB			*/
	char	*prb_keyword;						/* Pointer to actual keyword in PRB			*/
	short	len;

	prb_keystruct = find_prb_keyword(parm_a,key,kw_num);

	if (!prb_keystruct)						/* Not found						*/
	{
		return(0);
	}

	prb_keyword = (char *)prb_keystruct + sizeof(KEYWSHM);		/* Get ptr to keyword in PRB				*/

	if (kw_num != 0)						/* Copy the keyword to key				*/
	{
		memcpy(key,prb_keyword,8);
	}
	memcpy(&len,&(prb_keystruct->value_len),sizeof(short));		/* Load the value into dest.		*/
	memset(dest,' ',(int)maxlen);
	strncpy(dest,prb_keyword+8, ((len > (int)maxlen) ? (int)maxlen : len));

	return(1);
}

#ifdef OLD

int search_parm_area(dest,key,val,maxlen,parm_a,kw_num)
char 	*dest,								/* Destination to load value found.			*/
	*key, 								/* Keyword to search for (if kw_num==0)			*/
	*val, 								/* UNUSED						*/
	*parm_a;							/* Parm area ptr (SHMH *)				*/
long 	maxlen;								/* Maxlen of dest					*/
int 	kw_num;								/* Relative keyword to find (if kw_num != 0)		*/
{
	char 	l_keyword[9], 						/* Local keyword (Null terminated)			*/
		d_keyword[9];						/* Keyword from PRB (Null terminated)			*/
	char 	*data, 							/* Ptr to keyword in PRB				*/
		*strchr();
	char 	*str,							/* Ptr for going thru PRB				*/
		*p, 							/* Temp ptr						*/
		*dptr, 							/* Ptr to key in PRB					*/
		*kptr;							/* Ptr to key in arguments				*/
	int 	i, j, 
		kw_cnt, 						/* Which keyword are we looking at in the loop		*/
		kwl;							/* Length of keyword.					*/
	short 	offs,							/* Offset to next keyword in PRB			*/
		len, 							/* Length of current value				*/
		cnt;							/* Number of keywords in PRB 				*/
	KEYWSHM *ptr_shm;
	
	kptr = key;

	memcpy(l_keyword,key,8);							/* Copy key into local variable.	*/
	l_keyword[8] = '\0';	
	p = strchr(l_keyword,' ');							/* Find first occurance of space & null	*/
	if (p) *p=(char)0;								/* terminate the string.		*/

	kw_cnt = 1;									/* Start with first keyword info.	*/
	memcpy(&cnt,&(((SHMH*)parm_a)->keyw_cnt),sizeof(short));			/* Get the number of keywords in PRB	*/

	for (i = cnt, str = parm_a+sizeof(SHMH); i; 
	     --i, str = str+offs) 
	{
		ptr_shm = (KEYWSHM *)str;
		data = (char *)ptr_shm + sizeof(KEYWSHM);				/* get ptr to keyword in PRB		*/
		memcpy(d_keyword,data,8);
		d_keyword[8] = '\0';
		p=strchr(d_keyword,' ');
		if (p) *p=(char)0;

		if (kw_num == kw_cnt || !strncmp(d_keyword,l_keyword,8))		/* If at requested keyword # or match	*/
		{									/* the keyword value.			*/
			if (kw_num == kw_cnt)						/* Copy the keyword to key		*/
			{
				dptr = d_keyword;					/* Set pointer to keyword.		*/
				kwl = strlen(d_keyword);
				for (j = 0; j < kwl; j++) *kptr++ = *dptr++;
				for (j = kwl; j < 8; j++) *kptr = ' ';			/* Pad with spaces.			*/
			}
			memcpy(&len,&(ptr_shm->value_len),sizeof(short));		/* Load the value into dest.		*/
			memset(dest,' ',maxlen);
			strncpy(dest,data+8, ((len > maxlen) ? maxlen : len));
			return 1;
		}
	        memcpy(&offs,&(ptr_shm->next_offs),sizeof(short));			/* Get offset to next keyword		*/
		kw_cnt++;								/* Increment the keyword position.	*/
	}
	return 0;
}

#endif




