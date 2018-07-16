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

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wshmem.h"
#include "putparm.h"
#include "wdefines.h"
#include "wglobals.h"

static int	test_nextarg();
static int4 	write_putparm();							/* Do the write to the shared memory.	*/
static int	block_fmtlist();
static int      test_next_arg();

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif

/*
**	PUTPARM('D/E', [Usage_count], Prname,  Keyword_count, {Keyword,Value,Length}, PFkey, PPlabel, Reflabel, Cleanup, RC)
**	PUTPARM('M',                  PPlabel, Keyword_count, {Keyword,Recvr,Length}, PFkey,                    Cleanup, RC)
**	PUTPARM('R',                  PPlabel, Reciever, RecvLength, Totlength,       PFkey,                    Cleanup, RC)
**	PUTPARM('C',                 [PPlabel], RC)
*/
int PUTPARM(va_alist)
va_dcl
{
#define		ROUTINE		48000

	va_list the_args;								/* A pointer to traverse the stack.	*/
	int	arg_count;								/* Called with this many arguments.	*/
	int4	usage_count, keyword_count;
	int4	*long_item;
	int4	swap_me, *return_code;
	char	*pp_function, *prname, *pplabel_arg, *byte_pointer;			/* Pointers to args off the stack.	*/
	char 	pp_label[9], pp_reflbl[9];
	int	rc_fl, idx;
	int4	ret;
	int4	process_ref;
	KEYW	*fmtlist;
	char	*ptr;
	char	*r_receiver;
	int4	*rlptr, rcvr_len, *tlptr, tot_len;
	char	*pfkey_rcvr, assign_pfk, cleanup_opt;
	char	**m_kwdata;

	werrlog( ERRORCODE(1),0,0,0,0,0,0,0,0 );

	return_code = (int4 *)NULL;							/* initialize ret code pointer		*/
	rc_fl = FALSE;									/* Init flag, if have return code addrs.*/
	strcpy(pp_label, "        ");							/* Init label.				*/
	strcpy(pp_reflbl,"        ");							/* Init label.				*/

	process_ref = FALSE;

	va_start(the_args);    		 						/* Init. pointer to top of stack.	*/
	arg_count = va_count(the_args);							/* How many args are there ?	      	*/
                                                                                                                                  
	va_start(the_args);								/* Init. pointer to top of stack.	*/
                                                                                                                                  
	pp_function = va_arg(the_args, char*);						/* What type of putparm are we doin' ?	*/
	arg_count -= 1;									/* One less argument.			*/

	switch (*pp_function)
	{
		case 'D':
		case 'E':
		case 'C':
		case 'R':
		case 'M':
			break;
		default:
		{
			werrlog(ERRORCODE(18),*pp_function,0,0,0,0,0,0,0);		/* Function not implemented.		*/
			return(0);
		}
	}										/* Test for USAGE COUNT or PRNAME.	*/
											/* If C,R,M test for PUTPARM label.	*/
	long_item     = va_arg(the_args, int4*);					/* Address of the usage count, maybe.	*/
	arg_count -= 1;									/* One less argument.			*/
	byte_pointer = (char *) long_item;

	usage_count = 1;								/* Default to this value.		*/
	if (('D' == *pp_function || 'E' == *pp_function) &&
	    longargtest(byte_pointer, 2) 			)			/* Did they specify the usage count ?	*/
	{
		GETBIN(&swap_me,long_item,sizeof(int4));
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


	/*
	**	Clean Up (Function C)
	*/
	if ('C' == *pp_function)							/* Delete and set return code.		*/
	{										/* PUTPARM label is in prname for 'C'.	*/
		if ( 1 == arg_count )
		{
			long_item = va_arg(the_args,int4*);				/* Get the PUTPARM return code address.	*/
			return_code = long_item;					/* Save the address of the return code.	*/
		}
		else
		{
			return_code = long_item;
			pplabel_arg = "        ";					/* Trigger an ERASE-ALL			*/
		}

		if (pplabel_arg && ' ' != pplabel_arg[0])				/* If a label was supplied then		*/
		{
			SHMH	*prb;
			prb = (SHMH *)get_prb_area(NULL,pplabel_arg,OK_USED);		/* find the PRB				*/
			if (prb)
			{
				ret = (int4)erase_prb(prb);				/* erase the prb			*/
			}
			else
			{
				ret = 4L;						/* PRB was not found			*/
			}
		}
		else	/* Erase all PRB's at this level */
		{
			ret = (int4)erase_prb_level();					/* Erase all PRB's at this level	*/
		}

		wswap(&ret);
		PUTBIN(return_code,&ret,sizeof(int4));
		cleanup_shrfil();
		return(0);								/* Done with function 'C'.		*/
	}


	ret = 0;									/* Continue processing.			*/

	keyword_count = 0;								/* Initialize keyword count		*/
	long_item = va_arg(the_args, int4*);						/* Addr. of the no. of keywords, maybe.	*/
	arg_count -= 1;									/* One less argument.			*/
	GETBIN(&swap_me,long_item,sizeof(int4));					/* Get a local copy to be swapped.	*/
	wswap(&swap_me);								/* Swap the byte order.			*/
	if (test_next_arg(the_args,arg_count))						/* Are there any more arguments ?	*/
	{										/* Yes, so test which function.		*/
/*
**	PUTPARM('D/E', [Usage_count], Prname,  ^ Keyword_count, {Keyword,Value,Length}, PFkey, PPlabel, Reflabel, Cleanup, RC)
**	PUTPARM('M',                  PPlabel, ^ Keyword_count, {Keyword,Recvr,Length}, PFkey,                    Cleanup, RC)
**	PUTPARM('R',                  PPlabel, ^ Receiver, RecvLength, Totlength,       PFkey,                    Cleanup, RC)
*/
		if ('R' == *pp_function)						/* Is 'R' so is the receiver and no	*/
		{									/*  keywords to be processed,		*/
			r_receiver  = (char *)long_item;				/* Set local copy of this value.	*/
			long_item = va_arg(the_args, int4*);				/* Addr. of the receiver length.	*/
			rlptr = long_item;						/* Save the pointer.			*/
			GETBIN(&rcvr_len,rlptr,sizeof(int4));
			wswap(&rcvr_len);						/* Swap the byte order.			*/
			long_item = va_arg(the_args, int4*);				/* Addr. of the total length, maybe.	*/
			arg_count -= 1;							/* One less argument.			*/
			if (test_next_arg(the_args,arg_count))				/* Are there any more arguments ?	*/
			{								/* Yes, so test which function.		*/
				tlptr = long_item;
				GETBIN(&tot_len,tlptr,sizeof(int4));			/* Set local copy of this value.	*/
				wswap(&tot_len);					/* Swap the byte order.			*/
			}
			else								/* Nope.  This is the return code.	*/
			{
				tlptr = NULL;
				return_code = long_item;				/* Save the address of the return code.	*/
				rc_fl = TRUE;
			}
		} 									/* else is the keyword count.		*/
		else 
		{
			keyword_count = swap_me;					/* Set local copy of this value.	*/
		}
	}
	else
	{										/* Nope.  This is the return code.	*/
		return_code = long_item;						/* Save the address of the return code.	*/
		rc_fl = TRUE;
	}
                        
/*
**	PUTPARM('D/E', [Usage_count], Prname,  Keyword_count, ^ {Keyword,Value,Length}, PFkey, PPlabel, Reflabel, Cleanup, RC)
**	PUTPARM('M',                  PPlabel, Keyword_count, ^ {Keyword,Recvr,Length}, PFkey,                    Cleanup, RC)
**	PUTPARM('R',                  PPlabel, Reciever, RecvLength, Totlength,       ^ PFkey,                    Cleanup, RC)
*/
	fmtlist = NULL;
	m_kwdata = NULL;
	for (idx = 0; idx < keyword_count; idx++)
	{
		KEYW	*p;
		char	*nameptr, *valptr;
		int4	*lenptr, len;

		/*
		**	This loop is only done for D,E, and M that have a keyword_count.
		**
		**	Build a linked list of the keywords.
		**
		**	fmtlist -> KEYW -> KEYW -> KEYW ->(NULL)
		*/
		if (!fmtlist) 
		{
			p = fmtlist = (KEYW *)calloc(1,sizeof(KEYW));
			if ('M' == *pp_function) m_kwdata = (char **)calloc((size_t)keyword_count,sizeof(char *));
		}
		else
		{
			p->next = (KEYW *)calloc(1,sizeof(KEYW));
			p = p->next;
		}
		p->next = NULL;

		nameptr	= va_arg(the_args,char*);
		valptr	= va_arg(the_args,char*);
		if ('M' == *pp_function) m_kwdata[idx] = valptr;			/* Save ptr to keyword data receiver.	*/
		lenptr	= va_arg(the_args,int4*);
		arg_count -= 3;
		GETBIN(&len,lenptr,sizeof(int4));
		wswap(&len);
		if (-1 == len)
		{
			/*
			**	If len == -1 then this is the special case of a single keyword being backwards referenced
			**	The value field will be 16 bytes and contain the LABEL (8) and KEYWORD (8) blank padded.
			**	This will only occur from the LEXICAL procedure interrupter.
			*/
			p->special = SPECIAL_KEY;
			len = 16;
		}
		else
		{
			p->special = SPECIAL_NOT;
		}

		p->len = len;								/* set len 				*/
		loadpad(p->keyword,nameptr,8);						/* copy the keyword in (incl. spaces) 	*/

		/*
		**	Special case for FILE, LIBRARY, VOLUME to ensure that if backwards referenced the fields are big enough.
		**	len will be set to the parameter length, p->len will be set to the adjusted length.
		*/
		if      (0==memcmp(p->keyword,"FILE    ",8)) { p->len = MAX(p->len,8); }
		else if (0==memcmp(p->keyword,"LIBRARY ",8)) { p->len = MAX(p->len,8); }
		else if (0==memcmp(p->keyword,"VOLUME  ",8)) { p->len = MAX(p->len,6); }

		p->value = (char *)calloc((int)(p->len+1),(int)sizeof(char));		/* grab space 				*/
		if (p->len > len) memset(p->value,' ',(size_t)p->len);			/* if special case then blank fill	*/
		memcpy(p->value,valptr,(int)len);					/* copy len bytes 			*/
	}

/*
**	PUTPARM('D/E', [Usage_count], Prname,  Keyword_count, {Keyword,Value,Length}, ^ PFkey, PPlabel, Reflabel, Cleanup, RC)
**	PUTPARM('M',                  PPlabel, Keyword_count, {Keyword,Recvr,Length}, ^ PFkey,                    Cleanup, RC)
**	PUTPARM('R',                  PPlabel, Reciever, RecvLength, Totlength,       ^ PFkey,                    Cleanup, RC)
*/
	assign_pfk = '@';
	pfkey_rcvr = &assign_pfk;
	if (!rc_fl)									/* If more parameters.			*/
	{
		ptr = va_arg(the_args, char*);						/* Get the address of PFkey, maybe.	*/
		--arg_count;
		if (test_next_arg(the_args,arg_count))					/* Are there any more arguments ?	*/
		{									/* Yes, so set pfkey address.		*/
			pfkey_rcvr = ptr;
			assign_pfk = *ptr;

			if (' ' == assign_pfk || (char)0 == assign_pfk)			/* If AID is space or null then use '@'	*/
			{
				assign_pfk = '@';
			}
		}
		else									/* No, so set return code address.	*/
		{
			return_code = (int4 *)ptr;					/* Save the address of the return code.	*/
			rc_fl = TRUE;
		}
	}

	if (('D' == *pp_function || 'E' == *pp_function) && !rc_fl)			/* Continue processing 'D' and 'E'.	*/
	{
		ptr = va_arg(the_args, char*);						/* Get the address of the label, maybe.	*/
		--arg_count;
		if (test_next_arg(the_args,arg_count))					/* Are there any more arguments ?	*/
		{									/* Yes, so set label address.		*/
			memcpy(pp_label,ptr,8);
			pp_label[8] = '\0';						/* Null terminate the string.		*/
		}
		else									/* No, so set return code address.	*/
		{
			return_code = (int4 *)ptr;					/* Save address of the return code.	*/
			rc_fl = TRUE;
		}
		if (!rc_fl)								/* Get address of reference label, maybe*/
		{
			ptr = va_arg(the_args, char*);
			--arg_count;
			if (test_next_arg(the_args,arg_count))				/* Are there any more arguments ?	*/
			{								/* Yes, so set reference label address.	*/
				memcpy(pp_reflbl,ptr,8); 				/* Set up kws & vals from reference.	*/
				pp_reflbl[8] = '\0';					/* null terminate the string.		*/
				if (strcmp(pp_reflbl,"        "))			/* If a reference label has been	*/
				{							/* specified then set flag so will try	*/
					process_ref = TRUE; 				/*  to access it.			*/
				}
			}
			else								/* No, so set return code address.	*/
			{
				return_code = (int4 *)ptr;				/* Save address of the return code.	*/
				rc_fl = TRUE;
			}
		}
	}
	cleanup_opt = ' ';								/* Set the default cleanup options.	*/
	if (!rc_fl)									/* Get address of cleanup opts, maybe.	*/
	{
		ptr = va_arg(the_args, char*);
		--arg_count;
		if (test_next_arg(the_args,arg_count))					/* Are there any more arguments ?	*/
		{									/* Yes, so set clean up options address.*/
			cleanup_opt = *ptr;
		}
		else									/* No, so set return code address.	*/
		{
			return_code = (int4 *)ptr;					/* Save address of the return code.	*/
			rc_fl = TRUE;
		}
	}
	
	if (!rc_fl) return_code = va_arg(the_args, int4*);				/* Last arg is the return code receiver.*/

	/*
	**	All args have been parsed -- Handle the requests!
	*/

	if ('R' == *pp_function || 'M' == *pp_function)				/* Retreive info for 'R' and 'M'.		*/
	{
		KEYW	*ref_fmtlist;
		char	*parm_area;

		/*
		**	This section handles both 'R' and 'M' requests.
		*/
		parm_area = (char *)get_prb_area(NULL,pplabel_arg,OK_USED);	/* Search for label match.			*/
		if (parm_area)							/* If reference label found.			*/
		{								
			ret = load_fmtlist(parm_area,&ref_fmtlist);		/* Load the ref PRB into a fmtlist		*/
			if (0==ret && 'M' == *pp_function)
			{
				/*
				**	This handles the 'M' (multiple) request.  Each matching keyword is updated.
				**	If no fmtlist then only getting the pfkey.
				*/
				if (ref_fmtlist && fmtlist)			/* If we have both fmtlists then merge them	*/
				{
					KEYW	*p;
					int	ii;

					mergex_fmtlist(ref_fmtlist,fmtlist);	/* Merge the ref into the dest fmtlist		*/
					for(p=fmtlist,ii=0; p; p=p->next,ii++)	/* Replace the updated values			*/
					{
						memcpy(m_kwdata[ii],p->value,(int)(p->len));
					}
					free(m_kwdata);				/* Free the holding area			*/
				}
			}
			else if (0==ret && 'R' == *pp_function)
			{
				/*
				**	This handles the 'R' (receiver) request.  
				**	The whole fmtlist is written to the receiver as a formated block.
				*/
				block_fmtlist(ref_fmtlist,r_receiver,&rcvr_len,&tot_len);
				wswap(&rcvr_len);		
				PUTBIN(rlptr,&rcvr_len,sizeof(int4));		/* Return # bytes used by receiver.		*/
				if (tlptr)
				{
					wswap(&tot_len);			/* Total bytes needed				*/
					PUTBIN(tlptr,&tot_len,sizeof(int4));
				}
			}

			free_fmtlist(ref_fmtlist);				/* Free the reference fmtlist			*/

			if (0==ret)
			{
				*pfkey_rcvr = ((SHMH *)parm_area)->pfkey;	/* Get aid char from referenced PUTPARM.	*/

				if ('C' == cleanup_opt || 'c' == cleanup_opt)	/* Cleanup the referenced PUTPARM.		*/
				{
					erase_prb((struct shm_header *)parm_area);
				}
			}
		}
		else
		{
			ret = 4;						/* Back ref not found				*/
		}
	}
	else /* 'D' and 'E' */							/* Only ENTER and DISPLAY generate a new PRB	*/
	{
		/*
		**	This section handles 'D' and 'E' requests.
		*/

		if (process_ref) 
		{
			char	*ref_prb;

			ref_prb = (char *)get_prb_area(NULL,pp_reflbl,OK_USED);		/* Search for label match.		*/
			if (ref_prb)							/* If reference label found.		*/
			{
				/*
				**	Ensure that the reference PRB exists.
				**	If there is an fmtlist then do the backwards reference now, otherwise it
				**	will be done when the GETPARM occurs.
				*/
				if (fmtlist)
				{
					KEYW	*ref_fmtlist;
					ret = load_fmtlist(ref_prb,&ref_fmtlist);	/* Load the reference fmtlist		*/

					if (0==ret && ref_fmtlist)			/* If OK then merge the fmtlists	*/
					{
						mergex_fmtlist(ref_fmtlist,fmtlist);
					}
					free_fmtlist(ref_fmtlist);

					if ('C' == cleanup_opt || 'c' == cleanup_opt)	/* Cleanup the referenced PUTPARM.	*/
					{
						erase_prb((struct shm_header *)ref_prb);
					}
					memset(pp_reflbl,' ',8);			/* Clear the reflbl - it's been done	*/
				}
			}
			else
			{
				ret = 4;
			}
		}

		if (0==ret)
		{
			ret = write_putparm(pp_function,usage_count,prname,fmtlist,assign_pfk,pp_label,pp_reflbl,cleanup_opt);
		}
	}

	free_fmtlist(fmtlist);
	cleanup_shrfil();								/* Cleanup shared memory		*/

	wswap(&ret);
	PUTBIN(return_code,&ret,sizeof(int4));						/* Set the return code for PUTPARM.	*/
	return(0);
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

/*
**	Routine:	write_putparm()
**
**	Function:	To write a new putparm into shared memory.
**
**	Description:	Calc the memory needed and keyword count.
**			Create a table entry and get a chunk of shared memory.
**			Write the info into the structures.
**
**	Arguments:
**	function	The type of putparm, 'D' or 'E'.
**	ucount		The usage count
**	prname		The parameter reference name.
**	fmtlist		The formated list of keyword-value pairs
**	pfkey		The pfkey AID value to use.
**	pp_label	The label
**	pp_reflbl	The reference label
**	cleanup		The cleanup option for the referenced putparm
**
**	Globals:	None
**
**	Return:
**	0		Success
**	8		No prname
**	12		Unable to get shared memory
**
**	Warnings:	None
**
**	History:	
**	08/27/92	Written by GSL
**
*/

static int4 write_putparm(function,ucount,prname,fmtlist,pfkey,pp_label,pp_reflbl,cleanup)
char	*function;	/* 'D' or 'E' */
int4 	ucount;
char 	*prname;
KEYW	*fmtlist;
char	pfkey;
char	*pp_label;
char	*pp_reflbl;
char	cleanup;
{
	int 	mem_needed;
	int	keyword_count;
	char	*shmaddr;
	int	id,size;
	SHMH 	*pputparm;

	if (!prname || ' ' == *prname || 0 == *prname)					/* Didn't supply a prname!		*/
	{
		return(20L);								/* Does NOT generate a PUTPARM!!!	*/
	}

#ifdef OLD
	There can be multiple putparms with the same LABEL at different link-levels.

	if ( pp_label[0] && pp_label[0] != ' ' )					/* If Labeled then delete previous	*/
	{										/* (Only 1 PRB with a given label.)	*/
		erase_prb((struct shm_header *)get_prb_area(NULL,pp_label,OK_USED));	/* Get & erase the labeled PRB		*/
	}
#endif

	size_fmtlist(fmtlist,&keyword_count,&mem_needed);				/* Get size values for fmtlist		*/

	shmaddr = get_sh_seg(prname,pp_label,mem_needed,&id,&size);			/* get the memory if first write.	*/
	if (!shmaddr)									/* Some kind of error when trying to	*/
	{										/* the shared memory address.		*/
		return(12L);
	}
	pputparm = (SHMH *) shmaddr;							/* cast our header struct onto it 	*/
	pputparm->prb_id 	= id;
	pputparm->prb_size 	= size;
	pputparm->type 		= *function;						/* insert the function type  		*/
	pputparm->usage_cnt 	= ucount;						/* and usage count			*/
	memcpy(pputparm->prname,prname,8);						/* and prname				*/
	pputparm->keyw_cnt 	= keyword_count;					/* and keyword count 			*/
	pputparm->status 	= P_OK;

	write_fmtlist(pputparm,fmtlist);

	pputparm->pfkey = pfkey;
	if (strcmp(pp_label,"        ")) memcpy(pputparm->label,pp_label,8);		/* Copy the PUTPARM label to structure	*/
	else				 memset(pputparm->label,0,8);			/*  else set it to NULL.		*/
	if (strcmp(pp_reflbl,"        ")) memcpy(pputparm->reflbl,pp_reflbl,8);		/* Copy reference label to structure.	*/
	else				  memset(pputparm->reflbl,0,8);			/*  else set it to NULL.		*/
	pputparm->cleanup = cleanup;

	finish_sh_seg(size);								/* Update counters			*/

	return(0L);
}

/*
**	Routine:	write_fmtlist()
**
**	Function:	To write a fmtlist to PRB
**
**	Description:	This routine is given a pointer to a PRB and a fmtlist and it writes the contents of the
**			fmtlist into the PRB following the PRB header.
**
**	Arguments:
**	prb		The PRB pointer.
**	fmtlist		The fmtlist to write from.
**
**	Globals:	None
**
**	Return:		0
**
**	Warnings:	None
**
**	History:	
**	08/28/92	Written by GSL
**
*/
int write_fmtlist(prb,fmtlist)
SHMH	*prb;
KEYW	*fmtlist;
{
	char	*dest;
	KEYW	*p;
	int	offs;

	dest = (char *)prb;
	dest += sizeof(SHMH);							/* Point to after the PRB header		*/

	for (p=fmtlist; p; p=p->next)						/* Loop thru and write each keywshm item	*/
	{	
		offs = load_keywshm((struct keyw_st_shmem *)dest,p);
		dest += offs;
	}
	return(0);
}

/*
**	Routine:	size_fmtlist()
**
**	Function:	To calc the size of this fmtlist. (Memory needed and keyword count)
**
**	Description:	Loop thru the fmtlist adding up the values.
**
**	Arguments:
**	fmtlist		The fmtlist to size.
**	cnt		The number of items found in fmtlist.
**	mem		The memory size needed to write a PRB (including PRB header)
**
**	Globals:	None
**
**	Return:		0
**
**	Warnings:	None
**
**	History:	
**	08/28/92	Written by GSL
**
*/
int size_fmtlist(fmtlist,cnt,mem)
KEYW	*fmtlist;
int	*cnt;
int	*mem;
{
	KEYW	*p;

	*cnt = 0;
	*mem = sizeof(SHMH);								/* PRB header size			*/
	for(p=fmtlist; p; p=p->next)
	{
		*cnt += 1;
		/* Each keyword takes: size of KEYWSHM struct + 8 for keyword + length of value + null terminator		*/
		*mem += sizeof(KEYWSHM) + 8 + p->len + 1;
	}
	return(0);
}

/*
**	Routine:	load_fmtlist()
**
**	Function:	To load a fmtlist from a PRB in shared memory.
**
**	Description:	This routine is passed a pointer to an in memory PRB and it builds a fmtlist from it.
**
**	Arguments:
**	parm_area	The putparm PRB
**	fmtlist_src	The fmtlist to create
**
**	Globals:	None
**
**	Return:
**	0		Success
**	12		keyword missing
**
**	Warnings:	None
**
**	History:	
**	08/26/92	Written by GSL
**
*/

int load_fmtlist(parm_area,fmtlist_ptr)
char 	*parm_area;
KEYW	**fmtlist_ptr;
{
	SHMH 	*pputparm;
	int	i;
	int	ret;

	ret = 0;									/* Assume success.			*/
	pputparm = (SHMH *) parm_area;							/* cast our header struct onto it 	*/

	*fmtlist_ptr = NULL;

	for (i = 1; i <= pputparm->keyw_cnt; i++)
	{
		KEYW	*p;
		KEYWSHM	*prb_keystruct;						/* Pointer to keyword struct in PRB		*/
		char	*prb_keyword;						/* Pointer to actual keyword in PRB		*/
		short	len;

		prb_keystruct = find_prb_keyword(parm_area,NULL,i);		/* Get pointer to keyword struct		*/

		if (!prb_keystruct)
		{
			ret = 12;
			break;
		}

		if (!*fmtlist_ptr) 
		{
			p = *fmtlist_ptr = (KEYW *)calloc(1,sizeof(KEYW));
		}
		else
		{
			p->next = (KEYW *)calloc(1,sizeof(KEYW));
			p = p->next;
		}
		p->next = NULL;

		prb_keyword = (char *)prb_keystruct + sizeof(KEYWSHM);		/* Get ptr to keyword in PRB			*/
		memcpy(p->keyword,prb_keyword,8);				/* copy the keyword in (incl. spaces) */

		memcpy(&len,&(prb_keystruct->value_len),sizeof(short));		/* Load the value into dest.			*/
		p->len = (int4)len;						/* set len */

		p->value = (char *)calloc((int)len+1,(int)sizeof(char));	/* grab space */
		memcpy(p->value,prb_keyword+8,(int)len);			/* copy len bytes */
		p->special = prb_keystruct->special;
	}


	return(ret);
}

/*
**	Routine:	mergex_fmtlist()
**
**	Function:	To merge (exclusive) two fmtlist's
**
**	Description:	This routine does an exclusive merge of two fmtlist's.
**			If a Keyword is in both the source and destination then it's value is copied into the destination.
**			The source field will be truncated or blank padded to fit into the dest field.
**
**	Arguments:	
**	src		The source fmtlist
**	dest		The destination fmtlist
**
**	Globals:	None
**
**	Return:		0	Success
**			1	Nothing merged
**
**	Warnings:	None
**
**	History:	
**	08/26/92	Written by GSL
**
*/

int mergex_fmtlist(src,dest)
KEYW	*src, *dest;
{
	KEYW	*s, *d;
	int	rc;

	rc = 1;									/* Nothing has been merged			*/
	if (!src || !dest) return(rc);

	for(d=dest; d; d=d->next)
	{
		for(s=src; s; s=s->next)
		{
			if (0==memcmp(d->keyword,s->keyword,8) && SPECIAL_NOT == s->special)
			{
				/*
				**	The destination (d) should never be type SPECIAL, the backwards reference should
				**	have already resolved it.  If the source (s) is type SPECIAL then it has not yet been
				**	resolved so ignore it. (This also should never happen.)
				*/
				d->special = s->special;
				memset(d->value,' ',(int)(d->len));
				memcpy(d->value,s->value,(int)MIN(d->len,s->len));
				rc = 0;						/* Something has been merged			*/
				break;
			}
		}
	}	
	return(rc);
}

/*
**	Routine:	merge_fmtlist()
**
**	Function:	To merge (additive) two fmtlist's
**
**	Description:	This routine does an additive merge of two fmtlist's.
**			If a Keyword is in both the source and destination then it's value is copied into the destination.
**			The source field will be truncated or blank padded to fit into the dest field.
**
**	Arguments:	
**	src		The source fmtlist
**	dest		The destination fmtlist
**
**	Globals:	None
**
**	Return:		0	Success
**			1	Nothing merged
**
**	Warnings:	None
**
**	History:	
**	12/11/92	Written by GSL
**
*/

int merge_fmtlist(src,dest_ptr)
KEYW	*src, **dest_ptr;
{
	KEYW	*s, *d, *append_list, *p;
	int	rc;
	int	item_merged;

	rc = 1;									/* Nothing has been merged			*/
	if (!src) return(rc);

	append_list = NULL;

	for(s=src; s; s=s->next)
	{
		item_merged = 0;
		for(d = *dest_ptr; d; d = d->next)
		{
			if (0==memcmp(d->keyword,s->keyword,8))
			{
				/*
				**	The destination (d) should never be type SPECIAL, the backwards reference should
				**	have already resolved it.  If the source (s) is type SPECIAL then it has not yet been
				**	resolved so ignore it. (This also should never happen.)
				*/
				d->special = s->special;
				memset(d->value,' ',(int)(d->len));
				memcpy(d->value,s->value,(int)MIN(d->len,s->len));
				rc = 0;						/* Something has been merged			*/
				item_merged = 1;
				break;
			}
		}

		/*
		**	If item was not merged then add it to a temp fmtlist which will be appended to the dest fmtlist.
		*/
		if (!item_merged)
		{
			if (!append_list) 
			{
				p = append_list = (KEYW *)calloc(1,sizeof(KEYW));
			}
			else
			{
				p->next = (KEYW *)calloc(1,sizeof(KEYW));
				p = p->next;
			}
			p->next = NULL;

			memcpy(p->keyword,s->keyword,8);				/* copy the keyword in (incl. spaces) */
			p->len = s->len;						/* set len */
			p->value = (char *)calloc((int)p->len+1,(int)sizeof(char));	/* grab space */
			memcpy(p->value,s->value,(int)p->len);				/* copy len bytes */
			p->special = s->special;
		}
	}

	/*
	**	If there is anything in the append list then append it to the end of dest.
	*/
	if (append_list)
	{
		rc = 0;

		if (!*dest_ptr)
		{
			*dest_ptr = append_list;
		}
		else
		{
			/*
			**	Find the end of dest list and point it at the append_list;
			*/
			for(d = *dest_ptr; d->next; d = d->next) {}
			d->next = append_list;
		}
	}

	return(rc);
}

/*
**	Routine:	free_fmtlist()
**
**	Function:	To free a fmtlist
**
**	Description:	This routine will free a fmtlist.
**			Free the value for each node then free the node itself.
**
**	Arguments:
**	fmtlist		The fmtlist to free.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	08/26/92	Written by GSL
**
*/

int free_fmtlist(fmtlist)
KEYW	*fmtlist;
{
	KEYW	*p;

	while(fmtlist)
	{
		p = fmtlist;
		fmtlist = p->next;

		free(p->value);
		free(p);
	}
	return(0);
}

/*
**	Routine:	block_fmtlist()
**
**	Function:	To unload the fmtlist into a receiver block.
**
**	Description:	This routine writes the fmtlist into the receiver block in a repeating sequence of:
**			1-8	KEYWORD
**			9-12	LENGTH
**			13-end	VALUE
**
**	Arguments:
**	fmtlist		The fmtlist to unload
**	receiver	The receiver block
**	rlen		The length of the receiver. This is updated to be the actual length used.
**	tlen		The total length that would be needed.
**
**	Globals:	None
**
**	Return:		0
**
**	Warnings:	None
**
**	History:	
**	08/26/92	Written by GSL
**
*/
static int block_fmtlist(fmtlist,receiver,rlen,tlen)
KEYW	*fmtlist;
char	*receiver;
int4	*rlen;
int4	*tlen;
{
	KEYW	*p;
	char	*ptr;
	int4	used;

	used = 0;
	*tlen = 0;
	ptr = receiver;

	for(p=fmtlist; p; p=p->next)						/* Loop thru the whole fmtlist			*/
	{
		int	size;
		int4	ll;

		if (SPECIAL_NOT != p->special)
		{
			/*
			**	If SPECIAL then keyword has not yet been resolved so skip it.
			**	(This should never happen.)
			*/
			continue;
		}

		size = 12 + p->len;						/* Calc total size needed			*/
		*tlen += size;							/* Maintain total size needed			*/

		if (*rlen >= size)						/* If room left in receiver then unload		*/
		{
			memcpy(ptr,p->keyword,8);				/* 1-8 		KEYWORD				*/
			ptr += 8;
			ll = p->len;	
			wswap(&ll);						/* Swap the length field for output		*/
			memcpy(ptr,&ll,4);					/* 9-12		LENGTH				*/
			ptr += 4;
			memcpy(ptr,p->value,(int)(p->len));			/* 13-end	VALUE				*/
			ptr += p->len;
			used += size;
			*rlen -= size;						/* decrement remaining receiver size		*/
		}
	}

	*rlen = used;								/* Set rlen to actual amount used		*/
	return(0);
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

	if ( 0==kw_num )								/* if scan by KEY match			*/
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

int search_parm_area(dest,key,unused,maxlen,parm_a,kw_num)
char 	*dest,								/* Destination to load value found.			*/
	*key, 								/* Keyword to search for (if kw_num==0)			*/
	*unused, 							/* UNUSED - old arg no longer used			*/
	*parm_a;							/* Parm area ptr (SHMH *)				*/
int4 	maxlen;								/* Maxlen of dest					*/
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


