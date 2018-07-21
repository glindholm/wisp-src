/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
*/

/*
**	File:		putparm.c
**
**	Project:	wisp/lib
**
**	Purpose:	This routine emmulates the WANG VSSUB  PUTPARM.
**
**	Routines:	
*/

/*
**	Includes
*/

#ifdef unix 
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdarg.h>									/* This routine uses variable args.	*/

#include "idsistd.h"
#include "werrlog.h"
#include "sharemem.h"
#include "putparm.h"
#include "wdefines.h"
#include "wglobals.h"
#include "wisplib.h"
#include "idsisubs.h"
#include "wmalloc.h"

/*
**	Structures and Defines
*/

#define PRNAME_SIZE	8
#define LABEL_SIZE	8
#define KEYWORD_SIZE	8

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

static int      test_next_arg();
static int4 	write_putparm(const char *function, int4 ucount, char *prname, FMTLIST *fmtlist, char pfkey, 
				char *pp_label, char *pp_reflbl, char cleanup);
static int 	block_fmtlist(FMTLIST *fmtlist, char *receiver, int4 *rlen, int4 *tlen);

/*
**	PUTPARM('D/E', [Usage_count], Prname,  Keyword_count, {Keyword,Value,Length}, PFkey, PPlabel, Reflabel, Cleanup, RC)
**	PUTPARM('M',                  PPlabel, Keyword_count, {Keyword,Recvr,Length}, PFkey,                    Cleanup, RC)
**	PUTPARM('R',                  PPlabel, Reciever, RecvLength, Totlength,       PFkey,                    Cleanup, RC)
**	PUTPARM('C',                 [PPlabel], RC)
*/
int PUTPARM(const char* pp_function, ...)
{
	va_list the_args;								/* A pointer to traverse the stack.	*/
	int	arg_count;								/* Called with this many arguments.	*/
	int4	usage_count, keyword_count;
	int4	*int4_ptr;
	int4	*return_code;
	char	*prname, *pplabel_arg, *byte_pointer;
	char 	pp_label[LABEL_SIZE + 1], pp_reflbl[LABEL_SIZE + 1];
	int	rc_fl, idx;
	int4	ret;
	int4	process_ref;
	FMTLIST	*fmtlist;
	char	*ptr;
	char	*r_receiver;
	int4	*rlptr, rcvr_len, *tlptr, tot_len;
	char	*pfkey_rcvr, assign_pfk, cleanup_opt;
	char	**m_kwdata;

	va_start(the_args, pp_function); 
	arg_count = WL_va_count();
	WL_wtrace("PUTPARM","ENTRY","Function=[%c] args=%d", *pp_function, arg_count);

	return_code = (int4 *)NULL;							/* initialize ret code pointer		*/
	rc_fl = FALSE;									/* Init flag, if have return code addrs.*/
	strcpy(pp_label, "        ");							/* Init label.				*/
	strcpy(pp_reflbl,"        ");							/* Init label.				*/

	process_ref = FALSE;
                                                                                                                                  
	arg_count -= 1;	/* Count arg1 pp_function */

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
			werrlog(WERRCODE(48018),*pp_function,0,0,0,0,0,0,0);		/* Function not implemented.		*/
			return(0);
		}
	}										/* Test for USAGE COUNT or PRNAME.	*/
											/* If C,R,M test for PUTPARM label.	*/
	int4_ptr     = va_arg(the_args, int4*);						/* Address of the usage count, maybe.	*/
	arg_count -= 1;									/* One less argument.			*/
	byte_pointer = (char *) int4_ptr;

	usage_count = 1;								/* Default to this value.		*/
	if (('D' == *pp_function || 'E' == *pp_function) &&
	    WL_longargtest(byte_pointer, 2) 			)			/* Did they specify the usage count ?	*/
	{
		usage_count = WL_get_swap(int4_ptr);

		prname = va_arg(the_args, char*);					/* Address of the prname.		*/
		arg_count -= 1;								/* One less argument.			*/
	}
	else
	{                                                               	        /* Nope.  First byte is not a null.	*/
		prname = (char *) int4_ptr;						/* This argument is the prname.		*/
		pplabel_arg = prname;							/* pplabel for "R", "M", and "C"	*/
	}


	/*
	**	Clean Up (Function C)
	*/
	if ('C' == *pp_function)							/* Delete and set return code.		*/
	{										/* PUTPARM label is in prname for 'C'.	*/
		if ( 1 == arg_count )
		{
			int4_ptr = va_arg(the_args,int4*);				/* Get the PUTPARM return code address.	*/
			return_code = int4_ptr;						/* Save the address of the return code.	*/
		}
		else
		{
			return_code = int4_ptr;
			pplabel_arg = "        ";					/* Trigger an ERASE-ALL			*/
		}

		if (pplabel_arg && ' ' != pplabel_arg[0])				/* If a label was supplied then		*/
		{
			SHMH	*prb;
			prb = WL_get_prb_area(NULL,pplabel_arg,OK_USED);			/* find the PRB				*/
			if (prb)
			{
				ret = (int4)WL_erase_prb(prb);				/* erase the prb			*/
			}
			else
			{
				ret = 4L;						/* PRB was not found			*/
			}
		}
		else	/* Erase all PRB's at this level */
		{
			ret = (int4)WL_erase_prb_level();					/* Erase all PRB's at this level	*/
		}

		WL_put_swap( return_code, ret );
		WL_cleanup_shrfil();
		return(0);								/* Done with function 'C'.		*/
	}


	ret = 0;									/* Continue processing.			*/

	keyword_count = 0;								/* Initialize keyword count		*/
	int4_ptr = va_arg(the_args, int4*);						/* Addr. of the no. of keywords, maybe.	*/
	arg_count -= 1;									/* One less argument.			*/
	if (test_next_arg(the_args,arg_count))						/* Are there any more arguments ?	*/
	{										/* Yes, so test which function.		*/
/*
**	PUTPARM('D/E', [Usage_count], Prname,  ^ Keyword_count, {Keyword,Value,Length}, PFkey, PPlabel, Reflabel, Cleanup, RC)
**	PUTPARM('M',                  PPlabel, ^ Keyword_count, {Keyword,Recvr,Length}, PFkey,                    Cleanup, RC)
**	PUTPARM('R',                  PPlabel, ^ Receiver, RecvLength, Totlength,       PFkey,                    Cleanup, RC)
*/
		if ('R' == *pp_function)						/* Is 'R' so is the receiver and no	*/
		{									/*  keywords to be processed,		*/
			r_receiver  = (char *)int4_ptr;					/* Set local copy of this value.	*/

			rlptr = va_arg(the_args, int4*);				/* Addr. of the receiver length.	*/
			rcvr_len = WL_get_swap(rlptr);

			int4_ptr = va_arg(the_args, int4*);				/* Addr. of the total length, maybe.	*/
			arg_count -= 1;							/* One less argument.			*/
			if (test_next_arg(the_args,arg_count))				/* Are there any more arguments ?	*/
			{								/* Yes, so test which function.		*/
				tlptr = int4_ptr;
				tot_len = WL_get_swap(tlptr);
			}
			else								/* Nope.  This is the return code.	*/
			{
				tlptr = NULL;
				return_code = int4_ptr;					/* Save the address of the return code.	*/
				rc_fl = TRUE;
			}
		} 									/* else is the keyword count.		*/
		else 
		{
			keyword_count = WL_get_swap(int4_ptr);				/* Set local copy of this value.	*/
		}
	}
	else
	{										/* Nope.  This is the return code.	*/
		return_code = int4_ptr;							/* Save the address of the return code.	*/
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
		static FMTLIST	*p;	/* P must retain it's value from last iteration of this loop */
		char	*nameptr, *valptr;
		int4	*lenptr, len;

		/*
		**	This loop is only done for D,E, and M that have a keyword_count.
		**
		**	Build a linked list of the keywords.
		**
		**	fmtlist -> FMTLIST -> FMTLIST -> FMTLIST ->(NULL)
		*/
		if (!fmtlist) 
		{
			p = fmtlist = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
			if ('M' == *pp_function) m_kwdata = (char **)wisp_calloc((size_t)keyword_count,sizeof(char *));
		}
		else
		{
			p->next = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
			p = p->next;
		}
		p->next = NULL;

		nameptr	= va_arg(the_args,char*);
		valptr	= va_arg(the_args,char*);
		if ('M' == *pp_function) m_kwdata[idx] = valptr;			/* Save ptr to keyword data receiver.	*/
		lenptr	= va_arg(the_args,int4*);
		arg_count -= 3;
		len = WL_get_swap(lenptr);
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
		cstr2cobx(p->keyword,nameptr,KEYWORD_SIZE);				/* copy the keyword in (incl. spaces) 	*/

		/*
		**	Special case for FILE, LIBRARY, VOLUME to ensure that if backwards referenced the fields are big enough.
		**	len will be set to the parameter length, p->len will be set to the adjusted length.
		*/
		if      (0==memcmp(p->keyword,"FILE    ",KEYWORD_SIZE)) { p->len = MAX(p->len,8); }
		else if (0==memcmp(p->keyword,"LIBRARY ",KEYWORD_SIZE)) { p->len = MAX(p->len,8); }
		else if (0==memcmp(p->keyword,"VOLUME  ",KEYWORD_SIZE)) { p->len = MAX(p->len,6); }

		p->value = (char *)wisp_calloc((int)(p->len+1),(int)sizeof(char));		/* grab space 				*/
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
			memcpy(pp_label,ptr,LABEL_SIZE);
			pp_label[LABEL_SIZE] = '\0';				       	/* Null terminate the string.		*/
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
				memcpy(pp_reflbl,ptr,LABEL_SIZE); 			/* Set up kws & vals from reference.	*/
				pp_reflbl[LABEL_SIZE] = '\0';				/* null terminate the string.		*/
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
		FMTLIST	*ref_fmtlist;
		SHMH	*parm_area;

		/*
		**	This section handles both 'R' and 'M' requests.
		*/
		parm_area = WL_get_prb_area(NULL,pplabel_arg,OK_USED);		/* Search for label match.			*/
		if (parm_area)							/* If reference label found.			*/
		{								
			ret = WL_load_fmtlist(parm_area,&ref_fmtlist);		/* Load the ref PRB into a fmtlist		*/
			if (0==ret && 'M' == *pp_function)
			{
				/*
				**	This handles the 'M' (multiple) request.  Each matching keyword is updated.
				**	If no fmtlist then only getting the pfkey.
				*/
				if (ref_fmtlist && fmtlist)			/* If we have both fmtlists then merge them	*/
				{
					FMTLIST	*p;
					int	ii;

					WL_mergex_fmtlist(ref_fmtlist,fmtlist);	/* Merge the ref into the dest fmtlist		*/
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
				WL_put_swap( rlptr, rcvr_len );			/* Return # bytes used by receiver.		*/
				if (tlptr)
				{
					WL_put_swap(tlptr, tot_len);		/* Total bytes needed				*/
				}
			}

			WL_free_fmtlist(ref_fmtlist);				/* Free the reference fmtlist			*/

			if (0==ret)
			{
				*pfkey_rcvr = parm_area->pfkey;			/* Get aid char from referenced PUTPARM.	*/

				if ('C' == cleanup_opt || 'c' == cleanup_opt)	/* Cleanup the referenced PUTPARM.		*/
				{
					WL_erase_prb(parm_area);
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
			SHMH	*ref_prb;

			ref_prb = WL_get_prb_area(NULL,pp_reflbl,OK_USED);			/* Search for label match.		*/
			if (ref_prb)							/* If reference label found.		*/
			{
				/*
				**	Ensure that the reference PRB exists.
				**	If there is an fmtlist then do the backwards reference now, otherwise it
				**	will be done when the GETPARM occurs.
				*/
				if (fmtlist)
				{
					FMTLIST	*ref_fmtlist;
					ret = WL_load_fmtlist(ref_prb,&ref_fmtlist);	/* Load the reference fmtlist		*/

					if (0==ret && ref_fmtlist)			/* If OK then merge the fmtlists	*/
					{
						WL_mergex_fmtlist(ref_fmtlist,fmtlist);
					}
					WL_free_fmtlist(ref_fmtlist);

					if ('C' == cleanup_opt || 'c' == cleanup_opt)	/* Cleanup the referenced PUTPARM.	*/
					{
						WL_erase_prb(ref_prb);
					}
					memset(pp_reflbl,' ',LABEL_SIZE);		/* Clear the reflbl - it's been done	*/
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

	WL_free_fmtlist(fmtlist);
	WL_cleanup_shrfil();								/* Cleanup shared memory		*/

	WL_put_swap(return_code, ret);
	return(0);
}                                                                                                                                 

/*
**	Remove original test in test_next_arg;  under Watcom, this test DOES 
**	increment the ptr to the list.
*/
static int test_next_arg(args,cnt)							/* Return TRUE if there is another arg.	*/
va_list args;										/* on the stack.			*/
int cnt;										/* NOTE: This does not adjust the ptr	*/
{											/*       to the arguments above!	*/

	if (cnt > 0)
		return (TRUE);
	else
		return (FALSE);
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

static int4 write_putparm(const char *function, int4 ucount, char *prname, FMTLIST *fmtlist, char pfkey, 
			char *pp_label, char *pp_reflbl, char cleanup)
{
	int 	mem_needed;
	int	keyword_count;
	char	*shmaddr;
	int	id,size;
	SHMH 	*pputparm;

	wtrace("PUTPARM", "WRITE", "fcn=%c prname=%8.8s", *function, prname);

	if (!prname || ' ' == *prname || 0 == *prname)					/* Didn't supply a prname!		*/
	{
		return(20L);								/* Does NOT generate a PUTPARM!!!	*/
	}

#ifdef OLD
	There can be multiple putparms with the same LABEL at different link-levels.

	if ( pp_label[0] && pp_label[0] != ' ' )					/* If Labeled then delete previous	*/
	{										/* (Only 1 PRB with a given label.)	*/
		WL_erase_prb(WL_get_prb_area(NULL,pp_label,OK_USED));			/* Get & erase the labeled PRB		*/
	}
#endif
	if ( pp_label[0] && pp_label[0] != ' ' )
	{
		/*
		**	Only one PRB with a given label allowed at a given level.
		*/
		WL_erase_prb_label_level(pp_label);
	}

	WL_size_fmtlist(fmtlist,&keyword_count,&mem_needed);				/* Get size values for fmtlist		*/

	shmaddr = WL_get_sh_seg(prname,pp_label,mem_needed,&id,&size);			/* get the memory if first write.	*/
	if (!shmaddr)									/* Some kind of error when trying to	*/
	{										/* the shared memory address.		*/
		return(12L);
	}
	pputparm = (SHMH *) shmaddr;							/* cast our header struct onto it 	*/
	pputparm->prb_id 	= id;
	pputparm->prb_size 	= size;
	pputparm->type 		= *function;						/* insert the function type  		*/
	pputparm->usage_cnt 	= ucount;						/* and usage count			*/
	memcpy(pputparm->prname,prname,PRNAME_SIZE);					/* and prname				*/
	pputparm->keyw_cnt 	= keyword_count;					/* and keyword count 			*/
	pputparm->status 	= P_OK;

	WL_write_fmtlist(pputparm,fmtlist);

	pputparm->pfkey = pfkey;
	if (strcmp(pp_label,"        ")) memcpy(pputparm->label,pp_label,LABEL_SIZE);	/* Copy the PUTPARM label to structure	*/
	else				 memset(pputparm->label,0,LABEL_SIZE);		/*  else set it to NULL.		*/
	if (strcmp(pp_reflbl,"        ")) memcpy(pputparm->reflbl,pp_reflbl,LABEL_SIZE);/* Copy reference label to structure.	*/
	else				  memset(pputparm->reflbl,0,LABEL_SIZE);	/*  else set it to NULL.		*/
	pputparm->cleanup = cleanup;

	WL_finish_sh_seg(size);								/* Update counters			*/

	return(0L);
}

/*
**	Routine:	WL_write_fmtlist()
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
int WL_write_fmtlist(SHMH *prb, FMTLIST *fmtlist)
{
	char	*dest;
	FMTLIST	*p;
	int	offs;

	dest = (char *)prb;
	dest += sizeof(SHMH);							/* Point to after the PRB header		*/

	for (p=fmtlist; p; p=p->next)						/* Loop thru and write each keywshm item	*/
	{	
		offs = WL_load_keywshm((KEYWSHM *)dest,p);
		dest += offs;
	}
	return(0);
}

/*
**	Routine:	WL_size_fmtlist()
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
int WL_size_fmtlist(FMTLIST *fmtlist, int *cnt, int *mem)
{
	FMTLIST	*p;

	*cnt = 0;
	*mem = sizeof(SHMH);								/* PRB header size			*/
	for(p=fmtlist; p; p=p->next)
	{
		*cnt += 1;
		/* Each keyword takes: size of KEYWSHM struct + 8 for keyword + length of value + null terminator		*/
		*mem += sizeof(KEYWSHM) + KEYWORD_SIZE + p->len + 1;
	}
	return(0);
}

/*
**	Routine:	WL_load_fmtlist()
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

int WL_load_fmtlist(SHMH *parm_area, FMTLIST **fmtlist_ptr)
{
	int	i;
	int	ret;

	ret = 0;									/* Assume success.			*/

	*fmtlist_ptr = NULL;

	for (i = 1; i <= parm_area->keyw_cnt; i++)
	{
		FMTLIST	*p;
		KEYWSHM	*prb_keystruct;						/* Pointer to keyword struct in PRB		*/
		char	*prb_keyword;						/* Pointer to actual keyword in PRB		*/
		int2	len;

		prb_keystruct = WL_find_prb_keyword(parm_area,NULL,i);		/* Get pointer to keyword struct		*/

		if (!prb_keystruct)
		{
			ret = 12;
			break;
		}

		if (!*fmtlist_ptr) 
		{
			p = *fmtlist_ptr = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
		}
		else
		{
			p->next = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
			p = p->next;
		}
		p->next = NULL;

		prb_keyword = (char *)prb_keystruct + sizeof(KEYWSHM);		/* Get ptr to keyword in PRB			*/
		memcpy(p->keyword,prb_keyword,KEYWORD_SIZE);			/* copy the keyword in (incl. spaces) */

		len = WL_a_int2(&prb_keystruct->value_len);			/* Load the value into dest.			*/
		p->len = (int4)len;						/* set len */

		p->value = (char *)wisp_calloc((int)len+1,(int)sizeof(char));	/* grab space */
		memcpy(p->value,prb_keyword+KEYWORD_SIZE,(int)len);		/* copy len bytes */
		p->special = prb_keystruct->special;
	}


	return(ret);
}

/*
**	Routine:	WL_mergex_fmtlist()
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

int WL_mergex_fmtlist(FMTLIST *src, FMTLIST *dest)
{
	FMTLIST	*s, *d;
	int	rc;

	rc = 1;									/* Nothing has been merged			*/
	if (!src || !dest) return(rc);

	for(d=dest; d; d=d->next)
	{
		for(s=src; s; s=s->next)
		{
			if (0==memcmp(d->keyword,s->keyword,KEYWORD_SIZE) && SPECIAL_NOT == s->special)
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
**	Routine:	WL_merge_fmtlist()
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

int WL_merge_fmtlist(FMTLIST *src, FMTLIST **dest_ptr)
{
	FMTLIST	*s, *d, *append_list, *p;
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
			if (0==memcmp(d->keyword,s->keyword,KEYWORD_SIZE))
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
				p = append_list = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
			}
			else
			{
				p->next = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
				p = p->next;
			}
			p->next = NULL;

			memcpy(p->keyword,s->keyword,KEYWORD_SIZE);			/* copy the keyword in (incl. spaces) */
			p->len = s->len;						/* set len */
			p->value = (char *)wisp_calloc((int)p->len+1,(int)sizeof(char));	/* grab space */
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
**	Routine:	WL_free_fmtlist()
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

int WL_free_fmtlist(FMTLIST *fmtlist)
{
	FMTLIST	*p;

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
static int block_fmtlist(FMTLIST *fmtlist, char *receiver, int4 *rlen, int4 *tlen)
{
	FMTLIST	*p;
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
			memcpy(ptr,p->keyword,KEYWORD_SIZE);			/* 1-8 		KEYWORD				*/
			ptr += KEYWORD_SIZE;
			ll = p->len;	
			WL_wswap(&ll);						/* Swap the length field for output		*/
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
	WL_find_prb_keyword	Return a pointer to the keyword in a PRB.

		key; 		Keyword to search for (if kw_num==0)
		parm_a;		Parm area ptr (SHMH *)
		kw_num;		Relative keyword to find (if kw_num != 0)
*/

KEYWSHM *WL_find_prb_keyword(SHMH *parm_a, char *key, int kw_num)
{
	char 	l_keyword[KEYWORD_SIZE+1], 					/* Local keyword (Null terminated)		*/
		d_keyword[KEYWORD_SIZE+1];					/* Keyword from PRB (Null terminated)		*/
	char 	*data; 								/* Ptr to keyword in PRB			*/
	char 	*str,								/* Ptr for going thru PRB			*/
		*p; 								/* Temp ptr					*/
	int 	kw_cnt; 							/* Which keyword are we looking at in the loop	*/
	int2 	offs,								/* Offset to next keyword in PRB		*/
		cnt;								/* Number of keywords in PRB 			*/
	KEYWSHM	*ptr_shm;							/* Pointer to keyword in PRB			*/

	if ( 0==kw_num )							/* if scan by KEY match				*/
	{
		if (!key) return(0);						/* no key passed				*/

		memcpy(l_keyword,key,KEYWORD_SIZE);				/* Copy key into local variable.		*/
		l_keyword[KEYWORD_SIZE] = '\0';	
		p = strchr(l_keyword,' ');					/* Find first occurance of space & null		*/
		if (p) *p=(char)0;						/* terminate the string.			*/
	}

	cnt = WL_a_int2(&parm_a->keyw_cnt);					/* Get the number of keywords in PRB		*/

	str = (char *)parm_a;
	str += sizeof(SHMH);							/* Point to first keyword			*/
	for (kw_cnt=1; kw_cnt<=cnt; kw_cnt++, str = str+offs) 
	{
		ptr_shm = (KEYWSHM *)str;
		data = (char *)ptr_shm + sizeof(KEYWSHM);				/* get ptr to keyword in PRB		*/

		memcpy(d_keyword,data,KEYWORD_SIZE);					/* Load & null terminate the keyword	*/
		d_keyword[KEYWORD_SIZE] = '\0';
		p=strchr(d_keyword,' ');
		if (p) *p=(char)0;

		if ( kw_num == kw_cnt || 						/* If requested keyword # or		*/
		    (kw_num == 0 && !strncmp(d_keyword,l_keyword,KEYWORD_SIZE)))	/* a keyword match.			*/
		{
			return( ptr_shm );						/* Return the ptr			*/
		}
	        offs = WL_a_int2(&ptr_shm->next_offs);				/* Get offset to next keyword		*/
	}
	return( 0 );
}


/*
	WL_search_parm_area	Search a given PRB for a keyword (or by number -- kw_num) and return the value in dest.
				If keyword found, dest is set to spaces (for maxlen) then loaded with the value found.
				Returns 0 if not found.  If kw_num != 0 then it also loads the keyword into key.

		dest,		Destination to load value found.
		key, 		Keyword to search for (if kw_num==0)
		parm_a;		Parm area ptr (SHMH *)
		maxlen;		Maxlen of dest

*/

int WL_search_parm_area(char *dest, char *key, int4 maxlen, SHMH *parm_a)
{
	KEYWSHM	*prb_keystruct;						/* Pointer to keyword struct in PRB			*/
	char	*prb_keyword;						/* Pointer to actual keyword in PRB			*/
	int2	len;

	prb_keystruct = WL_find_prb_keyword(parm_a,key,0);

	if (!prb_keystruct)						/* Not found						*/
	{
		return(0);
	}

	prb_keyword = (char *)prb_keystruct + sizeof(KEYWSHM);		/* Get ptr to keyword in PRB				*/

	len = WL_a_int2(&prb_keystruct->value_len);			/* Load the value into dest.		*/
	memset(dest,' ',(int)maxlen);
	strncpy(dest,prb_keyword+KEYWORD_SIZE, ((len > (int)maxlen) ? (int)maxlen : len));

	return(1);
}

/*
**	History:
**	$Log: putparm.c,v $
**	Revision 1.25  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.24  2003/01/29 21:08:11  gsl
**	Change PUTPARM to use stdarg.h
**	
**	Revision 1.23  2002/12/10 20:54:12  gsl
**	use WERRCODE()
**	
**	Revision 1.22  2002/12/09 21:09:30  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.21  2002/07/16 16:24:54  gsl
**	Globals
**	
**	Revision 1.20  2002/07/12 17:00:59  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.19  2002/07/11 20:29:11  gsl
**	Fix WL_ globals
**	
**	Revision 1.18  2002/07/10 21:05:22  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.17  2002/07/09 04:13:59  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.16  2002/07/02 21:15:27  gsl
**	Rename wstrdup
**	
**	Revision 1.15  2002/06/21 03:10:39  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.14  1998/08/03 21:09:20  jlima
**	Support Logical Volume Translation to long file names containing eventual embedded blanks.
**	
**	Revision 1.13  1998-07-10 11:15:43-04  gsl
**	Make a local variable "p" static so it retains it's value between
**	iterations of the for loop.
**
**	Revision 1.12  1997-04-15 23:09:26-04  gsl
**	Update to use wtrace() plus remove a lot of unneeded entry logging
**
**	Revision 1.11  1996-07-17 17:51:56-04  gsl
**	change to use wcalloc()
**
**	Revision 1.10  1996-07-08 16:35:29-07  gsl
**	fix for NT
**
**	Revision 1.9  1995-05-15 06:08:26-07  gsl
**	Added call to WL_erase_prb_label_level() to erase an existing putparm
**	with the same label at the same linklevel.
**	Replaced numeric literals with meaningful defines.
**	Added trace calls to werrlog() for entry into all the routines.
**
**
**
*/
