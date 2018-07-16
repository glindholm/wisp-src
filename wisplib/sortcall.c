			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/********************************************************************************************************************************
*																*
*	SORTCALL.C  Emulation of WANG VSSUB:  SORTCALL										*
*																*
********************************************************************************************************************************/
/********************************************************************************************************************************
*																*
*	Revision History:                           										*
*																*
*	R01 - 10/30/90  DM  Fix isempty call to sys$get was returning 0x186F4 RMS$_USZ						*
*			    Also, rewrite cnvpicz to allow for both right and left justification				*
*	R02 - 11/02/90	DM  Fix call to wfname so that "##name" will work correctly						*
*																*
*																*
*																*
********************************************************************************************************************************/



#ifdef VMS
#include <varargs.h>                                                                    /* Allow variable number of arguments	*/
#include <ctype.h>									/* for isdigit()			*/
#include <descrip.h>
#include <rms.h>
#include <ssdef.h>
#include "wcommon.h"

#define MAX_REC_SZ 8192									/*R01* used in isempty as default size	*/
											/*R01* for a file record.		*/

struct key_block {									/* VMS key block structure 		*/
	short type;									/* type of key (char/decimal/etc)	*/
	short order;									/* 1-descending, 0-ascending		*/
	short offset;									/* offset from start of file record 	*/
	short length;									/* length in bytes			*/
	};
struct key_struct {									/* passed to SOR$BEGIN_SORT		*/
	short cnt;									/* number of keys to sort on		*/
	struct key_block keys[8];							/* actual key info (above)		*/
	};

#define SOR$M_NOSIGNAL 8								/* option for SOR$ sort routine		*/
											/* to return err code instead of 	*/
											/* signaling it				*/
struct k_inf {										/* key info as passed from cobol 	*/
	char field_pos[4];								/* PIC Z(4) key offset 			*/
	char flen[3];									/* PIC Z(3) key len			*/
	char ftype;									/* PIC Z type 'C'har, 'P'acked Decimal	*/
	char sort_order;								/* PIC Z order 'A'sc, 'D'escending	*/
	};

struct info_s {										/* info struct passed from cobol	*/
	char ifile[8];									/* input file name 			*/
	char ilib[8];									/* input file library			*/
	char ivol[6];									/* input file volume			*/
	char ofile[8];									/* output name				*/
	char olib[8];									/* output library			*/
	char ovol[6];									/* output volume			*/
	struct k_inf key_info[8];							/* key data (above)			*/
	};

struct info_s *sort_info;								/* working pointer to passed info	*/

SORTCALL(va_alist)

va_dcl
{
	va_list	the_args;								/* A pointer to traverse the stack.	*/
	int 	arg_count;								/* passed arg count			*/
	int 	*ret_status;								/* passed ret status			*/
	char 	*passed_info;								/* arg1 from cobol			*/
	char 	invol[7],infil[9],inlib[9];						/* input file info buffers 		*/
	char	outvol[7],outfil[9],outlib[9];						/* output file info buffers		*/
	char 	infile[100], outfile[100], *wfname();					/* pathname buffers			*/
	char	*p;									/* scratch pointer			*/
	long	mode, status;								/* mode for wfname(), status from SOR$	*/
	extern char WISPFILEXT[39];							/* foo					*/

	struct key_struct sor_key_info;							/* key struct to pass to SOR$		*/
	struct key_block *sor_key_p;							/* working pointer for ^^^		*/
	struct k_inf *pas_key_p;							/* working pointer for passed key data	*/
	short 	cnvpicz();								/*R01 convert PIC Z type into short	*/
	long 	opt = SOR$M_NOSIGNAL;							/* option word for SOR$BEGIN_SORT()	*/
	
	$DESCRIPTOR(inp_desc,infile);
	$DESCRIPTOR(out_desc,outfile);
	

	/********************************************************
	*	Receive variable number of arguments		*
	********************************************************/

	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/

	va_start(the_args);								/* Go back to the top of the stack.	*/

	passed_info  = va_arg(the_args, char*);						/* Get address of the information 	*/
	arg_count -= 1;									/* One less argument.			*/

	ret_status = va_arg(the_args, int*);						/* return code 				*/
	arg_count -= 1;									/* One less argument.			*/

	sort_info = (struct info_s *)passed_info;					/* so we can access this block of 	*/
											/* mem as a struct info_s		*/
	memset(infile,(char)0,sizeof(infile));						/* null these buffers 			*/
	memset(outfile,(char)0,sizeof(outfile));

	/*
	memset(invol,' ',sizeof(invol));
	memset(inlib,' ',sizeof(inlib));
	memset(infil,' ',sizeof(infil));
	memset(outvol,' ',sizeof(outvol));
	memset(outlib,' ',sizeof(outlib));
	memset(outfil,' ',sizeof(outfil));
	strncpy(invol,sort_info->ivol,6);
	strncpy(inlib,sort_info->ilib,8);
	strncpy(infil,sort_info->ifile,8);
	strncpy(outvol,sort_info->ovol,6);
	strncpy(outlib,sort_info->olib,8);
	strncpy(outfil,sort_info->ofile,8);
	*/
	
	mode = 0;
	mode |= IS_BACKFILL;
	p=wfname(&mode,sort_info->ivol,sort_info->ilib,sort_info->ifile,infile);	/* convert wang style to vax names	*/

	mode = 0;									/*R02 reset mode for 2nd wfname call    */
	mode |= IS_BACKFILL;                                                            /*R02 return new vol, lib, file names	*/
	mode |= IS_OUTPUT;                                                              /*R02 file is an output file        	*/
	p=wfname(&mode,sort_info->ovol,sort_info->olib,sort_info->ofile,outfile);	/* convert wang style to vax names	*/

	/********************************************************
	*	Initialize SOR$ parameters			*
	********************************************************/

	*ret_status = 0;

	if (isempty(infile))
	{
		*ret_status=4;

		wswap(ret_status);
		return;
	}

	inp_desc.dsc$w_length = strlen(infile);						/* set descriptor length info		*/
	inp_desc.dsc$w_length = strlen(outfile);

	status = SOR$PASS_FILES(&inp_desc,&out_desc);					/* tell SOR$ our in and out files	*/

	memset(&sor_key_info,(char)0,sizeof(struct key_struct));			/* zero our info structure		*/
	sor_key_p = sor_key_info.keys;							/* init working pointers		*/
	pas_key_p = sort_info->key_info;						
	while (strncmp((char *)pas_key_p,"       ",7)&&sor_key_info.cnt<8)
											/* spaces mean no more keys		*/
	{
		int tmp;

		switch (pas_key_p->ftype)						/* convert type byte			*/
		{
			case 'C':	sor_key_p->type = DSC$K_DTYPE_T; 		/* char key data			*/
					break;
			case 'P':	sor_key_p->type = DSC$K_DTYPE_P;		/* packed decimal data			*/
					break;
			case 'Z':	sor_key_p->type = DSC$K_DTYPE_NZ;		/* zoned decimal data			*/
					break;                  
			default:	vre("%SRTC-FLD-NOIMP, field type <%c> not implemented, field %d",
						pas_key_p->ftype,sor_key_info.cnt+1);
					pas_key_p++; sor_key_info.cnt++;
					continue;					/* else ignore this key (not implementd	*/
		}
		sor_key_p->offset = cnvpicz(pas_key_p->field_pos,4)-1;			/* set key position			*/
		tmp = sor_key_p->length = cnvpicz(pas_key_p->flen,3);	   		/* and length				*/
		if (sor_key_p->type==DSC$K_DTYPE_P)
			sor_key_p->length = tmp*2 -1; 
		sor_key_p->order  = pas_key_p->sort_order=='D'?1:0;			/* convert order byte			*/
 		sor_key_info.cnt++;							/* adjust key count			*/
		sor_key_p++; pas_key_p++;						/* point to next struct in array	*/
	}	

	status = SOR$BEGIN_SORT(&sor_key_info,0,&opt);	      				/* pass this ^^^ info to SOR$		*/
                                                             

	/********************************************************
	*	Do the sorting					*
	********************************************************/

	status = SOR$SORT_MERGE();							/* Do the sort				*/
	if (status != SS$_NORMAL)
		*ret_status = 4;							/* could not open input file		*/

	status = SOR$END_SORT();							/* release resources			*/

	wswap(ret_status);
}

		/*R01	the following routine was rewritten	*/

short cnvpicz(p,digits)		/*R01*/							/* convert a PIC Z type to long		*/
char *p;										/* pointer to picture			*/
int digits;										/* number of digits			*/
{
	int digit;									/* which digit are we looking at	*/
	short val=0;									/* current value			*/

	char dig;									/* current digit			*/


	for ( digit=0 ; ( *(p+digit) == ' ' ) && ( digit < digits ) ; ++digit );	/* skip the initial spaces if any	*/

	for ( ; ( *(p+digit) != ' ' ) && ( digit < digits ) ; ++digit )			/* look at each digit until space	*/
	{
		dig = *(p+digit);							/* get the char at location 'digit'	*/
		if ( !isdigit(dig)) return -1;						/* abort if it is not a digit		*/
   
		val = (val * 10) + (dig - 0x30);					/* multiply previous value by 10 and	*/
											/* add the numeric value of dig		*/
	}

	return val;									/* return with the cumulative val	*/
}

static int isempty(file_name)
char *file_name;
{
	int ret;
	struct FAB file_block;
        struct RAB record_block;

	int rms_status;
        char *byte_buffer, *malloc();

	file_block = cc$rms_fab;
	file_block.fab$l_fna = file_name;
	file_block.fab$b_bks = 4;
	file_block.fab$l_dna = ".DAT";
	file_block.fab$b_dns = 4;             
	file_block.fab$b_fns = strlen(file_name);			/*R01*/                                          
	file_block.fab$b_fac = FAB$M_GET;                
	file_block.fab$l_fop = FAB$M_CIF;
	file_block.fab$b_shr = FAB$V_SHRPUT | FAB$V_SHRGET | FAB$V_SHRDEL | FAB$V_SHRUPD;

	record_block = cc$rms_rab;
	record_block.rab$l_fab = &file_block;
	
	rms_status = sys$open(&file_block);
	if (rms_status != RMS$_NORMAL) return 1;			/*R01*/

    	byte_buffer = malloc(MAX_REC_SZ);				/*R01*/
    	record_block.rab$l_ubf = byte_buffer;
    	record_block.rab$w_usz = MAX_REC_SZ;				/*R01*/

    	rms_status = sys$connect(&record_block);
	if (rms_status == RMS$_NORMAL)					/*R01*/
	{								/*R01*/
    		rms_status = sys$get(&record_block);

    		if (rms_status == RMS$_NORMAL)				 /* Just verify successful retrieval of RAB info.*/
    		{
			ret=0;
    		}
    		else
		{
			ret=2;
		}
    	}								/*R01*/
	else								/*R01*/
	{								/*R01*/
		ret=3;							/*R01*/
    	}								/*R01*/
	rms_status = sys$close(&file_block);
	free(byte_buffer);						/*R01*/
	return ret;
}
#endif /* VMS */

#ifndef VMS
#include "werrlog.h"

/*
	SORTCALL	For UNIX and MS-DOS this routine will call WISPSORT.  It will use the info from SORTINFO
			for the extra arguments.
*/
void SORTCALL(sortdata,retcode)
char	*sortdata;
long	*retcode;
{
#define		ROUTINE		61000
	char	filetype;
	long	recsize;
	long	*sortcode_ptr;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	getsortinfo(&filetype, &recsize, &sortcode_ptr);

	WISPSORT(sortdata,&filetype,&recsize,sortcode_ptr,retcode);
	return;
}
#endif /* NOT VMS */
