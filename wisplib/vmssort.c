static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vmssort.c
**
**	Purpose:	VMS specific emulation of the Wang SORT
**
**	Routines:	
**	vmssort()	The VMS specific sort
**
**
*/


#ifdef VMS
#include <stdio.h>
#include <varargs.h>                                                                    /* Allow variable number of arguments	*/
#include <ctype.h>									/* for isdigit()			*/
#include <descrip.h>
#include <rms.h>
#include <ssdef.h>
#include <stsdef.h>
#include <sordef.h>
#include <string.h>

#include "idsistd.h"
#include "wangkeys.h"
#include "wcommon.h"
#include "wfname.h"
#include "vmssort.h"


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
#define SOR$M_STABLE 1  								/* option for SOR$ sort routine		*/
											/* to return err code instead of 	*/
											/* signaling it				*/
struct fieldlst_s
{
	int start;
	int len;
};

static char specfilename[80];

static void vmssort_cleanup(void);
static int isempty(char *file_name);
static int doselinfo(struct select_criteria selinfo[], int4 selcnt);
static int findfield(struct fieldlst_s lst[], int pos, int len, int cnt);
static void addfield(struct fieldlst_s lst[], int pos, int len, int cnt);
static int vmssort_err(int vms_status, char *routine, int4 *wangcode, int4 *errcode, char *errmess);


/*
**	Routine:	vmssort()
**
**	Function:	The VMS specific backend to the Wang SORT utility
**
**	Description:	This is the interface to the VMS SOR$_xxx system services which will
**			perform the sort.
**
**	Arguments:
**	flist		List of input FILE names
**	llist		List of input LIBRARY names
**	vlist		List of input VOLUME names
**	infilename	List of native input filename.
**	infile_count	Count of input files ( 1 - 20 )
**	ofile		Output FILE name
**	olib		Output LIBRARY name
**	ovol		Output VOLUME name
**	outfilename	The native output filename.
**	ofileorg	Output file organization FILEORG (C, R, I)
**	maxrec		Output file maximum record size RECSIZE
**	outrectype	Output file record type RECTYPE (F, V)
**	replace_flag	Was REPLACE=YES specified.
**	selinfo		Select criteria list
**	selcnt		Select count
**	keyinfo		Keys info list
**	keycnt		Keys count
**	stable		Stable sort flag
**	wangcode	The wang return code for sort.
**				 0	Success
**				 1	Success with misc warning
**				 4	Input file was empty
**				99	Fatal misc error
**	errcode		An error code used to indicate what type of error occured.
**				 0	no error
**				 1	a recoverable error (reissue OUTPUT getparm)
**				 2	non recoverable error.
**	errmess		An 80 char error message.
**
**	Globals:
**	specfilename	The sort specification file name.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	10/03/94	Updated and documented. GSL
**
*/
void vmssort(char flist[][9], char llist[][9], char vlist[][7], char infilename[][80], int4 infile_count,
		char ofile[], char olib[], char ovol[], char *outfilename, 
		char ofileorg, int4 maxrec, char outrectype, int replace_flag,
		struct select_criteria selinfo[], int4 selcnt, struct key_info keyinfo[], int4 keycnt, int4 stable, 
		int4 *wangcode, int4 *errcode, char *errmess)
{
	char 	infile[100], outfile[100];						/* pathname buffers			*/
	int4	idx;
	int	status;									/* status from SOR$			*/
	struct key_struct sor_key_info;							/* key struct to pass to SOR$		*/
	struct key_block *sor_key_p;							/* working pointer for ^^^		*/
	unsigned char out_org,out_rfm;							/* Output file organization.		*/
	long 	begin_opt;
	char	num_work_files;
	
#include "vmssort1.d"

	*wangcode = 0;
	*errcode = 0;
	errmess[0] = (char)0;

	specfilename[0] = (char)0;

	/*
	**	Setup record SELECT options
	*/
	if (selcnt)
	{
		status = doselinfo(selinfo,selcnt);

		if (vmssort_err(status, "SOR$SPEC_FILE", wangcode, errcode, errmess))
		{
			vmssort_cleanup();
			return;
		}
	}

	/*
	**	Prepare the OUTPUT file 
	*/
	strcpy(outfile,outfilename);
	out_desc.dsc$w_length = strlen(outfile);
	switch(ofileorg)
	{
	case 'C':	out_org = FAB$C_SEQ;	break;
	case 'I':	out_org = FAB$C_IDX;	break;
	case 'R':	out_org = FAB$C_REL;	break;
	default:	out_org = FAB$C_SEQ;	break;
	}
	out_rfm = (outrectype=='F') ? FAB$C_FIX : FAB$C_VAR;

	/*
	**	Setup all the INPUT files.
	*/
	for (idx=0; idx<infile_count; ++idx)
	{
		strcpy(infile,infilename[idx]);
		inp_desc.dsc$w_length = strlen(infile);

		if (1 == infile_count && isempty(infile))
		{
			*wangcode = 4;
			if (selcnt)
			{
				vmssort_cleanup();
			}
			return;
		}

		if (0 == idx)
		{
			/*
			**	Pass the first input file and the output file.
			*/
			if (maxrec)
			{
				status = SOR$PASS_FILES(&inp_desc,&out_desc,&out_org,&out_rfm,NULL,NULL,&maxrec);
			}
			else
			{
				status = SOR$PASS_FILES(&inp_desc,&out_desc,&out_org,&out_rfm);
			}
		}
		else
		{
			/*
			**	Additional input files
			*/
			status = SOR$PASS_FILES(&inp_desc);
		}

		if (vmssort_err(status, "SOR$PASS_FILE", wangcode, errcode, errmess))
		{
			vmssort_cleanup();
			return;
		}
	}

	/*
	**	Setup the KEYS info for sorting
	*/
	memset(&sor_key_info,(char)0,sizeof(struct key_struct));			/* zero our info structure		*/
	sor_key_p = sor_key_info.keys;							/* init working pointers		*/

	for (idx=0; idx<keycnt; ++idx)
	{
		switch (keyinfo[idx].type)						/* convert type byte			*/
		{
		case 'C':
			sor_key_p->type = DSC$K_DTYPE_T; 				/* char key data			*/
			break;
		case 'P':
			sor_key_p->type = DSC$K_DTYPE_P;				/* packed decimal data			*/
			break;
		case 'Z':
			sor_key_p->type = DSC$K_DTYPE_NZ;				/* zoned decimal data			*/
			break;
		default:
/*>>>> Support more types */
			/*
			**	NOT IMPLEMENTED - ABORT
			*/
			sprintf(errmess, "SORT KEY FIELD TYPE%d=%c IS NOT IMPLEMENTED", idx+1, keyinfo[idx].type);
			*errcode = 2;
			vmssort_cleanup();
			return;
		}
		sor_key_p->offset = keyinfo[idx].spos-1;
		sor_key_p->length = keyinfo[idx].len;

		if (sor_key_p->type==DSC$K_DTYPE_P)
		{
			/*
			**	For Packed decimal the length is the number of digits.
			*/
			sor_key_p->length = (sor_key_p->length * 2) - 1;
		}
		sor_key_p->order  = (keyinfo[idx].order=='D')?1:0;			/* convert order byte			*/

 		sor_key_info.cnt++;							/* adjust key count			*/
		sor_key_p++; 
	}

	begin_opt = SOR$M_NOSIGNAL;
	if (stable)
	{
		begin_opt |= SOR$M_STABLE;
	}
	num_work_files = 4;

	/*
	**	Initialize the SORT -- open files and validate key info and options
	*/
	status = SOR$BEGIN_SORT(&sor_key_info,0,&begin_opt,0,0,0,0,&num_work_files);
	
	if (vmssort_err(status, "SOR$BEGIN_SORT", wangcode, errcode, errmess))
	{
		vmssort_cleanup();
		return;
	}
                                                             
	/*
	**	Perform the SORT
	*/
	status = SOR$SORT_MERGE();

	if (vmssort_err(status, "SOR$SORT_MERGE", wangcode, errcode, errmess))
	{
		vmssort_cleanup();
/*>>>> only if not a replace (sort in place) */
		strcat(outfile,";0");
		delete(outfile);
		return;
	}

	vmssort_cleanup();

	if (isempty(outfile))
	{
		/*
		**	The OUTPUT file is empty. 
		**	Delete it and return a warning (4).
		*/
		*wangcode = 4;
		strcat(outfile,";0");
		delete(outfile);
		return;
	}

	return;
}

static void vmssort_cleanup(void)
{
	SOR$END_SORT();	
	if (specfilename[0]) delete(specfilename);
}

static int isempty(char *file_name)
{
#define MAX_REC_SZ 8192	
	int ret;
	struct FAB file_block;
        struct RAB record_block;

	int rms_status;
        char *byte_buffer;

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

static int doselinfo(struct select_criteria selinfo[], int4 selcnt)
{
	char 	selbuffer[8192];						/* Big enough to hold 32 entries of 256		*/
	char	sizebuffer[30];
	int 	idx;
	char 	tmpbuffer[256];
	struct fieldlst_s fldlist[32];
	int 	fcnt;
	int 	status;
	int 	condidx;
	
	FILE	*specfile;
	
#include "vmssort2.d"
	selbuffer[0]='\0';

	/*
	**	Create all the fields definitions needed for the record selection
	*/
	for (fcnt=idx=0; idx<selcnt; ++idx)
	{
		char *type;
		if (0 == findfield(fldlist,selinfo[idx].fpos,selinfo[idx].flen,fcnt))
		{
			addfield(fldlist,selinfo[idx].fpos,selinfo[idx].flen,fcnt);
			++fcnt;
		}
		else continue;
		switch(selinfo[idx].ftyp)
		{
		case 'C':
			type="CHARACTER";
			sprintf(sizebuffer,"SIZE:%d,",selinfo[idx].flen);
			break;
		case 'U':
			type="BINARY,UNSIGNED";
			sprintf(sizebuffer,"SIZE:%d,",selinfo[idx].flen);
			break;
		case 'B':
			type="BINARY";
			sprintf(sizebuffer,"SIZE:%d,",selinfo[idx].flen);
			break;
		case 'D':
			type="DECIMAL";
			sprintf(sizebuffer,"DIGITS:%d,",selinfo[idx].flen);
			break;
		case 'F':
			type="D_FLOATING";
			strcpy(sizebuffer,"");
			break;
		case 'Z':
		case 'L':
			type="ZONED";
			sprintf(sizebuffer,"SIZE:%d,",selinfo[idx].flen);
			break;
		case 'P':
			type="PACKED_DECIMAL";
			sprintf(sizebuffer,"DIGITS:%d,",selinfo[idx].flen);
			break;
		}
			
		sprintf(tmpbuffer,"/FIELD=(NAME=FLD%d,POSITION:%d,%s%s)\n",fcnt,selinfo[idx].fpos,
			sizebuffer,type);
		strcat(selbuffer,tmpbuffer);
			
	}

	/*
	**	Define the conditions
	*/
	for (condidx=0, idx=0; idx<selcnt; ++condidx)
	{
		sprintf(tmpbuffer,"/COND=(NAME=COND%d,TEST=(",condidx);
		strcat(selbuffer,tmpbuffer);
		
		for (; idx<selcnt ; ++idx)
		{
/*>>>> Handle position values */
			char value[19], connect[4];
			int indx, br;

			memcpy(value,selinfo[idx].value,18);
			value[18]='\0'; 
			for (indx=17; indx >= 0 && value[indx] == ' '; --indx);
			value[++indx]='\0';

			memcpy(connect,selinfo[idx].connect,3);
			connect[3]='\0';
			for (indx=2; indx >= 0 && connect[indx] == ' '; --indx);
			connect[++indx]='\0';
			
			if (!memcmp(connect,"OR",2))
			{
				br=TRUE;
				connect[0]='\0';
			}
			else
			{
				br=FALSE;
			}

			sprintf(tmpbuffer,"FLD%d %2.2s %s %s ",findfield(fldlist,selinfo[idx].fpos,selinfo[idx].flen,fcnt),
				selinfo[idx].testrel,value,connect);
			strcat(selbuffer,tmpbuffer);
			if (br)
			{
				++idx;
				break;
			}
			strcat(selbuffer,"\n        ");
		}
		strcat(selbuffer,"))\n");
	}
	for (--condidx;condidx>=0;--condidx)
	{
		sprintf(tmpbuffer,"/INCLUDE=(CONDITION=COND%d)\n",condidx);
		strcat(selbuffer,tmpbuffer);
	}

	/*
	**	Create a temp selection options file
	*/
	sprintf(specfilename,"SYS$LOGIN:WSORT%d.SRT",getpid());	
	specfile=fopen(specfilename,"w");
	if (specfile)
	{
		fprintf(specfile,selbuffer);
		fclose(specfile);
	}
	else
	{
#define SORT_F_SPECFILE 0x001CFF14
		return SORT_F_SPECFILE;
	}

	strcpy(selbuffer,specfilename);
	sel_buf.dsc$w_length = strlen(selbuffer);				
	status = SOR$SPEC_FILE(&sel_buf);
	return status;
}

static int findfield(struct fieldlst_s lst[], int pos, int len, int cnt)
{
	register int srchidx;
	
	for (srchidx=0; srchidx<cnt;++srchidx)
	{
		if (lst[srchidx].start == pos && lst[srchidx].len == len)
		{
			return srchidx+1;
		}
	}
	return 0;
}

static void addfield(struct fieldlst_s lst[], int pos, int len, int cnt)
{
	lst[cnt].start = pos;
	lst[cnt].len = len;
}

/*
**	Routine:	vmssort_err()
**
**	Function:	Translate an error from vmssort.
**
**	Description:	Check the vms_status and translate it into a wangcode an error code and an error message.
**
**	Arguments:
**	vms_status	The code returned from vmssort.
**	wangcode	The wang return code for sort.
**	errcode		An error code used to indicate what type of error occured.
**			0 = no error
**			1 = a recoverable error (reissue OUTPUT getparm)
**			2 = non recoverable error.
**	errmess		An 80 char error message.
**
**	Globals:	None
**
**	Return:
**	0		No error
**	1		Some type of error
**
**	Warnings:	None
**
**	History:	
**	09/30/94	Written by GSL
**
*/
static int vmssort_err(int vms_status, char *routine, int4 *wangcode, int4 *errcode, char *errmess)
{
	short	status_message_len;
	char	status_message[256];
	int	status;

#include "vmssort3.d"

	*wangcode = 0;
	*errcode = 0;
	*errmess = (char)0;

	if (SS$_NORMAL & vms_status)
	{
		/*
		**	Success
		*/
		return 0;
	}

	/*
	**	Get the status code translated to a message
	*/
	status_message_len = 0;
	status_message_desc.dsc$w_length = sizeof(status_message) - 1;
	status = SYS$GETMSG( vms_status, &status_message_len, &status_message_desc, 15, 0 );
	if (SS$_NORMAL & status)
	{
		status_message[status_message_len] = (char)0;
		status_message[65] = (char)0;
	}
	else
	{
		sprintf(status_message,"STATUS=%ld",(long)vms_status);
	}
	sprintf(errmess,"%s (%s)", status_message, routine);


	if ($VMS_STATUS_SEVERITY(vms_status)== STS$K_WARNING)
	{
		/*
		**	Some misc warning occured
		*/
		*wangcode = 1;
		return 0;
	}

#define SORT_WRITEERR	0x001C10D0
#define SORT_OPENOUT	0x001C10A0

	if ($VMS_STATUS_SEVERITY(vms_status)== STS$K_ERROR)
	{
		if ((vms_status & STS$M_COND_ID) == SORT_WRITEERR)
		{
			/*
			**	SORT-E-WRITEERR occurs when record lenght or record type changes.
			**	Output still gets produced (padded/truncated)
			*/
			*wangcode = 1;
			return 0;
		}

		if ((vms_status & STS$M_COND_ID) == SORT_OPENOUT)
		{
			if (0==strcmp(routine,"SOR$SORT_MERGE"))
			{
				/*
				**	Replace message with one that is more meaningful.
				*/
				sprintf(errmess,"%s (%s)", "%SORT-E-OPENOUT error opening Workfile as output", routine);
			}
		}
	}

	if (SORT_F_SPECFILE == vms_status)
	{
		sprintf(errmess,"%%SORT-F-SPECFILE Error creating %s (%s)",specfilename,routine);
	}

	*wangcode = 99;
	*errcode = 2;
	return 1;

#ifdef OLD
#define ERRSYSTEM 1839540		/* 0x001C11B4 SORT-F-SYSERROR	SHR$_SYSERROR	0x11B0	*/
#define ERROUTPUT 1867940		/* 0x001C80A4 SORT-F-NOMSG	SHR$_OPENOUT	0x10A0	*/
#define ERRINPUT  1839256		/* 0x001C1098 SORT-W-OPENIN	SHR$_OPENIN	0x1098	*/
#define ERRWRITING 1839314		/* 0x001C10D2 SORT-E-WRITEERR	SHR$_WRITEERR	0x10D0	*/

	int4	vmserrcode;
	
	vmserrcode = vms_status & 0x00ffffff;

	if (ERROUTPUT == vmserrcode || ERRSYSTEM == vmserrcode)
	{
		*errcode = 1;
		return 1;
	}
	else if (ERRWRITING == vmserrcode)  		/* ERRWRITING appears to happen when producing */
	{						/* a fixed length output from a var length inp */
		*wangcode = 1;
		return 0;
	}
	else
	{
		*wangcode = 99;
		*errcode = 2;
		return 1;
	}
#endif
}


#endif /* VMS */
/*
**	History:
**	$Log: vmssort.c,v $
**	Revision 1.5  1996/08/19 22:33:06  gsl
**	drcs update
**	
**
**
*/
