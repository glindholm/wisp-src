static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	Copyright (c) 1994 by International Digital Scientific, Inc.	*/
			/* 	All rights reserved.						*/
			/************************************************************************/

#include "idsistd.h"
#include "werrlog.h"

#ifdef VMS
#include <stdio.h>
#include <descrip.h>
#define MAXLINE 10000
#include <ssdef.h>
#include <libdef.h>

int *btransl(char *in_string,short *len_flds,char *eb_as)
{	
	struct dsc$descriptor_s in_ebs, out_ebs;				/* Descirptor to sting for system service.	*/
 
	char  *l_ebas;								 /* Create local values.			*/
	short l_numflds;
	short *l_rtn;
	
	int w, sz_of, i, tmp_sz, k, l;						/* Indexes for working string area.		*/
	char tmp_fld;								/* Temporary type of field being checked.	*/
	int4 mode;								/* Mode for wfname.				*/
	int4 ez;
	char *in_line, *hld_out, *hld_in, *out_line, *in_fld, *out_fld, *hld_fld;	/* Hold address for repointing address.	*/
	char *inn_hld;

	in_line		= in_string;						/* Points to input file/lib/vol.		*/
	sz_of		= *len_flds;						/* Assigns the nbr of field definitions passed.	*/
	l_ebas		= eb_as;						/* Points to field which tells asci to ebsidic.	*/
	out_line = malloc(sz_of+3);						/* Set area for command line to be created out.	*/
	out_fld = malloc(sz_of+3);						/* Set area for command line to be created out.	*/
	hld_fld = malloc(sz_of+3);						/* Set area for command line to be created out.	*/
	memset(out_line, '\0', sz_of+3); 					/* Initializes write buffer.			*/
	memset(out_fld, '\0', sz_of+3); 					/* Initializes write buffer.			*/
	memset(hld_fld, '\0', sz_of+3); 					/* Initializes write buffer.			*/

	w = 0;
	inn_hld = out_line;		
	hld_in = hld_fld;							/*hold initial addresses.			*/
	hld_out = out_fld;
	k = 0;
	while(k < sz_of)							/* Evaluate each field with k.			*/
	{
		hld_fld[0] = in_line[k];

		in_ebs.dsc$w_length = 1;					/* Initialize structure for in field usE.*/
		in_ebs.dsc$a_pointer = hld_fld;					/* Hold address	.			*/
		in_ebs.dsc$b_dtype = DSC$K_DTYPE_T;
		in_ebs.dsc$b_class = DSC$K_CLASS_S;
		out_ebs.dsc$w_length = 1;					/* Initialize structure for out string.	*/
		out_ebs.dsc$a_pointer = out_fld;				/* Init address.			*/
		out_ebs.dsc$b_dtype = DSC$K_DTYPE_T;
		out_ebs.dsc$b_class = DSC$K_CLASS_S;
		ez = 0;
		if (hld_fld[0] == '\0')
			out_line[k] = '\0';
		else
		{
			if (*eb_as == '2')					/* If flag 2 asc to ebs	.			*/
				ez = LIB$TRA_ASC_EBC(&in_ebs, &out_ebs);
			else
				ez = LIB$TRA_EBC_ASC(&in_ebs, &out_ebs);	/*if flag 1 ebs to asc.				*/
			if ((ez == SS$_NORMAL || ez == LIB$_INVCHA))
				out_line[k] = out_fld[0];			/* Copy to write buffer.			*/
			else
				out_line[k] = hld_fld[0];			/* Copy to write buffer.			*/
		}
		++k;
	}
	
	i = 0;									/* Set counter to 0.				*/
	while (i < sz_of)							/* Until end of count of file size.		*/
	{
		*in_line++ = *out_line++;					/* Move out line to inline.			*/
		++i;								/* Bump the counter.				*/
	}	

	out_line = inn_hld;		
	hld_fld = hld_in;							/*hold initial addresses.			*/
	out_fld = hld_out;

	free(out_line);								/* Free the used areas.				*/
	free(out_fld);								/* Free the used areas.				*/
	free(hld_fld);								/* Free the used areas.				*/

	return(NULL);								/* Why is this here?				*/
}	
											
#else
int *btransl(char *in_string,short *len_flds,char *eb_as)
{
	werr_message_box("btransl: Not Supported");
	return(NULL);								/* Why is this here?				*/
}
#endif
											
/*
**	History:
**	$Log: btransl.c,v $
**	Revision 1.12  1996-08-19 18:32:10-04  gsl
**	drcs update
**
**
**
*/
