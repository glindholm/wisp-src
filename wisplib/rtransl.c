static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include "idsistd.h"
#ifdef VMS
#include <stdio.h>
#include <descrip.h>
#define MAXLINE 10000
#include <ssdef.h>
#include <libdef.h>

rtransl(in_string,lay_out,num_flds,eb_as,rtn)
	char *in_string, *lay_out, *eb_as;
	short *num_flds, *rtn;
{	
	struct r_layout 							/* Structure defines table passed from cobol.	*/
	{ 				
		short str_pos;							/* Starting pos pic s9(4).			*/
		short end_pos;							/* Ending pos pic s9(4).			*/
		char fld_type;							/* Field type pic x value Z,C,P,B. 		*/
	} *struct_ptr;

	struct dsc$descriptor_s in_ebs, out_ebs;				/* Descirptor to sting for system service.	*/
 
	char  *l_ebas;								 /* Create local values.			*/
	short l_numflds;
	short *l_rtn;
	
	int w, sz_of, i, tmp_sz, k, l, u;					/* Indexes for working string area.		*/
	char *b_string;
	short *b_length;
	short  tmp_call;
	char tmp_fld;								/* Temporary type of field being checked.	*/
	int4 mode;								/* Mode for wfname.				*/
	int4 ez;
	char *in_line, *hld_out, *hld_in, *out_line, *in_fld, *out_fld, *hld_fld;	/* Hold address for repointing address.	*/

	in_line		= in_string;						/* Points to input file/lib/vol.		*/
	l_numflds	= *num_flds;						/* Assigns the nbr of field definitions passed.	*/
	l_rtn		= rtn; 							/* Points to the return code.			*/
	l_ebas		= eb_as;						/* Points to field which tells asci to ebsidic.	*/
										/* Casts Char pointer to pointer of struct.	*/
	struct_ptr	= (struct r_layout *) lay_out;				/* Point stucture of layout to pointer from call*/
										/* Initialize sz_of to length of the file.	*/
										/* By calculating the file length from the last.*/
										/* Field past starting pos + length.		*/
	sz_of	= ((struct_ptr+l_numflds-1)->str_pos) + ((struct_ptr+l_numflds-1)->end_pos);
	out_line = malloc(sz_of+3);						/* Set area for command line to be created out.	*/
	memset(out_line, '\0', sz_of+3); 					/* Initializes write buffer.			*/


	w = 0;		
	hld_in = in_line;							/*hold initial addresses.			*/
	hld_out = out_line;
	k = 0;
	while(k < l_numflds)							/* Evaluate each field with k.			*/
	{
		tmp_sz = (struct_ptr+k)->end_pos;				/* Store local size. 				*/
		tmp_fld = (struct_ptr+k)->fld_type;				/* Store local file type.			*/
		in_fld = malloc(tmp_sz+1);					/* Set area for command line to be created in.	*/
		out_fld = in_fld;						/* Set area for command line to be created out.	*/
		memset(in_fld, '\0', tmp_sz+1);					/* Initialize field so null terminated.		*/

		l = 0;
		hld_fld = in_fld;						/* Store address of infld.		*/

		if (tmp_fld == 'C')						/* If character convert all.		*/
		{
			while (l <= tmp_sz-1)
			{
				*in_fld++ = *in_line++;
				++l;
			}
			tmp_call = tmp_sz;
			btransl(hld_fld,&tmp_call,eb_as);
			l = 0;
			while (l <= tmp_sz-1)					/* Bump up to write buffer to last byte.*/
				{
					*out_line++ = *hld_fld++;
					++l;
				}
			} 

			if (tmp_fld == 'Z')					/* If zoned dec convert all but last byte.	*/
			{
				while (l <= tmp_sz-2)
					{
						*in_fld++ = *in_line++;
						++l;
					}
				tmp_call = tmp_sz-1;
				btransl(hld_fld,&tmp_call,eb_as);
				l = 0;
				while (l <= tmp_sz-2)
					{
						*out_line++ = *hld_fld++;
						++l;
					}
					b_string = in_line;			/* Set temporary address.			*/
					b_length = (short *)1;			/* Set length for conversion.			*/
                                        cmp_byte(b_string);			/* Convert signed byte info.			*/
                                        btransl(b_string,&b_length,eb_as);	/* Convert the byte.				*/
					*in_line = *b_string;			/* Move the converted string back.		*/
					*out_line++ = *in_line++;		/* Put in the sign byte not converted.		*/
				} 
	
			if ( (tmp_fld == 'P') || (tmp_fld == 'B') )		/* If packed or binary do not touch just.	*/
				{						/* Copy to output string with convert.		*/
					while (l <= tmp_sz-1)			
					{
						*out_line++ = *in_line++;
						++l;
					}
				} 
			in_fld = out_fld;					/* Reset original pointer.			*/
			free(in_fld);						/* Free the used areas.				*/
			++k;
	}
	
	in_line = hld_in;							/* Reset to pointer to get next file.		*/
	out_line = hld_out;							/* Reset pointer to write buffer.		*/

	i = 0;									/* Set counter to 0.				*/
	while (i <= sz_of)							/* Until end of count of file size.		*/
	{
		*in_line++ = *out_line++;					/* Move out line to inline.			*/
		++i;								/* Bump the counter.				*/
	}	

	free(out_line);								/* Free the used areas.				*/
}	
											
#endif
#ifdef unix
rtransl()
{
	werr_message_box("rtransl: Not Supported");
}
#endif
											
/*
**	History:
**	$Log: rtransl.c,v $
**	Revision 1.9  1996-08-19 18:32:50-04  gsl
**	drcs update
**
**
**
*/
