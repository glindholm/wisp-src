static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef VMS
#include <stdio.h>
#include <rms.h>
#include <descrip.h>
#include <ssdef.h>
#include <libdef.h>

#include "idsistd.h"
#include "wperson.h"
#include "filext.h"

#define MAXLINE 10000

wtransl(in_file,in_lib,in_vol,out_file,out_lib,out_vol,lay_out,num_flds,eb_as,rtn)
	char *in_file, *in_lib, *in_vol, *out_file, *out_lib, *out_vol, *lay_out, *eb_as;
	short *num_flds, *rtn;
{	
	struct r_layout 							/* Structure defines table passed from cobol.	*/
	{ 				
		short str_pos;							/* Starting pos pic s9(4).			*/
		short end_pos;							/* Ending pos pic s9(4).			*/
		char fld_type;							/* Field type pic x value Z,C,P,B. 		*/
	} *struct_ptr;

	struct FAB fab1; 							/* Structure point to File ATT Block.		*/
	struct RAB rab1;							/* Structure point to Record ATT Block.		*/ 
	int rms_status;

	struct dsc$descriptor_s in_ebs, out_ebs;				/* Descirptor to sting for system service.	*/
 
	char *l_infile, *l_inlib, *l_invol, *l_outfile, *l_outlib, *l_outvol, *l_ebas;
										 /* Create local values.			*/
	char	def_invol[6], def_inlib[8], def_outvol[6], def_outlib[8];
	short l_numflds;
	short l_rtn;
	
	FILE *file1, **file2;							/* Creat file name.				*/
	char *r, *end_name, *end_name1;						/* Pointers for wfname.				*/
	int w, sz_of, i, tmp_sz, k, l;						/* Indexes for working string area.		*/
	int tmp_sz_of;
	char *wfname();								/* Change Wfname to pointer to character.	*/
	char *b_string;								/* String pointer to character for btransl.	*/
	short b_length;
	char tmp_fld;								/* Temporary type of field being checked.	*/
	int4 mode;								/* Mode for wfname.				*/
	int4 ez;
	char *in_line, *hld_out, *hld_in, *out_line, *in_fld, *out_fld, *hld_fld;
										/* Hold address for repointing address.		*/
	char n_name[80];
	char l_name[80];							/* Local names for wfname.			*/

	memset(l_name, ' ', 80);						/* Initialize name to 80 soaces.		*/
	memset(n_name, ' ', 80);						/* Initialize name to 80 soaces.		*/
	mode		= 0;
	l_infile	= in_file;						/* Points to input file/lib/vol.		*/
	l_inlib		= in_lib;
	l_invol		= in_vol;
	l_outfile	= out_file;						/* Points to output file/lib/vol.		*/
	l_outlib	= out_lib;
	l_outvol	= out_vol;
	l_numflds	= *num_flds;						/* Assigns the nbr of field definitions passed.	*/
	l_rtn		= *rtn;							/* Points to the return code.			*/
	l_ebas		= eb_as;						/* Points to field which tells asci to ebsidic.	*/
										/* Cast pointer to pointer to struct.		*/
	struct_ptr	= (struct r_layout *)lay_out;				/* Point stucture of layout to pointer from call*/
										/* Initialize sz_of to length of the file.	*/
										/* By calculating the file length from the last.*/
										/* Field past starting pos + length.		*/
	sz_of	= ((struct_ptr+l_numflds-1)->str_pos) + ((struct_ptr+l_numflds-1)->end_pos);

	if (*eb_as == '2')							/* If flag 2 asc to ebs	.			*/
		tmp_sz_of = sz_of;
	else									/* If flag = 1 ebs to asc.			*/
		tmp_sz_of = sz_of - 1;
	in_line = malloc(MAXLINE);						/* Set area for command line to be created in.	*/
	out_line = malloc(sz_of);						/* Set area for command line to be created out.	*/
	memset(in_line, '\0', MAXLINE); 					/* Initializes read buffer.			*/
	memset(out_line, '\0', sz_of+3); 					/* Initializes write buffer.			*/
	memset(l_name, '\0', 80); 						/* Clears name for wfname.			*/
	memset(n_name, '\0', 80); 

 	if(*in_file == ' ')							/* Check if infile blank.			*/
	{
		l_rtn = 32;
		*rtn = l_rtn;
		return;
 	}

	if(*out_file == ' ')							/* Check if outfile blank.			*/
	{
		l_rtn = 32;
		*rtn = l_rtn;
		return;
	}

	wpload();								/* Load usage constants.			*/

	if (*in_vol == ' ') 
	{
		get_defs(DEFAULTS_IV,def_invol);
		l_invol = def_invol;						/* Point to the invol.				*/
	}

	if (*in_lib == ' ')							/* Only if no lib or volume defined		*/
	{
		get_defs(DEFAULTS_IL,def_inlib);
		l_inlib = def_inlib;						/* and the inlib.				*/
	}
										
	if (*out_vol == ' ') 
	{
		get_defs(DEFAULTS_OV,def_outvol);
		l_outvol = def_outvol;						/* Point to the outvol.				*/
	}

	if (*out_lib == ' ')							/* Only if no lib or volume defined		*/
	{
		get_defs(DEFAULTS_OL,def_outlib);
		l_outlib = def_outlib;						/* and the outlib.				*/
	}

	if (WISPFILEXT[0] == ' ' || WISPFILEXT[0] == 0)				/* Need to use our file extension.		*/
	{
		setwispfilext("DAT");						/* Just copy it in.				*/
	}

	end_name = wfname(&mode,l_invol,l_inlib,l_infile,l_name);		/* Call wfname to make name.			*/
	l_name[*end_name] = '\0';						/* Null terminate the name.			*/
	file1 = fopen(l_name,"r");						/* Open input file with name from wfname.	*/

	end_name1 = wfname(&mode,l_outvol,l_outlib,l_outfile,n_name);		/* Call wfname to make name.			*/
	n_name[*end_name1] = '\0';						/* Null terminate the name.			*/

	fab1 = cc$rms_fab;							/* Initialize fab to rms file.			*/
	fab1.fab$b_fac = FAB$M_PUT;						/* File to be output.				*/

	fab1.fab$l_fna = n_name;						/* Name of file.				*/
	fab1.fab$b_fns = sizeof(n_name) -1;					/* Size of file name.				*/
	fab1.fab$l_fop = 0;							/* Set file option to create.			*/
	fab1.fab$w_mrs = sz_of-1;						/* Set record length.				*/
	fab1.fab$b_org = FAB$C_SEQ;						/* Set file tpye sequential.			*/
	fab1.fab$b_rat = FAB$M_CR;						/* Set typr to carrage return.			*/
	fab1.fab$b_rfm = FAB$C_FIX;						/* Set file type fixed.				*/
	fab1.fab$b_shr = FAB$M_NIL;						/* Set file to no share.			*/

	rab1 = cc$rms_rab;							/* Set record att bloct to standard.		*/
	rab1.rab$l_fab = &fab1;							/* Point rab to fab.				*/
	rab1.rab$b_rac = RAB$C_SEQ;						/* Set record type to sequential.		*/
	rab1.rab$l_rbf = out_line;						/* Set input buffer.				*/
	rab1.rab$w_rsz = sz_of-1;						/* Set write buffer size.			*/
	rab1.rab$l_ubf = out_line;						/* Set output buffer.				*/
	rab1.rab$w_usz = rab1.rab$w_rsz; 					/* Set input output buffer =.			*/

										/* Open output file with name from wfanme.	*/
	rms_status = sys$create (&fab1);
	if ( (rms_status != RMS$_NORMAL) && (rms_status != RMS$_CREATED) )	/* Is the file created or open.			*/
	{
		l_rtn = 35;
		*rtn = l_rtn;
		return;
	}

	if ( (rms_status = sys$connect (&rab1) ) != RMS$_NORMAL)		/* Connet to file file lock.			*/
	{
		l_rtn = 36;
		*rtn = l_rtn;
		return;
	}

	if (file1)								/* Check to see if both files are open.		*/
	{
		w = 1;		
		hld_in = in_line;						/*hold initial addresses.			*/
		hld_out = out_line;

 		while((fread(in_line,tmp_sz_of,1,file1)) != 0)			/* Read file from disk.				*/
		{
			k = 0;
			while(k < l_numflds)					/* Evaluate each field with k.			*/
			{
				tmp_sz = (struct_ptr+k)->end_pos;		/* Store local size. 				*/
				tmp_fld = (struct_ptr+k)->fld_type;		/* Store local file type.			*/
				in_fld = malloc(tmp_sz+1);			/* Set area for command line to be created in.	*/
				out_fld = malloc(tmp_sz+1);			/* Set area for command line to be created out.	*/
				memset(in_fld, '\0', tmp_sz+1);			/* Initialize field so null terminated.		*/
				memset(out_fld, '\0', tmp_sz+1); 

				l = 0;
				hld_fld = in_fld;				/* Store address of infld.			*/

				if (tmp_fld == 'C')				/* If character convert all.			*/
					{
						while (l <= tmp_sz-1)
						{
							*in_fld++ = *in_line++;
							++l;
						}
					in_ebs.dsc$w_length = tmp_sz;
										/* Initialize structure for in field using.	*/
					in_ebs.dsc$a_pointer = hld_fld;		/* Hold address	.				*/
					in_ebs.dsc$b_dtype = DSC$K_DTYPE_T;
					in_ebs.dsc$b_class = DSC$K_CLASS_S;
					out_ebs.dsc$w_length = tmp_sz;		/* Initialize structure for out string.		*/
					out_ebs.dsc$a_pointer = out_fld;	/* Init address.				*/
					out_ebs.dsc$b_dtype = DSC$K_DTYPE_T;
					out_ebs.dsc$b_class = DSC$K_CLASS_S;
					ez == 0;
					if (*eb_as == '2')			/* If flag 2 asc to ebs	.			*/
						ez = LIB$TRA_ASC_EBC(&in_ebs, &out_ebs);
					else
						ez = LIB$TRA_EBC_ASC(&in_ebs, &out_ebs);
										/*if flag 1 ebs to asc.			*/
					l = 0;
					while (l <= tmp_sz-1)			/* Bump up to write buffer to last byte.	*/
						{
							if ((ez == SS$_NORMAL) || (ez == LIB$_INVCHA))
								*out_line++ = *out_fld++;
							else
								*out_line++ = *hld_fld++;
							++l;
						}
					} 

				if (tmp_fld == 'Z')				/* If zoned dec convert all but last byte.	*/
				{
					while (l <= tmp_sz-2)
					{
						*in_fld++ = *in_line++;
						++l;
					}
					in_ebs.dsc$w_length = tmp_sz-1;
										/* Inital struct for descriptors.		*/
					in_ebs.dsc$a_pointer = hld_fld;
					in_ebs.dsc$b_dtype = DSC$K_DTYPE_T;
					in_ebs.dsc$b_class = DSC$K_CLASS_S;
					out_ebs.dsc$w_length = tmp_sz-1;
					out_ebs.dsc$a_pointer = out_fld;
					out_ebs.dsc$b_dtype = DSC$K_DTYPE_T;
					out_ebs.dsc$b_class = DSC$K_CLASS_S;
					ez = 0;
					if (*eb_as == '2')			/* Same as above if statement above.		*/
						ez = LIB$TRA_ASC_EBC(&in_ebs, &out_ebs);
					else
						ez = LIB$TRA_EBC_ASC(&in_ebs, &out_ebs);
					l = 0;
					while (l <= tmp_sz-2)
					{
						if ((ez == SS$_NORMAL) || (ez == LIB$_INVCHA))
							*out_line++ = *out_fld++;
						else
							*out_line++ = *hld_fld++;
						++l;
					}
					b_string = in_line;			/* Set temporary address.			*/
					b_length = 1;				/* Set length for conversion.			*/
                                        cmp_byte(b_string);		/* Convert signed byte info.			*/
                                        btransl(b_string,&b_length,eb_as);	/* Convert the byte.				*/
					*in_line = *b_string;			/* Move the converted string back.		*/
					*out_line++ = *in_line++;		/* Put in the sign byte not converted.		*/
				} 
	
				if ( (tmp_fld == 'P') || (tmp_fld == 'B') )	/* If packed or binary do not touch just.	*/
				{						/* Copy to output string with convert.		*/
					while (l <= tmp_sz-1)			
					{
						*out_line++ = *in_line++;
						++l;
					}
				} 
				++k;
			}
			in_line = hld_in;					/* Reset to pointer to get next file.		*/
			out_line = hld_out;					/* Reset pointer to write buffer.		*/
	
			if ((rms_status = sys$put (&rab1)) != RMS$_NORMAL)	/* Write the output.				*/
			{
				l_rtn = 38;
				*rtn = l_rtn;
				return;
			}
	
		} 

		fclose(file1);							/* Close the file before you go.		*/
		if ((rms_status = sys$disconnect (&rab1)) != RMS$_NORMAL)	/* Check disconnect or unlock of rms file seq.	*/
		{
			l_rtn = 39;
			*rtn = l_rtn;
			return;
		}
		if ((rms_status = sys$close (&fab1)) != RMS$_NORMAL)		/* Check status of the close if good go for it.	*/
		{
			l_rtn = 40;
			*rtn = l_rtn;
			return;
		}
		free(in_line);							/* Free the used areas.				*/
		free(out_line);						
	}
}	

cmp_byte(temp_string)											
char *temp_string;
{
	int i;
	char cmp_string[30];
	char out_string[30];
	char *l_string;

	cmp_string[0] = 0x7b;
	cmp_string[1] = 0x41;
	cmp_string[2] = 0x42;
	cmp_string[3] = 0x43;
	cmp_string[4] = 0x44;
	cmp_string[5] = 0x45;
	cmp_string[6] = 0x46;
	cmp_string[7] = 0x47;
	cmp_string[8] = 0x48;
	cmp_string[9] = 0x49;
	cmp_string[10] = 0x7d;
	cmp_string[11] = 0x4a;
	cmp_string[12] = 0x4b;
	cmp_string[13] = 0x4c;
	cmp_string[14] = 0x4d;
	cmp_string[15] = 0x4e;
	cmp_string[16] = 0x4f;
	cmp_string[17] = 0x50;
	cmp_string[18] = 0x51;
	cmp_string[19] = 0x52;
	cmp_string[20] = 0xF0;
	cmp_string[21] = 0xF1;
	cmp_string[22] = 0xF2;
	cmp_string[23] = 0xF3;
	cmp_string[24] = 0xF4;
	cmp_string[25] = 0xF5;
	cmp_string[26] = 0xF6;
	cmp_string[27] = 0xF7;
	cmp_string[28] = 0xF8;
	cmp_string[29] = 0xF9;

	out_string[0] = 0x30;
	out_string[1] = 0x31;
	out_string[2] = 0x32;
	out_string[3] = 0x33;
	out_string[4] = 0x34;
	out_string[5] = 0x35;
	out_string[6] = 0x36;
	out_string[7] = 0x37;
	out_string[8] = 0x38;
	out_string[9] = 0x39;
	out_string[10] = 0x7d;
	out_string[11] = 0x4a;
	out_string[12] = 0x4b;
	out_string[13] = 0x4c;
	out_string[14] = 0x4d;
	out_string[15] = 0x4e;
	out_string[16] = 0x4f;
	out_string[17] = 0x50;
	out_string[18] = 0x51;
	out_string[19] = 0x52;
	out_string[20] = 0xC0;
	out_string[21] = 0xC1;
	out_string[22] = 0xC2;
	out_string[23] = 0xC3;
	out_string[24] = 0xC4;
	out_string[25] = 0xC5;
	out_string[26] = 0xC6;
	out_string[27] = 0xC7;
	out_string[28] = 0xC8;
	out_string[29] = 0xC9;

	i = 0;
	while (i < 30)
	{
		if (*temp_string == cmp_string[i])
                    *temp_string = out_string[i];
		i++;
	}
}
#endif
#ifdef unix
void wtransl()
{
	werrlog(102,"wtransl: Not Supported",0,0,0,0,0,0,0);
}
#endif
/*
**	History:
**	$Log: wtransl.c,v $
**	Revision 1.11  1996-08-19 18:33:26-04  gsl
**	drcs update
**
**
**
*/
