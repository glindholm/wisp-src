/*
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
*/


/*
**	File:		string.c
**
**	Project:	WISPLIB
**
**	Purpose:	Wang VSSUB STRING
**
*/

/*
**	Includes
*/

#include <ctype.h> 
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"
#include "wmalloc.h"

static unsigned char ascii2ebcdic[256] =
{
/* ATOE   x0    x1    x2    x3    x4    x5    x6    x7    x8    x9    xA    xB    xC    xD    xE    xF   */
/* 0x */  0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F, 0x16, 0x05, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
/* 1x */  0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26, 0x18, 0x19, 0x3F, 0x27, 0x3F, 0x3F, 0x3F, 0x3F,
/* 2x */  0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D, 0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,
/* 3x */  0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,
/* 4x */  0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
/* 5x */  0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,
/* 6x */  0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,
/* 7x */  0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xC0, 0x4F, 0xD0, 0xA1, 0x07,
/* 8x */  0xFD, 0x73, 0x69, 0x8A, 0x75, 0x57, 0xEB, 0x56, 0x8B, 0x76, 0x58, 0x77, 0x8C, 0x59, 0xB7, 0xFC,
/* 9x */  0xCC, 0xFA, 0x9C, 0x8D, 0x78, 0x62, 0x8E, 0x63, 0x66, 0xBA, 0xBB, 0x3F, 0x43, 0x3F, 0x3F, 0x3F,
/* Ax */  0x8F, 0x9A, 0x9B, 0x3F, 0x9D, 0xDC, 0x41, 0x3F, 0x3F, 0x3F, 0x52, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
/* Bx */  0x3F, 0x3F, 0x3F, 0x29, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x4A, 0x44, 0x3F,
/* Cx */  0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
/* Dx */  0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
/* Ex */  0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x55,
/* Fx */  0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x48, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0xFF
};

static unsigned char ebcdic2ascii[256] =
{
/* ETOA   x0    x1    x2    x3    x4    x5    x6    x7    x8    x9    xA    xB    xC    xD    xE    xF   */
/* 0x */  0x00, 0x01, 0x02, 0x03, 0x1A, 0x09, 0x0A, 0x7F, 0x1A, 0x1A, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 
/* 1x */  0x10, 0x11, 0x12, 0x13, 0x1A, 0x0A, 0x08, 0x1A, 0x18, 0x19, 0x1A, 0x1A, 0x1A, 0x1A, 0x1A, 0x1A, 
/* 2x */  0x1A, 0x1A, 0x1A, 0x1A, 0x1A, 0x0A, 0x17, 0x1B, 0x1A, 0x1A, 0x1A, 0x1A, 0x1A, 0x05, 0x06, 0x07, 
/* 3x */  0x1A, 0x1A, 0x16, 0x1A, 0x1A, 0x1A, 0x1A, 0x04, 0x1A, 0x1A, 0x1A, 0x1A, 0x14, 0x15, 0x1A, 0x1A, 
/* 4x */  0x20, 0xA6, 0xE1, 0x9C, 0xBE, 0x1A, 0x1A, 0x1A, 0xF5, 0x1A, 0xBD, 0x2E, 0x3C, 0x28, 0x2B, 0x7C, 
/* 5x */  0x26, 0x1A, 0xAA, 0x1A, 0x22, 0xEF, 0x87, 0x85, 0x8A, 0x8D, 0x21, 0x24, 0x2A, 0x29, 0x3B, 0x5E, 
/* 6x */  0x2D, 0x2F, 0x95, 0x97, 0x61, 0x6F, 0x98, 0x85, 0x8A, 0x82, 0xBA, 0x2C, 0x25, 0x5F, 0x3E, 0x3F, 
/* 7x */  0x8D, 0x95, 0x97, 0x81, 0x87, 0x84, 0x89, 0x8B, 0x94, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22, 
/* 8x */  0x81, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x83, 0x88, 0x8C, 0x93, 0x96, 0xA0, 
/* 9x */  0x82, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72, 0xA1, 0xA2, 0x92, 0xA4, 0x41, 0x45, 
/* Ax */  0x49, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x4F, 0x55, 0x41, 0x5B, 0x59, 0x4A, 
/* Bx */  0x45, 0x45, 0x49, 0x4F, 0x55, 0x59, 0x53, 0x8E, 0x45, 0x49, 0x99, 0x9A, 0x41, 0x5D, 0x49, 0x4F, 
/* Cx */  0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x55, 0x41, 0x90, 0x49, 0x1A, 0x1A, 
/* Dx */  0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x4F, 0x55, 0xA5, 0x1A, 0x1A, 0x1A, 
/* Ex */  0x5C, 0x1A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x1A, 0x86, 0x87, 0x1A, 0x1A, 0x1A, 
/* Fx */  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x91, 0x1A, 0x8F, 0x80, 0x1A, 0xFF 
};

/*
**	Static Function Prototypes
*/

static void center_text(char *l_in_string, int4 l_len, char *l_out_string);
static void leftj_text(char *l_in_string, int4 l_len, char *l_out_string);
static void rightj_text(char *l_in_string, int4 l_len, char *l_out_string);
static void rev_text(char *l_in_string, int4 l_len, char *l_out_string);
static void move_and_pad(char *l_in_str, int4 l_ndx, int4 l_len, char *l_out_str, int4 l_ondx, int4 l_olen, char l_pad);
static void str_xlat(char func, char *in_string, char *out_string, int4 len, char *table);
					     						
/*
STRING (Function, variable number of parameters dependent on Function)
STRING ("MV", Instr, Inlen, Outstr, Outlen, Pad)
STRING ("MI", Instr, Inoff, Inlen, Outstr, Outoff, Outlen, Pad)
STRING ("AE", Instr, Inlen, Outstr)
STRING ("BI", Instr, Inlen, Outstr)
STRING ("CT", Instr, Inlen, Outstr)
STRING ("EA", Instr, Inlen, Outstr)
STRING ("LJ", Instr, Inlen, Outstr)
STRING ("RJ", Instr, Inlen, Outstr)
STRING ("RV", Instr, Inlen, Outstr)
STRING ("TT", Instr, Inlen, Trantab, Outstr)
STRING ("TL", Instr, Inlen, Tranlist, Outstr)
*/

void STRING(char* func_type, char* in_string, ...)
{
	va_list the_args;
	char *out_string;
	int4  ndx, ondx; 								/* items.				*/
        int  arg_count;									/* Number of int4words in arg. list.	*/
	int4 len, olen, *plen, *pndx;							/* Local copy of lengths.		*/
	char padc, *pc;
	char *table;
	
	va_start(the_args, in_string);							/* Setup pointer to start of list.	*/
	arg_count = WL_va_count();							/* How many args are there ?		*/
	if (arg_count < 3)
	{
		werrlog(WERRCODE(63004),0,0,0,0,0,0,0,0);					/* Invalid number of parameters.	*/
		va_end(the_args);							/* Done with the args.			*/
		return;
	}

	/* ARG3 - Inlen (for all funcitons except "MI") */
	plen = va_arg(the_args, int4*);							/* Get addr. of length parameter.	*/
	len = WL_get_swap(plen);

	if (WL_wtracing())
	{
		char	tbuf[44];

		if (len < sizeof(tbuf)-4)
		{
			memcpy(tbuf,in_string, len);
			tbuf[len] = '\0';
		}
		else
		{
			memcpy(tbuf,in_string, sizeof(tbuf)-4);
			tbuf[sizeof(tbuf)-4] = '\0';
			strcat(tbuf,"...");
		}

		wtrace("STRING","ENTRY", "Function=[%2.2s] Instr=[%s] Arg3=[%d] args=%d", func_type, tbuf, len, arg_count);
	}

	switch(*func_type)								/* Check first char in function type.	*/
	{                                     
		case 'C': case 'L': case 'R':
		{
			if (arg_count == 4)						/* Is there a fourth arg.		*/
			{
				out_string = va_arg(the_args, char*);			/* Get addr. of fourth arg.		*/
			}
			else
			{
				out_string = in_string;					/* Addr. of out is in.			*/
			}
			if (*func_type == 'C')  center_text(in_string,len,out_string);	/* Center the text			*/
			if (*func_type == 'L')  leftj_text(in_string,len,out_string);	/* Left-justify the text		*/
			if (*func_type == 'R')
			{
				if (*(func_type+1) == 'J')  
			      		rightj_text(in_string,len,out_string);	/* Right-justify the text		*/
				else 
					rev_text(in_string,len,out_string);
			}
			break;								/* All done.				*/
		}
		case 'M':
		{
			if (func_type[1] == 'I')					/* Move string starting with offset pos.*/
			{								/* and pad with specified character.	*/
				if (arg_count < 5)
				{
					werrlog(WERRCODE(63004),0,0,0,0,0,0,0,0);		/* Invalid number of parameters.	*/
					va_end(the_args);				/* Done with the args.			*/
					return;
				}
				/* The arg3 was actually the ndx not the len */
				ndx = len;

				/* Now get the len */
				plen = va_arg(the_args, int4*);				/* Get addr. of length parameter.	*/
				len = WL_get_swap(plen);
			}
			else if (func_type[1] == 'V')					/* Move string and pad with specified	*/
			{								/*  character.				*/
				if (arg_count < 4)
				{
					werrlog(WERRCODE(63004),0,0,0,0,0,0,0,0);		/* Invalid number of parameters.	*/
					va_end(the_args);				/* Done with the args.			*/
					return;
				}
				ndx = 0;						/* Set so no offset on move.		*/
				ondx = 0;
			}
			else
			{
				werrlog(WERRCODE(63002),func_type,0,0,0,0,0,0,0);		/* Function not implemented.		*/
				va_end(the_args);					/* Done with the args.			*/
				return;
			}

			out_string = va_arg(the_args, char*);				/* Get addr. of output location.	*/
			if (func_type[1] == 'I')					/* Get all of the optional arguments.	*/
			{
				if (arg_count > 5)					/* Get passed Output Index.		*/
				{
					pndx = va_arg(the_args, int4*);			/* Get addr. of output offset parameter.*/
					ondx = WL_get_swap(pndx);
				}
				else ondx = 0;						/* Start with beginning of out_string.	*/
				if (arg_count > 6)					/* Get passed Output length.		*/
				{
					plen = va_arg(the_args, int4*);			/* Get addr. of output length parameter.*/
					olen = WL_get_swap(plen);
				}
				else olen = len;					/* Use the in_string length.		*/
				if (arg_count > 7)					/* Get the passed Pad Character.	*/
				{
					pc = va_arg(the_args, char*);			/* Get addr. of pad character.		*/
					padc = *pc;					/* Assign the padding character.	*/
				}
				else padc = ' ';					/* Space is assumed for pad.		*/
			}
			if (func_type[1] == 'V')					/* Get all of the optional arguments.	*/
			{
				if (arg_count > 4)					/* Get passed Output Length.		*/
				{
					plen = va_arg(the_args, int4*);			/* Get addr. of output length parameter.*/
					olen = WL_get_swap(plen);
				}
				else olen = len;					/* Use the in_string length.		*/
				if (arg_count > 5)					/* Get passed Pad Character.		*/
				{
					pc = va_arg(the_args, char*);			/* Get addr. of pad character.		*/
					padc = *pc;					/* Assign the padding character.	*/
				}
				else padc = ' ';					/* Space is assumed for pad.		*/
			}
			move_and_pad(in_string,ndx,len,out_string,ondx,olen,padc);	/* Do move and pad with character.	*/
			break;
		}
	        case 'A':
	        case 'E':
			if (arg_count < 3)
			{
				werrlog(WERRCODE(63004),0,0,0,0,0,0,0,0);			/* Invalid number of parameters.	*/
				va_end(the_args);					/* Done with the args.			*/
				return;
			}

			if (arg_count==4) 
			{
				out_string=va_arg(the_args,char*);
			}
			else
			{
				out_string=in_string;
			}

			if ('A' == func_type[0] && 'E' == func_type[1])
			{
				str_xlat('T',in_string,out_string,len,(char *)ascii2ebcdic);
			}
			else if ('E' == func_type[0] && 'A' == func_type[1])
			{
				str_xlat('T',in_string,out_string,len,(char *)ebcdic2ascii);
			}
			else
			{
				werrlog(WERRCODE(63002),func_type,0,0,0,0,0,0,0);		/* Function not implemented.		*/
				va_end(the_args);					/* Done with the args.			*/
				return;
			}
			
			break;
	        case 'T':
	        {
			if (arg_count < 4)
			{
				werrlog(WERRCODE(63004),0,0,0,0,0,0,0,0);			/* Invalid number of parameters.	*/
				va_end(the_args);					/* Done with the args.			*/
				return;
			}

			table=va_arg(the_args, char*);
			if (arg_count==5) 
			{
				out_string=va_arg(the_args,char*);
			}
			else
			{
				out_string=in_string;
			}
			str_xlat(func_type[1],in_string,out_string,len,table);
			break;
		}
		default:
		{
			werrlog(WERRCODE(63002),func_type,0,0,0,0,0,0,0);			/* Function not implemented.		*/
			break;
		}
 	}
 	va_end(the_args);								/* Done with the args.			*/
}

static void center_text(char *l_in_string, int4 l_len, char *l_out_string)			/* Centera line of text.		*/
{
	int  i, i_len, x;								/* Work variables.			*/
	char *work_area;								/* Pointer to work area			*/
	
      	i_len = l_len;									/* Save some time, fetch mem. once.	*/
	work_area = wisp_malloc(i_len);							/* Allocate required memory.		*/
	--i_len;									/* Offsets start at 0.			*/
	for (i = i_len; (l_in_string[i]==' ') && i; --i);				/* Where does it end.			*/
	++i_len;									/* Restore to actual length.		*/
	if (i)  									/* String length is 1 plus i if i not	*/
	{                                                                               /* = 0					*/
		++i;	  								
	}
	else
	{
		return;									/* Input string is all spaces.		*/
	}
	while (*l_in_string == ' ')							/* Now skip leading spaces.		*/
	{
		l_in_string++;
		i--;
	}
	x = i;										/* Save the actual length.		*/
	i = (i_len - i)/2;								/* Compute the starting point.		*/
	memset(work_area,' ',i_len);							/* Initialize the area.			*/
	memcpy(&work_area[i], l_in_string, x);						/* Move the string.			*/
	memcpy(l_out_string, work_area, i_len);						/* Move it to the destination addr.	*/
	free(work_area);								/* Done with storage.			*/
}
											/* Left justify a line of text.		*/
static void leftj_text(char *l_in_string, int4 l_len, char *l_out_string)
{                                                                                                                                 
	char *tmp;
	char *src, *dest;
	int len, outlen;

	tmp = (char *)calloc((int)(l_len+1),sizeof(char));
	for (	src = l_in_string, len=0 ;						/*  skip leading white space		*/ 
		isspace((int)*src) && len < l_len; 
		++src, ++len);
	for (	dest = tmp, outlen=0; 							/* copy relevant data			*/
		len<l_len; 
		++dest, ++src, ++len, ++outlen)	 *dest = *src;
	for ( ; outlen<l_len; ++dest, ++outlen)						/* pad with spaces on right		*/
		*dest = ' ';
	memcpy(l_out_string,tmp,(int)l_len);
	free(tmp);
}

static void rightj_text(char *l_in_string, int4 l_len, char *l_out_string)			/* Right justify a line of text.	*/
{
	char *tmp;
	char *src, *dest;
	int len;

	tmp = (char *)calloc((int)(l_len+1),sizeof(char));
	memset(tmp,' ',(int)l_len);							/* Fill with spaces.			*/
	len = l_len;									/* The length of the string.		*/
	src =  l_in_string + l_len - 1;							/* Point to last char.			*/
	dest = tmp;
	while( len && *src == ' ')							/* While spaces and still text.		*/
	{
		src--;									/* Search backwards.			*/
		dest++;									/* Increment the destination.		*/
		len--;
	}
	if (len) memcpy(dest,l_in_string,len);						/* If there was text copy it.		*/
	memcpy(l_out_string,tmp,(int)l_len);
	free(tmp);
}

static void rev_text(char *l_in_string, int4 l_len, char *l_out_string)			/* Right justify a line of text.	*/
{
	char *src, *dest, *tmpbuf;
       	int i;
	
	tmpbuf = (char *)calloc((int)(l_len+1),sizeof(char));
	for (src=l_in_string+l_len-1, dest=tmpbuf, i=l_len;
		i; --i, --src, ++dest) *dest = *src;
	memcpy(l_out_string,tmpbuf,(int)l_len);
	free(tmpbuf);
}

static void move_and_pad(char *l_in_str, int4 l_ndx, int4 l_len, char *l_out_str, int4 l_ondx, int4 l_olen, char l_pad)
											/* Move and pad with character.		*/
{
	char *st_pos, *cpy_pos;
	int i, cnt;
	
	st_pos = l_in_str;								/* Point to beginning of input string.	*/
	for (i = 0; i < l_ndx; i++) st_pos++;						/* Step to starting offset in in_string.*/
	cpy_pos = l_out_str;								/* Point to beginning of output string.	*/
	for (i = 0; i < l_ondx; i++) cpy_pos++;						/* Step to starting offset in out_string.*/
	cnt = 0;
	for (i = 0; i < l_len; i++)
	{
		*cpy_pos++ = *st_pos++;							/* Move char to output string.		*/
		cnt++;									/* Increment num characters copied.	*/
	}
	for (i = cnt; i < l_olen; i++)  *cpy_pos++ = l_pad;				/* Pad character for remaining length.	*/
}

static void str_xlat(char func, char *in_string, char *out_string, int4 len, char *table)
{
	char *tmp_outbuff;
	register char *outp, *inp, *p;
	register int cnt,cnt2;
	int tablen;

	tmp_outbuff = calloc((int)(len+1),sizeof(char));
	switch (func)
	{
	case 'T':	/* Translation Table */
		for (inp=in_string, outp=tmp_outbuff, cnt=0; cnt<len; ++cnt, ++inp, ++outp)
		{
			*outp = table[ *((unsigned char *)inp) ];
		}
		break;

	case 'L':	/* Translation List */
		for (p=table, tablen=0; !(*p==(char)0 && *(p+1)==(char)0); ++tablen, p+=2) 
		{
			/* Calculate tablen = number of entries in the list */
		}

		for (inp=in_string, outp=tmp_outbuff, cnt=0; cnt<len; ++cnt, ++inp, ++outp)
		{
			for (p=table, cnt2=0; cnt2<tablen && *(p+1) != *inp; cnt2++, p += 2) { /* EMPTY */ };
			if (cnt2<tablen) 
				*outp = *p;
			else 
				*outp = *inp;
		}
	}
	memcpy(out_string,tmp_outbuff,(int)len);
	free(tmp_outbuff);
}
/*
**	History:
**	$Log: string.c,v $
**	Revision 1.25  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.24  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.23  2003/01/17 19:46:23  gsl
**	Switch STRING to use stdarg.h
**	
**	Revision 1.22  2002/12/10 17:09:16  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.21  2002/12/09 21:09:32  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.20  2002/07/16 16:24:51  gsl
**	Globals
**	
**	Revision 1.19  2002/07/12 19:10:17  gsl
**	Global unique WL_ changes
**	
**	Revision 1.18  2002/07/12 17:01:01  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.17  2002/07/11 20:29:14  gsl
**	Fix WL_ globals
**	
**	Revision 1.16  2002/07/10 21:05:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.15  2002/07/02 21:15:29  gsl
**	Rename wstrdup
**	
**	Revision 1.14  2002/06/25 15:21:53  gsl
**	Change to use wmalloc()
**	
**	Revision 1.13  1998/10/20 14:27:12  gsl
**	Add tracing
**	
**	Revision 1.12  1996-08-19 18:33:00-04  gsl
**	drcs update
**
**
**
*/
