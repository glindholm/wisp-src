/* Subroutine STRING.C ... Performs string functions for VS to VMS conversion support.						*/

#include <ctype.h> 
#ifndef VMS	/* unix or MSDOS */
#include <malloc.h>
#endif
#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#include "movebin.h"
#include "werrlog.h"
#include <varargs.h>

void center_text(), leftj_text(), rightj_text(), rev_text(), move_and_pad(), str_xlat();
					     						
void STRING(va_alist)						                        /* Function uses variable arguments.	*/
va_dcl											/* Parameter declaration.     		*/
{
#define		ROUTINE		63000
	va_list the_args;								/* Declare variable to traverse list.	*/
	char *the_item, *func_type, *in_string, *out_string;				/* Declare pointers for the indiv. 	*/
	long  ndx, ondx; 								/* items.				*/
        int  arg_count;									/* Number of longwords in arg. list.	*/
	long len, olen, *plen, *pndx;							/* Local copy of lengths.		*/
	char padc, *pc;
	char *table;
	
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	va_start(the_args);								/* Setup pointer to start of list.	*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/
	va_start(the_args);								/* Reset the pointer.			*/
	if (arg_count < 3)
	{
		werrlog(ERRORCODE(4),0,0,0,0,0,0,0,0);					/* Invalid number of parameters.	*/
		return;
	}
	func_type    = va_arg(the_args, char*);  					/* Get addr. of first arg.		*/
	in_string = va_arg(the_args, char*);						/* Get addr. of string to manipulate.	*/
	switch(*func_type)								/* Check first char in function type.	*/
	{                                     
		case 'C': case 'L': case 'R':
		{
			plen = va_arg(the_args, long*);					/* Get addr. of length parameter.	*/
			GETBIN(&len,plen,4);						/* Put aligned value into local var.	*/
			wswap(&len);							/* Swap the word order.			*/
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
					werrlog(ERRORCODE(4),0,0,0,0,0,0,0,0);		/* Invalid number of parameters.	*/
					return;
				}
				pndx = va_arg(the_args, long*);				/* Get addr. of offset parameter.	*/
				GETBIN(&ndx,pndx,4);					/* Put aligned value into local var.	*/
				wswap(&ndx);						/* Swap the word order.			*/
			}
			else if (func_type[1] == 'V')					/* Move string and pad with specified	*/
			{								/*  character.				*/
				if (arg_count < 4)
				{
					werrlog(ERRORCODE(4),0,0,0,0,0,0,0,0);		/* Invalid number of parameters.	*/
					return;
				}
				ndx = 0;						/* Set so no offset on move.		*/
				ondx = 0;
			}
			else
			{
				werrlog(ERRORCODE(2),func_type,0,0,0,0,0,0,0);		/* Function not implemented.		*/
				return;
			}
			plen = va_arg(the_args, long*);					/* Get addr. of length parameter.	*/
			GETBIN(&len,plen,4);						/* Put aligned value into local var.	*/
			wswap(&len);							/* Swap the word order.			*/
			out_string = va_arg(the_args, char*);				/* Get addr. of output location.	*/
			if (func_type[1] == 'I')					/* Get all of the optional arguments.	*/
			{
				if (arg_count > 5)					/* Get passed Output Index.		*/
				{
					pndx = va_arg(the_args, long*);			/* Get addr. of output offset parameter.*/
					GETBIN(&ondx,pndx,4);				/* Put aligned value into local var.	*/
					wswap(&ondx);					/* Swap the word order.			*/
				}
				else ondx = 0;						/* Start with beginning of out_string.	*/
				if (arg_count > 6)					/* Get passed Output length.		*/
				{
					plen = va_arg(the_args, long*);			/* Get addr. of output length parameter.*/
					GETBIN(&olen,plen,4);				/* Put aligned value into local var.	*/
					wswap(&olen);					/* Swap the word order.			*/
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
					plen = va_arg(the_args, long*);			/* Get addr. of output length parameter.*/
					GETBIN(&olen,plen,4);				/* Put aligned value into local var.	*/
					wswap(&olen);					/* Swap the word order.			*/
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
	        case 'T':
	        {
			if (arg_count < 4)
			{
				werrlog(ERRORCODE(4),0,0,0,0,0,0,0,0);			/* Invalid number of parameters.	*/
				return;
			}
			the_item=va_arg(the_args, char*);
			GETBIN(&len,the_item,sizeof(long));
			wswap(&len);
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
			werrlog(ERRORCODE(2),func_type,0,0,0,0,0,0,0);			/* Function not implemented.		*/
			break;
		}
 	}
 	va_end(the_args);								/* Done with the args.			*/
}

void center_text(l_in_string, l_len, l_out_string)					/* Centera line of text.		*/
unsigned char *l_in_string, *l_out_string;						/* Arg. pointers.			*/
long  l_len;										/* " "					*/
{
	int  i, i_len, x;								/* Work variables.			*/
	char *work_area;								/* Pointer to work area			*/

      	i_len = l_len;									/* Save some time, fetch mem. once.	*/
	work_area = malloc(i_len);							/* Allocate required memory.		*/
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
void leftj_text(l_in_string, l_len, l_out_string)					/*  added  10-12-89 jec			*/
char *l_in_string, *l_out_string;							/* Arg. pointers.			*/
long  l_len;										/* " "					*/
{                                                                                                                                 
	char *tmp;
	char *src, *dest, *p;
	int len, outlen;

	tmp = (char *)calloc((int)(l_len+1),sizeof(char));
	for (	src = l_in_string, len=0 ;						/*  skip leading white space		*/ 
		isspace(*src) && len < l_len; 
		++src, ++len);
	for (	dest = tmp, outlen=0; 							/* copy relevant data			*/
		len<l_len; 
		++dest, ++src, ++len, ++outlen)	 *dest = *src;
	for ( ; outlen<l_len; ++dest, ++outlen)						/* pad with spaces on right		*/
		*dest = ' ';
	memcpy(l_out_string,tmp,(int)l_len);
	free(tmp);
}

void rightj_text(l_in_string, l_len, l_out_string)					/* Right justify a line of text.	*/
char *l_in_string, *l_out_string;							/* Arg. pointers.			*/
long  l_len;										/* " "					*/
{
	char *tmp;
	char *src, *dest, *p;
	int len, outlen;

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

void rev_text(l_in_string, l_len, l_out_string)						/* Right justify a line of text.	*/
char *l_in_string, *l_out_string;							/* Arg. pointers.			*/
long  l_len;										/* " "					*/
{
	char *src, *dest, *tmpbuf;
       	int i;
	
	tmpbuf = (char *)calloc((int)(l_len+1),sizeof(char));
	for (src=l_in_string+l_len-1, dest=tmpbuf, i=l_len;
		i; --i, --src, ++dest) *dest = *src;
	memcpy(l_out_string,tmpbuf,(int)l_len);
	free(tmpbuf);
}

void move_and_pad(l_in_str,l_ndx,l_len,l_out_str,l_ondx,l_olen,l_pad)			/* Move and pad with character.		*/
char *l_in_str, *l_out_str, l_pad;
long l_ndx, l_len, l_ondx, l_olen;
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

void str_xlat(func,in_string,out_string,len,table)
char func;
char *in_string, *out_string;
long len;
char *table;
{
	char *tmp;
	register char *inp, *outp, *p;
	register int cnt,cnt2;
	int tablen;

	tmp = (char *)calloc((int)(len+1),sizeof(char));
	switch (func)
	{
	      case 'T':
		for (inp=in_string, outp=tmp, cnt=0; cnt<len; ++cnt, ++inp, ++outp)
		  *outp = *(table + (int)*inp);
		break;
	      case 'L':
		for (p=table, tablen=0; !(*p==(char)0 && *(p+1)==(char)0); ++tablen,p+=2);
		for (inp=in_string, outp=tmp, cnt=0; cnt<len; ++cnt, ++inp, ++outp)
		{
			for (p=table, cnt2=0; cnt2<tablen && *(p+1) != *inp; cnt2++, p += 2);
			if (cnt2<tablen) *outp = *p;
			else *outp = *inp;
		}
	}
	strncpy(out_string,tmp,(int)len);
}
