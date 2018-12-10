/*
******************************************************************************
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
******************************************************************************
*/


#define EXT extern

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "pgcommon.h"
#include "pgglobal.h"
#include "pgstruct.h"
#include "pgkeyw.h"
#include "pgeqtns.h"
#include "pgcblsrc.h"

static int tstkey(char* cptr,char* list[]);						/* Test if valid end for key position.	*/
static void adj_quotes(char* lptr, int qcnt);						/* Change quotes within string to ".	*/
static void convert_to_dbl(void);							/* Convert ' within string to ".	*/


void go_upper(char *strp)								/* Converts a string to upper case.	*/
{
	char *tmp_add;

	tmp_add = strp;
	while (*tmp_add)
	{
		*tmp_add = toupper(*tmp_add);						/* Upper case the keyword to compare.	*/
		tmp_add++;
	}
}

int strpos(char* src, char* srch)							/* search a string for the occurence of	*/
						 				/* another string src is the string to	*/
{											/* search, srch is the match		*/
	int i;
	char *tsrc,*tsrch;

	i = 0;										/* start position counter		*/
	do
	{
		tsrc = src;								/* copy the pointer			*/
		tsrch = srch;

		do
		{
			if (*tsrch != *tsrc) break;					/* no match				*/
			tsrch++;
			tsrc++;
		} while (*tsrch && *tsrc);						/* till null				*/
		if (!*tsrch) return(i);							/* a match				*/
		if (!*tsrc) return(-1);							/* out of space				*/
		i++;
	} while (*(++src));								/* till null				*/
	return(-1);									/* didn't match				*/
}

int trim(char* string)									/* Trim a string of trailing blanks.	*/
{
	register int i;									/* Working register storage.		*/

	for (i = strlen(string)-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == '\0')); i--)
	{
		string[i] = '\0';
	}
	return(i+1);									/* Return the string length.		*/
}

int strlast(char* string, char srch)							/* determine if the srch char is the	*/
											/* last non-whitespace			*/
{
	register int i;									/* Working register storage.		*/
	for 	(i = strlen(string)-1;
		(i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == '\0') || (string[i] == '\n'));
		(i--));
	if (string[i] == srch)	return(1);						/* it is the last char			*/
	else			return(0);						/* not found				*/
}

void sp_trunc(char* text)								/* Truncate a string at the first space	*/
{
	int i;
	i = strpos(text," ");								/* find the space			*/
	if ( i != -1 ) text[i] = '\0';							/* replace with a null			*/

}

int nexttok(void)										/* Get psn of next token from linein,	*/
{											/*  and set the next ptr as well.	*/
	if (captr == aptr) lcnt++;							/* Increment loop counter.		*/
	captr = aptr;									/* Set the current position.		*/
	if (lcnt > 10)
	{
		write_log(util,'F','R',"LOOPING","Looping due to mishandled statement: %s",aptr);
		exit(1);								/* Force exit.				*/
	}

	if (aptr - linein >= WPLMAX)							/* Test if past valid code.		*/
	{
		while (*aptr != LNFD && *aptr != '\r') aptr++;						/* Step to the end of the line.		*/
		return(FALSE);
	}

	if ((*aptr == ':') && tststrt(aptr+1))
	{
		aptr++;									/* Step over the colon.			*/
		return(TRUE);								/* No space between label and next token.*/
	}
	while (!tststrt(next_ptr) && *next_ptr != '=' )					/* While a space, null or LNFD then step.*/
	{
		if (*next_ptr == '\0' || *next_ptr == LNFD || *next_ptr == '\r') break;			/* No more characters on line to test.	*/
		next_ptr++;								/* Step over all spacing chars.		*/
	}
	aptr = next_ptr;								/* Save the current token position.	*/
	while (tststrt(next_ptr))							/* While not a space, null or LNFD step.*/
	{
		next_ptr++;								/* Step over current token.		*/
	}
	if (*aptr == '\0' || *aptr == LNFD || *aptr == '\r' || (aptr - linein >= WPLMAX)) return(FALSE); /* No more tokens left to process.	*/
	else return(TRUE);
}

int tststrt(char* cptr)									/* Test if valid start for token pson.	*/
{
	if (*cptr == ' '  || *cptr == ',' || *cptr == HT || *cptr == '\0' || *cptr == LNFD || *cptr == '\r' ||
		(cptr - linein >= WPLMAX) )						/* In the mod code/comment area.	*/
	{										
		return FALSE;
	}
	else	return(TRUE);
}

static int tstkey(char* cptr,char* list[])						/* Test if valid end for key position.	*/
{
	if (*cptr == ' '  || *cptr == ',' || *cptr == '=' || *cptr == '(' || *cptr == '\'' ||
	    *cptr == HT || *cptr == '\0' || *cptr == LNFD || *cptr == '\r' ||
	    ( in_prompt && *cptr == '\'') ||
	    ((list != built_in_funcs && list != search_equations) && *cptr == '&'))
	{                              
		return FALSE;
	}
	else	return(TRUE);
}

int find_char(char keyc, char* keylist[])						/* Find the keychar in array.		*/
{
	register int i;

	i = 0;
	while (*keylist[i])								/* Check address.			*/
	{
		if (*keylist[i] == toupper(keyc)) return(i);				/* Return index if match,		*/
		else i++;								/*  else test the next one.		*/
	}
	return(-1);
}

int find_colin(char** cptr)								/* Look for : to define proc-heading.	*/
{
	while (tststrt(*cptr))
	{
		if (**cptr == ':') return(TRUE);
		(*cptr)++;
	}
	return(FALSE);
}

int find_keyword(char *keystr, char *keylist[])						/* Find the keyword in the list.	*/
											/* Return index in keylist.		*/
{
	char *tmp_add, *tptr;
	char tmp_str[STRBUFF];
	register int i;

	tmp_add = keystr;								/* Set local copy of ptr to token.	*/
	tptr = tmp_str;
	while (tstkey(tmp_add,keylist)) *tptr++ = toupper(*tmp_add++);
	*tptr = '\0';									/* Null terminate the string.		*/
	i = 0;
	while (*keylist[i])								/* While there are keywords to compare.	*/
	{
		if (!strcmp(tmp_str,keylist[i]))
		{
			if (*tmp_add == '\'')						/* If no space between the keyword and	*/
			{								/*  quote then change last char of key	*/
				tmp_add--;						/* to a space to nexttok() will work.	*/
				*tmp_add = ' ';
				next_ptr = tmp_add;
			}
			return(i);							/* If a match return array pos in list.	*/
		}
		i++;
	}

	return(-1);
}

int number(char c)									/* Test if character is a number.	*/
{
	if (c >= '0' && c <= '9') return(TRUE);
	else return(FALSE);
}

int operand(char c)									/* Test if character ia an operand.	*/
{											/*          + , - . / *			*/
	if (c >= '*' && c <= '/') return(TRUE);
	else return(FALSE);
}

int letter(char c)									/* Test if character is a letter.	*/
{
	if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) return(TRUE);
	else return(FALSE);
}

void set_value(char* num_buf,int* num_asn,int current_row)				/* Init the screen variable and get	*/
											/* the picture clause.			*/
{
	char literal_field[100], *cptr, lenstr[FLDLEN];
	char field_name[FLDLEN], pic_len[FLDLEN];
	int tlen, cont;

	strcpy(field_name,"&LITERAL-");							/* Load the literal.			*/
	strcat(field_name,num_buf);							/* Load in the counter.			*/

	cptr = literal_field;
	tlen = 0;
	cont = TRUE;
	while (cont)									/* Copy text into value.		*/
	{										/* Copy test to temp var and get length.*/
		convert_to_dbl();							/* Convert ' within string to ".	*/
		while (*aptr != '\'' && *aptr != LNFD && *aptr != '\r' && *aptr != '\0' && (aptr - linein < WPLMAX))
		{
			if (*aptr == '"')
			{
				*cptr++ = '\'';						/* Force display of single quote.	*/
				aptr++;
			}
			else *cptr++ = *aptr++;
			tlen++;
		}
		if (*aptr != '\'')							/* If not the closing quote then get	*/
		{									/* then next line and append to string.	*/
			get_next_line();
		}
		else cont = FALSE;		
	}
	*cptr = '\0';									/* Null the string.			*/
	aptr++;										/* Step past the closing quote.		*/
	next_ptr = aptr;

	if (in_prompt || in_message)
	{
		sprintf(lenstr,"%d",tlen);
		strcpy(cur_scn_fld->screen_fld,literal_field);				/* Load the text string.		*/
		strcpy(cur_scn_fld->len,lenstr);					/* Load the length.			*/
		if (*cur_scn_fld->screen_fld == '&') strcpy(cur_scn_fld->scn_fld_type,"SS"); /* Load the type.			*/
		else strcpy(cur_scn_fld->scn_fld_type,"S ");

		cur_scn_fld->row = current_row;						/* Load the current row.		*/
	}
	else
	{
		strcpy(cur_decl->value,literal_field);					/* Load the value.			*/
		strcpy(cur_decl->field1,field_name);					/* Load the file name.			*/
		if (in_run || in_print || in_submit)
		{
			init_using();							/* point to next using_variable.	*/
			strcpy(cur_using->var,field_name);				/* Load the field name.			*/
			strcpy(cur_using->type,"SL");
		}
		sprintf(pic_len,"(%d)",tlen);						/* Make the picture clause.		*/
		strcpy(cur_decl->length,pic_len);					/* Load the current pic size.		*/
		strcpy(cur_decl->type,"SL");						/* Load the current type.		*/
	}
}

void get_type_len(char*fld, char* rcvr,int* num_var,int* num_val,char getop)		/* Search table for fld match.		*/
											/* getop = 'T' return type in rcvr,	*/
											/* getop = 'L' return length in rcvr.	*/
{											/* getop = 'S' assign screen item len.	*/
	int retfl;									/* getop = 'A' add field to link list	*/
	char fielda[FLDLEN], fieldb[FLDLEN];						/*         if not already there.	*/
	char *cstr;								/* getop = 'C' add to linked list for	*/
	struct putparm_item *hld_pp;							/*         define of subscripted var.	*/

	write_log(util,'I','R',"GET","Getting type/size of %s from variable list.",fld);
	retfl = FALSE;									/* Init to not found.			*/

	strcpy(fieldb,fld);								/* Copy field to local variable.	*/
	go_upper(fieldb);								/* Upcase the name.			*/
											/* First check the DECLARE variables.	*/
	hld_table = cur_decl;								/* Keep track of starting declare posn.	*/
	while (cur_decl)								/* Till beginning of the list.		*/
	{
		strcpy(fielda,cur_decl->field1);					/* Copy declare field to local variable.*/
		go_upper(fielda);							/* Upcase the name.			*/

		if (!strcmp(fielda,fieldb))						/* If declare fld match.		*/
		{									/* Load the type.			*/
			if (getop == 'T' || getop == 'A' || getop == 'C') strcpy(rcvr,cur_decl->type);
			else	strcpy(rcvr,cur_decl->length);				/* Load the length.			*/

			retfl = TRUE;							/* Assignment has been made.		*/
			break;
		}
		cur_decl = cur_decl->prev_item;						/* Point to the previous one.		*/
	}
	cur_decl = hld_table;								/* Set ptr back.			*/

	if (!retfl)									/* No match yet so test the LINK vars.	*/
	{
		hld_link = cur_link;							/* Keep track of starting link posn.	*/
		while (cur_link)							/* Till beginning of the list.		*/
		{
			strcpy(fielda,cur_link->field1);				/* Copy field to local variable.	*/
			go_upper(fielda);						/* Upcase the name.			*/

			if (!strcmp(fielda,fieldb))
			{
				if (getop == 'T') strcpy(rcvr,cur_link->type); 		/* Load the type.			*/
				else	strcpy(rcvr,cur_link->length);			/* Copy length to receiver.		*/

				retfl = TRUE;
				break;
			}
			cur_link = cur_link->prev_item;					/* Point to the previous one.		*/
		}
		cur_link = hld_link;							/* Set the ptr back.			*/
	}

	if (!retfl)									/* No match yet so test the & functions.*/
	{
		int ndx;

		ndx = find_keyword(fieldb,built_in_funcs);
		if (ndx >= 0)								/* Found a match for built in function.*/
		{
			if (getop == 'T') strcpy(rcvr,"SF");				/* Load the type.			*/
			else if (getop == 'L')
			{
				strcpy(cur_assign->start_pos,"1");
				strcpy(cur_assign->length,"6");
			}
			else	strcpy(cur_scn_fld->len,"6");

			retfl = TRUE;							/* Set found flag.			*/
		}
	}

	if (!retfl)
	{
		int match;

		if (getop == 'A') write_log(util,'I','R',"ADDINGVAR","Adding Backwards referenced var: %s.", fieldb);
		else if (getop == 'C') write_log(util,'I','R',"ADDINGVAR","Adding subscript var: %s.", fieldb);
		else write_log(util,'W','R',"UNDEFVAR","Undefined variable: %s, need to adjust PIC clause.", fieldb);

		init_current(num_var);							/* Alloc next node for declare.		*/
		(*num_val)++;								/* Keep track of init stmnt role back.	*/
		strcpy(cur_decl->field1,fieldb);					/* Put in the variable name.		*/

		if (getop == 'A') strcpy(cur_decl->type," B");				/* Set to String, Backwards ref. type.	*/
		else if (getop == 'C') strcpy(cur_decl->type,rcvr);			/* Set decl type to type passed in.	*/
		else 	strcpy(cur_decl->type,"SE");					/* Set to String, External global type.	*/

		if (getop == 'A')							/* Search putparms for var referenced.	*/
		{
			char *lptr, *fptr;
			char label[FLDLEN], key[FLDLEN];
			int check_key;

			lptr = label;
			fptr = fieldb;
			while (*fptr != '-') *lptr++ = *fptr++;				/* Copy label part to temp var.		*/
			*lptr = '\0';							/* Null terminate the string.		*/
			fptr++;								/* Step over the - symbol.		*/
			strcpy(key,fptr);						/* Copy the keyword to find.		*/

			hld_pp = cur_pp;						/* Keep track of starting putparm posn.	*/
			match = 0;							/* Assume no match.			*/
			if (!strcmp(key,"FILE") || !strcmp(key,"LIBRARY") || !strcmp(key,"VOLUME"))
			{								/* If standard key then use defaults	*/
				check_key = FALSE;					/*  and don't need to check putparms.	*/
			}
			else check_key = TRUE;

			while (check_key && cur_pp)					/* Till beginning of the list.		*/
			{
				strcpy(fielda,cur_pp->label);				/* Copy putparm label to local variable.*/
				go_upper(fielda);					/* Upcase the name.			*/

				if (!strcmp(fielda,label))				/* If putparm label match.		*/
				{
					struct pp_keywords *cur_kwl;

					cur_kwl = cur_pp->kwlist;
					while (cur_kwl)					/* Till end of the keyword list.	*/
					{
						strcpy(fielda,cur_kwl->keywrd);		/* Copy putparm keyword to local variable.*/
						go_upper(fielda);			/* Upcase the name.			*/

						if (!strcmp(fielda,key))		/* If putparm keyword match.		*/
						{
							match = 1;			/* Found a match.			*/
							*cur_decl->type = *cur_kwl->type;		/* Load the type.	*/
							sprintf(cur_decl->length,"(%d)",cur_kwl->len);	/* Load the length.	*/
							strcpy(rcvr,cur_decl->type);	/* Load the return type.		*/
							
						}
						cur_kwl = cur_kwl->next_item;		/* Point to the next one.		*/
					}

				}
				cur_pp = cur_pp->prev_item;				/* Point to the previous one.		*/
			}
			cur_pp = hld_pp;						/* Set ptr back.			*/
			if (!match)							/* No match so set defaults.		*/
			{
				*cur_decl->type = 'S';					/* Load the type.			*/
				if (0==strcmp(key,"FILE")) strcpy(cur_decl->length,"(8)");
				else if (0==strcmp(key,"LIBRARY")) strcpy(cur_decl->length,"(8)");
				else if (0==strcmp(key,"VOLUME")) strcpy(cur_decl->length,"(6)");
				else
				{
					write_log(util,'W','R',"UNDEFVAR",
					"Undefined variable: %s, need to adjust PIC clause.", fieldb);
				 	strcpy(cur_decl->length,"(8)");			/* Set the default length.		*/
				}
				strcpy(rcvr,cur_decl->type);				/* Load the return type.		*/
			}
		}
		else
		{
		 	strcpy(cur_decl->length,"(8)");
			if (getop == 'T') strcpy(rcvr,cur_decl->type);			/* Load the type for ACCEPT.		*/
			else if (getop == 'C')						/* Assign the length field.		*/
			{
				char len[5];

				if (0 != strncmp(fld,"&START",6))			/* Set lenght for DECLARE.  No length	*/
				{							/* is needed for integer var START.	*/
					cstr = cur_decl->field1;
					while (*cstr != '-') cstr++;			/* Step to start position.		*/
					cstr++;						/* Step over the - symbol.		*/
					while (*cstr != '-') cstr++;			/* Step to length position.		*/
					cstr++;						/* Step over the - symbol.		*/
					while (strpos(cstr,"-") >= 0)			/* While var name has - in it, need to	*/
					{						/*  step to next one.			*/
						while (*cstr != '-') cstr++;		/* Step to next position.		*/
						cstr++;					/* Step over the - symbol.		*/
					}
					strcpy(len,cstr);				/* Should be at length position.	*/
					sprintf(cur_decl->length,"(%s)",len);		/* Set the length for declare.		*/
				}
			}
			else if (getop == 'L')
			{
				if (*cur_assign->start_pos == ' ') strcpy(cur_assign->start_pos,"1");
				strcpy(cur_assign->length,"(8)");
			}
			else	strcpy(cur_scn_fld->len,"(8)");
		}
	}
}

void makeint(char* equate1, char* name_buf, int* num_asn)
{
	char field_name[FLDLEN];
	char *str_val, *end_val;
	int sz;

	while (operand(*equate1) || number(*equate1)) equate1--;			/* Look back until no numeric or *+-/.,	*/
	equate1++;									/* Go to the first variable.		*/
	str_val = equate1;								/* Point it to the first number buffer.	*/
	while (operand(*equate1) || number(*equate1)) equate1++;
	end_val = equate1;								/* Point to next open value.		*/
	if (end_val) sz = end_val - str_val;						/* calculate size of field.		*/
	else	sz = 1;									/* Default to picture size.		*/
	strcpy(field_name,"&NUMBER-");							/* Load the number.			*/
	strcat(field_name,name_buf);							/* Load in the counter.			*/
	strncpy(cur_decl->value,str_val,sz);						/* Load the value.			*/
	cur_decl->value[sz] = '\0';							/* Null terminate it.			*/

	strcpy(cur_decl->field1,field_name);						/* Load the file name.			*/
	if (in_assign)
	{
		init_assign(num_asn);							/* point to next assign variable.	*/
		strcpy(cur_assign->field1,field_name);					/* Load the field name.			*/
		strcpy(cur_assign->type,"IL");
	}
	else if (in_run || in_print || in_submit)
	{
		init_using();								/* point to next assign variable.	*/
		strcpy(cur_using->var,field_name);					/* Load the file name.			*/
		strcpy(cur_using->type,"IL");
	}
	strcpy(cur_decl->type,"IL");							/* Load the current type.		*/
}

void get_next_line(void)										/* Read the next line for input source.	*/
{
	char *cstr;

	fgets(linein,STRBUFF,infile);
	num_lineins++;
	cstr = linein;
	setup_line(cstr);								/* Put linein into expected format.	*/
	aptr = linein;									/* Set the pointer to the current line.	*/
	next_ptr = aptr;
	lcnt = 0;									/* Init the looping field indicators.	*/
	captr = (char *)0;
}

void concat_var_prefix(char type)							/* Concatenate the variable prefix for	*/
											/* the current variable to cobline.	*/
{
	if (type == 'E') strcat(cobline,"EG-");		 				/* Load the External Global indicator.	*/
	else if (type == 'G') strcat(cobline,"PG-"); 					/* Load the Global indicator.		*/
	else if (type == 'L') strcat(cobline,"WS-");					/* Load the Literal indicator.		*/
	else if (type == 'R') strcat(cobline,"RC-"); 					/* Load the Return code indicator.	*/
	else if (type == 'F') strcat(cobline,"BF-"); 					/* Load the Built in Function indicator.*/
	else if (type == 'B') return;		 					/* No prefix for backwards reference.	*/
	else if (type == 'P') strcat(cobline,"WSA-N");					/* Load the proctran variable indicator.*/
	else strcat(cobline,"PL-");							/* Load the local indicator.		*/
}

int len_to_int(char *strlen)								/* Convert string length field to int.	*/
{
	char *cpos, slen[10];
	int i, len;

	cpos = strlen;									/* Now get length of var to adjust ccol.*/
	if (*cpos == '(') cpos++;							/* Step past open paren of field len.	*/
	i = 0;
	while (*cpos && *cpos != ')') slen[i++] = *cpos++;				/* Copy length to local var.		*/
	slen[i] = '\0';									/* Null terminate string len.		*/
	len = atoi(slen);
	return(len);
}

void setup_line(char* lptr)								/* Put linein into expectd format.	*/
{
	char *cstr;
	int qcnt, concat;

	cstr = lptr;									/* Set the local copy of the line ptr.	*/
	qcnt = 0;
	concat = 0;
	while (*cstr != '\0' && *cstr != LNFD && *cstr != '\r')
	{
		if (*cstr == '!' && *(cstr+1) == '!' )					/* Change all occurance of !! in the	*/
		{									/* current line to 2 spaces.		*/
			if (in_prompt && (qcnt % 2))					/* Test if !! is within the string.	*/
			{
				cstr += 2;
			}
			else
			{
				concat++;						/* Increment the concatenation count.	*/
				*cstr++ = ' ';
				*cstr++ = ' ';
			}
		}
		else if (*cstr == '"')							/* Change all occurances of " in the	*/
		{									/* current line to ' (single quote),	*/
			qcnt++;
			if (*(cstr+1) == '"')						/* unless two side by side then change	*/
			{								/* 1st to space and second to ' (single)*/
				*cstr++ = ' ';
			}
			*cstr++ = '\'';
		}
		else if (*cstr == '\'')							/* Test if quote within string or	*/
		{									/*  apostrophe.				*/
			if (qcnt > 0 && (letter(*(cstr+1)) || *(cstr+1) == '\''))
			{								/* Change to ~ for processing.		*/
				*cstr = '~';						/* If is a quote within a string,	*/
				cstr++;							/* Check if just apostrophe,		*/
                                if ( (*cstr == 'S' || *cstr == 's') && *(cstr+1) == '"')
				{
					/* Is apostrophe at end of line so continue.*/
				}
				else if (strncmp(cstr,"S ",2) && strncmp(cstr,"s ",2)	/*  else set paired ' to ~ also.	*/
					 && strncmp(cstr,"S.",2) && strncmp(cstr,"s.",2))
				{
					while (*cstr != '\'' && *cstr != LNFD && *cstr != '\r' && *cstr != '\0') cstr++;
					if (*cstr == '\'') *cstr = '~';
				}
			}
			else								/* else is regular quote around string.	*/
			{
				qcnt++;
				cstr++;
			}
		}
		else	cstr++;
	}
	if (concat)
	{
		if ( qcnt%concat || (in_assign && ( (qcnt/2) != concat) ) )
		{
			adj_quotes(lptr,qcnt);						/* Check if quotes are within string.	*/
		}
	}
	else if (qcnt > 2) adj_quotes(lptr,qcnt);					/* Check if quotes are within string.	*/
}

static void adj_quotes(char* lptr, int qcnt)						/* Change quotes within string to ".	*/
{
	char *spos;
	int cqcnt, match_sgl, match_dbl, ndx;

	qcnt--;										/* Set so doesn't change last one.	*/
	while (*lptr != '\'') lptr++;							/* Set up to point to first quote.	*/
	lptr++;										/* Step past first quote.		*/
	cqcnt = 1;
	match_sgl = 1;									/* Set because already past 1st single.	*/
	match_dbl = 0;									/* Set to don't need to match double.	*/
	while (cqcnt < qcnt)
	{
		if (*lptr == '\'')
		{
			cqcnt++;
			if (!match_sgl)							/* If starting a new string.		*/
			{
				match_sgl = 1;
			}
			else if (match_dbl)						/* If need to match double.		*/
			{
				*lptr = '"';						/* Set the matching quote.		*/
				match_dbl = 0;
			}
			else
			{
				spos = lptr;						/* Save position of quote.		*/
				lptr++;
				while (*lptr == ' ') lptr++;				/* Step to next char.			*/
				if ( !match_sgl )					/* If have matched all the single '.	*/
				{
					if (*lptr == '\0' || *lptr == LNFD || *lptr == '\r')  break;	/* At the end of the line.		*/
					else write_log(util,'I','R',"NOQUOTE","No matching quote.  Check procedure code.");
				}
				else
				{							/* Check if a keyword.			*/
					if ((0==strncmp(lptr,"THEN",4)) || (0==strncmp(lptr,"AND",3)) ||
				   	    (0==strncmp(lptr,"OR",2)) || (ndx = find_keyword(lptr,proc_keywords) >= 0) )
					{
						break;
  					}
				}

				if (*lptr != ',' && *lptr != '&')
				{
					if (in_prompt && *lptr == ';')			/* In PROMPT and end of string.		*/
					{
						while (*lptr != '\'') lptr++;		/* Step to next quote.			*/
						cqcnt++;
					}
					else
					{
						if (*lptr != '\'' && !match_dbl && !match_sgl)	/* If don't need to match double.*/
						{
							if (match_sgl) match_sgl = 0;
							else match_sgl = 1;
						}
						else
						{
							*spos = '"';			/* Replace so quote in string.		*/
							if (match_dbl) match_dbl = 0;
							else match_dbl = 1;
						}

						if (*lptr == '\'')
						{
							cqcnt++;			/* At a quote so increment.		*/
							if (!match_dbl)			/* If don't have match double.		*/
							{
								*spos = '"';		/* Replace so quote in string.		*/
								match_dbl = 1;
							}
							else if (match_dbl)		/* At position to match double.		*/
							{
								*lptr = '"';
								match_dbl = 0;
							}
							else
							{
								if (match_sgl) match_sgl = 0;
								else match_sgl = 1;
							}
						}
					}
				}
				else
				{
					while (*lptr != '\'') lptr++;			/* Step to next quote.			*/
					cqcnt++;
				}
			}
		}
		lptr++;
	} 
}

char *upper_string(char* str)									/* Convert a string to uppercase.	*/
{
	for (; *str; str++)
	{
		*str = toupper(*str);
	}
	return( str );
}


static void convert_to_dbl(void)							/* Convert ' within string to ".	*/
{
	char *laptr;
	int i, cnt;

	laptr = aptr;									/* Set the local pointer to position.	*/
	if (*laptr == '\'') *laptr = '"';						/* Change beginning quote to double.	*/
	cnt = 0;
	while (*laptr != ',' && *laptr != '\0' && *laptr != LNFD && *laptr != '\r') 			/* Count the single quotes.		*/
	{
		if (*laptr == '\'') cnt++;
		else if (*laptr == '~') *laptr = '"';
		laptr++;
	}
	if (cnt == 1)									/* If only one see if is an apostrophe.	*/
	{
		laptr = aptr;								/* Set ptr back to the beginning.	*/
		while (*laptr != '\'') laptr++;						/* Step to the '.			*/
		if (*(laptr+1) == 'S' || *(laptr+1) == 's') *laptr = '"';		/* Change apostrophe (quote) to double.	*/
	
		return;									/* No need to change any so return.	*/
	}
	laptr = aptr;									/* Set ptr back to the beginning.	*/
	i = 1;
	while (*laptr != ',' && *laptr != '\0' && *laptr != LNFD && *laptr != '\r')			/* Change ' to ".			*/
	{
		if (*laptr == '\'')
		{
			if (in_prompt)
			{
				char *tptr;

				tptr = laptr;
				tptr++;
				while (*tptr == ' ') tptr++;
				if (*tptr != ';') *laptr = '"';
			}
			else *laptr = '"';
			i++;
			if ( i == cnt) break;						/* Don't change the last one.		*/
		}
		laptr++;								/* Step to next char.			*/
	}
}

int is_backref(void)									/* Determine if field is backwards	*/
{											/*  referenced.				*/
	char * l_aptr;

	l_aptr = aptr;
	if (*l_aptr != '(') return (FALSE);

	l_aptr ++;
	if (*l_aptr == ' ') l_aptr++;

	if (*l_aptr == '&' || number(*l_aptr)) return (FALSE);
	else return(TRUE);
}

void write_log(facil,sever,rw,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)			/* write a log line to the current log	*/
char *facil,sever,*mess, rw;								/* facility, severity, message, read/write*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7;						/* The format and parms for the text	*/
{

	if (logging)
	{
		if (sever != ' ')							/* if severity set to space, skip	*/
		{
			if (lncnt_type)							/* In reading in file.			*/
			{								/* write facility, severity, message	*/
				fprintf(logfile,"%%%s-%c-%s (%c:%d) ",facil,sever,mess,rw,num_lineins);
			}
			else	fprintf(logfile,"%%%s-%c-%s (%c:%d) ",facil,sever,mess,rw,num_outlines);
		}
		fprintf(logfile,lform,p0,p1,p2,p3,p4,p5,p6,p7);				/* if logging, write it			*/
		fprintf(logfile,"\n");
	}
	if (sever != 'I' && sever != ' ' && !logging)					/* if not informational only, report it	*/
	{
		if (sever != ' ')							/* if severity set to space, skip	*/
		{
			if (lncnt_type)							/* In reading in file.			*/
			{								/* write facility, severity, message	*/
				fprintf(logfile,"%%%s-%c-%s (%c:%d) ",facil,sever,mess,rw,num_lineins);
			}
			else	fprintf(logfile,"%%%s-%c-%s (%c:%d) ",facil,sever,mess,rw,num_outlines);
		}
		printf(lform,p0,p1,p2,p3,p4,p5,p6,p7);					/* and the text				*/
		printf("\n");
	}
}

/*
**	History:
**	$Log: ptutils.c,v $
**	Revision 1.9  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.8  2003/01/28 20:08:56  gsl
**	Move write_log() to end to avoid proto-typing warnings
**	
**	Revision 1.7  1999/09/13 19:50:41  gsl
**	fix "==" vs. "="
**	
**	Revision 1.6  1997-04-21 11:24:50-04  scass
**	Corrected copyright.
**
**	Revision 1.5  1996-09-12 19:18:23-04  gsl
**	Fix prototypes
**
**
**
*/
