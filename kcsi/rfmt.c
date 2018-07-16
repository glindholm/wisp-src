static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*---- 
Copyright (c) 1989-93 King Computer Services, Inc. 
All rights reserved. 
------*/
/*-----
Numeric formatting.
------*/
/*----
Mods:
 Jan 24, 1992
	error in position dollar sign in non-zero suppressed fields
	corrected by adding --dollar after call to zero_supp, and
	inserting the dollar at *dollar instead of dollar - 1
 Feb 27, 1990
	Blank pad logic in format_alpha causing errors in alpha fields that
	contain embedded nuls. Corrected
 Apr 14, 1990
	Changed zero suppress logic. Previously 5Z suppressed first 5 chars
	now 5Z (as with Wang) suppresses all but last 5.
	The new routine uses the original name zero_supp.
	uses the
------*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "dtype.h"
#include "rptprm.h"
#include "rptsrt.h"
#include "rptglb.h"
#include "iocode.h"
#include "kcsio.h"
#include "shrthand.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)rfmt.c	1.12 7/17/94";

/*----
Non-int functions
------*/
/*----
Shorthand
------*/
#define	FIELD_ADDRESS(x)	(x->_new->_base + x->_new->_pos)
#define	ITS_NUMERIC(x)		(x->_new->_type & IS_NUM)


static void do_inserts(RPT_RFL *rfl,char *d,int zerochar);
static void do_commas(char *d,char *e);
static void delchar(char* d);
static int is_zero(char *str);

static char *format_alpha(RPT_RFL *rfl,char *d);
static char *format_numeric(RPT_RFL *rfl,char *d,int editing);
static char *zero_supp(char* dest, int ch,int count,int editing);


static int myisblank(char *str);


/*----
Testing logic.
------*/
#ifdef KFORMAT_TEST
#include "rptcob.h"
main()
{
	int i,ch;
	RPT_RFL x;
	DTYPE l_new;

	char input[132];
	char format[132];
	static char cr[]= "CR";
	static char db[]= "DB";
	static char minus[]="-";
	static char dol[]="$$";
	char *sptr;
	char buf[101];
	int more_input;


	memset(format,0,132);
	memset(&l_new,0,sizeof(DTYPE));
	x._new = &l_new;
	x._new->_base = input;	
	x._new->_pos = 0;

	printf("1 Numeric 2 Alpha\n");
	gets(buf);
	if(buf[0] == '1')
		x._new->_type = APCK;
	else
		x._new->_type = ACHR;
	printf("Data\n");
	gets(buf);
	input[0] = 0;
	sscanf(buf,"%s",input);
	x._new->_len = strlen(input);
	printf("Edited size\n");
	x._ext_size = 0;
	gets(buf);
	sscanf(buf,"%d",&x._ext_size);
	printf("Real decimals\n");
	gets(buf);
	ch = buf[0];
	ch -= '0';
	if((ch < 0) || (ch > 9))
		ch = 0;
	x._new->_dec = ch;
	printf("Edit decimals\n");
	gets(buf);
	ch = buf[0];
	ch -= '0';
	if((ch < 0) || (ch > 9))
		ch = 0;
	x._dec_carry = ch;
	printf("Sign 0 = None 1 = 'CR' 2 = 'DB' 9 = '-'\n");
	gets(buf);
	ch = buf[0];
	ch -= '0';
	if((ch < 0) || (( ch > 2) && ( ch != 9)))
		ch = 0;
	x._sign_control = ch;
	printf("Zero char (Z , * or none)\n");
	gets(buf);
	ch = buf[0];
	ch = toupper(ch);
	if((ch == 'Z')	|| (ch == '*'))
		x._zero_char = ch;
	else
		x._zero_char = 0;
	if(ch)
		{
		printf("Zero count\n");
		gets(buf);
		ch = buf[0];
		ch -= '0';
		if((ch < 0) ||(ch > 9))
			ch = 0;
		x._zero_count = ch;
		}
	printf("Commas 0 or 1?\n");
	gets(buf);
	ch = buf[0];
	ch -= '0';
	if((ch < 0) || (ch > 1))
		ch = 0;
	x._comma = ch;

	printf("Dollars 0, 1($$) or 2($9)?\n");
	gets(buf);
	ch = buf[0];
	ch -= '0';
	if((ch < 0) || (ch > 2))
		ch = 0;
	x._dollar_sign = ch;

	for( i = 0,more_input = 1; i < 3 ; ++ i)
		{
		if(more_input)
			{
			printf("Insert count?\n");
			gets(buf);
			ch = buf[0];
			ch -= '0';
			}
		else
			{
			ch = 0;
			}
		if((ch < 0) || (ch > 9))
			ch = 0;
		more_input = x._insert_count[i] = ch;
		if (ch > 0)
			{
			printf("Insert char?\n");
			gets(buf);
			ch = buf[0];
			x._insert_char[i] = ch;
			}
		else
			{
			x._insert_char[i] = 0;
			}
		}
	sptr=minus;
	if(x._sign_control == 0)
		++sptr;
	else
	if(x._sign_control == 1)
		sptr =cr;
	else
	if(x._sign_control == 2)
		sptr =db;

	printf("Data            = %s \n",input);
	printf("Edited Length   = %d\n",x._ext_size);
	printf("Decimals        = .%d\n",x._new->_dec);
	printf("Edited Decimals = .%d\n",x._dec_carry);
	
	printf("Sign            = %s\n",sptr);
	printf("Zeroes          = %c%d\n",x._zero_char,x._zero_count);
	printf("Commas          = %s\n",(x._comma)?",":"");
	for(i = 0; i < 3; ++i)
		printf("Insert          = %c%d\n",
			x._insert_char[i],x._insert_count[i]);
	sptr = dol;
	if(x._dollar_sign == 0)
		sptr +=2;
	else
	if(x._dollar_sign == 2)
		++sptr;
	printf("Dollar          = %s\n",sptr);
	rpt_format_a_field(&x,format,0);
	printf("<%s>\n",format);
	
		
}

cvt_one_rfl(d,s,record)
DTYPE *d,*s;
char *record;
{

	cvt_record(d,s,record);

		
	if(KCSI_fieldeq(record,RFL_SIGN_CONTROL,"CR"))
		wrk_rfl._sign_control = 1;
	else
	if(KCSI_fieldeq(record,RFL_SIGN_CONTROL,"DB"))
		wrk_rfl._sign_control = 2;

	if(KCSI_fieldeq(record,RFL_DEC_CARRY," "))
		wrk_rfl._dec_carry = -1;

	if(wrk_rfl._e._origin == 3)
		wrk_rfl._old->_pos = -1;

}
#endif /* KFORMAT_TEST */
/*----
KFORMAT is a COBOL accessible routine that does formatting.
It is passed a PIC 9 with decimal count, a PIC X(35) to receive
the edit right justified, and a PICX(85) RFL RECORD.
------*/
void KFORMAT(char* dec,char* rcvr,char* rflrec)
{
	char work_field[36];
	static char data_field[]="-999999999999999";
	DTYPE temp,old;

	memset(&temp,0,sizeof(DTYPE));
	memset(&old,0,sizeof(DTYPE));
	memset(work_field,0,36);
	wrk_rfl._new = &temp;
	wrk_rfl._old = &old;
	cvt_one_rfl(rpt_rfl_dest,rpt_rfl_src,rflrec);
	wrk_rfl._new->_dec = kcsi_atoilen(dec,1);
	wrk_rfl._new->_base = data_field;
	wrk_rfl._new->_pos = 0;
	wrk_rfl._new->_len = 16;
	wrk_rfl._new->_type = BZON;
	rpt_format_a_field(&wrk_rfl,work_field,1);
	while(strlen(work_field) < 35)
		KCSI_inschar(work_field,' ');
	memcpy(rcvr,work_field,35);
	
}

/*----
The parts of an RFL needed for formatting are
	_new->_type,
	_new->_len
	_new->_dec,
	_dec_carry,
	_ext_size,
	_sign_control,
	_zero_char,
	_zero_count,
	_comma,
	_dollar_sign
	_insert_char	[ 0 - 2],
	_insert_count	[ 0 - 2 ],
------*/
	

/*----
Format a field as alpha or numeric, and then do the inserts.
------*/
char *rpt_format_a_field(RPT_RFL *rfl, char *d, int code)
{
	if(ITS_NUMERIC(rfl))
		format_numeric(rfl,d,code);
	else
		format_alpha(rfl,d);
	return(d);
}

static void lmg_dummy_edit_case(start,rfl)
char *start;
RPT_RFL *rfl;
{
	kcsitrace(4,"LISTMGMT","EDITCASE", "Case editing requested -- NOT IMPLEMENTED");
}

static void (*lmg_edit_case)() = lmg_dummy_edit_case;

void KCSI_set_lmg_edit_case(void (*func)())
{
	if(func)
		lmg_edit_case = func;
}


static void  lmg_dummy_edit_spaces(char *start,RPT_RFL *rfl)
{
	kcsitrace(4,"LISTMGMT","EDITSPACES", "Space editing requested -- NOT IMPLEMENTED");
}

static void (*lmg_edit_spaces)(char *,RPT_RFL *) = lmg_dummy_edit_spaces;

void KCSI_set_lmg_edit_spaces(void (*func)(char *,RPT_RFL *))
{
	if(func)
		lmg_edit_spaces = func;
}


static int count_inserts(RPT_RFL *rfl)
{
	int i,count;
	for(count = 0, i = 0; i < 3; ++i)
		{
		if(rfl->_insert_char[i])
			++count;
		}
	return(count);
	
}

/*----
Alpha formatting is much easier than numeric.
------*/
static char *format_alpha(RPT_RFL *rfl,char *d)
{
	int ext_size,inserts,shortest,ch,len;
	char print_value[133];
	char *start,*end,*tstart;

/* How many inserts will be made and reduce the ext_size by that much*/
	inserts = count_inserts(rfl);
	ext_size = rfl->_ext_size - inserts;

/*
 * Copy the shorter of the two fields either the original or the edit
 */
	shortest = min(ext_size,rfl->_new->_len);

	start = print_value;
	memcpy(start,FIELD_ADDRESS(rfl),shortest);
	end = &print_value[shortest];

	*end = 0;
/*
 * Replace non printing characters and NULLS with spaces. Added Feb 27 90
 */
	tstart = start;
	while(tstart < end)
		{
		ch = *tstart;
		if( (ch < ' ') || (ch > 126))
			ch = ' ';
		*tstart++ = ch;
		}

/*
 * Pad the field out until it is blank filled at the tail.
 */
	while((int)strlen(start) < ext_size)
		{
		KCSI_inschar(end++,' ');
		}

/* Do the inserts */

	if(inserts)
		do_inserts(rfl,start,0);
/*
 * Finally plunk it at the destination field.
 */
	len = rfl->_ext_size;
	if(*rpt_caller < ':')
		{
		(*lmg_edit_spaces)(start,rfl);
		(*lmg_edit_case)(start,rfl);
		len = strlen(start);
		}
	if(*rpt_caller < ':')
		strcpy(d,start);
	else
		memcpy(d,start,len);
	return(d);

}

static char *format_numeric(RPT_RFL *rfl,char *d,int editing)
{
	double fl;
	char *start,*end,*dollar,*decpos;
	char print_value[133];
	char work_v[133];
	int dec,ddec,sign,zerochar,inserts,ext_size,len;

/*
 * Get the actual decimal and decimals to be printed. If the requested value
 * is minus 1 this is equivalent to the WANG blank field and means display all
 * decimal positions but without a decimal point. This is handled by forcing
 * both decimal counts to zero.
 */
	dec = rfl->_new->_dec;
	ddec = rfl->_dec_carry;
	if(ddec == -1)
		ddec = dec = 0;

/* 
 * First step is to adjust the actual value against the intended number
 * of edited decimals
 */
	fl = 0;
	sscanf(FIELD_ADDRESS(rfl),"%16lf",&fl);

/* even it out to a decimal-less field */
	while(ddec > dec)
		{
		fl *= 10;
		++dec;
		}
	while(ddec < dec)
		{
		fl /= 10;
		--dec;
		}
/* Round and set it in the work area */
	sprintf(work_v,"%+016.0lf",fl);
/* Truncate if it got too big leaving the sign in position 0*/
	while(strlen(work_v) > 16)
		strcpy(&work_v[1],&work_v[2]);

/* How many inserts will be made and reduce the ext_size by that much*/
	inserts = count_inserts(rfl);
	ext_size = rfl->_ext_size - inserts;
	len = rfl->_new->_len;

/* Save the sign */
	start = work_v;
	if(*start == '-')
		sign = -1;
	else
		sign = 1;
	if((*start == '-') || (*start == '+'))
		{
		++start;
		--len;
		}

/*
 * Copy the data (without sign) into a work field
 * and set up a pointers to the start and end of the field.
 */
	memcpy(print_value,start,len);
/* This is redundant, but included for debugging */
	print_value[len] = 0;

	start = print_value;
	end = start + len;
	*end = 0;
	end = start + strlen(start);	/*In case the field was really shorter*/


/* Add any needed trailing sign or space for the implied positive*/
	if(sign < 0)
		{
		switch(rfl->_sign_control)
			{
			case 1:
				strcpy(end,"CR");
				break;
			case 2:
				strcpy(end,"DB");
				break;
			case 9:
				strcpy(end,"-");
				break;
			default:
				break;
			}
		}
	else
		{
		switch(rfl->_sign_control)
			{
			case 1:
			case 2:
				strcpy(end,"  ");
				break;
			case 9:
				strcpy(end," ");
				break;
			}
		}

/*
 * end is left pointing at the character before the sign (or last char
 * if not signed). Add any requested decimals.
 * First match requested to displayed since the requested decimals
 * may be more or less than those actually in the field. Insert or
 * delete zeroes until requested and actual match. After inserts or
 * deletes, end still points to the spot at the end of the digits.
 * move back by the amount requested for decimals if any and insert
 * a decimal. Only one of the two while loops will be executed.
 * (or neither).
 */
	end -= ddec;
	if(ddec)
		KCSI_inschar(end,'.');
/*
 * Pad the left with zeroes until the correct string size is
 * reached.
 */
	while((int)strlen(start) < ext_size)
		{
		KCSI_inschar(start,'0');
		++end;
		}
/*
 * Point to the decimal position and move end out to the actual
 * end of the field. End is not used further, but kept around
 * for future mods.
 */
	decpos = end;
	while(*end)
		++end;

/*
 * Every thing from decpos to the end of the field is solid and all further
 * work is done left of the decimal.
 */

/*
 * Insert commas as requested.
 */
	if(rfl->_comma)
		do_commas(start,decpos);
/*
 * Now bring the field back down to the correct external size. This could
 * leave the field starting on a comma which should be blanked if it
 * exists.
 */
	while((int)strlen(start) > ext_size)
		delchar(start);
	if(*start == ',')
		*start = ' ';

/*
 * If there is going to be a dollar sign, then at least the
 * first character will be destroyed. And the next character
 * if it was supposed to be a comma.
 */
	if(rfl->_dollar_sign)
		{
		*start = ' ';
		if(*(start + 1) == ',')
			*(start + 1) = ' ';
		}
/*
 * Zero suppression is done in two modes based on the passed editing. If
 * editing is zero then live data is being suppressed other wise an edit field
 * is being suppressed. The field is zeroed (see notes on that function) and
 * returns a pointer to last suppressed field. ( or NULL if none )
 */

/*
 * The suppression character is either ' ' or '*' for live data or
 * '*' or 'Z' or '$' for edit data. So figure it out.
 * If not editing and the user requests
 */
	dollar = start;
	zerochar = 0;
	if(rfl->_zero_char > ' ')
		{
		zerochar = ' ';
		if(rfl->_zero_char == '*')
			zerochar = '*';
		if(editing)
			{
			zerochar = rfl->_zero_char;
			if ((rfl->_dollar_sign == 1) && (zerochar == ' '))
				zerochar = '$';
			}
/* And do it */
		dollar = zero_supp(start,zerochar,rfl->_zero_count,editing);
		--dollar;
		}

/*
 * On return, dollar points to the last character that was suppressed
 * which could be the end of the field. If a floating dollar sign is used
 * then one back from this position is where the floating '$' goes.
 * If the whole field is suppressed (blank) then no $ is added.
 */
	if((editing) || (*dollar))
		{
		if(myisblank(start))
			;
		else
			{
			if(rfl->_dollar_sign == 2)
				*start = '$';
			else
			if(rfl->_dollar_sign == 1)
/* Previously *(dollar - 1) = '$'; */
				*dollar = '$';
			}
		}
/*
 * After all this stuff is done, the field may still have insert edits
 * to add. Inserts are tricky on numerics. The resulting field is supposed
 * to equal the ext_len. They should skip the dollar sign but include other
 * characters. This makes an efffort to handle floating dollars
 * and suppressed zeroes by using a suppression character instead of the
 * insertion where appropriate.
 */
	if(inserts)
		do_inserts(rfl,start + ((*start == '$') ? 1 : 0),zerochar);
/*
 * For LISTMGMT, all leading blanks are disposed of
 */
	len = rfl->_ext_size;
	if(*rpt_caller < ':')
		{
		rfl->lmg_supp_lspace = 1;
		(*lmg_edit_spaces)(start,rfl);
		len = strlen(start);
		}
/*
 * Finally plunk it at the destination field.
 */
	if(*rpt_caller < ':')
		strcpy(d,start);
	else
		memcpy(d,start,len);
	return(d);
}
/*----
Works in two ways. In edit mode a field of nines is expected and
is flagged with Z's or another edit character to indicate the suppression.
in the non-edit version anactual number is correctly suppressed.
dest is the target field.
ch is the suppression character.
count is the number of digits at the edn of the field to protect
edit ia 1 or zero to indicate whether in edit mode.
------*/

static char *zero_supp(char* dest, int ch,int count,int editing)
{
    int len, suppchar,isz;
    char *end,*dpoint;

/*Check if field is all zeroes */
    isz = is_zero(dest);
    dpoint = NULL;

    len = strlen(dest);
/* If the string is shorter than the count, nothing is suppressed */
    if (len <= count)
        return(dest);
/* If the suppress character is nothing, nothing is suppressed */
    if (!ch)
        return(dest);

/* Suppress zeroes or nines based on the editing flag */
    if(editing)
        suppchar = '9';
    else
        suppchar = '0';

/* point to the last character */
    end = dest + len - 1;
/* Step backward throughg digits until count is exhausted (or the field)*/
    while(count)
        {
        if (end < dest)
            break;
        if( isdigit(*end) )
            --count;
        --end;
        }
    if(end < dest)
        return(dest);
/* Set end to point to the first protected digit ignoring edit chars*/
    ++end;
    while(! isdigit(*end))
        {
        if (!(*end))
            break;
        ++end;
        }
/*
 * Everything up to but not including (*end) can be suppressed except '.'
 */
    while(dest < end)
        {
	if(*dest != '.')
            {
            if(!editing)
                {
                if( (*dest > '0') && (*dest <= '9'))
                    break;
                }
/*
	    *dest = ch;
*/
	    if(!editing)
	    	*dest = ch;
	    else
		{
		if(isdigit(*dest))
			*dest = ch;
		}
            }
	else
	    {
	    dpoint = dest;
	    }
        ++dest;
        }
/* 
 * If the field was zeroes, and we encountered a decimal point
 * and the suppression character is not 'Z', then the decimal
 * is also suppressed
 */
    if( (isz) &&
        (dpoint) )
	{
	if(ch != 'Z')
		{
		*dpoint = ch;
		}
	}
    return(dest);

}

/*----
Return true if all digits are zeroes
-------*/
static int is_zero(char *str)
{
	int rc;

	rc = 1;
	while(*str)
		{
		if(isdigit(*str))
			{
			if(*str != '0')
				{
				rc = 0;
				break;
				}
			}
		++str;
		}
	return(rc);

}


/*----
e points to the decimal and d to the start of the field.
Insert commas every third character moving backwards.
------*/
static void do_commas(char *d,char *e)
{
	for(e -= 3;e > d; e -= 3)
		KCSI_inschar(e,',');
}
/*----
If the position for the insert is a zero suppressed character, (not 'Z')
then insert the zero suppress char instead of the actual char.
------*/
static void do_one_insert(d,c,count,zerochar)
char *d;
int c,count,zerochar;
{
	if((!count) || (!c))
		return;

	d += count;
	KCSI_inschar(d,(zerochar == 'Z')?c:(*d == zerochar)?zerochar:c);
}

static void do_inserts(RPT_RFL *rfl,char *d,int zerochar)
{
	int i;
	int a,b;
	int *achar,*count;

	achar = &rfl->_insert_char[0];
	count = &rfl->_insert_count[0];

/*
 * two pass sort to get longest insert first
 */
	for(a = 0; a < 2; ++a)
		{
		for(i = 0; i < 2; ++ i)
			{
			if(count[i+1] > count[i])
				{
				b = count[i];
				count[i] = count[i+1];
				count[i+1] = b;
				b = achar[i];
				achar[i] = achar[i+1];
				achar[i+1] = b;
				}
			}
		}
	for(i = 0; i < 3; ++i)
		{
		do_one_insert(d,achar[i],count[i],zerochar);
		}
}


/*----
Insert a character and ripple down to the end
------*/
void KCSI_inschar(char* d, int c)
{
	int x;

	x = c;
	while(x)
		{
		c=x;
		x = *d;
		*d=c;
		++d;
		}
	*d = 0;
}
/*----
Delete a character and ripple down to the end. If we are at the end
then just back up one.
------*/
static void delchar(char* d)
{
	char *s;

	if(!*d)
		*(--d) = 0;
	else
		{
		s = d;
		++s;
		strcpy(d,s);
		}
}

/*----
Fill out to 132 characters.
------*/
void KCSI_strfil(char* s)
{
	int len;

	s[132] = 0;
	len = strlen(s);
	if(len < 132)
		memset(&s[len],' ',132 - len);
}

/*----
Copy src to dest for size. If size is not exhausted then add blanks
Return ptr to point after.
------*/
char *KCSI_cpsize(d,s,len)
char *d,*s;
int len;
{
	while(len--)
		{
		if(*s)
			*d++ = *s++;
		else
			*d++ = ' ';
		}
	return(d);
}


/*----
Return true if the entire field is spaces
------*/
static int myisblank(char *str)
{
	while(*str)
		{
		if(*str != ' ')
			return(0);
		++str;
		}
	return(1);
}
/*
**	History:
**	$Log: rfmt.c,v $
**	Revision 1.6.2.1  2002/11/12 15:56:33  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.10  2002/10/24 14:20:35  gsl
**	Make globals unique
**	
**	Revision 1.9  2002/10/17 21:22:43  gsl
**	cleanup
**	
**	Revision 1.8  2002/10/17 17:56:19  gsl
**	Rename variables new to l_new
**	
**	Revision 1.7  2002/07/25 15:20:25  gsl
**	Globals
**	
**	Revision 1.6  2002/04/22 15:40:21  gsl
**	Change crid_error_trace to kcsitrace
**	
**	Revision 1.5  1997-10-02 15:34:09-04  gsl
**	fix warnings
**
**	Revision 1.4  1997-10-02 09:44:58-04  gsl
**	fix warnings
**
**	Revision 1.3  1996-09-17 19:45:48-04  gsl
**	drcs update
**
**
**
*/
