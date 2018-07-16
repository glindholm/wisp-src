			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include <time.h>
#include "video.h"									/* Include the video database.		*/
#include "vform.h"									/* Include the view database.		*/
#include "vintdef.h"
#include "vplus.h"
#include "vlocal.h"
#include "vdata.h"

static char buff[128];									/* Working buffer.			*/
static char *states = {"ALAKAZARCACZCOCTDEDCFLGAGUHIIDILINIAKSKYLAMEMDMAMIMNMSMOMTNENVNHNJNMNYNCNDOHOKORPAPRRISCSDTNTXUTVTVAVIWAWVWIWY"};

static int apply_field_edits();								/* Function prototypes.			*/
static int check_empty_fields();
static int check_data_types();
static int check_table();
static int fcomp();
static int blank();
static int check_num();
static int digit();
float numval();
int onbyname();
int vfblank();
int vtoday();
static int name_match();
static int matched();
static int simple_conditional();
static int simple_edit();
static int truecond();
static int inrange();
static int iftrue();
static int intdate();
static int string_compare();
static int integer_compare();
static void dshuf();
static void trim();
static void usedit();
static void jledit();
void jredit();
static void jcedit();
static void vdoedits();
static void dofill();
static void dostrip();
static void doset();
static void dorem();
static void set_local_error();
static int alphanum();
static int minlength();

extern char *vgetproc();

#define DQ '\042'									/* Define a double quote.		*/
#define SQ '\047'									/* Define a single quote.		*/

#define CODE_GT 1									/* Conditional comparison codes.	*/
#define CODE_GE 2
#define CODE_LT 3
#define CODE_LE 4
#define CODE_EQ 5
#define CODE_NE 6

/*						Subroutine entry point.								*/

void VINITFORM(comarea) struct vplus_comarea *comarea;
{
	vdoedits(comarea,0);
}

void VFIELDEDITS(comarea) struct vplus_comarea *comarea;
{
	vdoedits(comarea,1);
}

void VFINISHFORM(comarea) struct vplus_comarea *comarea;
{
	vdoedits(comarea,2);
}

static void vdoedits(comarea,level) struct vplus_comarea *comarea; int level;		/* Apply edits.				*/
{
	int i,j;

	comarea->numerrs = 0;								/* Assume no errors.			*/
	comarea->cstatus = 0;

	for (i = 0; i < vformcurrent->fields; i++)					/* Check every field.			*/
	{
		vformcurrent->field[i].error_code = 0;					/* Assume no errors.			*/
	}

	if (level == 1)
	{
		check_empty_fields(comarea);						/* Check all empty fields.		*/
		if (comarea->numerrs) goto setx;					/* Exit if set.				*/
		check_data_types(comarea);						/* Check if data type valid.		*/
		if (comarea->numerrs) goto setx;					/* Exit if set.				*/
	}

	apply_field_edits(comarea,level);						/* Do the non-error edits.		*/

setx:	for (i = 0; i < vformcurrent->fields; i++)					/* Check every field.			*/
	{
		if (vformcurrent->field[i].error_code)					/* Is this the first error?		*/
		{
			comarea->cstatus = vformcurrent->field[i].error_code;		/* Yes, then remember it.		*/
			return;								/* We're done.				*/
		}
	}
}

static int apply_field_edits(comarea,level) struct vplus_comarea *comarea; int level;
{
	int i,j,k;										/* Working integers.		*/
	int lin,len,typ,dat,dpl;
	int errs;
	int iflevel;										/* Conditional level.		*/
	int noelse;										/* Scan to the else.		*/
	char *dp;										/* Data pointer.		*/
	char *pp;										/* Process pointer.		*/

	errs = 0;										/* Assume the first edit error.	*/

	for (dp = vform_data_buffer, i = 0; (i < vformcurrent->fields) && !errs; dp = dp + len, i++)
	{
		if      (level == 0) lin = vformcurrent->field[i].init_processing_lines;	/* Set the lines.		*/
		else if (level == 1) lin = vformcurrent->field[i].field_processing_lines;
		else if (level == 2) lin = vformcurrent->field[i].finish_processing_lines;

		len = vformcurrent->field[i].length;						/* Set the length.		*/
		typ = vformcurrent->field[i].type;
		dat = vformcurrent->field[i].datatype;
		dpl = vformcurrent->field[i].decimal_places;

		if ((level != 1) || (typ & (FIELD_REQUIRED | FIELD_PROCESSED)) || ((typ & FIELD_OPTIONAL) && !blank(dp,len)))
		{
			iflevel = 0;								/* No if processing yet.	*/
			if (lin) pp = vgetproc(current_form,i,level);				/* Yes so get first proc line.	*/

			for (j = 0; j < lin; pp = (pp+strlen(pp)+1), j++)			/* Loop through each edit.	*/
			{
new_level:			if (iflevel)							/* Are we processing an if?	*/
				{
					if (!memcmp("ELSE",(pp+((iflevel-1)*3)),4))		/* Is this else at this level.	*/
					{
						while (j < lin-1) 					/* All lines done?	*/
						{
							pp = pp + strlen(pp) + 1; j++;			/* Next line.		*/
							if (!blank(pp,(iflevel*3)))			/* End of the if?	*/
							{
								iflevel--;				/* Down one level.	*/
								goto new_level;				/* Now process.		*/
							}
						}
						goto loop_end;					/* We've run out of lines.	*/
					}

					else if (!blank(pp,(iflevel*3))) 			/* End of if with no else?	*/
					{
						iflevel--;					/* Down one if level.		*/
						goto new_level;					/* Reprocess this line.		*/
					}

					else pp = pp + (iflevel * 3);				/* Bias for processing.		*/
				}

				if (simple_edit(dp,len,pp));						/* Do simple edits.	*/

				else if (k = simple_conditional(pp))					/* A simple condition?	*/
				{
					if (!truecond(k,dp,len,dat,pp)) set_local_error(comarea,i,&errs);
				}

				else if (!memcmp("FILL ",pp,5))	dofill(dp,len,pp);			/* Fill processing.	*/

				else if (!memcmp("STRIP ",pp,6)) dostrip(dp,len,pp);			/* Strip processing.	*/

				else if (!memcmp("SET ",pp,4)) doset(dp,len,dat,dpl,pp);		/* Set to a value.	*/

				else if (!memcmp("MINLEN ",pp,7))					/* Check min length?	*/
				{
					if (!minlength(dp,len,pp)) set_local_error(comarea,i,&errs);	/* Is it long enough?	*/
				}

				else if (!memcmp("MATCH ",pp,6)) 					/* Match processing.	*/
				{
					if (!matched(dp,len,pp)) set_local_error(comarea,i,&errs);	/* Does it match.	*/
				}

				else if (!memcmp("IN ",pp,3))						/* Range table check?	*/
				{
					if (!inrange(dp,len,pp)) set_local_error(comarea,i,&errs);	/* value in range?	*/
				}

				else if (!memcmp("IF ",pp,3))						/* Conditional?		*/
				{
					if (iftrue(dp,len,dat,pp)) iflevel++;				/* Is the if true?	*/
					else								/* No then find else.	*/
					{
						noelse = TRUE;						/* But not found yet.	*/
						while (noelse && (j < lin-1))				/* No, look for else.	*/
						{
							pp = pp + strlen(pp) + 1;			/* Next line.		*/
							j++;
							pp = pp + (iflevel * 3);			/* Start of the level.	*/

							if (!memcmp("ELSE",pp,4))			/* Is this the else?	*/
							{
								iflevel++;				/* Yes, do the else.	*/
								noelse = FALSE;				/* Found an else.	*/
							}
							else if (!blank(pp,3))				/* End of the if?	*/
							{
								while (*pp != CHAR_NULL) pp--;		/* Go back one line.	*/
								j--;
								noelse = FALSE;				/* Logical else found.	*/
							}
						}
					}
				}

				else vre_window("VFIELDEDITS: Unsupported field edit %c%c%c%c encountered.",*pp,*(pp+1),*(pp+2),*(pp+3));
loop_end:;
			}
		}
	}
}

static int check_empty_fields(comarea) struct vplus_comarea *comarea;
{
	int i,j;
	char *dp;

	for (i = 0, dp = vform_data_buffer; i < vformcurrent->fields; dp = dp + vformcurrent->field[i].length, i++)
	{
		if (vformcurrent->field[i].type & FIELD_REQUIRED)				/* Is this a required field?	*/
		{
			if (blank(dp,vformcurrent->field[i].length)) 				/* Is the field blank?		*/
			{
				comarea->numerrs++;						/* Count the error.		*/
				vformcurrent->field[i].error_code = REQUIRED_FIELD_EMPTY;	/* Remember for forms.		*/
			}
		}
	}
}

static int check_data_types(comarea) struct vplus_comarea *comarea;
{
	int i,j,k;
	int typ,dat,len,dpl;
	int okdata;
	char *dp, c;

	for (i = 0, dp = vform_data_buffer; i < vformcurrent->fields; dp = dp + vformcurrent->field[i].length, i++)
	{
		typ = vformcurrent->field[i].type;						/* Make easy to work with copy.	*/
		dat = vformcurrent->field[i].datatype;
		len = vformcurrent->field[i].length;
		dpl = vformcurrent->field[i].decimal_places;
		
		if ((typ != FIELD_DISPLAY_ONLY) && (dat != FIELD_DATA_CHAR) && !blank(dp,len))		/* Check the data?	*/
		{
			if (dat == FIELD_DATA_DIG)							/* Must it be digit?	*/
			{
				okdata = check_num(dp,len);						/* Check valid number.	*/
				if (!okdata)								/* Bad data in field.	*/
				{
					comarea->numerrs++;						/* Count the error.	*/
					vformcurrent->field[i].error_code = FIELD_MUST_BE_DIG;		/* Remember for forms.	*/
				}
			}

			else if (dat == FIELD_DATA_IMP)							/* Must it be a digit?	*/
			{
				okdata = check_num(dp,len);						/* Check valid number.	*/
				if (okdata)								/* Apply the edits.	*/
				{
					vre_window("VFIELDEDITS: Data ok but %d implied decimal not applied. Continuing...",dpl);
				}
				else									/* Bad data in field.	*/
				{
					comarea->numerrs++;						/* Count the error.	*/
					vformcurrent->field[i].error_code = FIELD_MUST_BE_IMPN;		/* Remember for forms.	*/
				}
			}

			else if (dat == FIELD_DATA_NUM)							/* Must it be a digit?	*/
			{
				int ec = FIELD_MUST_BE_NUM;						/* Assume plain NUM.	*/

				for (j = 0, okdata = FALSE; (j < len) && !okdata; j++)			/* FIND 1 good char.	*/
				{
					if ((*(dp+j) >= '0') &&  (*(dp+j) <= '9')) okdata = TRUE;	/* A valid digit?	*/
				}

				for (j = 0; (j < len) && okdata; j++) 					/* Find a bad char?	*/
				{
					c = *(dp+j);							/* Make a working copy.	*/
					if (((c >= '0') && (c <= '9')) || ((c == ' ') ||
					     (c == '-') || (c == '+')  ||  (c == '.') || (c == '.')));
					else okdata = FALSE;						/* A valid digit?	*/
				}

				if (okdata && (dpl != -1))						/* Apply the edits?	*/
				{
					ec = FIELD_MUST_BE_NUMN+dpl;					/* Decimal places.	*/
					k = get_places(dp,len);						/* Get the places.	*/
					if ((k == -1) && (dpl == 0));					/* Zero is ok.		*/
					else if (k > dpl) okdata = FALSE;				/* Entered must be >=	*/
				}
				if (!okdata)
				{
					comarea->numerrs++;						/* Count the error.	*/
					vformcurrent->field[i].error_code = ec;				/* Remember for forms.	*/
				}
			}

			else if ((dat == FIELD_DATA_MDY) || (dat == FIELD_DATA_YMD) || (dat == FIELD_DATA_DMY))
			{
				okdata = TRUE;								/* Assume ok.		*/
				if (intdate(dp,len,dat) == -1) okdata = FALSE;				/* Is date ok?		*/
				if (!okdata)								/* No, then report.	*/
				{
					int ec;								/* Assume plain NUM.	*/
					comarea->numerrs++;						/* Count the error.	*/
					if (dat == FIELD_DATA_MDY) ec = FIELD_MUST_BE_MDY;
					if (dat == FIELD_DATA_DMY) ec = FIELD_MUST_BE_DMY;
					if (dat == FIELD_DATA_YMD) ec = FIELD_MUST_BE_YMD;
					vformcurrent->field[i].error_code = ec;				/* Remember for forms.	*/
				}
			}
		}
	}
}

static int simple_edit(dp,len,px) char *dp; int len; char *px;					/* Perform simple edits.	*/
{
	int simple;

	simple = FALSE;

	if      (!memcmp("UPSHIFT"       ,px, 7)) {usedit(dp,len); simple = TRUE;}		/* Is this upshift?		*/
	else if (!memcmp("JUSTIFY LEFT"  ,px,12)) {jledit(dp,len); simple = TRUE;}		/* Is this justify left?	*/
	else if (!memcmp("JUSTIFY RIGHT" ,px,13)) {jredit(dp,len); simple = TRUE;}		/* Justify right?		*/
	else if (!memcmp("JUSTIFY CENTER",px,14)) {jcedit(dp,len); simple = TRUE;}		/* Justify center.		*/

	return(simple);										/* Report if it was simple.	*/
}

static int blank(dx,len) char *dx; int len;							/* Check if a field is blank.	*/
{
	int i,j,filled;

	filled = FALSE;										/* Assume the field is blank.	*/
	for (j = 0; (j < len) && !filled; j++) 							/* Check each character.	*/
	{
		if (*dx++ != ' ') filled = TRUE;						/* Found an error.		*/
	}
	return(!filled);									/* Let the caller know.		*/
}

static void usedit(ptr,len) char *ptr; int len;
{
	char toupper();
	int k;
	for (k = 0; k < len; k++) *(ptr+k) = toupper(*(ptr+k));					/* Make it upper case.		*/
}

static void jledit(ptr,len) char *ptr; int len;							/* Left justify.		*/
{
	int k;

	if (blank(ptr,len)) return;								/* Don't edit if blank.		*/

	while (*ptr == ' ')									/* Loop until LHS not blank.	*/
	{
		for (k = 0; k < len-1; k++) *(ptr+k) = *(ptr+k+1);				/* Shuffle left.		*/
		*(ptr+k) = ' ';									/* Fill in a space.		*/
	}
}

void jredit(ptr,len) char *ptr; int len;							/* Right justify.		*/
{
	int k;

	if (blank(ptr,len)) return;								/* Don't edit if blank.		*/

	while (*(ptr+len-1) == ' ')								/* Loop until RHS not blank.	*/
	{
		for (k = len-1; k > 0; k--) *(ptr+k) = *(ptr+k-1);				/* Shuffle right.		*/
		*ptr = ' ';									/* Fill in a blank.		*/
	}
}

static void jcedit(ptr,len) char *ptr; int len;
{
	vre_window("jcedit: Justify center not supported...");
}

static int inrange(dp,len,px) char *dp, *px; int len;						/* Check for an inrange value.	*/
{
	int i,j,k;										/* Working integers.		*/
	float x,y,z;										/* Working real numbers.	*/
	int checked;										/* True when table checked.	*/
	int matched;										/* True when a match found.	*/
	char dl;										/* The delimiter in use.	*/
	char *wp;										/* Working pointer.		*/
	char toupper();										/* Uppercase.			*/
	char hi_value[128], low_value[128], the_value[128];					/* Values in string format.	*/
	int colon_found;									/* Value separated by colon.	*/

	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the range table.	*/

	checked = FALSE;									/* Not checked yet.		*/
	matched = FALSE;									/* Not matched yet.		*/
	while (!checked && !matched)								/* Loop until all checked or	*/
	{											/*   a match is found.		*/
		dl = CHAR_NULL;
		if      (*px == SQ) dl = SQ;							/* Delimiter is a single quote.	*/
		else if (*px == DQ) dl = DQ;							/* Delimiter is a double quote.	*/

		if (*px == dl)									/* Is this a char compare.	*/
		{
			px++;									/* Move to the first real char.	*/
			matched = TRUE;								/* Assume we will match.	*/
			for (i = 0; (*px != dl); i++, px++)					/* Now loop until the trailer.	*/
			{
				if (*px != *(dp+i)) matched = FALSE;				/* Were we right.		*/
			}
			px++;									/* Move past the delimiter.	*/
		}

		else if (digit(*px))								/* Is it a numeric?		*/
		{
			colon_found = FALSE;							/* Assume not a range entry.	*/
			wp = low_value;								/* Fill the low value string.	*/
			while(digit(*px)) *wp++ = *px++;					/* Get all the digits.		*/
			*wp = CHAR_NULL;							/* Null terminate.		*/

			if (*px == ':')								/* Is it a range entry?		*/
			{
				colon_found = TRUE;						/* Yes so remember it.		*/
				px++;								/* Move to the next value.	*/
				wp = hi_value;							/* Point to the high value.	*/
				while(digit(*px)) *wp++ = *px++;				/* Copy to the high value.	*/
				*wp = CHAR_NULL;						/* Null terminate.		*/
			}

			for (i = 0; i < len; i++) the_value[i] = *(dp+i);			/* Copy to the value.		*/
			the_value[i] = CHAR_NULL;						/* Null terminate.		*/

			x = numval(the_value);							/* Decode the value.		*/
			y = numval(low_value);							/* Decode the low value.	*/
			if (colon_found)							/* Hi low range?		*/
			{
				z = numval(hi_value);						/* Decode the high value.	*/
				if ((x >= y) && (x <= z)) matched = TRUE;			/* Are we in range?		*/
			}
			else									/* Discrete value comparison.	*/
			{
				if (x == y) matched = TRUE;					/* Matched if the same.		*/
			}
		}

		else if (!memcmp("$STATE",px,6))						/* Special state processing?	*/
		{
			wp = states;								/* Point to the state table.	*/
			while ((*wp != CHAR_NULL) && !matched)					/* Is it in the state table?	*/
			{
				if ((toupper(*dp) == *wp) && (toupper(*(dp+1)) == *(wp+1))) 	/* Is it in the table?		*/
				{
					matched = TRUE;						/* Yes, then we have matched.	*/
				}
				wp++; wp++;							/* Move to the next state.	*/
			}
			px = px + 6;								/* Move past this value.	*/
		}

		else
		{
			vre_window("inrange: Invalid range table entry");
			return(FAILURE);
		}

		if (!matched)									/* Have we matched?		*/
		{
			while (*px == ' ') px++;						/* Move to next nonblank.	*/
			if (*px == ',') px++;							/* Move past comma if it is.	*/
			else checked = TRUE;							/* No more range values.	*/
			while ((*px == ' ') && (*px != CHAR_NULL)) px++;			/* Move to start of next value.	*/
		}
	}

	if (!matched)										/* Did we match?		*/
	{											/* No, then remember message.	*/
		buff[0] = CHAR_NULL;								/* Assume no message.		*/

		if (*px != CHAR_NULL)								/* Be sure not at end of proc.	*/
		{
			dl = CHAR_NULL;								/* Assume no delimiter.		*/
			if      (*px == SQ) dl = SQ;						/* Delimiter is a single quote.	*/
			else if (*px == DQ) dl = DQ;						/* Delimiter is a double quote.	*/
			if (dl != CHAR_NULL)							/* At end of proc?		*/
			{
				px++;								/* Move past the delimiter.	*/
				for (i = 0; ((*px != dl) && (*px != CHAR_NULL)); i++, px++) 	/* Now loop until the trailer.	*/
				{
					buff[i] = *px;						/* Copy the message.		*/
				}
				buff[i] = CHAR_NULL;						/* Store trailing null.		*/
			}
		}
	}

	return(matched);									/* Return the result.		*/
}

float numval(string) char *string;								/* Decode a string to numeric.	*/
{
	float i;										/* Working registers.		*/

	if (blank(string,strlen(string))) return(0.0);						/* Return a floating 0.		*/
	sscanf(string,"%f",&i);									/* Decode to an integer.	*/
	return(i);										/* Return the value.		*/
}

static int simple_conditional(px) char *px;							/* Simple conditional check?	*/
{
	int i,j;

	i = 0;											/* Assume an invalid code.	*/
	if      (!memcmp("GT ",px,3)) i = CODE_GT;						/* Is it a valid code?		*/
	else if (!memcmp("LT ",px,3)) i = CODE_LT;
	else if (!memcmp("GE ",px,3)) i = CODE_GE;
	else if (!memcmp("LE ",px,3)) i = CODE_LE;
	else if (!memcmp("EQ ",px,3)) i = CODE_EQ;
	else if (!memcmp("NE ",px,3)) i = CODE_NE;

	return(i);										/* Return the code value.	*/
}

static int truecond(cc,dp,len,dat,px) int cc; char *dp, *px; int len,dat;			/* Check for an inrange value.	*/
{
	int i,j,k;										/* Working integers.		*/
	char dl;										/* The delimiter in use.	*/
	char *wp;										/* Working pointer.		*/
	char toupper();										/* Uppercase.			*/
	char low_value[128], the_value[128];							/* Values in string format.	*/
	int state;										/* State of trueness.		*/
	int same;										/* State of comparison.		*/
	char buf1[128], buf2[128];								/* Working buffers.		*/
	int f;

	state = FALSE;										/* Assume condition not true.	*/
	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the condition value.	*/

	dl = CHAR_NULL;										/* Null out the delimiter.	*/
	if      (*px == SQ) dl = SQ;								/* Delimiter is a single quote.	*/
	else if (*px == DQ) dl = DQ;								/* Delimiter is a double quote.	*/

	if (*px == dl)										/* Is this a char compare.	*/
	{
		px++;										/* Move past the delimiter.	*/

		for (i = 0; i < 128; i++) 							/* Set to spaces.		*/
		{
			buf1[i] = ' ';
			buf2[i] = ' ';
		}

		wp = dp;									/* Point to the data.		*/
		for (i = 0; i < len; i++) buf1[i] = *wp++;					/* Copy the data.		*/
		for (i = 0; ((i < 128) && (*px != dl)); i++) buf2[i] = *px++;			/* Copy the match.		*/

		state = string_compare(cc,buf1,buf2,len,i);					/* Compare the strings.		*/
	}

	else if (digit(*px))									/* Is it a numeric?		*/
	{
		wp = low_value;									/* Fill the low value string.	*/
		while(digit(*px)) *wp++ = *px++;						/* Get all the digits.		*/
		*wp = CHAR_NULL;								/* Null terminate.		*/

		for (i = 0; i < len; i++) the_value[i] = *(dp+i);				/* Copy to the value.		*/
		the_value[i] = CHAR_NULL;							/* Null terminate.		*/

		state = integer_compare(cc,the_value,low_value);				/* Compare the integers.	*/
	}

	else if (!memcmp("$EMPTY",px,6))							/* Special processing?		*/
	{
		if ((cc == CODE_EQ) || (cc == CODE_NE));					/* Is this valid for now?	*/
		else vre_window("truecond: Invalid comparison to $EMPTY, only EQ & NE supported.");

		if (blank(dp,len) && (cc == CODE_EQ)) state = TRUE;				/* Equal to empty?		*/
		else if (!blank(dp,len) && (cc == CODE_NE)) state = TRUE;			/* Not equal to emplty.		*/

		px = px + 6;									/* Move past.			*/
	}

	else if (!memcmp("$TODAY",px,6))							/* Compare to today's date?	*/
	{
		i = vtoday();									/* Get today's date.		*/
		j = intdate(dp,len,dat);							/* Get the reference date.	*/
		state = fcomp(cc, (float)j, (float)i);						/* Do the comparison.		*/
	}

	else if ((j = name_match(px)) >= 0)							/* Comparing another field?	*/
	{
		wp = vform_data_buffer;								/* Point to the data buffer.	*/
		for (i = 0; i < j; i++) wp = wp + vformcurrent->field[i].length;		/* Move to field's data.	*/
		f = i;										/* Remember 2nd field.		*/

		if (vformcurrent->field[f].datatype == FIELD_DATA_CHAR)				/* Do a character comparision?	*/
		{
			for (i = 0; i < 128; i++) 						/* Set to spaces.		*/
			{
				buf1[i] = ' ';
				buf2[i] = ' ';
			}
	
			for (i = 0; i < len; i++) buf1[i] = *(dp+i);					/* Copy the data.	*/
			for (i = 0; i < vformcurrent->field[f].length; i++) buf2[i] = *(wp+i);		/* Copy the match.	*/

			state = string_compare(cc,buf1,buf2,len,vformcurrent->field[f].length);		/* Compare the strings.	*/
		}

		else if (vformcurrent->field[f].datatype == FIELD_DATA_DIG)				/* Is it a numeric?	*/
		{
			for (i = 0; i < vformcurrent->field[f].length; i++) low_value[i] = *(wp+i);
			low_value[i] = CHAR_NULL;
			for (i = 0; i < len; i++) the_value[i] = *(dp+i);				/* Copy to the value.	*/
			the_value[i] = CHAR_NULL;							/* Null terminate.	*/

			state = integer_compare(cc,the_value,low_value);
		}

		else if (vformcurrent->field[f].datatype == FIELD_DATA_MDY)				/* Is it a date?	*/
		{
			i = intdate(dp,len,dat);
			j = intdate(wp,vformcurrent->field[f].length,vformcurrent->field[f].datatype);
			state = fcomp(cc,(float)i,(float)j);						/* Compare.		*/
		}

		else
		{
			vre_window("truecond: Invalid comparison of field to field");
			return(FAILURE);
		}
	}

	else
	{
		vre_window("truecond: Invalid value for field entry");
		return(FAILURE);
	}

	if (!state)										/* Is condition true?		*/
	{
		while (*px != ' ') px++;							/* Move to next nonblank.	*/
		while ((*px == ' ') && (*px != CHAR_NULL)) px++;				/* Move to start of message	*/

		buff[0] = CHAR_NULL;								/* Assume no message.		*/

		if (*px != CHAR_NULL)								/* Be sure not at end of proc.	*/
		{
			dl = CHAR_NULL;								/* Assume no delimiter.		*/
			if      (*px == SQ) dl = SQ;						/* Delimiter is a single quote.	*/
			else if (*px == DQ) dl = DQ;						/* Delimiter is a double quote.	*/
			if (dl != CHAR_NULL)							/* At end of proc?		*/
			{
				px++;								/* Move past the delimiter.	*/
				for (i = 0; ((*px != dl) && (*px != CHAR_NULL)); i++, px++) 	/* Now loop until the trailer.	*/
				{
					buff[i] = *px;						/* Copy the message.		*/
				}
				buff[i] = CHAR_NULL;						/* Store trailing null.		*/
			}
		}
	}

	return(state);										/* Return the result.		*/
}

static int iftrue(dx,len,dat,px) char *dx, *px; int len,dat;					/* Determine if an if is true.	*/
{
	int i,j,k;										/* Working integers.		*/
	char *wp;										/* Working pointer.		*/
	int state;										/* State of trueness.		*/

	state = FALSE;										/* Assume condition not true.	*/

	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the conditional.	*/

	if (j = simple_conditional(px)) state = truecond(j,dx,len,dat,px);			/* Is it a simple conditional?	*/

	else if (!memcmp("MINLEN ",px,7)) state = minlength(dx,len,px);				/* Check min length?	*/

	else if (!memcmp("MATCH ",px,6)) state = matched(dx,len,px);				/* Match processing.		*/

	else if (!memcmp("IN ",px,3)) state = inrange(dx,len,px);				/* An in conditional?		*/

	else if ((j = name_match(px)) >= 0)							/* Comparing another field?	*/
	{
		wp = vform_data_buffer;								/* Point to the data buffer.	*/
		for (i = 0; i < j; i++) wp = wp + vformcurrent->field[i].length;		/* Move to field's data.	*/
		state = iftrue(wp,vformcurrent->field[j].length,vformcurrent->field[j].datatype,px);
	}

	else vre_window("iftrue: Invalid conditional, false returned");				/* Don't know what it is.	*/

	return(state);										/* Return to the caller.	*/
}

static int name_match(px) char *px;								/* Match to a field name.	*/
{
	int i,j,k;
	char fn[HP_FORM_NAME_SIZE+1];								/* Form name working storage.	*/
	char *wp;										/* Working pointer.		*/

	for (i = 0; i <HP_FORM_NAME_SIZE; i++) fn[i] = ' ';					/* Initialize.			*/
	fn[i] = CHAR_NULL;									/* Null terminate.		*/

	wp = fn;										/* Point to the name.		*/
	while (alphanum(*px)) *wp++ = *px++;							/* Copy the given copy.		*/
	return(onbyname(fn));									/* Return the field number.	*/
}

int onbyname(fn) char *fn;									/* Return field order.		*/
{
	int i,k;

	for (i = 0, k = -1; (i < vformcurrent->fields) && (k == -1); i++)			/* Check fields until a match.	*/
	{
		if (!memcmp(fn,vformcurrent->field[i].field_name,HP_FORM_NAME_SIZE)) k = i;	/* Did we match?		*/
	}

	return(k);										/* Return if we matched.	*/
}

static int digit(num) char num;									/* Make sure number is numeric.	*/
{
	int i;

	i = FALSE;
	if ((num >= '0') && (num <= '9')) i = TRUE;
	return(i);
}

static int alphanum(text) char text;								/* Make sure alphanumeric.	*/
{
	int i;

	i = FALSE;
	if      ((text >= '0') && (text <= '9')) i = TRUE;
	else if ((text >= 'a') && (text <= 'z')) i = TRUE;
	else if ((text >= 'A') && (text <= 'Z')) i = TRUE;
	return(i);
}

static void set_local_error(comarea,field,errs) struct vplus_comarea *comarea; int field; int *errs;	/* Load local error.	*/
{				
	comarea->numerrs++;										/* No, count the error.	*/
	vformcurrent->field[field].error_code = LOCAL_ERROR_ACTIVE;					/* Flag the field.	*/
	if (!(*errs))											/* Local error loaded?	*/
	{
		vputlocal_error_message(buff,strlen(buff));						/* No, load this one.	*/
		(*errs)++;										/* Now it is loaded.	*/
	}
}

static int matched(dx,len,px) char *dx; int len; char *px;
{
	vre_window("MATCH: not fully implemented, assumed correct, program will continue without error.");
	return(TRUE);
}

static void dofill(dx,len,px) char *dx; int len; char *px;					/* Do fill processing.		*/
{
	int i,j,k;										/* Working integers.		*/
	char *wp;										/* Working pointer.		*/

	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the conditional.	*/
	wp = dx;										/* Working pointer.		*/

/*	if (blank(dx,len)) return;	*/							/* Don't edit if blank.		*/

	if (!memcmp("LEADING ",px,8))								/* Fill leading?		*/
	{
		while (*px != ' ') px++;							/* Find the end of current.	*/
		while (*px == ' ') px++;							/* Find the conditional.	*/
		px++;										/* Move past the delimiter.	*/
		for (j = 0; (j < len) && (*wp == ' '); j++)					/* Loop through leading spaces.	*/
		{
			*wp = *px;								/* Copy the character.		*/
			wp++;									/* Next character.		*/
		}
	}

	else if (!memcmp("TRAILING ",px,9))							/* Fill trailing?		*/
	{
		while (*px != ' ') px++;							/* Find the end of current.	*/
		while (*px == ' ') px++;							/* Find the conditional.	*/
		px++;										/* Move past the delimiter.	*/
		for (j = len-1; (j >= 0) && (*wp == ' '); j--)					/* Go through trailing spaces.	*/
		{
			*(wp+j) = *px;								/* Store the new trailer.	*/
		}
	}

	else vre_window("FILL: Must be LEADING or TRAILING");					/* Error.			*/
}

get_places(dx,len) char *dx; int len;								/* Determine the decimal	*/
{												/*   places entered.		*/
	int i,j,k;
	int eod;										/* End of data flag.		*/

	j = -1;
	for (i = 0; i < len; i++)								/* Find a decimal.		*/
	{
		if (*(dx+i) == '.') j = i;							/* Is this a decimal point?	*/
	}
	if (j == -1) return(j);									/* No decimal entered.		*/

	eod = FALSE;										/* Not at end of data.		*/
	for (i = j+1, k = 0; (i < len) && !eod; i++)
	{
		if ((*(dx+i) >= '0') && (*(dx+i) <= '9')) k++;					/* Count the decimal point.	*/
		else eod = TRUE;								/* Oops, at end of data.	*/
	}
	return(k);										/* Return the decimal points.	*/
}

static int string_compare(cc, buf1,buf2,len1,len2) int cc; char buf1[],buf2[]; int len1, len2;	/* Compare two strings.		*/
{
	int i, j;										/* Working ints.		*/
	int same,state;										/* Control flags.		*/

	state = FALSE;										/* Assume a false state.	*/
	j = len1;										/* Assume as long as the field.	*/
	if (len2 > len1) j = len2;								/* Other way around?		*/

	for (i = 0, same = TRUE; (i < j) && same; i++)						/* Check if they are different.	*/
	{
		if (buf1[i] != buf2[i])								/* Are they the same?		*/
		{
			same = FALSE;								/* No longer the same.		*/
			if ((buf1[i] > buf2[i]) && (cc == CODE_GT)) state = TRUE;		/* Determine the state change.	*/
			if ((buf1[i] > buf2[i]) && (cc == CODE_GE)) state = TRUE;
			if ((buf1[i] < buf2[i]) && (cc == CODE_LT)) state = TRUE;
			if ((buf1[i] < buf2[i]) && (cc == CODE_LE)) state = TRUE;
		}
	}

	if (same && (cc == CODE_EQ)) state = TRUE;						/* Check for equal.		*/
	if (same && (cc == CODE_LE)) state = TRUE;
	if (same && (cc == CODE_GE)) state = TRUE;
	if (!same && (cc == CODE_NE)) state = TRUE;

	return(state);
}

static int integer_compare(cc,the_value,low_value) int cc; char the_value[], low_value[];	/* Compare to integer strings.	*/
{
	float i,j;
	int state;

	i = numval(the_value);									/* Decode the value.		*/
	j = numval(low_value);									/* Decode the low value.	*/

	state = fcomp(cc,i,j);
	return(state);
}

static int fcomp(cc,i,j) int cc; float i,j;
{
	int state;

	state = FALSE;										/* Assume not true.		*/
	if      ((cc == CODE_EQ) && (i == j)) state = TRUE;					/* The condition is true.	*/
	else if ((cc == CODE_NE) && (i != j)) state = TRUE;
	else if ((cc == CODE_LT) && (i <  j)) state = TRUE;
	else if ((cc == CODE_LE) && (i <= j)) state = TRUE;
	else if ((cc == CODE_GT) && (i >  j)) state = TRUE;
	else if ((cc == CODE_GE) && (i >= j)) state = TRUE;

	return(state);
}

static void dostrip(dx,len,px) char *dx; int len; char *px;					/* Do strip processing.		*/
{
	int i,j,k;										/* Working integers.		*/
	char *wp;										/* Working pointer.		*/

	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the conditional.	*/
	wp = dx;										/* Working pointer.		*/

	if (blank(dx,len)) return;								/* Don't edit if blank.		*/

	if (!memcmp("LEADING ",px,8))								/* Strip leading?		*/
	{
		while (*px != ' ') px++;							/* Find the end of current.	*/
		while (*px == ' ') px++;							/* Find the conditional.	*/
		px++;										/* Move past the delimiter.	*/

		for (j = 0; (j < len) && ((*wp == ' ') || (*wp == *px)); j++)			/* Loop through leading.	*/
		{
			*wp = ' ';								/* Copy the character.		*/
			wp++;									/* Next character.		*/
		}
		if (blank(dx,len)) *(dx+len-1) = *px;						/* Store single if made blank.	*/
	}

	else if (!memcmp("TRAILING ",px,9))							/* Strip trailing?		*/
	{
		while (*px != ' ') px++;							/* Find the end of current.	*/
		while (*px == ' ') px++;							/* Find the conditional.	*/
		px++;										/* Move past the delimiter.	*/

		for (j = len-1; (j >= 0) && ((*(wp+j) == ' ') || (*(wp+j) == *px)); j--)	/* Go through trailing.		*/
		{
			*(wp+j) = ' ';								/* Store the new trailer.	*/
		}
	}

	else if (!memcmp("ALL ",px,4))								/* Strip all?			*/
	{
		while (*px != ' ') px++;							/* Find the end of current.	*/
		while (*px == ' ') px++;							/* Find the conditional.	*/
		px++;										/* Move past the delimiter.	*/

		for (j = 0; j < len; j++)							/* Loop through the string.	*/
		{
			if (*(wp+j) == *px)							/* Is this to be stripped?	*/
			{
				for (k = j; k < len-1; k++) *(wp+k) = *(wp+k+1);		/* Shuffle left.		*/
				*(wp+k) = ' ';							/* Now insert a space.		*/
				j--;								/* Go back in case together.	*/
			}
		}
	}

	else vre_window("STRIP: Must be LEADING, TRAILING or ALL");				/* Error.			*/
}

static int minlength(dp,len,px) char *dp, *px; int len;						/* Check for valid length.	*/
{
	int i,j;
	float x,y;
	char value[128];
	char *wp;
	char dl;
	int min;

	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the length value.	*/

	wp = value;										/* Fill the low value string.	*/
	while(digit(*px)) *wp++ = *px++;							/* Get all the digits.		*/
	*wp = CHAR_NULL;									/* Null terminate.		*/
	while (*px == ' ') px++;								/* Go to the message.		*/

	x = numval(value);									/* Decode the value.		*/

	min = TRUE;										/* Assume minimum.		*/
	if (blank(dp,len)) min = FALSE;								/* Blank is bad.		*/
	else
	{
		i = 0;										/* Number of non blanks.	*/
		for (j = 0; j < len; j++)							/* Decrement the loop.		*/
		{
			if (*(dp+j) != ' ') i++;						/* Count the character.		*/
		}
		if (i < (int)x) min = FALSE;							/* Well, do we have enough?	*/
		else min = TRUE;
	}

	if (!min)										/* Did we match?		*/
	{											/* No, then remember message.	*/
		buff[0] = CHAR_NULL;								/* Assume no message.		*/

		if (*px != CHAR_NULL)								/* Be sure not at end of proc.	*/
		{
			dl = CHAR_NULL;								/* Assume no delimiter.		*/
			if      (*px == SQ) dl = SQ;						/* Delimiter is a single quote.	*/
			else if (*px == DQ) dl = DQ;						/* Delimiter is a double quote.	*/
			if (dl != CHAR_NULL)							/* At end of proc?		*/
			{
				px++;								/* Move past the delimiter.	*/
				for (i = 0; ((*px != dl) && (*px != CHAR_NULL)); i++, px++) 	/* Now loop until the trailer.	*/
				{
					buff[i] = *px;						/* Copy the message.		*/
				}
				buff[i] = CHAR_NULL;						/* Store trailing null.		*/
			}
		}
	}

	return(min);										/* Return the result.		*/
}

static void doset(dx,len,dat,dpl,px) char *dx; int len, dat, dpl; char *px;			/* Set the field to a value.	*/
{
	int i,j,k;
	char dl, *wp;
	char *tp, temp[128];

	tp = temp;
	for (i = 0; i < 128; i++) *tp++ = CHAR_NULL;						/* Null it out.			*/

	while (*px != ' ') px++;								/* Find the end of current.	*/
	while (*px == ' ') px++;								/* Find the next data.		*/

	if (!memcmp("TO ",px,3))								/* Is it a SET TO command?	*/
	{
		while (*px != ' ') px++;							/* Find the end of current.	*/
		while (*px == ' ') px++;							/* Find the next data.		*/

		dl = CHAR_NULL;									/* Null out the delimiter.	*/
		if      (*px == SQ) dl = SQ;							/* Delimiter is a single quote.	*/
		else if (*px == DQ) dl = DQ;							/* Delimiter is a double quote.	*/

		if (*px == dl)									/* Is this a char compare.	*/
		{
			px++;									/* Move past the delimiter.	*/
			wp = temp;								/* Point to the data.		*/
			for (i = 0; (i < len) && (*px != dl); i++) *wp++ =  *px++;		/* Copy the data.		*/
		}

		else if (!memcmp("$EMPTY",px,6)) px = px + 6;					/* Special processing?		*/

		else if ((j = name_match(px)) >= 0)						/* Comparing another field?	*/
		{
			wp = vform_data_buffer;							/* Point to the data buffer.	*/
			for (i = 0; i < j; i++) wp = wp + vformcurrent->field[i].length;	/* Move to field's data.	*/

			tp = temp;								/* Point to the temp buf.	*/
			for (j = 0; (j < len) && (j < vformcurrent->field[i].length); j++)	/* Copy the data.		*/
			{
				*(tp+j) = *(wp+j);
			}
		}

		else
		{
			vre_window("doset: Invalid value for field for SET TO command.");
		}

		if ((dat == FIELD_DATA_NUM) || (dat == FIELD_DATA_IMP) || (dat == FIELD_DATA_DIG))
		{
			dostrip(temp,strlen(temp),"X ALL '+'");					/* Trim all + signs.		*/
			dostrip(temp,strlen(temp),"X ALL ','");					/* Trim all commas.		*/
			dostrip(temp,strlen(temp),"X LEADING '0'");				/* Trim all leading zeroes.	*/
			jledit(temp,strlen(temp));						/* Make easier to work with.	*/

			if ((dat == FIELD_DATA_DIG) || (dpl == 0))				/* Remove decimal point?	*/
			{
				if ((i = strpos(temp,".")) != -1)				/* Is there a decimal point?	*/
				{
					for (j = i; j < strlen(temp); j++) temp[j] = ' ';	/* Remove all fractions.	*/
				}
			}

			else if (dat == FIELD_DATA_IMP)						/* NUM or IMP with decimals.	*/
			{
				if ((i = strpos(temp,".")) != -1)				/* Is there a decimal point?	*/
				{
					dostrip(temp,strlen(temp),"X ALL '.'");			/* Yes then strip it.		*/
					for (j = i; (j < strlen(temp)) && (j-i < dpl); j++);	/* Add zeroes.			*/
					{
						if (!digit(temp[j])) temp[j] = '0';		/* Fill.			*/
					}
				}
			}

			else if (dat == FIELD_DATA_NUM)						/* NUM with decimals.		*/
			{
				if (blank(temp,strlen(temp))) temp[0] = '0';			/* Store a zero if blank.	*/
				if ((i = strpos(temp,".")) == -1) 				/* Is there a decimal point.	*/
				{
					i = strpos(temp," ");					/* Find the first space.	*/
					temp[i] = '.';						/* Make it a decimal point.	*/
				}
				for (j = i+1; (j < strlen(temp)) && (j-i-1 < dpl); j++)		/* Fill with zeroes.		*/
				{	
					if (!digit(temp[j])) temp[j] = '0';			/* Fill with zeroes.		*/
				}
			}

			jredit(temp,strlen(temp));						/* Right justify.		*/
		}

		for (i = 0; (i < len) && (i < strlen(temp)); i++) *(dx+i) = temp[i];		/* Copy the final result.	*/
	}

	else
	{
		vre_window("doset: Only SET TO currently supported");				/* No setting of another field.	*/
	}	
}

static int intdate(dx,len,dt) char dx[]; int len; int dt;					/* Create internal date.	*/
{
	int i,j;										/* Working integers.		*/
	char ws[64];
	char t[4];
	int y,m,d;

	for (i = 0; i < len; i++) ws[i] = dx[i];						/* Copy the data string.	*/
	ws[i] = CHAR_NULL;

	trim(ws);
	dshuf(ws);
	trim(ws);
	dshuf(&ws[3]);
	trim(ws);
	dorem(ws,"/");
	trim(ws);
	dorem(ws,"-");
	trim(ws);
	dorem(ws," ");
	trim(ws);
	dorem(ws,".");
	trim(ws);
	dorem(ws,",");
	trim(ws);

	for (i = 0; i < strlen(ws); i++)
	{
		if (!digit(ws[i])) return(-1);
	}

	if (dt == FIELD_DATA_DMY)
	{
		t[0] = ws[0];
		t[1] = ws[1];
		ws[0] = ws[4];
		ws[1] = ws[5];
		ws[4] = t[0];
		ws[5] = t[1];
	}
	else if (dt = FIELD_DATA_MDY)
	{
		t[0] = ws[4];
		t[1] = ws[5];
		for (i = 5; i >= 2; i--) ws[i] = ws[i-2];
		ws[0] = t[0];
		ws[1] = t[1];
	}

	y = ((ws[0]-'0')*10) + (ws[1]-'0');
	m = ((ws[2]-'0')*10) + (ws[3]-'0');
	d = ((ws[4]-'0')*10) + (ws[5]-'0');

	if ((m < 1) || (m > 12)) return(-1);

	i = 31;
	if ((m == 9) || (m == 4) || (m == 6) || (m == 11)) i = 30;	/* 30 days has September, April, June and November.	*/
	if ((m == 2) && (y == 92) || (y == 96)) i = 29;
	else if (m == 2) i = 28;

	if ((d > i) || (d < 1)) return(-1);

	i = (int)numval(ws);
	return(i);
}

static void trim(ws) char *ws;
{
	int i;

	for (i = strlen(ws)-1; i >= 0; i--)							/* Trim it.			*/
	{
		if (ws[i] == ' ') ws[i] = CHAR_NULL;
	}
}

static void dorem(wp,px) char *wp,*px;								/* Remove from a string.	*/
{
	int i,j,k;
	int len;

	len = strlen(wp);

	for (j = 0; j < len; j++)								/* Loop through the string.	*/
	{
		if (*(wp+j) == *px)								/* Is this to be stripped?	*/
		{
			for (k = j; k < len-1; k++) *(wp+k) = *(wp+k+1);			/* Shuffle left.		*/
			*(wp+k) = ' ';								/* Now insert a space.		*/
			j--;									/* Go back in case together.	*/
		}
	}
}

static void dshuf(ws) char ws[];
{
	int i;
	if (!digit(ws[1]))
	{
		for (i = strlen(ws); i > 0; i--) ws[i] = ws[i-1];
		ws[0] = '0';
	}
}

static int check_num(dp,len) char *dp; int len;
{
	int i,j;
	int okdata;

	for (j = 0, okdata = FALSE; (j < len) && !okdata; j++)						/* FIND 1 good char.	*/
	{
		if ((*(dp+j) >= '0') &&  (*(dp+j) <= '9')) okdata = TRUE;				/* A valid digit?	*/
	}
	for (j = 0; (j < len) && okdata; j++) 								/* Find a bad char?	*/
	{
		if (((*(dp+j) < '0') || (*(dp+j) > '9')) && (*(dp+j) != ' ')) okdata = FALSE;
	}
	if (!okdata) return(okdata);

	j = 0;
	while ((*(dp+j) == ' ') && (j < len)) j++;							/* Find 1st non space.	*/
	if (j == len) return(TRUE);									/* The data is ok.	*/
	while ((*(dp+j) != ' ') && (j < len)) j++;							/* Find next space.	*/
	if (j == len) return(TRUE);									/* Space to end ok.	*/
	while ((*(dp+j) == ' ') && (j < len)) j++;
	if (j == len) return(TRUE);
	return(FALSE);
}

int vfblank(dp,len) char *dp; int len;
{
	return(blank(dp,len));
}

int vtoday()
{
	int i, j, t;
	char *ctime();
	long time_data;
	char tbuff[32];

	time_data=time(NULL);									/* Get the current time.	*/
	strcpy(tbuff,ctime(&time_data));							/* Get the time string.		*/

	t = 0;											/* Initialize.			*/
	t = t + (tbuff[22]-'0') * 100000;							/* Get the decade.		*/
	t = t + (tbuff[23]-'0') *  10000;							/* Get the year.		*/

	if      (!memcmp("Jan",&tbuff[4],3)) t = t +  100;					/* Add in the month.		*/
	else if (!memcmp("Feb",&tbuff[4],3)) t = t +  200;
	else if (!memcmp("Mar",&tbuff[4],3)) t = t +  300;
	else if (!memcmp("Apr",&tbuff[4],3)) t = t +  400;
	else if (!memcmp("May",&tbuff[4],3)) t = t +  500;
	else if (!memcmp("Jun",&tbuff[4],3)) t = t +  600;
	else if (!memcmp("Jul",&tbuff[4],3)) t = t +  700;
	else if (!memcmp("Aug",&tbuff[4],3)) t = t +  800;
	else if (!memcmp("Sep",&tbuff[4],3)) t = t +  900;
	else if (!memcmp("Oct",&tbuff[4],3)) t = t + 1000;
	else if (!memcmp("Nov",&tbuff[4],3)) t = t + 1100;
	else if (!memcmp("Dec",&tbuff[4],3)) t = t + 1200;

	if (tbuff[8] == ' ') t = t + (tbuff[8]-' ') * 10;
	else t = t + (tbuff[8]-'0') * 10;							/* Add in the date.		*/
	t = t + (tbuff[9]-'0');

	return(t);										/* Return the value.		*/
}
