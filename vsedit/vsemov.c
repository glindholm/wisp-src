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

#include <stdio.h>

#include "idsistd.h"
#include "vsemov.h"
#include "vseglb.h"
#include "vsescr.h"
#include "vsedscr.h"
#include "vsebasic.h"
#include "vsedel.h"
#include "vsedfnd.h"
#include "vsedit.h"
#include "vsedmod.h"
#include "vsegps.h"
#include "vsetxt.h"
#include "vseutl.h"


static char start_field[17],end_field[17],target_field[17],emsg_field[81];
static char cpy_start_field[17],cpy_end_field[17];

static VSESCR_FLDS(ed_mov_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Press (ENTER) to MOVE the specified range of lines to after target line.")},
{LEN(0)	ROW(1)	COL(77)	ULINEVALUE("Move")},
{LEN(0)	ROW(2)	COL(2)	VALUE("Press (1) to return to display mode.")},
{LEN(0)	ROW(3)	COL(2)	FROM(emsg_field)},

{LEN(0)	 ROW(4)	COL(2)	VALUE("Start =")},
{LEN(16) ROW(4)	COL(10)	USING(start_field)},
{LEN(0)	 ROW(4)	COL(29)	VALUE("End   =")},
{LEN(16) ROW(4)	COL(37)	USING(end_field)},
{LEN(0)	 ROW(4)	COL(56)	VALUE("Target =")},
{LEN(16) ROW(4)	COL(65)	USING(target_field)},

{LASTITEM}
};

static VSESCR_FLDS(ed_cpy_flds) = {
{LEN(0)	ROW(1)	COL(2)	VALUE("Press (ENTER) to COPY the specified range of lines to after target line.")},
{LEN(0)	ROW(1)	COL(77)	ULINEVALUE("Copy")},
{LEN(0)	ROW(2)	COL(2)	VALUE("Press (1) to return to display mode.")},
{LEN(0)	ROW(3)	COL(2)	FROM(emsg_field)},

{LEN(0)	 ROW(4)	COL(2)	VALUE("Start =")},
{LEN(16) ROW(4)	COL(10)	USING(start_field)},
{LEN(0)	 ROW(4)	COL(29)	VALUE("End   =")},
{LEN(16) ROW(4)	COL(37)	USING(end_field)},
{LEN(0)	 ROW(4)	COL(56)	VALUE("Target =")},
{LEN(16) ROW(4)	COL(65)	USING(target_field)},

{LASTITEM}
};


static int vse_mov_cpy(int type);
static int vse_ed_mov_range(int type, int4 start_line, int4 end_line, int4 target_line);
static void copy_block(TEXT **st, TEXT **end, int type);
static char *mystrdup(char *str);
static int renumber_range(TEXT *st_txt, TEXT *end_txt);
static int check_renumber(int4 number, int4 incr, int4 start, int4 end, TEXT **txt_start_p, TEXT **txt_end_p, int4 *count);
static int vse_xcopy_copy(char *sysname, int4 start_line, int4 end_line, int4 target_line, char *message, int keep_modcode);

#define VSE_LINE_MOVE 1
#define VSE_LINE_COPY 2

void vse_mov_lin(void)
{
	CLEAR_FIELD(start_field);
	CLEAR_FIELD(end_field);

	vse_mov_cpy(VSE_LINE_MOVE);
}

void vse_cpy_lin(void)
{
	static int first=1;

	if (first)
	{
		first = 0;
		
		CLEAR_FIELD(cpy_start_field);
		CLEAR_FIELD(cpy_end_field);
	}
	
	memcpy(start_field, cpy_start_field, sizeof(start_field));
	memcpy(end_field,   cpy_end_field,   sizeof(end_field));
	
	vse_mov_cpy(VSE_LINE_COPY);

	memcpy(cpy_start_field, start_field, sizeof(start_field));
	memcpy(cpy_end_field,   end_field,   sizeof(end_field));
}

static int vse_mov_cpy(int type) 
{
	int4 	start_line, end_line, target_line;
	int	rc;
	
	rc = get_line_and_index((int)(ed_oa[OA_CURSOR_ROW]),&target_line);
	if (rc < 0)
	{
		target_line = 0;
		CLEAR_FIELD(target_field);
	}
	else
	{
		sprintf(target_field,"%06ld          ",(long)target_line);
	}

	ed_oa[OA_COL] = ed_oa[OA_CURSOR_ROW] = 0;

	CLEAR_FIELD(emsg_field);
	
	for(;;)
	{
		vsescr_init(ed_scr);
		vse_ed_load_full_lines();
		strcpy(ed_pfs,"0001X");
		vsescr(ed_line_flds,ed_scr);
		if (VSE_LINE_MOVE==type)
		{
			vsescr(ed_mov_flds,ed_scr);
		}
		else
		{
			vsescr(ed_cpy_flds,ed_scr);
		}
		  
		vse_ed_add_numbers();
		vse_ed_add_mod();

		d_and_r_ed(TAB_TO_FIELD);

		int4_from_str2(ed_pfcode,&vse_edit_pick);
		if(1 == vse_edit_pick)
		{
			/*
			**	PF1 to abort
			*/
			return 1;
		}

		if (VSE_LINE_MOVE==type)
		{
			vseunscr(ed_mov_flds,ed_scr);
		}
		else
		{
			vseunscr(ed_cpy_flds,ed_scr);
		}

		rc = validate_range(start_field, &start_line, end_field, &end_line);

		if (!rc)
		{
			if (rc = validate_linenum(target_field, &target_line))
			{
				rc = RESP_TARGET;
			}
			else if (-3 == target_line)
			{
				target_line = (text_first) ? text_first->lineno : 1;
			}
			else if (-4 == target_line)
			{
				target_line = (text_last) ? text_last->lineno : 999999;
			}
			else if (-1 == target_line || -2 == target_line)
			{
				rc = RESP_TARGET;				
			}

			if (!rc && target_line >= start_line && target_line < end_line)
			{
				rc = RESP_TARGET;				
			}
		}

		if(!rc)
		{
			rc = vse_ed_mov_range(type, start_line, end_line, target_line);

			if (!rc)
			{
				return 0;
			}
		}

		strcpy(emsg_field,vse_err(rc));
	}
}

static int vse_ed_mov_range(int type, int4 start_line, int4 end_line, int4 target_line) 
{
	TEXT 	*txt=NULL,*start=NULL,*end=NULL,*target=NULL,*last_txt=NULL;
	TEXT 	*ostart,*oend;
	int	num;
	int	found_target = 0;
	int	idx;

	if (0==target_line || target_line < text_first->lineno)
	{
		found_target = 1;
		target_line = 0;
	}
	
	/* Corrected logic. start should equal the first line with a line number
	   greater than or equal to start_line, end should equal the first line
	   that is less than or equal to end_line, and target should equal the first
	   line that is closest to target_line. Added by CIS: 07/22/93 AJA */
	for (txt = text_first, num=0;
	     txt && (!start || !end || !found_target) ;
	     txt = txt->next, (start && !end)?++num:num)
	{
		if ( !end )
		{
			if ( txt->lineno > end_line )
			{
				if (!start)
				{
					return RESP_RANGE;
				}
				end = txt->prev;
			}
			else if (txt->lineno == end_line)
			{
				end=txt;
			}
		}
		if ( !start )
		{
			if (txt->lineno >= start_line)
				start=txt;
		}
		if ( !found_target )
		{
			if (txt->lineno == target_line)
			{
				target=txt;
				found_target = 1;
			}
			else if ( txt->lineno > target_line )
			{
				target = txt->prev;
				found_target = 1;
			}
		}
		last_txt = txt;
	}

	if (!start)
	{
		return RESP_START;
	}

	/* If the end of the text came before end or target was set, set them to
	   last_txt which is the last line of text. Added by CIS: 07/22/93 AJA */
	if ( !end )
		end = last_txt;
	if ( !found_target )
		target = last_txt;

	if (0==target_line && num >= text_first->lineno)
	{
		ask_renumber();
		return 0;
	}
	else if (target && target->next && (num >= target->next->lineno - target->lineno))
	{
		ask_renumber();
		return 0;
	}
	else if (target && !target->next && (num >= 999999 - target->lineno))
	{
		ask_renumber();
		return 0;
	}


	/*
	**	Make a copy of the text block
	*/
	ostart=start;
	oend=end;
	copy_block(&start,&end,type);

	/*
	**	Hookup the copied block.
	*/
	if (0==target_line)
	{
		start->prev = text_first->prev;
		end->next = text_first;
		end->next->prev = end;
		text_first = start;
		scr_first = start;
	}
	else
	{
		start->prev = target;
		end->next = target->next;
		if (end->next)
			end->next->prev = end;
		target->next = start;

		if (text_last==target)
			text_last=end;

		/*
		**	Reposition screen so that Target and at least one moved/copied
		**	line are on the screen.
		*/
		txt = target;
		for (idx = 0; idx < VSE_EDIT_ROWS - 1; ++idx)
		{
			if (ed_txt[idx] == target)
			{
				/*
				**	Target is on screen
				*/
				txt = scr_first;
				break;
			}
		}
		scr_first = txt;
	}

	renumber_range(start,end);

	if (type == VSE_LINE_MOVE)
	{
		start_line = ostart->lineno;
		end_line = oend->lineno;
		vse_ed_del_range(start_line, end_line);

		if (scr_first->lineno >= start_line && scr_first->lineno <= end_line)
		{
			scr_first = (start->prev) ? start->prev : start;
		}
	}

	vse_file_changed = 1;

	return 0;
}

static void copy_block(TEXT **st, TEXT **end, int type)
{
	TEXT *p;
	TEXT *newst, *newp;
	void *calloc();
	char *mystrdup();
	int notdone;

	for (notdone=1, p = *st, newst=newp=NULL; notdone; p=p->next)
	{
		if (newst==NULL)
		{
			newp = newst = new_text(p->text);
		}
		else
		{
			newp->next = new_text(p->text);
			newp->next->prev = newp;
			newp=newp->next;
		}

		/* If moving text, copy the line number from p so that when renumbering
		   occurs and the language is BASIC, the lines will renumber correctly.
		   If you are copying text, set the new line number for the duplicate
		   lines to 0 so that when renumbering in BASIC you do not renumber the
		   references to point to the new lines but the old ones.
		   Modified by CIS: 08/05/93 AJA */
		if ( type == VSE_LINE_MOVE )
			newp->lineno = p->lineno;
		else
			newp->lineno = 0;

		/* newp->text = mystrdup(p->text); */

		newp->modfld = NULL;
		add_modcode(newp);

		if ( lang_type() == LANG_BASIC )
			find_linenum( newp );
		if (p== *end)
		  notdone=0;
	}
	*st = newst;
	*end = newp;
}
static char *mystrdup(char *str)
{
	void *malloc();
	static char *x;
	x=(char*)malloc(strlen(str)+1);
	strcpy(x,str);
	return x;
}

static int renumber_range(TEXT *st_txt, TEXT *end_txt)
{
	int stnum,stline,endline,diff,inc,num,maxinc;
	TEXT *p;

	stline = (st_txt->prev) ? st_txt->prev->lineno : 0;
	endline = (end_txt->next) ? end_txt->next->lineno : 999999;

	diff = endline - stline - 1;           /* difference;  (range we can renumber in) */
	if (diff <= 0)
	{
		return -1;
	}

	for (num=0, p=st_txt; p!=end_txt->next; p=p->next)
	{
		  num += 1;
	}

	if (num > diff) return -1;     /* cannot renumber, not enough numbers */

	inc= maxinc = diff / num;
	if (inc >=100) inc=100;
	else if (inc <100 && inc >=50) inc=50;
	else if (inc <50 && inc >=10) inc=10;
	else if (inc <10 && inc >=5) inc=5;
	else if (inc <5 && inc >=2) inc=2;
	else if (inc <2 ) inc=1;

	stnum = stline + inc;

	if ((stnum + inc*(num-1)) >= endline)
	{
		return -1;
	}

	for (p=st_txt,num=stnum;
	     p!=end_txt->next; 
	     p=p->next, num+=inc)
	{
		if (lang_type() == LANG_BASIC)
			update_linenum( p->lineno, num );
	  	p->lineno = num;
	}

	/* After renumbering is complete, update the branch values in the linked
	   list of embedded BASIC line numbers. Added by CIS, 07/13/93 AJA */
	if (lang_type() == LANG_BASIC)
		update_branch();
	return 0;
}

/*
**	Routine:	vse_renumberer()
**
**	Function:	To renumber a range of lines.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	number		The starting new number (1-999999)
**	incr		The increment (1-999999)
**	start		Range starting line number before renumbering (1-999999)
**	end		Range ending line number before renumbering (1-999999)
**	count		Number of lines affected.
**
**	Globals:	
**	The text list
**	vse_file_changed
**
**	Return:
**	0		Completed OK
**	RESP_NUMBER	Bad number
**	RESP_INCR	Bad incr
**	RESP_START	Bad start
**	RESP_END	Bad end
**	RESP_RANGE	Bad range
**	RESP_NUMINCR	Bad number/incr combination
**
**	Warnings:	None
**
**	History:	
**	03/11/94	Written by GSL
**
*/
int vse_renumberer(int4 number, int4 incr, int4 start, int4 end, int4 *count)
{
	TEXT	*txt_start, *txt_end;
	int	rc;

	if (rc = check_renumber(number,incr,start,end,&txt_start,&txt_end,count))
	{
		return rc;
	}

	if (0 == *count)
	{
		/*
		**	No line numbers
		*/
		return 0;
	}

	/*
	**	Everything is OK so now really do the renumbering
	*/

	do_renumber(txt_start, txt_end, number, incr);

	return 0;
}

int do_renumber(TEXT *txt_start, TEXT *txt_end, int4 number, int4 incr)
{
	TEXT	*txt;
	int4	newnum;

	newnum = number - incr;
	for (txt=txt_start; txt; txt=txt->next)
	{
		newnum += incr;

		if (lang_type() == LANG_BASIC)
		{
			update_linenum( txt->lineno, newnum );
		}

		txt->lineno = newnum;

		if (txt == txt_end)
		{
			break;
		}
	}

	/*
	**	After renumbering is complete, update the branch values in the linked
	**	list of embedded BASIC line numbers. Added by CIS, 07/13/93 AJA 
	*/
	if (lang_type() == LANG_BASIC)
	{
		update_branch();
	}

	if (vse_num_start_col)
	{
		vse_file_changed = 1;
	}

	return 0;
}

/*
**	Routine:	check_renumber()
**
**	Function:	To check the number and incr before doing a renumber.
**
**	Description:	
**			The start-end don't have to land exactly on actual line numbers.
**
**	Arguments:
**	number		The starting new number (1-999999)
**	incr		The increment (1-999999)
**	start		Range starting line number before renumbering (1-999999)
**	end		Range ending line number before renumbering (1-999999)
**	txt_start_p	The returned starting text pointer
**	txt_end_p	The returned ending text pointer
**	count		Number of lines that would be renumbered.
**
**	Globals:	The text list
**
**	Return:
**	0		Check is OK
**	RESP_NUMBER	Bad number
**	RESP_INCR	Bad incr
**	RESP_START	Bad start
**	RESP_END	Bad end
**	RESP_RANGE	Bad range
**	RESP_NUMINCR	Bad number/incr combination
**
**	Warnings:
**			If there are no line numbers it returns a 0 and start or end text 
**			pointers to NULL.
**
**	History:	
**	03/11/94	Written by GSL
**
*/
static int check_renumber(int4 number, int4 incr, int4 start, int4 end, TEXT **txt_start_p, TEXT **txt_end_p, int4 *count)
{
	TEXT	*txt_start, *txt_end, *txt;
	int4	newnum;

	*count = 0;

	if (number < 1 || number > 999999) return RESP_NUMBER;
	if (incr   < 1 || incr   > 999999) return RESP_INCR;
	if (start  < 1 || start  > 999999) return RESP_START;
	if (end    < 1 || end    > 999999) return RESP_END;
	if (start > end) return RESP_RANGE;

	/*
	**	Find start
	*/

	if (!text_first) 
	{
		*txt_start_p = NULL;
		*txt_end_p = NULL;
		return 0;
	}

	txt_start = txt_end = NULL;

	for (txt_start=text_first; txt_start; txt_start=txt_start->next)
	{
		if (txt_start->lineno >= start)
		{
			break;
		}
	}

	if (!txt_start) return 0;

	if (txt_start->prev && txt_start->prev->lineno >= number)
	{
		return RESP_NUMBER;
	}

	/*
	**	Find the end plus keep track of new line numbers
	*/
	newnum = number;
	for (txt=txt_start; txt; txt=txt->next)
	{
		if (txt->lineno > end)
		{
			break;
		}

		*count += 1;
		newnum += incr;
		txt_end = txt;

		if (txt->lineno == end)
		{
			break;
		}
	}

	if (!txt_end) return 0;

	if (txt_end->next && txt_end->next->lineno <= newnum)
	{
		return RESP_NUMINCR;
	}

	if (newnum > 999999) return RESP_NUMINCR;

	/*
	**	Everything is OK
	*/

	*txt_start_p = txt_start;
	*txt_end_p = txt_end;	

	return 0;
}

/*
**	Routine:	validate_numincr()
**
**	Function:	Validate the NUMBER and INCR fields for a renumber.
**
**	Description:	{Full detailed description}...
**
**	Arguments:
**	number_field	The NUMBER field
**	number		The returned number
**	incr_field	The INCR field
**	incr		The returned incr
**
**	Globals:	None
**
**	Return:
**	0		OK
**	RESP_NUMBER	Bad NUMBER
**	RESP_INCR	Bad INCR
**
**	Warnings:	None
**
**	History:	
**	03/14/94	Written by GSL
**
*/
int validate_numincr(char *number_field, int4 *number, char *incr_field, int4 *incr)
{
	char	lineno[17];

	memcpy(lineno,number_field,6);
	lineno[6] = (char)0;
	untrunc(lineno,16);

	if (validate_linenum(lineno, number) || *number < 1)
	{
		return RESP_NUMBER;
	}

	memcpy(lineno,incr_field,6);
	lineno[6] = (char)0;
	untrunc(lineno,16);

	if (validate_linenum(lineno, incr) || *incr < 1)
	{
		return RESP_INCR;
	}

	return 0;
}

/*
**	Routine:	vse_xcopy()
**
**	Function:	Special command menu EXTERNAL COPY.
**
**	Description:	Issue the XCOPY getparm, validate the values then call vse_xcopy_copy()
**			to do the external copy.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/23/94	Written by GSL
**
*/
int vse_xcopy(void)
{
	char	file_field[VSE_FILENAME_LEN+1], library_field[VSE_LIBNAME_LEN+1], volume_field[VSE_VOLNAME_LEN+1];
	char	sysname_field[VSE_SYSNAME_LEN+1];
	char	start_field[17], end_field[17], target_field[17], modcode_field[4];	
	char	message_field[81];
	int	message_code;
	int	resp, rc;
	int4	start_line, end_line, target_line;

	/*
	**	Get the target line number.
	*/
	rc = get_line_and_index((int)(vse_save_row),&target_line);
	if (rc < 0)
	{
		target_line = 0;
		CLEAR_FIELD(target_field);
	}
	else
	{
		sprintf(target_field,"%06ld          ",(long)target_line);
	}

	memset(file_field,' ',sizeof(file_field));
	memcpy(library_field,vse_gp_input_library,sizeof(library_field));
	memcpy(volume_field,vse_gp_input_volume,sizeof(volume_field));
	memset(sysname_field,' ',sizeof(sysname_field));
	CLEAR_STRING(end_field);

	strcpy(start_field,"ALL             ");
	strcpy(modcode_field,"NO ");

	message_field[0] = (char)0;
	message_code = 0;
	resp = 0;

	/*
	**	Loop until the XCOPY is done or the user aborts with PF1
	*/
	for(;;)
	{
		untrunc(sysname_field,sizeof(sysname_field)-1);

		rc = vse_xcopy_gp(file_field, library_field, volume_field, sysname_field, 
		 		  start_field, end_field, target_field, modcode_field,
		 		  message_field, message_code, resp);

		if (rc)
		{
			/*
			**	PF1 to ABORT
			*/
			return 1;
		}

		message_code = 0;
		resp = 0;

		trunc(sysname_field);
		if(!exists(sysname_field))
		{
			strcpy(message_field,"\224SORRY\204- File not found.");
			message_code = 1;
		}

		if (!message_code)
		{
			if (resp = validate_range(start_field,&start_line,end_field,&end_line))
			{
				strcpy(message_field,vse_err(resp));
				message_code = 1;
			}
		}

		if (!message_code)
		{
			if (validate_linenum(target_field,&target_line) || -2 == target_line || -1 == target_line)
			{
				resp = RESP_TARGET;
				strcpy(message_field,vse_err(resp));
				message_code = 1;
			}
			else if (-3 == target_line)
			{
				target_line = 1;
			}
			else if (-4 == target_line)
			{
				target_line = 999999;
			}
		}

		if (!message_code)
		{
			int	keep_modcode;

			keep_modcode = (0==memcmp(modcode_field,"YES",3));

			/*
			**	We've done all the validating we can do so far.
			**	Attempt to do the copy.
			*/
			message_code = vse_xcopy_copy(sysname_field,start_line,end_line,target_line,message_field,keep_modcode);
			if (!message_code)
			{
				return 0;
			}

			if (message_code < 0)
			{
				/*
				**	Handle special return codes
				*/
				resp = -message_code;
				message_code = 1;
			}
		}
	}
}

/*
**	Routine:	vse_xcopy_copy()
**
**	Function:	Do the actual external copy.
**
**	Description:	Open up the xfile and read thru it creating a list of records to be copied.
**			Find the target in the work file and insert the list at that point.
**			Check for error conditions at every point.
**
**	Arguments:
**	sysname		The name of the xfile to copy.
**	start_line	The START line number in xfile.
**	end_line	The END line number in xfile.
**	target_line	The TARGET line number in the work file.
**	message		The formatted error message if return not zero.
**	keep_modcode	Flag to keep the modcode from the xfile.
**
**	Globals:	YES
**	The language constants.
**	text_first/last
**	scr_first
**
**	Return:
**	0		Specified lines were copied.
**	1		Error, message is set.
**	2		Invalid numbers in xfile (use as message_code to vse_xcopy_gp())
**	-RESP_START	Error, message is set, respecify START.
**
**	Warnings:	I'm not sure the BASIC line numbering is correct.
**
**	History:	
**	03/23/94	Written by GSL
**
*/
static int vse_xcopy_copy(char *sysname, int4 start_line, int4 end_line, int4 target_line, char *message, int keep_modcode)
{
	FILE 	*ff;
	int 	rc, idx;
	TEXT	*txt, *list_start, *list_end, *target;
	int4	lastnum, linecount;
	int	bad_line_numbers;

	strcpy(message,"");

	/*
	**	Reset the myfgets() stuff used by load_txt_line() and was_line_too_long()
	*/
	myfgets(NULL,0,NULL);

        ff = fopen(sysname,"r");
	if (!ff)
	{
		strcpy(message,"\224SORRY\204- Unable to open file for reading.");
		return(1);
	}

	rc = 0;
	bad_line_numbers = 0;
	linecount = 0;
        lastnum = 0;

	list_start = list_end = NULL;

	/*
	**	Read thru the file and create a list of the lines to be copied
	*/
	for(;;)
	{
		if (load_txt_line(ff,&txt) || !txt)
		{
			break;
		}

		/*
		**	Assign and check line numbers
		*/
		if (!vse_num_start_col)
		{
			/*
			**	For un-numbered language types use relative numbering.
			*/
			txt->lineno = lastnum += 1;
		}
		else if (txt->lineno <= lastnum)
		{
			/*
			**	Bad line numbers
			*/
			txt->lineno = lastnum = 0;
		}
		else
		{
			lastnum = txt->lineno;
		}

		if (lastnum < 1 || lastnum > 999999)
		{
			/*
			**	File needs renumbering.
			*/
			bad_line_numbers = 1;
			break;
		}

		if (txt->lineno > end_line)
		{
			/*
			**	Finished with file.
			*/
			break;
		}

		if (txt->lineno >= start_line)
		{
			linecount += 1;

			if (!list_start)
			{
				/*
				**	Start the list
				*/
				txt->next = txt->next = NULL;
				list_start = list_end = txt;
			}
			else
			{
				/*
				**	Add to end of list
				*/
				txt->next = NULL;
				txt->prev = list_end;
				list_end->next = txt;
				list_end = txt;
			}

			if (vse_mod_width)
			{
				if (!keep_modcode)
				{
					/*
					**	Don't keep the old modcodes, add new ones.
					*/
					add_modcode(list_end);
				}
			}
		}
		else
		{
			free_one_text(txt);
		}

		txt = NULL;
	}

        fclose(ff);

	if (txt)
	{
		/*
		**	Free the last unused txt
		*/
		free_one_text(txt);
	}

	if (was_line_too_long())
	{
		/*
		**	The xfile has long lines that need to be split.
		*/
		if (vse_longline_gp(sysname))
		{
			free_text_list(list_start);
			strcpy(message,"\224SORRY\204- File has long lines that need to be split.");
			return(1);
		}
	}

	if (bad_line_numbers)
	{
		/*
		**	Invalid linenumbers
		*/
		free_text_list(list_start);
		return(2);
	}

	if (0==linecount)
	{
		/*
		**	No lines found
		*/
		free_text_list(list_start);
		strcpy(message,"\224SORRY\204- The START of the range was not found in the file.");
		return( -RESP_START );
	}

	/*
	**	Find the TARGET in the work file.
	*/
	if (!text_first || (text_first && target_line < text_first->lineno))
	{
		target_line = 0;
		target = NULL;
	}
	else
	{
		for (txt=text_first; txt; txt = txt->next)
		{
			if (txt->lineno > target_line)
			{
				break;
			}
			target = txt;
		}
	}

	/*
	**	Check if work file needs renumbering
	*/
	if (text_first)
	{
		int	needs_renumbering;

		needs_renumbering = 0;

		if (0==target_line && linecount >= text_first->lineno)
		{
			needs_renumbering = 1;
		}
		else if (target && target->next && (linecount >= target->next->lineno - target->lineno))
		{
			needs_renumbering = 1;
		}
		else if (target && !target->next && (linecount >= 999999 - target->lineno))
		{
			needs_renumbering = 1;
		}

		if (needs_renumbering)
		{
			/*
			**	The work file needs to be renumbered.
			**	Cancel the current operation and ask to renumber.
			*/
			free_text_list(list_start);
			ask_renumber();
			return 0;
		}
	}

	/*
	**	Hookup the copied block.
	*/
	if (!text_first)
	{
		text_first = list_start;
		text_last = list_end;
		scr_first = text_first;
	}
	else if (0==target_line)
	{
		list_start->prev = text_first->prev;
		list_end->next = text_first;
		list_end->next->prev = list_end;
		text_first = list_start;
		scr_first = list_start;
	}
	else
	{
		list_start->prev = target;
		list_end->next = target->next;
		if (list_end->next)
			list_end->next->prev = list_end;
		target->next = list_start;

		if (text_last==target)
			text_last=list_end;

		/*
		**	Reposition screen so that Target and at least one moved/copied
		**	line are on the screen.
		*/
		txt = target;
		for (idx = 0; idx < VSE_EDIT_ROWS - 1; ++idx)
		{
			if (ed_txt[idx] == target)
			{
				/*
				**	Target is on screen
				*/
				txt = scr_first;
				break;
			}
		}
		scr_first = txt;
	}

	if ( lang_type() == LANG_BASIC )
	{
		/*
		**	Handle BASIC line numbering stuff.
		**	I'm not positive how this should be done, or if this is correct.
		*/
		txt = list_start;
		for(;;)
		{
			find_linenum( txt );
			if (txt == list_end)
			{
				break;
			}
			txt = txt->next;
		}
	}

	renumber_range(list_start,list_end);

	vse_file_changed = 1;

	return 0;
}


/*
**	History:
**	$Log: vsemov.c,v $
**	Revision 1.9  1996/09/03 22:24:08  gsl
**	drcs update
**	
**
**
*/
