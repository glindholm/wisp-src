/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

#include <stdio.h>
#include "dbsc.h"
#include "dglb.h"
#include "shrthand.h"
#include "iocode.h"
#include "cobioblk.h"
#include "dtype.h"
#include "dmnt.h"
#include "intdef.h"
#include "kcsifunc.h"


static char scr22[]=" DATENTRYDATENTRY-SCREEN                 D          ";
static char scr20[]=" DATENTRYDATENTRY-SCREEN                 ";
static char *start_scr;

static void bld_main_scr();
static void add_message();
static void add_footer();
static void add_fld_prompt(FIELD *fld);
static void add_fld_item(FIELD *fld);
static void dmntl(char *mode,char *idx);
static void add_dte_logo();
static void end_main_screen();
static void add_fld(FIELD *fld);
static void add_fld_pic(FIELD *fld);
static void add_prompt(int row,int col,char *prompt,char **fld,char **pfac);
static void add_dte_scr_header();


void DMNT(char *hdrs,char *t1,char *t2,char *cblock,char *dblock,char *mode,char *idx)
{
/* This is all debugging
	int  i,j;
	FIELD *f;

	printf("Prepare for display\r\n");
	getchar();
	for(i = 0,j = 0;(dtefld[i].name[0] > ' ') && (i < MAX_FIELDS); ++i,++j)
		{
		f = &dtefld[i];
		if(j == 18)
			{
			printf("Enter to continue\r\n");
			getchar();
			j = 0;
			}
		printf("%s row %d %d col %d %d ",
			f->name,f->prow,f->frow,f->pcol,f->fcol);
		printf("key %d dups %d ",f->keynum,f->dups);
		printf("len %d %d occ=%d\r\n",
			f->edit_len,f->plen,f->occurrence);
		}
	printf("Enter to continue\r\n");
	getchar();
	for(i = 0; i < 25; ++i)
		printf("\r\n");
 Debug Ends here */

/* Save all the good stuff*/
	dte_cf_t1 = t1;
	dte_cf_t2 = t2;
	dte_cf_hdrs = hdrs;
	dte_cio_block = cblock;
	dte_dio_block = dblock;
	dmntl(mode,idx);
}



static void dmntl(char *mode,char *idx)
{
	int pfkey;
	if(dte_new_screen)
		bld_main_scr();
	dte_new_screen = 0;
	while(1)
		{
		switch(*mode)
			{
			case 'A':
				pfkey = dte_add_records();
				if(pfkey == 9)
					*mode = 'B';
				break;
			case 'B':
				pfkey = dte_change_records(idx,mode);
				if(pfkey == 9)
					*mode = 'A';
				break;
			case 'C':
				pfkey = dte_delete_records(idx,mode);
				break;
			}
		if(pfkey == 16)
			break;
		}
}

/*----
		SCREEN BUILDING LOGIC
------*/
static void bld_main_scr()
{
	int i;

	dte_scr = (char*)dte_main_scr;
	add_dte_scr_header();
	add_dte_logo();
	add_message();
	if (dte_file_is_relative()) 	/* relfiles 25-Mar-1990*/
	    add_fld(&dte_relative_record);	/* relfiles 25-Mar-1990*/
	for(i = 0; dtefld[i].name[0] > ' '; ++i)
		{
		if(dtefld[i].frow)
			add_fld(&dtefld[i]);
		}
	add_footer();
	end_main_screen();
}

void dte_load_footer(char *prompt)
{
	memset(dte_trailer,' ',79);
	memcpy(dte_trailer,prompt,strlen(prompt));
}

static void add_text_prompt(int row,int col,char *prompt)
{
	char *junk;

	add_prompt(row,col,prompt,&junk,&junk);
}

static void end_main_screen()
{
	if(LIB_VERS == 22)
		*dte_scr++ = '.';
	else
		{
		*dte_scr++ = (char)0xff;
		*dte_scr++ = (char)0xff;
		}
}

/*----
Copy the header data into the screen.
------*/
static void add_dte_scr_header()
{
	int len;

	if(LIB_VERS == 22)
		start_scr = scr22;
	else
		start_scr = scr20;

	start_scr[0] = LIB_VERS;
	memcpy(dte_scr,start_scr,len = strlen(start_scr));
	dte_scr += len;
}
static void add_dte_logo()
{
	
	static char deu[]=
       "Data Entry Utility  File: XXXXXXXX  Library: XXXXXXXX  Volume: XXXXXX";

/*
	memcpy(&deu[26],data_file._name,8);
	memcpy(&deu[45],data_file._library,8);
	memcpy(&deu[63],data_file._volume,6);
*/
	memcpy(&deu[26],&dte_dio_block[NAME_POS],NAME_LEN);
	memcpy(&deu[45],&dte_dio_block[LIBRARY_POS],LIBRARY_LEN);
	memcpy(&deu[63],&dte_dio_block[VOLUME_POS],VOLUME_LEN);
	
	add_text_prompt(1,2,deu);
}

/*----
Create a 70 byte message field at row 2.
------*/
static void add_message()
{
	char *junk;

	add_prompt(2,2,
     "                                                                      ",
	&dte_message,&junk);
}

static void add_footer()
{
	char *junk;
	char buf[80];

	memset(buf,' ',sizeof(buf));
	buf[sizeof(buf)-1] = '\0';

	add_prompt(24,2,buf,&dte_trailer,&junk);
}

/*----
Add the field as a prompt and a data item.
------*/
static void add_fld(FIELD *fld)
{
	add_fld_prompt(fld);
	add_fld_item(fld);
}

/*----
The name of a field is either the name or the name followed by
an occurrence.
	CUST	or	CUST (02)
------*/
static void add_fld_prompt(FIELD *fld)
{
	char prompt[80];

	if(fld->occurrence)
		sprintf(prompt,"%s (%02d)",fld->name,fld->occurrence);
	else
		sprintf(prompt,"%s",fld->name);
	add_prompt(fld->prow,fld->pcol,prompt,&fld->fld,&fld->pfac);	
}

static void add_fld_item(FIELD *fld)
{


/* First work out the correct fac. If Space or less then default */
/* Number of decimals is forced to zero if Alpha or Binary field */
	if(fld->fac < 0x40 )
		{
		if(fld->type == 'C')
			{
			fld->fac = ALPHA_FAC;
			fld->dec = 0;
			}
		else
		if( fld->type == 'B')
			{
			fld->dec = 0;
			if(fld->bin == 0)
				fld->fac = ALPHA_FAC;
			else
				fld->fac = NUMERIC_FAC;
			}
		else
			fld->fac = NUMERIC_FAC;
		}
	else
	if(fld->type == 'C')
		;  /* Do nothing */
	else
	if(	(fld->type == 'B')	&&	(fld->bin == 0)	)
		;	/* Do nothing */
	else
		/* force a numeric fac & retain other attributes */
		{
		fld->fac &= 0xfe;	/* Mask off uplow bit */
		fld->fac |= 0x02;	/* Mask on numeric bit */
		}

	sprintf(dte_scr,"L5R%dC%dP{",fld->frow,fld->fcol);
	dte_scr += strlen(dte_scr);
	add_fld_pic(fld);
	sprintf(dte_scr,"};");	
	dte_scr += strlen(dte_scr);

	*dte_scr++ = fld->fac;

	fld->fld = dte_scr;
	memset(dte_scr,' ',fld->edit_len);
	dte_scr += fld->edit_len;
}

static void add_fld_pic(FIELD *fld)
{
	char pic[101];


	KCSI_get_pic_len(	pic,
		    	fld->type,
			fld->len,
			fld->dec,
			fld->bin);

	strcat(dte_scr,pic);
			
	dte_scr += strlen(dte_scr);

}

/*----
This routine is not used at all in the current version and is probably
not needed.
No commas or dollars allowed.
------*/
#ifdef NOT_USED
static int add_edit_flags(FIELD *fld,char *edits)
{
	char *zedits;
	int dollars,commas,wrk_len,mask,commac;

/* No dollars or commas for now. */
	commas = dollars = 0;

/* Create edit masks only if zero suppressed or comma'd*/
	if((fld->supp) || (commas))
		*edits &= 0x80;
	else
		return(4);

/* wrk_len is everything left of the decimal */
	wrk_len = fld->edit_len;
	if(fld->dec)
		wrk_len -= (fld->dec + 1);
	if(fld->sign)
		--wrk_len;
/*
 * This logic is forced to inactive
 * for now by earlier setting dollars
 * and commas to zero.
 * When commas are included every 4th character will be a comma
 */
	if(commas)
		commac = 4;
	else
		commac = 3;
/* Move to the last byte of the edits */
	edits += 3;
/* Zedits start after 4 bytes of edit */
	zedits = edits + 4;
	mask = 0x80;
/* Leave 1 zero left of the decimal unsuppressed*/
	while(wrk_len > 1)
		{
		if(mask == 0)
			{
			mask = 0x80;
			--zedits;
			--edits;
			}
		if(dollars)
			{
			mask >>= 1;
			dollars = 0;
			}
		else
		if(wrk_len % commac)		/*Not on comma char*/
			{
			if(commas)
				*edits &= (~mask);	/* Comma off */
			if(fld->supp)
				*zedits |= mask;		/* Z on */
			}
		else
			{
			if(commas)
				*edits |= mask;		/* Comma on */
			if(fld->supp)
				*zedits &= (~mask);	/* Z off */
			}
		--wrk_len;
		mask >>= 1;
		}
       return(8);
}
#endif /* NOT_USED */

/*----
A prompt is a simple field containing no modifiable characteristics
------*/
static void add_prompt(int row,int col,char *prompt,char **fld,char **pfac)
{
	int len;

	sprintf(dte_scr,"L5R%dC%dP{%s(%d)};",
		row,col,"X",len = strlen(prompt));

	dte_scr += strlen(dte_scr);
	*pfac = dte_scr;
	*dte_scr++ = DIM_FAC;
	*fld = dte_scr;
	strcpy(dte_scr,prompt);
	dte_scr += len;
	
}


/*
**	History:
**	$Log: dmnt.c,v $
**	Revision 1.14  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.12  2002/10/24 15:48:33  gsl
**	Make globals unique
**	
**	Revision 1.11  2002/10/24 14:20:39  gsl
**	Make globals unique
**	
**	Revision 1.10  2002/10/23 21:07:27  gsl
**	make global name unique
**	
**	Revision 1.9  2002/10/23 20:39:09  gsl
**	make global name unique
**	
**	Revision 1.8  2002/08/01 16:49:53  gsl
**	type warnings
**	
**	Revision 1.7  2002/07/25 15:20:28  gsl
**	Globals
**	
**	Revision 1.6  2002/07/12 17:17:00  gsl
**	Global unique WL_ changes
**	
**	Revision 1.5  1996/09/26 21:22:44  gsl
**	fix a memory overwrite in add_footer() which was wiping out the stack on NT
**	
**	Revision 1.4  1996-09-17 16:45:35-07  gsl
**	drcs update
**
**
**
*/
