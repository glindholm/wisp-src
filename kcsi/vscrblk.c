/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/



#include <stdio.h>
#include <ctype.h>
#include "vscrglb.h"
#include "shrthand.h"
#include "kcsifunc.h"
#include "create.h"

static int fld_pf,blk_pf,blk_no,fld_no;


static char msg_fld[81];


static void cr_get_fields(void);
static void set_fill(CR_FLD *fld);
static int cr_check_fill(void);
static int cr_show_unfilled(void);
static void clear_fill(void);
static int add_unfilled(int idx);
static int cr_get_field(void);
static void init_cr_get_field(void);
static int enter_cr_get_field(void);
static int val_get_field(char *rec_len);
static void val_file_name(void);
static void val_file_type(void);
static void val_record_len(char *rec_len);
static void val_file_exists(void);
static int cr_get_lit_field(void);
static int val_cr_get_lit(char *pos, char *len);
static void val_cr_lit_len(char *len);
static CR_FLD *save_cr_fld(void);
static void save_cr_blk(void);
static int cr_get_pad_field(void);
static int val_cr_get_pad(char* pad, char *pos, char *len);
static void val_cr_pad_pad(char *pad);
static void val_cr_pos(char *pos);
static void val_cr_len(char *len);
static int cr_get_num_field(void);
static int val_cr_get_num(char *begval, char *endval, char *incr, char *pos, char *len, char *rpt);
static void val_cr_num_beg(char *begval);
static void val_cr_num_end(char *endval);
static void val_cr_num_incr(char *incr);
static void val_cr_num_rpt(char *rpt);
static int cr_must_count(void);
static int forces_count(CR_FLD *fld);
static int blk_is_counted(void);
static int cr_get_count(void);
static int val_cr_get_count(char *count);
static void cr_free_a_fld(CR_FLD *fld);
static void cr_free_a_blk(CR_BLK *blk);
static void cr_add_blk(CR_BLK *blk);
static void cr_add_fld(CR_BLK *blk, CR_FLD *fld);
static void cr_no_mem_exit(char *type);
static CR_FLD *cr_new_fld(void);
static KCSIO_BLOCK *cr_new_kcsio(void);
static int cr_get_field_type(void);
static void cr_blk_and_fld(int row, int col);
static CR_BLK *cr_new_blk(void);

/*============================================================================*/

#define CR_MEM_ALLOC_TYPE( t )		((t*)cr_mem_alloc( sizeof( t ), #t ))
#define CR_MEM_ALLOC_STR( s, x )	((char *)cr_mem_alloc( s, x ))
#define CR_MEM_FREE( p, x )		{ cr_mem_free( p, x ); p = NULL;}


static int cr_mem_alloc_cnt = 0;
static void *cr_mem_alloc(size_t size, const char *text)
{
	void *ptr;
	ptr = calloc(1,size);

	cr_mem_alloc_cnt++;
	kcsitrace(2,"CREATE","MEM_ALLOC","%s cnt=%d size=%u ptr=0x%08X",
		text, cr_mem_alloc_cnt, size, ptr);
	return ptr;
}

static void cr_mem_free(void *ptr, const char *text)
{
	if (cr_mem_alloc_cnt == 0)
	{
		kcsitrace(4,"CREATE",
			"MEM_FREE","Attempting to FREE more memory then was allocated");
	}
	kcsitrace(2,"CREATE","MEM_FREE","%s cnt=%d ptr=0x%08X",
		text, cr_mem_alloc_cnt, ptr);
	cr_mem_alloc_cnt--;
	free(ptr);
}
/*============================================================================*/

/*----
Work area for each field before it is created
------*/

void cr_get_blocks(void)
{
	cr_free_blks();
	blk_pf = blk_no = 0;
	while(1)
		{
		fld_no = 0;
		cr_get_fields();
		if( (fld_no == 0) &&
		    (fld_pf == 16) )
			{
			blk_pf = 16;
			break;
			}
		++blk_no;
		}
}

static void cr_get_fields(void)
{
	int rc;

	while(1)
		{
		rc = cr_get_field();
		if(rc == 4)
			continue;
		if(fld_pf == 16)
			{
			if(fld_no > 0)
				{
				if(cr_check_fill() != 16)
					continue;
				if(cr_must_count() == 1)
					continue;
				}
			break;
			}
		if(fld_pf == 1)
			continue;
		++fld_no;
		}
}


/*----
Checks for a filling gap(s) and displays any found
------*/
static char fill_map[2049];
static char fill_start[5][5], fill_len[5][5];
static int fill_idx;

static void set_fill(CR_FLD *fld)
{
	int idx;

	for(idx = 0; idx < fld->len; ++idx)
		{
		fill_map[idx + fld->outpos] = 'X';
		}
}

static int cr_check_fill(void)
{
	CR_BLK *blk;

	blk = ll_last((LLTYPE*)cr_blk);
	memset(fill_map,' ',2048);
	ll_all(blk->fld,set_fill);
	return(cr_show_unfilled());
}


static int cr_show_unfilled(void)
{
	int idx,rc;
	long pf;

	clear_fill();

	for(idx = 0; idx < cr_out.ofile._record_len; ++idx)
		{
		if(fill_map[idx] != 'X')
			{
			idx = add_unfilled(idx);
			}
		}
	if(fill_idx == 0)
		return(16);

/* Otherwise we give em a GETPARM */
	WL_wpload();
	GP_pfkeys = GP_PF_01|GP_PF_16;
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	GPSTD("GAP     ","CREATE");
	GPCTEXT(
	"One or more bytes of the output record have not been defined.",
	9,2);
	GPCTEXT(
	"If not defined, their values will be set to NULs (hex 00).",
	10,2);

	GPCTEXT("The first five undefined regions are:",12,2);
	GPCTEXT("Start          Length",13,20);
	for(idx = 0; idx < 5; ++idx)
		{
		GPCTEXT(fill_start[idx],idx+14,21);
		GPCTEXT(fill_len[idx],idx+14,36);
		}

	GPCTEXT("Press (ENTER) or (1) to return to field selection.",22,2);
	GPCTEXT("or (16) to End this block.",23,2);

	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	rc = pf;
	return(rc);
}

static void clear_fill(void)
{
	int idx;

	fill_idx = 0;

	for(idx = 0 ; idx < 5; ++idx)
		{
		strcpy(fill_start[idx],"    ");
		strcpy(fill_len[idx],"    ");
		}

}



static int add_unfilled(int idx)
{
	int len;

	if(fill_idx < 5)
		sprintf(fill_start[fill_idx],"%04d",(idx + 1));
	for( len = 0 ; idx < cr_out.ofile._record_len; ++idx, ++len)
		{
		if(fill_map[idx] == 'X')
			break;
		}
	if(fill_idx < 5)
		sprintf(fill_len[fill_idx],"%04d",len);
	++fill_idx;
	
	return(--idx);
}




static int cr_get_field(void)
{
	int save_pf;

	save_pf = fld_pf = cr_get_field_type();
	
	switch(fld_pf)
		{
		case 1:
			fld_pf = cr_get_lit_field();
			break;
		case 2:
			fld_pf = cr_get_pad_field();
			break;
		case 3:
			fld_pf = cr_get_num_field();
			break;
		case 4:
			fld_pf = cr_get_count();
			break;
		case 0:
			fld_pf = cr_get_file_field();
			break;
		case 16:
			break;
		}
	return(save_pf);
}

static int cr_get_field_type(void)
{
	init_cr_get_field();
	return(enter_cr_get_field());
}

static void init_cr_get_field(void)
{
	memset(&CR_wkfb,0,sizeof(CR_wkfb));
	memset(&CR_wfld,0,sizeof(CR_wfld));
	strcpy(CR_wkfb._name,"        ");
	strcpy(CR_wkfb._library,"        ");
	strcpy(CR_wkfb._volume,"      ");
	CR_wfld.kfb = &CR_wkfb;
}

static int enter_cr_get_field(void)
{
	int rc;
	long pf;
	char rec_len[5];


	strcpy(CR_wkfb._org,"I");	
	strcpy(rec_len,"    ");
	memcpy(CR_wkfb._library,create_inlib,8);
	memcpy(CR_wkfb._volume,create_invol,6);
	while(1)
		{
		WL_wpload();
		GP_pfkeys=GP_PF_01|GP_PF_02|GP_PF_03|GP_PF_04|GP_PF_16;
		WL_wswap(&GP_pfkeys);
		GPSETUP();
		GPSTD("INPUT   ","CREATE");
		cr_blk_and_fld(10,29);
		GPCTEXT("Select PFKey for the appropriate field type:",11,2);
		GPCTEXT("(ENTER) to specify fields from a file.",12,2);
		GPFLVUC(CR_wkfb._name,
			CR_wkfb._library,
			CR_wkfb._volume,
			14,2);
		GPCTEXT("File",15,2);
		GPKW("TYPE    ",CR_wkfb._org,1,15,7,"U");
		GPCTEXT("(I/R/C/B)",15,20);
		GPKW("RECLEN  ",rec_len,4,15,32,"N");
		GPCTEXT("(for Rel/Bin files only)",15,50);
		GPCTEXT("(1)    - Specify a literal string field.",17,2);
		GPCTEXT("(2)    - Specify pad field (single character).",18,2);
		GPCTEXT("(3)    - Specify a numeric sequencing field.",19,2);
		if(fld_no != 0)
			{
			GPCTEXT("(4)    - Set/Reset record count for block.",
				20,2);
			}
		GPCTEXT("(16)   - End definitions for this block.",22,2);
		GPCTEXT(msg_fld,24,2);
		GPPFS(&GP_pfkeys);
		pf = GP_display_and_read();
		strcpy(msg_fld,"");
		rc = pf;
		if(rc == 0)
			{
			if(val_get_field(rec_len))
				break;
			}
		else
			break;
		}
	return(rc);
}

static int val_get_field(char *rec_len)
{
	val_file_name();
	if(msg_fld[0] < '!')
		val_file_type();
	if(msg_fld[0] < '!')
		val_record_len(rec_len);
	if(msg_fld[0] < '!')
		val_file_exists();
	if(msg_fld[0] < '!')
		return(1);
	else
		return(0);
}

static void val_file_name(void)
{
	if(! KCSI_valspec_filled(CR_wkfb._name,
			   CR_wkfb._library,
			   CR_wkfb._volume))
		{
		strcpy(msg_fld,
		"File Specification must be filled in correctly.");
		return;
		}
}

static void val_file_type(void)
{
	if( (CR_wkfb._org[0] != 'I') &&
	    (CR_wkfb._org[0] != 'C') &&
	    (CR_wkfb._org[0] != 'B') &&
	    (CR_wkfb._org[0] != 'R')  )
		{
		strcpy(msg_fld,"Must be Indexed, Rel, Consec or Binary.");
		return;
		}
}

static void val_record_len(char *rec_len)
{
	int value;

	value = atoi(rec_len);

	if( (CR_wkfb._org[0] == 'R') ||
	    (CR_wkfb._org[0] == 'B') )
		{
		if(value < 1)
			{
			strcpy(msg_fld,
	"Record length must be specified for Relative and Binary Files.");
			return;
			}
		}
	if(CR_wkfb._org[0] == 'C')
		value = 2048;
	if(CR_wkfb._org[0] == 'I')
		value = 0;
	CR_wkfb._record_len = value;
}

static void val_file_exists(void)
{
	static 	char wrecord[2049];

	CR_wkfb._record = wrecord;
	if(!KCSI_ckexists(CR_wkfb._name,CR_wkfb._library,CR_wkfb._volume))
		{
		strcpy(msg_fld,"File Not Found");
		return;
		}
	strcpy(CR_wkfb._prname,"INFILE  ");

	cr_fileio(&CR_wkfb, OPEN_INPUT);
	if(CR_wkfb._status != 0)
		{
		strcpy(msg_fld,"File not Found or specified incorrectly");
		return;
		}
	cr_fileio(&CR_wkfb, FILE_INFO);
	cr_fileio(&CR_wkfb, CLOSE_FILE);
}


static char blk_and_fld_work[81];

static void cr_blk_and_fld(int row, int col)
{
	sprintf(blk_and_fld_work,"Block %3d. Field %3d.",blk_no+1,fld_no+1);
	GPCTEXT(blk_and_fld_work,row,col);
}

static int cr_get_lit_field(void)
{
	char pos[6],len[3];
	int rc;
	long pf;

/* init the fields */
	memset(CR_wfld.string,' ',64);
	strcpy(pos,"     ");
	strcpy(len,"  ");
/* enter_the_fields */
	while(1)
		{
		WL_wpload();
		GP_pfkeys = GP_PF_01;
		WL_wswap(&GP_pfkeys);
		GPSETUP();
		GPSTD("LITERAL ","CREATE");
		cr_blk_and_fld(10,2);
		GPCTEXT("Please Supply the literal specifications and press (ENTER).",12,2);
		GPCTEXT("Literal String:",14,2);
		GPKW("STRING  ",CR_wfld.string,64,15,2,"C");
		GPCTEXT("Starting position in the record.",17,2);
		GPKW("POSITION",pos,5,17,45,"N");
		GPCTEXT("Length of string (if not non-blank length).",19,2);
		GPKW("LENGTH  ",len,2,19,45,"N");
		GPCTEXT("Or Select:",22,2);
		GPCTEXT("(1) Return to field selection menu:",23,2);
		GPCTEXT(msg_fld,24,2);
		GPPFS(&GP_pfkeys);
		pf = GP_display_and_read();
		strcpy(msg_fld,"");
		rc = pf;
		if(rc == 1)
			return(rc);
		if(val_cr_get_lit(pos,len))
			{
			break;
			}
		}
	CR_wfld.type = CFIELD_STRING;
	save_cr_fld();
	return(rc);
}

static int val_cr_get_lit(char *pos, char *len)
{
	val_cr_pos(pos);
	if(msg_fld[0] < '!')
		val_cr_lit_len(len);
	if(msg_fld[0] < '!')
		return(1);
	else
		return(0);
}

#ifdef NOT_USED
static void val_cr_lit_pos(char *pos)
{
	int value;

	value = atoi(pos);
	if(value < 1)
		{
		strcpy(msg_fld, "Position must be entered.");
		return;
		}
	--value;
	if(value > cr_out.ofile._record_len)
		{
		strcpy(msg_fld, "Position not within output record.");
		return;
		}
	CR_wfld.outpos = value;
}
#endif /* NOT_USED */

static void val_cr_lit_len(char *len)
{
	int value,idx;

	value = atoi(len);
	if(value == 0)
		{
		for(idx = 63; idx >= 0; --idx)
			{
			if(CR_wfld.string[idx] != ' ')
				break;
			}
		value = idx + 1;
		}
	if( (CR_wfld.outpos + value) > cr_out.ofile._record_len)
		{
		strcpy(msg_fld,"LENGTH extends beyond end of record.");
		return;
		}
	if(value == 0)
		{
		strcpy(msg_fld,"Field has no length.");
		return;
		}
	CR_wfld.len = value;

}

void cr_save_in_file(void)
{
	CR_FLD *fld;

	fld = save_cr_fld();
	fld->kfb = cr_new_kcsio();
/*
	CR_wkfb._record = cr_in_rec;
*/
	memcpy(fld->kfb,&CR_wkfb,sizeof(CR_wkfb));
}

static CR_FLD *save_cr_fld(void)
{
	CR_BLK *blk;
	CR_FLD *fld;

	save_cr_blk();

/* We are always adding fields to the last block */
	blk = ll_last((LLTYPE*)cr_blk);
/* We need a new field appended to the block */
	fld = cr_new_fld();
	cr_add_fld(blk,fld);
/* Copy what we have saved so far */
	memcpy(fld,&CR_wfld,sizeof(CR_wfld));
	return(fld);
}

static void save_cr_blk(void)
{
	CR_BLK *blk;
/* if this is the first field, we need a new block */
	if(fld_no == 0)
		{
		blk = cr_new_blk();
		blk->count = -1;
		blk->counter = 0;
		cr_add_blk(blk);
		}
}

static int pad_hex;

static int cr_get_pad_field(void)
{
	char pos[6],len[6],pad[3];
	int rc;
	long pf;

/* init the fields */
	memset(CR_wfld.string,' ',64);
	strcpy(pos,"     ");
	strcpy(len,"     ");
	strcpy(pad,"  ");
	pad_hex = 0;
/* enter_the_fields */
	while(1)
		{
		WL_wpload();
		GP_pfkeys = GP_PF_01|GP_PF_02;
		WL_wswap(&GP_pfkeys);
		GPSETUP();
		GPSTD("PAD     ","CREATE");
		cr_blk_and_fld(10,2);
		GPCTEXT("Please Supply the pad character and press (ENTER).",12,2);
		GPCTEXT("Pad Character",15,2);
		if(pad_hex)
			{
			GPKW("PAD     ",pad,2,15,45,"U");
			}
		else
			{
			GPKW("PAD     ",pad,1,15,45,"C");
			}
		GPCTEXT("Starting position in the record.",17,2);
		GPKW("POSITION",pos,5,17,45,"N");
		GPCTEXT("Length of pad field.",19,2);
		GPKW("LENGTH  ",len,4,19,45,"N");
		GPCTEXT("Or Select:",22,2);
		GPCTEXT("(1) Return to field selection menu",23,2);
		if(pad_hex)
			{
			GPCTEXT("(2) Switch to ASCII Mode",23,40);
			}
		else
			{
			GPCTEXT("(2) Switch to HEX Mode",23,40);
			}
		GPCTEXT(msg_fld,24,2);
		GPPFS(&GP_pfkeys);
		pf = GP_display_and_read();
		strcpy(msg_fld,"");
		rc = pf;
		if(rc == 2)
			{
			pad_hex = !pad_hex;
			strcpy(pad,"  ");
			continue;
			}
		if(rc == 1)
			return(rc);
		if(val_cr_get_pad(pad,pos,len))
			{
			break;
			}
		}
	CR_wfld.type = CFIELD_PAD;
	save_cr_fld();
	return(rc);
}

static int val_cr_get_pad(char* pad, char *pos, char *len)
{
	val_cr_pad_pad(pad);
	if(msg_fld[0] < '!')
		val_cr_pos(pos);
	if(msg_fld[0] < '!')
		val_cr_len(len);
	if(msg_fld[0] < '!')
		return(1);
	else
		return(0);
}

static void val_cr_pad_pad(char *pad)
{
	int value;

/* In ASCII mode anything goes */
	if(!pad_hex)
		{
		CR_wfld.string[0] = *pad;
		return;
		}
/* In hex mode they must be hex digits */
	if((*pad == ' ') &&( *(pad + 1) == ' '))
		{
		strcpy(msg_fld,"Hex value must be entered.");
		return;
		}
	if( (*pad ==  ' ') || (isxdigit((int)*pad)) )
		;
	else
		{
		strcpy(msg_fld,"Hex value must be entered.");
		return;
		}
	++pad;
	if( (*pad ==  ' ') || (isxdigit((int)*pad)) )
		;
	else
		{
		strcpy(msg_fld,"Hex value must be entered.");
		return;
		}

	value = 0;
	sscanf(pad,"%x",&value);
	CR_wfld.string[0] = value;
}


static void val_cr_pos(char *pos)
{
	int value;

	value = atoi(pos);
	if(value < 1)
		{
		strcpy(msg_fld, "Position must be entered.");
		return;
		}
	--value;
	if(value > cr_out.ofile._record_len)
		{
		strcpy(msg_fld, "Position not within output record.");
		return;
		}
	CR_wfld.outpos = value;
}

static void val_cr_len(char *len)
{
	int value;

	value = atoi(len);
	if(value == 0)
		{
		strcpy(msg_fld, "LENGTH must be entered.");
		return;
		}
	if( (CR_wfld.outpos + value) > cr_out.ofile._record_len)
		{
		strcpy(msg_fld,"LENGTH extends beyond end of record.");
		return;
		}
	CR_wfld.len = value;

}

static int cr_get_num_field(void)
{
	char pos[6],len[3],begval[11],endval[11],incr[11],rpt[4];
	int rc;
	long pf;

/* init the fields */
	strcpy(pos,"     ");
	strcpy(len,"  ");
	strcpy(begval,"          ");
	strcpy(endval,"          ");
	strcpy(incr,"1         ");
	strcpy(rpt,"NO ");
/* enter_the_fields */
	while(1)
		{
		WL_wpload();
		GP_pfkeys = GP_PF_01;
		WL_wswap(&GP_pfkeys);
		GPSETUP();
		GPSTD("ASCII   ","CREATE");
		cr_blk_and_fld(10,2);
		GPCTEXT("Please Supply the ASCII Number field and press (ENTER).",12,2);
		GPCTEXT("Starting value",14,2);
		GPKW("START   ",begval,10,14,45,"N");
		GPCTEXT("Ending value:",15,2);
		GPKW("END     ",endval,10,15,45,"N");
		GPCTEXT("Increment:",16,2);
		GPKW("INCRMENT",incr,10,16,45,"N");
		GPCTEXT("Starting position in the record:",18,2);
		GPKW("POSITION",pos,5,18,45,"N");
		GPCTEXT("Length of numeric field:",19,2);
		GPKW("LENGTH  ",len,2,19,45,"N");
		GPCTEXT("Indefinite repeat:",20,2);
		GPKW("REPEAT  ",rpt,3,20,45,"U");
		GPCTEXT("(YES/NO)",20,70);
		GPCTEXT("Or Select:",22,2);
		GPCTEXT("(1) Return to field selection menu:",23,2);
		GPCTEXT(msg_fld,24,2);
		GPPFS(&GP_pfkeys);
		pf = GP_display_and_read();
		strcpy(msg_fld,"");
		rc = pf;
		if(rc == 1)
			return(rc);
		if(val_cr_get_num(begval,endval,incr,pos,len,rpt))
			{
			break;
			}
		}
	CR_wfld.type = CFIELD_SEQUENCE;
	save_cr_fld();
	return(rc);
}

static int val_cr_get_num(char *begval, char *endval, char *incr, char *pos, char *len, char *rpt)
{
	val_cr_num_beg(begval);
	if(msg_fld[0] < '!')
		val_cr_num_end(endval);
	if(msg_fld[0] < '!')
		val_cr_num_incr(incr);
	if(msg_fld[0] < '!')
		val_cr_pos(pos);
	if(msg_fld[0] < '!')
		val_cr_len(len);
	if(msg_fld[0] < '!')
		val_cr_num_rpt(rpt);
	if(msg_fld[0] < '!')
		return(1);
	else
		return(0);
}

static void val_cr_num_beg(char *begval)
{
	long value;

	value = atol(begval);
	CR_wfld.begval = value;
	CR_wfld.curval = -1;
}

static void val_cr_num_end(char *endval)
{
	long value;

	value = atol(endval);
	if(value < CR_wfld.begval)
		{
		strcpy(msg_fld, "Start value must be less than end value.");
		return;
		}
	CR_wfld.endval = value;
}

static void val_cr_num_incr(char *incr)
{
	long value;

	value = atol(incr);
	if(value < 1)
		{
		strcpy(msg_fld,"Increment must be at least 1.");
		return;
		}
	CR_wfld.increment = value;
}

static void val_cr_num_rpt(char *rpt)
{
	if( (strcmp("YES",rpt)) && (strcmp("NO ",rpt)) )
		{
		strcpy(msg_fld,"Repeat must be YES or NO.");
		return;
		}
	CR_wfld.repeat = 0;
	if(!(strcmp("YES",rpt)))
		CR_wfld.repeat = 1;
}

static int cr_must_count(void)
{
	while(1)
		{
		if(blk_is_counted())
			return(0);
		if(cr_get_count() == 1)
			return(1);
		}
}

/*----
Two types of field will force a block to have a fixed
count. A file field, or a sequence field that is non-rpeating
------*/
static int forces_count(CR_FLD *fld)
{
	if(fld->type == CFIELD_FILE)
		return(1);
	if(fld->type == CFIELD_SEQUENCE)
		{
		if(!fld->repeat)
			return(1);
		}
	return(0);
}

static int blk_is_counted(void)
{
	CR_BLK *blk;

	blk = ll_last((LLTYPE*) cr_blk);

	if(blk->count > 0)
		return(1);

	if(ll_select((LLTYPE*)blk->fld,forces_count))
		return(1);
	return(0);
}


static int cr_get_count(void)
{
	char count[8], block[4];
	int rc;
	long pf;
	CR_BLK *blk;

/* init the fields */
	ClearStr(count);
	ClearStr(block);
	save_cr_blk();
	sprintf(block,"%03d",blk_no + 1);
	blk = ll_last((LLTYPE*) cr_blk);
	if(blk->count > 0)
		sprintf(count,"%07ld",blk->count);
/* enter_the_fields */
	while(1)
		{
		WL_wpload();
		GP_pfkeys = GP_PF_01;
		WL_wswap(&GP_pfkeys);
		GPSETUP();
		GPSTD("COUNT   ","CREATE");
		GPCTEXT("Enter the maximum number of records to create and press (ENTER)",9,2);
		GPCTEXT("Number of records for block     :",11,2);
		GPCTEXT(block,11,31);
		GPKW("RECORDS ",count,7,11,50,"N");
		GPCTEXT("(1) Return to field selection menu:",23,2);
		GPCTEXT(msg_fld,24,2);
		GPPFS(&GP_pfkeys);
		pf = GP_display_and_read();
		strcpy(msg_fld,"");
		rc = pf;
		if(rc == 1)
			return(rc);
		if(val_cr_get_count(count))
			{
			break;
			}
		}
	return(rc);

}

static int val_cr_get_count(char *count)
{
	CR_BLK *blk;
	long value;

	blk = ll_last((LLTYPE*)cr_blk);
	value = atol(count);

	if(value < 0)
		{
		strcpy(msg_fld,"RECORDS must be a positive number");
		return(0);
		}

	if(value == 0)
		blk->count = -1;
	else
		blk->count = value;

	return 0;
}


/*----
Routines for manipulating blocks and fields
------*/

static void cr_add_blk(CR_BLK *blk)
{
	if(cr_blk == NULL)
		cr_blk = blk;
	else
		ll_append((LLTYPE*)cr_blk,(LLTYPE*)blk);
}

static void cr_add_fld(CR_BLK *blk, CR_FLD *fld)
{
	if(blk->fld == NULL)
		blk->fld = fld;
	else
		ll_append((LLTYPE*)blk->fld,(LLTYPE*)fld);
}

/*
cr_del_blk(blk)
CR_BLK *blk;
{
	cr_blk = ll_unlink(blk);
}
cr_del_fld(blk,fld)
CR_BLK *blk;
CR_FLD *fld;
{
	blk->fld = ll_unlink(fld);
}
*/

static void cr_free_a_fld(CR_FLD *fld)
{
	if(fld->kfb)
	{
		if(fld->kfb->_record)
		{
			CR_MEM_FREE(fld->kfb->_record, "KCSIO_BLOCK->_record");
		}

		if (fld->kfb != &CR_wkfb) /* HACK - temp variable */
		{
			/*
			**	CR_wkfb this seems to be a global temp variable
			**	that init_cr_get_field() sets kfb to point to.
			*/
			CR_MEM_FREE(fld->kfb, "KCSIO_BLOCK");
		}
	}

	if (fld != &CR_wfld)  /* HACK - temp variable */
	{
		CR_MEM_FREE(fld, "CR_FLD");
	}
}

static void cr_free_a_blk(CR_BLK *blk)
{
	ll_all(blk->fld,cr_free_a_fld);
	CR_MEM_FREE(blk, "CR_BLK");
}

void cr_free_blks(void)
{
	ll_all(cr_blk,cr_free_a_blk);
	cr_blk = NULL;
}

static CR_BLK *cr_new_blk(void)
{
	CR_BLK *blk;

	blk = CR_MEM_ALLOC_TYPE(CR_BLK);
	if(blk == NULL)
		{
		cr_no_mem_exit("BLOCK");
		}
	return(blk);
}

static CR_FLD *cr_new_fld(void)
{
	CR_FLD *fld;
	fld = CR_MEM_ALLOC_TYPE(CR_FLD);
	if(fld == NULL)
		cr_no_mem_exit("FIELD");
	return(fld);
}

static KCSIO_BLOCK *cr_new_kcsio(void)
{
	KCSIO_BLOCK *kfb;
	kfb = CR_MEM_ALLOC_TYPE(KCSIO_BLOCK);
	if(kfb == NULL)
		cr_no_mem_exit("INPUT");
	kfb->_record = CR_MEM_ALLOC_STR(2049, "KCSIO_BLOCK->_record");
	if(kfb->_record == NULL)
		cr_no_mem_exit("RECORD");
	return(kfb);
}


static void cr_no_mem_exit(char *type)
{
	long pf;

	WL_wpload();
	GP_pfkeys=0;
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	GPSTD("MEMORY  ","CREATE");
	GPCTEXT("Unable to acquire memory for a new",10,26);
	GPCTEXT(type,12,32);
	GPCTEXT("definition.",12,38);
	GPCTEXT("Press (ENTER) to exit the program.",23,2);
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	kcsi_exit(0);
}

/*
**	History:
**	$Log: vscrblk.c,v $
**	Revision 1.17  2003/02/20 19:29:54  gsl
**	fix -Wall warnings
**	
**	Revision 1.16  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.15  2002/11/14 13:57:37  gsl
**	Fix memory problem, attempting to free() non-alloc-ed memory
**	Remove atexit() processing - not needed
**	
**	Revision 1.14  2002/10/24 15:48:30  gsl
**	Make globals unique
**	
**	Revision 1.13  2002/10/24 14:20:32  gsl
**	Make globals unique
**	
**	Revision 1.12  2002/10/23 21:07:25  gsl
**	make global name unique
**	
**	Revision 1.11  2002/10/21 18:29:20  gsl
**	cleanup
**	
**	Revision 1.10  2002/10/17 21:22:44  gsl
**	cleanup
**	
**	Revision 1.9  2002/07/25 15:20:23  gsl
**	Globals
**	
**	Revision 1.8  2002/07/12 17:17:02  gsl
**	Global unique WL_ changes
**	
**	Revision 1.7  2002/07/10 21:06:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.6  1999/09/13 19:54:49  gsl
**	fix missing return code
**	
**	Revision 1.5  1996-10-02 20:11:15-04  gsl
**	Fixed pfkey tags for W4W support
**
**	Revision 1.4  1996-10-02 15:12:39-07  gsl
**	fix calls to ll_all()
**
**	Revision 1.3  1996-10-02 09:07:37-07  gsl
**	Add standard headers
**	Fix prototypes
**	Fix compiler warnings
**
**
**
*/
