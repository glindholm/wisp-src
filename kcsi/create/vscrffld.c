static char copyright[]="Copyright (c) 1988-1997 NeoMedia Technologies, Inc., All rights reserved.";
static char rcsid[]="$Id:$";

#include <stdio.h>
#include <ctype.h>
#include "vscrglb.h"
#include "shrthand.h"
#include "kcsifunc.h"

/*----
Routines for indentifying the fields within a file
to be extracted for a creation.
------*/

/*----
The fields for the screen.
------*/

static char start_key[65], end_key[65], increment[8];
static char cinpos[5], clength[3], compare[3], cstring[65];
static char inpos[5], outpos[5], length[5], count[5];
static int keyflen, keylen, org, reclen, screen_error;
static char reclen_s[5], keylen_s[5], keypos_s[5];
static KCSIO_BLOCK *kfb;
static char msg_fld[81];
static int hexmode;

static void fields_init(void);
static void init_key_field(void);
static void init_cstring(void);
static int fields_entry(void);
static int fields_screen(void);
static void val_fields(void);
static void val_inpos(void);
static void val_outpos(void);
static void val_length(void);
static void val_count(void);
static void val_start_key(void);
static void val_index_start_key(void);
static void val_rel_start_key(void);
static void val_end_key(void);
static void val_index_end_key(void);
static void val_rel_end_key(void);
static void val_increment(void);
static void val_cinpos(void);
static void val_clength(void);
static void val_compare(void);
static void val_cstring(void);
static int non_blanklen(char *str);
static int must_be_positive_error(int value, char *str);
static int negative_error(int value, char *str);
static int zero_error(int value, char *str);

int cr_get_file_field(void)
{
	int rc;

	fields_init();
	rc = fields_entry();
	return rc;
}

static void fields_init(void)
{
	hexmode = 0;

	ClearStr(cinpos);
	ClearStr(clength);
	ClearStr(compare);
	init_cstring();

	strcpy(inpos,"1   ");
	strcpy(outpos,"1   ");
	strcpy(count,"1   ");
	strcpy(increment,"1      ");
	kfb = &wkfb;
	org = kfb->_org[0];
	reclen = kfb->_record_len;
	sprintf(length,"%-4d",reclen);
	sprintf(reclen_s,"%-4d",reclen);
	init_key_field();
}

static void init_key_field(void)
{
	int idx;

	keyflen = keylen = 7;
	if( org != 'I' )
	{
		strcpy(start_key,"FIRST  ");
		strcpy(end_key,"LAST   ");
		return;
	}
									/* Init for a keyed file */
	for(idx = 0, keylen = 0; idx < kfb->_key[0].k_nparts; ++idx)
	{
		keylen += kfb->_key[0].k_part[idx].kp_leng;
	}
	ClearStr( start_key );
	if( keylen > 64 )
	{
		keylen = 64;
	}
	wfld.keylen = keylen;
	keyflen = keylen;
	memset(end_key, 'z', keyflen);

									/* If we are in hex mode then */
	if( !hexmode )
	{
		return;
	}
	keyflen *= 2;
	memset(start_key,'0',keyflen);
	memset(end_key,'F',keyflen);
}

static void init_cstring(void)
{
	ClearStr(cstring);
}

static int fields_entry(void)
{
	int rc;

	while( 1 )
	{
		screen_error = 0;
		rc = fields_screen();
		strcpy(msg_fld,"");
		if(rc == 1)
		{
			break;
		}

		if(rc == 2)
		{
			hexmode = !hexmode;
			init_key_field();
			init_cstring();
			continue;
		}
		if(rc == 13)
		{
			cr_file_field_help();
			continue;
		}
		if(rc == 0)
		{
			val_fields();
			if(screen_error)
				continue;
			else
			{
				save_cr_in_file();
				break;
			}
		}
	}
	return(rc);
}

static int fields_screen(void)
{
	int rc;
	long pf;

	wpload();
	gppfkeys = GP_PF_01|GP_PF_02|GP_PF_13;
	wswap(&gppfkeys);
	GPSETUP();
	GPSTD("FILE    ","CREATE");
	GPCTEXT("Input file          in          on        is",9,2);
	GPCTEXT(wkfb._name,9,13);
	GPCTEXT(wkfb._library,9,25);
	GPCTEXT(wkfb._volume,9,37);
	switch( org )
	{
		case 'I':
			GPCTEXT("indexed.",9,47);
			break;
		case 'R':
			GPCTEXT("relative.",9,47);
			break;
		case 'C':
			GPCTEXT("consecutive.",9,47);
			break;
	}
	GPCTEXT("Record Length=",9, 60);
	GPCTEXT(reclen_s,9,74);
	if( org == 'I' )
	{
		GPCTEXT("Key starts at      and length is     .",10,2);
		sprintf(keypos_s,"%-4d",wkfb._key[0].k_part[0].kp_start + 1);
		GPCTEXT(keypos_s,10,16);
		sprintf(keylen_s,"%-4d",keylen);
		GPCTEXT(keylen_s,10,35);
		if(kfb->_key[0].k_nparts > 1)
		{
			GPCTEXT("Key is split.",10,41);
		}
	}
	GPCTEXT("Starting positions and field lengths:",12,2);
	GPKW("INPOS   ",inpos,4,13,3,"N");
	GPKW("OUTPOS  ",outpos,4,13,20,"N");
	GPKW("LENGTH  ",length,4,13,40,"N");
	GPKW("COUNT   ",count,4,13,60,"N");
	GPCTEXT("Range of records to be used:",14,2);
	GPKW("START   ",start_key,keyflen,15,3,"C");
	GPKW("END     ",end_key,keyflen,16,3,"C");
	if( org != 'I' )
	{
		GPKW("INCRMENT",increment,7,16,25,"N");
	}
	GPCTEXT("Compare field specifications:",17,2);
	GPKW("CINPOS  ",cinpos,4,18,3,"N");
	if( !hexmode )
	{
		GPKW("CLENGTH ",clength,2,18,20,"N");
	}
	GPKW("COMPARE ",compare,2,18,40,"U");
	GPKW("CSTRING ",cstring,64,19,3,"C");
	GPCTEXT("(1) Return to the field selection menu",22,2);
	if( hexmode )
	{
		GPCTEXT("(2) Switch to ASCII",23,2);
	}
	else
	{
		GPCTEXT("(2) Switch to HEX",23,2);
	}
	GPCTEXT("(13) Help",23,40);
	GPCTEXT(msg_fld,24,2);

	GPPFS(&gppfkeys);
	pf = display_and_read_gp();
	rc = pf;
	return(rc);
}

/*----
Validation routines
------*/

static void val_fields(void)
{
	if(!screen_error) val_inpos();
	if(!screen_error) val_outpos();
	if(!screen_error) val_length();
	if(!screen_error) val_count();
	if(!screen_error) val_start_key();
	if(!screen_error) val_end_key();
	if(!screen_error) val_increment();
	if(!screen_error) val_cinpos();
	if(!screen_error) val_clength();
	if(!screen_error) val_compare();
	if(!screen_error) val_cstring();
}

static void val_inpos(void)
{
	int value;

	wfld.type = CFIELD_FILE;

	value = atoi(inpos);
	if(must_be_positive_error(value,"INPOS"))
		return;

	if(value > wkfb._record_len)
	{
		strcpy(msg_fld,"INPOS is outside the input record.");
		screen_error = 1;
		return;
	}
	wfld.inpos = value - 1;
}

static void val_outpos(void)
{
	int value;

	value = atoi(outpos);
	if(must_be_positive_error(value,"OUTPOS"))
		return;

	if(value > cr_out.ofile._record_len)
	{
		strcpy(msg_fld,"OUTPOS is outside the output record.");
		screen_error = 1;
		return;
	}
	wfld.outpos = value - 1;
}

static void val_length(void)
{
	int value;

	value = atoi(length);
	if(must_be_positive_error(value,"LENGTH"))
		return;

	if( (value + wfld.inpos) > wkfb._record_len)
	{
		strcpy(msg_fld,"The field is not within the input record.");
		screen_error = 1;
		return;
	}

	if( (value + wfld.outpos) > cr_out.ofile._record_len)
	{
		strcpy(msg_fld,"The field is not within the output record.");
		screen_error = 1;
		return;
	}

	wfld.len = value;
}

static void val_count(void)
{
	int value;

	value = atoi(count);
	if(must_be_positive_error(value,"COUNT"))
		return;

	wfld.count = value;
}

static void val_start_key(void)
{
	if( org == 'I' )
	{
		val_index_start_key();
	}
	else
	{
		val_rel_start_key();
	}
}

static void val_index_start_key(void)
{
	int idx,value;

	if( !hexmode )
	{
		memcpy(wfld.string,start_key,65);
		return;
	}

	for(idx = 0; idx < keyflen; ++idx)
	{
		if(!isxdigit(start_key[idx]))
		{
			strcpy(msg_fld,"START must be hex value");
			screen_error = 1;
			return;
		}
	}

	for(idx = 0; idx < keyflen; idx += 2)
	{
		value = 0;
		sscanf(&start_key[idx],"%2x",&value);
		wfld.string[idx/2] = value;
	}
}

static void val_rel_start_key(void)
{
	long value;

	if(!memcmp(start_key,"FIRST  ",7))
	{
		wfld.begval = 0;
		return;
	}

	value = atol(start_key);
	if(value < 1)
	{
		strcpy(msg_fld,"START record must be greater than zero.");
		screen_error = 1;
		return;
	}
	wfld.begval = value;
	wfld.curval = 0;
}

static void val_end_key(void)
{
	if( org == 'I' )
	{
		val_index_end_key();
	}
	else
	{
		val_rel_end_key();
	}
}

static void val_index_end_key(void)
{
	int idx,value;

	if( !hexmode )
	{
		memcpy(wfld.endrange,end_key,65);
		return;
	}

	for(idx = 0; idx < keyflen; ++idx)
	{
		if(!isxdigit(end_key[idx]))
		{
			strcpy(msg_fld,"END must be hex value");
			screen_error = 1;
			return;
		}
	}

	for(idx = 0; idx < keyflen; idx += 2)
	{
		value = 0;
		sscanf(&end_key[idx],"%2x",&value);
		wfld.endrange[idx/2] = value;
	}
}

static void val_rel_end_key(void)
{
	long value;

	if(!memcmp(end_key,"LAST   ",7))
	{
		wfld.endval = 0;
		return;
	}
	value = atol(end_key);
	if(value < 1)
	{
		strcpy(msg_fld,"END record must be greater than zero.");
		screen_error = 1;
		return;
	}
	wfld.endval = value;
}

static void val_increment(void)
{
	long value;

	if( org == 'I' )
	{
		return;
	}

	value = atol(increment);

	if( value < 1 )
	{
		strcpy(msg_fld,"INCRMENT must be greater than zero.");
		screen_error = 1;	
		return;
	}
	wfld.increment = value;
}

static void val_cinpos(void)
{
	int value;

	value = atoi(cinpos);
	if(negative_error(value,"CINPOS"))
		return;
	--value;

	wfld.cinpos = value;
}

static void val_clength(void)
{
	int value;

	if( hexmode )
	{
		wfld.clen = 0;
		return;
	}
	if(wfld.cinpos == -1)
		return;
	value = atoi(clength);
	if(negative_error(value,"CLENGTH"))
		return;
	wfld.clen = value;
}

static void val_compare(void)
{
	if(wfld.cinpos == -1)
		return;
	strcpy(wfld.compare,compare);
	if(Streq(compare,"EQ"))
		return;
	if(Streq(compare,"NE"))
		return;
	if(Streq(compare,"GT"))
		return;
	if(Streq(compare,"GE"))
		return;
	if(Streq(compare,"LT"))
		return;
	if(Streq(compare,"LE"))
		return;
	strcpy(msg_fld,"COMPARE must be EQ,NE,GT,GE,LT or LE.");
	screen_error = 1;
}

static void val_cstring(void)
{
	int value,idx;

	if( !hexmode )
	{
		strcpy(wfld.cstring,cstring);
		if(wfld.clen == 0)
		{
			wfld.clen = non_blanklen(cstring);
		}
		return;
	}
	wfld.clen = non_blanklen(cstring);
	for(idx = 0; idx < wfld.clen; ++idx)
	{
		if(!isxdigit(cstring[idx]))
		{
			strcpy(msg_fld,"CSTRING must be hex.");
			screen_error = 1;
			return;
		}
	}

	for(idx = 0; idx < wfld.clen; idx += 2)
	{
		value =0;
		sscanf(&cstring[idx],"%2x",&value);
		wfld.cstring[idx/2] = value;
	}
	wfld.clen = idx/2;
}

static int non_blanklen(char *str)
{
	int len;

	len = strlen(str);

	if(len > 0)
		--len;
	while( (str[len] == ' ') && (len > -1))
	{
		--len;
	}
	++len;
	return(len);
}

static int must_be_positive_error(int value, char *str)
{
	if(negative_error(value,str))
		return(1);
	if(zero_error(value,str))
		return(1);
	return(0);
}

static int negative_error(int value, char *str)
{
	if(value < 0)
	{
		sprintf(msg_fld,"%s may not be negative.",str);
		screen_error = 1;
		return(1);
	}
	return(0);
}

static int zero_error(int value, char *str)
{
	if(value == 0)
	{
		sprintf(msg_fld,"%s may not be zero.",str);
		screen_error = 1;
		return(1);
	}
	return(0);
}

/*
**	History:
**	$Log: vscrffld.c,v $
**	Revision 1.4  1997-08-13 10:36:45-04  scass
**	Fixed some formatting of code.
**
**	Revision 1.3  1996-10-02 12:08:48-04  gsl
**	Add standard headers
**	Fix prototypes and warnings
**
*/
