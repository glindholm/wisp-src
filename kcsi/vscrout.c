/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


/*----
  	vscrout.c

-----*/

/*
**	Includes
*/
#include <stdio.h>

#include "create.h"
#include "kcsifunc.h"
#include "vscrglb.h"
#include "gp.h"
#include "mffcd.h"

/*
**	Structures and Defines
*/
#define	not_screen_error	(screen_error == 0)

/*----
Are we allowing split keys and split key overlaps.
------*/

#define	UPPER_TYPE	"U"
#define	NUMERIC_TYPE	"N"
#define CHARTYPE	0

typedef struct {
	char pos[6];
	char len[4];
	char dup[2];
	}AKEY;

/*
**	Globals and Externals
*/
static AKEY	akey[WANG_MAX_KEYS];
static AKEY	split[NPARTS];
static AKEY	blank_akey = { "     ", "   ", " "};

/*
**	Static data
*/
/* CHANGE-COPYRIGHT-DATE */
static char app[]="CREATE Utility - Version %s.%s (c) KCSI/Shell Stream Software LLC";
static char logo[81], message_field[81], m1_field[31], m2_field[31];
static char recsize[5], errlist[4], output_org[2], output_format[2];
static int screen_error;
static char keypos[6], keylen[4];
static char poslit[16][10], lenlit[16][10], duplit[16][10];
static char *keypos_type, *keylen_type;

/*
**	Function Prototypes
*/
static int get_file_name_and_type(char *vers, char *platform);
static int get_keys(void);
static int open_new_file(void);
static void name_and_type_init(char *vers, char *platform);
static int name_and_type_entry(void);
static int name_and_type_screen(void);
static void val_name_and_type(void);
static void val_name(void);
static void val_type(void);
static void val_recsize(void);
static void val_format(void);
static void get_keys_init(void);
static int get_primary_alt_keys(void);
static int get_primary_key(void);
static int get_alt_keys(void);
static void get_primary_key_init(void);
static int get_primary_key_entry(void);
static int primary_key_screen(void);
static int is_a_split(char *pos, char *len);
static int get_primary_split(void);
static void val_primary_key(void);
static void val_keypos(void);
static void val_keylen(void);
static void val_errlist(void);
static void val_pos_in_record(int pos);
static void val_len_in_record(int len, int pos);
static int get_split_entry(int key, char *dups);
static void get_split_init(void);
static int split_key_screen(int key, char *dups);
static void val_split(int key, char *dups);
static void squeeze_split(void);
static void val_overlap(int pos, int len);
static void init_key_part(struct keydesc *key, int part, int pos, int len, char *dups);
static void get_alt_keys_init(void);
static int get_alt_keys_entry(void);
static int alt_keys_screen(void);
static int val_alt_keys(void);
static void squeeze_alt_keys(void);
static int val_all_akey_fields(int idx);
static void val_alt_dup(int idx);
static int get_alt_key_split(int idx);
static void val_alt_key(int key);
static int k_isblank(char *str, int len);
static void akey_pos_len_error(int idx);
static void val_akeypos(int idx);
static void val_akeylen(int idx);

/*
**      Routine:        cr_get_output_spec()
**
**      Function:
**
**      Description:
**
**      Arguments:
**
**      Globals:        None
**
**      Return:		None
**
**      Warnings:       None
**
**      History:
**      08/07/97        Written by MoB
**
*/
int cr_get_output_spec(char *vers, char *platform)
{
	int rc;

	if(cr_split_keys)
		{
		keypos_type = UPPER_TYPE;
		keylen_type = UPPER_TYPE;
		}
	else
		{
		keypos_type = NUMERIC_TYPE;
		keylen_type = NUMERIC_TYPE;
		}

	rc = get_file_name_and_type(vers, platform);
	if(rc == 16)
		return rc;
	rc = get_keys();
	if(rc != 0)
		return rc;
	rc = open_new_file();
	return rc;
}
	
static int open_new_file(void)
{
	cr_io(OPEN_OUTPUT);
	cr_io(CLOSE_FILE);
	return 0;
}

/*----
			NAME AND TYPE SCREEN
------*/


static int get_file_name_and_type(char *vers, char *platform)
{
	int rc;
	name_and_type_init(vers,platform);
	rc = name_and_type_entry();
	return(rc);
}


static void name_and_type_init(char *vers, char *platform)
{

	memcpy(cr_out.ofile._library,create_outlib,8);
	memcpy(cr_out.ofile._volume,create_outvol,8);
	cr_out.ofile._org[0] = ' ';
#ifdef KCSI_MFX
	cr_out.ofile._format = MF_FILE_DEFAULT_FORMAT;
#endif
#ifdef KCSI_ACU
	cr_out.ofile._format = '0';
#endif
	strcpy(recsize,"    ");
	strcpy(output_org," ");
	strcpy(output_format," ");

	sprintf(logo,app,vers,platform);
}

static int name_and_type_entry(void)
{
	int rc;

	while(1)
		{
		screen_error = 0;
		rc = name_and_type_screen();
		if(rc == 16)
			break;
		if(rc == 13)
			{
			cr_name_and_type_help();
			continue;
			}
		val_name_and_type();
		if(!screen_error)
			break;
		}

	return(rc);
}

static int name_and_type_screen(void)
{

	int rc;
	long pf;

	WL_wpload();
	GP_pfkeys = GP_PF_13|GP_PF_16;
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	GPSTD("OUTPUT  ","CREATE");
	GPCTEXT(logo,10,15);
	GPCTEXT("Please specify output parameters and press (ENTER).",12,2);
	GPCTEXT("Output:",14,2);
	GPFLVUC(cr_out.ofile._name,
		cr_out.ofile._library,
		cr_out.ofile._volume,14,15);
	GPCTEXT("Type:",15,2);
	GPCTEXT("(I)ndexed, (A)lt Indexed, (R)el, (C)onsec.,",15,35);
	GPCTEXT("(B)inary Sequential",16,35);
	GPKW("TYPE    ",output_org,1,15,15,"U");
	GPCTEXT("Record size:",17,2);
	GPCTEXT("Record Length",17,35);
	GPKW("RECSIZE ",recsize,4,17,15,"N");
#ifdef KCSI_MFX
	GPCTEXT("Format:",18,2);
	GPCTEXT("0 = Default, 1 = C-ISAM, 2 = LEVEL II COBOL,",18,35);
	GPCTEXT("3 = Format used by system, 4 = IDXFORMAT\"4\"",19,35);
	GPKW("FORMAT  ",output_format,1,18,15,"N");
#endif
#ifdef KCSI_ACU_OLD
	GPCTEXT("Format:",18,2);
	GPCTEXT("Vision Version '2', '3', or '4',",18,35);
	GPCTEXT("Default = '0'",19,35);
	GPKW("FORMAT  ",output_format,1,18,15,"N");
#endif
	GPCTEXT("Or Select:",22,2);
	GPCTEXT("(13) Help  (16) Exit ",23,45);
	GPCTEXT(message_field,24,2);
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	strcpy(message_field,"");
	rc = pf;
	return(rc);
}

static void val_name_and_type(void)
{
	if(! screen_error) val_name();
	if(! screen_error) val_type();
	if(! screen_error) val_recsize();
	if(! screen_error) val_format();
}


static void val_recsize(void)
{
	int rl;

	rl = cr_out.ofile._record_len = atoi(recsize);
	if( (rl < 1) || (rl > 2040) )
		{
		strcpy(message_field,"RECSIZE must be between 1 and 2040");
		screen_error = 1;
		}
}

static void val_type(void)
{
	int typ;

	typ = output_org[0];

	if( (typ != 'I') &&
	    (typ != 'R') &&
	    (typ != 'C') &&
	    (typ != 'B') &&
	    (typ != 'A') )
		{
		strcpy(message_field,
		"TYPE must be Indexed, Alt Indexed, Rel. Consec. or Binary");
		screen_error = 1;
		return;
		}
	cr_output_org = typ;
	if(typ == 'A')
		typ = 'I';
	cr_out.ofile._org[0] = typ;
}

static void val_name(void)
{
	if(! KCSI_valspec_filled(cr_out.ofile._name,
			   cr_out.ofile._library,
			   cr_out.ofile._volume))
		{
		strcpy(message_field,
		"Output File Specification must be filled in correctly.");
		screen_error = 1;
		return;
		}
}

static void val_format(void)
{
	char format;

	format = output_format[0];

#ifdef KCSI_MFX
	if ( format == '1' )	cr_out.ofile._format = MF_CISAM_FORMAT;
	else if ( format == '2' )	cr_out.ofile._format = MF_LEVEL2_FORMAT;
	else if ( format == '3' )	cr_out.ofile._format = MF_NATIVE_FORMAT;
	else if ( format == '4' )	cr_out.ofile._format = MF_IDX4_FORMAT;
	else if ( format == '0' || format == ' ' )
	{
		cr_out.ofile._format = MF_FILE_DEFAULT_FORMAT;
	}
	else
	{
		strcpy(message_field,
		"FORMAT must be ( 0 - 4 )");
		screen_error = 1;
		return;
	}
#endif
#ifdef KCSI_ACU_OLD
	if ( format == '2' )		cr_out.ofile._format = '2';
	else if ( format == '3' )	cr_out.ofile._format = '3';
	else if ( format == '4' )	cr_out.ofile._format = '4';
	else if ( format == '0' || format == ' ' )
	{
		cr_out.ofile._format = '0';
	}
	else
	{
		strcpy(message_field,
		"FORMAT must be ( 0,2,3,4 )");
		screen_error = 1;
		return;
	}
#endif
	
}


/*----
			BOTH KEY SCREENS
------*/

static int get_keys(void)
{
	int rc;

	get_keys_init();

	rc = 0;
	if(cr_out.ofile._org[0] == 'I')
		rc = get_primary_alt_keys();
	return(rc);
}

static int get_primary_alt_keys(void)
{
	int rc;
	rc = get_primary_key();
	if(rc != 1)
		{
		if(cr_output_org == 'A')
			rc = get_alt_keys();
		}
	return(rc);
}

static void get_keys_init(void)
{
	memset(&cr_out.ofile._key[0],0,sizeof(struct keydesc) * WANG_MAX_KEYS);
}

/*----
			PRIMARY KEY SCREEN
------*/

static int get_primary_key(void)
{
	get_primary_key_init();
	return(get_primary_key_entry());
}

static void get_primary_key_init(void)
{
	strcpy(keypos,"     ");
	strcpy(keylen,"   ");
	strcpy(errlist,"NO ");
	strcpy(message_field,"");
}


static int get_primary_key_entry(void)
{
	int rc;

	while(1)
		{
		screen_error = 0;
		rc = primary_key_screen();
		if(rc == 1)
			break;
		if(rc == 13)
			{
			cr_primary_key_help();
			continue;
			}
		if(is_a_split(keypos,keylen))
			{
			rc = get_primary_split();	
			if(rc == 1)
				break;
			}
		else
			val_primary_key();
		if(! screen_error)
			break;
		}

	return(rc);
}

static int is_a_split(char *pos, char *len)
{
	if(strcmp(pos,"SPLIT"))
		return(0);
	if(strcmp(len,"KEY"))
		return(0);
	return(1);
}


static int primary_key_screen(void)
{
	
	int rc;
	long pf;

	WL_wpload();
	GP_pfkeys = GP_PF_01|GP_PF_13;
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	GPSTD("INDEXOPT","CREATE");
	GPCTEXT(logo,10,15);
	GPCTEXT("Please specify the indexed file parameters and press (ENTER).",
			12,2);
	GPCTEXT("Primary key location:",14,2);
	GPKW("KEYPOS  ",keypos,5,14,29,keypos_type);
	GPCTEXT("Primary key length:",15,2);
	GPKW("KEYLEN  ",keylen,3,15,29,keylen_type);
	GPCTEXT("(1-255)",15,50);
	GPCTEXT("Print key errors:",16,2);
	GPKW("ERRLIST ",errlist,3,16,29,"U");
	GPCTEXT("(YES / NO)",16,50);
	GPCTEXT("Press (1) to respecify output parameters", 22, 2);
	GPCTEXT("(13) Help",22,45);
	GPCTEXT(message_field,24,2);
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	strcpy(message_field,"");
	rc = pf;
	return(rc);
}

static void val_primary_key(void)
{
	if(! screen_error)  val_keypos();
	if(! screen_error)  val_keylen();
	if(! screen_error)  val_errlist();
}

static void val_keypos(void)
{
	int kp;

	kp = atoi(keypos);
	if(kp < 1)
		{
		strcpy(message_field,"KEYPOS must be entered.");
		screen_error = 1;
		return;
		}
	val_pos_in_record(kp);
	if(screen_error)
		return;
	cr_out.ofile._key[0].k_part[0].kp_start = kp - 1;
}

static void val_pos_in_record(int pos)
{
	if(pos > cr_out.ofile._record_len)
		{
		strcpy(message_field,"KEYPOS is beyond the end of the record.");
		screen_error = 1;
		return;
		}
}

static void val_keylen(void)
{
	int kl,kp;

	kl = atoi(keylen);
	kp = cr_out.ofile._key[0].k_part[0].kp_start;
	if(kl < 1)
		{
		strcpy(message_field,"KEYLEN must be entered.");
		screen_error = 1;
		return;
		}
	val_len_in_record(kl,kp);
	if(screen_error)
		return;
	cr_out.ofile._key[0].k_part[0].kp_leng = kl;
	cr_out.ofile._key[0].k_part[0].kp_type = CHARTYPE;
	cr_out.ofile._key[0].k_flags = ISNODUPS;
	cr_out.ofile._key[0].k_nparts = 1;
}

static void val_len_in_record(int len, int pos)
{

	if((len + pos) > cr_out.ofile._record_len)
		{
		strcpy(message_field,"KEYLEN is beyond the end of the record.");
		screen_error = 1;
		return;
		}
}

static void val_errlist(void)
{
	if(! strcmp(errlist,"YES"))
		{
		cr_errlist = 1;
		return;
		}
	if(! strcmp(errlist,"NO "))
		{
		cr_errlist = 0;
		return;
		}
	strcpy(message_field,"ERRLIST must be YES or NO.");
	screen_error = 1;
}

/*----
			PRIMARY SPLIT
------*/

static int get_primary_split(void)
{
	get_split_init();
	return(get_split_entry(0,"N"));
}

static void get_split_init(void)
{
	int idx;

	for(idx = 0; idx < 8; ++idx)
		{
		strcpy(split[idx].pos,"     ");
		strcpy(split[idx].len,"   ");
		}
}

static int get_split_entry(int key, char *dups)
{
	int rc;

	while(1)
		{
		screen_error = 0;
		rc = split_key_screen(key,dups);
		if(rc == 1)
			break;
		if(rc == 13)
			{
			cr_split_help();
			continue;
			}
		else
			val_split(key,dups);
		if(! screen_error)
			break;
		}

	return(rc);
}

static int split_key_screen(int key, char *dups)
{
	
	int rc,idx;
	long pf;
	char gpname[10];
	char duptext[81];

	WL_wpload();
	GP_pfkeys = GP_PF_01|GP_PF_13;
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	if(key == 0)
		{
		sprintf(gpname,"PRIMARY ");
		duptext[0] = '\0';
		}
	else
		{
		sprintf(gpname,"ALTKEY%d ",key);
		if(*dups == 'Y')
			sprintf(duptext,"With Duplicates allowed");
		else
			sprintf(duptext,"Duplicates are NOT allowed");
		}
	GPSTD(gpname,"CREATE");
	GPCTEXT(
	"Please enter positions and lengths of the SPLIT KEY elements.",
	9,2);
	for(idx = 0; idx < 8; ++idx)
		{
		sprintf(poslit[idx],"KEYPOS%d ",idx + 1);
		GPKW(poslit[idx],split[idx].pos,5,idx + 11,2,"N");
		sprintf(lenlit[idx],"KEYLEN%d ",idx + 1);
		GPKW(lenlit[idx],split[idx].len,3,idx + 11,19,"N");
		}
	GPCTEXT("Press (1) to respecify output parameters  (13) for Help",22,2);
	GPCTEXT(message_field,24,2);
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	strcpy(message_field,"");
	rc = pf;
	return(rc);
}

static char key_map[2049];

static void val_split(int key, char *dups)
{
	int idx;
	int kp,kl;

	memset(key_map,0,2049);
	squeeze_split();
	for(idx = 0; idx < 8; ++idx)
		{
		kp = atoi(split[idx].pos);
		if(kp < 1)
			{
			kl = 0;
			kp = 0;
			strcpy(split[idx].pos,"     ");
			strcpy(split[idx].len,"   ");
			break;
			}
		else
			kp -= 1;
		val_pos_in_record(kp);
		if(screen_error)
			break;
		kl = atoi(split[idx].len);
		val_len_in_record(kl,kp);
		if(screen_error)
			break;
#ifndef SPLIT_KEYS_OVERLAP
		val_overlap(kp,kl);
#endif
		if(screen_error)
			break;
		if(kl)
			{
			init_key_part(&cr_out.ofile._key[key],
				idx,kp,kl,dups);
			}
		}
}

static void init_key_part(struct keydesc *key, int part, int pos, int len, char *dups)
{
	++key->k_nparts;
	key->k_flags = (*dups == 'D')?ISDUPS:ISNODUPS;
	key->k_part[part].kp_start = pos;
	key->k_part[part].kp_leng = len;
	key->k_part[part].kp_type = CHARTYPE;
	
}


static void val_overlap(int pos, int len)
{
	int idx;

	for( idx = 0; idx < len; ++idx, ++pos)
		{
		if(key_map[pos] == 'X')
			{
			strcpy(message_field,"KEY parts may not overlap");
			screen_error = 1;
			break;
			}
		key_map[pos] = 'X';
		}
}



/*----
squeeze routine for AKEY arrays
------*/

static void squeeze(char *base, int len, int count, 
		    int (*squeezable)(void *,void *), 
		    int (*squeezer)(void *,void *))
{
	int idx,jidx;
	char *next;
	
	for(idx = 0; idx < (count - 1); ++idx, base += len)
	{
		for(
			jidx = idx + 1, next = base + len;
			jidx < count;
			++jidx , next += len)
		{
			if( (*squeezable)(base,next))
			{
				(*squeezer)(base,next);
				break;
			}
		}
	}
}

/*----
Semi generic squeezing routine, uses squeeze()
------*/

static int can_squeeze_akey(void *old, void *l_new)
{
	if(strcmp(((AKEY *)old)->pos,"     "))
		return(0);
	return(1);
}

static int  squeeze_akey(void *old, void *l_new)
{
	memcpy(old,l_new,sizeof(AKEY));
	memcpy(l_new,&blank_akey,sizeof(AKEY));
	return 0;
}

static void squeeze_split(void)
{
	squeeze((char*)split, sizeof(AKEY), 8,
		can_squeeze_akey, squeeze_akey);
}

static void squeeze_alt_keys(void)
{
	squeeze((char*)akey, sizeof(AKEY), 16,
		can_squeeze_akey, squeeze_akey);
}




/*-----
			ALT KEYS SCREEN
------*/

static int get_alt_keys(void)
{
	get_alt_keys_init();
	return(get_alt_keys_entry());
}

static void get_alt_keys_init(void)
{
	int idx;

	for(idx = 0; idx < 16; ++idx)
		{
		strcpy(akey[idx].pos,"     ");
		strcpy(akey[idx].len,"   ");
		strcpy(akey[idx].dup," ");
		}
}

static int get_alt_keys_entry(void)
{
	int rc;

	while(1)
		{
		screen_error = 0;
		rc = alt_keys_screen();
		if(rc == 1)
			break;
		if(rc == 13)
			{
			cr_alt_keys_help();
			continue;
			}
		rc = val_alt_keys();
		if(rc == 1)
			break;
		if(! screen_error)
			break;
		}
	return(rc);

}

/*----
This GETPARM exhausted the GP macro, and I had to remove all
cosmetics to make it work correctly, and provide some room in case
a field needs to be added.
You can add about 3 fields if needed before the space is exhausted
------*/

static int alt_keys_screen(void)
{
	
	int rc,idx;
	long pf;

	WL_wpload();
	GP_pfkeys = GP_PF_01|GP_PF_13;
	WL_wswap(&GP_pfkeys);
	GPSETUP();
	GPSTD("ALTKEYS ","CREATE");
	for(idx = 0; idx < 16; ++idx)
		{
		sprintf(poslit[idx],"KEYPOS%d ",idx + 1);
		GPKW(poslit[idx],akey[idx].pos,5,idx + 9,2,keypos_type);
		sprintf(lenlit[idx],"KEYLEN%d ",idx + 1);
		GPKW(lenlit[idx],akey[idx].len,3,idx + 9,19,keylen_type);
		sprintf(duplit[idx],"FLAGS%d  ",idx + 1);
		GPKW(duplit[idx],akey[idx].dup,1,idx + 9,34,"U");
		}
	GPCTEXT("   D = duplicates allowed",12,50);
	GPCTEXT("  Press (1) to respecify",20,50);
	GPCTEXT("     output parameters",21,50);
	GPCTEXT("  (13) Help",22,50);
	GPCTEXT(m1_field,23,50);
	GPCTEXT(m2_field,24,50);
	GPPFS(&GP_pfkeys);
	pf = GP_display_and_read();
	strcpy(message_field,"");
	rc = pf;
	return(rc);
}

static int val_alt_keys(void)
{
	int rc, idx;

	rc = 0;
	squeeze_alt_keys();
	for(idx = 0; idx < 16; ++idx)
		{
		if(val_all_akey_fields(idx) == 0)
			break;
		if(screen_error)
			break;
		val_alt_dup(idx);
		if(screen_error)
			break;
		if(is_a_split(akey[idx].pos,akey[idx].len))
			{
			if(!screen_error)
				{
				rc = get_alt_key_split(idx);	
				if(rc == 1)
					break;
				}
			}
		else
			val_alt_key(idx);
		}
	return(rc);
}

/*----
Return(0) If no fields are filled in.
1 if any are filled in.
Sets screen error if not all 3 are filled in.
So either 0 or 3 fields must be filled in.
------*/

static int val_all_akey_fields(int idx)
{
	int fcount;

	fcount = 0;

	if(!k_isblank(akey[idx].pos,5))
		++fcount;
	if(!k_isblank(akey[idx].len,3))
		++fcount;

/* if pos or len is blank, then an error */
	if(fcount == 1)
		{
		akey_pos_len_error(idx);
		return(1);
		}
/* if pos and len are both blank, dup field must be blank */
	if(fcount == 0)
		{
		if(!k_isblank(akey[idx].dup,1))
			{
			akey_pos_len_error(idx);
			return(1);
			}
		else
			return(0);
		}
	return(1);
}

static void akey_pos_len_error(int idx)
{
	screen_error = 1;
	sprintf(m1_field,
		"Position or Length of ALTKEY%d",idx + 1);
	sprintf(m2_field,
		"is missing.");
}



static int get_alt_key_split(int idx)
{
	get_split_init();
	return(get_split_entry(idx + 1,akey[idx].dup));
}

static void val_alt_key(int key)
{
	if(! screen_error)  val_akeypos(key);
	if(! screen_error)  val_akeylen(key);
}

static void val_akeypos(int idx)
{
	int kp;

	kp = atoi(akey[idx].pos);
	if(kp < 1)
		{
		sprintf(m1_field,"KEYPOS%d is invalid.",idx + 1);
		screen_error = 1;
		return;
		}
	val_pos_in_record(kp);
	if(screen_error)
		return;
	cr_out.ofile._key[idx + 1].k_part[0].kp_start = kp - 1;
}

static void val_akeylen(int idx)
{
	int kl,kp;

	kl = atoi(akey[idx].len);
	kp = cr_out.ofile._key[idx + 1].k_part[0].kp_start;
	if(kl < 1)
		{
		sprintf(m1_field,"KEYLEN%d is invalid.", idx + 1);
		screen_error = 1;
		return;
		}
	val_len_in_record(kl,kp);
	if(screen_error)
		return;
	cr_out.ofile._key[idx + 1].k_part[0].kp_leng = kl;
	cr_out.ofile._key[idx + 1].k_part[0].kp_type = CHARTYPE;
	cr_out.ofile._key[idx + 1].k_flags = 
		akey[idx].dup[0] == 'D'?ISDUPS:ISNODUPS;
	cr_out.ofile._key[idx + 1].k_nparts = 1;
	++cr_out.ofile._altkey_count;
}


static void val_alt_dup(int idx)
{
	if((akey[idx].dup[0] == 'D') || (akey[idx].dup[0] == ' '))
		return;
	screen_error = 1;
	sprintf(m1_field,"FLAGS%d must be 'D' or BLANK.",idx +1); 
}

static int k_isblank(char *str, int len)
{
	for( ; len ; --len, ++str)
		{
		if(*str != ' ')
			return(0);
		}
	return(1);
}

/*
**	History:
**	$Log: vscrout.c,v $
**	Revision 1.29  2010/01/16 02:04:28  gsl
**	new release
**	wisp 5.1.00
**	kcsi 4.2.00
**	
**	Revision 1.28  2010/01/10 00:58:33  gsl
**	fix LINUX warnings
**	trunc
**	isblank
**	sys_errlist
**	
**	Revision 1.27  2007/01/03 14:11:44  gsl
**	copyright 2007
**	
**	Revision 1.26  2005/01/03 19:12:06  gsl
**	Copyright year 2005
**	
**	Revision 1.25  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.24  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.23  2003/01/24 20:38:53  gsl
**	Change year to 2003
**	
**	Revision 1.22  2002/10/24 15:48:30  gsl
**	Make globals unique
**	
**	Revision 1.21  2002/10/24 14:20:30  gsl
**	Make globals unique
**	
**	Revision 1.20  2002/10/21 18:27:59  gsl
**	fix setting value of cr_errlist (was 'Y' or 'N')
**	
**	Revision 1.19  2002/10/21 15:27:04  gsl
**	cleanup
**	
**	Revision 1.18  2002/10/17 21:22:44  gsl
**	cleanup
**	
**	Revision 1.17  2002/10/17 17:56:19  gsl
**	Rename variables new to l_new
**	
**	Revision 1.16  2002/07/25 15:20:22  gsl
**	Globals
**	
**	Revision 1.15  2002/07/12 17:17:02  gsl
**	Global unique WL_ changes
**	
**	Revision 1.14  2002/07/10 21:06:27  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.13  2002/03/28 14:19:02  gsl
**	FIx PF1 -> (1) tag
**	
**	Revision 1.12  2002-03-27 16:08:45-05  gsl
**	(C) 2002
**
**	Revision 1.11  2001-09-06 11:41:13-04  gsl
**	Remove the FORMAT output field for Acucobol
**
**	Revision 1.10  1998-05-19 10:17:20-04  gsl
**	Fix text that was overlayed
**
**	Revision 1.9  1998-05-18 16:43:23-04  gsl
**	Add the FORMAT keyword to allow specifing a Vision Version number.
**	FOR ACUCOBOL just like what was being done for MF
**
**	Revision 1.8  1997-08-15 10:37:37-04  scass
**	Added NEOM to app string, fixed date.
**
**	Revision 1.7  1997-08-13 10:37:39-04  scass
**	Added code to prompt for the file format when using Micro Focus
**	COBOL.  This is needed for proper identification of the
**	extended formats available.
**
**	Revision 1.6  1997-08-07 17:36:25-04  scass
**	Added defins of CHARTYPE and added comments
**
**	Revision 1.5  1996-10-02 20:12:41-04  gsl
**	fixed pfkey tags for w4w support
**
**	Revision 1.4  1996-10-02 15:13:52-07  gsl
**	Fix squeeze() routines
**
**	Revision 1.3  1996-10-02 09:11:28-07  gsl
**	Add standard headers
**	Fix prototypes and warnings
**	Removed duplicate clear_keys() routine.
**
**
**
*/
