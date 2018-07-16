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

#include "dbsc.h"
#include "dglb.h"
#include "shrthand.h"
#include "iocode.h"
#include "cobioblk.h"
#include "dtype.h"
#include "dmnt.h"
#include "kcsifunc.h"


static void val_one_fld(FIELD *fld);
static void val_one_range(FIELD *fld);
static void val_one_table(FIELD *fld);
static int chkval(char *mem1,char *mem2,int len);

/*----
			VALIDATION ROUTINES
------*/
void dte_val_all_fields()
{

	int i;
	for(i = 0; dtefld[i].name[0] > ' '; ++i)
		{
		if(dtefld[i].frow)
			val_one_fld(&dtefld[i]);
		}

}

static void val_one_fld(FIELD *fld)
{
	if(Streq(fld->val,"T "))
		val_one_table(fld);
	if(Streq(fld->val,"CT"))
		val_one_table(fld);
	if(Streq(fld->val,"R "))
		val_one_range(fld);
	if(Streq(fld->val,"CR"))
		val_one_range(fld);
}

/*----
Validate against the range given in the control record
------*/
static void val_one_range(FIELD *fld)
{
	int lorc,hirc;

	static char out_of_range[] = "Error - Entry is out of range.";
	if(((lorc = chkval(fld->lo,fld->fld,fld->edit_len)) < 1) &&
	   ((hirc = chkval(fld->fld,fld->hi,fld->edit_len)) < 1) )
		return;
	else
		KCSI_field_error(fld,out_of_range);
}

static void val_one_table(FIELD *fld)
{
	static char out_of_table[]="Error - Value not in the table for this field.";
	static char no_table[]="Error - Table file not found.";
	static char tio_block[3500];
	static char table_record[16];
	static char ufb[1];

	if(fld->table[0] < 33)
		return;
	INIDIO(tio_block);
	tio_block[ORG_POS] = 'I';
	memcpy(&tio_block[IO_POS],OPEN_INPUT,IO_LEN);		/*Open Input*/
	memcpy(&tio_block[NAME_POS],fld->table,6);		/*Table File Name*/
	memcpy(&tio_block[LIBRARY_POS],&dte_cio_block[LIBRARY_POS],14); /*ctrl lib and vol*/
	memcpy(&tio_block[PRNAME_POS],"TABLE   ",PRNAME_LEN);	/*PRNAME*/
	KCSIO(tio_block,ufb,table_record);			/*OPEN*/
	if(memcmp(&tio_block[STATUS_POS],"00",STATUS_LEN))	/*IO not ok*/
		{
		KCSI_field_error(fld,no_table);
		return;
		}
	memcpy(table_record,fld->fld,fld->edit_len);
	memcpy(&tio_block[IO_POS],READ_RECORD,IO_LEN);		/*read*/
	KCSIO(tio_block,ufb,table_record);
	if(memcmp(&tio_block[STATUS_POS],"00",STATUS_LEN))	/*IO not ok*/
		{
		KCSI_field_error(fld,out_of_table);
		}
	memcpy(&tio_block[IO_POS],CLOSE_FILE,IO_LEN);		/*Close up*/
	KCSIO(tio_block,ufb,table_record);
}

/*----
Included as memcmp does not appear to work as advertised.
------*/
static int chkval(char *mem1,char *mem2,int len)
{
	int result;
	while(len)
		{
		if((result = (*mem1 &0xff) - (*mem2 & 0xff)) != 0)
			break;
		--len;
		++mem1;
		++mem2;
		}
	return(result);
}

/*
**	History:
**	$Log: dval.c,v $
**	Revision 1.6  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.5  2002/10/24 15:48:33  gsl
**	Make globals unique
**	
**	Revision 1.4  2002/07/25 15:20:28  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:45:36  gsl
**	drcs update
**	
**
**
*/
