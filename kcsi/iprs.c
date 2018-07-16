static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <ctype.h>
#include "strtkn.h"
#include "itkn.h"
#include "iglb.h"
#include "shrthand.h"
#include "rptprm.h"
#include "rptcob.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)iprs.c	1.5 8/3/93";

/*----
The primary addition to the inquiry program from a 'C' perspective
is the routine to parse the data limit request.

It receives the report_records (several arrays of data) and
the screen of requests.
The passing fields are
the data limits array,
the report records
the result screen 7*79
the received screen 7*79
a message field 79
a return code 99

The rfls and data limits arrays are described in WSRPTREC.WCB.
------*/
/*----
These are offsets and values into the passed rfl and dl fields.
------*/

#define	OUT_OF_RANGE		99
#define	MAX_RFLS		(RFL_ENTRY_COUNT - 1)

static int last_token;
static char *first_dlo,*next_dlo;
static int didx = 0;
static int dline_idx = 0;


static char* next_inq_dl;
static char *next_inq_rfl;
static char *inq_inqd;
static char *inq_msg;
static char *inq_rfl;
static char *inq_dl;
static int inq_rfl_idx = 0;
static int inq_dl_idx = 0;
static int inq_dlo_idx = 0;


static int parse_it();
static int process_one_tkn(int status);
static int add_to_dl(char *str);
static int add_operator(char *str);
static int add_operand(char *str);
static void add_field_operand(char *str);
static void add_lit_operand(char *str);
static void add_connector(char *str);
static void end_current_dl();
static void add_to_dest(char *str);
static int add_to_rfl(char *str);
static void add_occurs(char *str);
static void add_occurs_here(char *dest,char *src);
static void emsg(char *str);
static void imsg(char *str);
static void lmsg(char *str,int count);
static void squeeze_rfls();

void PARSEINQ(char *rpt_rfl,char *rpt_dl,char *res_flds,char *scr_flds,char *msg,char *ret_code)
{

	int rc;
	char rcbuf[3];

/* zero the return code */
	memcpy(ret_code,"00",2);
/* make some passed values globally available */
	inq_rfl = rpt_rfl;
	inq_dl  = rpt_dl;
	inq_inqd = res_flds;
	inq_inqs = scr_flds;
	inq_msg = msg;

	rc = parse_it();
	sprintf(rcbuf,"%02d",rc);
	memcpy(ret_code,rcbuf,2);
	squeeze_rfls();
	
}

/*----
Parsing consists of repeatedly pulling tokens from the input
and placing them in the output as well as into the receiving
fields.
------*/
/*----
Statuses for parsing errors
------*/
static int parse_it()
{
/* status field holds the next type of token expected */
	int status;

	dline_idx = didx = 0;
	next_inq_rfl = inq_rfl;
	inq_rfl_idx = 0;
	next_inq_dl = inq_dl;
	inq_dl_idx = 0;
	first_dlo = next_inq_dl + DLO_ENTRY_POS;
	next_dlo = first_dlo;
	inq_dlo_idx = 0;
	first_inq_tkn();
	if(inq_token != LIST_TKN)
		{
		emsg("Initial command must be LIST, PRINT, SELECT etc.");
		return(99);
		}
	status = LIST_TKN;
	last_token = 0;
	while(inq_token != LAST_TKN)
		{
		if(inq_token == INVALID_TKN)
			{
			imsg("Do not recognize %s.");
			return(99);
			}
		if(inq_token != NO_TKN)
			{
			status = process_one_tkn(status);
			if(status == INVALID_TKN)
				return(99);
			}
		last_token = inq_token;
		next_inq_tkn();
		}
	return(0);
}

/*----
The incoming tokens will be formatted from an input such as
LIST A B C WHEN A > 0 AND < 100 OR D NE 5 AND 6

This is translated into an array such that

REPORT FIELDS A B C D 
IF
	A GT 0		AND
	  LT 100 

	OR

	D NE 5		AND
	  NE 6

------*/
static int process_one_tkn(int status)
{
/*
 * If the incoming token is an occurrence field then it is handled as an
 * extension of a field token. STatus does not change, but the occurs
 * must follow a field token.
 */
	if(inq_token == OCC_TKN)
		{
		if(last_token != FIELD_TKN)
			{
			imsg("Invalid occurs clause at %s.");
			return(INVALID_TKN);
			}
		else
			{
			add_to_dest(inq_tokens);
			add_occurs(inq_tokens);
			return(status);
			}
		}
/*
 * Process incoming tokens based on what is expected.
 */
	switch(status)
		{
/* A list token must arrive as the first field */
		case LIST_TKN:
			if( inq_token != LIST_TKN )
				{
				emsg("Expected Initial LIST or PRINT verb.");
				return(INVALID_TKN);
				}
			add_to_dest(inq_tokens);
			status = FIELD_LIST_TKN;
			break;
/* During a field list, a field or IF can arrive */
		case FIELD_LIST_TKN:
			if (inq_token == FIELD_TKN)
				{
				add_to_dest(inq_tokens);
				if(add_to_rfl(inq_tokens) == OUT_OF_RANGE)
					{
					lmsg("List exceeds %d Fields.",MAX_RFLS);
					return(INVALID_TKN);
					}
				}
			else
			if (inq_token == IF_TKN)
				{
				add_to_dest(inq_tokens);
				status = FIELD_TKN;
				}
			else
				{
				imsg("Expected Field Name or IF clause at %s.");
				return(INVALID_TKN);
				}
			break;
/* A field on its own as the intriduction to a set */
field_tkn_code:
		case FIELD_TKN:
			if (inq_token != FIELD_TKN)
				{
				imsg("Expected Field Name at %s.");
				return(INVALID_TKN);
				}
			else
				{
				add_to_dest(inq_tokens);
				if(add_to_rfl(inq_tokens) == OUT_OF_RANGE)
					{
					lmsg("List exceeds %d Fields.",MAX_RFLS);
					return(INVALID_TKN);
					}
				if(add_to_dl(inq_tokens) == OUT_OF_RANGE)
					{
					lmsg("Maximum of %d Data Limits Allowed.",LMG_DL_ENTRY_COUNT);
					return(INVALID_TKN);
					}
				status = OPERATOR_TKN;
				}
			break;
/* And operator LT GT EQ NEQ ETC. */
operator_tkn_code:
		case OPERATOR_TKN:
			if (! (inq_token & OPERATOR_TKN) )
				{
				imsg("Expected Operator EQ LT GE etc. at %s.");
				return(INVALID_TKN);
				}
			else
				{
				add_to_dest(inq_tokens);
				add_operator(inq_tokens);
				if(add_operator(inq_tokens) == OUT_OF_RANGE)
					{
					lmsg("Maximum of %d Comparisons per Limit Set.",DLO_ENTRY_COUNT);
					return(INVALID_TKN);
					}
				status = OPERAND_TKN;
				}
			break;

/* Operand can be a field or a lit */
		case OPERAND_TKN:
			if(!(inq_token & OPERAND_TKN))
				{
				imsg("Expected Field Name or Constant at %s.");
				return(INVALID_TKN);
				}
			else
				{
				add_to_dest(inq_tokens);
				if(add_operand(inq_tokens) == OUT_OF_RANGE)
					{
					lmsg("List exceeds %d Fields.",MAX_RFLS);
					return(INVALID_TKN);
					}
				status = CONNECT_TKN;
				}
			break;
/* AND / OR can connect with in a set or one set to the other */
		case CONNECT_TKN:
			if(!(inq_token & CONNECT_TKN))
				{
				imsg("Expected AND/OR at %s.");
				return(INVALID_TKN);
				}
			else
				{
				add_to_dest(inq_tokens);
				add_connector(inq_tokens);
				status = (FIELD_TKN | OPERATOR_TKN);
				}
			break;
/* A field starting a new set, or an operator contnuing the current set*/
		case FIELD_TKN | OPERATOR_TKN:
			if(( inq_token != FIELD_TKN ) &&
			   ( ! (inq_token & OPERATOR_TKN) ) )
				{
				imsg("Expected Field Name or Operator EQ LT GE etc. at %s.");
				return(INVALID_TKN);
				}
			else
			if(inq_token & OPERATOR_TKN)
				{
				goto operator_tkn_code;
				}
			else
				{
				end_current_dl();
				goto field_tkn_code;
				}
			break;
		}
	return(status);
}

/*----
Set up the field that will be used as the basis for the set of comparisons
------*/
static int add_to_dl(char *str)
{
	if(inq_dl_idx == LMG_DL_ENTRY_COUNT)
		return(OUT_OF_RANGE);
	memcpy(&next_inq_dl[DL_NAME_POS],str,strlen(str));
	memcpy(&next_inq_dl[DL_OCCURRENCE_POS],"00",2);
	memcpy(&next_inq_dl[DL_ORIGIN_POS],"1",1);
	first_dlo = next_inq_dl + DLO_ENTRY_POS;
	next_dlo = first_dlo;
	inq_dlo_idx = 0;
	next_inq_dl += DL_ENTRY_LEN;
	++inq_dl_idx;
	return(0);
}

/*----
Add the operator to the DL
------*/
static int add_operator(char *str)
{
	char *get_inq_tkn_str();

	if(inq_dlo_idx == DLO_ENTRY_COUNT)
		return(OUT_OF_RANGE);

	memcpy(&next_dlo[DLO_OP_CODE_POS],get_inq_tkn_str(inq_token),2);
	return(0);
}

/*----
And an operand. An operand is either a field name or a lit.
------*/
static int add_operand(char *str)
{
	if(inq_token == FIELD_TKN)
		{
		add_field_operand(str);
		if(add_to_rfl(str) == OUT_OF_RANGE)
			{
			lmsg("List exceeds %d Fields.",MAX_RFLS);
			return(INVALID_TKN);
			}
		}
	else
		add_lit_operand(str);
	next_dlo += DLO_ENTRY_LEN;
	++inq_dlo_idx;
	return(0);
}

static void add_field_operand(char *str)
{
	memcpy(&next_dlo[DLO_NAME_POS],str,strlen(str));
	memcpy(&next_dlo[DLO_ORIGIN_POS],"1",1);
	memcpy(&next_dlo[DLO_NOT_LIT_CODE_POS],"X",1);	/*Not lit flag*/
}

static void add_lit_operand(char *str)
{
	memcpy(&next_dlo[DLO_LIT_POS],str,strlen(str));
}

/*----
Add the and or OR the pointer to the current DLO has been moved on by
one so it must be brought back filled in and pushed on.
------*/
static void add_connector(char *str)
{
	next_dlo -= DLO_ENTRY_LEN;
	memcpy(&next_dlo[DLO_CONNECTOR_POS],str,1);
	next_dlo += DLO_ENTRY_LEN;
}

/*----
The current set has ended. The connector which should be a set connector
has been left as a connector within the last set. Both the dl and dlo
are one beyond the point that they should be.
------*/
static void end_current_dl()
{
	
	next_dlo -= DLO_ENTRY_LEN;
	next_inq_dl -= DL_ENTRY_LEN;
	memcpy(&next_inq_dl[DL_SET_CONNECTOR_POS],&next_dlo[DLO_CONNECTOR_POS],1);
	memcpy(&next_dlo[DLO_CONNECTOR_POS]," ",1);
	next_dlo += DLO_ENTRY_LEN;
	next_inq_dl += DL_ENTRY_LEN;
}

/*----
Concatenate a string to the out put result fields. Add a trailing space.
------*/
static void add_to_dest(char *str)
{
	int len;
	char *tstr, *get_inq_tkn_str();

	if(tstr = get_inq_tkn_str(inq_token))
	    str = tstr;
	len = strlen(str);
	if (len > (INQ_FIELD_LEN - didx))
		{
		didx = 0;
		++dline_idx;
		}
	memcpy(&inq_inqd[(dline_idx * INQ_FIELD_LEN) + didx],str,len);
	didx += (len + 1);
}

/*----
Add a field to the list of all fields.By copying in the name, the
occurrence as a zero and the origin of "1".
------*/
static int add_to_rfl(char *str)
{
	if (inq_rfl_idx == MAX_RFLS)
		return(OUT_OF_RANGE);
	memset(&next_inq_rfl[RFL_COL_HEAD_POS],' ',50);
	memset(&next_inq_rfl[RFL_NAME_POS],' ',8);
	if(strlen(str) > 8)
		memcpy(&next_inq_rfl[RFL_COL_HEAD_POS],str,strlen(str));
	else
		memcpy(&next_inq_rfl[RFL_NAME_POS],str,strlen(str));
	memcpy(&next_inq_rfl[RFL_OCCURRENCE_POS],"00",2);
	memcpy(&next_inq_rfl[RFL_ORIGIN_POS],"1",1);
	next_inq_rfl += RFL_ENTRY_LEN;
	++inq_rfl_idx;
	return(0);
}

/*----
Add an occurrence value to the previous rfl. It is also possible that the
field in question has just been added to a dlo or a dl
------*/
static void add_occurs(char *str)
{

/*
 * First the rfl
 */

	next_inq_rfl -= RFL_ENTRY_LEN;
	add_occurs_here(&next_inq_rfl[RFL_OCCURRENCE_POS],str);
	next_inq_rfl += RFL_ENTRY_LEN;
/*
 * Now if the next dlo pointer is not set to the first dlo pointer
 * then we just moved past a field.
 * So go back and fill in the occurs.
 * Otherwise test if the DL has advanced in which case a field
 * was just added to DL so add the occurrs there.
 */
	if( next_dlo != first_dlo)
		{
		next_dlo -= DLO_ENTRY_LEN;
		memcpy(&next_dlo[DLO_OCCURRENCE_POS],"(00)",4);
		add_occurs_here(&next_dlo[DLO_OCCURRENCE_POS + 1],str);
		next_dlo += DLO_ENTRY_LEN;
		}
	else
	if( next_inq_dl != inq_dl)
		{
		next_inq_dl -= DL_ENTRY_LEN;
		add_occurs_here(&next_inq_dl[DL_OCCURRENCE_POS],str);
		next_inq_dl += DL_ENTRY_LEN;
		}
}

static void add_occurs_here(char *dest,char *src)
{
	int len;

	len = strlen(src);
	if(len == 3)
		memcpy(dest+1,src+1,1);
	else
		memcpy(dest,src+1,2);

}

/*----
Output and error message.
------*/
static void emsg(char *str)
{
	memcpy(inq_msg,str,strlen(str));
}

/*----
Output an error message using inq_tokens to fill in the blank
------*/
static void imsg(char *str)
{
	char msg_buf[256];

	sprintf(msg_buf,str,inq_tokens);
	msg_buf[79] = 0;
	emsg(msg_buf);
}

static void lmsg(char *str,int count)
{
	char msg_buf[256];

	sprintf(msg_buf,str,count);
	msg_buf[79] = 0;
	emsg(msg_buf);
}




/*----
Remove duplicate rfls
------*/
static void squeeze_rfls()
{
	char *rfl1,*rfl2,*rfl3,*rfl4;

	rfl1 = inq_rfl;
	while(memcmp(&rfl1[RFL_NAME_POS],"        ",8))
		{
		rfl2 = rfl1 + RFL_ENTRY_LEN;
		while(memcmp(&rfl2[RFL_NAME_POS],"        ",8))
			{
			while((Memeq(&rfl1[RFL_NAME_POS],&rfl2[RFL_NAME_POS],8)) &&
			   (Memeq(&rfl1[RFL_OCCURRENCE_POS],&rfl2[RFL_OCCURRENCE_POS],2)))
				{
				rfl3 = rfl2;
				while(memcmp(&rfl3[RFL_NAME_POS],"        ",8))
					{
					rfl4 = rfl3 + RFL_ENTRY_LEN;
					memcpy(rfl3,rfl4,RFL_ENTRY_LEN);
					rfl3 += RFL_ENTRY_LEN;
					}
				}
			rfl2 += RFL_ENTRY_LEN;
			}
		rfl1 += RFL_ENTRY_LEN;
		}
}
/*
**	History:
**	$Log: iprs.c,v $
**	Revision 1.3.2.1  2002/11/12 15:56:26  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
**	
**	Revision 1.7  2002/10/24 15:48:32  gsl
**	Make globals unique
**	
**	Revision 1.6  2002/10/23 21:07:26  gsl
**	make global name unique
**	
**	Revision 1.5  2002/10/17 17:17:18  gsl
**	Removed VAX VMS code
**	
**	Revision 1.4  2002/07/25 15:20:27  gsl
**	Globals
**	
**	Revision 1.3  1996/09/17 23:45:38  gsl
**	drcs update
**	
**
**
*/
