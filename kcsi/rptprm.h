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


#ifndef	_RPTPRM_H
#define	_RPTPRM_H
#include "dtype.h"
#include "kcsio.h"

#define	NF_OP_ENTRY_COUNT	10
#define	NF_ENTRY_COUNT		10
#define	RTT_ENTRY_COUNT		6
#define	RCB_ENTRY_COUNT		5
#define	RCD_ENTRY_COUNT		5
#define	RFL_ENTRY_COUNT		81
#define	CRF_ENTRY_COUNT		81
#define	DL_OP_ENTRY_COUNT	10
#define	DLO_ENTRY_COUNT		DL_OP_ENTRY_COUNT
#define	DL_ENTRY_COUNT		10
/* 
 * This actually for LPI, but tested under ACUCOBOL since I haven't got
 *  LPI
*/
#define	LMG_DL_ENTRY_COUNT	100

#define	SRT_ENTRY_COUNT		8
/*----
And the C version
------*/
/*----
Every field in report is identifiable by locating the field
name, the occurrence number and the origin. These three fields
establish the uniqueness of a field.
------*/
typedef	struct _element{
	char _name[20];
	int  _occurrence;
	int _origin;
	}ELEMENT;

/*----
This structure is used to keep the totalling for control breaking
puposes.
------*/
typedef struct _sum{
	double _total;
	double _max;
	double _min;
	}SUM;

/*----
General control information about the report.
------*/
typedef struct _rpt_opt{
	char _id[9];	/* Not used on the 'C' side as RPTDEF is loaded*/
	char _date[9];	/* Edited date */
	int _device;	/* 1 printer 0 = display*/
	int _option;	/* 0=NO, -1=YES,  1-999 number of records to print*/
	int _only;	/* TRUE FALSE */
	int _page;	/* page length 5 - 99 */
	int _line[3];	/* each position can be 1 or 2 or 3 (mutually ex)*/
	int _spacing[3]; /* each can be 0 - 5 */
	int _ext_sort;	/* yes or no */
	}RPT_OPT;

/*----
A header record with some flags for sort data limit secondary
file etc. This also includes the key positions and lengths
for primary to secondary file chaining.
------*/
typedef	struct _rpt_rhd{
	int _sec_file;
	int _sort;
	int _data_limit;
	int _control_fields;
	int _column_subs;
	char _key_to_sec[9];
	int _occurrence;
	DTYPE *_old_key1;	/* Contain the old positions before*/
	DTYPE *_old_key2;	/* The record is unpacked*/
	}RPT_RHD;

/*----
The operands that make up a new field.
------*/
typedef	struct	_rpt_nfo{
	ELEMENT _e;
	int  _code;		/* OPCODE & + / * - */
	int  _not_lit;		/* True if not a literal */
	DTYPE **_pnew;		/* Into record if not a lit or to next field*/
	DTYPE _lit_op;		/* Pseudo op for lits */
	}RPT_NFO;
/*----
A new field description.
------*/
typedef	struct	_rpt_nf{
	ELEMENT _e;
	RPT_NFO	_o[NF_OP_ENTRY_COUNT + 1];	/* And how to build it */
	DTYPE	_fld;				/* type len and dec only*/
	DTYPE   **_pnew;
	int	_origin2;	/*set if new field includes file 2 fields*/
	}RPT_NF;
/*----
This is the raw data describing each field used in the report,
and data on how it is to be edited and printed.
------*/
typedef	struct _rpt_rfl{
	ELEMENT _e;
/* Output formatting specs */
	int _line;
	int _lseq[4];		/* added 3 seqs for listlmgmt */
	char _col_head[26];
	char _col_sub_head[26];
	int _spaces;
	int _ext_size;
	int _zero_char;
	int _zero_count;
	int _sign_control;	/* 1 CR on -, 2 DB on - ,9 trailing -*/
	int _dec_carry;		/* 0 - 9 or -1 (prints all but no '.')*/
	int _insert_char[3];
	int _insert_count[3];
	int _dollar_sign;	/* 1 fixed, 2 floating*/
	int _comma;

/* CONTROL BREAKING INFORMATION */
	int _total;
	int _max;
	int _min;
	int _avg;
	SUM _val[6];
	int _is_a_break;
	int _rcbidx;		/* And which level it is */
	int _repeat_code;	/* And whether to repeat it */
	RPT_NF  *_nf;           /* Get into the new field def */

/* Reading and reformatting information */
	DTYPE *_old;		/* Position in the old record */
	DTYPE *_new;		/* And in the new */

	int _col;		/* The column at which the field is printed */
	int _is_breaking;	/* break flag */
	double _value;

/* Additional fields for LISTMGMT */
	int lmg_supp_lspace;	/* Space handling for char fields */
	int lmg_supp_tspace;
	int lmg_comp_space;
	int lmg_case;		/* Case handling for char fields */
	int lmg_case_table;
	char *lmg_case_file;
	char *lmg_case_lib;
	char *lmg_case_vol;
	char *lmg_case_sysname;
	}RPT_RFL;

/*----
5 control breaks and what to do for each.
------*/
typedef struct _rpt_rcb{
	ELEMENT _e;             /* Identification*/
	int _action_code;	/* Lines to skip or -1 to page eject*/
	int _repeat_code;	/* Repeat break on each line TRUE FLASE */
	char _break[41];	/* What to say*/
	int _col;		/* Where to say it*/
	DTYPE **_pnew;		/* Where is it in the new record*/
	long _rec_count;
	char _brk_val[133];
	int _is_breaking;
	RPT_RFL *_rfl;
	}RPT_RCB;

/*----
Pages and report titles. This first 3 appear on page 1.
The second 3 appear on subsequent pages is selected otherwise
the first three are used.
------*/
typedef struct _rpt_rtt{
	char _title[133];
	}RPT_RTT;

/*----
Data limits used to select records for printing and not again
thereafter.
------*/
typedef	struct	_rpt_dlo{
	ELEMENT _e;
	char _code[3];		/* Comp Op EQ NE GT LT GE LE */
	int  _connector;	/* 'A'nd 'O'r */
	int  _not_lit;		/* True when not a lit */
	DTYPE **_pnew;		/* Points into new record */
	int _origin2;		/* True if limit includes a file 2*/
	int _origin3;		/* True if limit includes a built field*/
	DTYPE _lit_op;		/* Pseudo op for lits */
	}RPT_DLO;

typedef	struct	_rpt_dl{
	ELEMENT	_e;				/* The field */
	DTYPE	**_pnew;				/* Where */
	RPT_DLO	_o[DL_OP_ENTRY_COUNT + 1];	/* How to test it */
	int	_set_connector;			/* 'A'nd 'O'r */
	int	lmg_skip_space;			/* select when fld spaces */ 
	int	lmg_skip_zero;			/* select when fld zero */
	int	lmg_rec_no;		/* when range is record nums */
	}RPT_DL;

/*----
8 sort descriptions.
------*/
typedef struct _rpt_srt{
	ELEMENT _e;		/* The name */
	int _order;		/* 'A' or 'D'*/
	DTYPE **_pnew;		/* Position in the new record */
	DTYPE **_pold;		/* Position in the old record */
	}RPT_SRT;

/*----
All of the above combined in a structure.
------*/
/* The report definition */
typedef	struct	_rpt_def{
	RPT_OPT	*_opt;
	RPT_RHD	*_rhd;
	RPT_RCB	*_rcb;
	RPT_RFL	*_rfl;
	RPT_RTT	*_rtt;
	RPT_SRT	*_srt;
	RPT_DL	*_dl;
	RPT_NF	*_nf;
	DTYPE   *_old;
	DTYPE	*_new;
	KCSIO_BLOCK	*_kfb1;
	KCSIO_BLOCK	*_kfb2;
	char	*_input;
	}RPT_DEF;


char *rpt_format_a_field(RPT_RFL *rfl, char *d, int code);


#endif

/*
**	History:
**	$Log: rptprm.h,v $
**	Revision 1.7  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.6  2002/10/24 14:20:34  gsl
**	Make globals unique
**	
**	Revision 1.5  2002/10/23 21:07:25  gsl
**	make global name unique
**	
**	Revision 1.4  2002/10/17 17:17:22  gsl
**	Removed VAX VMS code
**	
**	Revision 1.3  1996/09/17 23:34:18  gsl
**	drcs update
**	
**
**
*/
