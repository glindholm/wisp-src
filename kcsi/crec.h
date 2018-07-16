/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

#ifndef	_CREC_H
#define	_CREC_H
/*----
Locations of various fields in the control and other records
------*/

#define	PKEY_POS	11
#define	AKEY1_POS	(10 + 72)
#define	AKEY2_POS	(AKEY1_POS + 82)

/*----
A structure for some of the parts of the Control record
can be filled out later (used in listmgmt)
------*/

/*----
The control field record
------*/
typedef struct _ctrlf{
	char    new_seq[2];		/* Used only in ctrlseq */
	char    name[8];
	char	type[1];		/* Char Uns Pak Zone Binary */
	char	int_len[3];
	char	start_pos[4];
	char	occurrences[2];
	char	zero_suppress[1];	/* 0-No 1-Yes 2-protect */
	char	decimal_insert[1];
	char	sign_control[1];	/* 0-No 1-99- 2-99CR 3-99DB */
	char	dollar_comma[1];	/* 0-None 1-, 2-$ 3-$, */
	char	ext_len[3];
	char	report_code[1];
	char	update_code[1];
	char	decimal_pos[1];
	char	binary_edit[1];
	char	filler2[1];
	char	update_seq[2];
	char	validation[2];		/* "R "ange "CF"cummulative "T "able */
					/* "DF" mmddyy "GD" yymmdd "DS" yyddd*/
					/* "TS" hhmmss99 "CS" create stamp */
					/*  "CM" mod stamp */
	char	table_name[6];
	char	lo_range[16];
	char	hi_range[16];
	char	packed_digits[2];
	char	cummulative_name[8];
	char	alias[31];
	char	count_or_occur[2];	/*grp cnt or elem occur */
	char	seq_or_len[3];		/*grp seq or elem len */
	char	filler3[4];
	char	display_code[1];
	char	edited_len[3];
	char	default_fac[1];
	char	filler4[1];
	}CTRLF;

typedef struct _ctrlh{
	char fill1[2];
	char hkey[8];	
	char type[1];	
	char primary[8];
	char report[1];
	char update[1];
	char delete[1];
	char rec_len[4];
	char timeout[3];
	char user_exit[8];
	char altkey_count[2];
	char fill2[8];
	char de_spacing[1];
	char org[1];
	char open_mode[1];
	char data_file[8];
	char data_lib[8];
	char data_vol[8];
	}CTRLH;
	



#endif	/* _CREC_H */
/*
**	History:
**	$Log: crec.h,v $
**	Revision 1.3  1996/09/17 23:34:03  gsl
**	drcs update
**	
**
**
*/
