#ifndef VPLUS_INCLUDED
#define VPLUS_INCLUDED

struct vplus_comarea
{
	int2	cstatus;	/* Status, error returns.		*/
	int2	language;	/* Language of calling programs.	*/
	int2	comarealen;	/* Length (in 16 bit words) of COMAREA	*/
	int2	usrbuflen;	/* COMAREA extension length (BASIC)	*/
	int2	cmode;		/* Current mode (collect or browse).	*/
	int2	lastkey;	/* Code of last key pressed.		*/
	int2	numerrs;	/* Number of errors in current form.	*/
	int2	windowenh;	/* Code for window enhancement.		*/
	int2	multiusage;	/* Next form flag (parent/son).		*/
	int2	label_opt;	/* Function key label indicator.	*/
	char	cfname[15];   	/* Current form name.			*/
	char	filler4[1];	/* 1 byte filler to keep word boundary.	*/
	char	nfname[15];	/* Next form name.			*/
	char	filler5[1];	/* 1 byte filler to keep word boundary.	*/
	int2	repeatapp;	/* Repeat flag (freeze/append).		*/
	int2	freezapp;	/* Next form flag (freeze/append).	*/
	int2	cfnumlines;	/* Number of lines in current form.	*/
	int2	dbuflen;	/* Data bufer length in bytes.		*/
	int2	filler6;	/* Reserved for system use.		*/
	int2	lookahead;	/* Form preload indicator.		*/
	int2	deleteflag;	/* Delete current batch record.		*/
        int2	showcontrol;	/* VSHOWFORM control flags.		*/
	int2	filler7;	/* Reserved for system use.		*/
        int2	printfilenum;	/* File number of forms print file.	*/
	int2	filerrnum;	/* MPE file error number from FCHECK.	*/
	int2	errfilenum;	/* MPE file number, error message file.	*/
	int2	numforms;	/* Number of forms in buffer.		*/
	int2	filler8;	/* Reserved for system use.		*/
	int2	filler9;	/* Reserved for system use.		*/
	int2	filler10;	/* Reserved for system use.		*/
	int4	numrecs;	/* Number of records in batch file.	*/
	int4	recnum;		/* Record number of current batch.	*/
	int2	filler11;	/* Reserved for system use.		*/
	int2	filler12;	/* Reserved for system use.		*/
	int2	filen;		/* MPE file number of terminal.		*/
	int2	filler13;	/* Reserved for system use.		*/
	int2	filler14;	/* Reserved for system use.		*/
	int2	filler15;	/* Reserved for system use.		*/
	int2	filler16;	/* Reserved for system use.		*/
	int2	filler17;	/* Reserved for system use.		*/
	int2	retries;	/* Maximum number of retries.		*/
	int2	options;	/* Suppress messages and autoread.	*/
	int2	environ;	/* Terminal environment - system LDEV.	*/
	int2	usertime;	/* User defined time out length.	*/
	int2	identifier;	/* Type of terminal.			*/
	int2	labinfo;	/* Number and length of function keys.	*/
};

struct vplus_entry_table
{
	char	field_name[16];		/* Field name.			*/
	int2	order_number;		/* Order number.		*/
	int2	field_number;		/* Field number.		*/
	int2	field_length;		/* Field length.		*/
	int2	field_position;		/* Field position.		*/
	int2	field_enhancements;	/* Field enhancements.		*/
};

struct vplus_infobuf
{
	int2	number_of_entries;	/* Entries in the buffer.	*/
	int2	entry_length;		/* Length of the entries.	*/
	char	form_name[16];		/* Name of the form.		*/
	struct	vplus_entry_table entry_table[128];
};

#endif /* VPLUS_INCLUDED */
