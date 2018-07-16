			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* 
	FILE_GETPARM    This routine will allow operator input of a filename, by one of two methods: WANG native or VAX/VMS/unix
			native mode.
 			File_getparm() was changed to file_getparm2() to add the 3 printer paramters.				
*/

#include <stdio.h>
#include <varargs.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wangkeys.h"
#include "wperson.h"
#include "movebin.h"
#include "werrlog.h"
#include "wdefines.h"											

#define WANG_MODE 0
#define NATIVE_MODE !WANG_MODE

static char rval[4];									/* The place to put the return code.	*/

static	int4	N[255];
static	int4	Ni=0;
static	int4 	two=2;

static char *inf_msg[] = 
{
	"(TO BE CREATED AS OUTPUT BY THE PROGRAM)",
	"(TO BE UPDATED (SHARED) BY THE PROGRAM)",
	"(TO BE USED AS INPUT BY THE PROGRAM)",
	"(TO BE UPDATED (I-O) BY THE PROGRAM)",
	0 
};

file_getparm2(f_mode,file,lib,vol,prname,issuer,entry_mode,getparm_type,native_path,msg1,msg2,pfkey_rcvr,intv_type,orig_file,
		prtclass, form, copies)
int4 f_mode;
char *file, *lib, *vol, *prname, *issuer, *orig_file;
int4 *entry_mode;
char *getparm_type;
char *native_path;
char *msg1,*msg2;
char *pfkey_rcvr;
char intv_type;							/* 'E' Error  'R' Rename (open output, file exists)		*/
char	*prtclass;
int4	*form;
int4	*copies;
{                                                  
#define		ROUTINE		18000

	int4	pfkey;
	int	i,inf;
	char	*error_msg;
	int4	va_cnt;
	int4	*long_ptr;
	char	pf_footer[100];
	char	temp[100];
	char	temp2[10];
	char	filename[80];
	int	done, type_ID;
	char	prtform[10], prtcopies[10];

	struct argst { char *ptrs[160]; } args;
	int4	cnt;
#define GP	args.ptrs[cnt++] = (char *)

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	*pfkey_rcvr = ENTER_KEY_PRESSED;					/* Default the reciever to '@'			*/
	
	dispchars(file,8);							/* Remove all non-display characters		*/
	dispchars(lib,8);
	dispchars(vol,6);
	dispchars(native_path,sizeof(filename));

	if ((f_mode & IS_PRINTFILE) && (f_mode & IS_OUTPUT))				/* Is this a print file open for output	*/
	{
		sprintf(prtform,"%03ld",*form);
		sprintf(prtcopies,"%05ld",*copies);
	}

	error_msg = "YOU MUST ENTER THE NAME AND LOCATION OF THE FILE.           ";	/* Initialize the  the error msg.	*/

	if (!Ni) 
	{
		for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 
		{
			N[i]=(int4)i; 
			wswap(&N[i]);
		}
		++Ni;
	}

	wpload();									/* Get user personality and defaults.	*/

	if (intv_type == 'R')
		pfkey = PFKEY_3_ENABLED|PFKEY_5_ENABLED|PFKEY_16_ENABLED;
	else
		pfkey = PFKEY_5_ENABLED|PFKEY_16_ENABLED;				/* Only two keys enabled.		*/

	wswap( &pfkey );								/* Do system dependent swap		*/

	if (f_mode & IS_IO)
	{
		if (f_mode & IS_NOWRITE)
			inf = 3;
		else
			inf = 1;
	}
	else if (f_mode & IS_OUTPUT) inf = 0;
	else inf=2;

	memcpy(filename, native_path, sizeof(filename));				/* Initialize filename.			*/

	strncpy(temp2,orig_file,8);
	temp2[8]=(char)0;
	sprintf(temp,"\"%s\"",temp2);

	done = type_ID = 0;
	if (!strncmp(getparm_type,"ID",2)) ++type_ID;					/* "ID" doesn't loop (HIDDEN)		*/

	do
	{
		char	*mode_string;

		cnt = 0;
		GP getparm_type; GP "R"; 	GP prname; 	GP pfkey_rcvr;	GP "0001"; 	GP issuer;	GP &N[0];
		GP msg1?"T":"t"; GP msg1;	GP &N[msg1?strlen(msg1):0];	GP "A";	GP &N[9];	GP "A";	GP &N[2];
		GP msg2?"T":"t"; GP msg2;	GP &N[msg2?strlen(msg2):0];	GP "A";	GP &N[10];	GP "A";	GP &N[2];
		GP "T";	GP "PLEASE ASSIGN ";	GP &N[14];			GP "A";	GP &N[12];	GP "A";	GP &N[2];
		GP "T";	GP temp;		GP &N[strlen(temp)+1];		GP "A";	GP &N[12];	GP "A";	GP &N[16];
		GP "T";	GP inf_msg[inf];	GP &N[strlen(inf_msg[inf])];	GP "A";	GP &N[12];	GP "A";	GP &N[32];
		GP "T";	GP "TO ASSIGN THIS FILE TO A DISK FILE, PLEASE SPECIFY:"; GP &N[51];
										GP "A";	GP &N[14];	GP "A";	GP &N[2];
		if (*entry_mode == NATIVE_MODE)
		{
			GP "K";	GP "        ";	GP filename;	GP &N[79];	GP "A";	GP &N[16];	GP "A";	GP &N[2];
			GP "C";

			mode_string = "GENERIC";
		}
		else
		{
			GP "K";	GP "FILE    ";	GP file;	GP &N[8];	GP "A";	GP &N[16];	GP "A";	GP &N[6]; GP "L";
			GP "T";	GP "IN ";	GP &N[3];			GP "A";	GP &N[16];	GP "A";	GP &N[26];
			GP "K";	GP "LIBRARY ";	GP lib;		GP &N[8];	GP "A";	GP &N[16];	GP "A";	GP &N[29]; GP "L";
			GP "T";	GP "ON ";	GP &N[3];			GP "A";	GP &N[16];	GP "A";	GP &N[49];
			GP "K";	GP "VOLUME  ";	GP vol;		GP &N[6];	GP "A";	GP &N[16];	GP "A";	GP &N[52]; GP "L";

			mode_string = "NATIVE ";
		}


		if ((f_mode & IS_PRINTFILE) && (f_mode & IS_OUTPUT))			/* Is this a print file open for output	*/
		{
			GP "T";	GP "PRINTER OPTIONS:";	GP &N[16];		GP "A";	GP &N[20];	GP "A";	GP &N[6];
			GP "K";	GP "PRTCLASS";	GP prtclass;	GP &N[1];	GP "A";	GP &N[20];	GP "A";	GP &N[27]; GP "L";
			GP "K";	GP "FORM#   ";	GP prtform;	GP &N[3];	GP "A";	GP &N[20];	GP "A";	GP &N[43]; GP "I";
			GP "K";	GP "COPIES  ";	GP prtcopies;	GP &N[5];	GP "A";	GP &N[20];	GP "A";	GP &N[61]; GP "I";
		}

		if (intv_type == 'R')
			sprintf(pf_footer,"     (Press PF3 to continue, PF5 for %s entry mode or PF16 to exit.)       ",
					mode_string);
		else
			sprintf(pf_footer,"            (Press PF5 for %s entry mode or PF16 to exit.)                 ",
					mode_string);

		GP "T";	GP pf_footer;		GP &N[79];			GP "A";	GP &N[24];	GP "A";	GP &N[1];
		GP "E";
		GP "P";	GP &pfkey;


		wvaset(&two);
		GETPARM(&args,&cnt);						/* Use the new method			*/


		if (*entry_mode == NATIVE_MODE)
		{
			memcpy(native_path,filename,sizeof(filename));
		}

		if ((*pfkey_rcvr == ENTER_KEY_PRESSED) ||
		    (*pfkey_rcvr == PFKEY_3_PRESSED)   ||
		    (*pfkey_rcvr == PFKEY_16_PRESSED))					/* Do they want out ?		*/
		{
			++done;
			if (*pfkey_rcvr == PFKEY_16_PRESSED)
			{
				setretcode("016");
			}
			continue;
		}
                
		if (*pfkey_rcvr == PFKEY_5_PRESSED)					/* Do they wanna switch modes ?		*/
		{
			*entry_mode = ! *entry_mode;
			continue;
		}
	} while (!done && !type_ID);

	dispchars(file,8);							/* Remove all non-display characters		*/
	dispchars(lib,8);
	dispchars(vol,6);
	dispchars(native_path,sizeof(filename));

	if ((f_mode & IS_PRINTFILE) && (f_mode & IS_OUTPUT))			/* Is this a print file open for output	*/
	{
		sscanf(prtform,"%d",form);
		sscanf(prtcopies,"%d",copies);
	}
}

