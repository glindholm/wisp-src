static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
#include "wisplib.h"
#include "idsisubs.h"
#include "filgparm.h"

#define WANG_MODE 0
#define NATIVE_MODE !WANG_MODE

#define GP	args.ptrs[cnt++] = (char *)

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

void file_getparm2(
		   int4 f_mode, 				/* wfopen mode		*/
		   char file[8], char lib[8], char vol[6], 	/* Wang style file spec */
		   char prname[8], 				/* Prname[8]		*/
		   char issuer[6], 				/* Issuer[6]		*/
		   int4 *entry_mode, 				/* 0 = Wang style, 1 = Native style filepath */
		   char getparm_type[2],
		   char native_path[80],
		   char *msg1, char *msg2, 
		   char *pfkey_rcvr, 
		   char intv_type,				/* 'R' - Rename PF3,  'E' - Everything else */
		   char orig_file[8], 				/* Original Wang file name */
		   char *prtclass, 
		   int4 *form, 
		   int4* copies
		   )
{                                                  
#define		ROUTINE		18000

	int4	pfkey;
	int	i,inf;
	char	*error_msg;
	char	pf_footer[100];
	char	temp[100];
	char	temp2[10];
	char	filename[80];
	int	done, type_ID;
	char	prtform[10], prtcopies[10];
	int	exit_key;

	struct argst { char *ptrs[160]; } args;
	int4	cnt;

	wtrace("FILE_GETPARM","ENTRY","PRNAME=[%8.8s] ISSUER=[%6.6s] TYPE=[%2.2s] STYLE=[%s] [%c]",
	       prname, issuer, getparm_type, ((*entry_mode)?"Native":"Wang"), intv_type);

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

	if (intv_type == 'R') /* 'E' Error  'R' Rename (open output, file exists) */
		pfkey = PFKEY_3_ENABLED|PFKEY_5_ENABLED|PFKEY_16_ENABLED;
	else
		pfkey = PFKEY_5_ENABLED|PFKEY_16_ENABLED;				/* Only two keys enabled.		*/

	if (pfkeys12())
	{
		pfkey |= PFKEY_12_ENABLED;
		exit_key = 12;
	}
	else
	{
		exit_key = 16;
	}
	
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
			sprintf(pf_footer,"     (Press (3) to continue, (5) for %s entry mode or (%d) to exit.)       ",
					mode_string, exit_key);
		else
			sprintf(pf_footer,"            (Press (5) for %s entry mode or (%d) to exit.)                 ",
					mode_string, exit_key);

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
		    (*pfkey_rcvr == PFKEY_12_PRESSED)  ||
		    (*pfkey_rcvr == PFKEY_16_PRESSED))					/* Do they want out ?		*/
		{
			++done;
			if (*pfkey_rcvr == PFKEY_12_PRESSED || *pfkey_rcvr == PFKEY_16_PRESSED)
			{
				*pfkey_rcvr = PFKEY_16_PRESSED;
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

	if (*entry_mode)
	{
		wtrace("FILE_GETPARM","EXIT","NATIVEFILE=[%80.80s] KEY=[%c]",
		       native_path, *pfkey_rcvr);
	}
	else
	{
		wtrace("FILE_GETPARM","EXIT","FILE=[%8.8s] LIB=[%8.8s] VOL=[%6.6s] KEY=[%c]",
		       file, lib, vol, *pfkey_rcvr);
	}
}

/*
**	ROUTINE:	password_getparm()
**
**	FUNCTION:	Issue the PASSWORD and USERNAME GETPARM
**
**	DESCRIPTION:	This is used with SUBMIT on NT/95
**
**	ARGUMENTS:	
**	initial		Is this an initial ("I ") or respecify ("R ") getparm
**	uservalue	The USERNAME value
**	userlen		THe field length to use for USERNAME
**	passvalue	The PASSWORD value
**	passlen		The field length to use for PASSWORD
**	savevalue	The SAVE value (YES or NO)
**	messtext	The message text.
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Terminated with ENTER key (Normal exit)
**	1		Aborted
**
**	WARNINGS:	None
**
*/
int password_getparm(int initial, char* uservalue, int userlen, char* passvalue, int passlen, char* savevalue, char* messtext)
{
	int	i;
	struct argst { char *ptrs[160]; } args;
	int4	cnt;
	char	pfkey_rcvr[1];
	char	*prname, *issuer, *userkey, *passkey, *savekey, *endmess, *savemess, *savexxx;
	char	*ut, *pt, *gt;
	
	if (!Ni) 
	{
		for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 
		{
			N[i]=(int4)i; 
			wswap(&N[i]);
		}
		++Ni;
	}

	*pfkey_rcvr = ENTER_KEY_PRESSED;					/* Default the reciever to '@'			*/

	prname = "PASSWORD";
	issuer = "SUBMIT";
	userkey = "USERNAME";
	passkey = "PASSWORD";
	savekey = "SAVE";
	savemess = "Do you want to save these values for additional SUBMIT commands?";
	savexxx  = "(\"YES\" or \"NO \")";
	endmess  = "Enter the required values and press (ENTER) to continue.";

	gt = (initial) ? "I " : "R ";
	
	ut = (' ' == uservalue[0]) ? "R" : "K";
	pt = (' ' == passvalue[0]) ? "R" : "K";

	cnt = 0;
	GP gt;   GP "R"; GP prname; GP pfkey_rcvr; GP "0001"; GP issuer; GP &N[1]; GP messtext; GP &N[strlen(messtext)];
	GP pt;   GP passkey; GP passvalue; GP &N[passlen]; GP "A"; GP &N[11]; GP "A"; GP &N[10]; GP "B";
	GP ut;   GP userkey; GP uservalue; GP &N[userlen]; GP "A"; GP &N[13]; GP "A"; GP &N[10]; GP "C";
	GP "T";  GP savemess; GP &N[strlen(savemess)];     GP "A"; GP &N[17]; GP "A"; GP &N[10];
	GP "K";  GP savekey; GP savevalue; GP &N[3];       GP "A"; GP &N[19]; GP "A"; GP &N[10]; GP "A";
	GP "T";  GP savexxx;  GP &N[strlen(savexxx)];      GP "A"; GP &N[19]; GP "A"; GP &N[30];
	GP "T";  GP endmess;  GP &N[strlen(endmess)];      GP "A"; GP &N[24]; GP "A"; GP &N[(80-strlen(endmess))/2];
	GP "E";
	
	wvaset(&two);
	GETPARM(&args,&cnt);

	/*
	**	This getparm can only end with the ENTER key 
	**	(or the HELP key if the command processor is already active).
	*/
	if (ENTER_KEY_PRESSED == pfkey_rcvr[0])
	{
		return 0;	/* ENTER key */
	}
	else
	{
		return 1;	/* HELP key */
	}
}


/*
**	History:
**	$Log: filgparm.c,v $
**	Revision 1.14  1997-10-20 17:16:46-04  gsl
**	Add tracing
**
**	Revision 1.13  1997-09-24 17:12:02-04  gsl
**	Add support for pfkeys12()
**
**	Revision 1.12  1997-08-22 17:38:37-04  gsl
**	Finish the PASSWORD GETPARM
**
**	Revision 1.11  1996-08-19 18:32:20-04  gsl
**	drcs update
**
**
**
*/
