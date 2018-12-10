/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/

/* 
	FILE_GETPARM    
	
		This routine will allow operator input of a filename, by one of two methods: 
		WANG native or native mode.
		file_getparm2() was changed to file_getparn3() to replace mode with flags.
 		File_getparm() was changed to file_getparm2() to add the 3 printer paramters.				
*/

#include <stdio.h>
#include <string.h>

#include "idsistd.h"
#include "wcommon.h"
#include "wangkeys.h"
#include "wperson.h"
#include "werrlog.h"
#include "wdefines.h"											
#include "wisplib.h"
#include "vssubs.h"
#include "idsisubs.h"
#include "filgparm.h"

#define WANG_MODE 0
#define NATIVE_MODE !WANG_MODE

#define GP	gp_args[cnt++] = (char *)
#define	GPNUM(num)			GP &N[num]
#define	GPLEN(len)			GPNUM(len)
#define	GPAT(row,col)			GP "A";GPNUM(row);GP "A";GPNUM(col)
#define	GPFLD(fld,len,row,col)		GP fld;GPLEN(len);GPAT(row,col)
#define	GPTEXT(txt,ln,rw,cl)		GP "T";GPFLD(txt,ln,rw,cl)
#define	GPCTEXT(txt,rw,cl)		GPTEXT(txt,(strlen(txt)),rw,cl)
#define	GPKW(x,kw,rcv,ln,rw,cl,typ)	GP x;GP kw;GPFLD(rcv,ln,rw,cl);GP typ
#define	GPID(pn,pf,mn,mi)		GP pn;GP pf;GP mn; GP mi
#define	GPTYP(tp,rq)			GP tp;GP rq
#define	GPTOP(tp,rq,pn,pf,mn,mi,mc)	GPTYP(tp,rq);GPID(pn,pf,mn,mi);GPLEN(mc);
#define	GPSETUP()			init_gpint();gpcnt=0
#define	GPSTD(pn,mi,mc)			GPTOP("I ","R",pn,gppfrcvr,"0001",mi,mc)
#define GPRES(pn,mi,mc)                 GPTOP("R ","R",pn,gppfrcvr,"0001",mi,mc)
#define GPMSG(ln)                       GP ln; GPLEN(strlen(ln))

#define	GPSYSNAME(sn,sr)		GPKW("SYSNAME ",sn,60,sr,3,"C")
#define	GPPFS(x)			GP "P";GP x
#define	GPENTER()			GP "E"
#define	GPNOENTER()			GP "N"

static	int4	N[255];
static	int4	Ni=0;


static void init_N(void)
{
	if (!Ni) 
	{
		int i;
		for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 
		{
			N[i]=(int4)i; 
			WL_wswap(&N[i]);
		}
		++Ni;
	}
}

void WL_file_getparm3(
	char *file, char *lib, char *vol, 	/* Wang style file spec */
	const char *prname, 			/* Prname[8]		*/
	const char *issuer, 			/* Issuer[6]		*/
	int4 *entry_mode, 			/* 0 = Wang style, 1 = Native style filepath */
	char *getparm_type,
	char *native_path,
	char *msg1, char *msg2, 
	char *pfkey_rcvr, 
	char intv_type,					/* 'R' - Rename PF3,  'E' - Everything else */
	char *orig_file, 				/* Original Wang file name */
	char *prtclass, 
	int4 *form, 
	int4 *copies,
	int  is_output,
	int  is_printer,
	int  is_io,
	int  is_shared)
{                                                  
#define		ROUTINE		18000

	int4	pfkey;
	char	*error_msg;
	char	pf_footer[100];
	char	temp[100];
	char	temp2[10];
	char	filename[80];
	int	done, type_ID;
	char	prtform[10], prtcopies[10];
	int	exit_key;
	char	*inf_msg_ptr = "";

	char* gp_args[GETPARM_MAX_ARGS];
	int4	cnt;

	wtrace("FILE_GETPARM","ENTRY","PRNAME=[%8.8s] ISSUER=[%6.6s] TYPE=[%2.2s] STYLE=[%s] [%c]",
	       prname, issuer, getparm_type, ((*entry_mode)?"Native":"Wang"), intv_type);

	*pfkey_rcvr = ENTER_KEY_PRESSED;					/* Default the reciever to '@'			*/
	
	dispchars(file,8);							/* Remove all non-display characters		*/
	dispchars(lib,8);
	dispchars(vol,6);
	dispchars(native_path,sizeof(filename));

	if ((is_printer) && (is_output))				/* Is this a print file open for output	*/
	{
		sprintf(prtform,"%03d",*form);
		sprintf(prtcopies,"%05d",*copies);
	}

	error_msg = "YOU MUST ENTER THE NAME AND LOCATION OF THE FILE.           ";	/* Initialize the  the error msg.	*/

	init_N();

	WL_wpload();									/* Get user personality and defaults.	*/

	if (intv_type == 'R') /* 'E' Error  'R' Rename (open output, file exists) */
		pfkey = PFKEY_3_ENABLED|PFKEY_5_ENABLED|PFKEY_16_ENABLED;
	else
		pfkey = PFKEY_5_ENABLED|PFKEY_16_ENABLED;				/* Only two keys enabled.		*/

	if (WL_pfkeys12())
	{
		pfkey |= PFKEY_12_ENABLED;
		exit_key = 12;
	}
	else
	{
		exit_key = 16;
	}
	
	WL_wswap( &pfkey );								/* Do system dependent swap		*/

	if (is_io)
	{
		inf_msg_ptr = "(TO BE UPDATED (I-O) BY THE PROGRAM)";
	}
	else if (is_shared)
	{
		inf_msg_ptr = "(TO BE UPDATED (SHARED) BY THE PROGRAM)";
	}
	else if (is_output) 
	{
		inf_msg_ptr = "(TO BE CREATED AS OUTPUT BY THE PROGRAM)";
	}
	else 
	{
		inf_msg_ptr = "(TO BE USED AS INPUT BY THE PROGRAM)";
	}

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
		GP "T";	GP inf_msg_ptr;		GP &N[strlen(inf_msg_ptr)];	GP "A";	GP &N[12];	GP "A";	GP &N[32];
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


		if ((is_printer) && (is_output))			/* Is this a print file open for output	*/
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


		GETPARM2(gp_args,cnt);						/* Use the new method			*/


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
				SETRETCODE("016");
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

	if ((is_printer) && (is_output))			/* Is this a print file open for output	*/
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
**	ROUTINE:	WL_password_getparm()
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
int WL_password_getparm(int initial, char* uservalue, int userlen, char* passvalue, int passlen, char* savevalue, char* messtext)
{
	char* gp_args[GETPARM_MAX_ARGS];
	int4	cnt;
	char	pfkey_rcvr[1];
	char	*prname, *issuer, *userkey, *passkey, *savekey, *endmess, *savemess, *savexxx;
	char	*ut, *pt, *gt;
	
	init_N();

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
	
	GETPARM2(gp_args, cnt);

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
**	Routine:	WL_display_util_options_getparm()
**
**	Function:	Issue the OPTIONS getparm and validate.
**
**	Description:	Put up the OPTIONS getparm for DISPLAY and validate all fields.
**			Currently only the RECSIZE field supported.
**
**	Arguments:
**	recsize		The records size (returned)
**
**	Globals:
**	N		The wswaped numbers array.
**
**	Return:		Pfkey number
**	0		Continue
**
**	Warnings:	None
**
*/
int WL_display_util_options_getparm(int *recsize)
{
	char* gp_args[GETPARM_MAX_ARGS];
	int4	cnt;

	char	*gptype;
	char	*messid,*mess1,*mess2,*mess3,*mess4,*blank;
	char	*x_recsize;

	char	recsize_field[3+1];

	init_N();

	gptype = "ID";
	blank=" ";
	mess1=mess2=mess3=mess4=blank;
	
	messid = "0000";

	memcpy(recsize_field,	"0  ",  3);
	recsize_field[3] = '\0';
	
	x_recsize =  "K";

	for(;;)
	{
		int	row;
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		cnt = 0;
		GP gptype; GP "R"; GP "OPTIONS "; GP pfkey_recv; GP messid; GP "DISP  "; GPNUM(4); 
			GP mess1; GPNUM(strlen(mess1));
			GP mess2; GPNUM(strlen(mess2));
			GP mess3; GPNUM(strlen(mess3));
			GP mess4; GPNUM(strlen(mess4));

		row = 12;
		GPKW(x_recsize,"RECSIZE ",recsize_field,3,row,13,"A");
		GPCTEXT("(0=Variable)",row,34);
		

		GPENTER();

		GETPARM2(gp_args,cnt);
	
		x_recsize =  "K";
		gptype = "R ";

		*recsize = 0;
		sscanf(recsize_field,"%d", recsize);
		if (*recsize < 0 || *recsize >256)
		{
			messid = "ER12";
			mess1 = "\224SORRY\204- Invalid record size. (0, 1-256)";
			x_recsize="R";
			continue;
		}

		/*
		**	Passed all the tests
		*/
		break;
	}

	return 0;
}


/*
**	History:
**	$Log: filgparm.c,v $
**	Revision 1.28  2003/02/20 23:14:35  gsl
**	Add OPTIONS get to DISPLAY utility that gets the record size RECSIZE
**	
**	Revision 1.27  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.26  2003/02/17 22:07:18  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.25  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.24  2003/01/31 17:33:56  gsl
**	Fix  copyright header
**	
**	Revision 1.23  2002/07/30 19:12:40  gsl
**	SETRETCODE
**	
**	Revision 1.22  2002/07/29 21:13:26  gsl
**	setretcode -> SETRETCODE
**	
**	Revision 1.21  2002/07/12 17:00:55  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.20  2002/07/11 20:29:08  gsl
**	Fix WL_ globals
**	
**	Revision 1.19  2002/07/10 21:05:16  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.18  2002/07/02 21:15:24  gsl
**	Rename wstrdup
**	
**	Revision 1.17  2002/07/01 04:02:37  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.16  2002/06/26 04:25:04  gsl
**	Cleanup mode/status bit fields
**	
**	Revision 1.15  2002/06/21 03:10:36  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.14  1997/10/20 21:16:46  gsl
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
