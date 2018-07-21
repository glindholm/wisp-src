/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/


/*
**	File:		wcopy.c
**
**	Purpose:	To hold WCOPY utility source file.
**
**	Routines:	
**	main()		The WCOPY main routine.
**
**	History:
**	03/01/93	Changed to use GETPARMS like the Wang COPY utility. GSL
**	03/05/93	Fix to also work on VMS. GSL
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef unix
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "filext.h"
#include "wcommon.h"
#include "wperson.h"
#include "wangkeys.h"
#include "wfname.h"
#include "wexit.h"
#include "wisplib.h"
#include "level.h"
#include "idsisubs.h"
#include "vwang.h"
#include "vssubs.h"

static int vscopy();
static int get_options();
static int get_failed();
static int library_copy();
static int get_input();
static int get_output();
static int get_eoj();
static int get_dummy();


#ifdef unix
static void badusage();
static void copy();
#endif

static	char 	*COPY_VERSION = "WISP Copy Program - Version 1.01.00";
static	int4	N[255];

#define GP	gp_args[gp_cnt++] = (char *)
static char* gp_args[GETPARM_MAX_ARGS];
static int gp_cnt;

static char *terminate_text;
static int4 terminate_key_enabled;
static char terminate_key_pressed;
static int  terminate_key;
static char terminate_buff[80];

/*
**	Routine:	main()
**
**	Function:	Main routine for WCOPY utility
**
**	Description:	This routine emulates the Wang COPY utility when no args are passed.
**			If arguments are passed it uses the earlier command line syntax. (unix only)
**
**				wcopy
**				wcopy oldfile oldlib oldvol newfile newlib newvol
**				wcopy LIBRARY oldlib oldvol newlib newvol
**
**	Arguments:	If argc==1 then use GETPARM interface.
**			If argc==7 then use old command line file copy. 	(unix only)
**			If argc==6 then use old command line library copy. 	(unix only)
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	03/01/93	Add support for GETPARM. GSL
**
*/
int main(argc,argv)
int	argc;
char	*argv[];
{
#ifdef unix
	int	retcode;
	char	key[20], type[1];
	char	oldfile[20], oldlib[20], oldvol[20];
	char	newfile[20], newlib[20], newvol[20];
#endif

        /*********** PATCH FOR ALPHA/OPENVMS *************/
        /* patch needed since argc is allways 2 on alpha */
        /* , even if no parms passed to main.  So check  */
        /* if argc = 2 and argv[1] = null, set argc to 1.*/
        if (2 == argc && !argv[1])
        {
           argc=1;
        }
        /*************************************************/
	vwang_title("WISP COPY");
	WL_initglbs("WCOPY   ");

	if (WL_pfkeys12())
	{
		terminate_text = "Press (12) to end COPY processing";
		terminate_key_enabled = PFKEY_12_ENABLED | PFKEY_16_ENABLED;
		terminate_key_pressed = PFKEY_12_PRESSED;
		terminate_key = 12;
	}
	else
	{
		terminate_text = "Press (16) to end COPY processing";
		terminate_key_enabled = PFKEY_16_ENABLED;
		terminate_key_pressed = PFKEY_16_PRESSED;
		terminate_key = 16;
	}


#ifndef unix

	WL_wexit(vscopy());

#else /* unix */
	if (argc == 1)
	{
		WL_wexit(vscopy());
	}
	if (argc != 1 && argc != 6 && argc != 7) badusage();

	strcpy( key, argv[1] );
	WL_upper_string( key );

	if ( 0 == strcmp( "LIBRARY", key ) )
	{
		if (argc != 6) badusage();
		strcpy(oldfile, "        ");
		strcpy(oldlib,  argv[2]);
		strcpy(oldvol,  argv[3]);
		strcpy(newfile, "        ");
		strcpy(newlib,  argv[4]);
		strcpy(newvol,  argv[5]);
		type[0] = 'L';
	}
	else
	{
		if (argc != 7) badusage();
		strcpy(oldfile, argv[1]);
		strcpy(oldlib,  argv[2]);
		strcpy(oldvol,  argv[3]);
		strcpy(newfile, argv[4]);
		strcpy(newlib,  argv[5]);
		strcpy(newvol,  argv[6]);
		type[0] = 'F';
	}
	 
	if ( strlen(oldfile)>8 ||
	     strlen(oldlib)>8  ||
	     strlen(oldvol)>6  ||
	     strlen(newvol)>6  ||
	     strlen(newfile)>8 ||
	     strlen(newlib)>8   ) badusage();
 
	retcode = 0;


	copy(type, oldfile, oldlib, oldvol,
                   newfile, newlib, newvol, &retcode);

	exit(retcode);
#endif /* unix */
	return 0;
}

#ifdef unix
static void copy(type,oldfile,oldlib,oldvol,newfile,newlib,newvol,status)
char	*type;
char	*oldfile, *oldlib, *oldvol, *newfile, *newlib, *newvol;
int	*status;
{
	char old_filename[132], new_filename[132];					/* Strings to contain the filenames.	*/
	char libpath[80];
	int4 mode;
	char *name_end;
	char cmd[100];									/* buffer to hold cmd string 		*/
	int found, has_ext;

	*status = 0;

	if ( *type == 'L' ) 								/* Doing a library copy.		*/
	{
		name_end = WL_wanglib2path(oldvol, oldlib, old_filename);			/* Construct the old filename.		*/
		*name_end = '\0';
	}
	else
	{
		mode = 0;
		name_end = WL_wfname(&mode, oldvol, oldlib, oldfile, old_filename);	/* Construct the old filename.		*/
		*name_end = '\0';
	}
	if ( *(--name_end) == '/' ) *name_end = '\0';					/* separator not needed			*/

	
	if ( *type == 'L' ) 								/* Doing a library copy.		*/
	{
		name_end = WL_wanglib2path(newvol, newlib, new_filename);			/* Construct the new filename.		*/
		*name_end = '\0';
	}
	else
	{
		mode = 0;
		name_end = WL_wfname(&mode, newvol, newlib, newfile, new_filename);	/* Construct the new filename.		*/
		*name_end = '\0';
	}
	if ( *(--name_end) == '/' ) *name_end = '\0';					/* separator not needed			*/

	strcpy(libpath,new_filename);
	if ( *type != 'L')
	{
		int x;

		for( x=strlen(libpath)-1; x>=0 && libpath[x]!='/'; x--);
		if (x>=0) libpath[x] = '\0'; 
		else      libpath[0] = '\0';		
	}



	if ( !WL_fexists(libpath) )							/* If new_lib doesn't exist.		*/
	{
		if(mkdir(libpath,0777))							/* Create the new lib.			*/
		{
			*status=24;
			return;
		}
	}

	if ( *type == 'L' )
	{
		sprintf(cmd,"cp %s/* %s >/dev/null 2>&1",old_filename,new_filename);
		if (system(cmd)) 
		{
			*status=20;
		}
		return;
	}

	found = 0;
	has_ext = hasext(old_filename);

	if (WL_fexists(old_filename))							/* file does exist in this form	*/
	{
		if (WL_fexists(new_filename))						/* New file already exists.		*/
		{
			*status = 52;
			return;
		}

		sprintf(cmd,"cp %s %s >/dev/null 2>&1",old_filename,new_filename);
		if (system(cmd)) 
		{
			*status=24;
			return;
		}
		found = 1;
	}

	if (!found && has_ext)								/* does it already have extension?	*/
	{
		*status=20;								/* yes, so return file not found	*/
		return;
	}

	strcat(old_filename,".idx");							/* else try it with a .idx extension	*/
	strcat(new_filename,".idx");
	if (WL_fexists(old_filename))							/* idx found				*/
	{

		if (WL_fexists(new_filename))						/* Does new file already exists?	*/
		{
			*status = 52;
			return;
		}

		sprintf(cmd,"cp %s %s >/dev/null 2>&1",old_filename,new_filename);	/* build the copy cmd, direct all output*/
		if (system(cmd))							/* to /dev/null.  if system() returns	*/
		{									/* non-zero, gen an error		*/
			*status=24;
			return;
		}
		found = 1;
	}

	strcpy(strrchr(old_filename,'.'),".dat");					/* replace the '.idx' with a '.dat'	*/
	strcpy(strrchr(new_filename,'.'),".dat");
	if (WL_fexists(old_filename))							/* dat found				*/
	{
		if (WL_fexists(new_filename))						/* Does new file already exists?	*/
		{
			*status = 52;
			return;
		}

		sprintf(cmd,"cp %s %s >/dev/null 2>&1",old_filename,new_filename);
		if (system(cmd))
		{
			*status=48;							/* Serious error if only half worked.	*/
			return;
		}
		found = 1;
	}

	if (!found)
	{
		*status = 20;
		return;
	}

}
#endif /* unix */

#ifdef unix
static void badusage()
{
	printf("\n");
	printf("Usage: wcopy\n");
	printf("       wcopy         oldfile oldlib oldvol newfile newlib newvol\n");
	printf("       wcopy LIBRARY         oldlib oldvol         newlib newvol\n");
	printf("\n");
	exit(0);
}
#endif

/*
**	Routine:	vscopy()
**
**	Function:	To provide a GETPARM interface like the Wang COPY utility.
**
**	Description:	To emulate the Wang COPY utility.
**
**	Arguments:	None
**
**	Globals:
**	N		The wswaped numbers array.
**
**	Return:
**	0		success
**
**	Warnings:	None
**
**	History:	
**	03/01/93	Written by GSL
**
*/
static int vscopy()
{
	char	ifile[9], ilib[9], ivol[7];
	char	ofile[9], olib[9], ovol[7];
	char	copy[8];
	char	message[80];
	char	*messid;
	int	pfkey;
	int4	i;
	int4	retcode;

	for (i=0; i<(sizeof(N)/sizeof(N[0])); ++i) 				/* Initialize a swapped numbers array		*/
	{
		N[i]=i; 
		WL_wswap(&N[i]);
	}

	memcpy(copy,"FILE   ",7);

	for(;;)
	{
		pfkey = get_input(ifile,ilib,ivol,copy);			/* Getparm INPUT info				*/
		if (pfkey == 16) break;

		get_dummy();							/* Getparm misc dummy info.			*/

		pfkey = get_output(ofile,olib,ovol,ifile,ilib,ivol,copy);	/* Getparm OUTPUT info				*/
		if (pfkey != 16)
		{
			if (copy[0] == 'F')					/* Do a file copy				*/
			{
				WL_set_va_count(7);
				FILECOPY(ifile,ilib,ivol,ofile,olib,ovol,&retcode);
				WL_wswap(&retcode);
				if (retcode == 0)
				{
					sprintf(message,"File %8.8s in Library %8.8s on Volume %6.6s was created",ofile,olib,ovol);
					messid = "0028";
				}
				else
				{
					sprintf(message,"COPY failed return code = %04d",retcode);
					messid = "F001";
				}
			}
			else
			{
				int	copycnt;

				copycnt = library_copy(ilib,ivol,olib,ovol);	/* Do the library copy				*/

				sprintf(message,"%04d Files have been created in Library %8.8s on Volume %6.6s",copycnt,olib,ovol);
				messid = "0029";
			}
		}
		else
		{
			strcpy(message,"No file copied");
			messid = "0045";
		}

		pfkey = get_eoj(message, messid);				/* Getparm EOJ info				*/
		if (pfkey != 1)							/* If not PF1 then exit				*/
		{
			break;
		}
	}

	return(0);
}

/*
**	Routine:	library_copy()
**
**	Function:	To copy a library one file at a time.
**
**	Description:	This routine copies a whole library one file at a time.
**			If the file exists in the target library it issues a getparm
**			to let user decide how to handle conflict.
**
**	Arguments:
**	ilib		Input library
**	ivol		Input volume
**	olib		Output library
**	ovol		Output volume
**
**	Globals:	None
**
**	Return:		Number of files copied.
**
**	Warnings:	None
**
**	History:	
**	03/03/93	Written by GSL
**
*/
static int library_copy(ilib,ivol,olib,ovol)
char	*ilib, *ivol, *olib, *ovol;
{
	int	copycnt;
	int	remaining;
	int4	start;
	int4	count;
	int4	retcode;
	char	reciever[2200];
	char	ifile[9], ofile[9];
	char	*fileptr = NULL;
	int	pfkey;

#define RSIZE	100

	start = 1;

	copycnt = 0;
	remaining = 0;

	for(;;)
	{
		if (remaining < 1)
		{
			count = RSIZE;
			WL_wswap(&count);
			WL_wswap(&start);
			WL_set_va_count(6);
			FIND("?       ",ilib,ivol,&start,&count,reciever);
			WL_wswap(&count);
			if (count == 0) break;					/* Nothing found so break out			*/

			remaining = count;					/* How many were returned			*/

			WL_wswap(&start);						/* Setup start for next time			*/
			start += RSIZE;

			fileptr = reciever + 14;				/* Point to the first file in reciever		*/
		}
		else
		{
			fileptr += 22;						/* Point to next file				*/
		}

		memcpy(ifile,fileptr,8);					/* Load the file name				*/
		memcpy(ofile,ifile,8);
		remaining -= 1;							/* Using up one of the remaining		*/

		if (WL_wfexists(ifile,ilib,ivol))					/* Check if Input file exists			*/
		{
			int	copyit;

			copyit = 0;
			if (WL_wfexists(ofile,olib,ovol))				/* Check if Output file exists			*/
			{
				char	option[2];

				option[0] = 'N';
				pfkey = get_options(ifile,ilib,ivol,ofile,olib,ovol,option);
				if (pfkey == 16) break;				/* Exit						*/

				if (option[0] != 'N')
				{
					copyit = 1;
				}
			}
			else
			{
				copyit = 1;
			}

			if (copyit)						/* Do the copy					*/
			{
				WL_set_va_count(7);
				FILECOPY(ifile,ilib,ivol,ofile,olib,ovol,&retcode);
				WL_wswap(&retcode);
				if (retcode == 0)
				{
					copycnt += 1;				/* Copy succeeded				*/
				}
				else						/* FILECOPY failed				*/
				{
					pfkey = get_failed(ifile,ilib,ivol,ofile,olib,ovol,retcode);
					if (pfkey == 16) break;			/* Exit						*/
				}
			}
		}
	}
	return copycnt;
}

/*
**	Routine:	get_input()
**
**	Function:	To get the input file specs for COPY.
**
**	Description:	Put up the INPUT getparm for COPY and validate all fields.
**
**	Arguments:
**	ifile		The input file 		(returned)	
**	ilib		The input library 	(returned)
**	ivol		The input volume 	(returned)
**	copy		The copy option		(returned)
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	03/02/93	Written by GSL
**
*/
static int get_input(ifile,ilib,ivol,copy)
char *ifile, *ilib, *ivol, *copy;
{
	int4	pfkey_mask;
	char	*gptype;
	char	*message;
	char	*messid;
	char	*xf,*xl,*xv,*xc;
	int	done;

	xf = xl = xv = xc = "K";

	memset(ifile,' ',8);
	WL_get_defs(DEFAULTS_IL,ilib);
	WL_get_defs(DEFAULTS_IV,ivol);

	pfkey_mask = terminate_key_enabled;
	WL_wswap(&pfkey_mask);
	gptype = "I ";
	message = COPY_VERSION;
	messid = "0001";

	done = 0;

	for(;;)
	{
		int4	len;
		char	pfkey_recv[1];
		char	copyx[8];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "INPUT   "; GP pfkey_recv; GP messid; GP "WCOPY "; GP &N[1]; 
			GP message; GP &N[strlen(message)];
		GP "U"; GP "Specify the file, library, and volume information:X"; GP &N[50]; GP "A"; GP &N[10]; GP "A"; GP &N[3];
		GP xf;  GP "FILE    ";	GP ifile;	GP &N[8];	GP "A";	GP &N[12];	GP "A"; GP &N[6]; GP "L";
		GP "T";	GP "in";	GP &N[2];			        GP &N[0];	GP &N[1];
		GP xl;  GP "LIBRARY ";	GP ilib;	GP &N[8];	        GP &N[0];	GP &N[1]; GP "L";
		GP "T";	GP "on";	GP &N[2];			        GP &N[0];	GP &N[1];
		GP xv;  GP "VOLUME  ";	GP ivol;	GP &N[6];	        GP &N[0];	GP &N[1]; GP "L";
		GP "U"; GP "Specify the desired copy option:X"; GP &N[32]; GP "A"; GP &N[14]; GP "A"; GP &N[3];
		GP xc;  GP "COPY    ";	GP copy;	GP &N[7];	   GP "A"; GP &N[16]; GP "A"; GP &N[6]; GP "L";
		GP "T"; GP "(Options = FILE, or LIBRARY)X"; GP &N[28];     GP "A"; GP &N[16]; GP "A"; GP &N[33];
		GP "T"; GP terminate_text; GP &N[33]; GP "A"; GP &N[22]; GP "C"; GP &N[0]; 
		GP "E";
		GP "P";	GP &pfkey_mask;

		GETPARM2(gp_args,gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		if (done) return 0;

		gptype = "R ";
		xf = xl = xv = xc = "K";

		leftjust(copy,7);
		unloadpad(copyx,copy,7);
		len = strlen(copyx);
		if ( len == 0 || (0 != memcmp(copyx,"LIBRARY",len) && 0 != memcmp(copyx,"FILE   ",len)) )
		{
			messid = "0007";
			message = "COPY option must be FILE, or LIBRARY";
			xc = "R";
			continue;
		}

		leftjust(ivol,6);
		if (' '==ivol[0])
		{
			messid = "0004";
			message = "Specify VOLUME name";
			xv = "R";
			continue;
		}

		leftjust(ilib,8);
		if (' '==ilib[0])
		{
			messid = "0005";
			message = "Specify LIBRARY name";
			xl = "R";
			continue;
		}

		if (copy[0] == 'F')
		{
			leftjust(ifile,8);
			if (' '==ifile[0])
			{
				messid = "0006";
				message = "Specify FILE name";
				xf = "R";
				continue;
			}
		}

		/*
		**	Check if voloume and library exists
		*/
		{
			int4	start, count;
			char	recvr[22];
			char	buff[80];

			if (0==WL_wlgtrans(ivol,buff))
			{
				messid = "0010";
				message = "VOLUME not found, please respecify";
				xv = "R";
				continue;
			}
			start = 1;
			WL_wswap(&start);
			count = 1;
			WL_wswap(&count);

			WL_set_va_count(6);
			FIND("        ", ilib, ivol, &start, &count, recvr);
			WL_wswap(&count);
			if (count == 0)
			{
				messid = "0011";
				message = "LIBRARY not found, please respecify";
				xl = "R";
				continue;
			}
		}

		if (copy[0] == 'L')						/* Library was found				*/
		{
			/*
			**	Update the PUTPARM with updated file info.
			**	The WL_use_last_prb() is an internal "hint" to GETPARM
			**	to use the last PRB.  This will prevent GETPARM from
			**	possibly updating the wrong "INPUT" putparm if
			**	there are multiple.
			**
			**	GETPARM does not fully correctly handle link-levels
			**	and PUTPARM chaining so this scenerio can happen:
			**	If the original "INPUT" putparm is not labeled then
			**	it will be deleted once used (instead of just being
			**	marked as used).  If there is another INPUT putparm
			**	even at a lower link-level it will be found by the
			**	RD GETPARM and incorrectly updated.  The call to 
			**	WL_use_last_prb() tell GETPARM to just update the last
			**	PUTPARM if it still exists instead of going looking
			**	for a matching PUTPARM.
			*/	
			WL_use_last_prb();					/* back fill the getparm			*/
			gptype = "RD";
			done = 1;
			continue;
		}
		
		if (WL_wfexists(ifile,ilib,ivol)) 					/* File exists so return			*/
		{
			WL_use_last_prb();					/* back fill the getparm			*/
			gptype = "RD";	
			done = 1;
			continue;
		}

		messid = "R014";
		message = "FILE not found, please respecify";
		xf = "R";
	}
}

/*
**	Routine:	get_output()
**
**	Function:	To get the output file specs for COPY.
**
**	Description:	Put up the OUTPUT getparm for COPY and validate all fields.
**
**	Arguments:
**	ofile		The output file 	(returned)	
**	olib		The output library 	(returned)
**	ovol		The output volume 	(returned)
**	ifile		The input file
**	ilib		The input library
**	ivol		The input volume
**	copy		The copy option
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	03/03/93	Written by GSL
**
*/
static int get_output(ofile,olib,ovol,ifile,ilib,ivol,copy)
char *ofile, *olib, *ovol;
char *ifile, *ilib, *ivol, *copy;
{
	int4	pfkey_mask;
	char	*gptype;
	char	*message;
	char	*messid;
	char	*xf,*xl,*xv;
	int4	retcode;
	char	buff[80];

	xf = xl = xv = "K";

	memset(ofile,' ',8);
	WL_get_defs(DEFAULTS_OL,olib);
	WL_get_defs(DEFAULTS_OV,ovol);

	pfkey_mask = terminate_key_enabled;
	WL_wswap(&pfkey_mask);
	gptype = "I ";
	message = COPY_VERSION;

	if (copy[0] == 'F')	messid = "OUT1";
	else			messid = "0003";

	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		if (copy[0] == 'F')
		{
			sprintf(terminate_buff,"    (%d)     Exit                     ",terminate_key);
			
			gp_cnt = 0;
			GP gptype; GP "R"; GP "OUTPUT  "; GP pfkey_recv; GP messid; GP "WCOPY "; GP &N[1]; 
				GP message; GP &N[strlen(message)];
			GP "U"; GP "Please enter the parameters necessary to define the output file:X"; GP &N[64]; 
				GP "A"; GP &N[9]; GP "C"; GP &N[0];
			GP xf;  GP "FILE    ";	GP ofile;	GP &N[8];	GP "A";	GP &N[11];	GP "A"; GP &N[6]; GP "L";
			GP "T";	GP "in";	GP &N[2];			        GP &N[0];	GP &N[1];
			GP xl;  GP "LIBRARY ";	GP olib;	GP &N[8];	        GP &N[0];	GP &N[1]; GP "L";
			GP "T";	GP "on";	GP &N[2];			        GP &N[0];	GP &N[1];
			GP xv;  GP "VOLUME  ";	GP ovol;	GP &N[6];	        GP &N[0];	GP &N[1]; GP "L";
			GP "U"; GP "Press:"; GP &N[6]; 					GP "A"; GP &N[20]; GP "A"; GP &N[6];
			GP "T"; GP "   (ENTER)   To create the output file"; GP &N[38]; GP "A"; GP &N[21]; GP "A"; GP &N[6];
			GP "T"; GP terminate_buff; GP &N[38]; GP "A"; GP &N[22]; GP "A"; GP &N[6];
			GP "E";
			GP "P";	GP &pfkey_mask;
		}
		else
		{
			gp_cnt = 0;
			GP gptype; GP "R"; GP "OUTPUT  "; GP pfkey_recv; GP messid; GP "WCOPY "; GP &N[1]; 
				GP message; GP &N[strlen(message)];
			GP "U"; GP "Specify the Output Library and Volume names and press (ENTER):X"; GP &N[62]; 
				GP "A"; GP &N[11]; GP "C"; GP &N[0];
			GP xl;  GP "LIBRARY ";	GP olib;	GP &N[8];	GP "A";	GP &N[13];	GP "A"; GP &N[16]; GP "L";
			GP "T";	GP "on";	GP &N[2];			        GP &N[0];	GP &N[1];
			GP xv;  GP "VOLUME  ";	GP ovol;	GP &N[6];	        GP &N[0];	GP &N[1]; GP "L";
			GP "T"; GP terminate_text; GP &N[33]; GP "A"; GP &N[22]; GP "C"; GP &N[0]; 
			GP "E";
			GP "P";	GP &pfkey_mask;
		}

		GETPARM2(gp_args,gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		gptype = "R ";
		xf = xl = xv = "K";
		pfkey_mask = terminate_key_enabled;
		WL_wswap(&pfkey_mask);

		leftjust(ovol,6);
		if (' '==ovol[0])
		{
			messid = "0004";
			message = "Specify VOLUME name";
			xv = "R";
			continue;
		}

		leftjust(olib,8);
		if (' '==olib[0])
		{
			messid = "0005";
			message = "Specify LIBRARY name";
			xl = "R";
			continue;
		}

		if (copy[0] == 'F')
		{
			leftjust(ofile,8);
			if (' '==ofile[0])
			{
				messid = "0006";
				message = "Specify FILE name";
				xf = "R";
				continue;
			}
		}

		/*
		**	Check if volume exists
		*/
		{
			char	buff[80];

			if (0==WL_wlgtrans(ovol,buff))
			{
				messid = "0010";
				message = "VOLUME not found, please respecify";
				xv = "R";
				continue;
			}
		}

		if ( 0==memcmp(ivol,ovol,6) && 0==memcmp(ilib,olib,8) )
		{
			if (copy[0] == 'L' || 0==memcmp(ifile,ofile,8))
			{
				messid = "0027";
				message = "Output parameters are same as Input, Respecify";
				continue;
			}
		}

		if (copy[0] == 'F')
		{
			if (WL_wfexists(ofile,olib,ovol))				/* Check if file exists				*/
			{
				if (pfkey_recv[0] == PFKEY_3_PRESSED) 		/* If second time thru & PF3 was pressed	*/
				{
					WL_set_va_count(5);
					SCRATCH("F",ofile,olib,ovol,&retcode);	/* Scratch the target file			*/
					WL_wswap(&retcode);
					if (retcode == 0) return 0;		/* Scratch suceeded.				*/

					messid = "F002";			/* Scratch failed				*/
					sprintf(buff,"\204Scratch failed return code = %04d",retcode);
					message = buff;
					xf = "R";
					continue;
				}

				messid = "A004";
				message = "\204File already exists.  Respecify or press (3) to scratch it.";
				xf = "R";
				pfkey_mask = PFKEY_3_ENABLED | terminate_key_enabled;
				WL_wswap(&pfkey_mask);
				continue;
			}
		}

		return 0;
	}
}

/*
**	Routine:	get_eoj()
**
**	Function:	To
**
**	Description:	Put up the EOJ getparm for COPY
**
**	Arguments:
**	message		The EOJ message.
**	messid		The EOJ message id.
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0,16		Exit
**	1		Copy another
**
**	Warnings:	None
**
**	History:	
**	03/03/93	Written by GSL
**
*/
static int get_eoj(message,messid)
char *message;
char *messid;
{
	int4	pfkey_mask;
	char	*gptype;
	char	pfkey_recv[1];

	pfkey_mask = PFKEY_1_ENABLED | terminate_key_enabled;
	WL_wswap(&pfkey_mask);
	gptype = "I ";

	pfkey_recv[0] = '@';

	sprintf(terminate_buff,"Select (ENTER) or (%d) to End   the program ",terminate_key);

	gp_cnt = 0;
	GP gptype; GP "R"; GP "EOJ     "; GP pfkey_recv; GP messid; GP "WCOPY "; GP &N[1]; 
		GP message; GP &N[strlen(message)];
	GP "T"; GP terminate_buff; GP &N[44]; GP "A"; GP &N[9]; GP "A"; GP &N[2];
	GP "T"; GP "               or (1)  to rerun the program."; GP &N[44]; GP "A"; GP &N[10]; GP "A"; GP &N[2];
	GP "E";
	GP "P";	GP &pfkey_mask;

	GETPARM2(gp_args,gp_cnt);

	if (pfkey_recv[0] == PFKEY_16_PRESSED || 
	    pfkey_recv[0] == terminate_key_pressed) 
	{
		return 16;
	}
	if (pfkey_recv[0] == PFKEY_1_PRESSED) return 1;
	return 0;
}

/*
**	Routine:	get_dummy()
**
**	Function:	To issue dummy getparms not used in WCOPY.
**
**	Description:	There are hidden getparms used on Wang COPY that we don't care about, however
**			we want any putparms cleaned up so we issue hidden dummy getparms to handle.
**
**	Arguments:	None
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		None.
**
**	Warnings:	None
**
**	History:	
**	03/03/93	Written by GSL
**
*/
static int get_dummy()
{
	char	pfkey_recv[1];
	char	*message;

	pfkey_recv[0] = '@';
	message = "NOT IMPLEMENTED";

	gp_cnt = 0;
	GP "ID"; GP "R"; GP "OPTIONS  "; GP pfkey_recv; GP "DUM1"; GP "WCOPY "; GP &N[1]; 
		GP message; GP &N[strlen(message)];
	GP "E";

	GETPARM2(gp_args,gp_cnt);

	gp_cnt = 0;
	GP "ID"; GP "R"; GP "LOCK    "; GP pfkey_recv; GP "DUM2"; GP "WCOPY "; GP &N[1]; 
		GP message; GP &N[strlen(message)];
	GP "E";

	GETPARM2(gp_args,gp_cnt);

	return 0;
}

/*
**	Routine:	get_options()
**
**	Function:	To get the output OPTIONS for COPY.
**
**	Description:	Put up the OPTIONS getparm for COPY and validate all fields.
**			It also does any possible Rename or Scratch.
**
**	Arguments:
**	ifile		The input file
**	ilib		The input library
**	ivol		The input volume
**	ofile		The output file 	(returned)	
**	olib		The output library 
**	ovol		The output volume 
**	option		The option		(returned)
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	03/04/93	Written by GSL
**
*/

static int get_options(ifile,ilib,ivol,ofile,olib,ovol,option)
char *ifile, *ilib, *ivol;
char *ofile, *olib, *ovol;
char *option;
{
	int4	pfkey_mask;
	char	*gptype;
	char	*message;
	char	*messid;
	char	*xo, *xn;
	int4	retcode;
	char	buff[80];
	char	newname[9];

	xn = xo = "K";

	pfkey_mask = terminate_key_enabled;
	WL_wswap(&pfkey_mask);
	gptype = "I ";
	message = COPY_VERSION;
	messid = "0018";

	memset(newname,' ',8);

	for(;;)
	{
		char	pfkey_recv[1];

		pfkey_recv[0] = '@';

		gp_cnt = 0;
		GP gptype; GP "R"; GP "OPTIONS "; GP pfkey_recv; GP messid; GP "WCOPY "; GP &N[1]; 
				GP message; GP &N[strlen(message)];

		GP "T"; GP "File"; GP &N[4]; GP "A"; GP &N[10]; GP "A"; GP &N[2];
		GP "T"; GP ifile; GP &N[8]; 					GP &N[0]; GP &N[0];
		GP "T"; GP "is listed in both the input Library"; GP &N[35];	GP &N[0]; GP &N[0];
		GP "T"; GP ilib; GP &N[8]; 					GP &N[0]; GP &N[0];
		GP "T"; GP "on Volume"; GP &N[9]; 				GP &N[0]; GP &N[0];
		GP "T"; GP ivol; GP &N[6]; 					GP &N[0]; GP &N[0];
		GP "T"; GP "and the output Library"; GP &N[22]; GP "A"; GP &N[11]; GP "A"; GP &N[29];
		GP "T"; GP olib; GP &N[8]; 					GP &N[0]; GP &N[0];
		GP "T"; GP "on Volume"; GP &N[9]; 				GP &N[0]; GP &N[0];
		GP "T"; GP ovol; GP &N[6]; 					GP &N[0]; GP &N[0];

		GP "U"; GP "Specify an option to resolve the file conflict:"; GP &N[47]; GP "A"; GP &N[13]; GP "A"; GP &N[2];

		GP xo;  GP "OPTION  ";	GP option;	GP &N[1];	GP "A";	GP &N[15];	GP "A"; GP &N[6]; GP "L";

		GP "T"; GP "N - No copy will take place for this file  "; GP &N[43]; GP "A"; GP &N[15]; GP "A"; GP &N[30];
		GP "T"; GP "S - Scratch the output file before copying "; GP &N[43]; GP "A"; GP &N[16]; GP "A"; GP &N[30];
		GP "T"; GP "R - Rename the file that is already in the "; GP &N[43]; GP "A"; GP &N[17]; GP "A"; GP &N[30];
		GP "T"; GP "    output library                         "; GP &N[43]; GP "A"; GP &N[18]; GP "A"; GP &N[30];
		GP "T"; GP "C - Copy the file using a new file name    "; GP &N[43]; GP "A"; GP &N[19]; GP "A"; GP &N[30];

		GP xn;  GP "NEWNAME";	GP newname;	GP &N[8];	GP "A";	GP &N[21];	GP "A"; GP &N[6]; GP "L";

		GP "T"; GP "(for option R and C)"; GP &N[20]; GP "A"; GP &N[21]; GP "A"; GP &N[30];

		GP "T"; GP terminate_text; GP &N[33]; GP "A"; GP &N[24]; GP "C"; GP &N[0]; 
		GP "E";
		GP "P";	GP &pfkey_mask;


		GETPARM2(gp_args,gp_cnt);

		if (pfkey_recv[0] == PFKEY_16_PRESSED || 
		    pfkey_recv[0] == terminate_key_pressed) 
		{
			return 16;
		}

		gptype = "R ";
		xo = xn = "K";
		pfkey_mask = terminate_key_enabled;
		WL_wswap(&pfkey_mask);

		/*
		**	Validate the OPTION field
		*/

		switch (option[0])
		{
		case 'N':
			return 0;
		case 'S':
		case 'R':
		case 'C':
			break;
		default:
			messid = "0019";
			message = "Invalid option. Please respecify";
			xo = "R";
			continue;
		}

		if (option[0] == 'S')						/* Scratch the output file first		*/
		{
			WL_set_va_count(5);
			SCRATCH("F",ofile,olib,ovol,&retcode);			/* Scratch the target file			*/
			WL_wswap(&retcode);
			if (retcode == 0) return 0;				/* Scratch suceeded.				*/

			messid = "F002";					/* Scratch failed				*/
			sprintf(buff,"\204Scratch failed return code = %04d",retcode);
			message = buff;
			xo = "R";
			continue;
		}

		leftjust(newname,8);
		if (' '==newname[0])
		{
			messid = "0024";
			message = "A new file name must be specified";
			xn = "R";
			continue;
		}

		if (WL_wfexists(newname,olib,ovol))
		{
			messid = "0022";
			message = "The new file name specified already exists";
			xn = "R";
			continue;
		}

		if (option[0] == 'R')
		{
			WL_set_va_count(6);
			RENAME("F",ofile,olib,ovol,newname,&retcode);
			WL_wswap(&retcode);
			if (retcode == 0) return 0;				/* Rename suceeded.				*/

			messid = "F003";					/* Rename failed				*/
			sprintf(buff,"\204Rename failed return code = %04d",retcode);
			message = buff;
			xo = "R";
			continue;
		}

		memcpy(ofile,newname,8);					/* Copy with new name				*/

		return 0;
	}
}

/*
**	Routine:	get_failed()
**
**	Function:	To nofify user that COPY failed.
**
**	Description:	Put up the FAILED getparm for COPY.
**
**	Arguments:
**	ifile		The input file
**	ilib		The input library
**	ivol		The input volume
**	ofile		The output file
**	olib		The output library 
**	ovol		The output volume 
**	retcode		The error code from COPY
**
**	Globals:
**	gp_cnt,gp_args	The Getparm macro and variables for GP.
**	N		The wswaped numbers array.
**	two		Number two.
**
**	Return:		Pfkey number
**	0		Continue
**	16		Exit
**
**	Warnings:	None
**
**	History:	
**	03/04/93	Written by GSL
**
*/

static int get_failed(ifile,ilib,ivol,ofile,olib,ovol,retcode)
char *ifile, *ilib, *ivol;
char *ofile, *olib, *ovol;
int4 retcode;
{
	int4	pfkey_mask;
	char	*gptype;
	char	message[80];
	char	*messid;
	char	pfkey_recv[1];

	pfkey_mask = terminate_key_enabled;
	WL_wswap(&pfkey_mask);
	gptype = "I ";
	messid = "F001";
	sprintf(message,"\204Copy failed with return code = %04d",retcode);
	sprintf(terminate_buff, "    (%d)     Exit    ", terminate_key);

	pfkey_recv[0] = '@';

	gp_cnt = 0;
	GP gptype; GP "R"; GP "FAILED  "; GP pfkey_recv; GP messid; GP "WCOPY "; GP &N[1]; 
				GP message; GP &N[strlen(message)];

	GP "T"; GP "INPUT  File"; GP &N[11]; GP "A"; GP &N[10]; GP "A"; GP &N[2];
	GP "T"; GP ifile; GP &N[8]; 					GP &N[0]; GP &N[0];
	GP "T"; GP "in Library"; GP &N[10];				GP &N[0]; GP &N[0];
	GP "T"; GP ilib; GP &N[8]; 					GP &N[0]; GP &N[0];
	GP "T"; GP "on Volume"; GP &N[9]; 				GP &N[0]; GP &N[0];
	GP "T"; GP ivol; GP &N[6]; 					GP &N[0]; GP &N[0];

	GP "T"; GP "OUTPUT File"; GP &N[11]; GP "A"; GP &N[12]; GP "A"; GP &N[2];
	GP "T"; GP ofile; GP &N[8]; 					GP &N[0]; GP &N[0];
	GP "T"; GP "in Library"; GP &N[10];				GP &N[0]; GP &N[0];
	GP "T"; GP olib; GP &N[8]; 					GP &N[0]; GP &N[0];
	GP "T"; GP "on Volume"; GP &N[9]; 				GP &N[0]; GP &N[0];
	GP "T"; GP ovol; GP &N[6]; 					GP &N[0]; GP &N[0];

	GP "U"; GP "Press:"; GP &N[6]; 					GP "A"; GP &N[20]; GP "A"; GP &N[6];
	GP "T"; GP "   (ENTER)   Continue"; GP &N[21]; 			GP "A"; GP &N[21]; GP "A"; GP &N[6];
	GP "T"; GP terminate_buff; GP &N[21]; 			GP "A"; GP &N[22]; GP "A"; GP &N[6];

	GP "E";
	GP "P";	GP &pfkey_mask;

	GETPARM2(gp_args,gp_cnt);

	if (pfkey_recv[0] == PFKEY_16_PRESSED || 
	    pfkey_recv[0] == terminate_key_pressed) 
	{
		return 16;
	}

	return 0;
}

/*
**	History:
**	$Log: wcopy.c,v $
**	Revision 1.33  2003/04/24 13:43:32  gsl
**	Comments on WL_use_last_prb()
**	
**	Revision 1.32  2003/02/20 16:49:23  gsl
**	use GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.31  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.30  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.29  2003/01/30 21:11:22  gsl
**	Change RENAME to use stdarg.h
**	
**	Revision 1.28  2002/07/25 17:03:42  gsl
**	MSFS->WIN32
**	
**	Revision 1.27  2002/07/23 21:24:54  gsl
**	wrename -> RENAME
**	
**	Revision 1.26  2002/07/12 19:10:22  gsl
**	Global unique WL_ changes
**	
**	Revision 1.25  2002/07/12 17:17:03  gsl
**	Global unique WL_ changes
**	
**	Revision 1.24  2002/07/11 14:33:56  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.23  2002/07/10 21:06:30  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.22  2002/07/10 04:27:32  gsl
**	Rename global routines with WL_ to make unique
**	
**	Revision 1.21  2002/07/09 04:13:50  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.20  2002/06/28 04:02:57  gsl
**	Work on native version of wfopen and wfname
**	
**	Revision 1.19  2002/06/25 18:18:35  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 1.18  2000/03/13 19:16:52  gsl
**	Fix WIN32 warning
**	
**	Revision 1.17  1998-06-18 10:26:35-04  gsl
**	Trk 542 - For NT/95 change to ignore arguments.
**
**	Revision 1.16  1997-09-24 17:56:12-04  gsl
**	Add support for pfkeys12()
**
**	Revision 1.15  1997-06-10 14:51:16-04  scass
**	/changed long to int4 for portability.
**
**	Revision 1.14  1996-12-17 12:05:46-05  gsl
**	Fix short string in getparm message
**
**	Revision 1.13  1996-11-18 15:53:49-08  jockc
**	added call to vwang_title to set screen title
**
**	Revision 1.12  1996-08-29 17:15:24-07  gsl
**	Removed the link-level logic as it is now handled in initglbs()
**
**	Revision 1.11  1996-07-26 10:46:34-07  gsl
**	fix long vs int4 warnings
**
**	Revision 1.10  1996-07-24 15:09:54-07  gsl
**	Fix for NT
**
**	Revision 1.9  1996-07-23 11:13:05-07  gsl
**	drcs update
**
**
**
*/
