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
**	File:		getparm.c
**
**	Project:	wisp/lib
**
**	Purpose:	???
**
**	Routines:	
**	GETPARM()
**	WL_use_last_prb()
*/

/*
	getparm.c	The emulation of the VSSUB  GETPARM.

			The basic algorithim is:
				1) Get the header arguments
				2) Search for a PRB that matches this prname
				3) Build the screen while decoding the arguments (including substituting PRB keyword values)
				4) Display the GETPARM screen
				5) Extract the updated keyword values
				6) Update the PRB with updated values


			Getparm TYPE differences:

				"I " - Initial request
					Update the G/A (getparm arguments) with PRB values.  If PRB not found then request
					from user at W/S (workstation).
				"ID" - Initial Default request
					Update the G/A with PRB values.  This is "Hidden Getparm", no W/S request is performed
					unless the PRB is type=Display.
				"R " - Respecification request
					Update the G/A with W/S request.
				"RD" - Respecification Default request
					Using the G/A updated the PRB values.  This does not "use" the PRB and never
					does a W/S request.  The putparm must be labeled and can be "used".

			Updating of labeled PRBs.

				If the PRB is labeled then it is updated with the final values.

			QUESTIONS ???????

				1) Does a getparm of type "RD" use-up the PRB ?
					Answer: NO.  Also it works on "used" putparms (KEPT). Putparms must be labeled.

				2) Can you force a W/S request on a type "RD" getparm by specifing a DISPLAY putparm ?
					Answer: NO.

				3) With a type "I " getparm if the PRB is found but does not contain all the keywords
				   requested will it force a W/S request ? 
					Answer: NO
*/

/*
**	Includes
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>

#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#endif

#include "idsistd.h"
#include "werrlog.h"
#include "wangkeys.h"
#include "wcommon.h"
#include "scnfacs.h"
#include "sharemem.h"
#include "putparm.h"
#include "wglobals.h"
#include "wispvers.h"
#include "wisplib.h"
#include "vssubs.h"
#include "wexit.h"
#include "idsisubs.h"
#include "vwang.h"
#include "wmalloc.h"
#include "wperson.h"
#include "setprgid.h"
#include "link.h"

/*
**	Structures and Defines
*/

/*
20004	%%GETPARM-F-ARGUMENTS Unable to decipher.(%s - %d) (Add optional args) 
20006	%%GETPARM-E-NOTSUP Function (%s) not supported
20014	%%GETPARM-E-MAXFIELDS Max 32 modifiable fields
*/

#define		INITIAL			0					/* type = "I "					*/
#define		INITIAL_DEFAULT		1					/* type = "ID"					*/
#define		RESPECIFY		2					/* type = "R "					*/
#define		RESPECIFY_DEFAULT	3					/* type = "RD"					*/

#define		FIRST_PFKEY	((uint4)0x80000000)

#define		ALL_KEYS	((uint4)0xFFFFFFFF)
#define		NO_KEYS		((uint4)0x00000000)

/*
**	Globals and Externals
*/

int WL_getparm_alpha_pfkey = 0;	/* This is an external global that tells GETPARM to treat the pfkey-mask as 4 byte alpha value. It is	*/
			/* normally treated as a 4 byte binary (2+2) which creates a problem if on a "little-Endian" (byte-swap)*/
			/* machines if the pfkey-mask is an alpha.								*/

/*
**	Static data
*/

static SHMH *curr_prb;								/* Ptr to PRB for current prname		*/
static int2 last_prb_id=0;							/* The last PRB used				*/
static int use_last_prb_id=0;							/* Use the last PRB used as in last_prd_id	*/

static int nativecharmap = -1;		/* Flag to do automatic CHARMAP translation to native charset */

/*
**	Function Prototypes
*/

static void gdisp_init(void);
static void gdisp_put(int row, int col, fac_t fac, const char* val);
static void gdisp_getparm(char* keylist, unsigned char *aid_char);
static void gdisp_get(int row, int col, char *val, int4 len);			/* get a field from the screen	*/
static void gdisp_cleanup(void);
static void gdisp_postdisp(void);


/*==============================================================================================================================*/
/*
**	GETPARM supports 2 types of calling sequence.
**		1) The regular VSSUB argument list
**		2) A special 2 argument sequence designed for calling it from C. 
**		   The first arg is a pointer to an array of pointers 
**		   the second arg is the number of arguments in the array.
**		   void GETPARM(char* args[], int* cnt)
*/

void GETPARM(char* arg1, char* arg2, ...)
{
	va_list	the_args;
	int arg_count;

	va_start(the_args, arg2); 
	arg_count = WL_va_count();

	if (2 == arg_count)
	{
		GETPARM2((char **)arg1, *((int *)arg2));
	}
	else
	{
		int i;
		char* args[GETPARM_MAX_ARGS];

		args[0] = arg1;
		args[1] = arg2;
		for (i=2;i<arg_count;i++)
		{
			args[i] = va_arg(the_args,char*);
		}
		GETPARM2(args, arg_count);
	}

}

void GETPARM2(char* arg_addr[], int arg_count)
{
	int done,i,j;
	char *the_item;
	int4 *long_item;
	char	keylist[80], templine[84];
	uint4   pfkey_mask;						/* the mask of possible pfkeys			*/
	uint4	key_num;
	int	the_type;							/* the type of getparm				*/
	char	the_type_str[2];
	char	*the_form;							/* the form					*/
	char	*the_name;							/* the name of the caller			*/
	char	*pf_ret;							/* the pfkey return field			*/
	char	*messid;							/* the message id				*/
	char	*messiss;							/* the message issuer				*/
	int4	messlines;							/* the number of lines				*/
	int4	messlines_save;							/* the number of lines				*/
	char	*messtxt;							/* the message text				*/
	int4	messlen;							/* the text length				*/
	char	*sptype;							/* spec type					*/
	char	*spkey;								/* spec keyword					*/
	char	*spval;								/* spec string					*/
	int4	splen;								/* length of the string				*/
	char	*sprf;								/* row flag					*/
       	int4	sprow;	  							/* row value					*/
	char	*spcf;								/* collumn flag					*/
	int4	spcol;								/* collumn value				*/
	char	*spdtype;							/* data type					*/
	char    spaces[9];             						/* A string full of spaces.			*/
	int     relative_row, row, relative_column, column, old_row;		/* Used for relative locations.			*/
	fac_t	keyfieldfac;							/* The fac translated for data type.		*/
	char	prname[9];
	int	found_keyword, display_flag, missing_word, enter_flagged;	/* 1 if screen display required			*/
	int	main_arg7;
	int 	arg4,arg6,arg7,arg9;
	int4    long_temp;
	int	aa_ndx;								/* Address array index value.			*/
	char 	prid[8];							/* Local copy of program id.			*/
	FMTLIST	*fmtlist;							/* List to hold keyword/value pairs		*/
	int	labeled_prb;							/* Was a PRB found and was it labeled		*/
	int	chained_prb;							/* Was a PRB found that is chained		*/
	int	used_ok;							/* Are PRBs with USED status ok to return	*/
	char	tracebuff[2000];

	if (arg_count < 8)
	{
		WL_werrlog_error(WERRCODE(20002),"GETPARM","ARGCNT","Argument Count=[%d] is too small to be a valid", arg_count);
		return;
	}

	WL_wtrace("GETPARM","ENTRY","Entry into GETPARM Type=[%2.2s] Form=[%1.1s] Prname=[%8.8s] Msgid=[%4.4s] Issuer=[%6.6s] args=%d",
		arg_addr[0], arg_addr[1], arg_addr[2], arg_addr[4], arg_addr[5], arg_count);
	tracebuff[0] = '\0';

	/* 
	**	INITIALIZATION
	*/

	/* First time thru set nativecharmap flag */
	if (-1 == nativecharmap)
	{
		nativecharmap = 0;
		if (WL_get_wisp_option("NATIVECHARMAP"))
		{
			nativecharmap = 1;
			/*
			**	If using NATIVECHARMP then ensure the charmap is loaded
			*/
			vwang_load_charmap(0);
		}
	}

	enter_flagged = 0;
	memset(spaces,' ', 8);							/* Fill the string with spaces.			*/
	spaces[8] = '\0';							/* And null terminate it.			*/
	missing_word = 0;							/* Assume no missing keywords			*/


	/*
	**	GETPARM supports 2 types of calling sequence.
	**		1) The regular VSSUB argument list
	**		2) A special 2 argument sequence designed for calling it from C. The first arg is a pointer
	**			to an array of pointers the second arg is the number of arguments in the array.
	*/

	done = arg_count;
	aa_ndx = 0;								/* Start 1st element of array.			*/


	/*
	**	Get the GETPARM Header arguments
	**		1) TYPE
	**		2) FORM
	**		3) PRNAME
	*/

	/*** ARG1 - Type */
	the_item = (char *)arg_addr[aa_ndx++];
	--done;
	
	if      (the_item[0] == 'I' && the_item[1] == ' ') the_type = INITIAL;
	else if (the_item[0] == 'I' && the_item[1] == 'D') the_type = INITIAL_DEFAULT;
	else if (the_item[0] == 'R' && the_item[1] == ' ') the_type = RESPECIFY;
	else if (the_item[0] == 'R' && the_item[1] == 'D') the_type = RESPECIFY_DEFAULT;
	else
	{
		/*
		**	The Wang also supports types "O ", "S ", and "OD".
		*/
		werrlog(WERRCODE(20006),the_item,0,0,0,0,0,0,0);			/* Function not implemented.			*/
		return;
	}

	memcpy(the_type_str,the_item,2);

	/*** ARG2 - Form */
	the_form = (char *)arg_addr[aa_ndx++];
	--done;

	if (*the_form == 'S')							/* if form is Select, default is to enable	*/
	{									/* all pfkeys					*/
		pfkey_mask = ALL_KEYS;
	}
	else									/* otherwise disable the keys			*/
	{
		pfkey_mask = NO_KEYS; 
	}

	strcpy(keylist,"");							/* start the pfkey list				*/

	/*** ARG3 - PRNAME */
	the_name = (char *)arg_addr[aa_ndx++];
	--done;

	memset(prname,0,sizeof(prname));					/* prname is 9 chars null terminated.		*/
	strncpy(prname,the_name,8);


	/*
	**	Search for a PRB that matches the PRNAME for this GETPARM.
	*/

	curr_prb = NULL;
	if (the_type == RESPECIFY_DEFAULT)
	{
		used_ok = 1;							/* PRBs of type USED are ok.			*/

		if (use_last_prb_id)						/* If using last PRB				*/
		{
			if (last_prb_id)					/* We have a last PRB to use			*/
			{
				curr_prb = WL_get_prb_id(last_prb_id);		/* Get the PRB based on the ID			*/
			}

			if (!curr_prb)						/* If no PRB then nothing to do			*/
			{
				goto exit_getparm;				/* Bail out!					*/
			}
		}
	}
	else
	{
		used_ok = 0;
	}
	use_last_prb_id = 0;

	if (!curr_prb && the_type != RESPECIFY)					/* For RESPECIFY don't look for a PRB		*/
	{
		for(;;)
		{
			curr_prb = WL_get_prb_area(prname,NULL,used_ok);	/* Search for a PRB with this PRNAME		*/

			if (!curr_prb) break;					/* No PRB found					*/

			if (curr_prb->status == P_OK) break;			/* We found a valid PRB.			*/

			if (curr_prb->status == P_USED)
			{
				if (the_type == RESPECIFY_DEFAULT) break;	/* For "RD" a used PRB is accepted		*/
			}

			if (curr_prb->status == P_DELETED)			/* If PRB marked DELETED then 			*/
			{
				WL_wax_table_entry(curr_prb);			/* Remove the PRB entry from pr_table		*/
			}
		}
	}

	if (curr_prb && (the_type == INITIAL || the_type == INITIAL_DEFAULT))
	{
		WL_backwards_reference(&curr_prb);				/* Perform any backwards referencing needed	*/
	}

	labeled_prb = 0;							/* Assume no label PRB				*/
	chained_prb = 0;
	if (curr_prb)
	{
		if (curr_prb->label[0] && curr_prb->label[0] != ' ')
		{
			labeled_prb = 1;					/* We found a labeled PRB (Updated PRB values)	*/

			if (WL_get_chained_prb(curr_prb))			/* Check if this is a chained putparm		*/
			{
				chained_prb = 1;
			}
		}
	}

	/*
	**	Save the PRB id used bye the last "I " or "ID" getparm.
	**	This is used by wfopen so the RD will update the correct PRB.
	*/

	if (the_type == INITIAL || the_type == INITIAL_DEFAULT)
	{
		if (labeled_prb)						/* Only save id for a labeled PRB		*/
		{
			last_prb_id = curr_prb->prb_id;				/* Save the id					*/
		}
		else
		{
			last_prb_id = 0;					/* No PRB used					*/
		}
	}

	if (the_type == RESPECIFY_DEFAULT && !labeled_prb)			/* If "RD" and not a labeled PRB then exit.	*/
	{
		goto exit_getparm;
	}

	/*
	**	Get the rest of the Header arguments
	*/

	/*** ARG4 - PFKEY RECEIVER */
	pf_ret = (char *)arg_addr[aa_ndx++];
	--done;

	/*** ARG5 - MESSAGE ID */
	messid = (char *)arg_addr[aa_ndx++];
	--done;

	/*** ARG6 - MESSAGE ISSUER */
	messiss = (char *)arg_addr[aa_ndx++];
	--done;


	/*
	**	Build the GETPARM screen
	*/

	gdisp_init();
	
	gdisp_put(1,2,PLAIN_TEXT,"WISP GETPARM");
	gdisp_put(1,15,PLAIN_TEXT,wisp_version());
	memset(templine,0,sizeof(templine));
	sprintf(templine,"Parameter reference Name: %-s",prname);
	gdisp_put(1,46,PLAIN_TEXT,templine);
	memset(templine,0,sizeof(templine));
	sprintf(templine,"Message Id: %4.4s",messid);
	gdisp_put(2,60,PLAIN_TEXT,templine);
	memset(templine,0,sizeof(templine));
	strcpy(templine,"Component: ");
	strncpy(templine+strlen(templine),messiss,6);
	gdisp_put(3,61,PLAIN_TEXT,templine);
	memset(templine,0,sizeof(templine));

	if (RESPECIFY == the_type)						/* The message is dependent on the type/form	*/
		strcpy(templine,"Correction Required by ");
	else if ('R' != *the_form)
		strcpy(templine,"Response Required by ");
	else
		strcpy(templine,"Information Required by ");

	memcpy(prid,wisp_get_runname(),WISP_RUNNAME_SIZE);
	strncpy(templine+strlen(templine),prid,WISP_RUNNAME_SIZE);
	gdisp_put(4,23,PLAIN_TEXT,templine);
	sprintf(templine,"----------------------------------------");
	strcat(templine,"---------------------------------------");
	gdisp_put(6,2,PLAIN_TEXT,templine);

	/*
	**	ARG7 is optional
	*/

	/*** ARG7 - MESSAGE TEXT LINE COUNT (OPTIONAL) */
	the_item = (char *)arg_addr[aa_ndx++];
	--done;

	/* NOTE: If Little-Endian and noswap and arg7 is not present and Arg8 is only 1 byte int4 then 			*/
	/*	 the test will likely fail. -- Arg8 MUST be made longer then 1 byte.						*/

	if (WL_longargtest(the_item,1))
	{
		/*** ARG7 - MESSAGE TEXT LINE COUNT (PRESENT) */
		main_arg7 = 1;
		long_item = (int4 *)the_item;					/* the number of lines in mess			*/
		messlines = WL_get_swap(long_item);
		messlines_save = messlines;

		while (messlines)						/* get each line of text			*/
		{
			/*** ARG8 - MESSAGE TEXT */
			messtxt = (char *)arg_addr[aa_ndx++];
			--done;

			/*** ARG9 - MESSAGE TEXT LENGTH */
			long_item = (int4 *)arg_addr[aa_ndx++];
			--done;

			messlen = WL_get_swap(long_item);
			messlines--;

			memset(templine,0,sizeof(templine));
			if (messlen > WSB_COLS-1) messlen = WSB_COLS-1;
			memcpy(templine, messtxt,(int)messlen);
			gdisp_put((int)(6+messlines_save-messlines),2,PLAIN_TEXT,templine);
		}
	}
	else
	{
		int	arg7_line;

		main_arg7 = 0;

		/*** ARG8 - MESSAGE TEXT */
		messlines_save = 0;
		messtxt = the_item;						/* the message text itself			*/

		/*** ARG9 - MESSAGE TEXT LENGTH */
		long_item = (int4 *)arg_addr[aa_ndx++];
		--done;

		messlen = WL_get_swap(long_item);

		arg7_line = 6;
		memset(templine,0,sizeof(templine));
		for( i=0, j=0; i < messlen && arg7_line < 23; i++)
		{
			if (messtxt[i] != 0x0d)
			{
				templine[j++] = messtxt[i];
			}

			if ( messtxt[i] == 0x0d || i >= (messlen-1) || j >= WSB_COLS-1 ) /* Break the line on a x0d 		*/
			{
				arg7_line += 1;
				messlines_save += 1;
				gdisp_put(arg7_line,2,PLAIN_TEXT,templine);
				memset(templine,0,sizeof(templine));
				j = 0;
			}
		}
	}

	relative_row = 9 + messlines_save;					/* Start relative offset here.			*/
	relative_column = 2;							/* Ditto.					*/
	row = 0; column = 0;							/* Initialize row & column			*/


	/* 
	**	This section handles the keylist specifications	
	*/
                                          
	while (done > 0)							/* loop till nul ptr or all args are processed	*/
        {

		/* get pointer to next item			*/
		sptype = (char *)arg_addr[aa_ndx++];
		--done;

		switch (*sptype)						/* process each type differently 		*/
		{
		default:

			werrlog(WERRCODE(20004),"SPTYPE",*sptype,0,0,0,0,0,0);	/* Report the args are messed-up		*/
			wexit(WERRCODE(20004));
			break;

		case 'K': case 'k':	/* STANDARD KEYWORD FIELD	*/
		case 'R': case 'r':	/* RESPECIFY KEYWORD FIELD	*/

			{							/* process keyword field    			*/
				int	force_uppercase;

			/*2*/	/* get ptr to KEYWORD NAME item			*/
				spkey = (char *)arg_addr[aa_ndx++];
				--done;

			/*3*/	/* get ptr to KEYWORD VALUE item		*/
				spval = (char *)arg_addr[aa_ndx++];
				--done;

			/*4*/	/* get ptr to KW VAL LENGTH item		*/
				long_item = (int4 *)arg_addr[aa_ndx++];
				splen = WL_get_swap(long_item);
				--done;

			/*5 OPTIONAL */	
				/* get ptr to ROW FLAG item			*/
				sprf = (char *)arg_addr[aa_ndx++];
				--done;

				if (*sprf == 'A' || *sprf == 'R')
				{
			/*6*/		/* get ptr to ROW VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else
				{
					long_item = (int4 *)sprf;
					sprf = "R";
				}
				sprow = WL_get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(WERRCODE(20004),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
				}

			/*7 OPTIONAL */	
				/* get ptr to COLUMN FLAG item			*/
				the_item = (char *)arg_addr[aa_ndx++];
				--done;

				arg9=1;						/* Get arg9					*/
				arg7=0;						/* arg7 not present				*/
				if (*the_item=='R') arg7=1;			/* arg7 was present				*/
				if (*the_item=='A' || *the_item=='C' || *the_item=='J') 
					arg7 = -1;				/* arg7 likely was present			*/
				if (arg7 == -1 && WL_bytenormal()) 
					arg7=1;
				if (arg7)
				{
					spcf = the_item;
			/*8*/		/* get ptr to COLUMN VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else	
				{
					long_item = (int4 *) the_item;
					spcf = "R";
				}

				long_temp = WL_get_swap(long_item);

				if ( long_temp < 0 || long_temp > 80 )
				{
					arg9=0;					/* Don't get arg9 (already have)		*/
					the_item = (char *) long_item;
					spcol = 2;
				}
				else
				{
					spcol = long_temp;
				}


				if (arg9)
				{	
			/*9*/		/* get ptr to DATA TYPE item			*/
					spdtype = (char *)arg_addr[aa_ndx++];
					--done;
				}
				else	spdtype = the_item;

				force_uppercase = 0;

				switch( *spdtype )
				{
				case 'A':
				case 'L':
				case 'U':
				case 'H':
					force_uppercase = 1;
					keyfieldfac = (*sptype == 'R') ? BLINKUP_FIELD:UPCASE_FIELD;
					break;
				case 'a':
				case 'l':
				case 'u':
				case 'h':
					keyfieldfac = UPPROT_FIELD;
					break;
				case 'C':
					keyfieldfac = (*sptype == 'R') ? BLINK_FAC:STANDARD_FIELD;
					break;
				case 'c':
					keyfieldfac = PLAIN_TEXT;
					break;
				case 'N':
				case 'I':
					keyfieldfac = (*sptype == 'R') ? BLINKNUM_FIELD:NUMERIC_FIELD;
					break;
				case 'n':
				case 'i':
					keyfieldfac = NUMPROT_FIELD;
					break;
				case 'B':
					keyfieldfac = ((fac_t)0x98); /* BLANK_FIELD */
					break;
				case 'b':
					keyfieldfac = ((fac_t)0x9C); /* BLANK_FIELD */
					break;
				default:
					werrlog(WERRCODE(20004),"SPDTYPE",*spdtype,0,0,0,0,0,0);
					wexit(WERRCODE(20004));
					break;
				}
										
				old_row = row;
				row = (*sprf == 'A') ? sprow : sprow + relative_row;	/* Relative or absolute loc ?		*/
				if ( row < 9 || row > 24 ) row = 9;
				relative_row = row;				/* New relative row base.			*/
				if ( old_row != row ) relative_column = 0;	/* if row changes start new			*/

				if      (*spcf == 'A') column = spcol;
				else if (*spcf == 'C') column = 40 - splen/2;
				else if (*spcf == 'J') column = 80 - splen - 2;
				else                   column = spcol + relative_column;

				if ( column < 2 || column > 79 ) column = 2;
				relative_column = column;			/* New relative column base.			*/

				if (strcmp(spkey,spaces))			/* Did they specify a keyword ?			*/
				{         
				     	memcpy(templine,spkey,8);		/* copy the keyword (8 chars)			*/
					templine[8] = '\0';			/* null terminate				*/
					strcat(templine," = ");			/* add the = symbol				*/

					if (*spcf == 'C') column = (column >= 5+2) ? column-5: 2;
					if (*spcf == 'J') column = (column >= 11+2) ? column-11: 2;
					if (*sptype == 'K' || *sptype == 'R')
					{
	 					gdisp_put(row,column,PLAIN_TEXT,templine);	/* put on the screen		*/
					}
					column += 11;				/* add 11 to the column				*/
				}

				/*
				**	Get the keyword value from the PRB or the G/A.
				**	For "I " and "ID" the value from the PRB overrides the G/A value.
				**	For "R " and "RD" the G/A value is always used.
				**	NOTE: For "R " there will never be a "curr_prb".
				*/

				found_keyword=0;
				if (curr_prb && the_type != RESPECIFY_DEFAULT)	/* If there is a PRB then search for keyword	*/
				{
					found_keyword = WL_search_parm_area(templine,spkey,splen,curr_prb);

					if (chained_prb)
					{
						/*
						**	If this is a chained putparm then we will search the parm area
						**	of each prb on the chain for the keyword.
						*/

						SHMH	*temp_prb;

						temp_prb = curr_prb;

						while ((temp_prb = WL_get_chained_prb(temp_prb)))
						{
							if (WL_search_parm_area(templine,spkey,splen,temp_prb))
							{
								found_keyword = 1;
							}
						}
					}
				}
				if (!found_keyword)				/* If no PRB value then use spval (G/A)		*/
				{
					missing_word = 1;			/* We didn't find the keyword.			*/
					memcpy(templine,spval,(int)splen);
				}

				templine[splen] = '\0';				/* null terminate				*/

				relative_column = column + splen + 1;

				if (*sptype == 'K' || *sptype == 'R')		/* If keyword is not a "skip"			*/
				{
					if (splen > 0)
					{
						if (force_uppercase)
						{
							WL_upper_string(templine);
						}

						gdisp_put(row,column,keyfieldfac,templine);	/* output the line		*/
					}
				}
				break;
			}

		case 'T': case 't':	/* PLAIN TEXT FIELD 		*/
		case 'U': case 'u':	/* UNDERLINE TEXT FIELD 	*/

			{							/* process Text field				*/
			/*2*/	/* get ptr to VALUE item			*/
				spval = (char *)arg_addr[aa_ndx++];
				--done;

			/*3*/	/* get ptr to VALUE LENGTH item			*/
				long_item = (int4 *)arg_addr[aa_ndx++];
				--done;
				splen = WL_get_swap(long_item);

			/*4 OPTIONAL */	
				/* get ptr to ROW_FLAG item			*/
				the_item = (char *)arg_addr[aa_ndx++];
				--done;

				if (*the_item == 'A' || *the_item == 'R')
				{
					sprf = the_item;
					arg4 = 1;
				}
				else
				{
					sprf = "R";
					arg4 = 0;
				}
				if (arg4) 
				{
			/*5*/		/* get ptr to ROW VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else	long_item = (int4 *)the_item;
				sprow = WL_get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(WERRCODE(20004),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
				}

			/*6 OPTIONAL */	
				/* get ptr to COLUMN FLAG item			*/
				the_item = (char *)arg_addr[aa_ndx++];
				--done;

				if ( *the_item == 'A' || *the_item == 'R'  || *the_item == 'C'  || *the_item == 'J' )
				{
					spcf = the_item;
					arg6 = 1;
				}
				else
				{
					spcf = "R";
					arg6 = 0;
				}
				if (arg6)
				{
			/*7*/		/* get ptr to COLUMN VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else	long_item = (int4 *)the_item;
				spcol = WL_get_swap(long_item);
				if ( spcol < 0 || spcol >80 )			/* Report the args are messed-up		*/
				{
					werrlog(WERRCODE(20004),"SPCOL",spcol,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
				}

				old_row = row;
				row = (*sprf == 'A') ? sprow : sprow + relative_row;	/* Relative or absolute loc ?		*/
				if ( row < 9 || row > 24 ) row = 9;
				relative_row = row;				/* New relative row base.			*/
				if ( old_row != row ) relative_column = 0;	/* if row changes start new			*/

				if      (*spcf == 'A') column = spcol;
				else if (*spcf == 'C') column = 40 - splen/2;
				else if (*spcf == 'J') column = 80 - splen - 2;
				else                   column = spcol + relative_column;

				if ( column < 2 || column > 79 ) column = 2;
				relative_column = column;			/* New relative column base.			*/

				memcpy(templine,spval,(int)splen);		/* get the text value				*/
				templine[splen] = '\0';				/* null terminate				*/
				relative_column = column + splen + 1;

				if (*sptype == 'T' || *sptype == 'U')		/* If not a "skip"				*/
				{
					fac_t	fieldfac;

					if (*sptype == 'T')	fieldfac = PLAIN_TEXT;
					else			fieldfac = UNDER_TEXT;

					if (splen > 0)
					{
						gdisp_put(row,column,fieldfac,templine);	/* output the line		*/
					}
				}
				break;
			}
                                 
		case 'p':
			{							/* Skip the following params.			*/
				aa_ndx++;
				--done;
				break;
			}
		case 'P':      
			{							/* process pfkey mask field			*/
										/* get ptr to VALUE item			*/
				spval = (char *)arg_addr[aa_ndx++];
				--done;

				long_item = (int4*) spval;                                                           
				pfkey_mask = WL_get_swap(long_item);
				if (WL_getparm_alpha_pfkey && !WL_bytenormal())
				{
					WL_reversebytes((char *)&pfkey_mask,4);  /* Correct alpha pfkey-mask			*/
				}
				break;
			}
                                       
		case 'e':
			{
				enter_flagged = 1;
				break;
			}
		case 'E':
		   	{
				strcat(keylist,"00");				/* enter is allowed				*/
				enter_flagged = 1;
				break;                                                                                            
			}
                                 
		case 'N':
		case 'n':
			{							/* process ENTER flag spec			*/
				enter_flagged = 1;
				break;
			}
		}								/* end of switch				*/
	}

	if ( !enter_flagged )							/* No "enter" sequences so			*/
	{
		strcpy(keylist,"00");						/* assume enter is allowed			*/
	}

	key_num = FIRST_PFKEY;

	for (i=1; i<33; i++)							/* check the pfkeys				*/
	{
		if (key_num & pfkey_mask)					/* if the key is enabled...			*/
		{
			sprintf(templine,"%02d",i);				/* make it a 2 char integer			*/
			strcat(keylist,templine);				/* add to the list				*/
		}
		key_num = key_num >> 1;						/* shift right one bit				*/
	}                                                                                                                         
	strcat(keylist,"X");							/* terminate with an X				*/

	/*
	**	Determine if we are going to do a Workstation request (W/S).
	*/

	switch(the_type)
	{
	case INITIAL:
		display_flag = 1;						/* Display unless PRB type ENTER		*/
		if (curr_prb && curr_prb->type == 'E')
		{
			display_flag = 0;
		}
		break;
	case INITIAL_DEFAULT:
		display_flag = 0;						/* Don't display unless PRB type DISPLAY	*/
		if (curr_prb && curr_prb->type == 'D')
		{
			display_flag = 1;
		}
		break;
	case RESPECIFY:
		display_flag = 1;						/* Alway display				*/
		break;
	case RESPECIFY_DEFAULT:
		display_flag = 0;						/* Never display				*/
		break;
	default:
		display_flag = 1;						/* This will never happen.			*/
		break;
	}

	if (display_flag) 							/* Do we have to display ?			*/
	{
		unsigned char aid_char;
		
		gdisp_getparm(keylist,&aid_char);

		pf_ret[0] = aid_char;
		sprintf(&tracebuff[strlen(tracebuff)],"AID=[%c]", pf_ret[0]);
	}
	else
	{
		if (curr_prb) 	
		{
			pf_ret[0] = curr_prb->pfkey;				/* Get the pfkey from the putparm		*/
			sprintf(&tracebuff[strlen(tracebuff)],"AID=[%c]", pf_ret[0]);
		}
	        else
		{
			/* If no display and no putparm found then don't modify pf_ret. */
			/* enter_key_presspf_ret[0] = ENTER_KEY_PRESSED; */

			/* NOTE: pf_ret may be NULL in this case */
		}
	}

	strcat(tracebuff, " FIELDS: ");

	fmtlist = NULL;

	/* start again getting items	*/
	done = arg_count;
	aa_ndx = 0;						/* Start 1st element of array.			*/

	aa_ndx++;						/* Skip ptr to TYPE item			*/
	--done;
	aa_ndx++;						/* Skip ptr to FORM item			*/
	--done;
	aa_ndx++;						/* Skip ptr to NAME item			*/
	--done;
	aa_ndx++;						/* Skip ptr to PFKEY item			*/
	--done;
	aa_ndx++;						/* Skip ptr to ID item				*/
	--done;
	aa_ndx++;						/* Skip ptr to ISSUER item			*/
	--done;

	if (main_arg7)
	{
		/* get # MESSAGE LINES item			*/
		long_item = (int4 *)arg_addr[aa_ndx++];
		messlines = WL_get_swap(long_item);
		--done;
                             
		while (messlines)				/* skip each line of text			*/
		{
			aa_ndx++;				/* Skip ptr to TEXT item			*/
			--done;
			aa_ndx++;				/* Skip ptr to TEXT LENGTH item			*/
			--done;
			messlines--;
		}
	}
	else
	{
			aa_ndx++;				/* Skip ptr to MESSQGE TEXT item		*/
			--done;
			aa_ndx++;				/* Skip ptr to TEXT LENGTH item			*/
			--done;
	}
	relative_row = 9 + messlines_save;					/* Start relative offset here.			*/
	relative_column = 2;							/* Ditto.					*/
	row = 0; column = 0;							/* Initialize row & column			*/
										/* keylist specifications			*/
	while (done != 0)							/* loop till all done with the			*/
	{									/* args.              				*/
										/* get ptr to next item				*/
		sptype = (char *)arg_addr[aa_ndx++];
		--done;

		switch (*sptype)						/* process each type differently 		*/
		{                

		default:
			werrlog(WERRCODE(20004),"SPTYPE",*sptype,0,0,0,0,0,0);	/* Report the args are messed-up		*/
			wexit(WERRCODE(20004));
			break;

		case 'K': case 'k':	/* STANDARD KEYWORD FIELDS	*/
		case 'R': case 'r':	/* RESPECIFY KEYWORD FIELDS	*/

			{							/* process keyword field			*/
			/*2*/	/* get ptr to KEYWORD item			*/
				spkey = (char *)arg_addr[aa_ndx++];
				--done;

			/*3*/	/* get ptr to KEYWORD VALUE item		*/
				spval = (char *)arg_addr[aa_ndx++];
				--done;

			/*4*/	/* get ptr to KW VAL LENGTH item		*/
				long_item = (int4 *)arg_addr[aa_ndx++];
				--done;
				splen = WL_get_swap(long_item);

			/*5 OPTIONAL */	
				/* get ptr to ROW FLAG item			*/
				sprf = (char *)arg_addr[aa_ndx++];
				--done;

				if (*sprf == 'A' || *sprf == 'R')
				{
			/*6*/		/* get ptr to ROW VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else
				{
					long_item = (int4 *)sprf;
					sprf = "R";
				}
				sprow = WL_get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(WERRCODE(20004),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
				}

			/*7 OPTIONAL */	
				/* get ptr to COLUMN FLAG item			*/
				the_item = (char *)arg_addr[aa_ndx++];
				--done;

				arg9=1;						/* Get arg9					*/
				arg7=0;						/* arg7 not present				*/
				if (*the_item=='R') arg7=1;			/* arg7 was present				*/
				if (*the_item=='A' || *the_item=='C' || *the_item=='J') arg7 = -1;/* arg7 likely was present	*/
				if (arg7 == -1 && WL_bytenormal()) arg7=1;
				if (arg7)
				{
					spcf = the_item;
			/*8*/		/* get ptr to COLUMN VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else	
				{
					long_item = (int4 *) the_item;
					spcf = "R";
				}

				long_temp = WL_get_swap(long_item);
				if ( long_temp < 0 || long_temp > 80 )
				{
					arg9=0;					/* Don't get arg9 (already have)		*/
					the_item = (char *) long_item;
					spcol = 2;
				}
				else
				{
					spcol = long_temp;
				}
				if (arg9)
				{	
			/*9*/		/* get ptr to DATA TYPE item			*/
					spdtype = (char *)arg_addr[aa_ndx++];
					--done;
				}
				else	spdtype = the_item;

				switch (*spdtype)				/* Validate ARG9				*/
				{
				case 'A': case 'a':
				case 'C': case 'c':
				case 'I': case 'i':
				case 'N': case 'n':
				case 'L': case 'l':
				case 'U': case 'u':
				case 'H': case 'h':
				case 'B': case 'b':
					break;
				default:					/* Report the args are messed-up		*/
					werrlog(WERRCODE(20004),"SPDTYPE",*spdtype,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
					break;
				}

				old_row = row;
				row = (*sprf == 'A') ? sprow : sprow + relative_row;	/* Relative or absolute loc ?		*/
				if ( row < 9 || row > 24 ) row = 9;
				relative_row = row;				/* New relative row base.			*/
				if ( old_row != row ) relative_column = 0;	/* if row changes start new			*/
				if      (*spcf == 'A') column = spcol;
				else if (*spcf == 'C') column = 40 - splen/2;
				else if (*spcf == 'J') column = 80 - splen - 2;
				else                   column = spcol + relative_column;
				if ( column < 2 || column > 79 ) column = 2;
				relative_column = column;			/* New relative column base.			*/
				if (strcmp(spkey,spaces))			/* Did they specify a keyword ?			*/
				{
					if (*spcf == 'C') column = (column >= 5+2) ? column-5: 2;
					if (*spcf == 'J') column = (column >= 11+2) ? column-11: 2;
					column += 11;				/* add 11 to the column				*/
				}
				templine[splen] = '\0';				/* null terminate				*/
				relative_column = column + splen + 1;

				if (*sptype == 'K' || *sptype == 'R')		/* If not a "skip"				*/
				{
					if (splen > 0)
					{
						char tracefield[200];

						gdisp_get(row,column,spval,splen);	/* get the field			*/

						/*
						**	Trace the FIELD
						*/
						sprintf(tracefield,"%8.8s=[%*.*s] ",spkey,splen,splen,spval);
						strcat(tracebuff,tracefield);

						/*
						** 	If PRB and labeled then we have to updated the PRB with all these
						**	values so build a fmtlist.
						*/

						if (labeled_prb)
						{
							static FMTLIST	*p;

							if (!fmtlist)
							{
								p = fmtlist = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
							}
							else
							{
								p->next = (FMTLIST *)wisp_calloc(1,sizeof(FMTLIST));
								p = p->next;
							}
							p->next = NULL;
							memcpy(p->keyword,spkey,8);
							p->len = splen;
							p->value = (char *)wisp_calloc((int)(p->len+1),sizeof(char));
							memcpy(p->value,spval,(int)(p->len));
							p->special = SPECIAL_NOT;
						}
					}
				}
				break;
			}


		case 'T': case 't':	/* PLAIN TEXT FIELD	*/
		case 'U': case 'u':	/* UNDERLINE TEXT FIELD	*/

			{							/* skip the Text field				*/
			/*2*/	/* get ptr to VALUE item			*/
				spval = (char *)arg_addr[aa_ndx++];
				--done;

			/*3*/	/* get ptr to VALUE LENGTH item			*/
				long_item = (int4 *)arg_addr[aa_ndx++];
				--done;
				splen = WL_get_swap(long_item);

			/*4 OPTIONAL */	
				/* get ptr to ROW_FLAG item			*/
				the_item = (char *)arg_addr[aa_ndx++];
				--done;

				if (*the_item == 'A' || *the_item == 'R')
				{
					sprf = the_item;
					arg4 = 1;
				}
				else
				{
					sprf = "R";
					arg4 = 0;
				}

				if (arg4) 
				{
			/*5*/		/* get ptr to ROW VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else          long_item = (int4 *)the_item;
				sprow = WL_get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(WERRCODE(20004),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
				}

			/*6 OPTIONAL */	
				/* get ptr to COLUMN FLAG item			*/
				the_item = (char *)arg_addr[aa_ndx++];
				--done;

				if ( *the_item == 'A' || *the_item == 'R'  || *the_item == 'C'  || *the_item == 'J' )
				{
					spcf = the_item;
					arg6 = 1;
				}
				else
				{
					spcf = "R";
					arg6 = 0;
				}
				if (arg6) 
				{
			/*7*/		/* get ptr to COLUMN VALUE item			*/
					long_item = (int4 *)arg_addr[aa_ndx++];
					--done;
				}
				else	long_item = (int4 *)the_item;
				spcol = WL_get_swap(long_item);
				if ( spcol < 0 || spcol >80 )			/* Report the args are messed-up		*/
				{
					werrlog(WERRCODE(20004),"SPCOL",spcol,0,0,0,0,0,0);	
					wexit(WERRCODE(20004));
				}
				old_row = row;
				row = (*sprf == 'A') ? sprow : sprow + relative_row;	/* Relative or absolute loc ?		*/
				if ( row < 9 || row > 24 ) row = 9;
				relative_row = row;				/* New relative row base.			*/
				if ( old_row != row ) relative_column = 0;	/* if row changes start new			*/
				if      (*spcf == 'A') column = spcol;
				else if (*spcf == 'C') column = 40 - splen/2;
				else if (*spcf == 'J') column = 80 - splen - 2;
				else                   column = spcol + relative_column;
				if ( column < 2 || column > 79 ) column = 2;
				relative_column = column;			/* New relative column base.			*/
				memcpy(templine,spval,(int)splen);		/* get the text value				*/
				templine[splen] = '\0';				/* null terminate				*/
				relative_column = column + splen + 1;
				break;
			}
		case 'p':
			{							/* Skip the following params.			*/
				aa_ndx++;
				--done;
				break;
			}
		case 'P':
			{
				/* get ptr to PFKEY MASK item			*/
				spval = (char *)arg_addr[aa_ndx++];
				--done;
				break;
			}
		case 'E':
		case 'e':
		case 'N':
		case 'n':
			{							/* process ENTER flag spec			*/
				break;                                                                                            
			}
		}								/* end of switch				*/
	}

	if (labeled_prb && fmtlist)						/* Update the labeled PRB with new values	*/
	{
		WL_update_prb(&curr_prb,fmtlist);

		if (chained_prb)
		{
			SHMH	*temp_prb;

			temp_prb = curr_prb;
			while ( (temp_prb = WL_get_chained_prb(temp_prb)) )
			{
				WL_update_prb(&temp_prb,fmtlist);
			}
		}

		WL_free_fmtlist(fmtlist);
	}

	if (display_flag)
	{
		gdisp_postdisp();
	}

	/*
	**	If a PRB was found then "use" it up (except for type "RD" which doesn't use-up PRBs).
	*/

	if (curr_prb && the_type != RESPECIFY_DEFAULT)
	{
		WL_use_prb(curr_prb);						/* Use this PRB.				*/

#ifdef NOTYET
		/*
		**	Currently we only "USE" the lowest one on the chain.
		**
		**	The use_last_prb logic doesn't work very well for chained putparms,
		**	this is because the "chain" structure is not maintained in the prb.
		**	The "chain" structure is figured out each time it is needed, if
		**	the putparms up the chain got "used" then the chain structure
		**	would be different next time it is traced.
		**
		**	In the future, we will store the chain in the prb as a chain_prb_id
		**	field.   Then WL_update_prb() and WL_use_prb() can be made to follow
		**	the chain.
		*/
		if (chained_prb)
		{
			SHMH	*temp_prb;

			temp_prb = curr_prb;
			while ( temp_prb = WL_get_chained_prb(temp_prb) )
			{
				WL_use_prb(temp_prb);
			}
		}
#endif
	}

exit_getparm:


	WL_cleanup_shrfil();
	gdisp_cleanup();

	WL_wtrace("GETPARM","RETURN", "Prname=[%8.8s] %s", arg_addr[2], tracebuff);

	return;
}

/*
	WL_use_last_prb	This routine instructs GETPARM to use the last PRB that was used. (last_prb_id)
			It is only called before a GETPARM type="RD" to indicate that we want to update the PRB used
			by the last GETPARM of type "I " or "ID".
*/
void WL_use_last_prb(void)
{
	use_last_prb_id = 1;
}

/*==============================================================================================================================*/

#define MAX_COB_GP_FIELDS	32	/* Max number of entry fields */
#define MAX_GP_FIELD_LEN	79	/* Max length of entry fields */

static unsigned char *local_screen = NULL;
static int cob_fields = 0;

typedef struct
{
	uint2 row;			/* UNSIGNED-SHORT 	*/
	uint2 col;			/* UNSIGNED-SHORT 	*/
	uint2 len;			/* UNSIGNED-SHORT 	*/
	char fac;			/* PIC X COMP-X 	*/
	char field[MAX_GP_FIELD_LEN];	/* PIC X(79) 		*/
} cob_gp_field;

static cob_gp_field *cob_field_list = NULL;

/*
**	ROUTINE:	gdisp_init()
**
**	FUNCTION:	Init the screen display structs.
**
**	DESCRIPTION:	WISP screens: 
**			Init the vwang() screen buffer
**
**			Native screens:
**			Init the static text buffer and the field table.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	
**	local_screen	Screen image buffer.
**	cob_fields	Number of entry fields.
**	cob_field_list	Table of entry fields for cobol native screens
**
**	RETURN:		None
**
**	WARNINGS:	This must be called as the first gdisp_xxx() routines.
**
*/
static void gdisp_init(void)
{
	int	i;
	
	if (wisp_nativescreens())
	{
		cob_fields = 0;
		
		local_screen = wisp_malloc(WSB_ROWS*WSB_COLS);
		memset(local_screen,' ', WSB_ROWS*WSB_COLS);

		cob_field_list = wisp_malloc(sizeof(cob_gp_field)*MAX_COB_GP_FIELDS);
		memset(cob_field_list, ' ', sizeof(cob_gp_field)*MAX_COB_GP_FIELDS);

		for(i=0; i<MAX_COB_GP_FIELDS;i++)
		{
			/* This is a empty spot on a GETPARM screen */
			if (wisp_acu_cobol())
			{
				cob_field_list[i].row =  2;
				cob_field_list[i].col = 79;
				cob_field_list[i].len =  1;
			}
			else /* if (wisp_mf_cobol()) */
			{
				cob_field_list[i].row =  0;
				cob_field_list[i].col =  0;
				cob_field_list[i].len =  0;
			}
		}
	}
	else
	{
		local_screen = wisp_malloc(2000);
		WL_wsc_init(local_screen,0,0);
	}
}

/*
**	ROUTINE:	gdisp_postdisp()
**
**	FUNCTION:	Perform post-display screen actions.
**
**	DESCRIPTION:	WISP screens: 
**			Pop the screen stack and resynch if needed
**
**			Native screens:
**			No-op.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	Must be called after gdisp_getparm().
**
*/
static void gdisp_postdisp(void)
{
	if (wisp_nativescreens())
	{
		/* No action needed */
	}
	else
	{
		vwang_wpopscr();						/* Restore any saved screen.			*/
		vwang_set_synch(ede_synch);					/* Synch. not needed unless EDE			*/
	}
}


/*
**	ROUTINE:	gdisp_cleanup()
**
**	FUNCTION:	Cleanup the screen display structs.
**
**	DESCRIPTION:	WISP screens: 
**			Free the screen buffer
**
**			Native screens:
**			Free the field table
**
**	ARGUMENTS:	None
**
**	GLOBALS:	
**	local_screen	Screen image buffer.
**	cob_fields	Number of entry fields.
**	cob_field_list	Table of entry fields for cobol native screens
**
**	RETURN:		None
**
**	WARNINGS:	Must be called as the last action.
**
*/
static void gdisp_cleanup(void)
{
	free(local_screen);
	local_screen = NULL;

	if (cob_field_list)
	{
		free(cob_field_list);
		cob_field_list = NULL;
	}
}

/*
**	ROUTINE:	gdisp_put()
**
**	FUNCTION:	Add a text of entry field to the getparm screen.
**
**	DESCRIPTION:	WISP screens: 
**			Insert the fac and data into the vwang() screen buffer.
**
**			Native screens:
**			Add the text to the static text buffer. 
**			If a entry field add it to the entry field table.
**
**	ARGUMENTS:	
**	row		The fields row position (1-24)
**	col		The fields column position (1-80)
**	fac		The wang style fac for this field.
**	val		The field value (NULL terminated)
**
**	GLOBALS:	
**	local_screen	Screen image buffer.
**	cob_fields	Number of entry fields.
**	cob_field_list	Table of entry fields for cobol native screens
**
**	RETURN:		None
**
**	WARNINGS:	Must call gdisp_init() first.
**
*/
static void gdisp_put(int row, int col, fac_t fac, const char* val)
{
	if (wisp_nativescreens())
	{
		int	len;
		
		len = strlen(val);		

		/*
		**	Validate the values
		*/
		if (row < 1 || row > WSB_ROWS)
		{
			WL_wtrace("GETPARM","ROW","Invalid row value %d",row);
			row = 1;
		}
		if (col < 1 || col > WSB_COLS)
		{
			WL_wtrace("GETPARM","COLUMN","Invalid column value %d",col);
			col = 1;
		}
		if (len > MAX_GP_FIELD_LEN)
		{
			WL_wtrace("GETPARM","LENGTH","Invalid field length value %d",len);
			len = MAX_GP_FIELD_LEN;
		}
		
		/*
		**	If not a blank field - add it to the static text buffer
		*/
		if (!FAC_BLANK(fac))
		{
			memcpy(&local_screen[(row-1)*WSB_COLS + col-1], val, len);
		}

		/*
		**	If an entry field then add it to the entry field table
		*/
		if (!FAC_PROTECTED(fac))
		{
			if (cob_fields >= MAX_COB_GP_FIELDS)
			{
				WL_werrlog_error(WERRCODE(20014),"GETPARM", "MAXFIELDS", 
					"Max 32 modifiable fields");
			}
			else
			{
				cob_field_list[cob_fields].row = row;
				cob_field_list[cob_fields].col = col;
				cob_field_list[cob_fields].len = len;

				cob_field_list[cob_fields].fac = fac;
			
				memcpy(cob_field_list[cob_fields].field, val, len);
				cob_fields++;
			}
		}
	}
	else
	{
		/*
		**	Insert the FAC and text into the vwang buffer
		*/

		char str[256];

		strcpy(str,val);
		
		if (nativecharmap)
		{
			/*
			**	Translate ansi charset into wang charset in preparation of vwang call.
			*/
			vwang_ansi2wang((unsigned char *)str, strlen(str));
		}

		WL_put_screen_text(local_screen, row, col, fac, str);
	}
}


/*
**	ROUTINE:	gdisp_getparm()
**
**	FUNCTION:	Display the GETPARM.
**
**	DESCRIPTION:	WISP screens: 
**			Push the screen then call vwang() to display the screen buffer.
**
**			Native screens:
**			Call "WACUGETPARM" or "WMFNGETPARM" to display the screen.
**
**	ARGUMENTS:	
**	keylist		The valid function keys in ascii pairs (eg "00010316X")
**	aid_char	The returned AID char used to terminate the GETPARM.
**
**	GLOBALS:	
**	local_screen	Screen image buffer.
**	cob_fields	Number of entry fields.
**	cob_field_list	Table of entry fields for cobol native screens
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void gdisp_getparm(char* keylist, unsigned char *aid_char)
{
	if (wisp_nativescreens())
	{
		/*
		**	CALL "WACUGETPARM"/"WMFNGETPARM" USING
		**	[0]	static_text		PIC X(24*80)
		**	[1]	field_cnt		PIC 99			0-32
		**	[2]	field_table		PIC X(86) * 32	
		**	[3]	key_list		PIC X(80)
		**	[4]	key_cnt			UNSIGNED-SHORT		1-32
		**	[5]	term_key		UNSIGNED-SHORT		0-32
		*/
		char	*parms[6];
		int4	lens[6];
		int4	rc;
		uint2	field_cnt;
		uint2	key_cnt;
		uint2	term_key;
		char 	*ptr;

		field_cnt = cob_fields;
		if (field_cnt > MAX_COB_GP_FIELDS)
		{
			/* ERROR - Too many fields */
			WL_wtrace("GETPARM","FIELDCNT","Field count [%d] exceeds NATIVESCREENS limit [%d]",
				field_cnt, MAX_COB_GP_FIELDS);
		}

		/*
		**	Count number of keys in the key list
		*/
		key_cnt = 0;
		if ((ptr = strchr(keylist,'X')))
		{
			*ptr = (char)0;
			key_cnt = (uint2)(strlen(keylist) / 2);
		}
		if (key_cnt < 1)
		{
			WL_wtrace("GETPARM","KEYLIST","Invalid keylist [%s]",keylist);
			key_cnt = 1;
			strcpy(keylist,"00X");
		} 
		else if (key_cnt > 32)
		{
			/* ERROR - Too many keys */
			WL_wtrace("GETPARM","KEYCNT","Key count [%d] exceeds NATIVESCREENS limit [%d]",
				key_cnt, 32);
		}

		parms[0] = (char*)local_screen;
		lens[0]  = WSB_ROWS*WSB_COLS;

		parms[1] = (char*)&field_cnt;
		lens[1]  = sizeof(field_cnt);

		parms[2] = (char*)cob_field_list;
		lens[2]  = sizeof(cob_gp_field)*MAX_COB_GP_FIELDS;
		
		parms[3] = keylist;
		lens[3]  = 80;
		
		parms[4] = (char*)&key_cnt;
		lens[4]  = sizeof(key_cnt);
		
		parms[5] = (char*)&term_key;
		lens[5]  = sizeof(term_key);
		
		if (wisp_acu_cobol())
		{
			WL_call_acucobol("WACUGETPARM", 6, parms, lens, &rc);
		}
		else if (wisp_mf_cobol())
		{
			WL_call_mfcobol("WMFNGETPARM", 6, parms, lens, &rc);
		}

		if (rc)
		{
			int4	wrc, wcc;
			
			if (wisp_acu_cobol())
			{
				WL_call_acucobol_error(rc, &wrc, &wcc, "WACUGETPARM");
			}
			else if (wisp_mf_cobol())
			{
				WL_wtrace("GETPARM","WMFNGETPARM","RC = [%d]", rc);
			}
		}
		else
		{
			int i;
			
			/*
			**	Load the field data into the screen buffer so it can
			**	be retrieved with gdisp_get().
			*/
			for(i=0; i<cob_fields; i++)
			{
				int	row,col,len;

				row = cob_field_list[i].row;
				col = cob_field_list[i].col;
				len = cob_field_list[i].len;
				
				memcpy(&local_screen[(row-1)*WSB_COLS + col-1], cob_field_list[i].field, len);
			}

			/*
			**	Convert the term key to an aid char
			*/
			if (term_key > 32)   /* term_key is unsigned */
			{
				WL_wtrace("GETPARM","TERMKEY","Invalid term_key [%d]",term_key);
				term_key = 0;
			}
			
			*aid_char = ("@ABCDEFGHIJKLMNOPabcdefghijklmnop")[term_key];
		}
	}
	else
	{
		unsigned char	function, lines, no_mod[2];
		char term[2];

		no_mod[0] = no_mod[1] = ' ';
		function = DISPLAY_AND_READ_ALTERED;
		lines = 24;

		vwang_wpushscr();						/* Push the old screen.				*/
		vwang(&function,local_screen,&lines,keylist,term,no_mod);	/* display the screen				*/
		*aid_char = no_mod[1];						/* Get the key value				*/
	}
}

/*
**	ROUTINE:	gdisp_get()
**
**	FUNCTION:	Get an entry fields data after user input.
**
**	DESCRIPTION:	Copy the data out of the screen buffer.
**
**	ARGUMENTS:	
**	row		The fields row position (1-24)
**	col		The fields column position (1-80)
**	val		The returned value
**	len		The length of the field (1-79)
**
**	GLOBALS:	
**	local_screen	Screen image buffer.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void gdisp_get(int row, int col, char *val, int4 len)
{
	if (wisp_nativescreens())
	{
		memcpy(val, &local_screen[((row-1)*WSB_COLS)+(col-1)], len);
	}
	else
	{
		memcpy(val, &local_screen[((row-1)*WSB_COLS)+(col-1)+4], len);

		if (nativecharmap)
		{
			/*
			**	Translate wang charset into ansi charset following a vwang read.
			*/
			vwang_wang2ansi((unsigned char *)val, len);
		}
	}
}


/*
**	History:
**	$Log: getparm.c,v $
**	Revision 1.51  2003/08/28 20:28:32  gsl
**	FIx native screen field initialization
**	
**	Revision 1.50  2003/08/25 21:10:17  gsl
**	MF Native Screens
**	
**	Revision 1.49  2003/04/21 20:02:59  gsl
**	WL_use_last_prb() is void
**	
**	Revision 1.48  2003/04/14 14:05:23  gsl
**	Tracing
**	
**	Revision 1.47  2003/04/04 18:05:43  gsl
**	Trace fields
**	
**	Revision 1.46  2003/04/04 17:08:09  gsl
**	Trace fields
**	
**	Revision 1.45  2003/02/21 19:36:11  gsl
**	Switch GETPARM to stdarg.h
**	
**	Revision 1.44  2003/02/19 22:16:13  gsl
**	Add GETPARM2() the 2 arg interface to GETPARM()
**	
**	Revision 1.43  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.42  2003/01/31 21:24:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.41  2003/01/31 17:33:56  gsl
**	Fix  copyright header
**	
**	Revision 1.40  2002/12/10 17:09:19  gsl
**	Use WL_wtrace for all warning messages (odd error codes)
**	
**	Revision 1.39  2002/12/09 21:09:27  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.38  2002/12/09 19:15:31  gsl
**	Change to use WL_werrlog_error()
**	
**	Revision 1.37  2002/10/18 19:14:11  gsl
**	Cleanup
**	
**	Revision 1.36  2002/08/01 15:31:27  gsl
**	type warnings
**	
**	Revision 1.35  2002/08/01 14:09:11  gsl
**	type warnings
**	
**	Revision 1.34  2002/07/16 16:24:56  gsl
**	Globals
**	
**	Revision 1.33  2002/07/12 17:00:56  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.32  2002/07/11 20:29:08  gsl
**	Fix WL_ globals
**	
**	Revision 1.31  2002/07/11 14:33:58  gsl
**	Fix WL_ unique globals
**	
**	Revision 1.30  2002/07/10 21:05:16  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.29  2002/07/09 04:14:01  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.28  2002/07/02 21:15:24  gsl
**	Rename wstrdup
**	
**	Revision 1.27  2002/07/02 04:00:38  gsl
**	change acu_cobol and mf_cobol to wisp_acu_cobol() and wisp_mf_cobol()
**	
**	Revision 1.26  2002/07/01 04:02:37  gsl
**	Replaced globals with accessors & mutators
**	
**	Revision 1.25  1999/05/21 14:49:29  gsl
**	Change getprogid() to WISPRUNNAME as the program name shown on the
**	getparm screen.
**	
**	Revision 1.24  1998-10-13 14:55:59-04  gsl
**	Fix test of term_key since it is unsigned it can't be neg.
**
**	Revision 1.23  1998-07-09 16:56:31-04  gsl
**	initialize the no_mod arg to vwang()
**
**	Revision 1.22  1997-12-18 20:58:40-05  gsl
**	Fix CHARMAP handling
**
**	Revision 1.21  1997-12-17 20:44:56-05  gsl
**	Add support for NATIVECHARMAP option
**
**	Revision 1.20  1997-10-21 10:15:32-04  gsl
**	Changed to use getprogid()
**	Changed to use WL_get_swap()
**
**	Revision 1.19  1997-09-30 14:05:51-04  gsl
**	FIx warnings
**
**	Revision 1.18  1997-09-24 12:00:45-04  gsl
**	Add error tracing
**
**	Revision 1.17  1997-09-23 17:13:31-04  gsl
**	Add the Acucobol native screen support
**
**	Revision 1.16  1997-08-21 16:19:45-04  gsl
**	Add support for a BLANK "B" key field data type.
**
**	Revision 1.15  1997-05-08 17:17:31-04  gsl
**	Change to use WL_wtrace()
**
**	Revision 1.14  1997-04-22 21:31:24-04  gsl
**	Fix handling of Message_Text so it doesn't overwrite memory with
**	a long message
**
**	Revision 1.13  1996-07-17 17:50:27-04  gsl
**	change to use wcalloc()
**
**	Revision 1.12  1996-07-09 16:34:50-07  gsl
**	Fix last_prb_id to be an int2
**
**	Revision 1.11  1996-06-28 08:51:57-07  gsl
**	add missing include files
**
**	Revision 1.10  1995-06-14 08:23:40-07  gsl
**	change to use wisp_version()
**
 * Revision 1.9  1995/04/25  09:52:47  gsl
 * drcs state V3_3_15
 *
 * Revision 1.8  1995/04/17  11:46:10  gsl
 * drcs state V3_3_14
 *
 * Revision 1.7  1995/03/09  13:42:02  gsl
 * replace synch_required with call to vwang_set_synch() plus
 * added standard headers
 *
**
**
*/
