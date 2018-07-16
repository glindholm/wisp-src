static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		getparm.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
**	Routines:	
**	GETPARM()
**	use_last_prb()
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
#include <varargs.h>
#include <errno.h>
#include <string.h>

#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#endif

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wangkeys.h"
#include "wcommon.h"
#include "scnfacs.h"
#include "sharemem.h"
#include "putparm.h"
#include "wglobals.h"
#include "wispvers.h"
#include "wisplib.h"
#include "wexit.h"
#include "idsisubs.h"
#include "vwang.h"
#include "wmalloc.h"
#include "cobrun.h"
#include "wperson.h"
#include "setprgid.h"

/*
**	Structures and Defines
*/
#define		ROUTINE		20000
/*
20001	%%GETPARM-I-ENTRY Entry into GETPARM
20002	%%GETPARM-F-NOMEM Can't get memory for GETPARM screen
20003	%%GETPARM-E-DATATYPE Invalid keyword datatype %c
20004	%%GETPARM-F-ARGUMENTS Unable to decipher.(%s - %d) (Add optional args) 
20006	%%GETPARM-E-NOTSUP Function (%s) not supported
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

int alpha_pfkey = 0;	/* This is an external global that tells GETPARM to treat the pfkey-mask as 4 byte alpha value. It is	*/
			/* normally treated as a 4 byte binary (2+2) which creates a problem if on a "little-Endian" (byte-swap)*/
			/* machines if the pfkey-mask is an alpha.								*/

extern void call_acucobol_error(int rc, int4 *wang_retcode, int4 *wang_compcode, char *link_filespec);

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
void GETPARM(va_alist)
va_dcl
{

	int done,i,j,arg_count;
	va_list	the_args;
	char *the_item;
	int4 *long_item;
	int *int_item;
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
	int	struct_type;							/* Flag-determine passed struct.		*/
	char	**arg_addr;							/* Ptr to array of addresses.			*/
	int	aa_ndx;								/* Address array index value.			*/
	char 	prid[8];							/* Local copy of program id.			*/
	FMTLIST	*fmtlist;							/* List to hold keyword/value pairs		*/
	int	labeled_prb;							/* Was a PRB found and was it labeled		*/
	int	chained_prb;							/* Was a PRB found that is chained		*/
	int	used_ok;							/* Are PRBs with USED status ok to return	*/

	/* 
	**	INITIALIZATION
	*/

	/* First time thru set nativecharmap flag */
	if (-1 == nativecharmap)
	{
		nativecharmap = 0;
		if (get_wisp_option("NATIVECHARMAP"))
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

	va_start(the_args); 							/* start getting items				*/
	arg_count = va_count(the_args);						/* How many args are there ?			*/
	done = arg_count;				  			/* Use done so we don't have to			*/
										/* get the count again.				*/
	va_start(the_args);							/* Reset the pointer.				*/

	/*
	**	GETPARM supports 2 types of calling sequence.
	**		1) The regular VSSUB argument list
	**		2) A special 2 argument sequence designed for calling it from C. The first arg is a pointer
	**			to an array of pointers the second arg is the number of arguments in the array.
	*/

	if (done == 2)								/* Is ptr to array of addresses			*/
	{									/* of args and ptr to # args.			*/
		struct_type = 1;
		arg_addr = (char **)va_arg(the_args, char*);			/* Get pointer to array of addr.		*/
		int_item = va_arg(the_args, int*);				/* Get pointer to # of params.			*/
		done = *int_item;
		aa_ndx = 0;							/* Start 1st element of array.			*/
	}
	else									/* else passed normal var args			*/
	{									/* structure.					*/
		struct_type = 0;
	}


	/*
	**	Get the GETPARM Header arguments
	**		1) TYPE
	**		2) FORM
	**		3) PRNAME
	*/

	if (struct_type)							/* get pointer to TYPE item			*/
	{
		the_item = (char *)arg_addr[aa_ndx++];
	}
	else the_item = va_arg(the_args, char*);
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
		werrlog(ERRORCODE(6),the_item,0,0,0,0,0,0,0);			/* Function not implemented.			*/
		return;
	}

	memcpy(the_type_str,the_item,2);

	if (struct_type)							/* get pointer to FORM item			*/
	{
		the_form = (char *)arg_addr[aa_ndx++];
	}
	else the_form = va_arg(the_args, char*);
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

	if (struct_type)							/* get pointer to PRNAME item			*/
	{
		the_name = (char *)arg_addr[aa_ndx++];
	}
	else the_name = va_arg(the_args, char*);
	--done;

	memset(prname,0,sizeof(prname));					/* prname is 9 chars null terminated.		*/
	strncpy(prname,the_name,8);


	wtrace("GETPARM","ENTER","Type=[%2.2s] Form=[%c] Prname=[%8.8s]", the_type_str ,*the_form ,prname);


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
				curr_prb = get_prb_id(last_prb_id);		/* Get the PRB based on the ID			*/
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
			curr_prb = get_prb_area(prname,NULL,used_ok);		/* Search for a PRB with this PRNAME		*/

			if (!curr_prb) break;					/* No PRB found					*/

			if (curr_prb->status == P_OK) break;			/* We found a valid PRB.			*/

			if (curr_prb->status == P_USED)
			{
				if (the_type == RESPECIFY_DEFAULT) break;	/* For "RD" a used PRB is accepted		*/
			}

			if (curr_prb->status == P_DELETED)			/* If PRB marked DELETED then 			*/
			{
				wax_table_entry(curr_prb);			/* Remove the PRB entry from pr_table		*/
			}
		}
	}

	if (curr_prb && (the_type == INITIAL || the_type == INITIAL_DEFAULT))
	{
		backwards_reference(&curr_prb);					/* Perform any backwards referencing needed	*/
	}

	labeled_prb = 0;							/* Assume no label PRB				*/
	chained_prb = 0;
	if (curr_prb)
	{
		if (curr_prb->label[0] && curr_prb->label[0] != ' ')
		{
			labeled_prb = 1;					/* We found a labeled PRB (Updated PRB values)	*/

			if (get_chained_prb(curr_prb))				/* Check if this is a chained putparm		*/
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

	if (struct_type)							/* get pointer to PFKEY item			*/
	{
		pf_ret = (char *)arg_addr[aa_ndx++];
	}
	else pf_ret = va_arg(the_args, char*);
	--done;

	if (struct_type)							/* get pointer to ID item			*/
	{
		messid = (char *)arg_addr[aa_ndx++];
	}
	else messid = va_arg(the_args, char*);
	--done;

	if (struct_type)							/* get pointer to ISSUER item			*/
	{
		messiss = (char *)arg_addr[aa_ndx++];
	}
	else messiss = va_arg(the_args, char*);
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

	memcpy(prid,getprogid(),8);						/* Copy prog id from global to local.		*/
	strncpy(templine+strlen(templine),prid,8);
	gdisp_put(4,23,PLAIN_TEXT,templine);
	sprintf(templine,"----------------------------------------");
	strcat(templine,"---------------------------------------");
	gdisp_put(6,2,PLAIN_TEXT,templine);

	/*
	**	ARG7 is optional
	*/

	if (struct_type)							/* get pointer to next item			*/
	{
		the_item = (char *)arg_addr[aa_ndx++];
	}
	else the_item =  va_arg(the_args, char*);

	/* NOTE: If Little-Endian and noswap_words and arg7 is not present and Arg8 is only 1 byte int4 then 			*/
	/*	 the test will likely fail. -- Arg8 MUST be made longer then 1 byte.						*/

	if (longargtest(the_item,1))
	{
		main_arg7 = 1;
		long_item = (int4 *)the_item;					/* the number of lines in mess			*/
		messlines = get_swap(long_item);
		--done;
		messlines_save = messlines;

		while (messlines)						/* get each line of text			*/
		{
			if (struct_type)					/* get ptr to MESSAGE TEXT item			*/
			{
				messtxt = (char *)arg_addr[aa_ndx++];
			}
			else messtxt = va_arg(the_args, char*);
			--done;

			if (struct_type)					/* get ptr to TEXT LENGTH item			*/
			{
				long_item = (int4 *)arg_addr[aa_ndx++];
			}
			else long_item = va_arg(the_args, int4*);
			--done;

			messlen = get_swap(long_item);
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

		messlines_save = 0;
		messtxt = the_item;						/* the message text itself			*/
		--done;								/* One less arg.				*/

		if (struct_type)						/* get ptr to TEXT LENGTH item			*/
		{
			long_item = (int4 *)arg_addr[aa_ndx++];
		}
		else long_item = va_arg(the_args, int4*);
		--done;

		messlen = get_swap(long_item);

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
                                          
	while (done != 0)							/* loop till nul ptr or all args are processed	*/
        {

		if (struct_type)						/* get pointer to next item			*/
		{
			sptype = (char *)arg_addr[aa_ndx++];
		}
		else sptype = va_arg(the_args, char*);
		--done;

		switch (*sptype)						/* process each type differently 		*/
		{
		default:

			werrlog(ERRORCODE(4),"SPTYPE",*sptype,0,0,0,0,0,0);	/* Report the args are messed-up		*/
			wexit(ERRORCODE(4));
			break;

		case 'K': case 'k':	/* STANDARD KEYWORD FIELD	*/
		case 'R': case 'r':	/* RESPECIFY KEYWORD FIELD	*/

			{							/* process keyword field    			*/
				int	force_uppercase;

			/*2*/	if (struct_type)				/* get ptr to KEYWORD NAME item			*/
				{
					spkey = (char *)arg_addr[aa_ndx++];
				}
				else spkey = va_arg(the_args, char*);
				--done;

			/*3*/	if (struct_type)				/* get ptr to KEYWORD VALUE item		*/
				{
					spval = (char *)arg_addr[aa_ndx++];
				}
				else spval = va_arg(the_args, char*);
				--done;

			/*4*/	if (struct_type)				/* get ptr to KW VAL LENGTH item		*/
				{
					long_item = (int4 *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, int4*);
				splen = get_swap(long_item);
				--done;

			/*5 OPTIONAL */	
				if (struct_type)				/* get ptr to ROW FLAG item			*/
				{
					sprf = (char *)arg_addr[aa_ndx++];
				}
				else sprf = va_arg(the_args, char*);
				--done;

				if (*sprf == 'A' || *sprf == 'R')
				{
			/*6*/		if (struct_type)			/* get ptr to ROW VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else
				{
					long_item = (int4 *)sprf;
					sprf = "R";
				}
				sprow = get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(ERRORCODE(4),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
				}

			/*7 OPTIONAL */	
				if (struct_type)				/* get ptr to COLUMN FLAG item			*/
				{
					the_item = (char *)arg_addr[aa_ndx++];
				}
				else the_item = va_arg(the_args, char*);
				--done;

				arg9=1;						/* Get arg9					*/
				arg7=0;						/* arg7 not present				*/
				if (*the_item=='R') arg7=1;			/* arg7 was present				*/
				if (*the_item=='A' || *the_item=='C' || *the_item=='J') 
					arg7 = -1;				/* arg7 likely was present			*/
				if (arg7 == -1 && bytenormal()) 
					arg7=1;
				if (arg7)
				{
					spcf = the_item;
			/*8*/		if (struct_type)			/* get ptr to COLUMN VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else	
				{
					long_item = (int4 *) the_item;
					spcf = "R";
				}

				long_temp = get_swap(long_item);

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
			/*9*/		if (struct_type)			/* get ptr to DATA TYPE item			*/
					{
						spdtype = (char *)arg_addr[aa_ndx++];
					}
					else spdtype = va_arg(the_args, char*);
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
					werrlog(ERRORCODE(4),"SPDTYPE",*spdtype,0,0,0,0,0,0);
					wexit(ERRORCODE(4));
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
					found_keyword = search_parm_area(templine,spkey,splen,curr_prb);

					if (chained_prb)
					{
						/*
						**	If this is a chained putparm then we will search the parm area
						**	of each prb on the chain for the keyword.
						*/

						SHMH	*temp_prb;

						temp_prb = curr_prb;

						while (temp_prb = get_chained_prb(temp_prb))
						{
							if (search_parm_area(templine,spkey,splen,temp_prb))
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
							upper_string(templine);
						}

						gdisp_put(row,column,keyfieldfac,templine);	/* output the line		*/
					}
				}
				break;
			}

		case 'T': case 't':	/* PLAIN TEXT FIELD 		*/
		case 'U': case 'u':	/* UNDERLINE TEXT FIELD 	*/

			{							/* process Text field				*/
			/*2*/	if (struct_type)				/* get ptr to VALUE item			*/
				{
					spval = (char *)arg_addr[aa_ndx++];
				}
				else spval = va_arg(the_args, char*);
				--done;

			/*3*/	if (struct_type)				/* get ptr to VALUE LENGTH item			*/
				{
					long_item = (int4 *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, int4*);
				--done;
				splen = get_swap(long_item);

			/*4 OPTIONAL */	
				if (struct_type)				/* get ptr to ROW_FLAG item			*/
				{
					the_item = (char *)arg_addr[aa_ndx++];
				}
				else the_item = va_arg(the_args, char*);
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
			/*5*/		if (struct_type)			/* get ptr to ROW VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else	long_item = (int4 *)the_item;
				sprow = get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(ERRORCODE(4),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
				}

			/*6 OPTIONAL */	
				if (struct_type)				/* get ptr to COLUMN FLAG item			*/
				{
					the_item = (char *)arg_addr[aa_ndx++];
				}
				else the_item = va_arg(the_args, char*);
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
			/*7*/		if (struct_type)			/* get ptr to COLUMN VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else	long_item = (int4 *)the_item;
				spcol = get_swap(long_item);
				if ( spcol < 0 || spcol >80 )			/* Report the args are messed-up		*/
				{
					werrlog(ERRORCODE(4),"SPCOL",spcol,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
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
				if (struct_type) aa_ndx++;
				else the_item = va_arg(the_args, char*);
				--done;
				break;
			}
		case 'P':      
			{							/* process pfkey mask field			*/
				if (struct_type)				/* get ptr to VALUE item			*/
				{						/* (4 bytes)					*/
					spval = (char *)arg_addr[aa_ndx++];
				}
				else spval = va_arg(the_args, char*);
				--done;

				long_item = (int4*) spval;                                                           
				pfkey_mask = get_swap(long_item);
				if (alpha_pfkey && !bytenormal())
				{
					reversebytes((char *)&pfkey_mask,4);  	/* Correct alpha pfkey-mask			*/
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
	}
	else
	{
		if (curr_prb) 	
		{
			pf_ret[0] = curr_prb->pfkey;				/* Get the pfkey from the putparm		*/
		}
	        else
		{
			/* If no display and no putparm found then don't modify pf_ret. */
			/* enter_key_presspf_ret[0] = ENTER_KEY_PRESSED; */
		}
	}

	fmtlist = NULL;

	va_end(the_args);
	va_start(the_args);							/* start getting items				*/
	done = arg_count;							/* Reset to number of args.			*/
	if (done == 2)								/* Is ptr to array of addresses			*/
	{									/* of args and ptr to # args.			*/
		struct_type = 1;
		arg_addr = (char **)va_arg(the_args, char*);			/* Get pointer to array of addr.		*/
		long_item = va_arg(the_args, int4*);				/* Get pointer to # of params.			*/
		done = *long_item;
		aa_ndx = 0;							/* Start 1st element of array.			*/
	}
	else									/* else passed normal var args			*/
	{									/* structure.					*/
		struct_type = 0;
	}
	if (struct_type) aa_ndx++;						/* Skip ptr to TYPE item			*/
	else the_item = va_arg(the_args, char*);
	--done;
	if (struct_type) aa_ndx++;						/* Skip ptr to FORM item			*/
	else the_item = va_arg(the_args, char*);
	--done;
	if (struct_type) aa_ndx++;						/* Skip ptr to NAME item			*/
	else the_item = va_arg(the_args, char*);
	--done;
	if (struct_type) aa_ndx++;						/* Skip ptr to PFKEY item			*/
	else the_item = va_arg(the_args, char*);
	--done;
	if (struct_type) aa_ndx++;						/* Skip ptr to ID item				*/
	else the_item = va_arg(the_args, char*);
	--done;
	if (struct_type) aa_ndx++;						/* Skip ptr to ISSUER item			*/
	else the_item = va_arg(the_args, char*);
	--done;

	if (main_arg7)
	{
		if (struct_type)						/* get # MESSAGE LINES item			*/
		{
			long_item = (int4 *)arg_addr[aa_ndx++];
		}
		else long_item = va_arg(the_args, int4*);
		messlines = get_swap(long_item);
		--done;
                             
		while (messlines)						/* skip each line of text			*/
		{
			if (struct_type) aa_ndx++;				/* Skip ptr to TEXT item			*/
			else the_item = va_arg(the_args, char*);
			--done;
			if (struct_type) aa_ndx++;				/* Skip ptr to TEXT LENGTH item			*/
			else long_item = va_arg(the_args, int4*);
			--done;
			messlines--;
		}
	}
	else
	{
			if (struct_type) aa_ndx++;				/* Skip ptr to MESSQGE TEXT item		*/
			else the_item = va_arg(the_args, char*);
			--done;
			if (struct_type) aa_ndx++;				/* Skip ptr to TEXT LENGTH item			*/
			else long_item = va_arg(the_args, int4*);
			--done;
	}
	relative_row = 9 + messlines_save;					/* Start relative offset here.			*/
	relative_column = 2;							/* Ditto.					*/
	row = 0; column = 0;							/* Initialize row & column			*/
										/* keylist specifications			*/
	while (done != 0)							/* loop till all done with the			*/
	{									/* args.              				*/
		if (struct_type)						/* get ptr to next item				*/
		{
			sptype = (char *)arg_addr[aa_ndx++];
		}
		else sptype = va_arg(the_args, char*);
		--done;

		switch (*sptype)						/* process each type differently 		*/
		{                

		default:
			werrlog(ERRORCODE(4),"SPTYPE",*sptype,0,0,0,0,0,0);	/* Report the args are messed-up		*/
			wexit(ERRORCODE(4));
			break;

		case 'K': case 'k':	/* STANDARD KEYWORD FIELDS	*/
		case 'R': case 'r':	/* RESPECIFY KEYWORD FIELDS	*/

			{							/* process keyword field			*/
			/*2*/	if (struct_type)				/* get ptr to KEYWORD item			*/
				{
					spkey = (char *)arg_addr[aa_ndx++];
				}
				else spkey = va_arg(the_args, char*);
				--done;

			/*3*/	if (struct_type)				/* get ptr to KEYWORD VALUE item		*/
				{
					spval = (char *)arg_addr[aa_ndx++];
				}
				else spval = va_arg(the_args, char*);
				--done;

			/*4*/	if (struct_type)				/* get ptr to KW VAL LENGTH item		*/
				{
					long_item = (int4 *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, int4*);
				--done;
				splen = get_swap(long_item);

			/*5 OPTIONAL */	
				if (struct_type)				/* get ptr to ROW FLAG item			*/
				{
					sprf = (char *)arg_addr[aa_ndx++];
				}
				else sprf = va_arg(the_args, char*);
				--done;

				if (*sprf == 'A' || *sprf == 'R')
				{
			/*6*/		if (struct_type)			/* get ptr to ROW VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else
				{
					long_item = (int4 *)sprf;
					sprf = "R";
				}
				sprow = get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(ERRORCODE(4),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
				}

			/*7 OPTIONAL */	
				if (struct_type)				/* get ptr to COLUMN FLAG item			*/
				{
					the_item = (char *)arg_addr[aa_ndx++];
				}
				else the_item = va_arg(the_args, char*);
				--done;

				arg9=1;						/* Get arg9					*/
				arg7=0;						/* arg7 not present				*/
				if (*the_item=='R') arg7=1;			/* arg7 was present				*/
				if (*the_item=='A' || *the_item=='C' || *the_item=='J') arg7 = -1;/* arg7 likely was present	*/
				if (arg7 == -1 && bytenormal()) arg7=1;
				if (arg7)
				{
					spcf = the_item;
			/*8*/		if (struct_type)			/* get ptr to COLUMN VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else	
				{
					long_item = (int4 *) the_item;
					spcf = "R";
				}

				long_temp = get_swap(long_item);
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
			/*9*/		if (struct_type)			/* get ptr to DATA TYPE item			*/
					{
						spdtype = (char *)arg_addr[aa_ndx++];
					}
					else spdtype = va_arg(the_args, char*);
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
					werrlog(ERRORCODE(4),"SPDTYPE",*spdtype,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
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
						gdisp_get(row,column,spval,splen);	/* get the field			*/

						/*
						** 	If PRB and labeled then we have to updated the PRB with all these
						**	values so build a fmtlist.
						*/

						if (labeled_prb)
						{
							static FMTLIST	*p;

							if (!fmtlist)
							{
								p = fmtlist = (FMTLIST *)wcalloc(1,sizeof(FMTLIST));
							}
							else
							{
								p->next = (FMTLIST *)wcalloc(1,sizeof(FMTLIST));
								p = p->next;
							}
							p->next = NULL;
							memcpy(p->keyword,spkey,8);
							p->len = splen;
							p->value = (char *)wcalloc((int)(p->len+1),sizeof(char));
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
			/*2*/	if (struct_type)				/* get ptr to VALUE item			*/
				{
					spval = (char *)arg_addr[aa_ndx++];
				}
				else spval = va_arg(the_args, char*);
				--done;

			/*3*/	if (struct_type)				/* get ptr to VALUE LENGTH item			*/
				{
					long_item = (int4 *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, int4*);
				--done;
				splen = get_swap(long_item);

			/*4 OPTIONAL */	
				if (struct_type)				/* get ptr to ROW_FLAG item			*/
				{
					the_item = (char *)arg_addr[aa_ndx++];
				}
				else the_item = va_arg(the_args, char*);
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
			/*5*/		if (struct_type)			/* get ptr to ROW VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else          long_item = (int4 *)the_item;
				sprow = get_swap(long_item);
				if ( sprow < 0 || sprow >24 )			/* Report the args are messed-up		*/
				{
					werrlog(ERRORCODE(4),"SPROW",sprow,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
				}

			/*6 OPTIONAL */	
				if (struct_type)				/* get ptr to COLUMN FLAG item			*/
				{
					the_item = (char *)arg_addr[aa_ndx++];
				}
				else the_item = va_arg(the_args, char*);
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
			/*7*/		if (struct_type)			/* get ptr to COLUMN VALUE item			*/
					{
						long_item = (int4 *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, int4*);
					--done;
				}
				else	long_item = (int4 *)the_item;
				spcol = get_swap(long_item);
				if ( spcol < 0 || spcol >80 )			/* Report the args are messed-up		*/
				{
					werrlog(ERRORCODE(4),"SPCOL",spcol,0,0,0,0,0,0);	
					wexit(ERRORCODE(4));
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
				if (struct_type) aa_ndx++;
				else the_item = va_arg(the_args, char*);
				--done;
				break;
			}
		case 'P':
			{
				if (struct_type)				/* get ptr to PFKEY MASK item			*/
				{						/* (4 bytes)					*/
					spval = (char *)arg_addr[aa_ndx++];
				}
				else spval = va_arg(the_args, char*);
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
		update_prb(&curr_prb,fmtlist);

		if (chained_prb)
		{
			SHMH	*temp_prb;

			temp_prb = curr_prb;
			while ( temp_prb = get_chained_prb(temp_prb) )
			{
				update_prb(&temp_prb,fmtlist);
			}
		}

		free_fmtlist(fmtlist);
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
		use_prb(curr_prb);						/* Use this PRB.				*/

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
		**	field.   Then update_prb() and use_prb() can be made to follow
		**	the chain.
		*/
		if (chained_prb)
		{
			SHMH	*temp_prb;

			temp_prb = curr_prb;
			while ( temp_prb = get_chained_prb(temp_prb) )
			{
				use_prb(temp_prb);
			}
		}
#endif
	}

exit_getparm:

	va_end(the_args);							/* done with the list				*/

	cleanup_shrfil();
	gdisp_cleanup();

	return;
}

/*
	use_last_prb	This routine instructs GETPARM to use the last PRB that was used. (last_prb_id)
			It is only called before a GETPARM type="RD" to indicate that we want to update the PRB used
			by the last GETPARM of type "I " or "ID".
*/
int use_last_prb()
{
	use_last_prb_id = 1;
	return(0);
}

/*==============================================================================================================================*/

#define MAX_COB_GP_FIELDS	32	/* Max number of entry fields */
#define MAX_GP_FIELD_LEN	79	/* Max length of entry fields */

static char *local_screen = NULL;
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
	
	if (acu_cobol && nativescreens())
	{
		cob_fields = 0;
		
		local_screen = wmalloc(WSB_ROWS*WSB_COLS);
		memset(local_screen,' ', WSB_ROWS*WSB_COLS);

		cob_field_list = wmalloc(sizeof(cob_gp_field)*MAX_COB_GP_FIELDS);
		memset(cob_field_list, ' ', sizeof(cob_gp_field)*MAX_COB_GP_FIELDS);

		for(i=0; i<MAX_COB_GP_FIELDS;i++)
		{
			/* This is a empty spot on a GETPARM screen */
			cob_field_list[i].row =  2;
			cob_field_list[i].col = 79;
			cob_field_list[i].len =  1;
		}
	}
	else
	{
		local_screen = wmalloc(2000);
		wsc_init(local_screen,0,0);
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
	if (acu_cobol && nativescreens())
	{
		/* No action needed */
	}
	else
	{
		wpopscr();							/* Restore any saved screen.			*/
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
	if (acu_cobol && nativescreens())
	{
		int	len;
		
		len = strlen(val);		

		/*
		**	Validate the values
		*/
		if (row < 1 || row > WSB_ROWS)
		{
			wtrace("GETPARM","ROW","Invalid row value %d",row);
			row = 1;
		}
		if (col < 1 || col > WSB_COLS)
		{
			wtrace("GETPARM","COLUMN","Invalid column value %d",col);
			col = 1;
		}
		if (len > MAX_GP_FIELD_LEN)
		{
			wtrace("GETPARM","LENGTH","Invalid field length value %d",len);
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
				werrlog(104,"%%GETPARM-E-MAXFIELDS Max 32 modifiable fields",0,0,0,0,0,0,0);
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

		wput(local_screen, row, col, fac, str);
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
**			Call "WACUGETPARM" to display the screen.
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
	if (acu_cobol && nativescreens())
	{
		/*
		**	CALL "WACUGETPARM" USING
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

		/*
		**	Count number of keys in the key list
		*/
		key_cnt = 0;
		if (ptr = strchr(keylist,'X'))
		{
			*ptr = (char)0;
			key_cnt = strlen(keylist) / 2;
		}
		if (key_cnt < 1)
		{
			wtrace("GETPARM","KEYLIST","Invalid keylist [%s]",keylist);
			key_cnt = 1;
			strcpy(keylist,"00X");
		}

		parms[0] = local_screen;
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
		
		call_acucobol("WACUGETPARM", 6, parms, lens, &rc);

		if (rc)
		{
			int4	wrc, wcc;
			
			call_acucobol_error(rc, &wrc, &wcc, "WACUGETPARM");
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
			if (term_key < 0 || term_key > 32) 
			{
				wtrace("GETPARM","TERMKEY","Invalid term_key [%d]",term_key);
				term_key = 0;
			}
			
			*aid_char = ("@ABCDEFGHIJKLMNOPabcdefghijklmnop")[term_key];
		}
	}
	else
	{
		unsigned char	function, lines, term[2], no_mod[2];

		function = DISPLAY_AND_READ_ALTERED;
		lines = 24;

		wpushscr();							/* Push the old screen.				*/
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
	if (acu_cobol && nativescreens())
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
**	Revision 1.22  1997-12-18 20:58:40-05  gsl
**	Fix CHARMAP handling
**
**	Revision 1.21  1997-12-17 20:44:56-05  gsl
**	Add support for NATIVECHARMAP option
**
**	Revision 1.20  1997-10-21 10:15:32-04  gsl
**	Changed to use getprogid()
**	Changed to use get_swap()
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
**	Change to use wtrace()
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
