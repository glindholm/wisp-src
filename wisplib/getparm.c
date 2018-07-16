			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


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

#ifdef unix
#include <sys/types.h>
#include <sys/ipc.h>
#endif

#include <stdio.h>
#include <varargs.h>
#include <errno.h>

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#include "movebin.h"
#include "werrlog.h"
#include "wangkeys.h"
#include "wcommon.h"
#include "scnfacs.h"
#include "wshmem.h"
#include "wglobals.h"

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>

#define		INITIAL			0					/* type = "I "					*/
#define		INITIAL_DEFAULT		1					/* type = "ID"					*/
#define		RESPECIFY		2					/* type = "R "					*/
#define		RESPECIFY_DEFAULT	3					/* type = "RD"					*/

#define		FIRST_PFKEY	0x80000000

#define		ALL_KEYS	0xFFFFFFFF
#define		NO_KEYS		0x00000000

int	tag_prb();
char	*strchr();

static SHMH *curr_prb;								/* Ptr to PRB for current prname		*/
static int last_prb_id=0;							/* The last PRB used				*/
static int use_last_prb_id=0;							/* Use the last PRB used as in last_prd_id	*/

extern int rts_first;								/* Init the video drivers			*/

int alpha_pfkey = 0;	/* This is an external global that tells GETPARM to treat the pfkey-mask as 4 byte alpha value. It is	*/
			/* normally treated as a 4 byte binary (2+2) which creates a problem if on a "little-Endian" (byte-swap)*/
			/* machines if the pfkey-mask is an alpha.								*/


/*==============================================================================================================================*/
void GETPARM(va_alist)
va_dcl
{
#define		ROUTINE		20000

	int done,i,j,arg_count;
	va_list	the_args;
	char *the_item;
	long *long_item;
	int *int_item;
	char	keylist[80], templine[84], function, lines, term[2], no_mod[2];
	unsigned long	pfkey_mask;						/* the mask of possible pfkeys			*/
	unsigned long	key_num;
	int	the_type;							/* the type of getparm				*/
	char	*the_form;							/* the form					*/
	char	*the_name;							/* the name of the caller			*/
	char	*pf_ret;							/* the pfkey return field			*/
	char	*messid;							/* the message id				*/
	char	*messiss;							/* the message issuer				*/
	long	messlines;							/* the number of lines				*/
	long	messlines_save;							/* the number of lines				*/
	char	*messtxt;							/* the message text				*/
	long	messlen;							/* the text length				*/
	char	*sptype;							/* spec type					*/
	char	*spkey;								/* spec keyword					*/
	char	*spval;								/* spec string					*/
	long	splen;								/* length of the string				*/
	char	*sprf;								/* row flag					*/
       	long	sprow;	  							/* row value					*/
	char	*spcf;								/* collumn flag					*/
	long	spcol;								/* collumn value				*/
	char	*spdtype;							/* data type					*/
	char 	*screen, screen_mem[2000];					/* The screen pointer & memory (1924+fudge)	*/
	char    spaces[9];             						/* A string full of spaces.			*/
	int     relative_row, row, relative_column, column, old_row;		/* Used for relative locations.			*/
	int	keyfieldfac;							/* The fac translated for data type.		*/
	char	prname[9];
	int	found_keyword, display_flag, missing_word, enter_flagged;	/* 1 if screen display required			*/
	short	main_arg7;
	short 	arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9;
	long	long_temp;
	int	struct_type;							/* Flag-determine passed struct.		*/
	char	**arg_addr;							/* Ptr to array of addresses.			*/
	int	aa_ndx;								/* Address array index value.			*/
	char 	prid[8];							/* Local copy of program id.			*/
	int	label_prb;							/* Was a labled PRB found			*/
	prb_keyword_struct	keyword_list[100];				/* List of keywords to update PRB with		*/
	int	keyword_count;							/* Number of keywords				*/
	int	labeled_prb;							/* Was a PRB found and was it labeled		*/
	int	keep_ok;							/* Are PRBs with KEEP status ok to return	*/

	/* 
	**	INITIALIZATION
	*/
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);					/* Say we are here.				*/

	screen = NULL;
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


	/*
	**	Search for a PRB that matches the PRNAME for this GETPARM.
	*/

	curr_prb = NULL;
	if (the_type == RESPECIFY_DEFAULT)
	{
		keep_ok = 1;							/* PRBs of type KEEP are ok.			*/

		if (use_last_prb_id)						/* If using last PRB				*/
		{
			if (last_prb_id)					/* We have a last PRB to use			*/
			{
				curr_prb = (SHMH *)get_prb_id(last_prb_id);	/* Get the PRB based on the ID			*/
			}

			if (!curr_prb)						/* If no PRB then nothing to do			*/
			{
				goto exit_getparm;				/* Bail out!					*/
			}
		}
	}
	else
	{
		keep_ok = 0;
	}
	use_last_prb_id = 0;

	if (!curr_prb && the_type != RESPECIFY)					/* For RESPECIFY don't look for a PRB		*/
	{
		for(;;)
		{
			curr_prb = (SHMH *)get_prb_area(prname,NULL,keep_ok);	/* Search for a PRB with this PRNAME		*/

			if (!curr_prb) break;					/* No PRB found					*/

			if (curr_prb->status == P_OK) break;			/* We found a valid PRB.			*/

			if (curr_prb->status == P_KEEP)
			{
				if (the_type == RESPECIFY_DEFAULT) break;	/* For "RD" a used PRB is accepted		*/
			}

			if (curr_prb->status == P_DELETED)			/* If PRB marked DELETED then 			*/
			{
				wax_table_entry((char *)curr_prb);		/* Remove the PRB entry from pr_table		*/
			}
		}
	}

	labeled_prb = 0;							/* Assume no label PRB				*/
	if (curr_prb)
	{
		if (curr_prb->label[0] && curr_prb->label[0] != ' ')
		{
			labeled_prb = 1;					/* We found a labeled PRB (Updated PRB values)	*/
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

	screen = screen_mem;							/* get the screen memory			*/

	wsc_init(screen,0,0);							/* init the screen				*/

	wput(screen,1,2,PLAIN_TEXT,"WISP GETPARM");
	wput(screen,1,15,PLAIN_TEXT,WISP_VERSION);
	memset(templine,0,sizeof(templine));
	sprintf(templine,"Parameter reference Name: %-s",prname);
	wput(screen,1,46,PLAIN_TEXT,templine);
	memset(templine,0,sizeof(templine));
	sprintf(templine,"Message Id: %04d",atoi(messid));
	wput(screen,2,60,PLAIN_TEXT,templine);
	memset(templine,0,sizeof(templine));
	strcpy(templine,"Component: ");
	strncpy(templine+strlen(templine),messiss,6);
	wput(screen,3,61,PLAIN_TEXT,templine);
	memset(templine,0,sizeof(templine));

	if (RESPECIFY == the_type)						/* The message is dependent on the type/form	*/
		strcpy(templine,"Correction Required by ");
	else if ('R' != *the_form)
		strcpy(templine,"Response Required by ");
	else
		strcpy(templine,"Information Required by ");

	memcpy(prid,WISPPROGID,8);						/* Copy prog id from global to local.		*/
	strncpy(templine+strlen(templine),prid,8);
	wput(screen,4,23,PLAIN_TEXT,templine);
	sprintf(templine,"----------------------------------------");
	strcat(templine,"---------------------------------------");
	wput(screen,6,2,PLAIN_TEXT,templine);

	/*
	**	ARG7 is optional
	*/

	if (struct_type)							/* get pointer to next item			*/
	{
		the_item = (char *)arg_addr[aa_ndx++];
	}
	else the_item =  va_arg(the_args, char*);

	/* NOTE: If Little-Endian and noswap_words and arg7 is not present and Arg8 is only 1 byte long then 			*/
	/*	 the test will likely fail. -- Arg8 MUST be made longer then 1 byte.						*/

	if (longargtest(the_item,1))
	{
		main_arg7 = 1;
		long_item = (long *)the_item;					/* the number of lines in mess			*/
		GETBIN(&messlines,long_item,4);
		--done;
		wswap(&messlines);						/* put words in correct order			*/
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
				long_item = (long *)arg_addr[aa_ndx++];
			}
			else long_item = va_arg(the_args, long*);
			--done;

			GETBIN(&messlen,long_item,4);
			wswap(&messlen);
			messlines--;

			memset(templine,0,sizeof(templine));
			strncpy(templine,messtxt,(int)messlen);
			wput(screen,(int)(6+messlines_save-messlines),4,PLAIN_TEXT,templine);
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
			long_item = (long *)arg_addr[aa_ndx++];
		}
		else long_item = va_arg(the_args, long*);
		--done;

		GETBIN(&messlen,long_item,4);
		wswap(&messlen);

		arg7_line = 6;
		memset(templine,0,sizeof(templine));
		for( i=0, j=0; i<=messlen;)
		{
			if ( messtxt[i] == 0x0d || i == messlen )		/* Break the line on a x0d 			*/
			{
				arg7_line += 1;
				messlines_save += 1;
				wput(screen,arg7_line,4,PLAIN_TEXT,templine);
				memset(templine,0,sizeof(templine));
				j = 0;
			}
			else
			{
				templine[j] = messtxt[i];
				j++;
			}
			i++;
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
					long_item = (long *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, long*);
				GETBIN(&splen,long_item,4);
				--done;
				wswap(&splen);

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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else
				{
					long_item = (long *)sprf;
					sprf = "R";
				}
				GETBIN(&sprow,long_item,4);
				wswap(&sprow);
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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else	
				{
					long_item = (long *) the_item;
					spcf = "R";
				}

				GETBIN(&long_temp,long_item,4);
				wswap(&long_temp);

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

				switch( *spdtype )
				{
				case 'A':
				case 'L':
				case 'U':
				case 'H':
					keyfieldfac = UPCASE_FIELD;
					break;
				case 'a':
				case 'l':
				case 'u':
				case 'h':
					keyfieldfac = UPPROT_FIELD;
					break;
				case 'C':
					keyfieldfac = STANDARD_FIELD;
					break;
				case 'c':
					keyfieldfac = PLAIN_TEXT;
					break;
				case 'N':
				case 'I':
					keyfieldfac = NUMERIC_FIELD;
					break;
				case 'n':
				case 'i':
					keyfieldfac = NUMPROT_FIELD;
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
	 					wput(screen,row,column,PLAIN_TEXT,templine);	/* put on the screen		*/
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
					found_keyword = search_parm_area(templine,spkey,spval,splen,(char *)curr_prb,0);
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
						wput(screen,row,column,keyfieldfac,templine);	/* output the line		*/
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
					long_item = (long *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, long*);
				--done;
				GETBIN(&splen,long_item,4);
				wswap(&splen);

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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else	long_item = (long *)the_item;
				GETBIN(&sprow,long_item,4);
				wswap(&sprow);
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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else	long_item = (long *)the_item;
				GETBIN(&spcol,long_item,4);
				wswap(&spcol);
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
					if (splen > 0)
					{
						wput(screen,row,column,PLAIN_TEXT,templine);	/* output the line		*/
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

				long_item = (long*) spval;                                                           
				GETBIN(&pfkey_mask,long_item,4);
				wswap(&pfkey_mask);				/* change order of words			*/
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

	function = 7;
	lines = 24;

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
		wpushscr();							/* Push the old screen.				*/
		vwang(&function,screen,&lines,keylist,term,no_mod);		/* display the screen				*/
		pf_ret[0] = no_mod[1];						/* Get the key value				*/
	}
	else
	{
		if (curr_prb) 	pf_ret[0] = curr_prb->pfkey;
	        else	  	pf_ret[0] = ENTER_KEY_PRESSED;			/* Fake the key value				*/
	}									/* return the values				*/

	keyword_count = 0;

	va_end(the_args);
	va_start(the_args);							/* start getting items				*/
	done = arg_count;							/* Reset to number of args.			*/
	if (done == 2)								/* Is ptr to array of addresses			*/
	{									/* of args and ptr to # args.			*/
		struct_type = 1;
		arg_addr = (char **)va_arg(the_args, char*);			/* Get pointer to array of addr.		*/
		long_item = va_arg(the_args, long*);				/* Get pointer to # of params.			*/
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
			long_item = (long *)arg_addr[aa_ndx++];
		}
		else long_item = va_arg(the_args, long*);
		GETBIN(&messlines,long_item,4);
		--done;
		wswap(&messlines);
                             
		while (messlines)						/* skip each line of text			*/
		{
			if (struct_type) aa_ndx++;				/* Skip ptr to TEXT item			*/
			else the_item = va_arg(the_args, char*);
			--done;
			if (struct_type) aa_ndx++;				/* Skip ptr to TEXT LENGTH item			*/
			else long_item = va_arg(the_args, long*);
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
			else long_item = va_arg(the_args, long*);
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
					long_item = (long *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, long*);
				--done;
				GETBIN(&splen,long_item,4);
				wswap(&splen);

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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else
				{
					long_item = (long *)sprf;
					sprf = "R";
				}
				GETBIN(&sprow,long_item,4);
				wswap(&sprow);
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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else	
				{
					long_item = (long *) the_item;
					spcf = "R";
				}

				GETBIN(&long_temp,long_item,4);
				wswap(&long_temp);
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
						wfget(screen,row,column,spval,splen);	/* get the field			*/

						/*
						** 	If PRB and labeled then we have to updated the PRB with all these
						**	values so build a table of keyword=value items.
						**	NOTE: We are using pointers into the parameter list (spkey,spval).
						*/

						if (labeled_prb)
						{
							keyword_list[keyword_count].keyword = spkey;
							keyword_list[keyword_count].len = splen;
							keyword_list[keyword_count].value = spval;
							keyword_count++;
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
					long_item = (long *)arg_addr[aa_ndx++];
				}
				else long_item = va_arg(the_args, long*);
				--done;
				GETBIN(&splen,long_item,4);
				wswap(&splen);

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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else          long_item = (long *)the_item;
				GETBIN(&sprow,long_item,4);
				wswap(&sprow);
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
						long_item = (long *)arg_addr[aa_ndx++];
					}
					else long_item = va_arg(the_args, long*);
					--done;
				}
				else	long_item = (long *)the_item;
				GETBIN(&spcol,long_item,4);
				wswap(&spcol);
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

	if (labeled_prb && keyword_count)					/* Update the labeled PRB with new values	*/
	{
		update_prb_list(&curr_prb,keyword_count,keyword_list);

/* make sure that this doesn't screw up the update_prb() stuff for wfopen(). The tag_prb() may need to be ajusted.		*/
/* It looks like it should be OK because the TAG takes place later BUT need to update curr_prb for the tag_prb().		*/
	}

	if (display_flag)
	{
		wpopscr();							/* Restore any saved screen.			*/
		synch_required = ede_synch;					/* Synch. not needed unless EDE			*/
	}

	/*
	**	If a PRB was found then "use" it up (except for type "RD" which doesn't use-up PRBs).
	*/

	if (curr_prb && the_type != RESPECIFY_DEFAULT)
	{
		delete_parm(prname,NULL,(char *)curr_prb);			/* Attempt to delete the area.			*/
	}

exit_getparm:

	va_end(the_args);							/* done with the list				*/

	cleanup_shrfil();

	return;
}

/*
	use_last_prb	This routine instructs GETPARM to use the last PRB that was used. (last_prb_id)
			It is only called before a GETPARM type="RD" to indicate that we want to update the PRB used
			by the last GETPARM of type "I " or "ID".
*/
use_last_prb()
{
	use_last_prb_id = 1;
}

/*==============================================================================================================================*/
static wfget(screen,row,col,val,len)						/* get a field from the screen	*/
char	*screen;								/* pointer to screen structure	*/
int	row;									/* the row to start		*/
int	col;									/* the column			*/
char	*val;									/* the place to store the value	*/
long	len;									/* the size of the area		*/
{
 	int i;
              
	i = ((row-1) * 80) + (col-1) + 4;					/* location in screen map	*/
	memcpy(val,&screen[i],(int)len);					/* copy the data		*/
}

