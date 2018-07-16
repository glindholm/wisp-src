			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* 		These routines are used for access to the various personality files for the WISP run-time library		*/

#define INIT_WPERSON

#include <stdio.h>
#include <ctype.h>
#include <v/video.h>
#include "wperson.h"								/* get the structure definitions		*/
#include "werrlog.h"
#include "wdefines.h"
#include "wglobals.h"

#ifdef VMS
#include <descrip.h>
#include <jpidef.h>
#include <ssdef.h>
#endif

#ifndef VMS	/* unix and MSDOS */
#include <sys/types.h>
#include <sys/stat.h>
#include <malloc.h>
#endif

char *getenv();
char *wanguid3();

static char curr_progvol[7] = "      ";						/* The current in memory PROGVOL value		*/
static char curr_proglib[9] = "        ";					/* The current in memory PROGLIB value		*/

static int  paths_built = 0;
static char person_path[80];
static char parent_path[80];
static char temp_person_path[80];
static char system_person_path[80];

static char ttmap_path[80];
static char pqmap_path[80];
static char lpmap_path[80];
static char lgmap_path[80];
static char scmap_path[80];
static char forms_path[80];
static char prmap_path[80];
static char options_path[80];

int opt_max_pages = 0;								/* Max prb pages				*/
int opt_max_parms = 0;								/* Max prb parms				*/

#ifndef VMS	/* unix && MSDOS */
static logical_id	*logical_ptr;
static scmap_id 	*scmap_ptr;
static forms_id 	*forms_ptr;
static prmap_id 	*prmap_ptr;
extern int PGRPID;
#endif	/* unix && MSDOS */

#ifdef VMS
#define LIB$K_CLI_GLOBAL_SYM	2
static char	constr[sizeof(usr_defaults)+1];					/* A string to hold the usage constants.	*/
static $DESCRIPTOR(sym,"$W_USAGE_CONSTANTS");					/* The descriptor of the usage constant symbol.	*/
static $DESCRIPTOR(conres,constr);						/* A descriptor to hold the constant string.	*/
#endif

static int loaded = 0;								/* flag to indicate info is loaded		*/
#define		ROUTINE		83000
void wpload()									/* routine to load in term info and user defaults*/
{										/* this routine must be called before accesing 	*/
										/* any terminal info and the user constants	*/
	FILE *the_file;								/* a file pointer				*/
	char inline[132],tstr[32],lptstr1[80],lptstr2[80];
	char *scn_ptr, *prm_ptr, tempc, *ptr;
	int flag, i;
	prt_id	*prt_ptr;							/* Pointer to printer list structure		*/
	int	config,lt_dev;
	int	pnum;								/* Printer number				*/
	char	buff[256];

#ifdef VMS
	term_id 	*term_ptr;
	lat_id 		*lat_ptr;
	pq_id		*pq_ptr;
#endif

#ifndef VMS	/* unix and MSDOS */
	if ( ! PGRPID ) PGRPID = wgetpgrp();
#endif

	if ( !paths_built ) build_config_paths();

	if (loaded) return;							/* already done					*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);
	werrlog(ERRORCODE(3),0,0,0,0,0,0,0,0);

#ifdef VMS

	/*
	**
	**	TTMAP		- Terminal definitions
	**
	*/

	term_list = 0;								/* initialize the list				*/
	lat_list = 0;

	the_file = fopen(ttmap_path,"r");					/* first load the terminal definitions		*/

	if (the_file)								/* no error opening file			*/
	{
		if (fgets(inline,132,the_file))   do
		{
			scn_ptr = &inline[0];

			flag = 1;						/* switch to parse name, number			*/

			do							/* scan the line and extract the parms		*/
			{							/* skip over spaces, commas, newlines , tabs	*/
				if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
				{
					scn_ptr++;
				}
				else							/* copy this parm			*/
				{
					if (flag == 1)					/* copy term name			*/
					{
						flag = 2;				/* set the flag				*/
						prm_ptr = tstr;				/* load terminal name			*/
						*prm_ptr = *scn_ptr++;			/* get first char			*/
						if (*prm_ptr != '_')			/* have to put in a leading '_'		*/
						{
							tempc = *prm_ptr;		/* save char				*/
							*prm_ptr++ = '_';		/* put in '_'				*/
							*prm_ptr = tempc;		/* restore char				*/
						}
						prm_ptr++;				/* next char position			*/

						do
						{					/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						prm_ptr--;				/* need to look at last character	*/
						if (*prm_ptr != ':')			/* has to end with a colon		*/
						{
							prm_ptr++;			/* add one on...			*/
							*prm_ptr = ':';
						}
						prm_ptr++;				/* point to end				*/
						*prm_ptr = 0;				/* end with a null			*/

						if (!strncmp(tstr,"_LTA0:",6)) 		/* Is it a pseudo LT device?		*/
						{
							lt_dev = 1;
							alloc_lat(&lat_ptr);		/* Get mem for next lat device.		*/
						}
						else
						{
							lt_dev = 0;
							alloc_term(&term_ptr);
							strcpy(term_ptr->termname,tstr);	/* Save the name.		*/
						}
					}
					else if (flag == 2)				/* copy term number			*/
					{
						i = 0;
						do
						{					/* copy till no digits			*/
							i *= 10;
							i += (*scn_ptr++) - '0';
						} while (isdigit(*scn_ptr));		/* stop when no digits			*/

						if (lt_dev)				/* Is it a LAT device?			*/
						{

							lat_ptr->termnum = i;		/* Save the LAT term number.		*/
							prm_ptr = lat_ptr->latname;	/* point to where the name will go.	*/

							do 
							{
								scn_ptr++;		/* Skip over whitespace			*/
							}
							while ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\t'));

							while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
							      && (*scn_ptr != '\t'))
							{				/* copy till next whitespace		*/
								*prm_ptr++ = *scn_ptr++;
							}
							*prm_ptr = '\0';		/* Null terminate			*/
						}
						else
						{
							term_ptr->termnum = i;
						}

						flag = 0;				/* clear the flag			*/

						if (strpos(scn_ptr,"KEYPAD_OFF") != -1)		/* Set the keypad off bit	*/
						{
							flag |= KEYPAD_OFF;
						}

						if (strpos(scn_ptr,"SETUP_CORRECTLY") != -1)	/* User did it bit on.		*/
						{
							flag |= SETUP_CORRECTLY;
						}

						if (strpos(scn_ptr,"SPACES_BLINK") != -1)	/* Set the keypad off bit	*/
						{
							flag |= SPACES_BLINK;
						}

						if (strpos(scn_ptr,"PRO_RT") != -1)		/* Special Pro/RT-11 system.	*/
						{
							flag |= PRO_RT;
						}

						if (lt_dev) lat_ptr->flags = flag;
						else	    term_ptr->flags = flag;

						flag = 0;
					}
				}
			} while (*scn_ptr && flag);					/* Till a null or flag is clear.	*/
		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the term file			*/
	}
	else
	{										/* error opening the file		*/
		werrlog(ERRORCODE(8),WISP_TERMINAL_FILE,0,0,0,0,0,0,0);
		sprintf(buff,"%%WPERSON-F-FACCESS Unable to open file %s",ttmap_path);
		werrvre(buff);
	}
#endif	/* VMS */

	/*
	**
	**	LPMAP		printer definitions
	**
	*/

	prt_list = 0;									/* initialize the list			*/
	the_file = fopen(lpmap_path,"r");						/* now load the printer definitions	*/

	if (the_file)									/* no error opening file		*/
	{
		if (fgets(inline,132,the_file))   do
		{
			if (!prt_list)							/* first time?				*/
			{
				prt_list = (prt_id *)malloc(sizeof(prt_id));		/* get some memory			*/
				if (!prt_list)
				{
					werrlog(ERRORCODE(6),"prt_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prt_list->next = 0;					/* set next pointer to zero		*/
				prt_ptr = prt_list;					/* set up local pointer			*/
			}
			else
			{
				prt_ptr->next = (struct prt_id *)malloc(sizeof(prt_id));/* get some memory			*/
				if (!prt_ptr->next)
				{
					werrlog(ERRORCODE(6),"prt_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prt_ptr = (prt_id *)prt_ptr->next;			/* set pointer				*/
				prt_ptr->next = 0;					/* set next pointer to zero		*/
			}
#ifdef VMS
			scn_ptr = &inline[0];

			flag = 1;							/* switch to parse class, printer	*/
			*lptstr1 = '\0';						/* Initialize strings for number, qname	*/
			*lptstr2 = '\0';

			do								/* scan the line and extract the parms	*/
			{							/* skip over spaces, commas, newlines , tabs	*/
				if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
				{
					scn_ptr++;
				}
				else							/* copy this parm			*/
				{
					if (flag == 1)					/* copy class letter			*/
					{
						flag = 2;				/* set the flag				*/
						prt_ptr->class = *scn_ptr++;		/* get first char			*/
					}
					else if (flag == 2)
					{
						flag = 3;				/* set the flag				*/
						prm_ptr = lptstr1;			/* load next string			*/
						do
						{					/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						*prm_ptr = '\0';			/* null terminate			*/
						if ((*scn_ptr == '\n') || (*scn_ptr == '\0')) flag = 0;	/* signal we are done	*/
					}
					else if (flag == 3)
					{
						prm_ptr = lptstr2;			/* load next string			*/
						do
						{					/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						*prm_ptr = '\0';			/* null terminate			*/
						flag = 0;				/* signal we are done			*/
					}
					if (flag == 0)
					{
						if (*lptstr2 == '\0')			/* then lptstr1 is the qname		*/
						{
							prt_ptr->prtnum = 0;		/* Default to printer number 0.		*/
							prm_ptr = prt_ptr->qname;	/* load queue name			*/
							ptr = lptstr1;			/* ...from second string on the line.	*/
							do
							{
								*prm_ptr++ = *ptr++;
							} while (*ptr);
						}
						else
						{
							pnum = atoi(lptstr1);		/* Obtain the printer number		*/
											/* Error in atoi will default pnum = 0	*/
							prt_ptr->prtnum = pnum;		/* Load printer number			*/
							prm_ptr = prt_ptr->qname;	/* load queue name			*/
							ptr = lptstr2;			/* ...from third string on the line.	*/
							do
							{
								*prm_ptr++ = *ptr++;
							} while (*ptr);
						}
					}
				}
			} while (*scn_ptr && flag);					/* till a null or flag is clear		*/
#endif	/* VMS */

#ifndef VMS	/* unix and MSDOS */
			if ( (int)strlen(inline) > 82 )
			{
				werrlog(ERRORCODE(16),0,0,0,0,0,0,0);
				werrlog(102,inline,0,0,0,0,0,0,0);			/* Display the text.			*/
				wexit(ERRORCODE(16));
			}
			inline[strlen(inline)-1] = '\0';				/* remove trailing NL			*/
			prt_ptr->class = inline[0];					/* Load the class			*/
			strcpy( prt_ptr->prt_string, &inline[2] );			/* Get lp control string		*/

#endif	/* unix and MSDOS */

		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the term file			*/
	}
	else
	{										/* error opening the file		*/
#ifdef VMS
		werrlog(ERRORCODE(8),WISP_PRINTER_FILE,0,0,0,0,0,0,0);
		sprintf(buff,"%%WPERSON-F-FACCESS Unable to open file %s",lpmap_path);
		werrvre(buff);
#endif
	}

#ifdef VMS
	/*
	**
	**	PQMAP			Procedure Queues
	**
	*/

	pq_list = 0;									/* initialize the list			*/
	the_file = fopen(pqmap_path,"r");						/* now load the procedure definitions	*/

	if (the_file)									/* no error opening file		*/
	{
		if (fgets(inline,132,the_file))   do
		{
			if (!pq_list)							/* first time?				*/
			{
				pq_list = (pq_id *)malloc(sizeof(pq_id));		/* get some memory			*/
				if (!pq_list)
				{
					werrlog(ERRORCODE(6),"pq_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				pq_list->next = 0;					/* set next pointer to zero		*/
				pq_ptr = pq_list;					/* set up local pointer			*/
			}
			else
			{
				pq_ptr->next = (struct pq_id *)malloc(sizeof(pq_id));	/* get some memory			*/
				if (!pq_ptr->next)
				{
					werrlog(ERRORCODE(6),"pq_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				pq_ptr = (pq_id *)pq_ptr->next;				/* set pointer				*/
				pq_ptr->next = 0;					/* set next pointer to zero		*/
			}
			scn_ptr = &inline[0];
			flag = 1;							/* switch to parse class, printer	*/
			do								/* scan the line and extract the parms	*/
			{							/* skip over spaces, commas, newlines , tabs	*/
				if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
				{
					scn_ptr++;
				}
				else							/* copy this parm			*/
				{
					if (flag == 1)					/* copy class letter			*/
					{
						flag = 2;				/* set the flag				*/
						pq_ptr->class = *scn_ptr++;		/* get first char			*/
					}
					else if (flag == 2)
					{
						prm_ptr = pq_ptr->qname;		/* load queue name			*/
						do
						{					/* copy till next whitespace		*/
							*prm_ptr++ = *scn_ptr++;
						} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
						      && (*scn_ptr != '\t'));
						*prm_ptr = '\0';			/* null terminate			*/
						flag = 0;				/* signal we are done			*/
					}
				}
			} while (*scn_ptr && flag);					/* till a null or flag is clear		*/
		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the term file			*/
	}
	else
	{										/* error opening the file		*/
		werrlog(ERRORCODE(8),WISP_PROC_FILE,0,0,0,0,0,0,0);
		sprintf(buff,"%%WPERSON-F-FACCESS Unable to open file %s",pqmap_path);
		werrvre(buff);
	}
#endif	/* VMS */

#ifndef VMS	/* unix && MSDOS */
	/*
	**
	**	LGMAP
	**
	*/

	logical_list = 0;								/* initialize the list			*/
	config = 0;									/* CONFIG logical not found.		*/
	the_file = fopen(lgmap_path,"r");						/* now load the logicals 		*/

	if (the_file)									/* no error opening file		*/
	{
		char	lgmap_scan[40];

		sprintf(lgmap_scan,"%%6s %%%ds",MAX_TRANSLATE);

		if (fgets(inline,132,the_file))   do
		{
			get_logical_list_item( );					/* Set logical_ptr.			*/
			sscanf(inline, lgmap_scan, logical_ptr->logical, logical_ptr->translate );
			upper_string( logical_ptr->logical );

			if ( strcmp( logical_ptr->logical, "CONFIG" ) == 0 ) config = 1; /* CONFIG logical found.		*/

		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the logical file		*/
	}
	else
	{										/* error opening the file		*/
		werrlog(ERRORCODE(8),WISP_LOGICAL_FILE,0,0,0,0,0,0,0);
		sprintf(buff,"%%WPERSON-F-FACCESS Unable to open file %s",lgmap_path);
		werrvre(buff);
	}

	if ( ! config )								/* ADD 'CONFIG' to list of volumes.	*/
	{
		get_logical_list_item( );					/* Set logical_ptr.			*/
		strcpy( logical_ptr->logical, "CONFIG" );			/* VOLUME 'CONFIG'			*/
		if ( ptr = getenv( WISP_CONFIG_ENV ) )
			strcpy( logical_ptr->translate, ptr );			/* TRANSLATE $WISPCONFIG		*/
		else
			no_wispconfig();
	}
											/* ADD '.' to list of volumes. 		*/
	get_logical_list_item( );						/* Set logical_ptr.			*/
	strcpy( logical_ptr->logical, "." );					/* VOLUME '.'				*/
	strcpy( logical_ptr->translate, "." );					/* TRANSLATES '.'			*/

	/*
	**
	**	SCMAP		Submit Class
	**
	*/

	scmap_list = 0;									/* initialize the list			*/
	the_file = fopen(scmap_path,"r");						/* now load the scmap	 		*/

	if (the_file)									/* no error opening file		*/
	{
		if (fgets(inline,132,the_file))   do
		{
			if (!scmap_list)						/* first time?				*/
			{
				scmap_list = (scmap_id *)malloc(sizeof(scmap_id));	/* get some memory			*/
				if (!scmap_list)
				{
					werrlog(ERRORCODE(6),"scmap_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				scmap_list->next = 0;					/* set next pointer to zero		*/
				scmap_ptr = scmap_list;					/* set up local pointer			*/
			}
			else
			{
				scmap_ptr->next = (struct scmap_id *)malloc(sizeof(scmap_id));	/* get some memory	*/
				if (!scmap_ptr->next)
				{
					werrlog(ERRORCODE(6),"scmap_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				scmap_ptr = (scmap_id *)scmap_ptr->next;		/* set pointer				*/
				scmap_ptr->next = 0;					/* set next pointer to zero		*/
			}
			if ( (int)strlen(inline) > 84 )
			{
				werrlog(ERRORCODE(14),0,0,0,0,0,0,0);
				werrlog(102,inline,0,0,0,0,0,0,0);			/* Display the text.			*/
				wexit(ERRORCODE(14));
			}
			inline[strlen(inline)-1] = '\0';				/* remove trailing NL			*/
			scmap_ptr->class = toupper(inline[0]);				/* get the class			*/
			scmap_ptr->nice = atoi(&inline[2]);				/* convert nice to int			*/
		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the scmap file			*/
	}

	/*
	**
	**	FORMS
	**
	*/
	forms_list = 0;									/* initialize the list			*/
	the_file = fopen(forms_path,"r");						/* now load the forms	 		*/

	if (the_file)									/* no error opening file		*/
	{
		if (fgets(inline,132,the_file))   do
		{
			if (!forms_list)						/* first time?				*/
			{
				forms_list = (forms_id *)malloc(sizeof(forms_id));	/* get some memory			*/
				if (!forms_list)
				{
					werrlog(ERRORCODE(6),"forms_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				forms_list->next = 0;					/* set next pointer to zero		*/
				forms_ptr = forms_list;					/* set up local pointer			*/
			}
			else
			{
				forms_ptr->next = (struct forms_id *)malloc(sizeof(forms_id));	/* get some memory	*/
				if (!forms_ptr->next)
				{
					werrlog(ERRORCODE(6),"forms_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				forms_ptr = (forms_id *)forms_ptr->next;		/* set pointer				*/
				forms_ptr->next = 0;					/* set next pointer to zero		*/
			}
			if ( (int)strlen(inline) > 84 )
			{
				werrlog(ERRORCODE(14),0,0,0,0,0,0,0);
				werrlog(102,inline,0,0,0,0,0,0,0);			/* Display the text.			*/
				wexit(ERRORCODE(14));
			}
			inline[strlen(inline)-1] = '\0';				/* remove trailing NL			*/
			inline[3] = '\0';						/* null term after form#		*/
			forms_ptr->form_num = atoi(inline);				/* convert formnum to int		*/
			strcpy( forms_ptr->form_string, &inline[4] );			/* Get form control string		*/
		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the forms file			*/
	}

	/*
	**
	**	PRMAP 	Printer number map
	**
	*/
	prmap_list = 0;									/* initialize the list			*/
	the_file = fopen(prmap_path,"r");						/* now load the prmap	 		*/

	if (the_file)									/* no error opening file		*/
	{
		if (fgets(inline,132,the_file))   do
		{
			if (!prmap_list)						/* first time?				*/
			{
				prmap_list = (prmap_id *)malloc(sizeof(prmap_id));	/* get some memory			*/
				if (!prmap_list)
				{
					werrlog(ERRORCODE(6),"prmap_list",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prmap_list->next = 0;					/* set next pointer to zero		*/
				prmap_ptr = prmap_list;					/* set up local pointer			*/
			}
			else
			{
				prmap_ptr->next = (struct prmap_id *)malloc(sizeof(prmap_id));	/* get some memory	*/
				if (!prmap_ptr->next)
				{
					werrlog(ERRORCODE(6),"prmap_ptr->next",0,0,0,0,0,0,0);
					wexit(ERRORCODE(6));
				}
				prmap_ptr = (prmap_id *)prmap_ptr->next;		/* set pointer				*/
				prmap_ptr->next = 0;					/* set next pointer to zero		*/
			}
			if ( (int)strlen(inline) > 84 )
			{
				werrlog(ERRORCODE(14),0,0,0,0,0,0,0);
				werrlog(102,inline,0,0,0,0,0,0,0);			/* Display the text.			*/
				wexit(ERRORCODE(14));
			}
			inline[strlen(inline)-1] = '\0';				/* remove trailing NL			*/
			inline[3] = '\0';						/* null term after printer #		*/
			prmap_ptr->prmap_num = atoi(inline);				/* convert printer # to int		*/
			strcpy( prmap_ptr->prmap_string, &inline[4] );			/* Get printer control string		*/
		}while (fgets(inline,132,the_file));					/* while lines in file			*/

		fclose(the_file);							/* close the prmap file			*/
	}
#endif	/* unix && MSDOS */

	load_options();									/* Load the runtime options file.	*/

	wpl_usr(&defaults);								/* now load the user defaults		*/

	loaded = 1;									/* already loaded now			*/
}


#ifndef VMS	/* unix && MSDOS */
static	get_logical_list_item( )
{
	if (!logical_list)						/* first time?				*/
	{
		logical_list = (logical_id *)malloc(sizeof(logical_id));/* get some memory			*/
		if (!logical_list)
		{
			werrlog(ERRORCODE(6),"logical_list",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		logical_list->next = 0;					/* set next pointer to zero		*/
		logical_ptr = logical_list;				/* set up local pointer			*/
	}
	else
	{
		logical_ptr->next = (struct logical_id *)malloc(sizeof(logical_id));	/* get some memory	*/
		if (!logical_ptr->next)
		{
			werrlog(ERRORCODE(6),"logical_ptr->next",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		logical_ptr = (logical_id *)logical_ptr->next;		/* set pointer				*/
		logical_ptr->next = 0;					/* set next pointer to zero		*/
	}
}
#endif	/* unix  && MSDOS */

wpl_usr(the_def)									/* Load a user default personality.	*/
usr_defaults *the_def;
{
	if ( wpl_symbol(the_def) ) return(0);						/* First look for the symbol def.	*/
	wpl_file(the_def,"");								/* Otherwise, read the file.		*/
	return(1);
}
											/* save a user default personality	*/
wps_usr(the_def)
usr_defaults *the_def;
{
	wps_symbol(the_def);								/* First store in the symbol.		*/
}

int wpl_file(the_def,the_name)								/* Load the personality from the file.	*/
usr_defaults *the_def;
char *the_name;
{
	FILE *the_file;
	int amt, retfl;									/* Set flag if use defaults.		*/

	if ( ! paths_built ) build_config_paths();
	retfl = 0;									/* Initialize flag.			*/
	if (*the_name)
#ifdef VMS
		the_file = fopen(the_name,"r", "dna=*.dat;0");				/* Open the requested one.		*/
#else
		the_file = fopen(the_name,"r");						/* Open the requested one.		*/
#endif
	else the_file = fopen(person_path,"r");						/* try to load the local user's default	*/

	if (!the_file)									/* not found, go for the system file	*/
	{
		the_file = fopen(system_person_path,"r");				/* try to open it			*/
	}

	if (!the_file)									/* neither file found			*/
	{										/* set our defaults			*/
		retfl = 1;								/* Set flag to detect use of defaults.	*/
		the_def->prt_mode = 'S';						/* print mode is spooled		*/
		the_def->prt_class = 'A';						/* class is A				*/
		the_def->prt_num = 0;							/* printer number 0			*/
		the_def->prt_form = 0;							/* printer form number is 0		*/

		strcpy(the_def->inlib,"        ");					/* blank out library names		*/
		strcpy(the_def->outlib,"        ");
		strcpy(the_def->runlib,"        ");
		strcpy(the_def->spoolib,"        ");
		strcpy(the_def->proglib,"        ");

		genworklib(the_def->worklib);						/* Generate the worklib			*/
		genspoollib(the_def->spoolib);						/* Generate the spoolib			*/
		strcpy(the_def->invol,"      ");					/* blank out volume names		*/
		strcpy(the_def->outvol,"      ");
		strcpy(the_def->runvol,"      ");
		strcpy(the_def->spoolvol,"      ");
		strcpy(the_def->progvol,"      ");
		strcpy(the_def->workvol,"      ");
		genworkvol(the_def->workvol);						/* Generate the workvol			*/

		the_def->proc_stat = 'R';						/* Set procedure queue constants.	*/
		the_def->proc_class = 'A';
		strcpy(the_def->proc_cpu,"000000");
		the_def->kb_map[0] = '\0';						/* Clear keyboard map type name.	*/
		the_def->flags = 0x0FFFFFFFF;						/* All things are enabled.		*/

		the_def->pf_version = 0L;						/* Unset version.			*/
		wpl_valid( the_def );							/* Set version and the following:	*/

#if 0											/* These are set in wpl_valid():	*/

		the_def->psb_select = 'B';						/* Pseudo blank char is a blank.	*/
		the_def->psb_charset = 0;						/*  from the DEFAULT character set.	*/
		the_def->psb_rendition = 2;						/*  using an UNDERSCORE rendition..	*/
		the_def->prt_lines = 55;						/* printer default lines per page is 55	*/
		the_def->mp_cursor = TRUE;						/* Display cursor on menu pick flag	*/
		the_def->automove = FALSE;						/* Flag for auto_move default is FALSE.	*/
		the_def->autotab = TRUE;						/* Flag for auto_tab default is TRUE.	*/
#ifdef MSDOS
		the_def->bgchange = TRUE;						/* Change background by default.	*/
		the_def->bgcolor = TRUE;						/* Default background color is Grey.	*/
#else	/* unix or VMS */
		the_def->bgchange = FALSE;						/* Don't change background by default.	*/
		the_def->bgcolor = FALSE;						/* Default background color is black.	*/
#endif
		the_def->excolor = FALSE;						/* Exit with a black background.	*/
#endif	/* 0 */
	}
	else
	{
		fread(the_def,sizeof(usr_defaults),1,the_file);				/* read the info.			*/
		wpl_valid(the_def);							/* Validate the version info.		*/

		genworklib(the_def->worklib);						/* Generate the worklib			*/
		genworkvol(the_def->workvol);						/* Generate the workvol			*/
		genspoollib(the_def->spoolib);						/* Generate the spoollib		*/

		fclose(the_file);
	}

	strcpy(the_def->proglib,"        ");						/* Always reset PV/PL			*/
	strcpy(the_def->progvol,"      ");

	wps_symbol(the_def);								/* create the symbol.			*/

	return(retfl);
}
											/* save a user default personality	*/
wps_file(the_def,the_name)
usr_defaults *the_def;									/* NOTE the structure MUST be properly	*/
char *the_name;										/* loaded or there will be a problem!	*/
{
	FILE 	*the_file;
	int 	amt;
	char	*fptr;

	if ( ! paths_built ) build_config_paths();
	the_file = 0;
	if (*the_name) fptr = the_name;							/* Use the supplied name.		*/
	else           fptr = person_path;						/* Use the default.			*/

#ifdef VMS
	the_file = fopen(fptr,"w","dna=*.dat;0");					/* Open the file.			*/
#else
	the_file = fopen(fptr,"w");							/* Open the file.			*/
#endif
	if (!the_file)									/* Open failed.				*/
	{
		werrlog(ERRORCODE(10),fptr,0,0,0,0,0,0,0);
		return(-1);
	}
	amt = fwrite(the_def,sizeof(usr_defaults),1,the_file);				/* Write out the usage constants.	*/
	fclose(the_file);
	if (amt == 0)									/* Write failed.			*/
	{
		werrlog(ERRORCODE(12),fptr,0,0,0,0,0,0,0);
		return(-1);
	}
	return(0);
}

wpl_symbol(the_def)									/* Read the usage constants from the sym*/
usr_defaults *the_def;
{
#ifdef VMS
	long status;
	long len,tabtyp;

	tabtyp = LIB$K_CLI_GLOBAL_SYM;
	status = lib$get_symbol(&sym,&conres,&len,&tabtyp);				/* Get it's contents.			*/
	if (status == SS$_NORMAL)
	{
		memcpy(the_def,constr,sizeof(usr_defaults));
		wpl_valid(the_def);							/* Validate the version info.		*/
		genworklib(the_def->worklib);						/* Generate the worklib			*/
		genworkvol(the_def->workvol);						/* Generate the workvol			*/
		genspoollib(the_def->spoolib);						/* Generate the spoollib		*/
		return(1);								/* We were sucessfull.			*/
	}
	else
	{
		return(0);								/* Not sucessfull.			*/
	}
#endif	/* VMS */
#ifndef VMS	/* unix && MSDOS */
	return( get_person(the_def) );
#endif

}

wps_symbol(the_def)									/* Store the usage constants in the sym	*/
usr_defaults *the_def;
{
#ifdef VMS
	long status;
	long len,tabtyp;

	tabtyp = LIB$K_CLI_GLOBAL_SYM;
	memcpy(constr,the_def,sizeof(usr_defaults));					/* Copy them into the descriptor.	*/
	status = lib$set_symbol(&sym,&conres,&tabtyp);					/* Now set the global symbol.		*/
	if (status == SS$_NORMAL)
	{
		return(1);								/* We were sucessfull.			*/
	}
	else
	{
		return(0);								/* Not sucessfull.			*/
	}
#endif	/* VMS */
#ifndef VMS	/* unix && MSDOS */
	return( set_person(the_def) );
#endif
}

wferror(fname)
char *fname;
{
	int i;

	werrlog(ERRORCODE(8),fname,0,0,0,0,0,0,0);
	perror("\n%%WISP-E-RTLERR ");							/* Report last error.			*/
}

#ifndef VMS	/* unix && MSDOS */

static int get_person( the_def )					/* unix equv to LIB$GET_SYMBOL.				*/
usr_defaults *the_def;							/* reads the PERSONALITY## into memory			*/
{
	FILE	*fp;
	int	size;

	size = 0;
	if ( ! paths_built ) build_config_paths();
	if ( fp = fopen( temp_person_path, "r" ) )
	{
		size = fread( (char *)the_def, sizeof( *the_def ), 1, fp );
		fclose( fp );
	}
	if ( !size && wbackground() )					/* if temp not found && background copy from parent	*/
	{
		if ( fp = fopen( parent_path, "r" ) )
		{
			size = fread( (char *)the_def, sizeof( *the_def ), 1, fp );
			fclose( fp );
		}
	}
	if ( size )
	{
		genworklib(the_def->worklib);						/* Generate the worklib			*/
		genworkvol(the_def->workvol);						/* Generate the workvol			*/
		genspoollib(the_def->spoolib);						/* Generate the spoollib		*/
	}
	
	return( size );
}

static int set_person( the_def )					/* unix equv to LIB$SET_SYMBOL.				*/
usr_defaults *the_def;							/* Write the PERSONALITY## file from memory.		*/
{
	FILE	*fp;
	int	size;

	if ( ! paths_built ) build_config_paths();
	if ( fp = fopen( temp_person_path, "w" ) )
	{
		size = fwrite( (char *)the_def, sizeof( *the_def ), 1, fp );
		fclose( fp );
		chmod( temp_person_path, 0666 );
		return(size); 								/* size will eq 1 or 0			*/
	}
	return( 0 );
}

char *wforms(num)									/* return the string for form#		*/
int	num;
{
	forms_ptr = forms_list;
	while( forms_ptr )
	{
		if ( forms_ptr->form_num == num ) return( forms_ptr->form_string );
		forms_ptr = (forms_id *)forms_ptr->next;
	}

	return("");
}

char *getprmap(num)									/* return the string for printer #	*/
int	num;
{
	prmap_ptr = prmap_list;
	while( prmap_ptr )
	{
		if ( prmap_ptr->prmap_num == num ) return( prmap_ptr->prmap_string );
		prmap_ptr = (prmap_id *)prmap_ptr->next;
	}

	return("");
}

char *wlpclass(lpclass)									/* return the string for lpclass#	*/
char	lpclass;
{
	prt_id	*prt_ptr;

	prt_ptr = prt_list;
	while( prt_ptr )
	{
		if ( prt_ptr->class == lpclass ) return( prt_ptr->prt_string );
		prt_ptr = (prt_id *)prt_ptr->next;
	}

	return("");
}

ttyid5(tty)
char	*tty;
{
#ifdef MSDOS
	strcpy( tty, "conso" );
#endif

#ifdef unix
	char	*ptr;
	char	*path;
	struct stat buf;
	int	i;

	if ( wbackground() )
	{
		if ( ptr = getenv( WISP_TTY_ENV ) )
		{
			strcpy( tty, ptr );
			return;
		}
	}


	path = (char *)ttyname(0);
	if ( path )
	{
		path += strlen(path);
		tty  += 5;
		for(i=0; i <= 5; path--)
		{
			if (*path != '/')
			{
				*tty-- = *path;
				i++;
			}
		}
	}
	else
	{
		strcpy( tty, "ERROR" );
	}
#endif	/* #ifdef unix */

}

static build_config_paths()
{
/*
					For the following paths, assume "\\" for "/" and "C:\\TMP" for "/usr/tmp" on MSDOS:

	person_path			$HOME/$WISP_USER_PERSON_FILE
	temp_person_path		/usr/tmp/$WISP_TEMP_PERSON_PREFIX$TTY$UID		(forground unix)
					/usr/tmp/$WISP_TEMP_PERSON_PREFIX$UID$W_T_P_SUFFIX	(forground unix)
					/usr/tmp/$WISP_TEMP_PERSUB_PREFIX$WISP_PID_ENV		(background unix only)
	parent_path			/usr/tmp/$WISP_TEMP_PERSON_PREFIX$TTY$UID		(forground  unix only)
					/usr/tmp/$WISP_TEMP_PERSON_PREFIX$WISP_TTY_ENV$UID	(background unix only)
	system_person_path		$WISPCONFIG/$WISP_SYSTEM_PERSON_FILE
	forms_path			$WISPCONFIG/$WISP_FORMS_FILE
*/

	char	*ptr;
	char	temp[20];
	char	tty[10];
	char	uid[4];
	long	pid;

	if ( paths_built ) return(0);

	*person_path = '\0';
	*parent_path = '\0';
	*temp_person_path = '\0';
	*system_person_path = '\0';
	*ttmap_path = '\0';
	*lpmap_path = '\0';
	*pqmap_path = '\0';
	*scmap_path = '\0';
	*lgmap_path = '\0';
	*forms_path = '\0';
	*prmap_path = '\0';
	*options_path = '\0';

	if ( ptr = getenv( WISP_CONFIG_ENV ) )
	{
		strcpy( system_person_path, ptr );
		strcat( system_person_path, DSS );
	}
	else
	{
		no_wispconfig();
	}

	if ( ptr = getenv( WISP_HOME_ENV ) )
	{
		strcpy( person_path, ptr );
		strcat( person_path, DSS );
	}

#ifdef MSDOS

	else
	{
		strcpy( person_path, "C:\\" );					/* Environment Variable HOME not set, use root.	*/
	}

#endif	/* MSDOS */

	strcat( person_path, WISP_USER_PERSON_FILE );

	strcpy( parent_path, TMP_DIR );
	strcat( parent_path, DSS );
	strcat( parent_path, WISP_TEMP_PERSON_PREFIX );

	memcpy(uid,wanguid3(),3);
	if(	 uid[0] == ' ' )	uid[0] = '\0';
	else if( uid[1] == ' ' )	uid[1] = '\0';
	else if( uid[2] == ' ' )	uid[2] = '\0';
	else				uid[3] = '\0';

#ifdef MSDOS
	strcpy( temp_person_path, parent_path );
	strcat( temp_person_path, uid );
	strcat( temp_person_path, WISP_TEMP_PERSON_SUFFIX );
#endif

#ifdef unix
	ttyid5(tty);

	strcat( parent_path, tty );
	strcat( parent_path, uid );

	strcpy( temp_person_path, "/usr/tmp/" );

	if (wbackground())
	{
		strcat( temp_person_path, WISP_TEMP_PERSUB_PREFIX );
		if ( ptr = getenv( WISP_PID_ENV ) )
		{
			strcat( temp_person_path, ptr );
		}
		else /* WISP_PID_ENV not set */
		{
			pid = (long)getpid();
			sprintf(temp,"%06d",pid);
			strcat( temp_person_path, temp );
		}
	}
	else /* foreground */
	{
		strcat( temp_person_path, WISP_TEMP_PERSON_PREFIX );
		strcat( temp_person_path, tty );
		strcat( temp_person_path, uid );
	}
#endif	/* #ifdef unix */
	
	strcpy( ttmap_path, system_person_path );
	strcat( ttmap_path, WISP_TERMINAL_FILE );
	strcpy( lpmap_path, system_person_path );
	strcat( lpmap_path, WISP_PRINTER_FILE );
	strcpy( pqmap_path, system_person_path );
	strcat( pqmap_path, WISP_PROC_FILE );
	strcpy( scmap_path, system_person_path );
	strcat( scmap_path, WISP_SUBCLASS_FILE );
	strcpy( lgmap_path, system_person_path );
	strcat( lgmap_path, WISP_LOGICAL_FILE );
	strcpy( forms_path, system_person_path );
	strcat( forms_path, WISP_FORMS_FILE );
	strcpy( prmap_path, system_person_path );
	strcat( prmap_path, WISP_PRMAP_FILE );
	strcpy( options_path, system_person_path );
	strcat( options_path, WISP_OPTIONS_FILE );
	strcat( system_person_path, WISP_SYSTEM_PERSON_FILE );
	paths_built = 1;
}
#endif	/* unix  && MSDOS */

#ifdef VMS
static build_config_paths()
{
	strcpy( person_path, WISP_USER_PERSON_FILE );
	strcpy( parent_path, WISP_USER_PERSON_FILE );
	strcpy( system_person_path, WISP_SYSTEM_PERSON_FILE );
	strcpy( ttmap_path, WISP_TERMINAL_FILE );
	strcpy( lpmap_path, WISP_PRINTER_FILE );
	strcpy( pqmap_path, WISP_PROC_FILE );
	strcpy( options_path, WISP_OPTIONS_FILE );

	paths_built = 1;
}
#endif	/* VMS */

/* Validate the structure, and set up any initial values which aren't in the old structure versions.				*/

wpl_valid(the_def)
usr_defaults *the_def;
{
											/****************************************/
											/* This switch/case mechanism was	*/
											/* changed to an "if" structure due	*/
											/* to MSDOS porting: a switch can not	*/
											/* use a long int, it must be a short.	*/
											/* Since the version numbers are long,	*/
											/* ifs are needed instead of switches.	*/
											/****************************************/

	if ( the_def->pf_version != 61591 )						/* If before the version of 6/15/91	*/
	{
		if ( the_def->pf_version != 52490 )					/* If before the version of 5/24/90	*/
		{
			if ( the_def->pf_version != 103089 )				/* If before the version of 10/30/89	*/
			{
				if ( the_def->pf_version != 60889 )			/* If before the version of 6/08/89	*/
				{							/* Unrecognized version, set defaults.	*/
					the_def->kb_map[0] = '\0';			/* Clear keyboard map type name.	*/
				}
											/* If it's the version of 6/8/89	*/
				the_def->psb_select = 'B';				/* Use default characteristics for 	*/
				the_def->psb_charset = 0;				/*  pseudo blank.			*/
				the_def->psb_rendition = 2;
			}
											/* If it's the version of 10/30/89	*/
			the_def->prt_lines = 55;					/* printer default lines per page is 55	*/
			the_def->mp_cursor = TRUE;					/* Display cursor on menu pick flag	*/
			the_def->automove = FALSE;					/* Flag for auto_move default is FALSE.	*/
			the_def->autotab = TRUE;					/* Flag for auto_tab default is TRUE.	*/
		}
											/* If it's the version of 05/24/90	*/

#ifdef MSDOS
		the_def->bgchange = TRUE;						/* Change background by default.	*/
		the_def->bgcolor = TRUE;						/* Default background color is Grey.	*/
#else	/* unix or VMS */
		the_def->bgchange = FALSE;						/* Don't change background by default.	*/
		the_def->bgcolor = FALSE;						/* Default background color is black.	*/
#endif
		the_def->excolor = FALSE;						/* Exit with a black background.	*/
	}
											/* If it's the version of 06/15/91.	*/

/**************************************** MAKE SURE THIS VALUE IS CORRECT WHEN VERSIONS CHANGE **********************************/
	the_def->pf_version = 61591;							/* Set the file version.		*/
}

#ifdef VMS
alloc_term(term_ptr)
term_id **term_ptr;
{
	if (!term_list)									/* first time?				*/
	{
		term_list = (term_id *)malloc(sizeof(term_id));				/* get some memory			*/
		if (!term_list)
		{
			werrlog(ERRORCODE(6),"term_list",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		term_list->next = 0;							/* set next pointer to zero		*/
		term_list->termname[0] = '\0';
		term_list->termnum = 0;
		term_list->flags = 0;
		*term_ptr = term_list;							/* set up local pointer			*/
	}
	else
	{
		(*term_ptr)->next = (struct term_id *)malloc(sizeof(term_id));
		if (!(*term_ptr)->next)
		{
			werrlog(ERRORCODE(6),"term_ptr->next",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		*term_ptr = (term_id *)(*term_ptr)->next;					/* set pointer				*/
		(*term_ptr)->next = 0;							/* set next pointer to zero		*/
		(*term_ptr)->termname[0] = '\0';
		(*term_ptr)->termnum = 0;
		(*term_ptr)->flags = 0;
	}
}

alloc_lat(lat_ptr)
lat_id **lat_ptr;
{
	if (!lat_list)									/* first time?				*/
	{
		lat_list = (lat_id *)malloc(sizeof(lat_id));				/* get some memory			*/
		if (!lat_list)
		{
			werrlog(ERRORCODE(6),"lat_list",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		lat_list->next = 0;							/* set next pointer to zero		*/
		lat_list->latname[0] = '\0';
		lat_list->termnum = 0;
		lat_list->flags = 0;
		*lat_ptr = lat_list;							/* set up local pointer			*/
	}
	else
	{
		(*lat_ptr)->next = (struct lat_id *)malloc(sizeof(lat_id));
		if (!(*lat_ptr)->next)
		{
			werrlog(ERRORCODE(6),"lat_ptr->next",0,0,0,0,0,0,0);
			wexit(ERRORCODE(6));
		}
		*lat_ptr = (lat_id *)(*lat_ptr)->next;					/* set pointer				*/
		(*lat_ptr)->next = 0;							/* set next pointer to zero		*/
		(*lat_ptr)->latname[0] = '\0';
		(*lat_ptr)->termnum = 0;
		(*lat_ptr)->flags = 0;
	}
}
#endif	/* VMS */

static genworklib(worklib)
char	*worklib;
{
	char temp[20];
	long  pid;									/* Process id				*/
	char	*ptr;
#ifdef VMS
	unsigned status;
	unsigned short retlen;
	struct	{
		short unsigned int	buflen;						/* the length of the buffer		*/
		short unsigned int 	item_code;					/* the code for the request to GETDVI	*/
		char 			*bufptr;					/* a pointer to the buffer		*/
		short unsigned int	*retlen;					/* the return length of the buffer	*/
		long int 		endbuf;						/* the end of the buffer		*/
	} pidbuf;
#endif	/* VMS */

#ifdef unix
	if ( wbackground() )
	{
		if ( ptr = getenv( WISP_PID_ENV ) )
		{
			strcpy(temp,ptr);
			pid = atol(temp);
		}
		else
		{
			pid = getpid();
		}
	}
	else
	{

		if ( ! PGRPID ) PGRPID = wgetpgrp();
		pid = PGRPID;
	}
	sprintf(temp,"%08d", pid);
#endif	/* unix */

#ifdef MSDOS

	if ( ! PGRPID ) PGRPID = wgetpgrp();
	pid = PGRPID;

	sprintf(temp,"%08d", pid);
#endif	/* MSDOS */

#ifdef VMS
	pidbuf.buflen = 4;								/* Now get the process ID of the current */
	pidbuf.item_code = JPI$_MASTER_PID;						/* process in the tree.			*/
	pidbuf.retlen = &retlen;
	pidbuf.bufptr = (char *) &pid;
	pidbuf.endbuf = 0;

	status = sys$getjpi((long) 0,(long) 0,(long) 0, &pidbuf,(long) 0,(long) 0,(long) 0);	/* Get the ID.			*/

	if (status != SS$_NORMAL) return(status);					/* Some error.				*/
	sprintf(temp,"%08x", pid);
#endif	/* VMS */

	memcpy(&worklib[0], "WK", 2);
	memcpy(&worklib[2], &temp[2], 6);
	upper_mem(worklib,8);
}

static int genworkvol(workvol)
char	*workvol;
{

	if ( memcmp(workvol,"      ",6) != 0 ) return(0);
	memcpy(workvol, "WRKVOL", 6 );
	return(1);
}

static int genspoollib(spoollib)
char	*spoollib;
{
	char *ptr;

	if ( memcmp(spoollib,"        ",8) != 0 ) return(0);
	spoollib[0] = '#';
	ptr = wanguid3();
	spoollib[1] = ptr[0];
	spoollib[2] = ptr[1];
	spoollib[3] = ptr[2];
	if ( spoollib[2] == ' ' ) memcpy( &spoollib[2], "PRT", 3);
	else if ( spoollib[3] == ' ' ) memcpy( &spoollib[3], "PRT", 3);
	else memcpy( &spoollib[4], "PRT", 3);
	return(1);
}

/*
	load_options:		Load the runtime OPTIONS file

				ERRFLAG {num}
				SIGNALSOFF
				SIGNALSON		(default)
				NULLISDOT
				NULLISSPACE		(default)
				OUTPUTVERIFYOFF
				OUTPUTVERIFYON		(default)
				MAXPRBPARMS {num}	(default 128)
				MAXPRBPAGES {num}	(default 40)
				HELPSTYLE1		(default Wang style)
				HELPSTYLE2		(Non-Wang style)
				IDALPHA			(default)
				IDNUMERIC
*/
load_options()								/* Load the runtime OPTIONS file.		*/
{
static	int	first=1;
	FILE 	*the_file;
	char	inline[132], keyword[80], value[80];
	int	cnt,i;
	int	len;

	if (!first) return(0);
	first=0;
	build_config_paths();

	the_file = fopen(options_path,"r");
	if (the_file)
	{
		while(fgets(inline,132,the_file))
		{
			len=strlen(inline);
			if (len>0 && inline[len-1] == '\n') inline[len-1] = '\0';	/* Null out the newline char		*/
			cnt = sscanf(inline,"%s %s",keyword,value);
			if ( cnt < 1 ) continue;
			if (keyword[0] == '#') continue;
			upper_string(keyword);

			if      (strcmp(keyword,"ERRFLAG") == 0)		/* Set the ERRFLAG (same as initwisp).		*/
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inline,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(ERRORCODE(18),"Invalid Value",inline,0,0,0,0,0,0);
					continue;
				}
				opt_errflag_found = 1;
				opt_errflag = i;

			}
			else if (strcmp(keyword,"SIGNALSOFF") == 0)		/* Disable Unix signal trapping.		*/
			{
				opt_signalsoff = 1;
			}
			else if (strcmp(keyword,"SIGNALSON") == 0)		/* Enable Unix signal trapping.			*/
			{
				opt_signalsoff = 0;
			}
			else if (strcmp(keyword,"NULLISSPACE") == 0)		/* Set so NULLs will display as a space.	*/
			{
				opt_nulldisplay = 0;
			}
			else if (strcmp(keyword,"NULLISDOT") == 0)		/* Set so NULLs will display as a dot.		*/
			{
				opt_nulldisplay = 1;
			}
			else if (strcmp(keyword,"OUTPUTVERIFYOFF") == 0)	/* Turn off the PF3 to continue screens		*/
			{
				opt_outputverifyoff = 1;
			}
			else if (strcmp(keyword,"OUTPUTVERIFYON") == 0)		/* Turn on the PF3 to continue screens		*/
			{
				opt_outputverifyoff = 0;
			}
			else if (strcmp(keyword,"CREATEVOLUMEON") == 0)		/* UNIX auto create VOLUME if not found		*/
			{
				opt_createvolumeon = 1;
			}
			else if (strcmp(keyword,"CREATEVOLUMEOFF") == 0)	/* UNIX don't create VOLUME if not found	*/
			{
				opt_createvolumeon = 0;
			}
			else if (strcmp(keyword,"IDSIPRINTON") == 0)		/* UNIX use the IDSI print spooler		*/
			{
				opt_idsiprint = 1;
			}
			else if (strcmp(keyword,"IDSIPRINTOFF") == 0)		/* UNIX use lp, not the IDSI print spooler	*/
			{
				opt_idsiprint = 0;
			}
			else if (strcmp(keyword,"IDNUMERIC") == 0)		/* UNIX EXTRACT ID returns Numeric user ID	*/
			{
				opt_idnumeric = 1;
			}
			else if (strcmp(keyword,"IDALPHA") == 0)		/* UNIX EXTRACT ID returns Alpha user ID	*/
			{
				opt_idnumeric = 0;
			}
			else if (strcmp(keyword,"HELPSTYLE1") == 0)		/* Help is Wang style				*/
			{
				opt_helpstyle = 1;
			}
			else if (strcmp(keyword,"HELPSTYLE2") == 0)		/* Help is non-Wang style			*/
			{
				opt_helpstyle = 2;
			}
			else if (strcmp(keyword,"ALLSTATUSKEYS") == 0)		/* Pass All STATUS keys thru to user declaritive*/
			{
				opt_allstatuskeys = 1;
			}
			else if (strcmp(keyword,"MAXPRBPARMS") == 0)		/* Set the MAX PRB (putparm) number of parms	*/
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inline,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(ERRORCODE(18),"Invalid Value",inline,0,0,0,0,0,0);
					continue;
				}
				opt_max_parms = i;

			}
			else if (strcmp(keyword,"MAXPRBPAGES") == 0)		/* Set the MAX PRB (putparm) number of parms	*/
			{
				if ( cnt < 2 )
				{
					werrlog(ERRORCODE(18),"Missing Value",inline,0,0,0,0,0,0);
					continue;
				}
				cnt = sscanf(value,"%d",&i);
				if ( cnt != 1 )
				{
					werrlog(ERRORCODE(18),"Invalid Value",inline,0,0,0,0,0,0);
					continue;
				}
				opt_max_pages = i;

			}
			else
			{
				werrlog(ERRORCODE(18),"Unknown keyword",inline,0,0,0,0,0,0);
			}
		}
		fclose(the_file);
	}
}

/*
	get_defs	Get a default based on the code.
*/
int get_defs(code,ptr)
int	code;
char	*ptr;
{
	wpload();

	switch(code)
	{
	case DEFAULTS_PM:
		*ptr = defaults.prt_mode;
		break;
	case DEFAULTS_PC:
		*ptr = defaults.prt_class;
		break;
	case DEFAULTS_PR:
		memcpy(ptr,&defaults.prt_num,sizeof(defaults.prt_num));
		break;
	case DEFAULTS_FN:
		memcpy(ptr,&defaults.prt_form,sizeof(defaults.prt_form));
		break;
	case DEFAULTS_IL:
		memcpy(ptr,defaults.inlib,8);
		break;
	case DEFAULTS_IV:
		memcpy(ptr,defaults.invol,6);
		break;
	case DEFAULTS_OL:
		memcpy(ptr,defaults.outlib,8);
		break;
	case DEFAULTS_OV:
		memcpy(ptr,defaults.outvol,6);
		break;
	case DEFAULTS_RL:
		memcpy(ptr,defaults.runlib,8);
		break;
	case DEFAULTS_RV:
		memcpy(ptr,defaults.runvol,6);
		break;
	case DEFAULTS_SL:
		memcpy(ptr,defaults.spoolib,8);
		break;
	case DEFAULTS_SV:
		memcpy(ptr,defaults.spoolvol,6);
		break;
	case DEFAULTS_PL:
		getproglib(ptr);
		break;
	case DEFAULTS_PV:
		getprogvol(ptr);
		break;
	case DEFAULTS_WL:
		memcpy(ptr,defaults.worklib,8);
		break;
	case DEFAULTS_WV:
		memcpy(ptr,defaults.workvol,6);
		break;
	case DEFAULTS_JS:
		*ptr = defaults.proc_stat;
		break;
	case DEFAULTS_JC:
		*ptr = defaults.proc_class;
		break;
	case DEFAULTS_JL:
		memcpy(ptr,defaults.proc_cpu,6);
		break;
	case DEFAULTS_FLAGS:
		memcpy(ptr,&defaults.flags,sizeof(defaults.flags));
		break;
	case DEFAULTS_PSB_CHAR:
		*ptr = defaults.psb_select;
		break;
	case DEFAULTS_PSB_SET:
		memcpy(ptr,&defaults.psb_charset,sizeof(defaults.psb_charset));
		break;
	case DEFAULTS_PSB_REN:
		memcpy(ptr,&defaults.psb_rendition,sizeof(defaults.psb_rendition));
		break;
	case DEFAULTS_LI:
		memcpy(ptr,&defaults.prt_lines,sizeof(defaults.prt_lines));
		break;
	case DEFAULTS_MP_CURSOR:
		memcpy(ptr,&defaults.mp_cursor,sizeof(defaults.mp_cursor));
		break;
	case DEFAULTS_AUTOMOVE:
		memcpy(ptr,&defaults.automove,sizeof(defaults.automove));
		break;
	case DEFAULTS_AUTOTAB:
		memcpy(ptr,&defaults.autotab,sizeof(defaults.autotab));
		break;
	case DEFAULTS_BGCHANGE:
		memcpy(ptr,&defaults.bgchange,sizeof(defaults.bgchange));
		break;
	case DEFAULTS_BGCOLOR:
		memcpy(ptr,&defaults.bgcolor,sizeof(defaults.bgcolor));
		break;
	case DEFAULTS_EXCOLOR:	
		memcpy(ptr,&defaults.excolor,sizeof(defaults.excolor));
		break;
	}
}

/*
	set_defs	Set a default based on the code.

	* * * * WARNING * * * *  
		Set_defs only loads the value into the internal structure "defaults" -- after you finish a series of
		calls to "set_defs" you need to perform a "wps_usr(&defaults)" to save these values into a temp area.

*/
int set_defs(code,ptr)
int	code;
char	*ptr;
{
	wpload();

	switch(code)
	{
	case DEFAULTS_PM:
		defaults.prt_mode = *ptr;
		break;
	case DEFAULTS_PC:
		defaults.prt_class = *ptr;
		break;
	case DEFAULTS_PR:
		memcpy(&defaults.prt_num,ptr,sizeof(defaults.prt_num));
		break;
	case DEFAULTS_FN:
		memcpy(&defaults.prt_form,ptr,sizeof(defaults.prt_form));
		break;
	case DEFAULTS_IL:
		loadpadnull(defaults.inlib,ptr,8);
		break;
	case DEFAULTS_IV:
		loadpadnull(defaults.invol,ptr,6);
		break;
	case DEFAULTS_OL:
		loadpadnull(defaults.outlib,ptr,8);
		break;
	case DEFAULTS_OV:
		loadpadnull(defaults.outvol,ptr,6);
		break;
	case DEFAULTS_RL:
		loadpadnull(defaults.runlib,ptr,8);
		break;
	case DEFAULTS_RV:
		loadpadnull(defaults.runvol,ptr,6);
		break;
	case DEFAULTS_SL:
		loadpadnull(defaults.spoolib,ptr,8);
		break;
	case DEFAULTS_SV:
		loadpadnull(defaults.spoolvol,ptr,6);
		break;
	case DEFAULTS_PL:
		loadpadnull(defaults.proglib,ptr,8);
		strcpy(curr_proglib,defaults.proglib);
		break;
	case DEFAULTS_PV:
		loadpadnull(defaults.progvol,ptr,6);
		strcpy(curr_progvol,defaults.progvol);
		break;
	case DEFAULTS_WL:
		loadpadnull(defaults.worklib,ptr,8);
		break;
	case DEFAULTS_WV:
		loadpadnull(defaults.workvol,ptr,6);
		break;
	case DEFAULTS_JS:
		defaults.proc_stat = *ptr;
		break;
	case DEFAULTS_JC:
		defaults.proc_class = *ptr;
		break;
	case DEFAULTS_JL:
		memcpy(defaults.proc_cpu,ptr,6);
		defaults.proc_cpu[6] = '\0';
		break;
	case DEFAULTS_FLAGS:
		memcpy(&defaults.flags,ptr,sizeof(defaults.flags));
		break;
	case DEFAULTS_PSB_CHAR:
		defaults.psb_select = *ptr;
		break;
	case DEFAULTS_PSB_SET:
		memcpy(&defaults.psb_charset,ptr,sizeof(defaults.psb_charset));
		break;
	case DEFAULTS_PSB_REN:
		memcpy(&defaults.psb_rendition,ptr,sizeof(defaults.psb_rendition));
		break;
	case DEFAULTS_LI:
		memcpy(&defaults.prt_lines,ptr,sizeof(defaults.prt_lines));
		break;
	case DEFAULTS_MP_CURSOR:
		memcpy(&defaults.mp_cursor,ptr,sizeof(defaults.mp_cursor));
		break;
	case DEFAULTS_AUTOMOVE:
		memcpy(&defaults.automove,ptr,sizeof(defaults.automove));
		break;
	case DEFAULTS_AUTOTAB:
		memcpy(&defaults.autotab,ptr,sizeof(defaults.autotab));
		break;
	case DEFAULTS_BGCHANGE:
		memcpy(&defaults.bgchange,ptr,sizeof(defaults.bgchange));
		break;
	case DEFAULTS_BGCOLOR:
		memcpy(&defaults.bgcolor,ptr,sizeof(defaults.bgcolor));
		break;
	case DEFAULTS_EXCOLOR:	
		memcpy(&defaults.excolor,ptr,sizeof(defaults.excolor));
		break;
	}
}

/*
	loadpadnull	Load dest with src padding out to size with blanks then null terminate
*/
loadpadnull(dest,src,size)
char *dest;
char *src;
int  size;
{
	loadpad(dest,src,size);
	dest[size] = '\0';
}

/*===============================================================================================================================*/
/*
**	The following routines deal with maintaining PROGLIB and PROGVOL.
*/
/*===============================================================================================================================*/
/*
	DESCRIPTION OF PROGLIB PROGVOL ON THE WANG:

		On the Wang PROGLIB and PROGVOL represent the location where the currently running program or procedure resides.
		These defaults are used by the VSSUBS LINK and SUBMIT, and by procedure verbs RUN and SUBMIT.  They are the
		default location to look for programs to execute.  This values automatically change when a new program is
		run and change back when the LINK completes.  It is however possible to SET these values manually.
		They are normally used as follows: the first program of an application is run by explicitly giving the
		lib and vol, then all other programs are run by using PROGLIB and PROGVOL as defaults.  If a program wants to
		run another program that is in a different library this can be done in two ways: 1) Give the lib and vol
		of the new program in the LINK or RUN commands.  2) Set PROGLIB and PROGVOL to the new location then do
		the LINK or RUN without specifing the lib or vol and allowing it to default to PROGLIB and PROGVOL.

		NOTE:	RUNLIB and RUNVOL are only used when starting a program from the COMMAND PROCESSOR.
			CURRLIB and CURRVOL give the location of the currently running program, so they are useless
			when running from Procedure because they will point to the system library where the procedure
			interrupter resides.

	IMPLEMENTATION:

		If PROGLIB or PROGVOL are not set the RUNLIB and RUNVOL are returned.
		On a LINK or SUBMIT the values are passed to the new process by setting shell variables after the fork.
		The value of PROGLIB and PROGVOL is determined as follows:
			1)  In memory save values.   (curr_proglib/curr_progvol)
			2)  The SYMBOL i.e. temp defaults
			3)  Shell variables
			4)  RUNLIB/RUNVOL values
		The SYMBOL will only be set by a call to SET (also update in memory save values).

		The big problem is making this all work in a shell script that is linked to from a cobol program that
		does a WUSAGE SET to change PROGLIB and PROGVOL.  It has to use the SYMBOL to store this value as it
		can not set shell variables that persist.

		For SUBMIT we must use shell variables because the SYMBOL may change before it is read.
		For shell scripts must use SYMBOL because wusage can't set shell variable.
		On a LINK or SUBMIT must clear the SYMBOL so the shell variables will be used.
		After reading from SYMBOL or shell variable must set save values.
		Save the values in SYMBOL on entry and reset them on exit of a cobol program. (initwisp & wispexit) This is
		in case a linked to program sets these values, you don't want them still around after you return from the link.
		Initwisp() must do a get_defs() for PROGVOL and PROGLIB to force an update of curr.

	SCENARIO:
		A-Program  LINKs to  B-Script
		B-Script   RUNs      C-Program
		C-Program  LINKs to  D-Program

		[A] -link-> [B] -run-> [C] -link-> [D]

		B:
		>wusage set proglib=abc
		>wrun C

	NOTES:
				
*/

#define SHELL_PROGVOL	"_PROGVOL"
#define SHELL_PROGLIB	"_PROGLIB"

/*
	getprogvol	
	getproglib	These routines return the current value of PROGLIB and PROGVOL. 
			It checks curr_progvol/lib first and if not set it checks symbol then shell variable.  If found in
			symbol or shell variable it will update curr_proglib/vol.
			If there is still not current values it will return RUNLIB RUNVOL.
			These routines are only called from get_defs().
*/

static getprogvol(result)
char 	result[6];
{
	char	*ptr;
	int	len;

	if (curr_progvol[0] == ' ')						/* If in memory save is not set check symbol	*/
	{
		if (defaults.progvol[0] != ' ')					/* If symbol is set then use it			*/
		{
			memcpy(curr_progvol,defaults.progvol,6);		/* Copy symbol in in memory save area		*/
		}
#ifdef unix
		else if (ptr = (char *)getenv(SHELL_PROGVOL))			/* Check the shell variable			*/
		{
			if (*ptr != ' ')					/* if shell var exists and not null then use it	*/
			{
				len = strlen(ptr);
				len = (len < 6) ? len : 6;
				memset(curr_progvol,' ',6);
				memcpy(curr_progvol,ptr,len);			/* Copy symbol in in memory save area		*/
			}
		}
#endif
	}
	curr_progvol[6] = '\0';

	if (curr_progvol[0] != ' ')						/* If in memory save area is set then use it	*/
	{
		memcpy(result,curr_progvol,6);
	}
	else									/* Else return run values			*/
	{
		memcpy(result,defaults.runvol,6);
	}
}
static getproglib(result)
char 	result[8];
{
	char	*ptr;
	int	len;

	if (curr_proglib[0] == ' ')						/* If in memory save is not set check symbol	*/
	{
		if (defaults.proglib[0] != ' ')					/* If symbol is set then use it			*/
		{
			memcpy(curr_proglib,defaults.proglib,8);		/* Copy symbol in in memory save area		*/
		}
#ifdef unix
		else if (ptr = (char *)getenv(SHELL_PROGLIB))			/* Check the shell variable			*/
		{
			if (*ptr != ' ')					/* if shell var exists and not null then use it	*/
			{
				len = strlen(ptr);
				len = (len < 8) ? len : 8;
				memset(curr_proglib,' ',8);
				memcpy(curr_proglib,ptr,len);			/* Copy symbol in in memory save area		*/
			}
		}
#endif
	}
	curr_proglib[8] = '\0';

	if (curr_proglib[0] != ' ')						/* If in memory save area is set then use it	*/
	{
		memcpy(result,curr_proglib,8);
	}
	else									/* Else return run values			*/
	{
		memcpy(result,defaults.runlib,8);
	}
}

/*
	clearprogsymb	This routine clears the PROGLIB and PROGVOL symbol both in defaults and in temp.
			This is called before a LINK or SUBMIT.
*/
clearprogsymb()
{
	set_defs(DEFAULTS_PV,"      ");
	set_defs(DEFAULTS_PL,"        ");
	wps_usr(&defaults);							/* Write changes to temp area			*/
}

/*
	setprogdefs	Set the shell variables PROGVOL and PROGLIB.
			This is only called during a LINK, LINKPROC, and SUBMIT.
*/
setprogdefs(progvol,proglib)
char	*progvol;
char	*proglib;
{
	setprogvol(progvol);
	setproglib(proglib);
}
static setprogvol(progvol)
char	*progvol;
{
#ifdef unix
	char	buff[20];
	sprintf(buff,"%s=%s",SHELL_PROGVOL,progvol);
	setenvstr(buff);
#endif /* unix */
}
static setproglib(proglib)
char	*proglib;
{
#ifdef unix
	char	buff[20];
	sprintf(buff,"%s=%s",SHELL_PROGLIB,proglib);
	setenvstr(buff);
#endif /* unix */
}

/*
	saveprogdefs
	restoreprogdefs		These routines save the symbol values or PROGLIB and PROGVOL when a COBOL program
				is started and restore them on exit.
*/
static char	def_progvol[7] = "      ";
static char	def_proglib[9] = "        ";

saveprogdefs()
{
#ifdef unix
	char	buff[20];

	get_defs(DEFAULTS_PV,buff);						/* Force an update of curr			*/
	get_defs(DEFAULTS_PL,buff);

	memcpy(def_progvol,defaults.progvol,6);					/* Save the entry values			*/
	memcpy(def_proglib,defaults.proglib,8);
#endif
}

restoreprogdefs()
{
#ifdef unix
	set_defs(DEFAULTS_PV,def_progvol);					/* Restore the entry values			*/
	set_defs(DEFAULTS_PL,def_proglib);
	wps_usr(&defaults);							/* Write changes to temp area			*/
#endif
}

