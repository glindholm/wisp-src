static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#include <stdio.h>
#include <string.h>

#include "vsetxt.h"
#include "vseglb.h"
#include "vsedit.h"
#include "vsebasic.h"

#include "idsisubs.h"

TEXT *new_text(char *str)
{
	TEXT *txt;

	txt = (TEXT*) calloc(1,sizeof(TEXT));

	if(!txt)
	{
		return(NULL);
	}

	txt->modfld = NULL;
	txt->lineno = 0;

	if(!(over_text(txt,str)))
	{
		free(txt);
		return(NULL);
	}
	++vse_lines;
	return(txt);
}

TEXT *over_text(TEXT *txt, char *str)
{
	if(txt->text)
	{
		free(txt->text);
	}

	if (!str)
	{
		str = "";
	}

	txt->text = (char*) calloc(1,strlen(str) + 1);

	if(!txt->text)
		return(NULL);
	strcpy(txt->text,str);
	return(txt);
}

void insert_text(TEXT *txt1, TEXT *txt2, TEXT *newtxt)
{
	if(txt1 == NULL)
		text_first = newtxt;
	else
		txt1->next = newtxt;
	if(txt2 == NULL)
		text_last = newtxt;
	else
		txt2->prev = newtxt;
	newtxt->prev = txt1;
	newtxt->next = txt2;
}

void del_text(TEXT *txt)
{
	if (scr_first==txt)
	  scr_first=txt->next;
	if(txt->prev == NULL)
		text_first = txt->next;
	else
		txt->prev->next = txt->next;
	if(txt->next == NULL)
		text_last = txt->prev;
	else
		txt->next->prev = txt->prev;

	/* Check the list of linenumbers and if this line is to be deleted,
	   remove the entry from the list. Added by CIS, 07/13/93 AJA */
	if ( lang_type() == LANG_BASIC )
		delete_linenum( txt->lineno );
	free_one_text(txt);
	
}

void append_text(TEXT *txt)
{
	if(text_first == NULL)
		{
		text_first = text_last = txt;
		}
	else
		{
		text_last->next = txt;
		txt->prev = text_last;
		text_last = txt;
		}
}

void free_text(void)
{
	free_text_list(text_first);
	text_first = text_last = NULL;
}

void free_text_list(TEXT *first)
{
	TEXT *txt,*next;

	txt = first;
	while(txt)
	{
		next= txt->next;
		free_one_text(txt);
		txt = next;
	}
}

void free_one_text(TEXT *txt)
{
	if(!txt)
		return;
	if(txt->text && (txt->text!= (char*)-1))
		free(txt->text);

	if ( txt->modfld )
	{	
		free( txt->modfld );
	}

	free(txt);

	--vse_lines;
}
/*
**	History:
**	$Log: vsetxt.c,v $
**	Revision 1.10  1996/09/03 22:24:12  gsl
**	drcs update
**	
**
**
*/
