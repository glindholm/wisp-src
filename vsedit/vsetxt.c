#include <stdio.h>

#include "vseglb.h"


TEXT *new_text(str)
char *str;
{
	TEXT *txt,*over_text();

	txt = (TEXT*) calloc(1,sizeof(TEXT));

	if(!txt)
		return(NULL);

	if(!(over_text(txt,str)))
		{
		free(txt);
		return(NULL);
		}
	++vse_lines;
	return(txt);
}

TEXT *over_text(txt,str)
TEXT *txt;
char *str;
{
	if(txt->text)
		free(txt->text);
	txt->text = (char*) calloc(1,strlen(str) + 1);
	if(!txt->text)
		return(NULL);
	strcpy(txt->text,str);
	return(txt);
}

insert_text(txt1,txt2,newtxt)
TEXT *txt1,*txt2,*newtxt;
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

del_text(txt)
TEXT *txt;
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
	if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
		delete_linenum( txt->lineno );
	free_one_text(txt);
	--vse_lines;
	
}

append_text(txt)
TEXT *txt;
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

free_text()
{
	TEXT *txt,*next;

	txt = text_first;
	while(txt)
		{
		next= txt->next;
		free_one_text(txt);

		/* Free the storage used for the mod field.
		   Added by CIS: 07/27/93 AJA */
		free( txt->modfld );
		txt = next;
		}
	text_first = text_last = NULL;
}

free_one_text(txt)
TEXT *txt;
{
	if(!txt)
		return;
	if(txt->text && (txt->text!= (char*)-1))
		free(txt->text);
	free(txt);
}
