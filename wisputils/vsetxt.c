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
	if(txt->prev == NULL)
		text_first = txt->next;
	else
		txt->prev->next = txt->next;
	if(txt->next == NULL)
		text_last = txt->prev;
	else
		txt->next->prev = txt->prev;
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
		txt = next;
		}
	text_first = text_last = NULL;
}

free_one_text(txt)
TEXT *txt;
{
	if(!txt)
		return;
	if(txt->text)
		free(txt->text);
	free(txt);
}
