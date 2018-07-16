#include <stdio.h>
#include <ctype.h>
#include "vseglb.h"

/*----
Passed values are a system file name and an
upper case language, at the moment, COBOL C SHELL and TEXT are all
valid. The fields should be null terminated
------*/
vsedit(name,lang)
char *name;
char *lang;
{
	strcpy(vse_sysname,name);
	strcpy(vse_gp_input_language,lang);
	init_lang();
	init_lastnum();
	vse_menu=EDIT_MENU;
	vse_lines = 0;
	vse_loading_menu();
	if(load_file() == -1)
		return;
	else
		vse_menus();
}

/*----
Set up options for each of the possible languages.
------*/
init_lang()
{
	int len;

	len = strlen(vse_gp_input_language);
	if(!(memcmp(vse_gp_input_language,COBOL_LANGUAGE,len)))
		init_cobol();
	if(!(memcmp(vse_gp_input_language,C_LANGUAGE,len)))
		init_c();
	if(!(memcmp(vse_gp_input_language,TEXT_LANGUAGE,len)))
		init_text();
	if(!(memcmp(vse_gp_input_language,SHELL_LANGUAGE,len)))
		init_shell();
	convert_tabs();
	convert_cols();
}

init_cobol()
{
	strcpy(vse_gp_defaults_tabs,COBOL_TAB_STRING);
	strcpy(vse_gp_defaults_columns,COBOL_COL_STRING);
	vse_text_col = 7;
	vse_num_col = 1;
	vse_numbering = 1;
	strcpy(vse_gp_defaults_mode,COBOL_CASE_MODE);
	strcpy(vse_gp_defaults_case,COBOL_MATCH_MODE);
}
init_c()
{
	strcpy(vse_gp_defaults_tabs,C_TAB_STRING);
	strcpy(vse_gp_defaults_columns,C_COL_STRING);
	vse_text_col = 1;
	vse_num_col = 0;
	vse_numbering =0;
	strcpy(vse_gp_defaults_mode,C_CASE_MODE);
	strcpy(vse_gp_defaults_case,C_MATCH_MODE);
}

init_text()
{
	strcpy(vse_gp_defaults_tabs,TEXT_TAB_STRING);
	strcpy(vse_gp_defaults_columns,TEXT_COL_STRING);
	vse_text_col = 1;
	vse_num_col = 0;
	vse_numbering =0;
	strcpy(vse_gp_defaults_mode,TEXT_CASE_MODE);
	strcpy(vse_gp_defaults_case,TEXT_MATCH_MODE);
}
init_shell()
{
	strcpy(vse_gp_defaults_tabs,SHELL_TAB_STRING);
	strcpy(vse_gp_defaults_columns,SHELL_COL_STRING);
	vse_text_col = 1;
	vse_num_col = 0;
	vse_numbering =0;
	strcpy(vse_gp_defaults_mode,SHELL_CASE_MODE);
	strcpy(vse_gp_defaults_case,SHELL_MATCH_MODE);
}
	

/*----
Convert all tab entries to values
------*/
convert_tabs()
{
	int idx;

	for(idx=0; idx < NO_OF_TABS; ++idx)
		{
		long_from_str2(&vse_gp_defaults_tabs[idx *3],&vse_tab[idx]);
		}
}

/*----
Convert 2 column string entries
------*/
convert_cols()
{
	long_from_str2(&vse_gp_defaults_columns[0],&vse_column[0]);
	long_from_str2(&vse_gp_defaults_columns[3],&vse_column[1]);
	vse_text_width = (vse_column[1] - vse_column[0]) + 1;
}

/*---
Convert next 2 bytes of str to long pointed to by value
------*/
long_from_str2(str,value)
char *str;
long *value;
{
	char buf[3];

	memcpy(buf,str,2);
	buf[2] = 0;
	
	*value = 0;
	sscanf(buf,"%ld",value);
}

vse_menus()
{
	scr_first = text_first;
	for(;;)
		{
		if(vse_menu == EDIT_MENU)
			{
			vse_standard_menu();
			}
		else
			{
			vse_special_menu();
			if( (vse_special_pick == 16) ||
			    (vse_special_pick == 4)   )
				break;
			}
		}
}

load_file()
{
	FILE *ff;
	int rc;

	if(vse_new_file)
		return(0);
	trunc(vse_sysname);

	ff = fopen(vse_sysname,"r");
	untrunc(vse_sysname,sizeof(vse_sysname)-1);
	rc = load_lines(ff);
	fclose(ff);
	return(rc);
}

static long lastnum;

init_lastnum()
{
	lastnum = 0L;
}

load_lines(ff)
FILE *ff;
{
	char buf[82],*bptr;
	long num,next_lineno();
	TEXT *txt;

	lastnum = 0L;
	vse_lines = 0L;
	while(fgets(buf,81,ff))
		{
		trunc(buf);
		bptr = buf;
		num = 0L;
		if(vse_numbering)
			{
			sscanf(&buf[vse_num_col - 1]," %ld",&num);
			if(vse_num_col == 1)
				{
				bptr = &buf[6];
				}
			else
				{
				buf[vse_num_col - 1] = 0;
				}
			}
		txt =(TEXT*) new_text(bptr);
		if(!txt)
			{
			free_text();
			out_of_space();
			return(-1);
			}
		txt->lineno = next_lineno(num);
		if(txt->lineno == -1)
			{
			free_text();
			return(-1);
			}
		append_text(txt);
		}
	return(0);
}

/*----
Returns the next line number
The second condition (num <= lastnum) is actually an error in the input
file. WANG handles this with a BADNUMS getparm asking the user if
the file should be numbered, or allowing a PFKey to respecify
the input language. In this example, In this example vse_badnums() stub
is set to return true so the renumber goes ahead.
------*/
long next_lineno(num)
long num;
{
	if(!vse_numbering)
		{
		return(lastnum += vse_options_incr);
		}
	if(num <= lastnum)
		{
		if(vse_badnums())
			return(lastnum += vse_options_incr);
		else
			return(-1);
		}
	return(lastnum = num);
}
/*----
This stub routine should return 1 if the user wants to proceed
and let the loader renumber the file.
or zero if the user wants to abort
------*/
vse_badnums()
{
	return(1);
}

/*----
Some sort of can't load message goes here
------*/
out_of_space()
{
}

save_file()
{
	FILE *ff;
	int rc;

	trunc(vse_sysname);

	ff = fopen(vse_sysname,"w");
	if(!ff)
		return(-1);
	untrunc(vse_sysname,sizeof(vse_sysname)-1);
	rc = save_lines(ff);
	fclose(ff);
	if(vse_new_file)
		vse_new_file = 0;
	return(rc);
}

save_lines(ff)
FILE *ff;
{
	TEXT *txt;

	txt = text_first;
	while(txt)
		{
		if(vse_numbering)
			{
			if(vse_num_col == 1)
				fprintf(ff,"%06ld%s\n",txt->lineno,txt->text);
			else
				fprintf(ff,"%-74s%06ld",txt->text,txt->lineno);
			}
		else
			{
			fprintf(ff,"%s\n",txt->text);
			}
		txt = txt->next;
		}
	return(1);
}

