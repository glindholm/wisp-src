			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	       Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		vsedit.c
**
**	Purpose:	?
**
**	Routines:	
**
**	vsedit()	The main entry point once the filename is known.
**	init_lang()
**	init_cobol()	Initialize for COBOL
**	init_c()	Initialize for C
**	init_text()	Initialize for TEXT
**	init_shell()	Initialize for SHELL
**	init_basic()	Initialize for BASIC
**	convert_tabs()
**	convert_cols()
**	int4_from_str2()
**	vse_menus()
**	load_file()	Loads the file
**	init_lastnum()
**	load_lines()
**	next_lineno()
**	vse_badnums()
**	save_file()
**	save_lines()
**	myfgets()	Reads the file and messages the input.
**	numb()
*/

#include <stdio.h>
#include <ctype.h>
#ifdef unix
#include <fcntl.h>
#endif
#include "vseglb.h"
#include "idsistd.h"
/*----
Passed values are a system file name and an
upper case language, at the moment, COBOL C SHELL and TEXT are all
valid. The fields should be null terminated
------*/
vsedit(name,lang)
char *name;
char *lang;
{  
	char settab;
	
        strcpy(vse_sysname,name);
        strcpy(vse_gp_input_language,lang);
	if(!(strcmp(vse_gp_input_language,COBOL_LANGUAGE)))
	  strcpy(vse_tab_setting,DEFAULT_COB_TAB_STRING);
	
	/* Added by CIS for BASIC language support 07/09/93 AJA */
	else if(!(strcmp(vse_gp_input_language,BASIC_LANGUAGE)))
	  strcpy(vse_tab_setting,DEFAULT_BASIC_TAB_STRING);
	else
	  strcpy(vse_tab_setting,DEFAULT_C_TAB_STRING);
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

		/* Added by CIS for BASIC language support 07/09/93 AJA */
        if(!(memcmp(vse_gp_input_language,BASIC_LANGUAGE,len)))
                init_basic();
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
		strcpy( vse_defaults_modcol, COBOL_MOD_COL_STRING );
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
		strcpy( vse_defaults_modcol, C_MOD_COL_STRING );
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
		strcpy( vse_defaults_modcol, TEXT_MOD_COL_STRING );
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
		strcpy( vse_defaults_modcol, SHELL_MOD_COL_STRING );
}

/* Added by CIS for BASIC language support 07/09/93 AJA */
init_basic()
{
		head_linenum = NULL;	
        strcpy(vse_gp_defaults_tabs,BASIC_TAB_STRING);
        strcpy(vse_gp_defaults_columns,BASIC_COL_STRING);
        vse_text_col = 7;
        vse_num_col = 1;
        vse_numbering = 1;
        strcpy(vse_gp_defaults_mode,BASIC_CASE_MODE);
        strcpy(vse_gp_defaults_case,BASIC_MATCH_MODE);
		strcpy( vse_defaults_modcol, BASIC_MOD_COL_STRING );
}
/*----
Convert all tab entries to values
------*/
convert_tabs()
{
        int idx;
        for(idx=0; idx < NO_OF_TABS; ++idx)
                {
                int4_from_str2(&vse_gp_defaults_tabs[idx *3],&vse_tab[idx]);
                }
}
/*----
Convert 2 column string entries
------*/
convert_cols()
{
        int4_from_str2(&vse_gp_defaults_columns[0],&vse_column[0]);
        int4_from_str2(&vse_gp_defaults_columns[3],&vse_column[1]);
        vse_text_width = (vse_column[1] - vse_column[0]) + 1;

		/* Set the text width if mod codes are displayed.
		   Added by CIS: 07/23/93 AJA */
        int4_from_str2(&vse_defaults_modcol[0],&vse_column[0]);
        int4_from_str2(&vse_defaults_modcol[3],&vse_column[1]);
        vse_mod_text_width = (vse_column[1] - vse_column[0]) + 1;
}
/*---
Convert next 2 bytes of str to int4 pointed to by value
------*/
int4_from_str2(str,value)
char *str;
int4 *value;
{
        char buf[3];
        memcpy(buf,str,2);
        buf[2] = 0;
        *value = 0;
        *value = ATOI4(buf);
}
vse_menus()
{
	extern char ed_oa[];
	
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
	if (numb(ff))
	  init_cobol();
	else
	  init_c();
        rc = load_lines(ff);
        fclose(ff);
        return(rc);
}
static int4 lastnum;
init_lastnum()
{
        lastnum = 0;
}
load_lines(ff)
FILE *ff;
{
        char buf[256],*bptr;
		char *mod_code=NULL;
        int4 num,next_lineno();
        TEXT *txt;
        int myfgets();
        lastnum = 0;
        vse_lines = 0;
        memset(buf,0,sizeof(buf));

		/* Temporarily comment out this line, we want to get 80 chars no matter
		   what. CIS: 07/23/93 AJA
        while(myfgets(buf,vse_numbering?80:72,ff)) */
		while( myfgets( buf, 80, ff ) )
        {
                bptr = buf;
                num = 0;
                if(vse_numbering)
                {

						/* Modified sscanf statement to take first 6 digits
						   of the line and assign this as the line number.
						   This prevents additional digits following the line
						   number from being stored as part of line number.
						   Modified by CIS: 08/25/93 AJA */
                        sscanf(&buf[vse_num_col - 1]," %06ld",&num);
                        if(vse_num_col == 1)
                        {
                                bptr = &buf[6];
                        }
                        else
                        {
                                buf[vse_num_col - 1] = 0;
                        }
                }
		++vse_file_linecount;

				/* Save mod code in mod_code, and clear it out of bptr.
   		   	   	   Added by CIS: 07/23/93 AJA */
				mod_code = (char *) calloc( 9, sizeof( char ) );
				strncpy( mod_code, &(bptr[vse_text_width]), 8 );
				if ( strlen( bptr ) > vse_text_width )
					memset( &(bptr[vse_text_width]), 0x0, 8 );
                txt =(TEXT*) new_text(bptr);
                if(!txt)
                {
                        free_text();

						/* If BASIC, free up list of line numbers.
						   Added by CIS, 07/13/93 AJA */
						if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
							free_linenum();
                        out_of_space();
                        return(-1);
                }
                txt->lineno = next_lineno(num);
                if(txt->lineno == -1)
                {
                        free_text();

						/* If BASIC, free up list of line numbers.
						   Added by CIS, 07/13/93 AJA */
						if ( !(strcmp( vse_gp_input_language, BASIC_LANGUAGE )) )
							free_linenum();
                        return(-1);
                }

				/* Set the modfld if everything is O.K.
				   Added by CIS: 07/27/93 AJA */
				if ( ( !(strcmp( vse_gp_input_language, COBOL_LANGUAGE )) ||
					!(strcmp( vse_gp_input_language, BASIC_LANGUAGE)) ) &&
					strlen( mod_code ) )
				{
					txt->modfld = (char *) calloc( 9, sizeof( char ) );
					strncpy( txt->modfld, mod_code, strlen( mod_code ) );
				}
				else
					txt->modfld = NULL;
				free( mod_code );
                append_text(txt);

				/* If language is BASIC, look for embedded line number in
			   	   line. Added by CIS, 07/12/93 AJA */
				if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
					find_linenum( txt );
                memset(buf,0,sizeof(buf));
        }
	if (!vse_numbering || (vse_file_linecount>10000) )
	{	
		int inc=100, maxinc=100, diff=999990-100, num=vse_file_linecount;
		
		if  (vse_file_linecount>10000) 
		{
			inc= maxinc = diff / num;
			if (inc >=100) inc=100;
			else if (inc <100 && inc >=50) inc=50;
			else if (inc <50 && inc >=10) inc=10;
			else if (inc <10 && inc >=5) inc=5;
			else if (inc <5 && inc >=2) inc=2;
			else if (inc <2 ) inc=1;
		}
		text_first->lineno=100;
		text_last->lineno=999999;
		renumber_range(text_first,text_last,100,vse_file_linecount,inc);
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
int4 next_lineno(num)
int4 num;
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
	vse_file_changed = 0;
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

					/* If using BASIC, output the text with line number and mod
					   field. Added by CIS: 08/06/93 AJA */
					if (!(strcmp( vse_gp_input_language, BASIC_LANGUAGE )))
						fprintf( ff, "%06ld%s%s\n", txt->lineno, txt->text, txt->modfld );
                    else if(vse_num_col == 1)
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
/*fprintf(ff,"%06ld%s\n",txt->lineno,txt->text); else fprintf(ff,"%-74s%06ld",txt->text,txt->lineno);>text,txt->lineno);*/
myfgets(buf,size,file)
FILE *file;
int size;
char buf[];
{
        static char linebuf[2000];
        static int bytesleft=0, curpos=0;
        static int warned_line_too_long=0;
        int bytes;
        --size;
        while (1)
        {
                if (bytesleft)
                {
                        if (bytesleft > size + 1)
                          bytes = size-1;
                        else
                          bytes = bytesleft;
                        strncpy(buf,linebuf+curpos,bytes);
                        buf[bytes]=(char)0;
                        bytesleft -= bytes;
                        curpos += bytes;
                        return 1;
                }
                if (fgets(linebuf,sizeof(linebuf),file)==0)
                  return 0;
                untabify(linebuf,sizeof(linebuf));
                trunc(linebuf);
		if (strlen(linebuf)==0)
		{
			strcpy(linebuf,"      ");
		}
                bytesleft=strlen(linebuf);
                if (bytesleft > size + 1)
                {
                        if (!warned_line_too_long)
                        {
                                ++warned_line_too_long;
                                vre_window("Lines that are too long to be displayed have been split.");
                        }
                }
                curpos=0;
        }
}
numb(ff)
FILE *ff;
{
	int i, numb, j;
	char buf[256],*p;
	
	for(i=0, numb=1; i<10; ++i)
	{
		p=fgets(buf,sizeof(buf),ff);
		if (!p)
		  break;
		for (j=0; j<6; ++j)
		{
			if (!((buf[j] >='0' && buf[j]<='9') || (buf[j]==' ')))
			{
				numb=0;
				break;
			}
		}
	}
	fseek(ff,0,0);
	return numb;
}
