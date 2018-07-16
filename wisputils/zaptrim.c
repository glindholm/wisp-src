static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*						Include standard header files.							*/

#include <ctype.h>										/* Get character type macros.	*/
#include <stdio.h>										/* Reference standard I/O.	*/

/*						Local definitions.								*/

#define HT '\t'											/* Define horizontal tab.	*/
#define LF '\x0A'										/* Define line feed.		*/
#define SP ' '											/* Define space character.	*/
#define FNAMLEN 256										/* Length of file name.		*/
#define MAXBUF	82										/* Maximum length of buffer.	*/
/*						Static data									*/

FILE *in, *out;									/* Reference input and output.	*/
/*						Entry point.									*/

main(argc,argv)
int argc;
char *argv[];
{
	char file [FNAMLEN];
	char inline[MAXBUF];									/* Input line buffer.		*/
	char new_lib[8];									/* New library name.		*/
	int zap,trim;										/* 0 - zap  trim - length.	*/
	void search_record();

	if (argc > 3)										/* If input library specified..	*/
	{
		strcpy(file,argv[1]);								/* Get the file name.		*/
		zap = atoi(argv[2]);								/* Get the zap variable.	*/
		trim = atoi(argv[3]);								/* Get the trim length.		*/
	}
	else if (argc > 2)									/* If input library specified..	*/
	{
		trim = 0;									/* Do not trim.			*/
		strcpy(file,argv[1]);								/* Get the file name.		*/
		zap = atoi(argv[2]);								/* Get the zap variable.	*/
	}
	else
	{
		printf("%%ZAP_TRIM-F-INVALPAR No file name specified or lengths missing\n");
												/* No parameter, exit.		*/
		return;
	}
	if (!file[0] || (file[0] == '\n')) exit(1);						/* If null or blank line, exit.	*/
	else if ((in = fopen(file,"r")) == NULL) printf("%%ZAP_TRIM-F-NOTFOUND Input file %s not found.\n",file);
	else if ((out = fopen(file,"w")) == NULL) printf("%%ZAP_TRIM-F-CANTOPEN Unable to open output file %s\n",file);
	else
	{
		memset(inline,'\0',MAXBUF);					/* Initialize the buffer.			*/
		if (fgets(inline,MAXBUF,in))					/* Files successfully opened.			*/
		do
		{
			search_record(inline,zap,trim);				/* Make changes to line.			*/
			fputs(inline,out);
			memset(inline,'\0',MAXBUF);				/* Initialize the buffer.			*/
		} while (fgets(inline,MAXBUF,in));
		fclose(in);
		fclose(out);
	}
}

void search_record(string, zap, trim)						/* Search for COPY filename IN statement.	*/
char *string;
int zap,trim;
{
	char tmp_string[MAXBUF];						/* Copy the stuff into a temp string.		*/
	int i,j;								/* Local variable.				*/
	char *scn_ptr;								/* temp_ptr for string variable.		*/
	scn_ptr = string;							/* Initialize pointer to scan input line.	*/
	memset(tmp_string,'\0',MAXBUF);						/* Clean it out each time.			*/
	i = 0;
	do 
	{
		if (trim > 0 && zap >= trim) 
		{
			tmp_string[i++] = '\n';
			tmp_string[i] = '\0';
			break;
		}
		if (string[zap] == '\0' || string[zap] == LF)
		{
			tmp_string[i++] = '\n';
			tmp_string[i] = '\0';
			break;
		}
		tmp_string[i++] = string[zap++];
	}while (i < MAXBUF);							/* Till cont_parse is false.			*/
	memset(string,'\0',MAXBUF);						/* Clean it out just in case.			*/
	strncpy(string,tmp_string,i);						/* put the contents in place.			*/
	return;
}
/*
**	History:
**	$Log: zaptrim.c,v $
**	Revision 1.8  1996/07/23 18:08:35  gsl
**	Add standard Id and log
**	
**
**
*/
