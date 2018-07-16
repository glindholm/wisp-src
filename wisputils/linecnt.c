static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/********************************************************************************************************************************
 *																*
 *  linecount															*
 *  Count the lines in a COBOL program which will be processed by WISP.  In other words count all lines except comments,	*
 *  ("*" or "/" in column 7), and blank lines, (all whitespace from column 7 to 72). User enters foreign command:		*
 *  $ linecount file_specification												*
 *  ...where file_specification can have wild cards										*
 *																*
 *	input:	file specification												*
 *	output: informational message to sys$output indicating total lines in file, comment or purely cosmetic lines in file	*
 *		and COBOL lines in file.  Totals message to sys$output at end.							*
 *																*
 ********************************************************************************************************************************/
#include <descrip.h>
#include <rmsdef.h>
#include <stdio.h>

#define MAXBUF 256
#define HT '\t'									/* Define horizontal tab.			*/
#define SP ' '									/* Define space character.			*/
#define CHAR_NULL 0								/* Define null character.			*/

main(argc,argv)

int	argc;
char	*argv[];

{
long	status, line_count = 0, cobol_lines = 0;
long	total_line_count = 0, total_cobol_lines = 0;
char    template[MAXBUF],							/* user entered file specification, wildcard OK	*/
	result_spec[MAXBUF],							/* file specification after wildcard resolved	*/
	*context;
int	i;

#pragma NOSTANDARD
$DESCRIPTOR(template_desc, template);
$DESCRIPTOR(result_desc, result_spec);
#pragma STANDARD

	memset(template,'\0',sizeof(template));						/* Prepare template for system services	*/
	switch (argc)									/* Input handling logic			*/
	{
		case 1:		get_file_spec(template);				/* No file specification entered	*/
				break;
		case 2:		strcpy(template,argv[1]);				/* Single file specification entered	*/
				break;
		default:	printf("%%LINECOUNT-F-TOOMANYARGS Too many arguments\n");
				return;
	}
	context = 0;
	do
	{
		status = LIB$FIND_FILE(&template_desc,&result_desc,&context,0,0,0,0);		/* process as if wildcarded	*/
		if (status == RMS$_NORMAL)
		{
			i = strpos(result_spec," ");
			result_spec[i] = '\0';				/* system services do not return null terminated string	*/
			count_lines(result_spec,&line_count,&cobol_lines);
			total_line_count += line_count;
			total_cobol_lines += cobol_lines;
		}
	}while (status == RMS$_NORMAL);

	printf("\n%%LINECOUNT-I-TOTALS  %s (total lines: %d; total comments: %d; total COBOL: %d)\n",
		template, total_line_count, total_line_count - total_cobol_lines, total_cobol_lines);
	status = LIB$FIND_FILE_END(&context);
}

/********************************************************************************************************************************
 *	get_file_spec														*
 *	Get file specification from sys$input											*
 *	input:	user entry													*
 *	output:	null terminated character string containing file specification							*
 *	return:	number of characters in file specification string								*
 ********************************************************************************************************************************/

int get_file_spec(file_name)

char	*file_name;
{
char	c;
int	i = 0;

	do
	{
		printf("_File:  ");
		while ((c=getchar ()) != '\n' && c != EOF)
			if (i < MAXBUF - 1) file_name[i++] = c;
	}while (file_name[0] == '\0');

	file_name[i] = CHAR_NULL;							/* always terminate with null		*/
	return i;

}
/********************************************************************************************************************************
 *	count_lines														*
 *	Count lines in given file.												*
 *	total_lines = total number of lines in file										*
 *	wisp_lines  = total_lines minus comment ("*" in col 7), form-feed ("/" in col 7), and blank lines.  (i.e. all lines	*
 *		      to be processed by WISP)											*
 *	return: -1 on error													*
 ********************************************************************************************************************************/

int count_lines(file_name, total_lines, wisp_lines)

char	*file_name;
int	*total_lines, *wisp_lines;								/* line counters		*/
{
	FILE	*file_ptr;
	char    buffer[MAXBUF];								/* characters read from file	*/
	int	trim_length;									/* length of compilable line	*/

	*total_lines = 0;
	*wisp_lines = 0;
	if ((file_ptr = fopen(file_name, "r")) == CHAR_NULL)
		printf("%%LINECOUNT-F-CANTOPEN Unable to open file %s for input\n", file_name);
	else
	{
		while (fgets(buffer, MAXBUF, file_ptr))
		{
	 		(*total_lines)++;
			buffer[72] = CHAR_NULL;
			trim_length = trim(buffer);
			if (trim_length > 7 && buffer[6] != '*' && buffer[6] != '/') (*wisp_lines)++;
		}
		fclose(file_ptr);
		printf("%%LINECOUNT-I-FILE %s (lines: %d; comments: %d; COBOL: %d)\n",
			file_name, *total_lines, *total_lines - *wisp_lines, *wisp_lines);
	}
	return(1);
}

/********************************************************************************************************************************
 *	trim															*
 *	string = character string to remove trailing white space from								*
 ********************************************************************************************************************************/

int trim(string) char string[];									/* Trim trailing blanks.	*/
{
	register int i;
	for (i = strlen(string)-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT) || (string[i] == CHAR_NULL)); i--)
	{
		string[i] = CHAR_NULL;
	}
	return(i+1);										/* Return string length.	*/
}
/*
**	History:
**	$Log: linecnt.c,v $
**	Revision 1.8  1996-07-23 14:12:54-04  gsl
**	drcs update
**
**
**
*/
