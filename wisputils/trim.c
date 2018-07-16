static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <ctype.h>										/* Get character type macros.	*/
#include <stdio.h>										/* Reference standard I/O.	*/

/*						Local definitions.								*/

#define HT '\t'											/* Define horizontal tab.	*/
#define SP ' '											/* Define space character.	*/
#define NUL '\000'
#define MAXBUF 256

/*						Static data									*/

FILE *in;											/* Reference input and output.	*/
int out;

#define OUTPUT_BUFFER_SIZE	32768
int fo_cnt = 0;
char fo_buff[OUTPUT_BUFFER_SIZE];


/*						Entry point.									*/

main(argc,argv) int argc; char *argv[];
{
	char buffer[MAXBUF];									/* I/O buffer.			*/
	char *bufptr;
	char file[MAXBUF];
	int total_lines = 0;									/* line count accumulators	*/
	int cobol_lines = 0;
	int okdata = 0;
	int endcol = 32767;									/* Ending column number		*/
	int repeat;
	register char c;
	register int i, line;

	if (argc > 1)
	{
		strcpy(file,argv[1]);								/* Get the file name.		*/
		if (argc > 2)									/* Next arg is ending column	*/
		{
			endcol = atoi(argv[2]);
		}
		repeat = 0;
	}
	else
	{
		repeat = 1;
	}


	do
	{
		if (repeat)
		{
			printf("_File: ");
			fgets(file,MAXBUF,stdin);
			i = strlen(file) - 1;
			if (file[i] == '\n') file[i] = '\0';
		}
		okdata = 1;
		if (!file[0] || (file[0] == '\n')) exit(1);					/* If null or blank line, exit.	*/
		else if (!(in = fopen(file,"r"))) printf("%%TRIM-F-Input file %s not found.\n",file);
		else if (!(out = creat(file))) printf("%%TRIM-F-Cannot open output file %s.\n",file);
		else
		{
			i = 0;
			bufptr = &buffer[0];
			while ((*bufptr = getc(in)) != EOF)					/* Repeat until end of file.	*/
			{
				if (i >= MAXBUF)
				{
					printf("%%TRIM-F-Line too long, first 80 chars of line are...\n");
					buffer[79] = NUL;
					printf("%s\n",buffer);
					exit(0);
				}
				if ((*bufptr == '\n') || (*bufptr == '\014'))			/* Is this end of line?		*/
				{
					c = *bufptr;
					*bufptr = NUL;						/* Store a null.		*/
					trim(buffer,i);						/* Trim the string.		*/
					put_line(buffer);
					total_lines++;						/* Increment line accumulators	*/
					if (i > 7 && buffer[6] != '*' && buffer[6] != '/') cobol_lines++;
					bufptr = &buffer[0];
					i = 0;							/* Reset pointer.		*/
					if (c == '\014') put_line("\014");			/* Don't forget the page break.	*/
				}
				else if (i < endcol)						/* And don't go beyond the end.	*/
				{
					bufptr++;
					i++;
				}
			}

			if (okdata)
			{
				if (i != 0)
				{
					*bufptr = NUL;						/* Store a null.		*/
					trim(buffer,i);						/* Trim the string.		*/
					if (buffer[0] != NUL)					/* Unterminated text?		*/
					{
						printf("%%TRIM-W-Unterminated text at end of file, new-line added\n");
						put_line(buffer);
					}
				}
				wflush();
				close(out);							/* Close files and exit.	*/
				fclose(in);
				printf("%%TRIM-I-LINECOUNT (%s total: %d; comment: %d; COBOL: %d)\n",
					file, total_lines,total_lines - cobol_lines,cobol_lines);
				exit(1);							/* Normal exit.			*/
			}
			else
			{
				printf("%%TRIM-F-No data in input file.\n");			/* Report error.		*/
				exit(0);							/* Error exit.			*/
			}
		}
	} while (repeat);									/* Repeat if needed		*/
}

static int trim(string,len) char string[]; int len;						/* Trim trailing blanks.	*/
{
	register int i;

	for (i = len-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT)); i--);
	string[i+1] = NUL;
	return(i+1);										/* Return string length.	*/
}


static put_line(the_line) char *the_line;						/* put a line into the output buffer	*/
{
	int i;

	if (*the_line)									/* If we are reading the main input 	*/
	{										/* and if not a null string		*/
		do
		{
			if (fo_cnt == OUTPUT_BUFFER_SIZE) wflush();			/* buffer full, flush it		*/
			fo_buff[fo_cnt++] = *the_line++;				/* copy each character			*/
		}
		while (*the_line);							/* till we see a null			*/
	}
	if (fo_cnt == OUTPUT_BUFFER_SIZE) wflush();					/* buffer full, flush it		*/
	fo_buff[fo_cnt++] = '\n';							/* Add a newline.			*/
}

static wflush()										/* flush WISP output buffer		*/
{
	int i;

	if (fo_cnt)
	{
		i = write(out,fo_buff,fo_cnt);						/* write it out				*/
		if ((i == -1) || ( i != fo_cnt))					/* Some kind of error.			*/
		{
			printf("%%TRIM-F-Error writing output file.");
			exit(0);
		}
	}
	fo_cnt = 0;
}
/*
**	History:
**	$Log: trim.c,v $
**	Revision 1.8  1996-07-23 14:13:01-04  gsl
**	drcs update
**
**
**
*/
