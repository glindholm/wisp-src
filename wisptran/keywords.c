			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		keywords.c
**
**	Purpose:	To ...
**
**	Routines:	
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#include <stdio.h>

#include "wmalloc.h"

int keyword(the_word,the_list)						/* search a list of keywords to see if the_word is in	*/
char *the_word;								/* the list						*/
char *the_list[];
{
	int 	i;
	char	**list_ptr;

	for(i=0, list_ptr = the_list; **list_ptr; i++, list_ptr++)	/* this routine will return keyword numbers starting	*/
	{								/* with 1, not 0.					*/
		if (*the_word == **list_ptr)				/* Is first char the same ?				*/
		{
			if (0==strcmp(the_word,*list_ptr)) 
				return(i+1);				/* found a match					*/
		}		
	}

	return(0);							/* no match						*/
}

/*
**	Routine:	binkeyword()
**
**	Function:	To check if word is in a sorted list of keywords.
**
**	Description:	Does a binary search of a list of keywords.
**
**	Arguments:
**	the_word	The word to search for
**	the_list	The SORTED list of keywords
**	numitems	The number of items in list.
**
**	Globals:	None
**
**	Return:
**	0		Not found
**	pos		The position in the list.
**
**	Warnings:	None
**
**	History:	
**	05/17/93	Written by GSL
**
*/
int binkeyword(the_word,the_list,numitems)
char	*the_word;
char	*the_list[];
int	numitems;
{
	int	low,curr,high;
	int	rc;

	low = 0;
	high = numitems-1;

	while(low <= high)
	{
		if (low == high)
		{
			curr = low;
		}
		else
		{
			curr = low + (high-low)/2;
		}

		rc = strcmp(the_word,the_list[curr]);

		if (rc == 0) return(curr+1);					/* found a match				*/

		if (rc < 0)
		{
			high = curr - 1;
		}
		else
		{
			low = curr + 1;
		}
	}
	return(0);
}

/* 					This is a list of keywords which begin something					*/

static char *proc_keywords[] = 	
{
	"ACCEPT", "ADD","ALTER",
	"BEGIN",
	"CALL","CANCEL","CLOSE","COMMIT","COMPUTE","CONTINUE",
	"DELETE","DISABLE","DISPLAY","DIVIDE",
	"ELSE","ENABLE","ENTER","EXIT",
	"FREE",
	"GENERATE","GO",
	"HOLD",
	"IF","INITIALIZE","INITIATE","INSPECT",
	"MERGE","MOVE","MULTIPLY",
	"NEXT",
	"OPEN",
	"PERFORM",
	"READ","READY","RECEIVE","RELEASE","RESET","RETURN","REWRITE","ROLLBACK",
	"SEARCH","SEND","SET","SORT","START","STOP","STRING","SUBTRACT","SUPRESS",
	"TERMINATE",
	"UNSTRING",
	"WHEN","WRITE",
	""
};

int proc_keyword(the_word)
char *the_word;
{
	static int count = 0;

	if (0==count)
	{
		for(;proc_keywords[count][0];count++);
	}

	return(binkeyword(the_word,proc_keywords,count));
}

static char *brk_keywords[] = 	
{
	"ACCEPT",
	"CALL","CLOSE","COMMIT",
	"DELETE","DISPLAY",
	"EXIT",
	"FREE",
	"GO",
	"IF",
	"MOVE",
	"OPEN",
	"PERFORM",
	"READ","REWRITE","ROLLBACK",
	"SEARCH","SORT","START","STOP",
	"WHEN","WRITE",
	""
};

int brk_keyword(the_word)
char *the_word;
{
	static int count = 0;

	if (0==count)
	{
		for(;brk_keywords[count][0];count++);
	}

	return(binkeyword(the_word,brk_keywords,count));
}


/*				These are COBOL-85 reserved words that may appear as field names in WANG COBOL			*/

static char **res_keywords;

static char *res_defaults[] =	
{
	"BELL",	"BEEP",	"BEGINNING","BLINK","BLINKING",
	"CLASS","CRT",	"CURRENT",
	"DAY-OF-WEEK",	"DB", "DEFAULT",
	"EMPTY",
	"FALSE",
	"ID",
	"LENGTH",
	"MATCH","MESSAGE",
	"NAME",	"NO-ECHO",
	"OTHER",
	"PROCESS",
	"RESEND","RETURN-CODE",
	"SEQUENCE-NUMBER","SCREEN","SHUT-DOWN",	"STANDBY",
	"TAB","TERMINAL","TRUE",	
	"WAIT",
	""
};

strcmpptr(p1,p2)
char	**p1, **p2;
{
	return(strcmp(*p1,*p2));
}

int reserved_keyword(the_word)
char *the_word;
{
	static int count = 0;

	if (0==count)
	{
		for(;res_keywords[count][0];count++);

		qsort(res_keywords, count, sizeof(char *), strcmpptr);
	}

	return(binkeyword(the_word,res_keywords,count));
/*	return(keyword(the_word,res_keywords)); */
}

#define MAX_RESWORDS 100

int load_res_keywords(filename)
char	*filename;
{
	FILE	*fd;
	char	*ptr;
	char	buf[256];
	int	rc,i,j;

	if ( !filename || !filename[0] )
	{
		res_keywords = (char **)res_defaults;
		return(0);
	}

	fd = fopen(filename, "r");		
	if (!fd)
	{
		printf("Unable to open WORD FILE %s\n",filename);
		exit_with_err();
	}

	res_keywords = (char **)wmalloc(MAX_RESWORDS * sizeof(char *));

	for(i=0;i<MAX_RESWORDS;i++)
	{
		rc = fscanf(fd,"%s",buf);
		if ( rc != 1 ) break;
		uppercase(buf);
		res_keywords[i] = (char *)wdupstr(buf);
	}
	fclose(fd);
	res_keywords[i] = "";	
}


/*
**	List of all COBOL keywords
**
**	*** Must be kept SORTED ***
*/
static char *verb_keywords[] = 	
{
	"ACCEPT", "ADD","ALTER",
	"BEGIN",
	"CALL","CLOSE","COMMIT","COMPUTE","CONTINUE","COPY",
	"DELETE","DISPLAY","DIVIDE",
	"ENTER","EVALUATE","EXIT",
	"FREE",
	"GO",
	"HOLD",
	"IF","INITIALIZE","INSPECT",
	"MERGE","MOVE","MULTIPLY",
	"OPEN",
	"PERFORM",
	"READ","RELEASE","RETURN","REWRITE","ROLLBACK",
	"SEARCH","SET","SORT","START","STOP","STRING","SUBTRACT",
	"UNSTRING","USE",
	"WRITE",
	""
};

int verb_keyword(the_word)
char *the_word;
{
	static int count = 0;

	if (0==count)
	{
		for(;verb_keywords[count][0];count++);
	}

	return(binkeyword(the_word,verb_keywords,count));
}

/*
**	List of all COBOL keywords
**
**	*** Must be kept SORTED ***
*/
static char *cobol_keywords[] = 	
{
	"ACCEPT", "ACCESS", "ADD", "ADDRESS", "ADVANCING", "AFTER", "ALARM", "ALL", "ALLOWING", "ALPHABET", 
		"ALPHABETIC", "ALPHABETIC-LOWER", "ALPHABETIC-UPPER", "ALPHANUMERIC", "ALPHANUMERIC-EDITED", 
		"ALSO", "ALTER", "ALTERED", "ALTERNATE", "AND", "ANY", "APPLY","ARE", "AREA", "AREAS", 
		"ASCENDING", "ASSIGN", "AT", "AUTHOR", "AUTO", "AUTO-SKIP", "AUTOMATIC", "AUTOTERMINATE",
	"BACKGROUND", "BEEP", "BEFORE", "BEGIN", "BELL", "BINARY", "BLANK", "BLINK", "BLOCK", "BLOCKING", 
		"BLOCKS", "BOLD", "BOTTOM", "BOX", "BOXED", "BUFFER", "BY", 
	"CALL", "CANCEL", "CD", "CENTERED", "CF", "CH", "CHAIN", "CHAINING", "CHARACTER", "CHARACTERS", 
		"CLASS", "CLOCK-UNITS", "CLOSE", "COBOL", "CODE", "CODE-SET", "COL", "COLLATING", "COLOR", 
		"COLUMN", "COMMA", "COMMAND-LINE", "COMMIT", "COMMON", "COMMUNICATION", "COMP", "COMPRESSED", 
		"COMPRESSION", "COMPUTATIONAL", "COMPUTE", "CONFIGURATION", "CONSOLE", "CONTAINS", "CONTENT", 
		"CONTINUE", "CONTROL", "CONTROLS", "CONVERSION", "CONVERT", "CONVERTING", "COPY", "CORR",
		"CORRESPONDING", "COUNT", "CRT", "CURRENCY", "CURSOR", 
	"DATA", "DATE", "DATE-COMPILED", "DATE-WRITTEN", "DAY", "DAY-OF-WEEK", "DE", "DEADLOCK", "DEBUG-CONTENTS", 
		"DEBUG-ITEM", "DEBUG-LINE", "DEBUG-NAME", "DEBUG-SUB-1", "DEBUG-SUB-2", "DEBUG-SUB-3", "DEBUGGING", 
		"DECIMAL-POINT", "DECLARATIVES", "DEFAULT", "DELETE", "DELETION", "DELIMITED", "DELIMITER", 
		"DEPENDING", "DESCENDING", "DESTINATION", "DETAIL", "DISABLE", "DISPLAY", "DISPLAY-WS", 
		"DIVIDE", "DIVISION", "DOWN", "DUPLICATES", "DYNAMIC", 
	"ECHO", "EGI", "ELSE", "EMI", "EMPTY-CHECK", "ENABLE", "ENCRYPTION", "END", "END-ACCEPT", "END-ADD", 
		"END-BEGIN", "END-CALL", "END-CHAIN", "END-COMMIT", "END-COMPUTE", "END-DELETE", 
		"END-DISPLAY", "END-DIVIDE", "END-EVALUATE", "END-FREE", "END-HOLD", "END-IF", "END-MOVE", "END-MULTIPLY", 
		"END-OF-PAGE", "END-PERFORM", "END-READ", "END-RECEIVE", "END-RETURN", "END-REWRITE", "END-ROLLBACK", 
		"END-SEARCH", "END-START", "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE", "ENTER", "ENTRY", 
		"ENVIRONMENT", "EOL", "EOP", "EOS", "EQUAL", "ERASE", "ERROR", "ESCAPE", "ESI", "EVALUATE", 
		"EVERY", "EXCEPTION", "EXCLUSIVE", "EXIT", "EXTEND", "EXTENSION-RIGHTS", "EXTERNAL", 
	"FAC", "FALSE", "FD", "FIGURATIVE-CONSTANTS", "FILE", "FILE-CONTROL", "FILE-ID", "FILE-PREFIX", 
		"FILLER", "FINAL", "FIRST", "FOOTING", "FOR", "FREE", "FROM", "FULL", "FUNCTION", 
	"GENERATE", "GIVING", "GLOBAL", "GO", "GOBACK", "GREATER", "GRID", "GROUP", 
	"HASHSIZE", "HEADING", "HIGH", "HIGH-VALUE", "HIGH-VALUES", "HIGHLIGHT", "HOLD", "HOLDER-ID", 
	"I-O", "I-O-CONTROL", "IDENTIFICATION", "IF", "IN", "INDEX", "INDEXED", "INDICATE", "INITIAL", "INITIALIZE", 
		"INITIATE", "INPUT", "INPUT-OUTPUT", "INSPECT", "INSTALLATION", "INTO", "INVALID", "INVOKE", "IS", 
	"JUST", "JUSTIFIED", 
	"KEPT", "KEY", "KEYS", 
	"LABEL", "LAST", "LEADING", "LEFT", "LEFTLINE", "LENGTH", "LENGTH-CHECK", "LESS", "LIMIT", "LIMITS",
		"LINAGE", "LINAGE-COUNTER", "LINE", "LINE-COUNTER", "LINES", "LINKAGE", "LIST", "LOCK", 
		"LOCK-HOLDING", "LOW", "LOW-VALUE", "LOW-VALUES", "LOWER", "LOWLIGHT", 
	"MANUAL", "MASS-UPDATE", "MEMORY", "MERGE", "MESSAGE", "MODE", "MODIFIABLE", "MODIFY", "MODULES", 
		"MOVE", "MULTIPLE", "MULTIPLY", 
	"NATIVE", "NEGATIVE", "NEXT", "NO", "NO-ECHO", "NO-MOD", "NODISPLAY", "NORESPECIFY", "NORMAL", "NOT", 
		"NULL", "NULLS", "NUMBER", "NUMERIC", "NUMERIC-EDITED", "NUMERIC-FILL", 
	"OBJECT", "OBJECT-COMPUTER", "OCCURS", "OF", "OFF", "OMITTED", "ON", "ONLY", "OPEN", "OPTIONAL", 
		"OR", "ORDER", "ORDER-AREA", "ORGANIZATION", "OTHER", "OTHERS", "OUTPUT", "OVERFLOW", "OVERLINE",
	"PACKED-DECIMAL", "PADDING", "PAGE", "PAGE-COUNTER", "PERFORM", "PF", "PFKEY", "PFKEYS", "PH", "PIC", "PICTURE", 
		"PLUS", "POINTER", "POP-UP", "POS", "POSITION", "POSITIVE", "PREVIOUS", "PRINT-CONTROL", "PRINTING", 
		"PRIOR", "PROCEDURE", "PROCEDURES", "PROCEED", "PROGRAM", "PROGRAM-ID", "PROMPT", "PROTECT",
		"PROTECTED", "PURGE", 
	"QUEUE", "QUOTE", "QUOTES",
	"RANDOM", "RANGE", "RD", "READ", "READERS", "READY", "RECEIVE", "RECORD", "RECORDING", "RECORDS",
		"REDEFINES", "REEL", "REFERENCE", "REFERENCES", "RELATIVE", "RELEASE", "REMAINDER", "REMARKS",
		"REMOVAL", "RENAMES", "REPLACE", "REPLACING", "REPORT", "REPORTING", "REPORTS", "REQUIRED",
		"RERUN", "RESERVE", "RESET", "RESIDENT", "RESPECIFY", "RESTART", "RETRIEVAL", "RETURN", "RETURN-CODE", 
		"REVERSE", "REVERSE-VIDEO", "REVERSED", "REWIND", "REWRITE", "RF", "RH", "RIGHT", "ROLL", 
		"ROLLBACK", "ROUNDED", "ROW", "RUN", 
	"SAME", "SCREEN", "SCROLL", "SD", "SEARCH", "SECOND", "SECONDS", "SECTION", "SECURE", "SECURITY", 
		"SEGMENT", "SEGMENT-LIMIT", "SELECT", "SEND", "SENTENCE", "SEPARATE", "SEQUENCE", "SEQUENTIAL", 
		"SET", "SETTING", "SHADOW", "SHARED", "SIGN", "SIZE", "SORT", "SORT-MERGE", "SOURCE", 
		"SOURCE-COMPUTER", "SPACE", "SPACES", "SPECIAL-INPUT", "SPECIAL-NAMES", "STANDARD", 
		"STANDARD-1", "STANDARD-2", "START", "STATUS", "STOP", "STRING", "SUB-QUEUE-1", "SUB-QUEUE-2", 
		"SUB-QUEUE-3", "SUBTRACT", "SUBTRANSACTION", "SUM", "SUPPRESS", "SYMBOLIC", "SYNC", 
		"SYNCHRONIZED", "SYSTEM-INFO",
	"TAB", "TABLE", "TALLYING", "TAPE", "TERMINAL", "TERMINAL-INFO", "TERMINATE", "TEST", "TEXT", "THAN", 
		"THEN", "THROUGH", "THRU", "TIME", "TIMEOUT", "TIMES", "TITLE",	"TO", "TOP", "TRACE", "TRAILING", 
		"TRANSACTION", "TRUE", "TYPE", 
	"UNDERLINE", "UNDERLINED", "UNIT", "UNLOCK", "UNSTRING", "UNTIL", "UP", "UPDATE", "UPDATES", 
		"UPON", "UPPER", "USAGE", "USE", "USING", 
	"VALUE", "VALUES", "VARYING", 
	"WHEN", "WINDOW", "WITH", "WORDS", "WORKING-STORAGE", "WRAP", "WRITE", "WRITERS", 
	"ZERO", "ZERO-FILL", "ZEROES", "ZEROS", 
	""
};

int cobol_keyword(the_word)
char *the_word;
{
	static int count = 0;

	if (0==count)
	{
		for(;cobol_keywords[count][0];count++);
	}

	return(binkeyword(the_word,cobol_keywords,count));
}
