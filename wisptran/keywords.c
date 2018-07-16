static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

/*
**	File:		keywords.c
**
**	Project:	wisp/tran
**
**	RCS:		$Source:$
**
**	Purpose:	Keyword routines
**
**	Routines:	
*/

/*
**	Includes
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define EXT extern
#include "wisp.h"

#include "wmalloc.h"
#include "keywords.h"
#include "proto.h"


/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/

/*
**	Static Function Prototypes
*/

static int binkeyword(const char *the_word, char *the_list[], int numitems);

static int validate_list(char *the_list[], int cnt)
{
	int	i;

	if (!the_list) return 0;
	
	if (!the_list[0][0]) return 0; /* Not a empty list */

	for (i=1; i<cnt; i++)
	{
		if ( strcmp(the_list[i-1], the_list[i]) >= 0 ) 
		{
			printf("Keywords not in order [%s] >= [%s]\n", the_list[i-1], the_list[i]);
			return 0;
		}
	}
	return 1;
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
static int binkeyword(const char *the_word, char *the_list[], int numitems)
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

int strcmpptr(const void *v1, const void *v2)
{
	char **p1 = (char **)v1;
	char **p2 = (char **)v2;

	return(strcmp(*p1,*p2));
}

/*				These are COBOL-85 reserved words that may appear as field names in WANG COBOL			*/


#define CHANGE_WORDS_ALLOC_CHUNK 200
static char **change_words_list = NULL;
static int change_words_cnt = 0;
static int change_words_limit = 0;
static int change_words_sorted = 0;


static char *reserved_keywords_defaults[] =	
{
	"BELL",	"BEEP",	"BEGINNING", "BLINK", "BLINKING",
	"CRT", "CURRENT",
	"DB",
	"EMPTY",
	"ID",
	"LENGTH",
	"MATCH", "MESSAGE",
	"NAME",	"NO-ECHO",
	"PROCESS",
	"RESEND", "RETURN-CODE",
	"SEQUENCE-NUMBER", "SHUT-DOWN", "STANDBY",
	"TERMINAL",
	""
};

int is_change_word(const char *the_word)
{
	if (!change_words_sorted)
	{
		qsort((void *)change_words_list, (size_t)change_words_cnt, sizeof(char *), strcmpptr);

		assert(validate_list(change_words_list,change_words_cnt));

		change_words_sorted = 1;
	}

	return(binkeyword(the_word,change_words_list,change_words_cnt));
}

/*
**	ROUTINE:	get_change_word()
**
**	FUNCTION:	Get the changed word
**
**	DESCRIPTION:	There are two types of change words:
**			1) COBOL keywords that are being used a data name.   	(KW-dataname)
**			2) WISP generated data names.				(dataname-G)
**
**	ARGUMENTS:	
**	new_word	The changed word returned
**	orig_word	The original word to be changed
**
**	GLOBALS:	None
**
**	RETURN:		new_word
**
**	WARNINGS:	Do not use gen_data_name() because it must work before working-storage done.
**
*/
char* get_change_word(char* new_word, const char *orig_word)
{
	if (cobol_keyword(orig_word))
	{
		sprintf(new_word,"KW-%s",orig_word);
	}
	else
	{
		strcpy(new_word,orig_word);
		new_word[30-2] = '\0';
		strcat(new_word, "-G");
	}

	return new_word;
}

void do_change_word_token(TOKEN *the_token)
{
	char changed_word[80];

	get_change_word(changed_word, token_data(the_token));

	edit_token(the_token,changed_word);
	the_token->type = IDENTIFIER;
}


void add_change_word(const char* new_word)
{
	/*
	**	Add the new keyword
	*/
	if (change_words_cnt >= change_words_limit)
	{
		change_words_limit += CHANGE_WORDS_ALLOC_CHUNK;
		change_words_list = (char **)wrealloc(change_words_list, change_words_limit * sizeof(char *));
	}
	
	change_words_list[change_words_cnt] = (char *)wdupstr(new_word);
	uppercase(change_words_list[change_words_cnt]);
	change_words_cnt++;
	change_words_sorted = 0;
}

void load_change_words(const char *filename)
{
	FILE	*fd;
	char	buf[256];
	int	rc,i;

	change_words_list = NULL;
	change_words_limit = 0;
	change_words_cnt = 0;
	change_words_sorted = 0;

	if ( !filename || !filename[0] )
	{
		/* defaults table is static need to make dynamic */
		for(i=0;reserved_keywords_defaults[i][0];i++)
		{
			add_change_word(reserved_keywords_defaults[i]);
		}
		return;
	}

	fd = fopen(filename, "r");		
	if (!fd)
	{
		printf("Unable to open WORD FILE %s\n",filename);
		exit_with_err();
	}

	for(;;)
	{
		rc = fscanf(fd,"%s",buf);
		if ( rc != 1 ) break;
		add_change_word(buf);
	}
	fclose(fd);
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
	"CALL","CANCEL","CHAIN","CLOSE","COMMIT","COMPUTE","CONTINUE","COPY",
	"DELETE","DESTROY","DISPLAY","DIVIDE",
	"ENTER","EVALUATE","EXIT",
	"FREE",
	"GO","GOBACK",
	"HOLD",
	"IF","INITIALIZE","INQUIRE","INSPECT",
	"MERGE","MOVE","MULTIPLY",
	"OPEN",
	"PERFORM",
	"READ","RELEASE","RETURN","REWRITE","ROLLBACK",
	"SEARCH","SET","SORT","START","STOP","STRING","SUBTRACT",
	"UNLOCK","UNSTRING","USE",
	"WAIT","WRITE",
	""
};

int verb_keyword(const char *the_word)
{
	static int count = 0;

	if (0==count)
	{
		for(;verb_keywords[count][0];count++);

		assert(validate_list(verb_keywords,count));
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
	"BACKGROUND", "BACKWARD", "BEEP", "BEFORE", "BEGIN", "BELL", "BINARY", "BLANK", "BLINK", "BLOCK", "BLOCKING", 
		"BLOCKS", "BOLD", "BOTTOM", "BOX", "BOXED", "BUFFER", "BY", 
	"CALL", "CANCEL", "CD", "CENTERED", "CF", "CH", "CHAIN", "CHAINING", "CHARACTER", "CHARACTERS", 
		"CLASS", "CLOCK-UNITS", "CLOSE", "COBOL", "CODE", "CODE-SET", "COL", "COLLATING", "COLOR", 
		"COLUMN", "COMMA", "COMMAND-LINE", "COMMIT", "COMMON", "COMMUNICATION", "COMP", 
	        "COMP-1","COMP-2","COMP-3","COMP-4","COMP-5","COMP-6","COMP-X",
	        "COMPRESSED", "COMPRESSION", "COMPUTATIONAL", 
	        "COMPUTATIONAL-1","COMPUTATIONAL-2","COMPUTATIONAL-3","COMPUTATIONAL-4","COMPUTATIONAL-5",
	        "COMPUTATIONAL-6","COMPUTATIONAL-X",
	        "COMPUTE", "CONFIGURATION", "CONSOLE", "CONTAINS", "CONTENT", 
		"CONTINUE", "CONTROL", "CONTROLS", "CONVERSION", "CONVERT", "CONVERTING", "COPY", "CORR",
		"CORRESPONDING", "COUNT", "CRT", "CURRENCY", "CURSOR", 
	"DATA", "DATE", "DATE-COMPILED", "DATE-WRITTEN", "DAY", "DAY-OF-WEEK", "DE", "DEADLOCK", "DEBUG-CONTENTS", 
		"DEBUG-ITEM", "DEBUG-LINE", "DEBUG-NAME", "DEBUG-SUB-1", "DEBUG-SUB-2", "DEBUG-SUB-3", "DEBUGGING", 
		"DECIMAL-POINT", "DECLARATIVES", "DEFAULT", "DELETE", "DELETION", "DELIMITED", "DELIMITER", 
		"DEPENDING", "DESCENDING", "DESTINATION", "DESTROY", "DETAIL", "DISABLE", "DISPLAY", "DISPLAY-WS", 
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
	"PACKED-DECIMAL", "PADDING", "PAGE", "PAGE-COUNTER", "PARAGRAPH", "PERFORM", "PF", "PFKEY", "PFKEYS", 
                "PH", "PIC", "PICTURE", 
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

/*
**	List of all ACUCOBOL keywords
**
**	*** Must be kept SORTED ***
*/
static char *acucobol_keywords[] = 	
{
	"ATTRIBUTE", "AUTO-MINIMIZE", "AUTO-RESIZE",
	"BACKGROUND-COLOR", "BACKGROUND-HIGH", "BACKGROUND-LOW", "BIND",
	"CCOL","CELLS","CENTURY-DATE","CENTURY-DAY","CHART","CLINE","CLINES",
	"COMP-N","COMPUTATIONAL-N","CSIZE","CYCLE",
	"DOUBLE",
	"END-MODIFY","END-WAIT","EVENT",
	"FLOAT","FLOATING","FONT","FOREGROUND-COLOR","GRAPHICAL",
	"HANDLE", "HEIGHT", "HELP-ID",
	"ID", "INQUIRE",
	"LINK",
	"MENU",	"MESSAGES","MODAL","MODELESS",
	"OVERLAPPED",
	"POP-UP","PRIORITY","PROPERTY", 
	"RESIZABLE",
	"SIGNED-INT", "SIGNED-LONG", "SIGNED-SHORT","STYLE","SUBWINDOW","SYSTEM",
	"THREAD","THREADS","TITLE-BAR","TRANSACTION-STATUS",
	"UNSIGNED-INT", "UNSIGNED-LONG", "UNSIGNED-SHORT",
	"VISABLE",
	"WAIT",	"WIDTH",
	""
};

int cobol_keyword(const char *the_word)
{
	static int count = 0;
	static int acu_count = 0;
	int	rc;

	if (0==count)
	{
		for(;cobol_keywords[count][0];count++);

		assert(validate_list(cobol_keywords,count));

		if (acu_cobol)
		{
			for(;acucobol_keywords[acu_count][0];acu_count++);

			assert(validate_list(acucobol_keywords,acu_count));
		}
		
	}

	rc = binkeyword(the_word,cobol_keywords,count);

	if (0 == rc && acu_cobol)
	{
		rc = binkeyword(the_word,acucobol_keywords,acu_count);
	}

	return rc;
}

/*
**	List of COBOL keywords that are part of a data description entry
**
**	*** Must be kept SORTED ***
*/
static char *dd_keywords[] = 	
{
	"ASCENDING",
	"BINARY",
	"BLANK",
	"COLUMN",
	"COMP",
	"COMPUTATIONAL",
	"DESCENDING",
	"DISPLAY",
	"DISPLAY-WS",
	"EXTERNAL",
	"FILLER",
	"INDEX",
	"INDEXED",
	"IS",
	"JUST",
	"JUSTIFIED",
	"LEADING",
	"LINE",
	"NOT",
	"OBJECT",
	"OCCURS",
	"PACKED-DECIMAL",
	"PIC",
	"PICTURE",
	"RANGE",
	"REDEFINES",
	"ROW",
	"SEPARATE",
	"SIGN",
	"SOURCE",
	"SYNC",
	"SYNCHRONIZED",
	"TRAILING",
	"USAGE",
	"VALUE",
	""
};
int dd_keyword(const char *the_word)
{
	static int count = 0;

	if (0==count)
	{
		for(;dd_keywords[count][0];count++);

		assert(validate_list(dd_keywords,count));
	}

	return(binkeyword(the_word,dd_keywords,count));
}



#define SYMBOL_TABLE_CHUNK	1000
static int symbol_table_limit = 0;
static int symbol_table_cnt = 0;
static char **symbol_table = NULL;
static int symbol_table_sorted = 0;

void add_user_symbol(const char* name)
{
	if (symbol_table_cnt >= symbol_table_limit)
	{
		/* expand symbol table */

		symbol_table_limit += SYMBOL_TABLE_CHUNK;
		symbol_table = (char **)wrealloc(symbol_table, symbol_table_limit * sizeof(char *));
	}

	symbol_table[symbol_table_cnt] = (char*) wdupstr(name);
	symbol_table_cnt++;
	symbol_table_sorted = 0;
}

int is_symbol(const char *name)
{
	if (!symbol_table_sorted)
	{
		qsort((void *)symbol_table, (size_t)symbol_table_cnt, sizeof(char *), strcmpptr);
		symbol_table_sorted = 1;
	}
	
	return(binkeyword(name,symbol_table,symbol_table_cnt));
}


/*
**	History:
**	$Log: keywords.c,v $
**	Revision 1.13  1999-09-07 10:35:44-04  gsl
**	Fix return type of load_change_words()
**
**	Revision 1.12  1998-03-27 13:37:36-05  gsl
**	fix warning
**
**	Revision 1.11  1998-03-27 10:49:57-05  gsl
**	Change reserved_words processing to change_word routines
**
**	Revision 1.10  1998-03-23 13:46:17-05  gsl
**	Move OLD to old.c
**	Add validate_list() and assert() on every list to ensure sorted.
**	Removed CLASS, DEFAULT, OTHER, SCREEN, WAIT, TAB from reserved keyword list
**	Add add_reserved_keyword() to dynamically add keywords to list.
**	Add change_reserved_keyword() to change a token that is reserved keyword.
**	Add acucobol_keywords[] list which is extension fo acucobol that is
**	used as part of cobol_keyword()
**	Add dd_keywords() which is used to detect data definition keywords
**	as part of the auto adding keywords to reserved list logic.
**	Add symbol table add_user_symbol() and is_symbol() that are used
**	as part of gen_data_name() to prevent generating a name which
**	conflicts with a user defined symbol
**
**	Revision 1.9  1998-03-03 15:35:46-05  gsl
**	update
**
**	Revision 1.8  1998-02-10 16:07:24-05  gsl
**	remove DAY-OF-WEEK from reserved list.
**
**	Revision 1.7  1996-06-24 14:08:49-04  gsl
**	Fix prototypes for NT
**
**	Revision 1.6  1995-05-09 04:36:41-07  gsl
**	Removed TRUE and FALSE from the reserved words list.
**	These are used in COBOL-85 stmts like SET condition TO TRUE.
**
**
**
*/
