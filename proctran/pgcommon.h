/* PGCOMMON.H															*/
/*		 Common variable definitions											*/

#define PROCTRAN_VERSION	"V3.1b"							/* Procedure Translatore Version.	*/

#define HT		'\t'								/* Define horizontal tab.		*/
#define SP		' '								/* Define space character.		*/
#define LNFD		(char)0x0a							/* Define line feed character.		*/

#ifndef NULL
#define NULL		'\0'								/* Standard null character def.		*/
#endif

#ifndef CHAR_NULL									/* Use so VMS compiler likes it.	*/
#define CHAR_NULL	'\000'								/* Standard null character def.		*/
#endif

#ifndef FALSE
#define FALSE		0								/* Standard false definition.		*/
#endif

#ifndef TRUE
#define TRUE		!FALSE								/* Standard true definition.		*/
#endif

#define FAILURE		0								/* Standard failure definition.		*/
#define SUCCESS		1								/* Standard success definition.		*/

#define WPLMAX		71								/* Max len of procedure source line.	*/
#define COBBUF		74								/* Maximum length of COBOL buffer.	*/
#define STRBUFF		256								/* Maximum length of STRING.		*/
#define FLDLEN		20								/* Length of field.			*/
#define CNTTAB		15								/* Nul name tab for pic.		*/

/* Define some CASE symbols for finding key words.										*/

#define ASSIGN		0
#define CALL		2
#define DECLARE		4
#define END		9
#define EXTRACT		12
#define GOTO		13
#define IF		14
#define LOGOFF		17
#define MESSAGE		18
#define PRINT		23
#define PROMPT		24
#define RENAME		26
#define RETURN		27
#define RUN		28
#define SCRATCH		29
#define SET		30
#define SUBMIT		33
#define PROCEDURE	35
#define PROC		36

#define CODE		0
#define DISPLAY		1
#define ENTER		2
#define ERROR		3
#define CANCEL		4

#define CLASS		0
#define DISP		1
#define STATUS		2
#define DUMP		3
#define CPULIMIT	4
#define ACTION		5
#define ENVIRONMENT	6
#define GLOBALS		7

#define USING		0 
#define INTEGER		1
#define STRING		2
#define INITIAL		3
#define INIT		4
#define GLOBAL		5

#define IN		1
#define ON		2
#define USE		3

#define AND		0
#define OR		1
#define NOT		2
#define EXISTS		3
#define FILENAME	4
#define LIBRARY		5
#define VOLUME		6
#define IFIN		7
#define IFON		8
#define IFLT		9
#define IFGT		10
#define IFNE		11
#define IFEQ		12
#define IFGE		13
#define IFLE		14
#define IFNEQ		15
#define IFNLT		16
#define IFNGT		17

#define RSLIBRARY	0
#define RSIN		1
#define RSON		2
#define RSTO		3
#define RSLIB		4

#define CENTER		0
#define BLANK		1
#define BLINK		2
#define BRIGHT		3
#define DIM		4
#define LINE		5
#define NUMERIC		6
#define TAB		7
#define UPLOW		8
#define UPPER		9

#define ROW		10
#define PFKEY		11
#define CURCOL		12
#define CURROW		13
#define ALARM		14
#define ERASE		15

/*		 Define FAC bits for mask											*/

#define FMCENTER	0x0001 
#define FMBLANK		0x0002
#define FMBLINK		0x0004
#define FMBRIGHT	0x0008
#define FMDIM		0x0010
#define FMLINE		0x0020
#define FMNUMERIC	0x0040
#define FMTAB		0x0080
#define FMUPLOW		0x0100
#define FMUPPER		0x0200
