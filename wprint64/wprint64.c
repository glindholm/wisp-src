/*
** Copyright (c) 2010, Shell Stream Software LLC. All Rights Reserved.
*/



/*
**	Includes
*/
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <conio.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "getopt.h"

/*
**	Structures and Defines
*/

#define PRINT_RC_0_SUCCESS			0
#define PRINT_RC_20_FILE_NOT_FOUND		20
#define PRINT_RC_24_INVALID_FILETYPE		24
#define PRINT_RC_28_ACCESS_DENIED		28
#define PRINT_RC_40_INVALID_PARAM		40
#define PRINT_RC_99_QUEUE_NOT_AVAILABLE		99

/*
**	Globals and Externals
*/
#define WERRCODE(x)	((int)(x))			/* Source code marker for error codes		*/

static void WL_werrlog_error(int errcode, const char* routine, const char* code, const char* format, ... /* args */);
static void WL_wtrace(const char* routine, const char* code, const char* format, ... /* args */);

/*
**	Static data
*/
static int isTracing = 0;

/*
**	Static Function Prototypes
*/

/*
 * Module-specific global variables.
 */

#define MAXPAGEWIDTH  500
#define MAXPAGEHEIGHT 500

static BOOL CALLBACK AbortProc( HDC hDC, int Error );
static HDC GetDefPrinterDC(char szDefaultPrtName[]);
static void InitDocStruct( DOCINFO* di, char* szDocname);
static void DrawStuff( HDC hDC, char *page[], int nPageHeight, int nTopMargin, int nFontHeight, int nFontSpace);

static BOOL GetPage(FILE *infile, char *page[], int nPageHeight, int nPageWidth, 
		    int nLeftMargin, BOOL *bSavedData, char szSavedLine[], BOOL bWrapText);

static void SetupForm(HDC hDC,  int nReqCPI, int nReqLPI, LPCTSTR lpszFace, int nPoints,
		      int *lpnFontHeight, int *lpnFontWidth, int *lpnFontSpace, HFONT *lpnTheFont);

static void ParseFormstr(char *lpszFormstr,int *lpnPageWidth,int *lpnPageHeight,
			 int *lpnReqCPI,int *lpnReqLPI,BOOL *lpbLandscape,int *lpnDuplex, 
			 BOOL *lpbRawPrintData, int *lpnTopMargin, int *lpnLeftMargin, 
			 int *lpnPoints, char *lpszFace, BOOL *lpbWrapText);

static BOOL NextPair(char *lpszFormStr, int *lpnPosition, char **lpszKeyword, char **lpszValue);
static DEVMODE *GetDEVMODE(char *printername, HANDLE *phPrinter);
static BOOL PrintRawFile(char* szPtrName, char* file);


static int wprint64_main(char* lpFileName, int nCopies, char* lpPrinterName, char* lpAttrs);

static int WL_fcanread(const char* name);
static int WL_fexists(const char* name);
static INT64 WL_filesize(const char* path);
static char *WL_GetWin32Error(char *szText);

static const char* WPRINT64 = "WPRINT64";
static const char* VERSION = "1.0";

#define MAXARGLEN  500

static void printusage(void)
{
	//      12345678901234567890123456789012345678901234567890123456789012345678901234567890
	printf("\n");
	printf("Copyright 2010 (c) Shell Stream Software LLC, All rights reserved.\n");
	printf("WPRINT64: Version=[%s]\n", VERSION);
	printf("\n");
	printf("A 64-bit command line utility for sending a file to the Windows Print Queue.\n");
	printf("Use this utility on Server 2008 64-bit as a \"Custom Print Queue\" to work\n");
	printf("around 32-bit printing limitations.\n");
	printf("\n");
	printf("Usage: wprint64.exe [options] filename\n");
	printf("Options:\n");
	printf("   -c num-copies       Number of copies to print (default is 1)\n");
        printf("   -p \"printer-name\"   Printer name (enclose in quotes)\n");
        printf("   -a \"form-attrs\"     Form attributes (in quotes)\n");
        printf("   -v                  Verbose (turns on tracing)\n");
        printf("   -?                  Print help\n");
	printf("\n");
	printf("Form Attributes: (same as WISP FORMS file attributes)\n");
	printf("  cpi=10              Characters per inch\n");
	printf("  duplex=0            Duplex printing, 0=One sided, 1=horizontal, 2=vertical\n");
	printf("  face='Courier New'  Font face to use (enclode in single-quotes)\n");
	printf("  landscape=0         Use landscape mode, 0=portrait, 1=landscape\n");
	printf("  lm=0                Left margin\n");
	printf("  lpi=6               Lines per inch\n");
	printf("  lpp=60              Lines per page\n");
	printf("  points=0            Fonts size in points, 0=calculate from cpi and lpi\n");
	printf("  pw=80               Page width\n");
	printf("  raw=0               Use RAW print driver, 0=normal, 1=raw\n");
	printf("  tm=2                Top margin\n");
	printf("  wrap=0              Wrap long lines, 0=truncate, 1=wrap\n");
	printf("\n");
	printf("Examples:\n");
	printf("  wprint64.exe -c 2 -p \"My Printer\" -a \"face='Courier New' cpi=9 landscape=1\" \"path\\MyDocument.txt\"\n");
	printf("\n");
	printf("Notes:\n");
	printf("  If the printer name or the file name contains spaces they must \n");
	printf("  be enclosed in quotes.\n");
	printf("\n");
	printf("WISP OPTIONS File Configuration:\n");
	printf("  PQCMD \"path\\wisp\\bin\\wprint64.exe\" -c %%COPIES%% %%CLASSOPT%% -a \"%%FORMMAP%%\" \"%%FILE%%\"\n");
	printf("  PQCLASSOPT -p \"%%LPMAP%%\"\n");
	printf("  PQNOHOLD\n");
	printf("  PQDELETELOCAL\n");
	printf("\n");
	printf("Exit codes:\n");
	printf("  0 Success\n");
	printf(" 20 File not found\n");
	printf(" 24 Empty file\n");
	printf(" 28 Access Denied\n");
	printf(" 40 Invalid parameter\n");
	printf(" 99 System error: see error message for details\n");
	printf("\n");
}

int main (int argc, char *argv[])
{
	extern	char	*optarg;
	extern	int	optind;
	int	        optionFlag;

	char szFileName[MAXARGLEN];
	int  nCopies;
	char szPrinterName[MAXARGLEN];
	char szFormstr[MAXARGLEN];
	char* lpCommandLine;

	// Initialize parameter vars
	szFileName[0] = 0;
	szPrinterName[0] = 0;
	szFormstr[0] = 0;
	nCopies = 1;
	lpCommandLine = GetCommandLine();
	//printf("CommandLine=[%s]\n",lpCommandLine);

	while ( (optionFlag = getopt( argc, argv, "vc:p:a:?h")) != -1 )
	{
		switch( optionFlag )
		{
			case 'v': // Verbose (Tracing)
				isTracing = 1;
				break;

			case 'c': // Copies
				nCopies = atoi(optarg);
				if (nCopies == 0)
				{
					WL_werrlog_error(WERRCODE(83700), WPRINT64, "BADCOPIES", "Invalid number of copies [%s]", optarg );
					return( PRINT_RC_40_INVALID_PARAM );
				}
				break;

			case 'p': // Printer Name
				strcpy(szPrinterName,optarg);
				break;

			case 'a': // Form Attributes
				strcpy(szFormstr,optarg);
				break;

			case '?': // Help
			case 'h': // Help
			default:
				printusage();
				return( PRINT_RC_40_INVALID_PARAM );
		}
	}

	if (optind >= argc)
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "NOFILE", "No filename" );
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "COMMANDLINE", "[%s]", lpCommandLine );
		printusage();
		return( PRINT_RC_40_INVALID_PARAM );
	}

	if (optind < argc-1)
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "BADARG", "Invalid command line argument [%s]",argv[optind] );
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "COMMANDLINE", "[%s]", lpCommandLine );
		printusage();
		return( PRINT_RC_40_INVALID_PARAM );
	}

	/* Get the filename */
	strcpy( szFileName, argv[argc-1] );

	return wprint64_main(szFileName, nCopies, szPrinterName, szFormstr);
}


/*
**	Routine:	win32_printfile()
**
**	Function:	Print a file from WIN32
**
**	Description:	build printer DC, create document, call appropriate print functions
**
**	Arguments:
**	file		The file to print
**	nCopies		Number of copies
**	formnum		The form number
**	lpclass		The print class
**	printer		The printer number
**
**	Globals:	None
**
**	Return:
**	0		Success
**	20		File not found
**	24		Empty file
**	28		Unable to read
**	60		Invalid mode
**	63		LP command failed
**      96              System Error (document properties 1)
**      97              System Error (document properties 2)
**      98              System Error (setting abort proc)
**      99              System Error (creating printer DC)
**
**	Warnings:	None
**
*/

static int wprint64_main(char *file, int nCopies, char* lpPrinterName, char *szFormstr)
{
	HDC hDC = NULL;
	HDC hDefaultDC = NULL;
	HANDLE hPrinter = INVALID_HANDLE_VALUE;
	DEVMODE *lpInitData = NULL;
	HFONT hTheFont = NULL;
	int nRC;

	char szDefaultPrtName[1024], szPrtName[1024];
	int nFontHeight, nFontWidth, nFontSpace;


	/* form data loaded from FORMS file */
	int 	nReqCPI=10;
	int 	nReqLPI=6;
	int 	nPageHeight=60;			/* Number of lines of text on the page */
	int 	nPageWidth=80;
	BOOL 	bLandscape=FALSE;
	int 	nDuplex=0;
	BOOL 	bRawPrintData=FALSE;
	int 	nTopMargin=2;
	int 	nLeftMargin=0;
	int	nPoints=0;
	BOOL	bWrapText=FALSE;

	char *page[MAXPAGEHEIGHT];
	int nIdx;
	DOCINFO di;	
	FILE *infile;
	BOOL bSavedData=0;
	char szSavedLine[MAXPAGEWIDTH];

	char lpszFace[32] = "Courier New";

	nRC  = PRINT_RC_0_SUCCESS;

	WL_wtrace(WPRINT64,"START","File=[%s] Printer=[%s] Copies=[%d] Formstr=[%s]", file, lpPrinterName, nCopies, szFormstr);

	if ( !WL_fexists(file) )								/* Does file exist?			*/
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "NOFILE", "File does not exist file=[%s]", file);
		return( PRINT_RC_20_FILE_NOT_FOUND );
	}
	if ( !WL_fcanread(file) )								/* Can we read the file?		*/
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "NOACCESS","Unable to read file=[%s]", file);
		return( PRINT_RC_28_ACCESS_DENIED );
	}

	if ( WL_filesize(file) == 0 )
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "EMPTY","Empty file not printed [%s]", file);
		return( PRINT_RC_24_INVALID_FILETYPE );
	}                     

	if (nCopies < 1)
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "NOCOPIES","Nothing to print copies=[%d]", nCopies);
		return( PRINT_RC_40_INVALID_PARAM );
	}

	if (szFormstr && strlen(szFormstr))
	{
		WL_wtrace(WPRINT64,"FORMSTR","Formstr=[%s]", szFormstr);
		ParseFormstr(szFormstr,&nPageWidth,&nPageHeight,&nReqCPI,&nReqLPI,
			     &bLandscape,&nDuplex,&bRawPrintData,&nTopMargin, &nLeftMargin,
			     &nPoints, lpszFace, &bWrapText);
	}
	if (bRawPrintData)
	{
		WL_wtrace(WPRINT64,"RAW", "Raw print mode");
	}
	else
	{
		WL_wtrace(WPRINT64,"FORMSTR", "Face=[%s] CPI=[%d] LPI=[%d] PW=[%d] LPP=[%d] TM=[%d] LM=[%d] Points=[%d]%s%s%s",
		       lpszFace, nReqCPI, nReqLPI, nPageWidth, nPageHeight, nTopMargin, nLeftMargin, nPoints,
		       (bLandscape)?" Landscape":"", (nDuplex)?" Duplex":"", (bWrapText)?" Wrap":"");
	}
	
	if (lpPrinterName && strlen(lpPrinterName))
	{
		strcpy(szPrtName,lpPrinterName);
		WL_wtrace(WPRINT64,"PRTNAME", "NAME=[%s]", szPrtName);
	}
	else
	{
		hDefaultDC = GetDefPrinterDC(szDefaultPrtName);
		if (!hDefaultDC)
		{
			char errtxt1[1024], *errtxt2;
			sprintf(errtxt1,"Error getting DC for default printer");
			errtxt2 = WL_GetWin32Error(errtxt1);
			WL_werrlog_error(WERRCODE(83700), WPRINT64, "DEFAULTPRINTER", "%s", errtxt2 );
			nRC = PRINT_RC_99_QUEUE_NOT_AVAILABLE;
			goto cleanup;
		}
		strcpy(szPrtName,szDefaultPrtName);

		WL_wtrace(WPRINT64,"PRTNAME", "DEFAULT NAME=[%s]", szPrtName);
	}

	/*
	**	Raw printing is done by the PrintRawFile() routine.
	*/
	if (bRawPrintData)
	{
		WL_wtrace(WPRINT64,"PRINTING", "Begin (raw) printing to printer=[%s]", szPrtName);

		while (nCopies>0)
		{
			if (! PrintRawFile(szPrtName, file) )
			{
				nRC = PRINT_RC_99_QUEUE_NOT_AVAILABLE;
				goto cleanup;
			}
			
			nCopies--;
		}			

		goto cleanup;
	}


	lpInitData = GetDEVMODE(szPrtName,&hPrinter);
	if (lpInitData == NULL)
	{
		    nRC = PRINT_RC_99_QUEUE_NOT_AVAILABLE;
		    goto cleanup;
	}

	if (bLandscape)
	{
		lpInitData->dmOrientation = DMORIENT_LANDSCAPE;
		lpInitData->dmFields = DM_ORIENTATION;
	}
	else
	{
		lpInitData->dmOrientation = DMORIENT_PORTRAIT;
		lpInitData->dmFields = DM_ORIENTATION;
	}
	lpInitData->dmDuplex = nDuplex;
	lpInitData->dmFields |= DM_DUPLEX;

	hDC = CreateDC("WINSPOOL",szPrtName,NULL,lpInitData);

	if (!hDC)
	{
		char errtxt1[1024], *errtxt2;
		sprintf(errtxt1,"Error creating DC for printer (%s)",szPrtName);
		errtxt2=WL_GetWin32Error(errtxt1);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "CREATEDC", "%s", errtxt2 );

		nRC = PRINT_RC_99_QUEUE_NOT_AVAILABLE;
		goto cleanup;
	}
	
	if ( SetAbortProc( hDC, AbortProc) == SP_ERROR )
	{
		char *errtxt;
		errtxt = WL_GetWin32Error("Error setting AbortProc");
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "ABORTPROC", "%s", errtxt );

		nRC = PRINT_RC_99_QUEUE_NOT_AVAILABLE;
		goto cleanup;
	}

	SetupForm(hDC, nReqCPI, nReqLPI, lpszFace, nPoints, &nFontHeight, &nFontWidth, &nFontSpace, &hTheFont);

	WL_wtrace(WPRINT64,"PRINTING", "Begin printing to printer=[%s]", szPrtName);

	InitDocStruct(&di, file);
	StartDoc(hDC, &di);
	infile = fopen(file,"r");
	if (infile == NULL)
	{
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "NOACCESS", "No access to file=[%s]", file);
		nRC = PRINT_RC_28_ACCESS_DENIED;
		goto cleanup;
	}
	for (nIdx=0; nIdx<MAXPAGEHEIGHT; ++nIdx)
	{
		page[nIdx]=calloc(1,MAXPAGEWIDTH+1);
	}
	while (nCopies>0)
	{
		fseek(infile,0,SEEK_SET);
		
		while (GetPage(infile,page,nPageHeight,nPageWidth,nLeftMargin,
			       &bSavedData, szSavedLine, bWrapText))
		{
			StartPage(hDC);
			SelectObject(hDC, hTheFont);
			SetBkMode(hDC,TRANSPARENT);
			DrawStuff(hDC,page,nPageHeight,nTopMargin,nFontHeight,nFontSpace);
			EndPage(hDC);
		}
		nCopies--;
	}
	fclose(infile);
	for (nIdx=0; nIdx<MAXPAGEHEIGHT; ++nIdx)
	{	
		free(page[nIdx]);
	}
			
	EndDoc(hDC);

cleanup:
	if (hDefaultDC) DeleteDC(hDefaultDC);
	if (hDC)	DeleteDC(hDC);
	if (INVALID_HANDLE_VALUE != hPrinter)	ClosePrinter(hPrinter);
	if (hTheFont)	DeleteObject(hTheFont);
	if (lpInitData)	free(lpInitData);

	WL_wtrace(WPRINT64,"FINISHED", "Finished with RC=[%d]", nRC);

	return nRC;
}


static void formAttrError(char* lpszKeyword, char* lpszValue)
{
	WL_werrlog_error(WERRCODE(83702), WPRINT64, "FORMSTR", "Bad value for form attribute [%s=%s]", lpszKeyword, lpszValue );
}

/*
**	ROUTINE:	ParseFormstr
**
**	FUNCTION:	get print options from the string in FORMS files
**
**	DESCRIPTION:	get print options from the string in FORMS files
**
**	ARGUMENTS:	
**                      lpszFormstr         string from FORMS file
**                      lpnPageWidth        receives user specified page width (in chars)
**                      lpnPageHeight       receives user specified page len (in lines)
**                      lpnReqCPI           receives user specified chars per inch
**                      lpnReqLPI           receives user specified lines per inch
**                      lpbLandscape        receives user specified orientation
**                      lpnDuplex           receives user specified duplex mode
**			lpbRawPrintData	    receives user specified "raw" flag
**			lpnTopMargin	    Top margin in lines	
**			lpnLeftMargin	    Left margin in columns
**			lpnPoints	    Point size 
**			lpszFace	    Face name
**			lpbWrapText	    Wrap long text flag
**
**
**	GLOBALS:	
**
**	RETURN:		
**
**	WARNINGS:	
**
*/
static void ParseFormstr(char *lpszFormstr,int *lpnPageWidth,int *lpnPageHeight,
			 int *lpnReqCPI,int *lpnReqLPI,BOOL *lpbLandscape,int *lpnDuplex, 
			 BOOL *lpbRawPrintData, int *lpnTopMargin, int *lpnLeftMargin, 
			 int *lpnPoints, char *lpszFace, BOOL *lpbWrapText)
{
	int nPosition, nVal;
	char *lpszKeyword, *lpszValue;
	
#define GOTKW(kw) 0==strcmp(lpszKeyword,(kw))

	nPosition=0;
	while (NextPair(lpszFormstr, &nPosition, &lpszKeyword, &lpszValue))
	{
		if (GOTKW("pw") || GOTKW("pagewidth"))
		{
			nVal = atoi(lpszValue);
			if (nVal > 0)
			{
				*lpnPageWidth = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("pl") || GOTKW("pagelength") || GOTKW("lpp"))
		{
			nVal = atoi(lpszValue);
			if (nVal > 0)
			{
				*lpnPageHeight = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("cpi"))
		{
			nVal = atoi(lpszValue);
			if (nVal > 0)
			{
				*lpnReqCPI = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("lpi"))
		{
			nVal = atoi(lpszValue);
			if (nVal > 0)
			{
				*lpnReqLPI= nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("landscape"))
		{
			nVal = atoi(lpszValue);
			if (nVal == 0 || nVal == 1)
			{
				*lpbLandscape = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("duplex"))
		{
			switch(atoi(lpszValue))
			{
			case 0:
				*lpnDuplex = DMDUP_SIMPLEX;
				break;
			case 1:
				*lpnDuplex = DMDUP_HORIZONTAL;
				break;
			case 2:
				*lpnDuplex = DMDUP_VERTICAL;
				break;
			default:
				formAttrError(lpszKeyword, lpszValue);
				*lpnDuplex = DMDUP_SIMPLEX;
				break;
			}
			continue;
		}
		if (GOTKW("raw"))
		{
			nVal = atoi(lpszValue);
			if (nVal == 0 || nVal == 1)
			{
				*lpbRawPrintData = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("tm"))
		{
			nVal = atoi(lpszValue);
			if (nVal >= 0)
			{
				*lpnTopMargin = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("lm"))
		{
			nVal = atoi(lpszValue);
			if (nVal >= 0)
			{
				*lpnLeftMargin = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("points"))
		{
			nVal = atoi(lpszValue);
			if (nVal > 0)
			{
				*lpnPoints = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("face"))
		{
			if (lpszValue && *lpszValue && (strlen(lpszValue) <= 31) )
			{
				strcpy(lpszFace, lpszValue);
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		if (GOTKW("wrap"))
		{
			nVal = atoi(lpszValue);
			if (nVal == 0 || nVal == 1)
			{
				*lpbWrapText = nVal;
			}
			else
			{
				formAttrError(lpszKeyword, lpszValue);
			}
			continue;
		}
		WL_werrlog_error(WERRCODE(83702), WPRINT64, "FORMSTR", "Bad form attribute [%s]", lpszKeyword );
	}
}


/*
**	ROUTINE:	NextPair()
**
**	FUNCTION:	get next keyword=value pair from a string
**
**	ARGUMENTS:	
**                      lpszFormStr            string we are processing
**                      lpnPosition            current position in the string (retained between calls)
**                      lppszKeyword           receives pointer to char* containing kw
**                      lppszValue             receives pointer to char* containing value
**
**	GLOBALS:	
**
**	RETURN:		TRUE                   got a pair
**                      FALSE                  end of string or got an error
**
**	WARNINGS:	
**
*/
static BOOL NextPair(char *lpszFormStr, int *lpnPosition, char **lppszKeyword, char **lppszValue)
{
	static char lpszKeywordBuffer[32];
	static char lpszValueBuffer[32];
	int nCopyIdx;

#define CURCH lpszFormStr[*lpnPosition] 
#define NEXTCH ++*lpnPosition
#define NULLCH CURCH=='\0'
#define SKIPWHITE while (isspace((int)CURCH)) { NEXTCH; }

	*lppszKeyword=NULL;
	*lppszValue=NULL;
	SKIPWHITE;
	if (NULLCH)
	{
		return FALSE;
	}
	nCopyIdx=0;
	while (!isspace((int)CURCH) && CURCH!='=')
	{
		if (isalpha((int)CURCH))
		{
			lpszKeywordBuffer[nCopyIdx] = tolower(CURCH);
		}
		else
		{
			lpszKeywordBuffer[nCopyIdx] = CURCH;
		}
		++nCopyIdx;
		NEXTCH;
	}
	lpszKeywordBuffer[nCopyIdx] = '\0';
	SKIPWHITE;
	if (CURCH != '=')
	{
		WL_werrlog_error(WERRCODE(83702), WPRINT64, "FORMSTR", "bad syntax near position %d [%s]", *lpnPosition, lpszFormStr );
		return FALSE;
	}
	NEXTCH;
	SKIPWHITE;
	nCopyIdx=0;

	if ('"' == CURCH)
	{
		NEXTCH;
		while (('"' != CURCH) && CURCH != '\0')
		{
			lpszValueBuffer[nCopyIdx] = CURCH;
			
			++nCopyIdx;
			NEXTCH;
		}
		if ('"' == CURCH)
		{
			NEXTCH;
		}
	}
	else if ('\'' == CURCH)
	{
		NEXTCH;
		while (('\'' != CURCH) && CURCH != '\0')
		{
			lpszValueBuffer[nCopyIdx] = CURCH;
			
			++nCopyIdx;
			NEXTCH;
		}
		if ('\'' == CURCH)
		{
			NEXTCH;
		}
	}
	else
	{
		while (!isspace((int)CURCH) && CURCH!= '\0')
		{
			lpszValueBuffer[nCopyIdx] = CURCH;
			
			++nCopyIdx;
			NEXTCH;
		}
	}
	
	lpszValueBuffer[nCopyIdx] = '\0';
	if (strlen(lpszKeywordBuffer)==0 || strlen(lpszValueBuffer)==0)
	{
		WL_werrlog_error(WERRCODE(83702), WPRINT64, "FORMSTR", "bad syntax near position %d [%s]", *lpnPosition, lpszFormStr );
		return FALSE;
	}
	*lppszKeyword=lpszKeywordBuffer;
	*lppszValue=lpszValueBuffer;
	return TRUE;
} 
/*
**	ROUTINE:	SetupForm()
**
**	FUNCTION:	prepare printer settings based on form number
**
**	DESCRIPTION:	setup printer font and page based on form number.
**                      currently hard coded to the following values:
**                        012 - 12 cpi 6 lpi   017 - 16.67 cpi, 8 lpi
**                        010 or anything else 10 cpi 6 lpi
**                      these values should be configurable (as well as
**                      font? , orientation)
**
**	ARGUMENTS:	
**      hDC           	printer DC
**	nReqCPI		Requested CPI
**	nReqLPI		Requested LPI
**	lpszFace	Requested Face
**	nPoints		Point size (if 0 then calculate from LPI & CPI)
**	lpnFontHeight	The font size without inter-line space
**	lpnFontSpace	The inter-line space
**	lpnTheFont	The font
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:	
**
*/
static void SetupForm(HDC hDC,  int nReqCPI, int nReqLPI, LPCTSTR lpszFace, int nPoints,
		      int *lpnFontHeight, int *lpnFontWidth, int *lpnFontSpace, HFONT *lpnTheFont)
{
	TEXTMETRIC current;
	BOOL bSuccess;
	int nLogPelsX, nLogPelsY;
	int nReqHeight, nReqWidth, nFullHeight;
	HFONT hTheFont;
	HGDIOBJ selobjret;

	char errtxt1[1024], *errtxt2;
	
	
	/*
	** get device characteristics for current font, also
	** pixels per inch
	*/
		
	nLogPelsX = GetDeviceCaps(hDC, LOGPIXELSX);
	nLogPelsY = GetDeviceCaps(hDC, LOGPIXELSY);

	/*
	** 	Create a new font and select it; compute the size by dividing
	** 	the device pixels per inch by the desired CPI and LPI
	**
	**	When calculating the font height you must account for the space between lines.
	**	The value passed to CreateFont() is the height of the characters not counting the space.
	**
	**	nFullHeight  - the font height plus the line spacing
	**	nReqHeight   - the font height which is 88/100th of nFullHeight 
	**		       this is the average ratio found in test fonts.
	**		       Pass a negative value for better matching.
	**	nReqWidth    - the average width of a character
	**
	*/
	nFullHeight = nLogPelsY / nReqLPI;

	if (nPoints > 0)
	{
		/*
		**	Points are messured in 72nth of an inch.
		*/
		nReqHeight = (nLogPelsY * nPoints) / 72;
	}
	else
	{
		nReqHeight = (nLogPelsY * 88) / (nReqLPI * 100);
	}
	
	nReqWidth  = nLogPelsX / nReqCPI;

	WL_wtrace(WPRINT64,"CREATEFONT", 
	       "nFullHeight=[%d] nReqHeight=[%d] (LOGPIXELSY[%d] / LPI[%d]), nReqWidth=[%d] (LOGPIXELSX[%d] / CPI[%d]), ",
	       nFullHeight, nReqHeight, nLogPelsY, nReqLPI, nReqWidth,  nLogPelsX, nReqCPI);

	hTheFont = CreateFont(-nReqHeight, nReqWidth, 0, 0, FW_DONTCARE, 0,0,0, DEFAULT_CHARSET,
			     OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
			     FF_MODERN|FIXED_PITCH, lpszFace);
	if (hTheFont == NULL)
	{
		sprintf(errtxt1,"Error creating font \"%s\" [h:%d,w:%d]",lpszFace,nReqHeight,nReqWidth);
		errtxt2 = WL_GetWin32Error(errtxt1);
		
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "CREATEFONT", "%s", errtxt2 );
	}
	selobjret = SelectObject(hDC, hTheFont);
	if (selobjret == (HGDIOBJ)NULL || selobjret == (HGDIOBJ)GDI_ERROR)
	{
		sprintf(errtxt1,"Error calling SelectObject(%d,%d)",hDC,hTheFont);
		errtxt2 = WL_GetWin32Error(errtxt1);
		
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SELECTOBJECT", "%s", errtxt2 );
	}
	
	bSuccess = GetTextMetrics(hDC, &current);
	if (bSuccess==0)
	{
		errtxt2 =  WL_GetWin32Error("GetTextMetrics");
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "GETTEXTMETRICS", "%s", errtxt2 );
	}

	WL_wtrace(WPRINT64,"FONT", "TEXTMETRICS tmHeight=[%d] tmAveCharWidth=[%d] tmInternalLeading=[%d] tmExternalLeading=[%d]",
	       current.tmHeight, current.tmAveCharWidth, current.tmInternalLeading, current.tmExternalLeading);

	/*
	**	There is two ways of the space between lines being identified: internal or external.
	**	Internal space is counted as part of tmHeight where external space is the "recommended" space to use.
	**	From experience it has been found that of tnInternalLeading and tmExternalLeading one will
	**	be 0 and one will be set.
	**	Calculate the height of the font (without space) by subtracting the internal space (may be zero).
	**	Calculate the space by subtracting the font height  (without space) from the desired full height.
	*/

	*lpnFontHeight = current.tmHeight - current.tmInternalLeading;
	*lpnFontWidth = current.tmAveCharWidth;
	*lpnFontSpace = nFullHeight - *lpnFontHeight;
	*lpnTheFont = hTheFont;

	WL_wtrace(WPRINT64,"SETUPFORM", "FontHeight=[%d] FontWidth=[%d] FontSpace=[%d]",
	       *lpnFontHeight, *lpnFontWidth, *lpnFontSpace);	
}

/*
**	ROUTINE:	GetDefPrinterDC()
**
**	FUNCTION:	get a DC for the default printer
**
**	DESCRIPTION:	call the PrintDlg routine to get a DC for the default
**                      printer.. (Flags set to prevent the dialog box from 
**                      actually appearing)
**
**	ARGUMENTS:	void
**
**	GLOBALS:	void
**
**	RETURN:		HDC    DC for the default printer
**
**	WARNINGS:	?
**
*/
static HDC GetDefPrinterDC(char szDefaultPrtName[])
{
        PRINTDLG pdlg;
	DEVNAMES *dn;
	char *base;

	WL_wtrace(WPRINT64,"GETDEFPRINTERDC", "Retreiving DEFAULT Printer Name");

	/*
	** zero init the struct 
	*/
	memset(&pdlg, 0, sizeof(PRINTDLG));
	pdlg.lStructSize = sizeof(PRINTDLG);
	/*
	** these flags cause it to return the default printer DC
	** instead of displaying a dialog box on screen
	**/
	pdlg.Flags = PD_RETURNDEFAULT | PD_RETURNDC;
	if ( 0 == PrintDlg(&pdlg))
	{
		strcpy(szDefaultPrtName, "DEFAULT");
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "PRINTDLG", "%s", WL_GetWin32Error("PrintDlg() failed to get default printer") );

		return NULL;
	}

	dn = GlobalLock(pdlg.hDevNames);
	base = (void*)dn;
	strcpy(szDefaultPrtName,base+dn->wDeviceOffset);
	GlobalUnlock(pdlg.hDevNames);
	return pdlg.hDC;
}

/*
**	ROUTINE:	AbortProc()
**
**	FUNCTION:	abort procedure needed by printing routines
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	
**
**	GLOBALS:	
**
**	RETURN:		
**
**	WARNINGS:	
**
*/
static BOOL CALLBACK AbortProc( HDC hDC, int Error )
{
	MSG msg;
	while (PeekMessage(&msg, NULL, 0,0,PM_REMOVE))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return TRUE;
}
/*
**	ROUTINE:	InitDocStruct()
**
**	FUNCTION:	setup document name
**
**	DESCRIPTION:	setup document name
**
**	ARGUMENTS:	di         pointer to DOCINFO struct
**                      docname    name of document
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:	
**
*/
static void InitDocStruct( DOCINFO *lpDI, char* lpszDocName)
{
	memset( lpDI, 0, sizeof(DOCINFO));
	lpDI->cbSize = sizeof(DOCINFO);
	lpDI->lpszDocName = lpszDocName;
}
/*
**	ROUTINE:	DrawStuff()
**
**	FUNCTION:	write a page to the printer
**
**	DESCRIPTION:	copy the page from the page array to the printer DC
**
**	ARGUMENTS:	
**	hDC           	the printer DC
**      page          	array of char containing current page
**	nPageHeigth	The number of lines
**	nTopMargin	Top margin
**	nFontHeight	The size of the font without interline space
**	nFontSpace 	The size of the interline space.
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:	
**
*/
static void DrawStuff( HDC hDC, char *page[], int nPageHeight, int nTopMargin, int nFontHeight, int nFontSpace)
{
	int 	nIdx;			/* Index into page[] */
	int	nLine;			/* Line number (differs from nIdx because of overstrikes) */
	int	nOS;			/* Overstrike offset */
	size_t	nLen;			/* Lenght of the line */
	char	*pCR;
	
	nOS = 0;
	
	for (nLine=0, nIdx=0; (nLine < nPageHeight) && (nIdx < MAXPAGEHEIGHT) ; ++nIdx)
	{
		nLen = strlen(page[nIdx]);
		
		pCR = strchr(page[nIdx],'\r');
		if (NULL != pCR)
		{
			/*
			**	Remove the trailing CR
			*/
			*pCR = '\0';
			nLen--;
		}
			
		if (nLen > 0)
		{
			TextOut(hDC, nOS, nOS+((nFontHeight+nFontSpace) * (nLine+nTopMargin)) ,page[nIdx], (int)nLen);
		}
		

		if (NULL == pCR)
		{
			/*
			**	This was a regular line so increment the line number
			*/
			nLine++;
			nOS = 0;
		}
		else
		{
			/*
			**	This line ended with a CR so the next line will overstrike
			*/
			nOS += 2;
		}
	}
	
}
/*
**	ROUTINE:	GetPage()
**
**	FUNCTION:	load the page array with the next page from the file
**
**	DESCRIPTION:	Load a page, process tabs, ff, and lf
**			An input "line" is terminated by either a CR,  LF or CR+LF.
**			The page[line] field will be NULL terminated.
**			Overstrikes (AFTER ADVANCING 0 LINES) will have a CR as 
**			the last character in the line.
**
**	ARGUMENTS:	infile    input FILE*
**                      page      array of char* to receive page text
** 
**	GLOBALS:	pagewidth, pageheight
**
**	RETURN:		TRUE      got a page
**                      FALSE     end of file reached
**
**	WARNINGS:	
**
*/
static BOOL GetPage(FILE *infile, char *page[], int nPageHeight, int nPageWidth, 
		    int nLeftMargin, BOOL *bSavedData, char szSavedLine[], BOOL bWrapText)
{
        char	szLine[MAXPAGEWIDTH];
	int	nLine;		/* The line number */
	int 	nIdx;		/* Index into page[] - differs from nLine because of overstrikes */
	int 	nSrcIdx;	/* Index into szLine */
	int	nDestIdx;	/* Index into page[nIdx] */
	int	nDestActualPos; /* Actual column number */
	size_t	nLen;
	
	/*
	** erase last page 
	*/
	for (nIdx=0; nIdx<MAXPAGEHEIGHT; ++nIdx)
	{
		page[nIdx][0]='\0';
	}

	/*
	**	Loop for each line in the page.
	*/	
	for (nLine=0, nIdx=0; (nLine < nPageHeight) && (nIdx < MAXPAGEHEIGHT) ; ++nIdx)
	{
		if (*bSavedData)
		{
			strcpy(szLine,szSavedLine);
			*bSavedData=0;
		}	
		else
		{
			if (feof(infile))
			{
				break;
			}

			if (fgets(szLine,sizeof(szLine),infile)==NULL) 
			{
				break;
			}
		}
		nLen = strlen(szLine);
		
		/*
		**	Add in the left margin
		*/
		nDestIdx = nLeftMargin;
		if (nLeftMargin)
		{
			memset(page[nIdx],' ',nLeftMargin);
		}
		
		nSrcIdx=0; 
		nDestActualPos = 0;

		for (;;)
		{
			/*
			**	First check conditions that will end this line
			*/
			if (szLine[nSrcIdx]=='\0') 		/* Null */
			{
				/*
				**	We should never end a line with NULL unless the line
				**	was too big for fgets() or EOF.
				*/

				if (0 == feof(infile) &&
				    NULL != fgets(szLine,sizeof(szLine),infile))
				{
					/*
					**	We got more of the line so keep going
					*/
					nLen = strlen(szLine);
					nSrcIdx = 0;
				}
				else
				{
					if (nSrcIdx > 0)
					{
						nLine++;
					}
					break;
				}

			}

			if (szLine[nSrcIdx]   == '\r' &&	
			    szLine[nSrcIdx+1] == '\n'    ) /* CR LF  Carrage return Line feed */
			{
				nLine++;
				break;
			}
			else if (szLine[nSrcIdx]=='\n') 	/* LF Linefeed */
			{
				nLine++;
				break;
			}
			else if (szLine[nSrcIdx]=='\r')		/* CR carrage return */
			{
				/* 
				**	You can not overstrike and wrap, so if wrap is set then
				**	overstrike is disabled.
				*/
				if (bWrapText)
				{
					nLine++;
				}
				else
				{
					/*
					**	Add the CR to the end of the line.
					**	Terminate the line.
					**	Don't increment nLine
					*/
					*(page[nIdx]+nDestIdx) = '\r';
					++nDestIdx;
				}
				
				++nSrcIdx;

				*bSavedData = TRUE;
				strcpy(szSavedLine,&szLine[nSrcIdx]);

				break;
			}
			else if (szLine[nSrcIdx]==0x0c) 	/* FF formfeed */
			{
				if (szLine[nSrcIdx+1]) 
				{
					*bSavedData = TRUE;
					strcpy(szSavedLine,&szLine[++nSrcIdx]);
				}

			        nLine=nPageHeight;		/* Force end of page */
				break;
			}

			if ( nDestActualPos >= nPageWidth )
			{
				if (bWrapText)
				{
					*bSavedData = TRUE;
					strcpy(szSavedLine,&szLine[nSrcIdx]);
					nLine++;
					break;
				}
				else
				{
					++nSrcIdx;
					continue;
				}
			}


			/*
			**	Non-terminating conditions
			*/

			if (szLine[nSrcIdx]==0x09)		/* HT Tab */
			{
				*(page[nIdx]+nDestIdx++) = ' ';
				++nDestActualPos;
				
				while (nDestActualPos%8 != 0 && nDestActualPos < nPageWidth)
				{
					*(page[nIdx]+nDestIdx) = ' ';
					++nDestIdx;
					++nDestActualPos;
				}
				++nSrcIdx;
			}
			else
			{
				*(page[nIdx]+nDestIdx) = szLine[nSrcIdx];
				++nDestIdx;
				++nSrcIdx;
				++nDestActualPos;
			}
		}

		/*
		**	NULL terminate this line
		*/
		*(page[nIdx]+nDestIdx) = '\0';
		
	}

	/*
	** 	If 0 lines then got EOF 
	*/
	if (0 == nLine && 0 == nIdx) 
	{
		return FALSE; 
	}
	else 
	{
		return TRUE;
	}
	
}
/*
**	ROUTINE:	GetDEVMODE()
**
**	FUNCTION:	return DEVMODE struct for printer
**
**	DESCRIPTION:	call EnumPrinters to get a list of printers
**                      for a system;  if matching printer is found,
**                      return DEVMODE struct from it's PRINTER_INFO_2 struct
**
**	ARGUMENTS:	printername  char* name of printer
** 
**	GLOBALS:	none
**
**	RETURN:		NULL      EnumPrinters didn't find printer
**                      DEVMODE*  newly malloc'd devmode struct
**
**	WARNINGS:	pointer returned must be free()'d later
**
*/
static DEVMODE *GetDEVMODE(char *printername, HANDLE *phPrinter)
{
	BOOL bSuccess, bFound;
	char *lpPrtEnumData=NULL, *lpEnumName=NULL;
	DWORD obytes, bytes, count;
	PRINTER_INFO_4 *pi4;
	PRINTER_INFO_1 *pi1;
	int idx;
	DEVMODE *retbuf=NULL;
	HWND hWndParent;
	int nStructSize;
	OSVERSIONINFO ovi;
	DWORD dwEnumFlags;
	
	ovi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	bSuccess = GetVersionEx(&ovi);
	if (!bSuccess)
	{
		char *errtxt2;
		errtxt2 = WL_GetWin32Error("GetVersionEx()");
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "GETVERSIONEX", "%s", errtxt2 );
		
		goto gdm_cleanup;
	}
	/*
	** assuming either win95 or winNT.. there is also WIN32s which
	** would be caught in the else code block, but that should never
	** happen..
	*/
	if (ovi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
	{
	    dwEnumFlags = PRINTER_ENUM_LOCAL;
	    obytes=sizeof(PRINTER_INFO_1);
	    lpPrtEnumData = malloc(obytes);
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)1,lpPrtEnumData,
				    (DWORD)obytes,&bytes,&count);
	    /*
	    ** if bytes is > obytes, there is more data, so realloc lpPrtEnumData to the
	    ** indicated size and call again
	    */
	    if (bytes > obytes)
	    {
		free(lpPrtEnumData);
		lpPrtEnumData = malloc(bytes);
	    }
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)1,lpPrtEnumData,
				    (DWORD)bytes,&bytes,&count);
	    if ( !bSuccess )
	    {
		char *errtxt;
		errtxt = WL_GetWin32Error("EnumPrinters");
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "ENUMPRINTERS", "%s", errtxt );
		
		goto gdm_cleanup;
	    }
	}
	else 
	{
	    dwEnumFlags = PRINTER_ENUM_CONNECTIONS|PRINTER_ENUM_LOCAL;
	    obytes=sizeof(PRINTER_INFO_4);
	    lpPrtEnumData = malloc(obytes);
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)4,lpPrtEnumData,
				(DWORD)obytes,&bytes,&count);
	    /*
	    ** if bytes is > obytes, there is more data, so realloc lpPrtEnumData to the
	    ** indicated size and call again
	    */
	    if (bytes > obytes)
	    {
		free(lpPrtEnumData);
		lpPrtEnumData = malloc(bytes);
	    }
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)4,lpPrtEnumData,
				    (DWORD)bytes,&bytes,&count);
	
	    if ( !bSuccess )
	    {
		char *errtxt;
		errtxt = WL_GetWin32Error("EnumPrinters");
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "ENUMPRINTERS", "%s", errtxt );

		goto gdm_cleanup;
	    }
	}
	
	/*	
	** call first with enuff space for a single struct
	*/
	
	/*
	** now loop and look for printername
	*/
	if (ovi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
	{
	    for (bFound=FALSE, idx=0, pi1 =(PRINTER_INFO_1*)lpPrtEnumData; idx<(int)count; ++idx, ++pi1)
	    {
		if (0==strncmp(pi1->pName,printername,strlen(pi1->pName)))
		{
		    /* 
		    ** if printername is found, then call openprinter, documentproperties, etc
		    */
		    bFound = TRUE;
		    lpEnumName = pi1->pName;
		}
	    }
	}
	else
	{
	    for (bFound=FALSE, idx=0, pi4= (PRINTER_INFO_4*)lpPrtEnumData; idx<(int)count; ++idx, ++pi4)
	    {
		if (0==strncmp(pi4->pPrinterName,printername,strlen(pi4->pPrinterName)))
		{
		    /* 
		    ** if printername is found, then call openprinter, documentproperties, etc
		    */
		    bFound = TRUE;
		    lpEnumName = pi4->pPrinterName;
		}
	    }
	}

	if (bFound)
	{
	    bSuccess=OpenPrinter(lpEnumName,phPrinter,NULL);
	    if (!bSuccess)
	    {
		char errtxt1[1024], *errtxt2;
		sprintf(errtxt1,"OpenPrinter(%s)",lpEnumName);
		errtxt2 = WL_GetWin32Error(errtxt1);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", errtxt2 );
		
		goto gdm_cleanup;
	    }

	    hWndParent = NULL;
	    
	    nStructSize = DocumentProperties(hWndParent,*phPrinter,lpEnumName,NULL,NULL,0);
	    if (nStructSize < 0)
	    {
		char errtxt1[1024],*errtxt2;
		sprintf(errtxt1,"DocumentPropreties(structsize) printer(%s)",lpEnumName);
		errtxt2=WL_GetWin32Error(errtxt1);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", errtxt2 );
		
		goto gdm_cleanup;
	    }
	    retbuf = (DEVMODE *) malloc(nStructSize);
	    bSuccess = DocumentProperties(hWndParent,*phPrinter,lpEnumName,retbuf,NULL,DM_OUT_BUFFER);
	    if (bSuccess < 0)
	    {
		char errtxt1[1024],*errtxt2;
		sprintf(errtxt1,"DocumentPropreties(structdata) printer(%s)",lpEnumName);
		errtxt2=WL_GetWin32Error(errtxt1);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", errtxt2 );
		
		free(retbuf);
		retbuf=NULL;
		goto gdm_cleanup;
	    }
	}	
	else
	{
		char errtxt1[1024];
		sprintf(errtxt1,"The printer name \"%s\" was not found.",printername);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "NOTFOUND", "%s", errtxt1 );
	}
	
      gdm_cleanup:
	free(lpPrtEnumData);
	
	return retbuf;
}

/*
**	ROUTINE:	PrintRawFile()
**
**	FUNCTION:	Print a "raw" (fully rendered) file. 
**
**	DESCRIPTION:	Send a fully rendered data file to a printer.
**			This uses the mechanism described in PSS ID # Q138594
**			of the Win32 Knowledge base.
**
**			The file may contains PCL code or maybe postscript or something else.
**
**	ARGUMENTS:	
**	szPrtName	The printer name
**	file		The file to print
**
**	GLOBALS:	None
**
**	RETURN:		
**	TRUE		Successfully printed file
**	FALSE		Failed
**
**	WARNINGS:	Unknown.
**
*/
static BOOL PrintRawFile(char* szPtrName, char* file)
{
	BOOL bSuccess;
	HANDLE hPrinter = INVALID_HANDLE_VALUE;
	char errtxt1[1024];
	DWORD	dwJob = 0;
	DOC_INFO_1  DocInfo1;
	FILE *infile = NULL;
	
	
	/*
	**	Open the Printer to get a handle hPrinter.
	*/
	bSuccess=OpenPrinter(szPtrName,&hPrinter,NULL);
	if (!bSuccess || INVALID_HANDLE_VALUE==hPrinter)
	{
		sprintf(errtxt1,"OpenPrinter(%s)", szPtrName);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", WL_GetWin32Error(errtxt1) );
		return FALSE;
	}

	/*
	**	Load up the DOC_INFO_1 struct with a datatype of "RAW"
	**	and start the printer document
	*/
	DocInfo1.pDocName = file;
	DocInfo1.pOutputFile = NULL;
	DocInfo1.pDatatype = "RAW";
	dwJob = StartDocPrinter(hPrinter, 1, (LPSTR)&DocInfo1);
	if (0 == dwJob)
	{
		sprintf(errtxt1,"StartDocPrinter(%s,1,\"RAW\")", szPtrName);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", WL_GetWin32Error(errtxt1) );

		ClosePrinter(hPrinter);	
		return FALSE;
	}

	/*
	**	Start the page
	*/
	if ( ! StartPagePrinter( hPrinter ))
	{
		sprintf(errtxt1,"StartPagePrinter(%s)", szPtrName);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", WL_GetWin32Error(errtxt1) );

		EndDocPrinter( hPrinter );
		ClosePrinter(hPrinter);	
		return FALSE;
	}

	/*
	**	Open up the file
	*/
	infile = fopen(file,"rb");
	if (!infile)
	{
		sprintf(errtxt1,"fopen(\"%s\",\"rb\") fialed errno=%d",file,errno);
		WL_werrlog_error(WERRCODE(83700), WPRINT64, "FOPEN", "%s", errtxt1 );
		
		EndDocPrinter( hPrinter );
		ClosePrinter(hPrinter);	
		return FALSE;
	}

	/*
	**	Loop while reading thru the file until EOF
	*/
	while (!feof(infile))
	{
		char	buff[1024];
		size_t	cnt;
		DWORD	dwBytesWritten;
		size_t	offset;
		
		/*
		**	Read a block from the file and check for errors
		*/
		cnt = fread(buff,1,sizeof(buff),infile);
		if (ferror(infile))
		{
			sprintf(errtxt1,"fread(\"%s\") fialed errno=%d",file,errno);
			WL_werrlog_error(WERRCODE(83700), WPRINT64, "FREAD", "%s", errtxt1 );

			fclose(infile);
			EndDocPrinter( hPrinter );
			ClosePrinter(hPrinter);	
			return FALSE;
		}

		/*
		**	Write the block to the printer.  
		**     	May need to loop if the bytes written is less then the cnt
		*/
		offset = 0;
		while(cnt > 0)
		{
			if (! WritePrinter( hPrinter, &buff[offset], (DWORD)cnt, &dwBytesWritten))
			{
				sprintf(errtxt1,"WritePrinter(\"%s\",cnt=%d)", szPtrName, cnt);
				WL_werrlog_error(WERRCODE(83700), WPRINT64, "SYS", "%s", WL_GetWin32Error(errtxt1) );

				fclose(infile);
				EndDocPrinter( hPrinter );
				ClosePrinter(hPrinter);	
				return FALSE;
			}
			cnt -= dwBytesWritten;
			offset += dwBytesWritten;
		}
	}

	/*
	**	Done, cleanup everything.
	*/
	fclose(infile);

	EndPagePrinter( hPrinter );
	EndDocPrinter( hPrinter );
	ClosePrinter(hPrinter);	

	return TRUE;
}

static int WL_fcanread(const char* name)
{
	return (0==_access(name,004)) ? 1 : 0;
}

static int WL_fexists(const char* name)
{

	/*
	 *	Use access() because stat() fails on a share name like "\\machine\share".
	 */

	if (0==_access(name,000))
	{
		return 1;
	}
	return 0;
}

static int WL_stat_size_int8(const char* name, INT64 *size)
{
	struct _stat64 buf;
	if ( 0 != _stat64(name,&buf) )
	{
		return -1;
	}

	*size = buf.st_size;
	return 0;
}

static INT64 WL_filesize(const char* path)     /* Return the length of the file in bytes.                      */
{
	INT64 stat_size;

	if ( 0 != WL_stat_size_int8(path, &stat_size) )
	{
		WL_werrlog_error(WERRCODE(65300), WPRINT64, "FILESIZE", 
			"_stat64() failed path=[%s] errno=[%d] msg=[%s]", 
			path, errno, strerror(errno));
		return( -1 );
	}

	WL_wtrace(WPRINT64,"FILESIZE","path=[%s] size=[%ld]", path, stat_size);
	return( stat_size );
}


static char *WL_GetWin32Error(char *szText)
{
	DWORD dwError, nBytes;
	static char *savebuff = NULL;
	LPTSTR lpMsgBuf = NULL;
 
	dwError = GetLastError();
	nBytes = FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		NULL,
		dwError,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		(LPTSTR) &lpMsgBuf,
		0,
		NULL );
	if (savebuff) free(savebuff);
	savebuff = malloc(nBytes + strlen(szText) + 10);
	if (nBytes <= 0) lpMsgBuf = NULL;
	sprintf(savebuff,"%s: %s",szText, lpMsgBuf ? lpMsgBuf:"(no message)");

	LocalFree( lpMsgBuf );
	return savebuff;
}


/*
**	ROUTINE:	WL_werrlog_error()
**
**	FUNCTION:	Write a error message to the wisp error log (wisperr.log)
**
**	DESCRIPTION:	(errcode) %%ROUTINE-E-CODE format ..
**
**	ARGUMENTS:
**	errcode		Error number (even)
**	routine		The routine name
**	code		A code for header
**	format		A printf() style format message
**	...		arguments for the format message
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
#define ERRBUFF_LEN 2048

static void WL_werrlog_error(int errcode, const char* routine, const char* code, const char* format, ... /* args */)
{
	va_list ap;
	char	buff[ERRBUFF_LEN];

	va_start(ap, format);
	vsprintf(buff, format, ap);
	va_end(ap);

	fprintf(stdout, "%%%s-E-%s %s\n", routine, code, buff);
}

/*
**	ROUTINE:	WL_wtrace()
**
**	FUNCTION:	Write a trace message to the wisp error log (wisperr.log)
**
**	DESCRIPTION:	%%ROUTINE-T-CODE format ..
**
**	ARGUMENTS:	
**	routine		The routine name
**	code		A code for header
**	format		A printf() style format message
**	...		arguments for the format message
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/

static void WL_wtrace(const char* routine, const char* code, const char* format, ... /* args */)
{
	va_list ap;
	char	buff[ERRBUFF_LEN], mess[ERRBUFF_LEN];

	if (!isTracing)
	{
		return;
	}

	va_start(ap, format);
	vsprintf(buff, format, ap);
	va_end(ap);

	sprintf(mess, "(WTRACE) %%%s-T-%s %s", routine, code, buff);

	/*
	**	If errno is part of the message string then
	**	tack on the errno message string
	*/
	if (NULL != strstr(format,"errno"))
	{
		strcat(mess, " (");
		strcat(mess, strerror(errno));
		strcat(mess, ")");
	}
	strcat(mess, "\n");

	fprintf(stdout,"%s",mess);
}
