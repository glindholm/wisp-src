static char copyright[]="Copyright (c) 1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	Project:	WISPLIB 
**
**	RCS:		$Source:$
**
** 	Purpose:     	Print a file under WIN32 platforms
**
**	Routines:	
*/

#ifdef  WIN32

/*
**	Includes
*/
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "idsistd.h"
#include "que_jobs.h"
#include "wperson.h"
#include "werrlog.h"
#include "wglobals.h"
#include "win32err.h"
#include "wmalloc.h"

#if defined(WIN32DEBUG)
#include <stdarg.h>
#endif

#include <conio.h>

/*
**	Structures and Defines
*/
/*
**	Globals and Externals
*/

/* calling video func in vrawntcn.. should fix this */
extern HWND vraw_get_console_hWnd(void);
extern int wbackground(void);

extern char *wforms(int formnum);
extern char *getprmap(int num);
extern char *wlpclass(char lpclass);
extern int fexists(char *file);
extern int fcanread(char *file);
extern int filesize(char *file);
/*
**	Static data
*/

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

static void ParseFormstr(int formnum, char *lpszFormstr,int *lpnPageWidth,int *lpnPageHeight,
			 int *lpnReqCPI,int *lpnReqLPI,BOOL *lpbLandscape,int *lpnDuplex, 
			 BOOL *lpbRawPrintData, int *lpnTopMargin, int *lpnLeftMargin, 
			 int *lpnPoints, char *lpszFace, BOOL *lpbWrapText);

static BOOL NextPair(int formnum, char *lpszFormStr, int *lpnPosition, char **lpszKeyword, char **lpszValue);
static DEVMODE *GetDEVMODE(char *printername, HANDLE *phPrinter);
static BOOL PrintRawFile(char* szPtrName, char* file);

/*
**	Routine:	win32_printfile()
**
**	Function:	Print a file from WIN32
**
**	Description:	build printer DC, create document, call appropriate print functions
**
**	Arguments:
**	file		The file to print
**	copies		Number of copies
**	formnum		The form number
**	lpclass		The print class
**	printer		The printer number
**	disposition	The disposition "DX", "RS", "DS"
**	mode		The print mode 'H','K','S','P'
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
**	History:	
**	08/29/96	written by JEC
**
*/
#define ROUTINE 83500
int win32_printfile(char *file, int copies, int formnum, char lpclass, int4 printer, char *disposition, char mode)
{

	HDC hDC = NULL;
	HDC hDefaultDC = NULL;
	HANDLE hPrinter = INVALID_HANDLE_VALUE;
	DEVMODE *lpInitData = NULL;
	HFONT hTheFont = NULL;
	int nRC;
	int nSize;
	BOOL bDeleteAfter=0,bRespool=0;
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

	char *szFormstr=NULL,          	/* actual text from $WISPCONFIG files */
	     *szPrinterNum=NULL,
	     *szPrinterCls=NULL;

	char *page[MAXPAGEHEIGHT];
	int nIdx;
	DOCINFO di;	
	FILE *infile;
	BOOL bSavedData=0;
	char szSavedLine[MAXPAGEWIDTH];

	char lpszFace[32] = "Courier New";

	nRC  = 0;

	if ( !fexists(file) )								/* Does file exist?			*/
	{
		return( 20 );
	}
	if ( !fcanread(file) )								/* Can we read the file?		*/
	{
		return( 28 );
	}

	switch(mode)
	{
	case 'H': /* Hold */
	case 'K': /* Keep */
		return(0);

	case 'S': /* Spool and Delete */
		bDeleteAfter = 1;
		break;

	case 'P': /* Print (do not delete) */
		break;

	default:
		return(60);
	}

	if (disposition)
	{
		if      (0==memcmp(disposition,"DX",2))
		{
			bDeleteAfter = 1;						/* Delete after printing.	*/
		}
		else if (0==memcmp(disposition,"RS",2)) 
		{
			bDeleteAfter = 0;
			bRespool = 1;
		}
		else if (0==memcmp(disposition,"DS",2))
		{
			bDeleteAfter = 0;
		}
		else return(61);
	}

	nSize = filesize(file);
	if ( nSize == 0 )
	{
		werrlog(ERRORCODE(5),file,0,0,0,0,0,0,0);
		nRC =  24;
		goto delete_after;
	}                     

	if (copies < 1)
	{
		goto delete_after;
	}

	szFormstr = wforms(formnum);
	if (szFormstr && strlen(szFormstr))
	{
		wtrace("WIN32PRT","FORMS","Formnum=[%d] Formstr=[%s]", formnum, szFormstr);
		ParseFormstr(formnum, szFormstr,&nPageWidth,&nPageHeight,&nReqCPI,&nReqLPI,
			     &bLandscape,&nDuplex,&bRawPrintData,&nTopMargin, &nLeftMargin,
			     &nPoints, lpszFace, &bWrapText);
	}
	if (bRawPrintData)
	{
		wtrace("WIN32PRT","FORMDEF", "Raw print mode");
	}
	else
	{
		wtrace("WIN32PRT","FORMDEF", "Face=[%s] CPI=[%d] LPI=[%d] PW=[%d] LPP=[%d] TM=[%d] LM=[%d] Points=[%d]%s%s%s",
		       lpszFace, nReqCPI, nReqLPI, nPageWidth, nPageHeight, nTopMargin, nLeftMargin, nPoints,
		       (bLandscape)?" Landscape":"", (nDuplex)?" Duplex":"", (bWrapText)?" Wrap":"");
	}
	
	szPrinterNum = getprmap(printer);
	szPrinterCls = wlpclass(lpclass);
	
	if (szPrinterNum && strlen(szPrinterNum))
	{
		strcpy(szPrtName,szPrinterNum);
		wtrace("WIN32PRT","PRTNAME", "PRINTER=[%ld] NAME=[%s]", (long)printer, szPrtName);
	}
	else if (szPrinterCls && strlen(szPrinterCls))
	{
		strcpy(szPrtName,szPrinterCls);
		wtrace("WIN32PRT","PRTNAME", "PRTCLASS=[%c] NAME=[%s]", lpclass, szPrtName);
	}
	else
	{
		hDefaultDC = GetDefPrinterDC(szDefaultPrtName);
		if (!hDefaultDC)
		{
			char errtxt1[1024], *errtxt2;
			sprintf(errtxt1,"Error getting DC for default printer (%s)",szDefaultPrtName);
			errtxt2 = GetWin32Error(errtxt1);
			werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
			nRC = 99;
			goto cleanup;
		}
		strcpy(szPrtName,szDefaultPrtName);

		wtrace("WIN32PRT","PRTNAME", "DEFAULT NAME=[%s]", szPrtName);
	}

	/*
	**	Raw printing is done by the PrintRawFile() routine.
	*/
	if (bRawPrintData)
	{
		while (copies>0)
		{
			if (! PrintRawFile(szPrtName, file) )
			{
				nRC = 99;
				goto cleanup;
			}
			
			copies--;
		}			

		goto delete_after;
	}


	lpInitData = GetDEVMODE(szPrtName,&hPrinter);
	if (lpInitData == NULL)
	{
		    nRC = 99;
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
		errtxt2=GetWin32Error(errtxt1);
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);

		nRC = 99;
		goto cleanup;
	}
	
	if ( SetAbortProc( hDC, AbortProc) == SP_ERROR )
	{
		char *errtxt;
		errtxt = GetWin32Error("Error setting AbortProc");
		werrlog(ERRORCODE(16),errtxt,0,0,0,0,0,0,0);

		nRC = 98;
		goto cleanup;
	}

	SetupForm(hDC, nReqCPI, nReqLPI, lpszFace, nPoints, &nFontHeight, &nFontWidth, &nFontSpace, &hTheFont);

	InitDocStruct(&di, file);
	StartDoc(hDC, &di);
	infile = fopen(file,"r");
	if (infile == NULL)
	{
		nRC = 95;
		goto cleanup;
	}
	for (nIdx=0; nIdx<MAXPAGEHEIGHT; ++nIdx)
	{
		page[nIdx]=calloc(1,MAXPAGEWIDTH+1);
	}
	while (copies>0)
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
		copies--;
	}
	fclose(infile);
	for (nIdx=0; nIdx<MAXPAGEHEIGHT; ++nIdx)
	{	
		free(page[nIdx]);
	}
			
	EndDoc(hDC);

delete_after:	
	if ( bDeleteAfter )
	{
		nRC = unlink(file);
		if ( nRC < 0 )
		{
			werrlog(ERRORCODE(3),file,errno,0,0,0,0,0,0);
		}
	}

cleanup:
	if (hDefaultDC) DeleteDC(hDefaultDC);
	if (hDC)	DeleteDC(hDC);
	if (INVALID_HANDLE_VALUE != hPrinter)	ClosePrinter(hPrinter);
	if (hTheFont)	DeleteObject(hTheFont);
	if (lpInitData)	free(lpInitData);

	return nRC;
}

/*
**	ROUTINE:	ParseFormstr
**
**	FUNCTION:	get print options from the string in FORMS files
**
**	DESCRIPTION:	get print options from the string in FORMS files
**
**	ARGUMENTS:	formnum             form number (for error reporting)
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
static void ParseFormstr(int formnum, char *lpszFormstr,int *lpnPageWidth,int *lpnPageHeight,
			 int *lpnReqCPI,int *lpnReqLPI,BOOL *lpbLandscape,int *lpnDuplex, 
			 BOOL *lpbRawPrintData, int *lpnTopMargin, int *lpnLeftMargin, 
			 int *lpnPoints, char *lpszFace, BOOL *lpbWrapText)
{
	int nPosition, nVal;
	char *lpszKeyword, *lpszValue;
	
#define GOTKW(kw) 0==strcmp(lpszKeyword,(kw))

	nPosition=0;
	while (NextPair(formnum, lpszFormstr, &nPosition, &lpszKeyword, &lpszValue))
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
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
				werrlog(ERRORCODE(10),formnum,lpszKeyword,lpszValue,0,0,0,0,0);
			}
			continue;
		}
		werrlog(ERRORCODE(12),formnum,lpszKeyword,0,0,0,0,0,0);
	}

}
/*
**	ROUTINE:	NextPair()
**
**	FUNCTION:	get next keyword=value pair from a string
**
**	ARGUMENTS:	formnum                form number (for error reporting)
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
static BOOL NextPair(int formnum, char *lpszFormStr, int *lpnPosition, char **lppszKeyword, char **lppszValue)
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
	while (!isspace(CURCH) && CURCH!='=')
	{
		if (isalpha(CURCH))
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
		werrlog(ERRORCODE(14),formnum, *lpnPosition+3,0,0,0,0,0,0);
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
	else
	{
		while (!isspace(CURCH) && CURCH!= '\0')
		{
			lpszValueBuffer[nCopyIdx] = CURCH;
			
			++nCopyIdx;
			NEXTCH;
		}
	}
	
	lpszValueBuffer[nCopyIdx] = '\0';
	if (strlen(lpszKeywordBuffer)==0 || strlen(lpszValueBuffer)==0)
	{
		werrlog(ERRORCODE(14),formnum, *lpnPosition+3,0,0,0,0,0,0);
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

	wtrace("WIN32PRT","CREATEFONT", 
	       "nFullHeight=[%d] nReqHeight=[%d] (LOGPIXELSY[%d] / LPI[%d]), nReqWidth=[%d] (LOGPIXELSX[%d] / CPI[%d]), ",
	       nFullHeight, nReqHeight, nLogPelsY, nReqLPI, nReqWidth,  nLogPelsX, nReqCPI);

	hTheFont = CreateFont(-nReqHeight, nReqWidth, 0, 0, FW_DONTCARE, 0,0,0, DEFAULT_CHARSET,
			     OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
			     FF_MODERN|FIXED_PITCH, lpszFace);
	if (hTheFont == NULL)
	{
		sprintf(errtxt1,"Error creating font \"%s\" [h:%d,w:%d]",lpszFace,nReqHeight,nReqWidth);
		errtxt2 = GetWin32Error(errtxt1);
		
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
	}
	selobjret = SelectObject(hDC, hTheFont);
	if (selobjret == (HGDIOBJ)NULL || selobjret == (HGDIOBJ)GDI_ERROR)
	{
		sprintf(errtxt1,"Error calling SelectObject(%d,%d)",hDC,hTheFont);
		errtxt2 = GetWin32Error(errtxt1);
		
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
	}
	
	bSuccess = GetTextMetrics(hDC, &current);
	if (bSuccess==0)
	{
		errtxt2 =  GetWin32Error("GetTextMetrics");
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
	}

	wtrace("WIN32PRT","FONT", "TEXTMETRICS tmHeight=[%d] tmAveCharWidth=[%d] tmInternalLeading=[%d] tmExternalLeading=[%d]",
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

	wtrace("WIN32PRT","SETUPFORM", "FontHeight=[%d] FontWidth=[%d] FontSpace=[%d]",
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
	PrintDlg(&pdlg);

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
	int	nLen;			/* Lenght of the line */
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
			TextOut(hDC, nOS, nOS+((nFontHeight+nFontSpace) * (nLine+nTopMargin)) ,page[nIdx], nLen);
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
	int	nLen;
	
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
		errtxt2 = GetWin32Error("GetVersionEx()");
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
		
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
	    lpPrtEnumData = wmalloc(obytes);
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)1,lpPrtEnumData,
				    (DWORD)obytes,&bytes,&count);
	    /*
	    ** if bytes is > obytes, there is more data, so realloc lpPrtEnumData to the
	    ** indicated size and call again
	    */
	    if (bytes > obytes)
	    {
		free(lpPrtEnumData);
		lpPrtEnumData = wmalloc(bytes);
	    }
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)1,lpPrtEnumData,
				    (DWORD)bytes,&bytes,&count);
	    if ( !bSuccess )
	    {
		char *errtxt;
		errtxt = GetWin32Error("EnumPrinters");
		werrlog(ERRORCODE(16),errtxt,0,0,0,0,0,0,0);
		
		goto gdm_cleanup;
	    }
	}
	else 
	{
	    dwEnumFlags = PRINTER_ENUM_CONNECTIONS|PRINTER_ENUM_LOCAL;
	    obytes=sizeof(PRINTER_INFO_4);
	    lpPrtEnumData = wmalloc(obytes);
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)4,lpPrtEnumData,
				(DWORD)obytes,&bytes,&count);
	    /*
	    ** if bytes is > obytes, there is more data, so realloc lpPrtEnumData to the
	    ** indicated size and call again
	    */
	    if (bytes > obytes)
	    {
		free(lpPrtEnumData);
		lpPrtEnumData = wmalloc(bytes);
	    }
	    bSuccess = EnumPrinters(dwEnumFlags,NULL,(DWORD)4,lpPrtEnumData,
				    (DWORD)bytes,&bytes,&count);
	
	    if ( !bSuccess )
	    {
		char *errtxt;
		errtxt = GetWin32Error("EnumPrinters");
		werrlog(ERRORCODE(16),errtxt,0,0,0,0,0,0,0);
		
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
		errtxt2 = GetWin32Error(errtxt1);
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
		
		goto gdm_cleanup;
	    }

	    if (wbackground())
	    {
		    hWndParent = NULL;
	    }
	    else
	    {
		    hWndParent = vraw_get_console_hWnd();
	    }
	    
	    nStructSize = DocumentProperties(hWndParent,*phPrinter,lpEnumName,NULL,NULL,0);
	    if (nStructSize < 0)
	    {
		char errtxt1[1024],*errtxt2;
		sprintf(errtxt1,"DocumentPropreties(structsize) printer(%s)",lpEnumName);
		errtxt2=GetWin32Error(errtxt1);
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
		
		goto gdm_cleanup;
	    }
	    retbuf = (DEVMODE *) wmalloc(nStructSize);
	    bSuccess = DocumentProperties(hWndParent,*phPrinter,lpEnumName,retbuf,NULL,DM_OUT_BUFFER);
	    if (bSuccess < 0)
	    {
		char errtxt1[1024],*errtxt2;
		sprintf(errtxt1,"DocumentPropreties(structdata) printer(%s)",lpEnumName);
		errtxt2=GetWin32Error(errtxt1);
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
		
		free(retbuf);
		retbuf=NULL;
		goto gdm_cleanup;
	    }
	}	
	else
	{
		char errtxt1[1024];
		sprintf(errtxt1,"The printer name \"%s\" was not found.",printername);
		werrlog(ERRORCODE(16),errtxt1,0,0,0,0,0,0,0);
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
		werrlog(ERRORCODE(16),GetWin32Error(errtxt1),0,0,0,0,0,0,0);
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
		werrlog(ERRORCODE(16),GetWin32Error(errtxt1),0,0,0,0,0,0,0);

		ClosePrinter(hPrinter);	
		return FALSE;
	}

	/*
	**	Start the page
	*/
	if ( ! StartPagePrinter( hPrinter ))
	{
		sprintf(errtxt1,"StartPagePrinter(%s)", szPtrName);
		werrlog(ERRORCODE(16),GetWin32Error(errtxt1),0,0,0,0,0,0,0);

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
		werrlog(ERRORCODE(16),errtxt1,0,0,0,0,0,0,0);
		
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
			werrlog(ERRORCODE(16),errtxt1,0,0,0,0,0,0,0);

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
			if (! WritePrinter( hPrinter, &buff[offset], cnt, &dwBytesWritten))
			{
				sprintf(errtxt1,"WritePrinter(\"%s\",cnt=%d)", szPtrName, cnt);
				werrlog(ERRORCODE(16),GetWin32Error(errtxt1),0,0,0,0,0,0,0);

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


#endif  /* WIN32 */

/*
**	History:
**	$Log: win32prt.c,v $
**	Revision 1.17  2001-11-09 15:41:17-05  gsl
**	Fix trk# 507
**	In GetPage() the "erase last page" logic only erased the nPageHeight
**	number of lines. If previous page has overstrikes then nPageHeight may
**	be exceeded and lines are left uncleared.
**	Change to erase MAXPAGEHEIGHT
**
**	Revision 1.16  1998-08-28 15:55:39-04  gsl
**	Fix the way top margins are handled and overstrikes.
**
**	Revision 1.15  1998-08-25 10:56:24-04  gsl
**	Big enhancements: Fix LPI & CPI so acurate.
**	Add FORMS fields tm,lm,face,points,wrap
**
**	Revision 1.14  1998-04-21 11:39:47-04  gsl
**	Added support for "RAW" printing on NT/95.
**	In the FORMS file you would specify "raw=1" as the only option.
**
**	Revision 1.13  1998-01-22 10:22:37-05  gsl
**	If in background then don't call vraw_get_console_hWnd()
**
**	Revision 1.12  1998-01-06 09:48:14-05  gsl
**	Add trace statements
**
**	Revision 1.11  1997-04-30 14:54:31-04  jockc
**	code added for WIN95 vs. WINNT, because WIN95 didn't support
**	EnumPrinters with PRINTER_INFO_4 structs.. using INFO_1 for 95.
**
**	Revision 1.10  1997-04-29 17:24:30-04  jockc
**	revised logic.  call to open printer/document properties was moved
**	to a separate function, and is preceeded by call to enumprinters
**	to get a list of valid printers.
**
**	Revision 1.9  1997-03-24 20:55:30-05  gsl
**	Fixed bug - it wasn't closing the file so the unlink() was always failing.
**	Reorganized win32_printfile() to make it easier to follow.
**	Centralized the cleanup and exit code.
**	Rearranged logic to first validate all the parameters before opening
**	the device contexts.
**
**	Revision 1.8  1997-03-10 10:01:31-05  jockc
**	renamed some vars for clarity.. fixed support for CR overstrike
**
**	Revision 1.8  1997-03-03 10:31:19-05  jockc
**	added code to support carriage return overstrike
**
**	Revision 1.7  1996-12-11 18:44:51-05  jockc
**	added missing history (forgot to add $Log: win32prt.c,v $
**	added missing history (forgot to add Revision 1.17  2001-11-09 15:41:17-05  gsl
**	added missing history (forgot to add Fix trk# 507
**	added missing history (forgot to add In GetPage() the "erase last page" logic only erased the nPageHeight
**	added missing history (forgot to add number of lines. If previous page has overstrikes then nPageHeight may
**	added missing history (forgot to add be exceeded and lines are left uncleared.
**	added missing history (forgot to add Change to erase MAXPAGEHEIGHT
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.16  1998-08-28 15:55:39-04  gsl
**	added missing history (forgot to add Fix the way top margins are handled and overstrikes.
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.15  1998-08-25 10:56:24-04  gsl
**	added missing history (forgot to add Big enhancements: Fix LPI & CPI so acurate.
**	added missing history (forgot to add Add FORMS fields tm,lm,face,points,wrap
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.14  1998-04-21 11:39:47-04  gsl
**	added missing history (forgot to add Added support for "RAW" printing on NT/95.
**	added missing history (forgot to add In the FORMS file you would specify "raw=1" as the only option.
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.13  1998-01-22 10:22:37-05  gsl
**	added missing history (forgot to add If in background then don't call vraw_get_console_hWnd()
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.12  1998-01-06 09:48:14-05  gsl
**	added missing history (forgot to add Add trace statements
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.11  1997-04-30 14:54:31-04  jockc
**	added missing history (forgot to add code added for WIN95 vs. WINNT, because WIN95 didn't support
**	added missing history (forgot to add EnumPrinters with PRINTER_INFO_4 structs.. using INFO_1 for 95.
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.10  1997-04-29 17:24:30-04  jockc
**	added missing history (forgot to add revised logic.  call to open printer/document properties was moved
**	added missing history (forgot to add to a separate function, and is preceeded by call to enumprinters
**	added missing history (forgot to add to get a list of valid printers.
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.9  1997-03-24 20:55:30-05  gsl
**	added missing history (forgot to add Fixed bug - it wasn't closing the file so the unlink() was always failing.
**	added missing history (forgot to add Reorganized win32_printfile() to make it easier to follow.
**	added missing history (forgot to add Centralized the cleanup and exit code.
**	added missing history (forgot to add Rearranged logic to first validate all the parameters before opening
**	added missing history (forgot to add the device contexts.
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.8  1997-03-10 10:01:31-05  jockc
**	added missing history (forgot to add renamed some vars for clarity.. fixed support for CR overstrike
**	added missing history (forgot to add
**	added missing history (forgot to add Revision 1.8  1997-03-03 10:31:19-05  jockc
**	added missing history (forgot to add added code to support carriage return overstrike
**	added missing history (forgot to add until 1.6)
**
**	Revision 1.6  1996-12-11 15:36:16-08  jockc
**	moved selectobject (font) to inside of startpage-draw-endpage loop.
**	on Win95 the printer font was being reset to default with each
**	page.
**
**	revision 1.5  1996-12-06 15:37:18-08  jockc                     
**	include win32err.h for proto for GetWin32Error                  
**	                                                                
**	revision 1.4  1996-09-13 12:00:43-07  jockc                     
**	replaced calls to perr (in vrawntcn) with calls to werrlog()..  
**	increased printer name size from 64 to 1024                     
**	                                                                
**	revision 1.3  1996-09-06 09:48:13-07  jockc                     
**	added support for PRMAP and LPMAP files.. moved                 
**	getconsolehwnd to vrawntcn.c.  added some additional cleanup    
**	of DCs                                                          
**	                                                                
**	revision 1.2  1996-09-05 13:24:23-07  jockc                     
**	added additional error reporting in the forms parsing code      
**	                                                                
**	revision 1.1  1996-09-05 11:05:47-07  jockc                     
**	Initial revision                                                
**
**
*/
