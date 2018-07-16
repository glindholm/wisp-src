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

#define MAXPAGEWIDTH 2048
#define MAXPAGEHEIGHT 200

static BOOL CALLBACK AbortProc( HDC hDC, int Error );
static HDC GetDefPrinterDC(char szDefaultPrtName[]);
static void InitDocStruct( DOCINFO* di, char* szDocname);
static void DrawStuff( HDC hDC, char *page[], int nPageHeight, int nFontHeight, int nFontExt);
static BOOL GetPage(FILE *infile, char *page[], int nPageHeight, int nPageWidth, int nLeftMargin,
		    int nRightMargin, int nTopMargin, int nBottomMargin, BOOL *bSavedData, char szSavedLine[]);
static void SetupForm(HDC hDC, int nReqCPI, int nReqLPI,
		      int *lpnFontHeight, int *lpnFontWidth, int *lpnFontExt, HFONT *lpnTheFont);

static void ParseFormstr(int formnum, char *lpszFormstr,int *lpnPageWidth,int *lpnPageHeight,
			 int *lpnReqCPI,int *lpnReqLPI,BOOL *lpbLandscape,int *lpnDuplex);

static BOOL NextPair(int formnum, char *lpszFormStr, int *lpnPosition, char **lpszKeyword, char **lpszValue);
DEVMODE *GetDEVMODE(char *printername);

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
	HANDLE hPrinter = NULL;
	DEVMODE *lpInitData = NULL;
	HFONT hTheFont = NULL;
	int nRC;
	int nSize;
	BOOL bDeleteAfter=0,bRespool=0;
	char szDefaultPrtName[1024], szPrtName[1024];
	int nPageHeight=66, nPageWidth=80;
	int nFontHeight, nFontWidth, nFontExt;

	int nLeftMargin=0,		/* form data loaded from FORMS file */
	   nRightMargin=0,
	   nTopMargin=2,
	   nBottomMargin=2,
	   nReqCPI=10,
	   nReqLPI=6,
	   nDuplex=0;
	BOOL bLandscape=0;

	char *szFormstr=NULL,          	/* actual text from $WISPCONFIG files */
	     *szPrinterNum=NULL,
	     *szPrinterCls=NULL;

	char *page[MAXPAGEHEIGHT];
	int nIdx;
	DOCINFO di;	
	FILE *infile;
	BOOL bSavedData=0;
	char szSavedLine[MAXPAGEWIDTH];

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
		ParseFormstr(formnum, szFormstr,&nPageWidth,&nPageHeight,&nReqCPI,&nReqLPI,&bLandscape,&nDuplex);
	}
	wtrace("WIN32PRT","FORMDEF", "CPI=[%d] LPI=[%d] PW=[%d] PL=[%d] %s%s",
	       nReqCPI, nReqLPI, nPageWidth, nPageHeight, (bLandscape)?"Landscape ":"", (nDuplex)?"Duplex":"");
	
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

	lpInitData = GetDEVMODE(szPrtName);;
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

	SetupForm(hDC, nReqCPI, nReqLPI, &nFontHeight, &nFontWidth, &nFontExt, &hTheFont);

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
			       nRightMargin,nTopMargin,nBottomMargin, 
			       &bSavedData, szSavedLine))
		{
			StartPage(hDC);
			SelectObject(hDC, hTheFont);
			SetBkMode(hDC,TRANSPARENT);
			DrawStuff(hDC,page,nPageHeight,nFontHeight,nFontExt);
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
	if (hPrinter)	ClosePrinter(hPrinter);
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
			 int *lpnReqCPI,int *lpnReqLPI,BOOL *lpbLandscape,int *lpnDuplex)
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
	while (!isspace(CURCH) && CURCH!= '\0')
	{
		if (isalpha(CURCH))
		{
			lpszValueBuffer[nCopyIdx] = tolower(CURCH);
		}
		else
		{
			lpszValueBuffer[nCopyIdx] = CURCH;
		}
		++nCopyIdx;
		NEXTCH;
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
**	ARGUMENTS:	int form          form number
**                      HDC hDC           printer DC
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:	
**
*/
static void SetupForm(HDC hDC,  int nReqCPI, int nReqLPI,
		      int *lpnFontHeight, int *lpnFontWidth, int *lpnFontExt, HFONT *lpnTheFont)
{
	TEXTMETRIC current;
	BOOL bSuccess;
	int nLogPelsX, nLogPelsY;
	int nReqHeight, nReqWidth;
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
	** create a new font and select it; compute the size by dividing
	** the device pixels per inch by the desired CPI and LPI
	**
	*/

	nReqHeight = nLogPelsY / nReqLPI;
	nReqWidth  = nLogPelsX / nReqCPI;

	wtrace("WIN32PRT","CREATEFONT", 
	       "nHeight=[%d] (LOGPIXELSY[%d] / LPI[%d]), nWidth=[%d] (LOGPIXELSX[%d] / CPI[%d]), ",
	       nReqHeight, nLogPelsY, nReqLPI, nReqWidth,  nLogPelsX, nReqCPI);

	hTheFont = CreateFont(nReqHeight, nReqWidth, 0, 0, FW_MEDIUM, 0,0,0, DEFAULT_CHARSET,
			     OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
			     DEFAULT_PITCH, "Courier New");
	if (hTheFont == NULL)
	{
		sprintf(errtxt1,"Error creating font \"Courier New\" [h:%d,w:%d]",nReqHeight,nReqWidth);
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
	*lpnFontHeight = current.tmHeight;
	*lpnFontWidth = current.tmAveCharWidth;
	*lpnFontExt = current.tmExternalLeading;
	*lpnTheFont = hTheFont;

	wtrace("WIN32PRT","FONT", "TEXTMETRICS Height=[%d] AveCharWidth=[%d] ExternalLeading=[%d]",
	       *lpnFontHeight, *lpnFontWidth, *lpnFontExt);
	
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
**	ARGUMENTS:	hDC           the printer DC
**                      page          array of char containing current page
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:	
**
*/
static void DrawStuff( HDC hDC, char *page[], int nPageHeight, int nFontHeight, int nFontExt)
{
	int nIdx, nNoAdvanceOffs;
	char *cr, *lpPage;
	
	for (nIdx=0; nIdx < nPageHeight ; ++nIdx)
	{
		cr = strchr(page[nIdx],(int)'\r');
		if (!cr)
		{
			TextOut(hDC, 0, (nFontHeight + nFontExt) * nIdx ,page[nIdx], strlen(page[nIdx]));
		}
		else
		{
			lpPage = page[nIdx];
			nNoAdvanceOffs = 0;
			do
			{
				*cr='\0';
				TextOut(hDC, nNoAdvanceOffs, (nFontHeight + nFontExt) * nIdx + nNoAdvanceOffs,lpPage, strlen(lpPage));
				lpPage = ++cr;
				cr = strchr(lpPage,(int)'\r');
				nNoAdvanceOffs += 2;
			} while (cr);
			if (*lpPage)
			{
				TextOut(hDC, nNoAdvanceOffs, (nFontHeight + nFontExt) * nIdx  + nNoAdvanceOffs ,lpPage, strlen(lpPage));
			}
		}
	}
	
}
/*
**	ROUTINE:	GetPage()
**
**	FUNCTION:	load the page array with the next page from the file
**
**	DESCRIPTION:	load a page, process tabs, ff, and lf
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
static BOOL GetPage(FILE *infile, char *page[], int nPageHeight, int nPageWidth, int nLeftMargin,
		    int nRightMargin, int nTopMargin, int nBottomMargin, BOOL *bSavedData, char szSavedLine[])
{
        char szLine[MAXPAGEWIDTH];
	int nIdx;
	register int nSrcIdx, nDestIdx, nDestActualPos;
	BOOL bFormfeed;
	
	/*
	** erase last page 
	*/
	for (nIdx=0; nIdx<nPageHeight; ++nIdx)
	{
		memset(page[nIdx],' ', nPageWidth);
		*(page[nIdx]+nPageWidth)='\0';
	}
	/*
	** load page data
	** (margins hard coded at 2 lines)
	*/
	

	for (bFormfeed=0, nIdx=nTopMargin; !bFormfeed && nIdx < (nPageHeight - nBottomMargin); ++nIdx)
	{
		memset(szLine,' ',sizeof(szLine)-1);

		if (*bSavedData)
		{
			strcpy(szLine,szSavedLine);
			*bSavedData=0;
		}	
		else
		{
			if (fgets(szLine,MAXPAGEWIDTH-1,infile)==NULL) 
				break;
		}
		
		for (nSrcIdx=0, nDestActualPos = nDestIdx = nLeftMargin;;)
		{
			if (szLine[nSrcIdx]=='\n') 
				break;
			if (szLine[nSrcIdx]=='\0') 
				break;
			if (szLine[nSrcIdx]=='\r')
			{
				nDestActualPos=0;
			}
			if (szLine[nSrcIdx]==0x0c) 
			{
				if (szLine[nSrcIdx+1]) 
				{
					*bSavedData = TRUE;
					strcpy(szSavedLine,&szLine[++nSrcIdx]);
				}
			        bFormfeed=TRUE;
				break;
			}
			if (szLine[nSrcIdx]==0x09)
			{
				*(page[nIdx]+nDestIdx++) = ' ';
				++nDestActualPos;
				
				while (nDestIdx%8 != 0 && nDestIdx < (nPageWidth - nRightMargin))
				{
					*(page[nIdx]+nDestIdx) = ' ';
					++nDestIdx;
					++nDestActualPos;
				}
				++nSrcIdx;
				continue;
			}
			*(page[nIdx]+nDestIdx) = szLine[nSrcIdx];
			++nDestIdx;
			++nSrcIdx;
			++nDestActualPos;
		} 
		
	}
	/*
	** nIdx=2 here if got EOF 
	*/
	if (nIdx==nTopMargin) return FALSE; 
	else return TRUE;
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
DEVMODE *GetDEVMODE(char *printername)
{
	BOOL bSuccess, bFound;
	char *lpPrtEnumData=NULL, *lpEnumName=NULL;
	DWORD obytes, bytes, count;
	PRINTER_INFO_4 *pi4;
	PRINTER_INFO_1 *pi1;
	int idx;
	DEVMODE *retbuf=NULL;
	HANDLE hPrinter;
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
	    bSuccess=OpenPrinter(lpEnumName,&hPrinter,NULL);
	    if (!bSuccess)
	    {
		char errtxt1[1024], *errtxt2;
		sprintf(errtxt1,"OpenPrinter(%s)",lpEnumName);
		errtxt2 = GetWin32Error(errtxt1);
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
		
		goto gdm_cleanup;
	    }
	    hWndParent = vraw_get_console_hWnd();
	    nStructSize = DocumentProperties(hWndParent,hPrinter,lpEnumName,NULL,NULL,0);
	    if (nStructSize < 0)
	    {
		char errtxt1[1024],*errtxt2;
		sprintf(errtxt1,"DocumentPropreties(structsize) printer(%s)",lpEnumName);
		errtxt2=GetWin32Error(errtxt1);
		werrlog(ERRORCODE(16),errtxt2,0,0,0,0,0,0,0);
		
		goto gdm_cleanup;
	    }
	    retbuf = (DEVMODE *) wmalloc(nStructSize);
	    bSuccess = DocumentProperties(hWndParent,hPrinter,lpEnumName,retbuf,NULL,DM_OUT_BUFFER);
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

#endif  /* WIN32 */

/*
**	History:
**	$Log: win32prt.c,v $
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
