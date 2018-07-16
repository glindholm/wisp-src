static char copyright[]="Copyright (c) 1996-1999 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		vrawntcn.c
**
**	Project:	VIDEOLIB
**
**	RCS:		$Source:$
**
** 	Purpose:     	This is the implementation file for the vraw package
**			for Wisp for Windows NT using Console mode I/O
**
**	Overview:	
**
**	There are three console configurations:
**	1) Direct IO with a WIN32 console.
**	2) Stream IO with named pipes (Local Costar).
**	3) Stream IO with a telnet session.
**
**	There are three mode of input:
**	1) Read without waiting.  	vcheck() -> vrawcheck() -> xgetc_nw()
**	2) Read with wait.		vgetm() -> vgetc() -> vrawinput() -> xgetc_w()
**	3) Read with wait and timeout.	vgetm_timed() -> vrawtimeout()/vgetc() -> vrawinput() -> xgetc_w()/xgetc_nw()
*/

#ifdef  WIN32

/*
**	Includes
*/

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include <conio.h>
#include <assert.h>
#include <crtdbg.h>

#include "video.h"
#include "vlocal.h"
#include "vmodules.h"
#include "vraw.h"

/*
**	Structures and Defines
*/
#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#ifndef MAX
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#endif

#define PERR(bSuccess, api) {if (!(bSuccess)) perr(__FILE__, __LINE__, \
    api, GetLastError());}

#define LOGICAL_COLS	80			/* The logical size of the screen */
#define LOGICAL_ROWS	24

enum e_event { E_KEYPRESS, E_MOUSELCLICK, E_MOUSERCLICK };

struct my_event
{
	enum e_event type;
	int keyval;
	int mousex, mousey;
};
#define VID_MAXEVENTS 512	/* Size of local event queue */


/*
**	Globals and Externals
*/
HWND vraw_get_console_hWnd(void);

extern int vcur_atr;

/*
 * Globals whose storage is defined in other compilation units:
 *
 * int vb_pure:         This will be TRUE (non-zero) when the higher
 *                      layers are intending to write output that should
 *                      not be post processed. If this flag is FALSE. then
 *                      any line-feed ('\012') character seen in the output
 *                      stream will have a carriage return ('\015') character
 *                      pre-pended to it.
 *
 * int vb_count:        This is the count of how many characters are currently
 *                      in the output buffer. It is read by higher layers
 *                      of Video for optimization purposes.
 *
 * int debugging:       This is a debugging hook flag which will cause any
 *                      output data in the buffer owned by this compilation
 *                      unit to be flushed to the output device immediately.
 *
 */
extern int vb_pure;
extern int vb_count;
extern int debugging;
extern int video_inited;

/*
**	Static data
*/

static int actual_cols = LOGICAL_COLS;				/* The actual size of the screen */
static int actual_rows = LOGICAL_ROWS;

static HANDLE hConsole		= INVALID_HANDLE_VALUE;		/* Console output handle */
static HANDLE hConsoleIn	= INVALID_HANDLE_VALUE;		/* Console input handle */
static HANDLE hConsoleSave	= INVALID_HANDLE_VALUE;		/* Saved original console handle to be restored on exit */

static DWORD dwOriginalInMode = 0;				/* Save the original input mode */

static int out_row = 0;						/* Current real cursor position */
static int out_col = 0;						/* Current real cursor position */

static HWND hwndConsole = NULL; 				/* Handle to the console window */

static int nOrigRows = 25;					/* Original console size */
static int nOrigCols = 80;

static BOOL bStreamIO=0;					/* Flag if stream IO is used with COSTAR and telnet */
static BOOL bPipeIO = 0;					/* Is IO to a PIPE? */
static BOOL bTelnet = 0;

#define FG FOREGROUND_GREEN
#define FR FOREGROUND_RED
#define FB FOREGROUND_BLUE
#define FI FOREGROUND_INTENSITY
#define BG BACKGROUND_GREEN
#define BR BACKGROUND_RED
#define BB BACKGROUND_BLUE
#define BI BACKGROUND_INTENSITY

#define NUMATTR	16
static WORD attributes[NUMATTR]=		/* Map attributes to colors */
{						/*	Attr	1-Bold	2-Under	4-Blink	8-Rev	*/
	FB|FI		| BG|BR|BB|BI,		/*	0	0	0	0	0	*/
	FR|FI		| BG|BR|BB|BI,		/*	1	1	0	0	0	*/

	FB|FI		| BG|BB|BI,		/*	2	0	1	0	0	*/
	FR|FI		| BG|BB|BI,		/*	3	1	1	0	0	*/

	FB|FI		| BG|BR|BI,		/*	4	0	0	1	0	*/
	FR|FI		| BG|BR|BI,		/*	5	1	0	1	0	*/

	FB|FI		| BG|BI,		/*	6	0	1	1	0	*/
	FR|FI		| BG|BI,		/*	7	1	1	1	0	*/

	FB|FI		| BG|BR|BB,		/*	8	0	0	0	1	*/
	FR|FI		| BG|BR|BB,		/*	9	1	0	0	1	*/

	FR|FB|FG|FI	| BG|BB,		/*	a	0	1	0	1	*/
	FB|FG|FI	| BG|BB,		/*	b	1	1	0	1	*/

	FR|FB|FG|FI	| BR,			/*	c	0	0	1	1	*/
	FB|FG|FI	| BR,			/*	d	1	0	1	1	*/

	FR|FB|FG|FI	| BB,			/*	e	0	1	1	1	*/
	FB|FG|FI	| BB			/*	f	1	1	1	1	*/
};

static int vraw_init_flag = 0;					/* Flag if vrawinit() has been called - reset by vrawexit() */
static char *vraw_contitle_def = "WISP for Windows";		/* Default console title - used if vraw_contitle not set. */
static char vraw_contitle[80] = "";				/* Console title set by vrawtitle() */

static int vread_timed_out_flag = FALSE;			/* Flag if last read timed out */

static int mouse_row=0, mouse_col=0;				/* Last mouse click position. */

static struct my_event my_events[VID_MAXEVENTS];		/* Local event queue for keystrokes and mouse clicks */
static int head_event=0, tail_event=0;				/* Positions in the local events queue (my_events[]) */

static	DWORD vtimeout_tick_value = 0;				/* The number of ticks between start and timeout. */

/*
**	Static Function Prototypes
*/


static void initcolors(void);
static int xdebug();
static int x_lastline();
static int xdo_debug();
static void xd_put_menu();
static int xd_mp();
static int xd_not_available();
static int xd_invalid();
static void xd_clr_menu();
static void perr(PCHAR szFileName, int line, PCHAR szApiName, DWORD dwError);
static void queue_event(enum e_event type, int keyval, int mousex, int mousey);
static struct my_event * get_queued_event(void);
static int local_event_count(void);
static void vrawtimeout_set(void);
static int vrawinit(void);
static int vrawchin(void);
static unsigned char xgetc_nw(void);
static unsigned char xgetc_w(void);
static BOOL check_event(void);
static BOOL vraw_set_default_title(void);
static BOOL vrawSetCursorPosition(int row, int col);
static BOOL vrawGetCursorPosition(int *row, int *col);
static void vrawSetConsoleSize(HANDLE hConsole);
static void vrawFillDefaultAttribute(HANDLE hConsole);
static int localMessageBox(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType);
static int past_stop_tick(void);

/*
**	ROUTINE:	vrawAllocConsole()
**
**	FUNCTION:	Create a new console.
**
**	DESCRIPTION:	First free any existing console then create a new one.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	The console handles.
**
**	RETURN:		Last error code.
**
**	WARNINGS:	None.
**
*/
static DWORD vrawAllocConsole(void)
{
	BOOL bSuccess = FALSE;
	DWORD rc = 0;
	
	_ASSERT(!bStreamIO);

	FreeConsole();
	hConsole	= INVALID_HANDLE_VALUE;
	hConsoleIn	= INVALID_HANDLE_VALUE;
	hConsoleSave	= INVALID_HANDLE_VALUE;
	hwndConsole	= NULL;

	SetLastError(0);
	bSuccess = AllocConsole();
	PERR(bSuccess, "AllocConsole");
	rc = GetLastError();

	return rc;
}

/*
 * Routine:     vrawinit()              (Module-private routine)
 *
 * Purpose:     To perform initializion required by this complation unit and the Video package at large.
 *
 * Invocation:  success_flag = vrawinit();
 *
 * Inputs:              None.
 *
 * Outputs:             None.
 *
 * Side effects:        1. Any pending output on the stdin, stdout and stder  streams/files is flushed.
 *
 * Returns:             SUCCESS -- the vraw layer was successfully initialized.
 */
static int vrawinit(void)
{
	BOOL bSuccess = FALSE;
	DWORD dwOriginalOutMode = 0;
	DWORD dwTestMode = 0;
	DWORD rc = 0;
	RECT rectWin;
	DWORD modeout, modein;
	static BOOL bFirstTimeMoveWindow = TRUE;
	
	if (vraw_init_flag)
	{
		return SUCCESS;
	}
	else
	{
		OSVERSIONINFO osVer;

		osVer.dwOSVersionInfoSize = sizeof(osVer);
		bSuccess = GetVersionEx(&osVer);
		PERR(bSuccess, "GetVersionEx");

		if (osVer.dwPlatformId == VER_PLATFORM_WIN32s) 
		{
			localMessageBox(NULL, 
					"This application cannot run on Windows 3.1.\n"
					"This application will now terminate.",
					"Error: Windows NT or Windows 95 Required to Run",  
					MB_OK );
			exit(0);
			return FAILURE;
		}
	}

	SetLastError(0);

	initcolors();
	
	// Test if using stream IO screen handling (Co*STAR & telnet)
	bStreamIO = !(vrawdirectio());

	//if using stream IO then just clear the screen,
	//the rest is handled by Co*STAR, Telnet, or whatever at task level
	if( bStreamIO )
	{
		
		//Disable buffered output
		setvbuf( stdout, NULL, _IONBF, 0 );

		// Set for character at a time input
		// CoStar sets this up OK but it is needed for Telnet
		setvbuf( stdin, NULL, _IONBF, 0 );

		hConsole     = GetStdHandle(STD_OUTPUT_HANDLE);
		hConsoleIn   = GetStdHandle(STD_INPUT_HANDLE);

		bSuccess = SetConsoleMode(hConsoleIn,0);  // Do not process input.

		/*
		**	Were done
		*/
		video_inited = TRUE;	
		vraw_init_flag = 1;

		return SUCCESS;
	}
	
	
	/*
	**	Check if we already have a console.
	**	If not alloc one and set the default title.
	*/
	hConsoleSave = GetStdHandle(STD_OUTPUT_HANDLE);
	if (INVALID_HANDLE_VALUE == hConsoleSave)
	{
		rc = vrawAllocConsole();
	}

	bSuccess = vraw_set_default_title();
	if (TRUE != bSuccess)
	{
		rc = vrawAllocConsole();

		bSuccess = vraw_set_default_title();
		PERR(bSuccess,"SetConsoleTitle");
	}

	vrawFillDefaultAttribute(GetStdHandle(STD_OUTPUT_HANDLE));

	/*
	**	Initialize the console window handle now.
	*/
	vraw_get_console_hWnd();


	/*
	 *	Set the Icon in the console window
	 */
	{
		HICON hWispIcon = NULL;
		HMODULE hModule = NULL;

		hModule = GetModuleHandle(NULL);
		if (NULL != hModule)
		{
			/*
			 *	Note: LoadIcon() really wants an HINSTANCE but a console app doesn't 
			 *	have one, however the module handle works the same in this instance.
			 */
			hWispIcon = LoadIcon(hModule,"WISPICON");
		}
		if (NULL == hWispIcon)
		{
			hWispIcon = LoadIcon(NULL,IDI_WINLOGO);
		}
		if (NULL != hWispIcon)
		{
			SendMessage(vraw_get_console_hWnd(),
				    WM_SETICON,(WPARAM)ICON_SMALL, (LPARAM)hWispIcon);
		}
	}


	/*
	**	If this process was created with the "hide parent" option
	**	then this ShowWindow() will hide the window as the SHOWDEFAULT
	**	will be ignored. 
	*/
	bSuccess = ShowWindow(vraw_get_console_hWnd(), SW_SHOWDEFAULT);

	/* get the standard handles */
	
	/*
	**	Since we are going to modify the attributes
	**	of the console buffer we need to create are own
	**	buffer and make it the active buffer.
	*/
	hConsole = CreateConsoleScreenBuffer( 
		GENERIC_READ|GENERIC_WRITE,
		FILE_SHARE_READ | FILE_SHARE_WRITE,
		NULL,
		CONSOLE_TEXTMODE_BUFFER,
		NULL);
	PERR((hConsole != INVALID_HANDLE_VALUE), "CreateConsoleScreenBuffer");

	vrawFillDefaultAttribute(hConsole);

	bSuccess = SetConsoleActiveScreenBuffer(hConsole);
	PERR(bSuccess,"SetConsoleActiveScreenBuffer");

	hConsoleIn = GetStdHandle(STD_INPUT_HANDLE);
	PERR(!(hConsoleIn == INVALID_HANDLE_VALUE), "GetStdHandle(STD_INPUT_HANDLE)");

	/* 
	** Set the input mode:
	** First save the original input mode so it can be restored later.
	** Next, set the input mode to mouse input.
	*/
	bSuccess = GetConsoleMode(hConsoleIn, &dwOriginalInMode);
	PERR(bSuccess, "GetConsoleMode");

	/*
	  modein = (dwOriginalInMode & ~(ENABLE_LINE_INPUT|ENABLE_ECHO_INPUT)) | ENABLE_MOUSE_INPUT;
	*/
	SetLastError(0);
	modein = ENABLE_PROCESSED_INPUT | ENABLE_MOUSE_INPUT;
	bSuccess = SetConsoleMode(hConsoleIn,modein);
	PERR(bSuccess, "SetConsoleMode");
	/*
	** Test to ensure the mode got set correctly
	*/
	bSuccess = GetConsoleMode(hConsoleIn, &dwTestMode);
	PERR(bSuccess, "GetConsoleMode");
	if (dwTestMode != modein)
	{
		vrawerror("Unable to set Console input mode.");
	}

	/*
	** Set the output mode:
	*/
	SetLastError(0);
	modeout = ENABLE_PROCESSED_OUTPUT;
	bSuccess = SetConsoleMode(hConsole, modeout);
	PERR(bSuccess, "SetConsoleMode");
	/*
	** Test to ensure the mode got set correctly
	*/
	bSuccess = GetConsoleMode(hConsole, &dwTestMode);
	PERR(bSuccess, "GetConsoleMode");
	if (dwTestMode != modeout)
	{
		vrawerror("Unable to set Console output mode.");
	}

	/*
	**	Set the window and buffer sizes
	*/
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
		PERR(bSuccess, "GetConsoleScreenBufferInfo");
		if (bSuccess)
		{
			nOrigRows = csbi.dwSize.Y;
			nOrigCols = csbi.dwSize.X;
		}
	}
	vrawSetConsoleSize(hConsole);


	/*
	**	Move the console window to the same location as it's parents.
	**	Only do this the first time in to avoid interferring with
	**	other maintainence of the window location.
	**	(E.g. After a LINK the window will be correctly already positioned.)
	*/
	if (bFirstTimeMoveWindow)
	{
		bSuccess = GetWindowRect(vraw_get_console_hWnd(), &rectWin);
		bFirstTimeMoveWindow = FALSE;
	}
	else
	{
		bSuccess = FALSE;
	}
	if (bSuccess)
	{
		char* ptr;
		LONG	nWidth, nHeight, nLeft, nTop;

		nTop	= rectWin.top;
		nLeft	= rectWin.left;
		nWidth  = rectWin.right  - rectWin.left;
		nHeight = rectWin.bottom - rectWin.top;

		/*
		**	The parents console window position is set as part
		**	of the spawn. Get the position and if different 
		**	then move the window.
		**
		**	If the position was not set then this code does nothing.
		*/
		ptr = getenv("CONWINPOS");
		if (ptr && *ptr)
		{
			sscanf(ptr, "%ld:%ld", &nLeft, &nTop);
		}
		if (nLeft != rectWin.left || nTop != rectWin.top)
		{
			bSuccess = MoveWindow(vraw_get_console_hWnd(),
					      nLeft, nTop, nWidth, nHeight, TRUE);
		}
	}

	/*
	**	Activate and display the console window
	*/
	bSuccess = ShowWindow(vraw_get_console_hWnd(), SW_SHOWNORMAL);
	bSuccess = SetForegroundWindow(vraw_get_console_hWnd());


	/*
	**	Set - video was initialized.
	*/
	video_inited = TRUE;	
	vraw_init_flag = 1;

	/*
	**	Set Cursor size and make invisible
	*/
	rc = vrawcursor(0); /* hide the cursor */
	if (!rc) vrawerror("Unable to set Cursor info.");

	/*
	**	Erase the screen.
	**	This causes the background to be painted.
	*/
	vrawerase(0, 0, LOGICAL_ROWS-1, LOGICAL_COLS-1);

	return SUCCESS;
}

/*
**	ROUTINE:	vrawSetWindowSize()
**
**	FUNCTION:	Set the Window size for the console
**
**	DESCRIPTION:	Set the Window size then test to see if it really was successful.
**			Attempt this (2) times.
**
**	ARGUMENTS:	
**	hConsole	The Console handle
**	nrows		The new number of rows
**	ncols		The new number of cols.
**
**	GLOBALS:	None
**
**	RETURN:		
**	TRUE		Success - Window size was set.
**	FALSE		Failed.
**
**	WARNINGS:	You can not set the Window size larger then the buffer size.
**
*/
static BOOL vrawSetWindowSize(HANDLE hConsole, int nrows, int ncols)
{
	BOOL bSuccess = FALSE;
	DWORD rc = 0;
	const BOOL absolute = TRUE;
	SMALL_RECT rectWin;
	COORD coordLargest;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	int i;

	_ASSERT(!bStreamIO);

	/* get the largest size we can size the console window to */
	coordLargest = GetLargestConsoleWindowSize(hConsole);
	PERR(coordLargest.X | coordLargest.Y, "GetLargestConsoleWindowSize");

	/* set the window size */
	rectWin.Left = 0;
	rectWin.Top=0;
	rectWin.Bottom = nrows - 1; 
	rectWin.Right  = ncols - 1;

	for(i=0;i<2;i++)
	{
		SetLastError(0);
		bSuccess = SetConsoleWindowInfo(hConsole,absolute,&rectWin);
		Sleep(60);
		rc = GetLastError();

		bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
		PERR(bSuccess, "GetConsoleScreenBufferInfo");

		if (csbi.srWindow.Left   == rectWin.Left &&
		    csbi.srWindow.Top    == rectWin.Top	&&
		    csbi.srWindow.Bottom == rectWin.Bottom &&
		    csbi.srWindow.Right  <= rectWin.Right )
		{
			/*
			**	All we really care about is  "Bottom".
			**	The "Top" and "Left" will always be zero
			**	and the "Right can be smaller if scroll bars.
			*/
			return TRUE;
		}
	}

	return FALSE;
}

/*
**	ROUTINE:	vrawSetBufferSize()
**
**	FUNCTION:	Set the buffer size for the console.
**
**	DESCRIPTION:	Set the buffer size then test to see if it really was successful.
**			Attempt this (2) times.
**
**	ARGUMENTS:	
**	hConsole	The Console handle
**	nrows		The new number of rows
**	ncols		The new number of cols.
**
**	GLOBALS:	None
**
**	RETURN:		
**	TRUE		Success - Buffer size was set.
**	FALSE		Failed.
**
**	WARNINGS:	You can not set the buffer size smaller then the Window size.
**
*/
static BOOL vrawSetBufferSize(HANDLE hConsole, int nrows, int ncols)
{
	BOOL bSuccess = FALSE;
	DWORD rc = 0;
	COORD coordBuf;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	int i;
	int	cxmin, cymin;

	_ASSERT(!bStreamIO);
	
	cxmin = GetSystemMetrics(SM_CXMIN);
	cymin = GetSystemMetrics(SM_CYMIN);

	/* set the buffer size */
	coordBuf.X = ncols;
	coordBuf.Y = nrows;

	for(i=0;i<2;i++)
	{
		SetLastError(0);
		bSuccess = SetConsoleScreenBufferSize(hConsole, coordBuf);
		Sleep(60);
		rc = GetLastError();

		bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
		PERR(bSuccess, "GetConsoleScreenBufferInfo");

		if (csbi.dwSize.X == coordBuf.X &&
		    csbi.dwSize.Y == coordBuf.Y    )
		{
			return TRUE;
		}
	}

	return FALSE;

}

/*
**	ROUTINE:	vrawConsoleResize()
**
**	FUNCTION:	Resize/size the console window and buffer
**
**	DESCRIPTION:	Set the console window and buffer size in the correct
**			order based on the current size then test to see if
**			it was successful.
**
**	ARGUMENTS:	
**	hConsole	The Console handle
**	nrows		The new number of rows
**	ncols		The new number of cols.
**
**	GLOBALS:	None
**
**	RETURN:		
**	TRUE		Success - Buffer size was set.
**	FALSE		Failed.
**
**	WARNINGS:	You can not set the buffer size smaller then the Window size.
**
*/
static BOOL vrawConsoleResize(HANDLE hConsole, int rows, int cols)
{
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	BOOL bSuccess = FALSE;

	_ASSERT(!bStreamIO);

	/* Get info on console buff */
	bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
	PERR(bSuccess, "GetConsoleScreenBufferInfo");

	/*
	**	If current buff size is greater then desired size
	**	then set the window info first.
	*/
	if ((csbi.dwSize.X * csbi.dwSize.Y) > (cols * rows))
	{
		bSuccess = vrawSetWindowSize(hConsole, rows, cols);
		if (bSuccess)
		{
			bSuccess = vrawSetBufferSize(hConsole, rows, cols);
		}
	}
	else
	{
		bSuccess = vrawSetBufferSize(hConsole, rows, cols);
		if (bSuccess)
		{
			bSuccess = vrawSetWindowSize(hConsole, rows, cols);
		}
	}

	/*
	**	Check if we got the size we wanted.
	*/
	bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
	PERR(bSuccess, "GetConsoleScreenBufferInfo");

	if (csbi.dwSize.X != cols ||
	    csbi.dwSize.Y != rows    )
	{
		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

/*
**	ROUTINE:	vrawSetConsoleSize()
**
**	FUNCTION:	Set the Console size
**
**	DESCRIPTION:	Resize the console.
**			See KLUDGE for info on windows 95.
**
**	ARGUMENTS:	
**	hConsole	The console handle
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	This contains a kludge for Win95
**
*/
static void vrawSetConsoleSize(HANDLE hConsole)
{
	BOOL bSuccess = FALSE;

	_ASSERT(!bStreamIO);

	bSuccess = vrawConsoleResize(hConsole, LOGICAL_ROWS, LOGICAL_COLS);
	if (bSuccess) return;

	/*
	**	KLUDGE - Console mode IO has problems on Win95.
	**	To allows it to work on Win95 resize the window large 
	**	and small first then size it normally.
	**	
	*/

	bSuccess = vrawConsoleResize(hConsole, 25, 80);
	bSuccess = vrawConsoleResize(hConsole, LOGICAL_ROWS, LOGICAL_COLS);
	if (bSuccess) return;

	bSuccess = vrawConsoleResize(hConsole, 26, 80);
	bSuccess = vrawConsoleResize(hConsole, LOGICAL_ROWS, LOGICAL_COLS);
	if (bSuccess) return;

	bSuccess = vrawConsoleResize(hConsole, 23, 80);
	bSuccess = vrawConsoleResize(hConsole, LOGICAL_ROWS, LOGICAL_COLS);
	if (bSuccess) return;

	bSuccess = vrawConsoleResize(hConsole, 27, 80);
	bSuccess = vrawConsoleResize(hConsole, LOGICAL_ROWS, LOGICAL_COLS);
	if (bSuccess) return;

	bSuccess = vrawConsoleResize(hConsole, 22, 80);
	bSuccess = vrawConsoleResize(hConsole, LOGICAL_ROWS, LOGICAL_COLS);
	if (bSuccess) return;

	bSuccess = vrawcursor(0); /* Test if console responding properly */
	if (!bSuccess)
	{
		vrawerror("Unable to properly size the Console window.\n"
			  "Consider creating a PIF file for this program\n"
			  "or for CONAGENT.EXE. Specify a font size\n"
			  "other then \"auto\".");
	}
	else
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;

		bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
		PERR(bSuccess, "GetConsoleScreenBufferInfo");
		actual_rows = MAX(csbi.dwSize.Y, LOGICAL_ROWS);
		actual_cols = MAX(csbi.dwSize.X, LOGICAL_COLS);
	}
}

/*
**	ROUTINE:	vrawWriteConsole()
**
**	FUNCTION:	Do the actual writing to the console
**
**	DESCRIPTION:	This routine calls WriteConsole.
**
**	ARGUMENTS:	
**	hConsole	The console handle
**	buff		The character buffer to write (not terminated)
**	len		The number of chars to write
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static BOOL vrawWriteConsole(HANDLE hConsole, char *buff, int len)
{
	DWORD cCharsWritten = 0;
	BOOL bSuccess = TRUE;

	_ASSERT(vraw_init_flag);

	if( bStreamIO )
	{
		if (buff && len > 0)
		{
			//printf( buff );	//just print to stdout or use WriteFile()
		
			bSuccess =  WriteFile(
						hConsole,	// handle to file to write to 
						buff,		// pointer to data to write to file 
						len,		// number of bytes to write 
						&cCharsWritten,	// pointer to number of bytes written 
						NULL ); 	// pointer to structure needed for overlapped I/O
   		}
		
		return( TRUE );
	}

	if (buff && len > 0)
	{
		bSuccess = WriteConsole(hConsole, buff, len, &cCharsWritten, NULL);
		PERR(bSuccess, "WriteConsole");
	}

	return bSuccess;
}


/*
**	ROUTINE:	vrawinput()
**
**	FUNCTION:	Read the next input character, wait if none available. (Blocking read)
**
**	DESCRIPTION:	Init if needed then call xgetc_w to WAIT.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	?
**
**	RETURN:		The next character
**
**	WARNINGS:	none
**
*/
char vrawinput()
{
	if (!vraw_init_flag) vrawinit();

	return xgetc_w(); 
}

/*
 * Routine:     vrawcheck()             (Globally visible)
 *
 * Purpose:     This routine is called to check for the availability of a single
 *              character from the input stream. If none is available, this
 *              routine will return 'NULL' immediately.
 *
 * Invocation:  ch_read = vrawcheck();
 *
 * Inputs:
 *
 *      Implicit:       vraw_init_flag
 *
 *      Explicit:       None.
 *
 * Outputs:
 *
 *      Implicit:       None.
 *
 *      Explicit:       None.
 *
 *
 * Side effects:        If the routine "vrawinit()" has not been called yet,
 *                      this routine will call it before proceeding to get
 *                      the character. Hence the "vraw_init_flag" may be
 *                      set to non-zero as a result of calling vrawinit().
 *                      See comments for vrawinit() for other side effects.
 *
 * Returns:             The first of any characters waiting in the input buffer
 *                      to be read or NULL if none. (Note that this routine
 *                      may also return NULL if an error occurred while trying
 *                      to read the character.)
 *
 */
char vrawcheck()
{
	if (!vraw_init_flag) vrawinit();
	return xgetc_nw();			/* Check for a char, don't wait */
}

int vrawprint(char* buf)			/* Do raw output.       */
{
	if (!vraw_init_flag) vrawinit();        /* Init if not done.    */

	if (buf)                   		/* Anything to do?      */
	{
		vrawWriteConsole(hConsole, buf, strlen(buf));
	}
	return (SUCCESS);                       /* Success always.      */
}

int vrawflush(void)
{
	return(SUCCESS);
}

int vrawputc(char ch)				/* Output single char.  */
{

	if (!vraw_init_flag) vrawinit();        /* Init if not done.    */

	vrawWriteConsole(hConsole, &ch, 1);

	return(SUCCESS);
}

/*
**	Reposition the parents console window so that it is the same as 
**	the current window.  This is called from vrawexit() to fix the 
**	parents position before an exit so that the windows will not
**	jump when the child exits.
*/
static int repositionParentConsole(void)
{
	HWND hwndParent;
	const char *env;
	BOOL bSuccess = FALSE;
	RECT rectWin;

	if (NULL == hwndConsole)
	{
		return 0;
	}

	hwndParent = NULL;
	env = getenv("WISPHCONSOLE");
	if (NULL==env)
	{
		return 0;
	}
	sscanf(env,"%d",&hwndParent);
	if (NULL == hwndParent)
	{
		return 0;
	}
	if (hwndConsole == hwndParent)
	{
		return 0;
	}

	/*
	**	Get the position of this window an move the parent window to the same location
	*/
	bSuccess = GetWindowRect(hwndConsole, &rectWin);
	if (bSuccess)
	{
		LONG	nWidth, nHeight, nLeft, nTop;

		nTop	= rectWin.top;
		nLeft	= rectWin.left;
		nWidth  = rectWin.right  - rectWin.left;
		nHeight = rectWin.bottom - rectWin.top;

		bSuccess = MoveWindow(hwndParent, nLeft, nTop, nWidth, nHeight, FALSE);
	}

	return 0;
}

int vrawexit()
{
	if (vraw_init_flag)		/* Init if done.    */
	{
		BOOL bSuccess = FALSE;

		if( bStreamIO )
		{
			vraw_init_flag = 0;   /* No longer initialized.               */
			return SUCCESS;
		}

		repositionParentConsole();

		vrawcursor(1); /* restore the cursor */
//		bSuccess = vrawConsoleResize(hConsole, nOrigRows, nOrigCols);
		
		if ( INVALID_HANDLE_VALUE != hConsoleSave )
		{
			/*
			**	Restore the saved buffer
			*/
			bSuccess = SetConsoleActiveScreenBuffer(hConsoleSave);
			PERR(bSuccess,"SetConsoleActiveScreenBuffer");
//			bSuccess = vrawConsoleResize(hConsole, nOrigRows, nOrigCols);
			hConsoleSave = INVALID_HANDLE_VALUE;
		}
		if ( INVALID_HANDLE_VALUE != hConsole )
		{
			/*
			**	Close the buffer we created
			*/
			bSuccess = CloseHandle(hConsole);
			PERR(bSuccess,"CloseHandle");
			hConsole = INVALID_HANDLE_VALUE;
		}
		if ( INVALID_HANDLE_VALUE != hConsoleIn )
		{
			/*
			**	Restore the input mode
			*/
			bSuccess = SetConsoleMode(hConsoleIn, dwOriginalInMode);
			PERR(bSuccess,"SetConsoleMode");
		}

		vraw_init_flag = 0;   /* No longer initialized.               */
	}

	return SUCCESS;
}

/*
 *
 * Procedure to shut down pending inputs in anticipation of the creation
 * of a sub process.
 */
void vshut()
{
}

int vrawntcn_get_mouse_position( int *row, int *col )
{
	*row = mouse_row;
	*col = mouse_col;
	return 0;
}

/*
**	ROUTINE:	xgetc_nw()
**
**	FUNCTION:	Get the next character (NOWAIT)
**
**	DESCRIPTION:	Call check_event() to move any events to the local event queue.
**			If there are now any local events call xgetc_w() to get the character.
**			If there is no character available then return 0 without waiting.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The next character or 0 if no character available.
**
**	WARNINGS:	none
**
*/
static unsigned char xgetc_nw(void)
{
	check_event();
	
	if ( local_event_count() )
	{
		return ( xgetc_w() ) ;
	}
	else
	{
		return ( 0 ) ;
	}
}

/*
**	ROUTINE:	readStreamConsole()
**
**	FUNCTION:	Read one character from the console in stream IO mode (for Costar and telnet).
**
**	DESCRIPTION:	Use ReadFile() to do a blocking read of one character.
**
**	ARGUMENTS:	None
**
**	RETURN:		The character read or '\0'.
**
**	WARNINGS:	None
**
*/
static char readStreamConsole(void)
{
	BOOL bSuccess;
	char the_char;
	DWORD dwInputEvents=0;

	_ASSERT(bStreamIO);
	
	if (bTelnet)
	{
		DWORD dw;
		INPUT_RECORD ir;

		for (;;)
		{
			bSuccess = ReadConsoleInput(hConsoleIn, &ir, 1, &dw);
			PERR(bSuccess,"ReadConsoleInput");

			if (ir.EventType != KEY_EVENT) {
				continue;	// Only interested in KEY events
			}
			if (!ir.Event.KeyEvent.bKeyDown) {
				continue;	// Only interested in key down events
			}
			if (ir.Event.KeyEvent.uChar.AsciiChar == '\0') {
				continue;	// Discard char 0 that preceeds control chars
			}
			return ir.Event.KeyEvent.uChar.AsciiChar;
		}
	}
	else
	{
		bSuccess = ReadFile( hConsoleIn, &the_char, 1, &dwInputEvents, NULL );
		PERR(bSuccess,"ReadFile");

		if (dwInputEvents > 0)
		{
			return the_char;
		}
		else
		{
			return '\0';
		}
	}
}

/*
**	ROUTINE:	xgetc_w()
**
**	FUNCTION:	Get the next character (WAIT)
**
**	DESCRIPTION:	Get a character if one is available, if not available then wait of it.
**			Handle timed reads.
**
**			Loop until a character is read or a timeout occurs.
**
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		The next character or 0 if no character available.
**
**	WARNINGS:	none
**
*/
static unsigned char xgetc_w(void)
{
	int ch;
	struct my_event *ev;
	DWORD windows_event;
	DWORD win_timeout;
	
	_ASSERT(vraw_init_flag);

	if (bStreamIO)
	{
		/*
		**	Check if there is a char already in the queue.
		*/
		if (local_event_count())
		{
			ev = get_queued_event();
			return ev->keyval;
		}
		
		/*
		**	No timeout - just read it.
		*/
		if (!vtimeout_tick_value)
		{
			return (unsigned char) readStreamConsole();
		}

		/*
		**	Timed read - this is the tricky one.
		*/
		for(;;)
		{
			check_event();
			if (local_event_count())
			{
				ev = get_queued_event();
				return ev->keyval;
			}

			if (past_stop_tick())
			{
				vrawtimeout_set();
				return '\0';
			}

			Sleep(100);
		}
	}
	
	if (vtimeout_tick_value)
	{
		/*
		**	Setup for a 1/10 of a second timeout on each wait.
		*/
		win_timeout = 100;
	}
	else
	{
		win_timeout = INFINITE;
	}

	check_event();

	while(NULL == (ev = get_queued_event()))
	{
		if (0 == local_event_count())
		{
			if (vtimeout_tick_value)
			{
				/*
				**	Each time thru loop check if we have timed-out.
				*/
				if (past_stop_tick())
				{
					vrawtimeout_set();
					return '\0';
				}
			}

			SetLastError(0);
			windows_event = WaitForMultipleObjectsEx(1,&hConsoleIn,
				FALSE, win_timeout, FALSE);

			if (0xFFFFFFFF == windows_event)   	/* Wait FAILED */
			{
				DWORD dwLastError = GetLastError();

				if (dwLastError)		/* Check if there realy was an error */
				{
					perr(__FILE__, __LINE__, "WaitForMultipleObjectsEx", dwLastError);
				}
				else 
				{
					/*
					**	No error but we propably lost our wait time
					**	so we sleep here to simulate, also this should
					**	prevent a live-loop racing condition.
					*/
					Sleep(win_timeout);
				}
			}
		}

		check_event();
	}
	
	if (ev->type == E_MOUSELCLICK)
	{
		mouse_row = ev->mousey;
		mouse_col = ev->mousex;
		
		return (unsigned char)'\376';
	}
	
	ch = ev->keyval;
	
	return(ch);
}

/*
**	ROUTINE:	check_event()
**
**	FUNCTION:	Check for an event and add it to the local event queue.
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	none
**
**	GLOBALS:	?
**
**	RETURN:		
**	TRUE		An event was added to the local event queue
**	FALSE		No events 
**
**	WARNINGS:	?
**
*/
static BOOL check_event(void)
{
	BOOL bSuccess, bGotInput;
	DWORD dwNumEvents, dwInputEvents;
	INPUT_RECORD input_record;
	KEY_EVENT_RECORD *keyp;
	MOUSE_EVENT_RECORD *mousep;
	int idx;
	char szBuf[1];		//for bStreamIO
	
	_ASSERT(vraw_init_flag);

	if( bStreamIO )
	{
		/* Peek at the input stream, dwInputEvents will be zero if empty */
		if (bPipeIO && !bTelnet)
		{
			/* 
			**	This is used by Costar as it attaches a pipe to stdin.
			*/
			bSuccess = PeekNamedPipe(
					hConsoleIn,				// handle to pipe to copy from 
					szBuf,				       	// pointer to data buffer 
					sizeof( szBuf ),			// size, in bytes, of data buffer 
					&dwInputEvents,				// pointer to number of bytes read 
					NULL,					// pointer to total number of bytes available  
					NULL ); 				// pointer to unread bytes in this message 

			PERR(bSuccess,"PeekNamedPipe");
		}
		else
		{
			/*
			 *	This is used by telnet.
			 *	This was based on ATAMAN sample code for ALTRS.
			 */
			INPUT_RECORD ir;
			for(;;)
			{
				/*
				 *	Check if something is available
				 */
				bSuccess = PeekConsoleInput(
					hConsoleIn,				// handle to pipe to copy from 
					&ir,				       	// pointer to input record 
					1,					// number of records 
					&dwInputEvents);			// pointer to number of bytes read 
				PERR(bSuccess,"PeekConsoleInput");
				/*
				 *	If there was a keyboard event but not a key-down
				 *	then read it and discard it because not intrested 
				 *	in key-up events. Go back and peek again.
				 *	Note: Mouse, menu, focus, and window events have not been enabled
				 *		so the only events we should get are KEY_EVENT's.
				 */
				if (dwInputEvents > 0 &&
				    ir.EventType == KEY_EVENT && 
				    !ir.Event.KeyEvent.bKeyDown) 
				{
					bSuccess = ReadConsoleInput(hConsoleIn, &ir, 1, &dwInputEvents);
					PERR(bSuccess,"ReadConsoleInput");
					continue;  // Go back and peek again
				}

				break;  // We are done.
			}
		}
		
		if( dwInputEvents == 0 )
		{
			return FALSE;
		}

		/* Read one character */
		szBuf[0] = readStreamConsole();
		
		if (szBuf[0])
		{
			/* Add an ascii character to the event queue */
			queue_event(E_KEYPRESS, szBuf[0], 0,0);
			return TRUE;
		}
		return FALSE;
		 
	} /* bStreamIO */
	

	bSuccess=GetNumberOfConsoleInputEvents(hConsoleIn, &dwNumEvents);
	PERR(bSuccess,"GetNumberOfConsoleInputEvents");
	if (dwNumEvents==0) 
	{
		return FALSE;
	}

	for ( bGotInput=FALSE ; bGotInput==FALSE && dwNumEvents; --dwNumEvents)
	{
		WORD vscancode;
		CHAR AsciiChar;

		bSuccess = ReadConsoleInput(hConsoleIn, &input_record, (DWORD)1, &dwInputEvents);
		PERR(bSuccess,"ReadConsoleInput");
		
		switch(input_record.EventType)
		{
		case KEY_EVENT:
			keyp = &(input_record.Event.KeyEvent);
			
/*
			writelog("keypress: down=%d, rep=%d, vkey=%d, vscan=%d, uni=%d, ascii=%d, ctrl=%x\n",
				 keyp->bKeyDown, keyp->wRepeatCount, keyp->wVirtualKeyCode, keyp->wVirtualScanCode,
				 keyp->uChar.UnicodeChar,keyp->uChar.AsciiChar, keyp->dwControlKeyState);
*/

			vscancode = keyp->wVirtualScanCode;
			AsciiChar = keyp->uChar.AsciiChar;

			/* 
			 * skip key up , also keydown of ctrl, alt, shift, etc
			 */
			if (keyp->bKeyDown == FALSE	/* Key Up */
			    || vscancode==42  		/* L Shift */
			    || vscancode==29 		/* Control */
			    || vscancode==54 		/* R Shift */
			    || vscancode==56  		/* Alt */
			    || vscancode==58  		/* Caps */
			    || vscancode==69  		/* Num */
			    || vscancode==70  		/* Scroll */
			    )
			{
				break;
			}

			if (1 == vscancode) /* Escape Key */
			{
				/*
				**	Treat the escape key as a scan code instead of
				**	an ascii char 27.
				**
				**	This allows it to be mapped to the help key without
				**	interfering with the Costar escape sequences.
				*/
				AsciiChar = 0;	/* Not an Ascii character */
			}

			/*
			 * handled repeated characters
			 */
			for (idx=0; idx<keyp->wRepeatCount; ++idx)
			{
				if (AsciiChar != 0)
				{
					queue_event(E_KEYPRESS, AsciiChar, 0,0);
				}
				else
				{
					/*
					**	NON-ASCII KEYS
					**
					**	All non-ascii keys start with '\035' ('\X') followed by
					**	optional "state" characters then the scan-code.
					**
					**	The optional state characters occur in the following order:
					**
					**		Shift	= '\052'
					**		Control = '\035'
					**		Alt	= '\070'
					**
					**	Examples:
					**
					**	      	F1		= "\X\073"
					**		Shift+F1 	= "\X\052\073"
					**		Shift+Ctrl+F1	= "\X\052\035\073"
					**		Ctrl+Alt+F1	= "\X\035\070\073"
					*/
					
					queue_event(E_KEYPRESS, '\035', 0,0);

					if (keyp->dwControlKeyState & SHIFT_PRESSED)
					{
						queue_event(E_KEYPRESS, '\052', 0,0);  /* 052 = 42 = SHIFT key */
					}

					if (   (keyp->dwControlKeyState & RIGHT_CTRL_PRESSED) 
					    || (keyp->dwControlKeyState & LEFT_CTRL_PRESSED) )
					{
						queue_event(E_KEYPRESS, '\035', 0,0);  /* 035 = 29 = CONTROL key */
					}

					if (   (keyp->dwControlKeyState & RIGHT_ALT_PRESSED) 
					    || (keyp->dwControlKeyState & LEFT_ALT_PRESSED) )
					{
						queue_event(E_KEYPRESS, '\070', 0,0);  /* 070 = 56 = ALT key */
					}

					queue_event(E_KEYPRESS, vscancode, 0,0);
				}
			}
			bGotInput = TRUE;
			
			break;
			
		case MOUSE_EVENT:
			mousep = &(input_record.Event.MouseEvent);

/*
			writelog("mouseevent: pos=%d,%d, state=%s, ctrl=%x, event=%s\n",
				 mousep->dwMousePosition.X,mousep->dwMousePosition.Y,
				 mousep->dwButtonState == FROM_LEFT_1ST_BUTTON_PRESSED? " fromleft1st":
				 mousep->dwButtonState == RIGHTMOST_BUTTON_PRESSED? " rightmost":
				 mousep->dwButtonState == FROM_LEFT_2ND_BUTTON_PRESSED? " fromleft2nd": "unk",
				 mousep->dwControlKeyState,
				 mousep->dwEventFlags==0? "click or release":
				 mousep->dwEventFlags==MOUSE_MOVED? "move":
				 mousep->dwEventFlags==DOUBLE_CLICK?  "2x click" : "unk");
*/		      
			/*
			 * skip mouse moved events
			 */
			if (mousep->dwEventFlags == MOUSE_MOVED)
			{
				break;
			}
			if (mousep->dwButtonState == FROM_LEFT_1ST_BUTTON_PRESSED)
			{
				queue_event(E_MOUSELCLICK,0,mousep->dwMousePosition.X,mousep->dwMousePosition.Y);
				queue_event(E_KEYPRESS, '@', 0,0);
				bGotInput = TRUE;
			}
			break;
			
		default:
			break;
		}
		
	}
	return bGotInput;
}

static void queue_event(enum e_event type, int keyval, int mousex, int mousey)
{
	int next_event;
	
	_ASSERT(tail_event >= 0 && tail_event < VID_MAXEVENTS);

	next_event = tail_event+1;
	if (next_event == VID_MAXEVENTS) 
	{
		next_event=0;
	}
	
	if (next_event == head_event)
	{
		/* Queue OVERFLOW - don't add anymore events */
		_ASSERT(0);
		return;
	}
	
	my_events[tail_event].type = type;
	my_events[tail_event].keyval = keyval;
	my_events[tail_event].mousex = mousex;
	my_events[tail_event].mousey = mousey;

	tail_event = next_event;
}

static struct my_event * get_queued_event(void)
{
	struct my_event *p;

	_ASSERT(head_event >= 0 && head_event < VID_MAXEVENTS);
	
	/* Check if event queue is empty */
	if (head_event == tail_event) 
	{
		return NULL;
	}
	
	p = &my_events[head_event];

	head_event++;
	if (head_event==VID_MAXEVENTS) head_event=0;
	return p;
}

static int local_event_count()
{
	if (tail_event >= head_event) 
	{
		return tail_event - head_event;
	}
	return (VID_MAXEVENTS - head_event) + tail_event;
}

/*
**	Routine:	initcolors()
**
**	Function:	To initialize colors from environment variable
**
**	Description:	This routine reads in the colors from the variable VCOLORS.
**			It then shifts them to be in the second byte and assigns
**			them to attributes.
**
**	Arguments:	None
**
**	Globals:	
**	attribute	The attribute table.
**
**	Return:		None
**
**	Warnings:	If the VCOLORS var is badly formed no error message will
**			be reported.
**
**	History:	
**	01/26/93	Written by GSL
**
*/
static void initcolors()
{
	char	*ptr, buff[3];
	int	i;
	int	attr;

	if (ptr = getenv("VCOLORS"))
	{
		for(i=0; i<NUMATTR && *ptr && *(ptr+1); i++)
		{
			buff[0] = *ptr++;
			buff[1] = *ptr++;
			buff[2] = (char)0;
			if (1==sscanf(buff,"%2x",&attr))
			{
				attributes[i] = attr;
			}
			else
			{
				break;
			}
		}
	}
}

/*
**	Routine:	vrawsetattributes()
**
**	Function:	To change the color attributes.
**
**	Description:	Load the passed in attribute table overtop of
**			the default attribute table.
**			If a passed in attribute is zero then don't replace it.
**			And it with 0xff00 to ensure nothing sneaks into the
**			character position.
**
**	Arguments:
**	attr		The new attribute table. 
**			Each element is of the form 0xff00 and is a "raw" MSDOS 
**			attribute.
**
**	Globals:
**	attribute	The attribute table.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	01/25/93	Written by GSL
**
*/
void vrawsetattributes(int attrs[NUMATTR])
{
	int	i;

	for(i=0;i<NUMATTR;i++)
	{
		attributes[i] = attrs[i];
	}
}

WORD vrawgetattribute(int attr)
{
	return attributes[attr%NUMATTR];
}

void vrawattribute(int atr)
{
	_ASSERT(!bStreamIO);
	_ASSERT(atr >= 0 && atr < NUMATTR);

	if (!vraw_init_flag) vrawinit();

	SetConsoleTextAttribute(hConsole,attributes[atr%NUMATTR]);
}

void vrawmove(int row, int col)
{
	int new_row, new_col;

	_ASSERT(!bStreamIO);

	if (!vraw_init_flag) vrawinit();

	new_row = row % LOGICAL_ROWS;
	new_col = col % LOGICAL_COLS;

	vrawSetCursorPosition(new_row, new_col);
}

static BOOL vrawSetCursorPosition(int row, int col)
{
	BOOL bSuccess = FALSE;

	_ASSERT(!bStreamIO);
	_ASSERT(vraw_init_flag);

	if ( INVALID_HANDLE_VALUE != hConsole )
	{
		COORD coordScreen;
		int	new_row, new_col;

		coordScreen.X = col;
		coordScreen.Y = row;

		SetConsoleCursorPosition(hConsole, coordScreen);

		bSuccess = vrawGetCursorPosition(&new_row, &new_col);
		if (bSuccess)
		{
			bSuccess = (new_row == row && new_col == col) ? TRUE : FALSE;
		}
	}
	return bSuccess;
}

static BOOL vrawGetCursorPosition(int *row, int *col)
{
	BOOL bSuccess = FALSE;

	_ASSERT(!bStreamIO);
	_ASSERT(vraw_init_flag);

	if ( INVALID_HANDLE_VALUE != hConsole )
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;
	
		bSuccess = GetConsoleScreenBufferInfo(hConsole, &csbi);
		if (bSuccess)
		{
			out_row = csbi.dwCursorPosition.Y;
			out_col = csbi.dwCursorPosition.X;

			if (row) *row = out_row;
			if (col) *col = out_col;

		}
	}
	return bSuccess;
}


void vrawerase(int fr, int fc, int tr, int tc) 			/* from/to row/column   */
{
	int save_row, save_col;
	static char *spaces = NULL;

	_ASSERT(!bStreamIO);
	if (!vraw_init_flag) vrawinit();

	if (spaces==NULL)
	{
		spaces=malloc(actual_cols+1);
		memset(spaces,' ',actual_cols);
		spaces[actual_cols] = '\0';
	}
	
	/*
	**	If erasing to the "logical" end of the screen,
	**	then erase to the "actual" end of the screen.
	*/
	if (LOGICAL_COLS-1 == tc) tc = actual_cols-1;
	if (LOGICAL_ROWS-1 == tr) tr = actual_rows-1;

	vrawGetCursorPosition(&save_row, &save_col);

	vrawattribute( 0 );
	
	for( ; fr <= tr ; fr++ )
	{
		vrawSetCursorPosition(fr,fc);
		vrawWriteConsole(hConsole, spaces, tc-fc+1);
	}

	vrawattribute( vcur_atr ); /* Restore the attribute */

	vrawmove(save_row,save_col);
}

void vrawsetscroll(int top,int bottom)
{
	/*
	**	Only a full screen scroll area is supported.
	**
	**	Scrolling is actually not supported but most video apps
	**	will set the scroll region to be the entire screen 
	**	when they initialize.
	*/
	if (top != 0 || bottom != LOGICAL_ROWS-1)
	{
		vrawerror("Invalid scroll region specified.");
	}
}

int vrawscroll(int dir)
{
	vrawerror("Scrolling is not supported.");
	return 0;
}

/*
**	ROUTINE:	vrawcursor()
**
**	FUNCTION:	Set state of the cursor.
**
**	DESCRIPTION:	Set the cursor on or off based on the state flag
**
**	ARGUMENTS:	
**	State		The state to change the cursor to.
**			0 = set cursor off
**			1 = set cursor on
**
**	GLOBALS:	
**	hConsole	The output console handle
**
**	RETURN:		
**	SUCCESS		
**	FAILURE
**
**	WARNINGS:	None
**
*/
int vrawcursor( int state )
{
	CONSOLE_CURSOR_INFO cinfo, cinfoTest;
	BOOL bSuccess = FALSE;
	int rc = FAILURE;

	_ASSERT(!bStreamIO);

	cinfo.dwSize=100;

	if ( state )
	{
		cinfo.bVisible = TRUE;
	}
	else
	{
		cinfo.bVisible = FALSE;
	}

	if (hConsole != INVALID_HANDLE_VALUE)
	{
		SetLastError(0);
		bSuccess = SetConsoleCursorInfo(hConsole,&cinfo);
		Sleep(60);

		if (!bSuccess)
		{
			PERR(bSuccess, "SetConsoleCursorInfo");
		}
		else if (!(bSuccess = GetConsoleCursorInfo(hConsole,&cinfoTest)))
		{
			PERR(bSuccess, "GetConsoleCursorInfo");
		}
		else
		{
			/*
			** Test to ensure the cursor was correctly set.
			*/
			if (cinfoTest.dwSize   == cinfo.dwSize &&
			    cinfoTest.bVisible == cinfo.bVisible )
			{
				rc = SUCCESS;
			}
		}
	}

	return rc;
}

/*      End of low-level WIN32 section */


/*
 * vtimeout:  set the timeout value (if any) for normal "blocked" reads
 *
 */
static	DWORD vtimeout_start_tick = 0;		/* The timer starting tick value. */
static	DWORD vtimeout_stop_tick = 0;		/* The timer ending tick value. */

/*
**	ROUTINE:	past_stop_tick()
**
**	FUNCTION:	Check if the current time is past the timeout stop tick.
**
**	DESCRIPTION:	This checks the current time to see if it is past the 
**			timeout time (vtimeout_stop_tick).
**			It uses GetTickCount() which gives the time the system has
**			been up in milli-seconds.  GetTickCount() will will wrap 
**			around to 0 periodically.
**
**			This routine handles wrapping around to 0 tick.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	
**	vtimeout_start_tick	The tick the timer started.
**	vtimeout_stop_tick	The stop time (in ticks) maybe wrapped around 0.
**	vtimeout_tick_value	The number of ticks (milli-secs) to wait.
**
**	RETURN:		
**	0		Not past stop tick (or timer not set)
**	1		Timed out (past stop tick)
**
**	WARNINGS:	None
**
*/
static int past_stop_tick(void)
{
	DWORD now_tick, elapsed;
	
	if (vtimeout_start_tick == vtimeout_stop_tick)
	{
		/*
		**	Timer not set so not past stop tick
		*/
		return 0;
	}
	else
	{
		now_tick = GetTickCount();
		
		/*
		**	Check if we have wrapped around 0.
		*/
		if (now_tick < vtimeout_start_tick)
		{
			if (vtimeout_stop_tick > vtimeout_start_tick)
			{
				/*
				**	No wrapping in timeout range so we have past and wrapped.
				*/
				return 1;
			}

			return ( now_tick > vtimeout_stop_tick ) ? 1 : 0;
		}
		else
		{
			/*
			**	Have not wrapped around 0 tick so check
			**	if elapsed time is greater then timeout value.
			*/
			elapsed = now_tick - vtimeout_start_tick;
			
			return (elapsed > vtimeout_tick_value) ? 1 : 0;
		}
	}
}

/*
**	ROUTINE:	vrawtimeout()
**
**	FUNCTION:	Set (or clear) the timeout value.
**
**	DESCRIPTION:	Set the vtimeout variables to tick values.
**			If seconds is 0 then clear the values.
**
**	ARGUMENTS:	
**	seconds		The number of seconds to wait before timing out.
**
**	GLOBALS:	
**	vtimeout_start_tick	The tick the timer started.
**	vtimeout_stop_tick	The stop time (in ticks) maybe wrapped around 0.
**	vtimeout_tick_value	The number of ticks (milli-secs) to wait.
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void vrawtimeout(int seconds)
{
	if (seconds > 0)
	{
		vtimeout_tick_value = seconds * 1000;
		vtimeout_start_tick = GetTickCount();
		vtimeout_stop_tick = vtimeout_start_tick + vtimeout_tick_value;
	}
	else
	{
		vtimeout_tick_value = 0;
		vtimeout_start_tick = 0;
		vtimeout_stop_tick = 0;
	}

	vrawtimeout_clear();
}
/*
 * vtimeout_clear:  clear the "timed out" status
 *
 */
void vrawtimeout_clear()
{
	vread_timed_out_flag = FALSE;
}
/*
 * vtimeout_check: return TRUE if time out occured on last read
 *                 FALSE otherwise
 *
 */
int vrawtimeout_check()
{
	return vread_timed_out_flag;
}

static void vrawtimeout_set()
{
	vread_timed_out_flag = TRUE;
}

/* maximum size of the buffer to be returned from FormatMessage */
#define MAX_MSG_BUF_SIZE 512

static void perr(PCHAR szFileName, int line, PCHAR szApiName, DWORD dwError)
{
	CHAR szTemp[1024];
	DWORD cMsgLen = 0;
	CHAR *msgBuf = NULL;		/* buf for msg text from FormatMessage */
	int iButtonPressed = 0;		/* gets button pressed in the error box */

	
	sprintf(szTemp, "VRAWNTCN: Error %d from %s on line %d:\n",
		dwError, szApiName, line);


	cMsgLen = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |				/* get the text description for */
				FORMAT_MESSAGE_ALLOCATE_BUFFER | 40, NULL, dwError,	/* the error number from the system */
				MAKELANGID(0, SUBLANG_ENGLISH_US), (LPTSTR) &msgBuf, 
				MAX_MSG_BUF_SIZE,
				NULL);
	if (!cMsgLen)
	{
		
		sprintf(szTemp + strlen(szTemp), "Unable to obtain error message text! \n"
			"%s: Error %d from %s on line %d", __FILE__,
			GetLastError(), "FormatMessage", __LINE__);
	}
	else
	{
		strcat(szTemp, msgBuf);
	}

	vre_write_logfile(szTemp);
	
	
	strcat(szTemp, "\n\nContinue execution?");
	MessageBeep(MB_ICONEXCLAMATION);
	iButtonPressed = localMessageBox(NULL, szTemp, "Console API Error",
				    MB_ICONEXCLAMATION | MB_YESNO | MB_SETFOREGROUND);

	if (cMsgLen)										/* free the message buffer */
	{
		LocalFree((HLOCAL) msgBuf);
	}
	
	if (iButtonPressed == IDNO)
	{
		exit(1);
	}
	
	return;
}
/*
**	ROUTINE:	vraw_get_console_hWnd()
**
**	FUNCTION:	get handle to window containing console
**
**	DESCRIPTION:	
**
**	ARGUMENTS:	void
**
**	GLOBALS:	
**
**	RETURN:		
**	HWND   		Handle for current console
**	NULL		If stream IO
**
**	WARNINGS:	taken from MSDEV online tech notes, this is 
**                      a bit kludgy but apparently the only way to do it
**                      and it works just fine
**
*/
HWND vraw_get_console_hWnd(void)
{
#define MY_BUFSIZE 1024 
	BOOL bSuccess;

	if (!vrawdirectio())
	{
		return NULL;
	}
	
	if (!hwndConsole)
	{
		char pszNewWindowTitle[MY_BUFSIZE]; 
		char pszOldWindowTitle[MY_BUFSIZE]; 

		bSuccess = GetConsoleTitle(pszOldWindowTitle, MY_BUFSIZE);
		if (bSuccess)
		{
			wsprintf(pszNewWindowTitle,"%d/%d",GetTickCount(), GetCurrentProcessId());
			bSuccess = SetConsoleTitle(pszNewWindowTitle);
 
			if (bSuccess)
			{
				Sleep(40);
 
				hwndConsole=FindWindow(NULL, pszNewWindowTitle);
 
				SetConsoleTitle(pszOldWindowTitle);
			}
		}
	}
 
	return(hwndConsole);
}

/*
**	ROUTINE:	vrawtitle()
**
**	FUNCTION:	set console title to specified string
**
**	DESCRIPTION:	sets title and also saves pointer to string
**
**	ARGUMENTS:	const char *title   title to set
**
**	GLOBALS:	char *vraw_contitle  saved pointer to string
**
**	RETURN:		void
**
**	WARNINGS:
**
*/
void vrawtitle(const char *title)
{
	BOOL bSuccess;

	if (bStreamIO)
	{
		return;
	}
	
	if (title==NULL)
	{
		return;
	}

	if (title != vraw_contitle)
	{
		strcpy(vraw_contitle, title);
	}

	if (hConsole != INVALID_HANDLE_VALUE)
	{
		bSuccess = SetConsoleTitle(vraw_contitle);
		if (!bSuccess)
		{
			/* PERR(bSuccess, "SetConsoleTitle"); */
			return;  /* No point reporting an error */
		}
	}
}

/*
**	ROUTINE:	vraw_set_default_title()
**
**	FUNCTION:	set console title to default value
**
**	DESCRIPTION:	set to default value, either default or
**                      user specified value (from vrawtitle)
**
**	ARGUMENTS:	void
**
**	GLOBALS:	static const char *vraw_contitle    current scr title
**
**	RETURN:		success code from set console title
**
**	WARNINGS:	
**
*/
static BOOL vraw_set_default_title(void)
{
	BOOL bSuccess = FALSE;
	DWORD rc = 0;
	
	if (!vraw_contitle[0])
	{
		strcpy(vraw_contitle, vraw_contitle_def);
	}
	bSuccess = SetConsoleTitle(vraw_contitle);
	return bSuccess;
}

/*
**	ROUTINE:	vrawerror()
**
**	FUNCTION:	Display a VRAW error message box
**
**	DESCRIPTION:	Display error. allow user to abort.
**
**	ARGUMENTS:
**	message		The message to display.
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void vrawerror(const char* message)
{
	CHAR szTemp[1024];
	int iButtonPressed;
	
	sprintf(szTemp, "%s\n\nContinue execution?", message);

	MessageBeep(MB_ICONEXCLAMATION);
	iButtonPressed = localMessageBox(NULL, szTemp, "Video Raw Error",
				    MB_ICONEXCLAMATION | MB_YESNO | MB_SETFOREGROUND);

	if (iButtonPressed == IDNO)
	{
		exit(1);
	}
}

/*
**	ROUTINE:	vrawdirectio()
**
**	FUNCTION:	Check if using "direct" I/O
**
**	DESCRIPTION:	WIN32 will normally use direct I/O unless COSTAR (or telnet) is used.
**			If stdout is a pipe then assume stream I/O.
**			Override by setting W4W=1 or 2  if costar present.
**			Or setting WISPTELNET=1.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		
**	1		Direct I/O
**	0		Stream I/O (Costar or telnet)
**
**	WARNINGS:	None
**
*/
int vrawdirectio(void)
{
	static int directio = -1;
	
	if (-1 == directio) /* First time thru */
	{
		char* ptr;
		
		bPipeIO = (GetFileType( GetStdHandle(STD_OUTPUT_HANDLE)) == FILE_TYPE_PIPE) ? 1 : 0;
		directio = (bPipeIO) ? 0: 1;

		if (ptr = getenv("W4W"))
		{
			/*
			**	Setting the W4W environment variable will force directio off.
			*/
			if ('1' == *ptr || '2' == *ptr)
			{
				directio = 0;
			}
		}

		/*
		**	Check if this is a telnet session. If it is then turn off direct io
		*/

		/*
		**	If REMOTEADDRESS is set then assume this is a telnet session.
		**	REMOTEADDRESS is auto set by ATRLS.
		*/
		if (getenv("REMOTEADDRESS"))
		{
			bTelnet = 1;
		}
		
		if (ptr = getenv("WISPTELNET"))
		{
			/*
			**	Setting WISPTELNET=1 will force directio off (without turning Costar on).
			*/
			if ('1' == *ptr)
			{
				bTelnet = 1;
			}
			else
			{
				bTelnet = 0;
			}
		}

		if (bTelnet)
		{
			directio = 0;
		}
		
	}

	return directio;
}

static void vrawFillDefaultAttribute(HANDLE hConsole)
{
	COORD zeroCoord = {0,0};
	DWORD nDone;

	FillConsoleOutputAttribute(hConsole, vrawgetattribute(0), 80*25, zeroCoord, &nDone);
}



void vrawDebugBreak(void)
{
	DebugBreak();
}


void vraw_stty_save()    { /* STUB */ }
void vraw_stty_restore() { /* STUB */ }
void vraw_stty_sync()    { /* STUB */ }

/*
**	ROUTINE:	localMessageBox()
**
**	FUNCTION:	A local frontend to MessageBox()
**
**	DESCRIPTION:	MessageBox() hangs up when costar is used so redirect to stderr.
**
**	ARGUMENTS:	(Same as MessageBox())
**
*/
static int localMessageBox(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType)
{
	if (vrawdirectio())
	{
		return MessageBox(hWnd, lpText, lpCaption, uType);
	}
	else
	{
		fprintf(stderr,"\r\n***************************************");
		fprintf(stderr,"\r\n***************************************");
		fprintf(stderr,"\r\n***************************************");
		
		fprintf(stderr,"\r\n\r\n***%s***\r\n***\r\n***%s***\r\n***\r\n", lpCaption, lpText);

		fprintf(stderr,"\r\n***************************************");
		fprintf(stderr,"\r\n***************************************");
		fprintf(stderr,"\r\n***************************************");

		fflush(stderr);
		
		Sleep(5000);
		return IDNO; /* Pressed the "Continue?" NO button */
	}
}


/*****
FILE *LOGF=NULL;
static char *LOGFNAME="C:\\VIDEO.LOG";
static void writelog(char *fmt,...)
{
	va_list args;
	char bigbuf1[1024], bigbuf2[1024];
	int idx;
	
	va_start(args,fmt);
	
	if (LOGF==NULL) 
	{
		LOGF=fopen(LOGFNAME,"w");
	}
	vsprintf(bigbuf1,fmt,args);
	for (bigbuf2[0]='\0',idx=0 ; bigbuf1[idx]; ++idx)
	{
		char tmpbuf[5];
		if (bigbuf1[idx] >= ' ' && bigbuf1[idx] <= '~') 
		{
			tmpbuf[0]=bigbuf1[idx];
			tmpbuf[1]='\0';
		}
		else
		{
			sprintf(tmpbuf,"<%02X>",bigbuf1[idx]);
		}
		strcat(bigbuf2,tmpbuf);
	}
	
	fprintf(LOGF,"%s\n",bigbuf2);
	fflush(LOGF);
	
}
******/

#endif  /* WIN32 */

/*
**	History:
**	$Log: vrawntcn.c,v $
**	Revision 1.64  2001/11/19 20:21:09  gsl
**	FIx readStreamConsole() for telnet to use ReadConsoleInput().
**	Previously used ReadFile() which was filtering out Escape chars on Win 2000
**	and filtering out Newline chars 0x0a on all systems.
**	
**	Revision 1.63  2001-11-12 15:13:03-05  gsl
**	Change default title
**
**	Revision 1.62  2001-10-15 11:18:13-04  gsl
**	Log perr() messages
**
**	Revision 1.61  2000-03-16 12:23:53-05  gsl
**	Finished color fix
**
**	Revision 1.60  2000-03-16 11:44:16-05  gsl
**	Fix "erratic colors" problem.
**	vrawerase() was resetting the current attribute to "0" the default.
**
**	Revision 1.59  1999-09-15 09:16:16-04  gsl
**	CHange the default console title from NT/95 to NT/9x
**
**	Revision 1.58  1999-06-18 09:03:37-04  gsl
**	Move the logic that loads the icon up so that it occurs as soon as we
**	have the console window handle.
**	THis is to minimize the amount of time the MSDOS icon is visible.
**
**	Revision 1.57  1999-05-27 16:23:56-04  gsl
**	Add logic to set the console icon.
**
**	Revision 1.56  1999-04-30 09:30:35-04  gsl
**	Fix vrawtitle to not abort when costar is used.
**
**	Revision 1.55  1999-02-24 19:02:58-05  gsl
**	Fix the bPipeIO logic - it was reversed.
**	Add bTelnet logic and test REMOTEADDRESS and WISPTELNET
**
**	Revision 1.54  1999-02-17 16:39:18-05  gsl
**	Changes to support Telnet an an NT server.
**	Convert costar code to the generic streamIO logic for both costar and telnet.
**	Mod check_event() to handle telnet using peekConsoleInput() instead
**	of peekNamedPipe() used by costar.
**	Removed old vrawgetc() routine and have vrawinput() and vrawcheck() call
**	the xgetc_xx() functions directly.
**	The NT telnet logic was tested with ATRLS by ATAMAN.
**
**	Revision 1.53  1998-11-19 10:15:01-05  gsl
**	vrawdirectio() will return false if either W4W or WISPTELNET are set.
**
**	Revision 1.52  1998-11-19 09:56:11-05  gsl
**	If using redirected IO then setup input for character at a time.
**	This is needed for telnet access.
**
**	Revision 1.51  1998-08-27 16:57:03-04  gsl
**	Add logic so that the windows don't jump around when a child process ends.
**	The parents window will be move to the same location as the childs.
**
**	Revision 1.50  1998-05-20 16:55:04-04  gsl
**	Add W4W=2 support
**
**	Revision 1.49  1998-04-30 14:37:47-04  gsl
**	Fixed vraw_get_console_hWnd() so it will not do the FindWindow() or the
**	SetConsoleTitle() if there is no console.
**
**	Revision 1.48  1997-07-23 15:20:13-04  gsl
**	Re-do and document the way key scan-codes are handled.
**	For non-ascii key use the true scan-code preceeded by optional
**	state characters for SHIFT, CONTROL, and ALT.
**	Treate escape key as a scan-code (\X\001) instead of an ascii char.
**
**	Revision 1.47  1997-07-17 17:17:10-04  gsl
**	Fix timed reads from costar
**	WaitForMultipleEventsEx() does not work with COSTAR
**
**	Revision 1.46  1997-07-16 22:11:44-04  gsl
**	Add logic to fill the console with the default attribute to prevent
**	the console from flashing black.
**
**	Revision 1.45  1997-07-12 16:13:18-04  gsl
**	Re-work the colors so look ok with costar facs
**	Enable the VCOLORS enviroment variable for settting the colors
**
**	Revision 1.44  1997-07-09 11:59:00-04  gsl
**	Changes for COSTAR and WIN32.
**	Add vrawdirectio() which detects if COSTAR present.
**	Removed much of the bStreamIO logic which was the first pass
**	at the COSTAR stuff.
**	Fix the console window handle logic to return NULL when COSTAR present
**
**	Revision 1.43  1997-05-20 09:18:55-04  gsl
**	Remove unneeded setting and getting of the cursor position.
**
**	Revision 1.42  1997-05-19 17:07:33-04  gsl
**	In vrawWriteConsole() remove the call to vrawGetCursorPosition() as
**	it is not needed.
**
**	Revision 1.41  1997-05-08 11:33:07-04  gsl
**	UPdated with changed for COSTAR
**
**	Revision 1.40  1997-05-02 22:01:50-04  gsl
**	Remove the _flushall() as it was emptying the input stream for wproc
**
**	Revision 1.39  1997-04-30 11:38:54-04  gsl
**	Add the first pass at the COSTAR32 for nt code.
**
**	Revision 1.38  1997-02-24 20:41:53-05  gsl
**	Fixed the set console title logic so it can be called even if the console
**	does not exist yet
**
**	Revision 1.37  1997-01-23 08:40:52-08  gsl
**	Added an screen erase as the last step of vrawinit(), this causes the
**	background to be painted.
**
**	Revision 1.36  1997-01-22 14:25:23-08  gsl
**	Add logic to move window to same location as the parent windows console.
**	Add SetForegroundWindow() to force window to front.
**
**	Revision 1.35  1997-01-14 17:04:13-08  gsl
**	Add a ShowWindow() to ensure the console is visable.
**	Restore the cursor and reset the console size on exit
**
**	Revision 1.34  1997-01-13 17:25:53-08  gsl
**	Changed the vraw_get_console_hWnd() to only check once during vrawinit()
**	In vrawexit() restore the console size
**
**	Revision 1.33  1997-01-11 15:38:12-08  gsl
**	make vrawerror() extern
**
**	Revision 1.32  1997-01-11 13:07:15-08  gsl
**	Update windows 95 fixes
**
**	Revision 1.31  1997-01-10 11:38:45-08  gsl
**	Split CONSOLE_ROWS/COLS into LOGICAL (24x80) and actual_rows/COLS
**	because on Windows95 the console can be a different size then
**	what you requested.
**	Also removed the vraw "scrolling" code which was never properly implemented
**	now you get a "scrolling not supported message".
**
**	Revision 1.30  1997-01-07 20:21:20-08  gsl
**	Reworked and documented all the console IO mode stuff to work around
**	the console mode bugs that Windows 95 has
**
**	Revision 1.29  1996-12-31 17:55:56-08  gsl
**	Cleanup for CONSOLE IO on Windows 95
**	Changed to use a private console buffer.
**	Restore console in vrawexit()
**	Add Sleep(60) milliseconds after "set" calls to console.
**	/
**
**	Revision 1.28  1996-12-27 18:41:28-08  gsl
**	Some cleanup for console IO on win95
**
**	Revision 1.27  1996-11-19 11:14:29-08  jockc
**	removed reset of saved title string.  this makes the title
**	reset properly after a link
**
**	Revision 1.26  1996-11-18 15:52:04-08  jockc
**	revised vraw_set_console_title code; allows title to be set
**	before vrawinit is called.
**
**	Revision 1.25  1996-11-15 18:29:51-08  gsl
**	Cleaned up the kludge for Win95 a bit and added doc.
**
**	Revision 1.24  1996-11-15 16:00:47-08  gsl
**	Consolidated the console size stuff together.
**	Consolidated the cursor setting stuff together
**	Figured out a kludge so works on Win95 - size the console small first
**	then re-size to 24x80
**
**	Revision 1.23  1996-11-14 15:32:52-08  gsl
**	Add vrawinit() check to vrawattribute()
**
**	Revision 1.22  1996-11-14 13:01:51-08  gsl
**	Added checks to ensure vrawinit() was called first in a number of
**	the vraw routines.
**
**	Revision 1.21  1996-11-13 17:45:52-08  gsl
**	Added vrawflush()
**	In vrawprint() if len==0 then do nothing
**
**	Revision 1.20  1996-11-12 15:06:24-08  jockc
**	changed arg of vraw_set_console_title from char* to
**	const char *
**
**	Revision 1.19  1996-11-12 10:24:04-08  jockc
**	missed a vtimeout* call; changed to vrawtimeout
**
**	Revision 1.18  1996-11-11 15:06:35-08  jockc
**	renamed some functions (so all globals would begin with
**	vraw...), local functions all declared static and prototyped
**	at start of module.. global functions now prototyped in vrawntcn.h
**
**	Revision 1.17  1996-09-06 09:49:34-07  jockc
**	added vraw_set_console_title, vraw_get_console_hwnd..
**	fixed right shift key bug that caused a 6 to be typed when
**	right shift was pressed.
**
**	Revision 1.16  1996-08-22 16:38:56-07  jockc
**	began cleanup of 'dead' code (unused here but present because
**	of code borrowed from other vrawxxx module(s))
**
**	Revision 1.15  1996-08-22 10:27:09-07  jockc
**	added call to vtimeout_set() if read times out
**
**	Revision 1.14  1996-08-22 09:28:06-07  gsl
**	Fixed the attibute table (colors)
**	Added standard headers
**	Changed 80 & 24 to CONSOLE_COLS and CONSOLE_ROWS defines
**
**	Revision 1.7  1996-01-02 07:24:39-08  gsl
**	Changed copyright date.
**
**
**
*/

