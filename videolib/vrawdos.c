/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


#ifdef  MSDOS
/*
 * File:        vrawdos.c
 *
 * Purpose:     This is the implementation file for the vraw package.
 *
 */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#include <dos.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#include <graph.h>

#include "video.h"
#include "vlocal.h"
#include "vmodules.h"

#define EXT_VRAWDOS
#include "vrawdos.h"

static void initcolors();
static int xdebug();
static void xclock();
static int x_lastline();
static int xdo_debug();
static void xd_put_menu();
static int xd_mp();
static int xd_not_available();
static int xd_invalid();
static void xd_clr_menu();

#define EXT_PREFIX ((unsigned char)29)          /* Extended character set prefix        */

#define DOS_COLS	80
#define DOS_ROWS	25

/*
 * Globals whose storage is defined in other compilation units:
 *
 * int VL_vb_pure:         This will be TRUE (non-zero) when the higher
 *                      layers are intending to write output that should
 *                      not be post processed. If this flag is FALSE. then
 *                      any line-feed ('\012') character seen in the output
 *                      stream will have a carriage return ('\015') character
 *                      pre-pended to it.
 *
 *
 * int debugging:       This is a debugging hook flag which will cause any
 *                      output data in the buffer owned by this compilation
 *                      unit to be flushed to the output device immediately.
 *
 */
extern int VL_vb_pure;
extern int VL_video_inited;

#ifdef  NO_ERRNO
extern  int     errno ;
#endif  /*  NO_ERRNO    */

/*
 * Video requires some "quit" and "interrupt" characters. Under Unix, "quit"
 * normally means "stop process execution now and take a process dump". 
 * "Interrupt" means "stop the current process and return to superior".
 */
#ifndef VRAW_INT_CHAR
#define VRAW_INT_CHAR   '\003'
#endif
#ifndef VRAW_QUIT_CHAR
#define VRAW_QUIT_CHAR  CQUIT
#endif

/*
 * The "VRAW_BUFSIZ" preprocessor token defines the size (in bytes) of the
 * output holding buffer in this layer of Video.
 */
#ifndef VRAW_BUFSIZ
#define VRAW_BUFSIZ     4096            /* Size of internal buffer */
#endif


/*
 * Module-specific global variables.
 */

static unsigned char vrawgetc(int);
static void xputc();
static int xchin();
extern int getch();
extern int putch();

/*
 * vraw_init_flag:      This flag is non-zero after the routine "vrawinit()" has
 *                      been called. After vrawexit() is called, is it reset 
 *                      to 0.
 *
 */
static int vraw_init_flag = 0;                                                          /* TRUE if module is init'ed.           */

/*
 * vraw_errno:  This variable is used to hold the results of calling
 *              "errno()" after an error is returned from a read, write, open,
 *              close or ioctl operation on the vraw_fildes file number.
 *
 *              This error may be retrieved by higher layers by calling the
 *              vrawerrno() function after they receive a "FAILURE" return
 *              from this layer.
 */
static int vraw_errno = 0;              /* Module-specific error flag */

/*
 * vtimeout_value:      Holds timeout value for nonblocking char input
 *
 */
static int vread_timed_out_flag = FALSE;
static int vtimeout_value = 0;

/*
 * vraw_signal: This variable holds the action of the SIGINT (interrupt)
 *              signal previous to the execution of the vrawsigset()
 *              funcion. This is used to restore the previous interrupt
 *              action upon program exit.
 */
void    (*vraw_signal)();


/*
 * The following is the local output buffer. This buffer will be flushed
 * to the terminal when a vrawprint(NULL) call is made or the buffer fills
 * during execution of vrawprint() or vrawputc().
 */
static unsigned char vraw_buffer[VRAW_BUFSIZ];

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
static int vrawinit()
{
	fflush(stdout);                                                                 /* Flush all pending output on the      */
	fflush(stderr);                                                                 /* stdout and stderr channels. Then grab*/
											/* the fileno for the stdout file and   */

	initcolors();

	if( ! screen_memory )                                                           /* If screen_memory not initialized.    */
	{
		screen_memory = BASE_VIDEO_ADDR;                 			/* Set it to the base video address.    */
	}

	vrawerasetype( FULL_SCREEN );                                                   /* Erase the full screen to defaults.   */

											/* Erase bottom line of screen:         */
	out_row = 24;
	out_atr = BACK_BLACK | FORE_WHITE;
	for( out_col = 0 ; out_col < DOS_COLS ; ++out_col )
	{
		SET_CVT( ' ' );
	}
	out_row = 0;
	out_col = 0;
	out_atr = attributes[0];

	vraw_init_flag = TRUE;                                                                  /* Mark as initialized.         */
	VL_video_inited = TRUE;                                                                    /* Set  - video was initialized.*/
	return (SUCCESS);
}                                                                                               /* end of vrawinit().           */

/*
 * Routine:     vrawinput()             (Globally visible)
 *
 * Purpose:     To get a character from the terminal input stream, waiting if
 *              none is currently available.
 *
 * Invocation:  ch_read = vrawinput();
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
 * Side effects:        If the routine "vrawinit()" has not been called yet,
 *                      this routine will call it before proceeding to get
 *                      the character. Hence the "vraw_init_flag" may be
 *                      set to non-zero as a result of calling vrawinit().
 *                      See comments for vrawinit() for other side effects.
 *
 * Returns:             The character read from the input device or NULL if
 *                      some error  occured.
 *
 */
char vrawinput()
{
	if (!vraw_init_flag) vrawinit();
	return (vrawgetc(TRUE));        /* Get a character and wait for it */
}                                       /* end of vrawinput() */

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
	return (vrawgetc(FALSE));       /* Check for a char, don't wait */
}

/*      vrawcheck() should be replaced with vrawchin()  */
/*      when the character itself is not needed.        */

int vrawchin()
{
    if (!vraw_init_flag) vrawinit();
    return (xchin());   /* Check for is there any input char(s) */
}

/*
 * Routine:     vrawgetc()              (Module-private routine)
 *
 * Purpose:     To return the next character available from the input buffer.
 *
 * Invocation:  ch_read - vrawgetc(wait);
 *
 * Inputs:
 *
 *      Explicit:       wait            -- TRUE if routine should block if no
 *                                         character is available.
 *
 * Outputs:
 *
 *      Explicit:       None.
 *
 *
 * Returns:             The character read from the input, NULL if no 
 *                      character was available in non-blocking mode or 
 *                      if an error occurred.
 *
 */
static  unsigned        char    vrawgetc(wait)
int     wait;                   /* IN -- TRUE means read synchronously/ */
{
	unsigned        char    xgetc_nw();
	unsigned        char    xgetc_w();

	unsigned        char    ch_read = 0;            /* Character we read */

	SET_MF( 2 );                                                            /* Set mode flag to GREEN.                      */

	vrawsetcursor();                                                        /* Position the cursor before getting input.    */

	/*
	 * If the caller does not wish to wait for a character to appear,
	 * then return immediately, even if no characater is available.
	 */

	if (!wait)      /* Do not wait for a character to be typed.     */
	{
		ch_read = xgetc_nw() ;  /* No Wait         */
	}
	else
	{
		ch_read = xgetc_w() ;   /* Wait for a character */
	}

	SET_MF( 4 );                                                            /* Set mode flag to RED.                        */

	return (ch_read);
}

int vrawprint(buf) unsigned char *buf;          /* Do raw output.       */
{
	register unsigned char *p;              /* Fast pointer         */

	if (!vraw_init_flag) vrawinit();        /* Init if not done.    */
	p = buf;                                /* Init fast pointer.   */
	if (p)                   		/* Anything to do?      */
	{
		while (*p) xputc(*p++);         /* Output pure string.  */
	}
	return (SUCCESS);                       /* Success always.      */
}

int vrawflush()
{
	return(SUCCESS);
}

int vrawputc(ch) char ch;                       /* Output single char.  */
{
	unsigned char x[2];                     /* Working string.      */

	if ((ch == 0) || VL_vb_pure)
	{
		xputc(ch);                      /* Output if a null.    */
	}
	else
	{
		x[0] = ch;                      /* Copy the character.  */
		x[1] = 0;                       /* End the string.      */
		vrawprint(x);                   /* Do by rawprint.      */
	}
	return(SUCCESS);
}

/*
 * Routine:     vrawexit()              (Globally visible)
 *
 * Purpose:     To reset the terminal when a Video client has completed.
 *
 * Invocation:  success_flag = vrawexit();
 *
 * Inputs:
 *
 *      Implicit:       prev_termio     -- termio characteristics previous to
 *                                         execution of vrawinit().
 *
 *      Explicit:       None.
 *
 * Outputs:
 *
 *      Implicit:       vraw_init_flag  -- Set to 0.
 *                      vraw_errno      -- Set to any errors on the way out.
 *
 *      Explicit:       None.
 *
 *
 * Side effects:        The terminal input/output characteristics are reset to
 *                      their state before the vrawinit() call was made.
 *
 * Returns:             SUCCESS         -- buffer was written successfully.
 *                      FAILURE         -- the caller should call vrawerrno()
 *                                         to discover the cause of the error.
 */
int vrawexit()
{
	int error = 0;

#if defined(_INTELC32_) || defined(WATCOM)
	_clearscreen(_GCLEARSCREEN);
#endif
	vrawcursor(1);

	vrawsetcursor();                                                                /* Move cursor to out_row, out_col      */

	vraw_init_flag = 0;                                                             /* No longer initialized.               */
	return ((error == -1) ? FAILURE : SUCCESS);
}                                                                                       /* end of vrawexit().                   */

/*
 * Routine:     vrawerrno()             (Globally visible)
 *
 * Purpose:     To indicate what errors occurred while vraw-layer processing was
 *              happening.
 *
 * Invocation:  errno_int = vrawerrno();
 *
 * Inputs:
 *
 *      Implicit:       vraw_errno      -- the module-specific errno variable.
 *
 *      Explicit:       None.
 *
 * Outputs:
 *
 *      Implicit:       vraw_errno      -- cleared.
 *
 *      Explicit:       None.
 *
 *
 * Side effects:        None.
 *
 * Returns:             The contents of vraw_errno.
 *
 */
int vrawerrno()
{
	int     error = vraw_errno;
	vraw_errno = 0;                 /* Clear the error */
	return (error);
}                                       /* End of vrawerrno(). */

/*
 *
 * Procedure to shut down pending inputs in anticipation of the creation
 * of a sub process.
 */
void VL_vshut()
{
}


/*      MS-DOS low-level SECTION:       */

/*      Get and Put characters using these interupt 21H functions:      */

/*      0x06    - direct output to the screen                   */
/*      0x06 (with 0xFF) input from the screen (detect ^C)      */
/*      0x07    - direct input from the keyboard                */
/*      0x0B    - Look for an input character                   */

				/* Interupt 21H function definitions:   */
#define I21H_DCIO       0x06    /* Direct Console I/O (with echo)       */
#define I21H_DCIN       0x07    /* Direct Console Input; no echo        */
#define I21H_CHIN       0x0B    /* Check Input Device Status            */

#define KEYBOARD_INTERRUPT      0x16
#define GET_KEYSTROKE           0x10

#define AH      (regs.h.ah)
#define AL      (regs.h.al)
				/* VIDEO uses NULL as a flag indicating */
				/* the absence of any input characters. */
				/* The DOS ansi driver, however, uses   */
				/* NULL as a prefix for all extended    */
				/* characters such as function keys.    */
				/* At this low-level, the DOS prefix    */
				/* will be converted to 0x80 for VIDEO  */
				/* compatability and is reflected in    */
				/* the "ansidos" video cap file.        */


static  union   REGS    regs ;

	/*      xgetc_nw - get char; no wait                    */
	/*              get a character if one is available,    */
	/*              if character is not available, return 0 */

static  unsigned        char    xgetc_nw()
{
	if ( xchin() )
	{
		return ( xgetc_w() ) ;
	}
	else
	{
		return ( 0 ) ;
	}
}

	/*      xgetc_w - get char; wait                        */
	/*              get a character if one is available,    */
	/*              if char is not available, wait for it.  */

static  unsigned        char    xgetc_w()
{
	int     ch = 0;

	while( 0 == ch )                                /* Repeat after xdebug. */
	{
		if (vtimeout_value < 1) 		/* Read without TIMEOUT */
		{
			while( ! xchin() );             /* Wait for char input. */
		}
		else 					/* Read with TIMEOUT 	*/
		{
			time_t	stop_time;

			stop_time = time(NULL) + vtimeout_value;
			while( ! xchin() )		/* Loop until char ready*/
			{
				/*
				**	Check if we've reached the stop time.
				**	If so then indicate a timeout.
				*/
				if ( time(NULL) >= stop_time )
				{
					vrawtimeout_set();
					/*
					**	Return a NULL char to indicate
					**	a timeout.  Any real NULLs will
					**	be converted before returned.
					*/
					return(0);
				}
			}
		}

		ch = getch();                           /* Get the character.   */

		if ( ch == 0 )                          /* extended character   */
		{
			return(EXT_PREFIX);             /* Never return a NULL  */
		}
		else if ( ch == 3 )                     /* Abort on control C.  */
		{
			VL_vexit();
			exit(0);
		}
		else if ( ch == 24 )                    /* Debug on control X.  */
		{
			ch = xdebug();                  /* Enter debug routine. */
			if( ch != 24 )                  /* If not control X.    */
			{
				ch = 0;                 /* Repeat this loop.    */
			}
		}
		else
		{                                       /* Return character.    */
			return( 0x007F & ch ) ;         /* Filter out high bit. */
		}
	}
}


	/*      xchin - check character input                   */
	/*              returns 1 if there is a char to be read */
	/*              returns 0 if no chars are waiting.      */

static  int     xchin()                 /* Check for available input    */
{

#ifndef WATCOM								/* WATCOM does not support _strtime; researching strftime */
	xclock();
#endif

	AH = I21H_CHIN ;
	intdos ( &regs, &regs ) ;

	if ( AL == 0 )
	{
		return ( 0 ) ;          /* no input available   */
	}
	else
	{
		return ( 1 ) ;          /* input is available   */
	}
}

static void xclock()					/* Display a clock on the screen. */
{
	char    time_buf[9];
	int     save_row, save_col, save_atr;

#if defined(_MSC_VER) || defined(_INTELC32_)
	_strtime( time_buf );
#endif
	save_row = out_row;
	save_col = out_col;
	save_atr = out_atr;
	out_row = 24;
	out_atr = BACK_BLACK | FORE_WHITE;
#if     0
	for( out_col = 0 ; out_col < DOS_COLS ; ++out_col )
	{
		SET_CVT( ' ' );
	}
#endif
	out_col = 70;
	vrawprint( time_buf );
	out_row = save_row;
	out_col = save_col;
	out_atr = save_atr;
}

	/*      xputc - put a character                         */
	/*              send a raw char to the DOS ansi driver. */

static void xputc( ch )
char ch;
{
	if ( '\012' == ch )
	{
		if( (23 == out_row) || (scroll_bottom == out_row) )
		{
			vrawscroll( 1 );                /* scroll up 1 line.    */
		}
		else
		{
			out_row++;
		}
		if( ! VL_vb_pure )
		{
			out_col = 0;
		}
	}
	else
	{
		SET_CVT(ch);
		NEXT_COL;
	}
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
	int	i,j;
	int	attr;

	if (ptr = getenv("VCOLORS"))
	{
		for(i=0; i<16 && *ptr && *(ptr+1); i++)
		{
			buff[0] = *ptr++;
			buff[1] = *ptr++;
			buff[2] = (char)0;
			if (1==sscanf(buff,"%2x",&j))
			{
				attr = j;
				attr <<= 8;
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
void vrawsetattributes(attrs) int attrs[];
{
	int	i;
	int	temp;

	for(i=0;i<16;i++)
	{
		temp = attrs[i] & 0xff00;
		if (temp)
		{
			attributes[i] = temp;
		}
	}
}

void vrawattribute(atr) int atr;
{
	out_atr = attributes[atr%16];
}

void vrawmove(row,col) int row, col;
{
	out_row = row % DOS_ROWS;
	out_col = col % DOS_COLS;
}

void vrawsetcursor()
{
	_settextposition( (1 + out_row), (1 + out_col) );                       /* Position the cursor to the current location. */
}

void vrawerasetype(type) int type;
{
	int     fr, fc, tr, tc;                         /* from/to row/column   */

	switch( type )
	{
	case TO_EOS:                                            /* To End Of Screen.            */
		fr = out_row;
		fc = 0;
		tr = 23;
		tc = 79;
		break;
	case TO_EOL:                                            /* To End Of Line.              */
		fr = out_row;
		fc = out_col;
		tr = out_row;
		tc = 79;
		break;
	case FULL_SCREEN:                                       /* Full Screen Erase.           */
	default:                                                /* default to Full Screen.      */
		fr = 0;
		fc = 0;
		tr = 23;
		tc = 79;
		break;
	}

	vrawerase( fr, fc, tr, tc );
}

void vrawerase(fr,fc,tr,tc) int fr,fc,tr,tc;					/* from/to row/column   */
{
	int save_row, save_col;

	if( !(tc < DOS_COLS) )
	{
		tc = DOS_COLS-1;
	}
	save_row = out_row;
	save_col = out_col;
	for( out_row=fr ; out_row <= tr ; ++out_row )
	{
		for( out_col=fc ; out_col <= tc ; ++out_col )
		{
			SET_VT( out_row, out_col, ' ', attributes[0]);		/* Always erase with default attribute		*/
		}
	}
	out_row = save_row;
	out_col = save_col;
}

void vrawsetscroll(top,bottom) int top, bottom;
{
	int     hold_it;
										/* Make sure top is not greater than bottom:    */
	if( top > bottom )
	{
		hold_it = top;
		top = bottom;
		bottom = hold_it;
	}
											/* Make sure top is on the page:        */
	if( (top < 0) || (top > 23))
	{
		scroll_top = 0;
	}
	else
	{
		scroll_top = top;
	}
											/* Make sure bottom is on the page:     */
	if( (bottom < 0) || (bottom > 23))
	{
		scroll_bottom = 23;
	}
	else
	{
		scroll_bottom = bottom;
	}
}

int vrawscroll(dir) int dir;
{
	int     save_row;
	int     top, bottom, lines, mid, count, move_to, move_from, fill_top;

	if (0 == dir)                                                                          /* dir must not be 0.           */
	{
		return( 0 );
	}

	save_row = out_row;                                                                     /* Save out_row.                */

	if( (out_row < scroll_top) || (out_row > scroll_bottom) )                               /* If out of scroll window:     */
	{
		top = 0;                                                                        /* Top row of screen.           */
		bottom = 23;                                                                    /* Bottom row of screen.        */

		vrawsetscroll( (scroll_top - dir), (scroll_bottom - dir) );                     /* Move scroll window too.      */
	}
	else                                                                                    /* Scroll within scroll window. */
	{
		top = scroll_top;
		bottom = scroll_bottom;
	}

	lines = (dir > 0) ? dir : (0 - dir);                                                    /* Absolute value of dir.       */
	lines = (lines > (1 + bottom - top)) ? (1 + bottom - top) : lines;                      /* No larger than window size.  */
	mid = top + lines;                                                                      /* One of the move points.      */
	count = 1 + bottom - mid;                                                               /* Number of lines to move.     */

	if( dir > 0 )                                                                           /* Move lines up.               */
	{
		move_to = top;
		move_from = mid;
		fill_top = bottom - lines;                                                      /* Space fill from the bottom.  */
	}
	else                                                                                    /* Move lines down.             */
	{
		move_to = mid;
		move_from = top;
		fill_top = top;                                                                 /* Space fill from the top.     */
	}
												/* Move the video memory.       */
	memmove( (screen_memory + (move_to * DOS_COLS)), (screen_memory + (move_from * DOS_COLS)), (count * 160) );

	out_row = fill_top;
	while( out_row < (fill_top + lines))  							/* For each row to be filled.   */
	{
		out_row++;
		vrawerase( out_row, 0, out_row, 79 );						/* Erase the line.              */
	}

	out_row = save_row;                                                                     /* Reset out_row value.         */
	return 0;
}

vrawcursor( state )
int	state;
{
	if ( state )
	{
		_settextcursor ( CURSOR_UNDERLINE );
	}
	else
	{
		_settextcursor ( CURSOR_OFF );
	}

	return( SUCCESS );
}

/*      End of low-level MS-DOS section */


#define MM_ROWS 17
static  char    *xm_mm[MM_ROWS] = 
{
	"",
	"Debug MAIN MENU",
	"",
	"M. Toggle Mode Flag On/Off            ",
	"",
	"N. Change Color Of Rendition NORMAL   ",
	"U. Change Color Of Rendition UNDERLINE",
	"R. Change Color Of Rendition REVERSE  ",
	"                   B. Change Color Of Rendition BOTH (Reverse and Underline)",
	"",
	"1. Change Character Of Pseudo Blank 1 ",
	"2. Change Character Of Pseudo Blank 2 ",
	"3. Change Character Of Pseudo Blank 3 ",
	"4. Change Character Of Pseudo Blank 4 ",
	"",
	"Press Q to return to application.",
	""
};

static xdebug()
{
	int     ch;

	ch = x_lastline( "Press Ctrl-X again to transmit, any other for DEBUG." );

	if( ch != 24 )
	{
		ch = xdo_debug();
	}

	return( ch );
}

static int xdo_debug()
{
	int     ch=0;

	_setactivepage(1);
	_setvisualpage(1);

	xd_put_menu( MM_ROWS, xm_mm );
	ch = getch();

	while( (ch != 'q') && (ch != 'Q') && (ch != '\033') && (ch != '\003') )
	{
		switch( ch )
		{
		case 'm':
		case 'M':
			ch = xd_mp();
			break;
		case 'n':
		case 'N':
			ch = xd_not_available();
			break;
		case 'u':
		case 'U':
			ch = xd_not_available();
			break;
		case 'r':
		case 'R':
			ch = xd_not_available();
			break;
		case 'b':
		case 'B':
			ch = xd_not_available();
			break;
		case '1':
			ch = xd_not_available();
			break;
		case '2':
			ch = xd_not_available();
			break;
		case '3':
			ch = xd_not_available();
			break;
		case '4':
			ch = xd_not_available();
			break;
		default:
			ch = xd_invalid();
			break;
		}
	}
	xd_clr_menu( MM_ROWS, xm_mm );

	_setactivepage(0);
	_setvisualpage(0);
	return( ch );
}

static void xd_put_menu(rows,menu) int rows; char *menu[];
{
	int     i, col;

	for( i = 0 ; i < rows ; ++i )
	{
		if( menu[i][0] != '\0' )
		{
			col = 40 - ( strlen( menu[i] ) / 2 );
			_settextposition( (i + 1 ), col );
			printf( menu[i] );
		}
	}
}

static void xd_clr_menu(rows,menu) int rows; char *menu[];
{
	int     i, j, col;

	for( i = 0 ; i < rows ; ++i )
	{
		if( menu[i][0] != '\0' )
		{
			col = 40 - ( strlen( menu[i] ) / 2 );
			_settextposition( (i + 1 ), col );
			for(j = 0; j < (int) strlen(menu[i]); ++j)
			{
				putch(' ');
			}
		}
	}
}

static int xd_mp()
{
	int     ch;

	_settextposition( 24, 16 );
	if( mode_flag )
	{
		mode_flag = 0;
		printf( "Mode flag is now OFF.  Select another function." );
	}
	else
	{
		mode_flag = 1;
		printf( "Mode flag is now ON.  Select another function." );
	}
	ch = getch();
	_settextposition( 24, 16 );
	printf( "                                                " );

	return( ch );
}

static  xd_invalid()
{
	int     ch;

	_settextposition( 24, 17 );
	printf( "Invalid response.  Select another function." );
	ch = getch();
	_settextposition( 24, 17 );
	printf( "                                           " );

	return( ch );
}

static int xd_not_available()
{
	int     ch;

	_settextposition( 24, 7 );
	printf( "Selection not available at this time.  Make another selection." );
	ch = getch();
	_settextposition( 24, 7 );
	printf( "                                                              " );

	return( ch );
}

static int x_lastline(str) char str[];
{
	int     save_row, save_col, save_atr;
	int     ch, i;

	save_row = out_row;
	save_col = out_col;
	save_atr = out_atr;

	out_atr = BACK_BLACK | FORE_MAGENTA;
	out_row = 24;
	out_col = 2;

	if( strlen( str ) > 66 )                /* Set maximum length   */
	{
		str[66] = '\0';
	}

	for( i = 0 ; str[i] ; ++i )             /* Remove newlines */
	{
		if ( '\n' == str[i] )
		{
			str[i] = ' ';
		}
	}

	vrawprint( str );
	ch = getch() ;

	out_row = 24;
	out_col = 2;

	for( i = 0 ; i < (int) strlen( str ) ; ++i )  /* Clear bottom line    */
	{
		xputc( ' ' );
	}

	out_row = save_row;
	out_col = save_col;
	out_atr = save_atr;

	return( ch );
}

x_p_lastline( str )
char    str[];
{
	int     ch, i;

	if( strlen( str ) > 66 )                /* Set maximum length   */
	{
		str[66] = '\0';
	}

	for( i = 0 ; str[i] ; ++i )             /* Remove newlines */
	{
		if ( '\n' == str[i] )
		{
			str[i] = ' ';
		}
	}

	_settextposition( 25, 3 );
	printf( str );
	ch = getch();

	_settextposition( 25, 3 );

	for( i = 0 ; i < (int) strlen( str ) ; ++i )  /* Clear bottom line    */
	{
		putch( ' ' );
	}

	return( ch );
}

extern void v_modeflag(int bg, int fg, int ch)
{
	if( mode_flag )
	{
		SET_VT( 24, 0, ch, ((bg << 12) | (fg << 8)));
	}
}

put_lastline( str )
char    str[];
{
	int i;

	if (strlen( str ) > 66)                 /* Set maximum length   */
	{
		str[66] = '\0';
	}

	for( i = 0 ; str[i] ; ++i )             /* Remove newlines */
	{
		if ( '\n' == str[i] )
		{
			str[i] = ' ';
		}
	}

	_settextposition( 25, 3 );
	printf( "%s", str );
	return(0);
}

clear_lastline()
{
	int     i;
	_settextposition( 25, 3 );

	for( i = 0 ; i < 66; ++i )  /* Clear bottom line    */
	{
		putch( ' ' );
	}

	return( 0 );
}

/*
 * vrawtimeout:  set the timeout value (if any) for normal "blocked" reads
 *
 */
void vrawtimeout(seconds) int seconds;
{
	vtimeout_value = seconds;
	vrawtimeout_clear();
}
/*
 * vrawtimeout_clear:  clear the "timed out" status
 *
 */
void vrawtimeout_clear()
{
	vread_timed_out_flag = FALSE;
}
/*
 * vrawtimeout_check: return TRUE if time out occured on last read
 *                 FALSE otherwise
 *
 */
int vrawtimeout_check()
{
	return vread_timed_out_flag;
}

void vrawtimeout_set()
{
	vread_timed_out_flag = TRUE;
}

void vraw_stty_save()
{
}

void vraw_stty_restore()
{
}

void vraw_stty_sync()
{
}

#endif  /* MS-DOS */
/*
**	History:
**	$Log: vrawdos.c,v $
**	Revision 1.18  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.17  2002/07/17 21:06:03  gsl
**	VL_ globals
**	
**	Revision 1.16  2002/07/16 14:11:48  gsl
**	VL_ globals
**	
**	Revision 1.15  2002/07/16 13:40:21  gsl
**	VL_ globals
**	
**	Revision 1.14  2002/07/15 20:16:12  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/07/15 17:10:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  1997/07/09 15:57:16  gsl
**	Remove unsupported erase codes
**	
**	Revision 1.11  1996-11-14 11:44:00-05  jockc
**	changed vtimeout* to vrawtimeout*
**
**	Revision 1.10  1996-11-13 17:44:47-08  gsl
**	Added vrawflush() instead of vrawprint(NULL)
**
**	Revision 1.9  1996-03-12 05:33:16-08  gsl
**	Remove reference to holding_output
**
**
**
*/
