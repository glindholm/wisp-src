/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


#ifdef unix
/*
 * File:	vrawunix.c
 *
 * Purpose:	This is the implementation file for the vraw package.
 *
 * Author:	david s.a. stine
 *
 * Edits:
 *----------------------------------------------------------------------------
 * 89.05.19	dsa	Created.
 * 89.06.01	dsa	Fixed output flush problem on stdout channel.
 * 89.06.10	dsa	Added #include of vdata.h Changed error returns
 *			to conform to Video conventions.
 * 89.07.01	dsa	Finally made to function under Video. Had to add
 *			global coupling as required by the Video package.
 * 89.07.03	dsa	Added comment headers, signal handling.
 *
 * 89.08.14	fjd	Added VSHUT module.
 * 92.11.30     jec     Added xlat layer for IVS Chinese <==> native on the
 *                      fly translation
 * 93.05.13     jec     Took out flush of stdio descriptors to permit typeahead
 * 93.12.03	gsl	Removed SIGNAL handling 
 *
 */

#ifdef OSF1_ALPHA
#include "/usr/sys/include/sys/ttydev.h"
#endif

#include <sys/types.h>
#include <fcntl.h>
#include <time.h>

#ifdef USEIOCTL
/*
**	Use ioctl() to control the terminal.
*/

#ifdef OSF1_ALPHA
#define _USE_OLD_TTY
#endif

#include <termio.h>

typedef struct termio termio_t;

#else
/*
**	Use POSIX tc functions to control the terminal.
*/

#include <termios.h>
#include <unistd.h>

typedef struct termios termio_t;

#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "video.h"
#include "vlocal.h"
#include "vutil.h"
#include "vraw.h"
#include "vchinese.h"

#define DOLLAR '$'
#define OPENBR '<'
#define CLOSBR '>'

#define E_DOL  0
#define E_OPEN 1
#define E_CLOSE 2
#define E_DIG 3
#define E_ANY 4
#define PEVENTCNT 5

#ifdef VDBGPADST
char *evstr[]=
{
	"DOLLAR $",
	"OPEN   <",
	"CLOSE  >",
	"DIG  0-9",
	"ANY     ",
	NULL
};
#endif

#define S_NORMAL 0
#define S_GOTDOL 1
#define S_GOTOP 2
#define S_RNUMS 3
#define S_EMIT 4
#define S_ABORT 5
#define PSTCNT 6

#ifdef VDBGPADST
char *ststr[]=
{
	"NORMAL",
	"GOTDOL",
	"GOTOP ",
	"RNUMS ",
	"EMIT  ",
	"ABORT ",
	NULL
};
#endif

static int padstatetab[PEVENTCNT][PSTCNT] = 
{        /* NORMAL    GOTDOL   GOTOP     RNUMS      EMIT       ABORT   */
/*DOL*/ { S_GOTDOL,  S_ABORT, S_ABORT,   S_ABORT,   S_NORMAL,  S_NORMAL },  
/*OP */ { S_NORMAL,  S_GOTOP, S_ABORT,   S_ABORT,   S_NORMAL,  S_NORMAL },  
/*CLO*/ { S_NORMAL,  S_ABORT, S_ABORT,   S_EMIT,    S_NORMAL,  S_NORMAL },  
/*DIG*/ { S_NORMAL,  S_ABORT, S_RNUMS,   S_RNUMS,   S_NORMAL,  S_NORMAL },  
/*ANY*/ { S_NORMAL,  S_ABORT, S_ABORT,   S_ABORT,   S_NORMAL,  S_NORMAL }  
};

/*
 * Globals whose storage is defined in other compilation units:
 *
 * int VL_vb_pure:		This will be TRUE (non-zero) when the higher
 *			layers are intending to write output that should
 *			not be post processed. If this flag is FALSE. then
 *			any new-line ('\n') character seen in the output
 *			stream will have be changed to carriage return 
 *			followed by new-line ('\r\n').
 *
 * int VL_vb_count:	This is the count of how many characters are currently
 *			in the output buffer. It is read by higher layers
 *			of Video for optimization purposes.
 *
 *
 * int VL_vbuffering_on():	This flag is non-zero when the higher layers of
 *			Video wish to postpone flushing output waiting in the
 *			buffer until a "logical sequence" of operations is
 *			complete.
 * 
 * int VL_v_control_data:  This flag is non-zero when the higher layers of 
 *                      video have handed us output data that should not
 *                      be processed by the translation routine, ie, control
 *                      data.  The reason is because control data (ie cursor 
 *                      position) resembles IVS characters (^[[3;3H for example)
 *                      which would be munged up by the translator
 */
extern int VL_vb_pure;
extern int VL_vb_count;	/* # of chars valid in vraw_buffer. */
extern int VL_video_inited;

extern int VL_v_control_data;

/*
 * Video requires some "quit" and "interrupt" characters. Under Unix, "quit"
 * normally means "stop process execution now and take a process dump". 
 * "Interrupt" means "stop the current process and return to superior".
 */
#define CONTROL_C		'\003'
#define CONTROL_D		'\004'
#define CONTROL_BACKSLASH	'\034'

#ifndef	VRAW_INT_CHAR
#define	VRAW_INT_CHAR	CONTROL_C
#endif
#ifndef	VRAW_EOF_CHAR
#define	VRAW_EOF_CHAR	CONTROL_D
#endif
#ifndef	VRAW_QUIT_CHAR
#define	VRAW_QUIT_CHAR	CONTROL_BACKSLASH
#endif

/*
 * The "VRAW_BUFSIZ" preprocessor token defines the size (in bytes) of the
 * output holding buffer in this layer of Video.
 */
#ifndef VRAW_BUFSIZ
#define	VRAW_BUFSIZ	4096		/* Size of internal buffer */
#endif

/*
 * Module-specific global variables.
 */

/*
 * Since the whole purpose of these routines is to affect the presentation
 * of terminal IO, we will require some storage for the terminal input/output
 * characteristics. "vraw_termio" is the "termio" struct that is used for
 * defining Video's style of terminal input/output. "prev_termio" is used
 * to store the terminal characteristics as they existed before
 * any low-level Video routines were called.
 */
static termio_t	vraw_termio;		/* vraw (new) term io mode */
static termio_t async_termio;		/* vraw (new) term io mode for nonblocking read */
static termio_t prev_termio;		/* term io previous to vraw_init */

/*
 * vraw_init_flag:	This flag is non-zero after the routine "vrawinit()" has
 *			been called. After vrawexit() is called, is it reset 
 *			to 0.
 *
 * vraw_fildes:		This variable holds the file number of the terminal IO 
 *			path. By convention, it is set to 0.
 */
static int vraw_init_flag = 0;								/* TRUE if module is init'ed.		*/
static int vraw_fildes = 0;								/* File channel to fileno(stdin). 	*/

static int vcurbaud;

#define VRAW_BLOCKED 1
#define VRAW_NONBLOCKED 2
#define VRAW_TIMEOUT 3
static int vraw_mode = VRAW_BLOCKED;

/*
 * vraw_errno:	This variable is used to hold the results of calling
 *		"errno()" after an error is returned from a read, write, open,
 *		close or ioctl operation on the vraw_fildes file number.
 *
 *		This error may be retrieved by higher layers by calling the
 *		vrawerrno() function after they receive a "FAILURE" return
 *		from this layer.
 */
static int vraw_errno = 0;		/* Module-specific error flag */

/*
 * vtimeout_value:      Holds timeout value for nonblocking char input
 *
 */
static int vread_timed_out_flag = FALSE;
static int vtimeout_value = 0;

static int vrawinit(void);
static unsigned char vrawgetc(int wait);
static int vraw_add_outbuf(unsigned char ch);

static int xlat_read(int fd, unsigned char* callerbuf, int bufsize);

/*
 * The following is the local output buffer. This buffer will be flushed
 * to the terminal when a vrawprint(NULL) call is made or the buffer fills
 * during execution of vrawprint() or vrawputc().
 */
static unsigned char vraw_buffer[VRAW_BUFSIZ];


/* This is a scratch buff and size variable used by the routines below
 * which call xlat_read etc 
 */
static  unsigned char xbuf[VRAW_BUFSIZ];
static 	int xcnt;

/*
**	ROUTINE:	vrawttyset()
**
**	FUNCTION:	Set the terminal attributes
**
**	DESCRIPTION:	Frontend for ioctl() or tcsetattr()
**
**	ARGUMENTS:	
**	fildes		The tty file number
**	tt		terminal attributes
**
**	GLOBALS:	
**	vraw_errno	Gets set if there is an error
**
**	RETURN:	       
**	0		Success
**	-1		Error (see vraw_errno)
**
**	WARNINGS:	None
**
*/
int vrawttyset(int fildes, void *p_tt)
{
	int	rc;
	termio_t* tt;
	
	tt = (termio_t *)p_tt;
	
#ifdef USEIOCTL
	rc = ioctl(fildes, TCSETAW, tt);
#else
	rc = tcsetattr(fildes, TCSADRAIN, tt);
#endif

	if ( rc )
	{
		vraw_errno = errno;
	}

	return (rc) ? -1 : 0;
}

/*
**	ROUTINE:	vrawttyget()
**
**	FUNCTION:	Get the terminal attributes
**
**	DESCRIPTION:	Frontend for ioctl() or tcgetattr()
**
**	ARGUMENTS:	
**	fildes		The tty file number
**	tt		terminal attributes
**
**	GLOBALS:	
**	vraw_errno	Gets set if there is an error
**
**	RETURN:	       
**	0		Success
**	-1		Error (see vraw_errno)
**
**	WARNINGS:	None
**
*/
int vrawttyget(int fildes, void *p_tt)
{
	int	rc;
	termio_t* tt;
	
	tt = (termio_t *)p_tt;
	
#ifdef USEIOCTL
	rc = ioctl(fildes, TCGETA, tt);
#else
	rc = tcgetattr(fildes, tt);
#endif

	if ( rc )
	{
		vraw_errno = errno;
	}

	return (rc) ? -1 : 0;
}

/*
**	ROUTINE:	vrawttyalloc()
**
**	FUNCTION:	Allocate memory for the terminal attributes
**
**	DESCRIPTION:   
**
**	ARGUMENTS:	
**
**	GLOBALS:	
**
**	RETURN:	       
**	NULL or		Error (see vraw_errno)
**	void*		pointer to malloced termio structure
**
**	WARNINGS:	None
**
*/
void* vrawttyalloc(void)
{
	termio_t* new;
	
	new = (termio_t*) malloc( sizeof(termio_t) );
	
	if ( !new )
	{
		vraw_errno = ENOMEM;
	}

	return( (void*)new );
}


/*
 * Routine:	vrawinit()		(Module-private routine)
 *
 * Purpose:	To perform initializion required by this complation unit and
 *		the Video package at large.
 *
 * Invocation:	success_flag = vrawinit();
 *
 * Inputs:
 *
 *	Implicit:	stdin, stdout, stderr	-- C RTL Standard IO streams.
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	vraw_errno		-- module error variable.
 *			prev_termio		-- previous terminal settings.
 *			vraw_termio		-- new terminal settings.
 *			vraw_init_flag		-- TRUE upon successful return.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	1. Any pending output on the stdin, stdout and stderr
 *			   streams/files is flushed before this routine changes
 *			   the IO characteristics.
 *			2. The file number associated with the stdin FILE stream
 *			   is set to perform input in non-canonical mode, with
 *			   XON/XOFF enabled, parity checking disabled, single-
 *			   character reads.
 *			3. The output is set to disable all output 
 *			   post-processing.
 *
 * Returns:		SUCCESS -- the vraw layer was successfully initialized.
 *			FAILURE -- some error occurred. The caller should call
 *				   vrawerrno() to discover the reason code.
 *
 */
static int vrawinit()
{
	int bau;
	
	vraw_fildes = fileno(stdin);							/* Get current terminal characteristics.*/
	if ( vrawttyget(vraw_fildes, &prev_termio) )		 			/* These should be restored on exit. 	*/
	{
		vre("VIDEO-E-VRAWINIT vrawttyget() failed on fileno(stdin)=%d errno=%d",vraw_fildes,vraw_errno);
		vraw_fildes = -1;							/* Re-init this 			*/
		return (FAILURE);
	}
	bau = prev_termio.c_cflag & CBAUD;
	vcurbaud = 
#ifdef B38400
	  bau==B38400? 38400:
#endif
#ifdef B19200
	  bau==B19200? 19200:
#endif
	  bau==B9600? 9600:
	  bau==B2400? 2400:
	  bau==B1200? 1200:
	  bau==B300? 300:0;
											/* Copy the old termio buffers to the	*/
											/* ones we will use to set the desired	*/
	memcpy(&vraw_termio, &prev_termio, sizeof(vraw_termio));			/* characteristics.			*/

	/* Input modes: c_iflag */
	vraw_termio.c_iflag = 0;							/* Set everything off			*/
	vraw_termio.c_iflag |= IGNBRK;							/* Set "Ignore Break"			*/
	vraw_termio.c_iflag |= (prev_termio.c_iflag & IXON);				/* Preserve IXON			*/
	vraw_termio.c_iflag |= (prev_termio.c_iflag & IXOFF);				/* Preserve IXOFF			*/
#ifdef IXANY
	vraw_termio.c_iflag |= (prev_termio.c_iflag & IXANY);				/* Preserve IXANY			*/
#endif
#ifdef IMAXBEL
	vraw_termio.c_iflag |= (prev_termio.c_iflag & IMAXBEL);				/* Preserve IMAXBEL			*/
#endif

	/* Local modes: c_lflag */
	vraw_termio.c_lflag &= ~ECHO;							/* Don't echo input. 			*/
	vraw_termio.c_lflag &= ~ICANON;							/* Non-canonical input. 		*/
	vraw_termio.c_lflag &= ~NOFLSH;							/* Allow flushes after INTR signal. 	*/

	/* Output modes: c_oflag */
	vraw_termio.c_oflag &= ~OPOST;							/* No output processing 		*/

	/* Control modes: c_cflag */
#ifdef	EIGHT_BIT
	vraw_termio.c_cflag |= CS8;							/* Allow 8-bit input 			*/
	vraw_termio.c_cflag &= ~PARENB;
#endif

	/*
	 * Set up the default kill characters.
	 * We use the preprocessor constant INT_CHAR for interrupts and
	 * QUIT_CHAR for the QUIT character.
	 * Also, tell the tty device that we want one
	 * character at a time, no matter how long it
	 * takes. (Blocking reads).
	 */
	vraw_termio.c_cc[VINTR]	 = VRAW_INT_CHAR;					/* Must set these for Micro Focus Anim	*/
	vraw_termio.c_cc[VQUIT]	 = VRAW_QUIT_CHAR;
	vraw_termio.c_cc[VEOF]	 = VRAW_EOF_CHAR;

	vraw_termio.c_cc[VMIN]	 = sizeof(char);
	vraw_termio.c_cc[VTIME]	 = 0;							/* Wait forever for 1 char 		*/

	memcpy(&async_termio, &vraw_termio, sizeof(async_termio));			/* setup a termio_t for a nonblocked read*/
	async_termio.c_cc[VMIN] = 0;

	if ( vrawttyset(vraw_fildes, &vraw_termio) )
	{
		vre("VIDEO-E-VRAWINIT vrawttyset() failed fileno=%d errno=%d",vraw_fildes,vraw_errno);

		vrawttyset(vraw_fildes, &prev_termio);					/* Reset it 				*/
		vraw_fildes = -1;
		return (FAILURE);
	}

	vraw_mode = VRAW_BLOCKED;
	VL_vb_count = 0;										/* At start of local buffer.	*/

	vraw_init_flag = TRUE;									/* Mark as initialized.		*/
	VL_video_inited = TRUE;									/* Set  - video was initialized.*/

	return (SUCCESS);
}												/* end of vrawinit(). 		*/

/*
 * Routine:	vrawinput()		(Globally visible)
 *
 * Purpose:	To get a character from the terminal input stream, waiting if
 *		none is currently available.
 *
 * Invocation:	ch_read = vrawinput();
 *
 * Inputs:
 *
 *	Implicit:	vraw_init_flag
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	None.
 *
 *	Explicit:	None.
 *
 * Side effects:	If the routine "vrawinit()" has not been called yet,
 *			this routine will call it before proceeding to get
 *			the character. Hence the "vraw_init_flag" may be
 *			set to non-zero as a result of calling vrawinit().
 *			See comments for vrawinit() for other side effects.
 *
 * Returns:		The character read from the input device. If an error
 *			occured it will return NULL and vrawerrno() will return
 *			a non-zero value; vseterr() is then called to pass then
 *			error up to a higher level out of the "raw" routines.
 *
 */
char vrawinput()
{
	unsigned char	the_char;
	int	error;

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())		/* If init not successful then report set error	*/
		{
			VL_vseterr(vrawerrno());
			return(CHAR_NULL);
		}
	}

	the_char = (unsigned char) vrawgetc(TRUE);	/* Get a character and wait for it */

	if ((error = vrawerrno()))			/* Check for an error */
	{
		VL_vseterr(error);			/* There was an error so set the error number flag */
	}

	return (the_char);
}					/* end of vrawinput() */

/*
 * Routine:	vrawcheck()		(Globally visible)
 *
 * Purpose:	This routine is called to check for the availability of a single
 *		character from the input stream. If none is available, this
 *		routine will return 'NULL' immediately.
 *
 * Invocation:	ch_read = vrawcheck();
 *
 * Inputs:
 *
 *	Implicit:	vraw_init_flag
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	None.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	If the routine "vrawinit()" has not been called yet,
 *			this routine will call it before proceeding to get
 *			the character. Hence the "vraw_init_flag" may be
 *			set to non-zero as a result of calling vrawinit().
 *			See comments for vrawinit() for other side effects.
 *
 * Returns:		The first of any characters waiting in the input buffer
 *			to be read or NULL if none. (Note that this routine
 *			may also return NULL if an error occurred while trying
 *			to read the character.)
 * 			If an error occured then vrawerrno() will return
 *			a non-zero value; vseterr() is then called to pass then
 *			error up to a higher level out of the "raw" routines.
 *
 */
char vrawcheck()
{
	unsigned char	the_char;
	int	error;

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())		/* If init not successful then report set error	*/
		{
			VL_vseterr(vrawerrno());
			return(CHAR_NULL);
		}
	}

	the_char = (unsigned char) vrawgetc(FALSE);	/* Check for a char, don't wait */

	if ((error = vrawerrno()))			/* Check for an error */
	{
		VL_vseterr(error);			/* There was an error so set the error number flag */
	}

	return (the_char);
}					/* end of vrawcheck() */

/*
 * Routine:	vrawgetc()		(Module-private routine)
 *
 * Purpose:	To return the next character available from the input buffer.
 *
 * Invocation:	ch_read - vrawgetc(wait);
 *
 * Inputs:
 *
 *	Implicit:	vraw_termio	-- the terminal IO control structure.
 *			vraw_fildes	-- the terminal file number for IO.
 *			errno		-- run-time library error report.
 *
 *	Explicit:	wait		-- TRUE if routine should block if no
 *					   character is available.
 *
 * Outputs:
 *
 *	Implicit:	vraw_errno	-- error caching variable. (Set when
 *					   an error occurs.)
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	If the caller does not wish to block while reading
 *			a character from the input, this routine will set the
 *			terminal input characteristics to not wait for a 
 *			character for the duration of the single-character read.
 *
 *			If the caller is calling this routine in non-blocking 
 *			mode, any pending output on the terminal device is 
 *			flushed and the ioctl performing the flush will wait 
 *			until the output action is complete before setting 
 *			the terminal characteristics to perform a non-blocking 
 *			read. See "termio(7)" in the manual set.
 *
 * Returns:		The character read from the input, NULL if no 
 *			character was available in non-blocking mode or 
 *			if an error occurred.
 *
 */
static unsigned char vrawgetc(int wait)
{
	extern	int	vraw_errno;	/* Package error caching variable. */
	extern	int	vraw_fildes;	/* Module file number for IO. */
	extern  int     vraw_mode;      /* mode: blocked or nonblocked */

	int	error = 0;		/* Local error flag */
	unsigned char	ch_read = 0;		/* Character we read */

	time_t	before_time, after_time;
	
	/*
	 * If the caller does not wish to wait for a character to appear,
	 * then temporarially set the terminal characteristics to return
	 * immediately, even if no characater is available. Then
	 * reset the characteristics after completion of the read.
	 *
	 * Note to other people who know what they are doing:
	 * ---------------------------------------------------
	 * The ioctl's here are performed with TCSETAW because if a Video
	 * client is performing output in some sort of loop while checking
	 * for a character by calling this routine in non-blocking mode,
	 * the output will be garbled when these ioctl's are performed
	 * with any other command other than SETAW. (SETAF isn't what
	 * you want, either. It only *flushes* pending output on the clists,
	 * and doesn't wait for it to clear the UART...)
	 *
	 * The only other way to get this sort of output/input concurrency
	 * to happen effectively is to make this whole layer a STREAMS client.
	 * That way, you can queue an ioctl to the tail of the stream, behind 
	 * any waiting output and then queue the input as well. (Note that this
	 * may not be the desired action, merely supposition at this time.)
	 *
	 * While this is probably the best way to perform this type of IO
	 * in systems that have STREAMS, not all Un*x systems that were/are
	 * SysIII/SysV based have the AT&T S5R3 level of STREAMS functionality.
	 * And while this type of IO isn't all that pretty, it *is* portable.
	 */

	vraw_errno = 0;	/* Clear this now. It is too late for it to be valid past this point */

	before_time = after_time = 0;
	if (!wait)
	{
		if (vraw_mode!=VRAW_NONBLOCKED)
		{
			async_termio.c_cc[VTIME]=0;
			if ( vrawttyset(vraw_fildes, &async_termio) )
			{
				return (CHAR_NULL);	/* No character read due to error. */
			}
			vraw_mode = VRAW_NONBLOCKED;
		}

		/* note: use of read() in this function was replace by xlat_read(), which is at the bottom 
                ** of this module.  Xlat_read() looks just like read except that it
                ** transparently calls the xlat routine.
                */

		error = xlat_read(vraw_fildes, &ch_read, 1);					/* xlat calls read and then xlat*/
		
		if (error==0)  /* no character available ... pass back a zero */
		{
			ch_read=CHAR_NULL;
		}
	}
	else				/* Blocking read for one character. */
	{
		if (vtimeout_value)
		{
			async_termio.c_cc[VTIME] = 10;  /* value is in tenths of a second.. */
			if ( vrawttyset(vraw_fildes, &async_termio) )
			{
				return (CHAR_NULL);	/* No character read due to error. */
			}
			vraw_mode = VRAW_TIMEOUT;
		}
		else if (vraw_mode != VRAW_BLOCKED)
		{
			if ( vrawttyset(vraw_fildes, &vraw_termio) )
			{
				return (CHAR_NULL);	/* No character read due to error. */
			}
			vraw_mode = VRAW_BLOCKED;
		}
		if (vtimeout_value)
		{
			before_time = time(&before_time);
			do
			{
				vrawtimeout_clear();
				error = xlat_read(vraw_fildes, &ch_read, 1);
				if (vrawtimeout_check())
				{
					after_time = time(&after_time);
				}
			} while (vrawtimeout_check() && ((after_time-before_time)<vtimeout_value));
		}
		else
		{
			error = xlat_read(vraw_fildes, &ch_read, 1);
		}
		if (vtimeout_value)
		{
			alarm(0);
		}
		if (error == -1)
		{
			if (errno != EINTR)
			{
				vraw_errno = errno;
			}
			ch_read = CHAR_NULL;
		}
		if (vrawtimeout_check())
		{
			ch_read = CHAR_NULL;
		}
	}
	return (ch_read);
}					/* end of vrawgetc(). */


/*
 * Routine:	vrawprint()		(Globally visible)
 *
 * Purpose:	To write a buffer without output post-processing.
 *
 * Invocation:	success_flag = vrawprint(buf);
 *
 * Inputs:
 *
 *	Implicit:	vraw_buffer	-- buffer for output.
 *			VL_vb_count	-- number of characters in buffer.
 *			VL_vb_pure		-- CR stuffing flag.
 *			VL_vbuffering_on()	-- false if buffer should be flushed 
 *					   when this call completes.
 *			debugging	-- TRUE if buffer should be flushed when
 *					   this call completes.
 *
 *	Explicit:	buf		-- pointer to buffer to be written to
 *					   terminal.
 *
 * Outputs:
 *
 *	Implicit:	VL_vb_count	-- updated for how many characters are 
 *					   in buffer.
 *		        vraw_buffer	-- characters from input buffer are put
 *					   here.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	See "Side Effects" under vrawflush() and vrawinit().
 *
 * Returns:		SUCCESS	-- the caller may consider his buffer as being
 *				   written.
 *			FAILURE -- the caller should call vrawerrno() to find
 *				   the cause of the error. The state of the
 *				   output buffer is not known.
 *
 */

   static unsigned char padbuf[2048];
   static int           padidx=0, padstate=S_NORMAL, padevent, padms;
   static double        charsper, total;
#  define PADCHAR '\0'
#ifdef VDBGPADST
   static FILE *padlog=NULL;
#endif

int vrawprint(const char* buf)
	/* char	*buf;		IN -- pointer to the buffer. */
{
	const char *p;		/* Fast pointer */
	
	/*
	 * If the vraw module isn't yet initialized, then do so now.
	 */
	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())					/* If init not successful then report set error	*/
		{
			/*
			**	If the vrawinit() failed then the print can't be done.
			**	This used to return FAILURE up to vprint() and beyond, however
			**	no one ever checks the return code from vprint() so a lowlevel
			**	failure never gets detected and vprint() gets called repeatedly.
			**	Also vprint() may never call vrawprint() depending on optimization
			**	and buffering so the vprint() return code is useless as the error
			**	won't be returned until a vdefer(RESTORE) occurs.
			**
			**	The result of all this is that no higher layer is setup to correctly
			**	deal with a vrawinit() failure on output.  
			**	Therefore we print a message and exit.
			*/
			vre("VIDEO-F-VRAWPRINT Uncorrectable error occurred in vrawinit(), aborting!");
			exit(0);
		}
	}

	if (buf == NULL)		/* NIL buffer? */
	{
		return (vrawflush());	/* Yes, so flush the buffer. */
	}

	if (!VL_v_control_data)            /* if this is screen data, not terminal control data */
	{
		IVS_xlat_stream((unsigned char*)buf,strlen(buf),xbuf,&xcnt, &IVS_outctx);  /* call the xlat routine using the output stream context */
		p =  (char*)xbuf;		/* and setup the pointer */
	}
	else
	{
		if (IVS_outctx)					   /* otherwise, adjust the xlcontext to clear any pending  */
		  IVS_outctx->holdstart = IVS_outctx->holdpos;	   /* stuff that it thinks is part of a IVS sequence.       */
								   /* a more object oriented approach would not allow this  */
								   /* type of code.  but this is ok for now                 */
		p=buf;						   /* and use the buffer "buf" with the original data in it */
	}
#ifdef VDBGPADST
	if (padlog==NULL)
	{
		padlog=fopen("padlog","a");
		fflush(padlog);
	}
#endif

	while (TRUE)
	{
		register char ch; 
		int padbytecnt;
		
		ch = *p++;

		if (ch==DOLLAR)	     { padevent=E_DOL; }
		else if (ch==OPENBR)	     { padevent=E_OPEN; }
		else if (ch==CLOSBR)	     { padevent=E_CLOSE; }
		else if ((ch>='0' && ch<='9') 
			 || ch=='*')         { padevent=E_DIG; }
		else if (ch == '\0')         { padevent=E_ANY; }
		else                         { padevent=E_ANY; }
		

#ifdef VDBGPADST
		fprintf(padlog,"event is %s %c, switching from %s ==> %s\n",evstr[padevent],ch,
			ststr[padstate],ststr[padstatetab[padevent][padstate]]);
		fflush(padlog);
#endif
		
		padstate=padstatetab[padevent][padstate];
		if (padstate == S_RNUMS && padidx > 7)
		{
			padstate = S_ABORT;
		}
		switch(padstate)
		{
		      case S_NORMAL:
			if (vraw_add_outbuf(ch)==FAILURE)
			{
				return FAILURE;
			}
			break;
			
		      case S_GOTDOL:
			padbuf[padidx++] = ch;
			continue;
			
		      case S_GOTOP:
			padbuf[padidx++] = ch;
			padms = 0;
			continue;
			
		      case S_RNUMS:
			padbuf[padidx++] = ch;
			if (ch!='*')
			{
				padms *= 10;
				padms += (ch - 0x30);
			}
			else
			{
				padms *= 24;
			}
			continue;
			
		      case S_EMIT:
			charsper = (double)vcurbaud / (double)1000;
			total = charsper * (double)padms;
			padbytecnt = (int)total;
			
			while (padbytecnt)
			{
				if (VL_vb_count + padbytecnt > VRAW_BUFSIZ)
				{
					memset(&vraw_buffer[VL_vb_count],PADCHAR,VRAW_BUFSIZ-VL_vb_count);
					padbytecnt -= (VRAW_BUFSIZ - VL_vb_count);
					VL_vb_count = VRAW_BUFSIZ;
					if (vrawflush() == FAILURE)
					{
						return FAILURE;
					}
				}
				else
				{
					memset(&vraw_buffer[VL_vb_count],PADCHAR,padbytecnt);
					VL_vb_count += padbytecnt;
					padbytecnt=0;
				}
			}
			padidx=0;
			break;
			
		      case S_ABORT:
			while (padidx)
			{
				if (VL_vb_count + padidx > VRAW_BUFSIZ)
				{
					if (vrawflush() == FAILURE)
					{
						return FAILURE;
					}
				}
				else
				{
					memcpy(&vraw_buffer[VL_vb_count],padbuf,padidx);
					VL_vb_count += padidx;
					padidx=0;
				}
			}
			if (vraw_add_outbuf(ch)==FAILURE)
			{
				return FAILURE;
			}
			padidx=0;
			break;
			
		}
		if (ch=='\0')
		  break;
	}

	/*
	 * If we are debugging or no longer holding the output buffer, then
	 * flush the buffer to the terminal now. Otherwise, simply return.
	 */
	if (!VL_vbuffering_on())
	{
		if (vrawflush()==FAILURE)		/* Do the output now! */
		{
			return FAILURE;
		}
	}
	return (SUCCESS);
}					/* End of vrawprint(). */

static int vraw_add_outbuf(unsigned char ch)
{
	vraw_buffer[VL_vb_count++] = ch;

	if ((ch == '\n') && !VL_vb_pure)
	{
		/*
		**	Change a '\n' into a '\r\n'
		*/
		vraw_buffer[VL_vb_count - 1] = '\r';
		if (VL_vb_count >= VRAW_BUFSIZ)
		{
			if (vrawflush() == FAILURE)
			  return (FAILURE);
		}
		vraw_buffer[VL_vb_count++] = '\n';
	}
	if (VL_vb_count >= VRAW_BUFSIZ)
	{
		if (vrawflush() == FAILURE)
		  return (FAILURE);
	}
	return SUCCESS;
}


/*
 * Routine:	vrawputc()		(Globally visible)
 *
 * Purpose:	To write a single character to the output.
 *
 * Invocation:	success_flag = vrawputc(ch);
 *
 *	Implicit:	vraw_buffer	-- buffer for output.
 *			VL_vb_count	-- number of characters in buffer.
 *			VL_vb_pure		-- CR stuffing flag.
 *			VL_vbuffering_on()	-- false if buffer should be flushed 
 *					   when this call completes.
 *
 *	Explicit:	buf		-- pointer to buffer to be written to
 *					   terminal.
 *
 * Outputs:
 *
 *	Implicit:	VL_vb_count	-- updated for how many characters are 
 *					   in buffer.
 *		        vraw_buffer	-- characters from input buffer are put
 *					   here.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	See "Side Effects" under vrawflush() and vrawinit().
 *
 * Returns:		SUCCESS	-- the caller may consider his character as
 *				   being written.
 *			FAILURE -- the caller should call vrawerrno() to find
 *				   the cause of the error. The state of the
 *				   output buffer is not known.
 */
int vrawputc(char ch)
{
	char tmp[2];
	tmp[0]=ch;
	tmp[1]=(char)0;
	vrawprint(tmp);
	return 0;
}


/*
 * Routine:	vrawflush()		(Module-private routine)
 *
 * Purpose:	To flush any output waiting in the vraw_buffer to the output 
 *		device.
 *
 * Invocation:	success_flag = vrawflush();
 *
 * Inputs:
 *
 *	Implicit:	vraw_fildes	-- the output file number of the 
 *					   terminal.
 *			vraw_buffer	-- the buffer to write to the terminal.
 *			VL_vb_count	-- the number of byte of vraw_buffer 
 *					   to write.
 *			
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	VL_vb_count	-- set back to 0 (all output written).
 *			vraw_errno	-- set to 'errno' if an error occurred.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	None.
 *
 * Returns:		SUCCESS		-- buffer was written successfully.
 *			FAILURE		-- the caller should call vrawerrno()
 *					   to discover the cause of the error.
 *
 */
int vrawflush(void)
{
	extern	int	vraw_fildes;	/* File number to write to. */
	extern	int	vraw_errno;	/* Module error caching variable. */

	int		error;		/* Local error/write flag. */
	register int	nb_written = 0;	/* Number of bytes written. */

	/*
	 * Note: While improbable, we should make sure that all the
	 * specified output was actually written before returing.
	 * Unless an error occurs, 'error' will be the number of
	 * bytes taken from the output buffer by the write() call.
	 */

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())
		{
			return(FAILURE);
		}
	}
	

#ifdef VDBGPADST
	fprintf(padlog,"flushbuf[");
	fwrite(vraw_buffer,1,VL_vb_count,padlog);
	fprintf(padlog,"]\n");
	fflush(padlog);
#endif

	while (VL_vb_count > 0)
	{
		error = write(vraw_fildes, &vraw_buffer[nb_written], VL_vb_count);
		if (error == -1)
		{
			VL_vb_count = 0;	/* We don't know what happened... */
			vraw_errno = errno;
			return (FAILURE);
		}
		VL_vb_count -= error;	/* Account for what was written */
		nb_written += error;
	}

	return (SUCCESS);
}					/* End of vraw_flush() */

/*
 * Routine:	vrawexit()		(Globally visible)
 *
 * Purpose:	To reset the terminal when a Video client has completed.
 *		*** The terminal is forced into a SANE mode ***
 *
 * Invocation:	success_flag = vrawexit();
 *
 * Inputs:
 *
 *	Implicit:	prev_termio	-- termio characteristics previous to
 *					   execution of vrawinit().
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	vraw_init_flag	-- Set to 0.
 *			vraw_errno	-- Set to any errors on the way out.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:        The terminal input/output characteristics are reset to
 *			their state before the vrawinit() call was made.
 *
 * Returns:		SUCCESS		-- buffer was written successfully.
 *			FAILURE		-- the caller should call vrawerrno()
 *					   to discover the cause of the error.
 */
int vrawexit()
{
	extern int vraw_fildes;
	termio_t exit_termio;

	int error = 0;

	/*
		This is required to use with Micro Focus animator 3.0. 
		When you "zoom" it sets the terminal to raw mode and sets the
		interrupt char to ESC (first char of PFkeys.)
	*/
	memcpy(&exit_termio, &prev_termio, sizeof(exit_termio));			/* Get a copy of termio struct		*/
											/* Force term into SANE mode		*/
	exit_termio.c_lflag |= ECHO | ICANON;						/* Force ECHO & ICANON on		*/
	exit_termio.c_oflag |= OPOST | ONLCR;						/* Force output processing		*/
	exit_termio.c_iflag |= ICRNL; 							/* Force input processing		*/
	exit_termio.c_cc[VEOF]  = CONTROL_D;						/* Force EOF = ^D			*/
	exit_termio.c_cc[VINTR] = CONTROL_C;						/* Force INTR = ^C			*/
	exit_termio.c_cc[VQUIT] = VRAW_QUIT_CHAR;
	
	vrawflush();									/* Flush any remaining output. 		*/
	error = vrawttyset(vraw_fildes, &exit_termio);
	vraw_fildes = -1;								/* Re-init the file channels.		*/
	vraw_init_flag = 0;								/* No longer initialized.		*/
	return ((error == -1) ? FAILURE : SUCCESS);
}
											/* end of vrawexit().			*/
/*
**	Routine:	vraw_stty_sync()
**
**	Function:	To ensure the stty is set to the values video last used.
**
**	Description:	This routine is used when a debugger or other video
**			handler is active and my be changing the stty values
**			behind video back.  It ensures the stty values are
**			actually set to what video thinks they are.
**
**	Arguments:	None
**
**	Globals:
**	vraw_init_flag
**	vraw_fildes
**	vraw_mode
**
**	Return:		None
**
**	Warnings:	If video has not yet initialized the stty then this routine
**			will do nothing as video has no expectations as to what
**			the stty values are.
**
**	History:	
**	04/20/93	Written by GSL
**
*/
void vraw_stty_sync(void) 
{
	termio_t temp_termio;

	if (!vraw_init_flag) return;						/* If not initialized then return		*/

	vrawttyget(vraw_fildes, &temp_termio);					/* Get current stty settings			*/
	if (0 != memcmp((char *)&temp_termio,(char *)&vraw_termio,sizeof(vraw_termio)))	/* If stty settings are different	*/
	{
		vrawttyset(vraw_fildes, &vraw_termio);				/* Set to required values.			*/
		vraw_mode = VRAW_BLOCKED;
	}
}

/*
**	Routine:	vraw_stty_save()
**			vraw_stty_restore()
**
**	Function:	To save/restore the stty values.
**
**	Description:	vraw_stty_save() will save a copy of the current stty values
**			and vraw_stty_restore() will restore the last stty values
**			that were saved.
**
**	Arguments:	None
**
**	Globals:
**	vraw_stty_saved	Flag if there are saved stty values that can be restored.
**	save_termio	The saved stty values.
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	04/20/93	Written by GSL
**			These routines replace vraw_stty_set/reset which never worked
**			a 100%.
**
*/
static int vraw_stty_saved = 0;
static termio_t save_termio;
void vraw_stty_save(void) 
{
	vraw_stty_saved = 0;
	if (0 == vrawttyget(fileno(stdin), &save_termio))			/* Get currect stty values.			*/
	{
		vraw_stty_saved = 1;
	}
}
void vraw_stty_restore(void)
{
	if (vraw_stty_saved)
	{
		vrawttyset(fileno(stdin), &save_termio);			/* Reset the stty to last stored value		*/
	}
}

/*
**	Routine:	vraw_stty_sane()
**
**	Function:	To force the stty to a sane state.
**
**	Description:	This routine is used when a debugger or other video
**			handler is active and my be changing the stty values
**			behind video back.  It ensures the stty is in a sane state.
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	01/03/94	Written by GSL
**
*/
int vraw_stty_sane(void)
{
	termio_t sane_termio;
	int error = 0;

	if ( vrawttyget(fileno(stdin), &sane_termio) )		 			/* Get the current state		*/
	{
		/*
		**	Probably in background
		*/
		return (FAILURE);
	}
											/* Force term into SANE mode		*/
	sane_termio.c_lflag |= ECHO | ICANON;						/* Force ECHO & ICANON on		*/
	sane_termio.c_oflag |= OPOST | ONLCR;						/* Force output processing		*/
	sane_termio.c_iflag |= ICRNL; 							/* Force input processing		*/
	sane_termio.c_cc[VEOF]  = CONTROL_D;						/* Force EOF = ^D			*/
	sane_termio.c_cc[VINTR] = CONTROL_C;						/* Force INTR = ^C			*/
	sane_termio.c_cc[VQUIT] = VRAW_QUIT_CHAR;
	
	error = vrawttyset(fileno(stdin), &sane_termio);

	return ((error == -1) ? FAILURE : SUCCESS);
}


/*
 * Routine:	vrawerrno()		(Globally visible)
 *
 * Purpose:     To indicate what errors occurred while vraw-layer processing was
 *		happening.
 *
 * Invocation:	errno_int = vrawerrno();
 *
 * Inputs:
 *
 *	Implicit:	vraw_errno	-- the module-specific errno variable.
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	vraw_errno	-- cleared.
 *
 *	Explicit:	None.
 *
 *
 * Side effects:	None.
 *
 * Returns:		The contents of vraw_errno.
 *
 */
int vrawerrno(void)
{
	int	error = vraw_errno;
	vraw_errno = 0;			/* Clear the error */
	return (error);
}					/* End of vrawerrno(). */

/*
 *
 * Procedure to shut down pending inputs in anticipation of the creation
 * of a sub process.
 */
void VL_vshut(void)
{
}

/*
 * vtimeout:  set the timeout value (if any) for normal "blocked" reads
 *
 */
void vrawtimeout(int seconds)
{
	vtimeout_value = seconds;
	vrawtimeout_clear();
}
/*
 * vtimeout_clear:  clear the "timed out" status
 *
 */
void vrawtimeout_clear(void)
{
	vread_timed_out_flag = FALSE;
}
/*
 * vtimeout_check: return TRUE if time out occured on last read
 *                 FALSE otherwise
 *
 */
int vrawtimeout_check(void)
{
	return vread_timed_out_flag;
}
static void vrawtimeout_set(void)
{
	vread_timed_out_flag = TRUE;
}

/*
**	ROUTINE:	vrawdirectio()
**
**	FUNCTION:	Check if using "direct" I/O
**
**	DESCRIPTION:	On Unix this always returns false (0). On unix stream I/O is always used.
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		0
**
**	WARNINGS:	None
**
*/
int vrawdirectio(void)
{
	return 0;
}


/*
**	ROUTINE:	vrawtitle()
**
**	FUNCTION:	Set the window title
**
**	DESCRIPTION:	Stub
**
**	ARGUMENTS:	const char *title   title to set
**
**	GLOBALS:	
**
**	RETURN:		void
**
**	WARNINGS:
**
*/
void vrawtitle(const char *title)
{
	return;
}

/*
**	ROUTINE:	vrawerror()
**
**	FUNCTION:	Display a VRAW error
**
**	DESCRIPTION:	Display error
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
	fprintf(stdout,"\007\r\n%s\r\n",message);
	fflush(stdout);

	if (vraw_init_flag)
	{
		fprintf(stdout, "Press Enter to continue...\r\n");
		fflush(stdout);
		vrawinput(); /* <<<<<< This is dangerous as it can result in recursive calls to vre() */
	}
	else
	{
		VL_vwait(4,0);
	}	
}


#ifdef TEST

/*
 * Define the preprocessor variable 'TEST' to be 1 when you wish to test
 * the code in this compilation unit without invoking the rest of
 * Video.
 */

/* int	VL_vb_count = 0; */			/* Private VL_vb_count for testing. */
/* int	VL_vb_pure = TRUE; */			/* Private VL_vb_pure flag for testing. */

/*
 * This section is only compiled when testing of the vraw package is desired.
 *
 * Author:	david s.a. stine
 *
 */
main()
{
	int	err;
	int	vr_ch;			/* vraw character */
	char	buf[2048];
	FILE	*f;

	fflush(stdout);
	printf("Waiting for some characters: (^D to break loop)\r\n");
	fflush(stdout);
	fflush(stderr);
	vrawinit();
	vrawprint("This line should have no return (cursor here)");
	vrawflush();
	vrawinput();
	vrawputc('\n');
	vrawflush();
	do
	{
		vr_ch = vrawinput();
		if (vr_ch == -1)
		{
			err = vrawerrno();
			printf("\r\n\r\nErrno == %d\r\n", err);
			fflush(stdout);
			fflush(stderr);
			vrawexit();
			return (1);
		}
		vrawputc(vr_ch);	/* Write the character back out */
		vrawflush();		/* And force an immediate flush */
	} while (vr_ch != CEOF);
	vrawflush();		/* Flush the output... */

	printf("\r\n\r\nNow entering no-wait snatch loop...\r\n");
	vr_ch = 0;			/* Make sure to drop into loop...*/
	while (vr_ch == 0)
	{
		vr_ch = vrawinput();
		if (vr_ch == -1)
		{
			err = vrawerrno();
			printf("Errno from vraw_check: %d\n", err);
			fflush(stdout);
			fflush(stderr);
			vrawexit();
			return (1);
		}
		if (vr_ch != 0)
		{
			vrawputc(vr_ch);
			vrawflush();	/* Flush the output */
		}
		if (vr_ch == CEOF)
			break;		/* Leave loop on ^Z */

		vr_ch = 0;

	}				/* while() */

	if (vr_ch == -1)
	{
		printf("\r\nFell out of while() loop!\r\n");
		printf("Error from vraw_check: %d\n", vrawerrno());
		fflush(stdout);
		fflush(stderr);
		vrawexit();
		exit(0);
	}
	/*
	 * Now comes the test of large quantities of output.
	 */
	f = fopen("vrawunix.c", "r");
	fread(buf, sizeof(buf), 1, f);
	vrawprint(buf);
	vrawprint("\r\n\r\n...Flushing buffer\r\n");
	vrawflush();
	fclose(f);

	vrawexit();
	exit(0);
}
#endif /* TEST */


/*
**      Routine:        xlat_read()
**
**      Function:       this acts as a front end to read which xlats incoming
**                      data
**
**      Description:    read() is called  to get data.  IVS_xlat_stream is called
**                      to operate on read data.  This procedure is repeated
**                      until enough bytes to satisfy the call are gathered
**
**      Input:          interface is identical to read(2)
**
**      Output:         appropriate bytes are put into out buffer
**
**      Return:         None
**
**      Warnings:       None
**
**      History:      
**
*/
static int xlat_read(int fd, unsigned char* callerbuf, int bufsize)
{
	static unsigned char raw_buf[2048],xlat_buf[2048];
	static int raw_cnt=0, xlat_cnt=0, xlat_st=0, xlat_end=0;

	extern struct xlcontext *IVS_inctx;
	int pos;
	
	pos=0;
	
	                                                   /* bufsize is the number of bytes requested */
	while (bufsize)                                    /* while user still wants more bytes */
	{
		if (!xlat_cnt)                             /* any bytes in the xlat buffer ? */
		{
			raw_cnt=read(fd,raw_buf,bufsize);  /* no, so get more */
			if (raw_cnt == 0 && vtimeout_value)
			{
				vrawtimeout_set();
				return raw_cnt;
			}
			if (raw_cnt == -1 || raw_cnt == 0)
			{
				return raw_cnt;		   /* read returns -1, return it */
			}
			                                   /* otherwise, xlat what we got and put it in the xlat buffer */
			IVS_xlat_stream(raw_buf,raw_cnt,xlat_buf,&xlat_cnt, &IVS_inctx);

			xlat_st=0;			   /* reset the  "start of valid xlat data" index */
			xlat_end=xlat_cnt;		   /* and the "end of valid xlat data" index */
							   /* xlat_cnt is the number of bytes  */
		}		
		if (xlat_cnt >= bufsize)		   /* do we have sufficient bytes to satisfy his request? */
		{
			                                   /* yes, so memcpy them to him */
			memcpy(callerbuf+pos,xlat_buf+xlat_st,bufsize);

			xlat_st += bufsize;		   /* adjust these indices */
			xlat_cnt -= bufsize;		   /*  "   "   "     "     */
			return bufsize;			   /* return the bytecnt like read would do */
		}
							   /* not enough, so copy what we have into his buffer and loop again */
		memcpy(callerbuf+pos,xlat_buf,xlat_cnt);  
		pos += xlat_cnt;			   /* pos is the "end of valid data" in his buffer */
		bufsize -= xlat_cnt;			   /* bufsize is adjusted by how many bytes we've stuck in his buffer */
		xlat_cnt = xlat_end =  xlat_st = 0;	   /* reset these indices, as all the data was used     */
	}

	return 0;
}
#endif /* unix */

/*
**	History:
**	$Log: vrawunix.c,v $
**	Revision 1.44  2011/10/28 01:42:03  gsl
**	fix warnings
**	
**	Revision 1.43  2011/10/20 01:02:47  gsl
**	mark warnings
**	
**	Revision 1.42  2011/08/23 01:22:03  gsl
**	vrawprint const
**	
**	Revision 1.41  2003/02/07 14:31:41  gsl
**	fix warnings
**	
**	Revision 1.40  2003/02/04 17:05:01  gsl
**	Fix -Wall warnings
**	
**	Revision 1.39  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.38  2003/01/31 19:38:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.37  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.36  2002/07/17 21:06:04  gsl
**	VL_ globals
**	
**	Revision 1.35  2002/07/17 17:49:40  gsl
**	Fix unsigned char warning
**	
**	Revision 1.34  2002/07/16 13:40:20  gsl
**	VL_ globals
**	
**	Revision 1.33  2002/07/15 20:56:40  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.32  2002/07/15 20:20:17  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.31  2002/07/15 20:16:13  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.30  2002/07/15 17:10:06  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.29  2002/07/15 13:29:01  gsl
**	IVS_ globals
**	
**	Revision 1.28  2002/07/12 20:40:44  gsl
**	Global unique WL_ changes
**	
**	Revision 1.27  2002/07/09 19:58:15  gsl
**	Add missing include string.h
**	
**	Revision 1.26  2001/10/12 20:04:26  gsl
**	Changed vrawerror() to only ask to press "ENTER" if initialized
**	otherwise wait 4 seconds
**	
**	Revision 1.25  1999-02-10 11:02:28-05  gsl
**	Change vrawinit() to preserve IXON, IXOFF, IXANY, and IMAXBEL
**
**	Revision 1.24  1998-01-19 09:52:37-05  gsl
**	add time.h and change time vars to be time_t type.
**
**	Revision 1.23  1997-07-12 17:47:42-04  gsl
**	Fix doc on VL_vb_pure.
**	Change octals to control characters.
**	Fix testing code
**
**	Revision 1.22  1997-07-08 16:28:27-04  gsl
**	Add some stub routines for vraw.h
**	Removed unused padstate tables.
**	Add vrawerror() for raw level errors.
**
**	Revision 1.21  1997-06-06 14:45:09-04  scass
**	Put back the USEIOCTL and moved include ot ttydev.h outside of
**	USEIOCTL.
**
**	Revision 1.19  1996-11-13 20:42:33-05  gsl
**	Change to use vrawflush() instead of vrawprint(NULL)
**	Changed vrawflush() to check init
**
**	Revision 1.18  1996-11-12 09:14:14-08  jockc
**	renamed vtimeout* functions to vrawtimeout*
**
**	Revision 1.17  1996-10-10 11:42:25-07  scass
**	added routines to allocate, set and get a termio
**	structure and keep a user defined version of it.
**
**	Revision 1.16  1996-10-09 12:57:47-07  scass
**	Added routine vrawttyalloc() which allocates memory for
**	a termio structure
**
**	Revision 1.15  1996-08-14 12:50:48-07  scass
**	Change routine to not be static vrawttyset() and vrawttyget()
**
**	Revision 1.14  1996-07-26 09:41:52-07  gsl
**	Remove unsigned from vrawprint
**
**	Revision 1.13  1996-07-26 09:05:23-07  gsl
**	Fix prototypes and return values
**
**	Revision 1.12  1996-07-17 16:18:37-07  jockc
**	moved xlat_read to inside the ifdef unix area
**
**	Revision 1.11  1996-07-17 14:23:12-07  gsl
**	fix prototypes
**
**	Revision 1.10  1996-07-11 11:09:24-07  gsl
**	Change to default to use tcsetsttr() and tcgetattr() instead of ioctl().
**	On AIX 4.1 ioctl() was not handling type-ahead correctly.
**	The newer posix machanism for controlling the terminal is
**	to use tcsetattr() and tcgetattr().
**	If the system does not have this posix suppport you can compile
**	with the USEIOCTL define.
**
**	Revision 1.9  1996-03-12 05:32:40-08  gsl
**	CHange use of holding_output to vbuffering_on()
**
**
**
*/
