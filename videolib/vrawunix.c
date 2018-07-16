			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1992				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

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
 *
 */
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#ifdef OSF1_ALPHA
#define _USE_OLD_TTY
#endif
#include <termio.h>
#ifdef OSF1_ALPHA
#include "/usr/sys/include/sys/ttydev.h"
#endif
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "video.h"
#include "vlocal.h"
#include "vrawunix.h"
#include "vchinese.h"

/*
 * Globals whose storage is defined in other compilation units:
 *
 * int vb_pure:		This will be TRUE (non-zero) when the higher
 *			layers are intending to write output that should
 *			not be post processed. If this flag is FALSE. then
 *			any line-feed ('\012') character seen in the output
 *			stream will have a carriage return ('\015') character
 *			pre-pended to it.
 *
 * int vb_count:	This is the count of how many characters are currently
 *			in the output buffer. It is read by higher layers
 *			of Video for optimization purposes.
 *
 * int debugging:	This is a debugging hook flag which will cause any
 *			output data in the buffer owned by this compilation
 *			unit to be flushed to the output device immediately.
 *
 * int holding_output:	This flag is non-zero when the higher layers of
 *			Video wish to postpone flushing output waiting in the
 *			buffer until a "logical sequence" of operations is
 *			complete.
 * 
 * int v_control_data:  This flag is non-zero when the higher layers of 
 *                      video have handed us output data that should not
 *                      be processed by the translation routine, ie, control
 *                      data.  The reason is because control data (ie cursor 
 *                      position) resembles IVS characters (^[[3;3H for example)
 *                      which would be munged up by the translator
 */
extern int vb_pure;
extern int vb_count;
extern int debugging;
extern int holding_output;
extern int video_inited;

extern int v_control_data;

/*
 * Define the preprocessor variable 'TEST' to be 1 when you wish to test
 * the code in this compilation unit without invoking the rest of
 * Video.
 */
#ifdef	TEST
int	vb_count = 0;			/* Private vb_count for testing. */
int	vb_pure = TRUE;			/* Private vb_pure flag for testing. */
#endif	/* TEST */

/*
 * Video requires some "quit" and "interrupt" characters. Under Unix, "quit"
 * normally means "stop process execution now and take a process dump". 
 * "Interrupt" means "stop the current process and return to superior".
 */
#define CONTROL_C	'\003'
#define CONTROL_D	'\004'
#ifndef	VRAW_INT_CHAR
#define	VRAW_INT_CHAR	CONTROL_C
#endif
#ifndef	VRAW_EOF_CHAR
#define	VRAW_EOF_CHAR	CONTROL_D
#endif
#ifndef	VRAW_QUIT_CHAR
#define	VRAW_QUIT_CHAR	CQUIT
#endif

/*
 * The "VRAW_BUFSIZ" preprocessor token defines the size (in bytes) of the
 * output holding buffer in this layer of Video.
 */
#ifndef VRAW_BUFSIZ
#define	VRAW_BUFSIZ	4096		/* Size of internal buffer */
#endif

typedef struct termio termio_t;

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

int vcurbaud;

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


/*
 * vraw_signal:	This variable holds the action of the SIGINT (interrupt)
 *		signal previous to the execution of the vrawsigset()
 *		funcion. This is used to restore the previous interrupt
 *		action upon program exit.
 */
void	(*vraw_signal)();

static int vrawflush();
static unsigned char vrawgetc();
static int vrawintrfunc();


/*
 * The following is the local output buffer. This buffer will be flushed
 * to the terminal when a vrawprint(NULL) call is made or the buffer fills
 * during execution of vrawprint() or vrawputc().
 */
static unsigned char vraw_buffer[VRAW_BUFSIZ];

/* The following are the translation stream contexts defined in vchinese.c. 
 * They are used by the translation engine to keep track of a data stream.
 */
extern struct xlcontext *inctx, *outctx;

/* This is a scratch buff and size variable used by the routines below
 * which call xlat_read etc 
 */
static unsigned char xbuf[VRAW_BUFSIZ];
static 	int xcnt;


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
	extern int	vrawintrfunc();							/* Interrupt recovery function. 	*/
	extern termio_t prev_termio;							/* Previous terminal settings. 		*/
	extern termio_t vraw_termio;							/* "Raw" terminal settings. 		*/

	int bau;
	
	termio_t buf;									/* termio buffer 			*/
	int	error = 0;								/* Any error we might encounter 	*/

#ifdef OLD
	fflush(stdout);									/* Flush all pending output on the 	*/
	fflush(stderr);									/* stdout and stderr channels. Then grab*/
											/* the fileno for the stdout file and	*/
											/* modify its output characteristics.	*/
	ioctl(fileno(stdout), TCGETA, &buf);						/* Flush stdout. 			*/
	ioctl(fileno(stdout), TCSETAW, &buf);

	ioctl(fileno(stderr), TCGETA, &buf);						/* Flush stderr. 			*/
	ioctl(fileno(stderr), TCSETAW, &buf);

	ioctl(fileno(stdin), TCGETA, &buf);	/* NO FLUSH to allow typeahead */	/* Flush stdin. 			*/
	error = ioctl(fileno(stdin), TCSETAF, &buf);
	if (error == -1)
	{
		vraw_errno = errno;
		vre("VIDEO-E-VRAWINIT ioctl TCSETAF failed errno=%d",errno);
		return (FAILURE);
	}
#endif
	vraw_fildes = fileno(stdin);							/* Get current terminal characteristics.*/
	error = ioctl(vraw_fildes, TCGETA, &prev_termio);	 			/* These should be restored on exit. 	*/
	if (error == -1)
	{
		vre("VIDEO-E-VRAWINIT ioctl TCGETA failed on fileno(stdin)=%d errno=%d",vraw_fildes,errno);
		vraw_errno = errno;
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

	vraw_termio.c_iflag = (IXON | IXOFF | IGNBRK);
	vraw_termio.c_iflag &= ~ICRNL;	/* Don't map \r -> \n */
#ifdef	ISTRIP
	vraw_termio.c_iflag &= ~ISTRIP;
#endif

	vraw_termio.c_lflag &= ~ECHO;							/* Don't echo input. 			*/
	vraw_termio.c_lflag &= ~ICANON;							/* Non-canonical input. 		*/
	vraw_termio.c_lflag &= ~NOFLSH;							/* Allow flushes after INTR signal. 	*/

	vraw_termio.c_oflag &= ~OPOST;							/* No output processing 		*/
#ifdef OLD
	This stuff is redundent, we have already said NO OUTPUT PROCESSING

	vraw_termio.c_oflag &= ~OLCUC;
	vraw_termio.c_oflag &= ~ONLCR;							/* No \n -> \r mapping. 		*/
	vraw_termio.c_oflag &= ~OCRNL;							/* No \r -> \n mapping. 		*/
	vraw_termio.c_oflag &= ~ONOCR;
	vraw_termio.c_oflag &= ~ONLRET;							/* No \n after \r stuffing. 		*/
#endif /* OLD */

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

	error = ioctl(vraw_fildes, TCSETA, &vraw_termio);				/* We should not have to wait for any	*/
	if (error == -1)			 					/* output flushing here, as we waited 	*/
	{										/*  up above. 				*/
		vraw_errno = errno;
		vre("VIDEO-E-VRAWINIT ioctl TCSETA failed fileno=%d errno=%d",vraw_fildes,errno);

		ioctl(vraw_fildes, TCSETA, &prev_termio);				/* Reset it 				*/
		vraw_fildes = -1;
		return (FAILURE);
	}

	vraw_mode = VRAW_BLOCKED;
	vb_count = 0;										/* At start of local buffer.	*/

	vrawsigset(vrawintrfunc);
	vraw_init_flag = TRUE;									/* Mark as initialized.		*/
	video_inited = TRUE;									/* Set  - video was initialized.*/

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
 *			occured it will return NULL and vrawerror() will return
 *			a non-zero value; vseterr() is then called to pass then
 *			error up to a higher level out of the "raw" routines.
 *
 */
char vrawinput()
{
	extern int	vrawinit();
	unsigned char	the_char;
	int	error;

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())					/* If init not successful then report set error	*/
		{
			vseterr(vrawerrno());
			return(CHAR_NULL);
		}
	}

	the_char = (unsigned char) vrawgetc(TRUE);	/* Get a character and wait for it */

	if (error = vrawerrno())	/* Check for an error */
		vseterr(error);		/* There was an error so set the error number flag */

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
 * 			If an error occured then vrawerror() will return
 *			a non-zero value; vseterr() is then called to pass then
 *			error up to a higher level out of the "raw" routines.
 *
 */
char vrawcheck()
{
	extern unsigned char vrawgetc();
	extern int  vrawinit();
	unsigned char	the_char;
	int	error;

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())					/* If init not successful then report set error	*/
		{
			vseterr(vrawerrno());
			return(CHAR_NULL);
		}
	}

	the_char = (unsigned char) vrawgetc(FALSE);	/* Check for a char, don't wait */

	if (error = vrawerrno())	/* Check for an error */
		vseterr(error);		/* There was an error so set the error number flag */

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
static unsigned char vrawgetc(wait)
	int	wait;			/* IN -- TRUE means read synchronously/ */
{
	extern	termio_t vraw_termio;	/* Current termio setting for input. */
	extern	termio_t async_termio;	/* Current termio setting for input. */
	extern	int	vraw_errno;	/* Package error caching variable. */
	extern	int	vraw_fildes;	/* Module file number for IO. */
	extern  int     vraw_mode;      /* mode: blocked or nonblocked */

	int	error = 0;		/* Local error flag */
	unsigned char	ch_read = 0;		/* Character we read */

	int     elapsed_time=0;
	int     before_time, after_time;
        int     per_call_wait=0;
	
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
			error = ioctl(vraw_fildes, TCSETAW, &async_termio);
			if (error == -1)
			{
				vraw_errno = errno;
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
			error = ioctl(vraw_fildes, TCSETAW, &async_termio);
			if (error == -1)
			{
				vraw_errno = errno;
				return (CHAR_NULL);	/* No character read due to error. */
			}
			vraw_mode = VRAW_TIMEOUT;
		}
		else if (vraw_mode != VRAW_BLOCKED)
		{
			error = ioctl(vraw_fildes, TCSETAW, &vraw_termio);
			if (error == -1)
			{
				vraw_errno = errno;
				return (CHAR_NULL);	/* No character read due to error. */
			}
			vraw_mode = VRAW_BLOCKED;
		}
		if (vtimeout_value)
		{
			before_time = time(&before_time);
			do
			{
				vtimeout_clear();
				error = xlat_read(vraw_fildes, &ch_read, 1);
				if (vtimeout_check())
				{
					after_time = time(&after_time);
				}
			} while (vtimeout_check() && ((after_time-before_time)<vtimeout_value));
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
		if (vtimeout_check())
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
 *			vb_count	-- number of characters in buffer.
 *			vb_pure		-- CR stuffing flag.
 *			holding_output	-- false if buffer should be flushed 
 *					   when this call completes.
 *			debugging	-- TRUE if buffer should be flushed when
 *					   this call completes.
 *
 *	Explicit:	buf		-- pointer to buffer to be written to
 *					   terminal.
 *
 * Outputs:
 *
 *	Implicit:	vb_count	-- updated for how many characters are 
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
   static int           padidx=0, padstate=S_NORMAL, padevent, padms, abortidx;
   static double        charsper, total;
#  define PADCHAR '\0'
#ifdef VDBGPADST
   static FILE *padlog=NULL;
#endif

int vrawprint(buf)
	unsigned char	*buf;		/* IN -- pointer to the buffer. */
{
	extern	int	vrawinit();	/* Forward declaration. */
	extern	int	vrawflush();	/* Forward declaration. */
	extern	int	vb_count;	/* How many characters in buffer. */
	extern	int	vb_pure;	/* TRUE for no CR stuffing. */
	extern	int	holding_output;	/* FALSE to flush after processing. */
	extern	int	debugging;

	int	error;			/* Local error flags */
	register unsigned char *p;	/* Fast pointer */
	
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

	if (!v_control_data)            /* if this is screen data, not terminal control data */
	{
		xlat_stream(buf,strlen(buf),xbuf,&xcnt, &outctx);  /* call the xlat routine using the output stream context */
		p = (unsigned char *)xbuf;			   /* and setup the pointer */
	}
	else
	{
		if (outctx)					   /* otherwise, adjust the xlcontext to clear any pending  */
		  outctx->holdstart = outctx->holdpos;		   /* stuff that it thinks is part of a IVS sequence.       */
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

	while (*p)			/* Copy until end of string. */
	{
		register char ch;
		int padbytecnt;
		
		ch = *p++;

		if      (ch==DOLLAR)	     { padevent=E_DOL; }
		else if (ch==OPENBR)	     { padevent=E_OPEN; }
		else if (ch==CLOSBR)	     { padevent=E_CLOSE; }
		else if ((ch>='0' && ch<='9') 
			 || ch=='*')         { padevent=E_DIG; }
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
				if (vb_count + padbytecnt > VRAW_BUFSIZ)
				{
					memset(&vraw_buffer[vb_count],PADCHAR,VRAW_BUFSIZ-vb_count);
					padbytecnt -= (VRAW_BUFSIZ - vb_count);
					vb_count = VRAW_BUFSIZ;
					if (vrawflush() == FAILURE)
					{
						return FAILURE;
					}
				}
				else
				{
					memset(&vraw_buffer[vb_count],PADCHAR,padbytecnt);
					vb_count += padbytecnt;
					padbytecnt=0;
				}
			}
			padidx=0;
			break;
			
		      case S_ABORT:
			while (padidx)
			{
				if (vb_count + padidx > VRAW_BUFSIZ)
				{
					if (vrawflush() == FAILURE)
					{
						return FAILURE;
					}
				}
				else
				{
					memcpy(&vraw_buffer[vb_count],padbuf,padidx);
					vb_count += padidx;
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
	}

	/*
	 * If we are debugging or no longer holding the output buffer, then
	 * flush the buffer to the terminal now. Otherwise, simply return.
	 */
	if (debugging || !holding_output)
	{
		if (vrawflush()==FAILURE)		/* Do the output now! */
		{
			return FAILURE;
		}
	}
	return (SUCCESS);
}					/* End of vrawprint(). */
vraw_add_outbuf(ch)
unsigned char ch;
{
	if (((vraw_buffer[vb_count++] = ch) == '\012') && !vb_pure)
	{
		vraw_buffer[vb_count - 1] = '\015';
		if (vb_count >= VRAW_BUFSIZ)
		{
			if (vrawflush() == FAILURE)
			  return (FAILURE);
		}
		vraw_buffer[vb_count++] = '\012';
	}
	if (vb_count >= VRAW_BUFSIZ)
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
 *			vb_count	-- number of characters in buffer.
 *			vb_pure		-- CR stuffing flag.
 *			holding_output	-- false if buffer should be flushed 
 *					   when this call completes.
 *			debugging	-- TRUE if buffer should be flushed when
 *					   this call completes.
 *
 *	Explicit:	buf		-- pointer to buffer to be written to
 *					   terminal.
 *
 * Outputs:
 *
 *	Implicit:	vb_count	-- updated for how many characters are 
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
int vrawputc(ch)
char ch;
{
	char tmp[2];
	tmp[0]=ch;
	tmp[1]=(char)0;
	vrawprint(tmp);
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
 *			vb_count	-- the number of byte of vraw_buffer 
 *					   to write.
 *			
 *
 *	Explicit:	None.
 *
 * Outputs:
 *
 *	Implicit:	vb_count	-- set back to 0 (all output written).
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
static int vrawflush()
{
	extern	int	vraw_fildes;	/* File number to write to. */
	extern	int	vraw_errno;	/* Module error caching variable. */
	extern	int	vb_count;	/* # of chars valid in vraw_buffer. */

	int		error;		/* Local error/write flag. */
	register int	nb_written = 0;	/* Number of bytes written. */

	/*
	 * Note: While improbable, we should make sure that all the
	 * specified output was actually written before returing.
	 * Unless an error occurs, 'error' will be the number of
	 * bytes taken from the output buffer by the write() call.
	 */

#ifdef VDBGPADST
	fprintf(padlog,"flushbuf[");
	fwrite(vraw_buffer,1,vb_count,padlog);
	fprintf(padlog,"]\n");
	fflush(padlog);
#endif

	while (vb_count > 0)
	{
		error = write(vraw_fildes, &vraw_buffer[nb_written], vb_count);
		if (error == -1)
		{
			vb_count = 0;	/* We don't know what happened... */
			vraw_errno = errno;
			return (FAILURE);
		}
		vb_count -= error;	/* Account for what was written */
		nb_written += error;
	}

	return (SUCCESS);
}					/* End of vraw_flush() */

/*
 * Routine:	vrawsigset()		(Globally visible)
 *
 * Purpose:	To set the action routine for the Video "interrupt" character.
 *
 * Invocation:	success_flag = vrawsigset(func_addr);
 *
 * Inputs:
 *
 *	Implicit:	vraw_intr_func	-- The vraw layer's interrupt function 
 *					   vector.
 *
 *	Explicit:	func_addr	-- pointer to caller's interrupt 
 *					   function.
 *
 * Outputs:
 *
 *	Implicit:	None.
 *
 *	Explicit:	vraw_intr_func	-- The vraw layer's interrupt function 
 *					   vector.
 *
 *
 * Side effects:	If the function address passed is NULL, then the signal
 *			action is set to 'SIG_IGN', which will ignore the 
 *			signal.
 *
 * Returns:		SUCCESS		-- the interrupt function was set.
 *			FAILURE		-- an error occurred. The caller should
 *					   call vrawerrno() to discover why.
 */
int vrawsigset(func_addr)
	void (*func_addr)();		/* IN -- ptr to interrupt function. */
{
	extern	void	(*vraw_intr_func)();
	extern	int	vraw_errno;

	int	error;			/* Local error return. */

	if (func_addr == NULL)
		func_addr = SIG_IGN;

	error = (int)signal(SIGINT, func_addr);
	if (error == -1)
	{
		vraw_errno = errno;
		return (FAILURE);
	}
	else
	{
		vraw_signal = (void(*)())error;
	}
	return (SUCCESS);
}					/* End of vrawsigset(). */

/*
 * Routine:	vrawintrfunc()		(Module-private function)
 *
 * Purpose:	This function provides some "sane" default interrupt character
 *		handling.	
 *
 * Invocation:	success_flag = vrawintrfunc();
 *
 * Inputs:
 *
 *	Implicit:	None.
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
 * Side effects:	Those performed by vexit() and vrawexit().
 *
 * Returns:		?
 *
 */
static int vrawintrfunc()
{
	extern int	vexit();

	vexit();
	exit(0);			/* Won't return from here */
	/*NOTREACHED*/
	return (SUCCESS);
}

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
	extern termio_t prev_termio;
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
	error = ioctl(vraw_fildes, TCSETAW, &exit_termio);
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
int vraw_stty_sync() 
{
	termio_t temp_termio;

	if (!vraw_init_flag) return(0);						/* If not initialized then return		*/

	ioctl(vraw_fildes, TCGETA, &temp_termio);				/* Get current stty settings			*/
	if (0 != memcmp((char *)&temp_termio,(char
*)&vraw_termio,sizeof(vraw_termio)))		/* If stty settings are different		
*/
	{
		ioctl(vraw_fildes, TCSETAW, &vraw_termio);			/* Set to required values.			*/
		vraw_mode = VRAW_BLOCKED;
	}
	return(0);
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
int vraw_stty_save() 
{
	vraw_stty_saved = 0;
	if (-1 != ioctl(fileno(stdin), TCGETA, &save_termio))			/* Get currect stty values.			*/
	{
		vraw_stty_saved = 1;
	}
	return(0);
}
int vraw_stty_restore()
{
	if (vraw_stty_saved)
	{
		ioctl(fileno(stdin), TCSETAW, &save_termio);			/* Reset the stty to last stored value		*/
	}
	return(0);
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
int vrawerrno()
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
vshut()
{
}

vkbtimer(cnt)
int	cnt;
{
	vre_window("Warning: vkbtimer has been replaced by vtimeout");
	return;
}

/*
 * vtimeout:  set the timeout value (if any) for normal "blocked" reads
 *
 */
vtimeout(seconds)
int seconds;
{
	vtimeout_value = seconds;
	vtimeout_clear();
}
/*
 * vtimeout_clear:  clear the "timed out" status
 *
 */
vtimeout_clear()
{
	vread_timed_out_flag = FALSE;
}
/*
 * vtimeout_check: return TRUE if time out occured on last read
 *                 FALSE otherwise
 *
 */
vtimeout_check()
{
	return vread_timed_out_flag;
}
static void vtimeout_set()
{
	vread_timed_out_flag = TRUE;
}


#ifdef TEST
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
	vrawprint(NULL);
	vrawinput();
	vrawputc('\012');
	vrawprint(NULL);
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
	vrawprint(NULL);		/* Flush the output... */

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
			vrawprint(NULL);	/* Flush the output */
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
	vrawprint(NULL);
	fclose(f);

	vrawexit();
	exit(0);
}
#endif
#endif

/*
**      Routine:        xlat_read()
**
**      Function:       this acts as a front end to read which xlats incoming
**                      data
**
**      Description:    read() is called  to get data.  xlat_stream is called
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
xlat_read(fd,callerbuf,bufsize)
int fd, bufsize;
char *callerbuf;
{
	static char raw_buf[2048],xlat_buf[2048];
	static int raw_cnt,xlat_cnt=0, xlat_st=0, xlat_end=0;

	extern struct xlcontext *inctx;
	extern char vlangfile[];
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
				vtimeout_set();
				return raw_cnt;
			}
			if (raw_cnt == -1 || raw_cnt == 0)
			{
				return raw_cnt;		   /* read returns -1, return it */
			}
			                                   /* otherwise, xlat what we got and put it in the xlat buffer */
			xlat_stream(raw_buf,raw_cnt,xlat_buf,&xlat_cnt, &inctx);

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
}
