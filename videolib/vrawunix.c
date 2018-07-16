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
 */
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#include <termio.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "video.h"
#include "vlocal.h"
#include "vrawunix.h"

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
 */
extern int vb_pure;
extern int vb_count;
extern int debugging;
extern int holding_output;
extern int video_inited;

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
#ifndef	VRAW_INT_CHAR
#define	VRAW_INT_CHAR	'\003'
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
 * vtimer:      Holds timeout value for nonblocking char input
 *
 */
static int vtimer = 0;

/*
 * vraw_signal:	This variable holds the action of the SIGINT (interrupt)
 *		signal previous to the execution of the vrawsigset()
 *		funcion. This is used to restore the previous interrupt
 *		action upon program exit.
 */
void	(*vraw_signal)();


/*
 * The following is the local output buffer. This buffer will be flushed
 * to the terminal when a vrawprint(NULL) call is made or the buffer fills
 * during execution of vrawprint() or vrawputc().
 */
static unsigned char vraw_buffer[VRAW_BUFSIZ];

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
	extern unsigned char vraw_buffer[];						/* Internal buffer. 			*/

	termio_t buf;									/* termio buffer 			*/
	int	error = 0;								/* Any error we might encounter 	*/

	fflush(stdout);									/* Flush all pending output on the 	*/
	fflush(stderr);									/* stdout and stderr channels. Then grab*/
											/* the fileno for the stdout file and	*/
											/* modify its output characteristics.	*/
	error = ioctl(fileno(stdout), TCGETA, &buf);					/* Flush stdout. 			*/
	error = ioctl(fileno(stdout), TCSETAW, &buf);
#ifdef OLD
	if (error == -1)
	{
		vraw_errno = errno;
		return (FAILURE);
	}
#endif
	error = ioctl(fileno(stderr), TCGETA, &buf);					/* Flush stderr. 			*/
	error = ioctl(fileno(stderr), TCSETAW, &buf);
#ifdef OLD
	if (error == -1)
	{
		vraw_errno = errno;
		return (FAILURE);
	}
#endif
	error = ioctl(fileno(stdin), TCGETA, &buf);					/* Flush stdin. 			*/
	error = ioctl(fileno(stdin), TCSETAF, &buf);
	if (error == -1)
	{
		vraw_errno = errno;
		vre("VIDEO-E-VRAWINIT ioctl TCSETAF failed errno=%d",errno);
		return (FAILURE);
	}
	vraw_fildes = fileno(stdin);							/* Get current terminal characteristics.*/
	error = ioctl(vraw_fildes, TCGETA, &prev_termio);	 			/* These should be restored on exit. 	*/
	if (error == -1)
	{
		vraw_errno = errno;
		vraw_fildes = -1;							/* Re-init this 			*/
		vre("VIDEO-E-VRAWINIT ioctl TCGETA failed errno=%d",errno);
		return (FAILURE);
	}										/* Copy the old termio buffers to the	*/
											/* ones we will use to set the desired	*/
	memcpy(&vraw_termio, &prev_termio, sizeof(vraw_termio));			/* characteristics.			*/

	vraw_termio.c_iflag = (IXON | IXOFF | IGNBRK);
	vraw_termio.c_iflag &= ~ICRNL;	/* Don't map \r -> \n */
#ifdef	ISTRIP
	vraw_termio.c_iflag &= ~ISTRIP;
#endif
	vraw_termio.c_lflag &= ~ECHO;							/* Don't echo input. 			*/
	vraw_termio.c_lflag &= ~ICANON;							/* Non-canonical input. 		*/
#ifdef OLD
	vraw_termio.c_lflag |= ISIG;							/* Leave signals enabled. 		*/
#endif
	vraw_termio.c_lflag &= ~NOFLSH;							/* Allow flushes after INTR signal. 	*/

	vraw_termio.c_oflag &= ~OPOST;							/* No output processing 		*/
	vraw_termio.c_oflag &= ~OLCUC;
	vraw_termio.c_oflag &= ~ONLCR;							/* No \n -> \r mapping. 		*/
	vraw_termio.c_oflag &= ~OCRNL;							/* No \r -> \n mapping. 		*/
	vraw_termio.c_oflag &= ~ONOCR;
	vraw_termio.c_oflag &= ~ONLRET;							/* No \n after \r stuffing. 		*/

#ifdef	NO_TAB
	vraw_termio.c_oflag |= TAB3; 							/* Expand tabs to spaces 		*/
#endif
#ifdef	EIGHT_BIT
	vraw_termio.c_cflag |= CS8;							/* Allow 8-bit input 			*/
	vraw_termio.c_cflag &= ~PARENB;
#endif

	/*
	 * Set up the default kill characters.
	 * We use the preprocessor constant INT_CHAR for interrupts and
	 * QUIT_CHAR for the QUIT character.
	 * For the special characters, (SWTCH) we
	 * init these to something fairly off-beat.
	 * Also, tell the tty device that we want one
	 * character at a time, no matter how long it
	 * takes. (Blocking reads).
	 */
	vraw_termio.c_cc[VINTR]	 = VRAW_INT_CHAR;
	vraw_termio.c_cc[VQUIT]	 = VRAW_QUIT_CHAR;
#ifndef _AIX
	vraw_termio.c_cc[VSWTCH] = 0377;						/* Disable shell switch 		*/
#endif
	vraw_termio.c_cc[VMIN]	 = sizeof(char);
	vraw_termio.c_cc[VTIME]	 = 0;							/* Wait forever for 1 char 		*/

	error = ioctl(vraw_fildes, TCSETA, &vraw_termio);				/* We should not have to wait for any	*/
	if (error == -1)			 					/* output flushing here, as we waited 	*/
	{										/*  up above. 				*/
		vraw_errno = errno;
		vre("VIDEO-E-VRAWINIT ioctl TCSETA failed errno=%d",errno);

		ioctl(vraw_fildes, TCSETA, &prev_termio);				/* Reset it 				*/
		vraw_fildes = -1;
		return (FAILURE);
	}

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
	extern char	vrawgetc();
	extern int	vrawinit();
	char	the_char;
	int	error;

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())					/* If init not successful then report set error	*/
		{
			vseterr(vrawerrno());
			return(CHAR_NULL);
		}
	}

	the_char = vrawgetc(TRUE);	/* Get a character and wait for it */

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
	extern char vrawgetc();
	extern int  vrawinit();
	char	the_char;
	int	error;

	if (!vraw_init_flag)
	{
		if (SUCCESS != vrawinit())					/* If init not successful then report set error	*/
		{
			vseterr(vrawerrno());
			return(CHAR_NULL);
		}
	}

	the_char = vrawgetc(FALSE);	/* Check for a char, don't wait */

	if (error = vrawerrno())	/* Check for an error */
		vseterr(error);		/* There was an error so set the error number flag */

	return (the_char);
}					/* end of vrawcheck() */


static char v_read_alarm_signal;
/*
** Routine: vrawreadalarm()
**
** Purpose:	To set a flag that says the ALARM signal was recieved.
*/
static void vrawreadalarm(sig)
int sig;
{
	v_read_alarm_signal = (char) 1;	
}

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
static char vrawgetc(wait)
	int	wait;			/* IN -- TRUE means read synchronously/ */
{
	extern	termio_t vraw_termio;	/* Current termio setting for input. */
	extern	int	vraw_errno;	/* Package error caching variable. */
	extern	int	vraw_fildes;	/* Module file number for IO. */

	int	error = 0;		/* Local error flag */
	char	ch_read = 0;		/* Character we read */
	termio_t async_termio;		/* In case we need to sync up */

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

	if (!wait)
	{
		memcpy(&async_termio, &vraw_termio, sizeof(async_termio));
		async_termio.c_cc[VMIN] = 0;
		error = ioctl(vraw_fildes, TCSETAW, &async_termio);
		if (error == -1)
		{
			vraw_errno = errno;
			return (CHAR_NULL);	/* No character read due to error. */
		}

		/*
		** There is some sort of a problem with this code and ACUCOBOL, namely the "read" doesn't return while setting
		** up the terminal. So we added an alarm that will signal if the read doesn't return. If this happens we return
		** a CHAR_NULL as the character.
		*/
#ifdef ultrix
		v_read_alarm_signal = CHAR_NULL;						/* Turn off alarm flag		*/
 		signal( SIGALRM, vrawreadalarm );     						/* Set signal to catch alarm	*/
		alarm(vtimer?vtimer:1);								/* Set a one second alarm	*/
#else
		if (vtimer)
		{
			v_read_alarm_signal = CHAR_NULL;					/* Turn off alarm flag		*/
			signal( SIGALRM, vrawreadalarm );					/* Set signal to catch alarm	*/
			alarm(vtimer);								/* Set a one second alarm	*/
		}
		
#endif

		error = read(vraw_fildes, &ch_read, 1);

#ifdef ultrix
		alarm(0);									/* Cancel the alarm		*/
 		signal( SIGALRM, SIG_DFL );							/* reset the signal		*/
		if (v_read_alarm_signal) ch_read = CHAR_NULL;					/* If alarm flag set then alarm	*/
												/* went off so return CHAR_NULL	*/
#else
		if (vtimer)
		{
			alarm(0);								/* Cancel the alarm		*/
			signal( SIGALRM, SIG_DFL );						/* reset the signal		*/
			if (v_read_alarm_signal) ch_read = CHAR_NULL;				/* If alarm flag set then alarm	*/
		}										/* went off so return CHAR_NULL	*/
#endif

		/*
		 * Reset the previous terminal settings.
		 */
		error = ioctl(vraw_fildes, TCSETAW, &vraw_termio);
		if (error == -1)
		{
			vraw_errno = errno;
			return (CHAR_NULL);	/* No character read due to error. */
		}
	}
	else				/* Blocking read for one character. */
	{
		error = read(vraw_fildes, &ch_read, 1);
		if (error == -1)
		{
			vraw_errno = errno;
			return (CHAR_NULL);	/* No character read due to error. */
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
int vrawprint(buf)
	unsigned char	*buf;		/* IN -- pointer to the buffer. */
{
	extern	int	vrawinit();	/* Forward declaration. */
	extern	int	vrawflush();	/* Forward declaration. */
	extern	unsigned char vraw_buffer[];
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
			vseterr(vrawerrno());
			return(FAILURE);
		}
	}

	if (buf == NULL)		/* NIL buffer? */
	{
		return (vrawflush());	/* Yes, so flush the buffer. */
	}

	p = (unsigned char *)buf;
	while (*p)			/* Copy until end of string. */
	{
		if (((vraw_buffer[vb_count++] = *p++) == '\012') && !vb_pure)
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
	}

	/*
	 * If we are debugging or no longer holding the output buffer, then
	 * flush the buffer to the terminal now. Otherwise, simply return.
	 */
	if (debugging || !holding_output)
		vrawflush();		/* Do the output now! */

	return (SUCCESS);
}					/* End of vrawprint(). */

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
	extern	int	vrawinit();	/* Forward declaration. */
	extern	int	vrawflush();	/* Forward declaration. */
	extern	unsigned char vraw_buffer[];
	extern	int	vb_count;	/* # of characters in buffer. */
	extern	int	vb_pure;	/* TRUE for no CR stuffing. */
	extern	int	holding_output;	/* FALSE to flush after processing. */
	extern	int	debugging;	
 
	int	error = SUCCESS;


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

	/*
	 * If we are debugging or no longer holding the output buffer, then
	 * flush the buffer to the terminal now. Otherwise, simply return.
	 */
	if (debugging || !holding_output)
		vrawflush();		/* Start output now! */
	return (SUCCESS);
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
	extern	unsigned char vraw_buffer[]; /* Output buffer. */

	int		error;		/* Local error/write flag. */
	register int	nb_written = 0;	/* Number of bytes written. */


	/*
	 * Note: While improbable, we should make sure that all the
	 * specified output was actually written before returing.
	 * Unless an error occurs, 'error' will be the number of
	 * bytes taken from the output buffer by the write() call.
	 */
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
	extern termio_t vraw_termio;
	extern int vraw_fildes;

	int error = 0;

	vrawflush();									/* Flush any remaining output. 		*/
	error = ioctl(vraw_fildes, TCSETAW, &prev_termio);
	vraw_fildes = -1;								/* Re-init the file channels.		*/
	vraw_init_flag = 0;								/* No longer initialized.		*/
	return ((error == -1) ? FAILURE : SUCCESS);
}											/* end of vrawexit().			*/
/* functions vraw_stty_set() and vraw_stty_reset() are used
 * to place the tty into video's desired mode and reset it to
 * the previous mode respectively.  Needed for MicroFocus animator
 * which conflicts with video by mucking with the settings.  VWANG
 * calls  vraw_stty_set() at the beginning of processing to insure the
 * proper mode, then calls reset before exiting so the animator gets
 * the mode he expects also...
 */
int vraw_stty_set() 
{
	if (vraw_init_flag)
	  ioctl(vraw_fildes, TCSETA, &vraw_termio);
}
int vraw_stty_reset()
{
	if (vraw_init_flag)
	  ioctl(vraw_fildes, TCSETAW, &prev_termio);
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
	vtimer=cnt;
	return;
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
