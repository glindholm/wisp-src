static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#ifdef VMS
			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include required header files.							*/

#include <stdio.h>									/* Reference standard I/O definitions.	*/
#include <descrip.h>									/* Reference VAX/VMS descriptor header.	*/
#include <iodef.h>									/* Reference VAX/VMS I/O defintions.	*/
#include <ssdef.h>									/* Reference VAX/VMS System Status.	*/
#include <syidef.h>									/* Reference VAX/VMS System wide info.	*/
#include "video.h"									/* Reference video definitions.		*/

extern int video_inited;								/* Flag if video has been initialized.	*/

/*						Local definitions.								*/

#define NUMBER_OF_BUFFERS 2								/* Number of buffers to use.		*/
#define MAX_BUFFER_SIZE 4096								/* Maximum buffer size.			*/

/*						Static data definitions.							*/

static $DESCRIPTOR(ttname,"SYS$OUTPUT");						/* Create descriptor for TT: device.	*/
static int ttchan;									/* Allocate control data storage.	*/
static short iosb[NUMBER_OF_BUFFERS][4];						/* Allocate IO status blocks.		*/
static long  ioefn[NUMBER_OF_BUFFERS];							/* Allocate event flag storage.		*/
static short syiret;									/* A place to put the return length.	*/

static struct {	short syi_len;								/* The length of the buffer for GETSYI.	*/
		short syi_code;								/* The item we want.			*/
		int *syi_buf;								/* The address for the answer.		*/
		short *syi_ret;								/* The address for the return length.	*/
	      } syi_desc;

static struct two_longs {long t1; long t2;};						/* The I/O status block.		*/
static char *outbuf;									/* Pointer to current output buffer.	*/
static char *bufbase;									/* Pointer to the base of buffer area.	*/
static int bufsize = 0;									/* Size of each buffer area.		*/
extern int vb_count;									/* Count of characters in a buffer.	*/
static int selection = 0;								/* Buffer to be used.			*/
static int wait_sum = 0;								/* Count times program has waited.	*/
static int first = TRUE;								/* First time flag.			*/
static int zeroes = 0;									/* Number of zero outputs.		*/
static int total = 0;									/* Total buffer size.			*/
static int number = 0;									/* Number of times doing output.	*/
static int average = 0;									/* Average size of output.		*/
static int maxout = 0;									/* Maximum size of output.		*/
static int minout = 0;									/* Minimum size of output.		*/

/*						Subroutine entry point.								*/

int vrawflush(void)
{
	return vrawprint(NULL);
}

int vrawprint(char* a_line)								/* String passed by reference.		*/
{
	register int i;									/* Working registers.			*/
	register char a_char;								/* Working character storage.		*/
	extern int vb_pure;								/* flag to control pure lf or new line.	*/
	extern int debugging;								/* Debugging control flag reference.	*/

	if (first)
	{
		initialize();								/* First time in this routine?		*/
		video_inited = TRUE;							/* Set because video is initialized.	*/
	}
	if (a_line == NULL) return(start_output());					/* Does he want buffer dumped?		*/

	if (vb_count == 0) wait_for_buffer();						/* At start, wait for buff to be free.	*/

	while (a_char = *(a_line++))							/* Get the next character.		*/
	{
		if (((outbuf[vb_count++] = a_char) == '\n') && !vb_pure)		/* Is this a \012 in pure state?	*/
		{
			outbuf[vb_count-1] = '\015';					/* Replace LF with CR.			*/
			check_end();							/* Check if at end of this buffer.	*/
			outbuf[vb_count++] = '\012';					/* Put in line-feed.			*/
		}
		check_end();								/* See if buffer is full.		*/
	}

	if (debugging || !vbuffering_on()) start_output();				/* Start the output as appropriate.	*/

	return(1);									/* Return with unconditional success.	*/
}

/*					Subroutine to actually write the buffer.						*/

static start_output()
{
	long unsigned status;								/* System call status long-word.	*/

	if (vb_count == 0)								/* Anything to output.			*/
	{
		zeroes = zeroes + 1;							/* Count this attempt.			*/
		return(1);								/* Don't bother if nothing to print.	*/
	}
	if (minout == 0) minout = vb_count;						/* Track if this is the smallest.	*/
	else if (minout > vb_count) minout = vb_count;
	if (maxout < vb_count) maxout = vb_count;					/* Track if this is the largest.	*/
	total = total + vb_count;							/* Calculate the total bytes output.	*/
	number = number +1;								/* Calculate the number of outputs.	*/
	average = total/number;								/* Calculate the average.		*/

	iosb[selection][0] = 0;								/* Clear the status word.		*/

	lib$get_ef(&ioefn[selection]);							/* Get an event flag.			*/
	if (ioefn[selection] == -1) ioefn[selection] = 0;				/* No flag availiable, use flag 0. This	*/
											/* will work, but may be sharing the	*/
											/* flag with some other event.		*/
											/* Start the output.			*/
	status = sys$qio(ioefn[selection],ttchan,IO$_WRITEVBLK | IO$M_NOFORMAT,iosb[selection],0,0,outbuf,vb_count,0,0,0,0);

	if (status != SS$_NORMAL)
	{										/* Report errors.			*/
		outbuf[vb_count] = '\0';
		printf("\n\007Internal error in QIO in v_output(), status is %x (hex).     \nData is : %s",status,outbuf);
		printf("\nProgram aborting...\n");
		lib$signal(status);							/* Signal to get a traceback.		*/
		exit(0);								/* Abort the program.			*/
	}

	vb_count = 0;									/* Reset the character count.		*/
	selection++;									/* Move to the next buffer.		*/
	if (selection >= NUMBER_OF_BUFFERS) selection = 0;				/* Wrap around if necessary.		*/
	outbuf = &bufbase[selection*bufsize];						/* Point to the newly selected buffer.	*/

	return(1);									/* Unconditional success.		*/
}


/*					Subroutine to wait for buffer to become free.						*/

static wait_for_buffer()								/* Loop until status says I/O done.	*/
{
	if (!iosb[selection][0])							/* It didn't complete, so wait for it.	*/
	{
		wait_sum++;								/* Record how many times we've waited.	*/
		sys$synch(ioefn[selection],iosb[selection]);				/* Not completed so synchronize on the 	*/
	}										/*   event flag. NOTE: If EFN is zero, 	*/
											/*   this will still work, but we can	*/
											/*   not free flag zero.		*/
	if (ioefn[selection])
	{
		lib$free_ef(&ioefn[selection]);						/* If there was an event flag, free it	*/
		ioefn[selection] = 0;							/* Clear the reference to it.		*/
	}

	return(1);
}

/*				Be sure each pending QIO has ended and the event flags have been freed.				*/

vrawexit()
{
	register int i;									/* Working counter.			*/

	for (i=0; i<NUMBER_OF_BUFFERS; i++)						/* Examine each buffer			*/
	{
		wait_for_buffer();							/* See if it is busy.			*/
		selection++;								/* Move to the next buffer.		*/
		if (selection >= NUMBER_OF_BUFFERS) selection = 0;			/* Wrap around if necessary.		*/
	}
}


/*				Perform first time initialization.								*/

static initialize()
{
	long unsigned status;								/* Status word.				*/
	char *malloc();									/* Reference memory allocation routine.	*/
	register int i;									/* Working loop counter.		*/

	sys$assign(&ttname,&ttchan,0,0);						/* Yes, so assign a channel.		*/

	syi_desc.syi_len = 4;								/* The length of the buffer for GETSYI.	*/
	syi_desc.syi_code = SYI$_MAXBUF;						/* The item we want.			*/
	syi_desc.syi_buf = &bufsize;							/* The address for the answer.		*/
	syi_desc.syi_ret = &syiret;							/* The address for the return length.	*/

	status = sys$getsyiw(0,0,0,&syi_desc,0,0,0);					/* Get the info.			*/

	if (status != SS$_NORMAL)							/* Is the system information valid?	*/
	{
		printf("\n\007Internal error requesting MAXBUF in v_output(), status is %x hex.\n",status);
		printf("Program aborting...\n");
		exit(0);								/* Abort the program.			*/
	}

	bufsize = bufsize - 128;							/* Leave an overflow area.		*/
	if (bufsize > MAX_BUFFER_SIZE) bufsize = MAX_BUFFER_SIZE;			/* Don't use more than we really need.	*/
	if (!(bufbase = malloc(bufsize*NUMBER_OF_BUFFERS)))				/* Attempt to get buffer area.		*/
	{
		printf("\n\007Internal error creating buffer area in v_output(), malloc() size was 0.\n");
		printf("Program aborting...\n");
		exit(0);								/* Abort abort abort.			*/
	}

	outbuf = bufbase;								/* Point to buffer #0			*/
	selection = 0;									/* Remember which one it is		*/
	vb_count = 0;									/* Start at the start of the buffer.	*/

	for (i = 0; i < NUMBER_OF_BUFFERS; i++) iosb[i][0] = SS$_NORMAL;		/* Clear the I/O status flags.		*/

	first = FALSE;									/* No longer the first time.		*/
	return(1);									/* Unconditional success (or exit).	*/
}

/*					Check if at end of buffer area.								*/

static check_end()
{
	if (vb_count >= bufsize)							/* Is buffer full?			*/
	{
		start_output();								/* Start output on the current chan.	*/
		wait_for_buffer();							/* Wait for next buffer to be free.	*/
	}
}

/*					Output a single character.								*/

int vrawputc(c) char c;									/* Output character c.			*/
{
	char string[2];									/* Working string.			*/
	string[0] = c;									/* Enter the character into the string.	*/
	string[1] = 0;									/* Terminate with a null.		*/
	vrawprint(string);								/* Output the string.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

/*					Subroutine to synchronize with QIO's and timers.					*/

void vr_sync(in_efn,in_iosb) long in_efn;						/* Event flag number.			*/
struct two_longs *in_iosb;								/* The I/O status block.		*/
{
	sys$synch(in_efn,in_iosb);							/* Wait for the QIO.			*/
}

unsigned v_pfkey_ef = -1;								/* The pfkey_ef for video/message.	*/

signal_msg()										/* Signal to MESSAGE.			*/
{
	if (v_pfkey_ef != -1)
	{
		sys$setef(v_pfkey_ef);							/* Set the pfkey event flag.		*/
	}
}

#endif

/*
**	History:
**	$Log: vrawvms2.c,v $
**	Revision 1.11  1996-11-13 20:44:25-05  gsl
**	Added vrawflush()
**
**	Revision 1.10  1996-07-26 09:23:24-07  gsl
**	Remove unsigned from vrawprint
**
**	Revision 1.9  1996-03-12 05:33:42-08  gsl
**	chnage to use vbuffering_on()
**
**
**
*/
