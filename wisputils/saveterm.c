/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#include <termio.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

typedef struct termio termio_t;
static termio_t prev_termio;							/* The saved termio			*/

static int fileid = -1;								/* File channel to fileno(stdin). 	*/

saveterm()
{
	int	error;

	fileid = fileno(stdin);							/* Get current terminal characteristics.*/
	error = ioctl(fileid, TCGETA, &prev_termio);	 			/* These should be restored on exit. 	*/
	if (error == -1)
	{
		fileid = -1;
	}
}

restoreterm()
{
	if (fileid == -1 ) return;
	ioctl(fileid, TCSETAW, &prev_termio);
}


#ifdef MAIN_SAVETERM
main()
{
	saveterm();
	printf("turn terminal to raw\n\n press any key :");
	vrawinput();
	restoreterm();
}
#endif

/*
**	History:
**	$Log: saveterm.c,v $
**	Revision 1.8  1996/07/23 18:24:58  gsl
**	Fix redefinition
**	
**	Revision 1.7  1996-07-23 11:13:00-07  gsl
**	drcs update
**
**
**
*/
