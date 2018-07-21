/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
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
**
** $Author: gsl $
**
**
******************************************************************************
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef USEIOCTL
#include <termio.h>
struct termio old,new;
#else
#include <termios.h>
struct termios old,new;
#endif

static char *makeprintable(unsigned char value);
static char *oct(unsigned char value);
static void rawmode();
static void cleanexit(int sig);


int main()
{
	unsigned char ch;
	int	exit_flag;

#ifdef TESTING	
sleep(3);
#endif

	signal(SIGQUIT,cleanexit);
	signal(SIGINT,cleanexit);

	printf("\nPress <Enter> <Enter> to exit\n\n");
	
	rawmode();
	exit_flag = 0;
	for(;;)
	{
		read(0,&ch,1);
		printf("Character %4.4s: octal %s, decimal %d, hex %02x\n",
		       makeprintable(ch),oct(ch),ch,ch);
		if (ch == 0x0d)
		{
			if (exit_flag) break;
			else exit_flag=1;
		}
		else
		{
			exit_flag =0;
		}
	}
	cleanexit(0);
	return 0;
}
static char *makeprintable(unsigned char value)
{
	static char buf[4];
	
	memset(buf,0,sizeof(buf));
	if (value<' ')
	  sprintf(buf,"'^%c'",value+64);
	else sprintf(buf,"'%c'",value);
	return buf;
}
static char *oct(unsigned char value)
{
	static char buf[4];
	
	memset(buf,0,sizeof(buf));

	buf[0]= ((unsigned char)(value & 0700) >> 6) + 0x30;
	buf[1]= ((unsigned char)(value & 070)  >> 3) + 0x30;
	buf[2]= ((unsigned char)(value & 07)       ) + 0x30;
	return buf;
}
static void rawmode()
{
#ifdef USEIOCTL
	ioctl(0,TCGETA,&old);
#else
	tcgetattr(0,&old);
#endif
	
	new=old;
	new.c_cc[VMIN]=1;
	new.c_cc[VTIME]=0;
	new.c_lflag &= ~(ICANON|ECHO);
	
	new.c_iflag = 0;

#ifdef USEIOCTL
	ioctl(0,TCSETAW,&new);
#else
	tcsetattr(0,TCSADRAIN,&new);
#endif
	
}
static void cleanexit(int sig)
{
#ifdef USEIOCTL
	ioctl(0,TCSETAW,&old);
#else
	tcsetattr(0,TCSANOW,&old);
#endif

	exit(0);
}

	     

/*
**	History:
**	$Log: viewkey.c,v $
**	Revision 1.13  2003/03/12 18:18:11  gsl
**	FIx -Wall warnings
**	
**	Revision 1.12  2003/02/04 21:05:36  gsl
**	fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.10  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.9  1996/07/26 22:14:16  gsl
**	Fix includes for termio.h and termios.h
**	
**	Revision 1.8  1996-07-23 11:13:02-07  gsl
**	drcs update
**
**
**
*/
