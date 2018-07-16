static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
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

main()
{
	unsigned char ch;
	void cleanexit();
	char *makeprintable();
	char *oct();
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
}
char *makeprintable(value)
unsigned char value;
{
	static char buf[4];
	
	memset(buf,0,sizeof(buf));
	if (value<' ')
	  sprintf(buf,"'^%c'",value+64);
	else sprintf(buf,"'%c'",value);
	return buf;
}
char *oct(value)
unsigned char value;
{
	static char buf[4];
	
	memset(buf,0,sizeof(buf));

	buf[0]= ((unsigned char)(value & 0700) >> 6) + 0x30;
	buf[1]= ((unsigned char)(value & 070)  >> 3) + 0x30;
	buf[2]= ((unsigned char)(value & 07)       ) + 0x30;
	return buf;
}
rawmode()
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
void cleanexit(sig)
int sig;
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
**	Revision 1.9  1996-07-26 18:14:16-04  gsl
**	Fix includes for termio.h and termios.h
**
**	Revision 1.8  1996-07-23 11:13:02-07  gsl
**	drcs update
**
**
**
*/
