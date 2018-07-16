			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <termio.h>
#include <signal.h>

struct termio old,new;

main()
{
	unsigned char ch;
	void cleanexit();
	char *makeprintable();
	char *oct();
	int	exit_flag;
	
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
	ioctl(0,TCGETA,&old);
	new=old;
	new.c_cc[VMIN]=1;
	new.c_cc[VTIME]=0;
	new.c_lflag &= ~(ICANON|ECHO);
	new.c_iflag = 0;
	ioctl(0,TCSETA,&new);
}
void cleanexit(sig)
int sig;
{
	ioctl(0,TCSETA,&old);
	exit(0);
}

	     

