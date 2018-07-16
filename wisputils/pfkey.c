/* insert IDSI copyright notice here	*/

/* pfkey.c : get a AT&T 605 function key F1-F16 */

#include <stdio.h> 
#include <termio.h>
#include <signal.h>

struct termio old,new;

main()
{
	int key;
	int fixup();

	signal(SIGINT,fixup);							/* handle premature exit			*/
	signal(SIGQUIT,fixup);

	ioctl(0,TCGETA,&old);							/* get a save copy				*/
	ioctl(0,TCGETA,&new);							/* and a copy to modify				*/
	new.c_lflag &= ~ICANON;							/* turn of canonical processing			*/
	new.c_lflag &= ~ECHO;							/* and echo					*/
	new.c_cc[VMIN]=1;							/* mininum chars to satisfy a read is 1		*/
	new.c_cc[VTIME]=0;							/* timeout not used here, so 0			*/
	ioctl(0,TCSETA,&new);							/* mod the tty					*/

	key=getpfkey();								/* get a key					*/
	printf("%d",key);							/* and supply it to caller			*/

	fixup();								/* restore					*/
}
getpfkey()
{
	char buf[10];
	int ch,cnt=0;
	int i;

	buf[cnt++]=ch=getchar();						/* get first char				*/
	if (ch!=27)								/* is it ESC?					*/
	{
		do								/* no, user must be typing the actual number	*/
		{
			buf[cnt++]=ch=getchar();				/* get the rest of the number			*/

		} while (ch!='\n');						/* till he presses return			*/
		ch=atoi(buf);							/* convert to int				*/
	}
	else
	{
		buf[cnt++]=ch=getchar();					/* else process second char			*/
		if (ch=='[')
		{
			buf[cnt++]=ch=getchar();
			if (ch=='H') ch=15;
			else
			{
				for (i=0; i<4; i++) buf[cnt++]=ch=getchar();
				if (memcmp(buf,"\033[24;1H",7)==0) ch=16;
				else ch=0;
			}
		}
		else
		{
			buf[cnt++]=getchar();
			if (buf[1]=='O') ch=buf[2]-'b';
			else if (buf[1]=='N') ch=buf[2]-'f';
		}
	}
	if (ch<0 || ch>16) ch=0;
	return ch;
}
fixup()
{
	ioctl(0,TCSETA,&old);
	exit();
}
