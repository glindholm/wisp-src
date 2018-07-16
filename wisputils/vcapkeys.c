/************************************************************************/
/*									*/
/*	        WISP - Wang Interchange Source Pre-processor		*/
/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/*									*/
/************************************************************************/

/*
	vcapkeys:	Utility that prompts user to press PFKEYs and 
			records ESC sequence in videocap files.
*/

#define NAME	"vcapkeys"

#include <stdio.h>
#include <sys/types.h>
#include <termio.h>
#include <signal.h>


#if defined(_AIX) || defined(SEQUENT)
#include <sys/select.h>
#include <time.h>
#endif


#ifdef SCO						/* SCO unix only; this used to be i386, but conflicted with NCR.	*/
#include <sys/select.h>
#include <sys/times.h>
#define bzero(x,y) memset(x,0,y) 
#else
#include <sys/time.h>
#endif

char *getenv();

static char *makeprintable();

#define GEN_ENTER		 0
#define GEN_UP			33
#define GEN_DOWN		34
#define GEN_RIGHT		35
#define GEN_LEFT		36
#define GEN_HELP		37
#define GEN_TAB			38
#define GEN_BACKTAB		39
#define GEN_DELETE		40
#define GEN_REMOVE		41
#define GEN_INSERT		42
#define GEN_NEWLINE		43
#define GEN_CLEAR_AFTER		44
#define GEN_CLEAR_FIELD		45
#define TABLE_SIZE		46

#define MAX_ESC_LEN		50

struct
{
	char	seq_esc[MAX_ESC_LEN];
	char	seq_mess[50];
	char	seq_dvalue[50];
	char	seq_name[25];
} et[TABLE_SIZE];

static int load101();
static int getkey();
static int prompt();
static int ynprompt();
static int getstr_to();
static int writevcap();
static int init_table();
static int promptkey();
static int dupvalue();

main(argc,argv)
int	argc;
char	*argv[];
{
	int	i, len, rc;
	char	buff[256];
	char	message[50];
	char	filename[128];
	char	dvalue[50];
	int	k101, backup, review;
	char	*ptr;

	      /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	printf("**** Define videocap keys ****\n\n");
	printf("This program will assist you in creating a videocap file. It will prompt you to\n");
	printf("press \"Function Keys\", then store the escape sequences into a file which can\n");
	printf("be combined with \"Capablities\" to create a videocap file.  See appendix D of\n");
	printf("the WISP User's Guide for a complete description of videocap files.\n\n");

restart_label:
	k101 = 0;
	backup = 0;
	review = 0;

	init_table();								/* Initialize the table				*/

	ptr = getenv("WISPTERM");
	if (!ptr) 
	{
		ptr = getenv("TERM");
		if (!ptr)
			ptr = "vcap";
	}
	strcpy(filename,ptr);							/* Set default filename				*/
	strcat(filename,".keys");

review_label:
	strcpy(buff,filename);
	prompt("Enter the name of videocap key file",buff,filename);
	
	if ( !review && ynprompt("Is this a 101 keyboard with 12 PFkeys",1))
	{
		if ( ynprompt("Use ESC+PF for 11-20, ESC+ESC+PF for 21-32",1))
			k101=1;
	}

	printf("\nPress each of the keys as you are prompted. There is a 3/4 second delay\n");
	printf("after each key -- don't press the next key until prompted.\n");
	printf("Pressing <Enter> will give you the default value shown in brackets.\n");
	printf("Press the q key to Quit, the b key to Backup, the space key for more options.\n\n"); 

	for(i=0;i<TABLE_SIZE;)							/* Loop getting all the keys			*/
	{
		if (i==13 && k101 && !review)
		{
			load101();						/* Auto load keys 13 thru 32			*/
			i = 33;
		}

		if (et[i].seq_esc[0])						/* Set the default value			*/
			strcpy(dvalue,et[i].seq_esc);
		else
			strcpy(dvalue,et[i].seq_dvalue);

		rc = getkey(et[i].seq_mess,dvalue,buff,i,&backup);		/* Get the key					*/

		if (rc==1)
		{
			goto restart_label;					/* Restart					*/
		}
		if (rc==2)
		{
			break;							/* Exit						*/
		}

		if (backup)							/* Backup and re-prompt for previous		*/
		{
			i--;
			if (i< 0) i = 0;
			backup=0;
		}
		else
		{
			strcpy(et[i].seq_esc,buff);				/* Save the escape sequence			*/
			i++;
		}
	}

	for(;;)
	{
		sprintf(buff,"Do you want to write file %s?",filename);
		if (ynprompt(buff,1))
		{
			if (writevcap(filename))
			{
				strcpy(buff,filename);
				prompt("Enter the NEW name of videocap key file",buff,filename);
				continue;
			}
			break;
		}

		if (ynprompt("Do you want to Quit Immediately?",1))
		{
			exit(0);
		}

		if (ynprompt("Do you want to review/change your responses?",1))
		{
			review = 1;
			goto review_label;
		}

		if (ynprompt("Do you want to restart?",1))
		{
			goto restart_label;
		}
	}

	exit(0);
}

static load101()								/* Load 13 thru 32 based on 1-12		*/
{
	int	i;
	char	buff[80];

	for(i=21;i<=32;i++)
	{
		sprintf(et[i].seq_esc,"\"\\E\\E%s",&et[i-20].seq_esc[1]);
	}
	sprintf(buff,"%s|\"\\E%s",et[11].seq_esc, &et[1].seq_esc[1]);		/* Doubly define PF11 and PF12			*/
	strcpy(et[11].seq_esc,buff);
	sprintf(buff,"%s|\"\\E%s",et[12].seq_esc, &et[2].seq_esc[1]);
	strcpy(et[12].seq_esc,buff);
	for(i=13;i<=20;i++)
	{
		sprintf(et[i].seq_esc,"\"\\E%s",&et[i-10].seq_esc[1]);
	}

	for(i=13;i<=32;i++)							/* Printout the results				*/
	{
		printf("101-Keyboard PF%d: %s\n",i,et[i].seq_esc);
	}
}

static int getkey(message,dvalue,buff,pos,backup)				/* Get a key and handle requests		*/
char	*message;
char	*dvalue;
char	*buff;
int	pos;
int	*backup;
{
	int	rc;

	for(;;)
	{
		rc = promptkey(message,dvalue,buff);

		if (rc)	printf("\n");

		if (rc == 3)
		{
			printf("Invalid escape sequence: Choose a different key.\n");
			continue;
		}

		if (rc == 1)
		{
			if (ynprompt("Do you want to Quit Immediately?",1))
				exit(0);
			continue;
		}

		if (rc == 4)
		{
			*backup=1;
			break;
		}

		if (rc)
		{
			if (ynprompt("Do you want to re-enter previous key?",0))
			{
				*backup=1;
				break;
			}
			if (ynprompt("Do you want to Restart?",0))
			{
				return(1);
			}
			if (ynprompt("Do you want to Quit Immediately?",0))
			{
				exit(0);
			}
			if (ynprompt("Do you want to Exit?",0))
			{
				return(2);
			}
			continue;
		}
		if (!dupvalue(buff,pos)) break;
	}
	return(0);
}

static prompt(message,dvalue,buff)
char 	*message;			/* The prompt			*/
char	*dvalue;			/* Default value		*/
char	*buff;				/* return buffer		*/
{
	*buff = '\0';
	if (*dvalue)
		printf("%s [%s]: ",message,dvalue);
	else
		printf("%s: ",message);

	if ( !gets(buff) || strlen(buff)==0 )
	{
		if (*dvalue)
			strcpy(buff,dvalue);
		else
			*buff = '\0';
	}
}

static int ynprompt(message,dvalue)
char	*message;
int	dvalue;
{
	char	yn, buff[40];

	yn = (dvalue) ? 'y':'n';

	for(;;)
	{
		printf("%s [%c]: ",message,yn);
		if ( !gets(buff) || strlen(buff)==0 )
			return( dvalue );

		if (buff[0] == 'y' || buff[0] == 'Y')
			return( 1 );

		if (buff[0] == 'n' || buff[0] == 'N')
			return( 0 );

		printf("Invalid response: Please enter 'y'(yes) or 'n'(no).\n");
	}
}

static int promptkey(message,dvalue,buff)
char	*message;			/* The keyname			*/
char	*dvalue;			/* Default value		*/
char	*buff;				/* The returned esc_seq		*/
{
	int	i,len;
	char	*p, *ptr;
	char	tbuff[50];

	printf("Press key %s [%s]: ",message, dvalue);
	fflush(stdout);
	getstr_to(tbuff,MAX_ESC_LEN-1,fileno(stdin),&len);

	if (len==1 && tbuff[0] == 'q')		/* quit			*/
	{
		return(1);
	}
	else if (len==1 && tbuff[0] == ' ')	/* more options		*/
	{
		return(2);
	}
	else if (len==1 && tbuff[0] == 'b')	/* Backup 		*/
	{
		return(4);
	}
	else if (len==1 && tbuff[0] == 13)	/* Carrage Return	*/
	{
		strcpy(buff,dvalue);
	}
	else if (tbuff[0] >= ' ' && tbuff[0] <= '~') 
	{
		return(3);
	}
	else
	{
		strcpy(buff,"\"");
		for (p=tbuff; len; ++p, --len) 
		{
			ptr = makeprintable(*p);
			strcat(buff,ptr);
		}
		strcat(buff,"\"");
	}
	printf("%s\n",buff);
	return(0);
}

static int dupvalue(buff,pos)							/* Test for a duplicate				*/
char	*buff;
int	pos;
{
	int	i,bsize;

	bsize = strlen(buff);

	for(i=0; i<TABLE_SIZE;i++)
	{
		if (i != pos && 0==memcmp(et[i].seq_esc,buff,bsize))
		{
			printf("Duplicate escape sequence with %s: Choose a different key.\n",et[i].seq_mess);
			return(1);
		}
	}
	return(0);
}

/* play with these to affect the timeout */
#define TOSEC 0 /* timeout seconds  */
#define TOUSEC 750000 /* timeout microseconds */

static char *makeprintable(value)
unsigned char value;
{
	static char buf[6];
	
	memset(buf,0,sizeof(buf));
	if (value==27)
		strcpy(buf,"\\E");
	else if (value=='\"')
	  	strcpy(buf,"\\\"");
	else if (value<' ')
	  	sprintf(buf,"^%c",value+64);
	else if (value>'~')
		sprintf(buf,"\\%03o",value);
	else 	sprintf(buf,"%c",value);
	return buf;
}

static getstr_to(string,maxsz,fd,len)  
char *string; /* receiver area */
int maxsz,fd; /* maxsz is max size of receiver, fd is input fd */
int *len;
{
	struct timeval to;
	fd_set rfds;
	int pos,cnt,reading;
	struct termio old, new;

	signal(SIGINT,SIG_IGN);
	
	FD_ZERO(&rfds);
	FD_SET(fd,&rfds);
	
	if (ioctl(fd,TCGETA,&old)<0)
	{
		perror("ioctl tcgeta");
		signal(SIGINT,SIG_DFL);
		exit(1);
	}
	new=old;
	new.c_cc[VMIN]=1;
	new.c_cc[VTIME]=0;
	new.c_lflag &= ~(ICANON|ECHO);
	new.c_iflag = 0;
	if (ioctl(fd,TCSETA,&new)<0)
	{
		perror("ioctl tcseta");
		signal(SIGINT,SIG_DFL);
		exit(1);
	}

	if (ioctl(fd,TCSETA,&new)<0)
	{
		perror("ioctl tcgeta");
		signal(SIGINT,SIG_DFL);
		exit(1);
	}

	to.tv_sec  = TOSEC;
	to.tv_usec = TOUSEC;
	pos=0;
	read(fd,string+(pos++),1);
	while (cnt=select(20,&rfds,0,0,&to))
	{
		read(fd,string+pos,1);
		FD_ZERO(&rfds);
		FD_SET(fd,&rfds);
		++pos;
	}
	*(string+pos)=(char)0;
	*len = pos;
	ioctl(fd,TCSETA,&old);

	signal(SIGINT,SIG_DFL);
}

static int writevcap(filename)						/* Write-out the file.					*/
char	*filename;
{
	int	i;
	FILE	*fd;

	fd = fopen(filename,"w");
	if (!fd)
	{
		printf("Unable to open file %s\n",filename);
		return(1);
	}


	for(i=0; i<TABLE_SIZE; i++)
	{
		if (et[i].seq_esc[0])
			fprintf(fd,"%-20s=%s\n",et[i].seq_name,et[i].seq_esc);
	}

	fclose(fd);
	return(0);
}

static init_table()
{
	int	i;

	for(i=0;i<TABLE_SIZE;i++) et[i].seq_esc[0] = '\0';
	for(i=1;i<=32;i++)
	{
		sprintf(et[i].seq_mess,"PF%d",i);
		sprintf(et[i].seq_dvalue,"\"^F%02d\"",i);
		sprintf(et[i].seq_name,"generic_pf%d",i);
	}
	strcpy(et[GEN_ENTER].seq_mess, "ENTER");
	strcpy(et[GEN_ENTER].seq_dvalue, "\"^M\"");
	strcpy(et[GEN_ENTER].seq_name, "generic_enter");

	strcpy(et[GEN_UP].seq_mess, "UP-ARROW");
	strcpy(et[GEN_UP].seq_dvalue, "\"\\E[A\"");
	strcpy(et[GEN_UP].seq_name, "generic_up");

	strcpy(et[GEN_DOWN].seq_mess, "DOWN-ARROW");
	strcpy(et[GEN_DOWN].seq_dvalue, "\"\\E[B\"");
	strcpy(et[GEN_DOWN].seq_name, "generic_down");

	strcpy(et[GEN_RIGHT].seq_mess, "RIGHT-ARROW");
	strcpy(et[GEN_RIGHT].seq_dvalue, "\"\\E[C\"");
	strcpy(et[GEN_RIGHT].seq_name, "generic_right");

	strcpy(et[GEN_LEFT].seq_mess, "LEFT-ARROW");
	strcpy(et[GEN_LEFT].seq_dvalue, "\"\\E[D\"");
	strcpy(et[GEN_LEFT].seq_name, "generic_left");

	strcpy(et[GEN_HELP].seq_mess, "HELP");
	strcpy(et[GEN_HELP].seq_dvalue, "\"^E\"");
	strcpy(et[GEN_HELP].seq_name, "generic_help");

	strcpy(et[GEN_TAB].seq_mess, "TAB");
	strcpy(et[GEN_TAB].seq_dvalue, "\"^I\"");
	strcpy(et[GEN_TAB].seq_name, "generic_tab");

	strcpy(et[GEN_BACKTAB].seq_mess, "BACKTAB");
	strcpy(et[GEN_BACKTAB].seq_dvalue, "\"^F^I\"");
	strcpy(et[GEN_BACKTAB].seq_name, "generic_backtab");

	strcpy(et[GEN_DELETE].seq_mess, "BACKSPACE");
	strcpy(et[GEN_DELETE].seq_dvalue, "\"^H\"");
	strcpy(et[GEN_DELETE].seq_name, "generic_delete");

	strcpy(et[GEN_REMOVE].seq_mess, "DEL-CHAR");
	strcpy(et[GEN_REMOVE].seq_dvalue, "\"\\177\"");
	strcpy(et[GEN_REMOVE].seq_name, "generic_remove");

	strcpy(et[GEN_INSERT].seq_mess, "INS-CHAR");
	strcpy(et[GEN_INSERT].seq_dvalue, "\"^Fi\"");
	strcpy(et[GEN_INSERT].seq_name, "generic_insert");

	strcpy(et[GEN_NEWLINE].seq_mess, "NEWLINE");
	strcpy(et[GEN_NEWLINE].seq_dvalue, "\"^Fn\"");
	strcpy(et[GEN_NEWLINE].seq_name, "generic_newline");

	strcpy(et[GEN_CLEAR_AFTER].seq_mess, "CLEAR-AFTER (ERASE)");
	strcpy(et[GEN_CLEAR_AFTER].seq_dvalue, "\"\\E\\E[C\"");
	strcpy(et[GEN_CLEAR_AFTER].seq_name, "generic_clear_after");

	strcpy(et[GEN_CLEAR_FIELD].seq_mess, "CLEAR-FIELD");
	strcpy(et[GEN_CLEAR_FIELD].seq_dvalue, "\"\\E\\E[A\"");
	strcpy(et[GEN_CLEAR_FIELD].seq_name, "generic_clear_field");
}
