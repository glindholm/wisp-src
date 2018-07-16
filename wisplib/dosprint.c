			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef MSDOS

/*
**	File:		dosprint.c
**
**	Purpose:	To hold MS-DOS printing routines.
**
**	Routines:	
**	dos_print()	Print a file with all the Wang style options including copies and delete after.
**	prtfile()	Print a file on a printer (0-2) using class and form info.
**	prtwrite()	Write a block of data to a dos printer.
**	prtcheck()	Check if a dos printer if ready for data. INT x17 x02
**	prtputc()	Write a single character to a dos printer. INT x17 x00
**	prtstatus()	Return a coded error number based on a dos printer status.
**	prtinit()	Initialize a dos printer. INT x17 x01
**	prterr()	Return an error message for a return code from prtfile()
**	get_prtinfo()	Get printer setup and reset strings based on prtnum, class, and form
**	escstr()	Change a written escape string into real escape codes
**
*/

#include <io.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <stdio.h>
#include <dos.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "idsistd.h"
#include "wdefines.h"
#include "werrlog.h"

#define PRTFILE_SUCCESS                 0
#define PRTFILE_FILE_OPEN_FAILED        1
#define PRTFILE_PRINTER_OPEN_FAILED     2
#define PRTFILE_READ_FAILED             3
#define PRTFILE_WRITE_FAILED            4
#define PRTFILE_NOT_READY		5

#define ERR_OK			0
#define ERR_NOT_SELECTED	1
#define ERR_BUSY		2
#define ERR_OUT_OF_PAPER	3
#define ERR_IO_ERROR		4

static char *statusmess[] = {
	"OK",
	"NOT SELECTED",
	"BUSY",
	"OUT OF PAPER",
	"IO ERROR"
	};

static int prtfile();
static int prtwrite();
static int prtcheck();
static int prtputc();
static int prtstatus();
static int prtinit();
static char *prterr();
static int get_prtinfo();
static int escstr();


/*
**	Routine:	dos_print()
**
**	Function:	Print a file with all the Wang style options including copies and delete after.
**
**	Description:	This routine handles the Wang style mode and copies options and calls prtfile()
**			to print a single copy of a file.
**
**	Arguments:
**	file		Dos style filename.
**	copies		Number of copies.
**	formnum		Wang style form number FORM_NUM.
**	lpclass		Wang style print class PRTCLASS.
**	printer		Wang style printer number PRINTER. (0-2)
**	delete_after	Flag to delete file after printing.
**	mode		Wang style print mode PRNTMODE. 
**				S,P	Print
**				H,K	Don't print
**				
**
**	Globals:	None
**
**	Return:
**	0		Success
**	20		File not found
**	24		Zero length file
**	28		Delete after failed
**	99		Print failed or invalid argument
**
**	Warnings:
**			Only PRINTER 0,1, and 2 are supported.
**			If printing fails then the file will not be deleted.
**
**
*/
int dos_print( file, copies, formnum, lpclass, printer, delete_after, mode )
char    *file;
int     copies;
int     formnum;
char    lpclass;
long	printer;
int     delete_after;
char    mode;
{
#undef          ROUTINE
#define         ROUTINE         46600
/*
46601	%%DOS_PRINT-I-ENTRY File=%s copies=%d form=%d printer=%d
46602	%%DOS_PRINT-E-FAILED Print of file %s failed [errno=%d]
46603	%%DOS_PRINT-W-UNLINK Unable to delete %s errno=%d
46604	%%DOS_PRINT %s
46605	%%DOS_PRINT-W-SIZE Zero lenght file not printed %s
46606	%%DOS_PRINT-E-FAILED Print of file %s failed %s
*/
	char    lastline[80];
	int     rc, size, zerofile;

	werrlog(ERRORCODE(1),file,copies,formnum,printer,0,0,0,0);

	switch(mode)
	{
	case 'H':
	case 'K':
		return(0);                                                              /* Don't print if HOLD mode.            */
		break;
	case 'S':
	case 'P':                                                                       /* SPOOL it.                            */
		break;
	default:
		return(99);
		break;
	}

	if ( access( file, 00 ) )                                                       /* Does file exist?                     */
	{
		return( 20 );
	}
	
	rc  = 0;
	zerofile = 0;

	size = filesize(file);
	if ( size == 0 )
	{
		werrlog(ERRORCODE(5),file,0,0,0,0,0,0,0);
		zerofile = 1;
	}                     

	if ( !zerofile  && copies > 0  )
	{
		sprintf(lastline,"Printing %s",file);
		put_lastline(lastline);
		while( copies-- )
		{
			rc = prtfile(file,(int)printer,lpclass,(int)formnum);
			if ( rc != 0 )
			{
				werrlog(ERRORCODE(6),file,prterr(rc),0,0,0,0,0,0); 
				clear_lastline();
				return( 99 );
			}
		}
		clear_lastline();
	}

	if ( delete_after )
	{
		rc = unlink(file);
		if ( rc < 0 )
		{
			werrlog(ERRORCODE(3),file,errno,0,0,0,0,0,0);
			return( 28 );
		}
	}

	if ( zerofile ) rc = 24;

	return( rc );
}

/*
**	Routine:	prtfile()
**
**	Function:	Print a file on a printer (0-2) using class and form info.
**
**	Description:	This routine prints a file to a dos printer first sending a set string then the file 
**			then the reset string. The prtnum is translated into a dos printer number and the
**			setup and reset strings are looked up based on the prtnum, class, and form in PRTMAP.
**
**			The Wang style printer number is translated into a dos printer number.
**				0	LPT1 (PRN)
**				1	LPT2
**				2	LPT3
**
**			An alert screen will occur if the printer is not ready.
**			It uses prtwrite() to write the data to the printer.
**
**	Arguments:
**	file		The dos style filename.
**	prtnum		Wang style printer number (0-2)
**	class		Wang style print class
**	form		Wang style form number
**
**	Globals:	None
**
**	Return:		Coded error number. 
**	0		Success
**	Not zero	Error. Use prterr() to get message.
**
**	Warnings:	A prtnum outside of 0-2 will be changed to 0.
**			This assumes printable data and uses stream I/O to read the file.  If the file
**			contains NULLs or other non-printable data then printing may be corrupted.
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int prtfile(file,prtnum,class,form)
char    *file;
int	prtnum;
char	class;
int	form;
{
	FILE	*infile;
        char    buffer[1024];
        int     rc;
	char	*printer;
	int	setup_len, reset_len;
	char	setup[80], reset[80];
	int	serial_printer;

	switch(prtnum)
	{
	default:
	case 0:
		printer = "LPT1";
		prtnum = 0;
		break;
	case 1:
		printer = "LPT2";
		break;
	case 2:
		printer = "LPT3";
		break;
	}

	get_prtinfo(prtnum,class,form,&setup_len,setup,&reset_len,reset);

	/*
	**	For now lets always assume parallel printer.
	**	The printing will work on a serial printer but will not handle errors as gracefully.
	*/
	serial_printer = 0;

	while (rc = prtcheck(prtnum))
	{
		char	buff[256];

		sprintf(buff,"Printer %s (%d) is not ready (%s).\nCorrect the problem then press (ENTER) to retry.\n ",
			printer,prtnum,statusmess[rc]);
		switch(valert(buff,"Retry",NULL,"Cancel Print"))
		{
		case 0:	/* Retry */
			break;
		case 16:/* Cancel Print */
			return(PRTFILE_NOT_READY);
		}
	}

        rc = 0;

	if (setup_len)
	{
		if (prtwrite(prtnum,serial_printer,setup,setup_len))
		{
			return(PRTFILE_NOT_READY);
		}
	}
	
	if (infile = fopen(file,"rb"))
	{
		while (fgets(buffer, sizeof(buffer), infile))
		{
			untabify(buffer,sizeof(buffer));
			if (prtwrite(prtnum,serial_printer,buffer,strlen(buffer)))
			{
				rc = PRTFILE_NOT_READY;
				break;
			}
		}
		if (ferror(infile))
		{
                        rc = PRTFILE_READ_FAILED;			
		}
		fclose(infile);
	}
	else
	{
                rc = PRTFILE_FILE_OPEN_FAILED;
	}

	if (reset_len && rc != PRTFILE_NOT_READY)
	{
		if (prtwrite(prtnum,serial_printer,reset,reset_len))
		{
			rc = PRTFILE_NOT_READY;
		}
	}

        return( rc );
}

/*
**	Routine:	prtwrite()
**
**	Function:	Write a block of data to a dos printer.
**
**	Description:	Sends a block of data to the printer one character at a time.
**			Before and after each character is written the status is checked 
**			and an alert screen will be issued if the printer is not ready.
**			If printer is BUSY we wait a second then try again, if BUSY for
**			20 seconds an alert window is displayed.
**
**			For parallel printers we don't need the prtcheck() before each
**			character as the prtputc() returns the correct status.  Also
**			a BUSY status is expected from a call to prtputc() as it is
**			busy with the char you just gave it.
**
**	Arguments:	
**	printer		Dos printer number
**	serial_printer	Flag 1=serial 0=parallel printer
**	data		The block of data
**	len		The length of data
**
**	Globals:	None
**
**	Return:		Number of bytes NOT written.
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**	05/13/93	Added serial_printer flag and support of parallel printers. GSL
**
*/
static int prtwrite(printer,serial_printer,data,len)
int	printer;
int	serial_printer;
char	*data;
int	len;
{
#define BUSY_TIMEOUT	20
	char	buff[256];
	int	rc;
	int	twiddle;

	for(; len; len--)
	{
		twiddle = 0;
		if (serial_printer)
		{
			while (rc = prtcheck(printer))
			{
				twiddle++;
				if (rc != ERR_BUSY || twiddle > BUSY_TIMEOUT)
				{
					twiddle = 0;
					sprintf(buff,"Error writing to printer (%d) \nPrinter is %s\n%s\n ",
						printer,statusmess[rc],"Correct the problem then press (ENTER) to continue.");
					switch(valert(buff,"Continue",NULL,"Cancel Print"))
					{
					case 0:	/* Retry */
						break;
					case 16:/* Cancel Print */
						return(len);
					}
				}
				else
				{
					sleep(1);
				}
			}
		}

		while (rc = prtputc(printer,*data))
		{
			if (!serial_printer)
			{
				if (rc == ERR_BUSY)
				{
					/*
					**	For parallel printers BUSY is not an error.
					*/
					break;
				}
			}

			sprintf(buff,"Error writing to printer (%d) \nPrinter is %s\n%s\n ",
				printer,statusmess[rc],"Correct the problem then press (ENTER) to continue.");
			switch(valert(buff,"Continue",NULL,"Cancel Print"))
			{
			case 0:	/* Retry */
				break;
			case 16:/* Cancel Print */
				return(len);
			}
		}
		data++;
	}
	return 0;
}

/*
**	Routine:	prtcheck()
**
**	Function:	Check if a dos printer if ready for data. INT x17 x02
**
**	Description:	Does a DOS interupt 17 function 2 to get the status of the printer.
**			Then calls prtstatus() to convert the status into an error code.
**
**	Arguments:
**	printer		Dos printer number
**
**	Globals:	None
**
**	Return:		See prtstatus()
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int prtcheck(printer)
int	printer;
{
#define	NOT_BUSY	0x80
#define	OUT_OF_PAPER	0x20
#define SELECTED	0x10
#define IO_ERROR	0x08
	union REGS reg;
	reg.h.ah = 2;
	reg.x.dx = printer;
	int86(0x17, &reg, &reg);

	return(prtstatus((unsigned char)reg.h.ah));
}

/*
**	Routine:	prtputc()
**
**	Function:	Write a single character to a dos printer. INT x17 x00
**
**	Description:	Does an Inturrupt 17 func 00 to write a single character to the printer.
**			Then calls prtstatus() to convert the status into an error code.
**
**	Arguments:
**	printer		Dos printer number
**
**	Globals:	None
**
**	Return:		See prtstatus()
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int prtputc(printer,c)
int	printer;
char	c;
{
	union REGS reg;
	reg.h.ah = 0;
	reg.h.al = (unsigned char)c;
	reg.x.dx = printer;
	int86(0x17, &reg, &reg);

	return(prtstatus((unsigned char)reg.h.ah));
}

/*
**	Routine:	prtstatus()
**
**	Function:	To convert the DOS int 17 printer status into a error code.
**
**	Description:	Test the error bits of the status and return an error code.
**
**	Arguments:
**	reg_h_ah	The printer status from an INT 17.
**
**	Globals:	None
**
**	Return:		error code.
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int prtstatus(reg_h_ah)
unsigned char reg_h_ah;
{
	if (  reg_h_ah & OUT_OF_PAPER) 	return(ERR_OUT_OF_PAPER);
	if (!(reg_h_ah & SELECTED)) 	return(ERR_NOT_SELECTED);
	if (!(reg_h_ah & NOT_BUSY)) 	return(ERR_BUSY);
	if (  reg_h_ah & IO_ERROR) 	return(ERR_IO_ERROR);
	return(0);
}

/*
**	Routine:	prtinit()
**
**	Function:	Initialize a dos printer. INT x17 x01
**
**	Description:	Does a DOS interupt 17 function 1 to initialize a printer.
**			Then calls prtstatus() to convert the status into an error code.
**
**	Arguments:
**	printer		Dos printer number
**
**	Globals:	None
**
**	Return:		See prtstatus()
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int prtinit(printer)
int	printer;
{
	union REGS reg;
	reg.h.ah = 1;
	reg.x.dx = printer;
	int86(0x17, &reg, &reg);
	return(prtstatus((unsigned char)reg.h.ah));
}

/*
**	Routine:	prterr()
**
**	Function:	Return an error message for a return code from prtfile()
**
**	Description:	Convert an error code into a message.
**
**	Arguments:
**	rc		The error code.
**
**	Globals:	None
**
**	Return:		Error message.
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static char	*prterr(rc)
int	rc;
{
static	char	message[80];
	char	*ptr;

	switch(rc)
	{
	case PRTFILE_SUCCESS:
		ptr = "SUCCESS";
		break;
	case PRTFILE_FILE_OPEN_FAILED:
		ptr = "OPEN of data file failed";
		break;
	case PRTFILE_PRINTER_OPEN_FAILED:
		ptr = "OPEN of printer failed";
		break;
	case PRTFILE_READ_FAILED:
		ptr = "READ of data file failed";
		break;
	case PRTFILE_WRITE_FAILED:
		ptr = "WRITE to printer failed";
		break;
	case PRTFILE_NOT_READY:
		ptr = "PRINTER was not ready";
		break;
	default:
		ptr = "FAILED for unknown reason";
		break;
	}
	strcpy(message,ptr);
	return(message);
}

/*
**	Routine:	get_prtinfo()
**
**	Function:	Get printer setup and reset strings based on prtnum, class, and form.
**
**	Description:	Reads the PRTMAP file in $WISPCONFIG to build an internal table of SETUP and RESET strings.
**			Searches this table and returns the first entry that matches prt_num, prt_class and prt_form.
**
**	#
**	#	PRTMAP		MS-DOS PRINTING MAP
**	#
**	#	PRINTER		Wang style printer number.
**	#	CLASS		Wang style print class.
**	#	FORM		Wang style form number.
**	#	SETUP		Printer setup string to use.
**	#	RESET		Printer reset string to use.
**	#
**	#	This table is searched sequentially for a matching
**	#	printer, class, and form number.  An '*' can be used
**	#	to wildcard printer, class, and form number.  When
**	#	a match is found corresponding setup, and reset will
**	#	be used.  The setup string will be sent to the printer 
**	#	before you file is printed, and reset string will be
**	#	sent after. A '-' can be used to indicate no setup
**	#	or reset string.  To include a space, tab or newline
**	#	in a string use the control character or octal number.
**	#	(Space = \040 Tab = ^I or \011 Newline = ^J or \012)
**	#
**	#	\E	Escape	\033
**	#	\ddd	Octal
**	#	\n	Newline
**	#	\\	'\'
**	#	\"	'"'
**	#	^x	Control x	x & 0x1F
**	#
**	#PRINTER CLASS FORM SETUP     		RESET
**	#0         A    000 "\EE\E&l0O" 	"\EE"
**	0	   *	000 "\EE\E&l0O" 	"\EE"
**	0	   *	132 "\EE\E&l1O" 	"\EE"
**	0	   *	133 "\EE\E&l1O\E&l8D"	"\EE" 
**	0	   *	088 "\EE\E&l8D" 	"\EE"
**	0	   *    044 "\EE\E&l4D"		"\EE"
**	0	   *	*   "\EE"		"\EE"
**	1	   *	*   "\EE"		"\EE"
**	#*         *    *   -         -
**
**	Arguments:
**	prt_num		Wang style printer number.
**	prt_class	Wang style print class.
**	prt_form	Wang style form number.
**	prt_setup_len	Length to setup string.
**	prt_setup	The setup string.
**	prt_reset_len	Length to reset string.
**	prt_reset	The reset string.
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int get_prtinfo(prt_num,prt_class,prt_form,prt_setup_len,prt_setup,prt_reset_len,prt_reset)
int	prt_num;
char	prt_class;
int	prt_form;
int	*prt_setup_len;
char	*prt_setup;
int	*prt_reset_len;
char	*prt_reset;
{
#define WILD_NUM	-1
#define WILD_CHAR	'*'

	static int first=1;
	struct prt_info
	{
		struct prt_info *next;
		int	num;
		char	class;
		int	form;
		int	setup_len;
		char	setup[80];
		int	reset_len;
		char	reset[80];
	};
	static struct prt_info *prt_info_list;
	struct prt_info *curr;

	if (first)
	{
		char	*ptr;
		char	filename[80];
		FILE	*fh;

		first = 0;
		prt_info_list = (struct prt_info *)malloc(sizeof(struct prt_info));
		curr = prt_info_list;

		if (ptr = getenv(WISP_CONFIG_ENV))
		{
			buildfilepath(filename,ptr,"PRTMAP");
			if (fh = fopen(filename,"r"))
			{
				for(;;)
				{
					char	buff[256];

					if (fgets(buff,sizeof(buff),fh))
					{
						if (buff[0] && buff[0] != '#')
						{
							char	arg1[80],arg2[80],arg3[80],arg4[80],arg5[80];
							int	rc;
							rc = sscanf(buff,"%s %s %s %s %s",arg1,arg2,arg3,arg4,arg5);
							if (rc == 5)
							{
								if (strcmp("*",arg1)==0) curr->num = WILD_NUM;
								else curr->num = atoi(arg1);

								curr->class = arg2[0];

								if (strcmp("*",arg3)==0) curr->form = WILD_NUM;
								else curr->form = atoi(arg3);

								if (strcmp("-",arg4)==0) curr->setup_len = (char)0;
								else curr->setup_len = escstr(arg4,curr->setup);

								if (strcmp("-",arg5)==0) curr->reset_len = (char)0;
								else curr->reset_len = escstr(arg5,curr->reset);

								curr->next = (struct prt_info *)malloc(sizeof(struct prt_info));
								curr = curr->next;
							}
							else
							{
								/* sscanf failed */
							}
						}
					}
					else
					{
						break;
					}
				}
				fclose(fh);
			}
		}
		/*
		**	Last entry (Catch-all)
		*/
		curr->next = NULL;
		curr->num = WILD_NUM;
		curr->class = WILD_CHAR;
		curr->form = WILD_NUM;
		curr->setup_len = 0;
		curr->reset_len = 0;
	}

	*prt_setup_len = 0;
	*prt_reset_len = 0;

	for (curr = prt_info_list; curr; curr = curr->next)
	{
		if (	(curr->num   == WILD_NUM  || curr->num   == prt_num) &&
			(curr->class == WILD_CHAR || curr->class == prt_class) &&
			(curr->form  == WILD_NUM  || curr->form  == prt_form)     )
		{
			/*
			**	Found a match.
			**	NOTE:  A match will always be found because of the "catch-all" last item.
			*/
			memcpy(prt_setup, curr->setup, curr->setup_len);
			*prt_setup_len = curr->setup_len;
			memcpy(prt_reset, curr->reset, curr->reset_len);
			*prt_reset_len = curr->reset_len;
			break;
		}
	}

	return 0;
}

/*
**	Routine:	escstr()
**
**	Function:	Change a written escape string into real escape codes
**
**	Description:	Decode the written escape string and write to dest.
**			The leading and trailing " quote are ignored.
**		
**			\E	Escape	\033
**			\ddd	Octal
**			\n	Newline
**			\\	'\'
**			\"	'"'
**			^x	Control x	x & 0x1F
**
**
**	Arguments:
**	src		The source written escape string. enclosed in " quote chars. I.e "\E\020\n"
**	dest		The destination real escape string.
**
**	Globals:	None
**
**	Return:		Length to dest.
**
**	Warnings:	None
**
**	History:	
**	04/01/93	Written by GSL
**
*/
static int escstr(src,dest)
char	*src;
char	*dest;
{
	char	*sptr, *dptr;
	int	len;

	sptr = src;
	dptr = dest;
	len = 0;

	if (*sptr == '"') sptr++;						/* Skip leading quote				*/

	while(*sptr)								/* Loop until null char				*/
	{
		switch(*sptr)
		{
		case '\\':							/* Leading \ char				*/
			sptr++;
			switch(*sptr)
			{
			case 'E':						/* \E escape					*/
				*dptr++ = '\033';
				sptr++;
				len++;
				break;
			case '\\':						/* \\ \" \'   remove special meaning		*/
			case '"':
			case '\'':
				*dptr++ = *sptr++;
				len++;
				break;
			case 'n':						/* \n newline					*/
				*dptr++ = '\n';
				sptr++;
				len++;
				break;
			default:						/* Either an octal number or ignore \		*/
				if (*sptr >= '0' && *sptr <= '7') 		/* \ddd  - accept 1,2 or 3 digits		*/
				{
					*dptr = *sptr - '0';
					sptr++;
					if (*sptr >= '0' && *sptr <= '7') 
					{
						*dptr *= 8;
						*dptr += *sptr - '0';
						sptr++;
						if (*sptr >= '0' && *sptr <= '7') 
						{
							*dptr *= 8;
							*dptr += *sptr - '0';
							sptr++;
						}
					}
					dptr++;
					len++;
				}
				break;
			}
			break;
		case '^':							/* Control character ^x				*/
			sptr++;
			*dptr = *sptr & 0x1F;
			sptr++;
			dptr++;
			len++;
			break;

		case '"':							/* Trailing " char				*/
			sptr = "";
			break;

		default:							/* Normal character				*/
			*dptr++ = *sptr++;
			len++;
			break;
		}
	}

	return(len);
}

#else /* MSDOS */
static int dummy_dosprint;
#endif

