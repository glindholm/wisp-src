			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


/*
**	screen.c
*/

#include <stdio.h>
#include <time.h>                                                                             /* Include time definitions.    */
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <varargs.h>

#ifdef VMS
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#ifdef MSDOS
#include <io.h>
#endif

#include <varargs.h>

#include "idsistd.h"
#include "vwang.h"
#include "wcommon.h"
#include "vwang.h"									/* Include file for wisp screen defs.	*/
#include "werrlog.h"
#include "wperson.h"
#include "wdefines.h"
#include "movebin.h"
#include "wanguid.h"

char	*wfname();
											
#define USF_BIT	0x80								        /* Underscore FAC bit set.		*/

#define ROUTINE		57000

struct CPOS_STRUCT
{
	int4 row;
	int4 col;
};

void SCREEN(va_alist)
va_dcl
{
	va_list	the_args;	
	/* SCREEN(status, ufb_addr, func_type, output_rcvr, cpos) */
	char 	*status, *ufb_addr, *func_type, *output_rcvr;
	struct CPOS_STRUCT *cpos;

	struct	CRT_STRUCT                                                              /* Screen structure definition.		*/
	{
		char order_area[4];							/* First 4 bytes are the order area.	*/
		char screen_area[1920];							/* 80 columns X 24 rows = 1920 bytes.	*/
	} crt_record;	       								/* Allocate storage automatically.	*/
	int file_stat;									/* Value returned by UNIX OPEN func.	*/
	unsigned char vwfunc;								/* Funxtion performed by VWANG.		*/
	unsigned char nlines;
	int4 screen_size;
	char filename[81],								/* Local storage for VAX filename.	*/
	     pfkey[2],									/* Receive the PFKEY value from VWANG.	*/
	     vwang_status[2],								/* Returned status from VWANG.		*/
	     *work_area;   								/* Dynamically allocated storage.	*/
	char print_file[9];
	va_list screen_args;
	int s_arg_cnt=0;
	int retcd;
	
	werrlog(ERRORCODE(1),'?',0,0,0,0,0,0,0);

	va_start(the_args);
	s_arg_cnt = va_count(the_args);
	va_start(the_args);

	status = va_arg(the_args, char*);
	ufb_addr = va_arg(the_args, char*);
	func_type = va_arg(the_args, char*);
	output_rcvr = va_arg(the_args, char*);
	if (s_arg_cnt == 5)
	{
		cpos = va_arg(the_args, struct CPOS_STRUCT *);
	}
	va_end(the_args);

	werrlog(ERRORCODE(1),*func_type,0,0,0,0,0,0,0);
	
	vwfunc = READ_ALL;								/* Cause VWANG to return entire screen.	*/
	nlines = 24;									/* Tell VWANG to return 24 lines.	*/
	screen_size = 1920;								/* Size of the screen. 24 X 80.		*/

	memcpy(print_file,"##      ",8);
	memcpy(&print_file[2],wanguid3(),3);
	

	crt_record.order_area[0] = 1;							/* Initialize order area just like a	*/
	crt_record.order_area[1] = 160;							/* WISP/COBOL program would when 	*/
	crt_record.order_area[2] = 0;							/* reading the entire screen.		*/
	crt_record.order_area[3] = 0;

											/* Call VWANG to read entire screen.	*/
	vwang(&vwfunc, (unsigned char *)&crt_record, &nlines, "00X", pfkey, vwang_status);
	retcd = 0;
	switch (*func_type)
	{                      		
		case 'I':               						/* Return exact image of screen w/ FACs	*/
		{
		  	memcpy(output_rcvr, crt_record.screen_area, (int)screen_size);	/* Place entire screen at specified loc */
			break;								/* All done.				*/
		}
		case 'N':								/* Return printable image of screen	*/
		{
			strip_facs(crt_record.screen_area, screen_size,0);		/* Remove all FAC characters.		*/
											/* Open up and output file.		*/
			retcd = di_write_file(crt_record.screen_area, 1920, 80, output_rcvr, print_file);	
			break;								/* Done with case action.		*/
		}
		case 'P':								/* Create a printable image w/ borders.	*/
		{
			strip_facs(crt_record.screen_area, screen_size,0);              /* Remove non-printing characters.	*/
			work_area = malloc(3060);	/* 3060 = (80+10)*(24+10) */	/* Allocate neccessary space for image.	*/
			border_screen(work_area, crt_record.screen_area, 80, 80, 24);	/* Border it up.			*/
			retcd = di_write_file(work_area, 3060, 90, output_rcvr, print_file); /* Open and output file.		*/
			break;								/* Done with case action.		*/
		}
	    	default:								/* No default action.  Give error msg.	*/
		{             
			werrlog(ERRORCODE(2),*func_type,0,0,0,0,0,0,0);
			break;
		}
	}
	if (retcd)									/* Some error when trying to print	*/
	{										/* the current screen.			*/
		werrlog(ERRORCODE(8),retcd,0,0,0,0,0,0,0);
	}

	if (s_arg_cnt==5)
	{
		extern int vcur_lin, vcur_col;
		int lin,col;
		
		lin=vcur_lin+1;
		col=vcur_col+1;
		PUTBIN(&(cpos->row),&lin,sizeof(int4));
		PUTBIN(&(cpos->col),&col,sizeof(int4));
	}
}                                                     



#define	FAC_BIT		0x80
#define PROTECT_BIT	0x04
#define	LINE_BIT	0x20

											/* Remove all FAC characters, replacing	*/
strip_facs(string_addr, string_len,type)						/*  with a space.			*/
char *string_addr;						                        /* Address of string to be stripped.	*/
int4 string_len;									/* Length of that string.		*/
int type;		/* 0=Wang screen map (with FACs & 0x0b), 1=vchr_map */
{
	unsigned char *l_string_addr, *pos;						/* Local copy of the string address.	*/
	int4 i, usfl, blfl, pbfl;							/* Keeps the count.			*/
	unsigned char psb_char, psb_select;						/* Pseudo blank character.		*/
	char	def_psb_select;

	wpload();
	get_defs(DEFAULTS_PSB_CHAR,&def_psb_select);
	get_psb_char(def_psb_select,&psb_char,&psb_select);

	l_string_addr = (unsigned char *)string_addr;					/* Get the passed value.		*/
	i = string_len;		     							/* Ditto.				*/

	usfl = FALSE;									/* Underscore flag			*/
	blfl = FALSE;									/* Blank flag				*/
	pbfl = FALSE;									/* Pseudo blank flag			*/
	while (i) 
	{
		if (*l_string_addr == PSEUDO_BLANK_CHAR) *l_string_addr = '*';		/* PSB? Yes.  Replace with an asterisk.	*/
		else if (*l_string_addr == '\0') *l_string_addr = ' ';			/* NULL? Yes.  Replace with a space.	*/
	 	else if ((*l_string_addr & (unsigned char)FAC_BIT) || 
			 (*l_string_addr == WANG_MENU_PICK)          )			/* Is it a FAC character ?		*/
		{
			pos = l_string_addr;						/* Set pos for clarity.			*/

#ifdef OLD
			if ( ((*pos >= 0x98) && (*pos <= 0x9E)) ||			/* Is it a BLANK FAC?			*/
			     ((*pos >= 0xB8) && (*pos <= 0xBE)) )
#endif
			if ( (*pos & (unsigned char)0x10) &&
			     (*pos & (unsigned char)0x08)   )
			{
				blfl = TRUE;						/* Yes, set flag for the nondisplay.	*/
			}
			else	blfl = FALSE;

			if ( *pos & (unsigned char)LINE_BIT )				/* Is it a LINE FAC?			*/
			{
				usfl = TRUE;						/* Yes, set flag for the underscore.	*/
			}
			else usfl = FALSE;

			if ( !(*pos & (unsigned char)PROTECT_BIT) )			/* Is it not protect (MODIFY FAC)	*/
			{
				pbfl = TRUE;						/* Yes, set flag for the pseudo blank.	*/
			}
			else pbfl = FALSE;

			*pos = ' ';							/* Replace FAC with a space.		*/
		}
		else if (!type && (usfl || blfl || pbfl))				/* Test if uderscore and type P scn func*/
		{									/* or blank flag or pseudo blank.	*/
			if (blfl) *l_string_addr = ' ';					/* If BLANK FAC set then no display.	*/
			else if (usfl && ' ' == *l_string_addr) *l_string_addr = '_';	/* Replace with underscore		*/
			else if (pbfl)
			{
				if ( ' ' == *l_string_addr )
					*l_string_addr = '*'; 				/* Replace with Asterisk		*/
			}
		}
		else if ( type && ' ' != *l_string_addr && psb_char == *l_string_addr )
		{
			*l_string_addr = '*';
		}

		l_string_addr++;							/* Update pointer for next char.	*/
		i--;									/* One less char to test.		*/

		if (i % 80 == 0) 
		{									/* Test if is off screen and thus has	*/
			pbfl = FALSE;							/* implied end of field FAC.		*/
			usfl = FALSE;
			blfl = FALSE;
		}
	}
}                                                                                                                                 
											/* WRITE_FILE.C ... 			*/
int di_write_file(text, text_len, rec_len, filelibvol, def_filename)			/* Open up a printer output file.	*/
char *filelibvol; 									/* Pointer to the file to be opened.	*/
char *text;										/* Pointer to the stuff to be printed.	*/
char *def_filename;									/* Pointer to the default file name.	*/
int  text_len, rec_len;									/* Length values.			*/
{
	char volume[7], library[9], file[9];						/* VS like file specification.		*/
	char *l_filelibvol;    	     							/* Local copy of the filename pointer.	*/
	char *l_text;									/* Local copy of the text pointer.	*/
	int x;                                                                          /* Just a working variable.		*/
/*	int out_file_stat;	*/							/* Keeps track of the output file.	*/
	FILE	*out_file;
	char	out_buf[256];
	int write_stat;	   								/* Status from the write function.	*/
      	int4 mode;									/* Argument for the wfopen macro.	*/
	char native_filename[81];						       	/* Constructed VAX filename.		*/
	char alq_text[11], mrs_text[11];						/* Arguments for dynamic setting.	*/
	int retcd, i, us_fl;
	char us_buf[255];								/* Buffer for Underscore characters.	*/
	char *longuid(), id[32], tty_str[5];						/* Vars for extract.			*/
	int4 tty_num, argcnt;
        char *ctime();                                                                  /* Time request data.                   */
        time_t time_data;
        char *hpos, head_line_1[80], head_line_2[80];                                   /* Header lines work area.              */
	char *scratchp;
	char	def_prt_mode;
	char	def_prt_class;
	int4	def_prt_form;

	strcpy(file,"        ");
	strcpy(library,"        ");
	strcpy(volume,"      ");

        time_data = time(NULL);
        strcpy(head_line_2, ctime(&time_data));                                         /* Set up header lines.                 */

	strcpy(id,longuid());								/* Get the user id.			*/
	wswap(&tty_num);
	argcnt=2;
	wvaset(&argcnt);
	EXTRACT("W#",&tty_num);								/* Get the tty number from TTMAP.	*/
	wswap(&tty_num);
        sprintf(head_line_1, "WORKSTATION  %4d -  USER %3s - %s",tty_num,wanguid3(),id);

	wpload();
	retcd = 0;									/* Init return code to success.		*/
	l_filelibvol = filelibvol;     							/* Get initial value.			*/
	l_text = text;									/* Get intial value.			*/

 	memcpy(file, l_filelibvol, 8);							/* First 8 characters are the filename.	*/
	if (file[0] == ' ')								/* Did they specify a filename 	?	*/
	{
		memcpy(file, def_filename, 8);						/* No.  Use supplied default.		*/
		memcpy(l_filelibvol, def_filename, 8);					/* Since the user didn't specify a file	*/
											/* name, put one in for him.		*/
	}                                   

	l_filelibvol += 8;								/* Move to start of library name.	*/
	memcpy(library, l_filelibvol, 8);						/* Copy 8 chars. of library name.	*/
	l_filelibvol += 8;								/* Move to start of volume name.	*/
	memcpy(volume, l_filelibvol, 6);						/* Copy 6 chars. of volume name.	*/

	mode = 0;									/* Initial mode value.  All bits off.	*/
	mode |= IS_OUTPUT;								/* Set the output bit on.		*/
	mode |= IS_PRINTFILE;                                                          	/* Set the print file bit on.		*/
	mode |= IS_BACKFILL;                                                          	/* Set so will return temp fil, vol,lib.*/

	scratchp = wfname(&mode, volume, library, file, native_filename);		/* Construct the native filename.	*/
	*scratchp = (char)0;

	makepath(native_filename);							/* Ensure the path exists		*/

	if (mode & IS_BACKFILL)								/* Set to return default names used.	*/
	{
		l_filelibvol = filelibvol;						/* Get initial value.			*/
 		memcpy(l_filelibvol,file, 8);						/* First 8 characters are the filename.	*/
		l_filelibvol += 8;							/* Move to start of library name.	*/
		memcpy(l_filelibvol,library, 8);					/* Copy 8 chars. of library name.	*/
		l_filelibvol += 8;							/* Move to start of volume name.	*/
		memcpy(l_filelibvol,volume, 6);						/* Copy 6 chars. of volume name.	*/
	}

	out_file = fopen(native_filename,"w");
	if (!out_file)
	{
		werrlog(ERRORCODE(4),native_filename,errno,0,0,0,0,0,0);
		return(0);
	}

	fprintf(out_file,"\n%s\n\n%s\n",head_line_1,head_line_2);

	x = text_len / rec_len;								/* Number of records to print.		*/
	while (x)
	{
		us_fl = FALSE;								/* Assume no underscores in record.	*/
		for (i = 0; i < rec_len; i++)						/* Setup underscore buffer, if needed.	*/
		{
			out_buf[i] = l_text[i];
			us_buf[i] = ' ';						/* Otherwise set to a space.		*/
			if (out_buf[i] & USF_BIT)					/* If the high bit is set then set to	*/
			{								/* display an underscore.		*/
				us_buf[i] = DEC_MENU_PICK;
				us_fl = TRUE;						/* Set so will write print buffer.	*/
				out_buf[i] -= USF_BIT;					/* Turn the high bit off for display.	*/
			}
			else if (!out_buf[i])						/* CHange NULL into space		*/
			{
				out_buf[i] = ' ';
			}
		}
		out_buf[rec_len] = (char)0;						/* NULL terminate print line		*/
		us_buf[rec_len] = (char)0;

		if (us_fl)
		{
			fprintf(out_file,"%s\r%s\n",out_buf,us_buf);			/* Write record and underscores		*/
		}
		else
		{
			fprintf(out_file,"%s\n",out_buf);				/* Write the record.			*/
		}
		l_text += rec_len;							/* Update pointer to start of next rec.	*/
       		x--;									/* One less record to print.		*/
	}
	fclose(out_file);		 						/* Close the file.			*/

	get_defs(DEFAULTS_PM,&def_prt_mode);
	get_defs(DEFAULTS_PC,&def_prt_class);
	get_defs(DEFAULTS_FN,&def_prt_form);

#ifdef VMS
	if ('O'==def_prt_mode)
	{
		/*
		**	If ON-LINE printing then the printing has already happened.
		**	No need to call wprint() to spool the file.
		*/
		return(retcd);
	}
#endif /* VMS */

	wprint(native_filename,def_prt_mode,0,1,def_prt_class,def_prt_form,&retcd);

#ifdef VMS
	wdellock(&mode,native_filename);
#endif
	return(retcd);
}

                                             						/* BORDER_SCREEN.C ... 			*/
											/* Take a screen and create borders.	*/
border_screen(work_area, screen_image, image_rec_size, screen_rec_size, screen_rec_count)
char *work_area,   									/* Address of destination.		*/
     *screen_image;									/* Address of screen image.		*/
int  image_rec_size,									/* Size of the image on the screen.	*/
     screen_rec_size,									/* The size of the screen records.	*/
     screen_rec_count;									/* The number of records. (screen rows)	*/
{
	char *l_work_area,   								/* Local copies of above arguments.	*/
	     *l_screen_image;
	int x,y,z, work_area_size, bordered_line_length, bordered_lines;		 /* Work variables.			*/
	char *num_line_1;								/* Two character strings for border.	*/
	char *num_line_2;
                        
	l_work_area = work_area;							/* Get local copy of passed addr.	*/
	l_screen_image = screen_image;							/* Get local copy of passed addr.	*/
                                              
	bordered_line_length = image_rec_size + BORDERED_COLUMN_TOTAL;			/* What't the length of a bordered line	*/
	bordered_lines = screen_rec_count + BORDERED_LINE_TOTAL;			/* How many bordered lines are there ?	*/

	work_area_size = bordered_line_length * bordered_lines;				/* Compute amount of space needed for	*/
			 								/* final image.	    			*/
	num_line_1 = malloc(bordered_line_length);					/* Allocate required space.		*/
	num_line_2 = malloc(bordered_line_length);					/* Allocate required space.		*/

	if (num_line_1 == 0 || num_line_2 == 0)						/* Were we able to allocate the space ?	*/
	{
		werrlog(ERRORCODE(6),bordered_line_length,0,0,0,0,0,0,0);
		return(0);								/* Exit out.				*/
	}

        memset(num_line_1, ' ', image_rec_size); 					/* Initialize the string to spaces.	*/
	memset(num_line_2, ' ', image_rec_size);					/* Ditto.				*/
	
	y = 9;										/* Offset starts at pos 9		*/
	for (x = 1; x <= (image_rec_size/10); x++)					/* Create the first number border line.	*/
	{                                                     
		sprintf(&num_line_1[y], "%d", x);					/* Turn a number to ascii.		*/
		y += 10;								/* Update the offset.			*/
	}

	for (x = 1; x <= image_rec_size; x++)						/* Create the second number border line.*/
	{
		sprintf(&num_line_2[x - 1], "%d", x % 10);				/* Turn a number into an ascii char.	*/
	}
                                       
	for (x=1; x <= image_rec_size; x++)						/* Do this loop as many times as needed.*/
	{
		if (num_line_1[x] == '\0')						/* Is this  a null character ?		*/
		{
			num_line_1[x] = ' ';						/* Convert it to a space.		*/
		}
		if (num_line_2[x] == '\0')						/* Is this  a null character ?		*/
		{
			num_line_2[x] = ' ';						/* Convert it to a space.		*/
		}
	}
	
	memset(l_work_area, ' ', work_area_size);					/* Initialize the entire area.		*/
	memset(l_work_area, '*', bordered_line_length);           			/* Initialize one line to '*'		*/
	l_work_area += bordered_line_length;						/* Point to the next line.		*/

	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 5;								/* Update pointer.			*/
                                                                                     
	memcpy(l_work_area, num_line_1, image_rec_size);				/* Put in the first number line.	*/
	l_work_area += (image_rec_size + 1);						/* Update pointer.			*/
                                    
	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 4;								/* Update pointer.  Starts at new line.	*/

	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 5;								/* Update pointer.			*/

	memcpy(l_work_area, num_line_2, image_rec_size);				/* Put in the second number line.	*/
	l_work_area += (image_rec_size + 1);						/* Update pointer.			*/

	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 4;								/* Update pointer.  Starts at new line.	*/

	memset(l_work_area, '*', bordered_line_length);					/* Insert a full line.			*/
	l_work_area += bordered_line_length;						/* Update pointer.			*/

	memcpy(l_work_area, "*  *", 4);							/* Insert this string.			*/
	l_work_area += (image_rec_size + 6);			 			/* Update pointer.			*/
	memcpy(l_work_area, "*  *", 4);							/* Insert this string.			*/
	l_work_area += 4;								/* Update pointer.  Starts a new line.	*/

	x = WS_MAX_LINES_PER_SCREEN;							/* Set the iteration count.		*/
	y = 1;										/* First row of output.			*/
	do
	{                                     
		z = 0;                        						/* Initialize z.			*/
		z = y % 10;								/* Divide row number by 10.		*/

		memcpy(l_work_area, "*", 1);  						/* Insert one asterisk.			*/
		l_work_area++;								/* Update pointer.			*/

		if (z)									/* Is there a remainder ?		*/
		{
			sprintf(l_work_area, " %d", z);					/* Yes.  Convert remainder value.	*/
		}
		else
		{       
			sprintf(l_work_area, "%d", y);					/* Even division.  Convert row value.	*/
		}
		l_work_area += 2;							/* Skip two positions.			*/

		memcpy(l_work_area, "*", 1);  						/* Insert one asterisk.			*/
		l_work_area += 2;							/* Skip two positions.			*/

		memcpy(l_work_area, l_screen_image, image_rec_size);			/* Move one line from the screen image.	*/
		l_work_area += (image_rec_size + 1);					/* Update by line length plus one space	*/
		l_screen_image += screen_rec_size;                             		/* Point to next line in screen image.	*/
                 
		memcpy(l_work_area, "*", 1);						/* Insert one asterisk.			*/
		l_work_area++;								/* Point to next character.		*/

		if (z)									/* Is there a remainder value ?		*/
		{
			sprintf(l_work_area, " %d", z);					/* Yes, convert that dec. value.	*/
		}
		else
		{
			sprintf(l_work_area, "%d", y);					/* No.  Convert row value.		*/
		}
		l_work_area += 2;							/* Skip two characters.			*/

		memcpy(l_work_area, "*", 1);						/* Insert one asterisk.			*/
		l_work_area++;								/* Point to next character.		*/

		x--;									/* Done with one row.			*/
		y++;									/* Up row count.			*/
 	} while (x);									/* Done with all rows ?			*/

	memcpy(l_work_area, "*  *", 4);							/* Insert this string.			*/
	l_work_area += (image_rec_size + 6);						/* Update pointer.			*/
	memcpy(l_work_area, "*  *", 4);							/* Insert this string.			*/
	l_work_area += 4;								/* Update pointer.  Starts a new line.	*/

	memset(l_work_area, '*', bordered_line_length);           			/* Initialize one line to '*'		*/
	l_work_area += bordered_line_length;						/* Point to the next line.		*/

	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 5;								/* Update pointer.			*/
                                                                                     
	memcpy(l_work_area, num_line_1, image_rec_size);				/* Put in the first number line.	*/
	l_work_area += (image_rec_size + 1);						/* Update by line length plus one space	*/

	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 4;								/* Update pointer.  Starts at new line.	*/

	memset(l_work_area, '*', 4);  							/* Insert 4 asterisks.			*/
	l_work_area += 5;								/* Update pointer.			*/

	memcpy(l_work_area, num_line_2, image_rec_size);				/* Put in the second number line.	*/
	l_work_area += (image_rec_size + 1);						/* Update by line length plus one space	*/

	memset(l_work_area, '*', 4);							/* Insert 4 asterisks.			*/
	l_work_area += 4;								/* Update pointer.  Starts at new line.	*/

	memset(l_work_area, '*', bordered_line_length);					/* Insert a full line of asterisks.	*/
}