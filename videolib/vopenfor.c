			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vintdef.h"									/* Include the COBOL interface defs.	*/
#include "vplus.h"									/* Include the view database.		*/
#include "vform.h"									/* Include the video forms database.	*/
#include "vlocal.h"									/* Include the video internal data.	*/
#include "vdata.h"									/* Include the video database.		*/

#define HT '\t'											/* Define horizontal tab.	*/
#define SP ' '											/* Define space character.	*/
#define NUL '\000'
#define MAXBUF 128
#define YES 1
#define NO 0

static int fdfirst = TRUE;
static int wfirst = TRUE;
static char *vfp;										/* Processing data index.	*/
static char *vid;										/* Initialization data index.	*/

static int get_line();
static int set_rendition();
static int get_proc();
static int trim();
static int set_init();

/*						Subroutine entry point.								*/

void VOPENFORMF(comarea,formfile) struct vplus_comarea *comarea; unsigned char *formfile;
{
	int i,j;										/* Working registers.		*/
	unsigned char ff[MAXBUF];

	vtrimlen(ff,formfile,8);								/* Decode the form file name.	*/

	if (!vloadfd(ff))									/* Read the form in.		*/
	{
		vre_window("VOPENFORMSF: Unable to open the forms file '%s'", ff);		/* Report the problem.		*/
		vexit(); exit(0);
	}

	for (i = 0; i < 1920; i++) vform_data_buffer[i] = ' ';					/* Initialize the data buffer.	*/

	comarea->lastkey = 0;									/* Now do the default return	*/
	comarea->numerrs = 0;									/*  values for this function.	*/
	comarea->recnum = 0;
	comarea->dbuflen = 0;
	comarea->cmode = COLLECT_MODE;
	comarea->repeatapp = NO_REPEAT_OR_APPEND;
	comarea->freezapp = CLEAR_CURRENT_FORM;
	comarea->printfilenum = 0;
	comarea->deleteflag = FALSE;
	comarea->cstatus = 0;
	comarea->filerrnum = 0;
	vputlen(comarea->nfname,head_form_name,HP_FORM_NAME_SIZE);			/* Send back the head form.	*/
	vputlen(comarea->cfname,"",HP_FORM_NAME_SIZE);
	comarea->windowenh = vrentoenh(vformdata->window_rendition);			/* Set the enhancement.		*/

	current_form = 0;								/* Start at the first form.	*/
	vformcurrent = vformdata;							/* Point to the first form.	*/
	vsetnf(comarea->nfname);							/* Set the current form number.	*/

	return;
}

vloadfd(ff) unsigned char *ff;
{
	FILE *fid, *fopen();
	unsigned char line_buffer[MAXBUF];
	unsigned char *ptr;
	int i,j,f;
	int r,c,n,nf,ddr,eer,mwl,wer;
	unsigned char m,t;
	unsigned char name[80], tag[80];
	int dummy;
	struct video_form *vcform, *malloc();
#ifdef unix
	char *getenv();
#endif

#ifdef unix
	if (ptr = (unsigned char *)getenv("VIDEOFORMS"))								/* Is VIDEOFORMS environment	*/
	{											/*    variable defined?		*/
		strcpy(line_buffer,ptr);							/* Yes, then load the pointer.	*/
		strcat(line_buffer,"/");							/* Add a slash.			*/
		strcat(line_buffer,ff);								/* Add in the file name.	*/
	}
	else strcpy(line_buffer,ff);								/* No environment variable.	*/
#else
	strcpy(line_buffer,ff);									/* Copy the file name.		*/
#endif
	fid = fopen(line_buffer,"r");								/* Open the forms file.		*/
	if (fid == NULL)									/* Did it work?			*/
	{
		strcat(line_buffer,".frm");							/* Try adding the extension.	*/
		fid = fopen(line_buffer,"r");							/* Try again.			*/
		if (fid == NULL) return(FAILURE);						/* All ok?			*/
	}

	get_line(fid,line_buffer,YES);								/* Get the forms file version.	*/
	if (strcmp(line_buffer,"Video View Forms Control File V4.01"))				/* Is this a valid file?	*/
	{
		vre_window("vloadfd: Invalid forms file format or version number.");
		vexit(); exit();
	}

	get_line(fid,line_buffer,YES);								/* Get number of forms in file.	*/
	sscanf((char *)line_buffer,"%d",&nf);							/* Decode.			*/

	if ((nf < 1) || (nf > MAX_NUMBER_OF_FORMS))						/* Is this ok?			*/
	{
		verase(FULL_SCREEN);
		vre_window("VOPENFORMSF: %d is an invalid number of forms.",nf);		/* Report the error.		*/
		vexit(0); exit(0);								/* Exit with an error.		*/
	}

	if (fdfirst)
	{
		fdfirst = FALSE;								/* Not first time anymore.	*/
		vformdata = NULL;								/* Null out the pointer.	*/
	}

	if (vformdata != NULL)									/* Already allocated?		*/
	{
		verase(FULL_SCREEN);
		vre_window("VOPENFORMSF: A form file is already open.");			/* Already open...		*/
		vexit(0); exit(0);
	}

	i = sizeof(struct video_form) * nf;
	vformdata = malloc(sizeof(struct video_form) * nf);					/* Allocate memory.		*/
	vformproc = (char *)malloc(MAX_PROCESSING_CHARS);
	vfp = vformproc;
	vinitdata = (char *)malloc(MAX_INIT_CHARS);
	vid = vinitdata;

	if ((vformdata == NULL) || (vformproc == NULL) || (vinitdata == NULL))			/* Did we get it?		*/
	{
		verase(FULL_SCREEN);
		vre_window("VOPENFORMS: Unable to allocate sufficient memory for forms data.");
		vexit(0); exit(0);
	}

	get_line(fid,line_buffer,YES);								/* Get the head form.		*/
	vputlen(head_form_name,line_buffer,HP_FORM_NAME_SIZE);					/* Remember it for us.		*/

	get_line(fid,line_buffer,YES);								/* Get the default rendition.	*/
	set_rendition(line_buffer,BOLD,&ddr,&dummy);						/* Set the rendition.		*/

	get_line(fid,line_buffer,YES);								/* Get the error rendition.	*/
	set_rendition(line_buffer,BOLD,&eer,&dummy);						/* Set the rendition.		*/

	get_line(fid,line_buffer,YES);								/* Get the window line.		*/
	sscanf((char *)line_buffer,"%d",&mwl);							/* Decode.			*/
	mwl = mwl - 1;										/* Convert to video coordinates	*/

	get_line(fid,line_buffer,YES);								/* Get the window enhancement.	*/
	set_rendition(line_buffer,BOLD,&wer,&dummy);						/* Set the rention too.		*/

	for (f = 0, vcform = vformdata; f < nf; f++, vcform++)					/* Loop through every form.	*/
	{
		vcform->default_rendition = ddr;						/* Set the default rendition.	*/
		vcform->error_rendition = eer;							/* Set the error rendition.	*/
		vcform->window_rendition = wer;							/* Set the window renditon.	*/
		vcform->window_line = mwl;							/* Set the window line.		*/

		vcform->start_row = 0;								/* Assume the start row is 0.	*/
		if (wer == 0) vcform->start_row = 1;						/* No, then it is 1.		*/

		get_line(fid,line_buffer,YES);							/* Get the form name.		*/
		vputlen(vcform->name,line_buffer,HP_FORM_NAME_SIZE);				/* Save it.			*/

		get_line(fid,line_buffer,YES);							/* Get the repeat option.	*/
		sscanf((char *)line_buffer,"%c",&t);						/* Decode.			*/
		if (t == 'N') vcform->repeatapp = 0;						/* Convert to vplus internal.	*/
		else if (t == 'R') vcform->repeatapp = 1;
		else if (t == 'A') vcform->repeatapp = 2;
		else
		{
			verase(FULL_SCREEN);
			vre_window("VOPENFORMF: %c is an invalid repeat option character.",t);
		}

		get_line(fid,line_buffer,YES);							/* Get the next form opt.	*/
		sscanf((char *)line_buffer,"%c",&t);						/* Decode.			*/
		if (t == 'C') vcform->freezapp = 0;						/* Convert to vplus internal.	*/
		else if (t == 'A') vcform->freezapp = 1;
		else if (t == 'F') vcform->freezapp = 2;
		else
		{
			verase(FULL_SCREEN);
			vre_window("VOPENFORMF: %c is an invalid next form option character.",t);
		}
		
		get_line(fid,line_buffer,YES);							/* Get the next form name.	*/
		vputlen(vcform->next_form,line_buffer,HP_FORM_NAME_SIZE);			/* Save it.			*/

		get_line(fid,line_buffer,YES);							/* Get the help key status.	*/
		if (!memcmp(line_buffer,"ON",2)) vcform->keys_on = TRUE;			/* Turn the keys on.		*/
		else vcform->keys_on = FALSE;

		get_line(fid,line_buffer,YES);							/* Get the form text lines.	*/
		sscanf((char *)line_buffer,"%d",&vcform->text_rows);				/* Get the number of lines.	*/

		for (i = 0; i < vcform->text_rows; i++)						/* Loop through each line.	*/
		{
			get_line(fid,line_buffer,NO);						/* Get a line of background.	*/
			strcpy(vcform->text[i],line_buffer);					/* Store it away.		*/
		}

		get_line(fid,line_buffer,YES);							/* Get number of graphic lines.	*/
		sscanf((char *)line_buffer,"%d",&vcform->lines);				/* Decode.			*/

		for (i = 0; i < vcform->lines; i++)						/* Read in each line.		*/
		{
			get_line(fid,line_buffer,YES);						/* Get the line.		*/
			sscanf((char *)line_buffer,"%c %d %d %d",&t,&r,&c,&n);			/* Decode.			*/
			if (t == 'H') vcform->line[i].type = HORIZONTAL;			/* Select the line type.	*/
			else if (t == 'h') vcform->line[i].type = HORIZONTAL;			/* Upper case is "double" line.	*/
			else if (t == 'V') vcform->line[i].type = VERTICAL;			/* Lower case is "single" line.	*/
			else if (t == 'v') vcform->line[i].type = VERTICAL;
			vcform->line[i].row = r-1;						/* Set the row.			*/
			vcform->line[i].column = c-1;						/* Set the column.		*/
			vcform->line[i].length = n;
		}

		get_line(fid,line_buffer,YES);							/* Get the number of fields.	*/
		sscanf((char *)line_buffer,"%d",&vcform->fields);				/* Decode.			*/
		vcform->dbuflen = 0;								/* Start buffer length at 0.	*/
		vcform->clear_screen = TRUE;							/* Assume screen to be cleared.	*/

		for (i = 0; i < vcform->fields; i++)						/* Read in each field.		*/
		{
			get_line(fid,line_buffer,YES);							/* Get the line.	*/
			sscanf((char *)line_buffer,"%s %s %d %d %d %d %c",name,tag,&j,&r,&c,&n,&t);	/* Decode.		*/

			vcform->field[i].number = j;							/* Field number.	*/

			for (j = 0; j < HP_FORM_NAME_SIZE; j++) vcform->field[i].field_name[j] = ' ';	/* Blank it out.	*/
			vcform->field[i].field_name[HP_FORM_NAME_SIZE] = CHAR_NULL;			/* Null terminate.	*/
			for (j = 0; (j < HP_FORM_NAME_SIZE) && (tag[j] > ' '); j++)			/* Copy the tag.	*/
			{
				vcform->field[i].field_name[j] = tag[j];				/* Copy a char.		*/
			}

			vcform->field[i].type = FIELD_DISPLAY_ONLY;				/* Assume display only.		*/
			if (t == 'R') vcform->field[i].type = FIELD_REQUIRED;			/* Make read/write.		*/
			else if (t == 'O') vcform->field[i].type = FIELD_OPTIONAL;
			else if (t == 'P') vcform->field[i].type = FIELD_PROCESSED;
			vcform->field[i].row = r-1;						/* Set the row.			*/
			vcform->field[i].column = c-1;						/* Set the col ERROR CASE 	*/
			vcform->field[i].length = n;						/* Set the length.		*/
			vcform->dbuflen = vcform->dbuflen + n;					/* Add length of this field.	*/
			vcform->field[i].error_code = 0;					/* No errors yet.		*/

			set_init(line_buffer,&vcform->field[i].init_data_chars);		/* Insert the init data.	*/

			get_line(fid,line_buffer,YES);						/* Get the field enhancements.	*/
			set_rendition(line_buffer,ddr,&vcform->field[i].rendition,&vcform->field[i].secure);

			get_line(fid,line_buffer,YES);						/* Get the data type.		*/
			vcform->field[i].datatype = 0;						/* Default it.			*/
			vcform->field[i].decimal_places = 0;
			if      (!memcmp("CHAR",line_buffer,4)) vcform->field[i].datatype = FIELD_DATA_CHAR;
			else if (!memcmp("MDY",line_buffer,3))	vcform->field[i].datatype = FIELD_DATA_MDY;
			else if (!memcmp("DMY",line_buffer,3))	vcform->field[i].datatype = FIELD_DATA_DMY;
			else if (!memcmp("YMD",line_buffer,3))	vcform->field[i].datatype = FIELD_DATA_YMD;
			else if (!memcmp("DIG",line_buffer,3))	vcform->field[i].datatype = FIELD_DATA_DIG;
			else if (!memcmp("NUM",line_buffer,3))
			{
				vcform->field[i].datatype = FIELD_DATA_NUM;
				if      (!memcmp("NUM0",line_buffer,4)) vcform->field[i].decimal_places = 0;
				else if (!memcmp("NUM1",line_buffer,4)) vcform->field[i].decimal_places = 1;
				else if (!memcmp("NUM2",line_buffer,4)) vcform->field[i].decimal_places = 2;
				else if (!memcmp("NUM3",line_buffer,4)) vcform->field[i].decimal_places = 3;
				else if (!memcmp("NUM4",line_buffer,4)) vcform->field[i].decimal_places = 4;
				else if (!memcmp("NUM5",line_buffer,4)) vcform->field[i].decimal_places = 5;
				else if (!memcmp("NUM6",line_buffer,4)) vcform->field[i].decimal_places = 6;
				else if (!memcmp("NUM7",line_buffer,4)) vcform->field[i].decimal_places = 7;
				else if (!memcmp("NUM8",line_buffer,4)) vcform->field[i].decimal_places = 8;
				else if (!memcmp("NUM9",line_buffer,4)) vcform->field[i].decimal_places = 9;
				else vcform->field[i].decimal_places = -1;
			}
			else if (!memcmp("IMP",line_buffer,3))
			{
				vcform->field[i].datatype = FIELD_DATA_IMP;
				if      (!memcmp("IMP0",line_buffer,4)) vcform->field[i].decimal_places = 0;
				else if (!memcmp("IMP1",line_buffer,4)) vcform->field[i].decimal_places = 1;
				else if (!memcmp("IMP2",line_buffer,4)) vcform->field[i].decimal_places = 2;
				else if (!memcmp("IMP3",line_buffer,4)) vcform->field[i].decimal_places = 3;
				else if (!memcmp("IMP4",line_buffer,4)) vcform->field[i].decimal_places = 4;
				else if (!memcmp("IMP5",line_buffer,4)) vcform->field[i].decimal_places = 5;
				else if (!memcmp("IMP6",line_buffer,4)) vcform->field[i].decimal_places = 6;
				else if (!memcmp("IMP7",line_buffer,4)) vcform->field[i].decimal_places = 7;
				else if (!memcmp("IMP8",line_buffer,4)) vcform->field[i].decimal_places = 8;
				else if (!memcmp("IMP9",line_buffer,4)) vcform->field[i].decimal_places = 9;
				else
				{
					vstate(0);
					verase(FULL_SCREEN);
					vre_window("VOPENFORMF: Invalid IMP data type in form");
					vexit(0);
					exit();
				}
			}
			else
			{
				verase(FULL_SCREEN);
				vre_window("VOPENFORMF: %s is an invalid data type.",line_buffer);
				exit(0);
			}

			get_proc(fid,line_buffer,vcform,i,&vcform->field[i].init_processing_lines);
			get_proc(fid,line_buffer,vcform,i,&vcform->field[i].field_processing_lines);
			get_proc(fid,line_buffer,vcform,i,&vcform->field[i].finish_processing_lines);
		}
	}

	fclose(fid);										/* Close the file, all loaded.	*/

	form_count = nf;									/* Remember number of forms.	*/

	if (wfirst)										/* Is this the first time?	*/
	{
		wfirst = FALSE;									/* Don't initialize again.	*/
		for (i = 0; i < 80; i++ ) window_message[i] = ' ';				/* Blank out the window msg.	*/
		window_message[80] = CHAR_NULL;							/* Null terminate.		*/
		highest_line_written = -1;							/* Remember the highest line.	*/
	}

	return(SUCCESS);
}

static int get_line(in, buffer, compress) FILE *in; unsigned char buffer[]; int compress;
{
	int i;
	unsigned char *bufptr;
	unsigned char c;

	i = 0;
	bufptr = &buffer[0];
	while ((*bufptr = getc(in)) != EOF)							/* Repeat until end of file.	*/
	{
		if ((*bufptr == '\n') || (*bufptr == '\014'))					/* Is this end of line?		*/
		{
			c = *bufptr;
			*bufptr = CHAR_NULL;							/* Store a null.		*/
			trim(buffer,i,compress);						/* Trim the string.		*/
			return(SUCCESS);
		}
		else 										/* And don't go beyond the end.	*/
		{
			bufptr++;
			i++;
		}
	}
	return(FAILURE);
}

static int trim(string,len,to_be_compressed) char string[]; int len;				/* Trim trailing blanks.	*/
{
	int i;
	for (i = len-1; (i >= 0) && ((string[i] == SP) || (string[i] == HT)); i--);		/* Trim it all up.		*/
	string[i+1] = NUL;
	if (!to_be_compressed) return(SUCCESS);
	while ((string[0] == ' ') || (string[0] == '\t')) strcpy(string,&string[1]);		/* Trim leading whitespace.	*/
	return(SUCCESS);									/* Return string length.	*/
}

int vrentoenh(ren) int ren;									/* Convert rendition to enh.	*/
{
	unsigned char a[4];									/* Character array.		*/
	int i,enh;										/* Working register.		*/
	
	enh = 0;										/* Assume no enhancement.	*/

	if (ren & REVERSE) enh = enh | HP_INVERSE;						/* Add reverse?			*/
	if (ren & UNDERSCORE) enh = enh | HP_UNDERLINE;						/* Add underscore?		*/
	if (ren & BLINK) enh = enh | HP_BLINK;							/* Add blinking.		*/
	if (!(ren & BOLD)) enh = enh | HP_HALF_BRIGHT;						/* Add half bright.		*/

	if (enh) enh = enh | HP_ENHANCEMENT;							/* Make it a char.		*/

	return(enh);										/* Return the value.		*/
}

static int set_rendition(a,initial,rendition,security) char a[]; int initial, *rendition, *security;
{
	register int i;										/* Working register.		*/

	*rendition = initial;									/* Set an initial value.	*/
	for (i = 0; (i < 4) && (a[i] != CHAR_NULL); i++)					/* Do they specify any other?	*/
	{
		if ((a[i] == 'I') || (a[i] == 'U') || (a[i] == 'H') || (a[i] == 'B')) *rendition = BOLD;
	}

	for (i = 0; (i < 4) && (a[i] != CHAR_NULL); i++)					/* Add in other renditions.	*/
	{
		if      (a[i] == 'I') *rendition = *rendition | REVERSE;			/* Add reverse?			*/
		else if (a[i] == 'U') *rendition = *rendition | UNDERSCORE;			/* Add underscore?		*/
		else if (a[i] == 'H') *rendition = *rendition & ~BOLD;				/* Remove bold?			*/
		else if (a[i] == 'B') *rendition = *rendition | BLINK;				/* Add blinking.		*/
		else if (a[i] == 'N') *rendition = BOLD;					/* Clear it out.		*/
		else if (a[i] == 'S')								/* Secure (non-echo) field?	*/
		{
			*security = TRUE;							/* It is secure.		*/
			*rendition = CLEAR;							/* Clear the rendition.		*/
		}
	}
}

static int get_proc(fid,line_buffer,vcform,f,pl) FILE *fid; char *line_buffer; struct video_form *vcform; int f, *pl;
{
	register int i,j;									/* Working registers.		*/

	get_line(fid,line_buffer,YES);								/* Get a line from the file.	*/
	sscanf((char *)line_buffer,"%d",pl);							/* Decode the number of lines.	*/
	for (j = 0; j < *pl; j++)								/* Loop until all read.		*/
	{
		get_line(fid,line_buffer,NO);							/* Get a processing line.	*/
		i = strlen(line_buffer);							/* Get the length of the data.	*/
		if ((vfp + i) > (vformproc + MAX_PROCESSING_CHARS))				/* Room for this data?		*/
		{
			vre_window("VOPENFORMF: Insufficient memory to load processing data.");	/* Report the error.		*/
			vexit(0); exit(0);							/* Fatal, so exit.		*/
		}
		strcpy(vfp, line_buffer);							/* Copy the data.		*/
		vfp = vfp + i + 1;								/* Move to the next free loc.	*/
	}
}

static int set_init(lb,ic) char *lb; int *ic;							/* Store initialization data.	*/
{
	int i,j;										/* Working ints.		*/
	char *wp;										/* Working pointers.		*/

	wp = lb;										/* Point to the line buffer.	*/
	for (i = 0; i < 6; i++)									/* Move past the leaders.	*/
	{
		while ((*wp != ' ') && (*wp != '\t')) wp++;					/* Move past the field name.	*/
		while ((*wp == ' ') || (*wp == '\t')) wp++;					/* Move past the delimiter.	*/
	}

	wp++;											/* Move past the field type.	*/
	if (*wp != ' ')										/* A space means init data.	*/
	{
		*ic = 0;									/* Nothing in init.		*/
		return;
	}

	wp++;											/* Move to start of init data.	*/
	*ic = strlen(wp);									/* Get the string length.	*/
	for (i = 0; i < *ic; i++)								/* Anything to put in init?	*/
	{
		*vid++ = *wp++;									/* Store the char.		*/
		if (vid >= (vinitdata + MAX_INIT_CHARS - 2))					/* Enough room?			*/
		{
			vre_window("VOPENFOR: Insufficient memory to load field initialization data.");
			vexit(); exit();
		}
	}

	if (*ic) *vid++ = CHAR_NULL;
}
