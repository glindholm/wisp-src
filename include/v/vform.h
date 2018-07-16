/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*			    Copyright (c) 1993				*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

#ifndef VFORM_INCLUDED
#define VFORM_INCLUDED

struct video_form_field
{
	int row;
	int column;
	int length;
	int type;
	int datatype;
	int number;
	int decimal_places;
	int init_processing_lines;
	int field_processing_lines;
	int finish_processing_lines;
	int init_data_chars;
	int rendition;
	int secure;
	int error_code;
	char field_name[HP_FORM_NAME_SIZE+1];
};

struct video_form_line
{
	int row;
	int column;
	int length;
	int type;
};

struct video_form
{
	int version_number;
	int control;
	int start_row;
	int clear_screen;
	int keys_on;
	int fields;
	int lines;
	int text_rows;
	int default_rendition;
	int window_line;
	int window_rendition;
	int error_rendition;
	int repeatapp;
	int freezapp;
	int dbuflen;
	char name[HP_FORM_NAME_SIZE];
	char filler1;
	char next_form[HP_FORM_NAME_SIZE];
	char filler2;
	char text[MAX_LINES_PER_SCREEN][82];
	struct video_form_field field[MAX_FORM_FIELDS];
	struct video_form_line line[MAX_FORM_LINES];
};

#endif /* VFORM_INCLUDED */
