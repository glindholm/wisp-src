//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : crt_io.hpp
// Author : George Soules
// Date   : 27 May 1992

#ifndef CRT_IO__HPP
#define CRT_IO__HPP

// Classes
// (none)
#include "object.hpp"

// Definitions and subprograms
#include "colors.hpp"
#include "environ.hpp"

// NOTE: The ATTR_xxxx's are not a clean bit pattern
//	 ATTR_NORMAL is mutually exclusive with ATTR_UNDER_DIM and ATTR_UNDER_BRIGHT.
#define ATTR_BLINK          0x80   	// 1000 0000
#define ATTR_BRIGHT         0x08   	// 0000 1000
#define ATTR_NORMAL         0x07	// 0000 0111
#define ATTR_REVERSE        0x70	// 0111 0000
#define ATTR_UNDER_DIM      0x01	// 0000 0001
#define ATTR_UNDER_BRIGHT   0x09	// 0000 1001

#define CURSOR_SHAPE_NORMAL 0XD0E0
#define CURSOR_SHAPE_INSERT 0X80FF

struct crt_info {
    usign_8 left;
    usign_8 top;
    usign_8 right;
    usign_8 bottom;
};

void clear_screen();

void clear_to_eol();

Boolean color_monitor();

int cursor_shape();

void cursor_visible(Boolean visible);

void define_window(int left, int top, int right, int bottom);

void draw_box(
   int         style,
   int         left,
   int         top,
   int         right,
   int         bottom,
   Boolean     erase,
   const char *title,
   colors      line_colors,
   colors      title_colors);

void get_crt_info(struct crt_info *crt);

int get_key_pressed();

void get_region(int left, int top, int right, int bottom, usign_8 *&a_region);

void get_screen_dimensions(int &rows, int &cols);

void goto_xy(int x, int y);

Boolean in_graphics_mode();

Boolean key_pressed(int waittime);

void normal_video();

#if DOS || DOS_HOST
void put_region(int left, int top, int right, int bottom, usign_8 *a_region);
#else
void put_region(usign_8 *a_region);
#endif

void put_text(int column, int row, const char *text, usign_8 attribute);

void put_text(const char *text);

char *read_stdin(int chars);

void restore_full_screen_state(Boolean hard_restore = true);

void restore_video_state();

void ring_bell();

void save_full_screen_state();

void set_cursor_shape(int a_shape);

void text_background(color a_color);

void text_foreground(color a_color);

void use_standard_io();

int where_x();

int where_y();

void write_stdout(const char* s);

//	The W4W handler class is used to handle all aspects of mouse support.
//	The main thing it does is maintain a screen image buffer for locating the hotspots
class w4w_handler : public object
{
	public:
	w4w_handler();
	~w4w_handler();

	// Methods for maintaining the screen text buffer
	void clear_image();
	void put_text(unsigned int row, unsigned int col, const char* text);
	void put_text(unsigned int row, unsigned int col, const char* text, unsigned int len);

	Boolean w4w(); 				// Is W4W processing being done ?
	int costar();				// Is COSTAR specific processing being done?

	void enable_mouse();
	void disable_mouse();
	int get_mouse_position(int* row, int* col);

	int mark_pfkey_tags();			// Mark the pfkey tags on the screen
	int pfkey_tag_at(int row, int col);	// Check if there is a tag where the mouse was clicked

	private:

	char buff[SCREEN_HEIGHT][SCREEN_WIDTH];
};

// There is only one W4W handler and it is global and always exists.
extern w4w_handler global_w4w_handler;

#endif


//
//	History:
//	$Log: crt_io.hpp,v $
//	Revision 1.8  1998/10/12 16:13:02  gsl
//	FIxed w4w_handler problems on DGUX
//	
//	Revision 1.7  1998-10-02 15:36:46-04  gsl
//	Add w4w_handler class to handle all aspects of W4W processing
//
//	Revision 1.6  1998-08-31 15:13:38-04  gsl
//	drcs update
//
//

//	
//	RCS file: /disk1/neomedia/RCS/wisp/wproc/crt_io.hpp,v
//	Working file: crt_io.hpp
//	head: 1.5
//	branch:
//	locks: strict
//	access list:
//		gsl
//		scass
//		ljn
//		jockc
//		jlima
//	symbolic names:
//	keyword substitution: kv
//	total revisions: 5;	selected revisions: 5
//	description:
//	----------------------------
//	revision 1.5
//	date: 1996-07-25 19:45:43-04;  author: gsl;  state: V4_3_00;  lines: +0 -2
//	NT
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:45-04;  author: gsl;  state: V3_3_19;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:01-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:41-05;  author: gsl;  state: V3_3x12;  lines: +6 -6
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:00-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
