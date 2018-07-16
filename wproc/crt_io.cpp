//
//	Copyright (c) 1996-1998 NeoMedia Technologies Inc. All rights reserved.
//
//	Project:	WPROC
//	Id:		$Id:$
//	RCS:		$Source:$
//	
//// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : crt_io.cpp
// Author : George Soules
// Date   : 27 May 1992

#if DOS || DOS_HOST
// Tell compiler there is inline assembler in this unit
#pragma inline
#endif

// Specification
#include "crt_io.hpp"

// Classes
#include "process.hpp"
#include "screen.hpp"

// Definitions and subprograms
#include "assert.hpp"
#if DOS || DOS_HOST
#include "conio.h"
#include <dos.h>
#endif
#if UNIX
#include <unistd.h>
#endif
#if WIN32
#include <io.h>
#endif
#if WANG
#include <stdio.h>
#include <stdlib.h>
#include "video_rt.h"
#include "wisp_rts.h"
#endif
#include "memory.hpp"
#include "utility.hpp"
#include "process.hpp"

extern "C" {
#include "costar.h"
}

#if DOS || DOS_HOST
// Spontaneous Assembly routines
extern "C" void console_init(void);
extern "C" void get_cursor(void);
extern "C" void get_dmode(void);
extern "C" void get_vmode(void);
extern "C" void get_str(void);
extern "C" void put_str(void);
extern "C" void set_cursor(void);

// Spontaneous Assembly global variables
extern unsigned char screen_cols;
extern unsigned char screen_rows;

extern int _wscroll = 0;  // Borland global prevents unwanted scrolling
#endif

#if WANG
extern "C" void vwang_wang2term(unsigned char *new_text, int len);
extern "C" void vwang_term2wang(unsigned char *new_text, int len);
extern "C" void vwang_wang2ansi(unsigned char *new_text, int len);
extern "C" void vwang_ansi2wang(unsigned char *new_text, int len);
extern "C" void vwang_load_charmap(int force);
extern "C" const char *get_wisp_option(const char *option);
static int charmap_loaded = 0;
static int nativecharmap = 0;
static screen_contents *saved_screen = NULL;
#if ! DOS_HOST
static crt_info active_window;
static Boolean  using_full_screen_io = false;
static color    background_color     = color_black;
static color    foreground_color     = color_white;
static int      default_attribute    = CLEAR;
#endif
#endif


w4w_handler global_w4w_handler;  // The global W4W handler object for handling all mouse and costar functions.


#if WANG && ! DOS_HOST
void filtered_vtext(int attr, int row, int column, const char *text) {
	int i = 0;

   	char *new_text = dup_string(text);

	if (!charmap_loaded)
	{
		charmap_loaded = 1;
		vwang_load_charmap(0);
		nativecharmap = (get_wisp_option("NATIVECHARMAP"))?1:0;
	}

	if (nativecharmap)
	{
		vwang_ansi2wang((unsigned char *)new_text, (int)strlen(new_text)); 
	}
	vwang_wang2term((unsigned char *)new_text, (int)strlen(new_text)); 

	for(i=0; new_text[i]; i++)
	{
		// new_text is signed so 8-bit chars will be negative
		if (new_text[i] >= 0 && new_text[i] < ' ') 
		{
			new_text[i] = '.';
		}
	}

	vtext(attr, row, column, new_text);

	delete_string(new_text);
}


void use_full_screen_io() {
   static int first_call = true;

   active_window.left   = 1;
   active_window.top    = 1;
   active_window.right  = SCREEN_WIDTH;
   active_window.bottom = SCREEN_HEIGHT;

   if (! using_full_screen_io) {
	if (wbackground())
	{
		//	Got a problem, can't do screen IO in background!
		char	buff[512];

		sprintf(buff,"%%WPROC-F-SCREENIO Screen IO was attempted in background\nWPROC Input file = %s",
			the_process->the_input_pathname);
		werrlog(102,buff);
		exit(1);
	}
	using_full_screen_io = true;
	vstate(DEFAULT);

      if (first_call) {
         // Erase entire screen so video can establish a screen map, otherwise
         // it won't know about the whole screen and cursor won't move properly
         verase(FULL_SCREEN);
         first_call = false;
      }
   }
}
#endif


void clear_screen() {
#if DOS || DOS_HOST
   clrscr();
#else
#if WANG
   if (! using_full_screen_io)
      use_full_screen_io();
   default_attribute = VMODE_CLEAR;
   if (foreground_color == color_black && background_color == color_white)
      default_attribute = VMODE_REVERSE;

   if (active_window.left == 1 && active_window.right == SCREEN_WIDTH &&
       active_window.top == 1 && active_window.bottom == SCREEN_HEIGHT &&
       default_attribute == VMODE_CLEAR)
   {
      verase(FULL_SCREEN);
      global_w4w_handler.clear_image();
   }
   else {
      char blanks[SCREEN_WIDTH + 1];
      init_string_with_blank_pad(blanks, SCREEN_WIDTH);
      blanks[active_window.right - active_window.left + 1] = '\0';
      for (int i = active_window.top; i <= (int)(active_window.bottom); i++)
      {
         vtext(default_attribute, i - 1, active_window.left - 1, blanks);
	 global_w4w_handler.put_text(i-1, active_window.left -1, blanks);
      }
   }
   vmove(active_window.top - 1, active_window.left - 1);
#endif
#endif
}


void clear_to_eol() {
#if DOS || DOS_HOST
   clreol();
#else
#if WANG
   verase(TO_EOL);
#endif
#endif
}


Boolean color_monitor() {
#if DOS
   int mode;
   asm call far ptr get_dmode
   mode = _AL;
   return BOOLEAN(mode >= 4);
#else
   return false;
#endif
}


int cursor_shape() {
#if DOS
   asm call far ptr console_init
   asm call far ptr get_cursor
   return _AX;
#else
   return 1;
#endif
}


void cursor_visible(Boolean visible) {
#if DOS || DOS_HOST
   _setcursortype(visible ? _NORMALCURSOR : _NOCURSOR);
#else
#if WANG
   if (wbackground()) return;
   if (! using_full_screen_io)
      use_full_screen_io();
   if (visible)
	vset_cursor_on();
   else
	vset_cursor_off();
#endif
#endif
   the_process->cursor_off = BOOLEAN(visible == false);
}


void define_window(int left, int top, int right, int bottom) {
#if DOS || DOS_HOST
   window(left, top, right, bottom);
#else
#if WANG
   active_window.left   = left;
   active_window.top    = top;
   active_window.right  = right;
   active_window.bottom = bottom;
#endif
#endif
}


void draw_box(
   int         style,
   int         left,
   int         top,
   int         right,
   int         bottom,
   Boolean     erase,
   const char *title,
   colors      line_colors,
   colors      title_colors)
{
#if DOS || DOS_HOST
   int ul; // upper left corner
   int ur; // upper right corner
   int ll; // lower left corner
   int lr; // lower right corner
   int v;  // vertical line
   int h;  // horizontal line
   int i;
   int height;
   int width;

   switch (style) {
      case 0 :
      case 1 :
         break;

      case 2 :
      case 3 :
         ul = 'É'; // 201
         ll = 'È'; // 200
         ur = '»'; // 187
         lr = '¼'; // 188
         v  = 'º'; // 186
         h  = 'Í'; // 205
         break;

      case 4 :
      case 5 :
         ul = 'Ú'; // 218
         ll = 'À'; // 192
         ur = '¿'; // 191
         lr = 'Ù'; // 217
         v  = '³'; // 179
         h  = 'Ä'; // 196
         break;

      case 6 :
      case 7 :
         ul = 'Õ'; // 213
         ll = 'Ô'; // 212
         ur = '¸'; // 184
         lr = '¾'; // 190
         v  = '³'; // 179
         h  = 'Í'; // 205
         break;

      case 8 :
      case 9 :
         ul = 'Ö'; // 214
         ll = 'Ó'; // 211
         ur = '·'; // 183
         lr = '½'; // 189
         v  = 'º'; // 186
         h  = 'Ä'; // 196
         break;

      default :
         assert(UNREACHABLE);
   }

   // Set the virtual window to the outer bounds of the border
   define_window(left, top, right, bottom);

   // Erase the area behind the virtual window
   if (erase)
      clear_screen();

   height = bottom - top + 1;
   width  = right - left + 1;

   if (style > 1) {
      int wd = width * 2;
      int ht = height * 2;
      char *buffer = new char[max(wd, ht)];

      if (line_colors.background >= FIRST_LT_COLOR)
         // Disallow bright color as background
         line_colors.background =
            (color) (line_colors.background - FIRST_LT_COLOR);

      usign_8 attr =
         line_colors.foreground + (line_colors.background << 4);

      for (i = 0; i < wd; i += 2) {
         buffer[i] = h;
         buffer[i + 1] = attr;
      }
      buffer[0] = ll;
      buffer[wd - 2] = lr;
      puttext(left, bottom, right, bottom, buffer);
      buffer[0] = ul;
      buffer[wd - 2] = ur;

      if (title) {
         if (title_colors.background >= FIRST_LT_COLOR)
            // Disallow bright color as background
            title_colors.background =
               (color) (title_colors.background - FIRST_LT_COLOR);

         usign_8 attr =
            title_colors.foreground + (title_colors.background << 4);
         int len = strlen(title);
         if (len > width - 2)
            len = width - 2;
         int offset = width - len;
         if (offset % 2)
            offset -= 1;
         i = 0;
         int c = title[i];
         while (c && i < len) {
            buffer[offset++] = c;
            buffer[offset++] = attr;
            c = title[++i];
         }
      }
      puttext(left, top, right, top, buffer);

      for (i = 0; i < ht; i += 2) {
         buffer[i]     = v;
         buffer[i + 1] = attr;
      }
      top    += 1;
      bottom -= 1;

      puttext(left, top, left, bottom, buffer);
      puttext(right, top, right, bottom, buffer);

      delete buffer;
   }

#else
#if WANG
   if (! using_full_screen_io)
      use_full_screen_io();

   int width  = right - left + 1;
   int height = bottom - top + 1;

   int horizontal_line;
   int vertical_line;

   switch (style) {
      case 0 :
      case 1 :
         break;

      case 2 :
      case 3 :
      // horizontal_line = FAT_HORIZONTAL;
      // vertical_line   = FAT_VERTICAL;
         horizontal_line = HORIZONTAL;
         vertical_line   = VERTICAL;
         break;

      case 4 :
      case 5 :
         horizontal_line = HORIZONTAL;
         vertical_line   = VERTICAL;
         break;

      case 6 :
      case 7 :
      // horizontal_line = FAT_HORIZONTAL;
         horizontal_line = HORIZONTAL;
         vertical_line   = VERTICAL;
         break;

      case 8 :
      case 9 :
         horizontal_line = HORIZONTAL;
      // vertical_line   = FAT_VERTICAL;
         vertical_line   = VERTICAL;
         break;

      default :
         assert(UNREACHABLE);
   }

   Boolean cursor_on = BOOLEAN(! the_process->cursor_off);
   cursor_visible(false);

   // Set the virtual window to the outer bounds of the border
   define_window(left, top, right, bottom);

   // Erase the area behind the virtual window
   if (erase)
      clear_screen();

   if (style >= 2) {
      vmode(default_attribute);
      vmove(top - 1, left - 1);
      vline(vertical_line, height);
      vmove(top - 1, right - 1);
      vline(vertical_line, height);
      vmove(top - 1, left - 1);
      vline(horizontal_line, width);
      vmove(bottom - 1, left - 1);
      vline(horizontal_line, width);
   }

   if (title) {
      int len = strlen(title);
      if (len > width - 2)
         len = width - 2;
      char *buffer = new_string(len + 1);
      init_string_with_blank_pad(buffer, len, title);
      int offset = width - len;
      offset /= 2;
      filtered_vtext
         (default_attribute, top - 1, left + offset - 1, buffer);
      delete_string(buffer);
   }

   cursor_visible(cursor_on);
#endif
#endif
}


void get_crt_info(struct crt_info *crt) {
#if DOS || DOS_HOST
   struct text_info info;
   gettextinfo(&info);
   crt->left   = info.winleft;
   crt->top    = info.wintop;
   crt->right  = info.winright;
   crt->bottom = info.winbottom;
#else
#if WANG
   crt->left   = 1;
   crt->right  = SCREEN_WIDTH;
   crt->top    = 1;
   crt->bottom = SCREEN_HEIGHT;
#endif
#endif
}


int get_key_pressed() {
#if DOS || DOS_HOST
   int key = getch();
   if (! key)
      key = -getch(); // extended key
   return key;
#else
#if WANG
	int	the_char;

	if (! using_full_screen_io)
      		use_full_screen_io();

   	the_char = vgetm();

	if (the_char > 0 && the_char < 256)
	{
		unsigned char c;

		c = (unsigned char)the_char;
		vwang_term2wang(&c,1);

		if (nativecharmap)
		{
			vwang_wang2ansi(&c,1);
		}

		the_char = (int)c;
	}

	return the_char;
#endif
#endif
}


void get_region(int left, int top, int right, int bottom, usign_8 *&a_region) {
#if DOS || DOS_HOST
   gettext(left, top, right, bottom, a_region);
#else
#if WANG
   if (! using_full_screen_io)
      use_full_screen_io();
   a_region = vsss(top - 1, left - 1, bottom - top + 1, right - left + 1);
#endif
#endif
}


void get_screen_dimensions(int &rows, int &cols) {
#if DOS
   asm call far ptr console_init
   asm mov al,screen_rows
   rows = _AL;
   asm mov al,screen_cols
   cols = _AL;
#else
   cols = SCREEN_WIDTH;
   rows = SCREEN_HEIGHT;
#endif
}


void goto_xy(int x, int y) {
#if DOS || DOS_HOST
   gotoxy(x, y);
#else
#if WANG
   if (! using_full_screen_io)
      use_full_screen_io();
   int save_verbose = verbose;
   verbose = false;
   vmove(y + active_window.top - 2, x + active_window.left - 2);
   verbose = save_verbose;
#endif
#endif
}


Boolean in_graphics_mode() {
#if DOS || DOS_HOST
   int mode;
   asm call far ptr get_vmode
   mode = _AL;
   mode = 2;
   return BOOLEAN(! (mode == 2 || mode == 3 || mode == 7));
#else
   return false;
#endif
}


Boolean key_pressed(int waittime) {
#if DOS || DOS_HOST
   	return BOOLEAN(kbhit());
#else
#if WANG
   	if (! using_full_screen_io)
      		use_full_screen_io();

  	 char c = 0;

	if (waittime)
	{
		assert(0);  /* Waittime is not used in the WISP implementation */
	}
	else
	{
		if ((c = vcheck())) vpushc(c);
	}
   	return BOOLEAN(c != 0);
#endif
#endif
}


void normal_video() {
#if DOS || DOS_HOST
   normvideo();
#else
#if WANG
   background_color = color_black;
   foreground_color = color_white;
   vmode(VMODE_CLEAR);
   default_attribute = VMODE_CLEAR;
#endif
#endif
}


#if DOS || DOS_HOST
void put_region(int left, int top, int right, int bottom, usign_8 *a_region) {
   puttext(left, top, right, bottom, a_region);
}
#else
#if WANG
void put_region(usign_8 *a_region) {
   assert(using_full_screen_io);
   vrss(a_region);
}
#endif
#endif


void put_text(int column, int row, const char *text, usign_8 attribute) {
#if DOS || DOS_HOST
   int i = 0;
   int j = 0;
   int len = strlen(text);
   char *screen_text = new_string(len * 2);
   while (text[i]) {
      screen_text[j++] = text[i++];
      screen_text[j++] = attribute;
   }
   puttext(column, row, column + len - 1, row, screen_text);
   delete_string(screen_text);
#else
#if WANG
   assert(using_full_screen_io);
   int original_x = vcur_col;
   int original_y = vcur_lin;

   if (foreground_color == color_black && background_color == color_white)
      default_attribute = VMODE_REVERSE;

   usign_8 attr = VMODE_CLEAR;

   if (attribute & ATTR_BRIGHT)
      attr |= VMODE_BOLD;
   if (attribute & ATTR_BLINK)
      attr |= VMODE_BLINK;
   attribute = attribute & 0x7F; 				// Remove ATTR_BLINK bit
   if (attribute != ATTR_NORMAL)
   {
      if (((attribute & ATTR_REVERSE) == ATTR_REVERSE) ||	// ATTR_REVERSE is multiple bits, check if all on.
	  default_attribute == VMODE_REVERSE)
      {
         attr |= VMODE_REVERSE;
      }
   }
   attribute = attribute & 0x0F; 				// Remove ATTR_REVERSE bits
   if (attribute == ATTR_UNDER_DIM || attribute == ATTR_UNDER_BRIGHT)
      attr |= VMODE_UNDERSCORE;
   filtered_vtext(attr, row - 1, column - 1, text);
   vmove(original_y, original_x);
#endif
#endif
}


void put_text(const char *text) {
#if DOS || DOS_HOST
   cputs(text);
#else
#if WANG
   if (foreground_color == color_black && background_color == color_white)
      vmode(VMODE_REVERSE);
   vprint((char *) text);
#endif
#endif
}


char *read_stdin(int chars) {
   char buffer[127 + 3]; // +3 required by get_str
#if DOS
   _CL = min(chars, 127);
   asm push ds
   _DS = FP_SEG(buffer);
   _SI = FP_OFF(buffer);
   asm call far ptr get_str
   asm pop  ds
#else
   fscanf(stdin, "%s", buffer);
#endif
   return dup_string(buffer);
}


void restore_full_screen_state(Boolean hard_restore) {
#if WANG
   if (wbackground()) return;
   assert(saved_screen);
   use_full_screen_io();
   clear_screen();
   saved_screen->restore_screen();
   vdefer_restore();
   if (hard_restore)
      vrefresh(HARD_REFRESH);
   delete saved_screen;
   saved_screen = NULL;
#endif
}


void restore_video_state() {
#if DOS || DOS_HOST
   cursor_visible(true);
#else
#if WANG
   use_standard_io();
#endif
#endif
}


void ring_bell() {
#if DOS || DOS_HOST
   sound(500);
   delay(50);
   nosound();
#else
   if (using_full_screen_io && isatty(fileno(stdin)))
      vbell();
   else
      write_stdout("\a");
#endif
}


void save_full_screen_state() {
#if WANG
   if (wbackground()) return;

   assert(! saved_screen);
   saved_screen = new screen_contents(1, 1, SCREEN_WIDTH, SCREEN_HEIGHT);
   use_standard_io();
#endif
}


void set_cursor_shape(int a_shape) {
#if DOS
   asm call far ptr console_init
   _AX = a_shape;
   asm call far ptr set_cursor
#endif
}


void text_background(color a_color) {
#if DOS || DOS_HOST
   textbackground(a_color);
#else
#if WANG
   background_color = a_color;
#endif
#endif
}


void text_foreground(color a_color) {
#if DOS || DOS_HOST
   textcolor(a_color);
#else
#if WANG
   foreground_color = a_color;
#endif
#endif
}


void use_standard_io() {
#if WANG && ! DOS_HOST
   if (video_inited) {
      vmode(VMODE_CLEAR);
      vonexit(0);
      vexit();
   }
   using_full_screen_io = false;
#endif
}


int where_x() {
#if DOS || DOS_HOST
   return wherex();
#else
#if WANG
   if (! using_full_screen_io)
      use_full_screen_io();
   return vcur_col - active_window.left + 2;
#endif
#endif
}


int where_y() {
#if DOS || DOS_HOST
   return wherey();
#else
#if WANG
   if (! using_full_screen_io)
      use_full_screen_io();
   return vcur_lin - active_window.top + 2;
#endif
#endif
}


void write_stdout(const char* s) {
#if DOS || DOS_HOST
   printf("%s", s);
/*
   asm push ds
   _DS = FP_SEG(s);
   _SI = FP_OFF(s);
   asm call far ptr put_str
   asm pop  ds
*/
#else
#if WANG
   if (using_full_screen_io && isatty(fileno(stdin))) {
      int save_verbose = verbose;
      verbose = false;
      vprint((char *) s);
      verbose = save_verbose;
   }
   else {
      printf("%s", s);
      fflush(stdout);
   }

   if (wbackground())
   {
        werr_write(s);
   }
#endif
#endif
}

//------------------------------------------------------------
//
//	w4w_handler methods.
//
//------------------------------------------------------------
w4w_handler::w4w_handler()
{
	clear_image();

	/*
		Note: On DG/UX Intel the global object global_w4w_handler was
		not being initialized. This is why there is no member variables
		to store the use_w4w() and use_costar() values.
	*/
}
w4w_handler::~w4w_handler()
{
}
void w4w_handler::clear_image()
{
	memset(buff,' ', sizeof(buff));
}

/*
**	ROUTINE:	w4w_handler::put_text()
**
**	FUNCTION:	Put text into the screen buffer.
**
**	DESCRIPTION:	All non-edit field text needs to be added.
**			This is used to find hotspots.
**
**	ARGUMENTS:	
**	row		The starting row on the screen (0-23)
**	col		The starting col in the screen (0-79)
**	text		The text buffer
**	len		The length of text. (If not supplied then text in null terminated.)
**
**	GLOBALS:	none
**
**	RETURN:		none
**
**	WARNINGS:	Text will wrap to the next line but not back to the top.
**
*/
void w4w_handler::put_text(unsigned int row, unsigned int col, const char* text)
{
	put_text(row, col, text, strlen(text));
}
void w4w_handler::put_text(unsigned int row, unsigned int col, const char* text, unsigned int len)
{
	if (row > (SCREEN_HEIGHT-1)) return;
	if (col > (SCREEN_WIDTH-1)) return;

	if ( ((row*SCREEN_WIDTH) + col + len ) >= sizeof(buff) )
	{
		len = sizeof(buff) - ((row*SCREEN_WIDTH) + col);
	}

	memcpy(&buff[row][col], text, len);
}

/*
**	ROUTINE:	w4w_handler::mark_pfkey_tags()
**
**	FUNCTION:	Mark the pfkey tags on the screen by writting them 
**			with the hotspots attribute.
**
**	DESCRIPTION:	Scan the text buffer for pfkey tags and rewrite
**			the tags with the hotspot attributes.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		
**	0		No pfkey tags on the screen
**	1		There are pfkey tags on the screen
**
**	WARNINGS:	none
**
*/
int w4w_handler::mark_pfkey_tags()
{
	int row, col;
	int rc = 0;
	int hotspot_vmode;
	char the_char[2];

	the_char[1] = '\0';
	hotspot_vmode = w4w_hotspot_vmode();

	// Loop for each row and check for tags
	for(row=0; row< SCREEN_HEIGHT; row++)
	{
		char the_row[SCREEN_WIDTH+1], the_mask[SCREEN_WIDTH+1];

		memcpy(the_row, buff[row], SCREEN_WIDTH);
		the_row[SCREEN_WIDTH] = '\0';

		if (w4w_mask_row(the_row, the_mask))
		{
			// There is at least one tag on this line
			rc = 1;

			for(col=0; col < SCREEN_WIDTH; col++)
			{
				if ('X' == the_mask[col])
				{
					the_char[0] = the_row[col];
					filtered_vtext(hotspot_vmode, row, col, the_char); 
				}
			}
		}
	}
	return rc;
}

/*
**	ROUTINE:	w4w_handler::enable_mouse()
**			w4w_handler::disable_mouse()
**
**	FUNCTION:	Enable/disable the mouse. 
**
**	DESCRIPTION:	This is required for COSTAR.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		none
**
**	WARNINGS:	none
**
*/
void w4w_handler::enable_mouse()
{
	if (0!=use_costar())
	{
		costar_enable_mouse(1);
	}
}
void w4w_handler::disable_mouse()
{
	if (0!=use_costar())
	{
		costar_enable_mouse(0);
	}
}

/*
**	ROUTINE:	w4w_handler::get_mouse_position()
**
**	FUNCTION:	Get the position on the screen where the mouse was clicked.
**
**	DESCRIPTION:	This is called after a KEY_MOUSE_CLICK key is read to retieve 
**			the position.
**
**	ARGUMENTS:	
**	pRow		The returned row (0-23)
**	pCol		The returned col (0-79)
**
**	GLOBALS:	none
**
**	RETURN:		
**	0		Success
**	none 0		Failure  (row & col are not set)
**
**	WARNINGS:	none
**
*/
int w4w_handler::get_mouse_position(int* pRow, int* pCol)
{
	if (use_costar())
	{
		return costar_get_mouse_position(pRow, pCol);
	}
	else
	{
#ifdef WIN32
		return vrawntcn_get_mouse_position(pRow, pCol);
#else
		return 1;
#endif
	}
}

/*
**	ROUTINE:	w4w_handler::pfkey_tag_at()
**
**	FUNCTION:	Check if the mouse clicked onto a on-screen pfkey tag.
**
**	DESCRIPTION:	Check the position against the screen buffer and
**			return the pfkey value of the tag.
**
**	ARGUMENTS:	
**	row		Row where mouse clicked (0-23)
**	col		Column where mouse clicked (0-79)
**
**	GLOBALS:	none
**
**	RETURN:		
**	-1		Not on a tag
**	0		Enter key tag
**	1-32		Pfkey 1-32
**
**	WARNINGS:	none
**
*/
int w4w_handler::pfkey_tag_at(int row, int col)
{
	if (row < 0 || row > SCREEN_HEIGHT-1) 
	{
		return -1;
	}
	if (col < 0 || col > SCREEN_WIDTH-1)
	{
		return -1;
	}
	return w4w_click_row(col, buff[row]);
}

/*
**	ROUTINE:	w4w_handler::costar()
**
**	FUNCTION:	Is COSTAR being used
**
**	DESCRIPTION:	Check if costar is being used.
**
**	ARGUMENTS:	none
**
**	GLOBALS:	none
**
**	RETURN:		
**	0		COSTAR is not being used
**	1		COSTAR with mapping mode V1
**	2		COSTAR with mapping mode V2
**
**	WARNINGS:	none
**
*/
int w4w_handler::costar()
{
	return use_costar();
}

Boolean w4w_handler::w4w()
{
	return (use_w4w() != 0) ? true : false;
}

//
//	History:
//	$Log: crt_io.cpp,v $
//	Revision 1.20  2001/08/22 20:17:00  gsl
//	fix missing type
//	
//	Revision 1.19  1999-02-17 15:38:51-05  gsl
//	Removed the vrawtimeout logic in the key_pressed() because the waittime
//	option was never used in the wisp world.
//
//	Revision 1.18  1998-10-12 12:12:21-04  gsl
//	Fixed problems on DGUX with the global_w4w_handler not being initialized.
//
//	Revision 1.17  1998-10-02 17:00:02-04  gsl
//	Add W4W (and COSTAR) mouse support using a global w4w_handler object.
//
//	Revision 1.16  1998-09-02 17:26:02-04  gsl
//	Changed write_stdout() logic to use video if possible.
//	It was checking if stdout was a tty but stdout get redirected
//	to a file if WPROCDEBUG is set so now check stdin.
//
//	Revision 1.15  1998-08-31 15:13:38-04  gsl
//	drcs update
//	----------------------------
//	revision 1.14
//	date: 1997-12-18 19:53:28-05;  author: gsl;  state: V4_3_00;  lines: +37 -6
//	Fixed support for CHARMAP
//	----------------------------
//	revision 1.13
//	date: 1997-12-15 17:55:18-05;  author: gsl;  state: Exp;  lines: +25 -26
//	Add support for CHARMAP
//	----------------------------
//	revision 1.12
//	date: 1997-10-02 08:42:58-04;  author: gsl;  state: Exp;  lines: +0 -3
//	fix warnings
//	----------------------------
//	revision 1.11
//	date: 1997-10-01 09:04:26-04;  author: gsl;  state: Exp;  lines: +2 -2
//	fix warnings
//	----------------------------
//	revision 1.10
//	date: 1997-07-08 15:55:09-04;  author: gsl;  state: V4_1_02;  lines: +19 -16
//	Use new video.h defines
//	----------------------------
//	revision 1.9
//	date: 1996-11-13 17:00:13-05;  author: gsl;  state: V4_0_00;  lines: +4 -4
//	Change vtimeout to vrawtimeout
//	----------------------------
//	revision 1.8
//	date: 1996-07-25 19:45:38-04;  author: gsl;  state: Exp;  lines: +31 -29
//	NT
//	----------------------------
//	revision 1.7
//	date: 1996-07-25 14:14:31-04;  author: gsl;  state: Exp;  lines: +0 -0
//	Renamed from crt_io.cc to crt_io.cpp
//	----------------------------
//	revision 1.6
//	date: 1995-08-28 10:44:24-04;  author: gsl;  state: V3_3_19;  lines: +1 -4
//	Fix the problem where the screen does not get properly cleared
//	on a return into a proc from running a cobol program and when
//	the lowest level proc has no screen IO.
//	----------------------------
//	revision 1.5
//	date: 1995-06-02 12:26:08-04;  author: gsl;  state: V3_3_18;  lines: +1 -1
//	fix warning
//	----------------------------
//	revision 1.4
//	date: 1995-04-25 05:59:44-04;  author: gsl;  state: V3_3_16;  lines: +0 -0
//	drcs state V3_3_15
//	----------------------------
//	revision 1.3
//	date: 1995-04-17 07:52:01-04;  author: gsl;  state: V3_3_14;  lines: +0 -0
//	drcs state V3_3_14
//	----------------------------
//	revision 1.2
//	date: 1995-01-27 18:32:40-05;  author: gsl;  state: V3_3x12;  lines: +59 -22
//	drcs load
//	----------------------------
//	revision 1.1
//	date: 1995-01-27 16:51:00-05;  author: gsl;  state: V3_3c;
//	drcs load
//	=============================================================================
