// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : crt_io.hpp
// Author : George Soules
// Date   : 27 May 1992

#ifndef CRT_IO__HPP
#define CRT_IO__HPP

// Classes
// (none)

// Definitions and subprograms
#include "colors.hpp"
#include "environ.hpp"

#define ATTR_BLINK          0x80
#define ATTR_BRIGHT         0x08
#define ATTR_NORMAL         0x07
#define ATTR_REVERSE        0x70
#define ATTR_UNDER_DIM      0x01
#define ATTR_UNDER_BRIGHT   0x09

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
#endif

