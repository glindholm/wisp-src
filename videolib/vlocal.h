/************************************************************************/
/*	      VIDEO - Video Interactive Development Environment		*/
/*			Copyright (c) 1988, 1989, 1990			*/
/*	 An unpublished work of International Digital Scientific Inc.	*/
/*			    All rights reserved.			*/
/************************************************************************/

/*			Local (Internal) Header File			*/

#define INT_SET_TABLE_SIZE      17	/* vset() control table size.	*/
#define TAG_AS_OLD		 1	/* vmap to tag as old data.	*/
#define SCROLL_UP		 2	/* vmap to scroll the map up.	*/
#define SCROLL_DOWN		 3	/* vmap to scroll the map down.	*/
#define REFRESH_CHARACTER    '\027'	/* Refresh screen on control W.	*/
#define ABORT_CHARACTER	     '\003'	/* Abort program on control C.	*/
#define TRIGGER_CHARACTER    '\002'	/* Trigger on control B.	*/
#define DOUBLE_TOP		 1	/* Top of double high line.	*/
#define DOUBLE_BOTTOM		 2	/* Bottom of double high line.	*/
#define PRINT_BUFFER_SIZE      256	/* Maximum print buffer size.	*/
#define MAP_SIZE	      3168	/* Bytes in a screen map.	*/
#define MOTION_ONLY		 2	/* Only motion action deferred.	*/

/* save_screen - A structure to contain saved addresses and variables	*/
/*		 of the VIDEO screen database.				*/

struct save_screen { char *xchr_map; char *xatr_map; int xcur_lin;
		     int xcur_col; int xcur_atr; int xchr_set;
		     int *xcur_set; int *xlin_atr; int xscr_atr;
		     int xrol_top; int xrol_bot; int xmap_top;
		     struct save_screen *prev_ptr; };

/* Define unsigned one byte storage.					*/

#ifdef rt11
#define BYTE_1 char
#else
#define BYTE_1 unsigned char
#endif

/************************************************************************/
/*	Physical key Meta-character definitions.			*/
/************************************************************************/

#define	ANSI_PF1_KEY			01001	/* PF1 .. PF4		*/
#define	ANSI_PF2_KEY			01002	/*   Keep in order.	*/
#define	ANSI_PF3_KEY			01003
#define	ANSI_PF4_KEY			01004

#define ANSI_UP_ARROW_KEY		01005	/* Cursor sequences.	*/
#define ANSI_DOWN_ARROW_KEY		01006	/*   Keep in order.	*/
#define ANSI_RIGHT_ARROW_KEY		01007
#define ANSI_LEFT_ARROW_KEY		01010

#define DEC_VT_KP_ENTER_KEY		01011	/* Enter key.		*/

#define DEC_VT_FIND_KEY			01012	/* VT200 special keys.	*/
#define DEC_VT_INSERT_KEY		01013	/*  Keep in order.	*/
#define DEC_VT_REMOVE_KEY		01014
#define DEC_VT_SELECT_KEY		01015
#define DEC_VT_PREVIOUS_SCREEN_KEY	01016
#define DEC_VT_NEXT_SCREEN_KEY		01017

#define	DEC_VT_F6_KEY			01025
#define	DEC_VT_F7_KEY			01026
#define	DEC_VT_F8_KEY			01027
#define	DEC_VT_F9_KEY			01030
#define	DEC_VT_F10_KEY			01031
#define	DEC_VT_F11_KEY			01032
#define	DEC_VT_F12_KEY			01033
#define	DEC_VT_F13_KEY			01034
#define	DEC_VT_F14_KEY			01035
#define	DEC_VT_F15_KEY			01036
#define	DEC_VT_F16_KEY			01037
#define	DEC_VT_F17_KEY			01040
#define	DEC_VT_F18_KEY			01041
#define	DEC_VT_F19_KEY			01042
#define	DEC_VT_F20_KEY			01043

#define DEC_VT_KP_0_KEY			01060	/* Keypad numeric keys.	*/
#define DEC_VT_KP_1_KEY			01061	/* Keypad must be in	*/
#define DEC_VT_KP_2_KEY			01062	/*   applications mode.	*/
#define DEC_VT_KP_3_KEY			01063
#define DEC_VT_KP_4_KEY			01064
#define DEC_VT_KP_5_KEY			01065
#define DEC_VT_KP_6_KEY			01066
#define DEC_VT_KP_7_KEY			01067
#define DEC_VT_KP_8_KEY			01070
#define DEC_VT_KP_9_KEY			01071
#define	DEC_VT_KP_COMMA_KEY		01072	/* Keypad comma, minus	*/
#define	DEC_VT_KP_MINUS_KEY		01073	/* and period keys.	*/
#define	DEC_VT_KP_PERIOD_KEY		01074	/* Keypad must be in	*/
						/*   applications mode.	*/

#define ATT_605_F1_KEY			01101	/* AT&T 605 Fn keys.	*/
#define	ATT_605_F2_KEY			01102	/*   Keep in order.	*/
#define	ATT_605_F3_KEY			01103
#define	ATT_605_F4_KEY			01104
#define	ATT_605_F5_KEY			01105
#define	ATT_605_F6_KEY			01106
#define	ATT_605_F7_KEY			01107
#define	ATT_605_F8_KEY			01110
#define	ATT_605_F9_KEY			01111
#define	ATT_605_F10_KEY			01112
#define	ATT_605_F11_KEY			01113
#define	ATT_605_F12_KEY			01114
#define	ATT_605_F13_KEY			01115
#define	ATT_605_F14_KEY			01116
#define ATT_605_SHIFT_F1_KEY		01117
#define ATT_605_SHIFT_F2_KEY		01120
#define ATT_605_SHIFT_F3_KEY		01121
#define ATT_605_SHIFT_F4_KEY		01122
#define ATT_605_SHIFT_F5_KEY		01123
#define ATT_605_SHIFT_F6_KEY		01124
#define ATT_605_SHIFT_F7_KEY		01125
#define ATT_605_SHIFT_F8_KEY		01126
#define ATT_605_SHIFT_F9_KEY		01127
#define ATT_605_SHIFT_F10_KEY		01130
#define ATT_605_SHIFT_F11_KEY		01131
#define ATT_605_SHIFT_F12_KEY		01132
#define ATT_605_SHIFT_F13_KEY		01133
#define ATT_605_SHIFT_F14_KEY		01134

#define ATT_605_END_KEY			01140	/* End key on AT&T 605.	*/
#define ATT_605_INSERT_KEY		01141
#define ATT_605_HOME_KEY		01142
#define ATT_605_DELETE_KEY		01143
#define ATT_605_PAGE_UP_KEY		01144
#define ATT_605_PAGE_DOWN_KEY		01145

#define ATT_605_SHIFT_TAB_KEY		01146	/* AT*T 605 shifted.	*/
#define ATT_605_SHIFT_INSERT_KEY	01147
#define ATT_605_SHIFT_HOME_KEY		01150
#define ATT_605_SHIFT_DELETE_KEY	01151

#define ATT_605_SHIFT_UP_ARROW_KEY	01152
#define ATT_605_SHIFT_DOWN_ARROW_KEY	01153
#define ATT_605_SHIFT_RIGHT_ARROW_KEY	01154
#define ATT_605_SHIFT_LEFT_ARROW_KEY	01155

#define GENERIC_UP_ARROW_KEY		01200	/* Generic unix terms.	*/
#define GENERIC_DOWN_ARROW_KEY		01201
#define GENERIC_RIGHT_ARROW_KEY		01202
#define GENERIC_LEFT_ARROW_KEY		01203
#define GENERIC_HOME_KEY		01204
#define GENERIC_TAB_KEY			01205
#define GENERIC_BACKTAB_KEY		01206
#define GENERIC_LAST_SCREEN_KEY		01207
#define GENERIC_NEXT_SCREEN_KEY		01210
#define GENERIC_DELETE_KEY		01211
#define GENERIC_INSERT_KEY		01212
#define GENERIC_REMOVE_KEY		01213
#define GENERIC_CLEAR_FIELD_KEY		01214
#define GENERIC_CLEAR_AFTER_KEY		01215
#define GENERIC_CLEAR_BEFORE_KEY	01216
#define GENERIC_CANCEL_KEY		01217
#define GENERIC_ENTER_KEY		01220
#define GENERIC_RETURN_KEY		01221
#define GENERIC_HELP_KEY		01222
#define GENERIC_FN1_KEY			01223
#define GENERIC_FN2_KEY			01224
#define GENERIC_FN3_KEY			01225
#define GENERIC_FN4_KEY			01226
#define GENERIC_FN5_KEY			01227
#define GENERIC_FN6_KEY			01230
#define GENERIC_FN7_KEY			01231
#define GENERIC_FN8_KEY			01232
#define GENERIC_FN9_KEY			01233
#define GENERIC_FN10_KEY		01234
#define GENERIC_FN11_KEY		01235
#define GENERIC_FN12_KEY		01236
#define GENERIC_FN13_KEY		01237
#define GENERIC_FN14_KEY		01240
#define GENERIC_FN15_KEY		01241
#define GENERIC_FN16_KEY		01242
#define GENERIC_FN17_KEY		01243
#define GENERIC_FN18_KEY		01244
#define GENERIC_FN19_KEY		01245
#define GENERIC_FN20_KEY		01246
#define GENERIC_FN21_KEY		01247
#define GENERIC_FN22_KEY		01250
#define GENERIC_FN23_KEY		01251
#define GENERIC_FN24_KEY		01252
#define GENERIC_FN25_KEY		01253
#define GENERIC_FN26_KEY		01254
#define GENERIC_FN27_KEY		01255
#define GENERIC_FN28_KEY		01256
#define GENERIC_FN29_KEY		01257
#define GENERIC_FN30_KEY		01260
#define GENERIC_FN31_KEY		01261
#define GENERIC_FN32_KEY		01262
#define GENERIC_EXTRA_1_KEY		01263
#define GENERIC_EXTRA_2_KEY		01264
#define GENERIC_EXTRA_3_KEY		01265
#define GENERIC_EXTRA_4_KEY		01266
#define GENERIC_EXTRA_5_KEY		01267
#define GENERIC_EXTRA_6_KEY		01270
#define GENERIC_EXTRA_7_KEY		01271
#define GENERIC_EXTRA_8_KEY		01272
#define GENERIC_EXTRA_9_KEY		01273
#define GENERIC_BLUE_KEY		01274
#define GENERIC_GOLD_KEY		01275
#define GENERIC_PINK_KEY		01276
#define GENERIC_GREY_KEY		01277

#define	GOLD_BIT		04000	/* Bit indicates gold function.	*/
#define GREY_BIT	       010000	/* Bit indicates grey function.	*/
#define	PINK_BIT	       020000	/* Bit indicates pink function.	*/
#define BLUE_BIT	       040000	/* Bit indicates blue function.	*/

#define SUBSTITUTION_TABLE_SIZE    64	/* Size of substitution table.	*/

/************************************************************************/
/*		Internal forms error codes and parameters.		*/
/************************************************************************/

#define FORMS_ERROR_FILE "video.err"

#ifdef unix
#define VIDEOINFODIR 	"/usr/local/lib/videoinfo"
#define VIDEOFORMDIR	"/usr/local/lib/videoform"
#endif
#ifdef MSDOS
#define VIDEOINFODIR 	"C:\\video"
#define VIDEOFORMDIR	"C:\\video"
#endif

#define REQUIRED_FIELD_EMPTY		202
#define INVALID_FIELD_TYPE		203
#define FIELD_MUST_BE_DIG		204
#define FIELD_MUST_BE_NUM		205
#define FIELD_MUST_BE_IMPN		207
#define FIELD_MUST_BE_YMD		208
#define FIELD_MUST_BE_MDY		209
#define FIELD_MUST_BE_DMY		210
#define EDIT_TEST_FAILED		222
#define FIELD_CANNOT_BE_NEGATIVE	328
#define FIELD_MUST_BE_NUMN		230
#define LOCAL_ERROR_ACTIVE		999
