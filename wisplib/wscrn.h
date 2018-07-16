/* WSCRN_STRUCT .H ... An include file to contain saved addresses and variables of the Wisp screen structures, for use by	*/
/*                     WPUSHSCR & WPOPSCR routines.										*/

struct wscrn_struct
{
	char    *saved_ws_atr;
	char    *saved_ws_at2;
	int     saved_ws_good;
	int     saved_old_line;
	int     saved_old_column;
	int     saved_errset;
	int     saved_kb_locked;
	int     saved_fast_read;
	int	saved_screen_width;
	struct  wscrn_struct *prev_wscrn_struct_ptr;
};
