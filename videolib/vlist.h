			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

						/* List structures header file */

struct len_text	{
			long lentext;							/* Length of text.			*/
			unsigned char *dtext;						/* Ptr to text to display.		*/
			int txtrend;							/* Rendition of text line.		*/
		};

struct column	{
			unsigned long type;						/* The type of data in this column.	*/
			unsigned long width;						/* Number of chars to display.		*/
			unsigned char *citems;						/* Pointer to array of data for column.	*/
			unsigned long length;						/* Actual length of data items.		*/
			struct column *nextc;						/* Pointer to next column.		*/
			struct column *prevc;						/* Pointer to previous column.		*/
		};

struct available_keys {
			short meta_key;							/* VIDEO meta_key value.		*/
			short list_function;						/* Value defined by vlist.		*/
		};

struct list	{
			long num_rows;							/* Number of rows in the list.		*/
			struct column *headcol;						/* Ptr to linked list of column struct.	*/
			struct len_text thead[4];					/* Head information max of 4 lines.	*/
			struct len_text tfoot[4];					/* Footer information max of 4 lines.	*/
			long start_scroll_row;						/* Row to start scroll region.		*/
			long sfoot_row;							/* Row to begin footer.			*/
		};

struct active_list {
			long l_id;							/* Identification of list.		*/
			struct list *thedef;						/* Pointer to definition of list struc.	*/
			struct available_keys *func_keys;				/* Pointer to array of available keys.	*/
			struct active_list *nextl;					/* Pointer to next active list.		*/
		};

struct tmpst	{
			struct len_text it[4];						/* Temporary holding structure.		*/
		};
