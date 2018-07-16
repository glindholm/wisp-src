/* PGGLOBAL.H															*/
/*		 Global definitions of variables used throughout the utility							*/

EXT FILE *infile, *outfile;								/* Ptrs to source file and output file.	*/
EXT FILE *logfile;									/* Current logfile (stdout by default)	*/

EXT char inline[STRBUFF], rline[STRBUFF];
EXT char cobline[COBBUF];
EXT char *aptr, *next_ptr;								/* Tells the pointer to next string.	*/

EXT int logging;									/* Flag to enable logging.		*/
EXT int log_stats;									/* Flag to enable statistic logging.	*/
EXT int num_inlines;									/* Number of input lines read.		*/
EXT int num_outlines;									/* Number of output lines written.	*/
EXT int lncnt_type;									/* Flag to say print in or out line cnt.*/

EXT int in_assign, in_print, in_run, in_submit, in_if, in_set;				/* Define of all "in" flags.		*/
EXT int in_extract, in_return, in_scratch, in_rename, in_message;
EXT int in_prompt, in_pfkey, in_curcol, in_currow, in_erase;
EXT int in_alarm, in_declare, in_proc, in_using, in_logoff;
EXT int in_library, in_row, in_putparm, in_readfdr, in_label;
EXT int in_global, in_trace, in_filecopy;
EXT int in_np_putparm;

EXT int spif;										/* Spacing for if flag.			*/
EXT long cur_fac_msk;									/* FAC ATRIBUTES FOR A FIELD.		*/
EXT char cur_mod_fac;

EXT int got_rename, got_find, got_print, got_run, got_submit;				/* Define of all "got" flags.		*/
EXT int got_set, got_extract, got_link, got_logoff, got_scratch;
EXT int got_prompt, got_name, got_message, got_putparm, got_string;
EXT int got_readfdr, got_filecopy, got_backref;
