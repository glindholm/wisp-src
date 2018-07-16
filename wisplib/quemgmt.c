static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#ifdef VMS
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*
**	File:		quemgmt.c
**
**	Purpose:	To manage the jobs in the queues under VMS.
**
**			This is only a display front end to the OpenVMS queue manager.
**
**	Routines:	
**	quemngmnt()		Entry point into the queue manager routines.
**	menu_header_footer()	Init the text for header and footer.
**	prntq_header_footer()
**	batchq_header_footer()
**	init_q_fkeys()		Assign values and functions to available queue screen keys.
**	i_qe_fkeys()		Assign values and functions to queue entry screen.
**	init_rlist()
**	init_qmenu()
**	init_jobs()
**	get_time()
**	menu_help()		Display queue help window.
**	display_help()		Display queue entries help window.
**	alloc_queue_arrays()	Allocate the memory needed for the columns of the queue.
**	alloc_job_arrays()	Allocate the memory needed for the columns of the choosen queue.
**	get_qt_text()		Return the appropriate queue type.
**	get_qs_text()		Return the appropriate queue status in text format.
**	get_cpul_text()		Return the appropriate CPU limit in text format.
**	gen_queue_arrays()	This will generate the arays of data for the menu list.
**	get_js_text()		Return the appropriate job status in text format.
**	add_queue_cols()	Init the queue list with data that has been retrieved from the system.
**	add_job_cols()		Init the queue list with job info that has been retrieved from the system.
**	gen_job_arrays()	This will generate the arays of data for the specified queue.
**	hold_release_job()	Toggle the HOLD or release status of a job.
**	delete_job()
**	remove_job()		Function to delete from the current queue a pending or executing job.
**	change_disp()		Function to change the disposition of a job.
**	requeue_job()		Function to requeue an executing job.
**	submit_remove_job()	Submit job to new queue and remove from the old queue.
**	get_first_last()	Get the first and last print pages.
**	genq_empty_list()
**	genj_empty_list()
**	check_result_list()	Return TRUE if items selected.
**	rescan_jobs()		Return if still have entries.
**	change_job()		Change information on a queue entry.
**	c_mntdef_qform()	Change the default queue form number.
**	display_job()		Display job using DISPLAY Utility.
**	find_new_qname()	Get new qname info.
**	start_stop_queue()	Toggle start/stop of a queue.
**	report_error()		Display the error that occured.
**	que_mem_err()		display no memory error.
**	free_qmenu()		Free up memory assiciated with queues.
**	free_qentries()		Free up memory assiciated with queue entries.
**	work_message()		Display the working message.
**	display_box()		Draw the outline box.
**	save_window()		Save orig data under window.
**	restore_window()	Display orig data under window.
**
*/

#include "quemgmt.h"

/********************************************************************************************************************************/
/*****                               Static Global Variables								    *****/
/********************************************************************************************************************************/

static long function, mlist_id, list_id, retcd;						/* Variables used to identify the list.	*/
static long qrows, jrows;								/* Value of rows available in list.	*/
static char *tstr[8], *menutstr[8];							/* Array to hold ptr to header/footer txt.*/
static long tstrl[8], menutstrl[8];							/* Array to hold header & footer length.*/
static int tstrrnd[8], menurnd[8];							/* Array to hold the rendition of line.	*/
static long *result_list;								/* Array to hold selected results.	*/
static struct available_keys {
			short meta_key;							/* VIDEO meta_key value.		*/
			short list_function;						/* Value defined by vlist.		*/
		};
static struct available_keys fn_keys[16];						/* Array to hold avail. print job keys.	*/
static struct available_keys queuefn_keys[10];						/* Array to hold avail. menu queue keys.*/
static int llen, nopriv_fl;

static struct qui_itmlst_struct qui_qbuf[11];						/* Return queue info for each QUI call.	*/
static struct qui_itmlst_struct quibuf[3];						/* Return queue info for each QUI call.	*/
static struct qui_itmlst_struct qui_jbuf[9];						/* Buffer to hold job info from QUI call.*/
static struct qui_itmlst_struct qui_fbuf[3];						/* Buffer to hold file info from QUI call.*/
static struct io_status_block {								/* $GETQUI iosb parameter structure.	*/
		long status;								/* Completion status of func call.	*/
		long fill;								/* Condition value returned.		*/
	} iosb;
static long stat_ss, stat_qui, stat_snd;						/* Return codes from system calls.	*/
static char *qm_qn, *qm_qt, *qm_qs, *qm_cpul;						/* Ptrs to arrays of queue colume data.	*/
static long *qm_bp, *qm_jl, *qm_blmin, *qm_blmax, *qm_quet;
static char curr_queue[32];
static char *qe_jbn, *qe_usrn, *qe_st, *qe_frmn;					/* Ptrs to arrays of job column data.	*/
static long *qe_ent, *qe_blks, *qe_jf, *qe_cpy, *qe_jobs, *del_entry;
static char separ[2], dash[2];								/* Separator variables.			*/
static int num_mhead = 0;
static int num_mfoot = 0;
static int num_qehead = 0;
static int num_qefoot = 0;
static int vue_batch_que = FALSE;							/* Flags if view type queue for free.	*/
static int vue_print_que = FALSE;
static int fstque_fl = TRUE;								/* Flag to depict first time in queue.	*/
static int class_qname_typ;								/* Flag print class is in queue name.	*/

static int window_top;									/* Vars for the window parameters.	*/
static int window_rows;
static char window_data[WSB_ROWS][WSB_COLS];						/* Array to save data under window.	*/
static long firstp, lastp;

static void genj_empty_list();
static void genq_empty_list();
static void add_queue_cols();
static void add_job_cols();
static void init_rlist();

/*
**	Routine:	quemngmnt()
**
**	Function:	
**
**	Description:	
**
**	Arguments:	None
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	mm/dd/yy	Written by SMC
**
*/
void quemngmnt(queflags,quenameptr,numq)
long queflags, numq;									/* Usage flags set for search queues.	*/
char *quenameptr;
{
#define		ROUTINE		49500

	long st_qr, st_qc, st_row, st_col;
	long rqrow, rrow, rcol, key;
	long type, width, length, icol;
	register int i;									/* Working registers.			*/
	int c, contfl, another, available, tst;
	char *tcpt;
	int ckfl, svo, chng_que, cnt;
	long *jpt;
	int adjust_sq;
	char qpntclass[15], *pcptr;							/* Print class of queue.		*/

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	if (wbackground()) return;

	chng_que = TRUE;								/* Set so will display queue.		*/
	fstque_fl = TRUE;								/* Set first time flag TRUE.		*/
	another = TRUE;
	adjust_sq = TRUE;

	while (another)									/* Display queues to choose from.	*/
	{
		if (fstque_fl)								/* Is this the first time through? 	*/
		{
			available = gen_queue_arrays(queflags,quenameptr,numq);		/*  Generate queue columns.		*/
			if (!available)							/* There are no queues to choose from.	*/
			{
				genq_empty_list(&rrow,&rcol,&key);			/* Init an empty queue list.		*/
				return;
			}
			init_rlist(qrows);						/* Zero out the result array.		*/
			mlist_id = MENU_LIST;
			st_row = 0;							/* Init to top of list.			*/
			st_col = 0;
			rrow = st_row;
			rcol = st_col;
			init_qmenu(mlist_id);						/* Create queue list structure.		*/
			add_queue_cols();						/* Init the queue list strucutre.	*/
			fstque_fl = FALSE;
		}
		if (chng_que)
		{
			function = DISPLAY_LIST;					/* If queue has changed then redisplay.	*/
			chng_que = FALSE;
		}
		else 	function = NO_DISPLAY_LIST;					/* No changes so don't re-display list.	*/
		vlist(&function,&mlist_id,&st_row,&st_col,&rrow,&rcol,result_list,&key,&retcd,&adjust_sq);
		if (adjust_sq)
		{
			rrow = rrow - st_row;
			adjust_sq = FALSE;
		}
		rqrow = st_row + rrow;

		if (key == fn16_key) 							/* Exit the queue management routine.	*/
		{									/* Have hit the PF16 key.		*/
			function = FREE_LIST;
			vlist(&function,&mlist_id,&retcd);				/* Free up the memory of the queue menu.*/
			free_qmenu(mlist_id);						/* Free all memory associated with	*/
			if (vue_batch_que)						/* If viewed a batch queue, free mem.	*/
			{
				list_id = BQUEUE_LIST;					/* Set up for a batch queue.		*/
				vlist(&function,&list_id,&retcd);			/* Free up memory for queues entries.	*/
				vue_batch_que = FALSE;
			}
			if (vue_print_que)						/* If viewed a print queue, free mem.	*/
			{
				list_id = PQUEUE_LIST;					/* Set up for a print queue.		*/
				vlist(&function,&list_id,&retcd);			/* Free up memory for queue entries.	*/
				vue_print_que = FALSE;
			}
			free(result_list);						/*  main queue menu.			*/
			return;
		}                               
		tcpt = qm_qn + (rqrow * 31);						/* Get position in queue name array.	*/
		i = 0;
		cnt = 0;
		if (class_qname_typ)
		{
			memset(qpntclass,'\0',10);					/* Set print class to nulls.		*/
			pcptr = qpntclass;						/* Set ptr to print class string.	*/
			while (*tcpt != ' ')						/* Step past the class info.		*/
			{
				*pcptr++ = *tcpt;					/* Get the class info.			*/
				tcpt++;
				cnt++;
			}
			*pcptr = '\0';							/* Null terminate the string.		*/
			tcpt++;								/* Step past the seperating space.	*/
			cnt++;
		}
		while (*tcpt != ' ' && i < 31-cnt) curr_queue[i++] = *tcpt++;		/* Copy name into variable.		*/
		curr_queue[i] = '\0';							/* Null terminate the string.		*/

		tst = WSB_ROWS - num_mhead - num_mfoot;					/* Calculate scroll region.		*/
		st_qr = st_row;
		st_qc = st_col;
		if (key == fn7_key)							/* Toggle Start and Stop queue.		*/
		{
			chng_que = start_stop_queue(rqrow);				/* Toggle start/stop of a queue.	*/
		}
		else if (key == fn9_key || key == fn10_key)				/* Change mounted/default form # of que.*/
		{
			long qtfl;

			qtfl = *(qm_quet + rqrow);					/* Get the queue flags for current queue.*/
			if ( ((qtfl & QUI$M_QUEUE_PRINTER) != QUI$M_QUEUE_PRINTER) &&
			     ((qtfl & QUI$M_QUEUE_TERMINAL) != QUI$M_QUEUE_TERMINAL) )
			{
				vbell();						/* Is not a printer queue.		*/
				chng_que = FALSE;
			}
			else
			{
				c_mntdef_qform(key);
				chng_que = TRUE;					/* Set so will re-display screen.	*/
			}
		}
		else if (key == help_key)						/* Hit the help key.			 */
		{
			menu_help();							/* Display queue menu help screen.	*/
		}
		else if (key == return_key || key == enter_key)				/* Selected a queue.			 */
		{
			int etype, rs_flag;

			chng_que = TRUE;						/* Set so will redisplay queue on return.*/
			nopriv_fl = FALSE;
			available = gen_job_arrays(queflags,&rs_flag);			/* Generate job columns, return # rows.	*/
			if (!available)							/* There are no jobs to choose from.	*/
			{
				genj_empty_list(&rrow,&rcol,&key);
			}
			else
			{
				int rqerow, chng_entries;				/* Flag do depict if to re-display.	*/
				int adjust_sj;						/* First screen of job display.		*/

				adjust_sj = TRUE;
				etype = FALSE;
				init_jobs(etype,rqrow);					/* Create job list structure.		*/
				add_job_cols(rqrow);					/* Init the job list structure.		*/
				tst = WSB_ROWS - num_qehead - num_qefoot;		/* Calculate scroll region.		*/
				st_row = 0;						/* Init to top of list.			*/
				st_col = 0;
				rrow = st_row;
				rcol = st_col;
				chng_entries = TRUE;					/* Set so will display first time in.	*/
				contfl = TRUE;
				if (rs_flag)						/* If need to delete entries where files*/
				{							/*  don't exist then rescan first before*/
					register cnt;					/*  displaying.				*/
					long *dptr;

					for (cnt = 0; cnt < jrows; cnt++)
					{
						dptr = del_entry + cnt;			/* Point to pos in del entry array.	*/
						if (*dptr == 1)
						{
							remove_job(cnt,1);		/* Remove the job from the queue.	*/
						}                                     
					}
					contfl = rescan_jobs(rqrow,queflags);		/* Set TRUE if still have entries.	*/
				}

				while (contfl)						/* While still want to continue.	*/
				{
					int chnged, wmon;				/* Flag if entry actually changed.	*/

					chnged = FALSE;					/* Assumes no changes in list structure.*/
					rqerow = rrow;
					if (chng_entries)
					{
						function = DISPLAY_LIST;		/* Display the jobs in queue.		*/
						chng_entries = FALSE;
					}
					else 	function = NO_DISPLAY_LIST;		/* No changes so don't re-display list.	*/
					vlist(&function,&list_id,&st_row,&st_col,&rrow,&rcol,result_list,&key,&retcd,&adjust_sj);

					if (adjust_sj)
					{
						rrow = rrow - st_row;
						adjust_sj = FALSE;
					}
					rqerow = st_row + rrow;

					if (key == help_key)
					{
						display_help();				/* Display the help menu.		*/
					}
					else if (key == fn16_key)			/* Exit the entries list by pushing PF16.*/
					{
						free_qentries(etype);			/* Free up memory from entries list.	*/
						contfl = FALSE;				/* pushing PF16 key.			*/
					}
					else if (key == fn6_key)			/* Re-scan the jobs in the queue.	 */
					{
						contfl = rescan_jobs(rqrow,queflags);	/* Set TRUE if still have entries.	*/
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else if (key == fn7_key)			/* Toggle hold/release job.		 */
					{
						wmon = 0;
						vset_cursor_off();
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{					/* Loop through for each selected job.	*/
							for (i = 0; i < jrows; i++)
							{				 /* Output flashing working message.	*/
								if (jrows > 10) wmon = work_message(wmon);
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = hold_release_job(i);
								}
							}
						}
						else  chnged = hold_release_job(rqerow);/* Perform on cursor job only.		*/
						chng_entries = TRUE;			/* Set so will do display.		*/
						vset_cursor_on();
					}						/* Remove the job from queue and	*/
					else if (key == fn8_key)			/* Scratch the file.			*/
					{
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{					/* Loop through for each selected job.	*/
							for (i = 0; i < jrows; i++)
							{
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = delete_job(i);
									if (chnged) rqerow--;
								}
							}
						}					/* Perform on cursor job only.		*/
						else
						{
							chnged = delete_job(rqerow);
							if (chnged) rqerow--;
						}
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else if (key == fn10_key)			/* Change the job.			*/
					{
						int caller;

						caller = 0;				/* Set so gives modifiable fields.	*/
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{
							for (i = 0; i < jrows; i++)
							{
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = change_job(i,qpntclass,numq,caller);
									if (chnged == 2) rqerow--; /* Moved to another queue	*/
								}
							}
						}
						else
						{
							chnged = change_job(rqerow,qpntclass,numq,caller);
							if (chnged == 2) rqerow--;	/* Moved to another queue.		*/
						}
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else if (key == fn11_key)			/* Display the job.			*/
					{
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{					/* Loop through for each selected job.	*/
							for (i = 0; i < jrows; i++)
							{
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = display_job(i);
								}
							}
						}					/* Perform on cursor job only.		*/
						else
						{
							chnged = display_job(rqerow);
						}
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else if (key == fn12_key)			/* Remove the job from queue.		 */
					{
						wmon = 0;
						vset_cursor_off();
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{					/* Loop through for each selected job.	*/
							for (i = 0; i < jrows; i++)
							{				 /* Output flashing working message.	*/
								if (jrows > 10) wmon = work_message(wmon);
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = remove_job(i);
									if (chnged) rqerow--;
								}
							}
						}
						else
						{
							chnged = remove_job(rqerow);	/* Perform on cursor job only.		*/
							if (chnged) rqerow--;
						}
						vset_cursor_on();
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else if (key == fn13_key)			/* Change the disposition of the job.	*/
					{
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{
							for (i = 0; i < jrows; i++)
							{
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = change_disp(i,queflags);
								}
							}
						}
						else chnged = change_disp(rqerow,queflags);
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else if (key == fn14_key)			/* Requeue an executing job.		*/
					{
						if (ckfl = check_result_list())		/* TRUE if there are items selected.	*/
						{
							for (i = 0; i < jrows; i++)
							{
								jpt = (long *)(result_list + i);
								if (*jpt)		/* If the job is selected.		*/
								{
									chnged = requeue_job(i,queflags,qpntclass,numq);
								}
							}
						}
						else chnged = requeue_job(rqerow,queflags,qpntclass,numq);
						chng_entries = TRUE;			/* Set so will do display.		*/
					}
					else
					{
						vbell();				/* No match of job/entry function key.	*/
						chnged = FALSE;				/* The queue entries did not change.	*/
						chng_entries = FALSE;			/* Set so will do display.		*/
					}

					if (chnged)
					{
						contfl = rescan_jobs(rqrow,queflags);	/* Set TURE if still have entries.	*/
						if (rqerow < 0) rqerow = 0;
					}

					rcol = 0;
				}							/* End while display jobs.		*/
			}								/* End else - display jobs.		*/
		}									/* End else of RETURN key.		*/
		else									/* Should never do because vlist checks	*/
		{									/*  for a valid function key.		*/
			vbell();							/* No match of queue function key.	*/
		}

		rrow = rqrow - st_qr;							/* Set cursor to chosen queue on return.*/
		st_row = st_qr;
		st_col = st_qc;
		rcol = 0;
	}										/* End while - display queues.		*/
}

static void menu_header_footer(l_id)							/* Init the text for header and footer.	*/
long l_id;
{
	char *cp;
	char buff[81];
	char txt[81];

	strcpy(txt,"OpenVMS QUEUE MANAGEMENT");						/* Init the header string.		*/
	menutstrl[0] = get_time(buff,txt);						/* Get user,date,time & return length.	*/
	if (!(cp = (char *)calloc(menutstrl[0],sizeof(char)))) que_mem_err(menutstrl[0]); /* Get some memory for the string.	*/
	menutstr[0] = cp;								/* Init the pointer array to the string.*/
	strcpy(cp,buff);								/* Init the header string.		*/
	menurnd[0] = CLEAR;
	menutstrl[1] = 77;
	if (!(cp = (char *)calloc(menutstrl[1],sizeof(char)))) que_mem_err(menutstrl[1]); /* Get some memory for the string.	*/
	menutstr[1] = cp;								/* Init the pointer array to the string.*/
	strcpy(cp,"                                 Base Job                            Block   ");
	menurnd[1] = CLEAR;
	menutstrl[2] = 77;								/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(menutstrl[2],sizeof(char)))) que_mem_err(menutstrl[2]); /* Get some memory for the string.	*/
	menutstr[2] = cp;								/* Init the pointer array to the string.*/
	if (class_qname_typ) strcpy(cp," Class/Name              Type    Prio Lim   CPU Limit  Status        Limit   ");
	else strcpy(cp," Name                    Type    Prio Lim   CPU Limit  Status        Limit   ");
	menurnd[2] = UNDERSCORE;
											/* Now init the footer strings.		*/
	if (l_id == EMPTY_LIST)								/* If is an empty list.			*/
	{
		menutstrl[4] = 42;							/* Init the array of lengths.		*/
		if (!(cp = (char *)calloc(menutstrl[4],sizeof(char)))) que_mem_err(menutstrl[4]); /* Get some memory for string.*/
		menutstr[4] = cp;							/* Init the pointer array to the string.*/
		strcpy(cp,"     Press (ENTER) or (16) to continue.");
		menurnd[4] = BOLD;
		return;
	}
	menutstrl[4] = 73;								/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(menutstrl[4],sizeof(char)))) que_mem_err(menutstrl[4]); /* Get some memory for the string.	*/
	menutstr[4] = cp;								/* Init the pointer array to the string.*/
	strcpy(cp,"     Position cursor to indicate queue and press (ENTER), (16) to exit");
	menurnd[4] = CLEAR;
	menutstrl[5] = 77;								/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(menutstrl[5],sizeof(char)))) que_mem_err(menutstrl[5]); /* Get some memory for the string.	*/
	menutstr[5] = cp;								/* Init the pointer array to the string.*/
	strcpy(cp,"  (HELP) Help  (7) Start/Stop Queue  (9) Mounted Form #  (10) Default Form #");
	menurnd[5] = CLEAR;
	return;
}

static void prntq_header_footer(type)
int type;
{
	char *cp;
	char buff[81];
	char txt[81];
	int lnl;

	strcpy(txt,"OpenVMS Print Queue Management");					/* Init the header string.		*/
	tstrl[0] = get_time(buff,txt);							/* Get user,date,time & return length.	*/
	if (!(cp = (char *)calloc(tstrl[0],sizeof(char)))) que_mem_err(tstrl[0]);	/* Get some memory for the string.	*/
	tstr[0] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,buff);								/* Init the header string.		*/
	tstrrnd[0] = CLEAR;
	lnl = (WSB_COLS - strlen(curr_queue)) / 2;
	memset(buff,' ',lnl);								/* Copy the blanks into buffer.		*/
	buff[lnl] = '\0';
	strcat(buff,curr_queue);
	tstrl[1] = strlen(buff);;
	if (!(cp = (char *)calloc(tstrl[1],sizeof(char)))) que_mem_err(tstrl[1]);	/* Get some memory for the string.	*/
	tstr[1] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,buff);								/* Init the header string.		*/
	tstrrnd[1] = BOLD;
	tstrl[2] = WSB_COLS;								/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[2],sizeof(char)))) que_mem_err(tstrl[2]);	/* Get some memory for the string.	*/
	tstr[2] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp," Job name                       Username    Entry  Status       Blocks Form Cpy");
	tstrrnd[2] = UNDERSCORE;
											/* Now init footer text.		*/
	if (type)									/* If is an empty list.			*/
	{
		tstrl[4] = 42;								/* Init the array of lengths.		*/
		if (!(cp = (char *)calloc(tstrl[4],sizeof(char)))) que_mem_err(tstrl[4]); /* Get some memory for string.	*/
		tstr[4] = cp;								/* Init the pointer array to the string.*/
		strcpy(cp,"     Press (ENTER) or (16) to continue.");
		tstrrnd[4] = BOLD;
		return;
	}
	tstrl[4] = 72;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[4],sizeof(char)))) que_mem_err(tstrl[4]);	/* Get some memory for the string.	*/
	tstr[4] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"     Position cursor to indicate file and press PFkey to perform action:");
	tstrrnd[4] = CLEAR;
	tstrl[5] = 67;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[5],sizeof(char)))) que_mem_err(tstrl[5]);	/* Get some memory for the string.	*/
	tstr[5] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"(HELP) Help screen       (1) Select/De-select          (11) Display");
	tstrrnd[5] = CLEAR;
	tstrl[6] = 78;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[6],sizeof(char)))) que_mem_err(tstrl[6]);	/* Get some memory for the string.	*/
	tstr[6] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp," (6) Re-scan queue       (7) Hold/Release (10) Change  (13) Change disposition");
	tstrrnd[6] = CLEAR;
	tstrl[7] = 71;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[7],sizeof(char)))) que_mem_err(tstrl[7]);	/* Get some memory for the string.	*/
	tstr[7] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"(16) Exit                (8) Scratch      (12) Remove  (14) Requeue job");
	tstrrnd[7] = CLEAR;
	return;
}

static void batchq_header_footer(type)
int type;
{
	char *cp;
	char buff[81];
	char txt[81];
	int lnl;

	strcpy(txt,"OpenVMS Batch Queue Management");					/* Init the header string.		*/
	tstrl[0] = get_time(buff,txt);							/* Get user,date,time & return length.	*/
	if (!(cp = (char *)calloc(tstrl[0],sizeof(char)))) que_mem_err(tstrl[0]);	/* Get some memory for the string.	*/
	tstr[0] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,buff);								/* Init the header string.		*/
	tstrrnd[0] = CLEAR;
	lnl = (WSB_COLS - strlen(curr_queue)) / 2;
	memset(buff,' ',lnl);								/* Copy the blanks into buffer.		*/
	buff[lnl] = '\0';
	strcat(buff,curr_queue);
	tstrl[1] = strlen(buff);;
	if (!(cp = (char *)calloc(tstrl[1],sizeof(char)))) que_mem_err(tstrl[1]);	/* Get some memory for the string.	*/
	tstr[1] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,buff);								/* Init the header string.		*/
	tstrrnd[1] = BOLD;
	tstrl[2] = WSB_COLS;								/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[2],sizeof(char)))) que_mem_err(tstrl[2]);	/* Get some memory for the string.	*/
	tstr[2] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp," Job name                       Username    Entry  Status                       ");
	tstrrnd[2] = UNDERSCORE;
											/* Now init footer text.		*/
	if (type)									/* If is an empty list.			*/
	{
		tstrl[4] = 42;								/* Init the array of lengths.		*/
		if (!(cp = (char *)calloc(tstrl[4],sizeof(char)))) que_mem_err(tstrl[4]); /* Get some memory for string.	*/
		tstr[4] = cp;								/* Init the pointer array to the string.*/
		strcpy(cp,"     Press (ENTER) or (16) to continue.");
		tstrrnd[4] = BOLD;
		return;
	}
	tstrl[4] = 72;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[4],sizeof(char)))) que_mem_err(tstrl[4]);	/* Get some memory for the string.	*/
	tstr[4] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"     Position cursor to indicate file and press PFkey to perform action:");
	tstrrnd[4] = CLEAR;
	tstrl[5] = 67;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[5],sizeof(char)))) que_mem_err(tstrl[5]);	/* Get some memory for the string.	*/
	tstr[5] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"(HELP) Help screen       (1) Select/De-select          (11) Display");
	tstrrnd[5] = CLEAR;
	tstrl[6] = 41;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[6],sizeof(char)))) que_mem_err(tstrl[6]);	/* Get some memory for the string.	*/
	tstr[6] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp," (6) Re-scan queue       (7) Hold/Release");
	tstrrnd[6] = CLEAR;
	tstrl[7] = 53;									/* Init the array of lengths.		*/
	if (!(cp = (char *)calloc(tstrl[7],sizeof(char)))) que_mem_err(tstrl[7]);	/* Get some memory for the string.	*/
	tstr[7] = cp;									/* Init the pointer array to the string.*/
	strcpy(cp,"(16) Exit                                 (12) Remove");
	tstrrnd[7] = CLEAR;
	return;
}

static void init_q_fkeys(l_id)								/* Assign values and functions to 	*/
long l_id;										/*  available keys.			*/
{
	int i;

	i = 0;
	if (l_id != EMPTY_LIST)								/* If not an empty list.		*/
	{
		queuefn_keys[i].list_function = UP_PAGE;				/* Scroll up a complete page.		*/
		queuefn_keys[i].meta_key = fn4_key;					/* Assign PF4.				 */
		i++;
		queuefn_keys[i].list_function = DOWN_PAGE;				/* Scroll down a complete page.		*/
		queuefn_keys[i].meta_key = fn5_key;					/* Assign PF5.				*/
		i++;
		queuefn_keys[i].list_function = VLIST_TOP;				/* Go to top of list.			*/
		queuefn_keys[i].meta_key = fn2_key;					/* Assign PF2.				*/
		i++;
		queuefn_keys[i].list_function = VLIST_BOTTOM;				/* Go to bottom of list.		*/
		queuefn_keys[i].meta_key = fn3_key;					/* Assign PF3.				*/
		i++;
		queuefn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
		queuefn_keys[i].meta_key = fn7_key;					/* Assign PF7.				*/
		i++;
		queuefn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
		queuefn_keys[i].meta_key = fn9_key;					/* Assign PF9.				*/
		i++;
		queuefn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
		queuefn_keys[i].meta_key = fn10_key;					/* Assign PF10.				*/
		i++;
		queuefn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
		queuefn_keys[i].meta_key = help_key;					/* Assign HELP_KEY.			*/
		i++;
	}
	queuefn_keys[i].list_function = ALLOW_KEY;					/* Allow termination of input.		*/
	queuefn_keys[i].meta_key = fn16_key;						/* Assign PF16.				*/
	i++;
	queuefn_keys[i].list_function = 0;						/* Define so can detect the end of the	*/
	queuefn_keys[i].meta_key = 0;							/*  list.				*/
	return;
}

static void i_qe_fkeys(type,l_id)							/* Assign values and functions to 	*/
int type;										/*  available keys.			*/
long l_id;
{
	int i;

	i = 0;
	if (!type)									/* If not an empty list.		*/
	{
		fn_keys[i].list_function = UP_PAGE;					/* Scroll up a complete page.		*/
		fn_keys[i].meta_key = fn4_key;						/* Assign PF4.				*/
		i++;
		fn_keys[i].list_function = DOWN_PAGE;					/* Scroll down a complete page.		*/
		fn_keys[i].meta_key = fn5_key;						/* Assign PF5.				*/
		i++;
		fn_keys[i].list_function = VLIST_TOP;					/* Go to top of list.			*/
		fn_keys[i].meta_key = fn2_key;						/* Assign PF2.				*/
		i++;
		fn_keys[i].list_function = VLIST_BOTTOM;				/* Go to bottom of list.		*/
		fn_keys[i].meta_key = fn3_key;						/* Assign PF3.				*/
		i++;
		fn_keys[i].list_function = SELECT_ROW;					/* Toggle Select/De-select this row.	*/
		fn_keys[i].meta_key = fn1_key;						/* Assign PF1.				*/
		i++;
		fn_keys[i].list_function = DESELECT_ROW;				/* Toggle De-select/Select this row.	*/
		fn_keys[i].meta_key = fn1_key;						/* Assign PF1.				*/
		i++;
		fn_keys[i].list_function = ALLOW_KEY;					/* Allow termination of input.		*/
		fn_keys[i].meta_key = help_key;						/* Assign HELP_KEY.			*/
		i++;
		fn_keys[i].list_function = ALLOW_KEY;					/* Allow termination of input.		*/
		fn_keys[i].meta_key = fn7_key;						/* Assign PF7.				*/
		i++;
		fn_keys[i].list_function = ALLOW_KEY;					/* Allow Display of file.		*/
		fn_keys[i].meta_key = fn11_key;						/* Assign PF11.				*/
		i++;
		fn_keys[i].list_function = ALLOW_KEY;					/* Allow termination of input.		*/
		fn_keys[i].meta_key = fn12_key;						/* Assign PF12.				*/
		i++;
		fn_keys[i].list_function = ALLOW_KEY;					/* Define requeue executing job.	*/
		fn_keys[i].meta_key = fn14_key;						/* Assign PF14.				*/
		i++;
		fn_keys[i].list_function = ALLOW_KEY;					/* Define re-scan jobs in queue key.	*/
		fn_keys[i].meta_key = fn6_key;						/* Assign PF6.				*/
		i++;
		if (l_id == PQUEUE_LIST)						/* If is a print queue.			*/
		{
			fn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
			fn_keys[i].meta_key = fn10_key;					/* Assign PF10.				*/
			i++;
			fn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
			fn_keys[i].meta_key = fn13_key;					/* Assign PF13.				*/
			i++;
			fn_keys[i].list_function = ALLOW_KEY;				/* Allow termination of input.		*/
			fn_keys[i].meta_key = fn8_key;					/* Assign PF8.				*/
			i++;
		}
	}
	fn_keys[i].list_function = ALLOW_KEY;						/* Define exit with no changes.		*/
	fn_keys[i].meta_key = fn16_key;							/* Assign PF16.				*/
	i++;
	fn_keys[i].list_function = 0;							/* Define so can detect the end of the	*/
	fn_keys[i].meta_key = 0;							/*  list.				*/
	return;
}

static void init_rlist(rows)
int rows;
{
	char *cp;
	register int i;

	if (fstque_fl)									/* If this is the first time in.	*/
	{										/* Allocate for result list.		*/
		if (!(result_list = (long *)malloc(rows*sizeof(long)))) que_mem_err(rows);
	}
	else
	{										/* Reallocate for result list.		*/
		free(result_list);
		if (!(result_list = (long *)malloc(rows*sizeof(long)))) que_mem_err(rows);
	}

	for (i = 0; i < rows; i++)
	{
		result_list[i] = 0;							/* Init the result array.		*/
	}
	return;
}

static int init_qmenu(l_id)
long l_id;
{
	function = INIT_LIST;								/* Allocate memory for queue list.	*/
	vlist(&function,&l_id,&qrows,&retcd);						/* Init the list.			*/
	init_q_fkeys(l_id);								/* Assign available function keys.	*/
	menu_header_footer(l_id);							/* Init the header & footer text.	*/
	function = SET_FUNCTIONS;
	vlist(&function,&l_id,queuefn_keys,&retcd);					/* Init the array of function keys.	*/
	function = ADD_HEADER;								/* Add the list header.			*/
	num_mhead = 3;
	vlist(&function,&l_id,&num_mhead,&retcd,menutstr[0],&menutstrl[0],&menurnd[0],menutstr[1],&menutstrl[1],&menurnd[1],
				menutstr[2],&menutstrl[2],&menurnd[2]);
	function = ADD_FOOTER;								/* Add the list footer.			*/
	if (l_id == EMPTY_LIST)
	{
		num_mfoot = 1;
		vlist(&function,&l_id,&num_mfoot,&retcd,menutstr[4],&menutstrl[4],&menurnd[4]);
	}
	else
	{
		num_mfoot = 2;
		vlist(&function,&l_id,&num_mfoot,&retcd,menutstr[4],&menutstrl[4],&menurnd[4],
				menutstr[5],&menutstrl[5],&menurnd[5]);
	}
	return(SUCCESS);
}

static int init_jobs(etype,rrow)
int etype;
long rrow;
{
	long qtfl;

	qtfl = *(qm_quet + rrow);							/* Get the queue flags for current queue.*/
	if ( ((qtfl & QUI$M_QUEUE_BATCH) == QUI$M_QUEUE_BATCH) ||
	     ((qtfl & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC) )
	{
		list_id = BQUEUE_LIST;							/* Set up for a batch queue.		*/
		batchq_header_footer(etype);						/* Init the header & footer text.	*/
	}
	else
	{
		list_id = PQUEUE_LIST;							/* Set up for a print queue.		*/
		prntq_header_footer(etype);						/* Init the header & footer text.	*/
	}
	i_qe_fkeys(etype,list_id);							/* Assign available functions.		*/
	function = INIT_LIST;								/* Allocate memory for job queue list.	*/
	vlist(&function,&list_id,&jrows,&retcd);					/* Init the print queue job list.	*/
	if (!etype)  init_rlist(jrows);							/* Zero out the result array.		*/
	function = SET_FUNCTIONS;
	vlist(&function,&list_id,fn_keys,&retcd);					/* Init the array of function keys.	*/
	function = ADD_HEADER;								/* Add the list header for each queue.	*/
	num_qehead = 3;
	vlist(&function,&list_id,&num_qehead,&retcd,tstr[0],&tstrl[0],&tstrrnd[0],tstr[1],&tstrl[1],&tstrrnd[1],
				tstr[2],&tstrl[2],&tstrrnd[2]);
	function = ADD_FOOTER;								/* Add the list footer.			*/
	if (!etype)
	{
		num_qefoot = 4;								/* If not an empty list.		*/
		vlist(&function,&list_id,&num_qefoot,&retcd,tstr[4],&tstrl[4],&tstrrnd[4],tstr[5],&tstrl[5],&tstrrnd[5],
				tstr[6],&tstrl[6],&tstrrnd[6],tstr[7],&tstrl[7],&tstrrnd[7]);
		if (list_id == BQUEUE_LIST) vue_batch_que = TRUE;			/* Set so can free on exit.		*/
		else 			    vue_print_que = TRUE;
	}
	else
	{
		num_qefoot = 1;
		vlist(&function,&list_id,&num_qefoot,&retcd,tstr[4],&tstrl[4],&tstrrnd[4]);
	}
	return(SUCCESS);
}

static int get_time(cp,txt)
char *cp, *txt;
{
	struct tm *time_structure;
	char username[34], *tptr;
	char timestring[80];
	char temp[80], *bptr;
	int i, cnt, timel, numsp;
	time_t time_val;
	static char *month[12] = {"January","February","March","April","May","June","July","August","September","October",
					"November","December"};
	static char *hour[2] = {"AM","PM"};

	bptr = cp;										/* Set buffer ptr to beginning.	*/
	cnt = 0;
	strcpy(temp,"User: ");									/* Copy text onto temp var.	*/
	tptr = temp;										/* Set ptr to username.		*/
	while (*tptr)
	{
		*bptr++ = *tptr++;								/* Copy text into buffer.	*/
		cnt++;										/* Increment charactes copied.	*/
	}
	cuserid(username);
	tptr = username;									/* Set ptr to username.		*/
	while (*tptr)
	{
		*bptr++ = *tptr++;								/* Copy username into buffer.	*/
		cnt++;										/* Increment charactes copied.	*/
	}
	time(&time_val);
	time_structure = localtime(&time_val);
	if (time_structure->tm_hour > 12)
	{
		time_structure->tm_hour = (time_structure->tm_hour) - 12;
		i = 1;
	}
	else i = 0;
	sprintf(timestring,"%s %d, %d  %d:%02d %s",
		month[time_structure->tm_mon],
		time_structure->tm_mday,
		time_structure->tm_year+1900,
		time_structure->tm_hour,
		time_structure->tm_min,
		hour[i]);
	timel = strlen(timestring);
	i = cnt + timel + strlen(txt);							/* Calculate the number of used chars.	*/
	numsp = (WSB_COLS - i)/2;							/* Assign number of padding spaces.	*/
	for (i = 0; i < numsp; i++)							/* Put spaces into buffer.		*/
	{
		*bptr++ = ' ';
		cnt++;
	}
	tptr = txt;									/* Set ptr to text.			*/
	while (*tptr)
	{
		*bptr++ = *tptr++;							/* Copy text into buffer.		*/
		cnt++;									/* Increment charactes copied.		*/
	}
	for (i = 0; i < numsp; i++)							/* Put spaces into buffer.		*/
	{
		*bptr++ = ' ';
		cnt++;
	}
	tptr = timestring;								/* Set ptr to timestring.		*/
	while (*tptr)
	{
		*bptr++ = *tptr++;							/* Copy time string into buffer.	*/
		cnt++;									/* Increment characters copied.		*/
	}
	*bptr = '\0';									/* Null terminate the string.		*/
	return(cnt);									/* Return the length of the buffer.	*/
}

static menu_help()									/* Display queue help window.		*/
{
	int lhs;									/* Left hand side of display.		*/
	int columns;									/* Define size of the help window.	*/
	register int i;									/* Working register.			*/

	vstate(-1);									/* Save where we are.			*/

	window_top = 7;
	window_rows = 9;
	lhs = 3;									/* Assume at column 3.			*/
	columns = 77;									/* The help area is 77 columns wide.	*/

	save_window(lhs,columns);							/* Save data before window created.	*/
	display_box(lhs,columns);
	i = window_top;									/* Point the help area.			*/
	vmode(VMODE_REVERSE|VMODE_UNDERSCORE);						/* Do it in reverse underscore,		*/
	vmove(i++,lhs);									/* Move to the info area.		*/
	vprint("                      *** QUEUE MANAGEMENT HELP MENU ***                    ");
	vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	vmode(VMODE_REVERSE);								/* Do it in reverse.			*/
	vmove(i++,lhs);
	vprint("FUNCTION KEYS                             CURSOR MOVEMENT                   ");
	vmove(i++,lhs);
	vprint("HELP  Help Menu                                                             ");
	vmove(i++,lhs);
	vprint("(7)   Start/Stop queue toggle             (2)   Go to the TOP of list       ");
	vmove(i++,lhs);
	vprint("(9)   Change queue mounted print form     (3)   Go to the BOTTOM of the list");
	vmove(i++,lhs);
	vprint("(10)  Change queue default print form     (4)   Scroll UP a page in list    ");
	vmove(i++,lhs);
	vprint("(16)  Exit from queue management          (5)   Scroll DOWN a page in list  ");
	vmove(i++,lhs);
	vprint("Use the HELP key to bring this window back.                                 ");
	vmove(i,lhs);
	vprint("Depress any key to remove this window.                                      ");
	vmove(i,lhs+39);
	vgetm();
	restore_window(lhs,columns);							/* Display orig data under window.	*/
	vstate(1);									/* Restore where we were.		*/
}

static display_help()									/* Display queue entries help window.	*/
{
	int lhs;									/* Left hand side of display.		*/
	int columns;									/* Define size of the help window.	*/
	register int i;									/* Working register.			*/

	vstate(-1);									/* Save where we are.			*/

	window_top = 5;
	window_rows = 13;
	lhs = 3;									/* Assume at column 3.			*/
	columns = 77;									/* The help area is 77 columns wide.	*/

	save_window(lhs,columns);							/* Save data before window created.	*/
	display_box(lhs,columns);
	i = window_top;									/* Point the help area.			*/
	vmode(VMODE_REVERSE|VMODE_UNDERSCORE);						/* Do it in reverse underscore,		*/
	vmove(i++,lhs);									/* Move to the info area.		*/
	vprint("                       *** QUEUE ENTRIES HELP MENU ***                      ");
	vmode(VMODE_CLEAR);								/* Clear all renditions.		*/
	vmode(VMODE_REVERSE);								/* Do it in reverse.			*/
	vmove(i++,lhs);
	vprint("FUNCTION KEYS                             CURSOR MOVEMENT                   ");
	vmove(i++,lhs);
	vprint("HELP  Help Menu                                                             ");
	vmove(i++,lhs);
	vprint("(1)    Select/De-select an entry toggle   (2)   Go to the TOP of the list   ");
	vmove(i++,lhs);
	vprint("(6)    Re-scan the jobs in the queue      (3)   Go to the BOTTOM of the list");
	vmove(i++,lhs);
	vprint("(7)    Hold/Release an entry toggle       (4)   Scroll UP a page in list    ");
	vmove(i++,lhs);
	vprint("(8)    Remove from queue and scratch      (5)   Scroll DOWN a page in list  ");
	vmove(i++,lhs);
	vprint("(10)   Change the print file                                                ");
	vmove(i++,lhs);
	vprint("(12)   Remove a file from queue only                                        ");
	vmove(i++,lhs);
	vprint("(13)   Change print file disposition                                        ");
	vmove(i++,lhs);
	vprint("(16)   Exit from the current queue                                          ");
	vmove(i++,lhs);
	vprint("Use the HELP key to bring this window back.                                 ");
	vmove(i,lhs);
	vprint("Depress any key to remove this window.                                      ");
	vmove(i,lhs+39);
	vgetm();
	restore_window(lhs,columns);							/* Display orig data under window.	*/
	vstate(1);									/* Restore where we were.		*/
}

static void alloc_queue_arrays(queflags,qnptr,numq)					/* Allocate the memory needed for the 	*/
long queflags, numq;									/*  columns of the queues.		*/
char *qnptr;
{
	char	srchname[2], queue_name[32], qttext[8], cputext[12];
	char	qstext[12];
	int 	i;
	long	flgs;

	if (numq)									/* If already have queues.		*/
	{
		qrows = numq;								/* Assign queue rows to number of queues.*/
		class_qname_typ = TRUE;							/* Got info from LPMAP & PQMAP files so	*/
	}										/*  the class is included in qname array.*/
	else										/* Use system services to gen. queues.	*/
	{
		class_qname_typ = FALSE;						/* qname array does not include class.	*/
		strcpy(srchname,"*");							/* Copy wildcard character to qname.	*/
		qrows = 0;								/* Set to 0 queues available.		*/
		if (((queflags & SEARCH_BATCH_ENABLED) != SEARCH_BATCH_ENABLED) &&
		    ((queflags & SEARCH_GENERIC_ENABLED) != SEARCH_GENERIC_ENABLED) &&
		    ((queflags & SEARCH_OUTPUT_ENABLED) != SEARCH_OUTPUT_ENABLED) )
		{
			return;								/* Don't search any queues.		*/
		}

		quibuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
		quibuf[0].buflen = strlen(srchname);
		quibuf[0].bufptr = (long *)srchname;					/* Pass the wildcard character.		*/
		quibuf[0].retlen = 0;

		quibuf[1].item_code = QUI$_QUEUE_FLAGS;					/* Set up call to get the queue type.	*/
		quibuf[1].buflen = sizeof(long);
		quibuf[1].bufptr = (long *)&flgs;
		quibuf[1].retlen = 0;

		quibuf[2].item_code = 0;						/* Mark the end of the buffer.		*/
		quibuf[2].buflen = 0;
		quibuf[2].bufptr = (long *)0;
		quibuf[2].retlen = 0;

		stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);
											/* Check if any queues available.	*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are queues available.	*/
		{
			if ( ((flgs & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC) &&	/* Is a generic queue and usage flag	*/
			     ((queflags & SEARCH_GENERIC_ENABLED) != SEARCH_GENERIC_ENABLED) );	/* is set so don't use.		*/
			else								/* Is batch queue and not generic and 	*/
			{								/* usage flag set so don't use.		*/
				if ( (((flgs & QUI$M_QUEUE_BATCH) == QUI$M_QUEUE_BATCH) &&
				     !((flgs & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC) ) &&
				     ((queflags & SEARCH_BATCH_ENABLED) != SEARCH_BATCH_ENABLED) );
				else 							/* Is an output queue and usage	flag is	*/
				{							/* set so don't use.			*/
					if ( (((flgs & QUI$M_QUEUE_PRINTER) == QUI$M_QUEUE_PRINTER) ||
					      ((flgs & QUI$M_QUEUE_TERMINAL) == QUI$M_QUEUE_TERMINAL) ) &&
					     ((queflags & SEARCH_OUTPUT_ENABLED) != SEARCH_OUTPUT_ENABLED) );
					else							
					{
						qrows++;				/* Increment number of queues.		*/
					}
				}
			}
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
		}
	}
	if (qrows)									/* If there are queues the allocate	*/
 	{										/*  memory for the columns in list.	*/
		if (numq)								/* If already have queue name array.	*/
		{
			qm_qn = qnptr;							/* Set queue name ptr to current ptr.	*/
		}
		else									/* Need to get the queue name array.	*/
		{									/* Get memory for the queue name array.	*/
			if (!(qm_qn = (char *)calloc(qrows,32))) que_mem_err(qrows*32);
		}
		if (!(qm_quet = (long *)calloc(qrows,sizeof(long)))) que_mem_err(qrows); /* Get mem for queue type long array.	*/
		if (!(qm_qt = (char *)calloc(qrows,8))) que_mem_err(qrows*8);		/* Get mem for queue type text array.	*/
		if (!(qm_bp = (long *)calloc(qrows,sizeof(long)))) que_mem_err(qrows);	/* Get memory for base priority array.	*/
		if (!(qm_jl = (long *)calloc(qrows,sizeof(long)))) que_mem_err(qrows);	/* Get memory for max job limit array.	*/
		if (!(qm_cpul = (char *)calloc(qrows,12))) que_mem_err(qrows*12);	/* Get memory for CPU limit array.	*/
		if (!(qm_qs = (char *)calloc(qrows,12))) que_mem_err(qrows*12);		/* Get memory for queue stat array.	*/
		if (!(qm_blmin = (long *)calloc(qrows,sizeof(long)))) que_mem_err(qrows); /* Get memory for job limit min array.*/
		if (!(qm_blmax = (long *)calloc(qrows,sizeof(long)))) que_mem_err(qrows); /* Get memory for job limit max array.*/
	}
	return;
}

static void alloc_job_arrays(queflags)							/* Allocate the memory needed for the 	*/
unsigned long queflags;									/*  columns of the chosen queue jobs.	*/
{
	char	jobname[40], jusername[13], sttext[15];
	unsigned long qflags;
	long exec_cnt, hold_cnt, pend_cnt, ret_cnt, tr_cnt;
	int wmon;

	qflags = QUI$M_SEARCH_WILDCARD;							/* Set up for queue context.		*/

	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = QUI$_EXECUTING_JOB_COUNT;				/* Get the executing job count.		*/
	qui_qbuf[2].buflen = sizeof(long);
	qui_qbuf[2].bufptr = (long *)&exec_cnt;
	qui_qbuf[2].retlen = 0;

	qui_qbuf[3].item_code = QUI$_HOLDING_JOB_COUNT;					/* Get the holding job count.		*/
	qui_qbuf[3].buflen = sizeof(long);
	qui_qbuf[3].bufptr = (long *)&hold_cnt;
	qui_qbuf[3].retlen = 0;

	qui_qbuf[4].item_code = QUI$_PENDING_JOB_COUNT;					/* Get the pending job count.		*/
	qui_qbuf[4].buflen = sizeof(long);
	qui_qbuf[4].bufptr = (long *)&pend_cnt;
	qui_qbuf[4].retlen = 0;

	qui_qbuf[5].item_code = QUI$_RETAINED_JOB_COUNT;				/* Get the retained job count.		*/
	qui_qbuf[5].buflen = sizeof(long);
	qui_qbuf[5].bufptr = (long *)&ret_cnt;
	qui_qbuf[5].retlen = 0;

	qui_qbuf[6].item_code = QUI$_TIMED_RELEASE_JOB_COUNT;				/* Get the timed released job count.	*/
	qui_qbuf[6].buflen = sizeof(long);
	qui_qbuf[6].bufptr = (long *)&tr_cnt;
	qui_qbuf[6].retlen = 0;

	qui_qbuf[7].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[7].buflen = 0;
	qui_qbuf[7].bufptr = (long *)0;
	qui_qbuf[7].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);
	jrows = 0;									/* Set to 0 jobs available.		*/

	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{
		jrows = (exec_cnt + hold_cnt + pend_cnt + ret_cnt + tr_cnt);		/* Estimate number of jobs in queue.	*/
	}
	if (jrows)									/* If there are jobs then allocate	*/
 	{										/*  memory for the columns in list.	*/
		if (!(qe_usrn = (char *)calloc(jrows,13+2))) que_mem_err(jrows*13);	/* Get memory for user name array.	*/
		if (!(qe_st   = (char *)calloc(jrows,15+2))) que_mem_err(jrows*15);	/* Get memory for job status text.	*/
		if (!(qe_ent  = (long *)calloc(jrows,sizeof(long)))) que_mem_err(jrows); /* Get memory for job entry array.	*/
		if (!(qe_jobs = (long *)calloc(jrows,sizeof(long)))) que_mem_err(jrows); /* Get memory for job status array.	*/
		if (!(qe_blks = (long *)calloc(jrows,sizeof(long)))) que_mem_err(jrows); /* Get memory for job size in blocks.	*/
		if (!(qe_jf   = (long *)calloc(jrows,sizeof(long)))) que_mem_err(jrows); /* Get memory for job form array.	*/
		if (!(qe_cpy  = (long *)calloc(jrows,sizeof(long)))) que_mem_err(jrows); /* Get memory for job copies array.	*/
		if (!(qe_jbn  = (char *)calloc(jrows,40+2))) que_mem_err(jrows*40);	/* Get memory for job name array.	*/
		if (!(del_entry = (long *)calloc(jrows,sizeof(long)))) que_mem_err(jrows); /* Get mem for delete job flag array.*/
	}
	return;
}

static void get_qt_text(flags,dtext)							/* Return the appropriate queue type	*/
long flags;										/* in text format.			*/
char *dtext;
{
	char itext[9], *tcpt, *dpt;

	strcpy(itext,"Busy    ");							/* Init to Unknown queue type.		*/

	if ((flags & QUI$M_QUEUE_BATCH) == QUI$M_QUEUE_BATCH)			strcpy(itext,"Batch   ");
	if ((flags & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC)		strcpy(itext,"Generic ");
	if ((flags & QUI$M_QUEUE_PRINTER) == QUI$M_QUEUE_PRINTER)		strcpy(itext,"Print   ");
	if ((flags & QUI$M_QUEUE_TERMINAL) == QUI$M_QUEUE_TERMINAL)		strcpy(itext,"Terminal");

	tcpt = itext;									/* Set ptr to beginning of text.	*/
	dpt = dtext;									/* Set ptr to beginning of destinamtion.*/
	while (*tcpt)
	{
		*dpt++ = *tcpt++;							/* Copy assigned text into dest. var.	*/
	}
	*dpt = '\0';									/* Null terminate the string.		*/
	return;
}

static void get_qs_text(qstat, dtext)							/* Return the appropriate queue status 	*/
long qstat;										/* in text format.			*/
char *dtext;										/* The highest status willprevail.	*/
{
	char itext[12], *tcpt, *dpt;

	strcpy(itext,"Busy       ");

	if (qstat == 0)							  strcpy(itext,"Executing  ");
	if ((qstat & QUI$M_QUEUE_ALIGNING) == QUI$M_QUEUE_ALIGNING)	  strcpy(itext,"Aligning   ");
	if ((qstat & QUI$M_QUEUE_CLOSED) == QUI$M_QUEUE_CLOSED)		  strcpy(itext,"Closed     ");
	if ((qstat & QUI$M_QUEUE_IDLE) == QUI$M_QUEUE_IDLE)		  strcpy(itext,"Idle       ");
	if ((qstat & QUI$M_QUEUE_LOWERCASE) == QUI$M_QUEUE_LOWERCASE)	  strcpy(itext,"On         ");
	if ((qstat & QUI$M_QUEUE_PAUSED) == QUI$M_QUEUE_PAUSED)		  strcpy(itext,"Paused     ");
	if ((qstat & QUI$M_QUEUE_PAUSING) == QUI$M_QUEUE_PAUSING)	  strcpy(itext,"Pausing    ");
	if ((qstat & QUI$M_QUEUE_REMOTE) == QUI$M_QUEUE_REMOTE)		  strcpy(itext,"Remote     ");
	if ((qstat & QUI$M_QUEUE_RESETTING) == QUI$M_QUEUE_RESETTING)	  strcpy(itext,"Resetting  ");
	if ((qstat & QUI$M_QUEUE_RESUMING) == QUI$M_QUEUE_RESUMING)	  strcpy(itext,"Resuming   ");
	if ((qstat & QUI$M_QUEUE_SERVER) == QUI$M_QUEUE_SERVER)		  strcpy(itext,"Server     ");
	if ((qstat & QUI$M_QUEUE_STALLED) == QUI$M_QUEUE_STALLED)      	  strcpy(itext,"Stalled    ");
	if ((qstat & QUI$M_QUEUE_STARTING) == QUI$M_QUEUE_STARTING)    	  strcpy(itext,"Starting   ");
	if ((qstat & QUI$M_QUEUE_STOPPED) == QUI$M_QUEUE_STOPPED)	  strcpy(itext,"Stopped    ");
	if ((qstat & QUI$M_QUEUE_STOPPING) == QUI$M_QUEUE_STOPPING)	  strcpy(itext,"Stopping   ");
	if ((qstat & QUI$M_QUEUE_UNAVAILABLE) == QUI$M_QUEUE_UNAVAILABLE) strcpy(itext,"Unavailable");

	tcpt = itext;									/* Set ptr to beginning of text.	*/
	dpt = dtext;									/* Set ptr to beginning of destination.	*/
	while (*tcpt)
	{
		*dpt++ = *tcpt++;							/* Copy assigned text into dest. var.	*/
	}
	*dpt = '\0';									/* Null terminate the string.		*/
	return;
}

static void get_cpul_text(cpulimit,dtext)						/* Return the appropriate CPU limit	*/
long cpulimit;										/* in text format.			*/
char *dtext;
{
	char itext[12], *tcpt, *dpt;
	long actsecs, days, hours, minutes, seconds;
	char nexti[4];

	memset(itext,' ',12);								/* Copy the blanks into string.		*/
	itext[12] = '\0';
	days = 0;
	hours = 0;
	minutes = 0;
	seconds = 0;
	actsecs = cpulimit / 100;							/* Calculate the actual cpu seconds.	*/
	days = actsecs / 86400;								/* Divide by secs in a day to get days.	*/
	actsecs %= (long)86400;								/* Return remainder of division.	*/
	hours = actsecs / 3600;								/* Divide by secs in hour to get hours.	*/
	actsecs %= (long)3600;								/* Return remainder of division.	*/
	minutes = actsecs / 60;								/* Divide by secs in min to get minutes.*/
	seconds = actsecs % (long)60;							/* Return remainding seconds.		*/ 

	itext[0] = '\0';
	sprintf(nexti,"%2d",days);							/* Convert long to string format.	*/
	strcat(itext,nexti);
	strcat(itext,"-");
	sprintf(nexti,"%02d",hours);							/* Convert long to string format.	*/
	strcat(itext,nexti);
	strcat(itext,":");								/* Concatenate start with character :	*/
	sprintf(nexti,"%02d",minutes);							/* Convert long to string format.	*/
	strcat(itext,nexti);
	strcat(itext,":");								/* Concatenate start with character -	*/
	sprintf(nexti,"%02d",seconds);							/* Convert long to string format.	*/
	strcat(itext,nexti);
	tcpt = itext;									/* Set ptr to beginning of text.	*/
	dpt = dtext;									/* Set ptr to beginning of destination.	*/
	while (*tcpt)
	{
		*dpt++ = *tcpt++;							/* Copy assigned text into return var.	*/
	}
	*dpt = '\0';									/* Null terminate the string.		*/
	return;
}

static int gen_queue_arrays(queflags,qnptr,numq)					/* This will generate the arays of data	*/
long queflags, numq;									/*  for the menu list.			*/
char *qnptr;
{
	char	qname[32], *cpt;							/* Queue name.				*/
	char	srchname[32], *sqnptr;							/* Search queue specifier.		*/
	char 	dtext[12];
	long 	*lpt, flgs, bprior, joblimit, cpulimit, qstat, blimmin, blimmax, tnq;
	int	cq, i, sl, contsfl, scnt;

	alloc_queue_arrays(queflags,qnptr,numq);					/* Allocate memory , set avail rows.	*/
	if (!qrows)
	{
		return(FALSE);								/* There are no queues available.	*/
	}
	cq = 0;
	if (numq)									/* Have list to search.			*/
	{
		tnq = numq;								/* Assign number of queues to manipulate.*/
		sqnptr = qm_qn + (cq * 31);						/* Get position in queue name array.	*/
		scnt = 0;	
		if (class_qname_typ)							/* Set ptr to start pos of actual name.	*/
		{									/*  (step past the class info.)		*/
			while (*sqnptr != ' ')
			{
				sqnptr++;
				scnt++;
			}
			sqnptr++;							/* Step past the seperating space char.	*/
			scnt++;
		}
		for (i = 0; i < 31-scnt; i++) srchname[i] = *sqnptr++;			/* Copy name into variable.		*/
		srchname[i] = '\0';							/* Null terminate the string.		*/
		tnq--;									/* Decrement number of queues.		*/
		sl = TRUE;								/* Loop is specified.			*/
	}
	else
	{
		strcpy(srchname,"*");							/* Copy wildcard character to qname.	*/
		sl = FALSE;
	}
	contsfl = TRUE;

	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(srchname);
	qui_qbuf[0].bufptr = (long *)srchname;						/* Pass the wildcard character.		*/
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_QUEUE_NAME;					/* Set up call to get the queue name.	*/
	qui_qbuf[1].buflen = 31;
	qui_qbuf[1].bufptr = (long *)qname;
	qui_qbuf[1].retlen = 0;

	qui_qbuf[2].item_code = QUI$_QUEUE_FLAGS;					/* Set up call to get the queue type.	*/
	qui_qbuf[2].buflen = sizeof(long);						/*  and the default form.		*/
	qui_qbuf[2].bufptr = (long *)&flgs;
	qui_qbuf[2].retlen = 0;

	qui_qbuf[3].item_code = QUI$_BASE_PRIORITY;					/* Set up call to get the base priority.*/
	qui_qbuf[3].buflen = sizeof(long);
	qui_qbuf[3].bufptr = (long *)&bprior;
	qui_qbuf[3].retlen = 0;

	qui_qbuf[4].item_code = QUI$_JOB_LIMIT;						/* Set up call to get the queue job limit*/
	qui_qbuf[4].buflen = sizeof(long);
	qui_qbuf[4].bufptr = (long *)&joblimit;
	qui_qbuf[4].retlen = 0;

	qui_qbuf[5].item_code = QUI$_CPU_LIMIT;						/* Set up call to get the queue CPU limit*/
	qui_qbuf[5].buflen = sizeof(long);
	qui_qbuf[5].bufptr = (long *)&cpulimit;
	qui_qbuf[5].retlen = 0;

	qui_qbuf[6].item_code = QUI$_QUEUE_STATUS;					/* Set up call to get the queue status.	*/
	qui_qbuf[6].buflen = sizeof(long);
	qui_qbuf[6].bufptr = (long *)&qstat;
	qui_qbuf[6].retlen = 0;

	qui_qbuf[7].item_code = QUI$_JOB_SIZE_MINIMUM;					/* Set up call to get minimum size job.	*/
	qui_qbuf[7].buflen = sizeof(long);
	qui_qbuf[7].bufptr = (long *)&blimmin;
	qui_qbuf[7].retlen = 0;

	qui_qbuf[8].item_code = QUI$_JOB_SIZE_MAXIMUM;					/* Set up call to get maximum size job.	*/
	qui_qbuf[8].buflen = sizeof(long);
	qui_qbuf[8].bufptr = (long *)&blimmax;
	qui_qbuf[8].retlen = 0;

	qui_qbuf[9].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[9].buflen = 0;
	qui_qbuf[9].bufptr = (long *)0;
	qui_qbuf[9].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);
											/* Check if any queues available.	*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if (stat_qui == JBC$_NOSUCHQUE)							/* Check LPMAP.DAT and PQMAP.DAT files.	*/
	{
		return(report_error(stat_qui,0,srchname,"Invalid queue, check LPMAP.DAT and PQMAP.DAT")); /* Report situation.	*/
	}
	while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL) && contsfl)		/* While there are queues available.	*/
	{										/* GENERATE THE ARRAYS.			*/
		if ( ((flgs & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC) &&		/* Is a generic queue and usage flag	*/
		     ((queflags & SEARCH_GENERIC_ENABLED) != SEARCH_GENERIC_ENABLED) );	/* is set so don't use.			*/
		else
		{
			if ( (((flgs & QUI$M_QUEUE_BATCH) == QUI$M_QUEUE_BATCH) &&	/* Is a batch queue and not generic and */
			     !((flgs & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC) ) && /* the usage flag is set so don't use.	*/
			     ((queflags & SEARCH_BATCH_ENABLED) != SEARCH_BATCH_ENABLED) );
			else 
			{
				if ( (((flgs & QUI$M_QUEUE_PRINTER) == QUI$M_QUEUE_PRINTER) || /* Is an output queue and usage	*/
				      ((flgs & QUI$M_QUEUE_TERMINAL) == QUI$M_QUEUE_TERMINAL) ) && /* flag is set so don't use.	*/
				     ((queflags & SEARCH_OUTPUT_ENABLED) != SEARCH_OUTPUT_ENABLED) );
				else							
				{
					if (!sl)					/* Need to get queue names.		*/
					{
						cpt = qm_qn + (cq * 31);		/* Point to pos in queue name array.	*/
						strcpy(cpt,qname);			/* Copy return qname into array.	*/
					}

					if (qm_quet)
					{
						lpt = qm_quet + cq;			/* Point to pos in queue type long array.*/
						*lpt = flgs;				/* Put return queue type into array.	*/
					}
					dtext[0] = '\0';				/* Begin with no text.			*/
					get_qt_text(flgs,dtext);			/* Get the appropriate queue type text.	*/

					if (qm_qt)
					{
						cpt = qm_qt + (cq * 8);			/* Point to pos in queue type array.	*/
						strcpy(cpt,dtext);			/* Copy text into the array.		*/
					}

					if (qm_bp)
					{
						lpt = qm_bp + cq;			/* Point to pos in base priority array.	*/
						*lpt = bprior;				/* Put return bprior into array.	*/
					}

					if (qm_jl)
					{
						lpt = qm_jl + cq;			/* Point to pos in max job limit array.	*/
						*lpt = joblimit;			/* Put return joblimit into array.	*/
					}

					dtext[0] = '\0';				/* Begin with no text.			*/
					if (qm_cpul)
					{
						cpt = qm_cpul + (cq * 11);		/* Point to pos in CPU limit array.	*/
						if (flgs & QUI$M_QUEUE_BATCH)		/* Only applies to Batch queues.	*/
						{
							get_cpul_text(cpulimit,dtext);	/* Convert CPU limit into text format.	*/
						}
						else strcpy(dtext,"           ");	/* Fill with blanks.			*/
						strcpy(cpt,dtext);			/* Copy text into the array.		*/
					}

					dtext[0] = '\0';				/* Begin with no text.			*/
					get_qs_text(qstat,dtext);			/* Get appropriate queue status text.	*/
					if (qm_qs)
					{
						cpt = qm_qs + (cq * 11);		/* Point to pos in queue status array.	*/
						strcpy(cpt,dtext);			/* Copy text into array.		*/
					}

					if (qm_blmin)
					{
						lpt = qm_blmin + cq;			/* Point to pos in job limit max array.	*/
						*lpt = blimmin;				/* Put return blimmin into array.	*/
					}

					if (qm_blmax)
					{
						lpt = qm_blmax + cq;			/* Point to pos in job limit max array.	*/
						*lpt = blimmax;				/* Put return blimmax into array.	*/
					}

					cq++;
				}
			}
		}
		if (sl)									/* Assign the next queue to search.	*/
		{
			if (tnq <= 0)  contsfl = FALSE;					/* No more queues to search.		*/
			else
			{
				sqnptr = qm_qn + (cq * 31);				/* Get position in queue name array.	*/
				scnt = 0;
				if (class_qname_typ)					/* Set ptr to start pos of actual name.	*/
				{							/*  (step past the class info.)		*/
					while (*sqnptr != ' ')
					{
						sqnptr++;
						scnt++;
					}
					sqnptr++;					/* Step past the seperating space char.	*/
					scnt++;
				}
				for (i = 0; i < 31-scnt; i++) srchname[i] = *sqnptr++; 	/* Copy name into variable.		*/
				srchname[i] = '\0';					/* Null terminate the string.		*/
				tnq--;							/* Decrement number of queues.		*/
			}
		}
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
	}
	return(TRUE);									/* Return info available.		*/
}

static void get_js_text(jstat, dtext)							/* Return the appropriate job status 	*/
long jstat;										/* in text format.			*/
char *dtext;										/* The highest status willprevail.	*/
{
	char itext[15], *tcpt, *dpt;

	strcpy(itext,"Unknown       ");

	if ((jstat & QUI$M_JOB_ABORTING) == QUI$M_JOB_ABORTING)	  	  strcpy(itext,"Aborting      ");
	if ((jstat & QUI$M_JOB_EXECUTING) == QUI$M_JOB_EXECUTING)	  strcpy(itext,"Executing     ");
	if ((jstat & QUI$M_JOB_HOLDING) == QUI$M_JOB_HOLDING)	  	  strcpy(itext,"Holding       ");
	if ((jstat & QUI$M_JOB_INACCESSIBLE) == QUI$M_JOB_INACCESSIBLE)	  strcpy(itext,"Inaccessible  ");
	if ((jstat & QUI$M_JOB_REFUSED) == QUI$M_JOB_REFUSED)		  strcpy(itext,"Refused       ");
	if ((jstat & QUI$M_JOB_REQUEUE) == QUI$M_JOB_REQUEUE)		  strcpy(itext,"Requeue       ");
	if ((jstat & QUI$M_JOB_RESTARTING) == QUI$M_JOB_RESTARTING)	  strcpy(itext,"Restarting    ");
	if ((jstat & QUI$M_JOB_RETAINED) == QUI$M_JOB_RETAINED) 	  strcpy(itext,"Retained      ");
	if ((jstat & QUI$M_JOB_STARTING) == QUI$M_JOB_STARTING)		  strcpy(itext,"Starting      ");
	if ((jstat & QUI$M_JOB_TIMED_RELEASE) == QUI$M_JOB_TIMED_RELEASE) strcpy(itext,"Timed Released");
	if ((jstat & QUI$M_JOB_SUSPENDED) == QUI$M_JOB_SUSPENDED)	  strcpy(itext,"Suspended     ");
	if ((jstat & QUI$M_JOB_PENDING) == QUI$M_JOB_PENDING)		  strcpy(itext,"Pending       ");

	tcpt = itext;									/* Set ptr to beginning of text.	*/
	dpt = dtext;									/* Set ptr to beginning of destination.	*/
	while (*tcpt)
	{
		*dpt++ = *tcpt++;							/* Copy assigned text into dest. var.	*/
	}
	*dpt = '\0';									/* Null terminate the string.		*/
	return;
}

static void add_queue_cols()								/* Init the queue list with data that	*/
{											/*  has been retrieved from system.	*/
	long type, width, length, icol;

	function = ADD_COLUMN;
	type = TEXTIT;
	width = 24;
	length = 31;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_qn,&length,&icol,&retcd);		/* Add queue name column.		*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 9;
	length = 8;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_qt,&length,&icol,&retcd);		/* Add queue type column.		*/
	function = ADD_COLUMN;
	type = ULONG;
	width = 5;
	length = sizeof(long);
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_bp,&length,&icol,&retcd);		/* Add base priorityu column.		*/
	function = ADD_COLUMN;
	type = ULONG;
	width = 4;
	length = sizeof(long);
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_jl,&length,&icol,&retcd);		/* Add job limit column.		*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 12;
	length = 11;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_cpul,&length,&icol,&retcd);		/* Add CPU limit column.		*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 12;
	length = 11;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_qs,&length,&icol,&retcd);		/* Add queue status column.		*/
	function = ADD_COLUMN;
	type = ULONG;
	width = 4;
	length = sizeof(long);
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_blmin,&length,&icol,&retcd);		/* Add block limit min column.		*/
	function = ADD_COLUMN;
	type = SEPARATOR;
	width = 3;
	length = 3;
	strcpy(dash,"-");
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,dash,&length,&icol,&retcd);		/* Add a dash column seperator column.	*/
	function = ADD_COLUMN;
	type = ULONG;
	width = 4;
	length = sizeof(long);
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,qm_blmax,&length,&icol,&retcd);		/* Add block limit max column.		*/
	return;
}

static void add_job_cols(rrow)								/* Init the queue list with job info 	*/
long rrow;										/*  retrieved from system.		*/
{
	long type, width, length, icol;
	long qtfl;

	function = ADD_COLUMN;
	type = TEXTIT;
	width = 31;
	length = 39;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,qe_jbn,&length,&icol,&retcd);		/* Add job name column.			*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 13;
	length = 12;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,qe_usrn,&length,&icol,&retcd);		/* Add user name column.		*/
	function = ADD_COLUMN;
	type = ULONG;
	width = 6;
	length = sizeof(long);
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,qe_ent,&length,&icol,&retcd);		/* Add job entry number column.		*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 15;
	length = 14;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,qe_st,&length,&icol,&retcd);		/* Add job status column.		*/
	qtfl = *(qm_quet + rrow);							/* Get the queue flags for current queue.*/
	if ( ((qtfl & QUI$M_QUEUE_BATCH) != QUI$M_QUEUE_BATCH) &&			/* Add columns if not a batch queue.	*/
	     ((qtfl & QUI$M_QUEUE_GENERIC) != QUI$M_QUEUE_GENERIC) )
	{
		function = ADD_COLUMN;
		type = SEPARATOR;
		width = 1;
		length = 1;
		strcpy(separ," ");
		icol = 0;								/* Is zero for add to end of list.	*/
		vlist(&function,&list_id,&type,&width,separ,&length,&icol,&retcd);	/* Add blank separator column.		*/
		function = ADD_COLUMN;
		type = ULONG;
		width = 6;
		length = sizeof(long);
		icol = 0;								/* Is zero for add to end of list.	*/
		vlist(&function,&list_id,&type,&width,qe_blks,&length,&icol,&retcd);	/* Add job size column.			*/
		function = ADD_COLUMN;
		type = ULONG;
		width = 4;
		length = sizeof(long);
		icol = 0;								/* Is zero for add to end of list.	*/
		vlist(&function,&list_id,&type,&width,qe_jf,&length,&icol,&retcd);	/* Add job form column.			*/
		function = ADD_COLUMN;
		type = ULONG;
		width = 3;
		length = sizeof(long);
		icol = 0;								/* Is zero for add to end of list.	*/
		vlist(&function,&list_id,&type,&width,qe_cpy,&length,&icol,&retcd);	/* Add job copies column.		*/
	}
	return;
}

static int gen_job_arrays(unsigned long queflags,int *rescan_flag)			/* This will generate the arays of data	*/
{											/*  for the specified queue.		*/
	char jobname[40], jusername[13], formname[32];
	char the_file[256];
	char dtext[15], *cpt;
	long num, size, jstat, copies, form, *lpt;
	int cq, wmon;
	unsigned long qflags, jflags;
	register int i, j;
	char username[34];
	
	*rescan_flag = FALSE;
	alloc_job_arrays(queflags);							/* Allocate memory , set avail rows.	*/
	if (!jrows)
	{
		return(FAILURE);							/* There are no jobs available.		*/
	}
	if (!(qe_frmn = (char *)calloc(jrows,32+2))) que_mem_err(jrows*32);		/* Get memory for the form name array.	*/

	qflags = QUI$M_SEARCH_WILDCARD;							/* Set up for queue context.		*/
	jflags = QUI$M_SEARCH_WILDCARD;
	if ((queflags & RESTRICT_JOBS_DISABLED) == RESTRICT_JOBS_DISABLED)
	{
		jflags |= QUI$M_SEARCH_ALL_JOBS;					/* Flag set so pick up all jobs.	*/
	}										/* Set up the buffers for system calls.	*/
	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	qui_jbuf[0].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_jbuf[0].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_jbuf[0].bufptr = (long *)&jflags;						/* which is flags.			*/
	qui_jbuf[0].retlen = 0;								/* return length address		*/

	qui_jbuf[1].item_code = QUI$_JOB_NAME;						/* Set up to return Job name.		*/
	qui_jbuf[1].buflen = 39;
	qui_jbuf[1].bufptr = (long *)jobname;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = QUI$_USERNAME;						/* Set up to return Account name.	*/
	qui_jbuf[2].buflen = 12;
	qui_jbuf[2].bufptr = (long *)jusername;
	qui_jbuf[2].retlen = 0;

	qui_jbuf[3].item_code = QUI$_ENTRY_NUMBER;					/* Set up to return entry number.	*/
	qui_jbuf[3].buflen = sizeof(long);
	qui_jbuf[3].bufptr = (long *)&num;
	qui_jbuf[3].retlen = 0;

	qui_jbuf[4].item_code = QUI$_JOB_SIZE;						/* Set up to return job size.		*/
	qui_jbuf[4].buflen = sizeof(long);
	qui_jbuf[4].bufptr = (long *)&size;
	qui_jbuf[4].retlen = 0;

	qui_jbuf[5].item_code = QUI$_JOB_STATUS;					/* Set up to return job status.		*/
	qui_jbuf[5].buflen = sizeof(long);
	qui_jbuf[5].bufptr = (long *)&jstat;
	qui_jbuf[5].retlen = 0;

	qui_jbuf[6].item_code = QUI$_FORM_NAME;						/* Set up to return job form name to 	*/
	qui_jbuf[6].buflen = 31;							/*  use in search name of display form	*/
	qui_jbuf[6].bufptr = (long *)formname;						/*  system call.			*/
	qui_jbuf[6].retlen = 0;

	qui_jbuf[7].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[7].buflen = 0;
	qui_jbuf[7].bufptr = (long *)0;
	qui_jbuf[7].retlen = 0;

	qui_fbuf[0].item_code = QUI$_FILE_COPIES;					/* Set up to return file copies.	*/
	qui_fbuf[0].buflen = sizeof(long);
	qui_fbuf[0].bufptr = (long *)&copies;
	qui_fbuf[0].retlen = 0;

	qui_fbuf[1].item_code = QUI$_FILE_SPECIFICATION;				/* Set up to return the file spec so	*/
	qui_fbuf[1].buflen = 255;							/*  can check if exists.		*/
	qui_fbuf[1].bufptr = (long *)the_file;
	qui_fbuf[1].retlen = 0;

	qui_fbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_fbuf[2].buflen = 0;
	qui_fbuf[2].bufptr = (long *)0;
	qui_fbuf[2].retlen = 0;

	quibuf[0].item_code = QUI$_SEARCH_NAME;						/* Set up search path.			*/
	quibuf[0].buflen = 31;								/* Pass in the form name.		*/
	quibuf[0].bufptr = (long *)formname;
	quibuf[0].retlen = 0;

	quibuf[1].item_code = QUI$_FORM_NUMBER;						/* Set up to return form number.	*/
	quibuf[1].buflen = sizeof(long);
	quibuf[1].bufptr = (long *)&form;
	quibuf[1].retlen = 0;

	quibuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	quibuf[2].buflen = 0;
	quibuf[2].bufptr = (long *)0;
	quibuf[2].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, (long) 0, (long) 0, (long) 0);
	cq = 0;										/* Set to 0 jobs available.		*/
											/* Check if any queues available.	*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		wmon = 0;
		vset_cursor_off();
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{
			if (jrows > 10)							 /* Output flashing working message.	*/
			{
				if (fmod(cq,8) == 0) wmon = work_message(wmon);
			}
			if ( ((jstat & QUI$M_JOB_INACCESSIBLE) == QUI$M_JOB_INACCESSIBLE) ||
			     ((jstat & QUI$M_JOB_REFUSED) == QUI$M_JOB_REFUSED)	)
			{									/* Dont't add the entry to list	*/
				jrows--;							/* because no privs.		*/
				nopriv_fl = TRUE;
			}
			else
			{								/* Get the # copies for the file.	*/
				stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, qui_fbuf,
						&iosb, (long) 0, (long) 0);
				stat_qui = iosb.status;

				lpt = del_entry + cq;
				if (!fexists(the_file))					/* Check if file exists.		*/
				{
					cuserid(username);
					if (strcmp(username,jusername))			/* Not the owner so don't mark.		*/
					{
						*lpt = 0;
					}
					else
					{
						*rescan_flag = TRUE;
						*lpt = 1;
					}
				}
				else	*lpt = 0;

				if (qe_jbn)
				{
					cpt = qe_jbn + (cq * 39);			/* Point to pos in job name array.	*/
					strcpy(cpt,jobname);				/* Copy return jobname into array.	*/
				}

				if (qe_usrn)
				{
					cpt = qe_usrn + (cq * 12);			/* Point to pos in user name array.	*/
					strcpy(cpt,jusername);				/* Copy return username into array.	*/
				}

				if (qe_cpy)
				{
					lpt = qe_cpy + cq;				/* Point to pos in copies array.	*/
					*lpt = copies;					/* Put return copies into array.	*/
				}

				if (qe_blks)
				{
					lpt = qe_blks + cq;				/* Point to pos in job size array.	*/
					*lpt = size;					/* Put return size into array.		*/
				}

				if (qe_ent)
				{
					lpt = qe_ent + cq;				/* Point to pos in job entry array.	*/
					*lpt = num;					/* Put return num into array.		*/
				}

				if (qe_jobs)
				{
					lpt = qe_jobs + cq;				/* Point to pos in job status array.	*/
					*lpt = jstat;					/* Put return num into array.		*/
				}

				dtext[0] = '\0';					/* Begin with no text.			*/
				get_js_text(jstat,dtext);				/* Get appropriate job status text.	*/

				if (qe_st)
				{
					cpt = qe_st + (cq * 14);			/* Point to pos in job status text array.*/
					strcpy(cpt,dtext);				/* Copy text into array.		*/
				}

				if (qe_frmn)
				{
					cpt = qe_frmn + (cq * 31);			/* Point to pos in form name array.	*/
					strcpy(cpt,formname);				/* Copy return formname into array.	*/
				}

				cq++;
			}
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
		}
		vset_cursor_on();
	}
	if (!jrows)
	{
		return(FAILURE);							/* Jobs in queue but not accessible.	*/
	}

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, (long) 0, (long) 0, (long) 0);

	for (i = 0; i < jrows; i++)							/* Get the form numbers.		*/
	{
		j = 0;
		if (qe_frmn)
		{
			cpt = qe_frmn + (i * 31);					/* Point to pos in form name array.	*/
			j = 0;
			while (*cpt != ' ' && j < 31) formname[j++] = *cpt++;		/* Copy formname into field.		*/
		}
		formname[j] = '\0';							/* Null terminate the form name.	*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FORM, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		if (!((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))) form = 0;	/* Is no form number or default.	*/
		if (qe_jf)
		{
			lpt = qe_jf + i;						/* Point to pos in form array.		*/
			*lpt = form;							/* Put return form number into array.	*/
		}
	}
	return(SUCCESS);								/* Return info available.		*/
}

static int hold_release_job(ndx)							/* Function to release the job with 	*/
long ndx;										/*  status of holding, timed release or	*/
{											/*  retained otherwise it will hold the */
	long entry_num, job_status, *lpt;						/*  job.				*/
	int tfl;
	register i;

	lpt = qe_ent + ndx;								/* Set ptr to pos in job entry arrya.	*/
	entry_num = *lpt;								/* Put value into variable.		*/
	lpt = qe_jobs + ndx;								/* Set ptr to pos in job status array.	*/
	job_status = *lpt;								/* Put value into variable.		*/
											/* Set up buffer for system call.	*/
	qui_jbuf[0].item_code = SJC$_QUEUE;						/* Set up to input queue name.		*/
	qui_jbuf[0].buflen = strlen(curr_queue);
	qui_jbuf[0].bufptr = (long *)curr_queue;
	qui_jbuf[0].retlen = 0;

	qui_jbuf[1].item_code = SJC$_ENTRY_NUMBER;					/* Set up to input job entry number.	*/
	qui_jbuf[1].buflen = sizeof(long);
	qui_jbuf[1].bufptr = (long *)&entry_num;
	qui_jbuf[1].retlen = 0;

	if (((job_status & QUI$M_JOB_HOLDING) == QUI$M_JOB_HOLDING) ||
	    ((job_status & QUI$M_JOB_RETAINED) == QUI$M_JOB_RETAINED))
	{										/* Can be released.			*/
		tfl = 1;
		qui_jbuf[2].item_code = SJC$_NO_HOLD;					/* Input to release the job.		*/
		qui_jbuf[2].buflen = 0;
		qui_jbuf[2].bufptr = (long *)0;
		qui_jbuf[2].retlen = 0;
	}
	else if ((job_status & QUI$M_JOB_TIMED_RELEASE) == QUI$M_JOB_TIMED_RELEASE)
	{										/* Can be released.			*/
		tfl = 1;
		qui_jbuf[2].item_code = SJC$_NO_AFTER_TIME;				/* Input to release the job.		*/
		qui_jbuf[2].buflen = 0;
		qui_jbuf[2].bufptr = (long *)0;
		qui_jbuf[2].retlen = 0;
	}
	else if ((job_status & QUI$M_JOB_PENDING) == QUI$M_JOB_PENDING)
	{										/* Can be changed to a holding status. 	*/
		tfl = 0;
		qui_jbuf[2].item_code = SJC$_HOLD;					/* Input to hold the job.		*/
		qui_jbuf[2].buflen = 0;
		qui_jbuf[2].bufptr = (long *)0;
		qui_jbuf[2].retlen = 0;
	}
	else  return(FALSE);								/* Function not applicable.		*/

	qui_jbuf[3].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[3].buflen = 0;
	qui_jbuf[3].bufptr = (long *)0;
	qui_jbuf[3].retlen = 0;

	stat_ss = sys$sndjbcw((long) 0, SJC$_ALTER_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
	stat_snd = iosb.status;
	if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))  return(TRUE);
	else
	{
		if (stat_snd == JBC$_NOSUCHENT)						/* Is an invalid entry.			*/
		{
			report_error(stat_snd,entry_num,"","Hold/Release entry");
			return(TRUE);							/* Set so will rescan the queue.	*/
		}
		else return(report_error(stat_snd,entry_num,"","Hold/Release entry"));	/* Report the situation.		*/
	}
}

static int delete_job(ndx)
long ndx;
{
	char *screen, wcc;								/* Ptr to screen to call vwang.		*/
	char vwfunc, lines, term[2], no_mod[2];						/* Working vars. for vwang.		*/
	char the_file[256], disp_name[255];						/* File name				*/
	char jobname[40], curr_job[40], *jcpt;
	register int i;
	unsigned long qflags;
	int state, c, return_code;
#include "quemgmt1.d"

	i = 0;
	if (qe_jbn)
	{
		jcpt = qe_jbn + (ndx * 39);						/* Point to position in job name array.	*/
		i = 0;
		while (*jcpt != ' ' && i < 39) jobname[i++] = *jcpt++;			/* Copy job name from array to var.	*/
	}
	jobname[i] = '\0';								/* Null terminate the string.		*/

	qflags = QUI$M_SEARCH_WILDCARD;
											/* Set up the buffers for system calls.	*/
	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	qui_jbuf[0].item_code = QUI$_JOB_NAME;						/* Set up to return Job name.		*/
	qui_jbuf[0].buflen = 39;
	qui_jbuf[0].bufptr = (long *)curr_job;
	qui_jbuf[0].retlen = 0;

	qui_jbuf[1].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[1].buflen = 0;
	qui_jbuf[1].bufptr = (long *)0;
	qui_jbuf[1].retlen = 0;

	quibuf[0].item_code = QUI$_FILE_SPECIFICATION;					/* Set up to return file spec.		*/
	quibuf[0].buflen = 255;
	quibuf[0].bufptr = (long *)the_file;
	quibuf[0].retlen = 0;

	quibuf[1].item_code = 0;							/* Mark the end of the buffer.		*/
	quibuf[1].buflen = 0;
	quibuf[1].bufptr = (long *)0;
	quibuf[1].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, (long) 0, (long) 0, (long) 0);
											/* Check if any queues available.	*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{
			if (strcmp(curr_job,jobname) == 0) break;			/* Job name equals current job.		*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
		}
		if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* Job name was found.			*/
		{									/* Call to get the file identification.	*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
			if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL));	/* File was found.			*/
			else return(report_error(stat_qui,0,the_file,"Delete file, getting file")); /* Report situation.	*/
		}
		else
		{
			if (stat_qui == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_qui,0,jobname,"Delete file, getting job");
				return(TRUE);						/* Set so will rescan the queue.	*/
			}
			else return(report_error(stat_qui,0,jobname,"Delete file, getting job")); /* Report situation.		*/
		}
	}
	else return(report_error(stat_qui,0,curr_queue,"Delete file, getting queue"));	/* Report situation.			*/

	state = FALSE;									/* Assume did not delete.		*/
	jcpt = the_file;								/* Point to the file specification.	*/
	while (*jcpt != ']')  jcpt++;							/* Step to beginning of filename.	*/
	i = 0;
	while (*jcpt++) disp_name[i++] = *jcpt;						/* Copy file name into temp var.	*/
	disp_name[i] = '\0';								/* Null terminate the string.		*/

	if ((screen = (char *)malloc(WSB_LENGTH)) == 0) que_mem_err(WSB_LENGTH);	/* Get some memory for screen.		*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,25,PLAIN_TEXT,"*** DELETE FILE SUBROUTINE ***");			/* Layout the screen.			*/
	wput(screen, 7, 5,PLAIN_TEXT,"NOTE:  Aborting this routine will stop the scratch of the file as well");
	wput(screen, 8, 5,PLAIN_TEXT,"       as stop the removal of the job from the queue.");
	wput(screen,11, 2,PLAIN_TEXT,"Scratch file:");
	wput(screen,12, 2,BOLD_TEXT,disp_name);
	wput(screen,23,10,PLAIN_TEXT,"Push (ENTER) to continue or (16) to abort.");

	wcc = UNLOCK_KEYBOARD | POSITION_CURSOR;					/* Set the Wang Control Character.	*/
	screen[WCC_BYTE] = wcc;
	vwfunc = DISPLAY_AND_READ;							/* Set vwang function.			*/
	lines = WSB_ROWS;
	ws_erap(FULL_SCREEN);
	do
	{
		vwang(&vwfunc,screen,&lines,"0016X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		if (term[0] == '1' && term[1] == '6') state = FALSE;			/* Hit the abort PF16 key.		*/
		else if (term[0] == '0' && term[1] == '0')				/* Hit the return key to continue.	*/
		{
			state = remove_job(ndx);					/* Remove job, if success set TRUE.	*/
			if (state) LIB$DELETE_FILE(&p_desc);				/* Now go delete it			*/
		}
		else vbell();
	} while ((no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6'))); 	/* Repeat until valid return key.	*/

	free(screen);									/* Free up screen memory.		*/
	return(state);									/* Return TRUE if successfully deleted.	*/
}

static int remove_job(ndx)								/* Function to delete from the current	*/
long ndx;										/*  queue a pending or executing job.	*/
{
	long entry_num, *lpt;

	lpt = qe_ent + ndx;								/* Set ptr to pos in job entry arrya.	*/
	entry_num = *lpt;
											/* Set up buffer for system call.	*/
	qui_jbuf[0].item_code = SJC$_QUEUE;						/* Set up to input queue name.		*/
	qui_jbuf[0].buflen = strlen(curr_queue);
	qui_jbuf[0].bufptr = (long *)curr_queue;
	qui_jbuf[0].retlen = 0;

	qui_jbuf[1].item_code = SJC$_ENTRY_NUMBER;					/* Set up to input job entry number.	*/
	qui_jbuf[1].buflen = sizeof(long);
	qui_jbuf[1].bufptr = (long *)&entry_num;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[2].buflen = 0;
	qui_jbuf[2].bufptr = (long *)0;
	qui_jbuf[2].retlen = 0;

	stat_ss = sys$sndjbcw((long) 0, SJC$_DELETE_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
	stat_snd = iosb.status;
	if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL)) return(TRUE);		/* Set to True because removed entry.	*/
	else
	{
		if (stat_snd == JBC$_NOSUCHENT)						/* Is an invalid entry.			*/
		{
			report_error(stat_snd,entry_num,"","Removing entry");
			return(TRUE);							/* Set so will rescan the queue.	*/
		}
		else  return(report_error(stat_snd,entry_num,"","Removing entry"));	/* Report the situation.		*/
	}
}

static int change_disp(ndx,queflags)							/* Function to change the disposition 	*/
long ndx, queflags;									/*  of a job.				*/
{
	char *screen, wcc;								/* Ptr to screen to call vwang.		*/
	char vwfunc, lines, term[2], no_mod[2];						/* Working vars. for vwang.		*/
	long entry_num, rent_num, fflgs, *lpt;
	long fnum, copies, jstat;
	char jobn[40], *pjbn, fdisp[7];
	long char_num;
	char the_file[256], jobname[40];
	int c, state, return_code;
	register int i;
	unsigned long jflags, sqflags, qflags;

	sqflags = QUI$M_SEARCH_WILDCARD;						/* Set up for queue context.		*/
	jflags = QUI$M_SEARCH_WILDCARD;
	if ((queflags & RESTRICT_JOBS_DISABLED) == RESTRICT_JOBS_DISABLED)
	{
		jflags |= QUI$M_SEARCH_ALL_JOBS;					/* Flag set so pick up all jobs.	*/
	}										/* Set up the buffers for system calls.	*/

	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&sqflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = QUI$_QUEUE_FLAGS;					/* Set up to return queue flags.	*/
	qui_qbuf[2].buflen = sizeof(long);
	qui_qbuf[2].bufptr = (long *)&qflags;
	qui_qbuf[2].retlen = 0;

	qui_qbuf[3].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[3].buflen = 0;
	qui_qbuf[3].bufptr = (long *)0;
	qui_qbuf[3].retlen = 0;

	qui_jbuf[0].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_jbuf[0].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_jbuf[0].bufptr = (long *)&jflags;						/* which is flags.			*/
	qui_jbuf[0].retlen = 0;								/* return length address		*/

	qui_jbuf[1].item_code = QUI$_ENTRY_NUMBER;					/* Set up to return Job entry number.	*/
	qui_jbuf[1].buflen = sizeof(long);
	qui_jbuf[1].bufptr = (long *)&rent_num;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = QUI$_JOB_NAME;						/* Set up to return Job name.		*/
	qui_jbuf[2].buflen = 39;
	qui_jbuf[2].bufptr = (long *)jobname;
	qui_jbuf[2].retlen = 0;

	qui_jbuf[3].item_code = QUI$_JOB_STATUS;					/* Set up to return job status.		*/
	qui_jbuf[3].buflen = sizeof(long);
	qui_jbuf[3].bufptr = (long *)&jstat;
	qui_jbuf[3].retlen = 0;

	qui_jbuf[4].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[4].buflen = 0;
	qui_jbuf[4].bufptr = (long *)0;
	qui_jbuf[4].retlen = 0;

	qui_fbuf[0].item_code = QUI$_FILE_SPECIFICATION;				/* Set up to return file spec.		*/
	qui_fbuf[0].buflen = 255;
	qui_fbuf[0].bufptr = (long *)the_file;
	qui_fbuf[0].retlen = 0;

	qui_fbuf[1].item_code = QUI$_FILE_COPIES;					/* Set up to return file copies.	*/
	qui_fbuf[1].buflen = sizeof(long);
	qui_fbuf[1].bufptr = (long *)&copies;
	qui_fbuf[1].retlen = 0;

	qui_fbuf[2].item_code = QUI$_FILE_FLAGS;					/* Set up to return file flags.		*/
	qui_fbuf[2].buflen = sizeof(long);
	qui_fbuf[2].bufptr = (long *)&fflgs;
	qui_fbuf[2].retlen = 0;

	qui_fbuf[3].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_fbuf[3].buflen = 0;
	qui_fbuf[3].bufptr = (long *)0;
	qui_fbuf[3].retlen = 0;

	if (qe_ent)
	{
		lpt = qe_ent + ndx;							/* Set ptr to pos in job entry arrya.	*/
		entry_num = *lpt;
	}
	i = 0;
	if (qe_jbn)
	{
		pjbn = qe_jbn + (ndx * 39);						/* Point to position in job name array.	*/
		i = 0;
		while (*pjbn != ' ' && i < 39) jobn[i++] = *pjbn++;			/* Copy job name into variable.		*/
	}
	jobn[i] = '\0';									/* Null terminate the string.		*/

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);

	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{									/* Assume only one file in print job.	*/
			if (rent_num == entry_num)					/* Is the job we want.			*/
			{								/* Get the file flags.			*/
				stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, qui_fbuf,
						&iosb, (long) 0, (long) 0);
				stat_qui = iosb.status;
				break;							/* Get out of loop.			*/
			}
			else								/* Try again.				*/
			{
				stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
				stat_qui = iosb.status;
			}
		}
		if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL));		/* Successfully found file.		*/
		else
		{
			if (stat_qui == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_qui,entry_num,"","Change entry disposition, getting job");
				return(TRUE);						/* Set so will rescan the queue.	*/
			}
			else  return(report_error(stat_qui,entry_num,"","Change entry disposition, getting job")); /* Report.	*/
		}
	}
	else return(report_error(stat_qui,0,curr_queue,"Change entry disposition, getting queue")); /* Report situation.	*/

	if ((fflgs & QUI$M_FILE_DELETE) == QUI$M_FILE_DELETE)
	{
		strcpy(fdisp,"DELETE");
	}
	else
	{
		strcpy(fdisp,"SAVE  ");
	}

	if ((screen = (char *)malloc(WSB_LENGTH)) == 0) que_mem_err(WSB_LENGTH);	/* Get some memory for screen.		*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,20,PLAIN_TEXT,"*** CHANGE ENTRY DISPOSITION ***");		/* Layout the screen.			*/
	wput(screen, 5, 7,PLAIN_TEXT," (4) - SAVE   - don't delete file after printing");
	wput(screen, 7, 7,PLAIN_TEXT," (5) - DELETE - delete file after printing");
	wput(screen, 9, 7,PLAIN_TEXT,"(16) - NO CHANGE");
	wput(screen,12, 5,PLAIN_TEXT,"Job name:");
	wput(screen,12,26,BOLD_TEXT,jobn);
	wput(screen,13, 5,PLAIN_TEXT,"Current Disposition:");
	wput(screen,13,26,BOLD_TEXT,fdisp);
	wput(screen,23, 5,PLAIN_TEXT,"Push the appropriate PF key or (16) to exit.");

	wcc = UNLOCK_KEYBOARD | POSITION_CURSOR;					/* Set the Wang Control Character.	*/
	screen[WCC_BYTE] = wcc;
	vwfunc = DISPLAY_AND_READ;							/* Set vwang function.			*/
	lines = WSB_ROWS;
	ws_erap(FULL_SCREEN);
	state = TRUE;									/* Assume there will be a change.	*/
	do
	{
		vwang(&vwfunc,screen,&lines,"00040516X",term,no_mod);			/* Call Wang emulation to fill screen.	*/
											/* Hit the abort PF16 key or return key.*/
		if ((term[0] == '1' && term[1] == '6') || (term[0] == '0' && term[1] == '0'))
		{
			state = FALSE;							/* No change so set to FALSE.		*/
		}
		else if (term[0] == '0' && term[1] == '4')				/* Hit PF4 key.  Change dispositin to	*/
		{									/*  SAVE.				*/
			qui_jbuf[0].item_code = SJC$_QUEUE;				/* Set up to input queue name.		*/
			qui_jbuf[0].buflen = strlen(curr_queue);
			qui_jbuf[0].bufptr = (long *)curr_queue;
			qui_jbuf[0].retlen = 0;

			qui_jbuf[1].item_code = SJC$_ENTRY_NUMBER;			/* Set up to input job entry number.	*/
			qui_jbuf[1].buflen = sizeof(long);
			qui_jbuf[1].bufptr = (long *)&entry_num;
			qui_jbuf[1].retlen = 0;

			qui_jbuf[2].item_code = SJC$_NO_DELETE_FILE;			/* Change job to be save file status.	*/
			qui_jbuf[2].buflen = 0;
			qui_jbuf[2].bufptr = (long *)0;
			qui_jbuf[2].retlen = 0;

			qui_jbuf[3].item_code = 0;					/* Mark the end of the buffer.		*/
			qui_jbuf[3].buflen = 0;
			qui_jbuf[3].bufptr = (long *)0;
			qui_jbuf[3].retlen = 0;

			stat_ss = sys$sndjbcw((long) 0, SJC$_ALTER_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_snd = iosb.status;
			if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL)) state = TRUE;
			else
			{                             
				if (stat_snd == JBC$_NOSUCHENT)				/* Is an invalid entry.			*/
				{
					report_error(stat_snd,entry_num,"","Changing entry disposition to SAVE");
					return(TRUE);					/* Set so will rescan the queue.	*/
				}
				else return(report_error(stat_snd,entry_num,"","Changing entry disposition to SAVE"));
			}
		}
		else if (term[0] == '0' && term[1] == '5')				/* Hit PF5 key.  Change disposition to	*/
		{									/*  DELETE.				*/
			qui_jbuf[0].item_code = SJC$_QUEUE;				/* Set up to input queue name.		*/
			qui_jbuf[0].buflen = strlen(curr_queue);
			qui_jbuf[0].bufptr = (long *)curr_queue;
			qui_jbuf[0].retlen = 0;

			qui_jbuf[1].item_code = SJC$_ENTRY_NUMBER;			/* Set up to input job entry number.	*/
			qui_jbuf[1].buflen = sizeof(long);
			qui_jbuf[1].bufptr = (long *)&entry_num;
			qui_jbuf[1].retlen = 0;

			qui_jbuf[2].item_code = 0;					/* Mark the end of the buffer.		*/
			qui_jbuf[2].buflen = 0;
			qui_jbuf[2].bufptr = (long *)0;
			qui_jbuf[2].retlen = 0;
											/* First delete the original job.	*/
			stat_ss = sys$sndjbcw((long) 0, SJC$_DELETE_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_snd = iosb.status;

			if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL)) state = TRUE;
			else
			{
				if (stat_snd == JBC$_NOSUCHENT)				/* Is an invalid entry.			*/
				{
					report_error(stat_snd,entry_num,"","Changing entry disposition, delete job");
					return(TRUE);					/* Set so will rescan the queue.	*/
				}
				else return(report_error(stat_snd,entry_num,"","Changing entry disposition, delete job"));
			}
											/* Then, queue job with delete disp.	*/
			fflgs |= Q_DELETE_FILE;
			fflgs |= Q_HOLD_JOB;
			if (qe_jf)
			{
				lpt = qe_jf + ndx;					/* Set ptr to pos in form number array.	*/
				fnum = *lpt;
			}
											/* Queue job.				*/
			state = TRUE;
			return_code = que_job(PRINT_QUEUE,curr_queue,the_file,jobname,copies,fnum,fflgs);
			if (return_code != SS$_NORMAL)
			{
				return(report_error(stat_snd,entry_num,"","Changing entry disposition to DELETE"));
			}
		}
		else if (term[0] == '0' && term[1] == '6')				/* Hit PF6 key.  Change dispossition to */
		{									/*  RETAIN.				*/

			return(report_error(0,0,"","Not a supported function - RETAIN"));

			qui_jbuf[0].item_code = SJC$_QUEUE;				/* Set up to input queue name.		*/
			qui_jbuf[0].buflen = strlen(curr_queue);
			qui_jbuf[0].bufptr = (long *)curr_queue;
			qui_jbuf[0].retlen = 0;

			qui_jbuf[1].item_code = SJC$_ENTRY_NUMBER;			/* Set up to input job entry number.	*/
			qui_jbuf[1].buflen = sizeof(long);
			qui_jbuf[1].bufptr = (long *)&entry_num;
			qui_jbuf[1].retlen = 0;

			qui_jbuf[3].item_code = 0;					/* Mark the end of the buffer.		*/
			qui_jbuf[3].buflen = 0;
			qui_jbuf[3].bufptr = (long *)0;
			qui_jbuf[3].retlen = 0;

			stat_ss = sys$sndjbcw((long) 0, SJC$_DELETE_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_snd = iosb.status;

			if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL)) state = TRUE;
			else
			{
				if (stat_snd == JBC$_NOSUCHENT)				/* Is an invalid entry.			*/
				{
					report_error(stat_snd,entry_num,"","Changing entry disposition, retain job");
					return(TRUE);					/* Set so will rescan the queue.	*/
				}
				else return(report_error(stat_snd,entry_num,"","Changing entry disposition, delete job"));
			}
											/* Then, queue job with retain disp.	*/
/*			fflgs |= Q_RETAIN_JOB;	*/
			if (qe_jf)
			{
				lpt = qe_jf + ndx;					/* Set ptr to pos in form number array.	*/
				fnum = *lpt;
			}
											/* Queue job.				*/
			state = TRUE;
			return_code = que_job(PRINT_QUEUE,curr_queue,the_file,jobname,copies,fnum,fflgs);
			if (return_code != SS$_NORMAL)
			{
				return(report_error(stat_snd,entry_num,"","Changing entry disposition to RETAIN"));
			}
		}
		else vbell();
	} while ((no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6'))); 	/* Repeat until valid return key.	*/

	free(screen);									/* Free up screen memory.		*/
	return(state);									/* True if changed otherwise false.	*/
}

static int requeue_job(ndx,queflags,pc,numq)						/* Function to requeue an executing 	*/
long ndx, queflags, numq;								/*  job.				*/
char *pc;
{
	char *screen, wcc;								/* Ptr to screen to call vwang.		*/
	char vwfunc, lines, term[2], no_mod[2];						/* Working vars. for vwang.		*/
	long entry_num, rent_num, fflgs, *lpt;
	char jobn[40], *pjbn, fdisp[7];
	int c, state, caller;
	register int i;
	unsigned long jflags, qflags;

	qflags = QUI$M_SEARCH_WILDCARD;							/* Set up for queue context.		*/
	jflags = QUI$M_SEARCH_WILDCARD;
	if ((queflags & RESTRICT_JOBS_DISABLED) == RESTRICT_JOBS_DISABLED)
	{
		jflags |= QUI$M_SEARCH_ALL_JOBS;					/* Flag set so pick up all jobs.	*/
	}										/* Set up the buffers for system calls.	*/
	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	qui_jbuf[0].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_jbuf[0].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_jbuf[0].bufptr = (long *)&jflags;						/* which is flags.			*/
	qui_jbuf[0].retlen = 0;								/* return length address		*/

	qui_jbuf[1].item_code = QUI$_ENTRY_NUMBER;					/* Set up to return Job entry number.	*/
	qui_jbuf[1].buflen = sizeof(long);
	qui_jbuf[1].bufptr = (long *)&rent_num;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[2].buflen = 0;
	qui_jbuf[2].bufptr = (long *)0;
	qui_jbuf[2].retlen = 0;

	qui_fbuf[0].item_code = QUI$_FILE_FLAGS;					/* Set up to return file flags.		*/
	qui_fbuf[0].buflen = sizeof(long);
	qui_fbuf[0].bufptr = (long *)&fflgs;
	qui_fbuf[0].retlen = 0;

	qui_fbuf[1].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_fbuf[1].buflen = 0;
	qui_fbuf[1].bufptr = (long *)0;
	qui_fbuf[1].retlen = 0;

	lpt = qe_ent + ndx;								/* Set ptr to pos in job entry arrya.	*/
	entry_num = *lpt;
	pjbn = qe_jbn + (ndx * 39);							/* Point to position in job name array.	*/
	i = 0;
	while (*pjbn != ' ' && i < 39) jobn[i++] = *pjbn++;				/* Copy job name into variable.		*/
	jobn[i] = '\0';									/* Null terminate the string.		*/

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);

	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{									/* Assume only one file in print job.	*/
			if (rent_num == entry_num)					/* Is the job we want.			*/
			{								/* Get the file flags.			*/
				stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, qui_fbuf,
						&iosb, (long) 0, (long) 0);
				stat_qui = iosb.status;
				break;							/* Get out of loop.			*/
			}
			else								/* Try again.				*/
			{
				stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
				stat_qui = iosb.status;
			}
		}
		if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL));		/* Successfully found file.		*/
		else
		{
			if (stat_qui == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_qui,entry_num,"","Requeue entry, getting job");
				return(TRUE);						/* Set so will rescan the queue.	*/
			}
			else  return(report_error(stat_qui,entry_num,"","Requeue entry, getting job")); /* Report situation.	*/
		}
	}
	else return(report_error(stat_qui,0,curr_queue,"Requeue entry, getting queue")); /* Report situation.			*/

	qui_jbuf[0].item_code = SJC$_QUEUE;						/* Set up to input queue name.		*/
	qui_jbuf[0].buflen = strlen(curr_queue);
	qui_jbuf[0].bufptr = (long *)curr_queue;
	qui_jbuf[0].retlen = 0;

	qui_jbuf[1].item_code = SJC$_ENTRY_NUMBER;					/* Set up to input job entry number.	*/
	qui_jbuf[1].buflen = sizeof(long);
	qui_jbuf[1].bufptr = (long *)&entry_num;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = SJC$_REQUEUE;						/* Set up to requeue job.		*/
	qui_jbuf[2].buflen = 0;
	qui_jbuf[2].bufptr = (long *)0;
	qui_jbuf[2].retlen = 0;

	qui_jbuf[3].item_code = SJC$_HOLD;						/* Set up to HOLD job.			*/
	qui_jbuf[3].buflen = 0;
	qui_jbuf[3].bufptr = (long *)0;
	qui_jbuf[3].retlen = 0;
	
	qui_jbuf[4].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[4].buflen = 0;
	qui_jbuf[4].bufptr = (long *)0;                 
	qui_jbuf[4].retlen = 0;
											/* First abort the job and requeue with	*/
	stat_ss = sys$sndjbcw((long) 0, SJC$_ABORT_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);	/* HOLD status		*/
	stat_snd = iosb.status;
	if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL)) state = TRUE;
	else
	{
		if (stat_snd == JBC$_NOSUCHENT)						/* Is an invalid entry.			*/
		{
			report_error(stat_snd,entry_num,"","Requeueing entry");
			return(TRUE);							/* Set so will rescan the queue.	*/
		}
		else return(report_error(stat_snd,entry_num,"","Requeueing entry")); 	/* Report the situation.		*/
	}
											/* Then, change the page parameters.	*/
	caller = 1;									/* Set so give non-modifiable fields.	*/
	change_job(ndx,pc,numq,caller);
	state = TRUE;									/* Set so will do display.		*/
	return(state);									/* True if changed otherwise false.	*/
}

static int submit_remove_job(ndx,nqname,copies,fnum)					/* Submit job to new queue and remove 	*/
long ndx, copies, fnum;									/*  job from old queue.			*/
char *nqname;
{
	char the_file[256];								/* File name				*/
	char jobname[40], curr_job[40], *jcpt;
	register int i;
	unsigned long qflags, jflags, fflgs;
	int state, c, return_code;
	long jstat;
#include "quemgmt1.d"

	jcpt = qe_jbn + (ndx * 39);							/* Point to position in job name array.	*/
	i = 0;
	while (*jcpt != ' ' && i < 39) jobname[i++] = *jcpt++;				/* Copy job name from array to var.	*/
	jobname[i] = '\0';								/* Null terminate the string.		*/

	qflags = QUI$M_SEARCH_WILDCARD;
	jflags = QUI$M_SEARCH_WILDCARD;
	jflags |= QUI$M_SEARCH_ALL_JOBS;						/* Flag set so pick up all jobs.	*/
											/* Set up the buffers for system calls.	*/
	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	qui_jbuf[0].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_jbuf[0].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_jbuf[0].bufptr = (long *)&jflags;						/* which is flags.			*/
	qui_jbuf[0].retlen = 0;								/* return length address		*/

	qui_jbuf[1].item_code = QUI$_JOB_NAME;						/* Set up to return Job name.		*/
	qui_jbuf[1].buflen = 39;
	qui_jbuf[1].bufptr = (long *)curr_job;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = QUI$_JOB_STATUS;					/* Set up to return job status.		*/
	qui_jbuf[2].buflen = sizeof(long);
	qui_jbuf[2].bufptr = (long *)&jstat;
	qui_jbuf[2].retlen = 0;

	qui_jbuf[3].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[3].buflen = 0;
	qui_jbuf[3].bufptr = (long *)0;
	qui_jbuf[3].retlen = 0;

	quibuf[0].item_code = QUI$_FILE_SPECIFICATION;					/* Set up to return file spec.		*/
	quibuf[0].buflen = 255;
	quibuf[0].bufptr = (long *)the_file;
	quibuf[0].retlen = 0;

	quibuf[1].item_code = QUI$_FILE_FLAGS;						/* Set up to return file flags.		*/
	quibuf[1].buflen = sizeof(long);
	quibuf[1].bufptr = (long *)&fflgs;
	quibuf[1].retlen = 0;

	quibuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	quibuf[2].buflen = 0;
	quibuf[2].bufptr = (long *)0;
	quibuf[2].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, (long) 0, (long) 0, (long) 0);
											/* Check if any queues available.	*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{
			if (strcmp(curr_job,jobname) == 0) break;			/* Job name equals current job.		*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
		}
		if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* Job name was found.			*/
		{									/* Call to get the file identification.	*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
			if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL));	/* File was found.			*/
			else return(report_error(stat_qui,0,the_file,"Change class, getting file")); /* Report situation.	*/
		}
		else
		{
			if (stat_qui == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_qui,0,jobname,"Change class, getting job");
				return(TRUE);						/* Set so will rescan the queue.	*/
			}
			else return(report_error(stat_qui,0,jobname,"Change class, getting job")); /* Report situation.		*/
		}
	}
	else return(report_error(stat_qui,0,curr_queue,"Change class, getting queue"));	/* Report situation.			*/

	if ((jstat & QUI$M_JOB_HOLDING) == QUI$M_JOB_HOLDING)				/* Set to HOLD job if already on HOLD	*/
	{
		fflgs |= Q_HOLD_JOB;
	}
	return_code = que_job(PRINT_QUEUE,nqname,the_file,jobname,copies,fnum,fflgs);	/* Submit job to new queue.		*/

	if (return_code == SS$_NORMAL)
	{
		state = remove_job(ndx);						/* Remove job, if success set TRUE.	*/
		return(TRUE);
	}
	else	return(FALSE);
}

static int get_first_last(jobname)							/* Get the first and last print pages.	*/
char *jobname;
{
	char curr_job[40], *jcpt;
	register int i;
	unsigned long qflags, jflags;
	int state, c;

	qflags = QUI$M_SEARCH_WILDCARD;
	jflags = QUI$M_SEARCH_WILDCARD;
	jflags |= QUI$M_SEARCH_ALL_JOBS;						/* Flag set so pick up all jobs.	*/
											/* Set up the buffers for system calls.	*/
	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	qui_jbuf[0].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_jbuf[0].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_jbuf[0].bufptr = (long *)&jflags;						/* which is flags.			*/
	qui_jbuf[0].retlen = 0;								/* return length address		*/

	qui_jbuf[1].item_code = QUI$_JOB_NAME;						/* Set up to return Job name.		*/
	qui_jbuf[1].buflen = 39;
	qui_jbuf[1].bufptr = (long *)curr_job;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[2].buflen = 0;
	qui_jbuf[2].bufptr = (long *)0;
	qui_jbuf[2].retlen = 0;

	quibuf[0].item_code = QUI$_FIRST_PAGE;						/* Set up to return first page.		*/
	quibuf[0].buflen = sizeof(long);
	quibuf[0].bufptr = (long *)&firstp;
	quibuf[0].retlen = 0;

	quibuf[1].item_code = QUI$_LAST_PAGE;						/* Set up to return last page.		*/
	quibuf[1].buflen = sizeof(long);
	quibuf[1].bufptr = (long *)&lastp;
	quibuf[1].retlen = 0;

	quibuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	quibuf[2].buflen = 0;
	quibuf[2].bufptr = (long *)0;
	quibuf[2].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, (long) 0, (long) 0, (long) 0);
											/* Check if any queues available.	*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{
			if (strcmp(curr_job,jobname) == 0) break;			/* Job name equals current job.		*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
		}
		if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* Job name was found.			*/
		{									/* Call to get the file identification.	*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
			if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL));	/* File was found.			*/
			else return(report_error(stat_qui,0,jobname,"Change status, getting file")); /* Report situation.	*/
		}
		else
		{
			if (stat_qui == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_qui,0,jobname,"Change status, getting job");
				return(TRUE);						/* Set so will rescan the queue.	*/
			}
			else return(report_error(stat_qui,0,jobname,"Change startus, getting job")); /* Report situation.	*/
		}
	}
	else return(report_error(stat_qui,0,curr_queue,"Change status, getting queue"));	/* Report situation.		*/
}

static void genq_empty_list(rrow,rcol,key)
long *rrow, *rcol, *key;
{
	long type, width, length, icol;
	long st_row, st_col;
	char mesg[WSB_COLS];

	mlist_id = EMPTY_LIST;
	strcpy(mesg,"               THERE ARE NO QUEUES AVAILABLE!");
	qrows = 1;									/* Set so can print message.		*/
	init_qmenu(mlist_id);								/* Create queue list structure.		*/
	qrows = 0;									/* Set back for other comparisons.	*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 46;
	length = 46;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&mlist_id,&type,&width,mesg,&length,&icol,&retcd); 		/* Add mesg column.			*/
	function = DISPLAY_LIST;
	st_row = 0;
	st_col = 0;									/* Display the jobs in queue.		*/
	*rrow = st_row;
	*rcol = st_col;	
	vlist(&function,&mlist_id,&st_row,&st_col,rrow,rcol,result_list,key,&retcd);
	function = FREE_LIST;
	vlist(&function,&mlist_id,&retcd);						/* Free up the memory of the queue menu.*/
	free_qmenu(mlist_id);								/* Free all memory associated with	*/
	return;										/*  main queue menu.			*/
}

static void genj_empty_list(rrow,rcol,key)
long *rrow, *rcol, *key;
{
	long type, width, length, icol;
	long st_row, st_col;
	char mesg[WSB_COLS];
	int etype;
	int adjust_se;

	etype = TRUE;									/* Init to TRUE for an empty list.	*/
	if (nopriv_fl) 	strcpy(mesg,"         YOU DO NOT HAVE ACCESS TO THE ENTRIES IN THE QUEUE!");
	else		strcpy(mesg,"                THERE ARE NO ENTRIES IN THE QUEUE!          ");
	jrows = 1;									/* Set so can print message.		*/
	init_jobs(etype,*rrow);								/* Create job list structure.		*/
	jrows = 0;									/* Set back for other comparisons.	*/
	function = ADD_COLUMN;
	type = TEXTIT;
	width = 60;
	length = 60;
	icol = 0;									/* Is zero for add to end of list.	*/
	vlist(&function,&list_id,&type,&width,mesg,&length,&icol,&retcd); 		/* Add mesg column.			*/
	function = DISPLAY_LIST;
	st_row = 0;
	st_col = 0;									/* Display the message.			*/
	*rrow = st_row;
	*rcol = st_col;
	vlist(&function,&list_id,&st_row,&st_col,rrow,rcol,result_list,key,&retcd,&adjust_se);
	function = FREE_LIST;
	vlist(&function,&list_id,&retcd);						/* Free up memory for queue entries.	*/
	free_qentries(etype);								/* Free header/footer memory.		*/
	if (list_id == PQUEUE_LIST) vue_batch_que = FALSE;				/* Set FALSE because mem is free.	*/
	else 			    vue_print_que = FALSE;

	return;
}

static int check_result_list()								/* Return TRUE if items selected.	*/
{
	int i;
	long *lpt;

	for (i = 0; i < jrows; i++)							/* Go through the result list.		*/
	{
		lpt = (long *)(result_list + i);
		if (*lpt)  return(TRUE);						/* An item is selected.			*/
	}	
	return(FALSE);									/* No items are selected.		*/
}

static int rescan_jobs(rrow,queflags)							/* Return if still have entries.	*/
long rrow;
unsigned long queflags;
{
	int available, i, etype;
	long rcol, key, qtfl;
	int rs_flag;

	qtfl = *(qm_quet + rrow);							/* Get the queue flags for current queue.*/
	if ( ((qtfl & QUI$M_QUEUE_BATCH) == QUI$M_QUEUE_BATCH) ||
	     ((qtfl & QUI$M_QUEUE_GENERIC) == QUI$M_QUEUE_GENERIC) )
	{
		list_id = BQUEUE_LIST;
	}
	else  list_id = PQUEUE_LIST;
	function = FREE_LIST;
	vlist(&function,&list_id,&retcd);
	rcol = 0;
	rs_flag = TRUE;
	while (rs_flag)
	{
		available = gen_job_arrays(queflags,&rs_flag);				/* Generate job columns, return # rows.	*/
		if (rs_flag)								/* If need to delete entries where files*/
		{									/*  don't exist then rescan first before*/
			register cnt;							/*  displaying.				*/
			long *dptr;

			for (cnt = 0; cnt < jrows; cnt++)
			{
				dptr = del_entry + cnt;					/* Point to pos in del entry array.	*/
				if (*dptr == 1)
				{
					remove_job(cnt,1);				/* Remove the job from the queue.	*/
				}                                     
			}
		}
	}

	if (!available)									/* There are no jobs to choose from.	*/
	{
		genj_empty_list(&rrow,&rcol,&key);
		return(FALSE);	  							/* No more entries so return FALSE.	*/
	}

	etype = FALSE;
	init_jobs(etype,rrow);								/* Create new job list structure.	*/
	add_job_cols(rrow);								/* Init the new job list structure.	*/

	return(TRUE);									/* Still have entries so return TRUE.	*/
}

static int change_job(ndx,pc,numq,caller)						/* Change information on a queue entry.	*/
long ndx, numq;
char *pc;
int caller;
{
	char *screen, wcc;								/* Ptr to screen to call vwang.		*/
	char vwfunc, lines, term[2], no_mod[2];						/* Working vars. for vwang.		*/
	char *cpt, jobname[40], formname[32], sfnum[4], scpy[4];
	char q_print_class[15], spclass[2];
	char sfirst[6], slast[6];
	long job_size;
	long entry_num, copies, fnum, *lpt;
	int valid, state, change_queue, pc_flag;
	register int i;
	int return_code;
	char nqname[32];
                                                      
	strcpy(q_print_class,pc);							/* Copy queue print class to local	*/
	change_queue = FALSE;
	state = FALSE;									/* Set to assume no change.		*/
	lpt = qe_ent + ndx;								/* Set ptr to pos in job entry array.	*/
	entry_num = *lpt;
	lpt = qe_cpy + ndx;								/* Set ptr to pos in job copies array.	*/
	copies = *lpt;
	cpt = qe_jbn + (ndx * 39);							/* Set ptr to pos in job name array.	*/
	i = 0;
	while (*cpt != ' ' && i < 39) jobname[i++] = *cpt++;				/* Copy job name into temp var.		*/
	jobname[i] = '\0';								/* Null terminate the string.		*/
	cpt = qe_frmn + (ndx * 31);							/* Set ptr to pos in form name array.	*/
	i = 0;
	while (*cpt != ' ' && i < 31) formname[i++] = *cpt++;				/* Copy form name into temp var.	*/
	formname[i] = '\0';								/* Null terminate the string.		*/
	lpt = qe_jf + ndx;								/* Set ptr to pos in form number array.	*/
	fnum = *lpt;
	lpt = qe_blks + (ndx * sizeof(long));						/* Point to pos in job size array.	*/
	job_size = *lpt;								/* Put size into string.		*/
	sprintf(sfnum,"%03d",fnum);							/* Convert long to string.		*/
	sprintf(scpy,"%03d",copies);
	get_first_last(jobname);							/* Get the first and last print pages.	*/
	sprintf(sfirst,"%5d",firstp);
	sprintf(slast,"%5d",lastp);

	if ((screen = (char *)malloc(WSB_LENGTH)) == 0) que_mem_err(WSB_LENGTH);	/* Get some memory for screen.		*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,20,PLAIN_TEXT,"*** CHANGE AN ENTRY SUBROUTINE ***");		/* Layout the screen.			*/
	wput(screen, 4, 5,PLAIN_TEXT,"Changing job:");
	wput(screen, 4,20,BOLD_TEXT,jobname);
	wput(screen, 7, 5,PLAIN_TEXT,"Form:");
	wput(screen, 7,20,PLAIN_TEXT,formname);
	if (caller)									/* If caller = 1 then called from 	*/
	{										/*  requeue so no modify.		*/
		wput(screen, 8,20,PLAIN_TEXT,sfnum);
	}
	else
	{
		wput(screen, 8,20,NUMERIC_FIELD,sfnum);
	}
	wput(screen,10, 5,PLAIN_TEXT,"Copies:");
	if (caller)									/* If caller = 1 then called from 	*/
	{										/*  requeue so no modify.		*/
		wput(screen,10,20,PLAIN_TEXT,scpy);
	}
	else
	{
		wput(screen,10,20,NUMERIC_FIELD,scpy);
	}
	wput(screen,12, 5,PLAIN_TEXT,"Print Class:");
        if (strlen(q_print_class) > 1)							/* If more than one class assigned	*/
	{										/*  to the queue.			*/
		pc_flag = 2;								/* Set flag to use two line mode.	*/
		strcpy(spclass," ");
		wput(screen,12,20,PLAIN_TEXT,q_print_class);
		wput(screen,13, 5,PLAIN_TEXT,"New Class:");
		if (caller)								/* If caller = 1 then called from 	*/
		{									/*  requeue so no modify.		*/
			wput(screen,13,20,PLAIN_TEXT,spclass);
		}
		else
		{
			wput(screen,13,20,UPCASE_FIELD,spclass);
		}
	}
	else
	{
		pc_flag = 1;								/* Set flag to use one line mode.	*/
		strcpy(spclass,q_print_class);
		if (caller)								/* If caller = 1 then called from 	*/
		{									/*  requeue so no modify.		*/
			wput(screen,12,20,PLAIN_TEXT,spclass);
		}
		else
		{
			wput(screen,12,20,UPCASE_FIELD,spclass);
		}
	}
	wput(screen,15, 5,PLAIN_TEXT,"START printing at page:");
	wput(screen,15,30,NUMERIC_FIELD,sfirst);
	wput(screen,16, 5,PLAIN_TEXT,"END printing at page:");
	wput(screen,16,30,NUMERIC_FIELD,slast);
	
	wput(screen,24, 2,PLAIN_TEXT,"Change the information as appropriate and press (ENTER), (16) to exit.");

	wcc = UNLOCK_KEYBOARD | POSITION_CURSOR;					/* Set the Wang Control Character.	*/
	screen[WCC_BYTE] = wcc;
	vwfunc = DISPLAY_AND_READ_ALTERED;
	lines = WSB_ROWS;
	ws_erap(FULL_SCREEN);
	do
	{
		vwang(&vwfunc,screen,&lines,"0016X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		wget(screen, 8,20,sfnum);						/* Get the new form number.		*/
		wget(screen,10,20,scpy);						/* Get the new number of copies.	*/
		if (pc_flag == 2) wget(screen,13,20,spclass);				/* Get the new print class.		*/
		else 		  wget(screen,12,20,spclass);
		wget(screen,15,30,sfirst);						/* Get the new start page.		*/
		wget(screen,16,30,slast);						/* Get the new end .			*/

		valid = TRUE;								/* Assume valid.			*/
		if ( (no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6')) )
		{
			fnum = atol(sfnum);						/* Convert string to long.		*/
			copies = atol(scpy);
			firstp = atol(sfirst);
			lastp = atol(slast);

			quibuf[0].item_code = QUI$_SEARCH_NUMBER;			/* Set up search path.			*/
			quibuf[0].buflen = sizeof(long);				/* Pass in the form number.		*/
			quibuf[0].bufptr = (long *)&fnum;
			quibuf[0].retlen = 0;

			quibuf[1].item_code = QUI$_FORM_NAME;				/* Set up to return form name.		*/
			quibuf[1].buflen = 31;
			quibuf[1].bufptr = (long *)formname;
			quibuf[1].retlen = 0;

			quibuf[2].item_code = 0;					/* Mark the end of the buffer.		*/
			quibuf[2].buflen = 0;
			quibuf[2].bufptr = (long *)0;
			quibuf[2].retlen = 0;

			stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);

			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FORM, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;

			if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))	/* Is form number valid?		*/
			{
				if (copies > 0 && copies < 256)				/* New copies input is valid.		*/
				{
					if ((*spclass >= 'A') && (*spclass <= 'Z'))
					{						/* Get the new queue name.		*/
						return_code = find_new_qname(nqname,spclass,numq);
						if (return_code)
						{
							if (firstp == 0 && lastp == 0)	/* Print from beginning to end.		*/
							{
								valid = TRUE;		/* All entries are valid.		*/
							}
							else if ((firstp > 0 && lastp > 0) &&
								 (firstp < lastp))
							{
								valid = TRUE;		/* All entries are valid.		*/
							}
							else if ((firstp > 0 && lastp == 0) ||
								 (firstp == 0 && lastp > 0))
							{
								valid = TRUE;		/* All entries are valid.		*/
							}
							else
							{
								valid = FALSE;
								if (firstp < 0) wput(screen,15,30,NUMERIC_FIELD|BLINK_FAC,sfirst);
								if (lastp < 0)  wput(screen,16,30,NUMERIC_FIELD|BLINK_FAC,slast);
								screen[CURSOR_COL_BYTE] = 0; /* Reset cursor position.		*/
								screen[CURSOR_ROW_BYTE] = 0;
								wcc |= SOUND_ALARM;	/* Set the Wang Control Character.	*/
								screen[WCC_BYTE] = wcc;
							}
						}
						else
						{
							valid = FALSE;
							if (pc_flag == 2) wput(screen,13,20,UPCASE_FIELD|BLINK_FAC,spclass);
							else		  wput(screen,12,20,UPCASE_FIELD|BLINK_FAC,spclass);
							screen[CURSOR_COL_BYTE] = 0;	/* Reset cursor position.		*/
							screen[CURSOR_ROW_BYTE] = 0;
							wcc |= SOUND_ALARM;		/* Set the Wang Control Character.	*/
							screen[WCC_BYTE] = wcc;
						}
					}
					else
					{
						valid = FALSE;
						if (pc_flag == 2) wput(screen,13,20,UPCASE_FIELD|BLINK_FAC,spclass);
						else		  wput(screen,12,20,UPCASE_FIELD|BLINK_FAC,spclass);
						screen[CURSOR_COL_BYTE] = 0;		/* Reset cursor position.		*/
						screen[CURSOR_ROW_BYTE] = 0;
						wcc |= SOUND_ALARM;			/* Set the Wang Control Character.	*/
						screen[WCC_BYTE] = wcc;
					}
				}
				else
				{
					valid = FALSE;
					wput(screen,10,20,NUMERIC_FIELD|BLINK_FAC,scpy);
					screen[CURSOR_COL_BYTE] = 0;			/* Reset cursor position.		*/
					screen[CURSOR_ROW_BYTE] = 0;
					wcc |= SOUND_ALARM;				/* Set the Wang Control Character.	*/
					screen[WCC_BYTE] = wcc;
				}
			}
			else								/* Invalid form number.			*/
			{
				valid = FALSE;
				wput(screen,8,20,NUMERIC_FIELD|BLINK_FAC,sfnum);
				screen[CURSOR_COL_BYTE] = 0;				/* Reset cursor position.		*/
				screen[CURSOR_ROW_BYTE] = 0;
				wcc |= SOUND_ALARM;					/* Set the Wang Control Character.	*/
				screen[WCC_BYTE] = wcc;
			}
		}									/* End if no mod section.		*/
	} while (!valid && (no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6'))); /* Repeat until data is valid.	*/

	if ((no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6')))		/* Don't write unless they press enter	*/
	{										/* and they've changed something.	*/
											/* Call system services to change job.	*/
		qui_jbuf[0].item_code = SJC$_ENTRY_NUMBER;				/* Set up to input job entry number.	*/
		qui_jbuf[0].buflen = sizeof(long);
		qui_jbuf[0].bufptr = (long *)&entry_num;
		qui_jbuf[0].retlen = 0;

		qui_jbuf[1].item_code = SJC$_FORM_NUMBER;				/* Set up for new form number.		*/
		qui_jbuf[1].buflen = sizeof(long);
		qui_jbuf[1].bufptr = (long *)&fnum;
		qui_jbuf[1].retlen = 0;

		qui_jbuf[2].item_code = SJC$_FILE_COPIES;				/* Set up for new number of copies.	*/
		qui_jbuf[2].buflen = sizeof(long);
		qui_jbuf[2].bufptr = (long *)&copies;
		qui_jbuf[2].retlen = 0;

		if (firstp == 0)
		{
			qui_jbuf[3].item_code = SJC$_NO_FIRST_PAGE;			/* Set up to start with first page.	*/
			qui_jbuf[3].buflen = sizeof(long);
			qui_jbuf[3].bufptr = (long *)&firstp;
			qui_jbuf[3].retlen = 0;
		}
		else
		{
			qui_jbuf[3].item_code = SJC$_FIRST_PAGE;			/* Set up for new first page.		*/
			qui_jbuf[3].buflen = sizeof(long);
			qui_jbuf[3].bufptr = (long *)&firstp;
			qui_jbuf[3].retlen = 0;
		}

		if (lastp == 0)
		{
			qui_jbuf[4].item_code = SJC$_NO_LAST_PAGE;			/* Set up to print to end.		*/
			qui_jbuf[4].buflen = sizeof(long);
			qui_jbuf[4].bufptr = (long *)&lastp;
			qui_jbuf[4].retlen = 0;
		}
		else
		{
			qui_jbuf[4].item_code = SJC$_LAST_PAGE;				/* Set up for new last page.		*/
			qui_jbuf[4].buflen = sizeof(long);
			qui_jbuf[4].bufptr = (long *)&lastp;
			qui_jbuf[4].retlen = 0;
		}

		qui_jbuf[5].item_code = 0;						/* Mark the end of the buffer.		*/
		qui_jbuf[5].buflen = 0;
		qui_jbuf[5].bufptr = (long *)0;
		qui_jbuf[5].retlen = 0;

		stat_ss = sys$sndjbcw((long) 0, SJC$_ALTER_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_snd = iosb.status;
		if ((stat_snd == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
		{
			lpt = qe_cpy + ndx;						/* Set ptr to pos in job copies array.	*/
			*lpt = copies;							/* Put new value into array.		*/
			lpt = qe_jf + ndx;						/* Set ptr to pos in form number array.	*/
			*lpt = fnum;							/* Put new value into array.		*/
			cpt = qe_frmn + (ndx * 31);					/* Set ptr to pos in form name array.	*/
			for (i = 0; i < 31; i++) *cpt++ = formname[i];			/* Copy form name into var.		*/
			state = TRUE;							/* Set TRUE because entry was changed.	*/
			if (strpos(q_print_class,spclass) < 0)				/* If new class is not in current queue	*/
			{
				change_queue = submit_remove_job(ndx,nqname,copies,fnum); /* Submit job to new queue and Remove	*/
				if (change_queue) state = 2; 				/*  the job from current queue.		*/
			}
		}
		else
		{
			if (stat_snd == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_snd,entry_num,"","Changing entry");
				state = TRUE;						/* Set so will rescan the queue.	*/
			}
			else report_error(stat_snd,entry_num,"","Changing entry");	/* Report the situation.		*/
		}
	}
	free(screen);									/* Release the screen memory.		*/
	return(state);									/* Return TRUE if changed.		*/
}

static int c_mntdef_qform(fkey)								/* Change the default queue form number.*/
long fkey;
{
	char *screen, wcc;								/* Ptr to screen to call vwang.		*/
	char vwfunc, lines, term[2], no_mod[2];						/* Working vars. for vwang.		*/
	char formname[32], sfnum[4];
	long form, state, valid;

	state = FALSE;									/* Assume no change to the queue.	*/

	qui_jbuf[0].item_code = QUI$_SEARCH_NAME;					/* Set up to search queue name.		*/
	qui_jbuf[0].buflen = strlen(curr_queue);
	qui_jbuf[0].bufptr = (long *)curr_queue;
	qui_jbuf[0].retlen = 0;

	if (fkey == fn9_key) qui_jbuf[1].item_code = QUI$_FORM_NAME;			/* Set to get current mounted form name.*/
	else 		     qui_jbuf[1].item_code = QUI$_DEFAULT_FORM_NAME;		/* Set to get current default form name.*/
	qui_jbuf[1].buflen = 31;
	qui_jbuf[1].bufptr = (long *)formname;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[2].buflen = 0;
	qui_jbuf[2].bufptr = (long *)0;
	qui_jbuf[2].retlen = 0;

	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Set up search path.			*/
	qui_qbuf[0].buflen = 31;							/* Pass in the form name.		*/
	qui_qbuf[0].bufptr = (long *)formname;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_FORM_NUMBER;					/* Set up to return form number.	*/
	qui_qbuf[1].buflen = sizeof(long);
	qui_qbuf[1].bufptr = (long *)&form;
	qui_qbuf[1].retlen = 0;

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);

	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui != JBC$_NORMAL) || (stat_ss != SS$_NORMAL))
	{ 										/* Report situation.			*/
		if (fkey == fn9_key) return(report_error(stat_qui,0,curr_queue,"Change mounted form, getting queue"));
		else return(report_error(stat_qui,0,curr_queue,"Change default form, getting queue"));
	}

	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FORM, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui != JBC$_NORMAL) || (stat_ss != SS$_NORMAL))
	{										 /* Report situation.			*/
		if (fkey == fn9_key) return(report_error(stat_qui,0,formname,"Change queue, getting current mounted form"));
		else return(report_error(stat_qui,0,formname,"Change queue, getting current default form"));
	}
	sprintf(sfnum,"%03d",form);							/* Convert long to string.		*/

	if ((screen = (char *)malloc(WSB_LENGTH)) == 0) que_mem_err(WSB_LENGTH);	/* Get some memory for screen.		*/
	wsc_init(screen,0,0);								/* Initialize the screen layout.	*/

	wput(screen, 1,20,PLAIN_TEXT,"*** CHANGE A QUEUE SUBROUTINE ***");		/* Layout the screen.			*/
	wput(screen, 4, 5,PLAIN_TEXT,"Changing queue:");
	wput(screen, 4,20,BOLD_TEXT,curr_queue);
	if (fkey == fn9_key) wput(screen, 7, 5,PLAIN_TEXT,"Mounted form:");
	else		     wput(screen, 7, 5,PLAIN_TEXT,"Default form:");
	wput(screen, 7,20,PLAIN_TEXT,formname);
	wput(screen, 8,20,NUMERIC_FIELD,sfnum);
	wput(screen,24, 2,PLAIN_TEXT,"Change the information as appropriate and press (ENTER), (16) to exit.");

	wcc = UNLOCK_KEYBOARD | POSITION_CURSOR;					/* Set the Wang Control Character.	*/
	screen[WCC_BYTE] = wcc;
	vwfunc = DISPLAY_AND_READ_ALTERED;
	lines = WSB_ROWS;
	ws_erap(FULL_SCREEN);
	do
	{
		vwang(&vwfunc,screen,&lines,"0016X",term,no_mod);			/* Call Wang emulation to fill screen.	*/

		wget(screen, 8,20,sfnum);						/* Get the new form number.		*/

		valid = TRUE;								/* Assume valid.			*/
		if ( (no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6')) )
		{
			form = atol(sfnum);						/* Convert string to long.		*/

			quibuf[0].item_code = QUI$_SEARCH_NUMBER;			/* Set up search path.			*/
			quibuf[0].buflen = sizeof(long);				/* Pass in the form number.		*/
			quibuf[0].bufptr = (long *)&form;
			quibuf[0].retlen = 0;

			quibuf[1].item_code = QUI$_FORM_NAME;				/* Set up to return form name.		*/
			quibuf[1].buflen = 31;
			quibuf[1].bufptr = (long *)formname;
			quibuf[1].retlen = 0;

			quibuf[2].item_code = 0;					/* Mark the end of the buffer.		*/
			quibuf[2].buflen = 0;
			quibuf[2].bufptr = (long *)0;
			quibuf[2].retlen = 0;

			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FORM, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;

			if ((stat_qui != JBC$_NORMAL) || (stat_ss != SS$_NORMAL))	/* Is form number valid?		*/
			{
				valid = FALSE;						/* No so enter again.			*/
				wput(screen,8,20,NUMERIC_FIELD|BLINK_FAC,sfnum);
				screen[CURSOR_COL_BYTE] = 0;				/* Reset cursor position.		*/
				screen[CURSOR_ROW_BYTE] = 0;
				wcc |= SOUND_ALARM;					/* Set the Wang Control Character.	*/
				screen[WCC_BYTE] = wcc;
			}
		}									/* End if no mod section.		*/
	} while (!valid && (no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6'))); /* Repeat until data is valid.	*/

	if ((no_mod[0] == 'M') && !((term[0] == '1') && (term[1] == '6')))		/* Don't write unless they press enter	*/
	{										/* and they've changed something.	*/
											/* Call system services to change queue.*/
		qui_jbuf[0].item_code = SJC$_QUEUE;					/* Set up for queue.			*/
		qui_jbuf[0].buflen = strlen(curr_queue);
		qui_jbuf[0].bufptr = (long *)curr_queue;
		qui_jbuf[0].retlen = 0;

		if (fkey == fn9_key) qui_jbuf[1].item_code = SJC$_FORM_NUMBER;		/* Set up for new mounted form number.	*/
		else 		     qui_jbuf[1].item_code = SJC$_DEFAULT_FORM_NUMBER;	/* Set up for new default form number.	*/
		qui_jbuf[1].buflen = sizeof(long);
		qui_jbuf[1].bufptr = (long *)&form;
		qui_jbuf[1].retlen = 0;

		qui_jbuf[2].item_code = 0;						/* Mark the end of the buffer.		*/
		qui_jbuf[2].buflen = 0;
		qui_jbuf[2].bufptr = (long *)0;
		qui_jbuf[2].retlen = 0;

		stat_ss = sys$sndjbcw((long) 0, SJC$_ALTER_QUEUE, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_snd = iosb.status;
		if (stat_snd == JBC$_NORMAL && stat_ss == SS$_NORMAL)  state = TRUE;
		else
		{									 /* Report the situation.		*/
			if (fkey == fn9_key) return(report_error(stat_snd,0,curr_queue,"Changing queue mounted form"));
			else return(report_error(stat_snd,0,curr_queue,"Changing queue default form"));
		}
	}
	free(screen);									/* Release the screen memory.		*/
	return(state);
}

static int display_job(ndx)								/* Display job using DISPLAY Utility.	*/
long ndx;
{
	char *screen, wcc;								/* Ptr to screen to call vwang.		*/
	char vwfunc, lines, term[2], no_mod[2];						/* Working vars. for vwang.		*/
	char the_file[256], disp_name[255];						/* File name				*/
	char jobname[40], curr_job[40], *jcpt;
	register int i;
	unsigned long qflags;
	int state, c, return_code;
#include "quemgmt1.d"

	jcpt = qe_jbn + (ndx * 39);							/* Point to position in job name array.	*/
	i = 0;
	while (*jcpt != ' ' && i < 39) jobname[i++] = *jcpt++;				/* Copy job name from array to var.	*/
	jobname[i] = '\0';								/* Null terminate the string.		*/

	qflags = QUI$M_SEARCH_WILDCARD;
											/* Set up the buffers for system calls.	*/
	qui_qbuf[0].item_code = QUI$_SEARCH_NAME;					/* Init the buffer structure.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = QUI$_SEARCH_FLAGS;					/* Set search flags for this process.	*/
	qui_qbuf[1].buflen = sizeof(long);						/* in a 4 byte buffer			*/
	qui_qbuf[1].bufptr = (long *)&qflags;						/* which is flags.			*/
	qui_qbuf[1].retlen = 0;								/* return length address		*/

	qui_qbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[2].buflen = 0;
	qui_qbuf[2].bufptr = (long *)0;
	qui_qbuf[2].retlen = 0;

	qui_jbuf[0].item_code = QUI$_JOB_NAME;						/* Set up to return Job name.		*/
	qui_jbuf[0].buflen = 39;
	qui_jbuf[0].bufptr = (long *)curr_job;
	qui_jbuf[0].retlen = 0;

	qui_jbuf[1].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[1].buflen = 0;
	qui_jbuf[1].bufptr = (long *)0;
	qui_jbuf[1].retlen = 0;

	quibuf[0].item_code = QUI$_FILE_SPECIFICATION;					/* Set up to return file spec.		*/
	quibuf[0].buflen = 255;
	quibuf[0].bufptr = (long *)the_file;
	quibuf[0].retlen = 0;

	quibuf[1].item_code = 0;							/* Mark the end of the buffer.		*/
	quibuf[1].buflen = 0;
	quibuf[1].bufptr = (long *)0;
	quibuf[1].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, (long) 0, (long) 0, (long) 0);
											/* Check if any queues available.	*/
	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{										/* Check if any jobs available.		*/
		stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
		stat_qui = iosb.status;
		while ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* While there are jobs.		*/
		{
			if (strcmp(curr_job,jobname) == 0) break;			/* Job name equals current job.		*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_JOB, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
		}
		if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))		/* Job name was found.			*/
		{									/* Call to get the file identification.	*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_FILE, (long) 0, quibuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
			if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL));	/* File was found.			*/
			else return(report_error(stat_qui,0,the_file,"Display file, getting file")); /* Report situation.	*/
		}
		else
		{
			if (stat_qui == JBC$_NOSUCHENT)					/* Is an invalid entry.			*/
			{
				report_error(stat_qui,0,jobname,"Display file, getting job");
				return(TRUE);						/* Set so will rescan the queue.	*/
			}
			else return(report_error(stat_qui,0,jobname,"Display file, getting job")); /* Report situation.		*/
		}
	}
	else return(report_error(stat_qui,0,curr_queue,"Display file, getting queue"));	/* Report situation.			*/

	state = FALSE;									/* Assume did not delete.		*/
	jcpt = the_file;								/* Point to the file specification.	*/

	if ((i = greclen(the_file)) >= 0) vdisplay(the_file,i);				/* Get the record length.		*/
	else if (i == -2) werrlog(ERRORCODE(2),the_file,0,0,0,0,0,0,0);			/* Protection violation.		*/
	else werrlog(ERRORCODE(4),the_file,0,0,0,0,0,0,0);				/* Error on OPEN.			*/
}

static int find_new_qname(nqname,class,numq)						/* Get new qname info.			*/
char *nqname, *class;
long numq;
{
	int i, found, cq, scnt;
	char *cpt, pclass[15], *pcptr;
	char type[9], *ctyp;
	prt_id *prt_ptr;								/* Local ptrs to WISP$CONFIG data.	*/
	pq_id *pq_ptr;

	cq = 0;
	scnt = 0;
	found = FALSE;

	while (!found && cq < numq)							/* Loop through each queue until found.	*/
	{
		cpt = qm_qn + (cq * 31);						/* Point to pos in queue name array.	*/
		pcptr = pclass;
		while (*cpt != ' ')
		{
			*pcptr++ = *cpt++;
			scnt++;
		}
		*pcptr = '\0';								/* Null terminate the string.		*/
		cpt++;									/* Step past the seperating space char.	*/
		scnt++;
		if (strpos(pclass,class) >= 0)						/* If queue class matches new class.	*/
		{									/* Now test queue type.			*/
			ctyp = qm_qt + (cq * 8);					/* Point to pos in queue type array.	*/
			strncpy(type,ctyp,8);						/* Copy type into string.		*/
			type[8] = '\0';							/* Null terminate the string.		*/
			if ((strcmp(type,"Print   ") == 0) || (strcmp(type,"Terminal") == 0))
			{
				found = TRUE;						/* If queue type is Print or Terminal.	*/
			}
		}

		if (found)
		{
			for (i = 0; i < 31 && *cpt != ' '; i++) nqname[i] = *cpt++;	/* Copy name into variable.		*/
			nqname[i] = '\0';						/* Null terminate the string.		*/
		}
		else cq++;
	}
	if (found) return(1);
											/* Not found so check data files.	*/
	prt_ptr = get_prt_list();							/* Point to LPMAP data.			*/
	while (prt_ptr)									/* While have data from LPMAP.		*/
	{
		if (prt_ptr->class == *class)
		{
			strcpy(nqname,prt_ptr->qname);					/* Copy queue name to string.		*/
			return(1);							/* Return found (TRUE).			*/
		}
	}
	pq_ptr = get_pq_list();								/* Point to PQMAP data.			*/
	while (pq_ptr)									/* While have data from PQMAP.		*/
	{
		if (pq_ptr->class == *class)
		{
			strcpy(nqname,pq_ptr->qname);					/* Copy queue name into string.		*/ 
			return(1);							/* Return found (TRUE).			*/
		}
	}
	return(0);
}

static int start_stop_queue(ndx)							/* Toggle start/stop of a queue.	*/
int ndx;
{
	long que_stat;
	char *cpt, *dcpt;
	char dtext[12];
	int i;

	qui_jbuf[0].item_code = QUI$_SEARCH_NAME;					/* Set up to search queue name.		*/
	qui_jbuf[0].buflen = strlen(curr_queue);
	qui_jbuf[0].bufptr = (long *)curr_queue;
	qui_jbuf[0].retlen = 0;

	qui_jbuf[1].item_code = QUI$_QUEUE_STATUS;
	qui_jbuf[1].buflen = sizeof(long);
	qui_jbuf[1].bufptr = (long *)&que_stat;
	qui_jbuf[1].retlen = 0;

	qui_jbuf[2].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_jbuf[2].buflen = 0;
	qui_jbuf[2].bufptr = (long *)0;
	qui_jbuf[2].retlen = 0;

	qui_qbuf[0].item_code = SJC$_QUEUE;						/* Set up to input queue name.		*/
	qui_qbuf[0].buflen = strlen(curr_queue);
	qui_qbuf[0].bufptr = (long *)curr_queue;
	qui_qbuf[0].retlen = 0;

	qui_qbuf[1].item_code = 0;							/* Mark the end of the buffer.		*/
	qui_qbuf[1].buflen = 0;
	qui_qbuf[1].bufptr = (long *)0;
	qui_qbuf[1].retlen = 0;

	stat_ss = sys$getquiw((long) 0, QUI$_CANCEL_OPERATION, (long) 0,(long) 0, &iosb, (long) 0, (long) 0);

	stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
	stat_qui = iosb.status;
	if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
	{
		if (((que_stat & QUI$M_QUEUE_STOPPED) == QUI$M_QUEUE_STOPPED) ||
		    ((que_stat & QUI$M_QUEUE_STOPPING) == QUI$M_QUEUE_STOPPING))
		{									/* Can be started.			*/
			stat_ss = sys$sndjbcw((long) 0, SJC$_START_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
			stat_snd = iosb.status;
		}
		else 
		{									/* Can be stopped.			*/
			stat_ss = sys$sndjbcw((long) 0, SJC$_STOP_QUEUE, (long) 0, qui_qbuf, &iosb, (long) 0, (long) 0);
			stat_snd = iosb.status;
		}
		if ((stat_snd == JBC$_NORMAL || stat_snd == SS$_NORMAL) && stat_ss == SS$_NORMAL) /* Successful?		*/
		{									/* Get the new queue status.		*/
			stat_ss = sys$getquiw((long) 0, QUI$_DISPLAY_QUEUE, (long) 0, qui_jbuf, &iosb, (long) 0, (long) 0);
			stat_qui = iosb.status;
			if ((stat_qui == JBC$_NORMAL) && (stat_ss == SS$_NORMAL))
			{
				int scn_col;						/* Hold what column on screen is status.*/

				dtext[0] = '\0';					/* Start with nothing.			*/
				get_qs_text(que_stat,dtext);				/* Get appropriate queue status text.	*/
				cpt = qm_qs + (ndx * 11);				/* Point to pos in queue status array.	*/
				dcpt = cpt;						/* Assign position do display from.	*/
				for (i = 0; i < 11; i++)  *cpt++ = dtext[i];		/* Copy text into array.		*/
				function = DISP_ROW_ITEM;				/* Set to re-display an item in a row.	*/
				scn_col = 5;						/* Queue status column on screen.	*/
				vlist(&function,&mlist_id,&ndx,&scn_col,dcpt,&retcd);	/* Go display it.			*/
  				return(FALSE);						/* Set so will not re-display list.	*/
			}
			else
			{
				report_error(stat_qui,0,curr_queue,"Start/Stop queue, getting new status");
				return(TRUE);						/* Set so will re-display list.		*/
			}
		}
		else
		{
			report_error(stat_snd,0,curr_queue,"Start/Stop queue");		/* Report the situation.		*/
			return(TRUE);							/* Set so will re-display list.		*/
		}
	}
	else
	{
		report_error(stat_qui,0,curr_queue,"Start/Stop queue, getting current status");
		return(TRUE);								/* Set so will re-display list.		*/
	}
}

static int report_error(call_stat,entry,name,quemsg)					/* Display the error that occured.	*/
long call_stat, entry;
char *name, *quemsg;
{
	enum e_vop svo;
	long status, outbuf;
	short mlen;
	char msgstr[256];
#include "quemgmt2.d"

	if (stat_ss != SS$_NORMAL)							/* If was a failure on the system call	*/
	{										/*  then get the error message.		*/
		status = sys$getmsg(stat_ss,&mlen,&msgval,15,&outbuf);
	}
	else										/* else was a failure on the queue job	*/
	{										/*  system service so get error message.*/
		status = sys$getmsg(call_stat,&mlen,&msgval,15,&outbuf);
	}
	msgstr[mlen] = '\0';								/* Null terminate.			*/
	verase(FULL_SCREEN);
	svo = voptimize(VOP_OFF);							/* No optimization.			*/
	vmove(10,1);
	if (entry == 0)									/* Report queue situation.		*/
	{
		werrlog(ERRORCODE(6),name,quemsg,0,0,0,0,0);
	}
	else werrlog(ERRORCODE(4),entry,curr_queue,quemsg,0,0,0,0,0);			/* Report queue entry situation.	*/
	werrlog(ERRORCODE(2),msgstr,0,0,0,0,0,0,0);					/* Display system error message.	*/
	voptimize(svo);									/* Put it back to what it was.		*/
	if (status != SS$_NORMAL) lib$signal(status);

	return(FALSE);									/* Return to the caller.		*/
}

static que_mem_err(amt)									/* display no memory error.		*/
long amt;
{
	vmove(18,0);
	verase(TO_EOS);
	werrlog(ERRORCODE(8),amt,0,0,0,0,0,0,0);

/*	vexit();		*/							/* Unconditional exit.			*/
/*	wexit(ERRORCODE(8));	*/
}

static free_qmenu(l_id)									/* Free up memory assiciated with queues.*/
long l_id;
{
	register int i;
	int start, end;

	if (l_id != EMPTY_LIST)								/* If not an empty list.		*/
	{
		free(qm_qn);								/* Free up queue name array.		*/
		free(qm_quet);								/* Free up queue type string array.	*/
		free(qm_qt);								/* Free up queue type long array.	*/
		free(qm_bp);								/* Free up base priority array.		*/
		free(qm_jl);								/* Free up job limit array.		*/
		free(qm_cpul);								/* Free up cpu limit array.		*/
		free(qm_qs);								/* Free up queue status array.		*/
		free(qm_blmin);								/* Free up block size min array.	*/
		free(qm_blmax);								/* Free up block size max array.	*/
	}
	for (i = 0; i < num_mhead; i++)
	{
		free(menutstr[i]);							/* Free up header strings.		*/
	}
	start = num_mhead + 1;
	end = start + num_mfoot;
	for (i = start; i < end; i++)
	{
		free(menutstr[i]);							/* Free up footer strings.		*/
	}
	return;
}

static free_qentries(etype)								/* Free up memory assiciated with queue	*/
int etype;
{											/* entries.				*/
	register int i;
	int start, end;

	if (!etype)									/* If not an empty list.		*/
	{
		free(qe_jbn);								/* Free up job name array.		*/
		qe_jbn = '\0';
		free(qe_usrn);								/* Free up user name array.		*/
		qe_usrn = '\0';
		free(qe_st);								/* Free up job status long array.	*/
		qe_st = '\0';
		free(qe_ent);								/* Free up job entry array.		*/
		qe_ent = '\0';
		free(qe_jobs);								/* Free up job status string array.	*/
		qe_jobs = '\0';
		free(qe_blks);								/* Free up blocks array.		*/
		qe_blks = '\0';
		free(qe_jf);								/* Free up job file form long array.	*/
		qe_jf = '\0';
		free(qe_cpy);								/* Free up copies array.		*/
		qe_cpy = '\0';
		free(qe_frmn);								/* Free up job file form string array.	*/
		qe_frmn = '\0';
		free(del_entry);							/* Free up del entry table.		*/
		del_entry = '\0';
	}
	for (i = 0; i < num_qehead; i++)
	{
		free(tstr[i]);								/* Free up header strings.		*/
	}
	start = num_qehead + 1;
	end = start + num_qefoot;
	for (i = start; i < end; i++)
	{
		free(tstr[i]);								/* Free up footer strings.		*/
	}
	return;
}

static int work_message(on) int on;							/* Display the working message.		*/
{
	int lhs;									/* Left hand side of display.		*/
	register int i;									/* Working register.			*/

	vstate(-1);									/* Save wher we are.			*/

	window_top = 9;
	window_rows = 7;
	lhs = 32;									/* Assume at column 32.			*/

	if (on <= 0)
	{
		vmove(window_top+1,lhs-1);						/* Move to the clear area.		*/
		vmode(CLEAR);								/* Clear it out.			*/
		vprint("              ");						/* Output spaces.			*/
		i = 1;									/* Turn on next time.			*/
	}

	if (on >= 0)									/* On or off?				*/
	{
		vmove(window_top+1,lhs);						/* Move to the info area.		*/
		vmode(VMODE_REVERSE);							/* Select something noticeable.		*/
		vprint(" Working... ");							/* Display the working message.		*/
		i = -1;									/* Turn off the next time.		*/
	}

	vstate(1);									/* Restore where we were.		*/
	vcontrol_flush();								/* Show what is going on.		*/
	return(i);									/* Return the new state.		*/
}

static display_box(lmar,rmar)								/* Draw the outline box.		*/
int lmar, rmar;
{
	vmove(window_top-1,lmar-1);
	vline(VERTICAL,window_rows+2);
	vmove(window_top-1,lmar-1+rmar);
	vline(VERTICAL,window_rows+2);
	vmove(window_top-1,lmar-1);
	vline(HORIZONTAL,rmar+1);
	vmove(window_top+window_rows,lmar-1);
	vline(HORIZONTAL,rmar+1);
}

static save_window(lmar,rmar)								/* Save orig data under window.		*/
int lmar, rmar;
{
	register int i, j, k, mr, cc;

	for (i = 0; i < window_rows+2; i++) memset(window_data[i],' ',79);		/* Init lines to spaces.		*/

	mr = window_top-1;								/* Set starting row in map.		*/
	for (i = 0; i < window_rows+2; i++)						/* Save each line under window.		*/
	{
		k = vml(mr);								/* Compute the index into map.		*/
		cc = 0;
		for (j = lmar-1; j < rmar+3; j++)					/* Re-display each column in row.	*/
		{
			window_data[i][cc] = vchr_map[k][j];				/* Get the character in the map.	*/
			cc++;
		}
		window_data[i][cc] = '\0';						/* Null terminate the string.		*/
		mr++;									/* Increment map row.			*/
	}
}

static restore_window(lmar,rmar)							/* Display orig data under window.	*/
int lmar, rmar;
{
	register int i, j, cr;

	cr = window_top-1;								/* Set current row position (top box).	*/
	vmode(CLEAR);
	for (i = 0; i < window_rows+2; i++)						/* Re-display each line under window.	*/
	{
		vmove(cr,lmar-1);							/* Move to starting position.		*/
		vprint("%s",window_data[i]);
		cr++;
	}
}
#endif	/* VMS */

#if defined(unix) || defined(MSDOS)
#include "werrlog.h"

void quemngmnt()
{
#define		ROUTINE		49500

	werrlog(ERRORCODE(3),0,0,0,0,0,0,0,0);						/* Say we are here.			*/
	return;
}
#endif	/* unix || MSDOS */

/*
**	History:
**	$Log: quemgmt.c,v $
**	Revision 1.16  1997-10-03 13:38:02-04  gsl
**	YEAR2000 fix
**
**	Revision 1.15  1997-07-09 12:41:45-04  gsl
**	Change to use new video.h intefaces
**
**	Revision 1.14  1996-11-13 20:09:05-05  gsl
**	Changed to use vcontrol_flush()
**
**	Revision 1.13  1996-08-19 15:32:43-07  gsl
**	drcs update
**
**
**
*/
