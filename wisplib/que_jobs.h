/* 		Define the constants used by any program needing access to the 'que_jobs' routines				*/

/* These are the flag values		*/

#define	Q_DELETE_FILE			0x01						/* Delete the file after job		*/
#define Q_HOLD_JOB			0x02						/* Put it on hold when submitted	*/
#define Q_PRINT_LOG			0x04						/* Print the log file			*/

#define BATCH_QUEUE			1
#define PRINT_QUEUE			2

/* Structures		*/


typedef union	{
			struct	{
					short	buflen;				/* The length of the sending/receiving buffer	*/
					short	itemcode;			/* The code of the item requests		*/
					char	*bufadr;			/* The address of the buffer			*/
					int	*retadr;			/* The address of an int to hold the return len	*/
				} item;

			long	endlist;					/* Indicate the end of the item list		*/
		} item_list;

