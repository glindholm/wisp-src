/********************************************************************************************************************************/
/*    					 QMANAGER.H  Header file for the queue manager routines.				*/
/********************************************************************************************************************************/

struct qui_itmlst_struct {								/* $GETQUI itmlst parameter structure.	*/
				short unsigned int	buflen;				/* The length of the buffer.		*/
				short unsigned int	item_code;			/* The code for the request to GETDVI	*/
				long			*bufptr;			/* A pointer to the buffer.		*/
				long unsigned int	*retlen;			/* The return length of the buffer.	*/
			};

#define ANY_QUEUE	1
#define BATCH_QUEUE	2
#define PRINT_QUEUE	3

#define MENU_LIST	1
#define PQUEUE_LIST	2
#define BQUEUE_LIST	3
#define EMPTY_LIST	4

#define SEARCH_BATCH_ENABLED	0x00001000						/* Flag to allow access to batch queues.*/
#define SEARCH_GENERIC_ENABLED	0x00002000						/* Flag to allow access to generic queues.*/
#define SEARCH_OUTPUT_ENABLED	0x00004000						/* Flag to allow access to output queues.*/
#define RESTRICT_JOBS_DISABLED 	0x00008000						/* Disable restriction access to jobs 	*/
											/*  owned by user.			*/
