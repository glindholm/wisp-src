
/* Header for GETQUI system service call.											*/

#define QUI$_USERNAME		0x51						/* Job: Submitter's username			*/
#define QUI$_ENTRY_NUMBER	0x0F						/* Job: Entry number				*/
#define QUI$_JOB_STATUS		0x31						/* Job: Status information			*/
#define QUI$_JOB_SIZE		0x2E						/* Job: Total blocks in job			*/
#define QUI$_JOB_NAME		0x2C
#define QUI$_SEARCH_FLAGS	0x4C
#define QUI$V_SEARCH_THIS_JOB	0x04
#define QUI$_CANCEL_OPERATION   		0x01 				/*  Cancel a wildcard operation			*/
#define QUI$_DISPLAY_CHARACTERISTIC   		0x02				/* Return characteristic attributes		*/

#define QUI$_DISPLAY_FILE	0x03						/*Return file attributes			*/
#define QUI$_DISPLAY_FORM	0x04						/* Return form attributes			*/
#define QUI$_DISPLAY_JOB	0x05						/* Return job attributes			*/
#define QUI$_DISPLAY_QUEUE	0x06						/* Return queue attributes			*/
#define QUI$_QUEUE_NAME		0x46						/* Job, queue: Queue name			*/
#define QUI$_SEARCH_NAME	0x4D						/* Object name to search for			*/
#define QUI$_TRANSLATE_QUEUE	0x07						/* Validate and translate queue name		*/

#define QUI$M_SEARCH_ALL_JOBS	0x01
#define QUI$M_SEARCH_WILDCARD	0x02
#define QUI$M_SEARCH_BATCH	0x04
#define QUI$M_SEARCH_SYMBIONT	0x08
#define QUI$M_SEARCH_THIS_JOB	0x010

