#ifdef __cplusplus
extern "C" {
#endif

/* holdev.h

   Prototype header file for Hold Everything.
   For the C programming language.

        Copyright (C)1989, South Mountain Software, Inc.
        All Rights Reserved. */

/* prototype the main Hold EverythingSwitch! function */
int holdev( char *paths, int mem_needed, const char *command_line );

/* prototype the utility functions */
int  childret( void );
int  getchgint( void );
void setems( int flag );
void setxms( int flag );
void setenv( char *env[] );
void setint( int interrupt_num, int action_to_take );
void setovwr( int flag );
void setrestdir( int flag );
void settsr( int flag );
void setrestscr( int flag );

/* define the flags that are valid with setint (and those returned by */
/* getchgint() ).                                                     */
#define TAKEUNSAFE  1   /* take over the interrupt if it's in unsafe area */
#define CHECKSAFE   2   /* check if interrupt is unsafe - error if so */
#define TAKENOCHECK 4   /* take the interrupt over without checking */
#define ERRIRCHG    8   /* report an error if the interrupt changed in shell */
#define RESTRET     16  /* restore the interrupt after child is finished */
#define NEEDTSR     32  /* needs TSR to take over this interrupt */

/* define EQUates for the error return values */
#define CANT_FREE   3   /* can't free any memory */
#define FILE_GONE   2   /* user must have deleted temp file */
#define NOACCESS    5   /* ACCESS DENIED - ROOT FULL, OVERWRITE on - file read only */
#define NOCOMSPEC   6   /* Can't locate COMMAND.COM */
#define NOSPACE     10  /* Not enough space for the file on specified path(s) */
#define NODRIVE     11  /* None of the path(s) specified are on valid drives */
#define WRITEERROR  12  /* Error writing the file to disk */
#define UNSAFE_INT  13  /* An interrupt is unsafe **NOTE:  SEE DOCUMENTATION!** */
#define TOOMANYFH   14  /* Too many file handles - I can handle 255! */
#define BADPATH     15  /* None of the specified path(s) for the temp file are valid */
#define FILEEXISTS  16  /* The specified temp file exists, and overwrite flag is OFF */
#define INTCHANGED  17  /* Int was changed and option set to warn you **WARNING ONLY** */
#define TOOMANYMCB  18  /* Too many MCBs for me to handle - at default I do upto 80! */
#define NOMEMORY        19      /* Not enough memory to load COMMAND.COM */

#ifdef __cplusplus
}
#endif

