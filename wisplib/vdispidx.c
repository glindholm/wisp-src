/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
**
**
**
*/

/*
**	File:		vdispvsn.c 
**
**	Purpose:	To display index files
**
**	Routines:	 
**	vdispidx        main entry.  display file 
**	do_disp         main loop function
**	disp_action	To decide what action to perform during the disp loop
**      init_idxfile    initialize the idx struct 
**      show_screen     do the vwang of the current data 
**      setup_oa        setup the order area prior to vwang
**      setup_screen    setup the actual screen data prior to vwang
**      setup_fkeys     setup the "fkeys" part of the screen.. write the labels into the screen area
**      initialize      general init stuff
**      vdisp_file_type determine index file type if any
**      
**      open_vsn_file   file access routines for Vision files
**      get_vsn_info  
**      read_vsn_first
**      read_vsn_next 
**      read_vsn_keyed
**      
**      open_mf_file    file access routines for Microfocus files
**      get_mf_info  
**      read_mf_first
**      read_mf_next 
**      read_mf_keyed
**
**	History:
**	04/29/93        written. JEC
**
*/
#ifdef INCLUDE_VDISPIDX

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>            /* if this is not found (BSD types) then it it is in <sys/file.h>..  */
#include <string.h>
#ifdef WIN32
#include <io.h>
#endif
#ifdef unix
#include <unistd.h>
#endif

#include "idsistd.h"
#include "werrlog.h"
#include "wperson.h"
#include "scnfacs.h"
#include "vwang.h"
#include "ring.h"
#include "visn3.h"         /* Acucobol Vision internals header */
#include "wanguid.h"
#include "wisplib.h"
#include "screen.h"

#define VDISPIDX_C
#include "vdispidx.h"

/* function declarations */
       void   EXFUN(vdispidx,		   (char *, int));
       void   EXFUN(do_disp,		   (struct idxfile *));
       int    EXFUN(init_idxfile,	   (struct idxfile *, char *, int));
       int    EXFUN(show_screen,	   (int));

       void setup_oa();

       void   EXFUN(setup_screen,	   (struct idxfile *, int ));
       void   EXFUN(setup_screen_normal,   (struct idxfile *, int, int));
static void   EXFUN(initialize,	           (struct idxfile *));
       void   EXFUN(set_recs_per_page,     (struct idxfile *, int));
static void   EXFUN(shutdown,	           (struct idxfile *));
       void   EXFUN(setup_fkeys,	   ());
       int    EXFUN(vdisp_file_type,	   (char *));
       int    EXFUN(fill_rec_list,	   (struct idxfile *, int, int, char *, int *));
       void   EXFUN(search_text,	   (struct idxfile *, int, int, char *, int *));
       void   EXFUN(setup_search,          (char *, int));
       int    EXFUN(do_read,               (int, struct idxfile *, int, char *, int *));
       void   EXFUN(do_screen_setup,       (int, struct idxfile *, int *, struct vwang_screen **, char *, int));
       void   EXFUN(disp_action,           (int *, int, struct idxfile *, int *, int *, char *));
       void   EXFUN(trunc_srchbuf,         (char *));
       
       int4   EXFUN(open_vsn_file,	   (char *));
       void   EXFUN(get_vsn_info,	   (struct idxfile *));
       int    EXFUN(read_vsn_first,	   (struct idxfile *, char *));
       int    EXFUN(read_vsn_next,	   (struct idxfile *, char *));
       int    EXFUN(read_vsn_keyed,	   (struct idxfile *, char *, char *));
       
       int4   EXFUN(open_mf_file,	   (char *));
       void   EXFUN(get_mf_info,	   (struct idxfile *));
       int    EXFUN(read_mf_first,	   (struct idxfile *, char *));
       int    EXFUN(read_mf_next,	   (struct idxfile *, char *));
       int    EXFUN(read_mf_keyed,	   (struct idxfile *, char *, char *));

/* extern function stuff */


static int stx(unsigned char *string, unsigned char *mask);	/* Get string index in another string.	*/

/* ACUCOBOL functions */

extern int4   EXFUN(i_open,		   (char *, int, char *));
extern void   EXFUN(i_info,		   (int4, int, char *));
extern int    EXFUN(i_next,		   (int4, char *));
extern int    EXFUN(i_read,		   (int4, char *, int));
extern int    EXFUN(i_start,		   (int4, char *, int, int, int));
extern int    EXFUN(i_close,               (int4));		

/*
**	Routine:	vdispidx()
**
**	Function:	To display an index file
**
**	Input:		file name and type
**			
**
**	Output:		display the file on the ws
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93      written by JEC
**
*/
void vdispidx(filename,type)
char *filename;
int type;
{
	struct idxfile file;

	/* if return is FALSE it means file open failed */
	if (init_idxfile(&file,filename,type)==FALSE)
	  return;
       
	initialize(&file);  /* do general init */
	do_disp(&file);     /* perform main display loop */
	shutdown(&file);    /* cleanup */
}
/*
**	Routine:	init_idxfile()
**
**	Function:	To initialize the idx_file struct
**
**	Description:	Based on the index file type, setup the function pointers for
**                      accessing the file.
**
**	Input:		the (uninitialized) struct, and the file type
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	the idx_file struct is modified
**
**	History:	04/29/93         Written by JEC
**
*/
int init_idxfile(file,filename,type)
struct idxfile *file;
int type;
char *filename;
{
	memset(file,0,sizeof(struct idxfile));
	switch (type)
	{
	      /* now setup the function pointers for file access */
	      case VD_TYPE_VISION:
	      {
		      file->open = open_vsn_file;
		      file->info = get_vsn_info;
		      file->next = read_vsn_next;
		      file->first= read_vsn_first;
		      file->keyed= read_vsn_keyed;
		      break;
	      }
	      case VD_TYPE_MF:
	      {
		      file->open = open_mf_file;
		      file->info = get_mf_info;
		      file->next = read_mf_next;
		      file->first= read_mf_first;
		      file->keyed= read_mf_keyed;
	      }
	      default:
	      {
		      ;
	      }
        }
	file->fd = file->open(filename);  /* request file open */
	if (file->fd==0)		  /* open failed */
	  return FALSE;
	file->info(file);		  /* get file&key info */
	return TRUE;			  /* success */
}
/*
**	Routine:	do_disp()
**
**	Function:	The main loop for this module
**
**	Input:		idx_file struct. 
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/

void do_disp(file)
struct idxfile *file;
{
	int status=VD_READ_FIRST|KEY_INP_UPPER;          /* status word used to keep track of what we're doing */
							 /* it contains some flags and a "state" value.  The flags and */
							 /* state value are used to tell the various parts what to do */

	int scroll_count = 0;				 /* for the read routine.  tells how many recs to scroll */

	int read_status;				 /* status returned from the read */
	int fkey;					 /* fkey pressed by the user */
	static char *keybuf=NULL;			 /* buffer for key input */
	struct vwang_screen *scrptr;			 /* a screen pointer that we use for our vwang call */
	int findpos = -1;
	
	char vwang_fn_write=WRITE_ALL_PB;		 /* some vwang variables */
	char vwang_fn_read=READ_ALL_PB;
	char vwang_lines;
	char term[2],no_mod[2];

	rec_disp_start = 0;				 /* rec_disp_start tells what offset to start displaying the current */
							 /* record.  it's used when user goes (7)up or (6)down */
	if (keybuf==NULL)
	{
		keybuf=calloc(file->reclen,1);		 /* grab some space */
	}
	
	/* now loop until the status is "exit".  The loop consists of three things: a file read, a vwang, and an action */
	while (STATE(status) != VD_EXIT_DO_DISP)
	{
		read_status = do_read(status,file,scroll_count,keybuf,&findpos);	/* read the file if appropriate */
		if (STATE(status) == VD_READ_KEYED)	 /* if it was a keyed read */
		{
			if (read_status == ST_NOT_FOUND) /* was it successful? */
			{
				SETFLAG(status,KEY_INVALID); /* nope, so set key_invalid flag and */
				SETSTATE(status,VD_GET_KEY); /* stay in get_key moden */
			}
			else
			{
				CLRFLAG(status,KEY_INVALID); /* read was ok so clear this flag */
			}
		}
		if (STATE(status) == VD_SEARCH)
		{
			if (read_status == ST_NOT_FOUND) /* was it successful? */
			{
				SETFLAG(status,TEXT_NOT_FOUND); /* nope, so set text not found flag and */
			}
			else
			{
				CLRFLAG(status,TEXT_NOT_FOUND); 
			}
		}
		do_screen_setup(status,file,&rec_disp_start,&scrptr,&vwang_lines,findpos); /* now prepare a screen for display */
		CLRFLAG(status,TEXT_NOT_FOUND); 
		
		/* write and read the screen */
		vwang(&vwang_fn_write,(char *)scrptr,&vwang_lines,vwang_fkeys,term,no_mod);
		vwang(&vwang_fn_read,(char *)scrptr,&vwang_lines,vwang_fkeys,term,no_mod);
	        fkey = (term[0]-'0')*10 + (term[1]-'0');

		/* and then decide what to do next */
		disp_action(&status,fkey,file,&rec_disp_start,&scroll_count,keybuf);
	}
	free(keybuf);
	keybuf=NULL;
}
/*
**	Routine:	disp_action()
**
**	Function:	To decide what action to perform 
**
**	Description:	called from main disp loop (do_disp) to decide what to do
**                      based on which function key the user pressed, etc
**
**	Input:		status - the status information for what's happening 
**                      fkey   - the function key the user pressed 
**			file   - the struct containing everything relevant about the index file
**                      rec_disp_start - pointer to this value in case we want to adjust it
**                      scroll_count - pointer to this value so we can set it for the next read
**                      keybuf - pointer to the key buffer so we can load the user's key into it
**
**	Output:		adjust the state, do whatever is needed 
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/30/93   written by JEC
**
*/

void disp_action(status,fkey,file,rec_disp_start,scroll_count,keybuf)
int *status;              /* status of what's happening */
int fkey;		  /* fkey pressed by user */
struct idxfile *file;	  /* info about the file */
int *rec_disp_start;	  /* the starting position on screen of the current record */
int *scroll_count;	  /* number of recs to scroll for the next read */
char *keybuf;		  /* a pointer to the key buffer  in case  the user entered one */
{
	if (STATE(*status) == VD_GET_KEY)      /* this means we were just in the "get key" screen */
	{
		switch (fkey)
		{
		        case 0:		       /* did the user press enter? */
		        {
				SETSTATE(*status,VD_READ_KEYED);      /* yep, so change state to read using his key */
				*rec_disp_start = 0;		      /* reset the rec disp offset and copy the key */
				memcpy(keybuf,&get_key_scr.screen[80*2+9],file->keylen>64?64:file->keylen);
				break;
			}
		        case 1:                /* PF1 to abort enter key screen */
		        {
				SETSTATE(*status,VD_DO_NOTHING);      /* leave the old screen up */
				CLRFLAG(*status,KEY_INVALID);         /* clear this flag just in case */
				break;
		        }
		        case 2:                /* user wants to toggle upper/uplow */
		        {
				SETSTATE(*status,VD_GET_KEY);         /* leave us in get key mode */
				if (CHKFLAG(*status,KEY_INP_UPPER))   /* and toggle this flag */
				{
					CLRFLAG(*status,KEY_INP_UPPER);
				}
				else
				{
					SETFLAG(*status,KEY_INP_UPPER);
				}
				break;
			}
		}
	}
	else if (STATE(*status) == VD_GET_TEXT)
	{
		switch (fkey)
		{
		        case 0:		       /* did the user press enter? */
		        {
				char msg[84],write_all=WRITE_ALL,lines=1,dummy1[2],dummy2[2];
				
				SETSTATE(*status,VD_SEARCH);          /* yep, so change state to search for his text */
				*rec_disp_start = 0;		      /* reset the rec disp offset and copy the key */
				memcpy(keybuf,&get_key_scr.screen[80*3+9],64);
				memcpy(vd_text_buf,&get_key_scr.screen[80*3+9],64);
				
				setup_oa((struct vwang_screen *)msg,(char)3,(char)0,(char)0,(char)0);
				memset(&msg[4],' ',80);
				memcpy(&msg[4]," Searching.  Press any key to terminate search.",47);
				msg[4]=BLINK_TEXT;
				msg[15]=PLAIN_TEXT;
				memset(dummy1,0,2);
				memset(dummy2,0,2);
				vwang(&write_all,msg,&lines,"X",dummy1,dummy2);
				break;
			}
		        case 1:                /* PF1 to abort enter key screen */
		        {
				SETSTATE(*status,VD_DO_NOTHING);      /* leave the old screen up */
				break;
		        }
		        case 2:                /* user wants to toggle upper/uplow */
		        {
				SETSTATE(*status,VD_GET_TEXT);         /* leave us in get key mode */
				if (CHKFLAG(*status,SEARCH_ANY))   /* and toggle this flag */
				{
					CLRFLAG(*status,SEARCH_ANY);
				}
				else
				{
					SETFLAG(*status,SEARCH_ANY);
				}
				memcpy(vd_text_buf,&get_key_scr.screen[80*3+9],64);
				break;
			}
			
		}
	}
	else      /* the else case is the normal screen */
	{       
		switch (fkey)
		{
		      	case 16:               /* set state to cause exit */
			{
				SETSTATE(*status,VD_EXIT_DO_DISP);
				break;
			}
			case 2:		       /* set state to read first */
			{
				SETSTATE(*status,VD_READ_FIRST);
				break;
			}
			case 5:		       /* set state to read next */
			{
				SETSTATE(*status,VD_READ_NEXT);

				/* now compute the number of records we need to scroll up */
				if (file->recs_per_page > 1)
				{	
					*scroll_count = file->recs_per_page - 1;
				}
				else
				{
					*scroll_count = 1;
				}
				*rec_disp_start = 0;           /* reset the record display offset counter */
				break;
			}
			case 6:                /* roll the data down (only used when the record is too big for the screen */
			{
				SETSTATE(*status,VD_DO_NOTHING);           /* no need to read the file this time */
				if (CHKFLAG(*status,DISP_MODE_HEX)==FALSE)
				{ 
					*rec_disp_start -= 64;		   /* for ascii mode, scroll the data 64 chars */
				}
				else
				{
					*rec_disp_start -= 32;		   /* hex mode only has 32 chars per line */
				}
				if (*rec_disp_start < 0)
				{
					*rec_disp_start = 0;		   /* if we went negative (shouldn't happen), fixit */
				}
				break;
			}
			case 7:                /* roll the data up (if rec too big), or scroll the records up 1 record */
			{
				if (file->recs_per_page > 1)               /* more than 1 record per page? */
				{
					SETSTATE(*status,VD_READ_NEXT);    /* yes, so just do a read next */
					*scroll_count = 1;		   /* and tell it to just scroll 1 record */
				}
				else
				{
					SETSTATE(*status,VD_DO_NOTHING);   /* nope, so don't read anything else */
					if (CHKFLAG(*status,DISP_MODE_HEX)==FALSE)
					{
						*rec_disp_start += 64;	   /* and adjust the data as for PF6 */
					}
					else
					{
						*rec_disp_start += 32;
					}
				}
				break;
			}
			case 8:                /* do the get key screen on the next loop */
			{
				SETSTATE(*status,VD_GET_KEY);
				break;
			}		       /* toggle the hex/ascii mode */
		        case 9:
		        {
				SETSTATE(*status,VD_GET_TEXT);
				break;
		        }
		        case 10:
		        {
				if (CHKFLAG(*status,DISP_MODE_HEX))
				{
					CLRFLAG(*status,DISP_MODE_HEX);
					
					set_recs_per_page(file,64);        /* here we recompute the recs_per_page member. */
					if (*rec_disp_start % 64)          /* and we need to adjust this value so that */
					{				   /* it is on a 64 char boundary  */
						*rec_disp_start += 32;
					}
				}
				else
				{
					SETFLAG(*status,DISP_MODE_HEX);

					set_recs_per_page(file,32);
				}

				/* now, we fake a reread of the current screen since the recs per screen has changed. */
				/* make it appear that the user has requested a keyed read on the first rec on the screen. */
				/* this way we let the fill_recs code rebuild the list for us */
				ring_front(reclist,recbuf);
				memcpy(keybuf,recbuf+file->keyoffs,file->keylen);
				SETSTATE(*status,VD_READ_KEYED);
				break;
		        }
		        case 15:
		        {
				char filelibvol[23];
				char the_screen[1924];
				char vwang_fn_read = READ_ALL;
				char vwang_lines=24;
				char term[2],no_mod[2];
				
				term[0]=no_mod[0]='\0';
				memset(filelibvol,' ',22);
				memset(filelibvol,'#',2);
				memcpy(&filelibvol[2],WL_wanguid3(),3);
				memset(&filelibvol[5],' ',22-5); /* fix bug */

				/* for screen print, we vwang read the screen, and hand it to di_write_file */
				setup_oa((struct vwang_screen *)the_screen,(char)0,(char)0,(char)0,(char)0);
				vwang(&vwang_fn_read,(char *)the_screen,&vwang_lines,vwang_fkeys,term,no_mod);
				WL_di_write_file(the_screen+4,1920,80,filelibvol,filelibvol);  
				SETSTATE(*status,VD_DO_NOTHING);    /* no need to read this time */
			}
		}
	}
}
/*
**	Routine:	do_read()
**
**	Function:	To perform the read part of the loop
**
**	Description:	To decide whether or not a read is needed and then do it
**
**	Input:		status - our current mode of behavior
**			file   - info that all functions like to have about the index file
**                      keybuf - buffer containing a key (if it's a keyed read)
**
**	Output:		The reclist is filled with enough records to fill the screen
**			
**
**	Return:		status of the read is returned
**
**	Warnings:	None
**
**	History:	04/30/93    written by JEC
**
*/
int do_read(status,file,scroll_count,keybuf,findpos)
int status, scroll_count;
struct idxfile *file;
char *keybuf;
int *findpos;
{	
	int read_status;

	if (STATE(status) == VD_READ_FIRST || STATE(status) == VD_READ_NEXT || STATE(status) == VD_READ_KEYED ||
	    STATE(status) == VD_SEARCH)
	{
		read_status = fill_rec_list(file,status,scroll_count,keybuf,findpos);
		return read_status;
	}
	else
	{
		return ST_OK;
	}
}
/*
**	Routine:	fill_rec_list()
**
**	Function:	To read some records
**
**	Input:		status - our current mode of behavior
**			file   - info that all functions like to have about the index file
**                      keybuf - buffer containing a key (if it's a keyed read)
**                      scroll_count - number of records to scroll (and read)
**
**	Output:		The reclist is filled with enough records to fill the screen
**			
**
**	Return:		status of the read is returned
**
**	Warnings:	None
**
**	History:	04/30/93    written by JEC
**
*/
int fill_rec_list(file,status,scroll_count,keybuf,foundpos)
struct idxfile *file;
int status;
int scroll_count;
char *keybuf;
int *foundpos;
{
	extern char *reclist;
	int idx;
	extern char *recbuf;
	int read_status, recs_read, got_eof=FALSE;
	extern short f_errno;
	
	at_first = FALSE;
	*foundpos = -1;
	if (STATE(status) == VD_READ_NEXT)        /* read more records */
	{
		/* setup to loop and read */
		for (idx=scroll_count, got_eof=recs_read=0; idx; --idx)
		{		
			read_status = file->next(file,recbuf);
			if (read_status==0 && f_errno==E_NOT_FOUND)
			{
				++got_eof;            /* set this flag and exit the loop */
				break;
			}
			ring_que(reclist,recbuf);     /* add the record to the queue of records */
			++recs_read;		      /* and keep count */
		}
		/* now loop and "scroll off" records from the top of the list */
		for (idx= scroll_count>recs_read? recs_read:scroll_count ; idx; --idx)
		{
			ring_unque(reclist,NULL);
		}
	}     
	else if (STATE(status)==VD_READ_FIRST || STATE(status)==VD_READ_KEYED)    /* move somewhere in the file */
	{
		if (STATE(status)==VD_READ_FIRST)       /* jump to the first record */
		{
			read_status = file->first(file,recbuf);    /* call the first record routine */
			at_first=TRUE;				   /* and set this flag */
		}
		else if (STATE(status)==VD_READ_KEYED)  /* jump to the indicated record */
		{
			read_status = file->keyed(file,recbuf,keybuf);
			if (read_status==0 && f_errno==E_NOT_FOUND)    /* if record not found */
			{
				ring_back(reclist,recbuf);             /* get the last record on the screen */
				memcpy(keybuf,recbuf+file->keyoffs,file->keylen);   /* copy its key */
				file->keyed(file,recbuf,keybuf);       /* and reread that record. this resets the  */
								       /* file pointer (cursor) to the appropriate place, */
								       /* in case the user now tries to do a read next */
				return ST_NOT_FOUND;                   /* indicate the record wasn't found */
			}
		}
		ring_count(reclist,&idx);               /* count the records in the list now */
		while (idx)				/* now remove all the records */
		{
			ring_unque(reclist,NULL);
			--idx;
		}
		ring_que(reclist,recbuf);		/* and add the one we read */

		/* now loop and fill the rest of the screen */
		for (idx=0; idx<file->recs_per_page-1; ++idx)
		{
			read_status=file->next(file,recbuf);
			if (read_status==0 && f_errno==E_NOT_FOUND)  /* allow for reaching end of file before we */
			{					     /* reach the end of screen */
				++got_eof;
				break;
			}
			ring_que(reclist,recbuf);
		}
	}
	else if (STATE(status)==VD_SEARCH)
	{
		search_text(file,status,scroll_count,keybuf,foundpos);
		if (*foundpos == -1)
		{
			ring_back(reclist,recbuf);             /* get the last record on the screen */
			memcpy(keybuf,recbuf+file->keyoffs,file->keylen);   /* copy its key */
			file->keyed(file,recbuf,keybuf);       /* and reread that record. this resets the  */
			                                       /* file pointer (cursor) to the appropriate place, */
							       /* in case the user now tries to do a read next */
			return ST_NOT_FOUND;		       /* indicate the record wasn't found */
		}
		else
		{
			memcpy(keybuf,recbuf+file->keyoffs,file->keylen);   
			read_status = file->keyed(file,recbuf,keybuf);
			ring_count(reclist,&idx);               /* count the records in the list now */
			while (idx)				/* now remove all the records */
			{
				ring_unque(reclist,NULL);
				--idx;
			}
			ring_que(reclist,recbuf);		/* and add the one we read */
			
			/* now loop and fill the rest of the screen */
			for (idx=0; idx<file->recs_per_page-1; ++idx)
			{
				read_status=file->next(file,recbuf);
				if (read_status==0 && f_errno==E_NOT_FOUND)  /* allow for reaching end of file before we */
				{					     /* reach the end of screen */
					++got_eof;
					break;
				}
				ring_que(reclist,recbuf);
			}
		}
	}
	if (got_eof)    /* now set the status and the eof flag */
	{
		read_status = ST_EOF;
		at_eof_flag=TRUE;
	}
	else
	{
		read_status = ST_OK;
		at_eof_flag=FALSE;
	}
	return read_status;
}
/*
**	Routine:	do_screen_setup()
**
**	Function:       setup the screen for vwang
**
**	Input:		status - our current mode of behavior
**			file   - info that all functions like to have about the index file
**                      rec_disp_start - starting point for data in the current record to be displayed
**                      lines  - passed back.  the number of lines we actually want to write
**                      scrptr - passed back.  pointer to the screen we made
**
**	Output:		a screen is generated for use by vwang
**			
**
**	Return:		none
**
**	Warnings:	None
**
**	History:	04/30/93    written by JEC
**
*/
void do_screen_setup(status,file,rec_disp_start,scrptr,lines,foundpos)
int status, *rec_disp_start;
struct idxfile *file;
struct vwang_screen **scrptr;
char *lines;
int foundpos;
{
	int bytesper,hexmode,bytes_last_screen;
	
	hexmode = CHKFLAG(status,DISP_MODE_HEX)!=0;   /* setup an easy to use flag */
	bytesper = hexmode?32:64;		      /* compute the chars per line */
	bytes_last_screen = hexmode ?512:1024;	      /* compute the number of chars per screen */
	
	if (STATE(status) != VD_GET_KEY && STATE(status) != VD_GET_TEXT)	      /* this is a "normal" screen */
	{
		*scrptr = &main_scr;		      /* setup the pointer */
		if (CHKFLAG(status,DISP_MODE_HEX)==FALSE && STATE(status)==VD_SEARCH && foundpos != -1)
		{
			int crow,ccol;
			
			crow = 9  + foundpos / 64;
			ccol = 11 + foundpos % 64;
			
			if (crow>24)
			{
				*rec_disp_start = (crow - 24)*64;
				crow = 24;
			}
			setup_oa((struct vwang_screen *)*scrptr,(char)1,(char)UNLOCK_KEYBOARD|POSITION_CURSOR,(char)crow,(char)ccol);  /* and setup the order area */	
		}
		else
		{
			setup_oa((struct vwang_screen *)*scrptr,(char)1,(char)UNLOCK_KEYBOARD|POSITION_CURSOR,(char)0,(char)0);  /* and setup the order area */
		}
		setup_screen_normal(file,*rec_disp_start,status); /* call another routine to write the record data */
		*lines=24;			      /* and the lines */
		vwang_fkeys[0]='\0';		      /* now lets generate the fkey list */

		if (!at_first)			      /* if already at first don't allow a PF2 */
		{
			strcat(vwang_fkeys,"02");
		}
		if (!at_eof_flag)		      /* at end so don't allow a PF5 */
		{
			strcat(vwang_fkeys,"05");
		}
		if (*rec_disp_start)		      /* if rolled up into the record data, allow a PF6 to roll down */
		{
			strcat(vwang_fkeys,"06");
		}
		if (file->recs_per_page == 1)	      /* the decision to allow PF7 is more complex. If only 1 rec per screen, */
		{
			/* when the end of the record has be rolled up onto the screen, PF7 is no longer allowed */
			if (*rec_disp_start< ((file->reclen - bytes_last_screen) & ~(bytesper-1))+bytesper)
		        {
				strcat(vwang_fkeys,"07");
			}
		}
		else if (file->recs_per_page > 1)     /* if there are multiple recs, we can PF7 up until end of file */
		{
			if (!at_eof_flag)
		        {
				strcat(vwang_fkeys,"07");
			}
		}
		strcat(vwang_fkeys,"0809101516X");      /* now add find rec, hex/ascii, print screen, and exit */
	}
	else if (STATE(status) == VD_GET_KEY)
	{
		int fld_size;
		
		/* write first line */
		INSERTSCN(get_key_scr,0,0," Enter  key   value  and press ENTER.  Use PF1 to exit find mode.          ");
		fld_size = file->keylen > 64 ? 64 : file->keylen;   /* compute size of key input field */
		if (CHKFLAG(status,KEY_INVALID)==FALSE)
		{
			/* if we're not telling the user he entered a bad key */
			get_key_scr.screen[80*1+0] = PLAIN_TEXT;    /* this might be blink, so force to plain */
			INSERTSCN(get_key_scr,1,1,"Use PF2 to ");
			INSERTSCN(get_key_scr,1,12,                 /* this part can be toggled */
				  CHKFLAG(status,KEY_INP_UPPER) ? "allow lower case input.                             ":
				                                  "restrict input to upper case.                       ");
		}
		else
		{       /* write the invalid message and insert the blink fac */
			INSERTSCN(get_key_scr,1,1,"SORRY - key value is invalid or beyond end of file");
			get_key_scr.screen[80*1+0] = BLINK_TEXT;
			get_key_scr.screen[80*1+6] = PLAIN_TEXT;
		}
		memset(&get_key_scr.screen[80*2+9],' ',fld_size);   /* write spaces into the key field */
		memset(&get_key_scr.screen[80*3],' ',80);	    /* erase the text input line */
		
		INSERTSCN(get_key_scr,2,2,"KEY  =");

		/* and check the upper flag to decide the starting key input FAC */
		if (CHKFLAG(status,KEY_INP_UPPER))
		{
			get_key_scr.screen[80*2+8] = UPCASE_FIELD;
		}
		else
		{
			get_key_scr.screen[80*2+8] = STANDARD_FIELD;
		}
		get_key_scr.screen[80*2+fld_size+9] = PLAIN_TEXT;
		*scrptr = &get_key_scr;     /* set the scrprt to use this screen */
		*lines = 3;		    /* only want to write the first three lines */
		setup_oa((struct vwang_screen *)*scrptr,(char)1,(char)POSITION_CURSOR|UNLOCK_KEYBOARD,(char)0,(char)0);
		strcpy(vwang_fkeys,"000102X");  /* only allow enter, PF1 and PF2 */
	}
	else if (STATE(status) == VD_GET_TEXT)
	{
		/* write first line */   
		INSERTSCN(get_key_scr,0,0," Enter text to search for and press ENTER.  Use PF1 to exit find text mode.");

		get_key_scr.screen[80*1+0] = PLAIN_TEXT;    /* this might be blink, so force to plain */
		INSERTSCN(get_key_scr,1,1,"Use PF2 for ");
		INSERTSCN(get_key_scr,1,13,                 /* this part can be toggled */
			  CHKFLAG(status,SEARCH_ANY) ? "case sensitive search.                        ":
				                       "case insensitive search.                      ");

		memset(&get_key_scr.screen[80*2],' ',80);     /* erase the key input line */
		INSERTSCN(get_key_scr,3,1,"INPUT =");
		memcpy(&get_key_scr.screen[80*3+9],vd_text_buf,64);   /* write spaces into the key field */
		get_key_scr.screen[80*3+8] = STANDARD_FIELD;
		get_key_scr.screen[80*3+73] = PLAIN_TEXT;
		*scrptr = &get_key_scr;     /* set the scrprt to use this screen */
		*lines = 4;		    /* only want to write the first three lines */
		setup_oa((struct vwang_screen *)*scrptr,(char)1,(char)POSITION_CURSOR|UNLOCK_KEYBOARD,(char)0,(char)0);
		strcpy(vwang_fkeys,"000102X");  /* only allow enter, PF1 and PF2 */
	}
}	       
/*
**	Routine:	setup_screen()
**
**	Function:	To prepare the screen struct for display.
**
**	Input:		mode=normal,error,and getkey
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
void setup_screen_normal(file,rec_disp_start,status)
struct idxfile *file;
int rec_disp_start,status;
{
	int recidx, datidx, lines_per_rec, curline, linidx, colidx, counter;
	char offsetstr[10];
	int bytes_per_line;
	int hexmode=FALSE;
	int recs_this_page;

	/* write the fkey labels onto the screen */
	setup_fkeys(file,status,rec_disp_start);
	memset(&main_scr.screen[2*80],' ',80);   /* clear out lines 2 and 3 */
	memset(&main_scr.screen[3*80],' ',80);

	/* now decide what "ruler" to add to the top, and set the chars per line value */
	if (CHKFLAG(status,DISP_MODE_HEX)==FALSE)
	{
		INSERTSCN(main_scr,4,10,"          1         2         3         4         5         6    ");
		INSERTSCN(main_scr,5,10,"01234567890123456789012345678901234567890123456789012345678901234");
		bytes_per_line=64;
	}
	else
	{
		memset(&main_scr.screen[4*80],' ',80);
		INSERTSCN(main_scr,5,07,"00 . . . 04 . . . 08 . . . 0C . . . 10 . . . 14 . . . 18 . . . 1C . . .");
		bytes_per_line=32;
		hexmode=TRUE;
	}

	/* number of lines per rec is a useful value */
	lines_per_rec = file->reclen/bytes_per_line + (file->reclen%bytes_per_line?1:0) +1;
	ring_count(reclist,&recs_this_page);         /* count the recs in the current list */
	if (recs_this_page > file->recs_per_page)    /* make sure there's not too many to display */
	{
		recs_this_page = file->recs_per_page;
	}
	/* now we loop thru the list, and write the data into the screen */
	for (recidx=0; recidx<recs_this_page; ++recidx)
	{
		curline = 7+recidx*lines_per_rec;    /* compute the line for output.  data begins on line 7 */

		if (hexmode)  /* layout is slightly different for Hex and ASCII */
		{
			INSERTSCN(main_scr,curline,1,"Key =");
		}
		else
		{
			INSERTSCN(main_scr,curline,4,"Key =");
		}
		ring_get(reclist,recidx,recbuf);     /* get the needed record */

		/* now write the key onto the screen */
		if (hexmode)
		{
			char hexnum[3];
			for (datidx=0; datidx<32 && datidx<file->keylen; ++datidx)
			{
			sprintf(hexnum,"%02X",recbuf[file->keyoffs+datidx]);
				memcpy(&main_scr.screen[curline*80+7+datidx*2],hexnum,2);
			}
		}
		else
		{
			memcpy(&main_scr.screen[curline*80+10],&recbuf[file->keyoffs],file->keylen);
		}

		/* now loop and display the actual data */
		
		/*  start at rec_disp_start.                        go to end or 16 lines   */
		for (counter=datidx=rec_disp_start, linidx=colidx=0; datidx < file->reclen && linidx<16; ++datidx, ++colidx)
		{
			if (colidx >= bytes_per_line) /* when at the line end */
			{
				++linidx;	      /* bump the line counter */
				if (linidx >= 16)
				{
					continue;     /* this is the end of screen */
				}
				colidx=0;	      /* go back to beginning of next line */
				counter+=bytes_per_line; /* and increment the offset counter */
			}
			if (colidx == 0)	      /* at beginning of line write the offset value */
			{
				if (hexmode)
				{
					sprintf(offsetstr,"%04X",counter);
				}
				else
				{
					sprintf(offsetstr,"%-6d ",counter);
				}
				INSERTSCN(main_scr,curline+linidx+1,1,offsetstr);
			}
			if (hexmode)  /* now write the data, in hex or ascii */
			{
				char hexnum[3];
				int offs;
				
				sprintf(hexnum,"%02X",recbuf[datidx]);

				/* 80 times the line number, plus colidx *2 (hex is 2 digits), plus 7 (data starts at col 7), */
				/* plus colidx/4 (every four chars we must skip a space) */
				offs = (80*(curline+linidx+1))+(colidx*2)+7+(colidx/4);
				memcpy(&main_scr.screen[offs],hexnum,2);
			}
			else
			{
				int offs;
				 
				offs = (80*(curline+linidx+1))+colidx+10; /* offset is easy to compute */
				/* write a '.' if the current character is unprintable, otherwise write the data */
				main_scr.screen[offs] = 
				  recbuf[datidx]>=' ' && recbuf[datidx]<='~' ? recbuf[datidx] : '.';
			}
		}
	}
	/* and space out the rest of the screen */
	for (curline = 8+recs_this_page*lines_per_rec; curline<24; ++curline)
	{
		memset(&main_scr.screen[curline*80],' ',80);
	}
}
/*
**	Routine:	open_vsn_file()
**
**	Function:	To open a vision file.
**
**	Input:		Filename.
**			
**
**	Output:		The identifier used by vision routines to refer to the open file
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
int4 open_vsn_file(filename)
char *filename;
{
	int4 tmp;

	tmp = i_open(filename,Finput,"0,0,0");  
	return tmp;
}
/*
**	Routine:	get_vsn_info()
**
**	Function:	To get key, recsz, info about the file.  This is for Vision only.
**
**	Input:		the idx_file that refers to the idx file
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
void get_vsn_info(file)
struct idxfile *file;
{
	char idx_lparms[100];
	char idx_reccnt[16];
	char idx_keyinfo[100];
	int dummy;

	/* see ACUCOBOL provided internals document for details on these calls */
	i_info(file->fd,-1,idx_lparms);
	sscanf(idx_lparms,"%d,%d,%d",&file->reclen,&dummy,&dummy);

	i_info(file->fd,-4,idx_reccnt);
	sscanf(idx_reccnt,"%d",&file->reccnt);

	i_info(file->fd,0,idx_keyinfo);
	sscanf(idx_keyinfo,"%d,%d,%d,%d",&dummy,&dummy,&file->keylen,&file->keyoffs);

	set_recs_per_page(file,64);
}
/*
**	Routine:	read_vsn_next()
**
**	Function:	To read next on a vision file.
**
**	Input:		buffer to receive record
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
int read_vsn_next(file,buffer)
struct idxfile *file;
char *buffer;
{
	int st;
	
	st=i_next(file->fd, buffer);
	if (st)
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}
/*
**	Routine:	read_vsn_first()
**
**	Function:	To read the first record in a vision file.
**
**	Input:		buffer to receive record
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
int read_vsn_first(file,buffer)
struct idxfile *file;
char *buffer;
{
	int st;
	
	memset(buffer+file->keyoffs,'\0',file->keylen);
	st=i_start(file->fd, buffer, 0,file->keylen,F_NOT_LESS);
	if (st==0)
	{
		return FALSE;
	}
	st=i_next(file->fd, buffer);
	if (st)
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}
/*
**	Routine:	read_vsn_keyed()
**
**	Function:	To read a record with the given key
**
**	Input:		buffer to receive, pointer to key
**			
**	Output:		None
**			
**
**	Return:		FALSE=record not found,
**                      TRUE=record read
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
int read_vsn_keyed(file,buffer,key)
struct idxfile *file;
char *buffer;
char *key;
{
	int st;
	extern short f_errno;

	memcpy(buffer+file->keyoffs,key,file->keylen);
	st=i_read(file->fd,buffer,0);

	if (st)
	{
		return TRUE;
	}
	else
	{
		st=f_errno;
		return FALSE;
	}
}
/*
**	Routine:	setup_oa()
**
**	Function:	To setup the order area
**
**	Input:		screen data starting row,
**                      the wcc,
**                      the cursor row and col
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
void setup_oa(screen,rownum,wcc,crow,ccol)
struct vwang_screen *screen;
char rownum,wcc,crow,ccol;
{
	screen->oa.rownum = rownum;
	screen->oa.wcc    = wcc;
	screen->oa.cursor_row = crow;
	screen->oa.cursor_col = ccol;
}
/*
**	Routine:	setup_fkeys()
**
**	Function:	To stick the fkey labels into the screen map.
**
**	Input:		None
**			
**
**	Output:		None
**			
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
void setup_fkeys(file,status,rec_disp_start)
struct idxfile *file;
int status,rec_disp_start;
{
	int bytesper,hexmode,bytes_last_screen;
	
	hexmode = CHKFLAG(status,DISP_MODE_HEX)!=0;
	bytesper = hexmode?32:64;
	bytes_last_screen = hexmode ? 512:1024;
	
	if (!at_first)
	{
		INSERTSCN(main_scr,0,9,"(2)First");
	}
	else
	{
		INSERTSCN(main_scr,0,9,"        ");
	}		
	if (!at_eof_flag)
	{
		INSERTSCN(main_scr,0,38,"(5)Next");
	}
	else
	{
		INSERTSCN(main_scr,0,38,"        ");
	}		
	if (rec_disp_start)
	{
		INSERTSCN(main_scr,0,46,"(6)Down");
	}
	else
	{
		INSERTSCN(main_scr,0,46,"        ");
	}		
	if (file->recs_per_page == 1)
	{
		if (rec_disp_start< ((file->reclen - bytes_last_screen) & ~(bytesper-1))+bytesper)
		{
			INSERTSCN(main_scr,0,54,"(7)Up");
		}
		else 		
		{
			INSERTSCN(main_scr,0,54,"     ");
		}
	}
	else if (!at_eof_flag)
	{
		INSERTSCN(main_scr,0,54,"(7)Up");
	}
	else
	{
		INSERTSCN(main_scr,0,54,"     ");
	}		
	INSERTSCN(main_scr,0,60,"(8)Find Record");
	INSERTSCN(main_scr,1,55,"(15)Print");
	INSERTSCN(main_scr,1,65,"(16)End of Job");
	INSERTSCN(main_scr,1,1,"(9)Find Text");
	
	
	if (CHKFLAG(status,DISP_MODE_HEX)==TRUE)
	{
		INSERTSCN(main_scr,1,14,"(10)ASCII Mode");
	}
	else
	{
		INSERTSCN(main_scr,1,14,"(10)Hex mode  ");
	}
	memset(&main_scr.screen[80*2],' ',80*22);
}

static void initialize(file)
struct idxfile *file;
{
	extern char *recbuf;
	
	ring_open(&reclist,file->reclen,file->recs_per_page,1,NULL,0);
	recbuf=calloc(file->reclen,1);
	memset(vd_text_buf,' ',64);
	vd_text_buf[64]=(char)0;
	
}	
static void shutdown(file)
struct idxfile *file;
{
	free(recbuf);
	ring_close(reclist);
	i_close(file->fd);
}
void set_recs_per_page(file,bytes_per_line)
struct idxfile *file;
int bytes_per_line;
{
	int recdatalines;
	
	recdatalines = file->reclen/bytes_per_line + (file->reclen%bytes_per_line?1:0);
	++recdatalines;
	
        file->recs_per_page = 17 / recdatalines;
	if (file->recs_per_page == 0)
	{
		file->recs_per_page = 1;
	}
}
static int srch_len;
static char srch_ch_map[256];
static int srch_csense=TRUE;

void search_text(file,status,scroll_count,keybuf,foundpos)
struct idxfile *file;
int status;
int scroll_count;
char *keybuf;
int *foundpos;
{
	int recidx, reccnt, iter;
	char srchbuf[65];
	
	memcpy(srchbuf,keybuf,64);
	srchbuf[64]=(char)0;
	trunc_srchbuf(srchbuf);
	setup_search(srchbuf,status);

	ring_count(reclist,&reccnt);
	for (recidx=0; recidx<reccnt; ++recidx)
	{
		ring_get(reclist,recidx,recbuf);
		if ((*foundpos = stx(recbuf,srchbuf)) != -1)
		{
			return;
		}
	}
	iter = 0;
	while (file->next(file,recbuf))
	{
		if ((*foundpos = stx(recbuf,srchbuf)) != -1)
		{
			return;
		}
		++iter;
		if (iter % 100)
		{
			if (vwang_keypressed(1))
			{
				return;
			}
		}
	}
	*foundpos = -1;
}
void setup_search(srchptr,status)
char *srchptr;
int status;
{
	srch_len=0;
	memset(srch_ch_map,0,256);
	while (*srchptr != (char)0)
	{
		if (CHKFLAG(status,SEARCH_ANY))
		{
			if (!isalpha((int)*srchptr))
			{
				srch_ch_map[*srchptr]=TRUE;
			}
			else
			{
				srch_ch_map[tolower(*srchptr)]=TRUE;
				srch_ch_map[toupper(*srchptr)]=TRUE;
			}
		}
		else
		{
			srch_ch_map[*srchptr]=TRUE;
		}
		++srch_len;
		++srchptr;
	}
	if (CHKFLAG(status,SEARCH_ANY))
	{
		srch_csense = FALSE;
	}
	else
	{
		srch_csense = TRUE;
	}
}
static int stx(unsigned char *string, unsigned char *mask)	/* Get string index in another string.	*/
{
	register m_idx, s_idx, cmpval;
	register int m_len, s_len;
	int save;

	m_len = srch_len - 1;
	s_len = strlen((char *)string);
	save = s_idx = m_idx = m_len;
	if (srch_csense==FALSE)
	{
		while (s_idx < s_len)
		{
			while ( m_idx >= 0 )
			{
				cmpval =  mask[m_idx] ^ string[s_idx] ;
				if (cmpval && (cmpval != 0x20)) break;
				cmpval = mask[m_idx] & 0xdf;
				if (cmpval < 'A' || cmpval >'Z')
				  if (mask[m_idx] != string[s_idx]) break;
				--m_idx; --s_idx;
			}
			if (m_idx < 0) return s_idx + 1;
			else
			{
				while (srch_ch_map[string[s_idx]] && (save-s_idx < m_len)) --s_idx;
				save = s_idx += m_len + 1;
				m_idx = m_len;
			}
		}
	}
	else
	{
		while (s_idx < s_len)
		{
			while ( m_idx >= 0 )
			{
				cmpval =  mask[m_idx] ^ string[s_idx] ;
				if (cmpval) break;
				--m_idx; --s_idx;
			}
			if (m_idx < 0) return s_idx + 1;
			else
			{
				while (srch_ch_map[string[s_idx]] && (save-s_idx < m_len)) --s_idx;
				save = s_idx += m_len + 1;
				m_idx = m_len;
			}
		}
	}
	return -1;
}
void trunc_srchbuf(str)
char *str;
{
	int len;
	len = strlen(str)-1;

	while(len>=0)
	{
		if(str[len]==' '  ||
		   str[len]=='\214')
			str[len--] = '\0';
		else
			break;
	}
}

/*
**	Routine:	vdisp_file_type()
**
**	Function:	To determine index file type
**
**	Input:		name of the file
**			
**
**	Output:		None
**			
**
**	Return:		the type of index file or text
**
**	Warnings:	None
**
**	History:	04/29/93         Written by JEC
**
*/
int vdisp_file_type(filename)
char *filename;
{
	int the_fd,cnt,st;
	int4 magic;
	
	the_fd=open(filename,O_RDONLY);
	
	if (the_fd<0)
	{
		return VD_ERR_OPEN;
	}
	cnt=read(the_fd,&magic,sizeof(magic));
	close(the_fd);
	if (cnt<0)
	{
		return VD_ERR_READ;
	}
	switch(magic)
	{
	      case ACU_B: 
	      case ACU_L:
		st = VD_TYPE_VISION;
		break;

	      case MF_CISAM_B:
	      case MF_CISAM_L:
		st = VD_TYPE_MF;
		break;

	      default: 
                st = VD_TYPE_TEXT;
		break;
	}
	return st;
}

/********************************************************************/
/* the MF functionality is not implemented, but the hooks are there */
/********************************************************************/
int4 open_mf_file(filename)
char *filename;
{
	return 0;
}
void get_mf_info(file)
struct idxfile *file;
{
}
int read_mf_first(file,buffer)
struct idxfile *file;
char *buffer;
{
	return 0;
}
int read_mf_next(file,buffer)
struct idxfile *file;
char *buffer;
{
	return 0;
}
int read_mf_keyed(file,buffer,key)
struct idxfile *file;
char *buffer; 
char *key;
{
	return 0;
}
#else
static int dummy_vdispidx;
#endif
/*
**	History:
**	$Log: vdispidx.c,v $
**	Revision 1.14  2003/02/04 18:29:13  gsl
**	fix -Wall warnings
**	
**	Revision 1.13  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.12  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.11  2002/07/18 21:04:28  gsl
**	Remove MSDOS code
**	
**	Revision 1.10  2002/07/10 21:05:26  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  2002/06/21 03:10:42  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.8  1997/03/12 18:14:28  gsl
**	Change to use WIN32
**	
**	Revision 1.7  1996-08-19 18:33:03-04  gsl
**	drcs update
**
**
**
*/
