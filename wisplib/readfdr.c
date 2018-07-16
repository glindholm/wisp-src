			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/* READFDR.C ... This routine simulates some of the Wang-VS READFDR subroutine operation.					*/

#ifdef VMS
#include <rms.h>
#include <ssdef.h>
#include <stat.h>
#endif

#ifdef unix
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifdef MSDOS
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <varargs.h>
#include "wcommon.h"
#include "movebin.h"
#include "cobrun.h"
#include "werrlog.h"

#ifdef VMS
#define	FILE_TYPE	001
#define	RECORD_COUNT	002
#define	RECORD_SIZE	003
#endif

char *wfname();
extern char WISPFILEXT[39];

#define		ROUTINE		51000
READFDR(va_alist)	  								/* Function uses variable arguments.	*/
va_dcl
{
	va_list the_args;
	int 	arg_count;
	long	l_long, *retcod, access_code, *temp_long_ptr, l_mode;
	char 	*end_name, *temp_ptr;
	char 	*l_vol,*l_lib,*l_file, *l_field;
	char 	l_name[132], l_name_x[132];
	long 	wfname_mode;
	int	default_case;
	int	rc;
	struct stat sbuf;
	struct tm *localtime(), *tm_ptr;
	char 	tmp[7];
	char 	tmp_bs[19];
	int	is_cisam;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say hello.				*/

	va_start(the_args);								/* Set pointer to the top of the stack.	*/
	arg_count = va_count(the_args);
	va_start(the_args);								/* Set pointer to the top of the stack.	*/

	l_file = va_arg(the_args, char*);						/* Get addr. of the filename argument.	*/
	l_lib  = va_arg(the_args, char*);						/* Get addr. of the library argument.	*/
	l_vol  = va_arg(the_args, char*);						/* Get addr. of the volume argument.	*/
	temp_long_ptr = va_arg(the_args, long*);					/* Get addr. of the call mode argument.	*/
	arg_count -= 4;
	GETBIN(&l_mode, temp_long_ptr, sizeof(l_mode) );				/* Move to local var.			*/ 
	if (l_mode != 0 )
	{
		werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);
		return(0);
	}
	wfname_mode = 0;

	if ( rms_files && (WISPFILEXT[0] == ' ' || WISPFILEXT[0] == 0))			/* Need to use our file extension.	*/
	{
		setwispfilext("DAT");							/* Just copy it in.			*/
	}
	else if (!memcmp(WISPFILEXT,"LIS ",4))						/* Special case for printfiles.		*/
	{
		wfname_mode = IS_PRINTFILE;
	}

	end_name = wfname(&wfname_mode,l_vol,l_lib,l_file,l_name);			/* generate a VMS file name		*/
	*end_name = '\0';								/* Null terminate			*/

	is_cisam = 0;
	if ( cisam_files )								/* Try indexed file extension.		*/
	{
		strcpy(l_name_x,l_name);
		strcat(l_name_x, ".idx");
	}

	if (findcase(l_name,l_name))							/* does the file exist?			*/
	{
		access_code = 20;							/* No, set retcod = 20			*/
		
		if ( cisam_files && (WISPFILEXT[0] == ' ' || WISPFILEXT[0] == 0) )	/* Try indexed file extension.		*/
		{
			if (!findcase(l_name_x,l_name_x)) 
			{
				access_code = 0;
				is_cisam = 1;
			}
		}

	}
	else
	{										/* yes it exists, set retcod = 0	*/
		access_code = 0;
	}

	while ( arg_count > 1 )
	{
		l_field  = va_arg(the_args, char*);					/* Get the field indicator.		*/
		temp_ptr = va_arg(the_args, char*);					/* Get the receiver.			*/
		arg_count -= 2;

		if (access_code != 0 ) continue;

		default_case = 0;
		switch( l_field[0] )							/* Determine what the next arg type is.	*/
		{
			case 'B':
			{
				switch( l_field[1] )
				{
					case 'S':					/* Byte size.				*/
					{
						rc=stat(l_name,&sbuf);

						if ( rc )
						{
							access_code = rc;
							break;
						}
						l_long = sbuf.st_size;
						memset(tmp_bs,0,sizeof(tmp_bs));
						sprintf(tmp_bs,"%018d",l_long);
						strncpy(temp_ptr,tmp_bs,18);
						break;
					}
				}
				break;
			}
			case 'C':
			{
				switch ( l_field[1] )
				{
				case 'D':						/* Creation date.			*/
					if (rms_files) 
					{
						strcat(l_name,";-0");			/* we want the earliest version		*/
					}

					if (is_cisam)
					{
						rc=stat(l_name_x,&sbuf);
					}
					else
					{
						rc=stat(l_name,&sbuf);
					}
					if ( rc < 0 )
					{
						access_code = 32;
						break;
					}

					tm_ptr=localtime(&(sbuf.st_ctime));
					memset(tmp,0,sizeof(tmp));
					sprintf(tmp,"%02d%02d%02d",tm_ptr->tm_year,tm_ptr->tm_mon+1,tm_ptr->tm_mday);
					strncpy(temp_ptr,tmp,6);
					break;

				default:
					default_case = 1;
					break;
				}
				break;
			}
			case 'D':
			{
				switch( l_field[1] )
				{
				case 'P':
					memset(temp_ptr,0,4);
					break;
					
				case 'T':
					*temp_ptr = 'D';
					break;

				default:
					default_case = 1;
					break;
				}
			}
			case 'E':
			{
				switch( l_field[1] )
				{
				case 'A':
					l_long = 1;
					wswap(&l_long);					/* swap the words			*/
					PUTBIN(temp_ptr, &l_long, sizeof(l_long));
					break;
					
				case 'D':
					memcpy(temp_ptr,"991231",6);
					break;

				default:
					default_case = 1;
					break;
				}
			}
			case 'F':
			{
				switch( l_field[1] )
				{
				case 'T':					    /* File type.			*/
#ifdef VMS
					if (rms_files)
					{
						read_fab(&l_long, l_name, FILE_TYPE);	    /* Get the file organization.	*/
						*temp_ptr=l_long;			    /* FILE_TYPE returns a character.	*/
						if (wfname_mode == IS_PRINTFILE) *temp_ptr='P';	    /* Redundancy, just in case.*/
						break;
					}
#endif	/* #ifdef VMS */

#ifdef unix
					if (vision_files)
					{

						rc = acuvision( l_name, "FT", temp_ptr );
						access_code = rc;
						break;
					}
					else if (cisam_files)
					{
						rc = cisaminfo( l_name, "FT", temp_ptr );
						access_code = rc;
						break;
					}
					else
					{
						default_case = 1;
					}
#endif	/* #ifdef unix */

#ifdef MSDOS

#ifdef MSDOS_BYPASS	/* temp removed */

					if (vision_files)
					{

						rc = acuvision( l_name, "FT", temp_ptr );
						access_code = rc;
						break;
					}
					else if (cisam_files)
					{
						rc = cisaminfo( l_name, "FT", temp_ptr );
						access_code = rc;
						break;
					}
					else
#endif	/* #ifdef MSDOS_BYPASS */
					{
						default_case = 1;
					}
#endif	/* #ifdef MSDOS */
					break;
				default:
					default_case = 1;
					break;
				}
				break;
			}
			case 'M':
			{
				switch ( l_field[1] )
				{
				case 'D':						/* Mod date.				*/
					if (rms_files) 
					{
						strcat(l_name,";");			/* we want the latest version		*/
					}

					if (is_cisam)
					{
						rc=stat(l_name_x,&sbuf);
					}
					else
					{
						rc=stat(l_name,&sbuf);
					}
					if ( rc < 0 )
					{
						access_code = 32;
						break;
					}

					tm_ptr=localtime(&(sbuf.st_mtime));
					memset(tmp,0,sizeof(tmp));
					sprintf(tmp,"%02d%02d%02d",tm_ptr->tm_year,tm_ptr->tm_mon+1,tm_ptr->tm_mday);
					strncpy(temp_ptr,tmp,6);
					break;

				default:
					default_case = 1;
					break;
				}
				break;
			}
			case 'R':
			{
				switch( l_field[1] )
				{
#ifdef VMS
					case 'C':					    /* Record count.			*/
					{
						read_fab(&l_long, l_name, RECORD_COUNT);    /* Get the record count.		*/
						wswap(&l_long);				    /* swap the words			*/
						PUTBIN(temp_ptr, &l_long, sizeof(l_long));
						break;
					}
					case 'L':					    /* Record length.			*/
					case 'S':					    /* Record size.			*/
					{
						read_fab(&l_long, l_name, RECORD_SIZE);	    /* Get the record size.		*/
						wswap(&l_long);				/* swap the words			*/
						PUTBIN(temp_ptr, &l_long, sizeof(l_long));
						break;
					}
#endif	/* #ifdef VMS */

#ifdef unix
					case 'C':					    /* Record count.			*/
					{
						if (vision_files)
						{
							rc = acuvision( l_name, "RC", &l_long );
						}
						else if (cisam_files)
						{
							rc = cisaminfo( l_name, "RC", &l_long );
						}
						else
						{
							default_case = 1;
							break;
						}

						if ( rc )
						{
							access_code = rc;
							break;
						}
						wswap(&l_long);				/* swap the words			*/
						PUTBIN(temp_ptr, &l_long, sizeof(l_long));
						break;
					}
					case 'L':					    /* Record length.			*/
					case 'S':					    /* Record size.			*/
					{
						if (vision_files)
						{
							rc = acuvision( l_name, "RS", &l_long );
						}
						else if (cisam_files)
						{
							rc = cisaminfo( l_name, "RS", &l_long );
						}
						else
						{
							default_case = 1;
							break;
						}

						if ( rc )
						{
							access_code = rc;
							break;
						}
						wswap(&l_long);			/* swap the words			*/
						PUTBIN(temp_ptr, &l_long, sizeof(l_long));
						break;
					}
					case 'T':					    /* Record type.			*/
					{
						if (vision_files)
						{
							rc = acuvision( l_name, "RT", temp_ptr );
						}
						else if (cisam_files)
						{
							rc = cisaminfo( l_name, "RT", temp_ptr );
						}
						else
						{
							default_case = 1;
							break;
						}
						if ( rc )
						{
							access_code = rc;
							break;
						}
						break;
					}
#endif	/* #ifdef unix */

#ifdef MSDOS

					case 'C':					    /* Record count.			*/
					{

#ifdef MSDOS_BYPASS	/* temp removed */

						if (vision_files)
						{
							rc = acuvision( l_name, "RC", &l_long );
						}
						else if (cisam_files)
						{
							rc = cisaminfo( l_name, "RC", &l_long );
						}
						else
#endif	/* #ifdef MSDOS_BYPASS */
						{
							default_case = 1;
							break;
						}

						if ( rc )
						{
							access_code = rc;
							break;
						}
						wswap(&l_long);				/* swap the words			*/
						PUTBIN(temp_ptr, &l_long, sizeof(l_long));
						break;
					}
					case 'L':					    /* Record length.			*/
					case 'S':					    /* Record size.			*/
					{

#ifdef MSDOS_BYPASS	/* temp removed */

						if (vision_files)
						{
							rc = acuvision( l_name, "RS", &l_long );
						}
						else if (cisam_files)
						{
							rc = cisaminfo( l_name, "RS", &l_long );
						}
						else
#endif	/* #ifdef MSDOS_BYPASS */
						{
							default_case = 1;
							break;
						}

						if ( rc )
						{
							access_code = rc;
							break;
						}
						wswap(&l_long);			/* swap the words			*/
						PUTBIN(temp_ptr, &l_long, sizeof(l_long));
						break;
					}
					case 'T':					    /* Record type.			*/
					{

#ifdef MSDOS_BYPASS	/* temp removed */

						if (vision_files)
						{
							rc = acuvision( l_name, "RT", temp_ptr );
						}
						else if (cisam_files)
						{
							rc = cisaminfo( l_name, "RT", temp_ptr );
						}
						else
#endif	/* #ifdef MSDOS_BYPASS */
						{
							default_case = 1;
							break;
						}
						if ( rc )
						{
							access_code = rc;
							break;
						}
						break;
					}
#endif	/* #ifdef MSDOS */
					default:
					{
						default_case = 1;
						break;
					}
				}
				break;
			}
			default:
			{
				default_case = 1;
				break;
			}
		}  /* End of Case */

		if (default_case)
		{
			werrlog(ERRORCODE(4),l_field,0,0,0,0,0,0,0);
			access_code = 40;
		}

	}       
	if ( arg_count == 1 )
	{
		temp_ptr = va_arg(the_args, char *);					/* Get the return code pointer.		*/
		wswap(&access_code);							/* swap the words			*/
		PUTBIN(temp_ptr, &access_code, sizeof(access_code) );
	}

}
                                                                                       
#ifdef VMS
static int read_fab(recvr, file_name, action)

char *file_name;
int *recvr;
short action;

{
	struct FAB file_block;
        struct RAB record_block;

	int rms_status;
        char *byte_buffer, *malloc();

	file_block = cc$rms_fab;
	file_block.fab$l_fna = file_name;
	file_block.fab$b_bks = 4;
	file_block.fab$l_dna = ".DAT";
	file_block.fab$b_dns = 4;
	file_block.fab$b_fac = FAB$M_GET;
	file_block.fab$b_fns = 80;                                                     
	file_block.fab$l_fop = FAB$M_CIF;
	file_block.fab$b_shr = FAB$V_SHRPUT | FAB$V_SHRGET | FAB$V_SHRDEL | FAB$V_SHRUPD;
	record_block = cc$rms_rab;
	record_block.rab$l_fab = &file_block;
	
	rms_status = sys$open(&file_block);
	if (rms_status != RMS$_NORMAL) return;

	switch(action)
	{
	    case FILE_TYPE:
	    {
		switch(file_block.fab$b_org)
		{
		    case FAB$C_SEQ:							/* File organization is sequential.	*/
		    {
			    *recvr='C';							/* 'C' for Consecutive.			*/
			    break;
		    }
		    case FAB$C_REL:							/* File organization is relative.	*/
		    {
			    *recvr='R';
			    break;
		    }
		    case FAB$C_IDX:							/* File organization is indexed.	*/
		    {
			    *recvr='I';
			    break;
		    }
		    case FAB$C_HSH:							/* File organization is hashed.		*/
		    {
			    *recvr='U';							/* 'U' for Unknown to the WANG.		*/
			    break;
		    }
		    default:								/* VAX doesn't give any others.		*/
		    {
			    *recvr='U';
			    break;
		    }
		}
		break;
	    }
	    case RECORD_COUNT:
	    {
	    	byte_buffer = malloc(file_block.fab$w_mrs);
	    	record_block.rab$l_ubf = byte_buffer;
	    	record_block.rab$w_usz = file_block.fab$w_mrs;
	    	record_block.rab$l_rop = RAB$V_NLK;

	    	rms_status = sys$connect(&record_block);
	    	rms_status = sys$get(&record_block);

	    	if (rms_status == RMS$_NORMAL ||				/* Just verify successful retrieval of RAB info.*/
		    rms_status == RMS$_OK_RLK ||				/* Record was locked but we read it.		*/
		    rms_status == RMS$_RLK ||					/* Record was locked, so it must exist.		*/
		    rms_status == RMS$_OK_RRL)
	    	{
	    		*recvr = 1;						/* Return 1 record if records exist.		*/
	    	}
	    	else
	    	{
	    		*recvr = 0;						/* Return 0 records, if not.			*/
	    	}
		break;
	    }
	    case RECORD_SIZE:
	    {
		*recvr = file_block.fab$w_mrs;					/* Return the maximum record size.		*/
		break;
	    }
	    default:	break;
	}
	rms_status = sys$close(&file_block);

}
#endif	/* #ifdef VMS	*/

