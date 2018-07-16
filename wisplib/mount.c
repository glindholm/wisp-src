static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/********************************************************************************************************************************
*																*
*	MOUNT.C  Emulation of WANG USERSUB:  MOUNT										*
*																*
********************************************************************************************************************************/

#ifdef VMS
#include <descrip.h>
#include <stdio.h>
#include <ctype.h>
#include <ssdef.h>
#include <mntdef.h>
#include "idsistd.h"
#include "wdefines.h"
#include <varargs.h>                                                                    /* Allow variable number of arguments	*/

#define MAX_DEVICE_NAME_LENGTH	80

static struct
{
	short len;
	short code;
	char *addr;
	char *retadr;
} mntlist[4];

MOUNT(va_alist)

va_dcl
{
	va_list	the_args;								/* A pointer to traverse the stack.	*/
	int	arg_count,i;
	int4	status;
	int4	device,vsid,sysuse,*retcod;
	char	*volume,*label,*usage,*dtype,*bypass,*msg,*disp,*addr,dname[MAX_DEVICE_NAME_LENGTH];
	char 	volnam[7];
	char	errbuff[256];
	/********************************************************
	*	Receive variable number of arguments		*
	********************************************************/
	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/

	va_start(the_args);								/* Go back to the top of the stack.	*/

	device = *va_arg(the_args, int*);						/* Get device number.			*/
	wswap(&device);
	volume = va_arg(the_args,char*);						/* Get volume name.			*/

	arg_count -= 2;									/* 2 down.				*/

	vsid = 0;									/* Default vsid.			*/
	label = "A";									/* Standard label.			*/
	usage = "S";									/* Shared device.			*/
	dtype = "R";									/* Removable drive.			*/
	sysuse = 0;									/* No system usage.			*/
	bypass = "S";									/* Standard label processing.		*/
	msg = "M";									/* Normal Mount message.		*/
	disp = "D";									/* Display them.			*/
	addr = "S";									/* Standard addressing.			*/

	if (arg_count > 1)
	{
		vsid = *va_arg(the_args,int*);						/* Get the vsid.			*/
		wswap(&vsid);
		arg_count--;
	}

	if (arg_count > 1)
	{
		label = va_arg(the_args,char*);						/* Get the label option.		*/
		arg_count--;
	}

	if (arg_count > 1)
	{
		usage = va_arg(the_args,char*);						/* Usage type.				*/
		arg_count--;
	}

	if (arg_count > 1)
	{
		dtype = va_arg(the_args,char*);						/* Drive type.				*/
		arg_count--;
	}

	if (arg_count > 1)
	{
		sysuse = *va_arg(the_args,int*);					/* System usage.			*/
		wswap(&sysuse);
		arg_count--;
	}

	if (arg_count > 1)
	{
		bypass = va_arg(the_args,char*);					/* Bypass label processing.		*/
		arg_count--;
	}

	if (arg_count > 1)
	{
		msg = va_arg(the_args,char*);						/* Message processing.			*/
		arg_count--;
	}

	if (arg_count > 1)
	{
		disp = va_arg(the_args,char*);						/* Display processing.			*/
		arg_count--;
	}


	if (arg_count > 1)
	{
		addr = va_arg(the_args,char*);						/* Disk addressing mode.		*/
		arg_count--;
	}

	retcod = va_arg(the_args,int*);							/* The return code.			*/
	*retcod = 0;									/* Set to zero first.			*/

	status = get_dvname(device,dname);						/* Load the device-volume map.		*/

	if (status == 0)								/* Didn't find it			*/
	{
		*retcod = 16;								/* Say so, then exit.			*/
		wswap(retcod);
		return;
	}

	mntlist[0].len = strlen(dname);							/* Define the device name		*/
	mntlist[0].code = MNT$_DEVNAM;
	mntlist[0].addr = dname;
	mntlist[0].retadr = 0;

	memcpy(volnam,volume,6);
	volnam[6] = '\0';
	if (i = strpos(volnam," ") != -1) volnam[i] = '\0';

	mntlist[1].len = strlen(volnam);						/* Define the volume name.		*/
	mntlist[1].code = MNT$_VOLNAM;
	mntlist[1].addr = volnam;
	mntlist[1].retadr = 0;

	mntlist[2].len = 0;
	mntlist[2].code = 0;

	status = SYS$MOUNT(mntlist);							/* Do it.				*/

	if (status == SS$_NORMAL) return;						/* Success.				*/

	sprintf(errbuff,"MOUNT error, device=%s, vol=%s, status=%x (hex)",dname,volnam,status);
	werr_message_box(errbuff);
}



typedef struct
{
	int	devnum;									/* The Wang device number.		*/
	char	dname[MAX_DEVICE_NAME_LENGTH];						/* The VAX device name.			*/
	struct dstruct *next;								/* The next device.			*/
} dstruct;

static dstruct *devlist = 0;								/* This is the list.			*/
static dstruct *devptr;									/* And a scratch pointer		*/

get_dvname(dnum,dname)									/* Scan the device list, return the name*/
int dnum;
char *dname;
{

	load_dv();									/* Load the device information.		*/

	devptr = devlist;

	do
	{
		if (devptr->devnum == dnum)						/* Found it.				*/
		{
			strcpy(dname,devptr->dname);
			return(1);
		}
		devptr = (dstruct *)devptr->next;
	} while (devptr);

	return(0);									/* Was not found			*/
}




load_dv()										/* Load the device list.		*/
{
	char *prm_ptr,*scn_ptr,tempc,inline[133];
	int flag;
	FILE *the_file;
	char	errbuff[256];

	if (devlist) return(1);								/* Already did.				*/

	the_file = fopen(WISP_TAPE_FILE,"r");						/* Try to open the file.		*/

	if (!the_file)									/* Error opening file.			*/
	{
		sprintf(errbuff,"WISPLIB error (get_dvname). Error opening file %s",WISP_TAPE_FILE);
		werr_message_box(errbuff);
		return(0);
	}

	if (fgets(inline,132,the_file))   do
	{

		if (!devlist)								/* first time?				*/
		{
			devlist = (dstruct *)malloc(sizeof(dstruct));			/* get some memory			*/
			devlist->next = 0;						/* set next pointer to zero		*/
			devlist->dname[0] = '\0';
			devlist->devnum = 0;
			devptr = devlist;						/* set up local pointer			*/
		}
		else
		{
			devptr->next = (struct dstruct *)malloc(sizeof(dstruct));	/* get some memory			*/
			devptr = (dstruct *)devptr->next;				/* set pointer				*/
			devptr->next = 0;						/* set next pointer to zero		*/
			devptr->dname[0] = '\0';
			devptr->devnum = 0;
		}

		scn_ptr = &inline[0];

		flag = 1;								/* switch to parse name, number		*/

		do									/* scan the line and extract the parms	*/
		{									/* skip over space, comma, newline, tab */
			if ((*scn_ptr == ' ') || (*scn_ptr == ',') || (*scn_ptr == '\n') || (*scn_ptr == '\t'))
			{
				scn_ptr++;
			}
			else								/* copy this parm			*/
			{
				if (flag == 1)						/* copy term name			*/
				{
					flag = 2;					/* set the flag				*/
					prm_ptr = devptr->dname;			/* load device name			*/
					*prm_ptr = *scn_ptr++;			/* get first char			*/
					if (*prm_ptr != '_')			/* have to put in a leading '_'		*/
					{
						tempc = *prm_ptr;		/* save char				*/
						*prm_ptr++ = '_';		/* put in '_'				*/
						*prm_ptr = tempc;		/* restore char				*/
					}
					prm_ptr++;				/* next char position			*/

					do
					{					/* copy till next whitespace		*/
						*prm_ptr++ = *scn_ptr++;
					} while ((*scn_ptr) && (*scn_ptr != ' ') && (*scn_ptr != '\n')
					      && (*scn_ptr != '\t'));
					prm_ptr--;				/* need to look at last character	*/
					if (*prm_ptr != ':')			/* has to end with a colon		*/
					{
						prm_ptr++;			/* add one on...			*/
						*prm_ptr = ':';
					}
					prm_ptr++;				/* point to end				*/
					*prm_ptr = 0;				/* end with a null			*/
				}
				else if (flag == 2)				/* copy term number			*/
				{
					flag = 1;				/* clear the flag			*/
					devptr->devnum = 0;
					do
					{					/* copy till no digits			*/
						devptr->devnum *= 10;
						devptr->devnum += (*scn_ptr++) - '0';
					} while (isdigit(*scn_ptr));		/* stop when no digits			*/
				}
			}
		} while (*scn_ptr && flag);					/* Till a null or flag is clear.	*/
	}while (fgets(inline,132,the_file));					/* while lines in file			*/

	fclose(the_file);							/* close the term file			*/

}
#else
#include "werrlog.h"
void MOUNT()
{
	werr_message_box("MOUNT: Not Supported");
}
#endif
/*
**	History:
**	$Log: mount.c,v $
**	Revision 1.11  1996/08/19 22:32:32  gsl
**	drcs update
**	
**
**
*/
