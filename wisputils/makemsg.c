static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

#include "intdef.h"

main()
{
	char linein[256],*scn_ptr,*prm_ptr;
	char msg[256];
	int4 id,num_msgs;
	int	msg_len;
	int4 last_id,i;
	int4 id_loc,msg_loc;

	FILE *text_file;
	FILE *idx_file;
	char* iname = "wispmsg.txt";
	char* oname = "wispmsg.dat";

	printf("makemsg: ");

	text_file = fopen(iname,"r");						/* Open the text file.			*/

	if (!text_file)
	{
		printf("error opening input file %s [errno=%d].\n",iname,errno);
		exit(-1);
	}

	num_msgs = 0;
	while (fgets(linein,sizeof(linein)-1,text_file)) { num_msgs++;}		/* First count them.			*/

	fseek(text_file,0,SEEK_SET);						/* Reset the file.			*/

	printf("There are %d messages in the %s\n", num_msgs, iname);
	printf("Building index file %s\n", oname);

#ifndef unix
	idx_file = fopen(oname,"wb");						/* Open the indexed file.		*/
#else
	idx_file = fopen(oname,"w");						/* Open the indexed file.		*/
#endif

	if (!idx_file)
	{
		printf("error creating index file %s [errno=%d].\n",oname,errno);
		exit(-1);
	}

	last_id = 0;									/* The last ID. For error checks.	*/

	/*
		4	Number of message
		(n+1)*8	Index: pairs of number+location
		x	Message text:
	*/

	id_loc = 0 + sizeof(num_msgs);							/* The next ID goes in byte 4.		*/
	msg_loc = id_loc + ((num_msgs + 1) * (sizeof(id) + sizeof(msg_loc)));		/* The next message goes here.		*/
	
	fwrite(&num_msgs,sizeof(num_msgs),1,idx_file);					/* Write the count.			*/

	for (i=0; i < (num_msgs+1); i++) fwrite("        ",8,1,idx_file);		/* Extend the file.			*/

	while (fgets(linein,sizeof(linein)-1,text_file))
	{
		scn_ptr = &linein[0];

		while ((*scn_ptr == ' ') || (*scn_ptr == '\t')) {scn_ptr++;}		/* Skip whitespace			*/

		id = 0;

		while (isdigit(*scn_ptr))						/* Get the id number.			*/
		{
			id = id * 10;
			id = id + (*scn_ptr++ - '0');
		}

		if (id == 0)
		{
			printf("Error parsing input line, line is:\n%s\n",linein);
			exit(-1);
		}
		else if (id <= last_id)
		{
			printf("Error parsing input line, id out of order.\n");
			printf("Last ID = %d,  Current ID = %d.\n",last_id,id);
			exit(-1);
		}

		last_id = id;

		while ((*scn_ptr == ' ') || (*scn_ptr == '\t')) {scn_ptr++;}		/* Skip whitespace			*/

		prm_ptr = &msg[0];

		msg_len = 0;

		while (*scn_ptr && (*scn_ptr != '\n'))					/* Copy the text			*/
		{
			*prm_ptr++ = *scn_ptr++;
			msg_len++;
		}
		*prm_ptr = 0;

		/*
			Write out pairs of (message number)+(message text location)
		*/
		fseek(idx_file,id_loc,SEEK_SET);					/* Set ptr to loc for id/ptr in file.	*/
		fwrite(&id,sizeof(id),1,idx_file);					/* Write the ID.			*/
		fwrite(&msg_loc,sizeof(msg_loc),1,idx_file);				/* Write The message loc.		*/
		id_loc += sizeof(id) + sizeof(msg_loc);					/* point to next location.		*/

		/*
			Write out the message test (at message location).
		*/
		fseek(idx_file,msg_loc,SEEK_SET);					/* Point to where message will go.	*/
		fwrite(msg,msg_len,1,idx_file);						/* Write The message loc.		*/
		msg_loc = msg_loc + msg_len;						/* Get where next one will go.		*/
	}

	fseek(idx_file,id_loc,SEEK_SET);						/* Set ptr to loc for id/ptr in file.	*/
	id = id + 1;									/* Need an ending ID			*/
	fwrite(&id,sizeof(id),1,idx_file);						/* Write the ID.			*/
	fwrite(&msg_loc,sizeof(msg_loc),1,idx_file);					/* Write The message loc.		*/

	fclose(text_file);								/* Close the text file			*/
	fclose(idx_file);								/* Close the indexed file.		*/
	return 0;
}
/*
**	History:
**	$Log: makemsg.c,v $
**	Revision 1.10  1996-07-23 21:16:10-04  gsl
**	Fix for NT
**
**	Revision 1.9  1996-07-23 11:12:55-07  gsl
**	drcs update
**
**
**
*/
