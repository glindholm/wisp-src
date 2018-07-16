			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include <stdio.h>
#include <ctype.h>

main()
{
	char inline[256],*scn_ptr,*prm_ptr;
	char msg[256];
	char idstr[20];
	unsigned long id,num_msgs;
	int	msg_len;
	unsigned long last_id,i;
	unsigned long id_loc,msg_loc;

	FILE *text_file;
	FILE *idx_file;

	printf("makemsg: ");

	text_file = fopen("wispmsg.txt","r");						/* Open the text file.			*/

	if (!text_file)
	{
		printf("error opening input file.\n");
		exit(-1);
	}

	num_msgs = 0;
	while (fgets(inline,255,text_file)) { num_msgs++;}				/* First count them.			*/

	fseek(text_file,0,0);								/* Reset the file.			*/

	printf("There are %d messages in the file, beginning build of indexed file.\n",num_msgs);

#ifndef unix
	idx_file = fopen("wispmsg.dat","wb");						/* Open the indexed file.		*/
#else
	idx_file = fopen("wispmsg.dat","w");						/* Open the indexed file.		*/
#endif

	if (!idx_file)
	{
		printf("error creating index file.\n");
		exit(-1);
	}

	last_id = 0;									/* The last ID. For error checks.	*/

	id_loc = 4;									/* The next ID goes in byte 4.		*/
	msg_loc = (8 * (num_msgs + 1)) + 4;						/* The next message goes here.		*/
	
	fwrite(&num_msgs,4,1,idx_file);							/* Write the count.			*/

	for (i=0; i < (num_msgs+1); i++) fwrite("        ",8,1,idx_file);		/* Extend the file.			*/

	while (fgets(inline,255,text_file))
	{
		scn_ptr = &inline[0];

		while ((*scn_ptr == ' ') || (*scn_ptr == '\t')) {scn_ptr++;}		/* Skip whitespace			*/

		id = 0;

		while (isdigit(*scn_ptr))						/* Get the id number.			*/
		{
			id = id * 10;
			id = id + (*scn_ptr++ - '0');
		}

		if (id == 0)
		{
			printf("Error parsing input line, line is:\n%s\n",inline);
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

		fseek(idx_file,id_loc,0);						/* Set ptr to loc for id/ptr in file.	*/
		fwrite(&id,4,1,idx_file);						/* Write the ID.			*/
		fwrite(&msg_loc,4,1,idx_file);						/* Write The message loc.		*/
		id_loc = id_loc + 8;							/* point to next location.		*/

		fseek(idx_file,msg_loc,0);						/* Point to where message will go.	*/
		fwrite(msg,msg_len,1,idx_file);						/* Write The message loc.		*/
		msg_loc = msg_loc + msg_len;						/* Get where next one will go.		*/
	}

	fseek(idx_file,id_loc,0);							/* Set ptr to loc for id/ptr in file.	*/
	id = id + 1;									/* Need an ending ID			*/
	fwrite(&id,4,1,idx_file);							/* Write the ID.			*/
	fwrite(&msg_loc,4,1,idx_file);							/* Write The message loc.		*/

	fclose(text_file);								/* Close the text file			*/
	fclose(idx_file);								/* Close the indexed file.		*/
}
