			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#define EXT extern
#include "wisp.h"

/*
	p_accept:	Process the ACCEPT verb.

			ACCEPT xxx FROM {DATE|DAY|DAY-OF-WEEK|TIME}		- unchanged.

	(before)	ACCEPT field1 field2 ... field16.

	(after)		MOVE "field1" TO WISP-BUFF(1)				- load the field tags
			MOVE "field2" TO WISP-BUFF(2)
			. . .
			MOVE "fieldx" TO WISP-BUFF(x)
			MOVE x TO WISP-LONGWORD					- Set the argument count
			CALL "WACCEPT" USING WISP-LONGWORD WISP-BUFF(1)		- call WACCEPT
			   WISP-BUFF(2) . . . WISP-BUFF(x)
			UNSTRING WISP-BUFF(1) DELIMITED DEC-BYTE-0 INTO field1	- Unload the ACCEPTed values
			UNSTRING WISP-BUFF(2) DELIMITED DEC-BYTE-0 INTO field2
			. . .
			UNSTRING WISP-BUFF(x) DELIMITED DEC-BYTE-0 INTO fieldx
*/

p_accept()
{
	int	ptype, col, col2, toknum, i, paren, item;
	char	buf1[1280], buf2[1280], buf3[1280];
	char	field[80];
	char	tstr[80];

	write_log("WISP",'I',"ACCEPTPROC","Processing ACCEPT statement.");

	ptype = get_param(o_parms[0]);						/* Get the ACCEPT token				*/
	stredt(&inline[7],o_parms[0],"");					/* Edit the token out of the inline		*/
	col = 1 + get_ppos();							/* Get the starting column			*/

	for(toknum=0;toknum<36;toknum++)					/* Load in all the tokens first			*/
	{
		ptype = get_param(o_parms[toknum]);

		if (proc_keyword(o_parms[toknum]))				/* Is it a keyword?				*/
		{
			hold_line();
			break;
		}

		stredt(&inline[7],o_parms[toknum],"");				/* Edit the token out of the inline		*/

		if (ptype == -1)						/* Period found					*/
		{
			if (!o_parms[toknum][0]) break;				/* Stand alone period				*/
			toknum++;						/* Count the last token;			*/
			break;
		}
	}
	o_parms[toknum][0] = '\0';						/* Null out the trailing o_parm			*/

	if (	0==strcmp(o_parms[toknum-1],"DATE")	   ||			/* This is a "normal" ACCEPT, just output it	*/
		0==strcmp(o_parms[toknum-1],"DAY")	   ||
		0==strcmp(o_parms[toknum-1],"DAY-OF-WEEK") ||
		0==strcmp(o_parms[toknum-1],"TIME")	      )
	{
		char	buff[200];

		buff[0] = '\0';
		add2buff(buff,"ACCEPT",col,0);
		col+=4;
		for(i=0;i<toknum;i++)
		{
			add2buff(buff,o_parms[i],col,0);
		}
		if (ptype == -1) add2buff(buff,".",col,0);
		strcat(buff,"\n");
		tput_block(buff);
		return;
	}

	buf1[0] = '\0';
	buf2[0] = '\0';
	buf3[0] = '\0';

	col2 = col+4;

#ifdef DEBUG
	add2buff(buf1,"<1>",col,1);
	add2buff(buf2,"<2>",col,1);
	add2buff(buf3,"<3>",col,1);
#endif
	add2buff(buf2,"CALL \"WACCEPT\" USING WISP-LONGWORD",col,1);
	for(i=0,item=1;i<toknum;item++)
	{
		sprintf(field,"WISP-BUFF(%d)",item);

		add2buff(buf1,"MOVE",col,1);
		sprintf(tstr,"\"%s\"",o_parms[i]);
		add2buff(buf1,tstr,col2,0);

		add2buff(buf2,field,col2,0);

		add2buff(buf3,"UNSTRING",col,1);
		add2buff(buf3,field,col2,0);
		add2buff(buf3,"DELIMITED",col2,0);
		add2buff(buf3,"DEC-BYTE-0",col2,0);
		add2buff(buf3,"INTO",col2,0);
		add2buff(buf3,o_parms[i++],col2,0);

		paren = 0;
		for(;i<toknum;)
		{
			if (0==strcmp(o_parms[i],"OF"))
			{
				add2buff(buf3,o_parms[i++],col2,0);
				add2buff(buf3,o_parms[i++],col2,0);
			}
			else if (paren)
			{
				if (o_parms[i][0] == ')')
				{
					paren--;
				}
				add2buff(buf3,o_parms[i++],col2,0);
			}
			else if (o_parms[i][0] == '(')
			{
				paren++;
			}
			else
			{
				break;
			}
		}
		add2buff(buf1,"TO",col2,0);
		add2buff(buf1,field,col2,0);

	}

	tput_block(buf1);

	buf1[0] = '\0';
	sprintf(tstr,"MOVE %d TO WISP-LONGWORD",item-1);
	add2buff(buf1,tstr,col,1);
	tput_block(buf1);

	tput_block(buf2);

	if (ptype == -1) add2buff(buf3,".",col2,0);
	tput_block(buf3);
	tput_flush();
	
	return;
}

