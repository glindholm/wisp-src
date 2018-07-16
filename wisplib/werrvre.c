			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

extern int rts_first;

werrvre(buff)
char	*buff;
{
	if (rts_first) init_screen();

	vre_window("%s",buff);								/* Use VRE.				*/

}

