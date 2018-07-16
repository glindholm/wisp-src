			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	orarts:		This routine is used to build a Micro Focus COBOL/2 
			Run Time System (RTS or RTE).


*/



/*
	video() is a dummy routine which is never called.
	It exists to force the linker (ld) to include the
	listed routines into the RTS.
*/

int video_dummy = 1;

video()
{
	if (video_dummy) return;

/*
	The following are a list of all "C" video routines that could be called from 
	a RYLAND COBOL program.

	To add additional routines to the the RTS you can follow this example 
	and add a reference to the routine.
*/

        VCLOSEFORMF();
        VCLOSETERM(); 
        VERRMSG();
        VFIELDEDITS();
        VFINISHFORM();
        VGETBUFFER();
        VGETFIELDINFO();
        VGETNEXTFORM();
        VINITFORM();
        VLOADFORMS();  
        VOPENFORMF();
        VOPENTERM();
        VPRINTFORM();   
        VPUTBUFFER();
        VPUTWINDOW(); 
        VREADFIELDS();
        VSETERROR();
        VSETKEYLABELS();
        VSHOWFORM();

}
