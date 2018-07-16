static char config85_copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char config85_rcsid[]="$Id:$";
/* config85.c - runtime configuration options */

/* Sometimes it is desirable to create smaller versions of the runtime	*/
/* system.  This file allows you to remove selected pieces of the 	*/
/* runtime.  By removing pieces of the runtime, you can make it 	*/
/* smaller.  Of course, you will not be able to use the piece that you	*/
/* remove.  You can make the runtime smaller if memory space on the	*/
/* machine is very tight, or if you want to minimize the overhead	*/
/* associated with the runtime.  */

/* To reconfigure the runtime, you will need the 'C' compiler for the	*/
/* target machine.  See Appendix C for the list of which 'C' compiler	*/
/* to use for a particular machine.  Once you have selected the 	*/
/* configuration options that you want to use, recreate the runtime	*/
/* using the instructions listed in that Appendix.  */


/* You can remove the runtime source debugger by setting the following	*/
/* value to 1.  This will save 17+ Kbytes depending on your machine.	*/
/* One idea is to leave the debugger in the version of the runtime you	*/
/* develop with, but to remove it from the runtime you send to your	*/
/* users.  This way, you can still take advantage of the debugger while	*/
/* using less space on the user's machine.  */

#define	NO_DEBUGGER	0


/* If you wish to remove the SORT/MERGE module, you can set the next	*/
/* value to 1.  This will remove about 10 Kbytes.  You will not be	*/
/* able to use the SORT, MERGE, RELEASE or RETURN verbs if you set	*/
/* this value to 1.  */

#define	NO_SORT		0


/* If you set INT_POWER to 1, then only integer exponents will be 	*/
/* supported.  This prevents you from taking roots, but it saves a fair	*/
/* amount of code in the runtime.  In particular, it removes code that	*/
/* computes natural logs and code that computes e^x.  This saves 	*/
/* almost 2 Kbytes.   */

#define	INT_POWER	0


/* If you set NO_SCRN_SECTION to 1, then the Screen Section field	*/
/* manager will be removed from the runtime system.  This will prevent	*/
/* you from using any programs that contain a Screen Section.  You will	*/
/* have to use only field-level ACCEPT and DISPLAY statements.  This	*/
/* saves about 3 Kbytes of code.  */

#define	NO_SCRN_SECTION		0


/* The rest of this file implements the configuration options.  You	*/
/* will not need to modify this.  */


#if	NO_DEBUGGER

int pgh_trace()
{
	return 0;
}

int init_debug()
{
	eprintf("Sorry, the debugger has been removed from this runtime!\n");
	eprintf("*** Runtime Aborted ***\n" );
	return 0;
}

int debugger()
{
	return 0;
}

void Aload_dbg()
{
	return;
}

#endif



#if	NO_SORT

static int sort_error()
{
	stop_runtime( 255, 1, "SORT/MERGE module removed!" );
}

int init_sort()
{
	sort_error();
}

int sort_input()
{
	return 0;
}

int sort_output()
{
	return 0;
}

int do_sort()
{
	return 0;
}

int end_sort_merge()
{
	return 0;
}

int init_merge()
{
	sort_error();
}

int prime_merge()
{
	return 0;
}

int merge_input()
{
	return 0;
}

int do_release()
{
	sort_error();
}

int do_return()
{
	sort_error();
}

#endif



#if	INT_POWER

void do_real_power()
{
	do_int_power();
}

#endif



#if	NO_SCRN_SECTION

int Ascr_display()
{
	stop_runtime( 255, 1, "Screen Section support removed!" );
}

int Ascr_accept()
{
	stop_runtime( 255, 1, "Screen Section support removed!" );
}

#endif

/* that's all! */
/*
**	History:
**	$Log: config85.c,v $
**	Revision 1.3  1996-09-24 20:49:55-04  gsl
**	Change conflicting copyright vars
**
**	Revision 1.2  1996-09-17 16:45:27-07  gsl
**	drcs update
**
**
**
*/
