/* sub85rm.c - RM/COBOL-85 compatible 'C' routine interface */

/************************************************************************/
/* Copyright (c) 1995-2008 by Acucorp, Inc.  All rights reserved.	*/
/* Users of the ACUCOBOL-GT runtime may freely modify and distribute	*/
/* this file as they see fit in order to support an ACUCOBOL-GT based	*/
/* application. 							*/

/* THIS FILE IS #INCLUDED FROM sub.c.  BECAUSE SYSTEM HEADER FILES	*/
/* SHOULD BE INCLUDED BEFORE sub.h, AND BECAUSE THIS FILE IS INCLUDED	*/
/* AFTER sub.h, YOU REALLY SHOULDN'T INCLUDE ANY SYSTEM HEADER FILES    */
/* FROM THIS FILE.							*/

/* The following LIBTABLE should be modified to contain the names and	*/
/* function addresses of 'C' routines you wish to link into the runtime */
/* system.  This table is searched for each CALL statement to see if a	*/
/* matching routine name is found.  If so, then the corresponding	*/
/* 'C' function is called.  Note that the table must be terminated by   */
/* NULL pointers and that the routine names should be all upper case.	*/

/* Each 'C' routine receives 6 parameters:                              */
/* 1) a pointer to the name that was used to call the routine.		*/
/* 2) the number of USING arguments in the CALL statement.		*/
/* 3) a pointer to an array of ARGUMENT_ENTRY structures (see <sub.h>). */
/*    Each array element describes one USING argument. There is a	*/
/*    hidden argument entry, ARGUMENT_ENTRY[-1] that is either the	*/
/*    GIVING argument or an entry of the OMITTED (32).			*/
/* 4) initial entry flag: 1 if this is the first call, or has been	*/
/*    CANCELLED since the last CALL; otherwise 0.			*/
/* 5) a pointer to the RM_CALLBACK_TABLE (t.b.d.)			*/
/* 6) the handle of the main window, or 0 if not available.		*/

/************************************************************************/
/*    Definition File: <../lib/rtcallbk.h>                              */
/************************************************************************/

#ifdef	ACU_SOURCE_FILENAME
#undef	ACU_SOURCE_FILENAME
#endif	/* ACU_SOURCE_FILENAME */
#define	ACU_SOURCE_FILENAME	"lib/sub85rm.c"
const char what_lib_sub85rm_c_str[] = "@(#) " ACU_SOURCE_FILENAME " $Date: 2013-01-22 14:23:00 +0000 (Tue, 22 Jan 2013) $$Rev: 63265 $";

/************************************************************************/
/* RM Library Functions (table at bottom)				*/
/************************************************************************/

#define ArgGiving	(-1)

/*
 *  C$SHOWARGS
 *
 *  Print list of function arguments.  This routine can be
 *  be called directly, or it can be called at the beginning
 *  of another non-COBOL subprogram to print the program name
 *  and list of arguments for debugging purposes.
 *
 *  Calling sequence:
 *
 *	CALL "C$SHOWARGS" USING <arguments>.
 *
 *  Output:
 *
 *	Directed to the trace output by default, which is the debug
 *	log window unless -e is specified to direct trace output
 *	to a file.
 */

static char *szTypes[] = { "NSE ", "NSU ", "NTS ", "NTC ",
			   "NLS ", "NLC ", "NCS ", "NCU ",
			   "NPP ", "NPS ", "NPU ", "NBS ",
			   "NBU ", "bad ", "bad ", "bad ",
			   "ANS ", "ANS ", "ABS ", "ABS ",
			   "ANSE", "ABSE", "GRP ", "GRPV",
			   "bad ", "PTR ", "NBSN", "NBUN",
			   "bad ", "bad ", "bad ", "bad " };

static int
RM_ShowArgs(char *Name, int ArgCount, Argument Arguments[], int InitialStateIndicator,
	    RM_HWND hCobolWindow, RUNTIME_CALLS_TABLE *pRtCall)
{
    int i;
    FILE	*pTraceFile =
#if defined(_WINDOWS) || defined(_WIN64)    /* can't printf() on Windows--no stdout */
#if defined(_MSC_VER) && _MSC_VER >= 1400
				NULL;
				fopen_s(&pTraceFile, "showargs.log", "a+t");
#else
				fopen("showargs.log", "a+t");
#endif
#else
				stdout;
#endif

    (void)hCobolWindow;     /* unused */
    (void)pRtCall;	    /* unused */

    fprintf(pTraceFile, "Name=\"%s\", Args=%2d, Initial=%1d\n", Name, ArgCount, InitialStateIndicator);
    fflush(pTraceFile);

    for ( i = ArgGiving; i < ArgCount; ++i )
    {
	if ( i == ArgGiving )
	    fprintf(pTraceFile, "  Giving ");
	else
	    fprintf(pTraceFile, "  Arg #%2d", (i + 1));

	if ( Arguments[i].a_type == RM_OMITTED )
	    fprintf(pTraceFile, " OMITTED\n");
	else
	    fprintf(pTraceFile, " Ptr=%08lX Size=%5lu Type=%2d %s Digits=%3d Scale=%3d\n",
		    (unsigned long) Arguments[i].a_address,
		    (unsigned long) Arguments[i].a_length,
		    (int)	    Arguments[i].a_type,
		    (char *)	    ((Arguments[i].a_type < (short)(sizeof(szTypes)/sizeof(szTypes[0])))
				    ? szTypes[Arguments[i].a_type]
				    : "??? "),
		    (int)	    (char) Arguments[i].a_digits,
		    (int)	    (char) Arguments[i].a_scale);

	fflush(pTraceFile);
    }

#if defined(_WINDOWS) || defined(_WIN64)
    fclose(pTraceFile);
#endif
    return RM_FND;
}

/************************************************************************/
/* RM Library Function table (add new entries before the NULL entry)	*/
/************************************************************************/

struct	PROCTABLE LIBTABLE_RM[] =
{
	{"C$SHOWARGS", (pfSub85Intf) RM_ShowArgs, NULL},
	{ NULL, NULL, NULL }
};

