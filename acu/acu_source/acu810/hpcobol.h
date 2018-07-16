/* hpcobol.h - HP-COBOL instrinsic C interface.  This is to be included     */
/* in direct.c which is the direct interface between ACUCOBOL and 'C'.      */
/* Your operating system must have a library that can resolve the HP-COBOL  */
/* intrinsics.  On an HP e3000 these intrinsics live in the default library */
/* called XL.PUB.SYS.  On other operating systems you need to install and   */
/* link with an HP-COBOL intrinsic compatible library.  If there is an      */
/* intrinsic that you need that is not listed you can easily add it in the  */
/* same manner as the other intrinsics.  Note that there are two locations  */
/* in this file that need to be updated for each intrinsic that is added.   */
/* If any function passes any of its parameters by value then BY VALUE must */
/* be specified for those parameter in the COBOL CALL statement.            */

/* HP intrinsics */
#define	IMAGE	1
#define KSAM	0
#define	VPLUS	0
#define	SYSTEM_INTRINSICS 0

/* intrinsics added as of MPE/iX 6.5 */
#define MPE65	0

/* third party intrinsics */
#define	IMAGE_PLUS 0

/* WINGSPAN interface */
#define	WINGSPAN 1

#ifdef	WINGSPAN
short A_call_err;
extern short *Astdlib_A_call_err();
#endif	/* WINGSPAN */

/* This defines all the HP-COBOL procedure calls that will be used in      */
/* your application program.  The HP-COBOL procedures are declared         */
/* for C as external procedures.  The procedure name is identified by the  */
/* word "intrinsic"                                                        */

#if	IMAGE
#pragma intrinsic dbbegin
#pragma intrinsic dbclose
#pragma intrinsic dbcontrol
#pragma intrinsic dbdelete
#pragma intrinsic dbend
#pragma intrinsic dberror
#pragma intrinsic dbexplain
#pragma intrinsic dbfind
#pragma intrinsic dbget
#pragma intrinsic dbinfo
#pragma intrinsic dblock
#pragma intrinsic dbmemo
#pragma intrinsic dbopen
#pragma intrinsic dbput
#pragma intrinsic dbunlock
#pragma intrinsic dbupdate
#pragma intrinsic dbxbegin
#pragma intrinsic dbxend
#pragma intrinsic dbxundo
#endif	/* IMAGE */

#if	KSAM
#pragma intrinsic ckclose
#pragma intrinsic ckdelete
#pragma intrinsic ckerror
#pragma intrinsic cklock
#pragma intrinsic ckopen
#pragma intrinsic ckopenshr
#pragma intrinsic ckread
#pragma intrinsic ckreadbykey
#pragma intrinsic ckrewrite
#pragma intrinsic ckstart
#pragma intrinsic ckunlock
#pragma intrinsic ckwrite
#endif	/* KSAM */

#if	VPLUS
#pragma intrinsic varmscp
#pragma intrinsic vblockread
#pragma intrinsic vblockwrite
#pragma intrinsic vchangefield
#pragma intrinsic vclosebatch
#pragma intrinsic vcloseformf
#pragma intrinsic vcloseterm
#pragma intrinsic verrmsg
#pragma intrinsic vfieldedits
#pragma intrinsic vfinishform
#pragma intrinsic vgetarbinfo
#pragma intrinsic vgetbuffer
#pragma intrinsic vgetdint
#pragma intrinsic vgetfield
#pragma intrinsic vgetfieldinfo
#pragma intrinsic vgetfileinfo
#pragma intrinsic vgetforminfo
#pragma intrinsic vgetint
#pragma intrinsic vgetkeylabels
#pragma intrinsic vgetlang
#pragma intrinsic vgetlong
#pragma intrinsic vgetnextform
#if	MPE65
#pragma intrinsic vgetsavefield
#endif	/* MPE65 */
#pragma intrinsic vgetpacked
#pragma intrinsic vgetreal
#pragma intrinsic vgetscpdata
#pragma intrinsic vgetscpfield
#if	MPE65
#pragma intrinsic vgetyyyymmdd
#endif	/* MPE65 */
#pragma intrinsic vgetzoned
#pragma intrinsic vinitform
#pragma intrinsic vloadforms
#if	MPE65
#pragma intrinsic vmerge
#endif	/* MPE65 */
#pragma intrinsic vopenbatch
#pragma intrinsic vopenformf
#pragma intrinsic vopenterm
#pragma intrinsic vplacecursor
#pragma intrinsic vpostbatch
#pragma intrinsic vprintform
#pragma intrinsic vprintscreen
#pragma intrinsic vputbuffer
#pragma intrinsic vputdint
#pragma intrinsic vputfield
#if	MPE65
#pragma intrinsic vputsavefield
#endif	/* MPE65 */
#pragma intrinsic vputint
#pragma intrinsic vputlong
#pragma intrinsic vputpacked
#pragma intrinsic vputreal
#pragma intrinsic vputwindow
#if	MPE65
#pragma intrinsic vputyyyymmdd
#endif	/* MPE65 */
#pragma intrinsic vputzoned
#pragma intrinsic vreadbatch
#pragma intrinsic vreadfields
#pragma intrinsic vseterror
#pragma intrinsic vsetkeylabel
#pragma intrinsic vsetkeylabels
#pragma intrinsic vsetlang
#pragma intrinsic vshowform
#pragma intrinsic vturnoff
#pragma intrinsic vturnon
#pragma intrinsic vunloadform
#pragma intrinsic vwritebatch
#endif	/* VPLUS */

#if	IMAGE_PLUS
#pragma intrinsic dbibegin
#pragma intrinsic dbiclose
#pragma intrinsic dbicontrol
#pragma intrinsic dbidelete
#pragma intrinsic dbiend
#pragma intrinsic dbierror
#pragma intrinsic dbiexplain
#pragma intrinsic dbifind
#pragma intrinsic dbiget
#pragma intrinsic dbiinfo
#pragma intrinsic dbilock
#pragma intrinsic dbimemo
#pragma intrinsic dbiopen
#pragma intrinsic dbiput
#pragma intrinsic dbiunlock
#pragma intrinsic dbiupdate
#pragma intrinsic dbixbegin
#pragma intrinsic dbixend
#pragma intrinsic dbixundo
#endif	/* IMAGE_PLUS */

#if	SYSTEM_INTRINSICS
#pragma intrinsic ABORTSESS
#pragma intrinsic ACTIVATE
#pragma intrinsic ADJUSTUSLF
#pragma intrinsic ALMANAC
#pragma intrinsic ALTDSEG
#pragma intrinsic ARITRAP
#pragma intrinsic ASCII
#pragma intrinsic BEGINLOG
#pragma intrinsic BINARY
#pragma intrinsic CALENDAR
#pragma intrinsic CATCLOSE
#pragma intrinsic CATOPEN
#pragma intrinsic CATREAD
#pragma intrinsic CAUSEBREAK
#pragma intrinsic CLEANUSL
#pragma intrinsic CLOCK
#pragma intrinsic CLOSELOG
#pragma intrinsic COMMAND
#pragma intrinsic CREATE
#pragma intrinsic CREATEPROCESS
#pragma intrinsic CTRANSLATE
#pragma intrinsic DASCII
#pragma intrinsic DATELINE
#pragma intrinsic DBINARY
#pragma intrinsic DEBUG
#pragma intrinsic DLSIZE
#pragma intrinsic DMOVIN
#pragma intrinsic DMOVOUT
#pragma intrinsic ENDLOG
#pragma intrinsic EXPANDUSLF
#pragma intrinsic FATHER
#pragma intrinsic FCHECK
#pragma intrinsic FCONTROL
#pragma intrinsic FDELETE
#pragma intrinsic FDEVICECONTROL
#pragma intrinsic FERRMSG
#pragma intrinsic FFILEINFO
#pragma intrinsic FFINDBYKEY
#pragma intrinsic FFINDN
#pragma intrinsic FGETINFO
#pragma intrinsic FGETKEYINFO
#pragma intrinsic FINDJCW
#pragma intrinsic FINTEXIT
#pragma intrinsic FINTSTATE
#pragma intrinsic FLOCK
#pragma intrinsic FLUSHLOG
#pragma intrinsic FMTCALENDAR
#pragma intrinsic FMTCLOCK
#pragma intrinsic FMTDATE
#pragma intrinsic FPARSE
#pragma intrinsic FPOINT
#pragma intrinsic FREADBACKWARD
#pragma intrinsic FREADBYKEY
#pragma intrinsic FREADC
#pragma intrinsic FREADDIR
#pragma intrinsic FREADLABEL
#pragma intrinsic FREADSEEK
#pragma intrinsic FREEDSEG
#pragma intrinsic FREELOCRIN
#pragma intrinsic FRELATE
#pragma intrinsic FREMOVE
#pragma intrinsic FRENAME
#pragma intrinsic FSETMODE
#pragma intrinsic FSPACE
#pragma intrinsic FUNLOCK
#pragma intrinsic FUPDATE
#pragma intrinsic FWRITEDIR
#pragma intrinsic FWRITELABEL
#pragma intrinsic GENMESSAGE
#pragma intrinsic GETDSEG
#pragma intrinsic GETINFO
#pragma intrinsic GETJCW
#pragma intrinsic GETLOCRIN
#pragma intrinsic GETORIGIN
#pragma intrinsic GETPRIORITY
#pragma intrinsic GETPRIVMODE
#pragma intrinsic GETPROCID
#pragma intrinsic GETPROCINFO
#pragma intrinsic GETUSERMODE
#pragma intrinsic HP32208
#pragma intrinsic HPACDINFO
#pragma intrinsic HPACDPUT
#pragma intrinsic HPCALENDAR
#pragma intrinsic HPCICOMMAND
#pragma intrinsic HPCIDELETEVAR
#pragma intrinsic HPCIGETVAR
#pragma intrinsic HPCIPUTVAR
#pragma intrinsic HPDATECONVERT
#pragma intrinsic HPDATEDIFF
#pragma intrinsic HPDATEFORMAT
#pragma intrinsic HPDATEOFFSET
#pragma intrinsic HPDATEVALIDATE
#pragma intrinsic HPDEBUG
#pragma intrinsic HPDEVCONTROL
#pragma intrinsic HPDEVCREATE
#pragma intrinsic HPENBLTRAP
#pragma intrinsic HPERRDEPTH
#pragma intrinsic HPERRMSG
#pragma intrinsic HPERRREAD
#pragma intrinsic hpextin
#if	MPE65
#pragma intrinsic HPFADDTOPOINTER
#pragma intrinsic HPFFILLDATA
#pragma intrinsic HPFMOVEDATA
#pragma intrinsic HPFMOVEDATALTOR
#pragma intrinsic HPFMOVEDATARTOL
#endif	/* MPE65 */
#pragma intrinsic HPFIRSTLIBRARY
#pragma intrinsic HPFMTCALENDAR
#pragma intrinsic HPFOPEN
#pragma intrinsic HPFPCONVERT
#pragma intrinsic HPGETPROCPLABEL
#pragma intrinsic HPLOADCMPROCEDURE
#if	MPE65
#pragma intrinsic HPLOADNMPROC
#endif	/* MPE65 */
#pragma intrinsic HPMERGEEND
#pragma intrinsic HPMERGEERRORMESS
#pragma intrinsic HPMERGEINIT
#pragma intrinsic HPMERGEOUTPUT
#pragma intrinsic HPMERGESTAT
#pragma intrinsic HPMERGETITLE
#pragma intrinsic HPMYFILE
#pragma intrinsic HPMYPROGRAM
#pragma intrinsic HPPIPE
#pragma intrinsic HPRESETDUMP
#pragma intrinsic HPSELECT
#pragma intrinsic HPSETCCODE
#pragma intrinsic HPSETDUMP
#pragma intrinsic HPSORTEND
#pragma intrinsic HPSORTERRORMESS
#pragma intrinsic HPSORTINIT
#pragma intrinsic HPSORTINPUT
#pragma intrinsic HPSORTOUTPUT
#pragma intrinsic HPSORTSTAT
#pragma intrinsic HPSORTTITLE
#pragma intrinsic HPSWITCHTOCM
#if	MPE65
#pragma intrinsic HPSWTONMNAME
#pragma intrinsic HPSWTONMPLABEL
#endif	/* MPE65 */
#pragma intrinsic HPUNLOADCMPROCEDURE
#pragma intrinsic HPVOLINFO
#pragma intrinsic INITUSLF
#pragma intrinsic IODONTWAIT
#pragma intrinsic IOWAIT
#pragma intrinsic JOBINFO
#pragma intrinsic KILL
#pragma intrinsic LOADPROC
#pragma intrinsic LOCKGLORIN
#pragma intrinsic LOCKLOCRIN
#pragma intrinsic LOCRINOWNER
#pragma intrinsic LOGINFO
#pragma intrinsic LOGSTATUS
#pragma intrinsic MAIL
#pragma intrinsic MERGEEND
#pragma intrinsic MERGEERRORMESS
#pragma intrinsic MERGEINIT
#pragma intrinsic MERGEOUTPUT
#pragma intrinsic MERGESTAT
#pragma intrinsic MERGETITLE
#pragma intrinsic MYCOMMAND
#pragma intrinsic NLAPPEND
#pragma intrinsic NLCOLLATE
#pragma intrinsic NLCOLLATE2
#pragma intrinsic NLCONVCLOCK
#pragma intrinsic NLCONVCUSTDATE
#pragma intrinsic NLCONVNUM
#pragma intrinsic NLFINDSTR
#pragma intrinsic NLFMTCALENDAR
#pragma intrinsic NLFMTCLOCK
#pragma intrinsic NLFMTCUSTDATE
#pragma intrinsic NLFMTDATE
#pragma intrinsic NLFMTLONGCAL
#pragma intrinsic NLFMTNUM
#pragma intrinsic NLGETLANG
#pragma intrinsic NLINFO
#pragma intrinsic NLJUDGE
#pragma intrinsic NLKEYCOMPARE
#pragma intrinsic NLMATCH
#pragma intrinsic NLMATCHINIT
#pragma intrinsic NLNUMSPEC
#pragma intrinsic NLREPCHAR
#pragma intrinsic NLSCANMOVE
#pragma intrinsic NLSUBSTR
#pragma intrinsic NLSWITCHBUF
#pragma intrinsic NLTRANSLATE
#pragma intrinsic OPENLOG
#pragma intrinsic PAUSE
#pragma intrinsic PRINT
#pragma intrinsic PRINTFILEINFO
#pragma intrinsic PRINTOP
#pragma intrinsic PRINTOPREPLY
#pragma intrinsic PROCINFO
#pragma intrinsic PROCTIME
#pragma intrinsic PUTJCW
#pragma intrinsic QUIT
#pragma intrinsic QUITPROG
#pragma intrinsic READ
#pragma intrinsic READX
#pragma intrinsic RECEIVEMAIL
#pragma intrinsic RESETCONTROL
#pragma intrinsic RESETDUMP
#pragma intrinsic SEARCH
#pragma intrinsic SENDMAIL
#pragma intrinsic SETDUMP
#pragma intrinsic SETJCW
#pragma intrinsic SORTEND
#pragma intrinsic SORTERRORMESS
#pragma intrinsic SORTINIT
#pragma intrinsic SORTINPUT
#pragma intrinsic SORTOUTPUT
#pragma intrinsic SORTSTAT
#pragma intrinsic SORTTITLE
#pragma intrinsic STACKDUMP
#pragma intrinsic STARTSESS
#pragma intrinsic SUSPEND
#if	MPE65
#pragma intrinsic SWITCHDB
#endif	/* MPE65 */
#pragma intrinsic TERMINATE
#pragma intrinsic TIMER
#pragma intrinsic UNLOADPROC
#pragma intrinsic UNLOCKGLORIN
#pragma intrinsic UNLOCKLOCRIN
#pragma intrinsic WHO
#pragma intrinsic WRITELOG
#pragma intrinsic XARITRAP
#pragma intrinsic XCONTRAP
#pragma intrinsic XLIBTRAP
#pragma intrinsic XSYSTRAP
#pragma intrinsic ZSIZE
#endif	/* SYSTEM_INTRINSICS */

/* You can replace the LIBDIRECT structure in direct.c with the following  */
/* structure.  This defines the names of 'C' functions that you can call   */
/* directly from COBOL without any intervening handling.  In the first     */
/* column, is the name you use in the COBOL CALL statement.  In the second */
/* column, is the address of the routine to be called.  In the third       */
/* column, is the type of the function (these are declared in "sub.h").    */
/* Note that the CALL name is all uppercase.                               */
struct  DIRECTTABLE LIBDIRECT[] = {
#if	IMAGE
        { "DBBEGIN",            FUNC dbbegin,   	C_void },
        { "DBCLOSE",            FUNC dbclose,   	C_void },
        { "DBCONTROL",          FUNC dbcontrol, 	C_void },
        { "DBDELETE",           FUNC dbdelete,  	C_void },
        { "DBEND",              FUNC dbend,     	C_void },
        { "DBERROR",            FUNC dberror,   	C_void },
        { "DBEXPLAIN",          FUNC dbexplain, 	C_void },
        { "DBFIND",             FUNC dbfind,    	C_void },
        { "DBGET",              FUNC dbget,     	C_void },
        { "DBINFO",             FUNC dbinfo,    	C_void },
        { "DBLOCK",             FUNC dblock,    	C_void },
        { "DBMEMO",             FUNC dbmemo,    	C_void },
        { "DBOPEN",             FUNC dbopen,    	C_void },
        { "DBPUT",              FUNC dbput,     	C_void },
        { "DBUNLOCK",           FUNC dbunlock,  	C_void },
        { "DBUPDATE",           FUNC dbupdate,  	C_void },
        { "DBXBEGIN",           FUNC dbxbegin,  	C_void },
        { "DBXEND",             FUNC dbxend,    	C_void },
        { "DBXUNDO",            FUNC dbxundo,   	C_void },
#endif	/* IMAGE */
#if	KSAM
	{ "CKCLOSE",		FUNC ckclose,		C_void },
	{ "CKDELETE",		FUNC ckdelete,		C_void },
	{ "CKERROR",		FUNC ckerror,		C_void },
	{ "CKLOCK",		FUNC cklock,		C_void },
	{ "CKOPEN",		FUNC ckopen,		C_void },
	{ "CKOPENSHR",		FUNC ckopenshr,		C_void },
	{ "CKREAD",		FUNC ckread,		C_void },
	{ "CKREADBYKEY",	FUNC ckreadbykey,	C_void },
	{ "CKREWRITE",		FUNC ckrewrite,		C_void },
	{ "CKSTART",		FUNC ckstart,		C_void },
	{ "CKUNLOCK",		FUNC ckunlock,		C_void },
	{ "CKWRITE",		FUNC ckwrite,		C_void },
#endif	/* KSAM */
#if	VPLUS
	{ "VARMSCP",		FUNC varmscp,		C_void },
	{ "VBLOCKREAD",		FUNC vblockread,	C_void },
	{ "VBLOCKWRITE",	FUNC vblockwrite,	C_void },
	{ "VCHANGEFIELD",	FUNC vchangefield,	C_void },
	{ "VCLOSEBATCH",	FUNC vclosebatch,	C_void },
	{ "VCLOSEFORMF",	FUNC vclosebatch,	C_void },
	{ "VCLOSETERM",		FUNC vcloseterm,	C_void },
	{ "VERRMSG",		FUNC vcloseterm,	C_void },
	{ "VFIELDEDITS",	FUNC vfieldedits,	C_void },
	{ "VFINISHFORM",	FUNC vfinishform,	C_void },
	{ "VGETARBINFO",	FUNC vgetarbinfo,	C_void },
	{ "VGETBUFFER",		FUNC vgetbuffer,	C_void },
	{ "VGETDINT",		FUNC vgetdint,		C_void },
	{ "VGETFIELD",		FUNC vgetfield,		C_void },
	{ "VGETFIELDINFO",	FUNC vgetfieldinfo,	C_void },
	{ "VGETFILEINFO",	FUNC vgetfileinfo,	C_void },
	{ "VGETFORMINFO",	FUNC vgetforminfo,	C_void },
	{ "VGETINT",		FUNC vgetint,		C_void },
	{ "VGETKEYLABELS",	FUNC vgetkeylabels,	C_void },
	{ "VGETLANG",		FUNC vgetlang,		C_void },
	{ "VGETLONG",		FUNC vgetlong,		C_void },
	{ "VGETNEXTFORM",	FUNC vgetnextform,	C_void },
#if	MPE65
	{ "VGETSAVEFIELD",	FUNC vgetsavefield,	C_void },
#endif	/* MPE65 */
	{ "VGETPACKED",		FUNC vgetpacked,	C_void },
	{ "VGETREAL",		FUNC vgetreal,		C_void },
	{ "VGETSCPDATA",	FUNC vgetscpdata,	C_void },
	{ "VGETSCPFIELD",	FUNC vgetscpfield,	C_void },
#if	MPE65
	{ "VGETYYYYMMDD",	FUNC vgetyyyymmdd,	C_void },
#endif	/* MPE65 */
	{ "VGETZONED",		FUNC vgetzoned,		C_void },
	{ "VINITFORM",		FUNC vinitform,		C_void },
	{ "VLOADFORMS",		FUNC vloadforms,	C_void },
#if	MPE65
	{ "VMERGE",		FUNC vmerge,		C_void },
#endif	/* MPE65 */
	{ "VOPENBATCH",		FUNC vopenbatch,	C_void },
	{ "VOPENFORMF",		FUNC vopenformf,	C_void },
	{ "VOPENTERM",		FUNC vopenterm,		C_void },
	{ "VPLACECURSOR",	FUNC vplacecursor,	C_void },
	{ "VPOSTBATCH",		FUNC vpostbatch,	C_void },
	{ "VPRINTFORM",		FUNC vprintform,	C_void },
	{ "VPRINTSCREEN",	FUNC vprintscreen,	C_void },
	{ "VPUTBUFFER",		FUNC vputbuffer,	C_void },
	{ "VPUTDINT",		FUNC vputdint,		C_void },
	{ "VPUTFIELD",		FUNC vputfield,		C_void },
#if	MPE65
	{ "VPUTSAVEFIELD",	FUNC vputsavefield,	C_void },
#endif	/* MPE65 */
	{ "VPUTINT",		FUNC vputint,		C_void },
	{ "VPUTLONG",		FUNC vputlong,		C_void },
	{ "VPUTPACKED",		FUNC vputpacked,	C_void },
	{ "VPUTREAL",		FUNC vputreal,		C_void },
	{ "VPUTWINDOW",		FUNC vputwindow,	C_void },
#if	MPE65
	{ "VPUTYYYYMMDD",	FUNC vputyyyymmdd,	C_void },
#endif	/* MPE65 */
	{ "VPUTZONED",		FUNC vputzoned,		C_void },
	{ "VREADBATCH",		FUNC vreadbatch,	C_void },
	{ "VREADFIELDS",	FUNC vreadfields,	C_void },
	{ "VSETERROR",		FUNC vseterror,		C_void },
	{ "VSETKEYLABEL",	FUNC vsetkeylabel,	C_void },
	{ "VSETKEYLABELS",	FUNC vsetkeylabels,	C_void },
	{ "VSETLANG",		FUNC vsetlang,		C_void },
	{ "VSHOWFORM",		FUNC vshowform,		C_void },
	{ "VTURNOFF",		FUNC vturnoff,		C_void },
	{ "VTURNON",		FUNC vturnon,		C_void },
	{ "VUNLOADFORM",	FUNC vunloadform,	C_void },
	{ "VWRITEBATCH",	FUNC vwritebatch,	C_void },
#endif	/* VPLUS */
#if	IMAGE_PLUS
        { "DBIBEGIN",           FUNC dbibegin,   	C_void },
        { "DBICLOSE",           FUNC dbiclose,   	C_void },
        { "DBICONTROL",         FUNC dbicontrol, 	C_void },
        { "DBIDELETE",          FUNC dbidelete,  	C_void },
        { "DBIEND",             FUNC dbiend,     	C_void },
        { "DBIERROR",           FUNC dbierror,   	C_void },
        { "DBIEXPLAIN",         FUNC dbiexplain, 	C_void },
        { "DBIFIND",            FUNC dbifind,    	C_void },
        { "DBIGET",             FUNC dbiget,     	C_void },
        { "DBIINFO",            FUNC dbiinfo,    	C_void },
        { "DBILOCK",            FUNC dbilock,    	C_void },
        { "DBIMEMO",            FUNC dbimemo,    	C_void },
        { "DBIOPEN",            FUNC dbiopen,    	C_void },
        { "DBIPUT",             FUNC dbiput,     	C_void },
        { "DBIUNLOCK",          FUNC dbiunlock,  	C_void },
        { "DBIUPDATE",          FUNC dbiupdate,  	C_void },
        { "DBIXBEGIN",          FUNC dbixbegin,  	C_void },
        { "DBIXEND",            FUNC dbixend,    	C_void },
        { "DBIXUNDO",           FUNC dbixundo,   	C_void },
#endif	/* IMAGE_PLUS */
#if	SYSTEM_INTRINSICS
	{ "ABORTSESS",          FUNC ABORTSESS,         C_void },
	{ "ACTIVATE",           FUNC ACTIVATE,          C_void },
	{ "ADJUSTUSLF",         FUNC ADJUSTUSLF,        C_void },
	{ "ALMANAC",            FUNC ALMANAC,           C_void },
	{ "ALTDSEG",            FUNC ALTDSEG,           C_void },
	{ "ARITRAP",            FUNC ARITRAP,           C_void },
	{ "ASCII",              FUNC ASCII,             C_void },
	{ "BEGINLOG",           FUNC BEGINLOG,          C_void },
	{ "BINARY",             FUNC BINARY,            C_void },
	{ "CALENDAR",           FUNC CALENDAR,          C_void },
	{ "CATCLOSE",           FUNC CATCLOSE,          C_void },
	{ "CATOPEN",            FUNC CATOPEN,           C_void },
	{ "CATREAD",            FUNC CATREAD,           C_void },
	{ "CAUSEBREAK",         FUNC CAUSEBREAK,        C_void },
	{ "CLEANUSL",           FUNC CLEANUSL,          C_void },
	{ "CLOCK",              FUNC CLOCK,             C_void },
	{ "CLOSELOG",           FUNC CLOSELOG,          C_void },
	{ "COMMAND",            FUNC COMMAND,           C_void },
	{ "CREATE",             FUNC CREATE,            C_void },
	{ "CREATEPROCESS",      FUNC CREATEPROCESS,     C_void },
	{ "CTRANSLATE",         FUNC CTRANSLATE,        C_void },
	{ "DASCII",             FUNC DASCII,            C_void },
	{ "DATELINE",           FUNC DATELINE,          C_void },
	{ "DBINARY",            FUNC DBINARY,           C_void },
	{ "DEBUG",              FUNC DEBUG,             C_void },
	{ "DLSIZE",             FUNC DLSIZE,            C_void },
	{ "DMOVIN",             FUNC DMOVIN,            C_void },
	{ "DMOVOUT",            FUNC DMOVOUT,           C_void },
	{ "ENDLOG",             FUNC ENDLOG,            C_void },
	{ "EXPANDUSLF",         FUNC EXPANDUSLF,        C_void },
	{ "FATHER",             FUNC FATHER,            C_void },
	{ "FCHECK",             FUNC FCHECK,            C_void },
	{ "FCONTROL",           FUNC FCONTROL,          C_void },
	{ "FDELETE",            FUNC FDELETE,           C_void },
	{ "FDEVICECONTROL",     FUNC FDEVICECONTROL,    C_void },
	{ "FERRMSG",            FUNC FERRMSG,           C_void },
	{ "FFILEINFO",          FUNC FFILEINFO,         C_void },
	{ "FFINDBYKEY",         FUNC FFINDBYKEY,        C_void },
	{ "FFINDN",             FUNC FFINDN,            C_void },
	{ "FGETINFO",           FUNC FGETINFO,          C_void },
	{ "FGETKEYINFO",        FUNC FGETKEYINFO,       C_void },
	{ "FINDJCW",            FUNC FINDJCW,           C_void },
	{ "FINTEXIT",           FUNC FINTEXIT,          C_void },
	{ "FINTSTATE",          FUNC FINTSTATE,         C_void },
	{ "FLOCK",              FUNC FLOCK,             C_void },
	{ "FLUSHLOG",           FUNC FLUSHLOG,          C_void },
	{ "FMTCALENDAR",        FUNC FMTCALENDAR,       C_void },
	{ "FMTCLOCK",           FUNC FMTCLOCK,          C_void },
	{ "FMTDATE",            FUNC FMTDATE,           C_void },
	{ "FPARSE",             FUNC FPARSE,            C_void },
	{ "FPOINT",             FUNC FPOINT,            C_void },
	{ "FREADBACKWARD",      FUNC FREADBACKWARD,     C_void },
	{ "FREADBYKEY",         FUNC FREADBYKEY,        C_void },
	{ "FREADC",             FUNC FREADC,            C_void },
	{ "FREADDIR",           FUNC FREADDIR,          C_void },
	{ "FREADLABEL",         FUNC FREADLABEL,        C_void },
	{ "FREADSEEK",          FUNC FREADSEEK,         C_void },
	{ "FREEDSEG",           FUNC FREEDSEG,          C_void },
	{ "FREELOCRIN",         FUNC FREELOCRIN,        C_void },
	{ "FRELATE",            FUNC FRELATE,           C_void },
	{ "FREMOVE",            FUNC FREMOVE,           C_void },
	{ "FRENAME",            FUNC FRENAME,           C_void },
	{ "FSETMODE",           FUNC FSETMODE,          C_void },
	{ "FSPACE",             FUNC FSPACE,            C_void },
	{ "FUNLOCK",            FUNC FUNLOCK,           C_void },
	{ "FUPDATE",            FUNC FUPDATE,           C_void },
	{ "FWRITEDIR",          FUNC FWRITEDIR,         C_void },
	{ "FWRITELABEL",        FUNC FWRITELABEL,       C_void },
	{ "GENMESSAGE",         FUNC GENMESSAGE,        C_void },
	{ "GETDSEG",            FUNC GETDSEG,           C_void },
	{ "GETINFO",            FUNC GETINFO,           C_void },
	{ "GETJCW",             FUNC GETJCW,            C_void },
	{ "GETLOCRIN",          FUNC GETLOCRIN,         C_void },
	{ "GETORIGIN",          FUNC GETORIGIN,         C_void },
	{ "GETPRIORITY",        FUNC GETPRIORITY,       C_void },
	{ "GETPRIVMODE",        FUNC GETPRIVMODE,       C_void },
	{ "GETPROCID",          FUNC GETPROCID,         C_void },
	{ "GETPROCINFO",        FUNC GETPROCINFO,       C_void },
	{ "GETUSERMODE",        FUNC GETUSERMODE,       C_void },
	{ "HP32208",            FUNC HP32208,           C_void },
	{ "HPACDINFO",          FUNC HPACDINFO,         C_void },
	{ "HPACDPUT",           FUNC HPACDPUT,          C_void },
	{ "HPCALENDAR",         FUNC HPCALENDAR,        C_void },
	{ "HPCICOMMAND",        FUNC HPCICOMMAND,       C_void },
	{ "HPCIDELETEVAR",      FUNC HPCIDELETEVAR,     C_void },
	{ "HPCIGETVAR",         FUNC HPCIGETVAR,        C_void },
	{ "HPCIPUTVAR",         FUNC HPCIPUTVAR,        C_void },
	{ "HPDATECONVERT",      FUNC HPDATECONVERT,     C_void },
	{ "HPDATEDIFF",         FUNC HPDATEDIFF,        C_void },
	{ "HPDATEFORMAT",       FUNC HPDATEFORMAT,      C_void },
	{ "HPDATEOFFSET",       FUNC HPDATEOFFSET,      C_void },
	{ "HPDATEVALIDATE",     FUNC HPDATEVALIDATE,    C_void },
	{ "HPDEBUG",            FUNC HPDEBUG,           C_void },
	{ "HPDEVCONTROL",       FUNC HPDEVCONTROL,      C_void },
	{ "HPDEVCREATE",        FUNC HPDEVCREATE,       C_void },
	{ "HPENBLTRAP",         FUNC HPENBLTRAP,        C_void },
	{ "HPERRDEPTH",         FUNC HPERRDEPTH,        C_void },
	{ "HPERRMSG",           FUNC HPERRMSG,          C_void },
	{ "HPERRREAD",          FUNC HPERRREAD,         C_void },
	{ "HPEXTIN",		FUNC hpextin,	        C_void },
#if	MPE65
	{ "HPFADDTOPOINTER",    FUNC HPFADDTOPOINTER,   C_void },
	{ "HPFFILLDATA",        FUNC HPFFILLDATA,       C_void },
	{ "HPFMOVEDATA",        FUNC HPFMOVEDATA,       C_void },
	{ "HPFMOVEDATALTOR",    FUNC HPFMOVEDATALTOR,   C_void },
	{ "HPFMOVEDATARTOL",    FUNC HPFMOVEDATARTOL,   C_void },
#endif	/* MPE65 */
	{ "HPFIRSTLIBRARY",     FUNC HPFIRSTLIBRARY,    C_void },
	{ "HPFMTCALENDAR",      FUNC HPFMTCALENDAR,     C_void },
	{ "HPFOPEN",            FUNC HPFOPEN,           C_void },
	{ "HPFPCONVERT",        FUNC HPFPCONVERT,       C_void },
	{ "HPGETPROCPLABEL",    FUNC HPGETPROCPLABEL,   C_void },
	{ "HPLOADCMPROCEDURE",  FUNC HPLOADCMPROCEDURE, C_void },
#if	MPE65
	{ "HPLOADNMPROC",       FUNC HPLOADNMPROC,      C_void },
#endif	/* MPE65 */
	{ "HPMERGEEND",         FUNC HPMERGEEND,        C_void },
	{ "HPMERGEERRORMESS",   FUNC HPMERGEERRORMESS,  C_void },
	{ "HPMERGEINIT",        FUNC HPMERGEINIT,       C_void },
	{ "HPMERGEOUTPUT",      FUNC HPMERGEOUTPUT,     C_void },
	{ "HPMERGESTAT",        FUNC HPMERGESTAT,       C_void },
	{ "HPMERGETITLE",       FUNC HPMERGETITLE,      C_void },
	{ "HPMYFILE",           FUNC HPMYFILE,          C_void },
	{ "HPMYPROGRAM",        FUNC HPMYPROGRAM,       C_void },
	{ "HPPIPE",             FUNC HPPIPE,            C_void },
	{ "HPRESETDUMP",        FUNC HPRESETDUMP,       C_void },
	{ "HPSELECT",           FUNC HPSELECT,          C_void },
	{ "HPSETCCODE",         FUNC HPSETCCODE,        C_void },
	{ "HPSETDUMP",          FUNC HPSETDUMP,         C_void },
	{ "HPSORTEND",          FUNC HPSORTEND,         C_void },
	{ "HPSORTERRORMESS",    FUNC HPSORTERRORMESS,   C_void },
	{ "HPSORTINIT",         FUNC HPSORTINIT,        C_void },
	{ "HPSORTINPUT",        FUNC HPSORTINPUT,       C_void },
	{ "HPSORTOUTPUT",       FUNC HPSORTOUTPUT,      C_void },
	{ "HPSORTSTAT",         FUNC HPSORTSTAT,        C_void },
	{ "HPSORTTITLE",        FUNC HPSORTTITLE,       C_void },
	{ "HPSWITCHTOCM",       FUNC HPSWITCHTOCM,      C_void },
#if	MPE65
	{ "HPSWTONMNAME",       FUNC HPSWTONMNAME,      C_void },
	{ "HPSWTONMPLABEL",     FUNC HPSWTONMPLABEL,    C_void },
#endif	/* MPE65 */
	{ "HPUNLOADCMPROCEDURE",FUNC HPUNLOADCMPROCEDURE,C_void },
	{ "HPVOLINFO",          FUNC HPVOLINFO,         C_void },
	{ "INITUSLF",           FUNC INITUSLF,          C_void },
	{ "IODONTWAIT",         FUNC IODONTWAIT,        C_void },
	{ "IOWAIT",             FUNC IOWAIT,            C_void },
	{ "JOBINFO",            FUNC JOBINFO,           C_void },
	{ "KILL",               FUNC KILL,              C_void },
	{ "LOADPROC",           FUNC LOADPROC,          C_void },
	{ "LOCKGLORIN",         FUNC LOCKGLORIN,        C_void },
	{ "LOCKLOCRIN",         FUNC LOCKLOCRIN,        C_void },
	{ "LOCRINOWNER",        FUNC LOCRINOWNER,       C_void },
	{ "LOGINFO",            FUNC LOGINFO,           C_void },
	{ "LOGSTATUS",          FUNC LOGSTATUS,         C_void },
	{ "MAIL",               FUNC MAIL,              C_void },
	{ "MERGEEND",           FUNC MERGEEND,          C_void },
	{ "MERGEERRORMESS",     FUNC MERGEERRORMESS,    C_void },
	{ "MERGEINIT",          FUNC MERGEINIT,         C_void },
	{ "MERGEOUTPUT",        FUNC MERGEOUTPUT,       C_void },
	{ "MERGESTAT",          FUNC MERGESTAT,         C_void },
	{ "MERGETITLE",         FUNC MERGETITLE,        C_void },
	{ "MYCOMMAND",          FUNC MYCOMMAND,         C_void },
	{ "NLAPPEND",           FUNC NLAPPEND,          C_void },
	{ "NLCOLLATE",          FUNC NLCOLLATE,         C_void },
	{ "NLCOLLATE2",         FUNC NLCOLLATE2,        C_void },
	{ "NLCONVCLOCK",        FUNC NLCONVCLOCK,       C_void },
	{ "NLCONVCUSTDATE",     FUNC NLCONVCUSTDATE,    C_void },
	{ "NLCONVNUM",          FUNC NLCONVNUM,         C_void },
	{ "NLFINDSTR",          FUNC NLFINDSTR,         C_void },
	{ "NLFMTCALENDAR",      FUNC NLFMTCALENDAR,     C_void },
	{ "NLFMTCLOCK",         FUNC NLFMTCLOCK,        C_void },
	{ "NLFMTCUSTDATE",      FUNC NLFMTCUSTDATE,     C_void },
	{ "NLFMTDATE",          FUNC NLFMTDATE,         C_void },
	{ "NLFMTLONGCAL",       FUNC NLFMTLONGCAL,      C_void },
	{ "NLFMTNUM",           FUNC NLFMTNUM,          C_void },
	{ "NLGETLANG",          FUNC NLGETLANG,         C_void },
	{ "NLINFO",             FUNC NLINFO,            C_void },
	{ "NLJUDGE",            FUNC NLJUDGE,           C_void },
	{ "NLKEYCOMPARE",       FUNC NLKEYCOMPARE,      C_void },
	{ "NLMATCH",            FUNC NLMATCH,           C_void },
	{ "NLMATCHINIT",        FUNC NLMATCHINIT,       C_void },
	{ "NLNUMSPEC",          FUNC NLNUMSPEC,         C_void },
	{ "NLREPCHAR",          FUNC NLREPCHAR,         C_void },
	{ "NLSCANMOVE",         FUNC NLSCANMOVE,        C_void },
	{ "NLSUBSTR",           FUNC NLSUBSTR,          C_void },
	{ "NLSWITCHBUF",        FUNC NLSWITCHBUF,       C_void },
	{ "NLTRANSLATE",        FUNC NLTRANSLATE,       C_void },
	{ "OPENLOG",            FUNC OPENLOG,           C_void },
	{ "PAUSE",              FUNC PAUSE,             C_void },
	{ "PRINT",              FUNC PRINT,             C_void },
	{ "PRINTFILEINFO",      FUNC PRINTFILEINFO,     C_void },
	{ "PRINTOP",            FUNC PRINTOP,           C_void },
	{ "PRINTOPREPLY",       FUNC PRINTOPREPLY,      C_void },
	{ "PROCINFO",           FUNC PROCINFO,          C_void },
	{ "PROCTIME",           FUNC PROCTIME,          C_void },
	{ "PUTJCW",             FUNC PUTJCW,            C_void },
	{ "QUIT",               FUNC QUIT,              C_void },
	{ "QUITPROG",           FUNC QUITPROG,          C_void },
	{ "READ",               FUNC READ,              C_void },
	{ "READX",              FUNC READX,             C_void },
	{ "RECEIVEMAIL",        FUNC RECEIVEMAIL,       C_void },
	{ "RESETCONTROL",       FUNC RESETCONTROL,      C_void },
	{ "RESETDUMP",          FUNC RESETDUMP,         C_void },
	{ "SEARCH",             FUNC SEARCH,            C_void },
	{ "SENDMAIL",           FUNC SENDMAIL,          C_void },
	{ "SETDUMP",            FUNC SETDUMP,           C_void },
	{ "SETJCW",             FUNC SETJCW,            C_void },
	{ "SORTEND",            FUNC SORTEND,           C_void },
	{ "SORTERRORMESS",      FUNC SORTERRORMESS,     C_void },
	{ "SORTINIT",           FUNC SORTINIT,          C_void },
	{ "SORTINPUT",          FUNC SORTINPUT,         C_void },
	{ "SORTOUTPUT",         FUNC SORTOUTPUT,        C_void },
	{ "SORTSTAT",           FUNC SORTSTAT,          C_void },
	{ "SORTTITLE",          FUNC SORTTITLE,         C_void },
	{ "STACKDUMP",          FUNC STACKDUMP,         C_void },
	{ "STARTSESS",          FUNC STARTSESS,         C_void },
	{ "SUSPEND",            FUNC SUSPEND,           C_void },
#if	MPE65
	{ "SWITCHDB",           FUNC SWITCHDB,          C_void },
#endif	/* MPE65 */
	{ "TERMINATE",          FUNC TERMINATE,         C_void },
	{ "TIMER",              FUNC TIMER,             C_void },
	{ "UNLOADPROC",         FUNC UNLOADPROC,        C_void },
	{ "UNLOCKGLORIN",       FUNC UNLOCKGLORIN,      C_void },
	{ "UNLOCKLOCRIN",       FUNC UNLOCKLOCRIN,      C_void },
	{ "WHO",                FUNC WHO,               C_void },
	{ "WRITELOG",           FUNC WRITELOG,          C_void },
	{ "XARITRAP",           FUNC XARITRAP,          C_void },
	{ "XCONTRAP",           FUNC XCONTRAP,          C_void },
	{ "XLIBTRAP",           FUNC XLIBTRAP,          C_void },
	{ "XSYSTRAP",           FUNC XSYSTRAP,          C_void },
	{ "ZSIZE",              FUNC ZSIZE,             C_void },
#endif	/* SYSTEM_INTRINSICS */
        { NULL,                 NULL,           	0 }
        };

#ifdef	WINGSPAN

/* the following functions allow the user-defined stub routines in WINGSXL to */
/* call user-defined routines in the COBOL program.  The COBOL program */
/* defines these routines as ENTRY points.  The ACUCOBOL-GT RECURSION */
/* configuration variable must be set to a value of 1 to allow the COBOL */
/* program to call itself.  Other configuration variables that might be */
/* needed are CHECK_USING = 0 in case the data passed in to the COBOL */
/* linkage section is smaller than the size defined in the linkage section */

/* customconvert is called if there is a user-defined stub routine in WINGSXL */
/* called ccustomconvert */

int
customconvert(screenbuf, length, field, fieldtype, mode, status)
char    *screenbuf;
short   *length;
char    *field;
short   *fieldtype;
short   *mode;
short   *status;
{
        Argument        cblargs[6];
        int             retstatus;
        int             num_parms;

        cblargs[0].a_address = screenbuf;
        cblargs[0].a_length = 160;

        cblargs[1].a_address = (char *)length;
        cblargs[1].a_length = 2;

        cblargs[2].a_address = field;
        cblargs[2].a_length = 160;

        cblargs[3].a_address = (char *)fieldtype;
        cblargs[3].a_length = 2;

        cblargs[4].a_address = (char *)mode;
        cblargs[4].a_length = 2;

        cblargs[5].a_address = (char *)status;
        cblargs[5].a_length = 2;

        /* set the number of parameters */
        num_parms = 6;

        retstatus = cobol("CCUSTOMCONVERT", num_parms, cblargs);

        if (retstatus != 0 ) {
                /* the call to the ENTRY point failed */
		int msg_size;
		char msg[80];
                msg_size = 80;
                retstatus = cobol_exit_code ( msg, &msg_size );
		return 0;
        }

	return 1;

} /* customconvert */

/* userpicklist is called if there is a user-defined stub routine in WINGSXL */
/* called cuserpicklist */

int
userpicklist(buffer, len, type, lenx, stat)
char    *buffer;
short   *len;
short   *type;
short   *lenx;
short   *stat;
{
        Argument        cblargs[5];
        int             retstatus;
        int             num_parms;

        cblargs[0].a_address = buffer;
        cblargs[0].a_length = 81;

        cblargs[1].a_address = (char *)len;
        cblargs[1].a_length = 2;

        cblargs[2].a_address = (char *)type;
        cblargs[2].a_length = 2;

        cblargs[3].a_address = (char *)lenx;
        cblargs[3].a_length = 2;

        cblargs[4].a_address = (char *)stat;
        cblargs[4].a_length = 2;

        /* set the number of parameters */
        num_parms = 5;

        retstatus = cobol("CUSERPICKLIST", num_parms, cblargs);

        if (retstatus != 0 ) {
                /* the call to the ENTRY point failed */
		int msg_size;
		char msg[80];
                msg_size = 80;
                retstatus = cobol_exit_code ( msg, &msg_size );
		return 0;
        }

	return 1;

} /* userpicklist */
#endif	/* WINGSPAN */
