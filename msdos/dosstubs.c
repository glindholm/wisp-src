/*
	Copyright (c) 1991, International Digital Scientific, Incorporated.
*/
/*
	dosstubs.c	-	Stubbed out functions not found yet on MSDOS.

	These modules are from SCS WISP MSDOS:

		DOSSTUBS.C - Stubbed routines not found on MSDOS.
		MSDOSFNS.C - Functions written for MSDOS (many are from unix).
		WISPDMF.C  - Like WISPAIX.C, includes shutexitcobol().

	When a stubbed routine gets written, it should be moved from
	the DOSSTUBS.C module into the MSDOSFNS.C module.

	Current list of stubbed functions:
		GRECLEN()   [IDSI]
		LOGOFF()    [IDSI]
		MESSAGE()   [IDSI]
		RETCODE()   [IDSI]
		WISPSORT()  [IDSI]
		filesize()
		fork()      [unix]
		ilpmanage() [IDSI]
		ilpwisp()   [IDSI]
		isexec()
		kill()      [unix]		
		wait()      [unix]
*/

#ifdef MSDOS

int	GRECLEN( file_name )
unsigned	char	*file_name;
{
	vre( " dosstubs.c call to GRECLEN probably from wfiledis.c or COBOL " );
}

int	LOGOFF()
{
	vre( " dosstubs.c call to LOGOFF probably from COBOL " );
}

MESSAGE()
{
	vre( " dosstubs.c call to MESSAGE probably from COBOL " );
}

int	RETCODE( code )
char *code;
{
	vre( " dosstubs.c call to RETCODE probably from COBOL " );
}

WISPSORT()
{
	vre( " dosstubs.c call to WISP> WISPSORT probably from COBOL " );
}

long	filesize( path )
char	*path;
{
	vre( " dosstubs.c call to filesize() probably from wprint.c " );
}

int	fork()	/* void */
{
	vre( " dosstubs.c call to unix> fork() probably from wshelp.c " );
}

ilpmanage()
{
	vre( " dosstubs.c call to WISP> ilpmanage() probably from wshelp.c " );
}

ilpwisp()
{
	vre( " dosstubs.c call to WISP> ilpwisp() probably from wprint.c " );
}

int	isexec( name )
char	*name;
{
	vre( " dosstubs.c call to isexec() probably from wchain.c " );
}

int	kill( pid, sig )
int	pid, sig;
{
	vre( " dosstubs.c call to unix> kill() probably from wshelp.c " );
}

int	wait( termstat )	/* Available on OS/2 and unix.  Prototype is in process.h header file.	*/
int	*termstat;
{
	vre( " dosstubs.c call to unix> wait() probably from wshelp.c " );
}

#endif	/* #ifdef MSDOS */

/*	End of dosstubs.c	*/
