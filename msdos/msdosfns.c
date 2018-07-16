/*
	Copyright (c) 1991, International Digital Scientific, Incorporated.
*/
/*
	MSDOSFNS.C	-	MSDOS Functions from unix system calls.

	These modules are from SCS WISP MSDOS:

		DOSSTUBS.C - Stubbed routines not found on MSDOS.
		MSDOSFNS.C - Functions written for MSDOS (many are from unix).
		WISPDMF.C  - Like WISPAIX.C, includes shutexitcobol().

	When a stubbed routine gets written, it should be moved from
	the DOSSTUBS.C module into the MSDOSFNS.C module.

	Current list of functions:

		cuserid()  [unix] returns env var USER or DEFAULT_UID.
		ttyname()  [unix] returns env var TTY  or DEFAULT_TTY.
		sleep(s)   [unix] suspends operation for "s" seconds.
		getopt()   [unix] parses option flags.
		PARSECOM() [IDSI] parses COBOL startup command line.
*/

#ifdef MSDOS

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <memory.h>
#include <sys/types.h>

#define DEFAULT_UID	"USR-DOS"
#define DEFAULT_TTY	"/dev/tty00"
#define UID_ENV_NAME	"USER"
#define TTY_ENV_NAME	"TTY"

#define CAC	( argv[optind][cur_char] )

char	*optarg;
int	optind=0;
int	opterr=1;

static	int	cur_char=0;


char *cuserid(cuid)
char *cuid;
{
	static	int	first_time = 1;
	char	*env_uid;
	static	char	dosuid[40] = DEFAULT_UID;

	if( first_time )
	{
		first_time = 0 ;
		if( NULL != ( env_uid = getenv ( UID_ENV_NAME ) ) )
		{
			strcpy( dosuid, env_uid );
		}
	}

	if( NULL == cuid )
	{
		return( dosuid );
	}
	else
	{
		strcpy( cuid, dosuid );
		return( cuid );
	}
}




char	*ttyname(fd)
int	fd;
{
	static	int	first_time = 1;
	char	*env_tty;
	static	char	dostty[40] = DEFAULT_TTY;

	if( first_time )
	{
		first_time = 0 ;
		if( NULL != ( env_tty = getenv ( TTY_ENV_NAME ) ) )
		{
			strcpy( dostty, env_tty );
		}
	}

	return( dostty );
}


unsigned sleep(secs)
unsigned secs;
{
	time_t	wake;

	wake = secs + time( NULL );
	while( wake > time( NULL ) );

	return( 0 );
}



int	getopt( argc, argv, flags)
int	argc;
char	*argv[];
char	*flags;
{
	int	ret_code;
	char	*flg_ptr;

	ret_code = 0;

	if ( cur_char == 0 )
	{
		while((( CAC != '-') && ( CAC != '/' )) && (optind < argc))
		{
			++optind;
		}		
	}

	while((( CAC == '-') || ( CAC == '/' )) && (optind < argc))
	{
		next_cac( argv );
	}

	if ( optind >= argc )
	{
		ret_code = EOF;
		optind = 0;
		cur_char = 0;
	}
	else
	{
		if ( NULL == ( flg_ptr = strchr ( flags, CAC ) ) )
		{
			ret_code = '?';
			next_cac( argv );
		}
		else
		{
			next_cac( argv );
			if ( *flg_ptr == ':' )
			{
				ret_code = '?';
			}
			else
			{
				ret_code = *flg_ptr;
				++flg_ptr;
				if ( *flg_ptr == ':' )
				{
					optarg = &CAC;
					++optind;
					cur_char = 0;
				}
			}
		}
	}

	return ( ret_code );
}

static	next_cac( argv )
char	*argv[];
{
	++cur_char;
	if( CAC == '\0' )
	{
		++optind;
		cur_char = 0;
	}
}


PARSECOM( com_line, com_link )
char *com_line, *com_link;
{
	char	*cptr;
	int	i;

	cptr = NULL;

	for( i = 0 ; (i < 80) && ( '\0' != com_line[i] ) ; ++i )
	{
		if ( ' ' == com_line[i] )
		{
			if ( ' ' != com_line[i+1] )
			{
				cptr = &com_line[i+1];
			}
		}
	}

	if ( NULL == cptr )
	{
		strncpy( com_link, cptr, 12 );
	}
	else
	{
		memset( com_link, ' ', 12 );
	}
}


#endif	/* #ifdef MSDOS */
