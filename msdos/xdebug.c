/*      XDEBUG.C        -       debug functions for C modules   */

#include <stdio.h>
#include "xdebug.h"


int     first_debug=1;

int     do_debug=1;
int     all_pause=0;

static  int     print_bit=1;
static  int     pause_bit=2;

static  char    in_line[16];


xprintf( df, s, a1, a2, a3, a4, a5, a6, a7, a8, a9 )
int     df;
char    *s;
long    a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	if( first_debug )       get_debug_flags();

	if( ( df & print_bit ) && do_debug )
	{
		printf( s, a1, a2, a3, a4, a5, a6, a7, a8, a9 );
	}
	if( do_debug && (( df & pause_bit ) || all_pause ))
	{
		xpause( 63 );
	}
}

xpause( df )
int     df;
{
	if( first_debug )       get_debug_flags();

	if( ( df & pause_bit ) && do_debug )
	{
		printf( "\n Press return to continue: " );
		gets( in_line );
		if((in_line[0] == 'q') || (in_line[0] == 'Q') || (in_line[0] == 3))
		{
			printf( "\n Run Aborted. \n" );
			exit();
		}
	}
}

get_debug_flags()
{
	FILE    *dfp;

	first_debug = 0;

	dfp = fopen( "DEBUG.FLG", "r" );

	if( dfp != NULL )
	{
		fscanf( dfp, "%d", &do_debug );
		fscanf( dfp, "%d", &all_pause );
	}
}

xaddr( addr )
char    *addr;
{
	printf( "Address is <%8.8lX>\n", addr );
	xpause( 63 );
}
