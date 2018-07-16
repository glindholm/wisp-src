/*      XDEBUG.H        -       debug definitions for C modules   */

#ifndef DEBUG_FLAG
#define DEBUG_FLAG      1
#endif

#define XPRINT  1
#define XPAUSE  2

#define XPRF    ( DEBUG_FLAG * XPRINT )
#define XPSF    ( DEBUG_FLAG * XPAUSE )
#define XPPF    ( DEBUG_FLAG * ( XPRINT + XPAUSE ) )

#define XPF(str)        xprintf( XPRF, str )
#define XPF1(str,a1)    xprintf( XPRF, str, a1 )
#define XPF2(str,a1,a2) xprintf( XPRF, str, a1, a2 )
#define XPF3(str,a1,a2,a3) xprintf( XPRF, str, a1, a2, a3 )
#define XPF4(str,a1,a2,a3,a4) xprintf( XPRF, str, a1, a2, a3, a4 )
#define XPF5(str,a1,a2,a3,a4,a5) xprintf( XPRF, str, a1, a2, a3, a4, a5 )

#define XP()    xpause( XPSF )

#define XPFP(str)        xprintf( XPPF, str )
#define XPFP1(str,a1)    xprintf( XPPF, str, a1 )
#define XPFP2(str,a1,a2) xprintf( XPPF, str, a1, a2 )
#define XPFP3(str,a1,a2,a3) xprintf( XPPF, str, a1, a2, a3 )
#define XPFP4(str,a1,a2,a3,a4) xprintf( XPPF, str, a1, a2, a3, a4 )
#define XPFP5(str,a1,a2,a3,a4,a5) xprintf( XPPF, str, a1, a2, a3, a4, a5 )

