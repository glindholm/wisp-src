#ifndef MOVEBIN_INCLUDED
#define MOVEBIN_INCLUDED

#include <string.h>
#ifndef VMS	/* unix or MSDOS */
#include <memory.h>
#endif
/*
	GETBIN and PUTBIN are macros that get/put a 2 or 4 byte binary
        from/to a possible unalign memory location.

	D = long/short ptr to destination.
	S = long/short ptr to source.
	L = length; it must be 2 or 4.

	If mem is aligned then time is saved by doing a simple assignment.
*/

#if 0
#define MV1(D,S,L) { ((char *)(D))[L]=((char *)(S))[L]; }

#define MVCHS(D,S,L) { if (L==2) {MV1(D,S,0);MV1(D,S,1);} else {MV1(D,S,0);MV1(D,S,1);MV1(D,S,2);MV1(D,S,3);} } 
	 
#define GETBIN(D,S,L) { if ((long)(S)&((L==4)?3:1)) { MVCHS(D,S,L) } else { *(D) = *(S); } }
#define PUTBIN(D,S,L) { if ((long)(D)&((L==4)?3:1)) { MVCHS(D,S,L) } else { *(D) = *(S); } } 
#endif

#define PUTBIN(D,S,L) { memcpy((D),(S),(L)); }
#define GETBIN(D,S,L) { memcpy((D),(S),(L)); } 

#endif 	/* MOVEBIN_INCLUDED	*/

