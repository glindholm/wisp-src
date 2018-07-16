/* 
        Copyright (c) 1997 NeoMedia Technologies, Inc., All rights reserved.
        $Id:$
*/

/*
**      File:           kcsimem.h
**
**      Project:        kcsi/common
**
**      RCS:            $Source:$
**
**      Purpose:        Support memory routines
**
*/

#ifndef kcsimem_H
#define kcsimem_H

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
/*
**	Structures and Defines
*/
enum
{
  HeapMemMaxLevel = 10
};    

struct HeapMemStruct 
{
  int level;
  long numallocs[HeapMemMaxLevel];
  long numfrees[HeapMemMaxLevel];
};

typedef struct HeapMemStruct HeapMem;

/*
 *   Memory allocation
 */

/* Indirect memory allocation to allow leak detection and debugging */

#define FREEDMEMORY	(void*)0xdeadbeef

#define MALLOCT( t ) ((t*)HeapMem__Alloc( sizeof( t ) ))
#define MALLOCA( t, n ) ((t*)HeapMem__Alloc( sizeof( t )*n ))
#define MALLOCS( s ) ((char*)HeapMem__Alloc( strlen( s ) +1))

#define FREE( p ) if( p ) { HeapMem__Free( p ); p = FREEDMEMORY;}

#define ASSERTNOTFREE( p ) ASSERT( p && p != FREEDMEMORY )

/*
**	Function Prototypes
*/

void HeapMem__Exception( size_t nbytes );
void* HeapMem__Alloc( size_t bytes );
void* HeapMem__ReAlloc( void* ptr, size_t bytes );
void HeapMem__Free( void* ptr );

/* Debugging */
void HeapMem__LeakDetection( void );
void HeapMem__CheckForLeaks( void );

#endif	/* kcsimem_H */

/*
**      History:
**      $Log: kcsimem.h,v $
**      Revision 1.1  1997/08/01 15:18:44  scass
**      Initial revision
**
**
*/

