/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
**
******************************************************************************
*/


/*
**	File:		kcsimem.c
**
**	Project:	wisp/kcsi/common
**
**
**
**	Purpose:	Support routines for memory allocation/administration
** 			Indirect memory allocation to allow leak detection and debugging
*/

/*
**	Includes
*/
#include "kcsimem.h"
#include "assert.h"
#include "kcsit.h"

/*
**	Structures and Defines
*/


/*
**	Globals and Externals
*/
static HeapMem theHeap = { -1 };

/*
**	Function Prototypes
*/

void HeapMem__Exception( size_t nbytes )
{
  fprintf( stderr, "*** Out of memory: Failed to allocate %lu bytes ***\n\n", nbytes );
  kcsi_exit(1);
}

void* HeapMem__Alloc( size_t bytes )
{
  void *ret = malloc( bytes );
  kcsitrace(1,"HeapMem__Alloc()", "enter", "Entering subroutine ...");
  if( !ret ) HeapMem__Exception( bytes );
  ASSERTNOTFREE( ret );

#ifdef DEBUG
  {
    int i;
    for( i = 0; i <= theHeap.level; i++ )
      {
        theHeap.numallocs[i] += 1;
      }
  }
#endif

  return ret;
}

void* HeapMem__ReAlloc( void* ptr, size_t bytes )
{
  void* ret = realloc( ptr, bytes );
  if( !ret ) HeapMem__Exception( bytes );
  ASSERTNOTFREE( ret );
  return ret;
}

void HeapMem__Free( void* ptr )
{

#ifdef DEBUG
  int i;

  kcsitrace(1,"HeapMem__Free()", "enter", "Entering subroutine ...");

  for( i = 0; i <= theHeap.level; i++ )
    {
      theHeap.numfrees[i] += 1;
    }
#endif

  free( ptr );
}

void HeapMem__LeakDetection()
{
  theHeap.level++;
  ASSERT( theHeap.level < HeapMemMaxLevel );
  theHeap.numallocs[ theHeap.level ] = 0;
  theHeap.numfrees[ theHeap.level ] = 0;
}  

void HeapMem__CheckForLeaks()
{
  ASSERT( theHeap.level >= 0 );
  if( theHeap.numallocs[ theHeap.level ] != theHeap.numfrees[ theHeap.level ] )
    {
      fprintf( stderr, "Memory leak!: allocs = %ld, frees = %ld\n\n", 
              theHeap.numallocs[ theHeap.level ], theHeap.numfrees[ theHeap.level ] );
      ASSERT( 0 );
    }
  theHeap.level--;
}

/*
**	History:
**	$Log: kcsimem.c,v $
**	Revision 1.5  2010/01/22 16:44:05  gsl
**	Fix printf %lu size_t warnings
**	
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  2002/10/17 21:22:42  gsl
**	cleanup
**	
**	Revision 1.2  1997/08/01 18:09:35  scass
**	Removed include system.h
**	
**	Revision 1.1  1997-08-01 11:59:11-04  scass
**	Initial revision
**
**
*/
