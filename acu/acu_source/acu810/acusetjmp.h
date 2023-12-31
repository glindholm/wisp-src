/* acusetjmp.h - declarations for acusavenv and aculongjmp routines  */
/* $Id: acusetjmp.h 50640 2007-03-13 17:15:03Z merge $ */

/* Copyright (c) 2002-2007 by Acucorp, Inc.  All rights reserved.	*/


#ifndef	ACU_LIB_ACUSETJMP_H
#define	ACU_LIB_ACUSETJMP_H

#include <setjmp.h>

typedef struct tag_acujmpbuf {
	jmp_buf	setjmp_mark;
	void	*acu_state;
} acujmp_buf;

extern	jmp_buf	*acusavenv(acujmp_buf *buffer);
extern	void	aculongjmp(acujmp_buf *buffer);

#endif	/* ACU_LIB_ACUSETJMP_H */
