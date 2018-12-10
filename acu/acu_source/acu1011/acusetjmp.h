/* acusetjmp.h - declarations for acusavenv and aculongjmp routines  */
/* $Id: acusetjmp.h 66993 2015-02-03 22:56:44Z mark $ */

/* Copyright (C) 2002-2004 Micro Focus.  All rights reserved. */


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
