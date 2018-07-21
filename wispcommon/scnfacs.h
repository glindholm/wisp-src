/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		scnfacs.h
**
**	Project:	wisp
**
**	Purpose:	All the FAC macros and defines
**
*/

#ifndef SCNFACS_H
#define SCNFACS_H
/*
**	Structures and Defines
*/
typedef unsigned char fac_t;


/*
**	FAC component bit masks
*/
#define FAC_MASK_FAC		((fac_t)0x80)	/* 1000 0000 */
#define FAC_MASK_MODIFIED	((fac_t)0x40)	/* 0100 0000 */
#define FAC_MASK_UNDERSCORED	((fac_t)0x20)	/* 0010 0000 */
#define FAC_MASK_RENDITION	((fac_t)0x18)	/* 0001 1000 */
#define FAC_MASK_PROTECTED	((fac_t)0x04)	/* 0000 0100 */
#define FAC_MASK_DATATYPE	((fac_t)0x03)	/* 0000 0011 */

/*
**	FAC component values
*/
#define FAC_FAC_ON		((fac_t)0x80)	/* 1xxx xxxx */
#define FAC_MODIFIED_ON		((fac_t)0x40)	/* x1xx xxxx */
#define FAC_UNDERSCORED_ON	((fac_t)0x20)	/* xx1x xxxx */
#define FAC_RENDITION_BRIGHT	((fac_t)0x00)	/* xxx0 0xxx */
#define FAC_RENDITION_DIM	((fac_t)0x08)	/* xxx0 1xxx */
#define FAC_RENDITION_BLINK	((fac_t)0x10)	/* xxx1 0xxx */
#define FAC_RENDITION_BLANK	((fac_t)0x18)	/* xxx1 1xxx */
#define FAC_PROTECTED_ON	((fac_t)0x04)	/* xxxx x1xx */
#define FAC_DATATYPE_ALPHA	((fac_t)0x00)	/* xxxx xx00 */
#define FAC_DATATYPE_UPPER	((fac_t)0x01)	/* xxxx xx01 */
#define FAC_DATATYPE_NUMERIC	((fac_t)0x02)	/* xxxx xx10 */

/*
**	FAC macros
**
**	FAC_ATTR	Test an attribute
**	FAC_CLEAR_ATTR	Clear an attribute
**	FAC_SET_ATTR	Set an attribute
*/
#define FAC_ATTR(fac,mask,value)	(((fac) & (mask)) == (value))
#define FAC_CLEAR_ATTR(fac,mask)	((fac) & ~(mask))
#define FAC_SET_ATTR(fac,mask,value)	(FAC_CLEAR_ATTR(fac,mask) | (value))

/*
**	FAC macros to test each of the attributes
*/
#define FAC_FAC(fac)			FAC_ATTR(fac, FAC_MASK_FAC,		FAC_FAC_ON)
#define FAC_MODIFIED(fac)		FAC_ATTR(fac, FAC_MASK_MODIFIED,	FAC_MODIFIED_ON)
#define FAC_UNDERSCORED(fac)		FAC_ATTR(fac, FAC_MASK_UNDERSCORED,	FAC_UNDERSCORED_ON)
#define FAC_BRIGHT(fac)			FAC_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_BRIGHT)
#define FAC_DIM(fac)			FAC_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_DIM)
#define FAC_BLINK(fac)			FAC_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_BLINK)
#define FAC_BLANK(fac)			FAC_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_BLANK)
#define FAC_PROTECTED(fac)		FAC_ATTR(fac, FAC_MASK_PROTECTED,	FAC_PROTECTED_ON)
#define FAC_ALPHA(fac)			FAC_ATTR(fac, FAC_MASK_DATATYPE,	FAC_DATATYPE_ALPHA)
#define FAC_UPPER(fac)			FAC_ATTR(fac, FAC_MASK_DATATYPE,	FAC_DATATYPE_UPPER)
#define FAC_NUMERIC(fac)		FAC_ATTR(fac, FAC_MASK_DATATYPE,	FAC_DATATYPE_NUMERIC)

/*
**	FAC macros to set each of the attributes
*/
#define FAC_SET_FAC(fac)		FAC_SET_ATTR(fac, FAC_MASK_FAC,		FAC_FAC_ON)
#define FAC_SET_MODIFIED(fac)		FAC_SET_ATTR(fac, FAC_MASK_MODIFIED,	FAC_MODIFIED_ON)
#define FAC_SET_UNDERSCORED(fac)	FAC_SET_ATTR(fac, FAC_MASK_UNDERSCORED,	FAC_UNDERSCORED_ON)
#define FAC_SET_BRIGHT(fac)		FAC_SET_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_BRIGHT)
#define FAC_SET_DIM(fac)		FAC_SET_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_DIM)
#define FAC_SET_BLINK(fac)		FAC_SET_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_BLINK)
#define FAC_SET_BLANK(fac)		FAC_SET_ATTR(fac, FAC_MASK_RENDITION,	FAC_RENDITION_BLANK)
#define FAC_SET_PROTECTED(fac)		FAC_SET_ATTR(fac, FAC_MASK_PROTECTED,	FAC_PROTECTED_ON)
#define FAC_SET_ALPHA(fac)		FAC_SET_ATTR(fac, FAC_MASK_DATATYPE,	FAC_DATATYPE_ALPHA)
#define FAC_SET_UPPER(fac)		FAC_SET_ATTR(fac, FAC_MASK_DATATYPE,	FAC_DATATYPE_UPPER)
#define FAC_SET_NUMERIC(fac)		FAC_SET_ATTR(fac, FAC_MASK_DATATYPE,	FAC_DATATYPE_NUMERIC)

/*
**	FAC macros to clear each of the (binary) attributes
*/
#define FAC_CLEAR_FAC(fac)		FAC_CLEAR_ATTR(fac, FAC_MASK_FAC)
#define FAC_CLEAR_MODIFIED(fac)		FAC_CLEAR_ATTR(fac, FAC_MASK_MODIFIED)
#define FAC_CLEAR_UNDERSCORED(fac)	FAC_CLEAR_ATTR(fac, FAC_MASK_UNDERSCORED)
#define FAC_CLEAR_PROTECTED(fac)	FAC_CLEAR_ATTR(fac, FAC_MASK_PROTECTED)


/*  Definition of field attribute characters (FACs) for a screen to be passed to vwang.						*/

/* ALL       - Uppercase, lowercase and alphanumeric characters	*/
/* NUMERIC   - Digits 0 - 9, decimal and minus sign		*/
/* ALPHANUM  - Alphanumeric characters				*/
/* LINE      - Underlines displayed character			*/
/* NOLINE    - Character not underlined				*/
/* UPPERCASE - Alphanumeric uppercase				*/


#define FAC_MOD_BOLD		((fac_t)0x80)					/* BRIGHT MODIFY  ALL       NOLINE	*/
#define FAC_MOD_BOLD_UP		((fac_t)0x81)					/* BRIGHT MODIFY  UPPERCASE NOLINE	*/
#define FAC_MOD_BOLD_NUM	((fac_t)0x82)					/* BRIGHT MODIFY  NUMERIC   NOLINE	*/

#define FAC_PROT_BOLD		((fac_t)0x84)					/* BRIGHT PROTECT ALL       NOLINE	*/
#define FAC_PROT_BOLD_UP	((fac_t)0x85)					/* BRIGHT PROTECT UPPERCASE NOLINE	*/
#define FAC_PROT_BOLD_NUM	((fac_t)0x86)					/* BRIGHT PROTECT NUMERIC   NOLINE	*/

#define FAC_MOD_DIM		((fac_t)0x88)					/* DIM    MODIFY  ALL       NOLINE	*/
#define FAC_MOD_DIM_UP		((fac_t)0x89)					/* DIM    MODIFY  UPPERCASE NOLINE	*/
#define FAC_MOD_DIM_NUM		((fac_t)0x8A)					/* DIM    MODIFY  NUMERIC   NOLINE	*/

#define FAC_PROT_DIM		((fac_t)0x8C)					/* DIM    PROTECT ALL       NOLINE	*/
#define FAC_PROT_DIM_UP		((fac_t)0x8D)					/* DIM    PROTECT UPPERCASE NOLINE	*/
#define FAC_PROT_DIM_NUM	((fac_t)0x8E)					/* DIM    PROTECT NUMERIC   NOLINE	*/

#define FAC_MOD_BLINK		((fac_t)0x90)					/* BLINK  MODIFY  ALL       NOLINE	*/
#define FAC_MOD_BLINK_UP	((fac_t)0x91)					/* BLINK  MODIFY  UPPERCASE NOLINE	*/
#define FAC_MOD_BLINK_NUM	((fac_t)0x92)					/* BLINK  MODIFY  NUMERIC   NOLINE	*/

#define FAC_PROT_BLINK		((fac_t)0x94)					/* BLINK  PROTECT ALL       NOLINE	*/
#define FAC_PROT_BLINK_UP	((fac_t)0x95)					/* BLINK  PROTECT UPPERCASE NOLINE	*/
#define FAC_PROT_BLINK_NUM	((fac_t)0x96)					/* BLINK  PROTECT NUMERIC   NOLINE	*/

#define FAC_MOD_BLANK		((fac_t)0x98)					/* BLANK  MODIFY  ALL       NOLINE	*/
#define FAC_MOD_BLANK_UP	((fac_t)0x99)					/* BLANK  MODIFY  UPPERCASE NOLINE	*/
#define FAC_MOD_BLANK_NUM	((fac_t)0x9A)					/* BLANK  MODIFY  NUMERIC   NOLINE	*/

#define FAC_PROT_BLANK		((fac_t)0x9C)					/* BLANK  PROTECT ALL       NOLINE	*/
#define FAC_PROT_BLANK_UP	((fac_t)0x9D)					/* BLANK  PROTECT UPPERCASE NOLINE	*/
#define FAC_PROT_BLANK_NUM	((fac_t)0x9E)					/* BLANK  PROTECT NUMERIC   NOLINE	*/

#define FAC_MOD_BOLD_LINE	((fac_t)0xA0)					/* BRIGHT MODIFY  ALL       LINE	*/
#define FAC_MOD_BOLD_LINE_UP	((fac_t)0xA1)					/* BRIGHT MODIFY  UPPERCASE LINE	*/
#define FAC_MOD_BOLD_LINE_NUM	((fac_t)0xA2)					/* BRIGHT MODIFY  NUMERIC   LINE	*/

#define FAC_PROT_BOLD_LINE	((fac_t)0xA4)					/* BRIGHT PROTECT ALL       LINE	*/
#define FAC_PROT_BOLD_LINE_UP	((fac_t)0xA5)					/* BRIGHT PROTECT UPPERCASE LINE	*/
#define FAC_PROT_BOLD_LINE_NUM	((fac_t)0xA6)					/* BRIGHT PROTECT NUMERIC   LINE	*/

#define FAC_MOD_DIM_LINE	((fac_t)0xA8)					/* DIM    MODIFY  ALL       LINE	*/
#define FAC_MOD_DIM_LINE_UP	((fac_t)0xA9)					/* DIM    MODIFY  UPPERCASE LINE	*/
#define FAC_MOD_DIM_LINE_NUM	((fac_t)0xAA)					/* DIM    MODIFY  NUMERIC   LINE	*/

#define FAC_PROT_DIM_LINE	((fac_t)0xAC)					/* DIM    PROTECT ALL       LINE	*/
#define FAC_PROT_DIM_LINE_UP	((fac_t)0xAD)					/* DIM    PROTECT UPPERCASE LINE	*/
#define FAC_PROT_DIM_LINE_NUM	((fac_t)0xAE)					/* DIM    PROTECT NUMERIC   LINE	*/

#define FAC_MOD_BLINK_LINE	((fac_t)0xB0)					/* BLINK  MODIFY  ALL       LINE	*/
#define FAC_MOD_BLINK_LINE_UP	((fac_t)0xB1)					/* BLINK  MODIFY  UPPERCASE LINE	*/
#define FAC_MOD_BLINK_LINE_NUM	((fac_t)0xB2)					/* BLINK  MODIFY  NUMERIC   LINE	*/

#define	FAC_PROT_BLINK_LINE 	((fac_t)0xB4)					/* BLINK  PROTECT ALL       LINE	*/
#define	FAC_PROT_BLINK_LINE_UP 	((fac_t)0xB5)					/* BLINK  PROTECT UPPERCASE LINE	*/
#define	FAC_PROT_BLINK_LINE_NUM	((fac_t)0xB6)					/* BLINK  PROTECT NUMERIC   LINE	*/

#define	FAC_MOD_BLANK_LINE 	((fac_t)0xB8)					/* BLANK  MODIFY  ALL       LINE	*/
#define	FAC_MOD_BLANK_LINE_UP 	((fac_t)0xB9)					/* BLANK  MODIFY  UPPERCASE LINE	*/
#define	FAC_MOD_BLANK_LINE_NUM 	((fac_t)0xBA)					/* BLANK  MODIFY  NUMERIC   LINE	*/

#define	FAC_PROT_BLANK_LINE 	((fac_t)0xBC)					/* BLANK  PROTECT ALL       LINE	*/
#define	FAC_PROT_BLANK_LINE_UP 	((fac_t)0xBD)					/* BLANK  PROTECT UPPERCASE LINE	*/
#define	FAC_PROT_BLANK_LINE_NUM	((fac_t)0xBE)					/* BLANK  PROTECT NUMERIC   LINE	*/

/*
**	OLD FAC defines
*/
#define FAC_CHARACTER		((fac_t)0x80)	/* 1000 0000 */			/* FAC ID bit.				*/
#define FAC_ALTERED		((fac_t)0x40)	/* 0100 0000 */			/* Altered read bit.			*/
#define FAC_SELECTED		((fac_t)0x40)	/* 0100 0000 */			/* Selected write bit.			*/
#define FAC_UNDERSCORE  	((fac_t)0x20)	/* 0010 0000 */			/* Underscore bit.			*/
#define FAC_BLINKING		((fac_t)0x10)	/* 0001 0000 */			/* Blinking bit.			*/
#define FAC_LOW_INTENSITY	((fac_t)0x08)	/* 0000 1000 */			/* Low intensity bit.			*/
#define FAC_PROTECT		((fac_t)0x04)	/* 0000 0100 */			/* Protected char bit.			*/
#define FAC_NUMERIC_ONLY	((fac_t)0x02)	/* 0000 0010 */			/* Numeric only bit.			*/
#define FAC_UPPERCASE_ONLY	((fac_t)0x01)	/* 0000 0001 */			/* Uppercase only bit.			*/

#define FAC_DEFAULT_FIELD	FAC_MOD_BOLD_UP
#define FAC_UPLOW_FIELD		FAC_MOD_BOLD
#define FAC_NUMERIC_FIELD	FAC_MOD_BOLD_NUM
#define FAC_ERROR_FIELD		FAC_MOD_BLINK_UP
#define FAC_NUM_ERROR_FIELD	FAC_MOD_BLINK_NUM
#define FAC_DEFAULT_TEXT	FAC_PROT_DIM
#define FAC_BOLD_TEXT		FAC_PROT_BOLD
#define FAC_BLINK_TEXT		FAC_PROT_BLINK

#define STANDARD_FIELD		FAC_MOD_BOLD
#define UPCASE_FIELD		FAC_MOD_BOLD_UP
#define NUMERIC_FIELD		FAC_MOD_BOLD_NUM
#define BOLD_TEXT		FAC_PROT_BOLD
#define NUMPROT_BOLD_FIELD	FAC_PROT_BOLD_NUM
#define PLAIN_TEXT		FAC_PROT_DIM
#define UPPROT_FIELD		FAC_PROT_DIM_UP
#define NUMPROT_FIELD		FAC_PROT_DIM_NUM
#define BLINK_FAC		FAC_MOD_BLINK
#define BLINKUP_FIELD		FAC_MOD_BLINK_UP
#define BLINKNUM_FIELD		FAC_MOD_BLINK_NUM
#define BLINK_TEXT		FAC_PROT_BLINK
#define BOLD_UNDER_TEXT		FAC_PROT_BOLD_LINE
#define UNDER_TEXT		FAC_PROT_DIM_LINE
#define	BLINK_UNDER_TEXT 	FAC_PROT_BLINK_LINE

#endif /* SCNFACS_H */

/*
**	History:
**	$Log: scnfacs.h,v $
**	Revision 1.11  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.10  1998/01/07 15:27:57  gsl
**	Add common field defines
**	
**	Revision 1.9  1997-10-29 11:30:13-05  gsl
**	Add a bunch of missing FAC defines
**	Add all the FAC macros from vwang.h
**
**	Revision 1.8  1995-06-13 10:18:15-04  gsl
**	Cast everything to (fac_t)
**
**
*/
