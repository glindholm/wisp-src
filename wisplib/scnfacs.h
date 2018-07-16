/*  Definition of field attribute characters (FACs) for a screen to be passed to vwang.						*/

/**  NOTE:  Many of the facs are not defined.  Please add as needed.								*/

#define STANDARD_FIELD		0x80							/* BRIGHT MODIFY  ALL       NOLINE	*/
#define UPCASE_FIELD		0x81							/* BRIGHT MODIFY  UPPERCASE NOLINE	*/
#define NUMERIC_FIELD		0x82							/* BRIGHT MODIFY  NUMERIC   NOLINE	*/
#define ALPHANUM_FIELD		0x83							/* BRIGHT MODIFY  ALPHANUM  NOLINE	*/
#define BOLD_TEXT		0x84							/* BRIGHT PROTECT ALL       NOLINE	*/
											/* BRIGHT PROTECT UPPERCASE NOLINE	*/
#define NUMPROT_BOLD_FIELD	0x86							/* BRIGHT PROTECT NUMERIC   NOLINE	*/
											/* DIM    MODIFY  ALL       NOLINE	*/
											/* DIM    MODIFY  UPPERCASE NOLINE	*/
											/* DIM    MODIFY  NUMERIC   NOLINE	*/
#define PLAIN_TEXT		0x8C							/* DIM    PROTECT ALL       NOLINE	*/
#define UPPROT_FIELD		0x8D							/* DIM    PROTECT UPPERCASE NOLINE	*/
#define NUMPROT_FIELD		0x8E							/* DIM    PROTECT NUMERIC   NOLINE	*/
#define BLINK_FAC		0x90							/* BLINK  MODIFY  ALL       NOLINE	*/
#define BLINKUP_FIELD		0x91							/* BLINK  MODIFY  UPPERCASE NOLINE	*/
#define BLINKNUM_FIELD		0x92							/* BLINK  MODIFY  NUMERIC   NOLINE	*/
#define BLINK_TEXT		0x94							/* BLINK  PROTECT ALL       NOLINE	*/
											/* BLINK  PROTECT UPPERCASE NOLINE	*/
											/* BLINK  PROTECT NUMERIC   NOLINE	*/
											/* BLANK  MODIFY  ALL       NOLINE	*/
											/* BLANK  MODIFY  UPPERCASE NOLINE	*/
											/* BLANK  MODIFY  NUMERIC   NOLINE	*/
											/* BLANK  PROTECT ALL       NOLINE	*/
											/* BLANK  PROTECT UPPERCASE NOLINE	*/
											/* BLANK  PROTECT NUMERIC   NOLINE	*/
											/* BRIGHT MODIFY  ALL       LINE	*/
											/* BRIGHT MODIFY  UPPERCASE LINE	*/
											/* BRIGHT MODIFY  NUMERIC   LINE	*/
#define BOLD_UNDER_TEXT		0xA4							/* BRIGHT PROTECT ALL       LINE	*/
											/* BRIGHT PROTECT UPPERCASE LINE	*/
											/* BRIGHT PROTECT NUMERIC   LINE	*/
											/* DIM    MODIFY  ALL       LINE	*/
											/* DIM    MODIFY  UPPERCASE LINE	*/
											/* DIM    MODIFY  NUMERIC   LINE	*/
#define UNDER_TEXT		0xAC							/* DIM    PROTECT ALL       LINE	*/
											/* DIM    PROTECT UPPERCASE LINE	*/
											/* DIM    PROTECT NUMERIC   LINE	*/
											/* BLINK  MODIFY  ALL       LINE	*/
											/* BLINK  MODIFY  UPPERCASE LINE	*/
											/* BLINK  MODIFY  NUMERIC   LINE	*/
#define	BLINK_UNDER_TEXT 	0xB4							/* BLINK  PROTECT ALL       LINE	*/
											/* BLINK  PROTECT UPPERCASE LINE	*/
											/* BLINK  PROTECT NUMERIC   LINE	*/
											/* BLANK  MODIFY  ALL       LINE	*/
											/* BLANK  MODIFY  UPPERCASE LINE	*/
											/* BLANK  MODIFY  NUMERIC   LINE	*/
											/* BLANK  PROTECT ALL       LINE	*/
											/* BLANK  PROTECT UPPERCASE LINE	*/
											/* BLANK  PROTECT NUMERIC   LINE	*/
/* ALL       - Uppercase, lowercase and alphanumeric characters									*/
/* NUMERIC   - Digits 0 - 9, decimal and minus sign										*/
/* ALPHANUM  - Alphanumeric characters												*/
/* LINE      - Underlines displayed character											*/
/* NOLINE    - Character not underlined												*/
/* UPPERCASE - Alphanumeric uppercase												*/
