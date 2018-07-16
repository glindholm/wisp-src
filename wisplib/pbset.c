/********************************************************************************************************************************
*																*
*	pbset		Pseudo blank rendition set program									*
*																*
********************************************************************************************************************************/
pbset(pbrend)
short	*pbrend;									/* Accept as VAX COBOL COMP PIC S9(04).	*/
{
	extern	int psb_rndt;								/* Pseudo_blank is a global variable.	*/
	psb_rndt = *pbrend;								/* Set it according to passed param.	*/
}
