			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			    Copyright (c) 1990				*/
			/*    An unpublished work by Gregory L. Adams.  All rights reserved.	*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <v/video.h>									/* Include video definitions.		*/
#include <v/vlocal.h>
#include <v/vdata.h>
#include <v/vcap.h>

/*						Subroutine entry point.								*/

int wscset(char_set) int char_set;							/* Switch to requested character set.	*/
{
	unsigned char chstr[MAX_ESC];							/* The control string.			*/
	char designator;
	register int i;									/* Working register.			*/

	if ((!vchs_op) || (char_set != vchr_set))					/* Should we optimize?			*/
	{
		switch(char_set)							/* Select the character set.		*/
		{
			case DEFAULT:      {strcpy(chstr,defchs_esc); break;}		/* Select U.S. or U.K. as appropriate.	*/
			case GRAPHICS:     {strcpy(chstr,grchs_esc); break;}		/* Select graphics character set.	*/
			case ROM_STANDARD: {strcpy(chstr,romstdchs_esc); break;}	/* Select in-ROM character set.		*/
			case ROM_GRAPHICS: {strcpy(chstr,romgrchs_esc); break;}		/* Select in-ROM graphics set.		*/
			case DOWN_LOADED:  {strcpy(chstr,dlldchs_esc); break;}		/* Select down line loaded font.	*/

			case US:							/* Change default to U.S./Canada.	*/
			{
				strcpy(chstr,uschs_esc);				/* Load for the US.			*/
				vchs_default = 'B';					/* Set the default.			*/
				break;
			}

			case UK:							/* Explicitly select U.K.		*/
			{
				strcpy(chstr,ukchs_esc);				/* Load for the UK.			*/
				vchs_default = 'A';					/* Set the default.			*/
				break;
			}

			default:
			{
				vre("wscset(%d)-Invalid character set code",char_set);
				return(FAILURE);					/* Oops, invalid character set.		*/
			}
		}
		vcontrol(chstr);							/* Output the character string.		*/
		vchs_op = TRUE;								/* No more local optimization.		*/
		vchr_set = char_set;							/* Remember what it is now.		*/
	}
	return(SUCCESS);								/* And report all went ok.		*/
}
