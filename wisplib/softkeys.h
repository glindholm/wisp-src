/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/
/* SOFT_KEYS.H ... An include file that defines the map for all user definable, and software selectable keys.			*/
                                                                                        
/* This structure defines the two byte character to be assocated with a function key.						*/
/* Table length is 49, keys PF1 through PF4, F1 throuth F20 (available on VT220 or greater series terminals), 'selected'	*/
/* versions of these keys, and the help key.  Note that function keys F1 througth F6 will never be returned to the 		*/
/* program; these keys are detected by the terminal (VT220 or greater).								*/

struct soft_keys									
	{
	int pfkey_meta_value[48];							/* Help, PF1 to PF4, F1 to F20  	*/
											/* selected PF1 to PF4, and 		*/
	  										/* selected F1 to F20.			*/

	char pfkey_value[48][2];							/* Currently we're supporting a two	*/
											/* byte char string value.		*/
	};
/*
**	History:
**	$Log: softkeys.h,v $
**	Revision 1.8  1996-08-19 18:32:57-04  gsl
**	drcs update
**
**
**
*/
