/* SUB_CHAR.H ... A 'C' include file.  The structure described by this include file defines a table for substitution key values	*/
/*		  and their needed character set (mode) where the input key is the index.					*/

struct 	SUB_CHAR_TABLE_DEF
{
	char sub_value;									/* Substitution key value.		*/
	int  char_set;									/* Needed character set mode.		*/
};
											
