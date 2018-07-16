binary_to_packed(src, dest)		                           

long int *src;
unsigned char *dest;

{

	long int x, y, val;
	unsigned char *l_dest, low_ord, hi_ord;
											
	l_dest = dest;                                                                  /* Get local copy of addr.		*/
	*l_dest++ = 0;									/* Set eight bytes.			*/
	*l_dest++ = 0;
	*l_dest++ = 0;
	*l_dest++ = 0;
	*l_dest++ = 0;
	*l_dest++ = 0;
	*l_dest++ = 0;
	*l_dest++ = 0;

	if (!*src) return(0);								/* Ain't gotta do nuthin' if its 0.	*/

	l_dest = dest;									/* Get local copy of addr.		*/

	for (x = 0; x < 7; x++) l_dest++;						/* Point to end of packed variable.	*/

	val = *src;         								/* Copy of the source value.		*/
	low_ord = 12;									/* Start of as positive.		*/
	if (val < 0)									/* Is it negative ?			*/
	{										/* Yup.					*/
		low_ord = 13;								/* Change it to a negative.		*/
		val *= -1;								/* Switch the sign on the value.	*/
	}

	x = val % 10;									/* Get right most digit.		*/
	val = val / 10;									/* Remove right most digit.		*/
	hi_ord = x;									/* Set the high order byte.		*/
	hi_ord = hi_ord * 16;								/* And left shift it.			*/

	*l_dest = hi_ord;								/* Place the high order byte in var.	*/
	*l_dest |= low_ord;								/* And or in the low order byte.	*/
        l_dest--;									/* Move to the left one byte.		*/

	while (val)         								/* While there is still a value.	*/
	{                          
		x = val % 10;								/* Get right most digit.		*/
		val = val / 10;								/* Remove the right most digit.		*/
		low_ord = x;								/* Set the low order byte.		*/

		x = val % 10;								/* Get the right most digit.		*/
		val = val / 10;								/* Remove the right most digit.		*/
		hi_ord = x;								/* Set the high order byte.		*/
		hi_ord = hi_ord * 16;							/* Left shift it.			*/

	 	*l_dest = hi_ord;							/* Combine the low and high order bytes	*/
	 	*l_dest |= low_ord;							/* into the destination byte.		*/

		l_dest--;								/* Point to the next one.		*/
	}
}

packed_to_binary(src, dest)		                           
unsigned char *src;
long int *dest;

{
      	int x;
    	long val;
	unsigned char *source, y;
                      
	source = src;								        /* Local copy of the arg.		*/
                      
	while (!*source) source++;							/* Point to the first non-zero byte.	*/

	val = 0;									/* Start of the value at zero.		*/
	while (*source)									/* While there are still digits.	*/
	{
 		y = *source;								/* Get a copy of the byte.		*/
		y = y & 0xf0;								/* Get the high order byte.		*/
		y = y / 16;								/* Shift it right.			*/
		val *= 10;								/* Shift value left 1 place.		*/
		val += y;								/* Add in the high order byte.		*/

		y = *source;								/* Get a copy of the byte again.	*/
		y = y & 0x0f;								/* Get the low order byte.		*/
		if (y > 9) break;							/* If its a zero, break out.		*/
		val *= 10;								/* Shift value left 1 place.		*/
		val += y;								/* Add in the low order byte.		*/
	     	source++;								/* Point to the next byte.		*/
	}                 

	if (y == 11 || y ==  13) val *= -1;						/* Is this a positive number.		*/

	*dest = val;									/* And set the output.			*/

}
