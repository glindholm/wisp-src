/*  fxzone.c	*/

/*  This routine will convert Wang COBOL signed numeric values (ZONED) to the appropriate AIX COBOL signed numeric values. 	*/

int fxzone(num,sz)									/* Fix signed byte for Zoned numbers	*/
char *num;										/* Pass in ptr to number to check.	*/
short *sz;										/* Pass in ptr to size (bytes) of num.	*/
{
	char *spos;									/* Ptr to byte position in number.	*/
	int len;

	len = *sz;									/* Get the length of the num.		*/
	spos = num;									/* Set ptr to num.			*/
	len--;										/* Decrement so not past end of num.	*/
	spos += len;									/* Set ptr to position of signed byte.	*/

	switch(*spos)									/* Check if sign byte set to negative	*/
	{										/* using Wang values.			*/
		case '}':
			*spos = 'p';							/* convert negative 0, 0x7D to 0x70	*/
			break;
		case 'J':
			*spos = 'q';							/* convert negative 1, 0x4A to 0x71	*/
			break;
		case 'K':
			*spos = 'r';							/* convert negative 2, 0x4B to 0x72	*/
			break;
		case 'L':
			*spos = 's';							/* convert negative 3, 0x4C to 0x73	*/
			break;
		case 'M':
			*spos = 't';							/* convert negative 4, 0x4D to 0x74	*/
			break;
		case 'N':
			*spos = 'u';							/* convert negative 5, 0x4E to 0x75	*/
			break;
		case 'O':
			*spos = 'v';							/* convert negative 6, 0x4F to 0x76	*/
			break;
		case 'P':
			*spos = 'w';							/* convert negative 7, 0x50 to 0x77	*/
			break;
		case 'Q':
			*spos = 'x';							/* convert negative 8, 0x51 to 0x78	*/
			break;
		case 'R':
			*spos = 'y';							/* convert negative 9, 0x52 to 0x79	*/
			break;
		case 0xF0:
			*spos = '0';							/* Convert positive 0, 0xF0 to 0x30	*/
			break;
		case 0xF1:
			*spos = '1';							/* Convert positive 1, 0xF1 to 0x31	*/
			break;
		case 0xF2:
			*spos = '2';							/* Convert positive 2, 0xF2 to 0x32	*/
			break;
		case 0xF3:
			*spos = '3';							/* Convert positive 3, 0xF3 to 0x33	*/
			break;
		case 0xF4:
			*spos = '4';							/* Convert positive 4, 0xF4 to 0x34	*/
			break;
		case 0xF5:
			*spos = '5';							/* Convert positive 5, 0xF5 to 0x35	*/
			break;
		case 0xF6:
			*spos = '6';							/* Convert positive 6, 0xF6 to 0x36	*/
			break;
		case 0xF7:
			*spos = '7';							/* Convert positive 7, 0xF7 to 0x37	*/
			break;
		case 0xF8:
			*spos = '8';							/* Convert positive 8, 0xF8 to 0x38	*/
			break;
		case 0xF9:
			*spos = '9';							/* Convert positive 9, 0xF9 to 0x39	*/
			break;
		default:
			break;
	}
}
