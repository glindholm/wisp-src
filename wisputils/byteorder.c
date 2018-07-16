#include <stdio.h>
main()
{
	long	l;
	char	*ptr;

	l = 0x12345678;
	ptr = (char *)&l;

	printf( "\nBYTE-ORDER <%8.8x> = <%2.2x> <%2.2x> <%2.2x> <%2.2x> \n",
		l, ptr[0], ptr[1], ptr[2], ptr[3] );
}
	
