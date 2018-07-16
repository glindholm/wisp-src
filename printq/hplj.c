#include <stdio.h>
main()
{
	int ch;
	int col;

	col=0;

	while ((ch=getchar())!=EOF)
	{
		if (ch=='\n')
		{ 
			putchar('\n');
			col=1; 
		}
		else if (ch=='\t')
		{
			if (col%8==0)  ++col;
		        while (col%8) { putchar(' '); ++col; }
		}
		else if (ch=='\f')
		{
		        putchar(27);
			putchar('&');
			putchar('l'); putchar('0');
			putchar('H');
		}
		else { putchar(ch); ++col; }
	}
}
