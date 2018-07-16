/*
	prtargs - print the args
		- used to test LINKPROC
*/
#include <stdio.h>
#ifdef WIN32
#include <windows.h>
#endif

main(argc,argv)
int	argc;
char	*argv[];
{
	int	i;

	printf("\nprtargs - Print Arguments\n\n\n");
#ifdef WIN32
	printf("cmd line=[%s]\n\n", GetCommandLine());
#endif
	printf("argc = %d\n\n", argc);

	for(i=0; argv[i]; i++)
	{
		printf("arg[%d] = [%s]\n", i, argv[i]);
	}

	printf("\n\n --- DONE ---\n");

	printf("\n\nPress Enter to continue\n");
	getchar();
	return 0;
}

