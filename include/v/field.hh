

/* Field test is commented out for now............................

#include <time.h>

	struct tm *time_structure;
	int time_val;

	time(&time_val);

	time_structure = localtime(&time_val);

	if (time_structure->tm_year != 88)
	{
		printf("\033[2J\n\n\n\n\n\007\007 %%VIDEO-F-FLDTSTERN Field test terminated. Contact:\n");
		printf("                    Int'l Digital Scientific Incorporated (805) 295-1155.\n");
		sleep(10);
		exit();
	}
*/
