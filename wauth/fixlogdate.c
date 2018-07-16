#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DATA_START_COL 20

struct
{
		char	*month;
		char	*moy;
} monthstab[] =
{
	{"Jan","01"},
	{"Feb","02"},
	{"Mar","03"},
	{"Apr","04"},
	{"May","05"},
	{"Jun","06"},
	{"Jul","07"},
	{"Aug","08"},
	{"Sep","09"},
	{"Oct","10"},
	{"Nov","11"},
	{"Dec","12"},
	{NULL,NULL}
};

int main()
{
	char	inbuf[1024];
	char	*ptr;
	char	*rhs, *lhs;
	int	len;
	int	cnt = 0;	

	while(ptr = gets(inbuf))
	{
		if (0==memcmp("TIME",inbuf,4))
		{
			/*
			** 12345678901234567890
			** TIME               Thu May 8 15:59:05 1992
			*/
			char  sDay[20], sMonth[20], sTime[20], sYear[20];
			int nDom = 0;
			char *sMoy = NULL;
			int i;
			int rc;
			
			rc = sscanf(&inbuf[DATA_START_COL],"%s %s %d %s %s",
				    sDay, sMonth, &nDom, sTime, sYear);
			if (rc != 5)
			{
				fprintf(stderr, "[%s] sscanf() rc=%d\n", &inbuf[DATA_START_COL], rc);
				exit(1);
			}

			for(i=0; monthstab[i].month != NULL; i++)
			{
				if (0==strcmp(monthstab[i].month, sMonth))
				{
					sMoy = monthstab[i].moy;
					break;
				}
			}
			if (sMoy == NULL)
			{
				fprintf(stderr, "[%s] Cannot convert month [%s]\n", &inbuf[DATA_START_COL], sMonth);
				exit(1);
			}
			
			
			/*
			** "TIME               Thu May 8 15:59:05 1992"
			** "TIME               1992-05-08 15:59:05"
			*/
			printf("TIME               %s-%s-%02d %s\n",
			       sYear, sMoy, nDom, sTime);
		}
		else
		{
			printf("%s\n",inbuf);
		}
		cnt++;
		
	}

	fprintf(stderr,"Processed %d lines\n", cnt);
	return 0;
	
}


