#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static int rowcnt = 0;


static int readtabfile(FILE* fp, 
		       char *pType,
		       char *pUserId,
		       char *pTime,
		       char *pCustName,
		       char *pCustNum,
		       char *pPlatform,
		       char *pLicType,
		       char *pLicDate,
		       char *pExpDate,
		       char *pLicKey,
		       char *pMachId,
		       char *pValCode)
{
#define NUMARGS 12
	char buff[1024];
	char *args[NUMARGS];
	int i;
	char *bptr, *eptr;
	

	args[0] = pType;
	args[1] = pUserId;
	args[2] = pTime;
	args[3] = pCustName;
	args[4] = pCustNum;
	args[5] = pPlatform;
	args[6] = pLicType;
	args[7] = pLicDate;
	args[8] = pExpDate;
	args[9] = pLicKey;
	args[10] = pMachId;
	args[11] = pValCode;

	for(i=0; i<NUMARGS; i++)
	{
		args[i][0] = '\0';
	}

	if (NULL == fgets(buff, sizeof(buff), fp))
	{
		return 1;
	}
	buff[strlen(buff)-1] = '\0';  /* Remove newline (\n) */

	bptr = buff;
	for(i=0; i<NUMARGS; i++)
	{
		if ((eptr = strchr(bptr,'\t')))
		{
			*eptr = '\0';
		}
		strcpy(args[i],bptr);

		if (NULL==eptr)
		{
			if (i < NUMARGS-1)
			{
				fprintf(stderr,"readtabfile:Line %d has only %d args\n", rowcnt, i+1);
			}
			
			break;
		}
		
		bptr = eptr+1;
	}
	
	
	return 0;
}

int main()
{

	char 	szType[80],
		szUserId[80],
		szTime[80],
		szCustName[80],
		szCustNum[80],
		szPlatform[80],
		szLicType[80],
		szLicDate[80],
		szExpDate[80],
		szLicKey[80],
		szMachId[80],
		szValCode[80];

	while( 0==readtabfile(stdin, 
			      szType,
			      szUserId,
			      szTime,
			      szCustName,
			      szCustNum,
			      szPlatform,
			      szLicType,
			      szLicDate,
			      szExpDate,
			      szLicKey,
			      szMachId,
			      szValCode))
	{
		char  sDay[20], sMonth[20], sTime[20], sYear[20];
		int nDom = 0;
		char *sMoy = NULL;
		int i;
		int rc;
			
		rowcnt++;
		if (rowcnt != 1)	/* Skip the header row */
		{
			/*
			** "Thu May 8 15:59:05 1992"
			*/
			rc = sscanf(szTime,"%s %s %d %s %s",
				    sDay, sMonth, &nDom, sTime, sYear);
			if (rc != 5)
			{
				fprintf(stderr, "[%s] sscanf() rc=%d\n", szTime, rc);
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
				fprintf(stderr, "[%s] Cannot convert month [%s]\n", szTime, sMonth);
				exit(1);
			}
			
			/*
			** "Thu May 8 15:59:05 1992"
			** "1992-05-08 15:59:05"
			*/
			sprintf(szTime, "%s-%s-%02d %s", sYear, sMoy, nDom, sTime);
		}
		
		printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
		       szType,
		       szUserId,
		       szTime,
		       szCustName,
		       szCustNum,
		       szPlatform,
		       szLicType,
		       szLicDate,
		       szExpDate,
		       szLicKey,
		       szMachId,
		       szValCode);
		
	}

	fprintf(stderr,"Processed %d lines\n", rowcnt);
	return 0;
	
}


