/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
** $Author: gsl $
**
**
******************************************************************************
*/


/*
**	File:		wauthdata.c
**
**	Purpose:	Generate SQL to load the wauthorize data into SQL database.
**
**
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>
#ifdef unix
#include <grp.h>
#endif
#ifdef WIN32
#include <io.h>
#endif

#include "wlicense.h"
#include "prompt.h"
#include "platsubs.h"
#include "idsisubs.h"
#include "wisplib.h"




static const char* tabfilepath()
{
	static int first=1;
	static char	tabfile[256];

	/*
	**	Create tab file name by replacing .log with .tab
	*/
	if (first)
	{
		first = 0;

		strcpy(tabfile,authlogfile());
		tabfile[strlen(tabfile)-4] = '\0';
		strcat(tabfile,".tab");
	}

	return tabfile;
}

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

	if (NULL == fgets(buff, sizeof(buff), fp))
	{
		return 1;
	}
	buff[strlen(buff)-1] = '\0';

	bptr = buff;
	for(i=0; i<NUMARGS; i++)
	{
		if (eptr = strchr(bptr,'\t'))
		{
			*eptr = '\0';
		}
		strcpy(args[i],bptr);

		if (NULL==eptr)
		{
			break;
		}
		
		bptr = eptr+1;
	}
	
	return 0;
}


/*
**	DUMMY routines to prevent the whole WISPLIB from being included
*/
#include "wutils.h"



int main(int argc, char* argv[])
{
	FILE 	*fpTabFile = NULL;
	FILE 	*fpSqlFile = NULL;
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

	int	rownum = 0;
	int	nLicKeyCnt = 0;
	int	nValCodeCnt = 0;
	
	char	szRawLicenseKey[20];
	int4	nSplitCustNum;
	char	szSplitPlatformCode[3];
	int	nSplitLicenseTypeCode;
	char	szSplitLicenseDate[20];
	char	szSplitExpDate[20];
	char	szFormatLicenseDate[40];
	char	szFormatExpDate[40];

	int	nCustNum;

	fpTabFile = fopen(tabfilepath(),"r");
	if (!fpTabFile)
	{
		fprintf(stderr, "Unable to open tab file %s\n",tabfilepath());
		return 1;
	}
	fpSqlFile = fopen("wauthorize.sql","w");
	if (!fpSqlFile)
	{
		fprintf(stderr, "Unable to open wauthorize.sql file\n");
		return 1;
	}
	fprintf(fpSqlFile, "-- wauthorize.sql: load wauthorize data\n\n");

	fprintf(fpSqlFile, "BEGIN TRANSACTION\n");
	fprintf(fpSqlFile, "DELETE utb_license_docs\n");
	fprintf(fpSqlFile, "DELETE utb_validation_codes\n");
	fprintf(fpSqlFile, "DELETE utb_license_keys\n");
	fprintf(fpSqlFile, "DELETE utb_contacts\n");
	fprintf(fpSqlFile, "DELETE utb_customers\n");
	fprintf(fpSqlFile, "COMMIT TRANSACTION\n");
	fprintf(fpSqlFile, "GO\n\n");
	
	while( 0==readtabfile(fpTabFile, 
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
		rownum++;
		
		/*
			Type = LICKEY || VALCODE (|| TYPE 1st record)
		*/
		if (1 == rownum && 0==strcmp(szType, "TYPE"))
		{
			fprintf(stderr, "Discarding header record\n");
			continue;
		}

		fprintf(fpSqlFile, "-- %d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
				rownum,
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
		fprintf(fpSqlFile, "BEGIN TRANSACTION\n");
		fprintf(fpSqlFile, "DECLARE @rowcount AS INT\n\n");

		{ /* Replace " with ' */
			char *p = szCustName;
			while(p = strchr(p, '\"'))
			{
				*p = '\'';
			}

		}

		unformatkey(szRawLicenseKey,szLicKey);

		if (bklickey(&nSplitCustNum,szSplitPlatformCode,&nSplitLicenseTypeCode,szSplitLicenseDate,szSplitExpDate,szRawLicenseKey))
		{
			fprintf(stderr, "Invalid LICENSE KEY [%s]\n", szLicKey);
			return 1;
		}

		sprintf(szFormatLicenseDate,"'%4.4s/%2.2s/%2.2s'",&szSplitLicenseDate[0],&szSplitLicenseDate[4],&szSplitLicenseDate[6]);

		if (0 == memcmp(szSplitExpDate,"00000000",8))					/* format the date				*/
		{
			strcpy(szFormatExpDate,"NULL");
		}
		else
		{
			/*
				The ExpDate is generated and not validated to need to correct.
				30 days has sept, april, june, and nov.
			*/
			int nMonth = ((szSplitExpDate[4] - '0') * 10) + (szSplitExpDate[5] - '0');
			int nDay   = ((szSplitExpDate[6] - '0') * 10) + (szSplitExpDate[7] - '0');
			
			switch( nMonth )
			{
			case 2:	/* Feb */
				if (nDay > 28)
				{
					szSplitExpDate[6] = '2';
					szSplitExpDate[7] = '8';
				}
				break;
			case 4: /* April */
			case 6: /* June */
			case 9: /* Sept */
			case 11:/* Nov */
				if (nDay > 30)
				{
					szSplitExpDate[6] = '3';
					szSplitExpDate[7] = '0';
				}
				break;
			}

			sprintf(szFormatExpDate,"'%4.4s/%2.2s/%2.2s'",&szSplitExpDate[0],&szSplitExpDate[4],&szSplitExpDate[6]);
		}

		if (0==strcmp(szType, "LICKEY"))
		{
			nLicKeyCnt++;
			/*
			- add/update customer 
			SELECT @rowcount = COUNT(*) 
			FROM utb_customers
			WHERE cust_number = szCustNum

			IF @rowcount = 0
			BEGIN
				INSERT INTO utb_customers ( [cust_number], [cust_name])
				VALUES ( '##', 'name')
			END
			ELSE
			BEGIN
				UPDATE utb_customers
				SET [cust_name] = 'name'
				WHERE [cust_number] = 'num'
			END
 			*/
			nCustNum = atoi(szCustNum);
			fprintf(fpSqlFile, "SELECT @rowcount = COUNT(*)\n");
			fprintf(fpSqlFile, "  FROM utb_customers\n");
			fprintf(fpSqlFile, "  WHERE cust_number = '%06d'\n", nCustNum);
			fprintf(fpSqlFile, "IF @rowcount = 0\n");
			fprintf(fpSqlFile, "BEGIN\n");
			fprintf(fpSqlFile, "  INSERT INTO utb_customers ( [cust_number], [cust_name])\n");
			fprintf(fpSqlFile, "  VALUES ( \"%06d\", \"%s\")\n", nCustNum, szCustName);
			fprintf(fpSqlFile, "END\n");
			fprintf(fpSqlFile, "ELSE\n");
			fprintf(fpSqlFile, "BEGIN\n");
			fprintf(fpSqlFile, "  UPDATE utb_customers\n");
			fprintf(fpSqlFile, "  SET [cust_name] = \"%s\"\n", szCustName);
			fprintf(fpSqlFile, "  WHERE [cust_number] = \"%06d\"\n", nCustNum);
			fprintf(fpSqlFile, "END\n");

			/*
			- verify/add license-key
			SELECT @rowcount = COUNT(*) 
			FROM utb_license_keys
			WHERE license_key = szLicKey

			IF @rowcount = 0
			BEGIN
				INSERT INTO utb_license_keys ( .... )
				VALUES ( .... )
			END
 			*/

			fprintf(fpSqlFile, "SELECT @rowcount = COUNT(*)\n");
			fprintf(fpSqlFile, "  FROM utb_license_keys\n");
			fprintf(fpSqlFile, "  WHERE license_key = '%s'\n", szLicKey);
			fprintf(fpSqlFile, "IF @rowcount = 0\n");
			fprintf(fpSqlFile, "BEGIN\n");
			fprintf(fpSqlFile, "  INSERT INTO utb_license_keys ( [license_key], [cust_number], [platform_code], [license_type_code], [license_date], [expiration_date])\n");
			fprintf(fpSqlFile, "  VALUES ( '%s', '%06d', '%2.2s', '%d', %s, %s)\n", 
							szLicKey, 
							nSplitCustNum, 
							szSplitPlatformCode, 
							nSplitLicenseTypeCode,
							szFormatLicenseDate,
							szFormatExpDate);
			fprintf(fpSqlFile, "END\n");

			/*
			- Add the doc
			*/
			fprintf(fpSqlFile, "INSERT INTO utb_license_docs ( [doc_type], [licensee], [cust_number], [license_key], [platform_name], [license_type_name], [license_date], [expiration_date], [operator], [create_date])\n");
			fprintf(fpSqlFile, "VALUES ( '%s', \"%s\", '%06d', '%s', '%s', '%s', %s, %s, '%s', '%4.4s %15.15s')\n", 
							szType,
							szCustName,
							nSplitCustNum, 
							szLicKey, 
							&szPlatform[5], 
							lictypename(nSplitLicenseTypeCode),
							szFormatLicenseDate,
							szFormatExpDate,
							szUserId,
							&szTime[20], &szTime[4]);

		}
		else if (0==strcmp(szType, "VALCODE"))
		{
			nValCodeCnt++;

			/*
			- verify/add validation code
			SELECT @rowcount = COUNT(*) 
			FROM utb_validation_codes
			WHERE license_key = szLicKey
			AND machine_id = szMachId

			IF @rowcount = 0
			BEGIN
				INSERT INTO utb_validation_codes ( .... )
				VALUES ( .... )
			END
 			*/

			fprintf(fpSqlFile, "SELECT @rowcount = COUNT(*)\n");
			fprintf(fpSqlFile, "  FROM utb_validation_codes\n");
			fprintf(fpSqlFile, "  WHERE license_key = '%s'\n", szLicKey);
			fprintf(fpSqlFile, "  AND machine_id = '%s'\n", szMachId);
			fprintf(fpSqlFile, "IF @rowcount = 0\n");
			fprintf(fpSqlFile, "BEGIN\n");
			fprintf(fpSqlFile, "  INSERT INTO utb_validation_codes ( [license_key], [machine_id], [validation_code])\n");
			fprintf(fpSqlFile, "  VALUES ( '%s', '%s', '%s')\n", 
							szLicKey, 
							szMachId, 
							szValCode);
			fprintf(fpSqlFile, "END\n");

			/*
			- Add the doc
			*/
			fprintf(fpSqlFile, "INSERT INTO utb_license_docs ( [doc_type], [licensee], [cust_number], [license_key], [platform_name], [license_type_name], [license_date], [machine_id], [validation_code], [operator], [create_date])\n");
			fprintf(fpSqlFile, "VALUES ( '%s', \"%s\", '%06d', '%s', '%s', '%s', %s, '%s', '%s', '%s', '%4.4s %15.15s')\n", 
							szType,
							szCustName,
							nSplitCustNum, 
							szLicKey, 
							&szPlatform[5], 
							lictypename(nSplitLicenseTypeCode),
							szFormatLicenseDate,
							szMachId,
							szValCode,
							szUserId,
							&szTime[20], &szTime[4]);
		}
		else /* ERROR */
		{
			fprintf(stderr, "Unknown record type [%s]\n", szType);
			return 1;
		}
		fprintf(fpSqlFile, "COMMIT TRANSACTION\n");
		fprintf(fpSqlFile, "GO\n\n");

	}
	fprintf(fpSqlFile, "BEGIN TRANSACTION\n");
	fprintf(fpSqlFile, "DECLARE @rowcount AS INT\n\n");
	fprintf(fpSqlFile, "SELECT @rowcount = COUNT(*)\n");
	fprintf(fpSqlFile, "  FROM utb_license_docs\n");
	fprintf(fpSqlFile, "IF @rowcount = %d\n", nLicKeyCnt+nValCodeCnt);
	fprintf(fpSqlFile, "BEGIN\n");
	fprintf(fpSqlFile, "  PRINT 'Successfully loaded %d license documents'\n", nLicKeyCnt+nValCodeCnt);
	fprintf(fpSqlFile, "END\n");
	fprintf(fpSqlFile, "ELSE\n");
	fprintf(fpSqlFile, "BEGIN\n");
	fprintf(fpSqlFile, "  RAISERROR ('Expecting %d license documents but found %%d.', 16, 1, @rowcount)\n", nLicKeyCnt+nValCodeCnt);
	fprintf(fpSqlFile, "END\n");
	fprintf(fpSqlFile, "COMMIT TRANSACTION\n");
	fprintf(fpSqlFile, "GO\n\n");


	fclose(fpTabFile);
	fclose(fpSqlFile);
	fprintf(stderr, "Processed %5d records\n",rownum);
	fprintf(stderr, " %5d License Keys\n",nLicKeyCnt);
	fprintf(stderr, " %5d Validation Codes\n",nValCodeCnt);
    
	return 0;
}



/*
**	History:
**	$Log: wauthdata.c,v $
**	Revision 1.2  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.1  2002/08/12 14:21:06  gsl
**	no message
**	
**
**
*/
