/*
**	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
**	KCSI fragments for include into sub85.c 
**
**	This file is divided into three fragments, each is enclosed 
**	within #ifdef/#endif
**
**	KCSI_SUB85_HEADER	- Header info
**	KCSI_SUB85_LIBTABLE	- LIBTABLE entries
**	KCSI_SUB85_ROUTINES	- Supporting routines
*/

#ifdef KCSI_SUB85_HEADER
/*********************************************************************/

int	call_kcsi_create_main(char* name, int num_args, Argument args[], int initial, ...);
int	call_kcsio(char* name, int num_args, Argument args[], int initial, ...);
int	call_kdisp(char* name, int num_args, Argument args[], int initial, ...);
int	call_ctrlvers(char* name, int num_args, Argument args[], int initial, ...);
int	call_rptvers(char* name, int num_args, Argument args[], int initial, ...);
int	call_rptcall(char* name, int num_args, Argument args[], int initial, ...);
int	call_kformat(char* name, int num_args, Argument args[], int initial, ...);
int	call_inqvers(char* name, int num_args, Argument args[], int initial, ...);
int	call_parseinq(char* name, int num_args, Argument args[], int initial, ...);
int	call_geninq(char* name, int num_args, Argument args[], int initial, ...);
int	call_datvers(char* name, int num_args, Argument args[], int initial, ...);
int	call_dbsc(char* name, int num_args, Argument args[], int initial, ...);
int	call_inidio(char* name, int num_args, Argument args[], int initial, ...);
int	call_dmnt(char* name, int num_args, Argument args[], int initial, ...);
int	call_kexists(char* name, int num_args, Argument args[], int initial, ...);
int	call_valnam(char* name, int num_args, Argument args[], int initial, ...);
int	call_valvol(char* name, int num_args, Argument args[], int initial, ...);
int	call_valfnm(char* name, int num_args, Argument args[], int initial, ...);
int	call_rptwop(char* name, int num_args, Argument args[], int initial, ...);
int	call_kmatch(char* name, int num_args, Argument args[], int initial, ...);
int	call_kfiltyp(char* name, int num_args, Argument args[], int initial, ...);
int	call_ktrace(char* name, int num_args, Argument args[], int initial, ...);

void kcsi_create_main();
void KCSIO();
void INIDIO();
void KDISP();
void KFORMAT();
void CTRLVERS();
void RPTVERS();
void INQVERS();
void DATVERS();
void RPTCALL();
void PARSEINQ();
void GENINQ();
void DBSC();
void DMNT();
void VALNAM();
void VALVOL();
void VALFNM();
void KEXISTS();
void KMATCH();
void KFILTYP();
void KTRACE();
void RPTWOP();

/*********************************************************************/
#endif /*  KCSI_SUB85_HEADER */

#ifdef KCSI_SUB85_LIBTABLE
/*********************************************************************/

	{ "KCSI_CREATE_MAIN",	call_kcsi_create_main, NULL },
	{ "KCSIO",	call_kcsio, NULL },
	{ "KDISP",	call_kdisp, NULL },
	{ "CTRLVERS",	call_ctrlvers, NULL },
	{ "RPTVERS",	call_rptvers, NULL },
	{ "RPTCALL",	call_rptcall, NULL },
	{ "KFORMAT",	call_kformat, NULL },
	{ "INQVERS",	call_inqvers, NULL },
	{ "GENINQ",     call_geninq, NULL },
	{ "PARSEINQ",	call_parseinq, NULL },
	{ "DATVERS",    call_datvers, NULL },
	{ "DBSC",	call_dbsc, NULL },
	{ "INIDIO",	call_inidio, NULL },
	{ "DMNT",       call_dmnt, NULL },
	{ "KEXISTS",	call_kexists, NULL },
	{ "VALVOL",	call_valvol, NULL },
	{ "VALNAM",	call_valnam, NULL },
	{ "VALFNM",	call_valfnm, NULL },
	{ "RPTWOP",	call_rptwop, NULL },
	{ "KMATCH",	call_kmatch, NULL },
	{ "KFILTYP",	call_kfiltyp, NULL },
	{ "KTRACE",	call_ktrace, NULL },

/*********************************************************************/
#endif /*  KCSI_SUB85_LIBTABLE */

#ifdef KCSI_SUB85_ROUTINES
/*********************************************************************/

/*----
Call to file io routine kcsi_create_main();
------*/
int call_kcsi_create_main(char* name, int num_args, Argument args[], int initial, ...)
{
	kcsi_create_main();
	return Okay;
}

/*----
Call to file io routine KCSIO(*IO_BLOCK, *UFB, *record);
UFB passed but not used in vision 3
------*/
int call_kcsio(char* name, int num_args, Argument args[], int initial, ...)
{
	KCSIO(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

/*----
INIDIO(*IO_BLOCK)
------*/
int call_inidio(char* name, int num_args, Argument args[], int initial, ...)
{
	INIDIO(args[0].a_address);
	return Okay;
}

/*----
KDISP(*file_sys_name,*type,*return_code)
------*/
int call_kdisp(char* name, int num_args, Argument args[], int initial, ...)
{
	KDISP(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}
/*----
KFORMAT(*decimals,*receiver,*RFL)
-------*/

int call_kformat(char* name, int num_args, Argument args[], int initial, ...)
{
	KFORMAT(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

/*----
All versions are passed a receiving string for a formatted version
and logo.
CTRLVERS(*vers)
------*/
int call_ctrlvers(char* name, int num_args, Argument args[], int initial, ...)
{
	CTRLVERS(args[0].a_address,args[1].a_address);
	return Okay;
}

/*----
RPTVERS(char *VERS, char *A-BINARY, char *STYLE)
------*/
int call_rptvers(char* name, int num_args, Argument args[], int initial, ...)
{
	RPTVERS(args[0].a_address,args[1].a_address, args[2].a_address);
	return Okay;
}

int call_inqvers(char* name, int num_args, Argument args[], int initial, ...)
{
	INQVERS(args[0].a_address,args[1].a_address);
	return Okay;
}

int call_datvers(char* name, int num_args, Argument args[], int initial, ...)
{
	DATVERS(args[0].a_address,args[1].a_address);
	return Okay;
}

/*----
RPTCALL(*RPT_OPTIONS,*RHD,*RDF,*RCB,*RCD,*RFL,*CRF,*RTT,*ST,*DL,*NF,
	*DATA1_IO_BLOCK,*DATA2_IO_BLOCK,
	*printer_file,*library,*volume,
	*caller,*kts)
------*/
int call_rptcall(char* name, int num_args, Argument args[], int initial, ...)
{
	RPTCALL(args[0].a_address,
		args[1].a_address,
		args[2].a_address,
		args[3].a_address,
		args[4].a_address,
		args[5].a_address,
		args[6].a_address,
		args[7].a_address,
		args[8].a_address,
		args[9].a_address,
		args[10].a_address,
		args[11].a_address,
		args[12].a_address,
		args[13].a_address,
		args[14].a_address,
		args[15].a_address,
		args[16].a_address,
		args[17].a_address);
	return Okay;
}

/*----
PARSEINQ(*RFL,*DL,*result,*scr_flds,*message,*return_code);
------*/
int call_parseinq(char* name, int num_args, Argument args[], int initial, ...)
{
	PARSEINQ(args[0].a_address,
		args[1].a_address,
		args[2].a_address,
		args[3].a_address,
		args[4].a_address,
		args[5].a_address);
	return Okay;
}

/*----
GENINQ(	*SHELL_IO_BLOCK,*display_opt,*run_opt,
	*DATA_IO_BLOCK,*change_data_files,*display_extract_option,
	*CONTROL_IO_BLOCK,*query_title,*query_lines,
	*EXTRACT_IO_BLOCK)
------*/
int call_geninq(char* name, int num_args, Argument args[], int initial, ...)
{
	GENINQ(args[0].a_address,
		args[1].a_address,
		args[2].a_address,
		args[3].a_address,
		args[4].a_address,
		args[5].a_address,
		args[6].a_address,
		args[7].a_address,
		args[8].a_address,
		args[9].a_address);
	return Okay;
}
/*----
DBSC(*CTRL_HEADERS,*CTRL_TABLE1,*CTRL_TABLE2,
	*CTRL_IO_BLOCK,*DATA_IO_BLOCK,*return_code)
------*/

int call_dbsc(char* name, int num_args, Argument args[], int initial, ...)
{
	DBSC(args[0].a_address,
		args[1].a_address,
		args[2].a_address,
		args[3].a_address,
		args[4].a_address,
		args[5].a_address);
	return Okay;
}
/*----
DMNT(*CTRL_HEADERS,*CTRL_TABLE1,*CTRL_TABLE2,
	*CTRL_IO_BLOCK,*DATA_IO_BLOCK,*menu_mode,*key_index)
------*/

int call_dmnt(char* name, int num_args, Argument args[], int initial, ...)
{
	DMNT(args[0].a_address,
		args[1].a_address,
		args[2].a_address,
		args[3].a_address,
		args[4].a_address,
		args[5].a_address,
		args[6].a_address);
	return Okay;
}

/*----
VALNAM(*NAME (or LIB) ,*RC);
------*/

int call_valnam(char* name, int num_args, Argument args[], int initial, ...)
{
	VALNAM(args[0].a_address,
		args[1].a_address);
	return Okay;
}

/*----
VALVOL(*VOL ,*RC);
------*/

int call_valvol(char* name, int num_args, Argument args[], int initial, ...)
{
	VALVOL(args[0].a_address,
		args[1].a_address);
	return Okay;
}

/*----
VALFNM(*FIELDNAME ,*RC);
------*/

int call_valfnm(char* name, int num_args, Argument args[], int initial, ...)
{
	VALFNM(args[0].a_address,
		args[1].a_address);
	return Okay;
}

/*----
KEXISTS(*RC,*FILE, *LIBm, *VOL)
------*/

int call_kexists(char* name, int num_args, Argument args[], int initial, ...)
{
	KEXISTS(args[0].a_address,
		args[1].a_address,
		args[2].a_address,
		args[3].a_address);
	return Okay;
}

/*----
RPTWOP(*RC,*OPTIONS, *MESSAGE)
------*/

int call_rptwop(char* name, int num_args, Argument args[], int initial, ...)
{
	RPTWOP(args[0].a_address,
		args[1].a_address,
		args[2].a_address);
	return Okay;
}


/*----
KMATCH(*rc,*general-field-1, *general-field-2)
------*/
int call_kmatch(char* name, int num_args, Argument args[], int initial, ...)
{
	KMATCH(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

int call_kfiltyp(char* name, int num_args, Argument args[], int initial, ...)
{
	KFILTYP(args[0].a_address);
	return Okay;
}

/*----
KTRACE(*str[80])
------*/

int call_ktrace(char* name, int num_args, Argument args[], int initial, ...)
{
	KTRACE(args[0].a_address);
	return Okay;
}

/*********************************************************************/
#endif /*  KCSI_SUB85_ROUTINES */

#undef KCSI_SUB85_HEADER
#undef KCSI_SUB85_LIBTABLE
#undef KCSI_SUB85_ROUTINES

/*
**	History:
**	$Log: kcsi_sub85_inc.c,v $
**	Revision 1.5  2011/10/30 01:16:55  gsl
**	Acu 9.1.0
**	
**	Revision 1.4  2011/10/22 00:06:47  gsl
**	Acu 9.1.0
**	
**	Revision 1.3  2010/01/16 02:16:22  gsl
**	new release
**	wisp 5.1.00
**	kcsi 4.2.00
**	
**	Revision 1.2  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.1  2002/10/21 20:47:26  gsl
**	KCSI Replacement for crid.h cridtbl.c and crid85.c
**	Includes CRID + CREATE cobol API's
**	
**
**
*/
