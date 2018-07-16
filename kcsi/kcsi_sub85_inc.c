/*
**	Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
**	$Id:$
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

int	call_kcsi_create_main();
int	call_kcsio();
int	call_kdisp();
int	call_ctrlvers();
int	call_rptvers();
int	call_rptcall();
int	call_kformat();
int	call_inqvers();
int	call_parseinq();
int	call_geninq();
int	call_datvers();
int	call_dbsc();
int	call_inidio();
int	call_dmnt();
int	call_kexists();
int	call_valnam();
int	call_valvol();
int	call_valfnm();
int	call_rptwop();
int	call_kmatch();
int	call_kfiltyp();
int	call_ktrace();

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

	{ "KCSI_CREATE_MAIN",	call_kcsi_create_main},
	{ "KCSIO",	call_kcsio},
	{ "KDISP",	call_kdisp},
	{ "CTRLVERS",	call_ctrlvers},
	{ "RPTVERS",	call_rptvers},
	{ "RPTCALL",	call_rptcall},
	{ "KFORMAT",	call_kformat},
	{ "INQVERS",	call_inqvers},
	{ "GENINQ",     call_geninq},
	{ "PARSEINQ",	call_parseinq},
	{ "DATVERS",    call_datvers},
	{ "DBSC",	call_dbsc},
	{ "INIDIO",	call_inidio},
	{ "DMNT",       call_dmnt},
	{ "KEXISTS",	call_kexists},
	{ "VALVOL",	call_valvol},
	{ "VALNAM",	call_valnam},
	{ "VALFNM",	call_valfnm},
	{ "RPTWOP",	call_rptwop},
	{ "KMATCH",	call_kmatch},
	{ "KFILTYP",	call_kfiltyp},
	{ "KTRACE",	call_ktrace},

/*********************************************************************/
#endif /*  KCSI_SUB85_LIBTABLE */

#ifdef KCSI_SUB85_ROUTINES
/*********************************************************************/

/*----
Call to file io routine kcsi_create_main();
------*/
int call_kcsi_create_main( name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int 		initial;
{
	kcsi_create_main();
	return Okay;
}

/*----
Call to file io routine KCSIO(*IO_BLOCK, *UFB, *record);
UFB passed but not used in vision 3
------*/
int call_kcsio( name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int 		initial;
{
	KCSIO(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

/*----
INIDIO(*IO_BLOCK)
------*/
int call_inidio(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	INIDIO(args[0].a_address);
	return Okay;
}

/*----
KDISP(*file_sys_name,*type,*return_code)
------*/
int call_kdisp(name, num_args, args, initial)
char 	*name;
int	num_args;
Argument	args[];
int	initial;
{
	KDISP(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}
/*----
KFORMAT(*decimals,*receiver,*RFL)
-------*/

int call_kformat(name, num_args, args, initial)
char 	*name;
int	num_args;
Argument	args[];
int	initial;
{
	KFORMAT(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

/*----
All versions are passed a receiving string for a formatted version
and logo.
CTRLVERS(*vers)
------*/
int call_ctrlvers(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	CTRLVERS(args[0].a_address,args[1].a_address);
	return Okay;
}

/*----
RPTVERS(char *VERS, char *A-BINARY, char *STYLE)
------*/
int call_rptvers(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	RPTVERS(args[0].a_address,args[1].a_address, args[2].a_address);
	return Okay;
}

int call_inqvers(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	INQVERS(args[0].a_address,args[1].a_address);
	return Okay;
}

int call_datvers(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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
int call_rptcall(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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
int call_parseinq(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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
int call_geninq(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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

int call_dbsc(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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

int call_dmnt(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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

int call_valnam(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	VALNAM(args[0].a_address,
		args[1].a_address);
	return Okay;
}

/*----
VALVOL(*VOL ,*RC);
------*/

int call_valvol(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	VALVOL(args[0].a_address,
		args[1].a_address);
	return Okay;
}

/*----
VALFNM(*FIELDNAME ,*RC);
------*/

int call_valfnm(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	VALFNM(args[0].a_address,
		args[1].a_address);
	return Okay;
}

/*----
KEXISTS(*RC,*FILE, *LIBm, *VOL)
------*/

int call_kexists(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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

int call_rptwop(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	RPTWOP(args[0].a_address,
		args[1].a_address,
		args[2].a_address);
	return Okay;
}


/*----
KMATCH(*rc,*general-field-1, *general-field-2)
------*/
int call_kmatch(name, num_args, args, initial)
char 	*name;
int	num_args;
Argument	args[];
int	initial;
{
	KMATCH(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

int call_kfiltyp(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	KFILTYP(args[0].a_address);
	return Okay;
}

/*----
KTRACE(*str[80])
------*/

int call_ktrace(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
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
