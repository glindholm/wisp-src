/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
**
******************************************************************************
*/

/*----
Entry routines for support for CONTROL REPORT INQUIRY and 
DATENTRY. Acu-COBOL version 2.00 using Vision version 3 files.
------*/

/*----
Call to file io routine KCSIO(*IO_BLOCK, *UFB, *record);
UFB passed but not used in vision 3
------*/
call_kcsio( name, num_args, args, initial)
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
call_inidio(name, num_args, args, initial)
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
call_kdisp(name, num_args, args, initial)
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

call_kformat(name, num_args, args, initial)
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
call_ctrlvers(name, num_args, args, initial)
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
call_rptvers(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	RPTVERS(args[0].a_address,args[1].a_address, args[2].a_address);
	return Okay;
}

call_inqvers(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	INQVERS(args[0].a_address,args[1].a_address);
	return Okay;
}

call_datvers(name, num_args, args, initial)
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
call_rptcall(name, num_args, args, initial)
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
call_parseinq(name, num_args, args, initial)
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
call_geninq(name, num_args, args, initial)
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

call_dbsc(name, num_args, args, initial)
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

call_dmnt(name, num_args, args, initial)
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

call_valnam(name, num_args, args, initial)
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

call_valvol(name, num_args, args, initial)
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

call_valfnm(name, num_args, args, initial)
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

call_kexists(name, num_args, args, initial)
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

call_rptwop(name, num_args, args, initial)
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
call_kmatch(name, num_args, args, initial)
char 	*name;
int	num_args;
Argument	args[];
int	initial;
{
	KMATCH(args[0].a_address,args[1].a_address,args[2].a_address);
	return Okay;
}

call_kfiltyp(name, num_args, args, initial)
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

call_ktrace(name, num_args, args, initial)
char		*name;
int		num_args;
Argument	args[];
int		initial;
{
	KTRACE(args[0].a_address);
	return Okay;
}

/*
**	History:
**	$Log: crid85.c,v $
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  1996/09/18 18:34:33  gsl
**	Fix syntax error
**	
**	Revision 1.2  1996-09-17 16:45:28-07  gsl
**	drcs update
**
**
**
*/
