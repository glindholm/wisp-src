/*----
This is the initial input GETPARM screen. if vse_native is true 
it displays the getparm as a system_name style getparm,
otherwise it uses a std wang style input.
It is passed an error message.
It returns the value of the pressed pfkey
------*/
#include "vseglb.h"
#include "vsegp.h"

static char logo[]="Program Development Editor v 1.00";
static char specify1[]="Please specify the name of the file to be created.";
static char end_editor[]="(Press PF16 to end Create Processing)";

/*----
This GETPARM displays in two possible ways depnding on vse_native
global. See vsegp.h for the macros
------*/
vse_create(emsg)
char *emsg;
{
	wpload();

	gppfkeys=GP_PF_16;
	wswap(&gppfkeys);

	GPSETUP();
	GPSTD("OUTPUT  ","EDITOR");
	GPCTEXT(logo,9,2);
	GPCTEXT(specify1,14,2);
	GPCTEXT(emsg,16,2);
	if(vse_native)
		{
		GPSYSNAME(vse_sysname,17);
		}
	else
		{
		GPFILE(vse_gp_input_file,15,2);
		GPEXT(vse_gp_input_ext,16,2);
		GPLIB(vse_gp_input_library,15,38);
		GPVOL(vse_gp_input_volume,15,61);
		}
	GPCTEXT(end_editor,24,30);
	GPENTER();
	GPPFS(&gppfkeys);

	return(vse_create_pick = display_and_read_gp());
}

