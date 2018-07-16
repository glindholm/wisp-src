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
static char lang_opts[]="(COBOL,C,TEXT,SHELL)";
static char specify1[]="Please specify the name of the file to be edited.";
static char specify2[]="When creating a new file, leave the file name blank.";
static char specify3[]="Please specify the library and volume for:";
static char source[]="Source files . . . . . . . . . .";
static char go_native[]="(Press PF5 to switch to native mode";
static char go_vs[]=    "(Press PF5 to switch to   VS   mode";
static char end_editor[]="Press PF16 to end Editor Processing)";



vse_input(emsg)
char *emsg;
{
	wpload();

	gppfkeys=GP_PF_05|GP_PF_16;
	wswap(&gppfkeys);

	GPSETUP();
	GPSTD("INPUT   ","EDITOR");
	GPCTEXT(logo,9,2);
	GPKW("LANGUAGE",vse_gp_input_language,9,11,2,"A");
	GPCTEXT(lang_opts,11,23);
	GPCTEXT(specify1,14,24);
	GPCTEXT(specify2,15,24);
	GPCTEXT(emsg,16,2);
	if(vse_native)
		{
		GPSYSNAME(vse_sysname,17);
		GPCTEXT(go_vs,24,2);
		}
	else
		{
		GPCTEXT(specify3,17,2);
		GPFILE(vse_gp_input_file,14,2);
		GPEXT(vse_gp_input_ext,15,2);
		GPCTEXT(source,18,5);
		GPLIB(vse_gp_input_library,18,38);
		GPVOL(vse_gp_input_volume,18,61);
		GPCTEXT(go_native,24,2);
		}
	GPCTEXT(end_editor,24,40);
	GPENTER();
	GPPFS(&gppfkeys);

	return(vse_input_pick = display_and_read_gp());
}

