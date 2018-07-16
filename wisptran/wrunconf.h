/*
	wrunconfig.h
*/

#ifndef DEF_WRUNCONFIG
#define DEF_WRUNCONFIG

#define WRUNOPTIONS_ENV "WRUNOPTIONS"
#define WRUNCONFIG	"wrunconfig"

struct wruncfg
{
	char	wrun_options[80];
	char	wrun_runcbl[80];
	char	wrun_cobtype[10];
};

#endif

