#include "wperson.h"

ONHELP()									/* Turn help on.				*/
{
	defaults.flags |= HELP_ENABLED;
}

NOHELP()									/* Turn help off.				*/
{
	defaults.flags &= (~HELP_ENABLED);
}
