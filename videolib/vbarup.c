#include "video.h"
#include "vmenu.h"
#include "vlocal.h"
#include "vdata.h"

#define CLOCK_CODE -10
#define ZONE_CODE -11
#define CALENDAR_CODE -12
#define CALCULATOR_CODE -13
#define NOTEPAD_CODE -14
#define PUZZLE_CODE -15
#define PREFERENCE_CODE -16

vbarup()
{
	struct video_menu bar, help, goodies, exit;
	int choice, active;
	unsigned char *save, *vsss();

	save = vsss(0,0,1,vscr_wid);

	vinitmenu(&bar,BAR_MENU, REVERSE, 0, 50, 3);
	vmenuitem(&bar,"Help", 0, &help);
	vmenuitem(&bar,"Goodies", 0, &goodies);
	vmenuitem(&bar,"Exit", 0, &exit);

	vinitmenu(&help,0,0,0,0,0);
	vmenuitem(&help,"Sorry, on-line help is not currently available.",0,0);

	vinitmenu(&goodies,0,REVERSE,0,0,0);
	vmenuitem(&goodies,"Clock", CLOCK_CODE, 0);
	vmenuitem(&goodies,"Calculator", CALCULATOR_CODE, 0);
	vmenuitem(&goodies,"Calendar", CALENDAR_CODE, 0);
	vmenuitem(&goodies,"Note Pad", NOTEPAD_CODE, 0);
	vmenuitem(&goodies,"Preferences", PREFERENCE_CODE, 0);
	vmenuitem(&goodies,"Time Zones", ZONE_CODE, 0);
	vmenuitem(&goodies,"Puzzle", PUZZLE_CODE, 0);

	vinitmenu(&exit,0,0,0,0,0);
	vmenuitem(&exit,"Exit now!",-2,0);

	active = TRUE;
	while(active)
	{
		switch(choice = vmenu(&bar))
		{
			case CLOCK_CODE:	{ gclock(); break; }
			case CALENDAR_CODE:	{ gcalendar(); break; }
			case CALCULATOR_CODE:	{ gcalculator(); break; }
			case NOTEPAD_CODE:	{ gpreferences(); break; }
			case ZONE_CODE:		{ gzones(); break; }
			case PUZZLE_CODE:	{ gpuzzle(); break; }
			case PREFERENCE_CODE:	{ gpreferences(); break; }
			case -1:
			case -2:		{ active = FALSE; break; }
			default:
			{
				vre("mbarup-Internal error, invalid valud %d returned from vmenu.",choice);
				active = FALSE;
				break;
			}
		}
	}
	vrss(save);
}
