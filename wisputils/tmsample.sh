# Test script to call TEXTMENU with the tmsample.mnu file.

#  To be compatible with COBOL the shell variables must be
#  declared as indicated.

#  First specify the location of the .mnu file to use.

#                  Needs to be 8 char long.
RFILE="tmsample"
#                  Needs to be 8 char long.
RLIB="SEAOBJ  "
#                  Needs to be 6 char long.
RVOL="VOL444"

#   Note if the COMPANY symbol is left as 40 spaces then the two line
#   header for the menu will not be used, otherwise TEXTMENU will use
#   the indicated name in the two line header.
#                  Needs to be 40 chars long.
COMPANY="  International Digital Scientific, Inc."

wrun TEXTMENU using $RFILE $RLIB $RVOL "$COMPANY"
