wisp_4402g

Fixing problem with $ in path names.
If a shell command is issued the $ tries to be macro expanded.

Customer State Street has problem with SORT failing to
unload the indexed file using MF "rebuild".

Problem can effect:
sorting
printing
submitting
linking

Routines:
[x] run_unixcommand_silent()
[x] wsystem()
[x] wsystem_interactive()
[o] wsystem_standalone() - windows only

File:
[x] wfcisam.c
[x] link.c
[x] rename.c
[x] wfvision.c
[x] wprint.c
[x] mngfile.c
[o] wshelp.c -- checked OK



