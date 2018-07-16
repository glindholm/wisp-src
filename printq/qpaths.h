/*
 * header:  queue_paths
 * Program: IDSIprint
 * Purpose: declarations of path vars
 *
 */
extern char spooldir[];
extern char pfiledir[];
extern char initdir[];
extern char dumpdir[];
extern char qdumpfile[];
extern char pdumpfile[];
extern char cdumpfile[];
extern char oldqdump[];
extern char pstatusd[];
extern char printcap[];
extern char prtdef[];
extern char formdef[];
extern char classdef[];
extern char ilpdef[];
extern char remdir[];

extern char errlogfile[];
extern char olderrlogfile[];
extern char daemonpid[];

extern char *shmkeypaths[];
extern char semkeyfile[];
extern char prtkeyfile[];

extern char oldlockfile[];
extern char oldlocktmp[];
extern char lockfilep[];

#ifdef MQCODE
extern char mqpath[];
#endif
#ifdef SOCKCODE
extern char socketpath[];
#endif
