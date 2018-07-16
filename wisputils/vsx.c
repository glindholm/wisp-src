/*
 *  module: vsx_main.c
 * program: vsx
 *
 * $Log: vsx.c,v $
 * Revision 1.59  1993/11/08  19:04:57  jockc
 * rename loop var from open: to openloop:
 *
 * Revision 1.58  1993/10/14  17:28:20  jockc
 * fixed usage that was slightly wrong
 *
 * Revision 1.57  1993/10/14  17:23:57  jockc
 * change to use intdef.h.. also, fixed looping EOF thing
 *
 * Revision 1.56  1993/05/19  22:14:17  jockc
 * reinsert the code that lower cases the lib that was accidentally removed
 * in rev 1.55
 *
 * Revision 1.55  1992/12/01  22:52:50  jockc
 * added support for -9 debugging.. changed to print warning message if
 * user's tape contains INDEX files
 *
 * Revision 1.54  1992/08/20  21:01:50  jockc
 * added new logic to handle multi tape archives.  light testing.
 *
 * Revision 1.53  1992/08/17  20:42:54  jockc
 * first pass at multi-tape archives
 *
 * Revision 1.52  1992/06/11  16:57:42  jockc
 * removed more warnings
 *
 * Revision 1.51  1992/06/09  23:45:44  jockc
 * some changes to remove warnings from gcc and ansi C
 *
 * Revision 1.50  1992/06/04  22:32:20  jockc
 * att adds
 *
 * Revision 1.49  1992/05/21  22:49:37  jockc
 * prevent too-big blocks on NCR
 *
 * Revision 1.48  1992/04/09  17:48:04  jockc
 * pass arg to wait() for NCRE
 *
 * Revision 1.47  1992/01/22  22:34:05  jockc
 * fixup nasty 8-mil/9-track-reel bug whereby
 * data in files is repeated.. also added code
 * to do -tmp file processing
 *
 * Revision 1.46  1991/12/06  23:05:31  jockc
 * more null to space stuff..
 *
 * Revision 1.45  1991/12/06  22:11:49  jockc
 * added linefeeds for normal files, null check
 *
 * Revision 1.44  1991/12/04  21:02:15  jockc
 * fixed 2k block thing for normal files
 *
 * Revision 1.43  1991/12/03  21:00:30  jockc
 * write proper number of bytes for source files which have been trimmed to
 * less than recsz
 *
 * Revision 1.42  1991/11/26  00:27:28  jockc
 * added code for 2k blocking anomoly
 *
 * Revision 1.41  1991/11/21  18:14:39  jockc
 * changed fprintf to fwrite on output in postprocess
 *
 * Revision 1.40  1991/11/21  01:06:49  jockc
 * add lf/nolf logic
 *
 * Revision 1.39  1991/11/18  19:41:36  jockc
 * fixup extra junk at end of med block for fixed files
 *
 * Revision 1.38  1991/11/08  19:39:08  jockc
 * bug fixs: index mode trying to write tmp header on compressed files
 * archive end not detected on cartridge due to 8 mil logic reading past
 * end of tape file mark
 *
 * Revision 1.37  1991/09/06  22:08:47  jockc
 * rewrote 8 mil load stuff, comp and norm records handled
 * differently.  fixes skipped files thing.
 *
 * Revision 1.36  1991/09/05  00:20:24  jockc
 * big changes.  rewrote big portions of load() (again)
 * took out daemon aspect.  readtape now handled in main image.  readtape
 * handles 8 mil.  added 8 mil flag. updated usage().  massive cleanup
 * of unused code and variables.  some work still to be done there.
 * seperated decomp of records from load phase, moved decomp to
 * postprocessing phase.  fork before decomp to tolerate corrupt files.
 *
 * Revision 1.35  1991/04/30  00:59:41  jockc
 * fixed mis's bug.  index files were screwing things up, so
 * I hacked around it.  Index files are now skipped.  (that is
 * org byte & 0x03 == 0x02 means index file)
 *
 * Revision 1.34  1991/04/23  00:13:06  jec
 * fixed harte bug.  uninitialized var next in bldrec
 * was sending us into comp0 state at loops end
 *
 * Revision 1.1  1991/04/18  23:35:12  jockc
 * Initial revision
 *
 *
 * ---old sccs log follows---
 * 
 * D 1.32 91/03/13 13:56:42 root 32 31  00096/00067/00695
 * improved debugging, fixup enator tape big/eof thing, file org wrong byte
 * 
 * D 1.31 91/02/04 10:24:18 root 31 30  00003/00002/00759
 * cleaned up handling at end of tape if no eof mark
 * 
 * D 1.30 91/02/04 09:33:48 root 30 29  00001/00001/00760
 * D 1.29 91/02/04 09:04:34 root 29 28  00002/00005/00759
 * put verbose info back into no eof handler
 * 
 * D 1.28 91/02/03 11:16:06 root 28 27  00019/00002/00745
 * fixed datavantage extra bytes thing
 * 
 * D 1.27 91/02/01 16:05:39 root 27 26  00059/00025/00688
 * fixed aptech bug (bldrec state table.. needed to allow for NEXTCOMP instead of COMP0 )
 * fixed 2/4 byte reccnt bug
 * 
 * D 1.26 91/01/25 10:48:54 root 26 25  00001/00001/00712
 * fixed bug that appeared on SCO box.. shmat checked for less
 * than zero instead of -1
 * 
 * D 1.25 91/01/07 17:38:26 root 25 24  00009/00007/00704
 * fixed bug if big block ends right before header and grabs
 * two too many bytes (cnss tape)
 * 
 * D 1.24 91/01/07 15:30:50 jec 24 23   00002/00002/00709
 * fixed print of date
 * 
 * D 1.23 91/01/07 15:28:49 jec 23 22   00001/00001/00710
 * trying to fix auto print of version and date
 * 
 * D 1.22 91/01/07 15:22:04 jec 22 21   00024/00013/00687
 * changed printing of Noncompressed/Compressed in listing
 * added support for rectype variable
 * 
 * D 1.21 91/01/04 15:19:57 root 21 20  00013/00008/00687
 * cleaned up interactive prompting
 * 
 * D 1.20 91/01/04 14:50:29 root 20 19  00042/00007/00653
 * better handling of quit signal
 * 
 * D 1.19 91/01/04 13:02:19 root 19 18  00015/00003/00645
 * added experimental auto type detect (compressed vs noncompressed)
 * and auto source/data detect
 * 
 * D 1.18 90/12/04 19:57:07 root 18 17  00009/00002/00639
 * fixed null term record bug
 * 
 * D 1.17 90/12/04 19:01:24 root 17 16  00024/00008/00617
 * fixed bug: if get ffff0090 when expecting
 * med count, set eofflag and continue reading
 * 
 * D 1.16 90/12/04 13:44:38 root 16 15  00001/00001/00624
 * fixed bug in verbose printf listing
 * 
 * D 1.15 90/12/04 13:40:26 root 15 14  00070/00038/00555
 * made changes to support non compressed records.. need to test
 * 
 * D 1.14 90/12/03 16:34:19 root 14 13  00044/00012/00549
 * added code to support data files (not trimmed, different extensions
 * for source vs normal seq files.. default extensions added)
 * 
 * D 1.13 90/12/03 14:08:46 root 13 12  00005/00003/00556
 * more asthetic changes, and added copyright info
 * 
 * D 1.12 90/12/03 14:04:31 root 12 11  00003/00001/00556
 * more asthetic changes
 * 
 * D 1.11 90/12/03 14:00:28 root 11 10  00023/00000/00534
 * added usage(), fixed default archive, some asthetic fixes
 * 
 * D 1.10 90/12/03 13:37:49 root 10 9   00002/00002/00532
 * fixed trim code. was killing column 72-80. changed to kill 73-80, (and kill spaces)
 * 
 * D 1.9 90/11/20 11:32:17 root 9 8     00034/00335/00500
 * 2nd rewrite of load()... port to rs6000
 * 
 * D 1.8 90/11/20 10:13:38 root 8 7     00284/00058/00551
 * rewrote load, rewriting again
 * 
 * D 1.7 90/11/15 17:07:43 root 7 6     00136/00027/00473
 * finished load(), etc
 * 
 * D 1.6 90/11/14 18:39:32 root 6 5     00159/00013/00341
 * added most of load()
 * 
 * D 1.5 90/11/13 17:24:31 root 5 4     00171/00020/00183
 * improved daemon, load, buildrec...
 * need to finish load
 * 
 * D 1.4 90/11/13 12:47:17 root 4 3     00121/00006/00082
 * added daemon(), started load().. misc other funcs
 * 
 * D 1.3 90/11/12 16:19:39 root 3 2     00075/00000/00013
 * more stuff added, parseopts and matchopt
 * 
 * D 1.2 90/11/12 10:48:35 root 2 1     00004/00001/00009
 * testing sccs... 
 *
 * D 1.1 90/11/12 10:43:41 root 1 0     00010/00000/00000
 * date and time created 90/11/12 10:43:41 by root
 *
 */

#ifndef lint
static char copyright[] = "Copyright 1991 International Digital Scientific, Inc. All Rights Reserved.";
#endif
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

#include <stdio.h>
#include <fcntl.h>
#include "vsx.h"
#include "idsistd.h"

int tape_fd = -1;
int tapebytes =0;

main(argc,argv)
int argc;
char **argv;
{
        char *p,*e,*strchr();
	void handler();
	
        memset(VERSION,0,sizeof(VERSION));
        p=strchr(rcsid,' ');
        p=strchr(++p,' ');
        e=strchr(++p,' ');
        
        memcpy(VERSION,p,e-p);
        memset(MODDATE,0,sizeof(MODDATE));
        p=strchr(++e,' ');
        memcpy(MODDATE,e,p-e);

        setbuf(stderr,NULL);
        
        if (argc==2 && argv[1][0]=='-' && (argv[1][1]=='?'||argv[1][1]=='h')) usage();
        else
          fprintf(stderr,"VSX Release V%s (c)IDSI %s - 'vsx -h' for help\n",VERSION,MODDATE);

        setord();
        savemode();
        
        parseopts(argc,argv);
	if (eightmilx)
	  eightmil = TRUE;
	
	if (process_tmps)
	{
		arg_list = ++argv;
		postprocess();
		handler(0);
	}
        setdebuginfo();
        if (debug_level) { printf("debug level %d: files %d thru %d\n",debug_level,debug_start,debug_end);}

	if (strlen(norwd_file))
	{
		fprintf(stderr,"\nArchive file: %s\n",norwd_file);
	}
	else
	{
		fprintf(stderr,"\nArchive file: %s\n",arch_file);
	}

        signal(SIGINT,  handler );
        signal(SIGQUIT, handler );
        signal(SIGILL,  handler );
        signal(SIGEMT,  handler );
        signal(SIGBUS,  handler );
        signal(SIGSEGV, handler );
        load();
        printf("end of archive reached..\npostprocessing:\n");
        postprocess();
        handler( 0 );
        /*NOTREACHED*/
}
void handler(sig)
int sig;
{
        fprintf(stderr,"\nVSX exiting.\n");
	if (tape_fd != -1)
		close(tape_fd);
        exit(0);
}
readtape(buf,bytesrq,command)
unsigned char buf[];
int bytesrq,command;
{
        static int bufpos = 0;
        int stat, bytes;
        static int tapepos=0, file8num=0, readzero=0;
	static int fileseq= -1;
	static int eots=0;
	
DD{
fprintf(stderr,"readtape(buf,bytesrq=%d,mode=%s) tapebytes=%d, bufpos=%d/%d\n",
        bytesrq,
	command==RT_SKIPTOEND?"skip to end":
	command==RT_GETBLOCK?"get block":
	command==RT_GETBYTES?"get bytes":
	command==RT_SKIPBYTES?"skip bytes":"",
        tapebytes,bufpos,tape_blocksz);
}
DDD{
fprintf(stderr,"readtape(buf,bytesrq=%d,mode=%s) tapebytes=%d, bufpos=%d/%d\n",
        bytesrq,
	command==RT_SKIPTOEND?"skip to end":
	command==RT_GETBLOCK?"get block":
	command==RT_GETBYTES?"get bytes":
	command==RT_SKIPBYTES?"skip bytes":"",
        tapebytes,bufpos,tape_blocksz);
}
	if (fileseq == -1)
	{
		fileseq = debug_start;
	}
        if(command==RT_SKIPTOEND) 
        { 
                tapebytes=0;

                do 
                { 
                        stat=read(tape_fd,tapebuf,tape_blocksz); 
DDD{
printf("skipping %d %c%c%c%c\n",stat,tapebuf[0],tapebuf[1],tapebuf[2],tapebuf[3]); 
}
                } while (stat); 

                return 0;     
        }

        if (command == RT_GETBLOCK)
        {
                int ret;
               
                if (tapebytes)
                {
                        memcpy(buf,tapebuf+bufpos,tapebytes);
                        ret=tapebytes;
                        tapebytes=0;
                }
                else
                {
                        tapebytes = read(tape_fd,tapebuf,tape_blocksz);
                        memcpy(buf,tapebuf,tapebytes);
                        ret=tapebytes;
			tapebytes=0;
                }
                return ret;
        }
        for (bytes=0; bytes < bytesrq; )
        {
                if (tape_fd == -1)
                {
			if (eightmilx)
			{
				sprintf(arch_file,"file%d",fileseq++);
			}
                        tape_fd = open((strlen(norwd_file))?norwd_file:arch_file,O_RDONLY);
			eotape=FALSE;
                        if (tape_fd<0) 
                        {
			      openloop:
				
				if (errno==ENXIO)
				{
					fprintf(stderr,"Place device %s ONLINE and press Return.\n",
						arch_file);
					getchar();
					goto openloop;
				}
				else
				{
					fprintf(stderr,"Error opening %s: %s (errno=%d)\n",
						arch_file,sys_errlist[errno],errno);
					handler();
				}
                        }
                }
                if (tapebytes == 0)
                {
		      loop:
                        tapebytes = read(tape_fd,tapebuf,tape_blocksz);
DDD{
fprintf(stderr,"read %d bytes, bytes rqd is %d\n",tapebytes,bytesrq);
}                       

				

				
#ifdef ATT3B2
			if (tapebytes<0 && errno = ENXIO)
			{
				tapebytes=0;
			}
#endif
                        if (tapebytes<0) 
                        {
                                fprintf(stderr,"Error reading %s: %s (errno=%d)\n",
					arch_file,sys_errlist[errno],errno);
                                exit(1);
                        }
                        if (tapebytes == 0 && !eightmil)
                        {       
				if (!eotape)
				{
					if (eots==0)
					{
						fprintf(stderr,"\nEnd of tape encountered.\n");
						eotape=TRUE;
						++eots;
						if (strlen(norwd_file))
						{
							close(tape_fd);
							tape_fd =  -1; 
						}
						geteot();
					}
					else
					{
						return -1;
					}
				}
				else
				{
					fprintf(stderr,"\nMissing EOT block.  Check tape or try running\n");
					fprintf(stderr,"VSX again using the no-rewind device.\n");
					fprintf(stderr,"\n");
					return -1;
				}
				fprintf(stderr,"Rewinding tape\n");
				tapebytes=0;
				close(tape_fd);
				if (strlen(norwd_file))
				{
					if (strlen(arch_file) && strcmp(arch_file, TAPEDEV))
					{
						int  rwd;
						rwd=open(arch_file,O_RDONLY);
						close(rwd);
					}
					else
					{
						fprintf(stderr,"Unable to rewind.  When using -nrw <tape>, specify the\nrewind on close device as -f <tape>.\n");
					}
				}
				tape_fd =  -1; 
                                return -1;
                        }
			if (eightmil && tapebytes == 48)
			{
				if (!eotape)
				{
					eotape=TRUE;
					fprintf(stderr,"\nEnd of tape encountered.\n");
					geteot();
				}
				else
				{
					fprintf(stderr,"\nMissing EOT block.  Verify tape and try again.\n");
					fprintf(stderr,"\n");
				}
				fprintf(stderr,"Rewinding tape\n");
				tapebytes=0;
				close(tape_fd);
				tape_fd =  -1; 
				return -1;
			}
			if (eightmilx && tapebytes==0)
			{
				close(tape_fd);
				tape_fd =  -1; 
				if (fileseq == debug_end)
				{
					eotape=TRUE;
					return -1;
				}
			}
                        bufpos=0;
                }
                if ((bytesrq-bytes) < tapebytes)
                {
                        if (command != RT_SKIPBYTES) 
			  memcpy(buf+bytes,tapebuf+bufpos,bytesrq-bytes);
                        bufpos += (bytesrq - bytes);
                        tapebytes -= (bytesrq-bytes);
                        bytes=bytesrq;
                }
                else
                {
                        if (command != RT_SKIPBYTES) 
			  memcpy(buf+bytes,tapebuf+bufpos,tapebytes);
                        bytes += tapebytes;
                        tapebytes=0;
                }
        }
        tapepos += bytesrq;
        return tapepos;
}
geteot()
{
	struct tapetrailer x;
	FILE *eotlog;
	short seq;
	void getbin();
	int st;
	
	if (!eightmil)
	{
		st = readtape(&x,sizeof(x),RT_GETBYTES);
		if (st== -1)
		{
			fprintf(stderr,"Bad EOT block\n");
			eoarchive=TRUE;
			return;
		}
	}
	else
	{
		memcpy(&x,tapebuf,sizeof(struct tapetrailer));
	}
	getbin(&seq,x.fileseq,SHORT);
#if 0
	eotlog = fopen("eotlog","a");
	fprintf(eotlog,"BEOT     [%4.4s]\n",x.beot);
	fprintf(eotlog,"TAPEVOL  [%6.6s]\n",x.tapevol);
	fprintf(eotlog,"ORIGVOL  [%6.6s]\n",x.origvol);
	fprintf(eotlog,"NEXTVOL  [%6.6s]\n",x.nextvol);
	fprintf(eotlog,"SEQUENCE [%5.5d]\n",seq);
	fprintf(eotlog,"FLAG [%x/%c]\n",x.flag[0],x.flag[0]);
	fclose(eotlog);
#endif
	if (x.flag[0] == 0x80)
	{
		memcpy(expected_vol,x.nextvol,6);
		++expected_seq;
		file_continued=1;
		fprintf(stderr,"Backup continued on tape volume %6.6s\n",expected_vol);
	}
	else
	  eoarchive=TRUE;
}

load()
{
        unsigned int recs,fileeof,reccnt;
        unsigned short recsz;
        unsigned char *recbuf;
        char vol[7],lib[9],file[9];
        unsigned short small,smallextra;
        uint4 med,eofword;
        uint4 big, bigextra;
        unsigned int oreccnt;
        char outpath[200];
        char *strchr(),*s;
        int newlib;
        FILE *out;
        void getbin();
	static int saverecs;
        int dfpos;
        
        tapehdr=(struct tapeheader *)workbuf;

        act_file_type=file_type==FT_ASK_L?0:FT_SRC;

        big=sizeof(struct tapeheader); bigextra= 0x800-big;
        med=newlib=eoarchive=0;
        small=smallextra=skip_lib=0;
        eoarchive=FALSE;
        recbuf=NULL;
        spec_rec_type=rec_type;
        
        while (eoarchive==FALSE)
        {
		if (file_continued)
		{
			prompt_new_tape();
		}
                dfpos=
                getbytes(sizeof(struct tapeheader));
                if (dfpos== -1 || chk_valid_hdr()== -1)
		{
			continue;
		}
                big -= sizeof(struct tapeheader);
                getbin((unsigned char *)&recsz,   (unsigned char *)tapehdr->recsz,SHORT);
                getbin((unsigned char *)&reccnt,  (unsigned char *)tapehdr->reccnt,LONG);
                getbin((unsigned char *)&seq_num, (unsigned char *)tapehdr->seqnum,SHORT);
                org_type = tapehdr->filler4b&ORG_MASK;
                if (tapehdr->filler4b&0x40) 
		  org_type = ORG_INDEX;
                if (recbuf) 
                {
                        free(recbuf);
                        recbuf=NULL;
                }
                
                if (!file_continued) 
		  oreccnt=0;
                recbuf=(unsigned char *)calloc(recsz+1,1);
                memcpy(vol,tapehdr->srcvol,6);
                memcpy(lib,tapehdr->srclib,8);
                memcpy(file,tapehdr->filename,8);
                vol[6]=lib[8]=file[8]=(char)0;
DD{printf("header=%s\n",tapehdr->encfhdr); fflush(stdout);}
D{printf("header=%4.4s backsection=%1.1d volseq=%2.2d\n",tapehdr->encfhdr,tapehdr->backsection[1],tapehdr->volseq[1]); fflush(stdout);}

                if (interactive)
                {
                        if (strcmp(lib,oldlib))
                        {
                                int foo,ok;
                                
                                for (ok=FALSE; ok==FALSE; )
                                {
                                        printf("\nLibrary name: %8s    Restore this library? (y/n)-->",lib);
                                        rawmode();
                                        foo=getchar();
                                        normalmode();
                                        printf("\n");
                                        if (foo=='y'||foo=='Y') { skip_lib=FALSE; ok=TRUE; }                                    
                                        else if (foo=='n'||foo=='N') { skip_lib=TRUE; ok=TRUE; }
                                        else ok=FALSE;
                                        newlib=TRUE;
                                }
                        }
                }
                if (file_type==FT_ASK_L && !skip_lib)
                {
                        if (strcmp(lib,oldlib))
                        {
                                int foo;

                                if (interactive)
                                {
                                  printf("                          Type source/normal (s/n)-->",lib);
                                }
                                else
                                  printf("\nLibrary name: %8s     Type source/normal (s/n)-->",lib);
                                rawmode();
                                foo=getchar();
                                normalmode();
                                printf("\n\n");
                                act_file_type=(foo=='s'||foo=='S')?FT_SRC:FT_DAT;
                                if (foo=='x'||foo=='X') skip_lib=TRUE;
                                else skip_lib=FALSE;
                                newlib=TRUE;
                        }
                }
                if (newlib)
                {
                        strcpy(oldlib,lib);
                        newlib=FALSE;
                }
                s=strchr(lib,' ');
                if (s) *s=(char)0;
                s=strchr(file,' ');
                if (s) *s=(char)0;
                if (tapehdr->rectype==HD_RT_COMP) spec_rec_type=RT_COMP;
                else if (tapehdr->rectype==HD_RT_NORM) spec_rec_type=RT_NORM;
                else if (tapehdr->rectype==HD_RT_VAR) spec_rec_type=RT_VAR;
                else if (tapehdr->rectype==0xa0) spec_rec_type=RT_NORM;
                else spec_rec_type=rec_type;

                if (file_type!=FT_ASK_L)
                {
                        if (recsz==80) act_file_type=FT_SRC;
                        else act_file_type=FT_DAT;
                }
                if (skip_lib==TRUE) fprintf(stderr,".");

		
                if (action==ACT_EXTRACT && skip_lib==FALSE)
                {
                        char *use_ext;
                        
			if (lname_case==CASE_LOWER) 
			{
				lower(lib);
			}
                        if (fname_case==CASE_LOWER)
			{
				lower(file);
			}
                        mkdir(lib,0777);
                        chmod(lib,0777);
                        if (spec_rec_type != RT_NORM) /* it's compressed or var, needs post-processing */
                          use_ext = ".tmp";
                        else 
                          use_ext = act_file_type==FT_SRC?src_ext:norm_ext;
                        sprintf(outpath,"%s/%s%s",lib,file,use_ext);
                        if (spec_rec_type != RT_NORM && org_type != ORG_INDEX)
                          pushname(outpath);
			if (file_continued)
			  printf("Continuing %s...\n",outpath);
			if (org_type != ORG_INDEX)
			  printf("%5d:vol=%-6s lib=%-8s file=%-8s recsize=%-5d reccount=%-10d [%s,%s] ==>%s\n",seq_num,
				 vol,lib,file,recsz,reccnt, /* tapehdr->filler4b,tapehdr->rectype, */
				 rectypenames[spec_rec_type],
				 act_file_type==FT_SRC?"Source":"Data",outpath);
			else
			  printf("*** Index file skipped: %-6s %-8s %-8s\n",vol,lib,file);
DD{
                        printf("top %08x : big=%x/%d, bigextra=%x/%d, med=%x/%d, small=%x/%d, %x/%d\n",dfpos,
                       big,big,bigextra,bigextra,med,med,small,small,smallextra,smallextra);
}
                }
                else if (action==ACT_INDEX && skip_lib==FALSE)
                {
                        printf("%5d:vol=%-6s lib=%-8s file=%-8s recsize=%-5d reccount=%-10d [%s,%s,%02x]\n",seq_num,
                               vol,lib,file,recsz,reccnt, /*tapehdr->rectype, */
                               rectypenames[spec_rec_type],
                               act_file_type==FT_SRC?"Source":"Data",tapehdr->filler4b);
#ifdef OLD
                        printf("vol=%-6s lib=%-8s file=%-8s recsize=%-5d reccount=%-5d [%02x%02x]\n",
                               vol,lib,file,recsz,reccnt,tapehdr->filler4b,tapehdr->rectype);
#endif
                }               
                if(action==ACT_EXTRACT && skip_lib==FALSE && !file_continued) 
                  out=fopen(outpath,"w");
                fileeof=0;

                if (spec_rec_type != RT_NORM && out!=NULL && action==ACT_EXTRACT
			&& !file_continued) /* stamp an info struct for post-processing */
                {
                        struct my_header mh;
                        
                        mh.rectype = spec_rec_type;
                        mh.filetype = act_file_type;
                        mh.recsz = recsz;
                        mh.reccnt = reccnt;
                        fwrite(&mh,1,sizeof(struct my_header),out);
                }
		if (file_continued)
		{
			file_continued = FALSE;
			big=med=0;
			bigextra= 0x800-sizeof(struct tapeheader);
			med=0;
			small=smallextra=0;
			saverecs=recs;
		}
		else
		  saverecs = 0;
                while (!fileeof && !file_continued)
                {
                        DD{
                                printf("toploop %08x r%d: big=%x/%d, bigextra=%x/%d, med=%x/%d, small=%x/%d, %x/%d\n",dfpos,recs,
                                       big,big,bigextra,bigextra,med,med,small,small,smallextra,smallextra);
                        }
                        do
                        {
                                if (!eightmil)
                                {
                                        if (big==0)
                                        {
                                                if (bigextra) skipbytes(bigextra);
                                                if (getbytes(LONG)<0) 
						{
							continue;
						}
                                                
                                                getbin((unsigned char *)&big,workbuf,LONG);
                                                bigextra = 0x20000-big-4;
                                                DD{
                                                        printf("index: got big %x %x\n",big,bigextra);
                                                }
                                        }
                                        if (med==0)
                                        {
                                                getbytes(LONG);
                                                getbin((unsigned char *)&med,workbuf,LONG);
                                                big-=LONG;
                                                DD{
                                                        printf("index: got med %x\n",med);
                                                }
                                                
                                                eofword=med;
                                        }
                                        if (med != 0xffff0090) 
                                        {
                                                if (getbytes(med)<0)
						{
							continue;
						}
                                                big -= med;     
                                                if(action==ACT_EXTRACT && 
                                                   skip_lib==FALSE && 
                                                   out!=NULL && 
                                                   org_type!=ORG_INDEX) 
                                                if (spec_rec_type==RT_NORM)
                                                {
                                                        int recs_this_block;
							int offset;
							recs_this_block = 2048/recsz;
							for (offset=0;offset < med; offset+=2048)
							{
								if (oreccnt + recs_this_block > reccnt)
								{
									recs_this_block = reccnt - oreccnt;
								}
								if (recs_this_block)
								  writefixed(workbuf+offset,recs_this_block,recsz,out);
								oreccnt += recs_this_block;
							}
                                                }
                                                else
                                                {
                                                        fwrite(workbuf,1,med,out);
                                                }
                                        }
                                        med=0;
                                }
                                else /* 8 mil tapes have no big and medium blocking junk */
                                {
                                        int cnt;
                                        
                                        if (spec_rec_type==RT_NORM)
                                        {
						int cur, leftover;
						
						leftover = 0x800 % recsz;
                                                for (cur=0, recs=saverecs; recs<reccnt; ++recs)
                                                {
							if (cur + leftover >= 0x800)
							{
								skipbytes(leftover);
								cur=0;
							}
                                                        if (getbytes(recsz)<0)
							{
								goto endloop;
							}
							cur+=recsz;

                                                        if(action==ACT_EXTRACT && 
                                                           skip_lib==FALSE && 
                                                           out!=NULL && 
                                                           org_type!=ORG_INDEX) 
							{
								if (recsz==80 && check_nulls) nulls2spaces(workbuf);
								fwrite(workbuf,1,recsz,out);
								if (add_lf==ADDLF)
								  fprintf(out,"\n");
								if (add_lf==GUESSLF && recsz==80)
								  fprintf(out,"\n");
							}
                                                }
                                                readtape(0,0,RT_SKIPTOEND);
                                        }
                                        else
                                        {
                                                while (cnt=readtape(workbuf,0,RT_GETBLOCK)) /* skip was 1? */
                                                {
							if (cnt== -1)
							{
								goto endloop;
							}
                                                        if(action==ACT_EXTRACT && 
                                                           skip_lib==FALSE && 
                                                           out!=NULL && 
                                                           org_type!=ORG_INDEX) 
                                                          fwrite(workbuf,1,cnt,out);
                                                }

                                        }
                                        eofword = 0xffff0090;
                                        
                                }

			      endloop: ;
				
                        } while (eofword != 0xffff0090 && !eoarchive && !eotape);
                        ++fileeof;
                        continue;
                }
                if (action==ACT_EXTRACT && skip_lib==FALSE && !file_continued) fclose(out);
                if (org_type == ORG_INDEX) unlink(outpath);
        }
}
trim(buf)
unsigned char buf[];
{
        register unsigned char *p;
	int i;
	
	if (check_nulls)
	  nulls2spaces(buf);

        buf[72]=(char)0;
        p=buf+71;
        while (*p==' ') --p;
        ++p;
        *p=(char)0;
}
nulls2spaces(buf)
unsigned char buf[];
{
	int i;

	for (i=0; i<80; ++i) if (buf[i]==0x00) buf[i]=' ';
}
bldrec(src,dest,size)       /* need to redo states START COMP0 COMP and NEXTCOMP */
unsigned char *src,*dest;
int size;
{
        register unsigned int in_i,ch,cnt,next,out_i;
        register int state;
        char *reptstr();

        char *states[6];
        
        states[0]="start";
        states[1]="normal";
        states[2]="comp";
        states[3]="nextcomp";
        states[4]="end";
        states[5]="comp0";
        
        state=ST_START;
        in_i=out_i=0;
        next=65535;
        
        while (state != ST_END)
        {
                ch=src[in_i];

                switch (state)
                {
                      case ST_START:
                        if (ch>=0x80)
                        {
                                cnt=ch-0x80+1;
                                state=ST_COMP;
                        }
                        else 
                        {
                                next=ch+1;
                                state=ST_NORMAL;
                        }
                        ++in_i;
                        break;
                      case ST_NORMAL:
                        dest[out_i]=ch;
                        ++out_i;
                        ++in_i;
                        break;
                      case ST_COMP0:
                        if (ch>=0x80)
                        {
                                cnt=ch-0x80+1;
                                state=ST_COMP;
                        }
                        else 
                        {
                                next=ch+1;
                                state=ST_NORMAL;
                        }
                        ++in_i;
                        break;
                      case ST_COMP:
                        memcpy(&dest[out_i],reptstr(ch,cnt),cnt);
                        ++in_i;
                        out_i += cnt;
                        if (src[in_i]>=0x80) state=ST_COMP0;
                        else state=ST_NEXTCOMP;
                        break;
                      case ST_NEXTCOMP:
                        next=ch+1;
                        state=ST_NORMAL;
                        ++in_i;
                        break;
                }
                if (in_i>=size) state=ST_END;
                if (next==0) { state=ST_COMP0; next=65535; }
                else --next;
        }
}
char *reptstr(ch,cnt)
{
        static char rept[128];
        
        memset(rept,ch,cnt);
        rept[cnt]=(char)0;
        return rept;
}
getbytes(cnt)
int cnt;
{
        int pos;
        
        if(cnt<=0)return 0;
        pos=readtape(workbuf,cnt,RT_GETBYTES);
        return pos;
}
skipbytes(cnt)
int cnt;
{
        int tmp;
        
        if (cnt<=0)return 0;

        tmp=readtape(NULL,cnt,RT_SKIPBYTES);
        return tmp;
}
lower(p)
char *p;
{
        while (*p)
        {
                *p = tolower(*p);
                ++p;
        }
}
parseopts(c,v)
int c;
char **v;
{
        int i,num,type,val;
        char *destval;


	strcpy(arch_file,TAPEDEV);
        strcpy(src_ext,DEF_SRC_EXT);
        strcpy(norm_ext,DEF_NORM_EXT);
        
        for (i=1;i<c;)
        {
                matchopt(v[i],&num,&type,&destval,&val);
		if (num == -1 && process_tmps) return;
                if (num == -1) fprintf(stderr,"bad option \"%s\"\n",v[i]);
                if (type==OPT_INT) *((int*)destval)=val;
                else if (type==OPT_STRING) 
                {
                        if (strlen(v[i])!=strlen(optlist[num].opt)) strcpy(destval,v[i]+strlen(optlist[num].opt));
                        else strcpy(destval,v[++i]);
                }
                ++i;
        }
	if (strlen(tape_rq_str))
	{
		tape_blocksz=atoi(tape_rq_str);
	}
#ifdef ATT3B2
	if (tape_blocksz>5120) 
	  tape_blocksz=5120;
#endif
}
matchopt(opt,num,type,destval,val)
char *opt,**destval;
int *num,*type,*val;
{
        struct optstruct *p;
        int i;
        
        for(i=0,p=optlist;p->opt;++p,++i)
        {
                if (!strncmp(opt,p->opt,strlen(p->opt)))
                {
                        *num=i;
                        *type=p->type;
                        *destval=p->optdest;
                        *val=p->optval;
                        return;
                        
                }
                
        }
        *num= -1;
        *type = OPT_BAD;
}

void getbin(dest,src,cnt)
unsigned char *dest,*src;
int cnt;
{
        if (byteorder==ORDER_NORMAL)
	{
		memcpy(dest,src,cnt);
		return;
	}
        --cnt;
        while (cnt>=0)
        {
                *dest++ = *(src+cnt);
                --cnt;
        }
        return;
}
setord()
{
        int foo=0x12345678;
        if ((((char*)&foo)[0])==0x78) byteorder=ORDER_REVERSE;
        else byteorder=ORDER_NORMAL;
}
usage()
{
        fprintf(stderr,"VSX Release V%s (c)IDSI %s\n",VERSION,MODDATE);
        fprintf(stderr,"\nusage:\nvsx [-f <device or file>] [ -nrw <norewind file> ] [-e.ext] [-i] [-lnu] [-fnu]\n");
        fprintf(stderr,"   -f <archive> specify device or file containing archive (default: %s)\n",TAPEDEV);
        fprintf(stderr,"   -nrw <tape>  specify no-rewind device file\n",TAPEDEV);
        fprintf(stderr,"   -es.ext      append .ext to all source files created (normally .wcb)\n");
        fprintf(stderr,"   -en.ext      append .ext to all normal files created (normally .seq)\n");
        fprintf(stderr,"   -i           generate list of files in archive (do not extract)\n");
        fprintf(stderr,"   -lnu         generate uppercase directory names\n");
        fprintf(stderr,"   -fnu         generate uppercase file names\n");
        fprintf(stderr,"   -tl          query for filetype (source vs. nonsource) per library\n");
        fprintf(stderr,"   -q           query before restoring each library\n");
        fprintf(stderr,"   -8           archive is an 8 millimeter tape or 9-track reel tape\n");
        fprintf(stderr,"   -null        examine cobol source for embedded null values and fix\n");
        fprintf(stderr,"   -tmp         process .tmp files left over in case of vsx premature abort\n");
        fprintf(stderr,"   -lf          add linefeeds between records on all files\n");
        fprintf(stderr,"   -nolf        do not add linefeeds to any files\n");
        fprintf(stderr,"                (vsx normally adds linefeeds only to files with 80 byte records)\n");
        fprintf(stderr,"   -bs <size>   specify blocksize for tape reads\n");
        exit(1);
}
savemode()
{
        ioctl(0,TCGETA,&old);
}       
rawmode()
{
        new=old;
        new.c_cc[VMIN]=1;
        new.c_cc[VTIME]=0;
        new.c_lflag &= ~ICANON;
        new.c_iflag = 0;
        ioctl(0,TCSETA,&new);
}
normalmode()
{
        ioctl(0,TCSETA,&old);
}
setdebuginfo()
{
        char *p,*strchr();
        
        if (strlen(debug_level_str)) 
          debug_level=atoi(debug_level_str);

        if (strlen(debug_range))
        {
                p=strchr(debug_range,':');
                if (p==debug_range)
                {
                        debug_start=1;
                        debug_end=atoi(++p);
                        return;
                }
                else if (p)
                {
                        *p++ =(char)0;
                        debug_start=atoi(debug_range);
                        debug_end=atoi(p);
                        return;
                }
                else
                {
                        debug_start=atoi(debug_range);
                        debug_end=999999;
                        return;
                }
        }  
        else
        {
                debug_start=1;
                debug_end=999999;
                return;
        }               
}
char *strdup(str)
char *str;
{
        char *tmp;
        
        tmp=(char *)calloc(strlen(str)+1,1);
        strcpy(tmp,str);
        return tmp;
}
pushname(str)
char *str;
{
        char *strdup();
        struct stack_node *new_node;
        
        if (head==NULL)
        {
                new_node=stackp=head=(struct stack_node *)calloc(1,sizeof(struct stack_node));
                head->prev = NULL;
        }
        else
        {
                new_node=(struct stack_node *)calloc(1,sizeof(struct stack_node));
                new_node->prev = stackp;
        }
        new_node->name = (unsigned char*) strdup(str);
        stackp = new_node;
}
char *getname()
{
        char *tmp;
        struct stack_node *save;

        if (!process_tmps)
	{
		if (stackp==NULL) return NULL;
		tmp = (char*)stackp->name;
		save = stackp;
		stackp = stackp->prev;
		free(save);
		return tmp;
	}
	else
	{
		if (*arg_list == NULL) return NULL;
		while(**arg_list=='-') ++arg_list;
		return *arg_list++;
	}
}
postprocess()
{
        char *inname,*getname();
        int dummy;
        
        while (inname=getname())
        {
                if (fork()) wait(&dummy);
                else
                {
                        digest(inname);
                        exit(0);
                }
        }
}
void baddata(sig)
int sig;
{
        fprintf(stderr," file appears corrupted\n");
        exit(1);
}
digest(name)
char *name;
{
        char *p, *strchr();
	unsigned char *recbuf;
        struct my_header foo;
        FILE *in,*out;
        char outname[100];
        unsigned short small, smallextra, curlen;
        int recs, recsz,reccnt;
	void baddata();
	void getbin();
	
        signal(SIGINT,  baddata );
        signal(SIGQUIT, baddata );
        signal(SIGILL,  baddata );
        signal(SIGEMT,  baddata );
        signal(SIGBUS,  baddata );
        signal(SIGSEGV, baddata );

        fprintf(stderr,"%s",name); fflush(stderr);
        in=fopen(name,"r");
        strcpy(outname,name);
        p=strchr(outname,'.');
        if (!p)
        {
                fprintf(stderr,"bad temp filename: %s\n",outname);
                fclose(in);
        }
        fread(&foo,1,sizeof(struct my_header),in);
        spec_rec_type = foo.rectype;
        act_file_type = foo.filetype;
        recsz = foo.recsz;
        reccnt = foo.reccnt;
        recbuf=(unsigned char *)calloc(recsz+1,1);
        
        if (act_file_type==FT_SRC) strcpy(p,src_ext);
        else strcpy(p,norm_ext);
        fprintf(stderr," ==> %s : ",outname); fflush(stderr);
        
        if ((out = fopen(outname,"w"))==NULL)
        {
                fprintf(stderr,"cannot open output: %s\n",outname);
        }
        recs=small=smallextra=0;
        while (recs<reccnt)
        {
                if (small==0)
                {
                        if (smallextra) fread(workbuf,1,smallextra,in);
                        fread(workbuf,1,sizeof(short),in);
                        getbin((unsigned char *)&small,workbuf,2);
                        smallextra = 0x800 - small;
                        small -= 2;

                        
                }
                fread(workbuf,1,sizeof(short),in);
                getbin((unsigned char *)&curlen,workbuf,2);

                
                fread(workbuf,1,curlen-2,in);
                small -= curlen;
                if (spec_rec_type == RT_COMP) bldrec(workbuf,recbuf,curlen-2);
                else memcpy(recbuf,workbuf,recsz),*(recbuf+recsz)=(char)0;
                ++recs;
                if (act_file_type == FT_SRC) trim(recbuf);
                fwrite(recbuf,1,act_file_type == FT_SRC?strlen(recbuf):recsz,out);
                if (add_lf==ADDLF)
                  fprintf(out,"\n");
                if (add_lf==GUESSLF && recsz==80)
                  fprintf(out,"\n");
        }
        fprintf(stderr,"%d records, rec size %d bytes\n",reccnt,recsz);
        
        fclose(in);
        free(recbuf);
        unlink(name);
}

writefixed(buf,recs,size,f)
unsigned char buf[];
int recs,size;
FILE *f;
{
        while (recs)
        {
		if (size==80 && check_nulls) nulls2spaces(buf);
		
                fwrite(buf,1,size,f);
                buf += size;
                --recs;
                if (add_lf==ADDLF) 
                  fprintf(f,"\n");
                if (add_lf==GUESSLF && size==80)
                  fprintf(f,"\n");
        }
}
chk_valid_hdr()
{
	char volseq;
	int vsxdump;

	if (strncmp("ENCF",tapehdr->encfhdr,4))
	{
		close(tape_fd);
		tape_fd= -1;
		tapebytes=0;
		vsxdump=open(".vsxdump",O_WRONLY|O_CREAT|0666);
		write(vsxdump,tapebuf,tapebytes);
		close(vsxdump);
		if (new_vol)
		{
			fprintf(stderr,"Volume header appears to be bad.\n");
			return -1;
		}
		else
		{
			fprintf(stderr,"Bad file header.\n");
			postprocess();
			handler();
		}
	}
	if (strlen(expected_vol)==0)
	  return 1;
	if (new_vol && strncmp(expected_vol,tapehdr->vol1,6))
	{
		fprintf(stderr,"Warning: Incorrect volume label. [%6.6s/%6.6s/%6.6s]\n",
			tapehdr->vol1,tapehdr->tapevol,expected_vol);
	}
	else new_vol=FALSE;
	return 1;
	volseq = tapehdr->backsection[0];
	if (volseq != expected_seq)
	{
		close(tape_fd);
		tape_fd= -1;
		tapebytes=0;
		fprintf(stderr,"Tape appears to have incorrect sequence number (%d).  Expected %d.\n",
			volseq, expected_seq);
		return -1;
	}
	return 1;
}
prompt_new_tape()
{
	fprintf(stderr,"Insert next backup tape (volume: %6.6s).\n",
		expected_vol);
	fprintf(stderr,"Press return when ready\n");
	getchar();
	new_vol=TRUE;
}
