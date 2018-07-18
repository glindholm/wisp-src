#!/bin/sh -
#
# Usage: wmkdep_gcc.sh *.c
#
#


D=.depend			# default dependency file is .depend
append=0

while :
	do case "$1" in
		# -a appends to the depend file
		-a)
			append=1
			shift ;;

		# -f allows you to select a makefile name
		-f)
			D=$2
			shift; shift ;;

		# the -p flag produces "program: program.c" style dependencies
		# so .o's don't get produced
		-p)
			SED='s;\.o;;'
			shift ;;
		*)
			break ;;
	esac
done

if [ $# = 0 ] ; then
	echo 'usage: wmkdep_gcc.sh [-f depend_file] *.c'
	exit 1
fi

TMP=/tmp/mkdep$$

trap 'rm -f $TMP ; exit 1' 1 2 3 13 15

MYCFLAGS="-I ../wispcommon -I ../videolib -DSOLARIS -Dunix "

# GCC with the -MM flag creates output like this:
#vwang.o: vwang.c ../videolib/video.h ../videolib/verase.h \
# ../videolib/vmenu.h ../videolib/vintdef.h ../videolib/vline.h \
# ../videolib/vscreen.h ../videolib/vprint.h ../videolib/vlocal.h \
# ../videolib/vdata.h ../videolib/vcap.h ../videolib/vchinese.h \
# ../wispcommon/idsistd.h ../wispcommon/intdef.h \
# ../wispcommon/wperson.h ../wispcommon/vwang.h ../wispcommon/scnfacs.h \
# ../wispcommon/wglobals.h ../wispcommon/wfiles.h \
# ../wispcommon/wdefines.h ../wispcommon/wisplib.h \
# ../wispcommon/setenvst.h ../wispcommon/wexit.h assert.h wsfns.h \
# menu.h ../wispcommon/wmalloc.h ../wispcommon/costar.h \
# ../wispcommon/wispcfg.h ../wispcommon/werrlog.h
#
#
#  Sed then to:
#	- Change "../videolib" to "$(VC)"
#	- Change "../wispcommon" to "$(WC)"
#	- Make the objects part of a lib "$(THE_LIB)(xxx.o)"
#
#  Result looks like:
#
#$(THE_LIB)(vwang.o): vwang.c $(VC)/video.h $(VC)/verase.h \
# $(VC)/vmenu.h $(VC)/vintdef.h $(VC)/vline.h \
# $(VC)/vscreen.h $(VC)/vprint.h $(VC)/vlocal.h \
# $(VC)/vdata.h $(VC)/vcap.h $(VC)/vchinese.h \
# $(WC)/idsistd.h $(WC)/intdef.h \
# $(WC)/wperson.h $(WC)/vwang.h $(WC)/scnfacs.h \
# $(WC)/wglobals.h $(WC)/wfiles.h \
# $(WC)/wdefines.h $(WC)/wisplib.h \
# $(WC)/setenvst.h $(WC)/wexit.h assert.h wsfns.h \
# menu.h $(WC)/wmalloc.h $(WC)/costar.h \
# $(WC)/wispcfg.h $(WC)/werrlog.h
#

gcc $MYCFLAGS -MM $* |
sed "
	s;\.\./videolib/;\$(VC)/;g
	s;\.\./wispcommon/;\$(WC)/;g
	/^[^ ]/s;^;\$(THE_LIB)(;
	s;\.o:;.o):;
	$SED" > $TMP

if [ $append = 1 ]; then
	cat $TMP >> $D
	rm -f $TMP
else
	mv $TMP $D
fi
exit 0
