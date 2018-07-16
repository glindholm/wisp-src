
# which or whare cmd:	List command(s) in PATH
#
# This command should be named "which" and linked to "whare".

usage="Usage: $0 [ -a ] [ -e ] [ -l ] [ -p ] [ -v ] command"
verbose1="-a = path listing every match in PATH"
verbose2="-e = long listing first match in PATH"
verbose3="-l = long listing every match in PATH"
verbose4="-p = path listing first match in PATH"

dousage="n"				# do usage display, flag.
doverbose="n"				# do verbose display, flag.

case $# in
0)					# No args, display usage.
	dousage="y"
	;;
1)					# 1 arg, use command name as flag.
	if [ "-v" = "$1" ]
	then				# Display verbose usage.
		dousage="y"
		doverbose="y"
	else
		name=$1			# Only argument is name to search for.
		#
		case $0 in
		which)				# Same as -p
			lsflag=""
			all="n"
			;;
		whare)				# Same as -l
			lsflag="-l"
			all="y"
			;;
		*)				# Same as -a
			lsflag=""
			all="y"
			;;
		esac
	fi
	;;
2)
	name=$2
	#
	case $1 in
	-a)				# -a = path listing every match in PATH
		lsflag=""
		all="y"
		;;
	-e)				# -e = long listing first match in PATH
		lsflag="-l"
		all="n"
		;;
	-l)				# -l = long listing every match in PATH
		lsflag="-l"
		all="y"
		;;
	-p)				# -p = path listing first match in PATH
		lsflag=""
		all="n"
		;;
	-v)				# Display verbose usage.
		dousage="y"
		doverbose="y"
		;;
	*)
		dousage="y"
		;;
	esac
	;;
*)
	dousage="y"
	;;
esac

if [ "y" = "$dousage" ]
then
	echo $usage
	if [ "y" = "$doverbose" ]
	then
		echo ' '
		echo $verbose1
		echo $verbose2
		echo $verbose3
		echo $verbose4
		echo ' '
	fi
	exit 2
else
	found="n"
	for pi in `echo $PATH | sed 's/^:/.:/
				     s/::/:.:/g
				     s/:$/:./
				     s/:/ /g'`
	do
		if [ -f ${pi}/$name ]		# If "name is in the directory,
		then
			ls $lsflag ${pi}/$name	# Display path with "ls"
			if [ "n" = "$all" ]	# If only 1 is wanted,
			then
				exit 0
			else
				found="y"	# Set flag to found
			fi
		fi
	done

	if [ "n" = "$found" ]
	then
		if [ "y" = "$all" ]		# If "all" selected,
		then
			echo "$name not found on path."
		fi

		exit 1
	else
		exit 0
	fi
fi

exit 3						# This should never happen.
