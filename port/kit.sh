
name=$1
find $name -print > $name.fl
cpio -ocv > $name.cpio < $name.fl
compress $name.cpio
ls -l $name.*
