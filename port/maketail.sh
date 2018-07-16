#  maketail.sh [target]
echo Making $1
touch make.err
date >> make.err
echo Making $1 >> make.err
make -f makewisp.umf $1>> make.err 2>&1 &
tail -f make.err
