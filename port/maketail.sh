#  maketail.sh [target]
echo Making $1
make -f makewisp.umf $1>> make.err 2>&1 &
sleep 1
tail -f make.err
