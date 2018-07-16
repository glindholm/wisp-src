make >> make.err 2>&1 &
sleep 1
tail -f make.err
