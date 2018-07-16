	dcp -c -v 'scalos::scs$wisp:[utils.master.source]*.c' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.source]*.c' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.include]*.h' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.include]*.h' .
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.h' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.h' .

