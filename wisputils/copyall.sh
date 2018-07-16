	dcp -c -v 'scalos::scs$wisp:[display.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[trim.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[utils.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[wusage.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

	dcp -c -v 'scalos::scs$wisp:[data.master.source]wispmsg.txt' .
	dcp -c -v 'scalos::scs$wisp:[data.qahold.source]wispmsg.txt' .

