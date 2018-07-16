	dcp -c -v 'scalos::scs$wisp:[display.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[display.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[trim.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[trim.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[utils.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[wusage.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[wusage.qahold.build]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

	dcp -c -v 'scalos::scs$wisp:[data.qahold.source]wispmsg.txt' .

