# 
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
	dcp -c -v 'scalos::scs$wisp:[tran.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .

