
#include <stdio.h>
#include <stdlib.h>
#include "kcsifunc.h"


/*----
Fill in a passed 79 byte field with logo and version   
Consolidated file replaces ctrlvers.c rptvers.c datvers.c and inqvers.c
and uses one version number for all modules.
-------*/

/*----
Mods log, latest at top.
Entries between RNotes and EndRNotes are used to create Release Notes
using make Release.crid.
The ----- and +++++ fields in the left margin turn off and on the
output to the Release.crid file.

Place unhandled bugs here with MR no


RNotes

		Release Notes	-	12/28/94

*	Not Released indicates a version number that was used
	for internal development or bug-fix version control, and
	was not released to customers.

v. 2.75 01/23/95
Changed dbsc.c and kvxi.c to reflect Danzas mods.

v. 2.74 12/28/94
No external changes to program functions.
-----
v 2.74 Dec 28, 1994
New version incorporates everything I can think of except
the coding changes put in by Suzette for VAX file io.
+++++

v 2.73 Sept 14, 1994

Utility:	ALL

Problem:	Debug Tracing needed COBOL modules for
		Micro Focus COBOL versions

Resolution:	Tracing added
+++++

-----
v 2.73 Sept 14, 1994
+++++
v 2.72 June 13, 1994

Utility:	DATENTRY

Problem:	Default Key path for a file with multiple paths
		was blank.

Resolution:	Modified to imitate Wang logic and fill in the
		path with the primary key.

-----
MR:		273
Module:		dtekey.c		1.7
Changes:	Filled in the path for all cases.
+++++

v 2.71 March 10, 1994

Utility:	All

Problem:	Links between utilities were displaying erroneous
		link error messages when returning from a linked-to
		utility.

Resolution:	Link error message are now only displayed on a failure
		to link.

-----
Changes:	pllink.wcb

mr:		1201
+++++
v 2.70 March 6, 1994

Utility:	VAX CONTROL

Problem:	Linking from CONTROL to REPORT, DATENTRY or
		INQUIRY, the called programs were not being
		located.

Resolution:	Changes to Dynamic linking to first attempt a 
		"system" link, then a user link.

v. 2.69  Feb 26, 1994

No Changes, Internal version control only.

-----
Removed gpcsio, gpscsio and gpwcsio routines from vax version
and these will have to be handled for create (and possibly listmgmt)
if they are ever done for the VAX.
		vcsio.c		1.9
Removed extern declares for sys_nerr and  sys_errlist, and
included <errno.h>
+++++
v. 2.68 December 5, 1993

Utility:	REPORT and INQUIRY under UNIX.

Problem:	REPORT and INQUIRY are issuing an initial form feed
		before printing the report. This causes a problem with
		certain types of printers (laser) by ejecting an extra
		blank page before printing.

Resolution:	For all UNIX versions, an environment variable may
		be used to omit this initial form feed.

		To suppress the initial form feed, use the following 
		commands berfore running REPORT, or include these
		commands in a profile file for the user.

		kcsi_reportff=0 ; export kcsi_reportff

		The variable must be in lower case, and the value 
		used for kcsi_reportff must be 0 as written.


-----
v. 2.67
No substantial changes.
+++++

-----
v. 2.66 
reverted all of the below changes to try another approach
1.	Left KMATCH in, but made it function correctly
	kcsio.c		1.11
2.	Left in full init of the KFB
3.	Left out the info calls for RPTIO and CTRLIO
	But restored calls in tableio.wcb and ctrltbl.wcb
	and added new call for table info.
	tableio.wcb		1.5
	ctrltbl.wcb		1.7	1.8
4.	Added get_table_info() to vcsio.c
	vcsio.c			1.7

5.	Added ksam_table_info() to kmf.c (new) kv3.c and klpi.c
	klpi.c			1.5
	kv3.c			1.16


+++++
v. 2.65 November 6, 1993

Utility:	All - Microfocus versions

Problem:	The Microfocus file handler routine contains a bug that
		causes the return_file_info function of EXTFH() to
		produce invalid key structure information on a file.
		This causes REPORT, INQUIRY and DATENTRY to believe
		that the file does not match the layout given in the
		CONTROL definition file.

Resolution:	Until the bug in Microfocus is repaired, the requirement
		that files in REPORT, INQUIRY and DATENTRY match
		the definition in the CONTROL file has been disabled.

		This will allow the files to be opened, but can cause
		memory errors if the file being opened does not actually
		match the control definition.

		Normally REPORT, INQUIRY and DATENTRY will provide a message
		that the data file and control definition do match.

		With this disabled, you will probably simply receive
		a crash or memory fault error until the bug is fixed by
		Microfocus.

-----
MR:		1193
Changes:	1.	Added KMATCH routine to compare the areas of memory.
		Added calls to inqmain.wcb, dtectl.wcb and rptwmn.wcb to use
		this function. KMATCH routine is disabled when in KCSI_MF
		mode. Always returns a match.
		2.	Removed spurious and unused FILE-INFO requests from
			io and other modules.
		3.	Modified kcsio to fully init the kfb area duringany
			open.
Modules:	1.	kcsio.c		1.9
			rptwmn.wcb	1.14
			inqmain.wcb	1.18
			dtectl.wcb	1.8
			crid85.c	1.11
			cridtbl.c	1.7
		2.	ctrlio.wcb	1.5
			rptio.wcb	1.5
			ctrltbl.wcb	1.4
			tableio.wcb	1.4
		3.	kcsio.c		1.10
+++++
		
v. 2.64 October 31, 93

Utility:	DATENTRY

Problem:	Using DATENTRY to create a relative file would sometimes
		create a very large file.

Resolution:	Corrected the open output routine for relative files.
-----
This problem was only reported in CREATE, and was caused by not setting the
_space to 0 in an open output.
+++++



v. 2.63 October 20, 1993

Utility:	REPORT

Problem:	The Report utility is not handling titles and headings
		in the same way as the Utility.

Resolution:	Title and heading handling revised to match Wang style
		titles and headings.

-----
MR:		1192
Changes:	Had to change start new page logic such that title
		1 or heading 1 is included in the run_line, and nothing
		is printed for a blank title or line heading.
Modules:	rwrt.c		1.19
+++++	
v. 2.62 Aug 22, 1993

Utility:	ALL Micro Focus COBOL AIX Systems only.

Problem:	A problem in version 3.1 of the COBOL compiler was fixed
		with patch level 10.04. 

Resolution:	The utilities have been recompiled with this fix. There were
		no internal changes to the utilities.

v. 2.61 Aug 22, 1993

Utility:	ALL
Problem:	The criderr.log created during file IO errors is now
		used by all KCSI utilities and is no longer exclusive
		to CONTROL REPORT etc.
Resolution:	Name of the error log changed to kcsierr.log
-----
MR:		1189
Modules:	criderr.c	1.4
+++++
-----
MR:		1188
Modules:	kv3.c		1.13
Changes:	READFDR "RC" option not supported. Changed routine
		to extract file info to i_inof().
+++++

Utility:	INQUIRY

Problem:	Fields for alias names were too short to display
		the entire alias.

Resolution:	Fields lengthened to make room for all 31 characters.

-----
MR:		1187
Modules:	inqnam.wcb	1.3
Changes:	Modified field widths to 31 characters.
+++++


v. 2.60 Aug 3, 1993

Utility:	INQUIRY

Problem:	INQUIRY did not support the use of aliases.

Resolution:	Some limited alias capability has been incorporated
		in INQUIRY. An ALIAS that contains no spaces can
		be used for an inquiry. Thus COMPLETION DATE could
		not be used as an alias, but COMPLETION-DATE could be.
		CONTROL file Aliases are allowed to be 31 characters
		long. INQUIRY will not print all 31 characters, but
		only prints the first 25 as the column
		heading.

		Thus THE-DAY-UPON-WHICH-ALL-WAS-DONE can be used
		to formulate the query, but it will be printed as
		a column heading of:

		THE-DAY-UPON-WHICH-ALL-W

		This provides some alias capability for INQUIRY.
-----
MR:		1186
Changes:	Modified iprs.c and itkn.c to accept tokens longer
		than 8 chars. If longer than 8 they are stored
		in col_head rather than rfl-name.
		added routine to crtlary.wcb to FNDALIAS
		changed validation in inqent.wcb to test for
		col-head values.
Modules:	iprs.c		1.5
		itkn.c		1.4
		ctrlary.wcb	1.3
		inqent.wcb	1.7
+++++
		
-----
v. 2.55.x	for DOS only, incorporated changes from MR 1179
+++++

Utility:	REPORT/INQUIRY  (DOS version only)

Problems:	When Inquiry and Report were run back to back, it
		could sometimes set up conditions that caused Report
		or Inquiry to run incorrectly based on values set for
		the other utility.

Resolution:	The DOS version does not reload the run time, so
		all global variables had to be re-initialized
		for the DOS version. This eliminated the problem.

-----
MR:		1184 and 1185
Changes:	Added logic to clear _old and _new arrays when
		setting up a new report definition.
+++++
v. 2.59 July 15, 1993

Utility:	ALL

Problem:	Acu-COBOL versions required that run with C-ISAM
		files.

Resolution:	File IO module modified to handle C-ISAM files.
		New installation instructions for building a
		C-ISAM version of the WISP/Acu-COBOL/KCSI
		run time.
		There were no other changes to the utilities other
		than at file level, so there will be no visible
		difference in behavior.
-----
MR:		1173 1182
Changes:	Changed all routines in kv3.c to call i_io()
		versions of the routines instead of
		v3_io() routines. There was a problem in
		i_open() as the newest version by
		Acu-COBOL added an extra parameter.
+++++


v. 2.58 June 9, 1993

Utility:	REPORT INQUIRY

Problem:	Certain comparisons for data limits on decimal
		numbers not working correctly.

Resolution:	Comparison logic modified to correctly handle
		decimals.
-----
MR:		1180
Modules:	rcmp.c	1.8
+++++



v. 2.57 June 8, 1993
-----
Pulled TABS out of all COBOL modules
+++++

Utility:	REPORT

Problem:	String fields not being concatenated as New Fields.

Resolution:	Concatenation fixed.

-----
MR:		1179
Changes:	rbld.c logic changed to recognize a " " literal as
		not the end of the specification.
		rglb.c globals changed so that nfo_op_code
		is correctly converted.
		rtie.c some change ?
		rwrt.c added intialization of trecs to zero for each
		report.
Modules:	rbld.c		1.9
		rglb.c		1.11
		rtie.c		1.10
		rwrt.c		1.18
+++++


v. 2.56  May 13, 1993

Utility:	CONTROL and DATENTRY

Problem:	Dynamic Links from CONTROL and DATENTRY need to be
		(S)ystem calls to LINK, as the utilities are not
		correctly located unless in the active directory.

Resolution:	Links changed to (S)ystem calls and utilities
		are now searched for on the user
		path variable.

Utility:	REPORT and INQUIRY

Problem:	Some changes in the way limits are checked caused
		a problem with data limits in REPORT. Second data
		limit was being ignored.

Resolution:	Limits checking fixed.


v. 2.55 May 2, 1993

Utility:	REPORT

Problem:	Data Limit and New Field constants that fill
		the entire 20 character field, are being truncated
		at the last character, thus deleting the the closing
		quote at the end of the field.

Resolution:	Truncation eliminated.
-----
MR:		1012
Changes:	Moved LIT-CODE into the LIT-FIELDs which are
		now defined as 20 bytes in WSRPTREC. Had to Change
		edit and storage in rptlim.wcb rptnfl.wcb rptdnw.wcb
Modules:	rptnfl.wcb	1.4
		rptlim.wcb	1.7
		rptdnw.wcb	1.3
		rptmak.wcb	1.3
		wsrptwrk.wcb
+++++

v. 2.54 Apr 24, 1993


Utility:	REPORT INQUIRY

Problem:	Range values not being matched correctly.

Resolution:	Range handling corrected.
-----
+++++
v. 2.53 Apr 18, 1993

Internal changes for listmgmt.


v. 2.52 April 11, 1993

-----
MR:		1163

Changes:	Added sort of RFL records on exit from rptseq.wcb. This
		keeps everything to do with rfls sorted in sequence
		order.

Modules:	rptseq.wcb	1.2
+++++

Utility:	REPORT

Problem:	When the sequnce of fields selected changes, the Column
		heading screen did not change sequence to match the
		new sequence, unless exit report modify (or add mode)
		and then returned to modify mode.

Resolution:	Column headings now sort with the field sequence.
+++++
-----
MR:		1173
Experimental change of v3_open() etc routines to i_open() etc.
MR:		1162
Bug located in the 7 GETPARMS, some fixes done
Module:		rptmain.wcb	
MR:		1026
INstalled some hooks for field selection 1990 style
+++++

-----
MR:		1169

Changes:	Added a call to RPTFIL in RPTMAIN to get a parm when
		in add report mode.

Modules:	rptmain.wcb	1.5
+++++
Utility:	REPORT

Problem:	A RPTDEF GETPARM was missing when Add mode was selected
		while running REPORT through a procedure. The PUTPARM
		that filled in RPTDEF was not honored.

Resolution:	RPTDEF GETPARM added just before the screen to identify
		the new report definition to be created. If this RPTDEF
		GETPARM is filled in, its values are saved and used.

-----
MR:		1153

Changes:	passing parameters error in call from CTRLEXT TO CTRLERR

Modules:	CTRLEXT.WCB	1.2
+++++

Utility:	CONTROL - VAX only

Problem:	When a changing a control file, if an alternate key
		was added to the key list when the data name for that
		key had not yet been defined, CONTROL crashed.

Resolution:	Problem in linkage located and eliminated.

-----
MR:		1154

Changes:	Re-wisped with INIT=(NODATA) as VAX default is
		to re-init each data item on entry. This caused
		U-MAP and X-MAP to be wiped out every time the program
		was entered.

Modules:	None, but changed in vaxcom.umf to ensure correct
		WISP make is set up.
++++++
Utility:	CONTROL-VAX Only

Problem:	CONTROL was allowing updateable fields to overlap
		one another when adding fields.

Resolution:	Overlap validation corrected.

------
MR:		1167

Changes:	Added wdellock() call for VAX version.

Modules:	rpln.c		1.5
+++++
Utility:	REPORT-VAX Only

Problem:	Creating two report files. One empty and one containing
		the report. Both used the same name with different
		version numbers.

Resolution:	Extra was a 'lock' file being used to reserve the selected
		file name before the actual open. Logic added to remove
		the lock file.

-----
MR:		1168

Changes:	Just re-wisped with INIT=(NODATA). See 1154 above
		for a similar problem
+++++
Utility:	DATENTRY-VAX Only

Problem:	The alternate keys for a file, where they existed, were
		not being displayed as optional paths into the
		data file in modify mode.

Resolution:	Display and select of alternate paths corrected


v. 2.51 Apr 01, 1993

-----
MR:		1166

Changes:	GETPARMS for all files renamed so that they do not match
		RPTDEF, CONTROL, CONTROL2, INPUT and INPUT2 GETPARM
		names. Files are now named RPTDEF, CTRLIN, OPENIN1 and
		OPENIN2.

		Also changed rwrt.c to always generate a PRINT getparm
		even if no records are being printed. THis causes a file
		with a no Records message to be printed if no records
		are selected.

Modules:	rwrt.c		1.16
		rbld.c		1.6
		rcal.c		1.8
		rptwmn.wcb	1.12
		rptio.wcb	1.3
		ctrlio.wcb	1.2

+++++
Utility:	REPORT

Problem:	REPORT was issuing extra getparms causing long procedures
		to get out of synch because REPORT was using up pending
		PUTPARMS that should not have been consumed.

Resolution:	Extra GETPARMS removed or renamed so that they do not
		interfere with the correct course of PUTPARM as the
		utility runs.

-----
MR:		1162

Changes:	Added 7 GETPARMS behind the modify screen to allow
		users to specify RPTDEF, CONTROL, CONTROL2, INPUT1
		and INPUT2 when modifying a REPORT definition.

Modules:	rptmod.wcb	1.4
		rptmain.wcb	1.4
+++++

Utility:	REPORT

Problem:	When requesting a modify on a report definition, no
		GETPARMS were issued by the utility. The Wang version
		did issue GETPARMS.

Resolution:	Hidden GETPARMS for RPTDEF, CONTROL, CONTROL2, INPUT1
		and INPUT2 are now issued if PF3 is selected from the
		main menu.

v. 2.50		March 26, 1993
-----
Removed WISPSORT
+++++
Internal changes with no affect on the utilities

v. 2.49		March 26, 1993
-----
MR:		1164
Modules:	rptwmn.wcb	1.10
+++++ 

Utility:	REPORT

Problem:	Data File getparms INPUT1 and INPUT2 were coming
		up with blank PRNAMES when an error occurred on
		an open.

Resolution:	PRNAMES correctly filled in.


v. 2.48 March 20, 1993		Internal Changes

-----
WISPSORT was taken back out again.
WISPSORT option limited to Acu-COBOL only
rwop.c replaces rptwop.wcb and rwhlp.c replaces rptphlp.wcb
so that l;ogic can be turned on and off by C #ifdef directives.
+++++

v. 2.47 March 12, 1993
-----
MR:		1156
Modules:	kcsio.c		1.7
Changes:	Call kcsio_wfopen for open in IO-mode even
		if INPUT requested.
+++++

Utility:	REPORT

Problem:	Occasional crashes with signal 10 or 11 depending on
		machine.

Resolution:	Traced to file permissions and routines to check
		permissions not matching up. Modified permissions
		routines to veriry that user has read and write
		access at group level in order to access a data file
		control file or report definition file.

-----
MR:		1143
Modules:	rwsrt.c		1.5
		rtie.c		1.9
		rcal.c		1.7
		rptcld.wcb	1.5
		rptwop.wcb	1.5
Changes:	Reinstated EXT sort call to WISPSORT.
++++++

Utility:	REPORT

Problem:	Sort speeds need to be improved.

Resolution:	The IDSI WISPSORT utility is 20% to 30% faster than the 
		internal sort in REPORT. WISPSORT has been installed
		as the default sort type (Acu-COBOL only). This 
		can be changed from the OPTIONS menu by specifying
		WISPSORT=NO.

		WISPSORT produces a non-stable sort as duplicates do not
		appear in order. The internal sort is a stable sort, and 
		duplicates are returned in the order in which they
		appeared in the original input file. If you need a
		stable sort you will need to either specify
		WISPSORT = NO, or work out some sort key that
		will keep the records in the order that you require.



v. 2.46 	March 10, 1993

-----
MR:		1144
Modules:	rfmt.c		1.9
Changes:	Changed the way zero suppressed fields display
		edits and inserts in edit mode.
+++++
Module:		REPORT

Problem:	Inserts not showing up correctly in the edited picture
		of a zero suppressed field.

Resolution:	Display of edit for zero suppressed fields modifed to
		coorrect the display.
----- 
MR:		1139	1152
Modules:	rtie.c		1.8
		rptctlp.wcb	1.3
		rptcld.wcb	1.4
Changes:	Modified rptcld to load the KEY-TO-SEC record into
		element 81. Modified tie_up_rhd to directly access
		element 81 instead of polling all rfl structures
		looking for the key to sec.
+++++

Utility:	REPORT

Problem:	The KCSI version of REPORT expected that the field
		named as key to the secondary file would be picked
		as a report field. The Wang version does not require
		this. REPORT files that were converted from the Wang,
		that named a key to the secondary file that had not
		been selected, were crashing when run as the program
		was unable to locate in the key in the secondary
		record.

Resolution:	REPORT modified to locate and load the key to the 
		secondary file whether the field was selected or
		not.

v. 2.45 March 9, 1993

-----
MR:		1148
Modules:	rptlim.wcb	1.5
Changes:	Modified loop logic to quit before DL-IDX can go to 10
		limits.
+++++

Utility:	REPORT

Problem:	REPORT sometimes locks up when all 10 of the data limits
		are filled.

Resolution:	Lock up eliminated.

-----
MR:		1150
Modules:	rptlim.wcb
Changes:	Modified to accept sinlge quote as a valid field delimiter.
+++++

Utility:	REPORT, INQUIRY

Problem:	INQUIRY allows literals to be enclosed single or double
		quotes. REPORT data limits options only allowed double
		quotes, so data limits created by generating a REPORT
		definition from an INQUIRY were not valid if INQUIRY
		had used single quotes.

Resolution:	Modified REPORT to allow single or double quotes as
		a delimter for a literal field when specifying a
		data limit.

-----
MR:		1149
Problem:	100 Data limits caused the RPT-RECORDS data item to
		exceed 32K bytes, the limit for LPI COBOL.
Modules:	wsrptrec.wcb
		rptprm.h
Changes:	Both modified to initializze smaller set of Data limit
		conditions if LPI COBOL is used for the COMPILE.
+++++


v. 2.44	Mar 4, 1993

-----
MR:		1128
Modules:	rcmp.c		1.6
Changes:	Modifed to match trailing blanks
+++++

Utility:	REPORT and INQUIRY

Problem:	Selection or Data Limit would not work unless the
		selection criteria exactly matched the number
		of trailing blanks. For example a three byte field
		containing "PD " would not be matched if the
		selection request was FIELD="PD". The only way
		to match the field was to use FIELD="PD ". This
		was not the way the Wang utlities did their
		matching, as the Wang Utilities ignored trying
		to match trailing spaces.

Resolution:	Selection criteria for REPORT and INQUIRY modified
		to ignore trailing spaces when doing the
		comparison.

-----
MR:		1129
Modules:	inqent.wcb	1.4
		inqmain.wcb	1.12
Changes:	Removed repeated initialization of query fields.
+++++
Utility:	INQUIRY

Problem:	INQUIRY was clearing the query fields after each inquiry
		causing a need to re-enter a full query even though a
		second or subsequent query might be based on the
		original.

Resolution:	Query fields are no longer cleared, leaving the
		original query intact.


v. 2.43		March 3, 1993

-----
MR:		1146
Modules:	ctrload.wcb	1.2
		ctrlhdl.wcb	1.4
Changes:	Inspect all numeric fields for numeric content
		and move in zero where faulty
+++++

Utility:	CONTROL

Problem:	Control encountering problems with Wang control
		record fields specified as numeric, but containing
		non-numeric data.

Resolution:	CTRLOAD module modified to inspect all numeric
		fields for valid numeric data and correcting
		errors.

-----
MR:		1143
Modules:	rsrt.c		1.3
Changes:	Set unique value to a 9 byte number, and had it
		stop cycling up from 0 each time a duplicate was hit,
		so when a dup is encountered, unique is incremented and
		used and not reset to zero for each record.
+++++
Module:		REPORT

Problem:	The sort in report takes a long time when the sort
		involves duplicate keys.

Resolution:	Sort logic modified to improve handling for
		duplicated keys.


v. 2.42		Feb 22, 1993


Module:		DATENTRY

Problem:	Numeric fields defaulting to an upper entry fac
		when they should be numeric facs and allowing
		the FAC to be modified to UPLOW Entry.

Resolution:	Locked the UPLOW field for numeric items
		when requesting the modify fac options for
		a field.
-----
MR:		1137
Modules:	dtefac.wcb	1.4	1.5
		dmnt.c		1.2
+++++
Module:		REPORT

Problem:	Report was displaying extra GETPARMS which normally
		default in the Wang versions.

Changes:	GETPARMS modified to work as default GETPARMs in the
		same manner as the Wang REPORT utility.
-----
MR:		1133
Modules:	rptfil.wcb	1.2
Changes:	Bug in rptfil was causing a getparm instead of
		defaulting.


-----
Problem:	CONTROL not correctly putting CTLVOL in the putparm
		to call datentry.
		DATENTRY not corrrectly putting VOLUME in the putparm
		to call INQUIRY.
MR:		1134
Changes:	Corrected puptparm to use VOLUME keyword
Modules:	ctrldepp.wcb	1.2
		dteinqpp.wcb	1.2
+++++


v. 2.41 Feb 7, 1993

Module:		CONTROL

Problem:	CONTROL was not imitating Wang behavior of passing
		the CONTROL file values to DATENTRY when calling
		DATENTRY.

Resolution:	These values are now filled in when calling
		DATENTRY.

Module:		DATENTRY

Proiblem:	DATENTRY was not imitating Wang behavior of
		passing Data values to INQUIRY when calling
		INQUIRY.

Resolution:	These values are now filled when calling INQUIRY.
-----
MR:		1120	1121
Changes:	New modules to do putparms, called by DTEMAIN and
		CTRLMAIN.
Modules:	CTRLDEPP.wcb	1.1
		dteinqpp.wcb	1.1
		ctrlmain.wcb	1.5
		dtemain.wcb	1.5
+++++

Module:		REPORT

Problem:	Some RPTDEF conversions from the Wang are causing
		non numeric data to be stored in the occurence
		field in REPORT. This is probably a recent change
		in REPORT on the Wang that allows spaces to be stored
		in that field. This was causing 'Non-numeric' errors
		in REPORT when a newly converted RPTDEF was loaded
		for the first time.

Resolution:	Logic added to check for and handle this condition
		thus preventing anoying but not troublesome
		warning messages.
-----
MR:		1126
Changes:	Added test for NOT NUMERIC in RPTLOD.wcb
Modules:	rptlod.wcb	1.5
+++++
		
Module:		DATENTRY

Problem:	When the number and/or length of modifiable fields
		exceeded the screen size, the message indicating
		that a screen overflow had occurred was causing
		the program to crash. The effect was that DATENTRY
		would appear to crash immediately after entering the
		data file and control file names.

Resolution:	Logic for the warning message corrected.
-----
MR:		1124
Changes:	Missing parameter in call to GETPARM in DTEOOP.
Modules:	DTEOOP.WCB	1.4
+++++


Module:		REPORT

Problem:	Under certain conditions REPORT was refusing to
		accept a secondary input file stating that the
		seconday input file did not match the secondary
		control file definition when in fact it did.

Resolution:	Problem in identifying the layout of a secondary file
		corrected.
-----
MR:		1123
Chnages:	GENERAL-FIELD-1 was being used in two places,
		overwriting the comparison values already in 
		the variable. Replaced with a new variable to
		hold this temporary data.
Modules:	rptwmn.wcb	1.7
+++++

Module:		REPORT

Problem:	VAX version hanging up under certain conditions when
		trying to modify a report.

Resolution:	Error in RMS record holding logic corrected. Hangup
		eliminated.

-----
MR:		1125
Modules:	kvxi.c		1.4
Changes:	Hold when held was returning ALREADY-LOCKED as an
		error that was being translated to an a ERROR-LOCKED
		by vax_trans. Changed to translate to IO-OK.
+++++


v. 2.40 Jan 31, 1993


Module:		INQUIRY

Problem:	VAX inquiries to be formatted as .COM files.

Resolution:	The output inquiry specification is correctly
		formatted for a .COM file, but VAX names the
		file with a .DAT extension. These files need to be
		renamed before they can be run or must be run with
		the .DAT extension.
-----
MR:		1112
Changes:	Added conditionals to igen.c for VAX to insert
		command starters, continuation, and comment logic.
Modules:	igen.c		1.7
+++++

v. 2.39 Jan 31, 1993 Not released
Internal Changes for VAX version

-----
Holding a record is not implemented through the code, so VAX is
failing on deletes and rewrites.

Added holding inside kvxi.c
+++++
v. 2.38 Jan 30, 1993 Not released
Internal Changes for VAX version

-----
The rpt_caller variable was pointing to a NULL if edit fields called
from inside REPORT.
rglb.c		1.8

Internal changes for VAX version

+++++
v. 2.37 Jan 27, 1993	Not Released
Internal Changes for VAX version

-----
Various internal changes for the VAX version.

Changed valvol() to val_vol() because of VAX name conflicts
valflv.c 	1.5
Removed VAX call to WISPSORT
rwsrt.c		1.3
(this module can probably go all away by removing ext sort calls
in rbld.c eventually)
Added dummy calls into global modules, so that VAX linker
would resolve and include the global module.
rglb.c		1.7
rcal.c		1.6
iglb.c		1.2
iprs.c		1.3
dglb.c		1.4
dbsc.c		1.5

Modified KDISP to build print file name as a IS_PRINTFILE
as it was not finding the file to display on the VAX
kdisp.c		1.2

Modified dtekey.c to not use a call to getparmbuild() since VAX
has it not. changed to GP macros, and added gp.h and gp.c to
the library
dtekey.c	1.6
gp.c		1.1

Inquiry name needs to be set up in igen.c, and the iquiry needs
to be created as a .COM file (somehow).

+++++
v. 2.36 Jan 21, 1993

Utility:	REPORT

Problem:	Control breaking logic and what is printed on the report
		does not mirror the logic in the latest version of
		Wang REPORT

Resolution:	The newer version of Wang control breaking
		for Summary ONLY = YES reports was installed.
		In this version a summary ONLY = YES will cause
		the values on which the break is occurring to be.
		For example: A file containing REGION MANAGER SALESPER
		and DOLLARS Can be sorted by REGION, MANAGER and
		SALESPERS. Control Breaks can be specified of;

		Field		Level	Skip	Print each
		REGION		03	00	YES
		MANAGER		02	00	YES
		SALESPER	01	00	YES
	
		With Summary options set to total for the DOLLARS field
		this will result in a Summary ONLY = YES report
		that would look something like this.
		
		SALESPER	MANAGER		REGION	DOLLARS
		0014		001		12A	12,600.00
		0015		001		12A	18,200.00
		0022		001		12A	16,300.00
				001		12A	47,100.00
		0003		002		12A	11,200.00
		0083		002		12A	12,300.00
				002		12A	23,500.00
						12A	60,600.00
		0017		004		13B	14,100.00
		0018		004		13B	15,200.00
				004		13B	29,300.00
						13B	29,300.00
							89,900.00

		The second change in control breaking is the skip
		action. The Wang skip lines action was unpredicatable
		(buggy). A skip lines specification of 0, 1 or 2 would
		usually result in a skip of only 1 line, or sometimes
		0 lines. In this version of REPORT you get what you ask
		for, as in the example above, a skip of 0 was specified
		and no lines were skipped between totals. Wang report
		would have skipped one line between each line even though
		zero skip was requested.

		If you have report definitions already done for
		earlier versions of REPORT, you may have
		to alter your control break skip lines count
		to adjust for these changes.
		
		
-----
MR:		1100
Changes:	To line feeding and Control breaking logic
Modules:	rwrt.c 		1.15
+++++

Utility:	CONTROL REPORT INQUIRY DATENTRY	LISTMGMT

Problem:	Under the lastest version of the Wang utilities, a
		blank file org defaults to 'I'ndexed. It used to
		default to 'C'onsecutive in earlier versions
		which default is currently reflected in the CRID
		utilities.

Resolution:	All utilties set up to default to Indexed
		when the file organization field is blank
		in the control file.

-----
MR:		1095
Changes:	Modified ctrlhdl.wcb to check for a blank
		FILE-ORG immediately after the header record
		is read in, and to insert an 'I' file type.
Modules:	ctrlhdl.wcb	1.3
+++++
-----
Fixes for VAX port
MR:		1096 1101
Changes:	Changes to #include file syntax needed for VAX
Modules:	rbld.c		1.4
		rsrt.c		1.2
		rwsrt.c		1.2
		dtedat.c	1.2
		rpln.c		1.4

MR:		1097
Changes:	Too many GETPARM parameters in RPTWOP for VAX.
Modules:	rptwop.wcb	1.4

MR:		1098
Changes:	Equivalent defines needed for VADX version
Modules:	cridvers.c	2.36
		igen.c		1.4
		itkn.c		1.3
		rcvt.c		1.12
		daux.c		1.7

MR:		1099
Changes:	KEXISTS and DBSC causing duplicate names in VAX
		C which is not case sensitive
Modules:	kexists.c	1.4
		dbsc.c		1.4
+++++


v. 2.35 Jan 20, 1993

Utility:	INQUIRY

Problem:	Number of conditions was too limited for a
		complex INQUIRY. It was limited to 10 fields
		with 10 conditions per field.

Resolution:	Number of fields changed to 100 with 10 conditions
		per field.
		If more than 10 conditions for 1 field are required,
		the field can be repeated with another 10 conditions
		Ex:
		FIELD1 EQ "A" OR EQ "B" OR EQ "C" OR EQ "D" OR EQ "E"
		OR EQ "F" OR EQ "G" OR EQ "H" OR EQ "I" OR EQ "J"
		OR FIELD1 EQ "K" etc.
		The above example would be better handled by a range
		limit, and is only for illustration purposes.
-----
MR:		1088
Changes:	Added processing of 100 limits in rcal and rtie.
		Added 100 limit to parsing routines in iprs.c.
		Found error in REPORT that was preventing 
		a field name from appearing in two sets of limits.
		This was a useless restriction as it was the only way
		for long tests to be entered.
		Changed rsel so that it tests all 100 conditions
		for all rpt_caller's except 'R'.
		
Modules:	WSRPTREC.WCB	(increased to 100 occurrences)
		rtie.c		1.7
		rcal.c		1.5
		iprs.c		1.2
		rtplim.wcb	1.4
		rsel.c		1.6

+++++		

Utility:	REPORT and INQUIRY

Problem:	The utility not correctly recognizing a limit
		(or INQUIRY condition) that included blanks at
		the end of the field. 
		WHEN FIELD1 = "XY " (in INQUIRY) or
		FIELD1 EQ "XY " (in report)
		would not match a field in which FIELD1 actually
		contained "XY ".

Resolution:	Matching conditions adjusted so that testing
		is done by all field characters even when trailing
		characters include spaces.
-----
MR:		1089
Changes:	Changed data type for the string conversion of
		a data limit literal to a CSTR instead of a BTRN.
		The BTRN was a CSTR with blank truncation causing
		the trailing blanks to be lost.
Modules:	rglb.c		1.6
+++++		



v. 2.34 Jan 18,1993 Not Released

Utility:	DATENTRY

Problem:	When a file had reached an at end condition using
		PFKey 5 to 'next' through the file, subsequent
		presses of PFKey 5 to read the next record
		were causing an 'File corrupted' error to
		be logged in criderr.log. The file was in fact not
		corrupted. Acu-COBOL version.

Resolution:	Logging action removed for this condition.


v. 2.33 Jan 17, 1993 Not Released

Utility:	ALL

Problem:	DOS Acu-COBOL versions.

Resolution:	This version includes the release of the of
		the DOS Acu-COBOL versions of CONTROL, REPORT,
		INQUIRY and DATENTRY.

v. 2.32 Jan 13, 1993 Not Released

-----
Problem:	Sequence Fields needed for LISTMGMT
MR:		1087
Changes:	Changed RPT_RFL struct to be int _lseq[4]
		Changed rglb.c to load _lseq[0] instead of _seq
	
Modules:	rglb.c		1.5
		rcal.c		1.4
		rwrt.c		1.11 - 1.13
+++++
v. 2.31	Jan  5, 1993 Not Released

-----
Problem:	LISTMGMT needs selection by record number
MR:		1086
Changes:
Modules:	rsel.c		1.5
		rcmp.c		1.5
+++++
v. 2.30 Jan  5, 1993 Not released

-----
Problem:	LISTMGMT needed space suppression for numerics
MR:		1082
Changes:	rfmt.c	logic for if *rpt_caller < ':' (or > ' ')
Modules:	rfmt.c		1.7
Generally numerous changes have been made in internal report routines
causing them to exit to listmgmt output case or space handling
levels rwrt.c and rfmt.c are most heavily affected.
+++++
Utility:	DATENTRY

Problem:	Was not allowing the default fac to be changed
		for screen entry.

Resolution:	FAC handling corrected.

-----
MR:		1083
Changes:	FAC was being seen as a negative number therefore
		< 0x40. Handled by masking
Modules:	dbsc.c		1.3
+++++
-----
MR:		1081
Changes:	Bug in seqio.c not removing newline from read records.
Modules:	seqio.c		1.4
+++++
-----
MR:		1080
Changes:	Output logic for LISTMGMT can be handled inside report
		as we are writing to a flat file as well with different field
		separators. File naming needs to be modified to allow
		for the user specified names.
Modules:	rlmg.c		1.1
		Added this module to imitate rpln.c, but output
		is to an ASCII file instead of a print file.
+++++ 
v. 2.29  Dec 29, 1992  Not Released
-----
MR:		1079
Changes:	Added skip_space and skip_zeroes elements to rpt_dl
		structure for this selection option in listmgmt.
		made same_element() global
Modules:	rptprm.h
		rtie.c		1.6
+++++
-----
MR:		1055
Changes:	Removed swapsign() logic in rbld.c which was faulty
		and causing errors.
		Also located a NULL pointer in rtie.c
Modules:	rbld.c		1.2
		rtie.c		1.5
Notes:		This fix was placed on sonny as 2.27.1 for distribution
		to Xerox as they reported the problem
+++++

Utlity:		REPORT

Problem:	Occasional memory errors reported in reports
		with user defined (NEW) fields.

Resolution:	Bug located in sign handling of user defined numeric
		fields that was at least some of the cause of the
		error.

-----
v. 2.28	Dec 15, 1992 Not Released
+++++ 
-----
MR:		1075
Changes:	Extended number of limit fields to 101 in rglb.c
		to accomodate the 100 limits allowed by LISTMGMT
Modules:	rglb.c		1.4

Notes:		There are more changes needed to complete this.
Utility:	LISTMGMT
Problem:	Need 100 limit fields for LISTMGMT
Resolution:	Changes to rglb.c to extend no of limits.
+++++
-----
MR:		1077
Changes:	Added ctrldif.wcb called from dtectl.wcb
		rptwmn.wcb and inqmain.wcb
Modules:	ctrldif.wcb	1.1
		dtectl.wcb	1.6
		inqmain.wcb	1.11
		rptwmn.wcb	1.7
+++++
-----
MR:		1076
Changes:	Forced ctrlhdl.wcb to load a filetype of (F)ixed
		while reading records.
Modules:	ctrlhdl.wcb	1.2
+++++
Utility:	REPORT INQUIRY DATENTRY

Problem:	When a control file and data file do not match,
		the error message is too cryptic to be able to
		determine the problem.

Resolution:	Changed to provide a detailed display of
		Record length, Primary and alternate key definitions
		in order to be able to better isolate 
		problems.
-----
MR:		1075
Changes:	Listmgmt needs 100 data limits, Report
		only allows 10. Increased size to 100
Modules:	rglb.c		1.4
		rptprm.h	added constant LMG_DL_ENTRY_COUNT
+++++
-----
MR:		1074
Changes:	Added C type calls in kexsts.c and valflv.c
		for use by listmgmt.
Modules:	kexists.c	1.2
		valflv.c	1.4
+++++
-----
MR:		1072
Changes:	Added call to to LISTMOPT.wcb that generates a
		hidden GETPARM LISTMGMT in ctrlmain.wcb,
		and added optional menu ctrloptl.wcb. If Keyword
		LISTMGMT is changed to "Y", then ctrlopl.wcb menu
		is called instead of ctrlop.wcb.
Modules:	ctrlmain.wcb	1.4
		ctrloptl.wcb	1.1
		listmopt.wcb	1.1
+++++
-----
Utility:	CONTROL DATENTRY

Problem:	LISTMGMT access to CONTROL requires a
		slightly different front menu.

Resolution:	Hidden GETPARM added to CONTROL that is
		accessed by LISTMGMT when it calls CONTROL.
		This changes the front menu when called from
		CONTROL, but does not affect CONTROL's normal
		behavior when not called from LISTMGMT.

+++++
		

v. 2.27 Dec 2, 1992

-----
MR:		1071
Changes:	Modified rptctlp.wcb to include initialization for
		CF-IS-BINARY along with CF-IS-ZONED CF-IS-PACKED etc.
Modules:	rptctlp.wcb	1.2
		rptedt.wcb	1.2
+++++
Utility:	REPORT

Problem:	Binary decimal fields would default to COMMA=YES
		in the field edit options.

Resolution:	Now Defaulting to NO

-----
MR:		1070
Changes:	Modified klpi.c which was using ISEQUAL for a hold_next
		on a started file to ISCURR.
Modules:	klpi.c		1.3
		rptmak.wcb	1.2
+++++

Utility:	ALL but only showing up in REPORT

Problem:	HOLD-NEXT was not working correctly for LPI and 
		MicroFocus Isam files causing a record-not found
		status to be incorrectly returned. This showed
		up as REPORT records that would not modify. For
		example a change made to a field edit would disappear
		after the file was saved.

Resolution:	Hold next logic corrected.

-----
MR:		1069
Changes:	rcvt.c and daux.c had to changed to convert
		BINARY types to AHMN (machine type) for
		Microfocus which uses a machine order
		binary.
Modules:	rcvt.c		1.8 - 1.10
		daux.c		1.6
+++++

Utility:	CONTROL REPORT INQUIRY DATENTRY

Problem:	The Micro Focus equivalent of a WANG binary
		field is a COMP-5 which has a machine dependent
		byte order.

Resolution:	All utilities had to be modified
		to determine machine order for control fields
		described as binaries. This implies that data file
		conversions will have to take into account
		the machine order for binaries and convert them
		to the correct order for the target machine.


v. 2.26 Nov 21, 1992

-----
MR:		1068
Changes:	Changed library for a created report definition
		to usrRPT instead of OUTLIB
Modules:	inqrpt.wcb	1.4
+++++
Utility:	INQUIRY

Problem:	When saving a report definition from an inquiry,
		the output library defaulted to OUTLIB instead
		of userid + RPT.

Resolution:	Output library for saved report definitions 
		defaults to userid + RPT.

-----
MR:		1067
Changes:	Increased PASS-CHANGE to X(03)
Modules:	inqdat.wcb	1.5
+++++
Utility:	INQUIRY

Problem:	The shell script generated to execute an inquiry
		was attempting to enter CHANGE=YE for a change in
		control file, as the YES/NO option was being
		truncated to 2 characters

Resolution:	Truncation eliminated. Change is now correctly
		entered as CHANGE=YES.

-----
MR:		1066
Changes:	Computing PFKEY MASK in dtectl.wcb on the fly.
Modules:	dtectl.wcb	1.5
+++++
-----
MR:		1065
Changes:	Added test for open in ksam_close logic
Modules:	klpi.c		1.2
+++++

Utility:	ALL

Problem:	An error in the close logic for LPI and Micro
		Focus versions was causing a spurious error
		to be logged on certain file closes.

Resolution:	Close logic corrected.


v. 2.25 Nov 16, 1992 Not Released
-----
MR:		1049	(Micro-Focus conversion)
Changes:	Added Micro-Focus Zoned handling in rcvt.c
		Removed extraction of COBOL status directly from mffcd
Modules:	rcvt.c		1.7
		kcsio.c		1.3
+++++
-----
MR:		1064
Changes:	Added validation to disallow relative key 0 in
		add mode.
Modules:	dadd.c		1.5 & 1.6
+++++
-----
MR:		1063
Changes:	Added rel_field on and off to ddel.c and dadd.c
		to ensure that the rel_record field is explicitly 
		FACed.
Modules:	ddel.c		1.3
		dadd.c		1.4
+++++

Utility:	DATENTRY

Problem:	The RELATIVE RECORD field for relative files in ADD
		mode sometimes gets locked up and will not accept
		data entry. Also allowed entry of zero as a relative
		record number.

Resolution:	Correct initialization for the FAC of the relative
		record number field installed, and validation
		routines modified to prevent entry of zeroes.


v. 2.24 Nov 13, 1992

-----
MR:		1061
Changes:	modified rpln.c to use wfclose to close print files
Modules:	rpln.c		1.3
+++++
Utility:	REPORT and INQUIRY

Problem:	Print files were being incorrectly queued, or queued
		twice, and disposition was not being correctly set
		to Delete.

Resolution:	Print file close routines corrected to queue the
		files once with correct disposition set.


-----
MR:		1036	(LPI version)
Changes:	Corrected sign overpunching for acu and lpi versions
Modules:	rcvt.c		1.6
+++++
v. 2.23 11 Nov, 1992 Not Released

-----
MR:		1058
Changes:	Added plswait screen back in
Modules:	inqmain.wcb	1.10
		rptwmn.wcb	1.6
+++++
Utility:	REPORT and INQUIRY

Problem:	Processing screens not displayed while waiting for
		REPORT or INQUIRY results.

Resolution:	Please Wait .... Processing screens added and display
		if the task is running in foreground.

-----
MR:		1057
Changes:	Changed ctrlval to call VALFNM to validate
		field name, and added VALFNM C routine
Modules:	ctrlval.wcb	1.2
		valflv.c	1.2
+++++
Utility:	ALL

Problem:	Control field validation was not allowing a '#'
		to appear as a character in a field name.

Resolution:	Field validation corrected to allow
		valid field names to contain A-Z, 1-9, @, #, -,
		and $.

-----
MR:		1056
Changes:	Removed all use of ext_len as a field value
		& changed all logic to compute and use edit_len
		and edit_pic.
Modules:	dbsc.c		1.2
		piclen.c	1.2
		dglb.c		1.3
		daux.c		1.2
+++++ 
Utility:	DATENTRY

Problem:	Converted control files that used a different
		external length than the default length were
		not behaving correctly in DATENTRY causing
		memory problems and "Level not found" errors.

Resolution:	DATENTRY modified to use default external
		lengths for all DATENTRY as is done in the
		Wang VS environment.

-----
		Problems with VALNAME and VALVOL not working
		correctly under different versions of COBOL.
		Had to change then once for acucobol, now
		Microfocus is complaining about them.
		Also trouble with KEXISTS
MR:		1053
Changes:	Replaced valvol.wcb and valnam.wcb with 'C' versions
		in valflv.c and KEXISTS in kexists.c
Modules:	valflv.c
		kexists.c
++++++
-----
MR:		1052
Changes:	Added changing double quotes to single quotes
		for second half of inquiry field.
Modules:	igen.c		1.3
+++++
Utility:	INQUIRY

Problem:	Incorrect handing of quoted fields in a saved
		inquiry.

Resolution:	Corrected so that double quotes are consistently
		converted to single quotes



v. 2.22 Oct 27, 1992 Not Released

-----
MR:		1047
Changes:	Removed setting of file status in unlock()
		in relio.c seqio.c and kv3.c. This was causing
		file status on a rewrite to be lost.
Modules:	relio.c		1.5
		seqio.c		1.3
		kv3.c		1.3
+++++
Utility:	INQUIRY

Problem:	Rewrites on a file that caused a duplicate
		alternate key when no duplicates were allowed
		were not returning a correct file status. The
		record would not be rewritten, but no message
		appeared.

Resolution:	File status corrected for rewrites, and screen
		now correctly presents a Recprd Already on File
		message.

-----
MR:		1048
Changes:	Changed INQXTR to initialize the CONSEC field
		outside the main loop. Now if an error occurs
		on the screen, CONSEC does not get re-set to NO.
Modules:	inqxtr.wcb	1.6
+++++
-----
MR:		1040 1042
Changes:	Changed initialization of SAVQRY-LIB in inqmak.wcb
		and added wfopen() to igen to create necessary
		directories and added saving and PUTPARMing
		of the value of CONSEC.
Modules:	inqmak.wcb	1.4
		igen.c		1.2
+++++
Utility:	INQUIRY

Problem:	When saving an inquiry, the inquiry file could
		not be saved in a directory that did not exist,
		and the output library defaulted to OUTLIB instead
		of userid + INQ.

Resolution:	Inquiry files can now be saved in new directories
		provided the user's access allows creation of
		directories. Output library for saved queries defaults
		to USERID + INQ.
		The output PUTPARM for a saved Query now includes
		the value for CONSEC that was entered on that screen.	 

-----
MR:		1041
Changes:	Added "YES" and "REL" as valid entries to inqxtr.wcb
		and pass these back as 'C' or 'R'. Changed RPTCALL
		(rcal.c) to pass the file org from the COBOL ufb
		to set_inq_org() in iwrt.c. Added setting 'C' or 'R'
		types for the open, and added increment of rel_key
		for writing to a rel_file.
Modules:	iwrt.c		1.4
		rcal.c		1.3
		seqio.c		1.2
		inqxtr.wcb	1.5
+++++
Utility:	INQUIRY

Problem:	Would not create a consecutive output file when
		using the EXTRACT option.

Resolution:	If CONSEC on the EXTRACT screen is set to YES,
		the output will be a consecutive format with
		new line terminators. CONSEC will also accept
		a 'REL' option which will output the record
		as is with new line terminators.

-----
MR:		1038
Changes:	inqmain.wcb changed rpt-option to YES
Modules:	inqmain.wcb	1.8
+++++
Utility:	INQUIRY

Problem:	Not producing a final record count when displaying
		the results of an inquiry.

Resolution:	Total records selected not displayed at the end
		of the listing.

-----
MR:		1039
Changes:	Fix in rptspc.wcb forcing 2 when space = 0
Modules:	rptspc.wcb 	1.2
+++++
Utility:	REPORT

Problem:	Would not allow spaces between fields to be set to 0.

Resolution:	Now Accepts zero spacing.

-----
MR:		1037
Changes:	Complete write of dtekey.c which was a dummy routine 
		before. Also added (but then didn't need) an extra
		TEST-INT passed to CTRLVERS INQVERS etc. used
		to test the binariness of COBOL binaries.
Modules:	dtekey.c	1.2
		crid85.c	1.2
		cridvers	this one
		ctrfil.wcb	1.2
		rptopt.wcb	1.5
		inqdat.wcb	1.4
		dtectl.wcb	1.4
+++++
Utility:	DATENTRY

Problem:	Change mode for alternate key files was not prsenting
		an key path selection screen, so all changing had to be
		done on the primary key path.

Resolution:	Key path screen is now available for alternate keyed
		files in change mode. Delete mode still requires
		primary key path access.

-----
MR:		1045
Changes:	Added record to screen (dadd.c) and added key validation
		(dkey.c) and some changes to relio.c to handle
Modules:	dadd.c		1.2
		relio.c		1.4
		dkey.c		1.2
+++++
Utility:	DATENTRY

Problem:	Add mode for relative files was not requesting a
		relative record number.

Resolution:	Record number and record validation now appears
		on the add mode screen.

-----
MR:		1044 1033
Changes:	relio.c repositioning to record after a delete
		ddel.c delete_the_record() causing two load_next_rec()
		to happen
Modules:	relio.c		1.3
		dtemnu.wcb	1.4
		ddel.c		1.2
+++++
Utility:	DATENTRY

Problem:	After a delete on a relative or indexed 
		record the record pointer
		was not being positioned correctly.

Resolution:	Next record is now being displayed after a
		delete on a relative or keyed file.


-----
MR:		1046
Changes:	kv3.c W_DUP_OK value 101 was being returned as
		EBADFILE (default). Changed to return 0
Modules:	kv3.c		1.2
+++++
Utility:	DATENTRY

Problem:	Keyed files with duplicate alternates were returning
		a Record on file status when writing a record with
		an alternate key that was already on file. The record
		was being correctly written, but the screen displayed
		an erroneous "Record already on file" message.

Resolution:	File status corrected, and message no longer appears.


v. 2.21 July 8,1992

-----
MR:		1034 1035
Changes:	Modified rfmt.c to accept new formatting
Modules:	rfmt.c		1.5
+++++
Utility:	REPORT REPORTW

Problem:	Zero suppression of Z0 causes no suppression
		when it should cause full field suppression,
		and Z0 with a $ caused a dollar sign to be
		printed with a blank field when the field
		should have been completely blank.

Resolution:	Zero and dollar sign suppression corrected
		for 'Z0' and '$' type fields.


-----
MR:		1028
Changes:	rptlim.wcb modified to search for closing quote
		instead of next space.
Modules:	rptlim.wcb	1.3
+++++
Utility:	REPORT

Problem:	A data limit specification in a report
		that contained embedded spaces such as
		FIELD1 NE "DO " produced an incorrect
		error message of
		Literal must be delimited by double quotes.

Resolution:	Embedded spaces are now accepted.

-----
MR:		1027
Changes:	Added key_to_sec global in rglb.c
		Modified rptcld.wcb to extract the index to the field
		that is the key to the secondary.
		This field is passed back and forth in
		rptcld.wcb rcal.c and rptwmn.wcb.
		Added dummies to inqmain.wcb for call to rcal.wcb
		Added an additional parameter to RPTCALL in crid85.c
Modules:	rglb.c
		rtie.c
		rcal.c
		crid85.c
		rptwmn.wcb
		rptcld.wcb
		inqmain.wcb	
+++++
Utility:	REPORT REPORTW

Problem:	When specifying a report using two files the
		key to the secondary file must have the same
		field name as the field named in the primary 
		file. When this is not the case, the REPORT
		writer portion of the utility returns a
		CONTROL File and RPTDEF files do not match
		error.

Resolution:	This problem as fixed allowing different field names.


-----
MR:		1029
Changes:	Added routines to rcvt.c to convert to and from
		basic numeric formats of zoned, unsigned, binary
		and packed
Modules:	rcvt.c
+++++ 
Utility:	REPORT REPORTW INQUIRY

Problem:	Utilities required that the primary and
		secondary keys be identical in length and
		data type to work correctly.

Resolution:	Conversion routines modified to allow the key to
		the secondary file to be any length or data type.
		If the field in the primary file is characters,
		then the field in the secondary must be characters.
		If the field in the primary is numeric (zoned,
		unsigned, binary or packed) then the field in the
		secondary may be any of the numeric types and
		the values will be converted.

Notes:		The key field for the secondary file must always
		be selected as a field to use in the report,
		even if it is given a sequence of 99 preventing
		it being reported upon. It must be selected, or 
		reading of the secondary file will not take place
		correctly.


-----
MR: 		1030 1031 
Changes:	mods to rcvt.c to correctly handled zoned negatives
Modules:	rcvt.c
+++++
Utility:	ALL     

Problem:	Under certain circumstances, Zoned negatives
		in ACU-COBOL were being converted to positive
		values.

Resolution:	Conversions were corrected so that the sign
		of the field is preserved.


v. 2.20 July 8,1992

Utility:	INQUIRY

Problem:	When creating an EXTRACT file from an INQUIRY
		the INQUIRY was terminating with a fatal error
		10 if the library specified did not exist for
		the new output file.

Resolution:	Corrected. The directory and file will be created
		if the user has sufficient access rights to the
		directory in which the new directory and file
		are to be created.
-----		MR 1024 modified iwrt.c to call wfopen
+++++ 

Utility:	INQUIRY

Problem:	INQUIRY had no option to specify a consecutive
		file as an alternative form of an EXTRACT file.

Resolution:	Corrected. INQUIRY will now create a consecutive
		file if requested. The file format is equivalent to 
		the ACU-COBOL binary sequential file described in
		programmer's guide section on Sequential files.

-----		MR's 1025 modified iwrt.c inqmain.wcb and inqxtr.wcb
		to provide the needed screen fields, and pass them
		in to the iwrt() routine.
+++++


v. 2.18  6/27/92 - Not Released

Utility:	CONTROL REPORT INQUIRY DATENTRY REPORTW

Problem:	None

Resolution:	Internal changes to module names which will
		have no effect as far as the end user is
		concerned.


v. 2.17  6/22/92 - Not Released

-----
Utility:	REPORT DATENTRY INQUIRY

Problem:	Displays were too messy with no screen blanking.

Resolution:	Restored screen-blanking, but modified blnksc.wcb
		to test forground/background and not execute
		in back ground. Restored calls to blnksc in
		dtemain inqent inqmain rptwmn rptmnu rptpdt
		rptphlp rptsdt.
+++++
Utility:	CONTROL REPORT INQUIRY DATENTRY REPORTW

Problem:	ENOREC (RECORD-NOT-FOUND) is a valid 
		return code from a failed start but was being
		logged as a file error in criderr.log.

Resolution:	Removed from ccsioerr as a log-able error.
-----		MR 1018 affected ccsioerr.c 1.3.
+++++


Utility:	CONTROL REPORT INQUIRY DATENTRY REPORTW

Problem:	Error logging was split into several
		files.

Resolution:	All error logging is now done into criderr.log
		in the current directory for utility errors, as
		well as the usual wisperr.log in the users $HOME
		directory for wisp errors.
-----		MR 1017 affecting cridebug.c 1.2 and ccsioerr.c 1.2
+++++


Utility:	REPORT

Problem:	New fields defined with an initial literal
		field would not convert correctly if the the new
		field was defined as numeric.

Resolution:	Conversion problem fixed.
-----		MR 1010 affected rcvt.c 1.2
+++++


Utility:	REPORT

Problem:	New fields could not be defined with
		a literal that started with a sign or a decimal.

Resolution:	Fixed New field Literals may start with signs or
		decimal characters.
-----		MR 1015 affected rptdnw.wcb 1.2
+++++


Utility:	REPORT INQUIRY

Problem:	Zero suppression on fields with 
		decimal values was not being done correctly.
		A field defined as having 2 decimals with a zero
		suppression of Z1 would print as  ". 0" when the
		the field value was zero. A floating dollar sign
		on the same field would print as ".$0".

Resolution:	Corrected field formatting logic to imitate WANG
		logic and the above fields now print correctly
		as "0" and "$0".
-----		MRs 1003 1004 affected rfmt.c 1.2 zero_supp() routine
		and added is_zero() routine.
+++++


Utility:	REPORT

Problem:	Control break fields were not printing
		on the first record, if the control break action
		for a field specified not to print the field on
		on each line.

Resolution:	Fixed - Values now print correctly.
-----		MR 1005 Affected modules rwrt.c 1.3
+++++


Utility:	REPORT

Problem:	In modify mode would allow an
		invalid file to be named as the report
		definition file, but would lock up and
		not allow the user to exit via PFKey 16.

Resolution:	Fixed and PFkey 16 will now allow exit.
-----		MR 1007 affected rptmod.wcb 1.2
+++++
-----		Also removed calls to BLNKSC in rptmnu.wcb
		1.2 rptpdt.wcb 1.2, rptphlp.wcb 1.2,
		rptsdt.wcb 1.2 which were missed in
		the earlier passes.
+++++


v. 2.16 6/21/92	- Not Released

Utility:	INQUIRY

Problem:	Would not accept commas (,) as field 
		delimiters on a query screen.

Resolution:	Commas now accepted as a valid delimiter.
-----		MR 1008 Affected itkn.c 1.2
+++++ 
-----
Utility:	REPORT INQUIRY DATENTRY REPORTW

Problem:	BLNKSC call is redundant in all
		modules, and causing background problems
		in REPORT and INQUIRY.

Resolution:	All calls to BLNKSC removed.
		MR 10014 Affected rptwmn.wcb 1.3, inqmain.wcb 1.3
		inqent.wcb 1.2 dtemain 1.2
+++++


v. 2.15 6/21/92 - Not Released

Utility:	REPORT REPORTW

Problem:	If a field were specified for summary
		options; Minimum, Maximum, or Average, a literal
		was printed for each of these options. This literal
		continued to be printed even if the field in
		question were then given a sequence of 99. The 99
		corrrectly eliminated the field from the report
		but the summary option literals were still printed
		at the end of the report.

Resolution:	Fixed - Setting a field to 99 now eliminates the
		summary option literals.
-----		affected Modules rwrt.c
+++++


v. 2.14 6/20/92 - Not Released

Utility:	REPORT REPORTW INQUIRY

Problem:	"Please Wait" messages being displayed by REPORT and
		INQUIRY were interfering with background running when
		REPORT or INQUIRY were submitted to background.

Resolution:	"Please Wait" message removed from both utilities.
-----		Affected modules rptwmn.wcb inqmain.wcb
+++++


v.  2.13 6/20/92

This version consolidates CONTROL, REPORT, INQUIRY, and DATENTRY under
a single version numbering system. All of the utilities shared comnmon
routines such that any single modification will usually affect more
than one of the programs. Using a single version number for all four
utilities makes version tracking easier.

This release incorporates what were originally:

CONTROL		v. 1.50.27
REPORT		v. 1.70.02
REPORTW		v. 1.70.02
INQUIRY		v. 1.30.38
DATENTRY	v. 2.10.15


Known Bugs and Problems as of version 2.74 :

Utility:	REPORT

Problem:	When defining a secondary file for report,
		no checking is done to verify that the secondary
		file is indexed. Using a relative or consecutive
		file as a secondary file in REPORT can cause
		unpredictable results.



Utility:	REPORT

Problem:	In various places while defining a report,
		the edit checks for a subscripted field are very
		exacting about the name of the field. For example
		if a field named VALUE occurs 3 times, some of
		the screens require that the individual elements
		be defined as a eight character field name padded
		with blanks followed by parenthesis enclosing a 2
		digit subscript. Thus VALUE(3) would not be
		an acceptable entry, but VALUE   (03) would be
		accepted. 

-----		MR1012
+++++
Utility:	REPORT

Problem:	When specifying data limits, the input field is
		20 characters long. This should allow a literal
		of 18 characters plus the opening and closing
		quote marks. If a literal is defined using the
		the full 20 character field, the closing quote
		is not stored correctly in the file.

-----
MR:		1153
+++++
Utility:	CONTROL - (VAX version only)

Problem:	When adding an alternate key to an existing CONTROL file,
		if the field to be used as an alternate has not been
		defined, it causes a stack dump. On non-VAX versions this
		situation correctly displays the error message that
		the field requested has not been defined.

Work Around:	Ensure that fields to be added as alternates are defined
		before adding them as alternates.

-----
MR:		1154
+++++
Utility:	CONTROL - (VAX version only)

Problem:	The utility allows updateable fields to be entered that
		overlap each other in the record, and does not
		provide the error message that fields overlap.

Work Around:	Do not define overlapping updateable fields.



EndRNotes
-------*/

/*
**	Structures and Defines
*/
#define STRING_OUT(X)  # X
#define STRING_VERSION(X) STRING_OUT(X)

#ifdef DEBUG
#define DEBUG_STR " (DEBUG)"
#else
#define DEBUG_STR ""
#endif

/*
# To change the version for KCSI, change KCSI_VERSION here. 
#	wisp/src/kcsi/cridvers.c
#	wisp/src/kcsi/version.c
#	wisp/src/kcsi/kcsilibs.umf
#	wisp/src/kcsi/kcsicob.mak
#	wisp/src/kcsi/kcsi_relnotes.txt
#	wisp/src/kcsi/kcsintsetup.txt
#	wisp/src/kcsi/kcsi_acu_install.txt
#	wisp/src/kcsi/kcsi_mf_install.txt
#	wisp/src/kcsi/kcsi_packlist.txt
#	wisp/src/doc/wisp_relnotes.txt
#	wisp/src/acu/wruncbl.umf
#	wisp/src/acu/wrun32wisp_kcsi_acu50.mak
#	wisp/src/acu/wrun32wisp_kcsi_acu51.mak
#	wisp/src/acu/wrun32wisp_kcsi_acu52.mak
*/
#define KCSI_VERSION	4200

/*
**	Static data
*/


/*
**	ROUTINE:	crid_version()
**
**	FUNCTION:	Return the crid version string
**
**	DESCRIPTION:	Form a version string from KCSI_VERSION of the form v9.9.99 and return it.
**
**	ARGUMENTS:	?
**
**	GLOBALS:	?
**
**	RETURN:		Pointer to version string const
**
**	WARNINGS:	?
**
*/
const char *crid_version(void)
{
	static char the_version[20]; /* Big enough to hold "v9.9.99 (DEBUG)" */
	static int first = 1;

	if (first)
	{
		char buf[10];
		sprintf(buf,"%04d", (int)(KCSI_VERSION));
		sprintf(the_version,"v%c.%c.%c%c%s",buf[0],buf[1],buf[2],buf[3],DEBUG_STR);
		first = 0;
	}

	return the_version;
}

static char machine[]=
#ifdef KCSI_UNIX
"u";
#endif
#ifdef KCSI_WIN32
"w";
#endif

static char compiler[]=
#ifdef KCSI_MFX
"m";
#endif
#ifdef KCSI_ACU
"a";
#endif

/* CHANGE-COPYRIGHT-DATE */

static char logoformat[]=
"     %-9s %s.%s%s - (c) KCSI/Shell Stream Software LLC";

static char logo[80];

static void init_bin_test(char *bin);

void CTRLVERS(char *wvers,char *bin)
{
	sprintf(logo,logoformat,"CONTROL",crid_version(),machine, compiler);
	memset(wvers,' ',79);
	memcpy(wvers,logo,strlen(logo));
	init_crid_debug();
	init_bin_test(bin);
}


void RPTVERS(char *wvers,char *bin,char *style)
{
	sprintf(logo,logoformat,"REPORT",crid_version(),machine, compiler);
	memset(wvers,' ',79);
	memcpy(wvers,logo,strlen(logo));
	init_crid_debug();
	init_bin_test(bin);
	KCSI_init_report_style(style);
}

void INQVERS(char *wvers,char *bin)
{
	sprintf(logo,logoformat,"INQUIRY",crid_version(),machine, compiler);
	memset(wvers,' ',79);
	memcpy(wvers,logo,strlen(logo));
	init_crid_debug();
	init_bin_test(bin);
}

void DATVERS(char *wvers,char *bin)
{
	sprintf(logo,logoformat,"DATENTRY",crid_version(),machine, compiler);
	memset(wvers,' ',79);
	memcpy(wvers,logo,strlen(logo));
	init_crid_debug();
	init_bin_test(bin);
}

static int save_bin = 0;

static void init_bin_test(char *bin)
{
	save_bin = bin[1];
}

int KCSI_use_binary()  /* NOT USED */
{
	return(save_bin);
}

void KCSI_init_report_style(char *style)
{
	char *ptr;

	memcpy(style, "89", 2);

	ptr = getenv("REPORTSTYLE");
	if(ptr)
		memcpy(style, ptr, 2);
}


/*
**	History:
**	$Log: cridvers.c,v $
**	Revision 2.111  2010/01/16 02:04:28  gsl
**	new release
**	wisp 5.1.00
**	kcsi 4.2.00
**	
**	Revision 2.110  2007/01/03 14:11:43  gsl
**	copyright 2007
**	
**	Revision 2.109  2005/01/03 19:12:06  gsl
**	Copyright year 2005
**	
**	Revision 2.108  2003/04/11 18:34:16  gsl
**	Add support for Acucobol 5.0 and 5.1
**	
**	Revision 2.107  2003/03/17 21:39:24  gsl
**	KCSI Version 4.1.00
**	
**	Revision 2.106  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 2.105  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 2.104  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 2.103  2003/01/24 20:38:53  gsl
**	Change year to 2003
**	
**	Revision 2.102  2002/10/22 17:59:01  gsl
**	Combined KCSI
**	
**	Revision 2.101  2002/10/22 16:00:32  gsl
**	Combine CRID_VERSION and CREATE_VERSION into KCSI_VERSION
**	
**	Revision 2.100  2002/10/21 14:06:09  gsl
**	CRID_VERSION is now set in cridvers.c for unix
**	
**	Revision 2.99  2002/10/17 21:22:40  gsl
**	cleanup
**	
**	Revision 2.98  2002/10/17 17:17:15  gsl
**	Removed VAX VMS code
**	
**	Revision 2.97  2002/09/04 13:13:36  gsl
**	CRID 3.0.05
**	
**	Revision 2.96  2002/08/29 21:07:05  gsl
**	Move the CRID version define into cridvers.c and document places to change
**	
**	Revision 2.95  2002/07/25 15:20:30  gsl
**	Globals
**	
**	Revision 2.94  2002/06/25 18:18:33  gsl
**	Remove WISPRETURNCODE as a global, now must go thru set/get routines
**	
**	Revision 2.93  2002/06/25 17:46:03  gsl
**	Remove WISPFILEXT as a global, now must go thru set/get routines
**	
**	Revision 2.92  2002/06/21 20:48:16  gsl
**	Rework the IS_xxx bit flags and now include from wcommon.h instead of duplicate
**	definitions.
**	
**	Revision 2.91  2002/03/19 18:28:39  gsl
**	Change copyright date
**	
**	Revision 2.90  2001-09-04 16:47:40-04  gsl
**	Change to 9.9.99 style version number
**
**	Revision 2.89  2000-06-13 18:40:49-04  gsl
**	Copyright 2000
**
**	Revision 2.88  1998-03-25 14:00:36-05  gsl
**	change copyright date
**
**	Revision 2.87  1997-08-18 17:23:52-04  scass
**	Added NEOM to header display
**
**	Revision 2.86  1997-03-26 08:44:28-05  gsl
**	Change to NeoMedia Technologies
**
**	Revision 2.85  1996-12-12 13:42:05-05  gsl
**
**	Revision 2.84  1996-10-09 09:56:03-07  gsl
**	add include stdlib
**
**	Revision 2.83  1996-09-18 11:35:34-07  gsl
**	combine some code
**
**	Revision 2.82  1996-09-17 16:45:30-07  gsl
**	drcs update
**
**
**
*/
