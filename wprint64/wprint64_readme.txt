Copyright 2011 (c) Shell Stream Software LLC, All rights reserved.

WPRINT64 - A 64-bit Print Queuing Utility
-----------------------------------------

At the time of this writing there is a bug on all versions of Windows 64-bit 
servers that prevents multiple users from printing with 32-bit programs. The 
WISP runtime is a 32-bit program.

Microsoft Knowledge Base issue KB972616 describes the problem (but not a fix). 
See http://support.microsoft.com/kb/972616 

Specifically in the section “More Information” you will see this paragraph:

	On a computer that is running a 64-bit version of Microsoft Windows, 
	only one user account may print from a 32-bit program in a single 
	session. In a single session, the user account that prints first is 
	the only user account in which a 32-bit process can print, until a 
	time-out occurs or the session ends. If another user account in the 
	same session tries to print before the session ends, the user account 
	receives an "Invalid Handle" error message. Additionally, the print 
	request is unsuccessful.
	
Unfortunately the recommended “Hot Fix” doesn’t actually address the big 
problem, it only allows you to shorten the time you have to wait after the 
user that printed exits before the next user can print.

If you are running WISP migrated applications on a 64-bit Windows server then 
you will need to use a 64-bit print utility to avoid this problem. 

Shell Stream Software has developed the utility WPRINT64 to address this 
issue. This utility is available for no charge to licensed WISP users upon 
request.

See the WPRINT64 usage instructions (wprint64_usage.txt) for setup and 
configuration information.

