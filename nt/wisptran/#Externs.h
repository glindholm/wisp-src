#if !defined EXTERNS_H
#define EXTERNS_H

extern cApplication	cApp;
extern cWindows		cWnd;
extern struct _GlbFlags {
	BOOL isTranslating,
		isCompiling;
}GlbFlags;

#endif
