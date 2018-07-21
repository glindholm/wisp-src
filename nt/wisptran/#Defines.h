/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

#if !defined  DEFINES_H
#define DEFINES_H
//	General
#define AppName		"WISP Translator and Compiler Interface (v1.5)"									//	Name of the application
#ifdef _DEBUG
#define DEBUG_TAG "(DEBUG)"
#else
#define DEBUG_TAG ""
#endif
#define OPWndName		"Output Window"										//	Generic name for output window
#define OPBufSize		1048576													//	Size of character buffer for output window
#define OutputWndID	1															//	Control ID for the edit control in the output window
#define MaxSwitch		259														//	The maximum length of a command line switch
#define OutputBlock	2048														//	Size (in bytes) of text sent to the output window
#define MaxCmdLine	1024														//	Maximum size of a command line
//#define MaxFileSpec	1024														//	Maximum length of a dir path and file name
//#define MaxPath		1024
//#define MaxFileLen	256
#define MaxExt			5															//	Maximum length of a file name extension including the dot
#define GenText		512														//	Generic text - i.e. haven't decided how big to make the actual string sizes, a temporary band aid
//		Temporary place holders
#define AppIcon		LoadIcon ( NULL, IDI_APPLICATION );				//	Temporary placeholder for app icon
#define StatusIcon	LoadIcon ( NULL, IDI_APPLICATION );				//	Temporary placeholder for app cursor
//	Window sizes and coordinates
#define TabHeight			23														//	The height of the option sheet tabs
#define TabAdjust			6
#define TabWidth			100													//	The width of the option sheet tabs
#define OptionsHeight	250													//	The height of the options window
#define StatusBarHeight 18														//	The height of the Status Bar window
#define CmdLineWndHt		20
#define CmdLineLblWdth	85
#define CmdLineWndStart	90
//	Tab IDs - Used when iterating the tab windows
#define WISPTab	0																
#define COBOLTab	1
//	For Active Tab field - Used to track the active option window
#define AT_WISP	0
#define AT_COBOL	1
//	Option Sheet controls - Used to position controls in the WISP options window
#define CtlHeight	15
#define CtlWidth	225
#define CtlHSpace	200
#define CtlVSpace 19
//	Control Types
#define RadioButton		0
#define EditControl		1
#define PushButton		2
#define ComboBoxControl	3
#define DontMatter		100
//	Menu Positions
#define FILE_MENU 0
#define WISP_MENU	1
//	Custom Window Messages
#define CWM_QUITEXECUTE	WM_USER+100

//	Button Text
#define TranslateBtnTxt	"Translate"
#define CancelBtnTxt		"Cancel"
#define CompileBtnTxt	"Compile"
#define TransCompBtnTxt	"Translate && Compile"
//	Values for cApplication::Environment.CurAppStatus
#define cas_StartUp			0
#define cas_ExecWISP			1
#define cas_BuildCmdLine	2
#define cas_ShutDown			3
#define cas_Running			4
//	Current Language Options
#define CL_Acu		0
#define CL_MF		1

//	KeyMapping constants
//	COBOL sheet control IDs - used to set the tab order
#define KMC_ACU				0
#define KMC_MF					1
#define KMC_TARGETFILE		2
#define KMC_TARGETBRS		3
#define KMC_COMPILER			4
#define KMC_COMPILERBRS		5
#define KMC_FLAGS				6
#define KMC_OUTPUTDIR		7
#define KMC_OUTPUTDIRBRS	8
#define KMC_OBJEXT			9
//	WISP sheet control IDs - used to set the tab order
#define KMW_TARGETFILE		0
#define KMW_TARGETFILEBRS	1
#define KMW_1					2
#define KMW_C					3
#define KMW_x					4
#define KMW_q					5
#define KMW_S					6
#define KMW_D					7
#define KMW_m					8
#define KMW_f					9
#define KMW_w					10
#define KMW_L					11
#define KMW_l					14
#define KMW_R					12
#define KMW_T					15
#define KMW_e					13
#define KMW_4					16
#define KMW_I					17
#define KMW_IBrs				18
#define KMW_K					19
#define KMW_KBrs				20
#define KMW_P					21
#define KMW_PBrs				22
#define KMW_O					23
#define KMW_OBrs				24
#define KMW_W					25
#define KMW_WBrs				26
#define KMW_V					27
//	Arguments for TranComp
#define TC_Translate	0
#define TC_Compile	1
#define TC_TranComp	2
//	Used with the GetFileAttributes function
#define FA_COMPRESSED 0
#define FA_ARCHIVE 1
#define FA_DIRECTORY 2
#define FA_SYSTEM 3
#define FA_HIDDEN 4
#define FA_READONLY 5
#define FA_NORMAL 6
#define FA_TEMP 7

//	Registry Keys
#define RK_WBin                 "Software\\NeoMedia\\WISP\\WISPBin"
#define RK_WBin_WTran           "Software\\NeoMedia\\WISP\\WISPBin\\WISPTran"
#define RK_WBin_WTran_COBOL     "Software\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL"
#define RK_WBin_WTran_COBOL_Acu "Software\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL\\ACUCOBOL"
#define RK_WBin_WTran_COBOL_MF  "Software\\NeoMedia\\WISP\\WISPBin\\WISPTran\\COBOL\\MFCOBOL"

#endif

/*
**	History:
**	$Log: #Defines.h,v $
**	Revision 1.10  2003/06/18 16:43:06  gsl
**	Add CVS header and history
**	
**
*/
