/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
******************************************************************************
*/

/*
**	File:		#Defines.h
**
**	Project:	WISPTran Configuration Utility
**
**	Purpose:	Declares defines that are used in the application
**
*/

#ifndef DEFINES_H
#define DEFINES_H

#define REGKEY_NEOMEDIA_BASE			"SOFTWARE\\NeoMedia"

#define REGKEY_WISP				"SOFTWARE\\NeoMedia\\WISP"
#define REGKEY_WISP_ACP				"SOFTWARE\\NeoMedia\\WISP\\ACP"
#define REGKEY_WISP_LICENSE			"SOFTWARE\\NeoMedia\\WISP\\License"
#define REGKEY_WISP_VERSIONS			"SOFTWARE\\NeoMedia\\WISP\\Versions"
#define REGKEY_WISP_VIDEOCAP			"SOFTWARE\\NeoMedia\\WISP\\VIDEOCAP"

#define REGKEY_WISP_VSSUBS			"SOFTWARE\\NeoMedia\\WISP\\VSSUBS"
#define REGKEY_WISP_VSSUBS_EXTRACT		"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\EXTRACT"
#define REGKEY_WISP_VSSUBS_MESSAGE		"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\MESSAGE"
#define REGKEY_WISP_VSSUBS_SCRATCH		"SOFTWARE\\NeoMedia\\WISP\\VSSUBS\\SCRATCH"

#define REGKEY_WISP_WISPBIN			"SOFTWARE\\NeoMedia\\WISP\\WISPBin"
#define REGKEY_WISP_WISPBIN_DISPLAY		"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\DISPLAY"
#define REGKEY_WISP_WISPBIN_WPROC		"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WPROC"
#define REGKEY_WISP_WISPBIN_WISPTRAN		"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTRAN"
#define REGKEY_WISP_WISPBIN_WISPTRAN_COBOL	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTRAN\\COBOL"
#define REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_ACU	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTRAN\\COBOL\\ACUCOBOL"
#define REGKEY_WISP_WISPBIN_WISPTRAN_COBOL_MF	"SOFTWARE\\NeoMedia\\WISP\\WISPBin\\WISPTRAN\\COBOL\\MFCOBOL"

#define REGVAL_WISP_SERVER		"SERVER"
#define REGVAL_WISP_WISPDIR		"WISPDIR"
#define REGVAL_WISP_WISPCONFIG		"WISPCONFIG"
#define REGVAL_WISP_PATH		"PATH"
#define REGVAL_WISP_TMPDIR		"TMPDIR"
#define REGVAL_WISP_WISPSORTMEM		"WISPSORTMEM"
#define REGVAL_WISP_USERSDIR		"USERSDIR"

#define REGVAL_ACP_ACPCONFIG		"ACPCONFIG"
#define REGVAL_ACP_ACPMAP		"ACPMAP"

#define REGVAL_LICENSE_FILE		"FILE"
#define REGVAL_VERSIONS_WISP		"WISP"

#define REGVAL_VIDEOCAP_WISPTERM	"WISPTERM"
#define REGVAL_VIDEOCAP_VIDEOCAP	"VIDEOCAP"

#define REGVAL_EXTRACT_WISPCPU		"WISPCPU"
#define REGVAL_EXTRACT_WISPNETID	"WISPNETID"

#define REGVAL_MESSAGE_SHAREDIR		"SHAREDIR"

#define REGVAL_SCRATCH_WISPSCRATCHMODE	"WISPSCRATCHMODE"

#define REGVAL_WISPBIN_DISPLAY		"DISPLAY"
#define REGVAL_WISPBIN_WEDITOR		"WEDITOR"
#define REGVAL_WISPBIN_WISP		"WISP"

#define REGVAL_DISPLAY_WISPDISPLAY8BIT	"WISPDISPLAY8BIT"

#define REGVAL_WPROC_WPROC		"WPROC"
#define REGVAL_WPROC_WPROCDEBUG		"WPROCDEBUG"

#define REGVAL_WISPTRAN_HSIZE		"HSize"
#define REGVAL_WISPTRAN_VSIZE		"VSize"
#define REGVAL_WISPTRAN_WORKDIR		"WorkDir"

#define REGVAL_COBOL_LANGUAGE		"Language"
#define REGVAL_COBOL_COB		"COB" 
#define REGVAL_COBOL_COBFLAGS		"COBFLAGS" 
#define REGVAL_COBOL_OUTDIR		"OUTDIR" 
#define REGVAL_COBOL_OBJEXT		"OBJEXT" 

#endif 

/*
**	History:
**	$Log: #Defines.h,v $
**	Revision 1.3  2007/08/07 20:14:18  gsl
**	Refactor registry logic in prep for vista.
**	Refactor all the registry keys and values to be defined constants.
**	
**
*/
