# WISP Source

This repo contains the source for building WISP.

WISP (Wang Interchange Source Processor) is a product for migrating 
Wang VS Cobol applications to Linux/Unix or Windows with Acucobol or
Micro Focus Cobol.

## Build Instuctions

WISP can be built on Linux/Unix or Windows.

Build instructions for Unix/Linux are here: [portunix.txt](doc/portunix.txt)

Build instructions for Windows are here: [portwin32.txt](doc/portwin32.txt)

## Organization

| Folder     | Description                                                    |
| ---------- | -------------------------------------------------------------- |
| acu        | Acucobol files                                                 |
| amu        | WISP Application Management Utility                            |
| costar     | Co\*Star and W4W                                               |
| doc        | Source documentation                                           |
| ede        | EDE Source                                                     |
| etc        | Sample WISP config files                                       |
| ivslib     | International VS - Chinese character support                   |
| kcsi       | KCSI Utilities - CONTROL, REPORT, INQUIRY, DATENTRY and CREATE |
| mf         | Micro Focus Cobol files                                        |
| nt         | Windows Only utilities - WCONFIG and WISPTRAN                  |
| port       | Porting utilities and make files                               |
| proctran   | Wang VS Procedure Translation utility                          |
| sswsort    | SyncSort version of wsort utility                              |
| test       | Testing and QA                                                 |
| videocap   | Videocap Terminal configuration system                         |
| videolib   | Video Terminal I/O runtime library                             |
| videotest  | Test and QA for Videolib                                       |
| vsedit     | VSEDIT editor                                                  |
| wauth      | WISP runtime license generator                                 |
| wispcommon | WISP common source                                             |
| WISPer     | WISPer build utility source (java)                             |
| wisplib    | WISP runtime library                                           |
| wisptran   | WISP VS Cobol Translator                                       |
| wisputils  | WISP Utilities                                                 |
| wprint64   | WPRINT64 - A 64-bit Print Queuing Utility for WIN32            |
| wproc      | Wang VS Procedure interpreter                                  |

## WISP File Extentions

WISP uses some custom file extensions.

| Extention | File Type                                                |
| --------- | -------------------------------------------------------- |
| .wps      | Wang VS Procedure source                                 |
| .wcb      | Wang VS Cobol source                                     |
| .cob      | Translated Cobol source (either Acucobol or Micro Focus) |
| .acu      | Acucobol compiled objects                                |
| .umf      | Unix make files                                          |
| .mak      | Normally Windows nmake files                             |
| .vcap     | Videocap terminal configuration file                     |
| .rules    | make file include                                        |
