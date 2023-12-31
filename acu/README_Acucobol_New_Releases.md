Copyright (c) Shell Stream Software LLC. All Rights Reserved.

# Acucobol Extend - New Releases

When a new version of Acucobol (Micro Focus) Extend is released,
the procedure for building an Acucobol runtime with WISP will need to
be updated.

## Build Procedure

This document gives instruction on how to modify the Acucobol + WISP runtime build procedure.

## Unix/Linux Instructions

This section has instructions for updating the build procedure on unix/linux.

Files involved:

| File                       | Description                                                              |
| -------------------------- | ------------------------------------------------------------------------ |
| `src/acu/wruncbl.umf`      | The makefile for building the Acucobol + WISP runtime                    |
| `src/acu/acucobol.include` | Makefile variables that get included in `wruncbl.umf`                    |
| `src/acu/acuXXX/`          | Folder for files releated to Acucobol release XX.X                       |
| `src/acu/acuXXX/sub85.c`   | The modified version of `sub85.c` for WISP for this Acucobol release     |
| `${ACUDIR}/lib/sub85.c`    | The original (unmodified) version of `sub85.c` for this Acucobol release |
| `${ACUDIR}/lib/Makefile`   | The orginal makefile from this Acucobol release                          |
|                            |                                                                          |

You will need to make two updates:

- Create file `src/acu/acuXXX/sub85.c` from the `${ACUDIR}/lib/sub85.c` that comes with Acucobol
- Update file `src/acu/acucobol.include` with build information from `${ACUDIR}/lib/Makefile`

### Creating file `src/acu/acuXXX/sub85.c`

Create the folder `src/acu/acuXXX/` where XXX is replaced by the Acucobol release.
For example Acucobol 10.3 uses folder `src/acu/acu103/`.
This location will be referenced in file `src/acu/acucobol.include`.

The file `src/acu/acuXXX/sub85.c` is a modified version of the file `${ACUDIR}/lib/sub85.c`.
With all of the WISP mofification contained in `#ifdef WISP` conditional statements.

Take a look at `src/acu/acu103/sub85.c` and apply the same modifications to the new file.

### Update file `src/acu/acucobol.include`

Edit file `src/acu/acucobol.include` to add information for the new release of Acucobol.

The easiest way to discover what modifications are needed is to diff the `${ACUDIR}/lib/Makefile`
with an earlier `Makefile` of the pervious last supported release. The diff will show you what
changes are needed to made to `src/acu/acucobol.include`.

The file `src/acu/acucobol.include` contains documentation at the top that describes what
each of the variables means.

You need to create a new set of variables for the Acucobol release.
Follow the existing pattern in the file.

### Build the Acucobol plus WISP Runtime

Follow the instructions in `src/acu/wruncbl.umf` to build the new Acucobol plus WISP runtime.
You will need to follow the instructions in `port/portunix.txt` to build the WISP libraries
before you can build the Acucobol runtime.

## Windows Instructions

This section has instructions for updating the build procedure on Windows.

Files involved:

| File                                 | Description                                                           |
| ------------------------------------ | --------------------------------------------------------------------- |
| `src\acu\acuXXX\`                    | Folder for files releated to Acucobol release XX.X                    |
| `src\acu\acuXXX\sub85.c`             | The modified version of `sub85.c` for WISP for this Acucobol release  |
| `src\acu\acuXXX\wrundll.vcxproj`     | The modified VS Project file to build `wrun32.dll` that includes WISP |
| `%ACUDIR%\AcuGT\lib\sub85.c`         | The original (unmodified) version of `sub85.c` from Acucobol          |
| `%ACUDIR%\AcuGT\lib\wrundll.vcxproj` | The orginal `wrun32.dll`` VS Project file from this Acucobol release  |
|                                      |                                                                       |

You will need to create two new files by modifying the originals from Acucobol:

- Create file `src\acu\acuXXX\sub85.c` from the `%ACUDIR%\AcuGT\lib\sub85.c` that comes with Acucobol
- Create file `src\acu\acuXXX\wrundll.vcxproj` from `%ACUDIR%\AcuGT\lib\wrundll.vcxproj`

### Creating file `src\acu\acuXXX\sub85.c`

Create the folder `src\acu\acuXXX\` where XXX is replaced by the Acucobol release.
For example Acucobol 10.3 uses folder `src\acu\acu103\`.

The file `src\acu\acuXXX\sub85.c` is a modified version of the file `%ACUDIR%\AcuGT\lib\sub85.c`.
With all of the WISP mofification contained in `#ifdef WISP` conditional statements.

Take a look at `src\acu\acu103\sub85.c` and apply the same modifications to the new file.

### Creating file `src\acu\acuXXX\wrundll.vcxproj`

Copy file `%ACUDIR%\AcuGT\lib\wrundll.vcxproj` to `src\acu\acuXXX\wrundll.vcxproj` and edit it
to make the required modifications to add the WISP libraries.

The easiest way to discover what modifications are needed is to diff and earlier version
of `src\acu\acuXXX\wrundll.vcxproj` against the original.

For example, diff-ing `src\acu\acu103\wrundll.vcxproj` 
against `src\acu\acu_source\acu1031\wrundll.vcxproj` will show the needed changes.

The following updates are needed:

- Add `_CRT_SECURE_NO_DEPRECATE` to the `<PreprocessorDefinitions/>` in `<ClCompile/>`
- Add `<Library Include="wisp.lib" />` and `<Library Include="video.lib" />` to the list of libraries

### Build the Acucobol plus WISP Runtime

Modify the instructions in `src\acu\acu103\build_wisp_acu103_rts.txt` for the new `acuXXX` 
folder location to build the new Acucobol plus WISP runtime (`wrun32.dll`).

