--
-- Load License Types
--
DELETE utb_license_types
GO

INSERT INTO utb_license_types ( [license_type_code], [license_type_name] )
VALUES ( '1', 'SINGLE')

INSERT INTO utb_license_types ( [license_type_code], [license_type_name] )
VALUES ( '2', 'UNLIMITED')

INSERT INTO utb_license_types ( [license_type_code], [license_type_name] )
VALUES ( '3', 'TIMED')

INSERT INTO utb_license_types ( [license_type_code], [license_type_name] )
VALUES ( '4', 'CLUSTER')

INSERT INTO utb_license_types ( [license_type_code], [license_type_name] )
VALUES ( '5', 'NETWORK')

GO

--
-- Load Platforms
-- 
DELETE utb_platforms
GO

-- Active
INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "AX",	"AIX RISC", '1')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "HP",	"HP/UX 9000 RISC", '1')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SO",   "Sun Solaris 2", '1')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SC",	"SCO UNIX 386/486", '1')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "A1",	"OSF/1 (ALPHA)", '1')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "WN",	"WINDOWS NT", '1')


-- Inactive
INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "UL",	"Ultrix (DEC RISC)", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SU",	"SunOS (Sparc)", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "S3",	"Sun 3 (68020)", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SI",	"Solaris 2 (Intel)", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "DG",	"DG/UX Aviion", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "DI",	"DG/UX Intel", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "NC",	"NCR 386/486", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "NT",	"NCR Tower/32 68020", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "MI",	"Mips R2000", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "AT",	"AT&T 3b2", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "BU",	"BULL DPX/2", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "MO",	"Motorola 88000", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "US",	"Unisys 6000 SVR4", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SQ",	"Sequent", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "AP",	"AIX PS/2", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "D1",	"OSF/1 (DEC RISC)", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "A3",	"AIX 3090", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "NX",	"NeXT 68040", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "HI",	"HP MPE/iX", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SX",	"Stratus FTX", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "IC",	"ICL DRS 6000", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "AL",	"ALTOS 68020", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "CC",	"CONCURRENT", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "CD",	"CONTROL DATA", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "CV",	"CONVERGENT", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "AM",	"AMIGA 3000UX", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "NE",	"NEC", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "ND",	"NIXDORF TARGON", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "PR",	"PRIME", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "PY",	"PYRAMID", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "SY",	"SONY NEWS", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "WY",	"WYSE", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "UW",	"UNIXWARE", '0')


INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "VM",	"VAX/VMS", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "DS",	"MS-DOS", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "HM",	"HP MPE", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "VA",	"VMS (ALPHA)", '0')


INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "UA",	"Ultrix (ALPHA)", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "UV",	"Ultrix (VAX)", '0')


INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "UK",	"Unknown", '0')

INSERT INTO utb_platforms ( [platform_code], [platform_name], [platform_active] )
VALUES ( "QU",	"Unknown", '0')


GO

