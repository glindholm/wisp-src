ALTER TABLE [dbo].[utb_contacts] DROP CONSTRAINT FK_contacts_cust_number
GO

ALTER TABLE [dbo].[utb_license_docs] DROP CONSTRAINT FK_license_docs_cust_number
GO

ALTER TABLE [dbo].[utb_license_keys] DROP CONSTRAINT FK_license_keys_cust_number
GO

ALTER TABLE [dbo].[utb_license_keys] DROP CONSTRAINT FK_license_keys_license_type_code
GO

ALTER TABLE [dbo].[utb_license_keys] DROP CONSTRAINT FK_license_keys_platform_code
GO

ALTER TABLE [dbo].[utb_license_docs] DROP CONSTRAINT FK_license_docs_license_key
GO

ALTER TABLE [dbo].[utb_validation_codes] DROP CONSTRAINT FK_validation_codes_license_key
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_license_docs]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_license_docs]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_validation_codes]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_validation_codes]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_contacts]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_contacts]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_license_keys]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_license_keys]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_customers]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_customers]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_license_types]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_license_types]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[utb_platforms]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[utb_platforms]
GO

if exists (select * from systypes where name = N'udt_contact_name')
exec sp_droptype N'udt_contact_name'
GO

if exists (select * from systypes where name = N'udt_cust_name')
exec sp_droptype N'udt_cust_name'
GO

if exists (select * from systypes where name = N'udt_cust_number')
exec sp_droptype N'udt_cust_number'
GO

if exists (select * from systypes where name = N'udt_doc_id')
exec sp_droptype N'udt_doc_id'
GO

if exists (select * from systypes where name = N'udt_doc_type')
exec sp_droptype N'udt_doc_type'
GO

if exists (select * from systypes where name = N'udt_email')
exec sp_droptype N'udt_email'
GO

if exists (select * from systypes where name = N'udt_flag')
exec sp_droptype N'udt_flag'
GO

if exists (select * from systypes where name = N'udt_license_date')
exec sp_droptype N'udt_license_date'
GO

if exists (select * from systypes where name = N'udt_license_key')
exec sp_droptype N'udt_license_key'
GO

if exists (select * from systypes where name = N'udt_license_type_code')
exec sp_droptype N'udt_license_type_code'
GO

if exists (select * from systypes where name = N'udt_license_type_name')
exec sp_droptype N'udt_license_type_name'
GO

if exists (select * from systypes where name = N'udt_machine_id')
exec sp_droptype N'udt_machine_id'
GO

if exists (select * from systypes where name = N'udt_operator')
exec sp_droptype N'udt_operator'
GO

if exists (select * from systypes where name = N'udt_phone_number')
exec sp_droptype N'udt_phone_number'
GO

if exists (select * from systypes where name = N'udt_platform_code')
exec sp_droptype N'udt_platform_code'
GO

if exists (select * from systypes where name = N'udt_platform_name')
exec sp_droptype N'udt_platform_name'
GO

if exists (select * from systypes where name = N'udt_validation_code')
exec sp_droptype N'udt_validation_code'
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_contact_name', N'char (100)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_cust_name', N'char (100)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_cust_number', N'numeric(6,0)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_doc_id', N'int', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_doc_type', N'char (10)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_email', N'char (100)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_flag', N'bit', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_license_date', N'datetime', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_license_key', N'char (19)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_license_type_code', N'int', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_license_type_name', N'char (10)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_machine_id', N'char (50)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_operator', N'char (50)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_phone_number', N'char (30)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_platform_code', N'char (2)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_platform_name', N'char (50)', N'not null'
GO

setuser
GO

setuser N'dbo'
GO

EXEC sp_addtype N'udt_validation_code', N'char (3)', N'not null'
GO

setuser
GO

CREATE TABLE [dbo].[utb_customers] (
	[cust_number] [udt_cust_number] NOT NULL ,
	[cust_name] [udt_cust_name] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[utb_license_types] (
	[license_type_code] [udt_license_type_code] NOT NULL ,
	[license_type_name] [udt_license_type_name] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[utb_platforms] (
	[platform_code] [udt_platform_code] NOT NULL ,
	[platform_name] [udt_platform_name] NOT NULL ,
	[platform_active] [udt_flag] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[utb_contacts] (
	[cust_number] [udt_cust_number] NOT NULL ,
	[contact_name] [udt_contact_name] NOT NULL ,
	[contact_email] [udt_email] NULL ,
	[contact_email2] [udt_email] NULL ,
	[contact_phone] [udt_phone_number] NULL ,
	[contact_phone2] [udt_phone_number] NULL ,
	[contact_fax] [udt_phone_number] NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[utb_license_keys] (
	[license_key] [udt_license_key] NOT NULL ,
	[cust_number] [udt_cust_number] NOT NULL ,
	[platform_code] [udt_platform_code] NOT NULL ,
	[license_type_code] [udt_license_type_code] NOT NULL ,
	[license_date] [udt_license_date] NOT NULL ,
	[expiration_date] [udt_license_date] NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[utb_license_docs] (
	[doc_id] [udt_doc_id] IDENTITY (1, 1) NOT NULL ,
	[doc_type] [udt_doc_type] NOT NULL ,
	[licensee] [udt_cust_name] NOT NULL ,
	[cust_number] [udt_cust_number] NOT NULL ,
	[license_key] [udt_license_key] NOT NULL ,
	[platform_name] [udt_platform_name] NOT NULL ,
	[license_type_name] [udt_license_type_name] NOT NULL ,
	[license_date] [udt_license_date] NOT NULL ,
	[expiration_date] [udt_license_date] NULL ,
	[machine_id] [udt_machine_id] NULL ,
	[validation_code] [udt_validation_code] NULL ,
	[operator] [udt_operator] NOT NULL ,
	[create_date] [udt_license_date] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[utb_validation_codes] (
	[license_key] [udt_license_key] NOT NULL ,
	[machine_id] [udt_machine_id] NOT NULL ,
	[validation_code] [udt_validation_code] NOT NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[utb_customers] WITH NOCHECK ADD 
	CONSTRAINT [PK_customers_cust_number] PRIMARY KEY  CLUSTERED 
	(
		[cust_number]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_license_types] WITH NOCHECK ADD 
	CONSTRAINT [PK_license_types_license_type_code] PRIMARY KEY  CLUSTERED 
	(
		[license_type_code]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_platforms] WITH NOCHECK ADD 
	CONSTRAINT [DF_utb_platforms_platform_active] DEFAULT (1) FOR [platform_active],
	CONSTRAINT [PK_platforms_platform_code] PRIMARY KEY  CLUSTERED 
	(
		[platform_code]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_contacts] WITH NOCHECK ADD 
	CONSTRAINT [PK_contacts_cust_number_contact_name] PRIMARY KEY  CLUSTERED 
	(
		[cust_number],
		[contact_name]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_license_keys] WITH NOCHECK ADD 
	CONSTRAINT [PK_license_keys_license_key] PRIMARY KEY  NONCLUSTERED 
	(
		[license_key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_license_docs] WITH NOCHECK ADD 
	CONSTRAINT [PK_license_docs_docnum] PRIMARY KEY  CLUSTERED 
	(
		[doc_id]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_validation_codes] WITH NOCHECK ADD 
	CONSTRAINT [PK_validation_codes_license_key_machine_id] PRIMARY KEY  CLUSTERED 
	(
		[license_key],
		[machine_id]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[utb_contacts] ADD 
	CONSTRAINT [FK_contacts_cust_number] FOREIGN KEY 
	(
		[cust_number]
	) REFERENCES [dbo].[utb_customers] (
		[cust_number]
	)
GO

ALTER TABLE [dbo].[utb_license_keys] ADD 
	CONSTRAINT [FK_license_keys_cust_number] FOREIGN KEY 
	(
		[cust_number]
	) REFERENCES [dbo].[utb_customers] (
		[cust_number]
	),
	CONSTRAINT [FK_license_keys_license_type_code] FOREIGN KEY 
	(
		[license_type_code]
	) REFERENCES [dbo].[utb_license_types] (
		[license_type_code]
	),
	CONSTRAINT [FK_license_keys_platform_code] FOREIGN KEY 
	(
		[platform_code]
	) REFERENCES [dbo].[utb_platforms] (
		[platform_code]
	)
GO

ALTER TABLE [dbo].[utb_license_docs] ADD 
	CONSTRAINT [FK_license_docs_cust_number] FOREIGN KEY 
	(
		[cust_number]
	) REFERENCES [dbo].[utb_customers] (
		[cust_number]
	),
	CONSTRAINT [FK_license_docs_license_key] FOREIGN KEY 
	(
		[license_key]
	) REFERENCES [dbo].[utb_license_keys] (
		[license_key]
	)
GO

ALTER TABLE [dbo].[utb_validation_codes] ADD 
	CONSTRAINT [FK_validation_codes_license_key] FOREIGN KEY 
	(
		[license_key]
	) REFERENCES [dbo].[utb_license_keys] (
		[license_key]
	)
GO

