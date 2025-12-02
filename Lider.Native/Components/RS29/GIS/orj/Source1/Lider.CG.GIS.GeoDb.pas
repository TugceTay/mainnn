//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Encapsulation of a SQL file access.
  Currently support: MS Jet (Access), MS SQL Server, Interbase, Firebird,
  MySQL, DB2, Sybase, PostgreSql, BlackFishSqk, Informix, Oracle, Advantage,
  Sqlite and SapDB dialects.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoDb ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoDb"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Variants,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoLayer ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  /// <summary>
  ///   On add new field event.
  /// </summary>
  /// <param name="_uidname">
  ///   name of UID field
  /// </param>
  /// <param name="_name">
  ///   name of field to be added
  /// </param>
  /// <param name="_type">
  ///   type of field
  /// </param>
  /// <param name="_width">
  ///   width of field
  /// </param>
  /// <param name="_decimal">
  ///   decimal places
  /// </param>
  /// <param name="_saved">
  ///   True, if field exist in an original file
  /// </param>
  /// <param name="_binary">
  ///   binary width (used only for TAB/DAT files)
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_FIELDEXIST
  /// </exception>
  TGIS_LayerAddFieldEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure (
    const _uidname  : String          ;
    const _name     : String          ;
    const _type     : TGIS_FieldType  ;
    const _width    : Integer         ;
    const _decimal  : Integer         ;
//    {$IFNDEF OXYGENE}
//      const _saved  : Boolean  = True ;
//      const _binary : Integer  = 0
//?    {$ELSE}
    const _saved  : Boolean = True ;
    const _binary : Integer = 0
//    {$ENDIF}
  ) of object ;

  /// <summary>
  ///   On add new field event.
  /// </summary>
  /// <param name="_nArgs">
  ///   number of arguments
  /// </param>
  /// <param name="_args">
  ///   array of arguments
  /// </param>
  TGIS_DbFunction   = {$IFDEF OXYGENE} public {$ENDIF}
                      function( _nArgs : Integer ;
                                _args  : array of Variant
                              ) : Variant of object ;



  {$IFDEF OXYGENE}
    T_optionsDB nested in TGIS_DbAbstract = public record
      public
      {@ Bitwise value:
         0x01 - transaction upon update;
         0x02 - requery required after transaction commit;
         0x04 - transactions upon restructure;
         0x08 - use server cursors for ado connections;
         0x16 - global transaction upon all updates; }
        Value            : Integer ;
      {@ Update transactions required. }
        UpdateTransact   : Boolean ;
      {@ Requery after transaction commit required. }
        Requery          : Boolean ;
      {@ Transaction required on restructure. }
        RestructTransact : Boolean ;
      {@ Use server cursors for ado connections. }
        ServerCursor     : Boolean ;
      {@ Global transaction upon all updates. }
        GlobalUpdateTransact : Boolean ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Predefined data types.
  /// </summary>
  TGIS_DataType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Unknown data type.
    /// </summary>
    Unknown,
    /// <summary>
    ///   Text data type.
    /// </summary>
    &String,
    /// <summary>
    ///   Boolean data type.
    /// </summary>
    &Boolean,
    /// <summary>
    ///   Smallint data type.
    /// </summary>
    SmallInt,
    /// <summary>
    ///   Integer data type.
    /// </summary>
    &Integer,
    /// <summary>
    ///   LargeInt data type.
    /// </summary>
    LargeInt,
    /// <summary>
    ///   Float data type.
    /// </summary>
    Float,
    /// <summary>
    ///   DateTime data type.
    /// </summary>
    DateTime,
    /// <summary>
    ///   Memo data type.
    /// </summary>
    Memo,
    /// <summary>
    ///   Blob data type.
    /// </summary>
    Blob,
    /// <summary>
    ///   Object data type.
    /// </summary>
    &Object
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisDataTypeUnknown   = TGIS_DataType.Unknown   ;
      gisDataTypeString    = TGIS_DataType.String    ;
      gisDataTypeBoolean   = TGIS_DataType.Boolean   ;
      gisDataTypeSmallInt  = TGIS_DataType.SmallInt  ;
      gisDataTypeInteger   = TGIS_DataType.Integer   ;
      gisDataTypeLargeInt  = TGIS_DataType.LargeInt  ;
      gisDataTypeFloat     = TGIS_DataType.Float     ;
      gisDataTypeDateTime  = TGIS_DataType.DateTime  ;
      gisDataTypeMemo      = TGIS_DataType.Memo      ;
      gisDataTypeBlob      = TGIS_DataType.Blob      ;
      gisDataTypeObject    = TGIS_DataType.Object    ;
  {$ENDIF}

type
  /// <summary>
  ///   Geometry internal data type.
  /// </summary>
  TGIS_SubDataType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Unknown geometry type.
    /// </summary>
    Unknown,
    /// <summary>
    ///   Native geometry type.
    /// </summary>
    Native,
    /// <summary>
    ///   SdoGeometry type used in Oracle.
    /// </summary>
    SdoGeometry,
    /// <summary>
    ///   SqlGeometry type used in MSSQL.
    /// </summary>
    SqlGeometry,
    /// <summary>
    ///   SqlGeography type used in MSSQL.
    /// </summary>
    SqlGeography
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisSubDataTypeUnknown      = TGIS_SubDataType.Unknown      ;
      gisSubDataTypeNative       = TGIS_SubDataType.Native       ;
      gisSubDataTypeSdoGeometry  = TGIS_SubDataType.SdoGeometry  ;
      gisSubDataTypeSqlGeometry  = TGIS_SubDataType.SqlGeometry  ;
      gisSubDataTypeSqlGeography = TGIS_SubDataType.SqlGeography ;
  {$ENDIF}

type

  /// <summary>
  ///   Class that can read database.
  /// </summary>
  TGIS_DbAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class( TGIS_ObjectDisposable )
    protected

      /// <summary>
      ///   SQL dialect name.
      /// </summary>
      FDialect          : String  ;

      /// <summary>
      ///   Is PostgreSQL dialect.
      /// </summary>
      FisPostgreSql     : Boolean ;

      /// <summary>
      ///   Is Oracle dialect.
      /// </summary>
      FisOracle         : Boolean ;

      /// <summary>
      ///   Is MySql dialect.
      /// </summary>
      FisMySql          : Boolean ;

      /// <summary>
      ///   Is MsSql dialect.
      /// </summary>
      FisMsSql          : Boolean ;

      /// <summary>
      ///   Is MsJet dialect.
      /// </summary>
      FisMsJet          : Boolean ;

      /// <summary>
      ///   Is DB2 dialect.
      /// </summary>
      FisDb2            : Boolean ;

      /// <summary>
      ///   Is Interbase dialect.
      /// </summary>
      FisInterbase      : Boolean ;

      /// <summary>
      ///   Is Sqlite dialect.
      /// </summary>
      FisSqlite         : Boolean ;

      /// <summary>
      ///   Is Informix dialect.
      /// </summary>
      FisInformix       : Boolean ;

      /// <summary>
      ///   Is Sybase dialect.
      /// </summary>
      FisSybase         : Boolean ;

      /// <summary>
      ///   if True, JDBC is used.
      /// </summary>
      FIsJDBC             : Boolean ;

      /// <summary>
      ///   CodePage stored for optimization purposes.
      /// </summary>
      FiCodePage        : Integer ;

      /// <summary>
      ///   JoinCodePage stored for optimization purposes.
      /// </summary>
      FiJoinCodePage    : Integer ;

      /// <summary>
      ///   Engine options.
      /// </summary>
      {$IFDEF OXYGENE}
        options : T_optionsDB ;
      {$ELSE}
        options : record
         {@ Bitwise value:
            0x01 - transaction upon update;
            0x02 - requery required after transaction commit;
            0x04 - transactions upon restructure;
            0x08 - use server cursors for ado connections;
            0x16 - global transaction upon all updates;
          }
           Value                : Integer ;
         {@ Update transactions required. }
           UpdateTransact       : Boolean ;
         {@ Requery after transaction commit required. }
           Requery              : Boolean ;
         {@ Transaction required on restructure. }
           RestructTransact     : Boolean ;
         {@ Use server cursors for ado connections. }
           ServerCursor         : Boolean ;
         {@ Global transaction upon all updates. }
           GlobalUpdateTransact : Boolean ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   MultiUser mode.
      /// </summary>
      FMultiUserMode      : TGIS_MultiUser ;

      /// <summary>
      ///   Is provider initialized.
      /// </summary>
      FIsInitializedProvider : Boolean ;

      /// <summary>
      ///   Sql parameter prefix.
      /// </summary>
      FParameterPrefix    : String ;

      /// <summary>
      ///   Sql name max length.
      /// </summary>
      FNameMaxLength      : Integer ;

      /// <summary>
      ///   Use text parameters.
      /// </summary>
      FUseTextParameters  : Boolean ;

      /// <summary>
      ///   True, if batch mode is enabled.
      /// </summary>
      FInBatchMode        : Boolean ;

      /// <summary>
      ///   Number of rows fetched in batches from the cursor database (default 0).
      /// </summary>
      FRowsetSize         : Integer ;

      /// <summary>
      ///   if True, prefix with name is returned, otherwise only prefix.
      /// </summary>
      FFullSafeParam      : Boolean ;

      /// <summary>
      ///   Internal geometry type.
      /// </summary>
      FGeometryType       : TGIS_SubDataType ;

      /// <summary>
      ///   If True, reuse opened query, otherwise open a new query.
      /// </summary>
      FReuseQuery         : Boolean ;
    protected

      /// <summary>
      ///   SQL execute event handler.
      /// </summary>
      FOnSQLExecute   : TGetStrProc ;

      procedure fset_CurrentSQLDialect ( const _dialect       : String );
      procedure fset_EngineOptions     ( const _option        : Integer );
      procedure fset_GeometryType      ( const _type          : TGIS_SubDataType ); virtual;

    public
    // for internal use

      /// <summary>
      ///   Initialize database flags and options.
      /// </summary>
      /// <param name="_sqlParameters">
      ///   SQL parameters
      /// </param>
      /// <param name="_sqlDialectList">
      ///   SQL dialect list
      /// </param>
      procedure sqlInitialize         ( const _sqlParameters : {$IFDEF OXYGENE}
                                                                 TGIS_Strings ;
                                                               {$ELSE}
                                                                 TStrings     ;
                                                               {$ENDIF}
                                       const _sqlDialectList : {$IFDEF OXYGENE}
                                                                 TGIS_Strings
                                                               {$ELSE}
                                                                 TStrings
                                                               {$ENDIF}
                                      ) ; virtual;
      /// <summary>
      ///   Open database connection.
      /// </summary>
      /// <param name="_folder">
      ///   base folder for opening file based databases
      /// </param>
      /// <param name="_sqlParameters">
      ///   connect string on first position
      /// </param>
      procedure sqlConnect            ( const _folder        : String ;
                                        const _sqlParameters : {$IFDEF OXYGENE}
                                                                 TGIS_Strings
                                                               {$ELSE}
                                                                 TStrings
                                                               {$ENDIF}
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Close database connection.
      /// </summary>
      procedure sqlDisconnect         ; virtual; abstract;

      /// <summary>
      ///   Start transaction.
      /// </summary>
      procedure sqlTransactUpdateStart; virtual; abstract;

      /// <summary>
      ///   Commit transaction.
      /// </summary>
      procedure sqlTransactUpdateCommit
                                      ; virtual; abstract;
      /// <summary>
      ///   Start global transaction.
      /// </summary>
      procedure sqlTransactGlobalUpdateStart
                                      ; virtual; abstract;
      /// <summary>
      ///   Commit global transaction.
      /// </summary>
      procedure sqlTransactGlobalUpdateCommit
                                      ; virtual; abstract;
      /// <summary>
      ///   Start transaction.
      /// </summary>
      procedure sqlTransactRestructStart
                                      ; virtual; abstract;
      /// <summary>
      ///   Commit transaction.
      /// </summary>
      procedure sqlTransactRestructCommit
                                      ; virtual; abstract;
      /// <summary>
      ///   Rollback global transaction.
      /// </summary>
      procedure sqlTransactRollback   ; virtual; abstract;

      /// <summary>
      ///   Close the query.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure sqlQueryClose         ( const _cursor  : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Close the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      procedure sqlTableClose         ( const _id            : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Update the table (after any change).
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      procedure sqlTablePost          ( const _id            : Integer
                                      ) ; virtual; abstract;

      /// <summary>
      ///   Execute opened table command.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      procedure sqlTableExec          ( const _id            : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Open the table based on provided filter.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_query">
      ///   query sql statement
      /// </param>
      procedure sqlTableOpenRead      ( const _id            : Integer ;
                                        const _query         : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Open the table based on provided filter
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_query">
      ///   query sql statement
      /// </param>
      procedure sqlTableOpenWrite     ( const _id            : Integer ;
                                        const _query         : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Append a new record into the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_query">
      ///   query sql statement
      /// </param>
      procedure sqlTableAppend        ( const _id            : Integer ;
                                        const _query         : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Is table prepared for insert.
      /// </summary>
      /// <param name="_id">
      ///   table id
      /// </param>
      /// <returns>
      ///   True if table is prepared
      /// </returns>
      function  sqlTablePrepared      ( const _id            : Integer
                                      ) : Boolean ; virtual; abstract;
      /// <summary>
      ///   Open the query. Active query will be closed.
      /// </summary>
      /// <param name="_query">
      ///   query to be executed
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure sqlQueryOpen          ( const _query         : String ;
                                        const _cursor        : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Reset current sql query.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure sqlQueryReset         ( const _cursor        : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Execute a command.
      /// </summary>
      /// <param name="_command">
      ///   query to be executed
      /// </param>
      procedure sqlExec               ( const _command       : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Move to first record of query.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure sqlQueryMoveFirst     ( const _cursor        : Integer
                                      ); virtual; abstract;
      /// <summary>
      ///   Move to next record of query.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure sqlQueryMoveNext      ( const _cursor        : Integer
                                      ); virtual; abstract;
      /// <summary>
      ///   Test for Eof on query. Close the query if Eof.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   True if eof is reached
      /// </returns>
      function  sqlQueryEof           ( const _cursor        : Integer
                                      ): Boolean ; virtual; abstract;
      /// <summary>
      ///   Test for Eof on the table. Close the table if Eof.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <returns>
      ///   True if eof is reached
      /// </returns>
      function  sqlTableEof           ( const _id            : Integer
                                      ) : Boolean ; virtual; abstract;
      /// <summary>
      ///   Get field index from query.
      /// </summary>
      /// <param name="_name">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field index
      /// </returns>
      function  sqlQueryGetFieldIndex ( const _name          : String ;
                                        const _cursor        : Integer
                                      ) : Integer ; virtual; abstract;
      /// <summary>
      ///   Get a field from the query by field name.
      /// </summary>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetField      ( const _name          : String   ;
                                        const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;
      /// <summary>
      ///   Get a field from the query by field id.
      /// </summary>
      /// <param name="_id">
      ///   identificator of the field
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetFieldById  ( const _id            : Integer ;
                                        const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;
      /// <summary>
      ///   Get a field from the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlTableGetField      ( const _id            : Integer ;
                                        const _name          : String
                                      ) : Variant ; virtual; abstract;

      /// <summary>
      ///   Get a parameter from the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlTableGetParam      ( const _id            : Integer ;
                                        const _name          : String
                                      ) : Variant ; virtual; abstract;

      /// <summary>
      ///   Set a field to the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_val">
      ///   value of the field
      /// </param>
      /// <param name="_defSize">
      ///   default size
      /// </param>
      procedure sqlTableSetField      ( const _id            : Integer ;
                                        const _name          : String ;
                                        const _val           : Variant ;
                                        const _defSize       : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Get the field value for a shape given by a unique identity. Values
      ///   are retrieved from the database.
      /// </summary>
      /// <param name="_field">
      ///   field id (used as a bind number if _field = '' )
      /// </param>
      /// <param name="_rebindFields">
      ///   if True, binded fields will be refreshed.
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlGetBindedField     ( const _field         : Integer ;
                                        const _rebindFields  : Boolean ;
                                        const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;
      /// <summary>
      ///   Fill the current layer structure based on the query.
      /// </summary>
      /// <param name="_tableName">
      ///   table name
      /// </param>
      /// <param name="_uidName">
      ///   uid field name
      /// </param>
      /// <param name="_addFieldEvent">
      ///   event that will add a field to the layer
      /// </param>
      procedure sqlQueryStructure     ( const _tableName     : String ;
                                        const _uidName       : String ;
                                        const _addFieldEvent  :
                                                       TGIS_LayerAddFieldEvent
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Get a GEO.UID field from the query.
      /// </summary>
      /// <param name="_field">
      ///   geometry field name
      /// </param>
      /// <param name="_fieldEx">
      ///   additional geometry field name for PostgreSQL
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryNameGEOUID    ( const _field         : String ;
                                        const _fieldEx       : String ;
                                        const _cursor        : Integer
                                      ): Integer ; virtual; abstract;

      /// <summary>
      ///   Get a GEO.UID from the query.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetGEOUID     ( const _field         : String  ;
                                        const _cursor        : Integer
                                      ) : Variant ; overload; virtual; abstract;

      /// <summary>
      ///   Get a GEO.UID from the query.
      /// </summary>
      /// <param name="_index">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetGEOUID     ( const _index         : Integer  ;
                                        const _cursor        : Integer
                                      ) : Variant ; overload; virtual; abstract;

      /// <summary>
      ///   Get a SHAPETYE from the query.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetSHAPETYPE  ( const _field         : String   ;
                                        const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;
      /// <summary>
      ///   Get a XMIN from the query.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetXMIN       ( const _field         : String ;
                                        const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;
      /// <summary>
      ///   Get a YMIN from the query.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetYMIN       ( const _field         : String ;
                                        const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;
      /// <summary>
      ///   Get a geometry from the query.
      /// </summary>
      /// <param name="_field">
      ///   geometry field name
      /// </param>
      /// <param name="_fieldEx">
      ///   additional geometry field name for PostgreSQL
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_forceValue">
      ///   if True then use field value property
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetGeomVAR    ( const _field         : String ;
                                        const _fieldEx       : String ;
                                        const _cursor        : Integer ;
                                        {$IFNDEF OXYGENE}
                                          const _forceValue    : Boolean  = False
                                        {$ELSE}
                                          const _forceValue    : Boolean := False
                                        {$ENDIF}
                                      ) :  OleVariant; virtual; abstract;
      /// <summary>
      ///   Get blob data type.
      /// </summary>
      /// <param name="_name">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetBlob       ( const _name          : String ;
                                        const _cursor        : Integer
                                       ) : TStream ; virtual; abstract;
      {$IFDEF MANAGED}
        /// <summary>
        ///   Get a geometry from the query.
        /// </summary>
        /// <param name="_field">
        ///   geometry field name
        /// </param>
        /// <param name="_fieldEx">
        ///   additional geometry field name for PostgreSQL
        /// </param>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        /// <param name="_size">
        ///   geometry data size
        /// </param>
        /// <returns>
        ///   field value
        /// </returns>
        function  sqlQueryGetGeomPtr  ( const _field         : String ;
                                        const _fieldEx       : String ;
                                        const _cursor        : Integer;
                                          var _size          : Integer
                                      ) : TGIS_Bytes ; overload; virtual; abstract;

        /// <summary>
        ///   Get a geometry from the query.
        /// </summary>
        /// <param name="_field">
        ///   geometry field name
        /// </param>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        /// <param name="_size">
        ///   geometry data size
        /// </param>
        /// <returns>
        ///   field value
        /// </returns>
        function  sqlQueryGetGeomPtr  ( const _field         : String ;
                                        const _cursor        : Integer;
                                          var _size          : Integer
                                      ) : TBytes ; overload; virtual; abstract;
      {$ELSE}
        /// <summary>
        ///   Get a geometry from the query.
        /// </summary>
        /// <param name="_field">
        ///   geometry field name
        /// </param>
        /// <param name="_fieldEx">
        ///   additional geometry field name for PostgreSQL
        /// </param>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        /// <param name="_size">
        ///   geometry data size
        /// </param>
        /// <returns>
        ///   field value
        /// </returns>
        function  sqlQueryGetGeomPtr    ( const _field         : String ;
                                          const _fieldEx       : String ;
                                          const _cursor        : Integer ;
                                            var _size          : Integer
                                        ) : Pointer ; virtual; abstract;
      {$ENDIF}
      /// <summary>
      ///   Get a geometry object from the query.
      /// </summary>
      /// <param name="_field">
      ///   geometry field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field value
      /// </returns>
      function  sqlQueryGetGeomObj    ( const _field         : String ;
                                        const _cursor        : Integer
                                      ) : TObject ; virtual; abstract;
      /// <summary>
      ///   Set a geometry to the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_data">
      ///   shape data
      /// </param>
      /// <param name="_blob">
      ///   data stream for pixelstore
      /// </param>
      procedure sqlTableSetGeometry   ( const _id            : Integer ;
                                        const _name          : String ;
                                        const _data          : OleVariant ;
                                        const _blob          : TGIS_MemoryStream
                                      ) ; overload; virtual; abstract;

      /// <summary>
      ///   Set a geometry to the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_data">
      ///   shape data object
      /// </param>
      procedure sqlTableSetGeometry   ( const _id            : Integer ;
                                        const _name          : String ;
                                        const _data          : TObject
                                      ) ; overload; virtual; abstract;

      /// <summary>
      ///   Set a blob data to the table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_data">
      ///   blob data
      /// </param>
      /// <param name="_blob">
      ///   data stream for pixelstore
      /// </param>
      procedure sqlTableSetBlob       (  const _id    : Integer ;
                                         const _name  : String ;
                                         const _data  : OleVariant ;
                                         const _blob  : TGIS_MemoryStream
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Unprepare geometry access.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure sqlQueryUnPrepareGetGeom( const _cursor        : Integer
                                        ) ; virtual; abstract;

      /// <summary>
      ///   Obtain a bind number for a given field name.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   field index
      /// </returns>
      function  sqlBindField          ( const _field        : String ;
                                        const _cursor       : Integer
                                      ) : Integer ; virtual; abstract;
      /// <summary>
      ///   Build new SQL layer.
      /// </summary>
      /// <param name="_path">
      ///   path to .ttkls file; if empty the build base on existing
      ///   parameters SQLParameter; if points to non-existent file then
      ///   will be treated as a list of CRLF or '\n' deliminated parameters
      /// </param>
      /// <param name="_extent">
      ///   starting extent of a layer - cannot be zero-sized
      /// </param>
      /// <param name="_type">
      ///   shape type supported by the layer
      /// </param>
      /// <param name="_storage">
      ///   storage type
      /// </param>
      /// <param name="_layerName">
      ///   layer name
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    If _path was provided but not exists the Access (MSJET) layer
      ///    will be created in a database named "Layers.mdb". Current layer
      ///    name will be used. If current name is empty then name will be
      ///    constructed based on ttkls file name. Internal table name could
      ///    be canonized to be based only on ASCII characters - in worst case
      ///    replaced by NAME_xxxx unique identifier.
      ///    </note>
      /// </remarks>
      procedure sqlBuild              ( const _path         : String ;
                                        const _extent       : TGIS_Extent;
                                        const _type         : TGIS_ShapeType ;
                                        const _storage      : String ;
                                        const _layerName    : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Macro for starting updates.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
       procedure sqlUpdateStart         ( const _id           : Integer ;
                                          const _table        : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Get parameters list.
      /// </summary>
      /// <param name="_idx">
      ///   param index
      /// </param>
      /// <returns>
      ///   list of parameters
      /// </returns>
      {$IFDEF OXYGENE}
        function  sqlGetParams        ( const _idx          : Integer
                                      ) : TGIS_StringList ; virtual; abstract;
      {$ELSE}
        function  sqlGetParams        ( const _idx          : Integer
                                      ) : TStringList ; virtual; abstract;
      {$ENDIF}
      /// <summary>
      ///   Create a table parameter.
      /// </summary>
      /// <param name="_id">
      ///   table id
      /// </param>
      /// <param name="_name">
      ///   parameter name
      /// </param>
      /// <param name="_type">
      ///   parameter data type
      /// </param>
      /// <param name="_subtype">
      ///   parameter subdata type
      /// </param>
      /// <param name="_size">
      ///   parameter data size
      /// </param>
      procedure sqlTableCreateParam   ( const _id           : Integer ;
                                        const _name         : String ;
                                        const _type         : TGIS_DataType ;
                                        const _subtype      : TGIS_SubDataType ;
                                        const _size         : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Is geometry data a text.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor index
      /// </param>
      /// <returns>
      ///   True if geometry is in text format
      /// </returns>
      function  sqlQueryGeometryIsText( const _cursor       : Integer
                                      ): Boolean ; virtual; abstract;
      /// <summary>
      ///   Allocate new cursor. Cursor should be closes by calling
      ///   CursorClose().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be opened
      /// </param>
      procedure cursorOpen            ( const _cursor : Integer
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Free cursor allocated by CursorOpen().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure cursorClose           ( const _cursor : Integer
                                      ) ; virtual; abstract;

      /// <summary>
      ///   Get last opened cursor index.
      /// </summary>
      /// <returns>
      ///   cursor index
      /// </returns>
      function  getLastCursor       : Integer ; virtual; abstract;

      /// <summary>
      ///   Create a function on database site.
      /// </summary>
      /// <param name="_name">
      ///   function name
      /// </param>
      /// <param name="_nArgs">
      ///   number of arguments
      /// </param>
      /// <param name="_fnc">
      ///   function callback
      /// </param>
      procedure sqlCreateFunction     ( const _name  : String ;
                                        const _nArgs : Integer ;
                                        const _fnc   : TGIS_DbFunction
                                       ) ; virtual;

      /// <summary>
      ///   Get last inserted record id.
      /// </summary>
      /// <returns>
      ///   id value
      /// </returns>
      function sqlLastInsertId        : TGIS_Uid ; virtual;

      /// <summary>
      ///   Get a safe parameter.
      /// </summary>
      /// <param name="_param">
      ///   parameter name
      /// </param>
      /// <returns>
      ///   formatted parameter
      /// </returns>
      function  safeParam             ( const _param        : String
                                      ) : String ; virtual;


      /// <summary>
      ///   Return base folder for a layer. If layer path is embedded then
      ///   provide project folder.
      /// </summary>
      /// <param name="_layer">
      ///   _layer to be resolved
      /// </param>
      /// <returns>
      ///   layer base folder or empty string if cannot be resolved
      /// </returns>
      function  sqlBaseFolder         ( const _layer : TGIS_Layer ) : String ;

      /// <summary>
      ///   Resolves potentially embedded database path in a connection string
      ///   into an absolute path.
      /// </summary>
      /// <param name="_folder">
      ///   base folder for opening file based databases
      /// </param>
      /// <param name="_connection">
      ///   connection string
      /// </param>
      /// <param name="_jdbc">
      ///   if set (default is False) path will be represented according to JDBC
      /// </param>
      /// <returns>
      ///   if connection contains file then expanded absolute path;
      ///   _connection string otherwise
      /// </returns>
      /// <remarks>
      ///   This overload assumes that _connection contains just a file.
      /// </remarks>
      function  sqlPathAbsolute       ( const _folder       : String ;
                                        const _connection   : String ;
                                        const _jdbc         : Boolean = False
                                      ) : String ; overload ;

      /// <summary>
      ///   Resolves potentially embedded database path in a connection string
      ///   into an absolute path.
      /// </summary>
      /// <param name="_folder">
      ///   base folder for opening file based databases
      /// </param>
      /// <param name="_connection">
      ///   connection string
      /// </param>
      /// <param name="_separator">
      ///   items separator
      /// </param>
      /// <param name="_jdbc">
      ///   if set (default is False) path will be represented according to JDBC
      /// </param>
      /// <returns>
      ///   if connection contains file then expanded absolute path;
      ///   _connection string otherwise
      /// </returns>
      /// <remarks>
      ///   This overload assumes that _connection contains string in a format:
      ///   'someting1:someting2:something3' where ':' is a _separator.
      /// </remarks>
      function  sqlPathAbsolute       ( const _folder       : String ;
                                        const _connection   : String ;
                                        const _separator    : Char   ;
                                        const _jdbc         : Boolean = False
                                      ) : String ; overload ;

      /// <summary>
      ///   Resolves potentially embedded database path in a connection string
      ///   into an absolute path.
      /// </summary>
      /// <param name="_folder">
      ///   base folder for opening file based databases
      /// </param>
      /// <param name="_connection">
      ///   connection string
      /// </param>
      /// <param name="_separator">
      ///   items separator
      /// </param>
      /// <param name="_key">
      ///   key of the item which may contain path
      /// </param>
      /// <param name="_delimiter">
      ///   key-value delimiter
      /// </param>
      /// <returns>
      ///   if connection contains file then expanded absolute path;
      ///   _connection string otherwise
      /// </returns>
      /// <remarks>
      ///   This overload assumes that _connection contains string in a format:
      ///   'key1=valu1;key2=value;KEY=value3' where ';' is a _separator,
      ///   '=' is as _delimitter, and 'KEY' is a _key.
      /// </remarks>
      function  sqlPathAbsolute       ( const _folder       : String ;
                                        const _connection   : String ;
                                        const _separator    : Char ;
                                        const _key          : String ;
                                        const _delimiter    : Char
                                      ) : String ; overload ;

    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public

       /// <summary>
       ///   Constructor.
       /// </summary>
       constructor Create  ; virtual;

       /// <summary>
       ///   Check if layer can read a file.
       /// </summary>
       /// <param name="_path">
       ///   file path to open
       /// </param>
       /// <param name="_storage">
       ///   storage name from the file
       /// </param>
       /// <param name="_sqlType">
       ///   sql type from the file
       /// </param>
       /// <returns>
       ///   True if configuration to database is supported.
       /// </returns>
       function    PreRecognize       ( const _path         : String ;
                                        const _storage      : String ;
                                        const _sqlType      : Integer
                                      ) : Boolean; virtual;

       /// <summary>
       ///   Initialize provider.
       /// </summary>
       /// <returns>
       ///   True if provider was initialized.
       /// </returns>
       function  InitializeProvider   : Boolean ; virtual;

       /// <summary>
       ///   Finalize provider.
       /// </summary>
       procedure FinalizeProvider     ; virtual;

       /// <summary>
       ///   Return last error message text.
       /// </summary>
       /// <returns>
       ///   error message text.
       /// </returns>
       function  GetLastErrorMessage  : String ; virtual;

    public // properties

        /// <summary>
        ///   Current SQL dialect name.
        /// </summary>
        property CurrentSQLDialect  : String          read  FDialect
                                                      write fset_CurrentSQLDialect ;

        /// <summary>
        ///   Engine options.
        /// </summary>
        property EngineOptions      : Integer         write fset_EngineOptions ;

        /// <summary>
        ///   Is PostgreSQL dialect.
        /// </summary>
        property IsPostgreSql       : Boolean         read  FisPostgreSql
                                                      write FisPostgreSql ;

        /// <summary>
        ///   Is Oracle dialect.
        /// </summary>
        property IsOracle           : Boolean         read  FisOracle ;

        /// <summary>
        ///   Is MySql dialect.
        /// </summary>
        property IsMySql            : Boolean         read  FisMySql ;

        /// <summary>
        ///   Is MsSql dialect.
        /// </summary>
        property IsMsSql            : Boolean         read  FisMsSql ;

        /// <summary>
        ///   Is MsJet dialect.
        /// </summary>
        property IsMsJet            : Boolean         read  FisMsJet ;

        /// <summary>
        ///   Is DB2 dialect.
        /// </summary>
        property IsDb2              : Boolean         read  FisDb2 ;

        /// <summary>
        ///   Is Interbase dialect.
        /// </summary>
        property IsInterbase        : Boolean         read  FisInterbase ;

        /// <summary>
        ///   Is Informix dialect.
        /// </summary>
        property IsInformix         : Boolean         read  FisInformix ;

        /// <summary>
        ///   Is Sybase dialect.
        /// </summary>
        property IsSybase           : Boolean         read  FisSybase ;

        /// <summary>
        ///   Is Sqlite dialect.
        /// </summary>
        property IsSqlite           : Boolean         read  FisSqlite ;

        /// <summary>
        ///   if True, JDBC is used.
        /// </summary>
        property IsJDBC             : Boolean         read  FIsJDBC
                                                      write FIsJDBC ;

        /// <summary>
        ///   CodePage used for conversion.
        /// </summary>
        property iCodePage          : Integer         read  FiCodePage
                                                      write FiCodePage ;

        /// <summary>
        ///   JoinCodePage used for conversion.
        /// </summary>
        property iJoinCodePage      : Integer         read  FiJoinCodePage
                                                      write FiJoinCodePage ;

        /// <summary>
        ///   Multiuser mode.
        /// </summary>
        property MultiUserMode      : TGIS_MultiUser  read  FMultiUserMode ;

        /// <summary>
        ///   Is provider initialized.
        /// </summary>
        property IsProviderInitialized : Boolean      read  FIsInitializedProvider ;

        /// <summary>
        ///   Sql parameter prefix.
        /// </summary>
        property ParameterPrefix    : String          read  FParameterPrefix
                                                      write FParameterPrefix ;

        /// <summary>
        ///   Sql name maximum length.
        /// </summary>
        property NameMaxLength      : Integer         read  FNameMaxLength ;

        /// <summary>
        ///   Use text parameters.
        /// </summary>
        property UseTextParameters  : Boolean         read  FUseTextParameters
                                                      write FUseTextParameters ;

        /// <summary>
        ///   True, if batch mode is enabled for fast inserts.
        /// </summary>
        property InBatchMode        : Boolean         read  FInBatchMode
                                                      write FInBatchMode ;

        /// <summary>
        ///   if True, prefix with name is returned, otherwise only prefix.
        /// </summary>
        property FullSafeParam      : Boolean         read  FFullSafeParam
                                                      write FFullSafeParam ;

        /// <summary>
        ///   Number of rows fetched in batches from the cursor database (default 0).
        /// </summary>
        property RowsetSize        : Integer          read  FRowsetSize
                                                      write FRowsetSize ;

        /// <summary>
        ///   Internal geometry type.
        /// </summary>
        property GeometryType      : TGIS_SubDataType read  FGeometryType
                                                      write fset_GeometryType  ;

      published // events
        /// <event/>
        /// <summary>
        ///   SQL execute event handler.
        /// </summary>
        property SQLExecuteEvent    : TGetStrProc     read  FOnSQLExecute
                                                      write FOnSQLExecute ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoParams ;
{$ENDIF}

//=============================================================================
// TGIS_DbAbstract
//=============================================================================

  constructor TGIS_DbAbstract.Create ;
  begin
    inherited ;

    FParameterPrefix   := ':' ;
    FUseTextParameters := False ;
    FInBatchMode       := False ;
    FFullSafeParam     := True ;
    FIsJDBC            := False ;
    FRowsetSize        := 0 ;
    FGeometryType      := TGIS_SubDataType.Unknown ;
  end ;

  procedure TGIS_DbAbstract.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_DbAbstract.fset_CurrentSQLDialect(
    const _dialect : String
  ) ;
  begin
    FDialect       := UpperCase( _dialect ) ;
    FisOracle      := _dialect = GIS_SQL_DIALECT_NAME_ORACLE     ;
    FisMySql       := _dialect = GIS_SQL_DIALECT_NAME_MYSQL      ;
    FisPostgreSql  := _dialect = GIS_SQL_DIALECT_NAME_POSTGRESQL ;
    FisMsSql       := _dialect = GIS_SQL_DIALECT_NAME_MSSQL      ;
    FisMsJet       := _dialect = GIS_SQL_DIALECT_NAME_MSJET      ;
    FisDb2         := _dialect = GIS_SQL_DIALECT_NAME_DB2        ;
    FisInterbase   := _dialect = GIS_SQL_DIALECT_NAME_INTERBASE  ;
    FisInformix    := _dialect = GIS_SQL_DIALECT_NAME_INFORMIX   ;
    FisSqlite      := _dialect = GIS_SQL_DIALECT_NAME_SQLITE     ;
    FisSybase      := _dialect = GIS_SQL_DIALECT_NAME_SYBASE     ;

    FUseTextParameters := FisDb2 or FisPostgreSql ;
  end ;

  procedure TGIS_DbAbstract.fset_EngineOptions(
    const _option : Integer
  ) ;
  begin
    options.Value                 := _option ;
    options.UpdateTransact        := ( options.Value and $00000001 ) <> 0 ;
    options.Requery               := ( options.Value and $00000002 ) <> 0 ;
    options.RestructTransact      := ( options.Value and $00000004 ) <> 0 ;
    options.ServerCursor          := ( options.Value and $00000008 ) <> 0 ;
    options.GlobalUpdateTransact  := ( options.Value and $00000010 ) <> 0 ;
  end ;

  procedure TGIS_DbAbstract.fset_GeometryType(
    const _type          : TGIS_SubDataType
  ) ;
  begin
    FGeometryType := _type ;
  end ;

  procedure TGIS_DbAbstract.sqlCreateFunction(
    const _name   : String ;
    const _nArgs  : Integer ;
    const _fnc    : TGIS_DbFunction
  );
  begin

  end ;

  procedure TGIS_DbAbstract.sqlInitialize(
    const _sqlParameters   : {$IFDEF OXYGENE}
                               TGIS_Strings ;
                             {$ELSE}
                               TStrings     ;
                             {$ENDIF}
    const _sqlDialectList  : {$IFDEF OXYGENE}
                               TGIS_Strings
                             {$ELSE}
                               TStrings
                             {$ENDIF}
  ) ;
  var
    engine    : Integer ;
    value     : String  ;
    reuse     : String  ;
    valueEx   : String  ;
    dialect   : String  ;
    multiuser : String  ;
  begin
    // read options
    value     := _sqlDialectList.Values[ GIS_SQL_PARAMS_ENGINEOPTIONS ] ;
    valueEx   := _sqlParameters.Values [ GIS_SQL_PARAMS_ENGINEOPTIONS ] ;
    dialect   := _sqlDialectList.Values[ GIS_SQL_PARAMS_ENGINE        ] ;
    multiuser := _sqlParameters.Values [ GIS_INI_MULTIUSER_MODE       ] ;
    engine    := ParamInteger( value, 0 );

    EngineOptions     := ParamInteger( valueEx, engine );
    CurrentSQLDialect := UpperCase( dialect ) ;
    FMultiUserMode    := ParamMultiUser( multiuser, TGIS_MultiUser.Default ) ;
    value             := _sqlDialectList.Values[ 'MAX_NAMELENGTH' ] ;
    FNameMaxLength    := ParamInteger( value, 0 );

    reuse := _sqlParameters.Values[ GIS_SQL_PARAMS_REUSE_QUERY ] ;
    if IsStringEmpty( reuse ) then
      FReuseQuery := GisMetadataAsBoolean( 'TGIS_DbAbstract.' + GIS_SQL_PARAMS_REUSE_QUERY, True )
    else
      FReuseQuery := ParamBoolean( reuse, True ) ;
  end ;

  function TGIS_DbAbstract.sqlLastInsertId : TGIS_Uid;
  begin
    Result := -1 ;
  end;

  function TGIS_DbAbstract.safeParam(
    const _param : String
  ) : String ;
  begin
    if FFullSafeParam then begin
      Result := Format( '%s%s', [ FParameterPrefix, _param ] ) ;
      Result := Copy( Result, StringFirst, FNameMaxLength ) ;
    end
    else
      Result := FParameterPrefix ;
  end ;

  function TGIS_DbAbstract.sqlBaseFolder(
    const _layer : TGIS_Layer
  ) : String ;
  begin
    Result := '' ;

    if not assigned( _layer ) then
      exit ;

    if IsEmbeddedSQLPath( _layer.Path ) then begin
      if assigned( _layer.Viewer ) and
         assigned( _layer.Viewer.Ref.ProjectFile )
      then
        Result := GetFileDir( _layer.Viewer.Ref.ProjectName ) ;
    end
    else begin
      if ( not IsStringEmpty( _layer.Path ) ) and
         SafeFileExists( _layer.Path )
      then
        Result := GetFilePath( GetPathAbsolute( '', _layer.Path ) ) ;
    end;
  end;

  // Remove quotes from connection string.
  // _txt    connection string to be rewritten
  // return  rewritten connection string without quotes
  function remove_quotes( const _txt : String ) : String ;
  var
    b,e, sl : Integer ;
    cut     : Boolean ;
  begin
    if IsStringEmpty( _txt ) then begin
      Result := _txt ;
      exit ;
    end ;

    if _txt[StringFirst] = '"' then begin
      b   := StringFirst + 1 ;
      cut := True ;
    end
    else begin
      b   := StringFirst ;
      cut := False ;
    end ;

    sl := StringLast(_txt) ;
    if cut and ( _txt[sl] = '"' ) then
      e := sl - 1
    else
      e := sl ;

    Result := Copy( _txt, b, e-b+1 ) ;
  end ;


  function TGIS_DbAbstract.sqlPathAbsolute(
    const _folder       : String ;
    const _connection   : String ;
    const _jdbc         : Boolean = False
  ) : String ;
  var
    spath : String ;
  begin
    Result := _connection ;

    try
      spath := GetPathAbsolute( _folder, remove_quotes( _connection ) ) ;
      if FileExists( spath ) then begin
        if _jdbc then begin
          spath := '//' + spath ;
          spath := StringReplace(
                     spath, '\', '/',
                     DefReplaceFlags( [TReplaceFlag.rfReplaceAll] )
                   ) ;
        end;
        Result := spath ;
      end ;
    except
    end;
  end;

  function TGIS_DbAbstract.sqlPathAbsolute(
    const _folder       : String ;
    const _connection   : String ;
    const _separator    : Char   ;
    const _jdbc         : Boolean = False
  ) : String ;
  var
    i     : Integer ;
    tkn   : TGIS_Tokenizer ;
    bld   : TStringBuilder ;
  begin
    Result := _connection ;

    if _jdbc then begin
      if Pos( '//', _connection ) > 0 then
        exit ;
    end;

    tkn := TGIS_Tokenizer.Create ;
    bld := TStringBuilder.Create ;
    try

     tkn.ExecuteEx( remove_quotes( _connection ), _separator );
      for i:=0 to tkn.Result.Count-1 do begin
        if i > 0 then
          bld.Append( _separator ) ;

        bld.Append( sqlPathAbsolute( _folder, tkn.Result[i], _jdbc ) ) ;
      end;

    finally
      Result := bld.ToString ;
      FreeObject( tkn );
    end;
  end;

  function TGIS_DbAbstract.sqlPathAbsolute(
    const _folder       : String ;
    const _connection   : String ;
    const _separator    : Char ;
    const _key          : String ;
    const _delimiter    : Char
  ) : String ;
  var
    i    : Integer ;
    tkn  : TGIS_Tokenizer ;
    tkn2 : TGIS_Tokenizer ;
    bld  : TStringBuilder ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    bld := TStringBuilder.Create ;
    try
     tkn.ExecuteEx( remove_quotes( _connection ), _separator );
      for i:=0 to tkn.Result.Count-1 do begin
        if i > 0 then
          bld.Append( _separator ) ;

        tkn2 := TGIS_Tokenizer.Create ;
        try
          tkn2.ExecuteEx( tkn.Result[i], _delimiter ) ;
          if tkn2.Result.Count = 2 then begin
            if CompareText( tkn2.Result[0], _key ) = 0 then begin
              bld.Append( tkn2.Result[0] ) ;
              bld.Append( _delimiter ) ;
              bld.Append( sqlPathAbsolute( _folder, tkn2.Result[1] ) ) ;
              continue ;
            end;
          end;
          bld.Append( tkn.Result[i] ) ;
        finally
          FreeObject( tkn2 ) ;
        end;
      end;
    finally
      Result := bld.ToString ;
      FreeObject( bld );
      FreeObject( tkn );
    end;
  end ;


  function TGIS_DbAbstract.PreRecognize(
    const _path    : String ;
    const _storage : String ;
    const _sqlType : Integer
 ) : Boolean;
  var
    opengis : String   ;
    storage : String  ;
    sqlType : Integer ;

    function _s( const _storage : String ) : Boolean ;
    begin
      Result := CompareText( storage, _storage ) = 0 ;
    end ;

  begin
    sqlType := GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE, storage ) ;

    if IsStringEmpty( storage ) then begin
      GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_OPENGIS, opengis ) ;
      if      opengis = '1' then
              storage := GIS_INI_LAYERSQL_OPENGISBLOB
      else if opengis = '2' then
              storage := GIS_INI_LAYERSQL_OPENGISNORMALIZED
      else if opengis = '3' then
              storage := GIS_INI_LAYERSQL_GEOMEDIA
      else    storage := GIS_INI_LAYERSQL_NATIVE            ;
    end ;

    Result := ( sqlType = _sqlType ) and _s( _storage ) ;
  end ;

  function TGIS_DbAbstract.InitializeProvider : Boolean ;
  begin
    Result := True ;
    FIsInitializedProvider := True ;
  end ;

  procedure TGIS_DbAbstract.FinalizeProvider ;
  begin
    FIsInitializedProvider := False ;
  end ;

  function TGIS_DbAbstract.GetLastErrorMessage : String ;
  begin
    Result := '' ;
  end ;

{==================================== END =====================================}
end.

