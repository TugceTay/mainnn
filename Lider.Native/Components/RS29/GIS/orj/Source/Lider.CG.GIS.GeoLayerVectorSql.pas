//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Encapsulation of a SQL Vector layer.
}

{$IFDEF DCC}
  unit GisLayerVectorSql ;
  {$HPPEMIT '#pragma link "GisLayerVectorSql"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Variants,

    GisTypes,
    GisInterfaces,
    GisLayerVector,
    GisDb,
    GisConfig ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  /// <summary>
  ///   Class encapsulation.
  /// </summary>
  TGIS_LayerVectorSqlAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                class( TGIS_LayerVector )
    protected // properties events

      /// <summary>
      ///   Event to be fired before dialect change.
      /// </summary>
      FOnBeforeDialectChange  : {$IFDEF CLR}
                                  EventHandler ;
                                {$ELSE}
                                  TNotifyEvent ;
                                {$ENDIF}

      /// <summary>
      ///   Event to be fired after dialect change.
      /// </summary>
      FOnAfterDialectChange   : {$IFDEF CLR}
                                  EventHandler ;
                                {$ELSE}
                                  TNotifyEvent ;
                                {$ENDIF}

      /// <summary>
      ///   Event to be fired before uid reserve insert.
      /// </summary>
      FOnBeforeUidReserve   : TGIS_ShapeEvent ;

      procedure fset_SQLExecuteEvent    ( const _event   : TGetStrProc
                                        ) ;
      function  fget_SQLExecuteEvent    : TGetStrProc ;
      function  fget_SQLParameter       ( const _name    : String
                                        ) : String ;
      procedure fset_SQLParameter       ( const _name    : String;
                                          const _value    : String
                                        ) ;

      /// <summary>
      ///   Process tokens in a SQLParameters property.
      /// </summary>
      /// <param name="_token">
      ///   password token
      /// </param>
      /// <returns>
      ///   token
      /// </returns>
      function  passwordCallBack        ( const _token   : String
                                        ) : String ;

      /// <summary>
      ///   Return a SQL command associated with a given identifier.
      /// </summary>
      /// <param name="_id">
      ///   command identifier
      /// </param>
      /// <returns>
      ///   SQL command
      /// </returns>
      function  getCmd                  ( const _id      : Integer
                                        ) : String ;

      /// <summary>
      ///   Initialize command list process.
      /// </summary>
      /// <param name="_cnt">
      ///   number of commands
      /// </param>
      procedure initializeCommandList   ( const _cnt     : Integer 
                                        ) ;

      /// <summary>
      ///   Finalize command list process.
      /// </summary>
      procedure finalizeCommandList     ;

      function  fget_ViewFeatures       : String ; virtual;

      /// <summary>
      ///   Initialize connect process.
      /// </summary>
      /// <param name="_prepare">
      ///   if True, command list will be prepared
      /// </param>
      procedure initializeConnect       ( const _prepare : Boolean
                                        ) ;

      function  fget_Table              : String ; virtual;
      function  fget_MasterTable        : String ; virtual;

      /// <summary>
      ///   Prepare command list.
      /// </summary>
      procedure prepareCommandList      ; virtual; abstract;

      /// <summary>
      ///   Prepare additional parameters list.
      /// </summary>
      procedure prepareParametersExList ; virtual;

      /// <summary>
      ///   Add common SQL commands to SQL commands list.
      /// </summary>
      procedure addCommonCommands       ;

      /// <summary>
      ///   Convert shape type to geometry type.
      /// </summary>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_dim">
      ///   layer dimension
      /// </param>
      /// <returns>
      ///   geometry type identifier
      /// </returns>
      function  getGeometryShapeType    ( const _type    : TGIS_ShapeType ;
                                          const _dim     : TGIS_DimensionType
                                        ) : Integer ;

      /// <summary>
      ///   Make safe column name.
      /// </summary>
      /// <param name="_txt">
      ///   column name
      /// </param>
      /// <returns>
      ///   safe name
      /// </returns>
      function  safeName                ( const _txt     : String
                                        ) : String ; virtual;

      /// <summary>
      ///   Make safe parameter.
      /// </summary>
      /// <param name="_param">
      ///   parameter
      /// </param>
      /// <returns>
      ///   safe parameter
      /// </returns>      
      function  safeParam               ( const _param   : String
                                        ) : String ; virtual;
    // properties internal values
    protected

      /// <summary>
      ///   Feature view name if other then standard table.
      /// </summary>
      FViewFeatures   : String ;

      /// <summary>
      ///   Table name.
      /// </summary>
      FCurrTable      : String ;

      /// <summary>
      ///   Connection parameters.
      /// </summary>
      FSQLParameters  : TStringList ;

      /// <summary>
      ///   SQL dialect (list of tokens) attached to the layer.
      /// </summary>
      FSQLDialectList : TStringList ;

      /// <summary>
      ///   List of SQL commands.
      /// </summary>
      FSQLCommands    : TStringList ;

      /// <summary>
      ///   List of additional connection parameters.
      /// </summary>
      FSQLParametersEx : TStringList ;

      /// <summary>
      ///   CodePage stored for optimization purposes.
      /// </summary>
      iCodePage       : Integer ;

      /// <summary>
      ///   JoinCodePage stored for optimization purposes.
      /// </summary>
      iJoinCodePage   : Integer ;

      /// <summary>
      ///   Database supporting class handle.
      /// </summary>
      oGisDb          : TGIS_DbAbstract ;

      /// <summary>
      ///   Last command id.
      /// </summary>
      FLastCmdId      : Integer ;

      /// <summary>
      ///   If True, operations on the master table will be performed.
      /// </summary>
      FUseMasterTable : Boolean ;

      /// <summary>
      ///   Left quote definition.
      /// </summary>
      FQuoteLeft      : String ;

      /// <summary>
      ///   Right quote definition.
      /// </summary>
      FQuoteRight     : String ;

      /// <summary>
      ///   Connection pool id.
      /// </summary>
      FConnectionPoolId : String ;

    protected

      /// <summary>
      ///   Macro for connecting to the database.
      /// </summary>
      procedure macroConnect          ; virtual;

      /// <summary>
      ///   Macro for disconnecting to the database.
      /// </summary>
      procedure macroDisconnect       ; virtual;

      /// <summary>
      ///   Macro for dropping table.
      /// </summary>
      procedure macroTableDrop        ; virtual;

      /// <summary>
      ///   Macro for redefining a database structure.
      /// </summary>
      /// <param name="_layer">
      ///   layer on which fields were created
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_WRONGFIELD
      /// </exception>
      procedure macroTableAlter       ( const _layer  : TGIS_LayerVector
                                      ) ; virtual;

      /// <summary>
      ///   Macro for starting updates.
      /// </summary>
      procedure macroUpdateStart      ; virtual;

      /// <summary>
      ///   Macro for ending updates.
      /// </summary>
      procedure macroUpdateEnd        ; virtual;

      /// <summary>
      ///   Macro for building RTree index.
      /// </summary>
      procedure macroBuildRtree       ; virtual;

      /// <summary>
      ///   Macro for managing shape update process.
      /// </summary>
      /// <param name="_shp">
      ///   shape data to be saved
      /// </param>
      /// <param name="_import">
      ///   if True, then shape will be added in import mode
      /// </param>
      procedure macroShapeUpdate      ( const _shp     : TGIS_Shape ;
                                        const _import  : Boolean
                                      ) ; virtual;

      /// <summary>
      ///   Macro for updating master table
      /// </summary>
      /// <param name="_extent">
      ///   shape extent
      /// </param>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_name">
      ///   name identifier
      /// </param>
      /// <param name="_dim">
      ///   shape dimension
      /// </param>
      procedure macroMasterUpdate     ( const _extent : TGIS_Extent    ;
                                        const _type   : TGIS_ShapeType ;
                                        const _name   : String         ;
                                        const _dim    : TGIS_DimensionType
                                      ) ; virtual;

      /// <summary>
      ///   Gets maximum column name length.
      /// </summary>
      /// <returns>
      ///   maximum column name length
      /// </returns>      
      function  macroMaxNameLength    : Integer ; virtual;

      /// <summary>
      ///   Macro for beginning batch mode.
      /// </summary>
      procedure macroBeginBatchMode   ; virtual;

      /// <summary>
      ///   Macro for ending batch mode.
      /// </summary>
      procedure macroEndBatchMode     ; virtual;

    // properties access functions
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      procedure fset_UseRTree         ( const _value : Boolean
                                      ) ; override;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Makes additional layer setup.
      /// </summary>
      procedure setUp3                ; override;

    protected
    // destructor

      /// <summary>
      ///   Destroy a layer instance.
      /// </summary>
      procedure doDestroy ; override;

    public
    // constructors

      /// <summary>
      ///   Create a layer instance.
      /// </summary>
      /// <remarks>
      ///   See TGIS_LayerVector.Create for details and example.
      /// </remarks>
      constructor Create ; override;
    public

      /// <summary>
      ///   Execute custom SQL statement.
      /// </summary>
      /// <param name="_sql">
      ///   SQL String
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_SQLQUERY
      /// </exception>
      /// <returns>
      ///   True if succeeded
      /// </returns>
      function  ExecuteSQL ( const _sql : String
                           ) : Boolean ; virtual;

      /// <summary>
      ///   Reopen a connection.
      /// </summary>
      procedure OpenConnection ;

      /// <summary>
      ///   Close current connection and active queries.
      /// </summary>
      procedure CloseConnection ;

      /// <summary>
      ///   Reset the layer dataset to ensure the current database context
      ///   upon next drawing (e.g to notify changes when a record was deleted
      ///   manually in the table).
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Call it before the viewer update.
      ///    </note>
      /// </remarks>
      procedure ResetDataset ; virtual;

      /// <summary>
      ///   Close all active datasets of a layer. 
      ///   This can help to unlock a database like Sqlite that is being locked
      ///   after calling a select statement.
      /// </summary>
      procedure CloseActiveDatasets ;

      /// <summary>
      ///   Create master table.
      /// </summary>
      procedure CreateMasterTable ;

      /// <summary>
      ///   Delete record from master table.
      /// </summary>
      procedure DeleteFromMasterTable ;

      /// <summary>
      ///   Add layer entry to master table.
      /// </summary>
      /// <param name="_layerName">
      ///   layer name
      /// </param>
      /// <param name="_geometryColumn">
      ///   geometry column name
      /// </param>
      /// <param name="_indexColumn">
      ///   index column name
      /// </param>
      /// <param name="_storage">
      ///   storage type
      /// </param>
      /// <param name="_geometryType">
      ///   geometry type
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      /// <param name="_srid">
      ///   CS id
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      procedure AddToMasterTable( const _layerName      : String ;
                                  const _geometryColumn : String ;
                                  const _indexColumn    : String ;
                                  const _storage        : String ;
                                  const _geometryType   : TGIS_ShapeType ;
                                  const _dim            : TGIS_DimensionType ;
                                  const _srid           : Integer ;
                                  const _extent         : TGIS_Extent
                                 ) ;

      /// <summary>
      ///   Read layer entry from master table.
      /// </summary>
      /// <param name="_layerName">
      ///   layer name
      /// </param>
      /// <param name="_storage">
      ///   storage type
      /// </param>
      /// <param name="_geometryColumn">
      ///   geometry column name
      /// </param>
      /// <param name="_indexColumn">
      ///   index column name
      /// </param>
      /// <param name="_geometryType">
      ///   geometry type
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      /// <param name="_srid">
      ///   CS id
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      /// <returns>
      ///   True if succeeded
      /// </returns>
      function  ReadMasterTable ( const _layerName      : String ;
                                  const _storage        : String ;
                                    var _geometryColumn : String ;
                                    var _indexColumn    : String ;
                                    var _geometryType   : TGIS_ShapeType ;
                                    var _dim            : Integer ;
                                    var _srid           : Integer ;
                                    var _extent         : TGIS_Extent
                                 ) : Boolean ;

      /// <summary>
      ///   Update a layer entry in master table.
      /// </summary>
      /// <param name="_layerName">
      ///   layer name
      /// </param>
      /// <param name="_geometryType">
      ///   geometry type
      /// </param>
      /// <param name="_dim">
      ///   dimension type
      /// </param>
      /// <param name="_srid">
      ///   CS identifier
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      procedure UpdateMasterTable( const _layerName      : String ;
                                   const _geometryType   : TGIS_ShapeType ;
                                   const _dim            : TGIS_DimensionType ;
                                   const _srid           : Integer ;
                                   const _extent         : TGIS_Extent
                                  ) ;

      /// <inheritdoc/>
      procedure InitializeDirectWrite(
                                 const _path        : String           ;
                                 const _extent      : TGIS_Extent      ;
                                 const _type        : TGIS_ShapeType   ;
                                 const _dim         : TGIS_DimensionType
                               ) ; override;

      /// <inheritdoc/>
      procedure FinalizeDirectWrite    ; override;

      /// <inheritdoc/>
      procedure WriteShapeDirect( const _shape       : TGIS_Shape
                                  ) ; override;

      /// <summary>
      ///   Create a table with layer styles.
      /// </summary>
      procedure CreateStyleTable ;

      /// <summary>
      ///   Apply a style to the layer.
      /// </summary>
      /// <param name="_styleData">
      ///   style data
      /// </param>
      procedure ApplyStyle       ( const _styleData : String
                                 ) ;

      /// <summary>
      ///   Read a style.
      /// </summary>
      /// <param name="_name">
      ///   style name
      /// </param>
      /// <returns>
      ///   style data
      /// </returns>
      function  ReadStyle       ( const _name : String
                                ) : String ;

      /// <summary>
      ///   Write or update a style to database.
      /// </summary>
      /// <param name="_name">
      ///   style name
      /// </param>
      /// <param name="_desc">
      ///   style description
      /// </param>
      /// <param name="_style">
      ///   style data
      /// </param>
      procedure WriteStyle       ( const _name   : String ;
                                   const _desc   : String ;
                                   const _style  : String
                                 ) ;

      /// <summary>
      ///   Write or update a style to database.
      /// </summary>
      /// <param name="_name">
      ///   style name
      /// </param>
      /// <param name="_layer">
      ///   destination layer
      /// </param>
      procedure WriteStyleEx     ( const _name   : String ;
                                   const _layer  : TGIS_LayerVector
                                 ) ;

      /// <summary>
      ///   Get available styles from database.
      /// </summary>
      /// <returns>
      ///   list of available styles
      /// </returns>
      function GetAvailableStyles : {$IFDEF OXYGENE}
                                      TGIS_Strings ;
                                    {$ELSE}
                                      TStrings     ;
                                    {$ENDIF}

      /// <summary>
      ///   Create a table with projects.
      /// </summary>
      procedure CreateProjectTable ;

      /// <summary>
      ///   Read project from database.
      /// </summary>
      /// <param name="_name">
      ///   project name
      /// </param>
      /// <param name="_tokens">
      ///   list of tokens "token=replacement" to be replaced in project
      /// </param>
      /// <returns>
      ///   Project handle.
      /// </returns>
      function  GetProject        ( const _name   : String ;
                                    const _tokens : {$IFDEF OXYGENE}
                                                      TGIS_StringList
                                                    {$ELSE}
                                                      TStringList
                                                    {$ENDIF}
                                  ) : TGIS_ConfigAbstract ;

      /// <summary>
      ///   Write or update a project to database.
      /// </summary>
      /// <param name="_name">
      ///   project name
      /// </param>
      /// <param name="_desc">
      ///   project description
      /// </param>
      /// <param name="_project">
      ///   project data
      /// </param>
      procedure WriteProject      ( const _name     : String ;
                                    const _desc     : String ;
                                    const _project  : String
                                  ) ;
                                  
      /// <summary>
      ///   Get available projects from database.
      /// </summary>
      /// <returns>
      ///   list of available projects
      /// </returns>
      function GetAvailableProjects : {$IFDEF OXYGENE}
                                        TGIS_Strings ;
                                      {$ELSE}
                                        TStrings     ;
                                      {$ENDIF}

      /// <summary>
      ///   Delete a layer (tables, metadata) from database.
      /// </summary>
      procedure DeleteLayer ; virtual ;
    public // properties

      /// <summary>
      ///   Connection parameters.
      /// </summary>
      /// <param name="_name">
      ///   parameter name
      /// </param>
      property SQLParameter[ const _name : String ] : String
                                                    read  fget_SQLParameter
                                                    write fset_SQLParameter ;

       /// <summary>
       ///   List of additional connection parameters.
       /// </summary>
       property SQLParametersEx : {$IFDEF OXYGENE}
                                    TGIS_StringList
                                  {$ELSE}
                                    TStringList
                                  {$ENDIF}
                                  read  FSQLParametersEx ;

      /// <summary>
      ///   Dialect list in a form "token=replacement". Will be changed
      ///   after each change of SQLDialect property.
      /// </summary>
      property SQLDialectList  : {$IFDEF OXYGENE}
                                    TGIS_StringList
                                  {$ELSE}
                                    TStringList
                                  {$ENDIF}
                                  read  FSQLDialectList ;
      /// <summary>
      ///   SQL Commands used for database operations.
      /// </summary>
      property SQLCommands     : {$IFDEF OXYGENE}
                                    TGIS_StringList
                                  {$ELSE}
                                    TStringList
                                  {$ENDIF}
                                  read  FSQLCommands ;

      /// <summary>
      ///   Name of the features view. If attached name is different then the
      ///   default, then querying will be done based on provided view. By
      ///   providing empty String - default table name will be restored.
      ///   Updates will always be done on standard table.
      /// </summary>
      property ViewFeatures   : String read  fget_ViewFeatures
                                       write FViewFeatures ;

      /// <summary>
      ///   Name of the table (general).
      /// </summary>
      property Table          : String read  fget_Table
                                       write FCurrTable ;

      /// <summary>
      ///   Name of the master table.
      /// </summary>
      property MasterTable    : String read  fget_MasterTable ;

      /// <summary>
      ///   If True, operations on the master table will be performed.
      /// </summary>
      property UseMasterTable : Boolean read FUseMasterTable ;

      /// <summary>
      ///   Connection pool id. Used to group shared connections per viewer or
      ///   create new connection when a layer is not attached to the viewer.
      ///   User can switch to another shared connection by closing current
      ///   connection, assigning existing pool id and again opening the connection
      ///   (only if a layer is not attached to the viewer). A layer attached to
      ///   a viewer always gets the viewer pool id.
      /// </summary>
      property ConnectionPoolId : String read  FConnectionPoolId
                                         write FConnectionPoolId ;
    published // events
    
      /// <event/>
      /// <summary>
      ///   Will be fired upon any SQL execution to trace SQL statements.
      /// </summary>
      property SQLExecuteEvent : TGetStrProc read  fget_SQLExecuteEvent
                                             write fset_SQLExecuteEvent ;
      {$IFDEF CLR}
          
          /// <event/>
          /// <summary>
          ///   Will be fired before every SQLDialect change. By changing
          ///   SQLDialectList inside handler for this event you will be able
          ///   to modify dialect dynamically.
          /// </summary>
          event    BeforeDialectChangeEvent : EventHandler
                                              delegate FOnBeforeDialectChange ;
       {$ELSE}
          
          /// <event/>
          /// <summary>
          ///   Will be fired before every SQLDialect change. By changing
          ///   SQLDialectList inside handler for this event you will be able
          ///   to modify dialect dynamically.
          /// </summary>
          property   BeforeDialectChangeEvent : TNotifyEvent
                                                read  FOnBeforeDialectChange
                                                write FOnBeforeDialectChange ;
      {$ENDIF}
      
      {$IFDEF CLR}
          
          /// <event/>
          /// <summary>
          ///   Event to be fired before uid reserve insert.
          /// </summary>
          event    BeforeUidReserveEvent      : TGIS_ShapeEvent
                                                delegate FOnBeforeUidReserve ;
       {$ELSE}
          
          /// <event/>
          /// <summary>
          ///   Event to be fired before uid reserve insert.
          /// </summary>
          property   BeforeUidReserveEvent   : TGIS_ShapeEvent
                                               read  FOnBeforeUidReserve
                                               write FOnBeforeUidReserve ;
      {$ENDIF}
      
      {$IFDEF CLR}
      
         /// <event/>
         /// <summary>
         ///   Will be fired after SQLDialect change. By changing
         ///   SQLCommandList inside handler for this event you will be able
         ///   to modify commands dynamically.
         /// </summary>
         event    AfterDialectChangeEvent : EventHandler
                                            delegate FOnAfterDialectChange ;
      {$ELSE}
      
         /// <event/>
         /// <summary>
         ///   Will be fired after SQLDialect change. By changing
         ///   SQLCommandList inside handler for this event you will be able
         ///   to modify commands dynamically.
         /// </summary>
         property   AfterDialectChangeEvent : TNotifyEvent
                                              read  FOnAfterDialectChange
                                              write FOnAfterDialectChange ;
      {$ENDIF}

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,

    GisRtl,
    GisLogger,
    GisFunctions,
    GisInternals,
    GisClasses,
    GisResource,
    GisParams ;
{$ENDIF}

const
  // Common declarations for SQL layers.
  ID_ASQL_BEGIN = 0 ;

  ASQL_CREATE_GEOMETRY_COLUMNS_TABLE =
  'CREATE TABLE <#TTKGIS_GEOMETRY_COLUMNS#> ('                                 +
  'TABLE_NAME       <#VARCHAR#>(64),'                                          +
  'GEOMETRY_COLUMN  <#VARCHAR#>(64),'                                          +
  'INDEX_COLUMN     <#VARCHAR#>(64),'                                          +
  'STORAGE          <#VARCHAR#>(64),'                                          +
  'GEOMETRY_TYPE    <#INTEGER#>,'                                              +
  'SRID             <#INTEGER#>,'                                              +
  'XMIN             <#DOUBLE#>,'                                               +
  'XMAX             <#DOUBLE#>,'                                               +
  'YMIN             <#DOUBLE#>,'                                               +
  'YMAX             <#DOUBLE#> ) '                                             ;
  ID_ASQL_CREATE_GEOMETRY_COLUMNS_TABLE =
    ID_ASQL_BEGIN + 1                                                          ;

  ASQL_CREATE_GEOMETRY_COLUMNS_INDEX =
  'CREATE UNIQUE INDEX <#TTKGIS_GEOMETRY_COLUMNS#><#_IDX#>'                    +
  ' ON <#TTKGIS_GEOMETRY_COLUMNS#>(<#TABLE_NAME#>)'                            ;
  ID_ASQL_CREATE_GEOMETRY_COLUMNS_INDEX =
    ID_ASQL_CREATE_GEOMETRY_COLUMNS_TABLE + 1                                  ;

  ASQL_GEOMETRY_COLUMNS =
  '<#TTKGIS_GEOMETRY_COLUMNS#>'                                                ;
  ID_ASQL_GEOMETRY_COLUMNS =
    ID_ASQL_CREATE_GEOMETRY_COLUMNS_INDEX + 1                                  ;

  ASQL_DELETE_FROM_GEOMETRY_COLUMNS =
  'DELETE FROM <#TTKGIS_GEOMETRY_COLUMNS#> WHERE TABLE_NAME=''%s'''            ;
  ID_ASQL_DELETE_FROM_GEOMETRY_COLUMNS =
    ID_ASQL_GEOMETRY_COLUMNS + 1                                               ;

  ASQL_INSERT_INTO_GEOMETRY_COLUMNS =
  'INSERT INTO <#TTKGIS_GEOMETRY_COLUMNS#>(TABLE_NAME,GEOMETRY_COLUMN,'        +
  'INDEX_COLUMN,STORAGE,GEOMETRY_TYPE,SRID,XMIN,XMAX,YMIN,YMAX) '              +
  'VALUES(''%s'',''%s'',''%s'',''%s'',%d,%d,''%s'',''%s'',''%s'',''%s'')'      ;
  ID_ASQL_INSERT_INTO_GEOMETRY_COLUMNS =
    ID_ASQL_DELETE_FROM_GEOMETRY_COLUMNS + 1                                   ;

  ASQL_UPDATE_GEOMETRY_COLUMNS =
  'UPDATE <#TTKGIS_GEOMETRY_COLUMNS#> SET '                                    +
  'GEOMETRY_TYPE=%d, SRID=%d, XMIN=%s, XMAX=%s, YMIN=%s, YMAX=%s WHERE '       +
  'TABLE_NAME=''%s'''                                                          ;
  ID_ASQL_UPDATE_GEOMETRY_COLUMNS =
    ID_ASQL_INSERT_INTO_GEOMETRY_COLUMNS + 1                                   ;

  ASQL_SELECT_GEOMETRY_COLUMNS =
  'SELECT * FROM <#TTKGIS_GEOMETRY_COLUMNS#> WHERE '                           +
  'TABLE_NAME=''%s'' AND STORAGE=''%s'''                                       ;
  ID_ASQL_SELECT_GEOMETRY_COLUMNS =
    ID_ASQL_UPDATE_GEOMETRY_COLUMNS + 1                                        ;

  ASQL_CREATE_STYLE =
  'CREATE TABLE <#ttkGISStyle#> ( '                                            +
  ' STYLE_NAME <#VARCHAR#>(64), '                                              +
  ' STYLE_DESC <#VARCHAR#>(255), '                                             +
  ' STYLE_DATA <#CLOB#> ) ' ;
  ID_ASQL_CREATE_STYLE =
    ID_ASQL_SELECT_GEOMETRY_COLUMNS + 1 ;

  ASQL_CREATE_STYLE_INDEX =
  'CREATE UNIQUE INDEX <#ttkGISStyle#><#_IDX#>'                                +
  ' ON <#ttkGISStyle#>(STYLE_NAME)'                                            ;
  ID_ASQL_CREATE_STYLE_INDEX =
    ID_ASQL_CREATE_STYLE + 1                                                   ;

  ASQL_SELECT_STYLE =
  'SELECT STYLE_DATA FROM <#ttkGISStyle#> WHERE STYLE_NAME=''%s'' '            ;
  ID_ASQL_SELECT_STYLE =
    ID_ASQL_CREATE_STYLE_INDEX + 1                                             ;

  ASQL_INSERT_STYLE =
  'INSERT INTO <#ttkGISStyle#>(STYLE_NAME,STYLE_DESC,STYLE_DATA) '             +
  'VALUES(:STYLE_NAME,:STYLE_DESC,:STYLE_DATA)'                                ;
  ID_ASQL_INSERT_STYLE =
    ID_ASQL_SELECT_STYLE + 1                                                   ;

  ASQL_UPDATE_STYLE =
  'UPDATE <#ttkGISStyle#> SET STYLE_DESC=:STYLE_DESC,STYLE_DATA=:STYLE_DATA '  +
  'WHERE STYLE_NAME=''%s'''                                                    ;
  ID_ASQL_UPDATE_STYLE =
    ID_ASQL_INSERT_STYLE + 1                                                   ;

  ASQL_SELECT_STYLES =
  'SELECT STYLE_NAME FROM <#ttkGISStyle#> '                                    ;
  ID_ASQL_SELECT_STYLES =
    ID_ASQL_UPDATE_STYLE + 1                                                   ;

  ASQL_SELECT_STYLES_EX =
    'SELECT * FROM <#ttkGISStyle#> WHERE STYLE_NAME %s'                        ;
    ID_ASQL_SELECT_STYLES_EX =
      ID_ASQL_SELECT_STYLES + 1                                                ;

  ASQL_CREATE_PROJECT =
  'CREATE TABLE <#ttkGISProject#> ( '                                          +
  ' PROJECT_NAME <#VARCHAR#>(64), '                                            +
  ' PROJECT_DESC <#VARCHAR#>(255), '                                           +
  ' PROJECT_DATA <#CLOB#> ) '                                                  ;
  ID_ASQL_CREATE_PROJECT =
    ID_ASQL_SELECT_STYLES_EX + 1 ;

  ASQL_CREATE_PROJECT_INDEX =
  'CREATE UNIQUE INDEX <#ttkGISProject#><#_IDX#>'                              +
  ' ON <#ttkGISProject#>(PROJECT_NAME)'                                        ;
  ID_ASQL_CREATE_PROJECT_INDEX =
    ID_ASQL_CREATE_PROJECT + 1                                                 ;

  ASQL_SELECT_PROJECT =
    'SELECT PROJECT_DATA FROM <#ttkGISProject#> WHERE PROJECT_NAME=''%s'''     ;
    ID_ASQL_SELECT_PROJECT =
      ID_ASQL_CREATE_PROJECT_INDEX + 1                                         ;

  ASQL_SELECT_PROJECTS =
    'SELECT PROJECT_NAME FROM <#ttkGISProject#> '                              ;
    ID_ASQL_SELECT_PROJECTS =
      ID_ASQL_SELECT_PROJECT + 1                                               ;

  ASQL_SELECT_PROJECT_EX =
    'SELECT * FROM <#ttkGISProject#> WHERE PROJECT_NAME %s'                    ;
    ID_ASQL_SELECT_PROJECT_EX =
      ID_ASQL_SELECT_PROJECTS + 1                                              ;

  ASQL_INSERT_PROJECT =
  'INSERT INTO <#ttkGISProject#>(PROJECT_NAME,PROJECT_DESC,PROJECT_DATA) '     +
  'VALUES(:PROJECT_NAME,:PROJECT_DESC,:PROJECT_DATA)'                          ;
  ID_ASQL_INSERT_PROJECT =
    ID_ASQL_SELECT_PROJECT_EX + 1                                              ;

  ASQL_UPDATE_PROJECT =
  'UPDATE <#ttkGISProject#> SET PROJECT_DESC=:PROJECT_DESC, '                  +
  'PROJECT_DATA=:PROJECT_DATA WHERE PROJECT_NAME=''%s'''                       ;
  ID_ASQL_UPDATE_PROJECT =
    ID_ASQL_INSERT_PROJECT + 1                                                 ;

//=============================================================================
// TGIS_LayerVectorSqlAbstract
//=============================================================================

  constructor TGIS_LayerVectorSqlAbstract.Create ;
  begin
    inherited ;

    FSQLParameters   := TStringList.Create ;
    FSQLDialectList  := TStringList.Create ;
    FSQLCommands     := TStringList.Create ;
    FSQLParametersEx := TStringList.Create ;

    prepareParametersExList ;
    FConnectionPoolId := GetHashCode.ToString ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.doDestroy ;
  begin
    FreeObject( FSQLParameters   ) ;
    FreeObject( FSQLDialectList  ) ;
    FreeObject( FSQLCommands     ) ;
    FreeObject( FSQLParametersEx ) ;

    inherited ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.fset_SQLExecuteEvent(
    const _event : TGetStrProc
  ) ;
  begin
    if assigned( oGisDb ) then
      oGisDb.SQLExecuteEvent := _event
  end ;

  function TGIS_LayerVectorSqlAbstract.fget_SQLExecuteEvent
    : TGetStrProc ;
  begin
    if assigned( oGisDb ) then
      Result := oGisDb.SQLExecuteEvent
    else
      Result := nil ;
  end ;

  function TGIS_LayerVectorSqlAbstract.fget_SQLParameter(
    const _name : String
  ) : String ;
  begin
    Result := FSQLParameters.Values[ _name ] ;
  end ;

  function TGIS_LayerVectorSqlAbstract.fget_Table
    : String ;
  begin
    Result := FCurrTable ;
  end ;

  function TGIS_LayerVectorSqlAbstract.fget_MasterTable
    : String ;
  begin
    Result := getCmd( ID_ASQL_GEOMETRY_COLUMNS+FLastCmdId ) ;
  end ;

  function TGIS_LayerVectorSqlAbstract.fget_ViewFeatures
    : String ;
  begin
    Result := FViewFeatures ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.fset_SQLParameter(
    const _name  : String ;
    const _value : String
  ) ;
  begin
    FSQLParameters.Values[ _name ] := _value ;
  end ;

  function TGIS_LayerVectorSqlAbstract.passwordCallBack(
    const _token : String
  ) : String ;
  begin
    Result := GisPasswordList.Get( Name, _token ) ;
    if IsStringEmpty( Result ) then begin
      if assigned( FOnPassword ) then
        {$IFDEF OXYGENE}
          Result := FOnPassword( Self,
                                 TGIS_TemplateProducerEventArgs.Create( _token )
                               )
        {$ELSE}
          Result := PasswordEvent( Self, _token )
        {$ENDIF}
      else
        Result := _token ;
      GisPasswordList.Add( Name, _token, Result ) ;
    end ;
  end ;

  function TGIS_LayerVectorSqlAbstract.getCmd(
    const _id : Integer
  ) : String ;
  begin
    assert( _id < FSQLCommands.Count, 'no such command' ) ;
    Result := FSQLCommands[ _id ] ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_LayerVectorSqlAbstract.setUp3 ;
  var
    style : String ;
  begin
    inherited ;

    style := FSQLParameters.Values[ GIS_INI_STYLE ] ;
    if IsStringEmpty( style ) then exit ;

    ApplyStyle( ReadStyle( style ) ) ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.prepareParametersExList ;
  begin
    FSQLParametersEx.Clear ;
  end ;

  function TGIS_LayerVectorSqlAbstract.ExecuteSQL(
    const _sql : String
  ) : Boolean ;
  begin
    try
      oGisDb.sqlExec( _sql ) ;
    except
      on e : EGIS_Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SQLQUERY ), e.Message, 0 ) ;
    end ;
    Result := True ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.OpenConnection;
  begin
    macroConnect ;
  end;

  procedure TGIS_LayerVectorSqlAbstract.CloseConnection;
  begin
    macroDisconnect ;
  end;

  procedure TGIS_LayerVectorSqlAbstract.CloseActiveDatasets ;
  var
    i : Integer ;
  begin
    for i := 0 to BUILTIN_CURSORS - 1 do
      oGisDb.sqlQueryClose(i) ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.ResetDataset ;
  begin
    oGisDb.sqlQueryReset(0) ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.CreateMasterTable ;
  begin
    if not FUseMasterTable then exit ;
    try
      oGisDb.sqlExec( getCmd( ID_ASQL_CREATE_GEOMETRY_COLUMNS_TABLE+FLastCmdId ) ) ;
      oGisDb.sqlExec( getCmd( ID_ASQL_CREATE_GEOMETRY_COLUMNS_INDEX+FLastCmdId ) ) ;
    except
      // can exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.DeleteFromMasterTable ;
  begin
    if not FUseMasterTable then exit ;
    try
      oGisDb.sqlExec(
        Format( getCmd( ID_ASQL_DELETE_FROM_GEOMETRY_COLUMNS+FLastCmdId ),
                [Table]
               )
      ) ;
    except
      // can not exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.AddToMasterTable(
    const _layerName      : String ;
    const _geometryColumn : String ;
    const _indexColumn    : String ;
    const _storage        : String ;
    const _geometryType   : TGIS_ShapeType ;
    const _dim            : TGIS_DimensionType ;
    const _srid           : Integer ;
    const _extent         : TGIS_Extent
  ) ;
  begin
    if not FUseMasterTable then exit ;
    try
      oGisDb.sqlExec(
        Format( getCmd( ID_ASQL_INSERT_INTO_GEOMETRY_COLUMNS+FLastCmdId ),
                [Table,_geometryColumn,_indexColumn,_storage,
                 getGeometryShapeType(_geometryType,_dim),_srid,
                 DotFloatToStr(_extent.XMin),DotFloatToStr(_extent.XMax),
                 DotFloatToStr(_extent.YMin),DotFloatToStr(_extent.YMax)]
               )
      ) ;
    except
      // can not exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.UpdateMasterTable(
    const _layerName      : String ;
    const _geometryType   : TGIS_ShapeType ;
    const _dim            : TGIS_DimensionType ;
    const _srid           : Integer ;
    const _extent         : TGIS_Extent
  ) ;
  begin
    if not FUseMasterTable then exit ;
    try
      oGisDb.sqlExec(
        Format( getCmd( ID_ASQL_UPDATE_GEOMETRY_COLUMNS+FLastCmdId ),
                [getGeometryShapeType(_geometryType,_dim),_srid,
                 DotFloatToStr(_extent.XMin),DotFloatToStr(_extent.XMax),
                 DotFloatToStr(_extent.YMin),DotFloatToStr(_extent.YMax),
                 Table]
               )
      ) ;
    except
      // can not exist
    end ;
  end ;

  function TGIS_LayerVectorSqlAbstract.ReadMasterTable(
     const _layerName      : String ;
     const _storage        : String ;
       var _geometryColumn : String ;
       var _indexColumn    : String ;
       var _geometryType   : TGIS_ShapeType ;
       var _dim            : Integer ;
       var _srid           : Integer ;
       var _extent         : TGIS_Extent
    ) : Boolean ;
  var
    gtype : Integer ;
  begin
    Result := False ;

    if not FUseMasterTable then exit ;
    try
      oGisDb.sqlQueryOpen(
        Format( getCmd( ID_ASQL_SELECT_GEOMETRY_COLUMNS+FLastCmdId ),
                [_layerName,_storage]
               ),
        0
      ) ;
      if not oGisDb.sqlQueryEof(0) then begin
        _geometryColumn := VarToString(oGisDb.sqlQueryGetField('GEOMETRY_COLUMN',0)) ;
        _indexColumn    := VarToString(oGisDb.sqlQueryGetField('INDEX_COLUMN'   ,0)) ;
        gtype           := VarToInt32 (oGisDb.sqlQueryGetField('GEOMETRY_TYPE'  ,0)) ;
        case gtype div 1000 of
          1 : _dim := 3 ;
          2 : _dim := 3 ;
          3 : _dim := 4 ;
        else  _dim := 2 ;
        end ;
        {$IFDEF JAVA}
          case gtype mod 1000 of
            0 : _geometryType := TGIS_ShapeType.Unknown ;
            1 : _geometryType := TGIS_ShapeType.Deleted ;
            2 : _geometryType := TGIS_ShapeType.Point ;
            3 : _geometryType := TGIS_ShapeType.MultiPoint ;
            4 : _geometryType := TGIS_ShapeType.Arc ;
            5 : _geometryType := TGIS_ShapeType.Polygon ;
            6 : _geometryType := TGIS_ShapeType.Complex ;
            7 : _geometryType := TGIS_ShapeType.MultiPatch ;
            8 : _geometryType := TGIS_ShapeType.Null ;
          end;
        {$ELSE}
          _geometryType   := TGIS_ShapeType( gtype mod 1000 ) ;
        {$ENDIF}
        _srid           := VarToInt32 (oGisDb.sqlQueryGetField('SRID',0)) ;
        _extent.XMin    := VarToDouble(oGisDb.sqlQueryGetField('XMIN',0)) ;
        _extent.XMax    := VarToDouble(oGisDb.sqlQueryGetField('XMAX',0)) ;
        _extent.YMin    := VarToDouble(oGisDb.sqlQueryGetField('YMIN',0)) ;
        _extent.YMax    := VarToDouble(oGisDb.sqlQueryGetField('YMAX',0)) ;

        Result := True ;
      end ;
      oGisDb.sqlQueryClose(0) ;
    except
      // can not exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.initializeCommandList(
    const _cnt : Integer
  ) ;
  var
    i : Integer ;
  begin
    if assigned( FOnBeforeDialectChange ) then
      {$IFDEF CLR}
        FOnBeforeDialectChange( Self, EventArgs.Create ) ;
      {$ELSE}
        BeforeDialectChangeEvent( Self ) ;
      {$ENDIF}

    FSQLCommands.Clear ;
    for i := 0 to _cnt do
      FSQLCommands.Add( '' ) ;

    FLastCmdId := _cnt ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.finalizeCommandList ;
  var
    lst : TStringList ;
    i   : Integer ;
  begin
    addCommonCommands ;

    lst := TStringList.Create ;
    try
      lst.AddStrings( FSQLParameters  ) ;
      lst.AddStrings( FSQLDialectList ) ;

      for i := 0 to FSQLCommands.Count - 1 do begin
        FSQLCommands[i] := TemplateProducer( FSQLCommands[i], lst, nil, False ) ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    if assigned( FOnAfterDialectChange ) then
      {$IFDEF CLR}
        FOnAfterDialectChange( Self, EventArgs.Create ) ;
      {$ELSE}
        AfterDialectChangeEvent( Self ) ;
      {$ENDIF}
  end ;

  procedure TGIS_LayerVectorSqlAbstract.initializeConnect(
    const _prepare : Boolean
  ) ;
  var
    i           : Integer     ;
    lst         : TStringList ;
    tmp         : String      ;
    isReadOnly  : Boolean     ;
  begin
    oGisDb.InitializeProvider ;

    lst := TStringList.Create ;
    try
      ReadSQLParamsFromPath( Path, lst  ) ;

      for i := 0 to lst.Count - 1 do begin
        tmp := lst.Names[ i ] ;
        if IsStringEmpty( FSQLParameters.Values[ tmp ] ) then
          FSQLParameters.Values[ tmp ] := lst.Values[ tmp ] ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    // resolve any ID/Password tokens
    {$IFNDEF OXYGENE}
      FSQLParameters.Text  := TemplateProducer( FSQLParameters.Text, nil,
                                                 passwordCallBack, False
                                              ) ;
    {$ELSE}
      FSQLParameters.Text  := TemplateProducer( FSQLParameters.Text, nil,
                                                @passwordCallBack, False
                                              ) ;
    {$ENDIF}
    FSQLDialectList.Text := GetSQLDialect(
                               UpperCase(
                                  FSQLParameters.Values[
                                    GIS_INI_LAYERSQL_DIALECT
                                  ]
                               )
                             ) ;
    FCurrTable     := FSQLParameters.Values[ GIS_INI_LAYERSQL_LAYER    ] ;
    FViewFeatures  := FSQLParameters.Values[ GIS_INI_LAYERSQL_FEATURES ] ;
    isReadOnly     := ParamBoolean(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_READONLY ],
                        False
                      ) ;
    FUseMasterTable:= ParamString(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_METADATA_TABLE ],
                        ''
                      ) = GIS_INI_LAYERSQL_METADATA_TABLE_INTERNAL ;
    if isReadOnly then
      FSubType := FSubType - [ TGIS_LayerSubType.Exportable ] ;

    if FSQLParameters.Values[GIS_INI_LAYERSQL_CONNECTOR_FIREDAC] = 'Ora' then begin
      FSQLDialectList.Values[':WKB_GEOMETRY'] := '' ;
      FSQLDialectList.Values[':GEOMETRY'] := '' ;
      FSQLDialectList.Values[':CELL'] := '' ;
    end ;

    if _prepare then begin
      if FSQLCommands.Count = 0 then
        prepareCommandList ;
    end ;

    FQuoteLeft  := FSQLDialectList.Values[ 'QUOTE LEFT'  ] ;
    if IsStringEmpty( FQuoteLeft ) then
      FQuoteLeft := '"' ;
    FQuoteRight := FSQLDialectList.Values[ 'QUOTE RIGHT' ] ;
    if IsStringEmpty( FQuoteRight ) then
      FQuoteRight := '"' ;

    oGisDb.sqlInitialize( FSQLParameters, FSQLDialectList ) ;

    if assigned( Viewer ) then
      FConnectionPoolId := Viewer.GetHashCode.ToString ;

    FSQLParameters.Values[ GIS_INI_LAYERSQL_CONNECTION_POOL_ID ] := FConnectionPoolId ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.addCommonCommands ;
  begin
    FSQLCommands.Add( ASQL_CREATE_GEOMETRY_COLUMNS_TABLE ) ;
    FSQLCommands.Add( ASQL_CREATE_GEOMETRY_COLUMNS_INDEX ) ;
    FSQLCommands.Add( ASQL_GEOMETRY_COLUMNS              ) ;
    FSQLCommands.Add( ASQL_DELETE_FROM_GEOMETRY_COLUMNS  ) ;
    FSQLCommands.Add( ASQL_INSERT_INTO_GEOMETRY_COLUMNS  ) ;
    FSQLCommands.Add( ASQL_UPDATE_GEOMETRY_COLUMNS       ) ;
    FSQLCommands.Add( ASQL_SELECT_GEOMETRY_COLUMNS       ) ;
    FSQLCommands.Add( ASQL_CREATE_STYLE                  ) ;
    FSQLCommands.Add( ASQL_CREATE_STYLE_INDEX            ) ;
    FSQLCommands.Add( ASQL_SELECT_STYLE                  ) ;
    FSQLCommands.Add( ASQL_INSERT_STYLE                  ) ;
    FSQLCommands.Add( ASQL_UPDATE_STYLE                  ) ;
    FSQLCommands.Add( ASQL_SELECT_STYLES                 ) ;
    FSQLCommands.Add( ASQL_SELECT_STYLES_EX              ) ;
    FSQLCommands.Add( ASQL_CREATE_PROJECT                ) ;
    FSQLCommands.Add( ASQL_CREATE_PROJECT_INDEX          ) ;
    FSQLCommands.Add( ASQL_SELECT_PROJECT                ) ;
    FSQLCommands.Add( ASQL_SELECT_PROJECTS               ) ;
    FSQLCommands.Add( ASQL_SELECT_PROJECT_EX             ) ;
    FSQLCommands.Add( ASQL_INSERT_PROJECT                ) ;
    FSQLCommands.Add( ASQL_UPDATE_PROJECT                ) ;
  end ;

  function TGIS_LayerVectorSqlAbstract.getGeometryShapeType(
    const _type : TGIS_ShapeType ;
    const _dim  : TGIS_DimensionType
  ) : Integer ;
  begin
    {$IFDEF JAVA}
      case _type of
        TGIS_ShapeType.Unknown    : Result := 0 ;
        TGIS_ShapeType.Deleted    : Result := 1 ;
        TGIS_ShapeType.Point      : Result := 2 ;
        TGIS_ShapeType.MultiPoint : Result := 3 ;
        TGIS_ShapeType.Arc        : Result := 4 ;
        TGIS_ShapeType.Polygon    : Result := 5 ;
        TGIS_ShapeType.Complex    : Result := 6 ;
        TGIS_ShapeType.MultiPatch : Result := 7 ;
        TGIS_ShapeType.Null       : Result := 8 ;
      end ;
    {$ELSE}
      Result := ord(_type) ;
    {$ENDIF}

    case _dim of
      TGIS_DimensionType.XYZ   : inc( Result, 1000 ) ;
      TGIS_DimensionType.XYM   : inc( Result, 2000 ) ;
      TGIS_DimensionType.XYZM  : inc( Result, 3000 ) ;
    end ;
  end ;

  function TGIS_LayerVectorSqlAbstract.safeName(
    const _txt : String
  ) : String ;
  begin
    Result := Format( '%s%s%s', [ FQuoteLeft, _txt, FQuoteRight ] ) ;
  end ;

  function TGIS_LayerVectorSqlAbstract.safeParam(
    const _param : String
  ) : String ;
  begin
    Result := oGisDb.safeParam( _param ) ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroConnect ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroDisconnect ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroTableDrop ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroTableAlter(
    const _layer  : TGIS_LayerVector
  ) ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroUpdateStart ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroUpdateEnd ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroEndBatchMode ;
  begin
    oGisDb.InBatchMode := False ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroBeginBatchMode ;
  begin
    oGisDb.InBatchMode := True ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroBuildRtree ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroShapeUpdate(
    const _shp     : TGIS_Shape ;
    const _import  : Boolean
  ) ;
  begin

  end ;

  procedure TGIS_LayerVectorSqlAbstract.macroMasterUpdate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _name   : String         ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin

  end ;

  function TGIS_LayerVectorSqlAbstract.macroMaxNameLength : Integer ;
  begin
    Result := 16 ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.InitializeDirectWrite(
    const _path        : String           ;
    const _extent      : TGIS_Extent      ;
    const _type        : TGIS_ShapeType   ;
    const _dim         : TGIS_DimensionType
  ) ;
  begin
    Build( _path, _extent, _type, _dim ) ;

    DefaultShapeType := _type ;
    DefaultDimension := _dim ;

    macroConnect ;
    PrepareExportFieldNames( macroMaxNameLength, False, False ) ;

    macroTableAlter( Self ) ;
    macroUpdateStart ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.WriteShapeDirect(
    const _shape : TGIS_Shape
  ) ;
  begin
    macroShapeUpdate( _shape, True ) ;
    Extent := GisMaxExtent( Extent, _shape.Extent ) ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.FinalizeDirectWrite ;
  begin
    // update master table
    macroMasterUpdate( Extent, DefaultShapeType, Table, DefaultDimension ) ;

    macroUpdateEnd ;
    macroBuildRtree ;
    FIsModified := False ;

    macroDisconnect ;

    Items.Clear ;
    Fields.Clear ;

    FIsModified := False ;
    FIsOpened   := False ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.CreateStyleTable ;
  begin
    try
      oGisDb.sqlExec( getCmd( ID_ASQL_CREATE_STYLE+FLastCmdId ) ) ;
      oGisDb.sqlExec( getCmd( ID_ASQL_CREATE_STYLE_INDEX+FLastCmdId ) ) ;
    except
      // can exist
    end ;
  end ;

  function TGIS_LayerVectorSqlAbstract.ReadStyle(
    const _name : String
  ) : String ;
  begin
    Result := '' ;
    if IsStringEmpty( _name ) then exit ;

    try
      oGisDb.sqlQueryOpen(
        Format( getCmd( ID_ASQL_SELECT_STYLE+FLastCmdId ), [_name] ), 0
      ) ;
      try
        if not oGisDb.sqlQueryEof(0) then begin
          Result := VarToString( oGisDb.sqlQueryGetField('STYLE_DATA', 0) ) ;
        end ;
      finally
        oGisDb.sqlQueryClose(0) ;
      end ;
    except
      // may not exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.ApplyStyle(
    const _styleData : String
  ) ;
  var
    cfg   : TGIS_Config ;
    lst   : TStringList ;
  begin
    if IsStringEmpty( _styleData ) then exit ;

    cfg  := TGIS_ConfigFactory.CreateConfig( Self, '' ) ;
    try
      with cfg do begin
        lst := TStringList.Create ;
        try
          lst.Text := _styleData ;
          cfg.SetStrings( lst ) ;
        finally
          FreeObject( lst ) ;
        end ;
        SetLayer( Self ) ;
        SetSection( 0, False ) ;
        Self.applyConfigOptions( cfg ) ;
        Self.ParamsList.LoadFromConfig( cfg ) ;
      end ;
    finally
      FreeObject( cfg ) ;
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.WriteStyleEx(
    const _name   : String ;
    const _layer  : TGIS_LayerVector
   ) ;
  var
    cfg   : TGIS_Config ;
    lst   : TStringList ;
  begin
    if IsStringEmpty( _name ) then exit ;
    if not assigned( _layer ) then exit ;

    cfg  := TGIS_ConfigFactory.CreateConfig( Self, '' ) ;
    try
      with cfg do begin
        SetLayer( _layer ) ;
        _layer.ParamsList.SaveToConfig( cfg ) ;

        lst := TStringList.Create ;
        try
          cfg.GetStrings( lst ) ;

          WriteStyle( _name, _layer.Name, lst.Text ) ;
        finally
          FreeObject( lst ) ;
        end ;
      end ;
    finally
      FreeObject( cfg ) ;
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.WriteStyle(
    const _name   : String ;
    const _desc   : String ;
    const _style  : String
   ) ;
  begin
    oGisDb.sqlQueryOpen(
      Format( getCmd( ID_ASQL_SELECT_STYLE+FLastCmdId ), [_name] ), 0
    ) ;
    if oGisDb.sqlQueryEof(0) then begin
      oGisDb.sqlQueryClose(0) ;

      oGisDb.sqlUpdateStart( 0, Format( getCmd( ID_ASQL_SELECT_STYLES_EX+FLastCmdId ),
                                     [ 'IS NULL']
                                      )
                            ) ;
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          if True then
        {$ENDIF}
        {$IFDEF CLR}
          if not( typeOf( oGisDb ).Name = 'TGIS_DbAdo' ) then
        {$ENDIF}
      {$ELSE}
        if not( oGisDb.ClassNameIs('TGIS_DbAdo') ) then
      {$ENDIF}
          oGisDb.sqlTableAppend( 0, getCmd( ID_ASQL_INSERT_STYLE+FLastCmdId ) )
        else
          oGisDb.sqlTableAppend( 0,
            Format( getCmd( ID_ASQL_SELECT_STYLES_EX+FLastCmdId ), [ 'IS NULL'] )
          ) ;

      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'STYLE_NAME' ),
        TGIS_DataType.String,
        TGIS_SubDataType.Unknown,
        64
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'STYLE_DESC' ),
        TGIS_DataType.String,
        TGIS_SubDataType.Unknown,
        length( _desc  )
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'STYLE_DATA' ),
        TGIS_DataType.Memo,
        TGIS_SubDataType.Unknown,
        length( _style )
       ) ;

      oGisDb.sqlTableSetField( 0, 'STYLE_NAME', _name,  length( _name  ) ) ;
      oGisDb.sqlTableSetField( 0, 'STYLE_DESC', _desc,  length( _desc  ) ) ;
      oGisDb.sqlTableSetField( 0, 'STYLE_DATA', _style, length( _style ) ) ;
      oGisDb.sqlTablePost( 0 ) ;
    end
    else begin
      oGisDb.sqlQueryClose(0) ;
      oGisDb.sqlUpdateStart( 0, Format( getCmd( ID_ASQL_SELECT_STYLES_EX+FLastCmdId ),
                                     [ 'IS NULL']
                                      )
                            ) ;
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          if True then
        {$ENDIF}
        {$IFDEF CLR}
          if not( typeOf( oGisDb ).Name = 'TGIS_DbAdo' ) then
        {$ENDIF}
      {$ELSE}
        if not( oGisDb.ClassNameIs('TGIS_DbAdo') ) then
      {$ENDIF}
          oGisDb.sqlTableOpenWrite(
            0,
            Format( getCmd( ID_ASQL_UPDATE_STYLE+FLastCmdId ), [_name] )
          )
        else
          oGisDb.sqlTableOpenWrite( 0,
            Format( getCmd( ID_ASQL_SELECT_STYLES_EX+FLastCmdId ), ['='+QuotedStr(_name)] )
          ) ;

      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'STYLE_DESC' ),
        TGIS_DataType.String,
        TGIS_SubDataType.Unknown,
        length( _desc  )
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'STYLE_DATA' ),
        TGIS_DataType.Memo,
        TGIS_SubDataType.Unknown,
        length( _style )
       ) ;

      oGisDb.sqlTableSetField( 0, 'STYLE_DESC', _desc,  length( _desc  ) ) ;
      oGisDb.sqlTableSetField( 0, 'STYLE_DATA', _style, length( _style ) ) ;
      oGisDb.sqlTablePost( 0 ) ;
    end ;
  end ;


  function TGIS_LayerVectorSqlAbstract.GetAvailableStyles
    : {$IFDEF OXYGENE}
       TGIS_Strings;
     {$ELSE}
       TStrings;
     {$ENDIF}
  var
    sname : String  ;
  begin
    Result := TStringList.Create ;

    try
      oGisDb.sqlQueryOpen( getCmd( ID_ASQL_SELECT_STYLES+FLastCmdId ), 0 ) ;
      try
        while not oGisDb.sqlQueryEof(0) do begin
          sname := VarToString( oGisDb.sqlQueryGetFieldById( 0,0 ) ) ;
          Result.Add( sname ) ;
          oGisDb.sqlQueryMoveNext(0) ;
        end ;
      finally
        oGisDb.sqlQueryClose(0)  ;
      end ;
    except
      // can not exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.CreateProjectTable ;
  begin
    try
      oGisDb.sqlExec( getCmd( ID_ASQL_CREATE_PROJECT+FLastCmdId ) ) ;
      oGisDb.sqlExec( getCmd( ID_ASQL_CREATE_PROJECT_INDEX+FLastCmdId ) ) ;
    except
      // can exist
    end ;
  end ;

  function TGIS_LayerVectorSqlAbstract.GetProject(
    const _name   : String ;
    const _tokens : {$IFDEF OXYGENE}
                      TGIS_StringList
                    {$ELSE}
                       TStringList
                    {$ENDIF}
  ) : TGIS_ConfigAbstract ;
  var
    lst : TStringList ;
    i   : Integer ;
  begin
    Result := TGIS_ConfigFactory.CreateConfig( nil, _name ) ;
    if IsStringEmpty( _name ) then exit ;
    try
      macroConnect ;
      try
        try
          oGisDb.sqlQueryOpen(
            Format( getCmd( ID_ASQL_SELECT_PROJECT+FLastCmdId ), [GetPathNoExt(_name)]),
            0
          ) ;
          if not oGisDb.sqlQueryEof(0) then begin
            lst := TStringList.Create ;
            try
              lst.Text := VarToString( oGisDb.sqlQueryGetFieldById( 0, 0 ) ) ;

              for i := 0 to lst.Count - 1 do
                lst[i] := TemplateProducer( lst[i], _tokens, nil, False ) ;

              TGIS_Config( Result ).SetStrings( lst ) ;
            finally
              FreeObject( lst ) ;
            end ;
          end ;
        except
          // can not exist
        end ;
      finally
        oGisDb.sqlQueryClose(0) ;
        macroDisconnect ;
      end ;
    except
      // wrong connection
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.WriteProject(
    const _name     : String ;
    const _desc     : String ;
    const _project  : String
   ) ;
  begin
    oGisDb.sqlQueryOpen(
      Format( getCmd( ID_ASQL_SELECT_PROJECT+FLastCmdId ), [_name] ), 0
    ) ;
    if oGisDb.sqlQueryEof(0) then begin
      oGisDb.sqlQueryClose(0) ;

      oGisDb.sqlUpdateStart( 0, Format( getCmd( ID_ASQL_SELECT_PROJECT_EX+FLastCmdId ),
                                     [ 'IS NULL']
                                      )
                            ) ;
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          if True then
        {$ENDIF}
        {$IFDEF CLR}
          if not( typeOf( oGisDb ).Name = 'TGIS_DbAdo' ) then
        {$ENDIF}
      {$ELSE}
        if not( oGisDb.ClassNameIs('TGIS_DbAdo') ) then
      {$ENDIF}
          oGisDb.sqlTableAppend( 0, getCmd( ID_ASQL_INSERT_PROJECT+FLastCmdId ) )
        else
          oGisDb.sqlTableAppend( 0,
            Format( getCmd( ID_ASQL_SELECT_PROJECT_EX+FLastCmdId ), [ 'IS NULL'] )
          ) ;

      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'PROJECT_NAME' ),
        TGIS_DataType.String,
        TGIS_SubDataType.Unknown,
        64
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'PROJECT_DESC' ),
        TGIS_DataType.String,
        TGIS_SubDataType.Unknown,
        length( _desc  )
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'PROJECT_DATA' ),
        TGIS_DataType.Memo,
        TGIS_SubDataType.Unknown,
        length( _project )
       ) ;

      oGisDb.sqlTableSetField( 0, 'PROJECT_NAME', _name,    length( _name    ) ) ;
      oGisDb.sqlTableSetField( 0, 'PROJECT_DESC', _desc,    length( _desc    ) ) ;
      oGisDb.sqlTableSetField( 0, 'PROJECT_DATA', _project, length( _project ) ) ;
      oGisDb.sqlTablePost( 0 ) ;
    end
    else begin
      oGisDb.sqlQueryClose(0) ;
      oGisDb.sqlUpdateStart( 0, Format( getCmd( ID_ASQL_SELECT_PROJECT_EX+FLastCmdId ),
                                     [ 'IS NULL']
                                      )
                            ) ;
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          if True then
        {$ENDIF}
        {$IFDEF CLR}
          if not( typeOf( oGisDb ).Name = 'TGIS_DbAdo' ) then
        {$ENDIF}
      {$ELSE}
        if not( oGisDb.ClassNameIs('TGIS_DbAdo') ) then
      {$ENDIF}
          oGisDb.sqlTableOpenWrite(
            0,
            Format( getCmd( ID_ASQL_UPDATE_PROJECT+FLastCmdId ), [_name] )
          )
        else
          oGisDb.sqlTableOpenWrite( 0,
            Format( getCmd( ID_ASQL_SELECT_PROJECT_EX+FLastCmdId ), ['='+QuotedStr(_name)] )
          ) ;

      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'PROJECT_DESC' ),
        TGIS_DataType.String,
        TGIS_SubDataType.Unknown,
        length( _desc  )
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( 'PROJECT_DATA' ),
        TGIS_DataType.Memo,
        TGIS_SubDataType.Unknown,
        length( _project )
       ) ;

      oGisDb.sqlTableSetField( 0, 'PROJECT_DESC', _desc,    length( _desc    ) ) ;
      oGisDb.sqlTableSetField( 0, 'PROJECT_DATA', _project, length( _project ) ) ;
      oGisDb.sqlTablePost( 0 ) ;
    end ;
  end ;

  function TGIS_LayerVectorSqlAbstract.GetAvailableProjects
    : {$IFDEF OXYGENE}
        TGIS_Strings ;
      {$ELSE}
        TStrings     ;
      {$ENDIF}
  var
    sname : String  ;
  begin
    Result := TStringList.Create ;

    try
      oGisDb.sqlQueryOpen( getCmd( ID_ASQL_SELECT_PROJECTS+FLastCmdId ), 0 ) ;
      try
        while not oGisDb.sqlQueryEof(0) do begin
          sname := VarToString( oGisDb.sqlQueryGetFieldById( 0,0 ) ) ;
          Result.Add( sname ) ;
          oGisDb.sqlQueryMoveNext(0) ;
        end ;
      finally
        oGisDb.sqlQueryClose(0)  ;
      end ;
    except
      // can not exist
    end ;
  end ;

  procedure TGIS_LayerVectorSqlAbstract.DeleteLayer ;
  begin
    try
      macroDisconnect ;
      macroConnect ;
      macroTableDrop ;
      DeleteFromMasterTable ;
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;

  end ;

//==================================== END =====================================
end.

