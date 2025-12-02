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
  Encapsulation of a GeoMedia SQL Warehouse access.

  Implementation currently support: MS Jet (Access), MS SQL Server warehouses.

  GeoMedia SQL Layer support was created by Marcin Malinowski.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlGm ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlGm"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    {$IFDEF MSWINDOWS_OS}
    DAO,
    {$ENDIF}
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.Variants,
    System.SysUtils,
    System.Math,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerVectorSql ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {$IFDEF OXYGENE}
    T_cursorSql_LayerSqlGm nested in TGIS_LayerSqlGmAbstract = public {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse : Boolean ;

        /// <summary>
        ///   Is first record?
        /// </summary>
        isFirst : Boolean ;

        /// <summary>
        ///   If True the ordering by JoinPrimary code should be disabled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;

        /// <summary>
        ///   Eof marker.
        /// </summary>
        isEof       : Boolean ;

        /// <summary>
        ///   If True, then attribute query should be done on user side.
        /// </summary>
        fullSearch : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape   : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPoint   : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currArc     : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPolygon : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of abstract GeoMedia SQL layer.
  /// </summary>
  TGIS_LayerSqlGmAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                            class( TGIS_LayerVectorSqlAbstract )
    protected // other protected variables
      {$IFDEF OXYGENE}
        /// <summary>
        ///  sql cursor as array of record
        /// </summary>
        cursorSql : array of T_cursorSql_LayerSqlGm ;
      {$ELSE}
        /// <summary>
        ///  sql cursor as array of record
        /// </summary>
        cursorSql : array of record

          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse : Boolean ;

          /// <summary>
          ///   Is first record?
          /// </summary>
          isFirst : Boolean ;

          /// <summary>
          ///   If True the ordering by JoinPrimary code should be disabled
          ///   upon MoveFirst.
          /// </summary>
          disableJoinPrimary : Boolean ;

          /// <summary>
          ///   Eof marker.
          /// </summary>
          isEof       : Boolean ;

          /// <summary>
          ///   If True, then attribute query should be done on user side.
          /// </summary>
          fullSearch : Boolean ;

          /// <summary>
          ///   Current shape. Layer access is based on record by record access.
          /// </summary>
          currShape   : TGIS_Shape ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currPoint   : TGIS_ShapePoint ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currArc     : TGIS_ShapeArc ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currPolygon : TGIS_ShapePolygon ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currMultipoint : TGIS_ShapeMultiPoint ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   Is New version of Access Warehouse.
      /// </summary>
      isNewVersion : Boolean ;

      /// <summary>
      ///   Full path to Access Warehouse file.
      /// </summary>
      dataSource : String ;

      /// <summary>
      ///   Geometry index column name
      /// </summary>
      nameGeometryColumn : String ;

      /// <summary>
      ///   Geometry blob column name
      /// </summary>
      nameGeometryBlob : String ;

      /// <summary>
      ///   Geometry table name
      /// </summary>
      nameGeometryTable : String ;

      /// <summary>
      ///   Coordinate system GUID
      /// </summary>
      nameCoordSysGUID : String  ;

      /// <summary>
      ///   UID name fixup (GEO.UID or UID) for various db drivers.
      /// </summary>
      fixGEOUID : Integer ;

      /// <summary>
      ///   Recent calculated value of last uid.
      /// </summary>
      lastUid : TGIS_Uid ;

      /// <summary>
      ///   Forced geometry column name.
      /// </summary>
      forcedGeometryColumn : String ;

      /// <summary>
      ///   Can unchanged fields be skipped upon saving.
      /// </summary>
      canSkipField    : Boolean ;

      /// <summary>
      ///   Has a layer text features.
      /// </summary>
      hasText         : Boolean ;

    protected

      /// <summary>
      ///   Read a shape from a SQL query and recreate it.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure readShape           ( const _cursor : Integer ) ;

      /// <summary>
      ///   Copy list from resources into the internal list of commands.
      /// </summary>
      procedure prepareCommandList  ; override;

      /// <summary>
      ///   Prepare and return a SQL command from resources.
      /// </summary>
      /// <param name="_sqlCmd">
      ///   command String
      /// </param>
      /// <returns>
      ///  return a SQL command from resources
      /// </returns>
      function  prepareCommand      ( const _sqlCmd : String
                                    ) : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <param name="_id">
      ///   if 0, then returned 'UID'; if 1, then returned GEO.UID
      /// </param>
      /// <returns>
      ///   If 0, then returned 'UID'; if 1, then returned GEO.UID.
      /// </returns>
      function  getCmdGEOUID        ( const _id : Integer
                                    ) : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///  Shape Type as a String.
      /// </returns>
      function  getCmdSHAPETYPE     : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      /// Xmin as a String.
      /// </returns>
      function  getCmdXMIN          : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///  Ymin as a String.
      /// </returns>
      function  getCmdYMIN          : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <param name="_id">
      ///  geometry id
      /// </param>
      /// <returns>
      ///  Geometry as a String.
      /// </returns>
      function  getCmdGEOMETRY      ( const _id : Integer
                                    ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///  Return a SQL parametrized append command.
      /// </returns>
      function  prepareAppendCommand( const _table  : String
                                    ) : String ;

      /// <summary>
      ///   Return a SQL parametrized update command.
      /// </summary>
      /// <param name="_table">
      ///   table to be updated
      /// </param>
      /// <param name="_column">
      ///   column name used as a filter
      /// </param>
      /// <returns>
      ///  Return a SQL parametrized update command.
      /// </returns>
      function  prepareUpdateCommand( const _table  : String ;
                                      const _column : String
                                    ) : String ;

      /// <summary>
      ///   Return a SQL select command.
      /// </summary>
      /// <param name="_table">
      ///   table to be selected
      /// </param>
      /// <param name="_filter">
      ///   filter expression
      /// </param>
      /// <returns>
      ///  Return a SQL select command.
      /// </returns>
      function  prepareSelectCommand( const _table  : String ;
                                      const _filter : String
                                    ) : String ;

      /// <summary>
      ///   Return a UID=_uid filter.
      /// </summary>
      /// <param name="_uid">
      ///   uid value used to build filter
      /// </param>
      /// <returns>
      ///  Return a UID = _uid filter.
      /// </returns>
      function  prepareFilterUid    ( const _uid    : TGIS_Uid
                                    ) : String ;

      /// <summary>
      ///  Query to get geometry
      /// </summary>
      /// <param name="_uid">
      ///  shape uid
      /// </param>
      /// <param name="_shapetype">
      ///  shape type
      /// </param>
      /// <param name="_cursor">
      ///  shape cursor
      /// </param>
      procedure sqlQueryGetGeometry ( const _uid       : TGIS_Uid ;
                                      const _shapetype : TGIS_ShapeType ;
                                      const _cursor    : Integer
                                    ) ; virtual; abstract;
      /// <summary>
      ///  Get a GEO.UID field from the query.
      /// </summary>
      /// <param name="_cursor">
      ///  cursor identifier
      /// </param>
      /// <returns>
      ///  GEOU.UID field as a String
      /// </returns>
      function  sqlQueryNameGEOUID  ( const _cursor    : Integer
                                    ): String ; virtual; abstract;

      /// <summary>
      ///  Get a GEO.UID field from the query.
      /// </summary>
      /// <param name="_cursor">
      ///  cursor identifier
      /// </param>
      /// <returns>
      ///  GEOU.UID field as Variant
      /// </returns>
      function  sqlQueryGetGEOUID   ( const _cursor    : Integer
                                    ): Variant ; virtual; abstract;

      /// <summary>
      ///  Append a new record into the table.
      /// </summary>
      /// <param name="_id">
      ///  id of the table
      /// </param>
      /// <param name="_table">
      ///  table name
      /// </param>
      procedure sqlTableAppend      ( const _id        : Integer ;
                                      const _table     : String
                                    ) ; virtual; abstract;

      /// <summary>
      /// Open the table based on provided filter.
      /// </summary>
      /// <param name="_id">
      ///  id of the table
      ///</param>
      /// <param name="_table">
      ///  table name
      /// </param>
      /// <param name="_uidCol">
      ///  name of the uid column
      /// </param>
      /// <param name="_uidVal">
      ///  Uid value
      /// </param>
      procedure sqlTableOpenWrite   ( const _id        : Integer ;
                                      const _table     : String  ;
                                      const _uidCol    : String  ;
                                      const _uidVal    : TGIS_Uid
                                    ) ; virtual; abstract;

      /// <summary>
      /// Set a geometry to the table.
      /// </summary>
      /// <param name="_id">
      ///  id of the table
      ///</param>
      /// <param name="_name">
      ///  table name
      /// </param>
      /// <param name="_shp">
      ///  Shape to be inserted in a table.
      /// </param>

      procedure sqlTableSetGeometry ( const _id        : Integer ;
                                      const _name      : String ;
                                      const _shp       : TGIS_Shape
                                    ) ; virtual; abstract;

      /// <inheritdoc/>
      procedure macroConnect        ; override;

      /// <inheritdoc/>
      procedure macroDisconnect     ; override;

      /// <inheritdoc/>
      procedure macroUpdateStart    ; override;

      /// <inheritdoc/>
      procedure macroUpdateEnd      ; override;

      /// <summary>
      ///   Macro for creating master table
      /// </summary>
      procedure macroMasterCreate   ; virtual;

      /// <inheritdoc/>
      procedure macroMasterUpdate   ( const _extent  : TGIS_Extent    ;
                                      const _type    : TGIS_ShapeType ;
                                      const _name    : String ;
                                      const _dim     : TGIS_DimensionType
                                    ) ; override;

      /// <inheritdoc/>
      procedure macroTableCreate    ( const _extent  : TGIS_Extent    ;
                                      const _type    : TGIS_ShapeType
                                    ) ; virtual;

      /// <inheritdoc/>
      procedure macroTableAlter     ( const _layer   : TGIS_LayerVector
                                    ) ; override;

      /// <inheritdoc/>
      procedure macroTableDrop      ; override;

      /// <summary>
      ///   Set for setting a field to the table.
      /// </summary>
      /// <param name="_id">
      ///   field id
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_val">
      ///   value of the field
      /// </param>
      procedure macroTableSetField  ( const _id      : Integer ;
                                      const _name    : String ;
                                      const _val     : Variant
                                    ) ; virtual;

      /// <summary>
      ///   Macro for querying table for the structure (available fields)
      /// </summary>
      procedure macroQueryStructure   ; virtual;

      /// <summary>
      ///   Macro for deleting a shape from both: geometry and feature table.
      /// </summary>
      /// <param name="_uid">
      ///  shape uid value
      /// </param>
      procedure macroShapeDelete    ( const _uid     : TGIS_Uid
                                    ) ; virtual;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_uidname">
      ///   name of uid field
      /// </param>
      /// <param name="_name">
      ///   name of field to be add
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
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDEXIST
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    This method is used only for internal use on layer reading.
      ///    </note>
      /// </remarks>
      procedure macroAddField       ( const _uidname : String         ;
                                      const _name    : String         ;
                                      const _type    : TGIS_FieldType ;
                                      const _width   : Integer        ;
                                      const _decimal : Integer
                                    ) ; overload;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_name">
      ///   name of field to be add
      /// </param>
      /// <param name="_uidname">
      ///   name of uid field
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
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDEXIST
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    This method is used only for internal use on layer reading.
      ///    </note>
      /// </remarks>
      procedure macroAddField       ( const _uidname : String         ;
                                      const _name    : String         ;
                                      const _type    : TGIS_FieldType ;
                                      const _width   : Integer        ;
                                      const _decimal : Integer        ;
                                      const _saved   : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_uidname">
      ///   name of uid field
      /// </param>
      /// <param name="_name">
      ///   name of field to be add
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
      /// <remarks>
      ///   <note type="note">
      ///    This method is used only for internal use on layer reading.
      ///    </note>
      /// </remarks>
      procedure macroAddField       ( const _uidname : String         ;
                                      const _name    : String         ;
                                      const _type    : TGIS_FieldType ;
                                      const _width   : Integer        ;
                                      const _decimal : Integer        ;
                                      const _saved   : Boolean        ;
                                      const _binary  : Integer
                                    ) ; overload;

      /// <inheritdoc/>
      procedure macroShapeUpdate    ( const _shp     : TGIS_Shape ;
                                      const _import  : Boolean
                                    ) ; override;

      /// <summary>
      ///   Macro for obtaining last assigned uid.
      /// </summary>
      /// <returns>
      /// Last assigned uid.
      /// </returns>
      function  macroUidLast        : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for reserving new uid.
      /// </summary>
      /// <returns>
      ///  New uid.
      /// </returns>
      function  macroUidReserve     : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for generating new uid.
      /// </summary>
      /// <param name="_guaranted">
      ///   if True, the provided Uid is unique (for example generated by
      ///   stored procedure); otherwise it is treated only as a starting point
      ///   for further generation
      /// </param>
      /// <returns>
      ///  New uid.
      /// </returns>
      function  macroUidNew         ( var   _guaranted : Boolean
                                    ) : TGIS_Uid ; virtual;

      /// <summary>
      ///   Update single parameter on SQLDialectList.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter; if not exist, then will be created
      /// </param>
      /// <param name="_value">
      ///   value of the parameter; if empty, then parameter will be
      /// </param>
      procedure updateDialectList   ( const _name    : String ;
                                      const _value   : String
                                    ) ;

      /// <summary>
      ///   Macro for fetching proper record form the database.
      /// </summary>
      /// <param name="_uid">
      ///   uid of the shape for which corresponding record should be fetched
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///  True if record can be fetched.
      /// </returns>
      function  macroFetchRecord    ( const _uid     : TGIS_Uid ;
                                      const _cursor  : Integer
                                    ) : Boolean ; virtual;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
    // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      function  getFieldInternal  ( const _uid    : TGIS_Uid   ;
                                    const _name   : String  ;
                                    const _cursor : Integer
                                  ) : Variant ; override;

      /// <summary>
      ///   Makes a general layer setup.
      /// </summary>
      procedure setUp             ; override;

      /// <inheritdoc/>
      function  cursorOpen :  Integer ; override;

      /// <inheritdoc/>
      procedure cursorClose( const _cursor      : Integer
                            ) ; override;

      /// <inheritdoc/>
      procedure cursorFirst( const _cursor      : Integer          ;
                             const _viewerCS    : Boolean          ;
                             const _extent      : TGIS_Extent      ;
                             const _query       : String           ;
                             const _shape       : TGIS_Shape       ;
                             const _de9im       : String           ;
                             const _skipDeleted : Boolean
                           ) ; override;

      /// <inheritdoc/>
      procedure cursorNext ( const _cursor      : Integer
                           ) ; override;

      /// <inheritdoc/>
      function  cursorEof  ( const _cursor      : Integer
                           ) : Boolean ; override;

      /// <inheritdoc/>
      function  cursorShape( const _cursor      : Integer
                           ) : TGIS_Shape ; override;
    protected

      /// <summary>
      ///   Destroy a layer instance.
      /// </summary>
      procedure   doDestroy ; override;

    public

      /// <inheritdoc/>
      constructor Create    ; override;

      /// <summary>
      ///   Build new GeoMedia SQL Warehouse layer.
      /// </summary>
      /// <param name="_path">
      ///   path to .ttkls file; if empty the build base on existing
      ///   parameters SQLParameter; if points to to non-existent file then
      ///   will be treated as a list of CRLF or '\n' delimited parameter
      /// </param>
      /// <param name="_extent">
      ///   starting extent of a layer - cannot be zero-sized
      /// </param>
      /// <param name="_type">
      ///   shape type supported by the layer
      /// </param>
      /// <param name="_dim">
      ///   dimension; for default layer dimension use
      ///   TGIS_DimensionType.Unknown
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Build for details and example. Note that
      ///   existing SQL layer will be deleted.
      /// </remarks>
      procedure Build      ( const _path   : String ;
                             const _extent : TGIS_Extent ;
                             const _type   : TGIS_ShapeType ;
                             const _dim    : TGIS_DimensionType
                           ) ; override;

      /// <inheritdoc/>
      procedure ImportLayerEx( const _layer     : TGIS_LayerVector ;
                               const _extent    : TGIS_Extent;
                               const _type      : TGIS_ShapeType ;
                               const _scope     : String ;
                               const _shape     : TGIS_Shape ;
                               const _de9im     : String ;
                               const _truncated : Boolean
                             ) ; override;

      /// <inheritdoc/>
      procedure MergeLayerEx ( const _layer     : TGIS_LayerVector ;
                               const _extent    : TGIS_Extent;
                               const _type      : TGIS_ShapeType ;
                               const _scope     : String ;
                               const _shape     : TGIS_Shape       ;
                               const _de9im     : String           ;
                               const _truncated : Boolean;
                               const _restrict  : Boolean
                             ) ; override;

      /// <summary>
      ///   Reread fields list from the database.
      /// </summary>
      procedure ReStructure ;

      /// <summary>
      ///   Get available layers with their types from database.
      /// </summary>
      /// <remarks>
      ///    List Objects property keeps layer type as TGIS_ShapeType value
      /// </remarks>
      /// <returns>
      ///   registered layers names and types list
      /// </returns>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

      /// <inheritdoc/>
      function  GetShape    ( const _uid     : TGIS_Uid ;
                              const _cursor  : Integer
                            ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid  : TGIS_Uid ; override;

      /// <inheritdoc/>
      function  GetNewUid   : TGIS_Uid ; override;

      /// <inheritdoc/>
      procedure SaveData    ; override;

      /// <inheritdoc/>
      function  DormantGain : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant     ; override;
  end ;
  TGIS_LayerSqlGmAbstractClass = class of TGIS_LayerSqlGmAbstract ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    {$IFDEF MSWINDOWS}
      System.Win.Registry,
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      Lider.CG.GIS.GeoDaoInt,
    {$ENDIF}
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

type
  T_SQLGM = class
  public
  const

  // Number of retries for concurrent issues.
  GM_RETRY_COUNT             = 9 ;

  // Maximum time of interval between retries in concurrent mode.
  GM_RETRY_INTERVAL          = 100  ;

  // Maximum number of items fetched in GetShape.
  GM_GETSHAPE_FETCHED_LIMIT  = 500  ;

  ID_BEGIN = 0                                                         ;

  CREATE_G_FEATURES_TABLE =
    'CREATE TABLE [GFeatures]'                                             +
    ' ([<#FeatureName#>] <#VARCHAR#>(255) NOT NULL,'                       +
    ' [GeometryType] <#INTEGER#> NULL,'                                    +
    ' [PrimaryGeometryFieldName] <#VARCHAR#>(255) NULL,'                   +
    ' [<#FeatureDescription#>] <#VARCHAR#>(255) NULL,'                     +
    ' CONSTRAINT PK_GFeatures PRIMARY KEY ([<#FeatureName#>]) )'           ;
    ID_CREATE_G_FEATURES_TABLE =
       ID_BEGIN                                                        ;

   CREATE_GEOMETRY_PROPERTIES_TABLE =
     'CREATE TABLE [GeometryProperties]'                                   +
     ' ([IndexID] <#INTEGER#> NOT NULL,'                                   +
     ' [PrimaryGeometryFlag] <#BIT#> NULL,'                                +
     ' [GeometryType] <#INTEGER#> NULL,'                                   +
     ' [GCoordSystemGUID] <#VARCHAR#>(39) NULL,'                           +
     ' [<#FieldDescription#>] <#VARCHAR#>(255) NULL,'                      +
     ' CONSTRAINT PK_GeometryProperties PRIMARY KEY ([IndexID]) )'         ;
    ID_CREATE_GEOMETRY_PROPERTIES_TABLE =
      ID_CREATE_G_FEATURES_TABLE + 1                                   ;

  CREATE_ATTRIBUTE_PROPERTIES_TABLE =
    'CREATE TABLE [AttributeProperties]'                                   +
    ' ([IndexID] <#INTEGER#> NOT NULL,'                                    +
    ' [<#IsKeyField#>] <#BIT#> NULL,'                                      +
    ' [<#FieldDescription#>] <#VARCHAR#>(255) NULL,'                       +
    ' [<#FieldFormat#>] <#VARCHAR#>(255) NULL,'                            +
    ' [<#FieldType#>] <#SMALLINT#> NULL,'                                  +
    ' [<#IsFieldDisplayable#>] <#BIT#> NULL,'                              +
    ' [<#FieldPrecision#>] <#SMALLINT#> NULL, '                            +
    ' CONSTRAINT PK_AttributeProperties PRIMARY KEY ([IndexID]) )'         ;
    ID_CREATE_ATTRIBUTE_PROPERTIES_TABLE =
      ID_CREATE_GEOMETRY_PROPERTIES_TABLE + 1                          ;

  CREATE_FIELD_LOOKUP_TABLE =
    'CREATE TABLE [FieldLookup]'                                           +
    ' ([IndexID] <#COUNTER#> NOT NULL,'                                    +
    ' [<#Table#>] <#VARCHAR#>(255) NOT NULL,'                              +
    ' [<#FieldName#>] <#VARCHAR#>(255) NOT NULL,'                          +
    ' CONSTRAINT PK_FieldLookup PRIMARY KEY ([IndexID]) )'                 ;
    ID_CREATE_FIELD_LOOKUP_TABLE =
      ID_CREATE_ATTRIBUTE_PROPERTIES_TABLE + 1                         ;

  CREATE_G_COORD_SYSTEM_TABLE =
    'CREATE TABLE [GCoordSystem]'                                          +
    ' ([CSGUID] <#VARCHAR#>(39) NOT NULL,'                                 +
    ' [CSGUIDTYPE] <#INTEGER#> NULL,'                                      +
    ' [Name] <#VARCHAR#>(255) NULL,'                                       +
    ' [Description] <#VARCHAR#>(255) NULL,'                                +
    ' [BaseStorageType] <#BYTE#> NULL,'                                 +
    ' [Stor2CompMatrix1] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix2] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix3] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix4] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix5] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix6] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix7] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix8] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix9] <#DOUBLE#> NULL,'                                 +
    ' [Stor2CompMatrix10] <#DOUBLE#> NULL,'                                +
    ' [Stor2CompMatrix11] <#DOUBLE#> NULL,'                                +
    ' [Stor2CompMatrix12] <#DOUBLE#> NULL,'                                +
    ' [Stor2CompMatrix13] <#DOUBLE#> NULL,'                                +
    ' [Stor2CompMatrix14] <#DOUBLE#> NULL,'                                +
    ' [Stor2CompMatrix15] <#DOUBLE#> NULL,'                                +
    ' [Stor2CompMatrix16] <#DOUBLE#> NULL,'                                +
    ' [HeightStorageType] <#BYTE#> NULL,'                               +
    ' [LonNormStorageOpt] <#BYTE#> NULL,'                               +
    ' [GeodeticDatum] <#SMALLINT#> NULL,'                                  +
    ' [Ellipsoid] <#SMALLINT#> NULL,'                                      +
    ' [EquatorialRadius] <#DOUBLE#> NULL,'                                 +
    ' [InverseFlattening] <#DOUBLE#> NULL,'                                +
    ' [ProjAlgorithm] <#SMALLINT#> NULL,'                                  +
    ' [AzimuthAngle] <#DOUBLE#> NULL,'                                     +
    ' [FalseX] <#DOUBLE#> NULL,'                                           +
    ' [FalseY] <#DOUBLE#> NULL,'                                           +
    ' [Hemisphere] <#BYTE#> NULL,'                                      +
    ' [LatOfOrigin] <#DOUBLE#> NULL,'                                      +
    ' [LatOfTrueScale] <#DOUBLE#> NULL,'                                   +
    ' [LonOfOrigin] <#DOUBLE#> NULL,'                                      +
    ' [RadOfStandCircle] <#DOUBLE#> NULL,'                                 +
    ' [ScaleReductFact] <#DOUBLE#> NULL,'                                  +
    ' [StandPar1] <#DOUBLE#> NULL,'                                        +
    ' [StandPar2] <#DOUBLE#> NULL,'                                        +
    ' [Zone] <#SMALLINT#> NULL,'                                           +
    ' [PathNumber] <#SMALLINT#> NULL,'                                     +
    ' [RowNumber] <#SMALLINT#> NULL,'                                      +
    ' [Satellite] <#SMALLINT#> NULL,'                                      +
    ' [XAzDefOpt] <#BYTE#> NULL,'                                       +
    ' [GeomHeightOfOrig] <#DOUBLE#> NULL,'                                 +
    ' [GeomHeightOfPoint1] <#DOUBLE#> NULL,'                               +
    ' [GeomHeightOfPoint2] <#DOUBLE#> NULL,'                               +
    ' [LatOfPoint1] <#DOUBLE#> NULL,'                                      +
    ' [LatOfPoint2] <#DOUBLE#> NULL,'                                      +
    ' [LonOfPoint1] <#DOUBLE#> NULL,'                                      +
    ' [LonOfPoint2] <#DOUBLE#> NULL,'                                      +
    ' [ArgumentOfPerigee] <#DOUBLE#> NULL,'                                +
    ' [EarthRotPeriod] <#DOUBLE#> NULL,'                                   +
    ' [FourierExpansionDegree] <#BYTE#> NULL,'                          +
    ' [NodesInSimpsonIntegration] <#BYTE#> NULL,'                       +
    ' [OrbEarthRotPeriodRatio] <#DOUBLE#> NULL,'                           +
    ' [OrbEcc] <#DOUBLE#> NULL,'                                           +
    ' [OrbInclination] <#DOUBLE#> NULL,'                                   +
    ' [OrbOff] <#DOUBLE#> NULL,'                                           +
    ' [OrbPeriod] <#DOUBLE#> NULL,'                                        +
    ' [OrbSemimajAxis] <#DOUBLE#> NULL,'                                   +
    ' [OblMercDefMode] <#BYTE#> NULL,'                                  +
    ' CONSTRAINT PK_GCoordSystem PRIMARY KEY ([CSGUID]) )'                 ;
     ID_CREATE_G_COORD_SYSTEM_TABLE =
       ID_CREATE_FIELD_LOOKUP_TABLE + 1                                ;

   ALTER_G_COORD_SYSTEM_TABLE =
    'ALTER TABLE [GCoordSystem] ADD'                                       +
    ' [LatOfMapCenter] <#DOUBLE#> NULL,'                                   +
    ' [OblLamConfConDefMode] <#BYTE#> NULL,'                            +
    ' [RotNorthPoleLat] <#DOUBLE#> NULL,'                                  +
    ' [RotNorthPoleLon] <#DOUBLE#> NULL,'                                  +
    ' [GaussianLat] <#DOUBLE#> NULL,'                                      +
    ' [SpherModel] <#BYTE#> NULL,'                                      +
    ' [SpherRadius] <#DOUBLE#> NULL,'                                      +
    ' [LatOfBasisPointA] <#DOUBLE#> NULL,'                                 +
    ' [LatOfBasisPointB] <#DOUBLE#> NULL,'                                 +
    ' [LatOfBasisPointC] <#DOUBLE#> NULL,'                                 +
    ' [LonOfBasisPointA] <#DOUBLE#> NULL,'                                 +
    ' [LonOfBasisPointB] <#DOUBLE#> NULL,'                                 +
    ' [LonOfBasisPointC] <#DOUBLE#> NULL,'                                 +
    ' [ChamTriOriginDefMode] <#BYTE#> NULL,'                            +
    ' [AngOrientationProjPlaneDefMode] <#BYTE#> NULL,'                  +
    ' [AzOfUpwardTilt] <#DOUBLE#> NULL,'                                   +
    ' [FocalLength] <#DOUBLE#> NULL,'                                      +
    ' [HeightAboveEllipAtNadir] <#DOUBLE#> NULL,'                          +
    ' [HeightOrigOfLocalHorizSys] <#DOUBLE#> NULL,'                        +
    ' [LatOrigOfLocalHorizSys] <#DOUBLE#> NULL,'                           +
    ' [LocationOfProjPlaneDefMode] <#BYTE#> NULL,'                      +
    ' [LonOrigOfLocalHorizSys] <#DOUBLE#> NULL,'                           +
    ' [PerspCenterGeocX] <#DOUBLE#> NULL,'                                 +
    ' [PerspCenterGeocY] <#DOUBLE#> NULL,'                                 +
    ' [PerspCenterGeocZ] <#DOUBLE#> NULL,'                                 +
    ' [PerspCenterHeight] <#DOUBLE#> NULL,'                                +
    ' [PerspCenterLat] <#DOUBLE#> NULL,'                                   +
    ' [PerspCenterLon] <#DOUBLE#> NULL,'                                   +
    ' [PerspCenterXEast] <#DOUBLE#> NULL,'                                 +
    ' [PerspCenterYNorth] <#DOUBLE#> NULL,'                                +
    ' [PerspCenterZUp] <#DOUBLE#> NULL,'                                   +
    ' [RefCoordSysDefMode] <#BYTE#> NULL,'                              +
    ' [RotAboutXAxis] <#DOUBLE#> NULL,'                                    +
    ' [RotAboutYAxis] <#DOUBLE#> NULL,'                                    +
    ' [RotAboutZAxis] <#DOUBLE#> NULL,'                                    +
    ' [SwingAng] <#DOUBLE#> NULL,'                                         +
    ' [TiltAng] <#DOUBLE#> NULL,'                                          +
    ' [ExtendProjMatrix1] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix2] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix3] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix4] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix5] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix6] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix7] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix8] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix9] <#DOUBLE#> NULL,'                                +
    ' [ExtendProjMatrix10] <#DOUBLE#> NULL,'                               +
    ' [ExtendProjMatrix11] <#DOUBLE#> NULL,'                               +
    ' [ExtendProjMatrix12] <#DOUBLE#> NULL,'                               +
    ' [ExtendProjMatrix13] <#DOUBLE#> NULL,'                               +
    ' [ExtendProjMatrix14] <#DOUBLE#> NULL,'                               +
    ' [ExtendProjMatrix15] <#DOUBLE#> NULL,'                               +
    ' [ExtendProjMatrix16] <#DOUBLE#> NULL,'                               +
    ' [VerticalDatum] <#SMALLINT#> NULL,'                                  +
    ' [UndulationModel] <#SMALLINT#> NULL,'                                +
    ' [AverageUndulation] <#DOUBLE#> NULL'                                 ;
     ID_ALTER_G_COORD_SYSTEM_TABLE =
       ID_CREATE_G_COORD_SYSTEM_TABLE + 1                              ;

  CREATE_G_ALIAS_TABLE_TABLE =
    'CREATE TABLE [GAliasTable]'                                           +
    ' ([TableType] <#VARCHAR#>(255) NOT NULL,'                             +
    ' [TableName] <#VARCHAR#>(255) NOT NULL,'                              +
    ' CONSTRAINT PK_GAliasTable PRIMARY KEY ([TableType]) )'               ;
     ID_CREATE_G_ALIAS_TABLE_TABLE =
       ID_ALTER_G_COORD_SYSTEM_TABLE + 1                               ;

  CREATE_G_PARAMETERS_TABLE =
    'CREATE TABLE [GParameters]'                                           +
    ' ([GPARAMETER] <#VARCHAR#>(255) NOT NULL,'                            +
    ' [GVALUE] <#VARCHAR#>(255) NOT NULL,'                                 +
    ' CONSTRAINT PK_GParameters PRIMARY KEY ([GPARAMETER]) )'              ;
    ID_CREATE_G_PARAMETERS_TABLE =
      ID_CREATE_G_ALIAS_TABLE_TABLE + 1                                ;

  CREATE_G_FIELD_MAPPING_TABLE =
    'CREATE TABLE [GFieldMapping]'                                         +
    ' ([TABLE_NAME] <#VARCHAR#>(255) NOT NULL,'                            +
    ' [COLUMN_NAME] <#VARCHAR#>(255) NOT NULL,'                            +
    ' [DATA_TYPE] <#INTEGER#> NULL,'                                       +
    ' [DATA_SUBTYPE] <#INTEGER#> NULL,'                                    +
    ' [CSGUID] <#VARCHAR#>(39) NULL,'                                      +
    ' [AUTOINCREMENT] <#BIT#> NULL,'                                       +
    ' CONSTRAINT PK_GFieldMapping PRIMARY KEY'                             +
    ' ([TABLE_NAME], [COLUMN_NAME]) )'                                     ;
    ID_CREATE_G_FIELD_MAPPING_TABLE =
      ID_CREATE_G_PARAMETERS_TABLE + 1                                 ;

  CREATE_MODIFICATION_LOG_TABLE =
    'CREATE TABLE [ModificationLog]'                                       +
    ' ([ModificationNumber] <#COUNTER#> NOT NULL,'                         +
    ' [Type] <#BYTE#> NULL,'                                            +
    ' [ModifiedTableID] <#INTEGER#> NULL,'                                 +
    ' [KeyValue1] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue2] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue3] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue4] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue5] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue6] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue7] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue8] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue9] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyValue10] <#VARCHAR#>(255) NULL,'                                 +
    ' [SESSIONID] <#INTEGER#> NULL,'                                       +
    ' CONSTRAINT PK_ModificationLog PRIMARY KEY ([ModificationNumber]) )'  ;
    ID_CREATE_MODIFICATION_LOG_TABLE =
      ID_CREATE_G_FIELD_MAPPING_TABLE + 1                              ;

  CREATE_MODIFIED_TABLES_TABLE =
    'CREATE TABLE [ModifiedTables]'                                        +
    ' ([ModifiedTableID] <#COUNTER#> NOT NULL,'                            +
    ' [TableName] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField1] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField2] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField3] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField4] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField5] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField6] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField7] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField8] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField9] <#VARCHAR#>(255) NULL,'                                  +
    ' [KeyField10] <#VARCHAR#>(255) NULL,'                                 +
    'CONSTRAINT PK_ModifiedTables PRIMARY KEY ([ModifiedTableID]) )'       ;
    ID_CREATE_MODIFIED_TABLES_TABLE =
      ID_CREATE_MODIFICATION_LOG_TABLE + 1                             ;

  CREATE_G_SQLOPERATORTABLE_TABLE =
    'CREATE TABLE [GSQLOperatorTable]'                                     +
    ' ([Operator] <#VARCHAR#>(32) NULL,'                                   +
    ' [OperatorClass] <#VARCHAR#>(100) NULL,'                              +
    ' [Description] <#TEXT#> NULL )'                                       ;
    ID_CREATE_G_SQLOPERATORTABLE_TABLE =
      ID_CREATE_MODIFIED_TABLES_TABLE + 1                              ;

  CREATE_GADOINDEXCOLUMNS_TABLE =
    'CREATE TABLE [GADOIndexColumns]'                                      +
    '([OBJECT_NAME] <#VARCHAR#> (255) NOT NULL,'                           +
    ' [INDEX_NAME] <#VARCHAR#> (255) NOT NULL,'                            +
    ' [INDEX_TYPE] <#VARCHAR#> (2) NULL,'                                  +
    ' [COLUMN_NAME] <#VARCHAR#> (255) NOT NULL,'                           +
    ' [COLUMN_POSITION] <#INTEGER#> NULL)'                                 ;
    ID_CREATE_GADOINDEXCOLUMNS_TABLE =
      ID_CREATE_G_SQLOPERATORTABLE_TABLE + 1                           ;

  ALTER_GADOINDEXCOLUMNS_TABLE =
    'ALTER TABLE [GADOIndexColumns] WITH NOCHECK '                         +
    'ADD CONSTRAINT PK_GADOIndexColumns PRIMARY KEY CLUSTERED'             +
    '([OBJECT_NAME],[INDEX_NAME],[COLUMN_NAME])'                           ;
    ID_ALTER_GADOINDEXCOLUMNS_TABLE =
      ID_CREATE_GADOINDEXCOLUMNS_TABLE + 1                             ;

  CREATE_TABLE_GEOMETRY_MSSQL =
    'CREATE TABLE %s'                                                      +
    ' ( [<#GDO_ID#>] <#COUNTER#> NOT NULL,'                                +
    ' [<#GEOMETRY#>_XLO] <#DOUBLE#> NULL,'                                 +
    ' [<#GEOMETRY#>_YLO] <#DOUBLE#> NULL,'                                 +
    ' [<#GEOMETRY#>_XHI] <#DOUBLE#> NULL,'                                 +
    ' [<#GEOMETRY#>_YHI] <#DOUBLE#> NULL,'                                 +
    ' [<#GEOMETRY#>] <#BLOB#> NULL,'                                       +
    ' CONSTRAINT PK_%s PRIMARY KEY ([<#GDO_ID#>]) )'                       ;
    ID_CREATE_TABLE_GEOMETRY_MSSQL =
      ID_ALTER_GADOINDEXCOLUMNS_TABLE + 1                              ;

  CREATE_INDEX_XLO =
    'CREATE INDEX %s<#_IDX_#><#XLO#> ON %s ([<#GEOMETRY#>_XLO])'           ;
    ID_CREATE_INDEX_XLO = ID_CREATE_TABLE_GEOMETRY_MSSQL + 1       ;

  CREATE_INDEX_YLO =
    'CREATE INDEX %s<#_IDX_#><#YLO#> ON %s ([<#GEOMETRY#>_YLO])'           ;
    ID_CREATE_INDEX_YLO = ID_CREATE_INDEX_XLO + 1                  ;

  CREATE_INDEX_XHI =
    'CREATE INDEX %s<#_IDX_#><#XHI#> ON %s ([<#GEOMETRY#>_XHI])'           ;
    ID_CREATE_INDEX_XHI = ID_CREATE_INDEX_YLO + 1                  ;

  CREATE_INDEX_YHI =
    'CREATE INDEX %s<#_IDX_#><#YHI#> ON %s ([<#GEOMETRY#>_YHI])'           ;
    ID_CREATE_INDEX_YHI = ID_CREATE_INDEX_XHI + 1                  ;

  CREATE_TABLE_GEOMETRY_MSJET =
    'CREATE TABLE %s'                                                      +
    ' ( [<#GDO_ID#>] <#COUNTER#> NOT NULL,'                                +
    ' [<#GEOMETRY#>] <#BLOB#> NULL,'                                       +
    ' [<#GEOMETRY#>_SK] <#VARCHAR#>(15) NULL,'                             +
    ' CONSTRAINT PK_%s PRIMARY KEY ([<#GDO_ID#>]) )'                       ;
    ID_CREATE_TABLE_GEOMETRY_MSJET =
      ID_CREATE_INDEX_YHI + 1                                          ;

  CREATE_TABLE_FEATURES =
    'CREATE TABLE %s ( [<#GDO_ID#>] <#INTEGER#> )'                         ;
    ID_CREATE_TABLE_FEATURES =
      ID_CREATE_TABLE_GEOMETRY_MSJET + 1                               ;

  DROP_TABLE =
    'DROP TABLE <#IF_EXISTS#> [%s]'                                           ;
    ID_DROP_TABLE =
      ID_CREATE_TABLE_FEATURES + 1                                     ;

  SELECT_G_FEATURES_WHERE =
    'SELECT *'                                                             +
    ' FROM [<#GFeatures#>]'                                                +
    ' WHERE [<#FeatureName#>]=''%s'''                                      +
    ' AND [<#GeometryType#>]<>-1'                                          ;
    ID_SELECT_G_FEATURES_WHERE =
      ID_DROP_TABLE + 1                                                ;

  SELECT_G_FEATURES =
    'SELECT [<#FeatureName#>], [<#GeometryType#>]'                         +
    ' FROM [<#GFeatures#>]'                                                +
    ' WHERE [<#GeometryType#>]<>-1'                                         ;
    ID_SELECT_G_FEATURES =
      ID_SELECT_G_FEATURES_WHERE + 1                                    ;

  SELECT_GEOMETRY_PROPERTIES_WHERE =
    'SELECT *'                                                             +
    ' FROM [<#GeometryProperties#>] <#GP#>,'                               +
    ' [<#FieldLookup#>] <#FL#>'                                            +
    ' WHERE <#FL#>.[<#Table#>]=''%s'''                                     +
    ' AND <#FL#>.[<#IndexID#>]=<#GP#>.[<#IndexID#>]'                       ;
    ID_SELECT_GEOMETRY_PROPERTIES_WHERE =
      ID_SELECT_G_FEATURES + 1                                         ;

  SELECT_GEOMETRY_PROPERTIES_WHERE_F =
    'SELECT *'                                                             +
    ' FROM [<#GeometryProperties#>] <#GP#>,'                               +
    ' [<#FieldLookup#>] <#FL#>'                                            +
    ' WHERE <#FL#>.[<#Table#>]=''%s'' AND <#FL#>.[<#FieldName#>]=''%s'''   +
    ' AND <#FL#>.[<#IndexID#>]=<#GP#>.[<#IndexID#>]'                       ;
    ID_SELECT_GEOMETRY_PROPERTIES_WHERE_F =
      ID_SELECT_GEOMETRY_PROPERTIES_WHERE + 1                        ;

  SELECT_ATTRIBUTE_PROPERTIES_WHERE =
    'SELECT *'                                                             +
    ' FROM [<#AttributeProperties#>] <#AP#>,'                              +
    ' [<#FieldLookup#>] <#FL#>'                                            +
    ' WHERE <#FL#>.[<#Table#>]=''%s'''                                     +
    ' AND <#FL#>.[<#IndexID#>]=<#AP#>.[<#IndexID#>]'                       ;
    ID_SELECT_ATTRIBUTE_PROPERTIES_WHERE =
      ID_SELECT_GEOMETRY_PROPERTIES_WHERE_F + 1                      ;

  SELECT_LAST_GEOMETRY_GDO_ID =
    'SELECT <#MAX#>([<#GDO_ID#>]) <#AS#> <#LASTUID#>'                      +
    ' FROM %s'                                                             ;
    ID_SELECT_LAST_GEOMETRY_GDO_ID =
      ID_SELECT_ATTRIBUTE_PROPERTIES_WHERE + 1                         ;

  SELECT_GEOMETRY_GDO_ID_NAME =
    'SELECT [<#FieldName#>]'                                               +
    ' FROM [<#AttributeProperties#>] <#AP#>,'                              +
    ' [<#FieldLookup#>] <#FL#>'                                            +
    ' WHERE <#FL#>.[<#Table#>]=''%s'''                                     +
    ' AND (<#AP#>.[<#IsKeyField#>]=%d OR'                                  +
    ' <#AP#>.[<#FieldDescription#>] LIKE ''Primary Key'')'                 +
    ' AND <#FL#>.[<#IndexID#>]=<#AP#>.[<#IndexID#>]'                       ;
    ID_SELECT_GEOMETRY_GDO_ID_NAME =
      ID_SELECT_LAST_GEOMETRY_GDO_ID + 1                               ;

  SELECT_GEOMETRY_BLOB_NAME =
    'SELECT [<#FieldName#>], [<#GeometryType#>], [GCoordSystemGUID]'       +
    ' FROM [<#GeometryProperties#>] <#GP#>,'                               +
    ' [<#FieldLookup#>] <#FL#>'                                            +
    ' WHERE <#FL#>.[<#Table#>]=''%s'''                                     +
    ' AND <#FL#>.[<#IndexID#>]=<#GP#>.[<#IndexID#>]'                       ;
    ID_SELECT_GEOMETRY_BLOB_NAME =
      ID_SELECT_GEOMETRY_GDO_ID_NAME + 1                               ;

  SELECT_GEOMETRY_BLOB_NAME_F =
    'SELECT [<#FieldName#>], [<#GeometryType#>], [GCoordSystemGUID]'       +
    ' FROM [<#GeometryProperties#>] <#GP#>,'                               +
    ' [<#FieldLookup#>] <#FL#>'                                            +
    ' WHERE <#FL#>.[<#Table#>]=''%s'' AND <#FL#>.[<#FieldName#>]=''%s'''   +
    ' AND <#FL#>.[<#IndexID#>]=<#GP#>.[<#IndexID#>]'                       ;
    ID_SELECT_GEOMETRY_BLOB_NAME_F =
      ID_SELECT_GEOMETRY_BLOB_NAME + 1                               ;

  SELECT_GEOMETRY_EXTENT =
    'SELECT'                                                               +
    ' <#MIN#>([<#GEOMETRY#>_XLO]) <#AS#> [<#GEOMETRY#>_XLO],'              +
    ' <#MIN#>([<#GEOMETRY#>_YLO]) <#AS#> [<#GEOMETRY#>_YLO],'              +
    ' <#MAX#>([<#GEOMETRY#>_XHI]) <#AS#> [<#GEOMETRY#>_XHI],'              +
    ' <#MAX#>([<#GEOMETRY#>_YHI]) <#AS#> [<#GEOMETRY#>_YHI] '              +
    'FROM %s'                                                              ;
    ID_SELECT_GEOMETRY_EXTENT =
      ID_SELECT_GEOMETRY_BLOB_NAME_F + 1                             ;

  SELECT_TABLE_ALL =
    'SELECT * FROM %s'                                                     ;
    ID_SELECT_TABLE_ALL =
      ID_SELECT_GEOMETRY_EXTENT + 1                                    ;

  SELECT_TABLE_WHERE =
    'SELECT * FROM %s WHERE %s'                                            ;
    ID_SELECT_TABLE_WHERE = ID_SELECT_TABLE_ALL + 1                ;
  {}
  SELECT_JOIN_UID =
    'SELECT * FROM %s <#GEO#>'                                             +
    ' WHERE'                                                               +
    ' (<#GEO#>.%s=%d)'                                                     ;
    ID_SELECT_JOIN_UID = ID_SELECT_TABLE_WHERE + 1                 ;

  SELECT_JOIN =
    'SELECT * FROM %s <#GEO#>'                                             +
    ' WHERE'                                                               +
    ' <#GEO#>.[<#GEOMETRY#>_XHI]>%s AND <#GEO#>.[<#GEOMETRY#>_XLO]<%s AND' +
    ' <#GEO#>.[<#GEOMETRY#>_YHI]>%s AND <#GEO#>.[<#GEOMETRY#>_YLO]<%s'     +
    ' ORDER BY %s'                                                         ;
    ID_SELECT_JOIN = ID_SELECT_JOIN_UID + 1                        ;

  SELECT_JOIN_EX =
    'SELECT * FROM %s <#GEO#>'                                             +
    ' WHERE (%s)'                                                          +
    ' AND '                                                                +
    ' <#GEO#>.[<#GEOMETRY#>_XHI]>%s AND <#GEO#>.[<#GEOMETRY#>_XLO]<%s AND' +
    ' <#GEO#>.[<#GEOMETRY#>_YHI]>%s AND <#GEO#>.[<#GEOMETRY#>_YLO]<%s'     +
    ' ORDER BY %s'                                                         ;
    ID_SELECT_JOIN_EX = ID_SELECT_JOIN + 1                         ;

  SELECT_JOIN_NOEXT =
    'SELECT * FROM %s <#GEO#>'                                             +
    ' ORDER BY %s'                                                         ;
    ID_SELECT_JOIN_NOEXT = ID_SELECT_JOIN_EX + 1                   ;

  SELECT_JOIN_NOEXT_EX =
    'SELECT * FROM %s <#GEO#>'                                             +
    ' WHERE'                                                               +
    ' (%s)'                                                                +
    ' ORDER BY %s'                                                         ;
    ID_SELECT_JOIN_NOEXT_EX =
      ID_SELECT_JOIN_NOEXT + 1                                         ;

  SELECT_CSGUID =
    'SELECT CSGUID FROM GCoordSystem'                                      +
    ' WHERE CSGUID =''%s'''                                                ;
    ID_SELECT_CSGUID =
      ID_SELECT_JOIN_NOEXT_EX + 1                                      ;

  DELETE_SHAPE =
    'DELETE FROM %s WHERE %s=%d'                                           ;
    ID_DELETE_SHAPE =
      ID_SELECT_CSGUID + 1                                             ;

  DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES =
    'DELETE FROM [GeometryProperties]'                                     +
    ' WHERE [IndexID] in ('                                                +
    '  SELECT [IndexID] FROM [FieldLookup]'                                +
    '  WHERE [<#Table#>]=''%s'')'                                          ;
    ID_DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES =
      ID_DELETE_SHAPE + 1                                              ;

  DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES =
    'DELETE FROM [AttributeProperties]'                                    +
    ' WHERE [IndexID] in ('                                                +
    '  SELECT [IndexID] FROM [FieldLookup]'                                +
    '  WHERE [<#Table#>]=''%s'')'                                          ;
    ID_DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES =
      ID_DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES + 1                   ;

  DELETE_FEATURE_FROM_FIELD_LOOKUP =
    'DELETE FROM [FieldLookup]'                                            +
    ' WHERE [<#Table#>]=''%s'''                                            ;
    ID_DELETE_FEATURE_FROM_FIELD_LOOKUP =
      ID_DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES + 1                  ;

  DELETE_FEATURE_FROM_G_FEATURES =
    'DELETE FROM [GFeatures]'                                              +
    ' WHERE [<#FeatureName#>]=''%s'''                                      ;
    ID_DELETE_FEATURE_FROM_G_FEATURES =
      ID_DELETE_FEATURE_FROM_FIELD_LOOKUP + 1                          ;

  DELETE_FIELD_FROM_GEOMETRY_PROPERTIES =
    'DELETE FROM [GeometryProperties]'                                     +
    ' WHERE [IndexID] in ('                                                +
    '  SELECT [IndexID] FROM [FieldLookup]'                                +
    '  WHERE ([<#Table#>]     =''%s'')'                                    +
    '  AND ([<#FieldName#>] =''%s'') )'                                    ;
    ID_DELETE_FIELD_FROM_GEOMETRY_PROPERTIES =
      ID_DELETE_FEATURE_FROM_G_FEATURES + 1                            ;

  DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES =
    'DELETE FROM [AttributeProperties]'                                    +
    ' WHERE [IndexID] in ('                                                +
    '  SELECT [IndexID] FROM [FieldLookup]'                                +
    '  WHERE ([<#Table#>]     =''%s'')'                                    +
    '  AND ([<#FieldName#>] =''%s'') )'                                    ;
    ID_DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES =
      ID_DELETE_FIELD_FROM_GEOMETRY_PROPERTIES + 1                     ;

  DELETE_FIELD_FROM_FIELD_LOOKUP =
    'DELETE FROM [FieldLookup]'                                            +
    ' WHERE ([<#Table#>] =''%s'')'                                         +
    ' AND ([<#FieldName#>]   =''%s'')'                                     ;
    ID_DELETE_FIELD_FROM_FIELD_LOOKUP =
      ID_DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES + 1                    ;

  DELETE_FIELD_FROM_G_FIELD_MAPPING =
    'DELETE FROM [GFieldMapping]'                                          +
    ' WHERE ([TABLE_NAME]  =''%s'')'                                       ;
    ID_DELETE_FIELD_FROM_G_FIELD_MAPPING =
      ID_DELETE_FIELD_FROM_FIELD_LOOKUP + 1                            ;
  {}
  UPDATE_G_FEATURES_TABLE =
    'UPDATE [GFeatures]'                                                   +
    ' SET [GeometryType]=%d'                                               +
    ' WHERE [<#FeatureName#>]=''%s'''                                      ;
    ID_UPDATE_G_FEATURES_TABLE =
      ID_DELETE_FIELD_FROM_G_FIELD_MAPPING + 1                         ;

  UPDATE_GEOMETRY_COLUMNS_TABLE =
    'UPDATE [GEOMETRY_COLUMNS]'                                            +
    ' SET [GEOMETRY_TYPE]=%d'                                              +
    ' WHERE <#G_TABLE_CATALOG#>=''%s'''                                    +
    ' AND <#G_TABLE_SCHEMA#>=''%s'''                                       +
    ' AND <#G_TABLE_NAME#>=''%s'''                                         ;
    ID_UPDATE_GEOMETRY_COLUMNS_TABLE =
      ID_UPDATE_G_FEATURES_TABLE + 1                                   ;

  UPDATE_GEOMETRY_COLUMNS_UID =
    'UPDATE GEOMETRY_COLUMNS'                                              +
    ' SET LASTUID=%d'                                                      +
    ' WHERE <#G_TABLE_CATALOG#>=''%s'''                                    +
    ' AND <#G_TABLE_SCHEMA#>=''%s'''                                       +
    ' AND <#G_TABLE_NAME#>=''%s'''                                         ;
    ID_UPDATE_GEOMETRY_COLUMNS_UID =
      ID_UPDATE_GEOMETRY_COLUMNS_TABLE + 1                             ;

  UPDATE_STATISTICS_MSSQL =
    'UPDATE STATISTICS [%s]'                                               ;
    ID_UPDATE_STATISTICS_MSSQL =
      ID_UPDATE_GEOMETRY_COLUMNS_UID + 1                               ;

  UPDATE_DBX_GEO_MSSQL =
    '<#GEOMETRY#>_XLO=:<#GEOMETRY#>_XLO,'                                  +
    '<#GEOMETRY#>_YLO=:<#GEOMETRY#>_YLO,'                                  +
    '<#GEOMETRY#>_XHI=:<#GEOMETRY#>_XHI,'                                  +
    '<#GEOMETRY#>_YHI=:<#GEOMETRY#>_YHI,'                                  +
    '<#GEOMETRY#>=:<#GEOMETRY#>'                                           ;
  ID_UPDATE_DBX_GEO_MSSQL =
    ID_UPDATE_STATISTICS_MSSQL + 1                                     ;

  UPDATE_DBX_GEO_MSJET =
    '<#GEOMETRY#>=:<#GEOMETRY#>,'                                          +
    '<#GEOMETRY#>_SK=:<#GEOMETRY#>_SK'                                     ;
  ID_UPDATE_DBX_GEO_MSJET =
    ID_UPDATE_DBX_GEO_MSSQL + 1                                        ;

  INSERT_DBX_VALUE_MSSQL =
    '<#GDO_ID#>,<#GEOMETRY#>,'                                             +
    '<#GEOMETRY#>_XLO,<#GEOMETRY#>_YLO,'                                   +
    '<#GEOMETRY#>_XHI,<#GEOMETRY#>_YHI'                                    ;
    ID_INSERT_DBX_VALUE_MSSQL =
      ID_UPDATE_DBX_GEO_MSJET + 1                                      ;

  INSERT_DBX_PARAM_MSSQL =
    ':<#GDO_ID#>,:<#GEOMETRY#>,'                                           +
    ':<#GEOMETRY#>_XLO,:<#GEOMETRY#>_YLO,'                                 +
    ':<#GEOMETRY#>_XHI,:<#GEOMETRY#>_YHI'                                  ;
    ID_INSERT_DBX_PARAM_MSSQL =
      ID_INSERT_DBX_VALUE_MSSQL + 1                                    ;

  INSERT_DBX_VALUE_MSJET =
    '<#GDO_ID#>,<#GEOMETRY#>,<#GEOMETRY#>_SK'                              ;
    ID_INSERT_DBX_VALUE_MSJET =
      ID_INSERT_DBX_PARAM_MSSQL + 1                                    ;

  INSERT_DBX_PARAM_MSJET =
    ':<#GDO_ID#>,:<#GEOMETRY#>,:<#GEOMETRY#>_SK'                           ;
    ID_INSERT_DBX_PARAM_MSJET =
      ID_INSERT_DBX_VALUE_MSJET + 1                                    ;

  UPDATE_DBX =
    'UPDATE %s SET %s WHERE %s=:%s'                                        ;
    ID_UPDATE_DBX = ID_INSERT_DBX_PARAM_MSJET + 1                  ;

  UPDATE_DBX_ORA =
    'returning WKB_GEOMETRY into :WKB_GEOMETRY'                            ;
    ID_UPDATE_DBX_ORA = ID_UPDATE_DBX + 1                          ;

  INSERT_GEOMETRY_PROPERTIES_TABLE =
    'INSERT INTO [GeometryProperties] '                                    +
    '([IndexID], [PrimaryGeometryFlag], [GeometryType],'                   +
    ' [GCoordSystemGUID], [<#FieldDescription#>])'                         +
    ' SELECT [IndexID],%d,%d,''%s'','''''                                  +
    '  FROM [FieldLookup] '                                                +
    '  WHERE ([FieldLookup].[<#Table#>]     =''%s'')'                      +
    '  AND ([FieldLookup].[<#FieldName#>] =''%s'')'                        ;
    ID_INSERT_GEOMETRY_PROPERTIES_TABLE =
      ID_UPDATE_DBX_ORA + 1                                            ;

  INSERT_ATTRIBUTE_PROPERTIES_TABLE =
    'INSERT INTO [AttributeProperties] '                                   +
    '([IndexID],[<#IsKeyField#>],[<#FieldDescription#>],[<#FieldFormat#>],'+
    ' [<#FieldType#>],[<#IsFieldDisplayable#>],[<#FieldPrecision#>])'      +
    ' SELECT [IndexID],%d,'''',''%s'',%d,%d,%d'                            +
    '  FROM [FieldLookup] '                                                +
    '  WHERE ([FieldLookup].[<#Table#>]     =''%s'')'                      +
    '  AND ([FieldLookup].[<#FieldName#>] =''%s'')'                        ;
    ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE =
      ID_INSERT_GEOMETRY_PROPERTIES_TABLE + 1                          ;

  INSERT_FIELD_LOOKUP_TABLE =
    'INSERT INTO [FieldLookup]'                                            +
    ' ([<#Table#>],[<#FieldName#>])'                                       +
    ' VALUES(''%s'',''%s'')'                                               ;
    ID_INSERT_FIELD_LOOKUP_TABLE =
      ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE + 1                         ;

  INSERT_G_FEATURES_TABLE =
    'INSERT INTO [GFeatures]'                                              +
    ' ([<#FeatureName#>],[GeometryType],'                                  +
    '  [PrimaryGeometryFieldName],[<#FeatureDescription#>])'               +
    ' VALUES(''%s'',%d,''%s'','''')'                                       ;
    ID_INSERT_G_FEATURES_TABLE =
      ID_INSERT_FIELD_LOOKUP_TABLE + 1                                 ;

  INSERT_G_COORD_SYSTEM_TABLE =
    'INSERT INTO [GCoordSystem]'                                           +
    ' VALUES(''%s'',2,NULL,''Default'',0,'                                 +
    ' 1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,'                                    +
    ' 0,NULL,0,21,6378137,298.257223563011,0,NULL,NULL,NULL,NULL,'         +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'                            +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'        +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'        +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'        +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'        +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'        +
    ' NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'        +
    ' NULL,NULL,NULL,NULL,NULL,NULL)'                                      ;
    ID_INSERT_G_COORD_SYSTEM_TABLE =
      ID_INSERT_G_FEATURES_TABLE + 1                                   ;

  INSERT_G_FIELD_MAPPING_TABLE =
    'INSERT INTO [GFieldMapping]'                                          +
    ' ([TABLE_NAME],[COLUMN_NAME],[DATA_TYPE],'                            +
    '  [DATA_SUBTYPE],[CSGUID],[AUTOINCREMENT])'                           +
    ' VALUES(''%s'',''%s'',%s,%s,%s,%s)'                                   ;
    ID_INSERT_G_FIELD_MAPPING_TABLE =
      ID_INSERT_G_COORD_SYSTEM_TABLE + 1                               ;

  IMPORT_IDENTITY =
    'SET IDENTITY_INSERT %s ON '                                           +
    'INSERT INTO %s ( <#GDO_ID#> ) VALUES ( %d ) '                         +
    'SET IDENTITY_INSERT %s OFF'                                           ;
    ID_IMPORT_IDENTITY = ID_INSERT_G_FIELD_MAPPING_TABLE + 1       ;

  SELECT_IDENTITY =
    'SELECT @@IDENTITY AS ''<#GDO_ID#>'''                                  ;
    ID_SELECT_IDENTITY = ID_IMPORT_IDENTITY + 1                    ;

  INSERT_EXT =
    'INSERT INTO %s ( %s ) VALUES ( %s ) %s'                               ;
    ID_INSERT_EXT = ID_SELECT_IDENTITY + 1                         ;

  INSERT =
    'INSERT INTO %s VALUES ( %s )'                                         ;
    ID_INSERT = ID_INSERT_EXT + 1                                  ;

  ALTER_DROP_COLUMN =
    'ALTER TABLE %s <#DROP COLUMN#> <#QUOTE LEFT#>%s<#QUOTE RIGHT#>'       ;
    ID_ALTER_DROP_COLUMN = ID_INSERT + 1                           ;

  ALTER_ADD_STRING =
    'ALTER TABLE %s ADD [%s] <#VARCHAR#>(%d)'                              ;
    ID_ALTER_ADD_STRING = ID_ALTER_DROP_COLUMN + 1                 ;

  ALTER_ADD_MEMO =
    'ALTER TABLE %s ADD %s <#CLOB#>'                                           ;
    ID_ALTER_ADD_MEMO = ID_ALTER_ADD_STRING + 1                        ;

  ALTER_ALTER_STRING =
    'ALTER TABLE %s <#ALTER#> [%s] <#VARCHAR_LEN#> <#VARCHAR#>(%d)'        ;
    ID_ALTER_ALTER_STRING = ID_ALTER_ADD_MEMO + 1                ;

  ALTER_ADD_DATE =
    'ALTER TABLE %s ADD [%s] <#DATE#>'                                     ;
    ID_ALTER_ADD_DATE = ID_ALTER_ALTER_STRING + 1                  ;

  ALTER_ADD_BOOLEAN =
    'ALTER TABLE %s ADD [%s] <#BIT#>'                                      ;
    ID_ALTER_ADD_BOOLEAN = ID_ALTER_ADD_DATE + 1                   ;

  ALTER_ADD_INTEGER =
    'ALTER TABLE %s ADD [%s] <#INTEGER#>'                                  ;
    ID_ALTER_ADD_INTEGER = ID_ALTER_ADD_BOOLEAN + 1                ;

  ALTER_ADD_BIGINT =
    'ALTER TABLE %s ADD %s <#BIGINT#>'                                     ;
    ID_ALTER_ADD_BIGINT = ID_ALTER_ADD_INTEGER + 1                 ;

  ALTER_ADD_FLOAT =
    'ALTER TABLE %s ADD [%s] <#DOUBLE#>'                                   ;
    ID_ALTER_ADD_FLOAT = ID_ALTER_ADD_BIGINT + 1                   ;

  FILTER_UID =
    '%s=%d'                                                                ;
    ID_FILTER_UID = ID_ALTER_ADD_FLOAT + 1                         ;

  FILTER_UID_WEAK =
    '(%s>=%d) and (%s<=%d)'                                                ;
    ID_FILTER_UID_WEAK = ID_FILTER_UID + 1                         ;

  FILTER_TABLE =
    '      <#G_TABLE_CATALOG#>=''%s'''                                     +
    '  AND <#G_TABLE_SCHEMA#>=''%s'''                                      +
    '  AND <#G_TABLE_NAME#>=''%s'''                                        ;
    ID_FILTER_TABLE = ID_FILTER_UID_WEAK + 1                       ;

  MAX_NAMELENGTH =
    '<#MAX_NAMELENGTH#>'                                                   ;
    ID_MAX_NAMELENGTH = ID_FILTER_TABLE + 1                        ;

  MAX_TEXTLENGTH =
    '<#MAX_TEXTLENGTH#>'                                                      ;
    ID_MAX_TEXTLENGTH = ID_MAX_NAMELENGTH + 1                          ;

  XMIN =
    '<#GEOMETRY#>_XLO'                                                     ;
    ID_XMIN = ID_MAX_TEXTLENGTH + 1                                ;

  XMAX =
    '<#GEOMETRY#>_XHI'                                                     ;
    ID_XMAX = ID_XMIN + 1                                          ;

  YMIN =
    '<#GEOMETRY#>_YLO'                                                     ;
    ID_YMIN = ID_XMAX + 1                                          ;

  YMAX =
    '<#GEOMETRY#>_YHI'                                                     ;
    ID_YMAX = ID_YMIN + 1                                          ;

  SHAPETYPE =
    '<#SHAPETYPE#>'                                                        ;
    ID_SHAPETYPE = ID_YMAX + 1                                     ;

  GEOMETRY_TYPE =
    '<#GEOMETRYTYPE#>'                                                     ;
    ID_GEOMETRY_TYPE = ID_SHAPETYPE + 1                            ;

  COLUMN_NAME =
    '<#FieldName#>'                                                        ;
    ID_COLUMN_NAME = ID_GEOMETRY_TYPE + 1                          ;

  TABLE_NAME =
    '<#Table#>'                                                            ;
    ID_TABLE_NAME = ID_COLUMN_NAME + 1                             ;

  LASTUID =
    '<#LASTUID#>'                                                          ;
    ID_LASTUID = ID_TABLE_NAME + 1                                 ;

  UID =
    '<#GDO_ID#>'                                                           ;
    ID_UID = ID_LASTUID + 1                                        ;

  GEOMETRY =
    '<#GEOMETRY#>'                                                         ;
    ID_GEOMETRY = ID_UID + 1                                       ;

  GEOMETRY_SK =
    '<#GEOMETRY#>_SK'                                                      ;
    ID_GEOMETRY_SK = ID_GEOMETRY + 1                               ;

  G_COORD_SYSTEM_GUID =
    '<#GCOORDSYSTEMGUID#>'                                                 ;
    ID_G_COORD_SYSTEM_GUID = ID_GEOMETRY_SK + 1                    ;

  GEO =
    '<#GEO#>'                                                              ;
    ID_GEO = ID_G_COORD_SYSTEM_GUID + 1                            ;

  CREATE_INDEX_UID =
    'CREATE UNIQUE INDEX [<#IDX_#><#UID_#>%s] ON %s ([<#GEO_GID#>])'       ;
    ID_CREATE_INDEX_UID = ID_GEO + 1                               ;

  CREATE_INDEX_UIDEXT =
    'CREATE UNIQUE INDEX [<#IDX_#><#UIDEXT_#>%s] ON %s '                   +
    '([<#GEO_GID#>],[<#GEOMETRY#>_XHI],[<#GEOMETRY#>_XLO],'                +
                   '[<#GEOMETRY#>_YHI],[<#GEOMETRY#>_YLO])'                ;
    ID_CREATE_INDEX_UIDEXT =
      ID_CREATE_INDEX_UID + 1                                          ;

  CREATE_INDEX_GEOMETRY_SK =
    'CREATE INDEX [<#IDX_#><#GEOMETRY#>_SK_%s] ON %s ([<#GEOMETRY#>_SK])'  ;
    ID_CREATE_INDEX_GEOMETRY_SK = ID_CREATE_INDEX_UIDEXT + 1       ;

  CREATE_INDEX_G_FEATURES_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>GFeatures] '                             +
    'ON [GFeatures] '                                                      +
    '([<#FeatureName#>])'                                                  ;
    ID_CREATE_INDEX_G_FEATURES_TABLE =
      ID_CREATE_INDEX_GEOMETRY_SK + 1                                  ;

  CREATE_INDEX_ATTRIBUTE_PROPERTIES_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>AttributeProperties] '                   +
    'ON [AttributeProperties] '                                            +
    '([IndexID])'                                                          ;
    ID_CREATE_INDEX_ATTRIBUTE_PROPERTIES_TABLE =
      ID_CREATE_INDEX_G_FEATURES_TABLE + 1                             ;

  CREATE_INDEX_GEOMETRY_PROPERTIES_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>GeometryProperties] '                    +
    'ON [GeometryProperties] '                                             +
    '([IndexID])'                                                          ;
    ID_CREATE_INDEX_GEOMETRY_PROPERTIES_TABLE =
      ID_CREATE_INDEX_ATTRIBUTE_PROPERTIES_TABLE + 1                   ;

  CREATE_INDEX_FIELD_LOOKUP_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>FieldLookup] '                           +
    'ON [FieldLookup] '                                                    +
    '([IndexID])'                                                          ;
    ID_CREATE_INDEX_FIELD_LOOKUP_TABLE =
      ID_CREATE_INDEX_GEOMETRY_PROPERTIES_TABLE + 1                    ;

  CREATE_INDEX_G_COORD_SYSTEM_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>GCoordSystem] '                          +
    'ON [GCoordSystem] '                                                   +
    '([CSGUID])'                                                           ;
    ID_CREATE_INDEX_G_COORD_SYSTEM_TABLE =
      ID_CREATE_INDEX_FIELD_LOOKUP_TABLE + 1                           ;

  CREATE_INDEX_G_ALIAS_TABLE_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>GAliasTable] '                           +
    'ON [GAliasTable] '                                                    +
    '([TableType])'                                                        ;
    ID_CREATE_INDEX_G_ALIAS_TABLE_TABLE =
      ID_CREATE_INDEX_G_COORD_SYSTEM_TABLE + 1                         ;

  CREATE_INDEX_G_PARAMETERS_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>GParameters] '                           +
    'ON [GParameters] '                                                    +
    '([GPARAMETER])'                                                       ;
    ID_CREATE_INDEX_G_PARAMETERS_TABLE =
      ID_CREATE_INDEX_G_ALIAS_TABLE_TABLE + 1                          ;

  CREATE_INDEX_G_FIELD_MAPPING_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>GFieldMapping] '                         +
    'ON [GFieldMapping] '                                                  +
    '([TABLE_NAME], [COLUMN_NAME])'                                        ;
    ID_CREATE_INDEX_G_FIELD_MAPPING_TABLE =
      ID_CREATE_INDEX_G_PARAMETERS_TABLE + 1                           ;

  CREATE_INDEX_MODIFICATION_LOG_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>ModificationLog] '                       +
    'ON [ModificationLog] '                                                +
    '([ModificationNumber])'                                               ;
    ID_CREATE_INDEX_MODIFICATION_LOG_TABLE =
      ID_CREATE_INDEX_G_FIELD_MAPPING_TABLE + 1                        ;

  CREATE_INDEX_MODIFIED_TABLES_TABLE =
    'CREATE UNIQUE INDEX [<#IDX_#>ModifiedTables] '                        +
    'ON [ModifiedTables] '                                                 +
    '([ModifiedTableID])'                                                  ;
    ID_CREATE_INDEX_MODIFIED_TABLES_TABLE =
      ID_CREATE_INDEX_MODIFICATION_LOG_TABLE + 1                       ;

  FIELDS_MSSQL =
    '<#GDO_ID#>'        + #13#10                                           +
    '<#GEOMETRY#>_XLO'  + #13#10                                           +
    '<#GEOMETRY#>_YLO'  + #13#10                                           +
    '<#GEOMETRY#>_XHI'  + #13#10                                           +
    '<#GEOMETRY#>_YHI'  + #13#10                                           +
    '<#GEOMETRY#>'                                                         ;
  ID_FIELDS_MSSQL =
    ID_CREATE_INDEX_MODIFIED_TABLES_TABLE +1                           ;

  FIELDS_MSJET =
    '<#GDO_ID#>'        + #13#10                                           +
    '<#GEOMETRY#>'      + #13#10                                           +
    '<#GEOMETRY#>_SK'   + #13#10                                           +
    'GEOMETRIE_SK'      + #13#10                                           +
    'GraphicText_sk'    + #13#10                                           +
    'SpatialAny_sk'     + #13#10                                           +
    'Geometry2_SK'                                                         ;
  ID_FIELDS_MSJET =
    ID_FIELDS_MSSQL + 1                                                ;

  ID_END = ID_FIELDS_MSJET                                            ;

  // special features
  GIS_SQL_DIALECT_GM_OLD     =   'FeatureName=Name'               + #13#10 +
                                 'FieldName=Field'                + #13#10 +
                                 'IsKeyField=Key'                 + #13#10 +
                                 'FeatureDescription=Description' + #13#10 +
                                 'FieldDescription=Description'   + #13#10 +
                                 'FieldFormat=Format'             + #13#10 +
                                 'FieldType=Type'                 + #13#10 +
                                 'IsFieldDisplayable=Displayable' + #13#10 +
                                 'FieldPrecision=Precision'             ;

  GIS_SQL_DIALECT_GM_NEW     =   'Table=FeatureName'                    ;

  GM_LABEL       = 'GM_LABEL' ;
  GM_LABEL_ANGLE = 'GM_LABEL_ANGLE' ;
  end;

//=============================================================================
// TGIS_LayerSqlGmAbstract
//=============================================================================

  constructor TGIS_LayerSqlGmAbstract.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.prepareCommandList ;
  begin
    initializeCommandList( T_SQLGM.ID_END ) ;

    assert( FSQLCommands[ T_SQLGM.ID_CREATE_G_FEATURES_TABLE                  ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_G_FEATURES_TABLE                  ]
                          := T_SQLGM.CREATE_G_FEATURES_TABLE                  ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_GEOMETRY_PROPERTIES_TABLE         ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_GEOMETRY_PROPERTIES_TABLE         ]
                          := T_SQLGM.CREATE_GEOMETRY_PROPERTIES_TABLE         ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_ATTRIBUTE_PROPERTIES_TABLE        ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_ATTRIBUTE_PROPERTIES_TABLE        ]
                          := T_SQLGM.CREATE_ATTRIBUTE_PROPERTIES_TABLE        ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_FIELD_LOOKUP_TABLE                ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_FIELD_LOOKUP_TABLE                ]
                          := T_SQLGM.CREATE_FIELD_LOOKUP_TABLE                ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_G_COORD_SYSTEM_TABLE              ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_G_COORD_SYSTEM_TABLE              ]
                          := T_SQLGM.CREATE_G_COORD_SYSTEM_TABLE              ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_G_COORD_SYSTEM_TABLE               ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_G_COORD_SYSTEM_TABLE               ]
                          := T_SQLGM.ALTER_G_COORD_SYSTEM_TABLE               ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_G_ALIAS_TABLE_TABLE               ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_G_ALIAS_TABLE_TABLE               ]
                          := T_SQLGM.CREATE_G_ALIAS_TABLE_TABLE               ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_G_PARAMETERS_TABLE                ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_G_PARAMETERS_TABLE                ]
                          := T_SQLGM.CREATE_G_PARAMETERS_TABLE                ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_G_FIELD_MAPPING_TABLE             ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_G_FIELD_MAPPING_TABLE             ]
                          := T_SQLGM.CREATE_G_FIELD_MAPPING_TABLE             ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_MODIFICATION_LOG_TABLE            ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_MODIFICATION_LOG_TABLE            ]
                          := T_SQLGM.CREATE_MODIFICATION_LOG_TABLE            ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_MODIFIED_TABLES_TABLE             ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_MODIFIED_TABLES_TABLE             ]
                          := T_SQLGM.CREATE_MODIFIED_TABLES_TABLE             ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_G_SQLOPERATORTABLE_TABLE          ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_G_SQLOPERATORTABLE_TABLE          ]
                          := T_SQLGM.CREATE_G_SQLOPERATORTABLE_TABLE          ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_GADOINDEXCOLUMNS_TABLE            ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_GADOINDEXCOLUMNS_TABLE            ]
                          := T_SQLGM.CREATE_GADOINDEXCOLUMNS_TABLE            ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_GADOINDEXCOLUMNS_TABLE             ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_GADOINDEXCOLUMNS_TABLE             ]
                          := T_SQLGM.ALTER_GADOINDEXCOLUMNS_TABLE             ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_TABLE_GEOMETRY_MSSQL              ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_TABLE_GEOMETRY_MSSQL              ]
                          := T_SQLGM.CREATE_TABLE_GEOMETRY_MSSQL              ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_XLO                         ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_XLO                         ]
                          := T_SQLGM.CREATE_INDEX_XLO                         ;

    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_XHI                         ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_XHI                         ]
                          := T_SQLGM.CREATE_INDEX_XHI                         ;

    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_YLO                         ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_YLO                         ]
                          := T_SQLGM.CREATE_INDEX_YLO                         ;

    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_YHI                         ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_YHI                         ]
                          := T_SQLGM.CREATE_INDEX_YHI                         ;

    assert( FSQLCommands[ T_SQLGM.ID_CREATE_TABLE_GEOMETRY_MSJET              ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_TABLE_GEOMETRY_MSJET              ]
                          := T_SQLGM.CREATE_TABLE_GEOMETRY_MSJET              ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_TABLE_FEATURES                    ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_TABLE_FEATURES                    ]
                          := T_SQLGM.CREATE_TABLE_FEATURES                    ;
    assert( FSQLCommands[ T_SQLGM.ID_DROP_TABLE                               ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DROP_TABLE                               ]
                          := T_SQLGM.DROP_TABLE                               ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_G_FEATURES_WHERE                  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_G_FEATURES_WHERE                  ]
                          := T_SQLGM.SELECT_G_FEATURES_WHERE                  ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_G_FEATURES                        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_G_FEATURES                        ]
                          := T_SQLGM.SELECT_G_FEATURES                        ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE         ]
                          := T_SQLGM.SELECT_GEOMETRY_PROPERTIES_WHERE         ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE_F       ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE_F       ]
                          := T_SQLGM.SELECT_GEOMETRY_PROPERTIES_WHERE_F       ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_ATTRIBUTE_PROPERTIES_WHERE        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_ATTRIBUTE_PROPERTIES_WHERE        ]
                          := T_SQLGM.SELECT_ATTRIBUTE_PROPERTIES_WHERE        ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_LAST_GEOMETRY_GDO_ID              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_LAST_GEOMETRY_GDO_ID              ]
                          := T_SQLGM.SELECT_LAST_GEOMETRY_GDO_ID              ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_GDO_ID_NAME              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_GDO_ID_NAME              ]
                          := T_SQLGM.SELECT_GEOMETRY_GDO_ID_NAME              ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_BLOB_NAME                ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_BLOB_NAME                ]
                          := T_SQLGM.SELECT_GEOMETRY_BLOB_NAME                ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_BLOB_NAME_F              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_BLOB_NAME_F              ]
                          := T_SQLGM.SELECT_GEOMETRY_BLOB_NAME_F              ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_EXTENT                   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_GEOMETRY_EXTENT                   ]
                          := T_SQLGM.SELECT_GEOMETRY_EXTENT                   ;

    assert( FSQLCommands[ T_SQLGM.ID_SELECT_TABLE_ALL                         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_TABLE_ALL                         ]
                          := T_SQLGM.SELECT_TABLE_ALL                         ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_TABLE_WHERE                       ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_TABLE_WHERE                       ]
                          := T_SQLGM.SELECT_TABLE_WHERE                       ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_UID                          ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_UID                          ]
                          := T_SQLGM.SELECT_JOIN_UID                          ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_JOIN                              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_JOIN                              ]
                          := T_SQLGM.SELECT_JOIN                              ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_EX                           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_EX                           ]
                          := T_SQLGM.SELECT_JOIN_EX                           ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_NOEXT                        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_NOEXT                        ]
                          := T_SQLGM.SELECT_JOIN_NOEXT                        ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_NOEXT_EX                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_JOIN_NOEXT_EX                     ]
                          := T_SQLGM.SELECT_JOIN_NOEXT_EX                     ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_CSGUID                            ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_CSGUID                            ]
                          := T_SQLGM.SELECT_CSGUID                            ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_SHAPE                             ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_SHAPE                             ]
                          := T_SQLGM.DELETE_SHAPE                             ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES  ]
                          := T_SQLGM.DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES  ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES ]
                          := T_SQLGM.DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_FIELD_LOOKUP         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_FIELD_LOOKUP         ]
                          := T_SQLGM.DELETE_FEATURE_FROM_FIELD_LOOKUP         ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_G_FEATURES           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FEATURE_FROM_G_FEATURES           ]
                          := T_SQLGM.DELETE_FEATURE_FROM_G_FEATURES           ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_GEOMETRY_PROPERTIES    ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_GEOMETRY_PROPERTIES    ]
                          := T_SQLGM.DELETE_FIELD_FROM_GEOMETRY_PROPERTIES    ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES   ]
                          := T_SQLGM.DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES   ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_FIELD_LOOKUP           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_FIELD_LOOKUP           ]
                          := T_SQLGM.DELETE_FIELD_FROM_FIELD_LOOKUP           ;
    assert( FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_G_FIELD_MAPPING        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_DELETE_FIELD_FROM_G_FIELD_MAPPING        ]
                          := T_SQLGM.DELETE_FIELD_FROM_G_FIELD_MAPPING        ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_GEOMETRY_PROPERTIES_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_GEOMETRY_PROPERTIES_TABLE         ]
                          := T_SQLGM.INSERT_GEOMETRY_PROPERTIES_TABLE         ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE        ]
                          := T_SQLGM.INSERT_ATTRIBUTE_PROPERTIES_TABLE        ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE                ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE                ]
                          := T_SQLGM.INSERT_FIELD_LOOKUP_TABLE                ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_G_COORD_SYSTEM_TABLE              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_G_COORD_SYSTEM_TABLE              ]
                          := T_SQLGM.INSERT_G_COORD_SYSTEM_TABLE              ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_G_FEATURES_TABLE                  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_G_FEATURES_TABLE                  ]
                          := T_SQLGM.INSERT_G_FEATURES_TABLE                  ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_G_FIELD_MAPPING_TABLE             ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_G_FIELD_MAPPING_TABLE             ]
                          := T_SQLGM.INSERT_G_FIELD_MAPPING_TABLE             ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_G_FEATURES_TABLE                  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_G_FEATURES_TABLE                  ]
                          := T_SQLGM.UPDATE_G_FEATURES_TABLE                  ;

    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_GEOMETRY_COLUMNS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_GEOMETRY_COLUMNS_TABLE            ]
                          := T_SQLGM.UPDATE_GEOMETRY_COLUMNS_TABLE            ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_GEOMETRY_COLUMNS_UID              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_GEOMETRY_COLUMNS_UID              ]
                          := T_SQLGM.UPDATE_GEOMETRY_COLUMNS_UID              ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_STATISTICS_MSSQL                  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_STATISTICS_MSSQL                  ]
                          := T_SQLGM.UPDATE_STATISTICS_MSSQL                  ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_DBX_GEO_MSSQL                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_DBX_GEO_MSSQL                     ]
                          := T_SQLGM.UPDATE_DBX_GEO_MSSQL                     ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_DBX_GEO_MSJET                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_DBX_GEO_MSJET                     ]
                          := T_SQLGM.UPDATE_DBX_GEO_MSJET                     ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_DBX_VALUE_MSSQL                   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_DBX_VALUE_MSSQL                   ]
                          := T_SQLGM.INSERT_DBX_VALUE_MSSQL                   ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_DBX_PARAM_MSSQL                   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_DBX_PARAM_MSSQL                   ]
                          := T_SQLGM.INSERT_DBX_PARAM_MSSQL                   ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_DBX_VALUE_MSJET                   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_DBX_VALUE_MSJET                   ]
                          := T_SQLGM.INSERT_DBX_VALUE_MSJET                   ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_DBX_PARAM_MSJET                   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_DBX_PARAM_MSJET                   ]
                          := T_SQLGM.INSERT_DBX_PARAM_MSJET                   ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_DBX                               ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_DBX                               ]
                          := T_SQLGM.UPDATE_DBX                               ;
    assert( FSQLCommands[ T_SQLGM.ID_UPDATE_DBX_ORA                           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UPDATE_DBX_ORA                           ]
                          := T_SQLGM.UPDATE_DBX_ORA                           ;
    assert( FSQLCommands[ T_SQLGM.ID_IMPORT_IDENTITY                          ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_IMPORT_IDENTITY                          ]
                          := T_SQLGM.IMPORT_IDENTITY                          ;
    assert( FSQLCommands[ T_SQLGM.ID_SELECT_IDENTITY                          ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SELECT_IDENTITY                          ]
                          := T_SQLGM.SELECT_IDENTITY                          ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT_EXT                               ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT_EXT                               ]
                          := T_SQLGM.INSERT_EXT                               ;
    assert( FSQLCommands[ T_SQLGM.ID_INSERT                                   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_INSERT                                   ]
                          := T_SQLGM.INSERT                                   ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_DROP_COLUMN                        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_DROP_COLUMN                        ]
                          := T_SQLGM.ALTER_DROP_COLUMN                        ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_STRING                         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_STRING                         ]
                          := T_SQLGM.ALTER_ADD_STRING                         ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_MEMO                           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_MEMO                           ]
                          := T_SQLGM.ALTER_ADD_MEMO                           ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ALTER_STRING                       ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ALTER_STRING                       ]
                          := T_SQLGM.ALTER_ALTER_STRING                       ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_DATE                           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_DATE                           ]
                          := T_SQLGM.ALTER_ADD_DATE                           ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_BOOLEAN                        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_BOOLEAN                        ]
                          := T_SQLGM.ALTER_ADD_BOOLEAN                        ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_INTEGER                        ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_INTEGER                        ]
                          := T_SQLGM.ALTER_ADD_INTEGER                        ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_BIGINT                         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_BIGINT                         ]
                          := T_SQLGM.ALTER_ADD_BIGINT                         ;
    assert( FSQLCommands[ T_SQLGM.ID_ALTER_ADD_FLOAT                          ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_ALTER_ADD_FLOAT                          ]
                          := T_SQLGM.ALTER_ADD_FLOAT                          ;
    assert( FSQLCommands[ T_SQLGM.ID_FILTER_UID                               ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_FILTER_UID                               ]
                          := T_SQLGM.FILTER_UID                               ;
    assert( FSQLCommands[ T_SQLGM.ID_FILTER_UID_WEAK                          ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_FILTER_UID_WEAK                          ]
                          := T_SQLGM.FILTER_UID_WEAK                          ;
    assert( FSQLCommands[ T_SQLGM.ID_FILTER_TABLE                             ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_FILTER_TABLE                             ]
                          := T_SQLGM.FILTER_TABLE                             ;
    assert( FSQLCommands[ T_SQLGM.ID_MAX_NAMELENGTH                           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_MAX_NAMELENGTH                           ]
                          := T_SQLGM.MAX_NAMELENGTH                           ;
    assert( FSQLCommands[ T_SQLGM.ID_MAX_TEXTLENGTH                           ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_MAX_TEXTLENGTH                           ]
                          := T_SQLGM.MAX_TEXTLENGTH                           ;
    assert( FSQLCommands[ T_SQLGM.ID_XMIN                                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_XMIN                                     ]
                          := T_SQLGM.XMIN                                     ;
    assert( FSQLCommands[ T_SQLGM.ID_XMAX                                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_XMAX                                     ]
                          := T_SQLGM.XMAX                                     ;
    assert( FSQLCommands[ T_SQLGM.ID_YMIN                                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_YMIN                                     ]
                          := T_SQLGM.YMIN                                     ;
    assert( FSQLCommands[ T_SQLGM.ID_YMAX                                     ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_YMAX                                     ]
                          := T_SQLGM.YMAX                                     ;
    assert( FSQLCommands[ T_SQLGM.ID_SHAPETYPE                                ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_SHAPETYPE                                ]
                          := T_SQLGM.SHAPETYPE                                ;
    assert( FSQLCommands[ T_SQLGM.ID_GEOMETRY_TYPE                            ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_GEOMETRY_TYPE                            ]
                          := T_SQLGM.GEOMETRY_TYPE                            ;
    assert( FSQLCommands[ T_SQLGM.ID_LASTUID                                  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_LASTUID                                  ]
                          := T_SQLGM.LASTUID                                  ;
    assert( FSQLCommands[ T_SQLGM.ID_UID                                      ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_UID                                      ]
                          := T_SQLGM.UID                                      ;
    assert( FSQLCommands[ T_SQLGM.ID_GEOMETRY                                 ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_GEOMETRY                                 ]
                          := T_SQLGM.GEOMETRY                                 ;
    assert( FSQLCommands[ T_SQLGM.ID_GEOMETRY_SK                              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_GEOMETRY_SK                              ]
                          := T_SQLGM.GEOMETRY_SK                              ;
    assert( FSQLCommands[ T_SQLGM.ID_G_COORD_SYSTEM_GUID                      ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_G_COORD_SYSTEM_GUID                      ]
                          := T_SQLGM.G_COORD_SYSTEM_GUID                      ;
    assert( FSQLCommands[ T_SQLGM.ID_GEO                                      ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_GEO                                      ]
                          := T_SQLGM.GEO                                      ;
    assert( FSQLCommands[ T_SQLGM.ID_COLUMN_NAME                              ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_COLUMN_NAME                              ]
                          := T_SQLGM.COLUMN_NAME                              ;
    assert( FSQLCommands[ T_SQLGM.ID_TABLE_NAME                               ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_TABLE_NAME                               ]
                          := T_SQLGM.TABLE_NAME                               ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_UID                         ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_UID                         ]
                          := T_SQLGM.CREATE_INDEX_UID                         ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_UIDEXT                      ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_UIDEXT                      ]
                          := T_SQLGM.CREATE_INDEX_UIDEXT                      ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_GEOMETRY_SK                 ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_GEOMETRY_SK                 ]
                          := T_SQLGM.CREATE_INDEX_GEOMETRY_SK                 ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_FEATURES_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_FEATURES_TABLE            ]
                          := T_SQLGM.CREATE_INDEX_G_FEATURES_TABLE            ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_GEOMETRY_PROPERTIES_TABLE   ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_GEOMETRY_PROPERTIES_TABLE   ]
                          := T_SQLGM.CREATE_INDEX_GEOMETRY_PROPERTIES_TABLE   ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_ATTRIBUTE_PROPERTIES_TABLE  ] = '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_ATTRIBUTE_PROPERTIES_TABLE  ]
                          := T_SQLGM.CREATE_INDEX_ATTRIBUTE_PROPERTIES_TABLE  ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_FIELD_LOOKUP_TABLE          ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_FIELD_LOOKUP_TABLE          ]
                          := T_SQLGM.CREATE_INDEX_FIELD_LOOKUP_TABLE          ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_COORD_SYSTEM_TABLE        ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_COORD_SYSTEM_TABLE        ]
                          := T_SQLGM.CREATE_INDEX_G_COORD_SYSTEM_TABLE        ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_ALIAS_TABLE_TABLE         ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_ALIAS_TABLE_TABLE         ]
                          := T_SQLGM.CREATE_INDEX_G_ALIAS_TABLE_TABLE         ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_PARAMETERS_TABLE          ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_G_PARAMETERS_TABLE          ]
                          := T_SQLGM.CREATE_INDEX_G_PARAMETERS_TABLE          ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_MODIFICATION_LOG_TABLE      ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_MODIFICATION_LOG_TABLE      ]
                          := T_SQLGM.CREATE_INDEX_MODIFICATION_LOG_TABLE      ;
    assert( FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_MODIFIED_TABLES_TABLE       ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_CREATE_INDEX_MODIFIED_TABLES_TABLE       ]
                          := T_SQLGM.CREATE_INDEX_MODIFIED_TABLES_TABLE       ;

    assert( FSQLCommands[ T_SQLGM.ID_FIELDS_MSSQL                             ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_FIELDS_MSSQL                             ]
                          := T_SQLGM.FIELDS_MSSQL                             ;
    assert( FSQLCommands[ T_SQLGM.ID_FIELDS_MSJET                             ] =  '' ) ;
            FSQLCommands[ T_SQLGM.ID_FIELDS_MSJET                             ]
                          := T_SQLGM.FIELDS_MSJET                             ;

    finalizeCommandList ;
  end ;

  function TGIS_LayerSqlGmAbstract.prepareCommand(
    const _sqlCmd : String
   ) : String ;
  begin
    Result := TemplateProducer( _sqlCmd, FSQLDialectList, nil, False ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.getCmdGEOUID(
    const _id : Integer
   ) : String ;
  begin
    assert( _id in [0..1] ) ;
    if      _id = 0 then
         Result := nameGeometryColumn
    else if _id = 1 then
         Result := getCmd( T_SQLGM.ID_GEO ) + '.' + nameGeometryColumn ;
  end ;

  function TGIS_LayerSqlGmAbstract.getCmdSHAPETYPE : String ;
  begin
    Result := getCmd( T_SQLGM.ID_GEOMETRY_TYPE ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.getCmdXMIN : String ;
  begin
    Result := getCmd( T_SQLGM.ID_XMIN ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.getCmdYMIN : String ;
  begin
    Result := getCmd( T_SQLGM.ID_YMIN ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.getCmdGEOMETRY(
    const _id : Integer
  ) : String ;
  begin
    assert( _id in [0..1] ) ;
    if      _id = 0 then
         Result := getCmd( T_SQLGM.ID_GEOMETRY )
    else if _id = 1 then
         Result := getCmd( T_SQLGM.ID_GEO ) + '.' + getCmd( T_SQLGM.ID_GEOMETRY ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.prepareAppendCommand(
    const _table : String
   ) : String ;
  var
    i         : Integer ;
    param     : String  ;
    value     : String  ;
    tmp       : String  ;
  begin
    value := '' ;
    param := '' ;
    tmp   := '' ;

    if oGisDb.IsMsSql then begin
      value := getCmd( T_SQLGM.ID_INSERT_DBX_VALUE_MSSQL ) ;
      param := getCmd( T_SQLGM.ID_INSERT_DBX_PARAM_MSSQL ) ;
    end
    else begin
      value := getCmd( T_SQLGM.ID_INSERT_DBX_VALUE_MSJET ) ;
      param := getCmd( T_SQLGM.ID_INSERT_DBX_PARAM_MSJET ) ;
    end ;

    for i := 0 to Fields.Count - 1 do begin
      if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

      if not IsStringEmpty( value ) then
        value := value + ',' ;
      value := value + FieldInfo( i ).ExportName ;

      if not IsStringEmpty( param ) then
        param := param + ',' ;
      param := param +
               ':' + FieldInfo( i ).ExportName ;
    end ;

    Result := Format( getCmd( T_SQLGM.ID_INSERT_EXT ), [_table, value, param, tmp] ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.prepareUpdateCommand(
     const _table  : String ;
     const _column : String
   ) : String ;
  var
    i         : Integer ;
    param     : String  ;
  begin

    param := '' ;

    if oGisDb.IsMsSql then param := getCmd( T_SQLGM.ID_UPDATE_DBX_GEO_MSSQL )
                      else param := getCmd( T_SQLGM.ID_UPDATE_DBX_GEO_MSJET ) ;

    for i:=0 to Fields.Count - 1 do begin
      if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

      if not IsStringEmpty( param ) then
        param := param + ',' ;
      param := param +
               FieldInfo( i ).ExportName + '=:' +
               FieldInfo( i ).ExportName ;
    end ;

    Result := Format( getCmd( T_SQLGM.ID_UPDATE_DBX ),
                      [_table, param, _column, _column ]
                    ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.prepareSelectCommand(
    const _table  : String ;
    const _filter : String
  ) : String ;
  begin
    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLGM.ID_SELECT_TABLE_WHERE ), [ _table, _filter ] )
    else
      Result := Format( getCmd( T_SQLGM.ID_SELECT_TABLE_ALL ), [ _table ] ) ;
  end ;

  function TGIS_LayerSqlGmAbstract.prepareFilterUid(
    const _uid : TGIS_Uid
   ) : String ;
  begin
    Result := Format( getCmd( T_SQLGM.ID_FILTER_UID ),
                      [ nameGeometryColumn , _uid ]
                    ) ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroConnect ;
  var
    tkn         : TGIS_Tokenizer ;
    {$IFDEF MSWINDOWS}
    reg         : TRegistry      ;
    {$ENDIF}

    function remove_quotes( const _txt : String ) : String ;
    var
      b,e, sl : Integer ;
      cut     : Boolean ;
    begin
      if IsStringEmpty( _txt ) then begin
        Result := '' ;
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

  begin
    initializeConnect( False ) ;

    nameGeometryTable    := Table ;
    forcedGeometryColumn := Trim( FSQLParameters.Values['GeometryColumn'] ) ;

    oGisDb.CurrentSQLDialect := UpperCase( FSQLParameters.Values
                                           [ GIS_INI_LAYERSQL_DIALECT ] ) ;

    if not ( oGisDb.IsMsSql or oGisDb.IsMsJet ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDDIALECT ),
                                   FSQLParameters.Values[ GIS_INI_LAYERSQL_DIALECT ],
                                   0
                                 ) ;
    // Important only for MS Jet (MS Access) Warehouse
    if oGisDb.IsMsJet then begin

      isNewVersion :=
        UpperCase( GIS_INI_LAYERSQL_VERSION_OLD                    ) <>
        UpperCase( FSQLParameters.Values[GIS_INI_LAYERSQL_VERSION] ) ;

      // Important only for MS Jet (MS Access) Warehouse
      tkn := TGIS_Tokenizer.Create ;
      try
        if oGisDb.IsJDBC then
          tkn.ExecuteEx( remove_quotes(FSQLParameters.Values[GIS_INI_LAYERSQL_CONNECTOR_JDBC]), ';', ' ' )
        else
          tkn.ExecuteEx( remove_quotes(FSQLParameters.Values[GIS_INI_LAYERSQL_CONNECTOR_ADO]), ';', ' ' ) ;

        if ( ( tkn.Result.Count = 0 ) or ( IsStringEmpty( tkn.Result.Strings [ 0 ] ) ) ) then
          tkn.ExecuteEx( FSQLParameters.Values['DATABASE'], ';', ' ' ) ;

        if ( ( tkn.Result.Count = 0 ) or ( IsStringEmpty( tkn.Result.Strings [ 0 ] ) ) ) then
          {$IFNDEF OXYGENE}
            raise EGIS_Exception.Create( GIS_RS_ERR_BADPARAM,
                                         GIS_INI_LAYERSQL_LAYER,
                                         0
                                       )
          {$ELSE}
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                         GIS_INI_LAYERSQL_LAYER,
                                         0
                                       )
          {$ENDIF}
        else begin
          dataSource := tkn.Result.Values[ 'Data Source' ] ;

          if IsStringEmpty( dataSource ) then
            dataSource := tkn.Result[ 0 ] ;

          if not IsStringEmpty( dataSource ) then
            dataSource := GetPathAbsolute( GetFileDir(Path),
                                           dataSource
                                         ) ;
          if IsStringEmpty( dataSource ) then
            dataSource := tkn.Result.Values[ 'DBQ' ] ;

          if IsStringEmpty( dataSource ) then begin
            dataSource := tkn.Result.Values[ 'DSN' ] ;

            if not IsStringEmpty( dataSource ) then begin
              {$IFDEF MSWINDOWS}
              reg:= TRegistry.Create;
              try
                reg.RootKey := HKEY_LOCAL_MACHINE ;
                if reg.OpenKey( Format( 'SOFTWARE\ODBC\odbc.ini\%s',
                                        [dataSource] ), False ) then begin
                  dataSource := reg.ReadString('DBQ') ;
                  reg.CloseKey ;
                end ;
                reg.RootKey:= HKEY_CURRENT_USER ;
                if reg.OpenKey( Format( 'SOFTWARE\ODBC\odbc.ini\%s',
                                        [dataSource] ), False ) then begin
                  dataSource := reg.ReadString('DBQ') ;
                  reg.CloseKey ;
                end ;
              finally
                FreeObject( reg ) ;
              end ;
              {$ENDIF}
            end
            else
              dataSource := '' ;

          end ;

          if IsStringEmpty( dataSource ) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                         GIS_INI_LAYERSQL_CONNECTOR_ADO,
                                         0
                                       ) ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;

    end ;

    if oGisDb.IsMsSql then begin
      isNewVersion := True ;
      FSQLDialectList.Text := GetSQLDialect( GIS_SQL_DIALECT_NAME_MSSQL )
    end
    else
      FSQLDialectList.Text := GetSQLDialect( GIS_SQL_DIALECT_NAME_MSJET ) ;

    if isNewVersion then
      FSQLDialectList.Text := FSQLDialectList.Text + #13#10 +
                              T_SQLGM.GIS_SQL_DIALECT_GM_NEW
    else
      FSQLDialectList.Text := FSQLDialectList.Text + #13#10 +
                              T_SQLGM.GIS_SQL_DIALECT_GM_OLD  ;

    if FSQLCommands.Count = 0 then
      prepareCommandList ;

    oGisDb.sqlInitialize( FSQLParameters, FSQLDialectList );

    nameGeometryColumn := getCmd( T_SQLGM.ID_UID ) ;
    nameGeometryBlob   := getCmd( T_SQLGM.ID_GEOMETRY ) ;

    try
      oGisDb.sqlConnect( oGisDb.sqlBaseFolder(self), FSQLParameters );
    except
      on e : Exception do begin
        if IsStringEmpty( Path ) then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                  Format( '%s; %s', [ Table, e.Message ] ),
                  0
                )
        else
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                  Format( '%s; %s', [ GetSafeSQLPath( Path ), e.Message ] ),
                  0
                ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroDisconnect ;
  var
    i : Integer ;
  begin
    // guarantee connection closing
    for i := 0 to BUILTIN_CURSORS - 1 do
      oGisDb.sqlQueryClose(i) ;
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
    oGisDb.sqlDisconnect ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroUpdateStart ;
  begin
    lastUid := -1 ;
    oGisDb.sqlTransactGlobalUpdateStart ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroUpdateEnd ;
  begin
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
    oGisDb.sqlTransactGlobalUpdateCommit ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroMasterCreate ;
  var
    table, fields: String ;

  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_G_FEATURES_TABLE ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_GEOMETRY_PROPERTIES_TABLE ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_ATTRIBUTE_PROPERTIES_TABLE ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_FIELD_LOOKUP_TABLE ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_G_COORD_SYSTEM_TABLE ) ) ;
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_ALTER_G_COORD_SYSTEM_TABLE  ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_G_ALIAS_TABLE_TABLE ) ) ;

        table  := 'GAliasTable'          ;
        fields := 'TableType, TableName' ;

        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                   [ table, fields,
                     '''INGRFeatures'',''GFeatures''',
                     ''
                   ]
                 )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                   [ table, fields,
                     '''INGRFieldLookup'',''FieldLookup''',
                     ''
                   ]
                 )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                  [ table, fields,
                    '''INGRAttributeProperties'',''AttributeProperties''',
                    ''
                  ]
                 )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                   [ table, fields,
                     '''INGRGeometryProperties'',''GeometryProperties''',
                     ''
                   ]
                 )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                   [ table, fields,
                     '''GCoordSystemTable'',''GCoordSystem''',
                     ''
                   ]
                 )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                   [ table, fields,
                     '''GModifications'',''ModificationLog''',
                     ''
                   ]
                 )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                   [ table, fields,
                     '''GModifiedTables'',''ModifiedTables''',
                     ''
                   ]
                 )
               ) ;
        // Only for MS SQL Server Warehouse
        if oGisDb.IsMsSql then begin
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''GParameters'',''GParameters''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''GADOFieldMapping'',''GFieldMapping''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''INGRDictionaryProperties'',''GDictionaryProperties''',
                       ''
                     ]
                   )
                 ) ;
        end // Only for MS Jet (MS Access) Warehouse
        else if oGisDb.IsMsJet then begin
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''INGRSQLOperatorsTable'',''GSQLOperatorTable''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''INGRDictionaryProperties'',''GDictionaryProperties''',
                       ''
                     ]
                   )
                 ) ;
        end ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_MODIFICATION_LOG_TABLE ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_MODIFIED_TABLES_TABLE ) ) ;
      except
        // can exist
      end ;

      if oGisDb.IsMsSql then begin

        // Only for MS SQL Server Warehouse
        try
          oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_G_PARAMETERS_TABLE ) ) ;

          table  := 'GParameters'            ;
          fields := 'GPARAMETER, GVALUE' ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''ADOTypeForBinaryStorage'',''205''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''ADOTypeForGeometryStorage'',''205''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''ADOTypeForDateStorage'',''135''',
                       ''
                     ]
                   )
                 ) ;
        except
          // can exist
        end ;

        try
          oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_G_FIELD_MAPPING_TABLE ) ) ;
        except
          // can exist
        end ;

        try
          oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_GADOINDEXCOLUMNS_TABLE ) ) ;
          oGisDb.sqlExec( getCmd( T_SQLGM.ID_ALTER_GADOINDEXCOLUMNS_TABLE ) ) ;
        except
          // can exist
        end ;

      end
      else if oGisDb.IsMsJet then begin

        // Only for MS Jet (MS Access) Warehouse
        try
          oGisDb.sqlExec( getCmd( T_SQLGM.ID_CREATE_G_SQLOPERATORTABLE_TABLE ) ) ;

          table  := 'GSQLOperatorTable'                          ;
          fields := '[Operator], [OperatorClass], [Description]' ;

          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''%'',''gdbPattern'',''Zero or more characters may '   +
                       'fill this position.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''*'',''gdbArithmetic'',''This is the multiplication ' +
                       'sign.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''+'',''gdbArithmetic'',''This is the plus sign.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''-'',''gdbArithmetic'',''This is the minus sign.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''/'',''gdbArithmetic'',''This is the division sign.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''_'',''gdbPattern'',''Single character may fill '     +
                       'this position.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''||'',''gdbArithmetic'',''This is the concatenation ' +
                       'operator.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''AVG'',''gdbStatistics'',''Finds the average of a '   +
                       'column''''s values.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''BETWEEN'',''gdbSelection'',''Allows specification '  +
                       'of a range of values associated with a column''''s value.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''COUNT'',''gdbStatistics'',''Finds the number of '    +
                       'rows in the query.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''FALSE'',''gdbConstants'',''Indicator for negative '  +
                       'boolean values.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''IN'',''gdbSelection'',''Preceeds a list of values '  +
                       'from which the column''''s value shall be included.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''IS'',''gdbSelection'',''Preceeds a constant value '  +
                       'for comparison to a column''''s value.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''LIKE'',''gdbSelection'',''Used in conjunction with ' +
                       'wildcard characters to look for patterns.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''MAX'', ''gdbStatistics'',''Finds the maximum value ' +
                       'within a column.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''MIN'',''gdbStatistics'',''Finds the minimum value '  +
                       'within a column.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''NOT'',''gdbLogical'',''This is the logical '         +
                       'negation operator.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''NULL'',''gdbConstants'',''Indicator for NULL '       +
                       '(undefined) values.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''SUM'',''gdbStatistics'',''Finds the sum of a '       +
                       'column''''s testing common values.''',
                       ''
                     ]
                   )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                     [ table, fields,
                       '''TRUE'',''gdbConstants'',''Indicator for positive '   +
                       'boolean values.''',
                       ''
                     ]
                   )
                 ) ;
        except
          // can exist
        end ;

      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroMasterUpdate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _name   : String ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    // only fo save inheritance
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroTableCreate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType
  ) ;
  var
    geometry_type    : Integer ;
    data_type        : Integer ;
    geometry_name    : String  ;
    geometry_sk_name : String  ;
    uid_name         : String  ;
    coordsys_guid    : String  ;
    {$IFDEF MSWINDOWS}
      {$IFNDEF OXYGENE}
        dbenJet     : Variant ; // DBEngine
        wrkJet      : Variant ; // Workspace
        dbAccess    : Variant ; // Database
        fld         : Variant ; // Field
      {$ELSE}
        dbenJet     : _DBEngine  ;
        wrkJet      : Workspace  ;
        dbAccess    : Database   ;
        fld         : Field      ;
      {$ENDIF}
    {$ENDIF}
  begin
    if IsReadOnly then exit ;

    uid_name         := getCmd( T_SQLGM.ID_UID ) ;
    geometry_name    := getCmd( T_SQLGM.ID_GEOMETRY ) ;
    geometry_sk_name := getCmd( T_SQLGM.ID_GEOMETRY_SK ) ;
    coordsys_guid    := '{090EBEC3-B278-4920-ACEE-1DAC2A55DCAF}' ;

    nameGeometryColumn := uid_name      ;
    nameGeometryBlob   := geometry_name ;
    nameGeometryTable  := Table     ;

    case Integer(_type) of
      Integer(TGIS_ShapeType.Point):
        begin
          geometry_type := 10 ;
          data_type     := 32 ;
        end ;
      Integer(TGIS_ShapeType.MultiPoint) :
        begin
          geometry_type :=  3 ;
          data_type     := 32 ;
        end ;
      Integer(TGIS_ShapeType.Arc) :
        begin
          geometry_type :=  1 ;
          data_type     := 32 ;
        end ;
      Integer(TGIS_ShapeType.Polygon) :
        begin
          geometry_type :=  2 ;
          data_type     := 32 ;
        end ;
    else
      begin
        geometry_type :=  2 ;
        data_type     := 32 ;
      end ;
    end ;

    oGisDb.sqlTransactRestructStart ;
    try

      if oGisDb.IsMsSql then begin
        // GeoMedia MS SQL Warehouse

        // Creating Feature and Geometry table
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_TABLE_GEOMETRY_MSSQL ),
                         [ Table, Table ]
                       )
               ) ;

        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_INDEX_XLO ),
                         [ Table, Table ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_INDEX_XHI ),
                         [ Table, Table ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_INDEX_YLO ),
                         [ Table, Table ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_INDEX_YHI ),
                         [ Table, Table ]
                       )
               ) ;

        try
          // GDO_ID column
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE ),
                           [ Table, uid_name ]
                         )
                 ) ;

          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                           [ 1, 'General Number', 4, 1, 0, Table, uid_name ]
                         )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_G_FIELD_MAPPING_TABLE ),
                           [ Table, uid_name, 'NULL', 'NULL', 'NULL', '1' ]
                         )
                 ) ;

          // GEOMETRY column
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE ),
                           [ Table, geometry_name ]
                         )
                 ) ;

          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_GEOMETRY_PROPERTIES_TABLE ),
                           [ 1, geometry_type, coordsys_guid,
                             Table, geometry_name ]
                         )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_G_FEATURES_TABLE ),
                           [ Table, geometry_type, geometry_name ]
                         )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_G_FIELD_MAPPING_TABLE ),
                           [ Table, geometry_name, IntToStr(data_type),
                             IntToStr(geometry_type), ''''+coordsys_guid+'''',
                             'NULL' ]
                         )
                 ) ;
        except
          macroMasterUpdate( _extent, _type, '', DefaultDimension ) ;
        end ;

      end
      else if oGisDb.IsMsJet then begin
        try
          // GeoMedia Access Warehouse

          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_TABLE_GEOMETRY_MSJET ),
                           [ Table, Table ]
                         )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_CREATE_INDEX_GEOMETRY_SK ),
                           [ Table, Table ]
                         )
                 ) ;
          {$IFDEF MSWINDOWS}
          // Setting several properties
          try
            {$IFDEF CLR}
              dbenJet  := DAO.DBEngineClass.Create ;
              wrkJet   := dbenJet.CreateWorkspace('', 'Admin', '', DAO.WorkspaceTypeEnum.dbUseJet) ;
            {$ELSE}
              dbenJet  := CoDBEngine.Create ;
              wrkJet   := dbenJet.CreateWorkspace('', 'Admin', '', dbUseJet) ;
            {$ENDIF}
            dbAccess := wrkJet.OpenDatabase( dataSource,
                                             {$IFDEF OXYGENE}
                                               DriverPromptEnum.dbDriverComplete
                                             {$ELSE}
                                               dbDriverComplete
                                             {$ENDIF},
                                             False,
                                             ';UID=Admin;PWD=;'

                                           ) ;
            fld := dbAccess.TableDefs[ Table ].Fields[ geometry_name ] ;
            fld.Properties.Append( fld.CreateProperty( 'CoordSystemIndex',
                                                       {$IFDEF OXYGENE}
                                                         DataTypeEnum.dbText
                                                       {$ELSE}
                                                         dbText
                                                       {$ENDIF},
                                                       coordsys_guid,
                                                       False
                                                     )
                                 ) ;
            fld.Properties.Append( fld.CreateProperty( 'ExtendedType',
                                                       {$IFDEF OXYGENE}
                                                         DataTypeEnum.dbInteger
                                                       {$ELSE}
                                                         dbInteger
                                                       {$ENDIF},
                                                       data_type,
                                                       False
                                                     )
                                 ) ;
            fld.Properties.Append( fld.CreateProperty( 'SpatialKeyFieldName',
                                                       {$IFDEF OXYGENE}
                                                         DataTypeEnum.dbText
                                                       {$ELSE}
                                                         dbText
                                                       {$ENDIF},
                                                       geometry_sk_name,
                                                       False
                                                     )
                                 ) ;
            fld.Properties.Append( fld.CreateProperty( 'SubType',
                                                       {$IFDEF OXYGENE}
                                                         DataTypeEnum.dbInteger
                                                       {$ELSE}
                                                         dbInteger
                                                       {$ENDIF},
                                                       geometry_type,
                                                       False
                                                     )
                                  ) ;

            fld := dbAccess.TableDefs[ Table ].Fields[ geometry_sk_name ] ;
            fld.Properties.Append( fld.CreateProperty( 'SpatialKeyVersion',
                                                       {$IFDEF OXYGENE}
                                                         DataTypeEnum.dbText
                                                       {$ELSE}
                                                         dbText
                                                       {$ENDIF},
                                                       '4.0',
                                                       False
                                                     )
                                 ) ;
            dbAccess.Close ;
            wrkJet.Close ;
            {$IFNDEF CLR}
            dbenJet := Unassigned ;
            {$ELSE}
            dbenJet := nil ;
            {$ENDIF}
          except
          end ;
          {$ENDIF}

          // GDO_ID column
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE ),
                           [ Table, uid_name ]
                         )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                           [ -1, 'General Number', 4, -1, 0, Table, uid_name ]
                         )
                 ) ;

          // GEOMETRY column
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE ),
                           [ Table, geometry_name ]
                         )
                 ) ;

          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_GEOMETRY_PROPERTIES_TABLE ),
                           [ -1, geometry_type, coordsys_guid,
                             Table, geometry_name ]
                         )
                 ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_G_FEATURES_TABLE ),
                           [ Table, geometry_type, geometry_name ]
                         )
                 ) ;

          // GEOMETRY_SK column
          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE ),
                           [ Table, geometry_sk_name ]
                         )
                 ) ;

          oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                           [ 0, '', 10, 0, 0, Table, geometry_sk_name ]
                         )
                 ) ;
        except
          macroMasterUpdate( _extent, _type, '', DefaultDimension ) ;
        end ;
      end ;

    finally
      oGisDb.sqlTransactRestructCommit
    end ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_CSGUID ),
                          [ coordsys_guid ]
                        ), 0
                ) ;
    try
      if oGisDb.sqlQueryEof(0) then begin
        oGisDb.sqlQueryClose(0) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_G_COORD_SYSTEM_TABLE ),
                         [ coordsys_guid ]
                       )
               ) ;
      end ;
    except
      oGisDb.sqlQueryClose(0) ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroTableAlter(
    const _layer : TGIS_LayerVector
  ) ;
  const
    N_ITER   = 255 ; // maximum number of tries in new name finding
  var
    i         : Integer ;
    cnt       : Integer ;
    fld       : TGIS_FieldInfo ;
    fname     : String  ;
    lname     : Integer ;
    ltext     : Integer ;
    name_len  : Integer ;
    lftn      : String  ;

    procedure delete_metadata( const _field: String ) ;
    begin
      oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FIELD_FROM_FIELD_LOOKUP ),
                              [ Table, _field ]
                            )
                    ) ;

      oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FIELD_FROM_ATTRIBUTE_PROPERTIES ),
                              [ Table, _field ]
                            )
                    ) ;
    end ;

    procedure insert_metadata( const _field: String;
                               const _field_type: TGIS_FieldType
                             ) ;
    begin
      // Attribute column
      oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_FIELD_LOOKUP_TABLE ),
                              [ Table, _field ]
                            )
                    ) ;

      // GeoMedia MS SQL Warehouse
      if oGisDb.IsMsSql then begin
        case _field_type of
          TGIS_FieldType.String :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                              [ 0, '', 10, 1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Date :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                              [ 0, 'Date', 8, 1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Boolean :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, '', 1, 1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Number :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, 'General Number', 4, 1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Float :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, 'General Number', 7, 1, 6, Table, _field ]
                           )
                   ) ;
        end ;
      end
      // GeoMedia MS Access Warehouse
      else if oGisDb.IsMsJet then begin
        case _field_type of
          TGIS_FieldType.String :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, '', 10, -1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Date :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, 'Date', 8, -1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Boolean :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, '', 1, -1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Number :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, 'General Number', 4, -1, 0, Table, _field ]
                           )
                   ) ;
          TGIS_FieldType.Float :
             oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_ATTRIBUTE_PROPERTIES_TABLE ),
                             [ 0, 'General Number', 7, -1, 6, Table, _field ]
                           )
                   ) ;
        end ;
      end ;
    end ;

  begin
    if IsReadOnly then exit ;

    oGisDb.sqlQueryClose( 0 ) ;
    oGisDb.sqlTableClose( 0 ) ;

    try
      lname := StrToInt( getCmd( T_SQLGM.ID_MAX_NAMELENGTH ) ) ;
    except
      lname := 16 ;
    end ;

    try
      if not IsStringEmpty( getCmd( T_SQLGM.ID_MAX_TEXTLENGTH ) ) then
        ltext := StrToInt( getCmd( T_SQLGM.ID_MAX_TEXTLENGTH ) )
      else
        ltext := GIS_SQL_MEMO_SIZE ;
    except
      ltext := GIS_SQL_MEMO_SIZE ;
    end ;

    _layer.PrepareExportFieldNames( lname, Self <> _layer ) ;

    if IsStringEmpty( nameGeometryTable ) then
      lftn := Table
    else
      lftn := nameGeometryTable ;

    oGisDb.sqlTransactRestructStart ;
    try

      for i:=0 to _layer.Fields.Count - 1 do begin
        fld := _layer.FieldInfo( i ) ;

        fname    := fld.ExportName ;
        name_len := Min( length( fname ) + 2 , lname ) ;

        if fld.Temporary then continue ;

        if      fld.Deleted then // delete field
                begin
                   try
                     oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_DROP_COLUMN ),
                                             [ lftn, fname ]
                                           )
                                   ) ;
                   except
                     // name can not exist
                   end ;
                   // Delete metadata from GeoMedia werehouse tables
                   delete_metadata( fname ) ;
                end
        else if ( not fld.Saved ) or ( _layer <> Self ) then // create a field
        begin
          for cnt := 0 to N_ITER do begin
            try
              case fld.FieldType of
                TGIS_FieldType.String :
                   begin
                      if fld.NewWidth <= ltext then
                         oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_STRING ),
                                          [ lftn, fname, fld.NewWidth ]
                                        )
                                )
                      else
                         oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_MEMO ),
                                          [ lftn, fname ]
                                        )
                                ) ;
                     // Insert metadata to GeoMedia werehouse tables
                     insert_metadata( fname, fld.FieldType ) ;
                   end ;
                TGIS_FieldType.Date :
                   begin
                     oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_DATE ),
                                             [ lftn, fname ]
                                           )
                                   ) ;
                     // Insert metadata to GeoMedia werehouse tables
                     insert_metadata( fname, fld.FieldType ) ;
                   end ;
                TGIS_FieldType.Boolean :
                   begin
                     oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_BOOLEAN ),
                                             [ lftn, fname ]
                                           )
                                   ) ;
                     // Insert metadata to GeoMedia werehouse tables
                     insert_metadata( fname, fld.FieldType ) ;
                   end ;
                TGIS_FieldType.Number :
                   begin
                     if fld.NewDecimal = 0 then begin
                       if fld.Width <= 6 then
                         oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_INTEGER ),
                                                 [ lftn, fname ]
                                               )
                                       )
                       else
                         oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_BIGINT ),
                                                 [ lftn, fname ]
                                               )
                                       ) ;
                       // Insert metadata to GeoMedia werehouse tables
                       insert_metadata( fname, TGIS_FieldType.Number ) ;
                     end
                     else begin
                       oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_FLOAT ),
                                               [ lftn, fname ]
                                             )
                                     ) ;
                       // Insert metadata to GeoMedia werehouse tables
                       insert_metadata( fname, TGIS_FieldType.Float ) ;
                     end ;
                   end ;
                TGIS_FieldType.Float :
                   begin
                     oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ADD_FLOAT ),
                                             [ lftn, fname ]
                                           )
                                   ) ;
                     // Insert metadata to GeoMedia werehouse tables
                     insert_metadata( fname, fld.FieldType ) ;
                   end
                else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ),
                                                  '',
                                                  0
                                                ) ;
              end ;

              break ;

            except
              on e : Exception do begin
                fname := Format( '%s%.2x',
                                 [ Copy( fname, StringFirst, name_len -2 ), cnt ]
                               ) ;
                if cnt = N_ITER then raise ;
              end ;
            end ;
          end ;

          fld.Saved      := True  ;
          fld.ExportName := fname ;
          fld.Name       := fld.ExportName ;
          fld.Width      := fld.NewWidth   ;
          fld.Decimal    := fld.NewDecimal ;
        end
        else    begin
                  if fld.NewName <> fld.Name then begin // rename a files
                    // column renaming is not implemented on most databases
                    fld.NewName    := fld.Name ;
                    fld.ExportName := fld.Name ;
                  end ;
                  if ( fld.FieldType = TGIS_FieldType.String ) and
                     ( fld.Width    <> GIS_SQL_MEMO_SIZE  ) and  // ignore MEMO
                     ( fld.NewWidth <> fld.Width          ) then // change width
                  begin
                    try
                      oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_ALTER_ALTER_STRING ),
                                       [ lftn, fname, fld.NewWidth ]
                                     )
                             ) ;
                    except
                      // could fail on a number of databases
                      fld.NewWidth := fld.Width ;
                    end ;
                  end ;
                end ;
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroTableDrop ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DROP_TABLE ), [ Table ] ) ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FEATURE_FROM_GEOMETRY_PROPERTIES ),
                                [ Table ]
                              )
                      ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FEATURE_FROM_ATTRIBUTE_PROPERTIES ),
                                [ Table ]
                              )
                      ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FEATURE_FROM_FIELD_LOOKUP ),
                                [ Table ]
                              )
                      ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FEATURE_FROM_G_FEATURES ),
                                [ Table ]
                              )
                      ) ;
      except
        // can no exist
      end ;

    finally
       oGisDb.sqlTransactRestructCommit ;
    end ;

    // GeoMedia MS SQL Warehouse
    if oGisDb.IsMsSql then begin
      try
        oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_FIELD_FROM_G_FIELD_MAPPING ),
                                [ Table ]
                              )
                      ) ;
      except
        // can no exist
      end ;
    end ;

  end ;

  procedure TGIS_LayerSqlGmAbstract.macroTableSetField(
     const _id   : Integer ;
     const _name : String ;
     const _val  : Variant
   ) ;
  var
    i : Integer ;
  begin
    if IsReadOnly then exit ;

    i := FindField( _name ) ;
    try
      if ( i >= 0 ) and ( not FieldInfo( i ).FileFormat ) then // field from layer
        oGisDb.sqlTableSetField( _id, FieldInfo(i).ExportName, _val,
                                 FieldInfo(i).NewWidth )
      else // internal field
        oGisDb.sqlTableSetField( _id, _name, _val, 1 ) ;
      except
        // maybe field name was changed by someone else.....
        assert( False, 'Field ''' + _name + ''' does not exists.' ) ;
      end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, True, 0 ) ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer        ;
     const _saved   : Boolean
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, _saved, 0 ) ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer        ;
     const _saved   : Boolean        ;
     const _binary  : Integer
  ) ;
  begin
    AddFieldInternal( _name, _type, _width, _decimal, _saved, _binary );
  end;

  procedure TGIS_LayerSqlGmAbstract.macroQueryStructure ;
  var
    i   : Integer        ;
    k   : Integer        ;
    lst : TStringList    ;
    fld : TGIS_FieldInfo ;
  begin
    Fields.Clear ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_TABLE_WHERE ),
                                 [ nameGeometryTable,
                                   '1=0'
                                 ]
                               ), 0
                       ) ;

    {$IFNDEF OXYGENE}
      oGisDb.sqlQueryStructure( nameGeometryTable, nameGeometryColumn,  macroAddField ) ;
    {$ELSE}
      oGisDb.sqlQueryStructure( nameGeometryTable, nameGeometryColumn, @macroAddField ) ;
    {$ENDIF}

    // hide built in fields
    lst := TStringList.Create ;
    try
      if oGisDb.IsMsSql then
        lst.Text := getCmd( T_SQLGM.ID_FIELDS_MSSQL ) ;
      if oGisDb.IsMsJet then
        lst.Text := getCmd( T_SQLGM.ID_FIELDS_MSJET ) ;
      lst.Sorted := True ;
      for i:= 0 to Fields.Count -1 do begin
        fld := FieldInfo( i ) ;

        if lst.Find( fld.NewName, k ) then begin
          fld.FileFormat := True ;
          fld.Hidden     := True ;
          fld.ReadOnly   := True ;
          if fld.NewName = getCmdGEOUID(0) then
            fld.IsUID    := True ;
        end ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    oGisDb.sqlQueryClose( 0 ) ;

    if hasText then begin
      if FindField( T_SQLGM.GM_LABEL )        < 0 then
        AddFieldInternal( T_SQLGM.GM_LABEL, TGIS_FieldType.String,  1, 0, False, 0 ) ;
      if FindField( T_SQLGM.GM_LABEL_ANGLE )  < 0 then
        AddFieldInternal( T_SQLGM.GM_LABEL_ANGLE, TGIS_FieldType.Number, 18, 6, False, 0 ) ;
    end ;

    ReadFieldRules ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroShapeDelete(
    const _uid  : TGIS_Uid
  ) ;
  begin
    if IsReadOnly then exit ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_DELETE_SHAPE ),
                              [ Table,
                                nameGeometryColumn,
                                _uid
                              ]
                            )
                    ) ;
    except
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.macroShapeUpdate(
    const _shp    : TGIS_Shape ;
    const _import : Boolean
  ) ;
  var
    i         : Integer ;
    fldinfo   : TGIS_FieldInfo ;
    fld       : Variant ;
    uid       : TGIS_Uid ;
    txt       : String  ;
    iloop     : Integer ;
    guaranted : Boolean ;
    skipField : Boolean ;
    skipShape : Boolean ;
  begin
    if IsReadOnly then exit ;

    guaranted := False ;

    oGisDb.sqlTransactUpdateStart ;
    try
      iloop := T_SQLGM.GM_RETRY_COUNT ;
      while iloop > 0 do begin
        try
          skipField := False ;
          skipShape := False ;
          // update geometry
          if _import then begin
            uid := lastUid ;
            if uid < 0 then uid := macroUidNew( guaranted )
                       else inc( uid ) ;
            if not guaranted then
              lastUid := uid ;

            if oGisDb.IsMsSql then begin
              try
                oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_IMPORT_IDENTITY ),
                                        [ Table, Table, uid, Table ]
                                      )
                              ) ;
              except
                oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                                        [ Table, getCmd( T_SQLGM.ID_GEOMETRY ), 'NULL','' ]
                                      )
                              ) ;

                oGisDb.sqlQueryOpen( getCmd( T_SQLGM.ID_SELECT_IDENTITY ), 0 ) ;
                if not oGisDb.sqlQueryEof( 0 ) then begin
                  uid := VarToInt32(
                           oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_UID ), 0 )
                         ) ;
                  oGisDb.sqlQueryClose( 0 ) ;
                  guaranted := True ;
                end ;
              end ;

              sqlTableOpenWrite( 0, Table, nameGeometryColumn, uid ) ;
            end
            else begin
              if iloop = T_SQLGM.GM_RETRY_COUNT then
                sqlTableAppend( 0, Table ) ;
              macroTableSetField( 0, nameGeometryColumn, uid ) ;
            end ;
          end
          else begin
            skipField := canSkipField ;
            skipShape := not _shp.GeometryChanged and not _shp.IsNewShape ;
            uid := _shp.Uid ;

            sqlTableOpenWrite( 0, Table, nameGeometryColumn, uid ) ;
          end ;

          if not skipShape then
            sqlTableSetGeometry( 0, nameGeometryBlob, _shp ) ;

          for i := 0 to Fields.Count - 1 do begin

            fldinfo := FieldInfo( i ) ;
            if not (TGIS_FieldFlags.Exportable in fldinfo.Flags) then continue ;

            // skip unchanged fields due to Access limitation (max. 99 columns)
            if skipField then begin
              if _shp.Layer.FieldInfo(i).NewName = fldinfo.NewName then begin
                if not _shp.IsFieldModifiedEx( i ) then continue ;
              end
              else if not _shp.IsFieldModified( fldinfo.NewName ) then continue ;
            end ;

            if fldinfo.IsUID then
              fld := _shp.GetFieldEx( nameGeometryColumn )
            else
              fld := _shp.GetFieldEx( fldinfo.NewName ) ;

            case fldinfo.FieldType of
              TGIS_FieldType.String :
                 begin
                   if VarIsNull( fld ) then
                     macroTableSetField( 0, fldinfo.NewName, fld )
                   else begin
                     txt := VarToString( fld ) ;
                     macroTableSetField( 0, fldinfo.NewName, txt ) ;
                   end ;
                 end ;
              TGIS_FieldType.Date :
                 begin
                   macroTableSetField( 0, fldinfo.NewName, fld ) ;
                 end ;
              TGIS_FieldType.Boolean :
                 begin
                   if VarToBoolean( fld ) = True then
                     macroTableSetField( 0, fldinfo.NewName, '1' )
                   else
                     macroTableSetField( 0, fldinfo.NewName, '0' ) ;
                 end ;
              TGIS_FieldType.Number :
                 begin
                   macroTableSetField( 0, fldinfo.NewName, fld ) ;
                 end ;
              TGIS_FieldType.Float :
                 begin
                   macroTableSetField( 0, fldinfo.NewName, fld ) ;
                 end ;
              else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
            end ;
          end ;

          if oGisDb.IsMsSql then begin
            macroTableSetField ( 0, getCmd( T_SQLGM.ID_XMIN ), _shp.Extent.XMin ) ;
            macroTableSetField ( 0, getCmd( T_SQLGM.ID_XMAX ), _shp.Extent.XMax ) ;
            macroTableSetField ( 0, getCmd( T_SQLGM.ID_YMIN ), _shp.Extent.YMin ) ;
            macroTableSetField ( 0, getCmd( T_SQLGM.ID_YMAX ), _shp.Extent.YMax ) ;
          end ;

          oGisDb.sqlTablePost( 0 ) ;
          iloop := 0 ;
        except
          if not guaranted then begin
            dec( iloop ) ;
            if iloop <= 0 then raise ;
            Sleep( GetRandom( T_SQLGM.GM_RETRY_INTERVAL ) ) ;
            lastUid := GetLastUid ;
          end
          else
            break ;
        end ;
      end ;
    finally
      oGisDb.sqlTransactUpdateCommit ;
    end ;

  end ;

  function TGIS_LayerSqlGmAbstract.macroUidLast : TGIS_Uid ;
  var
    v : Variant ;
  begin
    try
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_LAST_GEOMETRY_GDO_ID ),
                                   [ nameGeometryTable ]
                                 ), 0
                         ) ;
      try
        if oGisDb.sqlQueryEof( 0 ) then
          Result := 0
        else
          try
            v := oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_LASTUID ), 0 ) ;
            if VarIsNull( v ) then Result := 0
                              else Result := VarToInt64( v ) ;
          except
            Result := 0 ;
          end ;
      finally
        oGisDb.sqlQueryClose( 0 ) ;
      end ;
    except
      Result := 0 ;
    end ;
  end ;

  function TGIS_LayerSqlGmAbstract.macroUidReserve : TGIS_Uid ;
  var
    iloop     : Integer ;
    uid       : TGIS_Uid ;
    guaranted : Boolean ;
  begin
    Result := -1 ;

    if IsReadOnly then exit ;

    guaranted := False ;
    macroUpdateStart ;
    try
      uid := -1 ;

      iloop := T_SQLGM.GM_RETRY_COUNT ;
      while iloop > 0 do begin
        try
          if uid < 0 then uid := macroUidNew( guaranted )
                     else inc( uid ) ;

          if oGisDb.IsMsSql then begin
            oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_INSERT_EXT ),
                                    [ Table, getCmd( T_SQLGM.ID_GEOMETRY ), 'NULL','' ]
                                  )
                          ) ;

            oGisDb.sqlQueryOpen( getCmd( T_SQLGM.ID_SELECT_IDENTITY ), 0 ) ;
            if not oGisDb.sqlQueryEof( 0 ) then begin
              uid := VarToInt64(
                       oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_UID ), 0 )
                     ) ;
              oGisDb.sqlQueryClose( 0 ) ;
              guaranted := True ;
            end ;
          end
          else begin
            if iloop = T_SQLGM.GM_RETRY_COUNT then
              sqlTableAppend( 0, Table ) ;

            macroTableSetField( 0, nameGeometryColumn  , uid ) ;
            oGisDb.sqlTablePost( 0 ) ;
          end ;

          iloop := 0 ;
        except
          if not guaranted then begin
          dec( iloop ) ;
          if iloop <= 0 then raise ;
          Sleep( GetRandom( T_SQLGM.GM_RETRY_INTERVAL ) ) ;
          lastUid := GetLastUid ;
          uid := lastUid ;
          end
          else
            break ;
        end ;
      end ;
      Result := uid ;
    finally
      macroUpdateEnd ;
    end ;
  end ;

  function TGIS_LayerSqlGmAbstract.macroFetchRecord(
    const _uid     : TGIS_Uid ;
    const _cursor  : Integer
   ) : Boolean ;
  var
    fetch : Boolean ;  // is fetch necessary?
  begin
    fetch := True ;

    if not oGisDb.sqlQueryEof(_cursor) then
       if VarToInt32( sqlQueryGetGEOUID(_cursor) ) = _uid then
          fetch := False ;

    if fetch then begin
      oGisDb.sqlQueryClose(_cursor) ;
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_JOIN_UID ),
                                    [ nameGeometryTable,
                                      nameGeometryColumn,
                                      _uid
                                    ]
                                  ), _cursor
                          ) ;
    end ;

    Result := fetch ;
  end;

  function TGIS_LayerSqlGmAbstract.macroUidNew(
    var _guaranted : Boolean
  ) : TGIS_Uid ;
  begin
    _guaranted := False ;
    Result := macroUidLast + 1 ;
  end ;

  function TGIS_LayerSqlGmAbstract.getFieldInternal(
    const _uid    : TGIS_Uid  ;
    const _name   : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    lockThread ;
    try
      if (_uid < 0) then begin
        Result := Unassigned ;
        exit ;
      end ;

      macroFetchRecord( _uid, _cursor ) ;

      if not oGisDb.sqlQueryEof( _cursor ) then begin
        if hasText and ( ( _name = T_SQLGM.GM_LABEL ) or ( _name = T_SQLGM.GM_LABEL_ANGLE ) )
        then
          Result := Unassigned
        else
          try
            Result := oGisDb.sqlQueryGetField( _name, _cursor ) ;
          except
            Result := Unassigned ;
          end ;
      end
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.setUp ;
  var
    ext   : TGIS_Extent ;
    gType : Variant ;
  begin
    FUseRTree   := False ;
    //useIdentity := True ;

    inherited ;

    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    macroConnect ;

    try
      if IsStringEmpty( forcedGeometryColumn ) then
        oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE ),
                                     [ Table ]
                                   ), 0
                           )
      else
        oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE_F ),
                                     [ Table, forcedGeometryColumn ]
                                   ), 0
                           ) ;
    except
      isNewVersion := not isNewVersion ;

      FSQLDialectList.Text := GetSQLDialect( GIS_SQL_DIALECT_NAME_MSJET ) ;

      if isNewVersion then
        FSQLDialectList.Text := FSQLDialectList.Text + #13#10 +
                                T_SQLGM.GIS_SQL_DIALECT_GM_NEW
      else
        FSQLDialectList.Text := FSQLDialectList.Text + #13#10 +
                                T_SQLGM.GIS_SQL_DIALECT_GM_OLD  ;

      prepareCommandList ;

      if IsStringEmpty( forcedGeometryColumn ) then
        oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE ),
                                     [ Table ]
                                   ), 0
                           )
      else
        oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_GEOMETRY_PROPERTIES_WHERE_F ),
                                     [ Table, forcedGeometryColumn ]
                                   ), 0
                           ) ;
    end ;

    if oGisDb.sqlQueryEof( 0 ) then begin
      oGisDb.sqlQueryClose( 0 ) ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), Table, 0 ) ;
    end ;

    gType := 0 ;
    try
      gType := oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_GEOMETRY_TYPE ), 0 ) ;
      if VarIsNull( gType ) or VarIsEmpty( gType ) then
        gType := 0 ;

      hasText := VarToInt32(gType) = 5 ;

      case VarToInt32(gType) of
        10, 5, 33 : begin
                      DefaultShapeType     := TGIS_ShapeType.Point        ;
                      FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Point ) ;
                      DefaultDimension     := TGIS_DimensionType.XYZ      ;
                      FSupportedDimensions := GisGetEmptyDimensionType    ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY ) ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ ) ;
                    end ;
        3         : begin
                      DefaultShapeType     := TGIS_ShapeType.Unknown      ;
                      FSupportedShapes     := GisGetEmptyShapeType        ;
                      FSupportedShapes     := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Point      ) ;
                      FSupportedShapes     := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.MultiPoint ) ;
                      FSupportedShapes     := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Arc        ) ;
                      FSupportedShapes     := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Polygon    ) ;
                      DefaultDimension     := TGIS_DimensionType.XYZ      ;
                      FSupportedDimensions := GisGetEmptyDimensionType    ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY ) ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ ) ;
                    end ;
        1         : begin
                      DefaultShapeType     := TGIS_ShapeType.Arc          ;
                      FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Arc ) ;
                      DefaultDimension     := TGIS_DimensionType.XYZ      ;
                      FSupportedDimensions := GisGetEmptyDimensionType    ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY ) ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ ) ;
                    end ;
        2         : begin
                      DefaultShapeType     := TGIS_ShapeType.Polygon      ;
                      FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Polygon ) ;
                      DefaultDimension     := TGIS_DimensionType.XYZ      ;
                      FSupportedDimensions := GisGetEmptyDimensionType    ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY ) ;
                      FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ ) ;
                    end
      else
        DefaultShapeType := TGIS_ShapeType.Unknown ;
        FSupportedShapes := GisGetEmptyShapeType ;
        DefaultDimension := TGIS_DimensionType.XYZ ;
        FSupportedDimensions := GisGetEmptyDimensionType    ;
        FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY ) ;
        FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ ) ;
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDFEATURE ),
                                     '', VarToInt32( gType ) ) ;
      end ;
    except
        FSupportedShapes := GisGetEmptyShapeType ;
        FSupportedDimensions := GisGetEmptyDimensionType    ;
        FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY ) ;
        FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ ) ;
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDFEATURE ),
                                     '', VarToInt32( gType ) ) ;
    end ;

    try
      nameGeometryTable := VarToString(
                             oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_TABLE_NAME ),0 )
                           ) ;
    except
      nameGeometryTable := Table ;
    end ;

    oGisDb.sqlQueryClose( 0 ) ;

    // Index column name
    if oGisDb.IsMsJet then
      oGisDb.sqlQueryOpen( Format( prepareCommand( T_SQLGM.SELECT_GEOMETRY_GDO_ID_NAME ),
                                   [ Table, -1 ]
                                 ), 0
                         )
    else
      oGisDb.sqlQueryOpen( Format( prepareCommand( T_SQLGM.SELECT_GEOMETRY_GDO_ID_NAME ),
                                   [ Table, 1 ]
                                 ), 0
                         ) ;
    try
      if oGisDb.sqlQueryEof( 0 ) then
        nameGeometryColumn := prepareCommand( T_SQLGM.UID )
      else
        nameGeometryColumn := VarToString(
                                oGisDb.sqlQueryGetField(
                                  prepareCommand( T_SQLGM.COLUMN_NAME ), 0
                                )
                              ) ;
    finally
      oGisDb.sqlQueryClose( 0 ) ;
    end ;

    // Geometry column name
    if IsStringEmpty( forcedGeometryColumn ) then
      oGisDb.sqlQueryOpen( Format( prepareCommand( T_SQLGM.SELECT_GEOMETRY_BLOB_NAME ),
                                   [ Table ]
                                 ), 0
                         )
    else
      oGisDb.sqlQueryOpen( Format( prepareCommand( T_SQLGM.SELECT_GEOMETRY_BLOB_NAME_F ),
                                   [ Table, forcedGeometryColumn ]
                                 ), 0
                         ) ;

    try
      if oGisDb.sqlQueryEof( 0 ) then begin
        nameGeometryBlob := prepareCommand( T_SQLGM.GEOMETRY ) ;
        nameCoordSysGUID := '' ;
      end
      else begin
        nameGeometryBlob := VarToString(
                              oGisDb.sqlQueryGetField(
                                prepareCommand( T_SQLGM.COLUMN_NAME ), 0
                              )
                            ) ;
        nameCoordSysGUID := VarToString(
                              oGisDb.sqlQueryGetField( prepareCommand(
                                T_SQLGM.G_COORD_SYSTEM_GUID ), 0
                              )
                            ) ;
      end ;
    finally
      oGisDb.sqlQueryClose( 0 ) ;
    end ;

    updateDialectList( 'GDO_ID'  , nameGeometryColumn ) ;
    updateDialectList( 'GEOMETRY', nameGeometryBlob   ) ;

    prepareCommandList ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_LAST_GEOMETRY_GDO_ID ),
                                 [ nameGeometryTable ]
                               ), 0
                       ) ;
    try
      if oGisDb.sqlQueryEof( 0 ) then
        lastUid := 0
      else begin
        try
          lastUid := VarToInt32(
                       oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_LASTUID ), 0 )
                     ) ;
        except
        end ;
      end ;
    finally
      oGisDb.sqlQueryClose( 0 ) ;
    end ;

    if not oGisDb.IsMsJet then begin
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_GEOMETRY_EXTENT ),
                                   [ nameGeometryTable ]
                                 ), 0
                         ) ;
      try
        ext.XMin := VarToDouble(
                      oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_XMIN ),0 )
                    ) ;
        ext.XMax := VarToDouble(
                      oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_XMAX ),0 )
                    ) ;
        ext.YMin := VarToDouble(
                      oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_YMIN ),0 )
                    ) ;
        ext.YMax := VarToDouble(
                      oGisDb.sqlQueryGetField( getCmd( T_SQLGM.ID_YMAX ),0 )
                    ) ;
        if not GisIsSameExtent( ext, GisExtent(0, 0, 0, 0) ) then
          Extent := ext ;
      except
        Extent := GisNoWorld ;
      end ;

      oGisDb.sqlQueryClose( 0 ) ;
    end ;

    macroQueryStructure ;

    fixGEOUID   := -1 ;

    if      oGisDb.IsMsSql then
            FFileInfo := 'GeoMedia MS SQL Server Warehouse (TTKLS)'
    else if oGisDb.IsMsJet then
            FFileInfo := 'GeoMedia Access Warehouse (TTKLS)' ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
   ) ;
  var
    ll : TGIS_LayerSqlGmAbstract ;
  begin
    inherited ;

    if IsReadOnly then exit ;

    if not IsStringEmpty( _path ) then begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          ll := TGIS_LayerSqlGmAbstract(&Class.forName(Self.Class.Name).getConstructor().newInstance());
        {$ELSE}
          ll := TGIS_LayerSqlGmAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
        {$ENDIF}
      {$ELSE}
        ll := TGIS_LayerSqlGmAbstractClass( Self.ClassType ).Create ;
      {$ENDIF}
      try
        ll.Path := _path ;
        ll.CS := CS ;
        ll.PasswordEvent := PasswordEvent ;
        ll.oGisDb.SQLExecuteEvent := oGisDb.SQLExecuteEvent ;
        ll.macroConnect ;
        try
          try
            ll.macroMasterCreate ;
          except
            // master can already exist
          end ;
          try
            ll.macroTableDrop ;
          except
            // table can not exist
          end ;
          ll.macroTableCreate( _extent, _type )  ;
        finally
          ll.macroDisconnect ;
        end ;
      finally
        FreeObject( ll ) ;
      end ;
    end
    else begin
      macroConnect ;
      try
        try
          macroMasterCreate ;
        except
          // master can already exist
        end ;
        macroTableCreate( _extent, _type )  ;
      finally
        macroDisconnect ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    lname     : Integer        ;
    shp       : TGIS_Shape     ;
    shp_tmp   : TGIS_Shape     ;
    shp_type  : TGIS_ShapeType ;
    first     : Boolean        ;
    shape_no  : Cardinal       ;
    end_uid   : TGIS_Uid        ;
    abort     : Boolean        ;
    eloop     : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False  ;

    FSQLParameters.Text := Path ;

    Extent   := _TGIS_Extent( _layer.Extent ) ;
    shp_type := _type ;
    first    := True  ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      try
        macroConnect ;
        macroTableDrop ;
        macroDisconnect ;
      except
        // could be nonexisting database
      end ;

      Build( Path, GisExtent( 0, 0, 0, 0 ), _type, _layer.DefaultDimension ) ;

      macroConnect ;

      Fields.Clear ;
      ImportStructure( _layer ) ;

      try
        lname := StrToInt( getCmd( T_SQLGM.ID_MAX_NAMELENGTH ) ) ;
      except
        lname := 16 ;
      end ;

      PrepareExportFieldNames( lname ) ;
      macroTableAlter( Self ) ;
      ExportStructureToFLD ;

      macroUpdateStart ;
      macroBeginBatchMode ;
      try
        eloop := _layer.Loop( _extent, _scope, _shape, _de9im ).GetEnumerator() ;
        try
          while eloop.MoveNext do begin // iterate all shapes
            shp := eloop.GetCurrent ;
            shp_tmp := shp.PrepareExportShape(
                             CS, _extent, _truncated, True
                           ) ;
            if assigned( shp_tmp ) then
              try
                if ( not shp_tmp.IsDeleted ) and
                   ( ( shp_tmp.ShapeType = shp_type            ) or
                     ( _type             = TGIS_ShapeType.Unknown )
                   )
                then begin

                  // calculate extent
                     if first then begin
                       Extent   := _TGIS_Extent( shp_tmp.Extent ) ;
                       shp_type := shp_tmp.ShapeType ;
                       first    := False ;
                     end
                     else
                       Extent := GisMaxExtent( Extent, shp_tmp.Extent ) ;

                  // insert record
                    macroShapeUpdate( shp_tmp, True ) ;
                end ;
              finally
                if shp <> shp_tmp then FreeObject( shp_tmp ) ;
              end ;

            if shape_no mod 100 = 1 then begin
              abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
              if abort then break ;
            end ;
            inc( shape_no ) ;
          end ;
        finally
          FreeObject( eloop ) ;
        end;
      finally
        // update master table
           macroMasterUpdate( Extent, shp_type, '', DefaultDimension ) ;

        macroUpdateEnd ;
        macroEndBatchMode ;
        FIsModified := False ;

        // GeoMedia MS SQL Warehouse
        if oGisDb.IsMsSql then begin
          // Update statistics in Geometry table for improve speed
          try
            oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_UPDATE_STATISTICS_MSSQL ),
                                    [ Table ]
                                  )
                          ) ;
          except
          end ;
        end ;

      end ;
    finally
      try
        macroDisconnect ;

        Items.Clear ;
        Fields.Clear ;

        FIsModified := False ;
        FIsOpened   := False ;

        Open ;
      except
      end ;

      RaiseBusyRelease( _layer ) ;
    end ;

  end ;

  procedure TGIS_LayerSqlGmAbstract.MergeLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape       ;
    const _de9im     : String           ;
    const _truncated : Boolean;
    const _restrict  : Boolean
  ) ;
  var
    lname     : Integer        ;
    shp       : TGIS_Shape     ;
    shp_tmp   : TGIS_Shape     ;
    shp_type  : TGIS_ShapeType ;
    first     : Boolean        ;
    shape_no  : Cardinal       ;
    end_uid   : TGIS_Uid        ;
    abort     : Boolean        ;
    eloop     : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;
    abort   := False ;

    FSQLParameters.Text := Path ;

    macroConnect ;

    end_uid  := _layer.GetLastUid ;
    Extent   := self.Extent     ;
    if GisIsWholeWorld( Extent ) then
      first := True
    else
      first := False ;

    shape_no := 0 ;
    shp_type := _type ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      try
        lname := StrToInt( getCmd( T_SQLGM.ID_MAX_NAMELENGTH ) ) ;
      except
        lname := 16 ;
      end ;

      PrepareExportFieldNames( lname ) ;
      _layer.PrepareExportFieldNames( lname, Self <> _layer ) ;

      MergeStructure( _layer, _restrict, True );

      macroUpdateStart ;
      try
        eloop := _layer.Loop( _extent, _scope, _shape, _de9im ).GetEnumerator() ;
        try
          while eloop.MoveNext do begin // iterate all shapes
            shp := eloop.GetCurrent ;
            shp_tmp := shp.PrepareExportShape(
                             CS, _extent, _truncated, True
                           ) ;
            if assigned( shp_tmp ) then
              try
                if ( not shp_tmp.IsDeleted ) and
                   ( ( shp_tmp.ShapeType=shp_type   ) or
                     ( shp_type=TGIS_ShapeType.Unknown )
                   )
                then begin
                  // calculate extent
                   if first then begin
                     Extent := _TGIS_Extent( shp_tmp.Extent ) ;
                     first  := False ;
                   end
                   else
                     Extent := GisMaxExtent( Extent, shp_tmp.Extent ) ;
                  // insert record
                   macroShapeUpdate( shp_tmp, True ) ;
                end ;
              finally
                if shp <> shp_tmp then FreeObject( shp_tmp ) ;
              end ;

            if shape_no mod 100 = 1 then begin
              abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
              if abort then break ;
            end ;

            inc( shape_no ) ;
          end ;
        finally
          FreeObject( eloop ) ;
        end;
      finally
        // update master table
        macroMasterUpdate( Extent, shp_type, Table, DefaultDimension ) ;
        macroUpdateEnd ;

        // GeoMedia MS SQL Warehouse
        if oGisDb.IsMsSql then begin
          // Update statistics in Geometry table for improve speed
          try
            oGisDb.sqlExec( Format( getCmd( T_SQLGM.ID_UPDATE_STATISTICS_MSSQL ),
                                    [ Table ]
                                  )
                          ) ;
          except
          end ;
        end ;

        FIsModified := False ;
      end ;
    finally
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  function TGIS_LayerSqlGmAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    lname       : String  ;
    ltype       : Integer ;
    defShpType  : TGIS_ShapeType ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    try
      macroConnect ;
      try
        try
          try
            oGisDb.sqlQueryOpen( getCmd( T_SQLGM.ID_SELECT_G_FEATURES ), 0 ) ;
          except
            isNewVersion := not isNewVersion ;

            FSQLDialectList.Text := GetSQLDialect( GIS_SQL_DIALECT_NAME_MSJET ) ;

            if isNewVersion then
              FSQLDialectList.Text := FSQLDialectList.Text + #13#10 +
                                      T_SQLGM.GIS_SQL_DIALECT_GM_NEW
            else
              FSQLDialectList.Text := FSQLDialectList.Text + #13#10 +
                                      T_SQLGM.GIS_SQL_DIALECT_GM_OLD  ;

            prepareCommandList ;
            oGisDb.sqlQueryOpen( getCmd( T_SQLGM.ID_SELECT_G_FEATURES ), 0 ) ;
          end ;

          while not oGisDb.sqlQueryEof( 0 ) do begin
            lname := VarToString( oGisDb.sqlQueryGetFieldById( 0, 0 ) ) ;
            ltype := VarToInt32(  oGisDb.sqlQueryGetFieldById( 1, 0 ) ) ;

            case ltype of
              10, 5 : defShpType := TGIS_ShapeType.Point       ;
              3     : defShpType := TGIS_ShapeType.MultiPoint  ;
              1     : defShpType := TGIS_ShapeType.Arc         ;
              2     : defShpType := TGIS_ShapeType.Polygon     ;
            else      defShpType := TGIS_ShapeType.Point       ;
            end;

            Result.Add(
              TGIS_LayerInfo.Create( lname,
                                     TGIS_RegisteredLayerType.Vector,
                                     defShpType
                                    )
            ) ;
            oGisDb.sqlQueryMoveNext( 0 ) ;
          end;
        except
          // can not exist
        end;
      finally
        oGisDb.sqlQueryClose( 0 ) ;
        macroDisconnect ;
      end;
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end;
  end;

  procedure TGIS_LayerSqlGmAbstract.ReStructure ;
  begin
    macroQueryStructure ;
  end ;

  function TGIS_LayerSqlGmAbstract.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 1 ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited;
    oGisDb.sqlQueryClose( 0 ) ;
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
  end;

  function  TGIS_LayerSqlGmAbstract.cursorOpen : Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorSql)  then
        SetLength( cursorSql, Result + 1 ) ;

      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorSql[Result] ) then
          cursorSql[Result] := new T_cursorSql_LayerSqlGm ;
      {$ENDIF}

      cursorSql[Result].curInUse := True ;

      cursorSql[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorSql[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorSql[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorSql[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorSql[Result].disableJoinPrimary := False ;

      if Result > BUILTIN_CURSORS - 1 then
        oGisDb.cursorOpen( Result ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerSqlGmAbstract.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorSql[_cursor].curInUse := False ;
      FreeObject( cursorSql[_cursor].currPoint ) ;
      FreeObject( cursorSql[_cursor].currMultipoint ) ;
      FreeObject( cursorSql[_cursor].currArc ) ;
      FreeObject( cursorSql[_cursor].currPolygon ) ;

      // truncate cursorState at the tail;
      for i := length( cursorSql ) - 1 downto 0 do begin
        if not cursorSql[i].curInUse then begin
          SetLength( cursorSql, i ) ;
        end
        else
          break ;
      end ;

      if ( _cursor > 0 ) and assigned( oGisDb ) then
        oGisDb.cursorClose( _cursor ) ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerSqlGmAbstract.cursorFirst(
    const _cursor      : Integer     ;
    const _viewerCS    : Boolean     ;
    const _extent      : TGIS_Extent ;
    const _query       : String      ;
    const _shape       : TGIS_Shape  ;
    const _de9im       : String      ;
    const _skipDeleted : Boolean
  ) ;
  var
    sqlquery : String ;
    order    : String ;
    ex       : TGIS_Extent ;
  begin
    lockThread ;
    try
      iCodePage           := CodePage ;
      iJoinCodePage       := JoinCodePage ;
      oGisDb.iCodePage    := iCodePage ;

      cursorSql[_cursor].currShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      try
        inherited cursorFirstInternal(
                    _cursor, _viewerCS,
                    _extent, _query, _shape, _de9im, _skipDeleted
                  ) ;

        prepareCandidates( _cursor ) ;

        sqlquery := ExpandSQLMacros( cursorState[ _cursor ].curQuery,
                                     FSQLDialectList
                                   ) ;
        sqlquery := ReplaceSQLToken( sqlquery,
                                     GIS_FIELD_UID,
                                     getCmdGEOUID( 1 )
                                   ) ;

        cursorSql[_cursor].fullSearch := TestSQLTokens( sqlquery,
                                                        GIS_FIELDS_PREDEFINED
                                                      ) ;
        if Pos( GIS_FIELD_JOINPREFIX, sqlquery ) >= StringFirst then
          cursorSql[_cursor].fullSearch := True ;

        {$IFDEF OXYGENE}
          if not assigned( JoinPrimary ) then
            JoinPrimary := '' ;
        {$ENDIF}

        if ( cursorSql[_cursor].disableJoinPrimary )             or
           ( IsStringEmpty( JoinPrimary ) )                      or
           ( CompareText( JoinPrimary, GIS_FIELD_UID     ) = 0 ) or
           ( CompareText( JoinPrimary, getCmdGEOUID( 0 ) ) = 0 ) or
           ( CompareText( JoinPrimary, getCmdGEOUID( 1 ) ) = 0 )
        then
          order := getCmdGEOUID( 1 )
        else
          order := JoinPrimary ;

        if GisIsWholeWorld( cursorState[ _cursor ].curRawExtent ) or oGisDb.IsMsJet then
        begin
          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_JOIN_NOEXT ),
                                  [ nameGeometryTable,
                                    order
                                  ]
                                ), _cursor
                        )
          else
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_JOIN_NOEXT_EX ),
                                  [ nameGeometryTable,
                                    sqlquery,
                                    order
                                  ]
                                ), _cursor
                        ) ;
        end
        else begin

          ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                                 GisExtent( -1E37, -1E37, 1E37, 1E37 )
                               ) ;

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_JOIN ),
                                  [ nameGeometryTable,
                                    DotFloatToStr( ex.XMin ),
                                    DotFloatToStr( ex.XMax ),
                                    DotFloatToStr( ex.YMin ),
                                    DotFloatToStr( ex.YMax ),
                                    order
                                  ]
                                ), _cursor
                        )
          else
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGM.ID_SELECT_JOIN_EX ),
                                  [ nameGeometryTable,
                                    sqlquery,
                                    DotFloatToStr( ex.XMin ),
                                    DotFloatToStr( ex.XMax ),
                                    DotFloatToStr( ex.YMin ),
                                    DotFloatToStr( ex.YMax ),
                                    order
                                  ]
                                ), _cursor
                        ) ;
        end ;

        cursorSql[_cursor].isEof   := False ;
        cursorSql[_cursor].isFirst := True ;

        cursorNext( _cursor ) ;
      except
        cursorSql[_cursor].isEof := True ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.cursorNext(
    const _cursor : Integer
  ) ;
  var
    ihglass  : Integer ;
    bvisible : Boolean ;
  begin
    lockThread ;
    try
      try
        ihglass := 0 ;
        while not cursorEof( _cursor ) do begin
          inc( ihglass ) ;
          if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
            if HourglassShake then begin
              cursorSql[_cursor].currShape := nil ;
              break ;
            end;
          end ;

          cursorSql[_cursor].currShape := nil ;

          if ( not oGisDb.sqlQueryEof(_cursor) ) and
             ( not cursorSql[_cursor].isFirst  )
          then
            oGisDb.sqlQueryMoveNext(_cursor ) ;

          cursorSql[_cursor].isFirst := False ;

          if not oGisDb.sqlQueryEof(_cursor ) then
            readShape( _cursor ) ;

          if cursorSql[_cursor].currShape = nil then begin
            cursorSql[_cursor].currShape := nextCandidate( _cursor ) ;

            if cursorSql[_cursor].currShape = nil then begin
              if oGisDb.sqlQueryEof(_cursor) then begin
                cursorSql[_cursor].isEof := True ;
                exit ;
              end
              else
                continue ;
            end ;
          end ;

          removeCandidate( _cursor, cursorSql[_cursor].currShape ) ;

          if cursorState[_cursor].curSkipDeleted and
             cursorSql[_cursor].currShape.IsDeleted then
            continue ;

          bvisible := isInScope( cursorSql[_cursor].currShape, _cursor ) ;

          if (( cursorSql[_cursor].currShape.IsEditable
               or cursorSql[_cursor].fullSearch
               or assigned( cursorState[_cursor].curRelShape )
               or ( assigned( Viewer )
                    and Viewer.Ref.InPaint
                    and ( cursorSql[_cursor].currShape.SmartSize <> 0 )
                  )
             )
             and ( not bvisible )
             ) or // filter scopeExtent for MSJET
             ( oGisDb.IsMsJet and ( IsStringEmpty( cursorState[ _cursor ].curQuery ) ) and
               not bvisible )
          then
            continue
          else
            exit ;
        end ;
      except
        cursorSql[_cursor].isEof := True ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlGmAbstract.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorSql[_cursor].isEof ;
  end ;

  function TGIS_LayerSqlGmAbstract.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorSql[_cursor].currShape ) then
       Result := cursorSql[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerSqlGmAbstract.GetShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    lockThread ;
    try
      // at first see is in edited list
      Result := inherited GetShape( _uid, _cursor ) ;
      if Result <> nil then exit ;

      // is it a current shape
      if ( cursorShape( _cursor )     <> nil ) and
         ( cursorShape( _cursor ).Uid = _uid ) then begin
        Result := cursorShape( _cursor ) ;
        exit ;
      end ;

      // check if it is subsequent call
      while not cursorEof( _cursor ) do begin
        cursorNext( _cursor ) ;
        if cursorShape( _cursor ) <> nil then begin
          if cursorShape( _cursor ).Uid = _uid then  begin
            Result := cursorShape( _cursor ) ;
            exit ;
          end
          else
          if cursorShape( _cursor ).Uid > _uid then
            break
        end
        else
          break ;
      end ;

      // then find in a database
      cursorSql[_cursor].disableJoinPrimary := True ;
      try
        cursorFirst( _cursor, False,
                      GisWholeWorld,
                      Format( getCmd( T_SQLGM.ID_FILTER_UID_WEAK ),
                              [ GIS_FIELD_UID,
                                _uid,
                                GIS_FIELD_UID,
                                _uid + T_SQLGM.GM_GETSHAPE_FETCHED_LIMIT
                              ]
                            ),
                      nil, '', True
                    ) ;
       finally
         cursorSql[_cursor].disableJoinPrimary := False ;
       end ;

       if ( cursorShape( _cursor )     <> nil ) and
          ( cursorShape( _cursor ).Uid = _uid ) then begin
         Result := cursorShape( _cursor ) ;
         exit ;
       end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlGmAbstract.GetLastUid : TGIS_Uid ;
  var
    tmp : TGIS_Uid ;
  begin
    lockThread ;
    try
      tmp    := macroUidLast ;
      Result := inherited GetLastUid ;
      if tmp > Result then Result := tmp ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlGmAbstract.GetNewUid : TGIS_Uid ;
  begin
    lockThread ;
    try
      Result := macroUidReserve ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.SaveData ;
  var
    i        : Integer    ;
    shp      : TGIS_Shape ;
    first    : Boolean    ;
    shape_no : Cardinal   ;
    end_uid  : Integer    ;
    abort    : Boolean    ;
  begin
    SaveFieldRules ;

    if IsReadOnly then exit ;

    iCodePage     := CodePage ;
    iJoinCodePage := JoinCodePage ;

    shape_no := 0 ;
    end_uid  := Items.Count ;
    abort    := False ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try

      // be sure that database is opened
         macroConnect ;

      // Alter table
         macroTableAlter( Self ) ;

      // update all items
         first := True ;

      ExportStructureToFLD ;

      macroUpdateStart ;
      try
        for i:=0 to Items.Count-1 do begin // iterate all edited shapes
          shp := TGIS_Shape( Items[i] ) ;

          // ignore not touched shapes
             if not shp.IsModified then
               continue ;

          // delete deleted items
             if shp.IsDeleted then begin
               macroShapeDelete( shp.Uid ) ;
               continue ;
             end ;

          // calculate extent
             if first then begin
                if GisIsNoWorld( Extent ) or GisIsWholeWorld( Extent )
                then
                   Extent := shp.Extent ;
                first  := False ;
             end ;

             Extent := GisMaxExtent( Extent, shp.Extent ) ;

          macroShapeUpdate( shp, False ) ;

          inc( shape_no ) ;
          if shape_no mod 100 = 1 then begin
            abort := RaiseBusyShake( Self, i + 1, end_uid ) ;
            if abort then break ;
          end ;
        end ;
      finally
        // update master table
           if not first then
             macroMasterUpdate( Extent, DefaultShapeType, Table, DefaultDimension ) ;

           Items.Clear ;

        macroUpdateEnd ;
        macroQueryStructure ;
        FIsModified := False ;

        for i := 0 to BUILTIN_CURSORS - 1 do begin
          prepareCandidates(i);
          cursorSql[i].currShape := nil ;
        end ;
      end ;

    finally
      RaiseBusyRelease( Self ) ;
    end ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.updateDialectList(
    const _name  : String ;
    const _value : String
  ) ;
  var
    idx : Integer     ;
  begin
    idx := FSQLDialectList.IndexOfName( _name ) ;
    if ( idx <> -1 ) then begin
      if ( _name <> _value ) then
        FSQLDialectList.Strings[ idx ] := _name+'='+_value
      else
        FSQLDialectList.Delete( idx )
    end
    else
      if ( _name <> _value ) then
        FSQLDialectList.Add( _name+'='+_value ) ;
  end ;

  procedure TGIS_LayerSqlGmAbstract.readShape(
    const _cursor : Integer
  ) ;
  var
    uid       : Integer    ;
    shapetype : TGIS_ShapeType ;
  begin
    uid       := VarToInt32( sqlQueryGetGEOUID( _cursor ) ) ;
    shapetype := TGIS_ShapeType.Unknown ;

    sqlQueryGetGeometry( uid, shapetype, _cursor ) ;

    if ( cursorSql[_cursor].currShape <> nil ) and hasText and
      not IgnoreShapeParams then begin
        cursorSql[_cursor].currShape.SetField(
          T_SQLGM.GM_LABEL,
          cursorSql[_cursor].currShape.Params.Labels.Value
        ) ;
        cursorSql[_cursor].currShape.SetField(
          T_SQLGM.GM_LABEL_ANGLE,
          cursorSql[_cursor].currShape.Params.Labels.Rotate
        ) ;
    end ;

    cursorSql[_cursor].currShape := getEdited( cursorSql[_cursor].currShape ) ;
  end ;

{==================================== END =====================================}
end.

