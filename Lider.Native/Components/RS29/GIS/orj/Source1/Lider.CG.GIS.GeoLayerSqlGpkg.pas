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
  Encapsulation of GeoPackage database access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlGpkg ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlGpkg"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Math,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Variants,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoFunctions,
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
    T_cursorSql_LayerSqlGpkg nested in TGIS_LayerSqlGpkgAbstract = public {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse : Boolean ;

        /// <summary>
        ///   Is first record?
        /// </summary>
        isFirst         : Boolean ;

        /// <summary>
        ///   Eof marker.
        /// </summary>
        isEof           : Boolean ;

        /// <summary>
        ///   If True, then attribute query should be done on user side.
        /// </summary>
        fullSearch      : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape       : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPoint       : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currArc         : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPolygon     : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultipoint  : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currComplex    : TGIS_ShapeComplex ;

        /// <summary>
        ///   If Ture the ordering by JoinPrimary code should be disbled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of abstract GeoPackage SQL layer.
  /// </summary>
  TGIS_LayerSqlGpkgAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                          class( TGIS_LayerVectorSqlAbstract )
    protected // other protected variables
      /// <summary>
      ///   internal cursor structure.
      /// </summary>
      {$IFDEF OXYGENE}
        cursorSql : array of T_cursorSql_LayerSqlGpkg ;
      {$ELSE}
        cursorSql : array of record

          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse : Boolean ;

          /// <summary>
          ///   Is first record?
          /// </summary>
          isFirst         : Boolean ;

          /// <summary>
          ///   Eof marker.
          /// </summary>
          isEof           : Boolean ;

          /// <summary>
          ///   If True, then attribute query should be done on user side.
          /// </summary>
          fullSearch      : Boolean ;

          /// <summary>
          ///   Current shape. Layer access is based on record by record access.
          /// </summary>
          currShape       : TGIS_Shape ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currPoint       : TGIS_ShapePoint ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currArc         : TGIS_ShapeArc ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currPolygon     : TGIS_ShapePolygon ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currMultipoint  : TGIS_ShapeMultiPoint  ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster then full Create constructor.
          /// </summary>
          currComplex    : TGIS_ShapeComplex ;

          /// <summary>
          ///   If True the ordering by JoinPrimary code should be disabled
          ///     upon MoveFirst.
          /// </summary>
          disableJoinPrimary : Boolean ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   Recent calculated value of last uid.
      /// </summary>
      lastUid         : TGIS_Uid ;

      /// <summary>
      ///   Recent calculated value of last new uid.
      /// </summary>
      lastNewUid      : TGIS_Uid ;

      /// <summary>
      ///   UID name fixup (GEO.UID or UID) for various db drivers.
      /// </summary>
      fixGEOUID       : Integer ;

      /// <summary>
      ///   Is rtree enabled.
      /// </summary>
      hasRtree        : Boolean ;

      /// <summary>
      ///   Can unchanged fields be skipped upon saving.
      /// </summary>
      canSkipField    : Boolean ;

      /// <summary>
      ///   Name of a layer.
      /// </summary>
      identifier  : String ;

      /// <summary>
      ///   Description of a layer.
      /// </summary>
      description : String ;

    protected // properties access routine

      function  fget_TableGeometry    : String ;
      function  fget_TableIndex       : String ;
      function  fget_ViewFeatures     : String ; override;

    protected
      /// <summary>
      ///   Read shape from a layer.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure readShape             ( const _cursor : Integer )             ;

      /// <inheritdoc/>
      procedure prepareCommandList    ; override;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <param name="_id">
      ///   if 0, then returned 'UID'; if 1, then returned GEO.UID
      /// </param>
      /// <returns>
      ///    column name
      /// </returns>
      function  getCmdGEOUID          ( const _id : Integer
                                      ) : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///    column name
      /// </returns>
      function  getCmdGEOMETRY        : String ;


      /// <summary>
      ///   Convert shape type to gpkg type.
      /// </summary>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <returns>
      ///    type text
      /// </returns>
      function shapeTypeToGpkgType    ( const _type   : TGIS_ShapeType
                                      ) : String ;

      /// <summary>
      ///   Convert gpkg type to shape type.
      /// </summary>
      /// <param name="_type">
      ///   gpkg type
      /// </param>
      /// <returns>
      ///    shape type
      /// </returns>
      function gpkgTypeToShapeType    ( const _type   : String
                                      ) : TGIS_ShapeType ;

      /// <summary>
      ///   Check if shape type is a subclass of another shape type.
      /// </summary>
      /// <param name="_eType">
      ///   shape type
      /// </param>
      /// <param name="_eSuperType">
      ///   shape type
      /// </param>
      /// <returns>
      ///    True if shape is subclass
      /// </returns>
      function isShapeTypeSubClassOf  ( const _eType      : TGIS_ShapeType ;
                                        const _eSuperType : TGIS_ShapeType
                                      ) : Boolean ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///    command
      /// </returns>
      function  prepareAppendCommand  ( const _table : String
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
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      /// <param name="_skipGeom">
      ///   skip updating shape geometry
      /// </param>
      /// <returns>
      ///    command
      /// </returns>
      function  prepareUpdateCommand  ( const _table  : String ;
                                        const _column : String ;
                                        const _shp    : TGIS_Shape ;
                                        const _skipGeom : Boolean
                                      ) : String ;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_uidname">
      ///   uid name
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
      procedure macroAddField         ( const _uidname : String         ;
                                        const _name    : String         ;
                                        const _type    : TGIS_FieldType ;
                                        const _width   : Integer        ;
                                        const _decimal : Integer
                                      ) ; overload;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_uidname">
      ///   uid name
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
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDEXIST
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    This method is used only for internal use on layer reading.
      ///    </note>
      /// </remarks>
      procedure macroAddField         ( const _uidname : String         ;
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
      ///   uid name
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
      procedure macroAddField         ( const _uidname : String         ;
                                        const _name    : String         ;
                                        const _type    : TGIS_FieldType ;
                                        const _width   : Integer        ;
                                        const _decimal : Integer        ;
                                        const _saved   : Boolean        ;
                                        const _binary  : Integer
                                      ) ; overload;
      /// <summary>
      ///   Append a record to table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
      procedure sqlTableAppend        ( const _id     : Integer ;
                                        const _table  : String
                                      ) ;

      /// <summary>
      ///   Update a record in table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      /// <param name="_uidCol">
      ///   uid column name
      /// </param>
      /// <param name="_uidVal">
      ///   uid value
      /// </param>
      /// <param name="_skipGeom">
      ///   skip updating shape geometry
      /// </param>
      procedure sqlTableOpenWrite     ( const _id     : Integer ;
                                        const _table  : String ;
                                        const _shp    : TGIS_Shape ;
                                        const _uidCol : String  ;
                                        const _uidVal : TGIS_Uid ;
                                        const _skipGeom   : Boolean
                                      ) ;
      /// <summary>
      ///   Update a record in table.
      /// </summary>
      /// <param name="_id">
      ///   table index
      /// </param>
      /// <param name="_name">
      ///   table name
      /// </param>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      procedure sqlTableSetGeometry   ( const _id     : Integer ;
                                        const _name   : String ;
                                        const _shp    : TGIS_Shape
                                      ) ;
      /// <summary>
      ///   Get uid column value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    column value
      /// </returns>
      function  sqlQueryGetGEOUID     ( const _cursor        : Integer
                                       ): Variant ;
      /// <summary>
      ///   Get uid column name.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    column name
      /// </returns>
      function  sqlQueryNameGEOUID    ( const _cursor     : Integer
                                         ): String ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_MinX        ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_MinY        ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_MaxX        ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_MaxY        ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_IsEmpty     ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_GeometryType( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_ST_SRID        ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;
      /// <summary>
      ///   Function callback mapped to database function.
      /// </summary>
      /// <param name="_nargs">
      ///   number of arguments
      /// </param>
      /// <param name="_args">
      ///   array of arguments
      /// </param>
      /// <returns>
      ///    function result
      /// </returns>
      function  sql_gpkg_IsAssignable   ( _nargs : Integer ;
                                          _args  : array of Variant
                                        ) : Variant ;

      /// <summary>
      ///   Serialize shape to gpkg structure.
      /// </summary>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      /// <returns>
      ///    serialized data buffer
      /// </returns>
      function  gpkgGeometryFromShp     ( const _shp    : TGIS_Shape
                                        ) : TBytes ;

      /// <inheritdoc/>
      procedure macroConnect          ; override;

      /// <inheritdoc/>
      procedure macroDisconnect       ; override;

      /// <inheritdoc/>
      procedure macroUpdateStart      ; override;

      /// <inheritdoc/>
      procedure macroUpdateEnd        ; override;

      /// <inheritdoc/>
      procedure macroBuildRtree       ; override;

      /// <summary>
      ///   Macro for creating master table
      /// </summary>
      procedure macroMasterCreate     ; virtual;

      /// <summary>
      ///   Macro for creating master log table
      /// </summary>
      procedure macroMasterMetadataCreate  ; virtual;

      /// <summary>
      ///   Macro for creating master style table.
      /// </summary>
      procedure macroSpatialIndexCreate  ; virtual;

      /// <inheritdoc/>
      procedure macroMasterUpdate     ( const _extent : TGIS_Extent    ;
                                        const _type   : TGIS_ShapeType ;
                                        const _name   : String         ;
                                        const _dim    : TGIS_DimensionType
                                      ) ; override;

      /// <summary>
      ///   Macro for creating table.
      /// </summary>
      /// <param name="_extent">
      ///   starting extent of layer
      /// </param>
      /// <param name="_type">
      ///   supported type of shape
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      procedure macroTableCreate      ( const _extent : TGIS_Extent    ;
                                        const _type   : TGIS_ShapeType ;
                                        const _dim    : TGIS_DimensionType
                                      ) ; virtual;

      /// <inheritdoc/>
      procedure macroTableAlter       ( const _layer  : TGIS_LayerVector
                                      ) ; override;

      /// <inheritdoc/>
      procedure macroTableDrop        ; override;

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
      procedure macroTableSetField    ( const _id     : Integer ;
                                        const _name   : String  ;
                                        const _val    : Variant
                                      ) ; virtual;

      /// <summary>
      ///   Macro for querying table for the structure (available fields)
      /// </summary>
      procedure macroQueryStructure   ; virtual;

      /// <summary>
      ///   Macro for deleting a shape from both: geometry and feature table.
      /// </summary>
      /// <param name="_uid">
      ///   shape uid
      /// </param>
      procedure macroShapeDelete      ( const _uid     : TGIS_Uid
                                      ) ; virtual;

      /// <inheritdoc/>
      procedure macroShapeUpdate      ( const _shp     : TGIS_Shape ;
                                        const _import  : Boolean
                                      ) ; override;

      /// <summary>
      ///   Macro for obtaining last assigned uid.
      /// </summary>
      /// <returns>
      ///    uid value
      /// </returns>
      function  macroUidLast          : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for reserving new uid.
      /// </summary>
      /// <returns>
      ///    uid value
      /// </returns>
      function  macroUidReserve       : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for generating new uid.
      /// </summary>
      /// <param name="_guaranted">
      ///   if True, the provided Uid is unique (for example generated by
      ///   stored procedure); otherwise it is treated only as a starting point
      ///   for further generation
      /// </param>
      /// <returns>
      ///    uid value
      /// </returns>
      function  macroUidNew           ( var   _guaranted : Boolean
                                      ) : TGIS_Uid ; virtual;

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
      ///    True if record was fetched to synchronize
      /// </returns>
      function  macroFetchRecord      ( const _uid     : TGIS_Uid ;
                                        const _cursor  : Integer
                                      ) : Boolean ; virtual;

      /// <inheritdoc/>
      function  macroMaxNameLength    : Integer ; override;

      /// <summary>
      ///   Attach functions callbacks to database functions.
      /// </summary>
      procedure macroAttachFunctions ;

      /// <summary>
      ///   Read layer metadata.
      /// </summary>
      procedure macroReadLayerMetadata ;

      /// <summary>
      ///   Update single parameter on SQLDialectList.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter; if not exist, then will be created
      /// </param>
      /// <param name="_value">
      ///   value of the parameter; if empty, then parameter will be
      /// </param>
      procedure updateDialectList     ( const _name  : String ;
                                        const _value : String
                                      ) ;


    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      function  getFieldInternal      ( const _uid       : TGIS_Uid          ;
                                        const _name      : String         ;
                                        const _cursor    : Integer
                                      ) : Variant ; override;

      /// <inheritdoc/>
      function  bindFieldInternal     ( const _name      : String         ;
                                        const _cursor    : Integer
                                      ) : Integer ; override;

      /// <inheritdoc/>
      function  getBindedFieldInternal( const _shape     : TObject        ;
                                        const _field     : Integer        ;
                                        const _cursor    : Integer
                                      ) : Variant ; override;
      /// <inheritdoc/>
      procedure setUp                 ; override;

      // cursor access function(s)

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

      /// <inheritdoc/>
      procedure doDestroy  ; override;

    public

      /// <inheritdoc/>
      constructor Create   ; override;

      /// <inheritdoc/>
      procedure Build      ( const _path   : String ;
                             const _extent : TGIS_Extent;
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

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;


      /// <inheritdoc/>
      function  GetShape    ( const _uid     : TGIS_Uid  ;
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

    public // properties

      /// <summary>
      ///   Name of the geometry table.
      /// </summary>
      property TableGeometry : String read fget_TableGeometry ;

      /// <summary>
      ///   Name of the rtree index table.
      /// </summary>
      property TableIndex    : String read fget_TableIndex ;

  end ;

  TGIS_LayerSqlGpkgAbstractClass = class of TGIS_LayerSqlGpkgAbstract ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoGeometryFactory,
    Lider.CG.GIS.GeoDb ;
{$ENDIF}

type

  T_GpkgHeader = record
    srs_id        : Integer ;
    envelope      : TGIS_Extent3D ;
    shpType       : TGIS_ShapeType ;
    flag_Empty    : Boolean ;
    flag_Extended : Boolean ;
    flag_Dims     : Integer ;
    header_size   : Integer ;

    function Read( const _buf : TBytes ) : Boolean ;
  end ;

  T_StandardGpkgBinary = record
    header    : T_GpkgHeader ;
    extension_code : TBytes ;
    geometry  : TBytes ;

    function  ReadGpkg    ( const _buf : TBytes     ) : Boolean ;
    function  ReadHeader  ( const _buf : TBytes     ) : Boolean ;
    function  ReadExtent  ( const _buf : TBytes     ) : Boolean ;
    function  GpkgToShape ( const _buf : TBytes     ) : TGIS_Shape ;
    function  ShapeToGpkg ( const _shp : TGIS_Shape ) : TBytes ;
  end ;

type
  T_SQLGPKG = class
  public
  const
  // Number of retries for concurrent issues.
  RETRY_COUNT             = 99 ;

  // Maximum time of interval between retries in concurrent mode.
  RETRY_INTERVAL          = 100  ;

  // Maximum number of items fetched in GetShape.
  GETSHAPE_FETCHED_LIMIT  = 500  ;

  ID_BEGIN = 0 ;

  PRAGMA_APPLICATION_ID =
    'PRAGMA application_id = 1196437808' ;
    ID_PRAGMA_APPLICATION_ID =
      ID_BEGIN + 1 ;

  CREATE_GPKG_SPATIAL_REF_SYS =
    'CREATE TABLE gpkg_spatial_ref_sys (' +
    '  srs_name TEXT NOT NULL,' +
    '  srs_id INTEGER NOT NULL PRIMARY KEY,' +
    '  organization TEXT NOT NULL,' +
    '  organization_coordsys_id INTEGER NOT NULL,' +
    '  definition  TEXT NOT NULL,' +
    '  description TEXT' +
    ') ;' ;
    ID_CREATE_GPKG_SPATIAL_REF_SYS =
      ID_PRAGMA_APPLICATION_ID + 1 ;

  INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN =
    'INSERT INTO gpkg_spatial_ref_sys ( ' +
    'srs_name, srs_id, organization, organization_coordsys_id, definition)' +
    ' VALUES (''Undefined Cartesian'', -1, ''NONE'', -1, ''Undefined'')' ;
    ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN =
      ID_CREATE_GPKG_SPATIAL_REF_SYS + 1 ;

  INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC =
    'INSERT INTO gpkg_spatial_ref_sys ( ' +
    'srs_name, srs_id, organization, organization_coordsys_id, definition) ' +
    ' VALUES (''Undefined Geographic'', 0, ''NONE'', 0, ''Undefined'')' ;
    ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC =
      ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN + 1 ;

  INSERT_GPKG_SPATIAL_REF_SYS_WGS84 =
    'INSERT INTO gpkg_spatial_ref_sys ( ' +
    'srs_name, srs_id, organization, organization_coordsys_id, definition) ' +
    ' VALUES (''WGS84'', 4326, ''epsg'', 4326,  ' +
    '''GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84", ' +
    '6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG", ' +
    '"6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT ' +
    '["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY ' +
    '["EPSG","4326"]]'')' ;
    ID_INSERT_GPKG_SPATIAL_REF_SYS_WGS84 =
      ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC + 1 ;

  CREATE_GPKG_CONTENTS =
    'CREATE TABLE gpkg_contents ( ' +
    '  table_name TEXT NOT NULL PRIMARY KEY, ' +
    '  data_type TEXT NOT NULL, ' +
    '  identifier TEXT UNIQUE, ' +
    '  description TEXT DEFAULT '''', ' +
    '  last_change DATETIME NOT NULL DEFAULT (strftime(''%Y-%m-%dT%H:%M:%fZ'',''now'')), ' +
    '  min_x DOUBLE, ' +
    '  min_y DOUBLE, ' +
    '  max_x DOUBLE, ' +
    '  max_y DOUBLE, ' +
    '  srs_id INTEGER, ' +
    '  CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id) ' +
    ') ;' ;
    ID_CREATE_GPKG_CONTENTS =
      ID_INSERT_GPKG_SPATIAL_REF_SYS_WGS84 + 1 ;

  CREATE_GPKG_GEOMETRY_COLUMNS =
    'CREATE TABLE gpkg_geometry_columns ( ' +
    '  table_name TEXT NOT NULL, ' +
    '  column_name TEXT NOT NULL, ' +
    '  geometry_type_name TEXT NOT NULL, ' +
    '  srs_id INTEGER NOT NULL, ' +
    '  z TINYINT NOT NULL, ' +
    '  m TINYINT NOT NULL, ' +
    '  CONSTRAINT pk_geom_cols PRIMARY KEY (table_name, column_name), ' +
    '  CONSTRAINT uk_gc_table_name UNIQUE (table_name), ' +
    '  CONSTRAINT fk_gc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents(table_name), ' +
    '  CONSTRAINT fk_gc_srs FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys (srs_id) ' +
    ') ;' ;
    ID_CREATE_GPKG_GEOMETRY_COLUMNS =
      ID_CREATE_GPKG_CONTENTS + 1 ;

  CREATE_GPKG_EXTENSIONS =
    'CREATE TABLE gpkg_extensions ( ' +
    '  table_name TEXT, ' +
    '  column_name TEXT, ' +
    '  extension_name TEXT NOT NULL, ' +
    '  definition TEXT NOT NULL, ' +
    '  scope TEXT NOT NULL, ' +
    '  CONSTRAINT ge_tce UNIQUE (table_name, column_name, extension_name) ' +
    ') ;' ;
    ID_CREATE_GPKG_EXTENSIONS =
      ID_CREATE_GPKG_GEOMETRY_COLUMNS + 1 ;

  CREATE_GPKG_DATA_COLUMNS =
    'CREATE TABLE gpkg_data_columns ( ' +
    '  table_name TEXT NOT NULL, ' +
    '  column_name TEXT NOT NULL, ' +
    '  name TEXT, ' +
    '  title TEXT, ' +
    '  description TEXT, ' +
    '  mime_type TEXT, ' +
    '  constraint_name TEXT, ' +
    '  CONSTRAINT pk_gdc PRIMARY KEY (table_name, column_name), ' +
    '  CONSTRAINT fk_gdc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents(table_name) ' +
    ') ;' ;
    ID_CREATE_GPKG_DATA_COLUMNS =
      ID_CREATE_GPKG_EXTENSIONS + 1 ;

  CREATE_GPKG_DATA_COLUMN_CONSTRAINTS =
    'CREATE TABLE gpkg_data_column_constraints ( ' +
    '  constraint_name TEXT NOT NULL, ' +
    '  constraint_type TEXT NOT NULL, ' +
    '  value TEXT, ' +
    '  min NUMERIC, ' +
    '  minIsInclusive BOOLEAN, ' +
    '  max NUMERIC, ' +
    '  maxIsInclusive BOOLEAN, ' +
    '  Description TEXT, ' +
    '  CONSTRAINT gdcc_ntv UNIQUE (constraint_name, constraint_type, value) ' +
    ') ;' ;
    ID_CREATE_GPKG_DATA_COLUMN_CONSTRAINTS =
      ID_CREATE_GPKG_DATA_COLUMNS + 1 ;

  CREATE_GPKG_METADATA =
    'CREATE TABLE gpkg_metadata ( ' +
    '  id INTEGER CONSTRAINT m_pk PRIMARY KEY ASC NOT NULL, ' +
    '  md_scope TEXT NOT NULL DEFAULT ''dataset'', ' +
    '  md_standard_uri TEXT NOT NULL, ' +
    '  mime_type TEXT NOT NULL DEFAULT ''text/xml'', ' +
    '  metadata TEXT NOT NULL ' +
    ') ;' ;
    ID_CREATE_GPKG_METADATA =
      ID_CREATE_GPKG_DATA_COLUMN_CONSTRAINTS + 1 ;

  CREATE_GPKG_METADATA_REFERENCE =
    'CREATE TABLE gpkg_metadata_reference ( ' +
    '  reference_scope TEXT NOT NULL, ' +
    '  table_name TEXT, ' +
    '  column_name TEXT, ' +
    '  row_id_value INTEGER, ' +
    '  timestamp DATETIME NOT NULL DEFAULT (strftime(''%Y-%m-%dT%H:%M:%fZ'',''now'')), ' +
    '  md_file_id INTEGER NOT NULL, ' +
    '  md_parent_id INTEGER, ' +
    '  CONSTRAINT crmr_mfi_fk FOREIGN KEY (md_file_id) REFERENCES gpkg_metadata(id), ' +
    '  CONSTRAINT crmr_mpi_fk FOREIGN KEY (md_parent_id) REFERENCES gpkg_metadata(id) ' +
    ') ;' ;
    ID_CREATE_GPKG_METADATA_REFERENCE =
      ID_CREATE_GPKG_METADATA + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT =
    'CREATE TRIGGER ''gpkg_metadata_md_scope_insert''  ' +
    'BEFORE INSERT ON ''gpkg_metadata''  ' +
    'FOR EACH ROW BEGIN  ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata violates ' +
    'constraint: md_scope must be one of undefined | fieldSession | ' +
    'collectionSession | series | dataset | featureType | feature | ' +
    'attributeType | attribute | tile | model | catalog | schema | ' +
    'taxonomy software | service | collectionHardware | ' +
    'nonGeographicDataset | dimensionGroup'')  ' +
    'WHERE NOT(NEW.md_scope IN  ' +
    '(''undefined'',''fieldSession'',''collectionSession'',''series'',''dataset'', ' +
    '''featureType'',''feature'',''attributeType'',''attribute'',''tile'',''model'', ' +
    '''catalog'',''schema'',''taxonomy'',''software'',''service'', ' +
    '''collectionHardware'',''nonGeographicDataset'',''dimensionGroup'')) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT =
      ID_CREATE_GPKG_METADATA_REFERENCE + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE =
    'CREATE TRIGGER ''gpkg_metadata_md_scope_update''  ' +
    'BEFORE UPDATE OF ''md_scope'' ON ''gpkg_metadata''  ' +
    'FOR EACH ROW BEGIN  ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata violates  ' +
    'constraint: md_scope must be one of undefined | fieldSession | ' +
    'collectionSession | series | dataset | featureType | feature | ' +
    'attributeType | attribute | tile | model | catalog | schema | ' +
    'taxonomy software | service | collectionHardware | ' +
    'nonGeographicDataset | dimensionGroup'')  ' +
    'WHERE NOT(NEW.md_scope IN  ' +
    '(''undefined'',''fieldSession'',''collectionSession'',''series'',''dataset'', ' +
    '''featureType'',''feature'',''attributeType'',''attribute'',''tile'',''model'', ' +
    '''catalog'',''schema'',''taxonomy'',''software'',''service'', ' +
    '''collectionHardware'',''nonGeographicDataset'',''dimensionGroup'')) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE =
      ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT =
    'CREATE TRIGGER ''gpkg_metadata_reference_reference_scope_insert''  ' +
    'BEFORE INSERT ON ''gpkg_metadata_reference''  ' +
    'FOR EACH ROW BEGIN  ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata_reference  ' +
    'violates constraint: reference_scope must be one of "geopackage", ' +
    'table", "column", "row", "row/col"'')  ' +
    'WHERE NOT NEW.reference_scope IN  ' +
    '(''geopackage'',''table'',''column'',''row'',''row/col'') ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT =
      ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE =
    'CREATE TRIGGER ''gpkg_metadata_reference_reference_scope_update''  ' +
    'BEFORE UPDATE OF ''reference_scope'' ON ''gpkg_metadata_reference''  ' +
    'FOR EACH ROW BEGIN  ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata_reference  ' +
    'violates constraint: referrence_scope must be one of "geopackage", ' +
    '"table", "column", "row", "row/col"'')  ' +
    'WHERE NOT NEW.reference_scope IN  ' +
    '(''geopackage'',''table'',''column'',''row'',''row/col'') ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT =
    'CREATE TRIGGER ''gpkg_metadata_reference_column_name_insert''  ' +
    'BEFORE INSERT ON ''gpkg_metadata_reference''  ' +
    'FOR EACH ROW BEGIN  ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata_reference  ' +
    'violates constraint: column name must be NULL when reference_scope  ' +
    'is "geopackage", "table" or "row"'')  ' +
    'WHERE (NEW.reference_scope IN (''geopackage'',''table'',''row'')  ' +
    'AND NEW.column_name IS NOT NULL) ; ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata_reference  ' +
    'violates constraint: column name must be defined for the specified  ' +
    'table when reference_scope is "column" or "row/col"'')  ' +
    'WHERE (NEW.reference_scope IN (''column'',''row/col'')  ' +
    'AND NOT NEW.table_name IN (  ' +
    'SELECT name FROM SQLITE_MASTER WHERE type = ''table''  ' +
    'AND name = NEW.table_name  ' +
    'AND sql LIKE (''%'' || NEW.column_name || ''%''))) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE =
    'CREATE TRIGGER ''gpkg_metadata_reference_column_name_update'' ' +
    'BEFORE UPDATE OF column_name ON ''gpkg_metadata_reference'' ' +
    'FOR EACH ROW BEGIN ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata_reference ' +
    'violates constraint: column name must be NULL when reference_scope ' +
    'is "geopackage", "table" or "row"'') ' +
    'WHERE (NEW.reference_scope IN (''geopackage'',''table'',''row'') ' +
    'AND NEW.column_nameIS NOT NULL) ; ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata_reference ' +
    'violates constraint: column name must be defined for the specified ' +
    'table when reference_scope is "column" or "row/col"'') ' +
    'WHERE (NEW.reference_scope IN (''column'',''row/col'') ' +
    'AND NOT NEW.table_name IN ( ' +
    'SELECT name FROM SQLITE_MASTER WHERE type = ''table'' ' +
    'AND name = NEW.table_name ' +
    'AND sql LIKE (''%'' || NEW.column_name || ''%''))) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT =
    'CREATE TRIGGER ''gpkg_metadata_reference_row_id_value_insert''  ' +
    'BEFORE INSERT ON ''gpkg_metadata_reference''  ' +
    'FOR EACH ROW BEGIN  ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata_reference  ' +
    'violates constraint: row_id_value must be NULL when reference_scope  ' +
    'is "geopackage", "table" or "column"'')  ' +
    'WHERE NEW.reference_scope IN (''geopackage'',''table'',''column'')  ' +
    'AND NEW.row_id_value IS NOT NULL ; ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata_reference  ' +
    'violates constraint: row_id_value must exist in specified table when  ' +
    'reference_scope is "row" or "row/col"'')  ' +
    'WHERE NEW.reference_scope IN (''row'',''row/col'')  ' +
    'AND NOT EXISTS (SELECT rowid  ' +
    'FROM (SELECT NEW.table_name AS table_name) WHERE rowid = ' +
    'NEW.row_id_value) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE =
    'CREATE TRIGGER ''gpkg_metadata_reference_row_id_value_update'' ' +
    'BEFORE UPDATE OF ''row_id_value'' ON ''gpkg_metadata_reference'' ' +
    'FOR EACH ROW BEGIN ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata_reference ' +
    'violates constraint: row_id_value must be NULL when reference_scope ' +
    'is "geopackage", "table" or "column"'') ' +
    'WHERE NEW.reference_scope IN (''geopackage'',''table'',''column'') ' +
    'AND NEW.row_id_value IS NOT NULL ; ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata_reference ' +
    'violates constraint: row_id_value must exist in specified table when ' +
    'reference_scope is "row" or "row/col"'') ' +
    'WHERE NEW.reference_scope IN (''row'',''row/col'') ' +
    'AND NOT EXISTS (SELECT rowid ' +
    'FROM (SELECT NEW.table_name AS table_name) WHERE rowid = ' +
    'NEW.row_id_value) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT =
    'CREATE TRIGGER ''gpkg_metadata_reference_timestamp_insert'' ' +
    'BEFORE INSERT ON ''gpkg_metadata_reference'' ' +
    'FOR EACH ROW BEGIN ' +
    'SELECT RAISE(ABORT, ''insert on table gpkg_metadata_reference ' +
    'violates constraint: timestamp must be a valid time in ISO 8601 ' +
    '"yyyy-mm-ddThh:mm:ss.cccZ" form'') ' +
    'WHERE NOT (NEW.timestamp GLOB ' +
    '''[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0- ' +
    '9]:[0-5][0-9].[0-9][0-9][0-9]Z'' ' +
    'AND strftime(''%s'',NEW.timestamp) NOT NULL) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE + 1 ;

  CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE =
    'CREATE TRIGGER ''gpkg_metadata_reference_timestamp_update'' ' +
    'BEFORE UPDATE OF ''timestamp'' ON ''gpkg_metadata_reference'' ' +
    'FOR EACH ROW BEGIN ' +
    'SELECT RAISE(ABORT, ''update on table gpkg_metadata_reference ' +
    'violates constraint: timestamp must be a valid time in ISO 8601 ' +
    '"yyyy-mm-ddThh:mm:ss.cccZ" form'') ' +
    'WHERE NOT (NEW.timestamp GLOB ' +
    '''[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]T[0-2][0-9]:[0-5][0- ' +
    '9]:[0-5][0-9].[0-9][0-9][0-9]Z'' ' +
    'AND strftime(''%s'',NEW.timestamp) NOT NULL) ; ' +
    'END' ;
    ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT + 1 ;

  CREATE_VIRTUAL_TABLE_RTREE =
    'CREATE VIRTUAL TABLE "rtree_%s_%s" USING rtree(id, minx, maxx, miny, maxy)' ;
    ID_CREATE_VIRTUAL_TABLE_RTREE =
      ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE + 1 ;

  INSERT_REPLACE_INTO_RTREE =
    'INSERT OR REPLACE INTO "rtree_%s_%s" ' +
    '  SELECT %s, ST_MinX(%s), ST_MaxX(%s), ST_MinY(%s), ST_MaxY(%s) FROM "%s" ;' ;
    ID_INSERT_REPLACE_INTO_RTREE =
      ID_CREATE_VIRTUAL_TABLE_RTREE + 1 ;

  CREATE_TRIGGER_RTREE_INSERT =
    'CREATE TRIGGER "rtree_%s_%s_insert" AFTER INSERT ON "%s" ' +
    '  WHEN (new.%s NOT NULL AND NOT ST_IsEmpty(NEW.%s)) ' +
    'BEGIN ' +
    '  INSERT OR REPLACE INTO "rtree_%s_%s" VALUES ( ' +
    '    NEW.%s, ' +
    '    ST_MinX(NEW.%s), ST_MaxX(NEW.%s), ' +
    '    ST_MinY(NEW.%s), ST_MaxY(NEW.%s) ' +
    '  ) ; ' +
    'end ;' ;
    ID_CREATE_TRIGGER_RTREE_INSERT =
      ID_INSERT_REPLACE_INTO_RTREE + 1 ;

  CREATE_TRIGGER_RTREE_UPDATE1 =
    'CREATE TRIGGER "rtree_%s_%s_update1" AFTER UPDATE OF %s ON "%s" ' +
    '  WHEN OLD.%s = NEW.%s AND ' +
    '       (NEW.%s NOTNULL AND NOT ST_IsEmpty(NEW.%s)) ' +
    'BEGIN ' +
    '  INSERT OR REPLACE INTO "rtree_%s_%s" VALUES ( ' +
    '    NEW.%s, ' +
    '    ST_MinX(NEW.%s), ST_MaxX(NEW.%s), ' +
    '    ST_MinY(NEW.%s), ST_MaxY(NEW.%s) ' +
    '  ) ; ' +
    'end ;' ;
    ID_CREATE_TRIGGER_RTREE_UPDATE1 =
      ID_CREATE_TRIGGER_RTREE_INSERT + 1 ;

  CREATE_TRIGGER_RTREE_UPDATE2 =
    'CREATE TRIGGER "rtree_%s_%s_update2" AFTER UPDATE OF %s ON "%s" ' +
    '  WHEN OLD.%s = NEW.%s AND ' +
    '       (NEW.%s ISNULL OR ST_IsEmpty(NEW.%s)) ' +
    'BEGIN ' +
    '  DELETE FROM "rtree_%s_%s" WHERE id = OLD.%s ; ' +
    'end ;' ;
    ID_CREATE_TRIGGER_RTREE_UPDATE2 =
      ID_CREATE_TRIGGER_RTREE_UPDATE1 + 1 ;

  CREATE_TRIGGER_RTREE_UPDATE3 =
    'CREATE TRIGGER "rtree_%s_%s_update3" AFTER UPDATE OF %s ON "%s" ' +
    '  WHEN OLD.%s != NEW.%s AND ' +
    '       (NEW.%s NOTNULL AND NOT ST_IsEmpty(NEW.%s))  ' +
    'BEGIN  ' +
    '  DELETE FROM "rtree_%s_%s" WHERE id = OLD.%s ;  ' +
    '  INSERT OR REPLACE INTO "rtree_%s_%s" VALUES (  ' +
    '    NEW.%s,  ' +
    '    ST_MinX(NEW.%s), ST_MaxX(NEW.%s),  ' +
    '    ST_MinY(NEW.%s), ST_MaxY(NEW.%s)  ' +
    '  ) ;  ' +
    'end ;' ;
    ID_CREATE_TRIGGER_RTREE_UPDATE3 =
      ID_CREATE_TRIGGER_RTREE_UPDATE2 + 1 ;

  CREATE_TRIGGER_RTREE_UPDATE4 =
    'CREATE TRIGGER "rtree_%s_%s_update4" AFTER UPDATE ON "%s"  ' +
    '  WHEN OLD.%s != NEW.%s AND  ' +
    '       (NEW.%s ISNULL OR ST_IsEmpty(NEW.%s))  ' +
    'BEGIN  ' +
    '  DELETE FROM "rtree_%s_%s" WHERE id IN (OLD.%s, NEW.%s) ;  ' +
    'end ;' ;
    ID_CREATE_TRIGGER_RTREE_UPDATE4 =
      ID_CREATE_TRIGGER_RTREE_UPDATE3 + 1 ;

  CREATE_TRIGGER_RTREE_DELETE =
    'CREATE TRIGGER "rtree_%s_%s_delete" AFTER DELETE ON "%s"  ' +
    '  WHEN old.%s NOT NULL  ' +
    'BEGIN  ' +
    '  DELETE FROM "rtree_%s_%s" WHERE id = OLD.%s ;  ' +
    'end ;' ;
    ID_CREATE_TRIGGER_RTREE_DELETE =
      ID_CREATE_TRIGGER_RTREE_UPDATE4 + 1 ;

  INSERT_RTREE_GPKG_EXTENSIONS =
    'INSERT INTO gpkg_extensions (  ' +
    'table_name,column_name,extension_name,definition,scope)   ' +
    'VALUES (''%s'', ''%s'', ''gpkg_rtree_index'',   ' +
    '''GeoPackage 1.0 Specification Annex L'', ''write-only'')' ;
    ID_INSERT_RTREE_GPKG_EXTENSIONS =
      ID_CREATE_TRIGGER_RTREE_DELETE + 1 ;

  SELECT_RTREE_GPKG_EXTENSIONS =
    'SELECT * FROM gpkg_extensions ' +
    'WHERE table_name=''%s'' AND column_name=''%s'' AND ' +
    'extension_name=''gpkg_rtree_index'' ' ;
    ID_SELECT_RTREE_GPKG_EXTENSIONS =
      ID_INSERT_RTREE_GPKG_EXTENSIONS + 1 ;

  CREATE_TABLE_FEATURE =
    'CREATE TABLE %s (  ' +
    '  <#UID#> INTEGER PRIMARY KEY AUTOINCREMENT,  ' +
    '  <#GEOM#> %s  ' +
    ') ;' ;
    ID_CREATE_TABLE_FEATURE =
      ID_SELECT_RTREE_GPKG_EXTENSIONS + 1 ;

  DROPTABLE =
    'DROP TABLE <#IF_EXISTS#> %s' ;
    ID_DROPTABLE =
      ID_CREATE_TABLE_FEATURE + 1 ;

  SELECT_MAX_UID =
    'SELECT <#MAX#>(<#UID#>) <#AS#> <#MAX_UID#> FROM %s' ;
    ID_SELECT_MAX_UID =
      ID_DROPTABLE + 1 ;

  MAX_UID =
    '<#MAX_UID#>' ;
    ID_MAX_UID =
      ID_SELECT_MAX_UID + 1 ;

  DELETESHAPE =
    'DELETE FROM %S WHERE <#UID#>=%d' ;
    ID_DELETESHAPE =
      ID_MAX_UID + 1 ;

  DELETE_FROM_GPKG_EXTENSIONS =
    'DELETE FROM gpkg_extensions WHERE table_name=''%s'' ' +
    'AND column_name=''%s'' AND extension_name=''gpkg_rtree_index''' ;
    ID_DELETE_FROM_GPKG_EXTENSIONS =
      ID_DELETESHAPE + 1 ;

  DELETE_FROM_GPKG_GEOMETRY_COLUMNS =
    'DELETE FROM gpkg_geometry_columns WHERE table_name = ''%s''' ;
    ID_DELETE_FROM_GPKG_GEOMETRY_COLUMNS =
      ID_DELETE_FROM_GPKG_EXTENSIONS + 1 ;

  DELETE_FROM_GPKG_CONTENTS =
    'DELETE FROM gpkg_contents WHERE table_name = ''%s''' ;
    ID_DELETE_FROM_GPKG_CONTENTS =
      ID_DELETE_FROM_GPKG_GEOMETRY_COLUMNS + 1 ;

  INSERT_GPKG_CONTENTS =
    'INSERT INTO gpkg_contents ' +
    '(table_name,data_type,identifier,description,last_change,srs_id) VALUES' +
    '(''%s'',''%s'',''%s'',''%s'',strftime(''%%Y-%%m-%%dT%%H:%%M:%%fZ'',CURRENT_TIMESTAMP),%d)' ;
    ID_INSERT_GPKG_CONTENTS =
      ID_DELETE_FROM_GPKG_CONTENTS + 1 ;

  SELECT_GPKG_CONTENTS =
    'SELECT table_name, data_type, identifier, description, ' +
    'min_x, min_y, max_x, max_y, srs_id FROM gpkg_contents ' +
    'WHERE table_name = ''%s'' and data_type = ''features''' ;
    ID_SELECT_GPKG_CONTENTS =
      ID_INSERT_GPKG_CONTENTS + 1 ;

  UPDATE_GPKG_CONTENTS =
    'UPDATE gpkg_contents SET ' +
    'identifier=''%s'', description=''%s'', ' +
    'min_x = %s, min_y = %s, max_x = %s, max_y = %s ' +
    'WHERE table_name = ''%s'' AND Lower(data_type) = ''features''' ;
    ID_UPDATE_GPKG_CONTENTS =
      ID_SELECT_GPKG_CONTENTS + 1 ;

  INSERT_GPKG_GEOMETRY_COLUMNS =
    'INSERT INTO gpkg_geometry_columns ' +
    '(table_name,column_name,geometry_type_name,srs_id,z,m)' +
    ' VALUES (''%s'',''%s'',''%s'',%d,%d,%d)' ;
    ID_INSERT_GPKG_GEOMETRY_COLUMNS =
      ID_UPDATE_GPKG_CONTENTS + 1 ;

  UPDATE_GPKG_GEOMETRY_COLUMNS =
    'UPDATE gpkg_geometry_columns ' +
    'SET geometry_type_name=''%s'', z=%d, m=%d ' +
    'WHERE table_name=''%s''' ;
    ID_UPDATE_GPKG_GEOMETRY_COLUMNS =
      ID_INSERT_GPKG_GEOMETRY_COLUMNS + 1 ;

  SELECT_GPKG_GEOMETRY_COLUMNS =
    'SELECT table_name, column_name, geometry_type_name, srs_id, z ' +
    'FROM gpkg_geometry_columns WHERE table_name = ''%s''' ;
    ID_SELECT_GPKG_GEOMETRY_COLUMNS =
      ID_UPDATE_GPKG_GEOMETRY_COLUMNS + 1 ;

  SELECT_GPKG_SPATIAL_REF_SYS =
    'SELECT srs_id, definition FROM gpkg_spatial_ref_sys WHERE ' +
    'upper(organization) = upper(''%s'') AND organization_coordsys_id = %d' ;
    ID_SELECT_GPKG_SPATIAL_REF_SYS =
      ID_SELECT_GPKG_GEOMETRY_COLUMNS + 1 ;

  SELECT_GPKG_SPATIAL_REF_SYS_MAX_SRID =
    'SELECT MAX(srs_id) as MAX_SRID FROM gpkg_spatial_ref_sys' ;
    ID_SELECT_GPKG_SPATIAL_REF_SYS_MAX_SRID =
      ID_SELECT_GPKG_SPATIAL_REF_SYS + 1 ;

  INSERT_GPKG_SPATIAL_REF_SYS =
    'INSERT INTO gpkg_spatial_ref_sys ' +
    '(srs_name,srs_id,organization,organization_coordsys_id,definition) ' +
    'VALUES (''%s'', %d, upper(''%s''), %d, ''%s'')' ;
    ID_INSERT_GPKG_SPATIAL_REF_SYS =
      ID_SELECT_GPKG_SPATIAL_REF_SYS_MAX_SRID + 1 ;

  UPDATE_GEO =
    '<#GEOM#>=:<#GEOM#> ' ;
    ID_UPDATE_GEO =
      ID_INSERT_GPKG_SPATIAL_REF_SYS + 1 ;

  SELECT_PRAGMA_INFO =
    'pragma table_info(''%s'')' ;
    ID_SELECT_PRAGMA_INFO =
      ID_UPDATE_GEO + 1 ;

  SELECT_TABLE_WHERE =
    'SELECT * FROM %s WHERE %s' ;
    ID_SELECT_TABLE_WHERE =
      ID_SELECT_PRAGMA_INFO + 1 ;

  UPDATE =
    'UPDATE %s SET %s WHERE %s=:%s %s' ;
    ID_UPDATE =
      ID_SELECT_TABLE_WHERE + 1 ;

  INSERT_GEO_VALUE =
    '<#UID#>,<#GEOM#>' ;
    ID_INSERT_GEO_VALUE =
      ID_UPDATE + 1 ;

  INSERT_GEO_PARAM =
    ':<#UID#>,:<#GEOM#>' ;
    ID_INSERT_GEO_PARAM =
      ID_INSERT_GEO_VALUE + 1 ;

  INSERT =
    'INSERT INTO %s ( %s ) VALUES ( %s )' ;
    ID_INSERT =
      ID_INSERT_GEO_PARAM + 1 ;

  INSERT_DEFAULT_VALUES =
    'INSERT INTO %s DEFAULT VALUES ' ;
    ID_INSERT_DEFAULT_VALUES =
      ID_INSERT + 1 ;

  ALTER_DROP_COLUMN =
    'ALTER TABLE %s <#DROP COLUMN#> <#QUOTE LEFT#>%s<#QUOTE RIGHT#>' ;
    ID_ALTER_DROP_COLUMN =
      ID_INSERT_DEFAULT_VALUES + 1 ;

  ALTER_ADD_STRING =
    'ALTER TABLE %s ADD %s <#VARCHAR#>(%d<#VARCHAR_TYPE#>)' ;
    ID_ALTER_ADD_STRING =
      ID_ALTER_DROP_COLUMN + 1 ;

  ALTER_ADD_MEMO =
    'ALTER TABLE %s ADD %s <#TEXT#>' ;
    ID_ALTER_ADD_MEMO =
      ID_ALTER_ADD_STRING + 1 ;

  ALTER_ALTER_STRING =
    'ALTER TABLE %s <#ALTER#> %s <#VARCHAR_LEN#> <#VARCHAR#>(%d)' ;
    ID_ALTER_ALTER_STRING =
      ID_ALTER_ADD_MEMO + 1 ;

  ALTER_ADD_DATE =
    'ALTER TABLE %s ADD %s <#DATE#>' ;
    ID_ALTER_ADD_DATE =
      ID_ALTER_ALTER_STRING + 1 ;

  ALTER_ADD_BOOLEAN =
    'ALTER TABLE %s ADD %s <#BOOLEAN#>' ;
    ID_ALTER_ADD_BOOLEAN =
      ID_ALTER_ADD_DATE + 1 ;

  ALTER_ADD_INTEGER =
    'ALTER TABLE %s ADD %s <#INTEGER#>' ;
    ID_ALTER_ADD_INTEGER =
      ID_ALTER_ADD_BOOLEAN + 1 ;

  ALTER_ADD_BIGINT =
    'ALTER TABLE %s ADD %s <#BIGINT#>' ;
    ID_ALTER_ADD_BIGINT =
      ID_ALTER_ADD_INTEGER + 1 ;

  ALTER_ADD_FLOAT =
    'ALTER TABLE %s ADD %s <#DOUBLE#>' ;
    ID_ALTER_ADD_FLOAT =
      ID_ALTER_ADD_BIGINT + 1 ;

  FILTER_UID =
    '%s=%d' ;
    ID_FILTER_UID =
      ID_ALTER_ADD_FLOAT + 1 ;

  FILTER_UID_WEAK =
    '(%s>=%d) and (%s<=%d)' ;
    ID_FILTER_UID_WEAK =
      ID_FILTER_UID + 1 ;

  MAX_NAMELENGTH =
    '<#MAX_NAMELENGTH#>' ;
    ID_MAX_NAMELENGTH =
      ID_FILTER_UID_WEAK + 1 ;

  MAX_TEXTLENGTH =
    '<#MAX_TEXTLENGTH#>' ;
    ID_MAX_TEXTLENGTH =
      ID_MAX_NAMELENGTH + 1 ;

  UID =
    '<#UID#>' ;
    ID_UID =
      ID_MAX_TEXTLENGTH + 1 ;

  GEOMETRY =
    '<#GEOM#>' ;
    ID_GEOMETRY =
      ID_UID + 1 ;

  GEO =
    '<#GEO#>' ;
    ID_GEO =
      ID_GEOMETRY + 1 ;

  CHECK_RTREE =
    'select * from sqlite_master where type=''table'' and name=''%s''' ;
    ID_CHECK_RTREE =
      ID_GEO + 1 ;

  SELECTJOIN_NOEXT =
    'SELECT * FROM %s <#GEO#> ' ;
    ID_SELECTJOIN_NOEXT =
      ID_CHECK_RTREE + 1 ;

  SELECTJOIN_NOEXT_EX =
    'SELECT * FROM %s <#GEO#> WHERE (%s) ' ;
    ID_SELECTJOIN_NOEXT_EX =
      ID_SELECTJOIN_NOEXT + 1 ;

  SELECTJOIN_RTREE =
    'SELECT * FROM %s <#GEO#>' +
    ' WHERE <#GEO#>.<#UID#> IN ( SELECT id FROM %s WHERE' +
    '  maxx>%s AND minx<%s AND maxy>%s AND miny<%s )' ;
    ID_SELECTJOIN_RTREE =
      ID_SELECTJOIN_NOEXT_EX + 1 ;

  SELECTJOIN_RTREE_EX =
    'SELECT * FROM %s <#GEO#>' +
    ' WHERE (%s) AND <#GEO#>.<#UID#> IN ( SELECT id FROM %s WHERE' +
    '  maxx>%s AND minx<%s AND maxy>%s AND miny<%s)' ;
    ID_SELECTJOIN_RTREE_EX =
      ID_SELECTJOIN_RTREE + 1 ;

  SELECTJOIN =
    'SELECT * FROM %s <#GEO#> WHERE ' +
    'ST_MaxX(%s)>%s AND ST_MinX(%s)<%s AND ST_MaxY(%s)>%s AND ST_MinY(%s)<%s' ;
    ID_SELECTJOIN =
      ID_SELECTJOIN_RTREE_EX + 1 ;

  SELECTJOIN_EX =
    'SELECT * FROM %s <#GEO#>' +
    ' WHERE (%s) AND ' +
    'ST_MaxX(%s)>%s AND ST_MinX(%s)<%s AND ST_MaxY(%s)>%s AND ST_MinY(%s)<%s' ;
    ID_SELECTJOIN_EX =
      ID_SELECTJOIN + 1 ;

  SELECT_GPKG_GEOMETRY_COLUMNS_ALL =
    'SELECT table_name, geometry_type_name FROM gpkg_geometry_columns ' ;
    ID_SELECT_GPKG_GEOMETRY_COLUMNS_ALL =
      ID_SELECTJOIN_EX + 1 ;


  ID_END = ID_SELECT_GPKG_GEOMETRY_COLUMNS_ALL                                           ;
  end;


{ T_GpkgHeader }

  function T_GpkgHeader.Read(
    const _buf : TBytes
  ) : Boolean ;
  var
    flags : Byte ;
    off   : Byte ;
  begin
    Result := False ;
    if _buf = nil then exit ;
    if length( _buf ) < 4 then exit ;

    if (_buf[0]<>$47) and (_buf[1]<>$50) then exit ;
//    if (_buf[2]<>0) then exit ; // only version 0 is supported

    flags         := _buf[3] ;
    flag_Empty    := (flags and ($01 shl 4)) = ($01 shl 4) ;
    flag_Extended := (flags and ($01 shl 5)) = ($01 shl 5);
    flag_Dims     := (flags and ($07 shl 1)) shr 1 ;
    {$IFDEF OXYGENE}
      srs_id := BitConverter.ToInt32(_buf, 4) ;
    {$ELSE}
      Move( _buf[4], srs_id, 4 ) ;
    {$ENDIF}

    envelope := GisNoWorld3D ;
    off := 8 ;
    if flag_Dims >= 1 then begin
      {$IFDEF OXYGENE}
       envelope.XMin := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.XMin, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.XMax := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.XMax, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.YMin := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.YMin, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.YMax := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.YMax, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
    end ;

    if flag_Dims = 2 then begin
      {$IFDEF OXYGENE}
        envelope.ZMin := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
       Move( _buf[off], envelope.ZMin, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.ZMax := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.ZMax, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
    end else if flag_Dims = 3 then begin
      {$IFDEF OXYGENE}
        envelope.MMin := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.MMin, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.MMax := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.MMax, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
    end
    else if flag_Dims = 4 then begin
      {$IFDEF OXYGENE}
        envelope.ZMin := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.ZMin, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.ZMax := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.ZMax, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.MMin := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.MMin, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
      {$IFDEF OXYGENE}
        envelope.MMax := BitConverter.ToDouble(_buf, off) ;
      {$ELSE}
        Move( _buf[off], envelope.MMax, 8 ) ;
      {$ENDIF}
      inc(off,8) ;
    end ;

    header_size := off ;
    Result := True ;
  end ;

{ T_StandardGpkgBinary }

  function T_StandardGpkgBinary.ReadHeader(
    const _buf : TBytes
  ) : Boolean ;
  begin
    Result := header.Read( _buf ) ;
  end ;


  function T_StandardGpkgBinary.ShapeToGpkg(
    const _shp : TGIS_Shape
  ) : TBytes ;
  var
    bflags    : Byte ;
    buf       : TGIS_MemoryStream ;
    envelope  : Byte ;
    ext       : TGIS_Extent3D ;
    wkb       : OleVariant ;
    wkbbuf    : TBytes ;
    b         : Byte ;
  begin
    assert( _shp <> nil );
    envelope := 0 ;
    buf := TGIS_MemoryStream.Create ;
    try
      // Header Magic
      b := $47 ;
      {$IFDEF OXYGENE}
        buf.WriteByte( b ) ;
      {$ELSE}
        buf.Write( b, 1 ) ;
      {$ENDIF}
      b := $50 ;
      {$IFDEF OXYGENE}
        buf.WriteByte( b ) ;
      {$ELSE}
        buf.Write( b, 1 ) ;
      {$ENDIF}
      // GPKG BLOB Version
      b := $00 ;
      {$IFDEF OXYGENE}
        buf.WriteByte( b ) ;
      {$ELSE}
        buf.Write( b, 1 ) ;
      {$ENDIF}

      if _shp.ShapeType = TGIS_ShapeType.Point then
        envelope := 0
      else begin
        if (_shp.Dimension = TGIS_DimensionType.XY) or (_shp.Dimension = TGIS_DimensionType.Unknown) then
          envelope := 1
        else if _shp.Dimension = TGIS_DimensionType.XYZ then
          envelope := 2
        else if _shp.Dimension = TGIS_DimensionType.XYM then
          envelope := 3
        else if _shp.Dimension = TGIS_DimensionType.XYZM then
          envelope := 4 ;
      end ;

      bflags := 0;
      if ( _shp.IsEmpty ) then begin
        envelope := 0 ;
        bflags := bflags or (1 shl 4) ;
      end ;

      bflags := bflags or (envelope shl 1) ;
      bflags := bflags or 1;

      // Write flags byte
      {$IFDEF OXYGENE}
        buf.WriteByte( bflags ) ;
      {$ELSE}
        buf.Write( bflags, 1 ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        buf.WriteInteger( _shp.Layer.CS.EPSG ) ;
      {$ELSE}
        buf.Write( _shp.Layer.CS.EPSG, 4 ) ;
      {$ENDIF}

      // Write envelope */
      if not _shp.IsEmpty and not ( _shp.ShapeType = TGIS_ShapeType.Point ) then begin
        ext := _shp.Extent3D ;
        {$IFDEF OXYGENE}
          buf.WriteDouble( ext.XMin, 8 ) ;
          buf.WriteDouble( ext.XMax, 8 ) ;
          buf.WriteDouble( ext.YMin, 8 ) ;
          buf.WriteDouble( ext.YMax, 8 ) ;
        {$ELSE}
          buf.Write( ext.XMin, 8 ) ;
          buf.Write( ext.XMax, 8 ) ;
          buf.Write( ext.YMin, 8 ) ;
          buf.Write( ext.YMax, 8 ) ;
        {$ENDIF}

        if _shp.Dimension = TGIS_DimensionType.XYZ then begin
          {$IFDEF OXYGENE}
            buf.WriteDouble( ext.ZMin, 8 ) ;
            buf.WriteDouble( ext.ZMax, 8 ) ;
          {$ELSE}
            buf.Write( ext.ZMin, 8 ) ;
            buf.Write( ext.ZMax, 8 ) ;
          {$ENDIF}
        end
        else if _shp.Dimension = TGIS_DimensionType.XYM then begin
          {$IFDEF OXYGENE}
            buf.WriteDouble( ext.MMin, 8 ) ;
            buf.WriteDouble( ext.MMax, 8 ) ;
          {$ELSE}
            buf.Write( ext.MMin, 8 ) ;
            buf.Write( ext.MMax, 8 ) ;
          {$ENDIF}
        end
        else if _shp.Dimension = TGIS_DimensionType.XYZM then begin
          {$IFDEF OXYGENE}
            buf.WriteDouble( ext.ZMin, 8 ) ;
            buf.WriteDouble( ext.ZMax, 8 ) ;
            buf.WriteDouble( ext.MMin, 8 ) ;
            buf.WriteDouble( ext.MMax, 8 ) ;
          {$ELSE}
            buf.Write( ext.ZMin, 8 ) ;
            buf.Write( ext.ZMax, 8 ) ;
            buf.Write( ext.MMin, 8 ) ;
            buf.Write( ext.MMax, 8 ) ;
          {$ENDIF}
        end ;
      end ;

      _shp.ExportToWKB( wkb ) ;
      wkbbuf := TBytes(wkb) ;
      {$IFDEF OXYGENE}
        buf.Write( wkbbuf, length( wkbbuf ) ) ;
      {$ELSE}
        buf.Write( wkbbuf[0], length( wkbbuf ) ) ;
      {$ENDIF}

      SetLength( Result, buf.Size ) ;
      buf.Position := 0 ;
      {$IFDEF OXYGENE}
        buf.Read( Result, 0, buf.Size ) ;
      {$ELSE}
        buf.Read( Result[0], buf.Size ) ;
      {$ENDIF}
    finally
      FreeObject( buf ) ;
    end ;

  end;

  function T_StandardGpkgBinary.ReadGpkg(
    const _buf : TBytes
  ) : Boolean ;
  var
    off : Integer ;
  begin
    Result := ReadHeader( _buf ) ;
    if Result then begin
      off := header.header_size ;
      if header.flag_Extended then begin
        SetLength( extension_code, 4 ) ;
        GisCopyMemory( _buf, off, extension_code, 0, 4 ) ;
        inc( off, 4 ) ;
      end ;
      geometry := Copy( _buf, off, length(_buf)-off )
    end
    else
      geometry := nil ;
  end ;

  function T_StandardGpkgBinary.ReadExtent(
    const _buf : TBytes
  ) : Boolean ;
  var
    shp : TGIS_Shape ;
  begin
    Result := ReadHeader( _buf ) ;
    if not Result then exit ;

    if (header.flag_Dims = 0) then begin
      geometry := Copy( _buf, header.header_size, length(_buf)-header.header_size ) ;
      shp := TGIS_GeometryFactory.GisCreateShapeFromWKB( geometry, nil, nil, False, -1, nil ) ;
      try
        if assigned( shp ) and not shp.IsEmpty then begin
          header.envelope := shp.Extent3D ;
          header.shpType  := shp.ShapeType ;
          Result := True ;
        end
        else
          Result := False ;
      finally
        FreeObject( shp ) ;
      end ;
    end
    else
      Result := True ;
  end ;

  function T_StandardGpkgBinary.GpkgToShape(
    const _buf : TBytes
  ) : TGIS_Shape ;
  begin
    if ReadGpkg( _buf ) then
      Result := TGIS_GeometryFactory.GisCreateShapeFromWKB( geometry, nil, nil, False, -1, nil )
    else
      Result := nil ;
  end ;


//=============================================================================
// TGIS_LayerSqlGpkg
//=============================================================================

  constructor TGIS_LayerSqlGpkgAbstract.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;

    lastNewUid   := -1 ;
    canSkipField := False ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.doDestroy ;
  begin
    inherited ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.fget_TableGeometry
    : String ;
  begin
    Result := safeName( Table ) ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.fget_TableIndex
    : String ;
  begin
    Result := safeName( Format( 'rtree_%s_%s', [ Table, getCmdGEOMETRY ] ) ) ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.fget_ViewFeatures
    : String ;
  begin
    if not IsStringEmpty( FViewFeatures ) then
      Result := FViewFeatures
    else
      Result := Table ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.prepareCommandList ;
  begin
    initializeCommandList( T_SQLGPKG.ID_END ) ;

    assert(FSQLCommands[T_SQLGPKG.ID_PRAGMA_APPLICATION_ID]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_PRAGMA_APPLICATION_ID]
                        := T_SQLGPKG.PRAGMA_APPLICATION_ID ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_SPATIAL_REF_SYS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_SPATIAL_REF_SYS]
                        := T_SQLGPKG.CREATE_GPKG_SPATIAL_REF_SYS ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN]
                        := T_SQLGPKG.INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC]
                        := T_SQLGPKG.INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_WGS84]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_WGS84]
                        := T_SQLGPKG.INSERT_GPKG_SPATIAL_REF_SYS_WGS84 ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_CONTENTS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_CONTENTS]
                        := T_SQLGPKG.CREATE_GPKG_CONTENTS ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_GEOMETRY_COLUMNS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_GEOMETRY_COLUMNS]
                        := T_SQLGPKG.CREATE_GPKG_GEOMETRY_COLUMNS ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_EXTENSIONS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_EXTENSIONS]
                        := T_SQLGPKG.CREATE_GPKG_EXTENSIONS ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_DATA_COLUMNS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_DATA_COLUMNS]
                        := T_SQLGPKG.CREATE_GPKG_DATA_COLUMNS ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_DATA_COLUMN_CONSTRAINTS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_DATA_COLUMN_CONSTRAINTS]
                        := T_SQLGPKG.CREATE_GPKG_DATA_COLUMN_CONSTRAINTS ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_METADATA]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_METADATA]
                        := T_SQLGPKG.CREATE_GPKG_METADATA ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_METADATA_REFERENCE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_GPKG_METADATA_REFERENCE]
                        := T_SQLGPKG.CREATE_GPKG_METADATA_REFERENCE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE]
                        := T_SQLGPKG.CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_VIRTUAL_TABLE_RTREE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_VIRTUAL_TABLE_RTREE]
                        := T_SQLGPKG.CREATE_VIRTUAL_TABLE_RTREE ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_REPLACE_INTO_RTREE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_REPLACE_INTO_RTREE]
                        := T_SQLGPKG.INSERT_REPLACE_INTO_RTREE ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_INSERT]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_INSERT]
                        := T_SQLGPKG.CREATE_TRIGGER_RTREE_INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE1]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE1]
                        := T_SQLGPKG.CREATE_TRIGGER_RTREE_UPDATE1 ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE2]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE2]
                        := T_SQLGPKG.CREATE_TRIGGER_RTREE_UPDATE2 ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE3]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE3]
                        := T_SQLGPKG.CREATE_TRIGGER_RTREE_UPDATE3 ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE4]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE4]
                        := T_SQLGPKG.CREATE_TRIGGER_RTREE_UPDATE4 ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_DELETE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_DELETE]
                        := T_SQLGPKG.CREATE_TRIGGER_RTREE_DELETE ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_RTREE_GPKG_EXTENSIONS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_RTREE_GPKG_EXTENSIONS]
                        := T_SQLGPKG.INSERT_RTREE_GPKG_EXTENSIONS ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_RTREE_GPKG_EXTENSIONS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_RTREE_GPKG_EXTENSIONS]
                        := T_SQLGPKG.SELECT_RTREE_GPKG_EXTENSIONS ;
    assert(FSQLCommands[T_SQLGPKG.ID_CREATE_TABLE_FEATURE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_CREATE_TABLE_FEATURE]
                        := T_SQLGPKG.CREATE_TABLE_FEATURE ;
    assert(FSQLCommands[T_SQLGPKG.ID_DROPTABLE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_DROPTABLE]
                        := T_SQLGPKG.DROPTABLE ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_MAX_UID]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_MAX_UID]
                        := T_SQLGPKG.SELECT_MAX_UID ;
    assert(FSQLCommands[T_SQLGPKG.ID_MAX_UID]='') ;
           FSQLCommands[T_SQLGPKG.ID_MAX_UID]
                        := T_SQLGPKG.MAX_UID ;
    assert(FSQLCommands[T_SQLGPKG.ID_DELETESHAPE]='') ;
           FSQLCommands[T_SQLGPKG.ID_DELETESHAPE]
                        := T_SQLGPKG.DELETESHAPE ;
    assert(FSQLCommands[T_SQLGPKG.ID_DELETE_FROM_GPKG_GEOMETRY_COLUMNS]='') ;
           FSQLCommands[T_SQLGPKG.ID_DELETE_FROM_GPKG_GEOMETRY_COLUMNS]
                        := T_SQLGPKG.DELETE_FROM_GPKG_GEOMETRY_COLUMNS ;
    assert(FSQLCommands[T_SQLGPKG.ID_DELETE_FROM_GPKG_EXTENSIONS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_DELETE_FROM_GPKG_EXTENSIONS]
                        := T_SQLGPKG.DELETE_FROM_GPKG_EXTENSIONS ;
    assert(FSQLCommands[T_SQLGPKG.ID_DELETE_FROM_GPKG_CONTENTS]='') ;
           FSQLCommands[T_SQLGPKG.ID_DELETE_FROM_GPKG_CONTENTS]
                        := T_SQLGPKG.DELETE_FROM_GPKG_CONTENTS ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_CONTENTS]='') ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_CONTENTS]
                        := T_SQLGPKG.INSERT_GPKG_CONTENTS ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_CONTENTS]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_CONTENTS]
                        := T_SQLGPKG.SELECT_GPKG_CONTENTS ;
    assert(FSQLCommands[T_SQLGPKG.ID_UPDATE_GPKG_CONTENTS]='') ;
           FSQLCommands[T_SQLGPKG.ID_UPDATE_GPKG_CONTENTS]
                        := T_SQLGPKG.UPDATE_GPKG_CONTENTS ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_GEOMETRY_COLUMNS]='') ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_GEOMETRY_COLUMNS]
                        := T_SQLGPKG.INSERT_GPKG_GEOMETRY_COLUMNS ;
    assert(FSQLCommands[T_SQLGPKG.ID_UPDATE_GPKG_GEOMETRY_COLUMNS]='') ;
           FSQLCommands[T_SQLGPKG.ID_UPDATE_GPKG_GEOMETRY_COLUMNS]
                        := T_SQLGPKG.UPDATE_GPKG_GEOMETRY_COLUMNS ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_GEOMETRY_COLUMNS]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_GEOMETRY_COLUMNS]
                        := T_SQLGPKG.SELECT_GPKG_GEOMETRY_COLUMNS ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_SPATIAL_REF_SYS]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_SPATIAL_REF_SYS]
                        := T_SQLGPKG.SELECT_GPKG_SPATIAL_REF_SYS ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_SPATIAL_REF_SYS_MAX_SRID]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_SPATIAL_REF_SYS_MAX_SRID]
                        := T_SQLGPKG.SELECT_GPKG_SPATIAL_REF_SYS_MAX_SRID ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS]
                        := T_SQLGPKG.INSERT_GPKG_SPATIAL_REF_SYS ;
    assert(FSQLCommands[T_SQLGPKG.ID_UPDATE_GEO]='') ;
           FSQLCommands[T_SQLGPKG.ID_UPDATE_GEO]
                        := T_SQLGPKG.UPDATE_GEO ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_PRAGMA_INFO]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_PRAGMA_INFO]
                        := T_SQLGPKG.SELECT_PRAGMA_INFO ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_TABLE_WHERE]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_TABLE_WHERE]
                        := T_SQLGPKG.SELECT_TABLE_WHERE ;
    assert(FSQLCommands[T_SQLGPKG.ID_UPDATE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_UPDATE]
                        := T_SQLGPKG.UPDATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GEO_VALUE]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GEO_VALUE]
                        := T_SQLGPKG.INSERT_GEO_VALUE ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_GEO_PARAM]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_GEO_PARAM]
                        := T_SQLGPKG.INSERT_GEO_PARAM ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT]='') ;
           FSQLCommands[T_SQLGPKG.ID_INSERT]
                        := T_SQLGPKG.INSERT ;
    assert(FSQLCommands[T_SQLGPKG.ID_INSERT_DEFAULT_VALUES]='') ;
           FSQLCommands[T_SQLGPKG.ID_INSERT_DEFAULT_VALUES]
                        := T_SQLGPKG.INSERT_DEFAULT_VALUES ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_DROP_COLUMN]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_DROP_COLUMN]
                        := T_SQLGPKG.ALTER_DROP_COLUMN ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_STRING]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_STRING]
                        := T_SQLGPKG.ALTER_ADD_STRING ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_MEMO]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_MEMO]
                        := T_SQLGPKG.ALTER_ADD_MEMO ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ALTER_STRING]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ALTER_STRING]
                        := T_SQLGPKG.ALTER_ALTER_STRING ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_DATE]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_DATE]
                        := T_SQLGPKG.ALTER_ADD_DATE ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_BOOLEAN]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_BOOLEAN]
                        := T_SQLGPKG.ALTER_ADD_BOOLEAN ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_INTEGER]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_INTEGER]
                        := T_SQLGPKG.ALTER_ADD_INTEGER ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_BIGINT]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_BIGINT]
                        := T_SQLGPKG.ALTER_ADD_BIGINT ;
    assert(FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_FLOAT]='') ;
           FSQLCommands[T_SQLGPKG.ID_ALTER_ADD_FLOAT]
                        := T_SQLGPKG.ALTER_ADD_FLOAT ;
    assert(FSQLCommands[T_SQLGPKG.ID_FILTER_UID]='') ;
           FSQLCommands[T_SQLGPKG.ID_FILTER_UID]
                        := T_SQLGPKG.FILTER_UID ;
    assert(FSQLCommands[T_SQLGPKG.ID_FILTER_UID_WEAK]='') ;
           FSQLCommands[T_SQLGPKG.ID_FILTER_UID_WEAK]
                        := T_SQLGPKG.FILTER_UID_WEAK ;
    assert(FSQLCommands[T_SQLGPKG.ID_MAX_NAMELENGTH]='') ;
           FSQLCommands[T_SQLGPKG.ID_MAX_NAMELENGTH]
                        := T_SQLGPKG.MAX_NAMELENGTH ;
    assert(FSQLCommands[T_SQLGPKG.ID_MAX_TEXTLENGTH]='') ;
           FSQLCommands[T_SQLGPKG.ID_MAX_TEXTLENGTH]
                        := T_SQLGPKG.MAX_TEXTLENGTH ;
    assert(FSQLCommands[T_SQLGPKG.ID_UID]='') ;
           FSQLCommands[T_SQLGPKG.ID_UID]
                        := T_SQLGPKG.UID ;
    assert(FSQLCommands[T_SQLGPKG.ID_GEOMETRY]='') ;
           FSQLCommands[T_SQLGPKG.ID_GEOMETRY]
                        := T_SQLGPKG.GEOMETRY ;
    assert(FSQLCommands[T_SQLGPKG.ID_GEO]='') ;
           FSQLCommands[T_SQLGPKG.ID_GEO]
                        := T_SQLGPKG.GEO ;
    assert(FSQLCommands[T_SQLGPKG.ID_CHECK_RTREE]='') ;
           FSQLCommands[T_SQLGPKG.ID_CHECK_RTREE]
                        := T_SQLGPKG.CHECK_RTREE ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_RTREE]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_RTREE]
                        := T_SQLGPKG.SELECTJOIN_RTREE ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_RTREE_EX]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_RTREE_EX]
                        := T_SQLGPKG.SELECTJOIN_RTREE_EX ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_NOEXT_EX]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_NOEXT_EX]
                        := T_SQLGPKG.SELECTJOIN_NOEXT_EX ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_NOEXT]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_NOEXT]
                        := T_SQLGPKG.SELECTJOIN_NOEXT ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECTJOIN]='' ) ;
           FSQLCommands[T_SQLGPKG.ID_SELECTJOIN]
                        := T_SQLGPKG.SELECTJOIN ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_EX]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECTJOIN_EX]
                        := T_SQLGPKG.SELECTJOIN_EX ;
    assert(FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_GEOMETRY_COLUMNS_ALL]='') ;
           FSQLCommands[T_SQLGPKG.ID_SELECT_GPKG_GEOMETRY_COLUMNS_ALL]
                        := T_SQLGPKG.SELECT_GPKG_GEOMETRY_COLUMNS_ALL ;

    finalizeCommandList ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.getCmdGEOUID(
    const _id : Integer
   ) : String ;
  begin
    assert( _id in [0..1] ) ;
    if      _id = 0 then
            Result := getCmd(T_SQLGPKG.ID_UID )
    else if _id = 1 then
            Result := getCmd(T_SQLGPKG.ID_GEO ) + '.' + getCmd(T_SQLGPKG.ID_UID ) ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.getCmdGEOMETRY : String ;
  begin
    Result := getCmd(T_SQLGPKG.ID_GEOMETRY ) ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.shapeTypeToGpkgType(
    const _type : TGIS_ShapeType
  ) : String ;
  begin
    case _type of
      TGIS_ShapeType.Point      : Result := 'POINT' ;
      TGIS_ShapeType.MultiPoint : Result := 'MULTIPOINT' ;
      TGIS_ShapeType.Arc        : Result := 'LINESTRING' ; // 'MULITLINESTRING'
      TGIS_ShapeType.Polygon    : Result := 'POLYGON' ;    // 'MULTIPOLYGON'
      TGIS_ShapeType.Complex    : Result := 'GEOMETRYCOLLECTION' ;
      TGIS_ShapeType.MultiPatch : Result := 'POLYGON' ;
    else                          Result := 'GEOMETRY' ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.gpkgTypeToShapeType(
    const _type : String
  ) : TGIS_ShapeType ;
  var
    gtype : String ;
  begin
    gtype := UpperCase( _type ) ;
    if      gtype = 'POINT'              then Result := TGIS_ShapeType.Point
    else if gtype = 'MULTIPOINT'         then Result := TGIS_ShapeType.MultiPoint
    else if gtype = 'LINESTRING'         then Result := TGIS_ShapeType.Arc
    else if gtype = 'MULITLINESTRING'    then Result := TGIS_ShapeType.Arc
    else if gtype = 'POLYGON'            then Result := TGIS_ShapeType.Polygon
    else if gtype = 'MULTIPOLYGON'       then Result := TGIS_ShapeType.Polygon
    else if gtype = 'GEOMETRYCOLLECTION' then Result := TGIS_ShapeType.Complex
    else if gtype = 'GEOMETRY'           then Result := TGIS_ShapeType.Unknown
    else if gtype = 'CIRCULARSTRING'     then Result := TGIS_ShapeType.Arc
    else if gtype = 'COMPOUNDCURVE'      then Result := TGIS_ShapeType.Arc
    else if gtype = 'MULTICURVE'         then Result := TGIS_ShapeType.Arc
    else if gtype = 'CURVEPOLYGON'       then Result := TGIS_ShapeType.Polygon
    else if gtype = 'MULTISURFACE'       then Result := TGIS_ShapeType.Polygon
    else                                      Result := TGIS_ShapeType.Unknown ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.isShapeTypeSubClassOf(
    const _eType      : TGIS_ShapeType ;
    const _eSuperType : TGIS_ShapeType
  ) : Boolean ;
  begin
    if ( _eSuperType = _eType ) or ( _eSuperType = TGIS_ShapeType.Unknown ) then
      Result := True
    else if ( _eSuperType = TGIS_ShapeType.Complex ) then
      Result := True
    else
      Result := False ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.prepareAppendCommand(
    const _table : String
   ) : String ;
  var
    i     : Integer ;
    param : TStringBuilder  ;
    value : TStringBuilder  ;
  begin
    param := TStringBuilder.Create ;
    value := TStringBuilder.Create ;
    try
      value.Append( getCmd(T_SQLGPKG.ID_INSERT_GEO_VALUE ) ) ;
      param.Append( getCmd(T_SQLGPKG.ID_INSERT_GEO_PARAM ) ) ;

      for i := 0 to Fields.Count-1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        value.Append( ',' ) ;
        value.Append( FieldInfo( i ).ExportName ) ;

        param.Append( ',:' ) ;
        param.Append( GisCanonicalSQLName( FieldInfo( i ).ExportName ) ) ;
      end ;

      Result := Format( getCmd(T_SQLGPKG.ID_INSERT ),
                        [_table, value.ToString, param.ToString]
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.prepareUpdateCommand(
    const _table  : String ;
    const _column : String ;
    const _shp    : TGIS_Shape ;
    const _skipGeom : Boolean
  ) : String ;
  var
    i     : Integer ;
    param : TStringBuilder  ;
  begin
    param := TStringBuilder.Create ;
    try
      if not _skipGeom then
        param.Append( getCmd(T_SQLGPKG.ID_UPDATE_GEO ) ) ;

      for i := 0 to Fields.Count - 1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;
        if not _shp.IsFieldModifiedEx( i ) then continue ;

        if (param.Length > 0) then
          param.Append( ',' ) ;
        param.Append( safeName(FieldInfo(i).ExportName) ) ;
        param.Append( '=:' ) ;
        param.Append( GisCanonicalSQLName( FieldInfo(i).ExportName ) ) ;
      end ;

      Result := Format( getCmd(T_SQLGPKG.ID_UPDATE ),
                        [_table, param.ToString, _column, _column, '']
                      ) ;
    finally
      FreeObject( param ) ;
    end ;
  end ;


  procedure TGIS_LayerSqlGpkgAbstract.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
   ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table ) )
    else
      oGisDb.sqlTableAppend( _id, '' )
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.sqlTableOpenWrite(
     const _id     : Integer ;
     const _table  : String  ;
     const _shp    : TGIS_Shape ;
     const _uidCol : String  ;
     const _uidVal : TGIS_Uid ;
     const _skipGeom : Boolean
    ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableOpenWrite( _id, prepareUpdateCommand( _table, _uidCol, _shp, _skipGeom ) )
    else
      oGisDb.sqlTableOpenWrite( _id, '' ) ;

    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String ;
    const _shp  : TGIS_Shape
  ) ;
  var
    store : TBytes ;
  begin
    store := gpkgGeometryFromShp( _shp ) ;
    oGisDb.sqlTableSetGeometry( _id, _name, store, nil );
  end ;



  function TGIS_LayerSqlGpkgAbstract.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ), _cursor
                                             );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor), _cursor ) ;
  end ;



  procedure TGIS_LayerSqlGpkgAbstract.macroConnect ;
  begin
    initializeConnect( True ) ;

    try
      oGisDb.sqlConnect( oGisDb.sqlBaseFolder(self), FSQLParameters );

      macroAttachFunctions ;
    except
      on e : Exception do begin
        if IsStringEmpty( Path ) then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                                       Format( '%s; %s', [ Table, e.Message ] ),
                                       0
                                      )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                                       Format( '%s; %s',
                                               [GetSafeSQLPath(Path), e.Message]
                                              ),
                                       0
                                      ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroDisconnect ;
  var
    i : Integer ;
  begin
    // guarantee connection closing
    for i := 0 to BUILTIN_CURSORS - 1 do
      oGisDb.sqlQueryClose(i) ;
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlDisconnect ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroUpdateStart ;
  begin
    lastUid := -1 ;
    oGisDb.sqlTransactGlobalUpdateStart ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroUpdateEnd ;
  begin
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTransactGlobalUpdateCommit ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroBuildRtree ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      macroSpatialIndexCreate ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroMasterCreate ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_PRAGMA_APPLICATION_ID ) ) ;

      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_SPATIAL_REF_SYS ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_CARTESIAN ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_UNDEFINED_GEOGRAPHIC ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS_WGS84 ) ) ;
      except
      end ;

      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_CONTENTS ) ) ;
      except
      end ;

      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_GEOMETRY_COLUMNS ) ) ;
      except
      end ;

      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_EXTENSIONS ) ) ;
      except
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroMasterMetadataCreate ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_DATA_COLUMNS ) ) ;
      except
      end ;
      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_DATA_COLUMN_CONSTRAINTS ) ) ;
      except
      end ;

      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_METADATA ) ) ;
      except
      end ;
      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_GPKG_METADATA_REFERENCE ) ) ;
      except
      end ;

      try
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_INSERT ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_MD_SCOPE_UPDATE ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_INSERT ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_REFERENCE_SCOPE_UPDATE ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_INSERT ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_COLUMN_NAME_UPDATE ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_INSERT ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_ROW_ID_VALUE_UPDATE ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_INSERT ) ) ;
        oGisDb.sqlExec( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_GPKG_METADATA_REFERENCE_TIMESTAMP_UPDATE ) ) ;
      except
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroSpatialIndexCreate ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      try
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_VIRTUAL_TABLE_RTREE ),
                                [ Table, getCmdGEOMETRY ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_INSERT_RTREE_GPKG_EXTENSIONS ),
                                [ Table, getCmdGEOMETRY ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_INSERT_REPLACE_INTO_RTREE ),
                                [ Table, getCmdGEOMETRY, getCmdGEOUID(0),
                                  getCmdGEOMETRY, getCmdGEOMETRY, getCmdGEOMETRY,
                                  getCmdGEOMETRY, Table
                          ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_INSERT ),
                                [ Table, getCmdGEOMETRY, Table,
                                  getCmdGEOMETRY, getCmdGEOMETRY,
                                  Table, getCmdGEOMETRY, getCmdGEOUID(0),
                                  getCmdGEOMETRY, getCmdGEOMETRY,
                                  getCmdGEOMETRY, getCmdGEOMETRY
                          ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE1 ),
                                [ Table, getCmdGEOMETRY, getCmdGEOMETRY, Table,
                                   getCmdGEOUID(0), getCmdGEOUID(0),
                                   getCmdGEOMETRY, getCmdGEOMETRY,
                                   Table, getCmdGEOMETRY,
                                   getCmdGEOUID(0),
                                   getCmdGEOMETRY, getCmdGEOMETRY,
                                   getCmdGEOMETRY, getCmdGEOMETRY
                          ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE2 ),
                                [ Table, getCmdGEOMETRY, getCmdGEOMETRY, Table,
                                   getCmdGEOUID(0), getCmdGEOUID(0),
                                   getCmdGEOMETRY, getCmdGEOMETRY,
                                   Table, getCmdGEOMETRY, getCmdGEOUID(0)
                          ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE3 ),
                                [ Table, getCmdGEOMETRY, getCmdGEOMETRY, Table,
                                   getCmdGEOUID(0), getCmdGEOUID(0),
                                   getCmdGEOMETRY, getCmdGEOMETRY,
                                   Table, getCmdGEOMETRY, getCmdGEOUID(0),
                                   Table, getCmdGEOMETRY,
                                   getCmdGEOUID(0),
                                   getCmdGEOMETRY, getCmdGEOMETRY,
                                   getCmdGEOMETRY, getCmdGEOMETRY
                          ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_UPDATE4 ),
                                [ Table, getCmdGEOMETRY, Table,
                                   getCmdGEOUID(0), getCmdGEOUID(0),
                                   getCmdGEOMETRY, getCmdGEOMETRY,
                                   Table, getCmdGEOMETRY, getCmdGEOUID(0),
                                   getCmdGEOUID(0)
                          ]
                               )
                       ) ;
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TRIGGER_RTREE_DELETE ),
                                [ Table, getCmdGEOMETRY, Table,
                                   getCmdGEOMETRY,
                                   Table, getCmdGEOMETRY, getCmdGEOUID(0)
                          ]
                               )
                       ) ;
      except
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;


  procedure TGIS_LayerSqlGpkgAbstract.macroMasterUpdate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _name   : String         ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    hasz  : Integer ;
    hasm  : Integer ;
    gtype : String ;
  begin
    if IsReadOnly then exit ;

    // guarantee connection closing
    oGisDb.sqlQueryClose(0) ;
    oGisDb.sqlTableClose( 0 ) ;

    oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_UPDATE_GPKG_CONTENTS ),
                             [ identifier, description,
                               DotFloatToStr( _extent.XMin ),
                               DotFloatToStr( _extent.YMin ),
                               DotFloatToStr( _extent.XMax ),
                               DotFloatToStr( _extent.YMax ),
                               Table
                       ]
                           )
                    ) ;

    gtype := shapeTypeToGpkgType( _type ) ;

    if (_dim = TGIS_DimensionType.XYZ) or (_dim = TGIS_DimensionType.XYZM) then
      hasz := 1
    else
      hasz := 0 ;
    if (_dim = TGIS_DimensionType.XYM) or (_dim = TGIS_DimensionType.XYZM) then
      hasm := 1
    else
      hasm := 0 ;

    oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_UPDATE_GPKG_GEOMETRY_COLUMNS ),
                             [ gtype, hasz, hasm, Table ]
                           )
                   ) ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroTableCreate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    gtype : String ;
    hasz  : Integer ;
    hasm  : Integer ;
    srid  : Integer ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      gtype := shapeTypeToGpkgType( _type ) ;
      if (_dim = TGIS_DimensionType.XYZ) or (_dim = TGIS_DimensionType.XYZM) then
        hasz := 1
      else
        hasz := 0 ;
      if (_dim = TGIS_DimensionType.XYM) or (_dim = TGIS_DimensionType.XYZM) then
        hasm := 1
      else
        hasm := 0 ;

      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_CREATE_TABLE_FEATURE ),
                               [ TableGeometry, gtype ]
                             )
                     ) ;
      srid := 0 ;
      if CS.EPSG <> 0 then begin
        oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_SELECT_GPKG_SPATIAL_REF_SYS ),
                                    [ 'EPSG', CS.EPSG  ] ),
                             0
                          ) ;
        try
          if not oGisDb.sqlQueryEof( 0 ) then
            srid := VarToInt32( oGisDb.sqlQueryGetField( 'srs_id', 0 ) )
          else begin
            srid := CS.EPSG ;
            oGisDb.sqlExec( Format( getCmd( T_SQLGPKG.ID_INSERT_GPKG_SPATIAL_REF_SYS  ),
                                   [ CS.FriendlyName, srid, 'EPSG', srid, CS.FullWKT ]
                                  )
                           ) ;
          end ;
        finally
          oGisDb.sqlQueryClose(0) ;
        end
      end ;

      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_INSERT_GPKG_CONTENTS ),
                               [ Table, 'features', Name, Caption, srid ]
                             )
                     ) ;
      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_INSERT_GPKG_GEOMETRY_COLUMNS ),
                               [ Table, getCmdGEOMETRY, gtype, srid, hasz, hasm ]
                             )
                     ) ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroTableAlter(
    const _layer : TGIS_LayerVector
  ) ;
  const
    N_ITER   = 255 ; // maximum number of tries in new name finding
  var
    i        : Integer ;
    cnt      : Integer ;
    fld      : TGIS_FieldInfo ;
    fname    : String  ;
    lname    : Integer ;
    name_len : Integer ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlQueryClose(0) ;
    oGisDb.sqlTableClose( 0 ) ;

    try
      lname := StrToInt( getCmd(T_SQLGPKG.ID_MAX_NAMELENGTH ) ) ;
    except
      lname := 16 ;
    end ;

    _layer.PrepareExportFieldNames( lname, Self <> _layer, False ) ;

    oGisDb.sqlTransactRestructStart ;
    try

      for i:=0 to _layer.Fields.Count - 1 do begin
        fld := _layer.FieldInfo( i ) ;

        fname    := fld.ExportName ;
        name_len := Min( length( fname ) + 2 , lname ) ;

        if fld.Temporary then continue ;

        if     fld.Deleted then // delete field
        begin
          try
            oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_DROP_COLUMN ),
                             [ TableGeometry, fname ]
                           )
                   ) ;
          except
            // name can not exist
          end ;
        end
        else if ( not fld.Saved ) or ( _layer <> Self ) then // create a field
        begin
          for cnt := 0 to N_ITER do begin
            try
              case fld.FieldType of
                TGIS_FieldType.String :
                   oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_MEMO ),
                                    [ TableGeometry, safeName(fname) ]
                                  )
                          ) ;
                TGIS_FieldType.Date :
                   oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_DATE ),
                                    [ TableGeometry, safeName(fname) ]
                                  )
                          ) ;
                TGIS_FieldType.Boolean :
                   oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_BOOLEAN ),
                                    [ TableGeometry, safeName(fname) ]
                                  )
                          ) ;
                TGIS_FieldType.Number :
                 begin
                   if fld.NewDecimal = 0 then begin
                      if fld.Width <= 9 then
                        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_INTEGER ),
                                        [ TableGeometry, safeName(fname) ]
                                        )
                                )
                      else
                        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_BIGINT ),
                                        [ TableGeometry, safeName(fname) ]
                                       )
                                ) ;
                   end
                   else
                     oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_FLOAT ),
                                     [ TableGeometry, safeName(fname) ]
                                    )
                            ) ;
                 end ;
                TGIS_FieldType.Float :
                     oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_ALTER_ADD_FLOAT ),
                                      [ TableGeometry, safeName(fname) ]
                                    )
                            ) ;
                else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
              end ;

              break ;

            except
              fname := Format( '%s%.2x',
                               [ Copy( fname, StringFirst, name_len - 2 ), cnt ]
                             ) ;
              if cnt = N_ITER then raise ;
            end ;
          end ;

            fld.Saved      := True  ;
            fld.ExportName := fname ;
            fld.Name       := fld.ExportName ;
            fld.Width      := fld.NewWidth   ;
            fld.Decimal    := fld.NewDecimal ;
        end
        else begin
                if fld.NewName <> fld.Name then begin // rename a files
                  // column renaming is not implemented on most databases
                  fld.NewName    := fld.Name ;
                  fld.ExportName := fld.Name ;
                end ;
             end ;
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroTableDrop ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      try
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_DROPTABLE ), [ TableGeometry ] ) ) ;
      except
        // can not exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_DROPTABLE ), [ TableIndex ] ) ) ;
      except
        // can not exist
      end ;

      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_DELETE_FROM_GPKG_GEOMETRY_COLUMNS),
                              [ Table ]
                             )
                    ) ;
      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_DELETE_FROM_GPKG_CONTENTS),
                              [ Table ]
                             )
                    ) ;
      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_DELETE_FROM_GPKG_EXTENSIONS),
                              [ Table, getCmdGEOMETRY ]
                             )
                    ) ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroTableSetField(
    const _id     : Integer ;
    const _name   : String ;
    const _val    : Variant
  ) ;
  var
    i : Integer ;
  begin
    if IsReadOnly then exit ;

    try
      i := FindField( _name ) ;
      // field from layer
      if ( i >= 0 ) and ( not FieldInfo( i ).FileFormat ) then
        oGisDb.sqlTableSetField( _id, GisCanonicalSQLName( FieldInfo(i).ExportName ), _val,
                                 FieldInfo(i).NewWidth )
      else // internal field
        oGisDb.sqlTableSetField( _id, _name, _val, 1 );
    except
      // maybe field name was changed by someone else.....
      assert( False, 'Field ''' + _name + ''' does not exists.' ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, True, 0 ) ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroAddField(
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

  procedure TGIS_LayerSqlGpkgAbstract.macroAddField(
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

    if CompareText( _name, _uidname ) = 0 then begin
      if Fields.Count > 0 then begin
        with FieldInfo( Fields.Count -1 ) do begin
          FileFormat  := True ;
          {$IFDEF OXYGENE}
            &ReadOnly := True ;
          {$ELSE}
            ReadOnly  := True ;
          {$ENDIF}
          Hidden      := True ;
          IsUID       := True ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroQueryStructure ;
  var
    ipk      : Integer ;
    uid_name : String ;
  begin
    Fields.Clear ;

    uid_name := '' ;
    oGisDb.sqlQueryOpen( Format(getCmd(T_SQLGPKG.ID_SELECT_PRAGMA_INFO), [Table]), 0 ) ;
    while not oGisDb.sqlQueryEof(0) do begin
      ipk := VarToInt32( oGisDb.sqlQueryGetField('pk', 0) ) ;
      if ipk = 1 then begin
        uid_name := VarToString( oGisDb.sqlQueryGetField('name', 0) ) ;
        break ;
      end;
      oGisDb.sqlQueryMoveNext(0);
    end ;
    oGisDb.sqlQueryClose(0) ;

    if not IsStringEmpty( uid_name ) then begin
      updateDialectList( 'UID', uid_name ) ;
      prepareCommandList ;
    end ;

    // try to return en empty set to save the time
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_SELECT_TABLE_WHERE ),
                                 [ safeName( ViewFeatures ),
                                  Format( '%s=0', [ getCmd(T_SQLGPKG.ID_UID) ] )
                           ]
                               ), 0
                       ) ;
    {$IFNDEF OXYGENE}
      oGisDb.sqlQueryStructure( ViewFeatures, getCmd(T_SQLGPKG.ID_UID),  macroAddField );
    {$ELSE}
      oGisDb.sqlQueryStructure( ViewFeatures, getCmd(T_SQLGPKG.ID_UID), @macroAddField );
    {$ENDIF}
    oGisDb.sqlQueryClose(0) ;

    ReadFieldRules ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroReadLayerMetadata ;
  var
    ext   : TGIS_Extent ;
    srid  : Integer ;
    geom_column_name : String ;
    geom_type_name : String ;
    z, m : Integer ;
    has_ext : Boolean ;
  begin
    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_SELECT_GPKG_CONTENTS ),
                                 [ Table ]
                               ), 0
                        ) ;
    try
      if oGisDb.sqlQueryEof(0) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), Table, 0 ) ;

      ext.XMin := VarToDouble( oGisDb.sqlQueryGetField('min_x', 0) ) ;
      ext.YMin := VarToDouble( oGisDb.sqlQueryGetField('min_y', 0) ) ;
      ext.XMax := VarToDouble( oGisDb.sqlQueryGetField('max_x', 0) ) ;
      ext.YMax := VarToDouble( oGisDb.sqlQueryGetField('max_y', 0) ) ;

      srid        := VarToInt32( oGisDb.sqlQueryGetField('srs_id', 0) ) ;
      identifier  := VarToString( oGisDb.sqlQueryGetField('identifier', 0) ) ;
      description := VarToString( oGisDb.sqlQueryGetField('description', 0) ) ;

    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_SELECT_GPKG_GEOMETRY_COLUMNS ),
                                 [ Table ]
                               ), 0
                        ) ;
    try
      if oGisDb.sqlQueryEof(0) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), Table, 0 ) ;

      geom_column_name  := VarToString( oGisDb.sqlQueryGetField('column_name', 0) ) ;
      geom_type_name    := VarToString( oGisDb.sqlQueryGetField('geometry_type_name', 0) ) ;

      z := VarToInt32( oGisDb.sqlQueryGetField('z', 0) ) ;
      m := VarToInt32( oGisDb.sqlQueryGetField('m', 0) ) ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    if not GisIsNoWorld( ext ) and
       not GisIsSameExtent( ext, GisExtent( 0, 0, 0, 0 ) ) then
      Extent := ext
    else
      Extent := GisNoWorld ;

    SetCSByEPSG( srid ) ;

    if CS is TGIS_CSUnknownCoordinateSystem then begin
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_SELECT_GPKG_SPATIAL_REF_SYS ),
                                  [ 'EPSG', srid  ] ),
                           0
                         ) ;
      try
        if not oGisDb.sqlQueryEof( 0 ) then
          SetCSByWKT( VarToString( oGisDb.sqlQueryGetField( 'definition', 0 ) ) ) ;
      finally
        oGisDb.sqlQueryClose(0) ;
      end ;
    end ;

    if (z = 0) and (m = 0) then
      DefaultDimension := TGIS_DimensionType.XY
    else if (z >= 1) and (m = 0) then
      DefaultDimension := TGIS_DimensionType.XYZ
    else if (z = 0) and (m >= 1) then
      DefaultDimension := TGIS_DimensionType.XYM
    else if (z >= 1) and (m >= 1) then
      DefaultDimension := TGIS_DimensionType.XYZM ;

    DefaultShapeType := gpkgTypeToShapeType( geom_type_name ) ;

    updateDialectList( 'GEOM', geom_column_name ) ;
    prepareCommandList ;

    has_ext := False ;
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_CHECK_RTREE ), ['gpkg_extensions'] ),0 ) ;
    try
      if not oGisDb.sqlQueryEof(0) then
        has_ext := True ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end;

    hasRtree := False ;
    if has_ext then begin
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_SELECT_RTREE_GPKG_EXTENSIONS ),
                                   [ Table, getCmdGEOMETRY ]
                                  ),
                           0
                          ) ;
      try
        if not oGisDb.sqlQueryEof(0) then
          hasRtree := True ;
      finally
        oGisDb.sqlQueryClose(0) ;
      end;
    end ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLGPKG.ID_CHECK_RTREE ),
                                [Format( 'rtree_%s_%s', [ Table, getCmdGEOMETRY ] )] ),0 ) ;
    try
      if not oGisDb.sqlQueryEof(0) then
        hasRtree := True ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end;

  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroShapeDelete(
    const _uid  : TGIS_Uid
  ) ;
  begin
    if IsReadOnly then exit ;

    try
      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_DELETESHAPE ),
                       [ TableGeometry, _uid ]
                     )
             ) ;
    except
    end ;

  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroShapeUpdate(
    const _shp : TGIS_Shape ;
    const _import  : Boolean
  ) ;
  var
    skipfields : Boolean        ;
    skipgeom   : Boolean        ;
    i          : Integer        ;
    fldinfo    : TGIS_FieldInfo ;
    fld        : Variant        ;
    uid        : TGIS_Uid        ;
    txt        : String         ;
    iloop      : Integer        ;
    guaranted  : Boolean        ;
  begin
    if IsReadOnly then exit ;

    guaranted := False ;

    oGisDb.sqlTransactUpdateStart ;
    try

      if _import then begin
        skipgeom   := False ;

        sqlTableAppend( 0, TableGeometry ) ;
      end
      else begin
        skipgeom   := not _shp.GeometryChanged and not _shp.IsNewShape ;
        skipfields := not _shp.FieldChanged or
                     (not IsStringEmpty(ViewFeatures) and (ViewFeatures<>Table)) ;

        if skipgeom and skipfields then exit ;
        uid := _shp.Uid ;
        sqlTableOpenWrite( 0, TableGeometry, _shp, getCmdGEOUID(0), uid, skipgeom ) ;
      end ;

      iloop := T_SQLGPKG.RETRY_COUNT ;
      while iloop > 0 do begin
        try
          // update geometry
          if _import then begin
            uid := lastUid ;
            if uid < 0 then uid := macroUidNew( guaranted )
                       else inc( uid ) ;
            if not guaranted then
              lastUid := uid ;

            macroTableSetField( 0, getCmd(T_SQLGPKG.ID_UID ), uid ) ;
          end ;

          // do not update feature tables if based on views
          // or fields do not exist
          if ( ViewFeatures = Table ) then begin

            for i := 0 to Fields.Count - 1 do begin
              fldinfo := FieldInfo( i ) ;
              if not (TGIS_FieldFlags.Saveable in fldinfo.Flags) then continue ;

              if not _import and not _shp.IsFieldModifiedEx( i ) then
                continue ;

              fld := _shp.GetFieldEx( fldinfo.NewName ) ;

              case fldinfo.FieldType of
                TGIS_FieldType.String :
                   begin
                     if VarIsNull( fld ) then
                       macroTableSetField(0, fldinfo.NewName, fld )
                     else begin
                       txt := VarToString(fld) ;
                       macroTableSetField( 0, fldinfo.NewName, txt ) ;
                     end ;
                   end ;
                TGIS_FieldType.Date :
                   begin
                     macroTableSetField( 0, fldinfo.NewName, fld ) ;
                   end ;
                TGIS_FieldType.Boolean :
                   begin
                     macroTableSetField( 0, fldinfo.NewName, fld )
                   end ;
                TGIS_FieldType.Number :
                   begin
                     macroTableSetField( 0, fldinfo.NewName, fld ) ;
                   end ;
                TGIS_FieldType.Float :
                   begin
                     macroTableSetField( 0, fldinfo.NewName, fld ) ;
                   end ;
                else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '',0 );
              end ;

            end ;
          end ;

          if not skipgeom then
            sqlTableSetGeometry( 0, getCmdGEOMETRY, _shp ) ;

          oGisDb.sqlTablePost( 0 ) ;

          iloop := 0 ;
        except
          if not guaranted then begin
            dec( iloop ) ;
            if iloop <= 0 then raise ;
            Sleep( GetRandom( T_SQLGPKG.RETRY_INTERVAL ) ) ;
            lastUid := GetLastUid ;
          end
          else
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SQLQUERY ),
                                         oGisDb.GetLastErrorMessage, 0 );
        end ;
      end ;
    finally
      oGisDb.sqlTransactUpdateCommit ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.macroUidLast : TGIS_Uid ;
  var
    v : Variant ;
  begin
    oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECT_MAX_UID ),
                                [ TableGeometry ] ), 0 ) ;
    try
      try
        v := oGisDb.sqlQueryGetField( getCmd(T_SQLGPKG.ID_MAX_UID ), 0 ) ;
        if VarIsNull( v ) then Result := 0
                          else Result := VarToInt64( v ) ;
      except
        Result := 0 ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.macroUidReserve : TGIS_Uid ;
  begin
    Result := -1 ;

    if IsReadOnly then exit ;

    macroUpdateStart ;
    try
      oGisDb.sqlExec( Format( getCmd(T_SQLGPKG.ID_INSERT_DEFAULT_VALUES),
                              [ TableGeometry ]
                             )
                      ) ;
      Result := oGisDb.sqlLastInsertId ;
    finally
      macroUpdateEnd ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.macroUidNew(
    var _guaranted : Boolean
  ) : TGIS_Uid ;
  begin
    _guaranted := False ;
    Result := macroUidLast + 1 ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.macroFetchRecord(
    const _uid     : TGIS_Uid  ;
    const _cursor  : Integer
   ) : Boolean ;
  var
    fetch : Boolean ;  // is fetch necessary?
  begin
    fetch := True ;

    if not oGisDb.sqlQueryEof( _cursor ) then
       if VarToInt64( sqlQueryGetGEOUID( _cursor ) ) = _uid then
          fetch := False ;

    if fetch then begin
      oGisDb.sqlQueryClose( _cursor ) ;
      oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECT_TABLE_WHERE ),
                            [ TableGeometry,
                              Format( '%s=%d', [ getCmd(T_SQLGPKG.ID_UID), _uid ] )
                            ]
                          ), _cursor
                  ) ;
    end ;

    Result := fetch ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.macroAttachFunctions ;
  begin
    try
      oGisDb.sqlCreateFunction( 'ST_MinX'          , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_MinX         ) ;
      oGisDb.sqlCreateFunction( 'ST_MinY'          , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_MinY         ) ;
      oGisDb.sqlCreateFunction( 'ST_MaxX'          , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_MaxX         ) ;
      oGisDb.sqlCreateFunction( 'ST_MaxY'          , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_MaxY         ) ;
      oGisDb.sqlCreateFunction( 'ST_IsEmpty'       , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_IsEmpty      ) ;
      oGisDb.sqlCreateFunction( 'ST_GeometryType'  , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_GeometryType ) ;
      oGisDb.sqlCreateFunction( 'ST_SRID'          , 1, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_ST_SRID         ) ;
      oGisDb.sqlCreateFunction( 'GPKG_IsAssignable', 2, {$IFDEF OXYGENE}@{$ENDIF}sql_gpkg_IsAssignable    ) ;
    except
      // functions may exist or there are active statements
    end;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.updateDialectList(
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

  function TGIS_LayerSqlGpkgAbstract.macroMaxNameLength : Integer ;
  begin
    try
      Result := StrToInt( getCmd(T_SQLGPKG.ID_MAX_NAMELENGTH ) ) ;
    except
      Result := 16 ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.getFieldInternal(
    const _uid      : TGIS_Uid  ;
    const _name     : String ;
    const _cursor   : Integer
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
        try
          if _name = getCmdGEOUID( 0 ) then
            Result := oGisDb.sqlQueryGetField( sqlQueryNameGEOUID(_cursor), _cursor )
          else
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

  function TGIS_LayerSqlGpkgAbstract.bindFieldInternal(
    const _name   : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    lockThread ;
    try
      Result := oGisDb.sqlBindField( _name, _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp    : TGIS_Shape ;
    rebind : Boolean ;
  begin
    lockThread ;
    try
      Result := Unassigned ;

      shp := TGIS_Shape( _shape ) ;

      if not assigned( shp ) then Exit ;

      if shp.IsEditable then begin
        // fetch corresponding record for in-memory shapes
        if macroFetchRecord( shp.Uid, _cursor ) then
          oGisDb.sqlGetBindedField( -1, True, _cursor ); // just rebind
      end ;

      rebind := ( not assigned( cursorSql[_cursor].currShape ) ) or
                ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
      if rebind then
        cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

      Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.setUp ;
  begin
    FUseRTree := False ;

    inherited ;

    macroConnect ;

    macroReadLayerMetadata ;

    macroQueryStructure ;

    fixGEOUID := -1 ;

    FFileInfo := 'TatukGIS SQL GeoPackage Coverage (TTKLS)' ;
  end ;


  function  TGIS_LayerSqlGpkgAbstract.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorSql)  then
        SetLength( cursorSql, Result + 1 ) ;

      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorSql[Result] ) then
          cursorSql[Result] := new T_cursorSql_LayerSqlGpkg ;
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
      cursorSql[Result].currComplex    := TGIS_ShapeComplex.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorSql[Result].disableJoinPrimary := False ;

      if Result > BUILTIN_CURSORS - 1 then
        oGisDb.cursorOpen( Result ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.cursorClose(
    const _cursor      : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorSql[_cursor].curInUse := False ;
      FreeObject( cursorSql[_cursor].currPoint      ) ;
      FreeObject( cursorSql[_cursor].currMultipoint ) ;
      FreeObject( cursorSql[_cursor].currArc        ) ;
      FreeObject( cursorSql[_cursor].currPolygon    ) ;
      FreeObject( cursorSql[_cursor].currComplex    ) ;

      // truncate cursorState at the tail;
      for i := length( cursorSql ) - 1 downto 0 do begin
        if not cursorSql[i].curInUse then begin
          SetLength( cursorSql, i ) ;
        end
        else
          break ;
      end ;

      if ( _cursor > 0 ) and assigned( oGisDb ) then
        oGisDb.cursorClose( _cursor );

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.cursorFirst(
    const _cursor      : Integer     ;
    const _viewerCS    : Boolean     ;
    const _extent      : TGIS_Extent ;
    const _query       : String      ;
    const _shape       : TGIS_Shape  ;
    const _de9im       : String      ;
    const _skipDeleted : Boolean
  ) ;
  var
    sqlquery    : String ;
    order       : String ;
    ex          : TGIS_Extent ;
    useIndex    : Boolean    ;

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

        if assigned( Viewer ) and Viewer.Ref.InPaint and
           IsStringEmpty( JoinPrimary ) and not cursorSql[_cursor].disableJoinPrimary then
          order := ''
        else begin
          if ( cursorSql[_cursor].disableJoinPrimary )             or
             ( IsStringEmpty( JoinPrimary ) )                      or
             ( CompareText( JoinPrimary, GIS_FIELD_UID     ) = 0 ) or
             ( CompareText( JoinPrimary, getCmdGEOUID( 0 ) ) = 0 ) or
             ( CompareText( JoinPrimary, getCmdGEOUID( 1 ) ) = 0 )
          then
            order := ' ORDER BY ' + getCmdGEOUID( 1 )
          else
            order := ' ORDER BY ' + JoinPrimary ;
        end ;

        if GisIsWholeWorld( cursorState[ _cursor ].curRawExtent ) or
           GisIsNoWorld( cursorState[ _cursor ].curRawExtent ) then
          useIndex := False
        else
          useIndex := ( GisExtentArea( cursorState[ _cursor ].curExtent ) <
                        GisExtentArea( Extent ) / 4
                       ) ;

        if GisIsWholeWorld( cursorState[ _cursor ].curRawExtent ) or
            not useIndex then begin

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
            oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECTJOIN_NOEXT ) + order,
                                        [ TableGeometry ]
                                      ), _cursor
                              )
          else
            oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECTJOIN_NOEXT_EX )+ order,
                                        [ TableGeometry, sqlquery ]
                                      ), _cursor
                              ) ;
        end
        else begin
          ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                                 GisExtent( -1E37, -1E37, 1E37, 1E37 )
                               ) ;
          if hasRtree then begin
            if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
              oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECTJOIN_RTREE ) + order,
                                    [ TableGeometry, TableIndex,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax )
                              ]
                                  ) , _cursor
                          )
            else
              oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECTJOIN_RTREE_EX ) + order,
                                    [ TableGeometry,
                                      sqlquery, TableIndex,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax )
                              ]
                                  ), _cursor
                          ) ;
          end
          else begin
            if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
              oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECTJOIN ) + order,
                                    [ TableGeometry,
                                      getCmdGEOMETRY, DotFloatToStr( ex.XMin ),
                                      getCmdGEOMETRY, DotFloatToStr( ex.XMax ),
                                      getCmdGEOMETRY, DotFloatToStr( ex.YMin ),
                                      getCmdGEOMETRY, DotFloatToStr( ex.YMax )
                              ]
                                  ) , _cursor
                          )
            else
              oGisDb.sqlQueryOpen( Format( getCmd(T_SQLGPKG.ID_SELECTJOIN_EX ) + order,
                                    [ TableGeometry,
                                      sqlquery,
                                      getCmdGEOMETRY, DotFloatToStr( ex.XMin ),
                                      getCmdGEOMETRY, DotFloatToStr( ex.XMax ),
                                      getCmdGEOMETRY, DotFloatToStr( ex.YMin ),
                                      getCmdGEOMETRY, DotFloatToStr( ex.YMax )
                              ]
                                  ), _cursor
                          ) ;
          end ;
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

  procedure TGIS_LayerSqlGpkgAbstract.cursorNext(
    const _cursor : Integer
  ) ;
  begin
    lockThread ;
    try
      try
        while not cursorEof( _cursor ) do begin
          if HourglassShake then begin
            cursorSql[ _cursor ].currShape := nil ;
            break ;
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

          if ( cursorSql[_cursor].currShape.IsEditable
               or cursorSql[_cursor].fullSearch
               or assigned( cursorState[_cursor].curRelShape )
               or ( assigned( Viewer )
                    and Viewer.Ref.InPaint
                    and ( cursorSql[_cursor].currShape.SmartSize <> 0 )
                  )
             )
             and ( not isInScope( cursorSql[_cursor].currShape, _cursor ) )
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

  function TGIS_LayerSqlGpkgAbstract.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorSql[_cursor].isEof ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorSql[_cursor].currShape ) then
       Result := cursorSql[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.readShape(
    const _cursor : Integer
  ) ;
  var
    uid : TGIS_Uid      ;
    gpkg : T_StandardGpkgBinary ;
    varStore  : OleVariant  ;
  begin
    cursorSql[_cursor].currShape := nil ;

    uid := VarToInt64( sqlQueryGetGEOUID( _cursor ) ) ;

    varStore := oGisDb.sqlQueryGetGeomVAR( getCmdGEOMETRY, '', _cursor, False ) ;
    try
      cursorSql[_cursor].currShape := gpkg.GpkgToShape( TBytes(varStore) ) ;
    except
      cursorSql[_cursor].currShape := nil ;
    end ;

    if not assigned( cursorSql[_cursor].currShape ) then exit ;

    case cursorSql[_cursor].currShape.ShapeType of
      TGIS_ShapeType.Point :
         begin
           cursorSql[_cursor].currPoint.Recreate     (
              cursorSql[_cursor].currShape, nil, False, uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
         end ;
      TGIS_ShapeType.MultiPoint :
         begin
           cursorSql[_cursor].currMultipoint.Recreate(
              cursorSql[_cursor].currShape, nil, False, uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currMultipoint ;
         end ;
      TGIS_ShapeType.Arc :
         begin
           cursorSql[_cursor].currArc.Recreate       (
              cursorSql[_cursor].currShape, nil, False, uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currArc ;
         end ;
      TGIS_ShapeType.Polygon :
         begin
           cursorSql[_cursor].currPolygon.Recreate   (
              cursorSql[_cursor].currShape, nil, False, uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currPolygon ;
         end ;
      TGIS_ShapeType.Complex :
         begin
           cursorSql[_cursor].currComplex.Recreate   (
              cursorSql[_cursor].currShape, nil, False, uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currComplex ;
         end ;
      else
         cursorSql[_cursor].currShape := nil ;
    end ;

    cursorSql[_cursor].currShape := getEdited( cursorSql[_cursor].currShape ) ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.gpkgGeometryFromShp(
    const _shp : TGIS_Shape
  ) : TBytes ;
  var
    gpkg : T_StandardGpkgBinary ;
  begin
    Result := gpkg.ShapeToGpkg( _shp ) ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_MinX(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    gpkg : T_StandardGpkgBinary ;
  begin
    assert( _nargs = 1 ) ;
    if gpkg.ReadExtent( TBytes(_args[0]) ) then
      Result := gpkg.header.envelope.XMin
    else
      Result := NullVar ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_MaxX(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    gpkg : T_StandardGpkgBinary ;
  begin
    assert( _nargs = 1 ) ;
    if gpkg.ReadExtent( TBytes(_args[0]) ) then
      Result := gpkg.header.envelope.XMax
    else
      Result := NullVar ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_MinY(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    gpkg : T_StandardGpkgBinary ;
  begin
    assert( _nargs = 1 ) ;
    if gpkg.ReadExtent( TBytes(_args[0]) ) then
      Result := gpkg.header.envelope.YMin
    else
      Result := NullVar ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_MaxY(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    gpkg : T_StandardGpkgBinary ;
  begin
    assert( _nargs = 1 ) ;
    if gpkg.ReadExtent( TBytes(_args[0]) ) then
      Result := gpkg.header.envelope.YMax
    else
      Result := NullVar ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_SRID(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    hdr : T_GpkgHeader ;
  begin
    assert( _nargs = 1 ) ;
    hdr.Read( TBytes(_args[0]) ) ;
    Result := hdr.srs_id ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_GeometryType(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    gpkg : T_StandardGpkgBinary ;
    shp  : TGIS_Shape ;
  begin
    assert( _nargs = 1 ) ;
    shp := gpkg.GpkgToShape( TBytes(_args[0]) ) ;
    try
      Result := shapeTypeToGpkgType( shp.ShapeType ) ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_ST_IsEmpty(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    hdr : T_GpkgHeader ;
  begin
    assert( _nargs = 1 ) ;
    hdr.Read( TBytes(_args[0]) ) ;
    Result := hdr.flag_Empty ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.sql_gpkg_IsAssignable(
    _nargs  : Integer ;
    _args   : array of Variant
  ) : Variant ;
  var
    expected : String ;
    actual   : String ;
  begin
    assert( _nargs = 2 ) ;

    expected := VarToString( _args[0] ) ;
    actual   := VarToString( _args[1] ) ;
    Result   := isShapeTypeSubClassOf( gpkgTypeToShapeType(actual),
                                       gpkgTypeToShapeType(expected)
                                      ) ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    lname : String  ;
    gtype : String ;
  begin
    Result := TGIS_LayerInfoList.Create ;
    try
      macroConnect ;
      try
        try
          oGisDb.sqlQueryOpen( getCmd(T_SQLGPKG.ID_SELECT_GPKG_GEOMETRY_COLUMNS_ALL), 0 ) ;
          while not oGisDb.sqlQueryEof(0) do begin
            lname := VarToString( oGisDb.sqlQueryGetFieldById( 0, 0 ) ) ;
            gtype := VarToString( oGisDb.sqlQueryGetFieldById( 1, 0 ) ) ;

            Result.Add(
              TGIS_LayerInfo.Create( lname,
                                     TGIS_RegisteredLayerType.Vector,
                                     gpkgTypeToShapeType(gtype)
                                    )
            ) ;

            oGisDb.sqlQueryMoveNext(0) ;
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
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.GetShape(
    const _uid     : TGIS_Uid  ;
    const _cursor  : Integer
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
                     Format( getCmd(T_SQLGPKG.ID_FILTER_UID_WEAK ),
                             [ GIS_FIELD_UID, _uid,
                               GIS_FIELD_UID, _uid + T_SQLGPKG.GETSHAPE_FETCHED_LIMIT
                             ]
                           ),
                     nil, '', True
                    ) ;
      finally
        cursorSql[_cursor].disableJoinPrimary := False ;
      end ;

      if ( cursorShape( _cursor ) <> nil ) and
         ( cursorShape( _cursor ).Uid = _uid ) then begin
        Result := cursorShape( _cursor ) ;
        exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;


  function TGIS_LayerSqlGpkgAbstract.GetLastUid : TGIS_Uid ;
  var
    tmp : TGIS_Uid ;
  begin
    lockThread ;
    try
      tmp    := macroUidLast ;
      Result := inherited GetLastUid ;
      if tmp > Result then
        Result := tmp ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.GetNewUid : TGIS_Uid ;
  begin
    lockThread ;
    try
      Result := macroUidReserve ;
    finally
      unlockThread ;
    end ;
  end ;


  procedure TGIS_LayerSqlGpkgAbstract.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
   ) ;
  var
    ll : TGIS_LayerSqlGpkgAbstract ;
  begin
    inherited ;

    if IsReadOnly then exit ;

    if not IsStringEmpty( _path ) then begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          ll := TGIS_LayerSqlGpkgAbstract(&Class.forName(Self.Class.Name).getConstructor().newInstance());
        {$ELSE}
          ll := TGIS_LayerSqlGpkgAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
        {$ENDIF}
      {$ELSE}
        ll := TGIS_LayerSqlGpkgAbstractClass( Self.ClassType ).Create ;
      {$ENDIF}
      try
        ll.FSQLParameters.Assign( FSQLParameters );
        ll.Path := _path ;
        ll.CS := CS ;
        ll.Name := Name ;
        ll.Caption := Caption ;
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
            ll.macroMasterMetadataCreate ;
          except
            // master log can already exist
          end ;
          try
            ll.macroTableDrop ;
          except
            // table can not exist
          end ;
          ll.macroTableCreate( _extent, _type, _dim )  ;
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
        try
          macroMasterMetadataCreate ;
        except
          // master can already exist
        end ;

        macroTableCreate( _extent, _type, _dim  )  ;
      finally
        macroDisconnect ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.ImportLayerEx(
     const _layer     : TGIS_LayerVector ;
     const _extent    : TGIS_Extent;
     const _type      : TGIS_ShapeType ;
     const _scope     : String ;
     const _shape     : TGIS_Shape       ;
     const _de9im     : String           ;
     const _truncated : Boolean
   ) ;
  var
    lname      : Integer        ;
    shp        : TGIS_Shape     ;
    shp_tmp    : TGIS_Shape     ;
    shp_type   : TGIS_ShapeType ;
    shp_dim    : TGIS_DimensionType ;
    first      : Boolean        ;
    shape_no   : Cardinal       ;
    end_uid    : TGIS_Uid        ;
    abort      : Boolean        ;
    old_view   : String         ;
    eloop      : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    Extent   := _TGIS_Extent( _layer.Extent ) ;
    shp_type := _type         ;
    first    := True          ;

    identifier  := Name ;
    description := Caption ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;

    old_view := FViewFeatures ;
    try
      try
        macroConnect ;
        macroTableDrop ;
        macroDisconnect ;
      except
        // could be non existing database
      end ;

      ViewFeatures := '' ;
      Build( Path, GisExtent(0,0,0,0), _type, _layer.DefaultDimension ) ;

      shp_dim := _layer.DefaultDimension ;
      SupportedDimensions := _layer.SupportedDimensions ;

      macroConnect ;
      ViewFeatures := '' ;

      Fields.Clear ;
      ImportStructure( _layer ) ;

      try
        lname := StrToInt( getCmd(T_SQLGPKG.ID_MAX_NAMELENGTH ) ) ;
      except
        lname := 16 ;
      end ;

      PrepareExportFieldNames( lname, False, False ) ;
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
                   ( ( shp_tmp.ShapeType=shp_type   ) or
                     ( shp_type=TGIS_ShapeType.Unknown )
                   )
                then begin
                  // calculate extent
                     if first then begin
                       Extent   := _TGIS_Extent( shp_tmp.Extent ) ;
                       first    := False ;
                       shp_dim  := shp_tmp.Dimension ;
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
        end ;
      finally
        // update master table
        macroMasterUpdate( Extent, shp_type, Table, shp_dim ) ;

        macroUpdateEnd ;
        macroEndBatchMode ;
        macroSpatialIndexCreate ;
        FIsModified := False ;
      end ;
    finally

      try
        macroDisconnect ;

        Items.Clear ;
        Fields.Clear ;

        FIsModified := False ;
        FIsOpened   := False ;

        ViewFeatures := old_view ;
        Open ;
      except
      end ;

      RaiseBusyRelease( _layer ) ;
    end ;

  end ;

  procedure TGIS_LayerSqlGpkgAbstract.MergeLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent ;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean ;
    const _restrict  : Boolean
   ) ;
  var
    lname      : Integer        ;
    shp        : TGIS_Shape     ;
    shp_tmp    : TGIS_Shape     ;
    shp_type   : TGIS_ShapeType ;
    first      : Boolean        ;
    shape_no   : Cardinal       ;
    end_uid    : TGIS_Uid        ;
    abort      : Boolean        ;
    old_view   : String         ;
    eloop      : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;
    abort   := False ;

    old_view := FViewFeatures ;

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
        lname := StrToInt( getCmd(T_SQLGPKG.ID_MAX_NAMELENGTH ) ) ;
      except
        lname := 16 ;
      end ;

      ViewFeatures := '' ;

      PrepareExportFieldNames( lname, False, False ) ;
      _layer.PrepareExportFieldNames( lname, Self <> _layer, False ) ;

      MergeStructure( _layer, _restrict, True );
      PrepareExportFieldNames( lname, False, False ) ;

      // Alter table
      macroTableAlter( Self ) ;

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
                     Extent   := _TGIS_Extent( shp_tmp.Extent )  ;
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

            inc( shape_no ) ;
            if shape_no mod 100 = 1 then begin
              abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
              if abort then break ;
            end ;

            inc( shape_no ) ;
          end ;
        finally
          FreeObject( eloop ) ;
        end ;
      finally
        // update master table
        macroMasterUpdate( Extent, shp_type, Table, DefaultDimension ) ;

        macroUpdateEnd ;
        FIsModified := False ;
      end ;
    finally
      ViewFeatures := old_view ;

      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.ReStructure ;
  begin
    macroQueryStructure ;
  end ;

  function TGIS_LayerSqlGpkgAbstract.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 1 ;
    end ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited;
    oGisDb.sqlQueryClose(0);
    oGisDb.sqlTableClose(0) ;
  end ;

  procedure TGIS_LayerSqlGpkgAbstract.SaveData  ;
  var
    i        : Integer    ;
    shp      : TGIS_Shape ;
    first    : Boolean    ;
    shape_no : Cardinal   ;
    end_uid  : Integer    ;
    abort    : Boolean    ;
    done     : Boolean    ;
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
        if not oGisDb.IsSqlite then
           macroConnect ;

      // Alter table
         macroTableAlter( Self ) ;

      // update all items
         first := True ;

      ExportStructureToFLD ;

      done := False ;
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
               if GisIsNoWorld( Extent ) or GisIsWholeWorld( Extent ) then
                 Extent := _TGIS_Extent( shp.Extent ) ;
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
        done := True ;
      finally
        if done then begin
          // update master table
         if not first then
           macroMasterUpdate( Extent, DefaultShapeType, Table, DefaultDimension ) ;

         Items.Clear ;
        end ;
        macroUpdateEnd ;
        macroBuildRtree ;

        macroQueryStructure ;
        FIsModified := False ;

        for i := 0 to BUILTIN_CURSORS - 1 do begin
          prepareCandidates(i);
          cursorSql[i].currShape := nil ;
          // close active queries to unlock the db
          if oGisDb.IsSqlite then
            oGisDb.sqlQueryClose(i) ;
        end ;
      end ;

    finally
      RaiseBusyRelease( Self ) ;
    end ;

    inherited ;
  end ;


{==================================== END =====================================}

end.

