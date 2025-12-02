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
  Encapsulation of a OpenGIS SQL file access.

  Currently support: MS Jet (Access), MS SQL Server, Interbase, Firebird,
  MySQL, DB2, Sybase, PostgreSql, BlackFishSql, Informix, Oracle, Advantage,
  Sqlite and SapDB dialects.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlOgis ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlOgis"'}
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
    T_cursorSql_LayerSqlOgis nested in TGIS_LayerSqlOgisAbstract = public {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
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
        ///   Eof marker.
        /// </summary>
        isEof : Boolean ;

        /// <summary>
        ///   If True the ordering by JoinPrimary code should be disabled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;

        /// <summary>
        ///   If True, then attribute query should be done on user side.
        /// </summary>
        fullSearch : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPoint : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currArc : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPolygon : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currComplex    : TGIS_ShapeComplex ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of abstract OpenGIS SQL layer.
  /// </summary>
  TGIS_LayerSqlOgisAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                              class( TGIS_LayerVectorSqlAbstract )

    protected // other protected variables
    {$IFDEF OXYGENE}
      /// <summary>
      ///  Cursor state. For internal use of TatukGIS.
      /// </summary>
      cursorSql : array of T_cursorSql_LayerSqlOgis ;
    {$ELSE}
      /// <summary>
      ///  Cursor state.
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
        ///   Eof marker.
        /// </summary>
        isEof : Boolean ;

        /// <summary>
        ///   If True the ordering by JoinPrimary code should be disabled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;

        /// <summary>
        ///   If True, then attribute query should be done on user side.
        /// </summary>
        fullSearch : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPoint : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currArc : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPolygon : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currComplex    : TGIS_ShapeComplex ;
      end ;
    {$ENDIF}

        /// <summary>
        ///   Geometry column name
        /// </summary>
        nameColumnGeometry : String ;

        /// <summary>
        ///   Features table name
        /// </summary>
        nameTableFeatures : String ;

        /// <summary>
        ///   Geometry table name
        /// </summary>
        nameTableGeometry : String ;

        /// <summary>
        ///  Maximum number of coordinates column
        /// </summary>
        valueMaxPPR : Integer ;

        /// <summary>
        ///   UID name fixup (GEO.UID or UID) for various db drivers.
        /// </summary>
        fixGEOUID : Integer ;

        /// <summary>
        ///   True if blob version was used.
        /// </summary>
        isBlob : Boolean ;

        /// <summary>
        ///   True if wkt version was used.
        /// </summary>
        isWkt  : Boolean ;

        /// <summary>
        ///   True if short SPATIAL_REFERENCE_SYS should be used used istead of
        ///   SPATIAL_REFERENCE_SYSTEMS
        /// </summary>
        isSRSshort : Boolean ;

        /// <summary>
        ///   Recent calculated value of last uid.
        /// </summary>
        lastUid : TGIS_Uid ;

        /// <summary>
        ///   Recent calculated value of last new uid.
        /// </summary>
        lastNewUid      : TGIS_Uid ;

        /// <summary>
        ///   Parsed catalog name.
        /// </summary>
        strCatalog : String ;

        /// <summary>
        ///   Parsed schema name.
        /// </summary>
        strSchema  : String ;

        /// <summary>
        ///   Parsed table name.
        /// </summary>
        strName    : String ;

        /// <summary>
        ///   Parsed table name.
        /// </summary>
        SridId     : Integer ;

        /// <summary>
        ///   Binded X1
        /// </summary>
        fldX1offset  : Integer ;

        /// <summary>
        ///   Can unchanged fields be skipped upon saving.
        /// </summary>
        canSkipField    : Boolean ;

        /// <summary>
        ///   Registered layer shape type.
        /// </summary>
        layerstp        : TGIS_ShapeType ;
    protected // properties access routine

      function  fget_TableMaster      : String ;
      function  fget_TableGeometry    : String ;
      function  fget_TableFeatures    : String ;
      function  fget_ViewFeatures     : String ; override;

    protected

      /// <summary>
      ///   Read a shape from a SQL query and recreate it.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor identifier
      /// </param>
      procedure readShape           ( const _cursor  : Integer
                                     ) ;

      /// <summary>
      ///   Copy list from resources into the internal list of commands.
      /// </summary>
      procedure prepareCommandList  ; override;

      /// <summary>
      ///   SQL command with GEOUID column name.
      /// </summary>
      /// <param name="_id">
      ///   if 0, then returned 'UID'; if 1, then returned GEO.UID
      /// </param>
      /// <returns>
      ///  SQL command with GEOUID column name.
      /// </returns>
      function  getCmdGEOUID        ( const _id      : Integer
                                    ) : String ;

      /// <summary>
      ///   Return a SQL command with SHAPETYPE column name.
      /// </summary>
      /// <returns>
      ///  SQL command with SHAPETYPE column name.
      /// </returns>
      function  getCmdSHAPETYPE     : String ;

      /// <summary>
      ///   Return a SQL command with XMIN column name.
      /// </summary>
      /// <returns>
      ///  SQL command with XMIN column name.
      /// </returns>
      function  getCmdXMIN          : String ;

      /// <summary>
      ///   SQL command with YMIN column name.
      /// </summary>
      /// <returns>
      ///  SQL command with YMIN column name.
      /// </returns>
      function  getCmdYMIN          : String ;

      /// <summary>
      ///   Return a SQL command with GEOMETRY column name.
      /// </summary>
      /// <returns>
      ///  SQL command with GEOMETRY column name.
      /// </returns>
      function  getCmdGEOMETRY      : String ;

      /// <summary>
      ///   Return a SQL command with GEOMETRY WKT column name.
      /// </summary>
      /// <returns>
      ///  SQL command with GEOMETRY WKT column name.
      /// </returns>
      function  getCmdGEOMETRYWkt   : String ;

      /// <summary>
      ///   Return a SQL command with SRS table name.
      /// </summary>
      /// <returns>
      ///  SQL command with SRS table name.
      /// </returns>
      function  getCmdSRSTable      : String ;

      /// <summary>
      ///   Return a table with schema.
      /// </summary>
      /// <param name="_table">
      ///  Table name.
      /// </param>
      /// <returns>
      ///  Table with schema.
      /// </returns>
      function  getTableWithSchema  ( const _table   : String
                                     )  : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///  SQL parametrized append command.
      /// </returns>
      function  prepareAppendCommand( const _table   : String
                                    ) : String ;

      /// <summary>
      ///   Prepare list of fields in a form "field=value".
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <param name="_params">
      ///   list of parameters
      /// </param>
      procedure prepareParamsCommand  ( const _table  : String ;
                                        const _params : TStrings
                                      ) ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///  SQL parametrized append command.
      /// </returns>
      function  prepareAppendParams ( const _table   : String
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
      ///  SQL parametrized update command.
      /// </returns>
      function  prepareUpdateCommand( const _table   : String ;
                                      const _column  : String
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
      ///  SQL parametrized update command.
      /// </returns>
      function  prepareUpdateParams( const _table   : String ;
                                      const _column  : String
                                    ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <param name="_params">
      ///   table parameters to be inserted
      /// </param>
      /// <returns>
      ///   parametrized append command
      /// </returns>
      function  prepareAppendCommandEx( const _table  : String ;
                                        const _params : TStrings
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized update command.
      /// </summary>
      /// <param name="_table">
      ///   table to be updated
      /// </param>
      /// <param name="_params">
      ///   table parameters to be updated
      /// </param>
      /// <returns>
      ///   parametrized update command
      /// </returns>
      function  prepareUpdateCommandEx( const _table  : String ;
                                        const _params : TStrings
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      procedure macroAppendParams    ( const _table : String
                                      )  ;

      /// <summary>
      ///   Return a SQL parametrized update command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      procedure macroUpdateParams    ( const _table : String
                                      )  ;

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
      ///  SQL select command.
      /// </returns>
      function  prepareSelectCommand( const _table   : String ;
                                      const _filter  : String
                                    ) : String ;

      /// <summary>
      ///   Return a UID=_uid filter.
      /// </summary>
      /// <param name="_uid">
      ///   uid value used to build filter
      /// </param>
      /// <returns>
      ///  UID=_uid filter.
      /// </returns>
      function  prepareFilterUid    ( const _uid     : TGIS_Uid
                                    ) : String ;

      /// <summary>
      ///   Return a master table filter.
      /// </summary>
      /// <param name="_catalog">
      ///   catalog name
      /// </param>
      /// <param name="_schema">
      ///   schema name
      /// </param>
      /// <param name="_name">
      ///   table name
      /// </param>
      /// <param name="_usealias">
      ///   if true query should use GC alias like GC.FIELD_NAME
      /// </param>
      /// <returns>
      ///  Master table filter.
      /// </returns>
      function  prepareMasterQuery  ( const _catalog : String ;
                                      const _schema  : String ;
                                      const _name    : String ;
                                      const _usealias : Boolean
                                    ) : String ;

      /// <summary>
      ///   Open the query. Active query will be closed.
      /// </summary>
      /// <param name="_query">
      ///   query to be executed
      /// </param>
      /// <param name="_cursor">
      ///   cursor identifier
      /// </param>
      procedure sqlQueryOpen        ( const _query    : String ;
                                      const _cursor   : Integer
                                    ) ;

      /// <summary>
      ///   Get a geometry from the query.
      /// </summary>
      /// <param name="_uid">
      ///   to be assigned
      /// </param>
      /// <param name="_shapetype">
      ///   type of the shape to be read
      /// </param>
      /// <param name="_cursor">
      ///   cursor identifier
      /// </param>
      procedure sqlQueryGetGeometry ( const _uid       : TGIS_Uid ;
                                      const _shapetype : TGIS_ShapeType ;
                                      const _cursor    : Integer
                                    ) ;

      /// <summary>
      /// Get a GEO.UID field from the query.
      /// </summary>
      /// <param name="_cursor">
      ///  Cursor identifier.
      /// </param>
      /// <returns>
      ///  GEO.UID field as a String.
      /// </returns>
      function  sqlQueryNameGEOUID  ( const _cursor    : Integer
                                     ): String ; virtual; abstract;

      /// <summary>
      /// Get a GEO.UID field from the query.
      /// </summary>
      /// <param name="_cursor">
      ///  Cursor identifier.
      /// </param>
      /// <returns>
      ///  GEO.UID field as a Variant.
      /// </returns>
      function  sqlQueryGetGEOUID   ( const _cursor    : Integer
                                     ): Variant ; virtual; abstract;

      /// <summary>
      /// Append a new record into the table.
      /// </summary>
      /// <param name="_id">
      ///  Table id.
      /// </param>
      /// <param name="_table">
      /// Table name.
      /// </param>
      procedure sqlTableAppend      ( const _id      : Integer ;
                                      const _table   : String
                                    ) ; virtual; abstract;
      /// <summary>
      /// Open the table.
      /// </summary>
      /// <param name="_id">
      ///  Table id.
      /// </param>
      /// <param name="_table">
      /// Table name.
      /// </param>
      /// <param name="_uidCol">
      /// Name of the uid column.
      /// </param>
      /// <param name="_uidVal">
      ///  Name of the uid value.
      /// </param>
      procedure sqlTableOpenWrite   ( const _id      : Integer ;
                                      const _table   : String  ;
                                      const _uidCol  : String  ;
                                      const _uidVal  : TGIS_Uid
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Update the table (after any change).
      /// </summary>
      /// <param name="_id">
      ///   ID of the table
      /// </param>
      procedure sqlTablePost          ( const _id     : Integer
                                      ) ; virtual;

      /// <summary>
      ///   Set a geometry to the table.
      /// </summary>
      /// <param name="_id">
      ///   table id
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_shp">
      ///   shape to be saved
      /// </param>
      procedure sqlTableSetGeometry ( const _id      : Integer ;
                                      const _name    : String  ;
                                      const _shp     : TGIS_Shape
                                    ) ;

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
                                      const _name    : String         ;
                                      const _dim     : TGIS_DimensionType
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
      ///   supported dimension
      /// </param>
      procedure macroTableCreate    ( const _extent  : TGIS_Extent    ;
                                      const _type    : TGIS_ShapeType ;
                                      const _dim    : TGIS_DimensionType
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
      ///   table id
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_val">
      ///   value of the field
      /// </param>
      /// <param name="_force">
      ///   if True, the field must be set as _name
      /// </param>
      procedure macroTableSetField  ( const _id      : Integer ;
                                      const _name    : String  ;
                                      const _val     : Variant ;
                                      const _force   : Boolean
                                    ) ; virtual;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_uidname">
      ///   name of uid field to be add
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
      /// <param name="_uidname">
      ///   name of uid field to be add
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
      ///   name of uid field to be add
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

      /// <summary>
      ///   Macro for querying table for the structure (available fields)
      /// </summary>
      procedure macroQueryStructure   ; virtual;

      /// <summary>
      ///   Macro for deleting a shape from both: geometry and feature table.
      /// </summary>
      /// <param name="_uid">
      ///   uid field to be add
      /// </param>
      procedure macroShapeDelete    ( const _uid     : TGIS_Uid
                                    ) ; virtual;

      /// <inheritdoc/>
      procedure macroShapeUpdate    ( const _shp     : TGIS_Shape ;
                                      const _import  : Boolean
                                    ) ; override;

      /// <inheritdoc/>
      function  macroUidLast        : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for reserving new uid.
      /// </summary>
      /// <returns>
      /// Macro for reserving new uid.
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
      ///  Macro for generating new uid.
      /// </returns>
      function  macroUidNew         ( var   _guaranted : Boolean
                                    ) : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for fetching proper record form the database.
      /// </summary>
      /// <param name="_uid">
      ///   uid of the shape for which corresponding record should be fetched
      /// </param>
      /// <param name="_cursor">
      ///   cursor identifier
      /// </param>
      /// <returns>
      ///  True if record can be fetched.
      /// </returns>
      function  macroFetchRecord      ( const _uid     : TGIS_Uid ;
                                        const _cursor  : Integer
                                      ) : Boolean ; virtual;

      /// <summary>
      ///   Parse config layer name.
      /// </summary>
      procedure parseConfigLayerName ;

      /// <summary>
      ///   Macro for setting shared connection.
      /// </summary>
      /// <param name="_ll">
      ///   layer
      /// </param>
      procedure macroSetSharedConnection( const _ll : TGIS_LayerVector
                                        )  ; virtual;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
    // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      function  getFieldInternal  ( const _uid     : TGIS_Uid  ;
                                    const _name    : String ;
                                    const _cursor  : Integer
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
      procedure doDestroy ; override;

    public

      /// <inheritdoc/>
      constructor Create   ; override;

      /// <summary>
      ///   Build new OGIS layer.
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
      ///   dimension
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Build for details and example. Note that
      ///   existing SQL layer will be deleted.
      /// </remarks>
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

    public  // properties

      /// <summary>
      ///   Name of the master table.
      /// </summary>
      property TableMaster : String read fget_TableMaster ;

      /// <summary>
      ///   Name of the geometry table.
      /// </summary>
      property TableGeometry : String read fget_TableGeometry ;

      /// <summary>
      ///   Name of the features table.
      /// </summary>
      property TableFeatures : String read fget_TableFeatures ;
  end ;
  TGIS_LayerSqlOGISClass = class of TGIS_LayerSqlOgisAbstract ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoGeometryFactory,
    Lider.CG.GIS.GeoDb ;
{$ENDIF}

type
  T_SQLOGIS = class
  public
  const
    // Number of retries for concurrent issues.
    OGIS_RETRY_COUNT    = 9 ;

    // Maximum time of interval between retries in concurrent mode.
    OGIS_RETRY_INTERVAL = 100  ;

    // Maximum number of items fetched in GetShape.
    OGIS_GETSHAPE_FETCHED_LIMIT = 500  ;

  ID_BEGIN = 0                                                         ;

  CREATE_GEOMETRY_COLUMNS_TABLE =
    'CREATE TABLE <#GEOMETRY_COLUMNS#>'                                       +
    '( F_TABLE_CATALOG <#VARCHAR#>(255),'                                     +
    'F_TABLE_SCHEMA <#VARCHAR#>(255),'                                        +
    'F_TABLE_NAME <#VARCHAR#>(255),'                                          +
    'F_GEOMETRY_COLUMN <#VARCHAR#>(255),'                                     +
    'G_TABLE_CATALOG <#VARCHAR#>(255),'                                       +
    'G_TABLE_SCHEMA <#VARCHAR#>(255),'                                        +
    'G_TABLE_NAME <#VARCHAR#>(255),'                                          +
    'STORAGE_TYPE <#INTEGER#>,'                                               +
    'GEOMETRY_TYPE <#INTEGER#>,'                                              +
    'COORD_DIMENSION <#INTEGER#>,'                                            +
    'MAX_PPR <#INTEGER#>,'                                                    +
    'SRID <#INTEGER#> )'                                                       ;
  ID_CREATE_GEOMETRY_COLUMNS_TABLE = ID_BEGIN                  ;

  CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS =
    'CREATE TABLE %s'                                                         +
    '( SRID <#INTEGER#> <#NOT NULL PRIMARY KEY#>,'                            +
    'AUTH_NAME <#VARCHAR#>(255),'                                             +
    'AUTH_SRID <#INTEGER#>,'                                                  +
    'SRTEXT <#VARCHAR(2048)#>)'                                                ;
  ID_CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS =
    ID_CREATE_GEOMETRY_COLUMNS_TABLE + 1                               ;

  CREATE_TABLE_GEOMETRY_BINARY =
    'CREATE TABLE %s '                                                        +
    '( <#GID#> <#INTEGER#> <#NOT NULL PRIMARY KEY#>,'                         +
    '<#XMIN#> <#DOUBLE#>, <#YMIN#> <#DOUBLE#>,'                               +
    '<#XMAX#> <#DOUBLE#>, <#YMAX#> <#DOUBLE#>,'                               +
    '<#WKB_GEOMETRY#> <#BLOB#> )'                                              ;
  ID_CREATE_TABLE_GEOMETRY_BINARY =
    ID_CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS + 1                      ;

  CREATE_TABLE_GEOMETRY_WKT =
    'CREATE TABLE %s '                                                        +
    '( <#GID#> <#INTEGER#> <#NOT NULL PRIMARY KEY#>,'                         +
    '<#XMIN#> <#DOUBLE#>, <#YMIN#> <#DOUBLE#>,'                               +
    '<#XMAX#> <#DOUBLE#>, <#YMAX#> <#DOUBLE#>,'                               +
    '<#WKT_GEOMETRY#> <#CLOB#> )'                                              ;
  ID_CREATE_TABLE_GEOMETRY_WKT =
    ID_CREATE_TABLE_GEOMETRY_BINARY + 1                                ;

  CREATE_TABLE_GEOMETRY_NORMALIZED =
    'CREATE TABLE %s '                                                        +
    '( <#GID#> <#INTEGER#>, '                                                 +
    'ESEQ <#INTEGER#>,'                                                       +
    'ETYPE <#INTEGER#>,'                                                      +
    'SEQ <#INTEGER#>,'                                                        +
    'X1 <#DOUBLE#>, Y1 <#DOUBLE#>,'                                           +
    'X2 <#DOUBLE#>, Y2 <#DOUBLE#>,'                                           +
    'X3 <#DOUBLE#>, Y3 <#DOUBLE#>,'                                           +
    'X4 <#DOUBLE#>, Y4 <#DOUBLE#>,'                                           +
    'X5 <#DOUBLE#>, Y5 <#DOUBLE#>,'                                           +
    'X6 <#DOUBLE#>, Y6 <#DOUBLE#>,'                                           +
    'X7 <#DOUBLE#>, Y7 <#DOUBLE#>,'                                           +
    'X8 <#DOUBLE#>, Y8 <#DOUBLE#>,'                                           +
    'X9 <#DOUBLE#>, Y9 <#DOUBLE#>,'                                           +
    'X10 <#DOUBLE#>, Y10 <#DOUBLE#>,'                                         +
    'X11 <#DOUBLE#>, Y11 <#DOUBLE#>,'                                         +
    'X12 <#DOUBLE#>, Y12 <#DOUBLE#>,'                                         +
    'X13 <#DOUBLE#>, Y13 <#DOUBLE#>,'                                         +
    'X14 <#DOUBLE#>, Y14 <#DOUBLE#>,'                                         +
    'X15 <#DOUBLE#>, Y15 <#DOUBLE#>,'                                         +
    'X16 <#DOUBLE#>, Y16 <#DOUBLE#> )'                                         ;
  ID_CREATE_TABLE_GEOMETRY_NORMALIZED =
    ID_CREATE_TABLE_GEOMETRY_WKT + 1                                   ;

  CREATE_TABLE_FEATURES =
    'CREATE TABLE %s ( <#GID#> <#INTEGER#> <#NOT NULL PRIMARY KEY#> )'         ;
  ID_CREATE_TABLE_FEATURES =
    ID_CREATE_TABLE_GEOMETRY_NORMALIZED + 1                            ;

  DROP_TABLE =
    'DROP TABLE <#IF_EXISTS#> %s'                                              ;
  ID_DROP_TABLE = ID_CREATE_TABLE_FEATURES + 1                 ;

  SELECT_GEOMETRY_COLUMNS_TABLE =
    'SELECT * '                                                               +
    'FROM <#GEOMETRY_COLUMNS#> <#GC#>, '                                      +
    '%s <#SRS#> '                                                             +
    'WHERE %s'                                                                +
    '  AND <#GC#>.<#SRID#>=<#SRS#>.<#SRID#>'                                   ;
  ID_SELECT_GEOMETRY_COLUMNS_TABLE = ID_DROP_TABLE + 1         ;

  SELECT_GEOMETRY_COLUMNS_TABLE_ALL =
    'SELECT <#G_TABLE_NAME#>, <#GEOMETRY_TYPE#> FROM <#GEOMETRY_COLUMNS#> '    ;
  ID_SELECT_GEOMETRY_COLUMNS_TABLE_ALL =
    ID_SELECT_GEOMETRY_COLUMNS_TABLE + 1                               ;

  SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG =
    '<#G_TABLE_CATALOG#>'                                                      ;
  ID_SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG =
    ID_SELECT_GEOMETRY_COLUMNS_TABLE_ALL + 1                           ;

  SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA =
    '<#G_TABLE_SCHEMA#>'                                                       ;
  ID_SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA =
    ID_SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG + 1                       ;

  SELECT_GEOMETRY_COLUMNS_TABLE_NAME =
    '<#G_TABLE_NAME#>'                                                         ;
  ID_SELECT_GEOMETRY_COLUMNS_TABLE_NAME =
    ID_SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA + 1                        ;

  SELECT_GEOMETRY_MAX_GID =
    'SELECT <#MAX#>(<#GID#>) <#AS#> <#MAX_GID#> FROM %s'                       ;
  ID_SELECT_GEOMETRY_MAX_GID =
    ID_SELECT_GEOMETRY_COLUMNS_TABLE_NAME + 1                          ;

  MAX_GID =
    '<#MAX_GID#>'                                                              ;
  ID_MAX_GID =
    ID_SELECT_GEOMETRY_MAX_GID + 1                                     ;

  SELECT_GEOMETRY_EXTENT =
    'SELECT <#MIN#>(<#XMIN#>) <#AS#> <#MIN_XMIN#>,'                            +
    '       <#MIN#>(<#YMIN#>) <#AS#> <#MIN_YMIN#>,'                            +
    '       <#MAX#>(<#XMAX#>) <#AS#> <#MAX_XMAX#>,'                            +
    '       <#MAX#>(<#YMAX#>) <#AS#> <#MAX_YMAX#> FROM %s'                     +
    ' WHERE <#XMAX#> >= <#XMIN#> AND <#YMAX#> >= <#YMIN#>'                     ;
  ID_SELECT_GEOMETRY_EXTENT =
    ID_MAX_GID + 1                                                     ;

  MIN_XMIN =
    '<#MIN_XMIN#>'                                                             ;
    ID_MIN_XMIN = ID_SELECT_GEOMETRY_EXTENT + 1                ;

  MIN_YMIN =
    '<#MIN_YMIN#>'                                                             ;
  ID_MIN_YMIN =
    ID_MIN_XMIN + 1                                                    ;

  MAX_XMAX =
    '<#MAX_XMAX#>'                                                             ;
  ID_MAX_XMAX =
    ID_MIN_YMIN + 1                                                    ;

  MAX_YMAX =
    '<#MAX_YMAX#>'                                                             ;
  ID_MAX_YMAX =
    ID_MAX_XMAX + 1                                                    ;

  SELECT_TABLE_ALL =
    'SELECT * FROM %s'                                                         ;
  ID_SELECT_TABLE_ALL =
    ID_MAX_YMAX + 1                                                    ;

  SELECT_TABLE_WHERE =
    'SELECT * FROM %s WHERE %s'                                                ;
  ID_SELECT_TABLE_WHERE = ID_SELECT_TABLE_ALL + 1              ;

  SELECT_JOIN_UID =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE'                                                                  +
    ' (<#GEO#>.%s=%d)'                                                        +
    ' AND (<#GEO#>.%s=<#FEA#>.%s)'                                             ;
  ID_SELECT_JOIN_UID = ID_SELECT_TABLE_WHERE + 1               ;

  SELECT_JOIN =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE'                                                                  +
    '  <#XMAX#>>%s AND <#XMIN#><%s AND'                                       +
    '  <#YMAX#>>%s AND <#YMIN#><%s %s'                                        +
    ' AND (<#GEO#>.%s=<#FEA#>.%s)'                                            +
    ' %s'                                                                      ;
  ID_SELECT_JOIN = ID_SELECT_JOIN_UID + 1                      ;

  SELECT_JOIN_EX =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE'                                                                  +
    ' (%s)'                                                                   +
    ' AND <#XMAX#>>%s AND <#XMIN#><%s AND'                                    +
    '     <#YMAX#>>%s AND <#YMIN#><%s %s'                                     +
    ' AND (<#GEO#>.%s=<#FEA#>.%s)'                                            +
    ' %s'                                                                      ;
  ID_SELECT_JOIN_EX = ID_SELECT_JOIN + 1                       ;

  SELECT_JOIN_NOEXT =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE %s'                                                               +
    '   (<#GEO#>.%s=<#FEA#>.%s)'                                              +
    ' %s'                                                                      ;
    ID_SELECT_JOIN_NOEXT = ID_SELECT_JOIN_EX + 1               ;

  SELECT_JOIN_NOEXT_EX =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE (%s) AND %s '                                                     +
    '  (<#GEO#>.%s=<#FEA#>.%s)'                                               +
    ' %s'                                                                     ;
  ID_SELECT_JOIN_NOEXT_EX = ID_SELECT_JOIN_NOEXT + 1           ;

  SELECT_JOIN_NORMALIZED_NOEXT =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE %s'                                                               +
    '   (<#GEO#>.%s=<#FEA#>.%s)'                                              +
    ' ORDER BY <#GEO#>.%s, <#GEO#>.ESEQ, <#GEO#>.SEQ'                          ;
  ID_SELECT_JOIN_NORMALIZED_NOEXT =
    ID_SELECT_JOIN_NOEXT_EX + 1                                        ;

  SELECT_JOIN_NORMALIZED_NOEXT_EX =
    'SELECT * FROM %s <#FEA#>, %s <#GEO#>'                                    +
    ' WHERE (%s) AND %s '                                                     +
    '  (<#GEO#>.%s=<#FEA#>.%s)'                                               +
    ' ORDER BY <#GEO#>.%s, <#GEO#>.ESEQ, <#GEO#>.SEQ'                          ;
  ID_SELECT_JOIN_NORMALIZED_NOEXT_EX =
    ID_SELECT_JOIN_NORMALIZED_NOEXT + 1                                ;

  SELECT_MAX_SRID =
    'SELECT <#MAX#>(<#SRID#>) <#AS#> MAX_SRID FROM %s' ;
  ID_SELECT_MAX_SRID =
    ID_SELECT_JOIN_NORMALIZED_NOEXT_EX + 1                             ;

  DELETE_SHAPE =
    'DELETE FROM %s WHERE %s=%d'                                               ;
  ID_DELETE_SHAPE =
    ID_SELECT_MAX_SRID + 1                                             ;

  DELETE_GEOMETRY_COLUMNS_TABLE =
    'DELETE FROM <#GEOMETRY_COLUMNS#> '                                       +
    'WHERE %s'                                                                 ;
  ID_DELETE_GEOMETRY_COLUMNS_TABLE = ID_DELETE_SHAPE + 1       ;

  UPDATE_GEOMETRY_COLUMNS_TABLE =
    'UPDATE <#GEOMETRY_COLUMNS#> '                                            +
    ' SET GEOMETRY_TYPE=%d, COORD_DIMENSION=%d '                              +
    'WHERE %s'                                                                 ;
  ID_UPDATE_GEOMETRY_COLUMNS_TABLE =
    ID_DELETE_GEOMETRY_COLUMNS_TABLE + 1                               ;

  UPDATE_DBX_GEO_BINARY =
    '<#XMIN#>=:<#XMIN#>,<#YMIN#>=:<#YMIN#>,'                                  +
    '<#XMAX#>=:<#XMAX#>,<#YMAX#>=:<#YMAX#>,'                                  +
    '<#WKB_GEOMETRY#>=<#:WKB_GEOMETRY#>'                                       ;
  ID_UPDATE_DBX_GEO_BINARY =
    ID_UPDATE_GEOMETRY_COLUMNS_TABLE + 1                               ;

  UPDATE_DBX_GEO_WKT =
    '<#XMIN#>=:<#XMIN#>,<#YMIN#>=:<#YMIN#>,'                                  +
    '<#XMAX#>=:<#XMAX#>,<#YMAX#>=:<#YMAX#>,'                                  +
    '<#WKT_GEOMETRY#>=<#:WKT_GEOMETRY#>'                                       ;
  ID_UPDATE_DBX_GEO_WKT =
    ID_UPDATE_DBX_GEO_BINARY + 1                                       ;

  UPDATE_DBX_GEO_NORMALIZED =
    'ESEQ=:ESEQ,ETYPE=:ETYPE,SEQ=:SEQ,X1=:X1,Y1=:Y1,X2=:X2,Y2=:Y2,'           +
    'X3=:X3,Y3=:Y3,X4=:X4,Y4=:Y4,X5=:X5,Y5=:Y5,X6=:X6,Y6=:Y6,X7=:X7,'         +
    'Y7=:Y7,X8=:X8,Y8=:Y8,X9=:X9,Y9=:Y9,X10=:X10,Y10=:Y10,X11=:X11,'          +
    'Y11=:Y11,X12=:X12,Y12=:Y12,X13=:X13,Y13=:Y13,X14=:X14,Y14=:Y14,'         +
    'X15=:X15,Y15=:Y15,X16=:X16,Y16=:Y16'                                      ;
  ID_UPDATE_DBX_GEO_NORMALIZED =
    ID_UPDATE_DBX_GEO_WKT + 1                                          ;

  UPDATE_DBX = 'UPDATE %s SET %s WHERE %s=:%s %s'                      ;
  ID_UPDATE_DBX =
    ID_UPDATE_DBX_GEO_NORMALIZED + 1                                   ;

  INSERT_GEOMETRY_COLUMNS_TABLE =
    'INSERT INTO <#GEOMETRY_COLUMNS#>'                                        +
    ' VALUES(''%s'',''%s'',''%s'',''%s'',''%s'',''%s'',''%s'','               +
             '%d,%d,%d,%d,%d)'                                                 ;
  ID_INSERT_GEOMETRY_COLUMNS_TABLE =
    ID_UPDATE_DBX + 1                                                  ;

  INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE =
    'INSERT INTO %s'                                                          +
    ' VALUES(%d,''%s'',%d,''%s'')'                                             ;
  ID_INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE =
    ID_INSERT_GEOMETRY_COLUMNS_TABLE + 1                               ;

  INSERT_DBX_GEO_VALUE_BINARY =
    '<#GID#>,<#XMIN#>,<#YMIN#>,<#XMAX#>,<#YMAX#>,<#WKB_GEOMETRY#>'             ;
    ID_INSERT_DBX_GEO_VALUE_BINARY =
      ID_INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE + 1                    ;

  INSERT_DBX_GEO_VALUE_WKT =
    '<#GID#>,<#XMIN#>,<#YMIN#>,<#XMAX#>,<#YMAX#>,<#WKT_GEOMETRY#>'             ;
    ID_INSERT_DBX_GEO_VALUE_WKT =
      ID_INSERT_DBX_GEO_VALUE_BINARY + 1                               ;

  INSERT_DBX_GEO_VALUE_NORMALIZED =
    '<#GID#>,ESEQ,ETYPE,SEQ,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,X6,Y6,X7,Y7,X8,Y8,' +
    'X9,Y9,X10,Y10,X11,Y11,X12,Y12,X13,Y13,X14,Y14,X15,Y15,X16,Y16'            ;
    ID_INSERT_DBX_GEO_VALUE_NORMALIZED =
      ID_INSERT_DBX_GEO_VALUE_WKT + 1                                  ;

  INSERT_DBX_GEO_PARAM_BINARY =
    ':<#GID#>,:<#XMIN#>,:<#YMIN#>,:<#XMAX#>,:<#YMAX#>,<#:WKB_GEOMETRY#>'       ;
    ID_INSERT_DBX_GEO_PARAM_BINARY =
      ID_INSERT_DBX_GEO_VALUE_NORMALIZED + 1                           ;

  INSERT_DBX_GEO_PARAM_BINARY_TEXT =
    '<#GID#>=#13#10<#XMIN#>=#13#10<#YMIN#>=#13#10<#XMAX#>=#13#10'+
    '<#YMAX#>=#13#10<#WKB_GEOMETRY#>=#13#10'                                  ;
    ID_INSERT_DBX_GEO_PARAM_BINARY_TEXT =
      ID_INSERT_DBX_GEO_PARAM_BINARY + 1                               ;

  INSERT_DBX_GEO_PARAM_WKT =
    ':<#GID#>,:<#XMIN#>,:<#YMIN#>,:<#XMAX#>,:<#YMAX#>,<#:WKT_GEOMETRY#>'       ;
    ID_INSERT_DBX_GEO_PARAM_WKT =
      ID_INSERT_DBX_GEO_PARAM_BINARY_TEXT + 1                          ;

  INSERT_DBX_GEO_PARAM_WKT_TEXT =
    '<#GID#>=#13#10<#XMIN#>=#13#10<#YMIN#>=#13#10<#XMAX#>=#13#10'+
    '<#YMAX#>=#13#10<#WKT_GEOMETRY#>=#13#10'                                   ;
    ID_INSERT_DBX_GEO_PARAM_WKT_TEXT =
      ID_INSERT_DBX_GEO_PARAM_WKT + 1                                  ;

  INSERT_DBX_GEO_PARAM_NORMALIZED =
    ':<#GID#>,:ESEQ,:ETYPE,:SEQ,:X1,:Y1,:X2,:Y2,:X3,:Y3,:X4,:Y4,:X5,:Y5,:X6,' +
    ':Y6,:X7,:Y7,:X8,:Y8,:X9,:Y9,:X10,:Y10,:X11,:Y11,:X12,:Y12,:X13,:Y13,'    +
    ':X14,:Y14,:X15,:Y15,:X16,:Y16'                                            ;
    ID_INSERT_DBX_GEO_PARAM_NORMALIZED =
      ID_INSERT_DBX_GEO_PARAM_WKT_TEXT + 1                             ;

  INSERT_DBX =
    'INSERT INTO %s ( %s ) VALUES ( %s ) %s'                                   ;
    ID_INSERT_DBX =
      ID_INSERT_DBX_GEO_PARAM_NORMALIZED + 1                           ;

  UPDATE_DBX_ORA =
    'returning <#WKB_GEOMETRY#> into :<#WKB_GEOMETRY#>'                        ;
    ID_UPDATE_DBX_ORA =
      ID_INSERT_DBX + 1                                                ;

  ALTER_DROP_COLUMN =
    'ALTER TABLE %s <#DROP COLUMN#> <#QUOTE LEFT#>%s<#QUOTE RIGHT#>'           ;
    ID_ALTER_DROP_COLUMN =
      ID_UPDATE_DBX_ORA + 1                                            ;

  ALTER_ADD_STRING =
    'ALTER TABLE %s ADD %s <#VARCHAR#>(%d<#VARCHAR_TYPE#>)'                   ;
    ID_ALTER_ADD_STRING =
      ID_ALTER_DROP_COLUMN + 1                                         ;

  ALTER_ADD_MEMO =
    'ALTER TABLE %s ADD %s <#CLOB#>'                                           ;
    ID_ALTER_ADD_MEMO =
      ID_ALTER_ADD_STRING + 1                                          ;

  ALTER_ALTER_STRING =
    'ALTER TABLE %s <#ALTER#> %s <#VARCHAR_LEN#> <#VARCHAR#>(%d)'              ;
    ID_ALTER_ALTER_STRING =
      ID_ALTER_ADD_MEMO + 1                                            ;

  ALTER_ADD_DATE =
    'ALTER TABLE %s ADD %s <#DATE#>'                                           ;
    ID_ALTER_ADD_DATE =
      ID_ALTER_ALTER_STRING + 1                                        ;

  ALTER_ADD_BOOLEAN =
    'ALTER TABLE %s ADD %s <#LOGICAL#>'                                        ;
    ID_ALTER_ADD_BOOLEAN =
      ID_ALTER_ADD_DATE + 1                                            ;

  ALTER_ADD_INTEGER =
    'ALTER TABLE %s ADD %s <#INTEGER#>'                                        ;
    ID_ALTER_ADD_INTEGER =
      ID_ALTER_ADD_BOOLEAN + 1                                         ;

  ALTER_ADD_BIGINT =
    'ALTER TABLE %s ADD %s <#BIGINT#>'                                         ;
    ID_ALTER_ADD_BIGINT =
      ID_ALTER_ADD_INTEGER + 1                                         ;

  ALTER_ADD_FLOAT =
    'ALTER TABLE %s ADD %s <#DOUBLE#>'                                         ;
    ID_ALTER_ADD_FLOAT =
      ID_ALTER_ADD_BIGINT + 1                                          ;

  FILTER_UID =
    '%s=%d'                                                                    ;
    ID_FILTER_UID =
      ID_ALTER_ADD_FLOAT + 1                                           ;

  FILTER_UID_WEAK =
    '(%s>=%d) and (%s<=%d)'                                                    ;
    ID_FILTER_UID_WEAK =
      ID_FILTER_UID + 1                                                ;

  FILTER_TABLE =
    '      <#G_TABLE_CATALOG#>=''%s'''                                        +
    '  AND <#G_TABLE_SCHEMA#>=''%s'''                                         +
    '  AND <#G_TABLE_NAME#>=''%s'''                                            ;
    ID_FILTER_TABLE =
      ID_FILTER_UID_WEAK + 1                                           ;

  MAX_NAMELENGTH =
    '<#MAX_NAMELENGTH#>'                                                       ;
    ID_MAX_NAMELENGTH =
      ID_FILTER_TABLE + 1                                              ;

  MAX_TEXTLENGTH =
    '<#MAX_TEXTLENGTH#>'                                                       ;
    ID_MAX_TEXTLENGTH =
      ID_MAX_NAMELENGTH + 1                                            ;

  TABLE_MASTER =
    '<#GEOMETRY_COLUMNS#>'                                                     ;
    ID_TABLE_MASTER =
      ID_MAX_TEXTLENGTH + 1                                            ;

  TABLE_SRS =
    '<#SPATIAL_REFERENCE_SYSTEMS#>'                                            ;
    ID_TABLE_SRS =
      ID_TABLE_MASTER + 1                                              ;

  TABLE_SRS_SHORT =
    '<#SPATIAL_REF_SYS#>'                                                      ;
    ID_TABLE_SRS_SHORT = ID_TABLE_SRS + 1                      ;

  SRTEXT =
    '<#SRTEXT#>'                                                               ;
    ID_SRTEXT = ID_TABLE_SRS_SHORT + 1                         ;

  XMIN =
    '<#XMIN#>'                                                                 ;
    ID_XMIN = ID_SRTEXT + 1                                    ;

  XMAX =
    '<#XMAX#>'                                                                 ;
    ID_XMAX = ID_XMIN + 1                                       ;

  YMIN =
    '<#YMIN#>'                                                                 ;
    ID_YMIN = ID_XMAX + 1                                      ;

  YMAX =
    '<#YMAX#>'                                                                 ;
    ID_YMAX = ID_YMIN + 1                                      ;

  MAX_SRID =
    '<#MAX_SRID#>'                                                             ;
    ID_MAX_SRID = ID_YMAX + 1                                  ;

  GEOMETRY_TYPE =
  '<#GEOMETRY_TYPE#>'                                                          ;
  ID_GEOMETRY_TYPE = ID_MAX_SRID + 1                           ;

  F_GEOMETRY_COLUMN =
    '<#F_GEOMETRY_COLUMN#>'                                                    ;
  ID_F_GEOMETRY_COLUMN = ID_GEOMETRY_TYPE + 1                  ;

  G_TABLE_NAME =
    '<#G_TABLE_NAME#>'                                                         ;
  ID_G_TABLE_NAME = ID_F_GEOMETRY_COLUMN + 1                   ;

  F_TABLE_NAME =
    '<#F_TABLE_NAME#>'                                                         ;
  ID_F_TABLE_NAME = ID_G_TABLE_NAME + 1                        ;

  STORAGE_TYPE =
    '<#STORAGE_TYPE#>'                                                         ;
  ID_STORAGE_TYPE = ID_F_TABLE_NAME + 1                        ;

  MAX_PPR =
    '<#MAX_PPR#>'                                                              ;
  ID_MAX_PPR = ID_STORAGE_TYPE + 1                             ;

  GID =
    '<#GID#>'                                                                  ;
    ID_GID = ID_MAX_PPR + 1                                    ;

  WKB_GEOMETRY =
    '<#WKB_GEOMETRY#>'                                                         ;
  ID_WKB_GEOMETRY = ID_GID + 1                                 ;

  WKB_GEOMETRY_EX =
    '<#:WKB_GEOMETRY#>'                                                        ;
    ID_WKB_GEOMETRY_EX = ID_WKB_GEOMETRY + 1                   ;

  WKT_GEOMETRY =
    '<#WKT_GEOMETRY#>'                                                         ;
  ID_WKT_GEOMETRY = ID_WKB_GEOMETRY_EX + 1                     ;

  GEO =
    '<#GEO#>'                                                                  ;
  ID_GEO = ID_WKT_GEOMETRY + 1                                 ;

  FEA =
    '<#FEA#>'                                                                  ;
  ID_FEA = ID_GEO + 1                                          ;

  FEATURE_SUFFIX =
    '_FEA'                                                                     ;
    ID_FEATURE_SUFFIX = ID_FEA + 1                             ;

  COORD_DIMENSION =
    '<#COORD_DIMENSION#>'                                                      ;
  ID_COORD_DIMENSION = ID_FEATURE_SUFFIX + 1                   ;

  CREATEINDEX_UID =
    'CREATE <#UNIQUE INDEX#> %s<#_IDX_#><#UID#> ON %s (GID)'                   ;
  ID_CREATEINDEX_UID = ID_COORD_DIMENSION + 1                  ;

  CREATEINDEX_XMIN =
    'CREATE INDEX %s<#_IDX_#><#XMIN#> ON %s (<#XMIN#>)'                        ;
    ID_CREATEINDEX_XMIN = ID_CREATEINDEX_UID + 1               ;

  CREATEINDEX_XMAX =
    'CREATE <#DESC_IB#> INDEX %s<#_IDX_#><#XMAX#> ON %s (<#XMAX#> <#DESC#>)'   ;
    ID_CREATEINDEX_XMAX = ID_CREATEINDEX_XMIN + 1              ;

  CREATEINDEX_YMIN =
    'CREATE INDEX %s<#_IDX_#><#YMIN#> ON %s (<#YMIN#>)'                        ;
    ID_CREATEINDEX_YMIN = ID_CREATEINDEX_XMAX + 1              ;

  CREATEINDEX_YMAX =
    'CREATE <#DESC_IB#> INDEX %s<#_IDX_#><#YMAX#> ON %s (<#YMAX#> <#DESC#>)'   ;
    ID_CREATEINDEX_YMAX =
      ID_CREATEINDEX_YMIN + 1                                          ;

  CREATEINDEX_EXTENT =
    'CREATE INDEX %s<#_IDX_#><#EXTENT#> ON %s (<#XMAX#>,<#XMIN#>,<#YMAX#>,<#YMIN#>)'   ;
    ID_CREATEINDEX_EXTENT =
      ID_CREATEINDEX_YMAX + 1                                          ;

  CREATEINDEX_NORMALIZED =
    'CREATE INDEX %s<#_IDX_#><#NRM#> ON %s '                                  +
    '(<#GID#>, ESEQ, SEQ)'                                                     ;
  ID_CREATEINDEX_NORMALIZED =
    ID_CREATEINDEX_EXTENT + 1                                            ;

  CREATEINDEX_GEOMETRY_COLUMNS =
    'CREATE <#UNIQUE#> INDEX <#GEOMETRY_COLUMNS#><#_IDX#> '                   +
    'ON <#GEOMETRY_COLUMNS#>'                                                 +
    '(G_TABLE_CATALOG, G_TABLE_SCHEMA, G_TABLE_NAME)'                          ;
  ID_CREATEINDEX_GEOMETRY_COLUMNS =
    ID_CREATEINDEX_NORMALIZED + 1                                      ;

  ESEQ =
    'ESEQ'                                                                     ;
  ID_ESEQ = ID_CREATEINDEX_GEOMETRY_COLUMNS + 1                ;

  ETYPE =
    'ETYPE'                                                                    ;
  ID_ETYPE = ID_ESEQ + 1                                       ;

  SEQ =
    'SEQ'                                                                      ;
  ID_SEQ = ID_ETYPE + 1                                        ;

  GEOMETRY_DECODE =
    'DECODE(''%s'', ''hex'')'                                                  ;
    ID_GEOMETRY_DECODE = ID_SEQ + 1                            ;

  GEOMETRY_BLOB =
    'BLOB(x''%s'')'                                                            ;
    ID_GEOMETRY_BLOB = ID_GEOMETRY_DECODE + 1                  ;


  ID_END = ID_GEOMETRY_BLOB                                       ;
  end;

//=============================================================================
// TGIS_LayerSqlOgisAbstract
//=============================================================================

  constructor TGIS_LayerSqlOgisAbstract.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;

    lastNewUid := -1 ;

    isWkt     := False ;
    isBlob    := False ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.doDestroy ;
  begin

    inherited ;
  end ;

  function TGIS_LayerSqlOgisAbstract.fget_TableMaster : String ;
  begin
    Result := getCmd( T_SQLOGIS.ID_TABLE_MASTER ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.fget_TableGeometry : String ;
  begin
    if ( not IsStringEmpty( strSchema ) ) and not oGisDb.IsMsJet then
      Result := Format( '%s.%s', [ strSchema, nameTableGeometry ] )
    else
      Result := nameTableGeometry ;
  end ;

  function TGIS_LayerSqlOgisAbstract.fget_TableFeatures : String ;
  begin
    if ( not IsStringEmpty( strSchema ) ) and not oGisDb.IsMsJet then
      Result := Format( '%s.%s', [ strSchema, nameTableFeatures ] )
    else
      Result := nameTableFeatures ;
  end ;

  function TGIS_LayerSqlOgisAbstract.fget_ViewFeatures : String ;
  begin
    if not IsStringEmpty( FViewFeatures ) then
      Result := FViewFeatures
    else
      Result := fget_TableFeatures ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.prepareCommandList ;
  begin
    initializeCommandList( T_SQLOGIS.ID_END ) ;

    assert( FSQLCommands[ T_SQLOGIS.ID_CREATE_GEOMETRY_COLUMNS_TABLE          ] =  '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATE_GEOMETRY_COLUMNS_TABLE          ]
                          := T_SQLOGIS.CREATE_GEOMETRY_COLUMNS_TABLE          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS ] =  '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS ]
                          := T_SQLOGIS.CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_BINARY           ] =  '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_BINARY           ]
                          := T_SQLOGIS.CREATE_TABLE_GEOMETRY_BINARY           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_WKT              ] =  '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_WKT              ]
                          := T_SQLOGIS.CREATE_TABLE_GEOMETRY_WKT              ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_NORMALIZED       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_NORMALIZED       ]
                          := T_SQLOGIS.CREATE_TABLE_GEOMETRY_NORMALIZED       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_FEATURES                  ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATE_TABLE_FEATURES                  ]
                          := T_SQLOGIS.CREATE_TABLE_FEATURES                  ;
    assert( FSQLCommands[ T_SQLOGIS.ID_DROP_TABLE                             ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_DROP_TABLE                             ]
                          := T_SQLOGIS.DROP_TABLE                             ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE          ]
                          := T_SQLOGIS.SELECT_GEOMETRY_COLUMNS_TABLE          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_ALL      ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_ALL      ]
                          := T_SQLOGIS.SELECT_GEOMETRY_COLUMNS_TABLE_ALL      ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG  ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG  ]
                          := T_SQLOGIS.SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG  ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA   ]
                          := T_SQLOGIS.SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_NAME     ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_NAME     ]
                          := T_SQLOGIS.SELECT_GEOMETRY_COLUMNS_TABLE_NAME     ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_MAX_GID                ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_MAX_GID                ]
                          := T_SQLOGIS.SELECT_GEOMETRY_MAX_GID                ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_GID                                ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_GID                                ]
                          := T_SQLOGIS.MAX_GID                                ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_EXTENT                 ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_GEOMETRY_EXTENT                 ]
                          := T_SQLOGIS.SELECT_GEOMETRY_EXTENT                 ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MIN_XMIN                               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MIN_XMIN                               ]
                          := T_SQLOGIS.MIN_XMIN                               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_XMAX                               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_XMAX                               ]
                          := T_SQLOGIS.MAX_XMAX                               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MIN_YMIN                               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MIN_YMIN                               ]
                          := T_SQLOGIS.MIN_YMIN                               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_YMAX                               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_YMAX                               ]
                          := T_SQLOGIS.MAX_YMAX                               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_TABLE_ALL                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_TABLE_ALL                       ]
                          := T_SQLOGIS.SELECT_TABLE_ALL                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_TABLE_WHERE                     ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_TABLE_WHERE                     ]
                          := T_SQLOGIS.SELECT_TABLE_WHERE                     ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_UID                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_UID                        ]
                          := T_SQLOGIS.SELECT_JOIN_UID                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN                            ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN                            ]
                          := T_SQLOGIS.SELECT_JOIN                            ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_EX                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_EX                         ]
                          := T_SQLOGIS.SELECT_JOIN_EX                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NOEXT                      ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NOEXT                      ]
                          := T_SQLOGIS.SELECT_JOIN_NOEXT                      ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NOEXT_EX                   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NOEXT_EX                   ]
                          := T_SQLOGIS.SELECT_JOIN_NOEXT_EX                   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT           ]
                          := T_SQLOGIS.SELECT_JOIN_NORMALIZED_NOEXT           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT_EX        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT_EX        ]
                          := T_SQLOGIS.SELECT_JOIN_NORMALIZED_NOEXT_EX        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SELECT_MAX_SRID                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SELECT_MAX_SRID                        ]
                          := T_SQLOGIS.SELECT_MAX_SRID                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_DELETE_SHAPE                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_DELETE_SHAPE                           ]
                          := T_SQLOGIS.DELETE_SHAPE                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_DELETE_GEOMETRY_COLUMNS_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_DELETE_GEOMETRY_COLUMNS_TABLE          ]
                          := T_SQLOGIS.DELETE_GEOMETRY_COLUMNS_TABLE          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_GEOMETRY_COLUMNS_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_GEOMETRY_COLUMNS_TABLE          ]
                          := T_SQLOGIS.INSERT_GEOMETRY_COLUMNS_TABLE          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE ]
                          := T_SQLOGIS.INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE ;
    assert( FSQLCommands[ T_SQLOGIS.ID_UPDATE_GEOMETRY_COLUMNS_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_UPDATE_GEOMETRY_COLUMNS_TABLE          ]
                          := T_SQLOGIS.UPDATE_GEOMETRY_COLUMNS_TABLE          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_GEO_BINARY                  ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_GEO_BINARY                  ]
                          := T_SQLOGIS.UPDATE_DBX_GEO_BINARY                  ;
    assert( FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_GEO_WKT                     ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_GEO_WKT                     ]
                          := T_SQLOGIS.UPDATE_DBX_GEO_WKT                     ;
    assert( FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_GEO_NORMALIZED              ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_GEO_NORMALIZED              ]
                          := T_SQLOGIS.UPDATE_DBX_GEO_NORMALIZED              ;
    assert( FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX                             ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX                             ]
                          := T_SQLOGIS.UPDATE_DBX                             ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_BINARY            ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_BINARY            ]
                          := T_SQLOGIS.INSERT_DBX_GEO_VALUE_BINARY            ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_WKT               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_WKT               ]
                          := T_SQLOGIS.INSERT_DBX_GEO_VALUE_WKT               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_NORMALIZED        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_NORMALIZED        ]
                          := T_SQLOGIS.INSERT_DBX_GEO_VALUE_NORMALIZED        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_BINARY            ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_BINARY            ]
                          := T_SQLOGIS.INSERT_DBX_GEO_PARAM_BINARY            ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_BINARY_TEXT       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_BINARY_TEXT       ]
                          := T_SQLOGIS.INSERT_DBX_GEO_PARAM_BINARY_TEXT       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_WKT               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_WKT               ]
                          := T_SQLOGIS.INSERT_DBX_GEO_PARAM_WKT               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_WKT_TEXT          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_WKT_TEXT          ]
                          := T_SQLOGIS.INSERT_DBX_GEO_PARAM_WKT_TEXT          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_NORMALIZED        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_NORMALIZED        ]
                          := T_SQLOGIS.INSERT_DBX_GEO_PARAM_NORMALIZED        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX                             ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_INSERT_DBX                             ]
                          := T_SQLOGIS.INSERT_DBX                             ;
    assert( FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_ORA                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_UPDATE_DBX_ORA                         ]
                          := T_SQLOGIS.UPDATE_DBX_ORA                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_DROP_COLUMN                      ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_DROP_COLUMN                      ]
                          := T_SQLOGIS.ALTER_DROP_COLUMN                      ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_STRING                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_STRING                       ]
                          := T_SQLOGIS.ALTER_ADD_STRING                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_MEMO                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_MEMO                         ]
                          := T_SQLOGIS.ALTER_ADD_MEMO                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ALTER_STRING                     ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ALTER_STRING                     ]
                          := T_SQLOGIS.ALTER_ALTER_STRING                     ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_DATE                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_DATE                         ]
                          := T_SQLOGIS.ALTER_ADD_DATE                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_BOOLEAN                      ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_BOOLEAN                      ]
                          := T_SQLOGIS.ALTER_ADD_BOOLEAN                      ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_INTEGER                      ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_INTEGER                      ]
                          := T_SQLOGIS.ALTER_ADD_INTEGER                      ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_BIGINT                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_BIGINT                       ]
                          := T_SQLOGIS.ALTER_ADD_BIGINT                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_FLOAT                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ALTER_ADD_FLOAT                        ]
                          := T_SQLOGIS.ALTER_ADD_FLOAT                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_FILTER_UID                             ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_FILTER_UID                             ]
                          := T_SQLOGIS.FILTER_UID                             ;
    assert( FSQLCommands[ T_SQLOGIS.ID_FILTER_UID_WEAK                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_FILTER_UID_WEAK                        ]
                          := T_SQLOGIS.FILTER_UID_WEAK                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_FILTER_TABLE                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_FILTER_TABLE                           ]
                          := T_SQLOGIS.FILTER_TABLE                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_NAMELENGTH                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_NAMELENGTH                         ]
                          := T_SQLOGIS.MAX_NAMELENGTH                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_TEXTLENGTH                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_TEXTLENGTH                         ]
                          := T_SQLOGIS.MAX_TEXTLENGTH                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_PPR                                ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_PPR                                ]
                          := T_SQLOGIS.MAX_PPR                                ;
    assert( FSQLCommands[ T_SQLOGIS.ID_TABLE_MASTER                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_TABLE_MASTER                           ]
                          := T_SQLOGIS.TABLE_MASTER                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_TABLE_SRS                              ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_TABLE_SRS                              ]
                          := T_SQLOGIS.TABLE_SRS                              ;
    assert( FSQLCommands[ T_SQLOGIS.ID_TABLE_SRS_SHORT                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_TABLE_SRS_SHORT                        ]
                          := T_SQLOGIS.TABLE_SRS_SHORT                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SRTEXT                                 ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SRTEXT                                 ]
                          := T_SQLOGIS.SRTEXT                                 ;
    assert( FSQLCommands[ T_SQLOGIS.ID_XMIN                                   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_XMIN                                   ]
                          := T_SQLOGIS.XMIN                                   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_XMAX                                   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_XMAX                                   ]
                          := T_SQLOGIS.XMAX                                   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_YMIN                                   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_YMIN                                   ]
                          := T_SQLOGIS.YMIN                                   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_YMAX                                   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_YMAX                                   ]
                          := T_SQLOGIS.YMAX                                   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_MAX_SRID                               ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_MAX_SRID                               ]
                          := T_SQLOGIS.MAX_SRID                               ;
    assert( FSQLCommands[ T_SQLOGIS.ID_GEOMETRY_TYPE                          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_GEOMETRY_TYPE                          ]
                          := T_SQLOGIS.GEOMETRY_TYPE                          ;
    assert( FSQLCommands[ T_SQLOGIS.ID_GID                                    ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_GID                                    ]
                          := T_SQLOGIS.GID                                    ;
    assert( FSQLCommands[ T_SQLOGIS.ID_WKB_GEOMETRY                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_WKB_GEOMETRY                           ]
                          := T_SQLOGIS.WKB_GEOMETRY                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_WKB_GEOMETRY_EX                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_WKB_GEOMETRY_EX                        ]
                          := T_SQLOGIS.WKB_GEOMETRY_EX                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_WKT_GEOMETRY                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_WKT_GEOMETRY                           ]
                          := T_SQLOGIS.WKT_GEOMETRY                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_GEO                                    ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_GEO                                    ]
                          := T_SQLOGIS.GEO                                    ;
    assert( FSQLCommands[ T_SQLOGIS.ID_FEA                                    ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_FEA                                    ]
                          := T_SQLOGIS.FEA                                    ;
    assert( FSQLCommands[ T_SQLOGIS.ID_FEATURE_SUFFIX                         ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_FEATURE_SUFFIX                         ]
                          := T_SQLOGIS.FEATURE_SUFFIX                         ;
    assert( FSQLCommands[ T_SQLOGIS.ID_COORD_DIMENSION                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_COORD_DIMENSION                        ]
                          := T_SQLOGIS.COORD_DIMENSION                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_F_GEOMETRY_COLUMN                      ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_F_GEOMETRY_COLUMN                      ]
                          := T_SQLOGIS.F_GEOMETRY_COLUMN                      ;
    assert( FSQLCommands[ T_SQLOGIS.ID_G_TABLE_NAME                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_G_TABLE_NAME                           ]
                          := T_SQLOGIS.G_TABLE_NAME                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_F_TABLE_NAME                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_F_TABLE_NAME                           ]
                          := T_SQLOGIS.F_TABLE_NAME                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_STORAGE_TYPE                           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_STORAGE_TYPE                           ]
                          := T_SQLOGIS.STORAGE_TYPE                           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_UID                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_UID                        ]
                          := T_SQLOGIS.CREATEINDEX_UID                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_XMIN                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_XMIN                       ]
                          := T_SQLOGIS.CREATEINDEX_XMIN                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_XMAX                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_XMAX                       ]
                          := T_SQLOGIS.CREATEINDEX_XMAX                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_YMIN                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_YMIN                       ]
                          := T_SQLOGIS.CREATEINDEX_YMIN                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_YMAX                       ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_YMAX                       ]
                          := T_SQLOGIS.CREATEINDEX_YMAX                       ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_EXTENT                     ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_EXTENT                     ]
                          := T_SQLOGIS.CREATEINDEX_EXTENT                     ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_NORMALIZED                 ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_NORMALIZED                 ]
                          := T_SQLOGIS.CREATEINDEX_NORMALIZED                 ;
    assert( FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_GEOMETRY_COLUMNS           ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_CREATEINDEX_GEOMETRY_COLUMNS           ]
                          := T_SQLOGIS.CREATEINDEX_GEOMETRY_COLUMNS           ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ESEQ                                   ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ESEQ                                   ]
                          := T_SQLOGIS.ESEQ                                   ;
    assert( FSQLCommands[ T_SQLOGIS.ID_ETYPE                                  ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_ETYPE                                  ]
                          := T_SQLOGIS.ETYPE                                  ;
    assert( FSQLCommands[ T_SQLOGIS.ID_SEQ                                    ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_SEQ                                    ]
                          := T_SQLOGIS.SEQ                                    ;
    assert( FSQLCommands[ T_SQLOGIS.ID_GEOMETRY_DECODE                        ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_GEOMETRY_DECODE                        ]
                          := T_SQLOGIS.GEOMETRY_DECODE                        ;
    assert( FSQLCommands[ T_SQLOGIS.ID_GEOMETRY_BLOB                          ] = '' ) ;
            FSQLCommands[ T_SQLOGIS.ID_GEOMETRY_BLOB                          ]
                          := T_SQLOGIS.GEOMETRY_BLOB                          ;

    finalizeCommandList ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdGEOUID(
    const _id : Integer
  ) : String ;
  begin
    assert( _id in [0..1] ) ;
    if      _id = 0 then
            Result := nameColumnGeometry
    else if _id = 1 then
            Result := getCmd( T_SQLOGIS.ID_FEA ) + '.' + nameColumnGeometry ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdSHAPETYPE : String ;
  begin
    Result := getCmd( T_SQLOGIS.ID_GEOMETRY_TYPE ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdXMIN : String ;
  begin
    Result := getCmd( T_SQLOGIS.ID_XMIN ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdYMIN : String ;
  begin
    Result := getCmd( T_SQLOGIS.ID_YMIN ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdGEOMETRY : String ;
  begin
    Result := getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdGEOMETRYWkt : String ;
  begin
    Result := getCmd( T_SQLOGIS.ID_WKT_GEOMETRY ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getCmdSRSTable : String ;
  begin
    if isSRSshort then Result := getCmd( T_SQLOGIS.ID_TABLE_SRS_SHORT )
                  else Result := getCmd( T_SQLOGIS.ID_TABLE_SRS       ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getTableWithSchema(
    const _table : String
   ) : String ;
  begin
    if ( not IsStringEmpty( strSchema ) ) and not oGisDb.IsMsJet then
      Result := Format( '%s.%s', [ strSchema, _table ] )
    else
      Result := _table ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.prepareParamsCommand(
    const _table  : String ;
    const _params : TStrings
  ) ;
  var
     i : Integer ;
  begin
    _params.Clear ;

    if ( _table = TableGeometry ) then begin

      if isBlob then begin
        _params.Text := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_BINARY_TEXT ) ;
      end
      else if isWkt then begin
        _params.Text := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_WKT_TEXT ) ;
      end
      else begin
        //_params.Text := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_NORMALIZED_TEXT ) ;
      end ;
    end
    else begin
      if IsStringEmpty( nameColumnGeometry ) then begin
        _params.Add( getCmd( T_SQLOGIS.ID_GID )+'=' ) ;
      end
      else begin
        _params.Add( nameColumnGeometry+'=' ) ;
      end ;

      for i:=0 to Fields.Count-1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        _params.Add( FieldInfo(i).ExportName+'=' ) ;
      end ;
    end ;

  end ;

  function TGIS_LayerSqlOgisAbstract.prepareAppendCommand(
    const _table : String
  ) : String ;
  var
    i         : Integer ;
    param     : String  ;
    value     : String  ;
    tmp       : String  ;
  begin
    if IsStringEmpty( nameColumnGeometry ) then begin
      value := getCmd( T_SQLOGIS.ID_GID ) ;
      param := ':' + value ;
    end
    else begin
      value := nameColumnGeometry ;
      param := ':' + nameColumnGeometry ;
    end ;

    if _table = TableGeometry then begin
      if isBlob then begin
        value := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_BINARY ) ;
        param := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_BINARY ) ;
        if oGisDb.IsOracle then
          tmp := getCmd( T_SQLOGIS.ID_UPDATE_DBX_ORA )
        else
          tmp := '' ;
      end
      else if isWkt then begin
        value := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_WKT ) ;
        param := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_WKT ) ;
        tmp   := '' ;
      end
      else begin
        value := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_NORMALIZED ) ;
        param := getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_PARAM_NORMALIZED ) ;
        tmp := '' ;
      end ;
    end
    else begin
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
    end ;

    Result := Format( getCmd( T_SQLOGIS.ID_INSERT_DBX ), [_table, value, param, tmp] ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareAppendParams(
    const _table : String
  ) : String ;
  var
    i     : Integer ;
    param : TStringBuilder  ;
    value : TStringBuilder  ;
    tmp   : String ;
  begin
    param := TStringBuilder.Create ;
    value := TStringBuilder.Create ;
    try
      if _table = TableGeometry then begin
        if isBlob then begin
          value.Append( getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_BINARY ) ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_GID ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ) ) ;
          param.Append( ',' ) ;
          if oGisDb.IsOracle then begin
            param.Append( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY_EX ) ) ;
            tmp := Format( 'returning %s into %s', [
                            getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ),
                            safeParam( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ) ]
                          )
          end
          else begin
            param.Append( safeParam( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ) ) ;
            tmp := '' ;
          end ;
        end
        else if isWkt then begin
          value.Append( getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_WKT ) ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_GID ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_WKT_GEOMETRY ) ) ) ;
          tmp := '' ;
        end
        else begin
          value.Append( getCmd( T_SQLOGIS.ID_INSERT_DBX_GEO_VALUE_NORMALIZED ) ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_GID ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_ESEQ ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_ETYPE ) ) ) ;
          param.Append( ',' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_SEQ ) ) ) ;
          for i := 1 to 16 do  begin
            param.Append( ',' ) ;
            param.Append( safeParam( Format( 'X%d', [i] ) ) ) ;
            param.Append( ',' ) ;
            param.Append( safeParam( Format( 'Y%d', [i] ) ) ) ;
          end ;
          tmp := '' ;
        end ;

      end
      else begin
        if IsStringEmpty( nameColumnGeometry ) then begin
          value.Append( getCmd( T_SQLOGIS.ID_GID ) ) ;
          param.Append( safeParam(getCmd( T_SQLOGIS.ID_GID )) ) ;
        end
        else begin
          value.Append( nameColumnGeometry ) ;
          param.Append( safeParam( nameColumnGeometry ) ) ;
        end ;

        for i := 0 to Fields.Count-1 do begin
          if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

          value.Append( ',' ) ;
          value.Append( safeName(FieldInfo( i ).ExportName) ) ;

          param.Append( ',' ) ;
          param.Append( safeParam( FieldInfo( i ).ExportName ) ) ;
        end ;
        tmp := '' ;
      end ;

      Result := Format( getCmd( T_SQLOGIS.ID_INSERT_DBX ),
                        [_table, value.ToString, param.ToString, tmp]
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareUpdateCommand(
      const _table  : String ;
      const _column : String
   ) : String ;
  var
    i         : Integer ;
    param     : String  ;
    tmp       : String  ;
  begin

    param := '' ;
    if _table = TableGeometry then begin
      if isBlob then begin
        param := getCmd( T_SQLOGIS.ID_UPDATE_DBX_GEO_BINARY ) ;
        if oGisDb.IsOracle then
          tmp := getCmd( T_SQLOGIS.ID_UPDATE_DBX_ORA )
        else
          tmp := '' ;
      end
      else if isWkt then begin
        param := getCmd( T_SQLOGIS.ID_UPDATE_DBX_GEO_WKT ) ;
        tmp   := '' ;
      end
      else begin
        param := getCmd( T_SQLOGIS.ID_UPDATE_DBX_GEO_NORMALIZED ) ;
        tmp := '' ;
      end ;
    end
    else begin
      for i:=0 to Fields.Count - 1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        if not IsStringEmpty( param ) then
          param := param + ',' ;
        param := param +
                 FieldInfo( i ).ExportName + '=:' +
                 FieldInfo( i ).ExportName ;
      end ;
      tmp := '' ;
    end ;

    Result := Format( getCmd( T_SQLOGIS.ID_UPDATE_DBX ),
                      [_table, param, _column, _column, tmp ]
                    ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareUpdateParams(
      const _table  : String ;
      const _column : String
   ) : String ;
  var
    i     : Integer ;
    param : TStringBuilder  ;
    tmp   : String  ;
  begin
    param := TStringBuilder.Create ;
    try
      if _table = TableGeometry then begin
        if isBlob then begin
          param.Append( getCmd( T_SQLOGIS.ID_XMIN ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_YMIN ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_XMAX ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_YMAX ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) + '=' ) ;

          if oGisDb.IsOracle then begin
            param.Append( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY_EX ) ) ;
            tmp := Format( 'returning %s into %s', [
                            getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ),
                            safeParam( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ) ]
                          )
          end
          else begin
            param.Append( safeParam( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ) ) ;
            tmp := '' ;
          end ;
        end
        else if isWkt then begin
          param.Append( getCmd( T_SQLOGIS.ID_XMIN ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_YMIN ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_XMAX ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_YMAX ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_WKT_GEOMETRY ) ) ) ;
          tmp := '' ;
        end
        else begin
          param.Append( getCmd( T_SQLOGIS.ID_GID ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_GID ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_ESEQ ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_ESEQ ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_ETYPE ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_ETYPE ) ) ) ;
          param.Append( ',' ) ;
          param.Append( getCmd( T_SQLOGIS.ID_SEQ ) + '=' ) ;
          param.Append( safeParam( getCmd( T_SQLOGIS.ID_SEQ ) ) ) ;
          for i := 1 to 16 do  begin
            param.Append( ',' ) ;
            param.Append( Format( 'X%d', [i] ) ) ;
            param.Append( safeParam( Format( 'X%d', [i] ) ) ) ;
            param.Append( ',' ) ;
            param.Append( Format( 'Y%d', [i] ) ) ;
            param.Append( safeParam( Format( 'Y%d', [i] ) ) ) ;
          end ;
        end ;

      end
      else begin
        for i := 0 to Fields.Count - 1 do begin
          if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

          param.Append( safeName(FieldInfo(i).ExportName) ) ;
          param.Append( '=' ) ;
          param.Append( safeParam( FieldInfo(i).ExportName ) ) ;
          if i < Fields.Count - 1 then
            param.Append( ',' ) ;
        end ;
        tmp := '' ;
      end ;
      Result := Format( 'UPDATE %s SET %s WHERE %s=%s %s',
                        [_table, param.ToString, _column, safeParam(_column), tmp ]
                      ) ;
    finally
      FreeObject( param ) ;
    end ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareAppendCommandEx(
    const _table  : String ;
    const _params : TStrings
  ) : String ;
  var
    i         : Integer ;
    param     : TStringBuilder  ;
    value     : TStringBuilder  ;
  begin
    value := TStringBuilder.Create ;
    param := TStringBuilder.Create ;
    try
    for i := 0 to _params.Count - 1 do begin
      if (_table = TableGeometry) or ( i = 0 ) then
        value.Append( _params.Names[i] )
      else
        value.Append( safeName(_params.Names[i]) ) ;

      if i < _params.Count - 1 then
        value.Append( ',' ) ;

      {$IFNDEF CLR}
        param.Append(_params.ValueFromIndex[i]) ;
      {$ELSE}
        param.Append(_params.Values[ _params.Names[i] ]) ;
      {$ENDIF}
        if i < _params.Count - 1 then
          param.Append( ',' ) ;
    end ;

      Result := Format( getCmd( T_SQLOGIS.ID_INSERT_DBX ),
                        [ _table, value.ToString, param.ToString, '' ]
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareUpdateCommandEx(
    const _table  : String ;
    const _params : TStrings
  ) : String ;
  var
    i         : Integer ;
    param     : String  ;
    fld       : TGIS_FieldInfo ;
  begin
    param := '' ;

    for i := 0 to _params.Count - 1 do begin

      fld := FieldInfo( FindField( _params.Names[i] ) ) ;

      if ( fld <> nil ) and ( fld.FileFormat ) then continue ;

      if not IsStringEmpty( param ) then
        param := param + ',' ;
      {$IFNDEF CLR}
        param := param +
                 safeName(_params.Names[i]) + '=' + _params.ValueFromIndex[i] ;
      {$ELSE}
        param := param +
                safeName(_params.Names[i]) + '=' + _params.Values[ _params.Names[i] ] ;
      {$ENDIF}
    end ;

    Result := Format( 'UPDATE %s SET %s WHERE %s=%s',
                      [ _table,
                        param,
                        getCmdGEOUID(0),
                        _params.Values[ getCmdGEOUID(0)]
                      ]
                    ) ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroAppendParams(
    const _table : String
   ) ;
 var
    i       : Integer ;
    pname   : String ;
    size    : Integer ;
    pdtype  : TGIS_DataType ;
  begin
    if _table = TableGeometry then begin
      if isBlob then begin
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ),
          TGIS_DataType.Blob,
          TGIS_SubDataType.Unknown,
          -1
         ) ;
      end
      else if isWkt then begin
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_WKT_GEOMETRY ) ),
          TGIS_DataType.Memo,
          TGIS_SubDataType.Unknown,
          -1
         ) ;
      end
      else begin
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_ESEQ ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_ETYPE ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_SEQ ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
          for i := 1 to 16 do  begin
            oGisDb.sqlTableCreateParam(
              0,
              safeParam( Format( 'X%d', [i] ) ),
              TGIS_DataType.Float,
              TGIS_SubDataType.Unknown,
              20
             ) ;
            oGisDb.sqlTableCreateParam(
              0,
              safeParam( Format( 'Y%d', [i] ) ),
              TGIS_DataType.Float,
              TGIS_SubDataType.Unknown,
              20
             ) ;
          end ;
      end ;
    end
    else begin
      if IsStringEmpty( nameColumnGeometry ) then
        oGisDb.sqlTableCreateParam(
          1,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         )
      else
        oGisDb.sqlTableCreateParam(
          1,
          safeParam( nameColumnGeometry ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;

      for i := 0 to Fields.Count-1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        pname := safeParam( FieldInfo(i).ExportName ) ;
        size  := FieldInfo(i).Width ;

        case FieldInfo(i).FieldType of
          TGIS_FieldType.String : pdtype := TGIS_DataType.String ;
          TGIS_FieldType.Number : if FieldInfo(i).NewDecimal = 0 then begin
                                    if FieldInfo(i).Width <= 9 then
                                      pdtype := TGIS_DataType.Integer
                                    else
                                      pdtype := TGIS_DataType.LargeInt ;
                                    end
                                  else
                                    pdtype := TGIS_DataType.Float ;
          TGIS_FieldType.Float  : pdtype := TGIS_DataType.Float ;
          TGIS_FieldType.Boolean: pdtype := TGIS_DataType.Boolean ;
          TGIS_FieldType.Date   : pdtype := TGIS_DataType.DateTime
          else                    pdtype := TGIS_DataType.String ;
        end ;
        oGisDb.sqlTableCreateParam( 1, pname, pdtype, TGIS_SubDataType.Unknown, size ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroUpdateParams(
    const _table : String
   ) ;
 var
    i       : Integer ;
    pname   : String ;
    size    : Integer ;
    pdtype  : TGIS_DataType ;
  begin
    if _table = TableGeometry then begin
      if isBlob then begin
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_WKB_GEOMETRY ) ),
          TGIS_DataType.Blob,
          TGIS_SubDataType.Unknown,
          -1
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
      end
      else if isWkt then begin
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMIN ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_XMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_YMAX ) ),
          TGIS_DataType.Float,
          TGIS_SubDataType.Unknown,
          20
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_WKT_GEOMETRY ) ),
          TGIS_DataType.Memo,
          TGIS_SubDataType.Unknown,
          -1
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
      end
      else begin
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_ESEQ ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_ETYPE ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
        oGisDb.sqlTableCreateParam(
          0,
          safeParam( getCmd( T_SQLOGIS.ID_SEQ ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;
          for i := 1 to 16 do  begin
            oGisDb.sqlTableCreateParam(
              0,
              safeParam( Format( 'X%d', [i] ) ),
              TGIS_DataType.Float,
              TGIS_SubDataType.Unknown,
              20
             ) ;
            oGisDb.sqlTableCreateParam(
              0,
              safeParam( Format( 'Y%d', [i] ) ),
              TGIS_DataType.Float,
              TGIS_SubDataType.Unknown,
              20
             ) ;
          end ;
          oGisDb.sqlTableCreateParam(
            0,
            safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
            TGIS_DataType.Integer,
            TGIS_SubDataType.Unknown,
            8
           ) ;
      end ;
    end
    else begin

      for i := 0 to Fields.Count-1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        pname := safeParam( FieldInfo(i).ExportName ) ;
        size  := FieldInfo(i).Width ;

        case FieldInfo(i).FieldType of
          TGIS_FieldType.String : pdtype := TGIS_DataType.String ;
          TGIS_FieldType.Number : if FieldInfo(i).NewDecimal = 0 then begin
                                    if FieldInfo(i).Width <= 9 then
                                      pdtype := TGIS_DataType.Integer
                                    else
                                      pdtype := TGIS_DataType.LargeInt ;
                                    end
                                  else
                                    pdtype := TGIS_DataType.Float ;
          TGIS_FieldType.Float  : pdtype := TGIS_DataType.Float ;
          TGIS_FieldType.Boolean: pdtype := TGIS_DataType.Boolean ;
          TGIS_FieldType.Date   : pdtype := TGIS_DataType.DateTime
          else                    pdtype := TGIS_DataType.String ;
        end ;
        oGisDb.sqlTableCreateParam( 1, pname, pdtype, TGIS_SubDataType.Unknown, size ) ;
      end ;
      if IsStringEmpty( nameColumnGeometry ) then
        oGisDb.sqlTableCreateParam(
          1,
          safeParam( getCmd( T_SQLOGIS.ID_GID ) ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         )
      else
        oGisDb.sqlTableCreateParam(
          1,
          safeParam( nameColumnGeometry ),
          TGIS_DataType.Integer,
          TGIS_SubDataType.Unknown,
          8
         ) ;

    end ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareSelectCommand(
     const _table  : String ;
     const _filter : String
   ) : String ;
  begin
    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLOGIS.ID_SELECT_TABLE_WHERE ), [ _table, _filter ] )
    else
      Result := Format( getCmd( T_SQLOGIS.ID_SELECT_TABLE_ALL ), [ _table ] ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareFilterUid(
    const _uid : TGIS_Uid
    ) : String ;
  begin
    if IsStringEmpty( nameColumnGeometry ) then
      Result := Format( getCmd( T_SQLOGIS.ID_FILTER_UID ),
                        [ getCmd( T_SQLOGIS.ID_GID ) , _uid ]
                      )
    else
      Result := Format( getCmd( T_SQLOGIS.ID_FILTER_UID ),
                        [ nameColumnGeometry , _uid ]
                      ) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.prepareMasterQuery(
     const _catalog  : String ;
     const _schema   : String ;
     const _name     : String ;
     const _usealias : Boolean
   ) : String ;
  var
    scatalog : String ;
    sschema  : String ;
    sname    : String ;

    ccatalog : String ;
    cschema  : String ;
    cname    : String ;
  begin
    if _usealias then begin
      ccatalog := 'GC.' ;
      cschema  := 'GC.' ;
      cname    := 'GC.' ;
    end
    else begin
      ccatalog := '' ;
      cschema  := '' ;
      cname    := '' ;
    end ;

    ccatalog := ccatalog + getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_CATALOG ) ;
    cschema  := cschema  + getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_SCHEMA  ) ;
    cname    := cname    + getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_NAME    ) ;

    if IsStringEmpty( _catalog ) then
      scatalog := Format( '((%s='''') OR (%s IS NULL)) ', [ ccatalog, ccatalog ] )
    else
      scatalog := Format( '(%s=''%s'')'                 , [ ccatalog, _catalog ] ) ;

    if IsStringEmpty( _schema ) then
      sschema  := Format( '((%s='''') OR (%s IS NULL)) ', [ cschema , cschema  ] )
    else
      sschema  := Format( '(%s=''%s'')'                 , [ cschema , _schema  ] ) ;

    sname      := Format( '(%s=''%s'')'                 , [ cname   , _name    ] ) ;

    Result := Format( '%s AND %s AND %s', [ scatalog, sschema, sname ] ) ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroConnect ;
  begin
    initializeConnect( True ) ;

    oGisDb.IsPostgreSql := False ;

    parseConfigLayerName ;

    try
      oGisDb.sqlConnect( oGisDb.sqlBaseFolder(self), FSQLParameters );

      try
        try
          sqlQueryOpen(
             Format( getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE ),
                     [ getCmdSRSTable,
                       prepareMasterQuery( strCatalog, strSchema, strName, True ) ]
                   ), 0
          ) ;

          if not oGisDb.sqlQueryEof(0) then begin
            nameColumnGeometry := VarToString(
              oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_F_GEOMETRY_COLUMN ), 0 )
              ) ;
            if not IsStringEmpty( nameColumnGeometry ) then begin
              // replace gid token with new value
              FSQLParameters.Values[ T_SQLOGIS.GID ] := nameColumnGeometry ;
              prepareCommandList ;
              FSQLParameters.Delete( FSQLParameters.IndexOfName(T_SQLOGIS.GID) ) ;
            end ;
            nameTableGeometry  := VarToString(
              oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_G_TABLE_NAME ), 0 )
              ) ;
            nameTableFeatures  := VarToString(
              oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_F_TABLE_NAME ), 0 )
              ) ;
          end ;
        finally
          oGisDb.sqlQueryClose(0) ;
        end;
      except
      end ;
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

  procedure TGIS_LayerSqlOgisAbstract.macroDisconnect ;
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

  procedure TGIS_LayerSqlOgisAbstract.macroUpdateStart ;
  begin
    lastUid := -1 ;
    oGisDb.sqlTransactGlobalUpdateStart ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroUpdateEnd ;
  begin
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
    oGisDb.sqlTransactGlobalUpdateCommit ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroMasterCreate ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      try
        oGisDb.sqlExec( getCmd( T_SQLOGIS.ID_CREATE_GEOMETRY_COLUMNS_TABLE ) ) ;
      except
        // can exist
      end ;
      try
        oGisDb.sqlExec( getCmd( T_SQLOGIS.ID_CREATEINDEX_GEOMETRY_COLUMNS ) ) ;
      except
        // can exist
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;

    try
      try
        oGisDb.sqlExec( Format(
                         getCmd( T_SQLOGIS.ID_CREATE_TABLE_SPATIAL_REFERENCE_SYSTEMS ),
                         [ getCmdSRSTable ]
                         )
                       ) ;
      except
        // can exist
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroMasterUpdate(
      const _extent : TGIS_Extent    ;
      const _type   : TGIS_ShapeType ;
      const _name   : String         ;
      const _dim    : TGIS_DimensionType
    ) ;
  var
    geometry_type : Integer ;
    dimSize       : Integer ;
  begin
    if IsReadOnly then exit ;

    // guarantee connection closing
      oGisDb.sqlQueryClose(0) ;
      oGisDb.sqlTableClose( 0 ) ;
      oGisDb.sqlTableClose( 1 ) ;

    case Integer(_type) of
      Integer(TGIS_ShapeType.Point)      : geometry_type := 1 ;
      Integer(TGIS_ShapeType.MultiPoint) : geometry_type := 7 ;
      Integer(TGIS_ShapeType.Arc)        : geometry_type := 9 ;
      Integer(TGIS_ShapeType.Polygon)    : geometry_type := 11 ;
    else
      geometry_type := 0 ;
    end ;

    case _dim of
      TGIS_DimensionType.XY  : dimSize := 2 ;
      TGIS_DimensionType.XYZ   : dimSize := 3 ;
      TGIS_DimensionType.XYM   : dimSize := 3 ;
      TGIS_DimensionType.XYZM  : dimSize := 4
    else                    dimSize := 2 ;
    end ;

    oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_UPDATE_GEOMETRY_COLUMNS_TABLE ),
                             [ geometry_type, dimSize,
                               prepareMasterQuery( strCatalog, strSchema, strName, False )
                             ]
                           )
                   ) ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroTableCreate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    geometry_type  : Integer ;
    storage_type   : Integer ;
    dimension_type : Integer ;
    max_ppr        : Integer ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      if isBlob or isWkt then begin

        storage_type := 1 ;
        if isBlob then begin
          oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_BINARY ),
                                   [ getTableWithSchema(strName) ]
                                 )
                         ) ;
        end
        else if isWkt then begin
          oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_WKT ),
                                   [ getTableWithSchema(strName) ]
                                 )
                         ) ;
         storage_type := 2 ;
        end ;

        try
          oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_UID  ),
                           [ strName, getTableWithSchema(strName) ]
                         )
                 ) ;
        except
          // can exists
        end ;

        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_XMIN ),
                         [ strName, getTableWithSchema(strName) ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_XMAX ),
                         [ strName, getTableWithSchema(strName) ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_YMIN ),
                         [ strName, getTableWithSchema(strName) ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_YMAX ),
                         [ strName, getTableWithSchema(strName) ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_EXTENT ),
                         [ strName, getTableWithSchema(strName) ]
                       )
               ) ;
        max_ppr := 0 ;
      end
      else begin

        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATE_TABLE_GEOMETRY_NORMALIZED ),
                         [ getTableWithSchema(strName) ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_NORMALIZED ),
                         [ strName, getTableWithSchema(strName)
                         ]
                       )
               ) ;
        storage_type := 0 ;
        max_ppr := 16 ;

      end ;

      oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATE_TABLE_FEATURES ),
                       [ getTableWithSchema(strName) + getCmd( T_SQLOGIS.ID_FEATURE_SUFFIX ) ]
                     )
             ) ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_CREATEINDEX_UID ),
                         [ strName + getCmd( T_SQLOGIS.ID_FEATURE_SUFFIX ),
                           getTableWithSchema(strName) + getCmd( T_SQLOGIS.ID_FEATURE_SUFFIX )
                         ]
                       )
               ) ;
      except
        // can exists
      end ;

      try
        case Integer(_type) of
          Integer(TGIS_ShapeType.Point)      : geometry_type := 1 ;
          Integer(TGIS_ShapeType.MultiPoint) : geometry_type := 7 ;
          Integer(TGIS_ShapeType.Arc)        : geometry_type := 9 ;
          Integer(TGIS_ShapeType.Polygon)    : geometry_type := 11 ;
        else
          geometry_type := 0 ;
        end ;

        case _dim of
          TGIS_DimensionType.XY : dimension_type := 2 ;
          TGIS_DimensionType.XYZ  : dimension_type := 3 ;
          TGIS_DimensionType.XYZM : dimension_type := 4 ;
        else                   dimension_type := 2 ;
        end ;

        sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_TABLE_WHERE ),
                              [ getCmdSRSTable, 'AUTH_SRID=' + IntToStr( CS.EPSG ) ] ),
                      0
                    ) ;
        if not oGisDb.sqlQueryEof( 0 ) then begin
          SridId := VarToInt32( oGisDb.sqlQueryGetField( 'SRID', 0 ) ) ;
          oGisDb.sqlQueryClose(0) ;
        end
        else begin
          oGisDb.sqlQueryClose(0) ;
          sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_MAX_SRID ), [ getCmdSRSTable ] ), 0 ) ;
          try
            try
              SridId := VarToInt32( oGisDb.sqlQueryGetField( 'MAX_SRID', 0 ) ) ;
            except
              SridId := 0 ;
            end;
            inc( SridId ) ;
          finally
            oGisDb.sqlQueryClose(0) ;
          end;
          oGisDb.sqlExec( Format(
                           getCmd( T_SQLOGIS.ID_INSERT_SPATIAL_REFERENCE_SYSTEMS_TABLE  ),
                           [ getCmdSRSTable, SridId, 'EPSG', CS.EPSG, CS.FullWKT ]
                           )
                         ) ;
        end;

        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_INSERT_GEOMETRY_COLUMNS_TABLE ),
                         [ strCatalog,
                           strSchema,
                           strName + getCmd( T_SQLOGIS.ID_FEATURE_SUFFIX ),
                           getCmd( T_SQLOGIS.ID_GID ),
                           strCatalog,
                           strSchema,
                           strName,
                           storage_type,
                           geometry_type,
                           dimension_type,
                           max_ppr,
                           SridId
                         ]
                       )
               ) ;
      except
        macroMasterUpdate( _extent, _type, '', DefaultDimension )
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroTableAlter(
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
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlQueryClose(0) ;
    oGisDb.sqlTableClose( 0 ) ;

    try
      lname := StrToInt( getCmd( T_SQLOGIS.ID_MAX_NAMELENGTH ) ) ;
    except
      lname := 16 ;
    end ;

    try
      if not IsStringEmpty( getCmd( T_SQLOGIS.ID_MAX_TEXTLENGTH ) ) then
        ltext := StrToInt( getCmd( T_SQLOGIS.ID_MAX_TEXTLENGTH ) )
      else
        ltext := GIS_SQL_MEMO_SIZE ;
    except
      ltext := GIS_SQL_MEMO_SIZE ;
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
              oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_DROP_COLUMN ),
                               [ TableFeatures, fname ]
                             )
                     ) ;
            except
              // name can not exist
            end ;
          end
        else if ( not fld.Saved ) or ( _layer <> Self ) then // create a field
        begin
          if (fld.IsUID) and (CompareText( fname, nameColumnGeometry ) = 0) then begin
            // ignore error and assume that we link index columns
            fld.Saved      := False  ;
            fld.ExportName := fname ;
            fld.Name       := fld.ExportName ;
            fld.Width      := fld.NewWidth   ;
            fld.Decimal    := fld.NewDecimal ;
          end
          else begin
            for cnt := 0 to N_ITER do begin
              try
                case fld.FieldType of
                  TGIS_FieldType.String :
                      if fld.NewWidth <= ltext then
                         oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_STRING ),
                                          [ TableFeatures, safeName(fname), fld.NewWidth ]
                                        )
                                )
                      else
                         oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_MEMO ),
                                          [ TableFeatures, safeName(fname) ]
                                        )
                                ) ;
                  TGIS_FieldType.Date :
                     oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_DATE ),
                                      [ TableFeatures, safeName(fname) ]
                                    )
                            ) ;
                  TGIS_FieldType.Boolean :
                     oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_BOOLEAN ),
                                      [ TableFeatures, safeName(fname) ]
                                    )
                            ) ;
                  TGIS_FieldType.Number :
                       begin
                         if fld.NewDecimal = 0 then begin
                           if fld.Width <= 9 then
                             oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_INTEGER ),
                                              [ TableFeatures, safeName(fname) ]
                                           )
                                    )
                           else
                             oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_BIGINT ),
                                              [ TableFeatures, safeName(fname) ]
                                            )
                                   ) ;
                         end
                         else
                           oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_FLOAT ),
                                            [ TableFeatures, safeName(fname) ]
                                          )
                                  ) ;
                       end ;
                  TGIS_FieldType.Float :
                       oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ADD_FLOAT ),
                                        [ TableFeatures, safeName(fname) ]
                                      )
                              ) ;
                  else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ),
                                                    '',
                                                    0
                                                  ) ;
                end ;
                break ;
              except
                fname := Format( '%s%.2x',
                                 [ Copy( fname, StringFirst, name_len -2 ), cnt ]
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
        end
        else begin
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
              oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_ALTER_ALTER_STRING ),
                               [ TableFeatures, safeName(fname), fld.NewWidth ]
                             )
                     ) ;
            except
              // could fail on a number of databases
              fld.NewWidth := fld.Width ;
            end ;
          end
        end ;
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroTableDrop ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      try
        sqlQueryOpen(
           Format(
             getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE ),
             [ getCmdSRSTable,
               prepareMasterQuery( strCatalog, strSchema, strName, True )
             ]
           ),0
        ) ;
        try
          if not oGisDb.sqlQueryEof(0) then begin
            nameTableGeometry  := VarToString(
              oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_G_TABLE_NAME ), 0 )
              ) ;
            nameTableFeatures  := VarToString(
              oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_F_TABLE_NAME ), 0 )
              ) ;
          end ;
        finally
          oGisDb.sqlQueryClose(0) ;
        end;

      except
        nameTableGeometry := '' ;
        // can no exist
      end ;

      try
        if not IsStringEmpty( nameTableGeometry ) then
          oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_DROP_TABLE ), [ TableGeometry ] ) ) ;
      except
        // can no exist
      end ;

      try
        if not IsStringEmpty( nameTableFeatures ) then
          oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_DROP_TABLE ), [ TableFeatures ] ) ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_DELETE_GEOMETRY_COLUMNS_TABLE ),
                        [ prepareMasterQuery( strCatalog, strSchema, strName, False ) ]
                       )
                ) ;
      except
        // can no exist
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroTableSetField(
    const _id     : Integer ;
    const _name   : String  ;
    const _val    : Variant ;
    const _force  : Boolean
  ) ;
  var
    i : Integer ;
  begin
    if IsReadOnly then exit ;

    i := FindField( _name ) ;
    try
      if ( not _force  ) and ( i >= 0 ) and ( not FieldInfo( i ).FileFormat ) then begin
        // field from layer
        oGisDb.sqlTableSetField( _id, FieldInfo(i).ExportName, _val, FieldInfo(i).NewWidth )
      end
      else begin
        // internal field
        oGisDb.sqlTableSetField( _id, _name, _val, 1 ) ;
      end ;
    except
      // maybe field name was changed by someone else.....
      assert( False, 'Field ''' + _name + ''' does not exists.' ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, True, 0 ) ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroAddField(
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

  procedure TGIS_LayerSqlOgisAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer        ;
     const _saved   : Boolean        ;
     const _binary  : Integer
  ) ;
  begin
    AddFieldInternal( _name, _type, _width, _decimal, _saved, _binary ) ;

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
  end;

  procedure TGIS_LayerSqlOgisAbstract.macroQueryStructure ;
  begin
    Fields.Clear ;

    // try to return en empty set to save the time
    sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_TABLE_WHERE ),
                          [ TableFeatures,
                            '1=0'
                          ]
                        ), 0
                ) ;
    try
      {$IFNDEF OXYGENE}
        oGisDb.sqlQueryStructure( TableFeatures, nameColumnGeometry,  macroAddField ) ;
      {$ELSE}
        oGisDb.sqlQueryStructure( TableFeatures, nameColumnGeometry, @macroAddField ) ;
      {$ENDIF}
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    ReadFieldRules ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroShapeDelete(
    const _uid  : TGIS_Uid
  ) ;
  begin
    if IsReadOnly then exit ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_DELETE_SHAPE ),
                       [ TableGeometry,
                         nameColumnGeometry,
                         _uid
                       ]
                     )
             ) ;
    except
    end ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLOGIS.ID_DELETE_SHAPE ),
                       [ TableFeatures,
                         nameColumnGeometry,
                         _uid
                       ]
                     )
              ) ;
    except
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroShapeUpdate(
    const _shp    : TGIS_Shape ;
    const _import : Boolean
  ) ;
  var
    i         : Integer        ;
    fldinfo   : TGIS_FieldInfo ;
    fld       : Variant        ;
    uid       : TGIS_Uid        ;
    txt       : String         ;
    iloop     : Integer        ;
    guaranted : Boolean        ;
    skipShape : Boolean        ;
    skipField : Boolean        ;
  begin
    if IsReadOnly then exit ;

    guaranted := False ;

    oGisDb.sqlTransactUpdateStart ;
    try
      uid := -1 ;

      iloop := T_SQLOGIS.OGIS_RETRY_COUNT ;
      while iloop > 0 do begin
        try
          skipShape := False ;
          // update geometry
          if _import then begin
            uid := lastUid ;
            if uid < 0 then uid := macroUidNew( guaranted )
                       else inc( uid ) ;
            if not guaranted then
              lastUid := uid ;

            if iloop = T_SQLOGIS.OGIS_RETRY_COUNT then begin
              sqlTableAppend( 0, TableGeometry ) ;
            end;

            if IsStringEmpty( nameColumnGeometry ) then
              macroTableSetField( 0, getCmd( T_SQLOGIS.ID_GID ) , uid, True )
            else
              macroTableSetField( 0, nameColumnGeometry, uid, True ) ;
          end
          else begin
            uid := _shp.Uid ;
            if ( MultiUserMode = TGIS_MultiUser.SingleUser ) and _shp.IsNewShape then begin
              sqlTableAppend( 0, TableGeometry ) ;
              if IsStringEmpty( nameColumnGeometry ) then
                macroTableSetField( 0, getCmd( T_SQLOGIS.ID_GID ) , uid, True )
              else
                macroTableSetField( 0, nameColumnGeometry, uid, True ) ;
            end
            else begin
              skipShape := not _shp.GeometryChanged and not _shp.IsNewShape ;
              if not skipShape then
                sqlTableOpenWrite( 0, TableGeometry, nameColumnGeometry, uid ) ;
            end;
          end ;

          if isBlob or isWkt then begin
            if not skipShape then begin
              macroTableSetField( 0, getCmd( T_SQLOGIS.ID_XMIN ), _shp.Extent.XMin, True ) ;
              macroTableSetField( 0, getCmd( T_SQLOGIS.ID_XMAX ), _shp.Extent.XMax, True ) ;
              macroTableSetField( 0, getCmd( T_SQLOGIS.ID_YMIN ), _shp.Extent.YMin, True ) ;
              macroTableSetField( 0, getCmd( T_SQLOGIS.ID_YMAX ), _shp.Extent.YMax, True ) ;

              if isBlob then
                sqlTableSetGeometry( 0, getCmdGEOMETRY, _shp )
              else
                sqlTableSetGeometry( 0, getCmdGEOMETRYWkt, _shp ) ;
            end ;
          end
          else begin
            if not skipShape then
              sqlTableSetGeometry( 0, '', _shp ) ;
          end;

          if not skipShape then begin
            sqlTablePost( 0 ) ;
          end;

          iloop := 0 ;
        except
          if not guaranted then begin
            dec( iloop ) ;
            if iloop <= 0 then raise ;

            Sleep( GetRandom( T_SQLOGIS.OGIS_RETRY_INTERVAL ) ) ;
            lastUid := GetLastUid ;
          end
          else
            break ;
        end ;
      end ;

    finally
      oGisDb.sqlTransactUpdateCommit ;
    end ;

    // do not update feature tables if based on views
    // or fields do not exist
    if ( ViewFeatures = TableFeatures ) then begin

      // update features
      if _import or _shp.IsNewShape then begin
        skipField := canSkipField and not oGisDb.IsPostgreSql and _shp.IsNewShape ;
        sqlTableAppend( 1, TableFeatures ) ;

        if IsStringEmpty( nameColumnGeometry ) then
          macroTableSetField( 1, getCmd( T_SQLOGIS.ID_GID ), uid, True )
        else
          macroTableSetField( 1, nameColumnGeometry, uid, True ) ;
      end
      else begin
        if not _shp.FieldChanged then exit ;

        skipField := canSkipField and not oGisDb.IsPostgreSql ;
        sqlTableOpenWrite( 1, TableFeatures, nameColumnGeometry, uid ) ;
      end ;

      for i:=0 to Fields.Count - 1 do begin

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
          fld := _shp.GetFieldEx( nameColumnGeometry )
        else
          fld := _shp.GetFieldEx( fldinfo.NewName ) ;

        case fldinfo.FieldType of
          TGIS_FieldType.String :
             begin
               if VarIsNull( fld ) then
                 macroTableSetField( 1, fldinfo.NewName, fld, False )
               else begin
                 txt := VarToString( fld ) ;
                 macroTableSetField( 1, fldinfo.NewName, txt, False ) ;
               end ;
             end ;
          TGIS_FieldType.Date :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld, False ) ;
             end ;
          TGIS_FieldType.Boolean :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld, False )
             end ;
          TGIS_FieldType.Number :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld, False ) ;
             end ;
          TGIS_FieldType.Float :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld, False ) ;
             end ;
          else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
        end ;
      end ;

      sqlTablePost( 1 )  ;
    end ;
  end ;

  function TGIS_LayerSqlOgisAbstract.macroUidLast : TGIS_Uid ;
  var
    v : Variant ;
  begin
    sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_MAX_GID ),
                          [ TableGeometry ]
                        ), 0
                ) ;
    try
      try
        v := oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_MAX_GID ), 0 ) ;
        if VarIsNull( v ) then Result := 0
                          else Result := VarToInt64( v ) ;
      except
        Result := 0 ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.macroSetSharedConnection(
    const _ll : TGIS_LayerVector
  ) ;
  begin

  end ;

  function TGIS_LayerSqlOgisAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
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
          // check if we really have opengis structure
          sqlQueryOpen(
             Format(
               getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE ),
               [  getCmdSRSTable,
                  prepareMasterQuery( strCatalog, strSchema, strName, True )
               ]
             ), 0
          ) ;
          oGisDb.sqlQueryClose(0) ;

          oGisDb.sqlQueryOpen( getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE_ALL ), 0 ) ;
          while not oGisDb.sqlQueryEof(0) do begin
            lname := VarToString( oGisDb.sqlQueryGetFieldById( 0,0 ) ) ;
            ltype := VarToInt32(  oGisDb.sqlQueryGetFieldById( 1,0 ) ) ;

            case ltype of
                  0     : defShpType := TGIS_ShapeType.Point      ;
                  1     : defShpType := TGIS_ShapeType.Point      ;
                  7     : defShpType := TGIS_ShapeType.MultiPoint ;
                  3, 9  : defShpType := TGIS_ShapeType.Arc        ;
                  5, 11 : defShpType := TGIS_ShapeType.Polygon    ;
            else          defShpType := TGIS_ShapeType.Point      ;
            end;

            Result.Add(
              TGIS_LayerInfo.Create( lname,
                                     TGIS_RegisteredLayerType.Vector,
                                     defShpType
                                    )
            ) ;

            oGisDb.sqlQueryMoveNext(0) ;
          end;
        except
          // can not exist
        end;
      finally
        oGisDb.sqlQueryClose(0) ;
        macroDisconnect ;
      end;
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end;
  end;

  function TGIS_LayerSqlOgisAbstract.macroUidReserve : TGIS_Uid ;
  var
    iloop : Integer ;
    uid   : TGIS_Uid ;
    guaranted : Boolean ;
  begin
    Result := -1 ;

    if IsReadOnly then exit ;

    guaranted := False ;
    macroUpdateStart ;
    try
      uid := -1 ;

      iloop := T_SQLOGIS.OGIS_RETRY_COUNT ;
      while iloop > 0 do begin
        try
          if uid < 0 then uid := macroUidNew( guaranted )
                     else inc( uid ) ;

          if iloop = T_SQLOGIS.OGIS_RETRY_COUNT then
            sqlTableAppend( 0, TableGeometry ) ;
          macroTableSetField( 0, getCmd( T_SQLOGIS.ID_GID ) , uid, True) ;

          if isBlob or isWkt then begin
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_XMIN ), GisNoWorld.XMin, True  ) ;
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_XMAX ), GisNoWorld.XMax, True  ) ;
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_YMIN ), GisNoWorld.YMin, True  ) ;
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_YMAX ), GisNoWorld.YMax, True  ) ;
          end
          else begin
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_ESEQ  ), 0, True ) ;
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_ETYPE ), 0, True ) ;
            macroTableSetField( 0, getCmd( T_SQLOGIS.ID_SEQ   ), 0, True ) ;
          end ;

          sqlTablePost( 0 ) ;
          iloop := 0 ;
        except
          if not guaranted then begin
            dec( iloop ) ;
            if iloop <= 0 then raise ;
            Sleep( GetRandom( T_SQLOGIS.OGIS_RETRY_INTERVAL ) ) ;
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

  function TGIS_LayerSqlOgisAbstract.macroFetchRecord(
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

    if not isBlob and not isWkt then
      if assigned( cursorSql[_cursor].currShape ) and
        ( cursorSql[_cursor].currShape.Uid = _uid ) then
        fetch := False ;

    if fetch then begin
      oGisDb.sqlQueryClose(_cursor) ;
      sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_UID ),
                            [ ViewFeatures, TableGeometry,
                              nameColumnGeometry,
                              _uid,
                              nameColumnGeometry,
                              nameColumnGeometry
                            ]
                          ), _cursor
                  ) ;
    end ;

    Result := fetch ;
  end ;

  function TGIS_LayerSqlOgisAbstract.macroUidNew(
    var _guaranted : Boolean
  ) : TGIS_Uid ;
  begin
    _guaranted := False ;
    Result := macroUidLast + 1 ;
  end ;

  function TGIS_LayerSqlOgisAbstract.getFieldInternal(
    const _uid     : TGIS_Uid  ;
    const _name    : String ;
    const _cursor  : Integer
  ) : Variant ;
  begin
    lockThread ;
    try
      if (_uid < 0) then begin
        Result := Unassigned ;
        exit ;
      end ;

      macroFetchRecord( _uid, _cursor ) ;

      if not oGisDb.sqlQueryEof(_cursor) then begin
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

  procedure TGIS_LayerSqlOgisAbstract.setUp ;
  var
    ext     : TGIS_Extent ;
    v       : Variant     ;
  begin
    FUseRTree := False ;

    inherited ;

    macroConnect ;

    MultiUserMode := oGisDb.MultiUserMode ;

    sqlQueryOpen(
       Format(
         getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_COLUMNS_TABLE ),
         [  getCmdSRSTable,
            prepareMasterQuery( strCatalog, strSchema, strName, True )
         ]
       ), 0
    ) ;
    try
      if oGisDb.sqlQueryEof(0) then begin
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), Table, 0 ) ;
      end ;

      case VarToInt32(oGisDb.sqlQueryGetField(getCmd(T_SQLOGIS.ID_COORD_DIMENSION),0)) of
        2 : DefaultDimension := TGIS_DimensionType.XY ;
        3 : DefaultDimension := TGIS_DimensionType.XYZ  ;
        4 : DefaultDimension := TGIS_DimensionType.XYZM  ;
      else  DefaultDimension := TGIS_DimensionType.Unknown ;
      end ;

      try
        case VarToInt32( oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_GEOMETRY_TYPE ),0 ) ) of
          0     : begin
                    DefaultShapeType := TGIS_ShapeType.Point ;
                    layerstp         := TGIS_ShapeType.Point ;
                  end ;
          1     : begin
                    DefaultShapeType := TGIS_ShapeType.Point ;
                    layerstp         := TGIS_ShapeType.Point ;
                  end ;
          7     : begin
                    DefaultShapeType := TGIS_ShapeType.MultiPoint ;
                    layerstp         := TGIS_ShapeType.MultiPoint ;
                  end ;
          3, 9  : begin
                    DefaultShapeType := TGIS_ShapeType.Arc ;
                    layerstp         := TGIS_ShapeType.Arc ;
                  end ;
          5, 11 : begin
                    DefaultShapeType := TGIS_ShapeType.Polygon ;
                    layerstp         := TGIS_ShapeType.Polygon ;
                  end
          else    begin
                    DefaultShapeType := TGIS_ShapeType.Point ;
                    layerstp         := TGIS_ShapeType.Point ;
                  end ;
        end ;
      except
        DefaultShapeType := TGIS_ShapeType.Point ;
      end ;

      nameColumnGeometry := VarToString(
        oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_F_GEOMETRY_COLUMN ),0 )
        ) ;
      nameTableGeometry  := VarToString(
        oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_G_TABLE_NAME ),0 )
        ) ;
      nameTableFeatures  := VarToString(
        oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_F_TABLE_NAME ),0 )
        ) ;

      valueMaxPPR := VarToInt32( oGisDb.sqlQueryGetField( getCmd ( T_SQLOGIS.ID_MAX_PPR ),0 ) ) ;

      v := oGisDb.sqlQueryGetField( getCmd ( T_SQLOGIS.ID_SRTEXT ),0 ) ;
      if not( VarIsNull( v ) or VarIsEmpty( v ) ) then
        SetCSByWKT( VarToString( v ) ) 
      else
        SetCSByEPSG(0) ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end;

    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    if isBlob or isWkt then begin
      if GisIsNoWorld( Extent ) then begin
        try
          sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_GEOMETRY_EXTENT ),
                                [ TableGeometry ]
                              ),0
                      ) ;
          try
            v := oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_MIN_XMIN ),0 ) ;
            if VarIsNull( v ) or VarIsEmpty( v) then
              Abort ;
            ext.XMin := VarToDouble( v ) ;

            v := oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_MAX_XMAX ),0 ) ;
            if VarIsNull( v ) or VarIsEmpty( v) then
              Abort ;
            ext.XMax := VarToDouble( v ) ;

            v := oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_MIN_YMIN ),0 ) ;
            if VarIsNull( v ) or VarIsEmpty( v) then
              Abort ;
            ext.YMin := VarToDouble( v ) ;

            v := oGisDb.sqlQueryGetField( getCmd( T_SQLOGIS.ID_MAX_YMAX ),0  ) ;
            if VarIsNull( v ) or VarIsEmpty( v) then
              Abort ;
            ext.YMax := VarToDouble( v ) ;
          finally
            oGisDb.sqlQueryClose(0) ;
          end ;

          Extent := ext ;
        except
          Extent := GisNoWorld ;
        end ;
      end
    end ;

    oGisDb.GeometryType := TGIS_SubDataType.Native ;

    macroQueryStructure ;

    fixGEOUID := -1 ;

    FFileInfo := 'OpenGIS SQL Vector Coverage (TTKLS)' + #13#10  ;
    if isBlob then
      FFileInfo := FFileInfo + 'Binary Geometry'
    else if isWkt then
      FFileInfo := FFileInfo + 'WKT Geometry'
    else
      FFileInfo := FFileInfo + 'Normalized Geometry' ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    ll : TGIS_LayerSqlOgisAbstract ;
  begin
    inherited ;

    if IsReadOnly then exit ;

    if not IsStringEmpty( _path ) then begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          ll := TGIS_LayerSqlOgisAbstract(&Class.forName(Self.Class.Name).getConstructor().newInstance());
        {$ELSE}
          ll := TGIS_LayerSqlOgisAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
        {$ENDIF}
      {$ELSE}
        ll := TGIS_LayerSqlOgisClass( Self.ClassType ).Create ;
      {$ENDIF}
      ll.FSQLParameters.Assign( FSQLParameters );
      try
        ll.Path := _path ;
        ll.oGisDb.SQLExecuteEvent := oGisDb.SQLExecuteEvent ;
        ll.CS := CS ;
        ll.PasswordEvent := PasswordEvent ;
        macroSetSharedConnection( ll ) ;
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
        macroTableCreate( _extent, _type, _dim )  ;
      finally
        macroDisconnect ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    lname       : Integer        ;
    shp         : TGIS_Shape     ;
    shp_tmp     : TGIS_Shape     ;
    shp_type    : TGIS_ShapeType ;
    shp_dim     : TGIS_DimensionType ;
    first       : Boolean        ;
    shape_no    : Cardinal       ;
    end_uid     : TGIS_Uid        ;
    abort       : Boolean        ;
    old_view    : String         ;
    eloop       : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;

    shape_no   := 0 ;
    end_uid    := _layer.GetLastUid ;
    abort      := False  ;

    Extent     := _TGIS_Extent( _layer.Extent ) ;
    shp_type   := _type ;
    first      := True  ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;

    old_view   := FViewFeatures ;
    try
      try
        macroDisconnect ;
        macroConnect ;
        macroTableDrop ;
        macroDisconnect ;
      except
        // could be nonexisting database
      end ;

      ViewFeatures := '' ;
      Build( Path, GisExtent( 0, 0, 0, 0 ), _type, _layer.DefaultDimension ) ;

      shp_dim := _layer.DefaultDimension ;
      SupportedDimensions := _layer.SupportedDimensions ;

      macroConnect ;

      ViewFeatures := '' ;
      Fields.Clear ;
      ImportStructure( _layer ) ;

      try
        lname := StrToInt( getCmd( T_SQLOGIS.ID_MAX_NAMELENGTH ) ) ;
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
                   ( ( shp_tmp.ShapeType=shp_type   ) or
                     ( shp_type=TGIS_ShapeType.Unknown )
                   )
                then begin
                 // calculate extent
                 if first then begin
                   Extent   := _TGIS_Extent( shp_tmp.Extent )  ;
                   first    := False ;
                   shp_dim  := shp_tmp.Dimension ;
                 end
                 else
                   Extent := GisMaxExtent( Extent, shp_tmp.Extent ) ;

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
        macroMasterUpdate( Extent, shp_type, '', shp_dim ) ;

        macroUpdateEnd ;
        macroEndBatchMode ;
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

  procedure TGIS_LayerSqlOgisAbstract.MergeLayerEx (
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
    lname       : Integer        ;
    shp         : TGIS_Shape     ;
    shp_tmp     : TGIS_Shape     ;
    shp_type    : TGIS_ShapeType ;
    first       : Boolean        ;
    shape_no    : Cardinal       ;
    end_uid     : TGIS_Uid        ;
    abort       : Boolean        ;
    old_view    : String         ;
    eloop       : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;
    abort   := False ;

    old_view := FViewFeatures ;

    macroDisconnect ;
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
        lname := StrToInt( getCmd( T_SQLOGIS.ID_MAX_NAMELENGTH ) ) ;
      except
        lname := 16 ;
      end ;
      ViewFeatures := '' ;

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
                     Extent   := _TGIS_Extent( shp_tmp.Extent ) ;
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
        macroMasterUpdate( Extent, shp_type, Table, DefaultDimension ) ;

        macroUpdateEnd ;
        FIsModified := False ;
      end ;
    finally
      ViewFeatures := old_view ;

      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.ReStructure ;
  begin
    macroQueryStructure ;
  end ;

  function TGIS_LayerSqlOgisAbstract.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0 ;
      else
        Result := 1 ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited;
    oGisDb.sqlQueryClose(0);
    oGisDb.sqlTableClose(0) ;
    oGisDb.sqlTableClose(1) ;
  end ;

  function  TGIS_LayerSqlOgisAbstract.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorSql)  then
        SetLength( cursorSql, Result + 1 ) ;

      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorSql[Result] ) then
          cursorSql[Result] := new T_cursorSql_LayerSqlOgis ;
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
  end;

  procedure TGIS_LayerSqlOgisAbstract.cursorClose(
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

  procedure TGIS_LayerSqlOgisAbstract.cursorFirst(
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
    useIndex    : Boolean ;
    pixelfilter : String ;
    psize       : Double ;
    ssize       : Integer ;

    function mf( const _s1,_f,_s2 : String ) : String ;
    begin
      if IsStringEmpty( _f ) then
        Result := ''
      else
        Result := _s1 + _f + _s2 ;
    end ;

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
           IsStringEmpty( JoinPrimary ) and not cursorSql[_cursor].disableJoinPrimary  then
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

        if assigned( Viewer ) and Viewer.Ref.InPaint then begin
          ssize := Min( Viewer.Ref.TwipsToPixels( Params.Line.SmartSize ),
                        Viewer.Ref.TwipsToPixels( Params.Area.SmartSize )
                       ) ;
          psize := ssize / ( findSmartSizeFactor * Viewer.Ref.Zoom ) ;
          if ( psize > 0 ) and
             ( layerstp <> TGIS_ShapeType.Point   ) and
             ( layerstp <> TGIS_ShapeType.Unknown ) then
            pixelfilter := Format( '((%s-%s)>%s OR (%s-%s)>%s)',
                                  [ getCmd( T_SQLOGIS.ID_XMAX ), getCmd( T_SQLOGIS.ID_XMIN ),
                                    DotFloatToStr( psize ),
                                    getCmd( T_SQLOGIS.ID_YMAX ), getCmd( T_SQLOGIS.ID_YMIN ),
                                    DotFloatToStr( psize )
                                   ]
                                 )
          else
            pixelfilter := '' ;
        end
        else
          pixelfilter := '' ;

        if GisIsWholeWorld( cursorState[ _cursor ].curRawExtent ) or
           GisIsNoWorld( cursorState[ _cursor ].curRawExtent ) then
          useIndex := False
        else
          useIndex := ( GisExtentArea( cursorState[ _cursor ].curExtent ) <
                        GisExtentArea( Extent ) / 4
                       ) ;

        if GisIsWholeWorld( cursorState[ _cursor ].curRawExtent ) or
            not useIndex then begin

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then begin
            if isBlob or isWkt then
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_NOEXT ),
                                    [ ViewFeatures, TableGeometry,
                                      mf('',pixelfilter,' AND '),
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      order
                                    ]
                                  ), _cursor
                          )
            else
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT ),
                                    [ ViewFeatures, TableGeometry,
                                      '',
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      nameColumnGeometry
                                    ]
                                  ), _cursor
                          )
          end
          else begin
            if isBlob or isWkt then
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_NOEXT_EX ),
                                    [ ViewFeatures, TableGeometry,
                                      sqlquery,
                                      mf('',pixelfilter,' AND '),
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      order
                                    ]
                                  ), _cursor
                          )
            else
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT_EX ),
                                    [ ViewFeatures, TableGeometry,
                                      sqlquery,
                                      '',
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      nameColumnGeometry
                                    ]
                                  ), _cursor
                          ) ;
          end

        end
        else begin

          ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                                 GisExtent( -1E37, -1E37, 1E37, 1E37 )
                               ) ;

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then begin
            if isBlob or isWkt then
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN ),
                                    [ ViewFeatures, TableGeometry,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax ),
                                      mf(' AND ',pixelfilter,''),
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      order
                                    ]
                                  ), _cursor
                          )
            else
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT ),
                                    [ ViewFeatures, TableGeometry,
                                      '',
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      nameColumnGeometry
                                    ]
                                  ), _cursor
                          );
          end
          else begin
            if isBlob or isWkt then
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_EX ),
                                    [ ViewFeatures, TableGeometry,
                                      sqlquery,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax ),
                                      mf(' AND ',pixelfilter,''),
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      order
                                    ]
                                  ), _cursor
                          )
            else
              sqlQueryOpen( Format( getCmd( T_SQLOGIS.ID_SELECT_JOIN_NORMALIZED_NOEXT_EX ),
                                    [ ViewFeatures, TableGeometry,
                                      sqlquery,
                                      '',
                                      nameColumnGeometry,
                                      nameColumnGeometry,
                                      nameColumnGeometry
                                    ]
                                  ), _cursor
                          ) ;
          end ;

        end ;

        cursorSql[_cursor].isEof := False ;
        cursorSql[_cursor].isFirst := True ;

        cursorNext(_cursor) ;
      except
        cursorSql[_cursor].isEof := True ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.cursorNext(
    const _cursor : Integer
  ) ;
  var
    ihglass : Integer ;
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

          cursorSql[_cursor].currShape := nil   ;

          if isBlob or isWkt then begin
            if ( not oGisDb.sqlQueryEof(_cursor) ) and
               not cursorSql[_cursor].isFirst
            then
              oGisDb.sqlQueryMoveNext(_cursor) ;
          end ;

          cursorSql[_cursor].isFirst := False ;

          if not oGisDb.sqlQueryEof(_cursor) then begin
            readShape(_cursor) ;
          end ;

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

  function TGIS_LayerSqlOgisAbstract.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorSql[_cursor].isEof ;
  end ;

  function TGIS_LayerSqlOgisAbstract.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorSql[_cursor].currShape ) then
       Result := cursorSql[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerSqlOgisAbstract.GetShape(
    const _uid     : TGIS_Uid ;
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
                     Format( getCmd( T_SQLOGIS.ID_FILTER_UID_WEAK ),
                             [ GIS_FIELD_UID, _uid,
                               GIS_FIELD_UID, _uid + T_SQLOGIS.OGIS_GETSHAPE_FETCHED_LIMIT
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

  function TGIS_LayerSqlOgisAbstract.GetLastUid : TGIS_Uid ;
  var
    tmp : TGIS_Uid ;
  begin
    lockThread ;
    try
      if MultiUserMode = TGIS_MultiUser.SingleUser then begin
        if lastNewUid = -1 then
          lastNewUid := macroUidLast ;

        Result := lastNewUid
      end
      else begin
        tmp    := macroUidLast ;
        Result := inherited GetLastUid ;
        if tmp > Result then
          Result := tmp ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlOgisAbstract.GetNewUid : TGIS_Uid ;
  begin
    lockThread ;
    try
      if MultiUserMode = TGIS_MultiUser.SingleUser then begin
        inc( lastNewUid ) ;
        Result := lastNewUid
      end
      else
        Result := macroUidReserve ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.SaveData ;
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
        done := True ;
      finally
        if done then begin
          // update master table
             if not first then
               macroMasterUpdate( Extent, DefaultShapeType, Table, DefaultDimension ) ;

             Items.Clear ;
        end ;
        macroUpdateEnd ;
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

  procedure TGIS_LayerSqlOgisAbstract.parseConfigLayerName ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    if not IsStringEmpty( FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ] ) then begin
      isBlob     := ( CompareText(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ],
                        GIS_INI_LAYERSQL_OPENGISNORMALIZED
                      ) <> 0
                    ) and
                    (
                      CompareText(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ],
                        GIS_INI_LAYERSQL_OPENGISNORMALIZED2
                      ) <> 0
                    ) and
                    (
                      CompareText(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ],
                        GIS_INI_LAYERSQL_OPENGISWKT
                      ) <> 0
                    )  ;
      isWkt      := CompareText(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ],
                        GIS_INI_LAYERSQL_OPENGISWKT
                      ) = 0 ;
      isSRSshort := ( CompareText(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ],
                        GIS_INI_LAYERSQL_OPENGISBLOB2
                      ) = 0
                    ) or
                    (
                      CompareText(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_STORAGE ],
                        GIS_INI_LAYERSQL_OPENGISNORMALIZED2
                      ) = 0
                    ) ;
    end
    else begin
      isBlob     := FSQLParameters.Values[ GIS_INI_LAYERSQL_OPENGIS ] = '1' ;
      isSRSshort := False ;
    end ;

    tkn := TGIS_Tokenizer.Create ;
    try
    {$IFNDEF OXYGENE}
      with tkn do begin
        ExecuteEx( Table, ';', ' ' ) ;
        if ( ( Result.Count = 0 ) or ( IsStringEmpty( Result.Strings [ 0 ] ) ) ) then
          raise EGIS_Exception.Create( GIS_RS_ERR_BADPARAM,
                                       GIS_INI_LAYERSQL_LAYER,
                                       0
                                     ) ;
        case Result.Count of
          1 : begin
                strName    := Result[0] ;
                strSchema  := ''        ;
                strCatalog := ''        ;
              end ;
          2 : begin
                strName    := Result[0] ;
                strSchema  := Result[1] ;
                strCatalog := ''        ;
              end ;
          3 : begin
                strName    := Result[0] ;
                strSchema  := Result[1] ;
                strCatalog := Result[2] ;
              end ;
        end ;
      end ;
    {$ELSE}
        tkn.ExecuteEx( Table, ';', ' ' ) ;
        if ( ( tkn.Result.Count = 0 ) or ( IsStringEmpty( tkn.Result.Strings [ 0 ] ) ) ) then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                       GIS_INI_LAYERSQL_LAYER,
                                       0
                                     ) ;
        case tkn.Result.Count of
          1 : begin
                strName    := tkn.Result[0] ;
                strSchema  := ''            ;
                strCatalog := ''            ;
              end ;
          2 : begin
                strName    := tkn.Result[0] ;
                strSchema  := tkn.Result[1] ;
                strCatalog := ''            ;
              end ;
          3 : begin
                strName    := tkn.Result[0] ;
                strSchema  := tkn.Result[1] ;
                strCatalog := tkn.Result[2] ;
              end ;
        end ;
    {$ENDIF}
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.readShape(
    const _cursor : Integer
  ) ;
  var
    uid       : Integer    ;
    shapetype : TGIS_ShapeType ;
  begin
    uid       := VarToInt32( sqlQueryGetGEOUID(_cursor) ) ;
    shapetype := TGIS_ShapeType.Unknown ;

    sqlQueryGetGeometry( uid, shapetype,_cursor ) ;

    cursorSql[_cursor].currShape := getEdited( cursorSql[_cursor].currShape ) ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.sqlQueryGetGeometry(
     const _uid       : TGIS_Uid ;
     const _shapetype : TGIS_ShapeType ;
     const _cursor    : Integer
   ) ;
  var
    i         : Integer ;
    next_GID  : Integer ;
    curr_GID  : Integer ;
    curr_ESEQ : Integer ;
    mem_ESEQ  : Integer ;
    v1,v2     : Variant ;
    v3,v4     : Variant ;
    bunknown  : Boolean ;
    varStore  : OleVariant  ;
    dim       : Integer ;
    x,y,z,m   : Double ;
    useval    : Boolean ;
  begin
    cursorSql[_cursor].currShape := nil ;

    if isBlob then begin
      useval := oGisDb.IsPostgreSql or oGisDb.UseTextParameters ;
      varStore := oGisDb.sqlQueryGetGeomVAR( getCmdGEOMETRY, '', _cursor, useval ) ;

      try
        cursorSql[_cursor].currShape := TGIS_GeometryFactory.GisCreateShapeFromWKB(
          varStore, nil, nil, False, 0, nil
        ) ;
      except
        cursorSql[_cursor].currShape := nil ;
      end ;

      bunknown := not assigned( cursorSql[_cursor].currShape ) ;
    end
    else if isWkt then begin
      varStore := oGisDb.sqlQueryGetField( getCmdGEOMETRYWkt, _cursor ) ;

      try
        cursorSql[_cursor].currShape := TGIS_GeometryFactory.GisCreateShapeFromWKT(
          VarToString( varStore ), nil, nil, False, 0, nil
        ) ;
      except
        cursorSql[_cursor].currShape := nil ;
      end ;

      bunknown := not assigned( cursorSql[_cursor].currShape ) ;
    end
    else begin

      if valueMaxPPR <= 0 then exit ;

      if fldX1offset < 0 then begin
        fldX1offset := oGisDb.sqlQueryGetFieldIndex( 'X1',_cursor );
      end ;

      if fldX1offset < 0 then
        exit ; // X1 column not found

      case StrToInt( VarToString( oGisDb.sqlQueryGetField( 'ETYPE',_cursor ) ) ) of
        1 : cursorSql[_cursor].currShape := TGIS_ShapePoint.Create  (
            nil, nil, False, _uid, self, DefaultDimension
          ) ;
        2 : cursorSql[_cursor].currShape := TGIS_ShapeArc.Create    (
            nil, nil, False, _uid, self, DefaultDimension
          ) ;
        3 : cursorSql[_cursor].currShape := TGIS_ShapePolygon.Create(
            nil, nil, False, _uid, self, DefaultDimension
          ) ;
      end;

      bunknown := not assigned( cursorSql[_cursor].currShape ) ;

      if bunknown then
        cursorSql[_cursor].currShape := TGIS_ShapePolygon.Create(
          nil, nil, False, _uid, self, DefaultDimension
        ) ;

      // copy all fields because record set will be set always one record after
      // current shape
      for i := 1 to Fields.Count -1 do begin
        cursorSql[_cursor].currShape.SetField(
          FieldInfo( i ).NewName,
          cursorSql[_cursor].currShape.GetFieldEx( FieldInfo( i ).Name )
        ) ;
      end ;

      cursorSql[_cursor].currShape.Lock( TGIS_Lock.Internal ) ;

      curr_GID := VarToInt32( oGisDb.sqlQueryGetField( sqlQueryNameGEOUID(_cursor),
                                                       _cursor ) ) ;
      next_GID := curr_GID ;
      mem_ESEQ := 0        ;
      case DefaultDimension of
        TGIS_DimensionType.XYZ  : dim := 3 ;
        TGIS_DimensionType.XYM  : dim := 3 ;
        TGIS_DimensionType.XYZM : dim := 4
      else                   dim := 2 ;
      end;

      while ( not oGisDb.sqlQueryEof(_cursor) ) and ( next_GID = curr_GID ) do begin

        curr_ESEQ := VarToInt32( oGisDb.sqlQueryGetField( 'ESEQ',_cursor ) ) ;
        if mem_ESEQ <> curr_ESEQ then
          cursorSql[_cursor].currShape.AddPart ;
        mem_ESEQ := curr_ESEQ ;

        i := 0 ;
        while i <= ( valueMaxPPR - 1 ) * dim do begin
          v1 := oGisDb.sqlQueryGetFieldById( fldX1offset + i,_cursor ) ;
          v2 := oGisDb.sqlQueryGetFieldById( fldX1offset + i+1,_cursor ) ;
          if dim > 2 then
            v3 := oGisDb.sqlQueryGetFieldById( fldX1offset + i+2,_cursor ) ;
          if dim > 3 then
            v4 := oGisDb.sqlQueryGetFieldById( fldX1offset + i+3,_cursor ) ;

          if ( not VarIsNull( v1 ) ) and ( not VarIsNull( v2 ) ) then begin
            x := VarToDouble( v1 ) ;
            y := VarToDouble( v2 ) ;

            if ( dim > 2 ) and not VarIsNull( v3 ) then
              z := VarToDouble( v3 )
            else
              z := 0 ;
            if ( dim > 3 ) and not VarIsNull( v4 ) then
              m := VarToDouble( v4 )
            else
              m := 0 ;

            if dim = 2 then
              cursorSql[_cursor].currShape.AddPoint( GisPoint( x,y ) )
            else if dim > 2 then
              cursorSql[_cursor].currShape.AddPoint3D( GisPoint3D( x,y,z,m ) )
          end
          else
            break ;

          inc( i, dim ) ;
        end ;

        oGisDb.sqlQueryMoveNext(_cursor) ;

        if not oGisDb.sqlQueryEof(_cursor) then
          next_GID := VarToInt32(oGisDb.sqlQueryGetField(
            sqlQueryNameGEOUID(_cursor),_cursor )
          ) ;
      end ;
      cursorSql[_cursor].currShape.Unlock ;
    end ;

    if not assigned( cursorSql[_cursor].currShape ) then exit ;

    if bunknown then
      cursorSql[_cursor].currShape.Reset ;

    case cursorSql[_cursor].currShape.ShapeType of
      TGIS_ShapeType.Point :
         begin
           cursorSql[_cursor].currPoint.Recreate     (
              cursorSql[_cursor].currShape, nil, False, _uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
         end ;
      TGIS_ShapeType.MultiPoint :
         begin
           cursorSql[_cursor].currMultipoint.Recreate(
              cursorSql[_cursor].currShape, nil, False, _uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currMultipoint ;
         end ;
      TGIS_ShapeType.Arc :
         begin
           cursorSql[_cursor].currArc.Recreate       (
              cursorSql[_cursor].currShape, nil, False, _uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currArc ;
         end ;
      TGIS_ShapeType.Polygon :
         begin
           cursorSql[_cursor].currPolygon.Recreate   (
              cursorSql[_cursor].currShape, nil, False, _uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currPolygon ;
         end ;
      TGIS_ShapeType.Complex :
         begin
           cursorSql[_cursor].currComplex.Recreate   (
              cursorSql[_cursor].currShape, nil, False, _uid, self,
              DefaultDimension
           ) ;
           FreeObject( cursorSql[_cursor].currShape ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currComplex ;
         end ;
      else
         cursorSql[_cursor].currShape := nil ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.sqlQueryOpen(
    const _query   : String ;
    const _cursor  : Integer
   ) ;
  begin
    oGisDb.sqlQueryOpen( _query, _cursor );

    fldX1offset := -1  ;
  end ;

  procedure TGIS_LayerSqlOgisAbstract.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String ;
    const _shp  : TGIS_Shape
  ) ;
  var
    {$IFDEF OXYGENE}
      obj          : TObject ;
      arr          : array of Byte ;
    {$ENDIF}
    store          : OleVariant ;
    seq            : Integer    ;
    gid            : Integer    ;
    e_type         : Integer    ;
    point_no       : Integer    ;
    part_no        : Integer    ;
    ptg            : TGIS_Point ;
    total_point_no : Integer    ;
    valWkt         : String     ;
    scast          : String     ;
    i              : Integer    ;
    b              : String     ;
  begin
    if isBlob then begin

      {$IFDEF OXYGENE}
        _shp.ExportToWKB( obj ) ;
        store := OleVariant( obj ) ;
      {$ELSE}
        _shp.ExportToWKB( store ) ;
      {$ENDIF}

      if oGisDb.UseTextParameters then begin
        if oGisDb.UseTextParameters then
          scast := getCmd( T_SQLOGIS.ID_GEOMETRY_DECODE )
        else
          scast := getCmd( T_SQLOGIS.ID_GEOMETRY_BLOB ) ;

        valWkt := '' ;
        for i := 0 to VarArrayHighBound( store, 1 ) do begin
          {$IFNDEF OXYGENE}
            b := IntToHex( Integer( store[i] ), 2 ) ;
          {$ELSE}
            arr := array of Byte( store ) ;
            b := IntToHex( Integer( arr[i] ), 2 ) ;
          {$ENDIF}
          valWkt := valWkt + b ;
        end ;

        oGisDb.sqlTableSetGeometry( _id, _name, Format( scast, [ valWkt ]), nil ) ;
      end
      else
        oGisDb.sqlTableSetGeometry( _id, _name, store, nil );

      store := Unassigned ;
    end
    else if isWkt then begin

      {$IFDEF OXYGENE}
        valWkt := _shp.ExportToWKT ;
      {$ELSE}
        valWkt := _shp.ExportToWKT ;
      {$ENDIF}

      oGisDb.sqlTableSetField( _id, _name, valWkt, length( valWkt ) );
      store := Unassigned ;

    end
    else begin

      total_point_no := 0;
      gid := VarToInt32( oGisDb.sqlTableGetField( _id, 'GID' ) ) ;

      for part_no := 1 to _shp.GetNumParts do begin
        seq := 1 ;

        for point_no := 1 to _shp.GetPartSize( part_no - 1 ) do begin
          inc( total_point_no ) ;
          ptg := _shp.GetPoint( part_no - 1, point_no - 1 ) ;

          oGisDb.sqlTableSetField( _id, 'GID', gid, 1 );
          oGisDb.sqlTableSetField( _id, 'ESEQ', part_no, 1 );
          e_type := 0 ;

          case _shp.ShapeType of
            TGIS_ShapeType.Point      : e_type := 1 ;
            TGIS_ShapeType.MultiPoint : e_type := 1 ;
            TGIS_ShapeType.Arc        : e_type := 2 ;
            TGIS_ShapeType.Polygon    : e_type := 3 ;
          end ;

          oGisDb.sqlTableSetField( _id, 'ETYPE', e_type, 1 );
          oGisDb.sqlTableSetField( _id, 'SEQ', seq, 1 );

          if( ( point_no - ( ( point_no div 16 ) * 16 ) ) = 0 ) then begin
            oGisDb.sqlTableSetField( _id, 'X16', ptg.X, 1 );
            oGisDb.sqlTableSetField( _id, 'Y16', ptg.Y, 1 );
          end
          else begin
            oGisDb.sqlTableSetField( _id, 'X' +
              IntToStr( point_no - ( ( point_no div 16 ) * 16 ) ), ptg.X, 1 );
            oGisDb.sqlTableSetField( _id, 'Y' +
              IntToStr( point_no - ( ( point_no div 16 ) * 16 ) ), ptg.Y, 1 );
          end ;

          if( ( point_no mod 16 ) = 0 ) then begin
            sqlTablePost( 0 ) ;
            if( total_point_no < _shp.GetNumPoints ) then
              oGisDb.sqlTableAppend( 0, strName ) ;
            inc( seq ) ;
          end ;

        end ;

      end ;

    end ;
  end;

  procedure TGIS_LayerSqlOgisAbstract.sqlTablePost(
    const _id     : Integer
  ) ;
  begin
    oGisDb.sqlTablePost( _id ) ;
  end;

{==================================== END =====================================}
end.

