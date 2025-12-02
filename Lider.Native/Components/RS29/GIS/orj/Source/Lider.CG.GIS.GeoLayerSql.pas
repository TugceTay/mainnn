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
  Encapsulation of a SQL file access.

  Currently support: MS Jet (Access), MS SQL Server, Interbase, Firebird,
  MySQL, DB2, Sybase, PostgreSql, BlackFishSql, Informix, Oracle, Advantage,
  Sqlite and SapDB dialects.
}

{$IFDEF DCC}
  unit GisLayerSql ;
  {$HPPEMIT '#pragma link "GisLayerSql"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
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
    System.SysUtils,
    System.Math,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Variants,

    GisTypes,
    GisLayerVector,
    GisLayerVectorSql ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {$IFDEF OXYGENE}
    T_cursorSql_LayerSql nested in TGIS_LayerSqlAbstract = public {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
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
        currMultiPatch  : TGIS_ShapeMultiPatch ;

        /// <summary>
        ///   If True the ordering by JoinPrimary code should be disabled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;
    end;
  {$ENDIF}

  /// <summary>
  /// Encapsulation of abstract SQL layer.
  /// </summary>
  TGIS_LayerSqlAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                          class( TGIS_LayerVectorSqlAbstract )
    {$IFDEF TATUKGIS_PRIVATE}
      IgnoreViewFeatures : Boolean ;
    {$ENDIF}
    protected // other protected variables
      {$IFDEF OXYGENE}

        /// <summary>
        ///   SQL cursor.
        /// </summary>
        cursorSql : array of T_cursorSql_LayerSql ;

      {$ELSE}

        /// <summary>
        ///   SQL cursor.
        /// </summary>
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
          ///   EOF marker.
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
          currMultiPatch  : TGIS_ShapeMultiPatch ;

          /// <summary>
          ///   If True the ordering by JoinPrimary code should be disabled
          ///     upon MoveFirst.
          /// </summary>
          disableJoinPrimary : Boolean ;
        end ;

      {$ENDIF}

      /// <summary>
      ///   Recent calculated value of last UID.
      /// </summary>
      lastUid         : TGIS_Uid ;

      /// <summary>
      ///   Recent calculated value of last new UID.
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
      ///   Columns names for layer table in comma separated form
      /// </summary>
      columnsNames    : String  ;

      /// <summary>
      ///   UID name fix up (GEO.UID or UID) for various database drivers.
      /// </summary>
      fixGEOUID       : Integer ;

      /// <summary>
      ///   Is RTree enabled.
      /// </summary>
      hasRtree        : Boolean ;

      /// <summary>
      ///   Can unchanged fields be skipped upon saving.
      /// </summary>
      canSkipField    : Boolean ;

      /// <summary>
      ///   Registered layer shape type.
      /// </summary>
      layerstp        : TGIS_ShapeType ;

      /// <summary>
      ///   If True, logging is enabled.
      /// </summary>
      hasLogging  : Boolean ;

      /// <summary>
      ///   If True, style is enabled.
      /// </summary>
      hasStyle  : Boolean ;

      /// <summary>
      ///   if True, mobile version is built without RTree SQLite,
      /// </summary>
      mobileVersion : Boolean ;

      /// <summary>
      ///   Custom column names for select query.
      /// </summary>
      FSelectColumns      : String ;

    protected // properties access routine
      function  fget_TableMaster   : String ;
      function  fget_TableGeometry : String ;
      function  fget_TableFeatures : String ;
      function  fget_TableIndex    : String ;
      function  fget_TableLog      : String ;
      function  fget_ViewFeatures  : String ; override;
   private
      function  getSelectColumns   : String ;
    protected

      /// <summary>
      ///   Read a shape from a SQL query and recreate it.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure readShape             ( const _cursor : Integer
                                      ) ;

      /// <inheritdoc/>
      procedure prepareCommandList    ; override;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <param name="_id">
      ///   if 0, then returned 'UID'; if 1, then returned GEO.UID
      /// </param>
      /// <returns>
      ///   UID or GEO.UID command
      /// </returns>
      function  getCmdGEOUID          ( const _id : Integer
                                      ) : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///   SHAPETYPE command
      /// </returns>
      function  getCmdSHAPETYPE       : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///   XMIN command
      /// </returns>
      function  getCmdXMIN            : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///   YMIN command
      /// </returns>
      function  getCmdYMIN            : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///   GEOMETRY command
      /// </returns>
      function  getCmdGEOMETRY        : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///   GEOMETRY_CAST command
      /// </returns>
      function  getCmdGEOMETRY_CAST   : String ;

      /// <summary>
      ///   Return shape type ID.
      /// </summary>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_dim">
      ///   dimension type
      /// </param>
      /// <returns>
      ///   shape type ID
      /// </returns>
      function  setShapeType          ( const _type   : TGIS_ShapeType ;
                                        const _dim    : TGIS_DimensionType
                                      ) : Integer ;

      /// <summary>
      ///   Decode shape type and dimension from ID.
      /// </summary>
      /// <param name="_val">
      ///   shape ID
      /// </param>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_dim">
      ///   dimension type
      /// </param>
      procedure checkShapeType        ( const _val    : Integer ;
                                          var _type   : TGIS_ShapeType ;
                                          var _dim    : TGIS_DimensionType
                                      ) ;

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
      /// <param name="_filter">
      ///   filter expression
      /// </param>
      /// <returns>
      ///   parametrized append command
      /// </returns>
      function  prepareAppendCmd      ( const _table  : String ;
                                        const _filter : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized update command.
      /// </summary>
      /// <param name="_table">
      ///   table to be updated
      /// </param>
      /// <param name="_filter">
      ///   filter expression
      /// </param>
      /// <returns>
      ///   parametrized update command
      /// </returns>
      function  prepareUpdateCmd      ( const _table  : String ;
                                        const _filter : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///   parametrized append command
      /// </returns>
      function  prepareAppendCommand  ( const _table : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///   parametrized append command
      /// </returns>
      function  prepareAppendParams   ( const _table : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      procedure macroAppendParams     ( const _table : String
                                      )  ;

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
      ///   table to be inserted
      /// </param>
      /// <param name="_column">
      ///   column name used as a filter
      /// </param>
      /// <returns>
      ///   parametrized update command
      /// </returns>
      function  prepareUpdateCommand  ( const _table  : String ;
                                        const _column : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized update command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <param name="_column">
      ///   column name used as a filter
      /// </param>
      /// <returns>
      ///   parametrized update command
      /// </returns>
      function  prepareUpdateParams   ( const _table  : String ;
                                        const _column : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      procedure macroUpdateParams     ( const _table : String
                                      )  ;

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
      ///   Return a SQL select command.
      /// </summary>
      /// <param name="_table">
      ///   table to be selected
      /// </param>
      /// <param name="_filter">
      ///   filter expression
      /// </param>
      /// <returns>
      ///   select command
      /// </returns>
      function  prepareSelectCommand  ( const _table  : String ;
                                        const _filter : String
                                      ) : String ;

      /// <summary>
      ///   Return an UID=_uid filter.
      /// </summary>
      /// <param name="_uid">
      ///   UID value used to build filter
      /// </param>
      /// <returns>
      ///   UID filter
      /// </returns>
      function  prepareFilterUid      ( const _uid    : TGIS_Uid
                                      ) : String ;

      /// <summary>
      ///   Return columns names for layer table in comma separated form.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///   comma separated column names
      /// </returns>
      function  prepareColumnsNames   ( const _table : String
                                      ) : String ;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_uidname">
      ///   unique name of the field to be added
      /// </param>
      /// <param name="_name">
      ///   name of the field to be added
      /// </param>
      /// <param name="_type">
      ///   type of the field
      /// </param>
      /// <param name="_width">
      ///   width of the field
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
      ///   unique name of the field to be added
      /// </param>
      /// <param name="_name">
      ///   name of the field to be add
      /// </param>
      /// <param name="_type">
      ///   type of the field
      /// </param>
      /// <param name="_width">
      ///   width of the field
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
      ///   unique name of the field to be added
      /// </param>
      /// <param name="_name">
      ///   name of the field to be add
      /// </param>
      /// <param name="_type">
      ///   type of the field
      /// </param>
      /// <param name="_width">
      ///   width of the field
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
      ///   Append a new record into the table.
      /// </summary>
      /// <param name="_id">
      ///   ID of the table
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
      procedure sqlTableAppend        ( const _id     : Integer ;
                                        const _table  : String
                                      ) ; virtual; abstract;

      /// <summary>
      ///   Open the table based on provided filter.
      /// </summary>
      /// <param name="_id">
      ///   ID of the table
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_uidCol">
      ///   name of the UID column
      /// </param>
      /// <param name="_uidVal">
      ///   name of the UID value
      /// </param>
      procedure sqlTableOpenWrite     ( const _id     : Integer ;
                                        const _table  : String ;
                                        const _uidCol : String  ;
                                        const _uidVal : TGIS_Uid
                                      ) ; virtual; abstract;

      /// <summary>
      ///   Set a geometry to the table.
      /// </summary>
      /// <param name="_id">
      ///   ID of the table
      /// </param>
      /// <param name="_name">
      ///   name of the field
      /// </param>
      /// <param name="_shp">
      ///   shape geometry
      /// </param>
      procedure sqlTableSetGeometry   ( const _id     : Integer ;
                                        const _name   : String ;
                                        const _shp    : TGIS_Shape
                                      ) ; virtual; abstract;

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
      ///   cursor ID
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      procedure sqlQueryGetGeometry   ( const _uid       : TGIS_Uid ;
                                        const _shapetype : TGIS_ShapeType ;
                                        const _cursor    : Integer ;
                                        const _dim       : TGIS_DimensionType
                                      ) ; virtual; abstract;

      /// <summary>
      ///   Get a GEO.UID from the query.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor ID
      /// </param>
      /// <returns>
      ///   GEO.UID
      /// </returns>
      function  sqlQueryGetGEOUID     ( const _cursor        : Integer
                                      ) : Variant ; virtual; abstract;

      /// <summary>
      ///   Update the table (after any change).
      /// </summary>
      /// <param name="_id">
      ///   ID of the table
      /// </param>
      procedure sqlTablePost          ( const _id     : Integer
                                      ) ; virtual; abstract;

      /// <summary>
      ///   Get a GEO.UID field from the query.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor ID
      /// </param>
      /// <returns>
      ///   GEO.UID
      /// </returns>
      function  sqlQueryNameGEOUID    ( const _cursor     : Integer
                                      ) : String ; virtual; abstract;

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
      ///   Macro for creating the master table.
      /// </summary>
      procedure macroMasterCreate     ; virtual;

      /// <summary>
      ///   Macro for creating the master log table.
      /// </summary>
      procedure macroMasterLogCreate  ; virtual;

      /// <summary>
      ///   Macro for creating the master style table.
      /// </summary>
      procedure macroMasterStyleCreate  ; virtual;

      /// <summary>
      ///   Macro for enabling logging.
      /// </summary>
      procedure macroEnableLogging    ; virtual;

      /// <summary>
      ///   Macro for disabling logging.
      /// </summary>
      procedure macroDisableLogging   ; virtual;

      /// <inheritdoc/>
      procedure macroMasterUpdate     ( const _extent : TGIS_Extent    ;
                                        const _type   : TGIS_ShapeType ;
                                        const _name   : String         ;
                                        const _dim    : TGIS_DimensionType
                                      ) ; override;

      /// <summary>
      ///   Macro for creating master style table.
      /// </summary>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      procedure macroMasterStyleUpdate( const _layer  : TGIS_LayerVector
                                      ) ;

      /// <summary>
      ///   Macro for creating table.
      /// </summary>
      /// <param name="_extent">
      ///   starting extent of the layer
      /// </param>
      /// <param name="_type">
      ///   supported type of the shape
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
      ///   shape UID
      /// </param>
      procedure macroShapeDelete      ( const _uid     : TGIS_Uid
                                      ) ; virtual;

      /// <inheritdoc/>
      procedure macroShapeUpdate      ( const _shp     : TGIS_Shape ;
                                        const _import  : Boolean
                                      ) ; overload; override;

      /// <inheritdoc/>
      procedure macroShapeUpdate      ( const _shp     : TGIS_Shape ;
                                        const _import  : Boolean ;
                                        const _dim     : TGIS_DimensionType
                                      ) ; overload;

      /// <summary>
      ///   Macro for obtaining last assigned UID.
      /// </summary>
      /// <returns>
      ///   last assigned UID
      /// </returns>
      function  macroUidLast          : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for reserving new UID.
      /// </summary>
      /// <returns>
      ///   new UID
      /// </returns>
      function  macroUidReserve       : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for generating new UID.
      /// </summary>
      /// <param name="_guaranted">
      ///   if True, the provided UID is unique (for example generated by
      ///   stored procedure); otherwise it is treated only as a starting point
      ///   for further generation
      /// </param>
      /// <returns>
      ///   new UID
      /// </returns>
      function  macroUidNew           ( var   _guaranted : Boolean
                                      ) : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for fetching proper record form the database.
      /// </summary>
      /// <param name="_uid">
      ///   UID of the shape for which corresponding record should be fetched
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   True if successful
      /// </returns>
      function  macroFetchRecord      ( const _uid     : TGIS_Uid ;
                                        const _cursor  : Integer
                                      ) : Boolean ; virtual;

      /// <inheritdoc/>
      function  macroMaxNameLength    : Integer ; override;

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

      /// <summary>
      ///   Parse configuration layer name.
      /// </summary>
      procedure parseConfigLayerName  ;

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
      function  getFieldInternal      ( const _uid      : TGIS_Uid  ;
                                        const _name     : String ;
                                        const _cursor   : Integer
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
      procedure doDestroy  ; override;

    public

      /// <inheritdoc/>
      constructor Create   ; override;

      /// <inheritdoc/>
      /// <param name="_path">
      ///   path to .ttkls file; if empty the build base on existing
      ///   parameters SQLParameter; if points to non-existent file then
      ///   will be treated as a list of CRLF or '\n' delimited parameter
      /// </param>
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
      procedure RevertShapes ; override;

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

      /// <inheritdoc/>
      function  GetShape       ( const _uid     : TGIS_Uid  ;
                                 const _cursor  : Integer
                               ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid     : TGIS_Uid ; override;

      /// <inheritdoc/>
      function  GetNewUid      : TGIS_Uid ; override;

      /// <inheritdoc/>
      procedure SaveData       ; override;

      /// <inheritdoc/>
      function  DormantGain    : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant        ; override;

      /// <summary>
      ///   Enable tracing in existing layer logging mode.
      /// </summary>
      procedure EnableLogging  ; virtual;

      /// <summary>
      ///   Disable tracing in existing layer logging mode.
      /// </summary>
      procedure DisableLogging ; virtual;

      /// <summary>
      ///   Get trace logs of a layer if available.
      /// </summary>
      /// <returns>
      ///   trace logs
      /// </returns>
      function GetLogs : {$IFDEF OXYGENE}
                           TGIS_Strings ; virtual;
                         {$ELSE}
                           TStrings     ; virtual;
                         {$ENDIF}
    public // properties

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

      /// <summary>
      ///   Name of the RTree index table.
      /// </summary>
      property TableIndex    : String read fget_TableIndex ;

      /// <summary>
      ///   Name of the log table.
      /// </summary>
      property TableLog      : String read fget_TableLog ;
  end ;
  TGIS_LayerSqlAbstractClass = class of TGIS_LayerSqlAbstract ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SyncObjs,
    GisRtl,
    GisLogger,
    GisFunctions,
    GisLayer,
    GisInternals,
    GisClasses,
    GisParams,
    GisResource,
    GisDb ;
{$ENDIF}

type
  T_SQLN = class
  public
  const
    // Number of retries for concurrent issues.
    RETRY_COUNT             = 99 ;

    // Maximum time of interval between retries in concurrent mode.
    RETRY_INTERVAL          = 100  ;

    // Maximum number of items fetched in GetShape.
    GETSHAPE_FETCHED_LIMIT  = 500  ;

    ID_BEGIN = 0                                                                ;

    CREATE_MASTER =
      'CREATE TABLE <#ttkGISLayerSQL#> '                                        +
      '( <#NAME#> <#VARCHAR#>(64),'                                             +
      '<#XMIN#> <#DOUBLE#>, <#XMAX#> <#DOUBLE#>,'                               +
      '<#YMIN#> <#DOUBLE#>, <#YMAX#> <#DOUBLE#>,'                               +
      '<#SHAPETYPE#> <#SMALLINT#>, '                                            +
      '<#SRTEXT#> <#VARCHAR(2048)#>'                                            +
      ')'                                                                       ;
    ID_CREATE_MASTER = ID_BEGIN                                                 ;

    CREATE_TABLE_GEO =
      'CREATE TABLE %s ('                                                       +
      '<#UID#> <#INTEGER#> <#NOT NULL PRIMARY KEY#>,'                           +
      '<#XMIN#> <#DOUBLE#> NOT NULL, <#XMAX#> <#DOUBLE#> NOT NULL,'             +
      '<#YMIN#> <#DOUBLE#> NOT NULL, <#YMAX#> <#DOUBLE#> NOT NULL,'             +
      '<#SHAPETYPE#> <#SMALLINT#>,'                                             +
      '<#GEOMETRY#> <#BLOB#>'                                                   +
      ')'                                                                       ;
    ID_CREATE_TABLE_GEO = ID_CREATE_MASTER + 1                                  ;

    CREATE_TABLE_FEA =
      'CREATE TABLE %s ('                                                       +
      '<#UID#> <#INTEGER#> <#NOT NULL PRIMARY KEY#>'                            +
      ')'                                                                       ;
    ID_CREATE_TABLE_FEA = ID_CREATE_TABLE_GEO + 1                               ;

    DROP_TABLE =
      'DROP TABLE <#IF_EXISTS#> %s'                                             ;
    ID_DROP_TABLE = ID_CREATE_TABLE_FEA + 1                                     ;

    CREATE_INDEX_NAME =
      'CREATE UNIQUE INDEX <#ttkGISLayerSQL#><#_IDX_#><#NAME#>'                 +
      ' ON <#ttkGISLayerSQL#> (<#NAME#>)'                                       ;
    ID_CREATE_INDEX_NAME = ID_DROP_TABLE + 1                                    ;

    CREATE_INDEX_UID =
      'CREATE UNIQUE <#DESC_IB#> INDEX %s<#_IDX_#><#UID#>'                      +
      ' ON %s (<#UID#> <#DESC#>)'                                               ;
    ID_CREATE_INDEX_UID = ID_CREATE_INDEX_NAME + 1                              ;

    CREATE_INDEX_XMIN =
      'CREATE INDEX %s<#_IDX_#><#XMIN#> ON %s (<#XMIN#>)'                       ;
    ID_CREATE_INDEX_XMIN = ID_CREATE_INDEX_UID + 1                              ;

    CREATE_INDEX_XMAX =
      'CREATE INDEX %s<#_IDX_#><#XMAX#> ON %s (<#XMAX#>)'                       ;
    ID_CREATE_INDEX_XMAX = ID_CREATE_INDEX_XMIN + 1                             ;

    CREATE_INDEX_YMIN =
      'CREATE INDEX %s<#_IDX_#><#YMIN#> ON %s (<#YMIN#>)'                       ;
    ID_CREATE_INDEX_YMIN = ID_CREATE_INDEX_XMAX + 1                             ;

    CREATE_INDEX_YMAX =
      'CREATE INDEX %s<#_IDX_#><#YMAX#> ON %s (<#YMAX#>)'                       ;
    ID_CREATE_INDEX_YMAX = ID_CREATE_INDEX_YMIN + 1                             ;

    INSERT_MASTER =
      'INSERT INTO <#ttkGISLayerSQL#>'                                          +
      ' VALUES(''%s'',%s,%s,%s,%s,%d,''%s'')'                                   ;
    ID_INSERT_MASTER = ID_CREATE_INDEX_YMAX + 1                                 ;

    INSERT_MASTER_OLD =
      'INSERT INTO <#ttkGISLayerSQL#>'                                          +
      ' VALUES(''%s'',%s,%s,%s,%s,%d)'                                          ;
    ID_INSERT_MASTER_OLD = ID_INSERT_MASTER + 1                                 ;

    UPDATE_MASTER =
      'UPDATE <#ttkGISLayerSQL#>'                                               +
      ' SET <#XMIN#>=%s,<#XMAX#>=%s,<#YMIN#>=%s,'                               +
      '      <#YMAX#>=%s,<#SHAPETYPE#>=%d, <#SRTEXT#>=''%s'''                   +
      ' WHERE <#NAME#>=''%s'''                                                  ;
    ID_UPDATE_MASTER = ID_INSERT_MASTER_OLD + 1                                 ;

    UPDATE_MASTER_OLD =
      'UPDATE <#ttkGISLayerSQL#>'                                               +
      ' SET <#XMIN#>=%s,<#XMAX#>=%s,<#YMIN#>=%s,'                               +
      '      <#YMAX#>=%s,<#SHAPETYPE#>=%d'                                      +
      ' WHERE <#NAME#>=''%s'''                                                  ;
    ID_UPDATE_MASTER_OLD = ID_UPDATE_MASTER + 1                                 ;

    SELECT_MASTER =
      'SELECT * FROM <#ttkGISLayerSQL#> WHERE <#NAME#>=''%s'''                  ;
    ID_SELECT_MASTER = ID_UPDATE_MASTER_OLD + 1                                 ;

    SELECT_MASTER_ALL =
      'SELECT <#NAME#>, <#SHAPETYPE#> FROM <#ttkGISLayerSQL#>'                  ;
    ID_SELECT_MASTER_ALL = ID_SELECT_MASTER + 1                                 ;

    SELECT_TABLE_ALL =
      'SELECT * FROM %s'                                                        ;
    ID_SELECT_TABLE_ALL = ID_SELECT_MASTER_ALL + 1                              ;

    SELECT_TABLE_ALL_EX =
      'SELECT %s FROM %s'                                                       ;
    ID_SELECT_TABLE_ALL_EX = ID_SELECT_TABLE_ALL + 1                            ;

    SELECT_TABLE_WHERE =
      'SELECT %s FROM %s WHERE %s'                                               ;
    ID_SELECT_TABLE_WHERE = ID_SELECT_TABLE_ALL_EX + 1                          ;

    SELECT_TABLE_WHERE_EX =
      'SELECT %s FROM %s WHERE %s'                                              ;
    ID_SELECT_TABLE_WHERE_EX = ID_SELECT_TABLE_WHERE + 1                        ;

    SELECT_JOIN_UID =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE'                                                                  +
      ' (<#GEO#>.<#UID#>=%d)'                                                   +
      ' AND (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                  ;
    ID_SELECT_JOIN_UID = ID_SELECT_TABLE_WHERE_EX + 1                           ;

    SELECT_JOIN =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE '                                                                 +
      '  <#XMAX#>>%s AND <#XMIN#><%s AND'                                       +
      '  <#YMAX#>>%s AND <#YMIN#><%s %s'                                        +
      ' AND (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                  ;
    ID_SELECT_JOIN = ID_SELECT_JOIN_UID + 1                                     ;

    SELECT_JOIN_EX =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE '                                                                 +
      ' (%s)'                                                                   +
      ' AND <#XMAX#>>%s AND <#XMIN#><%s AND'                                    +
      '     <#YMAX#>>%s AND <#YMIN#><%s %s'                                     +
      ' AND (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                  ;
    ID_SELECT_JOIN_EX = ID_SELECT_JOIN + 1                                      ;

    SELECT_JOIN_NOEXT =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE %s '                                                              +
      '   (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                    ;
    ID_SELECT_JOIN_NOEXT = ID_SELECT_JOIN_EX + 1                                ;

    SELECT_JOIN_NOEXT_EX =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE (%s) %s AND'                                                      +
      '  (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                     ;
    ID_SELECT_JOIN_NOEXT_EX = ID_SELECT_JOIN_NOEXT + 1                          ;

    SELECT_MAX_UID =
      'SELECT <#MAX#>(<#UID#>) <#AS#> <#MAX_UID#> FROM %s'                      ;
    ID_SELECT_MAX_UID = ID_SELECT_JOIN_NOEXT_EX + 1                             ;

    MAX_UID =
      '<#MAX_UID#>'                                                             ;
    ID_MAX_UID = ID_SELECT_MAX_UID + 1                                          ;

    DELETE_SHAPE =
      'DELETE FROM %S WHERE <#UID#>=%d'                                         ;
    ID_DELETE_SHAPE = ID_MAX_UID + 1                                            ;

    DELETE_TABLE =
      'DELETE FROM <#ttkGISLayerSQL#> WHERE <#NAME#>=''%s'''                    ;
    ID_DELETE_TABLE = ID_DELETE_SHAPE + 1                                       ;

    UPDATE_DBX_GEO =
      '<#XMIN#>=:<#XMIN#>,<#XMAX#>=:<#XMAX#>,'                                  +
      '<#YMIN#>=:<#YMIN#>,<#YMAX#>=:<#YMAX#>,'                                  +
      '<#SHAPETYPE#>=:<#SHAPETYPE#>,'                                           +
      '<#GEOMETRY#>=<#:GEOMETRY#> '                                             ;
    ID_UPDATE_DBX_GEO = ID_DELETE_TABLE + 1                                     ;

    UPDATE =
      'UPDATE %s SET %s WHERE %s=%s'                                            ;
    ID_UPDATE = ID_UPDATE_DBX_GEO + 1                                           ;

    UPDATE_DBX =
      'UPDATE %s SET %s WHERE %s=:%s %s'                                        ;
    ID_UPDATE_DBX = ID_UPDATE + 1                                               ;

    UPDATE_EX =
      'UPDATE %s SET %s WHERE %s'                                               ;
    ID_UPDATE_EX = ID_UPDATE_DBX + 1                                            ;

    INSERT_DBX_UID_VALUE =
      '<#UID#>'                                                                 ;
    ID_INSERT_DBX_UID_VALUE = ID_UPDATE_EX + 1                                  ;

    INSERT_DBX_UID_PARAM =
      ':<#UID#>'                                                                ;
    ID_INSERT_DBX_UID_PARAM = ID_INSERT_DBX_UID_VALUE + 1                       ;

    INSERT_DBX_GEO_VALUE =
      '<#UID#>,<#XMIN#>,<#XMAX#>,<#YMIN#>,<#YMAX#>,'                            +
      '<#SHAPETYPE#>,<#GEOMETRY#>'                                              ;
    ID_INSERT_DBX_GEO_VALUE = ID_INSERT_DBX_UID_PARAM + 1                       ;

    INSERT_DBX_GEO_PARAM =
      ':<#UID#>,:<#XMIN#>,:<#XMAX#>,:<#YMIN#>,:<#YMAX#>,'                       +
      ':<#SHAPETYPE#>,<#:GEOMETRY#>'                                            ;
    ID_INSERT_DBX_GEO_PARAM = ID_INSERT_DBX_GEO_VALUE + 1                       ;

    INSERT =
      'INSERT INTO %s ( %s ) VALUES ( %s )'                                     ;
    ID_INSERT = ID_INSERT_DBX_GEO_PARAM + 1                                     ;

    INSERT_DBX =
      'INSERT INTO %s ( %s ) VALUES ( %s ) %s'                                  ;
    ID_INSERT_DBX = ID_INSERT + 1                                               ;

    UPDATE_DBX_ORA =
      'returning <#GEOMETRY#> into :<#GEOMETRY#>'                               ;
    ID_UPDATE_DBX_ORA = ID_INSERT_DBX + 1                                       ;

    ALTER_DROP_COLUMN =
      'ALTER TABLE %s <#DROP COLUMN#> <#QUOTE LEFT#>%s<#QUOTE RIGHT#>'          ;
    ID_ALTER_DROP_COLUMN = ID_UPDATE_DBX_ORA + 1                                ;

    ALTER_ADD_STRING =
      'ALTER TABLE %s ADD %s <#VARCHAR#>(%d<#VARCHAR_TYPE#>)'                   ;
    ID_ALTER_ADD_STRING = ID_ALTER_DROP_COLUMN + 1                              ;

    ALTER_ADD_MEMO =
      'ALTER TABLE %s ADD %s <#CLOB#>'                                          ;
    ID_ALTER_ADD_MEMO = ID_ALTER_ADD_STRING + 1                                 ;

    ALTER_ALTER_STRING =
      'ALTER TABLE %s <#ALTER#> %s <#VARCHAR_LEN#> <#VARCHAR#>(%d)'             ;
    ID_ALTER_ALTER_STRING = ID_ALTER_ADD_MEMO + 1                               ;

    ALTER_ADD_DATE =
      'ALTER TABLE %s ADD %s <#DATE#>'                                          ;
    ID_ALTER_ADD_DATE = ID_ALTER_ALTER_STRING + 1                               ;

    ALTER_ADD_BOOLEAN =
      'ALTER TABLE %s ADD %s <#LOGICAL#>'                                       ;
    ID_ALTER_ADD_BOOLEAN = ID_ALTER_ADD_DATE + 1                                ;

    ALTER_ADD_INTEGER =
      'ALTER TABLE %s ADD %s <#INTEGER#>'                                       ;
    ID_ALTER_ADD_INTEGER = ID_ALTER_ADD_BOOLEAN + 1                             ;

    ALTER_ADD_BIGINT =
      'ALTER TABLE %s ADD %s <#BIGINT#>'                                        ;
    ID_ALTER_ADD_BIGINT = ID_ALTER_ADD_INTEGER + 1                              ;

    ALTER_ADD_FLOAT =
      'ALTER TABLE %s ADD %s <#DOUBLE#>'                                        ;
    ID_ALTER_ADD_FLOAT = ID_ALTER_ADD_BIGINT + 1                                ;

    FILTER_UID =
      '%s=%d'                                                                   ;
    ID_FILTER_UID = ID_ALTER_ADD_FLOAT + 1                                      ;

    FILTER_UID_WEAK =
      '(%s>=%d) and (%s<=%d)'                                                   ;
    ID_FILTER_UID_WEAK = ID_FILTER_UID + 1                                      ;

    FILTER_TABLE =
      '<#NAME#>=''%s'''                                                         ;
    ID_FILTER_TABLE = ID_FILTER_UID_WEAK + 1                                    ;

    MAX_NAME_LENGTH =
      '<#MAX_NAMELENGTH#>'                                                      ;
    ID_MAX_NAME_LENGTH = ID_FILTER_TABLE + 1                                    ;

    MAX_TEXT_LENGTH =
      '<#MAX_TEXTLENGTH#>'                                                      ;
    ID_MAX_TEXT_LENGTH = ID_MAX_NAME_LENGTH + 1                                 ;

    MASTERLAYER =
      '<#ttkGISLayerSQL#>'                                                      ;
    ID_MASTERLAYER = ID_MAX_TEXT_LENGTH + 1                                     ;

    NAME =
      '<#NAME#>'                                                                ;
    ID_NAME = ID_MASTERLAYER + 1                                                ;

    XMIN =
      '<#XMIN#>'                                                                ;
    ID_XMIN = ID_NAME + 1                                                       ;

    XMAX =
      '<#XMAX#>'                                                                ;
    ID_XMAX = ID_XMIN + 1                                                       ;

    YMIN =
      '<#YMIN#>'                                                                ;
    ID_YMIN = ID_XMAX + 1                                                       ;

    YMAX =
      '<#YMAX#>'                                                                ;
    ID_YMAX = ID_YMIN + 1                                                       ;

    SHAPETYPE =
      '<#SHAPETYPE#>'                                                           ;
    ID_SHAPETYPE = ID_YMAX + 1                                                  ;

    SRTEXT =
      '<#SRTEXT#>'                                                              ;
    ID_SRTEXT = ID_SHAPETYPE + 1                                                ;

    UID =
      '<#UID#>'                                                                 ;
    ID_UID = ID_SRTEXT + 1                                                      ;

    GEOMETRY =
      '<#GEOMETRY#>'                                                            ;
    ID_GEOMETRY = ID_UID + 1                                                    ;

    GEOMETRY_EX =
      '<#:GEOMETRY#>'                                                           ;
    ID_GEOMETRY_EX = ID_GEOMETRY + 1                                            ;

    GEOMETRY_DECODE =
      'DECODE(''%s'', ''hex'')'                                                 ;
    ID_GEOMETRY_DECODE = ID_GEOMETRY_EX + 1                                     ;

    GEOMETRY_BLOB =
      'BLOB(x''%s'')'                                                           ;
    ID_GEOMETRY_BLOB = ID_GEOMETRY_DECODE + 1                                   ;

    GEO_TABLE_PARAMS =
      '<#UID#>=#13#10'                                                          +
      '<#XMIN#>=#13#10<#XMAX#>=#13#10<#YMIN#>=#13#10<#YMAX#>=#13#10'            +
      '<#SHAPETYPE#>=#13#10'                                                    +
      '<#GEOMETRY#>=#13#10'                                                     ;
    ID_GEO_TABLE_PARAMS = ID_GEOMETRY_BLOB + 1                                  ;

    GEO =
      '<#GEO#>'                                                                 ;
    ID_GEO = ID_GEO_TABLE_PARAMS + 1                                            ;

    FEA =
      '<#FEA#>'                                                                 ;
    ID_FEA = ID_GEO + 1                                                         ;

    IDX=
      '<#IDX#>'                                                                 ;
    ID_IDX = ID_FEA + 1                                                         ;

    LOG=
      '<#LOG#>'                                                                 ;
    ID_LOG = ID_IDX + 1                                                         ;

    SELECT_COLUMNS_NAME_IN_TABLE =
      'SELECT column_name FROM information_schema.columns'                      +
      ' WHERE table_schema=''%s'' and table_name=''%s'''                        +
      ' AND column_name NOT LIKE ''<#WKB#>'''                                   +
      ' ORDER BY ordinal_position'                                              ;
    ID_SELECT_COLUMNS_NAME_IN_TABLE =  ID_LOG + 1                               ;

    COLUMN_NAME =
      '<#COLUMN_NAME#>'                                                         ;
    ID_COLUMN_NAME = ID_SELECT_COLUMNS_NAME_IN_TABLE + 1                        ;

    SELECT_DB_AND_CURRENT_USER_NAME =
      'SELECT CURRENT_DATABASE() <#F_TABLE_CATALOG#>,'                          +
      'CURRENT_SCHEMA <#F_TABLE_SCHEMA#>'                                       ;
    ID_SELECT_DB_AND_CURRENT_USER_NAME = ID_COLUMN_NAME + 1                     ;

    CHECK_RTREE =
      'select * from sqlite_master where type=''table'' and name=''%s'''        ;
    ID_CHECK_RTREE = ID_SELECT_DB_AND_CURRENT_USER_NAME + 1                     ;

    SELECT_JOIN_RTREE =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE <#GEO#>.<#UID#> IN ( SELECT <#UID#> FROM %s WHERE'                +
      '  <#XMAX#>>%s AND <#XMIN#><%s AND'                                       +
      '  <#YMAX#>>%s AND <#YMIN#><%s %s ORDER BY UID)'                          +
      ' AND (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                  ;
    ID_SELECT_JOIN_RTREE = ID_CHECK_RTREE + 1                                   ;

    SELECT_JOIN_RTREE_EX =
      'SELECT <#FEA#>.%s, <#GEO#>.* FROM %s <#FEA#>, %s <#GEO#>'                +
      ' WHERE (%s) AND <#GEO#>.<#UID#> IN ( SELECT <#UID#> FROM %s WHERE'       +
      '  <#XMAX#>>%s AND <#XMIN#><%s AND'                                       +
      '  <#YMAX#>>%s AND <#YMIN#><%s %s ORDER BY UID)'                          +
      ' AND (<#GEO#>.<#UID#>=<#FEA#>.<#UID#>)'                                  ;
    ID_SELECT_JOIN_RTREE_EX = ID_SELECT_JOIN_RTREE + 1                          ;

    CREATE_RTREE_INDEX =
      'CREATE VIRTUAL TABLE %s USING rtree( '                                   +
      '<#UID#>, <#XMIN#>, <#XMAX#>, <#YMIN#>, <#YMAX#> )'                       ;
    ID_CREATE_RTREE_INDEX = ID_SELECT_JOIN_RTREE_EX + 1                         ;

    BUILD_RTREE_INDEX =
      'INSERT OR IGNORE INTO %s '                                               +
      '(<#UID#>, <#XMIN#>, <#XMAX#>, <#YMIN#>, <#YMAX#>) '                      +
      'SELECT <#UID#>, <#XMIN#>, <#XMAX#>, <#YMIN#>, <#YMAX#> FROM %s'          ;
    ID_BUILD_RTREE_INDEX = ID_CREATE_RTREE_INDEX + 1                            ;

    CREATE_TRIGGER_GEO_AFTER_DELETE =
      'CREATE TRIGGER IF NOT EXISTS "tad_%s" AFTER DELETE ON "%s" '             +
      'FOR EACH ROW BEGIN '                                                     +
      ' DELETE FROM "%s" WHERE <#UID#> = OLD."<#UID#>"; '                       +
      'END '                                                                    ;
    ID_CREATE_TRIGGER_GEO_AFTER_DELETE = ID_BUILD_RTREE_INDEX + 1               ;

    CREATE_TRIGGER_GEO_AFTER_INSERT1 =
      'CREATE TRIGGER IF NOT EXISTS "tai1_%s" AFTER INSERT ON "%s" '            +
      'FOR EACH ROW WHEN '                                                      +
      ' (NEW."<#XMIN#>" > NEW."<#XMAX#>") AND (NEW."<#YMIN#>" > NEW."<#YMAX#>")'+
      ' BEGIN '                                                                 +
      '  INSERT INTO "%s" (<#UID#>, <#XMIN#>, <#XMAX#>, <#YMIN#>, <#YMAX#>) '   +
      '  VALUES (NEW."<#UID#>", NEW."<#XMIN#>", NEW."<#XMIN#>", NEW."<#YMIN#>",'+
      ' NEW."<#YMIN#>");'                                                       +
      'END '                                                                    ;
    ID_CREATE_TRIGGER_GEO_AFTER_INSERT1 = ID_CREATE_TRIGGER_GEO_AFTER_DELETE+1  ;

    CREATE_TRIGGER_GEO_AFTER_INSERT2 =
      'CREATE TRIGGER IF NOT EXISTS "tai2_%s" AFTER INSERT ON "%s" '               +
      'FOR EACH ROW WHEN '                                                         +
      ' (NEW."<#XMIN#>" <= NEW."<#XMAX#>") AND (NEW."<#YMIN#>" <= NEW."<#YMAX#>")' +
      ' BEGIN '                                                                    +
      '  INSERT INTO "%s" (<#UID#>, <#XMIN#>, <#XMAX#>, <#YMIN#>, <#YMAX#>) '      +
      '  VALUES (NEW."<#UID#>", NEW."<#XMIN#>", NEW."<#XMAX#>", NEW."<#YMIN#>",'   +
      ' NEW."<#YMAX#>");'                                                          +
      'END '                                                                       ;
    ID_CREATE_TRIGGER_GEO_AFTER_INSERT2 = ID_CREATE_TRIGGER_GEO_AFTER_INSERT1+1    ;

    CREATE_TRIGGER_GEO_AFTER_UPDATE =
      'CREATE TRIGGER IF NOT EXISTS "tau_%s" AFTER UPDATE ON "%s" '                +
      'FOR EACH ROW BEGIN '                                                        +
      '  UPDATE "%s" SET "<#XMIN#>" = NEW."<#XMIN#>", "<#XMAX#>" = NEW."<#XMAX#>",'+
      ' "<#YMIN#>" = NEW."<#YMIN#>", "<#YMAX#>" = NEW."<#YMAX#>" '                 +
      'WHERE "<#UID#>" = NEW."<#UID#>"; '                                          +
      'END '                                                                       ;
    ID_CREATE_TRIGGER_GEO_AFTER_UPDATE =
      ID_CREATE_TRIGGER_GEO_AFTER_INSERT2 + 1                                   ;

    CREATE_TABLE_LOG =
      'CREATE TABLE %s ('                                                       +
      ' ID INTEGER PRIMARY KEY ASC AUTOINCREMENT NOT NULL, '                    +
      ' ACTION_TYPE CHAR(1), '                                                  +
      ' ACTION_DATE DATETIME, '                                                 +
      ' SHAPE_UID <#INTEGER#>, '                                                +
      ' STATUS <#INTEGER#> DEFAULT (0) '                                        +
      ')'                                                                       ;
    ID_CREATE_TABLE_LOG = ID_CREATE_TRIGGER_GEO_AFTER_UPDATE + 1                ;

    CREATE_TABLE_LOG_INDEX =
      'CREATE UNIQUE INDEX %s<#_IDX_#> ON %s (ACTION_TYPE,SHAPE_UID)'           ;
    ID_CREATE_TABLE_LOG_INDEX = ID_CREATE_TABLE_LOG + 1                         ;

    CREATE_TABLE_LOG_MANAGER =
      'CREATE TABLE <#ttkGISLayerLOG#> ( '                                      +
      ' <#NAME#> <#VARCHAR#>(64) PRIMARY KEY NOT NULL UNIQUE, '                 +
      ' ENABLED BOOLEAN DEFAULT (1) '                                           +
      ')'                                                                       ;
    ID_CREATE_TABLE_LOG_MANAGER = ID_CREATE_TABLE_LOG_INDEX + 1                 ;

    CREATE_TABLE_LOG_MANAGER_INDEX =
      'CREATE UNIQUE INDEX <#ttkGISLayerLOG#><#_IDX_#><#NAME#>'                 +
      ' ON <#ttkGISLayerLOG#> (<#NAME#>)'                                       ;
    ID_CREATE_TABLE_LOG_MANAGER_INDEX = ID_CREATE_TABLE_LOG_MANAGER + 1         ;

    DELETE_LOG_MANAGER =
      'DELETE FROM <#ttkGISLayerLOG#> WHERE <#NAME#>=''%s'''                    ;
    ID_DELETE_LOG_MANAGER = ID_CREATE_TABLE_LOG_MANAGER_INDEX + 1               ;

    INSERT_MASTER_LOG =
      'INSERT INTO <#ttkGISLayerLOG#>'                                          +
      ' VALUES(''%s'',''%s'')'                                                  ;
    ID_INSERT_MASTER_LOG = ID_DELETE_LOG_MANAGER + 1                            ;

    UPDATE_MASTER_LOG =
      'UPDATE <#ttkGISLayerLOG#> SET ENABLED=%s WHERE <#NAME#>=''%s'''          ;
    ID_UPDATE_MASTER_LOG = ID_INSERT_MASTER_LOG + 1                             ;

    CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG =
      'CREATE TRIGGER IF NOT EXISTS "tau_log_%s" AFTER UPDATE ON "%s" '               +
      'FOR EACH ROW WHEN (SELECT ENABLED FROM <#ttkGISLayerLOG#> WHERE NAME=''%s'') ' +
      'BEGIN'                                                                         +
      '  INSERT OR IGNORE INTO %s( ACTION_TYPE, ACTION_DATE, SHAPE_UID )'             +
      '  VALUES( ''U'', datetime(''now''), NEW.UID  ); '                              +
      '  UPDATE %s SET ACTION_DATE=datetime(''now'') WHERE'                           +
      '  changes()=0 AND ACTION_TYPE=''U'' AND SHAPE_UID=NEW.UID; '                   +
      'end ;'                                                                         ;
    ID_CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG =  ID_UPDATE_MASTER_LOG + 1          ;

    CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG =
      'CREATE TRIGGER IF NOT EXISTS "tau_log_%s" AFTER UPDATE ON "%s" '               +
      'FOR EACH ROW WHEN (SELECT ENABLED FROM <#ttkGISLayerLOG#> WHERE NAME=''%s'') ' +
      'BEGIN'                                                                         +
      '  INSERT OR IGNORE INTO %s( ACTION_TYPE, ACTION_DATE, SHAPE_UID )'             +
      '  VALUES( ''U'', datetime(''now''), NEW.UID ); '                               +
      '  UPDATE %s SET ACTION_DATE=datetime(''now'') WHERE'                           +
      '  changes()=0 AND ACTION_TYPE=''U'' AND SHAPE_UID=NEW.UID; '                   +
      'end ; '                                                                        ;
    ID_CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG =
      ID_CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG + 1                                ;

    CREATE_TRIGGER_FEA_AFTER_INSERT_LOG =
      'CREATE TRIGGER IF NOT EXISTS "tai_log_%s" AFTER INSERT ON "%s" '               +
      'FOR EACH ROW WHEN (SELECT ENABLED FROM <#ttkGISLayerLOG#> WHERE NAME=''%s'') ' +
      'BEGIN'                                                                         +
      '  INSERT OR IGNORE INTO %s( ACTION_TYPE, ACTION_DATE, SHAPE_UID )'             +
      '  VALUES( ''I'', datetime(''now''), NEW.uid ); '                               +
      'end ;'                                                                         ;
    ID_CREATE_TRIGGER_FEA_AFTER_INSERT_LOG =
      ID_CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG + 1                                ;

    CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG =
      'CREATE TRIGGER IF NOT EXISTS "tbd_log_%s" BEFORE DELETE ON "%s" '              +
      'FOR EACH ROW WHEN (SELECT ENABLED FROM <#ttkGISLayerLOG#> WHERE NAME=''%s'') ' +
      'BEGIN'                                                                         +
      '  INSERT INTO %s( ACTION_TYPE, ACTION_DATE, SHAPE_UID )'                       +
      '  VALUES( ''D'', datetime(''now''), OLD.UID ); '                               +
      'end ; '                                                                        ;
    ID_CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG =
      ID_CREATE_TRIGGER_FEA_AFTER_INSERT_LOG + 1                                ;

    SELECT_TABLE_LOG =
      'SELECT SHAPE_UID,'                                                             +
      ' MAX(CASE(ACTION_TYPE) WHEN ''I'' THEN ''T'' ELSE ''F'' END) AS INSERTED,'     +
      ' MAX(CASE(ACTION_TYPE) WHEN ''U'' THEN ''T'' ELSE ''F'' END) AS UPDATED,'      +
      ' MAX(CASE(ACTION_TYPE) WHEN ''D'' THEN ''T'' ELSE ''F'' END) AS DELETED,'      +
      ' MAX(ACTION_DATE) AS DATE_ACTION,'                                             +
      ' MAX(STATUS) as STATUS '                                                       +
      'FROM %s GROUP BY SHAPE_UID'                                                    ;
    ID_SELECT_TABLE_LOG = ID_CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG + 1           ;

    ID_END = ID_SELECT_TABLE_LOG                                                ;
  end;

//=============================================================================
// TGIS_LayerSqlAbstract
//=============================================================================

  constructor TGIS_LayerSqlAbstract.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;

    lastNewUid   := -1 ;
    canSkipField := False ;
  end ;

  procedure TGIS_LayerSqlAbstract.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_LayerSqlAbstract.fget_TableMaster
    : String ;
  begin
    Result := getCmd( T_SQLN.ID_MASTERLAYER ) ;
  end ;

  function TGIS_LayerSqlAbstract.fget_TableGeometry
    : String ;
  begin
    Result := Table + '_' + getCmd( T_SQLN.ID_GEO ) ;
  end ;

  function TGIS_LayerSqlAbstract.fget_TableFeatures
    : String ;
  begin
    Result := Table + '_' + getCmd( T_SQLN.ID_FEA ) ;
  end ;

  function TGIS_LayerSqlAbstract.fget_TableIndex
    : String ;
  begin
    Result := Table + '_' + getCmd( T_SQLN.ID_IDX ) ;
  end ;

  function TGIS_LayerSqlAbstract.fget_TableLog
    : String ;
  begin
    Result := Table + '_' + getCmd( T_SQLN.ID_LOG ) ;
  end ;

  function TGIS_LayerSqlAbstract.fget_ViewFeatures
    : String ;
  begin
    {$IFDEF TATUKGIS_PRIVATE}
      if ( not IsStringEmpty( FViewFeatures ) ) and
         ( not IgnoreViewFeatures )
    {$ELSE}
      if not IsStringEmpty( FViewFeatures )
    {$ENDIF}
    then

      Result := FViewFeatures
    else
      Result := TableFeatures ;
  end ;

  function TGIS_LayerSqlAbstract.getSelectColumns : string ;
  begin
    if FSelectColumns = '*' then
      Result := FSelectColumns
    else
      Result := Format('%s,%s',[ getCmd( T_SQLN.ID_UID ), FSelectColumns]) ;
  end ;

  procedure TGIS_LayerSqlAbstract.prepareCommandList ;
  begin
    initializeCommandList( T_SQLN.ID_END ) ;

    assert( FSQLCommands[ T_SQLN.ID_CREATE_MASTER                        ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_MASTER                        ]
                          := T_SQLN.CREATE_MASTER                        ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TABLE_GEO                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TABLE_GEO                     ]
                          := T_SQLN.CREATE_TABLE_GEO                     ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TABLE_FEA                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TABLE_FEA                     ]
                          := T_SQLN.CREATE_TABLE_FEA                     ;
    assert( FSQLCommands[ T_SQLN.ID_DROP_TABLE                           ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_DROP_TABLE                           ]
                          := T_SQLN.DROP_TABLE                           ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_INDEX_NAME                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_INDEX_NAME                    ]
                          := T_SQLN.CREATE_INDEX_NAME                    ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_INDEX_UID                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_INDEX_UID                     ]
                          := T_SQLN.CREATE_INDEX_UID                     ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_INDEX_XMIN                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_INDEX_XMIN                    ]
                          := T_SQLN.CREATE_INDEX_XMIN                    ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_INDEX_XMAX                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_INDEX_XMAX                    ]
                          := T_SQLN.CREATE_INDEX_XMAX                    ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_INDEX_YMIN                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_INDEX_YMIN                    ]
                          := T_SQLN.CREATE_INDEX_YMIN                    ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_INDEX_YMAX                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_INDEX_YMAX                    ]
                          := T_SQLN.CREATE_INDEX_YMAX                    ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_MASTER                        ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_MASTER                        ]
                          := T_SQLN.INSERT_MASTER                        ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_MASTER_OLD                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_MASTER_OLD                    ]
                          := T_SQLN.INSERT_MASTER_OLD                    ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_MASTER                        ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_MASTER                        ]
                          := T_SQLN.UPDATE_MASTER                        ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_MASTER_OLD                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_MASTER_OLD                    ]
                          := T_SQLN.UPDATE_MASTER_OLD                    ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_MASTER                        ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_MASTER                        ]
                          := T_SQLN.SELECT_MASTER                        ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_MASTER_ALL                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_MASTER_ALL                    ]
                          := T_SQLN.SELECT_MASTER_ALL                    ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_TABLE_ALL                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_TABLE_ALL                     ]
                          := T_SQLN.SELECT_TABLE_ALL                     ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_TABLE_ALL_EX                  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_TABLE_ALL_EX                  ]
                          := T_SQLN.SELECT_TABLE_ALL_EX                  ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_TABLE_WHERE                   ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_TABLE_WHERE                   ]
                          := T_SQLN.SELECT_TABLE_WHERE                   ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_TABLE_WHERE_EX                ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_TABLE_WHERE_EX                ]
                          := T_SQLN.SELECT_TABLE_WHERE_EX                ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN_UID                      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN_UID                      ]
                          := T_SQLN.SELECT_JOIN_UID                      ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN                          ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN                          ]
                          := T_SQLN.SELECT_JOIN                          ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN_EX                       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN_EX                       ]
                          := T_SQLN.SELECT_JOIN_EX                       ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN_NOEXT                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN_NOEXT                    ]
                          := T_SQLN.SELECT_JOIN_NOEXT                    ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN_NOEXT_EX                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN_NOEXT_EX                 ]
                          := T_SQLN.SELECT_JOIN_NOEXT_EX                 ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_MAX_UID                       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_MAX_UID                       ]
                          := T_SQLN.SELECT_MAX_UID                       ;
    assert( FSQLCommands[ T_SQLN.ID_MAX_UID                              ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_MAX_UID                              ]
                          := T_SQLN.MAX_UID                              ;
    assert( FSQLCommands[ T_SQLN.ID_DELETE_SHAPE                         ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_DELETE_SHAPE                         ]
                          := T_SQLN.DELETE_SHAPE                         ;
    assert( FSQLCommands[ T_SQLN.ID_DELETE_TABLE                         ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_DELETE_TABLE                         ]
                          := T_SQLN.DELETE_TABLE                         ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_DBX_GEO                       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_DBX_GEO                       ]
                          := T_SQLN.UPDATE_DBX_GEO                       ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_DBX                           ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_DBX                           ]
                          := T_SQLN.UPDATE_DBX                           ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_EX                            ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_EX                            ]
                          := T_SQLN.UPDATE_EX                            ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_DBX_UID_VALUE                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_DBX_UID_VALUE                 ]
                          := T_SQLN.INSERT_DBX_UID_VALUE                 ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_DBX_UID_PARAM                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_DBX_UID_PARAM                 ]
                          := T_SQLN.INSERT_DBX_UID_PARAM                 ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_DBX_GEO_VALUE                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_DBX_GEO_VALUE                 ]
                          := T_SQLN.INSERT_DBX_GEO_VALUE                 ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_DBX_GEO_PARAM                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_DBX_GEO_PARAM                 ]
                          := T_SQLN.INSERT_DBX_GEO_PARAM                 ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT                               ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT                               ]
                          := T_SQLN.INSERT                               ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_DBX                           ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_DBX                           ]
                          := T_SQLN.INSERT_DBX                           ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_DBX_ORA                       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_DBX_ORA                       ]
                          := T_SQLN.UPDATE_DBX_ORA                       ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE                               ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE                               ]
                          := T_SQLN.UPDATE                               ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_DROP_COLUMN                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_DROP_COLUMN                    ]
                          := T_SQLN.ALTER_DROP_COLUMN                    ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_STRING                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_STRING                     ]
                          := T_SQLN.ALTER_ADD_STRING                     ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_MEMO                       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_MEMO                       ]
                          := T_SQLN.ALTER_ADD_MEMO                       ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ALTER_STRING                   ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ALTER_STRING                   ]
                          := T_SQLN.ALTER_ALTER_STRING                   ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_DATE                       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_DATE                       ]
                          := T_SQLN.ALTER_ADD_DATE                       ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_BOOLEAN                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_BOOLEAN                    ]
                          := T_SQLN.ALTER_ADD_BOOLEAN                    ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_INTEGER                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_INTEGER                    ]
                          := T_SQLN.ALTER_ADD_INTEGER                    ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_BIGINT                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_BIGINT                     ]
                          := T_SQLN.ALTER_ADD_BIGINT                     ;
    assert( FSQLCommands[ T_SQLN.ID_ALTER_ADD_FLOAT                      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_ALTER_ADD_FLOAT                      ]
                          := T_SQLN.ALTER_ADD_FLOAT                      ;
    assert( FSQLCommands[ T_SQLN.ID_FILTER_UID                           ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_FILTER_UID                           ]
                          := T_SQLN.FILTER_UID                           ;
    assert( FSQLCommands[ T_SQLN.ID_FILTER_UID_WEAK                      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_FILTER_UID_WEAK                      ]
                          := T_SQLN.FILTER_UID_WEAK                      ;
    assert( FSQLCommands[ T_SQLN.ID_FILTER_TABLE                         ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_FILTER_TABLE                         ]
                          := T_SQLN.FILTER_TABLE                         ;
    assert( FSQLCommands[ T_SQLN.ID_MAX_NAME_LENGTH                      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_MAX_NAME_LENGTH                      ]
                          := T_SQLN.MAX_NAME_LENGTH                      ;
    assert( FSQLCommands[ T_SQLN.ID_MAX_TEXT_LENGTH                      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_MAX_TEXT_LENGTH                      ]
                          := T_SQLN.MAX_TEXT_LENGTH                      ;
    assert( FSQLCommands[ T_SQLN.ID_MASTERLAYER                          ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_MASTERLAYER                          ]
                          := T_SQLN.MASTERLAYER                          ;
    assert( FSQLCommands[ T_SQLN.ID_NAME                                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_NAME                                 ]
                          := T_SQLN.NAME                                 ;
    assert( FSQLCommands[ T_SQLN.ID_XMIN                                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_XMIN                                 ]
                          := T_SQLN.XMIN                                 ;
    assert( FSQLCommands[ T_SQLN.ID_XMAX                                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_XMAX                                 ]
                          := T_SQLN.XMAX                                 ;
    assert( FSQLCommands[ T_SQLN.ID_YMIN                                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_YMIN                                 ]
                          := T_SQLN.YMIN                                 ;
    assert( FSQLCommands[ T_SQLN.ID_YMAX                                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_YMAX                                 ]
                          := T_SQLN.YMAX                                 ;
    assert( FSQLCommands[ T_SQLN.ID_SHAPETYPE                            ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SHAPETYPE                            ]
                          := T_SQLN.SHAPETYPE                            ;
    assert( FSQLCommands[ T_SQLN.ID_SRTEXT                               ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SRTEXT                               ]
                          := T_SQLN.SRTEXT                               ;
    assert( FSQLCommands[ T_SQLN.ID_UID                                  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UID                                  ]
                          := T_SQLN.UID                                  ;
    assert( FSQLCommands[ T_SQLN.ID_GEOMETRY                             ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_GEOMETRY                             ]
                          := T_SQLN.GEOMETRY                             ;
    assert( FSQLCommands[ T_SQLN.ID_GEOMETRY_EX                          ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_GEOMETRY_EX                          ]
                          := T_SQLN.GEOMETRY_EX                          ;
    assert( FSQLCommands[ T_SQLN.ID_GEOMETRY_DECODE                      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_GEOMETRY_DECODE                      ]
                          := T_SQLN.GEOMETRY_DECODE                      ;
    assert( FSQLCommands[ T_SQLN.ID_GEOMETRY_BLOB                        ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_GEOMETRY_BLOB                        ]
                          := T_SQLN.GEOMETRY_BLOB                        ;
    assert( FSQLCommands[ T_SQLN.ID_GEO_TABLE_PARAMS                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_GEO_TABLE_PARAMS                     ]
                          := T_SQLN.GEO_TABLE_PARAMS                     ;
    assert( FSQLCommands[ T_SQLN.ID_GEO                                  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_GEO                                  ]
                          := T_SQLN.GEO                                  ;
    assert( FSQLCommands[ T_SQLN.ID_FEA                                  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_FEA                                  ]
                          := T_SQLN.FEA                                  ;
    assert( FSQLCommands[ T_SQLN.ID_IDX                                  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_IDX                                  ]
                          := T_SQLN.IDX                                  ;
    assert( FSQLCommands[ T_SQLN.ID_LOG                                  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_LOG                                  ]
                          := T_SQLN.LOG                                  ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_COLUMNS_NAME_IN_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_COLUMNS_NAME_IN_TABLE         ]
                          := T_SQLN.SELECT_COLUMNS_NAME_IN_TABLE         ;
    assert( FSQLCommands[ T_SQLN.ID_COLUMN_NAME                          ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_COLUMN_NAME                          ]
                          := T_SQLN.COLUMN_NAME                          ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_DB_AND_CURRENT_USER_NAME      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_DB_AND_CURRENT_USER_NAME      ]
                          := T_SQLN.SELECT_DB_AND_CURRENT_USER_NAME      ;
    assert( FSQLCommands[ T_SQLN.ID_CHECK_RTREE                          ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CHECK_RTREE                          ]
                          := T_SQLN.CHECK_RTREE                          ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN_RTREE                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN_RTREE                    ]
                          := T_SQLN.SELECT_JOIN_RTREE                    ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_JOIN_RTREE_EX                 ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_JOIN_RTREE_EX                 ]
                          := T_SQLN.SELECT_JOIN_RTREE_EX                 ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_RTREE_INDEX                   ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_RTREE_INDEX                   ]
                          := T_SQLN.CREATE_RTREE_INDEX                   ;
    assert( FSQLCommands[ T_SQLN.ID_BUILD_RTREE_INDEX                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_BUILD_RTREE_INDEX                    ]
                          := T_SQLN.BUILD_RTREE_INDEX                    ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_DELETE      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_DELETE      ]
                          := T_SQLN.CREATE_TRIGGER_GEO_AFTER_DELETE      ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_INSERT1     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_INSERT1     ]
                          := T_SQLN.CREATE_TRIGGER_GEO_AFTER_INSERT1     ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_INSERT2     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_INSERT2     ]
                          := T_SQLN.CREATE_TRIGGER_GEO_AFTER_INSERT2     ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_UPDATE      ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_UPDATE      ]
                          := T_SQLN.CREATE_TRIGGER_GEO_AFTER_UPDATE      ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG                     ]
                          := T_SQLN.CREATE_TABLE_LOG                     ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG_INDEX               ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG_INDEX               ]
                          := T_SQLN.CREATE_TABLE_LOG_INDEX               ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG_MANAGER             ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG_MANAGER             ]
                          := T_SQLN.CREATE_TABLE_LOG_MANAGER             ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG_MANAGER_INDEX       ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TABLE_LOG_MANAGER_INDEX       ]
                          := T_SQLN.CREATE_TABLE_LOG_MANAGER_INDEX       ;
    assert( FSQLCommands[ T_SQLN.ID_DELETE_LOG_MANAGER                   ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_DELETE_LOG_MANAGER                   ]
                          := T_SQLN.DELETE_LOG_MANAGER                   ;
    assert( FSQLCommands[ T_SQLN.ID_INSERT_MASTER_LOG                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_INSERT_MASTER_LOG                    ]
                          := T_SQLN.INSERT_MASTER_LOG                    ;
    assert( FSQLCommands[ T_SQLN.ID_UPDATE_MASTER_LOG                    ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_UPDATE_MASTER_LOG                    ]
                          := T_SQLN.UPDATE_MASTER_LOG                    ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG  ]
                          := T_SQLN.CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG  ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG  ]
                          := T_SQLN.CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG  ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_FEA_AFTER_INSERT_LOG  ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_FEA_AFTER_INSERT_LOG  ]
                          := T_SQLN.CREATE_TRIGGER_FEA_AFTER_INSERT_LOG  ;
    assert( FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG ]
                          := T_SQLN.CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG ;
    assert( FSQLCommands[ T_SQLN.ID_SELECT_TABLE_LOG                     ] = '' ) ;
            FSQLCommands[ T_SQLN.ID_SELECT_TABLE_LOG                     ]
                          := T_SQLN.SELECT_TABLE_LOG                     ;
    finalizeCommandList ;
  end ;

  function TGIS_LayerSqlAbstract.getCmdGEOUID(
    const _id : Integer
   ) : String ;
  begin
    assert( _id in [0..1] ) ;
    if      _id = 0 then
            Result := getCmd( T_SQLN.ID_UID )
    else if _id = 1 then
            Result := getCmd( T_SQLN.ID_FEA ) + '.' + getCmd( T_SQLN.ID_UID ) ;
  end ;

  function TGIS_LayerSqlAbstract.getCmdSHAPETYPE : String ;
  begin
    Result := getCmd( T_SQLN.ID_SHAPETYPE ) ;
  end ;

  function TGIS_LayerSqlAbstract.getCmdXMIN : String ;
  begin
    Result := getCmd( T_SQLN.ID_XMIN ) ;
  end ;

  function TGIS_LayerSqlAbstract.getCmdYMIN : String ;
  begin
    Result := getCmd( T_SQLN.ID_YMIN ) ;
  end ;

  function TGIS_LayerSqlAbstract.getCmdGEOMETRY : String ;
  begin
    Result := getCmd( T_SQLN.ID_GEOMETRY ) ;
  end ;

  function TGIS_LayerSqlAbstract.getCmdGEOMETRY_CAST : String ;
  begin
    if oGisDb.IsPostgreSql then
      Result := getCmd( T_SQLN.ID_GEOMETRY_DECODE )
    else
      Result := getCmd( T_SQLN.ID_GEOMETRY_BLOB ) ;
  end ;

  function TGIS_LayerSqlAbstract.setShapeType(
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) : Integer ;
  begin
    case _type of
      TGIS_ShapeType.Point       : Result := 2 ;
      TGIS_ShapeType.MultiPoint  : Result := 3 ;
      TGIS_ShapeType.Arc         : Result := 4 ;
      TGIS_ShapeType.Polygon     : Result := 5 ;
      TGIS_ShapeType.Complex     : Result := 6 ;
      TGIS_ShapeType.MultiPatch  : Result := 7
      else                         Result := 0
    end ;

    case _dim of
      TGIS_DimensionType.XYZ   : inc( Result, 1000 ) ;
      TGIS_DimensionType.XYM   : inc( Result, 2000 ) ;
      TGIS_DimensionType.XYZM  : inc( Result, 3000 ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.checkShapeType(
    const _val    : Integer ;
      var _type   : TGIS_ShapeType ;
      var _dim    : TGIS_DimensionType
  ) ;
  var
    stype : Integer     ;
    dim   : Integer     ;
  begin
    dim := _val div 1000 ;
    case dim of
      1 :   _dim := TGIS_DimensionType.XYZ  ;
      2 :   _dim := TGIS_DimensionType.XYM  ;
      3 :   _dim := TGIS_DimensionType.XYZM ;
      else  _dim := TGIS_DimensionType.XY   ;
    end ;

    stype := _val mod 1000 ;
    case stype of
      2 :   _type := TGIS_ShapeType.Point      ;
      3 :   _type := TGIS_ShapeType.MultiPoint ;
      4 :   _type := TGIS_ShapeType.Arc        ;
      5 :   _type := TGIS_ShapeType.Polygon    ;
      6 :   _type := TGIS_ShapeType.Complex    ;
      7 :   _type := TGIS_ShapeType.MultiPatch ;
      else  _type := TGIS_ShapeType.Unknown    ;
    end ;

  end ;

  procedure TGIS_LayerSqlAbstract.prepareParamsCommand(
    const _table  : String ;
    const _params : TStrings
  ) ;
  var
     i : Integer ;
  begin
    _params.Clear ;

    if ( _table = TableGeometry ) then begin
      _params.Text := getCmd( T_SQLN.ID_GEO_TABLE_PARAMS ) ;
    end
    else begin
      _params.Add( getCmd( T_SQLN.ID_UID )+'=' ) ;

      for i:=0 to Fields.Count-1 do begin
        if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        _params.Add( FieldInfo(i).ExportName+'=' ) ;
      end ;
    end ;

  end ;

  function TGIS_LayerSqlAbstract.prepareAppendCmd(
    const _table  : String ;
    const _filter : String
  ) : String ;
  var
    i, cnt : Integer ;
    params : String  ;
    tkn    : TGIS_Tokenizer ;
  begin
    Result := '' ;
    if IsStringEmpty( _table ) then exit ;

    columnsNames := '' ;

    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLN.ID_SELECT_TABLE_WHERE_EX ),
                        [ prepareColumnsNames( _table ), _table, _filter ]
                      )
    else
      Result := Format( getCmd( T_SQLN.ID_SELECT_TABLE_ALL_EX ),
                        [ prepareColumnsNames( _table ), _table ]
                      ) ;
    try
      tkn := TGIS_Tokenizer.Create ;
    finally
      tkn.Execute( prepareColumnsNames( _table ), [','] ) ;
      cnt := tkn.Result.Count ;
      FreeObject( tkn ) ;
    end ;

    params := '' ;
    for i := 0 to cnt-1 do begin
      params := params + oGisDb.safeParam( IntToStr(i+1) ) ;
      if i<cnt-1 then
         params := params + ',' ;
    end ;

    Result := Result + ';' + Format( getCmd( T_SQLN.ID_INSERT ),
                                     [ _table, prepareColumnsNames( _table ), params ]
                                   ) ;
  end ;

  function TGIS_LayerSqlAbstract.prepareUpdateCmd(
    const _table  : String ;
    const _filter : String
  ) : String ;
  var
    i, cnt : Integer ;
    params : String  ;
    tkn    : TGIS_Tokenizer ;
  begin
    Result := '' ;
    if IsStringEmpty( _table ) then exit ;

    columnsNames := '' ;

    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLN.ID_SELECT_TABLE_WHERE_EX ),
                        [ prepareColumnsNames( _table ), _table, _filter ]
                      )
    else
      Result := Format( getCmd( T_SQLN.ID_SELECT_TABLE_ALL_EX ),
                        [ prepareColumnsNames( _table ), _table ]
                      ) ;
    try
      tkn := TGIS_Tokenizer.Create ;

      tkn.Execute( prepareColumnsNames( _table ), [','] ) ;

      cnt := tkn.Result.Count ;
      params := '' ;
      for i:=0 to cnt-1 do begin
        //if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

        if not IsStringEmpty( params ) then
          params := params + ',' ;
        params := params + safeName ( tkn.Result.Strings[i] ) +
                     '=' + safeParam( IntToStr( i + 1 ) ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;

    Result := Result + ';' + Format( getCmd( T_SQLN.ID_UPDATE_EX ),
                                     [ _table,
                                       params,
                                       _filter ]
                                   ) ;
  end ;

  function TGIS_LayerSqlAbstract.prepareAppendCommand(
    const _table : String
   ) : String ;
  var
    i     : Integer ;
    param : TStringBuilder  ;
    value : TStringBuilder  ;
    tmp   : String  ;
  begin
    param := TStringBuilder.Create ;
    value := TStringBuilder.Create ;
    try
      if _table = TableGeometry then begin
        value.Append( getCmd( T_SQLN.ID_INSERT_DBX_GEO_VALUE ) ) ;
        param.Append( getCmd( T_SQLN.ID_INSERT_DBX_GEO_PARAM ) ) ;
        if oGisDb.IsOracle then
          tmp := getCmd( T_SQLN.ID_UPDATE_DBX_ORA )
        else
          tmp := '' ;
      end
      else begin
        value.Append( getCmd( T_SQLN.ID_INSERT_DBX_UID_VALUE ) ) ;
        param.Append( getCmd( T_SQLN.ID_INSERT_DBX_UID_PARAM ) ) ;

        for i := 0 to Fields.Count-1 do begin
          if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

          value.Append( ',' ) ;
          value.Append( safeName( FieldInfo( i ).ExportName ) ) ;

          param.Append( ',:' ) ;
          param.Append( FieldInfo( i ).ExportName ) ;
        end ;
      end ;

      Result := Format( getCmd( T_SQLN.ID_INSERT_DBX ),
                        [_table, value.ToString, param.ToString, tmp]
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.prepareAppendParams(
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
        value.Append( getCmd( T_SQLN.ID_INSERT_DBX_GEO_VALUE ) ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_UID ) ) ) ;
        param.Append( ',' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_XMIN ) ) ) ;
        param.Append( ',' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_XMAX ) ) ) ;
        param.Append( ',' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_YMIN ) ) ) ;
        param.Append( ',' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_YMAX ) ) ) ;
        param.Append( ',' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_SHAPETYPE ) ) ) ;
        param.Append( ',' ) ;
        if oGisDb.IsOracle and not oGisDb.IsJDBC then begin
          param.Append( getCmd( T_SQLN.ID_GEOMETRY_EX ) ) ;
          tmp := Format( 'returning %s into %s', [
                          getCmd( T_SQLN.ID_GEOMETRY ),
                          safeParam( getCmd( T_SQLN.ID_GEOMETRY ) ) ]
                        )
        end
        else begin
          param.Append( safeParam( getCmd( T_SQLN.ID_GEOMETRY ) ) ) ;
          tmp := '' ;
        end ;
      end
      else begin
        value.Append( getCmd( T_SQLN.ID_INSERT_DBX_UID_VALUE ) ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_UID ) ) ) ;

        for i := 0 to Fields.Count-1 do begin
          if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

          value.Append( ',' ) ;
          value.Append( safeName( FieldInfo( i ).ExportName ) ) ;

          param.Append( ',' ) ;
          param.Append( safeParam( FieldInfo( i ).ExportName ) ) ;
        end ;
        tmp := '' ;
      end ;

      Result := Format( getCmd( T_SQLN.ID_INSERT_DBX ),
                        [_table, value.ToString, param.ToString, tmp]
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroAppendParams(
    const _table : String
   ) ;
  var
    i       : Integer ;
    pname   : String ;
    size    : Integer ;
    pdtype  : TGIS_DataType ;
  begin
    if _table = TableGeometry then begin
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_UID ) ),
        TGIS_DataType.Integer,
        TGIS_SubDataType.Unknown,
        8
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_XMIN ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_XMAX ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_YMIN ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_YMAX ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_SHAPETYPE ) ),
        TGIS_DataType.Integer,
        TGIS_SubDataType.Unknown,
        8
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_GEOMETRY ) ),
        TGIS_DataType.Blob,
        TGIS_SubDataType.Unknown,
        -1
       ) ;
    end
    else begin
      oGisDb.sqlTableCreateParam(
        1,
        safeParam( getCmd( T_SQLN.ID_UID ) ),
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

  function TGIS_LayerSqlAbstract.prepareAppendCommandEx(
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

      Result := Format( getCmd( T_SQLN.ID_INSERT ),
                        [ _table, value.ToString, param.ToString ]
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.prepareUpdateCommand(
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
        param.Append( getCmd( T_SQLN.ID_UPDATE_DBX_GEO ) ) ;
        if oGisDb.IsOracle then
          tmp := getCmd( T_SQLN.ID_UPDATE_DBX_ORA )
        else
          tmp := '' ;
      end
      else begin
        for i := 0 to Fields.Count - 1 do begin
          if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;

          param.Append( FieldInfo(i).ExportName ) ;
          param.Append( '=:' ) ;
          param.Append( FieldInfo(i).ExportName ) ;
          if i < Fields.Count - 1 then
            param.Append( ',' ) ;
        end ;
        tmp := '' ;
      end ;
      Result := Format( getCmd( T_SQLN.ID_UPDATE_DBX ),
                        [_table, param.ToString, _column, _column, tmp ]
                      ) ;
    finally
      FreeObject( param ) ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.prepareUpdateParams(
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
        param.Append( getCmd( T_SQLN.ID_XMIN ) + '=' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_XMIN ) ) ) ;
        param.Append( ',' ) ;
        param.Append( getCmd( T_SQLN.ID_XMAX ) + '=' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_XMAX ) ) ) ;
        param.Append( ',' ) ;
        param.Append( getCmd( T_SQLN.ID_YMIN ) + '=' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_YMIN ) ) ) ;
        param.Append( ',' ) ;
        param.Append( getCmd( T_SQLN.ID_YMAX ) + '=' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_YMAX ) ) ) ;
        param.Append( ',' ) ;
        param.Append( getCmd( T_SQLN.ID_SHAPETYPE ) + '=' ) ;
        param.Append( safeParam( getCmd( T_SQLN.ID_SHAPETYPE ) ) ) ;
        param.Append( ',' ) ;
        param.Append( getCmd( T_SQLN.ID_GEOMETRY ) + '=' ) ;

        if oGisDb.IsOracle and not oGisDb.IsJDBC then begin
          param.Append( getCmd( T_SQLN.ID_GEOMETRY_EX ) ) ;
          tmp := Format( 'returning %s into %s', [
                          getCmd( T_SQLN.ID_GEOMETRY ),
                          safeParam( getCmd( T_SQLN.ID_GEOMETRY ) ) ]
                        )
        end
        else begin
          param.Append( safeParam( getCmd( T_SQLN.ID_GEOMETRY ) ) ) ;
          tmp := '' ;
        end ;
      end
      else begin
        for i := 0 to Fields.Count - 1 do begin
          if not (TGIS_FieldFlags.Exportable in FieldInfo(i).Flags) then continue ;
          param.Append( safeName( FieldInfo(i).ExportName ) ) ;
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

  procedure TGIS_LayerSqlAbstract.macroUpdateParams(
    const _table : String
   ) ;
  var
    i       : Integer ;
    pname   : String ;
    size    : Integer ;
    pdtype  : TGIS_DataType ;
  begin
    if _table = TableGeometry then begin
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_XMIN ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_XMAX ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_YMIN ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_YMAX ) ),
        TGIS_DataType.Float,
        TGIS_SubDataType.Unknown,
        20
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_SHAPETYPE ) ),
        TGIS_DataType.Integer,
        TGIS_SubDataType.Unknown,
        8
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_GEOMETRY ) ),
        TGIS_DataType.Blob,
        TGIS_SubDataType.Unknown,
        -1
       ) ;
      oGisDb.sqlTableCreateParam(
        0,
        safeParam( getCmd( T_SQLN.ID_UID ) ),
        TGIS_DataType.Integer,
        TGIS_SubDataType.Unknown,
        8
       ) ;

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
      oGisDb.sqlTableCreateParam(
        1,
        safeParam( getCmd( T_SQLN.ID_UID ) ),
        TGIS_DataType.Integer,
        TGIS_SubDataType.Unknown,
        8
       ) ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.prepareUpdateCommandEx(
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

    Result := Format( getCmd( T_SQLN.ID_UPDATE ),
                      [ _table,
                        param,
                        getCmdGEOUID(0),
                        _params.Values[ getCmdGEOUID(0) ]
                      ]
                    ) ;
  end ;

  function TGIS_LayerSqlAbstract.prepareSelectCommand(
    const _table  : String ;
    const _filter : String
   ) : String ;
  begin
    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLN.ID_SELECT_TABLE_WHERE ),
                        [ getSelectColumns, _table, _filter ]
                      )
    else
      Result := Format( getCmd( T_SQLN.ID_SELECT_TABLE_ALL ), [ _table ] ) ;
  end ;

  function TGIS_LayerSqlAbstract.prepareFilterUid(
    const _uid : TGIS_Uid
   ) : String ;
  begin
    Result := Format( getCmd( T_SQLN.ID_FILTER_UID ), [ getCmdGEOUID(0), _uid ] ) ;
  end ;

  function TGIS_LayerSqlAbstract.prepareColumnsNames(
    const _table : String
  ) : String ;
  begin
    if IsStringEmpty( columnsNames ) then begin
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_COLUMNS_NAME_IN_TABLE ),
                                   [ strSchema, LowerCase( _table ) ]
                                 ),0
                         ) ;
      try
        columnsNames := '' ;
        try
          while not oGisDb.sqlQueryEof(0) do begin
            columnsNames := columnsNames +
              safeName( VarToString(
                          oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_COLUMN_NAME ), 0 )
                                   )
                      ) ;

            oGisDb.sqlQueryMoveNext(0) ;

            if (not oGisDb.sqlQueryEof(0)) and (not IsStringEmpty(columnsNames)) then
              columnsNames := columnsNames + ',' ;
          end ;
        except
          columnsNames := safeName( getCmd( T_SQLN.ID_UID ) ) ;
        end ;
      finally
        if IsStringEmpty(columnsNames) then
          columnsNames := safeName( getCmd( T_SQLN.ID_UID ) ) ;

        oGisDb.sqlQueryClose(0) ;
      end ;
    end ;

    Result := columnsNames ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroConnect ;
  begin
    initializeConnect( True ) ;

    if oGisDb.IsSqlite then
      hasLogging := UpperCase( FSQLParameters.Values[ GIS_INI_LAYERSQL_LOGGING ]
                              ) = 'TRUE'
    else
      hasLogging := False ;

    hasStyle := not IsStringEmpty( FSQLParameters.Values[ GIS_INI_STYLE ] ) ;
    mobileVersion := UpperCase( FSQLParameters.Values[ GIS_INI_LAYERSQL_MOBILEVERSION ]
                              ) = 'TRUE' ;
    FSelectColumns := ParamString( FSQLParameters.Values[ 'SelectColumns' ], '*' ) ;

    try
      oGisDb.sqlConnect( oGisDb.sqlBaseFolder(self), FSQLParameters );

      if oGisDb.IsPostgreSql then begin
        parseConfigLayerName ;

        updateDialectList( 'UID'      , 'uid'       ) ;
        updateDialectList( 'GEOMETRY' , 'geometry'  ) ;
        updateDialectList( 'SHAPETYPE', 'shapetype' ) ;
        {$IFDEF JAVA}
        updateDialectList( 'LOGICAL'  , 'boolean'   ) ;
        {$ENDIF}

        prepareCommandList ;
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

  procedure TGIS_LayerSqlAbstract.macroDisconnect ;
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

  procedure TGIS_LayerSqlAbstract.macroUpdateStart ;
  begin
    lastUid := -1 ;
    oGisDb.sqlTransactGlobalUpdateStart ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroUpdateEnd ;
  begin
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
    oGisDb.sqlTransactGlobalUpdateCommit ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroBuildRtree ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      if oGisDb.IsSqlite and not mobileVersion then begin
        oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_CHECK_RTREE ),
                                    [ TableIndex ]
                                    ), 0
                            ) ;
        if not oGisDb.sqlQueryEof(0) then begin
          // index table exists
          oGisDb.sqlQueryClose(0) ;
          try
            oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_BUILD_RTREE_INDEX ),
                                    [ TableIndex, TableGeometry ]
                                   )
                           ) ;
          except
            // can exist
          end ;

          try
            oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_DELETE ),
                                    [ TableGeometry, TableGeometry, TableIndex  ]
                                   )
                           ) ;
            oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_INSERT1 ),
                                    [ TableGeometry, TableGeometry, TableIndex ]
                                   )
                           ) ;
            oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_INSERT2 ),
                                    [ TableGeometry, TableGeometry, TableIndex ]
                                   )
                           ) ;
            oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_UPDATE ),
                                    [ TableGeometry, TableGeometry, TableIndex ]
                                   )
                           ) ;
          except
            // can exist
          end ;
        end
        else
          oGisDb.sqlQueryClose(0) ;
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroMasterCreate ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      oGisDb.sqlExec( getCmd( T_SQLN.ID_CREATE_MASTER     ) ) ;
      oGisDb.sqlExec( getCmd( T_SQLN.ID_CREATE_INDEX_NAME ) ) ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroMasterLogCreate ;
  begin
    if IsReadOnly then exit ;

    if hasLogging then begin
      oGisDb.sqlTransactRestructStart ;
      try
        try
          oGisDb.sqlExec( getCmd( T_SQLN.ID_CREATE_TABLE_LOG_MANAGER ) ) ;
          oGisDb.sqlExec( getCmd( T_SQLN.ID_CREATE_TABLE_LOG_MANAGER_INDEX ) ) ;
        except

        end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroMasterStyleCreate ;
  begin
    if hasStyle then
      CreateStyleTable ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroMasterStyleUpdate(
    const _layer  : TGIS_LayerVector
  ) ;
  begin
    if hasStyle then
      WriteStyleEx( FSQLParameters.Values[ GIS_INI_STYLE ], _layer ) ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroEnableLogging ;
  begin
    if hasLogging then begin
      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_UPDATE_MASTER_LOG ), ['1',Table] ) ) ;
      except

      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroDisableLogging ;
  begin
    if hasLogging then begin
      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_UPDATE_MASTER_LOG ), ['0',Table] ) ) ;
      except

      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroMasterUpdate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _name   : String         ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    vtype : Integer ;
  begin
    if IsReadOnly then exit ;

    // guarantee connection closing
    oGisDb.sqlQueryClose(0) ;
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;

    vtype := setShapeType( _type, _dim ) ;
    try
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_UPDATE_MASTER ),
                               [ DotFloatToStr( _extent.XMin ),
                                 DotFloatToStr( _extent.XMax ),
                                 DotFloatToStr( _extent.YMin ),
                                 DotFloatToStr( _extent.YMax ),
                                 vtype, CS.FullWKT, Table
                               ]
                             )
                      ) ;
    except
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_UPDATE_MASTER_OLD ),
                               [ DotFloatToStr( _extent.XMin ),
                                 DotFloatToStr( _extent.XMax ),
                                 DotFloatToStr( _extent.YMin ),
                                 DotFloatToStr( _extent.YMax ),
                                 vtype, Table
                               ]
                             )
                      ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroTableCreate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    vtype : Integer ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TABLE_GEO ),
                              [ TableGeometry ]
                             )
                     ) ;
      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_INDEX_UID ),
                               [ TableGeometry, TableGeometry ]
                               )
                       ) ;
      except
        // can exist
      end ;

      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_INDEX_XMIN ),
                       [ TableGeometry, TableGeometry ]
                     )
             ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_INDEX_XMAX ),
                       [ TableGeometry, TableGeometry ]
                     )
             ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_INDEX_YMIN ),
                       [ TableGeometry, TableGeometry ]
                     )
             ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_INDEX_YMAX ),
                       [ TableGeometry, TableGeometry ]
                     )
             ) ;

      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TABLE_FEA  ),
                       [ TableFeatures ]
                     )
             ) ;
      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_INDEX_UID  ),
                         [ TableFeatures, TableFeatures ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        if oGisDb.IsSqlite and not mobileVersion then
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_RTREE_INDEX  ),
                                  [ TableIndex ]
                                 )
                         ) ;
      except
        // can exist
      end ;

      vtype := setShapeType( _type, _dim ) ;
      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_INSERT_MASTER  ),
                         [ Table,
                           DotFloatToStr(_extent.XMin),
                           DotFloatToStr(_extent.XMax),
                           DotFloatToStr(_extent.YMin),
                           DotFloatToStr(_extent.YMax),
                           vtype, CS.FullWKT
                         ]
                       )
                ) ;
      except
        try
          // old structure coexistence
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_INSERT_MASTER_OLD ),
                           [ Table,
                             DotFloatToStr(_extent.XMin),
                             DotFloatToStr(_extent.XMax),
                             DotFloatToStr(_extent.YMin),
                             DotFloatToStr(_extent.YMax),
                             vtype
                           ]
                         )
                  ) ;
        except
          macroMasterUpdate( _extent, _type, Table, DefaultDimension ) ;
        end ;
      end ;

      if hasLogging then begin
        try
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TABLE_LOG ), [TableLog] ) ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TABLE_LOG_INDEX ),
                                  [ TableLog, TableLog ]
                                 )
                        ) ;
        except

        end ;

        try
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_INSERT_MASTER_LOG  ),
                                  [ Table, '1' ]
                                 )
                         ) ;
        except
          // can exist
        end ;

        try
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_GEO_AFTER_UPDATE_LOG ),
                                  [ TableGeometry, TableGeometry, Table,
                                    TableLog, TableLog ]
                                 )
                         ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_FEA_AFTER_UPDATE_LOG  ),
                                  [ TableFeatures, TableFeatures, Table,
                                    TableLog, TableLog ]
                                 )
                        ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_FEA_AFTER_INSERT_LOG ),
                                  [ TableFeatures, TableFeatures, Table, TableLog ]
                                 )
                        ) ;
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_CREATE_TRIGGER_FEA_BEFORE_DELETE_LOG ),
                                  [ TableFeatures, TableFeatures, Table, TableLog ]
                                 )
                        ) ;
        except

        end ;
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroTableAlter(
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
    ltext    : Integer ;
    name_len : Integer ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlQueryClose(0) ;
    oGisDb.sqlTableClose( 0 ) ;

    try
      lname := StrToInt( getCmd( T_SQLN.ID_MAX_NAME_LENGTH ) ) ;
    except
      lname := 16 ;
    end ;

    try
      if not IsStringEmpty( getCmd( T_SQLN.ID_MAX_TEXT_LENGTH ) ) then
        ltext := StrToInt( getCmd( T_SQLN.ID_MAX_TEXT_LENGTH ) )
      else
        ltext := GIS_SQL_MEMO_SIZE-1 ;
    except
      ltext := GIS_SQL_MEMO_SIZE-1 ;
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
            oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_DROP_COLUMN ),
                             [ TableFeatures, fname ]
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
                  if fld.NewWidth <= ltext then
                     oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_STRING ),
                                      [ TableFeatures, safeName(fname), fld.NewWidth ]
                                    )
                            )
                  else
                     oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_MEMO ),
                                      [ TableFeatures, safeName(fname) ]
                                    )
                            ) ;

                TGIS_FieldType.Date :
                   oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_DATE ),
                                    [ TableFeatures, safeName(fname) ]
                                  )
                          ) ;
                TGIS_FieldType.Boolean :
                   oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_BOOLEAN ),
                                    [ TableFeatures, safeName(fname) ]
                                  )
                          ) ;
                TGIS_FieldType.Number :
                 begin
                   if fld.NewDecimal = 0 then begin
                      if fld.Width <= 9 then
                        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_INTEGER ),
                                        [ TableFeatures, safeName(fname) ]
                                        )
                                )
                      else
                        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_BIGINT ),
                                        [ TableFeatures, safeName(fname) ]
                                       )
                                ) ;
                   end
                   else
                     oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_FLOAT ),
                                     [ TableFeatures, safeName(fname) ]
                                    )
                            ) ;
                 end ;
                TGIS_FieldType.Float :
                     oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ADD_FLOAT ),
                                      [ TableFeatures, safeName(fname) ]
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
                    oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_ALTER_ALTER_STRING ),
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

  procedure TGIS_LayerSqlAbstract.macroTableDrop ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DROP_TABLE ),
                                [ TableGeometry ]
                              )
                      ) ;
      except
        // can not exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DROP_TABLE ),
                                [ TableFeatures ]
                              )
                      ) ;
      except
        // can not exist
      end ;

      try
        if oGisDb.IsSqlite and not mobileVersion then
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DROP_TABLE ),
                                  [ TableIndex ]
                                )
                        ) ;
      except
        // can not exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DELETE_TABLE ),
                                [ Table ]
                              )
                      ) ;
      except
        // can not exist
      end ;

      if hasLogging then begin
        try
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DELETE_LOG_MANAGER ),
                                  [ Table ]
                                )
                        ) ;
        except
          // can not exist
        end ;
        try
          oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DROP_TABLE ),
                                  [ TableLog ]
                                )
                        ) ;
        except
          // can not exist
        end ;
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroTableSetField(
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
        oGisDb.sqlTableSetField( _id, FieldInfo(i).ExportName, _val,
                                 FieldInfo(i).NewWidth )
      else // internal field
        oGisDb.sqlTableSetField( _id, _name, _val, 1 );
    except
      // maybe field name was changed by someone else.....
      assert( False, 'Field ''' + _name + ''' does not exists.' ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroAddField(
     const _uidname : String         ;
     const _name    : String         ;
     const _type    : TGIS_FieldType ;
     const _width   : Integer        ;
     const _decimal : Integer
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, True, 0 ) ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroAddField(
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

  procedure TGIS_LayerSqlAbstract.macroAddField(
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

  procedure TGIS_LayerSqlAbstract.macroQueryStructure ;
  begin
    Fields.Clear ;

    // try to return en empty set to save the time
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_TABLE_WHERE ),
                          [ getSelectColumns, ViewFeatures,
                            '1=0'
                          ]
                        ), 0
                ) ;
    {$IFNDEF OXYGENE}
      oGisDb.sqlQueryStructure( ViewFeatures, getCmd( T_SQLN.ID_UID ),  macroAddField );
    {$ELSE}
      oGisDb.sqlQueryStructure( ViewFeatures, getCmd( T_SQLN.ID_UID ), @macroAddField );
    {$ENDIF}
    oGisDb.sqlQueryClose(0) ;

    ReadFieldRules ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroSetSharedConnection(
    const _ll : TGIS_LayerVector
  ) ;
  begin

  end ;

  procedure TGIS_LayerSqlAbstract.macroShapeDelete(
    const _uid  : TGIS_Uid
  ) ;
  begin
    if IsReadOnly then exit ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DELETE_SHAPE ),
                       [ TableGeometry, _uid ]
                     )
             ) ;
    except
    end ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLN.ID_DELETE_SHAPE ),
                       [ TableFeatures, _uid ]
                     )
              ) ;
    except
    end ;

  end ;

  procedure TGIS_LayerSqlAbstract.macroShapeUpdate(
    const _shp    : TGIS_Shape ;
    const _import : Boolean
  ) ;
  begin
    macroShapeUpdate( _shp, _import, _shp.Dimension ) ;
  end ;

  procedure TGIS_LayerSqlAbstract.macroShapeUpdate(
    const _shp    : TGIS_Shape ;
    const _import : Boolean ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    i         : Integer        ;
    fldinfo   : TGIS_FieldInfo ;
    fld       : Variant        ;
    uid       : TGIS_Uid        ;
    txt       : String         ;
    iloop     : Integer        ;
    guaranted : Boolean        ;
    skipField : Boolean        ;
    skipShape : Boolean        ;
  begin
    if IsReadOnly then exit ;

    guaranted := False ;

    oGisDb.sqlTransactUpdateStart ;
    try
      uid := -1 ;

      iloop := T_SQLN.RETRY_COUNT ;
      while iloop > 0 do begin
        try
          // update geometry
          skipShape := False ;
          if _import then begin
            uid := lastUid ;
            if uid < 0 then uid := macroUidNew( guaranted )
                       else inc( uid ) ;
            if not guaranted then
              lastUid := uid ;

            if iloop = T_SQLN.RETRY_COUNT then
              sqlTableAppend( 0, TableGeometry ) ;
            macroTableSetField( 0, getCmd( T_SQLN.ID_UID ) , uid   ) ;
          end
          else begin
            uid := _shp.Uid ;

            if ( MultiUserMode = TGIS_MultiUser.SingleUser ) and _shp.IsNewShape then
              sqlTableAppend( 0, TableGeometry )
            else begin
              skipShape := not _shp.GeometryChanged and not _shp.IsNewShape ;
              if not skipShape then
                sqlTableOpenWrite( 0, TableGeometry, getCmdGEOUID(0), uid  ) ;
            end ;

            if not skipShape then
              macroTableSetField( 0, getCmd( T_SQLN.ID_UID ) , uid ) ;
          end ;

          if not skipShape then begin
            macroTableSetField( 0, getCmd( T_SQLN.ID_XMIN ), _shp.Extent.XMin ) ;
            macroTableSetField( 0, getCmd( T_SQLN.ID_XMAX ), _shp.Extent.XMax ) ;
            macroTableSetField( 0, getCmd( T_SQLN.ID_YMIN ), _shp.Extent.YMin ) ;
            macroTableSetField( 0, getCmd( T_SQLN.ID_YMAX ), _shp.Extent.YMax ) ;
            macroTableSetField( 0, getCmd( T_SQLN.ID_SHAPETYPE),
                                setShapeType( _shp.ShapeType, _shp.Dimension )
                               ) ;
            if ( _shp.ShapeType = TGIS_ShapeType.Point  ) and
               ( _dim = TGIS_DimensionType.XY           ) then begin
              // do nothing - extent is enough to save point feature
              // so saving binary fields is not required
              {$IFDEF JAVA}
                // fix to insert null into bytea(blob) in postgres
                oGisDb.sqlTableSetGeometry( 0, getCmdGEOMETRY, NullVar, nil );
              {$ENDIF}
            end
            else
              sqlTableSetGeometry( 0, getCmdGEOMETRY, _shp ) ;

            sqlTablePost( 0 ) ;
          end ;

          iloop := 0 ;
        except
          if not guaranted then begin
            dec( iloop ) ;
            if iloop <= 0 then raise ;
            Sleep( GetRandom( T_SQLN.RETRY_INTERVAL ) ) ;
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
    if ( ViewFeatures = TableFeatures )
    then begin
      // update features
      if _import or _shp.IsNewShape then begin
        skipField := canSkipField and not oGisDb.IsPostgreSql and _shp.IsNewShape ;
        sqlTableAppend( 1, TableFeatures )
      end
      else begin
        if not _shp.FieldChanged then exit ;

        skipField := canSkipField and not oGisDb.IsPostgreSql ;
        sqlTableOpenWrite( 1, TableFeatures, getCmdGEOUID(0), uid ) ;
      end ;

      macroTableSetField( 1, getCmd( T_SQLN.ID_UID ), uid   ) ;

      for i := 0 to Fields.Count - 1 do begin

        fldinfo := FieldInfo( i ) ;
        if not (TGIS_FieldFlags.Exportable in fldinfo.Flags) then continue ;

        // skip unchanged fields due to Access limitation (maximum 99 columns)
        if skipField then begin
          if _shp.Layer.FieldInfo(i).NewName = fldinfo.NewName then begin
            if not _shp.IsFieldModifiedEx( i ) then continue ;
          end
          else if not _shp.IsFieldModified( fldinfo.NewName ) then continue ;
        end ;

        fld := _shp.GetFieldEx( fldinfo.NewName ) ;

        case fldinfo.FieldType of
          TGIS_FieldType.String :
             begin
               if VarIsNull( fld ) then
                 macroTableSetField( 1, fldinfo.NewName, fld )
               else begin
                 txt := VarToString( fld ) ;
                 macroTableSetField( 1, fldinfo.NewName, txt ) ;
               end ;
             end ;
          TGIS_FieldType.Date :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld ) ;
             end ;
          TGIS_FieldType.Boolean :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld )
             end ;
          TGIS_FieldType.Number :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld ) ;
             end ;
          TGIS_FieldType.Float :
             begin
               macroTableSetField( 1, fldinfo.NewName, fld ) ;
             end ;
          else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
        end ;
      end ;

      sqlTablePost( 1 )  ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.macroUidLast : TGIS_Uid ;
  var
    v : Variant ;
  begin
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_MAX_UID ),
                                [ TableGeometry ] ), 0 ) ;
    try
      try
        v := oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_MAX_UID ), 0 ) ;
        if VarIsNull( v ) then Result := 0
                          else Result := VarToInt64( v ) ;
      except
        Result := 0 ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.macroUidReserve : TGIS_Uid ;
  var
    iloop : Integer ;
    uid   : TGIS_Uid ;
    quaranted : Boolean ;
  begin
    Result := -1 ;

    if IsReadOnly then exit ;

    quaranted := False ;
    macroUpdateStart ;
    try
      uid := -1 ;

      iloop := T_SQLN.RETRY_COUNT ;
      while iloop > 0 do begin
        try
          if uid < 0 then uid := macroUidNew( quaranted )
                     else inc( uid ) ;

          if iloop = T_SQLN.RETRY_COUNT then
            sqlTableAppend( 0, TableGeometry ) ;
          macroTableSetField( 0, getCmd( T_SQLN.ID_UID ) , uid   ) ;

          macroTableSetField( 0, getCmd( T_SQLN.ID_XMIN     ), GisNoWorld.XMin  ) ;
          macroTableSetField( 0, getCmd( T_SQLN.ID_XMAX     ), GisNoWorld.XMax  ) ;
          macroTableSetField( 0, getCmd( T_SQLN.ID_YMIN     ), GisNoWorld.YMin  ) ;
          macroTableSetField( 0, getCmd( T_SQLN.ID_YMAX     ), GisNoWorld.YMax  ) ;
          macroTableSetField( 0, getCmd( T_SQLN.ID_SHAPETYPE), 0 ) ;

          {$IFDEF JAVA}
            // fix to insert null into bytea(blob) in postgres
            oGisDb.sqlTableSetGeometry( 0, getCmdGEOMETRY, NullVar, nil );
          {$ENDIF}

          sqlTablePost( 0 ) ;
          iloop := 0 ;
        except
          if not quaranted then begin
            dec( iloop ) ;
            if iloop <= 0 then raise ;
            Sleep( GetRandom( T_SQLN.RETRY_INTERVAL ) ) ;
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

  function TGIS_LayerSqlAbstract.macroUidNew(
    var _guaranted : Boolean
  ) : TGIS_Uid ;
  begin
    _guaranted := False ;
    Result := macroUidLast + 1 ;
  end ;

  function TGIS_LayerSqlAbstract.macroFetchRecord(
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
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN_UID ),
                            [ getSelectColumns, ViewFeatures, TableGeometry,
                              _uid
                            ]
                          ), _cursor
                  ) ;
    end ;

    Result := fetch ;
  end ;

  procedure TGIS_LayerSqlAbstract.parseConfigLayerName ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( FCurrTable, ';', ' ' ) ;
      if ( ( tkn.Result.Count = 0 ) or IsStringEmpty( tkn.Result.Strings[ 0 ] ) ) then
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

      if IsStringEmpty( strSchema ) then
        strSchema := 'public' ;

      if IsStringEmpty( strCatalog ) or IsStringEmpty( strSchema ) then begin
        oGisDb.sqlQueryOpen( getCmd( T_SQLN.ID_SELECT_DB_AND_CURRENT_USER_NAME ), 0 ) ;
        try
          if IsStringEmpty( strCatalog ) then
            strCatalog := UpperCase( VarToString( oGisDb.sqlQueryGetFieldById(0, 0) ) ) ;

          if IsStringEmpty( strSchema ) then
            strSchema := UpperCase( VarToString( oGisDb.sqlQueryGetFieldById(1, 0) ) ) ;
        finally
          oGisDb.sqlQueryClose(0) ;
        end ;
      end ;
    finally
      if not IsStringEmpty( strName ) then
        Table := strName ;
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.updateDialectList(
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

  function TGIS_LayerSqlAbstract.macroMaxNameLength : Integer ;
  begin
    try
      Result := StrToInt( getCmd( T_SQLN.ID_MAX_NAME_LENGTH ) ) ;
    except
      Result := 16 ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.getFieldInternal(
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

  procedure TGIS_LayerSqlAbstract.setUp ;
  var
    ext   : TGIS_Extent ;
    vtype : Integer     ;
    v     : Variant     ;
    dim   : TGIS_DimensionType ;
  begin
    FUseRTree := False ;

    inherited ;

    macroConnect ;

    MultiUserMode := oGisDb.MultiUserMode ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_MASTER ), [Table] ), 0 ) ;
    if oGisDb.sqlQueryEof(0) then exit ;

    try
      vtype := VarToInt32( oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_SHAPETYPE ), 0 ) ) ;

      checkShapeType( vtype, layerstp, dim ) ;
      DefaultShapeType := layerstp ;
      DefaultDimension := dim ;
    except
      DefaultShapeType := TGIS_ShapeType.Unknown ;
    end ;

    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    try
      ext.XMin := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_XMIN ),0 ) ) ;
      ext.XMax := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_XMAX ),0 ) ) ;
      ext.YMin := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_YMIN ),0 ) ) ;
      ext.YMax := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLN.ID_YMAX ),0 ) ) ;
      Extent := ext ;
    except
      Extent := GisNoWorld ;
    end ;

    v := oGisDb.sqlQueryGetField( getCmd ( T_SQLN.ID_SRTEXT ),0 ) ;
    if not( VarIsNull( v ) or VarIsEmpty( v ) ) then
      SetCSByWKT( VarToString( v ) ) 
    else
      SetCSByEPSG(0) ;

    oGisDb.sqlQueryClose(0) ;

    hasRtree := False ;
    if oGisDb.IsSqlite and not mobileVersion then begin
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_CHECK_RTREE ), [TableIndex] ),0 ) ;
      if not oGisDb.sqlQueryEof(0) then
        hasRtree := True ;
      oGisDb.sqlQueryClose(0) ;
    end ;

    oGisDb.GeometryType := TGIS_SubDataType.Native ;

    macroQueryStructure ;

    fixGEOUID := -1 ;

    FFileInfo := 'TatukGIS SQL Vector Coverage (TTKLS)' ;
    {$IFDEF JAVA}
      if (DefaultDimension = TGIS_DimensionType.XYZ ) or
         (DefaultDimension = TGIS_DimensionType.XYM ) or
         (DefaultDimension = TGIS_DimensionType.XYZM) then
    {$ELSE}
      if DefaultDimension in [TGIS_DimensionType.XYZ..TGIS_DimensionType.XYZM] then
    {$ENDIF}
      FFileInfo := FFileInfo + ' 3D';
  end ;

  procedure TGIS_LayerSqlAbstract.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
   ) ;
  var
    ll : TGIS_LayerSqlAbstract ;
  begin
    inherited ;

    if IsReadOnly then exit ;

    if not IsStringEmpty( _path ) then begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          ll := TGIS_LayerSqlAbstract(&Class.forName(Self.Class.Name).getConstructor().newInstance());
        {$ELSE}
          ll := TGIS_LayerSqlAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
        {$ENDIF}
      {$ELSE}
        ll := TGIS_LayerSqlAbstractClass( Self.ClassType ).Create ;
      {$ENDIF}
      try
        ll.FSQLParameters.Assign( FSQLParameters );
        ll.Path := _path ;
        ll.CS := CS ;
        ll.PasswordEvent := PasswordEvent ;
        ll.oGisDb.SQLExecuteEvent := oGisDb.SQLExecuteEvent ;
        macroSetSharedConnection( ll ) ;
        ll.macroConnect ;
        try
          try
            ll.macroMasterCreate ;
          except
            // master can already exist
          end ;
          try
            ll.macroMasterLogCreate ;
          except
            // master log can already exist
          end ;
          try
            ll.macroMasterStyleCreate ;
          except
            // master style can already exist
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
          macroMasterLogCreate ;
        except
          // master can already exist
        end ;
        try
          macroMasterStyleCreate ;
        except
          // master can already exist
        end ;
        macroTableCreate( _extent, _type, _dim  )  ;
      finally
        macroDisconnect ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.ImportLayerEx(
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
    shp_type := _type ;
    first    := True  ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;

    old_view := FViewFeatures ;
    try
      try
        macroDisconnect ;
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
        lname := StrToInt( getCmd( T_SQLN.ID_MAX_NAME_LENGTH ) ) ;
      except
        lname := 16 ;
      end ;

      PrepareExportFieldNames( lname, False, False ) ;
      macroTableAlter( Self ) ;
      ExportStructureToFLD ;

      macroDisableLogging ;
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
                     macroShapeUpdate( shp_tmp, True, shp_dim ) ;
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
        macroMasterStyleUpdate( _layer ) ;

        macroUpdateEnd ;
        macroEndBatchMode ;
        macroEnableLogging ;
        macroBuildRtree ;
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

  procedure TGIS_LayerSqlAbstract.MergeLayerEx(
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
    abort   := False ;

    old_view := FViewFeatures ;

    macroDisconnect ;
    macroConnect ;

    end_uid  := _layer.GetLastUid ;
    shp_dim  := DefaultDimension ;
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
        lname := StrToInt( getCmd( T_SQLN.ID_MAX_NAME_LENGTH ) ) ;
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
                     Extent   := _TGIS_Extent( shp_tmp.Extent ) ;
                     first    := False ;
                   end
                   else
                     Extent := GisMaxExtent( Extent, shp_tmp.Extent ) ;
                  // insert record
                   macroShapeUpdate( shp_tmp, True, shp_dim ) ;
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

  procedure TGIS_LayerSqlAbstract.ReStructure ;
  begin
    macroQueryStructure ;
  end ;

  procedure TGIS_LayerSqlAbstract.RevertShapes ;
  var
    i : Integer ;
  begin
    inherited ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorSql[i].currShape := nil ;
  end ;

  function TGIS_LayerSqlAbstract.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 1 ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited;
    oGisDb.sqlQueryClose(0);
    oGisDb.sqlTableClose(0) ;
    oGisDb.sqlTableClose(1) ;
  end ;

  procedure TGIS_LayerSqlAbstract.EnableLogging ;
  begin
    macroEnableLogging ;
  end ;

  procedure TGIS_LayerSqlAbstract.DisableLogging ;
  begin
    macroDisableLogging ;
  end ;

  function  TGIS_LayerSqlAbstract.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorSql)  then
        SetLength( cursorSql, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorSql[Result] ) then
          cursorSql[Result] := new T_cursorSql_LayerSql ;
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
      cursorSql[Result].currMultiPatch := TGIS_ShapeMultiPatch.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorSql[Result].disableJoinPrimary := False ;

      if Result > BUILTIN_CURSORS - 1 then
        oGisDb.cursorOpen( Result ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.cursorClose(
    const _cursor      : Integer
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
      FreeObject( cursorSql[_cursor].currMultiPatch ) ;

      // truncate cursorState at the tail;
      for i := length( cursorSql ) - 1 downto 0 do begin
        if not cursorSql[i].curInUse then begin
          SetLength( cursorSql, i ) ;
        end
        else
          break ;
      end ;

      if ( _cursor > BUILTIN_CURSORS - 1 ) and assigned( oGisDb ) then
        oGisDb.cursorClose( _cursor );

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlAbstract.cursorFirst(
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
    pixelfilter : String ;
    psize       : Double ;
    ssize       : Integer ;
    selCols     : String ;

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

        selCols := getSelectColumns ;

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

        if assigned( Viewer ) and Viewer.Ref.InPaint then begin
          ssize := Min( Viewer.Ref.TwipsToPixels( Params.Line.SmartSize ),
                        Viewer.Ref.TwipsToPixels( Params.Area.SmartSize )
                       ) ;
          psize := ssize / ( findSmartSizeFactor * Viewer.Ref.Zoom ) ;
          if ( psize > 0 ) and
             ( layerstp <> TGIS_ShapeType.Point   ) and
             ( layerstp <> TGIS_ShapeType.Unknown ) then
            pixelfilter := Format( '((%s-%s)>%s OR (%s-%s)>%s)',
                                  [ getCmd( T_SQLN.ID_XMAX ), getCmd( T_SQLN.ID_XMIN ),
                                    DotFloatToStr( psize ),
                                    getCmd( T_SQLN.ID_YMAX ), getCmd( T_SQLN.ID_YMIN ),
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

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN_NOEXT ) + order,
                                  [ selCols,ViewFeatures, TableGeometry,
                                    mf('',pixelfilter,' AND ') ]
                                ), _cursor
                        )
          else
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN_NOEXT_EX )+ order,
                                  [ selCols,ViewFeatures, TableGeometry, sqlquery,
                                    mf(' AND ',pixelfilter,'') ]
                                ), _cursor
                        ) ;
        end
        else begin
          ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                                 GisExtent( -1E37, -1E37, 1E37, 1E37 )
                               ) ;
          if hasRtree then begin
            if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
              oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN_RTREE ) + order,
                                    [ selCols,ViewFeatures, TableGeometry, TableIndex,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax ),
                                      mf(' AND ',pixelfilter,'')
                                    ]
                                  ) , _cursor
                          )
            else
              oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN_RTREE_EX ) + order,
                                    [ selCols,ViewFeatures, TableGeometry,
                                      sqlquery, TableIndex,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax ),
                                      mf(' AND ',pixelfilter,'')
                                    ]
                                  ), _cursor
                          ) ;
          end
          else begin
            if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then
              oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN ) + order,
                                    [ selCols,ViewFeatures, TableGeometry,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax ),
                                      mf(' AND ',pixelfilter,'')
                                    ]
                                  ) , _cursor
                          )
            else
              oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_JOIN_EX ) + order,
                                    [ selCols,ViewFeatures, TableGeometry,
                                      sqlquery,
                                      DotFloatToStr( ex.XMin ),
                                      DotFloatToStr( ex.XMax ),
                                      DotFloatToStr( ex.YMin ),
                                      DotFloatToStr( ex.YMax ),
                                      mf(' AND ',pixelfilter,'')
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

  procedure TGIS_LayerSqlAbstract.cursorNext(
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

  function TGIS_LayerSqlAbstract.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorSql[_cursor].isEof ;
  end ;

  function TGIS_LayerSqlAbstract.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorSql[_cursor].currShape ) then
       Result := cursorSql[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerSqlAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    lname : String  ;
    ltype : Integer ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    try
      macroConnect ;
      try
        try
          oGisDb.sqlQueryOpen( getCmd( T_SQLN.ID_SELECT_MASTER_ALL ), 0 ) ;
          while not oGisDb.sqlQueryEof(0) do begin
            lname := VarToString( oGisDb.sqlQueryGetFieldById( 0, 0 ) ) ;
            ltype := VarToInt32(  oGisDb.sqlQueryGetFieldById( 1, 0 ) ) ;

            Result.Add(
              TGIS_LayerInfo.Create( lname,
                                     TGIS_RegisteredLayerType.Vector,
                                     TGIS_ShapeType( ltype )
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

  function TGIS_LayerSqlAbstract.GetShape(
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
         ( cursorShape( _cursor ).Uid = _uid ) then
      begin
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
                     Format( getCmd( T_SQLN.ID_FILTER_UID_WEAK ),
                             [ GIS_FIELD_UID, _uid,
                               GIS_FIELD_UID, _uid + T_SQLN.GETSHAPE_FETCHED_LIMIT
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

  function TGIS_LayerSqlAbstract.GetLastUid : TGIS_Uid ;
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
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlAbstract.GetNewUid : TGIS_Uid ;
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

  procedure TGIS_LayerSqlAbstract.SaveData  ;
  var
    i        : Integer    ;
    shp      : TGIS_Shape ;
    first    : Boolean    ;
    shape_no : Cardinal   ;
    end_uid  : Integer    ;
    abort    : Boolean    ;
    done     : Boolean    ;
    shp_dim  : TGIS_DimensionType ;
  begin
    SaveFieldRules ;

    if IsReadOnly then exit ;

    iCodePage     := CodePage ;
    iJoinCodePage := JoinCodePage ;

    shape_no := 0 ;
    end_uid  := Items.Count ;
    shp_dim  := DefaultDimension ;
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
                 Extent := _TGIS_Extent( shp.Extent) ;
               first  := False ;
             end ;

             Extent := GisMaxExtent( Extent, shp.Extent ) ;

          macroShapeUpdate( shp, False, shp_dim ) ;

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
         macroMasterStyleUpdate( Self ) ;
        end ;
        macroUpdateEnd ;
        macroBuildRtree ;

        macroQueryStructure ;
        FIsModified := False ;

        for i := 0 to BUILTIN_CURSORS - 1 do begin
          prepareCandidates(i);
          cursorSql[i].currShape := nil ;
          // close active queries to unlock the db
          if oGisDb.IsSqlite or oGisDb.IsPostgreSql then
            oGisDb.sqlQueryClose(i) ;
        end ;
      end ;

    finally
      RaiseBusyRelease( Self ) ;
    end ;
    inherited ;
  end ;

  procedure TGIS_LayerSqlAbstract.readShape(
    const _cursor : Integer
  ) ;
  var
    uid       : TGIS_Uid      ;
    shapetype : TGIS_ShapeType ;
    vtype     : Integer    ;
    ptg       : TGIS_Point ;
    dim       : TGIS_DimensionType ;
  begin
    uid   := VarToInt64( sqlQueryGetGEOUID( _cursor ) );
    vtype := VarToInt32( oGisDb.sqlQueryGetSHAPETYPE( getCmdSHAPETYPE, _cursor ) ) ;
    checkShapeType( vtype, shapetype, dim ) ;

    if ( shapetype = TGIS_ShapeType.Point         ) and
       ( DefaultDimension = TGIS_DimensionType.XY ) then begin
      // create geometry form extent
      {$IFDEF GIS_NORECORDS}
        ptg := new TGIS_Point ;
      {$ENDIF}
      ptg.X := VarToDouble( oGisDb.sqlQueryGetXMIN( getCmdXMIN, _cursor ) ) ;
      ptg.Y := VarToDouble( oGisDb.sqlQueryGetYMIN( getCmdYMIN, _cursor ) ) ;

      FreeObject( cursorSql[_cursor].currPoint ) ;
      cursorSql[_cursor].currPoint := TGIS_ShapePoint.Create(
        nil, nil, False, uid, self, dim
      ) ;
      cursorSql[_cursor].currPoint.Lock( TGIS_Lock.Internal ) ;
        cursorSql[_cursor].currPoint.Reset ;
        cursorSql[_cursor].currPoint.AddPart ;
        cursorSql[_cursor].currPoint.AddPoint( ptg ) ;
      cursorSql[_cursor].currPoint.Unlock ;
      cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
    end
    else
      sqlQueryGetGeometry( uid, shapetype, _cursor, dim ) ;

    cursorSql[_cursor].currShape := getEdited( cursorSql[_cursor].currShape ) ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_LayerSqlAbstract.GetLogs : TGIS_Strings;
  {$ELSE}
    function TGIS_LayerSqlAbstract.GetLogs : TStrings;
  {$ENDIF}
  var
    val : String  ;
  begin
    Result := TStringList.Create ;

    try
      macroConnect ;
      try
        Result.Add( 'SHAPE_UID;INSERTED;UPDATED;DELETED;ACTION_DATE;STATUS' ) ;
        try
          oGisDb.sqlQueryOpen( Format( getCmd( T_SQLN.ID_SELECT_TABLE_LOG ),
                                      [TableLog]
                                      ), 0
                              ) ;
          while not oGisDb.sqlQueryEof(0) do begin
            val := VarToString( oGisDb.sqlQueryGetFieldById(0, 0) ) + ';' +
                   VarToString( oGisDb.sqlQueryGetFieldById(1, 0) ) + ';' +
                   VarToString( oGisDb.sqlQueryGetFieldById(2, 0) ) + ';' +
                   VarToString( oGisDb.sqlQueryGetFieldById(3, 0) ) + ';' +
                   VarToString( oGisDb.sqlQueryGetFieldById(4, 0) ) + ';' +
                   VarToString( oGisDb.sqlQueryGetFieldById(5, 0) ) ;

            Result.Add( val ) ;
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
    end ;
  end ;

{==================================== END =====================================}
end.

