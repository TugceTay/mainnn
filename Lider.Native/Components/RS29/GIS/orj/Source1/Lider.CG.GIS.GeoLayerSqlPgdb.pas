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
  Encapsulation of a ESRI Personal Geodatabase SQL layer direct access.

  ESRI Personal Geodatabase SQL Layer support was created by Marcin Malinowski.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlPgdb ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlPgdb"'}
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
    System.Variants,

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
    T_cursorSql_LayerSqlPgdb nested in TGIS_LayerSqlPgdbAbstract = public {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse      : Boolean ;

        /// <summary>
        ///   Is first record?
        /// </summary>
        isFirst       : Boolean ;

        /// <summary>
        ///   Eof marker.
        /// </summary>
        isEof         : Boolean ;

        /// <summary>
        ///   If True the ordering by JoinPrimary code should be disabled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;

        /// <summary>
        ///   If True the join should be disabled upon MoveFirst.
        /// </summary>
        disableJoin : Boolean ;

        /// <summary>
        ///   If True, then attribute query should be done on user side.
        /// </summary>
        fullSearch    : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape     : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPoint     : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currArc       : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPolygon   : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultiPatch   : TGIS_ShapeMultiPatch ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint  ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of abstract ESRI Personal Geodatabase SQL layer.
  /// </summary>
  TGIS_LayerSqlPgdbAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                              class( TGIS_LayerVectorSqlAbstract )

    protected // properties internal values

        /// <summary>
        ///   GID handle.
        /// </summary>
        memGID          : Integer ;
    protected // other protected variables
     /// <summary>
     ///   Internal cursor structure.
     /// </summary>
    {$IFDEF OXYGENE}
      cursorSql : array of T_cursorSql_LayerSqlPgdb ;
    {$ELSE}
      cursorSql : array of record

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse      : Boolean ;

        /// <summary>
        ///   Is first record?
        /// </summary>
        isFirst       : Boolean ;

        /// <summary>
        ///   Eof marker.
        /// </summary>
        isEof         : Boolean ;

        /// <summary>
        ///   If True the ordering by JoinPrimary code should be disabled
        ///   upon MoveFirst.
        /// </summary>
        disableJoinPrimary : Boolean ;

        /// <summary>
        ///   If True the join should be disabled upon MoveFirst.
        /// </summary>
        disableJoin : Boolean ;

        /// <summary>
        ///   If True, then attribute query should be done on user side.
        /// </summary>
        fullSearch    : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape     : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPoint     : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currArc       : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currPolygon   : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultiPatch   : TGIS_ShapeMultiPatch ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster then full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint  ;
      end;
    {$ENDIF}

        /// <summary>
        ///   Has elevation (3D)?.
        /// </summary>
        hasElevation  : Boolean ;

        /// <summary>
        ///   Minimum border of Z dimension (elevation)
        /// </summary>
        ZLow          : Double ;

        /// <summary>
        ///   Maximum border of Z dimension (elevation)
        /// </summary>
        ZHigh         : Double ;

        /// <summary>
        ///   Has measure ?.
        /// </summary>
        hasMeasure    : Boolean ;

        /// <summary>
        ///   Minimum border of M dimension (measure)
        /// </summary>
        MLow          : Double ;

        /// <summary>
        ///   Maximum border of M dimension (measure)
        /// </summary>
        MHigh         : Double ;

        /// <summary>
        ///   Use index grid ?.
        /// </summary>
        useIndex      : Boolean ;

        /// <summary>
        ///   The x offset of index grid to use when transforming ground
        ///   coordinates to internal system coordinates.
        /// </summary>
        idxOriginX    : Double ;

        /// <summary>
        ///   The y offset of index grid to use when transforming ground
        ///   coordinates to internal system coordinates.
        /// </summary>
        idxOriginY    : Double ;

        /// <summary>
        ///   The size cell of index grid to use when transforming ground
        ///   coordinates to internal system coordinates.
        /// </summary>
        idxGridSize   : Double ;

        /// <summary>
        ///   The count of objects stored in layer.
        /// </summary>
        idxCount      : Integer ;

        /// <summary>
        ///   The x offset to use when transforming ground coordinates to
        ///   internal system coordinates.
        /// </summary>
        falseX        : Double ;

        /// <summary>
        ///   The y offset to use when transforming ground coordinates to
        ///   internal system coordinates.
        /// </summary>
        falseY        : Double ;

        /// <summary>
        ///   The z offset to use when transforming ground coordinates to
        ///   internal system coordinates.
        /// </summary>
        falseZ        : Double ;

        /// <summary>
        ///   The measure offset to use when transforming measure values to
        ///   internal system coordinates.
        /// </summary>
        falseM        : Double ;

        /// <summary>
        ///   The scale factor to apply when transforming ground (in XY plane)
        ///   coordinates to internal system coordinates.
        /// </summary>
        scaleXY       : Double ;

        /// <summary>
        ///   The scale factor to use when transforming z values to internal
        ///   system coordinates.
        /// </summary>
        scaleZ        : Double ;

        /// <summary>
        ///   The scale factor to use when transforming measure values to
        ///   internal system coordinates.
        /// </summary>
        scaleM        : Double ;

        /// <summary>
        ///   Geometry index column name.
        /// </summary>
        nameColumnUID : String ;

        /// <summary>
        ///   Geometry blob column name.
        /// </summary>
        nameColumnGeometry : String ;

        /// <summary>
        ///   Layer name.
        /// </summary>
        nameLayer     : String ;

        /// <summary>
        ///   Dataset name.
        /// </summary>
        nameDataset   : String ;

        /// <summary>
        ///   Index table name.
        /// </summary>
        nameTableIndex : String ;

        /// <summary>
        ///   Is it ArcGIS 10 version of PersonalGDB ?
        /// </summary>
        isNewVersion  : Boolean ;

        /// <summary>
        ///   The ID of the spatial reference system used for the coordinate
        ///   geometry
        /// </summary>
        srId          : Integer ;

        /// <summary>
        ///   UID name fixup (GEO.UID or UID) for various db drivers.
        /// </summary>
        fixGEOUID     : Integer ;

        /// <summary>
        ///   Recent calculated value of last uid.
        /// </summary>
        lastUid       : TGIS_Uid ;

    protected // properties access routine

      function  fget_TableMaster      : String ;
      function  fget_ViewFeatures     : String ; override;

    protected

      /// <summary>
      ///   Read a shape from a SQL query and recreate it.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure readShape           ( const _cursor : Integer
                                     );

      /// <inheritdoc/>
      procedure prepareCommandList  ; override;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <param name="_id">
      ///   if 0, then returned 'UID'; if 1, then returned GEO.UID
      /// </param>
      /// <returns>
      ///    uid column name
      /// </returns>
      function  getCmdGEOUID        ( const _id : Integer
                                    ) : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///    command
      /// </returns>
      function  getCmdSHAPETYPE     : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///    command
      /// </returns>
      function  getCmdXMIN          : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///    command
      /// </returns>
      function  getCmdYMIN          : String ;

      /// <summary>
      ///   Return a SQL command.
      /// </summary>
      /// <returns>
      ///    command
      /// </returns>
      function  getCmdGEOMETRY      : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <param name="_type">
      ///   table type
      /// </param>
      /// <returns>
      ///    command
      /// </returns>
      function  prepareAppendCommand( const _table : String ;
                                      const _type  : TGIS_ShapeType
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
      ///    command
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
      ///    command
      /// </returns>
      function  prepareSelectCommand( const _table  : String ;
                                      const _filter : String
                                    ) : String ;

      /// <summary>
      ///   Return a UID=_uid filter.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_uid">
      ///   uid value used to build filter
      /// </param>
      /// <returns>
      ///    command
      /// </returns>
      function  prepareFilterUid    ( const _table  : String ;
                                      const _uid    : TGIS_Uid
                                    ) : String ;
      /// <summary>
      ///   Get geometry value.
      /// </summary>
      /// <param name="_uid">
      ///   shape uid
      /// </param>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_cursor">
      ///   cursor index
      /// </param>
      procedure sqlQueryGetGeometry ( const _uid    : TGIS_Uid ;
                                      const _type   : TGIS_ShapeType ;
                                      const _cursor : Integer
                                    ) ; virtual; abstract;
      /// <summary>
      ///   Get uid column name.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor index
      /// </param>
      /// <returns>
      ///    name text
      /// </returns>
      function  sqlQueryNameGEOUID  ( const _cursor : Integer
                                     ) : String ; virtual; abstract;
      /// <summary>
      ///   Get uid value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor index
      /// </param>
      /// <returns>
      ///    column value
      /// </returns>
      function  sqlQueryGetGEOUID   ( const _cursor : Integer
                                     ) : OleVariant ; virtual; abstract;
      /// <summary>
      ///   Append new record to table.
      /// </summary>
      /// <param name="_id">
      ///   table id
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_type">
      ///   shape type
      /// </param>
      procedure sqlTableAppend      ( const _id     : Integer ;
                                      const _table  : String  ;
                                      const _type   : TGIS_ShapeType
                                    ) ; virtual; abstract;
      /// <summary>
      ///   Update a table record.
      /// </summary>
      /// <param name="_id">
      ///   table id
      /// </param>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_uidCol">
      ///   uid column name
      /// </param>
      /// <param name="_uidVal">
      ///   uid value
      /// </param>
      procedure sqlTableOpenWrite   ( const _id     : Integer ;
                                      const _table  : String  ;
                                      const _uidCol : String  ;
                                      const _uidVal : TGIS_Uid
                                    ) ; virtual; abstract;
      /// <summary>
      ///   Set geometry value.
      /// </summary>
      /// <param name="_id">
      ///   table id
      /// </param>
      /// <param name="_name">
      ///   column name
      /// </param>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      procedure sqlTableSetGeometry ( const _id     : Integer ;
                                      const _name   : String  ;
                                      const _shp    : TGIS_Shape
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
      procedure macroMasterUpdate   ( const _extent : TGIS_Extent    ;
                                      const _type   : TGIS_ShapeType ;
                                      const _name   : String ;
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
      procedure macroTableCreate    ( const _extent : TGIS_Extent    ;
                                      const _type   : TGIS_ShapeType
                                    ) ; virtual;

      /// <inheritdoc/>
      procedure macroTableAlter     ( const _layer  : TGIS_LayerVector
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
      procedure macroTableSetField  ( const _id     : Integer ;
                                      const _name   : String ;
                                      const _val    : OleVariant
                                    ) ; virtual;

      /// <summary>
      ///   Macro for deleting a shape from both: geometry and feature table.
      /// </summary>
      /// <param name="_uid">
      ///   shape uid value
      /// </param>
      procedure macroShapeDelete    ( const _uid  : TGIS_Uid
                                    ) ; virtual;

      /// <inheritdoc/>
      procedure macroShapeUpdate    ( const _shp    : TGIS_Shape ;
                                      const _import : Boolean
                                    ) ; override;

      /// <summary>
      ///   Macro for obtaining last assigned uid.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  macroUidLast        : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for obtaining new uid.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  macroUidNew         : TGIS_Uid ; virtual;

      /// <summary>
      ///   Macro for reserving new uid.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  macroUidReserve     : TGIS_Uid ; virtual;

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

      /// <summary>
      ///   Update single parameter on SQLDialectList.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter; if not exist, then will be created
      /// </param>
      /// <param name="_value">
      ///   value of the parameter; if empty, then parameter will be deleted
      /// </param>
      procedure updateDialectList   ( const _name   : String ;
                                      const _value  : String
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
      ///    True if record was fetched for synchronization
      /// </returns>
      function  macroFetchRecord      ( const _uid    : TGIS_Uid ;
                                        const _cursor : Integer
                                      ) : Boolean ; virtual;

      /// <summary>
      ///   Parse config layer name.
      /// </summary>
      procedure parseConfigLayerName ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      function  getFieldInternal  ( const _uid     : TGIS_Uid    ;
                                    const _name    : String ;
                                    const _cursor  : Integer
                                  ) : Variant ; override;

      /// <inheritdoc/>
      procedure setUp             ; override;

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
      // destructor

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create   ; override;

      // new layer builder

      /// <inheritdoc/>
      procedure Build      ( const _path      : String           ;
                             const _extent    : TGIS_Extent      ;
                             const _type      : TGIS_ShapeType   ;
                             const _dim       : TGIS_DimensionType
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

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

      // shape access function

      /// <inheritdoc/>
      function  GetShape    ( const _uid      : TGIS_Uid           ;
                              const _cursor   : Integer
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
      ///   Name of the master table.
      /// </summary>
      property TableMaster    : String read fget_TableMaster ;

      /// <summary>
      ///   Name of the dataset.
      /// </summary>
      property Dataset        : String read nameDataset ;
  end ;
  TGIS_LayerSqlPgdbClass = class of TGIS_LayerSqlPgdbAbstract ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoFieldRules,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoXmlDoc,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

type
  T_SQLPGDB = class
  public
  const
    // Number of retries for concurrent issues.
    PGDB_RETRY_COUNT    = 9 ;

    // Maximum time of interval between retries in concurrent mode.
    PGDB_RETRY_INTERVAL = 100  ;

    // Maximum number of items fetched in GetShape.
    PGDB_GETSHAPE_FETCHED_LIMIT = 500  ;

  ID_BEGIN = 0                                                         ;

  CREATE_GDB_ANNOSYMBOLS_TABLE =
    'CREATE TABLE GDB_AnnoSymbols'                                            +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'SYMBOL <#BLOB#>)'                                                         ;
  ID_CREATE_GDB_ANNOSYMBOLS_TABLE = ID_BEGIN                   ;

  CREATE_GDB_ATTRRULES_TABLE =
    'CREATE TABLE GDB_AttrRules'                                              +
    '(RuleID <#INTEGER#> NOT NULL,'                                           +
    'Subtype <#INTEGER#> NOT NULL,'                                           +
    'FieldName <#VARCHAR#>(255) NOT NULL,'                                    +
    'DomainName <#VARCHAR#>(255) NOT NULL)'                                    ;
  ID_CREATE_GDB_ATTRRULES_TABLE =
    ID_CREATE_GDB_ANNOSYMBOLS_TABLE + 1                                ;

  CREATE_GDB_CODEDDOMAINS_TABLE =
    'CREATE TABLE GDB_CodedDomains'                                           +
    '(DomainID <#INTEGER#> NOT NULL,'                                         +
    'CodedValues <#BLOB#> NOT NULL)'                                           ;
  ID_CREATE_GDB_CODEDDOMAINS_TABLE =
    ID_CREATE_GDB_ATTRRULES_TABLE + 1                                  ;

  CREATE_GDB_DATABASELOCKS_TABLE =
     'CREATE TABLE GDB_DatabaseLocks'                                         +
     '(LockID <#COUNTER#> NOT NULL,'                                          +
     'LockType <#INTEGER#> NOT NULL,'                                         +
     'UserName <#TEXT#> NOT NULL,'                                            +
     'MachineName <#TEXT#> NOT NULL)'                                          ;
  ID_CREATE_GDB_DATABASELOCKS_TABLE =
    ID_CREATE_GDB_CODEDDOMAINS_TABLE + 1                               ;

  CREATE_GDB_DEFAULTVALUES_TABLE =
     'CREATE TABLE GDB_DefaultValues'                                         +
     '(ClassID <#INTEGER#> NOT NULL,'                                         +
     'FieldName <#VARCHAR#>(255) NOT NULL,'                                   +
     'Subtype <#INTEGER#> NOT NULL,'                                          +
     'DefaultNumber <#DOUBLE#>,'                                              +
     'DefaultString <#VARCHAR#>(255))'                                         ;
  ID_CREATE_GDB_DEFAULTVALUES_TABLE =
    ID_CREATE_GDB_DATABASELOCKS_TABLE + 1                              ;

  CREATE_GDB_DOMAINS_TABLE =
    'CREATE TABLE GDB_Domains'                                                +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'Owner <#VARCHAR#>(255),'                                                 +
    'DomainName <#VARCHAR#>(255) NOT NULL,'                                   +
    'DomainType <#INTEGER#> NOT NULL,'                                        +
    'Description <#VARCHAR#>(255),'                                           +
    'FieldType <#INTEGER#> NOT NULL,'                                         +
    'MergePolicy <#INTEGER#> NOT NULL,'                                       +
    'SplitPolicy <#INTEGER#> NOT NULL)'                                        ;
  ID_CREATE_GDB_DOMAINS_TABLE =
    ID_CREATE_GDB_DEFAULTVALUES_TABLE + 1                              ;

  CREATE_GDB_EDGECONNRULES_TABLE =
    'CREATE TABLE GDB_EdgeConnRules'                                          +
    '(RuleID <#INTEGER#> NOT NULL,'                                           +
    'FromClassID <#INTEGER#> NOT NULL,'                                       +
    'FromSubtype <#INTEGER#> NOT NULL,'                                       +
    'ToClassID <#INTEGER#> NOT NULL,'                                         +
    'ToSubtype <#INTEGER#> NOT NULL,'                                         +
    'Junctions <#BLOB#> NOT NULL)'                                             ;
  ID_CREATE_GDB_EDGECONNRULES_TABLE =
    ID_CREATE_GDB_DOMAINS_TABLE + 1                                    ;

  CREATE_GDB_EXTENSIONS_TABLE =
    'CREATE TABLE GDB_Extensions'                                             +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'CLSID <#VARCHAR#>(255) NOT NULL)'                                         ;
  ID_CREATE_GDB_EXTENSIONS_TABLE =
    ID_CREATE_GDB_EDGECONNRULES_TABLE + 1                              ;

  CREATE_GDB_FEATURECLASSES_TABLE =
    'CREATE TABLE GDB_FeatureClasses'                                         +
    '(ObjectClassID <#INTEGER#> NOT NULL,'                                    +
    'FeatureType <#INTEGER#> NOT NULL,'                                       +
    'GeometryType <#INTEGER#> NOT NULL,'                                      +
    'ShapeField <#VARCHAR#>(255) NOT NULL,'                                   +
    'GeomNetworkID <#INTEGER#>,'                                              +
    'GraphID <#INTEGER#>)'                                                     ;
  ID_CREATE_GDB_FEATURECLASSES_TABLE =
    ID_CREATE_GDB_EXTENSIONS_TABLE + 1                                 ;

  CREATE_GDB_FEATUREDATASET_TABLE =
    'CREATE TABLE GDB_FeatureDataset'                                         +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'SRID <#INTEGER#> NOT NULL)'                                               ;
  ID_CREATE_GDB_FEATUREDATASET_TABLE =
    ID_CREATE_GDB_FEATURECLASSES_TABLE + 1                             ;

  CREATE_GDB_FIELDINFO_TABLE =
    'CREATE TABLE GDB_FieldInfo'                                              +
    '(ClassID <#INTEGER#> NOT NULL,'                                          +
    'FieldName <#VARCHAR#>(255) NOT NULL,'                                    +
    'AliasName <#VARCHAR#>(255),'                                             +
    'ModelName <#VARCHAR#>(255),'                                             +
    'DefaultDomainName <#VARCHAR#>(255),'                                     +
    'DefaultValueString <#VARCHAR#>(255),'                                    +
    'DefaultValueNumber <#DOUBLE#>,'                                          +
    'IsRequired <#INTEGER#>,'                                                 +
    'IsSubtypeFixed <#INTEGER#>,'                                             +
    'IsEditable <#INTEGER#>)'                                                 ;
  ID_CREATE_GDB_FIELDINFO_TABLE =
    ID_CREATE_GDB_FEATUREDATASET_TABLE + 1                            ;

  CREATE_GDB_GEOMCOLUMNS_TABLE =
    'CREATE TABLE GDB_GeomColumns'                                            +
    '(TableName <#VARCHAR#>(255) NOT NULL,'                                   +
    'FieldName <#VARCHAR#>(255) NOT NULL,'                                    +
    'ShapeType <#INTEGER#> NOT NULL,'                                         +
    'ExtentLeft <#DOUBLE#>,'                                                  +
    'ExtentBottom <#DOUBLE#>,'                                                +
    'ExtentRight <#DOUBLE#>,'                                                 +
    'ExtentTop <#DOUBLE#>,'                                                   +
    'IdxOriginX <#DOUBLE#>,'                                                  +
    'IdxOriginY <#DOUBLE#>,'                                                  +
    'IdxGridSize <#DOUBLE#>,'                                                 +
    'SRID <#INTEGER#> NOT NULL,'                                              +
    'HasZ <#BIT#> NOT NULL,'                                                  +
    'HasM <#BIT#> NOT NULL,'                                                  +
    'ZLow <#DOUBLE#>,'                                                        +
    'ZHigh <#DOUBLE#>,'                                                       +
    'MLow <#DOUBLE#>,'                                                        +
    'MHigh <#DOUBLE#>)'                                                       ;
  ID_CREATE_GDB_GEOMCOLUMNS_TABLE =
    ID_CREATE_GDB_FIELDINFO_TABLE + 1                                 ;

  CREATE_GDB_JNCONNRULES_TABLE =
    'CREATE TABLE GDB_JnConnRules'                                            +
    '(RuleID <#INTEGER#> NOT NULL,'                                           +
    'EdgeClassID <#INTEGER#> NOT NULL,'                                       +
    'EdgeSubtype <#INTEGER#> NOT NULL,'                                       +
    'JunctionClassID <#INTEGER#> NOT NULL,'                                   +
    'JunctionSubtype <#INTEGER#> NOT NULL,'                                   +
    'EdgeMinCard <#INTEGER#> NOT NULL,'                                       +
    'EdgeMaxCard <#INTEGER#> NOT NULL,'                                       +
    'JunctionMinCard <#INTEGER#> NOT NULL,'                                   +
    'JunctionMaxCard <#INTEGER#> NOT NULL,'                                   +
    'IsDefault <#INTEGER#>)'                                                  ;
  ID_CREATE_GDB_JNCONNRULES_TABLE =
    ID_CREATE_GDB_GEOMCOLUMNS_TABLE + 1                               ;

  CREATE_GDB_NETDATASETS_TABLE =
    'CREATE TABLE GDB_NetDatasets'                                            +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'DatasetID <#INTEGER#> NOT NULL,'                                         +
    'Properties <#BLOB#>)'                                                     ;
  ID_CREATE_GDB_NETDATASETS_TABLE =
    ID_CREATE_GDB_JNCONNRULES_TABLE + 1                                ;

  CREATE_GDB_OBJECTCLASSES_TABLE =
    'CREATE TABLE GDB_ObjectClasses'                                          +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'AliasName <#VARCHAR#>(255),'                                             +
    'ModelName <#VARCHAR#>(255),'                                             +
    'CLSID <#VARCHAR#>(255) NOT NULL,'                                        +
    'EXTCLSID <#VARCHAR#>(255) NULL,'                                         +
    'EXTPROPS <#BLOB#>,'                                                      +
    'DatasetID <#INTEGER#>,'                                                  +
    'SubtypeField <#VARCHAR#>(255))'                                           ;
  ID_CREATE_GDB_OBJECTCLASSES_TABLE =
    ID_CREATE_GDB_NETDATASETS_TABLE + 1                                ;

  CREATE_GDB_RANGEDOMAINS_TABLE =
    'CREATE TABLE GDB_RangeDomains'                                           +
    '(DomainID <#INTEGER#> NOT NULL,'                                         +
    'MinValue <#DOUBLE#> NOT NULL,'                                           +
    'MaxValue <#DOUBLE#> NOT NULL)'                                            ;
  ID_CREATE_GDB_RANGEDOMAINS_TABLE =
    ID_CREATE_GDB_OBJECTCLASSES_TABLE + 1                              ;

  CREATE_GDB_RASTERCATALOGS_TABLE =
    'CREATE TABLE GDB_RasterCatalogs'                                         +
    '(ObjectClassID <#INTEGER#> NOT NULL,'                                    +
    'RasterField <#VARCHAR#>(255),'                                           +
    'IsRasterDataset <#INTEGER#> NOT NULL)'                                    ;
  ID_CREATE_GDB_RASTERCATALOGS_TABLE =
    ID_CREATE_GDB_RANGEDOMAINS_TABLE + 1                               ;

  CREATE_GDB_RELCLASSES_TABLE =
    'CREATE TABLE GDB_RelClasses'                                             +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'OriginClassID <#INTEGER#> NOT NULL,'                                     +
    'DestClassID <#INTEGER#> NOT NULL,'                                       +
    'ForwardLabel <#VARCHAR#>(255),'                                          +
    'BackwardLabel <#VARCHAR#>(255),'                                         +
    'Cardinality <#INTEGER#> NOT NULL,'                                       +
    'Notification <#INTEGER#> NOT NULL,'                                      +
    'IsComposite <#INTEGER#> NOT NULL,'                                       +
    'IsAttributed <#INTEGER#> NOT NULL,'                                      +
    'OriginPrimaryKey <#VARCHAR#>(255) NOT NULL,'                             +
    'DestPrimaryKey <#VARCHAR#>(255) NOT NULL,'                               +
    'OriginForeignKey <#VARCHAR#>(255) NOT NULL,'                             +
    'DestForeignKey <#VARCHAR#>(255) NOT NULL,'                               +
    'DatasetID <#INTEGER#>)'                                                   ;
  ID_CREATE_GDB_RELCLASSES_TABLE =
    ID_CREATE_GDB_RASTERCATALOGS_TABLE + 1                             ;

  CREATE_GDB_RELRULES_TABLE =
    'CREATE TABLE GDB_RelRules'                                               +
    '(RuleID <#INTEGER#> NOT NULL,'                                           +
    'OriginSubtype <#INTEGER#> NOT NULL,'                                     +
    'OriginMinCard <#INTEGER#> NOT NULL,'                                     +
    'OriginMaxCard <#INTEGER#> NOT NULL,'                                     +
    'DestSubtype <#INTEGER#> NOT NULL,'                                       +
    'DestMinCard <#INTEGER#> NOT NULL,'                                       +
    'DestMaxCard <#INTEGER#> NOT NULL)'                                        ;
  ID_CREATE_GDB_RELRULES_TABLE =
    ID_CREATE_GDB_RELCLASSES_TABLE + 1                                 ;

  CREATE_GDB_RELEASEINFO_TABLE =
    'CREATE TABLE GDB_ReleaseInfo'                                            +
    '(Major <#INTEGER#> NOT NULL,'                                            +
    'Minor <#INTEGER#> NOT NULL,'                                             +
    'Bugfix <#INTEGER#> NOT NULL)'                                             ;
  ID_CREATE_GDB_RELEASEINFO_TABLE =
    ID_CREATE_GDB_RELRULES_TABLE + 1                                   ;

  CREATE_GDB_REPLICADATASET_TABLE =
    'CREATE TABLE GDB_ReplicaDatasets'                                        +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'ReplicaID <#INTEGER#> NOT NULL,'                                         +
    'DatasetType <#INTEGER#> NOT NULL,'                                       +
    'DatasetID <#INTEGER#> NOT NULL,'                                         +
    'ParentOwner <#VARCHAR#>(255) NOT NULL,'                                  +
    'ParentDB <#VARCHAR#>(255))'                                               ;
  ID_CREATE_GDB_REPLICADATASET_TABLE =
    ID_CREATE_GDB_RELEASEINFO_TABLE + 1                                ;

  CREATE_GDB_REPLICAS_TABLE =
    'CREATE TABLE GDB_Replicas'                                               +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Version <#VARCHAR#>(255)NOT NULL,'                                       +
    'ParentID <#INTEGER#> NOT NULL,'                                          +
    'RepDate <#SMALLDATETIME#> NOT NULL,'                                     +
    'DefQuery <#BLOB#> NOT NULL,'                                             +
    'RepGuid <#VARCHAR#>(255) NOT NULL,'                                      +
    'RepCInfo <#VARCHAR#>(255) NOT NULL,'                                     +
    'Role <#INTEGER#> NOT NULL)'                                               ;
  ID_CREATE_GDB_REPLICAS_TABLE =
    ID_CREATE_GDB_REPLICADATASET_TABLE + 1                             ;

  CREATE_GDB_SPATIALREFS_TABLE =
    'CREATE TABLE GDB_SpatialRefs'                                            +
    '(SRID <#COUNTER#> NOT NULL,'                                             +
    'SRTEXT <#TEXT#> NOT NULL,'                                               +
    'FalseX <#DOUBLE#>,'                                                      +
    'FalseY <#DOUBLE#>,'                                                      +
    'XYUnits <#DOUBLE#>,'                                                     +
    'FalseZ <#DOUBLE#>,'                                                      +
    'ZUnits <#DOUBLE#>,'                                                      +
    'FalseM <#DOUBLE#>,'                                                      +
    'MUnits <#DOUBLE#>)'                                                       ;
  ID_CREATE_GDB_SPATIALREFS_TABLE =
    ID_CREATE_GDB_REPLICAS_TABLE + 1                                   ;

  CREATE_GDB_STRINGDOMAINS_TABLE =
    'CREATE TABLE GDB_StringDomains'                                          +
    '(DomainID <#INTEGER#> NOT NULL,'                                         +
    'Format <#VARCHAR#>(255) NOT NULL)'                                        ;
  ID_CREATE_GDB_STRINGDOMAINS_TABLE =
    ID_CREATE_GDB_SPATIALREFS_TABLE + 1                                ;

  CREATE_GDB_SUBTYPES_TABLE =
    'CREATE TABLE GDB_Subtypes'                                               +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'ClassID <#INTEGER#> NOT NULL,'                                           +
    'SubtypeCode <#INTEGER#> NOT NULL,'                                       +
    'SubtypeName <#VARCHAR#>(255) NOT NULL)'                                   ;
  ID_CREATE_GDB_SUBTYPES_TABLE =
    ID_CREATE_GDB_STRINGDOMAINS_TABLE + 1                              ;

  CREATE_GDB_TOOLBOXES_TABLE =
    'CREATE TABLE GDB_Toolboxes'                                              +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'Alias <#VARCHAR#>(255),'                                                 +
    'HelpFile <#VARCHAR#>(255),'                                              +
    'HelpContext <#INTEGER#>)'                                                 ;
  ID_CREATE_GDB_TOOLBOXES_TABLE =
    ID_CREATE_GDB_SUBTYPES_TABLE + 1                                   ;

  CREATE_GDB_TOPOCLASSES_TABLE =
    'CREATE TABLE GDB_TopoClasses'                                            +
    '(ClassID <#INTEGER#> NOT NULL,'                                          +
    'TopologyID <#INTEGER#> NOT NULL,'                                        +
    'Weight <#DOUBLE#> NOT NULL,'                                             +
    'XYRank <#INTEGER#> NOT NULL,'                                            +
    'ZRank <#INTEGER#> NOT NULL,'                                             +
    'EventsOnAnalyze <#INTEGER#> NOT NULL)'                                    ;
  ID_CREATE_GDB_TOPOCLASSES_TABLE =
    ID_CREATE_GDB_TOOLBOXES_TABLE + 1                                  ;

  CREATE_GDB_TOPORULES_TABLE =
    'CREATE TABLE GDB_TopoRules'                                              +
    '(RuleID <#INTEGER#> NOT NULL,'                                           +
    'OriginClassID <#INTEGER#> NOT NULL,'                                     +
    'OriginSubtype <#INTEGER#> NOT NULL,'                                     +
    'AllOriginSubtypes <#INTEGER#> NOT NULL,'                                 +
    'DestClassID <#INTEGER#> NOT NULL,'                                       +
    'DestSubtype <#INTEGER#> NOT NULL,'                                       +
    'AllDestSubtypes <#INTEGER#> NOT NULL,'                                   +
    'TopologyRuleType <#INTEGER#> NOT NULL,'                                  +
    'Name <#VARCHAR#>(255),'                                                  +
    'RuleGUID <#VARCHAR#>(255) NOT NULL)'                                      ;
  ID_CREATE_GDB_TOPORULES_TABLE =
    ID_CREATE_GDB_TOPOCLASSES_TABLE + 1                                ;

  CREATE_GDB_TOPOLOGIES_TABLE =
    'CREATE TABLE GDB_Topologies'                                             +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'DatasetID <#INTEGER#> NOT NULL,'                                         +
    'Properties <#BLOB#>)'                                                     ;
  ID_CREATE_GDB_TOPOLOGIES_TABLE =
    ID_CREATE_GDB_TOPORULES_TABLE + 1                                  ;

  CREATE_GDB_USERMETADATA_TABLE =
    'CREATE TABLE GDB_UserMetadata'                                           +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'DatabaseName <#VARCHAR#>(255),'                                          +
    'Owner <#VARCHAR#>(255),'                                                 +
    'Name <#VARCHAR#>(255) NOT NULL,'                                         +
    'DatasetType <#INTEGER#> NOT NULL,'                                       +
    'Xml <#BLOB#> NOT NULL)'                                                   ;
  ID_CREATE_GDB_USERMETADATA_TABLE =
    ID_CREATE_GDB_TOPOLOGIES_TABLE + 1                                 ;

  CREATE_GDB_VALIDRULES_TABLE =
    'CREATE TABLE GDB_ValidRules'                                             +
    '(ID <#COUNTER#> NOT NULL,'                                               +
    'RuleType <#INTEGER#> NOT NULL,'                                          +
    'ClassID <#INTEGER#> NOT NULL,'                                           +
    'RuleCategory <#INTEGER#> NOT NULL,'                                      +
    'HelpString <#VARCHAR#>(255))'                                             ;
  ID_CREATE_GDB_VALIDRULES_TABLE =
    ID_CREATE_GDB_USERMETADATA_TABLE + 1                               ;

  CREATE_TABLE_GEOMETRY_BINARY_POINTS =
    'CREATE TABLE %s '                                                        +
    '(<#OBJECTID#> <#COUNTER#> NOT NULL,'                                     +
    '<#Shape#> <#BLOB#>,'                                                     +
    'CONSTRAINT FDO_OBJECTID UNIQUE (<#OBJECTID#>))'                           ;
  ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS =
    ID_CREATE_GDB_VALIDRULES_TABLE + 1                                 ;

  CREATE_TABLE_GEOMETRY_BINARY_ARCS =
    'CREATE TABLE %s '                                                        +
    '(<#OBJECTID#> <#COUNTER#> NOT NULL,'                                     +
    '<#Shape#> <#BLOB#>,'                                                     +
    '<#Shape_Length#> <#DOUBLE#>,'                                            +
    'CONSTRAINT FDO_OBJECTID UNIQUE (<#OBJECTID#>))'                           ;
  ID_CREATE_TABLE_GEOMETRY_BINARY_ARCS =
    ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS + 1                         ;

  CREATE_TABLE_GEOMETRY_BINARY_POLYGONS =
    'CREATE TABLE %s '                                                        +
    '(<#OBJECTID#> <#COUNTER#> NOT NULL,'                                     +
    '<#Shape#> <#BLOB#>,'                                                     +
    '<#Shape_Area#> <#DOUBLE#>,'                                              +
    '<#Shape_Length#> <#DOUBLE#>,'                                            +
    'CONSTRAINT FDO_OBJECTID UNIQUE (<#OBJECTID#>))'                           ;
  ID_CREATE_TABLE_GEOMETRY_BINARY_POLYGONS =
    ID_CREATE_TABLE_GEOMETRY_BINARY_ARCS + 1                           ;

  CREATE_TABLE_GEOMETRY_BINARY_INDEX =
    'CREATE TABLE %s '                                                        +
    '(<#IndexedObjectID#> <#INTEGER#> NOT NULL,'                              +
    '<#MinGX#> <#INTEGER#> NOT NULL,<#MinGY#> <#INTEGER#> NOT NULL,'          +
    '<#MaxGX#> <#INTEGER#> NOT NULL,<#MaxGY#> <#INTEGER#> NOT NULL)'           ;
  ID_CREATE_TABLE_GEOMETRY_BINARY_INDEX =
    ID_CREATE_TABLE_GEOMETRY_BINARY_POLYGONS + 1                       ;

  DROP_TABLE =
    'DROP TABLE <#IF_EXISTS#> %s'                                              ;
  ID_DROP_TABLE = ID_CREATE_TABLE_GEOMETRY_BINARY_INDEX + 1    ;

  SELECT_GDB_FEATURE_DATASET_TABLE =
    'SELECT * '                                                               +
    'FROM GDB_FeatureDataset '                                                +
    'WHERE <#NAME#>=''%s'''                                                    ;
  ID_SELECT_GDB_FEATURE_DATASET_TABLE = ID_DROP_TABLE + 1      ;

  SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE =
    'SELECT * '                                                               +
    'FROM GDB_FeatureClasses <#GFC#>,'                                        +
    ' GDB_ObjectClasses <#GOC#> '                                             +
    'WHERE <#NAME#>=''%s'''                                                   +
    ' AND <#GFC#>.<#OBJECTCLASSID#>=<#GOC#>.<#ID#>'                            ;
  ID_SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE =
    ID_SELECT_GDB_FEATURE_DATASET_TABLE + 1                            ;

  SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE =
    'SELECT * '                                                               +
    'FROM GDB_GeomColumns <#GGC#> '                                           +
    'LEFT JOIN GDB_SpatialRefs <#GSR#>'                                       +
    ' ON <#GGC#>.<#SRID#>=<#GSR#>.<#SRID#> '                                  +
    'WHERE <#GGC#>.TableName=''%s'''                                           ;
  ID_SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE =
    ID_SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE + 1                     ;

  SELECT_GDB_GEOM_COLUMNS =
    'SELECT <#GGC#>.TableName, <#GGC#>.ShapeType '                            +
    'FROM GDB_GeomColumns <#GGC#> '                                           +
    'LEFT JOIN GDB_SpatialRefs <#GSR#>'                                       +
    ' ON <#GGC#>.<#SRID#>=<#GSR#>.<#SRID#> '                                   ;
  ID_SELECT_GDB_GEOM_COLUMNS =
    ID_SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE + 1                       ;

  SELECT_GDB_FIELD_INFO_TABLE =
    'SELECT * '                                                               +
    'FROM GDB_ObjectClasses <#GOC#>,'                                         +
    ' GDB_FieldInfo <#GFI#> '                                                 +
    'WHERE <#NAME#>=''%s'''                                                   +
    ' AND <#GFI#>.<#CLASSID#>=<#GOC#>.<#ID#>'                                 +
    ' AND <#GFI#>.<#ISREQUIRED#>=1'                                           +
    ' AND <#GFI#>.<#ISEDITABLE#>=0'                                           +
    ' AND <#GFI#>.<#FIELDNAME#> NOT LIKE ''%s*'''                             +
    ' AND <#GFI#>.<#FIELDNAME#> NOT LIKE ''<#Shape_Area#>'''                  +
    ' AND <#GFI#>.<#FIELDNAME#> NOT LIKE ''<#Shape_Length#>'''                +
    ' AND <#GFI#>.<#CLASSID#>=<#GOC#>.<#ID#>'                                  ;
  ID_SELECT_GDB_FIELD_INFO_TABLE =
    ID_SELECT_GDB_GEOM_COLUMNS + 1                                     ;

  SELECT_MAX_GEOMETRY_OBJECTID =
    'SELECT <#MAX#>(<#OBJECTID#>) <#AS#> <#ID#> FROM %s'                       ;
  ID_SELECT_MAX_GEOMETRY_OBJECTID =
    ID_SELECT_GDB_FIELD_INFO_TABLE + 1                                 ;

  SELECT_MAX_GDB_SPATIALREFS_SRID =
    'SELECT <#MAX#>(<#SRID#>) <#AS#> <#ID#> FROM GDB_SPATIALREFS'              ;
  ID_SELECT_MAX_GDB_SPATIALREFS_SRID =
    ID_SELECT_MAX_GEOMETRY_OBJECTID + 1                                ;

  SELECT_COUNT_GEOMETRY_OBJECTID =
    'SELECT <#COUNT#>(<#OBJECTID#>) <#AS#> <#ID#> FROM %s'                     ;
  ID_SELECT_COUNT_GEOMETRY_OBJECTID =
    ID_SELECT_MAX_GDB_SPATIALREFS_SRID + 1                             ;

  SELECT_TABLE_ALL =
    'SELECT * FROM %s'                                                         ;
  ID_SELECT_TABLE_ALL =
    ID_SELECT_COUNT_GEOMETRY_OBJECTID + 1                              ;

  SELECT_TABLE_WHERE =
    'SELECT * FROM %s WHERE %s'                                                ;
  ID_SELECT_TABLE_WHERE = ID_SELECT_TABLE_ALL + 1              ;

  SELECT_JOIN_UID =
    'SELECT * FROM %s'                                                        +
    ' WHERE %s=%d'                                                             ;
  ID_SELECT_JOIN_UID = ID_SELECT_TABLE_WHERE + 1               ;

  SELECT_JOIN =
    'SELECT <#GEO#>.* FROM %s <#GEO#>, %s <#IDX#>'                            +
    ' WHERE'                                                                  +
    ' <#MAXGX#>>=%d AND <#MINGX#><=%d AND'                                    +
    ' <#MAXGY#>>=%d AND <#MINGY#><=%d'                                        +
    ' AND (<#GEO#>.<#OBJECTID#>=<#IDX#>.<#INDEXEDOBJECTID#>)'                 +
    ' ORDER BY <#GEO#>.<#OBJECTID#>'                                           ;
  ID_SELECT_JOIN = ID_SELECT_JOIN_UID + 1                      ;

  SELECT_JOIN_EX =
    'SELECT <#GEO#>.* FROM %s <#GEO#>, %s <#IDX#>'                            +
    ' WHERE'                                                                  +
    ' (%s) AND'                                                               +
    ' <#MAXGX#>>=%d AND <#MINGX#><=%d AND'                                    +
    ' <#MAXGY#>>=%d AND <#MINGY#><=%d'                                        +
    ' AND (<#GEO#>.<#OBJECTID#>=<#IDX#>.<#INDEXEDOBJECTID#>)'                 +
    ' ORDER BY %s'                                                             ;
  ID_SELECT_JOIN_EX = ID_SELECT_JOIN + 1                       ;

  SELECT_NO_JOIN =
    'SELECT * FROM %s <#GEO#>'                                                 +
    ' ORDER BY <#GEO#>.<#OBJECTID#>'                                           ;
  ID_SELECT_NO_JOIN = ID_SELECT_JOIN_EX + 1                    ;

  SELECT_JOIN_NOEXT =
    'SELECT <#GEO#>.* FROM %s <#GEO#>, %s <#IDX#>'                            +
    ' WHERE (<#GEO#>.<#OBJECTID#>=<#IDX#>.<#INDEXEDOBJECTID#>) '              +
    ' ORDER BY %s'                                                             ;
  ID_SELECT_JOIN_NOEXT = ID_SELECT_NO_JOIN + 1                 ;

  SELECT_JOIN_NOEXT_EX =
    'SELECT <#GEO#>.* FROM %s <#GEO#>, %s <#IDX#>'                            +
    ' WHERE (<#GEO#>.<#OBJECTID#>=<#IDX#>.<#INDEXEDOBJECTID#>) AND (%s) '      +
    ' ORDER BY %s'                                                             ;
  ID_SELECT_JOIN_NOEXT_EX = ID_SELECT_JOIN_NOEXT + 1           ;

  DELETE_SHAPE =
    'DELETE FROM %s WHERE %s=%d'                                               ;
  ID_DELETE_SHAPE = ID_SELECT_JOIN_NOEXT_EX + 1                ;

  DELETE_FROM =
    'DELETE FROM %s'                                                           ;
  ID_DELETE_FROM = ID_DELETE_SHAPE + 1                         ;

  DELETE_FIELD_FROM_GDB_FIELDINFO =
    'DELETE FROM GDB_FieldInfo'                                               +
    ' WHERE CLASSID in'                                                       +
    '(SELECT <#ID#> FROM GDB_OBJECTCLASSES'                                   +
    ' WHERE <#NAME#>=''%s'') AND <#FIELDNAME#>=''%s'''                         ;
  ID_DELETE_FIELD_FROM_GDB_FIELDINFO =
    ID_DELETE_FROM + 1                                                 ;

  DELETE_FEATURE_FROM_GDB_FEATURECLASSES =
    'DELETE FROM GDB_FeatureClasses'                                          +
    ' WHERE OBJECTCLASSID in'                                                 +
    '(SELECT <#ID#> FROM GDB_OBJECTCLASSES'                                   +
    ' WHERE <#NAME#>=''%s'')'                                                  ;
  ID_DELETE_FEATURE_FROM_GDB_FEATURECLASSES =
    ID_DELETE_FIELD_FROM_GDB_FIELDINFO + 1                             ;

  DELETE_FEATURE_FROM_GDB_FIELDINFO =
    'DELETE FROM GDB_FieldInfo'                                               +
    ' WHERE CLASSID in'                                                       +
    '(SELECT <#ID#> FROM GDB_OBJECTCLASSES'                                   +
    ' WHERE <#NAME#>=''%s'')'                                                  ;
  ID_DELETE_FEATURE_FROM_GDB_FIELDINFO =
    ID_DELETE_FEATURE_FROM_GDB_FEATURECLASSES + 1                      ;

  DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS =
    'DELETE FROM GDB_GeomColumns'                                             +
    ' WHERE TABLENAME=''%s'''                                                  ;
  ID_DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS =
    ID_DELETE_FEATURE_FROM_GDB_FIELDINFO + 1                           ;

  DELETE_FEATURE_FROM_GDB_ITEMS =
    'DELETE FROM GDB_Items'                                                   +
    ' WHERE NAME=''%s'''                                                       ;
  ID_DELETE_FEATURE_FROM_GDB_ITEMS =
    ID_DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS + 1                         ;

  DELETE_FEATURE_FROM_GDB_USERMETADATA =
    'DELETE FROM GDB_UserMetadata'                                            +
    ' WHERE NAME=''%s'''                                                       ;
  ID_DELETE_FEATURE_FROM_GDB_USERMETADATA =
    ID_DELETE_FEATURE_FROM_GDB_ITEMS + 1                               ;

  DELETE_FEATURE_FROM_GDB_OBJECTCLASSES =
    'DELETE FROM GDB_ObjectClasses'                                           +
    ' WHERE NAME=''%s'''                                                       ;
  ID_DELETE_FEATURE_FROM_GDB_OBJECTCLASSES =
    ID_DELETE_FEATURE_FROM_GDB_USERMETADATA + 1                        ;

  UPDATE_GDB_GEOMCOLUMNS_TABLE =
    'UPDATE GDB_GeomColumns'                                                  +
    ' SET ShapeType=%d,'                                                      +
    ' ExtentLeft=%s,ExtentBottom=%s,ExtentRight=%s,ExtentTop=%s,'             +
    ' IdxOriginX=%s,IdxOriginY=%s,IdxGridSize=%s '                            +
    'WHERE <#TABLENAME#>=''%s'''                                               ;
  ID_UPDATE_GDB_GEOMCOLUMNS_TABLE =
    ID_DELETE_FEATURE_FROM_GDB_OBJECTCLASSES + 1                       ;

  UPDATE_DBX_GEO_BINARY =
    '<#GEOMETRY#>=:<#GEOMETRY#>'                                               ;
  ID_UPDATE_DBX_GEO_BINARY =
    ID_UPDATE_GDB_GEOMCOLUMNS_TABLE + 1                                ;

  UPDATE_DBX =
    'UPDATE %s SET %s WHERE %s=:%s'                                            ;
  ID_UPDATE_DBX = ID_UPDATE_DBX_GEO_BINARY + 1                 ;

  INSERT_GDB_SPATIALREFS_TABLE =
    'INSERT INTO GDB_SpatialRefs'                                             +
    '(<#SRTEXT#>,<#FalseX#>,<#FalseY#>,<#XYUnits#>,'                          +
    '<#FalseZ#>,<#ZUnits#>,<#FalseM#>,<#MUnits#>)'                            +
    'VALUES(''%s'',%s,%s,%s,%s,%s,%s,%s)'                                      ;
  ID_INSERT_GDB_SPATIALREFS_TABLE =
    ID_UPDATE_DBX + 1                                                  ;

  INSERT_GDB_FEATUREDATASET_TABLE =
    'INSERT INTO GDB_FeatureDataset'                                          +
    '(Name,SRID)'                                                             +
    'VALUES(''%s'',%d)'                                                        ;
  ID_INSERT_GDB_FEATUREDATASET_TABLE =
    ID_INSERT_GDB_SPATIALREFS_TABLE + 1                                ;

  INSERT_GDB_OBJECTCLASSES_TABLE =
    'INSERT INTO GDB_ObjectClasses'                                           +
    '(Name,CLSID,DatasetID)'                                                  +
    'VALUES(''%s'',''%s'',%s)'                                                 ;
  ID_INSERT_GDB_OBJECTCLASSES_TABLE =
    ID_INSERT_GDB_FEATUREDATASET_TABLE + 1                             ;

  INSERT_GDB_OBJECTCLASSES_EX_TABLE =
    'INSERT INTO GDB_ObjectClasses'                                           +
    '(Name,CLSID,DatasetID)'                                                  +
    'SELECT ''%s'',''%s'',<#ID#>'                                             +
    ' FROM GDB_FeatureDataset'                                                +
    ' WHERE (<#NAME#>=''%s'')'                                                 ;
  ID_INSERT_GDB_OBJECTCLASSES_EX_TABLE =
    ID_INSERT_GDB_OBJECTCLASSES_TABLE + 1                              ;

  INSERT_GDB_FEATURECLASSES_TABLE =
    'INSERT INTO GDB_FeatureClasses'                                          +
    '(ObjectClassID,FeatureType,GeometryType,ShapeField)'                     +
    'SELECT <#ID#>,%d,%d,''%s'''                                              +
    ' FROM GDB_ObjectClasses'                                                 +
    ' WHERE (<#NAME#>=''%s'')'                                                 ;
  ID_INSERT_GDB_FEATURECLASSES_TABLE =
    ID_INSERT_GDB_OBJECTCLASSES_EX_TABLE + 1                           ;

  INSERT_GDB_GEOMCOLUMNS_TABLE_OLD =
    'INSERT INTO GDB_GeomColumns'                                             +
    ' VALUES(''%s'',''%s'',%d,%s,%s,%s,%s,%s,%s,%s,%d,%d,%d)'                  ;
  ID_INSERT_GDB_GEOMCOLUMNS_TABLE_OLD =
    ID_INSERT_GDB_FEATURECLASSES_TABLE + 1                             ;

  INSERT_GDB_GEOMCOLUMNS_TABLE =
    'INSERT INTO GDB_GeomColumns'                                             +
    ' VALUES(''%s'',''%s'',%d,%s,%s,%s,%s,%s,%s,%s,%d,%d,%d,%s,%s,%s,%s)'      ;
  ID_INSERT_GDB_GEOMCOLUMNS_TABLE =
    ID_INSERT_GDB_GEOMCOLUMNS_TABLE_OLD + 1                            ;

  INSERT_GDB_ITEMS_TABLE =
    'INSERT INTO GDB_Items'                                                   +
    '(UUID,Type,Name,PhysicalName,Path,DatasetSubtype1,DatasetSubtype2,'      +
    'DatasetInfo1,DatasetInfo2,URL,Definition,Documentation,ItemInfo,'        +
    'Properties,Defaults,Shape) VALUES('                                      +
    '''%s'',''%s'',''%s'',''%s'',''%s'',%d,%d,''%s'',%s,%s,%s,%s,%s,%d,%s,%s)' ;
  ID_INSERT_GDB_ITEMS_TABLE =
    ID_INSERT_GDB_GEOMCOLUMNS_TABLE + 1                                ;

  INSERT_GDB_FIELDINFO =
    'INSERT INTO GDB_FieldInfo'                                               +
    '(ClassID,FieldName,AliasName,ModelName,'                                 +
    'IsRequired,IsSubTypeFixed,IsEditable)'                                   +
    'SELECT <#ID#>,''%s'',''%s'',''%s'',%d,%d,%d'                             +
    ' FROM GDB_ObjectClasses'                                                 +
    ' WHERE (<#NAME#>=''%s'')'                                                 ;
  ID_INSERT_GDB_FIELDINFO =
    ID_INSERT_GDB_ITEMS_TABLE + 1                                      ;

  INSERT_DBX_GEO_POINTS_VALUE =
    '<#OBJECTID#>,<#GEOMETRY#>'                                                ;
    ID_INSERT_DBX_GEO_POINTS_VALUE =
      ID_INSERT_GDB_FIELDINFO + 1                                      ;

  INSERT_DBX_GEO_POINTS_PARAM =
    ':<#OBJECTID#>,:<#GEOMETRY#>'                                              ;
    ID_INSERT_DBX_GEO_POINTS_PARAM =
      ID_INSERT_DBX_GEO_POINTS_VALUE + 1                               ;

  INSERT_DBX_GEO_ARCS_VALUE =
    '<#OBJECTID#>,<#GEOMETRY#>,<#SHAPE_LENGTH#>'                               ;
    ID_INSERT_DBX_GEO_ARCS_VALUE =
      ID_INSERT_DBX_GEO_POINTS_PARAM + 1                               ;

  INSERT_DBX_GEO_ARCS_PARAM =
    ':<#OBJECTID#>,:<#GEOMETRY#>,:<#SHAPE_LENGTH#>'                            ;
    ID_INSERT_DBX_GEO_ARCS_PARAM =
      ID_INSERT_DBX_GEO_ARCS_VALUE + 1                                 ;

  INSERT_DBX_GEO_POLYGONS_VALUE =
    '<#OBJECTID#>,<#GEOMETRY#>,<#SHAPE_AREA#>,<#SHAPE_LENGTH#>'                ;
    ID_INSERT_DBX_GEO_POLYGONS_VALUE =
      ID_INSERT_DBX_GEO_ARCS_PARAM + 1                                 ;

  INSERT_DBX_GEO_POLYGONS_PARAM =
    ':<#OBJECTID#>,:<#GEOMETRY#>,:<#SHAPE_AREA#>,:<#SHAPE_LENGTH#>'            ;
    ID_INSERT_DBX_GEO_POLYGONS_PARAM =
      ID_INSERT_DBX_GEO_POLYGONS_VALUE + 1                             ;

  INSERT_DBX_GEO_INDEX_VALUE =
    '<#INDEXEDOBJECTID#>,<#MINGX#>,<#MINGY#>,<#MAXGX#>,<#MAXGY#>'              ;
    ID_INSERT_DBX_GEO_INDEX_VALUE =
      ID_INSERT_DBX_GEO_POLYGONS_PARAM + 1                             ;

  INSERT_DBX_GEO_INDEX_PARAM =
    ':<#INDEXEDOBJECTID#>,:<#MINGX#>,:<#MINGY#>,:<#MAXGX#>,:<#MAXGY#>'         ;
    ID_INSERT_DBX_GEO_INDEX_PARAM =
      ID_INSERT_DBX_GEO_INDEX_VALUE + 1                                ;

  INSERT_RELEASEINFO =
    'INSERT INTO GDB_ReleaseInfo ( Major,Minor,Bugfix ) VALUES ( 2,0,1 )'      ;
    ID_INSERT_RELEASEINFO =
      ID_INSERT_DBX_GEO_INDEX_PARAM + 1                                ;

  INSERT_DBX =
    'INSERT INTO %s ( %s ) VALUES ( %s )'                                      ;
    ID_INSERT_DBX =
      ID_INSERT_RELEASEINFO + 1                                        ;

  ALTER_DROP_COLUMN =
    'ALTER TABLE %s <#DROP COLUMN#> <#QUOTE LEFT#>%s<#QUOTE RIGHT#>'           ;
    ID_ALTER_DROP_COLUMN =
      ID_INSERT_DBX + 1                                                ;

  ALTER_ADD_STRING =
    'ALTER TABLE %s ADD %s <#VARCHAR#>(%d)'                                    ;
    ID_ALTER_ADD_STRING =
      ID_ALTER_DROP_COLUMN + 1                                         ;

  ALTER_ADD_MEMO =
    'ALTER TABLE %s ADD %s <#CLOB#>'                                           ;
    ID_ALTER_ADD_MEMO =
      ID_ALTER_ADD_STRING + 1                                          ;

  ALTER_ALTER_STRING =
    'ALTER TABLE %s <#ALTER#> %s <#VARCHAR_LEN#> <#VARCHAR#>(%d)'              ;
    ID_ALTER_ALTER_STRING = ID_ALTER_ADD_MEMO + 1              ;

  ALTER_ADD_DATE =
    'ALTER TABLE %s ADD %s <#DATE#>'                                           ;
    ID_ALTER_ADD_DATE = ID_ALTER_ALTER_STRING + 1              ;

  ALTER_ADD_BOOLEAN =
    'ALTER TABLE %s ADD %s <#LOGICAL#>'                                        ;
    ID_ALTER_ADD_BOOLEAN = ID_ALTER_ADD_DATE + 1               ;

  ALTER_ADD_INTEGER =
    'ALTER TABLE %s ADD %s <#INTEGER#>'                                        ;
    ID_ALTER_ADD_INTEGER = ID_ALTER_ADD_BOOLEAN + 1            ;

  ALTER_ADD_FLOAT =
    'ALTER TABLE %s ADD %s <#DOUBLE#>'                                         ;
    ID_ALTER_ADD_FLOAT = ID_ALTER_ADD_INTEGER + 1              ;

  POSTRESTRUCTURE =
    '<#POSTRESTRUCTURE#>'                                                      ;
    ID_POSTRESTRUCTURE = ID_ALTER_ADD_FLOAT + 1                ;

  FILTER_UID =
    '%s=%d'                                                                    ;
    ID_FILTER_UID = ID_POSTRESTRUCTURE + 1                     ;

  FILTER_UID_WEAK =
    '(%s>=%d) and (%s<=%d)'                                                    ;
    ID_FILTER_UID_WEAK = ID_FILTER_UID + 1                     ;

  MAX_NAMELENGTH =
    '<#MAX_NAMELENGTH#>'                                                       ;
    ID_MAX_NAMELENGTH = ID_FILTER_UID_WEAK + 1                 ;

  MAX_TEXTLENGTH =
    '<#MAX_TEXTLENGTH#>'                                                       ;
    ID_MAX_TEXTLENGTH = ID_MAX_NAMELENGTH + 1                  ;

  TABLE_MASTER =
    'GDB_FeatureClasses'                                                       ;
    ID_TABLE_MASTER = ID_MAX_TEXTLENGTH + 1                    ;

  MINGX =
    '<#MINGX#>'                                                                ;
    ID_MINGX = ID_TABLE_MASTER + 1                             ;

  MAXGX =
    '<#MAXGX#>'                                                                ;
    ID_MAXGX = ID_MINGX + 1                                    ;

  MINGY =
    '<#MINGY#>'                                                                ;
    ID_MINGY = ID_MAXGX + 1                                    ;

  MAXGY =
    '<#MAXGY#>'                                                                ;
    ID_MAXGY = ID_MINGY + 1                                    ;

  EXTENTLEFT =
    '<#EXTENTLEFT#>'                                                           ;
    ID_EXTENTLEFT = ID_MAXGY + 1                               ;

  EXTENTRIGHT =
    '<#EXTENTRIGHT#>'                                                          ;
    ID_EXTENTRIGHT = ID_EXTENTLEFT + 1                         ;

  EXTENTBOTTOM =
    '<#EXTENTBOTTOM#>'                                                         ;
    ID_EXTENTBOTTOM = ID_EXTENTRIGHT + 1                       ;

  EXTENTTOP =
    '<#EXTENTTOP#>'                                                            ;
    ID_EXTENTTOP = ID_EXTENTBOTTOM + 1                         ;

  IDXORIGINX =
    '<#IDXORIGINX#>'                                                           ;
    ID_IDXORIGINX = ID_EXTENTTOP + 1                           ;

  IDXORIGINY =
    '<#IDXORIGINY#>'                                                           ;
    ID_IDXORIGINY = ID_IDXORIGINX + 1                          ;

  IDXGRIDSIZE =
    '<#IDXGRIDSIZE#>'                                                          ;
    ID_IDXGRIDSIZE = ID_IDXORIGINY + 1                         ;

  SHAPETYPE =
    '<#SHAPETYPE#>'                                                            ;
  ID_SHAPETYPE = ID_IDXGRIDSIZE + 1                            ;

  SHAPE_FIELD =
    '<#SHAPEFIELD#>'                                                           ;
  ID_SHAPE_FIELD = ID_SHAPETYPE + 1                            ;

  FIELDNAME =
    '<#FIELDNAME#>'                                                            ;
    ID_FIELDNAME = ID_SHAPE_FIELD + 1                          ;

  OBJECTID =
    '<#OBJECTID#>'                                                             ;
    ID_OBJECTID = ID_FIELDNAME + 1                             ;

  INDEXEDOBJECTID =
    '<#INDEXEDOBJECTID#>'                                                      ;
    ID_INDEXEDOBJECTID = ID_OBJECTID + 1                       ;

  GEOMETRY =
    '<#GEOMETRY#>'                                                             ;
    ID_GEOMETRY = ID_INDEXEDOBJECTID + 1                       ;

  INDEX =
    '<#Index#>'                                                                ;
    ID_INDEX = ID_GEOMETRY + 1                                 ;

  ID =
    '<#ID#>'                                                                   ;
  ID_ID = ID_INDEX + 1                                         ;

  SHAPE =
    '<#Shape#>'                                                                ;
  ID_SHAPE = ID_ID + 1                                         ;

  SHAPE_LENGTH =
    '<#Shape_Length#>'                                                         ;
  ID_SHAPE_LENGTH = ID_SHAPE + 1                               ;

  SHAPE_AREA =
    '<#Shape_Area#>'                                                           ;
  ID_SHAPE_AREA = ID_SHAPE_LENGTH + 1                          ;

  GEO =
    '<#GEO#>'                                                                  ;
  ID_GEO = ID_SHAPE_AREA + 1                                   ;

  SRID =
    '<#SRID#>'                                                                 ;
  ID_SRID = ID_GEO + 1                                         ;

  GGC_SRID =
    '<#GGC#>.<#SRID#>'                                                         ;
  ID_GGC_SRID = ID_SRID + 1                                    ;

  SRTEXT =
    '<#SRTEXT#>'                                                               ;
  ID_SRTEXT = ID_GGC_SRID + 1                                  ;

  FALSEX =
    '<#FALSEX#>'                                                               ;
    ID_FALSEX = ID_SRTEXT + 1                                  ;

  FALSEY =
    '<#FALSEY#>'                                                               ;
    ID_FALSEY = ID_FALSEX + 1                                  ;

  FALSEZ =
    '<#FALSEZ#>'                                                               ;
    ID_FALSEZ = ID_FALSEY + 1                                  ;

  FALSEM =
    '<#FALSEM#>'                                                               ;
    ID_FALSEM = ID_FALSEZ + 1                                  ;

  XYUNITS =
    '<#XYUNITS#>'                                                              ;
    ID_XYUNITS = ID_FALSEM + 1                                 ;

  ZUNITS =
    '<#ZUNITS#>'                                                               ;
    ID_ZUNITS = ID_XYUNITS + 1                                 ;

  MUNITS =
    '<#MUNITS#>'                                                               ;
    ID_MUNITS = ID_ZUNITS + 1                                  ;

  CREATE_INDEX =
    'CREATE INDEX %s ON %s (%s)'                                               ;
  ID_CREATE_INDEX = ID_MUNITS + 1                              ;

  CREATE_INDEX_UNIQUE =
    'CREATE UNIQUE INDEX %s ON %s (%s)'                                        ;
  ID_CREATE_INDEX_UNIQUE = ID_CREATE_INDEX + 1                 ;

  CREATE_INDEX_IGNORE_NULL =
    'CREATE INDEX %s ON %s (%s) WITH IGNORE NULL'                              ;
  ID_CREATE_INDEX_IGNORE_NULL = ID_CREATE_INDEX_UNIQUE + 1     ;

  CREATE_INDEX_UNIQUE_IGNORE_NULL =
    'CREATE UNIQUE INDEX %s ON %s (%s) WITH IGNORE NULL'                       ;
  ID_CREATE_INDEX_UNIQUE_IGNORE_NULL =
    ID_CREATE_INDEX_IGNORE_NULL + 1                                    ;

  ID_END = ID_CREATE_INDEX_UNIQUE_IGNORE_NULL                     ;
  end;

//=============================================================================
// TGIS_LayerSqlPgdbAbstract
//=============================================================================

  constructor TGIS_LayerSqlPgdbAbstract.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.doDestroy ;
  begin

    inherited ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.fget_TableMaster
    : String ;
  begin
    Result := getCmd( T_SQLPGDB.ID_TABLE_MASTER ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.fget_ViewFeatures
    : String ;
  begin
    if not IsStringEmpty( FViewFeatures ) then
      Result := FViewFeatures
    else
      Result := Table ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.prepareCommandList ;
  begin
    initializeCommandList( T_SQLPGDB.ID_END ) ;

    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_ANNOSYMBOLS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_ANNOSYMBOLS_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_ANNOSYMBOLS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_ATTRRULES_TABLE              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_ATTRRULES_TABLE              ]
                          := T_SQLPGDB.CREATE_GDB_ATTRRULES_TABLE              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_CODEDDOMAINS_TABLE           ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_CODEDDOMAINS_TABLE           ]
                          := T_SQLPGDB.CREATE_GDB_CODEDDOMAINS_TABLE           ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_DATABASELOCKS_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_DATABASELOCKS_TABLE          ]
                          := T_SQLPGDB.CREATE_GDB_DATABASELOCKS_TABLE          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_DEFAULTVALUES_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_DEFAULTVALUES_TABLE          ]
                          := T_SQLPGDB.CREATE_GDB_DEFAULTVALUES_TABLE          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_DOMAINS_TABLE                ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_DOMAINS_TABLE                ]
                          := T_SQLPGDB.CREATE_GDB_DOMAINS_TABLE                ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_EDGECONNRULES_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_EDGECONNRULES_TABLE          ]
                          := T_SQLPGDB.CREATE_GDB_EDGECONNRULES_TABLE          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_EXTENSIONS_TABLE             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_EXTENSIONS_TABLE             ]
                          := T_SQLPGDB.CREATE_GDB_EXTENSIONS_TABLE             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_FEATURECLASSES_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_FEATURECLASSES_TABLE         ]
                          := T_SQLPGDB.CREATE_GDB_FEATURECLASSES_TABLE         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_FEATUREDATASET_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_FEATUREDATASET_TABLE         ]
                          := T_SQLPGDB.CREATE_GDB_FEATUREDATASET_TABLE         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_FIELDINFO_TABLE              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_FIELDINFO_TABLE              ]
                          := T_SQLPGDB.CREATE_GDB_FIELDINFO_TABLE              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_GEOMCOLUMNS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_GEOMCOLUMNS_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_GEOMCOLUMNS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_JNCONNRULES_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_JNCONNRULES_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_JNCONNRULES_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_NETDATASETS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_NETDATASETS_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_NETDATASETS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_OBJECTCLASSES_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_OBJECTCLASSES_TABLE          ]
                          := T_SQLPGDB.CREATE_GDB_OBJECTCLASSES_TABLE          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RANGEDOMAINS_TABLE           ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RANGEDOMAINS_TABLE           ]
                          := T_SQLPGDB.CREATE_GDB_RANGEDOMAINS_TABLE           ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RASTERCATALOGS_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RASTERCATALOGS_TABLE         ]
                          := T_SQLPGDB.CREATE_GDB_RASTERCATALOGS_TABLE         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RELCLASSES_TABLE             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RELCLASSES_TABLE             ]
                          := T_SQLPGDB.CREATE_GDB_RELCLASSES_TABLE             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RELRULES_TABLE               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RELRULES_TABLE               ]
                          := T_SQLPGDB.CREATE_GDB_RELRULES_TABLE               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RELEASEINFO_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_RELEASEINFO_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_RELEASEINFO_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_REPLICADATASET_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_REPLICADATASET_TABLE         ]
                          := T_SQLPGDB.CREATE_GDB_REPLICADATASET_TABLE         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_REPLICAS_TABLE               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_REPLICAS_TABLE               ]
                          := T_SQLPGDB.CREATE_GDB_REPLICAS_TABLE               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_SPATIALREFS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_SPATIALREFS_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_SPATIALREFS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_STRINGDOMAINS_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_STRINGDOMAINS_TABLE          ]
                          := T_SQLPGDB.CREATE_GDB_STRINGDOMAINS_TABLE          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_SUBTYPES_TABLE               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_SUBTYPES_TABLE               ]
                          := T_SQLPGDB.CREATE_GDB_SUBTYPES_TABLE               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOOLBOXES_TABLE              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOOLBOXES_TABLE              ]
                          := T_SQLPGDB.CREATE_GDB_TOOLBOXES_TABLE              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOPOCLASSES_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOPOCLASSES_TABLE            ]
                          := T_SQLPGDB.CREATE_GDB_TOPOCLASSES_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOPORULES_TABLE              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOPORULES_TABLE              ]
                          := T_SQLPGDB.CREATE_GDB_TOPORULES_TABLE              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOPOLOGIES_TABLE             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_TOPOLOGIES_TABLE             ]
                          := T_SQLPGDB.CREATE_GDB_TOPOLOGIES_TABLE             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_USERMETADATA_TABLE           ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_USERMETADATA_TABLE           ]
                          := T_SQLPGDB.CREATE_GDB_USERMETADATA_TABLE           ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_VALIDRULES_TABLE             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_GDB_VALIDRULES_TABLE             ]
                          := T_SQLPGDB.CREATE_GDB_VALIDRULES_TABLE             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS     ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS     ]
                          := T_SQLPGDB.CREATE_TABLE_GEOMETRY_BINARY_POINTS     ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_ARCS       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_ARCS       ]
                          := T_SQLPGDB.CREATE_TABLE_GEOMETRY_BINARY_ARCS       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POLYGONS   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POLYGONS   ]
                          := T_SQLPGDB.CREATE_TABLE_GEOMETRY_BINARY_POLYGONS   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_INDEX      ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_INDEX      ]
                          := T_SQLPGDB.CREATE_TABLE_GEOMETRY_BINARY_INDEX      ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DROP_TABLE                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DROP_TABLE                              ]
                          := T_SQLPGDB.DROP_TABLE                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_FEATURE_DATASET_TABLE        ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_FEATURE_DATASET_TABLE        ]
                          := T_SQLPGDB.SELECT_GDB_FEATURE_DATASET_TABLE        ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE ]
                          := T_SQLPGDB.SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE   ]
                          := T_SQLPGDB.SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_FIELD_INFO_TABLE             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_FIELD_INFO_TABLE             ]
                          := T_SQLPGDB.SELECT_GDB_FIELD_INFO_TABLE             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_GEOM_COLUMNS                 ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_GDB_GEOM_COLUMNS                 ]
                          := T_SQLPGDB.SELECT_GDB_GEOM_COLUMNS                 ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_MAX_GEOMETRY_OBJECTID            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_MAX_GEOMETRY_OBJECTID            ]
                          := T_SQLPGDB.SELECT_MAX_GEOMETRY_OBJECTID            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_MAX_GDB_SPATIALREFS_SRID         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_MAX_GDB_SPATIALREFS_SRID         ]
                          := T_SQLPGDB.SELECT_MAX_GDB_SPATIALREFS_SRID         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_COUNT_GEOMETRY_OBJECTID          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_COUNT_GEOMETRY_OBJECTID          ]
                          := T_SQLPGDB.SELECT_COUNT_GEOMETRY_OBJECTID          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_TABLE_ALL                        ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_TABLE_ALL                        ]
                          := T_SQLPGDB.SELECT_TABLE_ALL                        ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_TABLE_WHERE                      ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_TABLE_WHERE                      ]
                          := T_SQLPGDB.SELECT_TABLE_WHERE                      ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_UID                         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_UID                         ]
                          := T_SQLPGDB.SELECT_JOIN_UID                         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN                             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN                             ]
                          := T_SQLPGDB.SELECT_JOIN                             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_EX                          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_EX                          ]
                          := T_SQLPGDB.SELECT_JOIN_EX                          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_NO_JOIN                          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_NO_JOIN                          ]
                          := T_SQLPGDB.SELECT_NO_JOIN                          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_NOEXT                       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_NOEXT                       ]
                          := T_SQLPGDB.SELECT_JOIN_NOEXT                       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_NOEXT_EX                    ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SELECT_JOIN_NOEXT_EX                    ]
                          := T_SQLPGDB.SELECT_JOIN_NOEXT_EX                    ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_SHAPE                            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_SHAPE                            ]
                          := T_SQLPGDB.DELETE_SHAPE                            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FROM                             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FROM                             ]
                          := T_SQLPGDB.DELETE_FROM                             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FIELD_FROM_GDB_FIELDINFO         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FIELD_FROM_GDB_FIELDINFO         ]
                          := T_SQLPGDB.DELETE_FIELD_FROM_GDB_FIELDINFO         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_FEATURECLASSES  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_FEATURECLASSES  ]
                          := T_SQLPGDB.DELETE_FEATURE_FROM_GDB_FEATURECLASSES  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_FIELDINFO       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_FIELDINFO       ]
                          := T_SQLPGDB.DELETE_FEATURE_FROM_GDB_FIELDINFO       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS     ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS     ]
                          := T_SQLPGDB.DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS     ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_ITEMS           ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_ITEMS           ]
                          := T_SQLPGDB.DELETE_FEATURE_FROM_GDB_ITEMS           ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_USERMETADATA    ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_USERMETADATA    ]
                          := T_SQLPGDB.DELETE_FEATURE_FROM_GDB_USERMETADATA    ;
    assert( FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_OBJECTCLASSES   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_OBJECTCLASSES   ]
                          := T_SQLPGDB.DELETE_FEATURE_FROM_GDB_OBJECTCLASSES   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_FEATUREDATASET_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_FEATUREDATASET_TABLE         ]
                          := T_SQLPGDB.INSERT_GDB_FEATUREDATASET_TABLE         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_OBJECTCLASSES_TABLE          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_OBJECTCLASSES_TABLE          ]
                          := T_SQLPGDB.INSERT_GDB_OBJECTCLASSES_TABLE          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_OBJECTCLASSES_EX_TABLE       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_OBJECTCLASSES_EX_TABLE       ]
                          := T_SQLPGDB.INSERT_GDB_OBJECTCLASSES_EX_TABLE       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_FEATURECLASSES_TABLE         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_FEATURECLASSES_TABLE         ]
                          := T_SQLPGDB.INSERT_GDB_FEATURECLASSES_TABLE         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_GEOMCOLUMNS_TABLE_OLD        ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_GEOMCOLUMNS_TABLE_OLD        ]
                          := T_SQLPGDB.INSERT_GDB_GEOMCOLUMNS_TABLE_OLD        ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_GEOMCOLUMNS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_GEOMCOLUMNS_TABLE            ]
                          := T_SQLPGDB.INSERT_GDB_GEOMCOLUMNS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_ITEMS_TABLE                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_ITEMS_TABLE                  ]
                          := T_SQLPGDB.INSERT_GDB_ITEMS_TABLE                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_FIELDINFO                    ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_FIELDINFO                    ]
                          := T_SQLPGDB.INSERT_GDB_FIELDINFO                    ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_SPATIALREFS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_GDB_SPATIALREFS_TABLE            ]
                          := T_SQLPGDB.INSERT_GDB_SPATIALREFS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_UPDATE_GDB_GEOMCOLUMNS_TABLE            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_UPDATE_GDB_GEOMCOLUMNS_TABLE            ]
                          := T_SQLPGDB.UPDATE_GDB_GEOMCOLUMNS_TABLE            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_UPDATE_DBX_GEO_BINARY                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_UPDATE_DBX_GEO_BINARY                   ]
                          := T_SQLPGDB.UPDATE_DBX_GEO_BINARY                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_UPDATE_DBX                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_UPDATE_DBX                              ]
                          := T_SQLPGDB.UPDATE_DBX                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POINTS_VALUE             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POINTS_VALUE             ]
                          := T_SQLPGDB.INSERT_DBX_GEO_POINTS_VALUE             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POINTS_PARAM             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POINTS_PARAM             ]
                          := T_SQLPGDB.INSERT_DBX_GEO_POINTS_PARAM             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_ARCS_VALUE               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_ARCS_VALUE               ]
                          := T_SQLPGDB.INSERT_DBX_GEO_ARCS_VALUE               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_ARCS_PARAM               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_ARCS_PARAM               ]
                          := T_SQLPGDB.INSERT_DBX_GEO_ARCS_PARAM               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POLYGONS_VALUE           ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POLYGONS_VALUE           ]
                          := T_SQLPGDB.INSERT_DBX_GEO_POLYGONS_VALUE           ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POLYGONS_PARAM           ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_POLYGONS_PARAM           ]
                          := T_SQLPGDB.INSERT_DBX_GEO_POLYGONS_PARAM           ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_INDEX_VALUE              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_INDEX_VALUE              ]
                          := T_SQLPGDB.INSERT_DBX_GEO_INDEX_VALUE              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_INDEX_PARAM              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX_GEO_INDEX_PARAM              ]
                          := T_SQLPGDB.INSERT_DBX_GEO_INDEX_PARAM              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_RELEASEINFO                      ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_RELEASEINFO                      ]
                          := T_SQLPGDB.INSERT_RELEASEINFO                      ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INSERT_DBX                              ]
                          := T_SQLPGDB.INSERT_DBX                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_DROP_COLUMN                       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_DROP_COLUMN                       ]
                          := T_SQLPGDB.ALTER_DROP_COLUMN                       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_STRING                        ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_STRING                        ]
                          := T_SQLPGDB.ALTER_ADD_STRING                        ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_MEMO                          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_MEMO                          ]
                          := T_SQLPGDB.ALTER_ADD_MEMO                          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ALTER_STRING                      ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ALTER_STRING                      ]
                          := T_SQLPGDB.ALTER_ALTER_STRING                      ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_DATE                          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_DATE                          ]
                          := T_SQLPGDB.ALTER_ADD_DATE                          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_BOOLEAN                       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_BOOLEAN                       ]
                          := T_SQLPGDB.ALTER_ADD_BOOLEAN                       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_INTEGER                       ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_INTEGER                       ]
                          := T_SQLPGDB.ALTER_ADD_INTEGER                       ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_FLOAT                         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ALTER_ADD_FLOAT                         ]
                          := T_SQLPGDB.ALTER_ADD_FLOAT                         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_POSTRESTRUCTURE                         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_POSTRESTRUCTURE                         ]
                          := T_SQLPGDB.POSTRESTRUCTURE                         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FILTER_UID                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FILTER_UID                              ]
                          := T_SQLPGDB.FILTER_UID                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FILTER_UID_WEAK                         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FILTER_UID_WEAK                         ]
                          := T_SQLPGDB.FILTER_UID_WEAK                         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MAX_NAMELENGTH                          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MAX_NAMELENGTH                          ]
                          := T_SQLPGDB.MAX_NAMELENGTH                          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MAX_TEXTLENGTH                          ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MAX_TEXTLENGTH                          ]
                          := T_SQLPGDB.MAX_TEXTLENGTH                          ;
    assert( FSQLCommands[ T_SQLPGDB.ID_TABLE_MASTER                            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_TABLE_MASTER                            ]
                          := T_SQLPGDB.TABLE_MASTER                            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MINGX                                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MINGX                                   ]
                          := T_SQLPGDB.MINGX                                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MAXGX                                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MAXGX                                   ]
                          := T_SQLPGDB.MAXGX                                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MINGY                                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MINGY                                   ]
                          := T_SQLPGDB.MINGY                                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MAXGY                                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MAXGY                                   ]
                          := T_SQLPGDB.MAXGY                                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_EXTENTLEFT                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_EXTENTLEFT                              ]
                          := T_SQLPGDB.EXTENTLEFT                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_EXTENTRIGHT                             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_EXTENTRIGHT                             ]
                          := T_SQLPGDB.EXTENTRIGHT                             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_EXTENTBOTTOM                            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_EXTENTBOTTOM                            ]
                          := T_SQLPGDB.EXTENTBOTTOM                            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_IDXORIGINX                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_IDXORIGINX                              ]
                          := T_SQLPGDB.IDXORIGINX                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_IDXORIGINY                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_IDXORIGINY                              ]
                          := T_SQLPGDB.IDXORIGINY                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_IDXGRIDSIZE                             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_IDXGRIDSIZE                             ]
                          := T_SQLPGDB.IDXGRIDSIZE                             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_EXTENTTOP                               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_EXTENTTOP                               ]
                          := T_SQLPGDB.EXTENTTOP                               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SHAPETYPE                               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SHAPETYPE                               ]
                          := T_SQLPGDB.SHAPETYPE                               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_OBJECTID                                ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_OBJECTID                                ]
                          := T_SQLPGDB.OBJECTID                                ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INDEXEDOBJECTID                         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INDEXEDOBJECTID                         ]
                          := T_SQLPGDB.INDEXEDOBJECTID                         ;
    assert( FSQLCommands[ T_SQLPGDB.ID_GEOMETRY                                ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_GEOMETRY                                ]
                          := T_SQLPGDB.GEOMETRY                                ;
    assert( FSQLCommands[ T_SQLPGDB.ID_INDEX                                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_INDEX                                   ]
                          := T_SQLPGDB.INDEX                                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ID                                      ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ID                                      ]
                          := T_SQLPGDB.ID                                      ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SHAPE                                   ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SHAPE                                   ]
                          := T_SQLPGDB.SHAPE                                   ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SHAPE_LENGTH                            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SHAPE_LENGTH                            ]
                          := T_SQLPGDB.SHAPE_LENGTH                            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SHAPE_AREA                              ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SHAPE_AREA                              ]
                          := T_SQLPGDB.SHAPE_AREA                              ;
    assert( FSQLCommands[ T_SQLPGDB.ID_GEO                                     ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_GEO                                     ]
                          := T_SQLPGDB.GEO                                     ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SRID                                    ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SRID                                    ]
                          := T_SQLPGDB.SRID                                    ;
    assert( FSQLCommands[ T_SQLPGDB.ID_GGC_SRID                                ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_GGC_SRID                                ]
                          := T_SQLPGDB.GGC_SRID                                ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SRTEXT                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SRTEXT                                  ]
                          := T_SQLPGDB.SRTEXT                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FALSEX                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FALSEX                                  ]
                          := T_SQLPGDB.FALSEX                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FALSEY                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FALSEY                                  ]
                          := T_SQLPGDB.FALSEY                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FALSEZ                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FALSEZ                                  ]
                          := T_SQLPGDB.FALSEZ                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FALSEM                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FALSEM                                  ]
                          := T_SQLPGDB.FALSEM                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_XYUNITS                                 ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_XYUNITS                                 ]
                          := T_SQLPGDB.XYUNITS                                 ;
    assert( FSQLCommands[ T_SQLPGDB.ID_ZUNITS                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_ZUNITS                                  ]
                          := T_SQLPGDB.ZUNITS                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_MUNITS                                  ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_MUNITS                                  ]
                          := T_SQLPGDB.MUNITS                                  ;
    assert( FSQLCommands[ T_SQLPGDB.ID_SHAPE_FIELD                             ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_SHAPE_FIELD                             ]
                          := T_SQLPGDB.SHAPE_FIELD                             ;
    assert( FSQLCommands[ T_SQLPGDB.ID_FIELDNAME                               ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_FIELDNAME                               ]
                          := T_SQLPGDB.FIELDNAME                               ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX                            ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX                            ]
                          := T_SQLPGDB.CREATE_INDEX                            ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX_UNIQUE                     ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX_UNIQUE                     ]
                          := T_SQLPGDB.CREATE_INDEX_UNIQUE                     ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX_IGNORE_NULL                ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX_IGNORE_NULL                ]
                          := T_SQLPGDB.CREATE_INDEX_IGNORE_NULL                ;
    assert( FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL         ] = '' ) ;
            FSQLCommands[ T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL         ]
                          := T_SQLPGDB.CREATE_INDEX_UNIQUE_IGNORE_NULL         ;

    finalizeCommandList ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.getCmdGEOUID(
    const _id : Integer
  ) : String ;
  begin
    assert( _id in [0..1] ) ;
    if      _id = 0 then
            Result := getCmd( T_SQLPGDB.ID_OBJECTID )
    else if _id = 1 then
            Result := getCmd( T_SQLPGDB.ID_GEO ) + '.' + getCmd( T_SQLPGDB.ID_OBJECTID ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.getCmdSHAPETYPE
    : String ;
  begin
    Result := getCmd( T_SQLPGDB.ID_SHAPETYPE ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.getCmdXMIN
    : String ;
  begin
    Result := getCmd( T_SQLPGDB.ID_MINGX ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.getCmdYMIN
    : String ;
  begin
    Result := getCmd( T_SQLPGDB.ID_MINGY ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.getCmdGEOMETRY
    : String ;
  begin
    Result := getCmd( T_SQLPGDB.ID_GEOMETRY ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.prepareAppendCommand(
    const _table : String ;
    const _type  : TGIS_ShapeType
  ) : String ;
  var
    i         : Integer ;
    param     : String  ;
    value     : String  ;
  begin

    if _table = nameTableIndex then begin
      value := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_INDEX_VALUE ) ;
      param := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_INDEX_PARAM ) ;
    end
    else if _table = Table then begin

      if _type = TGIS_ShapeType.Arc then begin
        value := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_ARCS_VALUE ) ;
        param := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_ARCS_PARAM ) ;
      end
      else if _type = TGIS_ShapeType.Polygon then begin
        value := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_POLYGONS_VALUE ) ;
        param := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_POLYGONS_PARAM ) ;
      end
      else begin
        value := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_POINTS_VALUE ) ;
        param := getCmd( T_SQLPGDB.ID_INSERT_DBX_GEO_POINTS_PARAM ) ;
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
    end ;

    Result := Format( getCmd( T_SQLPGDB.ID_INSERT_DBX ), [_table, value, param] ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.prepareUpdateCommand(
    const _table  : String ;
    const _column : String
  ) : String ;
  var
    i         : Integer ;
    param     : String  ;
  begin

    param := '' ;
    if _table = Table then begin
      param := getCmd( T_SQLPGDB.ID_UPDATE_DBX_GEO_BINARY ) ;
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
    end ;

    Result := Format( getCmd( T_SQLPGDB.ID_UPDATE_DBX ),
                      [ _table, param, _column, _column ]
                    ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.prepareSelectCommand(
    const _table  : String ;
    const _filter : String
  ) : String ;
  begin
    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLPGDB.ID_SELECT_TABLE_WHERE ),
                        [ _table, _filter ]
                      )
    else
      Result := Format( getCmd( T_SQLPGDB.ID_SELECT_TABLE_ALL ),
                        [ _table ]
                      ) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.prepareFilterUid(
    const _table : String ;
    const _uid   : TGIS_Uid
  ) : String ;
  begin
    if _table = Table then
      if IsStringEmpty( nameColumnUID ) then
        Result := Format( getCmd( T_SQLPGDB.ID_FILTER_UID ),
                          [ getCmd( T_SQLPGDB.ID_OBJECTID ) , _uid ]
                        )
      else
        Result := Format( getCmd( T_SQLPGDB.ID_FILTER_UID ),
                          [ nameColumnUID , _uid ]
                        )
    else if _table = nameTableIndex then
      Result := Format( getCmd( T_SQLPGDB.ID_FILTER_UID ),
                        [ getCmd( T_SQLPGDB.ID_INDEXEDOBJECTID ) , _uid ]
                      )
    else
      Result := '' ;

  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroConnect ;
  begin
    initializeConnect( True ) ;

    nameLayer := FSQLParameters.Values[ GIS_INI_LAYERSQL_LAYER ] ;

    if ( not oGisDb.IsMsJet ) then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_UNSUPPORTEDDIALECT ),
              FSQLDialectList.Values[ GIS_SQL_PARAMS_ENGINE ],
              0
            ) ;
    parseConfigLayerName ;

    try
      oGisDb.sqlConnect( oGisDb.sqlBaseFolder(self), FSQLParameters );

      try
        oGisDb.sqlQueryOpen(
           Format( getCmd( T_SQLPGDB.ID_SELECT_GDB_FEATURE_OBJECT_CLASSES_TABLE ),
                   [ Table ]
                 ),
           0
        ) ;

        if not oGisDb.sqlQueryEof(0) then
          nameColumnGeometry := VarToString( oGisDb.sqlQueryGetField(
                                    getCmd( T_SQLPGDB.ID_SHAPE_FIELD ),
                                    0
                                  )
                                ) ;

        oGisDb.sqlQueryClose(0) ;
        isNewVersion := False ;
      except
        oGisDb.sqlQueryClose(0) ;

        // check if it's new version
        try
          oGisDb.sqlQueryOpen(
             Format( getCmd( T_SQLPGDB.ID_SELECT_TABLE_ALL ),
                     [ 'GDB_Items' ]
                   ),
             0
          ) ;
          isNewVersion := True ;
          oGisDb.sqlQueryClose(0) ;
        except
          // it's empty database, use old version structure
          oGisDb.sqlQueryClose(0) ;
          isNewVersion := False ;
        end;
      end ;

      if IsStringEmpty( nameColumnGeometry ) then
        nameColumnGeometry := getCmd( T_SQLPGDB.ID_SHAPE ) ;

      updateDialectList( 'GEOMETRY', nameColumnGeometry ) ;

      try
        if not isNewVersion then begin
          oGisDb.sqlQueryOpen(
             Format( getCmd( T_SQLPGDB.ID_SELECT_GDB_FIELD_INFO_TABLE ),
                     [ Table, nameColumnGeometry ]
                   ),
             0
          ) ;

          if not oGisDb.sqlQueryEof(0) then
            nameColumnUID := VarToString( oGisDb.sqlQueryGetField(
                                 getCmd( T_SQLPGDB.ID_FIELDNAME ),
                                 0
                               )
                             ) ;

          oGisDb.sqlQueryClose(0) ;
        end ;
      except
        oGisDb.sqlQueryClose(0) ;
      end ;

      if IsStringEmpty( nameColumnUID ) then
        nameColumnUID := getCmd( T_SQLPGDB.ID_OBJECTID ) ;

      updateDialectList( 'OBJECTID', nameColumnUID ) ;

      prepareCommandList ;

      nameTableIndex := Format( '%s_%s_%s',
                                [ Table,
                                  nameColumnGeometry,
                                  getCmd( T_SQLPGDB.ID_INDEX )
                                ]
                              ) ;
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

  procedure TGIS_LayerSqlPgdbAbstract.macroDisconnect ;
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

  procedure TGIS_LayerSqlPgdbAbstract.macroUpdateStart ;
  begin
    lastUid := -1 ;
    oGisDb.sqlTransactGlobalUpdateStart ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroUpdateEnd ;
  begin
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
    oGisDb.sqlTransactGlobalUpdateCommit ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroMasterCreate ;
  begin
    if IsReadOnly or isNewVersion then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_ANNOSYMBOLS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_AnnoSymbols',
                           'ID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_ATTRRULES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'AttributeRuleByRuleId',
                           'GDB_AttrRules',
                           'RuleID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_CODEDDOMAINS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'ValueDomainByID',
                           'GDB_CodedDomains',
                           'DomainID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_DATABASELOCKS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'LockID_Index',
                           'GDB_DatabaseLocks',
                           'LockID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_DEFAULTVALUES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'ValuesClassByID',
                           'GDB_DefaultValues',
                           'ClassID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_DOMAINS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL ),
                         [ 'DomainByName',
                           'GDB_Domains',
                           'DomainName,Owner'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_Domains',
                           'ID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_EDGECONNRULES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'EdgeConnRuleByRuleID',
                           'GDB_EdgeConnRules',
                           'RuleID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_EXTENSIONS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'ExtensionsByCLSID',
                           'GDB_Extensions',
                           'CLSID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'ExtensionsByName',
                           'GDB_Extensions',
                           'Name'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_Extensions',
                           'ID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_FEATURECLASSES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_IGNORE_NULL ),
                         [ 'FeatureClassByGeomNetworkID',
                           'GDB_FeatureClasses',
                           'GeomNetworkID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_IGNORE_NULL ),
                         [ 'FeatureClassByGraphID',
                           'GDB_FeatureClasses',
                           'GraphID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FeatureClassByObjectClassID',
                           'GDB_FeatureClasses',
                           'ObjectClassID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_FEATUREDATASET_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_FeatureDataset',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL ),
                         [ 'FeatureDatasetByName',
                           'GDB_FeatureDataset',
                           'Name,Owner,DatabaseName'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'FeatureDatasetBySRID',
                           'GDB_FeatureDataset',
                           'SRID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_FIELDINFO_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'FieldInfoByClassId',
                           'GDB_FieldInfo',
                           'ClassID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'FieldInfoByFieldName',
                           'GDB_FieldInfo',
                           'FieldName'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_GEOMCOLUMNS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'SRID_Index',
                           'GDB_GeomColumns',
                           'SRID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'TableName_Index',
                           'GDB_GeomColumns',
                           'TableName'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_JNCONNRULES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'JnConnRuleByRuleID',
                           'GDB_JnConnRules',
                           'RuleID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_NETDATASETS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL ),
                         [ 'ByName',
                           'GDB_NetDatasets',
                           'Name,Owner,DatabaseName'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_NetDatasets',
                           'ID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_OBJECTCLASSES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_ObjectClasses',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_IGNORE_NULL ),
                         [ 'ObjectClassByDatasetID',
                           'GDB_ObjectClasses',
                           'DatasetID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_IGNORE_NULL ),
                         [ 'ObjectClassByModelName',
                           'GDB_ObjectClasses',
                           'ModelName'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL ),
                         [ 'ObjectClassByName',
                           'GDB_ObjectClasses',
                           'Name,Owner,DatabaseName'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_RANGEDOMAINS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'RangeDomainByID',
                           'GDB_RangeDomains',
                           'DomainID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_RASTERCATALOGS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'RasterCatalogByObjectClassID',
                           'GDB_RasterCatalogs',
                           'ObjectClassID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_RELCLASSES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_RelClasses',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL ),
                         [ 'ObjectClassByName',
                           'GDB_RelClasses',
                           'Name,Owner,DatabaseName'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_RELEASEINFO_TABLE ) ) ;
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_INSERT_RELEASEINFO ) ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_RELRULES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'RelationshipRuleByRuleID',
                           'GDB_RelRules',
                           'RuleID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_REPLICADATASET_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_ReplicaDatasets',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'ReplicasById',
                           'GDB_ReplicaDatasets',
                           'ReplicaID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_REPLICAS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_Replicas',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'ReplicasByName',
                           'GDB_Replicas',
                           'Name'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_SPATIALREFS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'SRID_Index',
                           'GDB_SpatialRefs',
                           'SRID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_STRINGDOMAINS_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'StringDomainByID',
                           'GDB_StringDomains',
                           'DomainID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_SUBTYPES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_Subtypes',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'SubtypeByClassByID',
                           'GDB_Subtypes',
                           'ClassID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_TOOLBOXES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_Toolboxes',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'ToolboxesByName',
                           'GDB_Toolboxes',
                           'Name'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_TOPOCLASSES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'TopologyClassByClassID',
                           'GDB_TopoClasses',
                           'ClassID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'TopologyClassByTopologyID',
                           'GDB_TopoClasses',
                           'TopologyID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_TOPOLOGIES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_Topologies',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                         [ 'TopologyByDatasetID',
                           'GDB_Topologies',
                           'DatasetID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'TopologyByID',
                           'GDB_Topologies',
                           'ID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'TopologyByName',
                           'GDB_Topologies',
                           'Name'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_TOPORULES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'TopologyRuleByGUID',
                           'GDB_TopoRules',
                           'RuleGUID'
                         ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'TopologyRuleByRuleID',
                           'GDB_TopoRules',
                           'RuleID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_USERMETADATA_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_UserMetadata',
                           'ID' ]
                       )
               ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE_IGNORE_NULL ),
                         [ 'UserMetadataByName',
                           'GDB_UserMetadata',
                           'Name,Owner,DatabaseName,DatasetType'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;

      try
        oGisDb.sqlExec( getCmd( T_SQLPGDB.ID_CREATE_GDB_VALIDRULES_TABLE ) ) ;
        oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX_UNIQUE ),
                         [ 'FDO_ID',
                           'GDB_ValidRules',
                           'ID'
                         ]
                       )
               ) ;
      except
        // can exist
      end ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroMasterUpdate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType ;
    const _name   : String ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    shape_type  : Integer ;
    newCount    : Integer ;
    newGridSize : Double  ;
    shape_no    : Cardinal ;
    end_uid     : TGIS_Uid ;
    abort       : Boolean ;
    idxEx       : TGIS_Extent ;
  begin
    if IsReadOnly then exit ;

    FSupportedShapes := GisAddShapeType( FSupportedShapes, _type ) ;
    DefaultShapeType := _type ;

    // guarantee connection closing
      oGisDb.sqlQueryClose(0) ;
      oGisDb.sqlTableClose( 0 ) ;
      oGisDb.sqlTableClose( 1 ) ;

    // Index table update
    try
      oGisDb.sqlQueryOpen(
        Format( getCmd( T_SQLPGDB.ID_SELECT_COUNT_GEOMETRY_OBJECTID ),
                [ Table ]
              ),
        0
      ) ;
      try
        newCount := VarToInt32( oGisDb.sqlQueryGetField(
                        getCmd( T_SQLPGDB.ID_ID ),
                        0
                      )
                    ) ;
      except
        newCount := 0 ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    if ( idxCount <> newCount ) or
       ( not GisIsContainExtent( Extent, _extent ) ) then begin

      if newCount > 0 then begin
        newGridSize := Sqrt( (_extent.XMax-_extent.XMin) *
                             (_extent.YMax-_extent.YMin) / newCount
                           ) ;

        if newGridSize > 0 then begin
          shape_no := 0 ;
          end_uid  := GetLastUid ;
          abort    := False ;

          macroUpdateStart ;
          try
            cursorSql[0].disableJoin := True ;
            oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_DELETE_FROM ),
                             [ nameTableIndex ]
                           )
                   ) ;

            cursorFirst( 0, False,
                         GisWholeWorld, '', nil, '', True
                       ) ;

            while not cursorEof(0) do begin // iterate all shapes
              try
                if ( assigned( cursorShape(0) )   ) and
                   ( not cursorShape(0).IsDeleted ) then begin
                  // update index
                  sqlTableAppend( 1, nameTableIndex, _type ) ;

                  idxEx.XMin := FloorS( (cursorShape(0).Extent.XMin - idxOriginX) /
                                        newGridSize
                                      ) ;
                  idxEx.XMax := FloorS( (cursorShape(0).Extent.XMax - idxOriginX) /
                                        newGridSize
                                      ) ;
                  idxEx.YMin := FloorS( (cursorShape(0).Extent.YMin - idxOriginY) /
                                        newGridSize
                                      ) ;
                  idxEx.YMax := FloorS( (cursorShape(0).Extent.YMax - idxOriginY) /
                                        newGridSize
                                      ) ;
                  macroTableSetField( 1, getCmd( T_SQLPGDB.ID_INDEXEDOBJECTID ),
                                         cursorShape(0).Uid
                                    ) ;
                  macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MINGX ),
                                         TruncS(idxEx.XMin)
                                    ) ;
                  macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MAXGX ),
                                         TruncS(idxEx.XMax)
                                    ) ;
                  macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MINGY ),
                                         TruncS(idxEx.YMin)
                                    ) ;
                  macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MAXGY ),
                                         TruncS(idxEx.YMax)
                                    ) ;

                  oGisDb.sqlTablePost( 1 )  ;
                end ;

                inc( shape_no ) ;
                if shape_no mod 100 = 1 then begin
                  abort := RaiseBusyShake( self, cursorShape(0).Uid, end_uid ) ;
                  if abort then break ;
                end ;
              finally
                cursorNext(0) ;
              end ;
            end ;

          finally
            macroUpdateEnd ;
            idxGridSize := newGridSize ;
            cursorSql[0].disableJoin := False ;
          end ;

        end ;
      end ;
    end ;

    // GDB_GeomColumns table update
    case Integer(_type) of
      Integer(TGIS_ShapeType.Point)      : shape_type := 1 ;
      Integer(TGIS_ShapeType.MultiPoint) : shape_type := 2 ;
      Integer(TGIS_ShapeType.Arc)        : shape_type := 3 ;
      Integer(TGIS_ShapeType.Polygon)    : shape_type := 4 ;
    else
      shape_type := 1 ;
    end ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_UPDATE_GDB_GEOMCOLUMNS_TABLE ),
                       [ shape_type,
                         DotFloatToStr( _extent.XMin ),
                         DotFloatToStr( _extent.YMin ),
                         DotFloatToStr( _extent.XMax ),
                         DotFloatToStr( _extent.YMax ),
                         DotFloatToStr( idxOriginX   ),
                         DotFloatToStr( idxOriginY   ),
                         DotFloatToStr( idxGridSize  ),
                         Table
                       ]
                     )
             ) ;
    except

    end ;
  end ;

  function prepareRandomGuid : String ;
  {$IFNDEF OXYGENE}
  var
    sguid : String ;
    oguid : TGuid  ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        Result := System.Guid.NewGuid().ToString('D') ;
      {$ENDIF}
      {$IFDEF JAVA}
        Result := TGuid.randomUUID().ToString ;
      {$ENDIF}
    {$ELSE}
      CreateGUID(oguid) ;

      sguid  := GuidToString( oguid ) ;
      Result := sguid ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroTableCreate(
    const _extent : TGIS_Extent    ;
    const _type   : TGIS_ShapeType
  ) ;
  const
    FCXML =
    '<DEFeatureClassInfo xsi:type=''typens:DEFeatureClassInfo'' ' +
      'xmlns:xsi=''http://www.w3.org/2001/XMLSchema-instance'' '+
      'xmlns:xs=''http://www.w3.org/2001/XMLSchema'' '+
      'xmlns:typens=''http://www.esri.com/schemas/ArcGIS/10.3''>' +
      '<CatalogPath>\%s</CatalogPath>' +
      '<Name>%s</Name>' +
      '<ChildrenExpanded>false</ChildrenExpanded>' +
      '<DatasetType>esriDTFeatureClass</DatasetType>' +
      '<DSID>3</DSID>' +
      '<Versioned>false</Versioned>' +
      '<CanVersion>false</CanVersion>' +
      '<ConfigurationKeyword></ConfigurationKeyword>' +
      '<RequiredGeodatabaseClientVersion>10.0</RequiredGeodatabaseClientVersion>' +
      '<HasOID>true</HasOID>' +
      '<OIDFieldName>OBJECTID</OIDFieldName>' +
      '<GPFieldInfoExs xsi:type=''typens:ArrayOfGPFieldInfoEx''>' +
        '<GPFieldInfoEx xsi:type=''typens:GPFieldInfoEx''>' +
          '<Name>OBJECTID</Name>' +
          '<FieldType>esriFieldTypeOID</FieldType>' +
          '<IsNullable>false</IsNullable>' +
          '<Required>true</Required>' +
          '<Editable>false</Editable>' +
        '</GPFieldInfoEx>' +
        '<GPFieldInfoEx xsi:type=''typens:GPFieldInfoEx''>' +
          '<Name>Shape</Name>' +
          '<FieldType>esriFieldTypeGeometry</FieldType>' +
          '<IsNullable>true</IsNullable>' +
          '<Required>true</Required>' +
        '</GPFieldInfoEx>' +
        '<GPFieldInfoEx xsi:type=''typens:GPFieldInfoEx''>' +
          '<Name>Shape_Length</Name>' +
          '<FieldType>esriFieldTypeDouble</FieldType>' +
          '<IsNullable>true</IsNullable>' +
          '<Required>true</Required>' +
          '<Editable>false</Editable>' +
        '</GPFieldInfoEx>' +
        '<GPFieldInfoEx xsi:type=''typens:GPFieldInfoEx''>' +
          '<Name>Shape_Area</Name>' +
          '<FieldType>esriFieldTypeDouble</FieldType>' +
          '<IsNullable>true</IsNullable>' +
          '<Required>true</Required>' +
          '<Editable>false</Editable>' +
        '</GPFieldInfoEx>' +
      '</GPFieldInfoExs>' +
      '<CLSID>{52353152-891A-11D0-BEC6-00805F7C4268}</CLSID>' +
      '<EXTCLSID></EXTCLSID>' +
      '<RelationshipClassNames xsi:type=''typens:Names''></RelationshipClassNames>' +
      '<AliasName></AliasName>' +
      '<ModelName></ModelName>' +
      '<HasGlobalID>false</HasGlobalID>' +
      '<GlobalIDFieldName></GlobalIDFieldName>' +
      '<RasterFieldName></RasterFieldName>' +
      '<ExtensionProperties xsi:type=''typens:PropertySet''>' +
        '<PropertyArray xsi:type=''typens:ArrayOfPropertySetProperty''></PropertyArray>' +
      '</ExtensionProperties>' +
      '<ControllerMemberships xsi:type=''typens:ArrayOfControllerMembership''></ControllerMemberships>' +
      '<EditorTrackingEnabled>false</EditorTrackingEnabled>' +
      '<CreatorFieldName></CreatorFieldName>' +
      '<CreatedAtFieldName></CreatedAtFieldName>' +
      '<EditorFieldName></EditorFieldName>' +
      '<EditedAtFieldName></EditedAtFieldName>' +
      '<IsTimeInUTC>true</IsTimeInUTC>' +
      '<FeatureType>esriFTSimple</FeatureType>' +
      '<ShapeType>esriGeometryPolygon</ShapeType>' +
      '<ShapeFieldName>Shape</ShapeFieldName>' +
      '<HasM>false</HasM>' +
      '<HasZ>false</HasZ>' +
      '<HasSpatialIndex>true</HasSpatialIndex>' +
      '<AreaFieldName>Shape_Area</AreaFieldName>' +
      '<LengthFieldName>Shape_Length</LengthFieldName>' +
      '<Extent xsi:nil=''true''/>' +
      '<SpatialReference xsi:type=''typens:%s''>' +
        '<WKT>%s</WKT>' +
        '<HighPrecision>true</HighPrecision>' +
        '<WKID>%d</WKID>' +
      '</SpatialReference>' +
      '<ChangeTracked>false</ChangeTracked>' +
      '<FieldFilteringEnabled>false</FieldFilteringEnabled>' +
      '<FilteredFieldNames xsi:type=''typens:Names''></FilteredFieldNames>' +
      '</DEFeatureClassInfo>' ;

      function escape_text( const _value : String ) : String ;
      var
        isrc   : Integer        ;
        res    : String         ;
        res_sb : TStringBuilder ;
      const
        ESC_AMP  = '&amp;'  ;
        ESC_LT   = '&lt;'   ;
        ESC_GT   = '&gt;'   ;
        ESC_APOS = '&apos;' ;
        ESC_QUOT = '&quot;' ;
      begin
        res := '' ;
        res_sb := TStringBuilder.Create ;
        try
          for isrc := StringFirst to StringLast( _value ) do begin
            case _value[ isrc ] of
              '&'  : res_sb.Append( ESC_AMP  ) ;
              '<'  : res_sb.Append( ESC_LT   ) ;
              '>'  : res_sb.Append( ESC_GT   ) ;
              '''' : res_sb.Append( ESC_APOS ) ;
              '"'  : res_sb.Append( ESC_QUOT ) ;
              else   res_sb.Append( _value[isrc] ) ;
            end ;
          end ;
          res := res_sb.ToString ;
        finally
          FreeObject( res_sb ) ;
        end ;

        Result := res ;
      end ;

      function getFCXML : String ;
      var
        cs_type : String ;
      begin
        if CS is TGIS_CSGeographicCoordinateSystem then
          cs_type := 'GeographicCoordinateSystem'
        else if CS is TGIS_CSProjectedCoordinateSystem then
          cs_type := 'ProjectedCoordinateSystem'
        else
          cs_type := 'UnknownCoordinateSystem' ;

        Result := Format( FCXML, [ Table, Table, cs_type, escape_text(CS.WKT), CS.EPSG ] ) ;
      end ;

  var
    shape_type : Integer ;
    new_sr     : Boolean ;
    sridText   : String  ;
  begin
    if IsReadOnly then exit ;

    shape_type := 1 ;

    oGisDb.sqlTransactRestructStart ;
    try
      try
        if      _type = TGIS_ShapeType.Point then begin
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS ),
                    [ Table ]
                  )
          ) ;
          shape_type := 1 ;
        end
        else if _type = TGIS_ShapeType.MultiPoint then begin
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS ),
                    [ Table ]
                  )
          ) ;
          shape_type := 2 ;
        end
        else if _type = TGIS_ShapeType.Arc then begin
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_ARCS ),
                    [ Table ]
                  )
          ) ;
          shape_type := 3 ;
        end
        else if  _type = TGIS_ShapeType.Polygon then begin
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POLYGONS ),
                    [ Table ]
                  )
          ) ;
          shape_type := 4 ;
        end
        else begin
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_POINTS ),
                    [ Table ]
                  )
          ) ;
        end ;
      except
        // can exists
      end ;

      try
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_CREATE_TABLE_GEOMETRY_BINARY_INDEX ),
                  [ nameTableIndex ]
                )
        ) ;
        oGisDb.sqlExec(
           Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                   [ 'IndexedObjectId_Index',
                     nameTableIndex,
                     'IndexedObjectId'
                   ]
                 )
        ) ;
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                  [ 'MaxGX_Index',
                    nameTableIndex,
                    'MaxGX'
                  ]
                )
        ) ;
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                  [ 'MaxGY_Index',
                    nameTableIndex,
                    'MaxGY'
                  ]
                )
        ) ;
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                  [ 'MinGX_Index',
                    nameTableIndex,
                    'MinGX'
                  ]
                )
        ) ;
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_CREATE_INDEX ),
                  [ 'MinGY_Index',
                    nameTableIndex,
                    'MinGY'
                  ]
                )
        ) ;
      except
        // can exists
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;

    try
      hasElevation := False ;
      hasMeasure   := False ;
      idxOriginX   := 0 ;
      idxOriginY   := 0 ;
      idxGridSize  := 0 ;

      falseX  := 0 ;
      falseY  := 0 ;
      scaleXY := 1 ;
      falseZ  := 0 ;
      scaleZ  := 1 ;
      falseM  := 0 ;
      scaleM  := 1 ;

      if not IsStringEmpty( nameDataset ) then begin
        new_sr := True ;
        try
          oGisDb.sqlQueryOpen(
            Format( getCmd( T_SQLPGDB.ID_SELECT_GDB_FEATURE_DATASET_TABLE ),
                    [ nameDataset ]
                  ),
            0
          ) ;
          if not oGisDb.sqlQueryEof(0) then begin
            new_sr := False ;
            srId   := VarToInt32( oGisDb.sqlQueryGetField(
                          getCmd( T_SQLPGDB.ID_SRID ),
                          0
                        )
                      ) ;
          end ;
        finally
          oGisDb.sqlQueryClose(0) ;
        end ;
      end
      else
        new_sr := True ;

      if CS.EPSG = 0 then
        sridText := '{B286C06B-0879-11D2-AACA-00C04FA33C20}'
      else
        sridText := CS.FullWKT ;

      // Setting Unknown spatial reference system
      if new_sr then begin
        try
          oGisDb.sqlTableClose( 0 ) ;
          try
            sqlTableAppend( 0, 'GDB_SpatialRefs', _type ) ;

            macroTableSetField( 0,
                                getCmd( T_SQLPGDB.ID_SRTEXT ),
                                sridText
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_FALSEX  ),
                                   DotFloatToStr(falseX)
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_FALSEY  ),
                                   DotFloatToStr(falseY)
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_XYUNITS ),
                                   DotFloatToStr(scaleXY)
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_FALSEZ  ),
                                   DotFloatToStr(falseZ)
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_ZUNITS  ),
                                   DotFloatToStr(scaleZ)
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_FALSEM  ),
                                   DotFloatToStr(falseM)
                              ) ;
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_MUNITS  ),
                                   DotFloatToStr(scaleM)
                              ) ;

            oGisDb.sqlTablePost( 0 )  ;

            srId := VarToInt32( oGisDb.sqlTableGetField( 0, getCmd( T_SQLPGDB.ID_SRID )  ) ) ;
            oGisDb.sqlTableClose( 0 ) ;
          except
            oGisDb.sqlExec(
              Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_SPATIALREFS_TABLE ),
                      [ sridText,
                        DotFloatToStr( falseX  ),
                        DotFloatToStr( falseY  ),
                        DotFloatToStr( scaleXY ),
                        DotFloatToStr( falseZ  ),
                        DotFloatToStr( scaleZ  ),
                        DotFloatToStr( falseM  ),
                        DotFloatToStr( scaleM  )
                      ]
                    )
              ) ;

            oGisDb.sqlQueryOpen(
              getCmd( T_SQLPGDB.ID_SELECT_MAX_GDB_SPATIALREFS_SRID ),
              0
            ) ;
            try
              try
                srId := VarToInt32( oGisDb.sqlQueryGetField(
                            getCmd( T_SQLPGDB.ID_ID ),
                            0
                          )
                        ) ;
              except
                srId := 0 ;
              end ;
            finally
              oGisDb.sqlQueryClose(0) ;
            end ;
          end ;
        except
          srId := 0 ;
          oGisDb.sqlTableClose( 0 ) ;
        end ;
      end ;

      if not isNewVersion then begin
        // Setting Dataset
        if IsStringEmpty( nameDataset ) then begin
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_OBJECTCLASSES_TABLE ),
                    [ Table,
                      '{52353152-891A-11D0-BEC6-00805F7C4268}',
                      'NULL'
                    ]
                  )
            ) ;
        end
        else begin

          if new_sr then begin
            try
              oGisDb.sqlExec(
                Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FEATUREDATASET_TABLE ),
                        [ nameDataset,
                          srId
                        ]
                      )
                ) ;
            except
              // can exists
            end ;
          end ;

          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_OBJECTCLASSES_EX_TABLE ),
                    [ Table,
                      '{52353152-891A-11D0-BEC6-00805F7C4268}',
                      nameDataset
                    ]
                  )
            ) ;
        end ;

        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FEATURECLASSES_TABLE ),
                  [ 1,
                    shape_type,
                    getCmd( T_SQLPGDB.ID_SHAPE ),
                    Table
                  ]
                )
          ) ;
      end
      else begin
        // UUID,Type,Name,PhysicalName,Path,DatasetSubtype1,DatasetSubtype2,
        // DatasetInfo1,DatasetInfo2,URL,Definition,Documentation,ItemInfo,
        // Properties,Defaults,Shape
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_ITEMS_TABLE ),
                  [ prepareRandomGuid,
                    '{70737809-852C-4A03-9E22-2CECEA5B9BFA}',
                    Table,
                    UpperCase(Table),
                    '\' + Table,
                    1,
                    shape_type,
                    getCmd( T_SQLPGDB.ID_SHAPE ),
                    'NULL',
                    'NULL',
                    getFCXML, // Definition
                    'NULL',
                    'NULL',
                    1,
                    'NULL',
                    'NULL'
                  ]
                )
          ) ;
      end;

      try
        oGisDb.sqlExec(
           Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_GEOMCOLUMNS_TABLE ),
                   [ Table,
                     getCmd( T_SQLPGDB.ID_SHAPE ),
                     shape_type,
                     DotFloatToStr( _extent.XMin ),
                     DotFloatToStr( _extent.YMin ),
                     DotFloatToStr( _extent.XMax ),
                     DotFloatToStr( _extent.YMax ),
                     DotFloatToStr( idxOriginX   ),
                     DotFloatToStr( idxOriginY   ),
                     DotFloatToStr( idxGridSize  ),
                     srId,
                     Integer( hasElevation ),
                     Integer( hasMeasure   ),
                     DotFloatToStr( ZLow   ),
                     DotFloatToStr( ZHigh  ),
                     DotFloatToStr( MLow   ),
                     DotFloatToStr( MHigh  )
                   ]
                 )
           ) ;
      except // try old version
        oGisDb.sqlExec(
           Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_GEOMCOLUMNS_TABLE_OLD ),
                   [ Table,
                     getCmd( T_SQLPGDB.ID_SHAPE ),
                     shape_type,
                     DotFloatToStr( _extent.XMin ),
                     DotFloatToStr( _extent.YMin ),
                     DotFloatToStr( _extent.XMax ),
                     DotFloatToStr( _extent.YMax ),
                     DotFloatToStr( idxOriginX   ),
                     DotFloatToStr( idxOriginY   ),
                     DotFloatToStr( idxGridSize  ),
                     srId,
                     Integer( hasElevation ),
                     Integer( hasMeasure   )
                   ]
                 )
           ) ;
      end;
    except
      macroMasterUpdate( _extent, _type, '', DefaultDimension ) ;
    end ;

    if not isNewVersion then begin
      try
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FIELDINFO ),
                  [ getCmd( T_SQLPGDB.ID_OBJECTID ),
                    getCmd( T_SQLPGDB.ID_OBJECTID ),
                    getCmd( T_SQLPGDB.ID_OBJECTID ),
                    1,
                    0,
                    0,
                    Table
                  ]
                )
        ) ;
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FIELDINFO ),
                  [ getCmd( T_SQLPGDB.ID_SHAPE ),
                    getCmd( T_SQLPGDB.ID_SHAPE ),
                    getCmd( T_SQLPGDB.ID_SHAPE ),
                    1,
                    0,
                    1,
                    Table
                  ]
                )
          ) ;
        if ( _type = TGIS_ShapeType.Arc     ) or
           ( _type = TGIS_ShapeType.Polygon ) then
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FIELDINFO ),
                    [ getCmd( T_SQLPGDB.ID_SHAPE_LENGTH ),
                      getCmd( T_SQLPGDB.ID_SHAPE_LENGTH ),
                      getCmd( T_SQLPGDB.ID_SHAPE_LENGTH ),
                      1,
                      0,
                      0,
                      Table
                    ]
                  )
          ) ;
        if _type = TGIS_ShapeType.Polygon then
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FIELDINFO ),
                    [ getCmd( T_SQLPGDB.ID_SHAPE_AREA ),
                      getCmd( T_SQLPGDB.ID_SHAPE_AREA ),
                      getCmd( T_SQLPGDB.ID_SHAPE_AREA ),
                      1,
                      0,
                      0,
                      Table
                    ]
                  )
          ) ;
      except
        //do nothing
      end ;
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroTableAlter(
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

      procedure delete_metadata(
        const _field: String
      ) ;
      begin
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_DELETE_FIELD_FROM_GDB_FIELDINFO ),
                  [ _field,
                    Table
                  ]
                )
         ) ;
      end ;

      procedure insert_metadata(
        const _field: String;
        const _field_type: TGIS_FieldType
      ) ;
      begin
        if not isNewVersion then
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_INSERT_GDB_FIELDINFO ),
                    [ _field,
                      _field,
                      _field,
                      1,
                      0,
                      1,
                      Table
                    ]
                  )
            ) ;
      end ;

  begin
    if IsReadOnly then exit ;

    oGisDb.sqlQueryClose(0) ;
    oGisDb.sqlTableClose( 0 ) ;

    try
      lname := StrToInt( getCmd( T_SQLPGDB.ID_MAX_NAMELENGTH ) ) ;
    except
      lname := 16 ;
    end ;

    try
      if not IsStringEmpty( getCmd( T_SQLPGDB.ID_MAX_TEXTLENGTH ) ) then
        ltext := StrToInt( getCmd( T_SQLPGDB.ID_MAX_TEXTLENGTH ) )
      else
        ltext := GIS_SQL_MEMO_SIZE ;
    except
      ltext := GIS_SQL_MEMO_SIZE ;
    end ;

    _layer.PrepareExportFieldNames( lname, Self <> _layer ) ;

    lftn := Table ;

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
                    oGisDb.sqlExec(
                      Format( getCmd( T_SQLPGDB.ID_ALTER_DROP_COLUMN ),
                              [ lftn,
                                fname
                              ]
                            )
                      ) ;
                  except
                    // name can not exist
                  end ;
                  // Delete metadata from GDB_FieldInfo table
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
                                 oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_STRING ),
                                                  [ lftn, fname, fld.NewWidth ]
                                                )
                                        )
                              else
                                 oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_MEMO ),
                                                  [ lftn, fname ]
                                                )
                                        ) ;
                             // Insert metadata to GDB_FieldInfo table
                             insert_metadata( fname, fld.FieldType ) ;
                           end ;
                        TGIS_FieldType.Date :
                           begin
                             oGisDb.sqlExec(
                               Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_DATE ),
                                       [ lftn,
                                         fname
                                       ]
                                     )
                               ) ;
                             // Insert metadata to GDB_FieldInfo table
                             insert_metadata( fname, fld.FieldType ) ;
                           end ;
                        TGIS_FieldType.Boolean :
                           begin
                             oGisDb.sqlExec(
                               Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_BOOLEAN ),
                                       [ lftn,
                                         fname
                                       ]
                                     )
                               ) ;
                             // Insert metadata to GDB_FieldInfo table
                             insert_metadata( fname, fld.FieldType ) ;
                           end ;
                        TGIS_FieldType.Number :
                           begin
                            if fld.NewDecimal = 0 then begin
                                oGisDb.sqlExec(
                                  Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_INTEGER ),
                                          [ lftn,
                                            fname
                                          ]
                                        )
                                  ) ;
                              // Insert metadata to GDB_FieldInfo table
                              insert_metadata( fname, TGIS_FieldType.Number ) ;
                            end
                            else begin
                              oGisDb.sqlExec(
                                Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_FLOAT ),
                                        [ lftn,
                                          fname
                                        ]
                                      )
                                ) ;
                              // Insert metadata to GDB_FieldInfo table
                              insert_metadata( fname, TGIS_FieldType.Float ) ;
                            end ;
                          end ;
                        TGIS_FieldType.Float :
                          begin
                            oGisDb.sqlExec(
                              Format( getCmd( T_SQLPGDB.ID_ALTER_ADD_FLOAT ),
                                      [ lftn,
                                        fname
                                      ]
                                    )
                              ) ;
                            // Insert metadata to GDB_FieldInfo table
                            insert_metadata( fname, fld.FieldType ) ;
                          end
                        else raise EGIS_Exception.Create(
                                     _rsrc( GIS_RS_ERR_WRONGFIELD ),
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
        else    begin
                  if fld.NewName <> fld.Name then // rename a files
                  begin
                    // column renaming is not implemented on most databases
                    fld.NewName    := fld.Name ;
                    fld.ExportName := fld.Name ;
                  end ;
                  if ( fld.FieldType = TGIS_FieldType.String ) and
                     ( fld.NewWidth <> fld.Width          ) then // change width
                  begin
                    try
                      oGisDb.sqlExec(
                        Format( getCmd( T_SQLPGDB.ID_ALTER_ALTER_STRING ),
                                [ lftn,
                                  fname,
                                  fld.NewWidth
                                ]
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

  end;

  procedure TGIS_LayerSqlPgdbAbstract.macroTableDrop ;
  begin
    if IsReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      if not isNewVersion then begin
        try
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_FEATURECLASSES ),
                    [ Table ]
                  )
            ) ;
        except
          // can no exist
        end ;

        try
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_FIELDINFO ),
                    [ Table ]
                  )
            ) ;
        except
          // can no exist
        end ;
      end
      else begin
        try
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_ITEMS ),
                    [ Table ]
                  )
            ) ;
        except
          // can no exist
        end ;
      end;

      try
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_GEOMCOLUMNS ),
                  [ Table ]
                )
          ) ;
      except
        // can no exist
      end ;

      if not isNewVersion then begin
        try
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_USERMETADATA ),
                    [ Table ]
                  )
            ) ;
        except
          // can no exist
        end ;

        try
          oGisDb.sqlExec(
            Format( getCmd( T_SQLPGDB.ID_DELETE_FEATURE_FROM_GDB_OBJECTCLASSES ),
                    [ Table ]
                  )
            ) ;
        except
          // can no exist
        end ;
      end ;

      try
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_DROP_TABLE ),
                  [ Table ]
                )
          ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec(
          Format( getCmd( T_SQLPGDB.ID_DROP_TABLE ),
                  [ nameTableIndex ]
                )
          ) ;
      except
        // can no exist
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroTableSetField(
    const _id   : Integer ;
    const _name : String ;
    const _val  : OleVariant
  ) ;
  var
    i : Integer ;
  begin
    if IsReadOnly then exit ;

    i := FindField( _name ) ;
    try
      if ( i >= 0 ) and ( not FieldInfo( i ).FileFormat ) then // field from layer
        oGisDb.sqlTableSetField(
          _id,
          FieldInfo(i).ExportName,
          _val,
          FieldInfo(i).NewWidth
        )
      else // internal field
        oGisDb.sqlTableSetField(
          _id,
          _name,
          _val,
          1
        );
    except
      // maybe field name was changed by someone else.....
      assert( False, 'Field not exists' ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroShapeDelete(
    const _uid : TGIS_Uid
  ) ;
  begin
    if IsReadOnly then exit ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_DELETE_SHAPE ),
                       [ Table,
                         nameColumnUID,
                         _uid
                       ]
                     )
             ) ;
    except
    end ;

    try
      oGisDb.sqlExec( Format( getCmd( T_SQLPGDB.ID_DELETE_SHAPE ),
                       [ nameTableIndex,
                         getCmd( T_SQLPGDB.ID_INDEXEDOBJECTID ),
                         _uid
                       ]
                     )
              ) ;
    except
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.macroShapeUpdate(
    const _shp    : TGIS_Shape ;
    const _import : Boolean
  ) ;
  var
    i         : Integer ;
    fldinfo   : TGIS_FieldInfo ;
    fld       : OleVariant ;
    uid       : TGIS_Uid ;
    txt       : String  ;
    iloop     : Integer ;
    idxEx     : TGIS_Extent ;
    skipField : Boolean ;
    skipShape : Boolean ;
  begin
    if IsReadOnly then exit ;

    uid := -1 ;

    oGisDb.sqlTransactUpdateStart ;
    try
      iloop := T_SQLPGDB.PGDB_RETRY_COUNT ;
      while iloop > 0 do begin
        try
          skipField := False ;
          skipShape := False ;
          // update geometry
          if _import then begin
            if lastUid < 0 then
              lastUid := macroUidLast ;
            inc( lastUid ) ;
            uid := lastUid ;
            if iloop = T_SQLPGDB.PGDB_RETRY_COUNT then
              sqlTableAppend( 0, Table, _shp.ShapeType ) ;

            if IsStringEmpty( nameColumnUID ) then
              macroTableSetField( 0, getCmd( T_SQLPGDB.ID_OBJECTID ) , uid )
            else
              macroTableSetField( 0, nameColumnUID, uid ) ;
          end
          else begin
            skipField := True ;
            uid := _shp.Uid ;
            skipShape := not _shp.GeometryChanged and not _shp.IsNewShape ;
            sqlTableOpenWrite( 0, Table, nameColumnUID, uid ) ;
          end ;

          if not skipShape then
            sqlTableSetGeometry( 0, nameColumnGeometry, _shp ) ;

          // update features
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
              fld := _shp.GetFieldEx( getCmd( T_SQLPGDB.ID_OBJECTID ) )
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
              else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
            end ;
          end ;

          if ( _shp.ShapeType = TGIS_ShapeType.Arc     ) or
             ( _shp.ShapeType = TGIS_ShapeType.Polygon ) then
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_SHAPE_LENGTH ) , _shp.Length ) ;

          if ( _shp.ShapeType = TGIS_ShapeType.Polygon ) then
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_SHAPE_AREA ) , _shp.Area ) ;

          oGisDb.sqlTablePost( 0 ) ;
          iloop := 0 ;
        except
          dec( iloop ) ;
          if iloop <= 0 then raise ;
          Sleep( GetRandom( T_SQLPGDB.PGDB_RETRY_INTERVAL ) ) ;
          lastUid := GetLastUid ;
        end ;
      end ;
    finally
      oGisDb.sqlTransactUpdateCommit ;
    end ;

    try
      // update index
      if _import or _shp.IsNewShape then begin

        sqlTableAppend( 1, nameTableIndex, _shp.ShapeType ) ;

        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_INDEXEDOBJECTID ), uid ) ;
      end
      else begin
        if not _shp.GeometryChanged then exit ;
        sqlTableOpenWrite( 1,
                           nameTableIndex,
                           getCmd( T_SQLPGDB.ID_INDEXEDOBJECTID ),
                           uid
                         ) ;
      end ;

      if idxGridSize > 0 then begin
        idxEx.XMin := FloorS( (_shp.Extent.XMin - idxOriginX) / idxGridSize ) ;
        idxEx.XMax := FloorS( (_shp.Extent.XMax - idxOriginX) / idxGridSize ) ;
        idxEx.YMin := FloorS( (_shp.Extent.YMin - idxOriginY) / idxGridSize ) ;
        idxEx.YMax := FloorS( (_shp.Extent.YMax - idxOriginY) / idxGridSize ) ;

        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MINGX ), idxEx.XMin ) ;
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MAXGX ), idxEx.XMax ) ;
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MINGY ), idxEx.YMin ) ;
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MAXGY ), idxEx.YMax ) ;
      end
      else begin
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MINGX ), 0 ) ;
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MAXGX ), 0 ) ;
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MINGY ), 0 ) ;
        macroTableSetField( 1, getCmd( T_SQLPGDB.ID_MAXGY ), 0 ) ;
      end ;

      oGisDb.sqlTablePost( 1 )  ;
    except

    end ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.macroUidLast
    : TGIS_Uid ;
  begin
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_MAX_GEOMETRY_OBJECTID ),
                          [ Table ]
                        ),0
                ) ;
    try
      try
        Result := VarToInt64( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_ID ),0 ) ) ;
      except
        Result := 0 ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.macroUidReserve
    : TGIS_Uid ;
  var
    iloop   : Integer ;
    uid     : TGIS_Uid ;
    fldinfo : TGIS_FieldInfo ;
    i       : Integer ;
    defval  : Variant ;
  begin
    Result := -1 ;

    if IsReadOnly then exit ;

    macroUpdateStart ;
    try
      uid := -1 ;
      PrepareExportFieldNames( StrToInt( getCmd( T_SQLPGDB.ID_MAX_NAMELENGTH ) ) ) ;

      iloop := T_SQLPGDB.PGDB_RETRY_COUNT ;
      while iloop > 0 do begin
        try
          if uid < 0 then uid := macroUidNew
                     else inc( uid ) ;

          if iloop = T_SQLPGDB.PGDB_RETRY_COUNT then
            sqlTableAppend( 0, Table, TGIS_ShapeType.Unknown ) ;

          if IsStringEmpty( nameColumnUID ) then
            macroTableSetField( 0, getCmd( T_SQLPGDB.ID_OBJECTID ) , uid )
          else
            macroTableSetField( 0, nameColumnUID, uid ) ;

          for i := 0 to Fields.Count - 1 do begin
            fldinfo := FieldInfo( i ) ;
            if not (TGIS_FieldFlags.Saveable in fldinfo.Flags) then continue ;

            if assigned( fldinfo.Rules ) then begin
              defval := TGIS_FieldRule(fldinfo.Rules).Values.DefaultValue ;

              macroTableSetField( 0, fldinfo.NewName, defval ) ;
            end ;
          end ;

          oGisDb.sqlTablePost( 0 ) ;

          iloop := 0 ;
        except
          dec( iloop ) ;
          if iloop <= 0 then raise ;
          Sleep( GetRandom( T_SQLPGDB.PGDB_RETRY_INTERVAL ) ) ;
          lastUid := GetLastUid ;
          uid := lastUid ;
        end ;
      end ;
      Result := uid ;
    finally
      macroUpdateEnd ;
    end ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.macroFetchRecord(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : Boolean ;
  var
    fetch : Boolean ;  // is fetch necessary?
  begin
    fetch := True ;

    if not oGisDb.sqlQueryEof( _cursor ) then
       if VarToInt32( sqlQueryGetGEOUID( _cursor ) ) = _uid then
          fetch := False ;

    if fetch then begin
      oGisDb.sqlQueryClose(_cursor) ;
      oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_JOIN_UID ),
                                    [ ViewFeatures,
                                      nameColumnUID,
                                      _uid
                                    ]
                                  ),_cursor
                          ) ;
    end ;

    Result := fetch ;
  end;

  function TGIS_LayerSqlPgdbAbstract.macroUidNew
    : TGIS_Uid ;
  begin
    Result := macroUidLast + 1 ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.getFieldInternal(
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

      macroFetchRecord( _uid,_cursor ) ;

      if not oGisDb.sqlQueryEof(_cursor) then begin
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

  procedure TGIS_LayerSqlPgdbAbstract.macroAddField(
    const _uidname : String         ;
    const _name    : String         ;
    const _type    : TGIS_FieldType ;
    const _width   : Integer        ;
    const _decimal : Integer
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, True, 0 ) ;
  end;

  procedure TGIS_LayerSqlPgdbAbstract.macroAddField(
    const _uidname : String         ;
    const _name    : String         ;
    const _type    : TGIS_FieldType ;
    const _width   : Integer        ;
    const _decimal : Integer        ;
    const _saved   : Boolean
  ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, _saved, 0 ) ;
  end;

  procedure TGIS_LayerSqlPgdbAbstract.macroAddField(
    const _uidname : String         ;
    const _name    : String         ;
    const _type    : TGIS_FieldType ;
    const _width   : Integer        ;
    const _decimal : Integer        ;
    const _saved   : Boolean        ;
    const _binary  : Integer
  );
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
  end;

  procedure TGIS_LayerSqlPgdbAbstract.setUp ;
  var
    ext       : TGIS_Extent ;
    shapeType : Integer ;
    v         : Variant ;
  begin
    FUseRTree := False ;

    inherited ;

    macroConnect ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_GDB_GEOM_COLUMNS_WITH_SR_TABLE ),
                          [ Table ]
                        ),0
                ) ;

    if oGisDb.sqlQueryEof(0) then begin
      oGisDb.sqlQueryClose(0) ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), Table, 0 ) ;
    end ;

    try
      {$IFDEF OXYGENE}
        shapeType := Integer(
                       oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_SHAPETYPE ),0 )
                     ) ;
      {$ELSE}
        shapeType := oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_SHAPETYPE ),0 ) ;
      {$ENDIF}

      case shapeType of
         1 : DefaultShapeType := TGIS_ShapeType.Point      ;
         2 : DefaultShapeType := TGIS_ShapeType.MultiPoint ;
         3 : DefaultShapeType := TGIS_ShapeType.Arc        ;
         4 : DefaultShapeType := TGIS_ShapeType.Polygon    ;
         9 : DefaultShapeType := TGIS_ShapeType.MultiPatch ;
       else
             DefaultShapeType := TGIS_ShapeType.Unknown ;
       end ;
    except
      DefaultShapeType := TGIS_ShapeType.Unknown ;
    end ;

    try
      srId := VarToInt32( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_GGC_SRID ),0 ) ) ;
    except
      srId := 0 ;
    end ;

    try
      falseX := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_FALSEX ),0 ) ) ;
    except
      falseX := 0 ;
    end ;

    try
      falseY := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_FALSEY ),0 ) ) ;
    except
      falseY := 0 ;
    end ;

    try
      falseZ := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_FALSEZ ),0 ) ) ;
    except
      falseZ := 0 ;
    end ;

    try
      falseM := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_FALSEM  ),0 ) ) ;
    except
      falseM := 0 ;
    end ;

    try
      scaleXY := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_XYUNITS ),0 ) ) ;
    except
      scaleXY := 1 ;
    end ;

    try
      scaleZ := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_ZUNITS  ),0 ) ) ;
    except
      scaleZ := 1 ;
    end ;

    try
      scaleM := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_MUNITS  ),0 ) ) ;
    except
      scaleM := 1 ;
    end ;

    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    try
      ext.XMin  := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_EXTENTLEFT   ),0 ) ) ;
      ext.XMax  := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_EXTENTRIGHT  ),0 ) ) ;
      ext.YMin  := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_EXTENTBOTTOM ),0 ) ) ;
      ext.YMax  := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_EXTENTTOP    ),0 ) ) ;

      Extent := ext ;
    except
      Extent := GisNoWorld ;
    end ;

    try
      idxOriginX   := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_IDXORIGINX  ),0 ) ) ;
      idxOriginY   := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_IDXORIGINY  ),0 ) ) ;
      idxGridSize  := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_IDXGRIDSIZE ),0 ) ) ;
    except
      idxOriginX   := 0 ;
      idxOriginY   := 0 ;
      idxGridSize  := 0 ;
    end ;

    v := oGisDb.sqlQueryGetField( 'HasZ',0 ) ;
    if not( VarIsNull( v ) or VarIsEmpty( v ) ) then
      if VarToBoolean( v ) then begin
        DefaultDimension := TGIS_DimensionType.XYZ  ;
        hasElevation     := True ;
        try
          ZLow  := VarToDouble( oGisDb.sqlQueryGetField( 'ZLow' ,0 ) ) ;
          ZHigh := VarToDouble( oGisDb.sqlQueryGetField( 'ZHigh',0 ) ) ;
        except
          // doesn't exist in old format
        end;
      end;

    v := oGisDb.sqlQueryGetField( 'HasM',0 ) ;
    if not( VarIsNull( v ) or VarIsEmpty( v ) ) then
      if VarToBoolean( v )  then begin
        DefaultDimension := TGIS_DimensionType.XYZM  ;
        hasMeasure     := True ;
        try
          MLow  := VarToDouble( oGisDb.sqlQueryGetField( 'MLow' ,0 ) ) ;
          MHigh := VarToDouble( oGisDb.sqlQueryGetField( 'MHigh',0 ) ) ;
        except
          // doesn't exist in old format
        end;
      end;

    v := oGisDb.sqlQueryGetField( getCmd ( T_SQLPGDB.ID_SRTEXT ),0 ) ;
    if not( VarIsNull( v ) or VarIsEmpty( v ) ) then
      SetCSByWKT( VarToString( v ) ) 
    else
      SetCSByEPSG(0) ;

    oGisDb.sqlQueryClose(0) ;

    try
      try
        oGisDb.sqlQueryOpen(
          Format( getCmd( T_SQLPGDB.ID_SELECT_COUNT_GEOMETRY_OBJECTID ),
                  [ Table ]
                ),
          0
        ) ;
        idxCount := VarToInt32( oGisDb.sqlQueryGetField( getCmd( T_SQLPGDB.ID_ID ),0 ) ) ;
      except
        idxCount := 0 ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    try
      try
        oGisDb.sqlQueryOpen(
          Format( getCmd( T_SQLPGDB.ID_SELECT_TABLE_WHERE ),
                  [ nameTableIndex, Format( 'MAXGX=%d AND MINGX=%d AND ' +
                                            'MAXGY=%d AND MINGY=%d',
                                           [ -2147483648, -2147483648,
                                             -2147483648, -2147483648 ]) ]
                ),
          0
        ) ;
        if oGisDb.sqlQueryEof(0) then
          useIndex := True
        else
          useIndex := False ;
      except
        useIndex := False ;
      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    // try to return en empty set to save the time
    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_TABLE_WHERE ),
                          [ Table,
                            Format( '%s=0', [ nameColumnUID ] )
                          ]
                        ),0
                ) ;
    {$IFNDEF OXYGENE}
      oGisDb.sqlQueryStructure( Table, nameColumnUID,  macroAddField ) ;
    {$ELSE}
      oGisDb.sqlQueryStructure( Table, nameColumnUID, @macroAddField ) ;
    {$ENDIF}
    oGisDb.sqlQueryClose(0) ;

    fixGEOUID := -1 ;

    FFileInfo := 'ESRI Personal Geodatabase (TTKLS)' ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
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
          oGisDb.sqlQueryOpen( getCmd( T_SQLPGDB.ID_SELECT_GDB_GEOM_COLUMNS ),0 ) ;
          while not oGisDb.sqlQueryEof(0) do begin
            lname := VarToString( oGisDb.sqlQueryGetFieldById( 0,0 ) ) ;
            ltype := VarToInt32(  oGisDb.sqlQueryGetFieldById( 1,0 ) ) ;

            case ltype of
               1 : defShpType := TGIS_ShapeType.Point      ;
               2 : defShpType := TGIS_ShapeType.MultiPoint ;
               3 : defShpType := TGIS_ShapeType.Arc        ;
               4 : defShpType := TGIS_ShapeType.Polygon    ;
               9 : defShpType := TGIS_ShapeType.MultiPatch ;
             else  defShpType := TGIS_ShapeType.Unknown ;
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

  procedure TGIS_LayerSqlPgdbAbstract.Build(
    const _path   : String            ;
    const _extent : TGIS_Extent       ;
    const _type   : TGIS_ShapeType    ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    ll : TGIS_LayerSqlPgdbAbstract ;
  begin
    inherited ;

    if IsReadOnly then exit ;

    if not IsStringEmpty( _path ) then begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          ll := TGIS_LayerSqlPgdbAbstract(&Class.forName(Self.Class.Name).getConstructor().newInstance());
        {$ELSE}
          ll := TGIS_LayerSqlPgdbAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
        {$ENDIF}
      {$ELSE}
        ll := TGIS_LayerSqlPgdbClass( Self.ClassType ).Create ;
      {$ENDIF}
      ll.FSQLParameters.Assign( FSQLParameters );
      ll.oGisDb.SQLExecuteEvent := oGisDb.SQLExecuteEvent ;
      try
        ll.CS := CS ;
        ll.PasswordEvent := PasswordEvent ;
        ll.Path := _path ;
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

  procedure TGIS_LayerSqlPgdbAbstract.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
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

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False  ;

    Extent   := _TGIS_Extent( _layer.Extent ) ;
    shp_type := _type ;
    first    := True  ;

    if shp_type = TGIS_ShapeType.Unknown then
      shp_type := _layer.DefaultShapeType ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;

    old_view := FViewFeatures ;
    try
      try
        macroConnect ;
        macroTableDrop ;
        macroDisconnect ;
      except
        // could be nonexisting database
      end ;

      ViewFeatures := '' ;
      Build( Path, GisExtent( 0, 0, 0, 0 ), shp_type, _layer.DefaultDimension ) ;

      macroConnect ;

      ViewFeatures := '' ;
      Fields.Clear ;
      ImportStructure( _layer ) ;

      try
        lname := StrToInt( getCmd( T_SQLPGDB.ID_MAX_NAMELENGTH ) ) ;
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
                   ( ( shp_tmp.ShapeType = shp_type   ) or
                     ( shp_type = TGIS_ShapeType.Unknown )
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
        macroMasterUpdate( Extent, shp_type, '', DefaultDimension ) ;

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

  procedure TGIS_LayerSqlPgdbAbstract.MergeLayerEx(
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
    lname      : Integer        ;
    shp        : TGIS_Shape     ;
    shp_tmp    : TGIS_Shape     ;
    shp_type   : TGIS_ShapeType ;
    first      : Boolean        ;
    shape_no   : Cardinal       ;
    end_uid    : TGIS_Uid        ;
    abort      : Boolean        ;
    eloop      : TGIS_LayerVectorEnumerator ;
  begin
    if IsReadOnly then exit ;

    if not assigned( _layer ) then exit ;

    assert( Self <> _layer ) ;
    abort   := False ;

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
        lname := StrToInt( getCmd( T_SQLPGDB.ID_MAX_NAMELENGTH ) ) ;
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
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 1 ;
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited;
    oGisDb.sqlQueryClose(0);
    oGisDb.sqlTableClose(0) ;
    oGisDb.sqlTableClose(1) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.cursorOpen
    : Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorSql)  then
        SetLength( cursorSql, Result + 1 ) ;

      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorSql[Result] ) then
          cursorSql[Result] := new T_cursorSql_LayerSqlPgdb ;
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
      cursorSql[Result].disableJoin        := False ;

      if Result > BUILTIN_CURSORS - 1 then
        oGisDb.cursorOpen( Result ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerSqlPgdbAbstract.cursorClose(
    const _cursor : Integer
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
      FreeObject( cursorSql[_cursor].currMultiPatch ) ;

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
  end;

  procedure TGIS_LayerSqlPgdbAbstract.cursorFirst(
    const _cursor      : Integer     ;
    const _viewerCS    : Boolean     ;
    const _extent      : TGIS_Extent ;
    const _query       : String      ;
    const _shape       : TGIS_Shape  ;
    const _de9im       : String      ;
    const _skipDeleted : Boolean
  ) ;
  var
    sqlquery  : String ;
    order     : String ;
    ex, idxEx : TGIS_Extent ;
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

        if GisIsWholeWorld( cursorState[ _cursor ].curRawExtent ) or
           ( idxGridSize <= 0 ) or not ( useIndex )               then
        begin

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then begin
            memGID := 0 ;

            if cursorSql[_cursor].disableJoin then
              oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_NO_JOIN ),
                                    [ ViewFeatures ]
                                  ), _cursor
                          )
            else
              oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_JOIN_NOEXT ),
                                    [ ViewFeatures, nameTableIndex, order ]
                                  ), _cursor
                          ) ;
          end
          else begin
            memGID := 0 ;

            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_JOIN_NOEXT_EX ),
                                  [ ViewFeatures, nameTableIndex,
                                    sqlquery,
                                    order
                                  ]
                                ),_cursor
                        ) ;
          end ;

        end
        else begin

          ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                                 GisExtent( -1E37, -1E37, 1E37, 1E37 )
                               ) ;
          {$IFDEF GIS_NORECORDS}
            idxEx := new TGIS_Extent ;
          {$ENDIF}
          idxEx.XMin := FloorS( ((ex.XMin - idxOriginX) / idxGridSize) ) ;
          idxEx.XMax := FloorS( ((ex.XMax - idxOriginX) / idxGridSize) ) ;
          idxEx.YMin := FloorS( ((ex.YMin - idxOriginY) / idxGridSize) ) ;
          idxEx.YMax := FloorS( ((ex.YMax - idxOriginY) / idxGridSize) ) ;

          if ( IsStringEmpty( sqlquery ) ) or cursorSql[_cursor].fullSearch then begin
            memGID := 0 ;
            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_JOIN ),
                                  [ ViewFeatures, nameTableIndex,
                                    TruncS( idxEx.XMin ),
                                    TruncS( idxEx.XMax ),
                                    TruncS( idxEx.YMin ),
                                    TruncS( idxEx.YMax )
                                  ]
                                ),_cursor
                        ) ;
          end
          else begin
            memGID := 0 ;

            oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPGDB.ID_SELECT_JOIN_EX ),
                                  [ ViewFeatures, nameTableIndex,
                                    sqlquery,
                                    TruncS( idxEx.XMin ),
                                    TruncS( idxEx.XMax ),
                                    TruncS( idxEx.YMin ),
                                    TruncS( idxEx.YMax ),
                                    order
                                  ]
                                ),_cursor
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

  procedure TGIS_LayerSqlPgdbAbstract.cursorNext(
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

          cursorSql[_cursor].currShape := nil ;

          if ( not oGisDb.sqlQueryEof(_cursor) ) and
             ( not cursorSql[_cursor].isFirst  )
          then
            oGisDb.sqlQueryMoveNext(_cursor ) ;

          cursorSql[_cursor].isFirst := False ;

          if not oGisDb.sqlQueryEof(_cursor ) then begin
            readShape( _cursor ) ;
            if cursorSql[_cursor].currShape = nil then continue ;
          end;

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

  function TGIS_LayerSqlPgdbAbstract.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorSql[_cursor].isEof ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorSql[_cursor].currShape ) then
       Result := cursorSql[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.GetShape(
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
                     Format( getCmd( T_SQLPGDB.ID_FILTER_UID_WEAK ),
                             [ GIS_FIELD_UID,
                               _uid,
                               GIS_FIELD_UID,
                               _uid + T_SQLPGDB.PGDB_GETSHAPE_FETCHED_LIMIT
                             ]
                           ),
                     nil, '', True
                   ) ;
      finally
        cursorSql[_cursor].disableJoinPrimary := False ;
      end ;

      if ( cursorShape( _cursor )     <> nil ) and
         ( cursorShape( _cursor ).Uid = _uid ) then
      begin
        Result := cursorShape( _cursor ) ;
        exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSqlPgdbAbstract.GetLastUid
    : TGIS_Uid ;
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

  function TGIS_LayerSqlPgdbAbstract.GetNewUid
    : TGIS_Uid ;
  begin
    lockThread ;
    try
      Result := macroUidReserve ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.SaveData ;
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

  procedure TGIS_LayerSqlPgdbAbstract.parseConfigLayerName ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
    {$IFNDEF OXYGENE}
      with tkn do begin
        ExecuteEx( nameLayer, ';', ' ' ) ;
        if ( ( Result.Count = 0 ) or ( IsStringEmpty( Result.Strings[ 0 ] ) ) ) then
          raise EGIS_Exception.Create( GIS_RS_ERR_BADPARAM,
                                       GIS_INI_LAYERSQL_LAYER,
                                       0
                                     ) ;

        case Result.Count of
          1 : begin
                Table   := Result[0] ;
                nameDataset  := ''        ;
              end ;
          2 : begin
                Table   := Result[0] ;
                nameDataset  := Result[1] ;
              end ;
        end ;
      end ;
    {$ELSE}
        tkn.ExecuteEx( nameLayer, ';', ' ' ) ;
        if ( ( tkn.Result.Count = 0 ) or ( IsStringEmpty( tkn.Result.Strings[ 0 ] ) ) ) then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                       GIS_INI_LAYERSQL_LAYER,
                                       0
                                     ) ;

        case tkn.Result.Count of
          1 : begin
                Table   := tkn.Result[0] ;
                nameDataset  := ''            ;
              end ;
          2 : begin
                Table   := tkn.Result[0] ;
                nameDataset  := tkn.Result[1] ;
              end ;
        end ;
    {$ENDIF}
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlPgdbAbstract.updateDialectList(
    const _name  : String ;
    const _value : String
  ) ;
  var
    idx : Integer ;
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

  procedure TGIS_LayerSqlPgdbAbstract.readShape(
    const _cursor : Integer
  ) ;
  var
    uid       : Integer    ;
    shapetype : TGIS_ShapeType ;
  begin
    uid       := VarToInt32( sqlQueryGetGEOUID(_cursor ) ) ;
    shapetype := DefaultShapeType ;

    sqlQueryGetGeometry( uid, shapetype, _cursor ) ;

    cursorSql[_cursor].currShape := getEdited( cursorSql[_cursor].currShape ) ;
  end ;

{==================================== END =====================================}
end.

