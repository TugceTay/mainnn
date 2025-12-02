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
  Encapsulation of a SQL - Sqlite file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlSqlite ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlSqlite"'}
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
    System.Variants,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerSql,
    Lider.CG.GIS.GeoLayerVector  ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerSqlSqlite = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  /// Layer that can read SQL file via Sqlite.
  /// </summary>
  TGIS_LayerSqlSqlite = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerSqlAbstract )
    protected

        /// <inheritdoc/>
        procedure sqlTablePost          ( const _id        : Integer
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlTableAppend        ( const _id        : Integer        ;
                                          const _table     : String
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlTableOpenWrite     ( const _id        : Integer        ;
                                          const _table     : String         ;
                                          const _uidCol    : String         ;
                                          const _uidVal    : TGIS_Uid
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlTableSetGeometry   ( const _id        : Integer        ;
                                          const _name      : String         ;
                                          const _shp       : TGIS_Shape
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlQueryGetGeometry   ( const _uid       : TGIS_Uid        ;
                                          const _shapetype : TGIS_ShapeType ;
                                          const _cursor    : Integer        ;
                                          const _dim       : TGIS_DimensionType
                                        ) ; override;

        /// <inheritdoc/>
        function  sqlQueryGetGEOUID     ( const _cursor    : Integer
                                        ): Variant ; override;

        /// <inheritdoc/>
        function  sqlQueryNameGEOUID    ( const _cursor    : Integer
                                        ): String  ; override;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

        /// <inheritdoc/>
        function  bindFieldInternal     ( const _name      : String         ;
                                          const _cursor    : Integer
                                        ) : Integer ; override;

        /// <inheritdoc/>
        function  getBindedFieldInternal( const _shape     : TObject        ;
                                          const _field     : Integer        ;
                                          const _cursor    : Integer
                                        ) : Variant ; override;
    protected
         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
         /// <inheritdoc/>
         constructor Create  ; override;
         /// <inheritdoc/>
         procedure Build                ( const _path      : String         ;
                                          const _extent    : TGIS_Extent    ;
                                          const _type      : TGIS_ShapeType ;
                                          const _dim       : TGIS_DimensionType
                                        ) ; override;
         /// <inheritdoc/>
         function  PreRecognize         ( const _path      : String         ;
                                          var   _new_path  : String
                                        ) : Boolean ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoDbSqlite,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlSqlite
//=============================================================================

  constructor TGIS_LayerSqlSqlite.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbSqlite.Create ;
  end ;

  procedure TGIS_LayerSqlSqlite.doDestroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlSqlite.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String ;
    const _shp  : TGIS_Shape
  ) ;
  var
    store : OleVariant ;
    {$IFDEF CLR}
      obj : System.Object ;
    {$ENDIF}
  begin
    {$IFDEF CLR}
      _shp.ExportToVAR( obj ) ;
      store := OleVariant( obj ) ;
    {$ELSE}
      _shp.ExportToVAR( store ) ;
    {$ENDIF}
    oGisDb.sqlTableSetGeometry( _id, _name, store, nil );
    store := Unassigned ;
  end ;

  procedure TGIS_LayerSqlSqlite.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    oGisDb.sqlTablePost( _id ) ;
  end ;

  procedure TGIS_LayerSqlSqlite.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
   ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table ) )
    else
      oGisDb.sqlTableAppend( _id, '' )
  end ;

  procedure TGIS_LayerSqlSqlite.sqlTableOpenWrite(
     const _id     : Integer ;
     const _table  : String  ;
     const _uidCol : String  ;
     const _uidVal : TGIS_Uid
    ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableOpenWrite( _id, prepareUpdateCommand( _table, _uidCol ) )
    else
      oGisDb.sqlTableOpenWrite( _id, '' ) ;

    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  procedure TGIS_LayerSqlSqlite.sqlQueryGetGeometry(
     const _uid       : TGIS_Uid ;
     const _shapetype : TGIS_ShapeType ;
     const _cursor    : Integer ;
     const _dim       : TGIS_DimensionType
   ) ;
  var
    {$IFDEF MANAGED}
      ptrvar : TGIS_Bytes ;
    {$ELSE}
      ptrvar : Pointer    ;
    {$ENDIF}
      size   : Integer ;
  begin
    ptrvar := oGisDb.sqlQueryGetGeomPtr( getCmdGEOMETRY, '', _cursor, size ) ;
    try
      if assigned( ptrvar ) and not assigned( cursorSql[_cursor].currShape ) then
        case _shapetype of
          TGIS_ShapeType.Point :
             begin
               cursorSql[_cursor].currPoint.Recreate     (
                  nil, ptrvar, True, _uid, self, _dim
               ) ;
               cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
             end ;
          TGIS_ShapeType.MultiPoint :
             begin
               cursorSql[_cursor].currMultipoint.Recreate(
                  nil, ptrvar, True, _uid, self, _dim
               ) ;
               cursorSql[_cursor].currShape := cursorSql[_cursor].currMultipoint ;
             end ;
          TGIS_ShapeType.Arc :
             begin
               cursorSql[_cursor].currArc.Recreate       (
                  nil, ptrvar, True, _uid, self, _dim
               ) ;
               cursorSql[_cursor].currShape := cursorSql[_cursor].currArc ;
             end ;
          TGIS_ShapeType.Polygon :
             begin
               cursorSql[_cursor].currPolygon.Recreate   (
                  nil, ptrvar, True, _uid, self, _dim
               ) ;
               cursorSql[_cursor].currShape := cursorSql[_cursor].currPolygon ;
             end ;
          TGIS_ShapeType.MultiPatch :
             begin
               cursorSql[_cursor].currMultiPatch.Recreate   (
                  nil, ptrvar, True, _uid, self, _dim
               ) ;
               cursorSql[_cursor].currShape := cursorSql[_cursor].currMultiPatch ;
             end ;
          else
             begin
               cursorSql[_cursor].currShape := nil ;
             end ;
        end ;
    finally
      oGisDb.sqlQueryUnPrepareGetGeom( _cursor ) ;
    end ;
  end ;

  function TGIS_LayerSqlSqlite.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ), _cursor
                                             );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlSqlite.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor), _cursor ) ;
  end ;

  function TGIS_LayerSqlSqlite.bindFieldInternal(
    const _name   : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _name, _cursor ) ;
  end ;

  function TGIS_LayerSqlSqlite.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp    : TGIS_Shape ;
    rebind : Boolean ;
  begin
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
  end ;

  procedure TGIS_LayerSqlSqlite.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_NATIVE, Name );
    finally
      inherited;
    end ;
  end ;

  function TGIS_LayerSqlSqlite.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_NATIVE,
                GIS_SQL_PROVIDER_SQLITE
              ) ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerSqlSqlite.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlSqlite,
                   GIS_TTKLAYER_VECTOR_FILTER,
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Server,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create,
                     TGIS_RegisteredOperationType.Merge
                   ],
                   True
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerSqlSqlite.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

