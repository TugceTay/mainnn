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
  Encapsulation of a GeoMedia SQL Warehouse ADO access.

  Implementation currently support: MS Jet (Access), MS SQL Server warehouses.

  GeoMedia SQL Layer support was created by Marcin Malinowski.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlGmAdo ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlGmAdo"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NOADO}
  {$Message Error 'Unit not supported - no ADO support' }
{$ENDIF}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    ADODB ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Variants,
    {$IFDEF ADOINTUNIT}
      AdoInt,
    {$ELSE}
      Lider.CG.GIS.GeoAdoInt,
    {$ENDIF}

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerSqlGm ;
{$ENDIF}

type
  {$IFNDEF GIS_NOADO}

    {#gendoc:hide}
    // Initialization section handler
    GisLayerSqlGmAdo = class
      public
        class procedure SelfRegisterLayer() ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of GeoMedia SQL layer via ADO.
  /// </summary>
  TGIS_LayerSqlGmAdo = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerSqlGmAbstract )
    protected
        procedure fset_SharedConnection   ( const _obj : _Connection );
        function  fget_SharedConnection : _Connection ;
    protected
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure sqlQueryGetGeometry   ( const _uid       : TGIS_Uid        ;
                                           const _shapetype : TGIS_ShapeType ;
                                           const _cursor    : Integer
                                         ) ; override;

         /// <inheritdoc/>
         function  sqlQueryNameGEOUID    ( const _cursor    : Integer
                                         ): String ; override;

         /// <inheritdoc/>
         function  sqlQueryGetGEOUID     ( const _cursor    : Integer
                                         ): Variant ; override;

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
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;

      // new layer builder

         /// <inheritdoc/>
         procedure Build                 ( const _path      : String         ;
                                           const _extent    : TGIS_Extent    ;
                                           const _type      : TGIS_ShapeType ;
                                           const _dim       : TGIS_DimensionType
                                         ) ; override;

         /// <inheritdoc/>
         function  PreRecognize          ( const _path      : String         ;
                                           var   _new_path  : String
                                         ) : Boolean ; override;
    public

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : _Connection    read  fget_SharedConnection
                                                 write fset_SharedConnection ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoDbAdo,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoGeometryFactory,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlGmAdo
//=============================================================================

  constructor TGIS_LayerSqlGmAdo.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbAdo.Create ;
    canSkipField := True ;
  end ;

  procedure TGIS_LayerSqlGmAdo.doDestroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlGmAdo.fset_SharedConnection(
    const _obj : _Connection
  ) ;
  begin
    TGIS_DbAdo( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlGmAdo.fget_SharedConnection : _Connection;
  begin
    Result := TGIS_DbAdo( oGisDb ).SharedConnection ;
  end ;

  procedure TGIS_LayerSqlGmAdo.sqlQueryGetGeometry(
    const _uid       : TGIS_Uid ;
    const _shapetype : TGIS_ShapeType ;
    const _cursor    : Integer
  ) ;
  var
    shp       : TGIS_Shape ;
    varStore  : OleVariant  ;
  begin
    cursorSql[_cursor].currShape := nil ;

    varStore := oGisDb.sqlQueryGetGeomVAR( getCmdGEOMETRY(0), getCmdGEOUID(1), _cursor ) ;

    try
      if not ( VarIsNull( varStore ) or VarIsEmpty( varStore ) ) then
        shp := TGIS_GeometryFactory.GisCreateShapeFromGDO( varStore, nil, nil, False, 0, self )
      else
        shp := nil ;
    except
      FreeObject( shp ) ;
    end ;

    if not assigned( shp ) then exit ;

    case shp.ShapeType of
      TGIS_ShapeType.Point :
         begin
           cursorSql[_cursor].currPoint.Recreate     ( shp, nil, True, _uid, self ) ;
           FreeObject( shp ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
         end ;
      TGIS_ShapeType.MultiPoint :
         begin
           cursorSql[_cursor].currMultipoint.Recreate( shp, nil, True, _uid, self ) ;
           FreeObject( shp ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currMultipoint ;
         end ;
      TGIS_ShapeType.Arc :
         begin
           cursorSql[_cursor].currArc.Recreate       ( shp, nil, True, _uid, self ) ;
           FreeObject( shp ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currArc ;
         end ;
      TGIS_ShapeType.Polygon :
         begin
           cursorSql[_cursor].currPolygon.Recreate   ( shp, nil, True, _uid, self ) ;
           FreeObject( shp ) ;
           cursorSql[_cursor].currShape := cursorSql[_cursor].currPolygon ;
         end ;
      else
         begin
           cursorSql[_cursor].currShape := nil ;
         end ;
    end ;

  end ;

  function TGIS_LayerSqlGmAdo.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ), _cursor
                                             );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlGmAdo.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor), _cursor ) ;
  end ;

  procedure TGIS_LayerSqlGmAdo.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
   ) ;
  begin
    oGisDb.sqlTableAppend( _id,
                           prepareSelectCommand( _table, prepareFilterUid(-1))
                          );
  end ;

  procedure TGIS_LayerSqlGmAdo.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String ;
    const _uidVal : TGIS_Uid
  ) ;
  var
    cmd : String ;
  begin
    cmd := prepareSelectCommand( _table, prepareFilterUid( _uidVal ) );
    oGisDb.sqlTableOpenWrite( _id, cmd );
  end ;

  procedure TGIS_LayerSqlGmAdo.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String ;
    const _shp  : TGIS_Shape
  ) ;
  var
    {$IFDEF CLR}
      obj : System.Object ;
    {$ENDIF}
    store : OleVariant ;
  begin
    {$IFDEF CLR}
      _shp.ExportToGDO( obj ) ;
      store := OleVariant( obj ) ;
    {$ELSE}
      _shp.ExportToGDO( store ) ;
    {$ENDIF}

    oGisDb.sqlTableSetGeometry( _id, _name, store, nil );
    store := Unassigned ;
  end ;

  function  TGIS_LayerSqlGmAdo.bindFieldInternal(
    const _name   : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _name, _cursor ) ;
  end ;

  function TGIS_LayerSqlGmAdo.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp     : TGIS_Shape ;
    rebind  : Boolean ;
  begin
    Result := Unassigned ;

    shp := TGIS_Shape( _shape ) ;

    if not assigned( shp ) then Exit ;

    if shp.IsEditable then begin
      // fetch corresponding record for in-memory shapes
      if macroFetchRecord( shp.Uid, _cursor ) then
        oGisDb.sqlGetBindedField( -1, True, _cursor ) ; // just rebind
    end ;

    rebind := ( not assigned( cursorSql[_cursor].currShape ) ) or
              ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
    if rebind then
      cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

    Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor ) ;
  end ;

  procedure TGIS_LayerSqlGmAdo.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_GEOMEDIA, Name );
    finally
      inherited;
    end ;
  end ;

  function TGIS_LayerSqlGmAdo.PreRecognize(
    const _path     : String ;
    var   _new_path : String
   ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_GEOMEDIA,
                GIS_SQL_PROVIDER_ADO
              ) ;
  end ;

{$IFNDEF GIS_NOADO}
  { Perform initialization section.
  }
  class procedure GisLayerSqlGmAdo.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlGmAdo,
                   GIS_TTKLAYER_VECTOR_FILTER,
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Server,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create,
                     TGIS_RegisteredOperationType.Merge ],
                   True
                 ) ;
  end ;

  {$IFNDEF OXYGENE}
    initialization
      GisLayerSqlGmAdo.SelfRegisterLayer() ;
  {$ENDIF}
{$ENDIF}

//==================================== END =====================================
end.

