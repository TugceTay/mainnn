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
  Encapsulation of a GeoMedia SQL Warehouse DBX (dbExpress) access.

  Implementation currently support: MS Jet (Access), MS SQL Server warehouses.
}

{$IFDEF DCC}
  unit GisLayerSqlGmDbx;
  {$HPPEMIT '#pragma link "GisLayerSqlGmDbx"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit GisLayerSqlGmDbx not supported - no DB support' }
  interface
  implementation
{$ELSE}

interface

uses
  System.Variants,
  {$IFNDEF GIS_NODB}
    Data.SqlExpr,
  {$ENDIF}

  GisTypes,
  GisLayerVector,
  GisLayerSqlGM ;

type

  /// <summary>
  ///   Encapsulation of GeoMedia SQL layer via DBX (dbExpress).
  /// </summary>
  TGIS_LayerSqlGmDbx = class( TGIS_LayerSqlGmAbstract )
    protected

        procedure fset_SharedConnection   ( const _obj      : TSQLConnection );
        function  fget_SharedConnection : TSQLConnection ;

    protected
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure sqlQueryGetGeometry ( const _uid       : TGIS_Uid ;
                                         const _shapetype : TGIS_ShapeType ;
                                         const _cursor    : Integer
                                       ) ; override;

         /// <inheritdoc/>
         procedure sqlTableAppend      ( const _id        : Integer ;
                                         const _table     : String
                                       ) ; override;

         /// <inheritdoc/>
         procedure sqlTableOpenWrite   ( const _id        : Integer ;
                                         const _table     : String  ;
                                         const _uidCol    : String  ;
                                         const _uidVal    : TGIS_Uid
                                       ) ; override;

         /// <inheritdoc/>
         function  sqlQueryNameGEOUID  ( const _cursor    : Integer
                                        ) : String ; override;

         /// <inheritdoc/>
         function  sqlQueryGetGEOUID   ( const _cursor    : Integer
                                        ) : Variant ; override;

         /// <inheritdoc/>
         procedure sqlTableSetGeometry ( const _id        : Integer ;
                                         const _name      : String  ;
                                         const _shp       : TGIS_Shape
                                       ) ; override;

         /// <inheritdoc/>
         procedure macroUpdateStart    ; override;
    protected

         /// <inheritdoc/>
         function  bindFieldInternal     ( const _field   : string ;
                                           const _cursor  : Integer
                                         ) : Integer ; override;

         /// <inheritdoc/>
         function  getBindedFieldInternal( const _shape   : TObject ;
                                           const _field   : Integer ;
                                           const _cursor  : Integer
                                         ) : Variant ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;

         /// <inheritdoc/>
         destructor  Destroy ; override;

         /// <inheritdoc/>
         function  PreRecognize      ( const _path     : String ;
                                       var  _new_path  : String
                                     ) : Boolean ; override;
    public

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : TSQLConnection read  fget_SharedConnection
                                                 write fset_SharedConnection ;
  end ;

//#############################################################################
implementation

uses
  GisRtl,
  GisDbDbx,
  GisGeometryFactory,
  GisRegistredLayers,
  GisResource;

//=============================================================================
// TGIS_LayerSqlGmDbx
//=============================================================================

  constructor TGIS_LayerSqlGmDbx.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbDbx.Create ;
  end ;

  destructor TGIS_LayerSqlGmDbx.Destroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlGmDbx.fset_SharedConnection(
    const _obj : TSQLConnection
  ) ;
  begin
    TGIS_DbDbx( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlGmDbx.fget_SharedConnection : TSQLConnection;
  begin
    Result := TGIS_DbDbx( oGisDb ).SharedConnection ;
  end ;

  procedure TGIS_LayerSqlGmDbx.sqlQueryGetGeometry(
     const _uid       : TGIS_Uid ;
     const _shapetype : TGIS_ShapeType ;
     const _cursor    : Integer
   ) ;
  var
    shp       : TGIS_Shape ;
    varStore  : OleVariant  ;
  begin
    cursorSql[_cursor].currShape := nil ;

    varStore := oGisDb.sqlQueryGetGeomVAR( getCmdGEOMETRY( 0 ), '', _cursor ) ;
    try
      shp := TGIS_GeometryFactory.GisCreateShapeFromGDO( varStore, nil, nil, False, 0, self ) ;
    except
      FreeObject( shp ) ;
    end ;

    if not Assigned( shp ) then exit ;

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

  function TGIS_LayerSqlGmDbx.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ), _cursor
                                             );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlGmDbx.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID( _cursor ), _cursor ) ;
  end ;

  procedure TGIS_LayerSqlGmDbx.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
  ) ;
  begin
    oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table ) ) ;
  end ;

  procedure TGIS_LayerSqlGmDbx.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String ;
    const _uidVal : TGIS_Uid
  ) ;
  var
    cmd : String ;
  begin
    cmd := prepareUpdateCommand( _table, _uidCol ) ;
    oGisDb.sqlTableOpenWrite( _id, cmd ) ;
    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  procedure TGIS_LayerSqlGmDbx.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String  ;
    const _shp  : TGIS_Shape
  ) ;
  var
    store : OleVariant ;
    {$IFDEF WINFORMS}
      obj : System.Object ;
    {$ENDIF}
  begin
    {$IFDEF WINFORMS}
      _shp.ExportToGDO( obj ) ;
      store := OleVariant( obj ) ;
    {$ELSE}
      _shp.ExportToGDO( store ) ;
    {$ENDIF}
    oGisDb.sqlTableSetGeometry( _id, _name, store, nil ) ;
  end ;

  procedure TGIS_LayerSqlGmDbx.macroUpdateStart ;
  begin
    inherited macroUpdateStart ;

    oGisDb.sqlUpdateStart( 0, prepareSelectCommand( Table, '' ) ) ;
  end ;

  function  TGIS_LayerSqlGmDbx.bindFieldInternal(
    const _field  : string ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _field, _cursor ) ;
  end ;

  function TGIS_LayerSqlGmDbx.getBindedFieldInternal(
     const _shape  : TObject ;
     const _field  : Integer ;
     const _cursor : Integer
   ) : Variant ;
  var
    shp     : TGIS_Shape ;
    rebind  : Boolean ;
  begin
    Result := unassigned ;

    shp := TGIS_Shape( _shape ) ;

    if not Assigned( shp ) then Exit ;

    // don't break the current cursor when Uid=-1 from MustCalculateStatistics
    if shp.Uid = -1 then Exit ;

    if shp.IsEditable then begin
      // fetch corresponding record for in-memory shapes
      if macroFetchRecord( shp.Uid, _cursor ) then
        oGisDb.sqlGetBindedField( -1, True, _cursor ) ; // just rebind
    end ;

    rebind := ( not Assigned( cursorSql[_cursor].currShape ) ) or
              ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
    if rebind then
      cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

    Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor ) ;
  end ;

  function TGIS_LayerSqlGmDbx.PreRecognize(
    const _path     : String ;
    var   _new_path : String
   ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_GEOMEDIA,
                GIS_SQL_PROVIDER_DBX
               ) ;
  end ;

initialization
  RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlGmDbx,
                 GIS_TTKLAYER_VECTOR_FILTER,
                 TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Server,
                 [ TGIS_RegisteredOperationType.Read,
                   TGIS_RegisteredOperationType.Write,
                   TGIS_RegisteredOperationType.&Create,
                   TGIS_RegisteredOperationType.Merge ],
                  True
                );
{$ENDIF} //GIS_NODB
//==================================== END =====================================
end.

