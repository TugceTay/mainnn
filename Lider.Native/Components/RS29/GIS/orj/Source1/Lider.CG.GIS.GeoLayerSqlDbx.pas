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
  Encapsulation of a SQL - DBX (dbExpress) file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlDbx ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlDbx"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit Lider.CG.GIS.GeoLayerSqlDbx not supported - no DB support' }
  interface
  implementation
{$ELSE}

interface

uses
  System.Variants,

  {$IFNDEF GIS_NODB}
    Data.SqlExpr,
  {$ENDIF}

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoLayerSql ;

type

  /// <summary>
  ///   Layer that can read SQL file via DBX (dbExpress).
  /// </summary>
  TGIS_LayerSqlDbx = class( TGIS_LayerSqlAbstract )
    protected

        procedure fset_SharedConnection   ( const _obj : TSQLConnection );
        function  fget_SharedConnection : TSQLConnection ;

    protected
      // for internal use of TGIS_Viewer

        /// <inheritdoc/>
        procedure sqlTableAppend         ( const _id        : Integer ;
                                           const _table     : String
                                          ) ; override;

        /// <inheritdoc/>
        procedure sqlTableSetGeometry    ( const _id        : Integer ;
                                           const _name      : String ;
                                           const _shp       : TGIS_Shape
                                          ) ; override;

        /// <inheritdoc/>
        procedure sqlQueryGetGeometry    ( const _uid       : TGIS_Uid ;
                                           const _shapetype : TGIS_ShapeType ;
                                           const _cursor    : Integer ;
                                           const _dim       : TGIS_DimensionType
                                         ) ; override;

        /// <inheritdoc/>
        procedure sqlTablePost           ( const _id        : Integer
                                          ) ; override;

        /// <inheritdoc/>
        procedure sqlTableOpenWrite      ( const _id        : Integer ;
                                           const _table     : String  ;
                                           const _uidCol    : String  ;
                                           const _uidVal    : TGIS_Uid
                                          ) ; override;

        /// <inheritdoc/>
        function  sqlQueryGetGEOUID      ( const _cursor    : Integer
                                          ) : Variant ; override;

        /// <inheritdoc/>
        function  sqlQueryNameGEOUID     ( const _cursor    : Integer
                                          ) : String  ; override;

        /// <inheritdoc/>
        procedure macroUpdateStart    ; override;
    protected

         /// <inheritdoc/>
         function  bindFieldInternal     ( const _field     : string ;
                                           const _cursor    : Integer
                                          ) : Integer ; override;

         /// <inheritdoc/>
         function  getBindedFieldInternal( const _shape     : TObject ;
                                           const _field     : Integer ;
                                           const _cursor    : Integer
                                          ) : Variant ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;

         /// <inheritdoc/>
         destructor  Destroy ; override;

         /// <inheritdoc/>
         function  PreRecognize     ( const _path      : String ;
                                        var _new_path  : String
                                    ) : Boolean ; override;
    public

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : TSQLConnection read  fget_SharedConnection
                                                 write fset_SharedConnection ;
  end ;

//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoDbDbx,
  Lider.CG.GIS.GeoRegistredLayers,
  Lider.CG.GIS.GeoResource;

//=============================================================================
// TGIS_LayerSqlDbx
//=============================================================================

  constructor TGIS_LayerSqlDbx.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DBDBX.Create ;
  end ;

  destructor TGIS_LayerSqlDbx.Destroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlDbx.fset_SharedConnection(
    const _obj : TSQLConnection
  ) ;
  begin
    TGIS_DBDBX( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlDbx.fget_SharedConnection : TSQLConnection;
  begin
    Result := TGIS_DBDBX( oGisDb ).SharedConnection ;
  end ;

  procedure TGIS_LayerSqlDbx.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    oGisDb.sqlTablePost( _id ) ;
  end ;

  procedure TGIS_LayerSqlDbx.macroUpdateStart ;
  begin
    inherited macroUpdateStart ;

    oGisDb.sqlUpdateStart(
      0,
      prepareSelectCommand( TableGeometry, prepareFilterUid(-1) )
    ) ;
    oGisDb.sqlUpdateStart(
      1,
      prepareSelectCommand( TableFeatures, prepareFilterUid(-1) )
     ) ;
  end ;

  function TGIS_LayerSqlDbx.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor ), _cursor );
  end ;

  procedure TGIS_LayerSqlDbx.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String  ;
    const _shp  : TGIS_Shape
  ) ;
  var
    {$IFDEF WINFORMS}
      obj : System.Object ;
    {$ENDIF}
    store : OleVariant ;
  begin
    {$IFDEF WINFORMS}
      _shp.ExportToVAR( obj ) ;
      store := OleVariant( obj ) ;
    {$ELSE}
      _shp.ExportToVAR( store ) ;
    {$ENDIF}
    oGisDb.sqlTableSetGeometry( _id, _name, store, nil ) ;
  end ;

  procedure TGIS_LayerSqlDbx.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
  ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table ) )
    else
      oGisDb.sqlTableAppend( _id, '' )
  end ;

  procedure TGIS_LayerSqlDbx.sqlQueryGetGeometry(
    const _uid       : TGIS_Uid ;
    const _shapetype : TGIS_ShapeType ;
    const _cursor    : Integer ;
    const _dim       : TGIS_DimensionType
  ) ;
  var
  {$IFDEF CLR}
    currPointer : TGis_Bytes  ;
  {$ELSE}
    currPointer : Pointer ;
      size   : Integer ;
  {$ENDIF}
  begin
    currPointer := oGisDb.sqlQueryGetGeomPtr( getCmdGEOMETRY, '', _cursor, size ) ;
    try
      case _shapetype of
        TGIS_ShapeType.Point :
           begin
             cursorSql[_cursor].currPoint.Recreate     (
                nil, currPointer, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
           end ;
        TGIS_ShapeType.MultiPoint :
           begin
             cursorSql[_cursor].currMultipoint.Recreate(
                nil, currPointer, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currMultipoint ;
           end ;
        TGIS_ShapeType.Arc :
           begin
             cursorSql[_cursor].currArc.Recreate       (
                nil, currPointer, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currArc ;
           end ;
        TGIS_ShapeType.Polygon :
           begin
             cursorSql[_cursor].currPolygon.Recreate   (
                nil, currPointer, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currPolygon ;
           end ;
        TGIS_ShapeType.MultiPatch :
           begin
             cursorSql[_cursor].currMultiPatch.Recreate   (
                nil, currPointer, True, _uid, self, _dim
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

  function TGIS_LayerSqlDbx.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ), _cursor );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  procedure TGIS_LayerSqlDbx.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String  ;
    const _uidVal : TGIS_Uid
  ) ;
  begin
    oGisDb.sqlTableOpenWrite( _id, prepareUpdateCommand( _table, _uidCol ) );
    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  function  TGIS_LayerSqlDbx.bindFieldInternal(
    const _field  : String ;
    const _cursor : Integer
   ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _field, _cursor ) ;
  end ;

  function TGIS_LayerSqlDbx.getBindedFieldInternal(
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

  function TGIS_LayerSqlDbx.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_NATIVE,
                GIS_SQL_PROVIDER_DBX
              ) ;
  end ;

initialization
  RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlDbx,
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

