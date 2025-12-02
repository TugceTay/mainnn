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
  Encapsulation of ESRI Personal Geodatabase SQL Layer - DBX (dbExpress) access.
}

{$IFDEF DCC}
  unit GisLayerSqlPgdbDbx ;
  {$HPPEMIT '#pragma link "GisLayerSqlPgdbDbx"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit GisLayerSqlOgisDbx not supported - no DB support' }
  interface
  implementation

uses GisFunctions;
{$ELSE}

interface

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Variants,
  System.SysUtils,
  {$IFNDEF GIS_NODB}
    Data.SqlExpr,
  {$ENDIF}

  GisTypes,
  GisLayerVector,
  GisLayerSqlPgdb ;

type

  /// <summary>
  ///   Layer that can read ESRI Personal Geodatabase SQL layer via DBX (dbExpress).
  /// </summary>
  TGIS_LayerSqlPgdbDbx = class( TGIS_LayerSqlPgdbAbstract )
    private
      /// <summary>
      ///   Pointer treated as "mapped" for shape geometry.
      /// </summary>
      currPointer :
        {$IFDEF CLR}
          TGis_Bytes  ;
        {$ELSE}
          Pointer ;
        {$ENDIF}
      FGeomBuf : TBytes ;
    protected
      procedure fset_SharedConnection    ( const _obj    : TSQLConnection );
      function  fget_SharedConnection : TSQLConnection ;
    protected
      /// <inheritdoc/>
      procedure sqlQueryGetGeometry   ( const _uid    : TGIS_Uid ;
                                        const _type   : TGIS_ShapeType ;
                                        const _cursor : Integer
                                       ) ; override;

      /// <inheritdoc/>
      procedure sqlTableAppend        ( const _id     : Integer ;
                                        const _table  : String  ;
                                        const _type   : TGIS_ShapeType
                                       ) ; override;

      /// <inheritdoc/>
      procedure sqlTableOpenWrite     ( const _id     : Integer ;
                                        const _table  : String  ;
                                        const _uidCol : String  ;
                                        const _uidVal : TGIS_Uid
                                       ) ; override;

      /// <inheritdoc/>
      procedure sqlTableSetGeometry   ( const _id     : Integer ;
                                        const _name   : String  ;
                                        const _shp    : TGIS_Shape
                                       ) ; override;

      /// <inheritdoc/>
      procedure macroUpdateStart    ; override;

      /// <inheritdoc/>
      function  sqlQueryNameGEOUID    ( const _cursor : Integer
                                       ) : String ; override;

      /// <inheritdoc/>
      function  sqlQueryGetGEOUID     ( const _cursor : Integer
                                        ) : OleVariant ; override;
    protected

      /// <inheritdoc/>
      function  bindFieldInternal     ( const _field  : string ;
                                        const _cursor : Integer
                                      ) : Integer ; override;

      /// <inheritdoc/>
      function  getBindedFieldInternal( const _shape  : TObject ;
                                        const _field  : Integer ;
                                        const _cursor : Integer
                                      ) : Variant ; override;
    public
      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      destructor  Destroy ; override;

      /// <inheritdoc/>
      function  PreRecognize         ( const _path       : String ;
                                       var   _new_path   : String
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
  GisRtl,
  GisDbDbx,
  GisClasses,
  GisResource,
  GisRegistredLayers,
  GisCompression,
  GisGeometryFactory ;

//=============================================================================
// TGIS_LayerSqlPgdbDbx
//=============================================================================

  constructor TGIS_LayerSqlPgdbDbx.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbDbx.Create ;
  end ;

  destructor TGIS_LayerSqlPgdbDbx.Destroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlPgdbDbx.fset_SharedConnection(
    const _obj : TSQLConnection
  ) ;
  begin
    TGIS_DbDbx( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlPgdbDbx.fget_SharedConnection : TSQLConnection;
  begin
    Result := TGIS_DbDbx( oGisDb ).SharedConnection ;
  end ;

  procedure TGIS_LayerSqlPgdbDbx.sqlQueryGetGeometry(
    const _uid    : TGIS_Uid ;
    const _type   : TGIS_ShapeType ;
    const _cursor : Integer
  ) ;
  var
    ptrvarTmp : TGIS_Bytes ;
    buflen    : Integer ;
    size      : Integer ;
  begin
    cursorSql[_cursor].currShape := nil ;
    currPointer := oGisDb.sqlQueryGetGeomPtr( getCmdGEOMETRY, '',_cursor, size );
    try
      if ( size >=14 ) and
         ( PByte( NativeInt( currPointer ) + 12 )^ = $78 ) and
         ( PByte( NativeInt( currPointer ) + 13 )^ = $DA ) then begin
        // zlib compressed shape
        if ( PCardinal( NativeInt( currPointer ) + 8 )^ = (size-11) ) and
           ( PCardinal( NativeInt( currPointer ) + 4 )^ > 0         ) then begin
          // verify header
          DecompressDeflateMemory( Pointer(NativeInt(currPointer)+12), size-11, FGeomBuf, buflen ) ;
          ptrvarTmp := TGIS_Bytes.Create( @FGeomBuf[0], 0, buflen ) ; ;
        end
        else
          ptrvarTmp := TGIS_Bytes.Create( currPointer, 0, size ) ;
      end
      else begin
        ptrvarTmp := TGIS_Bytes.Create( currPointer, 0, size ) ;
      end;

      try
        try
          cursorSql[_cursor].currShape := TGIS_GeometryFactory.GisCreateShapeFromShapeEx( ptrvarTmp ) ;
        except
          cursorSql[_cursor].currShape := nil ;
        end ;
      finally
        FreeObject( ptrvarTmp ) ;
      end;

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
        TGIS_ShapeType.MultiPatch :
           begin
             cursorSql[_cursor].currMultiPatch.Recreate   (
                cursorSql[_cursor].currShape, nil, False, _uid, self,
                DefaultDimension
             ) ;
             FreeObject( cursorSql[_cursor].currShape ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currMultiPatch ;
           end ;
        else
           cursorSql[_cursor].currShape := nil ;
      end;
    finally
      oGisDb.sqlQueryUnPrepareGetGeom( _cursor ) ;
    end ;
  end ;

  function TGIS_LayerSqlPgdbDbx.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ),_cursor );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlPgdbDbx.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : OleVariant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor),_cursor );
  end ;

  procedure TGIS_LayerSqlPgdbDbx.sqlTableAppend(
    const _id    : Integer ;
    const _table : String  ;
    const _type  : TGIS_ShapeType
  ) ;
  begin
    oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table, _type ) );
  end ;

  procedure TGIS_LayerSqlPgdbDbx.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String ;
    const _uidVal : TGIS_Uid
  ) ;
  begin
    oGisDb.sqlTableOpenWrite( _id, prepareUpdateCommand( _table, _uidCol ) ) ;
    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  procedure TGIS_LayerSqlPgdbDbx.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String  ;
    const _shp  : TGIS_Shape
  ) ;
  var
    store   : OleVariant ;
  begin
    TGIS_GeometryFactory.GisExportGeometryToShapeEx( _shp, store ) ;

    oGisDb.sqlTableSetGeometry( _id, _name, store, nil );

    store := Unassigned ;
  end ;

  procedure TGIS_LayerSqlPgdbDbx.macroUpdateStart ;
  begin
    inherited macroUpdateStart ;

    oGisDb.sqlUpdateStart( 0, prepareSelectCommand( Table, '' ) ) ;
    oGisDb.sqlUpdateStart( 1, prepareSelectCommand( nameTableIndex, '' ) ) ;
  end ;

  function  TGIS_LayerSqlPgdbDbx.bindFieldInternal(
    const _field  : string ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _field, _cursor );
  end ;

  function TGIS_LayerSqlPgdbDbx.getBindedFieldInternal(
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
        oGisDb.sqlGetBindedField( -1, True, _cursor ); // just rebind
    end ;

    rebind := ( not Assigned( cursorSql[_cursor].currShape ) ) or
              ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
    if rebind then
      cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

    Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor );
  end ;

  function TGIS_LayerSqlPgdbDbx.PreRecognize(
    const _path     : String ;
      var _new_path : String
   ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_PERSONALGDB,
                GIS_SQL_PROVIDER_DBX
              ) ;
  end ;

initialization
  RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlPgdbDbx,
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

