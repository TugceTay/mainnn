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
  Encapsulation of ESRI Personal Geodatabase SQL Layer - ADO access.

  ESRI Personal Geodatabase SQL Layer support was created by Marcin Malinowski.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlPgdbAdo ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlPgdbAdo"'}
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
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Variants,
    System.SysUtils,

    {$IFDEF ADOINTUNIT}
      AdoInt,
    {$ELSE}
      Lider.CG.GIS.GeoAdoInt,
    {$ENDIF}
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerSqlPgdb ;
{$ENDIF}

type
  {$IFNDEF GIS_NOADO}

    {#gendoc:hide}
    // Initialization section handler
    GisLayerSqlPgdbAdo = class
      public
        class procedure SelfRegisterLayer() ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read ESRI Personal Geodatabase SQL layer via ADO.
  /// </summary>
  TGIS_LayerSqlPgdbAdo = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerSqlPgdbAbstract )
    protected
        procedure fset_SharedConnection   ( const _obj : _Connection );
        function  fget_SharedConnection : _Connection ;
    private
      FGeomBuf : TBytes ;
    protected
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure sqlQueryGetGeometry   ( const _uid     : TGIS_Uid        ;
                                           const _type    : TGIS_ShapeType ;
                                           const _cursor  : Integer
                                         ) ; override;

         /// <inheritdoc/>
         procedure sqlTableAppend        ( const _id      : Integer        ;
                                           const _table   : String         ;
                                           const _type    : TGIS_ShapeType
                                         ) ; override;

         /// <inheritdoc/>
         procedure sqlTableOpenWrite     ( const _id      : Integer        ;
                                           const _table   : String         ;
                                           const _uidCol  : String         ;
                                           const _uidVal  : TGIS_Uid
                                         ) ; override;

         /// <inheritdoc/>
         procedure sqlTableSetGeometry   ( const _id      : Integer        ;
                                           const _name    : String         ;
                                           const _shp     : TGIS_Shape
                                         ) ; override;

         /// <inheritdoc/>
         function  sqlQueryNameGEOUID    ( const _cursor  : Integer
                                         ): String ; override;

         /// <inheritdoc/>
         function  sqlQueryGetGEOUID     ( const _cursor  : Integer
                                         ) : OleVariant ; override;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

         /// <inheritdoc/>
         function  bindFieldInternal     ( const _name    : String         ;
                                           const _cursor  : Integer
                                         ) : Integer ; override;

         /// <inheritdoc/>
         function  getBindedFieldInternal( const _shape   : TObject        ;
                                           const _field   : Integer        ;
                                           const _cursor  : Integer
                                         ) : Variant ; override;
    protected
         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
         /// <inheritdoc/>
         constructor Create  ; override;

         /// <inheritdoc/>
         procedure Build                 ( const _path     : String         ;
                                           const _extent   : TGIS_Extent    ;
                                           const _type     : TGIS_ShapeType ;
                                           const _dim      : TGIS_DimensionType
                                         ) ; override;
         /// <inheritdoc/>
         function  PreRecognize          ( const _path     : String         ;
                                           var   _new_path : String
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
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoDbAdo,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCompression,
    Lider.CG.GIS.GeoGeometryFactory ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlPgdbAdo
//=============================================================================

  constructor TGIS_LayerSqlPgdbAdo.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbAdo.Create ;
  end ;

  procedure TGIS_LayerSqlPgdbAdo.doDestroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlPgdbAdo.fset_SharedConnection(
    const _obj : _Connection
  ) ;
  begin
    TGIS_DbAdo( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlPgdbAdo.fget_SharedConnection : _Connection;
  begin
    Result := TGIS_DbAdo( oGisDb ).SharedConnection ;
  end ;

  procedure TGIS_LayerSqlPgdbAdo.sqlQueryGetGeometry(
    const _uid  : TGIS_Uid ;
    const _type : TGIS_ShapeType;
    const _cursor : Integer
  ) ;
  var
    {$IFDEF CLR}
      ptrvar    : TGIS_Bytes ;
    {$ELSE}
      ptrvar    : Pointer ;
    {$ENDIF}
      ptrvarTmp : TGIS_Bytes ;
      buflen    : Integer ;
      size      : Integer ;
  begin
    cursorSql[_cursor].currShape := nil ;
    ptrvar := oGisDb.sqlQueryGetGeomPtr( getCmdGEOMETRY, '',_cursor, size );
    if not assigned( ptrvar ) then begin
      cursorSql[_cursor].currShape := nil ;
      exit ;
    end ;

    try
      {$IFNDEF CLR}
        if ( size >=14 ) and
           ( PByte( NativeInt( ptrvar ) + 12 )^ = $78 ) and
           ( PByte( NativeInt( ptrvar ) + 13 )^ = $DA ) then begin
          // zlib compressed shape
          if ( PCardinal( NativeInt( ptrvar ) + 8 )^ = (size-11) ) and
             ( PCardinal( NativeInt( ptrvar ) + 4 )^ > 0         ) then begin
            // verify header
            DecompressDeflateMemory( Pointer(NativeInt(ptrvar)+12), size-11, FGeomBuf, buflen ) ;
            ptrvarTmp := TGIS_Bytes.Create( @FGeomBuf[0], 0, buflen ) ;
          end
          else
            ptrvarTmp := TGIS_Bytes.Create( ptrvar, 0, size ) ;
        end
        else begin
          ptrvarTmp := TGIS_Bytes.Create( ptrvar, 0, size ) ;
        end;
      {$ELSE}
        if ( size >=14 ) and
           ( ptrvar.Memory[12] = $78 ) and
           ( ptrvar.Memory[13] = $DA ) then begin
            // zlib compressed shape
          if ( ptrvar.ReadUInt32(8) = (size-12) ) and
             ( ptrvar.ReadUInt32(4) > 0         ) then begin
            // verify header
            DecompressDeflateBuffer( ptrvar.Memory, 12, size-12, FGeomBuf, buflen ) ;
            ptrvarTmp := TGIS_Bytes.Create( FGeomBuf, 0 );
          end
          else
            ptrvarTmp := TGIS_Bytes.Create( ptrvar.Memory, 0 )
        end
        else begin
          ptrvarTmp := TGIS_Bytes.Create( ptrvar.Memory, 0 );
        end;
      {$ENDIF}

      try
        try
          cursorSql[_cursor].currShape := TGIS_GeometryFactory.GisCreateShapeFromShapeEx( ptrvarTmp ) ;
        except
          cursorSql[_cursor].currShape := nil ;
        end ;
      finally
        FreeObject( ptrvarTmp ) ;
      end;

      if not assigned( cursorSql[_cursor].currShape ) then exit ;

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

  function TGIS_LayerSqlPgdbAdo.sqlQueryNameGEOUID(
    const _cursor : Integer
   ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ),
                                              _cursor
                                             );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlPgdbAdo.sqlQueryGetGEOUID(
    const _cursor : Integer
    ) : OleVariant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor),_cursor ) ;
  end ;

  procedure TGIS_LayerSqlPgdbAdo.sqlTableAppend(
     const _id    : Integer ;
     const _table : String  ;
     const _type  : TGIS_ShapeType
   ) ;
  begin
    oGisDb.sqlTableAppend( _id,
                           prepareSelectCommand( _table,
                                                 prepareFilterUid( _table, -1 ))
                          );
  end ;

  procedure TGIS_LayerSqlPgdbAdo.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String ;
    const _uidVal : TGIS_Uid
  ) ;
  var
    cmd : String ;
  begin
    cmd := prepareSelectCommand( _table, prepareFilterUid( _table, _uidVal ) );
    oGisDb.sqlTableOpenWrite( _id, cmd );
  end ;

  procedure TGIS_LayerSqlPgdbAdo.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String ;
    const _shp  : TGIS_Shape
  ) ;
  var
    store   : OleVariant ;
  begin
    TGIS_GeometryFactory.GisExportGeometryToShapeEx( _shp, store ) ;

    oGisDb.sqlTableSetGeometry( _id, _name, store, nil );

    store := Unassigned ;
  end ;

  function  TGIS_LayerSqlPgdbAdo.bindFieldInternal(
    const _name   : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _name, _cursor ) ;
  end ;

  function TGIS_LayerSqlPgdbAdo.getBindedFieldInternal(
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
        oGisDb.sqlGetBindedField( -1, True, _cursor ); // just rebind
    end ;

    rebind := ( not assigned( cursorSql[_cursor].currShape ) ) or
              ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
    if rebind then
      cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

    Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor );
  end ;

  procedure TGIS_LayerSqlPgdbAdo.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_PERSONALGDB, Name );
    finally
      inherited;
    end ;
  end ;

  function TGIS_LayerSqlPgdbAdo.PreRecognize(
    const _path     : String ;
    var   _new_path : String
   ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_PERSONALGDB,
                GIS_SQL_PROVIDER_ADO
              ) ;
  end ;

{$IFNDEF GIS_NOADO}
  { Perform initialization section.
  }
  class procedure GisLayerSqlPgdbAdo.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlPgdbAdo,
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
     GisLayerSqlPgdbAdo.SelfRegisterLayer() ;
  {$ENDIF}
{$ENDIF}

//==================================== END =====================================
end.

