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
  Encapsulation of OpenGIS SQL - DBX (dbExpress) file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqloGisDbx ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqloGisDbx"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit Lider.CG.GIS.GeoLayerSqloGisDbx not supported - no DB support' }
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
  Lider.CG.GIS.GeoLayerSqlOgis ;

type

  /// <summary>
  ///   Layer that can read OpenGIS SQL file via DBX (dbExpress).
  /// </summary>
  TGIS_LayerSqloGisDbx = class( TGIS_LayerSqlOgisAbstract )
    protected

      procedure fset_SharedConnection   ( const _obj : TSQLConnection );
      function  fget_SharedConnection : TSQLConnection ;

    protected
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure sqlTableAppend      ( const _id     : Integer ;
                                         const _table  : String
                                       ) ; override;

         /// <inheritdoc/>
         procedure sqlTableOpenWrite   ( const _id     : Integer ;
                                         const _table  : String  ;
                                         const _uidCol : String  ;
                                         const _uidVal : TGIS_Uid
                                       ) ; override;

         /// <inheritdoc/>
         function  sqlQueryNameGEOUID  ( const _cursor  : Integer
                                        ) : String ; override;

         /// <inheritdoc/>
         function  sqlQueryGetGEOUID   ( const _cursor  : Integer
                                        ) : Variant ; override;

         /// <inheritdoc/>
         procedure macroUpdateStart    ; override;
    protected

         /// <inheritdoc/>
         function  bindFieldInternal     ( const _field    : string ;
                                           const _cursor   : Integer
                                         ) : Integer ; override;

         /// <inheritdoc/>
         function  getBindedFieldInternal( const _shape    : TObject ;
                                           const _field    : Integer ;
                                           const _cursor   : Integer
                                         ) : Variant ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;

         /// <inheritdoc/>
         destructor  Destroy ; override;

         /// <inheritdoc/>
         function  PreRecognize         ( const _path       : String ;
                                            var _new_path   : String
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
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoRegistredLayers,
  Lider.CG.GIS.GeoResource;

//=============================================================================
// TGIS_LayerSqloGisDbx
//=============================================================================

  constructor TGIS_LayerSqloGisDbx.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbDbx.Create ;
  end ;

  destructor TGIS_LayerSqloGisDbx.Destroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqloGisDbx.fset_SharedConnection(
    const _obj : TSQLConnection
  ) ;
  begin
    TGIS_DbDbx( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqloGisDbx.fget_SharedConnection : TSQLConnection;
  begin
    Result := TGIS_DbDbx( oGisDb ).SharedConnection ;
  end ;

  function TGIS_LayerSqloGisDbx.sqlQueryNameGEOUID(
    const _cursor : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ),
                                              _cursor
                                             ) ;
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqloGisDbx.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor),_cursor ) ;
  end ;

  procedure TGIS_LayerSqloGisDbx.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
  ) ;
  begin
    oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table ) );
  end ;

  procedure TGIS_LayerSqloGisDbx.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String ;
    const _uidVal : TGIS_Uid
  ) ;
  var
    cmd : String ;
  begin
    cmd := prepareUpdateCommand( _table, _uidCol );
    oGisDb.sqlTableOpenWrite( _id, cmd );
    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  procedure TGIS_LayerSqloGisDbx.macroUpdateStart ;
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

  function  TGIS_LayerSqloGisDbx.bindFieldInternal(
    const _field  : string ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _field, _cursor ) ;
  end ;

  function TGIS_LayerSqloGisDbx.getBindedFieldInternal(
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
        oGisDb.sqlGetBindedField( -1, True, _cursor ); // just rebind
    end ;

    rebind := ( not Assigned( cursorSql[_cursor].currShape ) ) or
              ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
    if rebind then
      cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

    Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor );
  end ;

  function TGIS_LayerSqloGisDbx.PreRecognize(
    const _path     : String ;
    var   _new_path : String
   ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISBLOB,
                                   GIS_SQL_PROVIDER_DBX
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISWKT,
                                   GIS_SQL_PROVIDER_DBX
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISBLOB2,
                                   GIS_SQL_PROVIDER_DBX
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISNORMALIZED,
                                   GIS_SQL_PROVIDER_DBX
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISNORMALIZED2,
                                   GIS_SQL_PROVIDER_DBX
                                  ) ;
  end ;

initialization
  RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqloGisDbx,
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

