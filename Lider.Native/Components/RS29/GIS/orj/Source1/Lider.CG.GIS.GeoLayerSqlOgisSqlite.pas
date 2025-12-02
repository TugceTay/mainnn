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
  Encapsulation of OpenGIS SQL - Sqlite file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlOgisSqlite ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlOgisSqlite"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
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
    Lider.CG.GIS.GeoLayerSqlOgis ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerSqlOgisSqlite = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Layer that can read OpenGIS SQL file via Sqlite.
  /// </summary>
  TGIS_LayerSqlOgisSqlite = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerSqlOgisAbstract )
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
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

         /// <inheritdoc/>
         function  bindFieldInternal     ( const _name     : String  ;
                                           const _cursor   : Integer
                                         ) : Integer ; override;

         /// <inheritdoc/>
         function  getBindedFieldInternal( const _shape    : TObject ;
                                           const _field    : Integer ;
                                           const _cursor   : Integer
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
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlOgisSqlite
//=============================================================================

  constructor TGIS_LayerSqlOgisSqlite.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbSqlite.Create ;
  end ;

  procedure TGIS_LayerSqlOgisSqlite.doDestroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  function TGIS_LayerSqlOgisSqlite.sqlQueryNameGEOUID(
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

  function TGIS_LayerSqlOgisSqlite.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor),_cursor ) ;
  end ;

  procedure TGIS_LayerSqlOgisSqlite.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
  ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableAppend( _id, prepareAppendCommand( _table ) )
    else
      oGisDb.sqlTableAppend( _id, '' )
  end ;

  procedure TGIS_LayerSqlOgisSqlite.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String ;
    const _uidVal : TGIS_Uid
  ) ;
  begin
    if not oGisDb.sqlTablePrepared( _id ) then
      oGisDb.sqlTableOpenWrite( _id, prepareUpdateCommand( _table, _uidCol ) )
    else
      oGisDb.sqlTableOpenWrite( _id, '' ) ;

    oGisDb.sqlTableSetField( _id, _uidCol, _uidVal, 1 ) ;
  end ;

  function  TGIS_LayerSqlOgisSqlite.bindFieldInternal(
    const _name  : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _name, _cursor ) ;
  end ;

  function TGIS_LayerSqlOgisSqlite.getBindedFieldInternal(
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

  procedure TGIS_LayerSqlOgisSqlite.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_OPENGISBLOB, Name );
    finally
      inherited;
    end ;
  end ;

  function TGIS_LayerSqlOgisSqlite.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISBLOB,
                                   GIS_SQL_PROVIDER_SQLITE
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISWKT,
                                   GIS_SQL_PROVIDER_SQLITE
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISBLOB2,
                                   GIS_SQL_PROVIDER_SQLITE
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISNORMALIZED,
                                   GIS_SQL_PROVIDER_SQLITE
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISNORMALIZED2,
                                   GIS_SQL_PROVIDER_SQLITE
                                  ) ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerSqlOgisSqlite.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlOgisSqlite,
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
    GisLayerSqlOgisSqlite.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

