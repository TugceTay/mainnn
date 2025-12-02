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
  Encapsulation of OpenGIS SQL - ADO access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlOgisAdo ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlOgisAdo"'}
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
    Lider.CG.GIS.GeoLayerSqlOgis ;
{$ENDIF}

type
  {$IFNDEF GIS_NOADO}

    {#gendoc:hide}
    // Initialization section handler
    GisLayerSqlOgisAdo = class
      public
        class procedure SelfRegisterLayer() ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read OpenGIS SQL file via ADO.
  /// </summary>
  TGIS_LayerSqlOgisAdo = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerSqlOgisAbstract )
    protected

        procedure fset_SharedConnection   ( const _obj : _Connection );
        function  fget_SharedConnection : _Connection ;
    private
        /// <summary>
        ///   Is record in Append mode.
        /// </summary>
        isAppendMode : Boolean ;
    protected
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure sqlTablePost          ( const _id        : Integer
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
         /// <inheritdoc/>
         constructor Create  ; override;

         /// <inheritdoc/>
         procedure Build                ( const _path       : String         ;
                                          const _extent     : TGIS_Extent    ;
                                          const _type       : TGIS_ShapeType ;
                                          const _dim        : TGIS_DimensionType
                                        ) ; override;

         /// <inheritdoc/>
         function  PreRecognize         ( const _path       : String         ;
                                          var   _new_path   : String
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
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlOgisAdo
//=============================================================================

  constructor TGIS_LayerSqlOgisAdo.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbAdo.Create ;
    canSkipField := True ;
  end ;

  procedure  TGIS_LayerSqlOgisAdo.doDestroy ;
  begin
    FreeObject( oGisDb );

    inherited ;
  end ;

  procedure TGIS_LayerSqlOgisAdo.fset_SharedConnection(
    const _obj : _Connection
  ) ;
  begin
    TGIS_DbAdo( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlOgisAdo.fget_SharedConnection : _Connection;
  begin
    Result := TGIS_DbAdo( oGisDb ).SharedConnection ;
  end ;

  function TGIS_LayerSqlOgisAdo.sqlQueryNameGEOUID(
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

  function TGIS_LayerSqlOgisAdo.sqlQueryGetGEOUID(
    const _cursor : Integer
    ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor),_cursor ) ;
  end ;

  procedure TGIS_LayerSqlOgisAdo.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
  ) ;
  begin
    isAppendMode := True ;

    if oGisDb.UseTextParameters then begin
      prepareParamsCommand( _table, oGisDb.sqlGetParams( _id ) ) ;
    end
    else begin
      oGisDb.sqlTableAppend( _id,
                             prepareSelectCommand( _table, prepareFilterUid(-1))
                            );
    end;
  end ;

  procedure TGIS_LayerSqlOgisAdo.sqlTableOpenWrite( const _id     : Integer ;
                                                    const _table  : String  ;
                                                    const _uidCol : String ;
                                                    const _uidVal : TGIS_Uid
                                                  ) ;
  var
    cmd : String ;
  begin
    isAppendMode := False ;

    if oGisDb.UseTextParameters then begin
      prepareParamsCommand( _table, oGisDb.sqlGetParams( _id ) ) ;
      macroTableSetField( _id, _uidCol, _uidVal, True ) ;
    end
    else begin
      cmd := prepareSelectCommand( _table, prepareFilterUid( _uidVal ) );
      oGisDb.sqlTableOpenWrite( _id, cmd );
    end;
  end ;

  function  TGIS_LayerSqlOgisAdo.bindFieldInternal(
    const _name   : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _name, _cursor ) ;
  end ;

  function TGIS_LayerSqlOgisAdo.getBindedFieldInternal(
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

  procedure TGIS_LayerSqlOgisAdo.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_OPENGISBLOB, Name );
    finally
      inherited ;
    end ;
  end ;

  procedure TGIS_LayerSqlOgisAdo.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    if oGisDb.UseTextParameters then begin
      if ( isAppendMode ) then
        if ( _id = 0 ) then
          oGisDb.sqlExec( prepareAppendCommandEx( TableGeometry,
                                                  oGisDb.sqlGetParams( _id ) ) )
        else
          oGisDb.sqlExec( prepareAppendCommandEx( TableFeatures,
                                                  oGisDb.sqlGetParams( _id ) ) )
      else
        if ( _id = 0 ) then
          oGisDb.sqlExec( prepareUpdateCommandEx( TableGeometry,
                                                  oGisDb.sqlGetParams( _id ) ) )
        else
          oGisDb.sqlExec( prepareUpdateCommandEx( TableFeatures,
                                                  oGisDb.sqlGetParams( _id ) ) ) ;
    end
    else begin
      oGisDb.sqlTablePost( _id ) ;
    end ;
  end ;


  function TGIS_LayerSqlOgisAdo.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISBLOB,
                                   GIS_SQL_PROVIDER_ADO
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISWKT,
                                   GIS_SQL_PROVIDER_ADO
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISBLOB2,
                                   GIS_SQL_PROVIDER_ADO
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISNORMALIZED,
                                   GIS_SQL_PROVIDER_ADO
                                  ) or
              oGisDb.PreRecognize( _path,
                                   GIS_INI_LAYERSQL_OPENGISNORMALIZED2,
                                   GIS_SQL_PROVIDER_ADO
                                  ) ;
  end ;

{$IFNDEF GIS_NOADO}
  { Perform initialization section.
  }
  class procedure GisLayerSqlOgisAdo.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlOgisAdo,
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
      GisLayerSqlOgisAdo.SelfRegisterLayer() ;
  {$ENDIF}
{$ENDIF}

//==================================== END =====================================
end.

