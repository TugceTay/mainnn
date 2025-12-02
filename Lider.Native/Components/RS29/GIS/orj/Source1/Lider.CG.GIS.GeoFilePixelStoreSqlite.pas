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
  Encapsulation of PixelStore - Sqlite  access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFilePixelStoreSqlite ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFilePixelStoreSqlite"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF OXYGENE}
  uses
    TatukGIS.RTL;
{$ELSE}
  uses
    System.Classes,
    System.Variants,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoFilePixelStore,
    Lider.CG.GIS.GeoCsSystems;
{$ENDIF}

type

  /// <summary>
  ///   Encapsulation of PixelStore layer via Sqlite.
  /// </summary>
  TGIS_FilePixelStoreSqlite = {$IFDEF OXYGENE} public {$ENDIF}
                              class ( TGIS_FilePixelStoreAbstract )
    protected

      /// <inheritdoc/>
      function  sqlQueryGetCell       ( const _name        : String
                                      ) : TStream ; override;

      /// <inheritdoc/>
      procedure sqlTableOpenWrite     ( const _table       : String ;
                                        const _filter      : String
                                      ) ; override;

      /// <inheritdoc/>
      procedure sqlTableAppend        ( const _table       : String
                                      ) ; override;

      /// <inheritdoc/>
      procedure sqlTableSetCell       ( const _name        : String        ;
                                        const _blob        : TGIS_MemoryStream
                                      ) ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy             ; override;
    public

      /// <inheritdoc/>
      constructor Create              ; overload; override;

      /// <inheritdoc/>
      constructor Create              ( const _path        : String      ;
                                        const _ext         : TGIS_Extent ;
                                        const _width       : Integer     ;
                                        const _height      : Integer     ;
                                        const _subformat   : TGIS_LayerPixelSubFormat     ;
                                        const _ppi         : Integer     ;
                                        const _cs          : TGIS_CSCoordinateSystem
                                      ) ; overload; override;

      /// <inheritdoc/>
      function    Prerecognize        ( const _path        : String
                                      ) : Boolean ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoConfig,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoUtils,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoDbSqlite ;
{$ENDIF}

  function TGIS_FilePixelStoreSqlite.sqlQueryGetCell(
    const _name : String
  ) : TStream ;
  begin
    Result := oGisDb.sqlQueryGetBlob( _name,0 ) ;
  end ;

  procedure TGIS_FilePixelStoreSqlite.sqlTableOpenWrite(
    const _table  : String  ;
    const _filter : String
  ) ;
  begin
    oGisDb.sqlUpdateStart( 0, prepareSelectCommand( _table, _filter ) );
    oGisDb.sqlTableOpenWrite( 0, prepareUpdateCommand( _table, _filter ) );
  end ;

  procedure TGIS_FilePixelStoreSqlite.sqlTableAppend(
    const _table : String
  ) ;
  begin
    if not oGisDb.sqlTablePrepared( 0 ) then
      oGisDb.sqlTableAppend( 0, prepareAppendCommand( _table ) )
    else
      oGisDb.sqlTableAppend( 0, '' )
  end ;

  procedure TGIS_FilePixelStoreSqlite.sqlTableSetCell(
    const _name   : String ;
    const _blob   : TGIS_MemoryStream
  ) ;
  begin
    oGisDb.sqlTableSetGeometry( 0, _name, Unassigned, _blob );
  end ;

  constructor TGIS_FilePixelStoreSqlite.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbSqlite.Create ;
  end ;

  constructor TGIS_FilePixelStoreSqlite.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  var
    layername     : String   ;
    canonicalname : String   ;
    ttkpspath     : String   ;
    provider      : String   ;
    cfg           : TGIS_Config ;
  begin
    {$IFDEF OXYGENE}
      inherited Create( _path, _ext, _width, _height,
                        _subformat, _ppi, _cs
                      ) ;
    {$ENDIF}

    try
      if GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_LAYER, layername ) = GIS_SQL_PROVIDER_ADO then
      begin
        if not IsStringEmpty( layername ) then  exit ;
        if SafeFileExists( ttkpspath ) then exit ;

        ttkpspath := GetPathAbsolute( '', _path ) ;

        layername     := GetFileName( GetPathNoExt( _path ) ) ;
        canonicalname := TGIS_Utils.GisCanonicalSQLName( layername )  ;

        provider := GetPathRelative(
                      GetFilePath( ttkpspath ),
                      GetFilePath( ttkpspath ) + GIS_INI_LAYERSQL_DEFAULT_DATABASE_SQLITE
                    ) ;

        cfg := TGIS_ConfigFactory.CreateConfig( nil, _path ) ;
        try
          if cfg.ConfigFormat = TGIS_ConfigFormat.Ini then
            cfg.Section := GIS_INI_LAYER_HEADER
          else
            cfg.Section := GIS_INI_LAYERSQL_CONNECTOR ;

          cfg.WriteString( GIS_INI_LAYERSQL_STORAGE,
                           GIS_INI_LAYERSQL_PIXELSTORE2,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_LAYER,
                           canonicalname,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_DIALECT,
                           GIS_SQL_DIALECT_NAME_SQLITE,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_SQLITE,
                           provider,
                           ''
                         ) ;
          cfg.WriteString( GIS_SQL_PARAMS_ENGINEOPTIONS,
                           '16',
                           ''
                         ) ;
        finally
          cfg.Save ;
          FreeObject( cfg ) ;
        end ;

      end ;
    finally
      {$IFNDEF OXYGENE}
        inherited Create( _path, _ext, _width, _height,
                          _subformat, _ppi, _cs
                        ) ;
      {$ENDIF}
      oGisDb := TGIS_DbSqlite.Create ;

      doCreateForWrite( _path, _ext, _width, _height,
                        _subformat, _ppi, _cs
                      );
    end;
  end;

  procedure TGIS_FilePixelStoreSqlite.doDestroy ;
  begin
    FreeObject( oGisDb );

    inherited ;
  end ;

  function TGIS_FilePixelStoreSqlite.Prerecognize( const _path : String
                                                ) : Boolean ;
  begin
    Result := doPrerecognize( _path ) = GIS_SQL_PROVIDER_SQLITE ;
  end ;

//==================================== END =====================================
end.

