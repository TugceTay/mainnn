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
  unit Lider.CG.GIS.GeoLayerSqlGpkgSqlite ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlGpkgSqlite"'}
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
    System.SysUtils,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerSqlGpkg ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerSqlGpkgSqlite = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  /// Layer that can read GeoPackage via Sqlite.
  /// </summary>
  TGIS_LayerSqlGpkgSqlite = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_LayerSqlGpkgAbstract )
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

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoDbSqlite,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlGpkgSqlite
//=============================================================================

  constructor TGIS_LayerSqlGpkgSqlite.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbSqlite.Create ;
  end ;

  procedure TGIS_LayerSqlGpkgSqlite.doDestroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlGpkgSqlite.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_GPKG, Name );
    finally
      inherited;
    end ;
  end ;

  function TGIS_LayerSqlGpkgSqlite.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_GPKG,
                GIS_SQL_PROVIDER_SQLITE
              ) ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerSqlGpkgSqlite.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlGpkgSqlite,
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
    GisLayerSqlGpkgSqlite.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

