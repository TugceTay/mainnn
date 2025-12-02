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
  Encapsulation of a SQL - Sqlite file access.
}

{$IFDEF DCC}
  unit GisLayerSqlGpkgSqlite ;
  {$HPPEMIT '#pragma link "GisLayerSqlGpkgSqlite"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Variants,
    System.SysUtils,

    GisTypes,
    GisLayerSqlGpkg ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerSqlGpkgSqlite = class
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
    GisRtl,
    GisDbSqlite,
    GisResource,
    GisRegistredLayers ;
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
  class procedure Unit_GisLayerSqlGpkgSqlite.SelfRegisterLayer() ;
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
    Unit_GisLayerSqlGpkgSqlite.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

