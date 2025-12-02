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
  Encapsulation of a PixelStore - Sqlite file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerPixelStoreSqlite ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerPixelStoreSqlite"'}
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
    Lider.CG.GIS.GeoLayerPixelStore ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerPixelStoreSqlite = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of PixelStore layer via Sqlite.
  /// </summary>
  TGIS_LayerPixelStoreSqlite = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_LayerPixelStoreAbstract )
    public // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  PreRecognize          ( const _path       : String ;
                                        var   _new_path   : String
                                      ) : Boolean ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoFilePixelStoreSqlite ;
{$ENDIF}

//==============================================================================
// TGIS_LayerPixelStoreSqlite
//==============================================================================

  constructor TGIS_LayerPixelStoreSqlite.Create ;
  begin
    inherited ;

    FStore := TGIS_FilePixelStoreSqlite.Create ;
    FStore.GIS_Layer := self ;
  end ;

  function TGIS_LayerPixelStoreSqlite.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := Store.Prerecognize( _path ) ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerPixelStoreSqlite.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKPS', 'TatukGIS PixelStore', TGIS_LayerPixelStoreSqlite,
                   GIS_TTKLAYER_PIXEL_FILTER,
                   TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Server,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   True
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerPixelStoreSqlite.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.


