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
  Encapsulation of a PixelStore - DBX (dbExpress) file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerPixelStoreDbx;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerPixelStoreDbx"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit Lider.CG.GIS.GeoLayerPixelStoreDbx not supported - no DB support' }
  interface
  implementation
{$ELSE}

interface

uses
  Lider.CG.GIS.GeoLayerPixelStore ;

type

  /// <summary>
  ///   Encapsulation of PixelStore layer via DBX (dbExpress).
  /// </summary>
  TGIS_LayerPixelStoreDbx = class( TGIS_LayerPixelStoreAbstract )
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

uses
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoRegistredLayers,
  Lider.CG.GIS.GeoFilePixelStoreDbx;

//==============================================================================
// TGIS_LayerPixelStoreDbx
//==============================================================================

  constructor TGIS_LayerPixelStoreDbx.Create ;
  begin
    inherited ;

    FStore := TGIS_FilePixelStoreDbx.Create ;
    FStore.GIS_Layer := self ;
  end ;

  function TGIS_LayerPixelStoreDbx.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := Store.Prerecognize( _path ) ;
  end ;

initialization
  RegisterLayer( 'DK-TTKPS', 'TatukGIS PixelStore', TGIS_LayerPixelStoreDbx,
                 GIS_TTKLAYER_PIXEL_FILTER,
                 TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Server,
                 [ TGIS_RegisteredOperationType.Read,
                   TGIS_RegisteredOperationType.Write,
                   TGIS_RegisteredOperationType.&Create ],
                  True
                );
{$ENDIF} //GIS_NODB
{==================================== END =====================================}
end.

