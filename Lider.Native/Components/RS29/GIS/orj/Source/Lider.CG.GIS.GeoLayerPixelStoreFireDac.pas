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
  Encapsulation of a PixelStore - FireDAC data access.
}

{$IFDEF DCC}
  unit GisLayerPixelStoreFireDac;
  {$HPPEMIT '#pragma link "GisLayerPixelStoreFireDac"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit GisLayerPixelStoreFireDac not supported - no DB support' }
  interface
  implementation
{$ELSE}

interface

uses
  GisLayerPixelStore ;

type

  /// <summary>
  ///   Encapsulation of PixelStore layer via FireDAC.
  /// </summary>
  TGIS_LayerPixelStoreFireDac = class( TGIS_LayerPixelStoreAbstract )
    public // constructors

      /// <inheritdoc/>
      constructor Create              ; override;

      /// <inheritdoc/>
      function  PreRecognize          ( const _path       : String ;
                                        var   _new_path   : String
                                      ) : Boolean ; override;
  end ;

//##############################################################################
implementation

uses
  GisTypes,
  GisResource,
  GisRegistredLayers,
  GisFilePixelStoreFireDac;

//==============================================================================
// TGIS_LayerPixelStoreFireDac
//==============================================================================

  constructor TGIS_LayerPixelStoreFireDac.Create ;
  begin
    inherited ;

    FStore := TGIS_FilePixelStoreFireDac.Create ;
    FStore.GIS_Layer := self ;
  end ;

  function TGIS_LayerPixelStoreFireDac.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := Store.Prerecognize( _path ) ;
  end ;

initialization
  RegisterLayer( 'DK-TTKPS', 'TatukGIS PixelStore', TGIS_LayerPixelStoreFireDac,
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

