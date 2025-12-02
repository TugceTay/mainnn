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
  Encapsulation of a PixelStore - ADO file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerPixelStoreAdo ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerPixelStoreAdo"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$IFDEF GIS_NOADO}
  interface
  implementation
{$ELSE}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NOADO}
  {$Message Error 'Unit not supported - no ADO support' }
{$ENDIF}

interface

{$IFDEF OXYGENE}
  uses
    TatukGIS.RTL,
    ADODB ;
{$ELSE}
  uses
    {$IFDEF ADOINTUNIT}
      AdoInt,
    {$ELSE}
      Lider.CG.GIS.GeoAdoInt,
    {$ENDIF}
    Lider.CG.GIS.GeoLayerPixelStore ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerPixelStoreAdo = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of PixelStore layer via ADO.
  /// </summary>
  TGIS_LayerPixelStoreAdo = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_LayerPixelStoreAbstract )
    protected
        procedure fset_SharedConnection   ( const _obj : _Connection );
        function  fget_SharedConnection : _Connection ;
    public // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  PreRecognize          ( const _path       : String ;
                                        var _new_path     : String
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

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoFilePixelStoreAdo ;
{$ENDIF}

//==============================================================================
// TGIS_LayerPixelStoreAdo
//==============================================================================

  constructor TGIS_LayerPixelStoreAdo.Create ;
  begin
    inherited ;

    FStore := TGIS_FilePixelStoreAdo.Create ;
    FStore.GIS_Layer := self ;
  end ;

  procedure TGIS_LayerPixelStoreAdo.fset_SharedConnection(
    const _obj : _Connection
  ) ;
  begin
    TGIS_FilePixelStoreAdo( FStore ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerPixelStoreAdo.fget_SharedConnection : _Connection;
  begin
    Result := TGIS_FilePixelStoreAdo( FStore ).SharedConnection ;
  end ;

  function TGIS_LayerPixelStoreAdo.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result := Store.Prerecognize( _path ) ;
  end;

  { Perform initialization section.
  }
  class procedure GisLayerPixelStoreAdo.SelfRegisterLayer ;
  begin
    RegisterLayer( 'DK-TTKPS', 'TatukGIS PixelStore', TGIS_LayerPixelStoreAdo,
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
    GisLayerPixelStoreAdo.SelfRegisterLayer() ;
{$ENDIF}

{$ENDIF} // GIS_NODB
{==================================== END =====================================}
end.

