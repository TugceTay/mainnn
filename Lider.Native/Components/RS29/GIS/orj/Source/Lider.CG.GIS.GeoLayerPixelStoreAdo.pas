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
  Encapsulation of a PixelStore - ADO file access.
}

{$IFDEF DCC}
  unit GisLayerPixelStoreAdo ;
  {$HPPEMIT '#pragma link "GisLayerPixelStoreAdo"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$IFDEF GIS_NOADO}
  interface
  implementation
{$ELSE}

{$INCLUDE GisInclude.inc}

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
      GisAdoInt,
    {$ENDIF}
    GisLayerPixelStore ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerPixelStoreAdo = class
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
    GisTypes,
    GisResource,
    GisRegistredLayers,
    GisFilePixelStoreAdo ;
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
  class procedure Unit_GisLayerPixelStoreAdo.SelfRegisterLayer ;
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
    Unit_GisLayerPixelStoreAdo.SelfRegisterLayer() ;
{$ENDIF}

{$ENDIF} // GIS_NODB
{==================================== END =====================================}
end.

