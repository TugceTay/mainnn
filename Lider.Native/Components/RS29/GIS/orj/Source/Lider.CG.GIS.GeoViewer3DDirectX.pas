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
  Encapsulation of the TGIS_Renderer3DDirectX9 class.
}

{$IFDEF DCC}
  unit GisViewer3DDirectX ;
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.WinForms ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF OXYGENE}
  uses
    System.IO,
    System.Drawing,
    TatukGIS.RTL,
    TatukGIS.NDK ;
{$ELSE}
  uses
    GisInterfaces,
    GisTypes,
    GisViewer3DBase ;
{$ENDIF}

type
  /// <summary>
  ///   DirectX9 3D Viewer.
  /// </summary>
  TGIS_Viewer3DDirectX = {$IFDEF OXYGENE} public {$ENDIF}
                  class( TGIS_Viewer3DBase )
    public

      /// <inheritdoc/>
      constructor Create  ( const _hwnd     : THandle     ;
                            const _viewer   : IGIS_Viewer ;
                            const _extent   : TGIS_Extent ;
                            const _initdraw : Boolean
                          ) ; override;

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Update window handle of 3D renderer.
      /// </summary>
      /// <param name="_hwnd">
      ///   handle to a window in which viewer should be constructed;
      /// </param>
      procedure UpdateHWND( const _hwnd     : THandle ) ;
    protected
      /// <inheritdoc/>
      procedure doDestroy ; override ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRenderer3DDirectX9 ;
{$ENDIF}

  constructor TGIS_Viewer3DDirectX.Create(
    const _hwnd     : THandle        ;
    const _viewer   : IGIS_Viewer ;
    const _extent   : TGIS_Extent ;
    const _initdraw : Boolean
  ) ;
  begin
    inherited ;

    oRenderer := TGIS_Renderer3DDirectX9.Create ;
    oRenderer.SetViewer( _viewer, self ) ;
    TGIS_Renderer3DDirectX9( oRenderer ).SetHWND( _hwnd ) ;
  end ;

  procedure TGIS_Viewer3DDirectX.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_Viewer3DDirectX.UpdateHWND(
    const _hwnd : THandle
  ) ;
  begin
    TGIS_Renderer3DDirectX9( oRenderer ).SetHWND( _hwnd ) ;
  end ;

//==================================== END =====================================
end.
