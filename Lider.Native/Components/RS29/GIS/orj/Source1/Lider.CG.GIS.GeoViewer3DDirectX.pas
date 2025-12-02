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
  Encapsulation of the TGIS_Renderer3DDirectX9 class.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoViewer3DDirectX ;
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.WinForms ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF OXYGENE}
  uses
    System.IO,
    System.Drawing,
    TatukGIS.RTL,
    TatukGIS.NDK ;
{$ELSE}
  uses
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoViewer3DBase ;
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

    protected 
      /// <inheritdoc/>
      procedure doDestroy ; override ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRenderer3DDirectX9 ;
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

//==================================== END =====================================
end.
