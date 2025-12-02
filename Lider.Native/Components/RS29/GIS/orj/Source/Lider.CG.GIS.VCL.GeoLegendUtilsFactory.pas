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
  Utilities for constructing legend.
}

unit VCL.GisLegendUtilsFactory ;
{$HPPEMIT '#pragma link "VCL.GisLegendUtilsFactory"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  Winapi.Windows,

  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Themes,

  GisTypes,
  GisTypesUI,
  GisRendererAbstract,
  GisLayer,
  GisLegendUtils,
  VCL.GisStyleRenderer,
  VCL.GisViewerBmp ;

type

  /// <summary>
  ///   FOR INTERNAL USE ONLY. TGIS_ViewerBmp encapsulation.
  /// </summary>
  TGIS_LegendViewerVCL = class( TGIS_LegendViewer )
    private
      oViewer : TGIS_ViewerBmp ;
    protected
      function  fget_CustomPPI     : Integer ; override;
      procedure fset_CustomPPI     ( const _ppi    : Integer
                                   ) ; override;
      function  fget_Color         : TGIS_Color ; override;
      procedure fset_Color         ( const _color  : TGIS_Color
                                   ) ; override;
      function  fget_VisibleExtent : TGIS_Extent ; override;
      procedure fset_VisibleExtent ( const _extent : TGIS_Extent
                                   ) ; override;
      function  fget_NativeBitmap  : TObject ; override;
      function  fget_Bitmap        : TGIS_Bitmap ; override;
      function  fget_Renderer      : TGIS_RendererAbstract ; override;
      procedure fset_Renderer      ( const _rndr   : TGIS_RendererAbstract
                                   ) ; override;
    public
      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      /// <param name="_width">
      ///   width of the viewer
      /// </param>
      /// <param name="_height">
      ///   height of the viewer
      /// </param>
      constructor Create ( const _width  : Integer ;
                           const _height : Integer
                         ) ;
    public
      /// <inheritdoc/>
      destructor Destroy ; override;
    public
      /// <inheritdoc/>
      procedure Add           ( const _layer : TGIS_Layer
                              ) ; override;
      /// <inheritdoc/>
      function  TwipsToPixels ( const _size  : Integer
                              ) : Integer ; override;
      /// <inheritdoc/>
      function  PixelsToTwips ( const _size  : Integer
                              ) : Integer ; override;
      /// <inheritdoc/>
      procedure Draw          ; override;
  end ;


  /// <summary>
  ///   FOR INTERNAL USE ONLY. TGIS_LegendViewer creation utility.
  /// </summary>
  TGIS_LegendViewerFactoryVCL = class( TGIS_LegendViewerFactory )
    public
      /// <inheritdoc/>
      function CreateLegendViewer ( const _width  : Integer ;
                                    const _height : Integer
                                  ) : TGIS_LegendViewer ; override;
  end ;

const
  /// <summary>
  ///   Update delay.
  /// </summary>
  GIS_LEGEND_UPDATE_DELAY       : Integer = 1000 ;
  /// <summary>
  ///   Expand rectangle size.
  /// </summary>
  GIS_LEGEND_EXPAND_RECT_SIZE   : Integer = 9 ;
  /// <summary>
  ///   Checkbox rectangle size.
  /// </summary>
  GIS_LEGEND_CHECKBOX_RECT_SIZE : Integer = 14 ;
  /// <summary>
  ///   Legend level indent.
  /// </summary>
  GIS_LEGEND_LEVEL_INDENT       : Integer = 12 ;

//##############################################################################
implementation

uses
  System.SysUtils,
  System.Math,
  Winapi.UxTheme,

  GisRtl,
  VCL.GisFramework;

const
  THEME_BP_CHECKBOX = 3 ;

//==============================================================================
// TGIS_LegendViewerVCL
//==============================================================================

  constructor TGIS_LegendViewerVCL.Create(
    const _width  : Integer ;
    const _height : Integer
  ) ;
  begin
    inherited Create ;

    oViewer := TGIS_ViewerBmp.Create( _width, _height ) ;
  end ;

  destructor TGIS_LegendViewerVCL.Destroy ;
  begin
    FreeObject( oViewer ) ;
  end ;

  function TGIS_LegendViewerVCL.fget_CustomPPI : Integer ;
  begin
    Result := oViewer.CustomPPI ;
  end ;

  procedure TGIS_LegendViewerVCL.fset_CustomPPI(
    const _ppi : Integer
  ) ;
  begin
    oViewer.CustomPPI := _ppi ;
  end ;

  function TGIS_LegendViewerVCL.fget_Color : TGIS_Color ;
  begin
    Result := oViewer.Color ;
  end ;

  procedure TGIS_LegendViewerVCL.fset_Color(
    const _color  : TGIS_Color
  ) ;
  begin
    oViewer.Color := _color ;
  end ;

  function TGIS_LegendViewerVCL.fget_VisibleExtent : TGIS_Extent ;
  begin
    Result := oViewer.VisibleExtent ;
  end ;

  procedure TGIS_LegendViewerVCL.fset_VisibleExtent(
    const _extent : TGIS_Extent
  ) ;
  begin
    oViewer.VisibleExtent := _extent ;
  end ;

  function TGIS_LegendViewerVCL.fget_NativeBitmap : TObject ;
  begin
    Result := oViewer.Bitmap ;
  end ;

  function TGIS_LegendViewerVCL.fget_Bitmap : TGIS_Bitmap ;
  begin
    Result := oViewer.GIS_Bitmap ;
  end ;

  function TGIS_LegendViewerVCL.fget_Renderer : TGIS_RendererAbstract ;
  begin
    Result := oViewer.Renderer ;
  end ;

  procedure TGIS_LegendViewerVCL.fset_Renderer(
    const _rndr : TGIS_RendererAbstract
  ) ;
  begin
    oViewer.Renderer := _rndr ;
  end ;

  procedure TGIS_LegendViewerVCL.Add(
    const _layer : TGIS_Layer
  ) ;
  begin
    oViewer.Add( _layer ) ;
  end ;

  function TGIS_LegendViewerVCL.TwipsToPixels(
    const _size  : Integer
  ) : Integer ;
  begin
    Result := oViewer.TwipsToPixels( _size ) ;
  end ;

  function TGIS_LegendViewerVCL.PixelsToTwips(
    const _size  : Integer
  ) : Integer ;
  begin
    Result := oViewer.PixelsToTwips( _size ) ;
  end ;

  procedure TGIS_LegendViewerVCL.Draw ;
  begin
    oViewer.Draw ;
  end ;

//==============================================================================
// TGIS_LegendViewerFactoryVCL
//==============================================================================

  function TGIS_LegendViewerFactoryVCL.CreateLegendViewer(
    const _width  : Integer ;
    const _height : Integer
  ) : TGIS_LegendViewer ;
  begin
    Result := TGIS_LegendViewerVCL.Create( _width, _height ) ;
  end ;

//==============================================================================
// initialization/finalization
//==============================================================================

initialization
  LegendViewerFactory := TGIS_LegendViewerFactoryVCL.Create ;

finalization
  FreeObject( LegendViewerFactory ) ;

//==================================== END =====================================
end.
