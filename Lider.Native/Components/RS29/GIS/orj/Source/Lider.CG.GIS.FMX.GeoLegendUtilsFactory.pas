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

unit FMX.GisLegendUtilsFactory ;
{$HPPEMIT '#pragma link "FMX.GisLegendUtilsFactory"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,

  GisTypes,
  GisTypesUI,
  GisInterfaces,
  GisRendererAbstract,
  GisStyleRendererAbstract,
  GisLayer,
  GisSymbol,
  GisLegendUtils,
  FMX.GisViewerBmp ;

type

  {$IFDEF LEVEL_RX10_FMX}
    /// <summary>
    ///   Styled settings.
    /// </summary>
    TGIS_ControlLegendStyledSetting = (

      /// <summary>
      ///   Set background.
      /// </summary>
      Background,

      /// <summary>
      ///   Set font fammily.
      /// </summary>
      Family,

      /// <summary>
      ///   Set font size.
      /// </summary>
      Size,

      /// <summary>
      ///   Set font size.
      /// </summary>
      Style,

      /// <summary>
      ///   Set font color.
      /// </summary>
      FontColor,

      /// <summary>
      ///   Set font color.
      /// </summary>
      Checkbox
    ) ;

    /// <summary>
    ///   Set of styled settings.
    /// </summary>
    TGIS_ControlLegendStyledSettings = set of TGIS_ControlLegendStyledSetting ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   FOR INTERNAL USE ONLY. TGIS_ViewerBmp encapsulation.
  /// </summary>
  TGIS_LegendViewerFMX = class( TGIS_LegendViewer )
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


  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   FOR INTERNAL USE ONLY. TGIS_LegendViewer creation utility.
  /// </summary>
  TGIS_LegendViewerFactoryFMX = class( TGIS_LegendViewerFactory )
    public
      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      constructor Create ;
    public
      /// <summary>
      ///   Destructor, destroys the instance.
      /// </summary>
      destructor Destroy ; override;
    public

      /// <inheritdoc/>
      function CreateLegendViewer ( const _width  : Integer ;
                                    const _height : Integer
                                  ) : TGIS_LegendViewer ; override;
  end ;


  {#gendoc:hide}
  TGIS_StyledControl = class( TStyledControl )
  end ;

//##############################################################################
implementation

uses
  System.Math,
  {$IFDEF LEVEL_RX10_FMX}
    System.Math.Vectors,
  {$ENDIF}
  FMX.Objects,
  FMX.StdCtrls,
  FMX.TextLayout,
  FMX.Styles.Objects,

  GisRtl,
  FMX.GisFramework,
  FMX.GisRenderer ;

//==============================================================================
// TGIS_LegendViewerFMX
//==============================================================================

  constructor TGIS_LegendViewerFMX.Create(
    const _width  : Integer ;
    const _height : Integer
  ) ;
  begin
    inherited Create ;

    oViewer := TGIS_ViewerBmp.Create( _width, _height ) ;
  end ;


  destructor TGIS_LegendViewerFMX.Destroy ;
  begin
    FreeObject( oViewer ) ;
  end ;


  function TGIS_LegendViewerFMX.fget_CustomPPI : Integer ;
  begin
    Result := oViewer.CustomPPI ;
  end ;


  procedure TGIS_LegendViewerFMX.fset_CustomPPI(
    const _ppi : Integer
  ) ;
  begin
    oViewer.CustomPPI := _ppi ;
  end ;


  function TGIS_LegendViewerFMX.fget_Color : TGIS_Color ;
  begin
    Result := oViewer.Color ;
  end ;


  procedure TGIS_LegendViewerFMX.fset_Color(
    const _color  : TGIS_Color
  ) ;
  begin
    oViewer.Color := _color ;
  end ;


  function TGIS_LegendViewerFMX.fget_VisibleExtent : TGIS_Extent ;
  begin
    Result := oViewer.VisibleExtent ;
  end ;


  procedure TGIS_LegendViewerFMX.fset_VisibleExtent(
    const _extent : TGIS_Extent
  ) ;
  begin
    oViewer.VisibleExtent := _extent ;
  end ;

  function TGIS_LegendViewerFMX.fget_NativeBitmap : TObject ;
  begin
    Result := oViewer.Bitmap ;
  end ;

  function TGIS_LegendViewerFMX.fget_Bitmap : TGIS_Bitmap ;
  begin
    Result := oViewer.GIS_Bitmap ;
  end ;


  function TGIS_LegendViewerFMX.fget_Renderer : TGIS_RendererAbstract ;
  begin
    Result := oViewer.Renderer ;
  end ;


  procedure TGIS_LegendViewerFMX.fset_Renderer(
    const _rndr : TGIS_RendererAbstract
  ) ;
  begin
    oViewer.Renderer := _rndr ;
  end ;


  procedure TGIS_LegendViewerFMX.Add(
    const _layer : TGIS_Layer
  ) ;
  begin
    oViewer.Add( _layer ) ;
  end ;


  function TGIS_LegendViewerFMX.TwipsToPixels(
    const _size  : Integer
  ) : Integer ;
  begin
    Result := oViewer.TwipsToPixels( _size ) ;
  end ;


  function TGIS_LegendViewerFMX.PixelsToTwips(
    const _size  : Integer
  ) : Integer ;
  begin
    Result := oViewer.PixelsToTwips( _size ) ;
  end ;


  procedure TGIS_LegendViewerFMX.Draw ;
  begin
    oViewer.Draw ;
  end ;


//==============================================================================
// TGIS_LegendViewerFactoryFMX
//==============================================================================

  constructor TGIS_LegendViewerFactoryFMX.Create ;
  begin
    inherited ;

  end ;


  destructor TGIS_LegendViewerFactoryFMX.Destroy ;
  begin

    inherited ;
  end ;


  function TGIS_LegendViewerFactoryFMX.CreateLegendViewer(
    const _width  : Integer ;
    const _height : Integer
  ) : TGIS_LegendViewer ;
  begin
    Result := TGIS_LegendViewerFMX.Create( _width, _height ) ;
  end ;

//==================================== END =====================================
end.

