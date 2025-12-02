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
  Utilities for constructing legend.
}

unit Lider.CG.GIS.VCL.GeoLegendUtilsFactory ;
{$HPPEMIT '#pragma link "VCL.Lider.CG.GIS.GeoLegendUtilsFactory"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.Types,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Themes,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoRendererAbstract,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoLegendUtils,
  Lider.CG.GIS.VCL.GeoViewerBmp ;

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


  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   FOR INTERNAL USE ONLY. Class to render legend according to current visual style.
  /// </summary>
  TGIS_StyleLayer = class
    private
      iPPI           : Integer ;
      scaleFactor    : Double ;
      isStyleEnabled : Boolean ;
    private // no style elements
      icoUnchecked : Vcl.Graphics.TBitmap ;
      icoChecked   : Vcl.Graphics.TBitmap ;
      icoClosed    : Vcl.Graphics.TBitmap ;
      icoOpened    : Vcl.Graphics.TBitmap ;
    private
      function  fget_PPI  : Integer ;
      procedure fset_PPI  ( const _val : Integer
                          ) ;
    private
      function  apScale ( const _val : Integer
                        ) : Integer ;
      procedure prepareNoStyle ;
      procedure unprepareNoStyle ;

    public
      /// <summary>
      ///   Create an instance of the class.
      /// </summary>
      constructor Create ;

    public
      destructor Destroy ; override ;

    public
      /// <summary>
      ///   Get element details.
      /// </summary>
      /// <param name="_detail">
      ///   detail type
      /// </param>
      /// <returns>
      ///   element details
      /// </returns>
      function  GetElementDetails (    _detail     : TThemedButton
                                  ) : TThemedElementDetails ; overload;
      /// <summary>
      ///   Get element details.
      /// </summary>
      /// <param name="_detail">
      ///   detail type
      /// </param>
      /// <returns>
      ///   element details
      /// </returns>
      function  GetElementDetails (     _detail    : TThemedTreeview
                                  ) : TThemedElementDetails ; overload;
      /// <summary>
      ///   Draw element.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      /// <param name="_details">
      ///   element details
      /// </param>
      /// <param name="_rect">
      ///   rectangle to draw in
      /// </param>
      /// <returns>
      ///   True if ok, False otherwise
      /// </returns>
      function  DrawElement       ( const _canvas  : TCanvas ;
                                          _details : TThemedElementDetails ;
                                    const _rect    : TRect
                                  ) : Boolean ;
      /// <summary>
      ///   Get text extent.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      /// <param name="_details">
      ///   element details
      /// </param>
      /// <param name="_text">
      ///   text to examine
      /// </param>
      /// <param name="_flags">
      ///   text format
      /// </param>
      /// <param name="_rect">
      ///   extent rectangle
      /// </param>
      /// <returns>
      ///   True if ok, False otherwise
      /// </returns>
      function  GetTextExtent     ( const _canvas  : TCanvas ;
                                          _details : TThemedElementDetails ;
                                    const _text    : String ;
                                          _flags   : TTextFormat ;
                                      out _rect    : TRect
                                  ) : Boolean ;
      /// <summary>
      ///   Draw text.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      /// <param name="_details">
      ///   element details
      /// </param>
      /// <param name="_text">
      ///   text to draw
      /// </param>
      /// <param name="_rect">
      ///   extent rectangle
      /// </param>
      /// <param name="_flags">
      ///   text format
      /// </param>
      /// <returns>
      ///   True if ok, False otherwise
      /// </returns>
      function  DrawText          ( const _canvas  : TCanvas ;
                                          _details : TThemedElementDetails ;
                                    const _text    : String ;
                                      var _rect    : TRect ;
                                          _flags   : TTextFormat
                                  ) : Boolean ;
      /// <summary>
      ///   Get color.
      /// </summary>
      /// <param name="_color">
      ///   input color
      /// </param>
      /// <returns>
      ///   color according to current visual style
      /// </returns>
      function  GetStyleColor     (      _color    : TStyleColor
                                  ) : TColor ; overload ;

      /// <summary>
      ///   Get color.
      /// </summary>
      /// <param name="_color">
      ///   input color
      /// </param>
      /// <param name="_default">
      ///   default color
      /// </param>
      /// <returns>
      ///   color according to current visual style
      /// </returns>
      function  GetStyleColor     (      _color    : TStyleColor ;
                                         _default  : TColor
                                  ) : TColor ; overload ;

    public
      /// <summary>
      ///   Current PPI.
      /// </summary>
      property PixelsPerInch : Integer
                               read  fget_PPI
                               write fset_PPI ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   FOR INTERNAL USE ONLY. A legend renderer for VCL.
  /// </summary>
  TGIS_LegendRendererVCL = class( TGIS_LegendRendererAbstract )

    private
      FBorderStyle  : TBorderStyle ;
      FColor        : TColor       ;
      FParentColor  : Boolean      ;
      FFont         : TFont        ;

    private
      oParent       : TWinControl ;
      oCanvas       : TCanvas ;
      oCanvasBitmap : TBitmap ;
      bRightToLeft  : Boolean ;

    public

      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_parent">
      ///   parent control
      /// </param>
      constructor Create ( const _parent : TWinControl ) ;

    public
      destructor Destroy ; override ;

    public

      /// <summary>
      ///   Gets current border style.
      /// </summary>
      /// <returns>
      ///   current border style
      /// </returns>
      function  GetBorderStyle     : TBorderStyle ; virtual ;

      /// <summary>
      ///   Sets current border style.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetBorderStyle     ( const _value : TBorderStyle
                                   ) ; virtual ;

      /// <summary>
      ///   Gets current color.
      /// </summary>
      /// <returns>
      ///   current color
      /// </returns>
      function  GetColor           : TColor ; virtual ;

      /// <summary>
      ///   Sets color.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetColor           ( const _value : TColor
                                   ) ; virtual ;

      /// <summary>
      ///   Sets parent color flag.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetParentColor     ( const _value : Boolean
                                   ) ;

      /// <summary>
      ///   Gets current font.
      /// </summary>
      /// <returns>
      ///   current font
      /// </returns>
      function  GetFont            : TFont ; virtual ;

      /// <summary>
      ///   Sets current font.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetFont            ( const _value : TFont
                                   ) ; virtual ;

      /// <summary>
      ///   Gets actual background color.
      /// </summary>
      /// <param name="_selected">
      ///   if True, the color is for selected nodes
      /// </param>
      /// <returns>
      ///   if _selected is True, then the default background color for selected nodes
      ///   if _selected is False and _color = clWindow, then color according to current visual style
      ///                                                else set as background
      /// </returns>
      function  GetActualColor     ( const _selected    : Boolean
                                   ) : TColor ; overload ; virtual ;

      /// <summary>
      ///   Gets actual background color.
      /// </summary>
      /// <param name="_color">
      ///   current window Color
      /// </param>
      /// <param name="_parentColor">
      ///   if True, the color is always taken from Style
      /// </param>
      /// <param name="_selected">
      ///   if True, the color is for selected nodes
      /// </param>
      /// <returns>
      ///   if _selected is True, then the default background color for selected nodes
      ///   if _selected is False and _color = clWindow, then color according to current visual style
      ///                                                else set as background
      /// </returns>
      function  GetActualColor     ( const _color       : TColor ;
                                     const _parentColor : Boolean ;
                                     const _selected    : Boolean
                                   ) : TColor ; overload ; virtual ;

      /// <summary>
      ///   Gets actual font color.
      /// </summary>
      /// <param name="_selected">
      ///   if True, the color is for selected nodes
      /// </param>
      /// <returns>
      ///   if _selected is True, then the default font color for selected nodes
      ///   if _selected is False and _color = WindowText, then color according to current visual style
      ///                                                  else color set as font color
      /// </returns>
      function  GetActualFontColor ( const _selected : Boolean
                                   ) : TColor ; virtual ;

      /// <summary>
      ///   Sets PPI.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetPPI             ( const _value    : Integer
                                   ) ; virtual ;

      /// <summary>
      ///   Paint the border according to current style.
      /// </summary>
      procedure PaintBorder        ; virtual ;

      /// <summary>
      ///   Prepares a renderer context.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      /// <param name="_rtl">
      ///   right to left mode
      /// </param>
      procedure CreateContext      ( const _canvas   : TCanvas ;
                                     const _rtl      : Boolean
                                   ) ;

      /// <summary>
      ///   Frees a renderer context.
      /// </summary>
      procedure FreeContext        ;

      /// <inheritdoc/>
      function  GetBrushColor      : TGIS_Color ; override ;

      /// <inheritdoc/>
      procedure SetBrushColor      ( const _selected : Boolean
                                   ) ; override ;

      /// <inheritdoc/>
      procedure DrawCheckBox       ( const _checked  : Boolean ;
                                     const _rect     : TRect
                                   ) ; override ;

      /// <inheritdoc/>
      procedure DrawExpandCollapseMarker
                                   ( const _expanded : Boolean ;
                                     const _rect     : TRect
                                   ) ; override ;

      /// <inheritdoc/>
      procedure DrawRectangle      ( const _rect     : TRect
                                   ) ; overload ; override ;

      /// <inheritdoc/>
      procedure DrawRectangle      ( const _left     : Integer ;
                                     const _top      : Integer ;
                                     const _width    : Integer ;
                                     const _height   : Integer ;
                                     const _brush    : TGIS_Color ;
                                     const _pen      : TGIS_Color
                                   ) ; overload ; override ;

      /// <inheritdoc/>
      function  GetTextExtent      ( const _selected : Boolean ;
                                     const _text     : String
                                   ) : TPoint ; override ;

      /// <inheritdoc/>
      procedure DrawText           ( const _selected : Boolean ;
                                     const _text     : String  ;
                                     const _rect     : TRect
                                   ) ; override ;

      /// <inheritdoc/>
      procedure DrawImage          ( const _bitmap   : TObject ;
                                     const _left     : Integer ;
                                     const _top      : Integer ;
                                     const _transparent
                                                     : Boolean
                                   ) ; override ;

      /// <inheritdoc/>
      procedure CreateTemporaryContext
                                   ( const _width    : Integer ;
                                     const _height   : Integer ;
                                     const _ppi      : Integer
                                   ) ; override ;

      /// <inheritdoc/>
      procedure RenderTemporaryContext
                                   ( const _left     : Integer ;
                                     const _top      : Integer
                                   ) ; override ;
  end ;

var
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Instance of the class.
  /// </summary>
  oStyleLayer : TGIS_StyleLayer ;

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

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.VCL.GeoFramework;

const
  THEME_BP_CHECKBOX = 3 ;

//==============================================================================
// TGIS_LegendRendererVCL
//==============================================================================

  constructor TGIS_LegendRendererVCL.Create(
    const _parent : TWinControl
  ) ;
  begin
    inherited Create ;
    oParent := _parent ;

    FBorderStyle := bsSingle ;
    FColor  := clWindow ;
    FParentColor := False ;
    FFont := TFont.Create ;
    FFont.PixelsPerInch := 96 ;
    FFont.Name := 'Segoe UI' ;
    FFont.Size := 9 ;
    FFont.Style := [] ;
    FFont.Color := clWindowText ;

    oCanvas := nil ;
    oCanvasBitmap := nil ;
    bRightToLeft := False ;
  end ;

  destructor TGIS_LegendRendererVCL.Destroy ;
  begin
    FreeObject( FFont ) ;
  end ;

  procedure TGIS_LegendRendererVCL.CreateContext(
    const _canvas   : TCanvas ;
    const _rtl      : Boolean
  ) ;
  begin
    oCanvas := _canvas ;
    bRightToLeft := _rtl ;
  end ;

  procedure TGIS_LegendRendererVCL.FreeContext ;
  begin
    oCanvas := nil ;
  end ;

  function TGIS_LegendRendererVCL.GetBorderStyle
    : TBorderStyle ;
  begin
    Result := FBorderStyle ;
  end ;

  procedure TGIS_LegendRendererVCL.SetBorderStyle(
    const _value : TBorderStyle
  ) ;
  begin
    FBorderStyle := _value ;
  end ;

  function TGIS_LegendRendererVCL.GetColor
    : TColor ;
  begin
    Result := FColor ;
  end;

  procedure TGIS_LegendRendererVCL.SetColor(
    const _value : TColor
  ) ;
  begin
    FColor := _value ;
  end ;

  procedure TGIS_LegendRendererVCL.SetParentColor(
    const _value : Boolean
  ) ;
  begin
    FParentColor := _value ;
  end ;

  function TGIS_LegendRendererVCL.GetFont
    : TFont ;
  begin
    Result := FFont ;
  end ;

  procedure TGIS_LegendRendererVCL.SetFont(
    const _value : TFont
  ) ;
  begin
    FFont.Assign( _value ) ;
  end ;

  function TGIS_LegendRendererVCL.GetActualColor(
    const _selected : Boolean
  ) : TColor ;
  var
    style   : TCustomStyleServices ;
    details : TThemedElementDetails ;
    cl      : TColor ;
  begin
    if _selected then
      Result := $FFE8CC
    else begin
      if ( FColor = clWindow ) then
        Result := oStyleLayer.GetStyleColor( TStyleColor.scTreeView )
      else begin
        if FParentColor then
          Result := oStyleLayer.GetStyleColor( TStyleColor.scTreeView, FColor )
        else
          Result := FColor ;
      end ;
    end ;
  end ;

  function TGIS_LegendRendererVCL.GetActualColor(
    const _color       : TColor ;
    const _parentColor : Boolean ;
    const _selected    : Boolean
  ) : TColor ;
  begin
    SetColor( _color ) ;
    SetParentColor( _parentColor ) ;
    Result := GetActualColor( _selected ) ;
  end ;

  function TGIS_LegendRendererVCL.GetActualFontColor(
    const _selected : Boolean
  ) : TColor ;
  begin
    if _selected then
      Result := clBlack
    else begin
      if FFont.Color = clWindowText then
        Result := StyleServices.GetStyleFontColor(sfTreeItemTextNormal)
      else
        Result := FFont.Color ;
    end ;
  end ;

  procedure TGIS_LegendRendererVCL.SetPPI(
    const _value : Integer
  ) ;
  begin
    oStyleLayer.PixelsPerInch := _value ;
  end ;

  procedure TGIS_LegendRendererVCL.PaintBorder ;
  begin
    if ( FBorderStyle = bsSingle ) and ThemeControl( oParent ) then
      StyleServices.PaintBorder( oParent, False ) ;
  end ;

  procedure TGIS_LegendRendererVCL.SetBrushColor(
    const _selected : Boolean
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    oCanvas.Pen.Color   := GetActualColor( _selected ) ;
    oCanvas.Brush.Color := GetActualColor( _selected ) ;
  end ;

  function TGIS_LegendRendererVCL.GetBrushColor
    : TGIS_Color ;
  begin
    Result := GISColor( oCanvas.Brush.Color ) ;
  end ;

  procedure TGIS_LegendRendererVCL.DrawCheckBox(
    const _checked : Boolean ;
    const _rect    : TRect
  ) ;
  var
    dtl : TThemedElementDetails ;
    stl : TCustomStyleServices ;
  begin
    if not assigned( oCanvas ) then exit ;
    try
      stl := TStyleManager.Style['Windows'];
      if _checked then
        dtl := stl.GetElementDetails( TThemedButton.tbCheckBoxCheckedNormal )
      else
        dtl := stl.GetElementDetails( TThemedButton.tbCheckBoxUncheckedNormal ) ;
      {$IFDEF LEVEL_RX103_VCL}
        stl.DrawElement( oCanvas.Handle, dtl, _rect, nil, oStyleLayer.PixelsPerInch )
      {$ELSE}
        stl.DrawElement( oCanvas.Handle, dtl, _rect )
      {$ENDIF}
    except
      if _checked then
        dtl := oStyleLayer.GetElementDetails( TThemedButton.tbCheckBoxCheckedNormal )
      else
        dtl := oStyleLayer.GetElementDetails( TThemedButton.tbCheckBoxUncheckedNormal ) ;
      oStyleLayer.DrawElement( oCanvas, dtl, _rect ) ;
    end;
  end ;

  procedure TGIS_LegendRendererVCL.DrawExpandCollapseMarker(
    const _expanded : Boolean ;
    const _rect     : TRect
  ) ;
  var
    dtl : TThemedElementDetails ;
  begin
    if not assigned( oCanvas ) then exit ;
    if _expanded then
      dtl := oStyleLayer.GetElementDetails(
               TThemedTreeview.ttGlyphOpened )
    else
      dtl := oStyleLayer.GetElementDetails(
               TThemedTreeview.ttGlyphClosed ) ;
    oStyleLayer.DrawElement( oCanvas, dtl, _rect ) ;
  end ;

  procedure TGIS_LegendRendererVCL.DrawRectangle(
    const _rect : TRect
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    oCanvas.Rectangle( _rect ) ;
  end ;

  procedure TGIS_LegendRendererVCL.DrawRectangle(
    const _left   : Integer ;
    const _top    : Integer ;
    const _width  : Integer ;
    const _height : Integer ;
    const _brush  : TGIS_Color ;
    const _pen    : TGIS_Color
  ) ;
  var
    cnv : TCanvas ;
    bcl : Integer ;
    pcl : Integer ;
  begin
    if not assigned( oCanvas ) then exit ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := oCanvas ;
    bcl := cnv.Brush.Color ;
    pcl := cnv.Pen.Color ;
    try
      cnv.Brush.Color := ImitationColor( _brush ).ToBGR ;
      cnv.Pen.Color := VCLColor( _pen ) ;
      cnv.Rectangle( _left, _top, _left + _width, _top + _height ) ;
    finally
      cnv.Brush.Color := bcl ;
      cnv.Pen.Color := pcl ;
    end;
  end ;

  function TGIS_LegendRendererVCL.GetTextExtent(
    const _selected : Boolean ;
    const _text     : String
  ) : TPoint ;
  var
    dtl : TThemedElementDetails ;
    tf  : TTextFormat ;
    bmp : TBitmap ;
    cnv : TCanvas ;
    rct : TRect ;
  begin
    if _selected then
      dtl := oStyleLayer.GetElementDetails(
               TThemedTreeview.ttItemSelected )
    else
      dtl := oStyleLayer.GetElementDetails(
               TThemedTreeview.ttItemNormal ) ;
    if bRightToLeft then
      tf := [TTextFormats.tfSingleLine, TTextFormats.tfRtlReading]
    else
      tf := [TTextFormats.tfSingleLine] ;
    bmp := nil ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
    if assigned( oCanvas ) then
      cnv := oCanvas
    else begin
      bmp := TBitmap.Create ;
      bmp.PixelFormat := TPixelFormat.pf24bit ;
      cnv := bmp.Canvas ;
    end ;
    cnv.Font.Assign( FFont ) ;
    oStyleLayer.GetTextExtent( cnv, dtl, _text, tf, rct ) ;
    Result := Point( rct.Width, rct.Height ) ;
    if assigned( bmp ) then
      FreeObject( bmp ) ;
  end ;

  procedure TGIS_LegendRendererVCL.DrawText(
    const _selected : Boolean ;
    const _text     : String  ;
    const _rect     : TRect
  ) ;
  var
    cnv : TCanvas ;
    dtl : TThemedElementDetails ;
    tf  : TTextFormat ;
    rct : TRect ;
  begin
    if not assigned( oCanvas ) then exit ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := oCanvas ;
    if _selected then
      dtl := oStyleLayer.GetElementDetails(
               TThemedTreeview.ttItemSelected )
    else
      dtl := oStyleLayer.GetElementDetails(
               TThemedTreeview.ttItemNormal ) ;
    if bRightToLeft then
      tf := [TTextFormats.tfSingleLine, TTextFormats.tfRtlReading]
    else
      tf := [TTextFormats.tfSingleLine] ;
    rct := _rect ;
    cnv.Font.Assign( FFont ) ;
    cnv.Font.Color := GetActualFontColor( _selected ) ;
    oStyleLayer.DrawText( cnv, dtl, _text, rct, tf ) ;
  end ;

  procedure TGIS_LegendRendererVCL.DrawImage(
    const _bitmap      : TObject ;
    const _left        : Integer ;
    const _top         : Integer ;
    const _transparent : Boolean
  ) ;
  var
    cnv : TCanvas ;
    bmp : TBitmap ;
  begin
    if not assigned( oCanvas ) then exit ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := oCanvas ;
    if _transparent then begin
      bmp := TBitmap( _bitmap ) ;
      bmp.Transparent := True ;
    end;
    cnv.Draw( _left, _top, TBitmap( _bitmap ) ) ;
  end ;

  procedure TGIS_LegendRendererVCL.CreateTemporaryContext(
    const _width  : Integer ;
    const _height : Integer ;
    const _ppi    : Integer
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    FreeObject( oCanvasBitmap ) ;
    oCanvasBitmap := Vcl.Graphics.TBitmap.Create ;
    oCanvasBitmap.PixelFormat := TPixelFormat.pf24bit ;

    if bRightToLeft then
      oCanvasBitmap.SetSize( _width-1, _height )
    else
      oCanvasBitmap.SetSize( _width,   _height ) ;
    oCanvasBitmap.Canvas.Brush.Color := oCanvas.Brush.Color ;
    oCanvasBitmap.Canvas.Pen.Color := oCanvas.Brush.Color ;
    oCanvasBitmap.Canvas.Rectangle( 0, 0, oCanvasBitmap.Width, oCanvasBitmap.Height ) ;
    oCanvasBitmap.Canvas.Font.Assign( oCanvas.Font ) ;
  end ;

  procedure TGIS_LegendRendererVCL.RenderTemporaryContext(
    const _left : Integer ;
    const _top  : Integer
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    if bRightToLeft then
      oCanvas.Draw( _left + 1, _top, oCanvasBitmap )
    else
      oCanvas.Draw( _left,     _top, oCanvasBitmap ) ;
    FreeObject( oCanvasBitmap ) ;
  end ;

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
// TGIS_StyleLayer
//==============================================================================

  constructor TGIS_StyleLayer.Create ;
  begin
    inherited ;

    iPPI := 96 ;
    scaleFactor := 1.0 ;

    isStyleEnabled := StyleServices.Enabled ;
    if not isStyleEnabled then
      prepareNoStyle ;
  end ;

  destructor TGIS_StyleLayer.Destroy ;
  begin
    unprepareNoStyle ;

    inherited ;
  end ;

  function TGIS_StyleLayer.fget_PPI : Integer ;
  begin
    Result := iPPI ;
  end ;

  procedure TGIS_StyleLayer.fset_PPI(
    const _val : Integer
  ) ;
  var
    b : Boolean ;
  begin
    b := _val <> iPPI ;
    iPPI := _val ;
    scaleFactor := iPPI/96 ;

    if ( not isStyleEnabled ) or b then begin
      unprepareNoStyle ;
      prepareNoStyle ;
    end ;
  end ;

  function TGIS_StyleLayer.apScale(
    const _val : Integer
  ) : Integer ;
  begin
    Result := Floor( _val*scaleFactor ) ;
  end ;

  procedure TGIS_StyleLayer.prepareNoStyle ;
  var
    s : Integer ;
    h : Integer ;
    v : Integer ;
    i : Integer ;
  begin
    s := apScale( 13 ) ;
    if s mod 2 = 0 then
      Dec( s ) ;
    v := apScale( 2 ) ;
    icoUnchecked := Vcl.Graphics.TBitmap.Create ;
    icoUnchecked.PixelFormat := TPixelFormat.pf24bit ;
    icoUnchecked.SetSize( s, s ) ;
    icoUnchecked.Canvas.Rectangle( 0, 0, s, s ) ;

    icoChecked := Vcl.Graphics.TBitmap.Create ;
    icoChecked.PixelFormat := TPixelFormat.pf24bit ;
    icoChecked.SetSize( s, s ) ;
    icoChecked.Canvas.Rectangle( 0, 0, s, s ) ;
    for i := 0 to apScale( 2 ) do begin
      icoChecked.Canvas.MoveTo( v, s - 3*v - 2 + i ) ;
      icoChecked.Canvas.LineTo( 2*v + 1, s - 2*v - 1 + i ) ;
      icoChecked.Canvas.LineTo( s - v, v + i ) ;
    end ;

    s := apScale( 9 ) ;
    if s mod 2 = 0 then
      Dec( s ) ;
    h := Floor( s/2 ) ;
    icoClosed := Vcl.Graphics.TBitmap.Create ;
    icoClosed.PixelFormat := TPixelFormat.pf24bit ;
    icoClosed.SetSize( s, s ) ;
    icoClosed.Canvas.Rectangle( 0, 0, s, s ) ;
    icoClosed.Canvas.MoveTo( h, v ) ;
    icoClosed.Canvas.LineTo( h, s - v ) ;
    icoClosed.Canvas.MoveTo( v, h ) ;
    icoClosed.Canvas.LineTo( s - v, h ) ;

    icoOpened := Vcl.Graphics.TBitmap.Create ;
    icoOpened.PixelFormat := TPixelFormat.pf24bit ;
    icoOpened.SetSize( s, s ) ;
    icoOpened.Canvas.Rectangle( 0, 0, s, s ) ;
    icoOpened.Canvas.MoveTo( v, h ) ;
    icoOpened.Canvas.LineTo( s - v, h ) ;
  end ;

  procedure TGIS_StyleLayer.unprepareNoStyle ;
  begin
    FreeObject( icoUnchecked ) ;
    FreeObject( icoChecked   ) ;
    FreeObject( icoOpened    ) ;
    FreeObject( icoClosed    ) ;
  end ;

  function TGIS_StyleLayer.GetElementDetails(
    _detail : TThemedButton
  ) : TThemedElementDetails ;
  var
    ted : TThemedElementDetails ;
  begin
    if isStyleEnabled then begin
      Result := StyleServices.GetElementDetails( _detail ) ;
    end
    else begin
      ted.Element := TThemedElement.teButton ;
      ted.Part := THEME_BP_CHECKBOX ;
      case _detail of
        tbCheckBoxUncheckedNormal : ted.State := CBS_UNCHECKEDNORMAL ;
        tbCheckBoxCheckedNormal   : ted.State := CBS_CHECKEDNORMAL   ;
      end ;
      Result := ted ;
    end ;
  end ;

  function TGIS_StyleLayer.GetElementDetails(
    _detail : TThemedTreeview
  ) : TThemedElementDetails ;
  var
    ted : TThemedElementDetails ;
  begin
    if isStyleEnabled then begin
      Result := StyleServices.GetElementDetails( _detail ) ;
    end
    else begin
      ted.Element := TThemedElement.teTreeview ;
      case _detail of
        ttItemNormal   : begin
                           ted.Part  := TVP_TREEITEM   ;
                           ted.State := TREIS_NORMAL   ;
                         end ;
        ttItemSelected : begin
                           ted.Part  := TVP_TREEITEM   ;
                           ted.State := TREIS_SELECTED ;
                         end ;
        ttGlyphClosed  : begin
                           ted.Part  := TVP_GLYPH      ;
                           ted.State := GLPS_CLOSED    ;
                         end ;
        ttGlyphOpened  : begin
                           ted.Part  := TVP_GLYPH      ;
                           ted.State := GLPS_OPENED    ;
                         end ;
      end ;
      Result := ted ;
    end ;
  end ;

  function TGIS_StyleLayer.DrawElement(
    const _canvas  : TCanvas ;
          _details : TThemedElementDetails ;
    const _rect    : TRect
  ) : Boolean ;
  begin
    if isStyleEnabled then begin
      {$IFDEF LEVEL_RX103_VCL}
        Result := StyleServices.DrawElement(
                    _canvas.Handle, _details, _rect, nil, iPPI
                  ) ;
      {$ELSE}
        Result := StyleServices.DrawElement(
                    _canvas.Handle, _details, _rect
                  ) ;
      {$ENDIF}
    end
    else begin
      case _details.Element of
        teButton   :
          begin
            Assert( _details.Part = THEME_BP_CHECKBOX ) ;
            case _details.State of
              CBS_UNCHECKEDNORMAL :
                _canvas.Draw( _rect.Left, _rect.Top, icoUnchecked ) ;
              CBS_CHECKEDNORMAL   :
                _canvas.Draw( _rect.Left, _rect.Top, icoChecked ) ;
            end ;
          end ;
        teTreeview :
          begin
            Assert( _details.Part = TVP_GLYPH ) ;
            case _details.State of
              GLPS_CLOSED :
                _canvas.Draw( _rect.Left, _rect.Top, icoClosed ) ;
              GLPS_OPENED :
                _canvas.Draw( _rect.Left, _rect.Top, icoOpened ) ;
            end ;
          end ;
      end ;
      Result := True ;
    end ;
  end ;

  function TGIS_StyleLayer.GetTextExtent(
    const _canvas  : TCanvas ;
          _details : TThemedElementDetails ;
    const _text    : String ;
          _flags   : TTextFormat ;
      out _rect    : TRect
  ) : Boolean ;
  var
    siz : Integer ;
    ext : TSize ;
  begin
    if _canvas.Font.PixelsPerInch <> iPPI then begin
      siz := _canvas.Font.Size ;
      _canvas.Font.PixelsPerInch := iPPI ;
      _canvas.Font.Size := siz ;
    end ;

    if isStyleEnabled then begin
      Result := StyleServices.GetTextExtent(
                  _canvas.Handle, _details, Trim( _text ), _flags, _rect
                ) ;
    end
    else begin
      ext := _canvas.TextExtent( Trim( _text ) ) ;
      _rect := Rect( 0, 0, ext.Width, ext.Height ) ;
      Result := True ;
    end ;
  end ;

  function TGIS_StyleLayer.DrawText(
    const _canvas  : TCanvas ;
          _details : TThemedElementDetails ;
    const _text    : String ;
      var _rect    : TRect ;
          _flags   : TTextFormat
  ) : Boolean ;
  var
    siz : Integer ;
    cl  : TColor  ;
  begin
    if _canvas.Font.PixelsPerInch <> iPPI then begin
      siz := _canvas.Font.Size ;
      _canvas.Font.PixelsPerInch := iPPI ;
      _canvas.Font.Size := siz ;
    end ;

    if isStyleEnabled then begin
      if _canvas.Font.Color = clWindowText then
        cl := clNone
      else
        cl := _canvas.Font.Color ;
      Result := StyleServices.DrawText(
                  _canvas.Handle, _details, Trim( _text ), _rect, _flags, cl
                ) ;
    end
    else begin
      Assert( _details.Part = TVP_TREEITEM ) ;
      case _details.State of
        TREIS_NORMAL   :
          _canvas.Font.Color := StyleServices.GetSystemColor( clWindowText ) ;
        TREIS_SELECTED :
          _canvas.Font.Color := StyleServices.GetSystemColor( clHighlightText ) ;
      end ;

      _canvas.TextOut( _rect.Left, _rect.Top, Trim( _text ) ) ;
      Result := True ;
    end ;
  end ;

  function TGIS_StyleLayer.GetStyleColor(
    _color : TStyleColor
  ) : TColor ;
  begin
    if isStyleEnabled then begin
      Result := StyleServices.GetStyleColor( _color ) ;
    end
    else begin
      Result := StyleServices.GetSystemColor( clWindow ) ;
    end ;
  end ;

  function TGIS_StyleLayer.GetStyleColor(
    _color   : TStyleColor ;
    _default : TColor
  ) : TColor ;
  begin
    if StyleServices.Enabled and not StyleServices.IsSystemStyle then
      Result := oStyleLayer.GetStyleColor( TStyleColor.scTreeView )
    else
      Result := _default ;
  end ;

//==============================================================================
// initialization/finalization
//==============================================================================

initialization
  LegendViewerFactory := TGIS_LegendViewerFactoryVCL.Create ;
  oStyleLayer := TGIS_StyleLayer.Create ;

finalization
  FreeObject( oStyleLayer ) ;
  FreeObject( LegendViewerFactory ) ;

//==================================== END =====================================
end.
