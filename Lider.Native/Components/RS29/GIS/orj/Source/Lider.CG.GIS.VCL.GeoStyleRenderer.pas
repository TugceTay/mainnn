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
  Utilities connected with styled settings.
}

unit VCL.GisStyleRenderer;
{$HPPEMIT '#pragma link "VCL.GisStyleRenderer"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  Winapi.Windows,

  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Themes,

  GisTypesUI,
  GisStyleRendererAbstract;

type

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   FOR INTERNAL USE ONLY. Class to render legend according to current visual style.
  /// </summary>
  TGIS_StyleLayer = class
    private
      oParent : TWinControl ;
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
      /// <param name="_parent">
      ///   parent control
      /// </param>
      constructor Create( const _parent : TWinControl ) ;

    public
      destructor Destroy ; override ;

    public
      /// <summary>
      ///   Get Styleservice for controls.
      /// </summary>
      /// <returns>
      ///   Stle sevice insatnce.
      /// </returns>
      function ControlStyleService : TCustomStyleServices ;

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
  TGIS_StyleRendererVCL = class( TGIS_StyleRendererAbstract )

    private
      FBorderStyle  : TBorderStyle ;
      FColor        : TColor       ;
      FFont         : TFont        ;

      FSelectionColor     : TColor ;
      FSelectionFontColor : TColor ;

    private
      oStyleLayer   : TGIS_StyleLayer ;
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
      ///   Sets colors for s;ection.
      /// </summary>
      /// <param name="_color">
      ///   new backround color
      /// </param>
      /// <param name="_fontcolor">
      ///   new font color
      /// </param>
      procedure SetSelectionColors  ( const _color     : TColor;
                                      const _fontcolor : TColor
                                    ) ; virtual ;

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
      /// <param name="_selected">
      ///   if True, the color is for selected nodes
      /// </param>
      /// <returns>
      ///   if _selected is True, then the default background color for selected nodes
      ///   if _selected is False and _color = clWindow, then color according to current visual style
      ///                                                else set as background
      /// </returns>
      function  GetActualColor     ( const _color       : TColor ;
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
      procedure DrawImage          ( const _bitmap   : TGIS_Bitmap ;
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
// TGIS_StyleLayer
//==============================================================================

  constructor TGIS_StyleLayer.Create(
    const _parent : TWinControl
  ) ;
  begin
    inherited Create ;

    oParent := _Parent ;

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

  function TGIS_StyleLayer.ControlStyleService
    : TCustomStyleServices ;
  begin
    {$IFDEF LEVEL_RX11_VCL}
      Result := StyleServices(oParent);
    {$ELSE}
      Result := StyleServices;
    {$ENDIF}
  end;

  function TGIS_StyleLayer.GetElementDetails(
    _detail : TThemedButton
  ) : TThemedElementDetails ;
  var
    ted : TThemedElementDetails ;
  begin

    if isStyleEnabled then begin
      Result := ControlStyleService.GetElementDetails( _detail ) ;
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
        Result :=  ControlStyleService.DrawElement(
                    _canvas.Handle, _details, _rect, nil, iPPI
                  ) ;
      {$ELSE}
        Result := ControlStyleService.DrawElement(
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
      Result := ControlStyleService.GetTextExtent(
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
      cl := _canvas.Font.Color ;
      Result := ControlStyleService.DrawText(
                  _canvas.Handle, _details, Trim( _text ), _rect, _flags, cl
                ) ;
    end
    else begin
      _canvas.TextOut( _rect.Left, _rect.Top, Trim( _text ) ) ;

      Result := True ;
    end ;
  end ;

  function TGIS_StyleLayer.GetStyleColor(
    _color : TStyleColor
  ) : TColor ;
  begin
    if isStyleEnabled then begin
      Result := ControlStyleService.GetStyleColor( _color ) ;
    end
    else begin
      Result := ControlStyleService.GetSystemColor( clWindow ) ;
    end ;
  end ;

  function TGIS_StyleLayer.GetStyleColor(
    _color   : TStyleColor ;
    _default : TColor
  ) : TColor ;
  begin
    if StyleServices.Enabled and not StyleServices.IsSystemStyle then
      Result := GetStyleColor( TStyleColor.scTreeView )
    else
      Result := _default ;
  end ;

//==============================================================================
// TGIS_StyleRendererVCL
//==============================================================================

  constructor TGIS_StyleRendererVCL.Create(
    const _parent : TWinControl
  ) ;
  begin
    inherited Create ;
    oStyleLayer := TGIS_StyleLayer.Create( _parent ) ;

    oParent := _parent ;

    FBorderStyle := bsSingle ;
    FColor  := clWindow ;
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

  destructor TGIS_StyleRendererVCL.Destroy ;
  begin
    FreeObject( oStyleLayer );
    FreeObject( FFont ) ;
  end ;

  procedure TGIS_StyleRendererVCL.CreateContext(
    const _canvas   : TCanvas ;
    const _rtl      : Boolean
  ) ;
  begin
    oCanvas := _canvas ;
    bRightToLeft := _rtl ;
  end ;

  procedure TGIS_StyleRendererVCL.FreeContext ;
  begin
    oCanvas := nil ;
  end ;

  function TGIS_StyleRendererVCL.GetBorderStyle
    : TBorderStyle ;
  begin
    Result := FBorderStyle ;
  end ;

  procedure TGIS_StyleRendererVCL.SetBorderStyle(
    const _value : TBorderStyle
  ) ;
  begin
    FBorderStyle := _value ;
  end ;

  function TGIS_StyleRendererVCL.GetColor
    : TColor ;
  begin
    Result := FColor ;
  end;

  procedure TGIS_StyleRendererVCL.SetColor(
    const _value : TColor
  ) ;
  begin
    FColor := _value ;
  end ;

  procedure TGIS_StyleRendererVCL.SetSelectionColors(
    const _color     : TColor;
    const _fontcolor : TColor
  ) ;
  begin
    FSelectionColor := _color ;
    FSelectionFontColor := _fontcolor ;
  end;

  function TGIS_StyleRendererVCL.GetFont
    : TFont ;
  begin
    Result := FFont ;
  end ;

  procedure TGIS_StyleRendererVCL.SetFont(
    const _value : TFont
  ) ;
  begin
    FFont.Assign( _value ) ;
  end ;

  function TGIS_StyleRendererVCL.GetActualColor(
    const _selected : Boolean
  ) : TColor ;
  var
    style   : TCustomStyleServices ;
    details : TThemedElementDetails ;
    cl      : TColor ;
  begin
    if {$IFDEF LEVEL_RX11_VCL}
         oParent.IsCustomStyleActive
       {$ELSE}
         TStyleManager.IsCustomStyleActive
       {$ENDIF}
       and
       (seClient in oParent.StyleElements)
    then begin
      if _selected then begin
        if seFont in oParent.StyleElements then
          Result := StyleServices.GetSystemColor( clHighlight )
        else
          Result := FSelectionColor;
      end
      else begin
        Result := oStyleLayer.ControlStyleService .GetStyleColor( scListBox )
      end;
    end
    else begin
      if _selected then begin
        if seFont in oParent.StyleElements then
          Result := clHighlight
        else
          Result := FSelectionColor;
      end
      else begin
        if seClient in oParent.StyleElements then
          Result := clWindow
        else
          Result := FColor ;
      end;
    end ;
  end ;

  function TGIS_StyleRendererVCL.GetActualColor(
    const _color       : TColor ;
    const _selected    : Boolean
  ) : TColor ;
  begin
    SetColor( _color ) ;
    Result := GetActualColor( _selected ) ;
  end ;

  function TGIS_StyleRendererVCL.GetActualFontColor(
    const _selected : Boolean
  ) : TColor ;
  begin
    if {$IFDEF LEVEL_RX11_VCL}
         oParent.IsCustomStyleActive
       {$ELSE}
         TStyleManager.IsCustomStyleActive
       {$ENDIF}
       and
       ( seFont in oParent.StyleElements )
       and
       ( seClient in oParent.StyleElements )
    then begin
      if _selected then
        Result := StyleServices.GetStyleFontColor(sfListItemTextSelected)
      else
        Result := StyleServices.GetStyleFontColor(sfListItemTextNormal)
    end
    else begin
      if _selected then begin
        if seFont in oParent.StyleElements then
          Result := clHighlightText
        else
          Result := FSelectionFontColor;
      end
      else begin
        if seFont in oParent.StyleElements then
          Result := clWindowText
        else
          Result := FFont.Color ;
      end;
    end ;
  end;

  procedure TGIS_StyleRendererVCL.SetPPI(
    const _value : Integer
  ) ;
  begin
    oStyleLayer.PixelsPerInch := _value ;
  end ;

  procedure TGIS_StyleRendererVCL.SetBrushColor(
    const _selected : Boolean
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    oCanvas.Pen.Color   := GetActualColor( _selected ) ;
    oCanvas.Brush.Color := GetActualColor( _selected ) ;
  end ;

  function TGIS_StyleRendererVCL.GetBrushColor
    : TGIS_Color ;
  begin
    Result := GISColor( oCanvas.Brush.Color ) ;
  end ;

  procedure TGIS_StyleRendererVCL.DrawCheckBox(
    const _checked : Boolean ;
    const _rect    : TRect
  ) ;
  var
    dtl  : TThemedElementDetails ;
    dfcs : Integer ;
    bs   : TBrush  ;
  begin
    if not assigned( oCanvas ) then
      exit ;

    if StyleServices.Enabled then begin
      if _checked then
        dtl := oStyleLayer.GetElementDetails( TThemedButton.tbCheckBoxCheckedNormal )
      else
        dtl := oStyleLayer.GetElementDetails( TThemedButton.tbCheckBoxUncheckedNormal ) ;
      oStyleLayer.DrawElement( oCanvas, dtl, _rect ) ;
    end
    else begin
      if _checked then
        dfcs := DFCS_BUTTONCHECK or DFCS_CHECKED
      else
        dfcs := DFCS_BUTTONCHECK;

      DrawFrameControl(oCanvas.Handle,_rect, DFC_BUTTON, dfcs );
      // flatten button
      bs := TBrush.Create ;
      try
        bs.Assign( oCanvas.Brush );
        oCanvas.Brush.Style := bsClear;
        oCanvas.Rectangle( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom );
      finally
        oCanvas.Brush.Assign( bs );
        FreeObject( bs ) ;
      end;
    end;
  end ;

  procedure TGIS_StyleRendererVCL.DrawExpandCollapseMarker(
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

  procedure TGIS_StyleRendererVCL.DrawRectangle(
    const _rect : TRect
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    oCanvas.Rectangle( _rect ) ;
  end ;

  procedure TGIS_StyleRendererVCL.DrawRectangle(
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

  function TGIS_StyleRendererVCL.GetTextExtent(
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

  procedure TGIS_StyleRendererVCL.DrawText(
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

  procedure TGIS_StyleRendererVCL.DrawImage(
    const _bitmap      : TGIS_Bitmap ;
    const _left        : Integer ;
    const _top         : Integer ;
    const _transparent : Boolean
  ) ;
  var
    cnv : TCanvas ;
    bmp : TGIS_Bitmap ;
  begin
    if not assigned( oCanvas ) then exit ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := oCanvas ;
    if _transparent then
      _bitmap.Transparent := True ;
    bmp := TGIS_Bitmap.Create ;
    try
      bmp.Assign( _bitmap ) ;
      cnv.Draw( _left, _top, TBitmap( bmp.NativeBitmap ) ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

  procedure TGIS_StyleRendererVCL.CreateTemporaryContext(
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

  procedure TGIS_StyleRendererVCL.RenderTemporaryContext(
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

//==================================== END =====================================
end.
