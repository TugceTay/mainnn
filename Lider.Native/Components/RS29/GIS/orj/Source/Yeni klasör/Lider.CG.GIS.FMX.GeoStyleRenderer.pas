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

unit FMX.GisStyleRenderer ;
{$HPPEMIT '#pragma link "FMX.GisStyleRenderer"'}

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
  GisLayer,
  GisSymbol,
  GisStyleRendererAbstract,
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

  {#gendoc:hide}
  TGIS_StyledControl = class( TStyledControl )
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   A legend renderer for FMX.
  /// </summary>
  TGIS_StyleRendererFMX = class( TGIS_StyleRendererAbstract )

    private
      FColor        : TAlphaColor ;
      FFont         : TFont   ;
      FFontColor    : TAlphaColor ;

      {$IFDEF LEVEL_RX10_FMX}
        FStyledSettings  : TGIS_ControlLegendStyledSettings ;
        FStyledFont      : TFont ;
        FStyledFontColor : TAlphaColor ;
      {$ENDIF}

    private
      oParent       : TFmxObject ;
      oCanvas       : TCanvas ;
      oCanvasBitmap : TBitmap ;
      bRightToLeft  : Boolean ;
      iPPI          : Integer ;
      sCanvasScale  : Single  ;
      oFmx          : TStyledControl ;

      oStlWnd              : TControl ;
      oStlCtl              : TControl ;
      oStlItm              : TControl ;
      oStlItmTextSettings  : TTextSettings ;

    private
      lstSymbolStreams : TObjectList< TResourceStream > ;

    private
      procedure prepareFmxObject    ;
      procedure drawSymbol          ( const _symbol  : TGIS_SymbolAbstract ;
                                      const _rect    : TRect
                                    ) ;

    public

      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_parent">
      ///   parent control
      /// </param>
      constructor Create ( const _parent : TFmxObject
                         ) ;

    public
      destructor Destroy ; override ;

    public

      /// <summary>
      ///   Gets current background color.
      /// </summary>
      /// <returns>
      ///   current color
      /// </returns>
      function  GetBackgroundColor : TAlphaColor ; virtual ;

      /// <summary>
      ///   Sets background color.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetBackgroundColor ( const _value    : TAlphaColor
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
      procedure SetFont            ( const _value    : TFont
                                   ) ; virtual ;

      /// <summary>
      ///   Gets current font color.
      /// </summary>
      /// <returns>
      ///   current font color
      /// </returns>
      function  GetFontColor       : TAlphaColor ; virtual ;

      /// <summary>
      ///   Sets current font color.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetFontColor       ( const _value    : TAlphaColor
                                   ) ; virtual ;

      /// <summary>
      ///   Gets actual background color.
      /// </summary>
      /// <param name="_selected">
      ///   if True, the color is for selected nodes
      /// </param>
      /// <returns>
      ///   if _selected is True, then the default background color for selected nodes
      ///   if _selected is False then the color set as background
      /// </returns>
      function  GetActualColor     ( const _selected : Boolean
                                   ) : TAlphaColor ; virtual ;

      /// <summary>
      ///   Gets actual font family.
      /// </summary>
      /// <returns>
      ///   value for current style or default value
      /// </returns>
      function  GetActualFontFamily : TFontName ;

      /// <summary>
      ///   Gets actual font size.
      /// </summary>
      /// <returns>
      ///   value for current style or default value
      /// </returns>
      function  GetActualFontSize : Single ;

      /// <summary>
      ///   Gets actual font style.
      /// </summary>
      /// <returns>
      ///   value for current style or default value
      /// </returns>
      function  GetActualFontStyle : TFontStyles ;

      /// <summary>
      ///   Gets actual font color.
      /// </summary>
      /// <param name="_selected">
      ///   if True, the color is for selected nodes
      /// </param>
      /// <returns>
      ///   if _selected is True, then the default background color for selected nodes
      ///   if _selected is False then the color set as background
      /// </returns>
      function  GetActualFontColor ( const _selected : Boolean
                                   ) : TAlphaColor ;

      /// <summary>
      ///   Gets actual check box height.
      /// </summary>
      /// <returns>
      ///   value for current style or default value
      /// </returns>
      function  GetActualCheckBoxSize
                                   : Integer ;

      /// <summary>
      ///   Sets PPI.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetPPI             ( const _value    : Integer
                                   ) ; virtual ;

      /// <summary>
      ///   Sets canvas scale.
      /// </summary>
      /// <param name="_value">
      ///   new value
      /// </param>
      procedure SetCanvasScale     ( const _value    : Single
                                   ) ; virtual ;

      {$IFDEF LEVEL_RX10_FMX}
        /// <summary>
        ///   Gets current background color.
        /// </summary>
        procedure ChangeStyle      ;

        /// <summary>
        ///   Sets current style settings.
        /// </summary>
        /// <param name="_settings">
        ///   settings of the legend
        /// </param>
        procedure ApplyStyledSettings
                                   ( _settings : TGIS_ControlLegendStyledSettings
                                   )  ;
      {$ENDIF}

      /// <summary>
      ///   Fill the canvas with the current background color.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      /// <param name="_width">
      ///   width
      /// </param>
      /// <param name="_height">
      ///   height
      /// </param>
      procedure DrawBackground     ( const _canvas   : TCanvas ;
                                     const _width    : Integer ;
                                     const _height   : Integer
                                   ) ;

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
  GIS_LEGEND_CHECKBOX_RECT_SIZE : Integer = 13 ;
  /// <summary>
  ///   Legend level indent.
  /// </summary>
  GIS_LEGEND_LEVEL_INDENT       : Integer = 12 ;

  /// <summary>
  ///   Svg symbol for checked checkbox.
  /// </summary>
  GIS_LEGEND_CHECKED   : String = 'priv://legendres_checked.svg' ;
  /// <summary>
  ///   Svg symbol for unchecked checkbox.
  /// </summary>
  GIS_LEGEND_UNCHECKED : String = 'priv://legendres_unchecked.svg' ;
  /// <summary>
  ///   Svg symbol for expand marker.
  /// </summary>
  GIS_LEGEND_EXPANDED  : String = 'priv://legendres_expanded.svg' ;
  /// <summary>
  ///   Svg symbol for collapsed marker.
  /// </summary>
  GIS_LEGEND_COLLAPSED : String = 'priv://legendres_collapsed.svg' ;

//##############################################################################
implementation

{$R GisControlLegend_SVG.RES}

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
// TGIS_StyleRendererFMX
//==============================================================================

  procedure TGIS_StyleRendererFMX.prepareFmxObject ;
  begin
    try
      if not assigned( oFmx ) then begin
        if assigned( oParent ) then begin
          oFmx := TGIS_StyledControl.Create( oParent ) ;
          oFmx.Parent := oParent ;
          oFmx.Position.X := -150.0 ;
          oFmx.Position.Y := 10.0 ;
          oFmx.Width := 100.0 ;
          oFmx.Height := 50.0 ;
          oFmx.Visible := False ;
        end ;
      end ;
    except
      FreeObject( oFmx ) ;
    end ;
  end ;

  procedure TGIS_StyleRendererFMX.drawSymbol(
    const _symbol : TGIS_SymbolAbstract ;
    const _rect   : TRect
  ) ;
  var
    w, h  : Integer ;
    ctx   : TGIS_RendererContext ;
    cache : TBitmap ;
    rnd   : TGIS_RendererFMX ;
  begin
    w := _rect.Width ;
    h := _rect.Height ;

    ctx := TGIS_RendererContext.Create ;
    cache := TBitmap.Create( w, h ) ;
    try
      cache.Clear( TAlphaColorRec.Null ) ;

      ctx.AssignBaseMap( cache, True ) ;

      rnd := TGIS_RendererFMX.Create ;
      rnd.CreateContext( nil, nil, ctx, Point( 0, 0 ), w, h, iPPI, 100 ) ;
      try
        rnd.PrepareDraw ;
        _symbol.Prepare( nil,
                         -Max( w, h ),     // size in pixels
                         TGIS_Color.Black, // .Color,
                         TGIS_Color.Black, // .OutlineColor,
                         0,                // .SymbolRotate,
                         0,
                         TGIS_LabelPosition.MiddleCenter,
                         True,
                         rnd
                       ) ;
        _symbol.Draw( w / 2, h / 2 ) ;
        _symbol.Unprepare ;
      finally
        rnd.AfterDraw ;
        rnd.ReleaseContext ;
        FreeObject( rnd ) ;
      end ;
    finally
      oCanvas.DrawBitmap( cache, RectF( 0, 0, cache.Width, cache.Height ),
                                 RectF( _rect.Left, _rect.Top,
                                        _rect.Left + cache.Width,
                                        _rect.Top + cache.Height ) ,
                                 1, False ) ;
      oCanvas.Flush ;
      FreeObject( ctx ) ;
    end ;
  end ;

  constructor TGIS_StyleRendererFMX.Create(
    const _parent : TFmxObject
  ) ;
  begin
    inherited Create ;
    oCanvas := nil ;
    FColor := TAlphaColorRec.White ;
    FFontColor := TAlphaColorRec.Black ;
    FFont := TFont.Create ;
    bRightToLeft := False ;
    iPPI := 96 ;
    sCanvasScale := 1 ;
    oParent := _parent ;
    oFmx := nil ;

    {$IFDEF LEVEL_RX10_FMX}
      FStyledSettings := [] ;
      FStyledFont := nil ;
      FStyledFontColor := TAlphaColorRec.Black ;
    {$ENDIF}

    lstSymbolStreams := TObjectList< TResourceStream >.Create( True ) ;
    lstSymbolStreams.Add(
      TResourceStream.Create(hInstance, 'LEGEND_CHECKED'   , RT_RCDATA)
    ) ;
    lstSymbolStreams.Add(
      TResourceStream.Create(hInstance, 'LEGEND_UNCHECKED' , RT_RCDATA)
    ) ;
    lstSymbolStreams.Add(
      TResourceStream.Create(hInstance, 'LEGEND_COLLAPSED' , RT_RCDATA)
    ) ;
    lstSymbolStreams.Add(
      TResourceStream.Create(hInstance, 'LEGEND_EXPANDED'  , RT_RCDATA)
    ) ;
  end ;

  destructor TGIS_StyleRendererFMX.Destroy ;
  begin
    {$IFDEF LEVEL_RX10_FMX}
      FreeObject( FStyledFont ) ;
    {$ENDIF}
    FreeObject( FFont ) ;
    if assigned( oFmx ) then begin
      oFmx.Parent := nil ;
      FreeObject( oFmx ) ;
    end ;
    FreeObject( lstSymbolStreams ) ;
    inherited ;
  end ;

  function TGIS_StyleRendererFMX.GetBackgroundColor
    : TAlphaColor ;
  begin
    Result := FColor ;
  end ;

  procedure TGIS_StyleRendererFMX.SetBackgroundColor(
    const _value : TAlphaColor
  ) ;
  begin
    FColor := _value ;
  end ;

  function TGIS_StyleRendererFMX.GetFont
    : TFont ;
  begin
    Result := FFont ;
  end ;

  procedure TGIS_StyleRendererFMX.SetFont(
    const _value : TFont
  ) ;
  begin
    FFont.Assign( _value ) ;
  end ;

  function TGIS_StyleRendererFMX.GetFontColor
    : TAlphaColor ;
  begin
    Result := FFontColor ;
  end ;

  procedure TGIS_StyleRendererFMX.SetFontColor(
    const _value : TAlphaColor
  ) ;
  begin
    FFontColor := _value ;
  end ;

  function TGIS_StyleRendererFMX.GetActualColor(
    const _selected : Boolean
  ) : TAlphaColor ;
  begin
    if _selected then
      Result := FMXColor( TGIS_Color.FromBGR($FFE8CC) )
    else
      Result := FColor ;
  end ;

  function TGIS_StyleRendererFMX.GetActualFontFamily
    : TFontName ;
  begin
    Result := FFont.Family ;
    {$IFDEF LEVEL_RX10_FMX}
      if TGIS_ControlLegendStyledSetting.Family in FStyledSettings then
        if assigned( FStyledFont ) then
          Result := FStyledFont.Family ;
    {$ENDIF}
  end ;

  function TGIS_StyleRendererFMX.GetActualFontSize
    : Single ;
  begin
    Result := FFont.Size ;
    {$IFDEF LEVEL_RX10_FMX}
      if TGIS_ControlLegendStyledSetting.Size in FStyledSettings then
        if assigned( FStyledFont ) then
          Result := FStyledFont.Size ;
    {$ENDIF}
  end ;

  function TGIS_StyleRendererFMX.GetActualFontStyle
    : TFontStyles ;
  begin
    Result := FFont.Style ;
    {$IFDEF LEVEL_RX10_FMX}
      if TGIS_ControlLegendStyledSetting.Style in FStyledSettings then
        if assigned( FStyledFont ) then
          Result := FStyledFont.Style ;
    {$ENDIF}
  end ;

  function TGIS_StyleRendererFMX.GetActualFontColor(
    const _selected : Boolean
  ) : TAlphaColor ;
  begin
    if _selected then
      Result := TAlphaColorRec.Black
    else begin
      Result := FFontColor ;
      {$IFDEF LEVEL_RX10_FMX}
        if TGIS_ControlLegendStyledSetting.FontColor in FStyledSettings then
          if assigned( FStyledFont ) then
            Result := FStyledFontColor ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_StyleRendererFMX.GetActualCheckBoxSize
    : Integer ;
  var
    check : TCheckBox ;
  begin
    Result := GIS_LEGEND_CHECKBOX_RECT_SIZE ;

    {$IFDEF LEVEL_RX10_FMX}
      if not ( TGIS_ControlLegendStyledSetting.Checkbox in FStyledSettings ) then
        exit ;

      prepareFmxObject ;

      check := TCheckBox.Create( nil ) ;
      try
        check.SetNewScene( oFmx.Scene ) ;
        check.ApplyStyleLookup ;
        Result := RoundS( TStyleObject( check ).Height ) ;
      finally
        check.Free ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_StyleRendererFMX.SetPPI(
    const _value    : Integer
  ) ;
  begin
    iPPI := _value ;
  end ;

  procedure TGIS_StyleRendererFMX.SetCanvasScale(
    const _value : Single
  ) ;
  begin
    sCanvasScale := _value ;
  end ;

{$IFDEF LEVEL_RX10_FMX}
  procedure TGIS_StyleRendererFMX.ChangeStyle ;
  var
    stl  : TFmxObject ;
    stl2 : TFmxObject ;
  begin

    FreeObject( FStyledFont ) ;

    stl := TStyledControl.LookupStyleObject(
             oParent, oParent, TControl(oParent).Scene, '', 'backgroundstyle', '', True
           );

    if not ( Assigned( stl ) and ( stl is TControl ) ) then begin
      FreeObject( oStlWnd ) ;
      FreeObject( oStlCtl ) ;
      FreeObject( stl ) ;
      exit ;
    end ;

    FreeObject( oStlWnd );
    oStlWnd := TControl( stl ) ;

    stl := TStyledControl.LookupStyleObject(
             oParent, oParent, TControl(oParent).Scene, '', 'treeviewstyle.background', '', True
           ) ;

    if not ( Assigned( stl ) and ( stl is TControl ) ) then begin
      FreeObject( oStlWnd ) ;
      FreeObject( oStlCtl ) ;
      FreeObject( oStlItm ) ;
      FreeObject( stl ) ;
      exit ;
    end ;

    FreeObject( oStlCtl );
    oStlCtl := TControl( stl );

    stl2 := oStlCtl.FindStyleResource( 'content');
    if Assigned( stl2 ) and ( stl2 is TControl ) then
      TControl( stl2 ).Visible := False ;

    stl := TStyledControl.LookupStyleObject(
              oParent, oParent, TControl(oParent).Scene, '', 'treeviewitemstyle', '', True
            );
    if not ( Assigned( stl ) and ( stl is TControl ) ) then
      exit ;

    FreeObject( oStlItm );
    oStlItmTextSettings := nil ;
    oStlItm := TControl( stl );

    stl2 := stl.FindStyleResource('text') ;

    if Assigned( stl2 ) and ( stl2 is TText ) then
      oStlItmTextSettings := TText( stl2 ).TextSettings ;

    FStyledFont := TFont.Create ;
    FStyledFont.Family := oStlItmTextSettings.Font.Family ;
    FStyledFont.Size := oStlItmTextSettings.Font.Size ;
    FStyledFont.Style := oStlItmTextSettings.Font.Style ;
    FStyledFontColor := oStlItmTextSettings.FontColor ;
  end ;
{$ENDIF}

{$IFDEF LEVEL_RX10_FMX}
  procedure TGIS_StyleRendererFMX.ApplyStyledSettings(
    _settings : TGIS_ControlLegendStyledSettings
  ) ;
  begin
    FStyledSettings := _settings ;
  end ;
{$ENDIF}

  procedure TGIS_StyleRendererFMX.DrawBackground(
    const _canvas : TCanvas ;
    const _width  : Integer ;
    const _height : Integer
  ) ;

    {$IFDEF LEVEL_RX10_FMX}
      procedure draw_styled_background ;
      begin
        if not ( TGIS_ControlLegendStyledSetting.Background in FStyledSettings ) then exit ;

        if Assigned( oStlWnd ) then begin
          oStlWnd.Width  := _width ;
          oStlWnd.Height := _height ;

          oStlWnd.PaintTo(
            _canvas,
            RectF(
              0,
              0,
              oStlWnd.Width,
              oStlWnd.Height
            )
          ) ;
        end ;

        if Assigned( oStlCtl ) then begin
          oStlCtl.Width  := _width  + oStlCtl.Padding.Left + oStlCtl.Padding.Right ;
          oStlCtl.Height := _height + oStlCtl.Padding.Top  + oStlCtl.Padding.Bottom ;

          oStlCtl.PaintTo(
            _canvas,
            RectF(
              -oStlCtl.Padding.Left,
              -oStlCtl.Padding.Top,
              oStlCtl.Width  + oStlCtl.Padding.Left,
              oStlCtl.Height + oStlCtl.Padding.Top
            )
          ) ;
        end ;
      end ;
    {$ENDIF}

  begin
    _canvas.Fill.Kind := TBrushKind.Solid ;
    _canvas.Fill.Color := GetBackgroundColor ;
    _canvas.FillRect( RectF( 0, 0, _width, _height ), 0, 0,
      [ TCorner.TopLeft,
        TCorner.TopRight,
        TCorner.BottomLeft,
        TCorner.BottomRight
      ]
      , 1
    ) ;
    {$IFDEF LEVEL_RX10_FMX}
      draw_styled_background ;
    {$ENDIF}
  end ;

  procedure TGIS_StyleRendererFMX.CreateContext(
    const _canvas : TCanvas ;
    const _rtl    : Boolean
  ) ;
  var
    fnt : TFont ;
  begin
    oCanvas := _canvas ;
    fnt := TFont.Create ;
    try
      fnt.Family := GetActualFontFamily ;
      fnt.Size := GetActualFontSize * sCanvasScale ;
      fnt.Style := GetActualFontStyle ;
      oCanvas.Font.Assign( fnt ) ;
    finally
      FreeObject( fnt ) ;
    end;
    bRightToLeft := _rtl ;
  end ;

  procedure TGIS_StyleRendererFMX.FreeContext ;
  begin
    oCanvas := nil ;
  end ;

  function TGIS_StyleRendererFMX.GetBrushColor
    : TGIS_Color ;
  begin
    Result := TGIS_Color.None ;
  end ;

  procedure TGIS_StyleRendererFMX.SetBrushColor(
    const _selected : Boolean
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    oCanvas.Fill.Kind := TBrushKind.Solid ;
    oCanvas.Fill.Color := GetActualColor( _selected ) ;
    oCanvas.Stroke.Kind := TBrushKind.Solid ;
    oCanvas.Stroke.Color := GetActualColor( _selected ) ;
    oCanvas.Stroke.Thickness := 0.5 ;
  end ;

  procedure TGIS_StyleRendererFMX.DrawCheckBox(
    const _checked : Boolean ;
    const _rect    : TRect
  ) ;
  var
    style : TFmxObject ;
    bmp   : TBitmap ;
    sname : String ;
    strm  : TStream ;
    sym   : TGIS_SymbolAbstract ;
    check : TCheckBox ;
    drawn : Boolean ;
    {$IFDEF LEVEL_RX10_FMX}
      nn  : TMatrix ;
      rf  : TRectF ;
    {$ENDIF}
  begin
    if not assigned( oCanvas ) then exit ;
    if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;

    style := nil ;

    prepareFmxObject ;

    drawn := False ;
    {$IFDEF LEVEL_RX10_FMX}
      if assigned( oFmx ) then begin
        if TGIS_ControlLegendStyledSetting.Checkbox in FStyledSettings then begin
          check := TCheckBox.Create( nil ) ;
          try
            check.IsChecked := _checked ;
            check.SetNewScene( oFmx.Scene ) ;
            check.ApplyStyleLookup ;
            nn := oCanvas.Matrix ;
            try
              TStyleObject( check ).PaintTo(
                oCanvas,
                RectF( _rect.Left, _rect.Top,
                       _rect.Left + TStyleObject( check ).Width * sCanvasScale,
                       _rect.Top + TStyleObject( check ).Height * sCanvasScale )
              );
            finally
              oCanvas.SetMatrix( nn ) ;
            end ;
          finally
            check.Free ;
          end ;
          drawn := True ;
        end ;
      end ;
    {$ENDIF}

    if not drawn then begin
      // fall back drawing
      if _checked then begin
        sname := GIS_LEGEND_CHECKED ;
        strm  := TStream( lstSymbolStreams[ 0 ] ) ;
      end
      else begin
        sname := GIS_LEGEND_UNCHECKED ;
        strm  := TStream( lstSymbolStreams[ 1 ] ) ;
      end ;
      try
        sym := SymbolList.Prepare( sname, strm ) ;
        if assigned( sym ) then begin
          try
            drawSymbol( sym, _rect ) ;
          finally
            SymbolList.InternalDelete( sname ) ;
          end ;
        end ;
      except
      end ;
    end ;
  end ;

  procedure TGIS_StyleRendererFMX.DrawExpandCollapseMarker(
    const _expanded : Boolean ;
    const _rect     : TRect
  ) ;
  var
    sname : String ;
    strm  : TStream ;
    sym   : TGIS_SymbolAbstract ;
  begin
    if not assigned( oCanvas ) then exit ;
    if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;

    if _expanded then begin
      sname := GIS_LEGEND_EXPANDED ;
      strm  := TStream( lstSymbolStreams[ 3 ] ) ;
    end
    else begin
      sname := GIS_LEGEND_COLLAPSED ;
      strm  := TStream( lstSymbolStreams[ 2 ] ) ;
    end ;

    try
      sym := SymbolList.Prepare( sname, strm ) ;
      if assigned( sym ) then begin
        try
          drawSymbol( sym, _rect ) ;
        finally
          SymbolList.InternalDelete( sname ) ;
        end ;
      end ;
    except
    end ;
  end ;

  procedure TGIS_StyleRendererFMX.DrawRectangle(
    const _rect : TRect
  ) ;
  begin
    oCanvas.FillRect( RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
                      0, 0,
                      [ TCorner.TopLeft, TCorner.TopRight,
                        TCorner.BottomLeft, TCorner.BottomRight
                      ]
                      , 1
                    ) ;
    oCanvas.DrawRect( RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
                      0, 0,
                      [ TCorner.TopLeft, TCorner.TopRight,
                        TCorner.BottomLeft, TCorner.BottomRight
                      ]
                      , 1
                    ) ;
  end ;

  procedure TGIS_StyleRendererFMX.DrawRectangle(
    const _left   : Integer ;
    const _top    : Integer ;
    const _width  : Integer ;
    const _height : Integer ;
    const _brush  : TGIS_Color ;
    const _pen    : TGIS_Color
  ) ;
  var
    cnv : TCanvas ;
    bcl : TAlphaColor ;
    pcl : TAlphaColor ;
    thk : Single ;
    rct : TRectF ;
  begin
    if not assigned( oCanvas ) then exit ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := oCanvas ;
    bcl := cnv.Fill.Color ;
    pcl := cnv.Stroke.Color ;
    thk := cnv.Stroke.Thickness ;
    try
      cnv.Fill.Color := FMXColor( _brush ) ;
      cnv.Stroke.Color := FMXColor( _pen ) ;
      cnv.Stroke.Thickness := 1 ;
      rct := RectF( _left - 0.5, _top - 0.5, _left + _width - 0.5, _top + _height - 0.5 ) ;
      cnv.FillRect( rct, 0, 0,
                    [ TCorner.TopLeft, TCorner.TopRight,
                      TCorner.BottomLeft, TCorner.BottomRight
                    ]
                    , 1
                  ) ;
      cnv.DrawRect( rct, 0, 0,
                    [ TCorner.TopLeft, TCorner.TopRight,
                      TCorner.BottomLeft, TCorner.BottomRight
                    ]
                    , 1
                  ) ;
    finally
      cnv.Fill.Color := bcl ;
      cnv.Stroke.Color := pcl ;
      cnv.Stroke.Thickness := thk ;
    end ;
  end ;

  function TGIS_StyleRendererFMX.GetTextExtent(
    const _selected : Boolean ;
    const _text     : String
  ) : TPoint ;
  var
    bmp : TBitmap ;
    cnv : TCanvas ;
    rct : TRectF ;
    tl  : TTextLayout ;
  begin
    bmp := nil ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
    if assigned( oCanvas ) then
      cnv := oCanvas
    else begin
      bmp := TBitmap.Create( 10, 10 ) ;
      cnv := bmp.Canvas ;
    end ;

    if cnv.Font.Size = 0 then begin
      Result := Point( 0, 0 ) ;
      exit ;
    end ;

    rct := RectF( 0, 0, 32000, 32000 ) ;
    tl := TTextLayoutManager.TextLayoutByCanvas( cnv.ClassType ).Create( cnv ) ;
    try
      tl.BeginUpdate ;
      tl.TopLeft := PointF( rct.Left , rct.Top );
      tl.MaxSize := PointF( rct.Width, rct.Height);
      tl.Text := _text ;
      tl.WordWrap := False;
      tl.Opacity := 1;
      if bRightToLeft then
        tl.HorizontalAlign := TTextAlign.Trailing
      else
        tl.HorizontalAlign := TTextAlign.Leading ;
      tl.VerticalAlign := TTextAlign.Leading;
      tl.Font := cnv.Font;
      tl.Color :=  cnv.Stroke.Color ;
      tl.RightToLeft := False;
      tl.EndUpdate;
      rct := tl.TextRect ;
    finally
      FreeObject( tl ) ;
    end ;
    Result.X := RoundS( rct.Right - rct.Left ) ;
    Result.Y := RoundS( rct.Bottom ) ;
    if assigned( bmp ) then
      FreeObject( bmp ) ;
  end ;

  procedure TGIS_StyleRendererFMX.DrawText(
    const _selected : Boolean ;
    const _text     : String  ;
    const _rect     : TRect
  ) ;
  var
    cnv : TCanvas ;
    ta  : TTextAlign ;
    bcl : TAlphaColor ;
  begin
    if not assigned( oCanvas ) then exit ;
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := oCanvas ;
    if bRightToLeft then
      ta := TTextAlign.Trailing
    else
      ta := TTextAlign.Leading ;
    bcl := cnv.Fill.Color ;
    try
      cnv.Fill.Color := GetActualFontColor( _selected ) ;
      cnv.FillText( RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
                    _text, False, 1.0, [], ta ) ;
    finally
      cnv.Fill.Color := bcl ;
    end ;
  end ;

  procedure TGIS_StyleRendererFMX.DrawImage(
    const _bitmap      : TGIS_Bitmap ;
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
    bmp := TBitmap( _bitmap.NativeBitmap ) ;
    cnv.DrawBitmap( bmp,
                    RectF( 0, 0, bmp.Width, bmp.Height ),
                    RectF( _left, _top, _left + bmp.Width, _top + bmp.Height ),
                    1
                  ) ;
    cnv.Flush ;
  end ;

  procedure TGIS_StyleRendererFMX.CreateTemporaryContext(
    const _width  : Integer ;
    const _height : Integer ;
    const _ppi    : Integer
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    FreeObject( oCanvasBitmap ) ;
    oCanvasBitmap := TBitmap.Create ;
    if bRightToLeft then
      oCanvasBitmap.SetSize( _width-1, _height )
    else
      oCanvasBitmap.SetSize( _width,   _height ) ;
    oCanvasBitmap.Canvas.Fill.Kind := oCanvas.Fill.Kind ;
    oCanvasBitmap.Canvas.Fill.Color := oCanvas.Fill.Color  ;
    oCanvasBitmap.Canvas.Stroke.Kind := oCanvas.Stroke.Kind ;
    oCanvasBitmap.Canvas.Stroke.Color := oCanvas.Stroke.Color ;
    oCanvasBitmap.Canvas.Stroke.Thickness := oCanvas.Stroke.Thickness ;
    oCanvasBitmap.Canvas.Font.Assign( oCanvas.Font ) ;
    oCanvasBitmap.Canvas.BeginScene() ;
    oCanvasBitmap.Canvas.Clear( TAlphaColorRec.Null ) ;
  end ;

  procedure TGIS_StyleRendererFMX.RenderTemporaryContext(
    const _left : Integer ;
    const _top  : Integer
  ) ;
  begin
    if not assigned( oCanvas ) then exit ;
    oCanvasBitmap.Canvas.EndScene() ;
    oCanvas.DrawBitmap( oCanvasBitmap,
                        RectF( 0, 0, oCanvasBitmap.Width, oCanvasBitmap.Height ),
                        RectF( _left, _top,
                               _left + oCanvasBitmap.Width,
                               _top + oCanvasBitmap.Height ),
                        1
                      ) ;
    FreeObject( oCanvasBitmap ) ;
  end ;

//==================================== END =====================================
end.

