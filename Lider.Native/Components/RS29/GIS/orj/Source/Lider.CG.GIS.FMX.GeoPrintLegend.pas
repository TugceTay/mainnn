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
  Legend for printing.
}

unit FMX.GisPrintLegend ;
{$HPPEMIT '#pragma link "FMX.GisPrintLegend"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  FMX.Graphics,

  GisInterfaces,
  GisTypesUI,
  GisRendererAbstract,
  GisLayer,
  GisLegend,
  GisLegendUtils,
  FMX.GisLegendUtilsFactory,
  FMX.GisFramework,
  FMX.GisViewerWnd,
  FMX.GisControlLegend;

type

  /// <summary>
  ///   Legend for printing.
  /// </summary>
  TGIS_PrintLegend = class
    private
      oViewer     : IGIS_Viewer ;
      oBitmap     : TBitmap ;
      oLegend     : TGIS_ControlLegend ;
      oBMPFactory : TGIS_LegendIconFactory ;
      oRenderer   : TGIS_RendererAbstract ;
    private
      FMode         : TGIS_ControlLegendMode ;
      FReverseOrder : Boolean ;
      FRTL          : Boolean ;
      FFont         : TFont ;
      FFontColor    : TAlphaColor ;
      FCompactView  : Boolean ;
      FDrawIconStyle: TGIS_LegendIconStyle ;
    private
      sizRow    : Integer ;
      sizRowIco : Integer ;
      sizColIco : Integer ;
      sizIco    : Integer ;
      sizTopMrg : Integer ;
      sizMrg    : Integer ;
      sizLvl    : Integer ;
    private
      function  fget_Mode         : TGIS_ControlLegendMode ;
      procedure fset_Mode         ( const _value : TGIS_ControlLegendMode
                                  ) ;
      function  fget_ReverseOrder : Boolean ;
      procedure fset_ReverseOrder ( const _value : Boolean
                                  ) ;
      function  fget_RTL          : Boolean ;
      procedure fset_RTL          ( const _value : Boolean
                                  ) ;
      function  fget_Font         : TFont ;
      procedure fset_Font         ( const _value : TFont
                                  ) ;
      function  fget_FontColor    : TAlphaColor ;
      procedure fset_FontColor    ( const _value : TAlphaColor
                                  ) ;
      function  fget_CompactView  : Boolean ;
      procedure fset_CompactView  ( const _value : Boolean
                                  ) ;
      function  fget_DrawIconStyle: TGIS_LegendIconStyle ;
      procedure fset_DrawIconStyle( const _value : TGIS_LegendIconStyle
                                  ) ;
    private

      /// <summary>
      ///   Draws an legend item representing a layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be represented by the legend item
      /// </param>
      /// <param name="_index">
      ///   -1 or index of sublayer
      /// </param>
      /// <param name="_order">
      ///   the Y position of the legend item
      /// </param>
      /// <param name="_level">
      ///   the level of the legend item
      /// </param>
      procedure drawLayer         ( const _layer : TGIS_Layer ;
                                    const _index : Integer ;
                                      var _order : Integer ;
                                      var _level : Integer
                                  ) ;
      /// <summary>
      ///   Draws the legend in the Layers mode.
      /// </summary>
      procedure drawLayers ;
      /// <summary>
      ///   Draws the legend in the Groups mode.
      /// </summary>
      procedure drawGroups ;

    public

      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      /// <param name="_legend">
      ///   the legend component which is to be represented on the printout
      /// </param>
      /// <param name="_bitmap">
      ///   the bitmap to draw on
      /// </param>
      constructor Create ( const _legend : TGIS_ControlLegend ;
                           const _bitmap : TGIS_Bitmap
                         ) ; overload ;

    public
      /// <summary>
      ///   Destructor, destroys the instance.
      /// </summary>
      destructor Destroy ; override;
    public
      /// <summary>
      ///   Sets the dimensions of the printout bitmap.
      /// </summary>
      /// <param name="_width">
      ///   width (in pixels) of the legend image prepared for printing
      /// </param>
      /// <param name="_height">
      ///   height (in pixels) of the legend image prepared for printing
      /// </param>
      procedure SetSize ( const _width  : Integer ;
                          const _height : Integer
                        ) ;
      /// <summary>
      ///   Draws the printout bitmap based on the current state of the
      ///   associated TGIS_ControlLegend.
      /// </summary>
      procedure Draw    ; overload ;
      /// <summary>
      ///   Draws the printout bitmap based on the current state of the
      ///   associated TGIS_ControlLegend.
      /// </summary>
      /// <param name="_scale">
      ///   scale of the map; if 0 then map scale will be used
      /// </param>
      /// <param name="_ppi">
      ///   force PPI resolution; if 0 then set by corresponding GIS_Viewer
      ///   object
      /// </param>
      procedure Draw    ( const _scale : Double ;
                          const _ppi : Integer
                        ) ; overload ;
    public
      /// <summary>
      ///   Mode of the legend - list of layers or grouped tree view.
      /// </summary>
      property Mode         : TGIS_ControlLegendMode
                              read  fget_Mode
                              write fset_Mode ;
      /// <summary>
      ///   True if the order of legend entries in the Layer mode should be
      ///   reverse, i.e. the topmost layer in the attached TGIS_ViewerWnd
      ///   is bottommost in the legend.
      /// </summary>
      property ReverseOrder : Boolean
                              read  fget_ReverseOrder
                              write fset_ReverseOrder ;
      /// <summary>
      ///   If True, then the legend will be drawn from right to left.
      /// </summary>
      property RightToLeft  : Boolean
                              read  fget_RTL
                              write fset_RTL ;
      /// <summary>
      ///   Display font.
      /// </summary>
      property Font         : TFont
                              read  fget_Font
                              write fset_Font ;
      /// <summary>
      ///   Display font color.
      /// </summary>
      property FontColor    : TAlphaColor
                              read  fget_FontColor
                              write fset_FontColor ;
      /// <summary>
      ///   If True then the legend view is compacted - icons are smaller.
      /// </summary>
      property CompactView  : Boolean
                              read  fget_CompactView
                              write fset_CompactView ;
      /// <summary>
      ///   Draw style of legend icons.
      /// </summary>
      property DrawIconStyle  : TGIS_LegendIconStyle
                                read  fget_DrawIconStyle
                                write fset_DrawIconStyle
                                default TGIS_LegendIconStyle.Default ;
  end ;


//##############################################################################
implementation

uses
  System.SysUtils,
  System.Math,
  FMX.Types,

  GisRtl,
  GisTypes,
  GisClasses,
  GisParams,
  GisLayerVector,
  FMX.GisRenderer ;

//==============================================================================
// TGIS_PrintLegend
//==============================================================================

  constructor TGIS_PrintLegend.Create(
    const _legend : TGIS_ControlLegend ;
    const _bitmap : TGIS_Bitmap
  ) ;
  begin
    inherited Create ;

    oLegend := _legend ;
    oViewer := oLegend.GIS_Viewer ;
    oBitmap := TBitmap( _bitmap.NativeBitmap ) ;

    FMode := TGIS_ControlLegendMode.Layers ;
    FReverseOrder := False ;
    FRTL := False ;
    FFont := TFont.Create ;
    FFont.Assign( oBitmap.Canvas.Font ) ;
    FFontColor := TAlphaColorRec.Black ;
    FCompactView := False ;
    FDrawIconStyle := TGIS_LegendIconStyle.Default ;
  end ;

  destructor TGIS_PrintLegend.Destroy ;
  begin
    FreeObject( FFont ) ;

    inherited ;
  end ;

  function TGIS_PrintLegend.fget_Mode : TGIS_ControlLegendMode ;
  begin
    if Assigned( oLegend ) then
      Result := oLegend.Mode
    else
      Result := FMode ;
  end ;

  procedure TGIS_PrintLegend.fset_Mode(
    const _value : TGIS_ControlLegendMode
  ) ;
  begin
    if not assigned( oLegend ) then
      FMode := _value ;
  end ;

  function TGIS_PrintLegend.fget_ReverseOrder : Boolean ;
  begin
    if Assigned( oLegend ) then
      Result := oLegend.ReverseOrder
    else
      Result := FReverseOrder ;
  end ;

  procedure TGIS_PrintLegend.fset_ReverseOrder(
    const _value : Boolean
  ) ;
  begin
    if not assigned( oLegend ) then
      FReverseOrder := _value ;
  end ;

  function TGIS_PrintLegend.fget_RTL : Boolean ;
  begin
    if Assigned( oLegend ) then
      Result := False
    else
      Result := FRTL ;
  end ;

  procedure TGIS_PrintLegend.fset_RTL(
    const _value : Boolean
  ) ;
  begin
    if not Assigned( oLegend ) then
      FRTL := _value ;
  end ;

  function TGIS_PrintLegend.fget_Font : TFont ;
  begin
    if Assigned( oLegend ) then
      Result := oLegend.Font
    else
      Result := FFont ;
  end ;

  procedure TGIS_PrintLegend.fset_Font(
    const _value : TFont
  ) ;
  begin
    if not Assigned( oLegend ) then
      FFont.Assign( _value ) ;
  end ;

  function TGIS_PrintLegend.fget_FontColor : TAlphaColor ;
  begin
    if Assigned( oLegend ) then
      Result := oLegend.FontColor
    else
      Result := FFontColor ;
  end ;

  procedure TGIS_PrintLegend.fset_FontColor(
    const _value : TAlphaColor
  ) ;
  begin
    if not Assigned( oLegend ) then
      FFontColor := _value ;
  end ;

  function TGIS_PrintLegend.fget_CompactView
    : Boolean ;
  begin
    if Assigned( oLegend ) then
      Result := oLegend.CompactView
    else
      Result := FCompactView ;
  end ;

  procedure TGIS_PrintLegend.fset_CompactView(
    const _value : Boolean
  ) ;
  begin
    if Assigned( oLegend ) then
      exit ;

    if FCompactView <> _value then begin
      FreeObject( oBMPFactory ) ;
      oBMPFactory := TGIS_LegendIconFactory.Create( sizIco, _value, FDrawIconStyle ) ;
      FCompactView := _value ;
    end ;
  end ;

  function TGIS_PrintLegend.fget_DrawIconStyle : TGIS_LegendIconStyle ;
  begin
    if Assigned( oLegend ) then
      Result := oLegend.DrawIconStyle
    else
      Result := FDrawIconStyle ;
  end ;

  procedure TGIS_PrintLegend.fset_DrawIconStyle(
    const _value : TGIS_LegendIconStyle
  ) ;
  begin
    if Assigned( oLegend ) then
      exit ;

    if FDrawIconStyle <> _value then begin
      FreeObject( oBMPFactory ) ;
      oBMPFactory := TGIS_LegendIconFactory.Create( sizIco, FCompactView, _value ) ;
      FDrawIconStyle := _value ;
    end ;
  end ;

  procedure TGIS_PrintLegend.drawLayer(
    const _layer : TGIS_Layer ;
    const _index : Integer ;
      var _order : Integer ;
      var _level : Integer
  ) ;
  const
    LOCAL_ARR_CHART : array[0..9] of String = (
      '',
      '',
      '::5::::::::',
      ':::5:::::::',
      '::::5::::::',
      ':::::5:::::',
      '::::::5::::',
      ':::::::5:::',
      '::::::::5::',
      ':::::::::5:'
    ) ;
  var
    la  : TGIS_Layer ;
    lv  : TGIS_LayerVector ;
    str : String ;
    w   : Integer ;
    l0  : Integer ;
    i   : Integer ;
    dt  : TGIS_LegendItemData ;

    procedure draw_bitmap(
      const _l : Integer ; const _t : Integer ; const _b : TGIS_Bitmap
    ) ;
    var
      rr : TRect ;
    begin
      if RightToLeft then
        rr := Rect( oBitmap.Width - _l - _b.Width, _t + 3,
                    oBitmap.Width - _l, _t + _b.Height + 3 )
      else
        rr := Rect( _l, _t + 3, _l + _b.Width, _t + _b.Height + 3 ) ;
      oRenderer.CanvasDrawBitmap( _b, rr ) ;
      oRenderer.Flush ;
    end ;

    procedure draw_renderer_zones(
      const _s         : TGIS_ParamsSectionVector ;
      const _l         : Integer ;
      const _lico      : Integer ;
      var   _t         : Integer ;
      const _tmrg      : Integer ;
      const _hh        : Integer ;
      const _useZoneEx : Boolean
    ) ;
    var
      legend_txt         : String  ;
      legend_txt_extent  : TPoint  ;
      legend_txt_rect    : TRect   ;
      i_zone             : Integer ;
      zones_count        : Integer ;
      zone_index_sign    : Integer ;
    begin
      if _useZoneEx then begin
        zones_count :=  _s.Render.ZonesEx ;
        zone_index_sign := -1 ;
      end
      else begin
        zones_count :=  _s.Render.Zones ;
        zone_index_sign := 1 ;
      end;

      for i_zone := 1 to Abs( zones_count ) do begin
        if oBMPFactory.SetParamsRenderer( lv, _s.InternalIndex,
                                          zone_index_sign * i_zone ) then
        begin
          draw_bitmap( _l, _t + 1, oBMPFactory.GetBitmap ) ;
          legend_txt := oBMPFactory.Layer.Params.Legend ;
        end
        else begin
          // data cannot be retrived; probably shouldn't happen
          legend_txt := '[ no data ]' ;
        end ;
        if RightToLeft then begin
          legend_txt_extent := oRenderer.CanvasTextExtent( legend_txt ) ;
          legend_txt_rect := Rect( oBitmap.Width - _l - legend_txt_extent.X,
                                   _t + _tmrg,
                                   oBitmap.Width - _l, _t + _tmrg + _hh )
        end
        else
          legend_txt_rect := Rect( _l + _lico,
                                   _t + _tmrg,
                                   oBitmap.Width, _t + _tmrg + _hh ) ;

        oRenderer.CanvasDrawText( legend_txt_rect, legend_txt ) ;

        inc( _t, sizRowIco ) ;
      end ;
    end ;

    procedure draw_params_vector_renderer(
      const _s : TGIS_ParamsSectionVector ;
        var _t : Integer
    ) ;
    var
      ii : Integer ;
    begin
      if (_s.Render.MinVal < _s.Render.MinValEx) or
         (_s.Render.MaxVal < _s.Render.MaxValEx) then
      begin
        // first draw Zones, then ZonesEx
        draw_renderer_zones( _s, l0, sizColIco, _t, sizTopMrg, sizRow, False ) ;
        draw_renderer_zones( _s, l0, sizColIco, _t, sizTopMrg, sizRow, True ) ;
      end
      else begin
        // first draw ZonesEx, then Zones (historiacal, deprecated)
        draw_renderer_zones( _s, l0, sizColIco, _t, sizTopMrg, sizRow, True ) ;
        draw_renderer_zones( _s, l0, sizColIco, _t, sizTopMrg, sizRow, False ) ;
      end ;
    end ;

    procedure draw_params_vector( const _data : TGIS_LegendItemData ) ;
    var
      src  : TGIS_ParamsSectionVector ;
      fact : TGIS_LegendIconFactory ;
      l    : Integer ;
      t    : Integer ;
      bb   : Boolean ;
      ii   : Integer ;
      t0   : Integer ;
    begin
      if _data.DataType <> TGIS_LegendItemType.Params then
        exit ;

      lv := TGIS_LayerVector( _layer ) ;

      l := 0 ;
      t := 0 ;
      if _data.RowCount >= 0 then begin
        l0 := 6 + ( _level * 2 - 1 ) * sizLvl ;
        t0 := _order ;

        fact := oBMPFactory ;

        src := TGIS_ParamsSectionVector( _data.Params ) ;

        t := t0 ;
        oRenderer.CanvasBrush.Color := TGIS_Color.White ;
        fact.SetColor( oRenderer.CanvasBrush.Color ) ;
        if ( Length( src.Legend ) <> 0 ) and ( _data.FeatureCount > 1 ) then
          Inc( t, sizRow ) ;

        bb := False ;
        if src.Marker.ShowLegend then begin
          fact.IconType := TGIS_LegendIconType.Marker ;
          if _data.RenderMarker then
            draw_params_vector_renderer( src, t )
          else begin
            fact.SetParams( src ) ;
            draw_bitmap( l0+l, t+1, fact.GetBitmap ) ;
            if _data.Render then
              Inc( t, sizRowIco )
            else
              Inc( l, sizColIco ) ;
            bb := True ;
          end ;
        end ;

        if src.Line.ShowLegend then begin
          fact.IconType := TGIS_LegendIconType.Line ;
          if _data.RenderLine then
            draw_params_vector_renderer( src, t )
          else begin
            fact.SetParams( src ) ;
            draw_bitmap( l0+l, t+1, fact.GetBitmap ) ;
            if _data.Render then
              Inc( t, sizRowIco )
            else
              Inc( l, sizColIco ) ;
            bb := True ;
          end ;
        end ;

        if src.Area.ShowLegend then begin
          fact.IconType := TGIS_LegendIconType.Area ;
          if _data.RenderArea then
            draw_params_vector_renderer( src, t )
          else begin
            fact.SetParams( src ) ;
            draw_bitmap( l0+l, t+1, fact.GetBitmap ) ;
            if _data.Render then
              Inc( t, sizRowIco )
            else
              Inc( l, sizColIco ) ;
            bb := True ;
          end ;
        end ;

        if src.Labels.ShowLegend then begin
          fact.IconType := TGIS_LegendIconType.Label ;
          if _data.RenderLabel then
            draw_params_vector_renderer( src, t )
          else begin
            fact.SetParams( src ) ;
            draw_bitmap( l0+l, t+1, fact.GetBitmap ) ;
            bb := True ;
          end ;
        end ;

        if bb then
          Inc( t, sizRowIco ) ;
        if src.Chart.ShowLegend and
           ( not IsStringEmpty( _data.ChartMap ) ) then begin
          fact.IconType := TGIS_LegendIconType.Chart ;
          fact.Layer.Params.Chart.ColorsInternal := src.Chart.ColorsInternal ;

          for ii := 1 to Length( _data.ChartMap ) do begin
            fact.Layer.Params.Chart.Style := src.Chart.Style ;
            if _data.ChartMap[ii] = '1' then begin
              if src.Chart.Style = TGIS_ChartStyle.Pie then
                fact.Layer.Params.Render.Chart := LOCAL_ARR_CHART[ii-1]+'3'
              else
                fact.Layer.Params.Render.Chart := LOCAL_ARR_CHART[ii-1] ;
            end
            else
              continue ;

            draw_bitmap( l0, t+1, fact.GetBitmap ) ;

            oRenderer.CanvasDrawText(
              Rect(
                l0+sizColIco, t+sizTopMrg,
                oBitmap.Width, t+sizTopMrg+sizRow
              ),
              _data.ChartLegend[ii-1]
            ) ;
            Inc( t, sizRowIco ) ;
          end ;
        end ;

        Inc( _order, t-t0 ) ;

        l := 0 ;
        t := 0 ;
        if _data.FeatureCount = 1 then begin
          l := sizColIco ;
          t := sizTopMrg ;
        end ;

        if RightToLeft then begin
          w := oRenderer.CanvasTextExtent( _data.Params.Legend ).X ;
          oRenderer.CanvasDrawText(
            Rect( oBitmap.Width - l0 - l - w, t0+t,
                  oBitmap.Width - l0 - l, t0+t+sizRow ),
            _data.Params.Legend
          ) ;
        end
        else begin
          oRenderer.CanvasDrawText(
            Rect( l0+l, t0+t, oBitmap.Width, t0+t+sizRow ),
            _data.Params.Legend
          ) ;
        end ;

      end ;
    end ;

    procedure draw_params_pixel( const _data : TGIS_LegendItemData ) ;
    var
      ppix : TGIS_ParamsPixel ;
      tkn  : TGIS_Tokenizer ;
      gcl  : TGIS_Color ;
      ii   : Integer ;
      jj   : Integer ;
      l0   : Integer ;
      t0   : Integer ;
    begin
      if _data.DataType <> TGIS_LegendItemType.Params then
        exit ;

      l0 := 6 + ( _level * 2 - 1 ) * sizLvl ;
      t0 := _order ;

      ppix := TGIS_ParamsSectionPixel( _data.Params ).Pixel ;

      if not ppix.ShowLegend then exit ;

      jj := 0 ;
      tkn := TGIS_Tokenizer.Create ;
      try
        for ii := 0 to ppix.AltitudeMapZones.Count - 1 do begin
          tkn.ExecuteEx( ppix.AltitudeMapZones[ii] ) ;

          if tkn.Result.Count < 4 then
            continue ;

          gcl := ParamColor( tkn.Result.Strings[2], TGIS_Color.None ) ;

          oRenderer.CanvasPen.Color := gcl ;
          oRenderer.CanvasBrush.Color := gcl ;

          if RightToLeft then
            oRenderer.CanvasDrawRectangle(
              Rect( oBitmap.Width - l0 - sizIco, _order,
                    oBitmap.Width - l0, _order + sizRow )
            )
          else
            oRenderer.CanvasDrawRectangle(
              Rect( l0, _order, l0 + sizIco, _order + sizRow )
            ) ;

          oRenderer.CanvasPen.Color := TGIS_Color.Black ;
          oRenderer.CanvasBrush.Color := TGIS_Color.White ;

          if RightToLeft then begin
            w := oRenderer.CanvasTextExtent( tkn.Result.Strings[3] ).X ;
            oRenderer.CanvasDrawText(
              Rect(
                oBitmap.Width - l0 - sizIco - sizMrg - w,
                _order + RoundS( sizMrg/3 ),
                oBitmap.Width - l0 - sizIco - sizMrg,
                _order + RoundS( sizMrg/3 ) + sizRow
              ),
              tkn.Result.Strings[3]
            ) ;
          end
          else begin
            oRenderer.CanvasDrawText(
              Rect(
                l0 + sizIco + sizMrg, _order + RoundS( sizMrg/3 ),
                oBitmap.Width, _order + RoundS( sizMrg/3 ) + sizRow
              ),
              tkn.Result.Strings[3]
            ) ;
          end ;

          Inc( _order, sizRow + sizMrg ) ;

          Inc( jj ) ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

  begin

    if _index = -1 then
      la := _layer
    else
      la := TGIS_Layer( _layer.SubLayers[_index] ) ;

    str := la.Caption ;

    if _order > oBitmap.Height then
      exit ;

    oRenderer.CanvasFont.Name  := Font.Family ;
    oRenderer.CanvasFont.Size  := RoundS( Font.Size ) ;
    oRenderer.CanvasFont.Style := [] ;
    if TFontStyle.fsBold in Font.Style then
      oRenderer.CanvasFont.Style := oRenderer.CanvasFont.Style + [TGIS_FontStyle.Bold] ;
    if TFontStyle.fsItalic in Font.Style then
      oRenderer.CanvasFont.Style := oRenderer.CanvasFont.Style + [TGIS_FontStyle.Italic] ;
    if TFontStyle.fsUnderline in Font.Style then
      oRenderer.CanvasFont.Style := oRenderer.CanvasFont.Style + [TGIS_FontStyle.Underline] ;
    if TFontStyle.fsStrikeOut in Font.Style then
      oRenderer.CanvasFont.Style := oRenderer.CanvasFont.Style + [TGIS_FontStyle.StrikeOut] ;
    oRenderer.CanvasFont.Color := GISColor( FontColor ) ;

    if RightToLeft then begin
      w := oRenderer.CanvasTextExtent( str ).X ;
      oRenderer.CanvasDrawText(
        Rect(
          oBitmap.Width - 6 - _level * 2 * sizLvl - w,
          _order + 3,
          oBitmap.Width - 6 - _level * 2 * sizLvl,
          _order + 3 + sizRow
        ),
        str
      ) ;
    end
    else begin
      oRenderer.CanvasDrawText(
        Rect(
          6 + _level * 2 * sizLvl, _order + 3,
          oBitmap.Width, _order + 3 + sizRow
        ),
        str
      ) ;
    end ;

    Inc( _order, sizRow ) ;

    if assigned( oLegend ) then begin
      if not oLegend.IsExpanded( la ) then
        exit ;
    end ;

    if Assigned( oLegend ) and
       ( TGIS_ControlLegendOption.ShowSubLayers in oLegend.Options ) and
       Assigned( la.SubLayers ) then begin
      Inc( _level ) ;
      for i := 0 to la.SubLayers.Count - 1 do
        drawLayer( la, i, _order, _level ) ;
      Dec( _level ) ;
    end ;

    Inc( _level ) ;
    for i := 0 to la.ParamsList.Count - 1 do begin
      if not la.ParamsList[i].Visible then
        continue ;

      dt := TGIS_LegendItemData.Create(
        TGIS_ParamsSection( la.ParamsList[i] ), oViewer
      ) ;
      try
        if dt.IsVector then
          draw_params_vector( dt )
        else
          draw_params_pixel( dt ) ;
      finally
        FreeObject( dt ) ;
      end ;
    end ;
    Dec( _level ) ;
  end ;

  procedure TGIS_PrintLegend.drawLayers ;
  var
    ordr : Integer ;
    lvl  : Integer ;
    i    : Integer ;
  begin
    ordr := 0 ;
    lvl  := 0 ;

    if ReverseOrder then begin
      for i := 0 to oViewer.Items.Count - 1 do begin
        if TGIS_Layer( oViewer.Items[i] ).HideFromLegend or
           not TGIS_Layer( oViewer.Items[i] ).Active then
          continue ;
        drawLayer( TGIS_Layer( oViewer.Items[i] ), -1, ordr, lvl ) ;
      end ;
    end
    else begin
      for i := oViewer.Items.Count - 1 downto 0 do begin
        if TGIS_Layer( oViewer.Items[i] ).HideFromLegend or
           not TGIS_Layer( oViewer.Items[i] ).Active then
          continue ;
        drawLayer( TGIS_Layer( oViewer.Items[i] ), -1, ordr, lvl ) ;
      end ;
    end ;
  end ;

  procedure TGIS_PrintLegend.drawGroups ;
  var
    ordr : Integer ;
    lvl  : Integer ;
    w    : Integer ;
    i    : Integer ;

    procedure draw_group( const _g : IGIS_HierarchyGroup ) ;
    var
      ii   : Integer ;
    begin
      if ordr > oBitmap.Height then
        exit ;

      if RightToLeft then begin
        w := oRenderer.CanvasTextExtent( _g.Caption ).X ;
        oRenderer.CanvasDrawText(
          Rect(
            oBitmap.Width - 6 - lvl * 2 * sizLvl - w,
            ordr + 3,
            oBitmap.Width - 6 - lvl * 2 * sizLvl,
            ordr + 3 + sizRow
          ),
          _g.Caption
        ) ;
      end
      else begin
        oRenderer.CanvasDrawText(
          Rect(
            6 + lvl * 2 * sizLvl, ordr + 3,
            oBitmap.Width, ordr + 3 + sizRow
          ),
          _g.Caption
        ) ;
      end ;

      Inc( ordr, sizRow ) ;

      if assigned( oLegend ) then begin
        if not oLegend.IsExpanded( _g ) then
          exit ;
      end ;

      Inc( lvl ) ;
      for ii := 0 to _g.GroupsCount - 1 do
        draw_group( _g.Groups[ii] ) ;
      for ii := 0 to _g.LayersCount - 1 do begin
        if TGIS_Layer( _g.Layers[ii] ).HideFromLegend or
           not TGIS_Layer( _g.Layers[ii] ).Active then
          continue ;
        drawLayer( TGIS_Layer( _g.Layers[ii] ), -1, ordr, lvl ) ;
      end ;
      Dec( lvl ) ;
    end ;

  begin
    ordr := 0 ;
    lvl  := 0 ;

    for i := 0 to oViewer.Hierarchy.GroupsCount - 1 do
      draw_group( oViewer.Hierarchy.Groups[i] ) ;
  end ;

  procedure TGIS_PrintLegend.SetSize(
    const _width  : Integer ;
    const _height : Integer
  ) ;
  begin
    oBitmap.SetSize( _width, _height ) ;
  end ;

  procedure TGIS_PrintLegend.Draw ;
  begin
    Draw( 0, 0 ) ;
  end;

  procedure TGIS_PrintLegend.Draw(
    const _scale : Double ;
    const _ppi   : Integer
  ) ;
  var
    siz : Integer ;
    rt  : Double ;
    ctx : TGIS_RendererContext ;
    ppi : Integer ;
    scl : Integer ;
    rct : TRect ;
  begin
    if not Assigned( oViewer ) then
      exit ;

    if _ppi = 0 then
      ppi := oViewer.PPI
    else
      ppi := _ppi ;
    scl := oViewer.FontScale ;

    rt := ppi/96.0 ;
    siz := RoundS( Font.Size * rt ) ;

    sizRow      := siz + RoundS( Font.Size * rt ) ;
    sizIco      := RoundS( 28 * rt ) ;
    if CompactView then
      sizRowIco := RoundS( 16 * rt )
    else
      sizRowIco := RoundS( 32 * rt ) ;
    sizColIco   := RoundS( 32 * rt ) ;
    if CompactView then
      sizTopMrg := RoundS( 2 * rt )
    else
      sizTopMrg := RoundS( ( sizIco - siz )/2 ) - ( siz div 3 ) ;
    sizMrg      := RoundS( 3 * rt ) ;
    sizLvl      := RoundS( Font.Size * rt ) ;

    if oViewer is TGIS_ViewerWnd then
      oRenderer := TGIS_ViewerWnd(oViewer).Renderer.CreateInstance
    else
      oRenderer := TGIS_RendererFmx.Create ;
    oBMPFactory := TGIS_LegendIconFactory.Create( sizIco, CompactView, DrawIconStyle ) ;

    ctx := TGIS_RendererContext.Create ;
    try
      ctx.AssignBaseMap( oBitmap, False ) ;

      oRenderer.CreateContext(
        nil, nil, ctx, Point( 0, 0 ), oBitmap.Width, oBitmap.Height, ppi, scl
      ) ;
      oRenderer.CanvasFont.Name := oBitmap.Canvas.Font.Family ;
      oRenderer.CanvasFont.Size := RoundS( oBitmap.Canvas.Font.Size ) ;

      oRenderer.CanvasPen.Color := TGIS_Color.White ;
      oRenderer.CanvasBrush.Color := TGIS_Color.White ;
      rct := Rect( 0, 0, oBitmap.Width, oBitmap.Height ) ;
      oRenderer.CanvasDrawRectangle( rct ) ;

      oBMPFactory.CustomPPI := ppi ;
      if Mode = TGIS_ControlLegendMode.Layers then
        drawLayers
      else
        drawGroups ;

      oRenderer.ReleaseContext ;
    finally
      FreeObject( oBMPFactory ) ;
      FreeObject( oRenderer ) ;
      FreeObject( ctx ) ;
    end ;
  end ;

//==================================== END =====================================
end.

