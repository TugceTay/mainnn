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
  Chart unit.

  Currently supported BAR & PIE charts.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoChart ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoChart"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.Math,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoRendererAbstract ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

  /// <summary>
  ///   Draw a chart of a given location.
  /// </summary>
  /// <param name="_canvas">
  ///   Canvas of device
  /// </param>
  /// <param name="_center">
  ///   origin (center) of chart i canvas coordinates
  /// </param>
  /// <param name="_size">
  ///   size of the chart
  /// </param>
  /// <param name="_style">
  ///   chart style for new chart
  /// </param>
  /// <param name="_values">
  ///   array of values to be represented; first is minimum, second is maximum,
  ///   and then
  /// </param>
  /// <remarks>
  ///   See TGIS_LayerVector.DrawChart for details and example.
  /// </remarks>
  procedure GisDrawChart( const _canvas  : TGIS_RendererAbstract ;
                          const _center  : TPoint                ;
                          const _size    : Integer               ;
                          const _style   : TGIS_ChartStyle       ;
                          const _values  : TGIS_DoubleArray
                        ) ; overload;

  /// <summary>
  ///   Draw a chart of a given location.
  /// </summary>
  /// <param name="_canvas">
  ///   Canvas of device
  /// </param>
  /// <param name="_center">
  ///   origin (center) of chart i canvas coordinates
  /// </param>
  /// <param name="_size">
  ///   size of the chart
  /// </param>
  /// <param name="_style">
  ///   chart style for new chart
  /// </param>
  /// <param name="_values">
  ///   array of values to be represented; first is minimum, second is maximum,
  ///   and then
  /// </param>
  /// <param name="_colors">
  ///   array of colors to be represented
  /// </param>
  /// <remarks>
  ///   See TGIS_LayerVector.DrawChart for details and example.
  /// </remarks>
  procedure GisDrawChart( const _canvas  : TGIS_RendererAbstract ;
                          const _center  : TPoint                ;
                          const _size    : Integer               ;
                          const _style   : TGIS_ChartStyle       ;
                          const _values  : TGIS_DoubleArray      ;
                          const _colors  : TGIS_ColorArray
                        ) ; overload;


//##############################################################################
implementation


  procedure GisDrawChart(
    const _canvas  : TGIS_RendererAbstract ;
    const _center  : TPoint                ;
    const _size    : Integer               ;
    const _style   : TGIS_ChartStyle       ;
    const _values  : TGIS_DoubleArray
  ) ;
  var
    i          : Integer ;
    cnt        : Integer ;
    cnt_tmp    : Integer ;
    tmp        : Double  ;
    total      : Double  ;
    maximum    : Double  ;
    minimum    : Double  ;
    minmax     : Double  ;
    angle_last : Double  ;
    pt_start   : TPoint {$IFDEF JAVA} = new TPoint(0,0) {$ENDIF} ;
    pt_last    : TPoint {$IFDEF JAVA} = new TPoint(0,0) {$ENDIF} ;
    bar_gap,
    bar_width  : Integer ;

    ar_chart_color : TGIS_ColorArray ;

    procedure draw_pie( const _idx : Integer ) ;
    var
      pt    : TPoint {$IFDEF JAVA} = new TPoint(0,0) {$ENDIF} ;
      angle : Double ;
      sa,ca : Double ;
    begin
      if ( total <> 0 ) and ( _values[_idx] > 0 ) then begin
        angle := angle_last + _values[_idx] / total * 2 * Pi ;
      end
      else
        exit ;

      if _idx <> cnt then begin
        SinCos( angle, sa, ca ) ;
        pt.X := _center.X + RoundS( _size * {$IFDEF JAVA}Integer(ca){$ELSE} ca {$ENDIF} ) ;
        pt.Y := _center.Y + RoundS( _size * {$IFDEF JAVA}Integer(sa){$ELSE} sa {$ENDIF} ) ;
      end
      else
        pt := pt_start ;

      if ( pt.X = pt_last.X ) and
         ( pt.Y = pt_last.Y ) and
         ( (_values[_idx] / total) < 0.5 ) then
      begin
        // ignore small amounts
        exit ;
      end ;
      _canvas.CanvasDrawPie(
        angle_last + Pi/2, angle + Pi/2,
        _size,
        _center.X, _center.Y
      ) ;

      angle_last := angle ;
      pt_last    := pt{$IFDEF JAVA}.MakeCopy{$ENDIF}    ;
    end ;

    procedure draw_bar( const _idx : Integer; const _pos : Integer ) ;
    var
      height   : Integer ;
      left     : Integer ;
      right    : Integer ;
      bottom   : Integer ;
      top      : Integer ;
      ar_pt    : TGIS_DrawBuf ;
    begin
      height := RoundS( _values[_idx] / minmax * _size ) ;

      left   := pt_start.X + (_pos)* ( bar_width + bar_gap ) ;
      bottom := pt_start.Y ;
      right  := left + bar_width ;
      top    := bottom - height ;

      SetLength( ar_pt, 4 ) ;
      ar_pt[0] := Point( left , top    ) ;
      ar_pt[1] := Point( right, top    ) ;
      ar_pt[2] := Point( right, bottom ) ;
      ar_pt[3] := Point( left , bottom ) ;

      _canvas.CanvasDrawPolygon( ar_pt ) ;
    end ;

  begin
    if high( _values ) < 2 then exit ;

    SetLength( ar_chart_color, 9 ) ;
    { TODO : Make color list as public for user }
    ar_chart_color[ 0 ] := TGIS_Color.Red     ;
    ar_chart_color[ 1 ] := TGIS_Color.Lime    ;
    ar_chart_color[ 2 ] := TGIS_Color.Blue    ;
    ar_chart_color[ 3 ] := TGIS_Color.Fuchsia ;
    ar_chart_color[ 4 ] := TGIS_Color.Aqua    ;
    ar_chart_color[ 5 ] := TGIS_Color.Green   ;
    ar_chart_color[ 6 ] := TGIS_Color.White   ;
    ar_chart_color[ 7 ] := TGIS_Color.Black   ;
    ar_chart_color[ 8 ] := TGIS_Color.None    ;

    if high( _values ) - 2  > high( ar_chart_color ) then
      cnt := high( ar_chart_color ) + 2
    else
      cnt := high( _values ) ;

    // precomputed values
       total   := 0        ;
       minimum := _values[0] ;
       maximum := _values[1] ;

    // set styles
      _canvas.CanvasPen.Color := TGIS_Color.Black ;
      _canvas.CanvasPen.Width := 1 ;

    // precomputed starting values
       case _style of

         TGIS_ChartStyle.Pie :
           begin
             for i := 2 to cnt do begin
               if _values[i] >= GIS_HALF_MAX_DOUBLE then
                 continue
               else if _values[i] > 0 then
                 total := total + _values[i] ;
             end ;

             angle_last := -Pi/2 ;
             pt_start.X := _center.X + RoundS( _size * Cos( angle_last ) ) ;
             pt_start.Y := _center.Y + RoundS( _size * Sin( angle_last ) ) ;
             pt_last  := pt_start ;

             _canvas.CanvasPen.Style := TGIS_PenStyle.Solid ;
           end ;

         TGIS_ChartStyle.Bar :
           begin

             cnt_tmp := 0 ;
             for i := 2 to cnt do begin
               tmp := _values[i] ;
               if Abs( tmp ) >= GIS_HALF_MAX_DOUBLE then
                 continue
               else if tmp > 0 then begin
                 if tmp > maximum then maximum := tmp ;
               end
               else begin
                 if tmp < minimum then minimum := tmp ;
               end ;
               inc( cnt_tmp )  ;
             end ;
             minmax := Abs(minimum)+maximum ;
             if minmax = 0 then exit ;
             if cnt_tmp = 0 then exit ;

             bar_gap   :=  RoundS( ( 1.0 * _size / cnt_tmp ) * 1.0/5 ) ;
             bar_width :=  RoundS( ( 1.0 * _size / cnt_tmp ) - bar_gap ) ;

             pt_start.X := _center.X - _size div 2 ;
             pt_start.Y := _center.Y + _size div 2
                           + RoundS( minimum / minmax * _size ) ;

             _canvas.CanvasPen.Style := TGIS_PenStyle.Solid ;
             _canvas.CanvasDrawLine(
               _center.X - _size div 2 - bar_gap, pt_start.Y,
               _center.X + _size div 2 + bar_gap, pt_start.Y
             ) ;

             if bar_width > 3 then _canvas.CanvasPen.Style := TGIS_PenStyle.Solid
                              else _canvas.CanvasPen.Style := TGIS_PenStyle.Clear ;

           end ;
         else
           begin
             assert( False ) ;
           end ;
       end ;

    // draw chart
       cnt_tmp := 0 ;
       for i:=2 to cnt do begin
         _canvas.CanvasBrush.Color := ar_chart_color[i-2] ;
         if ar_chart_color[i-2].ARGB = TGIS_Color.None.ARGB then
           _canvas.CanvasBrush.Style := TGIS_BrushStyle.Clear
         else
           _canvas.CanvasBrush.Style := TGIS_BrushStyle.Solid ;

         if _values[i] >= GIS_HALF_MAX_DOUBLE then
           continue ;
         case _style of
           TGIS_ChartStyle.Pie : draw_pie( i ) ;
           TGIS_ChartStyle.Bar : draw_bar( i, cnt_tmp ) ;
         end ;

         inc( cnt_tmp ) ;
       end ;
  end ;


  procedure GisDrawChart(
    const _canvas  : TGIS_RendererAbstract ;
    const _center  : TPoint                ;
    const _size    : Integer               ;
    const _style   : TGIS_ChartStyle       ;
    const _values  : TGIS_DoubleArray      ;
    const _colors  : TGIS_ColorArray
  ) ;
  var
    i          : Integer ;
    j          : Integer ;
    cnt        : Integer ;
    cnt_tmp    : Integer ;
    tmp        : Double  ;
    total      : Double  ;
    maximum    : Double  ;
    minimum    : Double  ;
    minmax     : Double  ;
    angle_last : Double  ;
    pt_start   : TPoint {$IFDEF JAVA} = new TPoint(0,0) {$ENDIF} ;
    pt_last    : TPoint {$IFDEF JAVA} = new TPoint(0,0) {$ENDIF} ;
    bar_gap,
    bar_width  : Integer ;

    ar_chart_color : array of TGIS_Color ;

    procedure draw_pie( const _idx : Integer ) ;
    var
      pt    : TPoint {$IFDEF JAVA} = new TPoint(0,0) {$ENDIF} ;
      angle : Double ;
      sa,ca : Double ;
    begin
      if ( total <> 0 ) and ( _values[_idx] > 0 ) then begin
        angle := angle_last + _values[_idx] / total * 2 * Pi ;
      end
      else
        exit ;

      if _idx <> cnt then begin
        SinCos( angle, sa, ca ) ;
        pt.X := _center.X + RoundS( _size * {$IFDEF JAVA}Integer(ca){$ELSE} ca {$ENDIF} ) ;
        pt.Y := _center.Y + RoundS( _size * {$IFDEF JAVA}Integer(sa){$ELSE} sa {$ENDIF} ) ;
      end
      else
        pt := pt_start ;

      if ( pt.X = pt_last.X ) and
         ( pt.Y = pt_last.Y ) and
         ( (_values[_idx] / total) < 0.5 ) then
      begin
        // ignore small amounts
        exit ;
      end ;
      _canvas.CanvasDrawPie(
        angle_last + Pi/2, angle + Pi/2,
        _size,
        _center.X, _center.Y
      ) ;

      angle_last := angle ;
      pt_last    := pt{$IFDEF JAVA}.MakeCopy{$ENDIF}    ;
    end ;

    procedure draw_bar( const _idx : Integer; const _pos : Integer ) ;
    var
      height   : Integer ;
      left     : Integer ;
      right    : Integer ;
      bottom   : Integer ;
      top      : Integer ;
    begin
      height := RoundS( _values[i] / minmax * _size ) ;

      left   := pt_start.X + (_pos)* ( bar_width + bar_gap ) ;
      bottom := pt_start.Y ;
      right  := left + bar_width ;
      top    := bottom - height ;

      _canvas.CanvasDrawRectangle( Rect(left, top, right, bottom) ) ;
    end ;

  begin
    if high( _values ) < 2 then exit ;

    SetLength( ar_chart_color, 9 ) ;

    for j := 0 to high( _colors ) do
      ar_chart_color[j] := _colors[j] ;

    if high( _values ) - 2  > high( ar_chart_color ) then
      cnt := high( ar_chart_color ) + 2
    else
      cnt := high( _values ) ;

    // precomputed values
       total   := 0        ;
       minimum := _values[0] ;
       maximum := _values[1] ;

    // set styles
      _canvas.CanvasPen.Color := TGIS_Color.Black ;
      _canvas.CanvasPen.Width := 1 ;

    // precomputed starting values
       case _style of

         TGIS_ChartStyle.Pie :
           begin
             for i := 2 to cnt do begin
               if _values[i] >= GIS_HALF_MAX_DOUBLE then
                 continue
               else if _values[i] > 0 then
                 total := total + _values[i] ;
             end ;

             angle_last := -Pi/2 ;
             pt_start.X := _center.X + RoundS( _size * Cos( angle_last ) ) ;
             pt_start.Y := _center.Y + RoundS( _size * Sin( angle_last ) ) ;
             pt_last  := pt_start ;

             _canvas.CanvasPen.Style := TGIS_PenStyle.Solid ;
           end ;

         TGIS_ChartStyle.Bar :
           begin

             cnt_tmp := 0 ;
             for i := 2 to cnt do begin
               tmp := _values[i] ;
               if Abs( tmp ) >= GIS_HALF_MAX_DOUBLE then
                 continue
               else if tmp > 0 then begin
                 if tmp > maximum then maximum := tmp ;
               end
               else begin
                 if tmp < minimum then minimum := tmp ;
               end ;
               inc( cnt_tmp )  ;
             end ;
             minmax := Abs(minimum)+maximum ;
             if minmax = 0 then exit ;
             if cnt_tmp = 0 then exit ;

             bar_gap   :=  RoundS( ( 1.0 * _size / cnt_tmp ) * 1.0/5 ) ;
             bar_width :=  RoundS( ( 1.0 * _size / cnt_tmp ) - bar_gap ) ;

             pt_start.X := _center.X - _size div 2 ;
             pt_start.Y := _center.Y + _size div 2
                           + RoundS( minimum / minmax * _size ) ;

             _canvas.CanvasPen.Style := TGIS_PenStyle.Solid ;
             _canvas.CanvasDrawLine(
               _center.X - _size div 2 - bar_gap, pt_start.Y,
               _center.X + _size div 2 + bar_gap, pt_start.Y
             ) ;

             if bar_width > 3 then _canvas.CanvasPen.Style := TGIS_PenStyle.Solid
                              else _canvas.CanvasPen.Style := TGIS_PenStyle.Clear ;

           end ;
         else
           begin
             assert( False ) ;
           end ;
       end ;

    // draw chart
       cnt_tmp := 0 ;
       for i:=2 to cnt do begin
         _canvas.CanvasBrush.Color := ar_chart_color[i-2] ;
         if ar_chart_color[i-2].ARGB = TGIS_Color.None.ARGB then
           _canvas.CanvasBrush.Style := TGIS_BrushStyle.Clear
         else
           _canvas.CanvasBrush.Style := TGIS_BrushStyle.Solid ;

         if _values[i] >= GIS_HALF_MAX_DOUBLE then
           continue ;
         case _style of
           TGIS_ChartStyle.Pie : draw_pie( i ) ;
           TGIS_ChartStyle.Bar : draw_bar( i, cnt_tmp ) ;
         end ;

         inc( cnt_tmp ) ;
       end ;
  end ;

{==================================== END =====================================}
end.
