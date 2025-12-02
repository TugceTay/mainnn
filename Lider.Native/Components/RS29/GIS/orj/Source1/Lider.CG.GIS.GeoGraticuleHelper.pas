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
  Classes to help drawing graticule line.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoGraticuleHelper;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoGraticuleHelper"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    TatukGIS.NDK;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Math,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoRendererAbstract,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoViewer;
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

type
  /// <summary>
  ///   Graticule mode
  /// </summary>
  TGIS_GraticuleMode = (
    /// <summary>
    ///   Standard graticule lines.
    /// </summary>
    LatLon
  ) ;

  /// <summary>
  ///   Graticule position against graticule line.
  /// </summary>
  TGIS_GraticuleLabelPosition = (
    /// <summary>
    ///   A label is located on a left of a graticule line (or above depends on
    ///   graticule line orientation).
    /// </summary>
    Left,

    /// <summary>
    ///   A label is located in on a the right of a graticule line (or below
    ///   depends on graticule line orientation).
    /// </summary>
    Right,

    /// <summary>
    ///   A label is located in the middle of a graticule line.
    /// </summary>
    Middle
  ) ;

  /// <summary>
  ///   Graticule line label orientation.
  /// </summary>
  TGIS_GraticuleLabelOrientation = (
    /// <summary>
    ///   Labels will always be vertical.
    /// </summary>
    Vertical,

    /// <summary>
    ///   Labels will always be horizontal.
    /// </summary>
    Horizontal,

    /// <summary>
    ///   Labels will follow graticule lines orientation.
    /// </summary>
    Follow
  ) ;


  /// <summary>
  ///   Object defining graticule labels presentation. A part of TGIS_Graticule.
  /// </summary>
  TGIS_GraticuleLabel = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_ObjectDisposable )
    private
      FEnabled     : Boolean ;
      FFont        : TGIS_Font ;
      FShadowColor : TGIS_Color ;
      FPosition    : TGIS_GraticuleLabelPosition ;
      FOrientation : TGIS_GraticuleLabelOrientation ;
    public
      /// <inheritdoc/>
      constructor Create ;

    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;

    public
      /// <summary>
      ///   True if labels are visible.
      /// </summary>
      property Enabled     : Boolean
                             read  FEnabled ;

      /// <summary>
      ///   Labels font.
      /// </summary>
      property Font        : TGIS_Font
                             read  FFont ;

      /// <summary>
      ///  Labels shadow color. If different than TGIS_Color.None then the
      ///  shadow will be visible.
      /// </summary>
      property ShadowColor   : TGIS_Color
                             read  FShadowColor ;

      /// <summary>
      ///   Labels position.
      /// </summary>
      property Position    : TGIS_GraticuleLabelPosition
                             read  FPosition
                             write FPosition ;

      /// <summary>
      ///   Labels orientation.
      /// </summary>
      property Orientation : TGIS_GraticuleLabelOrientation
                             read  FOrientation
                             write FOrientation ;
  end ;

  /// <summary>
  ///   Object defining graticule lines presentation. A part of TGIS_Graticule.
  /// </summary>
  TGIS_GraticuleStyle = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_ObjectDisposable )
    private
      FEnabled     : Boolean ;
      FPen         : TGIS_Pen ;
      FLabelA      : TGIS_GraticuleLabel ;
      FLabelM      : TGIS_GraticuleLabel ;
      FLabelB      : TGIS_GraticuleLabel ;
      FStep        : Double ;

    public
      /// <inheritdoc/>
      constructor Create ;

    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;

    public
      /// <summary>
      ///   True if labels are visible.
      /// </summary>
      property Enabled : Boolean
                         read  FEnabled ;


      /// <summary>
      ///   Pen style for graticule lines.
      /// </summary>
      property Pen     : TGIS_Pen
                         read  FPen ;

      /// <summary>
      ///   Label styles for left/top labels.
      /// </summary>
      property LabelA  : TGIS_GraticuleLabel
                         read  FLabelA ;

      /// <summary>
      ///   Label styles for middle labels.
      /// </summary>
      property LabelM  : TGIS_GraticuleLabel
                         read  FLabelM ;

      /// <summary>
      ///   Label styles for bottom/right labels.
      /// </summary>
      property LabelB  : TGIS_GraticuleLabel
                         read  FLabelB ;


      /// <summary>
      ///   Defines how how many graticules line are visible. Positive number
      ///   is a step expressed in actual number (radians for graticules line,
      ///   map units for other). Negative numbers defines how many graticules
      ///   line should be visible. Default is -4.
      /// </summary>
      property Step    : Double
                         read  FStep
                         write FStep ;
  end ;


  /// <summary>
  ///   Object which defines graticules behavior.
  /// </summary>
  TGIS_Graticule = {$IFDEF OXYGENE} public {$ENDIF}
                   class( TGIS_ObjectDisposable )
    private
      oVwr : TGIS_Viewer ;
    private
      FEnabled         : Boolean ;
      FMode            : TGIS_GraticuleMode ;
      FHorizontalStyle : TGIS_GraticuleStyle ;
      FVerticalStyle   : TGIS_GraticuleStyle ;

    private
      procedure fset_Enabled( const _value : Boolean ) ;

    public
      /// <summary>
      ///   Create an instance
      /// </summary>
      /// <param name="_viewer">
      ///   Viewer context
      /// </param>
      constructor Create ( const _viewer            : TGIS_Viewer
                         ) ; virtual ;

    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;


    public
      /// <summary>
      ///  If true than Graticules are enabled ;
      /// </summary>
      property Enabled          : Boolean
                                  read  FEnabled
                                  write fset_Enabled ;

      /// <summary>
      ///  Mode of operations/ Default is LatLon.
      /// </summary>
      property Mode             : TGIS_GraticuleMode
                                  read  FMode
                                  write FMode ;

      /// <summary>
      ///  Styles for horizontal lines.
      /// </summary>
      property HorizontalStyle   : TGIS_GraticuleStyle
                                   read FHorizontalStyle ;

      /// <summary>
      ///  Styles for vertical lines.
      /// </summary>
      property VerticalStyle     : TGIS_GraticuleStyle
                                   read FVerticalStyle ;
  end;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Helper for drawing graticule.
  /// </summary>
  TGIS_GraticuleHelper = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_Graticule )
    private
      csWGS : TGIS_CSCoordinateSystem ;

    private
      oRenderer   : TGIS_RendererAbstract ;
      iWidth      : Integer               ;
      iHeight     : Integer               ;
      rctOriginal : TRectF                ;
      rctNew      : TRectF                ;
      extActual   : TGIS_Extent           ;

    public
      /// <inheritdoc/>
      constructor Create ( const _viewer            : TGIS_Viewer
                         ) ; override ;

    protected
      /// <inherited />
      procedure doDestroy ; override;

    private
      procedure drawGraticules ;

    public

      /// <summary>
      ///   Draw graticules.
      /// </summary>
      /// <param name="_renderer">
      ///   renderer object to be used
      /// </param>
      /// <param name="_width">
      ///   window width
      /// </param>
      /// <param name="_height">
      ///   window height
      /// </param>
      /// <param name="_originalRect">
      ///   original size before any kind of zooming and dragging
      /// </param>
      /// <param name="_newRect">
      ///   size reflects actual zooming and dragging
      /// </param>
      /// <param name="_actualExtent">
      ///   extent reflects actual zooming and dragging
      /// </param>
      procedure Draw( const _renderer     : TGIS_RendererAbstract;
                      const _width        : Integer              ;
                      const _height       : Integer              ;
                      const _originalRect : TRectF               ;
                      const _newRect      : TRectF               ;
                      const _actualExtent : TGIS_Extent
                    ) ; overload ;

      /// <summary>
      ///   Draw graticules.
      /// </summary>
      /// <param name="_renderer">
      ///   renderer object to be used
      /// </param>
      /// <param name="_width">
      ///   window width
      /// </param>
      /// <param name="_height">
      ///   window height
      /// </param>
      /// <param name="_actualExtent">
      ///   extent reflects actual zooming and dragging
      /// </param>
      procedure Draw( const _renderer     : TGIS_RendererAbstract;
                      const _width        : Integer              ;
                      const _height       : Integer              ;
                      const _actualExtent : TGIS_Extent
                    ) ; overload ;
  end;

implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoHtmlLabel;
{$ENDIF}


const
  // LatLon scaling scheme
  LATLON_SCALES : array[0..33] of Double = (

     1.0 / 60 / 60 / 60,
     2.5 / 60 / 60 / 60,
     5.0 / 60 / 60 / 60 / 60,
    10.0 / 60 / 60 / 60 / 60,
    15.0 / 60 / 60 / 60 / 60,
    30.0 / 60 / 60 / 60 / 60,
     1.0 / 60 / 60 / 60,
     2.5 / 60 / 60 / 60,
     5.0 / 60 / 60 / 60,
    10.0 / 60 / 60 / 60,
    15.0 / 60 / 60 / 60,
    30.0 / 60 / 60 / 60,
     1.0 / 60 / 60,
     2.5 / 60 / 60,
     5.0 / 60 / 60,
    10.0 / 60 / 60,
    15.0 / 60 / 60,
    30.0 / 60 / 60,
     1.0 / 60,
     2.5 / 60,
     5.0 / 60,
    10.0 / 60,
    15.0 / 60,
    30.0 / 60,
     1.0,
     2.5,
     5.0,
    10.0,
    15.0,
    30.0,
    45.0,
    60.0,
    90.0,
   999.0
   ) ;

// alternative set
{
  2.5 / 60 / 60 / 60 / 60,
  5.0 / 60 / 60 / 60 / 60,
 15.0 / 60 / 60 / 60 / 60,
 45.0 / 60 / 60 / 60 / 60,
  2.5 / 60 / 60 / 60,
  5.0 / 60 / 60 / 60,
 15.0 / 60 / 60 / 60,
 45.0 / 60 / 60 / 60,
  2.5 / 60 / 60,
  5.0 / 60 / 60,
 15.0 / 60 / 60,
 45.0 / 60 / 60,
  2.5 / 60,
  5.0 / 60,
 15.0 / 60,
 45.0 / 60,
  2.5,
  5.0,
 15.0,
 45.0,
 90.0,
 999
  );}

const
  INSIDE    = 1 ; // line crossing borders state
  OUTSIDE   = 2 ; // line crossing borders state
  CROSS_IN  = 3 ; // line crossing borders state
  CROSS_OUT = 4 ; // line crossing borders state

function find_cross(
  const _ext  : TGIS_Extent ;
  const _ptgA : TGIS_Point  ;
  const _ptgB : TGIS_Point
) : TGIS_Point ;
var
  line : TGIS_Line ;
  a : Boolean ;
  b : Boolean ;
begin

  a := GisIsPointInsideExtent( _ptgA, _ext ) ;
  b := GisIsPointInsideExtent( _ptgB, _ext ) ;

  line := GisLine( _ptgA, _ptgB ) ;

  if GisGetLinesCrossing(
       GisLine( _ext.XMin, _ext.YMin,
                _ext.XMin, _ext.YMax
              ),
       line,
       Result
     )
  then
    exit
  else
  if GisGetLinesCrossing(
       GisLine( _ext.XMin, _ext.YMax,
                _ext.XMax, _ext.YMax
              ),
       line,
       Result
     )
  then
    exit
  else
  if GisGetLinesCrossing(
       GisLine( _ext.XMax, _ext.YMax,
                _ext.XMax, _ext.YMin
              ),
       line,
       Result
     )
  then
    exit
  else
  if GisGetLinesCrossing(
       GisLine( _ext.XMax, _ext.YMin,
                _ext.XMin, _ext.YMin
              ),
       line,
       Result
     )
  then
    exit
  else begin
    assert( False, 'Something wrong' + Integer(Integer(a)+Integer(b)).ToString ) ;
  end ;
end;


{$REGION 'TGIS_Graticule'}

constructor TGIS_Graticule.Create(
  const _viewer : TGIS_Viewer
) ;
begin
  oVwr := _viewer ;

  FMode := TGIS_GraticuleMode.LatLon ;

  FHorizontalStyle := TGIS_GraticuleStyle.Create ;
  FVerticalStyle   := TGIS_GraticuleStyle.Create ;

  FHorizontalStyle.FPen.Color := TGIS_Color.Black ;

  FVerticalStyle.FPen.Style := TGIS_PenStyle.Solid ;

  FEnabled := False ;
end;

procedure TGIS_Graticule.doDestroy ;
begin
  FreeObject( FHorizontalStyle ) ;
  FreeObject( FVerticalStyle   ) ;
  inherited ;
end ;

procedure TGIS_Graticule.fset_Enabled(
  const _value : Boolean
) ;
begin
  if FEnabled = _value then
    exit ;

  FEnabled := _value ;

  oVwr.InvalidateTopmost ;
end;

{$ENDREGION 'TGIS_Graticule'}

{$REGION 'TGIS_GraticuleHelper'}

constructor TGIS_GraticuleHelper.Create(
  const _viewer : TGIS_Viewer
) ;
begin
  inherited Create( _viewer ) ;

  csWGS := TGIS_CSFactory.ByEPSG( 4326 ) ;
end;

procedure TGIS_GraticuleHelper.doDestroy ;
begin
  inherited ;
end ;

procedure TGIS_GraticuleHelper.drawGraticules;
var
  ext_mapa : TGIS_Extent ;
  ext_wgs : TGIS_Extent ;
  ext_tmp : TGIS_Extent ;

  scl_x1 : Double ;
  scl_x2 : Double ;
  scl_x3 : Double ;

  scl_y1 : Double ;
  scl_y2 : Double ;
  scl_y3 : Double ;

  vstep  : Double ;
  hstep  : Double ;

  rct    : TRectF ;

  function adjust_step( const _step : Double ) : Double ;
  var
    i : Integer ;
  begin
    Result := _step ;
    for i := 0 to high( LATLON_SCALES ) -1 do begin
      if Result <= LATLON_SCALES[i]/2 + LATLON_SCALES[i+1]/2 then begin
        Result := LATLON_SCALES[i] ;
        break ;
      end ;
    end ;
    if Result > LATLON_SCALES[high( LATLON_SCALES )-1] then
      Result := LATLON_SCALES[high( LATLON_SCALES )-1] ;
  end;

  procedure prepare_to_screen ;
  var
    rct : TRectF ;
  begin

    if ( rctOriginal.Width  <> iWidth  ) or
       ( rctOriginal.Height <> iHeight )
    then begin
      rct := rctNew ;
    end
    else begin
      rct := rctOriginal ;
    end;

    scl_x1  := rct.Width  / ( ext_mapa.XMax - ext_mapa.XMin ) ;
    scl_x2  := ext_mapa.XMin ;
    scl_x3  := ( iWidth - rct.Width   ) / 2 ;

    scl_y1  := rct.Height / ( ext_mapa.YMax - ext_mapa.YMin ) ;
    scl_y2  := ext_mapa.YMax ;
    scl_y3  := ( iHeight - rct.Height  ) / 2 ;
  end;

  function to_screen( const _ptg : TGIS_Point ) : TPoint ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TPoint.Create( 0, 0 ) ;
    {$ENDIF}
    Result.X := RoundS( scl_x1 *
                        ( _ptg.X - scl_x2 ) +
                        scl_x3
                      ) ;
    Result.Y := RoundS( scl_y1 *
                        ( scl_y2 - _ptg.Y ) +
                        scl_y3
                      )  ;
  end;

  procedure draw_label(
    _pt : TPoint;
    _angle : Double;
    _style : TGIS_GraticuleLabel;
    _text : String
  ) ;
  var
    lbl : TGIS_HtmlLabel ;

    w,
    h          : Integer ;
    x_off,
    y_off      : Integer ;
    left,
    top,
    right,
    bottom     : Integer ;
    angle      : Double  ;
    shadow_width
               : Integer ;
    rct        : TRect ;
    horizontal : Boolean ;
  begin
      oRenderer.CanvasFont.Assign( _style.FFont );

      lbl := TGIS_HtmlLabel.Create( oRenderer,
                                    _text,
                                    TGIS_LabelAlignment.Single,
                                    999999, 999999
                                  ) ;
      try
        if _style.FShadowColor <> TGIS_Color.None then begin
          // prepare size of outline in pixels
          // generally outline must be composed of 1 screen pixels (1/96 of inch)
          // for printer outline will must be a bit stronger then usual
          shadow_width := Max( 1, oRenderer.PPI div 96 ) ;
        end
        else
          shadow_width := 0 ;

        w := lbl.BoundingBox.Width ;
        h := lbl.BoundingBox.Height ;

        case _style.FOrientation of
          TGIS_GraticuleLabelOrientation.Horizontal :
            begin
              y_off := 0 ;
              case _style.FPosition of
                TGIS_GraticuleLabelPosition.Left   : x_off := w ;
                TGIS_GraticuleLabelPosition.Right  : x_off := 0 ;
                TGIS_GraticuleLabelPosition.Middle : x_off := w div 2 ;
              end;
            end;
          TGIS_GraticuleLabelOrientation.Vertical   ,
          TGIS_GraticuleLabelOrientation.Follow     :
            begin
              x_off := 0 ;
              case _style.FPosition of
                TGIS_GraticuleLabelPosition.Left   :
                  begin
                    y_off := 0 ;
                  end;
                TGIS_GraticuleLabelPosition.Right  :
                  begin
                    y_off := h ;
                  end;
                TGIS_GraticuleLabelPosition.Middle :
                  begin
                    y_off :=  h div 2
                  end ;
              end;
            end;
        end;


        angle := _angle ;
        if Abs( angle ) < 1E-7 then
          angle := 0 ;
        if Abs( Abs( angle ) - 2*Pi ) < 1E-7 then
          angle := 0 ;
        if Abs( Abs( angle ) - Pi ) < 1E-7 then
          angle := 0 ;


        if ( ( _pt.X = 0 ) or ( _pt.X = iWidth ) )
           and
           ( Abs( Abs( angle ) - Pi/2 ) < 0.02 ) // must be not more the 89deg
        then
          horizontal := True
        else
          horizontal := False ;

        if angle = 0 then begin
          // left-right drawing direction
          if _pt.X = 0 then
            left  := _pt.X
          else
            left  := _pt.X - w ;
          top   := _pt.Y ;
        end
        else
        if ( angle >= -Pi ) and ( angle < 0 ) then begin
          // bottom-up drawing direction

          if _angle >=- Pi/2 then begin
            left   := h div 5 ;
            top    := -h + y_off ;

            if horizontal then
              left := left + RoundS( Abs( top * Tan( angle ) ) )
            else
              left := left + RoundS( Abs( y_off / Tan( angle ) ) ) ;
          end
          else begin
            angle := angle + Pi ;

            left  := -w - h div 5 ;
            top   := -h + y_off ;

            if horizontal then
              left := left - RoundS( Abs( top * Tan( angle ) ) )
            else
              left := left - RoundS( Abs( y_off / Tan( angle ) ) ) ;
          end;
        end
        else begin
          // up-down drawing direction

          if angle <= Pi/2 then begin
           left   := h div 5 ;
           top    := -h + y_off ;

            if horizontal then
              left := left + RoundS( Abs( y_off * Tan( angle ) ) )
            else
              left := left + RoundS( Abs( top / Tan( angle ) ) ) ;
          end
          else begin
            angle := angle + Pi ;

            left  := -w - h div 5 ;
            top   := -h + y_off ;

            if horizontal then
              left := left - RoundS( Abs( y_off * Tan( angle ) ) )
            else
              left := left - RoundS( Abs( top / Tan( angle ) ) ) ;
          end;
        end;



         right  := left + w ;
         bottom := top  + h;



         rct := Rect( left, top, right, bottom ) ;

         lbl.Draw( rct, angle, _pt,
                   shadow_width, _style.FShadowColor ) ;
       finally
         FreeObject( lbl ) ;
       end ;
  end;

  procedure draw_vertical( const _step : Double ) ;
  var
    i : Integer ;
    iy : Integer ;
    x,y : Double ;
    ptg : TGIS_Point ;
    ptg_cross : TGIS_Point ;
    buf : TGIS_DrawBuf ;
    delta : Double ;
    angle : Double ;

    first : Boolean ;
    ptg_prev : TGIS_Point ;

    pointstate : Boolean ;
    crossstate : Integer ;

    idx_a : Integer ;
    idx_m : Integer ;
    idx_b : Integer ;
    idx_r : Integer ;
  begin

    if not FVerticalStyle.FEnabled then
      exit ;

    pointstate := false ;

    if oVwr.ScaleAsFloat = 1 then exit ;


    rct := RectF(0, 0, iWidth, iHeight )  ;

    x := RoundS( ext_wgs.XMin / _step ) * _step ;
    while x <= ext_wgs.XMax do begin
      SetLength( buf, 200 );

      i := 0 ;

      delta := ( Min( 90, ext_wgs.YMax )  - Max( -90, ext_wgs.YMin ) ) / 20 ;

      idx_a := -1 ;
      idx_m := -1 ;
      idx_b := -1 ;
      {$IFDEF GIS_NORECORDS}
        ptg := new TGIS_Point ;
      {$ENDIF}
      ptg.X := NaN ;
      first := True ;

      for iy := 0 to 20 do begin
        y := ext_wgs.YMin + iy * delta ;
        ptg_prev := ptg ;
        ptg := oVwr.RotatedPoint( oVwr.CS.FromCS( csWGS, GisPoint( x, y ) ) ) ;


        if GisIsPointInsideExtent( ptg, ext_mapa ) then begin
          if first then begin
            pointstate := True ;
            first := False ;
          end ;

          if pointstate then
            crossstate := INSIDE
          else
          crossstate := CROSS_IN ;

          pointstate := True ;
        end
        else begin
          if first then begin
            pointstate := False ;
            first := False ;
          end ;

          if pointstate then
            crossstate := CROSS_OUT
          else
            crossstate := OUTSIDE ;
          pointstate := False ;
        end ;

        case crossstate  of
          INSIDE   :  begin
                        // add point as usual
                      end ;
          OUTSIDE   : begin
                        // ignore current point
                        continue ;
                       end ;
          CROSS_IN  : begin
                        ptg_cross := find_cross( ext_mapa, ptg, ptg_prev ) ;

                        buf[i] := to_screen( ptg_cross ) ;

                        if ptg_cross.Y = ext_mapa.YMin then begin
                           idx_a := i ;
                        end ;

                        inc( i ) ;
                        // and add point as usual
                      end ;
          CROSS_OUT : begin
                        ptg_cross := find_cross( ext_mapa, ptg, ptg_prev ) ;
                        buf[i] := to_screen( ptg_cross ) ;
                        if ptg_cross.Y = ext_mapa.YMax then begin
                          idx_b := i  ;
                        end
                        else begin
                          ptg_cross := find_cross( ext_mapa, ptg, ptg_prev ) ;
                        end ;

                        inc( i ) ;
                        // and ignore current point
                        continue ;
                      end ;
          else         begin
                         assert( False, 'Something wrong' ) ;
                       end ;

        end;

//?      buf[i] := ToScreen( GIS.RotatedPoint( ptg ) ) ;

        buf[i] := to_screen( ptg ) ;


        if idx_a < 0 then begin
          if (y = ext_wgs.YMin ) or (ptg.Y = ext_mapa.YMin) then begin
            idx_a := i ;
          end ;
        end ;

        if idx_b < 0 then begin
          if ( Abs( y - ext_wgs.YMax ) < 1e-7 ) or (ptg.Y = ext_mapa.YMax) then begin
            idx_b := i ;
          end ;
        end ;

        inc( i ) ;

      end ;
      SetLength( buf, Max( 0, i ) ) ;


      if i > 0  then begin
        oRenderer.CanvasPen.Assign( FVerticalStyle.FPen ) ;
        oRenderer.CanvasDrawPolyLine( buf );


        if idx_a >= 0 then begin
          // bottom

          case FVerticalStyle.FLabelA.FOrientation of
            TGIS_GraticuleLabelOrientation.Vertical   : angle := -Pi/2 ;
            TGIS_GraticuleLabelOrientation.Horizontal : angle := 0 ;
            TGIS_GraticuleLabelOrientation.Follow     :
              begin
               idx_r := Min( idx_a+2, high( buf )  );
               if idx_r = high( buf ) then
                 idx_r := idx_r - 2 ;

                if idx_r >= 0 then begin
                  angle := ArcTan2( buf[idx_r].Y - buf[idx_a].Y, buf[idx_r].X - buf[idx_a].X ) ;
                  if angle = 0 then angle := Pi/2 ;
                end ;
              end;
          end;
          draw_label( buf[idx_a], angle, FVerticalStyle.FLabelA, GisLongitudeToStr(  x * Pi / 180 , True,2 ) ) ;
        end ;

        if idx_b >= 0 then begin
          // top

          case FVerticalStyle.FLabelB.FOrientation of
            TGIS_GraticuleLabelOrientation.Vertical   : angle := Pi/2 ;
            TGIS_GraticuleLabelOrientation.Horizontal : angle := 0 ;
            TGIS_GraticuleLabelOrientation.Follow     :
              begin
                idx_r := Max( idx_b-2, 0  );
                if idx_r >= 0 then begin
                  angle := ArcTan2( buf[idx_r].Y - buf[idx_b].Y, buf[idx_r].X - buf[idx_b].X ) ;
                  if angle = 0 then angle := Pi/2 ;
                end;
              end ;
          end;
          draw_label( buf[idx_b], angle, FVerticalStyle.FLabelB, GisLongitudeToStr(  x * Pi / 180 , True,2 ) ) ;
        end;

      end ;

      x := x + _step ;
    end;
  end;

  procedure draw_horizontal( const _step : Double ) ;
  var
    i : Integer ;
    ix : Integer ;
    x,y : Double ;
    ptg : TGIS_Point ;
    ptg_cross : TGIS_Point ;
    buf : TGIS_DrawBuf ;
    delta : Double ;
    angle : Double ;

    first : Boolean ;
    ptg_prev : TGIS_Point ;

    pointstate : Boolean ;
    crossstate : Integer ;

    idx_a : Integer ;
    idx_m : Integer ;
    idx_b : Integer ;
    idx_r : Integer ;
  begin

    if not FHorizontalStyle.FEnabled then
      exit ;

    pointstate := false ;

    if oVwr.ScaleAsFloat = 1 then exit ;

    rct := RectF(0, 0, iWidth, iHeight )  ;

    y := RoundS( ext_wgs.YMin / _step ) * _step ;
    while y <= ext_wgs.YMax do begin

      SetLength( buf, 200 );

      i := 0 ;

      delta := ( Min( 180, ext_wgs.XMax )  - Max( -180, ext_wgs.XMin ) ) / 20 ;

      idx_a := -1 ;
      idx_m := -1 ;
      idx_b := -1 ;
      {$IFDEF GIS_NORECORDS}
        ptg := new TGIS_Point ;
      {$ENDIF}
      ptg.X := NaN ;
      first := True ;

      for ix := 0 to 20 do begin
        x := ext_wgs.XMin + ix * delta ;
        ptg_prev := ptg ;
        ptg := oVwr.RotatedPoint( oVwr.CS.FromCS( csWGS, GisPoint( x, y ) ) ) ;


        if GisIsPointInsideExtent( ptg, ext_mapa ) then begin
          if first then begin
            pointstate := True ;
            first := False ;
          end ;

          if pointstate then
            crossstate := INSIDE
          else
          crossstate := CROSS_IN ;

          pointstate := True ;
        end
        else begin
          if first then begin
            pointstate := False ;
            first := False ;
          end ;

          if pointstate then
            crossstate := CROSS_OUT
          else
            crossstate := OUTSIDE ;
          pointstate := False ;
        end ;

        case crossstate  of
          INSIDE   :  begin
                        // add point as usual
                      end ;
          OUTSIDE   : begin
                        // ignore current point
                        continue ;
                       end ;
          CROSS_IN  : begin
                        ptg_cross := find_cross( ext_mapa, ptg, ptg_prev ) ;

                        buf[i] := to_screen( ptg_cross ) ;

                        if ptg_cross.X >= ext_mapa.XMin then begin
                           idx_a := i ;
                        end ;

                        inc( i ) ;
                        // and add point as usual
                      end ;
          CROSS_OUT : begin
                        ptg_cross := find_cross( ext_mapa, ptg, ptg_prev ) ;
                        buf[i] := to_screen( ptg_cross ) ;
                        if ptg_cross.X = ext_mapa.XMax then begin
                          idx_b := i  ;
                        end
                        else begin
                          ptg_cross := find_cross( ext_mapa, ptg, ptg_prev ) ;
                        end ;

                        inc( i ) ;
                        // and ignore current point
                        continue ;
                      end ;
          else         begin
                         assert( False, 'Something wrong' ) ;
                       end ;

        end;

//?      buf[i] := ToScreen( GIS.RotatedPoint( ptg ) ) ;
        buf[i] := to_screen( ptg ) ;


        if idx_a < 0 then begin
          if (x = ext_wgs.XMin ) or (ptg.X = ext_mapa.XMin) then begin
            idx_a := i ;
          end ;
        end ;

        if idx_b < 0 then begin
          if ( Abs( x - ext_wgs.XMax ) < 1e-7 ) or (ptg.X = ext_mapa.XMax) then begin
            idx_b := i ;
          end ;
        end ;

        inc( i ) ;

      end ;
      SetLength( buf, Max( 0, i ) ) ;


      if i > 0  then begin
        oRenderer.CanvasPen.Assign( FHorizontalStyle.FPen ) ;
        oRenderer.CanvasDrawPolyLine( buf );


        if idx_a >= 0 then begin
          // bottom

          case FHorizontalStyle.FLabelA.FOrientation of
            TGIS_GraticuleLabelOrientation.Vertical   : angle := -Pi/2 ;
            TGIS_GraticuleLabelOrientation.Horizontal : angle := 0 ;
            TGIS_GraticuleLabelOrientation.Follow     :
              begin
               idx_r := Min( idx_a+2, high( buf )  );
               if idx_r = high( buf ) then
                 idx_r := idx_r - 2 ;

                if idx_r >= 0 then begin
                  angle := ArcTan2( buf[idx_r].Y - buf[idx_a].Y, buf[idx_r].X - buf[idx_a].X ) ;
                end ;
              end;
          end;
          draw_label( buf[idx_a], angle, FHorizontalStyle.FLabelA, GisLatitudeToStr(  y * Pi / 180 , True,2 ) ) ;
        end ;

        if idx_b >= 0 then begin
          // top

          case FHorizontalStyle.FLabelB.FOrientation of
            TGIS_GraticuleLabelOrientation.Vertical   : angle := Pi/2 ;
            TGIS_GraticuleLabelOrientation.Horizontal : angle := 0 ;
            TGIS_GraticuleLabelOrientation.Follow     :
              begin
                idx_r := Max( idx_b-2, 0  );
                if idx_r >= 0 then begin
                  angle := ArcTan2( buf[idx_r].Y - buf[idx_b].Y, buf[idx_r].X - buf[idx_b].X ) ;
                end;
              end ;
          end;
          draw_label( buf[idx_b], angle, FHorizontalStyle.FLabelB, GisLatitudeToStr(  y * Pi / 180 , True,2 ) ) ;
        end;

      end ;

      y := y + _step ;
    end;
  end;

begin

  if GisIsNoWorld( extActual ) then
    exit ;

  if oVwr.IsEmpty then
    exit ;

  ext_mapa := extActual ;
  if GisIsNoWorld( ext_mapa ) then
    ext_mapa := oVwr.VisibleExtent ;

  if GisIsEmptyExtent( ext_mapa ) then exit ;

  ext_wgs := oVwr.CS.ExtentToCS( csWGS, oVwr.UnrotatedExtent(ext_mapa) ) ;

  ext_tmp := oVwr.CS.ValidityExtentWGS ;
  ext_tmp.XMin := RadToDeg( ext_tmp.XMin );
  ext_tmp.YMin := RadToDeg( ext_tmp.YMin );
  ext_tmp.XMax := RadToDeg( ext_tmp.XMax );
  ext_tmp.YMax := RadToDeg( ext_tmp.YMax );

  ext_wgs := GisCommonExtent( ext_wgs, ext_tmp ) ;


  if ext_wgs.XMin < -180 then ext_wgs.XMin := -180 ;
  if ext_wgs.XMax >  180 then ext_wgs.XMax :=  180 ;
  if ext_wgs.YMin <  -90 then ext_wgs.YMin :=  -90 ;
  if ext_wgs.YMax >   90 then ext_wgs.YMax :=   90 ;


  prepare_to_screen ;

  // calculate grid distance
  if FVerticalStyle.Step < 0 then begin
    vstep := ( ext_wgs.XMax - ext_wgs.XMin ) /
             Max( 1, Abs( RoundS( FVerticalStyle.Step ) ) ) ;
  end
  else
    vstep := RadToDeg( FVerticalStyle.Step ) ;

  if FHorizontalStyle.Step < 0 then begin
    hstep := ( ext_wgs.YMax - ext_wgs.YMin ) /
             Max( 1, Abs( RoundS( FHorizontalStyle.Step ) ) ) ;
  end
  else
    hstep := RadToDeg( FHorizontalStyle.Step ) ;

  if ( FVerticalStyle.Step < 0 )
     and
     ( FHorizontalStyle.Step < 0 )
     and
     ( vstep / hstep < 2 )
     and
     ( hstep / vstep < 2 )
  then begin
    vstep := adjust_step( Max( vstep, hstep ) );
    hstep := vstep ;
  end
  else begin
    if FVerticalStyle.Step < 0 then
      vstep := adjust_step( vstep ) ;
    if FHorizontalStyle.Step < 0 then
      hstep := adjust_step( hstep ) ;
  end ;

  draw_vertical( vstep ) ;
  draw_horizontal( hstep ) ;

end;

procedure TGIS_GraticuleHelper.Draw(
  const _renderer     : TGIS_RendererAbstract;
  const _width        : Integer              ;
  const _height       : Integer              ;
  const _originalRect : TRectF               ;
  const _newRect      : TRectF               ;
  const _actualExtent : TGIS_Extent
) ;
begin
  if oVwr.CS.EPSG = 0 then
    exit ;

  oRenderer   := _renderer      ;
  iWidth      := _width         ;
  iHeight     := _height        ;
  rctOriginal := _originalRect  ;
  rctNew      := _newRect       ;
  extActual   := _actualExtent  ;

  drawGraticules ;
end;

procedure TGIS_GraticuleHelper.Draw(
  const _renderer     : TGIS_RendererAbstract;
  const _width        : Integer              ;
  const _height       : Integer              ;
  const _actualExtent : TGIS_Extent
) ;
begin
  Draw( _renderer,
        _width, _height,
        RectF( 0, 0, _width, _height ),
        RectF( 0, 0, _width, _height ),
        _actualExtent
       ) ;
end;
{$ENDREGION 'TGIS_GraticuleHelper'}

{$REGION 'TGIS_GraticuleLabel'}

constructor TGIS_GraticuleLabel.Create;
begin
  FEnabled     := True ;
  FFont        := TGIS_Font.Create ;
  FFont.Size   := 8 ;
  FShadowColor := TGIS_Color.FromARGB( 254, 255, 255, 255 ) ;
  FPosition    := TGIS_GraticuleLabelPosition.Left ;
  FOrientation := TGIS_GraticuleLabelOrientation.Follow ;
end;

procedure TGIS_GraticuleLabel.doDestroy;
begin
  FreeObject( FFont ) ;

  inherited;
end;

{$ENDREGION 'TGIS_GraticuleLabel'}

{$REGION 'TGIS_GraticuleStyle'}

constructor TGIS_GraticuleStyle.Create;
begin
  FEnabled := True ;
  FPen := TGIS_Pen.Create ;
  FLabelA := TGIS_GraticuleLabel.Create ;
  FLabelB := TGIS_GraticuleLabel.Create ;
  FLabelM := TGIS_GraticuleLabel.Create ;
  FStep   := -4 ;
end;

procedure TGIS_GraticuleStyle.doDestroy;
begin
  FreeObject( FPen    ) ;
  FreeObject( FLabelA ) ;
  FreeObject( FLabelB ) ;
  FreeObject( FLabelM ) ;

  inherited;
end;

{$ENDREGION 'TGIS_GraticuleStyle'}

end.

