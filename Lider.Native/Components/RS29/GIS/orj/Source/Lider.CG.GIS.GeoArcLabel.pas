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
  Encapsulation of rotated labels for Arc.

  This unit will perform all actions necessary to draw labels which
  "follow the road". That means labels which rotate to match the flow of an
  Arc shape. Such labeling is perfect for labeling rivers etc.

  This feature was placed in a separate unit because is a bit complicated and
  too large to include in GisLayerVector (to which this unit is actually
  related).
}

{$IFDEF DCC}
  unit GisArcLabel ;
  {$HPPEMIT '#pragma link "GisArcLabel"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisTypes ;
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

  //----------------------------------------------------------------------------
  //  public functions
  //----------------------------------------------------------------------------

    /// <summary>
    ///   This function will perform all actions necessary to draw labels that
    ///   "follow the road".
    /// </summary>
    /// <param name="_original">
    ///   original shape - truncated shape has no fields etc.
    /// </param>
    /// <param name="_truncated">
    ///   shape on which all action must be performed; it MUST be a
    ///   TGIS_ShapeArc
    /// </param>
    /// <param name="_part">
    ///   the part of the shape on which the action is to be performed
    /// </param>
    /// <param name="_label">
    ///   label to be displayed
    /// </param>
    /// <param name="_tiled">
    ///   tiled rendering
    /// </param>
    /// <param name="_savePoints">
    ///   if True, points of the label will be saved
    /// </param>
    /// <param name="_points">
    ///   list of points delimiting label area
    /// </param>
    /// <returns>
    ///   True if label was drawn
    /// </returns>
    function GisDrawArcLabel( const _original   : TObject ;
                              const _truncated  : TObject ;
                              const _part       : Integer ;
                              const _label      : String  ;
                              const _tiled      : Boolean ;
                              const _savePoints : Boolean ;
                              var   _points     : TGIS_DrawBuf
                            ) : Boolean ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Types,
    System.Math,
    GisRtl,
    GisInterfaces,
    GisTypesUI,
    GisHtmlLabel,
    GisInternals,
    GisLayerVector,
    GisParams,
    GisRendererAbstract ;
{$ENDIF}

type
  { Rotated label for arc shapes. Will interface with Viewer.DrawBuf.
  }
  T_arcLabel = class
    private // property internal values

        /// <summary>
        ///   Reference to Shape.
        /// </summary>
        FShape : TGIS_ShapeArc ;

        /// <summary>
        ///   Reference to Viewer
        /// </summary>
        {$IFDEF DCC}
          [weak]
        {$ENDIF}
        FViewer : IGIS_Viewer ;

        /// <summary>
        ///   Reference to renderer.
        /// </summary>
        FRenderer : TGIS_RendererAbstract ;

        /// <summary>
        ///   Reference to Layer.
        /// </summary>
        FLayer : TGIS_LayerVector ;

        FTiled : Boolean ;

        FSavePoints : Boolean ;
        FPoints     : TGIS_DrawBuf ;

    private // another variable

        /// <summary>
        ///   Number of the part on which we operate.
        /// </summary>
        partNum : Integer ;

        /// <summary>
        ///   Number of points in DrawBuf
        /// </summary>
        drawBufSize : Integer ;
        DrawBuf : array of TPoint ;

        /// <summary>
        ///   Text of label
        /// </summary>
        labelTxt : String  ;

        /// <summary>
        ///   Precalculated half of line width + 1 pixel offset.
        /// </summary>
        lineGap : Integer ;

        /// <summary>
        ///   Shape parameters.
        /// </summary>
        shapeParams : TGIS_ParamsSectionVector ;
    private // other private functions

      /// <summary>
      ///   Calculates point-to-point distance.
      /// </summary>
      /// <param name="_pt1">
      ///   first point
      /// </param>
      /// <param name="_pt2">
      ///   second point
      /// </param>
      /// <returns>
      ///   distance between points
      /// </returns>
      function  point2Point  ( const _pt1, _pt2 : TPoint
                             ) : Integer ;

      /// <summary>
      ///   <para>
      ///     Calculates a point that resides on line in a given position.
      ///   </para>
      ///   <para>
      ///     If point will be outside scope, then result can be undefined.
      ///   </para>
      /// </summary>
      /// <param name="_lineA">
      ///   begin of line
      /// </param>
      /// <param name="_lineB">
      ///   end of line
      /// </param>
      /// <param name="_pos">
      ///   position related to _lineA
      /// </param>
      /// <returns>
      ///   calculated point
      /// </returns>
      function  point4Line   ( const _lineA, _lineB : TPoint ;
                               const _pos : Integer
                             ) : TPoint ;
      /// <summary>
      ///   Calculates line to point distance.
      /// </summary>
      /// <param name="_lineA">
      ///   begin of line
      /// </param>
      /// <param name="_lineB">
      ///   end of line <br />
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   distance between a point and a line, can be negative if <br />point
      ///   is right up to a line, $FFFFFFF if point is not <br />related to
      ///   line
      /// </returns>
      function  line2Point   ( const _lineA, _lineB, _ptg : TPoint
                             ) : Integer ;

      /// <summary>
      ///   Calculates length of DrawBuf.
      /// </summary>
      /// <returns>
      ///   calculated length
      /// </returns>
      function  getLength    : Integer ;
      /// <summary>
      ///   Returns a point that is located at position.
      /// </summary>
      /// <param name="_pos">
      ///   position of point (distance from polyline start)
      /// </param>
      /// <returns>
      ///   prev point in draw after _pos; <br />-1 if next point is outside a
      ///   polyline
      /// </returns>
      function  getStartPoint( const _pos : Integer
                             ) : Integer ;

      /// <summary>
      ///   <para>
      ///     Find vertex to a current subsegment of a polyline which will
      ///     have at least a specified width.
      ///   </para>
      ///   <para>
      ///     Knowing an achieved tolerance vertex can <br />be moved to not
      ///     cross with polyline. In one call only "above" <br />vertices
      ///     will be found; to <br />found vertices switch a points order in
      ///     DrawBuf.
      ///   </para>
      /// </summary>
      /// <param name="_idx">
      ///   index of a point that is just after a _start in a DrawBuf
      /// </param>
      /// <param name="_length">
      ///   expected length
      /// </param>
      /// <param name="_limit">
      ///   not more than a limited length of a polyline can be traversed
      /// </param>
      /// <param name="_ptA">
      ///   first point of found vertex
      /// </param>
      /// <param name="_ptB">
      ///   second point of found vertex
      /// </param>
      /// <returns>
      ///   tolerance - maximum distance between the vertex and <br />a
      ///   polyline ; -1 if vertex was not found
      /// </returns>
      function  getVertices  ( const _idx     : Integer ;
                               const _length  : Integer ;
                               const _limit   : Integer ;
                               var   _ptA     : TPoint  ;
                               var   _ptB     : TPoint
                             ) : Integer ;
      /// <summary>
      ///   Move a line by a given offset.
      /// </summary>
      /// <param name="_lineA">
      ///   beginning of a line and moved value upon return
      /// </param>
      /// <param name="_lineB">
      ///   ending of a line and moved value upon return
      /// </param>
      /// <param name="_offset">
      ///   offset by which line must be moved
      /// </param>
      procedure moveVertices ( var   _lineA   : TPoint  ;
                               var   _lineB   : TPoint  ;
                               const _offset  : Integer
                             ) ;
      /// <summary>
      ///   <para>
      ///     Prepare a buffer.
      ///   </para>
      ///   <para>
      ///     Vertices shorter then 5 pixels will be joined to improve
      ///     performance.
      ///   </para>
      /// </summary>
      procedure prepareDrawBuf ;

    public // constructors/destructors


      /// <summary>
      ///   Construct an object and set all precomputed values
      /// </summary>
      constructor Create( const _original   : TGIS_ShapeArc ;
                          const _truncated  : TGIS_ShapeArc ;
                          const _part       : Integer       ;
                          const _label      : String        ;
                          const _tiled      : Boolean       ;
                          const _savePoints : Boolean
                        ) ;
    public // API

      /// <summary>
      ///   <para>
      ///     Do all work necessary for drawing.
      ///   </para>
      /// </summary>
      /// <returns>
      ///   True,if label was drawn
      /// </returns>
      /// <remarks>
      ///   See TGIS_LayerVector.DrawLabel for details and example.
      /// </remarks>
      function DrawLabel : Boolean ;
    public

        /// <summary>
        ///   Reference to Shape.
        /// </summary>
        property Shape : TGIS_ShapeArc read FShape ;

        /// <summary>
        ///   Reference to Viewer.
        /// </summary>
        property Viewer : IGIS_Viewer read FViewer ;

        /// <summary>
        ///   Reference to Layer.
        /// </summary>
        property Layer : TGIS_LayerVector read FLayer ;

        /// <summary>
        ///   points delimiting the label.
        /// </summary>
        property Points : TGIS_DrawBuf read FPoints ;
  end ;

//==============================================================================
// Public routines
//==============================================================================

  function GisDrawArcLabel( const _original   : TObject ;
                            const _truncated  : TObject ;
                            const _part       : Integer ;
                            const _label      : String  ;
                            const _tiled      : Boolean ;
                            const _savePoints : Boolean ;
                            var   _points     : TGIS_DrawBuf
                          ) : Boolean ;
  var
    obj : T_arcLabel ;
    len : Integer ;
    len0 : Integer ;
    i : Integer ;
  begin
    Result := False ;

    assert( TGIS_Shape(_original) is TGIS_ShapeArc ) ;

    if IsStringEmpty( _label ) then exit ;

    if ( not _tiled )
       and
       ( not TGIS_ShapeArc(_original).Params.Labels.Duplicates )
       and
       TGIS_ShapeArc(_original).Layer.Viewer.Ref.LabelsReg.IsDuplicated( _label )
    then
      exit ;

    obj := T_arcLabel.Create( TGIS_ShapeArc( _original  ),
                              TGIS_ShapeArc( _truncated ),
                              _part, _label, _tiled, _savePoints
                            ) ;
    try
      Result := obj.DrawLabel ;
      if _savePoints then begin
        len := length( obj.Points ) ;
        if len > 0 then begin
          len0 := length( _points ) ;
          SetLength( _points, len0+len ) ;
          for i := 0 to len-1 do
            _points[len0+i] := obj.Points[i] ;
        end ;
      end ;
    finally
      FreeObject( obj ) ;
    end ;
  end ;

//==============================================================================
// T_arcLabel
//==============================================================================

  constructor T_arcLabel.Create( const _original, _truncated : TGIS_ShapeArc ;
                                 const _part : Integer; const _label : String ;
                                 const _tiled : Boolean ;
                                 const _savePoints : Boolean
                               ) ;
  var
    line_params : TGIS_ParamsLine ;
  begin
    inherited Create ;
    FShape    := TGIS_ShapeArc( _truncated ) ;
    FLayer    := Shape.Layer  ;
    FViewer   := FLayer.Viewer.Ref ;
    FRenderer := TGIS_RendererAbstract( FLayer.Renderer ) ;
    FTiled    := _tiled ;
    FSavePoints := _savePoints ;
    SetLength( FPoints, 0 ) ;

    partNum   := _part ;

    labelTxt  := _label ;

    shapeParams := _original.Params ;
    line_params := shapeParams.Line ;
    lineGap     := _original.Viewer.Ref.TwipsToPixels( line_params.Width +
                                             2 * line_params.OutlineWidth
                                           ) div 2 ;
  end ;

  function T_arcLabel.point2Point( const _pt1, _pt2 : TPoint ) : Integer ;
  var
    x1, y1, x2, y2 : Double ;
  begin
    // assign to double variables due to integer range error
    x1 := _pt1.X ;
    y1 := _pt1.Y ;
    x2 := _pt2.X ;
    y2 := _pt2.Y ;
    if ( x1 = x2 ) and ( y1 = y2 ) then
      Result := 0
    else
      Result := RoundS( Sqrt( Sqr( x1 - x2 ) + Sqr( y1 - y2 ) ) ) ;
  end ;

  function T_arcLabel.point4Line( const _lineA, _lineB : TPoint ;
                                  const _pos : Integer
                                ) : TPoint ;
  var
    sin_val : Double ;
    cos_val : Double ;
    sqr_val : Double ;
    pt_tmp  : TPoint ;
    xa, ya, xb, yb : Double ;
  begin
    if ( _lineA.X = _lineB.X ) and ( _lineA.Y = _lineB.Y ) then begin
      Result := _lineA ;
      exit ;
    end ;

    // assign to double variables due to integer range error
    xa := _lineA.X ;
    ya := _lineA.Y ;
    xb := _lineB.X ;
    yb := _lineB.Y ;
    sqr_val := Sqrt( Sqr( xa - xb ) + Sqr( ya - yb ) ) ;
    sin_val := Abs( xa - xb ) / sqr_val ;
    cos_val := Abs( ya - yb ) / sqr_val ;
    {$IFDEF GIS_NORECORDS}
      pt_tmp := new TPoint(0,0) ;
    {$ENDIF}
    pt_tmp.X := RoundS( sin_val * _pos ) ;
    pt_tmp.Y := RoundS( cos_val * _pos ) ;

    {$IFDEF GIS_NORECORDS}
      Result := new TPoint(0,0) ;
    {$ENDIF}
    if _lineB.X < _lineA.X then Result.X := _lineA.X - pt_tmp.X
                           else Result.X := _lineA.X + pt_tmp.X ;
    if _lineB.Y < _lineA.Y then Result.Y := _lineA.Y - pt_tmp.Y
                           else Result.Y := _lineA.Y + pt_tmp.Y ;
  end ;

  function T_arcLabel.line2Point( const _lineA, _lineB, _ptg : TPoint
                                 ) : Integer ;
  var
    s,
    r, m, m2  : Double ;
    xa, ya, xb, yb : Double ;
  begin
    // is line length = 0 ?
    if (( _lineA.X = _lineB.X ) and ( _lineA.Y = _lineB.Y ) ) then
       Result := RoundS( point2Point( _lineA, _ptg ) )
    else begin
      // assign to double variables due to integer range error
      xa := _lineA.X ;
      ya := _lineA.Y ;
      xb := _lineB.X ;
      yb := _lineB.Y ;
      m2 := Sqr( xb - xa ) + Sqr( yb - ya ) ;
      m := Sqrt( m2 ) ;
      r := ( (ya - _ptg.Y) * (ya - yb) - (xa - _ptg.X) * (xb - xa) ) / m2 ;
      s := ( (ya - _ptg.Y) * (xb - xa) - (xa - _ptg.X) * (yb - ya) ) / m2 ;

      if ( (r >= 0) and (r <= 1) ) then begin// is point left-bottom or right-up
        Result := -( RoundS( s*m ) ) ;
      end
      else
        Result := $ffffff ;
    end ;

  end ;

  function T_arcLabel.getLength : Integer ;
  var
    i : Integer ;
  begin
    Result := 0 ;
    for i:=0 to drawBufSize - 2 do
      Result := Result + point2Point( DrawBuf[i  ],
                                      DrawBuf[i+1]
                                    ) ;
  end ;

  function T_arcLabel.getStartPoint( const _pos : Integer ) : Integer ;
  var
    i     : Integer ;
    dist  : Integer ;
    tmp   : Integer ;
    pt_a  : TPoint  ;
    pt_b  : TPoint  ;
  begin
    Result := -1 ;

    dist := 0 ;
    for i:=0 to drawBufSize - 2 do begin
      pt_a := DrawBuf[i  ] ;
      pt_b := DrawBuf[i+1] ;
      tmp := point2Point( pt_a, pt_b ) ;
      if dist + tmp > _pos then begin
        Result := i ;
        exit ;
      end ;
      dist := dist + tmp ;
    end ;
  end ;

  function T_arcLabel.getVertices( const _idx     : Integer ;
                                   const _length  : Integer ;
                                   const _limit   : Integer ;
                                   var   _ptA     : TPoint  ;
                                   var   _ptB     : TPoint
                                 ) : Integer ;
  var
    i        : Integer ;
    dist     : Integer ;
    tmp      : Integer ;
    pt_start : TPoint  ;
    pt_end   : TPoint  ;
    pt_tmp   : TPoint  ;
    last_pos : Integer ;
    len      : Integer ;
  begin
    Result := -1 ;

    // found segment from which the point-to-point distance will be at least
    // _length
    len  := 0 ;
    last_pos := -1 ;
    pt_start := _TPoint( DrawBuf[_idx] ) ;
    pt_tmp   := pt_start;
    for i:=_idx+1 to drawBufSize -1 do begin
      pt_end := DrawBuf[i] ;

      dist := point2Point( pt_start , pt_end ) ;
      if dist >= _length then begin
        last_pos := i ;
        Result := 0 ;
        break ;
      end ;

      // we check for the limit right now just to give a chance for
      // one segment of polyline to be traversed etc.
      len := len + point2Point( pt_tmp, pt_end ) ;
      if len > _limit then exit ;
      pt_tmp := pt_end ;
    end ;

    if Result < 0 then exit ; // noting found

    // found maximum distance between the found segment and points located on it
    dist := 0 ;
    for i:= _idx to last_pos do begin
      pt_tmp := DrawBuf[i] ;
      tmp := Abs( line2Point( pt_start, pt_end, pt_tmp ) ) ;
      if tmp > dist then
        dist := tmp ;
    end ;
    Result := dist ;

    _ptA := _TPoint( pt_start ) ;
    _ptB := _TPoint( pt_end ) ;
  end ;

  procedure T_arcLabel.moveVertices( var   _lineA    : TPoint ;
                                     var   _lineB    : TPoint ;
                                     const _offset : Integer
                                   ) ;
  var
    angle  : Double ;
    pt_tmp : TPoint ;
    sa, ca : Double ;
  begin
    angle := ArcTan2( _lineA.X - _lineB.X,
                      _lineA.Y - _lineB.Y
                    ) ;
    {$IFDEF GIS_NORECORDS}
      pt_tmp := new TPoint(0,0) ;
    {$ENDIF}
    SinCos( angle, sa, ca ) ;
    pt_tmp.X := RoundS(  ca * _offset ) ;
    pt_tmp.Y := RoundS( -sa * _offset ) ;

    _lineA.Y := _lineA.Y - pt_tmp.Y ;
    _lineB.Y := _lineB.Y - pt_tmp.Y ;
    _lineA.X := _lineA.X - pt_tmp.X ;
    _lineB.X := _lineB.X - pt_tmp.X ;
  end ;

  procedure T_arcLabel.prepareDrawBuf ;
  var
    point_no : Integer ;
    pt_start : TPoint  ;
    pt_tmp   : TPoint  ;
    offset   : TPoint  ;
  begin
    offset := Point( Shape.Params.Labels.OffsetX, Shape.Params.Labels.OffsetY ) ;
    // prepare draw buffer
    with Viewer do begin
      if Shape.GetPartSize( partNum ) > 0 then begin

        // copy to DrawBuf but only significant changes
        pt_start := MapToScreen( Shape.GetPoint(partNum,0) ) ;
        pt_start.X := pt_start.X + offset.X ;
        pt_start.Y := pt_start.Y + offset.Y ;
        pt_tmp := pt_start ;

        drawBufSize := 1 ;

        if Shape.GetPartSize( partNum ) > high( DrawBuf ) then
          SetLength( DrawBuf, Shape.GetPartSize( partNum ) ) ;
        DrawBuf[0] := _TPoint( pt_start ) ;

        for point_no := 1 to Shape.GetPartSize( partNum ) -1 do begin
          pt_tmp := MapToScreen( Shape.GetPoint(partNum,point_no) ) ;
          pt_tmp.X := pt_tmp.X + offset.X ;
          pt_tmp.Y := pt_tmp.Y + offset.Y ;
          if point2Point( pt_start, pt_tmp ) < 5 then continue ;
          pt_start := pt_tmp ;
          DrawBuf[drawBufSize] := _TPoint( pt_start ) ;
          inc( drawBufSize ) ;
        end ;
        if point2Point( pt_start, pt_tmp ) > 5 then // end of buffer
          DrawBuf[drawBufSize] := _TPoint( pt_tmp ) ;
      end ;
    end ;
  end ;

  function T_arcLabel.DrawLabel : Boolean ;
  var
    bdrawn        : Boolean ;
    tmp           : Integer ;
    to_right      : Boolean ;
    label_width   : Integer ;
    label_height  : Integer ;
    start_pos     : Integer ;
    stop_pos      : Integer ;
    idx           : Integer ;
    min_dist      : Integer ;
    min_A         : TPoint  ;
    min_B         : TPoint  ;
    max_A         : TPoint  ;
    max_B         : TPoint  ;
    pt_tmp        : TPoint  ;
    rct           : TRect   ;
    rct_tmp       : TRect   ;
    rct_big       : TRect   ;
    angle         : Double  ;

    pt_A          : TGIS_Point ;
    pt_B          : TGIS_Point ;
    pt_C          : TGIS_Point ;
    pt_D          : TGIS_Point ;

    label_params  : TGIS_ParamsLabel ;

    pt_rct        : TPoint ;
    pt_param      : TPoint ;

    lbl           : TGIS_HtmlLabel ;
    lw            : Integer ;

     function readHashCode : Integer ;
     begin
      if assigned( Layer.ParentLayer ) then
        Result := Layer.ParentLayer.GetHashCode
      else
        Result := Layer.GetHashCode ;
     end ;

    function get_angle : Double ;
    begin
      Result := - ArcTan2( min_A.Y - min_B.Y,
                           min_B.X - min_A.X
                         ) ;
    end ;

  begin
    Result := False ;

    if Shape.IsEmpty then exit ;
    if IsStringEmpty( labelTxt ) then exit ;

    prepareDrawBuf ;

    label_params := shapeParams.Labels ;

    FRenderer.CanvasFont.Name  := label_params.FontName    ;
    FRenderer.CanvasFont.Size  := FRenderer.Viewer.TwipsToPoints(
                                    label_params.FontSize
                                  ) ;
    FRenderer.CanvasFont.Style := label_params.FontStyle   ;
    FRenderer.CanvasFont.Color := label_params.FontColor   ;

    lbl := TGIS_HtmlLabel.Create(
             FRenderer,
             labelTxt,
             TGIS_LabelAlignment.Center,
             1000, 1000
           );
    bdrawn := False ;
    try
      // prepare rectangle for label
      rct := Rect( 0, 0, 1000, 1000 ) ; // reserve enough space

      //rct.BottomRight
      {$IFDEF GIS_NORECORDS}
        pt_param := new TPoint(0,0) ;
        pt_rct   := new TPoint(0,0) ;
      {$ENDIF}
      pt_param.X := rct.Right ;
      pt_param.Y := rct.Bottom ;
      pt_rct.X   := lbl.BoundingBox.Right ;
      pt_rct.Y   := lbl.BoundingBox.Bottom ;

      if ( ( pt_rct.X - rct.Left ) <= 0 ) or
         ( ( pt_rct.Y - rct.Top  ) <= 0 )
      then begin
        rct := Rect( 0, 0, pt_rct.X, pt_rct.Y ) ;
        exit ;
      end ;

      rct := Rect( 0, 0, pt_rct.X, pt_rct.Y ) ;

      label_width  := rct.Right  ;
      label_height := rct.Bottom ;

      // label must be fitted within 1/4 of center
      tmp := getLength ;
      start_pos := 2 * ( ( tmp - label_width ) div 2 ) div 4 ;
      if start_pos < 0 then
        start_pos := 0 ;

      stop_pos := tmp - start_pos ;
      if stop_pos  > tmp then
        stop_pos  := tmp ;

      // compute vertices
      idx := getStartPoint( start_pos ) ;
      if idx < 0 then exit ;

      lw := label_width ;
      while True do begin
        min_dist := getVertices( idx, lw, stop_pos,
                                 min_A, min_B
                               ) ;
        if GisTestLabelPosition(
             TGIS_LabelPosition(TGIS_LabelPosition.Flow),
             TGIS_LabelPositions(Shape.Params.Labels.Position)
           )
        then begin
          // try again with smaller label width
          if min_dist < 0 then begin
            dec( lw ) ;
            if lw > 1 then
              continue
            else
              exit ;
          end ;
        end
        else
          if min_dist < 0 then exit ;
        inc( idx ) ;

        // is it close enough?
        if min_dist > label_height div 3 then continue ;

        if GisTestLabelPosition(
             TGIS_LabelPosition.MiddleCenter,
             Shape.Params.Labels.Position
        )
        then
          moveVertices( min_A, min_B, -label_height div 2)
        else
          moveVertices( min_A, min_B, min_dist + lineGap ) ;

        // calculate direction; if True, then line is generally in normal
        // read direction
        angle :=  ArcTan2( min_A.X - min_B.X,
                           min_A.Y - min_B.Y
                         ) ;

        to_right := Sin( angle ) >= 0 ;

        // if it to the right, then set printing to above base the line
        if to_right then begin
          pt_tmp := _TPoint( min_A ) ;
          min_A  := _TPoint( min_B ) ;
          min_B  := _TPoint( pt_tmp ) ;
        end ;

        // center text
        tmp   := ( point2Point( min_A, min_B) - label_width ) div 2 ;
        min_A := point4Line( min_A, min_B, tmp ) ;
        min_B := point4Line( min_A, min_B, label_width ) ;

        // prepare data for allocator
        max_A := _TPoint( min_A ) ;
        max_B := _TPoint( min_B ) ;
        if to_right then moveVertices( max_A, max_B, - label_height )
                    else moveVertices( max_A, max_B, + label_height ) ;
        pt_A := Viewer.ScreenToMap( ConvertPoint( min_A ) ) ;
        pt_B := Viewer.ScreenToMap( ConvertPoint( min_B ) ) ;
        pt_C := Viewer.ScreenToMap( ConvertPoint( max_A ) ) ;
        pt_D := Viewer.ScreenToMap( ConvertPoint( max_B ) ) ;

        // test label allocator
        if label_params.Allocator then begin

           if FTiled then begin
             if not Viewer.LabelsReg.IsAny(
                     readHashCode,
                     FShape.Uid
                   )
             then begin
               if ( not FShape.Params.Labels.Duplicates ) and
                  Viewer.LabelsReg.IsDuplicated( labelTxt )
               then begin
                 Result := False ;
                 exit ;
               end ;
             end ;
           end ;

           if not FRenderer.Viewer.LabelsReg.AllocateEx(
                   pt_A, pt_B, pt_C, pt_D,
                   readHashCode,
                   FShape.Uid,
                   1.5/Viewer.Zoom
                 )
           then
             continue ;
        end ;

        if ( not FShape.Params.Labels.Duplicates ) then
          FViewer.LabelsReg.AddDuplicated( labelTxt )  ;

        // and output it
        bdrawn := True ;

        if not to_right then begin
          pt_rct.X := max_A.X ;
          pt_rct.Y := max_A.Y ;
        end
        else begin
          pt_rct.X := min_A.X ;
          pt_rct.Y := min_A.Y ;
        end ;
        rct := Rect( pt_rct.X,
                     pt_rct.Y,
                     pt_rct.X + label_width,
                     pt_rct.Y + label_height
                   ) ;

        angle := get_angle ;
        if angle <> 0 then
          FRenderer.CanvasSetTransformation( angle, rct.Left, rct.Top ) ;
        try
          if angle <> 0 then
            rct_tmp := Rect( 0, 0, rct.Right-rct.Left, rct.Bottom-rct.Top )
          else
            rct_tmp := rct ;

          {$IFDEF CLR}
            rct_big := new TRect(
                         rct_tmp.Left   - 2,
                         rct_tmp.Top    - 2,
                         rct_tmp.Width  + 6,
                         rct_tmp.Height + 4
                       ) ;
          {$ELSE}
            rct_big.Left   := rct_tmp.Left   - 2 ;
            rct_big.Right  := rct_tmp.Right  + 4 ;
            rct_big.Top    := rct_tmp.Top    - 2 ;
            rct_big.Bottom := rct_tmp.Bottom + 2 ;
          {$ENDIF}

          //draw label shield with outline
          FRenderer.CanvasBrush.Style := label_params.Pattern ;
          FRenderer.CanvasBrush.Color := label_params.Color ;

          FRenderer.CanvasPen.Style := label_params.OutlineStyle ;
          FRenderer.CanvasPen.Width := Viewer.TwipsToPixels(
                                         label_params.OutlineWidth
                                       ) ;
          FRenderer.CanvasPen.Color := label_params.OutlineColor ;

          FRenderer.CanvasDrawRectangle( rct_big ) ;

          if FSavePoints then begin
            SetLength( FPoints, 5 ) ;
            if to_right then begin
              FPoints[0] := min_A ;
              FPoints[1] := min_B ;
              FPoints[2] := max_B ;
              FPoints[3] := max_A ;
              FPoints[4] := min_A ;
            end else begin
              FPoints[0] := max_A ;
              FPoints[1] := max_B ;
              FPoints[2] := min_B ;
              FPoints[3] := min_A ;
              FPoints[4] := max_A ;
            end ;
          end ;

          //draw label text outline
          if ( label_params.Pattern        =  TGIS_BrushStyle.Clear   )
             and
             TGIS_Bitmap.IsNilOrEmpty( label_params.Bitmap )
             and
             ( label_params.FontColor.ARGB <> label_params.Color.ARGB )
          then begin
            //draw label text with shadow
            lbl.Draw( rct_tmp,
                      0,
                      Point( 0, 0 ),
                      Max( 1, FRenderer.PPI div 96 ),
                      label_params.Color
                    ) ;
          end else begin
            //draw label text
            lbl.Draw( rct_tmp,
                      0,
                      Point( 0, 0 ),
                      0,
                      TGIS_Color.Black
                    ) ;
          end ;

          break ;
        finally
          if angle <> 0 then
            FRenderer.CanvasClearTransformation ;
        end ;
      end ; // while

    finally
      {$IFNDEF OXYGENE}
        lbl.Free ;
      {$ENDIF}

      Result := bdrawn ;
    end ;
  end ;

{==================================== END =====================================}
end.
