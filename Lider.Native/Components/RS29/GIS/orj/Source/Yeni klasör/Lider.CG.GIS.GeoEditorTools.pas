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
  Editor tools to build complex shapes and draw extra content on canvas during editing.
}
{$IFDEF DCC}
  unit GisEditorTools ;
  {$HPPEMIT '#pragma link "GisEditorTools"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.Generics.Collections,
    GisTypes,
    GisInterfaces,
    GisClasses,
    GisFunctions,
    GisRendererAbstract,
    GisLayer,
    GisTypesUI ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

{$REGION 'TGIS_EditorToolAbstract'}

type
  /// <summary>
  ///   Keeps locked parameters for the editor helper.
  /// </summary>
  TGIS_EditorToolLockedParams = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FDistance      : Double ;
      FAngle         : Double ;
      FRelativeAngle : Boolean ;
    public
      /// <summary>
      ///   Lock Distance parameter.
      /// </summary>
      /// <param name="_value">
      ///   lock value
      /// </param>
      procedure LockDistance      ( const _value : Double  ) ;
      /// <summary>
      ///   Lock Angle parameter.
      /// </summary>
      /// <param name="_value">
      ///   lock value
      /// </param>
      procedure LockAngle         ( const _value : Double  ) ;
      /// <summary>
      ///   Lock RelativeAngle parameter.
      /// </summary>
      /// <param name="_value">
      ///   lock value
      /// </param>
      procedure LockRelativeAngle ( const _value : Boolean ) ;
    public
      /// <summary>
      ///   Distance parameter.
      /// </summary>
      property Distance      : Double   read  FDistance
                                        write FDistance ;
      /// <summary>
      ///   Angle parameter.
      /// </summary>
      property Angle         : Double   read  FAngle
                                        write FAngle ;
      /// <summary>
      ///   Relative Angle parameter.
      /// </summary>
      property RelativeAngle : Boolean  read  FRelativeAngle
                                        write FRelativeAngle ;
  end ;

  /// <summary>
  ///   Base editor tool class.
  /// </summary>
  TGIS_EditorToolAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class
    strict protected
      /// <summary>
      ///   Renderer handle.
      /// </summary>
      FRenderer     : TGIS_RendererAbstract ;
      /// <summary>
      ///   Parent handle.
      /// </summary>
      FParent       : TObject ;
      /// <summary>
      ///   Layer handle.
      /// </summary>
      FLayer        : TGIS_Layer ;
      /// <summary>
      ///   Editor handle.
      /// </summary>
      {$IFDEF DCC} [weak] {$ENDIF}
      FEditor       : IGIS_Editor ;
    protected
      /// <summary>
      ///   Points buffer in the editor.
      /// </summary>
      FPoints       : TGIS_PointArray ;
      /// <summary>
      ///   Points count in the buffer.
      /// </summary>
      FPointsCount  : Integer ;
      /// <summary>
      ///   Current distance.
      /// </summary>
      FDistance     : Double ;
      /// <summary>
      ///   Current rounded distance.
      /// </summary>
      FDistanceI    : Integer ;
      /// <summary>
      ///   Current angle.
      /// </summary>
      FAngle        : Double ;
      /// <summary>
      ///   Tool name.
      /// </summary>
      FName         : String ;
    private
      FLockedParams : TGIS_EditorToolLockedParams ;
      FLastPoint    : TGIS_Point {$IFDEF GIS_NORECORDS} := TGIS_Point.create {$ENDIF} ;
    protected
      /// <summary>
      ///   Init tool.
      /// </summary>
      procedure init ; virtual ;
      /// <summary>
      ///   Restore tool state.
      /// </summary>
      /// <param name="_points">
      ///   points buffer
      /// </param>
      procedure restoreState( const _points : TGIS_PointArray
                            ) ; virtual ;
      /// <summary>
      ///   Apply locked parameters for calculating points.
      /// </summary>
      procedure applyLockedParams ; virtual ;
      /// <summary>
      ///   Draw text on canvas.
      /// </summary>
      /// <param name="_str">
      ///   text to draw
      /// </param>
      /// <param name="_rect">
      ///   rectangle to draw text inside
      /// </param>
      procedure drawCanvasText( const _str  : String ;
                                const _rect : TRect
                              ) ;
      /// <summary>
      ///   Prepare pen style for additional drawing.
      /// </summary>
      procedure prepareHelperPen ;
      /// <summary>
      ///   Prepare pen style for main drawing.
      /// </summary>
      procedure prepareMainPen ;
      /// <summary>
      ///   Convert twips to pixels.
      /// </summary>
      /// <param name="_size">
      ///   value to convert
      /// </param>
      /// <returns>
      ///   value in pixels
      /// </returns>
      function  twipsToPixels( const _size : Integer
                              ) : Integer ; inline ;
      /// <summary>
      ///   Convert map to screen units.
      /// </summary>
      /// <param name="_ptg">
      ///   point to convert
      /// </param>
      /// <returns>
      ///   point in screen coordinates
      /// </returns>
      function  mapToScreen  ( const _ptg : TGIS_Point
                              ) : TPoint ; inline ;
      /// <summary>
      ///   Convert map to screen units.
      /// </summary>
      /// <param name="_buffer">
      ///   points buffer to convert
      /// </param>
      /// <returns>
      ///   points in screen coordinates
      /// </returns>
      function  mapToScreenBuffer( const _buffer : TArray<TGIS_Point>
                                  ) : TGIS_DrawBuf ; inline ;
      /// <summary>
      ///   Convert screen to map units.
      /// </summary>
      /// <param name="_pt">
      ///   point to convert
      /// </param>
      /// <returns>
      ///   point in map coordinates
      /// </returns>
      function  screenToMap  ( const _pt : TPoint
                              ) : TGIS_Point ; inline ;
      /// <summary>
      ///   Convert screen to map units.
      /// </summary>
      /// <param name="_pt">
      ///   point to convert
      /// </param>
      /// <returns>
      ///   point in map coordinates
      /// </returns>
      function  screenToMap3D( const _pt : TPoint
                              ) : TGIS_Point3D ; inline ;
      /// <summary>
      ///   Convert RectF to Rect type.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to convert
      /// </param>
      /// <returns>
      ///   rectangle in Integer space
      /// </returns>
      function  rectFToRect  ( const _rect : TRectF
                              ) : TRect ; inline ;
      /// <summary>
      ///   True, if angle is locked.
      /// </summary>
      /// <returns>
      ///   True if angle is locked
      /// </returns>
      function  isAngleLocked : Boolean ; inline ;
      /// <summary>
      ///   True, if distance is locked.
      /// </summary>
      /// <returns>
      ///   True if distance is locked
      /// </returns>
      function  isDistanceLocked : Boolean ; inline ;
      /// <summary>
      ///   True, if ctrl key is pressed.
      /// </summary>
      /// <returns>
      ///   True if ctrl key is pressed
      /// </returns>
      function  isCtrlPressed : Boolean ; inline ;
      /// <summary>
      ///   True, if shift key is pressed.
      /// </summary>
      /// <returns>
      ///   True if shift key is pressed
      /// </returns>
      function  isShiftPressed : Boolean ; inline ;
      /// <summary>
      ///   Notify about action to execute.
      /// </summary>
      /// <param name="_prompt">
      ///   text message
      /// </param>
      procedure notifyAction( const _prompt : String
                            ) ;
      /// <summary>
      ///   Set flag to true when updating Editor.
      /// </summary>
      procedure beginUpdatingEditor ;
      /// <summary>
      ///   Set flag to false when updating Editor.
      /// </summary>
      procedure endUpdatingEditor ;
      /// <summary>
      ///   Reset tool points and setup on the current Editor position.
      /// </summary>
      procedure resetToolPoints ;
    public
      /// <summary>
      ///   Class constructor.
      /// </summary>
      constructor Create ; virtual ;
      {$IFDEF DCC}
      /// <summary>
      ///   Class destructor.
      /// </summary>
      destructor  Destroy ; override ;
      {$ENDIF}
      /// <summary>
      ///   Handle mouse down event.
      /// </summary>
      /// <param name="_pt">
      ///   point position
      /// </param>
      procedure DoMouseDown   ( const _pt : TPoint
                              ) ; virtual ; abstract ;
      /// <summary>
      ///   Handle mouse move event.
      /// </summary>
      /// <param name="_pt">
      ///   point position
      /// </param>
      procedure DoMouseMove   ( const _pt : TPoint
                              ) ; virtual ; abstract ;
      /// <summary>
      ///   Handle mouse up event.
      /// </summary>
      /// <param name="_pt">
      ///   point position
      /// </param>
      procedure DoMouseUp     ( const _pt : TPoint
                              ) ; virtual ; abstract ;
      /// <summary>
      ///   Handle canvas paint.
      /// </summary>
      procedure DoCanvasPaint ; virtual ; abstract ;
    public
      /// <summary>
      ///   Renderer handle.
      /// </summary>
      property Renderer     : TGIS_RendererAbstract       read  FRenderer
                                                          write FRenderer ;
      /// <summary>
      ///   Parent handle.
      /// </summary>
      property Parent       : TObject                     read  FParent
                                                          write FParent ;
      /// <summary>
      ///   Layer handle.
      /// </summary>
      property Layer        : TGIS_Layer                  read  FLayer
                                                          write FLayer ;
      /// <summary>
      ///   Editor handle.
      /// </summary>
      property Editor       : IGIS_Editor                 read  FEditor
                                                          write FEditor ;
      /// <summary>
      ///   Tool name.
      /// </summary>
      property Name         : String                      read  FName ;
      /// <summary>
      ///   Handle to locked parameters.
      /// </summary>
      property LockedParams : TGIS_EditorToolLockedParams read  FLockedParams ;
      /// <summary>
      ///   Last point created by tool.
      /// </summary>
      property LastPoint    : TGIS_Point                  read  FLastPoint
                                                          write FLastPoint ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorHelper'}
  /// <summary>
  ///   Helper for drawing extra content on canvas during editing.
  /// </summary>
  TGIS_EditorHelper = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FRenderer   : TGIS_RendererAbstract ;
      {$IFDEF DCC} [weak] {$ENDIF}
      FViewer     : IGIS_Viewer ;
      {$IFDEF DCC} [weak] {$ENDIF}
      FEditor     : IGIS_Editor ;
      FLayer      : TGIS_Layer ;
    private
      FDrawingTool      : TGIS_EditorToolAbstract ;
      FSnapTracing      : Boolean ;
      FSnapPointType    : TGIS_EditorSnapResultType ;
      FSnapTracePoint   : TGIS_Point ;
      FSnapTracePointE  : TGIS_Point ;
      FSnapTracePointS  : TGIS_Point ;
      FUseSnapTracing   : Boolean ;
      FUseSnapTracingE  : Boolean ;
      FUseSnapTracingS  : Boolean ;
      FHasSnapPoint     : Boolean ;
      FPolarSnapTracing : Boolean ;
      FPolarIncAngle    : Double ;
      FCtrlPressed      : Boolean ;
      FShiftPressed     : Boolean ;
      FUpdatingEditor   : Boolean ;
      FOnSnapPoint      : TGIS_EditorSnapPointEvent ;
    private
      FNotifyChanges    : TNotifyEvent ;
      FNotifyAction     : TGetStrProc ;
    private
      procedure doTracePaint ;
      function  findPolarPoint( const _refPt : TGIS_Point ;
                                const _pt    : TGIS_Point ;
                                  var _ptg   : TGIS_Point
                              ) : Boolean ;
      procedure doPolarPaint  ( const _pt    : TGIS_Point ;
                                const _pte   : TGIS_Point ;
                                const _color : TGIS_Color
                              ) ;
      function  doSnapPoint     ( const _ptg   : TGIS_Point3D ) : TGIS_Point3D ;
      function  doFindSnapPoint (   var _ptg   : TGIS_Point3D ) : Boolean ;
    protected
      procedure fset_DrawingTool( const _tool     : TGIS_EditorToolAbstract ) ;
      procedure fset_Renderer   ( const _renderer : TGIS_RendererAbstract   ) ;
      procedure fset_Layer      ( const _layer    : TGIS_Layer              ) ;
    public
      /// <summary>
      ///   Class constructor.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer handle
      /// </param>
      constructor Create( const _viewer   : IGIS_Viewer ) ;
      {$IFDEF DCC}
      /// <summary>
      ///   Class destructor.
      /// </summary>
      destructor  Destroy ; override ;
      {$ENDIF}
      /// <summary>
      ///   Handle mouse down event.
      /// </summary>
      /// <param name="_pt">
      ///   point position
      /// </param>
      procedure DoMouseDown( const _pt : TPoint ) ;
      /// <summary>
      ///   Handle mouse move event.
      /// </summary>
      /// <param name="_pt">
      ///   point position
      /// </param>
      procedure DoMouseMove( const _pt : TPoint ) ;
      /// <summary>
      ///   Handle mouse up event.
      /// </summary>
      /// <param name="_pt">
      ///   point position
      /// </param>
      procedure DoMouseUp  ( const _pt : TPoint ) ;

      /// <summary>
      ///   Paint on canvas.
      /// </summary>
      procedure DoCanvasPaint ;
      /// <summary>
      ///   Start editing operation.
      /// </summary>
      procedure DoStartEdit ;
      /// <summary>
      ///  End editing operation.
      /// </summary>
      procedure DoEndEdit ;
      /// <summary>
      ///   Undo last action.
      /// </summary>
      procedure DoUndo ;
      /// <summary>
      ///   Redo last action.
      /// </summary>
      procedure DoRedo ;
    public
      /// <summary>
      ///   Drawing tool handle.
      /// </summary>
      property DrawingTool      : TGIS_EditorToolAbstract   read  FDrawingTool
                                                            write fset_DrawingTool ;
      /// <summary>
      ///   Renderer handle.
      /// </summary>
      property Renderer         : TGIS_RendererAbstract     read  FRenderer
                                                            write fset_Renderer ;
      /// <summary>
      ///   Layer handle.
      /// </summary>
      property Layer            : TGIS_Layer                read  FLayer
                                                            write fset_Layer ;
      /// <summary>
      ///   Viewer handle.
      /// </summary>
      property Viewer           : IGIS_Viewer               read  FViewer ;
      /// <summary>
      ///   If True, snap tracing is enabled.
      /// </summary>
      property SnapTracing      : Boolean                   read  FSnapTracing
                                                            write FSnapTracing ;
      /// <summary>
      ///   If True, polar snap tracing is enabled.
      /// </summary>
      property PolarSnapTracing : Boolean                   read  FPolarSnapTracing
                                                            write FPolarSnapTracing ;
      /// <summary>
      ///   Angle for polar snap tracing
      /// </summary>
      property PolarAngle    : Double                       read  FPolarIncAngle
                                                            write FPolarIncAngle ;
      /// <summary>
      ///   True, if ctrl key is pressed.
      /// </summary>
      property CtrlPressed      : Boolean                   read  FCtrlPressed
                                                            write FCtrlPressed ;
      /// <summary>
      ///   True, if shift key is pressed.
      /// </summary>
      property ShiftPressed     : Boolean                   read  FShiftPressed
                                                            write FShiftPressed ;
      /// <summary>
      ///   True, if a tool is updating Editor.
      /// </summary>
      property UpdatingEditor   : Boolean                   read  FUpdatingEditor
                                                            write FUpdatingEditor ;
    published
      /// <event/>
      /// <summary>
      ///   Event to notify about changes.
      /// </summary>
      property NotifyChanges    : TNotifyEvent              read  FNotifyChanges
                                                            write FNotifyChanges ;
      /// <event/>
      /// <summary>
      ///   Event to notify about action.
      /// </summary>
      property NotifyAction     : TGetStrProc               read  FNotifyAction
                                                            write FNotifyAction ;
      /// <event/>
      /// <summary>
      ///   Event to call a custom snap routine.
      /// </summary>
      {$IFDEF CLR}
        event    SnapPointEvent   : TGIS_EditorSnapPointEvent
                                    delegate FOnSnapPoint ;
      {$ELSE}
        property SnapPointEvent   : TGIS_EditorSnapPointEvent
                                    read  FOnSnapPoint
                                    write FOnSnapPoint ;
      {$ENDIF}
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolCircle'}
  /// <summary>
  ///   Tool for building circle (center, radius).
  /// </summary>
  TGIS_EditorToolCircle = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_EditorToolAbstract )
    private
      procedure drawDistanceText ;
      procedure drawHelperLine ;
      procedure drawCircle ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolCircle3P'}
  /// <summary>
  ///   Tool for building circle based on three points.
  /// </summary>
  TGIS_EditorToolCircle3P = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_EditorToolAbstract )
    private
      procedure drawDistanceText ;
      procedure drawHelperLine ;
      procedure drawCircle3P ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolCircle2P'}
  /// <summary>
  ///   Tool for building circle based on three points.
  /// </summary>
  TGIS_EditorToolCircle2P = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_EditorToolAbstract )
    private
      procedure drawDistanceText ;
      procedure drawHelperLine ;
      procedure drawCircle2P ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolLine'}
  /// <summary>
  ///   Tool for building line.
  /// </summary>
  TGIS_EditorToolLine = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_EditorToolAbstract )
    private
      middleDistance : TGIS_Point ;
      lp : TGIS_Point ;
    private
      procedure drawDistanceText ;
      procedure drawAngleText ;
      procedure drawHelperLine ;
      procedure drawDimensionLine ;
      procedure drawLine ;
      procedure drawArc ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolRectangle'}
  /// <summary>
  ///   Tool for building rectangle.
  /// </summary>
  TGIS_EditorToolRectangle = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_EditorToolAbstract )
    private
      procedure drawDistanceText ;
      procedure drawAngleText ;
      procedure drawHelperLine ;
      procedure drawRectangle ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolRectangle90'}
  /// <summary>
  ///   Tool for building rectangle rotated 90 deg.
  /// </summary>
  TGIS_EditorToolRectangle90 = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_EditorToolAbstract )
    private
      points : TGIS_DrawBuf ;
    private
      procedure drawDistanceText ;
      procedure drawAngleText ;
      procedure drawHelperLine ;
      procedure drawLine ;
      procedure drawRectangle90 ;
      procedure drawArc ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolArc'}
  /// <summary>
  ///   Modes of arc tool.
  /// </summary>
  TGIS_EditorToolArcMode = (
    /// <summary>
    ///   Start, end, second point
    /// </summary>
    AmStartEndSecond,
    /// <summary>
    ///   Start, second, end point
    /// </summary>
    AmStartSecondEnd,
    /// <summary>
    ///   Start, center, end point
    /// </summary>
    AmStartCenterEnd,
    /// <summary>
    ///   Start, center point, angle
    /// </summary>
    AmStartCenterAngle,
    /// <summary>
    ///   Start, center point, chord
    /// </summary>
    AmStartCenterChord,
    /// <summary>
    ///   Start, end point, angle
    /// </summary>
    AmStartEndAngle,
    /// <summary>
    ///   Start, end point, radius
    /// </summary>
    AmStartEndRadius,
    /// <summary>
    ///   Center, start, end point
    /// </summary>
    AmCenterStartEnd,
    /// <summary>
    ///   Center, start point, angle
    /// </summary>
    AmCenterStartAngle,
    /// <summary>
    ///   Center, start point, chord
    /// </summary>
    AmCenterStartChord
  ) ;

  /// <summary>
  ///   Tool for building arc using different modes.
  /// </summary>
  TGIS_EditorToolArc = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_EditorToolAbstract )
    private
      arcStartPt  : TGIS_Point ;
      arcSecondPt : TGIS_Point ;
      arcEndPt    : TGIS_Point ;
      arcCenterPt : TGIS_Point ;
      arcMode     : TGIS_EditorToolArcMode ;
    private
      procedure drawDistanceText ;
      procedure drawHelperLine ;
      procedure drawArc ;
      procedure updateEditor( const _pt : TPoint ) ;
      procedure askForNextStep ;
      procedure rememberStep( const _index : Integer ) ;
    private
      procedure fset_Mode( const _mode : TGIS_EditorToolArcMode ) ;
      function  fget_Mode : TGIS_EditorToolArcMode ;
    protected
      /// <inheritdoc/>
      procedure init ; override ;
      /// <inheritdoc/>
      procedure restoreState( const _points : TGIS_PointArray ) ; override ;
    public
      /// <inheritdoc/>
      procedure DoMouseDown   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseMove   ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoMouseUp     ( const _pt : TPoint ) ; override ;
      /// <inheritdoc/>
      procedure DoCanvasPaint ; override ;
    public
      /// <summary>
      ///   Mode used to build arc.
      /// </summary>
      property Mode : TGIS_EditorToolArcMode  read  fget_Mode
                                              write fset_Mode ;
  end ;
{$ENDREGION}

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.SysUtils,
  System.Math,
  GisResource,
  GisRtl ;
{$ENDIF}

{$REGION 'TGIS_EditorUtils'}
type
  TGIS_EditorUtils = class
    class function SqrDistance(
      const _p1 : TPoint ;
      const _p2 : TPoint
    ) : Integer ; overload ; static ;
    class function SqrDistance(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point
    ) : Double ; overload ; static ;
    class function Distance(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point
    ) : Double ; overload ; static ;
    class function Distance(
      const _p1 : TPointF ;
      const _p2 : TPointF
    ) : Single ; overload ; static ;
    class function Distance(
      const _p1 : TPoint ;
      const _p2 : TPoint
    ) : Integer ; overload ; static ;
    class function MiddlePoint(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point
    ) : TGIS_Point ; static ;
    class function Centroid(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point ;
      const _p3 : TGIS_Point
    ) : TGIS_Point ; static ;
    class function InflateRect(
      const _center : TGIS_Point ;
      const _size   : TGIS_Point
    ) : TRectF ; overload ; static ;
    class function InflateRect(
      const _center : TPoint ;
      const _size   : TPoint
    ) : TRect ; overload ; static ;
    class function AngleBy2Points(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point ;
      const _cw : Boolean = True
    ) : Double ; static ;
    class function AngleBy2PointsPi(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point ;
      const _cw : Boolean = True
    ) : Double ; static ;
    class function AngleBy3Points(
      const _p1     : TGIS_Point ;
      const _vertex : TGIS_Point ;
      const _p2     : TGIS_Point ;
      const _cw     : Boolean = True
    ) : Double ; static ;
    class function PolarPointByPtAngle(
      const _p1    : TGIS_Point ;
      const _angle : Double ;
      const _dist  : Double
    ) : TGIS_Point ; static ;
    class function DoubleNear(
      const _a : Double ;
      const _b : Double
    ) : Boolean ; overload ; static ;
    class function DoubleNear(
      const _a         : Double ;
      const _b         : Double ;
      const _tolerance : Double
    ) : Boolean ; overload ; static ;
    class function CircleCenter(
      const _p1   : TGIS_Point ;
      const _p2   : TGIS_Point ;
      const _p3   : TGIS_Point ;
      var _center : TGIS_Point
    ) : Boolean ; static ;
    class function PerpendicularPointOnLine(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point ;
      const _pt : TGIS_Point
    ) : TGIS_Point ; static ;
    class function PerpendicularPointFromLine(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point ;
      const _pt : TGIS_Point
    ) : TGIS_Point ; static ;
    class function InfinityLinePerpOnMiddle(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point
    ) : TArray< TGIS_Point > ; static ;
    class function OffsetPoint(
      const _p  : TGIS_Point ;
      const _dx : Double ;
      const _dy : Double
    ) : TGIS_Point ; static ;
    class function CrossLines(
      const _p1, _p2, _p3, _p4 : TGIS_Point ;
        var _cross             : TGIS_Point
    ) : Boolean ; static ;
    class function AnglePointFromLine(
      const _p1    : TGIS_Point ;
      const _p2    : TGIS_Point ;
      const _pt    : TGIS_Point ;
      const _angle : Double
    ) : TGIS_Point ; static ;
    class procedure CalculateArcAngles(
      const _ptg1   : TGIS_Point ;
      const _ptg2   : TGIS_Point ;
      const _ptg3   : TGIS_Point ;
      const _center : TGIS_Point ;
        var _start  : Double ;
        var _stop   : Double
    ) ; static ;
    class function NormalizeAngle(
      const _angle : Double ;
      const _norm  : Double = 2 * Pi
    ) : Double ; static ;
    class function IsAngleBetweenAngles(
      const _startAngle, _endAngle, _angle : Double
    ) : Boolean ; static ;
    class function MovePoint(
      const _p  : TGIS_Point ;
      const _dx : Double ;
      const _dy : Double
    ) : TGIS_Point ; static ;
    class function LeftOfLine(
      const _p1 : TGIS_Point ;
      const _p2 : TGIS_Point ;
      const _p3 : TGIS_Point
    ) : Double ; static ;
    class function FMod(
      const a, b : Double
    ) : Double ; static ;
  end ;

  { TGIS_EditorUtils }

  class function TGIS_EditorUtils.FMod(
    const a, b : Double
  ) : Double ;
  var
   f : Integer ;
  begin
    f := TruncS( a/b ) ;
    Result := a - (b*f) ;
  end ;

  class function TGIS_EditorUtils.AngleBy2Points(
    const _p1, _p2 : TGIS_Point ;
    const _cw      : Boolean = True
  ) : Double ;
  var
    diffX : Double ;
    diffY : Double ;
    angle : Double ;
  begin
    diffX := _p2.X - _p1.X ;
    diffY := _p2.Y - _p1.Y ;

    if DoubleNear( diffX, 0 ) then begin // vertical
      if _p1.Y < _p2.Y then
        angle := Pi / 2
      else
        angle := Pi * 3 / 2 ;
    end
    else if DoubleNear( diffY, 0 ) then begin // horizontal
      if _p1.X <= _p2.X then
        angle := 0.0
      else
        angle := Pi
    end
    else begin
      angle := ArcTan( diffY / diffX ) ;
      if _cw then begin
        if diffX < 0 then
          angle := Pi + angle
        else if diffY < 0 then
          angle := 2 * Pi + angle ;
      end
    end ;

    Result := angle ;
  end ;

  class function TGIS_EditorUtils.AngleBy2PointsPi(
    const _p1, _p2 : TGIS_Point ;
    const _cw      : Boolean = True
  ) : Double ;
  var
    diffX : Double ;
    diffY : Double ;
    angle : Double ;
  begin
    diffX := _p2.X - _p1.X ;
    diffY := _p2.Y - _p1.Y ;

    if DoubleNear( diffX, 0 ) then begin // vertical
      if _p1.Y < _p2.Y then
        angle := Pi / 2
      else
        angle := Pi / 2 ;
    end
    else if DoubleNear( diffY, 0 ) then begin // horizontal
      if _p1.X <= _p2.X then
        angle := 0.0
      else
        angle := Pi
    end
    else begin
      angle := ArcTan( diffY / diffX ) ;
      if _cw then begin
        if diffX < 0 then
          angle := Pi + angle
        else if diffY < 0 then
          angle := 2 * Pi + angle ;
      end
      else begin
        if diffX < 0 then
          angle := Pi - angle
        else if diffY < 0 then
          angle := - angle ;
      end ;
    end ;

    Result := angle ;
  end ;

  class function TGIS_EditorUtils.AngleBy3Points(
    const _p1     : TGIS_Point ;
    const _vertex : TGIS_Point ;
    const _p2     : TGIS_Point ;
    const _cw     : Boolean = True
  ) : Double ;
  var
    angle1, angle2 : Double ;
  begin
    angle1 := AngleBy2Points( _p1, _vertex ) ;
    angle2 := AngleBy2Points( _p2, _vertex ) ;
    if _cw then begin
      if angle2 > angle1 then
        Result := ( 2 * Pi ) - ( angle2 - angle1 )
      else
        Result := angle1 - angle2
    end
    else begin
      if angle2 < angle1 then
        Result := ( 2 * Pi ) - ( angle1 - angle2 )
      else
        Result := angle2 - angle1
    end ;
  end ;

  class function TGIS_EditorUtils.MiddlePoint(
    const _p1 : TGIS_Point ;
    const _p2 : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := GisPoint( ( _p1.X + _p2.X ) / 2, ( _p1.Y + _p2.Y ) / 2 ) ;
  end ;

  class function TGIS_EditorUtils.MovePoint(
    const _p       : TGIS_Point ;
    const _dx, _dy : Double
  ) : TGIS_Point ;
  begin
    Result := GisPoint( _p.X + _dx, _p.Y + _dy ) ;
  end ;

  class procedure TGIS_EditorUtils.CalculateArcAngles(
    const _ptg1, _ptg2, _ptg3, _center : TGIS_Point ;
      var _start, _stop                : Double
  ) ;
  var
    p1_angle, p2_angle, p3_angle : Double ;
  begin
    p1_angle := - 1 * ArcTan2( _ptg1.Y - _center.Y, _ptg1.X - _center.X ) ;
    p2_angle := - 1 * ArcTan2( _ptg2.Y - _center.Y, _ptg2.X - _center.X ) ;
    p3_angle := - 1 * ArcTan2( _ptg3.Y - _center.Y, _ptg3.X - _center.X ) ;

    // Try positive (clockwise?) winding.
    while ( p2_angle < p1_angle ) do
      p2_angle := p2_angle + 2 * Pi ;

    while ( p3_angle < p2_angle ) do
      p3_angle := p3_angle + 2 * Pi ;

    // If that doesn't work out, then go anticlockwise.
    if ( p3_angle - p1_angle > 2 * Pi ) then begin
      while ( p2_angle > p1_angle ) do
        p2_angle := p2_angle - 2 * Pi ;

      while ( p3_angle > p2_angle ) do
        p3_angle := p3_angle - 2 * Pi ;
    end ;

    _start := p1_angle ;
    _stop := p3_angle ;
  end ;

  class function TGIS_EditorUtils.Centroid(
    const _p1 : TGIS_Point ;
    const _p2 : TGIS_Point ;
    const _p3 : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := GisPoint( ( _p1.X + _p2.X + _p3.X ) / 3,
                        ( _p1.Y + _p2.Y + _p3.Y ) / 3 ) ;
  end ;

  class function TGIS_EditorUtils.OffsetPoint(
    const _p       : TGIS_Point ;
    const _dx, _dy : Double
  ) : TGIS_Point ;
  begin
    Result := GisPoint( _p.X - _dx, _p.Y - _dy ) ;
  end ;

  class function TGIS_EditorUtils.Distance(
    const _p1 : TPointF ;
    const _p2 : TPointF
  ) : Single ;
  var
    dst : Single ;
  begin
    dst := Sqr( 0.0 + _p2.X - _p1.X ) + Sqr( 0.0 + _p2.Y - _p1.Y ) ;
    if dst <> 0 then
      Result := Sqrt( dst )
    else
      Result := dst ;
  end ;

  class function TGIS_EditorUtils.Distance(
    const _p1 : TPoint ;
    const _p2 : TPoint
  ) : Integer ;
  var
    dst : Double ;
    i : UInt64 ;
  begin
    dst := Sqr( 0.0 + _p2.X - _p1.X ) + Sqr( 0.0 + _p2.Y - _p1.Y ) ;
    if dst <> 0 then begin
      i := TruncS( Sqrt( dst ) ) ;
      if i > MaxInt then
        Result := MaxInt
      else
        Result := Integer( i ) ;
    end
    else
      Result := TruncS( dst ) ;
  end ;

  class function TGIS_EditorUtils.Distance(
    const _p1 : TGIS_Point ;
    const _p2 : TGIS_Point
  ) : Double ;
  var
    dst : Double ;
  begin
    dst := SqrDistance( _p1, _p2 ) ;
    if dst <> 0 then
      Result := Sqrt( dst )
    else
      Result := dst ;
  end ;

  class function TGIS_EditorUtils.DoubleNear(
  const _a, _b : Double
  ) : Boolean ;
  begin
    Result := DoubleNear( _a, _b, 1E-7 ) ;
  end ;

  class function TGIS_EditorUtils.DoubleNear(
    const _a, _b, _tolerance : Double
  ) : Boolean ;
  begin
    if _a > _b then
      Result := ( _a - _b ) <= _tolerance
    else
      Result := ( _b - _a ) <= _tolerance ;
  end ;

  class function TGIS_EditorUtils.InflateRect(
    const _center : TGIS_Point ;
    const _size   : TGIS_Point
  ) : TRectF ;
  begin
    Result := RectF( _center.X - ( _size.X / 2 ), _center.Y - _size.Y,
                     _center.X + ( _size.X / 2 ), _center.Y ) ;
  end ;

  class function TGIS_EditorUtils.InflateRect(
    const _center : TPoint ;
    const _size   : TPoint
  ) : TRect ;
  begin
    Result := Rect( _center.X - ( _size.X div 2 ), _center.Y - _size.Y,
                    _center.X + ( _size.X div 2 ), _center.Y ) ;
  end ;

  class function TGIS_EditorUtils.IsAngleBetweenAngles(
    const _startAngle, _endAngle, _angle : Double
  ) : Boolean ;
  var
    angle : Double ;
  begin
    angle := FMod( _angle, Pi * 2 ) ;

    Result := False ;
    if _startAngle < _endAngle then begin
      if ( ( angle > _startAngle ) or DoubleNear( angle, _startAngle ) ) and
        ( ( angle < _endAngle ) or DoubleNear( angle, _endAngle ) ) then
        Result := True
    end
    else begin
      if ( ( angle > 0 ) or DoubleNear( angle, 0 ) ) and
        ( ( angle < _endAngle ) or DoubleNear( angle, _endAngle ) ) then
        Result := True
      else if ( ( angle < ( Pi * 2 ) ) or DoubleNear( angle, ( Pi * 2 ) ) ) and
        ( ( angle > _startAngle ) or DoubleNear( angle, _startAngle ) ) then
        Result := True
    end ;
  end ;

  class function TGIS_EditorUtils.PerpendicularPointFromLine(
    const _p1, _p2, _pt : TGIS_Point
  ) : TGIS_Point ;
  var
    dist, angle : Double ;
  begin
    dist := Distance( _p2, _pt ) ;
    if dist = 0 then
      Result := GisPoint( _p2.X, _p2.Y )
    else begin
      if LeftOfLine( _p1, _p2, _pt ) > 0 then
        angle := AngleBy2Points( _p1, _p2 ) - Pi / 2
      else
        angle := AngleBy2Points( _p1, _p2 ) + Pi / 2 ;

      Result := PolarPointByPtAngle( _p2, angle, dist ) ;
    end ;
  end ;

  class function TGIS_EditorUtils.InfinityLinePerpOnMiddle(
    const _p1 : TGIS_Point ;
    const _p2 : TGIS_Point
  ) : TArray<TGIS_Point> ;
  var
    ptMiddle, pt2Middle : TGIS_Point ;
    dist, angle : Double ;
  begin
    ptMiddle := MiddlePoint( _p1, _p2 ) ;
    dist := Distance( _p1, ptMiddle ) ;
    if dist = 0 then begin
      Result := [ ] ;
      exit ;
    end ;
    angle := AngleBy2Points( _p1, _p2 ) + Pi / 2 ;
    pt2Middle := PolarPointByPtAngle( ptMiddle, angle, dist ) ;
    Result := [ ptMiddle, pt2Middle ] ;
  end ;

  class function TGIS_EditorUtils.AnglePointFromLine(
    const _p1    : TGIS_Point ;
    const _p2    : TGIS_Point ;
    const _pt    : TGIS_Point ;
    const _angle : Double
  ) : TGIS_Point ;
  var
    dist, angle : Double ;
  begin
    dist := Distance( _p2, _pt ) ;
    if dist = 0 then
      Result := GisPoint( _p2.X, _p2.Y )
    else begin
      if _pt.Y < _p2.Y then
        angle := AngleBy2Points( _p1, _p2 ) - Pi - _angle
      else
        angle := AngleBy2Points( _p1, _p2 ) + Pi - _angle ;

      Result := PolarPointByPtAngle( _p2, angle, dist ) ;
    end ;
  end ;

  class function TGIS_EditorUtils.PerpendicularPointOnLine(
    const _p1, _p2, _pt : TGIS_Point
  ) : TGIS_Point ;
  var
    diffX, diffY : Double ;
    X, Y, coeff  : Double ;
  begin
    diffX := _p2.X - _p1.X ;
    diffY := _p2.Y - _p1.Y ;

    if DoubleNear( diffX, 0 ) then
      Result := GisPoint( _p1.X, _pt.Y )
    else if DoubleNear( diffY, 0 ) then
      Result := GisPoint( _pt.X, _p1.Y )
    else begin
      coeff := diffY / diffX ;
      X := ( coeff * _p1.X - _p1.Y + _pt.X / coeff + _pt.Y ) /
        ( coeff + 1 / coeff ) ;
      Y := coeff * ( X - _p1.X ) + _p1.Y ;

      Result := GisPoint( X, Y ) ;
    end ;
  end ;

  class function TGIS_EditorUtils.PolarPointByPtAngle(
    const _p1           : TGIS_Point ;
    const _angle, _dist : Double
  ) : TGIS_Point ;
  var
    X, Y : Double ;
  begin
    Y := _dist * Sin( _angle ) ;
    X := _dist * Cos( _angle ) ;
    Result := GisPoint( _p1.X + X, _p1.Y + Y ) ;
  end ;

  class function TGIS_EditorUtils.SqrDistance(
    const _p1 : TGIS_Point ;
    const _p2 : TGIS_Point
  ) : Double ;
  begin
    Result := Sqr( _p2.X - _p1.X ) + Sqr( _p2.Y - _p1.Y ) ;
  end ;

  class function TGIS_EditorUtils.SqrDistance(
    const _p1 : TPoint ;
    const _p2 : TPoint
  ) : Integer ;
  var
    i : Int64 ;
  begin
    i := TruncS( Sqr( 0.0 + _p2.X - _p1.X ) + Sqr( 0.0 + _p2.Y - _p1.Y ) ) ;
    Result := Integer( i ) ;
  end ;

  class function TGIS_EditorUtils.CircleCenter(
    const _p1, _p2, _p3 : TGIS_Point ;
    var _center   : TGIS_Point
  ) : Boolean ;
  var
    X, Y       : Double ;
    ma, mb     : Double ;
    p1, p2, p3 : TGIS_Point ;

    procedure swap( var _p1, _p2 : TGIS_Point ) ;
    var
      temp : TGIS_Point ;
    begin
      temp := _p1 ;
      _p1 := _p2 ;
      _p2 := temp ;
    end ;

  begin
    Result := True ;
    ma := 0 ;
    mb := 0 ;
    p1 := _p1 ;
    p2 := _p2 ;
    p3 := _p3 ;

    if ( p1.X = p2.X ) or ( p1.Y = p2.Y ) then
      swap( p1, p3 ) ;
    if ( p2.X = p3.X ) then
      swap( p1, p2 ) ;

    if p1.X <> p2.X then
      ma := ( p2.Y - p1.Y ) / ( p2.X - p1.X )
    else
      Result := False ;
    if p2.X <> p3.X then
      mb := ( p3.Y - p2.Y ) / ( p3.X - p2.X )
    else
      Result := False ;
    if ( ma = 0 ) and ( mb = 0 ) then
      Result := False ;
    if ma = mb then
      Result := False ;

    if Result = True then begin
      X := ( ma * mb * ( p1.Y - p3.Y ) + mb * ( p1.X + p2.X ) - ma *
        ( p2.X + p3.X ) ) / ( 2 * ( mb - ma ) ) ;
      if ma <> 0 then
        Y := - ( X - ( p1.X + p2.X ) / 2 ) / ma + ( p1.Y + p2.Y ) / 2
      else
        Y := - ( X - ( p2.X + p3.X ) / 2 ) / mb + ( p2.Y + p3.Y ) / 2 ;

      {$IFDEF GIS_NORECORDS} 
      _center := TGIS_Point.create 
      {$ENDIF} ;
      _center.X := X ;
      _center.Y := Y ;
    end ;
  end ;

  class function TGIS_EditorUtils.CrossLines(
    const _p1, _p2, _p3, _p4 : TGIS_Point ;
    var _cross               : TGIS_Point
  ) : Boolean ;
  var
    la, lb, mab : Double ;
    ua, ub : Double ;
  begin
    {$IFDEF GIS_NORECORDS} 
    _cross := TGIS_Point.create 
    {$ENDIF} ;

    la := ( _p4.X - _p3.X ) * ( _p1.Y - _p3.Y ) - ( _p4.Y - _p3.Y ) *
      ( _p1.X - _p3.X ) ;
    lb := ( _p2.X - _p1.X ) * ( _p1.Y - _p3.Y ) - ( _p2.Y - _p1.Y ) *
      ( _p1.X - _p3.X ) ;
    mab := ( _p4.Y - _p3.Y ) * ( _p2.X - _p1.X ) - ( _p4.X - _p3.X ) *
      ( _p2.Y - _p1.Y ) ;

    if not GisIsSameValue( mab, 0.0 ) then begin
      ua := la / mab ;
      ub := lb / mab ;
      if ( ( ua >= 0.0 ) and ( ua <= 1.0 ) and ( ub >= 0.0 ) and ( ub <= 1.0 ) )
      then begin
        _cross.X := _p1.X + ua * ( _p2.X - _p1.X ) ;
        _cross.Y := _p1.Y + ua * ( _p2.Y - _p1.Y ) ;
        Result := True ; // crossing point inside lines
      end
      else begin
        _cross.X := _p1.X + ua * ( _p2.X - _p1.X ) ;
        _cross.Y := _p1.Y + ua * ( _p2.Y - _p1.Y ) ;
        Result := True ; // crossing point outside lines
      end
    end
    else begin
      Result := False ; // lines are identical or parallel
    end ;
  end ;

  class function TGIS_EditorUtils.NormalizeAngle(
    const _angle : Double ;
    const _norm  : Double = 2 * Pi
  ) : Double ;
  begin
    if _angle = 0 then
      Result := 0
    else if _angle > 0 then
      Result := FMod( _angle, _norm )
    else
      Result := _norm - FMod( - _angle, _norm ) ;
  end ;

  class function TGIS_EditorUtils.LeftOfLine(
    const _p1 : TGIS_Point ;
    const _p2 : TGIS_Point ;
    const _p3 : TGIS_Point
  ) : Double ;
  var
    f1, f2, f3, f4 : Double ;
  begin
    f1 := _p1.X - _p2.X ;
    f2 := _p3.Y - _p2.Y ;
    f3 := _p1.Y - _p2.Y ;
    f4 := _p3.X - _p2.X ;
    Result := f1 * f2 - f3 * f4 ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EditorToolAbstract'}
{ TGIS_EditorToolAbstract }

  constructor TGIS_EditorToolAbstract.Create ;
  begin
    inherited ;

    FLockedParams := TGIS_EditorToolLockedParams.Create ;
    init ;
  end ;

  {$IFDEF DCC}
  destructor TGIS_EditorToolAbstract.Destroy ;
  begin
    FreeObject( FLockedParams ) ;

    inherited ;
  end ;
  {$ENDIF}

  procedure TGIS_EditorToolAbstract.init ;
  begin
    FLockedParams.Distance := GIS_MAX_DOUBLE ;
    FLockedParams.Angle    := GIS_MAX_DOUBLE ;
    FLockedParams.RelativeAngle := False ;
    FLastPoint.X := GIS_MAX_DOUBLE ;
    FLastPoint.Y := GIS_MAX_DOUBLE ;
  end ;

  procedure TGIS_EditorToolAbstract.resetToolPoints ;
  var
    ptg : TGIS_Point ;
  begin
    init ;
    restoreState( [] ) ;
    if FEditor.PointCount > 0 then begin
      if assigned( FLayer ) then
        ptg := GisPoint2DFrom3D(FLayer.Project3D(FEditor.Point[FEditor.PointPos]))
      else
        ptg := GisPoint2DFrom3D( FEditor.Point[FEditor.PointPos] ) ;

      DoMouseDown( TGIS_EditorHelper(FParent).Viewer.MapToScreen(ptg) ) ;
    end ;
  end ;

  procedure TGIS_EditorToolAbstract.restoreState(
    const _points : TGIS_PointArray
  ) ;
  begin
  end ;

  function TGIS_EditorToolAbstract.isAngleLocked : Boolean ;
  begin
    Result := not GisIsSameValue(LockedParams.Angle, GIS_MAX_DOUBLE) ;
  end ;

  function TGIS_EditorToolAbstract.isDistanceLocked : Boolean ;
  begin
    Result := not GisIsSameValue(LockedParams.Distance, GIS_MAX_DOUBLE) ;
  end ;

  function TGIS_EditorToolAbstract.isCtrlPressed : Boolean ;
  begin
    Result := TGIS_EditorHelper(FParent).CtrlPressed ;
  end ;

  function TGIS_EditorToolAbstract.isShiftPressed : Boolean ;
  begin
    Result := TGIS_EditorHelper(FParent).ShiftPressed ;
  end ;

  procedure TGIS_EditorToolAbstract.prepareHelperPen ;
  begin
    FRenderer.CanvasPen.Width   := 2 ;
    FRenderer.CanvasPen.Color   := TGIS_Color.Gray ;
    FRenderer.CanvasPen.Style   := TGIS_PenStyle.Dot ;
    FRenderer.CanvasBrush.Style := TGIS_BrushStyle.Clear ;
  end ;

  procedure TGIS_EditorToolAbstract.prepareMainPen ;
  begin
    FRenderer.CanvasPen.Width   := 2 ;
    FRenderer.CanvasPen.Color   := TGIS_Color.Red ;
    FRenderer.CanvasPen.Style   := TGIS_PenStyle.Dash ;
    FRenderer.CanvasBrush.Style := TGIS_BrushStyle.Clear ;
  end ;

  function TGIS_EditorToolAbstract.screenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    Result := TGIS_EditorHelper(FParent).Viewer.ScreenToMap( _pt ) ;
  end ;

  function TGIS_EditorToolAbstract.screenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    Result := TGIS_EditorHelper(FParent).Viewer.ScreenToMap3D( _pt ) ;
  end ;

  function TGIS_EditorToolAbstract.mapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  begin
    Result := TGIS_EditorHelper(FParent).Viewer.MapToScreen( _ptg ) ;
  end ;

  function TGIS_EditorToolAbstract.mapToScreenBuffer(
    const _buffer : TArray<TGIS_Point>
  ) : TGIS_DrawBuf ;
  var
    i, cnt : Integer ;
  begin
    cnt := length( _buffer ) ;
    SetLength( Result, cnt ) ;
    for i := 0 to cnt-1 do
      Result[i] := mapToScreen( _buffer[i] ) ;
  end ;

  function TGIS_EditorToolAbstract.rectFToRect(
    const _rect : TRectF
  ) : TRect ;
  begin
    Result := Rect( mapToScreen( GisPoint( _rect.Left , _rect.Top    ) ),
                    mapToScreen( GisPoint( _rect.Right, _rect.Bottom ) )
                  ) ;
  end ;

  function TGIS_EditorToolAbstract.twipsToPixels(
    const _size : Integer
  ) : Integer ;
  begin
    Result := FRenderer.TwipsToPixels( _size ) ;
  end ;

  procedure TGIS_EditorToolAbstract.applyLockedParams ;
  var
    cw : Boolean ;
  begin
    if FPointsCount >= length( FPoints ) then exit ;

    cw := FPoints[FPointsCount].Y > FPoints[FPointsCount-1].Y ;

    if isDistanceLocked or isAngleLocked then begin
      if isDistanceLocked then
        FDistance  := LockedParams.Distance
      else
        FDistance := TGIS_EditorUtils.Distance( FPoints[FPointsCount-1],
                                                FPoints[FPointsCount] ) ;

      if isAngleLocked then begin
        if cw then
          FAngle := LockedParams.Angle
        else
          FAngle := -LockedParams.Angle
      end
      else begin
        if cw then
          FAngle := RadToDeg( TGIS_EditorUtils.AngleBy2PointsPi(
                                FPoints[FPointsCount-1], FPoints[FPointsCount],
                                cw
                              ) )
        else
          FAngle := -RadToDeg( TGIS_EditorUtils.AngleBy2PointsPi(
                                  FPoints[FPointsCount-1], FPoints[FPointsCount],
                                  cw
                               ) )
      end ;
      FPoints[FPointsCount] := TGIS_EditorUtils.PolarPointByPtAngle(
                                  FPoints[FPointsCount-1], DegToRad(FAngle), FDistance
                               ) ;
    end
    else begin
      FDistance  := TGIS_EditorUtils.Distance( FPoints[FPointsCount-1], FPoints[FPointsCount] ) ;
      FAngle     := RadToDeg( TGIS_EditorUtils.AngleBy2PointsPi(
                                FPoints[FPointsCount-1], FPoints[FPointsCount], cw )
                            ) ;
    end ;
    FDistanceI := TGIS_EditorUtils.Distance(
                    mapToScreen( FPoints[FPointsCount-1] ),
                    mapToScreen( FPoints[FPointsCount] )
                  ) ;
  end ;

  procedure TGIS_EditorToolAbstract.drawCanvasText(
    const _str  : String ;
    const _rect : TRect
  ) ;
  begin
    FRenderer.CanvasFont.Color  := TGIS_Color.Blue ;
    FRenderer.CanvasPen.Width   := 1 ;
    FRenderer.CanvasPen.Color   := TGIS_Color.Gray ;
    FRenderer.CanvasPen.Style   := TGIS_PenStyle.Solid ;
    FRenderer.CanvasBrush.Style := TGIS_BrushStyle.Clear ;

    FRenderer.CanvasDrawRectangle( _rect ) ;
    FRenderer.CanvasDrawText( _rect, _str ) ;
  end ;

  procedure TGIS_EditorToolAbstract.beginUpdatingEditor ;
  begin
    if assigned( FParent ) then
      TGIS_EditorHelper(FParent).UpdatingEditor := True ;
  end;

  procedure TGIS_EditorToolAbstract.endUpdatingEditor ;
  begin
    if assigned( FParent ) then
      TGIS_EditorHelper(FParent).UpdatingEditor := False ;
  end;

  procedure TGIS_EditorToolAbstract.notifyAction(
    const _prompt : String
  ) ;
  begin
    if assigned( FParent ) and assigned( TGIS_EditorHelper(FParent).FNotifyAction ) then
      TGIS_EditorHelper(FParent).FNotifyAction( _prompt ) ;
  end ;

  { TGIS_EditorToolLockedParams }

  procedure TGIS_EditorToolLockedParams.LockAngle(
    const _value : Double
  ) ;
  begin
    FAngle := _value ;
  end ;

  procedure TGIS_EditorToolLockedParams.LockDistance(
    const _value : Double
  ) ;
  begin
    FDistance := _value ;
  end ;

  procedure TGIS_EditorToolLockedParams.LockRelativeAngle(
    const _value : Boolean
  ) ;
  begin
    FRelativeAngle := _value ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorHelper'}
  { TGIS_EditorHelper }

  constructor TGIS_EditorHelper.Create(
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    FViewer       := _viewer ;
    FRenderer     := FViewer.ViewerParent.ControlRenderer as TGIS_RendererAbstract ;
    FEditor       := FViewer.Editor ;
    FDrawingTool  := nil ;

    FSnapTracing      := False ;
    FUseSnapTracing   := False ;
    FUseSnapTracingE  := False ;
    FUseSnapTracingS  := False ;
    FSnapPointType    := TGIS_EditorSnapResultType.Vertex ;
    FPolarSnapTracing := False ;
    FPolarIncAngle    := Pi/2 ;
    FCtrlPressed      := False ;
    FShiftPressed     := False ;
    FUpdatingEditor   := False ;
  end ;

  {$IFDEF DCC}
  destructor TGIS_EditorHelper.Destroy ;
  begin
    FreeObject( FDrawingTool ) ;

    inherited ;
  end ;
  {$ENDIF}

  procedure TGIS_EditorHelper.fset_DrawingTool(
    const _tool : TGIS_EditorToolAbstract
  ) ;
  var
    last_ptg   : TGIS_Point ;
    has_lastpt : Boolean ;
  begin
    if FDrawingTool = _tool then exit ;
    has_lastpt := False ;

    if assigned( FDrawingTool ) then begin
      last_ptg := FDrawingTool.LastPoint ;
      if not GisIsSameValue(last_ptg.X, GIS_MAX_DOUBLE) and
         not GisIsSameValue(last_ptg.Y, GIS_MAX_DOUBLE) then
      has_lastpt := True ;
    end ;
    if not has_lastpt and (FEditor.PointCount > 0) then begin
      if assigned( FLayer ) then
        last_ptg := GisPoint2DFrom3D(FLayer.Project3D(FEditor.Point[FEditor.PointPos]))
      else
        last_ptg := GisPoint2DFrom3D( FEditor.Point[FEditor.PointPos] ) ;
      has_lastpt := True ;
    end ;

    FreeObject( FDrawingTool ) ;

    FDrawingTool := _tool ;
    if assigned( FDrawingTool ) then begin
      FDrawingTool.Renderer := FRenderer ;
      FDrawingTool.Parent := Self ;
      FDrawingTool.Editor := FEditor ;
      FDrawingTool.Layer  := FLayer ;
      FDrawingTool.init ;
      if has_lastpt then
        FDrawingTool.DoMouseDown( FViewer.MapToScreen( last_ptg ) ) ;
    end ;
  end ;

  procedure TGIS_EditorHelper.fset_Layer(
    const _layer : TGIS_Layer
  ) ;
  begin
    FLayer := _layer ;
    if assigned( FDrawingTool ) then
      FDrawingTool.Layer := FLayer ;
  end ;

  procedure TGIS_EditorHelper.fset_Renderer(
    const _renderer : TGIS_RendererAbstract
  ) ;
  begin
    FRenderer := _renderer ;
    if assigned( FDrawingTool ) then
      FDrawingTool.Renderer := FRenderer ;
  end ;

  procedure TGIS_EditorHelper.doTracePaint ;
  var
    rdr : TGIS_RendererAbstract ;
    pt  : TPoint ;
    ps  : Integer ;
  begin
    if not FSnapTracing then exit ;
    if not FEditor.ViewerEnabled then exit ;

    rdr := FRenderer ;
    if FUseSnapTracing then begin
      if IsNan( FSnapTracePoint.X ) then exit ;

      rdr.CanvasPen.Width   := 2 ;
      rdr.CanvasPen.Color   := TGIS_Color.Green ;
      rdr.CanvasPen.Style   := TGIS_PenStyle.Solid ;
      rdr.CanvasBrush.Style := TGIS_BrushStyle.Clear ;

      pt := FViewer.MapToScreen( FSnapTracePoint ) ;
      ps := FViewer.TwipsToPixels( 60 ) ;

      rdr.CanvasDrawRectangle( Rect( pt.X - ps, pt.Y - ps, pt.X + ps, pt.Y + ps) ) ;
    end ;

    if FUseSnapTracingE then begin
      if IsNan( FSnapTracePointE.X ) then exit ;
      doPolarPaint( FSnapTracePointE, GisPoint2DFrom3D(FEditor.Point[FEditor.PointCount-1]), TGIS_Color.Blue ) ;
    end ;

    if FUseSnapTracingS then begin
      if IsNan( FSnapTracePointS.X ) then exit ;
      doPolarPaint( FSnapTracePointS, FSnapTracePoint, TGIS_Color.Green ) ;
    end ;
  end ;

  procedure TGIS_EditorHelper.doPolarPaint(
    const _pt    : TGIS_Point ;
    const _pte   : TGIS_Point ;
    const _color : TGIS_Color
  ) ;
  var
    rdr : TGIS_RendererAbstract ;
    ps  : Integer ;
    dst : Double ;
    ang : Double ;
    pte : TPoint ;
    pt  : TPoint ;
    ptg : TGIS_Point ;
  begin
    rdr := FRenderer ;
    rdr.CanvasPen.Width   := 2 ;
    rdr.CanvasPen.Color   := _color ;
    rdr.CanvasPen.Style   := TGIS_PenStyle.Solid ;
    rdr.CanvasBrush.Style := TGIS_BrushStyle.Clear ;

    // check if point is valid NAN ?
    if IsNan( FSnapTracePointS.X ) then exit ;

    ps := FViewer.TwipsToPixels( 60 ) ;
    pt := FViewer.MapToScreen( _pt ) ;
    rdr.CanvasDrawRectangle( Rect( pt.X - ps, pt.Y - ps, pt.X + ps, pt.Y + ps) ) ;

    if FSnapPointType = TGIS_EditorSnapResultType.Edge then begin
      rdr.CanvasPen.Width := 1 ;
      rdr.CanvasPen.Style := TGIS_PenStyle.Dash ;

      if _pte.X <> _pt.X then
        dst := (FViewer.VisibleExtent.XMax-FViewer.VisibleExtent.XMin)
      else
        dst := (FViewer.VisibleExtent.YMax-FViewer.VisibleExtent.YMin) ;

      ang := TGIS_EditorUtils.AngleBy2Points( _pte, _pt ) ;
      ptg := TGIS_EditorUtils.PolarPointByPtAngle( _pte, ang, dst ) ;
      pte := FViewer.MapToScreen( ptg ) ;
      pt  := FViewer.MapToScreen( _pte ) ;
      rdr.CanvasDrawLine( pt.X, pt.Y, pte.X, pte.Y ) ;
    end ;
  end ;

  function TGIS_EditorHelper.findPolarPoint(
    const _refPt : TGIS_Point ;
    const _pt    : TGIS_Point ;
      var _ptg   : TGIS_Point
  ) : Boolean ;
  var
    p1   : TGIS_Point ;
    dist : Double ;
    dst  : Double ;
    mrgn : Double ;
    a    : Double ;
  begin
    Result := False ;

    mrgn := FViewer.TwipsToPixels( 300 ) / FViewer.Zoom ;
    dist := TGIS_EditorUtils.Distance( _refPt, _pt ) ;
    a := 2 * Pi ;
    while a > 0 do begin
      p1   := TGIS_EditorUtils.PolarPointByPtAngle( _refPt, a, dist ) ;
      dst  := TGIS_EditorUtils.Distance( p1, _pt ) ;
      if dst < mrgn then begin
        _ptg   := p1 ;
        Result := True ;
        break ;
      end ;
      a := a - FPolarIncAngle ;
    end ;
  end ;

  procedure TGIS_EditorHelper.DoCanvasPaint ;
  begin
    if assigned( FDrawingTool ) then
      FDrawingTool.DoCanvasPaint ;

    doTracePaint ;
  end ;

  procedure TGIS_EditorHelper.DoStartEdit ;
  var
    ptg : TGIS_Point ;
  begin
    if assigned( FDrawingTool ) then begin
      FDrawingTool.init ;
      FDrawingTool.restoreState( [] ) ;
      FEditor.EditorMode := TGIS_EditorModeEx.Extended ;

      if FEditor.PointCount > 0 then begin
        if assigned( FLayer ) then
          ptg := GisPoint2DFrom3D(FLayer.Project3D(FEditor.Point[FEditor.PointCount-1]))
        else
          ptg := GisPoint2DFrom3D( FEditor.Point[FEditor.PointCount-1] ) ;

        FDrawingTool.DoMouseDown( FViewer.MapToScreen(ptg) ) ;
      end ;
    end ;
  end ;

  procedure TGIS_EditorHelper.DoEndEdit ;
  begin
    FUseSnapTracing   := False ;
    FUseSnapTracingE  := False ;
    FUseSnapTracingS  := False ;
    FHasSnapPoint     := False ;

    if assigned( FDrawingTool ) then
      FDrawingTool.init ;
  end ;

  procedure TGIS_EditorHelper.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    if assigned( FDrawingTool ) then begin
      FDrawingTool.DoMouseDown( _pt ) ;
    end ;
  end ;

  procedure TGIS_EditorHelper.DoMouseMove(
    const _pt : TPoint
  ) ;
  var
    ptg    : TGIS_Point ;
    ptg3D  : TGIS_Point3D ;
    pt     : TGIS_Point ;
    ptgres : TGIS_Point ;
  begin
    if assigned( FDrawingTool ) then
      FDrawingTool.DoMouseMove( _pt ) ;

    if FSnapTracing then begin
      if doFindSnapPoint( ptg3D ) then begin
        FSnapPointType  := TGIS_EditorSnapResultType.Vertex ;
        FSnapTracePoint := GisPoint2DFrom3D( ptg3D ) ;
        FUseSnapTracing := True ;
        FHasSnapPoint   := True ;
      end
      else if FEditor.FindSnapPoint( _pt, ptg, FSnapPointType ) then begin
        FSnapPointType  := TGIS_EditorSnapResultType.Vertex ;
        FSnapTracePoint := ptg ;
        FUseSnapTracing := True ;
        FHasSnapPoint   := True ;
      end
      else begin
        FUseSnapTracing := False ;
        pt := FViewer.ScreenToMap( _pt ) ;
        if (FEditor.PointCount > 0) and FPolarSnapTracing and
          findPolarPoint( GisPoint2DFrom3D( FEditor.Point[FEditor.PointCount-1] ),
                          pt, FSnapTracePointE ) then
        begin
          FUseSnapTracingE := True ;
          FSnapPointType  := TGIS_EditorSnapResultType.Edge ;
        end
        else
          FUseSnapTracingE := False ;

        if FHasSnapPoint then begin
          if FPolarSnapTracing and findPolarPoint( FSnapTracePoint, pt, FSnapTracePointS ) then begin
            FUseSnapTracingS := True ;
            FSnapPointType   := TGIS_EditorSnapResultType.Edge ;
          end
          else
            FUseSnapTracingS := False ;
        end ;

        if FUseSnapTracingE and FUseSnapTracingS then begin
          if TGIS_EditorUtils.CrossLines(
            FSnapTracePoint, FSnapTracePointS,
            GisPoint2DFrom3D( FEditor.Point[FEditor.PointCount-1] ), FSnapTracePointE,
            ptgres
          ) then begin
            FUseSnapTracingE := False ;
            FSnapTracePointS := ptgres ;
          end ;
        end ;
      end
    end ;
  end ;

  procedure TGIS_EditorHelper.DoMouseUp(
    const _pt : TPoint
  ) ;
  var
    pt : TPoint ;
  begin
    if FSnapTracing then begin
      if FUseSnapTracing then
        pt := FViewer.MapToScreen( FSnapTracePoint )
      else if FUseSnapTracingE then
        pt := FViewer.MapToScreen( FSnapTracePointE )
      else if FUseSnapTracingS then
        pt := FViewer.MapToScreen( FSnapTracePointS )
      else
        pt := _pt ;
    end
    else
      pt := _pt ;

    pt := FViewer.MapToScreen3D( doSnapPoint( FViewer.ScreenToMap3D( pt ) ) ) ;

    if assigned( FDrawingTool ) then begin
      FDrawingTool.DoMouseUp( pt ) ;
    end ;

    FUseSnapTracing   := False ;
    FUseSnapTracingE  := False ;
    FUseSnapTracingS  := False ;
    FHasSnapPoint     := False ;
  end ;

  function TGIS_EditorHelper.doSnapPoint(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
 var
  {$IFDEF OXYGENE}
     args    : TGIS_EditorSnapPointEventArgs ;
  {$ENDIF}
    dst       : Double     ;
    mrgn      : Double     ;
  begin
    mrgn    := FViewer.TwipsToPixels( GIS_SNAP_MARGIN ) / FViewer.Zoom ;
    Result  := _ptg ;
    dst     := GIS_MAX_DOUBLE ;

    if assigned( FOnSnapPoint ) then
      {$IFDEF OXYGENE}
        begin
          args := TGIS_EditorSnapPointEventArgs.Create(
                    _ptg, mrgn, Result, dst
                  ) ;
          FOnSnapPoint( Self, args ) ;
          Result := args.Proj ;
        end
      {$ELSE}
        FOnSnapPoint( Self, _ptg, mrgn, Result, dst )
      {$ENDIF}
    else
      Result := _ptg ;
  end ;

  function TGIS_EditorHelper.doFindSnapPoint(
    var _ptg   : TGIS_Point3D
  ) : Boolean ;
 var
  {$IFDEF OXYGENE}
     args    : TGIS_EditorSnapPointEventArgs ;
  {$ENDIF}
    dst       : Double     ;
    mrgn      : Double     ;
  begin
    mrgn    := FViewer.TwipsToPixels( GIS_SNAP_MARGIN ) / FViewer.Zoom ;
    Result  := False ;
    dst     := GIS_MAX_DOUBLE ;

    if assigned( FOnSnapPoint ) then begin
      {$IFDEF OXYGENE}
          args := TGIS_EditorSnapPointEventArgs.Create(
                    _ptg, mrgn, _ptg, dst
                  ) ;
          FOnSnapPoint( Self, args ) ;
          _ptg := args.Proj ;
      {$ELSE}
        FOnSnapPoint( Self, _ptg, mrgn, _ptg, dst ) ;
      {$ENDIF}
      Result := True ;
    end ;
  end ;

  procedure TGIS_EditorHelper.DoRedo ;
  begin
    if assigned( FDrawingTool ) then begin
      FDrawingTool.init ;
      if FEditor.PointCount > 0 then
        FDrawingTool.DoMouseDown(
          FViewer.MapToScreen(GisPoint2DFrom3D(FEditor.Point[FEditor.PointCount-1]))
        ) ;
    end ;
  end ;

  procedure TGIS_EditorHelper.DoUndo ;
  begin
    if assigned( FDrawingTool ) then begin
      FDrawingTool.init ;
      if FEditor.PointCount > 0 then
        FDrawingTool.DoMouseDown(
          FViewer.MapToScreen(GisPoint2DFrom3D(FEditor.Point[FEditor.PointCount-1]))
        ) ;
    end ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolCircle'}
  { TGIS_EditorToolCircle }

  procedure TGIS_EditorToolCircle.init ;
  begin
    FName := 'Circle tool' ;
    SetLength( FPoints, 2 ) ;
    FPointsCount := 0 ;
    askForNextStep ;
    inherited ;
  end ;

  procedure TGIS_EditorToolCircle.askForNextStep ;
  begin
    if FPointsCount = 0 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
    else if FPointsCount = 1 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_RADIUS ) )
  end ;

  procedure TGIS_EditorToolCircle.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount > 1 then exit ;

    FPoints[FPointsCount] := screenToMap(_pt) ;
    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolCircle.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[1] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolCircle.updateEditor(
    const _pt : TPoint
  ) ;
  var
    cnt      : Integer ;
    angle    : Double  ;
    arcangle : Double  ;
    delta    : Double  ;
    step     : Double  ;
    steps    : Integer ;
    ptg      : TGIS_Point3D {$IFDEF GIS_NORECORDS} = TGIS_Point3D.create {$ENDIF} ;
    stop,start,
    radius : Double ;

    function fmod( a, b : Double ) : Double ;
    var
     f : Integer ;
    begin
      f := TruncS( a/b ) ;
      Result := a - (b*f) ;
    end ;

  begin
    beginUpdatingEditor ;
    applyLockedParams ;
    start := 0 ;
    stop  := 2*Pi ;
    delta := (stop-start) ;

    if delta > 0 then
      arcangle := fmod( (stop-start) + 4*Pi, 2*Pi)
    else
      arcangle := fmod( (stop-start) - 4*Pi, 2*Pi) ;

    if arcangle = 0 then begin
      if stop <> start then
        arcangle := 2*Pi ;
    end ;

    steps := RoundS( Abs( arcangle ) / (2*Pi) * 64 ) ;
    steps := Max( 4, steps ) ;
    step  := arcangle / steps ;
    radius := TGIS_EditorUtils.Distance( FPoints[0], FPoints[1] ) ;
    angle := start ;

    for cnt := 0 to steps do begin
      ptg := GisPoint3DFrom2D( TGIS_EditorUtils.PolarPointByPtAngle( FPoints[0], angle, radius ) ) ;
      if cnt = 0 then begin
        Editor.AddPointEx( ptg ) ;
        Editor.DeletePoint(1) ;
      end
      else
        Editor.AddPoint( ptg ) ;

      angle := angle + step ;
    end ;
    LastPoint := FPoints[1] ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolCircle.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 2 then begin
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPointsCount := 0 ;
    end
    else
      FPoints[FPointsCount-1] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolCircle.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
  begin
    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen( TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolCircle.drawHelperLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareHelperPen ;

    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt1.X, pt1.Y ) ;
  end ;

  procedure TGIS_EditorToolCircle.drawCircle ;
  var
    pt0 : TPoint ;
  begin
    prepareMainPen ;
    pt0 := mapToScreen( FPoints[0] ) ;

    FRenderer.CanvasDrawEllipse( pt0.X-FDistanceI, pt0.Y-FDistanceI,
                                 2*FDistanceI, 2*FDistanceI
                               ) ;
  end ;

  procedure TGIS_EditorToolCircle.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    drawHelperLine ;
    drawDistanceText ;
    drawCircle ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolCircle3P'}
  { TGIS_EditorToolCircle3P }

  procedure TGIS_EditorToolCircle3P.init ;
  begin
    FName := 'Circle 3 points tool' ;
    SetLength( FPoints, 4 ) ;
    FPointsCount := 0 ;
    askForNextStep ;
    inherited ;
  end ;

  procedure TGIS_EditorToolCircle3P.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    FPoints[FPointsCount] := screenToMap(_pt) ;
    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolCircle3P.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[FPointsCount] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolCircle3P.updateEditor(
    const _pt : TPoint
  ) ;
  var
    cnt      : Integer ;
    angle    : Double  ;
    arcangle : Double  ;
    delta    : Double  ;
    step     : Double  ;
    steps    : Integer ;
    ptg      : TGIS_Point3D {$IFDEF GIS_NORECORDS} = TGIS_Point3D.create {$ENDIF} ;
    stop,start,
    radius : Double ;
    center : TGIS_Point ;

    function fmod( a, b : Double ) : Double ;
    var
     f : Integer ;
    begin
      f := TruncS( a/b ) ;
      Result := a - (b*f) ;
    end ;

  begin
    beginUpdatingEditor ;
    applyLockedParams ;

    TGIS_EditorUtils.CircleCenter( FPoints[0], FPoints[1], FPoints[2], center ) ;

    start := 0 ;
    stop := 2*Pi ;
    delta := (stop-start) ;

    if delta > 0 then
      arcangle := fmod( (stop-start) + 4*Pi, 2*Pi)
    else
      arcangle := fmod( (stop-start) - 4*Pi, 2*Pi) ;

    if arcangle = 0 then begin
      if stop <> start then
        arcangle := 2*Pi ;
    end ;

    steps := RoundS( Abs( arcangle ) / (2*Pi) * 64 ) ;
    steps := Max( 4, steps ) ;
    step  := arcangle / steps ;
    radius := TGIS_EditorUtils.Distance( center, FPoints[2] ) ;
    angle := start ;

    for cnt := 0 to steps do begin
      ptg := GisPoint3DFrom2D( TGIS_EditorUtils.PolarPointByPtAngle( center, angle, radius ) ) ;
      if cnt = 0 then begin
        Editor.AddPointEx( ptg ) ;
        Editor.DeletePoint(1) ;
      end
      else
        Editor.AddPoint( ptg ) ;

      angle := angle + step ;
    end ;
    LastPoint := FPoints[1] ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolCircle3P.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 3 then begin
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPointsCount := 0 ;
    end
    else
        FPoints[FPointsCount-1] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolCircle3P.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
  begin
    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen(TGIS_EditorUtils.MiddlePoint( FPoints[FPointsCount-1],
                                                       FPoints[FPointsCount] )                        ) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolCircle3P.drawHelperLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareHelperPen ;

    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt1.X, pt1.Y ) ;
  end ;

  procedure TGIS_EditorToolCircle3P.drawCircle3P ;
  var
    center : TGIS_Point ;
    pt0, pt1, pt2 : TPoint ;
    radiusi : Integer ;
  begin
    if TGIS_EditorUtils.CircleCenter( FPoints[0], FPoints[1], FPoints[2], center ) then begin
      prepareMainPen ;
      pt0 := mapToScreen( FPoints[0] ) ;
      pt2 := mapToScreen( FPoints[2] ) ;
      pt1 := mapToScreen( center ) ;
      radiusi := TGIS_EditorUtils.Distance( mapToScreen(center), mapToScreen(FPoints[2]) ) ;
      FRenderer.CanvasDrawEllipse( pt1.X-radiusi, pt1.Y-radiusi, 2*radiusi, 2*radiusi ) ;
      FRenderer.CanvasDrawLine( pt2.X, pt2.Y, pt1.X, pt1.Y ) ;
    end ;
  end ;

  procedure TGIS_EditorToolCircle3P.askForNextStep ;
  begin
    if FPointsCount = 0 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
    else if FPointsCount = 1 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_SECOND_POINT  ) )
    else
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_THIRD_POINT ) )
  end ;

  procedure TGIS_EditorToolCircle3P.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    drawHelperLine ;
    drawDistanceText ;
    if FPointsCount = 2 then
      drawCircle3P ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolCircle2P'}
  { TGIS_EditorToolCircle2P }

  procedure TGIS_EditorToolCircle2P.init ;
  begin
    FName := 'Circle tool' ;
    SetLength( FPoints, 3 ) ;
    FPointsCount := 0 ;
    askForNextStep ;
    inherited ;
  end ;

  procedure TGIS_EditorToolCircle2P.askForNextStep ;
  begin
    if FPointsCount = 0 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
    else if FPointsCount = 1 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_SECOND_POINT ) )
  end ;

  procedure TGIS_EditorToolCircle2P.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount > 1 then exit ;

    FPoints[FPointsCount] := screenToMap(_pt) ;
    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolCircle2P.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[1] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolCircle2P.updateEditor(
    const _pt : TPoint
  ) ;
  var
    cnt      : Integer ;
    angle    : Double  ;
    arcangle : Double  ;
    delta    : Double  ;
    step     : Double  ;
    steps    : Integer ;
    ptg      : TGIS_Point ;
    center   : TGIS_Point ;
    stop,start,
    radius : Double ;

    function fmod( a, b : Double ) : Double ;
    var
     f : Integer ;
    begin
      f := TruncS( a/b ) ;
      Result := a - (b*f) ;
    end ;

  begin
    beginUpdatingEditor ;
    applyLockedParams ;
    start := 0 ;
    stop  := 2*Pi ;
    delta := (stop-start) ;

    if delta > 0 then
      arcangle := fmod( (stop-start) + 4*Pi, 2*Pi)
    else
      arcangle := fmod( (stop-start) - 4*Pi, 2*Pi) ;

    if arcangle = 0 then begin
      if stop <> start then
        arcangle := 2*Pi ;
    end ;

    steps := RoundS( Abs( arcangle ) / (2*Pi) * 64 ) ;
    steps := Max( 4, steps ) ;
    step  := arcangle / steps ;
    angle := start ;

    radius := TGIS_EditorUtils.Distance( FPoints[0], FPoints[1] ) / 2 ;
    center := TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ;

    for cnt := 0 to steps do begin
      ptg := TGIS_EditorUtils.PolarPointByPtAngle( center, angle, radius ) ;
      if cnt = 0 then begin
        Editor.AddPointEx( GisPoint3DFrom2D( ptg ) ) ;
        Editor.DeletePoint(1) ;
        Editor.DeletePoint(1) ;
      end
      else
        Editor.AddPoint( GisPoint3DFrom2D( ptg ) ) ;

      angle := angle + step ;
    end ;
    LastPoint := ptg ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolCircle2P.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 2 then begin
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPointsCount := 0 ;
    end
    else if FPointsCount > 0 then
      FPoints[FPointsCount-1] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolCircle2P.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
  begin
    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen( TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolCircle2P.drawHelperLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareHelperPen ;

    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt1.X, pt1.Y ) ;
  end ;

  procedure TGIS_EditorToolCircle2P.drawCircle2P ;
  var
    pt0     : TPoint ;
    pt1     : TPoint ;
    radius  : Integer ;
    dist    : Integer ;
    center  : TPoint ;
  begin
    prepareMainPen ;
    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    dist := TGIS_EditorUtils.Distance( pt0, pt1 ) ;
    radius := RoundS( dist / 2 ) ;
    center := mapToScreen( TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ) ;
    FRenderer.CanvasDrawEllipse( center.X - radius, center.Y - radius, dist, dist ) ;
  end ;

  procedure TGIS_EditorToolCircle2P.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    drawHelperLine ;
    drawDistanceText ;
    drawCircle2P ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolLine'}
  procedure TGIS_EditorToolLine.init ;
  begin
    FName := 'Line tool' ;
    SetLength( FPoints, 2 ) ;
    FPointsCount := 0 ;
    askForNextStep ;
    inherited ;
  end ;

  procedure TGIS_EditorToolLine.askForNextStep ;
  begin
    if FPointsCount = 0 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
    else if FPointsCount = 1 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_NEXT_POINT ) )
  end ;

  procedure TGIS_EditorToolLine.drawDimensionLine ;
  var
    p1    : TGIS_Point ;
    p2    : TGIS_Point ;
    angle : Double ;
    pt1,
    pt2,
    pt    : TPoint ;
    dst   : Double ;
    pdst  : Integer ;
  begin
    pdst := TGIS_EditorUtils.Distance( mapToScreen(FPoints[0]), mapToScreen(FPoints[1]) ) ;
    if pdst < twipsToPixels( Editor.MinMove ) then exit ;

    prepareHelperPen ;
    if (FPoints[0].Y < FPoints[1].Y) then
      angle := TGIS_EditorUtils.AngleBy2Points( FPoints[0], FPoints[1] ) + Pi / 2
    else
      angle := TGIS_EditorUtils.AngleBy2Points( FPoints[0], FPoints[1] ) - Pi / 2 ;

    pt := mapToScreen( FPoints[0] ) ;
    dst := TGIS_EditorUtils.Distance( FPoints[0], screenToMap( Point( pt.X + 50, pt.Y ) ) ) ;
    p1 := TGIS_EditorUtils.PolarPointByPtAngle( FPoints[0], angle, dst ) ;
    p2 := TGIS_EditorUtils.PolarPointByPtAngle( FPoints[1], angle, dst ) ;

    pt1 := mapToScreen( p1 ) ;
    pt2 := mapToScreen( p2 ) ;
    FRenderer.CanvasDrawLine( pt.X, pt.Y, pt1.X, pt1.Y ) ;
    pt := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt.X, pt.Y, pt2.X, pt2.Y ) ;
    FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
    middleDistance := TGIS_EditorUtils.MiddlePoint( p1, p2 ) ;
  end ;

  procedure TGIS_EditorToolLine.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
    pdst  : Integer ;
  begin
    pdst := TGIS_EditorUtils.Distance( mapToScreen(FPoints[0]), mapToScreen(FPoints[1]) ) ;
    if pdst < twipsToPixels( Editor.MinMove ) then exit ;

    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen(middleDistance) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolLine.drawAngleText ;
  var
    size  : TPoint ;
    cpt   : TGIS_Point ;
    rct   : TRect ;
    str   : String ;
    pdst  : Integer ;
  begin
    pdst := TGIS_EditorUtils.Distance( mapToScreen(FPoints[0]), mapToScreen(FPoints[1]) ) ;
    if pdst < twipsToPixels( Editor.MinMove ) then exit ;

    str   := Format( '%.1f'+#176, [Abs(FAngle)] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    if not LockedParams.RelativeAngle then begin
      cpt   := TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ;
      cpt.X := cpt.X + FDistance / 2 ;
    end
    else begin
      if GisIsSameValue(LastPoint.X, GIS_MAX_DOUBLE) then exit ;
      cpt := TGIS_EditorUtils.Centroid( LastPoint, FPoints[0], FPoints[1] ) ;
    end ;
    rct := TGIS_EditorUtils.InflateRect( mapToScreen(cpt), size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolLine.drawHelperLine ;
  var
    p0, p1 : TPoint ;
    pdst  : Integer ;
  begin
    pdst := TGIS_EditorUtils.Distance( mapToScreen(FPoints[0]), mapToScreen(FPoints[1]) ) ;
    if pdst < twipsToPixels( Editor.MinMove ) then exit ;

    prepareHelperPen ;
    p0 := mapToScreen( FPoints[0] ) ;
    p1 := mapToScreen( GisPoint( FPoints[0].X + FDistance, FPoints[0].Y ) ) ;
    FRenderer.CanvasDrawLine( p0.X, p0.Y, p1.X, p1.Y ) ;
  end ;

  procedure TGIS_EditorToolLine.drawLine ;
  var
    p0, p1 : TPoint ;
  begin
    prepareMainPen ;
    if not LockedParams.RelativeAngle or not isAngleLocked then begin
      p0 := mapToScreen( FPoints[0] ) ;
      p1 := mapToScreen( FPoints[1] ) ;

      FRenderer.CanvasDrawLine( p0.X, p0.Y, p1.X, p1.Y )
    end
    else begin
      p0 := mapToScreen( FPoints[0] ) ;
      p1 := mapToScreen( FPoints[1] ) ;

      if GisIsSameValue(LastPoint.X, GIS_MAX_DOUBLE) then exit ;

      if TGIS_EditorUtils.LeftOfLine( FPoints[1], LastPoint, FPoints[0] ) < 0 then
        FPoints[1] := TGIS_EditorUtils.AnglePointFromLine(
                        LastPoint, FPoints[0], FPoints[1], DegToRad(FAngle)
                      )
      else
        FPoints[1] := TGIS_EditorUtils.AnglePointFromLine(
                        LastPoint, FPoints[0], FPoints[1], -DegToRad(FAngle+180)
                      ) ;
      p0 := mapToScreen( FPoints[0] ) ;
      p1 := mapToScreen( FPoints[1] ) ;
      FRenderer.CanvasDrawLine( p0.X, p0.Y, p1.X, p1.Y )
    end ;
  end ;

  procedure TGIS_EditorToolLine.drawArc ;
  var
    ddist : Integer ;
    p0, p1, p2 : TPoint ;
    pt0, pt1, p : TPoint ;
    sangle, eangle, tmp : Double ;
  begin
    prepareHelperPen ;
    ddist := 2 * FDistanceI ;

    if not LockedParams.RelativeAngle then begin
      pt0 := mapToScreen( FPoints[0] ) ;
      pt1 := mapToScreen( FPoints[1] ) ;
      if pt0.Y > pt1.Y then begin
        p0 := mapToScreen( FPoints[0] ) ;
        p1 := mapToScreen( FPoints[0] ) ;
        p2 := mapToScreen( FPoints[1] ) ;

        if ddist > 0 then
          FRenderer.CanvasDrawArc(
            p0.X-FDistanceI, p0.Y-FDistanceI, ddist, ddist,
            p1.X+FDistanceI, p1.Y, p2.X, p2.Y
          )
      end
      else  begin
        p0 := mapToScreen( FPoints[0] ) ;
        p1 := mapToScreen( FPoints[1] ) ;
        p2 := mapToScreen( FPoints[0] ) ;

        if ddist > 0 then
          FRenderer.CanvasDrawArc(
            p0.X-FDistanceI , p0.Y-FDistanceI , ddist, ddist,
            p1.X, p1.Y, p2.X+FDistanceI , p2.Y
          ) ;
      end ;
    end
    else begin
      sangle := -ArcTan2(FPoints[1].Y-FPoints[0].Y, FPoints[1].X-FPoints[0].X) ;
      eangle := -ArcTan2(LastPoint.Y-FPoints[0].Y, LastPoint.X-FPoints[0].X) ;

      pt0 := mapToScreen( FPoints[0] ) ;
      pt1 := mapToScreen( FPoints[1] ) ;
      if pt1.Y <= pt0.Y then begin
        tmp := sangle ;
        sangle  := eangle ;
        eangle  := tmp ;
      end ;

      if sangle < 0 then
        sangle := sangle + 2*Pi ;
      if eangle < 0 then
        eangle := eangle + 2*Pi ;

      p := mapToScreen( FPoints[0] ) ;
      FRenderer.CanvasDrawArc(
        p.X, p.Y, FDistanceI, -RadToDeg(sangle), -RadToDeg(eangle-sangle)
      ) ;
    end ;
  end ;

  procedure TGIS_EditorToolLine.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount > 1 then exit ;

    FPoints[FPointsCount] := screenToMap(_pt) ;
    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolLine.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[1] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolLine.updateEditor(
    const _pt : TPoint
  ) ;
  var
    ptg1, ptg2 : TGIS_Point3D ;
    pcnt : Integer ;
  begin
    beginUpdatingEditor ;
    applyLockedParams ;

    if LockedParams.RelativeAngle then
      FPoints[FPointsCount] := TGIS_EditorUtils.AnglePointFromLine(
                                  lp, FPoints[0], FPoints[1], DegToRad(FAngle)
                               ) ;
    ptg1 := GisPoint3DFrom2D( FPoints[0] ) ;
    ptg2 := GisPoint3DFrom2D( FPoints[1] ) ;

    pcnt := FEditor.PointCount ;
    Editor.InsertPointEx( FEditor.PointPos+1, ptg2 ) ;

    if pcnt < FEditor.PointCount then
      LastPoint := FPoints[0]
    else begin
      resetToolPoints ;
      LastPoint := FPoints[0] ;
      FPoints[FPointsCount] := LastPoint ;
    end ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolLine.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 2 then begin
      lp := LastPoint ;
      LastPoint := FPoints[0] ;
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPoints[0] := FPoints[FPointsCount] ;
    end ;
  end ;

  procedure TGIS_EditorToolLine.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    drawLine ;
    drawHelperLine ;
    drawDimensionLine ;
    drawDistanceText ;
    drawArc ;
    drawAngleText ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolRectangle'}
  { TGIS_EditorToolRectangle }

  procedure TGIS_EditorToolRectangle.init ;
  begin
    FName := 'Rectangle tool' ;
    SetLength( FPoints, 2 ) ;
    FPointsCount := 0 ;
    askForNextStep ;
    inherited ;
  end ;

  procedure TGIS_EditorToolRectangle.askForNextStep ;
  begin
    if FPointsCount = 0 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_CORNER ) )
    else if FPointsCount = 1 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_NEXT_CORNER ) )
  end ;

  procedure TGIS_EditorToolRectangle.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount > 1 then exit ;

    FPoints[FPointsCount] := screenToMap(_pt) ;
    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolRectangle.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[1] := screenToMap( _pt ) ;
  end ;

  procedure TGIS_EditorToolRectangle.updateEditor(
    const _pt : TPoint
  ) ;
  var
    ptg1, ptg2 : TGIS_Point3D ;
  begin
    beginUpdatingEditor ;
    applyLockedParams ;

    ptg1 := GisPoint3DFrom2D( FPoints[0] ) ;
    ptg2 := GisPoint3DFrom2D( FPoints[1] ) ;

    Editor.AddPointEx( GisPoint3D( ptg2.X, ptg1.Y, 0 ) ) ;
    Editor.AddPoint  ( ptg2 ) ;
    Editor.AddPoint  ( GisPoint3D( ptg1.X, ptg2.Y, 0 ) ) ;
    Editor.AddPoint  ( ptg1 ) ;
    LastPoint := FPoints[1] ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolRectangle.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 2 then begin
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPointsCount := 0 ;
    end ;
  end ;

  procedure TGIS_EditorToolRectangle.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect  ;
    str   : String ;
  begin
    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen( TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolRectangle.drawAngleText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    pt    : TPoint ;
    rct   : TRect ;
    str   : String ;
    offx  : Integer ;
    offy  : Integer ;
  begin
    str   := Format( '%.1f'+#176, [Abs(FAngle)] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    offx  := twipsToPixels(30) + size.X ;
    offy  := twipsToPixels(10) ;

    if FPoints[0].X > FPoints[1].X then
      offx := offx * -1 ;
    if FPoints[1].Y > FPoints[0].Y then
      offy := offy + size.Y ;

    pt := mapToScreen( FPoints[0] ) ;
    cpt   := Point( pt.X + offx, pt.Y + offy ) ;
    cpt.X := cpt.X ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolRectangle.drawHelperLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareHelperPen ;
    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt1.X, pt1.Y ) ;
  end ;

  procedure TGIS_EditorToolRectangle.drawRectangle ;
  begin
    prepareMainPen ;
    FRenderer.CanvasDrawRectangle(
      rectFToRect( RectF( FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y ) )
    ) ;
  end ;

  procedure TGIS_EditorToolRectangle.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    drawHelperLine ;
    drawDistanceText ;
    drawRectangle ;
    drawAngleText ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolRectangle90'}
  { TGIS_EditorToolRectangle90 }

  procedure TGIS_EditorToolRectangle90.init ;
  begin
    FName := 'Rectangle 90 tool' ;
    SetLength( FPoints, 4 ) ;
    SetLength( points, 4 ) ;

    FPointsCount := 0 ;
    askForNextStep ;
    inherited ;
  end ;

  procedure TGIS_EditorToolRectangle90.askForNextStep ;
  begin
    if FPointsCount = 0 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT  ) )
    else if FPointsCount = 1 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_SECOND_POINT ) )
    else if FPointsCount = 2 then
      notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_DISTANCE_POINT ) )
  end ;

  procedure TGIS_EditorToolRectangle90.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount < 2 then
      FPoints[FPointsCount] := screenToMap(_pt) ;

    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolRectangle90.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[FPointsCount] := screenToMap( _pt ) ;
  end ;

  procedure TGIS_EditorToolRectangle90.updateEditor(
    const _pt : TPoint
  ) ;
  var
    ptg0, ptg1,
    ptg2, ptg3 : TGIS_Point3D ;
  begin
    beginUpdatingEditor ;
    applyLockedParams ;

    FPoints[2] := TGIS_EditorUtils.PerpendicularPointFromLine(
                    FPoints[0], FPoints[1], FPoints[2]
                  ) ;
    FPoints[3] := TGIS_EditorUtils.OffsetPoint(
                    FPoints[2], FPoints[1].X - FPoints[0].X, FPoints[1].Y - FPoints[0].Y
                  ) ;
    ptg0 := GisPoint3DFrom2D( FPoints[0] ) ;
    ptg1 := GisPoint3DFrom2D( FPoints[1] ) ;
    ptg2 := GisPoint3DFrom2D( FPoints[2] ) ;
    ptg3 := GisPoint3DFrom2D( FPoints[3] ) ;

    Editor.AddPointEx( ptg1 ) ;
    Editor.AddPoint  ( ptg2 ) ;
    Editor.AddPoint  ( ptg3 ) ;
    Editor.AddPoint  ( ptg0 ) ;
    LastPoint := FPoints[3] ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolRectangle90.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 2 then begin
      FPoints[FPointsCount-1] := screenToMap(_pt) ;
    end
    else if FPointsCount= 3 then begin
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPointsCount := 0 ;
    end
  end ;

  procedure TGIS_EditorToolRectangle90.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
  begin
    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen(TGIS_EditorUtils.MiddlePoint( FPoints[FPointsCount-1], FPoints[FPointsCount] )) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolRectangle90.drawAngleText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
  begin
    str   := Format( '%.1f'+#176, [Abs(FAngle)] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen(TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] )) ;
    cpt.X := cpt.X + FDistanceI div 2 ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolRectangle90.drawHelperLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareHelperPen ;
    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt0.X + FDistanceI, pt0.Y ) ;
  end ;

  procedure TGIS_EditorToolRectangle90.drawLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareMainPen ;
    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt1.X, pt1.Y ) ;
  end ;

  procedure TGIS_EditorToolRectangle90.drawRectangle90 ;
  var
    i : Integer ;
  begin
    prepareMainPen ;
    FPoints[2] := TGIS_EditorUtils.PerpendicularPointFromLine(
                    FPoints[0], FPoints[1], FPoints[2]
                  ) ;
    FPoints[3] := TGIS_EditorUtils.OffsetPoint(
                    FPoints[2], FPoints[1].X - FPoints[0].X, FPoints[1].Y - FPoints[0].Y
                  ) ;
    for i := 0 to 3 do
      points[i] := mapToScreen( FPoints[i] ) ;

    FRenderer.CanvasDrawPolygon( points ) ;
  end ;

  procedure TGIS_EditorToolRectangle90.drawArc ;
  var
    ddist : Integer ;
    pt0, pt1 : TPoint ;
    p0, p1, p2 : TPoint ;
  begin
    prepareHelperPen ;
    ddist := 2 * FDistanceI ;

    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    if pt0.Y > pt1.Y then begin
      p0 := mapToScreen( FPoints[0] ) ;
      p1 := mapToScreen( FPoints[0] ) ;
      p2 := mapToScreen( FPoints[1] ) ;

      if ddist > 0 then
        FRenderer.CanvasDrawArc(
          p0.X-FDistanceI, p0.Y-FDistanceI, ddist, ddist,
          p1.X+FDistanceI, p1.Y, p2.X, p2.Y
        )
    end
    else  begin
      p0 := mapToScreen( FPoints[0] ) ;
      p1 := mapToScreen( FPoints[1] ) ;
      p2 := mapToScreen( FPoints[0] ) ;

      if ddist > 0 then
        FRenderer.CanvasDrawArc(
          p0.X-FDistanceI , p0.Y-FDistanceI , ddist, ddist,
          p1.X, p1.Y, p2.X+FDistanceI , p2.Y
        ) ;
    end ;
  end ;

  procedure TGIS_EditorToolRectangle90.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    if FPointsCount = 1 then begin
      drawLine ;
      drawDistanceText ;
      drawHelperLine ;
      drawArc ;
      drawAngleText ;
    end
    else begin
      drawDistanceText ;
      drawRectangle90 ;
    end ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorShapeArc'}
type
  TGIS_ShapeArcOptions = record
    ToleranceOfApproximation : Double ;
    MinimumSegments          : Integer ;
  end ;

  TGIS_EditorShapeArc = record
    public
      Radius      : Double ;
      Center      : TGIS_Point ;
      StartAngle  : Double ;
      EndAngle    : Double ;
      Reversed    : Boolean ;
      Options     : TGIS_ShapeArcOptions ;
    private
      function fget_TotalAngle  : Double ;
      function fget_Length      : Double ;
      function fget_Area        : Double ;
    public
      procedure InverseAngle ;
      procedure InitVariables ( const _options : TGIS_ShapeArcOptions
                              ) ;
      procedure SetArc        ( const _radius     : Double ;
                                const _center     : TGIS_Point ;
                                const _startAngle : Double ;
                                const _endAngle   : Double ;
                                const _reversed   : Boolean
                              ) ;
      function GetStartPoint  ( const _useReverseFlag : Boolean = True
                              ) : TGIS_Point ;
      function GetEndPoint    ( const _useReverseFlag : Boolean = True
                              ) : TGIS_Point ;
      function GetMiddlePoint ( const _useReverseFlag : Boolean = True
                              ) : TGIS_Point ;
      function AsPolyline     ( const _segments : Integer = 90
                              ) : TArray<TGIS_Point> ;
      function FromPolyline   ( const _points         : TArray<TGIS_Point> ;
                                const startVertex     : Integer = 0 ;
                                const atLeastNSegment : Integer = 0
                              ) : Boolean ;

      function BuildFromStartSecondEndPts    ( const _points : TArray<TGIS_Point>
                                             ) : Boolean ;
      function BuildFromStartCenterEndPts    ( const _start  : TGIS_Point ;
                                               const _center : TGIS_Point ;
                                               const _end    : TGIS_Point
                                             ) : Boolean ;
      function BuildFromStartCenterPtsAngle  ( const _start  : TGIS_Point ;
                                               const _center : TGIS_Point ;
                                               const _angle  : Double
                                             ) : Boolean ;
      function BuildFromStartCenterPtsLength ( const _start  : TGIS_Point ;
                                               const _center : TGIS_Point ;
                                               const _length : Double
                                             ) : Boolean ;
      function BuildFromStartCenterPtsChord  ( const _start  : TGIS_Point ;
                                               const _center : TGIS_Point ;
                                               const _chord  : Double
                                             ) : Boolean ;
      function BuildFromStartEndPtsAngle     ( const _start  : TGIS_Point ;
                                               const _end    : TGIS_Point ;
                                               const _angle  : Double
                                             ) : Boolean ;
      function BuildFromStartEndPtsRadius   ( const _start   : TGIS_Point ;
                                              const _end     : TGIS_Point ;
                                              const _radius  : Double
                                            ) : Boolean ;
    public
      property TotalAngle : Double read fget_TotalAngle ;
      property Length     : Double read fget_Length ;
      property Area       : Double read fget_Area ;
  end ;

  { TGIS_EditorShapeArc }

  procedure TGIS_EditorShapeArc.SetArc(
    const _radius     : Double ;
    const _center     : TGIS_Point ;
    const _startAngle : Double ;
    const _endAngle   : Double ;
    const _reversed   : Boolean
  ) ;
  begin
    Radius     := _radius ;
    Center     := _center ;
    Reversed   := _reversed ;
    StartAngle := TGIS_EditorUtils.NormalizeAngle( _startAngle ) ;
    EndAngle   := TGIS_EditorUtils.NormalizeAngle( _endAngle ) ;
  end ;

  function TGIS_EditorShapeArc.AsPolyline(
    const _segments : Integer = 90
  ) : TArray<TGIS_Point> ;
  var
    segmentLen,
    dummy           : Double ;
    tolerance       : Double ;
    _atLeastNSegment,
    segmentTotal    : Integer ;
    points          : TList<TGIS_Point> ;
    pt              : TGIS_Point ;
    i               : Integer ;
    angle           : Double ;
    offsetAngle     : Double ;
  begin
    Result := nil ;
    tolerance := IfThen( Options.ToleranceOfApproximation > 0,
                         Options.ToleranceOfApproximation, 0.1
                       ) ;
    _atLeastNSegment := IfThen( _segments > 0, _segments, Options.MinimumSegments ) ;
    dummy := Radius - tolerance ;
    if dummy <= 0 then
       segmentLen := Radius
    else begin
       dummy := (Radius * Radius) - (dummy * dummy) ;
       segmentLen := Sqrt(dummy) * 2 ;
    end ;

    if segmentLen = 0 then exit ;

    segmentTotal := CeilS(Length / segmentLen) ;
    if segmentTotal < _atLeastNSegment then
       segmentTotal := _atLeastNSegment
    else if segmentTotal > _segments then
       segmentTotal := _segments ;

    points := TList<TGIS_Point>.Create ;
    if Reversed then begin
      pt := TGIS_EditorUtils.PolarPointByPtAngle(Center, EndAngle, Radius) ;
      points.Add(pt) ;

      i := 1 ;
      angle := EndAngle ;
      offsetAngle := TotalAngle / segmentTotal ;
      while i < segmentTotal do begin
        angle := angle - offsetAngle ;
        pt := TGIS_EditorUtils.PolarPointByPtAngle(Center, angle, Radius) ;
        points.Add(pt) ;
        i := i + 1
      end ;
      pt := TGIS_EditorUtils.PolarPointByPtAngle(Center, StartAngle, Radius) ;
    end
    else begin
      pt := TGIS_EditorUtils.PolarPointByPtAngle(Center, StartAngle, Radius) ;
      points.Add(pt) ;

      i := 1 ;
      angle := StartAngle ;
      offsetAngle := TotalAngle / segmentTotal  ;
      while i < segmentTotal do begin
        angle := angle + offsetAngle ;
        pt := TGIS_EditorUtils.PolarPointByPtAngle(Center, angle, Radius) ;
        points.Add(pt) ;
        i := i + 1 ;
      end ;
      pt := TGIS_EditorUtils.PolarPointByPtAngle(Center, EndAngle, Radius) ;
    end ;
    points.Add(pt) ;

    Result := points.ToArray ;
    FreeObject( points ) ;
  end ;

  function TGIS_EditorShapeArc.FromPolyline(
    const _points         : TArray<TGIS_Point> ;
    const startVertex     : Integer = 0 ;
    const atLeastNSegment : Integer = 0
  ) : Boolean ;
  var
    i                 : Integer ;
    _atLeastNSegment  : Integer ;
    nSegment          : Integer ;
    totPoints         : Integer ;
    dx, dy            : Double ;
    myPoints          : TList<TGIS_Point> ;
    iLinePerpOnMid1,
    iLinePerpOnMid2   : TArray<TGIS_Point> ;
    center_ptg        : TGIS_Point ;
    hasCenter         : Boolean ;
    epsilon           : Double ;
    dradius           : Double ;
    tolerance         : Double ;
    startClockWise    : Boolean ;
    clockWise         : Boolean ;
    angle             : Double ;
  begin
    Result := False ;
    if _points = nil then exit ;

    _atLeastNSegment := IfThen( atLeastNSegment > 0,
                                atLeastNSegment, Options.MinimumSegments ) ;
    totPoints := {$IFDEF DCC}System.{$ELSE}RemObjects.Elements.System.{$ENDIF}length(_points) - startVertex ;
    nSegment  := totPoints - 1 ;
    if (nSegment < _atLeastNSegment) or (totPoints < 3) then exit ;

    dx := _points[startVertex].X ;
    dy := _points[startVertex].Y ;

    myPoints := TList<TGIS_Point>.Create ;
    try
      myPoints.Add( TGIS_EditorUtils.MovePoint(_points[startVertex    ], -dx, -dy) ) ;
      myPoints.Add( TGIS_EditorUtils.MovePoint(_points[startVertex + 1], -dx, -dy) ) ;
      myPoints.Add( TGIS_EditorUtils.MovePoint(_points[startVertex + 2], -dx, -dy) ) ;

      iLinePerpOnMid1 := TGIS_EditorUtils.InfinityLinePerpOnMiddle(myPoints[0], myPoints[1]) ;
      if {$IFDEF DCC}System.{$ELSE}RemObjects.Elements.System.{$ENDIF}length( iLinePerpOnMid1 ) = 0 then exit ;
      iLinePerpOnMid2 := TGIS_EditorUtils.InfinityLinePerpOnMiddle(myPoints[1], myPoints[2]) ;
      if {$IFDEF DCC}System.{$ELSE}RemObjects.Elements.System.{$ENDIF}length(iLinePerpOnMid2) = 0 then exit ;

      hasCenter := TGIS_EditorUtils.CrossLines(
        iLinePerpOnMid1[0],
        iLinePerpOnMid1[1],
        iLinePerpOnMid2[0],
        iLinePerpOnMid2[1],
        center_ptg
      ) ;
      if not hasCenter then exit ;

      epsilon := 1e-4  ;
      dradius := TGIS_EditorUtils.Distance(center_ptg, myPoints[0])  ;
      tolerance := dradius * epsilon ;
      startClockWise := False ;
      if TGIS_EditorUtils.LeftOfLine(myPoints[1], myPoints[0], myPoints[2]) < 0 then
        startClockWise := true ;
      angle := TGIS_EditorUtils.AngleBy3Points(myPoints[0], center_ptg, myPoints[2], startClockWise) ;

      i := 3 ;
      while i < totPoints do begin
         myPoints.Add(TGIS_EditorUtils.MovePoint(_points[i + startVertex], -dx, -dy)) ;
         if not TGIS_EditorUtils.DoubleNear(
            dradius, TGIS_EditorUtils.Distance(center_ptg, myPoints[i]), tolerance
         ) then break  ;

         clockWise := False ;
         if TGIS_EditorUtils.LeftOfLine(myPoints[i - 1], myPoints[i - 2], myPoints[i]) < 0 then
           clockWise := True ;
         if startClockWise <> clockWise then break ;
         angle := angle + TGIS_EditorUtils.AngleBy3Points(
                            myPoints[i - 1], center_ptg, myPoints[i], startClockWise
                          ) ;
         if angle >= 2 * Pi then break ;
         i := i + 1 ;
      end ;

      i := i - 1 ;
      if i < _atLeastNSegment then exit ;

      Center := center_ptg ;
      Radius := dradius ;

      if startClockWise then begin
         EndAngle   := TGIS_EditorUtils.AngleBy2Points(Center, myPoints[0]) ;
         StartAngle := TGIS_EditorUtils.AngleBy2Points(Center, myPoints[i]) ;
         Reversed   := True ;
      end
      else begin
         StartAngle := TGIS_EditorUtils.AngleBy2Points(Center, myPoints[0]) ;
         EndAngle   := TGIS_EditorUtils.AngleBy2Points(Center, myPoints[i]) ;
         Reversed   := False ;
      end ;
      Center := TGIS_EditorUtils.MovePoint(Center, dx, dy) ;
      Result := i + startVertex > 0 ;
    finally
      FreeObject( myPoints ) ;
    end ;
  end ;

  function TGIS_EditorShapeArc.fget_Area : Double ;
  var
    alpha0, alpha1, alpha2 : Double ;
    delta_alpha01, delta_alpha12 : Double ;
  begin
    if TGIS_EditorUtils.DoubleNear( TotalAngle, 2*Pi, 0.001 ) then begin
      Result := Pi * Radius * Radius ;
      exit ;
    end ;

    alpha0 := TGIS_EditorUtils.AngleBy2Points( Center, GetStartPoint  ) ;
    alpha1 := TGIS_EditorUtils.AngleBy2Points( Center, GetMiddlePoint ) ;
    alpha2 := TGIS_EditorUtils.AngleBy2Points( Center, GetEndPoint    ) ;

    delta_alpha01 := alpha1 - alpha0 ;
    delta_alpha12 := alpha2 - alpha1 ;
    // http://en.wikipedia.org/wiki/Circular_segment
    Result := 0.5 * Radius * Radius *
              Abs(delta_alpha01 - Sin(delta_alpha01) + delta_alpha12 - Sin(delta_alpha12)) ;
  end ;

  function TGIS_EditorShapeArc.fget_Length : Double ;
  begin
    Result := Radius * TotalAngle ;
  end ;

  function TGIS_EditorShapeArc.fget_TotalAngle : Double ;
  begin
    if StartAngle < EndAngle then
      Result := EndAngle - StartAngle
    else
      Result := (2 * Pi - StartAngle) + EndAngle
  end ;

  procedure TGIS_EditorShapeArc.InitVariables(
    const _options : TGIS_ShapeArcOptions
  ) ;
  begin
    Options.ToleranceOfApproximation := _options.ToleranceOfApproximation ;
    Options.MinimumSegments          := _options.MinimumSegments ;
  end ;

  procedure TGIS_EditorShapeArc.InverseAngle ;
  var
    tmp : Double ;
  begin
    tmp := StartAngle ;
    StartAngle := EndAngle ;
    EndAngle := tmp ;
  end ;

  function TGIS_EditorShapeArc.GetStartPoint(
    const _useReverseFlag : Boolean
  ) : TGIS_Point ;
  begin
    if _useReverseFlag then
      Result := TGIS_EditorUtils.PolarPointByPtAngle(Center, IfThen( Reversed, EndAngle, StartAngle), Radius)
    else
      Result := TGIS_EditorUtils.PolarPointByPtAngle(Center, StartAngle, Radius)
  end ;

  function TGIS_EditorShapeArc.GetEndPoint(
    const _useReverseFlag : Boolean
  ) : TGIS_Point ;
  begin
    if _useReverseFlag then
      Result := TGIS_EditorUtils.PolarPointByPtAngle(Center, IfThen( Reversed, StartAngle, EndAngle ), Radius)
    else
      Result := TGIS_EditorUtils.PolarPointByPtAngle(Center, EndAngle, Radius)
  end ;

  function TGIS_EditorShapeArc.GetMiddlePoint(
    const _useReverseFlag : Boolean
  ) : TGIS_Point ;
  begin
    Result := TGIS_EditorUtils.PolarPointByPtAngle(Center, (StartAngle + EndAngle) / 2, Radius) ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartCenterPtsAngle(
    const _start  : TGIS_Point ;
    const _center : TGIS_Point ;
    const _angle  : Double
  ) : Boolean ;
  begin
    if GisIsSamePoint( _start,  _center ) or (_angle = 0) then begin
      Result := False ;
      exit ;
    end ;

    Center      := _center ;
    Radius      := TGIS_EditorUtils.Distance(_center, _start) ;
    StartAngle  := TGIS_EditorUtils.AngleBy2Points(_center, _start) ;
    EndAngle    := StartAngle + _angle ;
    if EndAngle > Pi * 2 then
      EndAngle := TGIS_EditorUtils.FMod(EndAngle, Pi * 2 ) ;
    Reversed    := False ;
    Result := True ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartCenterEndPts(
    const _start  : TGIS_Point ;
    const _center : TGIS_Point ;
    const _end    : TGIS_Point
  ) : Boolean ;
  begin
    if GisIsSamePoint( _start,  _center ) or
       GisIsSamePoint( _start,  _end    ) or
       GisIsSamePoint( _center, _end    ) then begin
      Result := False ;
      exit ;
    end ;

    Center      := _center ;
    Radius      := TGIS_EditorUtils.Distance(_center, _start) ;
    StartAngle  := TGIS_EditorUtils.AngleBy2Points(_center, _start) ;
    EndAngle    := TGIS_EditorUtils.AngleBy2Points(_center, _end) ;
    Reversed    := False ;

    Result := True ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartCenterPtsLength(
    const _start  : TGIS_Point ;
    const _center : TGIS_Point ;
    const _length : Double
  ) : Boolean ;
  var
    circumference, angle : Double ;
  begin
    if GisIsSamePoint( _start,  _center ) or (_length = 0) then begin
      Result := False ;
      exit ;
    end ;

    Center := _center ;
    Radius := TGIS_EditorUtils.Distance(_center, _start) ;
    circumference := 2 * Pi * Radius ;
    if Length >= circumference then begin
      Result := False ;
      exit ;
    end ;
    StartAngle := TGIS_EditorUtils.AngleBy2Points(_center, _start) ;
    angle := Pi*2 * Length / circumference ;
    EndAngle := StartAngle + angle ;
    Reversed := False ;
    Result := True ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartCenterPtsChord(
    const _start  : TGIS_Point ;
    const _center : TGIS_Point ;
    const _chord  : Double
  ) : Boolean ;
  var
    angle : Double ;
  begin
    if GisIsSamePoint( _start,  _center ) or (_chord = 0) then begin
      Result := False ;
      exit ;
    end ;

    Center := _center ;
    Radius := TGIS_EditorUtils.Distance(_center, _start) ;

    if _chord > 2 * Radius then begin
      Result := False ;
      exit ;
    end ;
    StartAngle := TGIS_EditorUtils.AngleBy2Points(_center, _start) ;
    angle := 2 * ArcSin(_chord / (2 * Radius)) ;
    EndAngle := StartAngle + angle ;
    Reversed := False ;
    Result := True ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartEndPtsAngle(
    const _start  : TGIS_Point ;
    const _end    : TGIS_Point ;
    const _angle  : Double
  ) : Boolean ;
  var
    chord,
    half_chord,
    angleSegment,
    distFromCenter : Double ;
    ptMiddle       : TGIS_Point ;
  begin
    if GisIsSamePoint( _start,  _end ) or (_angle = 0) then begin
      Result := False ;
      exit ;
    end ;

    chord := TGIS_EditorUtils.Distance(_start, _end) ;
    half_chord := chord / 2  ;
    Radius := half_chord / Sin(_angle / 2) ;

    angleSegment := TGIS_EditorUtils.AngleBy2Points(_start, _end) ;
    ptMiddle := TGIS_EditorUtils.MiddlePoint(_start, _end) ;

    distFromCenter := Sqrt((self.Radius * self.Radius) - (half_chord * half_chord)) ;
    if _angle < Pi then
      Center := TGIS_EditorUtils.PolarPointByPtAngle(ptMiddle, angleSegment + (Pi / 2), distFromCenter)
    else
      Center := TGIS_EditorUtils.PolarPointByPtAngle(ptMiddle, angleSegment - (Pi / 2), distFromCenter) ;

    StartAngle := TGIS_EditorUtils.AngleBy2Points(self.Center, _start)  ;
    EndAngle := TGIS_EditorUtils.AngleBy2Points(self.Center, _end) ;
    Reversed := False ;
    Result := True ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartEndPtsRadius(
    const _start  : TGIS_Point ;
    const _end    : TGIS_Point ;
    const _radius : Double
  ) : Boolean ;
  var
    chord,
    half_chord,
    angleSegment,
    distFromCenter : Double ;
    ptMiddle       : TGIS_Point ;
  begin
    if GisIsSamePoint( _start,  _end ) or (_radius = 0) then begin
      Result := False ;
      exit ;
    end ;

    chord := TGIS_EditorUtils.Distance(_start, _end) ;
    half_chord := chord / 2  ;

    if _radius < half_chord then begin
      Result := False ;
      exit ;
    end ;
    Radius := _radius ;

    angleSegment := TGIS_EditorUtils.AngleBy2Points(_start, _end) ;
    ptMiddle := TGIS_EditorUtils.MiddlePoint(_start, _end) ;

    distFromCenter := Sqrt((self.Radius * self.Radius) - (half_chord * half_chord)) ;
    Center := TGIS_EditorUtils.PolarPointByPtAngle(ptMiddle, angleSegment + (Pi / 2), distFromCenter) ;

    StartAngle := TGIS_EditorUtils.AngleBy2Points(self.Center, _start)  ;
    EndAngle := TGIS_EditorUtils.AngleBy2Points(self.Center, _end) ;
    Reversed := False ;
    Result := True ;
  end ;

  function TGIS_EditorShapeArc.BuildFromStartSecondEndPts(
    const _points : TArray<TGIS_Point>
  ) : Boolean ;
  begin
    Result := FromPolyline( _points, 0, 2 ) ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_EditorToolArc'}
  { TGIS_EditorToolArc }

  procedure TGIS_EditorToolArc.init ;
  begin
    FName := 'Arc tool' ;
    SetLength( FPoints, 4 ) ;
    {$IFDEF GIS_NORECORDS}
      for i : Integer := 0 to 3 do
        FPoints[i] := new TGIS_Point() ;
    {$ENDIF}
    FPointsCount := 0 ;
    inherited ;

    askForNextStep ;
  end ;

  procedure TGIS_EditorToolArc.restoreState(
    const _points : TGIS_PointArray
  ) ;
  begin

  end ;

  procedure TGIS_EditorToolArc.askForNextStep ;
  begin
    case arcMode of
      TGIS_EditorToolArcMode.AmStartEndSecond:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_LAST_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_SECOND_POINT ) )
        end ;
      TGIS_EditorToolArcMode.AmStartSecondEnd:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_SECOND_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_LAST_POINT ) )
        end ;
      TGIS_EditorToolArcMode.AmStartCenterEnd:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_LAST_POINT ) )
        end ;
      TGIS_EditorToolArcMode.AmStartCenterAngle:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_ANGLE  ) )
        end ;
      TGIS_EditorToolArcMode.AmStartCenterChord:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CHORD ) )
        end ;
      TGIS_EditorToolArcMode.AmStartEndAngle:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_LAST_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_ANGLE  ) )
        end ;
      TGIS_EditorToolArcMode.AmStartEndRadius:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_LAST_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_RADIUS ) )
        end ;
      TGIS_EditorToolArcMode.AmCenterStartEnd:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_LAST_POINT ) )
        end ;
      TGIS_EditorToolArcMode.AmCenterStartAngle:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_ANGLE  ) )
        end ;
      TGIS_EditorToolArcMode.AmCenterStartChord:
        begin
          if FPointsCount = 0 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CENTER_POINT ) )
          else if FPointsCount = 1 then
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_FIRST_POINT ) )
          else
            notifyAction( _rsrc( GIS_RS_EDITOR_SPECIFY_CHORD ) )
        end ;
    end ;
  end ;

  procedure TGIS_EditorToolArc.rememberStep(
    const _index : Integer
  ) ;
  var
    ptg : TGIS_Point ;
  begin
    ptg := FPoints[_index] ;

    case arcMode of
      TGIS_EditorToolArcMode.AmStartEndSecond:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcEndPt := ptg
          else if _index = 2 then
            arcSecondPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmStartSecondEnd:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcSecondPt := ptg
          else if _index = 2 then
            arcEndPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmStartCenterEnd:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcCenterPt := ptg
          else if _index = 2 then
            arcEndPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmStartCenterAngle:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcCenterPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmStartCenterChord:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcCenterPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmStartEndAngle:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcEndPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmStartEndRadius:
        begin
          if _index = 0 then
            arcStartPt := ptg
          else if _index = 1 then
            arcEndPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmCenterStartEnd:
        begin
          if _index = 0 then
            arcCenterPt := ptg
          else if _index = 1 then
            arcStartPt := ptg
          else if _index = 2 then
            arcEndPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmCenterStartAngle:
        begin
          if _index = 0 then
            arcCenterPt := ptg
          else if _index = 1 then
            arcStartPt := ptg
        end ;
      TGIS_EditorToolArcMode.AmCenterStartChord:
        begin
          if _index = 0 then
            arcCenterPt := ptg
          else if _index = 1 then
            arcStartPt := ptg
        end ;
    end ;
  end ;

  procedure TGIS_EditorToolArc.DoMouseDown(
    const _pt : TPoint
  ) ;
  begin
    FPoints[FPointsCount] := screenToMap(_pt) ;
    inc( FPointsCount ) ;
    askForNextStep ;
  end ;

  procedure TGIS_EditorToolArc.DoMouseMove(
    const _pt : TPoint
  ) ;
  begin
    FPoints[FPointsCount] := screenToMap(_pt) ;
  end ;

  procedure TGIS_EditorToolArc.DoMouseUp(
    const _pt : TPoint
  ) ;
  begin
    if FPointsCount = 3 then begin
      dec( FPointsCount ) ;
      FPoints[FPointsCount] := screenToMap(_pt) ;
      updateEditor( _pt ) ;
      FPointsCount := 1 ;
      FPoints[0] := FPoints[FPointsCount] ;
      askForNextStep ;
    end
    else
      FPoints[FPointsCount-1] := screenToMap(_pt) ;

    rememberStep( FPointsCount-1 ) ;
  end ;

  procedure TGIS_EditorToolArc.updateEditor(
    const _pt : TPoint
  ) ;
  const
    ARC_MAX_SEGMENTS = 90 ;
  var
    cnt, i   : Integer ;
    angle    : Double  ;
    arcangle : Double  ;
    delta    : Double  ;
    step     : Double  ;
    steps    : Integer ;
    rsin     : Double  ;
    rcos     : Double  ;
    ptg      : TGIS_Point3D {$IFDEF GIS_NORECORDS} = TGIS_Point3D.create {$ENDIF} ;
    sa,ca    : Double ;
    stop,
    start,
    radius,
    chord,
    rotation : Double ;
    center   : TGIS_Point ;
    arc      : TGIS_EditorShapeArc ;
    is_arc   : Boolean ;
    op       : TGIS_ShapeArcOptions ;
    buf      : TArray<TGIS_Point> ;
  begin
    beginUpdatingEditor ;
    op.MinimumSegments := 45 ;
    op.ToleranceOfApproximation := 0.1 ;
    arc.InitVariables( op ) ;

    applyLockedParams ;
    is_arc := False ;
    case arcMode of
      TGIS_EditorToolArcMode.AmStartEndSecond:
        begin
          start := 0 ;
          stop  := 2*Pi ;
          rotation := 0 ;

          if TGIS_EditorUtils.CircleCenter( FPoints[0], FPoints[2], FPoints[1], center ) then
            TGIS_EditorUtils.CalculateArcAngles( FPoints[0], FPoints[2], FPoints[1], center, start, stop ) ;

          radius := TGIS_EditorUtils.Distance( center, FPoints[2] ) ;

          SinCos( -rotation, rsin, rcos ) ;
          delta := (stop-start) ;

          if delta > 0 then
            arcangle := TGIS_EditorUtils.FMod( (stop-start) + 4*Pi, 2*Pi)
          else
            arcangle := TGIS_EditorUtils.FMod( (stop-start) - 4*Pi, 2*Pi) ;

          if arcangle = 0 then begin
            if stop <> start then
              arcangle := 2*Pi ;
          end ;

          steps := RoundS( Abs( arcangle ) / (2*Pi) * ARC_MAX_SEGMENTS ) ;
          steps := Max( 4, steps ) ;
          step  := arcangle / steps ;
          angle := start ;

          for cnt := 0 to steps do begin
            SinCos( angle, sa, ca ) ;
            ptg.Y := -radius * sa * rcos + radius * ca * rsin + center.Y ;
            ptg.X :=  radius * ca * rcos + radius * sa * rsin + center.X ;
            ptg.Z :=  0 ;

            if cnt = 0 then
              Editor.InsertPointEx( Editor.PointPos  , ptg )
            else
              Editor.InsertPoint( Editor.PointPos + 1 , ptg ) ;

            angle := angle + step ;
          end ;
          FLastPoint := FPoints[1] ;
        end ;
      TGIS_EditorToolArcMode.AmStartSecondEnd:
        begin
          is_arc := arc.BuildFromStartSecondEndPts( [FPoints[0], FPoints[1], FPoints[2]] ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartCenterEnd:
        begin
          is_arc :=  arc.BuildFromStartCenterEndPts( FPoints[0], FPoints[1], FPoints[2] ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartCenterAngle:
        begin
          angle  := TGIS_EditorUtils.AngleBy2Points( arcCenterPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsAngle( FPoints[0], FPoints[1], angle ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartCenterChord:
        begin
          chord := TGIS_EditorUtils.Distance( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsChord( arcStartPt, arcCenterPt, chord ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartEndAngle:
        begin
          angle  := TGIS_EditorUtils.AngleBy2Points( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartEndPtsAngle( arcStartPt, arcEndPt, angle ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartEndRadius:
        begin
          radius := TGIS_EditorUtils.Distance( arcEndPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartEndPtsRadius( arcStartPt, arcEndPt, radius ) ;
        end ;
      TGIS_EditorToolArcMode.AmCenterStartEnd:
        begin
          is_arc := arc.BuildFromStartCenterEndPts( arcStartPt, arcCenterPt, FPoints[2] ) ;
        end ;
      TGIS_EditorToolArcMode.AmCenterStartAngle:
        begin
          angle := TGIS_EditorUtils.AngleBy2Points( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsAngle( arcStartPt, arcCenterPt, angle ) ;
        end ;
      TGIS_EditorToolArcMode.AmCenterStartChord:
        begin
          chord := TGIS_EditorUtils.Distance( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsChord( arcStartPt, arcCenterPt, chord ) ;
        end ;
    end ;

    if is_arc then begin
      if isCtrlPressed then
        arc.InverseAngle ;

      delta := (arc.EndAngle - arc.StartAngle) ;

      if delta > 0 then
        arcangle := TGIS_EditorUtils.FMod( (arc.EndAngle-arc.StartAngle) + 4*Pi, 2*Pi)
      else
        arcangle := TGIS_EditorUtils.FMod( (arc.EndAngle-arc.StartAngle) -4*Pi, 2*Pi) ;

      if arcangle = 0 then begin
        if stop <> start then
          arcangle := 2*Pi ;
      end
      else if arcangle < 0 then
        arcangle := 2*Pi - arcangle ;

      steps := RoundS( Abs( arcangle ) / (2*Pi) * ARC_MAX_SEGMENTS ) ;
      steps := Max( 4, steps ) ;

      buf := arc.AsPolyline( steps ) ;
      cnt := length(buf) ;
      for i := 0 to cnt-1 do begin
        ptg   := GisPoint3DFrom2D( buf[i] ) ;
        ptg.Z := 0 ;
        if i = 0 then
          Editor.InsertPointEx( Editor.PointPos  , ptg )
        else
          Editor.InsertPoint( Editor.PointPos + 1 , ptg ) ;
      end ;
      FPoints[1] := arc.GetEndPoint ;
      FLastPoint := FPoints[1] ;
    end ;
    endUpdatingEditor ;
  end ;

  procedure TGIS_EditorToolArc.drawDistanceText ;
  var
    size  : TPoint ;
    cpt   : TPoint ;
    rct   : TRect ;
    str   : String ;
  begin
    str   := Format('%f',[FDistance] ) ;
    size  := FRenderer.CanvasTextExtent( str ) ;
    cpt   := mapToScreen(TGIS_EditorUtils.MiddlePoint( FPoints[FPointsCount-1], FPoints[FPointsCount] )) ;
    rct   := TGIS_EditorUtils.InflateRect( cpt, size ) ;

    drawCanvasText( str, rct ) ;
  end ;

  procedure TGIS_EditorToolArc.drawHelperLine ;
  var
    pt0, pt1 : TPoint ;
  begin
    prepareHelperPen ;

    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt1.X, pt1.Y ) ;
  end ;

  function TGIS_EditorToolArc.fget_Mode : TGIS_EditorToolArcMode ;
  begin
    Result := arcMode ;
  end ;

  procedure TGIS_EditorToolArc.fset_Mode(
    const _mode : TGIS_EditorToolArcMode
  ) ;
  begin
    arcMode := _mode ;
    //init ;
  end ;

  procedure TGIS_EditorToolArc.drawArc ;
  const
    ARC_SEGMENTS = 45 ;
  var
    center : TGIS_Point ;
    middle : TGIS_Point ;
    start,
    angle,
    chord,
    radius,
    stop    : Double ;
    is_arc  : Boolean ;
    arc     : TGIS_EditorShapeArc ;
    op      : TGIS_ShapeArcOptions ;
    pt0, pt1,
    pt2, ct : TPoint ;
    radiusi : Integer ;
    buf     : TGIS_DrawBuf ;
  begin
    op.MinimumSegments := 45 ;
    op.ToleranceOfApproximation := 0.1 ;
    arc.InitVariables( op ) ;

    is_arc := False ;

    pt0 := mapToScreen( FPoints[0] ) ;
    pt1 := mapToScreen( FPoints[1] ) ;
    pt2 := mapToScreen( FPoints[2] ) ;
    case arcMode of
      TGIS_EditorToolArcMode.AmStartEndSecond:
        begin
          if TGIS_EditorUtils.CircleCenter( FPoints[0], FPoints[1], FPoints[2], center ) then begin
            prepareMainPen ;
            ct := mapToScreen( center ) ;
            radiusi := TGIS_EditorUtils.Distance( mapToScreen(center), mapToScreen(FPoints[2]) ) ;
            middle := TGIS_EditorUtils.MiddlePoint( FPoints[0], FPoints[1] ) ;
            FRenderer.CanvasDrawLine( pt2.X, pt2.Y, ct.X, ct.Y ) ;
            TGIS_EditorUtils.CalculateArcAngles( FPoints[0], FPoints[2], FPoints[1], center, start, stop ) ;
            FRenderer.CanvasDrawArc( ct.X, ct.Y, radiusi, -RadToDeg(start), -RadToDeg(stop-start) ) ;
          end ;
        end ;
      TGIS_EditorToolArcMode.AmStartSecondEnd:
        begin
          is_arc := arc.BuildFromStartSecondEndPts( [FPoints[0], FPoints[1], FPoints[2]] ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartCenterEnd:
        begin
          is_arc :=  arc.BuildFromStartCenterEndPts( FPoints[0], FPoints[1], FPoints[2] ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartCenterAngle:
        begin
          angle := TGIS_EditorUtils.AngleBy2Points( arcCenterPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsAngle( FPoints[0], FPoints[1], angle ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartCenterChord:
        begin
          chord := TGIS_EditorUtils.Distance( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsChord( arcStartPt, arcCenterPt, chord ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt0.X, pt0.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartEndAngle:
        begin
          angle := TGIS_EditorUtils.AngleBy2Points( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartEndPtsAngle( arcStartPt, arcEndPt, angle ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmStartEndRadius:
        begin
          radius := TGIS_EditorUtils.Distance( arcEndPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartEndPtsRadius( arcStartPt, arcEndPt, radius ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmCenterStartEnd:
        begin
          is_arc := arc.BuildFromStartCenterEndPts( arcStartPt, arcCenterPt, FPoints[2] ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmCenterStartAngle:
        begin
          angle := TGIS_EditorUtils.AngleBy2Points( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsAngle( arcStartPt, arcCenterPt, angle ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
      TGIS_EditorToolArcMode.AmCenterStartChord:
        begin
          chord := TGIS_EditorUtils.Distance( arcStartPt, FPoints[2] ) ;
          is_arc := arc.BuildFromStartCenterPtsChord( arcStartPt, arcCenterPt, chord ) ;
          prepareHelperPen ;
          FRenderer.CanvasDrawLine( pt1.X, pt1.Y, pt2.X, pt2.Y ) ;
        end ;
    end ;

    if is_arc then begin
      if isCtrlPressed then
        arc.InverseAngle ;
      prepareMainPen ;
      buf := mapToScreenBuffer( arc.AsPolyline(ARC_SEGMENTS) ) ;
      FRenderer.CanvasDrawPolyLine( buf ) ;
    end ;
  end ;

  procedure TGIS_EditorToolArc.DoCanvasPaint ;
  begin
    if FPointsCount = 0 then exit ;

    applyLockedParams ;
    drawHelperLine ;
    drawDistanceText ;
    if FPointsCount = 2 then
      drawArc ;
  end ;

{$ENDREGION}

//==================================== END =====================================
end.

