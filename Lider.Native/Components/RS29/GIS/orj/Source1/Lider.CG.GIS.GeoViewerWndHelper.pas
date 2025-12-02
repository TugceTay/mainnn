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
  Classes to help screen manipulation like zoom, drag etc.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoViewerWndHelper;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoViewerWndHelper"'}
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

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
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
  TGIS_ViewerWndHelper    = class ;
  TGIS_ViewerWndHelperRun = class ;

  /// <summary>
  ///   TGIS_ViewerWndHelperRun callback procedure
  /// </summary>
  /// <param name="_sender">
  ///   sender object
  /// </param>
  /// <param name="_context">
  ///   execution context
  /// </param>
  TGIS_ViewerWndHelperRunEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
     _sender  : TObject ;
     _context : TGIS_ViewerWndHelperRun
  ) of object;

  /// <summary>
  ///   TGIS_ViewerWndHelperRun callback procedure
  /// </summary>
  /// <param name="_sender">
  ///   sender object
  /// </param>
  /// <param name="_context">
  ///   execution context
  /// </param>
  /// <param name="_final">
  ///   if true then final synchronize; progressive mode otherwise
  /// </param>
  TGIS_ViewerWndHelperSynchronizeEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
     _sender  : TObject ;
     _context : TGIS_ViewerWndHelperRun ;
     _final   : Boolean
  ) of object;

  /// <summary>
  ///   TGIS_ViewerWndHelper callback procedure
  /// </summary>
  /// <param name="_sender">
  ///   sender object
  /// </param>
  TGIS_ViewerWndHelperSimpleEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
     _sender  : TObject
  ) of object;

  /// <summary>
  ///   A single update run context It could be a for example in a thread context
  /// </summary>
  TGIS_ViewerWndHelperRun = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_ViewerHelperRun )
    private
      bRunning : Boolean ;
      oContext : TGIS_ViewerWndHelper ;
      procedure Execute ;

    public
      /// <summary>
      ///   Standard constructor
      /// </summary>
      /// <param name="_context">
      ///   stored execution context
      /// </param>
      constructor Create( const _context : TGIS_ViewerWndHelper );
    public

      /// <summary>
      ///   Cached bitmap. To be used upon scaling and other animations.
      /// </summary>
      {$IFDEF DCC} [weak] {$ENDIF}
      Bitmap  : TObject ;

      /// <summary>
      ///   Cached bitmap width.
      /// </summary>
      BitmapWidth        : Single ;

      /// <summary>
      ///   Cached bitmap height.
      /// </summary>
      BitmapHeight       : Single ;

      /// <summary>
      ///   Cached extent.
      /// </summary>
      BitmapExtent       : TGIS_Extent ;
    public
      /// <summary>
      ///   Prepare context at the begging of drawing process.
      /// </summary>
      /// <param name="_width">
      ///   cached width
      /// </param>
      /// <param name="_height">
      ///   cached height
      /// </param>
      /// <param name="_extent">
      ///   cached visible extent
      /// </param>
      procedure Prepare      ( const _width  : Single ;
                               const _height : Single ;
                               const _extent : TGIS_Extent
                             ) ;

      /// <summary>
      ///   Check if execution should be terminated
      /// </summary>
      /// <returns>
      ///   True if break requested.
      /// </returns>
      function  MustBreak : Boolean ;

      /// <inheritdoc/>
      procedure DoSynchronize( const _final : Boolean ) ; override ;

  end;

  /// <summary>
  ///   Windows helper for operation like zoom, drag etc.
  /// </summary>
  TGIS_ViewerWndHelper = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      FDelayedTime         : Cardinal ;
      FKeepScale           : Boolean ;
      {$IFDEF DCC} [weak] {$ENDIF}
      oVwr                 : TGIS_Viewer;
      oDelayedUpdateTimer  : TGIS_Timer ;
      iDelayedUpdateTime   : Int64      ;
      iInterruptTimeout    : Integer    ;
      oTapLongTimer        : TGIS_Timer ;
      bInAnimation         : Boolean    ;
      bTooSlow             : Boolean    ;
      iResize              : Integer    ;
      fnRaiseTap           : TGIS_ViewerWndHelperSimpleEvent ;
    {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
      fnUpdateExecute      : TGIS_ViewerWndHelperRunEvent ;
      fnUpdateSynchronize  : TGIS_ViewerWndHelperSynchronizeEvent ;
    private
      /// <summary>
      ///   Original cached image rectangle.
      /// </summary>
      rctOriginal         : TRectF;

      /// <summary>
      ///   Actual scaled cached image.
      /// </summary>
      rctScaled           : TRectF;

      /// <summary>
      ///   Cached image extent upon mouse down.
      /// </summary>
      extOriginal         : TGIS_Extent;

      /// <summary>
      ///   Cached image extent as set by user.
      /// </summary>
      extForced           : TGIS_Extent;

      /// <summary>
      ///   Cached image rectangle upon mouse down.
      ///   For ActulExtent calculation.
      /// </summary>
      extNonscale         : TGIS_Extent;
      {$IFDEF JAVA}
        /// <summary>
        ///   Tells if the 1st resize is already done.
        /// </summary>
        bFirstResizeDone  : Boolean ;
      {$ENDIF}

    public
      /// <summary>
      ///   Create an instance
      /// </summary>
      /// <param name="_viewer">
      ///   Viewer context
      /// </param>
      /// <param name="_updateExecute">
      ///   procedure to execute updates
      /// </param>
      /// <param name="_updateSynchronize">
      ///   procedure to execute thread safe cache bitmap update
      /// </param>
      /// <param name="_raiseTap">
      ///   procedure to raise a tap event
      /// </param>
      constructor Create ( const _viewer            : TGIS_Viewer                  ;
                           const _updateExecute     : TGIS_ViewerWndHelperRunEvent ;
                           const _updateSynchronize : TGIS_ViewerWndHelperSynchronizeEvent ;
                           const _raiseTap          : TGIS_ViewerWndHelperSimpleEvent
                         ) ; overload;

      /// <summary>
      ///   Create an instance
      /// </summary>
      /// <param name="_viewer">
      ///   Viewer context
      /// </param>
      /// <param name="_updateExecute">
      ///   procedure to execute updates
      /// </param>
      /// <param name="_updateSynchronize">
      ///   procedure to execute thread safe cache bitmap update
      /// </param>
      constructor Create ( const _viewer            : TGIS_Viewer                  ;
                           const _updateExecute     : TGIS_ViewerWndHelperRunEvent ;
                           const _updateSynchronize : TGIS_ViewerWndHelperSynchronizeEvent
                         ) ; overload;

    protected
      procedure doDestroy ; override;

    private
      procedure fset_DelayedTime   ( const _value  : Cardinal
                                   ) ;
      function  fget_PendingUpdate : Boolean ;
      {$IFDEF JAVA}
        method  checkFirstResize ;
      {$ENDIF}
    private

      /// <summary>
      ///   Adjust rectangle to properly calculate margins of full extent etc.
      /// </summary>
      /// <param name="_rect">
      ///   image size to be adjusted
      /// </param>
      function  adjustToExtent     ( const _rect   : TRectF
                                   ) : TRectF ;

      /// <summary>
      ///   Animate transition between current scaled bitmap to a new scaled
      ///   bitmap
      /// </summary>
      /// <param name="_torect">
      ///   new scaled bitmap rectangle
      /// </param>
      /// <param name="_forced">
      ///   if true then animation will be performed even iv viewer has
      ///   animation disabled; use for example for mouse wheel handling
      /// </param>
      procedure animateScaledRect  ( const _torect : TRectF  ;
                                     const _forced : Boolean
                                   ) ;

      /// <summary>
      ///   Adjust context from window coordinates to scaled cache bitmap.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle in window coordinates
      /// </param>
      function  screen2scaled      ( const _rect   : TRectF
                                   )  : TRectF ;
      /// <summary>
      ///   Perform updated caused by UpdateDelayed
      /// </summary>
      procedure doDelayedUpdate    (       _sender : TObject
                                   ) ;

      /// <summary>
      ///   Perform mouse Tap
      /// </summary>
      procedure doTapLong          (       _sender : TObject
                                   ) ;

    public
      /// <summary>
      ///   Status of gesture: is it active etc.
      /// </summary>
      GestureState                 : TGIS_GestureState ;

      /// <summary>
      ///   Reset context to actual cache bitmap size and actual VisibleExtent
      ///   of the map
      /// </summary>
      /// <param name="_context">
      ///   execution context
      /// </param>
      procedure Reset              ( const _context : TGIS_ViewerWndHelperRun
                                   ) ;

      /// <summary>
      ///   Reset Delayed update threshold to original value.
      /// </summary>
      /// <remarks>
      ///   For example Wheel/DoZoom requires delayed operation in all cases.
      /// </remarks>
      procedure ResetDelayedTime   ;

      /// <summary>
      ///   Set VisibleExtent based on current position and size of cached bitmap
      /// </summary>
      procedure SetVisibleExtent   ;

      /// <summary>
      ///   Cached bitmap scaling sate.
      /// </summary>
      /// <returns>
      ///   True if cache bitmap is scaled or dragged
      /// </returns>
      function  IsScaled           : Boolean  ;

      /// <summary>
      ///   Drag a cache bitmap. Provide a visual feedback.
      /// </summary>
      /// <param name="_dx">
      ///   horizontal dragging distance in window coordinates
      /// </param>
      /// <param name="_dy">
      ///   vertical dragging distance in window coordinates
      /// </param>
      procedure DoDrag             ( const _dx     : Double ;
                                     const _dy     : Double
                                   ) ;

      /// <summary>
      ///   Zoom a cache bitmap by center point. Provide a visual feedback.
      /// </summary>
      /// <param name="_x">
      ///   center point in window coordinates
      /// </param>
      /// <param name="_y">
      ///   center point in window coordinates
      /// </param>
      /// <param name="_zoom">
      ///   zoom level
      /// </param>
      /// <param name="_animated">
      ///   if true then animation will be performed
      /// </param>
      /// <param name="_forced">
      ///   if true then animation will be performed even iv viewer has
      ///   animation disabled; use for example for mouse wheel handling
      /// </param>
      procedure DoZoom             ( const _x        : Double ;
                                     const _y        : Double ;
                                     const _zoom     : Double ;
                                     const _animated : Boolean;
                                     const _forced   : Boolean
                                   ) ; overload;

      /// <summary>
      ///   Zoom a cache bitmap by zooming rectangle. Provide a visual feedback.
      /// </summary>
      /// <param name="_x1">
      ///   first corner x coordinate
      /// </param>
      /// <param name="_y1">
      ///   first corner x coordinate
      /// </param>
      /// <param name="_x2">
      ///   second corner x coordinate
      /// </param>
      /// <param name="_y2">
      ///   second corner x coordinate
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    if rectangle is negative then zoom out
      ///    </note>
      /// </remarks>
      procedure DoZoom             ( const _x1     : Double ;
                                     const _y1     : Double ;
                                     const _x2     : Double ;
                                     const _y2     : Double
                                   ) ; overload;


      /// <summary>
      ///   Perform animation of placing map in a full extent.
      /// </summary>
      procedure DoFullExtent       ;

      /// <summary>
      ///   Animate cache bitmap to fit it properly into the window. For example
      ///   guarantee proper gap if it is a Full Extent case.
      /// </summary>
      procedure FitScreenAnimation ;

      /// <summary>
      ///   Cancel any pending full map updates.
      /// </summary>
      procedure UpdateCancel       ;

      /// <summary>
      ///   Cancel any pending full map updates.
      /// </summary>
      /// <returns>
      ///   True if operation was ingored; False if interrupt/cancel was executed
      /// </returns>
      /// <remarks>
      ///   silent period in milisceconds defaults to 250;
      ///   can be changed via 'TGIS_ViewerWnd.InterruptTimeout'; if set to to
      ///   -1 then process can not be interrapted
      /// </remarks>
      function UpdateCancelOptional : Boolean ;

      /// <summary>
      ///   Force immediate map update.
      /// </summary>
      procedure UpdateImmediate     ;

      /// <summary>
      ///   Perform delayed map update. Operation will give a chance to perform
      ///   next drag/zoom etc. before map will be updated.
      /// </summary>
      procedure UpdateDelayed       ;

      /// <summary>
      ///   Test if delayed update is ongoing.
      /// </summary>
      /// <returns>
      ///   True if UpdateDelayed is ongoing.
      /// </returns>
      function  UpdatePending       : Boolean ;

      /// <summary>
      ///   Start operation upon MouseDown gesture.
      /// </summary>
      /// <param name="_shift">
      ///   True if Shift key is down
      /// </param>
      /// <param name="_alt">
      ///   True if Alt key is down
      /// </param>
      /// <param name="_ctrl">
      ///   True if Ctrl key is down
      /// </param>
      /// <param name="_left">
      ///   True if left mouse button is down
      /// </param>
      /// <param name="_right">
      ///   True if right mouse button is down
      /// </param>
      /// <param name="_middle">
      ///   True if middle mouse button is down
      /// </param>
      /// <param name="_touch">
      ///   finger touch source
      /// </param>
      /// <param name="_pen">
      ///   pen touch source
      /// </param>
      /// <param name="_x">
      ///   gesture position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   gesture position in pixels relative to form
      /// </param>
      procedure GestureMouseDown   ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _touch  : Boolean ;
                                     const _pen    : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;

      /// <summary>
      ///   Start operation upon MouseUp gesture.
      /// </summary>
      /// <param name="_shift">
      ///   True if Shift key is down
      /// </param>
      /// <param name="_alt">
      ///   True if Alt key is down
      /// </param>
      /// <param name="_ctrl">
      ///   True if Ctrl key is down
      /// </param>
      /// <param name="_left">
      ///   True if left mouse button is down
      /// </param>
      /// <param name="_right">
      ///   True if right mouse button is down
      /// </param>
      /// <param name="_middle">
      ///   True if middle mouse button is down
      /// </param>
      /// <param name="_touch">
      ///   finger touch source
      /// </param>
      /// <param name="_pen">
      ///   pen touch source
      /// </param>
      /// <param name="_x">
      ///   gesture position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   gesture position in pixels relative to form
      /// </param>
      procedure GestureMouseUp     ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _touch  : Boolean ;
                                     const _pen    : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;
      /// <summary>
      ///   Start operation upon MouseMove gesture.
      /// </summary>
      /// <param name="_shift">
      ///   True if Shift key is down
      /// </param>
      /// <param name="_alt">
      ///   True if Alt key is down
      /// </param>
      /// <param name="_ctrl">
      ///   True if Ctrl key is down
      /// </param>
      /// <param name="_left">
      ///   True if left mouse button is down
      /// </param>
      /// <param name="_right">
      ///   True if right mouse button is down
      /// </param>
      /// <param name="_middle">
      ///   True if middle mouse button is down
      /// </param>
      /// <param name="_touch">
      ///   finger touch source
      /// </param>
      /// <param name="_pen">
      ///   pen touch source
      /// </param>
      /// <param name="_x">
      ///   gesture position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   gesture position in pixels relative to form
      /// </param>
      procedure GestureMouseMove   ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _touch  : Boolean ;
                                     const _pen    : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;

      /// <summary>
      ///   Cancel log tap gesture. For example if mouse went up before gesture
      ///   has been fired.
      /// </summary>
      procedure GestureCancelLongTap ;

      /// <summary>
      ///   Start gesture.
      /// </summary>
      /// <returns>
      ///   True if any gesture is currently active.
      /// </returns>
      function  GestureBegin       : Boolean ;

      /// <summary>
      ///   Finalize gesture.
      /// </summary>
      /// <param name="_clear">
      ///   if True then cancel all active gestures
      /// </param>
      /// <returns>
      ///   True if any gesture is currently active.
      /// </returns>
      function  GestureEnd         ( const _clear  : Boolean = True
                                   ) : Boolean ;

      /// <summary>
      ///   Gesture activity status.
      /// </summary>
      /// <returns>
      ///   True if any gesture is currently active.
      /// </returns>
      function  GestureActive      : Boolean ;

      /// <summary>
      ///   Gesture movement status.
      /// </summary>
      /// <returns>
      ///   Movements are within threshold and therefore should be treated as
      ///   "shakes" rather then actual movement.
      /// </returns>
      function  GestureNoMovement  : Boolean ;

      /// <summary>
      ///   Add movement to gesture engine.
      /// </summary>
      /// <param name="_x">
      ///   gesture position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   gesture position in pixels relative to form
      /// </param>
      procedure GestureRecordMovement(
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;

      /// <summary>
      ///   Actual extent of a map including current scale etc.
      /// </summary>
      /// <returns>
      ///   Computed extent.
      /// </returns>
      function ActualExtent : TGIS_Extent ;

      /// <summary>
      ///   Actual scale of a map.
      /// </summary>
      /// <returns>
      ///   Computed scale.
      /// </returns>
      function ActualScale : Double ;

      /// <summary>
      ///   Provides rectangle used to scale a cached bitmap upon Paint.
      /// </summary>
      /// <returns>
      ///   Computed scaling rectangle.
      /// </returns>
      /// <param name="_scale">
      ///   Scale to be used for display with scaled output like retina.
      /// </param>
      /// <remarks>
      ///   This overload is to be used upon scaling cached paint.
      /// </remarks>
      function ScaledRect          ( const _scale  : Single
                                   ) : TRectF ; overload ;

      /// <summary>
      ///   Provides rectangle used to scale a cache bitmap upon Paint.
      /// </summary>
      /// <param name="_ext">
      ///   extent of a bitmap to be scaled
      /// </param>
      /// <param name="_width">
      ///   width of a bitmap to be scaled
      /// </param>
      /// <param name="_height">
      ///   height of a bitmap to be scaled
      /// </param>
      /// <param name="_scale">
      ///   Scale to be used for display with scaled output like retina.
      /// </param>
      /// <returns>
      ///   Computed scaling rectangle.
      /// </returns>
      /// <remarks>
      ///   This overload is generally to be used upon scaling basemap.
      /// </remarks>
      function ScaledRect          ( const _ext    : TGIS_Extent ;
                                     const _width  : Double      ;
                                     const _height : Double      ;
                                     const _scale  : Single
                                   ) : TRectF ; overload ;

      /// <summary>
      ///   Provides rectangle used to scale a cache bitmap upon Paint.
      /// </summary>
      /// <param name="_ext">
      ///   extent of a bitmap to be scaled
      /// </param>
      /// <param name="_width">
      ///   width of a bitmap to be scaled
      /// </param>
      /// <param name="_height">
      ///   height of a bitmap to be scaled
      /// </param>
      /// <param name="_rotpt">
      ///   Rotation point used for fast rotaion opf basemaps.
      /// </param>
      /// <param name="_scale">
      ///   Scale to be used for display with scaled output like retina.
      /// </param>
      /// <returns>
      ///   Computed scaling rectangle.
      /// </returns>
      /// <remarks>
      ///   This overload is generally to be used upon scaling basemap.
      /// </remarks>
      function ScaledRect          ( const _ext    : TGIS_Extent ;
                                     const _width  : Double      ;
                                     const _height : Double      ;
                                     const _rotpt  : TPoint      ;
                                     const _scale  : Single
                                   ) : TRectF ; overload ;

      /// <summary>
      ///   Converts point coordinates from map related to screen related.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinate in map units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.DrawLabel for example.
      /// </remarks>
      /// <returns>
      ///   Map coordinates converted to screen coordinates.
      /// </returns>
      function  MapToScreen         ( const _ptg   : TGIS_Point
                                    ) : TPoint ;

      /// <summary>
      ///   Get the mouse position in map units
      /// </summary>
      /// <param name="_pt">
      ///   screen pixel position
      /// </param>
      /// <returns>
      ///   screen coordinates converted to map coordinates
      /// </returns>
      function  ScreenToMap        (  const _pt    : TPoint
                                   ) : TGIS_Point ;

      /// <summary>
      ///   Converts 3D point coordinates from map related to screen
      ///   related.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinate in map units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.DrawLabel for example.
      /// </remarks>
      /// <returns>
      ///   Map coordinates converted to screen coordinates.
      /// </returns>
      function  MapToScreen3D      ( const _ptg : TGIS_Point3D
                                   ) : TPoint ;

      /// <summary>
      ///   Converts point coordinates from screen related to map related.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in screen units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for example.
      /// </remarks>
      /// <returns>
      ///   Screen coordinates converted to map coordinates.
      /// </returns>
      function  ScreenToMap3D      ( const _pt  : TPoint
                                   ) : TGIS_Point3D ;

      /// <summary>
      ///   Converts point coordinates from map related to screen related,
      ///   but result will be in TGIS_Point.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in map units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for similar example.
      /// </remarks>
      /// <returns>
      ///   map coordinates converted to screen coordinates
      /// </returns>
      function  MapToScreenEx      ( const _pt  : TGIS_Point
                                   ) : TGIS_Point ;

      /// <summary>
      ///   Converts point coordinates from screen related to map related, but
      ///   source will be in TGIS_Point.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in screen units
      /// </param>
      /// <returns>
      ///   screen coordinates converted to map coordinates
      /// </returns>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for example.
      /// </remarks>
      function  ScreenToMapEx      ( const _pt  : TGIS_Point
                                   ) : TGIS_Point ;

      /// <summary>
      ///   Converts rectangle coordinates from map related to screen
      ///   related.
      /// </summary>
      /// <param name="_rct">
      ///   coordinate in map units
      /// </param>
      /// <returns>
      ///   Calculated screen rectangle.
      /// </returns>
      function  MapToScreenRect    ( const _rct : TGIS_Extent
                                   ) : TRect ;

      /// <summary>
      ///   Converts rectangle coordinates from screen related to map
      ///   related.
      /// </summary>
      /// <param name="_rct">
      ///   coordinate in screen units
      /// </param>
      /// <remarks>
      ///   This is a reverse process of MapToScreenRect method. See
      ///   MapToScreenRect for similar example.
      /// </remarks>
      /// <returns>
      ///   Calculated extent.
      /// </returns>
      function  ScreenToMapRect    ( const _rct : TRect
                                   ) : TGIS_Extent ;

    public
      /// <summary>
      ///  Delayed update threshold in milliseconds. If &gt;0 then
      ///  map will be updated after a while allowing for example subsequent
      ///  draw operation before full map redraw.
      ///  Default value is 700.
      /// </summary>
      property DelayedTime : Cardinal read  FDelayedTime
                                      write fset_DelayedTime ;
      /// <summary>
      ///  True if any timer based update is undergoing.
      /// </summary>
      property PendingUpdate : Boolean read  fget_PendingUpdate ;

      /// <summary>
      ///   True if a viewer should maintain scale upon resize.
      /// </summary>
      property KeepScale : Boolean read FKeepScale write FKeepScale ;
      /// <summary>
      ///  Original rectangle (before any zoom, drag, or scale).
      /// </summary>
      property OriginalRect : TRectF read  rctOriginal ;
  end;

const
  {#gendoc:hide}
  METADATA_INTERRUPTTIMEOUT
     = 'TGIS_ViewerWnd.InterruptTimeout';


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoFunctions ;
{$ENDIF}

const
  GIS_UPDATE_THRESHOLD = 700 ;


//==============================================================================
// TGIS_ViewerWndHelperRun
//==============================================================================

  constructor TGIS_ViewerWndHelper.Create(
    const _viewer            : TGIS_Viewer                  ;
    const _updateExecute     : TGIS_ViewerWndHelperRunEvent ;
    const _updateSynchronize : TGIS_ViewerWndHelperSynchronizeEvent
  ) ;
  var
    tmp : TGIS_ViewerWndHelperSimpleEvent ;
  begin
    tmp := nil ;
    Create( _viewer, _updateExecute, _updateSynchronize, tmp ) ;
  end;

  constructor TGIS_ViewerWndHelper.Create(
    const _viewer            : TGIS_Viewer                  ;
    const _updateExecute     : TGIS_ViewerWndHelperRunEvent ;
    const _updateSynchronize : TGIS_ViewerWndHelperSynchronizeEvent ;
    const _raiseTap          : TGIS_ViewerWndHelperSimpleEvent
  ) ;
  begin
    {$IFDEF JAVA}
      bFirstResizeDone := False ;
    {$ENDIF}
    oVwr := _viewer ;
    fnUpdateExecute     := _updateExecute     ;
    fnUpdateSynchronize := _updateSynchronize ;
    fnRaiseTap          := _raiseTap          ;

    oDelayedUpdateTimer := TGIS_Timer.Create ;
    iDelayedUpdateTime  := 0 ;

    FDelayedTime := GIS_UPDATE_THRESHOLD ;
    FKeepScale   := False ;
    iResize := 0;

    {$IFDEF OXYGENE}
      oDelayedUpdateTimer.OnTimer += doDelayedUpdate ;
    {$ELSE}
      oDelayedUpdateTimer.OnTimer := doDelayedUpdate ;
    {$ENDIF}

    oTapLongTimer   := TGIS_Timer.Create ;
    oTapLongTimer.Interval := GIS_GESTURE_TAPLONG_THRESHOLD ;
    {$IFDEF OXYGENE}
      oTapLongTimer.OnTimer += doTapLong ;
    {$ELSE}
      oTapLongTimer.OnTimer := doTapLong ;
    {$ENDIF}
    oTapLongTimer.Enabled := False ;

    extOriginal := GisNoWorld ;
    extNonscale := GisNoWorld ;
    extForced   := GisNoWorld ;

    bInAnimation := False ;
    bTooSlow     := False ;
  end;

  procedure TGIS_ViewerWndHelper.doDestroy ;
  begin
    UpdateCancel ;

    FreeObject( oDelayedUpdateTimer ) ;
    FreeObject( oTapLongTimer       ) ;
    inherited ;
  end ;

  procedure TGIS_ViewerWndHelper.fset_DelayedTime(
    const _value : Cardinal
  ) ;
  begin
    FDelayedTime := _value ;
    if FDelayedTime = 0 then
      oDelayedUpdateTimer.Interval := 1
    else
      oDelayedUpdateTimer.Interval := _value ;
  end;

  function TGIS_ViewerWndHelper.fget_PendingUpdate(
  ) : Boolean ;
  begin
    Result := oDelayedUpdateTimer.Enabled ;
  end;

  {$IFDEF JAVA}
    method TGIS_ViewerWndHelper.checkFirstResize;
    begin
      if not bFirstResizeDone then
        bFirstResizeDone := True ;
      inc(iResize);
    end;
  {$ENDIF}

  function TGIS_ViewerWndHelper.adjustToExtent(
    const _rect : TRectF
  ) : TRectF ;
  const
    eps = 1e-15 ;
  var
    w_s    : Double ;      // screen sizes
    h_s    : Double ;

    w_e    : Double ;      // extent sizes
    h_e    : Double ;

    dx     : Double ;
    dy     : Double ;

    be     : Integer ;      // big extent
    f_be   : Double ;
    x_be   : Double ;
    y_be   : Double ;

    w_or   : Double ;      // original
    h_or   : Double ;
    zx_or  : Double ;
    zy_or  : Double ;

    ce     : TGIS_Extent ; // current extent
    w_ce   : Double ;
    h_ce   : Double ;

    fe     : TGIS_Extent ; // full extent
    w_fe   : Double ;
    h_fe   : Double ;

    zmx_fe : Double ;      // full extent
    zmy_fe : Double ;
    zm_fe  : Double ;

    zm_ce  : Double ;
    zmx_ce : Double ;
    zmy_ce : Double ;

    w_ctl,                  // control
    h_ctl  : Double ;

  begin
    if ( not oVwr.RestrictedDrag  ) and
       ( _rect.Left < _rect.Right )     // and not fullextent
    then begin
      Result := _rect ;
      exit ;
    end ;

    fe   := oVwr.Extent ;
    w_fe := fe.XMax - fe.XMin ;
    h_fe := fe.YMax - fe.YMin ;

    if w_fe < 1e-7 then w_fe := 0.1 ;
    if h_fe < 1e-7 then h_fe := 0.1 ;

    w_or := rctOriginal.Right  - rctOriginal.Left ;
    h_or := rctOriginal.Bottom - rctOriginal.Top ;

    zx_or := ( extOriginal.XMax   - extOriginal.XMin ) / w_or ;
    zy_or := ( extOriginal.YMax   - extOriginal.YMin ) / h_or ;

    {$IFDEF GIS_NORECORDS}
      ce := new TGIS_Extent ;
    {$ENDIF}
    ce.XMin := extOriginal.XMin
               + zx_or * ( rctOriginal.Left    + _rect.Left   ) ;
    ce.XMax := extOriginal.XMin
               + zx_or * ( rctOriginal.Left    + _rect.Right  ) ;
    ce.YMin := extOriginal.YMin
               + zy_or * ( rctOriginal.Bottom  - _rect.Bottom ) ;
    ce.YMax := extOriginal.YMin
               + zy_or * ( rctOriginal.Bottom  - _rect.Top    ) ;

    if _rect.Left > _rect.Right then begin
      // fullextent case
      ce.XMin := fe.XMin - w_fe ;
      ce.XMax := fe.XMax + w_fe ;
      ce.YMin := fe.YMin - h_fe ;
      ce.YMax := fe.YMax + h_fe ;
    end ;

    // full extent calculation
    w_e := Max( Abs( w_fe ), eps ) ;
    h_e := Max( Abs( h_fe ), eps ) ;
    w_s := w_or ;
    h_s := h_or ;

    zmx_fe := w_s / w_e ;
    zmy_fe := h_s / h_e ;

    if zmx_fe < zmy_fe then // fit by width or height ?
      zm_fe := zmx_fe
    else
      zm_fe := zmy_fe ;

    // current extent calculation
    w_e := Max( Abs( ce.XMax - ce.XMin ), eps ) ;
    h_e := Max( Abs( ce.YMax - ce.YMin ), eps ) ;
    w_s := w_or ;
    h_s := h_or ;

    zmx_ce := w_s / w_e ;
    zmy_ce := h_s / h_e ;
    zm_ce  := Max( zmx_ce, zmy_ce ) ;

    if zm_ce <= zm_fe then begin
      ce := fe ;
    end ;

    w_ce := ce.XMax - ce.XMin ;
    h_ce := ce.YMax - ce.YMin ;

    be := oVwr.BigExtentMargin ;
    if be > 0 then begin
      f_be := 1.0 * Abs( oVwr.BigExtentMargin ) / 100 / 2 ;
      x_be := w_ce * f_be ;
      y_be := h_ce * f_be ;
    end
    else begin
      w_ctl := oVwr.Parent.ControlCanvasWidth ;
      h_ctl := oVwr.Parent.ControlCanvasWidth  ;

      f_be := 1.0 * Abs( oVwr.BigExtentMargin ) / 100 / 2 ;

      if _rect.Left > _rect.Right then begin
        w_ctl := RoundS( w_ctl - 1.0 * Abs( oVwr.BigExtentMargin ) / 100 * w_ctl ) ;
        h_ctl := RoundS( h_ctl - 1.0 * Abs( oVwr.BigExtentMargin ) / 100 * h_ctl ) ;

        f_be := f_be / Min( w_ctl / w_fe, h_ctl / h_fe );
      end
      else
        f_be := f_be / zm_ce ;

      x_be := oVwr.Parent.ControlCanvasWidth  * f_be ;
      y_be := oVwr.Parent.ControlCanvasHeight * f_be ;
    end;

    // add margins
    if zm_ce <= zm_fe then begin

      // proportionally extend extent
      ce.XMin := fe.XMin - x_be ;
      ce.XMax := fe.XMax + x_be ;
      ce.YMin := fe.YMin - y_be ;
      ce.YMax := fe.YMax + y_be ;

      w_ce := ce.XMax - ce.XMin ;
      h_ce := ce.YMax - ce.YMin ;

      dx := w_or / h_or *  h_ce / w_ce ;

      if dx > 1 then begin
        ce.XMin := ce.XMin - ( dx - 1) / 2 * w_ce ;
        ce.XMax := ce.XMax + ( dx - 1) / 2 * w_ce ;
      end
      else begin
        dy := 1 / dx ;
        ce.YMin := ce.YMin - ( dy - 1) / 2 * h_ce ;
        ce.YMax := ce.YMax + ( dy - 1) / 2 * h_ce ;
      end;

    end
    else begin
      if zmx_ce < zmx_fe then begin
        dx :=   (  fe.XMin + w_fe / 2 )
              - (  ce.XMin + w_ce / 2 )  ;

        ce.XMin := ce.XMin + dx ;
        ce.XMax := ce.XMax + dx ;
      end
      else if ce.XMin < fe.XMin - x_be then begin
        ce.XMin := fe.XMin - x_be;
        ce.XMax := ce.XMin + w_ce ;
      end
      else if ce.XMax > fe.XMax + x_be then begin
        ce.XMax := fe.XMax + x_be ;
        ce.XMin := ce.XMax - w_ce ;
      end ;

      if zmy_ce < zmy_fe then begin
        dy :=   (  fe.YMin + h_fe / 2 )
              - (  ce.YMin + h_ce / 2 )  ;

        ce.YMin := ce.YMin + dy ;
        ce.YMax := ce.YMax + dy ;
      end
      else if ce.YMin < fe.YMin - y_be then begin
        ce.YMin := fe.YMin - y_be;
        ce.YMax := ce.YMin + h_ce ;
      end
      else if ce.YMax > fe.YMax + y_be then begin
        ce.YMax := fe.YMax + y_be ;
        ce.YMin := ce.YMax - h_ce ;
      end ;
    end ;

    Result := RectF(
          ( ( ce.XMin - extOriginal.XMin ) / zx_or - rctOriginal.Left   ),
        - ( ( ce.YMax - extOriginal.YMin ) / zy_or - rctOriginal.Bottom ),
          ( ( ce.XMax - extOriginal.XMin ) / zx_or - rctOriginal.Left   ),
        - ( ( ce.YMin - extOriginal.YMin ) / zy_or - rctOriginal.Bottom )
    )

  end;

  function TGIS_ViewerWndHelper.screen2scaled(
    const _rect : TRectF
  )  : TRectF ;
  var
    zmx : Double ;
    zmy : Double ;
  begin
    zmx := ( rctScaled.Right   - rctScaled.Left   )
           /
           ( rctOriginal.Right - rctOriginal.Left );
    zmy := ( rctScaled.Right   - rctScaled.Left   )
           /
           ( rctOriginal.Right - rctOriginal.Left );

    if zmx > 1e7 then
      zmx := 1 ;
    if zmy > 1e7 then
      zmy := 1 ;

    Result := RectF( rctScaled.Left + _rect.Left   * zmx,
                     rctScaled.Top  + _rect.Top    * zmy,
                     rctScaled.Left + _rect.Right  * zmx,
                     rctScaled.Top  + _rect.Bottom * zmy
                   ) ;
  end ;

  procedure TGIS_ViewerWndHelper.animateScaledRect(
    const _torect : TRectF ;
    const _forced : Boolean
  ) ;
  var
    i    : Integer  ;
    rct  : TRectF   ;
    tm   : Int64 ;
  const
    STEPS = 5 ;
  begin
    if bInAnimation then exit ;
    bInAnimation := True ;

    try
      if ( Abs( rctScaled.Left   - _torect.Left   ) < 5 ) and
         ( Abs( rctScaled.Top    - _torect.Top    ) < 5 ) and
         ( Abs( rctScaled.Right  - _torect.Right  ) < 5 ) and
         ( Abs( rctScaled.Bottom - _torect.Bottom ) < 5 )
      then
        exit ;

      rct := RectF(
               ( _torect.Left    - rctScaled.Left   ) / STEPS,
               ( _torect.Top     - rctScaled.Top    ) / STEPS,
               ( _torect.Right   - rctScaled.Right  ) / STEPS,
               ( _torect.Bottom  - rctScaled.Bottom ) / STEPS
             ) ;

      if not oVwr.UseAnimations then
        bTooSlow := True ;

      for i := 1 to STEPS do begin
        rctScaled :=
          RectF(
            rctScaled.Left   + rct.Left   ,
            rctScaled.Top    + rct.Top    ,
            rctScaled.Right  + rct.Right  ,
            rctScaled.Bottom + rct.Bottom
          ) ;

        if not bTooSlow then begin
          tm := GetTickCount ;
          oVwr.Parent.ControlRepaint ;
          oVwr.Parent.ControlProcessMessages ;
          tm := GetTickCount - tm ;
          if tm < 20 then // 50 frames per second
            Sleep( 20 - tm );
          if tm > 100 then
            bTooSlow := True ;
        end;
      end;

      if bTooSlow then begin
        if oVwr.UseAnimations or _forced then begin
          oVwr.Parent.ControlRepaint ;
          oVwr.Parent.ControlProcessMessages ;
        end ;
      end ;
    finally
      bInAnimation := False ;
    end;
  end;

  procedure TGIS_ViewerWndHelper.doDelayedUpdate(
    _sender : TObject
  ) ;
  var
    thd : TGIS_ViewerWndHelperRun ;
  begin
    {$IFDEF JAVA}if bFirstResizeDone then{$ENDIF}
    oDelayedUpdateTimer.Enabled := False ;

    FitScreenAnimation ;

    thd := TGIS_ViewerWndHelperRun.Create( self ) ;
    try
    finally
      FreeObject( thd ) ;
    end;
    iDelayedUpdateTime := 0 ;
  end;

  procedure TGIS_ViewerWndHelper.doTapLong(
    _sender : TObject
  ) ;
  begin
    oTapLongTimer.Enabled := False ;
    GestureState.DownCount := 3 ;
    if assigned( fnRaiseTap ) then
      fnRaiseTap( _sender ) ;
  end;

  procedure TGIS_ViewerWndHelper.Reset(
    const _context : TGIS_ViewerWndHelperRun
  )  ;
  var
    rct : TRectF ;
  begin
    rct := RectF( 0, 0, _context.BitmapWidth, _context.BitmapHeight ) ;

    rctOriginal := rct ;
    rctScaled   := rct ;
    extOriginal := _TGIS_Extent( _context.BitmapExtent ) ;
    extNonscale := _TGIS_Extent( _context.BitmapExtent ) ;
  end;

  procedure TGIS_ViewerWndHelper.ResetDelayedTime ;
  begin
    DelayedTime := FDelayedTime ;
  end;

  procedure TGIS_ViewerWndHelper.SetVisibleExtent  ;
  var
    ext    : TGIS_Extent ;
    exttmp : TGIS_Extent ;
    zoomtmp : Double      ;
    dw, dh : Double      ;
  begin
    oVwr.Lock ;
    try
      if not GisIsEmptyExtent( extOriginal ) then begin

        ext := _TGIS_Extent( extOriginal ) ;
        exttmp := _TGIS_Extent( extOriginal ) ;

        if ( rctOriginal.Left   = rctScaled.Left )
           and
           ( rctOriginal.Top    = rctScaled.Top  )
           and
           ( rctOriginal.Right  = rctScaled.Right )
           and
           ( rctOriginal.Bottom = rctScaled.Bottom )
        then begin
          oVwr.Lock ;
          exttmp := extForced ;
          zoomtmp := oVwr.Zoom ;
          oVwr.VisibleExtent := exttmp ;
          if KeepScale then
            oVwr.Zoom := zoomtmp ;
          oVwr.UnLock ;
          exit ;
        end ;

        dw := ( exttmp.XMax   - exttmp.XMin ) /
              ( rctOriginal.Right  - rctOriginal.Left ) ;
        dh := ( exttmp.YMax   - exttmp.YMin ) /
              ( rctOriginal.Bottom - rctOriginal.Top  ) ;

        ext.XMin := exttmp.XMin
                    + dw * ( rctOriginal.Left    + rctScaled.Left   ) ;
        ext.XMax := exttmp.XMin
                    + dw * ( rctOriginal.Left    + rctScaled.Right  ) ;
        ext.YMin := exttmp.YMin
                    + dh * ( rctOriginal.Bottom  - rctScaled.Bottom ) ;
        ext.YMax := exttmp.YMin
                    + dh * ( rctOriginal.Bottom  - rctScaled.Top    ) ;

        oVwr.VisibleExtent := ext ;
      end ;
      extForced := oVwr.VisibleExtent ;
    finally
      oVwr.Unlock( False ) ;
    end;
  end;

  function TGIS_ViewerWndHelper.IsScaled : Boolean  ;
  begin
    Result := ( rctOriginal.Left   <> rctScaled.Left   ) or
              ( rctOriginal.Right  <> rctScaled.Right  ) or
              ( rctOriginal.Top    <> rctScaled.Top    ) or
              ( rctOriginal.Bottom <> rctScaled.Bottom ) ;
  end;

  procedure TGIS_ViewerWndHelper.DoDrag(
    const _dx : Double ;
    const _dy : Double
  ) ;
  begin
    if oVwr.IsLocked then exit ;

    rctScaled := screen2scaled(
                   RectF(
                      _dx,
                      _dy,
                      oVwr.Parent.ControlCanvasWidth  + _dx,  //?x
                      oVwr.Parent.ControlCanvasHeight + _dy
                   )
                 );
    oVwr.Parent.ControlRepaint;
  end;

  procedure TGIS_ViewerWndHelper.DoZoom(
    const _x        : Double  ;
    const _y        : Double  ;
    const _zoom     : Double  ;
    const _animated : Boolean ;
    const _forced   : Boolean
  ) ;
  var
    rtmp : TRectF ;
  begin
    if _forced and ( FDelayedTime < 700 ) then
      oDelayedUpdateTimer.Interval := 700 ;

    if oVwr.IsLocked then exit ;
    if GisIsEmptyExtent( extOriginal ) then exit ;

    if bInAnimation then exit ;

    rtmp := screen2scaled(
                   RectF( _x - _x / _zoom,
                          _y - _y / _zoom,
                          _x + ( oVwr.Parent.ControlCanvasWidth  - _x ) / _zoom,
                          _y + ( oVwr.Parent.ControlCanvasHeight - _y ) / _zoom
                        )
             );

    if _animated then begin
      animateScaledRect( adjustToExtent( rtmp ), _forced ) ;
    end
    else begin
      rctScaled := rtmp ;
      oVwr.Parent.ControlRepaint;
    end;
  end;

  procedure TGIS_ViewerWndHelper.DoZoom( const _x1, _y1, _x2, _y2 : Double ) ;
  var
    dx, dy : Double ;
    cx, cy : Double ;
    zm     : Double ;
  begin
    if oVwr.IsLocked then exit ;
    if GisIsEmptyExtent( extOriginal ) then exit ;

    dx := Abs( _x2 - _x1 ) / 2 ;
    dy := Abs( _y2 - _y1 ) / 2 ;

    cx := Min( _x1, _x2 ) + dx ;
    cy := Min( _y1, _y2 ) + dy ;

    if ( _x2 > _x1 ) and ( _y2 > _y1 ) then begin
      // zooming

      if dx / dy > oVwr.Parent.ControlCanvasWidth / oVwr.Parent.ControlCanvasHeight
      then
        dy := RoundS(dx * oVwr.Parent.ControlCanvasHeight / oVwr.Parent.ControlCanvasWidth)
      else
        dx := RoundS(dy * oVwr.Parent.ControlCanvasWidth / oVwr.Parent.ControlCanvasHeight);

      animateScaledRect(
        adjustToExtent(
          screen2scaled (
            RectF(
              cx - dx ,
              cy - dy ,
              cx + dx ,
              cy + dy
            )
          )
        ),
        False
      ) ;

    end
    else begin
      // zoomout
      zm := Sqrt( Sqr( _x2 - _x1 ) +
                  Sqr( _y2 - _y1 )
                )
            /
            Sqrt(
              Sqr( oVwr.Parent.ControlCanvasWidth  ) +
              Sqr( oVwr.Parent.ControlCanvasHeight )
            ) ;

      if zm = 1 then
        exit;
      if Abs( zm ) < 1E-10 then
        exit;

      animateScaledRect(
        adjustToExtent(
          screen2scaled (
            RectF(
              cx - cx / zm,
              cy - cy / zm,
              cx + ( oVwr.Parent.ControlCanvasWidth  - cx ) / zm,
              cy + ( oVwr.Parent.ControlCanvasHeight - cy ) / zm
            )
          )
        ),
        False
      ) ;
    end ;
  end;

  procedure TGIS_ViewerWndHelper.DoFullExtent ;
  begin
    if oVwr.IsLocked then exit ;

    if not oVwr.UseAnimations then exit ;

    if not GisIsEmptyExtent( extOriginal ) then
      animateScaledRect( adjustToExtent( RectF( 1, 1, -1, -1 ) ), False ) ;
  end;

  procedure TGIS_ViewerWndHelper.FitScreenAnimation ;
  begin
    if oVwr.IsLocked then exit ;

    if IsScaled then
      animateScaledRect( adjustToExtent( rctScaled ), False ) ;
  end;

  procedure TGIS_ViewerWndHelper.UpdateCancel ;
  begin
    oVwr.Interrupt ;
    {$IFDEF JAVA}if bFirstResizeDone then{$ENDIF}
    oDelayedUpdateTimer.Enabled := False ;
  end;

  procedure TGIS_ViewerWndHelper.UpdateImmediate ;
  var
    o_run : TGIS_ViewerWndHelperRun ;
  begin
    if oVwr.IsLocked then exit ;

    if oDelayedUpdateTimer.Enabled then
      exit ;

    if oVwr.UponDestroy then
      exit ;

    extOriginal := GisNoWorld ;

    iDelayedUpdateTime := GetTickCount ;
    o_run := TGIS_ViewerWndHelperRun.Create( self ) ;
    try
      // execute is going inside
    finally
      FreeObject( o_run ) ;
    end;
  end;

  procedure TGIS_ViewerWndHelper.UpdateDelayed ;
  begin
    if oVwr.IsLocked then exit ;

    if oVwr.UponDestroy then
      exit ;
    {$IFDEF JAVA}if bFirstResizeDone then{$ENDIF}
      oDelayedUpdateTimer.Enabled := False ;
    {$IFDEF JAVA}if bFirstResizeDone then{$ENDIF}
      oDelayedUpdateTimer.Enabled := True ;
    iDelayedUpdateTime := GetTickCount ;
  end;

  function TGIS_ViewerWndHelper.UpdatePending
    : Boolean ;
  begin
    Result := oDelayedUpdateTimer.Enabled ;
  end;

  function TGIS_ViewerWndHelper.UpdateCancelOptional
   : Boolean ;
  begin
    Result := False ;

    if iInterruptTimeout < 0 then
      exit
    else
    if iDelayedUpdateTime = 0 then begin
      exit ;
    end;
    if GetTickCount - iDelayedUpdateTime <= iInterruptTimeout then begin
      Result := True ;
      exit ;
    end ;

    UpdateCancel ;
  end;

  procedure TGIS_ViewerWndHelper.GestureMouseDown(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _touch  : Boolean ;
    const _pen    : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;

    procedure reset_mouse_state ;
    begin
      GestureState.Shift     := _shift ;
      GestureState.Alt       := _alt ;
      GestureState.Ctrl      := _ctrl ;
      GestureState.Left      := _left ;
      GestureState.Right     := _right ;
      GestureState.Middle    := _middle ;
      GestureState.Touch     := _touch ;
      GestureState.Pen       := _pen ;
      GestureState.DownX     := _x ;
      GestureState.DownY     := _y ;
      GestureState.DownTime  := GetTickCount ;
      GestureState.DownCount := 1 ;
      GestureState.MoveDelta := 0 ;
      oTapLongTimer.Enabled := True ;
    end ;
  begin

    if ( _touch or _pen ) and _right then begin
      // some platforms (VCL, FMX(Windows), WinForms) raise
      // right click event for long tap gesture,
      // so there is no need to use a timer
      reset_mouse_state ;
      doTapLong( Self ) ;
      exit ;
    end ;

    if GetTickCount - GestureState.DownTime > GIS_GESTURE_TAPLONG_THRESHOLD then
      GestureState.DownCount := 0 ;

    if GestureState.DownCount = 0 then begin
      reset_mouse_state ;
    end
    else begin
      GestureRecordMovement( _x, _y  );
      if GestureNoMovement  then begin
        // accepted as double click
        GestureState.DownCount := 2 ;
      end
      else begin
        // treat as a new sequence
        reset_mouse_state ;
      end;
    end;
  end;

  procedure TGIS_ViewerWndHelper.GestureMouseUp(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _touch  : Boolean ;
    const _pen    : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureCancelLongTap ;

    GestureRecordMovement( _x, _y  );

    case GestureState.DownCount of
      1  : begin
             if GestureNoMovement then begin
               if assigned( fnRaiseTap ) then
                 fnRaiseTap( self );
             end ;
           end ;
      2  : begin
             if GestureNoMovement then begin
               if GetTickCount - GestureState.DownTime
                  <
                  GIS_GESTURE_TAPLONG_THRESHOLD
               then begin
                 // double click
                 if assigned( fnRaiseTap ) then
                   fnRaiseTap( self );
               end ;
             end ;
             GestureState.DownCount := 0 ;  // reset mouse state
           end ;
      else begin
             GestureState.DownCount := 0 ; // reset mouse state
           end;
    end;
  end;

  procedure TGIS_ViewerWndHelper.GestureMouseMove(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _touch  : Boolean ;
    const _pen    : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureRecordMovement( _x, _y ) ;
    if not GestureNoMovement then
      GestureCancelLongTap ;
  end;

  procedure TGIS_ViewerWndHelper.GestureCancelLongTap ;
  begin
    oTapLongTimer.Enabled := False ;
  end;

  function TGIS_ViewerWndHelper.GestureBegin
    : Boolean ;
  begin
    GestureState.GestureCnt := GestureState.GestureCnt + 1 ;
    Result := GestureState.GestureCnt > 0 ;
  end;

  function TGIS_ViewerWndHelper.GestureEnd(
    const _clear : Boolean = True
  ) : Boolean ;
  begin
    if _clear then
      GestureState.GestureCnt := 0
    else begin
      GestureState.GestureCnt := GestureState.GestureCnt - 1 ;
      if GestureState.GestureCnt < 0 then
        GestureState.GestureCnt := 0 ;
    end ;
    Result := GestureState.GestureCnt > 0 ;
  end;

  function TGIS_ViewerWndHelper.GestureActive : Boolean ;
  begin
    Result := GestureState.GestureCnt > 0 ;
  end;

  function TGIS_ViewerWndHelper.GestureNoMovement : Boolean ;
  begin
    Result := GestureState.MoveDelta <
              RoundS( GIS_GESTURE_MOVEMENT_THRESHOLD * oVwr.PPI / 96 );
  end;

  procedure TGIS_ViewerWndHelper.GestureRecordMovement(
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureState.MoveDelta := Max( GestureState.MoveDelta,
                                   Sqrt( Sqr( GestureState.DownX - _x ) +
                                         Sqr( GestureState.DownY - _y )
                                       )
                                 ) ;
  end;

//==============================================================================
// TGIS_ViewerWndHelperRun
//==============================================================================

  // emulate TThread like behavior
  constructor TGIS_ViewerWndHelperRun.Create(
    const _context : TGIS_ViewerWndHelper
  );
  begin
    oContext := _context ;
    inherited Create ;

    bRunning := False ;

    Execute ;
  end;

  // emulate TThread like behavior
  procedure TGIS_ViewerWndHelperRun.Execute ;
  begin
    oContext.iInterruptTimeout :=  GisMetadataAsInteger(
      METADATA_INTERRUPTTIMEOUT,
      250
    ) ;

    bRunning := True ;
    try
      oContext.fnUpdateExecute( self, self ) ;
    finally
      bRunning := False ;
      oContext.ResetDelayedTime ;
    end;
  end;

  function TGIS_ViewerWndHelperRun.MustBreak
    : Boolean ;
  begin
//?    Result := oContext.oVwr.Interrupt ;
    Result := False ;
  end;

  procedure TGIS_ViewerWndHelperRun.DoSynchronize(
    const _final : Boolean
  ) ;
  begin
    if bRunning then
      oContext.fnUpdateSynchronize( self, self, _final ) ;
  end ;

  procedure TGIS_ViewerWndHelperRun.Prepare(
    const _width  : Single;
    const _height : Single;
    const _extent : TGIS_Extent
  ) ;
  begin
    BitmapWidth  := _width ;
    BitmapHeight := _height ;
    BitmapExtent :=  _TGIS_Extent( _extent ) ;
  end;

  function TGIS_ViewerWndHelper.ActualExtent : TGIS_Extent ;
  var
    ext  : TGIS_Extent ;
    dw, dh : Double      ;
  begin
    if not GisIsEmptyExtent( extNonscale ) then begin
      ext := _TGIS_Extent( extOriginal ) ;
      dw := ( extNonscale.XMax   - extNonscale.XMin ) /
            ( rctOriginal.Right  - rctOriginal.Left ) ;
      dh := ( extNonscale.YMax   - extNonscale.YMin ) /
            ( rctOriginal.Bottom - rctOriginal.Top  ) ;

      ext.XMin := extNonscale.XMin
                  + dw * ( rctOriginal.Left    + rctScaled.Left   ) ;
      ext.XMax := extNonscale.XMin
                  + dw * ( rctOriginal.Left    + rctScaled.Right  ) ;
      ext.YMin := extNonscale.YMin
                  + dh * ( rctOriginal.Bottom  - rctScaled.Bottom ) ;
      ext.YMax := extNonscale.YMin
                  + dh * ( rctOriginal.Bottom  - rctScaled.Top    ) ;

      Result := ext ;
    end
    else
      Result := GisNoWorld ;
  end;

  function TGIS_ViewerWndHelper.ActualScale : Double ;
  var
    dw : Double ;
  begin
    if not GisIsEmptyExtent( extNonscale ) then begin
      dw := ( extNonscale.XMax   - extNonscale.XMin ) /
            ( rctOriginal.Right  - rctOriginal.Left ) ;

      Result := oVwr.ScaleAsFloat * dw ;
    end
    else
      Result := 1 ;
  end;


  function TGIS_ViewerWndHelper.ScaledRect(
    const _scale  : Single
  ) : TRectF ;
  var
    w, h   : Double ;
    wc, hc : Double ;
    dx, dy : Double ;
    dzm    : Double ;
    rcttmp : TRectF ;
    zm1    : Double ;
    zm2    : Double ;
    zm     : Double ;
  begin
    Result := RectF( 0, 0, 0, 0 );
    if GisIsNoWorld( extOriginal ) then
      exit ;

    if ( rctOriginal.Left   = rctScaled.Left   )
       and
       ( rctOriginal.Top    = rctScaled.Top    )
       and
       ( rctOriginal.Right  = rctScaled.Right  )
       and
       ( rctOriginal.Bottom = rctScaled.Bottom )
    then begin
      // simple zooming

      dx := ( extOriginal.XMax - extOriginal.XMin ) / rctOriginal.Width  ;
      dy := ( extOriginal.YMax - extOriginal.YMin ) / rctOriginal.Height ;

      if ( dx = 0 ) or ( dy = 0 ) then
        exit ;

      rcttmp := RectF(
                  rctOriginal.Left   + ( extForced.XMin - extOriginal.XMin ) / dx,
                  rctOriginal.Bottom - ( extForced.YMax - extOriginal.YMin ) / dy,
                  rctOriginal.Left   + ( extForced.XMax - extOriginal.XMin ) / dx,
                  rctOriginal.Bottom - ( extForced.YMin - extOriginal.YMin ) / dy
                ) ;

      wc := oVwr.Parent.ControlCanvasWidth  * _scale ;
      hc := oVwr.Parent.ControlCanvasHeight * _scale ;

      if KeepScale then
        zm := _scale
      else begin
      zm1 := wc / rcttmp.Width ;
      zm2 := hc / rcttmp.Height ;

      if zm1 < zm2 then
        zm := zm1
      else
      if zm2 < zm1 then
        zm := zm2
      else
        zm := zm1 ;
      end;

      Result := RectF(
                  wc / 2 - rctOriginal.Width  / 2 * zm,
                  hc / 2 - rctOriginal.Height / 2 * zm,
                  wc / 2 + rctOriginal.Width  / 2 * zm,
                  hc / 2 + rctOriginal.Height / 2 * zm
                ) ;
    end
    else begin
      rcttmp := rctScaled ;

      w  := rctOriginal.Width  ;
      h  := rctOriginal.Height ;

      wc := oVwr.Parent.ControlCanvasWidth  * _scale ;
      hc := oVwr.Parent.ControlCanvasHeight * _scale ;

      if ( rctScaled.Width < 2 )
         or
         ( rctScaled.Height < 2 )
      then
        exit ;

      dx := rcttmp.Width  / w ;
      dy := rcttmp.Height / h ;
      if (dx = 0) then
        dx := 1e-7 ;
      if (dy = 0) then
        dy := 1e-7 ;

      dzm := Min( wc / w,
                  hc / h
                ) ;

      Result := RectF(
                 ( ( 0 - rcttmp.Left ) / dx       ) * dzm  + ( wc/2 - w/2 * dzm ),
                 ( ( 0 - rcttmp.Top  ) / dy       ) * dzm  + ( hc/2 - h/2 * dzm ),
                 ( w - ( rcttmp.Right  - w ) / dx ) * dzm  + ( wc/2 - w/2 * dzm ),
                 ( h - ( rcttmp.Bottom - h ) / dy ) * dzm  + ( hc/2 - h/2 * dzm )
                ) ;
    end;
  end;

  function TGIS_ViewerWndHelper.ScaledRect(
    const _ext    : TGIS_Extent ;
    const _width  : Double      ;
    const _height : Double      ;
    const _scale  : Single
  ) : TRectF ;
  var
    wc, hc : Double ;
    res    : TRectF ;
    r1,r2  : TPointF ;
    rct    : TRectF ;
    ex     : TGIS_Extent ;
  begin
    ex := ActualExtent ;
    wc := oVwr.Parent.ControlCanvasWidth  * _scale ;
    hc := oVwr.Parent.ControlCanvasHeight * _scale ;

    if ( oVwr.Parent.ControlCanvasWidth  <> rctOriginal.Width  )
       or
       ( oVwr.Parent.ControlCanvasHeight <> rctOriginal.Height )
       or
       ( _width  < 2  )
       or
       ( _height < 2 )
    then begin
      Result := RectF( 0, 0, 0, 0 );
      exit ;
    end ;

    r1.X := ( _ext.XMax - _ext.XMin ) / _width  ;
    r1.Y := ( _ext.YMax - _ext.YMin ) / _height ;
    r2.X := ( ex.XMax  - ex.XMin ) / wc ;
    r2.Y := ( ex.YMax  - ex.YMin ) / hc ;

    rct := RectF(
              ( ex.XMin - _ext.XMin) / r1.X ,
             -( ex.YMax - _ext.YMax) / r1.X ,
             -( ex.XMax - _ext.XMax) / r1.X ,
              ( ex.YMin - _ext.YMin) / r1.X
           ) ;

    if Abs( r1.X / r2.X ) > 1000 then begin
      Result := RectF( 0, 0, 0, 0 );
      exit ;
    end ;

    res := RectF(
             ( - rct.Left   * r1.X / r2.X ),
             ( - rct.Top    * r1.X / r2.X ),
             ( + rct.Right  * r1.X / r2.X ) + wc ,
             ( + rct.Bottom * r1.X / r2.X ) + hc
           ) ;

    Result := res ;
  end;

  function TGIS_ViewerWndHelper.ScaledRect(
    const _ext    : TGIS_Extent ;
    const _width  : Double      ;
    const _height : Double      ;
    const _rotpt  : TPoint ;
    const _scale  : Single
  ) : TRectF ;
  var
    wc, hc : Double ;
    res    : TRectF ;
    r1,r2  : TPointF ;
    rct    : TRectF ;
    ex     : TGIS_Extent ;
  begin
    ex := ActualExtent ;
    wc := oVwr.Parent.ControlCanvasWidth  * _scale ;
    hc := oVwr.Parent.ControlCanvasHeight * _scale ;

    if ( oVwr.Parent.ControlCanvasWidth  <> rctOriginal.Width  )
       or
       ( oVwr.Parent.ControlCanvasHeight <> rctOriginal.Height )
       or
       ( _width  < 2  )
       or
       ( _height < 2 )
    then begin
      Result := RectF( 0, 0, 0, 0 );
      exit ;
    end ;

    r1.X := ( _ext.XMax - _ext.XMin ) / _width  ;
    r1.Y := ( _ext.YMax - _ext.YMin ) / _height ;
    r2.X := ( ex.XMax  - ex.XMin ) / wc ;
    r2.Y := ( ex.YMax  - ex.YMin ) / hc ;

    rct := RectF(
              ( ex.XMin - _ext.XMin) / r1.X ,
             -( ex.YMax - _ext.YMax) / r1.X ,
             -( ex.XMax - _ext.XMax) / r1.X ,
              ( ex.YMin - _ext.YMin) / r1.X
           ) ;

    if Abs( r1.X / r2.X ) > 1000 then begin
      Result := RectF( 0, 0, 0, 0 );
      exit ;
    end ;

    res := RectF(
             ( - rct.Left   * r1.X / r2.X ) - _rotpt.X,
             ( - rct.Top    * r1.X / r2.X ) - _rotpt.Y,
             ( + rct.Right  * r1.X / r2.X ) - _rotpt.X + wc ,
             ( + rct.Bottom * r1.X / r2.X ) - _rotpt.Y + hc
           ) ;

    Result := res ;
  end;

  function TGIS_ViewerWndHelper.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  var
    wc, hc : Double ;
    ex     : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      result := new TGIS_Point ;
    {$ENDIF}
    ex := ActualExtent ;
    if GisIsNoWorld( ex ) then
      ex := oVwr.VisibleExtent ;

    wc := oVwr.Parent.ControlCanvasWidth  ;
    hc := oVwr.Parent.ControlCanvasHeight ;

    Result.X :=  ex.XMin + ( ex.XMax - ex.XMin) / wc * _pt.X ;
    Result.Y :=  ex.YMax - ( ex.YMax - ex.YMin) / hc * _pt.Y ;
  end;

  function TGIS_ViewerWndHelper.MapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  var
    wc, hc : Double ;
    ex     : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      result := new TPoint( 0, 0 ) ;
    {$ENDIF}
    ex := ActualExtent ;
    if GisIsNoWorld( ex ) then
      ex := oVwr.VisibleExtent ;

    wc := oVwr.Parent.ControlCanvasWidth  ;
    hc := oVwr.Parent.ControlCanvasHeight ;

    Result.X := RoundS( ( _ptg.X - ex.XMin ) / ( ex.XMax - ex.XMin) * wc ) ;
    Result.Y := RoundS( ( ex.YMax - _ptg.Y ) / ( ex.YMax - ex.YMin) * hc ) ;
  end;

  function TGIS_ViewerWndHelper.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
  begin
    Result := MapToScreen( GisPoint2DFrom3D( _ptg ) ) ;
  end;

  function TGIS_ViewerWndHelper.ScreenToMap3D(
    const _pt  : TPoint
  ) : TGIS_Point3D ;
  begin
    Result := GisPoint3DFrom2D( ScreenToMap( _pt ) ) ;
  end;

  function TGIS_ViewerWndHelper.MapToScreenEx(
    const _pt  : TGIS_Point
  ) : TGIS_Point ;
  var
    wc, hc : Double ;
    ex     : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      result := new TGIS_Point ;
    {$ENDIF}
    ex := ActualExtent ;
    if GisIsNoWorld( ex ) then
      ex := oVwr.VisibleExtent ;

    wc := oVwr.Parent.ControlCanvasWidth  ;
    hc := oVwr.Parent.ControlCanvasHeight ;

    Result.X := ( _pt.X - ex.XMin ) / ( ex.XMax - ex.XMin) * wc ;
    Result.Y := ( ex.YMax - _pt.Y ) / ( ex.YMax - ex.YMin) * hc ;
  end;

  function TGIS_ViewerWndHelper.ScreenToMapEx(
    const _pt  : TGIS_Point
  ) : TGIS_Point ;
  var
    wc, hc : Double ;
    ex     : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      result := new TGIS_Point ;
    {$ENDIF}
    ex := ActualExtent ;
    if GisIsNoWorld( ex ) then
      ex := oVwr.VisibleExtent ;

    wc := oVwr.Parent.ControlCanvasWidth  ;
    hc := oVwr.Parent.ControlCanvasHeight ;

    Result.X :=  ex.XMin + ( ex.XMax - ex.XMin) / wc * _pt.X ;
    Result.Y :=  ex.YMax - ( ex.YMax - ex.YMin) / hc * _pt.Y ;
  end;

  function TGIS_ViewerWndHelper.MapToScreenRect(
    const _rct : TGIS_Extent
  ) : TRect ;
  var
    wc, hc : Double ;
    ex     : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      result := new TRect ;
    {$ENDIF}
    ex := ActualExtent ;
    if GisIsNoWorld( ex ) then
      ex := oVwr.VisibleExtent ;

    wc := oVwr.Parent.ControlCanvasWidth  ;
    hc := oVwr.Parent.ControlCanvasHeight ;

    Result := Rect(
                RoundS( ( _rct.XMin -   ex.XMin ) / ( ex.XMax - ex.XMin) * wc ),
                RoundS( (   ex.YMax - _rct.YMin ) / ( ex.YMax - ex.YMin) * hc ),
                RoundS( ( _rct.XMax -   ex.XMin ) / ( ex.XMax - ex.XMin) * wc ),
                RoundS( (   ex.YMax - _rct.YMax ) / ( ex.YMax - ex.YMin) * hc )
              ) ;
  end;

  function TGIS_ViewerWndHelper.ScreenToMapRect(
    const _rct : TRect
  ) : TGIS_Extent ;
  var
    wc, hc : Double ;
    ex     : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      result := new TGIS_Extent ;
    {$ENDIF}
    ex := ActualExtent ;
    if GisIsNoWorld( ex ) then
      ex := oVwr.VisibleExtent ;

    wc := oVwr.Parent.ControlCanvasWidth  ;
    hc := oVwr.Parent.ControlCanvasHeight ;

    Result.XMin :=  ex.XMin + ( ex.XMax - ex.XMin) / wc * _rct.Left   ;
    Result.YMin :=  ex.YMax - ( ex.YMax - ex.YMin) / hc * _rct.Top    ;
    Result.XMax :=  ex.XMin + ( ex.XMax - ex.XMin) / wc * _rct.Right  ;
    Result.YMax :=  ex.YMax - ( ex.YMax - ex.YMin) / hc * _rct.Bottom ;
  end;

end.
