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
  FMX Viewer control.
}

unit Lider.CG.GIS.FMX.GeoViewerWnd;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoViewerWnd"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  System.SysUtils,
  System.UIConsts,
  System.Generics.Defaults,
  System.Generics.Collections,
  FMX.Objects,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Ani,
  FMX.Styles,

  Lider.CG.GIS.FMX.GeoViewer3D,
  FMX.Graphics,
  FMX.Printer,
  Lider.CG.GIS.FMX.GeoPrinters,
  Lider.CG.GIS.FMX.GeoPrintManager,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoViewer,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoRendererAbstract,
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoViewerWndHelper,
  Lider.CG.GIS.GeoBasemapHelper,
  Lider.CG.GIS.GeoGraticuleHelper,

  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.FMX.GeoViewerBmp;

type
  {$IFDEF LEVEL_RX10_FMX}
    /// <summary>
    ///   Styled settings.
    /// </summary>
    TGIS_ViewerWndStyledSetting = (

      /// <summary>
      ///   Set background.
      /// </summary>
      Background
    ) ;

    /// <summary>
    ///   Set of styled settings.
    /// </summary>
    TGIS_ViewerWndStyledSettings = set of TGIS_ViewerWndStyledSetting ;
  {$ENDIF}

  {#gendoc:hide:GENSCR}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Class used to pass touch event context.
  /// </summary>
  TGIS_TouchContext = class
    private
      FTouches : TTouches     ;
      FAction  : TTouchAction ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_touches">
      ///   list of points
      /// </param>
      /// <param name="_action">
      ///   type of action: Down, Move or Up.
      /// </param>
      constructor Create( const _touches : TTouches ;
                          const _action  : TTouchAction
                        ) ;
    public
      /// <summary>
      ///   List of points
      /// </summary>
      property Touches  : TTouches     read FTouches ;
      /// <summary>
      ///   Type of action: Down, Move or Up.
      /// </summary>
      property Action   : TTouchAction read FAction  ;
  end ;

  /// <summary>
  ///   Main visual control responsible for map presentation on window.
  /// </summary>
  /// <remarks>
  ///   For bitmap output see TGIS_ViewerBmp class.
  /// </remarks>
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_ViewerWnd = class( TStyledControl, IGIS_Viewer, IGIS_ViewerParent,
                          IGIS_ViewerWnd )
    private
      iWidth            : Integer ;
      iHeight           : Integer ;
      oVwr              : TGIS_Viewer;
      oRenderer         : TGIS_RendererAbstract ;
      oWndHelper        : TGIS_ViewerWndHelper ;
      oBasemapHelper    : TGIS_BasemapHelper ;
      oGraticuleHelper  : TGIS_GraticuleHelper ;
      oGraticule        : TGIS_Graticule ;
      oZoomingRect      : TRectangle ;
      oProgressRect     : TRectangle ;
      oProgressRectAni  : TBitmapListAnimation ;
      oDraggingTrack    : TLine ;
      iCanvasScale      : Single ;
      oLastHourglas     : Int64 ;

      oFullCache        : FMX.Graphics.TBitmap ;
      oPaintCacheTmp    : FMX.Graphics.TBitmap ;

      oParentForm       : TCustomForm ;
      oOnTouchOriginal  : TTouchEvent ;
      arTouches         : TTouches ;

      oStlWnd           : TControl ;
      oStlCtl           : TControl ;
      oStyledCtlColor   : TAlphaColor ;

    private
      bInUpdate         : Boolean ;
      bInFlash          : Boolean ;

    private
      mouseBlocked      : Boolean ;
      mousePos          : TPoint  ;
      mousePosOld       : TPoint  ;
      mousePosLast      : TPoint  ;
      zoomDistance      : Integer ;
      zoomGesture       : Boolean ;
      panGesture        : Boolean ;
      gestureTouches    : Integer ;

    private
        /// <summary>
        ///   If True, then the screen will be centered on click, edit, etc.
        /// </summary>
        FAutoCenter : Boolean ;

        /// <summary>
        ///   Mode of viewer (dragging, selecting ...).
        /// </summary>
        FMode : TGIS_ViewerMode ;

        /// <summary>
        ///   Active button for Mode operations (dragging, selecting ...).
        /// </summary>
        FModeMouseButton : TMouseButton ;

        /// <summary>
        ///   Used to force rendering in a different scale then actual.
        /// </summary>
        FTemporaryScaleInternal : Double ;

        /// <summary>
        ///   Minimal size of the zooming rectangle.
        /// </summary>
        FMinZoomSize : Integer ;

        /// <summary>
        ///   Cached rendering
        /// </summary>
        FCacheBitmap : TBitmap ;
        bCacheBitmapNew : Boolean ;

        /// <summary>
        ///   Bitmap use upon progressive display
        /// </summary>
        FProgressBitmap : TBitmap ;

        FDrawContext  : TGIS_RendererContext ;

        FTopContext   : TGIS_RendererContext ;

        FFlashTransparency : Integer ;

        FFlashContext : TGIS_RendererContext ;

        /// <summary>
        ///   State of dragging.
        /// </summary>
        FDragging   : Boolean ;

        /// <summary>
        ///   State of zooming.
        /// </summary>
        FZooming    : Boolean ;

        /// <summary>
        ///   State of editing.
        /// </summary>
        FEditing    : Boolean ;

        /// <summary>
        ///   Background color.
        /// </summary>
        FBackgroundColor : TAlphaColor ;

        /// <summary>
        ///   Cursor for TGIS_ViewerMode.Select mode.
        /// </summary>
        FCursorForSelect : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_ViewerMode.Drag mode.
        /// </summary>
        FCursorForDrag : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_ViewerMode.Zoom mode.
        /// </summary>
        FCursorForZoom : TCursor ;

        /// <summary>
        ///   Cursor for gisZommEx mode.
        /// </summary>
        FCursorForZoomEx : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_ViewerMode.Edit mode.
        /// </summary>
        FCursorForEdit : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_ViewerMode.UserDefined mode.
        /// </summary>
        FCursorForUserDefined : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.CameraXYZ.
        /// </summary>
        FCursorForCameraPosition : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.CameraRotation.
        /// </summary>
        FCursorForCameraRotation : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.CameraXYZ.
        /// </summary>
        FCursorForCameraXYZ : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.CameraXY.
        /// </summary>
        FCursorForCameraXY : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.Zoom.
        /// </summary>
        FCursorForCameraZoom : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.SunPosition.
        /// </summary>
        FCursorForSunPosition : TCursor ;

        /// <summary>
        ///   Cursor for TGIS_Viewer3DMode.Select.
        /// </summary>
        FCursorFor3DSelect : TCursor ;

        /// <summary>
        ///   Viewer 3D object. Nil if 3D mode is off.
        /// </summary>
        [weak]
        FViewer3D : IGIS_Viewer3D ;

        {$IFDEF LEVEL_RX10_FMX}
          /// <summary>
          ///   Styled settings.
          /// </summary>
          FStyledSettings   : TGIS_ViewerWndStyledSettings ;
        {$ENDIF}

        /// <summary>
        ///   If true then all labels, selections and topmost layers will be
        ///   visible upon scaling, dragging etc.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Paint.NavigateFeedback.FullCache'.
        ///   </para>
        ///   <para>
        ///     Default value is True.
        ///   </para>
        /// </remarks>
        metPaintNavigateFeedbackFullCache : Boolean ;

        /// <summary>
        ///   Long term operations will be drawn as semitransparent if value is
        ///   between 0..100.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Paint.Progressive.Transparency'.
        ///   </para>
        ///   <para>
        ///     Default value is 60.
        ///   </para>
        /// </remarks>
        metPaintProgressiveTransparency : Integer ;

        /// <summary>
        ///   If true then long term operations will be drawn behind last cached
        ///   view.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Paint.Progressive.FullCache'.
        ///   </para>
        ///   <para>
        ///     Default value is True.
        ///   </para>
        /// </remarks>
        metPaintProgressiveFullCache : Boolean ;

        /// <summary>
        ///   If true then labels will be drawn always on top.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Paint.LabelsOnTop'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metPaintLabelsOnTop : Boolean ;

        /// <summary>
        ///   If true then labels will be drawn always on top upon drawing topmost layers.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Paint.Topmost.LabelsOnTop'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metPaintTopmostLabelsOnTop : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for Mode.Drag.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Mode.Drag.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metModeDragAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for Mode.Zoom.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Mode.Zoom.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metModeZoomAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for Mode.Select.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Mode.Select.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is True.
        ///   </para>
        /// </remarks>
        metModeSelectAllGestures : Boolean ;

        /// <summary>
        ///   If true then zoom and 2-finger pan gestures are available for Mode.Edit.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Mode.Edit.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is True.
        ///   </para>
        /// </remarks>
        metModeEditAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.CameraPosition.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.CameraPosition.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metViewer3DModeCameraPositionAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.CameraXYZ.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.CameraXYZ.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metViewer3DModeCameraXYZAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.CameraXY.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.CameraXY.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metViewer3DModeCameraXYAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.CameraRotation.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.CameraRotation.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metViewer3DModeCameraRotationAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.SunPosition.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.SunPosition.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metViewer3DModeSunPositionAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.Zoom.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.Zoom.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is False.
        ///   </para>
        /// </remarks>
        metViewer3DModeZoomAllGestures : Boolean ;

        /// <summary>
        ///   If true then all gestures are available for TGIS_Viewer3DMode.Select.
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Available only via metadata:
        ///     'TGIS_ViewerWnd.Viewer3D.Mode.Select.AllGestures'.
        ///   </para>
        ///   <para>
        ///     Default value is True.
        ///   </para>
        /// </remarks>
        metViewer3DModeSelectAllGestures : Boolean ;

        /// <summary>
        ///   If True HiRes should be avtivated if possible.
        /// </summary>
        FHiRes : Boolean ;

        /// <summary>
        ///   Stored 2D mode context.
        /// </summary>
        oldMode2D : TGIS_ViewerMode ;

        {$IFDEF GIS_MOBILE}
          /// <summary>
          ///   Stored 2D mode clip children contex.
           ///   To bypas bug in cliping TViewport3D
          /// </summary>
          oldClipChildren : Boolean ;
        {$ENDIF}

    private // properties - events

        /// <summary>
        ///   Handler for ModeChange event.
        /// </summary>
        FOnModeChange    : TNotifyEvent ;

        /// <summary>
        ///   Handler for EditorChange event.
        /// </summary>
        FOnEditorChange : TNotifyEvent ;

        /// <summary>
        ///   BeforePaint event. Will be fired before any Paint operation.
        /// </summary>
        FOnBeforePaint  : TGIS_PaintEvent ;

        /// <summary>
        ///   BeforePaintRenderer event. Will be fired before any Paint operation.
        /// </summary>
        FOnBeforePaintRenderer : TGIS_RendererEvent ;

        /// <summary>
        ///   AfterPaint event. Will be fired after any Paint operation.
        /// </summary>
        FOnAfterPaint   : TGIS_PaintEvent ;

        /// <summary>
        ///   AfterPaintRenderer event. Will be fired after any Paint operation.
        /// </summary>
        FOnAfterPaintRenderer : TGIS_RendererEvent ;

        /// <summary>
        ///   PaintExtra event. Will be fired after real draw.
        /// </summary>
        FOnPaintExtra   : TGIS_RendererEvent ;

        /// <summary>
        ///   BeforeUpdate event. Will be fired before Update operation.
        /// </summary>
        FOnBeforeUpdate : TGIS_RendererEvent ;

        /// <summary>
        ///   UpdateEx event. Will be fired for Update operation.
        /// </summary>
        FOnUpdate       : TGIS_RendererEvent ;

        /// <summary>
        ///   AfterUpdate event. Will be fired after Update operation.
        /// </summary>
        FOnAfterUpdate  : TGIS_RendererEvent ;

        /// <summary>
        ///   OnTapSimple event. Will be fired upon press down/up.
        /// </summary>
        FOnTapSimple    : TMouseEvent ;

        /// <summary>
        ///   OnTapLong event. Will be fired upon longer press down.
        /// </summary>
        FOnTapLong      : TMouseEvent ;

        /// <summary>
        ///   OnTapDouble event. Will be fired upon double press down/up.
        /// </summary>
        FOnTapDouble    : TMouseEvent ;

    private // class private methods
      procedure ensureCursor     ;
      procedure ensure3DCursor   ;
      procedure readGesturesMetadata ;
      function  gesturePanEnabled: Boolean ;
      function  gestureZoomEnabled
                                 : Boolean ;
      function  gesture3DPanEnabled
                                 : Boolean ;
      function  gesture3DZoomEnabled
                                 : Boolean ;
      procedure doTouch          (       _sender   : TObject      ;
                                   const _touches  : TTouches     ;
                                   const _action   : TTouchAction
                                 ) ;
      procedure doGestureProc    (       _sender   : TObject      ;
                                   const _info     : TGestureEventInfo ;
                                   var   _handled  : Boolean
                                 ) ;
      procedure resetMousePos    ( const _x        : Integer      ;
                                   const _y        : Integer
                                 ) ;
      procedure updateExecute    (       _sender   : TObject      ;
                                         _ctx      : TGIS_ViewerWndHelperRun
                                 ) ;
      procedure updateSynchronize(       _sender   : TObject      ;
                                         _ctx      : TGIS_ViewerWndHelperRun;
                                         _final    : Boolean
                                 ) ;
      procedure updateTap        (       _sender   : TObject
                                 ) ;

      procedure doGraticule      (       _sender   : TObject      ;
                                         _renderer : TGIS_RendererAbstract ;
                                         _mode     : TGIS_DrawMode
                                 ) ;

      procedure doPrintBmp       ( const _bitmap   : T_FMXBitmap  ;
                                   const _full     : Boolean
                                 ) ;

    private // mouse handling for zooming rectangle
      procedure doZoomingRectMouseUp
                                 (       _sender   : TObject      ;
                                         _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ;
      procedure doZoomingRectMouseMove
                                 (       _sender   : TObject      ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ;

    protected // IGIS_Viewer property access routines
      function  fget_BigExtent          : TGIS_Extent ;
      function  fget_BigExtentMargin    : Integer    ;
      procedure fset_BigExtentMargin    ( const _value : Integer     ) ;
      function  fget_KeepScale          : Boolean ;
      procedure fset_KeepScale          ( const _value : Boolean     ) ;
      function  fget_BusyLevel          : Integer    ;
      function  fget_BusyText           : String     ;
      function  fget_Center             : TGIS_Point ;
      procedure fset_Center             ( const _value : TGIS_Point  ) ;
      function  fget_CenterPtg          : TGIS_Point ;
      procedure fset_CenterPtg          ( const _value : TGIS_Point  ) ;
      function  fget_Color              : TGIS_Color ;
      procedure fset_Color              ( const _value : TGIS_Color  ) ;
      function  fget_Copyright          : String     ;
      function  fget_CS                 : TGIS_CSCoordinateSystem ;
      procedure fset_CS                 ( const _value : TGIS_CSCoordinateSystem ) ;
      function  fget_CustomData         : TGIS_StringList ;
      function  fget_CustomPPI          : Integer ;
      procedure fset_CustomPPI          ( const _value : Integer     ) ;
      function  fget_Editor             : IGIS_Editor ;
      procedure fset_Editor             ( const _value : IGIS_Editor ) ;
      function  fget_Extent             : TGIS_Extent ;
      function  fget_FileCopyrights     : String      ;
      function  fget_FontScale          : Integer ;
      procedure fset_FontScale          ( const _value : Integer     ) ;
      function  fget_FullDrawExtent     : TGIS_Extent ;
      function  fget_Hierarchy          : IGIS_HierarchyManager ;
      function  fget_IncrementalPaint   : Boolean     ;
      procedure fset_IncrementalPaint   ( const _value : Boolean     ) ;
      function  fget_InPaint            : Boolean     ;
      function  fget_IsBusy             : Boolean     ;
      function  fget_IsEmpty            : Boolean     ;
      function  fget_IsLocked           : Boolean     ;
      function  fget_IsTopmost          : Boolean     ;
      function  fget_Items              : TGIS_LayerAbstractList ;
      function  fget_LabelsReg          : TGIS_LabelsAreaAbstract ;
      function  fget_Level              : Double ;
      procedure fset_Level              ( const _value : Double      ) ;
      function  fget_MultiUserMode      : TGIS_MultiUser ;
      procedure fset_MultiUserMode      ( const _value : TGIS_MultiUser ) ;
      function  fget_OverlappedExtentMargin
                                        : Integer     ;
      procedure fset_OverlappedExtentMargin
                                        ( const _value : Integer     ) ;
      function  fget_PPI                : Integer ;
      function  fget_ProjectFile        : TGIS_ConfigAbstract ;
      procedure fset_ProjectFile        ( const _value : TGIS_ConfigAbstract) ;
      function  fget_ProjectName        : String      ;
      function  fget_DelayedUpdate      : Integer     ;
      procedure fset_DelayedUpdate      ( const _value : Integer     ) ;
      function  fget_ProgressiveUpdate  : Integer     ;
      procedure fset_ProgressiveUpdate  ( const _value : Integer     ) ;
      function  fget_RestrictedDrag     : Boolean     ;
      procedure fset_RestrictedDrag     ( const _value : Boolean     ) ;
      function  fget_RestrictedExtent   : TGIS_Extent ;
      procedure fset_RestrictedExtent   ( const _value : TGIS_Extent ) ;
      function  fget_RotationAngle      : Double      ;
      procedure fset_RotationAngle      ( const _value : Double      ) ;
      function  fget_RotationPoint      : TGIS_Point  ;
      procedure fset_RotationPoint      ( const _value : TGIS_Point  ) ;
      function  fget_ScaleAsFloat       : Double      ;
      procedure fset_ScaleAsFloat       ( const _value : Double      ) ;
      function  fget_ScaleAsText        : String      ;
      procedure fset_ScaleAsText        ( const _value : String      ) ;
      function  fget_SelectionGisColor  : TGIS_Color  ;
      procedure fset_SelectionGisColor  ( const _value : TGIS_Color  ) ;
      function  fget_SelectionOutlineOnly: Boolean    ;
      procedure fset_SelectionOutlineOnly( const _value : Boolean    ) ;
      function  fget_SelectionTransparency : Integer  ;
      procedure fset_SelectionTransparency( const _value : Integer   ) ;
      function  fget_SelectionWidth     : Integer     ;
      procedure fset_SelectionWidth     ( const _value : Integer     ) ;
      function  fget_SystemPPI          : Integer ;
      function  fget_ViewerParent       : IGIS_ViewerParent ;
      function  fget_Viewport           : TGIS_Point  ;
      procedure fset_Viewport           ( const _value : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
      procedure fset_VisibleExtent      ( const _value : TGIS_Extent ) ;
      function  fget_UseAnimations      : Boolean     ;
      procedure fset_UseAnimations      ( const _value : Boolean     ) ;
      function  fget_UseRTree           : Boolean     ;
      procedure fset_UseRTree           ( const _value : Boolean     ) ;
      function  fget_Zoom               : Double      ;
      procedure fset_Zoom               ( const _value : Double      ) ;
      function  fget_ZoomEx             : Double      ;
      procedure fset_ZoomEx             ( const _value : Double      ) ;
      function  fget_UponDestroy        : Boolean     ;
      function  fget_TemporaryScaleInternal: Double      ;
      procedure fset_TemporaryScaleInternal( const _value : Double   ) ;
      function  fget_TemporaryVisibleExtent: TGIS_Extent ;
      procedure fset_TemporaryVisibleExtent( const _value : TGIS_Extent ) ;

    protected // TGIS_ViewerWnd property access routines
      function  fget_BackgroundColor        : TAlphaColor ;
      procedure fset_BackgroundColor        ( const _value : TAlphaColor ) ;
      function  fget_ActiveBackgroundColor  : TAlphaColor ;
      function  fget_HiRes                  : Boolean ;
      procedure fset_HiRes                  ( const _value : Boolean ) ;
      procedure fset_Mode                   ( const _value : TGIS_ViewerMode ) ;
      procedure fset_Renderer               ( const _value :
                                                       TGIS_RendererAbstract ) ;
      function  fget_SelectionColor         : TAlphaColor ;
      procedure fset_SelectionColor         ( const _value : TAlphaColor  ) ;
      function  fget_Viewer3D : IGIS_Viewer3D ;
      procedure fset_Viewer3D( const _viewer : IGIS_Viewer3D ) ;
      function  fget_View3D : Boolean ;
      procedure fset_View3D( const _value : Boolean ) ;

    protected // IGIS_Viewer event properties
      function  fget_BusyEvent               : TGIS_BusyEvent ;
      procedure fset_BusyEvent               ( const _value : TGIS_BusyEvent ) ;
      function  fget_HelpEvent               : TGIS_HelpEvent ;
      procedure fset_HelpEvent               ( const _value : TGIS_HelpEvent  ) ;
      function  fget_EditorSnapPointEvent    : TGIS_EditorSnapPointEvent ;
      procedure fset_EditorSnapPointEvent    ( const _value :
                                                   TGIS_EditorSnapPointEvent ) ;
      function  fget_EditorPointChangeEvent   : TGIS_EditorPointChangeEvent ;
      procedure fset_EditorPointChangeEvent   ( const _value :
                                                TGIS_EditorPointChangeEvent    ) ;
      function  fget_EditorPointMoveEvent   : TGIS_EditorPointMoveEvent ;
      procedure fset_EditorPointMoveEvent   ( const _value :
                                                TGIS_EditorPointMoveEvent    ) ;
      function  fget_ExtentChangeEvent       : TNotifyEvent ;
      procedure fset_ExtentChangeEvent       ( const _value : TNotifyEvent ) ;
      function  fget_LayerAddEvent           : TGIS_LayerEvent ;
      procedure fset_LayerAddEvent           ( const _value : TGIS_LayerEvent ) ;
      function  fget_LayerDeleteEvent        : TGIS_LayerEvent ;
      procedure fset_LayerDeleteEvent        ( const _value : TGIS_LayerEvent ) ;
      function  fget_PaintExceptionEvent     : TGIS_PaintExceptionEvent ;
      procedure fset_PaintExceptionEvent     ( const _value :
                                                    TGIS_PaintExceptionEvent ) ;
      function  fget_PasswordEvent           : TGIS_TemplateProducerEvent ;
      procedure fset_PasswordEvent           ( const _value :
                                                  TGIS_TemplateProducerEvent ) ;
      function  fget_ProjectCloseEvent       : TNotifyEvent ;
      procedure fset_ProjectCloseEvent       ( const _value : TNotifyEvent ) ;
      function  fget_ProjectOpenEvent        : TNotifyEvent ;
      procedure fset_ProjectOpenEvent        ( const _value : TNotifyEvent ) ;
      function  fget_VisibleExtentChangeEvent: TNotifyEvent ;
      procedure fset_VisibleExtentChangeEvent( const _value : TNotifyEvent ) ;
      function  fget_ZoomChangeEvent         : TNotifyEvent ;
      procedure fset_ZoomChangeEvent         ( const _value : TNotifyEvent ) ;

    protected  // event methods overridden
      {#gendoc:hide}
      procedure MouseDown        (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ; override;
      {#gendoc:hide}
      procedure MouseUp          (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ; override;
      {#gendoc:hide}
      procedure MouseMove        (       _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ; override;

      {#gendoc:hide}
      procedure ParentChanged    ; override;

      {#gendoc:hide}
      procedure Paint            ; override;

      {#gendoc:hide}
      procedure Resize           ; override;

      {$IFDEF LEVEL_RX10_FMX}
        {#gendoc:hide}
        procedure DoStyleChanged ; override;
      {$ENDIF}

    public // extra mouse handlers

      /// <summary>
      ///   Execute operation equal to the reaction to a mouse down event.
      /// </summary>
      /// <param name="_button">
      ///   active mouse button
      /// </param>
      /// <param name="_shift">
      ///   state of modifiers keys
      /// </param>
      /// <param name="_x">
      ///   X mouse location
      /// </param>
      /// <param name="_y">
      ///   Y mouse location
      /// </param>
      procedure DoMouseDown      (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Integer      ;
                                         _y        : Integer
                                 ) ;
      /// <summary>
      ///   Execute operation equal to the reaction to a mouse up event.
      /// </summary>
      /// <param name="_button">
      ///   active mouse button
      /// </param>
      /// <param name="_shift">
      ///   state of modifiers keys
      /// </param>
      /// <param name="_x">
      ///   X mouse location
      /// </param>
      /// <param name="_y">
      ///   Y mouse location
      /// </param>
      procedure DoMouseUp        (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Integer      ;
                                         _y        : Integer
                                 ) ;
      /// <summary>
      ///   Execute operation equal to the reaction to a mouse move event.
      /// </summary>
      /// <param name="_shift">
      ///   state of modifiers keys
      /// </param>
      /// <param name="_x">
      ///   X mouse location
      /// </param>
      /// <param name="_y">
      ///   Y mouse location
      /// </param>
      procedure DoMouseMove      (       _shift    : TShiftState  ;
                                         _x        : Integer      ;
                                         _y        : Integer
                                 ) ;

    protected  // new event methods

      /// <summary>
      ///   Call action equivalent to single tap.
      /// </summary>
      /// <param name="_button">
      ///   mouse buttons state
      /// </param>
      /// <param name="_shift">
      ///   keyboard shift keys state
      /// </param>
      /// <param name="_x">
      ///   tap position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   tap position in pixels relative to form
      /// </param>
      procedure TapSingle        (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ; virtual;

      /// <summary>
      ///   Call action equivalent to double tap.
      /// </summary>
      /// <param name="_button">
      ///   mouse buttons state
      /// </param>
      /// <param name="_shift">
      ///   keyboard shift keys state
      /// </param>
      /// <param name="_x">
      ///   tap position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   tap position in pixels relative to form
      /// </param>
      procedure TapDouble        (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ; virtual;

      /// <summary>
      ///   Call action equivalent to long tap (tap and keep tapped).
      /// </summary>
      /// <param name="_button">
      ///   mouse buttons state
      /// </param>
      /// <param name="_shift">
      ///   keyboard shift keys state
      /// </param>
      /// <param name="_x">
      ///   tap position in pixels relative to form
      /// </param>
      /// <param name="_y">
      ///   tap position in pixels relative to form
      /// </param>
      procedure TapLong          (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Single       ;
                                         _y        : Single
                                 ) ; virtual;

    public
      /// <summary>
      ///   Create the control.
      /// </summary>
      /// <param name="_owner">
      ///   control owner
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create         ( _owner         : TComponent
                                 ) ; override;

      /// <summary>
      ///   Destroy the control.
      /// </summary>
      destructor Destroy         ; override;

    public // IGIS_Viewer public methods

      /// <inheritdoc from="IGIS_Viewer"/>
      function ChangeHash        : Int64 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Subscribe        ( const _control : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnSubscribe      ( const _control : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure NotifySubscribers( const _event   : Integer ;
                                   const _context : TObject
                                 ) ;

      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function NotifyPaintException(
                                   const _message   : String   ;
                                   const _exception : Exception
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Lock             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ( const _redraw : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Interrupt        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  HourglassActive  : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassPrepare ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassRelease ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  HourglassShake   : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassRestart ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyPrepare      (       _sender  : TObject ;
                                         _text    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyRelease      (       _sender  : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyShake        (       _sender  : TObject ;
                                         _pos     : Int64 ;
                                         _end     : Int64 ;
                                   var   _abort   : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RaiseBusyEvent   (       _sender  : TObject ;
                                         _pos     : Int64 ;
                                         _end     : Int64 ;
                                   var   _abort   : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RaiseHelpEvent   (       _sender  : TObject ;
                                         _name    : String
                                 ) ;

      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedBusyEvent: TGIS_BusyEvent;

      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedHelpEvent: TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  StorePaintState  : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RestorePaintState( var _state     : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BeginPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure EndPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  SynchronizePaint ( const _interrupt : Boolean
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function ReParent          ( const _parent : IGIS_ViewerParent
                                 ) : IGIS_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function AttachLayer       ( const _layer : TGIS_LayerAbstract
                                 ) : IGIS_Viewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path    : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path    : String ;
                                   const _strict  : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure OpenEx           ( const _configFile : TGIS_ConfigAbstract ;
                                   const _path       : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure OpenEx           ( const _configFile : TGIS_ConfigAbstract ;
                                   const _path       : String              ;
                                   const _strict     : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Close            ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReadConfig       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RereadConfig     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WriteConfig      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Add              ( const _layer   : TGIS_LayerAbstract
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Get              ( const _name    : String
                                 ) : TGIS_LayerAbstract ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Delete           ( const _name    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure AddHierarchy     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Draw             ( const _renderer : TObject     ;
                                   const _mode     : TGIS_DrawMode
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetGrid          ( const _extent  : TGIS_Extent  ;
                                   const _grid    : TGIS_GridArray
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RevertAll        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProject      ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProject      ( const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAs    ( const _path         : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAs    ( const _path         : String          ;
                                   const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAsEx  ( const _configFile   : TGIS_ConfigAbstract ;
                                   const _path         : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAsEx  ( const _configFile   : TGIS_ConfigAbstract ;
                                   const _path         : String              ;
                                   const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveData         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveAll          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MustSave         : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MarkModified     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RecalcExtent     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Reposition       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent  : TGIS_Extent
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent : TGIS_Extent ;
                                   const _deep   : Boolean
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateWholeMap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateTopmost ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateBasemap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateSelection ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateEditor ( const _final   : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  FullExtentZoom   : Double ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure FullExtent       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _ptg     : TGIS_Point ;
                                   const _prec    : Double
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _ptg     : TGIS_Point ;
                                   const _prec    : Double     ;
                                   const _visible : Boolean
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _pt      : TPoint     ;
                                   const _prec    : Integer
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  LocateEx            ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double    ;
                                      const _visible : Boolean
                                    ) : TGIS_ShapeAbstractList ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen      ( const _ptg     : TGIS_Point
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen3D    ( const _ptg     : TGIS_Point3D
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap      ( const _pt      : TPoint
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap3D    ( const _pt      : TPoint
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenEx    ( const _pt      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapEx    ( const _pt      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenRect  ( const _rct     : TGIS_Extent
                                 ) : TRect ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapRect  ( const _rct     : TRect
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  PixelsToTwips    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPixels    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPoints    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewport     ( var   _dx, _dy : Integer
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x , _y  : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure CenterViewport   ( const _ptg     : TGIS_Point
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKT       ( const _wkt     : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByEPSG      ( const _epsg    : Integer
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKTFile   ( const _path    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint     ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint   ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint3D   ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RotatedPoint3D_ref(
                                   var   _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint3D ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnrotatedPoint3D_ref(
                                   var  _ptg      : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedExtent    ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedExtent  ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetRenderContext : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForBackgroundProcesses ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy   (       _sender  : TObject ;
                                   const _proc    : TGIS_WaitForNotBusyProc
                                 ) ;

    public // IGIS_ViewerParent methods

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlClose           ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlDrawTexture     (       _bmp     : TObject     ;
                                         const _extent  : TGIS_Extent ;
                                         const _ppi     : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlDrawTexture     (       _bmp      : TObject     ;
                                         const _layer    : TGIS_LayerAbstract ;
                                         const _extent   : TGIS_Extent ;
                                         const _ppi      : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlRenderer        : TObject ;


      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlFlash           ( const _times   : Integer ;
                                         const _delay   : Integer
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlSystemPPI       : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlPPI             : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlCanvasScale     : Single ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlCanvasHeight    : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlCanvasWidth     : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlRepaint         ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlProcessMessages ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function ControlUpdateSynchronize( const _interrupt : Boolean
                                       ) : Boolean ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateWholeMap  ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateProgressive  ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateTopmost   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateBasemap   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateSelection ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateEditor    ( const _final : Boolean
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlHourglassShow   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlHourglassHide   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlHourglassShake  : Boolean ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlSet3DMode       ( const _mode : TGIS_Viewer3DMode
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlRaiseEditorChangeEvent(
                                               _sender  : TObject
                                       ) ;
      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlAutoCenterViewport      ( const _dx, _dy : Double ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  SetViewer ( const _viewer : TObject ) : TObject ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  GetViewer : TObject ;

    public // IGIS_ViewerWnd public methods

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp       ( var   _bmp   : TGIS_Bitmap
                               ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp       ( var   _bmp   : TGIS_Bitmap ;
                                 const _full  : Boolean
                               ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print          ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print          ( _printer     : IGIS_Printer
                               ) ; overload ;


      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure ZoomBy         ( const _zm    : Double  ;
                                 const _x     : Integer ;
                                 const _y     : Integer
                               ) ; overload ;
    public

      /// <summary>
      ///   Return a copy of last cache bitmap.
      /// </summary>
      /// <desc>
      ///   The returned bitmap object should be freed by user.
      /// </desc>
      /// <returns>
      ///   cache bitmap or nil
      /// </returns>
      function  GetCacheBitmap   : FMX.Graphics.TBitmap ;

      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      procedure PrintBmp         ( var   _bmp   : {$IFDEF LEVEL_XE5_FMX}
                                                    FMX.Graphics.TBitmap
                                                  {$ELSE}
                                                    FMX.Types.TBitmap
                                                  {$ENDIF}
                                 ) ; overload ; virtual ;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      /// <param name="_full">
      ///   if yes, all paint events are triggered
      /// </param>
      procedure PrintBmp         ( var   _bmp   : {$IFDEF LEVEL_XE5_FMX}
                                                    FMX.Graphics.TBitmap ;
                                                  {$ELSE}
                                                    FMX.Types.TBitmap ;
                                                  {$ENDIF}
                                   const _full    : Boolean
                                 ) ; overload ; virtual ;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Print the current view to the clipboard.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNIMPLEMENTED if not supported.
      /// </exception>
      /// <remarks>
      ///   Available only on RAD Studio 10.1 Seattle or newer.
      /// </remarks>
      procedure PrintClipboard   ; overload ; virtual;

      /// <summary>
      ///   Print the current view to the clipboard.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNIMPLEMENTED if not supported.
      /// </exception>
      /// <remarks>
      ///   Available only on RAD Studio 10.1 Seattle or newer.
      /// </remarks>
      /// <param name="_full">
      ///   if yes, all paint events are triggered
      /// </param>
      procedure PrintClipboard   ( const _full    : Boolean
                                 ) ; overload ; virtual;

    public // IGIS_Viewer public properties

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtent    : TGIS_Extent      read  fget_BigExtent      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyLevel    : Integer          read  fget_BusyLevel      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyText     : String           read  fget_BusyText       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Center       : TGIS_Point       read  fget_Center
                                               write fset_Center         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CenterPtg    : TGIS_Point       read  fget_CenterPtg
                                               write fset_CenterPtg      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Copyright    : String           read  fget_Copyright      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CS           : TGIS_CSCoordinateSystem
                                               read  fget_CS
                                               write fset_CS             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomData : TGIS_StringList   read  fget_CustomData ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Editor : IGIS_Editor            read  fget_Editor
                                               write fset_Editor         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Extent       : TGIS_Extent      read  fget_Extent         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FileCopyrights : String         read  fget_FileCopyrights ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FullDrawExtent : TGIS_Extent    read  fget_FullDrawExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Hierarchy : IGIS_HierarchyManager
                                               read  fget_Hierarchy      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property InPaint      : Boolean          read  fget_InPaint        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsBusy       : Boolean          read  fget_IsBusy         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsEmpty      : Boolean          read  fget_IsEmpty        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsLocked     : Boolean          read  fget_IsLocked       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsTopmost    : Boolean          read  fget_IsTopmost      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Items        : TGIS_LayerAbstractList
                                               read  fget_Items          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property LabelsReg : TGIS_LabelsAreaAbstract
                                               read fget_LabelsReg       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Level : Double                  read  fget_Level
                                               write fset_Level          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property OverlappedExtentMargin : Integer
                                         read  fget_OverlappedExtentMargin
                                         write fset_OverlappedExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property PPI          : Integer      read fget_PPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectFile  : TGIS_ConfigAbstract
                                           read  fget_ProjectFile    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectName  : String       read  fget_ProjectName    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedExtent : TGIS_Extent  read  fget_RestrictedExtent
                                               write fset_RestrictedExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationAngleEx : Double        read  fget_RotationAngle
                                               write fset_RotationAngle  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationPoint : TGIS_Point      read  fget_RotationPoint
                                               write fset_RotationPoint  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Scale        : Double           read  fget_ScaleAsFloat
                                               write fset_ScaleAsFloat   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsFloat : Double           read  fget_ScaleAsFloat
                                               write fset_ScaleAsFloat   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsText  : String           read  fget_ScaleAsText
                                               write fset_ScaleAsText    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionGisColor : TGIS_Color  read  fget_SelectionGisColor
                                               write fset_SelectionGisColor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SystemPPI    : Integer          read fget_SystemPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryVisibleExtent : TGIS_Extent
                                               read  fget_TemporaryVisibleExtent
                                               write fset_TemporaryVisibleExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParent : IGIS_ViewerParent
                                               read  fget_ViewerParent   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Viewport     : TGIS_Point       read  fget_Viewport
                                               write fset_Viewport       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property VisibleExtent : TGIS_Extent     read  fget_VisibleExtent
                                               write fset_VisibleExtent  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Zoom         : Double           read  fget_Zoom
                                               write fset_Zoom           ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ZoomEx       : Double           read  fget_ZoomEx
                                               write fset_ZoomEx         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UponDestroy   : Boolean          read  fget_UponDestroy    ;

    public // TGIS_ViewerWnd public properties

      /// <summary>
      ///   Currently operating renderer. By assigning a new a different
      ///   rendering engine can be cooem.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Do not free renderer directly - control maintains a renderer
      ///     life time.
      ///   </note>
      /// </remarks>
      {#ownership:set:release}
      property Renderer : TGIS_RendererAbstract
                                               read  oRenderer
                                               write fset_Renderer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryScaleInternal : Double read  FTemporaryScaleInternal
                                               write FTemporaryScaleInternal ;
                                               //default 0;

      /// <summary>
      ///   3D Viewer object. Nil if 3D mode is off.
      /// </summary>
      property Viewer3D : IGIS_Viewer3D        read  fget_Viewer3D
                                               write fset_Viewer3D ;

      /// <summary>
      ///   3D state; If set to true them Viewer is in a 3D mode.
      /// </summary>
      property View3D   : Boolean              read  fget_View3D
                                               write fset_View3D  ;

      /// <summary>
      ///   Graticule object. Set Graticule.Enabled to turn on/off.
      /// </summary>
      property Graticule : TGIS_Graticule      read  oGraticule ;

      /// <summary>
      ///   Active Background color. Computed also for styles.
      /// </summary>
      property ActiveBackgroundColor : TAlphaColor
                                               read fget_ActiveBackgroundColor ;

    published // TGIS_ViewerWnd published properties

      /// <summary>
      ///   Cursor for TGIS_ViewerMode.Drag mode (dragging a map within the
      ///   component window).
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForDrag : TCursor         read  FCursorForDrag
                                               write FCursorForDrag ;

      /// <summary>
      ///   Cursor for TGIS_ViewerMode.Edit mode (editing a feature on a map).
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForEdit : TCursor         read  FCursorForEdit
                                               write FCursorForEdit ;

      /// <summary>
      ///   Cursor for TGIS_ViewerMode.Select mode (selecting items on a map).
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForSelect : TCursor       read  FCursorForSelect
                                               write FCursorForSelect ;

      /// <summary>
      ///   Cursor for TGIS_ViewerMode.UserDefined mode.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForUserDefined : TCursor  read  FCursorForUserDefined
                                               write FCursorForUserDefined ;

      /// <summary>
      ///   Cursor for TGIS_ViewerMode.Zoom mode (zooming a map using a
      ///   rubber-band rectangle).
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForZoom : TCursor         read  FCursorForZoom
                                               write FCursorForZoom ;

      /// <summary>
      ///   Cursor for TGIS_ViewerMode.ZoomEx mode (zooming a map
      ///   interactively).
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForZoomEx : TCursor       read  FCursorForZoomEx
                                               write FCursorForZoomEx ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.CameraPosition
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForCameraPosition : TCursor
                                         read  FCursorForCameraPosition
                                         write FCursorForCameraPosition ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.CameraRotation.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForCameraRotation : TCursor
                                         read  FCursorForCameraRotation
                                         write FCursorForCameraRotation ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.CameraXYZ.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForCameraXYZ      : TCursor
                                         read  FCursorForCameraXYZ
                                         write FCursorForCameraXYZ      ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.CameraXY.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForCameraXY       : TCursor
                                         read  FCursorForCameraXY
                                         write FCursorForCameraXY       ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.Zoom.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForCameraZoom     : TCursor
                                         read  FCursorForCameraZoom
                                         write FCursorForCameraZoom     ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.SunPosition.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorForSunPosition    : TCursor
                                         read  FCursorForSunPosition
                                         write FCursorForSunPosition    ;

      /// <summary>
      ///   Cursor for TGIS_Viewer3DMode.Select.
      ///   If not set (equal crDefault) then built-in cursor default for
      ///   this Mode will be used.
      /// </summary>
      property CursorFor3DSelect       : TCursor
                                         read  FCursorFor3DSelect
                                         write FCursorFor3DSelect       ;

      {$IFDEF LEVEL_RX10_FMX}
        /// <summary>
        ///   Styled settings.
        /// </summary>
        property StyledSettings        : TGIS_ViewerWndStyledSettings
                                         read  FStyledSettings
                                         write FStyledSettings ;
      {$ENDIF}

    published // IGIS_Viewer published properties

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtentMargin  : Integer      read  fget_BigExtentMargin
                                               write fset_BigExtentMargin
                                               default -10              ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property KeepScale        : Boolean      read  fget_KeepScale
                                               write fset_KeepScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomPPI        : Integer      read  fget_CustomPPI
                                               write fset_CustomPPI
                                               default 0                 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FontScale        : Integer      read  fget_FontScale
                                               write fset_FontScale
                                               default 100               ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IncrementalPaint : Boolean      read  fget_IncrementalPaint
                                               write fset_IncrementalPaint
                                               default True             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MultiUserMode : TGIS_MultiUser  read  fget_MultiUserMode
                                               write fset_MultiUserMode
                                               default TGIS_MultiUser.Default;

      /// <inheritdoc from="IGIS_Viewer"/>
      property DelayedUpdate : Integer         read  fget_DelayedUpdate
                                               write fset_DelayedUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProgressiveUpdate : Integer     read  fget_ProgressiveUpdate
                                               write fset_ProgressiveUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedDrag   : Boolean      read  fget_RestrictedDrag
                                               write fset_RestrictedDrag
                                               default True             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionOutlineOnly : Boolean  read  fget_SelectionOutlineOnly
                                               write fset_SelectionOutlineOnly
                                               default False            ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionTransparency : Integer read  fget_SelectionTransparency
                                               write fset_SelectionTransparency
                                               default 60               ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionWidth   : Integer      read  fget_SelectionWidth
                                               write fset_SelectionWidth
                                               default 100              ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseAnimations    : Boolean      read  fget_UseAnimations
                                               write fset_UseAnimations
                                               default True             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseRTree         : Boolean      read  fget_UseRTree
                                               write fset_UseRTree
                                               default True             ;

    published // TGIS_ViewerWnd published properties

      /// <summary>
      ///   If True, then the screen will be centered on click, edit, etc.
      /// </summary>
      property AutoCenter       : Boolean      read  FAutoCenter
                                               write FAutoCenter
                                               default False            ;

      /// <summary>
      ///   Background color. Default TAlphaColorRec.White
      /// </summary>
      property BackgroundColor  : TAlphaColor  read  fget_BackgroundColor
                                               write fset_BackgroundColor ;

      /// <summary>
      ///   HiRes support. If checked the on Retina/4K class system when
      ///   FMX Widow Scaling is &gt; 1 map will be produced without scaling.
      /// </summary>
      property HiRes            : Boolean      read  fget_HiRes
                                               write fset_HiRes ;

      /// <summary>
      ///   Minimal size of the zooming rectangle.
      /// </summary>
      property MinZoomSize      : Integer      read  FMinZoomSize
                                               write FMinZoomSize
                                               default GIS_MIN_ZOOM_SIZE ;

      /// <summary>
      ///   Mode of reaction to mouse events. Window can be treated as a
      ///   selected area (gisSelect) or map can be dragged within windows
      ///   (TGIS_ViewerMode.Drag). Normally TGIS_ViewerMode.Select.
      /// </summary>
      property Mode          : TGIS_ViewerMode read  FMode
                                               write fset_Mode
                                               default TGIS_ViewerMode.Select ;

      /// <summary>
      ///   Active button for Mode operations (dragging, selecting ...).
      ///   DefaultValue(System.Windows.Forms.MouseButtons.Left)
      /// </summary>
      property ModeMouseButton  : TMouseButton read  FModeMouseButton
                                               write FModeMouseButton
                                               default TMouseButton.mbLeft ;

      /// <summary>
      ///   Color for selected objects.
      /// </summary>
      property SelectionColor   : TAlphaColor  read  fget_SelectionColor
                                               write fset_SelectionColor ;


    published // IGIS_Viewer events

      /// <event/>
      /// <summary>
      ///   Busy event. Will be fired regularly during long-drawn operations.
      ///   If end value will be zero, the meaning is: long-drawn with
      ///   unknown end time. Close long-drawn operation by calling with
      ///   parameters (-1,-1).
      /// </summary>
      property BusyEvent : TGIS_BusyEvent      read  fget_BusyEvent
                                               write fset_BusyEvent ;

      /// <event/>
      /// <summary>
      ///   Help event. If attached, all dialogboxes will have a help button,
      ///   and clicking on this button will raise an event.
      /// </summary>
      property HelpEvent : TGIS_HelpEvent      read  fget_HelpEvent
                                               write fset_HelpEvent ;

      /// <event/>
      /// <summary>
      ///   EditorSnapPoint event. Will be fired upon snapping a point in the
      ///   editor in custom snapping mode.
      /// </summary>
      property EditorSnapPointEvent : TGIS_EditorSnapPointEvent
                                               read  fget_EditorSnapPointEvent
                                               write fset_EditorSnapPointEvent ;

      /// <event/>
      /// <summary>
      ///   EditorPointChange event. Will be fired upon changing a point in the
      ///   editor.
      /// </summary>
      property EditorPointChangeEvent : TGIS_EditorPointChangeEvent
                                               read  fget_EditorPointChangeEvent
                                               write fset_EditorPointChangeEvent ;

      /// <event/>
      /// <summary>
      ///   EditorPointMove event. Will be fired upon mouse moving of edited
      ///   point in the editor.
      /// </summary>
      property EditorPointMoveEvent : TGIS_EditorPointMoveEvent
                                               read  fget_EditorPointMoveEvent
                                               write fset_EditorPointMoveEvent ;

      /// <event/>
      /// <summary>
      ///   ExtentChange event. Will be fired on TGIS_Viewer.Extent change.
      /// </summary>
      property ExtentChangeEvent : TNotifyEvent read fget_ExtentChangeEvent
                                               write fset_ExtentChangeEvent ;

      /// <event/>
      /// <summary>
      ///   LayerAdd event. Will be fired upon adding layer to the viewer:
      ///   after creation but before opening.
      /// </summary>
      property LayerAddEvent : TGIS_LayerEvent read  fget_LayerAddEvent
                                               write fset_LayerAddEvent ;

      /// <event/>
      /// <summary>
      ///   LayerDelete event. Will be fired upon deleting layer to the
      ///   viewer: just before destructing the layer.
      /// </summary>
      property LayerDelete : TGIS_LayerEvent   read  fget_LayerDeleteEvent
                                               write fset_LayerDeleteEvent ;

      /// <event/>
      /// <summary>
      ///   PaintException event. Will be fired when Paint rises an
      ///   exception.
      /// </summary>
      property PaintExceptionEvent : TGIS_PaintExceptionEvent
                                               read  fget_PaintExceptionEvent
                                               write fset_PaintExceptionEvent ;

      /// <event/>
      /// <summary>
      ///   Password event. Will be fired upon opening layer to resolve any
      ///   username/password as a key/value pair. Supported only on selected
      ///   layers.
      /// </summary>
      property PasswordEvent : TGIS_TemplateProducerEvent
                                               read  fget_PasswordEvent
                                               write fset_PasswordEvent ;

      /// <event/>
      /// <summary>
      ///   ProjectClose event. Will be fired on TGIS_Viewer.Close.
      /// </summary>
      property ProjectCloseEvent : TNotifyEvent read fget_ProjectCloseEvent
                                               write fset_ProjectCloseEvent ;

      /// <event/>
      /// <summary>
      ///   ProjectOpen event. Will be fired on TGIS_Viewer.Open.
      /// </summary>
      property ProjectOpenEvent : TNotifyEvent read  fget_ProjectOpenEvent
                                               write fset_ProjectOpenEvent ;

      /// <event/>
      /// <summary>
      ///   VisibleExtentChange event. Will be fired before BeforePaint if
      ///   TGIS_Viewer.VisibleExtent was changed. Will not be fired if
      ///   TGIS_Viewer.VisibleExtent was changed based on changed Zoom - in
      ///   such situation only ZoomChange will be fired.
      /// </summary>
      property VisibleExtentChangeEvent : TNotifyEvent
                                          read  fget_VisibleExtentChangeEvent
                                          write fset_VisibleExtentChangeEvent ;

      /// <event/>
      /// <summary>
      ///   ZoomChange event. Will be fired before OnBeforePaint if
      ///   Viewer.Zoom was changed.
      /// </summary>
      property ZoomChangeEvent : TNotifyEvent  read  fget_ZoomChangeEvent
                                               write fset_ZoomChangeEvent ;

    published // IGIS_ViewerParent events

      /// <event/>
      /// <summary>
      ///   BeforePaint event. Will be fired before any Paint operation.
      ///   Uses painting context object.
      /// </summary>
      property BeforePaintEvent  : TGIS_PaintEvent read  FOnBeforePaint
                                                   write FOnBeforePaint ;

      /// <event/>
      /// <summary>
      ///   BeforePaintRenderer event. Will be fired before any Paint operation.
      ///   Uses renderer object.
      /// </summary>
      property BeforePaintRendererEvent : TGIS_RendererEvent
                                               read  FOnBeforePaintRenderer
                                               write FOnBeforePaintRenderer ;

      /// <event/>
      /// <summary>
      ///   AfterPaint event. Will be fired after any Paint operation.
      ///   Uses painting context object.
      /// </summary>
      property AfterPaintEvent   : TGIS_PaintEvent read  FOnAfterPaint
                                                   write FOnAfterPaint ;

      /// <event/>
      /// <summary>
      ///   AfterPaintRenderer event. Will be fired after any Paint operation.
      ///   Uses renderer object.
      /// </summary>
      property AfterPaintRendererEvent : TGIS_RendererEvent
                                               read  FOnAfterPaintRenderer
                                               write FOnAfterPaintRenderer ;

      /// <event/>
      /// <summary>
      ///   PaintExtra event. Called after renderer.PaintExtra() method.
      /// </summary>
      property PaintExtraEvent   : TGIS_RendererEvent
                                                read  FOnPaintExtra
                                                write FOnPaintExtra ;

      /// <event/>
      /// <summary>
      ///   BeforeUpdate event. Will be fired before Update operation.
      /// </summary>
      property BeforeUpdateEvent : TGIS_RendererEvent
                                                read  FOnBeforeUpdate
                                                write FOnBeforeUpdate ;

      /// <event/>
      /// <summary>
      ///   AfterUpdate event. Will be fired after Update operation.
      /// </summary>
      property AfterUpdateEvent  : TGIS_RendererEvent
                                                read  FOnAfterUpdate
                                                write FOnAfterUpdate ;

    published // events new for this class

      /// <event/>
      /// <summary>
      ///   Will be fired on viewer mode change.
      /// </summary>
      property ModeChangeEvent   : TNotifyEvent read  FOnModeChange
                                                write FOnModeChange ;

      /// <event/>
      /// <summary>
      ///   Will be fired after any change made by editor.
      /// </summary>
      property EditorChangeEvent : TNotifyEvent read  FOnEditorChange
                                                write FOnEditorChange ;

      /// <event/>
      /// <summary>
      ///   TapSimple event. Will be fired upon press down/up.
      /// </summary>
      property TapSimpleEvent    : TMouseEvent  read  FOnTapSimple
                                                write FOnTapSimple ;

      /// <event/>
      /// <summary>
      ///   TapLong event. Will be fired upon longer press down.
      /// </summary>
      property TapLongEvent      : TMouseEvent  read  FOnTapLong
                                                write FOnTapLong ;

      /// <event/>
      /// <summary>
      ///   TapDouble event. Will be fired upon double press down/up.
      /// </summary>
      property TapDoubleEvent    : TMouseEvent  read  FOnTapDouble
                                                write FOnTapDouble ;

    published // TControl inherited  published properties

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Action;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Align;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Anchors;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Enabled;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Locked default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Height;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property HitTest default True;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Padding;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Opacity;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Margins;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property PopupMenu;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Position;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property RotationAngle;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property RotationCenter;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Size;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property TouchTargetExpansion;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Visible;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Width;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property TabOrder;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property TabStop;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnApplyStyleLookup;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragLeave;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragOver;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragDrop;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragEnd;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnKeyDown;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnKeyUp;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnCanFocus;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnExit;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnClick;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDblClick;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseDown;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseMove;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseUp;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseWheel;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseLeave;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnPainting;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnPaint;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnResize;

   end;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation


{$R Lider.CG.GIS.GeoViewerWnd_16x16.RES}
{$R FMX.Images.res}
{$IFDEF MSWINDOWS}
  {$R FMX.Cursors.Win.RES}
{$ENDIF}
{$IFDEF MACOS}
  {$R FMX.Cursors.Mac.RES}
{$ENDIF}


uses
  System.Math.Vectors,

  FMX.Surfaces,
  {$IFDEF LEVEL_RX101_FMX}
    FMX.Clipboard,
  {$ENDIF}
  FMX.Platform,

  FMX.BehaviorManager,
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.Pthread,
  {$ENDIF}
  {$IFDEF MACOS}
    Macapi.ObjectiveC,
    {$IFDEF IOS}
      Iosapi.Foundation,
      Iosapi.CocoaTypes,
    {$ELSE}
      Macapi.AppKit,
      Macapi.Foundation,
      Macapi.CocoaTypes,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    FMX.Platform.Win,
  {$ENDIF}

  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.FMX.GeoRenderer ;

type
  T_FMXBitmap = FMX.Graphics.TBitmap ;

type
  T_platformExtender = class( TInterfacedObject, IFMXCursorService )
    private
      h : IntPtr ;
      cursorService : IFMXCursorService ;
      procedure SetCursor(const ACursor: TCursor);
      function GetCursor: TCursor;
    end ;
var
  platformExtender : T_platformExtender = nil;

const

  // Cursor for dragging.
  DRAG_CURSOR      = 15  ;

  // Cursor for selecting.
  SELECT_CURSOR    = 16  ;

  // Cursor for zooming.
  ZOOM_CURSOR      = 18  ;

  // Cursor for editing.
  EDIT_CURSOR      = 19  ;

  // Cursor for editing.
  CUSTOM_CURSOR    = 20  ;

  // Cursor for lenthly operation.
  WAIT_CURSOR      = 21  ;

  // Cursor for TGIS_Viewer3DMode.CameraPosition.
  CAMPOS_CURSOR   = 22  ;

  // Cursor for TGIS_Viewer3DMode.CameraRotation.
  CAMROT_CURSOR   = 23  ;

  // Cursor for TGIS_Viewer3DMode.CameraXYZ.
  CAMXYZ_CURSOR   = 24  ;

  // Cursor for TGIS_Viewer3DMode.CameraXY.
  CAMXY_CURSOR    = 25  ;

  // Cursor for TGIS_Viewer3DMode.Zoom.
  CAMZOOM_CURSOR  = 26  ;

  // Cursor for TGIS_Viewer3DMode.SunPosition.
  SUNPOS_CURSOR   = 27  ;

  // Cursor for TGIS_Viewer3DMode.Select.
  SELECT3D_CURSOR = 28  ;

  // Hourglass refresh interval in ms
  HOURGLASS_REFRESH = 250 ;

{$IFDEF MSWINDOWS}
  var
    CustomCursors : array[ DRAG_CURSOR..SELECT3D_CURSOR ] of HCursor ;
{$ENDIF}
{$IFDEF MACOS}
  {$IFNDEF IOS}
    var
      CustomCursors : array[ DRAG_CURSOR..SELECT3D_CURSOR ] of NSCursor ;
  {$ENDIF}
{$ENDIF}

const
  NO_MOUSE_POS = GIS_MIN_INTEGER ; // non existing previous mouse position

//==============================================================================
// TGIS_TouchContext class
//==============================================================================

  constructor TGIS_TouchContext.Create(
    const _touches : TTouches ;
    const _action  : TTouchAction
  ) ;
  begin
    inherited Create ;
    FTouches := _touches ;
    FAction  := _action  ;
  end ;

//==============================================================================
// cursors' service
//==============================================================================

{$IFDEF MSWINDOWS}
  procedure T_platformExtender.SetCursor(
    const ACursor : TCursor
  ) ;
  begin
    if ACursor >= DRAG_CURSOR then begin
      Winapi.Windows.SetCursor( CustomCursors[ ACursor ]);
    end
    else if ACursor = crDefault then
      Winapi.Windows.SetCursor( LoadCursor( 0, IDC_ARROW) )
    else begin
      platformExtender.cursorService.SetCursor( ACursor )
    end;
  end;

  function T_platformExtender.GetCursor
   : TCursor ;
  begin
    Result := platformExtender.cursorService.GetCursor ;
  end;

  procedure TGIS_ViewerWnd.ensureCursor ;
  var
    cr  : TCursor  ;
  begin
    if View3D then begin
      ensure3DCursor ;
      exit ;
    end;

    if (Root <> nil) and (Root.Captured = nil) and not (csLoading in ComponentState) then
    begin
      case Mode of
        TGIS_ViewerMode.Select :
            if CursorForSelect  = crDefault then
              cr := TCursor(SELECT_CURSOR)
            else
              cr := TCursor(CursorForSelect);
        TGIS_ViewerMode.Drag :
            if CursorForDrag = crDefault then
              cr := TCursor(DRAG_CURSOR)
            else
              cr := TCursor(CursorForDrag);
        TGIS_ViewerMode.Zoom :
            if CursorForZoom = crDefault then
              cr := TCursor(ZOOM_CURSOR)
            else
              cr := TCursor(CursorForZoom);
        TGIS_ViewerMode.Edit :
            if CursorForEdit = crDefault then
              cr := TCursor(EDIT_CURSOR)
            else
              cr := TCursor(CursorForEdit);
        TGIS_ViewerMode.ZoomEx      :
            if CursorForZoomEx = crDefault then
              cr := TCursor(ZOOM_CURSOR)
            else
              cr := TCursor(CursorForZoomEx);
        TGIS_ViewerMode.UserDefined :
            if CursorForUserDefined = crDefault then
              cr := TCursor(CUSTOM_CURSOR)
            else
              cr := TCursor(CursorForUserDefined);
        else
        begin
          Assert( False, 'not tested' ) ;
        end ;
      end ;
      if Cursor = cr then exit ;

      Cursor := cr ;

      // avoid cursor flickering
      SetClassLong( FmxHandleToHWND(TForm(Parent).Handle), GCL_HCURSOR, 0);
    end;
  end;

  procedure TGIS_ViewerWnd.ensure3DCursor ;
  var
    cr  : TCursor  ;
  begin
    if (Root <> nil) and (Root.Captured = nil) and not (csLoading in ComponentState) then
    begin
      case Viewer3D.Mode of
        TGIS_Viewer3DMode.CameraPosition :
          begin
            if FCursorForCameraPosition = crDefault then
              cr := TCursor(CAMPOS_CURSOR)
            else
              cr := TCursor(CursorForCameraPosition)
          end ;

        TGIS_Viewer3DMode.CameraRotation :
          begin
            if CursorForCameraRotation = crDefault then
              cr := TCursor(CAMROT_CURSOR)
            else
              cr := TCursor(CursorForCameraRotation)
          end ;

        TGIS_Viewer3DMode.CameraXYZ :
          begin
            if CursorForCameraXYZ = crDefault then
              cr := TCursor(CAMXYZ_CURSOR)
            else
              cr := TCursor(CursorForCameraXYZ)
          end ;

        TGIS_Viewer3DMode.CameraXY :
          begin
            if CursorForCameraXY = crDefault then
              cr := TCursor(CAMXY_CURSOR)
            else
              cr := TCursor(CursorForCameraXY)
          end ;

        TGIS_Viewer3DMode.Zoom :
          begin
            if CursorForZoom  = crDefault then
              cr := TCursor(CAMZOOM_CURSOR)
            else
              cr := TCursor(CursorForZoom)
          end ;

        TGIS_Viewer3DMode.SunPosition :
          begin
            if CursorForCameraPosition = crDefault then
              cr := TCursor(SUNPOS_CURSOR)
            else
              cr := TCursor(CursorForSunPosition)
          end ;

        TGIS_Viewer3DMode.Select :
          begin
            if CursorForSelect  = crDefault then
              cr := TCursor(SELECT3D_CURSOR)
            else
              cr := TCursor(CursorForSelect)
          end ;
        else
        begin
          Assert( False, 'not tested' ) ;
        end ;
      end ;
      if Cursor = cr then exit ;

      Cursor := cr ;

      // avoid cursor flickering
      SetClassLong( FmxHandleToHWND(TForm(Parent).Handle), GCL_HCURSOR, 0);
    end;
  end;
{$ENDIF}

{$IFDEF MACOSX}
  procedure T_platformExtender.SetCursor(
    const ACursor : TCursor
  ) ;
  begin
    if ACursor >= DRAG_CURSOR then begin
      CustomCursors[ ACursor ].push ;
    end
    else begin
      platformExtender.cursorService.SetCursor( ACursor )
    end;
  end;

  function T_platformExtender.GetCursor
    : TCursor ;
  begin
    Result := platformExtender.cursorService.GetCursor ;
  end;

  procedure TGIS_ViewerWnd.ensureCursor;
  var
    cr  : TCursor  ;
  begin
    if View3D then begin
      ensure3DCursor ;
      exit ;
    end;

    {$IFNDEF IOS}
      if (Root <> nil) and (Root.Captured = nil) and not (csLoading in ComponentState) then
      begin
        case Mode of
          TGIS_ViewerMode.Select      :
              if CursorForSelect     = crDefault then
                cr := TCursor(SELECT_CURSOR)
              else
                cr := TCursor(CursorForSelect);
          TGIS_ViewerMode.Drag        :
              if CursorForDrag       = crDefault then
                cr := TCursor(DRAG_CURSOR)
              else
                cr := TCursor(CursorForDrag);
          TGIS_ViewerMode.Zoom        :
              if CursorForZoom       = crDefault then
                cr := TCursor(ZOOM_CURSOR)
              else
                cr := TCursor(CursorForZoom);
          TGIS_ViewerMode.Edit        :
              if CursorForEdit       = crDefault then
                cr := TCursor(EDIT_CURSOR)
              else
                cr := TCursor(CursorForEdit);
          TGIS_ViewerMode.ZoomEx      :
              if CursorForZoomEx     = crDefault then
                cr := TCursor(ZOOM_CURSOR)
              else
                cr := TCursor(CursorForZoomEx);
          TGIS_ViewerMode.UserDefined :
              if CursorForUserDefined = crDefault then
                cr := TCursor(CUSTOM_CURSOR)
              else
                cr := TCursor(CursorForUserDefined);
          else
          begin
            Assert( False, 'not tested' ) ;
          end ;
        end ;
        if Cursor = cr then exit ;

        Cursor := cr ;
      end ;
    {$ENDIF}
  end;

  procedure TGIS_ViewerWnd.ensure3DCursor;
  var
    cr  : TCursor  ;
  begin
    {$IFNDEF IOS}
      if (Root <> nil) and (Root.Captured = nil) and not (csLoading in ComponentState) then
      begin
        case Viewer3D.Mode of
          TGIS_Viewer3DMode.CameraPosition :
            begin
              if FCursorForCameraPosition = crDefault then
                cr := TCursor(CAMPOS_CURSOR)
              else
                cr := TCursor(CursorForCameraPosition)
            end ;

          TGIS_Viewer3DMode.CameraRotation :
            begin
              if CursorForCameraRotation = crDefault then
                cr := TCursor(CAMROT_CURSOR)
              else
                cr := TCursor(CursorForCameraRotation)
            end ;

         TGIS_Viewer3DMode.CameraXYZ :
            begin
              if CursorForCameraXYZ = crDefault then
                cr := TCursor(CAMXYZ_CURSOR)
              else
                cr := TCursor(CursorForCameraXYZ)
            end ;

          TGIS_Viewer3DMode.CameraXY :
            begin
              if CursorForCameraXY = crDefault then
                cr := TCursor(CAMXY_CURSOR)
              else
                cr := TCursor(CursorForCameraXY)
            end ;

          TGIS_Viewer3DMode.Zoom :
            begin
              if CursorForZoom  = crDefault then
                cr := TCursor(CAMZOOM_CURSOR)
              else
                cr := TCursor(CursorForZoom)
            end ;

          TGIS_Viewer3DMode.SunPosition :
            begin
              if CursorForCameraPosition = crDefault then
                cr := TCursor(SUNPOS_CURSOR)
              else
                cr := TCursor(CursorForSunPosition)
            end ;

          TGIS_Viewer3DMode.Select :
            begin
              if CursorForSelect  = crDefault then
                cr := TCursor(SELECT3D_CURSOR)
              else
                cr := TCursor(CursorForSelect)
            end ;
          else
          begin
            Assert( False, 'not tested' ) ;
          end ;
        end ;
        if Cursor = cr then exit ;

        Cursor := cr ;
      end ;
    {$ENDIF}
  end ;

{$ENDIF}

{$IFDEF GIS_MOBILE}
  procedure T_platformExtender.SetCursor(
    const ACursor : TCursor
  ) ;
  begin
    platformExtender.cursorService.SetCursor( ACursor )
  end;

  function T_platformExtender.GetCursor: TCursor;
  begin
    Result := platformExtender.cursorService.GetCursor ;
  end;

  procedure TGIS_ViewerWnd.ensureCursor;
  begin
  end;

  procedure TGIS_ViewerWnd.ensure3DCursor;
  begin
  end;
{$ENDIF}

{$IFDEF LINUX}
  procedure T_platformExtender.SetCursor(
    const ACursor : TCursor
  ) ;
  begin
    platformExtender.cursorService.SetCursor( ACursor )
  end;

  function T_platformExtender.GetCursor: TCursor;
  begin
    Result := platformExtender.cursorService.GetCursor ;
  end;

  procedure TGIS_ViewerWnd.ensureCursor;
  begin
  end;

  procedure TGIS_ViewerWnd.ensure3DCursor;
  begin
  end;
{$ENDIF}

//==============================================================================
// private methods
//==============================================================================

  procedure TGIS_ViewerWnd.readGesturesMetadata ;
  begin
    metModeDragAllGestures := GisMetadataAsBoolean(
      METADATA_MODEDRAGALLGESTURES,
      False
    ) ;
    metModeZoomAllGestures := GisMetadataAsBoolean(
      METADATA_MODEZOOMALLGESTURES,
      False
    ) ;
    metModeSelectAllGestures := GisMetadataAsBoolean(
      METADATA_MODESELECTALLGESTURES,
      True
    ) ;
    metModeEditAllGestures := GisMetadataAsBoolean(
      METADATA_MODEEDITALLGESTURES,
      True
    ) ;
    metViewer3DModeCameraPositionAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODECAMERAPOSITIONALLGESTURES,
      False
    ) ;
    metViewer3DModeCameraXYZAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODECAMERAXYZALLGESTURES,
      False
    ) ;
    metViewer3DModeCameraXYAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODECAMERAXYALLGESTURES,
      False
    ) ;
    metViewer3DModeCameraRotationAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODECAMERAROTATIONALLGESTURES,
      False
    ) ;
    metViewer3DModeSunPositionAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODESUNPOSITIONALLGESTURES,
      False
    ) ;
    metViewer3DModeZoomAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODEZOOMALLGESTURES,
      False
    ) ;
    metViewer3DModeSelectAllGestures := GisMetadataAsBoolean(
      METADATA_VIEWER3DMODESELECTALLGESTURES,
      True
    ) ;
  end ;

  function TGIS_ViewerWnd.gesturePanEnabled
    : Boolean ;
  begin
    if ( ( FMode = TGIS_ViewerMode.Select ) and metModeSelectAllGestures ) or
       ( ( FMode = TGIS_ViewerMode.Edit   ) and metModeEditAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Drag   ) and metModeDragAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Zoom   ) and metModeZoomAllGestures   ) then
      Result := True
    else
      Result := False ;
  end ;

  function TGIS_ViewerWnd.gestureZoomEnabled
    : Boolean ;
  begin
    if ( ( FMode = TGIS_ViewerMode.Select ) and metModeSelectAllGestures ) or
       ( ( FMode = TGIS_ViewerMode.Edit   ) and metModeEditAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Drag   ) and metModeDragAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Zoom   ) and metModeZoomAllGestures   ) then
      Result := True
    else
      Result := False ;
  end ;

  function TGIS_ViewerWnd.gesture3DPanEnabled
    : Boolean ;
  begin
    if ( ( FViewer3D.Mode = TGIS_Viewer3DMode.Select ) and metViewer3DModeSelectAllGestures ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.Zoom   ) and metViewer3DModeZoomAllGestures   ) then
      result := true
    else
      result := false ;
  end ;

  function TGIS_ViewerWnd.gesture3DZoomEnabled
    : Boolean ;
  begin
    if ( ( FViewer3D.Mode = TGIS_Viewer3DMode.Select         ) and metViewer3DModeSelectAllGestures         ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.Zoom           ) and metViewer3DModeZoomAllGestures           ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.CameraXY       ) and metViewer3DModeCameraXYAllGestures       ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.CameraXYZ      ) and metViewer3DModeCameraXYZAllGestures      ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.CameraPosition ) and metViewer3DModeCameraPositionAllGestures ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.CameraRotation ) and metViewer3DModeCameraRotationAllGestures ) or
       ( ( FViewer3D.Mode = TGIS_Viewer3DMode.SunPosition    ) and metViewer3DModeSunPositionAllGestures    ) then
      result := true
    else
      result := false ;
  end ;

  procedure TGIS_ViewerWnd.doTouch(
          _sender  : TObject      ;
    const _touches : TTouches     ;
    const _action  : TTouchAction
  ) ;
  var
    context : TGIS_TouchContext ;
    loc : TPointF ;
  begin
    arTouches := _touches ;
    context := TGIS_TouchContext.Create( _touches, _action ) ;
    try
      NotifySubscribers( GIS_SUBSCRIBED_TOUCH, context );
    finally
      FreeObject( context ) ;
    end;

    if Assigned( oOnTouchOriginal ) then
      oOnTouchOriginal( _sender, _touches, _action ) ;
  end;

  procedure TGIS_ViewerWnd.doGestureProc(
          _sender  : TObject           ;
    const _info    : TGestureEventInfo ;
    var   _handled : Boolean
  ) ;
  var
    loc : TPointF ;

    procedure begin_mode ;
    begin
      oWndHelper.UpdateCancel ;

      resetMousePos(RoundS(loc.X), RoundS(loc.Y));
      zoomDistance := _info.Distance ;
      if ( not panGesture ) and ( _info.GestureID = igiZoom ) then
        zoomGesture := True ;
      if ( not zoomGesture ) and ( _info.GestureID = igiPan ) then
        panGesture := True ;
      gestureTouches := Length( arTouches ) ;

      oWndHelper.GestureBegin ;
      if View3D then
        FViewer3D.GestureBegin ;
    end;

    procedure end_mode ;
    begin
      if oWndHelper.GestureActive then begin
        zoomGesture := False ;
        panGesture  := False ;
        if View3D then
          FViewer3D.GestureEnd ;
        oWndHelper.GestureEnd ;
        if not View3D then begin
          oWndHelper.FitScreenAnimation ;
          oWndHelper.UpdateDelayed ;
        end ;
      end ;
    end;

    procedure do_zoom ;
    var
      zm : Double ;
    begin
      oWndHelper.GestureCancelLongTap ; // patch for iOS event order

      if panGesture then exit ;

      if View3D then begin
        zm := _info.Distance / zoomDistance ;
        FViewer3D.DoGesture( 0, 0, mousePos.X, mousePos.Y, zm, 0, 0, 0 ) ;
      end else begin
        zm := Power( _info.Distance / zoomDistance, 2 ) ;
        if zm <> 1 then
          oWndHelper.DoZoom( mousePos.X * ControlCanvasScale,
                             mousePos.Y * ControlCanvasScale,
                             zm, False, True
                           ) ;
      end ;
      zoomDistance := _info.Distance ;
    end ;

    procedure do_drag ;
    begin
      if not oWndHelper.GestureActive then
        exit ;

      if ( loc.X < 0     ) or
         ( loc.Y < 0     ) or
         ( loc.X > ControlCanvasWidth  / ControlCanvasScale ) or
         ( loc.Y > ControlCanvasHeight / ControlCanvasScale)
      then
        exit;

      if ( zoomDistance > 0 ) and ( _info.Distance = 0 ) then
        exit ;

      if zoomGesture then begin
        // drag upon zooming
        if View3D then
          FViewer3D.DoGesture( RoundS( mousePos.X - loc.X ),
                               RoundS( mousePos.Y - loc.Y ),
                               0, 0, 1, 0, 0, 0 )
        else
          oWndHelper.DoDrag( ( mousePos.X - loc.X ) * ControlCanvasScale,
                             ( mousePos.Y - loc.Y ) * ControlCanvasScale
                           );
      end else begin
        if gestureTouches = Length( arTouches ) then begin
          if View3D then begin
            if ( Length( arTouches ) <> 2 ) then
              // drag
              FViewer3D.DoGesture( RoundS( mousePos.X - loc.X ),
                                   RoundS( mousePos.Y - loc.Y ),
                                   0, 0, 1, 0, 0, 0 )
            else
              // camera position
              FViewer3D.DoGesture( 0, 0, 0, 0, 1,
                                   RoundS( loc.X - mousePos.X ),
                                   RoundS( loc.Y - mousePos.Y ), 1 ) ;
          end else
            oWndHelper.DoDrag( ( mousePos.X - loc.X ) * ControlCanvasScale,
                               ( mousePos.Y - loc.Y ) * ControlCanvasScale
                             );
        end ;
        gestureTouches := Length( arTouches ) ;
      end ;
      resetMousePos(RoundS(loc.X), RoundS(loc.Y));
    end;

  begin
    _handled := False ;

    if IsEmpty then
      exit ;
    if mouseBlocked then
      exit ;
    if not ( self.Root is TForm ) then
      exit ;

    readGesturesMetadata ;
    if ( View3D     and not gesture3DPanEnabled and not gesture3DZoomEnabled ) or
       ( not View3D and not gesturePanEnabled   and not gestureZoomEnabled   ) then begin
      oWndHelper.GestureEnd( True ) ;
      exit ;
    end ;

    loc := TForm( self.Root ).ClientToScreen( _info.Location ) ;
    loc := ScreenToLocal( loc ) ;

    case _info.GestureID of
      igiZoom : begin
                  if ( View3D     and gesture3DZoomEnabled ) or
                     ( not View3D and gestureZoomEnabled ) then begin
                    if TInteractiveGestureFlag.gfBegin in _info.Flags then
                      begin_mode
                    else if TInteractiveGestureFlag.gfEnd in _info.Flags then
                      end_mode
                    else
                      do_zoom ;
                  end ;
                  _handled := True ;
                end ;
      igiPan :  begin
                  if ( View3D     and gesture3DPanEnabled ) or
                     ( not View3D and gesturePanEnabled ) then begin
                    if TInteractiveGestureFlag.gfBegin in _info.Flags then begin
                      if not View3D and ( Mode = TGIS_ViewerMode.Edit ) then begin
                        if Length( arTouches ) = 2 then
                          begin_mode ;
                      end
                      else
                        begin_mode ;
                    end else if TInteractiveGestureFlag.gfEnd in _info.Flags then
                      end_mode
                    else begin
                      if not View3D and ( Mode = TGIS_ViewerMode.Edit ) then begin
                        if Length( arTouches ) = 2 then
                          do_drag ;
                      end
                      else
                        do_drag ;
                    end ;
                  end ;
                  _handled := True ;
                end ;
      igiRotate       : _handled := True ;
      igiTwoFingerTap : _handled := True ;
      igiPressAndTap  : _handled := True ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.resetMousePos(
    const _x, _y : Integer
  ) ;
  begin
    mousePos.X     := _x ;
    mousePos.Y     := _y ;
    mousePosOld.X  := NO_MOUSE_POS ;
    mousePosOld.Y  := NO_MOUSE_POS ;
  end;

  procedure TGIS_ViewerWnd.updateExecute(
    _sender : TObject ;
    _ctx    : TGIS_ViewerWndHelperRun
  ) ;
  begin
    if oVwr.Parent as TObject <> self then
      exit ;

    if ( Width < 5 ) or ( Height < 5 )  then
      exit ;

    if bInUpdate then exit ;
    bInUpdate := True ;

    mouseBlocked := True ;
    HourglassPrepare ;
    oWndHelper.SetVisibleExtent ;

    oRenderer.ReleaseContext ;

    FDrawContext.Clear ;

    FProgressBitmap := nil ;

    try
      _ctx.Prepare( ControlCanvasWidth, ControlCanvasHeight, VisibleExtent ) ;

      FDrawContext.ProgressiveHelper := _ctx ;

      if oVwr.IsEmpty then begin
        exit ;
      end;

      // Map
      FDrawContext.AssignBaseMap  ( nil, True ) ;

      // Selection
      FDrawContext.AssignSelection( nil, True ) ;

      // Charts
      FDrawContext.AssignCharts   ( nil, True ) ;

      // Labels
      FDrawContext.AssignLabels   ( nil, True ) ;

      oRenderer.CreateContext( Self, oVwr, FDrawContext, Point(0, 0),
                               ControlCanvasWidth, ControlCanvasHeight,
                               PPI, FontScale );
      try
        oVwr.LabelsReg.Reset ;
        oRenderer.BeforeDraw ;
        oVwr.BeginPaintInternal ;
        if assigned( FOnBeforeUpdate ) then
          FOnBeforeUpdate( Self, oRenderer, TGIS_DrawMode.AllExceptTop ) ;

        if assigned( FOnUpdate )
          then FOnUpdate( Self, oRenderer, TGIS_DrawMode.AllExceptTop )
          else Draw( oRenderer, TGIS_DrawMode.AllExceptTop ) ;

      finally
        if assigned( FOnAfterUpdate ) then
          FOnAfterUpdate( Self, oRenderer, TGIS_DrawMode.AllExceptTop ) ;
        oVwr.EndPaintInternal ;
        oRenderer.AfterDraw ;
      end;
    finally
      FDrawContext.ProgressiveHelper := nil ;
      FProgressBitmap := nil ;
      bInUpdate := False ;

      if not _ctx.MustBreak then begin
        _ctx.Bitmap := FDrawContext.BaseMap ;
        _ctx.DoSynchronize( True ) ;
      end ;
      mouseBlocked := False ;
      HourglassRelease ;
      NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, self ) ;
    end;
  end;

  procedure TGIS_ViewerWnd.updateSynchronize(
    _sender : TObject ;
    _ctx    : TGIS_ViewerWndHelperRun ;
    _final  : Boolean
  ) ;
  begin
    if _final then begin
      FreeObject( FCacheBitmap ) ;

      FCacheBitmap := T_FMXBitmap.Create( ControlCanvasWidth, ControlCanvasHeight ) ;

      FCacheBitmap.Assign( TPersistent( _ctx.Bitmap ) ) ;
      oWndHelper.Reset( _ctx ) ;
      bCacheBitmapNew := True ;

      ControlRepaint ;
    end
    else begin
      FProgressBitmap := T_FMXBitmap( FDrawContext.BaseMap ) ;
      ControlRepaint ;
    end;
  end;

  procedure TGIS_ViewerWnd.updateTap(
    _sender : TObject
  ) ;
  var
    btn_state   : TMouseButton ;
    shift_state : TShiftState  ;
  begin
    shift_state := []  ;

    if oWndHelper.GestureState.Left then begin
      btn_state   := TMouseButton.mbLeft ;
      shift_state := shift_state + [ssLeft] ;
    end;
    if oWndHelper.GestureState.Right then begin
      btn_state   := TMouseButton.mbRight ;
      shift_state := shift_state + [ssRight] ;
    end;
    if oWndHelper.GestureState.Middle then begin
      btn_state   := TMouseButton.mbMiddle ;
      shift_state := shift_state + [ssMiddle] ;
    end;
    if oWndHelper.GestureState.Shift then begin
      shift_state := shift_state + [ssShift] ;
    end;
    if oWndHelper.GestureState.Alt then begin
      shift_state := shift_state + [ssAlt] ;
    end;
    if oWndHelper.GestureState.Ctrl then begin
      shift_state := shift_state + [ssCtrl] ;
    end;
    if oWndHelper.GestureState.Touch then begin
      shift_state := shift_state + [ssTouch] ;
    end;
    if oWndHelper.GestureState.Pen then begin
      shift_state := shift_state + [ssPen] ;
    end;

    case oWndHelper.GestureState.DownCount of
      1 : TapSingle( btn_state,
                     shift_state,
                     oWndHelper.GestureState.DownX,
                     oWndHelper.GestureState.DownY
                   ) ;
      2 : TapDouble( btn_state,
                     shift_state,
                     oWndHelper.GestureState.DownX,
                     oWndHelper.GestureState.DownY
                   ) ;
      3 : TapLong  ( btn_state,
                     shift_state,
                     oWndHelper.GestureState.DownX,
                     oWndHelper.GestureState.DownY
                   ) ;
      else begin
             assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
           end ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.doGraticule(
   _sender   : TObject ;
   _renderer : TGIS_RendererAbstract ;
   _mode     : TGIS_DrawMode
  ) ;
    function scaled_rect( _rct : TRectF ) : TRectF ;
    begin
       Result.Left   := _rct.Left   / ControlCanvasScale ;
       Result.Top    := _rct.Top    / ControlCanvasScale ;
       Result.Right  := _rct.Right  / ControlCanvasScale ;
       Result.Bottom := _rct.Bottom / ControlCanvasScale ;
    end;
  begin
    oGraticuleHelper.Draw(
      _renderer,
      RoundS( Width ), RoundS( Height),
      scaled_rect( oWndHelper.OriginalRect ),
      scaled_rect( oWndHelper.ScaledRect( 1 ) ),
      oWndHelper.ActualExtent
    ) ;
  end ;

  procedure TGIS_ViewerWnd.doPrintBmp(
    const _bitmap : T_FMXBitmap ;
    const _full   : Boolean
  ) ;
  var
    bmp     : T_FMXBitmap ;
    w, h    : Integer ;
    wextent : TGIS_Extent ;
    scl     : Double ;

    procedure prepare_map(
      _ext : TGIS_Extent ;
      _scl : Double
    ) ;
    var
      ctx       : TGIS_RendererContext ;
      bincrmntl : Boolean ;
      basemap_store : array of Boolean ;

      procedure store_basemap ;
      var
        i : Integer ;
      begin
        SetLength( basemap_store, oVwr.Items.Count ) ;
        for i := 0 to oVwr.Items.Count - 1 do begin
          basemap_store[i] := TGIS_Layer(oVwr.Items[i]).Basemap ;
          if basemap_store[i] then
            TGIS_Layer(oVwr.Items[i]).Basemap := False ;
        end;
      end;

      procedure restore_basemap ;
      var
        i : Integer ;
      begin
        if not assigned( basemap_store ) then exit ;
        for i := 0 to oVwr.Items.Count - 1 do begin
          if basemap_store[i] then
            TGIS_Layer(oVwr.Items[i]).Basemap := basemap_store[i] ;
        end;
      end ;

    begin

      if oVwr.IsEmpty then exit ;

      WaitForBackgroundProcesses ;
      oBasemapHelper.LockBitmap ;

      bincrmntl := oVwr.IncrementalPaint ;
      oVwr.IncrementalPaint := False ;

      ctx := TGIS_RendererContext.Create ;
      try
        // Map
        ctx.AssignBaseMap  ( bmp, False ) ;

        // Selection
        ctx.AssignSelection( nil, True  ) ;

        // Charts
        ctx.AssignCharts   ( nil, True  ) ;

        // Labels
        ctx.AssignLabels   ( nil, True  ) ;

        store_basemap ;

        oRenderer.ReleaseContext ;

        if _full then begin
          bmp.Canvas.BeginScene;
            if assigned( FOnBeforePaintRenderer ) then
              oRenderer.PaintExtra( Self, bmp.Canvas, FOnBeforePaintRenderer ) ;

            if assigned( FOnBeforePaint ) then
              FOnBeforePaint( Self, bmp.Canvas ) ;
          bmp.Canvas.EndScene ;
        end;

        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                                 w, h, PPI, FontScale
                               ) ;

        oVwr.LabelsReg.Reset ;
        oRenderer.BeforeDraw ;
        oVwr.BeginPaintInternal ;
        if assigned( FOnBeforeUpdate ) then
          FOnBeforeUpdate( Self, oRenderer, TGIS_DrawMode.All ) ;

        try
          if assigned( FOnUpdate )
            then FOnUpdate( Self, oRenderer, TGIS_DrawMode.All )
            else Draw( oRenderer, TGIS_DrawMode.All ) ;

        finally
          if assigned( FOnAfterUpdate ) then
            FOnAfterUpdate( Self, oRenderer, TGIS_DrawMode.All ) ;
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;

          T_FMXBitmap(ctx.BaseMap).Canvas.BeginScene ;
          try
            if Assigned( ctx.Selection ) then
              T_FMXBitmap(ctx.BaseMap).Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Selection),
                RectF( 0, 0, T_FMXBitmap(ctx.Selection).Width,
                             T_FMXBitmap(ctx.Selection).Height),
                RectF( 0, 0, T_FMXBitmap(ctx.BaseMap).Width,
                             T_FMXBitmap(ctx.Selection).Height
                             / T_FMXBitmap(ctx.Selection).Width
                             * T_FMXBitmap(ctx.BaseMap).Width),
                oVwr.SelectionTransparency / 100, True
              ) ;
            if Assigned( ctx.Charts ) then
              T_FMXBitmap(ctx.BaseMap).Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Charts),
                RectF( 0, 0, T_FMXBitmap(ctx.Charts).Width,
                             T_FMXBitmap(ctx.Charts).Height),
                RectF( 0, 0, T_FMXBitmap(ctx.BaseMap).Width,
                             T_FMXBitmap(ctx.Charts).Height
                             / T_FMXBitmap(ctx.Charts).Width
                             * T_FMXBitmap(ctx.BaseMap).Width),
                       1, False
              ) ;
            if Assigned( ctx.Labels ) then
              T_FMXBitmap(ctx.BaseMap).Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Labels),
                RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                             T_FMXBitmap(ctx.Labels).Height),
                RectF( 0, 0, T_FMXBitmap(ctx.BaseMap).Width,
                             T_FMXBitmap(ctx.Labels).Height
                             / T_FMXBitmap(ctx.Labels).Width
                             * T_FMXBitmap(ctx.BaseMap).Width),
                       1, False
              ) ;
          finally
            oRenderer.ReleaseContext ;
          end ;

          if _full then begin
            bmp.Canvas.BeginScene;
              oRenderer.PaintExtra( Self, bmp.Canvas, FOnPaintExtra ) ;

              if assigned( FOnAfterPaint ) then
                FOnAfterPaint( Self, bmp.Canvas ) ;

              if assigned( FOnAfterPaintRenderer ) then
                oRenderer.PaintExtra( Self, bmp.Canvas, FOnAfterPaintRenderer ) ;
            bmp.Canvas.EndScene ;
          end;
        end ;

      finally
        FreeObject( ctx ) ;
        restore_basemap ;
        oVwr.IncrementalPaint := bincrmntl ;
        oBasemapHelper.UnlockBitmap ;
      end ;

    end ;

  begin
    if not assigned( _bitmap ) then exit ;

    bmp := _bitmap ;
    w := bmp.Width ;
    h := bmp.Height ;

    // paint background
    T_FMXBitmap(bmp).Clear( FMXColor(oVwr.Color) ) ;

    wextent := VisibleExtent ;
    try
      scl := 0 ;
      prepare_map( VisibleExtent, scl ) ;
    finally
      VisibleExtent := wextent ;
    end ;
  end ;

//==============================================================================
// mouse handling for zooming rectangle
//==============================================================================

  procedure TGIS_ViewerWnd.doZoomingRectMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    MouseUp( _button, _shift, _x, _y ) ;
  end;

  procedure TGIS_ViewerWnd.doZoomingRectMouseMove(
    _sender : TObject      ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    MouseMove( _shift,
               TControl(_sender).Position.X + _x,
               TControl(_sender).Position.Y + _y
             );
  end ;

//==============================================================================
// IGIS_Viewer property access routines
//==============================================================================

  function TGIS_ViewerWnd.fget_BigExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.BigExtent ;
  end;

  function TGIS_ViewerWnd.fget_BigExtentMargin
    : Integer ;
  begin
    Result := oVwr.BigExtentMargin ;
  end;

  procedure TGIS_ViewerWnd.fset_BigExtentMargin(
    const _value : Integer
  ) ;
  begin
    oVwr.BigExtentMargin := _value ;
  end;

  function TGIS_ViewerWnd.fget_KeepScale
    : Boolean ;
  begin
    Result := oVwr.KeepScale ;
  end ;

  procedure TGIS_ViewerWnd.fset_KeepScale(
    const _value : Boolean
  ) ;
  begin
    oVwr.KeepScale := _value ;
  end ;

  function TGIS_ViewerWnd.fget_BusyLevel
    : Integer ;
  begin
    Result := oVwr.BusyLevel ;
  end;

  function TGIS_ViewerWnd.fget_BusyText
    : String ;
  begin
    Result :=oVwr.BusyText ;
  end;

  function TGIS_ViewerWnd.fget_Center
    : TGIS_Point  ;
  begin
    Result := oVwr.Center ;
  end;

  procedure TGIS_ViewerWnd.fset_Center(
    const _value : TGIS_Point
  ) ;
  begin
    oWndHelper.UpdateCancel ;
    oVwr.Center := _value ;
  end;

  function TGIS_ViewerWnd.fget_CenterPtg
    : TGIS_Point  ;
  begin
    Result := oVwr.CenterPtg ;
  end;

  procedure TGIS_ViewerWnd.fset_CenterPtg(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.CenterPtg := _value ;
  end;

  function TGIS_ViewerWnd.fget_Color
    : TGIS_Color ;
  begin
    Result := oVwr.Color ;
  end;

  procedure TGIS_ViewerWnd.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    oVwr.Color := _value ;
  end;

  function TGIS_ViewerWnd.fget_Copyright
    : String ;
  begin
    Result := oVwr.Copyright ;
  end;

  function TGIS_ViewerWnd.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    Result := oVwr.CS ;
  end;

  procedure TGIS_ViewerWnd.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    oVwr.CS := _value ;
  end;

  function TGIS_ViewerWnd.fget_CustomPPI
    : Integer ;
  begin
    Result := oVwr.CustomPPI ;
  end ;

  procedure TGIS_ViewerWnd.fset_CustomPPI(
    const _value : Integer
  ) ;
  begin
    oVwr.CustomPPI := _value ;
  end ;

  function TGIS_ViewerWnd.fget_Editor
    : IGIS_Editor ;
  begin
    Result := oVwr.Editor ;
  end ;

  procedure TGIS_ViewerWnd.fset_Editor(
    const _value : IGIS_Editor
  ) ;
  begin
    if _value is TGIS_Editor then
      oVwr.Editor := TGIS_Editor( _value ) ;
  end ;

  function TGIS_ViewerWnd.fget_Extent
    : TGIS_Extent ;
  begin
    Result := oVwr.Extent ;
  end;

  function TGIS_ViewerWnd.fget_FileCopyrights
    : String      ;
  begin
    Result := oVwr.FileCopyrights ;
  end;

  function TGIS_ViewerWnd.fget_FontScale
    : Integer ;
  begin
    Result := oVwr.FontScale ;
  end ;

  procedure TGIS_ViewerWnd.fset_FontScale(
    const _value : Integer
  ) ;
  begin
    oVwr.FontScale := _value ;
  end ;

  function TGIS_ViewerWnd.fget_FullDrawExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.FullDrawExtent ;
  end;

  function TGIS_ViewerWnd.fget_IncrementalPaint
    : Boolean ;
  begin
    Result := oVwr.IncrementalPaint ;
  end;

  procedure TGIS_ViewerWnd.fset_IncrementalPaint(
    const _value : Boolean
  ) ;
  begin
    oVwr.IncrementalPaint := _value ;
  end;

  function TGIS_ViewerWnd.fget_InPaint
    : Boolean ;
  begin
    Result := oVwr.InPaint ;
  end;

  function TGIS_ViewerWnd.fget_IsBusy
    : Boolean ;
  begin
    Result := oVwr.IsBusy ;
  end;

  function TGIS_ViewerWnd.fget_IsEmpty
    : Boolean ;
  begin
    Result := oVwr.IsEmpty ;
  end;

  function TGIS_ViewerWnd.fget_IsLocked
   : Boolean ;
  begin
    Result := oVwr.IsLocked ;
  end;

  function TGIS_ViewerWnd.fget_IsTopmost
   : Boolean ;
  begin
    Result := oVwr.IsTopmost ;
  end;

  function TGIS_ViewerWnd.fget_Items
    : TGIS_LayerAbstractList ;
  begin
    Result := oVwr.Items ;
  end;

  function TGIS_ViewerWnd.fget_LabelsReg
    : TGIS_LabelsAreaAbstract ;
  begin
    Result := oVwr.LabelsReg ;
  end ;

  function TGIS_ViewerWnd.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    Result := oVwr.MultiUserMode ;
  end;

  procedure TGIS_ViewerWnd.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    oVwr.MultiUserMode := _value ;
  end;

  function TGIS_ViewerWnd.fget_CustomData
    : TGIS_StringList ;
  begin
    Result := oVwr.CustomData ;
  end;

  function TGIS_ViewerWnd.fget_OverlappedExtentMargin
    : Integer ;
  begin
    Result := oVwr.OverlappedExtentMargin ;
  end ;

  procedure TGIS_ViewerWnd.fset_OverlappedExtentMargin(
    const _value : Integer
  ) ;
  begin
    oVwr.OverlappedExtentMargin := _value ;
  end ;

  function TGIS_ViewerWnd.fget_PPI
    : Integer ;
  begin
    Result := oVwr.PPI ;
  end ;

  function TGIS_ViewerWnd.fget_ProjectFile
    : TGIS_ConfigAbstract ;
  begin
    Result := oVwr.ProjectFile
  end;

  procedure TGIS_ViewerWnd.fset_ProjectFile(
    const _value : TGIS_ConfigAbstract
  ) ;
  begin
    oVwr.ProjectFile := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ProjectName
    : String ;
  begin
    Result := oVwr.ProjectName
  end;

  function TGIS_ViewerWnd.fget_DelayedUpdate
    : Integer ;
  begin
    Result := oVwr.DelayedUpdate ;
  end;

  procedure TGIS_ViewerWnd.fset_DelayedUpdate(
    const _value : Integer
  ) ;
  begin
    oVwr.DelayedUpdate := _value ;
    oWndHelper.DelayedTime := oVwr.DelayedUpdate ;
  end;

  function TGIS_ViewerWnd.fget_ProgressiveUpdate
    : Integer ;
  begin
    Result := oVwr.ProgressiveUpdate ;
  end;

  procedure TGIS_ViewerWnd.fset_ProgressiveUpdate(
    const _value : Integer
  ) ;
  begin
    oVwr.ProgressiveUpdate := _value ;
  end;

  function TGIS_ViewerWnd.fget_RestrictedDrag
    : Boolean ;
  begin
    Result := oVwr.RestrictedDrag ;
  end;

  procedure TGIS_ViewerWnd.fset_RestrictedDrag(
    const _value : Boolean
  ) ;
  begin
    oVwr.RestrictedDrag := _value ;
  end;

  function TGIS_ViewerWnd.fget_RestrictedExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.RestrictedExtent ;
  end;

  procedure TGIS_ViewerWnd.fset_RestrictedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.RestrictedExtent := _value ;
  end;

  function TGIS_ViewerWnd.fget_RotationAngle
    : Double ;
  begin
    Result := oVwr.RotationAngle ;
  end;

  procedure TGIS_ViewerWnd.fset_RotationAngle(
    const _value : Double
  ) ;
  begin
    oVwr.RotationAngle := _value ;
  end;

  function TGIS_ViewerWnd.fget_RotationPoint
    : TGIS_Point ;
  begin
    Result := oVwr.RotationPoint ;
  end;

  procedure TGIS_ViewerWnd.fset_RotationPoint(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.RotationPoint := _value ;
  end;

  function TGIS_ViewerWnd.fget_ScaleAsFloat
    : Double ;
  begin
    Result := oVwr.ScaleAsFloat ;
  end;

  procedure TGIS_ViewerWnd.fset_ScaleAsFloat(
    const _value : Double
  ) ;
  begin
    oVwr.ScaleAsFloat := _value ;
  end;

  function TGIS_ViewerWnd.fget_ScaleAsText
    : String ;
  begin
    Result := oVwr.ScaleAsText ;
  end;

  procedure TGIS_ViewerWnd.fset_ScaleAsText(
    const _value : String
  ) ;
  begin
    oVwr.ScaleAsText := _value ;
  end;

  procedure TGIS_ViewerWnd.fset_Level(
    const _value : Double
  ) ;
  begin
    oVwr.Level := _value ;
  end ;

  function TGIS_ViewerWnd.fget_Level
    : Double ;
  begin
    Result := oVwr.Level ;
  end ;

  function TGIS_ViewerWnd.fget_SelectionGisColor
    : TGIS_Color ;
  begin
    Result := oVwr.SelectionGisColor ;
  end;

  procedure TGIS_ViewerWnd.fset_SelectionGisColor(
    const _value : TGIS_Color
  ) ;
  begin
    oVwr.SelectionGisColor := _value ;
  end;

  function TGIS_ViewerWnd.fget_SelectionOutlineOnly
    : Boolean ;
  begin
    Result := oVwr.SelectionOutlineOnly ;
  end;

  procedure TGIS_ViewerWnd.fset_SelectionOutlineOnly(
    const _value : Boolean
  ) ;
  begin
    oVwr.SelectionOutlineOnly := _value ;
  end;

  function TGIS_ViewerWnd.fget_SelectionTransparency
    : Integer ;
  begin
    Result := oVwr.SelectionTransparency ;
  end;

  procedure TGIS_ViewerWnd.fset_SelectionTransparency(
    const _value : Integer
  ) ;
  begin
    oVwr.SelectionTransparency := _value ;
  end;

  function TGIS_ViewerWnd.fget_SelectionWidth
    : Integer ;
  begin
    Result := oVwr.SelectionWidth ;
  end;

  procedure TGIS_ViewerWnd.fset_SelectionWidth(
    const _value : Integer
  ) ;
  begin
    oVwr.SelectionWidth := _value ;
  end;

  function TGIS_ViewerWnd.fget_SystemPPI
    : Integer ;
  begin
    Result := oVwr.SystemPPI ;
  end ;

  function TGIS_ViewerWnd.fget_ViewerParent
    : IGIS_ViewerParent ;
  begin
    Result := oVwr.ViewerParent ;
  end;

  function TGIS_ViewerWnd.fget_Viewport
    : TGIS_Point ;
  begin
    Result := oVwr.Viewport ;
  end;

  procedure TGIS_ViewerWnd.fset_Viewport(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.Viewport := _value ;
  end;

  function TGIS_ViewerWnd.fget_VisibleExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.VisibleExtent ;
  end;

  procedure TGIS_ViewerWnd.fset_VisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.VisibleExtent := _value ;
  end;

  function TGIS_ViewerWnd.fget_UseAnimations
    : Boolean ;
  begin
    Result := oVwr.UseAnimations ;
  end;

  procedure TGIS_ViewerWnd.fset_UseAnimations(
    const _value : Boolean
  ) ;
  begin
    oVwr.UseAnimations := _value ;
  end;

  function TGIS_ViewerWnd.fget_UseRTree
    : Boolean ;
  begin
    Result := oVwr.UseRTree ;
  end;

  procedure TGIS_ViewerWnd.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    oVwr.UseRTree := _value ;
  end;

  function TGIS_ViewerWnd.fget_Zoom
    : Double ;
  begin
    Result := oVwr.Zoom ;
  end;

  procedure TGIS_ViewerWnd.fset_Zoom(
    const _value : Double
  ) ;
  var
    zm : Double ;
    pt : TPoint ;
  begin
    if IsEmpty then
      exit ;

    if not SynchronizePaint( True) then
      exit ;

    zm := oVwr.Zoom ;
    pt := MapToScreen( CenterPtg ) ;

    oWndHelper.DoZoom( pt.X * iCanvasScale,
                       pt.Y * iCanvasScale,
                       _value / zm, True, False
                     ) ;

    oVwr.Zoom := _value ;
  end;

  function TGIS_ViewerWnd.fget_ZoomEx
    : Double ;
  begin
    Result := oVwr.ZoomEx ;
  end;

  procedure TGIS_ViewerWnd.fset_ZoomEx(
    const _value : Double
  ) ;
  var
    zm : Double ;
    pt : TPoint ;
  begin
    if IsEmpty then
      exit ;

    if not SynchronizePaint( True ) then
      exit ;

    zm := oVwr.ZoomEx ;
    pt := MapToScreen( CenterPtg ) ;

    oWndHelper.DoZoom( pt.X * iCanvasScale,
                       pt.Y * iCanvasScale,
                       _value / zm, True, False
                     ) ;

    oVwr.ZoomEx := _value ;
  end;

  function TGIS_ViewerWnd.fget_UponDestroy
    : Boolean ;
  begin
    Result := oVwr.UponDestroy ;
  end;

  function  TGIS_ViewerWnd.fget_TemporaryScaleInternal : Double      ;
  begin
    Result := oVwr.TemporaryScaleInternal ;
  end ;

  procedure TGIS_ViewerWnd.fset_TemporaryScaleInternal(
    const _value : Double
  ) ;
  begin
    oVwr.TemporaryScaleInternal := _value ;
  end;

  function  TGIS_ViewerWnd.fget_TemporaryVisibleExtent : TGIS_Extent ;
  begin
    Result := oVwr.TemporaryVisibleExtent ;
  end ;

  procedure TGIS_ViewerWnd.fset_TemporaryVisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.TemporaryVisibleExtent := _value ;
  end;

  function TGIS_ViewerWnd.fget_Hierarchy
    : IGIS_HierarchyManager ;
  begin
    Result := oVwr.Hierarchy ;
  end ;

//==============================================================================
// TGIS_ViewerWnd property access routines
//==============================================================================

  function TGIS_ViewerWnd.fget_BackgroundColor
    : TAlphaColor ;
  begin
    Result := FMXColor( oVwr.Color ) ;
  end ;

  procedure TGIS_ViewerWnd.fset_BackgroundColor(
    const _value : TAlphaColor
  ) ;
  begin
    oVwr.Color := GISColor( _value ) ;

    if _value <> FBackgroundColor then begin
      FBackgroundColor := _value ;
      Repaint ;
    end ;
  end ;

  function TGIS_ViewerWnd.fget_ActiveBackgroundColor
    : TAlphaColor ;
  begin
    {$IFDEF LEVEL_RX10_FMX}
    if TGIS_ViewerWndStyledSetting.Background in StyledSettings then
      Result := oStyledCtlColor
    else
    {$ENDIF}
      Result := BackgroundColor ;
  end ;

  function TGIS_ViewerWnd.fget_HiRes
    : Boolean ;
  begin
    Result := FHiRes
  end ;

  procedure TGIS_ViewerWnd.fset_HiRes(
    const _value : Boolean
  ) ;
  var
    ws   : IFMXWindowService;
    sc   : Single  ;
    dtmp : Single ;
    ex   : TGIS_Extent ;
  begin
    if _value then
      FHiRes := True
    else
      FHiRes := False ;

    if not ( csDesigning in ComponentState ) then begin
      ws := TPlatformServices.Current.GetPlatformService(
              IFMXWindowService
            ) as IFMXWindowService;
      if not assigned( self.Root ) then
        sc := 1
      else
        sc := ws.GetWindowScale( TCustomForm( self.Root) ) ;
      dtmp := sc ;
      if not _value then begin
        if sc >= 2 then
          dtmp := sc / 2
      end;

      if dtmp <> iCanvasScale then begin
        if csReading in ComponentState then begin
          iCanvasScale := dtmp ;
        end
        else begin
          Lock ;
          try
            ex := VisibleExtent ;
            iCanvasScale := dtmp ;
            VisibleExtent := ex ;
            InvalidateWholeMap ;
          finally
            Unlock ;
          end ;
        end ;
      end
    end;
  end ;

  procedure TGIS_ViewerWnd.fset_Mode(
    const _value : TGIS_ViewerMode
  ) ;
  var
    edt_change : Boolean ;
  begin
    edt_change := FMode = TGIS_ViewerMode.Edit ;

    FMode := _value ;
    ensureCursor ;
    FDragging := false ;
    FZooming := false ;
    FEditing := false ;

    if assigned( FOnModeChange ) then
      FOnModeChange( self ) ;

    edt_change := not Editor.InEdit
                  and
                  edt_change
                  or
                  ( FMode = TGIS_ViewerMode.Edit )
                  and
                  Editor.ShowTracking
                  and
                  assigned( Editor.SnapLayer ) ;

    Editor.ViewerEnabled := FMode = TGIS_ViewerMode.Edit ;

    if edt_change then
      InvalidateEditor(True) ;
  end;

  procedure TGIS_ViewerWnd.fset_Renderer(
    const _value : TGIS_RendererAbstract
  ) ;
  begin
    Assert( Assigned( _value ) ) ;
    FreeObject( oRenderer ) ;
    oRenderer := _value ;
  end ;

  function TGIS_ViewerWnd.fget_View3D : Boolean ;
  begin
    Result := Assigned( FViewer3D ) ;
  end;

  procedure TGIS_ViewerWnd.fset_View3D( const _value : Boolean ) ;
  var
    ext : TGIS_Extent   ;
    obj : TGIS_Viewer3D ;
  begin
    if _value and Assigned( FViewer3D ) then exit ;
    if (not _value) and (not Assigned( FViewer3D )) then exit ;
    if IsEmpty then exit ;

    if _value then begin
      if not SynchronizePaint(False) then exit ;
      // release all 2D drawing buffers
      FreeObject( FCacheBitmap ) ;
      oRenderer.ReleaseContext ;
      FDrawContext.Clear ;
      FTopContext.Clear ;
      FFlashContext.Clear ;

      oldMode2D := Mode ;

      obj := TGIS_Viewer3D.Create( Self );
      obj.Parent := Self ;
      obj.Align := TAlignLayout.Contents ;

      {$IFDEF GIS_MOBILE}
         // Mobile bug in cliping 3D
         oldClipChildren := ClipChildren ;
      {$ENDIF}

      obj.InitRenderer( Self );

      FViewer3D := obj ;

      NotifySubscribers( GIS_SUBSCRIBED_3D_UPDATE, nil );
    end
    else begin
      if TGIS_Viewer3D( FViewer3D ).IsBusy then exit ;

      ext := GisExtent( FViewer3D.VisibleExtent3D.XMin,
                        FViewer3D.VisibleExtent3D.YMin,
                        FViewer3D.VisibleExtent3D.XMax,
                        FViewer3D.VisibleExtent3D.YMax
                      ) ;

      {$IFDEF GIS_MOBILE}
        ClipChildren := oldClipChildren ;
      {$ENDIF}

      obj := TGIS_Viewer3D( FViewer3D ) ;
      obj.Parent := nil ;

      FViewer3D := nil ;

      {$IFDEF NEXTGEN}
        obj.DisposeOf ;
      {$ELSE}
        obj.Free ;
      {$ENDIF}


      Lock ;
      try
        VisibleExtent := ext ;
        InvalidateWholeMap ;
      finally
        Unlock ;
      end;

      NotifySubscribers( 1, nil );

      Mode := oldMode2D ;
    end;
  end;

  function  TGIS_ViewerWnd.fget_Viewer3D : IGIS_Viewer3D ;
  begin
    Result := FViewer3D ;
  end ;

  procedure TGIS_ViewerWnd.fset_Viewer3D( const _viewer : IGIS_Viewer3D ) ;
  begin
    FViewer3D := _viewer ;
  end ;


//==============================================================================
// reintroduced properties
//==============================================================================

  function TGIS_ViewerWnd.fget_SelectionColor
    : TAlphaColor ;
  begin
    Result := oVwr.SelectionGisColor.ARGB ;
  end ;

  procedure TGIS_ViewerWnd.fset_SelectionColor(
    const _value : TAlphaColor
  ) ;
  begin
    oVwr.SelectionGisColor := TGIS_Color.FromARGB( _value ) ;
  end ;

//==============================================================================
// property access routines of TGIS_Viewer's event handlers
//==============================================================================

  procedure TGIS_ViewerWnd.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    oVwr.BusyEvent := _value ;
  end;

  function TGIS_ViewerWnd.fget_BusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := oVwr.BusyEvent ;
  end;

  procedure TGIS_ViewerWnd.fset_HelpEvent(
    const _value : TGIS_HelpEvent
  ) ;
  begin
    oVwr.HelpEvent := _value ;
  end;

  function TGIS_ViewerWnd.fget_HelpEvent
    : TGIS_HelpEvent ;
  begin
    Result := oVwr.HelpEvent ;
  end;

  function TGIS_ViewerWnd.fget_EditorSnapPointEvent
    : TGIS_EditorSnapPointEvent ;
  begin
    Result := oVwr.EditorSnapPointEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_EditorSnapPointEvent(
    const _value : TGIS_EditorSnapPointEvent
  ) ;
  begin
    oVwr.EditorSnapPointEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_EditorPointChangeEvent
    : TGIS_EditorPointChangeEvent ;
  begin
    Result := oVwr.EditorPointChangeEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_EditorPointChangeEvent(
    const _value : TGIS_EditorPointChangeEvent
  ) ;
  begin
    oVwr.EditorPointChangeEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_EditorPointMoveEvent
    : TGIS_EditorPointMoveEvent ;
  begin
    Result := oVwr.EditorPointMoveEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_EditorPointMoveEvent(
    const _value : TGIS_EditorPointMoveEvent
  ) ;
  begin
    oVwr.EditorPointMoveEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ExtentChangeEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.ExtentChangeEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_ExtentChangeEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.ExtentChangeEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_LayerAddEvent
    : TGIS_LayerEvent ;
  begin
    Result := oVwr.LayerAddEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_LayerAddEvent(
    const _value : TGIS_LayerEvent
  ) ;
  begin
    oVwr.LayerAddEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_LayerDeleteEvent
    : TGIS_LayerEvent ;
  begin
    Result := oVwr.LayerDeleteEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_LayerDeleteEvent(
    const _value : TGIS_LayerEvent
  ) ;
  begin
    oVwr.LayerDeleteEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_PaintExceptionEvent
    : TGIS_PaintExceptionEvent ;
  begin
    Result := oVwr.PaintExceptionEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_PaintExceptionEvent(
    const _value : TGIS_PaintExceptionEvent
  ) ;
  begin
    oVwr.PaintExceptionEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_PasswordEvent
    : TGIS_TemplateProducerEvent ;
  begin
    Result := oVwr.PasswordEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_PasswordEvent(
    const _value : TGIS_TemplateProducerEvent
  ) ;
  begin
    oVwr.PasswordEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ProjectCloseEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.ProjectCloseEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_ProjectCloseEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.ProjectCloseEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ProjectOpenEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.ProjectOpenEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_ProjectOpenEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.ProjectOpenEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_VisibleExtentChangeEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.VisibleExtentChangeEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_VisibleExtentChangeEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.VisibleExtentChangeEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ZoomChangeEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.ZoomChangeEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_ZoomChangeEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.ZoomChangeEvent := _value ;
  end ;

//==============================================================================
// overridden protected methods
//==============================================================================

  procedure TGIS_ViewerWnd.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  var
    dx, dy : Single ;
    ix, iy : Integer ;
  begin
    if InPaint then begin
      oWndHelper.UpdateCancelOptional ;
      exit ;
    end;

    if IsEmpty then
      exit ;
    if mouseBlocked then
      exit ;

    if oWndHelper.GestureActive then
      exit ;

    inherited ;

    dx := _x ;
    dy := _y ;
    ix := RoundS( dx ) ;
    iy := RoundS( dy ) ;

    ensureCursor ;

    if oWndHelper.UpdateCancelOptional then
      exit ;

    oWndHelper.UpdateCancel ;

    resetMousePos( ix, iy );

    if View3D then begin
      if FViewer3D.Mode = TGIS_Viewer3DMode.Select then begin
        oWndHelper.GestureMouseDown(
          ssShift  in _shift,
          ssAlt    in _shift,
          ssCtrl   in _shift,
          ssLeft   in _shift,
          ssRight  in _shift,
          ssMiddle in _shift,
          ssTouch  in _shift,
          ssPen    in _shift,
          dx,
          dy
        )
      end
      else begin
        if FModeMouseButton = _button then
          FViewer3D.DoMouseDown( ix, iy ) ;
      end ;

      exit ;
    end ;

    oWndHelper.GestureMouseDown(
      ssShift  in _shift,
      ssAlt    in _shift,
      ssCtrl   in _shift,
      ssLeft   in _shift,
      ssRight  in _shift,
      ssMiddle in _shift,
      ssTouch  in _shift,
      ssPen    in _shift,
      dx,
      dy
    );

    if FModeMouseButton <> _button then exit ;

    case FMode of
      TGIS_ViewerMode.Select: // beginning of select mode
        begin
        end;

      TGIS_ViewerMode.Drag:   // beginning of dragging mode
        begin
          FDragging := True;
        end;

      TGIS_ViewerMode.Zoom:   // beginning of rectangle zoom mode
        begin
          FZooming := True;
          resetMousePos( ix, iy );
        end;

      TGIS_ViewerMode.ZoomEx: // beginning of rubber zooming mode
        begin
          FZooming := True;
        end;

      TGIS_ViewerMode.Edit:   // beginning of editing mode
        begin
          if ( oWndHelper.GestureState.DownCount = 1 ) or
             ( ssTouch in _shift ) or ( ssPen in _shift ) then begin
            FEditing := True;
            if ssTouch in _shift then
              Editor.PointerMode := TGIS_PointerMode.Touch
            else if ssPen in _shift then
              Editor.PointerMode := TGIS_PointerMode.Pen
            else
              Editor.PointerMode := TGIS_PointerMode.Mouse ;
            Editor.MouseBegin( Point( RoundS(ix * iCanvasScale), RoundS(iy * iCanvasScale) ),
                               ( ssCtrl in _shift )
                             ) ;
          end;
        end;
    end ;
  end ;

  procedure TGIS_ViewerWnd.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  var
    dx, dy : Single  ;
    ix, iy : Integer ;
  begin
    if IsEmpty then
      exit;
    if mouseBlocked then
      exit ;

    dx := _x ;
    dy := _y ;
    ix := RoundS( dx ) ;
    iy := RoundS( dy ) ;

    ensureCursor;

    try
      if View3D then begin
        if FViewer3D.Mode = TGIS_Viewer3DMode.Select then begin
          if oWndHelper.GestureState.GestureCnt > 0 then
            exit ; // on iOS zoom geture causes MouseUp

          oWndHelper.GestureMouseUp(
            ssShift  in _shift,
            ssAlt    in _shift,
            ssCtrl   in _shift,
            ssLeft   in _shift,
            ssRight  in _shift,
            ssMiddle in _shift,
            ssTouch  in _shift,
            ssPen    in _shift,
            dx,
            dy
          );
        end
        else begin
          if FModeMouseButton = _button then
          FViewer3D.DoMouseUp ;
        end ;

        exit ;
      end ;

      if FModeMouseButton = _button then begin
        case FMode of
          TGIS_ViewerMode.Select: // end of select mode
              begin
              if oWndHelper.GestureState.GestureCnt > 0 then
                exit ; // on iOS zoom geture causes MouseUp

              if FAutoCenter then
                CenterViewport( ScreenToMap( Point( ix, iy ) ) );
            end;

          TGIS_ViewerMode.Drag:   // end of dragging mode
            begin
              if not FDragging then
                exit;

              oWndHelper.FitScreenAnimation ;
              oWndHelper.UpdateDelayed ;

              FDragging := False;
            end;

          TGIS_ViewerMode.Zoom:   // end of rectangle zoom mode
            begin
              if not FZooming then
                exit;

              if mousePosOld.x <> NO_MOUSE_POS then
              begin
                // oZoomingRect.Visible := False;
                // mark zooming rect to be turned off upon next paint
                oZoomingRect.Tag := 1;

                oWndHelper.DoZoom( mousePos.X    * iCanvasScale,
                                   mousePos.Y    * iCanvasScale,
                                   mousePosOld.X * iCanvasScale,
                                   mousePosOld.Y * iCanvasScale
                                 ) ;
                oWndHelper.UpdateDelayed ;
              end;

              FZooming := False;
            end;

          TGIS_ViewerMode.ZoomEx: // end of rubber zooming mode
            begin
              if not FZooming then
                exit;
              oWndHelper.FitScreenAnimation ;
              oWndHelper.UpdateDelayed ;

              FZooming := False;
            end;

          TGIS_ViewerMode.Edit: // end of editing mode
            begin
              if not FEditing then
                exit;

              if Editor.ShowDraggingTrack then begin
                // hide any previous line
                if mousePosOld.X <> NO_MOUSE_POS then begin
                  oDraggingTrack.Visible := False;
                end ;
              end ;
              Editor.MouseEnd( Point( RoundS(ix * iCanvasScale), RoundS(iy * iCanvasScale) ) ) ;
              FEditing := False ;
            end;
        end ;
      end ;

      oWndHelper.GestureMouseUp(
        ssShift  in _shift,
        ssAlt    in _shift,
        ssCtrl   in _shift,
        ssLeft   in _shift,
        ssRight  in _shift,
        ssMiddle in _shift,
        ssTouch  in _shift,
        ssPen    in _shift,
        dx,
        dy
      );

      oWndHelper.GestureEnd( True ) ; // end of gesture for sure

    finally
      inherited ;
    end;
  end ;

  procedure TGIS_ViewerWnd.MouseMove(
     _shift : TShiftState ;
     _x     : Single      ;
     _y     : Single
  ) ;
  var
    ix, iy : Integer;
    dx, dy : Single;
    zm     : Double;

    procedure draw_zooming_rect(
       _x1, _y1 : Integer ;
       _x2, _y2 : Integer
    ) ;
    begin
      if IsEmpty then exit ;

      oZoomingRect.Visible := True;
      oZoomingRect.BringToFront;

      if _x2 < 0 then begin
        _x1 := _x1 + _x2;
        _x2 := Abs(_x2);
      end;

      if _y2 < 0 then begin
        _y1 := _y1 + _y2;
        _y2 := Abs(_y2);
      end;

      oZoomingRect.Position.x := _x1 ;
      oZoomingRect.Position.y := _y1 ;
      oZoomingRect.Width      := _x2 ;
      oZoomingRect.Height     := _y2 ;
      oZoomingRect.Stroke.Color := HSLtoRGB(Random(100) / 100, 1, 0.5);
    end;

    procedure draw_dragging_track(
       _x1, _y1 : Integer ;   // x, y
       _x2, _y2 : Integer     // start pos
    ) ;
    var
      x1, y1 : Single  ;
      x2, y2 : Single  ;
      ww, hh : Single ;
    begin
      if IsEmpty then exit ;

      x1 := _x1 ;
      y1 := _y1 ;
      x2 := _x2 ;
      y2 := _y2 ;

      ww := x1 - x2 ;
      hh := y1 - y2 ;

      oDraggingTrack.Position.Point := PointF( x2, y2 ) ;
      oDraggingTrack.Width          := Sqrt( ww*ww + hh*hh ) ;
      oDraggingTrack.RotationAngle  :=  RadToDeg( ArcTan2( hh, ww ) ) ;
      oDraggingTrack.Stroke.Color   := HSLtoRGB(Random(100) / 100, 1, 0.5) ;

      oDraggingTrack.Visible := True;
      oDraggingTrack.BringToFront;
    end ;

  begin
    if mouseBlocked then
      exit ;

    dx := _x ;
    dy := _y ;
    ix := RoundS( dx ) ;
    iy := RoundS( dy ) ;

    ensureCursor;

    mousePosLast.X := RoundS(dx);
    mousePosLast.Y := RoundS(dy);

    oWndHelper.GestureMouseMove(
      ssShift  in _shift,
      ssAlt    in _shift,
      ssCtrl   in _shift,
      ssLeft   in _shift,
      ssRight  in _shift,
      ssMiddle in _shift,
      ssTouch  in _shift,
      ssPen    in _shift,
      dx,
      dy
    );

    try
      // solve  mouse-moved-outside-control case
      if _shift = [] then begin
        if FZooming or FDragging then
          doZoomingRectMouseUp( Self, TMouseButton.mbLeft, [], dx, dy );
        exit;
      end;

      if View3D then begin
        if not oWndHelper.GestureActive then
          Viewer3D.DoMouseMove( ix, iy ) ;
        exit ;
      end ;

      ix := RoundS(dx);
      iy := RoundS(dy);

      // end of drag or zoom or edit mode
      if _shift = [] then begin
        FDragging := False;
        FZooming  := False;
        FEditing  := False;
      end;

      case FMode of

        TGIS_ViewerMode.Select: // select mode
          begin
          end;

        TGIS_ViewerMode.Drag:   // dragging mode
          begin
            if not FDragging then
              exit;

            if (ix < 0) or (iy < 0) or
               (ix > ControlCanvasWidth) or (iy > ControlCanvasHeight)
            then
              exit;

            oWndHelper.DoDrag( ( mousePos.x - ix ) * iCanvasScale,
                               ( mousePos.y - iy ) * iCanvasScale
                             ) ;

            resetMousePos(ix, iy);
          end;

        TGIS_ViewerMode.Zoom:   // rectangle zoom mode
          begin
            if not FZooming then
              exit;

            // restrict to window size
            if ix < 0 then
              ix := 0;
            if iy < 0 then
              iy := 0;
            if ix > Width then
              ix := Trunc( Width )- 1;
            if iy > Height then
              iy := Trunc( Height ) - 1;

            // hide any previous frame
            if mousePosOld.x <> NO_MOUSE_POS then begin
              resetMousePos(mousePos.x, mousePos.y);
            end;

            // allow zooming only for rectangle grater then MinZoomSize
            if Sqrt( Sqr( mousePos.x - ix ) +  Sqr( mousePos.y - iy ) )
               <
               TwipsToPixels( MinZoomSize )
            then begin
              oZoomingRect.Visible := False;
              exit ;
            end;

            // draw current frame
            draw_zooming_rect(ix, iy, mousePos.x - ix, mousePos.y - iy);
            mousePosOld.x := ix;
            mousePosOld.y := iy;
          end;

        TGIS_ViewerMode.ZoomEx: // rubber zooming mode
          begin
            if not FZooming then
              exit;

            if mousePosOld.X <> NO_MOUSE_POS then begin
              zm := Power(1 + 1.0 * Abs(mousePosLast.Y - mousePosOld.Y)/Height,
                          10);

              if zm = 1 then
                exit;

              if mousePosOld.Y > mousePosLast.Y then
                zm := 1.0 / zm;

              oWndHelper.DoZoom( mousePos.X* iCanvasScale,
                                 mousePos.Y* iCanvasScale,
                                 zm,
                                 False,
                                 False
                               ) ;
            end ;
            mousePosOld.X := RoundS(dx);
            mousePosOld.Y := RoundS(dy);
          end;

        TGIS_ViewerMode.Edit:
          begin
            if not FEditing then
              exit;
            if Editor.ShowDraggingTrack then begin
              // draw current line
              //zooming_color := Random($FFFFFF) ;
              draw_dragging_track( ix, iy, mousePos.X, mousePos.Y ) ;
            end ;
            mousePosOld.X := RoundS(dx);
            mousePosOld.Y := RoundS(dy);
            Editor.MouseMove( Point( RoundS(ix * iCanvasScale), RoundS(iy * iCanvasScale)) ) ;
          end;

      end ;
    finally
      inherited ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.ParentChanged ;
    function parent_form :  TCustomForm ;
    var
      tmp : TFmxObject ;
    begin
      tmp := self ;
      while Assigned( tmp ) do begin
        if tmp is TCustomForm then
          break ;
          tmp := tmp.Parent ;
      end;
      if tmp = nil then begin
        Result := nil ;
        exit ;
      end ;
      Assert( tmp is TCustomForm ) ;
      Result := TCustomForm( tmp ) ;
    end;
  begin
    inherited ;

    oParentForm := parent_form ;
  end;

  procedure TGIS_ViewerWnd.Paint ;
  var
    i              : Integer ;
    rnd            : TGIS_RendererFmx ;
    rct_dst        : TRectF  ;
    rct_dst_scaled : TRectF  ;
    cnv            : TCanvas ;
    dscaled        : Single  ;

    {$IFDEF LEVEL_RX10_FMX}
      procedure draw_styled_background ;
      var
        off  : TPointF    ;
        mtrx : TMatrix    ;

      begin
        if not ( TGIS_ViewerWndStyledSetting.Background in StyledSettings ) then
          exit ;

        if Assigned( oStlWnd ) then begin
          mtrx := Canvas.Matrix ;
          try
            oStlWnd.Width  := Width ;
            oStlWnd.Height := Height ;
            oStlWnd.SetNewScene( Scene ) ;

            off := Self.LocalToAbsolute( PointF(0,0) ) ;

            oStlWnd.PaintTo(
              Canvas,
              RectF(
                off.X,
                off.Y,
                oStlWnd.Width  + off.X,
                oStlWnd.Height + off.Y
              )
            ) ;
          finally
            Canvas.SetMatrix(mtrx) ;
         end;
        end ;

        if Assigned( oStlCtl ) then begin
          mtrx := Canvas.Matrix ;
          try
            oStlCtl.Width  := Width  + oStlCtl.Padding.Left + oStlCtl.Padding.Right ;
            oStlCtl.Height := Height + oStlCtl.Padding.Top  + oStlCtl.Padding.Bottom ;
            oStlCtl.SetNewScene( Scene ) ;

            off := Self.LocalToAbsolute(
                     PointF(-oStlCtl.Padding.Left, -oStlCtl.Padding.Top)
                   ) ;

            oStlCtl.PaintTo(
              Canvas,
              RectF(
                off.X,
                off.Y,
                oStlCtl.Width  + off.X,
                oStlCtl.Height + off.Y
              )
            ) ;
          finally
            Canvas.SetMatrix(mtrx) ;
          end;
        end ;
      end;
    {$ENDIF}

    function draw_topmost
      : Boolean ;
    begin
      Result := False ;
      if bInUpdate and not bInFlash then exit ;
      bInUpdate := True ;

      if assigned( FTopContext.BaseMap ) and
         ( ( T_FMXBitmap(FTopContext.BaseMap).Width  <> ControlCanvasWidth  ) or
           ( T_FMXBitmap(FTopContext.BaseMap).Height <> ControlCanvasHeight )
         )
      then
        FTopContext.Clear ;

      if not assigned( FTopContext.BaseMap ) then
        FTopContext.AssignBaseMap( nil, True )
      else
        T_FMXBitmap(FTopContext.BaseMap).Clear( TAlphaColorRec.Null ) ;
      if metPaintTopmostLabelsOnTop then
        FTopContext.AssignLabels( nil, True )
      else
        FTopContext.AssignLabels( nil, False ) ;

      oRenderer.ReleaseContext ;
      oRenderer.CreateContext( Self, oVwr, FTopContext,
                               Point(0, 0),
                               ControlCanvasWidth, ControlCanvasHeight,
                               PPI, FontScale
                             ) ;
      try
        oVwr.LabelsReg.Reset ;
        oRenderer.BeforeDraw ;
        oVwr.BeginPaintInternal ;
        if assigned( FOnBeforeUpdate ) then
          FOnBeforeUpdate( Self, oRenderer, TGIS_DrawMode.Top ) ;

        if assigned( FOnUpdate )
          then FOnUpdate( Self, oRenderer, TGIS_DrawMode.Top )
          else Draw( oRenderer, TGIS_DrawMode.Top ) ;

      finally
        if assigned( FOnAfterUpdate ) then
          FOnAfterUpdate( Self, oRenderer, TGIS_DrawMode.Top ) ;
        oVwr.EndPaintInternal ;
        oRenderer.AfterDraw ;
        bInUpdate := False ;
        Result := True ;
      end ;
    end ;

    procedure check_ppi_change ;
    var
      ws   : IFMXWindowService;
      sc   : Single  ;
    begin
      ws := TPlatformServices.Current.GetPlatformService(
              IFMXWindowService
            ) as IFMXWindowService;
      if not assigned( self.Root ) then
        sc := 1
      else
        sc := ws.GetWindowScale( TCustomForm( self.Root) ) ;

      if ( not FHiRes ) and ( sc >= 2 ) then
        sc := sc / 2 ;

      if sc <> iCanvasScale then begin
        iCanvasScale := sc ;
        Resize ;
      end;
    end;

    function valid_bmp(
      const _bmp : FMX.Graphics.TBitmap
    ) : Boolean ; overload ;
    begin
      Result := Assigned( _bmp )
                and
                ( _bmp.Width  > 0 )
                and
                ( _bmp.Height > 0 )
    end;

    function valid_bmp(
      const _bmp : TGIS_Bitmap
    ) : Boolean ; overload ;
    begin
      Result := Assigned( _bmp )
                and
                ( _bmp.Width  > 0 )
                and
                ( _bmp.Height > 0 )
    end;

    procedure rewire_OnTouch ;
    var
      tmp : TTouchEvent ;

      function same_method(aMethod1, aMethod2: TMethod): boolean;
      begin
        Result := (aMethod1.Code = aMethod2.Code) and
                  (aMethod1.Data = aMethod2.Data);
      end;

    begin
      if not Assigned( oParentForm ) then
        exit ;

      tmp := doTouch ;
      if not same_method( TMethod(oParentForm.OnTouch), TMethod( tmp ) ) then
        oOnTouchOriginal := oParentForm.OnTouch
      else
        oOnTouchOriginal := nil ;

     oParentForm.OnTouch := doTouch ;
    end;

  begin
    if oVwr.Parent as TObject <> self then
      exit ;

    try
      if View3D then begin
        Viewer3D.ControlRepaint ;
        exit ;
      end;

      metPaintNavigateFeedbackFullCache := GisMetadataAsBoolean(
        METADATA_PAINTNAVIGATEFEEDBACKFULLCACHE,
        True
      ) ;
      metPaintProgressiveTransparency := GisMetadataAsInteger(
        METADATA_PAINTPROGRESSIVETRANSPARENCY,
        60
      ) ;
      metPaintProgressiveTransparency :=
        Min(
          100,
          Max( 0, metPaintProgressiveTransparency )
        ) ;
      metPaintProgressiveFullCache := GisMetadataAsBoolean(
        METADATA_PAINTPROGRESSIVEFULLCACHE,
        True
      ) ;
      metPaintLabelsOnTop := GisMetadataAsBoolean(
         METADATA_PAINTLABELSONTOP,
         False
      ) ;
      metPaintTopmostLabelsOnTop := GisMetadataAsBoolean(
         METADATA_PAINTTOPMOSTLABELSONTOP,
         False
      ) ;

      rewire_OnTouch ; // way to bypass OnGesture limitation - no number of touches

      if oZoomingRect.Tag = 1 then begin
        // must hide zooming rect
        oZoomingRect.Tag := 0;
        oZoomingRect.Visible := False;
      end;

      if not ( csDesigning in ComponentState ) then
        check_ppi_change ;

      Canvas.Fill.Kind := TBrushKind.Solid ;
      Canvas.Fill.Color := FMXColor( oVwr.Color ) ;

      Canvas.FillRect( RectF( 0, 0, Width, Height ), 0, 0,
        [ TCorner.TopLeft,
          TCorner.TopRight,
          TCorner.BottomLeft,
          TCorner.BottomRight
        ]
       , 1
      );
      {$IFDEF LEVEL_RX10_FMX}
        draw_styled_background ;
      {$ENDIF}

      if assigned( FOnBeforePaintRenderer ) then
        oRenderer.PaintExtra( Self, Canvas, FOnBeforePaintRenderer ) ;
      if assigned( FOnBeforePaint ) then
        FOnBeforePaint( Self, Canvas ) ;
      try
        if IsEmpty then
          exit ;

        if not ( oWndHelper.IsScaled or FZooming )
           and
           Assigned( FCacheBitmap )
           and
           ( FCacheBitmap.Width  = ControlCanvasWidth  )
           and
           ( FCacheBitmap.Height = ControlCanvasHeight )
           and
           ( not Assigned( FProgressBitmap ) )
        then begin
          if metPaintNavigateFeedbackFullCache then begin
            if Assigned( oFullCache ) then begin
              if ( oFullCache.Width <> ControlCanvasWidth )
                 or
                 ( oFullCache.Height <> ControlCanvasHeight )
              then
                FreeObject( oFullCache ) ;
            end;

            if not Assigned( oFullCache ) then begin
              oFullCache := T_FMXBitmap.Create(
                              ControlCanvasWidth,
                              ControlCanvasHeight
                            ) ;
            end;
            oFullCache.Clear( TAlphaColorRec.Null );
            cnv := oFullCache.Canvas ;
            cnv.BeginScene ;

            rct_dst := RectF( 0, 0, ControlCanvasWidth, ControlCanvasHeight ) ;
            dscaled := 1 ;
          end
          else begin
            cnv := Canvas ;

            rct_dst := RectF( 0, 0, Width,
                              FCacheBitmap.Height/FCacheBitmap.Width * Width
                            ) ;
            dscaled := Width / ControlCanvasWidth
          end;

          rct_dst_scaled := RectF( 0, 0, Width,
                              FCacheBitmap.Height/FCacheBitmap.Width * Width
                            ) ;


          oBaseMapHelper.LockBitmap ;
          try
            if oBaseMapHelper.Active
               and
               valid_bmp( oBaseMapHelper.Bitmap )
               and
               ( not GisIsEmptyExtent( VisibleExtent ) )
            then begin
              cnv.DrawBitmap(
                T_FMXBitmap( oBaseMapHelper.Bitmap.NativeBitmap ),
                RectF(
                  0, 0,
                  oBaseMapHelper.Bitmap.Width, oBaseMapHelper.Bitmap.Height
                ),
                oWndHelper.ScaledRect(
                  oBaseMapHelper.Extent,
                  oBaseMapHelper.Bitmap.Width,
                  oBaseMapHelper.Bitmap.Height,
                  dscaled
                ),
                1, False
              );
            end;
            oBaseMapHelper.Request(
              ControlCanvasWidth,
              ControlCanvasHeight,
              VisibleExtent,
              RotationPoint,
              RotationAngle
            ) ;
          finally
            oBaseMapHelper.UnLockBitmap ;
          end ;

          cnv.DrawBitmap(
            FCacheBitmap,
            RectF( 0, 0, FCacheBitmap.Width, FCacheBitmap.Height),
            rct_dst,
            1, True
          ) ;
          oPaintCacheTmp := FCacheBitmap ;

          if not InPaint then begin
            if assigned( FDrawContext.Selection ) then
              cnv.DrawBitmap(
                T_FMXBitmap(FDrawContext.Selection),
                RectF( 0, 0,
                       T_FMXBitmap(FDrawContext.Selection).Width,
                       T_FMXBitmap(FDrawContext.Selection).Height),
                rct_dst,
                oVwr.SelectionTransparency / 100, True
              ) ;
            if assigned( FDrawContext.Charts ) then
              cnv.DrawBitmap(
                T_FMXBitmap(FDrawContext.Charts),
                RectF( 0, 0,
                       T_FMXBitmap(FDrawContext.Charts).Width,
                       T_FMXBitmap(FDrawContext.Charts).Height),
                rct_dst,
                1, True
              ) ;
            if not metPaintLabelsOnTop then begin
              if assigned( FDrawContext.Labels ) then
                cnv.DrawBitmap(
                  T_FMXBitmap(FDrawContext.Labels),
                  RectF( 0, 0,
                         T_FMXBitmap(FDrawContext.Labels).Width,
                         T_FMXBitmap(FDrawContext.Labels).Height),
                  rct_dst,
                  1, True
                ) ;
            end;
            if oVwr.IsTopmost and draw_topmost then
              cnv.DrawBitmap(
                T_FMXBitmap(FTopContext.BaseMap),
                RectF( 0, 0,
                       T_FMXBitmap(FTopContext.BaseMap).Width,
                       T_FMXBitmap(FTopContext.BaseMap).Height),
                rct_dst,
                1, True
              ) ;
            if oVwr.IsTopmost and assigned( FTopContext.Labels ) and
               not metPaintLabelsOnTop then
              cnv.DrawBitmap(
                T_FMXBitmap(FTopContext.Labels),
                RectF( 0, 0,
                       T_FMXBitmap(FTopContext.Labels).Width,
                       T_FMXBitmap(FTopContext.Labels).Height),
                rct_dst,
                1, True
              ) ;
            if assigned( FFlashContext.BaseMap ) then
              cnv.DrawBitmap(
                T_FMXBitmap(FFlashContext.BaseMap),
                RectF( 0, 0,
                       T_FMXBitmap(FFlashContext.BaseMap).Width,
                       T_FMXBitmap(FFlashContext.BaseMap).Height),
                rct_dst,
                FFlashTransparency / 100, True
              ) ;

            oRenderer.RenderEditor( cnv ) ;

            oRenderer.PaintExtra( Self, cnv, FOnPaintExtra ) ;

            if metPaintLabelsOnTop then begin
              if assigned( FDrawContext.Labels ) then
                cnv.DrawBitmap(
                  T_FMXBitmap(FDrawContext.Labels),
                  RectF( 0, 0,
                         T_FMXBitmap(FDrawContext.Labels).Width,
                         T_FMXBitmap(FDrawContext.Labels).Height),
                  rct_dst,
                  1, True
                ) ;
              if oVwr.IsTopmost and assigned( FTopContext.Labels ) then
                cnv.DrawBitmap(
                  T_FMXBitmap(FTopContext.Labels),
                  RectF( 0, 0,
                         T_FMXBitmap(FTopContext.Labels).Width,
                         T_FMXBitmap(FTopContext.Labels).Height),
                  rct_dst,
                  1, True
                ) ;
            end;
          end;

          if metPaintNavigateFeedbackFullCache then begin
            cnv.EndScene;
            Canvas.DrawBitmap(
              oFullCache,
              RectF( 0, 0,
                     oFullCache.Width ,
                     oFullCache.Height
                   ),
              rct_dst_scaled,
              1, True
            ) ;
            oPaintCacheTmp := oFullCache ;
          end
          else begin
            // oPaintCacheTmp (oPaintCacheTmpDX) set already to FCacheBitmap
          end ;

          NotifySubscribers( GIS_SUBSCRIBED_TRANSPARENT_CONTROL_UPDATE, Canvas ) ;
        end
        else begin
          if bCacheBitmapNew then begin
           oPaintCacheTmp := FCacheBitmap ;
          end ;

          oBaseMapHelper.LockBitmap ;
          try
            if oBaseMapHelper.Active
               and
               valid_bmp( oBaseMapHelper.Bitmap )
               and
               ( not GisIsEmptyExtent( VisibleExtent ) )
            then begin
              Canvas.DrawBitmap(
                T_FMXBitmap( oBaseMapHelper.Bitmap.NativeBitmap ),
                RectF(
                  0, 0,
                  oBaseMapHelper.Bitmap.Width, oBaseMapHelper.Bitmap.Height
                ),
                oWndHelper.ScaledRect(
                  oBaseMapHelper.Extent,
                  oBaseMapHelper.Bitmap.Width,
                  oBaseMapHelper.Bitmap.Height,
                  Width / ControlCanvasWidth
                ),
                1, False
              );
            end;
            oBaseMapHelper.Request(
              ControlCanvasWidth,
              ControlCanvasHeight,
              oWndHelper.ActualExtent,
              RotationPoint,
              RotationAngle
            ) ;
          finally
            oBaseMapHelper.UnlockBitmap ;
          end ;

          if assigned( FProgressBitmap ) then begin
            if metPaintProgressiveFullCache then begin
              if valid_bmp( oPaintCacheTmp ) then
                Canvas.DrawBitmap(
                  oPaintCacheTmp,
                  RectF( 0, 0, oPaintCacheTmp.Width, oPaintCacheTmp.Height),
                  oWndHelper.ScaledRect( Width / ControlCanvasWidth ),
                  metPaintProgressiveTransparency/100,
                  False
                )
            end
            else begin
              if valid_bmp( FCacheBitmap ) then
                Canvas.DrawBitmap(
                  FCacheBitmap,
                  RectF( 0, 0, FCacheBitmap.Width, FCacheBitmap.Height),
                  oWndHelper.ScaledRect( Width / ControlCanvasWidth ),
                  metPaintProgressiveTransparency/100,
                  False
                ) ;
            end;

            Canvas.DrawBitmap(
              FProgressBitmap,
              RectF( 0, 0, FProgressBitmap.Width, FProgressBitmap.Height),
              RectF( 0, 0, Width, Height),
              1,
              False
            ) ;
          end
          else begin
            if valid_bmp( FCacheBitmap ) then begin
              Canvas.DrawBitmap(
                oPaintCacheTmp,
                RectF( 0, 0, FCacheBitmap.Width, FCacheBitmap.Height),
                oWndHelper.ScaledRect( Width / ControlCanvasWidth ),
                1,
                False
              ) ;
            end;
          end;

          NotifySubscribers( GIS_SUBSCRIBED_TRANSPARENT_CONTROL_PAINT , Canvas ) ;
        end;
      finally
        bCacheBitmapNew := False ;

        if assigned( FOnAfterPaint ) then
          FOnAfterPaint( Self, Canvas ) ;
        if assigned( FOnAfterPaintRenderer ) then
          oRenderer.PaintExtra( Self, Canvas, FOnAfterPaintRenderer ) ;
        if Graticule.Enabled then
          oRenderer.PaintExtra( Self, Canvas, doGraticule ) ;

        oProgressRectAni.Pause := True ;
      end ;
    except
      On e : Exception do begin
        NotifyPaintException( 'PaintException', e ) ;
      end ;
    end;
  end ;

  procedure TGIS_ViewerWnd.Resize;
  begin
    if not View3D then begin
      oWndHelper.KeepScale := KeepScale ;
      oWndHelper.UpdateCancel ;
      Repaint ;
      if not IsEmpty then
        oWndHelper.UpdateDelayed ;
    end ;
    inherited ;
  end;

{$IFDEF LEVEL_RX10_FMX}
  procedure TGIS_ViewerWnd.DoStyleChanged;
  var
    stl   : TFmxObject  ;
    stl2  : TFmxObject  ;
    bmp   : T_FMXBitmap ;
    dat   : TBitmapData ;
    c     : TGIS_Color  ;
    cnt   : Integer     ;
    r,g,b : Integer     ;
    x,y   : Integer     ;
  begin
    inherited ;

    stl := TStyledControl.LookupStyleObject(
             Self, Self, Scene, '', 'backgroundstyle', '', True
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
             Self, Self, Scene, '', 'treeviewstyle.background', '', True
           ) ;

    if not ( Assigned( stl ) and ( stl is TControl ) ) then begin
      FreeObject( oStlWnd ) ;
      FreeObject( oStlCtl ) ;
      FreeObject( stl ) ;
      exit ;
    end ;

    FreeObject( oStlCtl );
    oStlCtl := TControl( stl );

    stl2 := oStlCtl.FindStyleResource( 'content');
    if Assigned( stl2 ) and ( stl2 is TControl ) then
      TControl( stl2 ).Visible := False ;

    bmp := T_FMXBitmap.Create( 16, 16 ) ;
    try
      bmp.Canvas.BeginScene ;
      try
        oStlWnd.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
        oStlCtl.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
      finally
        bmp.Canvas.EndScene ;
      end;

      bmp.Map( TMapAccess.Read, dat);
      try
        cnt := 0  ;
        r := 0 ;
        g := 0;
        b := 0 ;
        for x := RoundS( 2 + oStlCtl.Padding.Left )
            to RoundS( bmp.Width - oStlCtl.Padding.Right - 1 )
        do begin
          for y := RoundS( 2 + oStlCtl.Padding.Top )
             to RoundS( bmp.Height - oStlCtl.Padding.Bottom - 1 )
          do begin
            c := GISColor( dat.GetPixel( x-1, y-1 ) );
            r := r + c.fget_R ;
            g := g + c.fget_G ;
            b := b + c.fget_B ;
            Inc(cnt);
          end;
        end;
        oStyledCtlColor := FMXColor(
                             TGIS_Color.FromRGB(
                               r div cnt,
                               g div cnt,
                               b div cnt
                             )
                           ) ;
      finally
        bmp.Unmap(dat);
      end;
    finally
      FreeObject( bmp ) ;
    end;
  end;
{$ENDIF LEVEL_RX10_FMX}

  procedure TGIS_ViewerWnd.DoMouseDown(
    _button   : TMouseButton ;
    _shift    : TShiftState  ;
    _x        : Integer      ;
    _y        : Integer
  ) ;
  begin
    MouseDown( _button, _shift, _x, _y ) ;
  end;

  procedure TGIS_ViewerWnd.DoMouseUp(
    _button   : TMouseButton ;
    _shift    : TShiftState  ;
    _x        : Integer      ;
    _y        : Integer
  ) ;
  begin
    MouseUp( _button, _shift, _x, _y ) ;
  end;

  procedure TGIS_ViewerWnd.DoMouseMove(
    _shift    : TShiftState  ;
    _x        : Integer      ;
    _y        : Integer
  ) ;
  begin
    MouseMove( _shift, _x, _y ) ;
  end;

//==============================================================================
// new event methods
//==============================================================================

  procedure TGIS_ViewerWnd.TapSingle(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if Assigned( FOnTapSimple) then
      FOnTapSimple( Self, _button, _shift, _x, _y );
  end;

  procedure TGIS_ViewerWnd.TapDouble(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if Assigned( FOnTapDouble ) then
      FOnTapDouble( Self, _button, _shift, _x, _y );
  end;

  procedure TGIS_ViewerWnd.TapLong(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if Assigned( FOnTapLong ) then
      FOnTapLong( Self, _button, _shift, _x, _y );
  end;

//==============================================================================
// constructor / destructor
//==============================================================================

  constructor TGIS_ViewerWnd.Create(
    _owner : TComponent
  )  ;
    {$IFDEF MACOS}
      {$IFNDEF IOS}
        function set_cursor(
          const _res_name  : String   ;
          const _xpos      : Integer  ;
          const _ypos      : Integer
        ) : NSCursor ;
        var
          strm  : TResourceStream ;
          image : NSImage ;
          data  : NSData  ;
        begin
          Result := nil ;
          try
            if FindResource(HInstance, PWideChar(_res_name), RT_RCDATA) = 0 then
              exit ;

            strm := TResourceStream.Create(HInstance, _res_name, RT_RCDATA);
            try
               data   := TNSData.Wrap(
                           TNSData.Create.initWithBytes(
                             strm.Memory, strm.Size
                           )
                         ) ;
            finally
              strm.Free ;
            end ;
            image  := TNSImage.Wrap(
                        TNSImage.Create.initWithData(
                          data
                        )
                      ) ;
            Result := TNSCursor.Wrap(
                        TNSCursor.Create.initWithImage(
                          image,
                          NSPoint( PointF( _xpos, _ypos ) )
                        )
                      ) ;
          finally
            Assert( Assigned( Result), _res_name );
            if not Assigned( Result ) then begin
               Result := TNSCursor.Wrap(
                           TNSCursor.OCClass.currentSystemCursor
                         ) ;
            end ;
          end ;
        end ;

      {$ENDIF}
    {$ENDIF{}

    procedure load_image_from_resource(
      const _bmp      : T_FMXBitmap ;
      const _res_name : String
    ) ;
    var
      strm : TResourceStream ;
    begin
      if FindResource(HInstance, PWideChar(_res_name), RT_RCDATA) = 0 then
      exit ;

      strm := TResourceStream.Create(HInstance, _res_name, RT_RCDATA);
      try
        _bmp.LoadFromStream(strm);
      finally
        strm.Free ;
      end ;
    end ;

  begin
    inherited ;

    FHiRes := True ;

    iCanvasScale  := 1 ;
    mouseBlocked  := False ;

    if csDesigning in ComponentState then
      EnsureFramework ;

    ClipChildren := True ;

    oVwr := TGIS_Viewer.Create( Self ) ;

    FMinZoomSize      := GIS_MIN_ZOOM_SIZE ;
    FAutoCenter       := False ;
    FModeMouseButton  := TMouseButton.mbLeft ;
    FTemporaryScaleInternal := 0 ;

    bInUpdate := False ;
    bInFlash  := False ;
    BackgroundColor := TAlphaColorRec.White ;

    FCacheBitmap := nil ;
    bCacheBitmapNew := False ;
    FProgressBitmap := nil ;

    FDrawContext  := TGIS_RendererContext.Create ;
    FTopContext   := TGIS_RendererContext.Create ;
    FFlashContext := TGIS_RendererContext.Create ;
    oFullCache    := nil ;

    oRenderer := TGIS_RendererFmx.Create ;

    IncrementalPaint := True ;

    oZoomingRect := TRectangle.Create( Self ) ;
    oZoomingRect.Parent           := Self  ;
    oZoomingRect.Visible          := False ;
    oZoomingRect.Fill.Color       := MakeColor( TAlphaColorRec.Yellow, 0.2 ) ;
    oZoomingRect.OnMouseMove      := doZoomingRectMouseMove ;
    oZoomingRect.OnMouseUp        := doZoomingRectMouseUp   ;
    oZoomingRect.Stored           := False ;
    oZoomingRect.Stroke.Dash      := TStrokeDash.Dash       ;
    oZoomingRect.Stroke.Thickness := 2 ;
    oZoomingRect.Cursor           := 1 ;

    oProgressRect := TRectangle.Create( Self ) ;
    oProgressRect.Parent          := Self  ;
    oProgressRect.Stored          := False ;
    oProgressRect.Sides           := []    ;
    oProgressRect.Visible         := False ;
    oProgressRect.Fill.Kind       := TBrushKind.Bitmap ;
    oProgressRect.Fill.Bitmap.WrapMode := TWrapMode.TileStretch ;

    oProgressRectAni := TBitmapListAnimation.Create( oProgressRect ) ;
    oProgressRectAni.Parent      := oProgressRect  ;
    load_image_from_resource( oProgressRectAni.AnimationBitmap, 'PROGRESS_A_32X704' ) ;
    oProgressRectAni.AnimationCount    :=
       oProgressRectAni.AnimationBitmap.Width div
       oProgressRectAni.AnimationBitmap.Height ;
    oProgressRectAni.AnimationRowCount := 1  ;
    oProgressRectAni.Duration          := 2 ;
    oProgressRectAni.Loop              := True ;
    oProgressRectAni.PropertyName      := 'Fill.Bitmap.Bitmap' ;

    oProgressRect.Height := oProgressRectAni.AnimationBitmap.Height ;
    oProgressRect.Width  := oProgressRect.Height ;

    oDraggingTrack := TLine.Create( Self ) ;
    oDraggingTrack.Parent           := Self  ;
    oDraggingTrack.Stored           := False ;
    oDraggingTrack.Visible          := False ;
    oDraggingTrack.Fill.Color       := MakeColor( TAlphaColorRec.Yellow, 0.2 ) ;
    oDraggingTrack.HitTest          := False ;
    oDraggingTrack.Stroke.Dash      := TStrokeDash.Dash ;
    oDraggingTrack.Stroke.Thickness := 2 ;
    oDraggingTrack.LineType         := TLineType.Top ;
    oDraggingTrack.Height           := oDraggingTrack.Stroke.Thickness ;
    oDraggingTrack.Cursor           := 1 ;
    oDraggingTrack.RotationCenter.Point := PointF( 0, 0 ) ;

    oWndHelper       := TGIS_ViewerWndHelper.Create(
                          oVwr,
                          updateExecute,
                          updateSynchronize,
                          updateTap
                          ) ;
    oBasemapHelper   := TGIS_BasemapHelper.Create(
                          self,
                          TGIS_ViewerBmp.Create
                        ) ;
    oGraticuleHelper := TGIS_GraticuleHelper.Create(
                          self.oVwr
                        ) ;
    oGraticule       := oGraticuleHelper ;

    {$IFDEF MSWINDOWS}
      CustomCursors[DRAG_CURSOR]     := LoadCursor(HInstance, 'HANDGRAB') ;
      CustomCursors[SELECT_CURSOR]   := LoadCursor(HInstance, 'HANDPNT' ) ;
      CustomCursors[ZOOM_CURSOR]     := LoadCursor(HInstance, 'HANDZOOM') ;
      CustomCursors[EDIT_CURSOR]     := LoadCursor(HInstance, 'HANDEDIT') ;
      CustomCursors[CUSTOM_CURSOR]   := LoadCursor(0, IDC_ARROW ) ;
      CustomCursors[WAIT_CURSOR]     := LoadCursor(0, IDC_WAIT  ) ;
      CustomCursors[CAMPOS_CURSOR]   := LoadCursor(HInstance, 'CAMPOS'  ) ;
      CustomCursors[CAMROT_CURSOR]   := LoadCursor(HInstance, 'CAMROT'  ) ;
      CustomCursors[CAMXYZ_CURSOR]   := LoadCursor(HInstance, 'CAMXYZ'  ) ;
      CustomCursors[CAMXY_CURSOR]    := LoadCursor(HInstance, 'CAMXY'   ) ;
      CustomCursors[CAMZOOM_CURSOR]  := LoadCursor(HInstance, 'CAMZOOM' ) ;
      CustomCursors[SUNPOS_CURSOR]   := LoadCursor(HInstance, 'SUNPOS'  ) ;
      CustomCursors[SELECT3D_CURSOR] := LoadCursor(HInstance, 'HANDPNT' ) ;

    {$ENDIF}

    {$IFDEF MACOS}
      {$IFNDEF IOS}
        CustomCursors[DRAG_CURSOR]     := TNSCursor.Wrap(
                                            TNSCursor.OCClass.openHandCursor
                                          ) ;
        CustomCursors[SELECT_CURSOR]   := TNSCursor.Wrap(
                                            TNSCursor.OCClass.pointingHandCursor
                                          ) ;
        CustomCursors[ZOOM_CURSOR]     := set_cursor(
                                            'HANDZOOM', 14, 14
                                          ) ;
        CustomCursors[EDIT_CURSOR]     := TNSCursor.Wrap(
                                            TNSCursor.OCClass.crosshairCursor
                                          ) ;
        CustomCursors[CUSTOM_CURSOR]   := TNSCursor.Wrap(
                                            TNSCursor.OCClass.arrowCursor
                                          ) ;
        CustomCursors[WAIT_CURSOR]     := TNSCursor.Wrap(
                                            TNSCursor.OCClass.crosshairCursor
                                          ) ;
        CustomCursors[CAMPOS_CURSOR]   := set_cursor(
                                            'CAMPOS'  , 13, 17
                                          ) ;
        CustomCursors[CAMROT_CURSOR]   := set_cursor(
                                            'CAMROT'  , 16, 19
                                          ) ;
        CustomCursors[CAMXYZ_CURSOR]   := set_cursor(
                                            'CAMXYZ'  ,  7, 19
                                          ) ;
        CustomCursors[CAMXY_CURSOR]    := set_cursor(
                                            'CAMXY'   ,  7, 19
                                          ) ;
        CustomCursors[CAMZOOM_CURSOR]  := set_cursor(
                                            'CAMZOOM' , 14, 14
                                          ) ;
        CustomCursors[SUNPOS_CURSOR]   := set_cursor(
                                            'SUNPOS'  , 16, 15
                                          ) ;
        CustomCursors[SELECT3D_CURSOR] := TNSCursor.Wrap(
                                            TNSCursor.OCClass.pointingHandCursor
                                          ) ;
      {$ENDIF}
    {$ENDIF}

    readGesturesMetadata ;
    Touch.InteractiveGestures := [ TInteractiveGesture.Zoom,
                                   TInteractiveGesture.Pan
                                 ] ;

    OnGesture := doGestureProc ;
    FOnEditorChange := nil ;
    FOnBeforePaint  := nil ;
    FOnAfterPaint   := nil ;
    FOnBeforePaintRenderer := nil ;
    FOnAfterPaintRenderer  := nil ;
    FOnPaintExtra   := nil ;
    FOnBeforeUpdate := nil ;
    FOnUpdate       := nil ;
    FOnAfterUpdate  := nil ;

    oStlWnd := nil ;
    oStlCtl := nil ;

    {$IFDEF LEVEL_RX10_FMX}
      FStyledSettings := [] ;
    {$ENDIF}
  end ;

  destructor TGIS_ViewerWnd.Destroy ;
  begin
    {$IFDEF GIS_PDK}
      RemoveFreeNotifications ;
    {$ENDIF}

    View3D := False ;

    FreeObject( FCacheBitmap     ) ;
    FreeObject( oDraggingTrack   ) ;
    FreeObject( oZoomingRect     ) ;
    FreeObject( oBasemapHelper   ) ;
    FreeObject( oGraticuleHelper ) ;
    FreeObject( oWndHelper       ) ;
    FreeObject( oRenderer        ) ;
    FreeObject( oVwr             ) ;

    FreeObject( FDrawContext     ) ;
    FreeObject( FTopContext      ) ;
    FreeObject( FFlashContext    ) ;
    FreeObject( oFullCache       ) ;

    FreeObject( oStlWnd          ) ;
    FreeObject( oStlCtl          ) ;

    inherited ;
  end ;

//==============================================================================
// IGIS_Viewer public methods
//==============================================================================

  function TGIS_ViewerWnd.ChangeHash
    : Int64 ;
  begin
    Result := oVwr.ChangeHash ;
  end ;

  procedure TGIS_ViewerWnd.Subscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    oVwr.Subscribe( _control ) ;
  end;

  procedure TGIS_ViewerWnd.UnSubscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    oVwr.Unsubscribe( _control ) ;
  end ;

  procedure TGIS_ViewerWnd.NotifySubscribers(
    const _event   : Integer ;
    const _context : TObject
  ) ;
  begin
    oVwr.NotifySubscribers( _event, _context ) ;
  end ;

  function TGIS_ViewerWnd.NotifyPaintException(
    const _message   : String ;
    const _exception : Exception
  ) : Boolean ;
  begin
    Result := oVwr.NotifyPaintException( _message, _exception ) ;
  end ;

  procedure TGIS_ViewerWnd.Lock ;
  begin
    oVwr.Lock ;
  end ;

  procedure TGIS_ViewerWnd.Unlock ;
  begin
    oVwr.Unlock ;
  end ;

  procedure TGIS_ViewerWnd.Unlock(
    const _redraw : Boolean
  ) ;
  begin
    oVwr.Unlock( _redraw ) ;
  end ;

  procedure TGIS_ViewerWnd.Interrupt ;
  begin
    oVwr.Interrupt ;
  end ;

  function  TGIS_ViewerWnd.HourglassActive
    : Boolean ;
  begin
    Result := oVwr.HourglassActive ;
  end ;

  procedure TGIS_ViewerWnd.HourglassPrepare ;
  begin
    oVwr.HourglassPrepare ;
  end ;

  procedure TGIS_ViewerWnd.HourglassRelease ;
  begin
    oVwr.HourglassRelease ;
  end ;

  function  TGIS_ViewerWnd.HourglassShake
    : Boolean ;
  begin
    Result := oVwr.HourglassShake ;
  end ;

  procedure TGIS_ViewerWnd.HourglassRestart ;
  begin
    oVwr.HourglassRestart ;
  end ;

  procedure TGIS_ViewerWnd.BusyPrepare(
    _sender : TObject ;
    _text   : String
  ) ;
  begin
    oVwr.BusyPrepare( _sender, _text ) ;
  end ;

  procedure TGIS_ViewerWnd.BusyRelease(
    _sender : TObject
  ) ;
  begin
    oVwr.BusyRelease( _sender ) ;
  end ;

  procedure TGIS_ViewerWnd.BusyShake(
        _sender : TObject ;
        _pos    : Int64 ;
        _end    : Int64 ;
    var _abort  : Boolean
  ) ;
  begin
    oVwr.BusyShake( _sender, _pos, _end, _abort ) ;
  end ;

  procedure TGIS_ViewerWnd.RaiseBusyEvent(
        _sender  : TObject ;
        _pos     : Int64 ;
        _end     : Int64;
    var _abort   : Boolean
  ) ;
  begin
    oVwr.RaiseBusyEvent( _sender, _pos, _end, _abort ) ;
  end;

  procedure TGIS_ViewerWnd.RaiseHelpEvent(
        _sender  : TObject ;
        _name    : String
  ) ;
  begin
    oVwr.RaiseHelpEvent( _sender, _name ) ;
  end;

  function TGIS_ViewerWnd.AssignedBusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := oVwr.AssignedBusyEvent() ;
  end ;

  function TGIS_ViewerWnd.AssignedHelpEvent
    : TGIS_HelpEvent ;
  begin
    Result := oVwr.AssignedHelpEvent() ;
  end ;

  function TGIS_ViewerWnd.StorePaintState
    : TObject ;
  begin
    Result := oVwr.StorePaintState ;
  end ;

  procedure TGIS_ViewerWnd.RestorePaintState(
    var _state : TObject
  ) ;
  begin
    oVwr.RestorePaintState( _state ) ;
  end ;

  procedure TGIS_ViewerWnd.BeginPaintInternal ;
  begin
    oVwr.BeginPaintInternal ;
  end;

  procedure TGIS_ViewerWnd.EndPaintInternal ;
  begin
    oVwr.EndPaintInternal ;
  end;

  function TGIS_ViewerWnd.SynchronizePaint(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := oVwr.SynchronizePaint( _interrupt ) ;
  end;

  function TGIS_ViewerWnd.ReParent(
    const _parent : IGIS_ViewerParent
  ) : IGIS_ViewerParent ;
  begin
    Result := oVwr.ReParent( _parent ) ;
  end;

  function TGIS_ViewerWnd.AttachLayer(
    const _layer : TGIS_LayerAbstract
  ) : IGIS_Viewer ;
  begin
    Result := oVwr.AttachLayer( _layer ) ;
  end ;

  procedure TGIS_ViewerWnd.Open(
    const _path : String
  ) ;
  begin
    oVwr.Open( _path ) ;
  end ;

  procedure TGIS_ViewerWnd.Open(
    const _path   : String ;
    const _strict : Boolean
  ) ;
  begin
    oVwr.Open( _path, _strict  ) ;
  end ;

  procedure TGIS_ViewerWnd.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String
  ) ;
  begin
    oVwr.OpenEx( _configFile, _path ) ;
  end ;

  procedure TGIS_ViewerWnd.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String              ;
    const _strict     : Boolean
  ) ;
  begin
    oVwr.OpenEx( _configFile, _path, _strict ) ;
  end ;

  procedure TGIS_ViewerWnd.Close ;
  begin
    if View3D then begin
      if TGIS_Viewer3D( FViewer3D ).IsBusy then exit ;
    end;

    if not SynchronizePaint(False) then exit ;

    oVwr.Close ;
  end ;

  procedure TGIS_ViewerWnd.ReadConfig ;
  begin
    oVwr.ReadConfig ;
  end ;

  procedure TGIS_ViewerWnd.RereadConfig ;
  begin
    oVwr.RereadConfig ;
  end ;

  procedure TGIS_ViewerWnd.WriteConfig ;
  begin
    oVwr.WriteConfig ;
  end ;

  procedure TGIS_ViewerWnd.Add(
    const _layer : TGIS_LayerAbstract
  ) ;
  begin
    oVwr.Add( _layer ) ;
  end ;

  function TGIS_ViewerWnd.Get(
    const _name    : String
  ) : TGIS_LayerAbstract ;
  begin
    Result := oVwr.Get( _name ) ;
  end ;

  procedure TGIS_ViewerWnd.Delete(
    const _name : String
  ) ;
  begin
    oVwr.Delete( _name ) ;
  end ;

  procedure TGIS_ViewerWnd.AddHierarchy ;
  begin
    oVwr.AddHierarchy ;
  end ;

  procedure TGIS_ViewerWnd.Draw(
    const _renderer : TObject     ;
    const _mode     : TGIS_DrawMode
  ) ;
  begin
    oVwr.Draw( _renderer, _mode ) ;
  end ;

  function TGIS_ViewerWnd.GetGrid(
    const _extent : TGIS_Extent ;
    const _grid   : TGIS_GridArray
  ) : Boolean ;
  begin
    Result := oVwr.GetGrid( _extent, _grid ) ;
  end ;

  procedure TGIS_ViewerWnd.RevertAll ;
  begin
    oVwr.RevertAll ;
  end ;

  procedure TGIS_ViewerWnd.SaveProject ;
  begin
    oVwr.SaveProject ;
  end ;

  procedure TGIS_ViewerWnd.SaveProject(
    const _relativepath : Boolean
  ) ;
  begin
    oVwr.SaveProject( _relativepath ) ;
  end ;

  procedure TGIS_ViewerWnd.SaveProjectAs(
    const _path : String
  ) ;
  begin
    oVwr.SaveProjectAs( _path ) ;
  end ;

  procedure TGIS_ViewerWnd.SaveProjectAs(
    const _path         : String ;
    const _relativepath : Boolean
  ) ;
  begin
    oVwr.SaveProjectAs( _path, _relativepath ) ;
  end ;

  procedure TGIS_ViewerWnd.SaveProjectAsEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String
  ) ;
  begin
    oVwr.SaveProjectAsEx( _configFile, _path ) ;
  end ;

  procedure TGIS_ViewerWnd.SaveProjectAsEx(
    const _configFile   : TGIS_ConfigAbstract ;
    const _path         : String              ;
    const _relativepath : Boolean
  ) ;
  begin
    oVwr.SaveProjectAsEx( _configFile, _path, _relativepath ) ;
  end ;

  procedure TGIS_ViewerWnd.SaveData ;
  begin
    oVwr.SaveData ;
  end ;

  procedure TGIS_ViewerWnd.SaveAll ;
  begin
    oVwr.SaveAll ;
  end ;

  function  TGIS_ViewerWnd.MustSave
    : Boolean ;
  begin
    Result := oVwr.MustSave ;
  end ;

  procedure TGIS_ViewerWnd.MarkModified ;
  begin
    oVwr.MarkModified ;
  end ;

  procedure TGIS_ViewerWnd.RecalcExtent ;
  begin
    oVwr.RecalcExtent ;
  end ;

  procedure TGIS_ViewerWnd.Reposition ;
  begin
    oVwr.Reposition ;
  end ;

  procedure TGIS_ViewerWnd.InvalidateExtent(
    const _extent : TGIS_Extent
  ) ;
  begin
    oVwr.InvalidateExtent( _extent );
  end ;

  procedure TGIS_ViewerWnd.InvalidateExtent(
    const _extent : TGIS_Extent ;
    const _deep   : Boolean
  ) ;
  begin
    oVwr.InvalidateExtent( _extent, _deep ) ;
  end ;

  procedure TGIS_ViewerWnd.InvalidateWholeMap ;
  begin
    oVwr.InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerWnd.InvalidateTopmost ;
  begin
    oVwr.InvalidateTopmost ;
  end ;

  procedure TGIS_ViewerWnd.InvalidateBasemap ;
  begin
    oVwr.InvalidateBasemap ;
  end ;

  procedure TGIS_ViewerWnd.InvalidateSelection ;
  begin
    oVwr.InvalidateSelection ;
  end ;

  procedure TGIS_ViewerWnd.InvalidateEditor(
    const _final : Boolean
  ) ;
  begin
    oVwr.InvalidateEditor( _final ) ;
  end ;

  function TGIS_ViewerWnd.FullExtentZoom
    : Double ;
  begin
    Result := oVwr.FullExtentZoom ;
  end ;

  procedure TGIS_ViewerWnd.FullExtent ;
  begin
    if not SynchronizePaint( True ) then
      exit ;

    oWndHelper.DoFullExtent ;
    oVwr.FullExtent ;
  end ;

  function TGIS_ViewerWnd.Locate(
    const _ptg  : TGIS_Point ;
    const _prec : Double
  ) : TGIS_ShapeAbstract ;
  begin
    Result := oVwr.Locate( _ptg, _prec ) ;
  end ;

  function TGIS_ViewerWnd.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstract ;
  begin
    Result := oVwr.Locate( _ptg, _prec, _visible ) ;
  end ;

  function TGIS_ViewerWnd.Locate(
    const _pt      : TPoint     ;
    const _prec    : Integer
  ) : TGIS_ShapeAbstract ;
  var
    pt :  TPoint ;
  begin
    pt := Point( RoundS(_pt.X * iCanvasScale), RoundS(_pt.Y * iCanvasScale) ) ;
    if View3D then begin
      Result := TGIS_ShapeAbstract(
        Viewer3D.Locate( pt, _prec )
      ) ;
      exit ;
    end ;

    Result := oVwr.Locate( pt, _prec ) ;
  end;

  function TGIS_ViewerWnd.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double    ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstractList ;
  begin
    Result := oVwr.LocateEx( _ptg, _prec, _visible ) ;
  end ;

  function TGIS_ViewerWnd.MapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreen( _ptg )
    else
      Result := oVwr.MapToScreen( _ptg ) ;
    Result.X := RoundS(Result.X / iCanvasScale) ;
    Result.Y := RoundS(Result.Y / iCanvasScale) ;
  end ;

  function TGIS_ViewerWnd.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreen3D( _ptg )
    else
      Result := oVwr.MapToScreen3D( _ptg ) ;
    Result.X := RoundS(Result.X / iCanvasScale) ;
    Result.Y := RoundS(Result.Y / iCanvasScale) ;
  end ;

  function TGIS_ViewerWnd.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMap(
                  Point( RoundS(_pt.X * iCanvasScale), RoundS(_pt.Y *iCanvasScale) )
                )
    else
      Result := oVwr.ScreenToMap(
                  Point( RoundS(_pt.X * iCanvasScale), RoundS(_pt.Y *iCanvasScale) )
                ) ;
  end ;

  function TGIS_ViewerWnd.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMap3D(
                  Point( RoundS(_pt.X * iCanvasScale), RoundS(_pt.Y *iCanvasScale) )
                )
    else
      Result := oVwr.ScreenToMap3D(
                  Point( RoundS(_pt.X * iCanvasScale), RoundS(_pt.Y *iCanvasScale) )
                ) ;
  end ;

  function TGIS_ViewerWnd.MapToScreenEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreenEx( _pt )
    else
      Result := oVwr.MapToScreenEx( _pt ) ;
    Result.X := Result.X / iCanvasScale ;
    Result.Y := Result.Y / iCanvasScale ;
  end ;

  function TGIS_ViewerWnd.ScreenToMapEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMapEx(
                  GisPoint( _pt.X * iCanvasScale, _pt.Y *iCanvasScale )
                )
    else
      Result := oVwr.ScreenToMapEx(
                  GisPoint( _pt.X * iCanvasScale, _pt.Y *iCanvasScale )
                ) ;
  end ;

  function TGIS_ViewerWnd.MapToScreenRect(
    const _rct : TGIS_Extent
  ) : TRect ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreenRect( _rct )
    else
      Result := oVwr.MapToScreenRect( _rct ) ;
    Result.Left   := RoundS(Result.Left   / iCanvasScale ) ;
    Result.Top    := RoundS(Result.Top    / iCanvasScale );
    Result.Right  := RoundS(Result.Right  / iCanvasScale );
    Result.Bottom := RoundS(Result.Bottom / iCanvasScale );
  end ;

  function TGIS_ViewerWnd.ScreenToMapRect(
    const _rct : TRect
  ) : TGIS_Extent ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMapRect(
                  Rect( RoundS(_rct.Left  * iCanvasScale), RoundS(_rct.Top    *iCanvasScale),
                        RoundS(_rct.Right * iCanvasScale), RoundS(_rct.Bottom *iCanvasScale)
                      )
                )
    else
      Result := oVwr.ScreenToMapRect(
                  Rect( RoundS(_rct.Left  * iCanvasScale), RoundS(_rct.Top    *iCanvasScale),
                        RoundS(_rct.Right * iCanvasScale), RoundS(_rct.Bottom *iCanvasScale)
                      )
                ) ;
  end ;

  function TGIS_ViewerWnd.PixelsToTwips(
    const _size : Integer
  ) : Integer ;
  begin
    Result := oVwr.PixelsToTwips( _size ) ;
  end ;

  function TGIS_ViewerWnd.TwipsToPixels(
    const _size : Integer
  ) : Integer ;
  begin
    Result := oVwr.TwipsToPixels( _size ) ;
  end ;

  function TGIS_ViewerWnd.TwipsToPoints(
    const _size : Integer
  ) : Integer ;
  begin
    Result := oVwr.TwipsToPoints( _size ) ;
  end ;

  procedure TGIS_ViewerWnd.MoveViewport(
    var _dx : Integer ;
    var _dy : Integer
  ) ;
  begin
    _dx := RoundS(_dx * iCanvasScale) ;
    _dy := RoundS(_dy * iCanvasScale) ;

    try
      if View3D then begin
        oVwr.MoveViewport( _dx, _dy ) ;
        exit ;
      end ;

      oWndHelper.DoDrag( _dx * iCanvasScale, _dy * iCanvasScale ) ;
      oWndHelper.FitScreenAnimation ;
      oWndHelper.UpdateDelayed ;
    finally
      _dx := RoundS(_dx / iCanvasScale) ;
      _dy := RoundS(_dy / iCanvasScale) ;
    end;
  end ;

  procedure TGIS_ViewerWnd.MoveViewportEx(
    var _dx : Double ;
    var _dy : Double
  ) ;
  begin
    if View3D then begin
      oVwr.MoveViewportEx( _dx, _dy ) ;
      exit ;
    end ;

    oWndHelper.DoDrag( TruncS( _dx * Zoom ), TruncS( _dy * Zoom ) ) ;
    oWndHelper.FitScreenAnimation ;
    oWndHelper.UpdateDelayed ;
  end ;

  procedure TGIS_ViewerWnd.SetViewport(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    oVwr.SetViewport( _x, _y ) ;
  end ;

  procedure TGIS_ViewerWnd.CenterViewport(
    const _ptg : TGIS_Point
  ) ;
  begin
    oVwr.CenterViewport( _ptg ) ;
  end ;

  procedure TGIS_ViewerWnd.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    oVwr.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_ViewerWnd.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    oVwr.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_ViewerWnd.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    oVwr.SetCSByWKTFile( _path ) ;
  end ;

  function TGIS_ViewerWnd.RotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.RotatedPoint( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.UnrotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.UnrotatedPoint( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.RotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.RotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_ViewerWnd.RotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    oVwr.RotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.UnrotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.UnrotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_ViewerWnd.UnrotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    oVwr.UnrotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.RotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := oVwr.RotatedExtent( _extent ) ;
  end ;

  function TGIS_ViewerWnd.UnrotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := oVwr.UnrotatedExtent( _extent ) ;
  end ;

  function TGIS_ViewerWnd.GetRenderContext
    : TObject ;
  begin
    Result := oVwr.GetRenderContext ;
  end ;

  procedure TGIS_ViewerWnd.WaitForBackgroundProcesses ;
  begin
    oVwr.WaitForBackgroundProcesses ;
    oBasemapHelper.WaitFor ;
  end;

  procedure TGIS_ViewerWnd.WaitForNotBusy(
          _sender : TObject ;
    const _proc   : TGIS_WaitForNotBusyProc
  ) ;
  begin
    oVwr.WaitForNotBusy( _sender, _proc ) ;
  end;

//==============================================================================
// IGIS_ViewerParent public methods
//==============================================================================

  procedure TGIS_ViewerWnd.ControlClose ;
  begin
    oPaintCacheTmp := nil ;
    View3D := False ;
  end;

  procedure TGIS_ViewerWnd.ControlDrawTexture(
          _bmp    : TObject     ;
    const _extent : TGIS_Extent ;
    const _ppi    : Integer
  ) ;
  begin
    ControlDrawTexture( _bmp, nil, _extent, _ppi ) ;
  end;

  procedure TGIS_ViewerWnd.ControlDrawTexture(
          _bmp    : TObject     ;
    const _layer  : TGIS_LayerAbstract ;
    const _extent : TGIS_Extent ;
    const _ppi    : Integer
  ) ;
  var
    ctx     : TGIS_RendererContext ;
    bmp     : T_FMXBitmap          ;
    old_ext : TGIS_Extent          ;
    old_res : Boolean              ;
    la      : TGIS_Layer           ;
  begin
    Lock ;
    old_ext := VisibleExtent ;
    old_res := RestrictedDrag ;
    try
      bmp := T_FMXBitmap( TGIS_Bitmap( _bmp ).NativeBitmap ) ;
      iWidth := bmp.Width ;
      iHeight := bmp.Height ;
      RestrictedDrag := False ;
      VisibleExtent  := _extent ;
      if oVwr.Color.ARGB = TGIS_Color.None.ARGB then begin
      end
      else begin
//        bmp.Clear( FMXColor( oVwr.Color ) ) ;
        bmp.Clear( FMXColor( TGIS_Color.None ) ) ;
      end;

      if IsEmpty then exit ;

      ctx := TGIS_RendererContext.Create ;
      try
        ctx.AssignBaseMap  ( bmp, False ) ;
        ctx.AssignSelection( nil, True  ) ;
        ctx.AssignCharts   ( nil, True  ) ;
        ctx.AssignLabels   ( nil, True  ) ;

        oRenderer.ReleaseContext ;
        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                                 RoundS( bmp.Width), RoundS( bmp.Height ),
                                 _ppi, FontScale
                               ) ;
        try
          oVwr.LabelsReg.Reset ;
          oRenderer.BeforeDraw ;
          oVwr.BeginPaintInternal ;

          if Assigned( _layer ) then begin
            la := TGIS_Layer( _layer ) ;
            TGIS_RendererAbstract(oRenderer).LockTransparent( la.Transparency ) ;
            try
              try
                la.Renderer := oRenderer ;
                la.Paint ;
              except
                on e : Exception do begin
                  if not NotifyPaintException( la.Name, e ) then
                    raise;
                end ;
              end ;
            finally
              TGIS_RendererAbstract(oRenderer).UnlockTransparent ;
            end ;
          end
          else
            Draw( oRenderer, TGIS_DrawMode.AllExcept3D ) ;

        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;

          bmp.Canvas.BeginScene ;
          try
            if Assigned( ctx.Selection ) then
              bmp.Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Selection),
                RectF( 0, 0, T_FMXBitmap(ctx.Selection).Width,
                             T_FMXBitmap(ctx.Selection).Height),
                RectF( 0, 0, bmp.Width,
                             T_FMXBitmap(ctx.Selection).Height
                             / T_FMXBitmap(ctx.Selection).Width * bmp.Width),
                oVwr.SelectionTransparency / 100, True
              ) ;
            if Assigned( ctx.Charts ) then
              bmp.Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Charts),
                RectF( 0, 0, T_FMXBitmap(ctx.Charts).Width,
                             T_FMXBitmap(ctx.Charts).Height),
                RectF( 0, 0, bmp.Width,
                             T_FMXBitmap(ctx.Charts).Height
                             / T_FMXBitmap(ctx.Charts).Width * bmp.Width),
                       1, False
              ) ;
            if Assigned( ctx.Labels ) then
              bmp.Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Labels),
                RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                             T_FMXBitmap(ctx.Labels).Height),
                RectF( 0, 0, bmp.Width,
                             T_FMXBitmap(ctx.Labels).Height
                             / T_FMXBitmap(ctx.Labels).Width * bmp.Width),
                       1, False
              ) ;
          finally
            bmp.Canvas.EndScene ;
          end ;
        end ;
      finally
        oRenderer.ReleaseContext ;
        FreeObject( ctx )
      end ;
    finally
      iWidth := 0 ;
      iHeight := 0 ;
      VisibleExtent  := old_ext ;
      RestrictedDrag := old_res ;
      Unlock( False ) ;
    end;
  end ;

  function TGIS_ViewerWnd.ControlRenderer;
  begin
    Result := oRenderer ;
  end ;

  procedure TGIS_ViewerWnd.ControlFlash(
    const _times : Integer ;
    const _delay : Integer
  ) ;
  var
    i : Integer ;
    old_rnd : TGIS_RendererAbstract ;
  begin
    if View3D then exit ;

    if bInUpdate then exit ;
    bInUpdate := True ;

    old_rnd := oRenderer ;

    try
      FFlashContext.AssignBaseMap( nil, True ) ;
      try
        oRenderer.ReleaseContext ;
        oRenderer := oRenderer.CreateInstance ; // clone
        oRenderer.CreateContext( Self, oVwr, FFlashContext, Point(0, 0),
                                 ControlCanvasWidth, ControlCanvasHeight,
                                 PPI, FontScale
                               );
        try
          oVwr.LabelsReg.Reset ;
          oRenderer.BeforeDraw ;
          oVwr.BeginPaintInternal ;

          Draw( oRenderer, TGIS_DrawMode.Flash ) ;

        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;
        end ;

        bInFlash := True ;
        try
          for i:=1 to _times do
          begin
            Application.ProcessMessages ;

            FFlashTransparency := SelectionTransparency ;
            ControlRepaint ;
            ControlProcessMessages ;

            Sleep( _delay ) ;

            FFlashTransparency := 0 ;
            ControlRepaint ;
            ControlProcessMessages ;

            if i <> _times then
              Sleep( _delay ) ;
          end ;
        finally
          bInFlash := False ;
        end;

      finally
        oRenderer.ReleaseContext ;
        FFlashContext.Clear ;
        FreeObject( oRenderer ) ;
        oRenderer := old_rnd ;
        oRenderer.RestoreContext;
      end ;
    finally
      bInUpdate := False ;
    end;

  end ;

  function TGIS_ViewerWnd.ControlSystemPPI
    : Integer ;
  var
    dev : IDeviceBehavior;
    met : TDeviceDisplayMetrics;
    {$IFNDEF LEVEL_RX101_FMX}
      {$IFDEF MSWINDOWS}
        screen : THandle ;
        h,v    : Double  ;
      {$ENDIF}
    {$ENDIF}
  begin
    Result := 96 ;

    if TBehaviorServices.Current.SupportsBehaviorService(
      IDeviceBehavior,
      dev, Self
    )
    then begin
      met := dev.GetDisplayMetrics(Self);
      Result := RoundS( met.PixelsPerInch / met.ScreenScale ) ;
     end;
    {$IFNDEF LEVEL_RX101_FMX}
      {$IFDEF MSWINDOWS}
        screen := GetDC(0);
        h := GetDeviceCaps(screen,LOGPIXELSX);
        v := GetDeviceCaps(screen,LOGPIXELSY);
        ReleaseDC(0, screen);
        Result := RoundS( ( h+v ) / 2 ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  function TGIS_ViewerWnd.ControlPPI
    : Integer ;
  begin
    Result := PPI ;
  end;

  procedure TGIS_ViewerWnd.ControlRepaint ;
  begin
    if View3D then
      Viewer3D.ControlRepaint
    else
     Repaint ;
  end ;

  procedure TGIS_ViewerWnd.ControlProcessMessages ;
  begin
    Application.ProcessMessages ;
  end ;

  function TGIS_ViewerWnd.ControlUpdateSynchronize(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    if InPaint then begin
      if _interrupt then
        Interrupt ;
    end ;

    Result := ( not InPaint ) and ( not IsBusy );
  end;

  procedure TGIS_ViewerWnd.ControlUpdateWholeMap;
  begin
    if FZooming  or
       FDragging or
       FEditing  or
       oWndHelper.GestureActive
    then
      exit ;

    if View3D then begin
      Viewer3D.UpdateWholeMap ;
      NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, Self ) ;
    end
    else begin
      ApplyStyleLookup ;
      oWndHelper.UpdateImmediate ;
    end;
  end ;

  procedure TGIS_ViewerWnd.ControlUpdateProgressive ;
  begin
    if GetCurrentThreadId <> MainThreadID then exit ;

    FDrawContext.DoProgressiveUpdate ;
  end;

  procedure TGIS_ViewerWnd.ControlUpdateTopmost ;
  begin
    if ( not GisIsEmptyExtent(oWndHelper.ActualExtent ) )
       and
       ( not GisIsEmptyExtent( VisibleExtent ) )
       and
       ( not GisIsSameExtent( oWndHelper.ActualExtent, VisibleExtent ) )
       and
       ( not oWndHelper.UpdatePending )
    then
      oWndHelper.UpdateDelayed
    else
      ControlRepaint ;
  end;

  procedure TGIS_ViewerWnd.ControlUpdateBasemap ;
  begin
    if View3D then
      // do nothing
    else begin
      oBasemapHelper.Reset ;
    end;
  end;

  procedure TGIS_ViewerWnd.ControlUpdateSelection;
  begin
    if View3D then begin
      Viewer3D.UpdateAllSelectedObjects ;
      exit ;
    end ;

    if not GisIsSameExtent( oWndHelper.ActualExtent, VisibleExtent ) then begin
      oWndHelper.UpdateDelayed ;
      exit ;
    end;

    if bInUpdate then exit ;
    bInUpdate := True ;
    try
      // Selection
      FDrawContext.AssignSelection( nil, True ) ;

      oRenderer.ReleaseContext ;
      oRenderer.CreateContext( Self, oVwr, FDrawContext, Point(0, 0),
                               ControlCanvasWidth, ControlCanvasHeight,
                               PPI, FontScale
                             );
      try
        oRenderer.BeforeDraw ;
        oVwr.BeginPaintInternal ;

        Draw( oRenderer, TGIS_DrawMode.OnlySelectedAllExceptTop ) ;

      finally
        oVwr.EndPaintInternal ;
        oRenderer.AfterDraw ;
      end;

      ControlRepaint ;
    finally
      bInUpdate := False ;
    end;
  end ;

  procedure TGIS_ViewerWnd.ControlUpdateEditor(
    const _final : Boolean
  ) ;
  begin
    if _final then
      ControlUpdateWholeMap
    else begin
      ControlRepaint ;
      ControlProcessMessages ;
    end;
  end;

  function TGIS_ViewerWnd.ControlCanvasScale
    : Single ;
  begin
    Result := iCanvasScale ;
  end ;

  function TGIS_ViewerWnd.ControlCanvasHeight
    : Integer ;
  begin
    if iHeight <> 0 then
      Result := iHeight
    else
      Result := RoundS( Height * iCanvasScale ) ;
  end ;

  function TGIS_ViewerWnd.ControlCanvasWidth
    : Integer ;
  begin
    if iWidth <> 0 then
      Result := iWidth
    else
      Result := RoundS( Width * iCanvasScale ) ;
  end ;

  procedure TGIS_ViewerWnd.ControlHourglassShow  ;
  begin
    if GetCurrentThreadId <> MainThreadID then exit ;

    if Assigned( oRenderer ) then
      oRenderer.PrepareHourGlassContext ;

    oLastHourglas := GetTickCount ;
    oProgressRect.Align := TAlignLayout.Center ;
    oProgressRect.Visible := True ;

    oProgressRect.BringToFront ;
    oProgressRectAni.Enabled := True ;
    oProgressRect.Scale.X := 1 / inherited Scale.X ;
    oProgressRect.Scale.Y := 1 / inherited Scale.Y ;
    Application.ProcessMessages ;
  end;

  procedure TGIS_ViewerWnd.ControlHourglassHide  ;
  begin
    if GetCurrentThreadId <> MainThreadID then exit ;

    oProgressRectAni.Enabled := False ;
    oProgressRect.Visible    := False ;
  end;

  function TGIS_ViewerWnd.ControlHourglassShake
    : Boolean ;
  begin
    Result := False ;
    if GetCurrentThreadId <> MainThreadID then exit ;

    if Assigned( oRenderer ) then begin
      if Cardinal( GetTickCount - oLastHourglas ) > HOURGLASS_REFRESH then begin
        oProgressRectAni.Pause := False ;
        oRenderer.PrepareHourGlassContext ;
        Application.ProcessMessages ;
        oLastHourglas := GetTickCount ;
      end ;
    end;
  end ;

  procedure TGIS_ViewerWnd.ControlSet3DMode(
    const _mode : TGIS_Viewer3DMode
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerWnd.ControlRaiseEditorChangeEvent(
    _sender : TObject
  ) ;
  begin
    if Assigned( FOnEditorChange ) then
      FOnEditorChange( _sender ) ;
  end ;

  procedure TGIS_ViewerWnd.ControlAutoCenterViewport(
    const _dx, _dy : Double
  );
  var
    dx, dy : Double ;
  begin
    if FAutoCenter then begin
      dx := _dx ;
      dy := _dy ;
      oVwr.MoveViewportEx( dx, dy ) ;
      ControlUpdateWholeMap ;
    end ;
  end;

  function TGIS_ViewerWnd.SetViewer(const _viewer: TObject): TObject;
  begin
    Result := oVwr ;
    oVwr := _viewer as TGIS_Viewer;
    oVwr.ReParent( self ) ;
  end;

  function TGIS_ViewerWnd.GetViewer: TObject;
  begin
    Result := oVwr ;
  end;

//==============================================================================
// IGIS_ViewerWnd public methods
//==============================================================================

  procedure TGIS_ViewerWnd.PrintBmp(
    var _bmp : TGIS_Bitmap
  ) ;
  begin
    PrintBmp( _bmp, False ) ;
  end ;

  procedure TGIS_ViewerWnd.PrintBmp(
    var   _bmp  : TGIS_Bitmap ;
    const _full : Boolean
  ) ;
  var
    tbmp : TGIS_Bitmap ;
  begin
    if bInUpdate then exit ;
    bInUpdate := True ;
    try
      if assigned( _bmp ) then
        tbmp := _bmp
      else
        tbmp := TGIS_Bitmap.Create( ControlCanvasWidth, ControlCanvasHeight ) ;

      doPrintBmp( T_FMXBitmap( tbmp.NativeBitmap ), _full ) ;

      if not assigned( _bmp ) then
        _bmp := tbmp ;
    finally
      bInUpdate := False ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.Print ;
  begin
    Print( nil ) ;
  end ;

  procedure TGIS_ViewerWnd.Print(
    _printer : IGIS_Printer
  ) ;
  var
    pm : TGIS_PrintManager ;
    prn : TGIS_Printer ;
  begin
    // wait for ready
    if InPaint then exit ;

    if _printer = nil then
      prn := TGIS_Printer.Create( Printer )
    else begin
      if not ( _printer is TGIS_Printer ) then exit ;
      prn := _printer as TGIS_Printer ;
    end;

    pm := TGIS_PrintManager.Create ;
    try
      pm.Print( oVwr, prn ) ;
    finally
      FreeObject( pm ) ;
      if _printer = nil then
        FreeObject( prn )
    end ;
  end ;

  procedure TGIS_ViewerWnd.ZoomBy(
    const _zm : Double  ;
    const _x  : Integer ;
    const _y  : Integer
  ) ;
  begin
    if View3D then begin
      Viewer3D.ZoomBy( _zm, _x, _y );
    end
    else begin
      oWndHelper.UpdateCancel ;
      oWndHelper.DoZoom( _x * iCanvasScale,
                         _y * iCanvasScale,
                         _zm, True, True
                       );
      oWndHelper.UpdateDelayed ;
    end;
  end;

//==============================================================================
// TGIS_ViewerWnd public methods
//==============================================================================

  function TGIS_ViewerWnd.GetCacheBitmap
    : FMX.Graphics.TBitmap ;
  begin
    Result := nil ;
    if assigned( oPaintCacheTmp ) then begin
      Result := FMX.Graphics.TBitmap.Create(
                  oPaintCacheTmp.Width, oPaintCacheTmp.Height
                ) ;
      Result.Canvas.BeginScene ;
      try
        // background
        Result.Canvas.Fill.Kind := TBrushKind.Solid ;
        Result.Canvas.Fill.Color := BackgroundColor ;
        Result.Canvas.FillRect( RectF( 0, 0, Result.Width, Result.Height ), 0, 0,
                                [ TCorner.TopLeft,
                                  TCorner.TopRight,
                                  TCorner.BottomLeft,
                                  TCorner.BottomRight
                                ]
                                , 1
                              ) ;
        // copy cache
        Result.Canvas.DrawBitmap(
          oPaintCacheTmp,
          RectF( 0, 0, oPaintCacheTmp.Width, oPaintCacheTmp.Height),
          RectF( 0, 0, Result.Width, Result.Height),
          1, False
        ) ;
      finally
        Result.Canvas.EndScene ;
      end;
    end ;
  end ;

  procedure TGIS_ViewerWnd.PrintBmp(
    var _bmp : {$IFDEF LEVEL_XE5_FMX}
                 FMX.Graphics.TBitmap
               {$ELSE}
                 FMX.Types.TBitmap
               {$ENDIF}
  ) ;
  begin
    PrintBmp( _bmp, False ) ;
  end ;

  procedure TGIS_ViewerWnd.PrintBmp(
    var _bmp : {$IFDEF LEVEL_XE5_FMX}
                 FMX.Graphics.TBitmap ;
               {$ELSE}
                 FMX.Types.TBitmap ;
               {$ENDIF}
    const _full : Boolean
  ) ;
  var
    bmp : T_FMXBitmap ;
  begin
    if bInUpdate then exit ;
    bInUpdate := True ;
    try
      if assigned( _bmp ) then
        bmp := _bmp
      else
        bmp := T_FMXBitmap.Create( ControlCanvasWidth, ControlCanvasHeight ) ;

      doPrintBmp( bmp, _full ) ;

      if not assigned( _bmp ) then
        _bmp := bmp ;
    finally
      bInUpdate := False ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.PrintClipboard ;
  begin
    PrintClipboard( False ) ;
  end ;

  procedure TGIS_ViewerWnd.PrintClipboard(
    const _full : Boolean
  ) ;
  {$IFNDEF LEVEL_RX101_FMX}
    begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNIMPLEMENTED ) );
    end;
  {$ELSE}
    var
      bmp : T_FMXBitmap ;
      srf : TBitmapSurface ;
      clp : IFMXExtendedClipboardService ;
    begin
      clp := IFMXExtendedClipboardService(
               TPlatformServices.Current.GetPlatformService(
                 IFMXExtendedClipboardService
               )
             ) ;

      if not Assigned( clp ) then begin
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNIMPLEMENTED ) );
        exit ;
      end ;

      srf := nil ;
      bmp := nil ;
      try
        PrintBmp( bmp, _full ) ;

        srf := TBitmapSurface.Create ;
        srf.Assign( bmp );

        clp.SetImage( srf );
      finally
        FreeObject( srf ) ;
        FreeObject( bmp ) ;
      end ;
    end ;
  {$ENDIF}

  procedure Register ;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ViewerWnd ] ) ;
  end ;

var
  oldcursorService : IFMXCursorService ;
initialization
  {$IFNDEF GIS_MOBILE}
    // hook new cursor service before first use of a form
    // meaningless on IOS
    begin
      platformExtender := T_platformExtender.Create ;
      oldcursorService := IFMXCursorService( TPlatformServices.Current.GetPlatformService(IFMXCursorService)) ;
      platformExtender.cursorService := oldcursorService ;
      TPlatformServices.Current.RemovePlatformService(IFMXCursorService);
      TPlatformServices.Current.AddPlatformService(IFMXCursorService, platformExtender);
    end;
  {$ENDIF}

finalization
  {$IFNDEF GIS_MOBILE}
    // restore original cursor services
    TPlatformServices.Current.RemovePlatformService(IFMXCursorService);
    TPlatformServices.Current.AddPlatformService(IFMXCursorService, oldcursorService);
  {$ENDIF}

end.

