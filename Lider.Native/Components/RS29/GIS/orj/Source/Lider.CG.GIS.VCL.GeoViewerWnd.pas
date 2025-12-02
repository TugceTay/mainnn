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
  VCL Viewer Window.
}

unit VCL.GisViewerWnd ;
{$HPPEMIT '#pragma link "VCL.GisViewerWnd"'}

{$INCLUDE GisInclude.inc}

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  Winapi.D2D1,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections,

  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Clipbrd,
  Vcl.Printers,

  VCL.GisPrinters,
  VCL.GisPrintManager,

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  GisRtl,
  GisInterfaces,
  GisTypes,
  GisTypesUI,
  GisClasses,
  GisViewer,
  GisLayer,
  GisRendererAbstract,
  GisResource,
  GisViewerWndHelper,
  GisBasemapHelper,
  GisTilerHelper,  //TILER
  GisGraticuleHelper,
  GisCsSystems,

  VCL.GisFramework,
  VCL.GisRendererSkia.Stub,
  VCL.GisViewerBmp;

  {$HPPEMIT '#pragma comment( lib, "vclimg" )'}

type

  {#gendoc:hide}
  /// <summary>
  ///   Zooming rectangle.
  /// </summary>
  T_ZoomingRect = class
    public
      X : Integer ;
      Y : Integer ;
      Width : Integer ;
      Height : Integer ;
      Color : TGIS_Color ;
      Visible : Boolean ;
  end;

  {#gendoc:hide}
  /// <summary>
  ///   Dragging line.
  /// </summary>
  T_DraggingTrack = class
    public
      X1 : Integer ;
      Y1 : Integer ;
      X2 : Integer ;
      Y2 : Integer ;
      Color : TGIS_Color ;
      Visible : Boolean ;
  end;

  /// <summary>
  ///   Main visual control responsible for map presentation on window.
  /// </summary>
  /// <remarks>
  ///   For bitmap output see TGIS_ViewerBmp class.
  /// </remarks>
  [ComponentPlatformsAttribute( pfidWindows )]
  {#proxyhint:supported}
  TGIS_ViewerWnd = class( TCustomControl, IGIS_Viewer, IGIS_ViewerParent,
                          IGIS_ViewerWnd )
    private
      iWidth          : Integer ;
      iHeight         : Integer ;
      iTmpWidth       : Integer ;
      iTmpHeight      : Integer ;
      iReenterCount   : Integer ;
      oVwr            : TGIS_Viewer ;
      oRenderer       : TGIS_RendererAbstract ;
      oWndHelper      : TGIS_ViewerWndHelper ;
      oBasemapHelper  : TGIS_BasemapHelper ;
      oTilerHelper    : TGIS_TilerHelper ;  //TILER
      oTilerExtent    : TGIS_Extent ;       //TILER
      oTilerState     : TGIS_TilerState ;   //TILER
      bTilerSuspended : Boolean ;
      oGraticuleHelper: TGIS_GraticuleHelper ;
      oGraticule      : TGIS_Graticule ;
      oZoomingRect    : T_ZoomingRect ;
      oDraggingTrack  : T_DraggingTrack ;
      oExtent         : TGIS_Extent ;
      oTimer          : TTimer ;

      bD2DRendering   : Boolean ;
      bTopMostDirectOnWindowRenderTarget
                      : Boolean ;
      bD2DCanvas      : Boolean ;
      oD2DCanvas      : TD2DCanvas;

      bSkiaRendering    : Boolean ;
      oSKCanvas         : TObject ;
      oOpenGLConnection : TObject ;
      oOpenGLCanvas     : TObject ;

      /// <summary>
      ///   Paint object depended on the current renderer.
      ///   It is used to assemble the displayed content.
      /// </summary>
      oPaintTmp       : TObject ;

      /// <summary>
      ///   Cache bitmap with full displayed content.
      /// </summary>
      oFullCache      : TGIS_Bitmap ;

      /// <summary>
      ///   Cache used at paint operations (e.g. drawing, zooming).
      ///   Depending on parameters it can be set to oFullCache or FCacheBitmap.
      /// </summary>
      oPaintCache     : TGIS_Bitmap ;

      /// <summary>
      ///   Bitmap use upon progressive display.
      /// </summary>
      FProgressBitmap : TGIS_Bitmap ;

    private
      bInUpdate       : Boolean ;
      bInFlash        : Boolean ;

    private
      zmx             : TRect   ;
      dmx             : Double  ;
      mouseBlocked    : Boolean ;
      mousePosOld     : TPoint  ;
      mousePos        : TPoint  ;
      mousePosLast    : TPoint  ;
      zoomingColor    : TColor  ;
      zoomDistance    : Integer ;
      hourglassCursor : TCursor ;
    private

      /// <summary>
      ///   If True, then the screen will be centered on click, edit, etc.
      /// </summary>
      FAutoCenter : Boolean ;

      /// <summary>
      ///   Border style.
      /// </summary>
      FBorderStyle : TBorderStyle ;


      /// <summary>
      ///   Mode of viewer (dragging, selecting ...).
      /// </summary>
      FMode : TGIS_ViewerMode ;

      /// <summary>
      ///   Mode of viewer (dragging, selecting ...).
      /// </summary>
      F3DMode : TGIS_Viewer3DMode ;

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
      FCacheBitmap    : TGIS_Bitmap ;
      bCacheBitmapNew : Boolean ;

      FTileContext    : TGIS_RendererContext ;
      FDrawContext    : TGIS_RendererContext ;
      FTopContext     : TGIS_RendererContext ;

      FFlashContext   : TGIS_RendererContext ;
      FFlashTransparency : Integer ;

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
      FBackgroundColor : TColor  ;

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
      ///   Cursor for gisZoomEx mode.
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
      ///   Cursor for TGIS_Viewer3DMode.CameraPosition.
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

      /// <summary>
      ///   If true then drawing does not use WinForms bitmaps for caching.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Available only via metadata:
      ///     'TGIS_ViewerWnd.Paint.Direct2D.Full'.
      ///   </para>
      ///   <para>
      ///     Default value is False.
      ///   </para>
      /// </remarks>
      metPaintFullDirect2D : Boolean ;

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
      ///   If true then zoom gesture is available for Mode.Edit.
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
      ///   Stored 2D mode context.
      /// </summary>
      oldMode2D : TGIS_ViewerMode ;

      /// <summary>
      ///   Stored 2D renderer name.
      /// </summary>
      oldRenderer2D : String ;

      {$IFDEF GIS_PDK}
        oProxy : TGIS_ViewerWnd ;
        bItIsProxy : Boolean ;
      {$ENDIF}

    private // properties - events

      /// <summary>
      ///   Handler for ModeChange event.
      /// </summary>
      FOnModeChange    : TNotifyEvent ;

      /// <summary>
      ///   Handler for OnEditorChange event.
      /// </summary>
      FOnEditorChange  : TNotifyEvent ;

      /// <summary>
      ///   BeforePaint event. Will be fired before any Paint operation.
      /// </summary>
      FOnBeforePaint   : TGIS_PaintEvent ;

      /// <summary>
      ///   BeforePaintRenderer event. Will be fired before any Paint operation.
      /// </summary>
      FOnBeforePaintRenderer : TGIS_RendererEvent ;

      /// <summary>
      ///   AfterPaint event. Will be fired after any Paint operation.
      /// </summary>
      FOnAfterPaint    : TGIS_PaintEvent ;

      /// <summary>
      ///   AfterPaintRenderer event. Will be fired after any Paint operation.
      /// </summary>
      FOnAfterPaintRenderer : TGIS_RendererEvent ;

      /// <summary>
      ///   PaintExtra event. Will be fired after real draw.
      /// </summary>
      FOnPaintExtra    : TGIS_RendererEvent ;

      /// <summary>
      ///   BeforeUpdate event. Will be fired before Update operation.
      /// </summary>
      FOnBeforeUpdate  : TGIS_RendererEvent ;

      /// <summary>
      ///   UpdateEx event. Will be fired for Update operation.
      /// </summary>
      FOnUpdate        : TGIS_RendererEvent ;

      /// <summary>
      ///   AfterUpdate event. Will be fired after Update operation.
      /// </summary>
      FOnAfterUpdate   : TGIS_RendererEvent ;

      /// <summary>
      ///   OnTapSimple event. Will be fired upon press down/up.
      /// </summary>
      FOnTapSimple     : TMouseEvent ;

      /// <summary>
      ///   OnTapLong event. Will be fired upon longer press down. }
      /// </summary>
      FOnTapLong       : TMouseEvent ;

      /// <summary>
      ///   OnTapDouble event. Will be fired upon double press down/up. }
      /// </summary>
      FOnTapDouble     : TMouseEvent ;

    private  // class private methods
      procedure ensureCursor     ;
      procedure ensure3DCursor   ;
      function  gesturePanEnabled: Boolean ;
      function  gestureZoomEnabled
                                 : Boolean ;
      function  gesture3DPanEnabled
                                 : Boolean ;
      function  gesture3DZoomEnabled
                                 : Boolean ;
      procedure setGestures      ;
      procedure setGestures3D    ;
      procedure doGestureProc    (       _sender  : TObject ;
                                   const _info    : TGestureEventInfo;
                                   var   _handled : Boolean
                                 ) ;
      procedure createContext    ;
      procedure resizeContext    ;
      procedure destroyContext   ;

      procedure createSkiaCanvas ;
      procedure freeSkiaCanvas   ( const _freeOpenGL
                                                  : Boolean
                                 ) ;

      procedure doTimer          (       _sender  : TObject
                                 ) ;
      procedure resetMousePos    ( const _x, _y   : Integer
                                 ) ;
      procedure updateExecute    (       _sender  : TObject ;
                                         _ctx     : TGIS_ViewerWndHelperRun
                                 ) ;
      procedure updateSynchronize(       _sender  : TObject ;
                                         _ctx     : TGIS_ViewerWndHelperRun ;
                                         _final   : Boolean
                                 ) ;
      procedure updateTap        (       _sender  : TObject
                                 ) ;

      procedure doGraticule      (      _sender   : TObject ;
                                        _renderer : TGIS_RendererAbstract ;
                                        _mode     : TGIS_DrawMode
                                 ) ;
      procedure doPrintBmp       ( const _bitmap  : TGIS_Bitmap ;
                                   const _full    : Boolean
                                 ) ;

    private // IGIS_Viewer property access routines
      function  fget_AutoStyle          : Boolean;
      procedure fset_AutoStyle          ( const _value : Boolean      ) ;
      function  fget_BigExtent          : TGIS_Extent ;
      function  fget_BigExtentMargin    : Integer     ;
      procedure fset_BigExtentMargin    ( const _value  : Integer     ) ;
      function  fget_KeepScale          : Boolean ;
      procedure fset_KeepScale          ( const _value  : Boolean     ) ;
      function  fget_BusyLevel          : Integer     ;
      function  fget_BusyText           : String      ;
      function  fget_Center             : TGIS_Point  ;
      procedure fset_Center             ( const _value  : TGIS_Point  ) ;
      function  fget_CenterPtg          : TGIS_Point  ;
      procedure fset_CenterPtg          ( const _value  : TGIS_Point  ) ;
      function  fget_Color              : TGIS_Color  ;
      procedure fset_Color              ( const _value  : TGIS_Color  ) ;
      function  fget_Copyright          : String      ;
      function  fget_CS                 : TGIS_CSCoordinateSystem ;
      procedure fset_CS                 ( const _value  : TGIS_CSCoordinateSystem ) ;
      function  fget_CustomData         : TGIS_StringList ;
      function  fget_CustomPPI          : Integer ;
      procedure fset_CustomPPI          ( const _value  : Integer     ) ;
      function  fget_Editor             : IGIS_Editor ;
      procedure fset_Editor             ( const _value  : IGIS_Editor ) ;
      function  fget_Extent             : TGIS_Extent ;
      function  fget_FileCopyrights     : String      ;
      function  fget_FontScale          : Integer ;
      procedure fset_FontScale          ( const _value : Integer     ) ;
      function  fget_FullDrawExtent     : TGIS_Extent ;
      function  fget_Hierarchy          : IGIS_HierarchyManager ;
      function  fget_IncrementalPaint   : Boolean     ;
      procedure fset_IncrementalPaint   ( const _value  : Boolean     ) ;
      function  fget_TiledPaint         : Boolean     ;   //TILER
      procedure fset_TiledPaint         ( const _value  : Boolean     ) ; //TILER
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
      procedure fset_MultiUserMode      ( const _value  : TGIS_MultiUser ) ;
      function  fget_OverlappedExtentMargin
                                        : Integer     ;
      procedure fset_OverlappedExtentMargin
                                        ( const _value  : Integer     ) ;
      function  fget_PPI                : Integer ;
      function  fget_ProjectFile        : TGIS_ConfigAbstract ;
      procedure fset_ProjectFile        ( const _value  : TGIS_ConfigAbstract ) ;
      function  fget_ProjectName        : String      ;
      function  fget_DelayedUpdate      : Integer     ;
      procedure fset_DelayedUpdate      ( const _value  : Integer     ) ;
      function  fget_ProgressiveUpdate  : Integer     ;
      procedure fset_ProgressiveUpdate  ( const _value  : Integer     ) ;
      function  fget_RestrictedDrag     : Boolean     ;
      procedure fset_RestrictedDrag     ( const _value  : Boolean     ) ;
      function  fget_RestrictedExtent   : TGIS_Extent ;
      procedure fset_RestrictedExtent   ( const _value  : TGIS_Extent ) ;
      function  fget_RotationAngle      : Double      ;
      procedure fset_RotationAngle      ( const _value  : Double      ) ;
      function  fget_RotationPoint      : TGIS_Point  ;
      procedure fset_RotationPoint      ( const _value  : TGIS_Point  ) ;
      function  fget_ScaleAsFloat       : Double      ;
      procedure fset_ScaleAsFloat       ( const _value  : Double      ) ;
      function  fget_ScaleAsText        : String      ;
      procedure fset_ScaleAsText        ( const _value  : String      ) ;
      function  fget_SelectionGisColor  : TGIS_Color  ;
      procedure fset_SelectionGisColor  ( const _value  : TGIS_Color  ) ;
      function  fget_SelectionOutlineOnly
                                        : Boolean     ;
      procedure fset_SelectionOutlineOnly(
                                          const _value  : Boolean     ) ;
      function  fget_SelectionTransparency
                                        : Integer     ;
      procedure fset_SelectionTransparency(
                                          const _value  : Integer     ) ;
      function  fget_SelectionWidth     : Integer ;
      procedure fset_SelectionWidth     ( const _value  : Integer     ) ;
      function  fget_SystemPPI          : Integer     ;
      function  fget_ViewerParent       : IGIS_ViewerParent ;
      function  fget_ViewerParentRoot   : IGIS_ViewerParent ;
      function  fget_Viewport           : TGIS_Point  ;
      procedure fset_Viewport           ( const _value  : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
      procedure fset_VisibleExtent      ( const _value  : TGIS_Extent ) ;
      function  fget_UseAnimations      : Boolean     ;
      procedure fset_UseAnimations      ( const _value  : Boolean     ) ;
      function  fget_UseRTree           : Boolean     ;
      procedure fset_UseRTree           ( const _value  : Boolean     ) ;
      function  fget_Zoom               : Double      ;
      procedure fset_Zoom               ( const _value  : Double      ) ;
      function  fget_ZoomEx             : Double      ;
      procedure fset_ZoomEx             ( const _value  : Double      ) ;
      function  fget_MasterViewer       : IGIS_Viewer ;
      procedure fset_MasterViewer       ( const _value : IGIS_Viewer ) ;
      function  fget_UponDestroy        : Boolean ;
      function  fget_TemporaryScaleInternal
                                        : Double      ;
      procedure fset_TemporaryScaleInternal
                                        ( const _value : Double       ) ;
      function  fget_TemporaryVisibleExtent
                                        : TGIS_Extent ;
      procedure fset_TemporaryVisibleExtent
                                        ( const _value : TGIS_Extent  ) ;

    private // TGIS_ViewerWnd property access routines
      procedure fset_Mode               ( const _value  : TGIS_ViewerMode ) ;
      procedure fset_3DMode             ( const _value  : TGIS_Viewer3DMode ) ;
      procedure fset_Renderer           ( const _value  :
                                              TGIS_RendererAbstract ) ;
    private
      function  fget_ColorEx            : TColor ;
      procedure fset_ColorEx            ( const _value  : TColor      ) ;
      function  fget_SelectionColor     : TColor ;
      procedure fset_SelectionColor     ( const _value  : TColor      ) ;
      procedure fset_BorderStyle        ( const _value  : TBorderStyle) ;
      function  fget_View3D             : Boolean ;
      procedure fset_View3D             ( const _value  : Boolean     ) ;
      function  fget_Viewer3D           : IGIS_Viewer3D ;
      procedure fset_Viewer3D           ( const _viewer : IGIS_Viewer3D ) ;
    private
      function  fget_BusyEvent              : TGIS_BusyEvent ;
      procedure fset_BusyEvent              ( const _value : TGIS_BusyEvent  ) ;
      function  fget_HelpEvent              : TGIS_HelpEvent ;
      procedure fset_HelpEvent              ( const _value : TGIS_HelpEvent  ) ;
      {$IFDEF GIS_XDK}
        function  fget_UseHelp              : Boolean ;
        procedure fset_UseHelp              ( const _value : Boolean         ) ;
      {$ENDIF}
      function  fget_EditorSnapPointEvent   : TGIS_EditorSnapPointEvent ;
      procedure fset_EditorSnapPointEvent   ( const _value :
                                                TGIS_EditorSnapPointEvent    ) ;
      function  fget_EditorPointChangeEvent : TGIS_EditorPointChangeEvent ;
      procedure fset_EditorPointChangeEvent   ( const _value :
                                                TGIS_EditorPointChangeEvent    ) ;
      function  fget_EditorPointMoveEvent   : TGIS_EditorPointMoveEvent ;
      procedure fset_EditorPointMoveEvent   ( const _value :
                                                TGIS_EditorPointMoveEvent    ) ;
      function  fget_ExtentChangeEvent      : TNotifyEvent ;
      procedure fset_ExtentChangeEvent      ( const _value : TNotifyEvent    ) ;
      function  fget_LayerAddEvent          : TGIS_LayerEvent ;
      procedure fset_LayerAddEvent          ( const _value : TGIS_LayerEvent ) ;
      function  fget_LayerDeleteEvent       : TGIS_LayerEvent ;
      procedure fset_LayerDeleteEvent       ( const _value : TGIS_LayerEvent ) ;
      function  fget_PaintExceptionEvent    : TGIS_PaintExceptionEvent ;
      procedure fset_PaintExceptionEvent    ( const _value :
                                                 TGIS_PaintExceptionEvent    ) ;
      function  fget_PasswordEvent          : TGIS_TemplateProducerEvent ;
      procedure fset_PasswordEvent          ( const _value :
                                               TGIS_TemplateProducerEvent    ) ;
      function  fget_ProjectCloseEvent      : TNotifyEvent ;
      procedure fset_ProjectCloseEvent      ( const _value : TNotifyEvent    ) ;
      function  fget_ProjectOpenEvent       : TNotifyEvent ;
      procedure fset_ProjectOpenEvent       ( const _value : TNotifyEvent    ) ;
      function  fget_VisibleExtentChangeEvent : TNotifyEvent ;
      procedure fset_VisibleExtentChangeEvent ( const _value : TNotifyEvent  ) ;
      function  fget_ZoomChangeEvent        : TNotifyEvent ;
      procedure fset_ZoomChangeEvent        ( const _value : TNotifyEvent    ) ;


  private
      procedure WMPaint          ( var    _msg  : TWMPaint
                                 ); message WM_PAINT;
      procedure WMEraseBkGnd     ( var    _msg  : TWMPaint
                                 ); message WM_ERASEBKGND;
      procedure WMSize           ( var    _msg  : TWMSize
                                 ); message WM_SIZE;
      procedure CMCtl3DChanged   ( var    _msg : TMessage
                                 ); message CM_CTL3DCHANGED;

      procedure TurnD2DCanvas    ( const _value : Boolean);
      function  CreateD2DCanvas  : Boolean;
    protected
      {#gendoc:hide}
      procedure CreateWnd; override;


    protected  // windows overridden

      {#gendoc:hide}
      //   For recreating control using new styles such borders, scroll bars
      //   etc. Standard override.
      procedure CreateParams     ( var _params     : TCreateParams
                                 ) ; override;
      {#gendoc:hide}
      procedure MouseDown        (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Integer      ;
                                         _y        : Integer
                                 ) ; override;
      {#gendoc:hide}
      procedure MouseUp          (       _button   : TMouseButton ;
                                         _shift    : TShiftState  ;
                                         _x        : Integer      ;
                                         _y        : Integer
                                 ) ; override;
      {#gendoc:hide}
      procedure MouseMove        (       _shift    : TShiftState  ;
                                         _x        : Integer      ;
                                         _y        : Integer
                                 ) ; override;
      {#gendoc:hide}
      procedure Paint            ; override;
      {#gendoc:hide}
      procedure Resize           ; override;

      {#gendoc:hide}
      procedure WndProc          ( var   _msg      : TMessage
                                 ); override;

     public
      {$IFDEF GIS_PDK}
        {#gendoc:hide}
        procedure DestroyHandle  ; override ;
      {$ENDIF}

    public // extra mouse handlers

      /// <summary>
      ///   Execute operation equal to the reaction to a mouse down event.
      /// </summary>
      /// <param name="_button">
      ///    active mouse button
      /// </param>
      /// <param name="_shift">
      ///    state of modifiers keys
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
      ///   Standard constructor.
      /// </summary>
      /// <param name="_owner">
      ///   control owner
      /// </param>
      constructor Create         ( _owner            : TComponent
                                 ) ; override;
      /// <summary>
      ///   Standard destructor.
      /// </summary>
      destructor Destroy         ; override;

    public // IGIS_Viewer public methods

      /// <inheritdoc from="IGIS_Viewer"/>
      function ChangeHash        : Int64 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Subscribe        ( const _control    : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnSubscribe      ( const _control    : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure NotifySubscribers( const _event      : Integer ;
                                   const _context    : TObject
                                 ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function NotifyPaintException(
                                   const _message    : String ;
                                   const _exception  : Exception
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Lock             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ( const _redraw     : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Interrupt        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Interrupted      : Boolean ; //TILER

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
      procedure BusyPrepare      (       _sender     : TObject ;
                                         _text       : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyRelease      (       _sender     : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyShake        (       _sender     : TObject ;
                                         _pos        : Int64 ;
                                         _end        : Int64 ;
                                   var   _abort      : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RaiseBusyEvent   (       _sender     : TObject ;
                                         _pos        : Int64 ;
                                         _end        : Int64 ;
                                   var   _abort      : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RaiseHelpEvent   (       _sender     : TObject ;
                                         _name       : String
                                 ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedBusyEvent: TGIS_BusyEvent;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedHelpEvent: TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  StorePaintState  : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RestorePaintState( var _state        : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BeginPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure EndPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  SynchronizePaint ( const _interrupt : Boolean
                                 ) : Boolean ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentLock     ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentUnlock   ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function ReParent          ( const _parent     : IGIS_ViewerParent
                                 ) : IGIS_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function AttachLayer       ( const _layer : TGIS_LayerAbstract
                                 ) : IGIS_Viewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path       : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path       : String ;
                                   const _strict     : Boolean
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
      procedure Add              ( const _layer      : TGIS_LayerAbstract
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Get              ( const _name       : String
                                 ) : TGIS_LayerAbstract ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Delete           ( const _name       : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure AddHierarchy     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Draw             ( const _renderer   : TObject     ;
                                   const _mode       : TGIS_DrawMode
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetGrid          ( const _extent     : TGIS_Extent  ;
                                   const _grid       : TGIS_GridArray
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
      procedure InvalidateExtent ( const _extent  : TGIS_Extent ;
                                   const _deep    : Boolean
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateWholeMap  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateTopmost   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateBasemap   ;

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
      function  MapToScreen      ( const _ptg : TGIS_Point
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen3D    ( const _ptg : TGIS_Point3D
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap      ( const _pt  : TPoint
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap3D    ( const _pt  : TPoint
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenEx    ( const _pt  : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapEx    ( const _pt  : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenRect  ( const _rct : TGIS_Extent
                                 ) : TRect ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapRect  ( const _rct : TRect
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  PixelsToTwips    ( const _size : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPixels    ( const _size : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPoints    ( const _size : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewport     ( var   _dx, _dy  : Integer
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy  : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x ,  _y  : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure CenterViewport   ( const _ptg      : TGIS_Point
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKT       ( const  _wkt     : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByEPSG      ( const  _epsg    : Integer
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKTFile   ( const  _path    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint     ( const _ptg      : TGIS_Point
                                 ) : TGIS_Point;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint   ( const _ptg      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint3D   ( const _ptg      : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RotatedPoint3D_ref(
                                   var   _ptg      : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint3D ( const _ptg      : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnrotatedPoint3D_ref(
                                   var  _ptg       : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedExtent    ( const _extent   : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedExtent  ( const _extent   : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetRenderContext : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForBackgroundProcesses ;

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy   (        _sender  : TObject ;
                                    const _proc    : TGIS_WaitForNotBusyProc
                                 ) ;

    public // IGIS_ViewerParent public methods

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlClose           ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlDrawTexture     (       _bmp      : TObject     ;
                                         const _extent   : TGIS_Extent ;
                                         const _ppi      : Integer
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
      procedure ControlFlash           ( const _times    : Integer ;
                                         const _delay    : Integer
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
      procedure ControlProcessMessages ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlRepaint         ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
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
      procedure ControlUpdateEditor    ( const _final    : Boolean
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlHourglassShow   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlHourglassHide   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlHourglassShake  : Boolean ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlSet3DMode       ( const _mode     : TGIS_Viewer3DMode
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlRaiseEditorChangeEvent(
                                              _sender   : TObject
                                       ) ;
      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlAutoCenterViewport      ( const _dx, _dy : Double ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlExtentChanged ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  SetViewer ( const _viewer : TObject ) : TObject ;
      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  GetViewer : TObject ;

    public // IGIS_ViewerWnd public methods

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp         ( var   _bmp    : TGIS_Bitmap
                                 ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp         ( var   _bmp    : TGIS_Bitmap ;
                                   const _full   : Boolean
                                 ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print            ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print            ( _printer      : IGIS_Printer
                                 ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure ZoomBy           ( const _zm     : Double  ;
                                   const _x      : Integer ;
                                   const _y      : Integer
                                 ) ; overload;
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
      function  GetCacheBitmap   : TBitmap ;

      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      procedure PrintBmp         ( var   _bmp    : {$IFNDEF GENXDK}
                                                     VCL.Graphics.TBitmap
                                                   {$ELSE}
                                                     TBitmap
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
      procedure PrintBmp         ( var   _bmp    : {$IFNDEF GENXDK}
                                                     Vcl.Graphics.TBitmap ;
                                                   {$ELSE}
                                                     TBitmap ;
                                                   {$ENDIF}
                                   const _full   : Boolean
                                 ) ; overload ; virtual ;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Print the current view to the clipboard.
      /// </summary>
      procedure PrintClipboard   ; overload ; virtual ;

      /// <summary>
      ///   Print the current view to the clipboard.
      /// </summary>
      /// <param name="_full">
      ///   if yes, all paint events are triggered
      /// </param>
      procedure PrintClipboard   ( const _full   : Boolean
                                 ) ; overload ; virtual;


      {$IFDEF GIS_PDK}
        procedure SetProxy       ( const _proxy : TGIS_ViewerWnd
                                 ) ;
      {$ENDIF}

    public
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
      property CustomData   : TGIS_StringList  read  fget_CustomData     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Editor       : IGIS_Editor      read  fget_Editor
                                               write fset_Editor         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Extent       : TGIS_Extent      read  fget_Extent         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FileCopyrights : String         read  fget_FileCopyrights ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FullDrawExtent : TGIS_Extent    read  fget_FullDrawExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Hierarchy    : IGIS_HierarchyManager
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
      property LabelsReg    : TGIS_LabelsAreaAbstract
                                               read fget_LabelsReg       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Level : Double                  read  fget_Level
                                               write fset_Level          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property OverlappedExtentMargin : Integer
                                       read  fget_OverlappedExtentMargin
                                       write fset_OverlappedExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property PPI          : Integer  read fget_PPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectFile  : TGIS_ConfigAbstract
                                               read  fget_ProjectFile    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectName  : String           read  fget_ProjectName    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedExtent : TGIS_Extent  read  fget_RestrictedExtent
                                               write fset_RestrictedExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationAngle : Double          read  fget_RotationAngle
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
      property SelectionGisColor  : TGIS_Color read  fget_SelectionGisColor
                                               write fset_SelectionGisColor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SystemPPI    : Integer          read fget_SystemPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParent : IGIS_ViewerParent
                                               read  fget_ViewerParent   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParentRoot : IGIS_ViewerParent
                                               read  fget_ViewerParentRoot ;

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
      property MasterViewer : IGIS_Viewer      read  fget_MasterViewer
                                               write fset_MasterViewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UponDestroy  : Boolean          read  fget_UponDestroy    ;

      {$IFDEF GIS_PDK}
        {#gendoc:hide}
        property Proxy      : TGIS_ViewerWnd   read  oProxy ;
      {$ENDIF}

    public

      /// <summary>
      ///   Currently operating renderer. By assigning a new a different
      ///   rendering engine a rendering will be switched to a new engine .
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Do not free renderer directly - control maintains a renderer
      ///     life time.
      ///   </note>
      /// </remarks>
      property Renderer     : TGIS_RendererAbstract
                                               read  oRenderer
                                               write fset_Renderer       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryScaleInternal : Double read  fget_TemporaryScaleInternal
                                               write fset_TemporaryScaleInternal ;
                                               //default 0                 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryVisibleExtent : TGIS_Extent
                                               read  fget_TemporaryVisibleExtent
                                               write fset_TemporaryVisibleExtent ;

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

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Graticule object. Set Graticule.Enabled to turn on/off.
      /// </summary>
      property Graticule : TGIS_Graticule
                                               read  oGraticule ;


    published // TGIS_ViewerWnd

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


    published // properties derived from base class

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Align ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Anchors ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Ctl3D ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Enabled ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property HelpContext ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Hint ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentColor ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentCtl3D ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentShowHint ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property PopupMenu ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property TabStop  ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property TabOrder ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Visible ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property DragCursor ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property DragKind ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property DragMode ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property StyleElements;

      {$IFDEF LEVEL_RX11_VCL}
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property StyleName;
      {$ENDIF}

    published // published taken from IGIS_Viewer

      /// <inheritdoc from="IGIS_Viewer"/>
      property AutoStyle : Boolean             read  fget_AutoStyle
                                               write fset_AutoStyle ;

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
                                               default 0                ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FontScale        : Integer      read  fget_FontScale
                                               write fset_FontScale
                                               default 100              ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IncrementalPaint : Boolean      read  fget_IncrementalPaint
                                               write fset_IncrementalPaint
                                               default True             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TiledPaint : Boolean            read  fget_TiledPaint   //TILER
                                               write fset_TiledPaint
                                               default False           ;

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

    published // published taken from TGIS_ViewerWnd

      /// <summary>
      ///   If True, then the screen will be centered on click, edit, etc.
      /// </summary>
      property AutoCenter       : Boolean      read  FAutoCenter
                                               write FAutoCenter
                                               default False            ;

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

    published // reintroduced

      {#gendoc:hide:GENXDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      property Color            : TColor       read  fget_ColorEx
                                               write fset_ColorEx
                                               default clWindow         ;

    published // new properties

      /// <summary>
      ///   Border visibility.
      /// </summary>
      property BorderStyle     : TBorderStyle  read  FBorderStyle
                                               write fset_BorderStyle   ;

      /// <summary>
      ///   Color used for selecting object.
      /// </summary>
      property SelectionColor   : TColor       read  fget_SelectionColor
                                               write fset_SelectionColor
                                               default clRed            ;

    published // events derived from base class

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnContextPopup ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnDblClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnEnter ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnExit ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnKeyDown
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnKeyPress
               {$IFDEF GENDOC}
                 : TKeyPressEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnKeyUp
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseDown
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseMove
               {$IFDEF GENDOC}
                 : TMouseMoveEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseUp
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseWheel
               {$IFDEF GENDOC}
                 : TMouseWheelEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseWheelUp
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseWheelDown
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnDragDrop ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnDragOver ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnEndDrag ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnStartDrag ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnGesture ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnResize ;

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
      property HelpEvent : TGIS_HelpEvent     read  fget_HelpEvent
                                              write fset_HelpEvent ;

      {$IFDEF GIS_XDK}
         /// <summary>
         ///   True if Help Event should be used. There is no other way in ActiveX
         ///   to really know if viewer is attached.
         /// </summary>
         property UseHelp : Boolean           read  fget_UseHelp
                                              write fset_UseHelp ;
      {$ENDIF}

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
      property LayerDeleteEvent : TGIS_LayerEvent read fget_LayerDeleteEvent
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

    published // IGIS_Viewer events

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
      ///   AfterPaintRenderer event. Will be fired before any Paint operation.
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
      ///   Update event. Will be fired for Update operation. If attached,
      ///   then Draw(renderer) method will not be called internally.
      /// </summary>
      property UpdateEvent       : TGIS_RendererEvent
                                               read  FOnUpdate
                                               write FOnUpdate ;

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

    {$IFDEF GIS_XDK}
      public
        {#gendoc:hide:GENXDK}
        XDK : TGIS_ControlXDK ;
    {$ENDIF}

  end ;

  {#gendoc:hide}
  procedure Register;

const
   {#gendoc:hide}
   METADATA_PAINTFULLDIRECT2D
     = 'TGIS_ViewerWnd.Paint.Direct2D.Full';

{.$DEFINE SKIA_OPENGL_ONEWINDOW}

//##############################################################################
implementation

{$R GisViewerWnd_16x16.RES}
{$R VCL.Cursors.RES}

uses
  System.Math,
  System.UITypes,
  Vcl.Themes,

  GisInternals,
  GisEditor,
  GisFunctions,
  GisViewer3DDirectX,
  GisViewer3DBase,
  VCL.GisRenderer,

  VCL.GisRendererGdi32,
  VCL.GisRendererGdiPlus,
  VCL.GisRendererDirect2D,
  VCL.GisRendererSkia.Common;

var
  oldHandle : THandle = 0 ; //TILER

const

  // Cursor for dragging.
  DRAG_CURSOR     = 15  ;

  // Cursor for selecting.
  SELECT_CURSOR   = 16  ;

  // Cursor for zooming.
  ZOOM_CURSOR     = 18  ;

  // Cursor for editing.
  EDIT_CURSOR     = 19  ;

  // Cursor for custom operations.
  CUSTOM_CURSOR   = 20  ;

  // Cursor for lengthy operation.
  WAIT_CURSOR     = 21  ;

  // Cursor for TGIS_Viewer3DMode.CameraPosition.
  CAMPOS_CURSOR   = 30  ;

  // Cursor for TGIS_Viewer3DMode.CameraRotation.
  CAMROT_CURSOR   = 31  ;

  // Cursor for TGIS_Viewer3DMode.CameraXYZ.
  CAMXYZ_CURSOR   = 32  ;

  // Cursor for TGIS_Viewer3DMode.CameraXY.
  CAMXY_CURSOR    = 33  ;

  // Cursor for TGIS_Viewer3DMode.Zoom.
  CAMZOOM_CURSOR  = 34  ;

  // Cursor for TGIS_Viewer3DMode.SunPosition.
  SUNPOS_CURSOR   = 35  ;

  // Cursor for TGIS_Viewer3DMode.Select.
  SELECT3D_CURSOR = 36  ;

const
  NO_MOUSE_POS = GIS_MIN_INTEGER ; // non existing previous mouse position

type
  //TILER
  // factory to build TGIS)_ViewerBmp on demand
  T_ViewerBmpFactory = class ( TGIS_ViewerBmpFactory )
    public
      function CreateViewer : TObject ; override ;
  end;

  function T_ViewerBmpFactory.CreateViewer : TObject ;
  begin
    Result := TGIS_ViewerBmp.Create;
  end;

//==============================================================================
// private methods
//==============================================================================

  procedure TGIS_ViewerWnd.ensureCursor ;
  begin
    if View3D then begin
      ensure3DCursor ;
      exit ;
    end;

    if not (csLoading in ComponentState) then
    begin
      case Mode of
        TGIS_ViewerMode.Select      :
            if CursorForSelect  = crDefault then
              Cursor := SELECT_CURSOR
            else
              Cursor := CursorForSelect ;
        TGIS_ViewerMode.Drag        :
            if CursorForDrag   = crDefault then
              Cursor := DRAG_CURSOR
            else
              Cursor := CursorForDrag ;
        TGIS_ViewerMode.Zoom        :
            if CursorForZoom   = crDefault then
              Cursor := ZOOM_CURSOR
            else
              Cursor := CursorForZoom ;
        TGIS_ViewerMode.Edit        :
            if CursorForEdit   = crDefault then
              Cursor := EDIT_CURSOR
            else
              Cursor := CursorForEdit ;
        TGIS_ViewerMode.ZoomEx      :
            if CursorForZoomEx  = crDefault then
              Cursor := ZOOM_CURSOR
            else
              Cursor := CursorForZoomEx ;
        TGIS_ViewerMode.UserDefined :
            if CursorForUserDefined  = crDefault then
              Cursor := CUSTOM_CURSOR
            else
              Cursor := CursorForUserDefined ;
        else
        begin
          Assert( False, 'not tested' ) ;
        end ;
      end ;

      if not IsEmpty then
        Perform( Winapi.Messages.WM_SETCURSOR, Handle, HTCLIENT ) ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.ensure3DCursor ;
  begin
    if not (csLoading in ComponentState) then
    begin
      case F3DMode of
        TGIS_Viewer3DMode.CameraPosition :
          begin
            if FCursorForCameraPosition = crDefault then
              Cursor := CAMPOS_CURSOR
            else
              Cursor := CursorForCameraPosition
          end ;

        TGIS_Viewer3DMode.CameraRotation :
          begin
            if CursorForCameraRotation = crDefault then
              Cursor := CAMROT_CURSOR
            else
              Cursor := CursorForCameraRotation
          end ;

        TGIS_Viewer3DMode.CameraXYZ :
          begin
            if CursorForCameraXYZ = crDefault then
              Cursor := CAMXYZ_CURSOR
            else
              Cursor := CursorForCameraXYZ
          end ;

        TGIS_Viewer3DMode.CameraXY :
          begin
            if CursorForCameraXY = crDefault then
              Cursor := CAMXY_CURSOR
            else
              Cursor := CursorForCameraXY
          end ;

        TGIS_Viewer3DMode.Zoom :
          begin
            if CursorForZoom  = crDefault then
              Cursor := CAMZOOM_CURSOR
            else
              Cursor := CursorForZoom
          end ;

        TGIS_Viewer3DMode.SunPosition :
          begin
            if CursorForCameraPosition = crDefault then
              Cursor := SUNPOS_CURSOR
            else
              Cursor := CursorForSunPosition
          end ;

        TGIS_Viewer3DMode.Select :
          begin
            if CursorForSelect  = crDefault then
              Cursor := SELECT3D_CURSOR
            else
              Cursor := CursorForSelect
          end ;

        else
          begin
            Assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          end ;

      end ;
      {$IFDEF MSWINDOWS}
        Perform( Winapi.Messages.WM_SETCURSOR, Handle, HTCLIENT ) ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_ViewerWnd.gesturePanEnabled
    : Boolean ;
  begin
    if ( ( FMode = TGIS_ViewerMode.Select ) and metModeSelectAllGestures ) or
       ( ( FMode = TGIS_ViewerMode.Drag   ) and metModeDragAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Zoom   ) and metModeZoomAllGestures   ) then
      result := true
    else
      result := false ;
  end ;

  function TGIS_ViewerWnd.gestureZoomEnabled
    : Boolean ;
  begin
    if ( ( FMode = TGIS_ViewerMode.Select ) and metModeSelectAllGestures ) or
       ( ( FMode = TGIS_ViewerMode.Edit   ) and metModeEditAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Drag   ) and metModeDragAllGestures   ) or
       ( ( FMode = TGIS_ViewerMode.Zoom   ) and metModeZoomAllGestures   ) then
      result := true
    else
      result := false ;
  end ;

  function TGIS_ViewerWnd.gesture3DPanEnabled
    : Boolean ;
  begin
    if ( ( F3DMode = TGIS_Viewer3DMode.Select   ) and metViewer3DModeSelectAllGestures   ) or
       ( ( F3DMode = TGIS_Viewer3DMode.Zoom     ) and metViewer3DModeZoomAllGestures     ) then
      result := true
    else
      result := false ;
  end ;

  function TGIS_ViewerWnd.gesture3DZoomEnabled
    : Boolean ;
  begin
    if ( ( F3DMode = TGIS_Viewer3DMode.Select         ) and metViewer3DModeSelectAllGestures         ) or
       ( ( F3DMode = TGIS_Viewer3DMode.Zoom           ) and metViewer3DModeZoomAllGestures           ) or
       ( ( F3DMode = TGIS_Viewer3DMode.CameraXY       ) and metViewer3DModeCameraXYAllGestures       ) or
       ( ( F3DMode = TGIS_Viewer3DMode.CameraXYZ      ) and metViewer3DModeCameraXYZAllGestures      ) or
       ( ( F3DMode = TGIS_Viewer3DMode.CameraPosition ) and metViewer3DModeCameraPositionAllGestures ) or
       ( ( F3DMode = TGIS_Viewer3DMode.CameraRotation ) and metViewer3DModeCameraRotationAllGestures ) or
       ( ( F3DMode = TGIS_Viewer3DMode.SunPosition    ) and metViewer3DModeSunPositionAllGestures    ) then
      result := true
    else
      result := false ;
  end ;

  procedure TGIS_ViewerWnd.setGestures ;
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
    metModeEditAllGestures:= GisMetadataAsBoolean(
      METADATA_MODEEDITALLGESTURES,
      True
    ) ;

    Touch.InteractiveGestures := [] ;
    if gesturePanEnabled then
      Touch.InteractiveGestures := Touch.InteractiveGestures + [ igPan ] ;
    if gestureZoomEnabled then
      Touch.InteractiveGestures := Touch.InteractiveGestures + [ igZoom ] ;

    if FMode = TGIS_ViewerMode.Drag then
      // a special fix made for one of the clients DK-12680:
      // we enable zoom gesture, but take no action if not gestureZoomEnabled
      Touch.InteractiveGestures := Touch.InteractiveGestures + [ igZoom ] ;
  end ;

  procedure TGIS_ViewerWnd.setGestures3D ;
  begin
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

    Touch.InteractiveGestures := [] ;
    if gesture3DPanEnabled then
      Touch.InteractiveGestures := Touch.InteractiveGestures + [ igPan ] ;
    if gesture3DZoomEnabled then
      Touch.InteractiveGestures := Touch.InteractiveGestures + [ igZoom ] ;
  end ;

  procedure TGIS_ViewerWnd.doGestureProc(
          _sender  : TObject ;
    const _info    : TGestureEventInfo;
    var   _handled : Boolean
  ) ;
  var
    loc : TPoint ;

    procedure begin_mode ;
    begin
      oWndHelper.UpdateCancel ;

      resetMousePos( loc.X, loc.Y ) ;
      zoomDistance := _info.Distance ;

      oWndHelper.GestureBegin ;
      if View3D then
        FViewer3D.GestureBegin ;
    end;

    procedure do_zoom ;
    var
      dx, dy : Integer ;
      zm : Double ;
    begin
      if not oWndHelper.GestureActive then
        exit ;

      dx := 0 ;
      dy := 0 ;
      if (mousePos.X <> loc.X) or (mousePos.Y <> loc.Y) then begin
        if ( loc.X >= 0     ) and
           ( loc.Y >= 0     ) and
           ( loc.X <= ControlCanvasWidth  ) and
           ( loc.Y <= ControlCanvasHeight ) then
        begin
          dx := mousePos.X - loc.X ;
          dy := mousePos.Y - loc.Y ;
        end;
      end ;

      oWndHelper.GestureCancelLongTap ; // patch for iOS event order

      if ( mousePosOld.X <> NO_MOUSE_POS ) and
         ( mousePosOld.Y <> NO_MOUSE_POS ) then begin
        if View3D then begin
          zm := _info.Distance / zoomDistance ;
          FViewer3D.DoGesture( dx, dy, loc.X, loc.Y, zm, 0, 0, 0 ) ;
        end else begin
          if ( dx <> 0 ) or ( dy <> 0 ) then
            oWndHelper.DoDrag( dx, dy ) ;
          zm := Power( _info.Distance / zoomDistance, 2 ) ;
          if zm <> 1 then
            oWndHelper.DoZoom( loc.X, loc.Y, zm, False, True ) ;
        end ;
        zoomDistance := _info.Distance ;
      end ;
      mousePosOld.X := mousePos.X ;
      mousePosOld.Y := mousePos.Y ;
      mousePos.X := RoundS(loc.X) ;
      mousePos.Y := RoundS(loc.Y) ;
    end ;

    procedure do_drag ;
    begin
      if not oWndHelper.GestureActive then
        exit ;

      if ( loc.X < 0     ) or
         ( loc.Y < 0     ) or
         ( loc.X > ControlCanvasWidth  ) or
         ( loc.Y > ControlCanvasHeight )
      then
        exit;

      if ( zoomDistance > 0 ) and ( _info.Distance = 0 ) then
        exit ;

      if ( mousePosOld.X <> NO_MOUSE_POS ) and
         ( mousePosOld.Y <> NO_MOUSE_POS ) then begin
        if View3D then begin
          if _info.Distance = 0 then
            // drag
            FViewer3D.DoGesture( mousePos.X - loc.X, mousePos.Y - loc.Y,
                                 0, 0, 1, 0, 0, 0 )
          else
            // camera position
            FViewer3D.DoGesture( 0, 0, 0, 0, 1,
                                 loc.X - mousePos.X, loc.Y - mousePos.Y, 1 ) ;
        end else
          oWndHelper.DoDrag( mousePos.X - loc.X, mousePos.Y - loc.Y ) ;
      end ;
      mousePosOld.X := mousePos.X ;
      mousePosOld.Y := mousePos.Y ;
      mousePos.X := RoundS(loc.X) ;
      mousePos.Y := RoundS(loc.Y) ;
    end;

    procedure end_mode ;
    begin
      if oWndHelper.GestureActive then begin
        if View3D then
          FViewer3D.GestureEnd ;
        oWndHelper.GestureEnd ;
        if not View3D then begin
          oWndHelper.FitScreenAnimation ;
          oWndHelper.UpdateDelayed ;
        end;
      end ;
    end;

  begin
    _handled := False ;

    if IsEmpty then
      exit ;
    if mouseBlocked then
      exit ;

    if ( View3D     and not gesture3DPanEnabled and not gesture3DZoomEnabled ) or
       ( not View3D and not gesturePanEnabled   and not gestureZoomEnabled   ) then begin
      oWndHelper.GestureEnd( True ) ;
      exit ;
    end ;

    loc := _info.Location ;

    loc.X := RoundS( loc.X * ControlCanvasScale ) ;
    loc.Y := RoundS( loc.Y * ControlCanvasScale ) ;

    case _info.GestureID of
      igiZoom : begin
                  if ( View3D     and gesture3DZoomEnabled ) or
                     ( not View3D and gestureZoomEnabled   ) then begin
                    if TInteractiveGestureFlag.gfBegin in _info.Flags then
                      begin_mode
                    else if TInteractiveGestureFlag.gfEnd in _info.Flags then
                      end_mode
                    else
                      do_zoom ;
                  end ;
                  _handled := True ;
                end ;
      igiPan  : begin
                  if ( View3D     and gesture3DPanEnabled ) or
                     ( not View3D and gesturePanEnabled   ) then begin
                    if TInteractiveGestureFlag.gfBegin in _info.Flags then
                      begin_mode
                    else if TInteractiveGestureFlag.gfEnd in _info.Flags then
                      end_mode
                    else
                      do_drag ;
                  end ;
                  _handled := True ;
                end ;
      igiRotate       : _handled := True ;
      igiTwoFingerTap : _handled := True ;
      igiPressAndTap  : _handled := True ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.createContext ;
  begin
    bSkiaRendering := ( oRenderer.FriendlyName = 'Skia' ) ;
    TurnD2DCanvas( oRenderer is TGIS_RendererVclDirect2D  ) ;
    {$IFDEF SKIA_OPENGL_ONEWINDOW}
      if bSkiaRendering then
        createSkiaCanvas
    {$ENDIF}
  end ;

  procedure TGIS_ViewerWnd.resizeContext ;
  begin
    {$IFDEF SKIA_OPENGL_ONEWINDOW}
      if bSkiaRendering then begin
        freeSkiaCanvas( False ) ;
        createSkiaCanvas ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_ViewerWnd.destroyContext ;
  begin
    {$IFDEF SKIA_OPENGL_ONEWINDOW}
      freeSkiaCanvas( true ) ;
    {$ENDIF}
  end ;

  procedure TGIS_ViewerWnd.createSkiaCanvas ;
  begin
    try
      // regenerate connection - handle can change

      if Handle <> oldHandle then begin
        FreeObject(oOpenGLConnection );
        if Assigned( OpenGLSkiaConnectionFactory ) then
          oOpenGLConnection := OpenGLSkiaConnectionFactory.CreateConnection( Handle )
        else
          oOpenGLConnection := nil ;

        oldHandle := Handle ;
      end;

    except
      oOpenGLConnection := nil ;
    end ;

    if assigned( oOpenGLConnection ) then begin
      oOpenGLCanvas := TGIS_OpenGLSkiaConnection(oOpenGLConnection).CreateCanvas(
                         ControlCanvasWidth, ControlCanvasHeight
                       ) ;
      if not assigned( oOpenGLCanvas ) then
        // something wrong; free OpenGL connection
        FreeObject( oOpenGLConnection ) ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.freeSkiaCanvas(
    const _freeOpenGL : Boolean
  ) ;
  begin
    if assigned( oOpenGLConnection ) then
      TGIS_OpenGLSkiaConnection(oOpenGLConnection).FreeCanvas( oOpenGLCanvas ) ;
    if _freeOpenGL then
      FreeObject( oOpenGLConnection ) ;
  end ;

  procedure TGIS_ViewerWnd.doTimer(
    _sender : TObject
  ) ;
  var
    rct    : TRect ;
    zx, zy : Double ;
    dx, dy : Integer ;
  begin
    oTimer.Enabled := False ;

    if zmx.Right <> 0  then begin

      zmx.Right := 0 ;

      rct := MapToScreenRect( VisibleExtent ) ;
      zx := ( rct.Right  * dmx - rct.Right  ) / 2 ;
      zy := ( rct.Bottom * dmx - rct.Bottom ) / 2 ;
      dx := RoundS( ( mousePosOld.X - 1.0 * ControlCanvasWidth  / 2 ) *
                    zx / ControlCanvasWidth
                  ) * 2 ;
      dy := RoundS( ( mousePosOld.Y - 1.0 * ControlCanvasHeight / 2 ) *
                    zy / ControlCanvasHeight
                  ) * 2 ;

      Lock ;
      try
        Zoom := Zoom * dmx ;
        oVwr.MoveViewport( dx, dy ) ;
        oVwr.CenterPtg := GisCenterPoint( VisibleExtent ) ;
      finally
        Unlock ;
      end ;
      ControlUpdateWholeMap ;
    end
    else
      oVwr.VisibleExtent := oExtent ;
  end ;

  procedure TGIS_ViewerWnd.resetMousePos(
    const _x, _y : Integer
  ) ;
  begin
    mousePos.X    := _x ;
    mousePos.Y    := _y ;
    mousePosOld.X := NO_MOUSE_POS ;
    mousePosOld.Y := NO_MOUSE_POS ;
  end ;

  procedure TGIS_ViewerWnd.updateExecute(
    _sender : TObject ;
    _ctx    : TGIS_ViewerWndHelperRun
  ) ;
  begin
    if oVwr.Parent as TObject <> self then
      exit ;

    if ( ControlCanvasWidth < 5 ) or ( ControlCanvasHeight < 5 )  then
      exit ;

    if View3D then begin
      Viewer3D.UpdateWholeMap ;
      exit ;
    end;

    if TiledPaint then begin //TILER
      oWndHelper.SetVisibleExtent ;
      oWndHelper.Reset( VisibleExtent, ControlCanvasWidth, ControlCanvasHeight ) ;
      ControlRepaint;
      NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, self ) ;
      exit ;
    end ;

    if bInUpdate then exit ;
    bInUpdate := True ;

    mouseBlocked := True ;
    oWndHelper.SetVisibleExtent ;

    HourglassPrepare ;

    FTileContext := nil ;
    FDrawContext.Clear ;
    FDrawContext.AssignDrawContext( oD2DCanvas, nil, nil ) ;

    FreeObject( FProgressBitmap ) ;

    try
      _ctx.Prepare( ControlCanvasWidth, ControlCanvasHeight, VisibleExtent ) ;

      FDrawContext.ProgressiveHelper := _ctx ;

      // Map
      FDrawContext.AssignBaseMap  ( nil, True ) ;

      if oVwr.IsEmpty then exit ;

      // Selection
      FDrawContext.AssignSelection( nil, True ) ;

      // Charts
      FDrawContext.AssignCharts   ( nil, True ) ;

      // Labels
      FDrawContext.AssignLabels   ( nil, True ) ;

      oRenderer.ReleaseContext ;
      oRenderer.CreateContext( self, oVwr, FDrawContext, Point(0, 0),
                               ControlCanvasWidth, ControlCanvasHeight,
                               PPI, FontScale ) ;
      try
        oVwr.LabelsReg.Reset ;
        oRenderer.PrepareDraw ;
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
      FreeObject( FProgressBitmap ) ;
      bInUpdate := False ;
      if not _ctx.MustBreak then begin
        _ctx.Bitmap       := FDrawContext.BaseMap ;
        _ctx.DoSynchronize( True ) ;
      end ;
      mouseBlocked := False ;
      HourglassRelease ;
      NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, self ) ;
    end ;

  end ;

  procedure TGIS_ViewerWnd.updateSynchronize(
    _sender : TObject ;
    _ctx    : TGIS_ViewerWndHelperRun ;
    _final  : Boolean
  ) ;
  begin
    if _final then begin
      FreeObject( FCacheBitmap ) ;

      if assigned( _ctx.Bitmap ) then
        FCacheBitmap := oRenderer.ViewerPrepareCache( TGIS_Bitmap( _ctx.Bitmap ) )
      else
        FCacheBitmap := TGIS_Bitmap.Create;

      oWndHelper.Reset( _ctx ) ;
      bCacheBitmapNew := True ;

      ControlRepaint ;
    end
    else begin
      oRenderer.Flush ;
      FreeObject( FProgressBitmap ) ;

      FProgressBitmap := oRenderer.ViewerPrepareCache( TGIS_Bitmap( FDrawContext.BaseMap ) ) ;

      ControlRepaint ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.updateTap(
    _sender : TObject
  ) ;
  var
    btn_state   : TMouseButton ;
    shift_state : TShiftState  ;
  begin
    btn_state   := TMouseButton.mbLeft ;
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
           end;
    end ;
  end ;

  procedure TGIS_ViewerWnd.doGraticule(
    _sender   : TObject ;
    _renderer : TGIS_RendererAbstract ;
    _mode     : TGIS_DrawMode
  ) ;
  begin
    oGraticuleHelper.Draw(
      _renderer,
      Width, Height,
      oWndHelper.OriginalRect,
      oWndHelper.ScaledRect( 1 ),
      oWndHelper.ActualExtent
    ) ;
  end ;

  procedure TGIS_ViewerWnd.doPrintBmp(
    const _bitmap : TGIS_Bitmap ;
    const _full   : Boolean
  ) ;
  var
    bmp  : TGIS_Bitmap ;
    w, h : Integer ;
    scl  : Double ;
    wextent : TGIS_Extent ;

    procedure prepare_map(
      _ext : TGIS_Extent ;
      _scl : Double
    ) ;
    var
      ctx       : TGIS_RendererContext ;
      bincrmntl : Boolean ;
      basemap_store : array of Boolean ;
      cnv  : TObject ;

      procedure store_basemap ;
      var
        i : Integer ;
      begin
        if not assigned( oVwr ) then exit ;

        SetLength( basemap_store, oVwr.Items.Count ) ;
        for i := 0 to oVwr.Items.Count - 1 do begin
          TGIS_Layer(oVwr.Items[i]).Lock ;
          try
            basemap_store[i] := TGIS_Layer(oVwr.Items[i]).Basemap ;
            if basemap_store[i] then begin
              TGIS_Layer(oVwr.Items[i]).Basemap := False ;
            end;
          finally
            TGIS_Layer(oVwr.Items[i]).UnLock ;
          end;
        end;
      end;

      procedure restore_basemap ;
      var
        i : Integer ;
      begin
        if not assigned( oVwr ) then exit ;
        if not assigned( basemap_store ) then exit ;
        for i := 0 to oVwr.Items.Count - 1 do begin
          TGIS_Layer(oVwr.Items[i]).Lock ;
          try
            if basemap_store[i] then
              TGIS_Layer(oVwr.Items[i]).Basemap := basemap_store[i] ;
          finally
            TGIS_Layer(oVwr.Items[i]).UnLock ;
          end;
        end;
        oVwr.UnLock ;
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
        ctx.AssignSelection( nil, True ) ;

        // Charts
        ctx.AssignCharts   ( nil, True ) ;

        // Labels
        ctx.AssignLabels   ( nil, True ) ;

        store_basemap ;

        // paint layers
        oRenderer.ReleaseContext ;

        if _full then begin
          oRenderer.PaintExtra( Self, bmp, FOnBeforePaintRenderer ) ;
          oRenderer.PaintExtra( Self, bmp, FOnBeforePaint ) ;
        end ;

        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                                 w, h, PPI, FontScale
                               ) ;

        oVwr.LabelsReg.Reset ;
        oRenderer.PrepareDraw ;
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

          cnv := oRenderer.ViewerCreateTemporaryPaint( TGIS_Bitmap(ctx.BaseMap) ) ;
          try
            if Assigned( ctx.Selection ) then
              oRenderer.ViewerBlendBitmaps(
                ctx.Selection,
                cnv,
                oVwr.SelectionTransparency,
                False
              ) ;
            if Assigned( ctx.Charts ) then
              oRenderer.ViewerBlendBitmaps(
                ctx.Charts,
                cnv,
                100,
                False
              ) ;
            if Assigned( ctx.Labels ) then
              oRenderer.ViewerBlendBitmaps(
                ctx.Labels,
                cnv,
                100,
                False
              ) ;
          finally
            oRenderer.ViewerFreeTemporaryPaint( cnv, TGIS_Bitmap(ctx.BaseMap) ) ;
            oRenderer.ReleaseContext ;
          end ;
        end ;

        if _full then begin
          oRenderer.PaintExtra( Self, bmp, FOnPaintExtra ) ;
          oRenderer.PaintExtra( Self, bmp, FOnAfterPaint ) ;
          oRenderer.PaintExtra( Self, bmp, FOnAfterPaintRenderer ) ;
        end ;

      finally
        FreeObject( ctx );
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
    bmp.Clear( oVwr.Color ) ;

    wextent := VisibleExtent ;
    try
      scl := 0 ;
      prepare_map( VisibleExtent, scl ) ;
    finally
      VisibleExtent := wextent ;
    end ;
  end ;


//==============================================================================
// properties access functions of IGIS_Viewer
//==============================================================================

  function TGIS_ViewerWnd.fget_AutoStyle
    : Boolean ;
  begin
    Result := oVwr.AutoStyle ;
  end ;

  procedure TGIS_ViewerWnd.fset_AutoStyle(
    const _value : Boolean
  ) ;
  begin
    oVwr.AutoStyle := _value;
  end ;

  function TGIS_ViewerWnd.fget_BigExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.BigExtent ;
  end ;

  function TGIS_ViewerWnd.fget_BigExtentMargin
    : Integer ;
  begin
    Result := oVwr.BigExtentMargin ;
  end ;

  procedure TGIS_ViewerWnd.fset_BigExtentMargin(
    const _value : Integer
  ) ;
  begin
    oVwr.BigExtentMargin := _value ;
  end ;

  function TGIS_ViewerWnd.fget_KeepScale : Boolean ;
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
  end ;

  function TGIS_ViewerWnd.fget_BusyText
    : String ;
  begin
    Result := oVwr.BusyText ;
  end ;

  function TGIS_ViewerWnd.fget_Center
    : TGIS_Point  ;
  begin
    Result := oVwr.Center ;
  end ;

  procedure TGIS_ViewerWnd.fset_Center(
    const _value : TGIS_Point
  ) ;
  begin
    oWndHelper.UpdateCancel ;
    oVwr.Center := _value ;
  end ;

  function TGIS_ViewerWnd.fget_CenterPtg
    : TGIS_Point  ;
  begin
    Result := oVwr.CenterPtg ;
  end ;

  procedure TGIS_ViewerWnd.fset_CenterPtg(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.CenterPtg := _value ;
  end ;

  function TGIS_ViewerWnd.fget_Color
    : TGIS_Color ;
  begin
    Result := oVwr.Color ;
  end ;

  procedure TGIS_ViewerWnd.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    oVwr.Color := _value ;
  end ;

  function TGIS_ViewerWnd.fget_Copyright
    : String ;
  begin
    Result := oVwr.Copyright ;
  end ;

  function TGIS_ViewerWnd.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    Result := oVwr.CS ;
  end ;

  procedure TGIS_ViewerWnd.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    oVwr.CS := _value ;
  end ;

  procedure TGIS_ViewerWnd.fset_CustomPPI(
    const _value : Integer
  ) ;
  begin
    oVwr.CustomPPI := _value ;
  end ;

  function TGIS_ViewerWnd.fget_CustomPPI
    : Integer ;
  begin
    Result := oVwr.CustomPPI ;
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
  end ;

  function TGIS_ViewerWnd.fget_FileCopyrights
    : String      ;
  begin
    Result := oVwr.FileCopyrights ;
  end ;

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
  end ;

  function TGIS_ViewerWnd.fget_IncrementalPaint
    : Boolean ;
  begin
    Result := oVwr.IncrementalPaint ;
  end ;

  procedure TGIS_ViewerWnd.fset_IncrementalPaint(
    const _value : Boolean
  ) ;
  begin
    oVwr.IncrementalPaint := _value ;
  end ;

  //TILER
  function TGIS_ViewerWnd.fget_TiledPaint
    : Boolean ;
  begin
    Result := oVwr.TiledPaint ;
  end ;

  //TILER
  procedure TGIS_ViewerWnd.fset_TiledPaint(
    const _value : Boolean
  ) ;
  var
    factory : T_ViewerBmpFactory ;
  begin
    if _value = oVwr.TiledPaint then
      exit;

    if _value then begin
      factory := T_ViewerBmpFactory.Create ;
      try
        oTilerHelper := TGIS_TilerHelper.Create(
                          self,
                          factory,
                          4
                        ) ;
        oTilerExtent := GisNoWorld ;
        oTilerState := TGIS_TilerState.Repaint ;
      finally
        FreeObject( factory ) ;
      end;
    end
    else begin
      FreeObject( oTilerHelper ) ;
    end;

    oVwr.TiledPaint := _value ;

    InvalidateWholeMap ;
  end ;

  function TGIS_ViewerWnd.fget_InPaint
    : Boolean ;
  begin
    Result := oVwr.InPaint ;
  end ;

  function TGIS_ViewerWnd.fget_IsBusy
    : Boolean ;
  begin
    Result := oVwr.IsBusy ;
  end ;

  function TGIS_ViewerWnd.fget_IsEmpty
    : Boolean ;
  begin
    Result := oVwr.IsEmpty ;
  end ;

  function TGIS_ViewerWnd.fget_IsLocked
   : Boolean ;
  begin
    Result := oVwr.IsLocked ;
  end ;

  function TGIS_ViewerWnd.fget_IsTopmost
    : Boolean ;
  begin
    Result := oVwr.IsTopmost ;
  end ;

  function TGIS_ViewerWnd.fget_Items
    : TGIS_LayerAbstractList ;
  begin
    Result := oVwr.Items ;
  end ;

  function TGIS_ViewerWnd.fget_LabelsReg
    : TGIS_LabelsAreaAbstract ;
  begin
    Result := oVwr.LabelsReg ;
  end ;

  function TGIS_ViewerWnd.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    Result := oVwr.MultiUserMode ;
  end ;

  function TGIS_ViewerWnd.fget_CustomData
    : TGIS_StringList ;
  begin
    Result := oVwr.CustomData ;
  end;

  procedure TGIS_ViewerWnd.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    oVwr.MultiUserMode := _value ;
  end ;

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
  end ;

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
  end ;

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
  end ;

  procedure TGIS_ViewerWnd.fset_RestrictedDrag(
    const _value : Boolean
  ) ;
  begin
    oVwr.RestrictedDrag := _value ;
  end ;

  function TGIS_ViewerWnd.fget_RestrictedExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.RestrictedExtent ;
  end ;

  procedure TGIS_ViewerWnd.fset_RestrictedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.RestrictedExtent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_RotationAngle
    : Double ;
  begin
    Result := oVwr.RotationAngle ;
  end ;

  procedure TGIS_ViewerWnd.fset_RotationAngle(
    const _value : Double
  ) ;
  begin
    oVwr.RotationAngle := _value ;
  end ;

  function TGIS_ViewerWnd.fget_RotationPoint
    : TGIS_Point ;
  begin
    Result := oVwr.RotationPoint ;
  end ;

  procedure TGIS_ViewerWnd.fset_RotationPoint(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.RotationPoint := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ScaleAsFloat
    : Double ;
  begin
    Result := oVwr.ScaleAsFloat ;
  end ;

  procedure TGIS_ViewerWnd.fset_ScaleAsFloat(
    const _value : Double
  ) ;
  begin
    oVwr.ScaleAsFloat := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ScaleAsText
    : String ;
  begin
    Result := oVwr.ScaleAsText ;
  end ;

  procedure TGIS_ViewerWnd.fset_ScaleAsText(
    const _value : String
  ) ;
  begin
    oVwr.ScaleAsText := _value ;
  end ;

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
  end ;

  procedure TGIS_ViewerWnd.fset_SelectionGisColor(
    const _value : TGIS_Color
  ) ;
  begin
    oVwr.SelectionGisColor := _value ;
  end ;

  function TGIS_ViewerWnd.fget_SelectionOutlineOnly
    : Boolean ;
  begin
    Result := oVwr.SelectionOutlineOnly ;
  end ;

  procedure TGIS_ViewerWnd.fset_SelectionOutlineOnly(
    const _value : Boolean
  ) ;
  begin
    oVwr.SelectionOutlineOnly := _value ;
  end ;

  function TGIS_ViewerWnd.fget_SelectionTransparency
    : Integer ;
  begin
    Result := oVwr.SelectionTransparency ;
  end ;

  procedure TGIS_ViewerWnd.fset_SelectionTransparency(
    const _value : Integer
  ) ;
  begin
    oVwr.SelectionTransparency := _value ;
  end ;

  function TGIS_ViewerWnd.fget_SelectionWidth
    : Integer ;
  begin
    Result := oVwr.SelectionWidth ;
  end ;

  procedure TGIS_ViewerWnd.fset_SelectionWidth(
    const _value : Integer
  ) ;
  begin
    oVwr.SelectionWidth := _value ;
  end ;

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

  function TGIS_ViewerWnd.fget_ViewerParentRoot
    : IGIS_ViewerParent ;
  begin
    Result := oVwr.ViewerParentRoot ;
  end;

  function TGIS_ViewerWnd.fget_Viewport
    : TGIS_Point ;
  begin
    Result := oVwr.Viewport ;
  end ;

  procedure TGIS_ViewerWnd.fset_Viewport(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.Viewport := _value ;
  end ;

  function TGIS_ViewerWnd.fget_VisibleExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.VisibleExtent ;
  end ;

  procedure TGIS_ViewerWnd.fset_VisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.VisibleExtent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_UseAnimations
    : Boolean ;
  begin
    Result := oVwr.UseAnimations ;
  end ;

  procedure TGIS_ViewerWnd.fset_UseAnimations(
    const _value : Boolean
  ) ;
  begin
    oVwr.UseAnimations := _value ;
  end ;

  function TGIS_ViewerWnd.fget_UseRTree
    : Boolean ;
  begin
    Result := oVwr.UseRTree ;
  end ;

  procedure TGIS_ViewerWnd.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    oVwr.UseRTree := _value ;
  end ;

  function TGIS_ViewerWnd.fget_Zoom
    : Double ;
  begin
    Result := oVwr.Zoom ;
  end ;

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

    oWndHelper.DoZoom( pt.X, pt.Y, _value / zm, True, False ) ;

    oVwr.Zoom := _value ;
  end ;

  function TGIS_ViewerWnd.fget_ZoomEx
    : Double ;
  begin
    Result := oVwr.ZoomEx ;
  end ;

  procedure TGIS_ViewerWnd.fset_ZoomEx(
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

    zm := oVwr.ZoomEx ;
    pt := MapToScreen( CenterPtg ) ;

    oWndHelper.DoZoom( pt.X, pt.Y, _value / zm, True, False ) ;

    oVwr.ZoomEx := _value ;
  end ;

  function TGIS_ViewerWnd.fget_MasterViewer
    : IGIS_Viewer ;
  begin
    Result := oVwr.MasterViewer ;
  end;

  procedure TGIS_ViewerWnd.fset_MasterViewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    oVwr.MasterViewer := _value ;
  end;

  function TGIS_ViewerWnd.fget_UponDestroy
    : Boolean ;
  begin
    Result := oVwr.UponDestroy ;
  end;

  function  TGIS_ViewerWnd.fget_TemporaryScaleInternal : Double ;
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

  procedure TGIS_ViewerWnd.fset_Mode(
    const _value : TGIS_ViewerMode
  ) ;
  var
    edt_change : Boolean ;
  begin
    edt_change := FMode = TGIS_ViewerMode.Edit ;

    if FMode = _value then exit ;


    FMode := _value ;
    ensureCursor ;
    FDragging := false ;
    FZooming := false ;
    FEditing := false ;
    setGestures ;

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
  end ;

  procedure TGIS_ViewerWnd.fset_3DMode(
    const _value : TGIS_Viewer3DMode
  ) ;
  begin
    F3DMode := _value ;
    ensure3DCursor ;
    setGestures3D ;

    { TODO : Verify if it's still needed }
    //if assigned( FOnModeChange ) then
    //  FOnModeChange( self ) ;
  end ;

  procedure TGIS_ViewerWnd.fset_Renderer(
    const _value : TGIS_RendererAbstract
  ) ;
  begin
    Assert( Assigned( _value ) ) ;

    oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;
    FreeObject( oRenderer ) ;
    oRenderer := _value ;

    FreeObject( FCacheBitmap ) ;
    FreeObject( FProgressBitmap ) ;
    oPaintCache := nil ;

    destroyContext ;
    createContext ;
    ControlUpdateWholeMap ;
  end ;

//==============================================================================
// reintroduced properties
//==============================================================================

  function TGIS_ViewerWnd.fget_ColorEx
    : TColor ;
  begin
    Result := FBackgroundColor ;
  end ;

  procedure TGIS_ViewerWnd.fset_ColorEx(
    const _value : TColor
  ) ;
  begin
    inherited Color := _value ;

    oVwr.Color := GISColor( _value ) ;

    if _value <> FBackgroundColor then begin
      FBackgroundColor := _value ;
      Update ;
    end ;
  end ;

  function TGIS_ViewerWnd.fget_SelectionColor
    : TColor ;
  begin
    Result := VCLColor( oVwr.SelectionGisColor ) ;
  end ;

  procedure TGIS_ViewerWnd.fset_SelectionColor(
    const _value : TColor
  ) ;
  begin
    oVwr.SelectionGisColor := GISColor( _value ) ;
  end ;


//==============================================================================
// New properties
//==============================================================================

  procedure TGIS_ViewerWnd.fset_BorderStyle(
    const _value : TBorderStyle
  ) ;
  begin
    if FBorderStyle <> _value then begin
      FBorderStyle := _value ;
      RecreateWnd ;
    end;
  end ;


//==============================================================================
// property access routines of TGIS_Viewer's event handlers
//==============================================================================

  function TGIS_ViewerWnd.fget_BusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := oVwr.BusyEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    oVwr.BusyEvent := _value ;
  end ;

  function TGIS_ViewerWnd.fget_HelpEvent
    : TGIS_HelpEvent ;
  begin
    Result := oVwr.HelpEvent ;
  end ;

  procedure TGIS_ViewerWnd.fset_HelpEvent(
    const _value : TGIS_HelpEvent
  ) ;
  begin
    oVwr.HelpEvent := _value ;
  end ;

  {$IFDEF GIS_XDK}
    function TGIS_ViewerWnd.fget_UseHelp
      : Boolean ;
    begin
      Result := oVwr.UseHelp ;
    end ;

    procedure TGIS_ViewerWnd.fset_UseHelp(
      const _value : Boolean
    ) ;
    begin
      oVwr.UseHelp := _value ;
    end ;
  {$ENDIF}

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

  function TGIS_ViewerWnd.fget_View3D : Boolean ;
  begin
    Result := Assigned( FViewer3D ) ;
  end;

  procedure TGIS_ViewerWnd.fset_View3D( const _value : Boolean ) ;
  var
    ext : TGIS_Extent ;
    obj : TGIS_Viewer3DBase ;
  begin
    if _value and Assigned( FViewer3D ) then exit ;
    if (not _value) and (not Assigned( FViewer3D )) then exit ;
    if IsEmpty then exit ;

    if _value then begin
      if not SynchronizePaint(False) then exit ;

      oldMode2D := Mode ;
      if oRenderer.ClassName = 'TGIS_RendererVclSkia' then begin
        oldRenderer2D := oRenderer.ClassName ;
        Renderer := RendererManager.CreateInstance( 'TGIS_RendererVclDirect2D' );
      end
      else
        oldRenderer2D := '' ;
      FViewer3D := TGIS_Viewer3DDirectX.Create( self.Handle, Self,
                     GisCommonExtent( self.VisibleExtent, Extent ),
                     not self.IsLocked
                   ) ;
      setGestures3D ;
      HourglassPrepare ;

      if not FViewer3D.InitialRedraw(
               GisCommonExtent( self.VisibleExtent, Extent ),
               not self.IsLocked ) then begin
        HourglassRelease ;
        obj := TGIS_Viewer3DBase( FViewer3D ) ;
        obj.Free ;
        FViewer3D := nil ;
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ), 'InitD3D', 1 ) ;
        exit;
      end;

      DoubleBuffered := False ;

      HourglassRelease ;
      NotifySubscribers( GIS_SUBSCRIBED_3D_UPDATE, nil );

    end
    else begin
      if TGIS_Viewer3DBase( FViewer3D ).IsBusy then exit ;

      ext := GisExtent( FViewer3D.VisibleExtent3D.XMin,
                        FViewer3D.VisibleExtent3D.YMin,
                        FViewer3D.VisibleExtent3D.XMax,
                        FViewer3D.VisibleExtent3D.YMax
                      ) ;

      obj := TGIS_Viewer3DBase( FViewer3D ) ;
      FViewer3D := nil ;
      obj.Free ;

      if not IsStringEmpty( oldRenderer2D ) then
        Renderer := RendererManager.CreateInstance( oldRenderer2D );

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

  function  TGIS_ViewerWnd.fget_Viewer3D
    : IGIS_Viewer3D ;
  begin
    Result := FViewer3D ;
  end ;

  procedure TGIS_ViewerWnd.fset_Viewer3D(
    const _viewer : IGIS_Viewer3D
  ) ;
  begin
    FViewer3D := _viewer ;
  end ;

//==============================================================================
// D2DCanvas handling
//==============================================================================

  procedure TGIS_ViewerWnd.WMPaint(
    var _msg: TWMPaint
  );
  var
    paintstruct: TPaintStruct;

    procedure draw_dragging_track(
      _x1, _y1  : Integer ;
      _x2, _y2 : Integer
    ) ;
    var
      old_penmode : TPenMode ;
    begin
      Canvas.Brush.Color := clBlack ;
      Canvas.Pen.Width   := 1 ;
      Canvas.Pen.Style   := psDot ;
      Canvas.Pen.Color   := zoomingColor ;
      old_penmode := Canvas.Pen.Mode ;
      try
        Canvas.Pen.Mode  := pmNotXor ;
        Canvas.Pen.Width := 1 ;
        Canvas.MoveTo( _x1, _y1 ) ;
        Canvas.LineTo( _x2, _y2 ) ;
      finally
        Canvas.Pen.Mode := old_penmode ;
      end ;
    end ;

  begin
    {$IFDEF GIS_PDK}
      if bItIsProxy then begin
        exit ;
      end;
    {$ENDIF}

    if bD2DCanvas and ( not View3D ) then begin
      DoubleBuffered := False ;
      BeginPaint(Handle, paintstruct);
      try
        oD2DCanvas.BeginDraw;
        try
          Paint;
        finally
          oD2DCanvas.EndDraw;
        end;
      finally
        EndPaint(Handle, paintstruct);
      end;
     _msg.Result := 1 ;
    end
    else
    if bSkiaRendering and ( not View3D ) then begin
      DoubleBuffered := False ;
      inherited ;
    end
    else begin
      if ( View3D ) then
        DoubleBuffered := False
      else
        DoubleBuffered := True ;
      inherited ;
    end ;

    (*if FEditing then begin
      if Editor.ShowDraggingTrack then begin
        // hide any previous line
        if mousePosOld.X <> NO_MOUSE_POS then begin
          draw_dragging_track( mousePosOld.X, mousePosOld.Y,
                               mousePos.X   , mousePos.Y
                             ) ;
        end ;
      end ;
    end ;*)

//    if ( BorderStyle = bsSingle ) and ThemeControl(Self) then
//      StyleServices.PaintBorder(Self, False);
  end;

  procedure TGIS_ViewerWnd.WMEraseBkGnd(
    var _msg: TWMPaint
  );
  begin
    if csDesigning in ComponentState then begin
      inherited
    end
    else begin
      // do nothing
    end ;

    _msg.Result := 1 ;
  end;

  procedure TGIS_ViewerWnd.WMSize(
    var _msg : TWMSize
  ) ;
  var
    sz : TD2D1SizeU ;
  begin
    if oD2DCanvas <> nil then begin
      sz := D2D1SizeU(ClientWidth, ClientHeight);
      ID2D1HwndRenderTarget( oD2DCanvas.RenderTarget ).Resize(sz);
    end;

    inherited;
  end;

  procedure TGIS_ViewerWnd.CMCtl3DChanged(
    var _msg : TMessage
  );
  begin
    RecreateWnd;
  end;

  procedure TGIS_ViewerWnd.TurnD2DCanvas(
    const _value : Boolean
  );
  begin
    if _value = bD2DCanvas then
      exit;

    if not _value then begin
      bD2DCanvas := false;
    end
    else begin
      bD2DCanvas := oD2DCanvas <> nil;
    end;
  end;

  function TGIS_ViewerWnd.CreateD2DCanvas
    : Boolean;
  var
    scl : TD2DSizeF ;
  begin
    Result := False ;

    try
      FreeObject( oD2DCanvas ) ;

      {$IFDEF GIS_PDK}
        if bItIsProxy then begin
          exit ;
        end;
      {$ENDIF}

      if isWine then
        exit ;

      {$IFNDEF GIS_NODIRECT2D}
        oD2DCanvas := CreateHwndD2DCanvas( Handle ) ;
        oD2DCanvas.RenderTarget ;

        // non standard PPI setup
        scl.width  := 96 / Screen.PixelsPerInch ;
        scl.height := 96 / Screen.PixelsPerInch ;

        oD2DCanvas.RenderTarget.SetTransform(
          TD2DMatrix3X2F.Scale( scl, Point( 0, 0 ) )
        );

        Result := True ;
      {$ENDIF}
    except
      { Failed creating the D2D canvas. }
    end;
  end;

  procedure TGIS_ViewerWnd.CreateWnd;
  begin
    inherited;

    metPaintFullDirect2D := GisMetadataAsBoolean(
      METADATA_PAINTFULLDIRECT2D,
      true
    ) ;
    if (Win32MajorVersion >= 6) and (Win32Platform = VER_PLATFORM_WIN32_NT) and
       not ( csDesigning in ComponentState )
    then
      bD2DCanvas := CreateD2DCanvas
    else
      bD2DCanvas := false;

    bD2DCanvas := bD2DCanvas and ( oRenderer is TGIS_RendererVclDirect2D ) ;

    if csDesigning in ComponentState then
      bD2DCanvas := False ;
    bD2DRendering := metPaintFullDirect2D ;

    if bD2DCanvas and bD2DRendering then
      // case when any content has been already rendered
      FreeObject( FCacheBitmap ) ;
  end;

//==============================================================================
// overridden methods
//==============================================================================

  procedure TGIS_ViewerWnd.CreateParams(
    var _params : TCreateParams
  ) ;
  begin
    inherited CreateParams( _params ) ;

    with _params do begin
      Style := Style or WS_TABSTOP ;

      if FBorderStyle = bsSingle then
        if NewStyleControls and Ctl3D then begin
          Style := Style and not WS_BORDER;
          ExStyle := ExStyle or WS_EX_CLIENTEDGE;
        end
        else
          Style := Style or WS_BORDER;
    end;

    if {$IFDEF LEVEL_RX11_VCL}
         IsCustomStyleActive
       {$ELSE}
         TStyleManager.IsCustomStyleActive
       {$ENDIF}
       and
       ( BorderStyle = bsSingle )
       and
       ( seBorder in StyleElements )
    then
      ControlStyle := ControlStyle + [csNeedsBorderPaint]
    else
      ControlStyle := ControlStyle - [csNeedsBorderPaint] ;
  end ;

  procedure TGIS_ViewerWnd.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer       ;
    _y      : Integer
  ) ;
  begin
    {$IFDEF GIS_PDK}
      if bItIsProxy then begin
        inherited ;
        exit ;
      end;
    {$ENDIF}

    if (not TiledPaint) and InPaint then begin //TILER
      oWndHelper.UpdateCancelOptional ;
      exit ;
    end;

    if IsEmpty then
      exit ;
    if mouseBlocked then
      exit ;
    if IsBusy then
      exit ;

    if oWndHelper.GestureActive then
      exit ;

    inherited ;

    ensureCursor ;

    if (not TiledPaint) and oWndHelper.UpdateCancelOptional then //TILER
      exit ;

    if View3D then begin
      if FViewer3D.Mode = TGIS_Viewer3DMode.Select then
        oWndHelper.GestureMouseDown(
          ssShift  in _shift,
          ssAlt    in _shift,
          ssCtrl   in _shift,
          ssLeft   in _shift,
          ssRight  in _shift,
          ssMiddle in _shift,
          ssTouch  in _shift,
          ssPen    in _shift,
          _x,
          _y
        )
      else begin
        if FModeMouseButton = _button then
          FViewer3D.DoMouseDown( _x, _y ) ;
      end ;

      exit ;
    end ;

    if IsBusy  then exit ;

    oWndHelper.GestureMouseDown(
      ssShift  in _shift,
      ssAlt    in _shift,
      ssCtrl   in _shift,
      ssLeft   in _shift,
      ssRight  in _shift,
      ssMiddle in _shift,
      ssTouch  in _shift,
      ssPen    in _shift,
      _x,
      _y
    );

    if FModeMouseButton <> _button then exit ;

    case FMode of
      TGIS_ViewerMode.Select: // beginning of select mode
        begin
        end;

      TGIS_ViewerMode.Drag:   // beginning of dragging mode
        begin
          FDragging := True;
          resetMousePos( _x, _y ) ;
        end;

      TGIS_ViewerMode.Zoom:   // beginning of rectangle zoom mode
        begin
          FZooming := True;
          resetMousePos( _x, _y ) ;
        end;

      TGIS_ViewerMode.ZoomEx: // beginning of rubber zooming mode
        begin
          FZooming := True;
          resetMousePos( _x, _y ) ;
        end;

      TGIS_ViewerMode.Edit:   // beginning of editing mode
        begin
          if ( oWndHelper.GestureState.DownCount = 1 ) or
             ( ssTouch in _shift ) or ( ssPen in _shift ) then begin
            FEditing := True;
            resetMousePos( _x, _y ) ;
            if ssTouch in _shift then
              Editor.PointerMode := TGIS_PointerMode.Touch
            else if ssPen in _shift then
              Editor.PointerMode := TGIS_PointerMode.Pen
            else
              Editor.PointerMode := TGIS_PointerMode.Mouse ;
            Editor.MouseBegin( Point( _x, _y ), not ( ssCtrl in _shift ) ) ;
          end;
        end;
    end ;
  end ;

  procedure TGIS_ViewerWnd.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    {$IFDEF GIS_PDK}
      if bItIsProxy then begin
        inherited ;
        exit ;
      end;
    {$ENDIF}

    if IsEmpty then
      exit ;
    if IsBusy  then
      exit ;
    if mouseBlocked then
      exit ;

    ensureCursor ;

    try
      if View3D then begin
        if FViewer3D.Mode = TGIS_Viewer3DMode.Select then begin
          if oWndHelper.GestureState.GestureCnt > 0 then
            exit ; // on iOS zoom gesture causes MouseUp

          oWndHelper.GestureMouseUp(
            ssShift  in _shift,
            ssAlt    in _shift,
            ssCtrl   in _shift,
            ssLeft   in _shift,
            ssRight  in _shift,
            ssMiddle in _shift,
            ssTouch  in _shift,
            ssPen    in _shift,
            _x,
            _y
          );
        end
        else begin
          if FModeMouseButton = _button then
            FViewer3D.DoMouseUp() ;
        end ;

        exit ;
      end ;

      if FModeMouseButton = _button then begin
        case FMode of
          TGIS_ViewerMode.Select: // end of select mode
            begin
              if FAutoCenter then
                CenterViewport(ScreenToMap(Point(_x, _y)));

            end ;

          TGIS_ViewerMode.Drag:   // end of dragging mode
            begin
              if not FDragging then
                exit;

              oWndHelper.FitScreenAnimation ;
              oWndHelper.UpdateDelayed ;

              FDragging := False;
            end ;

          TGIS_ViewerMode.Zoom:   // end of rectangle zoom mode
            begin
              if not FZooming then
                exit;

              if mousePosOld.X <> NO_MOUSE_POS then
              begin
                oZoomingRect.Visible := false;
                oWndHelper.DoZoom( mousePos.X    ,
                                   mousePos.Y    ,
                                   mousePosOld.X ,
                                   mousePosOld.Y
                                 ) ;
                oWndHelper.UpdateDelayed ;
              end ;

              FZooming := False;
            end ;

          TGIS_ViewerMode.ZoomEx: // end of rubber zooming mode
            begin
              if not FZooming then
                exit;

              oWndHelper.FitScreenAnimation ;
              oWndHelper.UpdateDelayed ;

              FZooming := False;
            end ;

          TGIS_ViewerMode.Edit: // end of editing mode
            begin
              if not FEditing then exit ;

              if Editor.ShowDraggingTrack then begin
                // hide any previous line
                if mousePosOld.X <> NO_MOUSE_POS then begin
                  oDraggingTrack.Visible := False;
                  ControlRepaint;
                end ;
              end ;
              Editor.MouseEnd( Point(_x, _y) ) ;

              FEditing := False ;
            end ;
        end ;
      end;

      oWndHelper.GestureMouseUp(
        ssShift  in _shift,
        ssAlt    in _shift,
        ssCtrl   in _shift,
        ssLeft   in _shift,
        ssRight  in _shift,
        ssMiddle in _shift,
        ssTouch  in _shift,
        ssPen    in _shift,
        _x,
        _y
      ) ;

      oWndHelper.GestureEnd( True ) ; // end of gesture for sure

    finally
      inherited ;
    end;
  end ;

  procedure TGIS_ViewerWnd.MouseMove(
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  var
    x, y : Integer ;
    zm   : Double  ;

    procedure draw_zooming_rect(
       _x1, _y1 : Integer ;
       _x2, _y2 : Integer
    ) ;
    begin
      if IsEmpty then exit ;

      if _x2 < 0 then begin
        _x1 := _x1 + _x2;
        _x2 := Abs(_x2);
      end;

      if _y2 < 0 then begin
        _y1 := _y1 + _y2;
        _y2 := Abs(_y2);
      end;

      oZoomingRect.X := _x1 ;
      oZoomingRect.Y := _y1 ;
      oZoomingRect.Width  := _x2 ;
      oZoomingRect.Height := _y2 ;
      oZoomingRect.Color := TGIS_Color.FromHSL(GetRandom(100) / 100, 1, 0.5);
      oZoomingRect.Visible := True;
      ControlRepaint
    end;

    procedure draw_dragging_track(
       _x1, _y1 : Integer ;   // x, y
       _x2, _y2 : Integer     // start pos
    ) ;
    begin
      if IsEmpty then exit ;

      oDraggingTrack.X1 := _x1;
      oDraggingTrack.Y1 := _y1;
      oDraggingTrack.X2 := _x2;
      oDraggingTrack.Y2 := _y2;

      oDraggingTrack.Color := TGIS_Color.FromHSL(GetRandom(100) / 100, 1, 0.5) ;

      oDraggingTrack.Visible := True;
      ControlRepaint;
    end;

  begin
    {$IFDEF GIS_PDK}
      if bItIsProxy then begin
        inherited ;
        exit ;
      end;
    {$ENDIF}

    ensureCursor ;

    if ( mousePosLast.X = _x )
       and
       ( mousePosLast.Y = _y )
    then
      exit ;

    mousePosLast.X := _x ;
    mousePosLast.Y := _y ;

    oWndHelper.GestureMouseMove(
      ssShift  in _shift,
      ssAlt    in _shift,
      ssCtrl   in _shift,
      ssLeft   in _shift,
      ssRight  in _shift,
      ssMiddle in _shift,
      ssTouch  in _shift,
      ssPen    in _shift,
      _x,
      _y
    );

    try
      if ( not TiledPaint ) and InPaint then //TILER
        exit ;

      if View3D then begin
        Viewer3D.DoMouseMove( _x, _y ) ;
        exit ;
      end ;

      x := _x ;
      y := _y ;

      // end of drag or zoom or edit mode
      if _shift = [] then
      begin
        FDragging := False ;
        FZooming  := False ;
        FEditing  := False ;
      end ;

      case FMode of

        TGIS_ViewerMode.Select: // select mode
          begin
          end;

        TGIS_ViewerMode.Drag:   // dragging mode
          begin
            if not FDragging then
              exit;

            if (x < 0) or (y < 0) or
               (x > ControlCanvasWidth) or (y > ControlCanvasHeight)
            then
              exit;

            oWndHelper.DoDrag( mousePos.X - x, mousePos.Y - y ) ;

            resetMousePos(x, y);
          end;

        TGIS_ViewerMode.Zoom:   // rectangle zoom mode
          begin
            if not FZooming then
              exit;

            // restrict to window size
            if x < 0 then
              x := 0;
            if y < 0 then
              y := 0;
            if x > ControlCanvasWidth then
              x := RoundS(self.ControlCanvasWidth) - 1;
            if y > Height then
              y := RoundS(self.ControlCanvasHeight) - 1;

            // hide any previous frame
            if mousePosOld.X <> NO_MOUSE_POS then begin
              draw_zooming_rect( mousePosOld.X, mousePosOld.Y,
                                 mousePos.X - mousePosOld.X,
                                 mousePos.Y - mousePosOld.Y ) ;
              resetMousePos( mousePos.X, mousePos.Y ) ;
            end ;

            // allow zooming only for rectangle grater then MinZoomSize
            if Sqrt( Sqr( mousePos.X - x ) +  Sqr( mousePos.Y - y ) )
               <
               TwipsToPixels( MinZoomSize )
            then begin
              oZoomingRect.Visible := False;
              ControlRepaint;
              exit ;
            end ;

            // draw current frame
            zoomingColor := Random($FFFFFF) ;
            draw_zooming_rect( x, y, mousePos.X - x, mousePos.Y - y ) ;

            mousePosOld.X := x ;
            mousePosOld.Y := y ;
          end ;

        TGIS_ViewerMode.ZoomEx: // rubber zooming mode
          begin
            if not FZooming then
              exit;

            if mousePosOld.X <> NO_MOUSE_POS then begin
              zm := Power(1 + 1.0 * Abs(mousePosLast.Y - mousePosOld.Y) / Height, 10);

              if zm = 1 then
                exit;

              if mousePosOld.Y > mousePosLast.Y then
                zm := 1.0 / zm;

              oWndHelper.DoZoom( mousePos.X, mousePos.Y, zm, False, False ) ;
            end ;
            mousePosOld.X := RoundS(x);
            mousePosOld.Y := RoundS(y);
          end ;

        TGIS_ViewerMode.Edit:   // beginning of editing mode
          begin
            if not FEditing then exit;

            if Editor.ShowDraggingTrack then begin
              // hide any previous line
              if mousePosOld.X <> NO_MOUSE_POS then begin
                draw_dragging_track( mousePosOld.X, mousePosOld.Y,
                                     mousePos.X   , mousePos.Y
                                   ) ;
                resetMousePos( mousePos.X, mousePos.Y ) ;
              end ;

              // draw current line
              zoomingColor := Random($FFFFFF) ;
              draw_dragging_track( x          , y          ,
                                   mousePos.X , mousePos.Y
                                 ) ;
            end ;

            mousePosOld.X := x ;
            mousePosOld.Y := y ;

            Editor.MouseMove( Point(x, y) ) ;
          end ;
      end ;
    finally
      inherited ;
    end;
  end ;


  procedure TGIS_ViewerWnd.Paint ;
  var
    cl           : TGIS_Color  ;
    met_fastrot  : Boolean ;
    paintCanvas  : TObject ;
    wndCanvas    : TObject ;
    nativeCanvas : Boolean ;
    nsid         : Integer ;

    procedure prepare_renderer_context ;
    begin
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
      met_fastrot := GisMetadataAsBoolean(
         'TGIS_ViewerWnd.Paint.FastRotation',
         True
      ) ;

      // settings for renderers
      if bD2DCanvas then begin
        paintCanvas := nil ;
        wndCanvas := oD2DCanvas ;
        if bD2DRendering then
          paintCanvas := oD2DCanvas ;
        if not bD2DRendering then
          met_fastrot := False ;
        nativeCanvas := bD2DRendering and bTopMostDirectOnWindowRenderTarget ;
      end
      else if bSkiaRendering then begin
        {$IFNDEF SKIA_OPENGL_ONEWINDOW}
          createSkiaCanvas ;
        {$ENDIF}
        oSKCanvas := oRenderer.ViewerCreateWndPaint( ControlCanvasWidth,
                                                     ControlCanvasHeight,
                                                     oOpenGLCanvas ) ;
        paintCanvas := oSKCanvas ;
        wndCanvas := oSKCanvas ;
        met_fastrot := False ;
        nativeCanvas := True
      end
      else begin
        paintCanvas := nil ;
        wndCanvas := Canvas ;
        met_fastrot := False ;
        nativeCanvas := False ;
      end ;

    end ;

    procedure finish_renderer_context ;
    begin
      if bSkiaRendering then begin
        if assigned( oOpenGLConnection ) then
          TGIS_OpenGLSkiaConnection(oOpenGLConnection).Flush
        else
          oRenderer.ViewerFlushToWndPaint( oSKCanvas, Canvas ) ;
        oRenderer.ViewerFreeWndPaint( oSKCanvas ) ;
        {$IFNDEF SKIA_OPENGL_ONEWINDOW}
          freeSkiaCanvas( False ) ;
        {$ENDIF}
      end ;
    end ;

    function to_rect( const _r : TRectF ) : TRect ;
    begin
      Result := Rect(
                  Round( _r.Left   ),
                  Round( _r.Top    ),
                  Round( _r.Right  ),
                  Round( _r.Bottom )
                ) ;
    end;

    function scaled_factor(
      const ctx    : TGIS_TilerContext
    ) : Double ;
    var
      wc    : Double ;
      r1,r2 : Double ;
      ex    : TGIS_Extent ;
    begin
      ex := VisibleExtent ;
      wc := ControlCanvasWidth  ;

      r1 := ( ctx.Extent.XMax - ctx.Extent.XMin ) / ControlCanvasWidth  ;
      r2 := ( ex.XMax  - ex.XMin ) / wc ;

      Result := r1 / r2;
    end;

    function scaled_rect(
      const ctx    : TGIS_TilerContext
    ) : TRect ;
    var
      wc, hc : Double ;
      res    : TRect ;
      r1,r2  : TPointF ;
      rct    : TRectF ;
      ex     : TGIS_Extent ;
    begin
      ex := VisibleExtent ;
      wc := ControlCanvasWidth  ;
      hc := ControlCanvasHeight ;

      r1.X := ( ctx.Extent.XMax - ctx.Extent.XMin ) / wc  ;
      r1.Y := ( ctx.Extent.YMax - ctx.Extent.YMin ) / hc ;
      r2.X := ( ex.XMax  - ex.XMin ) / wc ;
      r2.Y := ( ex.YMax  - ex.YMin ) / hc ;

      rct := RectF(
                ( ex.XMin - ctx.Extent.XMin) / r1.X ,
               -( ex.YMax - ctx.Extent.YMax) / r1.X ,
               -( ex.XMax - ctx.Extent.XMax) / r1.X ,
                ( ex.YMin - ctx.Extent.YMin) / r1.X
             ) ;

      if Abs( r1.X / r2.X ) > 1000 then begin
        Result := Rect( 0, 0, 0, 0 );
        exit ;
      end ;

      res := Rect(
               RoundS( - rct.Left   * r1.X / r2.X ),
               RoundS( - rct.Top    * r1.X / r2.X ),
               RoundS( ( + rct.Right  * r1.X / r2.X ) + wc ) ,
               RoundS( ( + rct.Bottom * r1.X / r2.X ) + hc )
             ) ;

      Result := res ;
    end;

    function is_valid( _bitmap : TGIS_Bitmap ) : Boolean ;
    begin
      Result := assigned( _bitmap )
                and
                ( _bitmap is TGIS_Bitmap )
                and
                ( not TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap( _bitmap ) ) )
                and
                ( TGIS_Bitmap( _bitmap ).Width  > 0 )
                and
                ( TGIS_Bitmap( _bitmap ).Height > 0 ) ;
    end ;

    function draw_topmost
      : Boolean ;
    begin
      Result := False ;
      if bInUpdate and not bInFlash then exit ;
      bInUpdate := True ;

      if nativeCanvas then begin
        FTopContext.Clear ;
        FTopContext.AssignDrawContext( nil, oPaintTmp, oOpenGLConnection ) ;
      end else begin
        if assigned( FTopContext.BaseMap ) and
           ( ( TGIS_Bitmap(FTopContext.BaseMap).Width  <> ControlCanvasWidth ) or
             ( TGIS_Bitmap(FTopContext.BaseMap).Height <> ControlCanvasHeight ) ) then
          FTopContext.Clear ;

        if not assigned( FTopContext.BaseMap ) then
          FTopContext.AssignBaseMap( nil, True )
        else
          TGIS_Bitmap(FTopContext.BaseMap).Clear( TGIS_Color.None ) ;
      end ;
      if metPaintTopmostLabelsOnTop then
        FTopContext.AssignLabels( nil, True )
      else
        FTopContext.AssignLabels( nil, False ) ;

      try
        oRenderer.ReleaseContext ;

        oRenderer.CreateContext( Self, oVwr, FTopContext,
                                 Point(0, 0),
                                 ControlCanvasWidth, ControlCanvasHeight,
                                 PPI, FontScale ) ;

        try
          oVwr.LabelsReg.Reset ;
          oRenderer.PrepareDraw ;
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
          if not nativeCanvas then
            Result := True ;
        end;
      finally
        FTopContext.ClearDrawContext ;
        bInUpdate := False ;
      end;
    end ;

    function context_not_scaled(
      const ctx    : TGIS_TilerContext
    ) : Boolean ;
    begin
      Result := GisIsSameExtent( VisibleExtent, ctx.Extent );
    end;

    procedure paintStandard ;
    begin
      // draw background
      oRenderer.ViewerDrawBackground(
        wndCanvas,
        ControlCanvasWidth,
        ControlCanvasHeight,
        cl
      ) ;

      nsid := 0 ;

      // before paint events
      oRenderer.PaintExtra( Self, wndCanvas, FOnBeforePaintRenderer ) ;
      oRenderer.PaintExtra( Self, wndCanvas, FOnBeforePaint ) ;
      try
        if IsEmpty then
          exit ;

        if not oWndHelper.IsScaled
           and
           is_valid( FCacheBitmap )
           and
           ( FCacheBitmap.Width  = ControlCanvasWidth  )
           and
           ( FCacheBitmap.Height = ControlCanvasHeight )
           and
           not Assigned( FProgressBitmap )
        then begin

          oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;

          // set temporary paint
          oPaintTmp := oRenderer.ViewerCreateTemporaryPaint(
            ControlCanvasWidth, ControlCanvasHeight, paintCanvas
          ) ;

          try
            // begin draw
            oRenderer.ViewerBeginDrawOnTemporaryPaint( oPaintTmp, cl ) ;
            try
              oBaseMapHelper.LockBitmap ;
              try

                if oBaseMapHelper.Active
                   and
                   is_valid( oBaseMapHelper.Bitmap )
                   and
                   ( not GisIsEmptyExtent( VisibleExtent ) )
                then begin
                  // draw basemap
                  oRenderer.ViewerStretchBitmapFast(
                    oBaseMapHelper.Bitmap,
                    oPaintTmp,
                    to_rect(
                      oWndHelper.ScaledRect(
                        oBaseMapHelper.Extent,
                        oBaseMapHelper.Bitmap.Width,
                        oBaseMapHelper.Bitmap.Height,
                        1
                      )
                    )
                  ) ;
                end ;
                oBaseMapHelper.Request(
                  ControlCanvasWidth,
                  ControlCanvasHeight,
                  VisibleExtent,
                  RotationPoint,
                  RotationAngle,
                  met_fastrot
                ) ;
              finally
                oBaseMapHelper.UnLockBitmap ;
              end ;

              if not oVwr.IsEmpty and
                 ( assigned( FDrawContext.Basemap   ) or
                   assigned( FDrawContext.Selection ) or
                   assigned( FDrawContext.Charts    ) or
                   assigned( FDrawContext.Labels    ) or
                   oVwr.IsTopmost or
                   assigned( FFlashContext.BaseMap )
                 ) then
              begin
                oRenderer.ViewerBlendBitmaps(
                  FCacheBitmap,
                  oPaintTmp,
                  100,
                  True
                ) ;

                if assigned( FDrawContext.Selection ) then
                  oRenderer.ViewerBlendBitmaps(
                    FDrawContext.Selection,
                    oPaintTmp,
                    oVwr.SelectionTransparency,
                    True
                  ) ;

                if assigned( FDrawContext.Charts ) then
                  oRenderer.ViewerBlendBitmaps(
                    FDrawContext.Charts,
                    oPaintTmp,
                    100,
                    True
                  ) ;

                if not metPaintLabelsOnTop then
                  if assigned( FDrawContext.Labels ) then
                    oRenderer.ViewerBlendLabelBitmaps(
                      FDrawContext.Labels,
                      oPaintTmp,
                      True
                    ) ;

                if oVwr.IsTopmost and draw_topmost then
                  oRenderer.ViewerBlendBitmaps(
                    FTopContext.BaseMap,
                    oPaintTmp,
                    100,
                    True
                  ) ;

                if oVwr.IsTopmost and not metPaintLabelsOnTop then
                  if assigned( FTopContext.Labels ) then
                    oRenderer.ViewerBlendLabelBitmaps(
                      FTopContext.Labels,
                      oPaintTmp,
                      True
                    ) ;

                if assigned( FFlashContext.BaseMap ) then
                  oRenderer.ViewerBlendBitmaps(
                    FFlashContext.BaseMap,
                    oPaintTmp,
                    FFlashTransparency,
                    True
                  ) ;
              end ;

              oRenderer.RenderEditor( oPaintTmp ) ;

              oRenderer.PaintExtra( Self, oPaintTmp, FOnPaintExtra ) ;

              if not oVwr.IsEmpty and metPaintLabelsOnTop then
              begin
                // labels drawn on top
                if assigned( FDrawContext.Labels ) then
                  oRenderer.ViewerBlendLabelBitmaps(
                    FDrawContext.Labels,
                    oPaintTmp,
                    True
                  ) ;
                if oVwr.IsTopmost and assigned( FTopContext.Labels ) then
                  oRenderer.ViewerBlendLabelBitmaps(
                    FTopContext.Labels,
                    oPaintTmp,
                    True
                  ) ;
              end ;

            finally
              // end draw
              oRenderer.ViewerEndDrawOnTemporaryPaint( oPaintTmp ) ;
            end ;

            // prepare cache for navigation feedback
            FreeObject( oFullCache ) ;
            if metPaintNavigateFeedbackFullCache then begin
              oFullCache := oRenderer.ViewerCreateFullCache(
                              ControlCanvasWidth,
                              ControlCanvasHeight,
                              oPaintTmp,
                              oOpenGLConnection
                            ) ;
              oPaintCache := oFullCache ;
            end
            else begin
              oPaintCache := FCacheBitmap ;
            end ;

            // render on the window canvas
            if metPaintNavigateFeedbackFullCache then
              // use already prepared bitmap
              oRenderer.ViewerFlushTemporaryPaint(
                wndCanvas,
                ControlCanvasWidth,
                ControlCanvasHeight,
                oPaintTmp,
                oFullCache,
                Rect( 0, 0,
                      ControlCanvasWidth,
                      ControlCanvasHeight )
              )
            else
              oRenderer.ViewerFlushTemporaryPaint(
                wndCanvas,
                ControlCanvasWidth,
                ControlCanvasHeight,
                oPaintTmp,
                Rect( 0, 0,
                      ControlCanvasWidth,
                      ControlCanvasHeight )
              ) ;

          finally
            oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;
          end ;

          nsid := GIS_SUBSCRIBED_TRANSPARENT_CONTROL_UPDATE ;
        end
        else begin

          if bCacheBitmapNew then begin
            oPaintCache := FCacheBitmap ;
            oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;
          end ;

          if not Assigned( oPaintTmp )
             or
             ( Assigned( FCacheBitmap )
               and
               (
                 ( ControlCanvasWidth  <> FCacheBitmap.Width )
                 or
                 ( ControlCanvasHeight <> FCacheBitmap.Height )
               )
             )
          then
          begin
            // new paint is required
            oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;
            oPaintTmp := oRenderer.ViewerCreateTemporaryPaintEx(
              ControlCanvasWidth, ControlCanvasHeight, paintCanvas
            ) ;
          end
          else
            // clear existing paint
            oRenderer.ViewerClearTemporaryPaint( oPaintTmp ) ;

          oBaseMapHelper.LockBitmap ;
          try
            if oBaseMapHelper.Active
               and
               is_valid( oBaseMapHelper.Bitmap )
               and
               ( not GisIsEmptyExtent( VisibleExtent ) )
            then
              // draw basemap
              oRenderer.ViewerStretchBitmapFast(
                oBaseMapHelper.Bitmap,
                oPaintTmp,
                to_rect(
                  oWndHelper.ScaledRect(
                    oBaseMapHelper.Extent,
                    oBaseMapHelper.Bitmap.Width,
                    oBaseMapHelper.Bitmap.Height,
                    1
                  )
                )
              ) ;
            oBaseMapHelper.Request(
              ControlCanvasWidth,
              ControlCanvasHeight,
              oWndHelper.ActualExtent,
              RotationPoint,
              RotationAngle,
              met_fastrot
            ) ;
          finally
            oBaseMapHelper.UnlockBitmap ;
          end ;

          if is_valid( FProgressBitmap ) then begin
            // cache in background
            if metPaintProgressiveFullCache then
              oRenderer.ViewerDrawCache(
                oPaintCache,
                oPaintTmp,
                to_rect( oWndHelper.ScaledRect( 1 ) ),
                metPaintProgressiveTransparency
              )
            else
              oRenderer.ViewerDrawCache(
                FCacheBitmap,
                oPaintTmp,
                to_rect( oWndHelper.ScaledRect( 1 ) ),
                metPaintProgressiveTransparency
              ) ;

            // progress bitmap
            oRenderer.ViewerDrawProgressBitmap(
              FProgressBitmap,
              oPaintTmp,
              Rect( 0, 0, ControlCanvasWidth, ControlCanvasHeight )
            ) ;
          end
          else
            // draw cache on temporary paint
            oRenderer.ViewerDrawCache( oPaintCache,
                                       oPaintTmp,
                                       to_rect( oWndHelper.ScaledRect( 1 ) )
                                     ) ;

          // draw temporary paint on canvas
          oRenderer.ViewerFlushTemporaryPaint(
            wndCanvas,
            ControlCanvasWidth,
            ControlCanvasHeight,
            oPaintTmp,
            Rect( 0, 0,
                  ControlCanvasWidth,
                  ControlCanvasHeight )
          ) ;

          nsid := GIS_SUBSCRIBED_TRANSPARENT_CONTROL_PAINT ;
        end ;
      finally

        if oZoomingRect.Visible then
          oRenderer.ViewerDrawZoomingRect( wndCanvas,
                                           oZoomingRect.X,
                                           oZoomingRect.Y,
                                           oZoomingRect.Width,
                                           oZoomingRect.Height,
                                           oZoomingRect.Color
                                         ) ;
        if oDraggingTrack.Visible then
          oRenderer.ViewerDrawDraggingTrack( wndCanvas,
                                             oDraggingTrack.X1,
                                             oDraggingTrack.Y1,
                                             oDraggingTrack.X2,
                                             oDraggingTrack.Y2,
                                             oDraggingTrack.Color
                                           ) ;

        bCacheBitmapNew := False ;

        // after paint events
        oRenderer.PaintExtra( Self, wndCanvas, FOnAfterPaint ) ;
        oRenderer.PaintExtra( Self, wndCanvas, FOnAfterPaintRenderer ) ;
        // graticules
        if Graticule.Enabled then
          oRenderer.PaintExtra( Self, wndCanvas, doGraticule ) ;

        if nsid <> 0 then
          NotifySubscribers( nsid, wndCanvas ) ;
      end ;
    end ;

    procedure paintTiler ;
    var
      icol,irow : Int64   ;
      iw, ih    : Integer ;
      bresize   : Boolean ;
      bready    : Boolean ;
      scope     : TGIS_TilerScope ;
      sc        : Integer ;
      scx, scy  : Double ;
      tile      : TGIS_TilerTile;
      ctx       : TGIS_TilerContext ;
    begin
      if ( Renderer.FriendlyName <> 'Skia' ) and
         ( Renderer.FriendlyName <> 'Direct2D' ) then begin
        oVwr.TiledPaint := False ;
        ShowMessage( '#### TiledPaint currently supported only on SKIA and Direct2D ####' );  //TILER
        ControlUpdateWholeMap ;
        exit ;
      end;

      DelayedUpdate := 5 ; // force fast updates
      UseAnimations := True ;

      if oTilerHelper.TideUp then
        oTilerState := TGIS_TilerState.UpdateAll ;

      oRenderer.ViewerDrawBackground(
        wndCanvas,
        ControlCanvasWidth,
        ControlCanvasHeight,
        cl
      ) ;

      if IsEmpty then
        exit ;

      nsid := 0 ;

      oRenderer.Setup( self, oVwr, Point(0, 0),
                       ControlCanvasWidth, ControlCanvasHeight,
                       PPI, FontScale
                     ) ;
      oRenderer.PrepareDraw ;

      // before paint events
      oRenderer.PaintExtra( Self, wndCanvas, FOnBeforePaintRenderer ) ;
      oRenderer.PaintExtra( Self, wndCanvas, FOnBeforePaint ) ;

      if ( not oWndHelper.IsScaled )
        and
        ( not Assigned( FProgressBitmap ) )
        and
        ( oWndHelper.OriginalRect.Width  = ControlCanvasWidth  )
        and
        ( oWndHelper.OriginalRect.Height = ControlCanvasHeight )
      then begin
        ctx := oTilerHelper.GetContext ;
        try
          if not Assigned( ctx ) then
            FTileContext := nil
          else
            FTileContext := ctx.Context ;

          oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;

          // set temporary paint
          // for some renderers it is simply paintCanvas
          oPaintTmp := oRenderer.ViewerCreateTemporaryPaint(
            ControlCanvasWidth, ControlCanvasHeight, paintCanvas
          ) ;

          // begin draw
          oRenderer.ViewerBeginDrawOnTemporaryPaint( oPaintTmp, cl ) ;
          try

            // first step
            // drawing exiing tiles scaled to extent to provide feedback

            if GisIsNoWorld( oTilerExtent ) then
              oTilerExtent := VisibleExtent ;

            scx := ( oTilerExtent.XMax - oTilerExtent.XMin ) /
                   ( VisibleExtent.XMax - VisibleExtent.XMin ) ;
            scy := ( oTilerExtent.YMax - oTilerExtent.YMin ) /
                   ( oWndHelper.ActualExtent.YMax - oWndHelper.ActualExtent.YMin ) ;

            scope := oTilerHelper.GetScope( oTilerExtent,
                                            oWndHelper.ActualExtent,
                                            ControlCanvasWidth,
                                            ControlCanvasHeight
                                          ) ;

            if scx <> 1 then begin
              for icol := scope.Left to scope.Right do begin
                for irow := scope.Top to scope.Bottom do begin
                  // draw each tile
                  tile := oTilerHelper.LockTile( scope.Scale, icol, irow);
                  if not Assigned( tile ) then begin
                    continue;
                  end;

                  try
                    for sc := 0 to tile.Count -1 do begin
                      // draw each subtile
                      tile.Subtiles[sc].Lock;
                      try
                        if tile.Subtiles[sc].Bitmap = nil then begin
                          continue ;
                        end;

                        oRenderer.ViewerStretchBitmapFast(
                          tile.Subtiles[sc].Bitmap,
                          wndCanvas,
                          to_rect(
                            oWndHelper.ScaledRect(
                              tile.Extent,
                              oTilerHelper.TileSize,
                              oTilerHelper.TileSize,
                              1
                            )
                          )
                        );
                      finally
                        tile.Subtiles[sc].Unlock;
                      end;
                    end ;
                  finally
                    tile.Unlock ;
                  end;
                end ;
              end ;
            end;

            // second step
            // request tiles to match current visual extent
            // and draw them

            scope := oTilerHelper.GetScope( VisibleExtent,
                                            oWndHelper.ActualExtent,
                                            ControlCanvasWidth,
                                            ControlCanvasHeight
                                          ) ;

            bready := True ;
            for icol := scope.Left to scope.Right do begin
              for irow := scope.Top to scope.Bottom do begin
                tile := oTilerHelper.LockTile( scope.Scale, icol, irow );
                if not Assigned( tile ) then begin
                  bready := False ;
                  continue;
                end;

                try
                  // draw each tile
                  for sc := 0 to tile.Count -1  do begin
                    // draw each subtile
                    tile.Subtiles[sc].Lock;
                    try
                      if tile.Subtiles[sc].Bitmap = nil then begin
                        bready := False ;
                        continue ;
                      end ;
                      if tile.Subtiles[sc].Obsolate then
                        bready := False ;

                      oRenderer.ViewerStretchBitmapFast(
                        tile.Subtiles[sc].Bitmap,
                        wndCanvas,
                        to_rect(
                          oWndHelper.ScaledRect(
                            tile.Extent,
                            oTilerHelper.TileSize,
                            oTilerHelper.TileSize,
                            1
                          )
                        )
                      ) ;
                    finally
                      tile.Subtiles[sc].Unlock;
                    end;
                  end ;
                finally
                  tile.Unlock ;
                end;
              end ;
            end ;

            // request tiles matching extent

            case oTilerState of
              TGIS_TilerState.Repaint :
                begin
                end ;
              TGIS_TilerState.UpdateAll :
                begin
                  if not bready then begin
                    oTilerHelper.Request( VisibleExtent,
                                          ControlCanvasWidth,
                                          ControlCanvasHeight
                                        ) ;
                    oTilerState := TGIS_TilerState.UpdateLabels ;
                  end
                  else begin
                    oTilerHelper.RequestLabels( VisibleExtent,
                                                ControlCanvasWidth,
                                                ControlCanvasHeight
                                              ) ;
                    oTilerState := TGIS_TilerState.Repaint ;
                  end;
                end ;
              TGIS_TilerState.UpdateLabels :
                begin
                  if bready then begin
                    oTilerHelper.RequestLabels( VisibleExtent,
                                                ControlCanvasWidth,
                                                ControlCanvasHeight
                                              ) ;
                    oTilerState := TGIS_TilerState.Repaint ;
                  end;
                end ;
            end;

            if bready and ( scx <> 1 ) then begin
              // tiles are eady but not for the current scale
              oTilerExtent := VisibleExtent ;
              oTilerHelper.RequestRepaint ;
            end
            else begin
              // process any updates isseud from the code
              if oWndHelper.PendingUpdate then
                oWndHelper.UpdateDelayed ;
            end;

            // draw any exiting lables, selections etc.
            if not oVwr.IsEmpty then begin
              if assigned( FTileContext ) and
                 assigned( FTileContext.Selection ) then
              begin
                oRenderer.ViewerStretchBitmapFast(
                  FTileContext.Selection,
                  oPaintTmp,
                  scaled_rect(ctx),
                  oVwr.SelectionTransparency
                );
              end;

              if assigned( FTileContext ) and
                 assigned( FTileContext.Charts ) then
              begin
                oRenderer.ViewerStretchBitmapFast(
                  FTileContext.Charts,
                  oPaintTmp,
                  scaled_rect(ctx)
                );
              end;

              if not metPaintLabelsOnTop then begin
                if assigned( FTileContext ) and
                   assigned( FTileContext.Labels ) then
                begin
                  if scaled_factor(ctx) < 4 then

                  oRenderer.ViewerStretchBitmapFast(
                    FTileContext.Labels,
                    oPaintTmp,
                    scaled_rect(ctx)
                  );
                end;
              end;

              if oVwr.IsTopmost and draw_topmost then begin
                oRenderer.ViewerStretchBitmapFast(
                  FTopContext.BaseMap,
                  oPaintTmp,
                  scaled_rect(ctx)
                );
              end ;

              if oVwr.IsTopmost and assigned( FTopContext.Labels ) and
                 not metPaintLabelsOnTop
              then begin
                oRenderer.ViewerStretchBitmapFast(
                  FTopContext.Labels,
                  oPaintTmp,
                  scaled_rect(ctx)
                );
              end ;

              if assigned( FFlashContext.BaseMap ) then begin
                if context_not_scaled( ctx ) then
                  oRenderer.ViewerBlendBitmaps(
                    FFlashContext.BaseMap,
                    wndCanvas,
                    FFlashTransparency,
                    True
                  ) ;
              end ;


              if bready and oTilerHelper.Suspend then begin
                try
                  oRenderer.RenderEditor( oPaintTmp ) ;
                  oRenderer.PaintExtra( Self, oPaintTmp, FOnPaintExtra );
                finally
                  oTilerHelper.Continue;
                end;
              end;

              if not oVwr.IsEmpty and metPaintLabelsOnTop then begin
                if assigned( FTileContext.Labels ) then begin
                  oRenderer.ViewerStretchBitmapFast(
                    FTileContext.Labels,
                    oPaintTmp,
                    scaled_rect(ctx)
                  );
                end ;
              end ;

              if oVwr.IsTopmost and assigned( FTopContext.Labels ) then begin
                oRenderer.ViewerStretchBitmapFast(
                  FTopContext.Labels,
                  oPaintTmp,
                  scaled_rect(ctx)
                );
              end ;

            end ;
          finally
            // end draw
            oRenderer.ViewerEndDrawOnTemporaryPaint( oPaintTmp ) ;
          end ;

          oRenderer.ViewerFlushTemporaryPaint(
            wndCanvas,
            ControlCanvasWidth,
            ControlCanvasHeight,
            oPaintTmp,
            Rect( 0, 0,
                  ControlCanvasWidth,
                  ControlCanvasHeight
                )
          ) ;
        finally
          oTilerHelper.ReleaseContext ;
        end;

         nsid := GIS_SUBSCRIBED_TRANSPARENT_CONTROL_UPDATE ;
      end
      else begin
        // scaled draw

        oTilerState := TGIS_TilerState.UpdateAll ;

        // first step
        // draw scaled exiting tiles when zoom were changed to provide backround
        // before new ties are ready

        iw := ControlCanvasWidth ;
        ih := ControlCanvasHeight ;
        bresize := False ;

        if oWndHelper.OriginalRect.Width <> ControlCanvasWidth then begin
          iw := RoundS( oWndHelper.OriginalRect.Width );
          bresize := True ;
        end;
        if oWndHelper.OriginalRect.Height <> ControlCanvasHeight then begin
          ih := RoundS( oWndHelper.OriginalRect.Height );
          bresize := True ;
        end;

        scope := oTilerHelper.GetScope( oTilerExtent,
                                        oWndHelper.ActualExtent,
                                        iw,
                                        ih
                                      ) ;

        scx := ( oTilerExtent.XMax - oTilerExtent.XMin ) /
               ( oWndHelper.ActualExtent.XMax - oWndHelper.ActualExtent.XMin ) ;
        scy := ( oTilerExtent.YMax - oTilerExtent.YMin ) /
               ( oWndHelper.ActualExtent.YMax - oWndHelper.ActualExtent.YMin ) ;

        // only if somehow scaled
        if bresize or ( ( scx <> 1 ) and ( scy <> 1 ) ) then begin
          for icol := scope.Left to scope.Right do begin
            for irow := scope.Top to scope.Bottom do begin
              // draw each tile
              tile := oTilerHelper.LockTile( scope.Scale, icol, irow);
              if not Assigned( tile ) then
                continue;
              try
                for sc := 0 to tile.Count - 1 do begin
                  // draw each subtile
                  tile.Subtiles[sc].Lock;
                  try
                    if ( tile.Subtiles[sc].Bitmap = nil ) then begin
                      continue ;
                    end;

                    oRenderer.ViewerStretchBitmapFast(
                      tile.Subtiles[sc].Bitmap,
                      wndCanvas,
                      to_rect(
                         oWndHelper.ScaledRect(
                           oWndHelper.ActualExtent,
                           tile.Extent,
                           oTilerHelper.TileSize,
                           oTilerHelper.TileSize,
                           1
                         )
                      )
                    );
                  finally
                    tile.Subtiles[sc].Unlock;
                  end;
                end ;
              finally
                tile.Unlock ;
              end;
            end ;
          end ;
        end ;


        if bresize then begin
          if oWndHelper.PendingUpdate then
            oWndHelper.UpdateDelayed
        end
        else begin
          if oWndHelper.DesiredExtent.XMin <> 0  then
            // request tiles to match final extent
            oTilerHelper.Request( oWndHelper.DesiredExtent,
                                  ControlCanvasWidth,
                                  ControlCanvasHeight
                                ) ;


          // second step
          // draw tiles maching current extent and scale

          scope := oTilerHelper.GetScope( oWndHelper.ActualExtent,
                                          oWndHelper.ActualExtent,
                                          ControlCanvasWidth,
                                          ControlCanvasHeight
                                        ) ;

          bready := true ;
          for icol := scope.Left to scope.Right do begin
            for irow := scope.Top to scope.Bottom do begin
              // draw each tile
              tile := oTilerHelper.LockTile( scope.Scale, icol, irow );
              if not Assigned( tile ) then begin
                bready := False ;
                continue;
              end;

              try
                for sc := 0 to tile.Count - 1 do begin
                  // draw each subtile
                  tile.Subtiles[sc].Lock;
                  try
                    if tile.Subtiles[sc].Bitmap =  nil then begin
                      bready := false ; // extent not completed
                      continue ;
                    end;
                    if tile.Subtiles[sc].Obsolate then
                      bready := false ;

                    oRenderer.ViewerStretchBitmapFast(
                      tile.Subtiles[sc].Bitmap,
                      wndCanvas,
                        to_rect(
                           oWndHelper.ScaledRect(
                             oWndHelper.ActualExtent,
                             tile.Extent,
                             oTilerHelper.TileSize,
                             oTilerHelper.TileSize,
                             1
                           )
                        )
                    ) ;
                  finally
                    tile.Subtiles[sc].Unlock;
                  end;
                end ;
              finally
                tile.Unlock ;
              end;
            end ;
          end ;

          if bready then // completed extent
            oTilerExtent := oWndHelper.ActualExtent
          else begin
            // process any updates isseud from the code
            if oWndHelper.PendingUpdate then
               oWndHelper.UpdateDelayed
          end;

          if ( not bready ) and ( scx = 1 ) and ( scy = 1 ) then
            // no scaling request final tiles
            oTilerHelper.Request( scope.Extent,
                                  ControlCanvasWidth,
                                  ControlCanvasHeight
                                ) ;
        end;

        oRenderer.ViewerFlushTemporaryPaint(
          wndCanvas,
          ControlCanvasWidth,
          ControlCanvasHeight,
          oPaintTmp,
          to_rect( oWndHelper.ScaledRect( 1 ) )
        ) ;

        nsid := GIS_SUBSCRIBED_TRANSPARENT_CONTROL_PAINT ;
      end ;

      if oZoomingRect.Visible then
        oRenderer.ViewerDrawZoomingRect( wndCanvas,
                                         oZoomingRect.X,
                                         oZoomingRect.Y,
                                         oZoomingRect.Width,
                                         oZoomingRect.Height,
                                         oZoomingRect.Color
                                       ) ;
      if oDraggingTrack.Visible then
        oRenderer.ViewerDrawDraggingTrack( wndCanvas,
                                           oDraggingTrack.X1,
                                           oDraggingTrack.Y1,
                                           oDraggingTrack.X2,
                                           oDraggingTrack.Y2,
                                           oDraggingTrack.Color
                                         ) ;
      bCacheBitmapNew := False ;

      oRenderer.PaintExtra( Self, wndCanvas, FOnAfterPaint ) ;
      oRenderer.PaintExtra( Self, wndCanvas, FOnAfterPaintRenderer ) ;

      if Graticule.Enabled then
        oRenderer.PaintExtra( Self, wndCanvas, doGraticule ) ;
      if nsid <> 0 then
        NotifySubscribers( nsid, wndCanvas ) ;
    end ;


  begin
    if iReenterCount > 0 then
     exit ;
    Inc( iReenterCount );
    try
      {$IFDEF GIS_PDK}
        if bItIsProxy then begin
          inherited ;
          exit ;
        end;
      {$ENDIF}

      if oVwr.Parent as TObject <> self then
        exit ;

      if {$IFDEF LEVEL_RX11_VCL}
           IsCustomStyleActive
         {$ELSE}
           TStyleManager.IsCustomStyleActive
         {$ENDIF}
         and
         (seClient in StyleElements)
      then begin
        {$IFDEF LEVEL_RX11_VCL}
          cl := GisColor( StyleServices(self).GetSystemColor( clWindow ) )
        {$ELSE}
          cl := GisColor( StyleServices.GetSystemColor( clWindow ) )
        {$ENDIF}
      end
      else
          cl := oVwr.Color ;

      if csDesigning in ComponentState then begin
        inherited ;

        Canvas.Brush.Color := VCLColor( cl ) ;
        Canvas.FillRect( ClientRect );
        exit ;
      end ;

      if View3D then begin
        TGIS_Viewer3DDirectX(Viewer3D).UpdateHWND( self.Handle ) ;
        Viewer3D.ControlRepaint ;
        exit ;
      end;

      try
        prepare_renderer_context ;

        oRenderer.OptimizeBitmapCache ;

        if not TiledPaint then
          paintStandard
        else
          paintTiler ;

        finish_renderer_context ;
        HourglassRestart ;
      except
        On e : Exception do begin
          NotifyPaintException( 'PaintException', e ) ;
        end ;
      end;
    finally
      Dec( iReenterCount ) ;
    end;
  end ;

  procedure TGIS_ViewerWnd.Resize ;
  begin
    resizeContext ;

    oVwr.ResetPPI ;

    oWndHelper.KeepScale := KeepScale ;
    oWndHelper.UpdateCancel ;

    Invalidate ;


    if not IsEmpty then
      oWndHelper.UpdateDelayed ;

    inherited ;
  end ;

  procedure TGIS_ViewerWnd.WndProc(
    var _msg : TMessage
  ) ;
  begin
    inherited;

    {$IFDEF GIS_PDK}
      if Assigned( oProxy ) then begin
        if _msg.Msg = WM_SETCURSOR then
          exit ;

        SendNotifyMessage( oProxy.Handle, _msg.Msg, _msg.WParam, _msg.LParam );
      end;
    {$ENDIF}
  end;

  {$IFDEF GIS_PDK}
    procedure TGIS_ViewerWnd.DestroyHandle;
    begin
      inherited;
    end;
  {$ENDIF}

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
      FOnTapSimple( Self, _button, _shift, RoundS(_x), RoundS(_y) );
  end;

  procedure TGIS_ViewerWnd.TapDouble(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if Assigned( FOnTapDouble ) then
      FOnTapDouble( Self, _button, _shift, RoundS(_x), RoundS(_y) );
  end;

  procedure TGIS_ViewerWnd.TapLong(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if Assigned( FOnTapLong ) then
      FOnTapLong( Self, _button, _shift, RoundS(_x), RoundS(_y) );
  end;

//==============================================================================
// constructor/destructor
//==============================================================================

  constructor TGIS_ViewerWnd.Create(
    _owner: TComponent
  );
  begin
    inherited Create( _owner ) ;

    if csDesigning in ComponentState then
      EnsureFramework ;

    oVwr := TGIS_Viewer.Create( self ) ;

    FBorderStyle := bsNone ;

    IncrementalPaint := True ;

    FCacheBitmap    := nil ;
    bCacheBitmapNew := False ;
    FProgressBitmap := nil ;
    oD2DCanvas      := nil ;
    bD2DCanvas      := false ;
    bD2DRendering   := false ;
    iWidth          := 0 ;
    iHeight         := 0 ;
    iTmpWidth       := 0 ;
    iTmpHeight      := 0 ;
    iReenterCount   := 0 ;

    FDrawContext  := TGIS_RendererContext.Create ;
    FTopContext   := TGIS_RendererContext.Create ;
    FFlashContext := TGIS_RendererContext.Create ;

    oFullCache    := nil ;
    bTopMostDirectOnWindowRenderTarget := True ;

    bSkiaRendering := False ;
    {$IFDEF GIS_RENDERER_GDI32}
      oRenderer := RendererManager.CreateInstance( 'TGIS_RendererVclGdi32');
    {$ENDIF}
    {$IFNDEF GIS_RENDERER_GDIPLUS}
      if not Assigned( oRenderer ) then
        oRenderer := RendererManager.CreateInstance( 'TGIS_RendererVclSkia');
      if assigned( oRenderer ) then
        bSkiaRendering := True ;
      if not Assigned( oRenderer ) then begin
        oRenderer := RendererManager.CreateInstance( 'TGIS_RendererVclDirect2D');
        if Assigned( oRenderer ) then begin
          if TGIS_RendererVclDirect2D( oRenderer ).Supported then
            TGIS_RendererVclDirect2D( oRenderer ).KeepD2DCanvasAlive
          else
            FreeObject( oRenderer ) ;
        end;
      end ;
    {$ENDIF}
    if not Assigned( oRenderer ) then
      oRenderer := RendererManager.CreateInstance( 'TGIS_RendererVclGdiPlus');

    // Skia data
    oSKCanvas         := nil ;
    oOpenGLConnection := nil ;
    oOpenGLCanvas     := nil ;

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
    oTilerHelper     := nil ; //TILER
    bTilerSuspended  := False ;

    oGraticuleHelper := TGIS_GraticuleHelper.Create(
                          self.oVwr
                        ) ;
    oGraticule       := oGraticuleHelper ;

    DoubleBuffered := True ;

    Ctl3D      := True ;
    Color      := $7FFFFFFF ; // first stupid one
    Color      := clWindow ;  // next real value to avoid VCL
                              // testing for changed value

    FMinZoomSize      := GIS_MIN_ZOOM_SIZE ;
    FAutoCenter       := False ;
    FModeMouseButton  := TMouseButton.mbLeft ;
    FTemporaryScaleInternal := 0 ;

    IncrementalPaint  := True  ;    //TILER
    TiledPaint        := False ;    //TILER

    oDraggingTrack := T_DraggingTrack.Create ;
    oDraggingTrack.Visible := False;

    oZoomingRect := T_ZoomingRect.Create ;
    oZoomingRect.Visible := False;

    bInUpdate := False ;
    bInFlash  := False ;

    oTimer := TTimer.Create( nil ) ;
    oTimer.OnTimer := doTimer ;

    FOnEditorChange := nil ;
    FOnBeforePaint  := nil ;
    FOnAfterPaint   := nil ;
    FOnBeforePaintRenderer := nil ;
    FOnAfterPaintRenderer  := nil ;
    FOnPaintExtra   := nil ;
    FOnBeforeUpdate := nil ;
    FOnUpdate       := nil ;
    FOnAfterUpdate  := nil ;

    {$IFDEF MSWINDOWS}
      Screen.Cursors[DRAG_CURSOR]      := LoadCursor(HInstance, 'HANDGRAB') ;
      Screen.Cursors[SELECT_CURSOR]    := LoadCursor(HInstance, 'HANDPNT' ) ;
      Screen.Cursors[ZOOM_CURSOR]      := LoadCursor(HInstance, 'HANDZOOM') ;
      Screen.Cursors[EDIT_CURSOR]      := LoadCursor(HInstance, 'HANDEDIT') ;
      Screen.Cursors[CUSTOM_CURSOR]    := LoadCursor(HInstance, IDC_ARROW ) ;
      Screen.Cursors[WAIT_CURSOR]      := LoadCursor(HInstance, IDC_WAIT  ) ;
    {$ENDIF}

    FCursorForCameraPosition := crDefault ;
    FCursorForCameraRotation := crDefault ;
    FCursorForCameraXYZ      := crDefault ;
    FCursorForCameraXY       := crDefault ;
    FCursorForZoom           := crDefault ;
    FCursorForSunPosition    := crDefault ;
    FCursorFor3DSelect       := crDefault ;
    {$IFDEF MSWINDOWS}
      Screen.Cursors[CAMPOS_CURSOR ]   := LoadCursor(HInstance, 'CAMPOS'  ) ;
      Screen.Cursors[CAMROT_CURSOR ]   := LoadCursor(HInstance, 'CAMROT'  ) ;
      Screen.Cursors[CAMXYZ_CURSOR ]   := LoadCursor(HInstance, 'CAMXYZ'  ) ;
      Screen.Cursors[CAMXY_CURSOR ]    := LoadCursor(HInstance, 'CAMXY'   ) ;
      Screen.Cursors[CAMZOOM_CURSOR]   := LoadCursor(HInstance, 'CAMZOOM' ) ;
      Screen.Cursors[SUNPOS_CURSOR ]   := LoadCursor(HInstance, 'SUNPOS'  ) ;
      Screen.Cursors[SELECT3D_CURSOR ] := LoadCursor(HInstance, 'HANDPNT' ) ;
    {$ENDIF}

    Touch.InteractiveGestures := [ igPan, igZoom ] ;
    Touch.InteractiveGestureOptions := [ igoPanSingleFingerVertical,
                                         igoPanSingleFingerHorizontal,
                                         igoPanInertia,
                                         igoParentPassthrough ] ;
    OnGesture := doGestureProc ;
  end ;

  destructor TGIS_ViewerWnd.Destroy ;
  begin
    if View3D then begin
      TGIS_Viewer3DBase( FViewer3D ).Free ;
      FViewer3D := nil ;
    end;

    destroyContext ;
    // must be called here
    // before the renderer context bitmaps can be freed
    oRenderer.ReleaseContext ;

    FreeObject( FDrawContext     ) ;
    FreeObject( FTopContext      ) ;
    FreeObject( FFlashContext    ) ;

    FreeObject( oDraggingTrack   ) ;
    FreeObject( oZoomingRect     ) ;

    FreeObject( FCacheBitmap     ) ;
    FreeObject( FProgressBitmap  ) ;
    FreeObject( oTimer           ) ;
    FreeObject( oBasemapHelper   ) ;
    FreeObject( oTilerHelper     ) ; //TILER
    FreeObject( oGraticuleHelper ) ;
    FreeObject( oWndHelper       ) ;

    oRenderer.ViewerFreeTemporaryPaint( oPaintTmp ) ;
    FreeObject( oRenderer        ) ;
    FreeObject( oVwr             ) ;
    FreeObject( oD2DCanvas       ) ;
    FreeObject( oOpenGLConnection ) ;
    FreeObject( oFullCache       ) ;
    inherited ;
  end ;

//==============================================================================
// public methods of IGIS_Viewer
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
  end ;

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
    const _message    : String    ;
    const _exception  : Exception
  ) : Boolean ;
  begin
    Result := oVwr.NotifyPaintException( _message, _exception ) ;
  end ;

  procedure TGIS_ViewerWnd.Lock ;
  begin
    WaitForBackgroundProcesses;
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

  //TILER
  function TGIS_ViewerWnd.Interrupted
    : Boolean;
  begin
    Result := oVwr.Interrupted ;
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
  end;

  procedure TGIS_ViewerWnd.BusyPrepare(
    _sender  : TObject ;
    _text    : String
  ) ;
  begin
    oVwr.BusyPrepare( _sender, _text ) ;
  end ;

  procedure TGIS_ViewerWnd.BusyRelease(
    _sender  : TObject
  );
  begin
    oVwr.BusyRelease( _sender ) ;
  end ;

  procedure TGIS_ViewerWnd.BusyShake(
        _sender  : TObject ;
        _pos     : Int64 ;
        _end     : Int64 ;
    var _abort   : Boolean
  );
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
  end ;

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
  end ;

  procedure TGIS_ViewerWnd.EndPaintInternal ;
  begin
    oVwr.EndPaintInternal ;
  end ;

  function TGIS_ViewerWnd.SynchronizePaint(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := oVwr.SynchronizePaint( _interrupt ) ;
  end;

  procedure TGIS_ViewerWnd.ReParentLock ;
  begin
    oVwr.ReParentLock ;
    if Assigned( oTilerHelper ) then
      oTilerHelper.Suspend( True );
  end ;

  procedure TGIS_ViewerWnd.ReParentUnlock ;
  begin
    oVwr.ReParentUnlock ;
    if Assigned( oTilerHelper ) then begin
      oTilerHelper.Continue;
    end;
  end ;

  function TGIS_ViewerWnd.ReParent(
    const _parent : IGIS_ViewerParent
  ) : IGIS_ViewerParent ;
  begin
    Result := oVwr.ReParent( _parent ) ;
  end ;

  function TGIS_ViewerWnd.AttachLayer(
    const _layer : TGIS_LayerAbstract
  ) : IGIS_Viewer ;
  begin
    Result := oVwr.AttachLayer( _layer ) ;
  end ;

  procedure TGIS_ViewerWnd.Open(
    const _path    : String
  ) ;
  begin
    oVwr.Open( _path ) ;
  end ;

  procedure TGIS_ViewerWnd.Open(
    const _path    : String ;
    const _strict  : Boolean
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
      if TGIS_Viewer3DBase( FViewer3D ).IsBusy then exit ;

      TGIS_Viewer3DBase( FViewer3D ).Free ;
      FViewer3D := nil ;
    end;

    if not SynchronizePaint(False) then exit ;

    oVwr.Close ;
    FreeObject( FCacheBitmap ) ;
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
    oVwr.Draw( _renderer, _mode );
  end ;

  function TGIS_ViewerWnd.GetGrid(
    const _extent  : TGIS_Extent  ;
    const _grid    : TGIS_GridArray
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
    const _path         : String
  ) ;
  begin
    oVwr.SaveProjectAs( _path ) ;
  end ;

  procedure TGIS_ViewerWnd.SaveProjectAs(
    const _path         : String          ;
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
    const _extent  : TGIS_Extent
  ) ;
  begin
    oVwr.InvalidateExtent( _extent ) ;
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
    oVwr.InvalidateWholeMap
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
  end;

  function TGIS_ViewerWnd.FullExtentZoom
    : Double ;
  begin
    Result := oVwr.FullExtentZoom ;
  end ;

  procedure TGIS_ViewerWnd.FullExtent ;
  begin
    if not SynchronizePaint( True) then
      exit ;

    oWndHelper.DoFullExtent ;
    oVwr.FullExtent ;
  end ;

  function TGIS_ViewerWnd.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double
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
    const _pt     : TPoint ;
    const _prec   : Integer
  ) : TGIS_ShapeAbstract ;
  begin
    if View3D then begin
      Result := TGIS_ShapeAbstract( Viewer3D.Locate( _pt, _prec ) ) ;
      exit ;
    end ;

    Result := oVwr.Locate( _pt, _prec ) ;
  end ;

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
  end ;

  function TGIS_ViewerWnd.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreen3D( _ptg )
    else
      Result := oVwr.MapToScreen3D( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.ScreenToMap(
    const _pt  : TPoint
  ) : TGIS_Point ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMap( _pt )
    else
      Result := oVwr.ScreenToMap( _pt ) ;
  end ;

  function TGIS_ViewerWnd.ScreenToMap3D(
    const _pt  : TPoint
  ) : TGIS_Point3D ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMap3D( _pt )
    else
      Result := oVwr.ScreenToMap3D( _pt ) ;
  end ;

  function TGIS_ViewerWnd.MapToScreenEx(
    const _pt  : TGIS_Point
  ) : TGIS_Point ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreenEx( _pt )
    else
      Result := oVwr.MapToScreenEx( _pt ) ;
  end ;

  function TGIS_ViewerWnd.ScreenToMapEx(
    const _pt  : TGIS_Point
  ) : TGIS_Point ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMapEx( _pt )
    else
      Result := oVwr.ScreenToMapEx( _pt ) ;
  end ;

  function TGIS_ViewerWnd.MapToScreenRect(
    const _rct : TGIS_Extent
  ) : TRect ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.MapToScreenRect( _rct )
    else
      Result := oVwr.MapToScreenRect( _rct ) ;
  end ;

  function TGIS_ViewerWnd.ScreenToMapRect(
    const _rct : TRect
  ) : TGIS_Extent ;
  begin
    if IsSameType( Self, oVwr.Parent ) and ( not InPaint ) and ( not IsLocked ) then
      Result := oWndHelper.ScreenToMapRect( _rct )
    else
      Result := oVwr.ScreenToMapRect( _rct ) ;
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
    if View3D then begin
      oVwr.MoveViewport( _dx, _dy ) ;
      exit ;
    end ;

    oWndHelper.DoDrag( _dx, _dy ) ;
    oWndHelper.FitScreenAnimation ;
    oWndHelper.UpdateDelayed ;
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

  function TGIS_ViewerWnd.SetViewer(const _viewer: TObject): TObject;
  begin
    Result := oVwr ;
    oVwr := _Viewer as TGIS_Viewer ;
  end;

  procedure TGIS_ViewerWnd.SetViewport(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    oVwr.SetViewport( _x, _y ) ;
  end ;

  procedure TGIS_ViewerWnd.CenterViewport(
    const _ptg     : TGIS_Point
  ) ;
  begin
    oVwr.CenterViewport( _ptg ) ;
  end ;

  procedure TGIS_ViewerWnd.SetCSByWKT(
    const  _wkt    : String
  ) ;
  begin
    oVwr.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_ViewerWnd.SetCSByEPSG(
    const  _epsg    : Integer
  ) ;
  begin
    oVwr.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_ViewerWnd.SetCSByWKTFile(
    const  _path    : String
  ) ;
  begin
    oVwr.SetCSByWKTFile( _path ) ;
  end ;

  function TGIS_ViewerWnd.RotatedPoint(
    const _ptg    : TGIS_Point
  ) : TGIS_Point;
  begin
    Result := oVwr.RotatedPoint( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.UnrotatedPoint(
    const _ptg    : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.UnrotatedPoint( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.RotatedPoint3D(
    const _ptg    : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.RotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_ViewerWnd.RotatedPoint3D_ref(
    var _ptg      : TGIS_Point3D
  ) ;
  begin
    oVwr.RotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_ViewerWnd.UnrotatedPoint3D(
    const _ptg    : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.UnrotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_ViewerWnd.UnrotatedPoint3D_ref(
    var _ptg    : TGIS_Point3D
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

  function TGIS_ViewerWnd.GetRenderContext : TObject ;
  begin
    Result := oVwr.GetRenderContext ;
  end ;

  procedure TGIS_ViewerWnd.WaitForBackgroundProcesses ;
  begin
    oVwr.WaitForBackgroundProcesses ;
    if not TiledPaint then //TILER
      oBasemapHelper.WaitFor ;
  end;

  procedure TGIS_ViewerWnd.WaitForNotBusy(
          _sender : TObject ;
    const _proc   : TGIS_WaitForNotBusyProc
  ) ;
  begin
    oVwr.WaitForNotBusy( _sender, _proc ) ;
  end;

  function TGIS_ViewerWnd.GetViewer: TObject;
  begin
    Result := oVwr ;
  end;

//==============================================================================
// IGIS_ViewerParent public methods
//==============================================================================

  procedure TGIS_ViewerWnd.ControlClose ;
  begin
    oPaintCache := nil ;
  end ;

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
    bmp     : TGIS_Bitmap          ;
    cnv     : TObject              ;
    old_ext : TGIS_Extent          ;
    old_res : Boolean              ;
    la      : TGIS_Layer           ;
  begin
    Lock ;
    old_ext := VisibleExtent ;
    old_res := RestrictedDrag ;
    try
      bmp := TGIS_Bitmap( _bmp ) ;
      iWidth := bmp.Width ;
      iHeight := bmp.Height ;
      RestrictedDrag := False ;
      VisibleExtent  := _extent ;

      if oVwr.Color.ARGB = TGIS_Color.None.ARGB then begin
      end
      else
        bmp.Clear( TGIS_Color.FromARGB( 0, 0, 0, 0 ) ) ;

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
          oRenderer.PrepareDraw ;
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
            oVwr.Draw( oRenderer, TGIS_DrawMode.AllExcept3D ) ;

        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;

          cnv := oRenderer.ViewerCreateTemporaryPaint( TGIS_Bitmap(ctx.BaseMap) ) ;
          try
            if Assigned( ctx.Selection ) then
              oRenderer.ViewerBlendBitmaps(
                ctx.Selection,
                cnv,
                oVwr.SelectionTransparency,
                False
              ) ;
            if Assigned( ctx.Charts ) then
              oRenderer.ViewerBlendBitmaps(
                ctx.Charts,
                cnv,
                100,
                False
              ) ;
            if Assigned( ctx.Labels ) then
              oRenderer.ViewerBlendBitmaps(
                ctx.Labels,
                cnv,
                100,
                False
              ) ;
          finally
            oRenderer.ViewerFreeTemporaryPaint( cnv, TGIS_Bitmap(ctx.BaseMap) ) ;
          end;
        end;
      finally
        oRenderer.ReleaseContext ;
        FreeObject( ctx ) ;
      end ;
    finally
      iWidth := 0 ;
      iHeight := 0 ;
      VisibleExtent  := old_ext ;
      RestrictedDrag := old_res ;
      Unlock( False ) ;
    end;
  end ;

  function TGIS_ViewerWnd.ControlRenderer ;
  begin
    Result := oRenderer ;
  end ;

  procedure TGIS_ViewerWnd.ControlFlash(
    const _times : Integer ;
    const _delay : Integer
  ) ;
  begin
    System.Classes.TThread.Synchronize(System.Classes.TThread.CurrentThread,
      procedure()
      var
        i : Integer ;
        old_rnd : TGIS_RendererAbstract ;
      begin
        if Assigned( oTilerHelper ) then //TILER
          oTilerHelper.Suspend( True ) ;
        try

          if View3D then exit ;

          if bInUpdate then exit ;
          bInUpdate := True ;

          old_rnd := oRenderer ;
          try
            FFlashContext.AssignDrawContext( oD2DCanvas, nil, nil ) ;
            FFlashContext.AssignBaseMap( nil, True ) ;

            try
              oRenderer.ReleaseContext ;
              oRenderer := oRenderer.CreateInstance ; // clone
              oRenderer.CreateContext( Self, oVwr, FFlashContext, Point(0, 0),
                                       RoundS( ControlCanvasWidth ),
                                       RoundS( ControlCanvasHeight ),
                                       PPI, FontScale
                                     );
              try
                oVwr.LabelsReg.Reset ;
                oRenderer.PrepareDraw ;
                oVwr.BeginPaintInternal ;

                Draw( oRenderer, TGIS_DrawMode.Flash ) ;

              finally
                oVwr.EndPaintInternal ;
                oRenderer.AfterDraw ;
              end ;

              bInFlash := True ;
              try
                for i := 1 to _times do
                begin
                  ControlProcessMessages ;

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
        finally
          if Assigned( oTilerHelper ) then //TILER
            oTilerHelper.Continue ;
        end;
      end
    ) ;
  end ;

  function TGIS_ViewerWnd.ControlSystemPPI
    : Integer;
  var
    frm : TCustomForm ;
  begin
    {$IFDEF LEVEL_RX101_VCL}
      Result := Screen.PixelsPerInch;
      frm := GetParentForm( self, True ) ;
      if Assigned( frm ) then
        Result := frm.Monitor.PixelsPerInch;
    {$ELSE}
      Result := Screen.PixelsPerInch;
    {$ENDIF}
  end ;

  function TGIS_ViewerWnd.ControlPPI
    : Integer;
  begin
    Result := PPI ;
  end;

  function TGIS_ViewerWnd.ControlCanvasScale
    : Single ;
  begin
    Result := 1 ;
  end;

  function ownerHasParent( const _parent : TWinControl ) : Boolean ;
  var
    p : TWinControl ;
  begin
    {$IFDEF GIS_XDK}
      Result := True ;  // XDK never has a parent form!
    {$ELSE}
      p := _parent ;
      while assigned( p ) do begin
        if p is TCustomForm then
          break ;
        p := p.Parent ;
      end ;
      Result := Assigned( p ) ;
    {$ENDIF}
  end ;

  function TGIS_ViewerWnd.ControlCanvasHeight
    : Integer ;
  begin
    if iHeight <> 0 then
      Result := iHeight
    else if (iTmpHeight <> 0) and not ownerHasParent( Parent ) then
      Result := iTmpHeight
    else begin
      Result := RoundS( ClientHeight ) ;
      iTmpHeight := Result ;
    end;
  end ;

  function TGIS_ViewerWnd.ControlCanvasWidth
    : Integer ;
  begin
    if iWidth <> 0 then
      Result := iWidth
    else if (iTmpWidth <> 0) and not ownerHasParent( Parent ) then
      Result := iTmpWidth
    else begin
      Result := RoundS( ClientWidth ) ;
      iTmpWidth := Result ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.ControlProcessMessages ;
  begin
    Application.ProcessMessages ;
  end ;

  procedure TGIS_ViewerWnd.ControlRepaint ;
  begin
    if View3D then
      Viewer3D.ControlRepaint
    else
      Invalidate ;
  end ;

  function TGIS_ViewerWnd.ControlUpdateSynchronize(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    if InPaint then begin
      if _interrupt then
        Interrupt ;
    end ;

    Result := ( not InPaint ) and ( not IsBusy ) ;
  end;

  procedure TGIS_ViewerWnd.ControlUpdateWholeMap ;
  begin
    System.Classes.TThread.Synchronize(System.Classes.TThread.CurrentThread,
      procedure()
      var
        btiles : Boolean ;
        la     : TGIS_LayerAbstract ;
      begin
        if FZooming  or
           FDragging or
           FEditing  or
           oWndHelper.GestureActive
        then
          exit ;

        if Assigned( oTilerHelper ) then  // TILER
          oTilerHelper.ClearCache ;

        if TiledPaint or bTilerSuspended then begin
          btiles := True ;
          for la in Items do begin
            if not TGIS_Layer(la).SupportsTiledPaint then begin
              btiles := False ;
              break;
            end;
          end;

          if btiles then begin
            if bTilerSuspended then begin
              bTilerSuspended := False ;
              TiledPaint := True ;
              exit;
            end;
          end
          else begin
            if TiledPaint then begin
              bTilerSuspended := True ;
              TiledPaint := False ;
              exit;
            end ;
          end;
        end;

        if View3D then begin
          Viewer3D.UpdateWholeMap ;
          NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, self ) ;
        end else
          oWndHelper.UpdateImmediate ;
      end
    ) ;
  end ;

  procedure TGIS_ViewerWnd.ControlUpdateProgressive ;
  begin
    if GetCurrentThreadId <> MainThreadID then exit ;
    FDrawContext.DoProgressiveUpdate ;
  end;

  procedure TGIS_ViewerWnd.ControlUpdateTopmost ;
  begin
    if View3D then
      Viewer3D.UpdateTopmost
    else
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
      if not TiledPaint then //TILER
        oBasemapHelper.Reset ;
    end;
  end;

  procedure TGIS_ViewerWnd.ControlUpdateSelection ;
  begin
    System.Classes.TThread.Synchronize(System.Classes.TThread.CurrentThread,
      procedure()
      var
        ctx : TGIS_RendererContext ;
      begin

        if Assigned( oTilerHelper ) then //TILER
          oTilerHelper.Suspend( True ) ;
        try

          if View3D then begin
            Viewer3D.UpdateAllSelectedObjects ;
            exit ;
          end;

          if not GisIsSameExtent( oWndHelper.ActualExtent, VisibleExtent ) then begin
            oWndHelper.UpdateDelayed ;
            exit ;
          end;

          if bInUpdate then exit ;
          bInUpdate := True ;

          try
            if TiledPaint then begin
              if Assigned( FTileContext ) then
                ctx := FTileContext
              else
                exit ;
            end
            else
              ctx := FDrawContext ;

            ctx.AssignDrawContext( oD2DCanvas, nil, nil ) ;
            ctx.AssignSelection( nil, True ) ;

            oRenderer.ReleaseContext ;
            oRenderer.CreateContext( Self, oVwr, ctx, Point(0, 0),
                                     RoundS( ControlCanvasWidth ),
                                     RoundS( ControlCanvasHeight ),
                                     PPI, FontScale
                                   );
            try
              oRenderer.PrepareDraw ;
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

        finally
          if Assigned( oTilerHelper ) then //TILER
            oTilerHelper.Continue ;
        end;

      end
    );
  end ;

  procedure TGIS_ViewerWnd.ControlUpdateEditor(
    const _final : Boolean
  ) ;
  begin
    if _final then
      ControlUpdateWholeMap
    else
      ControlRepaint ;
  end;

  procedure TGIS_ViewerWnd.ControlHourglassShow ;
  begin
    if GetCurrentThreadId <> MainThreadID then exit ;
    hourglassCursor := Screen.Cursor ;
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

  procedure TGIS_ViewerWnd.ControlExtentChanged ;
  begin
    if Assigned( oTilerHelper ) then begin // TILER
      oTilerExtent := GisNoWorld ;
      if oTilerHelper.UpdateExtent then
        oTilerHelper.ClearAll ;
    end;
  end;

  procedure TGIS_ViewerWnd.ControlHourglassHide ;
  begin
    if GetCurrentThreadId <> MainThreadID then exit ;
    Screen.Cursor := hourglassCursor ;
  end ;

  function TGIS_ViewerWnd.ControlHourglassShake
    : Boolean ;
  begin
    Result := False ;
    if GetCurrentThreadId <> MainThreadID then exit ;

    Screen.Cursor := crHourglass ;
    ControlProcessMessages ;
  end ;

  procedure TGIS_ViewerWnd.ControlSet3DMode(
    const _mode : TGIS_Viewer3DMode
  ) ;
  begin
    fset_3DMode( _mode ) ;
  end ;

  procedure TGIS_ViewerWnd.ControlRaiseEditorChangeEvent(
    _sender : TObject
  ) ;
  begin
    if assigned( FOnEditorChange ) then
      FOnEditorChange( _sender ) ;
  end ;

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

    ReParentLock;
    bInUpdate := True ;
    try
      if assigned( _bmp ) then
        tbmp := _bmp
      else
        tbmp := TGIS_Bitmap.Create( ControlCanvasWidth, ControlCanvasHeight ) ;

      doPrintBmp( tbmp, _full ) ;

      if not assigned( _bmp ) then
        _bmp := tbmp ;
    finally
      bInUpdate := False ;
      ReParentUnlock;
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
    pm  : TGIS_PrintManager ;
    prn : TGIS_Printer ;
  begin
    // wait for ready
    if InPaint then exit ;


    if _printer = nil then
      prn := TGIS_Printer.Create( Printer )
    else begin
      if not ( _printer is TGIS_Printer ) then exit ;
      prn := _printer as TGIS_Printer ;
    end ;

    pm := TGIS_PrintManager.Create ;
    try
      pm.Print( oVwr, prn ) ;
    finally
      FreeObject( pm ) ;
      if _printer = nil then
        FreeObject( prn ) ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.ZoomBy(
    const _zm : Double  ;
    const _x  : Integer ;
    const _y  : Integer
  ) ;
  begin
    if View3D then begin
      Viewer3D.ZoomBy(_zm, _x, _y) ;
    end
    else begin
      oWndHelper.UpdateCancel ;
      oWndHelper.DoZoom( _x, _y, _zm, True, True );
      oWndHelper.UpdateDelayed ;
    end;
  end ;

//==============================================================================
// TGIS_ViewerWnd public methods
//==============================================================================

  function TGIS_ViewerWnd.GetCacheBitmap
    : VCL.Graphics.TBitmap ;
  var
    bmp : TGIS_Bitmap ;
  begin
    Result := nil ;
    if not TGIS_Bitmap.IsNilOrEmpty( oPaintCache )
       and
       ( oPaintCache.Width > 0 )
       and
       ( oPaintCache.Height > 0 ) then begin
      // create a temporary bitmap only for conversion to NativeBitmap
      bmp := TGiS_Bitmap.Create ;
      bmp.Assign( oPaintCache ) ;
      try
        Result := VCL.Graphics.TBitmap.Create ;
        Result.Width := oPaintCache.Width ;
        Result.Height := oPaintCache.Height ;
        Result.PixelFormat := pf32bit ;
        // background
        Result.Canvas.Brush.Style := bsSolid ;
        Result.Canvas.Brush.Color := VCLColor( oVwr.Color ) ;
        Result.Canvas.FillRect( Rect(0, 0, Result.Width, Result.Height ) );
        // copy cache
        TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
          bmp.NativeBitmap,
          Result,
          100,
          True
        ) ;
      finally
        FreeObject( bmp ) ;
      end ;
    end ;
  end ;

  procedure TGIS_ViewerWnd.PrintBmp(
    var _bmp : VCL.Graphics.TBitmap
  ) ;
  begin
    PrintBmp( _bmp, False ) ;
  end ;

  procedure TGIS_ViewerWnd.PrintBmp(
    var _bmp    : VCL.Graphics.TBitmap ;
    const _full : Boolean
  ) ;
  var
    bmp : TGIS_Bitmap ;
  begin
    if bInUpdate then exit ;
    bInUpdate := True ;
    bmp := nil ;
    try
      if assigned( _bmp ) then begin
        bmp := TGIS_Bitmap.Create ;
        bmp.LoadFromBitmap( _bmp, '' ) ;
      end else begin
        bmp := TGIS_Bitmap.Create( ControlCanvasWidth, ControlCanvasHeight ) ;
        //bmp.PixelFormat := pf32bit ;
      end ;

      doPrintBmp( bmp, _full ) ;

      if not assigned( _bmp ) then
        _bmp := TBitmap.Create ;
      _bmp.Assign( TBitmap(bmp.NativeBitmap) ) ;
    finally
      FreeObject( bmp ) ;
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
  var
    bmp : VCL.Graphics.TBitmap ;
  begin
    bmp := nil ;
    try
      PrintBmp( bmp, _full ) ;
      Clipboard.Assign( bmp ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

  {$IFDEF GIS_PDK}
    procedure TGIS_ViewerWnd.SetProxy(
      const _proxy : TGIS_ViewerWnd
    ) ;
    begin
      oProxy := _proxy ;
      if Assigned( oProxy ) then
        oProxy.bItIsProxy := True ;
    end;
  {$ENDIF}

  procedure Register;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ViewerWnd ] ) ;
  end ;

//==================================== END =====================================
end.


