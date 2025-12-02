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

{$IFDEF DCC}
  unit GisViewer3DBase ;
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.WinForms ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.IO,
    System.Drawing,
    TatukGIS.RTL,
    TatukGIS.NDK ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.UITypes,
    {$IFDEF LEVEL_XE3_RTL}
      System.Types,
    {$ELSE}
      Winapi.Windows,
    {$ENDIF}

    GisRtl,
    GisInterfaces,
    GisFunctions,
    GisTypes,
    GisTypesUI,
    GisClasses,
    GisLayer,
    GisRenderer3DAbstract,
    GisTypes3D ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    tatukgis.rtl ;
{$ENDIF}

type
  /// <summary>
  ///   Encapsulation of the 3D viewer.
  /// </summary>
  TGIS_Viewer3DBase = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_ObjectDisposable, IGIS_Viewer3D )

    private
      {$IFDEF DCC}
        // force uncounted interface
        function QueryInterface ( const _iid : TGUID;
                                  out   _obj
                                ) : HResult; stdcall;
        function _AddRef        : Integer; stdcall;
        function _Release       : Integer; stdcall;
      {$ENDIF}
    protected
      /// <summary>
      ///   3D renderer object.
      /// </summary>
      oRenderer : TGIS_Renderer3DAbstract ;
    private
      {$IFDEF CLR}
        FOnUpdate : EventHandler ;
      {$ELSE}
        FOnUpdate : TNotifyEvent ;
      {$ENDIF}
    public

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Construct an instance of 3D Viewer
      /// </summary>
      /// <param name="_hwnd">
      ///   handle to a window in which viewer should be constructed; if 0 then
      ///   object will be created on _viewer
      /// </param>
      /// <param name="_viewer">
      ///   master viewer window
      /// </param>
      /// <param name="_extent">
      ///   extent restriction (all objects outside will be cut off)
      /// </param>
      /// <param name="_initdraw">
      ///   draw initial view if True, do not draw if False
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    To support automatic mouse handling object should be created on a
      ///    </note>
      /// </remarks>
      constructor Create  ( const _hwnd     : THandle ;
                            const _viewer   : IGIS_Viewer ;
                            const _extent   : TGIS_Extent ;
                            const _initdraw : Boolean
                          ) ; virtual;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override ;

    private  // property internal values

      FMode          : TGIS_Viewer3DMode ;
      FAdvNavigation : Boolean ;
      FOrthoView     : Boolean ;
      sunCameraDelta : Single  ;

    private // property access routines
      procedure setRenderer                ( const _value : TGIS_Renderer3DAbstract
                                           ) ;

      function  fget_Mode                  : TGIS_Viewer3DMode ;
      procedure fset_Mode                  ( const _value : TGIS_Viewer3DMode
                                           ) ;
      function  fget_ScaleZ                : Double ;
      procedure fset_ScaleZ                ( const _value : Double
                                           ) ;
      function  fget_ScaleM                : Double ;
      procedure fset_ScaleM                ( const _value : Double
                                           ) ;
      function  fget_Scale                 : Double ;
      procedure fset_Scale                 ( const _value : Double
                                           ) ;
      function  fget_ScaleAsText           : String ;
      procedure fset_ScaleAsText           ( const _value : String
                                           ) ;
      function  fget_Wireframe             : Boolean ;
      procedure fset_Wireframe             ( const _value : Boolean
                                           ) ;
      function  fget_Lights                : Boolean ;
      procedure fset_Lights                ( const _value : Boolean
                                           ) ;
      function  fget_Labels                : Boolean ;
      procedure fset_Labels                ( const _value : Boolean
                                           ) ;
      function  fget_HideLabels             : Boolean ;
      procedure fset_HideLabels            ( const _value : Boolean
                                           ) ;
      function  fget_VectorEdges           : Boolean ;
      procedure fset_VectorEdges           ( const _value : Boolean
                                           ) ;
      function  fget_EdgesColor            : TGIS_Color ;
      procedure fset_EdgesColor            ( const _value : TGIS_Color
                                           ) ;
      function  fget_ReferencePointer      : Boolean ;
      procedure fset_ReferencePointer      ( const _value : Boolean
                                           ) ;
      function  fget_ReferencePoint        : TGIS_Point3D ;
      procedure fset_ReferencePoint        ( const _value : TGIS_Point3D
                                           ) ;
      function  fget_ReferencePointMode    : TGIS_Viewer3DReferenceMode ;
      procedure fset_ReferencePointMode    ( const _value : TGIS_Viewer3DReferenceMode
                                           ) ;
      function  fget_ReferencePointOffset  : Double ;
      procedure fset_ReferencePointOffset  ( const _value : Double
                                           ) ;
      function  fget_CameraPosition        : TGIS_Point3D ;
      procedure fset_CameraPosition        ( const _value : TGIS_Point3D
                                           ) ;
      function  fget_CameraPositionEx      : TGIS_Point3D ;
      procedure fset_CameraPositionEx      ( const _value : TGIS_Point3D
                                           ) ;
      function  fget_CameraRotation        : TGIS_Point3D ;
      procedure fset_CameraRotation        ( const _value : TGIS_Point3D
                                           ) ;
      function  fget_SunPosition           : TGIS_Point ;
      procedure fset_SunPosition           ( const _value : TGIS_Point
                                           ) ;
      function  fget_KeepSunCamera         : Boolean ;
      procedure fset_KeepSunCamera         ( const _value : Boolean
                                           ) ;
      function  fget_ShadowsLevel          : Integer ;
      procedure fset_ShadowsLevel          ( const _value : Integer
                                           ) ;
      function  fget_FastMode              : Boolean ;
      procedure fset_FastMode              ( const _value : Boolean
                                           ) ;
      function  fget_LightVector           : Boolean ;
      procedure fset_LightVector           ( const _value : Boolean
                                           ) ;
      function  fget_DemWalls              : TGIS_Viewer3DDemWall ;
      procedure fset_DemWalls              ( const _value : TGIS_Viewer3DDemWall
                                           ) ;
      function  fget_UniverseColor         : TGIS_Color ;
      procedure fset_UniverseColor         ( const _value : TGIS_Color
                                           ) ;
      function  fget_TextureMode           : Boolean ;
      procedure fset_TextureMode           ( const _value : Boolean
                                           ) ;
      function  fget_IsolineGap            : Double ;
      procedure fset_IsolineGap            ( const _value : Double) ;
      function  fget_IsolineColor          : TGIS_Color ;
      procedure fset_IsolineColor          ( const _value : TGIS_Color
                                           ) ;
      procedure fset_SolidWallColor        ( const _value : TGIS_Color
                                           ) ;
      function  fget_Flood                 : TGIS_Viewer3DFlood ;
      procedure fset_Flood                 ( const _value : TGIS_Viewer3DFlood
                                           ) ;
      function  fget_BasePlane             : TGIS_Viewer3DBasePlane ;
      procedure fset_BasePlane             ( const _value : TGIS_Viewer3DBasePlane
                                           ) ;
      function  fget_VisibleExtent3D       : TGIS_Extent3D ;
      function  fget_VisibleExtent         : TGIS_Extent ;
      procedure fset_VisibleExtent         ( const _value : TGIS_Extent ) ;
      function  fget_ZoomFactor            : Double ;
      function  fget_Restriction           : TGIS_Viewer3DViewRestriction ;
      procedure fset_Restriction           ( const _value : TGIS_Viewer3DViewRestriction
                                           ) ;
      function  fget_DemTransparency       : Boolean ;
      procedure fset_DemTransparency       ( const _value : Boolean
                                           ) ;
      function  fget_TransparencyPriority  : TGIS_Viewer3DTransparencyPriority ;
      procedure fset_TransparencyPriority  ( const _value : TGIS_Viewer3DTransparencyPriority
                                           ) ;
      function  fget_ErrorMsg              : String ;
      function  fget_PixelSize             : TGIS_Point ;
      function  fget_DemDetailExtentFactor : Double ;

      procedure fset_DemDetailExtentFactor ( const _value : Double
                                           ) ;
      function  fget_DemDraftExtentFactor  : Double ;
      procedure fset_DemDraftExtentFactor  ( const _value : Double
                                           ) ;
      function  fget_VectorExtentFactor    : Double ;
      procedure fset_VectorExtentFactor    ( const _value : Double
                                           ) ;
      function  fget_VectorSimplification  : Boolean ;
      procedure fset_VectorSimplification  ( const _value : Boolean
                                           ) ;
      function  fget_VectorSmartSize       : Integer ;
      procedure fset_VectorSmartSize       ( const _value : Integer
                                           ) ;
      function  fget_DemGridSize           : Integer ;
      function  fget_DemCachedSize         : TGIS_Viewer3DDemCacheSize ;
      procedure fset_DemCachedSize         ( const _value : TGIS_Viewer3DDemCacheSize
                                           ) ;
      function  fget_IgnoreAbove           : Double ;
      procedure fset_IgnoreAbove           ( const _value : Double
                                           ) ;
      function  fget_IgnoreBelow           : Double ;
      procedure fset_IgnoreBelow           ( const _value : Double
                                           ) ;
      function  fget_CutAbove              : Double ;
      procedure fset_CutAbove              ( const _value : Double
                                           ) ;
      function  fget_CutBelow              : Double ;
      procedure fset_CutBelow              ( const _value : Double
                                           ) ;
      function  fget_IgnoreEllipsoidHeight : Boolean ;
      procedure fset_IgnoreEllipsoidHeight ( const _value : Boolean
                                           ) ;
      function  fget_AdvNavigation         : Boolean ;
      procedure fset_AdvNavigation         ( const _value : Boolean
                                           ) ;
      function  fget_OrthoView             : Boolean ;
      procedure fset_OrthoView             ( const _value : Boolean ) ;

      function  fget_IsBusy                : Boolean ;

    private // other private variables
      oViewer       : IGIS_Viewer ;
      hWindow       : THandle ;
      bCanRender    : Boolean ;

      bWasMouseDown : Boolean ;

      oldCameraPosition : TGIS_Point3D ;
      oldCameraRotation : TGIS_Point3D ;
      oldSunPosition    : TGIS_Point ;
      oldMouse          : TPoint ;

    private // other private methods

      /// <summary>
      ///   Transform underlying exception into TGIS_Viewer.OnPaintException
      /// </summary>
      procedure notifyPaintExceptionEvent( const _layerName : String ;
                                           const _exception : Exception
                                         ) ;
      {$IFDEF CLR}
        /// <summary>
        ///   Handler for TGIS_Viewer3D.OnUpdate event.
        /// </summary>
        procedure doOnUpdate             (       _sender    : TObject ;
                                                 _e         : EventArgs
                                         ) ;
      {$ELSE}

        /// <summary>
        ///   Handler for TGIS_Viewer3D.OnUpdate event.
        /// </summary>
        procedure doOnUpdate             (       _sender    : TObject
                                         ) ;
      {$ENDIF}
      {$IFDEF CLR}
        procedure doOnPaintException     (        _sender    : System.Object ;
                                                  _e         :
                                                       TGIS_PaintExceptionEventArgs
                                         ) ;
      {$ELSE}
        procedure doOnPaintException     (        _sender    : TObject ;
                                                  _exception : EGIS_PaintException
                                         ) ;
      {$ENDIF}
    public

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property Mode
        : TGIS_Viewer3DMode
        read  FMode
        write fset_Mode ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ScaleZ
        : Double
        read  fget_ScaleZ
        write fset_ScaleZ ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ScaleM
        : Double
        read  fget_ScaleM
        write fset_ScaleM ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property Scale
        : Double
        read  fget_Scale
        write fset_Scale ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ScaleAsText
        : String
        read  fget_ScaleAsText
        write fset_ScaleAsText ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShowWireframe
        : Boolean
        read  fget_Wireframe
        write fset_Wireframe ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShowLights
        : Boolean
        read  fget_Lights
        write fset_Lights ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShowLabels
        : Boolean
        read  fget_Labels
        write fset_Labels ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property HideLabelsUponNavigation
        : Boolean
        read  fget_HideLabels
        write fset_HideLabels ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShowVectorEdges
        : Boolean
        read  fget_VectorEdges
        write fset_VectorEdges ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property EdgesColor
        : TGIS_Color
        read  fget_EdgesColor
        write fset_EdgesColor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShowReferencePoint
        : Boolean
        read  fget_ReferencePointer
        write fset_ReferencePointer ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ReferencePoint
        : TGIS_Point3D
        read  fget_ReferencePoint
        write fset_ReferencePoint ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ReferencePointMode
        : TGIS_Viewer3DReferenceMode
        read  fget_ReferencePointMode
        write fset_ReferencePointMode ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ReferencePointOffsetZ
        : Double
        read  fget_ReferencePointOffset
        write fset_ReferencePointOffset ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property CameraPosition
        : TGIS_Point3D
        read  fget_CameraPosition
        write fset_CameraPosition ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property CameraPositionEx
        : TGIS_Point3D
        read  fget_CameraPositionEx
        write fset_CameraPositionEx ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property CameraRotation
        : TGIS_Point3D
        read  fget_CameraRotation
        write fset_CameraRotation ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property SunPosition
        : TGIS_Point
        read  fget_SunPosition
        write fset_SunPosition ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property KeepSunCamera
        : Boolean
        read  fget_KeepSunCamera
        write fset_KeepSunCamera ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShadowsLevel
        : Integer
        read  fget_ShadowsLevel
        write fset_ShadowsLevel ;

      {$IFDEF OXYGENE}
        /// <event/>
        /// <inheritdoc from="IGIS_Viewer3D"/>
        event UpdateEvent
          : EventHandler
          delegate FOnUpdate ;
      {$ELSE}
        /// <event/>
        /// <inheritdoc from="IGIS_Viewer3D"/>
        property UpdateEvent
          : TNotifyEvent
          read  FOnUpdate
          write FOnUpdate ;
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemWalls
        : TGIS_Viewer3DDemWall
        read  fget_DemWalls
        write fset_DemWalls ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property UniverseColor
        : TGIS_Color
        read  fget_UniverseColor
        write fset_UniverseColor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ShowDemTexture
        : Boolean
        read  fget_TextureMode
        write fset_TextureMode ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemIsolineGap
        : Double
        read  fget_IsolineGap
        write fset_IsolineGap ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemIsolineColor
        : TGIS_Color
        read  fget_IsolineColor
        write fset_IsolineColor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property WallsColor
        : TGIS_Color
        write fset_SolidWallColor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property Flood
        : TGIS_Viewer3DFlood
        read  fget_Flood
        write fset_Flood ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property BasePlane
        : TGIS_Viewer3DBasePlane
        read  fget_BasePlane
        write fset_BasePlane ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property VisibleExtent3D
        : TGIS_Extent3D
        read  fget_VisibleExtent3D ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property VisibleExtent
        : TGIS_Extent
        read  fget_VisibleExtent
        write fset_VisibleExtent ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property Zoom
        : Double
        read fget_ZoomFactor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property AllowDemTransparency
        : Boolean
        read  fget_DemTransparency
        write fset_DemTransparency ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property TransparencyPriority
        : TGIS_Viewer3DTransparencyPriority
        read  fget_TransparencyPriority
        write fset_TransparencyPriority;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ViewRestriction
        : TGIS_Viewer3DViewRestriction
        read  fget_Restriction
        write fset_Restriction ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property ErrorMessage
        : String
        read  fget_ErrorMsg ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property PixelSize
        : TGIS_Point
        read  fget_PixelSize ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemDetailExtentFactor
        : Double
        read  fget_DemDetailExtentFactor
        write fset_DemDetailExtentFactor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemDraftExtentFactor
        : Double
        read  fget_DemDraftExtentFactor
        write fset_DemDraftExtentFactor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property VectorExtentFactor
        : Double
        read  fget_VectorExtentFactor
        write fset_VectorExtentFactor ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property VectorSimplification
        : Boolean
        read  fget_VectorSimplification
        write fset_VectorSimplification ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property VectorSmartSize
        : Integer
        read  fget_VectorSmartSize
        write fset_VectorSmartSize ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemGridSize
        : Integer
        read  fget_DemGridSize ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property DemCachedSize
        : TGIS_Viewer3DDemCacheSize
        read  fget_DemCachedSize
        write fset_DemCachedSize ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property IgnoreAbove
        : Double
        read  fget_IgnoreAbove
        write fset_IgnoreAbove ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property IgnoreBelow
        : Double
        read  fget_IgnoreBelow
        write fset_IgnoreBelow ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property CutAbove
        : Double
        read  fget_CutAbove
        write fset_CutAbove ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property CutBelow
        : Double
        read  fget_CutBelow
        write fset_CutBelow ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property FastMode
        : Boolean
        read  fget_FastMode
        write fset_FastMode ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property LightVector
        : Boolean
        read  fget_LightVector
        write fset_LightVector ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property IgnoreEllipsoidHeight
        : Boolean
        read  fget_IgnoreEllipsoidHeight
        write fset_IgnoreEllipsoidHeight ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property AdvNavigation
        : Boolean
        read  FAdvNavigation
        write fset_AdvNavigation ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property OrthoView
        : Boolean
        read  FOrthoView
        write fset_OrthoView ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      property IsBusy
        : Boolean
        read  fget_IsBusy ;
    public

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure Draw ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure LockUpdates ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure UnlockUpdates ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure Lock ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure Unlock ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  ScreenToMap           (  const _pt      : TPoint
                                      ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  ScreenToMap3D         (  const _pt      : TPoint
                                      ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  RayIntersectDem       (  const _orig    : TGIS_Point3D ;
                                         const _dir     : TGIS_Point3D ;
                                         out   _ptg     : TGIS_Point3D
                                      ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure PrintBmp              ( const _rect     : TRect ;
                                        const _bmp      : TGIS_Bitmap
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  PrintBegin             ( const _width  : Integer ;
                                         const _height : Integer
                                       ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure PrintTile             ( const _bmp      : TGIS_Bitmap ;
                                        const _offsetx  : Integer ;
                                        const _offsety  : Integer
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure PrintEnd              ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure Rotate                ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure Move                  ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure Drag                  ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure DragEx                ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure ZoomBy                ( const _value    : Double ;
                                        const _x        : Integer ;
                                        const _y        : Integer
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure DoMouseDown           ( const _xpos     : SmallInt ;
                                        const _ypos     : SmallInt
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure DoMouseUp ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure DoMouseMove           ( const _xpos    : SmallInt ;
                                        const _ypos    : SmallInt
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure GestureBegin          ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure DoGesture             ( const _dragdx       : SmallInt ;
                                        const _dragdy       : SmallInt ;
                                        const _zoomxpos     : SmallInt ;
                                        const _zoomypos     : SmallInt ;
                                        const _zoomdistance : Double   ;
                                        const _camerapositiondx
                                                            : SmallInt ;
                                        const _camerapositiondy
                                                            : SmallInt ;
                                        const _paramex      : Double
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure GestureEnd ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure ResetView ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure FullExtent            ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  GetDemLevelAtReferencePointer
                                      : Double ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  GetFps                : Integer ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure StoreMousePos         ( const _value   : TPoint
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  Locate                 ( const _pt     : TPoint ;
                                         const _prec   : Integer
                                       ) : TGIS_ShapeAbstract ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function  Locate3D              ( const _pt      : TPoint ;
                                        const _prec    : Integer ;
                                        var   _layer   : TGIS_LayerAbstract ;
                                        var   _ptg     : TGIS_Point3D ;
                                        var   _shp     : TGIS_ShapeAbstract ;
                                        var   _part    : Integer
                                      ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure MarkShape             ( const _layer   : TGIS_LayerAbstract ;
                                        const _shpid   : Integer ;
                                        const _part    : Integer ;
                                        const _color   : TGIS_Color ;
                                        const _update  : Boolean
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure UnMarkShape           ( const _layer   : TGIS_LayerAbstract ;
                                        const _shpid   : Integer ;
                                        const _update  : Boolean
                                      ) ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure UpdateAllSelectedObjects ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      function InitialRedraw          ( const _extent   : TGIS_Extent ;
                                        const _initdraw : Boolean
                                      ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure UpdateWholeMap ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure UpdateTopmost ;

      /// <inheritdoc from="IGIS_Viewer3D"/>
      procedure ControlRepaint ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisResource ;
{$ENDIF}

//==============================================================================
// force uncounted interface
//==============================================================================

{$IFDEF DCC}
  function TGIS_Viewer3DBase.QueryInterface(
    const _iid : TGUID ;
    out   _obj
  ): HResult;
  begin
    if GetInterface( _iid, _obj ) then
      Result := S_OK
    else
      Result := E_NOINTERFACE;
  end;

  function TGIS_Viewer3DBase._AddRef
    : Integer ;
  begin
    Result := -1;
  end;

  function TGIS_Viewer3DBase._Release
    : Integer ;
  begin
    Result := -1;
  end;
{$ENDIF}

//==============================================================================
// Constructors / Destructors
//==============================================================================

  constructor TGIS_Viewer3DBase.Create(
    const _hwnd     : THandle ;
    const _viewer   : IGIS_Viewer ;
    const _extent   : TGIS_Extent ;
    const _initdraw : Boolean
  ) ;
  begin
    if not assigned( _viewer ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ), 'Create', 0 ) ;
    if _viewer.IsEmpty then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ), 'Create', 1 ) ;

    bCanRender := True;

    oViewer := _viewer ;
    hWindow := _hwnd ;
    sunCameraDelta := 0;
  end ;

  function TGIS_Viewer3DBase.InitialRedraw(
    const _extent   : TGIS_Extent ;
    const _initdraw : Boolean
  ) : Boolean ;
  begin
    Result := False ;
    if not oRenderer.Init3D then
    begin
      bCanRender := False ;
      exit ;
    end ;

    {$IFDEF CLR}
      oRenderer.UpdateEvent += doOnUpdate ;
    {$ELSE}
      oRenderer.UpdateEvent := doOnUpdate ;
    {$ENDIF}

    // Initialize variables & arrays
    oRenderer.SetInitValues( _extent, oViewer.VisibleExtent, oViewer, self ) ;

    if _initdraw then begin
      oRenderer.ReRender ;
      //?ControlRepaint ;
    end;


    fset_Mode( FMode ) ;
    FastMode := False ;
    AdvNavigation := False ;
    oRenderer.SetNavigationType( AdvNavigation ) ;
    OrthoView := False ;
    oRenderer.SetOrthoView( OrthoView ) ;
    Result := True ;
  end;

  procedure TGIS_Viewer3DBase.UpdateWholeMap ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.UpdateWholeMap ;
  end;

  procedure TGIS_Viewer3DBase.UpdateTopmost ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.UpdateTopmost ;
  end;

  procedure TGIS_Viewer3DBase.ControlRepaint ;
  begin
    Draw ;
  end;

  procedure TGIS_Viewer3DBase.doDestroy ;
  begin
      {$IFDEF CLR}
        oRenderer.UpdateEvent -= doOnUpdate ;
      {$ELSE}
        oRenderer.UpdateEvent := nil ;
      {$ENDIF}

      FreeObject( oRenderer ) ;

      inherited ;
  end;


//==============================================================================
// Properties
//==============================================================================

  function  TGIS_Viewer3DBase.fget_Mode : TGIS_Viewer3DMode ;
  begin
    Result := FMode ;
  end;

  procedure TGIS_Viewer3DBase.fset_Mode(
    const _value : TGIS_Viewer3DMode
  ) ;
  begin
    FMode := _value ;
    oRenderer.SetNavigationMode( _value ) ;
    oViewer.ViewerParent.ControlSet3DMode( FMode ) ;
  end ;

  function TGIS_Viewer3DBase.fget_ScaleZ
    : Double ;
  begin
    Result := oRenderer.GetScaleZ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ScaleZ(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetScaleZ = _value then
      exit ;

    oRenderer.SetScaleZ ( _value ) ;
    if not bCanRender then
      exit;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_ScaleM
    : Double ;
  begin
    Result := oRenderer.GetScaleM ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ScaleM(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetScaleM = _value then
      exit ;

    oRenderer.SetScaleM ( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_Scale
   : Double ;
  begin
    Result := oRenderer.GetScale3D ;
  end ;

  procedure TGIS_Viewer3DBase.fset_Scale
   ( const _value : Double
   ) ;
  begin
    oRenderer.SetScale3D( _value ) ;
  end ;

  function TGIS_Viewer3DBase.fget_ScaleAsText
    : String ;
  begin
    Result := oRenderer.GetScale3DAsText ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ScaleAsText
    ( const _value : String
    ) ;
  begin
    oRenderer.SetScale3DAsText( _value ) ;
  end ;

  function TGIS_Viewer3DBase.fget_Wireframe
    : Boolean ;
  begin
    Result := oRenderer.GetWireframe ;
  end ;

  procedure TGIS_Viewer3DBase.fset_Wireframe(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetWireframe = _value then
      exit ;

    oRenderer.SetWireframe ( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_Lights
    : Boolean ;
  begin
    Result := oRenderer.GetLights ;
  end ;

  procedure TGIS_Viewer3DBase.fset_Lights(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetLights = _value then
      exit ;

    oRenderer.SetLights ( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_Labels
    : Boolean ;
  begin
    Result := oRenderer.GetLabels ;
  end ;

  procedure TGIS_Viewer3DBase.fset_Labels(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetLabels = _value then
      exit ;

    oRenderer.SetLabels ( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_HideLabels
    : Boolean ;
  begin
    Result := oRenderer.GetLabelsEx ;
  end ;

  procedure TGIS_Viewer3DBase.fset_HideLabels(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetLabelsEx = _value then
      exit ;

    oRenderer.SetLabelsEx ( _value ) ;
  end ;

  function TGIS_Viewer3DBase.fget_VectorEdges
    : Boolean ;
  begin
    Result := oRenderer.GetVectorEdges ;
  end ;

  procedure TGIS_Viewer3DBase.fset_VectorEdges(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetVectorEdges = _value then
      exit ;

    oRenderer.SetVectorEdges ( _value ) ;
  end ;

  function TGIS_Viewer3DBase.fget_EdgesColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetEdgesColor ;
  end ;

  procedure TGIS_Viewer3DBase.fset_EdgesColor
    ( const _value : TGIS_Color
    ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetEdgesColor = _value then
      exit ;

    oRenderer.SetEdgesColor( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_ReferencePointer
    : Boolean ;
  begin
    Result := oRenderer.GetOriginPointer ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ReferencePointer(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetOriginPointer ( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_ReferencePoint
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetOriginPoint ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ReferencePoint(
    const _value : TGIS_Point3D
  ) ;
  begin
    oRenderer.SetOriginPoint( _value );
  end;

  function TGIS_Viewer3DBase.fget_ReferencePointMode
    : TGIS_Viewer3DReferenceMode ;
  begin
    Result := oRenderer.GetReferencePointMode ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ReferencePointMode(
    const _value : TGIS_Viewer3DReferenceMode
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetReferencePointMode = _value then
      exit ;

    oRenderer.SetReferencePointMode( _value) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_ReferencePointOffset
    : Double ;
  begin
    Result := oRenderer.GetReferencePointOffset ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ReferencePointOffset(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetReferencePointOffset = _value then
      exit ;

    oRenderer.SetReferencePointOffset( _value) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_CameraPosition
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraPosition;
  end ;

  procedure TGIS_Viewer3DBase.fset_CameraPosition(
    const _value : TGIS_Point3D
  ) ;
  var
    sun : TGIS_Point  ;
    cam : TGIS_Point3D;
  begin
    if not bCanRender then
      exit;

    cam := oRenderer.GetCameraPosition ;

    if GisIsSamePoint3D( cam, _value ) then
      exit ;

    oRenderer.SetCameraPosition( _value ) ;

    sun := oRenderer.GetSunPosition ;

    if KeepSunCamera then begin
      sun.X := cam.X ;
      sun.Y := cam.Y + sunCameraDelta ;
      oRenderer.SetSunPosition( sun ) ;
    end ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_CameraPositionEx
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraPositionEx;
  end ;

  procedure TGIS_Viewer3DBase.fset_CameraPositionEx(
    const _value : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    if GisIsSamePoint3D( oRenderer.GetCameraPositionEx, _value ) then
      exit ;

    oRenderer.SetCameraPositionEx( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_CameraRotation
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraRotation;
  end ;

  procedure TGIS_Viewer3DBase.fset_CameraRotation(
    const _value : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetCameraRotation( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_SunPosition
    : TGIS_Point ;
  begin
    Result := oRenderer.GetSunPosition;
  end ;

  procedure TGIS_Viewer3DBase.fset_SunPosition(
    const _value : TGIS_Point
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetSunPosition( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_KeepSunCamera
    : Boolean ;
  begin
    Result := oRenderer.GetKeepSunCamera;
  end ;

  procedure TGIS_Viewer3DBase.fset_KeepSunCamera(
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetKeepSunCamera( _value ) ;
  end ;

  function TGIS_Viewer3DBase.fget_ShadowsLevel
    : Integer ;
  begin
    Result := oRenderer.GetShadowsLevel ;
  end ;

  procedure TGIS_Viewer3DBase.fset_ShadowsLevel(
    const _value : Integer
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetShadowsLevel = _value then
      exit ;

    oRenderer.SetShadowsLevel( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_DemWalls
    : TGIS_Viewer3DDemWall ;
  begin
    Result := oRenderer.GetDemWallType;
  end ;

  procedure TGIS_Viewer3DBase.fset_DemWalls(
    const _value : TGIS_Viewer3DDemWall
    ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetDemWallType = _value then
      exit ;

    oRenderer.SetDemWallType( _value ) ;
  end ;

  function  TGIS_Viewer3DBase.fget_UniverseColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetUniverseColor ;
  end;

  procedure TGIS_Viewer3DBase.fset_UniverseColor(
    const _value : TGIS_Color
    ) ;
  begin
    if oRenderer.GetUniverseColor = _value then
      exit ;

    oRenderer.SetUniverseColor( _value ) ;

    ControlRepaint ;
  end;

  function TGIS_Viewer3DBase.fget_TextureMode
    : Boolean ;
  begin
    Result := oRenderer.GetTexture ;
  end ;

  procedure TGIS_Viewer3DBase.fset_TextureMode(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetTexture = _value then
      exit ;

    oRenderer.SetTexture( _value ) ;
  end ;

  function TGIS_Viewer3DBase.fget_IsolineGap
    : Double ;
  begin
    Result := oRenderer.GetIsolineGap ;
  end ;

  procedure TGIS_Viewer3DBase.fset_IsolineGap(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetIsolineGap = _value then
      exit ;

    oRenderer.SetIsolineGap( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_IsolineColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetIsolineColor ;
  end ;

  procedure TGIS_Viewer3DBase.fset_IsolineColor(
    const _value : TGIS_Color
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetIsolineColor = _value then
      exit ;

    oRenderer.SetIsolineColor( _value ) ;

    ControlRepaint ;
  end ;

  procedure TGIS_Viewer3DBase.fset_SolidWallColor(
    const _value : TGIS_Color
    ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetSolidWallColor( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_Flood
    : TGIS_Viewer3DFlood ;
  begin
    Result := oRenderer.GetFlood ;
  end ;

  procedure TGIS_Viewer3DBase.fset_Flood(
    const _value : TGIS_Viewer3DFlood
  ) ;
  var
    tmp : TGIS_Viewer3DFlood ;
  begin
    if not bCanRender then
      exit;

    tmp := oRenderer.GetFlood ;
    if  ( tmp.Active       = _value.Active       ) and
        ( tmp.Level        = _value.Level        ) and
        ( tmp.Color        = _value.Color        ) and
        ( tmp.Transparency = _value.Transparency )
    then
      exit ;

    oRenderer.SetFlood( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_BasePlane
    : TGIS_Viewer3DBasePlane ;
  begin
    Result := oRenderer.GetBasePlane ;
  end ;

  procedure TGIS_Viewer3DBase.fset_BasePlane(
    const _value : TGIS_Viewer3DBasePlane
  ) ;
  var
    tmp : TGIS_Viewer3DBasePlane ;
  begin
    if not bCanRender then
      exit;

    tmp := oRenderer.GetBasePlane ;
    if  ( tmp.Active          = _value.Active          ) and
        ( tmp.Level           = _value.Level           ) and
        ( tmp.BackgroundColor = _value.BackgroundColor ) and
        ( tmp.GridColor       = _value.GridColor       )
    then
      exit ;

    oRenderer.SetBasePlane( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_VisibleExtent3D
   : TGIS_Extent3D ;
  begin
    Result := oRenderer.GetVisibleExtent3D ;
  end ;

  function TGIS_Viewer3DBase.fget_VisibleExtent
   : TGIS_Extent ;
  begin
    Result := oRenderer.GetVisibleExtent ;
  end ;

  procedure TGIS_Viewer3DBase.fset_VisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    if not bCanRender then
      exit;

    if GisIsSameExtent( oRenderer.GetVisibleExtent, _value  ) then
      exit ;

    oRenderer.SetVisibleExtent( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_ZoomFactor
   : Double ;
  begin
    assert( oRenderer.GetPixelSize.X <> 0 ) ;
    Result := 1.0 / oRenderer.GetPixelSize.X ;
  end ;

  function TGIS_Viewer3DBase.fget_Restriction
   : TGIS_Viewer3DViewRestriction ;
  begin
    Result := oRenderer.GetRestriction ;
  end ;

  procedure TGIS_Viewer3DBase.fset_Restriction
   ( const _value : TGIS_Viewer3DViewRestriction
   ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetRestriction = _value then
      exit ;

    oRenderer.SetRestriction( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_DemTransparency
   : Boolean ;
  begin
    Result := oRenderer.GetDemTransparency ;
  end ;

  procedure TGIS_Viewer3DBase.fset_DemTransparency
   ( const _value : Boolean
   ) ;
  begin
   if oRenderer.GetDemTransparency = _value then
      exit ;

    oRenderer.SetDemTransparency( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_TransparencyPriority
    : TGIS_Viewer3DTransparencyPriority ;
  begin
    Result := oRenderer.GetTranspPrior ;
  end ;


  procedure TGIS_Viewer3DBase.fset_TransparencyPriority
    ( const _value : TGIS_Viewer3DTransparencyPriority
    ) ;
  begin
    if not bCanRender then
      exit;

   if oRenderer.GetTranspPrior = _value then
      exit ;

    oRenderer.SetTranspPrior( _value ) ;

    ControlRepaint ;
  end ;

  function TGIS_Viewer3DBase.fget_ErrorMsg
    : String ;
  begin
    Result := oRenderer.GetErrorMsg ;
  end ;

  function TGIS_Viewer3DBase.fget_PixelSize
    : TGIS_Point;
  begin
    Result := oRenderer.GetPixelSize ;
  end ;

  function TGIS_Viewer3DBase.fget_DemDetailExtentFactor
    : Double ;
  begin
    Result := oRenderer.DemDetailExtentFactor ;
  end ;

  procedure TGIS_Viewer3DBase.fset_DemDetailExtentFactor (
    const _value : Double
  ) ;
  begin
    oRenderer.DemDetailExtentFactor := _value ;
  end ;

  function TGIS_Viewer3DBase.fget_DemDraftExtentFactor
    : Double ;
  begin
    Result := oRenderer.DemDraftExtentFactor ;
  end ;

  procedure TGIS_Viewer3DBase.fset_DemDraftExtentFactor (
    const _value : Double
  ) ;
  begin
      oRenderer.DemDraftExtentFactor := _value ;
  end ;

  function TGIS_Viewer3DBase.fget_VectorExtentFactor
    : Double ;
  begin
    Result := oRenderer.VectorExtentFactor ;
  end ;

  procedure TGIS_Viewer3DBase.fset_VectorExtentFactor (
    const _value : Double
  ) ;
  begin
    oRenderer.VectorExtentFactor := _value ;
  end ;

  function TGIS_Viewer3DBase.fget_VectorSimplification
    : Boolean ;
  begin
    Result := oRenderer.VectorSimplification ;
  end ;

  procedure TGIS_Viewer3DBase.fset_VectorSimplification (
    const _value : Boolean
  ) ;
  begin
    oRenderer.VectorSimplification :=  _value ;
  end ;

  function TGIS_Viewer3DBase.fget_VectorSmartSize
    : Integer ;
  begin
    Result := oRenderer.VectorSmartSize ;
  end ;

  procedure TGIS_Viewer3DBase.fset_VectorSmartSize (
    const _value : Integer
  ) ;
  begin
    oRenderer.VectorSmartSize :=  _value ;
  end ;

  function TGIS_Viewer3DBase.fget_DemGridSize
    : Integer ;
  begin
    Result := oRenderer.DemGridSize ;
  end ;

  function TGIS_Viewer3DBase.fget_DemCachedSize
    : TGIS_Viewer3DDemCacheSize ;
  begin
    Result := oRenderer.DemCachedSize ;
  end ;

  procedure TGIS_Viewer3DBase.fset_DemCachedSize (
    const _value : TGIS_Viewer3DDemCacheSize
  ) ;
  begin
    oRenderer.DemCachedSize := _value ;
  end ;

  function TGIS_Viewer3DBase.fget_IgnoreAbove
    : Double ;
  begin
    Result := oRenderer.GetIgnoreAbove ;
  end;

  procedure TGIS_Viewer3DBase.fset_IgnoreAbove (
    const _value : Double
  ) ;
  begin
    oRenderer.SetIgnoreAbove( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_IgnoreBelow
    : Double ;
  begin
    Result := oRenderer.GetIgnoreBelow ;
  end;

  procedure TGIS_Viewer3DBase.fset_IgnoreBelow (
    const _value : Double
  ) ;
  begin
    oRenderer.SetIgnoreBelow( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_CutAbove
    : Double ;
  begin
    Result := oRenderer.GetCutAbove ;
  end;

  procedure TGIS_Viewer3DBase.fset_CutAbove (
    const _value : Double
  ) ;
  begin
    oRenderer.SetCutAbove( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_CutBelow
    : Double ;
  begin
    Result := oRenderer.GetCutBelow ;
  end;

  procedure TGIS_Viewer3DBase.fset_CutBelow (
    const _value : Double
  ) ;
  begin
    oRenderer.SetCutBelow( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_FastMode
    : Boolean ;
  begin
    Result := oRenderer.GetFastMode ;
  end;

  procedure TGIS_Viewer3DBase.fset_FastMode (
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetFastMode( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_LightVector
    : Boolean ;
  begin
    Result := oRenderer.GetLightVector ;
  end;

  procedure TGIS_Viewer3DBase.fset_LightVector (
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetLightVector( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_IgnoreEllipsoidHeight
  : Boolean ;
  begin
    Result := oRenderer.GetIgnoreEllipsoidHeight ;
  end;

  procedure TGIS_Viewer3DBase.fset_IgnoreEllipsoidHeight (
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetIgnoreEllipsoidHeight( _value ) ;
  end;

//==============================================================================
// Other private routines
//==============================================================================

  procedure TGIS_Viewer3DBase.notifyPaintExceptionEvent(
    const _layerName : String ;
    const _exception : Exception
  ) ;
//  var
//    exc : EGIS_PaintException ;
  begin
    {$IFDEF OXYGENE}
{ TODO -cReview : to be checked }
(*
    if assigned( oViewer.FOnPaintException ) then
      begin
        if _exception is EGIS_PaintException then
          oViewer.FOnPaintException( Self,
                                     TGIS_PaintExceptionEventArgs.Create(
                                       EGIS_PaintException( _exception )
                                     )
                                   )
        else begin
          exc := EGIS_PaintException.Create( _layerName, _exception ) ;
          try
            oViewer.FOnPaintException( Self,
                                       TGIS_PaintExceptionEventArgs.Create(
                                         EGIS_PaintException( exc )
                                       )
                                     )
          finally
            FreeObject( exc ) ;
          end ;
        end ;
      end ;
*)
    {$ELSE}
{ TODO -cReview : to be checked }
(*
      if assigned( oViewer.PaintExceptionEvent ) then
      begin
        if _exception is EGIS_PaintException then
          oViewer.PaintExceptionEvent( Self, EGIS_PaintException( _exception ) )
        else begin
          exc := EGIS_PaintException.Create( _layerName, _exception ) ;
          try
            oViewer.PaintExceptionEvent( Self, exc ) ;
          finally
            FreeObject( exc ) ;
          end ;
        end ;
      end ;
*)
    {$ENDIF}
  end ;

  {$IFDEF CLR}
    procedure TGIS_Viewer3DBase.doOnUpdate(
      _sender : TObject ;
      _e      : EventArgs
    ) ;
  {$ELSE}

    procedure TGIS_Viewer3DBase.doOnUpdate(
      _sender : TObject
    ) ;
  {$ENDIF}
  begin
    oViewer.NotifySubscribers( GIS_SUBSCRIBED_3D_UPDATE, nil ) ;
    if assigned( FOnUpdate ) then
      {$IFDEF CLR}
        FOnUpdate( self, EventArgs.Empty ) ;
      {$ELSE}
        FOnUpdate( self ) ;
      {$ENDIF}
  end;

  {$IFDEF CLR}
    procedure TGIS_Viewer3DBase.doOnPaintException(
      _sender    : System.Object ;
      _e         : TGIS_PaintExceptionEventArgs
    ) ;
    begin
      notifyPaintExceptionEvent( '', _e.Exception ) ;
    end ;
  {$ELSE}
    procedure TGIS_Viewer3DBase.doOnPaintException(
      _sender    : TObject ;
      _exception : EGIS_PaintException
    ) ;
    begin
      notifyPaintExceptionEvent( '', _exception ) ;
    end;
  {$ENDIF}

//==============================================================================
// API
//==============================================================================

  procedure TGIS_Viewer3DBase.Draw ;
  begin
    try
      if not bCanRender then exit;
      if oRenderer.IsBusy then exit;
      oRenderer.Draw;
    except
      on e : Exception do begin
        notifyPaintExceptionEvent( '', e ) ;
      end
    end ;
  end ;

  procedure TGIS_Viewer3DBase.LockUpdates ;
  begin
    oRenderer.LockTimer( False ) ;
  end ;

  procedure TGIS_Viewer3DBase.UnlockUpdates ;
  begin
    oRenderer.LockTimer( True ) ;
  end ;

  procedure TGIS_Viewer3DBase.Lock ;
  begin
    oRenderer.Lock ;
  end ;

  procedure TGIS_Viewer3DBase.Unlock ;
  begin
    oRenderer.Unlock ;
  end ;

  function TGIS_Viewer3DBase.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    if not bCanRender then
      exit;

    Result := oRenderer.ScreenToMap( _pt ) ;
  end ;

  function TGIS_Viewer3DBase.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    Result := oRenderer.ScreenToMap3D( _pt ) ;
  end;

  function  TGIS_Viewer3DBase.RayIntersectDem(
    const _orig    : TGIS_Point3D ;
    const _dir     : TGIS_Point3D ;
    out   _ptg     : TGIS_Point3D
  ) : Boolean ;
  begin
    if not bCanRender then exit;
    if oRenderer.IsBusy then exit;
    Result := oRenderer.RayIntersectDem( _orig, _dir, _ptg ) ;
  end;

  procedure TGIS_Viewer3DBase.PrintBmp (
    const _rect : TRect ;
    const _bmp  : TGIS_Bitmap
  ) ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    oRenderer.PrintBmp( _rect, _bmp ) ;
  end ;

  function TGIS_Viewer3DBase.PrintBegin(
    const _width  : Integer ;
    const _height : Integer
  ) : TPoint ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    Result := oRenderer.PrintBegin( _width, _height ) ;
  end;

  procedure TGIS_Viewer3DBase.PrintTile (
    const _bmp  : TGIS_Bitmap ;
    const _offsetx  : Integer ;
    const _offsety  : Integer
  ) ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    oRenderer.PrintTile( _bmp, _offsetx,_offsety ) ;
  end ;

  procedure TGIS_Viewer3DBase. PrintEnd ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    oRenderer.PrintEnd ;
  end;

  procedure TGIS_Viewer3DBase.Rotate(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.Rotate3D( _delta ) ;

    ControlRepaint ;
  end ;

  procedure TGIS_Viewer3DBase.Move(
    const _delta : TGIS_Point3D
  ) ;

  begin
    if not bCanRender then
      exit;

    oRenderer.Move3D( _delta ) ;

    ControlRepaint ;
  end ;

  procedure TGIS_Viewer3DBase.Drag(
    const _delta : TGIS_Point3D
  ) ;

  begin
    if not bCanRender then
      exit;

    oRenderer.Drag3D ( _delta ) ;

    ControlRepaint ;
  end ;

  procedure TGIS_Viewer3DBase.DragEx(
    const _delta : TGIS_Point3D
  ) ;

  begin
    if not bCanRender then
      exit;

    oRenderer.Drag3DEx ( _delta ) ;

    ControlRepaint ;
  end ;

  procedure TGIS_Viewer3DBase.ZoomBy(
    const _value : Double ;
    const _x        : Integer ;
    const _y        : Integer
  ) ;
  var
    cam : TGIS_Point3D;
  begin
    assert( _value <> 0 ) ;
    if not bCanRender then exit;
    if oRenderer.IsBusy then exit;

    LockUpdates ;
    // allows MouseWheel works properly in ZoomMode
    StoreMousePos( Point( _x, _y ) ) ;
    cam := CameraPosition ;
    cam.Z := cam.Z / _value ;
    CameraPosition := cam ;
    UnlockUpdates ;
  end ;

  procedure TGIS_Viewer3DBase.DoMouseDown(
    const _xpos      : SmallInt ;
    const _ypos      : SmallInt
  ) ;
  begin
    if oRenderer.IsBusy then exit;
    bWasMouseDown := True ;
    oRenderer.StoreMousePos( Point( _xpos, _ypos ) ) ;

    if (Mode = TGIS_Viewer3DMode.CameraPosition) and AdvNavigation then begin
      oRenderer.NewRotationPoint( True ) ;
      oRenderer.ChangeRotationPoint( Point( _xpos, _ypos ) ) ;
    end;

    oldCameraRotation := CameraRotation ;
    oldCameraPosition := CameraPosition ;
    oldSunPosition    := SunPosition ;

    oldMouse := Point( _xpos, _ypos ) ;
    LockUpdates() ;
  end ;

  procedure TGIS_Viewer3DBase.DoMouseUp ;
  begin
    if oRenderer.IsBusy then exit;
    bWasMouseDown := False ;
    if (Mode = TGIS_Viewer3DMode.CameraPosition) and AdvNavigation then
      oRenderer.NewRotationPoint( False ) ;
    UnlockUpdates() ;
  end ;

  procedure TGIS_Viewer3DBase.DoMouseMove(
    const _xpos      : SmallInt ;
    const _ypos      : SmallInt
  ) ;
  var
    dx : Double ;
    dy : Double ;
    dz : Double ;
    cam : TGIS_Point3D ;
    sun : TGIS_Point ;
    d : TGIS_Point3D ;
    val : Double ;
  begin
    if oRenderer.IsBusy then exit;
    if not bWasMouseDown then exit ;

    d := GisPoint3D( 0, 0, 0, 0 ) ;

    case Mode of

      TGIS_Viewer3DMode.CameraPosition :
        begin
          if FOrthoView = True then exit;
          dx := ( ( _xpos - oldMouse.X ) / 400.0 ) * Pi ;
          dy := ( ( _ypos - oldMouse.Y ) / 200.0 ) * Pi/2 ;

          cam  := oldCameraPosition ;
          cam.X := cam.X + dy ;
          cam.Y := cam.Y + dx ;

          CameraPosition := cam ;
        end ;

      TGIS_Viewer3DMode.CameraXY       :
        begin
          d.X := - ( oldMouse.X - _xpos ) * oRenderer.GetPixelSize.X ;
          d.Y := - ( oldMouse.Y - _ypos ) * oRenderer.GetPixelSize.Y ;
          oldMouse := Point( _xpos, _ypos ) ;

          Drag( d ) ;
        end ;

      TGIS_Viewer3DMode.CameraXYZ     :
        begin
          if FOrthoView = True then exit;
          d.X := - ( oldMouse.X - _xpos ) * oRenderer.GetPixelSize.X ;
          d.Y := - ( oldMouse.Y - _ypos ) * oRenderer.GetPixelSize.Y ;
          oldMouse := Point( _xpos, _ypos ) ;

          DragEx( d ) ;
        end ;

      TGIS_Viewer3DMode.CameraRotation :
        begin
          if FOrthoView = True then exit;
          dx := ( ( _xpos - oldMouse.X ) / 400.0 ) * Pi ;
          dz := ( ( _ypos - oldMouse.Y ) / 200.0 ) * Pi/2 ;

          cam  := oldCameraRotation ;
          cam.Z := cam.Z + dx ;
          cam.X := cam.X + dz ;

          CameraRotation := cam ;
        end ;

      TGIS_Viewer3DMode.SunPosition    :
        begin
          dx := ( ( _xpos - oldMouse.X ) / 400.0 ) * Pi ;
          dy := ( ( _ypos - oldMouse.Y ) / 200.0 ) * Pi/2 ;

          sun  := oldSunPosition ;
          sun.X := sun.X - dy ;
          sun.Y := sun.Y + dx ;

          SunPosition := sun ;
       end ;

      TGIS_Viewer3DMode.Zoom :
        begin
          dy := ( ( _ypos - oldMouse.Y ) / 100.0 ) ;

          cam  := oldCameraPosition ;

          val := Abs( dy ) ;

          if dy > 0  then
            cam.Z := cam.Z / ( 1 + val )
          else
            cam.Z := cam.Z  * ( 1 + val ) ;

          CameraPosition := cam ;
       end ;

       TGIS_Viewer3DMode.Select :
         begin
         end ;

     else
       begin
        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
     end ;

    end ;
  end ;

  procedure TGIS_Viewer3DBase.GestureBegin ;
  begin
    DoMouseDown( 0, 0 ) ;
  end;

  procedure TGIS_Viewer3DBase.DoGesture(
    const _dragdx       : SmallInt ;
    const _dragdy       : SmallInt ;
    const _zoomxpos     : SmallInt ;
    const _zoomypos     : SmallInt ;
    const _zoomdistance : Double   ;
    const _camerapositiondx
                        : SmallInt ;
    const _camerapositiondy
                        : SmallInt ;
    const _paramex      : Double
  ) ;
  var
    repaint : Boolean ;
    d   : TGIS_Point3D ;
    cam : TGIS_Point3D ;
    dx  : Double ;
    dy  : Double ;
  begin
    if oRenderer.IsBusy then
      exit;

    repaint := False ;

    if ( _dragdx <> 0 ) or ( _dragdy <> 0 ) then begin
      // dragging
      d := GisPoint3D( 0, 0, 0, 0 ) ;

      d.X := - _dragdx * oRenderer.GetPixelSize.X ;
      d.Y := - _dragdy * oRenderer.GetPixelSize.Y ;

      if bCanRender then begin
        oRenderer.Drag3D ( d ) ;
        repaint := True ;
      end ;
    end ;

    if _zoomdistance <> 1 then begin
      // zooming
      StoreMousePos( Point( _zoomxpos, _zoomypos ) ) ;
      cam := CameraPosition ;
      cam.Z := cam.Z / _zoomdistance ;

      if bCanRender and
         not GisIsSamePoint3D( oRenderer.GetCameraPosition, cam ) then begin
        oRenderer.SetCameraPosition( cam ) ;
        repaint := True ;
      end ;
    end;

    if ( _camerapositiondx <> 0 ) or ( _camerapositiondy <> 0 ) then begin
      // camera position
      cam := CameraPosition ;

      dx := ( ( _camerapositiondx ) / 400.0 ) * Pi ;
      dy := ( ( _camerapositiondy ) / 200.0 ) * Pi/2 ;

      cam.X := cam.X + dy ;
      cam.Y := cam.Y + dx ;

      if bCanRender and
         not GisIsSamePoint3D( oRenderer.GetCameraPosition, cam ) then begin
        oRenderer.SetCameraPosition( cam ) ;
        repaint := True ;
      end;
    end;

    if repaint then
      ControlRepaint ;
  end ;

  procedure TGIS_Viewer3DBase.GestureEnd ;
  begin
    DoMouseUp ;
  end;

  procedure TGIS_Viewer3DBase.ResetView ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.ResetView;
  end ;

  procedure TGIS_Viewer3DBase.FullExtent ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.ResetView;
  end ;

  function TGIS_Viewer3DBase.GetDemLevelAtReferencePointer
    : Double ;
  begin
    Result := oRenderer.GetPointOnDEM ;
  end ;

  function  TGIS_Viewer3DBase.GetFps
    : Integer ;
  begin
    Result := oRenderer.GetFps ;
  end ;

  function  TGIS_Viewer3DBase.Locate(
    const _pt       : TPoint ;
    const _prec     : Integer
  ) : TGIS_ShapeAbstract ;
  begin
    Result := nil ;
    if oRenderer.IsBusy then exit;
    Result := oRenderer.Locate( _pt, _prec) ;
  end;

  function  TGIS_Viewer3DBase.Locate3D(
    const _pt       : TPoint ;
    const _prec     : Integer ;
    var   _layer    : TGIS_LayerAbstract ;
    var   _ptg      : TGIS_Point3D ;
    var   _shp      : TGIS_ShapeAbstract ;
    var   _part     : Integer
  ) : Boolean ;
  begin
    Result := False ;
    if oRenderer.IsBusy then exit;
    Result := oRenderer.Locate3D( _pt, _prec, _layer, _ptg, _shp, _part ) ;
  end;

  procedure TGIS_Viewer3DBase.MarkShape(
    const _layer : TGIS_LayerAbstract ;
    const _shpid : Integer ;
    const _part  : Integer ;
    const _color : TGIS_Color ;
    const _update: Boolean
  ) ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.MarkShape( TGIS_Layer( _layer ), _shpid, _part, _color, _update ) ;
  end;

  procedure TGIS_Viewer3DBase.UnMarkShape(
    const _layer : TGIS_LayerAbstract ;
    const _shpid : Integer ;
    const _update: Boolean
  ) ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.UnMarkShape( TGIS_Layer(_layer), _shpid, _update ) ;
  end;

  procedure TGIS_Viewer3DBase.UpdateAllSelectedObjects ;
  begin
    if oRenderer.IsBusy then exit;
    oRenderer.UpdateAllSelectedObjects ;
  end;

  procedure TGIS_Viewer3DBase.StoreMousePos(
    const _value : TPoint
  ) ;
  begin
    oRenderer.StoreMousePos( _value ) ;
  end;

  function  TGIS_Viewer3DBase.fget_AdvNavigation : Boolean ;
  begin
    Result := FAdvNavigation ;
  end;

  procedure TGIS_Viewer3DBase.fset_AdvNavigation(
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetNavigationType( _value ) ;
    FAdvNavigation := _value ;
  end;

  function  TGIS_Viewer3DBase.fget_OrthoView : Boolean ;
  begin
    Result := FOrthoView ;
  end;

  procedure TGIS_Viewer3DBase.fset_OrthoView(
    const _value : Boolean
  ) ;
  begin
    if FOrthoView = _value then exit;
    if oRenderer.IsBusy then exit;
    FOrthoView := _value ;
    oRenderer.SetOrthoView( FOrthoView ) ;
    if FOrthoView = True then
      Mode := TGIS_Viewer3DMode.CameraXY ;
  end;

  function TGIS_Viewer3DBase.fget_IsBusy : Boolean ;
  begin
    Result := oRenderer.IsBusy ;
  end ;

  procedure TGIS_Viewer3DBase.setRenderer(
    const _value : TGIS_Renderer3DAbstract
  ) ;
  begin
    assert( assigned( _value ) ) ;
    FreeObject( oRenderer ) ;
    oRenderer := _value ;
  end ;

//==================================== END =====================================
end.

