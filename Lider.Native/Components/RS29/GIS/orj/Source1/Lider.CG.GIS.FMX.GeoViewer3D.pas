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
  Viewer 3D class.
}

unit Lider.CG.GIS.FMX.GeoViewer3D;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoViewer3D"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  System.UITypes,
  System.Types,
  System.Math,
  FMX.Types,
  FMX.Types3D,
  FMX.Controls,
  FMX.Viewport3D,
  FMX.Controls3D,
  Lider.CG.GIS.FMX.GeoRenderer3D,
  FMX.Layers3D,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoTypes3D,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoLayer;

type
  /// <summary>
  ///   Encapsulation of the 3D viewer.
  /// </summary>
  TGIS_Viewer3D = class( TViewport3D, IGIS_Viewer3D )
  private
    { Private declarations }
    firstDraw         : Boolean ;
    oLayout           : TLayout3D ;
    oCamera           : TCamera ;
    oLight            : TLight  ;
    oRenderer         : TGIS_Renderer3DFMX ;
    FMode             : TGIS_Viewer3DMode  ;
    FAdvNavigation    : Boolean            ;
    oViewer           : IGIS_Viewer ;
    bCanRender        : Boolean     ;
    bWasMouseDown     : Boolean     ;
    oldCameraPosition : TGIS_Point3D ;
    oldCameraRotation : TGIS_Point3D ;
    oldSunPosition    : TGIS_Point   ;
    oldMouse          : TPoint       ;
    FOnUpdate         : TNotifyEvent ;
    FOrthoView        : Boolean ;

  private
    function  fget_Mode           : TGIS_Viewer3DMode ;
    procedure fset_Mode           ( const _value :
                                        TGIS_Viewer3DMode          ) ;
    function  fget_ScaleZ         : Double ;
    procedure fset_ScaleZ         ( const _value : Double        ) ;
    function  fget_ScaleM         : Double ;
    procedure fset_ScaleM         ( const _value : Double        ) ;
    function  fget_Scale          : Double ;
    procedure fset_Scale          ( const _value : Double        ) ;
    function  fget_ScaleAsText    : String ;
    procedure fset_ScaleAsText    ( const _value : String        ) ;
    function  fget_Wireframe      : Boolean ;
    procedure fset_Wireframe      ( const _value : Boolean       ) ;
    function  fget_Lights         : Boolean ;
    procedure fset_Lights         ( const _value : Boolean       ) ;
    function  fget_Labels          : Boolean ;
    procedure fset_Labels         ( const _value : Boolean       ) ;
    function  fget_HideLabels     : Boolean ;
    procedure fset_HideLabels     ( const _value : Boolean       ) ;
    function  fget_VectorEdges    : Boolean ;
    procedure fset_VectorEdges    ( const _value : Boolean       ) ;
    function  fget_EdgesColor     : TGIS_Color ;
    procedure fset_EdgesColor     ( const _value : TGIS_Color      ) ;
    function  fget_ReferencePointer : Boolean ;
    procedure fset_ReferencePointer ( const _value : Boolean       ) ;
    function  fget_ReferencePoint : TGIS_Point3D ;
    procedure fset_ReferencePoint ( const _value : TGIS_Point3D  ) ;
    function  fget_ReferencePointMode
                                  : TGIS_Viewer3DReferenceMode ;
    procedure fset_ReferencePointMode
                                  ( const _value :
                                      TGIS_Viewer3DReferenceMode ) ;
    function  fget_ReferencePointOffset
                                  : Double ;
    procedure fset_ReferencePointOffset
                                  ( const _value : Double ) ;
    function  fget_CameraPosition : TGIS_Point3D ;
    procedure fset_CameraPosition ( const _value : TGIS_Point3D  ) ;
    function  fget_CameraPositionEx : TGIS_Point3D ;
    procedure fset_CameraPositionEx ( const _value : TGIS_Point3D  ) ;
    function  fget_CameraPosition3DEx : TGIS_Point3D ;
    function  fget_CameraRotation   : TGIS_Point3D ;
    procedure fset_CameraRotation   ( const _value : TGIS_Point3D  ) ;
    function  fget_SunPosition      : TGIS_Point ;
    procedure fset_SunPosition      ( const _value : TGIS_Point    ) ;
    function  fget_ShadowsLevel     : Integer ;
    procedure fset_ShadowsLevel     ( const _value : Integer       ) ;
    function  fget_FastMode         : Boolean ;
    procedure fset_FastMode         ( const _value : Boolean       ) ;
    function  fget_LightVector      : Boolean ;
    procedure fset_LightVector      ( const _value : Boolean       ) ;
    function  fget_DemWalls         : TGIS_Viewer3DDemWall ;
    procedure fset_DemWalls         ( const _value : TGIS_Viewer3DDemWall  ) ;
    function  fget_UniverseColor    : TGIS_Color ;
    procedure fset_UniverseColor    ( const _value : TGIS_Color   ) ;
    function  fget_TextureMode      : Boolean ;
    procedure fset_TextureMode      ( const _value : Boolean       ) ;
    function  fget_IsolineGap       : Double ;
    procedure fset_IsolineGap       ( const _value : Double) ;
    function  fget_IsolineColor     : TGIS_Color ;
    procedure fset_IsolineColor     ( const _value : TGIS_Color) ;
    procedure fset_SolidWallColor   ( const _value : TGIS_Color        ) ;
    function  fget_Flood            : TGIS_Viewer3DFlood ;
    procedure fset_Flood            ( const _value : TGIS_Viewer3DFlood) ;
    function  fget_BasePlane        : TGIS_Viewer3DBasePlane ;
    procedure fset_BasePlane        ( const _value : TGIS_Viewer3DBasePlane) ;
    function  fget_VisibleExtent3D  : TGIS_Extent3D ;
    function  fget_VisibleExtent    : TGIS_Extent ;
    procedure fset_VisibleExtent    ( const _value : TGIS_Extent ) ;
    function  fget_ZoomFactor       : Double ;
    function  fget_Restriction      : TGIS_Viewer3DViewRestriction ;
    procedure fset_Restriction      ( const _value :
                                      TGIS_Viewer3DViewRestriction ) ;
    function  fget_DemTransparency  : Boolean ;
    procedure fset_DemTransparency  ( const _value : Boolean ) ;
    function  fget_TransparencyPriority : TGIS_Viewer3DTransparencyPriority ;
    procedure fset_TransparencyPriority ( const _value :
                                          TGIS_Viewer3DTransparencyPriority ) ;
    function  fget_ErrorMsg         : String ;
    function  fget_PixelSize        : TGIS_Point ;
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
    procedure fset_DemCachedSize         ( const _value :
                                           TGIS_Viewer3DDemCacheSize
                                         ) ;
    function  fget_IgnoreAbove           : Double  ;
    procedure fset_IgnoreAbove           ( const _value : Double
                                         ) ;
    function  fget_IgnoreBelow           : Double  ;
    procedure fset_IgnoreBelow           ( const _value : Double
                                         ) ;
    function  fget_CutAbove              : Double  ;
    procedure fset_CutAbove              ( const _value : Double
                                         ) ;
    function  fget_CutBelow              : Double  ;
    procedure fset_CutBelow              ( const _value : Double
                                         ) ;
    function  fget_IgnoreEllipsoidHeight : Boolean  ;
    procedure fset_IgnoreEllipsoidHeight ( const _value : Boolean ) ;
    function  fget_AdvNavigation         : Boolean ;
    procedure fset_AdvNavigation         ( const _value : Boolean ) ;
    function  fget_OrthoView             : Boolean ;
    procedure fset_OrthoView             ( const _value : Boolean ) ;
    function  fget_IsBusy                : Boolean ;

    procedure doRender( Sender: TObject; Context: TContext3D );

    procedure doOnUpdate           (       _sender    : TObject
                                   ) ;

    /// <summary>
    ///   Prepare Camera parameters before scene rendering.
    /// </summary>
    procedure prepareCameraParams ;

    /// <summary>
    ///   Simple scene repaint.
    /// </summary>
    procedure sceneRepaint ;

    /// <summary>
    ///   Full scene update.
    /// </summary>
    procedure sceneUpdate ;

  protected
    { Protected declarations }

  public
    /// <summary>
    ///   Set 3d Viewer navigation mode.
    /// </summary>
    property Mode                       : TGIS_Viewer3DMode
                                            read  FMode
                                            write fset_Mode ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ScaleZ         : Double          read  fget_ScaleZ
                                              write fset_ScaleZ ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ScaleM         : Double          read  fget_ScaleM
                                              write fset_ScaleM ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property Scale           : Double         read  fget_Scale
                                              write fset_Scale ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ScaleAsText     : String         read  fget_ScaleAsText
                                              write fset_ScaleAsText ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShowWireframe  : Boolean         read  fget_Wireframe
                                              write fset_Wireframe ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShowLights     : Boolean         read  fget_Lights
                                              write fset_Lights ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShowLabels     : Boolean         read  fget_Labels
                                              write fset_Labels ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    property HideLabelsUponNavigation   : Boolean
                                              read  fget_HideLabels
                                              write fset_HideLabels ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShowVectorEdges: Boolean         read  fget_VectorEdges
                                              write fset_VectorEdges ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property EdgesColor     : TGIS_Color      read  fget_EdgesColor
                                              write fset_EdgesColor ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShowReferencePoint : Boolean     read  fget_ReferencePointer
                                              write fset_ReferencePointer ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ReferencePoint : TGIS_Point3D    read  fget_ReferencePoint
                                              write fset_ReferencePoint   ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ReferencePointMode : TGIS_Viewer3DReferenceMode
                                  read  fget_ReferencePointMode
                                  write fset_ReferencePointMode ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ReferencePointOffset : Double    read  fget_ReferencePointOffset
                                              write fset_ReferencePointOffset ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property CameraPosition  : TGIS_Point3D   read  fget_CameraPosition
                                              write fset_CameraPosition ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property CameraPositionEx : TGIS_Point3D  read  fget_CameraPositionEx
                                              write fset_CameraPositionEx ;

    /// <summary>
    ///   Get camera position coordinates.
    /// </summary>
    /// <remarks>
    ///   Camera position is specified as:
    ///   <list type="table">
    ///     <listheader>
    ///       <term>Term</term>
    ///       <description>Description</description>
    ///     </listheader>
    ///     <item>
    ///       <term>X</term>
    ///       <description>X coordinate in 3D units</description>
    ///     </item>
    ///     <item>
    ///       <term>Y</term>
    ///       <description>Y coordinate in 3D units</description>
    ///     </item>
    ///     <item>
    ///       <term>Z</term>
    ///       <description>Z coordinate in 3D units</description>
    ///     </item>
    ///     <item>
    ///       <term>M</term>
    ///       <description>for future use</description>
    ///     </item>
    ///   </list>
    /// </remarks>
    property CameraPosition3DEx : TGIS_Point3D read fget_CameraPosition3DEx ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property CameraRotation  : TGIS_Point3D   read  fget_CameraRotation
                                              write fset_CameraRotation ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property SunPosition     : TGIS_Point     read  fget_SunPosition
                                              write fset_SunPosition ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShadowsLevel    : Integer        read  fget_ShadowsLevel
                                              write fset_ShadowsLevel ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemWalls        : TGIS_Viewer3DDemWall
                                              read  fget_DemWalls
                                              write fset_DemWalls ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property UniverseColor   : TGIS_Color     read  fget_UniverseColor
                                              write fset_UniverseColor ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ShowDemTexture  : Boolean        read  fget_TextureMode
                                              write fset_TextureMode ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemIsolineGap   : Double         read  fget_IsolineGap
                                              write fset_IsolineGap ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemIsolineColor : TGIS_Color     read  fget_IsolineColor
                                              write fset_IsolineColor ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property WallsColor      : TGIS_Color     write fset_SolidWallColor ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property Flood           : TGIS_Viewer3DFlood
                                              read  fget_Flood
                                              write fset_Flood ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property BasePlane       : TGIS_Viewer3DBasePlane
                                              read  fget_BasePlane
                                              write fset_BasePlane ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property VisibleExtent3D : TGIS_Extent3D  read  fget_VisibleExtent3D ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    property VisibleExtent   : TGIS_Extent    read  fget_VisibleExtent
                                              write fset_VisibleExtent ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property Zoom            : Double         read  fget_ZoomFactor ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property AllowDemTransparency : Boolean   read  fget_DemTransparency
                                              write fset_DemTransparency ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property TransparencyPriority : TGIS_Viewer3DTransparencyPriority
                                              read  fget_TransparencyPriority
                                              write fset_TransparencyPriority;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ViewRestriction      : TGIS_Viewer3DViewRestriction
                                              read  fget_Restriction
                                              write fset_Restriction ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property ErrorMessage         : String       read  fget_ErrorMsg ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property PixelSize            : TGIS_Point   read  fget_PixelSize ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemDetailExtentFactor   : Double
                                       read  fget_DemDetailExtentFactor
                                       write fset_DemDetailExtentFactor ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemDraftExtentFactor    : Double
                                       read  fget_DemDraftExtentFactor
                                       write fset_DemDraftExtentFactor  ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property VectorExtentFactor      : Double
                                       read  fget_VectorExtentFactor
                                       write fset_VectorExtentFactor    ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property VectorSimplification    : Boolean
                                       read  fget_VectorSimplification
                                       write fset_VectorSimplification  ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property VectorSmartSize         : Integer
                                       read  fget_VectorSmartSize
                                       write fset_VectorSmartSize       ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemGridSize             : Integer
                                       read  fget_DemGridSize           ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property DemCachedSize           : TGIS_Viewer3DDemCacheSize
                                       read  fget_DemCachedSize
                                       write fset_DemCachedSize         ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property IgnoreAbove             : Double
                                       read  fget_IgnoreAbove
                                       write fset_IgnoreAbove           ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property IgnoreBelow             : Double
                                       read  fget_IgnoreBelow
                                       write fset_IgnoreBelow           ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property CutAbove                : Double
                                       read  fget_CutAbove
                                       write fset_CutAbove              ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property CutBelow                : Double
                                       read  fget_CutBelow
                                       write fset_CutBelow              ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property FastMode                : Boolean
                                       read  fget_FastMode
                                       write fset_FastMode              ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property LightVector             : Boolean
                                       read  fget_LightVector
                                       write fset_LightVector           ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property IgnoreEllipsoidHeight   : Boolean
                                       read  fget_IgnoreEllipsoidHeight
                                       write fset_IgnoreEllipsoidHeight ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    property AdvNavigation           : Boolean
                                       read  FAdvNavigation
                                       write fset_AdvNavigation ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    property OrthoView               : Boolean
                                       read  FOrthoView
                                       write fset_OrthoView ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    property IsBusy                  : Boolean
                                       read  fget_IsBusy ;
  published //events
    /// <event/>
    /// <summary>
    ///   Event to be called upon each change of view (for example to
    ///   update camera details panel.
    /// </summary>
    property UpdateEvent   : TNotifyEvent     read  FOnUpdate
                                              write FOnUpdate ;


  public
    { Public declarations }

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure DoMouseDown           ( const _xpos     : Smallint          ;
                                      const _ypos     : Smallint
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure DoMouseUp             ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure DoMouseMove           ( const _xpos    : Smallint           ;
                                      const _ypos    : Smallint
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure GestureBegin          ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure DoGesture             ( const _dragdx       : Smallint ;
                                      const _dragdy       : Smallint ;
                                      const _zoomxpos     : Smallint ;
                                      const _zoomypos     : Smallint ;
                                      const _zoomdistance : Double   ;
                                      const _camerapositiondx
                                                          : SmallInt ;
                                      const _camerapositiondy
                                                          : SmallInt ;
                                      const _paramex      : Double
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure GestureEnd ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure ResetView             ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure FullExtent            ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function  GetDemLevelAtReferencePointer
                                    : Double  ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function  GetFps                : Integer ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function  Locate                 ( const _pt     : TPoint             ;
                                       const _prec   : Integer
                                     ) : TGIS_ShapeAbstract ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function  Locate3D              ( const _pt      : TPoint             ;
                                      const _prec    : Integer            ;
                                      var   _layer   : TGIS_LayerAbstract ;
                                      var   _ptg     : TGIS_Point3D       ;
                                      var   _shp     : TGIS_ShapeAbstract ;
                                      var   _part    : Integer
                                    ) : Boolean ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure MarkShape             ( const _layer   : TGIS_LayerAbstract ;
                                      const _shpid   : Integer            ;
                                      const _part    : Integer            ;
                                      const _color   : TGIS_Color         ;
                                      const _update  : Boolean
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure UnMarkShape           ( const _layer   : TGIS_LayerAbstract ;
                                      const _shpid   : Integer ;
                                      const _update  : Boolean
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure UpdateAllSelectedObjects ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure StoreMousePos         ( const _value   : TPoint
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function InitialRedraw          ( const _extent   : TGIS_Extent ;
                                      const _initdraw : Boolean
                                    ) : Boolean ;

    /// <summary>
    ///   Create an instance.
    /// </summary>
    /// <param name="_owner">
    ///   TComponent which owns an instance
    /// </param>
    {#ownership:_owner:ownif_empty}
    constructor Create( _owner: TComponent ); override;

    /// <summary>
    ///   Destroy an instance.
    /// </summary>
    destructor  Destroy; override;

    /// <summary>
    ///   Initialize 3D renderer.
    /// </summary>
    /// <param name="_viewer">
    ///   GIS Viewer interface
    /// </param>
    procedure InitRenderer( const _viewer : IGIS_Viewer ) ;

    /// <summary>
    ///  Free and exit 3D renderer.
    /// </summary>
    procedure ExitRenderer ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure Draw                  ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure LockUpdates           ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure UnlockUpdates         ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure Lock                  ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure Unlock                ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function  ScreenToMap           (  const _pt      : TPoint
                                    ) : TGIS_Point ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    function  ScreenToMap3D         (  const _pt     : TPoint
                                    ) : TGIS_Point3D ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    function  RayIntersectDem       (  const _orig    : TGIS_Point3D ;
                                       const _dir     : TGIS_Point3D ;
                                       out   _ptg     : TGIS_Point3D
                                    ) : Boolean ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure PrintBmp              ( const _rect     : TRect ;
                                      const _bmp      : TGIS_Bitmap
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    function  PrintBegin            ( const _width  : Integer ;
                                      const _height : Integer
                                    ) : TPoint ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    procedure PrintTile            ( const _bmp      : TGIS_Bitmap ;
                                     const _offsetx  : Integer ;
                                     const _offsety  : Integer
                                   ) ;

    /// <inheritdoc from="IGIS_Viewer3D"/>
    procedure PrintEnd             ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure Rotate                ( const _delta    : TGIS_Point3D
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure Move                  ( const _delta    : TGIS_Point3D
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure Drag                  ( const _delta    : TGIS_Point3D
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure DragEx                ( const _delta    : TGIS_Point3D
                                    ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure ZoomBy                ( const _value    : Double ;
                                      const _x        : Integer ;
                                      const _y        : Integer
                                      ) ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure UpdateWholeMap ;

    /// <inheritdoc from="IGIS_Viewer3D" />
    procedure UpdateTopmost ;

    /// <summary>
    ///   Scene repaint with the current settings.
    /// </summary>
    procedure ControlRepaint ;

  published
    { Published declarations }
  end;

//##############################################################################
implementation

uses
  Lider.CG.GIS.FMX.GeoViewerWnd ;

//------------------------------------------------------------------------------
// Constructors / Destructors
//------------------------------------------------------------------------------

  constructor TGIS_Viewer3D.Create( _owner: TComponent ) ;
  begin
    inherited;

    HitTest := False ;

    oCamera := TCamera.Create( Self ) ;
    oCamera.Parent := Self ;
    oCamera.OnRender := doRender ;

    oLight  := TLight.Create( Self ) ;
    oLight.Parent := Self ;
    oLight.LightType := TLightType.Directional ;
    oLight.Position.Point := Point3D(0,0,0);
    oLight.RotationAngle.Point := Point3D(0,0,0);
    oLight.Enabled := False ;

    Self.UsingDesignCamera := False ;
    Self.Camera := oCamera ;
    oRenderer := nil ;

  end;

  destructor TGIS_Viewer3D.Destroy;
  begin
    {$IFDEF GIS_PDK}
      RemoveFreeNotifications ;
    {$ENDIF}

    FreeObject( oRenderer ) ;
    FreeObject( oLight ) ;
    FreeObject( oCamera ) ;
    FreeObject( oLayout ) ;

    inherited;
  end;

//------------------------------------------------------------------------------
// Properties
//------------------------------------------------------------------------------

  function  TGIS_Viewer3D.fget_Mode : TGIS_Viewer3DMode ;
  begin
    Result := FMode ;
  end;

  procedure TGIS_Viewer3D.fset_Mode(
    const _value : TGIS_Viewer3DMode
  ) ;
  begin
    FMode := _value ;
  end ;

  function TGIS_Viewer3D.fget_ScaleZ
    : Double ;
  begin
    Result := oRenderer.GetScaleZ;
  end ;

  procedure TGIS_Viewer3D.fset_ScaleZ(
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

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_ScaleM
    : Double ;
  begin
    Result := oRenderer.GetScaleM ;
  end ;

  procedure TGIS_Viewer3D.fset_ScaleM(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetScaleM = _value then
      exit ;

    oRenderer.SetScaleM ( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_Scale
   : Double ;
  begin
    Result := oRenderer.GetScale3D ;
  end ;

  procedure TGIS_Viewer3D.fset_Scale
   ( const _value : Double
   ) ;
  begin
    oRenderer.SetScale3D( _value ) ;
  end ;

  function TGIS_Viewer3D.fget_ScaleAsText
    : String ;
  begin
    Result := oRenderer.GetScale3DAsText ;
  end ;

  procedure TGIS_Viewer3D.fset_ScaleAsText
    ( const _value : String
    ) ;
  begin
    oRenderer.SetScale3DAsText( _value ) ;
  end ;

  function TGIS_Viewer3D.fget_Wireframe
    : Boolean ;
  begin
    Result := oRenderer.GetWireframe ;
  end ;

  procedure TGIS_Viewer3D.fset_Wireframe(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetWireframe = _value then
      exit ;

    oRenderer.SetWireframe ( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_Lights
    : Boolean ;
  begin
    Result := oRenderer.GetLights ;
  end ;

  procedure TGIS_Viewer3D.fset_Lights(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetLights = _value then
      exit ;
    oLight.Enabled := _value ;
    oRenderer.SetLights ( _value ) ;
    Repaint ;
  end ;

  function TGIS_Viewer3D.fget_Labels
    : Boolean ;
  begin
    Result := oRenderer.GetLabels ;
  end ;

  procedure TGIS_Viewer3D.fset_Labels(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetLabels = _value then
      exit ;

    oRenderer.SetLabels ( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_HideLabels
    : Boolean ;
  begin
    Result := oRenderer.GetLabelsEx ;
  end ;

  procedure TGIS_Viewer3D.fset_HideLabels(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetLabelsEx = _value then
      exit ;

    oRenderer.SetLabelsEx ( _value ) ;
  end ;

  function TGIS_Viewer3D.fget_VectorEdges
    : Boolean ;
  begin
    Result := oRenderer.GetVectorEdges ;
  end ;

  procedure TGIS_Viewer3D.fset_VectorEdges(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetVectorEdges = _value then
      exit ;

    oRenderer.SetVectorEdges ( _value ) ;
  end ;

  function TGIS_Viewer3D.fget_EdgesColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetEdgesColor ;
  end ;

  procedure TGIS_Viewer3D.fset_EdgesColor
    ( const _value : TGIS_Color
    ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetEdgesColor = _value then
      exit ;

    oRenderer.SetEdgesColor( _value ) ;
    sceneUpdate ;
  end ;

  function TGIS_Viewer3D.fget_ReferencePointer
    : Boolean ;
  begin
    Result := oRenderer.GetOriginPointer ;
  end ;

  procedure TGIS_Viewer3D.fset_ReferencePointer(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetOriginPointer ( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_ReferencePoint
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetOriginPoint ;
  end ;

  procedure TGIS_Viewer3D.fset_ReferencePoint(
    const _value : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetOriginPoint( _value ) ;

    sceneRepaint ;
  end;

  function TGIS_Viewer3D.fget_ReferencePointMode
    : TGIS_Viewer3DReferenceMode ;
  begin
    Result := oRenderer.GetReferencePointMode ;
  end ;

  procedure TGIS_Viewer3D.fset_ReferencePointMode(
    const _value : TGIS_Viewer3DReferenceMode
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetReferencePointMode = _value then
      exit ;

    oRenderer.SetReferencePointMode( _value) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_ReferencePointOffset
    : Double ;
  begin
    Result := oRenderer.GetReferencePointOffset ;
  end ;

  procedure TGIS_Viewer3D.fset_ReferencePointOffset(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetReferencePointOffset = _value then
      exit ;

    oRenderer.SetReferencePointOffset( _value) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_CameraPosition
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraPosition;
  end ;

  procedure TGIS_Viewer3D.fset_CameraPosition(
    const _value : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    if GisIsSamePoint3D( oRenderer.GetCameraPosition, _value ) then
      exit ;

    oRenderer.SetCameraPosition( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_CameraPositionEx
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraPositionEx;
  end ;

  procedure TGIS_Viewer3D.fset_CameraPositionEx(
    const _value : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    if GisIsSamePoint3D( oRenderer.GetCameraPositionEx, _value ) then
      exit ;

    oRenderer.SetCameraPositionEx( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_CameraPosition3DEx
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraPosition3DEx;
  end ;

  function TGIS_Viewer3D.fget_CameraRotation
    : TGIS_Point3D ;
  begin
    Result := oRenderer.GetCameraRotation;
  end ;

  procedure TGIS_Viewer3D.fset_CameraRotation(
    const _value : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetCameraRotation( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_SunPosition
    : TGIS_Point ;
  begin
    Result := oRenderer.GetSunPosition;
  end ;

  procedure TGIS_Viewer3D.fset_SunPosition(
    const _value : TGIS_Point
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetSunPosition( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_ShadowsLevel
    : Integer ;
  begin
    Result := oRenderer.GetShadowsLevel ;
  end ;

  procedure TGIS_Viewer3D.fset_ShadowsLevel(
    const _value : Integer
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetShadowsLevel = _value then
      exit ;

    oRenderer.SetShadowsLevel( _value ) ;

    sceneRepaint ;
  end ;

  function  TGIS_Viewer3D.fget_FastMode
    : Boolean ;
  begin
    Result := oRenderer.GetFastMode ;
  end;

  procedure TGIS_Viewer3D.fset_FastMode (
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetFastMode( _value ) ;
  end;

  function  TGIS_Viewer3D.fget_LightVector
    : Boolean ;
  begin
    Result := oRenderer.GetLightVector ;
  end;

  procedure TGIS_Viewer3D.fset_LightVector (
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetLightVector( _value ) ;
    sceneUpdate ;
  end;

  function TGIS_Viewer3D.fget_DemWalls
    : TGIS_Viewer3DDemWall ;
  begin
    Result := oRenderer.GetDemWallType;
  end ;

  procedure TGIS_Viewer3D.fset_DemWalls(
    const _value : TGIS_Viewer3DDemWall
    ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetDemWallType = _value then
      exit ;

    oRenderer.SetDemWallType( _value ) ;
  end ;

  function  TGIS_Viewer3D.fget_UniverseColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetUniverseColor ;
  end;

  procedure TGIS_Viewer3D.fset_UniverseColor(
    const _value : TGIS_Color
    ) ;
  begin
    if oRenderer.GetUniverseColor = _value then
      exit ;
    oRenderer.SetUniverseColor( _value ) ;
    sceneRepaint ;
  end;

  function TGIS_Viewer3D.fget_TextureMode
    : Boolean ;
  begin
    Result := oRenderer.GetTexture ;
  end ;

  procedure TGIS_Viewer3D.fset_TextureMode(
    const _value : Boolean
  ) ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.GetTexture = _value then
      exit ;
    oRenderer.SetTexture( _value ) ;
  end ;

  function TGIS_Viewer3D.fget_IsolineGap
    : Double ;
  begin
    Result := oRenderer.GetIsolineGap ;
  end ;

  procedure TGIS_Viewer3D.fset_IsolineGap(
    const _value : Double
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetIsolineGap = _value then
      exit ;

    oRenderer.SetIsolineGap( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_IsolineColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetIsolineColor ;
  end ;

  procedure TGIS_Viewer3D.fset_IsolineColor(
    const _value : TGIS_Color
  ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetIsolineColor = _value then
      exit ;

    oRenderer.SetIsolineColor( _value ) ;

    sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.fset_SolidWallColor(
    const _value : TGIS_Color
    ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.SetSolidWallColor( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_Flood
    : TGIS_Viewer3DFlood ;
  begin
    Result := oRenderer.GetFlood ;
  end ;

  procedure TGIS_Viewer3D.fset_Flood(
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
    Repaint ;
  end ;

  function TGIS_Viewer3D.fget_BasePlane
    : TGIS_Viewer3DBasePlane ;
  begin
    Result := oRenderer.GetBasePlane ;
  end ;

  procedure TGIS_Viewer3D.fset_BasePlane(
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
    Repaint ;
  end ;

  function TGIS_Viewer3D.fget_VisibleExtent3D
   : TGIS_Extent3D ;
  begin
    Result := oRenderer.GetVisibleExtent3D ;
  end ;

  function TGIS_Viewer3D.fget_VisibleExtent
   : TGIS_Extent ;
  begin
    Result := oRenderer.GetVisibleExtent ;
  end ;

  procedure TGIS_Viewer3D.fset_VisibleExtent
   ( const _value : TGIS_Extent
   ) ;
  begin
    if not bCanRender then
      exit;

    if GisIsSameExtent( oRenderer.GetVisibleExtent, _value ) then
      exit;

    oRenderer.SetVisibleExtent( _value ) ;

    sceneRepaint ;
  end;

  function TGIS_Viewer3D.fget_ZoomFactor
   : Double ;
  begin
    Assert( oRenderer.GetPixelSize.X <> 0 ) ;
    Result := 1.0 / oRenderer.GetPixelSize.X ;
  end ;

  function TGIS_Viewer3D.fget_Restriction
   : TGIS_Viewer3DViewRestriction ;
  begin
    Result := oRenderer.GetRestriction ;
  end ;

  procedure TGIS_Viewer3D.fset_Restriction
   ( const _value : TGIS_Viewer3DViewRestriction
   ) ;
  begin
    if not bCanRender then
      exit;

    if oRenderer.GetRestriction = _value then
      exit ;

    oRenderer.SetRestriction( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_DemTransparency
   : Boolean ;
  begin
    Result := oRenderer.GetDemTransparency ;
  end ;

  procedure TGIS_Viewer3D.fset_DemTransparency
   ( const _value : Boolean
   ) ;
  begin
   if oRenderer.GetDemTransparency = _value then
      exit ;

    oRenderer.SetDemTransparency( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_TransparencyPriority
    : TGIS_Viewer3DTransparencyPriority ;
  begin
    Result := oRenderer.GetTranspPrior ;
  end ;


  procedure TGIS_Viewer3D.fset_TransparencyPriority
    ( const _value : TGIS_Viewer3DTransparencyPriority
    ) ;
  begin
    if not bCanRender then
      exit;

   if oRenderer.GetTranspPrior = _value then
      exit ;

    oRenderer.SetTranspPrior( _value ) ;

    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.fget_ErrorMsg
    : String ;
  begin
    Result := oRenderer.GetErrorMsg ;
  end ;

  function TGIS_Viewer3D.fget_PixelSize
    : TGIS_Point;
  begin
    Result := oRenderer.GetPixelSize ;
  end ;

  function TGIS_Viewer3D.fget_DemDetailExtentFactor
    : Double ;
  begin
    Result := oRenderer.DemDetailExtentFactor ;
  end ;

  procedure TGIS_Viewer3D.fset_DemDetailExtentFactor (
    const _value : Double
  ) ;
  begin
    oRenderer.DemDetailExtentFactor := _value ;
  end ;

  function TGIS_Viewer3D.fget_DemDraftExtentFactor
    : Double ;
  begin
    Result := oRenderer.DemDraftExtentFactor ;
  end ;

  procedure TGIS_Viewer3D.fset_DemDraftExtentFactor (
    const _value : Double
  ) ;
  begin
      oRenderer.DemDraftExtentFactor := _value ;
  end ;

  function TGIS_Viewer3D.fget_VectorExtentFactor
    : Double ;
  begin
    Result := oRenderer.VectorExtentFactor ;
  end ;

  procedure TGIS_Viewer3D.fset_VectorExtentFactor (
    const _value : Double
  ) ;
  begin
    oRenderer.VectorExtentFactor := _value ;
  end ;

  function TGIS_Viewer3D.fget_VectorSimplification
    : Boolean ;
  begin
    Result := oRenderer.VectorSimplification ;
  end ;

  procedure TGIS_Viewer3D.fset_VectorSimplification (
    const _value : Boolean
  ) ;
  begin
    oRenderer.VectorSimplification :=  _value ;
    sceneUpdate ;
  end ;

  function TGIS_Viewer3D.fget_VectorSmartSize
    : Integer ;
  begin
    Result := oRenderer.VectorSmartSize ;
  end ;

  procedure TGIS_Viewer3D.fset_VectorSmartSize (
    const _value : Integer
  ) ;
  begin
    oRenderer.VectorSmartSize :=  _value ;
  end ;

  function TGIS_Viewer3D.fget_DemGridSize
    : Integer ;
  begin
    Result := oRenderer.DemGridSize ;
  end ;

  function TGIS_Viewer3D.fget_DemCachedSize
    : TGIS_Viewer3DDemCacheSize ;
  begin
    Result := oRenderer.DemCachedSize ;
  end ;

  procedure TGIS_Viewer3D.fset_DemCachedSize (
    const _value : TGIS_Viewer3DDemCacheSize
  ) ;
  begin
    oRenderer.DemCachedSize := _value ;
  end ;

  function TGIS_Viewer3D.fget_IgnoreAbove
    : Double  ;
  begin
    Result := oRenderer.GetIgnoreAbove ;
  end;

  procedure TGIS_Viewer3D.fset_IgnoreAbove (
    const _value : Double
  ) ;
  begin
    oRenderer.SetIgnoreAbove( _value ) ;
  end;

  function  TGIS_Viewer3D.fget_IgnoreBelow
    : Double  ;
  begin
    Result := oRenderer.GetIgnoreBelow ;
  end;

  procedure TGIS_Viewer3D.fset_IgnoreBelow (
    const _value : Double
  ) ;
  begin
    oRenderer.SetIgnoreBelow( _value ) ;
  end;

  function  TGIS_Viewer3D.fget_CutAbove
    : Double  ;
  begin
    Result := oRenderer.GetCutAbove ;
  end;

  procedure TGIS_Viewer3D.fset_CutAbove (
    const _value : Double
  ) ;
  begin
    oRenderer.SetCutAbove( _value ) ;
  end;

  function  TGIS_Viewer3D.fget_CutBelow
    : Double  ;
  begin
    Result := oRenderer.GetCutBelow ;
  end;

  procedure TGIS_Viewer3D.fset_CutBelow (
    const _value : Double
  ) ;
  begin
    oRenderer.SetCutBelow( _value ) ;
  end;

  function  TGIS_Viewer3D.fget_IgnoreEllipsoidHeight
  : Boolean ;
  begin
    Result := oRenderer.GetIgnoreEllipsoidHeight ;
  end;

  procedure TGIS_Viewer3D.fset_IgnoreEllipsoidHeight (
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetIgnoreEllipsoidHeight( _value ) ;
  end;

  function  TGIS_Viewer3D.fget_AdvNavigation : Boolean ;
  begin
    Result := FAdvNavigation ;
  end;

  procedure TGIS_Viewer3D.fset_AdvNavigation(
    const _value : Boolean
  ) ;
  begin
    oRenderer.SetNavigationType( _value ) ;
    FAdvNavigation := _value ;
  end;

  function  TGIS_Viewer3D.fget_OrthoView : Boolean ;
  begin
    Result := FOrthoView ;
  end;

  procedure TGIS_Viewer3D.fset_OrthoView(
    const _value : Boolean
  ) ;
  begin
    if FOrthoView = _value then exit;
    FOrthoView := _value ;
    oRenderer.SetOrthoView( FOrthoView ) ;
    if FOrthoView = True then
      Mode := TGIS_Viewer3DMode.CameraXY ;
  end;

  function TGIS_Viewer3D.fget_IsBusy : Boolean ;
  begin
    Result := oRenderer.IsBusy ;
  end ;

//------------------------------------------------------------------------------
// API
//------------------------------------------------------------------------------

  procedure TGIS_Viewer3D.DoMouseDown(
    const _xpos      : Smallint ;
    const _ypos      : Smallint
  ) ;
  begin
    bWasMouseDown := True ;
    oRenderer.StoreMousePos( Point( _xpos, _ypos ) ) ;

    if (Mode = TGIS_Viewer3DMode.CameraPosition) and AdvNavigation then begin
      oRenderer.newRotationPoint( True ) ;
      oRenderer.changeRotationPoint( Point( _xpos, _ypos ) ) ;
      sceneRepaint ;
    end;

    oldCameraRotation := CameraRotation ;
    oldCameraPosition := CameraPosition ;
    oldSunPosition    := SunPosition    ;

    oldMouse := Point( _xpos, _ypos ) ;
    LockUpdates() ;
  end ;

  procedure TGIS_Viewer3D.DoMouseUp ;
  begin
    bWasMouseDown := False ;
    if (Mode = TGIS_Viewer3DMode.CameraPosition) and AdvNavigation then begin
      oRenderer.newRotationPoint( False ) ;
      sceneRepaint ;
    end;
    UnlockUpdates() ;
  end ;

  procedure TGIS_Viewer3D.DoMouseMove(
    const _xpos      : Smallint ;
    const _ypos      : Smallint
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
    if not bWasMouseDown then exit ;

    d := GisPoint3D( 0, 0, 0, 0 ) ;

    case Mode of

      TGIS_Viewer3DMode.CameraPosition :
        begin
          if FOrthoView = True then exit;
          dx := ( ( _xpos - oldMouse.X ) / 400.0 ) * Pi   ;
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
          dx := ( ( _xpos - oldMouse.X ) / 400.0 ) * Pi   ;
          dz := ( ( _ypos - oldMouse.Y ) / 200.0 ) * Pi/2 ;

          cam  := oldCameraRotation ;
          cam.Z := cam.Z + dx ;
          cam.X := cam.X + dz ;

          CameraRotation := cam ;
        end ;

      TGIS_Viewer3DMode.SunPosition    :
        begin
          dx := ( ( _xpos - oldMouse.X ) / 400.0 ) * Pi   ;
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
        Assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
     end ;

    end ;
  end ;

  procedure TGIS_Viewer3D.GestureBegin ;
  begin
    DoMouseDown( 0, 0 ) ;
  end;

  procedure TGIS_Viewer3D.DoGesture(
    const _dragdx       : Smallint ;
    const _dragdy       : Smallint ;
    const _zoomxpos     : Smallint ;
    const _zoomypos     : Smallint ;
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
      end;
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
    end ;

    if repaint then
      sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.GestureEnd ;
  begin
    DoMouseUp ;
  end;

  procedure TGIS_Viewer3D.ResetView ;
  begin
    oRenderer.ResetView ;
    sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.FullExtent ;
  begin
    oRenderer.ResetView ;
    sceneRepaint ;
  end ;

  function TGIS_Viewer3D.GetDemLevelAtReferencePointer
    : Double ;
  begin
    Result := oRenderer.GetPointOnDEM ;
  end ;

  function  TGIS_Viewer3D.GetFps
    : Integer ;
  begin
    Result := oRenderer.GetFps ;
  end ;

  function  TGIS_Viewer3D.Locate(
    const _pt       : TPoint             ;
    const _prec     : Integer
  ) : TGIS_ShapeAbstract ;
  begin
    Result := oRenderer.Locate( _pt, _prec) ;
  end;

  function  TGIS_Viewer3D.Locate3D(
    const _pt       : TPoint             ;
    const _prec     : Integer            ;
    var   _layer    : TGIS_LayerAbstract ;
    var   _ptg      : TGIS_Point3D       ;
    var   _shp      : TGIS_ShapeAbstract ;
    var   _part     : Integer
  ) : Boolean ;
  begin
    Result := oRenderer.Locate3D( _pt, _prec, _layer, _ptg, _shp, _part ) ;
  end;

  procedure TGIS_Viewer3D.MarkShape(
      const _layer : TGIS_LayerAbstract ;
      const _shpid : Integer            ;
      const _part  : Integer            ;
      const _color : TGIS_Color         ;
      const _update: Boolean
    ) ;
  begin
    oRenderer.MarkShape( TGIS_Layer( _layer ), _shpid, _part, _color, _update ) ;
  end;

  procedure TGIS_Viewer3D.UnMarkShape(
    const _layer : TGIS_LayerAbstract ;
    const _shpid : Integer ;
    const _update: Boolean
  ) ;
  begin
    oRenderer.UnMarkShape( TGIS_Layer( _layer ), _shpid, _update ) ;
  end;

  procedure TGIS_Viewer3D.UpdateAllSelectedObjects ;
  begin
    oRenderer.UpdateAllSelectedObjects ;
  end;

  procedure TGIS_Viewer3D.UpdateWholeMap;
  begin
    oRenderer.UpdateWholeMap ;
  end;

  procedure TGIS_Viewer3D.UpdateTopmost;
  begin
    oRenderer.UpdateTopmost ;
  end;

  procedure TGIS_Viewer3D.StoreMousePos(
    const _value : TPoint
  ) ;
  begin
    oRenderer.StoreMousePos( _value ) ;
  end;

  function TGIS_Viewer3D.InitialRedraw(
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

    // Initialize variables & arrays
    oRenderer.SetInitValues( _extent, oViewer.VisibleExtent, oViewer, self ) ;

    if _initdraw then
      oRenderer.Draw() ;

    fset_Mode( FMode ) ;
    FastMode := False ;
    AdvNavigation := False ;
    oRenderer.SetNavigationType( AdvNavigation ) ;
    OrthoView := False ;
    oRenderer.SetOrthoView( OrthoView ) ;
    Result := True ;
  end;

  procedure TGIS_Viewer3D.LockUpdates ;
  begin
    oRenderer.LockTimer( False ) ;
  end ;

  procedure TGIS_Viewer3D.UnlockUpdates ;
  begin
    oRenderer.LockTimer( True ) ;
  end ;

  procedure TGIS_Viewer3D.Lock ;
  begin
    oRenderer.Lock ;
  end ;

  procedure TGIS_Viewer3D.Unlock ;
  begin
    oRenderer.Unlock ;
  end ;

  function TGIS_Viewer3D.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    if not bCanRender then
      exit;

    Result := oRenderer.ScreenToMap( _pt ) ;
  end ;

  function TGIS_Viewer3D.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    if not bCanRender then
      exit;

    Result := oRenderer.ScreenToMap3D( _pt ) ;
  end;

  function  TGIS_Viewer3D.RayIntersectDem(
    const _orig    : TGIS_Point3D ;
    const _dir     : TGIS_Point3D ;
    out   _ptg     : TGIS_Point3D
  ) : Boolean ;
  begin
    if not bCanRender then exit;
    if oRenderer.IsBusy then exit;
    Result := oRenderer.RayIntersectDem( _orig, _dir, _ptg ) ;
  end;

  procedure TGIS_Viewer3D.PrintBmp (
    const _rect : TRect   ;
    const _bmp  : TGIS_Bitmap
  ) ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    oRenderer.PrintBmp( _rect, _bmp ) ;
  end ;

  function TGIS_Viewer3D.PrintBegin(
    const _width  : Integer ;
    const _height : Integer
  ) : TPoint ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    oRenderer.BeginContext( Context ) ;
    Result := oRenderer.PrintBegin( _width, _height ) ;
  end;

  procedure TGIS_Viewer3D.PrintTile (
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

  procedure TGIS_Viewer3D. PrintEnd ;
  begin
    if not bCanRender then
      exit;
    if oRenderer.IsBusy then exit;
    oRenderer.PrintEnd ;
  end;

  procedure TGIS_Viewer3D.Rotate(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.Rotate3D( _delta ) ;

    sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.Move(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.Move3D( _delta ) ;

    sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.Drag(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.Drag3D ( _delta ) ;

    sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.DragEx(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if not bCanRender then
      exit;

    oRenderer.Drag3DEx ( _delta ) ;

    sceneRepaint ;
  end ;

  procedure TGIS_Viewer3D.ZoomBy(
    const _value : Double  ;
    const _x     : Integer ;
    const _y     : Integer
  ) ;
  var
    cam : TGIS_Point3D;
  begin
    Assert( _value <> 0 ) ;

    if not bCanRender then
      exit;

    LockUpdates ;
    // allows MouseWheel works properly in ZoomMode
    StoreMousePos( Point( _x, _y ) ) ;
    cam := CameraPosition ;
    cam.Z := cam.Z / _value ;
    CameraPosition := cam ;
    UnlockUpdates ;
  end ;

  procedure TGIS_Viewer3D.ControlRepaint ;
  begin
    sceneRepaint ;
  end;

  procedure TGIS_Viewer3D.doOnUpdate(
    _sender : TObject
  ) ;
  begin
    oViewer.NotifySubscribers( GIS_SUBSCRIBED_3D_UPDATE, nil ) ;
    if Assigned( FOnUpdate ) then
      FOnUpdate( self ) ;
  end;

  procedure TGIS_Viewer3D.InitRenderer( const _viewer: IGIS_Viewer ) ;
  var
    pos1 : TGIS_Point3D ;
    pos2 : TGIS_Point3D ;
    npos : TPosition3D  ;
  begin
    firstDraw := True ;
    oRenderer := TGIS_Renderer3DFMX.Create ;

    if not Assigned ( oRenderer ) then
        bCanRender := False ;

    bCanRender    := True;
    oRenderer.BeginContext( Context ) ;
    oRenderer.Light   := oLight ;
    oRenderer.Camera  := oCamera ;

    oRenderer.Init3D ;
    oRenderer.SetInitValues(GisCommonExtent(_viewer.VisibleExtent,_viewer.Extent ),
                            _viewer.VisibleExtent,
                            _viewer,
                            self) ;

    oViewer := _viewer ;
    oRenderer.UpdateEvent := doOnUpdate ;

    prepareCameraParams ;

    FMode := TGIS_Viewer3DMode.CameraPosition ;
    fset_Mode( FMode ) ;
    FastMode := False ;
    AdvNavigation := False ;
    oRenderer.SetNavigationType( AdvNavigation ) ;
  end;

  procedure TGIS_Viewer3D.ExitRenderer ;
  begin
    oCamera.Context.Clear( TAlphaColorRec.Aqua) ;
    Repaint ;
    FreeObject( oRenderer ) ;
  end;

  procedure TGIS_Viewer3D.doRender( Sender: TObject; Context: TContext3D ) ;
  begin
    if not Assigned( oRenderer ) then
      exit ;

    oRenderer.BeginContext( Context ) ;
    Context.PushContextStates ;
    Context.SetMatrix( TMatrix3d.Identity );
    if firstDraw then begin
      oRenderer.UpdateWholeMap ;
      firstDraw := False ;
    end;
    Draw ;

    Context.PopContextStates ;
  end;

  procedure TGIS_Viewer3D.Draw ;
  begin
    try
      if not bCanRender then
        exit;

      oRenderer.Draw ;
    except
      on e : Exception do begin
        if not oViewer.NotifyPaintException( '', e ) then
          raise ;
      end ;
    end;
  end ;

  procedure TGIS_Viewer3D.sceneRepaint ;
  begin
    prepareCameraParams ;
  end;

  procedure TGIS_Viewer3D.sceneUpdate ;
  begin
    oRenderer.UpdateWholeMap ;
  end;

  procedure TGIS_Viewer3D.prepareCameraParams ;
  begin
    oRenderer.SetCameraParams ;
  end;

end.


